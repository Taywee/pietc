/* Copyright © 2016 Taylor C. Richberger <taywee@gmx.com>
 * This code is released under the license described in the LICENSE file
 */

#include <iostream>
#include <iomanip>
#include <set>
#include <list>
#include <vector>
#include <algorithm>
#include <memory>
#include <numeric>
#include <stack>
#include <functional>
#include <unordered_map>

#include <args.hxx>
#include <Magick++.h>

#include "color.hxx"

static Field ImageToField(const std::string filename, size_t codelsize);
static void PrintField(const Field &field, const Map &map);

int main(const int argc, const char **argv)
{
    args::ArgumentParser parser("A LLVM piet compiler, which can compile a Piet program "
            "into LLVM IR, LLVM bitcode, native assembly, native object code, or an "
            "executable, as well as run the optimized JIT compiled Piet program immediately.");
    args::HelpFlag help(parser, "help", "Display this help menu", {'h', "help"});
    args::ValueFlag<size_t> cs(parser, "size", "Set the codel size manually (0 tries to auto-detect, which usually works, but may be imperfect in certain scenarios)", {"cs", "codel-size"}, 0);
    args::Group required(parser, "", args::Group::Validators::All);
    args::Positional<std::string> input(required, "input", "Input file to use");

    try
    {
        parser.ParseCLI(argc, argv);
    }
    catch (const args::Help &)
    {
        std::cout << parser;
        return 0;
    }
    catch (const args::ValidationError &e)
    {
        if (!input)
        {
            std::cerr << "input is a required argument" << std::endl;
        }
        std::cerr << parser;
        return 1;
    }
    catch (const args::Error &e)
    {
        std::cerr << e.what() << std::endl;
        std::cerr << parser;
        return 1;
    }

    try {
        const Field field = ImageToField(args::get(input), args::get(cs));
        Map map;

        for (size_t y = 0; y < field.size(); ++y)
        {
            const auto &row = field[y];
            for (size_t x = 0; x < row.size(); ++x)
            {
                const auto &codel = row[x];
                if (map.find(Coords{x, y}) == map.end())
                {
                    // Queue-based flood fill algorithm: https://en.wikipedia.org/wiki/Flood_fill
                    std::unordered_set<Coords> processed;
                    for (std::unordered_set<Coords> queue{Coords{x, y}}; !queue.empty();)
                    {
                        const auto it = queue.begin();
                        const auto n = *it;
                        queue.erase(it);
                        const auto &x = std::get<0>(n);
                        const auto &y = std::get<1>(n);

                        const auto &ncodel = field[y][x];
                        // Check this codel color against other existing ones
                        if (ncodel == codel)
                        {
                            processed.emplace(n);

                            // West
                            if (x > 0)
                            {
                                const Coords coord{x - 1, y};
                                if (processed.find(coord) == processed.end() && queue.find(coord) == queue.end())
                                {
                                    queue.emplace(coord);
                                }
                            }
                            // East
                            if (x < (row.size() - 1))
                            {
                                const Coords coord{x + 1, y};
                                if (processed.find(coord) == processed.end() && queue.find(coord) == queue.end())
                                {
                                    queue.emplace(coord);
                                }
                            }
                            // North
                            if (y > 0)
                            {
                                const Coords coord{x, y - 1};
                                if (processed.find(coord) == processed.end() && queue.find(coord) == queue.end())
                                {
                                    queue.emplace(coord);
                                }
                            }
                            // South
                            if (y < (field.size() - 1))
                            {
                                const Coords coord{x, y + 1};
                                if (processed.find(coord) == processed.end() && queue.find(coord) == queue.end())
                                {
                                    queue.emplace(coord);
                                }
                            }
                        }
                    }
                    // The color blocks live entirely in the map
                    auto colorBlock = std::make_shared<ColorBlock>(codel, processed.size(), std::list<Coords>(std::begin(processed), std::end(processed)));

                    for (const auto &coord: processed)
                    {
                        map.insert(std::make_pair(coord, colorBlock));
                    }
                }
            }
        }

        // TODO:
        // * trace each color block out each of its 8 directions to another color block or dead end (This should probably be done inside of ColorBlock for easier encapsulation)
        // * do determinations on which color blocks and white directions are program exits
        // * Once this is done, the graphical image is no longer necessary and may be thrown away
        // * generate a sort of AST (but more of an Abstract Syntax Map, as the program is 2D) for use with code generation
        std::unordered_set<std::shared_ptr<ColorBlock>> colorBlocks;
        std::transform(std::begin(map), std::end(map), std::inserter(colorBlocks, std::end(colorBlocks)), [](const MapItem &item) {return item.second;});

        // Call setneighbors
        using namespace std::placeholders;
        for (auto &colorBlock: colorBlocks)
        {
            switch (colorBlock->codel.color)
            {
                case Color::Red:
                case Color::Yellow:
                case Color::Green:
                case Color::Cyan:
                case Color::Blue:
                case Color::Magenta:
                    colorBlock->SetNeighbors(field, map);
                    break;
                default:
                    break;
            }
        }

        CC cc = CC::Left;
        DC dc = DC::East;
        std::shared_ptr<ColorBlock> block = map.find(Coords{0, 0})->second;
        std::stack<long long int> stack;
        while (1)
        {
            // If this block has no neighbors, kill it.
            if (std::all_of(std::begin(block->neighbors), std::end(block->neighbors),
                [](const std::array<ColorBlock::Neighbor, 2> &arr)
                {
                    return std::all_of(std::begin(arr), std::end(arr),
                        [](const ColorBlock::Neighbor &n)
                        {
                            return n.block.expired();
                        });
                }))
            {
                return 0;
            }
            bool toggleCC = true;
            while (1)
            {
                auto neighbor = block->neighbors[static_cast<size_t>(dc)][static_cast<size_t>(cc)];
                if (neighbor.block.expired())
                {
                    if (toggleCC)
                        cc = Toggle(cc);
                    else
                        dc = Toggle(dc);
                    toggleCC = !toggleCC;
                } else
                {
                    std::cout << "Running operation " << static_cast<size_t>(neighbor.operation) << " on " << neighbor.block.lock() << " size " << neighbor.block.lock()->size << " with exit " << std::get<0>(neighbor.block.lock()->exits[0][0]) << "x" << std::get<1>(neighbor.block.lock()->exits[0][0]) << std::endl;
                    dc = neighbor.dc;
                    cc = neighbor.cc;
                    switch (neighbor.operation)
                    {
                        case OP::PUSH:
                            {
                                stack.push(block->size);
                                break;
                            }
                        case OP::POP:
                            if (stack.size() >= 1)
                            {
                                stack.pop();
                            }
                            break;
                        case OP::ADD:
                            if (stack.size() >= 2)
                            {
                                const auto first = stack.top();
                                stack.pop();
                                const auto second = stack.top();
                                stack.pop();
                                stack.push(first + second);
                            }
                            break;
                        case OP::SUBTRACT:
                            if (stack.size() >= 2)
                            {
                                const auto first = stack.top();
                                stack.pop();
                                const auto second = stack.top();
                                stack.pop();
                                stack.push(second - first);
                            }
                            break;
                        case OP::MULTIPLY:
                            if (stack.size() >= 2)
                            {
                                const auto first = stack.top();
                                stack.pop();
                                const auto second = stack.top();
                                stack.pop();
                                stack.push(first * second);
                            }
                            break;
                        case OP::DIVIDE:
                            if (stack.size() >= 2)
                            {
                                const auto first = stack.top();
                                stack.pop();
                                const auto second = stack.top();
                                stack.pop();
                                stack.push(second / first);
                            }
                            break;
                        case OP::MOD:
                            if (stack.size() >= 2)
                            {
                                auto first = stack.top();
                                stack.pop();
                                auto second = stack.top();
                                stack.pop();

                                const bool positive = second >= 0;
                                if (!positive)
                                {
                                    second *= -1;
                                    first *= -1;
                                }
                                auto result = second % first;
                                if (!positive)
                                {
                                    result *= -1;
                                }
                                stack.push(result);
                            }
                            break;
                        case OP::NOT:
                            if (stack.size() >= 1)
                            {
                                const auto first = stack.top();
                                stack.pop();
                                stack.push(!first);
                            }
                            break;
                        case OP::GREATER:
                            if (stack.size() >= 2)
                            {
                                const auto first = stack.top();
                                stack.pop();
                                const auto second = stack.top();
                                stack.pop();
                                stack.push(second > first);
                            }
                            break;
                        case OP::POINTER:
                            if (stack.size() >= 1)
                            {
                                auto first = stack.top();
                                stack.pop();
                                if (first > 0)
                                {
                                    dc = static_cast<DC>((static_cast<size_t>(dc) + first) % 4);
                                } else if (first < 0)
                                {
                                    first *= -1;
                                    dc = static_cast<DC>((static_cast<size_t>(dc) + 4 - (first % 4)) % 4);
                                }
                            }
                                break;
                        case OP::SWITCH:
                            if (stack.size() >= 1)
                            {
                                auto first = stack.top();
                                stack.pop();
                                if (first < 0)
                                {
                                    first *= -1;
                                }
                                if (first % 2 == 1)
                                {
                                    cc = Toggle(cc);
                                }
                            }
                                break;
                        case OP::DUPLICATE:
                            if (stack.size() >= 1)
                            {
                                stack.push(stack.top());
                                break;
                            }
                        case OP::ROLL:
                            {
                                auto amount = stack.top();
                                stack.pop();
                                auto depth = stack.top();
                                stack.pop();

                                if (depth < 0 || static_cast<size_t>(depth) >= stack.size())
                                {
                                    stack.push(depth);
                                    stack.push(amount);
                                } else
                                {
                                    // A roll equal to the depth restores the exact same order
                                    amount %= depth;

                                    // List for rolling
                                    std::list<long long int> list;
                                    while (depth--)
                                    {
                                        list.push_back(stack.top());
                                        stack.pop();
                                    }
                                    auto newfirst = std::begin(list);
                                    std::advance(newfirst, amount);
                                    std::rotate(std::begin(list), newfirst, std::end(list));
                                    using namespace std::placeholders;
                                    using Stack = std::stack<long long int>;
                                    std::for_each(list.crbegin(), list.crend(), std::bind(static_cast<void(Stack::*)(const long long int &)>(&Stack::push), &stack, _1));
                                }
                                break;
                            }
                        case OP::INN:
                            {
                                long long int input;
                                std::cin >> input;
                                stack.push(input);
                                break;
                            }
                        case OP::INC:
                            {
                                char input;
                                std::cin >> input;
                                stack.push(input);
                                break;
                            }
                        case OP::OUTN:
                            if (stack.size() >= 1)
                            {
                                std::cout << stack.top();
                                stack.pop();
                            }
                                break;
                        case OP::OUTC:
                            if (stack.size() >= 1)
                            {
                                std::cout << static_cast<char>(stack.top());
                                stack.pop();
                            }
                                break;
                        case OP::EXIT:
                            {
                                return 0;
                                break;
                            }
                        default:
                            break;
                    }
                    block = neighbor.block.lock();
                    break;
                }
            }
        }
    }
    catch (const Magick::Exception &e)
    {
        std::cerr << "Image file " << args::get(input) << " could not be read" << std::endl;
        std::cerr << parser;
        return 1;
    }
    return 0;
}

template <typename Pix>
bool Equals(const Pix &a, const Pix &b)
{
    return a.red == b.red
        && a.green == b.green
        && a.blue == b.blue;
}


Field ImageToField(const std::string filename, size_t codelsize)
{
    Magick::Image image;

    image.read(filename);

    Magick::Pixels pixels(image);

    const auto size = image.size();
    if (codelsize == 0)
    {
        std::set<size_t> linestops{0, size.width(), size.height()};

        // First do each row
        for (size_t y = 0; y < size.height(); ++y)
        {
            auto prevColor = pixels.get(0, y, 1, 1)[0];
            auto row = pixels.get(0, y, size.width(), 1);
            for (size_t x = 0; x < size.width(); ++x)
            {
                auto color = row[x];
                if (!Equals(prevColor, color))
                {
                    prevColor = color;
                    linestops.insert(x);
                }
            }
        }

        // Then each column in the same way
        for (size_t x = 0; x < size.width(); ++x)
        {
            auto prevColor = pixels.get(x, 0, 1, 1)[0];
            auto col = pixels.get(x, 0, 1, size.height());
            for (size_t y = 0; y < size.height(); ++y)
            {
                auto color = col[y];
                if (!Equals(prevColor, color))
                {
                    prevColor = color;
                    linestops.insert(y);
                }
            }
        }

        std::list<size_t> diffs(linestops.size());
        std::adjacent_difference(std::begin(linestops), std::end(linestops), std::begin(diffs));
        diffs.pop_front();
        codelsize = *std::min_element(std::begin(diffs), std::end(diffs));
        std::cout << "autodetected codel size: " << codelsize << std::endl;
    }

    // Each vector member is a row, indexed top down
    Field field = std::vector<std::vector<Codel>>(
            size.height() / codelsize,
            std::vector<Codel>(size.width() / codelsize));

    const std::unordered_map<PixelColor, Codel> colormap{
        {{255, 192, 192}, {Shade::Light, Color::Red}},
            {{255, 0, 0}, {Shade::Normal, Color::Red}},
            {{192, 0, 0}, {Shade::Dark, Color::Red}},
            {{255, 255, 192}, {Shade::Light, Color::Yellow}},
            {{255, 255, 0}, {Shade::Normal, Color::Yellow}},
            {{192, 192, 0}, {Shade::Dark, Color::Yellow}},
            {{192, 255, 192}, {Shade::Light, Color::Green}},
            {{0, 255, 0}, {Shade::Normal, Color::Green}},
            {{0, 192, 0}, {Shade::Dark, Color::Green}},
            {{192, 255, 255}, {Shade::Light, Color::Cyan}},
            {{0, 255, 255}, {Shade::Normal, Color::Cyan}},
            {{0, 192, 192}, {Shade::Dark, Color::Cyan}},
            {{192, 192, 255}, {Shade::Light, Color::Blue}},
            {{0, 0, 255}, {Shade::Normal, Color::Blue}},
            {{0, 0, 192}, {Shade::Dark, Color::Blue}},
            {{255, 192, 255}, {Shade::Light, Color::Magenta}},
            {{255, 0, 255}, {Shade::Normal, Color::Magenta}},
            {{192, 0, 192}, {Shade::Dark, Color::Magenta}},
            {{255, 255, 255}, {Shade::None, Color::White}},
            {{0, 0, 0}, {Shade::None, Color::Black}}};

    for (size_t y = 0, realy = 0; y < size.height(); y += codelsize, ++realy)
    {
        auto row = pixels.get(0, y, size.width(), 1);
        for (size_t x = 0, realx = 0; x < size.width(); x += codelsize, ++realx)
        {
            PixelColor pixelColor(row[x]);
            auto colorit = colormap.find(pixelColor);
            if (colorit == colormap.end())
            {
                std::cerr << "Unknown color found" << std::endl;

                field[realy][realx] = {Shade::None, Color::Unknown};
            } else
            {
                field[realy][realx] = colorit->second;
            }
        }
    }
    return field;
}


void PrintField(const Field &field, const Map &map)
{
    for (size_t y = 0; y < field.size(); ++y)
    {
        const auto &row = field[y];
        for (size_t x = 0; x < row.size(); ++x)
        {
            const auto it = map.find(Coords{x, y});
            if (it == std::end(map))
            {
                std::cout << "   ";
            } else
            {
                std::cout << static_cast<char>(033) << '[';
                const auto &codel = it->second->codel;
                switch (codel.color)
                {
                    case Color::Black:
                        std::cout << "40";
                        break;
                    case Color::Red:
                        std::cout << "41";
                        break;
                    case Color::Green:
                        std::cout << "42";
                        break;
                    case Color::Yellow:
                        std::cout << "43";
                        break;
                    case Color::Blue:
                        std::cout << "44";
                        break;
                    case Color::Magenta:
                        std::cout << "45";
                        break;
                    case Color::Cyan:
                        std::cout << "46";
                        break;
                    case Color::White:
                        std::cout << "47";
                        break;
                    default:
                        std::cout << "47";
                        break;
                }
                std::cout << 'm';
                std::cout << std::setfill('0') << std::setw(3) << it->second->size;
            }
            std::cout << ' ';
            std::cout << static_cast<char>(033) << "[39;49m";
        }
        std::cout << '\n';
    }
}
