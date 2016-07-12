/* Copyright © 2016 Taylor C. Richberger <taywee@gmx.com>
 * This code is released under the license described in the LICENSE file
 */

#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <list>
#include <memory>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <set>
#include <vector>

#include <Magick++.h>

using std::size_t;

enum class DC {
    North,
    East,
    South,
    West
};

enum class CC {
    Left,
    Right
};

enum class OP {
    NOP,
    PUSH,
    POP,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    MOD,
    NOT,
    GREATER,
    POINTER,
    SWITCH,
    DUPLICATE,
    ROLL,
    INN,
    INC,
    OUTN,
    OUTC,
    EXIT
};

const static std::array<std::array<OP, 3>, 6> OpTable {{
    {{OP::NOP, OP::PUSH, OP::POP}},
    {{OP::ADD, OP::SUBTRACT, OP::MULTIPLY}},
    {{OP::DIVIDE, OP::MOD, OP::NOT}},
    {{OP::GREATER, OP::POINTER, OP::SWITCH}},
    {{OP::DUPLICATE, OP::ROLL, OP::INN}},
    {{OP::INC, OP::OUTN, OP::OUTC}}
}};

enum class Color {
    Red,
    Yellow,
    Green,
    Cyan,
    Blue,
    Magenta,
    White, // These aren't used in the compiled output
    Black,
    Unknown // Not used in final output.  Compiler will decide what to do with this.
};

enum class Shade {
    Light,
    Normal,
    Dark,
    None
};

struct Codel {
    Shade shade;
    Color color;
    bool operator==(const Codel &other) const
    {
        return color == other.color && shade == other.shade;
    }

    // One-directional difference between two full codels with wraparound
    Codel operator-(const Codel &other) const
    {
        return {
            static_cast<Shade>((static_cast<size_t>(shade) + 3 - static_cast<size_t>(other.shade)) % 3),
            static_cast<Color>((static_cast<size_t>(color) + 6 - static_cast<size_t>(other.color)) % 6)
        };
    }
};

using Field = std::vector<std::vector<Codel>>;

using std::uint8_t;

struct PixelColor {
    const uint8_t red;
    const uint8_t green;
    const uint8_t blue;
    PixelColor(uint8_t red, uint8_t green, uint8_t blue) :
        red(red),
        green(green),
        blue(blue)
    {}

    PixelColor(const Magick::PixelPacket &pix) :
        red(static_cast<size_t>(pix.red) >> (8 * (sizeof(pix.red) - 1)) & 0x00000000000000FFL),
        green(static_cast<size_t>(pix.green) >> (8 * (sizeof(pix.green) - 1)) & 0x00000000000000FFL),
        blue(static_cast<size_t>(pix.blue) >> (8 * (sizeof(pix.blue) - 1)) & 0x00000000000000FFL)
    {}

    bool operator==(const PixelColor &other) const noexcept
    {
        return red == other.red
            && blue == other.blue
            && green == other.green;
    }
};

namespace std
{
    template<> struct hash<PixelColor>
    {
        typedef PixelColor argument_type;
        typedef std::size_t result_type;
        result_type operator()(argument_type const &pixelColor) const
        {
            const uint32_t red = (static_cast<uint32_t>(pixelColor.red) << 16) & 0x00FF0000;
            const uint32_t green = (static_cast<uint32_t>(pixelColor.green) << 8) & 0x0000FF00;
            const uint32_t blue = static_cast<uint32_t>(pixelColor.blue) & 0x000000FF;
            return red | green | blue;
        }
    };
}

using Coords = std::tuple<std::size_t, std::size_t>;

namespace std
{
    template<> struct hash<Coords>
    {
        typedef Coords argument_type;
        typedef std::size_t result_type;
        result_type operator()(argument_type const &coords) const
        {
            // Based on boost hash combine function, replacing "seed" with coords x, so it just combines a pair of ashes
            return std::get<1>(coords) + 0x9e3779b9 + (std::get<0>(coords) << 6) + (std::get<0>(coords) >> 2);
        }
    };
}

template <std::size_t element>
bool Compare(const Coords &first, const Coords &second)
{
    return std::get<element>(first) < std::get<element>(second);
}

struct ColorBlock;

using Map = std::unordered_map<Coords, std::shared_ptr<ColorBlock>>;
using MapItem = std::pair<const Coords, std::shared_ptr<ColorBlock>>;

static std::tuple<std::shared_ptr<ColorBlock>, DC, CC, bool> TraceWhite(Coords coords, const Field &field, Map &map, DC dc, CC cc);

struct ColorBlock
{
    const Codel codel;
    const size_t size;
    std::array<std::array<Coords, 2>, 4> exits;
    struct Neighbor
    {
        std::weak_ptr<ColorBlock> block;
        OP operation;
        DC dc;
        CC cc;
    };
    std::array<std::array<Neighbor, 2>, 4> neighbors;

    ColorBlock(const ColorBlock &other) : codel(other.codel), size(other.size)
    {
        exits = other.exits;
        neighbors = other.neighbors;
    }

    ColorBlock(ColorBlock &&other) : codel(std::move(other.codel)), size(other.size)
    {
        exits = other.exits;
        neighbors = other.neighbors;
    }

    template <typename List = std::list<Coords>>
    ColorBlock(const Codel &codel, const size_t size, List codels) : codel(codel), size(size)
    {
        // Ignore setting exits if white or black
        if (codel.color != Color::Black && codel.color != Color::White)
        {
            // Extremes in each direction, accessible by DC
            std::size_t extremes[4];
            extremes[static_cast<size_t>(DC::East)] = std::get<0>(*std::max_element(std::begin(codels), std::end(codels), Compare<0>));
            extremes[static_cast<size_t>(DC::West)] = std::get<0>(*std::min_element(std::begin(codels), std::end(codels), Compare<0>));
            extremes[static_cast<size_t>(DC::South)] = std::get<1>(*std::max_element(std::begin(codels), std::end(codels), Compare<1>));
            extremes[static_cast<size_t>(DC::North)] = std::get<1>(*std::min_element(std::begin(codels), std::end(codels), Compare<1>));

            List easts;
            List wests;
            List norths;
            List souths;

            std::copy_if(std::begin(codels), std::end(codels), std::back_inserter(easts),
                [&extremes](const Coords &coord)
                {
                    return extremes[static_cast<size_t>(DC::East)] == std::get<0>(coord);
                });

            std::copy_if(std::begin(codels), std::end(codels), std::back_inserter(wests),
                [&extremes](const Coords &coord)
                {
                    return extremes[static_cast<size_t>(DC::West)] == std::get<0>(coord);
                });

            std::copy_if(std::begin(codels), std::end(codels), std::back_inserter(norths),
                [&extremes](const Coords &coord)
                {
                    return extremes[static_cast<size_t>(DC::North)] == std::get<1>(coord);
                });

            std::copy_if(std::begin(codels), std::end(codels), std::back_inserter(souths),
                [&extremes](const Coords &coord)
                {
                    return extremes[static_cast<size_t>(DC::South)] == std::get<1>(coord);
                });

            // Get L and R values for each extreme
            // East
            // L is the far north side when facing East (far north is the LOWEST Y)
            exits[static_cast<size_t>(DC::East)][static_cast<size_t>(CC::Left)] = *std::min_element(std::begin(easts), std::end(easts), Compare<1>);
            // R is the far south side when facing East (far south is the HIGHEST Y)
            exits[static_cast<size_t>(DC::East)][static_cast<size_t>(CC::Right)] = *std::max_element(std::begin(easts), std::end(easts), Compare<1>);

            // West
            // L is the far south side when facing West
            exits[static_cast<size_t>(DC::West)][static_cast<size_t>(CC::Left)] = *std::max_element(std::begin(wests), std::end(wests), Compare<1>);
            // R is the far north side when facing West
            exits[static_cast<size_t>(DC::West)][static_cast<size_t>(CC::Right)] = *std::min_element(std::begin(wests), std::end(wests), Compare<1>);

            // North
            // L is the far west side when facing North
            exits[static_cast<size_t>(DC::North)][static_cast<size_t>(CC::Left)] = *std::min_element(std::begin(norths), std::end(norths), Compare<0>);
            // R is the far east side when facing North
            exits[static_cast<size_t>(DC::North)][static_cast<size_t>(CC::Right)] = *std::max_element(std::begin(norths), std::end(norths), Compare<0>);

            // South
            // L is the far east side when facing South
            exits[static_cast<size_t>(DC::South)][static_cast<size_t>(CC::Left)] = *std::max_element(std::begin(souths), std::end(souths), Compare<0>);
            // R is the far west side when facing South
            exits[static_cast<size_t>(DC::South)][static_cast<size_t>(CC::Right)] = *std::min_element(std::begin(souths), std::end(souths), Compare<0>);
        }
    }

    void SetNeighbors(const Field &field, Map &map)
    {
        const static auto GetNeighbor = [&field, &map](const Coords &coords, const Codel &codel, const DC dc, const CC cc)
        {
            auto neighbor = map.find(coords)->second;
            switch (neighbor->codel.color)
            {
                case Color::Black:
                    return Neighbor{{}, OP::NOP, DC::North, CC::Left};
                    break;
                case Color::White:
                    {
                        std::shared_ptr<ColorBlock> neighbor;
                        DC ndc;
                        CC ncc;
                        bool exit;
                        std::tie(neighbor, ndc, ncc, exit) = TraceWhite(coords, field, map, dc, cc);

                        return Neighbor{neighbor, (exit? OP::EXIT : OP::NOP), ndc, ncc};
                        break;
                    }
                default:
                    {
                        auto diff = neighbor->codel - codel;

                        return Neighbor{neighbor, OpTable[static_cast<size_t>(diff.color)][static_cast<size_t>(diff.shade)], dc, cc};
                        break;
                    }
            }
        };

        {
            const auto &el = exits[static_cast<size_t>(DC::East)][static_cast<size_t>(CC::Left)];
            const auto &er = exits[static_cast<size_t>(DC::East)][static_cast<size_t>(CC::Right)];
            if (std::get<0>(el) < field[0].size() - 1)
            {
                const auto l = std::make_tuple(std::get<0>(el) + 1, std::get<1>(el));
                const auto r = std::make_tuple(std::get<0>(er) + 1, std::get<1>(er));
                neighbors[static_cast<size_t>(DC::East)][static_cast<size_t>(CC::Left)] = GetNeighbor(l, codel, DC::East, CC::Left);
                neighbors[static_cast<size_t>(DC::East)][static_cast<size_t>(CC::Right)] = GetNeighbor(r, codel, DC::East, CC::Right);
            }
        }
        {
            const auto &sl = exits[static_cast<size_t>(DC::South)][static_cast<size_t>(CC::Left)];
            const auto &sr = exits[static_cast<size_t>(DC::South)][static_cast<size_t>(CC::Right)];
            if (std::get<1>(sl) < (field.size() - 1))
            {
                const auto l = std::make_tuple(std::get<0>(sl), std::get<1>(sl) + 1);
                const auto r = std::make_tuple(std::get<0>(sr), std::get<1>(sr) + 1);
                neighbors[static_cast<size_t>(DC::South)][static_cast<size_t>(CC::Left)] = GetNeighbor(l, codel, DC::South, CC::Left);
                neighbors[static_cast<size_t>(DC::South)][static_cast<size_t>(CC::Right)] = GetNeighbor(r, codel, DC::South, CC::Right);
            }
        }
        {
            const auto &wl = exits[static_cast<size_t>(DC::West)][static_cast<size_t>(CC::Left)];
            const auto &wr = exits[static_cast<size_t>(DC::West)][static_cast<size_t>(CC::Right)];
            if (std::get<0>(wl) > 0)
            {
                const auto l = std::make_tuple(std::get<0>(wl) - 1, std::get<1>(wl));
                const auto r = std::make_tuple(std::get<0>(wr) - 1, std::get<1>(wr));
                neighbors[static_cast<size_t>(DC::West)][static_cast<size_t>(CC::Left)] = GetNeighbor(l, codel, DC::West, CC::Left);
                neighbors[static_cast<size_t>(DC::West)][static_cast<size_t>(CC::Right)] = GetNeighbor(r, codel, DC::West, CC::Right);
            }
        }
        {
            const auto &nl = exits[static_cast<size_t>(DC::North)][static_cast<size_t>(CC::Left)];
            const auto &nr = exits[static_cast<size_t>(DC::North)][static_cast<size_t>(CC::Right)];
            if (std::get<1>(nl) > 0)
            {
                const auto l = std::make_tuple(std::get<0>(nl), std::get<1>(nl) - 1);
                const auto r = std::make_tuple(std::get<0>(nr), std::get<1>(nr) - 1);
                neighbors[static_cast<size_t>(DC::North)][static_cast<size_t>(CC::Left)] = GetNeighbor(l, codel, DC::North, CC::Left);
                neighbors[static_cast<size_t>(DC::North)][static_cast<size_t>(CC::Right)] = GetNeighbor(r, codel, DC::North, CC::Right);
            }
        }
    }
};

inline CC Toggle(CC cc)
{
    switch (cc)
    {
        case CC::Left:
            return CC::Right;
            break;
        case CC::Right:
            return CC::Left;
            break;
    }
}

inline DC Toggle(DC dc)
{
    switch (dc)
    {
        case DC::North:
            return DC::East;
            break;
        case DC::East:
            return DC::South;
            break;
        case DC::South:
            return DC::West;
            break;
        case DC::West:
            return DC::North;
            break;
    }
}

// Walk through whites until you either hit a color block or you start re-walking your steps
std::tuple<std::shared_ptr<ColorBlock>, DC, CC, bool> TraceWhite(Coords coords, const Field &field, Map &map, DC dc, CC cc)
{
    using BlockMove = std::tuple<Coords, DC, CC>;
    using Traversed = std::set<BlockMove>;
    Traversed traversed;

    while (1)
    {
        Traversed::iterator it;
        bool inserted;
        std::tie(it, inserted) = traversed.insert(BlockMove(coords, dc, cc));
        if (inserted)
        {
            Coords next;
            bool restricted = false;
            switch (dc)
            {
                case DC::North:
                    if (std::get<1>(coords) > 0)
                        next = std::make_tuple(std::get<0>(coords), std::get<1>(coords) - 1);
                    else
                        restricted = true;
                    break;
                case DC::East:
                    if (std::get<0>(coords) < (field[0].size() - 1))
                        next = std::make_tuple(std::get<0>(coords) + 1, std::get<1>(coords));
                    else
                        restricted = true;
                    break;
                case DC::South:
                    if (std::get<1>(coords) < (field.size() - 1))
                        next = std::make_tuple(std::get<0>(coords), std::get<1>(coords) + 1);
                    else
                        restricted = true;
                    break;
                case DC::West:
                    if (std::get<0>(coords) > 0)
                        next = std::make_tuple(std::get<0>(coords) - 1, std::get<1>(coords));
                    else
                        restricted = true;
                    break;
            }
            if (!restricted)
            {
                auto nextblock = map.find(next)->second;
                switch (nextblock->codel.color)
                {
                    case Color::Black:
                        restricted = true;
                        break;
                    case Color::White:
                        restricted = false;
                        break;
                    default:
                        return std::make_tuple(std::shared_ptr<ColorBlock>{nextblock}, dc, cc, false);
                }
            }

            // This "redundant" check is necessary, because the "next" value
            // can change the restricted status
            if (restricted)
            {
                dc = Toggle(dc);
                cc = Toggle(cc);
            } else
            {
                coords = next;
            }
        } else
        {
            return std::make_tuple(std::shared_ptr<ColorBlock>{}, dc, cc, true);
        }
    }
}

