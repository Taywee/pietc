/* Copyright © 2016 Taylor C. Richberger <taywee@gmx.com>
 * This code is released under the license described in the LICENSE file
 */

#include <iostream>
#include <set>
#include <list>
#include <algorithm>
#include <numeric>

#include <args.hxx>
#include <Magick++.h>

template <typename Pix>
bool Equals(const Pix &a, const Pix &b)
{
    return a.red == b.red
        && a.green == b.green
        && a.blue == b.blue;
}

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

    Magick::Image image;

    try {
        image.read(args::get(input));
    }
    catch (const Magick::Exception &e)
    {
        std::cerr << "Image file " << args::get(input) << " could not be read" << std::endl;
        std::cerr << parser;
        return 1;
    }

    size_t codelsize = args::get(cs);
    Magick::Pixels pixels(image);

    if (codelsize == 0)
    {
        std::set<size_t> linestops;
        const auto size = image.size();

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
    }

    return 0;
}
