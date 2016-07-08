/* Copyright © 2016 Taylor C. Richberger <taywee@gmx.com>
 * This code is released under the license described in the LICENSE file
 */

#include <iostream>
#include <args.hxx>

int main(const int argc, const char **argv)
{
    args::ArgumentParser parser("A LLVM piet compiler, which can compile a Piet program "
            "into LLVM IR, LLVM bitcode, native assembly, native object code, or an "
            "executable, as well as run the optimized JIT compiled Piet program immediately.");
    args::HelpFlag help(parser, "help", "Display this help menu", {'h', "help"});

    try
    {
        parser.ParseCLI(argc, argv);
    }
    catch (args::Help)
    {
        std::cout << parser;
        return 0;
    }
    catch (args::ParseError e)
    {
        std::cerr << e.what() << std::endl;
        std::cerr << parser;
        return 1;
    }

    return 0;
}
