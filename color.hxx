/* Copyright © 2016 Taylor C. Richberger <taywee@gmx.com>
 * This code is released under the license described in the LICENSE file
 */

enum class Color {
    Unknown, // Not used in final output.  Compiler will decide what to do with this.
    Red,
    Yellow,
    Green,
    Cyan,
    Blue,
    Magenta,
    White, // These aren't used in the compiled output
    Black
};

enum class Shade {
    Unknown,
    Light,
    Normal,
    Dark
};

struct Codel {
    Color color;
    Shade shade;
};
