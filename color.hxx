/* Copyright © 2016 Taylor C. Richberger <taywee@gmx.com>
 * This code is released under the license described in the LICENSE file
 */

#include <functional>
#include <cstddef>
#include <cstdint>

#include <Magick++.h>

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
    None,
    Light,
    Normal,
    Dark
};

struct Codel {
    Shade shade;
    Color color;
};

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
