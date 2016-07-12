/* Copyright © 2016 Taylor C. Richberger <taywee@gmx.com>
 * This code is released under the license described in the LICENSE file
 */

#include <functional>
#include <list>
#include <cstddef>
#include <array>
#include <tuple>
#include <cstdint>
#include <type_traits>

#include <Magick++.h>

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
};

// One-directional between colors with wraparound
Color Difference(Color from, Color to)
{
    using Type = std::underlying_type<Color>::type;
    return static_cast<Color>((static_cast<Type>(to) + 6 - static_cast<Type>(from)) % 6);
}

// One-directional between shades with wraparound
Shade Difference(Shade from, Shade to)
{
    using Type = std::underlying_type<Shade>::type;
    return static_cast<Shade>((static_cast<Type>(to) + 3 - static_cast<Type>(from)) % 3);
}

// One-directional difference between two full codels with wraparound
Codel Difference(const Codel &from, const Codel &to)
{
    return {Difference(from.shade, to.shade), Difference(from.color, to.color)};
}

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

using Coords = std::tuple<std::size_t, std::size_t>;

template <std::size_t element>
bool Compare(const Coords &first, const Coords &second)
{
    return std::get<element>(first) < std::get<element>(second);
}

struct ColorBlock
{
    enum class DC {
        North,
        South,
        East,
        West
    };
    enum class CC {
        Left,
        Right
    };
    const Codel codel;
    const size_t size;
    std::array<std::array<Coords, 2>, 4> exits;

    ColorBlock(const ColorBlock &other) : codel(other.codel), size(other.size)
    {
        exits = other.exits;
    }

    ColorBlock(ColorBlock &&other) : codel(std::move(other.codel)), size(other.size)
    {
        exits = other.exits;
    }

    template <typename List = std::list<Coords>>
    ColorBlock(const Codel &codel, const size_t size, List codels) : codel(codel), size(size)
    {
        // Ignore setting exits if white or black
        if (codel.color != Color::Black && codel.color != Color::White)
        {
            // Extremes in each direction, accessible by DC
            std::size_t extremes[4];
            extremes[static_cast<unsigned int>(DC::East)] = std::get<0>(*std::max_element(std::begin(codels), std::end(codels), Compare<0>));
            extremes[static_cast<unsigned int>(DC::West)] = std::get<0>(*std::min_element(std::begin(codels), std::end(codels), Compare<0>));
            extremes[static_cast<unsigned int>(DC::South)] = std::get<1>(*std::max_element(std::begin(codels), std::end(codels), Compare<1>));
            extremes[static_cast<unsigned int>(DC::North)] = std::get<1>(*std::min_element(std::begin(codels), std::end(codels), Compare<1>));

            // Get L and R values for each extreme
            // East
            auto end = std::remove_if(std::begin(codels), std::end(codels), [&extremes](const Coords &coord) -> bool {
                return extremes[static_cast<unsigned int>(DC::East)] != std::get<0>(coord);
                });
            // L is the far north side when facing East (far north is the LOWEST Y)
            exits[static_cast<unsigned int>(DC::East)][static_cast<unsigned int>(CC::Left)] = *std::min_element(std::begin(codels), end, Compare<1>);
            // R is the far south side when facing East (far south is the HIGHEST Y)
            exits[static_cast<unsigned int>(DC::East)][static_cast<unsigned int>(CC::Right)] = *std::max_element(std::begin(codels), end, Compare<1>);

            // West
            end = std::remove_if(std::begin(codels), std::end(codels), [&extremes](const Coords &coord) -> bool {
                return extremes[static_cast<unsigned int>(DC::West)] != std::get<0>(coord);
                });
            // L is the far south side when facing West
            exits[static_cast<unsigned int>(DC::West)][static_cast<unsigned int>(CC::Left)] = *std::max_element(std::begin(codels), end, Compare<1>);
            // R is the far north side when facing West
            exits[static_cast<unsigned int>(DC::West)][static_cast<unsigned int>(CC::Right)] = *std::min_element(std::begin(codels), end, Compare<1>);

            // North
            end = std::remove_if(std::begin(codels), std::end(codels), [&extremes](const Coords &coord) -> bool {
                return extremes[static_cast<unsigned int>(DC::North)] != std::get<1>(coord);
                });
            // L is the far west side when facing North
            exits[static_cast<unsigned int>(DC::North)][static_cast<unsigned int>(CC::Left)] = *std::min_element(std::begin(codels), end, Compare<0>);
            // R is the far east side when facing North
            exits[static_cast<unsigned int>(DC::North)][static_cast<unsigned int>(CC::Right)] = *std::max_element(std::begin(codels), end, Compare<0>);

            // South
            end = std::remove_if(std::begin(codels), std::end(codels), [&extremes](const Coords &coord) -> bool {
                return extremes[static_cast<unsigned int>(DC::South)] != std::get<1>(coord);
                });
            // L is the far east side when facing South
            exits[static_cast<unsigned int>(DC::South)][static_cast<unsigned int>(CC::Left)] = *std::max_element(std::begin(codels), end, Compare<0>);
            // R is the far west side when facing South
            exits[static_cast<unsigned int>(DC::South)][static_cast<unsigned int>(CC::Right)] = *std::min_element(std::begin(codels), end, Compare<0>);
        }
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
