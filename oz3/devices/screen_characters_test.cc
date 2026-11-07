// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/screen_characters.h"

#include <cstdint>
#include <string>
#include <vector>

#include "absl/types/span.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/devices/screen.h"

namespace oz3 {
namespace {

using ::testing::Each;
using ::testing::ElementsAre;
using ::testing::ElementsAreArray;

// Returns the pixels of `glyph` in the built-in character set, a string a row,
// with '#' lit and '.' unlit.
std::vector<std::string> GetGlyphPixels(int glyph) {
  const absl::Span<const uint16_t> characters = GetDefaultScreenCharacters();
  std::vector<std::string> pixels;
  for (int y = 0; y < kScreenGlyphHeight; ++y) {
    const uint16_t word = characters[glyph * kScreenGlyphSize + y / 2];
    const int row = y % 2 == 0 ? word >> 8 : word & 0xFF;
    std::string& line = pixels.emplace_back();
    for (int x = 0; x < kScreenGlyphWidth; ++x) {
      line += (row & (0x80 >> x)) != 0 ? '#' : '.';
    }
  }
  return pixels;
}

TEST(ScreenCharactersTest, IsAWholeCharacterSet) {
  EXPECT_EQ(GetDefaultScreenCharacters().size(), kScreenCharacterSetSize);
}

TEST(ScreenCharactersTest, AlternateGlyphsAreTheFirst256) {
  const absl::Span<const uint16_t> characters = GetDefaultScreenCharacters();
  const int half = 256 * kScreenGlyphSize;
  EXPECT_THAT(characters.subspan(half),
              ElementsAreArray(characters.first(half)));
}

TEST(ScreenCharactersTest, AsciiIsFromRow1WithDescendersBelow) {
  EXPECT_THAT(GetGlyphPixels(' '), Each("........"));
  EXPECT_THAT(GetGlyphPixels('A'), ElementsAre("........",  //
                                               ".####...",  //
                                               "##..##..",  //
                                               "##..##..",  //
                                               "######..",  //
                                               "##..##..",  //
                                               "##..##..",  //
                                               "##..##..",  //
                                               "........",  //
                                               "........"));
  EXPECT_THAT(GetGlyphPixels('g'), ElementsAre("........",  //
                                               "........",  //
                                               "........",  //
                                               ".####...",  //
                                               "##..##..",  //
                                               "##..##..",  //
                                               "##..##..",  //
                                               ".#####..",  //
                                               "....##..",  //
                                               ".####..."));
}

TEST(ScreenCharactersTest, SextantsLightTheSixthsOfTheirBits) {
  EXPECT_THAT(GetGlyphPixels(0x80), Each("........"));
  EXPECT_THAT(GetGlyphPixels(0xBF), Each("########"));
  EXPECT_THAT(GetGlyphPixels(0x80 | 0b100001), ElementsAre("####....",  //
                                                           "####....",  //
                                                           "####....",  //
                                                           "........",  //
                                                           "........",  //
                                                           "........",  //
                                                           "........",  //
                                                           "....####",  //
                                                           "....####",  //
                                                           "....####"));
  EXPECT_THAT(GetGlyphPixels(0x80 | 0b001100), ElementsAre("........",  //
                                                           "........",  //
                                                           "........",  //
                                                           "########",  //
                                                           "########",  //
                                                           "########",  //
                                                           "########",  //
                                                           "........",  //
                                                           "........",  //
                                                           "........"));
}

TEST(ScreenCharactersTest, LinesMeetInTheMiddleOfTheCell) {
  EXPECT_THAT(GetGlyphPixels(0xCA), ElementsAre("...##...",  // Single cross
                                                "...##...",  //
                                                "...##...",  //
                                                "...##...",  //
                                                "########",  //
                                                "...##...",  //
                                                "...##...",  //
                                                "...##...",  //
                                                "...##...",  //
                                                "...##..."));
  EXPECT_THAT(GetGlyphPixels(0xCD), ElementsAre("........",  // Double corner
                                                "........",  //
                                                "........",  //
                                                ".#######",  //
                                                ".##.....",  //
                                                ".##..###",  //
                                                ".##..##.",  //
                                                ".##..##.",  //
                                                ".##..##.",  //
                                                ".##..##."));
}

}  // namespace
}  // namespace oz3
