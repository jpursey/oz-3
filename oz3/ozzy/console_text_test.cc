// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/console_text.h"

#include <cstdint>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <vector>

#include "absl/strings/str_cat.h"
#include "gb/base/unicode.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/devices/screen.h"

namespace oz3 {
namespace {

//==============================================================================
// Helpers
//==============================================================================

// Returns a control sequence: `sequence`, after the escape and '['.
std::string Csi(std::string_view sequence) {
  return absl::StrCat("\x1B[", sequence);
}

// Returns the code point of the text of `character`, or 0 if it isn't UTF-8
// for one code point.
int GetCharacterCodePoint(int character) {
  std::string_view text = GetCharacterText(static_cast<uint8_t>(character));
  const std::optional<char32_t> code_point = gb::ConsumeCodePoint(&text);
  if (!code_point.has_value() || !text.empty()) {
    return 0;
  }
  return static_cast<int>(*code_point);
}

// Returns a picture of `rows` of plain characters, all the same width, with
// the cursor off.
ScreenPicture MakePicture(const std::vector<std::string_view>& rows) {
  ScreenPicture picture;
  picture.columns = static_cast<int>(rows[0].size());
  picture.rows = static_cast<int>(rows.size());
  for (std::string_view row : rows) {
    for (char character : row) {
      picture.cells.push_back(static_cast<uint8_t>(character));
    }
  }
  return picture;
}

// The sequence that hides the cursor.
constexpr std::string_view kHideCursor = "\x1B[?25l";

// Returns the text that draws `rows` of plain text from the top, and then
// `cursor`, as DrawConsoleText does when it draws every row.
std::string DrawAll(const std::vector<std::string_view>& rows,
                    std::string_view cursor = kHideCursor) {
  std::string text = absl::StrCat(kHideCursor, Csi("2J"));
  for (int row = 0; row < static_cast<int>(rows.size()); ++row) {
    absl::StrAppend(&text, Csi(absl::StrCat(row + 1, ";1H")), rows[row]);
  }
  absl::StrAppend(&text, cursor);
  return text;
}

//==============================================================================
// GetCharacterText
//==============================================================================

TEST(ConsoleTextTest, EveryCharacterIsOnePrintableCharacter) {
  for (int character = 0; character < 256; ++character) {
    SCOPED_TRACE(character);
    const int code_point = GetCharacterCodePoint(character);
    EXPECT_GE(code_point, 0x20);
    EXPECT_NE(code_point, 0x7F);
    EXPECT_FALSE(code_point >= 0x80 && code_point < 0xA0) << "A C1 control";
  }
}

TEST(ConsoleTextTest, AsciiIsItself) {
  for (int character = 0x20; character < 0x7F; ++character) {
    EXPECT_EQ(GetCharacterText(static_cast<uint8_t>(character)),
              std::string(1, static_cast<char>(character)));
  }
}

TEST(ConsoleTextTest, Graphics) {
  EXPECT_EQ(GetCharacterCodePoint(0x00), 0x25AF);
  EXPECT_EQ(GetCharacterCodePoint(0x01), 0x258C);
  EXPECT_EQ(GetCharacterCodePoint(0x0C), 0x254B);
  EXPECT_EQ(GetCharacterCodePoint(0x13), 0x00BC);
  EXPECT_EQ(GetCharacterCodePoint(0x16), 0x2588);
  EXPECT_EQ(GetCharacterCodePoint(0x1D), 0x00BD);
  EXPECT_EQ(GetCharacterCodePoint(0x1F), 0x251B);
  EXPECT_EQ(GetCharacterCodePoint(0x7F), 0x2592);
}

TEST(ConsoleTextTest, Sextants) {
  // The blank, the halves, and the full sextant are other characters, and
  // Unicode's sextants are the rest, in order.
  EXPECT_EQ(GetCharacterCodePoint(0x80), 0x0020);
  EXPECT_EQ(GetCharacterCodePoint(0x81), 0x1FB00);
  EXPECT_EQ(GetCharacterCodePoint(0x94), 0x1FB13);
  EXPECT_EQ(GetCharacterCodePoint(0x95), 0x258C);
  EXPECT_EQ(GetCharacterCodePoint(0x96), 0x1FB14);
  EXPECT_EQ(GetCharacterCodePoint(0xA9), 0x1FB27);
  EXPECT_EQ(GetCharacterCodePoint(0xAA), 0x2590);
  EXPECT_EQ(GetCharacterCodePoint(0xAB), 0x1FB28);
  EXPECT_EQ(GetCharacterCodePoint(0xBE), 0x1FB3B);
  EXPECT_EQ(GetCharacterCodePoint(0xBF), 0x2588);

  // Each sextant is its own character.
  std::set<int> sextants;
  for (int character = 0x80; character <= 0xBF; ++character) {
    sextants.insert(GetCharacterCodePoint(character));
  }
  EXPECT_EQ(sextants.size(), 64);
}

TEST(ConsoleTextTest, Symbols) {
  EXPECT_EQ(GetCharacterCodePoint(0xC0), 0x2500);
  EXPECT_EQ(GetCharacterCodePoint(0xCA), 0x253C);
  EXPECT_EQ(GetCharacterCodePoint(0xCB), 0x2550);
  EXPECT_EQ(GetCharacterCodePoint(0xD5), 0x256C);
  EXPECT_EQ(GetCharacterCodePoint(0xD6), 0x2574);
  EXPECT_EQ(GetCharacterCodePoint(0xDA), 0x256D);
  EXPECT_EQ(GetCharacterCodePoint(0xDE), 0x2591);
  EXPECT_EQ(GetCharacterCodePoint(0xE1), 0x2580);
  EXPECT_EQ(GetCharacterCodePoint(0xE3), 0x2571);
  EXPECT_EQ(GetCharacterCodePoint(0xE6), 0x25E2);
  EXPECT_EQ(GetCharacterCodePoint(0xEA), 0x2190);
  EXPECT_EQ(GetCharacterCodePoint(0xEE), 0x2660);
  EXPECT_EQ(GetCharacterCodePoint(0xF2), 0x2022);
  EXPECT_EQ(GetCharacterCodePoint(0xFC), 0x00A3);
  EXPECT_EQ(GetCharacterCodePoint(0xFF), 0x2195);
}

//==============================================================================
// Drawing rows
//==============================================================================

TEST(ConsoleTextTest, DrawsEveryRowFirst) {
  const ScreenPicture picture = MakePicture({"ab", "cd"});
  EXPECT_EQ(DrawConsoleText(picture, nullptr), DrawAll({"ab", "cd"}));
}

TEST(ConsoleTextTest, DrawsCharactersAsText) {
  ScreenPicture picture = MakePicture({"ab"});
  picture.cells = {0xC0, 0x81};
  EXPECT_EQ(
      DrawConsoleText(picture, nullptr),
      DrawAll({absl::StrCat(GetCharacterText(0xC0), GetCharacterText(0x81))}));
}

TEST(ConsoleTextTest, Attributes) {
  struct Case {
    uint16_t attributes;
    std::string_view style;
  };
  const Case kCases[] = {
      {kScreenInverse, "0;7m"},
      {kScreenHalfIntensity, "0;2m"},
      {kScreenBlink, "0;5m"},
      {kScreenLineBottom, "0;4m"},
      {kScreenLineTop, "0;53m"},
      {kScreenInverse | kScreenHalfIntensity | kScreenBlink |
           kScreenLineBottom | kScreenLineTop,
       "0;7;2;5;4;53m"},
  };
  for (const Case& test_case : kCases) {
    SCOPED_TRACE(test_case.style);
    ScreenPicture picture = MakePicture({"ab"});
    picture.cells[0] |= test_case.attributes;
    EXPECT_EQ(
        DrawConsoleText(picture, nullptr),
        absl::StrCat(Csi("?25l"), Csi("2J"), Csi("1;1H"), Csi(test_case.style),
                     "a", Csi("0m"), "b", Csi("?25l")));
  }
}

TEST(ConsoleTextTest, AttributesTextCantShow) {
  // The left and right lines, and the alternate character set, draw as plain
  // characters.
  ScreenPicture picture = MakePicture({"abc"});
  picture.cells[0] |= kScreenLineLeft;
  picture.cells[1] |= kScreenLineRight;
  picture.cells[2] |= kScreenAlternate;
  EXPECT_EQ(DrawConsoleText(picture, nullptr), DrawAll({"abc"}));
}

TEST(ConsoleTextTest, RunsShareTheirStyle) {
  // Each run of cells with the same attributes sets them once, and each row
  // ends in the default style.
  ScreenPicture picture = MakePicture({"abcd", "efgh"});
  picture.cells[1] |= kScreenInverse;
  picture.cells[2] |= kScreenInverse;
  picture.cells[3] |= kScreenBlink;
  picture.cells[4] |= kScreenBlink;
  picture.cells[5] |= kScreenBlink;
  EXPECT_EQ(DrawConsoleText(picture, nullptr),
            absl::StrCat(Csi("?25l"), Csi("2J"), Csi("1;1H"), "a", Csi("0;7m"),
                         "bc", Csi("0;5m"), "d", Csi("0m"), Csi("2;1H"),
                         Csi("0;5m"), "ef", Csi("0m"), "gh", Csi("?25l")));
}

TEST(ConsoleTextTest, DrawsOnlyChangedRows) {
  const ScreenPicture last = MakePicture({"ab", "cd", "ef"});
  ScreenPicture picture = MakePicture({"ab", "xd", "ef"});
  EXPECT_EQ(DrawConsoleText(picture, &last),
            absl::StrCat(Csi("?25l"), Csi("2;1H"), "xd", Csi("?25l")));

  // A change of attributes alone is a change.
  picture = last;
  picture.cells[5] |= kScreenInverse;
  EXPECT_EQ(DrawConsoleText(picture, &last),
            absl::StrCat(Csi("?25l"), Csi("3;1H"), "e", Csi("0;7m"), "f",
                         Csi("0m"), Csi("?25l")));
}

TEST(ConsoleTextTest, NothingChanged) {
  ScreenPicture picture = MakePicture({"ab", "cd"});
  picture.cursor_mode = ScreenCursorMode::kOn;
  EXPECT_EQ(DrawConsoleText(picture, &picture), "");

  // The pixels aren't drawn, so they make no difference.
  ScreenPicture last = picture;
  last.pixels = {kScreenPixelBright};
  EXPECT_EQ(DrawConsoleText(picture, &last), "");
}

TEST(ConsoleTextTest, DifferentSizeDrawsEveryRow) {
  const ScreenPicture picture = MakePicture({"ab", "cd"});
  const ScreenPicture wider = MakePicture({"abc", "cde"});
  EXPECT_EQ(DrawConsoleText(picture, &wider), DrawAll({"ab", "cd"}));
  const ScreenPicture taller = MakePicture({"ab", "cd", "ef"});
  EXPECT_EQ(DrawConsoleText(picture, &taller), DrawAll({"ab", "cd"}));
}

//==============================================================================
// The cursor
//==============================================================================

TEST(ConsoleTextTest, CursorShapes) {
  struct Case {
    ScreenCursorMode mode;
    ScreenCursorStyle style;
    std::string_view shape;
  };
  const Case kCases[] = {
      {ScreenCursorMode::kSlowBlink, ScreenCursorStyle::kBlock, "1 q"},
      {ScreenCursorMode::kOn, ScreenCursorStyle::kBlock, "2 q"},
      {ScreenCursorMode::kFastBlink, ScreenCursorStyle::kInverse, "1 q"},
      {ScreenCursorMode::kOn, ScreenCursorStyle::kInverse, "2 q"},
      {ScreenCursorMode::kSlowBlink, ScreenCursorStyle::kUnderline, "3 q"},
      {ScreenCursorMode::kOn, ScreenCursorStyle::kUnderline, "4 q"},
      {ScreenCursorMode::kFastBlink, ScreenCursorStyle::kLeftEdge, "5 q"},
      {ScreenCursorMode::kOn, ScreenCursorStyle::kLeftEdge, "6 q"},
  };
  for (const Case& test_case : kCases) {
    SCOPED_TRACE(test_case.shape);
    ScreenPicture picture = MakePicture({"abcd", "efgh"});
    picture.cursor_column = 3;
    picture.cursor_row = 1;
    picture.cursor_mode = test_case.mode;
    picture.cursor_style = test_case.style;
    EXPECT_EQ(
        DrawConsoleText(picture, nullptr),
        DrawAll({"abcd", "efgh"},
                absl::StrCat(Csi("2;4H"), Csi(test_case.shape), Csi("?25h"))));
  }
}

TEST(ConsoleTextTest, CursorChangedAlone) {
  // The cursor moves, or hides, without drawing any rows.
  ScreenPicture last = MakePicture({"ab", "cd"});
  last.cursor_mode = ScreenCursorMode::kOn;
  ScreenPicture picture = last;
  picture.cursor_column = 1;
  EXPECT_EQ(DrawConsoleText(picture, &last),
            absl::StrCat(Csi("1;2H"), Csi("2 q"), Csi("?25h")));

  picture = last;
  picture.cursor_mode = ScreenCursorMode::kOff;
  EXPECT_EQ(DrawConsoleText(picture, &last), Csi("?25l"));
}

TEST(ConsoleTextTest, CursorShownAfterChangedRows) {
  // Drawing rows moves the console's cursor, so it is put back, even where the
  // picture's cursor didn't change.
  ScreenPicture last = MakePicture({"ab", "cd"});
  last.cursor_mode = ScreenCursorMode::kSlowBlink;
  last.cursor_style = ScreenCursorStyle::kUnderline;
  ScreenPicture picture = last;
  picture.cells[0] = 'x';
  EXPECT_EQ(DrawConsoleText(picture, &last),
            absl::StrCat(Csi("?25l"), Csi("1;1H"), "xb", Csi("1;1H"),
                         Csi("3 q"), Csi("?25h")));
}

TEST(ConsoleTextTest, TeletypeTextKeepsPrintableCharacters) {
  EXPECT_EQ(GetTeletypeText(" Hello, world! ~"), " Hello, world! ~");
  EXPECT_EQ(GetTeletypeText(""), "");
}

TEST(ConsoleTextTest, TeletypeTextKeepsWhatTeletypesActOn) {
  EXPECT_EQ(GetTeletypeText("\a\b\t\n\r"), "\a\b\t\n\r");
  EXPECT_EQ(GetTeletypeText("ab\bc\r\n"), "ab\bc\r\n");
}

TEST(ConsoleTextTest, TeletypeTextDropsOtherControlCharacters) {
  EXPECT_EQ(GetTeletypeText("\x1B[2J"), "[2J");
  EXPECT_EQ(GetTeletypeText(std::string_view("a\x00\x01\x06\x0B\x0C\x0E", 7)),
            "a");
  EXPECT_EQ(GetTeletypeText("\x0F\x1A\x1F\x7F"
                            "b"),
            "b");
}

}  // namespace
}  // namespace oz3
