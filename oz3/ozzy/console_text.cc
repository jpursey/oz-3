// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/console_text.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <string>
#include <string_view>

#include "absl/base/no_destructor.h"
#include "absl/log/check.h"
#include "absl/strings/str_cat.h"
#include "gb/base/unicode.h"
#include "oz3/devices/screen.h"

namespace oz3 {

namespace {

//==============================================================================
// Characters
//==============================================================================

// The code points of 00h to 1Fh, the block and line graphics: blocks and
// quadrants, triangles, heavy lines, a dot, and 13h and 1Dh a quarter and a
// half.
constexpr char32_t kGraphicsCodePoints[32] = {
    0x25AF, 0x258C, 0x259E, 0x259F, 0x2590, 0x259C, 0x2596, 0x2597,
    0x25E3, 0x250F, 0x25E2, 0x2503, 0x254B, 0x25CF, 0x259A, 0x2533,
    0x2513, 0x259B, 0x2598, 0x00BC, 0x259D, 0x25E5, 0x2588, 0x2580,
    0x2584, 0x25E4, 0x2599, 0x2501, 0x2517, 0x00BD, 0x253B, 0x251B,
};

// The code point of 7Fh, the hatch: a medium shade.
constexpr char32_t kHatchCodePoint = 0x2592;

// The code points of C0h to FFh, the lines, shapes, and symbols, in the order
// GetDefaultScreenCharacters lists them.
constexpr char32_t kSymbolCodePoints[64] = {
    0x2500, 0x2502, 0x250C, 0x2510, 0x2514, 0x2518, 0x251C, 0x2524,
    0x252C, 0x2534, 0x253C, 0x2550, 0x2551, 0x2554, 0x2557, 0x255A,
    0x255D, 0x2560, 0x2563, 0x2566, 0x2569, 0x256C, 0x2574, 0x2575,
    0x2576, 0x2577, 0x256D, 0x256E, 0x256F, 0x2570, 0x2591, 0x2592,
    0x2593, 0x2580, 0x2584, 0x2571, 0x2572, 0x2573, 0x25E2, 0x25E3,
    0x25E4, 0x25E5, 0x2190, 0x2191, 0x2192, 0x2193, 0x2660, 0x2665,
    0x2666, 0x2663, 0x2022, 0x25CF, 0x25CB, 0x25A0, 0x25A1, 0x2713,
    0x00D7, 0x00F7, 0x00B1, 0x00B0, 0x00A3, 0x266A, 0x263A, 0x2195,
};

// Returns the code point of the sextant with `sixths` lit, 80h to BFh being
// 80h plus bit 0 for the top left sixth, bit 1 the top right, and so on.
// Unicode's sextants are in the order of their sixths, but leave out the four
// that other characters already are: blank, the left and right halves, and
// full.
char32_t GetSextantCodePoint(int sixths) {
  constexpr int kLeftHalf = 0b010101;
  constexpr int kRightHalf = 0b101010;
  constexpr int kFull = 0b111111;
  switch (sixths) {
    case 0:
      return U' ';
    case kLeftHalf:
      return 0x258C;
    case kRightHalf:
      return 0x2590;
    case kFull:
      return 0x2588;
  }
  int index = sixths - 1;
  if (sixths > kLeftHalf) {
    --index;
  }
  if (sixths > kRightHalf) {
    --index;
  }
  return 0x1FB00 + index;
}

// Returns the code point closest to the built-in glyph for `character`.
char32_t GetCodePoint(int character) {
  if (character < 0x20) {
    return kGraphicsCodePoints[character];
  }
  if (character < 0x7F) {
    return static_cast<char32_t>(character);
  }
  if (character == 0x7F) {
    return kHatchCodePoint;
  }
  if (character < 0xC0) {
    return GetSextantCodePoint(character - 0x80);
  }
  return kSymbolCodePoints[character - 0xC0];
}

// The text of every character.
using CharacterTexts = std::array<std::string, 256>;

// Returns the text of every character.
CharacterTexts MakeCharacterTexts() {
  CharacterTexts texts;
  for (int character = 0; character < 256; ++character) {
    CHECK(gb::AppendUtf8(&texts[character], GetCodePoint(character)));
  }
  return texts;
}

//==============================================================================
// Virtual terminal sequences
//==============================================================================

// The start of a control sequence.
constexpr std::string_view kCsi = "\x1B[";

// The attributes text can show, each with its style's parameter.
struct ShownAttribute {
  uint16_t attribute;
  std::string_view parameter;
};
constexpr ShownAttribute kShownAttributes[] = {
    {kScreenInverse, "7"},    {kScreenHalfIntensity, "2"}, {kScreenBlink, "5"},
    {kScreenLineBottom, "4"}, {kScreenLineTop, "53"},
};

// Returns the attributes of `cell` that text can show.
uint16_t GetShownAttributes(uint16_t cell) {
  uint16_t attributes = 0;
  for (const ShownAttribute& shown : kShownAttributes) {
    attributes |= cell & shown.attribute;
  }
  return attributes;
}

// Appends the sequence that sets the console's style to `attributes`, from
// the default.
void AppendStyle(uint16_t attributes, std::string& text) {
  absl::StrAppend(&text, kCsi, "0");
  for (const ShownAttribute& shown : kShownAttributes) {
    if ((attributes & shown.attribute) != 0) {
      absl::StrAppend(&text, ";", shown.parameter);
    }
  }
  absl::StrAppend(&text, "m");
}

// Appends the sequence that moves the console's cursor to `column` and `row`,
// from 0.
void AppendMove(int column, int row, std::string& text) {
  absl::StrAppend(&text, kCsi, row + 1, ";", column + 1, "H");
}

// Returns whether `a` and `b` have the same cursor.
bool IsSameCursor(const ScreenPicture& a, const ScreenPicture& b) {
  return a.cursor_column == b.cursor_column && a.cursor_row == b.cursor_row &&
         a.cursor_mode == b.cursor_mode && a.cursor_style == b.cursor_style;
}

// Appends the sequences that put the console's cursor where `picture`'s is,
// with its shape and blinking, or hide it.
void AppendCursor(const ScreenPicture& picture, std::string& text) {
  if (picture.cursor_mode == ScreenCursorMode::kOff) {
    absl::StrAppend(&text, kCsi, "?25l");
    return;
  }
  AppendMove(picture.cursor_column, picture.cursor_row, text);

  // The cursor shapes, each blinking and then steady. The console has no
  // inverse cursor, so it is a block.
  int shape = 1;
  if (picture.cursor_style == ScreenCursorStyle::kUnderline) {
    shape = 3;
  } else if (picture.cursor_style == ScreenCursorStyle::kLeftEdge) {
    shape = 5;
  }
  if (picture.cursor_mode == ScreenCursorMode::kOn) {
    ++shape;
  }
  absl::StrAppend(&text, kCsi, shape, " q", kCsi, "?25h");
}

}  // namespace

//==============================================================================
// Console text
//==============================================================================

std::string_view GetCharacterText(uint8_t character) {
  static const absl::NoDestructor<CharacterTexts> s_texts(MakeCharacterTexts());
  return (*s_texts)[character];
}

std::string DrawConsoleText(const ScreenPicture& picture,
                            const ScreenPicture* last) {
  const bool draw_all = last == nullptr || last->columns != picture.columns ||
                        last->rows != picture.rows;

  // The cursor is hidden while the rows are drawn, so it doesn't flicker
  // across them, and then put where it belongs.
  std::string text;
  if (draw_all) {
    absl::StrAppend(&text, kCsi, "?25l", kCsi, "2J");
  }
  for (int row = 0; row < picture.rows; ++row) {
    const auto first = picture.cells.begin() + row * picture.columns;
    const auto end = first + picture.columns;
    if (!draw_all &&
        std::equal(first, end, last->cells.begin() + row * picture.columns)) {
      continue;
    }
    if (text.empty()) {
      absl::StrAppend(&text, kCsi, "?25l");
    }

    // The console's style is the default between rows, and after the text.
    AppendMove(0, row, text);
    uint16_t style = 0;
    for (auto cell = first; cell != end; ++cell) {
      const uint16_t attributes = GetShownAttributes(*cell);
      if (attributes != style) {
        AppendStyle(attributes, text);
        style = attributes;
      }
      absl::StrAppend(&text, GetCharacterText(*cell & 0xFF));
    }
    if (style != 0) {
      AppendStyle(0, text);
    }
  }
  if (text.empty() && IsSameCursor(picture, *last)) {
    return text;
  }
  AppendCursor(picture, text);
  return text;
}

}  // namespace oz3
