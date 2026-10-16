// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_LEXER_H_
#define OZ3_TOOLS_PROGRAM_LEXER_H_

#include <cstdint>
#include <string>
#include <vector>

#include "absl/status/statusor.h"
#include "gb/parse/lexer.h"
#include "gb/parse/lexer_config.h"
#include "gb/parse/token.h"

namespace oz3 {

// The token type of a local label, such as "@loop". Its value is the whole
// token as written, with the "@". Unlike an identifier, it isn't turned to
// upper case, so code that names local labels by their value does that itself.
inline constexpr gb::TokenType kTokenLocalLabel = gb::kTokenUser + 0;

// Returns the lexer configuration for program source:
// - Decimal numbers, hex numbers with an "h" suffix that start with a digit
//   ("0FFh"), and binary numbers with a "b" suffix ("1011b"), with suffixes
//   in either case. Numbers have no sign: "-1" is the symbol '-' and 1. Hex
//   and binary numbers are unsigned 64-bit values, so one with its top bit
//   set has a negative value, which means it is too big.
// - Identifiers, turned to upper case. The registers a program names (R0 to
//   R7, BC, BS, BD, BE, MB, IP, FP, SP, and D0 to D3) are keywords in any
//   case, so they are never identifiers.
// - Strings in single or double quotes, with a quote inside doubled.
// - Local labels (kTokenLocalLabel).
// - Comments from ';' to the end of the line, and line breaks as tokens.
const gb::LexerConfig& GetProgramLexerConfig();

// Returns the words of a string token that `lexer` read:
// - 'Hello' is one character per word.
// - "Hello" is two characters per word, the first in the high byte, and the
//   last word padded with a zero byte.
// A character is its Unicode code point, which must fit in a word, or in a
// byte for two per word. Returns an error if one doesn't, or the string isn't
// valid UTF-8.
absl::StatusOr<std::vector<uint16_t>> GetStringWords(const gb::Lexer& lexer,
                                                     const gb::Token& token);

// Returns `value` as a program writes a hex number with `digits` digits, with
// an "h" suffix and a leading 0 if the first digit is a letter, so it isn't a
// name ("0FFFFh").
std::string FormatHexNumber(uint32_t value, int digits);

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_LEXER_H_
