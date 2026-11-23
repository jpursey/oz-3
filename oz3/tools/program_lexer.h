// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_LEXER_H_
#define OZ3_TOOLS_PROGRAM_LEXER_H_

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/status/statusor.h"
#include "gb/parse/lexer.h"
#include "gb/parse/lexer_config.h"
#include "gb/parse/lexer_program.h"
#include "gb/parse/token.h"

namespace oz3 {

// The token type of a local label, such as "@loop". Its value is the whole
// token turned to upper case, with the "@" ("@LOOP"), as an identifier's is.
inline constexpr gb::TokenType kTokenLocalLabel = gb::kTokenUser + 0;

// The token type of a local label that a macro's body writes, renamed for one
// use of the macro, such as "WAITIN#2@WAIT": the macro's name, the use's
// number, and the label. Its value is the whole token turned to upper case.
// Only a macro's expansion writes one (see MacroExpansion).
inline constexpr gb::TokenType kTokenMacroLabel = gb::kTokenUser + 1;

// Declares the program lexer's user tokens for a parser program, as
// %local_label and %macro_label. The number is the token type's offset from
// gb::kTokenUser.
inline constexpr std::string_view kProgramParserTokens =
    "%local_label = 0;\n%macro_label = 1;\n";
static_assert(kTokenLocalLabel == gb::kTokenUser + 0);
static_assert(kTokenMacroLabel == gb::kTokenUser + 1);

// Returns the lexer configuration for program source:
// - Decimal numbers, hex numbers with an "h" suffix that start with a digit
//   ("0FFh"), and binary numbers with a "b" suffix ("1011b"), with suffixes
//   in either case. Numbers have no sign: "-1" is the symbol '-' and 1. Hex
//   and binary numbers are 64 unsigned bits, so one with its top bit set has
//   a negative int64 value, which is its bits as a uint64_t.
// - Identifiers, turned to upper case. The registers a program names (R0 to
//   R7, BC, BS, BD, BE, MB, IP, FP, SP, and D0 to D3) are keywords in any
//   case, so they are never identifiers.
// - Strings in single or double quotes, with a quote inside doubled.
// - Local labels (kTokenLocalLabel), and the local labels of a macro's use
//   (kTokenMacroLabel), turned to upper case.
// - Comments from ';' to the end of the line, and line breaks as tokens.
const gb::LexerConfig& GetProgramLexerConfig();

// Returns the program lexer's configuration compiled once, to create each
// lexer for program source from.
std::shared_ptr<const gb::LexerProgram> GetProgramLexerProgram();

// Returns true if `token` ends a line: a line break, or the end of the content.
bool IsLineEnd(const gb::Token& token);

// Returns the words of a string token that `lexer` read:
// - 'Hello' is one character per word.
// - "Hello" is two characters per word, the first in the low byte, and the
//   last word padded with a zero byte.
// A character is its Unicode code point, which must fit in a word, or in a
// byte for two per word. Returns an error if one doesn't, or the string isn't
// valid UTF-8.
absl::StatusOr<std::vector<uint16_t>> GetStringWords(const gb::Lexer& lexer,
                                                     const gb::Token& token);

// Returns the words of `text`, as a string's are (see GetStringWords): one
// character per word, or two if `packed`.
absl::StatusOr<std::vector<uint16_t>> GetTextWords(std::string_view text,
                                                   bool packed);

// Returns program source `text` as the assembler sees it, for STR: letters
// outside strings in upper case, as names are read, and each run of spaces
// and tabs as one space, with none at either end. A string keeps its text as
// it is written, quotes and doubled quotes included.
std::string GetSourceText(std::string_view text);

// Returns `value` as a program writes a hex number with `digits` digits, with
// an "h" suffix and a leading 0 if the first digit is a letter, so it isn't a
// name ("0FFFFh").
std::string FormatHexNumber(uint32_t value, int digits);

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_LEXER_H_
