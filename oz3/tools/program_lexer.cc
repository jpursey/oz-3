// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_lexer.h"

#include <cstdint>
#include <iterator>
#include <string>
#include <string_view>
#include <vector>

#include "absl/base/no_destructor.h"
#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_format.h"
#include "absl/types/span.h"
#include "gb/base/unicode.h"
#include "gb/parse/lexer.h"
#include "gb/parse/lexer_config.h"
#include "gb/parse/symbol.h"
#include "gb/parse/token.h"
#include "oz3/core/cpu_core.h"
#include "oz3/tools/source_pattern.h"

namespace oz3 {

namespace {

// The symbols only expressions use, beyond those patterns can write: '$' for
// the current offset, and the shifts.
constexpr gb::Symbol kExpressionSymbols[] = {'$', "<<", ">>"};

constexpr std::string_view kLineComments[] = {";"};

// The capture is the whole token, so the value keeps its '@'.
constexpr gb::LexerConfig::UserToken kUserTokens[] = {
    {.name = "local label",
     .type = kTokenLocalLabel,
     .regex = R"-((@[a-zA-Z_]\w*))-",
     .value_case = gb::LexerCase::kUpper},
};

// The registers a program can name, which are the word registers R0 to SP,
// and the dword registers D0 to D3.
std::vector<std::string_view> GetRegisterNames() {
  std::vector<std::string_view> names;
  for (int reg = CpuCore::R0; reg <= CpuCore::SP; ++reg) {
    names.push_back(CpuCore::GetWordRegName(reg));
  }
  for (int reg : {CpuCore::D0, CpuCore::D1, CpuCore::D2, CpuCore::D3}) {
    names.push_back(CpuCore::GetDwordRegName(reg));
  }
  return names;
}

std::vector<gb::Symbol> GetSymbols() {
  absl::Span<const gb::Symbol> pattern_symbols = SourcePattern::GetSymbols();
  std::vector<gb::Symbol> symbols(pattern_symbols.begin(),
                                  pattern_symbols.end());
  symbols.insert(symbols.end(), std::begin(kExpressionSymbols),
                 std::end(kExpressionSymbols));
  return symbols;
}

struct ProgramLexerConfig {
  ProgramLexerConfig()
      : register_names(GetRegisterNames()),
        symbols(GetSymbols()),
        config({
            .flags = {gb::LexerFlag::kInt64, gb::LexerFlag::kDecimalIntegers,
                      gb::LexerFlag::kBinaryIntegers,
                      gb::LexerFlag::kHexUpperIntegers,
                      gb::LexerFlag::kHexLowerIntegers,
                      gb::LexerFlag::kHexLeadDigit,
                      gb::LexerFlag::kNumberAffixCaseInsensitive,
                      gb::LexerFlag::kSingleQuoteString,
                      gb::LexerFlag::kDoubleQuoteString,
                      gb::LexerFlag::kQuoteQuoteEscape,
                      gb::LexerFlag::kDecodeEscape,
                      gb::LexerFlag::kIdentForceUpper,
                      gb::LexerFlag::kIdentUnderscore,
                      gb::LexerFlag::kIdentNonLeadDigit,
                      gb::LexerFlag::kKeywordCaseInsensitive,
                      gb::LexerFlag::kLineBreak},
            .binary_suffix = "b",
            .hex_suffix = "h",
            .line_comments = kLineComments,
            .symbols = symbols,
            .keywords = register_names,
            .user_tokens = kUserTokens,
        }) {}

  std::vector<std::string_view> register_names;
  std::vector<gb::Symbol> symbols;
  gb::LexerConfig config;
};

// Returns the code point for a UTF-16 string at `index`, and advances `index`
// past it.
uint32_t NextCodePoint(std::u16string_view text, int& index) {
  const uint32_t unit = text[index++];
  if (unit < 0xD800 || unit > 0xDBFF ||
      index == static_cast<int>(text.size())) {
    return unit;
  }
  const uint32_t low = text[index++];
  return 0x10000 + ((unit - 0xD800) << 10) + (low - 0xDC00);
}

}  // namespace

const gb::LexerConfig& GetProgramLexerConfig() {
  static const absl::NoDestructor<ProgramLexerConfig> config;
  return config->config;
}

absl::StatusOr<std::vector<uint16_t>> GetStringWords(const gb::Lexer& lexer,
                                                     const gb::Token& token) {
  const std::string_view token_text = lexer.GetTokenText(token);
  DCHECK(token.IsString() && !token_text.empty());
  const std::string_view text = token.GetString();

  // ToUtf16 drops a leading byte order mark, which in a string is a character
  // (U+FEFF), so it is converted separately.
  constexpr std::string_view kByteOrderMark = "\xEF\xBB\xBF";
  const bool has_byte_order_mark = text.starts_with(kByteOrderMark);
  const std::string_view rest =
      (has_byte_order_mark ? text.substr(kByteOrderMark.size()) : text);
  std::u16string utf16_text = gb::ToUtf16(rest);
  if (utf16_text.empty() && !rest.empty()) {
    return absl::InvalidArgumentError("String is not valid UTF-8");
  }
  if (has_byte_order_mark) {
    utf16_text.insert(utf16_text.begin(), u'\xFEFF');
  }

  // The lexer has already decoded the string, so only its text says which
  // quotes it was in.
  const bool packed = (token_text.front() == '"');
  const uint32_t max_code_point = (packed ? 0xFF : 0xFFFF);
  std::vector<uint16_t> words;
  bool high = true;
  for (int i = 0; i < static_cast<int>(utf16_text.size());) {
    const uint32_t code_point = NextCodePoint(utf16_text, i);
    if (code_point > max_code_point) {
      return absl::InvalidArgumentError(
          absl::StrFormat("Character U+%04X does not fit in a %s", code_point,
                          packed ? "byte" : "word"));
    }
    if (!packed) {
      words.push_back(static_cast<uint16_t>(code_point));
    } else if (high) {
      words.push_back(static_cast<uint16_t>(code_point << 8));
    } else {
      words.back() |= static_cast<uint16_t>(code_point);
    }
    high = !high;
  }
  return words;
}

std::string FormatHexNumber(uint32_t value, int digits) {
  std::string text = absl::StrFormat("%0*Xh", digits, value);
  if (text[0] >= 'A') {
    text.insert(0, 1, '0');
  }
  return text;
}

}  // namespace oz3
