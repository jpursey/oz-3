// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_set_parser.h"

#include <memory>

#include "absl/base/no_destructor.h"
#include "absl/log/log.h"
#include "absl/memory/memory.h"
#include "absl/strings/str_cat.h"
#include "gb/file/path.h"
#include "gb/parse/parser.h"

namespace oz3 {

namespace {

constexpr gb::TokenType kTokenMacroName = gb::kTokenUser + 0;
constexpr gb::TokenType kTokenLabel = gb::kTokenUser + 1;

constexpr gb::Symbol kSymbols[] = {'(', ')', '{', '}', ':', ';', ',', '.'};
constexpr std::string_view kLineComments[] = {"#"};
constexpr gb::LexerConfig::UserToken kUserTokens[] = {
    {.name = "macro name",
     .type = kTokenMacroName,
     .regex = R"-(\$([a-zA-Z]\w*))-"},
    {.name = "label", .type = kTokenLabel, .regex = R"-(\@([a-zA-Z]\w*))-"},
};
constexpr gb::LexerConfig kLexerConfig = {
    .flags = {gb::kLexerFlags_AllPositiveIntegers,
              gb::LexerFlag::kBinaryIntegers, gb::LexerFlag::kHexUpperIntegers,
              gb::LexerFlag::kHexLowerIntegers,
              gb::LexerFlag::kDoubleQuoteString, gb::LexerFlag::kIdentLower,
              gb::LexerFlag::kIdentUpper,
              gb::LexerFlag::kIdentNonLeadUnderscore,
              gb::LexerFlag::kIdentNonLeadDigit},
    .binary_prefix = "b",
    .hex_prefix = "x",
    .line_comments = kLineComments,
    .symbols = kSymbols,
    .user_tokens = kUserTokens,
};

constexpr std::string_view kParserProgram = R"---(
  %macro_name = 0;
  %label = 1;

  InstructionSet {
    (Macro | Instruction)*
    %end;
  }

  Macro {
    "MACRO" 
    ["(" $config=MacroConfig,+ ")"] 
    $name=%ident 
    ["(" $arg=("p" | "P") ")"] 
    [":" $ret=("r" | "R")] 
    "{" $code=MacroCode* "}";
  }

  MacroConfig {
    "bits" ":" $bits=%int;
  }

  MacroCode {
    "CODE" [":" $ret=%ident] $source=%string
    "{" $code=MicroCode* "}";
  }

  Instruction {
    "INSTRUCTION"
    ["(" $config=InstructionConfig,+ ")"] 
    $name=%ident ["." $ext=%ident] 
    [$params=%string]
    "{" $code=MicroCode* "}";
  }

  InstructionConfig {
    "opcode" ":" $opcode=%int;
  }

  MicroCode {
    [$label=%label ":"] (
      $op=%ident ["(" $arg1=MicroArg ["," $arg2=MicroArg] ")"] |
      $macro=%macro_name ["(" $arg1=%ident ")"]
    ) ";";
  }

  MicroArg {
    $reg=%ident;
    $imm=%int;
    $label=%label;
  }
)---";

gb::ParseResult Parse(std::string_view filename, std::string text) {
  static const absl::NoDestructor<std::shared_ptr<const gb::ParserProgram>>
      parser_program(gb::ParserProgram::Create(kLexerConfig, kParserProgram));
  auto parser = gb::Parser::Create(*parser_program);
  if (parser == nullptr) {
    return gb::ParseError("Internal error: Invalid parser or lexer program");
  }
  gb::LexerContentId content =
      parser->GetLexer().AddFileContent(filename, std::move(text));
  if (content == gb::kNoLexerContent) {
    return gb::ParseError("Instruction set program is too big");
  }
  return parser->Parse(content, "InstructionSet");
}

}  // namespace

gb::ParseResult ParseInstructionSet(std::string text) {
  return Parse("", std::move(text));
}

gb::ParseResult ParseInstructionSetFile(gb::FileSystem& file_system,
                                        std::string_view path) {
  std::string normalized_path = gb::NormalizePath(path);
  std::string text;
  if (!file_system.ReadFile(normalized_path, &text)) {
    return gb::ParseError(
        absl::StrCat("Could not read file: ", normalized_path));
  }
  return Parse(gb::RemoveProtocol(normalized_path), std::move(text));
}

}  // namespace oz3
