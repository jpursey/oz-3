// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/source_pattern.h"

#include <algorithm>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

#include "absl/base/no_destructor.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/parse/lexer.h"
#include "gb/parse/parser.h"
#include "gb/parse/parser_program.h"
#include "gb/parse/symbol.h"
#include "gb/parse/token.h"
#include "oz3/core/instruction_def.h"

namespace oz3 {

namespace {

constexpr gb::TokenType kTokenPlaceholder = gb::kTokenUser + 0;

// Patterns can't use ';' or '@', which programs use for comments and local
// labels, or a '#' outside of "$#".
constexpr gb::Symbol kSymbols[] = {'(', ')', '[', ']', '{', '}', ',', '+',
                                   '-', '*', '/', '%', '&', '|', '^', '~',
                                   '!', '<', '>', '=', ':', '.', '?'};

// A placeholder: "$#" with its size, or "$" and a name, which is either one of
// the built-in placeholders (IsPlaceholderName) or a value set. The capture is
// the whole token, as a rewound token's value is its whole text.
constexpr gb::LexerConfig::UserToken kUserTokens[] = {
    {.name = "placeholder",
     .type = kTokenPlaceholder,
     .regex = R"-((\$(?:#[1-8]|[a-zA-Z_]\w*)))-"},
};

constexpr gb::LexerConfig kLexerConfig = {
    .flags = {gb::kLexerFlags_AllIntegers, gb::kLexerFlags_AllFloats,
              gb::LexerFlag::kDoubleQuoteString,
              gb::LexerFlag::kSingleQuoteString, gb::LexerFlag::kIdentLower,
              gb::LexerFlag::kIdentUpper, gb::LexerFlag::kIdentUnderscore,
              gb::LexerFlag::kIdentNonLeadDigit},
    .symbols = kSymbols,
    .user_tokens = kUserTokens,
};

// A Term can't contain ',', so the commas between arguments are the only
// commas in a pattern, which SourcePattern::Parse relies on.
constexpr std::string_view kParserProgram = R"---(
  %placeholder = 0;
  Source {
    $args=Arg,* %end;
  }
  Arg {
    $terms=Term+;
  }
  Term {
    %placeholder;
    '(' <Term>* ')';
    '[' <Term>* ']';
    '{' <Term>* '}';
    %ident;
    %int; %float; %string;
    '+'; '-'; '*'; '/'; '%';
    '~'; '&'; '|'; '^';
    '!'; '<'; '>'; '=';
    ':'; '.'; '?';
  }
)---";

std::shared_ptr<const gb::ParserProgram> GetParserProgram() {
  static const absl::NoDestructor<std::shared_ptr<const gb::ParserProgram>>
      parser_program(gb::ParserProgram::Create(kLexerConfig, kParserProgram));
  return *parser_program;
}

// Returns the part for a placeholder, from its name after the "$", or nullopt
// if it is neither a placeholder nor one of `value_sets`.
std::optional<SourcePattern::Part> ParsePlaceholder(
    std::string_view name, absl::Span<const ValueSetDef> value_sets) {
  using Kind = SourcePattern::Part::Kind;
  SourcePattern::Part part;
  if (name == "v") {
    part.kind = Kind::kWord;
    return part;
  }
  if (name == "V") {
    part.kind = Kind::kDword;
    return part;
  }
  part.kind = Kind::kArgument;

  // The lexer only reads "$#" with a size from 1 to 8.
  if (name[0] == '#') {
    part.arg = Argument(ArgType::kImmediate, name[1] - '0');
    return part;
  }
  if (!IsPlaceholderName(name)) {
    auto value_set = std::ranges::find(value_sets, name, &ValueSetDef::name);
    if (value_set == value_sets.end()) {
      return std::nullopt;
    }
    part.arg = Argument(ArgType::kImmediate, value_set->size);
    part.value_set = &*value_set;
    return part;
  }

  // A $m without a size takes its macro's size, which isn't known here.
  if (name[0] == 'm') {
    part.arg = Argument(ArgType::kMacro, name.size() == 2 ? name[1] - '0' : 0);
    return part;
  }
  const ArgType type =
      (name[0] == 'r' ? ArgType::kWordReg : ArgType::kDwordReg);
  part.arg =
      (name.size() == 2 ? Argument(type, name[1] - '0') : Argument(type));
  return part;
}

}  // namespace

absl::StatusOr<SourcePattern> SourcePattern::ParseInstruction(
    std::string_view source, absl::Span<const ValueSetDef> value_sets) {
  absl::StatusOr<SourcePattern> pattern = Parse(source, value_sets);
  if (!pattern.ok()) {
    return pattern;
  }
  if (pattern->args_.size() > 2) {
    return absl::InvalidArgumentError(
        "At most two non-value argument types ($m, $r, $R, $#, or a value set) "
        "are allowed for an instruction.");
  }
  if (pattern->args_.size() == 2 && pattern->args_[0].type == ArgType::kMacro &&
      pattern->args_[1].type == ArgType::kMacro) {
    return absl::InvalidArgumentError(
        "Only one macro argument type ($m) is allowed for an instruction.");
  }
  return pattern;
}

absl::StatusOr<SourcePattern> SourcePattern::ParseMacroCode(
    std::string_view source, absl::Span<const ValueSetDef> value_sets) {
  absl::StatusOr<SourcePattern> pattern = Parse(source, value_sets);
  if (!pattern.ok()) {
    return pattern;
  }
  if (pattern->args_.size() > 1) {
    return absl::InvalidArgumentError(
        "Only one non-value macro argument type ($r, $R, $#, or a value set) "
        "is allowed for a macro.");
  }
  if (!pattern->args_.empty() && pattern->args_[0].type == ArgType::kMacro) {
    return absl::InvalidArgumentError("$m is not allowed for a macro arg.");
  }
  return pattern;
}

absl::Span<const gb::Symbol> SourcePattern::GetSymbols() { return kSymbols; }

absl::StatusOr<SourcePattern> SourcePattern::Parse(
    std::string_view source, absl::Span<const ValueSetDef> value_sets) {
  auto parser = gb::Parser::Create(GetParserProgram());
  if (parser == nullptr) {
    return absl::InternalError("Invalid parser or lexer program");
  }
  gb::Lexer& lexer = parser->GetLexer();
  const gb::LexerContentId content = lexer.AddContent(std::string(source));
  if (content == gb::kNoLexerContent) {
    return absl::InvalidArgumentError("Pattern is too big");
  }
  auto result = parser->Parse(content, "Source");
  if (!result.IsOk()) {
    return absl::InvalidArgumentError(result.GetError().GetMessage());
  }

  // The parse checked the syntax, so every token is a part, in order. Token
  // text is a view of the content's text, which gives each part's offset.
  const std::string_view text = lexer.GetContentText(content);
  SourcePattern pattern;
  int arg_start = 0;
  lexer.RewindContent(content);
  for (gb::Token token = lexer.NextToken(content); !token.IsEnd();
       token = lexer.NextToken(content)) {
    if (token.IsSymbol(',')) {
      pattern.EndArg(arg_start);
      arg_start = static_cast<int>(pattern.parts_.size()) + 1;
    }
    std::optional<Part> part;
    if (token.IsUser(kTokenPlaceholder)) {
      part = ParsePlaceholder(token.GetString().substr(1), value_sets);
      if (!part.has_value()) {
        return absl::InvalidArgumentError(absl::StrCat(
            token.GetString(), " is not a placeholder or value set"));
      }
    } else {
      part.emplace();
      part->literal_type = token.GetType();
      part->literal = token.GetValue();
    }
    const std::string_view token_text = lexer.GetTokenText(token);
    part->offset = static_cast<int>(token_text.data() - text.data());
    part->size = static_cast<int>(token_text.size());
    if (part->kind == Part::Kind::kArgument) {
      pattern.args_.push_back(part->arg);
    } else if (part->kind == Part::Kind::kWord) {
      pattern.value_words_ += 1;
    } else if (part->kind == Part::Kind::kDword) {
      pattern.value_words_ += 2;
    }
    pattern.parts_.push_back(*std::move(part));
  }
  pattern.EndArg(arg_start);
  return pattern;
}

void SourcePattern::EndArg(int arg_start) {
  if (static_cast<int>(parts_.size()) - arg_start != 1) {
    return;
  }
  Part& part = parts_.back();
  if (part.kind != Part::Kind::kLiteral) {
    part.whole_arg = true;
  }
}

}  // namespace oz3
