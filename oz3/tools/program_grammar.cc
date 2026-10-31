// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_grammar.h"

#include <algorithm>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "absl/base/no_destructor.h"
#include "absl/container/flat_hash_map.h"
#include "absl/log/check.h"
#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/escaping.h"
#include "absl/strings/match.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/parse/lexer.h"
#include "gb/parse/lexer_config.h"
#include "gb/parse/lexer_program.h"
#include "gb/parse/parse_error.h"
#include "gb/parse/parse_result.h"
#include "gb/parse/parse_types.h"
#include "gb/parse/parser.h"
#include "gb/parse/parser_program.h"
#include "gb/parse/symbol.h"
#include "gb/parse/token.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_def.h"
#include "oz3/tools/expression.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program_lexer.h"
#include "oz3/tools/source_pattern.h"

namespace oz3 {

namespace {

using Part = SourcePattern::Part;
using ArgKind = InstructionForm::Arg::Kind;

// The assembler's rules, which every instruction set's grammar has, besides
// ArgToken, which is built from the program lexer's tokens.
//
// Each directive is an alternative in Directive, whose names are promoted to
// it. The parser reports the error that got furthest into the line, so a
// directive with a bad operand gets that operand's error, and an unknown one
// gets Directive's.
constexpr std::string_view kRules = R"---(
  Line {
    [$label=Label] [$directive=Directive | $instruction=Instruction]
        $end=EndOfLine;
  }
  Label {
    $name=%ident ':';
    $local_label=%local_label ':';
  }
  EndOfLine {
    %linebreak;
    %end;
  }
  Directive {
    $dot='.' (<DirectiveInclude> | <DirectiveConst> | <DirectiveDw> |
              <DirectiveDd> | <DirectiveDs> | <DirectiveStack> |
              <DirectiveInstances> | <DirectiveBank> |
              <DirectiveSegment>):"Unknown directive";
  }
  DirectiveInclude {
    $directive="INCLUDE" $path=%string;
  }
  DirectiveConst {
    $directive="CONST" $name=%ident '=' $value=Expression;
  }
  DirectiveDw {
    $directive="DW" $items=Expression,+;
  }
  DirectiveDd {
    $directive="DD" $items=Expression,+;
  }
  DirectiveDs {
    $directive="DS" $count=Expression;
  }
  DirectiveStack {
    $directive="STACK" $words=Expression;
  }
  DirectiveInstances {
    $directive="INSTANCES" $count=Expression;
  }
  DirectiveBank {
    $directive="BANK" $name=%ident [',' $mask=Expression];
  }
  DirectiveSegment {
    $directive="SEGMENT" $name=%ident $attributes=SegmentAttribute*;
  }
  SegmentAttribute {
    $role=("CODE" | "STACK" | "DATA" | "EXTRA");
    $rom="ROM";
    $instance="INSTANCE";
    "PAGE" '(' $page=Expression ')';
    "PAGES" '(' $pages=Expression ')';
    "BANK" '(' $bank=%ident ')';
  }
  Instruction {
    $name=%ident [$dot='.' $extension=%ident] $args=ArgToken*;
  }
)---";

// Brackets, which an instruction's arguments must balance.
constexpr std::string_view kOpenBrackets = "([{";
constexpr std::string_view kCloseBrackets = ")]}";

// Register sizes have a rule each, named by kind and bits ("WordReg3"), of
// the registers that many bits can hold.
constexpr int kMaxWordRegBits = 4;
constexpr int kMaxDwordRegBits = 2;

std::shared_ptr<const gb::LexerProgram> GetLexerProgram() {
  static const absl::NoDestructor<std::shared_ptr<const gb::LexerProgram>>
      program(gb::LexerProgram::Create(GetProgramLexerConfig()));
  return *program;
}

// Returns `text` as a token literal in a parser program, whose strings decode
// backslash escapes, and hex escapes for any other character.
std::string Quote(std::string_view text) {
  return absl::StrCat("\"", absl::CHexEscape(text), "\"");
}

// Returns the ArgToken rule: any token of the program lexer but the end of a
// line, with brackets balanced.
std::string GetArgTokenRule() {
  const gb::LexerConfig& config = GetProgramLexerConfig();
  std::string rule = "ArgToken {\n";
  for (int i = 0; i < static_cast<int>(kOpenBrackets.size()); ++i) {
    absl::StrAppend(&rule, "  ", Quote(kOpenBrackets.substr(i, 1)),
                    " ArgToken* ", Quote(kCloseBrackets.substr(i, 1)), ";\n");
  }
  absl::StrAppend(&rule, "  %int; %string; %ident; %local_label;\n");
  for (std::string_view keyword : config.keywords) {
    absl::StrAppend(&rule, "  ", Quote(keyword), ";\n");
  }
  for (const gb::Symbol& symbol : config.symbols) {
    const std::string_view text = symbol.GetString();
    if (text.size() != 1 || (!absl::StrContains(kOpenBrackets, text) &&
                             !absl::StrContains(kCloseBrackets, text))) {
      absl::StrAppend(&rule, "  ", Quote(text), ";\n");
    }
  }
  absl::StrAppend(&rule, "}\n");
  return rule;
}

std::string GetRegisterRules() {
  std::string rules;
  auto add_rules = [&rules](std::string_view kind, int max_bits,
                            auto get_name) {
    for (int bits = 1; bits <= max_bits; ++bits) {
      absl::StrAppend(&rules, kind, bits, " { ");
      for (int i = 0; i < (1 << bits); ++i) {
        absl::StrAppend(&rules, i == 0 ? "" : " | ", Quote(get_name(i)));
      }
      absl::StrAppend(&rules, "; }\n");
    }
  };
  add_rules("WordReg", kMaxWordRegBits,
            [](int i) { return CpuCore::GetWordRegName(CpuCore::R0 + i); });
  add_rules("DwordReg", kMaxDwordRegBits, [](int i) {
    return CpuCore::GetDwordRegName(CpuCore::D0 + i * 2);
  });
  return rules;
}

// Returns true if `after` starts where `before` ends, on the same line.
bool IsAdjacent(const gb::Lexer& lexer, const gb::Token& before,
                const gb::Token& after) {
  const gb::LexerLocation before_location = lexer.GetTokenLocation(before);
  const gb::LexerLocation after_location = lexer.GetTokenLocation(after);
  return after_location.line == before_location.line &&
         after_location.column ==
             before_location.column +
                 static_cast<int>(lexer.GetTokenText(before).size());
}

// Returns the name a form's rule gives the expression of its literal number
// `index` ("n0"), or of its argument `index` ("a0").
std::string GetNumberName(int index) { return absl::StrCat("n", index); }
std::string GetArgName(int index) { return absl::StrCat("a", index); }

// Returns the body of a form's rule: its parts, then the end of the line.
// Forms written the same way have the same body.
std::string GetFormRuleBody(const InstructionForm& form) {
  std::string body;
  int number_index = 0;
  int arg_index = 0;
  for (const Part& part : form.GetParts()) {
    if (part.kind == Part::Kind::kLiteral) {
      if (part.literal_type == gb::kTokenInt) {
        absl::StrAppend(&body, "$", GetNumberName(number_index++),
                        "=Expression ");
      } else {
        absl::StrAppend(&body,
                        Quote(form.GetPattern().substr(part.offset, part.size)),
                        " ");
      }
      continue;
    }
    const InstructionForm::Arg& arg = form.GetArgs()[arg_index];
    absl::StrAppend(&body, "$", GetArgName(arg_index++), "=");
    if (arg.kind == ArgKind::kWordReg) {
      absl::StrAppend(&body, "WordReg", arg.bits, " ");
    } else if (arg.kind == ArgKind::kDwordReg) {
      absl::StrAppend(&body, "DwordReg", arg.bits, " ");
    } else {
      absl::StrAppend(&body, "Expression ");
    }
  }
  absl::StrAppend(&body, "EndOfLine;");
  return body;
}

// Returns the arguments of a form, from its rule's match.
std::vector<InstructionArg> GetArgs(const gb::Lexer& lexer,
                                    const InstructionForm& form,
                                    const gb::ParsedItem& item) {
  std::vector<InstructionArg> args;
  args.reserve(form.GetArgs().size());
  for (int i = 0; i < static_cast<int>(form.GetArgs().size()); ++i) {
    const gb::ParsedItem& arg_item = *item.GetItem(GetArgName(i));
    const std::string_view text = arg_item.GetToken().GetString();
    InstructionArg& arg = args.emplace_back();
    arg.token = arg_item.GetToken();
    switch (form.GetArgs()[i].kind) {
      case ArgKind::kWordReg:
        arg.reg = GetRegisterNumber(ArgType::kWordReg, text);
        break;
      case ArgKind::kDwordReg:
        arg.reg = GetRegisterNumber(ArgType::kDwordReg, text);
        break;
      default:
        arg.value = Expression::Create(lexer, arg_item);
        break;
    }
  }
  return args;
}

// Returns the match of a form whose rule matched `item`, or nullopt if the
// form doesn't match: a value is in brackets as a whole, a literal number's
// expression has a different value, or a first pass value (a code word value,
// or a literal number) can't be evaluated in `context`. In the last case, sets
// `error` to the value's error, if it has none.
std::optional<InstructionMatch> MatchForm(
    const gb::Lexer& lexer, const InstructionForm& form,
    const gb::ParsedItem& item, const ExpressionContext& context,
    std::optional<gb::ParseError>& error) {
  InstructionMatch match = {.form = &form, .args = GetArgs(lexer, form, item)};
  for (const InstructionArg& arg : match.args) {
    if (arg.value.has_value() && arg.value->IsBracketed()) {
      return std::nullopt;
    }
  }
  std::vector<std::pair<Expression, uint32_t>> numbers;
  for (const Part& part : form.GetParts()) {
    if (part.kind != Part::Kind::kLiteral ||
        part.literal_type != gb::kTokenInt) {
      continue;
    }
    Expression number = Expression::Create(
        lexer, *item.GetItem(GetNumberName(static_cast<int>(numbers.size()))));
    if (number.IsBracketed()) {
      return std::nullopt;
    }
    numbers.emplace_back(
        std::move(number),
        static_cast<uint32_t>(std::get<int64_t>(part.literal)));
  }

  // First pass values are evaluated last, and an error is only kept from a
  // form that would otherwise match: one that no literal number rules out.
  std::optional<gb::ParseError> form_error;
  auto evaluate =
      [&](const Expression& value,
          const ExpressionContext& value_context) -> std::optional<uint32_t> {
    ExpressionResult result =
        value.Evaluate(ExpressionWidth::kWord, value_context);
    if (result.kind == ExpressionResult::Kind::kValue) {
      return result.value;
    }
    if (!form_error.has_value()) {
      form_error = std::move(result.error);
    }
    return std::nullopt;
  };
  for (const auto& [number, literal] : numbers) {
    const std::optional<uint32_t> value = evaluate(number, context);
    if (value.has_value() && *value != literal) {
      return std::nullopt;
    }
  }
  for (int i = 0; i < static_cast<int>(match.args.size()); ++i) {
    const InstructionForm::Arg& arg = form.GetArgs()[i];
    if (arg.kind != ArgKind::kCodeValue) {
      continue;
    }
    InstructionArg& match_arg = match.args[i];
    const std::optional<uint32_t> value =
        arg.value_set != nullptr
            ? evaluate(*match_arg.value,
                       ValueSetContext(arg.value_set, &context))
            : evaluate(*match_arg.value, context);
    match_arg.code_value = value.value_or(0);
  }
  if (form_error.has_value()) {
    if (!error.has_value()) {
      error = *std::move(form_error);
    }
    return std::nullopt;
  }
  return match;
}

}  // namespace

//==============================================================================
// ValueSetContext
//==============================================================================

std::optional<ExpressionResult> ValueSetContext::GetSymbolValue(
    std::string_view name, ExpressionWidth width) const {
  for (const NamedValueDef& value : value_set_->values) {
    if (absl::EqualsIgnoreCase(value.name, name)) {
      return ExpressionResult{.value = value.value};
    }
  }
  return context_->GetSymbolValue(name, width);
}

//==============================================================================
// ProgramGrammar
//==============================================================================

absl::StatusOr<std::unique_ptr<ProgramGrammar>> ProgramGrammar::Create(
    const InstructionSetSyntax& syntax) {
  auto grammar = absl::WrapUnique(new ProgramGrammar(&syntax));
  std::string text =
      absl::StrCat(kProgramParserTokens, Expression::GetParserRules(), kRules,
                   GetArgTokenRule(), GetRegisterRules());

  // Forms written the same way share a rule.
  absl::flat_hash_map<std::string, int> rules_by_body;
  for (const InstructionForm& form : syntax.GetForms()) {
    std::string body = GetFormRuleBody(form);
    auto [it, added] = rules_by_body.try_emplace(
        body, static_cast<int>(grammar->rule_names_.size()));
    if (added) {
      std::string name = absl::StrCat("Form", it->second);
      absl::StrAppend(&text, name, " { ", body, " }\n");
      grammar->rule_names_.push_back(std::move(name));
    }
    grammar->form_rules_[&form] = it->second;
  }

  std::string error;
  grammar->program_ =
      gb::ParserProgram::Create(GetLexerProgram(), text, &error);
  if (grammar->program_ == nullptr) {
    return absl::InvalidArgumentError(
        absl::StrCat("Invalid program grammar: ", error));
  }
  return grammar;
}

std::string_view ProgramGrammar::GetDirectiveName(
    const gb::Lexer& lexer, const gb::ParsedItem& directive,
    gb::ParseError& error) {
  const gb::Token name = directive.GetToken("directive");
  if (!IsAdjacent(lexer, directive.GetToken("dot"), name)) {
    error = gb::ParseError(lexer.GetTokenLocation(name),
                           "A directive's name can't have a space before it");
    return {};
  }
  return name.GetString();
}

std::string_view ProgramGrammar::GetInstructionName(
    const gb::Lexer& lexer, const gb::ParsedItem& instruction,
    gb::ParseError& error) {
  const std::string_view name =
      lexer.GetTokenText(instruction.GetToken("name"));
  const gb::ParsedItem* extension = instruction.GetItem("extension");
  if (extension == nullptr) {
    return name;
  }
  const gb::Token dot = instruction.GetToken("dot");
  for (auto [before, after] : {std::pair{instruction.GetToken("name"), dot},
                               std::pair{dot, extension->GetToken()}}) {
    if (!IsAdjacent(lexer, before, after)) {
      error = gb::ParseError(
          lexer.GetTokenLocation(after),
          "An instruction name can't have a space around its '.'");
      return {};
    }
  }

  // The tokens are adjacent, so the name is the text from the first to the
  // end of the last.
  const std::string_view extension_text =
      lexer.GetTokenText(extension->GetToken());
  return std::string_view(name.data(),
                          extension_text.data() + extension_text.size());
}

bool ProgramGrammar::MatchInstruction(gb::Parser& parser,
                                      gb::LexerContentId content,
                                      const gb::ParsedItem& line,
                                      const ExpressionContext& context,
                                      std::vector<InstructionMatch>& matches,
                                      gb::ParseError& error) const {
  matches.clear();
  gb::Lexer& lexer = parser.GetLexer();
  const gb::ParsedItem& instruction = *line.GetItem("instruction");
  const std::string_view name = GetInstructionName(lexer, instruction, error);
  if (name.empty()) {
    return false;
  }
  absl::Span<const InstructionForm* const> forms = syntax_->GetForms(name);
  if (forms.empty()) {
    error = gb::ParseError(lexer.GetTokenLocation(instruction.GetToken()),
                           absl::StrCat("Unknown instruction ", name));
    return false;
  }

  // The arguments start at the first token after the name, which is the end
  // of the line if there are none.
  absl::Span<const gb::ParsedItem> args = instruction.GetItems("args");
  const gb::Token args_token =
      args.empty() ? line.GetToken("end") : args.front().GetToken();
  const gb::Token next_token = lexer.NextToken(content, false);

  // Each rule is parsed once, for every form that shares it.
  std::vector<std::pair<int, gb::ParseResult>> results;
  std::optional<gb::ParseError> value_error;
  for (const InstructionForm* form : forms) {
    auto rule_it = form_rules_.find(form);
    CHECK(rule_it != form_rules_.end());
    const int rule = rule_it->second;
    auto result =
        std::find_if(results.begin(), results.end(),
                     [rule](const auto& r) { return r.first == rule; });
    if (result == results.end()) {
      lexer.SetNextToken(args_token);
      results.emplace_back(rule, parser.Parse(content, rule_names_[rule]));
      result = results.end() - 1;
    }
    if (!result->second.IsOk()) {
      continue;
    }
    std::optional<InstructionMatch> match =
        MatchForm(lexer, *form, *result->second, context, value_error);
    if (match.has_value()) {
      matches.push_back(*std::move(match));
    }
  }
  lexer.SetNextToken(next_token);
  if (matches.empty() && value_error.has_value()) {
    error = *std::move(value_error);
    return false;
  }
  return true;
}

}  // namespace oz3
