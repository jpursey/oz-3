// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_assembler.h"

#include <cstdint>
#include <string_view>
#include <vector>

#include "absl/base/no_destructor.h"
#include "absl/container/flat_hash_set.h"
#include "absl/memory/memory.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "absl/strings/str_split.h"
#include "gb/file/path.h"
#include "gb/parse/parser.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/instruction_def.h"

namespace oz3 {

namespace {

constexpr gb::TokenType kTokenMacroName = gb::kTokenUser + 0;
constexpr gb::TokenType kTokenLabel = gb::kTokenUser + 1;

constexpr gb::Symbol kSymbols[] = {'(', ')', '{', '}', ':', ';', ',', '.', '='};
constexpr std::string_view kLineComments[] = {"#"};
constexpr gb::LexerConfig::UserToken kUserTokens[] = {
    {.name = "macro name",
     .type = kTokenMacroName,
     .regex = R"-(\$([a-zA-Z]\w*))-"},
    {.name = "label", .type = kTokenLabel, .regex = R"-((\@[a-zA-Z]\w*))-"},
};
constexpr gb::LexerConfig kLexerConfig = {
    .flags = {gb::kLexerFlags_AllIntegers,
              gb::kLexerFlags_AllIntegerFormats - gb::LexerFlag::kOctalIntegers,
              gb::kLexerFlags_AllFloats, gb::kLexerFlags_CIdentifiers,
              gb::LexerFlag::kDoubleQuoteString,
              gb::LexerFlag::kSingleQuoteString},
    .binary_prefix = "0b",
    .hex_prefix = "0x",
    .line_comments = kLineComments,
    .symbols = kSymbols,
    .user_tokens = kUserTokens,
};

constexpr std::string_view kParserProgram = R"---(
  %macro_name = 0;
  %label = 1;

  InstructionSet {
    ($value_sets=ValueSet | $macros=Macro | $instructions=Instruction)*
    %end:'Expected "values", "macro", or "instruction"';
  }

  ValueSet {
    "values" "(" "bits" ":" $bits=%int ")" $name=%ident
    "{" $values=NamedValue,* "}";
  }

  NamedValue {
    $name=%ident "=" $value=%int;
  }

  Macro {
    "macro" 
    ["(" <MacroConfig>,+ ")"] 
    $name=%ident 
    ["(" $param=("p" | "P"):"Expected 'p' or 'P' for parameter" ")"] 
    [":" $ret=("r" | "R"):"Expected 'r' or 'R' for return"] 
    "{" $code=MacroCode* "}";
  }

  MacroConfig {
    "bits" ":" $bits=%int;
  }

  MacroCode {
    "code" [":" $ret=%ident] $source=%string
    "{" $code=MicroCode* "}";
  }

  Instruction {
    "instruction"
    ["(" <InstructionConfig>,+ ")"] 
    $name=%ident ["." $ext=%ident] 
    [$source=%string]
    "{" $code=MicroCode* "}";
  }

  InstructionConfig {
    "opcode" ":" $opcode=%int;
    $relative="relative";
  }

  MicroCode {
    [$label=%label ":"] (
      $op=%ident ["(" $arg1=MicroArg ["," $arg2=MicroArg] ")"] |
      $macro=%macro_name ["(" $arg1=%ident ")"]
    ) ";";
  }

  MicroArg {
    $reg=%ident [':' $mask=('0' | '1' | '2' | '3' | 'h' | 'H' | 'l' | 'L')];
    $imm=%int;
    $label=%label;
  }
)---";

std::shared_ptr<const gb::ParserProgram> GetInstructionParserProgram() {
  static const absl::NoDestructor<std::shared_ptr<const gb::ParserProgram>>
      parser_program(gb::ParserProgram::Create(kLexerConfig, kParserProgram));
  return *parser_program;
}

constexpr gb::TokenType kTokenArgType = gb::kTokenUser + 0;

// Patterns can't use ';' or '@', which programs use for comments and local
// labels, or a '#' outside of "$#".
constexpr gb::Symbol kSourceSymbols[] = {'(', ')', '[', ']', '{', '}', ',', '+',
                                         '-', '*', '/', '%', '&', '|', '^', '~',
                                         '!', '<', '>', '=', ':', '.', '?'};

// A placeholder: "$#" with its size, or "$" and a name, which is either one of
// the built-in placeholders (IsPlaceholderName) or a value set.
constexpr gb::LexerConfig::UserToken kSourceUserTokens[] = {
    {.name = "arg type",
     .type = kTokenArgType,
     .regex = R"-(\$(#[1-8]|[a-zA-Z_]\w*))-"},
};
constexpr gb::LexerConfig kSourceLexerConfig = {
    .flags = {gb::kLexerFlags_AllIntegers, gb::kLexerFlags_AllFloats,
              gb::LexerFlag::kDoubleQuoteString,
              gb::LexerFlag::kSingleQuoteString, gb::LexerFlag::kIdentLower,
              gb::LexerFlag::kIdentUpper, gb::LexerFlag::kIdentUnderscore,
              gb::LexerFlag::kIdentNonLeadDigit},
    .symbols = kSourceSymbols,
    .user_tokens = kSourceUserTokens,
};

constexpr std::string_view kSourceProgram = R"---(
  %arg_type = 0;
  Source {
    $args=Arg,* %end;
  }
  Arg {
    $terms=Term+;
  }
  Term {
    $types=%arg_type;
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

std::shared_ptr<const gb::ParserProgram> GetSourceParserProgram() {
  static const absl::NoDestructor<std::shared_ptr<const gb::ParserProgram>>
      parser_program(
          gb::ParserProgram::Create(kSourceLexerConfig, kSourceProgram));
  return *parser_program;
}

// Returns the names of the placeholders in an argument of a parsed source, in
// order, without their "$".
std::vector<std::string_view> GetPlaceholders(
    const gb::ParsedItem& parsed_arg) {
  std::vector<std::string_view> placeholders;
  for (const auto& term : parsed_arg.GetItems("terms")) {
    for (const auto& type : term.GetItems("types")) {
      placeholders.push_back(type.GetToken().GetString());
    }
  }
  return placeholders;
}

// Returns the name of the placeholder that is the whole of an argument of a
// parsed source (such as "v" for the argument "$v"), or an empty string if the
// argument has anything else in it (such as "D($v)" or "$v + 1").
std::string_view GetWholeArgPlaceholder(const gb::ParsedItem& parsed_arg) {
  auto terms = parsed_arg.GetItems("terms");
  if (terms.size() != 1 || !terms[0].IsUser(kTokenArgType)) {
    return {};
  }
  return terms[0].GetToken().GetString();
}

bool IsValuePlaceholder(std::string_view placeholder) {
  return placeholder == "v" || placeholder == "V";
}

std::string GetMacroCodeSourceKey(std::string_view macro_name,
                                  std::string_view source) {
  return absl::StrCat(
      macro_name, ";",
      absl::StrJoin(
          absl::StrSplit(source, absl::ByAsciiWhitespace(), absl::SkipEmpty()),
          ""));
}

}  // namespace

class InstructionSetAssembler {
 public:
  InstructionSetAssembler(gb::ParseError* error) : error_(error) {}

  std::string_view AddString(std::string_view str) {
    result_->strings_.push_back(std::make_unique<std::string>(str));
    return *result_->strings_.back();
  }

  std::unique_ptr<AsmInstructionSet> Assemble(std::string_view filename,
                                              std::string text);

 private:
  using CodeTokens = std::vector<gb::TokenIndex>;
  struct MacroCodeTokens {
    gb::TokenIndex def;
    CodeTokens code;
  };
  struct MacroTokens {
    gb::TokenIndex def;
    absl::flat_hash_map<std::string_view, MacroCodeTokens> code;
  };
  struct InstructionTokens {
    gb::TokenIndex def;
    CodeTokens code;
  };
  struct ValueSetInfo {
    gb::TokenIndex def;
    uint8_t size = 0;
  };

  // Which placeholders are a whole argument of an instruction's source, which
  // decides whether it can be relative.
  struct WholeArgs {
    bool value = false;  // "$v" is a whole argument.
    bool macro = false;  // "$m" is a whole argument.
  };

  template <typename... Args>
  bool ErrorAt(const gb::ParsedItem& item, Args&&... args) {
    if (error_ != nullptr) {
      *error_ =
          gb::ParseError(parser_->GetLexer().GetTokenLocation(item.GetToken()),
                         absl::StrCat(std::forward<Args>(args)...));
    }
    return false;
  }
  template <typename... Args>
  bool ErrorAt(gb::TokenIndex token_index, Args&&... args) {
    if (error_ != nullptr) {
      *error_ =
          gb::ParseError(parser_->GetLexer().GetTokenLocation(token_index),
                         absl::StrCat(std::forward<Args>(args)...));
    }
    return false;
  }
  template <typename... Args>
  bool Error(Args&&... args) {
    if (error_ != nullptr) {
      *error_ = gb::ParseError(absl::StrCat(std::forward<Args>(args)...));
    }
    return false;
  }

  gb::ParseResult Parse(std::string_view filename, std::string text);
  bool AssembleValueSet(const gb::ParsedItem& parsed_value_set);
  bool AssembleMacro(const gb::ParsedItem& parsed_macro);
  bool AssembleMacroCode(const gb::ParsedItem& parsed_macro_code,
                         MacroCodeDef& macro_code_def);
  bool AssembleMacroCodeSource(const gb::ParsedItem& parsed_macro_code,
                               std::string_view source,
                               MacroCodeDef& macro_code_def);
  bool AssembleInstruction(const gb::ParsedItem& parsed_instruction);
  bool AssembleInstructionCodeSource(const gb::ParsedItem& parsed_instruction,
                                     std::string_view source,
                                     InstructionDef& instruction_def,
                                     WholeArgs& whole_args);
  bool AssembleArgument(std::string_view placeholder, Argument& argument);
  bool AssembleMicrocode(absl::Span<const gb::ParsedItem> parsed_micro_codes,
                         CodeTokens& code_tokens, std::string& code_string,
                         Argument* macro_arg = nullptr,
                         std::string* arg_macro_name = nullptr);
  bool Compile();

  gb::ParseError* const error_;
  std::unique_ptr<gb::Parser> parser_;
  std::unique_ptr<AsmInstructionSet> result_;
  absl::flat_hash_map<std::string_view, ValueSetInfo> value_set_map_;
  absl::flat_hash_map<std::string_view, int> macro_map_;
  absl::flat_hash_set<std::string_view> macros_with_whole_arg_value_;
  absl::flat_hash_map<std::string, int> macro_code_map_;
  absl::flat_hash_map<std::string_view, int> instruction_map_;
  std::vector<int> needs_opcode_;
  absl::flat_hash_set<int> opcode_values_;
  absl::flat_hash_map<std::string_view, MacroTokens> macro_token_map_;
  absl::flat_hash_map<std::string_view, InstructionTokens>
      instruction_token_map_;
};

std::unique_ptr<AsmInstructionSet> InstructionSetAssembler::Assemble(
    std::string_view filename, std::string text) {
  auto parsed_set = Parse(filename, std::move(text));
  if (!parsed_set.IsOk()) {
    if (error_ != nullptr) {
      *error_ = parsed_set.GetError();
    }
    return nullptr;
  }
  result_ = absl::WrapUnique(new AsmInstructionSet);

  // Value sets come first, as instructions and macros may use them wherever
  // they are in the source.
  for (const auto& parsed_value_set : parsed_set->GetItems("value_sets")) {
    if (!AssembleValueSet(parsed_value_set)) {
      return nullptr;
    }
  }
  for (const auto& parsed_macro : parsed_set->GetItems("macros")) {
    if (!AssembleMacro(parsed_macro)) {
      return nullptr;
    }
  }
  for (const auto& parsed_instruction : parsed_set->GetItems("instructions")) {
    if (!AssembleInstruction(parsed_instruction)) {
      return nullptr;
    }
  }

  int opcode = 0;
  for (int instruction_index : needs_opcode_) {
    auto& instruction_def = result_->instruction_defs_[instruction_index];
    while (opcode_values_.contains(opcode)) {
      ++opcode;
    }
    if (opcode >= 256) {
      Error("Too many instructions defined");
      return nullptr;
    }
    instruction_def.op = static_cast<uint8_t>(opcode);
    ++opcode;
  }

  if (!Compile()) {
    return nullptr;
  }

  return std::move(result_);
}

gb::ParseResult InstructionSetAssembler::Parse(std::string_view filename,
                                               std::string text) {
  parser_ = gb::Parser::Create(GetInstructionParserProgram());
  if (parser_ == nullptr) {
    return gb::ParseError("Internal error: Invalid parser or lexer program");
  }
  gb::LexerContentId content =
      parser_->GetLexer().AddFileContent(filename, std::move(text));
  if (content == gb::kNoLexerContent) {
    return gb::ParseError("Instruction set program is too big");
  }
  return parser_->Parse(content, "InstructionSet");
}

bool InstructionSetAssembler::AssembleValueSet(
    const gb::ParsedItem& parsed_value_set) {
  auto& value_set_def = result_->value_set_defs_.emplace_back();
  value_set_def.name = AddString(parsed_value_set.GetString("name"));
  auto [it, inserted] = value_set_map_.try_emplace(value_set_def.name);
  if (!inserted) {
    return ErrorAt(parsed_value_set, "Value set '", value_set_def.name,
                   "' is already defined");
  }
  ValueSetInfo& value_set_info = it->second;
  value_set_info.def = parsed_value_set.GetToken().GetTokenIndex();

  const int64_t bits = parsed_value_set.GetInt("bits");
  if (bits < 0 || bits > 8 ||
      !Argument(ArgType::kImmediate, static_cast<int>(bits)).IsValid()) {
    return ErrorAt(parsed_value_set, "Invalid bits value: ", bits);
  }
  value_set_def.size = static_cast<uint8_t>(bits);
  value_set_info.size = value_set_def.size;

  // The value must be checked here, as it is narrowed to fit NamedValueDef.
  // CompileInstructionSet checks the rest.
  auto& named_value_defs = result_->named_value_defs_.emplace_back();
  for (const auto& parsed_value : parsed_value_set.GetItems("values")) {
    std::string_view name = AddString(parsed_value.GetString("name"));
    const int64_t value = parsed_value.GetInt("value");
    if (value < 0 || value >= (int64_t{1} << bits)) {
      return ErrorAt(parsed_value, "Value ", name, " = ", value,
                     " does not fit in ", bits, " bits");
    }
    named_value_defs.push_back(
        {.name = name, .value = static_cast<uint8_t>(value)});
  }
  value_set_def.values = named_value_defs;
  return true;
}

bool InstructionSetAssembler::AssembleMacro(
    const gb::ParsedItem& parsed_macro) {
  const int macro_index = result_->macro_defs_.size();
  auto& macro_def = result_->macro_defs_.emplace_back();
  macro_def.name = AddString(parsed_macro.GetString("name"));
  if (!macro_map_.insert({macro_def.name, macro_index}).second) {
    return ErrorAt(parsed_macro, "Macro '", macro_def.name,
                   "' is already defined");
  }
  if (std::string_view parsed_param = parsed_macro.GetString("param");
      !parsed_param.empty()) {
    macro_def.param =
        (parsed_param == "p" ? ArgType::kWordReg : ArgType::kDwordReg);
  }
  if (std::string_view parsed_ret = parsed_macro.GetString("ret");
      !parsed_ret.empty()) {
    macro_def.ret =
        (parsed_ret == "r" ? ArgType::kWordReg : ArgType::kDwordReg);
  }
  MacroTokens& macro_tokens = macro_token_map_[macro_def.name];
  macro_tokens.def = parsed_macro.GetToken().GetTokenIndex();
  auto& macro_code_defs = result_->macro_code_defs_.emplace_back();
  for (const auto& parsed_code : parsed_macro.GetItems("code")) {
    if (!AssembleMacroCode(parsed_code, macro_code_defs.emplace_back())) {
      return false;
    }
  }

  int fixed_bits = 0;
  if (const auto* parsed_bits = parsed_macro.GetItem("bits");
      parsed_bits != nullptr) {
    fixed_bits = parsed_bits->GetToken().GetInt();
    if (fixed_bits < 1 || fixed_bits > 8) {
      return ErrorAt(parsed_macro, "Invalid bits value: ", fixed_bits);
    }
  }
  const int max_bits = (fixed_bits > 0 ? fixed_bits : 8);
  const int max_values = 1 << max_bits;

  int sum_values = 0;
  int total_bits = std::max(fixed_bits, 1);
  for (const MacroCodeDef& code_def : macro_code_defs) {
    if (code_def.arg.size > total_bits) {
      total_bits = code_def.arg.size;
    }
    if (total_bits > max_bits) {
      return ErrorAt(parsed_macro, "Macro code \"", code_def.source,
                     "\" size exceeds max bits value: ", max_bits);
    }
    sum_values += 1 << code_def.arg.size;
  }
  while ((1 << total_bits) < sum_values) {
    ++total_bits;
  }
  if (total_bits > max_bits) {
    return ErrorAt(parsed_macro, "Macro requires ", total_bits,
                   " bits for all codes which exceeds max bits: ", max_bits);
  }

  // Sort the macro code definitions by argument size (largest to smallest), so
  // we can assign prefix values trivially. Codes of the same size keep the
  // order they are written in, which programs depend on.
  std::stable_sort(macro_code_defs.begin(), macro_code_defs.end(),
                   [](const MacroCodeDef& a, const MacroCodeDef& b) {
                     return a.arg.size > b.arg.size;
                   });

  int next_code = 0;
  for (int i = 0; i < macro_code_defs.size(); ++i) {
    auto& prefix = macro_code_defs[i].prefix;
    const int value_bits = macro_code_defs[i].arg.size;
    prefix.size = total_bits - value_bits;
    prefix.value = static_cast<int>(next_code >> value_bits);
    next_code += 1 << value_bits;
  }

  macro_def.size = total_bits;
  macro_def.code = macro_code_defs;
  return true;
}

bool InstructionSetAssembler::AssembleMacroCode(
    const gb::ParsedItem& parsed_macro_code, MacroCodeDef& macro_code_def) {
  const int macro_code_index = result_->macro_code_defs_.size() - 1;
  const MacroDef& macro_def = result_->macro_defs_.back();

  macro_code_def.source = AddString(parsed_macro_code.GetString("source"));
  std::string source_key =
      GetMacroCodeSourceKey(macro_def.name, macro_code_def.source);
  if (!macro_code_map_.insert({std::move(source_key), macro_code_index})
           .second) {
    return ErrorAt(parsed_macro_code, "Macro code source '",
                   macro_code_def.source, "' is already defined");
  }
  MacroCodeTokens& macro_code_tokens =
      macro_token_map_[macro_def.name].code[macro_code_def.source];
  macro_code_tokens.def = parsed_macro_code.GetToken().GetTokenIndex();

  if (!AssembleMacroCodeSource(parsed_macro_code, macro_code_def.source,
                               macro_code_def)) {
    return false;
  }

  std::string_view ret = parsed_macro_code.GetString("ret");
  if (macro_def.ret == ArgType::kNone) {
    if (!ret.empty()) {
      return ErrorAt(parsed_macro_code, "Macro does not return a value");
    }
  } else if (ret.empty()) {
    return ErrorAt(parsed_macro_code, "Macro code missing return value");
  } else if (macro_def.ret == ArgType::kWordReg) {
    // There is ambiguitiy between "p" and "m" for word registers, and "p0" and
    // "m0" for dword registers. We need to check the macro definition to
    // determine which is valid, as the Compile step cannot determine the
    // difference as they result in the same value.
    if ((ret == "p" && macro_def.param != ArgType::kWordReg) ||
        (ret == "p0" && macro_def.param != ArgType::kDwordReg) ||
        (ret == "m" && macro_code_def.arg.type != ArgType::kWordReg) ||
        (ret == "m0" && macro_code_def.arg.type != ArgType::kDwordReg)) {
      macro_code_def.ret = CpuCore::kInvalidReg;
    } else {
      macro_code_def.ret = CpuCore::GetVirtualWordRegFromName(ret);
    }
    if (macro_code_def.ret == CpuCore::kInvalidReg) {
      return ErrorAt(parsed_macro_code,
                     "Invalid macro code return word register: ", ret);
    }
  } else if (macro_def.ret == ArgType::kDwordReg) {
    macro_code_def.ret = CpuCore::GetVirtualDwordRegFromName(ret);
    if (macro_code_def.ret == CpuCore::kInvalidReg) {
      return ErrorAt(parsed_macro_code,
                     "Invalid macro code return dword register: ", ret);
    }
  }

  std::string code_string;
  if (!AssembleMicrocode(parsed_macro_code.GetItems("code"),
                         macro_code_tokens.code, code_string)) {
    return false;
  }
  macro_code_def.code = AddString(code_string);
  return true;
}

bool InstructionSetAssembler::AssembleMacroCodeSource(
    const gb::ParsedItem& parsed_macro_code, std::string_view source,
    MacroCodeDef& macro_code_def) {
  auto parser = gb::Parser::Create(GetSourceParserProgram());
  if (parser == nullptr) {
    return Error("Internal error: Invalid parser or lexer program");
  }
  gb::LexerContentId content =
      parser->GetLexer().AddContent(std::string(source));
  DCHECK(content != gb::kNoLexerContent);
  auto result = parser->Parse(content, "Source");
  if (!result.IsOk()) {
    return ErrorAt(parsed_macro_code, "Invalid macro code source: \"", source,
                   "\". Error: ", result.GetError().GetMessage());
  }

  bool has_macro_arg = false;
  for (const auto& arg : result->GetItems("args")) {
    if (GetWholeArgPlaceholder(arg) == "v") {
      macros_with_whole_arg_value_.insert(result_->macro_defs_.back().name);
    }
    for (std::string_view placeholder : GetPlaceholders(arg)) {
      if (IsValuePlaceholder(placeholder)) {
        continue;
      }
      if (has_macro_arg) {
        return ErrorAt(
            parsed_macro_code, "Invalid macro code source: \"", source,
            "\". Only one non-value macro argument type ($r, $R, $#, or a "
            "value set) is allowed for a macro.");
      }
      has_macro_arg = true;
      if (!AssembleArgument(placeholder, macro_code_def.arg)) {
        return ErrorAt(parsed_macro_code, "Invalid macro code source: \"",
                       source, "\". $", placeholder,
                       " is not a placeholder or value set.");
      }
      if (macro_code_def.arg.type == ArgType::kMacro) {
        return ErrorAt(parsed_macro_code, "Invalid macro code source: \"",
                       source, "\". $", placeholder,
                       " is not allowed for a macro arg.");
      }
    }
  }

  return true;
}

bool InstructionSetAssembler::AssembleInstruction(
    const gb::ParsedItem& parsed_instruction) {
  const int instruction_index = result_->instruction_defs_.size();
  auto& instruction_def = result_->instruction_defs_.emplace_back();

  const gb::ParsedItem* parsed_opcode = parsed_instruction.GetItem("opcode");
  if (parsed_opcode == nullptr) {
    needs_opcode_.push_back(instruction_index);
  } else {
    int op = parsed_instruction.GetInt("opcode");
    if (op < 0 || op > 255) {
      return ErrorAt(parsed_instruction, "Invalid opcode value: ", op);
    }
    instruction_def.op = static_cast<uint8_t>(op);
    if (!opcode_values_.insert(op).second) {
      return ErrorAt(parsed_instruction, "Opcode ", op, " is already defined");
    }
  }

  std::string op_name(parsed_instruction.GetString("name"));
  if (std::string_view ext = parsed_instruction.GetString("ext");
      !ext.empty()) {
    absl::StrAppend(&op_name, ".", ext);
  }
  instruction_def.op_name = AddString(op_name);
  if (!instruction_map_.insert({instruction_def.op_name, instruction_index})
           .second) {
    return ErrorAt(parsed_instruction, "Instruction '", instruction_def.op_name,
                   "' is already defined");
  }
  InstructionTokens& instruction_tokens =
      instruction_token_map_[instruction_def.op_name];
  instruction_tokens.def = parsed_instruction.GetToken().GetTokenIndex();

  instruction_def.source = AddString(parsed_instruction.GetString("source"));
  WholeArgs whole_args;
  if (!AssembleInstructionCodeSource(parsed_instruction, instruction_def.source,
                                     instruction_def, whole_args)) {
    return false;
  }
  Argument* macro_arg = nullptr;
  if (instruction_def.arg1.type == ArgType::kMacro) {
    macro_arg = &instruction_def.arg1;
  } else if (instruction_def.arg2.type == ArgType::kMacro) {
    macro_arg = &instruction_def.arg2;
  }
  std::string code_string;
  std::string arg_macro_name;
  if (!AssembleMicrocode(parsed_instruction.GetItems("code"),
                         instruction_tokens.code, code_string, macro_arg,
                         &arg_macro_name)) {
    return false;
  }
  instruction_def.code = AddString(code_string);
  if (!arg_macro_name.empty()) {
    instruction_def.arg_macro_name = AddString(arg_macro_name);
  }

  // A relative instruction needs a form where "$v" is a whole argument, either
  // in its own source, or in a macro code that takes the place of a "$m" that
  // is a whole argument.
  if (parsed_instruction.GetItem("relative") != nullptr) {
    instruction_def.relative = true;
    if (!whole_args.value &&
        !(whole_args.macro && macros_with_whole_arg_value_.contains(
                                  instruction_def.arg_macro_name))) {
      return ErrorAt(parsed_instruction, "Relative instruction '",
                     instruction_def.op_name,
                     "' has no form where $v is a whole argument");
    }
  }
  return true;
}

bool InstructionSetAssembler::AssembleInstructionCodeSource(
    const gb::ParsedItem& parsed_instruction, std::string_view source,
    InstructionDef& instruction_def, WholeArgs& whole_args) {
  auto parser = gb::Parser::Create(GetSourceParserProgram());
  if (parser == nullptr) {
    return Error("Internal error: Invalid parser or lexer program");
  }
  gb::LexerContentId content =
      parser->GetLexer().AddContent(std::string(source));
  DCHECK(content != gb::kNoLexerContent);
  auto result = parser->Parse(content, "Source");
  if (!result.IsOk()) {
    return ErrorAt(parsed_instruction, "Invalid instruction code source: \"",
                   source, "\". Error: ", result.GetError().GetMessage());
  }

  int instruction_arg_count = 0;
  bool has_macro_arg = false;
  for (const auto& arg : result->GetItems("args")) {
    const std::string_view whole_arg_placeholder = GetWholeArgPlaceholder(arg);
    if (whole_arg_placeholder == "v") {
      whole_args.value = true;
    }
    for (std::string_view placeholder : GetPlaceholders(arg)) {
      if (IsValuePlaceholder(placeholder)) {
        continue;
      }
      if (instruction_arg_count == 2) {
        return ErrorAt(parsed_instruction,
                       "Invalid instruction code source: \"", source,
                       "\". At most two non-value argument types ($m, $r, "
                       "$R, $#, or a value set) are allowed for an "
                       "instruction.");
      }
      ++instruction_arg_count;
      Argument& argument = (instruction_arg_count == 1 ? instruction_def.arg1
                                                       : instruction_def.arg2);
      if (!AssembleArgument(placeholder, argument)) {
        return ErrorAt(parsed_instruction,
                       "Invalid instruction code source: \"", source, "\". $",
                       placeholder, " is not a placeholder or value set.");
      }
      if (argument.type == ArgType::kMacro) {
        if (has_macro_arg) {
          return ErrorAt(parsed_instruction,
                         "Invalid instruction code source: \"", source,
                         "\". Only one macro argument type ($m) is allowed for "
                         "an instruction.");
        }
        has_macro_arg = true;
        if (!whole_arg_placeholder.empty()) {
          whole_args.macro = true;
        }
      }
    }
  }
  return true;
}

bool InstructionSetAssembler::AssembleArgument(std::string_view placeholder,
                                               Argument& argument) {
  DCHECK(!IsValuePlaceholder(placeholder));
  const char type_char = placeholder[0];
  if (type_char != '#' && !IsPlaceholderName(placeholder)) {
    auto it = value_set_map_.find(placeholder);
    if (it == value_set_map_.end()) {
      return false;
    }
    argument = {ArgType::kImmediate, it->second.size};
    return true;
  }
  const uint8_t size =
      static_cast<uint8_t>(placeholder.size() < 2 ? 0 : placeholder[1] - '0');
  switch (type_char) {
    case 'r':
      argument.type = ArgType::kWordReg;
      argument.size = (size == 0 ? GetDefaultArgTypeSize(argument.type) : size);
      DCHECK(argument.IsValid());
      break;
    case 'R':
      argument.type = ArgType::kDwordReg;
      argument.size = (size == 0 ? GetDefaultArgTypeSize(argument.type) : size);
      DCHECK(argument.IsValid());
      break;
    case '#':
      argument.type = ArgType::kImmediate;
      argument.size = size;
      DCHECK(argument.IsValid());
      break;
    case 'm':
      argument.type = ArgType::kMacro;
      argument.size = size;
      // Argument may not be valid yet, but we will check later once we can
      // verify the macro size is correct.
      break;
  }
  return true;
}

bool InstructionSetAssembler::AssembleMicrocode(
    absl::Span<const gb::ParsedItem> parsed_micro_codes,
    CodeTokens& code_tokens, std::string& code_string, Argument* macro_arg,
    std::string* arg_macro_name) {
  code_tokens.reserve(parsed_micro_codes.size());
  bool has_macro = false;
  for (const auto& parsed_micro_code : parsed_micro_codes) {
    code_tokens.push_back(parsed_micro_code.GetToken().GetTokenIndex());
    if (auto label = parsed_micro_code.GetString("label"); !label.empty()) {
      absl::StrAppend(&code_string, label, ":");
    }
    if (auto macro_name = parsed_micro_code.GetString("macro");
        !macro_name.empty()) {
      if (has_macro) {
        return ErrorAt(parsed_micro_code, "Multiple macro calls in code");
      }
      has_macro = true;
      if (macro_arg == nullptr) {
        return ErrorAt(parsed_micro_code, "No macro argument specified");
      }
      auto it = macro_map_.find(macro_name);
      if (it == macro_map_.end()) {
        return ErrorAt(parsed_micro_code, "Undefined macro: ", macro_name);
      }
      const MacroDef& macro_def = result_->macro_defs_[it->second];
      if (macro_arg->size == 0) {
        macro_arg->size = macro_def.size;
      } else if (macro_arg->size != macro_def.size) {
        return ErrorAt(parsed_micro_code, "Macro argument size ",
                       macro_arg->size, " != macro ", macro_name, " size ",
                       macro_def.size);
      }
      DCHECK(arg_macro_name != nullptr);
      *arg_macro_name = macro_name;
      absl::StrAppend(&code_string, "$", macro_name);
      if (const auto& arg1 = parsed_micro_code.GetToken("arg1").ToString();
          !arg1.empty()) {
        absl::StrAppend(&code_string, "(", arg1, ")");
      }
    } else {
      absl::StrAppend(&code_string, parsed_micro_code.GetString("op"));
      if (const auto& arg1 = parsed_micro_code.GetToken("arg1").ToString();
          !arg1.empty()) {
        absl::StrAppend(&code_string, "(", arg1);
        const auto& mask1 = parsed_micro_code.GetToken("arg1.mask").ToString();
        if (!mask1.empty()) {
          absl::StrAppend(&code_string, ":", mask1);
        }
        if (const auto& arg2 = parsed_micro_code.GetToken("arg2").ToString();
            !arg2.empty()) {
          absl::StrAppend(&code_string, ",", arg2);
          const auto& mask2 =
              parsed_micro_code.GetToken("arg2.mask").ToString();
          if (!mask2.empty()) {
            absl::StrAppend(&code_string, ":", mask2);
          }
        }
        absl::StrAppend(&code_string, ")");
      }
    }
    absl::StrAppend(&code_string, ";");
  }
  return true;
}

bool InstructionSetAssembler::Compile() {
  result_->instruction_set_def_ = {
      .instructions = result_->instruction_defs_,
      .macros = result_->macro_defs_,
      .value_sets = result_->value_set_defs_,
  };
  InstructionError error;
  result_->instruction_set_ =
      CompileInstructionSet(result_->instruction_set_def_, &error);
  if (error.message.empty()) {
    return true;
  }
  if (error.def == InstructionError::Def::kValueSet) {
    DCHECK(value_set_map_.contains(error.def_name));
    return ErrorAt(value_set_map_.find(error.def_name)->second.def,
                   error.message);
  }
  if (error.def == InstructionError::Def::kMacro) {
    DCHECK(macro_token_map_.contains(error.def_name));
    const auto& macro_tokens = macro_token_map_.find(error.def_name)->second;
    if (error.macro_source_name.empty()) {
      return ErrorAt(macro_tokens.def, error.message);
    }
    DCHECK(macro_tokens.code.contains(error.macro_source_name));
    const auto& macro_code_tokens =
        macro_tokens.code.find(error.macro_source_name)->second;
    if (error.code_index < 0) {
      return ErrorAt(macro_code_tokens.def, error.message);
    }
    DCHECK(error.code_index < macro_code_tokens.code.size());
    return ErrorAt(macro_code_tokens.code[error.code_index], error.message);
  }
  if (error.def == InstructionError::Def::kInstruction) {
    DCHECK(instruction_token_map_.contains(error.def_name));
    const auto& instruction_tokens =
        instruction_token_map_.find(error.def_name)->second;
    if (error.code_index < 0) {
      return ErrorAt(instruction_tokens.def, error.message);
    }
    DCHECK(error.code_index < instruction_tokens.code.size());
    return ErrorAt(instruction_tokens.code[error.code_index], error.message);
  }
  return Error(error.message);
}

std::unique_ptr<AsmInstructionSet> AssembleInstructionSet(
    std::string text, gb::ParseError* error) {
  return InstructionSetAssembler(error).Assemble("instruction_set",
                                                 std::move(text));
}

std::unique_ptr<AsmInstructionSet> AssembleInstructionSetFile(
    gb::FileSystem& file_system, std::string_view path, gb::ParseError* error) {
  std::string normalized_path = gb::NormalizePath(path);
  std::string text;
  if (!file_system.ReadFile(normalized_path, &text)) {
    if (error != nullptr) {
      *error = gb::ParseError(
          absl::StrCat("Could not read file: ", normalized_path));
    }
    return nullptr;
  }
  return InstructionSetAssembler(error).Assemble(
      gb::RemoveProtocol(normalized_path), std::move(text));
}

}  // namespace oz3
