// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_assembler.h"

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "absl/base/no_destructor.h"
#include "absl/container/flat_hash_set.h"
#include "absl/memory/memory.h"
#include "absl/status/statusor.h"
#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "gb/file/path.h"
#include "gb/parse/parser.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/instruction_def.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/source_pattern.h"

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
    $distinct="distinct";
    "exclude_a" ":" $exclude_a=%ident;
    "exclude_b" ":" $exclude_b=%ident;
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

  // Reports an error in the def at the source it came from.
  bool ErrorAt(const InstructionError& error);

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
                                     InstructionDef& instruction_def);
  bool AssembleExcludedRegisters(const gb::ParsedItem& parsed_instruction,
                                 std::string_view option, const Argument& arg,
                                 uint16_t& excluded);
  bool AssembleMicrocode(absl::Span<const gb::ParsedItem> parsed_micro_codes,
                         CodeTokens& code_tokens, std::string& code_string,
                         Argument* macro_arg = nullptr,
                         std::string* arg_macro_name = nullptr);
  bool Compile();
  bool CheckSyntax();

  gb::ParseError* const error_;
  std::unique_ptr<gb::Parser> parser_;
  std::unique_ptr<AsmInstructionSet> result_;
  absl::flat_hash_map<std::string_view, int> macro_map_;
  std::vector<int> needs_opcode_;
  absl::flat_hash_set<int> opcode_values_;
  absl::flat_hash_map<std::string_view, gb::TokenIndex> value_set_token_map_;
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

  if (!Compile() || !CheckSyntax()) {
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

  // InstructionSetSyntax checks the rules for value sets, apart from what
  // reading them needs: valid bits, which patterns that use the set take as
  // their argument's size, and values that fit in the def's bytes. Its errors
  // for a set with the same name as another are reported at the last one.
  value_set_token_map_[value_set_def.name] =
      parsed_value_set.GetToken().GetTokenIndex();
  const int64_t bits = parsed_value_set.GetInt("bits");
  if (bits < 1 || bits > 8) {
    return ErrorAt(parsed_value_set, "Invalid bits value: ", bits);
  }
  value_set_def.size = static_cast<uint8_t>(bits);

  auto& named_value_defs = result_->named_value_defs_.emplace_back();
  for (const auto& parsed_value : parsed_value_set.GetItems("values")) {
    std::string_view name =
        AddString(absl::AsciiStrToUpper(parsed_value.GetString("name")));
    const int64_t value = parsed_value.GetInt("value");
    if (value < 0 || value > 255) {
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
  const MacroDef& macro_def = result_->macro_defs_.back();

  // Errors in a macro code are found by its source, so it must be unique.
  // InstructionSetSyntax checks the rest of the rule for macro code sources.
  macro_code_def.source = AddString(parsed_macro_code.GetString("source"));
  auto [it, inserted] = macro_token_map_[macro_def.name].code.try_emplace(
      macro_code_def.source,
      MacroCodeTokens{.def = parsed_macro_code.GetToken().GetTokenIndex()});
  if (!inserted) {
    return ErrorAt(parsed_macro_code, "Macro code source '",
                   macro_code_def.source, "' is already defined");
  }
  MacroCodeTokens& macro_code_tokens = it->second;

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
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseMacroCode(source, result_->value_set_defs_);
  if (!pattern.ok()) {
    return ErrorAt(parsed_macro_code, "Invalid macro code source: \"", source,
                   "\". ", pattern.status().message());
  }
  if (!pattern->GetArgs().empty()) {
    macro_code_def.arg = pattern->GetArgs()[0];
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
  absl::AsciiStrToUpper(&op_name);
  instruction_def.op_name = AddString(op_name);

  // Errors in an instruction are found by its name, so it must be unique.
  auto [it, inserted] = instruction_token_map_.try_emplace(
      instruction_def.op_name,
      InstructionTokens{.def = parsed_instruction.GetToken().GetTokenIndex()});
  if (!inserted) {
    return ErrorAt(parsed_instruction, "Instruction '", instruction_def.op_name,
                   "' is already defined");
  }
  InstructionTokens& instruction_tokens = it->second;

  instruction_def.source = AddString(parsed_instruction.GetString("source"));
  if (!AssembleInstructionCodeSource(parsed_instruction, instruction_def.source,
                                     instruction_def) ||
      !AssembleExcludedRegisters(parsed_instruction, "exclude_a",
                                 instruction_def.arg1,
                                 instruction_def.arg1_excluded) ||
      !AssembleExcludedRegisters(parsed_instruction, "exclude_b",
                                 instruction_def.arg2,
                                 instruction_def.arg2_excluded)) {
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

  instruction_def.relative =
      (parsed_instruction.GetItem("relative") != nullptr);
  instruction_def.distinct_args =
      (parsed_instruction.GetItem("distinct") != nullptr);
  return true;
}

bool InstructionSetAssembler::AssembleInstructionCodeSource(
    const gb::ParsedItem& parsed_instruction, std::string_view source,
    InstructionDef& instruction_def) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction(source, result_->value_set_defs_);
  if (!pattern.ok()) {
    return ErrorAt(parsed_instruction, "Invalid instruction code source: \"",
                   source, "\". ", pattern.status().message());
  }
  auto args = pattern->GetArgs();
  if (args.size() > 0) {
    instruction_def.arg1 = args[0];
  }
  if (args.size() > 1) {
    instruction_def.arg2 = args[1];
  }
  return true;
}

// Each register an option names is held in `excluded`, as its number in `arg`.
// The registers are validated here, so each error is at its name, and
// InstructionSetSyntax validates the rest of the rules, such as leaving the
// argument at least one register.
bool InstructionSetAssembler::AssembleExcludedRegisters(
    const gb::ParsedItem& parsed_instruction, std::string_view option,
    const Argument& arg, uint16_t& excluded) {
  for (const gb::ParsedItem& parsed_register :
       parsed_instruction.GetItems(option)) {
    if (arg.type != ArgType::kWordReg && arg.type != ArgType::kDwordReg) {
      return ErrorAt(parsed_register, option,
                     " is for a register argument, but the argument isn't a "
                     "register");
    }
    const std::string_view name = parsed_register.GetToken().GetString();
    const int reg = GetRegisterNumber(arg.type, name);
    if (reg < 0 || reg >= (1 << arg.size)) {
      return ErrorAt(parsed_register, option, ": ", name,
                     " isn't a register the argument can hold");
    }
    if ((excluded & (1 << reg)) != 0) {
      return ErrorAt(parsed_register, option, ": ", name,
                     " is already excluded");
    }
    excluded |= static_cast<uint16_t>(1 << reg);
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
  if (result_->instruction_set_ == nullptr) {
    return ErrorAt(error);
  }
  return true;
}

bool InstructionSetAssembler::CheckSyntax() {
  InstructionError error;
  if (InstructionSetSyntax::Create(result_->instruction_set_def_,
                                   result_->instruction_set_.get(),
                                   &error) == nullptr) {
    return ErrorAt(error);
  }
  return true;
}

bool InstructionSetAssembler::ErrorAt(const InstructionError& error) {
  if (error.def == InstructionError::Def::kValueSet) {
    DCHECK(value_set_token_map_.contains(error.def_name));
    return ErrorAt(value_set_token_map_.find(error.def_name)->second,
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
