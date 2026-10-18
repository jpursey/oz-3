// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_set_syntax.h"

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/log/check.h"
#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/ascii.h"
#include "absl/strings/match.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/parse/token.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/instruction_def.h"
#include "oz3/core/instruction_set.h"
#include "oz3/tools/program_lexer.h"
#include "oz3/tools/source_pattern.h"

namespace oz3 {

namespace {

using Part = SourcePattern::Part;

// Returns true if the part is a placeholder for a value: $#, a value set, $v,
// or $V.
bool IsValuePart(const Part& part) {
  return part.IsValue() && part.kind != Part::Kind::kLiteral;
}

// Returns true if every statement that matches one part could match the other.
// A literal number matches any expression with its value, so it is the same as
// any value part.
bool IsWrittenTheSameWay(const Part& a, const Part& b) {
  if (IsValuePart(a) || IsValuePart(b)) {
    return a.IsValue() && b.IsValue();
  }
  if (a.kind != b.kind) {
    return false;
  }
  if (a.kind == Part::Kind::kArgument) {
    return a.arg.type == b.arg.type;
  }
  if (a.literal_type != b.literal_type) {
    return false;
  }
  if (a.literal_type == gb::kTokenIdentifier) {
    return absl::EqualsIgnoreCase(std::get<std::string>(a.literal),
                                  std::get<std::string>(b.literal));
  }
  return a.literal == b.literal;
}

bool IsWrittenTheSameWay(const SourcePattern& a, const SourcePattern& b) {
  auto a_parts = a.GetParts();
  auto b_parts = b.GetParts();
  if (a_parts.size() != b_parts.size()) {
    return false;
  }
  for (int i = 0; i < static_cast<int>(a_parts.size()); ++i) {
    if (!IsWrittenTheSameWay(a_parts[i], b_parts[i])) {
      return false;
    }
  }
  return true;
}

// Returns true if a placeholder in a pattern is the argument in a def: the
// same type and size, except that a $m without a size is any size.
bool IsSameArgument(const Argument& placeholder, const Argument& def_arg) {
  return placeholder.type == def_arg.type &&
         (placeholder.size == def_arg.size ||
          (placeholder.type == ArgType::kMacro && placeholder.size == 0));
}

// Returns the name of the register a register argument's value is, as
// InstructionSet::Decode decodes it.
std::string_view GetRegisterName(const InstructionForm::Arg& arg, int value) {
  return arg.kind == InstructionForm::Arg::Kind::kWordReg
             ? CpuCore::GetWordRegName(CpuCore::R0 + value)
             : CpuCore::GetDwordRegName(CpuCore::D0 + value * 2);
}

// Returns the value of a register or code value argument in a code word.
int GetCodeValue(const InstructionForm::Arg& arg,
                 absl::Span<const uint16_t> words) {
  return (words[0] >> arg.shift) & ((1 << arg.bits) - 1);
}

// Returns how errors name a form's source: the instruction's source, and its
// macro code's if it has one.
std::string GetSourceName(const InstructionForm& form) {
  std::string name =
      absl::StrCat("Source \"", form.GetInstruction().source, "\"");
  if (form.GetMacroCode() != nullptr) {
    absl::StrAppend(&name, " with macro code \"", form.GetMacroCode()->source,
                    "\"");
  }
  return name;
}

InstructionForm::Arg::Kind GetArgKind(const Part& part) {
  using Kind = InstructionForm::Arg::Kind;
  if (part.kind == Part::Kind::kWord) {
    return Kind::kWord;
  }
  if (part.kind == Part::Kind::kDword) {
    return Kind::kDword;
  }
  switch (part.arg.type) {
    case ArgType::kWordReg:
      return Kind::kWordReg;
    case ArgType::kDwordReg:
      return Kind::kDwordReg;
    default:
      return Kind::kCodeValue;
  }
}

}  // namespace

//==============================================================================
// InstructionForm
//==============================================================================

bool InstructionForm::Encode(absl::Span<const int64_t> values, uint16_t address,
                             std::vector<uint16_t>& words,
                             std::string* error) const {
  const int start = static_cast<int>(words.size());
  auto fail = [&](const auto&... message) {
    words.resize(start);
    if (error != nullptr) {
      *error = absl::StrCat(message...);
    }
    return false;
  };
  if (values.size() != args_.size()) {
    return fail("Expected ", args_.size(), " values, but got ", values.size());
  }

  words.push_back(code_word_);
  for (int i = 0; i < static_cast<int>(args_.size()); ++i) {
    const Arg& arg = args_[i];
    const int64_t value = values[i];
    switch (arg.kind) {
      case Arg::Kind::kWordReg:
      case Arg::Kind::kDwordReg:
      case Arg::Kind::kCodeValue:
        if (value < 0 || value >= (int64_t{1} << arg.bits)) {
          if (arg.kind == Arg::Kind::kCodeValue) {
            return fail("Value ", value, " does not fit in ", arg.bits,
                        " bits");
          }
          return fail("Register ", value, " is not ", GetRegisterName(arg, 0),
                      " to ", GetRegisterName(arg, (1 << arg.bits) - 1));
        }
        words[start] |= static_cast<uint16_t>(value << arg.shift);
        break;
      case Arg::Kind::kWord: {
        if (value < -0x8000 || value > 0xFFFF) {
          return fail("Value ", value, " does not fit in a word");
        }
        uint16_t word = static_cast<uint16_t>(value);
        if (arg.relative) {
          // The offset from the next instruction, wrapping as IP does.
          word -= static_cast<uint16_t>(address + size_);
        }
        words.push_back(word);
      } break;
      case Arg::Kind::kDword: {
        if (value < -0x80000000LL || value > 0xFFFFFFFFLL) {
          return fail("Value ", value, " does not fit in a dword");
        }
        const uint32_t dword = static_cast<uint32_t>(value);
        words.push_back(static_cast<uint16_t>(dword));
        words.push_back(static_cast<uint16_t>(dword >> 16));
      } break;
    }
  }
  return true;
}

std::string InstructionForm::Disassemble(absl::Span<const uint16_t> words,
                                         uint16_t address) const {
  DCHECK_EQ(static_cast<int>(words.size()), size_);
  std::string text(instruction_->op_name);
  if (!pattern_.empty()) {
    text.push_back(' ');
  }

  // Each placeholder is replaced by its value, and the text around them is
  // written as the pattern writes it.
  const std::string_view pattern = pattern_;
  int pattern_pos = 0;
  int arg_index = 0;
  int word_index = 1;
  for (const Part& part : parts_) {
    if (part.kind == Part::Kind::kLiteral) {
      continue;
    }
    absl::StrAppend(&text,
                    pattern.substr(pattern_pos, part.offset - pattern_pos));
    pattern_pos = part.offset + part.size;

    const Arg& arg = args_[arg_index++];
    switch (arg.kind) {
      case Arg::Kind::kWordReg:
      case Arg::Kind::kDwordReg:
        absl::StrAppend(&text, GetRegisterName(arg, GetCodeValue(arg, words)));
        break;
      case Arg::Kind::kCodeValue: {
        const int code_value = GetCodeValue(arg, words);
        std::string value_text = absl::StrCat(code_value);
        if (arg.value_set != nullptr) {
          for (const NamedValueDef& value : arg.value_set->values) {
            if (value.value == code_value) {
              value_text = value.name;
              break;
            }
          }
        }
        absl::StrAppend(&text, value_text);
      } break;
      case Arg::Kind::kWord: {
        uint16_t word = words[word_index++];
        if (arg.relative) {
          word += static_cast<uint16_t>(address + size_);
        }
        absl::StrAppend(&text, FormatHexNumber(word, 4));
      } break;
      case Arg::Kind::kDword: {
        const uint32_t dword =
            words[word_index] |
            (static_cast<uint32_t>(words[word_index + 1]) << 16);
        word_index += 2;
        absl::StrAppend(&text, FormatHexNumber(dword, 8));
      } break;
    }
  }
  absl::StrAppend(&text, pattern.substr(pattern_pos));
  return text;
}

//==============================================================================
// InstructionSetSyntaxBuilder
//==============================================================================

// Checks an InstructionSetDef, and builds its InstructionSetSyntax.
class InstructionSetSyntaxBuilder final {
 public:
  // `def` must outlive this, and the syntax it builds.
  InstructionSetSyntaxBuilder(const InstructionSetDef* def,
                              InstructionError* error)
      : def_(def), error_(error) {}

  std::unique_ptr<InstructionSetSyntax> Build(
      const InstructionSet* instruction_set);

 private:
  // A macro, and the parsed patterns of its codes, in the macro's order.
  struct MacroSyntax {
    const MacroDef* def = nullptr;
    std::vector<SourcePattern> patterns;
  };

  template <typename... Args>
  bool Error(InstructionError::Def def, std::string_view def_name,
             std::string_view macro_source_name, Args&&... args) {
    if (error_ != nullptr) {
      *error_ = {.message = absl::StrCat(std::forward<Args>(args)...),
                 .def = def,
                 .def_name = std::string(def_name),
                 .macro_source_name = std::string(macro_source_name)};
    }
    return false;
  }
  template <typename... Args>
  bool ValueSetError(const ValueSetDef& value_set, Args&&... args) {
    return Error(InstructionError::Def::kValueSet, value_set.name, {},
                 std::forward<Args>(args)...);
  }
  template <typename... Args>
  bool MacroCodeError(const MacroDef& macro, const MacroCodeDef& code,
                      Args&&... args) {
    return Error(InstructionError::Def::kMacro, macro.name, code.source,
                 std::forward<Args>(args)...);
  }
  template <typename... Args>
  bool InstructionDefError(const InstructionDef& instruction, Args&&... args) {
    return Error(InstructionError::Def::kInstruction, instruction.op_name, {},
                 std::forward<Args>(args)...);
  }

  bool CheckValueSet(const ValueSetDef& value_set,
                     absl::flat_hash_set<std::string_view>& names);
  bool CheckInstructionNames();
  bool ParseMacro(const MacroDef& macro);
  bool ParseMacroCode(const MacroDef& macro, const MacroCodeDef& code,
                      std::vector<SourcePattern>& patterns);
  bool AddInstruction(const InstructionDef& instruction,
                      const InstructionSet& instruction_set);
  bool CheckInstructionArgs(const InstructionDef& instruction,
                            const SourcePattern& pattern);
  void AddForm(const InstructionDef& instruction, const SourcePattern& pattern,
               const MacroSyntax* macro, int code_index);
  void AddPart(InstructionForm& form, const InstructionDef& instruction,
               Part part, int shift);
  bool CheckSize(const InstructionForm& form,
                 const InstructionSet& instruction_set);
  void IndexForms();

  const InstructionSetDef* const def_;
  InstructionError* const error_;
  std::unique_ptr<InstructionSetSyntax> syntax_;
  absl::flat_hash_map<std::string_view, MacroSyntax> macros_;
};

std::unique_ptr<InstructionSetSyntax> InstructionSetSyntaxBuilder::Build(
    const InstructionSet* instruction_set) {
  std::shared_ptr<const InstructionSet> compiled;
  if (instruction_set == nullptr) {
    compiled = CompileInstructionSet(*def_, error_);
    if (compiled == nullptr) {
      return nullptr;
    }
    instruction_set = compiled.get();
  }

  absl::flat_hash_set<std::string_view> value_set_names;
  for (const ValueSetDef& value_set : def_->value_sets) {
    if (!CheckValueSet(value_set, value_set_names)) {
      return nullptr;
    }
  }
  if (!CheckInstructionNames()) {
    return nullptr;
  }
  for (const MacroDef& macro : def_->macros) {
    if (!ParseMacro(macro)) {
      return nullptr;
    }
  }

  syntax_ = absl::WrapUnique(new InstructionSetSyntax);
  for (const InstructionDef& instruction : def_->instructions) {
    if (!AddInstruction(instruction, *instruction_set)) {
      return nullptr;
    }
  }
  IndexForms();
  return std::move(syntax_);
}

bool InstructionSetSyntaxBuilder::CheckValueSet(
    const ValueSetDef& value_set,
    absl::flat_hash_set<std::string_view>& names) {
  if (!names.insert(value_set.name).second) {
    return ValueSetError(value_set, "Value set '", value_set.name,
                         "' is already defined");
  }
  if (IsPlaceholderName(value_set.name)) {
    return ValueSetError(value_set, "Value set name is the placeholder $",
                         value_set.name);
  }
  if (!Argument(ArgType::kImmediate, value_set.size).IsValid()) {
    return ValueSetError(
        value_set, "Invalid bits value: ", static_cast<int>(value_set.size));
  }
  if (value_set.values.empty()) {
    return ValueSetError(value_set, "Value set has no values");
  }

  // Programs write value names in any case.
  absl::flat_hash_set<std::string> value_names;
  for (const NamedValueDef& value : value_set.values) {
    if (!value_names.insert(absl::AsciiStrToUpper(value.name)).second) {
      return ValueSetError(value_set, "Duplicate value name ", value.name);
    }
    if (value.value >= (1 << value_set.size)) {
      return ValueSetError(value_set, "Value ", value.name, " = ",
                           static_cast<int>(value.value), " does not fit in ",
                           static_cast<int>(value_set.size), " bits");
    }
  }
  return true;
}

bool InstructionSetSyntaxBuilder::CheckInstructionNames() {
  // Programs write instruction names in any case.
  absl::flat_hash_set<std::string> names;
  for (const InstructionDef& instruction : def_->instructions) {
    if (!names.insert(absl::AsciiStrToUpper(instruction.op_name)).second) {
      return InstructionDefError(instruction, "Instruction '",
                                 instruction.op_name, "' is already defined");
    }
  }
  return true;
}

bool InstructionSetSyntaxBuilder::ParseMacro(const MacroDef& macro) {
  MacroSyntax& macro_syntax = macros_[macro.name];
  macro_syntax = {.def = &macro};
  macro_syntax.patterns.reserve(macro.code.size());
  for (const MacroCodeDef& code : macro.code) {
    if (!ParseMacroCode(macro, code, macro_syntax.patterns)) {
      return false;
    }
  }
  return true;
}

bool InstructionSetSyntaxBuilder::ParseMacroCode(
    const MacroDef& macro, const MacroCodeDef& code,
    std::vector<SourcePattern>& patterns) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseMacroCode(code.source, def_->value_sets);
  if (!pattern.ok()) {
    return MacroCodeError(macro, code, "Invalid macro code source: \"",
                          code.source, "\". ", pattern.status().message());
  }
  auto args = pattern->GetArgs();
  if (args.empty() ? code.arg.type != ArgType::kNone
                   : !IsSameArgument(args[0], code.arg)) {
    return MacroCodeError(macro, code, "Macro code source \"", code.source,
                          "\" doesn't match its argument (",
                          ArgTypeToString(code.arg.type), " of ",
                          static_cast<int>(code.arg.size), " bits)");
  }

  // Each form of an instruction must be one a program can name, and its
  // macro's codes are what tell its forms apart.
  for (int i = 0; i < static_cast<int>(patterns.size()); ++i) {
    if (IsWrittenTheSameWay(patterns[i], *pattern)) {
      return MacroCodeError(macro, code, "Macro code source \"", code.source,
                            "\" is written the same way as \"",
                            macro.code[i].source, "\"");
    }
  }
  patterns.push_back(*std::move(pattern));
  return true;
}

bool InstructionSetSyntaxBuilder::AddInstruction(
    const InstructionDef& instruction, const InstructionSet& instruction_set) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction(instruction.source, def_->value_sets);
  if (!pattern.ok()) {
    return InstructionDefError(
        instruction, "Invalid instruction code source: \"", instruction.source,
        "\". ", pattern.status().message());
  }
  if (!CheckInstructionArgs(instruction, *pattern)) {
    return false;
  }

  const int first_form = static_cast<int>(syntax_->forms_.size());
  if (instruction.arg_macro_name.empty()) {
    AddForm(instruction, *pattern, nullptr, 0);
  } else {
    auto it = macros_.find(instruction.arg_macro_name);
    if (it == macros_.end()) {
      return InstructionDefError(instruction,
                                 "Unknown macro: ", instruction.arg_macro_name);
    }
    const MacroSyntax& macro = it->second;
    for (int i = 0; i < static_cast<int>(macro.patterns.size()); ++i) {
      AddForm(instruction, *pattern, &macro, i);
    }
  }

  bool has_relative_arg = false;
  for (int i = first_form; i < static_cast<int>(syntax_->forms_.size()); ++i) {
    const InstructionForm& form = syntax_->forms_[i];

    // Each pattern was checked as it was parsed, but a form's value can still
    // be followed by an operator from the other pattern, where a macro code's
    // value ends at the instruction's $m, or the instruction's value comes
    // just before it.
    if (absl::Status status =
            SourcePattern::CheckValueEnds(form.GetPattern(), form.GetParts());
        !status.ok()) {
      return InstructionDefError(instruction, GetSourceName(form), ": ",
                                 status.message());
    }
    if (!CheckSize(form, instruction_set)) {
      return false;
    }
    for (const InstructionForm::Arg& arg : form.args_) {
      has_relative_arg = has_relative_arg || arg.relative;
    }
  }
  if (instruction.relative && !has_relative_arg) {
    return InstructionDefError(instruction, "Relative instruction '",
                               instruction.op_name,
                               "' has no form where $v is a whole argument");
  }
  return true;
}

bool InstructionSetSyntaxBuilder::CheckInstructionArgs(
    const InstructionDef& instruction, const SourcePattern& pattern) {
  auto args = pattern.GetArgs();
  const Argument* def_args[] = {&instruction.arg1, &instruction.arg2};
  bool has_macro_arg = false;
  for (int i = 0; i < 2; ++i) {
    const bool matches = (i < static_cast<int>(args.size())
                              ? IsSameArgument(args[i], *def_args[i])
                              : def_args[i]->type == ArgType::kNone);
    if (!matches) {
      return InstructionDefError(
          instruction, "Source \"", instruction.source,
          "\" doesn't match the instruction's arguments");
    }
    has_macro_arg = has_macro_arg || def_args[i]->type == ArgType::kMacro;
  }
  if (has_macro_arg != !instruction.arg_macro_name.empty()) {
    return InstructionDefError(instruction, "Source \"", instruction.source,
                               "\" must have a $m exactly when the instruction "
                               "calls a macro");
  }
  return true;
}

void InstructionSetSyntaxBuilder::AddForm(const InstructionDef& instruction,
                                          const SourcePattern& pattern,
                                          const MacroSyntax* macro,
                                          int code_index) {
  syntax_->forms_.push_back(InstructionForm());
  InstructionForm& form = syntax_->forms_.back();
  form.instruction_ = &instruction;
  form.pattern_ = std::string(instruction.source);
  form.size_ = 1 + pattern.GetValueWords();

  // Arguments in the code word are in the order of the instruction's
  // placeholders, the first in the low bits.
  uint16_t code_word_args[2] = {0, 0};
  form.code_word_mask_ = 0xFF00;
  int arg_index = 0;
  int offset_change = 0;  // How far the macro code moved the parts after it.
  for (Part part : pattern.GetParts()) {
    part.offset += offset_change;
    const int shift = (arg_index == 0 ? 0 : instruction.arg1.size);
    if (part.kind == Part::Kind::kArgument) {
      ++arg_index;
    }
    if (part.kind != Part::Kind::kArgument ||
        part.arg.type != ArgType::kMacro) {
      AddPart(form, instruction, std::move(part), shift);
      continue;
    }

    // The macro code's source and parts take the place of the $m. Its parts
    // are whole arguments of the form only if the $m is.
    const MacroCodeDef& code = macro->def->code[code_index];
    const SourcePattern& code_pattern = macro->patterns[code_index];
    form.macro_code_ = &code;
    form.pattern_.replace(part.offset, part.size, code.source);
    offset_change += static_cast<int>(code.source.size()) - part.size;
    form.size_ += code_pattern.GetValueWords();
    for (Part code_part : code_pattern.GetParts()) {
      code_part.offset += part.offset;
      code_part.whole_arg = code_part.whole_arg && part.whole_arg;
      AddPart(form, instruction, std::move(code_part), shift);
    }
    code_word_args[arg_index - 1] = macro->def->Encode(code_index, 0);
    form.code_word_mask_ |= ((1 << code.prefix.size) - 1)
                            << (shift + code.arg.size);
  }
  form.code_word_ = instruction.Encode(code_word_args[0], code_word_args[1]);
}

// Adds a part to a form, with its argument if it is a placeholder. A code word
// argument's bits start at `shift`.
void InstructionSetSyntaxBuilder::AddPart(InstructionForm& form,
                                          const InstructionDef& instruction,
                                          Part part, int shift) {
  if (part.kind == Part::Kind::kArgument) {
    form.args_.push_back({.kind = GetArgKind(part),
                          .bits = part.arg.size,
                          .shift = shift,
                          .value_set = part.value_set});
  } else if (part.kind != Part::Kind::kLiteral) {
    form.args_.push_back({.kind = GetArgKind(part),
                          .relative = instruction.relative &&
                                      part.kind == Part::Kind::kWord &&
                                      part.whole_arg});
  }
  form.parts_.push_back(std::move(part));
}

// Programs size each form of an instruction from its pattern, so it must be
// the size its microcode decodes to.
bool InstructionSetSyntaxBuilder::CheckSize(
    const InstructionForm& form, const InstructionSet& instruction_set) {
  const InstructionDef& instruction = form.GetInstruction();
  DecodedInstruction decoded;
  if (!instruction_set.Decode(form.GetCodeWord(), decoded)) {
    return InstructionDefError(instruction, "No form of ", instruction.op_name,
                               " in the instruction set for code word ",
                               form.GetCodeWord());
  }
  if (decoded.size == form.GetSize()) {
    return true;
  }
  return InstructionDefError(
      instruction, GetSourceName(form), " has size ", form.GetSize(),
      ", but the microcode has size ", static_cast<int>(decoded.size),
      " (the code word, and a word for each LD in the fetch phase)");
}

// Indexes the forms, which no longer move.
void InstructionSetSyntaxBuilder::IndexForms() {
  for (const InstructionForm& form : syntax_->forms_) {
    const InstructionDef& instruction = form.GetInstruction();

    // Programs write instruction names in any case.
    std::string name = absl::AsciiStrToUpper(instruction.op_name);
    if (auto dot = name.find('.'); dot != std::string::npos) {
      syntax_->forms_by_name_[name.substr(0, dot)].push_back(&form);
    }
    syntax_->forms_by_name_[std::move(name)].push_back(&form);
    syntax_->forms_by_op_[instruction.op].push_back(&form);
  }
}

//==============================================================================
// InstructionSetSyntax
//==============================================================================

std::unique_ptr<InstructionSetSyntax> InstructionSetSyntax::Create(
    const InstructionSetDef& def, const InstructionSet* instruction_set,
    InstructionError* error) {
  return InstructionSetSyntaxBuilder(&def, error).Build(instruction_set);
}

absl::Span<const InstructionForm* const> InstructionSetSyntax::GetForms(
    std::string_view name) const {
  auto it = forms_by_name_.find(absl::AsciiStrToUpper(name));
  if (it == forms_by_name_.end()) {
    return {};
  }
  return it->second;
}

const InstructionForm* InstructionSetSyntax::FindForm(
    uint16_t code_word) const {
  for (const InstructionForm* form : forms_by_op_[code_word >> 8]) {
    if ((code_word & form->GetCodeWordMask()) == form->GetCodeWord()) {
      return form;
    }
  }
  return nullptr;
}

}  // namespace oz3
