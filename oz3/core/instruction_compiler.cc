// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_compiler.h"

#include <algorithm>
#include <optional>
#include <string>

#include "absl/container/flat_hash_map.h"
#include "absl/log/log.h"
#include "absl/strings/ascii.h"
#include "absl/strings/numbers.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_split.h"
#include "absl/types/span.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/port.h"

namespace oz3 {

namespace {

// Instruction word reg arguments.
static_assert(CpuCore::kMinWordArgReg ==
              -static_cast<uint8_t>(ABSL_ARRAYSIZE(DecodedInstruction::r)));

// Instruction dword reg arguments.
static_assert(CpuCore::B == CpuCore::A - 1,
              "Required offset to convert macro args");

constexpr int kFirstRegisterLock = 10;
constexpr int kFirstPortLock = 100;
constexpr int kFirstCoreLock = 200;
constexpr int kMaxSubInstructions =
    static_cast<int>(
        std::numeric_limits<
            decltype(microcode_internal::Instruction::sub_index)>::max()) +
    1;
const Argument kNoArgument;

std::string LockName(int lock) {
  switch (lock) {
    case -1:
      return "NONE";
    case CpuCore::CODE:
      return "CODE";
    case CpuCore::STACK:
      return "STACK";
    case CpuCore::DATA:
      return "DATA";
    case CpuCore::EXTRA:
      return "EXTRA";
  }
  if (lock >= kFirstCoreLock) {
    return absl::StrCat("Core section ", lock - kFirstCoreLock + 1);
  }
  if (lock >= kFirstPortLock) {
    return absl::StrCat("Port section ", lock - kFirstPortLock + 1);
  }
  return "UNKNOWN";
}

absl::string_view LockOpNameFromIndex(int lock) {
  if (lock < kFirstRegisterLock) {
    return "LK";
  }
  if (lock < kFirstPortLock) {
    return "LKR";
  }
  if (lock < kFirstCoreLock) {
    return "PLK";
  }
  return "CLK";
}

std::string ToBitString(int value, int count) {
  std::string result;
  for (int i = 1 << (count - 1); i > 0; i >>= 1) {
    result.push_back((value & i) ? '1' : '0');
  }
  return result;
}

}  // namespace

class InstructionCompiler {
 public:
  InstructionCompiler(absl::Span<const MicrocodeDef> microcode_defs)
      : microcode_defs_(microcode_defs) {}

  bool CompileMacro(const MacroDef& macro_def, InstructionError* error);
  bool CompileInstruction(const InstructionDef& instruction_def,
                          InstructionError* error);

  InstructionSet ToInstructionSet() && {
    return InstructionSet(std::move(instructions_),
                          std::move(sub_instructions_), std::move(microcodes_));
  }

 private:
  struct SubMacro {
    uint16_t code_start = 0;
    uint16_t code_size = 0;
    Argument arg;
    int8_t ret = 0;  // Return register index (or -1 for arg).
  };

  struct Macro {
    uint16_t sub_index = 0;
    uint16_t bit_size = 0;  // Number of bits that define the macro
    ArgType param = ArgType::kNone;
    ArgType ret = ArgType::kNone;
  };

  struct ParsedMicrocode {
    std::string_view code;       // Full code that was parsed.
    std::string_view op_name;    // Operation name.
    std::string_view arg1_name;  // First argument name.
    std::string_view arg2_name;  // Second argument name.
  };

  enum class LockType { kNone, kMemory, kPort, kCore };

  using MacrosMap = absl::flat_hash_map<std::string, Macro>;

  using Instruction = microcode_internal::Instruction;
  using SubInstruction = microcode_internal::SubInstruction;
  using InstructionCode = microcode_internal::InstructionCode;

  template <typename... Args>
  bool Error(Args&&... args) {
    if (error_ == nullptr) {
      return false;
    }
    std::string message = absl::StrCat(std::forward<Args>(args)...);
    std::string context;
    if (instruction_def_ != nullptr) {
      error_->def = InstructionError::Def::kInstruction;
      error_->def_name = instruction_def_->op_name;
      context = absl::StrCat(" in ", instruction_def_->op_name);
    } else if (macro_def_ != nullptr) {
      error_->def = InstructionError::Def::kMacro;
      error_->def_name = macro_def_->name;
      context = absl::StrCat(" in $", macro_def_->name);
      if (state_.macro_code_def != nullptr &&
          !state_.macro_code_def->source.empty()) {
        error_->macro_source_name = state_.macro_code_def->source;
        absl::StrAppend(&context, " ", state_.macro_code_def->source);
      }
    }
    if (state_.parsed != nullptr) {
      DCHECK(state_.code_index >= 0);
      absl::StrAppend(&context, " for \"", state_.parsed->code, "\" (",
                      state_.code_index);
      if (state_.sub_code_index >= 0) {
        absl::StrAppend(&context, "/", state_.sub_code_index);
      }
      absl::StrAppend(&context, ")");
    }
    error_->code_index = state_.code_index;
    error_->sub_code_index = state_.sub_code_index;
    error_->message = absl::StrCat("Error", context, ": ", message);
    return false;
  }

  bool ExtractLabels();
  ParsedMicrocode ParseMicrocode(std::string_view src_code);
  bool InitLocks();
  const MicrocodeDef* FindMicrocodeDef(std::string_view op_name);
  const MicrocodeDef* FindMicrocodeDef(int op);
  bool CompileSubInstruction();
  bool CompileInstructionVariant(int macro_index = 0, int macro_size = 0);
  bool CompileMicrocode();
  bool CompileMicroArg(std::string_view arg_name, MicroArgType arg_type,
                       int8_t& arg);
  bool ValidateMacroReturnRegister(int8_t ret);
  bool ValidateMicrocode(int index);
  bool ValidateMicroArg(int index, MicroArgType arg_type, int8_t arg);

  // Initial state.
  absl::Span<const MicrocodeDef> microcode_defs_;

  // Persistent state.
  std::vector<Instruction> instructions_;
  std::vector<SubInstruction> sub_instructions_;
  std::vector<Microcode> microcodes_;

  MacrosMap macros_;
  std::vector<SubMacro> sub_macros_;
  std::vector<Microcode> macro_codes_;
  std::vector<ParsedMicrocode> parsed_macro_codes_;

  // Temporary state during compilation.
  InstructionError* error_ = nullptr;
  const MacroDef* macro_def_ = nullptr;
  const InstructionDef* instruction_def_ = nullptr;
  int last_macro_code_start_ = -1;
  const SubInstruction* last_sub_instruction_ = nullptr;
  struct State {
    const MacroCodeDef* macro_code_def = nullptr;
    const SubMacro* sub_macro = nullptr;
    SubInstruction* sub_instruction = nullptr;
    const Argument* arg1 = &kNoArgument;
    const Argument* arg2 = &kNoArgument;
    const Argument* argm = &kNoArgument;
    int8_t macro_param = 0;
    std::optional<ArgType> macro_return_type;
    std::vector<std::string_view> src_code;
    std::vector<ParsedMicrocode> parsed_code;
    std::vector<ParsedMicrocode> pre_parsed;
    std::vector<ParsedMicrocode> post_parsed;
    std::vector<Microcode> microcodes;
    std::vector<Microcode> pre_codes;
    std::vector<Microcode> post_codes;
    absl::flat_hash_map<std::string, int> labels;
    const ParsedMicrocode* parsed = nullptr;
    int code_index = -1;
    int sub_code_index = -1;
    std::vector<int> locks;
    InstructionCode* code = nullptr;
    Microcode* microcode = nullptr;

    // Set by last LK, PLK, or CLK operation. Cleared by UL, PUL, and CUL.
    LockType lock_type_ = LockType::kMemory;

    // True if an ADR operation has been seen. Cleared by UL.
    bool has_adr_ = true;

    // True if still in the fetch phase cleared by UL and ADR.
    bool in_fetch_ = true;

    // True if an LK operation has been seen.
    bool had_lk_ = false;
  } state_;
};

bool InstructionCompiler::CompileMacro(const MacroDef& macro_def,
                                       InstructionError* error) {
  instruction_def_ = nullptr;
  macro_def_ = &macro_def;
  error_ = error;
  if (error_ != nullptr) {
    *error_ = {};
  }

  if (macro_def.name.empty()) {
    return Error("Macro name missing");
  }
  auto pos = macro_def.name.find_first_not_of(
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789");
  if (pos != std::string::npos) {
    return Error("Invalid character '", macro_def.name.substr(pos, 1),
                 "' in macro name");
  }

  if (macro_def.size < 1 || macro_def.size > 8) {
    return Error("Macro size must be between 1 and 8. Size is ",
                 macro_def.size);
  }
  int sub_count = (1 << macro_def.size);

  Macro& macro = macros_[absl::StrCat("$", macro_def.name)];
  macro.bit_size = static_cast<uint8_t>(macro_def.size);
  macro.param = macro_def.param;
  macro.ret = macro_def.ret;
  macro.sub_index = static_cast<uint16_t>(sub_macros_.size());
  sub_macros_.resize(sub_macros_.size() + sub_count);

  if (macro.param != ArgType::kNone && macro.param != ArgType::kWordReg &&
      macro.param != ArgType::kDwordReg) {
    return Error("Invalid macro parameter type: ",
                 ArgTypeToString(macro.param));
  }
  if (macro.ret != ArgType::kNone && macro.ret != ArgType::kWordReg &&
      macro.ret != ArgType::kDwordReg) {
    return Error("Invalid macro return type: ", ArgTypeToString(macro.ret));
  }

  std::vector<const MacroCodeDef*> sub_code_defs(sub_count, nullptr);
  for (const MacroCodeDef& code_def : macro_def.code) {
    state_ = {};
    state_.macro_code_def = &code_def;
    if (code_def.arg.size + code_def.prefix.size != macro_def.size) {
      return Error("Prefix + argument size ",
                   code_def.arg.size + code_def.prefix.size,
                   " does not match macro size ", macro_def.size);
    }
    const int max_prefix_value = (1 << code_def.prefix.size) - 1;
    if (code_def.prefix.value > max_prefix_value) {
      return Error("Prefix value too large: ", code_def.prefix.value, " > ",
                   max_prefix_value);
    }

    const int num_sub_macros = (1 << code_def.arg.size);
    const int start_sub_index =
        macro.sub_index + (code_def.prefix.value << code_def.arg.size);
    for (int i = start_sub_index; i < start_sub_index + num_sub_macros; ++i) {
      const int sub_index = i - macro.sub_index;
      if (sub_code_defs[sub_index] != nullptr) {
        return Error("Duplicate macro sub code ",
                     ToBitString(sub_index, macro_def.size), " between \"",
                     sub_code_defs[sub_index]->source, "\" and \"",
                     code_def.source, "\"");
      }
      sub_code_defs[sub_index] = &code_def;
    }

    if (!code_def.arg.IsValid()) {
      return Error(
          "Invalid macro argument (size is probably invalid for type)");
    }
    if (!ValidateMacroReturnRegister(code_def.ret)) {
      return false;
    }
    SubMacro sub_macro = {.arg = code_def.arg, .ret = code_def.ret};
    state_.argm = &sub_macro.arg;

    state_.src_code =
        absl::StrSplit(code_def.code, ';', absl::SkipWhitespace());
    if (state_.src_code.empty()) {
      return Error("No microcode");
    }
    if (!ExtractLabels()) {
      return false;
    }
    state_.parsed_code.reserve(state_.src_code.size());
    for (std::string_view& src_code : state_.src_code) {
      state_.parsed_code.push_back(ParseMicrocode(src_code));
    }
    sub_macro.code_start = static_cast<uint16_t>(macro_codes_.size());
    state_.code_index = 0;
    for (const ParsedMicrocode& parsed : state_.parsed_code) {
      state_.parsed = &parsed;
      state_.microcode = &macro_codes_.emplace_back();
      parsed_macro_codes_.emplace_back(parsed);
      if (macro_codes_.size() - sub_macro.code_start >
          kMaxInstructionMicrocodes) {
        return Error("Too much microcode");
      }
      if (!CompileMicrocode()) {
        return false;
      }
      ++state_.code_index;
    }
    state_.parsed = nullptr;
    state_.code_index = -1;
    DCHECK(macro_codes_.size() - sub_macro.code_start <=
           kMaxInstructionMicrocodes);
    sub_macro.code_size =
        static_cast<uint8_t>(macro_codes_.size() - sub_macro.code_start);

    for (int i = 0; i < num_sub_macros; ++i) {
      sub_macros_[start_sub_index + i] = sub_macro;
    }
  }

  return true;
}

bool InstructionCompiler::CompileInstruction(
    const InstructionDef& instruction_def, InstructionError* error) {
  macro_def_ = nullptr;
  instruction_def_ = &instruction_def;
  error_ = error;
  if (error_ != nullptr) {
    *error_ = {};
  }
  state_ = {};

  int op_index = static_cast<int>(instruction_def_->op);
  if (instructions_.size() <= op_index) {
    instructions_.resize(op_index + 1);
  }
  Instruction& instruction = instructions_[op_index];
  instruction.arg1 = instruction_def_->arg1;
  instruction.arg2 = instruction_def_->arg2;
  state_.arg1 = &instruction.arg1;
  state_.arg2 = &instruction.arg2;

  if (!instruction.arg1.IsValid()) {
    return Error("Invalid first argument (size is probably invalid for type)");
  }
  if (!instruction.arg2.IsValid()) {
    return Error("Invalid second argument (size is probably invalid for type)");
  }

  state_.src_code =
      absl::StrSplit(instruction_def_->code, ';', absl::SkipWhitespace());
  if (!ExtractLabels()) {
    return false;
  }
  state_.parsed_code.reserve(state_.src_code.size());
  for (std::string_view& src_code : state_.src_code) {
    state_.parsed_code.push_back(ParseMicrocode(src_code));
  }
  MacrosMap::const_iterator macro_it;
  int macro_pos = -1;
  const ParsedMicrocode* macro_parsed = nullptr;
  state_.microcodes.resize(state_.parsed_code.size());
  state_.microcode = state_.microcodes.data();
  state_.code_index = 0;
  for (const ParsedMicrocode& parsed : state_.parsed_code) {
    state_.parsed = &parsed;
    if (parsed.op_name.starts_with('$')) {
      if (macro_pos >= 0) {
        return Error("Multiple macros in instruction");
      }
      macro_parsed = &parsed;
      macro_pos = state_.code_index;
      macro_it = macros_.find(macro_parsed->op_name);
      if (macro_it == macros_.end()) {
        return Error("Unknown macro");
      }
      state_.macro_return_type = macro_it->second.ret;
    } else {
      if (state_.microcodes.size() > kMaxInstructionMicrocodes) {
        return Error("Too much compiled microcode");
      }
      if (!CompileMicrocode()) {
        return false;
      }
    }
    ++state_.microcode;
    ++state_.code_index;
  }
  state_.parsed = nullptr;
  state_.code_index = -1;

  if (macro_parsed == nullptr) {
    if (instruction.arg1.type == ArgType::kMacro ||
        instruction.arg2.type == ArgType::kMacro) {
      return Error("Macro argument specified without macro");
    }
    state_.code = &instruction.code;
    return CompileInstructionVariant();
  }

  if (instruction.arg1.type == ArgType::kMacro &&
      instruction.arg2.type == ArgType::kMacro) {
    return Error("Too many instruction macro arguments");
  }

  state_.parsed = macro_parsed;
  state_.code_index = macro_pos;
  const Macro& macro = macro_it->second;

  if (instruction.arg1.type != ArgType::kMacro &&
      instruction.arg2.type != ArgType::kMacro) {
    return Error("Macro called without instruction macro argument");
  }

  if (!macro_parsed->arg1_name.empty()) {
    if (macro.param == ArgType::kNone) {
      return Error("Macro does not take a first argument");
    }
    if (!CompileMicroArg(macro_parsed->arg1_name, ToMicroArgType(macro.param),
                         state_.macro_param)) {
      return false;
    }
  } else if (macro.param != ArgType::kNone) {
    return Error("Macro requires a first argument of type: ",
                 ArgTypeToString(macro.param));
  }
  if (!macro_parsed->arg2_name.empty()) {
    return Error("Macro does not take a second argument");
  }

  const int macro_size =
      ((instruction.arg1.type == ArgType::kMacro) ? instruction.arg1.size
                                                  : instruction.arg2.size);
  if (macro_size != macro.bit_size) {
    return Error("Macro size ", macro.bit_size,
                 " does not macro argument size ", macro_size);
  }

  state_.pre_codes.assign(state_.microcodes.begin(),
                          state_.microcodes.begin() + macro_pos);
  state_.post_codes.assign(state_.microcodes.begin() + macro_pos + 1,
                           state_.microcodes.end());
  state_.microcodes.clear();

  state_.pre_parsed.assign(state_.parsed_code.begin(),
                           state_.parsed_code.begin() + macro_pos);
  state_.post_parsed.assign(state_.parsed_code.begin() + macro_pos + 1,
                            state_.parsed_code.end());
  state_.parsed_code.clear();

  const int sub_count = (1 << macro_size);
  instruction.sub_index =
      static_cast<decltype(instruction.sub_index)>(sub_instructions_.size());
  sub_instructions_.resize(sub_instructions_.size() + sub_count);
  DCHECK(sub_instructions_.size() <= kMaxSubInstructions);

  const State start_state = state_;
  for (int i = 0; i < sub_count; ++i) {
    state_.sub_macro = &sub_macros_[macro.sub_index + i];
    state_.sub_instruction = &sub_instructions_[instruction.sub_index + i];
    if (!CompileSubInstruction()) {
      return false;
    }
    state_ = start_state;
  }
  return true;
}

bool InstructionCompiler::ExtractLabels() {
  int index = 0;
  for (std::string_view& code : state_.src_code) {
    if (code.starts_with('@')) {
      auto label_end = code.find(':', 1);
      if (label_end == std::string_view::npos) {
        return Error("Expected ':' after @ label: ", code);
      }
      std::string_view label = code.substr(1, label_end - 1);
      code.remove_prefix(label_end + 1);
      for (char c : label) {
        if (!absl::ascii_isalnum(c) && c != '_') {
          return Error("Invalid label: ", label);
        }
      }
      std::string label_key = absl::AsciiStrToUpper(label);
      if (state_.labels.contains(label_key)) {
        return Error("Duplicate label: ", label);
      }
      state_.labels[label_key] = index;
    }
    ++index;
  }
  return true;
}

bool InstructionCompiler::InitLocks() {
  state_.locks.reserve(state_.parsed_code.size());
  static_assert(
      CpuCore::CODE < kFirstPortLock && CpuCore::STACK < kFirstPortLock &&
          CpuCore::DATA < kFirstPortLock && CpuCore::EXTRA < kFirstPortLock,
      "Memory locks must be lower that first port lock");
  int port_lock = kFirstPortLock;
  int core_lock = kFirstCoreLock;
  int lock = CpuCore::CODE;
  int lock_count = 0;
  const int end = static_cast<int>(state_.microcodes.size());
  state_.microcode = state_.microcodes.data();
  for (int index = 0; index < end; ++index) {
    state_.locks.push_back(lock);
    switch (state_.microcode->op) {
      case kMicro_LK: {
        if (lock >= 0) {
          return Error("LK when prior ", LockOpNameFromIndex(lock),
                       " is still locked.");
        }
        lock = state_.microcode->arg1;
        ++lock_count;
      } break;
      case kMicro_LKR: {
        if (lock >= 0) {
          return Error("LKR when prior ", LockOpNameFromIndex(lock),
                       " is still locked.");
        }
        DCHECK(state_.microcode->arg1 >= -10);
        lock = kFirstRegisterLock + state_.microcode->arg1 + 10;
        DCHECK(lock >= kFirstRegisterLock);
        ++lock_count;
      } break;
      case kMicro_UL: {
        if (lock < 0 || lock >= kFirstPortLock) {
          return Error("UL without a prior LK or LKR.");
        }
        lock = -1;
      } break;
      case kMicro_PLK: {
        if (lock >= 0) {
          return Error("PLK when prior ", LockOpNameFromIndex(lock),
                       " is still locked.");
        }
        lock = port_lock++;
        ++lock_count;
      } break;
      case kMicro_PUL: {
        if (lock < kFirstPortLock) {
          return Error("PUL without a prior PLK.");
        }
        lock = -1;
      } break;
      case kMicro_CLK: {
        if (lock >= 0) {
          return Error("CLK when prior ", LockOpNameFromIndex(lock),
                       " is still locked.");
        }
        lock = core_lock++;
        ++lock_count;
      } break;
      case kMicro_CUL: {
        if (lock < kFirstCoreLock) {
          return Error("CUL without a prior CLK.");
        }
        lock = -1;
      } break;
    }
    if (lock_count > kMaxLocksPerInstruction) {
      return Error("Too many LK/UL, PLK/PUL, CLK/CUL pairs (max ",
                   kMaxLocksPerInstruction, ")");
    }
    ++state_.microcode;
  }
  // We need a value for one past the end, since it is a valid jump address.
  state_.locks.push_back(lock);
  return true;
}

InstructionCompiler::ParsedMicrocode InstructionCompiler::ParseMicrocode(
    std::string_view code) {
  auto RemovePrefix = [&code](std::string_view::size_type amount) {
    code.remove_prefix(std::min(code.size(), amount));
  };

  ParsedMicrocode parsed = {};
  parsed.code = code;
  auto next_token = code.find_first_of("(");
  parsed.op_name = code.substr(0, next_token);
  RemovePrefix(next_token);
  if (!code.empty() && code[0] == '(') {
    RemovePrefix(1);
    next_token = code.find_first_of(",)");
    parsed.arg1_name = code.substr(0, next_token);
    RemovePrefix(next_token);
    if (!code.empty() && code[0] == ',') {
      RemovePrefix(1);
      next_token = code.find(")");
      parsed.arg2_name = code.substr(0, next_token);
      RemovePrefix(next_token);
    }
  }
  return parsed;
}

const MicrocodeDef* InstructionCompiler::FindMicrocodeDef(
    std::string_view op_name) {
  for (const MicrocodeDef& def : microcode_defs_) {
    if (def.op_name == op_name) {
      state_.microcode->op = def.op;
      return &def;
    }
  }
  return nullptr;
}

const MicrocodeDef* InstructionCompiler::FindMicrocodeDef(int op) {
  for (const MicrocodeDef& def : microcode_defs_) {
    if (def.op == op) {
      return &def;
    }
  }
  return nullptr;
}

bool InstructionCompiler::CompileSubInstruction() {
  // If there was no sub-macro code, then this is not a valid sub-instruction.
  // We simply don't store any code, and it will automatically become a NOP if
  // it ever gets decoded.
  if (state_.sub_macro->code_size == 0) {
    return true;
  }

  // Macros with arguments generate many sub instructions with the same code.
  // We can just reuse the same code, since the instruction didn't change
  // either.
  if (last_macro_code_start_ == state_.sub_macro->code_start) {
    *state_.sub_instruction = *last_sub_instruction_;
    return true;
  }
  last_macro_code_start_ = state_.sub_macro->code_start;
  last_sub_instruction_ = state_.sub_instruction;

  const int pre_size = static_cast<int>(state_.pre_codes.size());
  const int post_size = static_cast<int>(state_.post_codes.size());
  const int macro_code_start = state_.sub_macro->code_start;
  const int macro_code_end = macro_code_start + state_.sub_macro->code_size;
  const int macro_code_size = state_.sub_macro->code_size;
  const int macro_size_increase = macro_code_size - 1;

  const int8_t arg_offset =
      (instruction_def_->arg1.type == ArgType::kMacro ? 0 : 1);

  const int8_t macro_param0 = state_.macro_param;
  const int8_t macro_param1 =
      (macro_param0 >= 0 ? macro_param0 + 1 : macro_param0 - 2);

  int8_t macro_return0 = state_.sub_macro->ret;
  if (macro_return0 == CpuCore::MP0) {
    macro_return0 = macro_param0;
  } else if (macro_return0 == CpuCore::MP1) {
    macro_return0 = macro_param1;
  }
  int8_t macro_return1 = state_.sub_macro->ret;
  if (macro_return1 >= 0) {
    macro_return1 += 1;
  } else if (macro_return1 == CpuCore::MP) {
    macro_return1 = macro_param1;
  }

  state_.microcodes.assign(state_.pre_codes.begin(), state_.pre_codes.end());
  state_.microcodes.insert(state_.microcodes.end(),
                           macro_codes_.begin() + macro_code_start,
                           macro_codes_.begin() + macro_code_end);
  state_.microcodes.insert(state_.microcodes.end(), state_.post_codes.begin(),
                           state_.post_codes.end());

  state_.parsed_code.assign(state_.pre_parsed.begin(), state_.pre_parsed.end());
  state_.parsed_code.insert(state_.parsed_code.end(),
                            parsed_macro_codes_.begin() + macro_code_start,
                            parsed_macro_codes_.begin() + macro_code_end);
  state_.parsed_code.insert(state_.parsed_code.end(),
                            state_.post_parsed.begin(),
                            state_.post_parsed.end());

  state_.code_index = 0;
  for (int k = 0; k < pre_size; ++k, ++state_.code_index) {
    state_.parsed = &state_.parsed_code[k];
    state_.microcode = &state_.microcodes[k];
    const MicrocodeDef* def = FindMicrocodeDef(state_.microcode->op);
    if (def->arg1 == MicroArgType::kAddress &&
        k + state_.microcode->arg1 >= pre_size) {
      if (macro_size_increase + state_.microcode->arg1 > 127) {
        return Error("Macro size increase too large for relative address");
      }
      state_.microcode->arg1 += macro_size_increase;
      DCHECK(state_.microcode->arg1 > 0);
    }
    if (def->arg2 == MicroArgType::kAddress &&
        k + state_.microcode->arg2 >= pre_size) {
      if (macro_size_increase + state_.microcode->arg2 > 127) {
        return Error("Macro size increase too large for relative address");
      }
      state_.microcode->arg2 += macro_size_increase;
      DCHECK(state_.microcode->arg2 > 0);
    }
  }
  for (int k = 0; k < macro_code_size; ++k) {
    const int index = pre_size + k;
    state_.parsed = &state_.parsed_code[index];
    state_.microcode = &state_.microcodes[index];
    const MicrocodeDef* def = FindMicrocodeDef(state_.microcode->op);
    if (((def->arg1 == MicroArgType::kWordReg) ||
         (def->arg1 == MicroArgType::kDwordReg)) &&
        state_.microcode->arg1 < CpuCore::kMinArgReg) {
      int8_t& arg1 = state_.microcode->arg1;
      if (arg1 == CpuCore::MI) {
        arg1 = CpuCore::C0 + arg_offset;
      } else if (arg1 == CpuCore::MP0) {  // Also matches CpuCore::MP
        arg1 = macro_param0;
      } else if (arg1 == CpuCore::MP1) {
        arg1 = macro_param1;
      } else if (arg1 == CpuCore::MM0) {
        arg1 = CpuCore::A0 - arg_offset;
      } else if (arg1 == CpuCore::MM1) {
        arg1 = CpuCore::A1 - arg_offset;
      } else {
        LOG(DFATAL) << "Unhandled register: " << arg1;
      }
    }
    if (((def->arg2 == MicroArgType::kWordReg) ||
         (def->arg2 == MicroArgType::kDwordReg)) &&
        state_.microcode->arg2 < CpuCore::kMinArgReg) {
      int8_t& arg2 = state_.microcode->arg2;
      if (arg2 == CpuCore::MI) {
        arg2 = CpuCore::C0 + arg_offset;
      } else if (arg2 == CpuCore::MP0) {  // Also matches CpuCore::MP
        arg2 = macro_param0;
      } else if (arg2 == CpuCore::MP1) {
        arg2 = macro_param1;
      } else if (arg2 == CpuCore::MM0) {
        arg2 = CpuCore::A0 - arg_offset;
      } else if (arg2 == CpuCore::MM1) {
        arg2 = CpuCore::A1 - arg_offset;
      } else {
        LOG(DFATAL) << "Unhandled register: " << arg2;
      }
    }
  }
  ++state_.code_index;
  for (int k = 0; k < post_size; ++k, ++state_.code_index) {
    const int index = pre_size + macro_size_increase + 1 + k;
    state_.parsed = &state_.parsed_code[index];
    state_.microcode = &state_.microcodes[index];
    const MicrocodeDef* def = FindMicrocodeDef(state_.microcode->op);
    if (def->arg1 == MicroArgType::kAddress && k + state_.microcode->arg1 < 0) {
      if (state_.microcode->arg1 - macro_size_increase < -128) {
        return Error("Macro size increase too large for relative address");
      }
      state_.microcode->arg1 -= macro_size_increase;
      DCHECK(state_.microcode->arg1 < 0);
    }
    if (def->arg2 == MicroArgType::kAddress && k + state_.microcode->arg2 < 0) {
      if (state_.microcode->arg2 - macro_size_increase < -128) {
        return Error("Macro size increase too large for relative address");
      }
      state_.microcode->arg2 -= macro_size_increase;
      DCHECK(state_.microcode->arg2 < 0);
    }
    if (((def->arg1 == MicroArgType::kWordReg) ||
         (def->arg1 == MicroArgType::kDwordReg)) &&
        state_.microcode->arg1 < CpuCore::kMinArgReg) {
      int8_t& arg1 = state_.microcode->arg1;
      if (arg1 == CpuCore::MR0) {  // Also matches CpuCore::MR
        arg1 = macro_return0;
      } else if (arg1 == CpuCore::MR1) {
        arg1 = macro_return1;
      }
    }
    if (((def->arg2 == MicroArgType::kWordReg) ||
         (def->arg2 == MicroArgType::kDwordReg)) &&
        state_.microcode->arg2 < CpuCore::kMinArgReg) {
      int8_t& arg2 = state_.microcode->arg2;
      if (arg2 == CpuCore::MR0) {  // Also matches CpuCore::MR
        arg2 = macro_return0;
      } else if (arg2 == CpuCore::MR1) {
        arg2 = macro_return1;
      }
    }
  }

  state_.parsed = nullptr;
  state_.code_index = -1;

  state_.sub_instruction->arg = state_.sub_macro->arg;
  state_.code = &state_.sub_instruction->code;
  return CompileInstructionVariant(pre_size, macro_code_size);
}

bool InstructionCompiler::CompileInstructionVariant(int macro_index,
                                                    int macro_size) {
  if (state_.microcodes.size() > kMaxInstructionMicrocodes) {
    return Error("Too much microcode: ", state_.microcodes.size(), " > ",
                 kMaxInstructionMicrocodes);
  }

  state_.code->pc_size = 1;
  state_.code->code_start = static_cast<uint16_t>(microcodes_.size());
  microcodes_.insert(microcodes_.end(), state_.microcodes.begin(),
                     state_.microcodes.end());
  state_.code->code_size = static_cast<uint8_t>(state_.microcodes.size());

  if (!InitLocks()) {
    return false;
  }
  int start = state_.code->code_start;
  int index = 0;
  for (const ParsedMicrocode& parsed : state_.parsed_code) {
    if (macro_size == 0 || index < macro_index) {
      state_.code_index = index;
    } else if (index == macro_index) {
      state_.code_index = index;
      state_.sub_code_index = 0;
    } else if (index > macro_index && index < macro_index + macro_size) {
      state_.sub_code_index = index - macro_index;
    } else {
      state_.code_index = index - macro_size + 1;
      state_.sub_code_index = -1;
    }
    state_.parsed = &parsed;
    state_.microcode = &microcodes_[start + index];
    if (!ValidateMicrocode(index)) {
      return false;
    }
    ++index;
  }
  state_.parsed = nullptr;
  state_.code_index = -1;
  state_.sub_code_index = -1;
  if (state_.lock_type_ == LockType::kMemory) {
    if (state_.had_lk_) {
      return Error("LK or LKR not cleared by UL");
    } else {
      return Error("No UL for instruction fetch");
    }
  } else if (state_.lock_type_ == LockType::kPort) {
    return Error("PLK not cleared by PUL");
  } else if (state_.lock_type_ == LockType::kCore) {
    return Error("CLK not cleared by CUL");
  }
  return true;
}

bool InstructionCompiler::CompileMicrocode() {
  const MicrocodeDef* def = FindMicrocodeDef(state_.parsed->op_name);
  if (def == nullptr) {
    return Error("Microcode OpCode not found");
  }
  if (!CompileMicroArg(state_.parsed->arg1_name, def->arg1,
                       state_.microcode->arg1)) {
    return false;
  }
  if (!CompileMicroArg(state_.parsed->arg2_name, def->arg2,
                       state_.microcode->arg2)) {
    return false;
  }
  return true;
}

bool InstructionCompiler::CompileMicroArg(std::string_view arg_name,
                                          MicroArgType arg_type, int8_t& arg) {
  if (arg_name.empty() && arg_type == MicroArgType::kNone) {
    return true;
  }
  if (arg_name.empty()) {
    return Error("Argument missing");
  }
  switch (arg_type) {
    case MicroArgType::kNone: {
      return Error("Argument not allowed");
    } break;
    case MicroArgType::kBank: {
      if (arg_name == "CODE") {
        arg = CpuCore::CODE;
      } else if (arg_name == "STACK") {
        arg = CpuCore::STACK;
      } else if (arg_name == "DATA") {
        arg = CpuCore::DATA;
      } else if (arg_name == "EXTRA") {
        arg = CpuCore::EXTRA;
      } else {
        return Error("Invalid bank: ", arg_name);
      }
      return true;
    } break;
    case MicroArgType::kStatus: {
      for (char c : arg_name) {
        if (c == 'Z') {
          arg |= CpuCore::Z;
        } else if (c == 'S') {
          arg |= CpuCore::S;
        } else if (c == 'C') {
          arg |= CpuCore::C;
        } else if (c == 'O') {
          arg |= CpuCore::O;
        } else if (c == 'I') {
          arg |= CpuCore::I;
        } else if (c != '_') {
          return Error("Invalid status flags: ", arg_name);
        }
      }
      return true;
    } break;
    case MicroArgType::kPortMode: {
      for (char c : arg_name) {
        if (c == 'T') {
          arg |= Port::T;
        } else if (c == 'S') {
          arg |= Port::S;
        } else if (c == 'A') {
          arg |= Port::A;
        } else if (c != '_') {
          return Error("Invalid port mode: ", arg_name);
        }
      }
      return true;
    } break;
    case MicroArgType::kCondition: {
      if (arg_name == "Z") {
        arg = CpuCore::ZShift | 4;
      } else if (arg_name == "NZ") {
        arg = CpuCore::ZShift;
      } else if (arg_name == "S") {
        arg = CpuCore::SShift | 4;
      } else if (arg_name == "NS") {
        arg = CpuCore::SShift;
      } else if (arg_name == "C") {
        arg = CpuCore::CShift | 4;
      } else if (arg_name == "NC") {
        arg = CpuCore::CShift;
      } else if (arg_name == "O") {
        arg = CpuCore::OShift | 4;
      } else if (arg_name == "NO") {
        arg = CpuCore::OShift;
      } else {
        return Error("Invalid condition: ", arg_name);
      }
      return true;
    } break;
    case MicroArgType::kAddress: {
      DCHECK(state_.parsed != nullptr);
      const int index =
          static_cast<int>(state_.parsed - state_.parsed_code.data());
      const int jump_from = index + 1;
      int jump = 0;
      if (arg_name[0] != '@') {
        if (!absl::SimpleAtoi(arg_name, &jump)) {
          return Error("Invalid argument: ", arg_name);
        }
      } else {
        std::string_view label = arg_name.substr(1);
        std::string label_key = absl::AsciiStrToUpper(label);
        if (!state_.labels.contains(label_key)) {
          return Error("Unknown label: ", label);
        }
        jump = state_.labels[label_key] - jump_from;
      }
      const int min_jump = std::max<int>(-128, -jump_from);
      const int max_jump = std::min<int>(
          127, static_cast<int>(state_.parsed_code.size()) - jump_from);
      if (jump < min_jump || jump > max_jump) {
        return Error("Value out of range [", min_jump, ",", max_jump,
                     "]: ", arg_name);
      }
      arg = jump;
      return true;
    } break;
    case MicroArgType::kValue: {
      int value;
      if (!absl::SimpleAtoi(arg_name, &value)) {
        return Error("Invalid argument: ", arg_name);
      }
      if (value < -128 || value > 127) {
        return Error("Value out of range [-128,127]: ", arg_name);
      }
      arg = value;
      return true;
    } break;
    case MicroArgType::kWordReg: {
      if (arg_name == "a") {
        if (state_.macro_code_def != nullptr) {
          return Error(
              "Macro code cannot reference instruction arguments (use \"m\" "
              "for macro arguments)");
        }
        if (state_.arg1->type != ArgType::kWordReg) {
          return Error("First argument is not a word register. Argument is ",
                       ArgTypeToString(state_.arg1->type));
        }
        arg = CpuCore::A;
      } else if (arg_name == "a0") {
        if (state_.macro_code_def != nullptr) {
          return Error(
              "Macro code cannot reference instruction arguments (use \"m0\" "
              "for macro arguments)");
        }
        if (state_.arg1->type != ArgType::kDwordReg) {
          return Error("First argument is not a dwrod register. Argument is ",
                       ArgTypeToString(state_.arg1->type));
        }
        arg = CpuCore::A0;
      } else if (arg_name == "a1") {
        if (state_.macro_code_def != nullptr) {
          return Error(
              "Macro code cannot reference instruction arguments (use \"m1\" "
              "for macro arguments)");
        }
        if (state_.arg1->type != ArgType::kDwordReg) {
          return Error("First argument is not a dwrod register. Argument is ",
                       ArgTypeToString(state_.arg1->type));
        }
        arg = CpuCore::A1;
      } else if (arg_name == "b") {
        if (state_.macro_code_def != nullptr) {
          return Error(
              "Macro code cannot reference instruction arguments (use \"m\" "
              "for macro arguments)");
        }
        if (state_.arg2->type != ArgType::kWordReg) {
          return Error("Second argument is not a word register. Argument is ",
                       ArgTypeToString(state_.arg2->type));
        }
        arg = CpuCore::B;
      } else if (arg_name == "b0") {
        if (state_.macro_code_def != nullptr) {
          return Error(
              "Macro code cannot reference instruction arguments (use \"m0\" "
              "for macro arguments)");
        }
        if (state_.arg2->type != ArgType::kDwordReg) {
          return Error("Second argument is not a dword register. Argument is ",
                       ArgTypeToString(state_.arg2->type));
        }
        arg = CpuCore::B0;
      } else if (arg_name == "b1") {
        if (state_.macro_code_def != nullptr) {
          return Error(
              "Macro code cannot reference instruction arguments (use \"m1\" "
              "for macro arguments)");
        }
        if (state_.arg2->type != ArgType::kDwordReg) {
          return Error("Second argument is not a dword register. Argument is ",
                       ArgTypeToString(state_.arg2->type));
        }
        arg = CpuCore::B1;
      } else if (arg_name == "i") {
        if (state_.macro_code_def == nullptr) {
          return Error(
              "Instruction code cannot reference macro arguments (use "
              "\"C0\"/\"C1\" for instruction immediate values)");
        }
        if (state_.argm->type != ArgType::kImmediate) {
          return Error("Macro argument is not an immediate value. Argument is ",
                       ArgTypeToString(state_.argm->type));
        }
        arg = CpuCore::MI;
      } else if (arg_name == "m") {
        if (state_.macro_code_def == nullptr) {
          return Error(
              "Instruction code cannot reference macro arguments (use "
              "\"a\"/\"b\" for instruction arguments)");
        }
        if (state_.argm->type != ArgType::kWordReg) {
          return Error("Macro argument is not a word register. Argument is ",
                       ArgTypeToString(state_.argm->type));
        }
        arg = CpuCore::MM;
      } else if (arg_name == "m0") {
        if (state_.macro_code_def == nullptr) {
          return Error(
              "Instruction code cannot reference macro arguments (use "
              "\"a0\"/\"b0\" for instruction arguments)");
        }
        if (state_.argm->type != ArgType::kDwordReg) {
          return Error("Macro argument is not a dword register. Argument is ",
                       ArgTypeToString(state_.argm->type));
        }
        arg = CpuCore::MM0;
      } else if (arg_name == "m1") {
        if (state_.macro_code_def == nullptr) {
          return Error(
              "Instruction code cannot reference macro arguments (use "
              "\"a1\"/\"b1\" for instruction arguments)");
        }
        if (state_.argm->type != ArgType::kDwordReg) {
          return Error("Macro argument is not a dword register. Argument is ",
                       ArgTypeToString(state_.argm->type));
        }
        arg = CpuCore::MM1;
      } else if (arg_name == "p") {
        if (state_.macro_code_def == nullptr) {
          return Error("Instruction code cannot reference macro parameter");
        }
        if (macro_def_->param != ArgType::kWordReg) {
          return Error("Macro parameter is not a word register. Parameter is ",
                       ArgTypeToString(macro_def_->param));
        }
        arg = CpuCore::MP;
      } else if (arg_name == "p0") {
        if (state_.macro_code_def == nullptr) {
          return Error("Instruction code cannot reference macro parameter");
        }
        if (macro_def_->param != ArgType::kDwordReg) {
          return Error("Macro parameter is not a dword register. Parameter is ",
                       ArgTypeToString(macro_def_->param));
        }
        arg = CpuCore::MP0;
      } else if (arg_name == "p1") {
        if (state_.macro_code_def == nullptr) {
          return Error("Instruction code cannot reference macro parameter");
        }
        if (macro_def_->param != ArgType::kDwordReg) {
          return Error("Macro parameter is not a dword register. Parameter is ",
                       ArgTypeToString(macro_def_->param));
        }
        arg = CpuCore::MP1;
      } else if (arg_name == "r") {
        if (state_.macro_code_def != nullptr) {
          return Error("Macro code cannot reference its own return value");
        }
        if (!state_.macro_return_type.has_value()) {
          return Error("Macro return type referenced before macro called");
        }
        if (*state_.macro_return_type != ArgType::kWordReg) {
          return Error(
              "Macro return type is not a word register. Return type is ",
              ArgTypeToString(*state_.macro_return_type));
        }
        arg = CpuCore::MR;
      } else if (arg_name == "r0") {
        if (state_.macro_code_def != nullptr) {
          return Error("Macro code cannot reference its own return value");
        }
        if (!state_.macro_return_type.has_value()) {
          return Error("Macro return type referenced before macro called");
        }
        if (*state_.macro_return_type != ArgType::kDwordReg) {
          return Error(
              "Macro return type is not a dword register. Return type is ",
              ArgTypeToString(*state_.macro_return_type));
        }
        arg = CpuCore::MR0;
      } else if (arg_name == "r1") {
        if (state_.macro_code_def != nullptr) {
          return Error("Macro code cannot reference its own return value");
        }
        if (!state_.macro_return_type.has_value()) {
          return Error("Macro return type referenced before macro called");
        }
        if (*state_.macro_return_type != ArgType::kDwordReg) {
          return Error(
              "Macro return type is not a dword register. Return type is ",
              ArgTypeToString(*state_.macro_return_type));
        }
        arg = CpuCore::MR1;
      } else {
        arg = CpuCore::GetWordRegFromName(arg_name);
        if (arg == CpuCore::kInvalidReg) {
          return Error("Invalid word argument: ", arg_name);
        }
      }
      return true;
    } break;
    case MicroArgType::kDwordReg: {
      if (arg_name == "A") {
        if (state_.macro_code_def != nullptr) {
          return Error(
              "Macro code cannot reference instruction arguments (use \"M\" "
              "for macro arguments)");
        }
        if (state_.arg1->type != ArgType::kDwordReg) {
          return Error("First argument is not a dwrod register. Argument is ",
                       ArgTypeToString(state_.arg1->type));
        }
        arg = CpuCore::A;
      } else if (arg_name == "B") {
        if (state_.macro_code_def != nullptr) {
          return Error(
              "Macro code cannot reference instruction arguments (use \"M\" "
              "for macro arguments)");
        }
        if (state_.arg2->type != ArgType::kDwordReg) {
          return Error("Second argument is not a dword register. Argument is ",
                       ArgTypeToString(state_.arg2->type));
        }
        arg = CpuCore::B;
      } else if (arg_name == "M") {
        if (state_.macro_code_def == nullptr) {
          return Error(
              "Instruction code cannot reference macro arguments (use "
              "\"A\"/\"B\" for instruction arguments)");
        }
        if (state_.argm->type != ArgType::kDwordReg) {
          return Error("Macro argument is not a dword register. Argument is ",
                       ArgTypeToString(state_.argm->type));
        }
        arg = CpuCore::MM;
      } else if (arg_name == "P") {
        if (state_.macro_code_def == nullptr) {
          return Error("Instruction code cannot reference macro parameters");
        }
        if (macro_def_->param != ArgType::kDwordReg) {
          return Error("Macro parameter is not a dword register. Parameter is ",
                       ArgTypeToString(macro_def_->param));
        }
        arg = CpuCore::MP;
      } else if (arg_name == "R") {
        if (state_.macro_code_def != nullptr) {
          return Error("Macro code cannot reference its own return value");
        }
        if (!state_.macro_return_type.has_value()) {
          return Error("Macro return type referenced before macro called");
        }
        if (*state_.macro_return_type != ArgType::kDwordReg) {
          return Error(
              "Macro return type is not a dword register. Return type is ",
              ArgTypeToString(*state_.macro_return_type));
        }
        arg = CpuCore::MR;
      } else {
        arg = CpuCore::GetDwordRegFromName(arg_name);
        if (arg == CpuCore::kInvalidReg) {
          return Error("Invalid dword argument: ", arg_name);
        }
      }
      return true;
    } break;
  }
  return Error("Unhandled argument type!");
}

bool InstructionCompiler::ValidateMacroReturnRegister(int8_t ret) {
  DCHECK(macro_def_ != nullptr);
  if (macro_def_->ret == ArgType::kNone) {
    if (ret != 0) {
      return Error("Invalid return register for macro with no return value");
    }
    return true;
  }
  if (macro_def_->ret == ArgType::kWordReg) {
    if (ret >= 0 && ret < CpuCore::kRegisterCount) {
      return true;
    }
    if (ret == CpuCore::MP0 && (macro_def_->param == ArgType::kWordReg ||
                                macro_def_->param == ArgType::kDwordReg)) {
      return true;
    }
    if (ret == CpuCore::MP1 && macro_def_->param == ArgType::kDwordReg) {
      return true;
    }
    return Error("Invalid word register '", CpuCore::GetVirtualWordRegName(ret),
                 "' for return type ", ArgTypeToString(macro_def_->ret));
  } else if (macro_def_->ret == ArgType::kDwordReg) {
    if (CpuCore::IsDwordReg(ret)) {
      return true;
    }
    if (ret == CpuCore::MP && macro_def_->param == ArgType::kDwordReg) {
      return true;
    }
    return Error("Invalid dword register '",
                 CpuCore::GetVirtualDwordRegName(ret), "' for return type ",
                 ArgTypeToString(macro_def_->ret));
  }
  LOG(FATAL) << "Invalid return type";  // Ensured by CompileMacro
  return false;
}

bool InstructionCompiler::ValidateMicrocode(int index) {
  const MicrocodeDef* def = FindMicrocodeDef(state_.microcode->op);
  DCHECK(def != nullptr);  // Validated by CompileMicrocode
  if (!ValidateMicroArg(index, def->arg1, state_.microcode->arg1)) {
    return false;
  }
  if (!ValidateMicroArg(index, def->arg2, state_.microcode->arg2)) {
    return false;
  }
  switch (def->op) {
    case kMicro_LK:
    case kMicro_LKR:
      CHECK(state_.lock_type_ == LockType::kNone);  // Handled by InitLocks.
      state_.lock_type_ = LockType::kMemory;
      state_.had_lk_ = true;
      break;
    case kMicro_UL:
      CHECK(state_.lock_type_ == LockType::kMemory);  // Handled by InitLocks.
      state_.lock_type_ = LockType::kNone;
      state_.has_adr_ = false;
      state_.in_fetch_ = false;
      break;
    case kMicro_ADR:
      if (state_.lock_type_ != LockType::kMemory) {
        return Error("ADR without a prior LK");
      }
      state_.has_adr_ = true;
      state_.in_fetch_ = false;
      break;
    case kMicro_LD:
      if (!state_.has_adr_) {
        return Error("LD without a prior ADR");
      }
      if (state_.in_fetch_) {
        state_.code->pc_size += 1;
      }
      break;
    case kMicro_ST:
      if (!state_.has_adr_) {
        return Error("ST without a prior ADR");
      }
      if (state_.in_fetch_) {
        return Error("ST invalid in fetch phase, call UL and LK again first");
      }
      break;
    case kMicro_STP:
      if (!state_.has_adr_) {
        return Error("STP without a prior ADR");
      }
      if (state_.in_fetch_) {
        return Error("STP invalid in fetch phase, call UL and LK again first");
      }
      break;
    case kMicro_PLK:
      CHECK(state_.lock_type_ == LockType::kNone);  // Handled by InitLocks.
      state_.lock_type_ = LockType::kPort;
      break;
    case kMicro_PUL:
      CHECK(state_.lock_type_ == LockType::kPort);  // Handled by InitLocks.
      state_.lock_type_ = LockType::kNone;
      break;
    case kMicro_PLD:
      if (state_.lock_type_ != LockType::kPort) {
        return Error("PLD without a prior PLK");
      }
      break;
    case kMicro_PST:
      if (state_.lock_type_ != LockType::kPort) {
        return Error("PST without a prior PLK");
      }
      break;
    case kMicro_CLK:
      CHECK(state_.lock_type_ == LockType::kNone);  // Handled by InitLocks.
      state_.lock_type_ = LockType::kCore;
      break;
    case kMicro_CUL:
      CHECK(state_.lock_type_ == LockType::kCore);  // Handled by InitLocks.
      state_.lock_type_ = LockType::kNone;
      break;
    case kMicro_WAIT:
      if (state_.in_fetch_) {
        return Error("WAIT in fetch phase");
      } else if (state_.lock_type_ == LockType::kMemory) {
        return Error("WAIT between LK and UL");
      } else if (state_.lock_type_ == LockType::kPort) {
        return Error("WAIT between PLK and PUL");
      } else if (state_.lock_type_ == LockType::kCore) {
        return Error("WAIT between CLK and CUL");
      }
      CHECK(state_.lock_type_ == LockType::kNone) << "Unhandled lock type";
      break;
    case kMicro_HALT:
      if (state_.in_fetch_) {
        return Error("HALT in fetch phase");
      } else if (state_.lock_type_ == LockType::kMemory) {
        return Error("HALT between LK and UL");
      } else if (state_.lock_type_ == LockType::kPort) {
        return Error("HALT between PLK and PUL");
      } else if (state_.lock_type_ == LockType::kCore) {
        return Error("HALT between CLK and CUL");
      }
      CHECK(state_.lock_type_ == LockType::kNone) << "Unhandled lock type";
      break;
    case kMicro_IRT:
      if (state_.in_fetch_) {
        return Error("IRT in fetch phase");
      } else if (state_.lock_type_ == LockType::kMemory) {
        return Error("IRT between LK and UL");
      } else if (state_.lock_type_ == LockType::kPort) {
        return Error("IRT between PLK and PUL");
      } else if (state_.lock_type_ == LockType::kCore) {
        return Error("IRT between CLK and CUL");
      }
      CHECK(state_.lock_type_ == LockType::kNone) << "Unhandled lock type";
      break;
    case kMicro_END:
      if (state_.in_fetch_) {
        return Error("END in fetch phase");
      } else if (state_.lock_type_ == LockType::kMemory) {
        return Error("END between LK and UL");
      } else if (state_.lock_type_ == LockType::kPort) {
        return Error("END between PLK and PUL");
      } else if (state_.lock_type_ == LockType::kCore) {
        return Error("END between CLK and CUL");
      }
      CHECK(state_.lock_type_ == LockType::kNone) << "Unhandled lock type";
      break;
    default:
      break;
  }
  return true;
}

bool InstructionCompiler::ValidateMicroArg(int index, MicroArgType arg_type,
                                           int8_t arg) {
  DCHECK(arg_type != MicroArgType::kWordReg ||
         (arg >= CpuCore::kMinWordArgReg) && arg < CpuCore::kRegisterCount);
  DCHECK(arg_type != MicroArgType::kDwordReg ||
         (arg >= CpuCore::kMinDwordArgReg && arg < 0) ||
         CpuCore::IsDwordReg(arg));
  if (arg_type == MicroArgType::kAddress) {
    const int jump = index + arg + 1;
    if (state_.locks[index] != state_.locks[jump]) {
      return Error("Jump between locks from ", LockName(state_.locks[index]),
                   " to ", LockName(state_.locks[jump]));
    }
  }
  return true;
}

std::shared_ptr<const InstructionSet> CompileInstructionSet(
    InstructionSetDef instruction_set_def, InstructionError* error,
    absl::Span<const MicrocodeDef> microcode_defs) {
  InstructionCompiler compiler(microcode_defs);
  for (const MacroDef& macro_def : instruction_set_def.macros) {
    if (!compiler.CompileMacro(macro_def, error)) {
      return std::make_shared<InstructionSet>();
    }
  }
  for (const InstructionDef& instruction_def :
       instruction_set_def.instructions) {
    if (!compiler.CompileInstruction(instruction_def, error)) {
      return std::make_shared<InstructionSet>();
    }
  }
  return std::make_shared<const InstructionSet>(
      std::move(compiler).ToInstructionSet());
}

}  // namespace oz3
