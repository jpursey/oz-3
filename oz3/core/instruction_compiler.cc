// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_compiler.h"

#include <algorithm>
#include <optional>
#include <string>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/ascii.h"
#include "absl/strings/numbers.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_split.h"
#include "absl/types/span.h"
#include "glog/logging.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/port.h"

namespace oz3 {

namespace {

constexpr int kFirstRegisterLock = 10;
constexpr int kFirstPortLock = 100;
constexpr int kFirstCoreLock = 200;

}  // namespace

class InstructionCompiler {
 public:
  InstructionCompiler(absl::Span<const MicrocodeDef> microcode_defs)
      : microcode_defs_(microcode_defs) {}

  bool Compile(const InstructionDef& instruction_def,
               std::string* error_string);

  InstructionSet ToInstructionSet() && {
    return InstructionSet(std::move(instructions_),
                          std::move(sub_instructions_), std::move(microcodes_));
  }

 private:
  friend class OldInstructionCompiler;

  struct MacroCode {
    uint16_t code_start = 0;  // Start index in microcode
    uint8_t code_size = 0;    // Number of microcodes
    int8_t ret;               // Return register index (or -1 for arg).
  };

  struct Macro {
    uint8_t bit_size = 0;  // Number of bits that define the macro
    ArgType arg = ArgType::kNone;
    ArgType ret = ArgType::kNone;
  };

  struct ParsedMicroCode {
    std::string_view code;       // Full code that was parsed.
    std::string_view op_name;    // Operation name.
    std::string_view arg1_name;  // First argument name.
    std::string_view arg2_name;  // Second argument name.
  };

  enum class LockType { kNone, kMemory, kPort, kCore };

  bool Error(const ParsedMicroCode* parsed, absl::string_view message) {
    if (state_.error != nullptr) {
      if (parsed != nullptr) {
        *state_.error =
            absl::StrCat("Error in ", state_.instruction_def->op_name,
                         " for \"", parsed->code, "\": ", message);
      } else {
        *state_.error = absl::StrCat(
            "Error in ", state_.instruction_def->op_name, ": ", message);
      }
    }
    return false;
  }
  bool Error(absl::string_view message) { return Error(nullptr, message); }

  bool CompileMicroCode(int index);
  bool ExtractLabels();
  ParsedMicroCode ParseMicroCode(std::string_view src_code);
  bool InitLocks();
  const MicrocodeDef* FindMicroCodeDef(std::string_view op_name);
  bool DecodeArg(ParsedMicroCode* parsed, int index, std::string_view arg_name,
                 MicroArgType arg_type, int8_t& arg);

  // Initial state.
  absl::Span<const MicrocodeDef> microcode_defs_;

  using Instruction = microcode_internal::Instruction;
  using SubInstruction = microcode_internal::SubInstruction;
  using InstructionCode = microcode_internal::InstructionCode;

  // Persistent state.
  std::vector<Instruction> instructions_;
  std::vector<SubInstruction> sub_instructions_;
  std::vector<Microcode> microcodes_;

  absl::flat_hash_map<std::string, Macro> macros_;
  std::vector<Microcode> macro_microcodes_;
  std::vector<MacroCode> macro_code_;

  // Temporary state during compilation.
  struct State {
    std::string* error = nullptr;
    const InstructionDef* instruction_def = nullptr;
    const Argument* arg1 = nullptr;
    const Argument* arg2 = nullptr;
    std::vector<std::string_view> src_code;
    absl::flat_hash_map<std::string, int> labels;
    std::vector<ParsedMicroCode> parsed_code;
    std::vector<int> locks;
    Instruction* instruction;
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

bool InstructionCompiler::Compile(const InstructionDef& instruction_def,
                                  std::string* error_string) {
  state_ = {};
  state_.instruction_def = &instruction_def;
  state_.error = error_string;
  if (state_.error != nullptr) {
    state_.error->clear();
  }

  int op_index = static_cast<int>(state_.instruction_def->op);
  if (instructions_.size() <= op_index) {
    instructions_.resize(op_index + 1);
  }
  state_.instruction = &instructions_[op_index];
  state_.instruction->arg1 = state_.instruction_def->arg1;
  state_.instruction->arg2 = state_.instruction_def->arg2;
  state_.arg1 = &state_.instruction->arg1;
  state_.arg2 = &state_.instruction->arg2;

  state_.instruction->code.code_start =
      static_cast<uint16_t>(microcodes_.size());
  state_.instruction->code.pc_size = 1;
  state_.src_code =
      absl::StrSplit(state_.instruction_def->code, ';', absl::SkipWhitespace());
  if (!ExtractLabels()) {
    return false;
  }
  state_.parsed_code.reserve(state_.src_code.size());
  for (std::string_view& src_code : state_.src_code) {
    state_.parsed_code.push_back(ParseMicroCode(src_code));
  }
  if (!InitLocks()) {
    return false;
  }
  for (int index = 0; index < state_.parsed_code.size(); ++index) {
    state_.microcode = &microcodes_.emplace_back();
    if (microcodes_.size() - state_.instruction->code.code_start >
        kMaxInstructionMicrocodes) {
      microcodes_.pop_back();
      return Error("Too much compiled microcode");
    }
    if (!CompileMicroCode(index)) {
      return false;
    }
  }
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
  DCHECK(microcodes_.size() - state_.instruction->code.code_start <=
         kMaxInstructionMicrocodes);
  state_.instruction->code.code_size = static_cast<uint8_t>(
      microcodes_.size() - state_.instruction->code.code_start);
  return true;
}

bool InstructionCompiler::CompileMicroCode(int index) {
  auto parsed = state_.parsed_code[index];
  const MicrocodeDef* def = FindMicroCodeDef(parsed.op_name);
  if (def == nullptr) {
    return Error(&parsed, "Microcode OpCode not found");
  }
  if (!DecodeArg(&parsed, index, parsed.arg1_name, def->arg1,
                 state_.microcode->arg1)) {
    return false;
  }
  if (!DecodeArg(&parsed, index, parsed.arg2_name, def->arg2,
                 state_.microcode->arg2)) {
    return false;
  }

  // Microcode specific handling.
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
        return Error(&parsed, "ADR without a prior LK");
      }
      state_.has_adr_ = true;
      state_.in_fetch_ = false;
      break;
    case kMicro_LD:
      if (!state_.has_adr_) {
        return Error(&parsed, "LD without a prior ADR");
      }
      if (state_.in_fetch_) {
        state_.instruction->code.pc_size += 1;
      }
      break;
    case kMicro_ST:
      if (!state_.has_adr_) {
        return Error(&parsed, "ST without a prior ADR");
      }
      if (state_.in_fetch_) {
        return Error(&parsed,
                     "ST invalid in fetch phase, call UL and LK again first");
      }
      break;
    case kMicro_STP:
      if (!state_.has_adr_) {
        return Error(&parsed, "STP without a prior ADR");
      }
      if (state_.in_fetch_) {
        return Error(&parsed,
                     "STP invalid in fetch phase, call UL and LK again first");
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
        return Error(&parsed, "PLD without a prior PLK");
      }
      break;
    case kMicro_PST:
      if (state_.lock_type_ != LockType::kPort) {
        return Error(&parsed, "PST without a prior PLK");
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
        return Error(&parsed, "WAIT in fetch phase");
      } else if (state_.lock_type_ == LockType::kMemory) {
        return Error(&parsed, "WAIT between LK and UL");
      } else if (state_.lock_type_ == LockType::kPort) {
        return Error(&parsed, "WAIT between PLK and PUL");
      } else if (state_.lock_type_ == LockType::kCore) {
        return Error(&parsed, "WAIT between CLK and CUL");
      }
      CHECK(state_.lock_type_ == LockType::kNone) << "Unhandled lock type";
      break;
    case kMicro_HALT:
      if (state_.in_fetch_) {
        return Error(&parsed, "HALT in fetch phase");
      } else if (state_.lock_type_ == LockType::kMemory) {
        return Error(&parsed, "HALT between LK and UL");
      } else if (state_.lock_type_ == LockType::kPort) {
        return Error(&parsed, "HALT between PLK and PUL");
      } else if (state_.lock_type_ == LockType::kCore) {
        return Error(&parsed, "HALT between CLK and CUL");
      }
      CHECK(state_.lock_type_ == LockType::kNone) << "Unhandled lock type";
      break;
    case kMicro_IRT:
      if (state_.in_fetch_) {
        return Error(&parsed, "IRT in fetch phase");
      } else if (state_.lock_type_ == LockType::kMemory) {
        return Error(&parsed, "IRT between LK and UL");
      } else if (state_.lock_type_ == LockType::kPort) {
        return Error(&parsed, "IRT between PLK and PUL");
      } else if (state_.lock_type_ == LockType::kCore) {
        return Error(&parsed, "IRT between CLK and CUL");
      }
      CHECK(state_.lock_type_ == LockType::kNone) << "Unhandled lock type";
      break;
    case kMicro_END:
      if (state_.in_fetch_) {
        return Error(&parsed, "END in fetch phase");
      } else if (state_.lock_type_ == LockType::kMemory) {
        return Error(&parsed, "END between LK and UL");
      } else if (state_.lock_type_ == LockType::kPort) {
        return Error(&parsed, "END between PLK and PUL");
      } else if (state_.lock_type_ == LockType::kCore) {
        return Error(&parsed, "END between CLK and CUL");
      }
      CHECK(state_.lock_type_ == LockType::kNone) << "Unhandled lock type";
      break;
    default:
      break;
  }

  return true;
}

bool InstructionCompiler::ExtractLabels() {
  int index = 0;
  for (std::string_view& code : state_.src_code) {
    if (code.starts_with('@')) {
      auto label_end = code.find(':', 1);
      if (label_end == std::string_view::npos) {
        return Error(absl::StrCat("Expected ':' after @ label: ", code));
      }
      std::string_view label = code.substr(1, label_end - 1);
      code.remove_prefix(label_end + 1);
      for (char c : label) {
        if (!absl::ascii_isalnum(c) && c != '_') {
          return Error(absl::StrCat("Invalid label: ", label));
        }
      }
      std::string label_key = absl::AsciiStrToUpper(label);
      if (state_.labels.contains(label_key)) {
        return Error(absl::StrCat("Duplicate label: ", label));
      }
      state_.labels[label_key] = index;
    }
    ++index;
  }
  return true;
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

bool InstructionCompiler::InitLocks() {
  state_.locks.reserve(state_.parsed_code.size());
  static_assert(
      CpuCore::CODE < kFirstPortLock && CpuCore::STACK < kFirstPortLock &&
          CpuCore::DATA < kFirstPortLock && CpuCore::EXTRA < kFirstPortLock,
      "Memory locks must be lower that first port lock");
  int port_lock = kFirstPortLock;
  int core_lock = kFirstCoreLock;
  int lock = CpuCore::CODE;
  int index = 0;
  int lock_count = 0;
  for (ParsedMicroCode& parsed : state_.parsed_code) {
    ++index;
    state_.locks.push_back(lock);
    if (parsed.op_name == "LK") {
      if (lock >= 0) {
        return Error(absl::StrCat("LK when prior ", LockOpNameFromIndex(lock),
                                  " is still locked. Code number: ", index));
      }
      int8_t arg;
      if (!DecodeArg(&parsed, index, parsed.arg1_name, MicroArgType::kBank,
                     arg)) {
        return false;
      }
      lock = arg;
      ++lock_count;
    } else if (parsed.op_name == "LKR") {
      if (lock >= 0) {
        return Error(absl::StrCat("LKR when prior ", LockOpNameFromIndex(lock),
                                  " is still locked. Code number: ", index));
      }
      int8_t arg;
      if (!DecodeArg(&parsed, index, parsed.arg1_name, MicroArgType::kWordReg,
                     arg)) {
        return false;
      }
      lock = kFirstRegisterLock + arg + 10;
      DCHECK(lock >= kFirstRegisterLock);
      ++lock_count;
    } else if (parsed.op_name == "UL") {
      if (lock < 0 || lock >= kFirstPortLock) {
        return Error(
            absl::StrCat("UL without a prior LK or LKR. Code number: ", index));
      }
      lock = -1;
    } else if (parsed.op_name == "PLK") {
      if (lock >= 0) {
        return Error(absl::StrCat("PLK when prior ", LockOpNameFromIndex(lock),
                                  " is still locked. Code number: ", index));
      }
      lock = port_lock++;
      ++lock_count;
    } else if (parsed.op_name == "PUL") {
      if (lock < kFirstPortLock) {
        return Error(
            absl::StrCat("PUL without a prior PLK. Code number: ", index));
      }
      lock = -1;
    } else if (parsed.op_name == "CLK") {
      if (lock >= 0) {
        return Error(absl::StrCat("CLK when prior ", LockOpNameFromIndex(lock),
                                  " is still locked. Code number: ", index));
      }
      lock = core_lock++;
      ++lock_count;
    } else if (parsed.op_name == "CUL") {
      if (lock < kFirstCoreLock) {
        return Error(
            absl::StrCat("CUL without a prior CLK. Code number: ", index));
      }
      lock = -1;
    }
    if (lock_count > kMaxLocksPerInstruction) {
      return Error(absl::StrCat("Too many LK/UL, PLK/PUL, CLK/CUL pairs (max ",
                                kMaxLocksPerInstruction, ")"));
    }
  }
  // We need a value for one past the end, since it is a valid jump address.
  state_.locks.push_back(lock);
  return true;
}

InstructionCompiler::ParsedMicroCode InstructionCompiler::ParseMicroCode(
    std::string_view code) {
  auto RemovePrefix = [&code](std::string_view::size_type amount) {
    code.remove_prefix(std::min(code.size(), amount));
  };

  ParsedMicroCode parsed = {};
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

const MicrocodeDef* InstructionCompiler::FindMicroCodeDef(
    std::string_view op_name) {
  for (const MicrocodeDef& def : microcode_defs_) {
    if (def.op_name == op_name) {
      state_.microcode->op = def.op;
      return &def;
    }
  }
  return nullptr;
}

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

bool InstructionCompiler::DecodeArg(ParsedMicroCode* parsed, int index,
                                    std::string_view arg_name,
                                    MicroArgType arg_type, int8_t& arg) {
  if (arg_name.empty() && arg_type == MicroArgType::kNone) {
    return true;
  }
  if (arg_name.empty()) {
    return Error(parsed, "Argument missing");
  }
  if (arg_type == MicroArgType::kNone) {
    return Error(parsed, "Argument not allowed");
  }
  if (arg_type == MicroArgType::kBank) {
    if (arg_name == "CODE") {
      arg = CpuCore::CODE;
    } else if (arg_name == "STACK") {
      arg = CpuCore::STACK;
    } else if (arg_name == "DATA") {
      arg = CpuCore::DATA;
    } else if (arg_name == "EXTRA") {
      arg = CpuCore::EXTRA;
    } else {
      return Error(parsed, absl::StrCat("Invalid bank: ", arg_name));
    }
    return true;
  }
  if (arg_type == MicroArgType::kStatus) {
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
        return Error(parsed, absl::StrCat("Invalid status flags: ", arg_name));
      }
    }
    return true;
  }
  if (arg_type == MicroArgType::kPortMode) {
    for (char c : arg_name) {
      if (c == 'T') {
        arg |= Port::T;
      } else if (c == 'S') {
        arg |= Port::S;
      } else if (c == 'A') {
        arg |= Port::A;
      } else if (c != '_') {
        return Error(parsed, absl::StrCat("Invalid port mode: ", arg_name));
      }
    }
    return true;
  }
  if (arg_type == MicroArgType::kCondition) {
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
      return Error(parsed, absl::StrCat("Invalid condition: ", arg_name));
    }
    return true;
  }
  if (arg_type == MicroArgType::kAddress) {
    int jump;
    const int jump_from = index + 1;
    if (arg_name[0] != '@') {
      if (!absl::SimpleAtoi(arg_name, &jump)) {
        return Error(parsed, absl::StrCat("Invalid argument: ", arg_name));
      }
    } else {
      std::string_view label = arg_name.substr(1);
      std::string label_key = absl::AsciiStrToUpper(label);
      if (!state_.labels.contains(label_key)) {
        return Error(parsed, absl::StrCat("Unknown label: ", label));
      }
      jump = state_.labels[label_key] - jump_from;
    }
    const int min_jump = std::max<int>(-128, -jump_from);
    const int max_jump = std::min<int>(
        127, static_cast<int>(state_.parsed_code.size()) - jump_from);
    if (jump < min_jump || jump > max_jump) {
      return Error(parsed, absl::StrCat("Value out of range [", min_jump, ",",
                                        max_jump, "]: ", arg_name));
    }
    if (state_.locks[index] != state_.locks[jump_from + jump]) {
      return Error(parsed,
                   absl::StrCat("Jump between locks from ",
                                LockName(state_.locks[index]), " to ",
                                LockName(state_.locks[jump_from + jump])));
    }
    arg = jump;
    return true;
  }
  if (arg_type == MicroArgType::kValue) {
    int value;
    if (!absl::SimpleAtoi(arg_name, &value)) {
      return Error(parsed, absl::StrCat("Invalid argument: ", arg_name));
    }
    if (value < -128 || value > 127) {
      return Error(parsed,
                   absl::StrCat("Value out of range [-128,127]: ", arg_name));
    }
    arg = value;
    return true;
  }
  if (arg_type == MicroArgType::kWordReg) {
    if (arg_name == "a") {
      if (state_.arg1->type != ArgType::kWordReg) {
        return Error(
            parsed,
            absl::StrCat("First argument is not a word register. It is \"",
                         ArgTypeToString(state_.arg1->type), "\")"));
      }
      arg = -1;
    } else if (arg_name == "a0") {
      if (state_.arg1->type != ArgType::kDwordReg) {
        return Error(
            parsed,
            absl::StrCat("First argument is not a dwrod register. It is\"",
                         ArgTypeToString(state_.arg1->type), "\")"));
      }
      arg = -1;
    } else if (arg_name == "a1") {
      if (state_.arg1->type != ArgType::kDwordReg) {
        return Error(
            parsed,
            absl::StrCat("First argument is not a dwrod register. It is\"",
                         ArgTypeToString(state_.arg1->type), "\")"));
      }
      arg = -3;
    } else if (arg_name == "b") {
      if (state_.arg2->type != ArgType::kWordReg) {
        return Error(
            parsed,
            absl::StrCat("Second argument is not a word register. It is \"",
                         ArgTypeToString(state_.arg2->type), "\")"));
      }
      arg = -2;
    } else if (arg_name == "b0") {
      if (state_.arg2->type != ArgType::kDwordReg) {
        return Error(
            parsed,
            absl::StrCat("Second argument is not a dword register. It is \"",
                         ArgTypeToString(state_.arg2->type), "\")"));
      }
      arg = -2;
    } else if (arg_name == "b1") {
      if (state_.arg2->type != ArgType::kDwordReg) {
        return Error(
            parsed,
            absl::StrCat("Second argument is not a dword register. It is \"",
                         ArgTypeToString(state_.arg2->type), "\")"));
      }
      arg = -4;
    } else if (arg_name[0] == 'R' && arg_name.size() == 2) {
      arg = arg_name[1] - '0';
      if (arg < 0 || arg > 7) {
        return Error(parsed,
                     absl::StrCat("Invalid register index: ", arg_name));
      }
      arg += CpuCore::R0;
    } else if (arg_name == "C0") {
      arg = CpuCore::C0;
    } else if (arg_name == "C1") {
      arg = CpuCore::C1;
    } else if (arg_name == "C2") {
      arg = CpuCore::C2;
    } else if (arg_name == "PC") {
      arg = CpuCore::PC;
    } else if (arg_name == "SP") {
      arg = CpuCore::SP;
    } else if (arg_name == "DP") {
      arg = CpuCore::DP;
    } else if (arg_name == "ST") {
      arg = CpuCore::ST;
    } else if (arg_name == "BM") {
      arg = CpuCore::BM;
    } else {
      return Error(parsed, absl::StrCat("Invalid word argument: ", arg_name));
    }
    return true;
  }
  if (arg_type == MicroArgType::kDwordReg) {
    if (arg_name == "A") {
      if (state_.arg1->type != ArgType::kDwordReg) {
        return Error(
            parsed,
            absl::StrCat("First argument is not a dwrod register. It is\"",
                         ArgTypeToString(state_.arg1->type), "\")"));
      }
      arg = -1;
    } else if (arg_name == "B") {
      if (state_.arg2->type != ArgType::kDwordReg) {
        return Error(
            parsed,
            absl::StrCat("Second argument is not a dword register. It is \"",
                         ArgTypeToString(state_.arg2->type), "\")"));
      }
      arg = -2;
    } else if (arg_name[0] == 'D' && arg_name.size() == 2) {
      arg = arg_name[1] - '0';
      if (arg < 0 || arg > 3) {
        return Error(parsed,
                     absl::StrCat("Invalid register index: ", arg_name));
      }
      arg = CpuCore::D0 + arg * 2;
    } else if (arg_name == "SD") {
      arg = CpuCore::SD;
    } else {
      return Error(parsed, absl::StrCat("Invalid dword argument: ", arg_name));
    }
    return true;
  }
  return Error(parsed, "Unhandled argument type!");
}

std::shared_ptr<const InstructionSet> CompileInstructionSet(
    InstructionSetDef instruction_set_def, std::string* error_string,
    absl::Span<const MicrocodeDef> microcode_defs) {
  InstructionCompiler compiler(microcode_defs);
  for (const InstructionDef& instruction_def :
       instruction_set_def.instructions) {
    if (!compiler.Compile(instruction_def, error_string)) {
      return std::make_shared<InstructionSet>();
    }
  }
  return std::make_shared<const InstructionSet>(
      std::move(compiler).ToInstructionSet());
}

}  // namespace oz3
