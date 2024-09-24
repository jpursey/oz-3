// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/microcode.h"

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

static constexpr int kFirstPortLock = 10;

const Microcode kNopMicroCode[] = {{.op = kMicro_UL}};
const DecodedInstruction kNopDecoded = {.code = kNopMicroCode, .size = 1};

const MicrocodeDef kMicroCodeDefs[] = {
    {kMicro_MSTC, "MSTC", MicroArgType::kStatus},
    {kMicro_MSTS, "MSTS", MicroArgType::kStatus},
    {kMicro_MSTX, "MSTX", MicroArgType::kStatus},
    {kMicro_MSTM, "MSTM", MicroArgType::kStatus, MicroArgType::kWordReg},
    {kMicro_MSTR, "MSTR", MicroArgType::kStatus, MicroArgType::kStatus},
    {kMicro_WAIT, "WAIT", MicroArgType::kWordReg},
    {kMicro_HALT, "HALT"},
    {kMicro_LK, "LK", MicroArgType::kBank},
    {kMicro_UL, "UL"},
    {kMicro_ADR, "ADR", MicroArgType::kWordReg},
    {kMicro_LADR, "LADR", MicroArgType::kWordReg},
    {kMicro_LD, "LD", MicroArgType::kWordReg},
    {kMicro_ST, "ST", MicroArgType::kWordReg},
    {kMicro_STP, "STP", MicroArgType::kWordReg},
    {kMicro_MOVI, "MOVI", MicroArgType::kWordReg, MicroArgType::kValue},
    {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_ADDI, "ADDI", MicroArgType::kWordReg, MicroArgType::kValue},
    {kMicro_ADD, "ADD", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_ADC, "ADC", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_SUB, "SUB", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_SBC, "SBC", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_NEG, "NEG", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_CMP, "CMP", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_NOT, "NOT", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_AND, "AND", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_OR, "OR", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_XOR, "XOR", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_SL, "SL", MicroArgType::kWordReg},
    {kMicro_SR, "SR", MicroArgType::kWordReg},
    {kMicro_SRA, "SRA", MicroArgType::kWordReg},
    {kMicro_RL, "RL", MicroArgType::kWordReg},
    {kMicro_RR, "RR", MicroArgType::kWordReg},
    {kMicro_RLC, "RLC", MicroArgType::kWordReg},
    {kMicro_RRC, "RRC", MicroArgType::kWordReg},
    {kMicro_JP, "JP", MicroArgType::kAddress},
    {kMicro_JC, "JC", MicroArgType::kCondition, MicroArgType::kAddress},
    {kMicro_JD, "JD", MicroArgType::kWordReg, MicroArgType::kAddress},
    {kMicro_INT, "INT", MicroArgType::kWordReg},
    {kMicro_ILD, "ILD", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_IST, "IST", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_PLK, "PLK", MicroArgType::kWordReg},
    {kMicro_PUL, "PUL"},
    {kMicro_PLD, "PLD", MicroArgType::kPortMode, MicroArgType::kWordReg},
    {kMicro_PST, "PST", MicroArgType::kPortMode, MicroArgType::kWordReg},
    {kMicro_END, "END"},
};

class InstructionCompiler {
 public:
  InstructionCompiler(const InstructionDef& instruction,
                      absl::Span<const MicrocodeDef> microcode_defs,
                      CompiledInstruction& compiled, std::string* error_string)
      : instruction_(instruction),
        microcode_defs_(microcode_defs),
        compiled_(compiled),
        error_string_(error_string) {
    if (error_string_ != nullptr) {
      error_string_->clear();
    }
  }

  bool Compile();

 private:
  struct ParsedMicroCode {
    std::string_view code;       // Full code that was parsed.
    std::string_view op_name;    // Operation name.
    std::string_view arg1_name;  // First argument name.
    std::string_view arg2_name;  // Second argument name.
  };

  enum class LockType { kNone, kMemory, kPort };

  bool Error(const ParsedMicroCode* parsed, absl::string_view message) {
    if (error_string_ != nullptr) {
      if (parsed != nullptr) {
        *error_string_ = absl::StrCat("Error in ", instruction_.decl.op_name,
                                      " for \"", parsed->code, "\": ", message);
      } else {
        *error_string_ =
            absl::StrCat("Error in ", instruction_.decl.op_name, ": ", message);
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

  const InstructionDef& instruction_;
  absl::Span<const MicrocodeDef> microcode_defs_;

  std::vector<std::string_view> src_code_;
  absl::flat_hash_map<std::string, int> labels_;
  std::vector<ParsedMicroCode> parsed_code_;
  std::vector<int> locks_;
  CompiledInstruction& compiled_;
  std::string* const error_string_;
  Microcode* microcode_ = nullptr;

  // Set by last LK or PLK operation. Cleared by UL and PUL.
  LockType lock_type_ = LockType::kMemory;

  // True if an ADR operation has been seen. Cleared by UL.
  bool has_adr_ = true;

  // True if still in the fetch phase cleared by UL and ADR.
  bool in_fetch_ = true;

  // True if an LK operation has been seen.
  bool had_lk_ = false;
};

bool InstructionCompiler::Compile() {
  compiled_.code.clear();
  compiled_.size = 1;
  src_code_ = absl::StrSplit(instruction_.code, ';', absl::SkipWhitespace());
  if (!ExtractLabels()) {
    return false;
  }
  parsed_code_.reserve(src_code_.size());
  for (std::string_view& src_code : src_code_) {
    parsed_code_.push_back(ParseMicroCode(src_code));
  }
  if (!InitLocks()) {
    return false;
  }
  for (int index = 0; index < parsed_code_.size(); ++index) {
    microcode_ = &compiled_.code.emplace_back();
    if (!CompileMicroCode(index)) {
      compiled_.code.clear();
      return false;
    }
  }
  if (lock_type_ == LockType::kMemory) {
    if (had_lk_) {
      return Error("LK not cleared by UL");
    } else {
      return Error("No UL for instruction fetch");
    }
  } else if (lock_type_ == LockType::kPort) {
    return Error("PLK not cleared by PUL");
  }
  return true;
}

bool InstructionCompiler::CompileMicroCode(int index) {
  auto parsed = parsed_code_[index];
  const MicrocodeDef* def = FindMicroCodeDef(parsed.op_name);
  if (def == nullptr) {
    return Error(&parsed, "Microcode OpCode not found");
  }
  if (!DecodeArg(&parsed, index, parsed.arg1_name, def->arg1,
                 microcode_->arg1)) {
    return false;
  }
  if (!DecodeArg(&parsed, index, parsed.arg2_name, def->arg2,
                 microcode_->arg2)) {
    return false;
  }

  // Microcode specific handling.
  switch (def->op) {
    case kMicro_LK:
      CHECK(lock_type_ == LockType::kNone);  // Handled by InitLocks.
      lock_type_ = LockType::kMemory;
      had_lk_ = true;
      break;
    case kMicro_UL:
      CHECK(lock_type_ == LockType::kMemory);  // Handled by InitLocks.
      lock_type_ = LockType::kNone;
      has_adr_ = false;
      in_fetch_ = false;
      break;
    case kMicro_ADR:
      if (lock_type_ != LockType::kMemory) {
        return Error(&parsed, "ADR without a prior LK");
      }
      has_adr_ = true;
      in_fetch_ = false;
      break;
    case kMicro_LD:
      if (!has_adr_) {
        return Error(&parsed, "LD without a prior ADR");
      }
      if (in_fetch_) {
        compiled_.size += 1;
      }
      break;
    case kMicro_ST:
      if (!has_adr_) {
        return Error(&parsed, "ST without a prior ADR");
      }
      if (in_fetch_) {
        return Error(&parsed,
                     "ST invalid in fetch phase, call UL and LK again first");
      }
      break;
    case kMicro_STP:
      if (!has_adr_) {
        return Error(&parsed, "STP without a prior ADR");
      }
      if (in_fetch_) {
        return Error(&parsed,
                     "STP invalid in fetch phase, call UL and LK again first");
      }
      break;
    case kMicro_PLK:
      CHECK(lock_type_ == LockType::kNone);  // Handled by InitLocks.
      lock_type_ = LockType::kPort;
      break;
    case kMicro_PUL:
      CHECK(lock_type_ == LockType::kPort);  // Handled by InitLocks.
      lock_type_ = LockType::kNone;
      break;
    case kMicro_PLD:
      if (lock_type_ != LockType::kPort) {
        return Error(&parsed, "PLD without a prior PLK");
      }
      break;
    case kMicro_PST:
      if (lock_type_ != LockType::kPort) {
        return Error(&parsed, "PST without a prior PLK");
      }
      break;
    case kMicro_WAIT:
      if (in_fetch_) {
        return Error(&parsed, "WAIT in fetch phase");
      } else if (lock_type_ == LockType::kMemory) {
        return Error(&parsed, "WAIT between LK and UL");
      } else if (lock_type_ == LockType::kPort) {
        return Error(&parsed, "WAIT between PLK and PUL");
      }
      CHECK(lock_type_ == LockType::kNone) << "Unhandled lock type";
      break;
    case kMicro_HALT:
      if (in_fetch_) {
        return Error(&parsed, "HALT in fetch phase");
      } else if (lock_type_ == LockType::kMemory) {
        return Error(&parsed, "HALT between LK and UL");
      } else if (lock_type_ == LockType::kPort) {
        return Error(&parsed, "HALT between PLK and PUL");
      }
      CHECK(lock_type_ == LockType::kNone) << "Unhandled lock type";
      break;
    default:
      break;
  }

  return true;
}

bool InstructionCompiler::ExtractLabels() {
  int index = 0;
  for (std::string_view& code : src_code_) {
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
      if (labels_.contains(label_key)) {
        return Error(absl::StrCat("Duplicate label: ", label));
      }
      labels_[label_key] = index;
    }
    ++index;
  }
  return true;
}

bool InstructionCompiler::InitLocks() {
  locks_.reserve(parsed_code_.size());
  static_assert(
      CpuCore::CODE < kFirstPortLock && CpuCore::STACK < kFirstPortLock &&
          CpuCore::DATA < kFirstPortLock && CpuCore::EXTRA < kFirstPortLock,
      "Memory locks must be lower that first port lock");
  int port_lock = kFirstPortLock;
  int lock = CpuCore::CODE;
  int index = 0;
  for (ParsedMicroCode& parsed : parsed_code_) {
    ++index;
    locks_.push_back(lock);
    if (parsed.op_name == "LK") {
      if (lock >= 0) {
        return Error(absl::StrCat("LK when prior ",
                                  (lock < kFirstPortLock ? "LK" : "PLK"),
                                  " is still locked. Code number: ", index));
      }
      int8_t arg;
      if (!DecodeArg(&parsed, index, parsed.arg1_name, MicroArgType::kBank,
                     arg)) {
        return false;
      }
      lock = arg;
    } else if (parsed.op_name == "UL") {
      if (lock < 0 || lock >= kFirstPortLock) {
        return Error(
            absl::StrCat("UL without a prior LK. Code number: ", index));
      }
      lock = -1;
    } else if (parsed.op_name == "PLK") {
      if (lock >= 0) {
        return Error(absl::StrCat("PLK when prior ",
                                  (lock < kFirstPortLock ? "LK" : "PLK"),
                                  " is still locked. Code number: ", index));
      }
      lock = port_lock++;
    } else if (parsed.op_name == "PUL") {
      if (lock < kFirstPortLock) {
        return Error(
            absl::StrCat("PUL without a prior PLK. Code number: ", index));
      }
      lock = -1;
    }
  }
  // We need a value for one past the end, since it is a valid jump address.
  locks_.push_back(lock);
  return true;
}

InstructionCompiler::ParsedMicroCode InstructionCompiler::ParseMicroCode(
    std::string_view code) {
  auto RemovePrefix = [&code](std::string_view::size_type amount) {
    code.remove_prefix(std::min(code.size(), amount));
  };

  ParsedMicroCode parsed = {};
  parsed.code = code;
  auto next_token = code.find_first_of(".(:");
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
      microcode_->op = def.op;
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
      if (!labels_.contains(label_key)) {
        return Error(parsed, absl::StrCat("Unknown label: ", label));
      }
      jump = labels_[label_key] - jump_from;
    }
    const int min_jump = std::max<int>(-128, -jump_from);
    const int max_jump =
        std::min<int>(127, static_cast<int>(parsed_code_.size()) - jump_from);
    if (jump < min_jump || jump > max_jump) {
      return Error(parsed, absl::StrCat("Value out of range [", min_jump, ",",
                                        max_jump, "]: ", arg_name));
    }
    if (locks_[index] != locks_[jump_from + jump]) {
      return Error(parsed, absl::StrCat("Jump between locks from ",
                                        LockName(locks_[index]), " to ",
                                        LockName(locks_[jump_from + jump])));
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
      if (instruction_.decl.arg1 != "a") {
        return Error(parsed, absl::StrCat("First argument not: a (it is \"",
                                          instruction_.decl.arg1, "\")"));
      }
      arg = -1;
    } else if (arg_name == "a0") {
      if (instruction_.decl.arg1 != "A") {
        return Error(parsed, absl::StrCat("First argument not: A (it is \"",
                                          instruction_.decl.arg1, "\")"));
      }
      arg = -1;
    } else if (arg_name == "a1") {
      if (instruction_.decl.arg1 != "A") {
        return Error(parsed, absl::StrCat("First argument not: A (it is \"",
                                          instruction_.decl.arg1, "\")"));
      }
      arg = -3;
    } else if (arg_name == "b") {
      if (instruction_.decl.arg2 != "b") {
        return Error(parsed, absl::StrCat("Second argument not: b (it is \"",
                                          instruction_.decl.arg2, "\")"));
      }
      arg = -2;
    } else if (arg_name == "b0") {
      if (instruction_.decl.arg2 != "B") {
        return Error(parsed, absl::StrCat("Second argument not: B (it is \"",
                                          instruction_.decl.arg2, "\")"));
      }
      arg = -2;
    } else if (arg_name == "b1") {
      if (instruction_.decl.arg2 != "B") {
        return Error(parsed, absl::StrCat("Second argument not: B (it is \"",
                                          instruction_.decl.arg2, "\")"));
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
    } else if (arg_name == "PC") {
      arg = CpuCore::PC;
    } else if (arg_name == "SP") {
      arg = CpuCore::SP;
    } else if (arg_name == "DP") {
      arg = CpuCore::DP;
    } else if (arg_name == "ST") {
      arg = CpuCore::ST;
    } else {
      return Error(parsed, absl::StrCat("Invalid word argument: ", arg_name));
    }
    return true;
  }
  if (arg_type == MicroArgType::kDwordReg) {
    if (arg_name == "A") {
      if (instruction_.decl.arg1 != arg_name) {
        return Error(parsed, absl::StrCat("First argument not: A (it is \"",
                                          instruction_.decl.arg1, "\")"));
      }
      arg = -1;
    } else if (arg_name == "B") {
      if (instruction_.decl.arg2 != arg_name) {
        return Error(parsed, absl::StrCat("Second argument not: B (it is \"",
                                          instruction_.decl.arg2, "\")"));
      }
      arg = -2;
    } else if (arg_name[0] == 'D' && arg_name.size() == 2) {
      arg = arg_name[1] - '0';
      if (arg < 0 || arg > 3) {
        return Error(parsed,
                     absl::StrCat("Invalid register index: ", arg_name));
      }
      arg = CpuCore::D0 + arg * 2;
    } else if (arg_name == "CD") {
      arg = CpuCore::CD;
    } else if (arg_name == "SD") {
      arg = CpuCore::SD;
    } else {
      return Error(parsed, absl::StrCat("Invalid dword argument: ", arg_name));
    }
    return true;
  }
  return Error(parsed, "Unhandled argument type!");
}

}  // namespace

InstructionMicrocodes::InstructionMicrocodes()
    : InstructionMicrocodes(kMicroCodeDefs) {}

InstructionMicrocodes::InstructionMicrocodes(
    absl::Span<const MicrocodeDef> microcode_defs)
    : microcode_defs_(microcode_defs), compiled_(256) {}

InstructionMicrocodes::~InstructionMicrocodes() = default;

bool InstructionMicrocodes::Compile(const InstructionDef& instruction,
                                    std::string* error_string) {
  int op_index = static_cast<int>(instruction.op);
  CompiledInstruction& compiled = compiled_[op_index];
  compiled.arg1 = ArgTypeBits(instruction.decl.arg1);
  compiled.arg2 = ArgTypeBits(instruction.decl.arg2);

  InstructionCompiler compiler(instruction, microcode_defs_, compiled,
                               error_string);
  if (!compiler.Compile()) {
    return false;
  }
  return true;
}

bool InstructionMicrocodes::Compile(
    absl::Span<const InstructionDef> instructions, std::string* error_string) {
  for (const InstructionDef& instruction : instructions) {
    if (!Compile(instruction, error_string)) {
      return false;
    }
  }
  return true;
}

bool InstructionMicrocodes::Decode(uint16_t instruction_code,
                                   DecodedInstruction& decoded) {
  int index = (instruction_code >> 8);
  CompiledInstruction& compiled = compiled_[index];
  if (compiled.code.empty()) {
    decoded = kNopDecoded;
    return false;
  }
  decoded = {.code = absl::MakeSpan(compiled.code), .size = compiled.size};

  if (compiled.arg1.type != ArgType::kNone) {
    uint16_t value = instruction_code & ((1 << compiled.arg1.size) - 1);
    instruction_code >>= compiled.arg1.size;
    if (compiled.arg1.type == ArgType::kWordReg) {
      decoded.r[0] = CpuCore::R0 + value;
    } else if (compiled.arg1.type == ArgType::kDwordReg) {
      decoded.r[0] = CpuCore::D0 + value * 2;
      decoded.r[2] = decoded.r[0] + 1;
    } else if (compiled.arg1.type == ArgType::kImmediate) {
      decoded.c[0] = value;
    }
  }
  if (compiled.arg2.type != ArgType::kNone) {
    uint16_t value = instruction_code & ((1 << compiled.arg2.size) - 1);
    if (compiled.arg2.type == ArgType::kWordReg) {
      decoded.r[1] = CpuCore::R0 + value;
    } else if (compiled.arg2.type == ArgType::kDwordReg) {
      decoded.r[1] = CpuCore::D0 + value * 2;
      decoded.r[2] = decoded.r[0] + 1;
    } else if (compiled.arg2.type == ArgType::kImmediate) {
      decoded.c[1] = value;
    }
  }

  return true;
}

}  // namespace oz3
