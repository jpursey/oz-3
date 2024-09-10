// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/micro_code.h"

#include <algorithm>
#include <optional>
#include <string>

#include "absl/strings/numbers.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_split.h"
#include "absl/types/span.h"
#include "oz3/core/cpu_core.h"

namespace oz3 {

namespace {

const MicroCode kNopMicroCode[] = {{.op = kMicro_UL}};
const DecodedInstruction kNopDecoded = {.code = kNopMicroCode, .size = 1};

const MicroCodeDef kMicroCodeDefs[] = {
    {kMicro_WAIT, "WAIT", ArgType::kWordRegister, ArgType::kNone},
    {kMicro_HALT, "HALT", ArgType::kNone, ArgType::kNone},
    {kMicro_LK, "LK", ArgType::kBank, ArgType::kNone},
    {kMicro_UL, "UL", ArgType::kNone, ArgType::kNone},
    {kMicro_ADR, "ADR", ArgType::kWordRegister, ArgType::kNone},
    {kMicro_LD, "LD", ArgType::kWordRegister, ArgType::kNone},
    {kMicro_ST, "ST", ArgType::kWordRegister, ArgType::kNone},
    {kMicro_MOV, "MOV", ArgType::kWordRegister, ArgType::kWordRegister},
};

class InstructionCompiler {
 public:
  InstructionCompiler(const InstructionDef& instruction,
                      absl::Span<const MicroCodeDef> micro_code_defs,
                      CompiledInstruction& compiled, std::string* error_string)
      : instruction_(instruction),
        micro_code_defs_(micro_code_defs),
        compiled_(compiled),
        error_string_(error_string) {}

  bool Compile();

 private:
  struct ParsedMicroCode {
    std::string_view code;       // Full code that was parsed.
    std::string_view op_name;    // Operation name.
    std::string_view bank;       // Bank specification.
    std::string_view arg1_name;  // First argument name.
    std::string_view arg2_name;  // Second argument name.
    std::string_view zsco;       // ZSCO flags.
  };

  bool Error(absl::string_view message, bool include_micro = true) {
    if (error_string_ != nullptr) {
      if (include_micro) {
        *error_string_ = absl::StrCat("Error in ", instruction_.decl.op_name,
                                      " for \"", parsed_.code, "\": ", message);
      } else {
        *error_string_ =
            absl::StrCat("Error in ", instruction_.decl.op_name, ": ", message);
      }
    }
    return false;
  }

  bool CompileMicroCode(absl::string_view micro_src_code);
  void ParseMicroCode(std::string_view code);
  const MicroCodeDef* FindMicroCodeDef(std::string_view op_name);
  bool DecodeArg(std::string_view arg_name, ArgType arg_type, int8_t& arg);
  bool DecodeZsco(std::string_view zsco);

  const InstructionDef& instruction_;
  absl::Span<const MicroCodeDef> micro_code_defs_;
  CompiledInstruction& compiled_;
  std::string* const error_string_;
  ParsedMicroCode parsed_ = {};
  MicroCode* micro_code_ = nullptr;

  // Set to the bank of the last LK operation. Cleared by UL for that bank.
  int lk_bank_ = CpuCore::CODE;

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
  std::vector<std::string_view> src_code =
      absl::StrSplit(instruction_.code, ';', absl::SkipWhitespace());
  for (auto& micro_src_code : src_code) {
    micro_code_ = &compiled_.code.emplace_back();
    if (!CompileMicroCode(micro_src_code)) {
      compiled_.code.clear();
      return false;
    }
  }
  if (lk_bank_ >= 0) {
    if (had_lk_) {
      return Error("LK not cleared by UL", false);
    } else {
      return Error("No UL for instruction fetch", false);
    }
  }
  return true;
}

bool InstructionCompiler::CompileMicroCode(absl::string_view micro_src_code) {
  ParseMicroCode(micro_src_code);
  const MicroCodeDef* def = FindMicroCodeDef(parsed_.op_name);
  if (def == nullptr) {
    return Error("MicroCode OpCode not found");
  }
  if (!DecodeArg(parsed_.arg1_name, def->arg1, micro_code_->arg1)) {
    return false;
  }
  if (!DecodeArg(parsed_.arg2_name, def->arg2, micro_code_->arg2)) {
    return false;
  }
  if (!DecodeZsco(parsed_.zsco)) {
    return false;
  }

  // Microcode specific handling.
  switch (def->op) {
    case kMicro_LK:
      if (lk_bank_ >= 0) {
        return Error("LK already active");
      }
      had_lk_ = true;
      lk_bank_ = micro_code_->arg1;
      break;
    case kMicro_UL:
      if (lk_bank_ < 0) {
        return Error("UL bank without a prior LK");
      }
      lk_bank_ = -1;
      has_adr_ = false;
      in_fetch_ = false;
      break;
    case kMicro_ADR:
      if (lk_bank_ < 0) {
        return Error("ADR without a prior LK");
      }
      has_adr_ = true;
      in_fetch_ = false;
      break;
    case kMicro_LD:
      if (!has_adr_) {
        return Error("LD without a prior ADR");
      }
      if (in_fetch_) {
        compiled_.size += 1;
      }
      break;
    case kMicro_ST:
      if (!has_adr_) {
        return Error("ST without a prior ADR");
      }
      if (in_fetch_) {
        return Error("ST invalid in fetch phase, call ADR first");
      }
      break;
    default:
      break;
  }

  return true;
}

void InstructionCompiler::ParseMicroCode(std::string_view code) {
  auto RemovePrefix = [&code](std::string_view::size_type amount) {
    code.remove_prefix(std::min(code.size(), amount));
  };

  parsed_ = {};
  parsed_.code = code;
  auto next_token = code.find_first_of(".(:");
  parsed_.op_name = code.substr(0, next_token);
  RemovePrefix(next_token);
  if (!code.empty() && code[0] == '.') {
    RemovePrefix(1);
    next_token = code.find_first_of("(:");
    parsed_.bank = code.substr(0, next_token);
    RemovePrefix(next_token);
  }
  if (!code.empty() && code[0] == '(') {
    RemovePrefix(1);
    next_token = code.find_first_of(",)");
    parsed_.arg1_name = code.substr(0, next_token);
    RemovePrefix(next_token);
    if (!code.empty() && code[0] == ',') {
      RemovePrefix(1);
      next_token = code.find(")");
      parsed_.arg2_name = code.substr(0, next_token);
      RemovePrefix(next_token);
    }
  }
  if (!code.empty() && code[0] == ':') {
    RemovePrefix(1);
    parsed_.zsco = code.substr(0);
  }
}

const MicroCodeDef* InstructionCompiler::FindMicroCodeDef(
    std::string_view op_name) {
  for (const MicroCodeDef& def : micro_code_defs_) {
    if (def.op_name == op_name) {
      micro_code_->op = def.op;
      return &def;
    }
  }
  return nullptr;
}

bool InstructionCompiler::DecodeArg(std::string_view arg_name, ArgType arg_type,
                                    int8_t& arg) {
  if (arg_name.empty() && arg_type == ArgType::kNone) {
    return true;
  }
  if (arg_name.empty()) {
    return Error("Argument missing");
  }
  if (arg_type == ArgType::kNone) {
    return Error("Argument not allowed");
  }
  if (arg_type == ArgType::kBank) {
    if (arg_name == "CODE") {
      arg = CpuCore::CODE;
    } else if (arg_name == "STACK") {
      arg = CpuCore::STACK;
    } else if (arg_name == "DATA") {
      arg = CpuCore::DATA;
    } else if (arg_name == "EXTRA") {
      arg = CpuCore::EXTRA;
    } else {
      return Error(absl::StrCat("Invalid bank: ", arg_name));
    }
    return true;
  }
  if (arg_type == ArgType::kWordRegister) {
    if (arg_name == "a") {
      if (instruction_.decl.arg1 != arg_name) {
        return Error(absl::StrCat("Argument not declared: ", arg_name));
      }
      if (compiled_.arg1.type != ArgType::kWordRegister) {
        return Error(absl::StrCat("Invalid argument type: ", arg_name));
      }
      arg = -1;
    } else if (arg_name == "b") {
      if (instruction_.decl.arg2 != arg_name) {
        return Error(absl::StrCat("Argument not declared: ", arg_name));
      }
      if (compiled_.arg2.type != ArgType::kWordRegister) {
        return Error(absl::StrCat("Invalid argument type: ", arg_name));
      }
      arg = -2;
    } else if (arg_name[0] == 'R' && arg_name.size() == 2) {
      arg = arg_name[1] - '0';
      if (arg < 0 || arg > 7) {
        return Error(absl::StrCat("Invalid register index: ", arg_name));
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
      return Error(absl::StrCat("Invalid word argument: ", arg_name));
    }
    return true;
  }
  if (arg_type == ArgType::kDwordRegister) {
    if (arg_name == "A") {
      if (instruction_.decl.arg1 != arg_name) {
        return Error(absl::StrCat("Argument not declared: ", arg_name));
      }
      if (compiled_.arg1.type != ArgType::kDwordRegister) {
        return Error(absl::StrCat("Invalid argument type: ", arg_name));
      }
      arg = -1;
    } else if (arg_name == "B") {
      if (instruction_.decl.arg2 != arg_name) {
        return Error(absl::StrCat("Argument not declared: ", arg_name));
      }
      if (compiled_.arg2.type != ArgType::kDwordRegister) {
        return Error(absl::StrCat("Invalid argument type: ", arg_name));
      }
      arg = -2;
    } else if (arg_name[0] == 'D' && arg_name.size() == 2) {
      arg = arg_name[1] - '0';
      if (arg < 0 || arg > 3) {
        return Error(absl::StrCat("Invalid register index: ", arg_name));
      }
      arg = CpuCore::D0 + arg * 2;
    } else if (arg_name == "CD") {
      arg = CpuCore::CD;
    } else if (arg_name == "SD") {
      arg = CpuCore::SD;
    } else {
      return Error(absl::StrCat("Invalid dword argument: ", arg_name));
    }
    return true;
  }
  if (arg_type == ArgType::kImmediate) {
    int value;
    if (!absl::SimpleAtoi(arg_name, &value)) {
      return Error(absl::StrCat("Invalid argument: ", arg_name));
    }
    if (value < -128 || value > 127) {
      return Error(absl::StrCat("Value out of range [-128,127]: ", arg_name));
    }
    arg = value;
    return true;
  }
  return Error("Unhandled argument type!");
}

bool InstructionCompiler::DecodeZsco(std::string_view zsco) {
  if (zsco.starts_with("z")) {
    micro_code_->st_clear |= CpuCore::Z;
    zsco.remove_prefix(1);
  }
  if (zsco.starts_with("s")) {
    micro_code_->st_clear |= CpuCore::S;
    zsco.remove_prefix(1);
  }
  if (zsco.starts_with("c")) {
    micro_code_->st_clear |= CpuCore::C;
    zsco.remove_prefix(1);
  }
  if (zsco.starts_with("o")) {
    micro_code_->st_clear |= CpuCore::O;
    zsco.remove_prefix(1);
  }
  if (zsco.starts_with("Z")) {
    micro_code_->st_set |= CpuCore::Z;
    zsco.remove_prefix(1);
  }
  if (zsco.starts_with("S")) {
    micro_code_->st_set |= CpuCore::S;
    zsco.remove_prefix(1);
  }
  if (zsco.starts_with("C")) {
    micro_code_->st_set |= CpuCore::C;
    zsco.remove_prefix(1);
  }
  if (zsco.starts_with("O")) {
    micro_code_->st_set |= CpuCore::O;
    zsco.remove_prefix(1);
  }
  if (!zsco.empty()) {
    return Error(absl::StrCat("Invalid ZSCO flags: ", zsco));
  }
  return true;
}

}  // namespace

InstructionMicroCodes::InstructionMicroCodes()
    : InstructionMicroCodes(kMicroCodeDefs) {}

InstructionMicroCodes::InstructionMicroCodes(
    absl::Span<const MicroCodeDef> micro_code_defs)
    : micro_code_defs_(micro_code_defs), compiled_(256) {}

InstructionMicroCodes::~InstructionMicroCodes() = default;

bool InstructionMicroCodes::Compile(const InstructionDef& instruction,
                                    std::string* error_string) {
  int op_index = static_cast<int>(instruction.op);
  CompiledInstruction& compiled = compiled_[op_index];
  compiled.arg1 = ArgTypeBits(instruction.decl.arg1);
  compiled.arg2 = ArgTypeBits(instruction.decl.arg2);

  InstructionCompiler compiler(instruction, micro_code_defs_, compiled,
                               error_string);
  if (!compiler.Compile()) {
    return false;
  }
  return true;
}

bool InstructionMicroCodes::Compile(
    absl::Span<const InstructionDef> instructions, std::string* error_string) {
  for (const InstructionDef& instruction : instructions) {
    if (!Compile(instruction, error_string)) {
      return false;
    }
  }
  return true;
}

bool InstructionMicroCodes::Decode(uint16_t instruction_code,
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
    if (compiled.arg1.type == ArgType::kWordRegister) {
      decoded.r[0] = CpuCore::R0 + value;
    } else if (compiled.arg1.type == ArgType::kDwordRegister) {
      decoded.r[0] = CpuCore::D0 + value * 2;
    } else if (compiled.arg1.type == ArgType::kImmediate) {
      decoded.c[0] = value;
    }
  }
  if (compiled.arg2.type != ArgType::kNone) {
    uint16_t value = instruction_code & ((1 << compiled.arg2.size) - 1);
    if (compiled.arg2.type == ArgType::kWordRegister) {
      decoded.r[1] = CpuCore::R0 + value;
    } else if (compiled.arg2.type == ArgType::kDwordRegister) {
      decoded.r[1] = CpuCore::D0 + value * 2;
    } else if (compiled.arg2.type == ArgType::kImmediate) {
      decoded.c[1] = value;
    }
  }

  return true;
}

}  // namespace oz3
