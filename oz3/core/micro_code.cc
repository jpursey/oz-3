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

const MicroCode kNopMicroCode[] = {{.op = MicroOp::UL, .bank = CpuCore::CODE}};
const DecodedInstruction kNopDecoded = {.code = kNopMicroCode};

const MicroCodeDef kMicroCodeDefs[] = {
    {MicroOp::LK, "LK", true, ArgType::kNone, ArgType::kNone},
    {MicroOp::UL, "UL", true, ArgType::kNone, ArgType::kNone},
    {MicroOp::WAIT, "WAIT", false, ArgType::kWordRegister, ArgType::kNone},
    {MicroOp::HALT, "HALT", false, ArgType::kNone, ArgType::kNone},
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

  bool Error(absl::string_view message) {
    if (error_string_ != nullptr) {
      *error_string_ =
          absl::StrCat("Error in \"", parsed_.code, "\": ", message);
    }
    return false;
  }

  bool CompileMicroCode(absl::string_view micro_src_code);
  void ParseMicroCode(std::string_view code);
  const MicroCodeDef* FindMicroCodeDef(std::string_view op_name);
  bool DecodeBank(const MicroCodeDef* def, absl::string_view bank);
  bool DecodeArg(std::string_view arg_name, ArgType arg_type, int8_t& arg);
  bool DecodeZsco(std::string_view zsco);

  const InstructionDef& instruction_;
  absl::Span<const MicroCodeDef> micro_code_defs_;
  CompiledInstruction& compiled_;
  std::string* const error_string_;
  ParsedMicroCode parsed_ = {};
  MicroCode* micro_code_ = nullptr;
};

bool InstructionCompiler::Compile() {
  compiled_.code.clear();
  std::vector<std::string_view> src_code =
      absl::StrSplit(instruction_.code, ';', absl::SkipWhitespace());
  for (auto& micro_src_code : src_code) {
    micro_code_ = &compiled_.code.emplace_back();
    if (!CompileMicroCode(micro_src_code)) {
      compiled_.code.clear();
      return false;
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
  if (!DecodeBank(def, parsed_.bank)) {
    return false;
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

bool InstructionCompiler::DecodeBank(const MicroCodeDef* def,
                                     absl::string_view bank) {
  if (def->has_bank) {
    if (bank.empty()) {
      return Error(absl::StrCat("Operation ", def->op_name,
                                " requires a bank specification"));
    }
    if (bank == "C") {
      micro_code_->bank = CpuCore::CODE;
    } else if (bank == "S") {
      micro_code_->bank = CpuCore::STACK;
    } else if (bank == "D") {
      micro_code_->bank = CpuCore::DATA;
    } else if (bank == "E") {
      micro_code_->bank = CpuCore::EXTRA;
    } else {
      return Error(absl::StrCat("Invalid bank specification: ", bank));
    }
  } else if (!bank.empty()) {
    return Error(absl::StrCat("Operation ", def->op_name,
                              " does not take a bank specification"));
  }
  return true;
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
  if (arg_type == ArgType::kWordRegister) {
    if (arg_name == "a") {
      if (compiled_.arg1.type != ArgType::kWordRegister ||
          instruction_.decl.arg1 != arg_name) {
        return Error(absl::StrCat("Invalid argument: ", arg_name));
      }
      arg = -1;
    } else if (arg_name == "b") {
      if (compiled_.arg2.type != ArgType::kWordRegister ||
          instruction_.decl.arg2 != arg_name) {
        return Error(absl::StrCat("Invalid argument: ", arg_name));
      }
      arg = -2;
    } else if (arg_name[0] == 'R' && arg_name.size() == 2) {
      arg = arg_name[1] - '0';
      if (arg < 0 || arg > 7) {
        return Error(absl::StrCat("Invalid argument: ", arg_name));
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
      return Error(absl::StrCat("Invalid argument: ", arg_name));
    }
    return true;
  }
  if (arg_type == ArgType::kDwordRegister) {
    if (arg_name == "A") {
      if (compiled_.arg1.type != ArgType::kDwordRegister ||
          instruction_.decl.arg1 != arg_name) {
        return Error(absl::StrCat("Invalid argument: ", arg_name));
      }
      arg = -1;
    } else if (arg_name == "B") {
      if (compiled_.arg2.type != ArgType::kDwordRegister ||
          instruction_.decl.arg2 != arg_name) {
        return Error(absl::StrCat("Invalid argument: ", arg_name));
      }
      arg = -2;
    } else if (arg_name[0] == 'D' && arg_name.size() == 2) {
      arg = arg_name[1] - '0';
      if (arg < 0 || arg > 3) {
        return Error(absl::StrCat("Invalid argument: ", arg_name));
      }
      arg = CpuCore::D0 + arg * 2;
    } else if (arg_name == "CD") {
      arg = CpuCore::CD;
    } else if (arg_name == "SD") {
      arg = CpuCore::SD;
    } else {
      return Error(absl::StrCat("Invalid argument: ", arg_name));
    }
    return true;
  }
  if (arg_type == ArgType::kImmediate) {
    int value;
    if (!absl::SimpleAtoi(arg_name, &value)) {
      return Error(absl::StrCat("Invalid argument: ", arg_name));
    }
    if (value < -128 || value > 127) {
      return Error(absl::StrCat("Value out of range: ", arg_name));
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

bool InstructionMicroCodes::Compile(
    const InstructionDef& instruction, std::string* error_string) {
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
  decoded = {};

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
    instruction_code >>= compiled.arg2.size;
    if (compiled.arg2.type == ArgType::kWordRegister) {
      decoded.r[1] = CpuCore::R0 + value;
    } else if (compiled.arg2.type == ArgType::kDwordRegister) {
      decoded.r[1] = CpuCore::D0 + value * 2;
    } else if (compiled.arg2.type == ArgType::kImmediate) {
      decoded.c[1] = value;
    }
  }

  decoded.code = absl::MakeSpan(compiled.code);
  return true;
}

}  // namespace oz3
