// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_set.h"

#include "absl/log/log.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/microcode.h"

namespace oz3 {

namespace {

const Microcode kNopMicroCode[] = {{.op = kMicro_UL}};
const DecodedInstruction kNopDecoded = {.code = kNopMicroCode, .size = 1};

}  // namespace

inline const InstructionSet::Instruction* InstructionSet::GetInstruction(
    int op) const {
  if (op >= 0 && op < instructions_.size()) {
    return &instructions_[op];
  }
  return nullptr;
}

inline absl::Span<const Microcode> InstructionSet::GetCode(
    const InstructionCode& code) const {
  DCHECK(code.code_start + code.code_size <= microcodes_.size());
  return absl::MakeSpan(microcodes_).subspan(code.code_start, code.code_size);
}

bool InstructionSet::Decode(uint16_t code_word,
                            DecodedInstruction& decoded) const {
  const int op = (code_word >> 8);
  const Instruction* instruction = GetInstruction(op);
  if (instruction == nullptr) {
    decoded = kNopDecoded;
    return false;
  }

  decoded = {};
  DecodeState state = {
      .decoded = decoded,
      .instruction = *instruction,
      .code = &instruction->code,
      .code_word = code_word,
  };

  if (DecodeArgument(state, instruction->arg1, 0)) {
    state.code_word >>= instruction->arg1.size;
    DecodeArgument(state, instruction->arg2, 1);
  }
  if (state.code->code_size == 0) {
    decoded = kNopDecoded;
    return false;
  }

  decoded.code = GetCode(*state.code);
  decoded.size = state.code->pc_size;
  return true;
}

int InstructionSet::GetTotalSizeInBytes() const {
  return static_cast<int>(instructions_.size() * sizeof(Instruction) +
                          sub_instructions_.size() * sizeof(SubInstruction) +
                          microcodes_.size() * sizeof(Microcode));
}

bool InstructionSet::DecodeArgument(DecodeState& state, const Argument& arg,
                                    int arg_index) const {
  DCHECK(arg.IsValid());
  const uint16_t arg_value = state.code_word & ((1 << arg.size) - 1);
  switch (arg.type) {
    case ArgType::kNone:
      return false;
    case ArgType::kMacro: {
      const SubInstruction& sub =
          sub_instructions_[state.instruction.sub_index + arg_value];
      state.code = &sub.code;
      DecodeArgument(state, sub.arg, arg_index);
    } break;
    case ArgType::kWordReg:
      state.decoded.r[arg_index] = CpuCore::R0 + arg_value;
      break;
    case ArgType::kDwordReg:
      state.decoded.r[arg_index] = CpuCore::D0 + arg_value * 2;
      state.decoded.r[arg_index + 2] = state.decoded.r[arg_index] + 1;
      break;
    case ArgType::kImmediate:
      state.decoded.c[arg_index] = arg_value;
      break;
  }
  return true;
}

}  // namespace oz3
