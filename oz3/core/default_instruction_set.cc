// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/default_instruction_set.h"

#include "absl/log/log.h"
#include "oz3/core/instruction_compiler.h"

namespace oz3 {

namespace {

// #include "oz3/core/default_instruction_set.inc"

constexpr InstructionCodeDef kInstructionCode_NOP[] = {
    {.prefix = {0, 8}, .code = "UL;"},
};

constexpr InstructionCodeDef kInstructionCode_HALT[] = {
    {.prefix = {0, 8}, .code = "UL; HALT;"},
};

constexpr InstructionCodeDef kInstructionCode_WAIT[] = {
    {.source = "$r",
     .prefix = {0, 8},
     .arg1 = {ArgType::kWordReg, 3},
     .code = "UL; WAIT a;"},
};

constexpr InstructionDef kInstructions[] = {
    {.op = 0, .op_name = "NOP", .code = kInstructionCode_NOP},
    {.op = 1, .op_name = "HALT", .code = kInstructionCode_HALT},
    {.op = 2, .op_name = "WAIT", .code = kInstructionCode_WAIT},
};

constexpr oz3::InstructionSetDef kInstructionSet = {
    .instructions = kInstructions,
};

}  // namespace

const InstructionSetDef& GetDefaultInstructionSetDef() {
  return kInstructionSet;
}

std::shared_ptr<const InstructionSet> GetDefaultInstructionSet() {
  static std::shared_ptr<const InstructionSet> instruction_set =
      CompileInstructionSet(kInstructionSet);
  return instruction_set;
}

}  // namespace oz3
