// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/default_instruction_set.h"

#include "absl/log/log.h"
#include "oz3/core/instruction_compiler.h"

namespace oz3 {

namespace {

constexpr oz3::InstructionDef kInstructions[] = {
    {.op = 0, .op_name = "NOP", .code = "UL;"},
    {.op = 1, .op_name = "HALT", .code = "UL;HALT;"},
    {.op = 2,
     .op_name = "WAIT",
     .arg_source = "$r",
     .arg1 = {oz3::ArgType::kWordReg, 3},
     .code = "UL;WAIT(a);"},
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
