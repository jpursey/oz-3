// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/default_instruction_set.h"

#include "absl/log/log.h"
#include "oz3/core/instruction_compiler.h"

namespace oz3 {

namespace {

const InstructionDef kDefaultInstructions[] = {
    {.op = kOp_NOP, .op_name = "NOP", .code = "UL;"},
    {.op = kOp_HALT,
     .op_name = "HALT",
     .code = "UL;"
             "HALT;"},
    {.op = kOp_WAIT,
     .op_name = "WAIT",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "WAIT(a);"},
};

const InstructionSetDef kDefaultInstructionSet = {
    .instructions = kDefaultInstructions,
};

}  // namespace

const InstructionSetDef& GetDefaultInstructionSetDef() {
  return kDefaultInstructionSet;
}

std::shared_ptr<const InstructionSet> GetDefaultInstructionSet() {
  static std::shared_ptr<const InstructionSet> instruction_set =
      CompileInstructionSet(kDefaultInstructionSet);
  return instruction_set;
}

}  // namespace oz3
