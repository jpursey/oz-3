// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/default_instruction_set.h"

namespace oz3 {

namespace {

const InstructionDef kDefaultInstructionSet[] = {
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

}  // namespace

absl::Span<const InstructionDef> GetDefaultInstructionSet() {
  return kDefaultInstructionSet;
}

}  // namespace oz3
