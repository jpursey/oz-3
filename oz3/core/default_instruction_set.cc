// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/default_instruction_set.h"

#include "absl/log/log.h"
#include "oz3/core/instruction_compiler.h"

namespace oz3 {

namespace {

// This switch is used so bugs can be fixed in the instruction set exporter.
// Otherwise, it will fail to build if the instruction set was invalid (due to
// a bug).
#if 1
#include "oz3/core/default_instruction_set.inc"
#else
constexpr InstructionDef kInstructions[] = {
    {.op = 0, .op_name = "NOP", .code = "UL;"},
};

constexpr oz3::InstructionSetDef kInstructionSet = {
    .instructions = kInstructions,
};
#endif
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
