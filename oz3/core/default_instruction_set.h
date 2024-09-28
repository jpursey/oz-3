// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_DEFAULT_INSTRUCTION_SET_H_
#define OZ3_CORE_DEFAULT_INSTRUCTION_SET_H_

#include <cstdint>
#include <memory>

#include "absl/types/span.h"
#include "oz3/core/instruction_set.h"

namespace oz3 {

// The list of opcodes for the default OZ-3 instruction set.
enum Op : uint8_t {
  kOp_NOP,
  kOp_HALT,
  kOp_WAIT,
};

// Returns the entire default instruction set for the OZ-3 CPU.
const InstructionSetDef& GetDefaultInstructionSetDef();

// Returns the default instruction set compiled into an InstructionSet.
std::shared_ptr<const InstructionSet> GetDefaultInstructionSet();

}  // namespace oz3

#endif  // OZ3_CORE_DEFAULT_INSTRUCTION_SET_H_
