// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_INSTRUCTION_COMPILER_H_
#define OZ3_CORE_INSTRUCTION_COMPILER_H_

#include <memory>
#include <string>

#include "absl/types/span.h"
#include "oz3/core/instruction_set.h"
#include "oz3/core/microcode.h"

namespace oz3 {

// Compiles the provided instruction set definition into an instruction set.
//
// If an error occurs, this function returns an empty instruction set (all
// decode operations will result in a NOP), and it will set the error_string to
// a description of the error.
std::shared_ptr<const InstructionSet> CompileInstructionSet(
    InstructionSetDef instruction_set_def, std::string* error_string = nullptr,
    absl::Span<const MicrocodeDef> microcode_defs = GetMicrocodeDefs());

}  // namespace oz3

#endif  // OZ3_CORE_INSTRUCTION_COMPILER_H_
