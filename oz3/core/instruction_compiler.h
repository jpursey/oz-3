// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_INSTRUCTION_COMPILER_H_
#define OZ3_CORE_INSTRUCTION_COMPILER_H_

#include <memory>
#include <string>

#include "absl/types/span.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_set.h"
#include "oz3/core/microcode.h"

namespace oz3 {

//==============================================================================
// CompileInstructionSet
//==============================================================================

// Structured data for an error that occurred during instruction set
// compilation.
struct InstructionError {
  enum class Def {
    kNone,
    kMacro,
    kInstruction,
  };

  // Full error message.
  std::string message;

  // Instruction or Macro definition where the error occurred.
  Def def = Def::kNone;

  // Name of the instruction or macro where the error occurred.
  std::string def_name;

  // The source string of the MacroCodeDef where the error occurred. This is
  // only set if def is Def::kMacro.
  std::string macro_source_name;

  // Index into the instruction or macro code where the error occurred.
  int code_index = -1;

  // Index into the macro code expanded into the instruction where the error
  // occured. This is only set if def is Def::kInstruction and the error occured
  // inside the macro code in the context of the instruction.
  int sub_code_index = -1;
};

// Compiles the provided instruction set definition into an instruction set.
//
// If an error occurs, this function returns an empty instruction set (all
// decode operations will result in a NOP), and it will set the error to a
// description of the error.
std::shared_ptr<const InstructionSet> CompileInstructionSet(
    InstructionSetDef instruction_set_def, InstructionError* error = nullptr,
    absl::Span<const MicrocodeDef> microcode_defs = GetMicrocodeDefs());

}  // namespace oz3

#endif  // OZ3_CORE_INSTRUCTION_COMPILER_H_
