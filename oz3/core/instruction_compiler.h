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

// Instruction compiler of instruction definitions for the OZ-3 CPU.
//
// The CpuCore uses this to compile all instructions from the provided
// instruction set into microcode, and then use Decode on to get the decoded
// instruction and microcode to execute.
class InstructionCompiler final {
 public:
  // Construct with default OZ-3 microcode definitions.
  InstructionCompiler();

  // Construct with specific microcode definitions (for testing). This is
  // non-functional outside of tests, as the CpuCore expects the OZ-3 defined
  // microcode definitions.
  explicit InstructionCompiler(absl::Span<const MicrocodeDef> micro_code_defs);

  InstructionCompiler(const InstructionCompiler&) = delete;
  InstructionCompiler& operator=(const InstructionCompiler&) = delete;
  ~InstructionCompiler();

  // Compiles the provided macros into microcode.
  //
  // A macro must be successfully compiled before any instruction or other macro
  // that uses it can be compiled.
  bool Compile(absl::Span<const MacroDef> macro_defs,
               std::string* error_string = nullptr);
  bool Compile(const MacroDef& macro_def, std::string* error_string = nullptr);

  // Compiles the provided instructions into microcode.
  //
  // Returns true if the instructions were all successfully compiled, and false
  // if there is an error in at least one of the Instruction source definition.
  // The `error_string` is set to a description of the error if it is not null.
  bool Compile(absl::Span<const InstructionDef> instruction_defs,
               std::string* error_string = nullptr);
  bool Compile(const InstructionDef& instruction_def,
               std::string* error_string = nullptr);

  // Returns the compiled instruction set.
  InstructionSet CreateInstructionSet() const;

  // Returns the compiled instruction set and invalidates the compiler.
  InstructionSet ToInstructionSet() &&;

 private:
  class Impl;

  using Instruction = microcode_internal::Instruction;
  using SubInstruction = microcode_internal::SubInstruction;
  using InstructionCode = microcode_internal::InstructionCode;

  std::unique_ptr<Impl> impl_;
};

}  // namespace oz3

#endif  // OZ3_CORE_INSTRUCTION_COMPILER_H_
