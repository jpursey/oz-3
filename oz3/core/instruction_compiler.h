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

//==============================================================================
// Internal constants and helperss used during compilation
//==============================================================================

namespace compiler_internal {

// Instruction word reg arguments.
inline constexpr int8_t kArg_a = CpuCore::A0;
inline constexpr int8_t kArg_a0 = CpuCore::A0;
inline constexpr int8_t kArg_a1 = CpuCore::A1;
inline constexpr int8_t kArg_b = CpuCore::B0;
inline constexpr int8_t kArg_b0 = CpuCore::B0;
inline constexpr int8_t kArg_b1 = CpuCore::B1;
inline constexpr int8_t kInstructionMinWordRegArg =
    -static_cast<uint8_t>(ABSL_ARRAYSIZE(DecodedInstruction::r));

// Instruction dword reg arguments.
inline constexpr int8_t kArg_A = kArg_a;
inline constexpr int8_t kArg_B = kArg_b;
static_assert(kArg_B == kArg_A - 1, "Required offset to convert macro args");
inline constexpr int8_t kInstructionMinDwordRegArg = std::min(kArg_A, kArg_B);

// Minimum value for instruction arguments.
inline constexpr int8_t kInstructionMinArg = kInstructionMinWordRegArg;

// Macro parameter arguments
static_assert(kInstructionMinArg - 1 == CpuCore::P0 &&
              kInstructionMinArg - 2 == CpuCore::P1);
inline constexpr int8_t kArg_p = CpuCore::P;
inline constexpr int8_t kArg_p0 = CpuCore::P0;
inline constexpr int8_t kArg_p1 = CpuCore::P1;
inline constexpr int8_t kArg_P = CpuCore::P;

// Macro return arguments
inline constexpr int8_t kArg_r = kInstructionMinArg - 3;
inline constexpr int8_t kArg_r0 = kArg_r;
inline constexpr int8_t kArg_r1 = kInstructionMinArg - 4;
inline constexpr int8_t kArg_R = kArg_r;

// Offset to convert macro arguments to instruction arguments.
inline constexpr int8_t kMacroArgOffset = CpuCore::kRegisterCount + 16;
static_assert(CpuCore::kRegisterCount - kMacroArgOffset <
              kInstructionMinArg - 5);
inline constexpr int8_t MacroArgValue(int8_t value) {
  return value - kMacroArgOffset;
}

// Macro arguments.
inline constexpr int8_t kArg_i = MacroArgValue(CpuCore::C0);
inline constexpr int8_t kArg_M = MacroArgValue(kArg_A);
inline constexpr int8_t kArg_m = MacroArgValue(kArg_a);
inline constexpr int8_t kArg_m0 = MacroArgValue(kArg_a0);
inline constexpr int8_t kArg_m1 = MacroArgValue(kArg_a1);

inline constexpr int8_t kMinArgValue = MacroArgValue(kInstructionMinArg);

// Returns the name of the register argument, including the intermediate ones
// used during microcode compiling.
std::string_view GetWordRegArgName(int8_t arg);

// Returns the name of the dword register argument, including the intermediate
// ones used during microcode compiling.
std::string_view GetDwordRegArgName(int8_t arg);

}  // namespace compiler_internal

}  // namespace oz3

#endif  // OZ3_CORE_INSTRUCTION_COMPILER_H_
