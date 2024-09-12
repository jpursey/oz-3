// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_INSTRUCTIONS_H_
#define OZ3_CORE_INSTRUCTIONS_H_

#include <cstdint>
#include <string_view>

#include "absl/types/span.h"

namespace oz3 {

// The Upper 8 bits of the instruction is the operation code.
enum Op : uint8_t {
  kOp_NOP,
  kOp_HALT,
  kOp_WAIT,
};

// Source argument definitions for instructions.
inline constexpr std::string_view kArgNone = "";
inline constexpr std::string_view kArgWordRegA = "a";
inline constexpr std::string_view kArgWordRegB = "b";
inline constexpr std::string_view kArgDwordRegA = "A";
inline constexpr std::string_view kArgDwordRegB = "B";
inline constexpr std::string_view kArgWordValue = "v";
inline constexpr std::string_view kArgDwordValue = "V";
inline constexpr std::string_view kArgImmValue1 = "#1";
inline constexpr std::string_view kArgImmValue2 = "#2";
inline constexpr std::string_view kArgImmValue3 = "#3";
inline constexpr std::string_view kArgImmValue4 = "#4";
inline constexpr std::string_view kArgImmValue5 = "#5";
inline constexpr std::string_view kArgImmValue6 = "#6";
inline constexpr std::string_view kArgImmValue7 = "#7";
inline constexpr std::string_view kArgImmValue8 = "#8";

// The type of an argument stored in instruction code low byte.
enum class ArgType {
  kNone,           // No argument.
  kBank,           // Memory bank index (2 bits).
  kZsco,           // ZSCO flags (4 bits).
  kImmediate,      // Immediate value (1-8 bits).
  kWordRegister,   // Word register (3 bits).
  kDwordRegister,  // Dword register (3 bits).
};

// Represents how an argument is encoded in the instruction's lower byte.
//
// Instructions arguments are filled low to high, with the remaining bits set to
// zero. For instance, if there are two arguments:
//   Argument 1: An immediate value that taked 4 bits (named '#4')
//   Argument 2: A word register that takes 3 bits (named 'b')
// Then the instruction encoding would be:
//   0|b b b|# # # #
struct ArgTypeBits {
  ArgTypeBits() = default;
  ArgTypeBits(ArgType in_type, int in_size) : type(in_type), size(in_size) {}

  // Constructs an ArgTypeBits from a valid source argument definition.
  explicit ArgTypeBits(std::string_view arg_def);

  ArgType type = ArgType::kNone;  // The type of argument.
  int size = 0;                   // Number of bits the argument takes.
};

// Textual definition of the declaration for an instruction. This is used
// both for generating documentation and for compiling microcode source.
struct InstructionDecl {
  std::string_view op_name;  // OZ-3 assembly operation name.
  std::string_view arg1;     // First argument definition.
  std::string_view arg2;     // Second argument definition.
  std::string_view zsco;     // ZSCO flags affected by the instruction.
};

// Represents the full source definition of an instruction in the OZ-3 CPU.
struct InstructionDef {
  // Encodes the instruction into a 16-bit instruction word.
  //
  // The parameters match the first and second parameters as specified in the
  // decl.
  uint16_t Encode(uint16_t a = 0, uint16_t b = 0) const;

  // The numeric operation code for the instruction (upper 8 bits of the 16-bit
  // instruction).
  uint8_t op;

  // The textual definition of the source operands for the instruction.
  InstructionDecl decl;

  // The microcode source for the instruction.
  std::string_view code;
};

// Returns the entire instruction set for the OZ-3 CPU.
absl::Span<const InstructionDef> GetInstructionSet();

// Converts an argument

}  // namespace oz3

#endif  // OZ3_CORE_INSTRUCTIONS_H_
