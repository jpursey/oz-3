// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_INSTRUCTION_H_
#define OZ3_CORE_INSTRUCTION_H_

#include <cstdint>
#include <string_view>

#include "absl/types/span.h"

namespace oz3 {

//------------------------------------------------------------------------------
// Source argument definitions for instructions.
//
// These are used to define the arguments that are encoded into the lower byte
// of an instruction. The first argument is encoded in the lower bits and the
// second argument is encoded in the next higher bits. For example:
//    0|r r r|# # # #
// This instruction has two arguments. The first argument is an immediate value
// 4 bits, and the second argument is a word register encoded in the next higher
// 3 bits. The remaining bits are set to zero.
//------------------------------------------------------------------------------

// The type of an argument stored in instruction code low byte or macro low
// bits.
enum class ArgType : uint8_t {
  // No argument (0 bits).
  kNone,

  // Immediate value argument (1-8 bits).
  //
  // In Microcode, these are loaded into C0 for the first argument, and C1 for
  // the second argument. As a macro argument, it is referred to as "i", which
  // will resolve to either C0 or C1 depending on which argument the macro was
  // for.
  kImmediate,

  // 16-bit register argument (3 bits).
  //
  // In microcode, the first argument is referred to as "a", and the second
  // argument is referred to as "b". As a macro argument, it is referred to as
  // "m", and as a macro parameter it is referred to as "p", and finally as a
  // macro return it is referred to as "r".
  //
  // Word registers are always 3 bits in size to refer to registers R0-R7.
  kWordReg,

  // 32-bit register argument (2 bits).
  //
  // In microcode, the first argument is referred to as "A"/"a0"/"a1", and the
  // second argument is referred to as "B"/"b0"/"b1". As a macro argument, it is
  // referred to as "M"/"m0"/"m1", and as a macro parameter it is referred to as
  // "P"/"p0"/"p1", and finally as a macro return it is referred to as
  // "R"/"r0"/"r1".
  kDwordReg,

  // Macro argument (1-8 bits).
  //
  // These are used to specify the number of bits used for an instruction macro.
  // Macros may also define an input argument (which can only be kWordReg or
  // kDwordReg), and they also may define a single internal argument (of any
  // type -- including another macro). There can only be one macro argument
  // at the instruction level, however.
  kMacro,
};

// Converts an argument type to a string for debugging purposes.
std::string_view ArgTypeToString(ArgType type);

// Returns the default size in bits for an argument type.
uint8_t GetDefaultArgTypeSize(ArgType type);

// Represents how an argument is encoded in the instruction's lower byte.
//
// Instructions arguments are filled low to high, with the remaining bits set to
// zero. For instance, if there are two arguments:
//   Argument 1: An immediate value that taked 4 bits (named '#4')
//   Argument 2: A word register that takes 3 bits (named 'r')
// Then the instruction encoding would be:
//   0|r r r|# # # #
struct Argument {
  Argument() = default;
  Argument(ArgType in_type)
      : type(in_type), size(GetDefaultArgTypeSize(in_type)) {}
  Argument(ArgType in_type, int in_size)
      : type(in_type), size(static_cast<uint8_t>(in_size)) {}

  ArgType type = ArgType::kNone;  // The type of argument.
  uint8_t size = 0;               // Number of bits the argument takes.
};
static_assert(sizeof(Argument) == 2);

//------------------------------------------------------------------------------
// Macro declarations and definitions.
//------------------------------------------------------------------------------

// Prefix bits for a macro. These are used to match a macro to an assembly
// source. The prefix must be unique within the set of macros.
struct MacroPrefix {
  // Value of the prefix bits.
  uint8_t value = 0;

  // Number of bits in the prefix.
  uint8_t size = 0;
};

// Definition of one of the code options for a macro.
struct MacroCodeDef {
  // The assembly source for the macro.
  //
  // This is only used by the OZ-3 assembler, and may be empty if not using it.
  //
  // This can be any unique string of ASCII non-control characters in the
  // range [32, 127] with the following restrictions:
  // - The string must be unique within the macro set.
  // - The string may contain space characters which will match one or more
  //   space characters in the assembly source.
  // - The string must not contain the following characters: ";", "#", ",", or
  //   "@". These are reserved by the assembler for comments, immediate values,
  //   parameter separators, and labels respectively.
  // - The string may contain "$r", "$R", or "$#1" to "$#8" which will match a
  //   word, dword, or immediate value respectively that can be referred to in
  //   the macro microcode as "m", "M"/"m0"/"m1", or "i". There may only be one
  //   of these in the string. The character "$" cannot otherwise appear in the
  //   string.
  // - If the string contains any parentheses, brackets, or braces, they must be
  //   balanced.
  std::string_view source;

  // A string of prefix bits which uniquely identify the macro. This is used to
  // match the macro to the assembly source. The prefix must be unique within
  // the set of macros. In addition prefix.size + arg.size must equal the
  // MacroDef size.
  MacroPrefix prefix;

  // Argument contained within the macro itself. This is used to specify the
  // number of bits the macro argument takes. The argument is referred to as
  // "m", "M"/"m0"/"m1", or "i" in the microcode source. This must match the
  // definition in the source. In addition prefix.size + arg.size must equal the
  // MacroDef size.
  Argument arg;

  // The return register index for the macro. This must be a register that
  // conforms to the return type of the macro. To return the macro parameter
  // itself, use -1. If the macro does not have a return type, this is ignored.
  int8_t ret = 0;

  // The microcode source for the macro.
  // See https://github.com/jpursey/oz-3/wiki/2.1-Microcode for valid syntax.
  std::string_view code;
};

// Definition of a microcode instruction macro for the OZ-3 CPU.
struct MacroDef {
  // The name of the macro. It should consist only of alphanumeric characters.
  // It is case-sensitive. This is referred to as "$name;" or "$name(arg);" in
  // the microcode to refer to a macro (depending on whether it takes an
  // argument or not).
  std::string_view name;

  // The type of the macro argument, if there is any. This is referred to as "p"
  // in the macro source if it is a word register, and "P"/"p0"/"p1" if it is a
  // dword register. Other argument types are not supported.
  ArgType arg = ArgType::kNone;

  // The return type of the macro. This is referred to as "r" in the instruction
  // source if it is a word register, and "R"/"r0"/"r1" if it is a dword
  // register. Other argument types are not supported.
  ArgType ret = ArgType::kNone;

  // The number of bits the macro argument takes.
  uint16_t size = 0;

  // The source code for the macro (up to 2^size entries).
  absl::Span<const MacroCodeDef> code;
};

//------------------------------------------------------------------------------
// Instruction declarations and definitions.
//------------------------------------------------------------------------------

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

  // OZ-3 assembly operation name. This does not need/ be unique with the
  // instruction op-code value itself. Multiple instructions can use the same
  // name like "MOV" as long as the argument types are sufficient to
  // disambiguate between them in the assembler.
  std::string_view op_name;

  // The argument definitions for the instruction that are encoded in the lower
  // byte of the instruction word. The total number of bits required must not
  // exceed a byte (8 bits).
  Argument arg1;  // First argument definition.
  Argument arg2;  // Second argument definition.

  // The microcode source for the instruction.
  std::string_view code;
};

//------------------------------------------------------------------------------
// InstructionSet definition.
//------------------------------------------------------------------------------

// Defines a set of instructions and macros for the OZ-3 CPU.
struct InstructionSetDef {
  // The list of all instruction definitions in the instruction set.
  absl::Span<const InstructionDef> instructions;

  // The list of all macro definitions in the instruction set.
  absl::Span<const MacroDef> macros;
};

}  // namespace oz3

#endif  // OZ3_CORE_INSTRUCTION_H_
