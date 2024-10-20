// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_MICROCODE_H_
#define OZ3_CORE_MICROCODE_H_

#include <cstdint>
#include <ostream>
#include <string>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "oz3/core/core_types.h"
#include "oz3/core/instruction_def.h"

namespace oz3 {

// The maximum number of microcode operations that can be defined for a single
// instruction in the OZ-3.
inline constexpr int kMaxInstructionMicrocodes = 255;

// The maximum number of microcode operations that can be defined for a single
// instruction set in the OZ-3.
inline constexpr int kMaxInstructionSetMicrocodes =
    kMaxInstructionMicrocodes * 256;

// The folowing lists all the microcode operations that can be executed by the
// OZ-3 CpuCore. For details see the OZ-3 Wiki:
// https://github.com/jpursey/oz-3/wiki/2.1-Microcode
enum MicroOp : uint8_t {
  kMicro_MSC,   // MSC(s);
  kMicro_MSS,   // MSS(s);
  kMicro_MSX,   // MSX(s);
  kMicro_MSM,   // MSM(s,r);
  kMicro_MSR,   // MSR(s,r);
  kMicro_WAIT,  // WAIT(r);
  kMicro_HALT,  // HALT;
  kMicro_LK,    // LK(b);
  kMicro_LKR,   // LKR(r);
  kMicro_UL,    // UL;
  kMicro_ADR,   // ADR(r);
  kMicro_LAD,   // LAD(r);
  kMicro_LD,    // LD(r);
  kMicro_ST,    // ST(r);
  kMicro_STP,   // STP(r);
  kMicro_MOVI,  // MOVI(r,v);
  kMicro_MOV,   // MOV(r,r);
  kMicro_ADDI,  // ADDI(r,v);
  kMicro_ADD,   // ADD(r,r);
  kMicro_ADC,   // ADC(r,r);
  kMicro_SUB,   // SUB(r,r);
  kMicro_SBC,   // SBC(r,r);
  kMicro_NEG,   // NEG(r,r);
  kMicro_CMP,   // CMP(r,r);
  kMicro_NOT,   // NOT(r,r);
  kMicro_AND,   // AND(r,r);
  kMicro_OR,    // OR(r,r);
  kMicro_XOR,   // XOR(r,r);
  kMicro_SL,    // SL(r);
  kMicro_SR,    // SR(r);
  kMicro_SRA,   // SRA(r);
  kMicro_RL,    // RL(r);
  kMicro_RR,    // RR(r);
  kMicro_RLC,   // RLC(r);
  kMicro_RRC,   // RRC(r);
  kMicro_JP,    // JP(a);
  kMicro_JC,    // JC(c,a);
  kMicro_JD,    // JD(r,a);
  kMicro_INT,   // INT(r);
  kMicro_ILD,   // ILD(r,r);
  kMicro_IST,   // IST(r,r);
  kMicro_IRT,   // IRT;
  kMicro_PLK,   // PLK(r);
  kMicro_PUL,   // PUL;
  kMicro_PLD,   // PLD(p,r);
  kMicro_PST,   // PST(p,r);
  kMicro_CLK,   // CLK(r);
  kMicro_CUL,   // CUL;
  kMicro_CBK,   // CBK(b,r);
  kMicro_CLD,   // CLD(r,r);
  kMicro_CST,   // CST(r,r);
  kMicro_END,   // END;
};

// The maximum number of lock/unlock pairs of any time (LK/UL, PLK/PUL, CLK/CUL)
// that can exist in a single instruction.
inline constexpr int kMaxLocksPerInstruction = 10;

// The type of an argument for microcode instructions.
enum class MicroArgType {
  kNone,       // No argument.
  kBank,       // Memory bank (CODE, STACK, DATA, or EXTRA).
  kStatus,     // ZSCOI flags (any combo of Z, S, C, O, I, and _ characters).
  kPortMode,   // Port mode (T, S, A, and _ characters).
  kCondition,  // ZSCO condition (Z, NZ, S, NS, C, NC, O, or NO).
  kAddress,    // Relative microcode address.
  kValue,      // Signed 8-bit value: [-128,127].
  kWordReg,    // Word register.
  kDwordReg,   // Dword register.
};

// Converts an ArgType to a MicroArgType.
MicroArgType ToMicroArgType(ArgType type);

// Definition of a microcode operation.
struct MicrocodeDef {
  uint8_t op;                // The microcode operation code.
  std::string_view op_name;  // The name of the operation.
  MicroArgType arg1;         // The type of the first argument.
  MicroArgType arg2;         // The type of the second argument.
};

// Microcode instruction for the OZ-3 CPU.
struct Microcode {
  // The operation code for the microcode.
  uint8_t op;

  // Arguments to the microcode. The microcode op specifies how many arguments
  // there are, and what type of argument value it is. There are two types of
  // arguments:
  // - Register: If the argument is a register (word or dword), then the value
  //   represents an index into the CpuCore register array. If it is greater
  //   than zero, it is just the register index. If it is less than zero, then
  //   it refers to the decoded arguments from the instruction code: -1 is reg1,
  //   and -2 is reg2, -3 is reg1 second word, and -4 is reg2 second word.
  // - Immediate: If the argument is an immediate value, bank, or status flags,
  //   port mode, condition, or address then the value is the arg itself.
  int8_t arg1;
  int8_t arg2;

  auto operator<=>(const Microcode&) const = default;
};

// Debug printer tests.
inline void PrintTo(const Microcode& microcode, std::ostream* os) {
  *os << "Microcode{op=" << static_cast<int>(microcode.op)
      << ", arg1=" << static_cast<int>(microcode.arg1)
      << ", arg2=" << static_cast<int>(microcode.arg2) << "}";
}

// Returns the microcode definitions for the OZ-3 CPU.
absl::Span<const MicrocodeDef> GetMicrocodeDefs();

// Internal structures used for compiling and decoding microcode.
namespace microcode_internal {

struct InstructionCode {
  uint16_t code_start = 0;  // Start index in microcode
  uint8_t code_size = 0;    // Number of microcodes
  uint8_t pc_size = 0;      // Size of the instruction for the program counter
};
static_assert(sizeof(InstructionCode) == 4);
static_assert(
    kMaxInstructionSetMicrocodes <=
    std::numeric_limits<decltype(InstructionCode::code_start)>::max());
static_assert(kMaxInstructionMicrocodes <=
              std::numeric_limits<decltype(InstructionCode::code_size)>::max());

struct SubInstruction {
  InstructionCode code;
  Argument arg;  // Argument (if any for the sub instruction)
};
static_assert(sizeof(SubInstruction) == 6);

struct Instruction {
  InstructionCode code;    // Simple arguments (no sub argument)
  uint16_t sub_index = 0;  // Index where sub instructions start.
  Argument arg1;
  Argument arg2;
};
static_assert(sizeof(Instruction) == 10);
static constexpr size_t kMaxSubInstructions =
    std::numeric_limits<decltype(Instruction::sub_index)>::max();

}  // namespace microcode_internal

}  // namespace oz3

#endif  // OZ3_CORE_MICROCODE_H_
