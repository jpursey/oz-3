// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_MICRO_CODE_H_
#define OZ3_CORE_MICRO_CODE_H_

#include <cstdint>
#include <string>

#include "absl/types/span.h"
#include "oz3/core/instruction.h"

namespace oz3 {

// The folowing lists all the microcode operations that can be executed by the
// OZ-3 CpuCore. For details see the OZ-3 Wiki:
// https://github.com/jpursey/oz-3/wiki/2.1.-Microcode
enum MicroOp : uint8_t {
  kMicro_MSC,   // MSC(s);
  kMicro_MSS,   // MSS(s);
  kMicro_MSX,   // MSX(s);
  kMicro_MSM,   // MSM(s,r);
  kMicro_MSR,   // MSR(s,r);
  kMicro_WAIT,  // WAIT(r);
  kMicro_HALT,  // HALT;
  kMicro_LK,    // LK(b);
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
  // - Immediate, bank, or ZSCO: If the argument is an immediate value, bank, or
  //   ZSCO flags, then the value is the arg itself.
  int8_t arg1;
  int8_t arg2;

  auto operator<=>(const Microcode&) const = default;
};

// Decoded instruction from the OZ-3 CPU.
//
// This is used by the CpuCode to execute instructions via microcode.
struct DecodedInstruction {
  absl::Span<const Microcode> code;
  uint16_t size;  // Size of the full instruction (including inline values).
  uint16_t c[2];  // Value of C0 and C1 registers from instruction
  int8_t r[4];    // Indexes into r_ in CpuCore from instruction

  auto operator<=>(const DecodedInstruction&) const = default;
};

// The compiled microcode for an instruction.
struct CompiledInstruction {
  std::vector<Microcode> code;
  uint16_t size;     // Size of the full instruction (including inline values).
  ArgTypeBits arg1;  // First argument encoding.
  ArgTypeBits arg2;  // Second argument encoding.

  auto operator<=>(const CompiledInstruction&) const = default;
};

// Compiled database of microcode instructions for the OZ-3 CPU.
//
// The CpuCore uses this to compile all instructions from the provided
// instruction set into microcode, and then use Decode on to get the decoded
// instruction and microcode to execute.
class InstructionMicrocodes final {
 public:
  // Construct with default OZ-3 microcode definitions.
  InstructionMicrocodes();

  // Construct with specific microcode definitions (for testing). This is
  // non-functional outside of tests, as the CpuCore expects the OZ-3 defined
  // microcode definitions.
  explicit InstructionMicrocodes(
      absl::Span<const MicrocodeDef> micro_code_defs);
  InstructionMicrocodes(const InstructionMicrocodes&) = delete;
  InstructionMicrocodes& operator=(const InstructionMicrocodes&) = delete;
  ~InstructionMicrocodes();

  // Compiles the provided instructions into microcode.
  //
  // Returns true if the instructions were all successfully compiled, and false
  // if there is an error in at least one of the Instruction source definition.
  // The `error_string` is set to a description of the error if it is not null.
  bool Compile(absl::Span<const InstructionDef> instructions,
               std::string* error_string = nullptr);
  bool Compile(const InstructionDef& instruction,
               std::string* error_string = nullptr);

  // Decodes an instruction code into a set of microcode instructions and
  // parameters.
  //
  // This requires that all instructions have been compiled to microcode first
  //
  // Returns false if the instruction is invalid. The `decoded` parameter is set
  // to the NOP instruction.
  bool Decode(uint16_t instruction_code, DecodedInstruction& decoded);

 private:
  absl::Span<const MicrocodeDef> microcode_defs_;
  std::vector<CompiledInstruction> compiled_;
};

}  // namespace oz3

#endif  // OZ3_CORE_MICRO_CODE_H_
