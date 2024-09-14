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
// OZ-3 CpuCore, and the microcode assembly semantics. Each operation has a
// opcode with a name that matches the enumeration and up to two argumnents.
//
// Arguments are of the following types:
//   r: Word register. In microcode assembly, this may be any explicitly named
//      word register (e.g. R0, PC, ST, etc.) or a register provided from
//      the instruction code (a or b for word args; a0, a1, b0, or b1 for
//      low/high word parts of dword args A and B).
//   d: Dword register. In microcode assembly, this may be any explicitly named
//      dword register (e.g. D0, CD, etc.) or provided by the instruction arg
//      code byte (A or B).
//   v: Immediate value. This is an 8-bit signed value. In microcode assembly,
//      this is an explicit integer in the range [-128,127].
//   b: Memory bank. In microcode assembly this must be one of CODE, STACK,
//      DATA, or EXTRA.
//   z: ZSCO flag mask. In microcode assembly, this is any combination of Z, S,
//      C, O, and _ characters (e.g. ZC or Z_C_ or just _ to indicate no flags).
//
// The following operations are documented first by their microcode assembly
// opcode and required arguments and types. For instance "OP(z,r);" means that
// the operation requires two arguments, the first of which is a ZSCO flag mask
// and the second is a word register.
//
// In the description, the arguments are referred to as follows:
//   arg1, arg2: The literal argument value provided (value or index).
//   reg1, reg2: The register value provided to the argument index (r or d).
enum MicroOp : uint8_t {

  // MSTC(z);
  //
  // Clears any flags specified in arg1 inside microcode ZSCO status flags
  // (MST).
  //
  // Explicitly:
  //   MST = MST & ~arg1;
  kMicro_MSTC,

  // MSTS(z);
  //
  // Sets any flags specified in arg1 inside microcode ZSCO status flags (MST).
  //
  // Explicitly:
  //   MST = MST | arg1;
  kMicro_MSTS,

  // MSTX(z);
  //
  // Exclusively ors arg1 with microcode ZSCO status (MST).
  //
  // Explicitly:
  //   MST = MST ^ arg1;
  kMicro_MSTX,

  // MSTR(z,z);
  //
  // Returns microcode status flags (MST) to the ST register, by clearing any
  // unset bits from MST specified by arg1 and setting any set bits from MST
  // specified by arg2.
  //
  // Explicitly:
  //   ST = (ST & (MST | ~arg1)) | (MST & arg2);
  //
  // Examples:
  //   ST Before   MST    arg1 (clr)   arg2 (set)   ST After
  //     ____      ZSCO      ZS__        __CO         __CO
  //     ZS__      ____      ZS__        __CO         ____
  //     Z___      _S_O      ZS__        ZSCO         _S_O
  //     ZS_O      Z_C_      ZS__        __CO         Z_CO
  kMicro_MSTR,

  // WAIT(r);
  //
  // Puts core into kWaiting state for the arg1 cycles in reg (specified in
  // arg1). Further execution of microcode in the instruction is terminated. If
  // the wait time is less than the amount of time taken so far, then it does
  // nothing.
  kMicro_WAIT,

  // HALT;
  //
  // Puts the core into kIdle state. Further execution of microcode in the
  // instruction is terminated. Execution will not continue until the core is
  // reset.
  kMicro_HALT,

  // LK(b);
  //
  // Lock memory bank arg1 and sets it as the active memory bank. This is used
  // to lock a memory bank for exclusive
  // access. If the bank is already locked, the microcode execution is paused
  // for the instruction until the bank is unlocked. Only one bank can be locked
  // at a time.
  kMicro_LK,

  // UL;
  //
  // Unlock memory bank previously locked by LK. All locked banks must be
  // unlocked before microcode execution in the instruction.
  kMicro_UL,

  // ADR(r);
  //
  // Sets the memory bank address bus to reg1. The memory bank must be locked
  // from LK.
  kMicro_ADR,

  // LD(r);
  //
  // Loads the value from memory into reg1, and advances the address bus. The
  // memory bank must be locked from LK, and the address set previously by ADR.
  kMicro_LD,

  // ST(r);
  //
  // Stores the value from reg1 into memory, and advances the address bus. The
  // memory bank must be locked from LK, and the address set previously by ADR.
  kMicro_ST,

  // STP(r);
  //
  // Decrements the address bus, then stores the value from reg1 into memory.
  // The memory bank must be locked from LK, and the address set previously by
  // ADR.
  kMicro_STP,

  // MOVI(r,v);
  //
  // Copies the signed value arg2 into reg1:
  //   reg1 = arg2
  kMicro_MOVI,

  // MOV(r,r);
  //
  // Copies the value from reg2 into reg1:
  //   reg1 = reg2
  kMicro_MOV,

  // ADDI(r,v);
  //
  // Adds the signed value arg2 to reg1:
  //   reg1 = reg1 + arg2
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: Unsigned overflow occurred
  //   O: Signed overflow occurred
  kMicro_ADDI,

  // ADD(r,r);
  //
  // Adds the value from reg2 to reg1:
  //   reg1 = reg1 + reg2
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: Unsigned overflow occurred
  //   O: Signed overflow occurred
  kMicro_ADD,

  // ADC(r,r)
  //
  // Adds the value from reg2 to reg1 with carry:
  //   reg1 = reg1 + reg2 + C
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: Unsigned overflow occurred
  //   O: Signed overflow occurred
  kMicro_ADC,

  // SUB(r,r);
  //
  // Subtracts the value from reg2 from reg1:
  //   reg1 = reg1 - reg2
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: Unsigned overflow occurred
  //   O: Signed overflow occurred
  kMicro_SUB,

  // SBC(r,r);
  //
  // Subtracts the value from reg2 from reg1 with borrow:
  //   reg1 = reg1 - reg2 - C
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: Unsigned overflow occurred
  //   O: Signed overflow occurred
  kMicro_SBC,

  // NEG(r,r);
  //
  // Negates the value in reg2 and stores it in reg1:
  //   reg1 = -reg2
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: cleared
  //   O: reg1 is -32768 (error detection of negating -32768)
  kMicro_NEG,

  // CMP(r,r);
  //
  // Compares the value in reg1 with reg2:
  //   reg1 - reg2
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: Unsigned overflow occurred
  //   O: Signed overflow occurred
  kMicro_CMP,

  // NOT(r,r);
  //
  // Bitwise NOT of the value in reg2 and stores it in reg1:
  //   reg1 = ~reg2
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: cleared
  //   O: cleared
  kMicro_NOT,

  // AND(r,r);
  //
  // Bitwise AND of the values in reg1 and reg2 and stores it in reg1:
  //   reg1 = reg1 & reg2
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: cleared
  //   O: cleared
  kMicro_AND,

  // OR(r,r);
  //
  // Bitwise OR of the values in reg1 and reg2 and stores it in reg1:
  //   reg1 = reg1 | reg2
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: cleared
  //   O: cleared
  kMicro_OR,

  // XOR(r,r);
  //
  // Bitwise XOR of the values in reg1 and reg2 and stores it in reg1:
  //   reg1 = reg1 ^ reg2
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: cleared
  //   O: cleared
  kMicro_XOR,

  // SL(r);
  //
  // Shifts the value in reg1 left:
  //   reg1 = reg1 << 1
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: bit pushed out of reg1
  //   O: cleared
  kMicro_SL,

  // SR(r);
  //
  // Shifts the value in reg1 right:
  //   reg1 = reg1 >> 1
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: bit pushed out of reg1
  //   O: cleared
  kMicro_SR,

  // SRA(r);
  //
  // Shifts the value in reg1 right with sign extension:
  //   reg1 = reg1 >> 1
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: bit pushed out of reg1
  //   O: cleared
  kMicro_SRA,
};

// The type of an argument for microcode instructions.
enum class MicroArgType {
  kNone,      // No argument.
  kBank,      // Memory bank (CODE, STACK, DATA, or EXTRA).
  kZsco,      // ZSCO flags (any combo of Z, S, C, O, and _ characters).
  kValue,     // Signed 8-bit value: [-128,127].
  kWordReg,   // Word register.
  kDwordReg,  // Dword register.
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
