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
//      low/high word parts of dword args A and B). There are some registers
//      that can't be permanently changed via standard microcode (this does mean
//      they can be used as temporary storage within an instruction's microcode,
//      as they will be restored automatically):
//      - The ST register is set to the intern MSR register at the end of the
//        instruction (which starts as equal to ST). To change the persistent
//        value of ST, use the MSC/MSS/MSX/MSM operations to set MST and
//        return with the MSR operation.
//      - The BM register specifies the memory bank mapping and is reset to the
//        actual memory bank binding at the end of the instruction. To change
//        the persistent value of BM (and the associated bank mapping), use the
//        CBK operation.
//   v: Immediate value. This is an 8-bit signed value. In microcode assembly,
//      this is an explicit integer in the range [-128,127].
//   b: Memory bank. In microcode assembly this must be one of CODE, STACK,
//      DATA, or EXTRA.
//   s: Status ZSCOI flag mask. In microcode assembly, this is any combination
//      of Z, S, C, O, I, and _ characters (e.g. ZC or Z_C_ or just _ to
//      indicate no flags).
//   c: ZSCO flag condition. In microcode assembly, this is one of the
//      following: Z, NZ, S, NS, C, NC, O, or NO.
//   a: Relative microcode address. In microcode assembly, this is an integer
//      value that is added to the microcode program counter (MPC) to jump to a
//      new location in the microcode program. It can also be an @ label to jump
//      to the specified instruction with the same label.
//   p: Port mode. In microcode assembly, this is any combination of T, S, A,
//      and _ characters (e.g. TA or T_A or just _ to indicate no flags).
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

  // MSC(s);
  //
  // Cycles: 0
  //
  // Clears any flags specified in arg1 from microcode ZSCOI status flags (MST):
  //   MST = MST & ~arg1
  kMicro_MSC,

  // MSS(s);
  //
  // Cycles: 0
  //
  // Sets any flags specified in arg1 inside microcode ZSCOI status flags (MST):
  //   MST = MST | arg1
  kMicro_MSS,

  // MSX(s);
  //
  // Cycles: 0
  //
  // Exclusively ors arg1 with microcode ZSCOI status (MST):
  //   MST = MST ^ arg1
  kMicro_MSX,

  // MSM(s,r);
  //
  // Cycles: 1
  //
  // Moves (copies) the flags specified by arg1 in reg2 to the microcode status
  // flags (MST):
  //   MST = (MST & ~arg1) | (reg2 & arg1)
  kMicro_MSM,

  // MSR(s,s);
  //
  // Cycles: 0
  //
  // Returns microcode status flags (MST) to the return status register (MSR)
  // and the ST register, by clearing any unset bits from MST specified by arg1
  // and setting any set bits from MST specified by arg2.
  //
  // Explicitly:
  //   MSR = (MSR & (MST | ~arg1)) | (MST & arg2)
  //   ST = MSR
  //
  // Examples:
  //   MSR/ST Before   MST    arg1 (clr)   arg2 (set)   MSR/ST After
  //       ____I      ZSCO_      ZS___       __CO_         __COI
  //       ZS___      _____      ZS___       __CO_         _____
  //       Z___I      _S_O_      ZS___       ZSCO_         _S_OI
  //       ZS_O_      Z_C__      ZS___       __CO_         Z_CO_
  //       Z_C_I      _S_O_      ____I       ____I         Z_C__
  //       _S_O_      C_Z_I      ____I       ____I         _S_OI
  kMicro_MSR,

  // WAIT(r);
  //
  // Cycles: 0 (see description)
  //
  // Puts core into kWaiting state for the arg1 cycles in reg (specified in
  // arg1). Further execution of microcode in the instruction is terminated. If
  // the wait time is less than the amount of time taken so far executing the
  // instruction (including the time when microcode execution is paused), then
  // it does nothing.
  kMicro_WAIT,

  // HALT;
  //
  // Cycles: 0 (see description)
  //
  // Puts the core into kIdle state. Further execution of microcode in the
  // instruction is terminated. Execution will not continue until the core is
  // reset.
  kMicro_HALT,

  // LK(b);
  //
  // Cycles: 0 (see description)
  //
  // Lock memory bank arg1 and sets it as the active memory bank. This is used
  // to lock a memory bank for exclusive access. If the bank is already locked,
  // the microcode execution is paused for the instruction until the bank is
  // unlocked. Only one lock can be active at a time (memory bank, port, or
  // core).
  kMicro_LK,

  // UL;
  //
  // Cycles: 0
  //
  // Unlock memory bank previously locked by LK. All locked banks must be
  // unlocked before microcode execution completes in the instruction.
  kMicro_UL,

  // ADR(r);
  //
  // Cycles: 1
  //
  // Sets the memory bank address bus to reg1. The memory bank must be locked
  // from LK.
  kMicro_ADR,

  // LAD(r);
  //
  // Cycles: 0
  //
  // Loads the current address from the memory into reg1. The memory bank must
  // be locked from LK.
  kMicro_LAD,

  // LD(r);
  //
  // Cycles: 1
  //
  // Loads the value from memory into reg1, and advances the address bus. The
  // memory bank must be locked from LK, and the address set previously by ADR.
  kMicro_LD,

  // ST(r);
  //
  // Cycles: 1
  //
  // Stores the value from reg1 into memory, and advances the address bus. The
  // memory bank must be locked from LK, and the address set previously by ADR.
  kMicro_ST,

  // STP(r);
  //
  // Cycles: 1
  //
  // Decrements the address bus, then stores the value from reg1 into memory.
  // The memory bank must be locked from LK, and the address set previously by
  // ADR.
  kMicro_STP,

  // MOVI(r,v);
  //
  // Cycles: 1
  //
  // Copies the signed value arg2 into reg1:
  //   reg1 = arg2
  kMicro_MOVI,

  // MOV(r,r);
  //
  // Cycles: 1
  //
  // Copies the value from reg2 into reg1:
  //   reg1 = reg2
  kMicro_MOV,

  // ADDI(r,v);
  //
  // Cycles: 1
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
  // Cycles: 1
  //
  // Adds the value from reg2 to reg1:
  //   reg1 = reg1 + reg2
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: Unsigned overflow occurred
  //   O: Signed overflow occurred
  kMicro_ADD,

  // ADC(r,r);
  //
  // Cycles: 1
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
  // Cycles: 1
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
  // Cycles: 1
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
  // Cycles: 1
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
  // Cycles: 1
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
  // Cycles: 1
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
  // Cycles: 1
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
  // Cycles: 1
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
  // Cycles: 1
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
  // Cycles: 1
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
  // Cycles: 1
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
  // Cycles: 1
  //
  // Shifts the value in reg1 right with sign extension:
  //   reg1 = reg1 >> 1
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: bit pushed out of reg1
  //   O: cleared
  kMicro_SRA,

  // RL(r);
  //
  // Cycles: 1
  //
  // Rotates the value in reg1 left:
  //   reg1 = (reg1 << 1) | (reg1 >> 15)
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: bit rotated out of reg1
  //   O: cleared
  kMicro_RL,

  // RR(r);
  //
  // Cycles: 1
  //
  // Rotates the value in reg1 right:
  //   reg1 = (reg1 >> 1) | (reg1 << 15)
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: bit rotated out of reg1
  //   O: cleared
  kMicro_RR,

  // RLC(r);
  //
  // Cycles: 1
  //
  // Rotates the value in reg1 left with carry:
  //   reg1 = (reg1 << 1) | C
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: bit rotated out of reg1
  //   O: cleared
  kMicro_RLC,

  // RRC(r);
  //
  // Cycles: 1
  //
  // Rotates the value in reg1 right with carry:
  //   reg1 = (reg1 >> 1) | (C << 15)
  // Sets or clears all MST flags as follows:
  //   Z: reg1 is zero
  //   S: reg1 high bit is set
  //   C: bit rotated out of reg1
  //   O: cleared
  kMicro_RRC,

  // JP(a);
  //
  // Cycles: 0
  //
  // Sets the microcode program counter (MPC) to the relative address specified
  // in arg1:
  //   MPC = MPC + arg1
  // It is invalid to jump to an address outside the microcode program, or
  // from/to an address where a lock is active (between LK and UL).
  kMicro_JP,

  // JC(c,a);
  //
  // Cycles: 0 if condition is false, 1 if condition is true
  //
  // Set the microcode program counter (MPC) to the relative address specified
  // in arg2 if the condition specified by arg1 is true:
  //   if (arg1) MPC = MPC + arg2
  // It is invalid to jump to an address outside the microcode program, or
  // from/to an address where a lock is active (between LK and UL).
  kMicro_JC,

  // JD(r,a);
  //
  // Cycles: 1
  //
  // Decrements register reg1, and then sets the microcode program counter (MPC)
  // to the relative address specified in arg2 if reg1 is not zero:
  //   reg1 = reg1 - 1
  //   if (reg1 != 0) MPC = MPC + arg2
  // It is invalid to jump to an address outside the microcode program, or
  // from/to an address where the lock state differs (set/cleared by LK/UL).
  kMicro_JD,

  // INT(r);
  //
  // Cycles: 0
  //
  // Initiates an interrupt with the interrupt number specified in reg1. This
  // does not end microcode execution, and the interrupt is not processed until
  // the next instruction is executed.
  kMicro_INT,

  // ILD(r,r);
  //
  // Cycles: 1
  //
  // Loads address of interrupt reg1 into reg2.
  kMicro_ILD,

  // IST(r,r);
  //
  // Cycles: 1
  //
  // Stores address in reg2 to interrupt reg1.
  kMicro_IST,

  // PLK(r);
  //
  // Cycles: 0
  //
  // Locks the port specified by reg1 and sets it as the active port. This is
  // used to lock a port for exclusive access. If the port is already locked,
  // the microcode execution is paused for the instruction until the port is
  // unlocked. Only one lock can be active at a time (memory bank, port, or
  // core). If the port does not exist on the processor, all port operations are
  // no-ops.
  kMicro_PLK,

  // PUL(r);
  //
  // Cycles: 0
  //
  // Unlocks the port previously locked by PLK. All locked ports must be
  // unlocked before microcode execution completes in the instruction.
  kMicro_PUL,

  // PLD(p,r);
  //
  // Cycles: 1
  //
  // Loads the value from the port into reg2, respecting specified mode flags in
  // arg1 as follows:
  //   T: Load only if the port status is set. If the port status is cleared,
  //      then the value in reg2 is not updated.
  //   S: The port status is cleared after the load operation.
  //   A: The port address is updated after the load operation. Ports have two
  //      words of memory, and this toggles between the two words.
  // Sets or clears the MST flags as follows:
  //   S: Port status was set (before the load operation).
  // The port must be locked from PLK
  kMicro_PLD,

  // PST(p,r);
  //
  // Cycles: 1
  //
  // Stores the value from reg2 into the port, respecting specified mode flags
  // in arg1 as follows:
  //   T: Store only if the port status is cleared. If the port status is set,
  //      then value on the port is not updated.
  //   S: The port status is set after the store operation.
  //   A: The port address is updated after the store operation. Ports have two
  //      words of memory, and this toggles between the two words.
  // Sets or clears the MST flags as follows:
  //   S: Port status was set (before the store operation).
  // The port must be locked from PLK.
  kMicro_PST,

  // CLK(r);
  //
  // Cycles: 0
  //
  // Locks the CPU core specified by reg1 and sets it as controlled core for
  // core control operations. This is used to lock a core for exclusive access.
  // If the core is already locked, the microcode execution is paused for the
  // instruction until the core is unlocked. Only one lock can be active at a
  // time (memory bank, port, or core).
  //
  // If the specified core does not exist on the processor, all core control
  // operations are no-ops until CUL is called. Before CLK, the controlled core
  // is this core (the one running the microcode).
  kMicro_CLK,

  // CUL(r);
  //
  // Cycles: 0
  //
  // Unlocks the CPU core previously locked by CLK. All locked cores must be
  // unlocked before microcode execution completes in the instruction. The
  // controlled core is reset to this core (the one running the microcode).
  kMicro_CUL,

  // CBK(b,r);
  //
  // Cycles: 1
  //
  // Sets the memory bank of the controlled core (from CLK) to the bank
  // specified by reg2. If no core is locked, then this affects this core.
  // This is the only way to set the BM register.
  kMicro_CBK,

  // CLD(r,r);
  //
  // Cycles: 1
  //
  // Loads the value from the controlled core register reg1 into this core's
  // reg2. If this is the controlled core, then this is equivalent to
  // MOV(reg2,reg1).
  kMicro_CLD,

  // CST(r,r);
  //
  // Cycles: 1
  //
  // Stores the value from this core's reg2 to the controlled core register
  // reg1. The BM and ST registers cannot be modified by this op. Otherwise, if
  // this is the controlled core, then this is equivalent to MOV(reg1,reg2).
  kMicro_CST,

  // END;
  //
  // Cycles: 0
  //
  // Terminates the microcode execution for the instruction.
  kMicro_END,
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
