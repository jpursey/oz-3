// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_MICRO_CODE_H_
#define OZ3_CORE_MICRO_CODE_H_

#include <cstdint>
#include <string>

#include "absl/types/span.h"
#include "oz3/core/instruction_set.h"

namespace oz3 {

enum MicroOp : uint8_t {
  kMicro_WAIT,  // Puts core into kWaiting state for specified cycles in reg.
  kMicro_HALT,  // Puts the core into kIdle state.
  kMicro_LK,    // Lock memory bank.
  kMicro_UL,    // Unlock memory bank.
  kMicro_ADR,   // Sets the memory bank address bus to the register value.
  kMicro_LD,    // Loads the value from memory into the register.
  kMicro_ST,    // Stores the value from the register into memory.
  kMicro_MOV,   // Moves a value from one register to another.
  kMicro_MOVI,  // Moves an immediate value into a register.
  kMicro_ADD,   // Adds a register value to another register.
  kMicro_ADDI,  // Adds an immediate value (can be negative) to a register.
  kMicro_SUB,   // Subtracks one register value from another register.
};

// Definition of a microcode operation.
struct MicroCodeDef {
  uint8_t op;                // The microcode operation code.
  std::string_view op_name;  // The name of the operation.
  ArgType arg1;              // The type of the first argument.
  ArgType arg2;              // The type of the second argument.
};

// Microcode instruction for the OZ-3 CPU.
struct MicroCode {
  // The operation code for the microcode.
  uint8_t op;

  // Arguments to the microcode. The micro code op specifies how many arguments
  // there are, and what type of argument value it is. There are two types of
  // arguments:
  // - Register: If the argument is a register (word or dword), then the value
  //   represents an index into the CpuCore register array. If it is greater
  //   than zero, it is just the register index. If it is less than zero, then
  //   it refers to the decoded arguments from the instruction code: -1 is reg1,
  //   and -2 is reg2.
  // - Immediate: If the argument is an immediate value, then the value is the
  //   immediate value itself.
  int8_t arg1;
  int8_t arg2;

  // These determine which of the ZSCO flags are allowed to be set or cleared in
  // the ST register by the micro code.
  uint8_t st_clear;  // Flags to clear in the ST register.
  uint8_t st_set;    // Flags to set in the ST register.

  auto operator<=>(const MicroCode&) const = default;
};

// Decoded instruction from the OZ-3 CPU.
//
// This is used by the CpuCode to execute instructions via microcode.
struct DecodedInstruction {
  absl::Span<const MicroCode> code;
  uint16_t size;  // Size of the full instruction (including inline values).
  uint16_t c[2];  // Value of C0 and C1 registers from instruction
  int8_t r[2];    // Indexes into r_ in CpuCore from instruction

  auto operator<=>(const DecodedInstruction&) const = default;
};

// The compiled microcode for an instruction.
struct CompiledInstruction {
  std::vector<MicroCode> code;
  uint16_t size;     // Size of the full instruction (including inline values).
  ArgTypeBits arg1;  // First argument encoding.
  ArgTypeBits arg2;  // Second argument encoding.

  auto operator<=>(const CompiledInstruction&) const = default;
};

// Compiled database of microcode instructions for the OZ-3 CPU.
//
// Typical usage is to compile all instructions into microcode, and then use
// Decode on a OZ-3 machine code to get the decoded instruction and microcode to
// execute within the CpuCore.
class InstructionMicroCodes final {
 public:
  // Construct with default OZ-3 microcode definitions.
  InstructionMicroCodes();

  // Construct with specific microcode definitions (for testing). This is
  // non-functional outside of tests, as the CpuCore expects the OZ-3 defined
  // micro code definitions.
  explicit InstructionMicroCodes(
      absl::Span<const MicroCodeDef> micro_code_defs);
  InstructionMicroCodes(const InstructionMicroCodes&) = delete;
  InstructionMicroCodes& operator=(const InstructionMicroCodes&) = delete;
  ~InstructionMicroCodes();

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
  absl::Span<const MicroCodeDef> micro_code_defs_;
  std::vector<CompiledInstruction> compiled_;
};

}  // namespace oz3

#endif  // OZ3_CORE_MICRO_CODE_H_
