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

enum class MicroOp : uint8_t {
  LK,    // Lock memory bank.
  UL,    // Unlock memory bank.
  WAIT,  // Decrements register, if not zero, rewinds the PC back to the WAIT.
  HALT,  // Rewinds the PC back to the HALT instruction.

  // These opcodes are used for testing, they are not part of the OZ-3 CPU.
  MICRO_OP = 150,
};

// Definition of a microcode operation.
struct MicroCodeDef {
  MicroOp op;                // The microcode operation code.
  std::string_view op_name;  // The name of the operation.
  bool has_bank;             // True if the operation requires a bank extension.
  ArgType arg1;              // The type of the first argument.
  ArgType arg2;              // The type of the second argument.
};

// Microcode instruction for the OZ-3 CPU.
struct MicroCode {
  // The operation code for the microcode.
  MicroOp op;

  // The memory bank to operate on in the range [0,3]. Ignored if the operation
  // does not need a bank specification or it is implied from arguments.
  int8_t bank;

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
  uint16_t c0;  // Value of C0 register from instruction
  uint16_t c1;  // Value of C1 register from instruction
  int8_t reg1;  // Index into r_ in CpuCore from instruction
  int8_t reg2;  // Index into r_ in CpuCore from instruction

  auto operator<=>(const DecodedInstruction&) const = default;
};

// The compiled microcode for an instruction.
struct CompiledInstruction {
  std::vector<MicroCode> code;
  ArgTypeBits arg1;
  ArgTypeBits arg2;

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

  // Construct with specific microcode definitions (for testing).
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
  bool CompileToMicroCode(absl::Span<const InstructionDef> instructions,
                          std::string* error_string);
  bool CompileToMicroCode(const InstructionDef& instruction,
                          std::string* error_string);

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
