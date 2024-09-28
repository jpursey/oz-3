// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_INSTRUCTION_SET_H_
#define OZ3_CORE_INSTRUCTION_SET_H_

#include <cstdint>

#include "absl/types/span.h"
#include "oz3/core/microcode.h"

namespace oz3 {

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

// This class represents a compiled instruction set for the OZ-3 CPU.
//
// To create an instruction set, use the OldInstructionCompiler class to compile
// a set of instructions into microcode, and then use this class to decode
// instruction codes into microcode instructions.
class InstructionSet {
 public:
  InstructionSet() = default;
  InstructionSet(const InstructionSet&) = default;
  InstructionSet& operator=(const InstructionSet&) = default;
  InstructionSet(InstructionSet&&) = default;
  InstructionSet& operator=(InstructionSet&&) = default;
  ~InstructionSet() = default;

  // Decodes an instruction code into a set of microcode instructions and
  // parameters.
  //
  // This requires that all instructions have been compiled to microcode first
  //
  // Returns false if the instruction is invalid. The `decoded` parameter is set
  // to the NOP instruction.
  bool Decode(uint16_t code_word, DecodedInstruction& decoded) const;

 private:
  friend class InstructionCompiler;

  using Instruction = microcode_internal::Instruction;
  using SubInstruction = microcode_internal::SubInstruction;
  using InstructionCode = microcode_internal::InstructionCode;

  struct DecodeState {
    DecodedInstruction& decoded;
    const Instruction& instruction;
    const InstructionCode* code;
    uint16_t code_word;
  };

  InstructionSet(std::vector<Instruction> instructions,
                 std::vector<SubInstruction> sub_intructions,
                 std::vector<Microcode> microcodes)
      : instructions_(instructions),
        sub_instructions_(sub_intructions),
        microcodes_(microcodes) {}

  const Instruction* GetInstruction(int op) const;
  absl::Span<const Microcode> GetCode(const InstructionCode& code) const;
  bool DecodeArgument(DecodeState& state, const Argument& arg,
                      int arg_index) const;

  std::vector<Instruction> instructions_;
  std::vector<SubInstruction> sub_instructions_;
  std::vector<Microcode> microcodes_;
};

}  // namespace oz3

#endif  // OZ3_CORE_INSTRUCTION_SET_H_
