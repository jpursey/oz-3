// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_INSTRUCTION_ASSEMBLER_H_
#define OZ3_TOOLS_INSTRUCTION_ASSEMBLER_H_

#include <memory>
#include <vector>

#include "gb/file/file_system.h"
#include "gb/parse/parse_error.h"
#include "oz3/core/instruction_def.h"
#include "oz3/core/instruction_set.h"

namespace oz3 {

class InstructionSetAssembler;

class AsmInstructionSet final {
 public:
  AsmInstructionSet(const AsmInstructionSet&) = delete;
  AsmInstructionSet& operator=(const AsmInstructionSet&) = delete;
  ~AsmInstructionSet() = default;

  // Returns the compiled instruction set. This may be used directly with
  // CpuCoreConfig to initialize CpuCores. It is valid beyond the lifetime of
  // this class.
  std::shared_ptr<const InstructionSet> GetInstructionSet() const {
    return instruction_set_;
  }

  // Returns this an instruction set definition backed by this class. This is
  // only valid as long as this class exists.
  const InstructionSetDef& GetInstructionSetDef() const {
    return instruction_set_def_;
  }

 private:
  friend class InstructionSetAssembler;

  AsmInstructionSet() = default;

  std::shared_ptr<const InstructionSet> instruction_set_;
  InstructionSetDef instruction_set_def_;
  std::vector<std::unique_ptr<std::string>> strings_;
  std::vector<std::vector<MacroCodeDef>> macro_code_defs_;
  std::vector<MacroDef> macro_defs_;
  std::vector<InstructionDef> instruction_defs_;
};

// Assembles an instruction set from a text definition.
std::unique_ptr<AsmInstructionSet> AssembleInstructionSet(
    std::string text, gb::ParseError* error = nullptr);

// Assembles an instruction set from a file.
std::unique_ptr<AsmInstructionSet> AssembleInstructionSetFile(
    gb::FileSystem& file_system, std::string_view path,
    gb::ParseError* error = nullptr);

}  // namespace oz3

#endif  // OZ3_TOOLS_INSTRUCTION_ASSEMBLER_H_
