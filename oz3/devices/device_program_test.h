// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_DEVICE_PROGRAM_TEST_H_
#define OZ3_DEVICES_DEVICE_PROGRAM_TEST_H_

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/log/check.h"
#include "gb/parse/parse_error.h"
#include "gtest/gtest.h"
#include "oz3/core/base_core_test.h"
#include "oz3/core/instruction_def.h"
#include "oz3/instruction_sets/default_instruction_set.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program_assembler.h"
#include "oz3/tools/program_loader.h"
#include "oz3/tools/program_map.h"

namespace oz3 {

//==============================================================================
// DeviceProgramTest
//==============================================================================

// A test fixture for devices, which runs OZ-3 programs against them on core 0,
// written in the default instruction set as programs are.
class DeviceProgramTest : public BaseCoreTest {
 protected:
  DeviceProgramTest()
      : BaseCoreTest(GetDefaultInstructionSetDef(),
                     GetDefaultInstructionSet()) {}

  // Assembles `text`, adding a test failure for each error, loads it, and
  // resets core 0 to run it. Init() must have been called. Returns whether it
  // did.
  bool Load(std::string_view text) {
    std::vector<gb::ParseError> errors;
    program_ = AssembleProgram(*syntax_, std::string(text), &errors);
    for (const gb::ParseError& error : errors) {
      ADD_FAILURE() << error.FormatMessage();
    }
    if (program_ == nullptr) {
      return false;
    }
    ProgramLoader loader(&GetProcessor());
    const ProgramLoader::Handle handle = loader.Load(program_->GetProgram());
    if (handle == ProgramLoader::kInvalidHandle) {
      ADD_FAILURE() << "The program doesn't load";
      return false;
    }
    GetState().ResetCore(loader.GetResetParams(handle));
    return true;
  }

  // Returns the offset of the label `name` (in upper case, as the assembler
  // writes it) in its segment, which for the code segment is its IP. Adds a
  // test failure and returns 0 if the loaded program has no such label.
  uint16_t GetLabel(std::string_view name) const {
    CHECK(program_ != nullptr);
    for (const ProgramMap::Label& label : program_->GetMap().GetLabels()) {
      if (label.name == name) {
        return static_cast<uint16_t>(label.offset);
      }
    }
    ADD_FAILURE() << "No label " << name;
    return 0;
  }

 private:
  static std::unique_ptr<InstructionSetSyntax> CreateSyntax() {
    InstructionError error;
    std::unique_ptr<InstructionSetSyntax> syntax =
        InstructionSetSyntax::Create(GetDefaultInstructionSetDef(),
                                     GetDefaultInstructionSet().get(), &error);
    CHECK(syntax != nullptr) << error.message;
    return syntax;
  }

  const std::unique_ptr<InstructionSetSyntax> syntax_ = CreateSyntax();
  std::unique_ptr<AsmProgram> program_;
};

}  // namespace oz3

#endif  // OZ3_DEVICES_DEVICE_PROGRAM_TEST_H_
