// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_DEVICE_PROGRAM_TEST_H_
#define OZ3_DEVICES_DEVICE_PROGRAM_TEST_H_

#include <cstdint>
#include <memory>
#include <string_view>
#include <vector>

#include "absl/time/civil_time.h"
#include "absl/types/span.h"
#include "gb/base/context.h"
#include "oz3/core/base_core_test.h"
#include "oz3/instructions/default_instruction_set.h"
#include "oz3/syntax/instruction_set_syntax.h"
#include "oz3/tools/program_assembler.h"

namespace oz3 {

//==============================================================================
// DeviceProgramTest
//==============================================================================

// A test fixture for devices, which runs OZ-3 programs against them on core 0,
// written in the default instruction set as programs are, with a fake of the
// host's clock for the devices that need one.
class DeviceProgramTest : public BaseCoreTest {
 protected:
  DeviceProgramTest()
      : BaseCoreTest(GetDefaultInstructionSetDef(),
                     GetDefaultInstructionSet()) {}

  // Destroys the processor while the clock's members are still alive, as a
  // drive calls the clock as it is destroyed.
  ~DeviceProgramTest() override { DestroyProcessor(); }

  // Assembles `text`, adding a test failure for each error, loads it, and
  // resets core 0 to run it. Init() must have been called. Returns whether it
  // did.
  bool Load(std::string_view text);

  // Returns the offset of the label `name` (in upper case, as the assembler
  // writes it) in its segment, which for the code segment is its IP. Adds a
  // test failure and returns 0 if the loaded program has no such label.
  uint16_t GetLabel(std::string_view name) const;

  // Returns `count` words from the label `label` on, in the loaded program's
  // data segment.
  std::vector<uint16_t> GetData(std::string_view label, int count);

  // Returns a context for the processor (see InitConfig::context) with a
  // clock that returns now_ and counts its calls in clock_calls_, or an empty
  // clock, for no date, if with_clock_ is false.
  gb::Context GetHostContext();

  // Sets what the clock returns from now on.
  void SetNow(absl::CivilSecond now) { now_ = now; }

  // Sets the words from the label `label` on, in the loaded program's data
  // segment.
  void SetData(std::string_view label, absl::Span<const uint16_t> words);

  // The host's clock that GetHostContext gives.
  bool with_clock_ = true;
  absl::CivilSecond now_;
  int clock_calls_ = 0;

 private:
  static std::unique_ptr<InstructionSetSyntax> CreateSyntax();

  const std::unique_ptr<InstructionSetSyntax> syntax_ = CreateSyntax();
  std::unique_ptr<AsmProgram> program_;
};

}  // namespace oz3

#endif  // OZ3_DEVICES_DEVICE_PROGRAM_TEST_H_
