// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/device_program_test.h"

#include <cstdint>
#include <string>

#include "absl/log/check.h"
#include "gb/parse/parse_error.h"
#include "gtest/gtest.h"
#include "oz3/core/date_time.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/tools/program_loader.h"
#include "oz3/tools/program_map.h"

namespace oz3 {

bool DeviceProgramTest::Load(std::string_view text) {
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

uint16_t DeviceProgramTest::GetLabel(std::string_view name) const {
  CHECK(program_ != nullptr);
  for (const ProgramMap::Label& label : program_->GetMap().GetLabels()) {
    if (label.name == name) {
      return static_cast<uint16_t>(label.offset);
    }
  }
  ADD_FAILURE() << "No label " << name;
  return 0;
}

std::vector<uint16_t> DeviceProgramTest::GetData(std::string_view label,
                                                 int count) {
  MemAccessor data = GetState().data;
  data.SetAddress(GetState().bd + GetLabel(label));
  std::vector<uint16_t> words;
  for (int i = 0; i < count; ++i) {
    words.push_back(data.GetValue());
  }
  return words;
}

gb::Context DeviceProgramTest::GetHostContext() {
  gb::Context context;
  if (with_clock_) {
    context.SetValue<HostClock>([this] {
      ++clock_calls_;
      return now_;
    });
  } else {
    context.SetValue<HostClock>(HostClock());
  }
  return context;
}

void DeviceProgramTest::SetData(std::string_view label,
                                absl::Span<const uint16_t> words) {
  MemAccessor data = GetState().data;
  data.SetAddress(GetState().bd + GetLabel(label));
  for (const uint16_t word : words) {
    data.AddValue(word);
  }
}

std::unique_ptr<InstructionSetSyntax> DeviceProgramTest::CreateSyntax() {
  InstructionError error;
  std::unique_ptr<InstructionSetSyntax> syntax = InstructionSetSyntax::Create(
      GetDefaultInstructionSetDef(), GetDefaultInstructionSet().get(), &error);
  CHECK(syntax != nullptr) << error.message;
  return syntax;
}

}  // namespace oz3
