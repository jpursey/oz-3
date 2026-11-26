// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/ozzy_testing.h"

#include "absl/algorithm/container.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_format.h"
#include "gb/parse/parse_error.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/processor.h"
#include "oz3/instruction_sets/default_instruction_set.h"
#include "oz3/tools/bare_program.h"
#include "oz3/tools/instruction_set_syntax.h"

namespace oz3 {

namespace {

// Returns the default instruction set's syntax, created once, as it is slow to
// create and never changes. Null if it can't be created.
const InstructionSetSyntax* GetDefaultSyntax() {
  static const InstructionSetSyntax* const s_syntax = [] {
    InstructionError error;
    std::unique_ptr<InstructionSetSyntax> syntax =
        InstructionSetSyntax::Create(GetDefaultInstructionSetDef(),
                                     GetDefaultInstructionSet().get(), &error);
    EXPECT_NE(syntax, nullptr) << error.message;
    return syntax.release();
  }();
  return s_syntax;
}

}  // namespace

gb::Context WithPowerCallback(gb::Context context,
                              std::vector<PowerRequest>* requests) {
  context.SetNew<PowerRequestCallback>([requests](PowerRequest request) {
    if (requests != nullptr) {
      requests->push_back(request);
    }
  });
  return context;
}

OzzySettings GetTeletypeSettings(int baud) {
  OzzySettings settings;
  settings.screen.reset();
  settings.teletype = OzzyTeletypeSettings();
  settings.teletype->baud = baud;
  return settings;
}

std::unique_ptr<AsmProgram> AssembleTestProgram(std::string_view source) {
  const InstructionSetSyntax* syntax = GetDefaultSyntax();
  if (syntax == nullptr) {
    ADD_FAILURE() << "No default instruction set syntax";
    return nullptr;
  }
  std::vector<gb::ParseError> errors;
  std::unique_ptr<AsmProgram> program =
      AssembleProgram(*syntax, std::string(source), &errors);
  for (const gb::ParseError& parse_error : errors) {
    ADD_FAILURE() << parse_error.FormatMessage();
  }
  EXPECT_NE(program, nullptr);
  return program;
}

DiskSuperblock::BootArea GetBootArea(const AsmProgram& program) {
  DiskSuperblock::BootArea boot_area(DiskSuperblock::kBootAreaWords, 0);
  const absl::StatusOr<BareProgram>& bare = program.GetBareProgram();
  EXPECT_TRUE(bare.ok()) << bare.status();
  if (!bare.ok()) {
    return boot_area;
  }
  EXPECT_EQ(bare->contents_words, DiskSuperblock::kBootAreaWords);
  for (const Program::Segment::Data& data : bare->data) {
    if (data.offset + data.data.size() > boot_area.size()) {
      ADD_FAILURE() << "A block at " << data.offset << " is past the boot area";
      continue;
    }
    absl::c_copy(data.data, boot_area.begin() + data.offset);
  }
  return boot_area;
}

void ExpectSourceConstants(const ProgramMap& map,
                           std::map<std::string_view, uint32_t> expected) {
  for (const ProgramMap::Constant& constant : map.GetConstants()) {
    if (auto it = expected.find(constant.name); it != expected.end()) {
      EXPECT_EQ(constant.dword_value, it->second) << it->first;
      expected.erase(it);
    }
  }
  EXPECT_THAT(expected, ::testing::IsEmpty());
}

std::string GetLoaderErrorStatus(int code) {
  return absl::StrFormat("ERROR: Loader error %02Xh", code);
}

CpuCore::State GetState(Ozzy& ozzy) {
  return ozzy.GetProcessor()->GetCore(0)->GetState();
}

CpuCore::Registers GetRegisters(Ozzy& ozzy) {
  CpuCore::Registers registers;
  ozzy.GetProcessor()->GetCore(0)->GetRegisters(registers);
  return registers;
}

bool IsHalted(Ozzy& ozzy) {
  return ozzy.GetProcessor()->GetCore(0)->GetState() == CpuCore::State::kIdle;
}

std::string GetRowText(const ScreenPicture& picture, int row) {
  std::string text;
  for (int column = 0; column < picture.columns; ++column) {
    text.push_back(
        static_cast<char>(picture.cells[row * picture.columns + column]));
  }
  text.erase(text.find_last_not_of(' ') + 1);
  return text;
}

}  // namespace oz3
