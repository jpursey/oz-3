// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/machine/ozzy_testing.h"

#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_format.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gb/parse/parse_error.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/date_time.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/lockable.h"
#include "oz3/core/processor.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_status.h"
#include "oz3/instructions/default_instruction_set.h"
#include "oz3/tools/bare_program.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program_loader.h"

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

// The sectors of the images MakeTestImage makes: a 360K floppy's.
constexpr int kTestImageSectors = 720;

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

std::unique_ptr<Ozzy> CreateTestOzzy(const OzzySettings& settings,
                                     gb::Context context) {
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(settings, WithPowerCallback(std::move(context)));
  EXPECT_TRUE(ozzy.ok()) << ozzy.status();
  return ozzy.ok() ? *std::move(ozzy) : nullptr;
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

bool LoadOnCore0(Ozzy& ozzy, std::string_view source) {
  std::unique_ptr<AsmProgram> program = AssembleTestProgram(source);
  if (program == nullptr) {
    return false;
  }
  ProgramLoader loader(ozzy.GetProcessor());
  const ProgramLoader::Handle handle = loader.Load(program->GetProgram());
  if (handle == ProgramLoader::kInvalidHandle) {
    ADD_FAILURE() << "The program doesn't load";
    return false;
  }
  CpuCore* core = ozzy.GetProcessor()->GetCore(0);
  std::unique_ptr<Lock> lock = core->RequestLock();
  EXPECT_TRUE(lock->IsLocked());
  core->Reset(*lock, loader.GetResetParams(handle));
  return true;
}

void MakeTestImage(gb::FileSystem* host, std::string_view path) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(host, path, kTestImageSectors);
  ASSERT_TRUE(image.ok()) << image.status();
  DiskFileSystem file_system({}, 1);
  file_system.AttachImage(*std::move(image));
  EXPECT_EQ(file_system.Format(*DiskLabel::Parse("WORK"), 0, DateTime()),
            DiskStatus::kOk);
}

std::optional<std::pair<DiskDirEntry, std::vector<uint16_t>>> ReadTestFile(
    gb::FileSystem* host, std::string_view path, std::string_view name) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(host, path);
  EXPECT_TRUE(image.ok()) << image.status();
  if (!image.ok()) {
    return std::nullopt;
  }
  DiskFileSystem file_system({}, 1);
  file_system.AttachImage(*std::move(image));
  const DiskFilename filename = *DiskFilename::Parse(name);
  DiskFile root = 0;
  EXPECT_EQ(file_system.OpenRoot(&root), DiskStatus::kOk);
  DiskDirEntry entry;
  const bool found =
      file_system.Find(root, filename, &entry) == DiskStatus::kOk;
  EXPECT_EQ(file_system.Close(root, DateTime()), DiskStatus::kOk);
  if (!found) {
    return std::nullopt;
  }
  DiskFile file = 0;
  EXPECT_EQ(file_system.Open(filename, &file), DiskStatus::kOk);
  std::vector<uint16_t> words(entry.GetSize().value_or(0));
  int read = 0;
  EXPECT_EQ(file_system.Read(file, absl::MakeSpan(words), &read),
            DiskStatus::kOk);
  words.resize(read);
  EXPECT_EQ(file_system.Close(file, DateTime()), DiskStatus::kOk);
  return std::make_pair(entry, std::move(words));
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

bool RunUntilCore0Halts(Ozzy& ozzy) {
  for (int run = 0; run < 10'000 && !IsHalted(ozzy); ++run) {
    ozzy.Run(absl::Milliseconds(1));
  }
  EXPECT_TRUE(IsHalted(ozzy)) << "Core 0 didn't halt";
  return IsHalted(ozzy);
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
