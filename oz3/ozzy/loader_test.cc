// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/time/time.h"
#include "gb/file/file_system.h"
#include "gb/parse/parse_error.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/date_time.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/processor.h"
#include "oz3/devices/disk_drive.h"
#include "oz3/devices/keyboard.h"
#include "oz3/devices/power_control.h"
#include "oz3/devices/screen.h"
#include "oz3/devices/teletype.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/disk_superblock.h"
#include "oz3/disk/faulty_file_protocol.h"
#include "oz3/instruction_sets/default_instruction_set.h"
#include "oz3/ozzy/loader_source.h"
#include "oz3/ozzy/ozzy.h"
#include "oz3/ozzy/ozzy_settings.h"
#include "oz3/tools/bare_program.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program_assembler.h"
#include "oz3/tools/program_file.h"

namespace oz3 {
namespace {

//==============================================================================
// Constants
//==============================================================================

// What the loader shows when it can't load the OS.
constexpr std::string_view kError = "ERROR: Can't load OZOS.SYS";

// The boot ROM's status row on the screen, where the error replaces
// "Loading...".
constexpr int kStatusRow = 7;

// The sectors of the images the tests make.
constexpr int kSectors = 720;

// The most time the tests give the loader, in steps of kStep.
constexpr absl::Duration kMaxRunTime = absl::Seconds(10);
constexpr absl::Duration kStep = absl::Milliseconds(10);

// Long enough for the screen to refresh, 30 times a second.
constexpr absl::Duration kScreenRefreshTime = absl::Milliseconds(34);

// The test OS: it halts at once, so a test sees the registers it started
// with. Its contents are its first two words, then `gap` words with no data,
// then two more, after which it has `stack` words of stack.
constexpr std::string_view kOsSource = R"(
    .STACK %d
    HALT
    .DW 1234h
    .DS %d
    .DW 5678h, 9ABCh
)";

// The test OS's contents, besides its gap.
constexpr int kOsWords = 4;

//==============================================================================
// Helpers
//==============================================================================

// Returns `source` assembled as a bare program.
BareProgram AssembleBare(std::string_view source) {
  InstructionError error;
  std::unique_ptr<InstructionSetSyntax> syntax = InstructionSetSyntax::Create(
      GetDefaultInstructionSetDef(), GetDefaultInstructionSet().get(), &error);
  EXPECT_NE(syntax, nullptr) << error.message;
  if (syntax == nullptr) {
    return {};
  }
  std::vector<gb::ParseError> errors;
  std::unique_ptr<AsmProgram> program =
      AssembleProgram(*syntax, std::string(source), &errors);
  for (const gb::ParseError& parse_error : errors) {
    ADD_FAILURE() << parse_error.FormatMessage();
  }
  if (program == nullptr) {
    return {};
  }
  const absl::StatusOr<BareProgram>& bare = program->GetBareProgram();
  EXPECT_TRUE(bare.ok()) << bare.status();
  return bare.ok() ? *bare : BareProgram();
}

// Returns the loader in its boot area, assembled once, as it never changes.
const DiskSuperblock::BootArea& GetLoaderBootArea() {
  static const DiskSuperblock::BootArea* const s_boot_area = [] {
    const BareProgram loader = AssembleBare(kLoaderSource.GetText());
    EXPECT_EQ(loader.contents_words, DiskSuperblock::kBootAreaWords);
    auto* boot_area =
        new DiskSuperblock::BootArea(DiskSuperblock::kBootAreaWords, 0);
    for (const Program::Segment::Data& data : loader.data) {
      absl::c_copy(data.data, boot_area->begin() + data.offset);
    }
    return boot_area;
  }();
  return *s_boot_area;
}

// Returns the words of the test OS's file, with `gap` and `stack` words.
std::vector<uint16_t> GetOsFile(int gap, int stack) {
  absl::StatusOr<std::vector<uint16_t>> words = WriteBareProgramWords(
      AssembleBare(absl::StrFormat(kOsSource, stack, gap)));
  EXPECT_TRUE(words.ok()) << words.status();
  return words.ok() ? *std::move(words) : std::vector<uint16_t>();
}

// Returns core 0's registers.
CpuCore::Registers GetRegisters(Ozzy& ozzy) {
  CpuCore::Registers registers;
  ozzy.GetProcessor()->GetCore(0)->GetRegisters(registers);
  return registers;
}

// Returns whether core 0 has halted, as the test OS does.
bool IsHalted(Ozzy& ozzy) {
  return ozzy.GetProcessor()->GetCore(0)->GetState() == CpuCore::State::kIdle;
}

// Returns the text of row `row` of the screen, without the spaces at its end.
std::string GetRowText(Ozzy& ozzy, int row) {
  const ScreenPicture& picture = ozzy.GetScreen()->GetPicture();
  std::string text;
  for (int column = 0; column < picture.columns; ++column) {
    text.push_back(
        static_cast<char>(picture.cells[row * picture.columns + column]));
  }
  text.erase(text.find_last_not_of(' ') + 1);
  return text;
}

// Expects the test OS, with `gap` and `stack` words, to have been started by
// the loader at page `page` of bank `bank`, and halted.
void ExpectOsRan(Ozzy& ozzy, int bank, int page, int gap, int stack) {
  ASSERT_TRUE(IsHalted(ozzy));
  const int base = page * kMemoryBankPageSize;
  const int size = kOsWords + gap + stack;
  const int pages = (size + kMemoryBankPageSize - 1) / kMemoryBankPageSize;

  const CpuCore::Registers registers = GetRegisters(ozzy);
  const CpuCore::Banks banks = CpuCore::Banks::FromWord(registers[CpuCore::MB]);
  EXPECT_EQ(banks.code, bank);
  EXPECT_EQ(banks.stack, bank);
  EXPECT_EQ(banks.data, bank);
  EXPECT_EQ(banks.extra, 0);
  EXPECT_EQ(registers[CpuCore::BC], base);
  EXPECT_EQ(registers[CpuCore::BD], base);
  EXPECT_EQ(registers[CpuCore::BS], static_cast<uint16_t>(base + size));
  EXPECT_EQ(registers[CpuCore::BE], 0);
  EXPECT_EQ(registers[CpuCore::SP], 0);
  EXPECT_EQ(registers[CpuCore::IP], 0);  // At its HALT.
  EXPECT_EQ(registers[CpuCore::R0], kOzzyDrivePort);
  EXPECT_EQ(registers[CpuCore::R1], 0);
  EXPECT_EQ(registers[CpuCore::R2], ((1 << pages) - 1) << page);
  EXPECT_EQ(registers[CpuCore::ST] & CpuCore::I, 0);

  // Each block is where it goes.
  const MemoryBank* memory = ozzy.GetProcessor()->GetMemory(bank);
  EXPECT_EQ(memory->GetMem(base + 1, 1)[0], 0x1234);
  EXPECT_EQ(memory->GetMem(base + 2 + gap, 1)[0], 0x5678);
  EXPECT_EQ(memory->GetMem(base + 3 + gap, 1)[0], 0x9ABC);
}

//==============================================================================
// LoaderTest
//==============================================================================

// Makes the disk images the tests put in the Ozzy's drive A:, in a host whose
// reads can fail, and Ozzys that record their power requests.
class LoaderTest : public ::testing::Test {
 protected:
  LoaderTest() {
    auto protocol = std::make_unique<FaultyFileProtocol>();
    protocol_ = protocol.get();
    host_.Register(std::move(protocol));
  }

  // Returns a new formatted image with the loader in its boot area, and
  // `os_file` as OZOS.SYS if it has a value.
  std::unique_ptr<DiskImage> MakeImage(
      std::optional<std::vector<uint16_t>> os_file) {
    const std::string path = absl::StrCat("mem:/", ++image_count_, ".ozd");
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&host_, path, kSectors);
    EXPECT_TRUE(image.ok()) << image.status();
    if (!image.ok()) {
      return nullptr;
    }
    DiskFileSystem file_system({}, 1);
    file_system.AttachImage(*std::move(image));
    EXPECT_EQ(file_system.Format(*DiskLabel::Parse("SYSTEM"), 0, DateTime()),
              DiskStatus::kOk);
    EXPECT_EQ(file_system.SetBootArea(GetLoaderBootArea()), DiskStatus::kOk);
    if (os_file.has_value()) {
      DiskFile file;
      EXPECT_EQ(file_system.Create(*DiskFilename::Parse("OZOS.SYS"), DateTime(),
                                   &file),
                DiskStatus::kOk);
      int count = 0;
      EXPECT_EQ(file_system.Write(file, *os_file, &count), DiskStatus::kOk);
      EXPECT_EQ(count, static_cast<int>(os_file->size()));
      EXPECT_EQ(file_system.Close(file, DateTime()), DiskStatus::kOk);
    }
    EXPECT_EQ(file_system.Flush(DateTime()), DiskStatus::kOk);
    return file_system.DetachImage();
  }

  // Returns an Ozzy for `settings`, with drive A: added if it has none, with
  // `image` in A:, which has run until the boot ROM waits for Enter, and then
  // been sent Enter. Null if the Ozzy can't be created.
  std::unique_ptr<Ozzy> Boot(OzzySettings settings,
                             std::unique_ptr<DiskImage> image) {
    if (settings.drives.empty()) {
      settings.drives.resize(1);
    }
    gb::Context context;
    context.SetNew<PowerRequestCallback>(
        [this](PowerRequest request) { requests_.push_back(request); });
    absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
        Ozzy::Create(settings, std::move(context));
    EXPECT_TRUE(ozzy.ok()) << ozzy.status();
    if (!ozzy.ok()) {
      return nullptr;
    }
    (*ozzy)->GetDrive('A')->AttachImage(std::move(image));
    RunUntil(**ozzy, [](Ozzy& ozzy) {
      return ozzy.GetProcessor()->GetCore(0)->GetState() ==
             CpuCore::State::kWaiting;
    });
    EXPECT_TRUE((*ozzy)->GetKeyboard()->TypeKey(kKeyEnter));
    return *std::move(ozzy);
  }

  // Runs `ozzy` in steps until `done` returns true for it, for at most
  // kMaxRunTime, and then long enough for the screen to refresh.
  void RunUntil(Ozzy& ozzy, const std::function<bool(Ozzy&)>& done) {
    absl::Duration time;
    while (!done(ozzy) && time < kMaxRunTime) {
      ozzy.Run(kStep);
      time += kStep;
    }
    EXPECT_TRUE(done(ozzy)) << "It didn't finish";
    ozzy.Run(kScreenRefreshTime);
  }

  // Runs `ozzy` until the OS halts.
  void RunUntilHalted(Ozzy& ozzy) { RunUntil(ozzy, IsHalted); }

  // Runs `ozzy` until the loader shows its error on the screen, or the
  // teletype without one, and expects it to restart the Ozzy once a key is
  // pressed, and not before.
  void ExpectError(Ozzy& ozzy) {
    if (ozzy.GetScreen() != nullptr) {
      RunUntil(ozzy, [](Ozzy& ozzy) {
        return GetRowText(ozzy, kStatusRow) == kError;
      });
    } else {
      std::string output;
      RunUntil(ozzy, [&output](Ozzy& ozzy) {
        output += ozzy.GetTeletype()->TakeOutput();
        return output.ends_with(absl::StrCat(kError, "\r\n"));
      });
      EXPECT_THAT(output, ::testing::EndsWith(absl::StrCat(
                              "Press Enter to Boot...\r\nLoading...\r\n",
                              kError, "\r\n")));
    }
    ozzy.Run(absl::Milliseconds(100));
    EXPECT_FALSE(IsHalted(ozzy));
    EXPECT_THAT(requests_, ::testing::IsEmpty());

    EXPECT_TRUE(ozzy.GetKeyboard()->TypeKey('x'));
    RunUntilHalted(ozzy);
    EXPECT_THAT(requests_, ::testing::ElementsAre(PowerRequest{
                               .command = PowerCommand::kRestart}));
  }

  gb::FileSystem host_;
  FaultyFileProtocol* protocol_ = nullptr;
  int image_count_ = 0;
  std::vector<PowerRequest> requests_;
};

//==============================================================================
// The source
//==============================================================================

TEST_F(LoaderTest, FillsTheBootArea) {
  // It assembles to the whole boot area, ending with the boot signature.
  const DiskSuperblock::BootArea& boot_area = GetLoaderBootArea();
  ASSERT_EQ(boot_area.size(), DiskSuperblock::kBootAreaWords);
  EXPECT_EQ(boot_area[DiskSuperblock::kBootAreaWords - 2], 0x4F42);
  EXPECT_EQ(boot_area[DiskSuperblock::kBootAreaWords - 1], 0x544F);
}

//==============================================================================
// Loading
//==============================================================================

TEST_F(LoaderTest, StartsTheOs) {
  // The OS goes in the pages after the loader's, page 1 of bank 0, and its
  // gap spans pages.
  std::unique_ptr<Ozzy> ozzy =
      Boot(OzzySettings(), MakeImage(GetOsFile(0x3000, 0x20)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, 0, 2, 0x3000, 0x20);
  EXPECT_THAT(requests_, ::testing::IsEmpty());
}

TEST_F(LoaderTest, SmallestOs) {
  std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(GetOsFile(0, 0)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, 0, 2, 0, 0);
}

TEST_F(LoaderTest, FillsTheLastRamPages) {
  // The default Ozzy's RAM is pages 1 to 14, so the OS has pages 2 to 14.
  constexpr int kStack = 0x100;
  constexpr int kGap = 13 * kMemoryBankPageSize - kOsWords - kStack;
  std::unique_ptr<Ozzy> ozzy =
      Boot(OzzySettings(), MakeImage(GetOsFile(kGap, kStack)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, 0, 2, kGap, kStack);
}

TEST_F(LoaderTest, InAnotherBank) {
  // The loader is in the first RAM page of the lowest bank with RAM, page 8
  // of bank 1, so the OS starts at page 9.
  OzzySettings settings;
  settings.banks = {0x0000, 0x0F00};
  std::unique_ptr<Ozzy> ozzy =
      Boot(settings, MakeImage(GetOsFile(0x1000, 0x10)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, 1, 9, 0x1000, 0x10);
}

TEST_F(LoaderTest, Teletype) {
  // The OS loads the same at 9600 baud, with nothing on the teletype from the
  // loader.
  OzzySettings settings;
  settings.screen.reset();
  settings.teletype = OzzyTeletypeSettings();
  settings.teletype->baud = 9600;
  std::unique_ptr<Ozzy> ozzy = Boot(settings, MakeImage(GetOsFile(0, 0)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, 0, 2, 0, 0);
  EXPECT_THAT(ozzy->GetTeletype()->TakeOutput(),
              ::testing::EndsWith("\r\nLoading...\r\n"));
}

//==============================================================================
// Failing
//==============================================================================

TEST_F(LoaderTest, NoOsFile) {
  std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(std::nullopt));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy);
}

TEST_F(LoaderTest, NotABareProgram) {
  // A program file's header, and a bare program file of another version.
  for (uint16_t version : {1, 2}) {
    SCOPED_TRACE(version);
    requests_.clear();
    std::vector<uint16_t> os_file = GetOsFile(0, 0);
    os_file[1] = version;
    if (version == 1) {
      os_file[0] = 0x5A4F;
    }
    std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(os_file));
    ASSERT_NE(ozzy, nullptr);
    ExpectError(*ozzy);
  }
}

TEST_F(LoaderTest, TooBig) {
  // One word more than pages 2 to 14 hold.
  constexpr int kGap = 13 * kMemoryBankPageSize - kOsWords;
  std::unique_ptr<Ozzy> ozzy =
      Boot(OzzySettings(), MakeImage(GetOsFile(kGap, 1)));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy);
}

TEST_F(LoaderTest, NotRam) {
  // Page 4 isn't RAM, so the OS can't have pages 2 to 4.
  OzzySettings settings;
  settings.banks = {0x7FEE};
  std::unique_ptr<Ozzy> ozzy =
      Boot(settings, MakeImage(GetOsFile(2 * kMemoryBankPageSize, 0)));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy);
}

TEST_F(LoaderTest, NotRamAtTheEndOfTheBank) {
  // With the loader on page 15, there is no page after it.
  OzzySettings settings;
  settings.screen.reset();
  settings.teletype = OzzyTeletypeSettings();
  settings.banks = {0x8000};
  std::unique_ptr<Ozzy> ozzy = Boot(settings, MakeImage(GetOsFile(0, 0)));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy);
}

TEST_F(LoaderTest, TeletypeError) {
  for (int baud : {0, 9600}) {
    SCOPED_TRACE(baud);
    requests_.clear();
    OzzySettings settings;
    settings.screen.reset();
    settings.teletype = OzzyTeletypeSettings();
    settings.teletype->baud = baud;
    std::unique_ptr<Ozzy> ozzy = Boot(settings, MakeImage(std::nullopt));
    ASSERT_NE(ozzy, nullptr);
    ExpectError(*ozzy);
  }
}

TEST_F(LoaderTest, NoScreenOrTeletype) {
  // It still waits for a key, and restarts.
  OzzySettings settings;
  settings.screen.reset();
  std::unique_ptr<Ozzy> ozzy = Boot(settings, MakeImage(std::nullopt));
  ASSERT_NE(ozzy, nullptr);
  ozzy->Run(absl::Milliseconds(100));
  EXPECT_FALSE(IsHalted(*ozzy));
  EXPECT_TRUE(ozzy->GetKeyboard()->TypeKey('x'));
  RunUntilHalted(*ozzy);
  EXPECT_THAT(requests_, ::testing::ElementsAre(
                             PowerRequest{.command = PowerCommand::kRestart}));
}

TEST_F(LoaderTest, DiskErrors) {
  // A read that fails at any point fails the load, never hanging: from the
  // directory to the OS file's last sector. Enough reads loads it.
  bool loaded = false;
  int reads = 0;
  for (; !loaded; ++reads) {
    SCOPED_TRACE(reads);
    ASSERT_LT(reads, 50);
    requests_.clear();
    protocol_->SetLimits({});
    std::unique_ptr<DiskImage> image = MakeImage(GetOsFile(0x2000, 0));
    std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), std::move(image));
    ASSERT_NE(ozzy, nullptr);
    protocol_->SetLimits({.reads = reads});
    RunUntil(*ozzy, [](Ozzy& ozzy) {
      return IsHalted(ozzy) || GetRowText(ozzy, kStatusRow) == kError;
    });
    if (IsHalted(*ozzy)) {
      ExpectOsRan(*ozzy, 0, 2, 0x2000, 0);
      loaded = true;
    } else {
      ExpectError(*ozzy);
    }
  }
  // The directory, and the OS file's sectors, each failed in turn.
  EXPECT_GT(reads, 3);
}

}  // namespace
}  // namespace oz3
