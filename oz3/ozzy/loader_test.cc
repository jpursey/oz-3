// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/time/time.h"
#include "gb/file/file_system.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/date_time.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/processor.h"
#include "oz3/devices/disk_drive.h"
#include "oz3/devices/disk_drive_command.h"
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
#include "oz3/ozzy/boot_rom.h"
#include "oz3/ozzy/loader_source.h"
#include "oz3/ozzy/machine_table.h"
#include "oz3/ozzy/ozzy.h"
#include "oz3/ozzy/ozzy_settings.h"
#include "oz3/ozzy/ozzy_testing.h"
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
// "Loading..." (SourceKnowsTheBootRom checks the loader agrees).
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

// Returns the loader, assembled once, as it never changes. Null if it doesn't
// assemble.
const AsmProgram* GetLoader() {
  static const AsmProgram* const s_loader =
      AssembleTestProgram(kLoaderSource.GetText()).release();
  return s_loader;
}

// Returns the loader in its boot area, made once.
const DiskSuperblock::BootArea& GetLoaderBootArea() {
  static const DiskSuperblock::BootArea* const s_boot_area =
      new DiskSuperblock::BootArea(
          GetLoader() != nullptr
              ? GetBootArea(*GetLoader())
              : DiskSuperblock::BootArea(DiskSuperblock::kBootAreaWords, 0));
  return *s_boot_area;
}

// Expects the loader's source to have each of `expected`'s constants, with its
// value.
void ExpectSourceConstants(std::map<std::string_view, uint32_t> expected) {
  ASSERT_NE(GetLoader(), nullptr);
  oz3::ExpectSourceConstants(GetLoader()->GetMap(), std::move(expected));
}

// Returns the words of the test OS's file, with `gap` and `stack` words.
std::vector<uint16_t> GetOsFile(int gap, int stack) {
  std::unique_ptr<AsmProgram> program =
      AssembleTestProgram(absl::StrFormat(kOsSource, stack, gap));
  if (program == nullptr || !program->GetBareProgram().ok()) {
    ADD_FAILURE() << "The test OS isn't a bare program";
    return {};
  }
  absl::StatusOr<std::vector<uint16_t>> words =
      WriteBareProgramWords(*program->GetBareProgram());
  EXPECT_TRUE(words.ok()) << words.status();
  return words.ok() ? *std::move(words) : std::vector<uint16_t>();
}

// Returns the text of the screen's status row.
std::string GetStatusText(Ozzy& ozzy) {
  return GetRowText(ozzy.GetScreen()->GetPicture(), kStatusRow);
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

  // Returns an Ozzy for `settings`, with drive A: added, with `image` in A:,
  // which has run until the boot ROM waits for Enter, and then been sent
  // Enter. Null if the Ozzy can't be created.
  std::unique_ptr<Ozzy> Boot(OzzySettings settings,
                             std::unique_ptr<DiskImage> image) {
    settings.drives.resize(1);
    absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
        Ozzy::Create(settings, WithPowerCallback({}, &requests_));
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
      RunUntil(ozzy, [](Ozzy& ozzy) { return GetStatusText(ozzy) == kError; });
    } else if (ozzy.GetTeletype() != nullptr) {
      std::string output;
      RunUntil(ozzy, [&output](Ozzy& ozzy) {
        output += ozzy.GetTeletype()->TakeOutput();
        return output.ends_with(absl::StrCat(kError, "\r\n"));
      });
      EXPECT_THAT(output, ::testing::EndsWith(absl::StrCat(
                              "Press Enter to Boot...\r\nLoading...\r\n",
                              kError, "\r\n")));
    }
    ozzy.Run(4 * kScreenRefreshTime);
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
  ASSERT_NE(GetLoader(), nullptr);
  ASSERT_TRUE(GetLoader()->GetBareProgram().ok());
  EXPECT_EQ(GetLoader()->GetBareProgram()->contents_words,
            DiskSuperblock::kBootAreaWords);
  const DiskSuperblock::BootArea& boot_area = GetLoaderBootArea();
  EXPECT_EQ(boot_area[DiskSuperblock::kBootAreaWords - 2], kBootSignature[0]);
  EXPECT_EQ(boot_area[DiskSuperblock::kBootAreaWords - 1], kBootSignature[1]);
}

TEST_F(LoaderTest, SourceKnowsTheMachineTable) {
  ExpectSourceConstants({
      {"TABLE", kMachineTableAddress},
      {"TABLE_DEVICES", kMachineTableAddress + kMachineHeaderDevices},
      {"TABLE_BANK_ENTRIES", kMachineTableAddress + kMachineTableBanks},
      {"TABLE_DEVICE_ENTRIES", kMachineTableAddress + kMachineTableDevices},
      {"BANK_RAM", kMachineBankRam},
      {"BANK_SIZE", kMachineBankSize},
      {"DEVICE_SIZE", kMachineDeviceSize},
      {"TELETYPE", static_cast<uint32_t>(MachineDeviceType::kTeletype)},
  });
  EXPECT_EQ(kMachineDeviceType, 0);
}

TEST_F(LoaderTest, SourceKnowsTheDevices) {
  ExpectSourceConstants({
      {"POWER_PORT", kOzzyPowerPort},
      {"POWER_RESTART", static_cast<uint32_t>(PowerCommand::kRestart)},
      {"KEYBOARD_PORT", kOzzyKeyboardPort},
      {"TELETYPE_PORT", kOzzyTeletypePort},
      {"SCREEN_START", kOzzyScreenPage * kMemoryBankPageSize},
      {"SCREEN_COLUMNS",
       kOzzyScreenPage * kMemoryBankPageSize + ScreenMemory::kColumnsAddress},
  });
}

TEST_F(LoaderTest, SourceKnowsTheBootRom) {
  // It shows its error where the boot ROM shows its status.
  oz3::ExpectSourceConstants(GetBootRom().GetMap(),
                             {{"STATUS_ROW", kStatusRow}});
  ExpectSourceConstants({{"STATUS_ROW", kStatusRow}});
}

TEST_F(LoaderTest, SourceKnowsTheDiskDrive) {
  ExpectSourceConstants({
      {"DRIVE_DATA", DiskDrive::kDataPort - DiskDrive::kCommandPort},
      {"DRIVE_CONTROL", DiskDrive::kControlPort - DiskDrive::kCommandPort},
      {"DRIVE_CMD_OPEN", static_cast<uint32_t>(DiskDriveCommand::kOpen)
                             << DiskDrive::kCommandShift},
      {"DRIVE_CMD_CLOSE", static_cast<uint32_t>(DiskDriveCommand::kClose)
                              << DiskDrive::kCommandShift},
      {"DRIVE_CMD_READ_PACKED",
       static_cast<uint32_t>(DiskDriveCommand::kReadPacked)
           << DiskDrive::kCommandShift},
      {"DRIVE_PHASE", DiskDrive::kPhaseMask},
      {"DRIVE_PHASE_TAKEN",
       DiskDrive::kPhaseMask &
           ~(static_cast<uint32_t>(DiskDrivePhase::kIdle) |
             static_cast<uint32_t>(DiskDrivePhase::kArguments))},
  });
}

TEST_F(LoaderTest, SourceKnowsBarePrograms) {
  ExpectSourceConstants({
      {"BARE_ID", (static_cast<uint32_t>(kBareProgramFileVersion) << 16) |
                      kBareProgramFileId},
      {"PAGE_SIZE", kMemoryBankPageSize},
  });
}

//==============================================================================
// Loading
//==============================================================================

TEST_F(LoaderTest, StartsTheOs) {
  // The OS goes in the pages after the loader's page (page 1 of bank 0), and
  // its gap spans pages.
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
  std::unique_ptr<Ozzy> ozzy =
      Boot(GetTeletypeSettings(9600), MakeImage(GetOsFile(0, 0)));
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
  const std::vector<uint16_t> bare_file = GetOsFile(0, 0);
  ASSERT_FALSE(bare_file.empty());
  for (auto [id, version] :
       {std::pair{kProgramFileId, kProgramFileVersion},
        std::pair{kBareProgramFileId,
                  static_cast<uint16_t>(kBareProgramFileVersion + 1)}}) {
    SCOPED_TRACE(absl::StrFormat("%04X version %d", id, version));
    requests_.clear();
    std::vector<uint16_t> os_file = bare_file;
    os_file[0] = id;
    os_file[1] = version;
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

TEST_F(LoaderTest, TooBigForTheBank) {
  // Its contents and stack are more than a bank, which wraps to a page.
  std::vector<uint16_t> os_file = GetOsFile(0, 0);
  ASSERT_GE(os_file.size(), 4);
  os_file[2] = 0xF000;
  os_file[3] = 0x2000;
  std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(os_file));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy);
}

TEST_F(LoaderTest, NotWhatItsHeaderSays) {
  // A file that is empty, shorter than its header, missing a block's words,
  // missing a block, or with words past its last block fails rather than
  // waiting for the drive forever.
  const std::vector<uint16_t> os_file = GetOsFile(0, 0);
  ASSERT_GE(os_file.size(), 8);
  std::vector<std::vector<uint16_t>> files = {
      {},
      std::vector<uint16_t>(os_file.begin(), os_file.begin() + 4),
      std::vector<uint16_t>(os_file.begin(), os_file.end() - 2),
      os_file,
      os_file,
  };
  files[3][4] += 1;
  files[4].insert(files[4].end(), {0, 0});
  for (int i = 0; i < static_cast<int>(files.size()); ++i) {
    SCOPED_TRACE(i);
    requests_.clear();
    std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(files[i]));
    ASSERT_NE(ozzy, nullptr);
    ExpectError(*ozzy);
  }
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
  OzzySettings settings = GetTeletypeSettings(0);
  settings.banks = {0x8000};
  std::unique_ptr<Ozzy> ozzy = Boot(settings, MakeImage(GetOsFile(0, 0)));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy);
}

TEST_F(LoaderTest, TeletypeError) {
  for (int baud : {0, 9600}) {
    SCOPED_TRACE(baud);
    requests_.clear();
    std::unique_ptr<Ozzy> ozzy =
        Boot(GetTeletypeSettings(baud), MakeImage(std::nullopt));
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
  ExpectError(*ozzy);
}

TEST_F(LoaderTest, DiskErrors) {
  // A read that fails at any point fails the load with the error, never
  // hanging: from the directory to the OS file's last sector. Enough reads
  // loads it. (NoOsFile checks the restart that follows the error.)
  const std::vector<uint16_t> os_file = GetOsFile(0x2000, 0);
  int reads = 0;
  for (;; ++reads) {
    SCOPED_TRACE(reads);
    ASSERT_LT(reads, 50);
    protocol_->SetLimits({});
    std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(os_file));
    ASSERT_NE(ozzy, nullptr);
    protocol_->SetLimits({.reads = reads});
    RunUntil(*ozzy, [](Ozzy& ozzy) {
      return IsHalted(ozzy) || GetStatusText(ozzy) == kError;
    });
    if (IsHalted(*ozzy)) {
      ExpectOsRan(*ozzy, 0, 2, 0x2000, 0);
      break;
    }
  }

  // The directory, and the OS file's sectors, each failed in turn.
  EXPECT_GT(reads, 3);
}

}  // namespace
}  // namespace oz3
