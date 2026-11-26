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

// Where the OS goes, after the loader and the boot ROM's stack in page 1.
constexpr int kOsStart = kBootRomStackEnd;

// The loader's own error codes (see the manual's Ozzy Computer page). A drive
// error is the drive's status.
constexpr uint16_t kNotBare = 0xF1;
constexpr uint16_t kWrongSize = 0xF2;
constexpr uint16_t kTooBig = 0xF3;

// The error a drive gives without the file the tests look for.
constexpr uint16_t kNotFound = static_cast<uint16_t>(DiskStatus::kNotFound);

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

// The words from kOsStart to the end of page `page`.
constexpr int GetWordsTo(int page) {
  return (page + 1) * kMemoryBankPageSize - kOsStart;
}

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

// Returns the value of a constant that is an address from the loader's start,
// in bank 0, as the source has it.
uint32_t FromLoader(int address) {
  return static_cast<uint32_t>(address - kLoaderStart);
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

// Returns the words of the test OS's file, with a header that says its
// contents and stack are `contents` and `stack` words.
std::vector<uint16_t> GetOsFileSaying(uint16_t contents, uint16_t stack) {
  std::vector<uint16_t> os_file = GetOsFile(0, 0);
  if (os_file.size() < 4) {
    ADD_FAILURE() << "The test OS has no header";
    return {};
  }
  os_file[2] = contents;
  os_file[3] = stack;
  return os_file;
}

// Returns the text of the screen's row `row`.
std::string GetScreenRow(Ozzy& ozzy, int row) {
  return GetRowText(ozzy.GetScreen()->GetPicture(), row);
}

// Returns the word at `address` of `bank`.
uint16_t GetWord(Ozzy& ozzy, int address, int bank = 0) {
  return ozzy.GetProcessor()->GetMemory(bank)->GetMem(address, 1)[0];
}

// Returns whether the boot ROM is waiting for Enter after the loader failed.
bool IsWaitingAfterError(Ozzy& ozzy) {
  return GetState(ozzy) == CpuCore::State::kWaiting &&
         GetWord(ozzy, kLoaderErrorAddress) != 0;
}

// Expects the test OS, with `gap` and `stack` words, to have been started by
// the loader and halted: at kOsStart in bank 0, in pages 1 to `last_page`, or
// at 0 in another `bank`, in pages 0 to `last_page`.
void ExpectOsRan(Ozzy& ozzy, int gap, int stack, int last_page, int bank = 0) {
  ASSERT_TRUE(IsHalted(ozzy));
  const int size = kOsWords + gap + stack;
  const int start = bank == 0 ? kOsStart : 0;
  const int first_page = bank == 0 ? 1 : 0;

  const CpuCore::Registers registers = GetRegisters(ozzy);
  const CpuCore::Banks banks = CpuCore::Banks::FromWord(registers[CpuCore::MB]);
  EXPECT_EQ(banks.code, bank);
  EXPECT_EQ(banks.stack, bank);
  EXPECT_EQ(banks.data, bank);
  EXPECT_EQ(banks.extra, 0);
  EXPECT_EQ(registers[CpuCore::BC], start);
  EXPECT_EQ(registers[CpuCore::BD], start);
  EXPECT_EQ(registers[CpuCore::BS], static_cast<uint16_t>(start + size));
  EXPECT_EQ(registers[CpuCore::BE], 0);
  EXPECT_EQ(registers[CpuCore::SP], 0);
  EXPECT_EQ(registers[CpuCore::IP], 0);  // At its HALT.
  EXPECT_EQ(registers[CpuCore::R0], kOzzyDrivePort);
  EXPECT_EQ(registers[CpuCore::R1], 0);
  EXPECT_EQ(registers[CpuCore::R2],
            static_cast<uint16_t>((2 << last_page) - (1 << first_page)));
  EXPECT_EQ(registers[CpuCore::ST] & CpuCore::I, 0);

  // Each block is where it goes.
  EXPECT_EQ(GetWord(ozzy, start + 1, bank), 0x1234);
  EXPECT_EQ(GetWord(ozzy, start + 2 + gap, bank), 0x5678);
  EXPECT_EQ(GetWord(ozzy, start + 3 + gap, bank), 0x9ABC);
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
      return GetState(ozzy) == CpuCore::State::kWaiting;
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

  // Runs `ozzy` until the loader has failed, and the boot ROM waits for Enter
  // again, and expects the boot ROM to show the loader's error `code`: on the
  // screen, the banner and prompt as at power on, with the error on the status
  // row, or on the teletype without one, the error and then the prompt again.
  // The Ozzy never asks to restart.
  void ExpectError(Ozzy& ozzy, uint16_t code) {
    std::string output;
    RunUntil(ozzy, [&output](Ozzy& ozzy) {
      if (ozzy.GetTeletype() != nullptr) {
        output += ozzy.GetTeletype()->TakeOutput();
      }
      return IsWaitingAfterError(ozzy) &&
             (ozzy.GetTeletype() == nullptr || output.ends_with(kBootPrompt));
    });
    EXPECT_EQ(GetWord(ozzy, kLoaderErrorAddress), code);
    if (ozzy.GetScreen() != nullptr) {
      EXPECT_EQ(GetScreenRow(ozzy, 0), " Ozzy 1.0");
      EXPECT_EQ(GetScreenRow(ozzy, kBootPromptRow), kBootPrompt);
      EXPECT_EQ(GetScreenRow(ozzy, kBootStatusRow), GetLoaderErrorStatus(code));
      EXPECT_EQ(ozzy.GetScreen()->GetPicture().cursor_mode,
                ScreenCursorMode::kSlowBlink);
    } else if (ozzy.GetTeletype() != nullptr) {
      EXPECT_THAT(output, ::testing::HasSubstr(absl::StrCat(
                              kBootPrompt, "\r\nLoading...\r\nOzzy 1.0\r\n")));
      EXPECT_THAT(output,
                  ::testing::EndsWith(absl::StrCat(
                      "\r\n\r\n", kBootPrompt, "\r\n",
                      GetLoaderErrorStatus(code), "\r\n\r\n", kBootPrompt)));
    }
    EXPECT_THAT(requests_, ::testing::IsEmpty());
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

TEST_F(LoaderTest, SourceKnowsTheBootRom) {
  ExpectSourceConstants({
      {"LOADER_START", kLoaderStart},
      {"STACK_END", kBootRomStackEnd},
      {"BOOT_ROM_EXIT", FromLoader(kBootRomExit)},
  });
}

TEST_F(LoaderTest, SourceKnowsTheMachineTable) {
  ExpectSourceConstants({
      {"TABLE", FromLoader(kMachineTableAddress)},
      {"TABLE_BANK_ENTRIES",
       FromLoader(kMachineTableAddress + kMachineTableBanks)},
      {"BANK_FIELD_RAM", kMachineBankRam},
      {"BANK_ENTRY_SIZE", kMachineBankSize},
      {"TABLE_BANK0_RAM",
       FromLoader(kMachineTableAddress + kMachineTableBanks + kMachineBankRam)},
      {"TABLE_BANK1_RAM", FromLoader(kMachineTableAddress + kMachineTableBanks +
                                     kMachineBankSize + kMachineBankRam)},
  });
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
  });
}

TEST_F(LoaderTest, ErrorCodesAreNotDriveStatuses) {
  // The loader's own codes are past every status the drive has.
  for (uint16_t code : {kNotBare, kWrongSize, kTooBig}) {
    EXPECT_GT(code, static_cast<uint16_t>(DiskStatus::kIoError));
  }
}

//==============================================================================
// Loading
//==============================================================================

TEST_F(LoaderTest, StartsTheOs) {
  // The OS goes at 1200h, after the loader and its stack in page 1 of bank 0,
  // and its gap spans pages.
  std::unique_ptr<Ozzy> ozzy =
      Boot(OzzySettings(), MakeImage(GetOsFile(0x3000, 0x20)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, 0x3000, 0x20, 4);
  EXPECT_THAT(requests_, ::testing::IsEmpty());
}

TEST_F(LoaderTest, SmallestOs) {
  std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(GetOsFile(0, 0)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, 0, 0, 1);
}

TEST_F(LoaderTest, FitsInTheLoadersPage) {
  // With only page 1 of RAM, an OS can have the rest of it.
  constexpr int kStack = 0x100;
  constexpr int kGap = GetWordsTo(1) - kOsWords - kStack;
  OzzySettings settings;
  settings.banks = {0x0002};
  std::unique_ptr<Ozzy> ozzy =
      Boot(settings, MakeImage(GetOsFile(kGap, kStack)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, kGap, kStack, 1);
}

TEST_F(LoaderTest, FillsTheLastRamPages) {
  // The default Ozzy's RAM is pages 1 to 14, so the OS has them all. It goes
  // in bank 0, as it fits there, even with all of bank 1 free.
  constexpr int kStack = 0x100;
  constexpr int kGap = GetWordsTo(14) - kOsWords - kStack;
  OzzySettings settings;
  settings.banks = {0x7FFE, 0xFFFF};
  std::unique_ptr<Ozzy> ozzy =
      Boot(settings, MakeImage(GetOsFile(kGap, kStack)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, kGap, kStack, 14);
}

TEST_F(LoaderTest, FillsTheBank) {
  // Without a screen, RAM can be pages 1 to 15, so the OS can end with the
  // bank, where its stack wraps BS to 0.
  constexpr int kStack = 0x100;
  constexpr int kGap = GetWordsTo(15) - kOsWords - kStack;
  OzzySettings settings = GetTeletypeSettings(0);
  settings.banks = {0xFFFE};
  std::unique_ptr<Ozzy> ozzy =
      Boot(settings, MakeImage(GetOsFile(kGap, kStack)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, kGap, kStack, 15);
}

TEST_F(LoaderTest, Teletype) {
  // The OS loads the same at 9600 baud, with nothing on the teletype from the
  // loader.
  std::unique_ptr<Ozzy> ozzy =
      Boot(GetTeletypeSettings(9600), MakeImage(GetOsFile(0, 0)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, 0, 0, 1);
  EXPECT_THAT(ozzy->GetTeletype()->TakeOutput(),
              ::testing::EndsWith("\r\nLoading...\r\n"));
}

//==============================================================================
// Failing
//==============================================================================

TEST_F(LoaderTest, NoOsFile) {
  std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(std::nullopt));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy, kNotFound);
}

TEST_F(LoaderTest, EnterTriesAgain) {
  // After an error, the boot ROM boots again on Enter, and the drive is
  // reset, so a disk with the OS put in A: then loads it.
  std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(std::nullopt));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy, kNotFound);
  EXPECT_NE(ozzy->GetDrive('A')->DetachImage(), nullptr);
  ozzy->GetDrive('A')->AttachImage(MakeImage(GetOsFile(0, 0)));
  EXPECT_TRUE(ozzy->GetKeyboard()->TypeKey(kKeyEnter));
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, 0, 0, 1);
}

TEST_F(LoaderTest, KeysWhileLoadingAreThrownAway) {
  // Keys typed while the loader runs don't count toward the next prompt, so
  // the boot ROM still waits after the error.
  std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(std::nullopt));
  ASSERT_NE(ozzy, nullptr);
  ozzy->Run(absl::Milliseconds(1));
  EXPECT_TRUE(ozzy->GetKeyboard()->TypeKey(kKeyEnter));
  ExpectError(*ozzy, kNotFound);
  ozzy->Run(4 * kScreenRefreshTime);
  EXPECT_TRUE(IsWaitingAfterError(*ozzy));
  EXPECT_EQ(GetScreenRow(*ozzy, kBootStatusRow),
            GetLoaderErrorStatus(kNotFound));
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
    std::vector<uint16_t> os_file = bare_file;
    os_file[0] = id;
    os_file[1] = version;
    std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(os_file));
    ASSERT_NE(ozzy, nullptr);
    ExpectError(*ozzy, kNotBare);
  }
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
    std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings(), MakeImage(files[i]));
    ASSERT_NE(ozzy, nullptr);
    ExpectError(*ozzy, kWrongSize);
  }
}

TEST_F(LoaderTest, TooBig) {
  // One word more than pages 1 to 14 hold from 1200h, and there is no other
  // bank.
  constexpr int kGap = GetWordsTo(14) - kOsWords;
  std::unique_ptr<Ozzy> ozzy =
      Boot(OzzySettings(), MakeImage(GetOsFile(kGap, 1)));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy, kTooBig);
}

TEST_F(LoaderTest, TooBigForTheBank) {
  // Every page is RAM, but the OS's contents and stack are more than a bank,
  // which wraps to a page, or would pass its end from 1200h, and there is no
  // other bank. It fails before reading any block, so its blocks needn't match.
  OzzySettings settings = GetTeletypeSettings(0);
  settings.banks = {0xFFFE};
  for (auto [contents, stack] : {std::pair<uint16_t, uint16_t>{0xF000, 0x2000},
                                 std::pair<uint16_t, uint16_t>{0xEE00, 1},
                                 std::pair<uint16_t, uint16_t>{0, 0xFFFF}}) {
    SCOPED_TRACE(absl::StrFormat("%04X and %04X", contents, stack));
    std::unique_ptr<Ozzy> ozzy =
        Boot(settings, MakeImage(GetOsFileSaying(contents, stack)));
    ASSERT_NE(ozzy, nullptr);
    ExpectError(*ozzy, kTooBig);
  }
}

TEST_F(LoaderTest, NotRam) {
  // Page 4 isn't RAM, so the OS can't have pages 1 to 4.
  OzzySettings settings;
  settings.banks = {0x7FEE};
  std::unique_ptr<Ozzy> ozzy =
      Boot(settings, MakeImage(GetOsFile(GetWordsTo(3), 0)));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy, kTooBig);

  // Up to page 3 is fine.
  ozzy = Boot(settings, MakeImage(GetOsFile(GetWordsTo(3) - kOsWords, 0)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, GetWordsTo(3) - kOsWords, 0, 3);
}

TEST_F(LoaderTest, LoadsInAnotherBank) {
  // Too big for bank 0 from 1200h, so it goes at 0 in the first bank with RAM
  // from page 0 to its end, which needn't be the next bank.
  constexpr int kStack = 0x101;
  constexpr int kGap = GetWordsTo(14) - kOsWords - kStack + 1;
  constexpr int kLastPage =
      (kOsWords + kGap + kStack - 1) / kMemoryBankPageSize;
  static_assert(kLastPage == 13);
  OzzySettings settings;
  settings.banks = {0x7FFE, 0x0000, 0x1FFF, 0xFFFE, 0x3FFF, 0xFFFF};
  std::unique_ptr<Ozzy> ozzy =
      Boot(settings, MakeImage(GetOsFile(kGap, kStack)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, kGap, kStack, kLastPage, 4);
  EXPECT_THAT(requests_, ::testing::IsEmpty());
}

TEST_F(LoaderTest, FillsAnotherBank) {
  // In another bank, it can have all but the bank's last word, as its contents
  // and stack are at most FFFFh words.
  constexpr int kStack = 0xFF;
  constexpr int kGap = kMemoryBankPageSize * 16 - kOsWords - kStack - 1;
  OzzySettings settings;
  settings.banks = {0x7FFE, 0xFFFF};
  std::unique_ptr<Ozzy> ozzy =
      Boot(settings, MakeImage(GetOsFile(kGap, kStack)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, kGap, kStack, 15, 1);
}

TEST_F(LoaderTest, LoadsInTheLastBank) {
  OzzySettings settings;
  settings.banks.assign(kMaxMemoryBanks, 0);
  settings.banks[0] = 0x0002;
  settings.banks[kMaxMemoryBanks - 1] = 0x0001;
  std::unique_ptr<Ozzy> ozzy =
      Boot(settings, MakeImage(GetOsFile(GetWordsTo(1), 0)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, GetWordsTo(1), 0, 0, kMaxMemoryBanks - 1);
}

TEST_F(LoaderTest, NoBankHasRoom) {
  // Every bank has RAM in pages 0 to 14 (1 to 14 in bank 0), so an OS one word
  // too big for bank 0 goes in bank 1, but one word more than 15 pages fits in
  // none.
  OzzySettings settings;
  settings.banks.assign(kMaxMemoryBanks, 0x7FFF);
  settings.banks[0] = 0x7FFE;
  std::unique_ptr<Ozzy> ozzy =
      Boot(settings, MakeImage(GetOsFile(GetWordsTo(14) - kOsWords, 1)));
  ASSERT_NE(ozzy, nullptr);
  RunUntilHalted(*ozzy);
  ExpectOsRan(*ozzy, GetWordsTo(14) - kOsWords, 1, 13, 1);

  ozzy = Boot(settings,
              MakeImage(GetOsFile(kMemoryBankPageSize * 15 - kOsWords, 1)));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy, kTooBig);
}

TEST_F(LoaderTest, TeletypeError) {
  for (int baud : {0, 9600}) {
    SCOPED_TRACE(baud);
    std::unique_ptr<Ozzy> ozzy =
        Boot(GetTeletypeSettings(baud), MakeImage(std::nullopt));
    ASSERT_NE(ozzy, nullptr);
    ExpectError(*ozzy, kNotFound);
  }
}

TEST_F(LoaderTest, NoScreenOrTeletype) {
  // The boot ROM still has the error, and waits for Enter.
  OzzySettings settings;
  settings.screen.reset();
  std::unique_ptr<Ozzy> ozzy = Boot(settings, MakeImage(std::nullopt));
  ASSERT_NE(ozzy, nullptr);
  ExpectError(*ozzy, kNotFound);
}

TEST_F(LoaderTest, DiskErrors) {
  // A read that fails at any point fails the load, never hanging: from the
  // directory to the OS file's last sector. The error is the drive's status,
  // or for a read that fails part way through the file, what the loader makes
  // of the words it was given. Enough reads loads it.
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
      return IsHalted(ozzy) || IsWaitingAfterError(ozzy);
    });
    if (IsHalted(*ozzy)) {
      ExpectOsRan(*ozzy, 0x2000, 0, 3);
      break;
    }
    const uint16_t code = GetWord(*ozzy, kLoaderErrorAddress);
    EXPECT_THAT(code,
                ::testing::AnyOf(static_cast<uint16_t>(DiskStatus::kIoError),
                                 kNotBare, kWrongSize));
    ExpectError(*ozzy, code);
  }

  // The directory, and the OS file's sectors, each failed in turn.
  EXPECT_GT(reads, 3);
}

}  // namespace
}  // namespace oz3
