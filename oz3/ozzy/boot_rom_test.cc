// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/boot_rom.h"

#include <algorithm>
#include <cstdint>
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
#include "absl/types/span.h"
#include "gb/base/context.h"
#include "gb/file/file_system.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/date_time.h"
#include "oz3/core/processor.h"
#include "oz3/devices/disk_drive.h"
#include "oz3/devices/disk_drive_command.h"
#include "oz3/devices/keyboard.h"
#include "oz3/devices/screen.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/disk_superblock.h"
#include "oz3/disk/faulty_file_protocol.h"
#include "oz3/ozzy/machine_table.h"
#include "oz3/ozzy/ozzy.h"
#include "oz3/ozzy/ozzy_settings.h"
#include "oz3/ozzy/ozzy_testing.h"
#include "oz3/tools/program_assembler.h"

namespace oz3 {
namespace {

//==============================================================================
// Constants
//==============================================================================

// What the boot ROM shows.
constexpr std::string_view kVersion = "Ozzy 1.0";
constexpr std::string_view kDefaultHardware = "1 core  56K RAM  1 MHz";
constexpr std::string_view kNoDrives = "Drives: none";
constexpr std::string_view kNoDrive = "ERROR: No drive A:";
constexpr std::string_view kNoDisk = "ERROR: No disk in drive A:";
constexpr std::string_view kUnformatted = "ERROR: Disk not formatted";
constexpr std::string_view kNotSystemDisk = "ERROR: Not a system disk";

// The banner's rows on the screen, in a box, above the prompt.
constexpr int kBannerRows = 4;

// The default screen's rows.
constexpr int kRows = 24;

// The sectors of the images the tests make.
constexpr int kSectors = 720;

// Long enough for the screen to refresh, 30 times a second.
constexpr absl::Duration kScreenRefreshTime = absl::Milliseconds(34);

// Long enough for the teletype to print a character at 9600 baud, the slowest
// the tests use.
constexpr absl::Duration kTeletypeCharacterTime = absl::Milliseconds(2);

// The most cycles the boot ROM takes to settle.
constexpr Cycles kMaxSettleCycles = 10'000'000;

// A loader that shows it ran, where the screen's page is in the default Ozzy,
// then halts: it writes its own word 'L' to the first cell, and the next key
// from the keyboard, or 0, to the second, keeping R0 and R1. It runs on the
// boot ROM's stack, so it needs no stack of its own.
constexpr std::string_view kLoaderSource = R"(
    .STACK 0
    MOV R2, D(letter)
    MOV E(%d), R2
    MVQ R3, 0
    INS %d, R3
    MOV E(%d), R3
    HALT
letter:
    .DW 'L'
    .DS %d - $
    .DW "BOOT"
)";

// A loader that fails with the error code R0 holds: it jumps to the boot ROM's
// exit, which is 0F002h from its own BC of 1000h.
constexpr std::string_view kFailingLoaderSource = R"(
    .STACK 0
    MOV R0, %d
    JP %d
    .DS %d - $
    .DW "BOOT"
)";

//==============================================================================
// Helpers
//==============================================================================

// Expects the boot ROM's source to have each of `expected`'s constants, with
// its value.
void ExpectSourceConstants(std::map<std::string_view, uint32_t> expected) {
  oz3::ExpectSourceConstants(GetBootRom().GetMap(), std::move(expected));
}

// Returns whether core 0 is waiting for Enter, or has halted.
bool IsSettled(Ozzy& ozzy) {
  return IsHalted(ozzy) || GetState(ozzy) == CpuCore::State::kWaiting;
}

// Runs `ozzy` until core 0 is waiting for Enter or halts, and long enough after
// for the screen to refresh, or without a screen, for the teletype to print the
// character its port holds. It runs at least a millisecond first, for keys
// just typed to reach it.
void Settle(Ozzy& ozzy) {
  // In steps of a millisecond, up to kMaxSettleCycles, so a fast clock doesn't
  // run far past it.
  const Processor* processor = ozzy.GetProcessor();
  const Cycles end_cycles = processor->GetCycles() + kMaxSettleCycles;
  do {
    ozzy.Run(absl::Milliseconds(1));
  } while (!IsSettled(ozzy) && processor->GetCycles() < end_cycles);
  EXPECT_TRUE(IsSettled(ozzy)) << "The boot ROM didn't settle";
  ozzy.Run(ozzy.GetScreen() != nullptr ? kScreenRefreshTime
                                       : kTeletypeCharacterTime);
}

// Presses each of `keys`, and settles `ozzy`.
void PressKeys(Ozzy& ozzy, absl::Span<const uint16_t> keys) {
  for (uint16_t key : keys) {
    EXPECT_TRUE(ozzy.GetKeyboard()->TypeKey(key));
  }
  Settle(ozzy);
}

// Presses Enter, and settles `ozzy`.
void PressEnter(Ozzy& ozzy) { PressKeys(ozzy, {kKeyEnter}); }

// Returns the rows the boot ROM shows on the default screen, for an Ozzy with
// `hardware` and `drives`, and `status`. The banner's lines start a column in.
std::vector<std::string> GetScreenRows(std::string_view hardware,
                                       std::string_view drives,
                                       std::string_view status = "") {
  std::vector<std::string> rows(kRows);
  rows[0] = absl::StrCat(" ", kVersion);
  rows[2] = absl::StrCat(" ", hardware);
  rows[3] = absl::StrCat(" ", drives);
  rows[kBootPromptRow] = kBootPrompt;
  rows[kBootStatusRow] = status;
  return rows;
}

// Returns the box's last column, for an Ozzy with `hardware` and `drives`: the
// blank column after the banner's widest line.
int GetBoxRight(std::string_view hardware, std::string_view drives) {
  const int widest = static_cast<int>(
      std::max({kVersion.size(), hardware.size(), drives.size()}));
  return widest + 1;
}

// Expects the screen to show `rows`, each without the spaces at its end, and
// the box's lines on the banner's rows from the first column to `box_right`,
// where they are on the screen, and no other lines.
void ExpectScreen(Ozzy& ozzy, const std::vector<std::string>& rows,
                  int box_right) {
  ASSERT_NE(ozzy.GetScreen(), nullptr);
  const ScreenPicture& picture = ozzy.GetScreen()->GetPicture();
  ASSERT_EQ(static_cast<int>(rows.size()), picture.rows);
  for (int row = 0; row < picture.rows; ++row) {
    EXPECT_EQ(GetRowText(picture, row), rows[row]) << "Row " << row;
    for (int column = 0; column < picture.columns; ++column) {
      uint16_t lines = 0;
      if (row < kBannerRows && column <= box_right) {
        if (row == 0) {
          lines |= kScreenLineTop;
        }
        if (row == kBannerRows - 1) {
          lines |= kScreenLineBottom;
        }
        if (column == 0) {
          lines |= kScreenLineLeft;
        }
        if (column == box_right) {
          lines |= kScreenLineRight;
        }
      }
      const uint16_t cell = picture.cells[row * picture.columns + column];
      EXPECT_EQ(cell & 0xFF00, lines) << "Row " << row << ", column " << column;
    }
  }
}

// Expects the screen's cursor to blink after the prompt, as it does while the
// boot ROM waits for Enter.
void ExpectPromptCursor(Ozzy& ozzy) {
  ASSERT_NE(ozzy.GetScreen(), nullptr);
  const ScreenPicture& picture = ozzy.GetScreen()->GetPicture();
  EXPECT_EQ(picture.cursor_column, static_cast<int>(kBootPrompt.size()));
  EXPECT_EQ(picture.cursor_row, kBootPromptRow);
  EXPECT_EQ(picture.cursor_mode, ScreenCursorMode::kSlowBlink);
  EXPECT_EQ(picture.cursor_style, ScreenCursorStyle::kUnderline);
}

// Expects the default screen to show the boot ROM waiting for Enter, for an
// Ozzy with `hardware` and `drives`, with `status`.
void ExpectWaiting(Ozzy& ozzy, std::string_view hardware,
                   std::string_view drives, std::string_view status = "") {
  ExpectScreen(ozzy, GetScreenRows(hardware, drives, status),
               GetBoxRight(hardware, drives));
  ExpectPromptCursor(ozzy);
  EXPECT_EQ(GetState(ozzy), CpuCore::State::kWaiting);
}

// Returns the banner on the teletype, for an Ozzy with `hardware` and
// `drives`.
std::string GetTeletypeBanner(std::string_view hardware,
                              std::string_view drives) {
  return absl::StrCat(kVersion, "\r\n\r\n", hardware, "\r\n", drives, "\r\n");
}

// Returns a boot area of all 0.
DiskSuperblock::BootArea GetBlankBootArea() {
  return DiskSuperblock::BootArea(DiskSuperblock::kBootAreaWords, 0);
}

// Returns the test loader in its boot area, assembled once, as it never
// changes.
const DiskSuperblock::BootArea& GetLoaderBootArea() {
  static const DiskSuperblock::BootArea* const s_boot_area = [] {
    const int screen = kOzzyScreenPage * kMemoryBankPageSize;
    std::unique_ptr<AsmProgram> program = AssembleTestProgram(
        absl::StrFormat(kLoaderSource, screen, kOzzyKeyboardPort, screen + 1,
                        DiskSuperblock::kBootAreaWords -
                            static_cast<int>(kBootSignature.size())));
    return new DiskSuperblock::BootArea(
        program != nullptr ? GetBootArea(*program) : GetBlankBootArea());
  }();
  return *s_boot_area;
}

// Returns a loader that fails with `code`, in its boot area.
DiskSuperblock::BootArea GetFailingBootArea(int code) {
  std::unique_ptr<AsmProgram> program = AssembleTestProgram(
      absl::StrFormat(kFailingLoaderSource, code,
                      static_cast<uint16_t>(kBootRomExit - kLoaderStart),
                      DiskSuperblock::kBootAreaWords -
                          static_cast<int>(kBootSignature.size())));
  return program != nullptr ? GetBootArea(*program) : GetBlankBootArea();
}

// Expects the test loader to have run and halted, started as the boot ROM
// starts every loader: in bank 0 at 1000h, with the boot ROM's stack, and no
// key from the keyboard.
void ExpectLoaderRan(Ozzy& ozzy) {
  ASSERT_TRUE(IsHalted(ozzy));
  const CpuCore::Registers registers = GetRegisters(ozzy);
  EXPECT_EQ(registers[CpuCore::MB], 0);
  EXPECT_EQ(registers[CpuCore::BC], kLoaderStart);
  EXPECT_EQ(registers[CpuCore::BD], kLoaderStart);
  EXPECT_EQ(registers[CpuCore::BS], kBootRomStackEnd);
  EXPECT_EQ(registers[CpuCore::BE], 0);
  EXPECT_EQ(registers[CpuCore::SP], 0);
  EXPECT_EQ(registers[CpuCore::R0], kOzzyDrivePort);
  EXPECT_EQ(registers[CpuCore::R1], 0);
  EXPECT_EQ(registers[CpuCore::ST] & CpuCore::I, 0);

  // None of the interrupts the boot ROM's work raised is still pending, such
  // as READ BOOT's completion, or the keyboard's for the keys it threw away.
  const CpuCore* core = ozzy.GetProcessor()->GetCore(0);
  const uint32_t pending = core->GetInterrupts() | core->GetSharedInterrupts();
  EXPECT_EQ(pending & (1 << kOzzyDriveInterrupt), 0);
  EXPECT_EQ(pending & (1 << kOzzyKeyboardInterrupt), 0);

  // It wrote its own word, and 0 for no key, over the box's first two cells,
  // and the cursor is hidden.
  if (ozzy.GetScreen() == nullptr) {
    return;
  }
  const ScreenPicture& picture = ozzy.GetScreen()->GetPicture();
  EXPECT_EQ(picture.cells[0], 'L');
  EXPECT_EQ(picture.cells[1], 0);
  EXPECT_EQ(GetRowText(picture, 0).substr(2), kVersion.substr(1));
  EXPECT_EQ(GetRowText(picture, kBootPromptRow), kBootPrompt);
  EXPECT_EQ(picture.cursor_mode, ScreenCursorMode::kOff);
}

//==============================================================================
// BootRomTest
//==============================================================================

// Makes the disk images the tests put in the Ozzy's drives, in a host whose
// reads can fail.
class BootRomTest : public ::testing::Test {
 protected:
  BootRomTest() {
    auto protocol = std::make_unique<FaultyFileProtocol>();
    protocol_ = protocol.get();
    host_.Register(std::move(protocol));
  }

  // Returns a new image, formatted with `boot_area` if it has a value, or else
  // unformatted.
  std::unique_ptr<DiskImage> MakeImage(
      std::optional<DiskSuperblock::BootArea> boot_area) {
    const std::string path = absl::StrCat("mem:/", ++image_count_, ".ozd");
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&host_, path, kSectors);
    EXPECT_TRUE(image.ok()) << image.status();
    if (!image.ok()) {
      return nullptr;
    }
    if (!boot_area.has_value()) {
      return *std::move(image);
    }
    DiskFileSystem file_system({}, 1);
    file_system.AttachImage(*std::move(image));
    EXPECT_EQ(file_system.Format(*DiskLabel::Parse("SYSTEM"), 0, DateTime()),
              DiskStatus::kOk);
    EXPECT_EQ(file_system.SetBootArea(*boot_area), DiskStatus::kOk);
    EXPECT_EQ(file_system.Flush(DateTime()), DiskStatus::kOk);
    return file_system.DetachImage();
  }

  // Returns a formatted image whose boot area is all 0.
  std::unique_ptr<DiskImage> MakeBlankImage() {
    return MakeImage(GetBlankBootArea());
  }

  // Returns a formatted image whose boot area is the test loader.
  std::unique_ptr<DiskImage> MakeSystemImage() {
    return MakeImage(GetLoaderBootArea());
  }

  // Returns a formatted image whose boot area is a loader that fails with
  // `code`.
  std::unique_ptr<DiskImage> MakeFailingImage(int code) {
    return MakeImage(GetFailingBootArea(code));
  }

  // Returns an Ozzy for `settings` that has run until the boot ROM waits for
  // Enter or halts, with `image` in A: (which `settings` must have) if it isn't
  // null, or null if the Ozzy can't be created.
  std::unique_ptr<Ozzy> Boot(const OzzySettings& settings,
                             std::unique_ptr<DiskImage> image = nullptr) {
    absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
        Ozzy::Create(settings, WithPowerCallback());
    EXPECT_TRUE(ozzy.ok()) << ozzy.status();
    if (!ozzy.ok()) {
      return nullptr;
    }
    if (image != nullptr) {
      (*ozzy)->GetDrive('A')->AttachImage(std::move(image));
    }
    Settle(**ozzy);
    return *std::move(ozzy);
  }

  // Returns the default Ozzy with drives A: to D:, settled, with `image` in A:
  // if it isn't null.
  std::unique_ptr<Ozzy> BootWithDrives(
      std::unique_ptr<DiskImage> image = nullptr) {
    OzzySettings settings;
    settings.drives.resize(kOzzyDriveCount);
    return Boot(settings, std::move(image));
  }

  gb::FileSystem host_;
  FaultyFileProtocol* protocol_ = nullptr;
  int image_count_ = 0;
};

// The drives line for an Ozzy with every drive.
constexpr std::string_view kEveryDrive = "Drives: A, B, C, D";

//==============================================================================
// The source
//==============================================================================

TEST_F(BootRomTest, SourceKnowsTheMachineTable) {
  // The source's own copy of the table's layout matches machine_table.h.
  ExpectSourceConstants({
      {"TABLE", kMachineTableAddress},
      {"TABLE_CLOCK_HZ", kMachineTableAddress + kMachineHeaderClockHzLow},
      {"TABLE_CORES", kMachineTableAddress + kMachineHeaderCores},
      {"TABLE_BANKS", kMachineTableAddress + kMachineHeaderBanks},
      {"TABLE_DEVICES", kMachineTableAddress + kMachineHeaderDevices},
      {"TABLE_BANK_ENTRIES", kMachineTableAddress + kMachineTableBanks},
      {"TABLE_DEVICE_ENTRIES", kMachineTableAddress + kMachineTableDevices},
      {"BANK_RAM", kMachineBankRam},
      {"BANK_SIZE", kMachineBankSize},
      {"DEVICE_TYPE", kMachineDeviceType},
      {"DEVICE_PORT", kMachineDevicePort},
      {"DEVICE_BANK", kMachineDeviceBank},
      {"DEVICE_PAGE", kMachineDevicePage},
      {"DEVICE_COLUMNS", kMachineDeviceData},
      {"DEVICE_ROWS", kMachineDeviceData + 1},
      {"DEVICE_LETTER", kMachineDeviceData},
      {"DEVICE_SIZE", kMachineDeviceSize},
      {"SCREEN", static_cast<uint32_t>(MachineDeviceType::kScreen)},
      {"KEYBOARD", static_cast<uint32_t>(MachineDeviceType::kKeyboard)},
      {"TELETYPE", static_cast<uint32_t>(MachineDeviceType::kTeletype)},
      {"DRIVE", static_cast<uint32_t>(MachineDeviceType::kDrive)},
  });
}

TEST_F(BootRomTest, SourceKnowsTheKeyboard) {
  // The source's own copy of the keyboard's wiring and Enter matches the
  // Ozzy's. Only core 0 runs the boot ROM, so the keyboard's interrupt must
  // reach it.
  ExpectSourceConstants({
      {"KEYBOARD_PORT", kOzzyKeyboardPort},
      {"KEYBOARD_INTERRUPT", kOzzyKeyboardInterrupt},
      {"KEY_ENTER", kKeyEnter},
  });
  EXPECT_NE(kOzzyKeyboardInterruptCores & 1, 0);
}

TEST_F(BootRomTest, SourceKnowsTheScreen) {
  ExpectSourceConstants({
      {"SCREEN_CURSOR_COLUMN", ScreenMemory::kCursorColumnAddress},
      {"SCREEN_CURSOR_ROW", ScreenMemory::kCursorRowAddress},
      {"SCREEN_CURSOR", ScreenMemory::kCursorAddress},
      {"SCREEN_FILL", ScreenMemory::kFillAddress},
      {"SCREEN_FILL_ROW", ScreenMemory::kFillRowAddress},
      {"CURSOR_STYLE",
       static_cast<uint32_t>(ScreenCursorMode::kSlowBlink) |
           static_cast<uint32_t>(ScreenCursorStyle::kUnderline)},
      {"CURSOR_OFF", static_cast<uint32_t>(ScreenCursorMode::kOff)},
      {"LINE_TOP", kScreenLineTop},
      {"LINE_BOTTOM", kScreenLineBottom},
      {"LINE_LEFT", kScreenLineLeft},
      {"LINE_RIGHT", kScreenLineRight},
      {"BANNER_ROWS", kBannerRows},
      {"STATUS_ROW", kBootStatusRow},
  });
}

TEST_F(BootRomTest, SourceKnowsTheDrive) {
  // The source's own copy of the drive's ports, control word, commands, and
  // statuses matches the drive's, and its boot drive is A:.
  ExpectSourceConstants({
      {"DRIVE_DATA", DiskDrive::kDataPort - DiskDrive::kCommandPort},
      {"DRIVE_CONTROL", DiskDrive::kControlPort - DiskDrive::kCommandPort},
      {"DRIVE_RESET", DiskDrive::kResetRequest},
      {"DRIVE_PHASE", DiskDrive::kPhaseMask},
      {"PHASE_WORKING", static_cast<uint32_t>(DiskDrivePhase::kWorking)},
      {"READ_BOOT", static_cast<uint32_t>(DiskDriveCommand::kReadBoot)
                        << DiskDrive::kCommandShift},
      {"NO_DISK", static_cast<uint32_t>(DiskStatus::kNoDisk)},
      {"UNFORMATTED", static_cast<uint32_t>(DiskStatus::kUnformatted)},
      {"BOOT_LETTER", static_cast<uint32_t>(GetOzzyDriveLetter(0))},
      {"BOOT_DRIVE", 0},
  });
}

TEST_F(BootRomTest, SourceKnowsTheLoader) {
  // The loader is the boot area, read as dwords to 1000h, just below the end of
  // the stack, and the signature its last two words. The exit a loader jumps to
  // when it fails is at 0002h, and the error code it leaves is kept where the
  // loader tests look for it.
  ExpectSourceConstants({
      {"BOOT_AREA_DWORDS", DiskSuperblock::kBootAreaWords / 2},
      {"LOADER_START", kLoaderStart},
      {"STACK_END", kBootRomStackEnd},
      {"EXIT_ADDRESS", kBootRomExit},
      {"LOADER", static_cast<uint32_t>(kLoaderStart - kBootRomStackEnd)},
      {"V_ERROR",
       static_cast<uint32_t>(kLoaderErrorAddress - kBootRomStackEnd)},
      {"SIGNATURE_LOW", kBootSignature[0]},
      {"SIGNATURE_HIGH", kBootSignature[1]},
  });
}

//==============================================================================
// The banner on the screen
//==============================================================================

TEST_F(BootRomTest, DefaultOzzy) {
  std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  ExpectWaiting(*ozzy, kDefaultHardware, kNoDrives);
}

TEST_F(BootRomTest, Cores) {
  OzzySettings settings;
  settings.cores = 2;
  std::unique_ptr<Ozzy> ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectWaiting(*ozzy, "2 cores  56K RAM  1 MHz", kNoDrives);

  // The other cores are left idle.
  EXPECT_EQ(ozzy->GetProcessor()->GetCore(1)->GetState(),
            CpuCore::State::kIdle);

  settings.cores = kMaxCores;
  ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectWaiting(*ozzy, "8 cores  56K RAM  1 MHz", kNoDrives);
}

TEST_F(BootRomTest, LeastRam) {
  OzzySettings settings;
  settings.banks = {0x0002};
  std::unique_ptr<Ozzy> ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectWaiting(*ozzy, "1 core  4K RAM  1 MHz", kNoDrives);
}

TEST_F(BootRomTest, MostRam) {
  // Every page but the boot ROM's and the screen's.
  OzzySettings settings;
  settings.banks.assign(kMaxMemoryBanks, 0xFFFF);
  settings.banks[0] = 0x7FFE;
  std::unique_ptr<Ozzy> ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectWaiting(*ozzy, "1 core  1016K RAM  1 MHz", kNoDrives);
}

TEST_F(BootRomTest, ClockRate) {
  // The clock rate is in the largest unit it is a whole number of.
  OzzySettings settings;
  settings.clock_hz = 4'770'000;
  std::unique_ptr<Ozzy> ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectWaiting(*ozzy, "1 core  56K RAM  4770 kHz", kNoDrives);

  settings.clock_hz = 32'768;
  ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectWaiting(*ozzy, "1 core  56K RAM  32768 Hz", kNoDrives);
}

TEST_F(BootRomTest, FastestClockRates) {
  // The most digits are a dword.
  OzzySettings settings = GetTeletypeSettings(0);
  settings.keyboard = false;
  settings.clock_hz = OzzySettings::kMaxClockHz;
  std::unique_ptr<Ozzy> ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(),
            GetTeletypeBanner("1 core  56K RAM  100 MHz", kNoDrives));

  settings.clock_hz = OzzySettings::kMaxClockHz - 1;
  ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(),
            GetTeletypeBanner("1 core  56K RAM  99999999 Hz", kNoDrives));
}

TEST_F(BootRomTest, Drives) {
  // The drives are in letter order, and the box is as wide as the widest
  // line.
  OzzySettings settings;
  settings.drives.resize(1);
  std::unique_ptr<Ozzy> ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectWaiting(*ozzy, kDefaultHardware, "Drives: A");

  settings.banks.assign(2, 0xFFFF);
  settings.banks[0] = 0x7FFE;
  settings.clock_hz = 4'770'000;
  settings.cores = 2;
  ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectWaiting(*ozzy, "2 cores  120K RAM  4770 kHz", "Drives: A");
}

TEST_F(BootRomTest, EveryDevice) {
  // The machine table has 8 devices, with the drives last. The screen is
  // used rather than the teletype.
  OzzySettings settings;
  settings.teletype = OzzyTeletypeSettings();
  settings.drives.resize(kOzzyDriveCount);
  std::unique_ptr<Ozzy> ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive);
  EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(), "");

  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive, kNoDisk);
  EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(), "");
}

TEST_F(BootRomTest, SmallScreenCutsOff) {
  // What passes the right edge or the bottom row isn't drawn, and nothing
  // scrolls, nor does any line wrap. The box's right and bottom edges are off
  // the screen.
  OzzySettings settings;
  settings.screen->columns = 10;
  settings.screen->rows = 3;
  std::unique_ptr<Ozzy> ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  const int box_right = GetBoxRight(kDefaultHardware, kNoDrives);
  const std::vector<std::string> rows = {" Ozzy 1.0", "", " 1 core  5"};
  ExpectScreen(*ozzy, rows, box_right);

  // The status is below the bottom row too.
  PressEnter(*ozzy);
  ExpectScreen(*ozzy, rows, box_right);
  EXPECT_EQ(GetState(*ozzy), CpuCore::State::kWaiting);

  // The prompt fits, but the status doesn't.
  settings.screen->rows = kBootStatusRow;
  ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  std::vector<std::string> taller_rows = {
      " Ozzy 1.0", "", " 1 core  5", " Drives: n", "", "Press Ente", ""};
  ExpectScreen(*ozzy, taller_rows, box_right);
  PressEnter(*ozzy);
  ExpectScreen(*ozzy, taller_rows, box_right);
  EXPECT_EQ(GetState(*ozzy), CpuCore::State::kWaiting);
}

TEST_F(BootRomTest, StackInPageOne) {
  // The stack grows down from 1200h in bank 0, whatever other RAM there is.
  for (uint16_t ram : {0x6FFE, 0x0002}) {
    SCOPED_TRACE(ram);
    OzzySettings settings;
    settings.banks = {ram, 0xFFFF};
    std::unique_ptr<Ozzy> ozzy = Boot(settings);
    ASSERT_NE(ozzy, nullptr);
    const CpuCore::Registers registers = GetRegisters(*ozzy);
    EXPECT_EQ(CpuCore::Banks::FromWord(registers[CpuCore::MB]).stack, 0);
    EXPECT_EQ(registers[CpuCore::BS], kBootRomStackEnd);
  }
}

//==============================================================================
// Waiting for Enter
//==============================================================================

TEST_F(BootRomTest, NoKeyboardHalts) {
  // Nothing could answer the prompt, so there is none.
  OzzySettings settings;
  settings.keyboard = false;
  std::unique_ptr<Ozzy> ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_TRUE(IsHalted(*ozzy));
  std::vector<std::string> rows = GetScreenRows(kDefaultHardware, kNoDrives);
  rows[kBootPromptRow].clear();
  ExpectScreen(*ozzy, rows, GetBoxRight(kDefaultHardware, kNoDrives));
  EXPECT_EQ(ozzy->GetScreen()->GetPicture().cursor_mode,
            ScreenCursorMode::kOff);
}

TEST_F(BootRomTest, NoScreenOrTeletype) {
  // It has nowhere to show anything, but still boots.
  OzzySettings settings;
  settings.screen.reset();
  settings.drives.resize(1);
  std::unique_ptr<Ozzy> ozzy = Boot(settings, MakeSystemImage());
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(GetState(*ozzy), CpuCore::State::kWaiting);
  PressEnter(*ozzy);
  ExpectLoaderRan(*ozzy);
}

TEST_F(BootRomTest, OtherKeysAreIgnored) {
  std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  PressKeys(*ozzy, {'a', ' ', kKeyBackspace, kKeyTab, kKeyEscape, kKeyDelete,
                    kKeyUp, kKeyInsert, kKeyF1, kKeyF12, 0x01, 0x1A, 0xFF,
                    kKeyShift | 'M', kKeyAlt | 'm'});
  ExpectWaiting(*ozzy, kDefaultHardware, kNoDrives);
}

TEST_F(BootRomTest, ModifiersMakeNoDifference) {
  // Only the key counts, so Ctrl+M, which the keyboard sends as Ctrl with
  // Enter, is Enter.
  for (uint16_t enter :
       {kKeyShift | kKeyEnter, kKeyCtrl | kKeyEnter, kKeyAlt | kKeyEnter}) {
    SCOPED_TRACE(enter);
    std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings());
    ASSERT_NE(ozzy, nullptr);
    PressKeys(*ozzy, {enter});
    ExpectWaiting(*ozzy, kDefaultHardware, kNoDrives, kNoDrive);
  }
}

TEST_F(BootRomTest, KeysBeforeThePromptCount) {
  // Keys pressed while the boot ROM shows the banner wait in the keyboard.
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(OzzySettings(), WithPowerCallback());
  ASSERT_TRUE(ozzy.ok()) << ozzy.status();
  EXPECT_TRUE((*ozzy)->GetKeyboard()->TypeKey('a'));
  EXPECT_TRUE((*ozzy)->GetKeyboard()->TypeKey(kKeyEnter));
  Settle(**ozzy);
  ExpectWaiting(**ozzy, kDefaultHardware, kNoDrives, kNoDrive);
}

TEST_F(BootRomTest, KeysWithEveryCore) {
  // The keyboard's interrupt is shared by every core, but only core 0 has a
  // handler, so the others drop it, and core 0 still sees Enter.
  OzzySettings settings;
  settings.cores = kMaxCores;
  std::unique_ptr<Ozzy> ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  PressKeys(*ozzy, {'a', kKeyEnter});
  ExpectWaiting(*ozzy, "8 cores  56K RAM  1 MHz", kNoDrives, kNoDrive);
}

TEST_F(BootRomTest, KeysWhileLoadingAreThrownAway) {
  // Keys typed while a boot fails don't count toward the next prompt, so a
  // second Enter doesn't boot again. On the teletype, each boot shows a line.
  OzzySettings settings = GetTeletypeSettings(0);
  settings.drives.resize(1);
  std::unique_ptr<Ozzy> ozzy = Boot(settings);
  ASSERT_NE(ozzy, nullptr);
  ozzy->GetTeletype()->TakeOutput();
  const std::string failed_boot =
      absl::StrCat("\r\nLoading...\r\n", kNoDisk, "\r\n\r\n", kBootPrompt);
  PressKeys(*ozzy, {kKeyEnter, 'a', kKeyEnter});
  EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(), failed_boot);
  EXPECT_EQ(GetState(*ozzy), CpuCore::State::kWaiting);

  // The next Enter boots again.
  PressEnter(*ozzy);
  EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(), failed_boot);
}

//==============================================================================
// Statuses
//==============================================================================

TEST_F(BootRomTest, NoDrive) {
  std::unique_ptr<Ozzy> ozzy = Boot(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kNoDrives, kNoDrive);

  // Again, on the same row.
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kNoDrives, kNoDrive);
}

TEST_F(BootRomTest, OnlyADiskInA) {
  // A disk in another drive doesn't boot.
  std::unique_ptr<Ozzy> ozzy = BootWithDrives();
  ASSERT_NE(ozzy, nullptr);
  ozzy->GetDrive('B')->AttachImage(MakeSystemImage());
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive, kNoDisk);
}

TEST_F(BootRomTest, Unformatted) {
  std::unique_ptr<Ozzy> ozzy = BootWithDrives(MakeImage(std::nullopt));
  ASSERT_NE(ozzy, nullptr);
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive, kUnformatted);
}

TEST_F(BootRomTest, NotASystemDisk) {
  std::unique_ptr<Ozzy> ozzy = BootWithDrives(MakeBlankImage());
  ASSERT_NE(ozzy, nullptr);
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive, kNotSystemDisk);

  // Each word of the signature counts.
  const int signature_words = static_cast<int>(kBootSignature.size());
  for (int word = 0; word < signature_words; ++word) {
    SCOPED_TRACE(word);
    DiskSuperblock::BootArea boot_area = GetBlankBootArea();
    boot_area[DiskSuperblock::kBootAreaWords - signature_words + word] =
        kBootSignature[word];
    ozzy = BootWithDrives(MakeImage(boot_area));
    ASSERT_NE(ozzy, nullptr);
    PressEnter(*ozzy);
    ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive, kNotSystemDisk);
  }
}

TEST_F(BootRomTest, DiskError) {
  // Any other status is shown by its code.
  std::unique_ptr<DiskImage> image = MakeBlankImage();
  protocol_->SetLimits({.reads = 0});
  std::unique_ptr<Ozzy> ozzy = BootWithDrives(std::move(image));
  ASSERT_NE(ozzy, nullptr);
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive,
                absl::StrFormat("ERROR: Disk error %02Xh",
                                static_cast<int>(DiskStatus::kIoError)));
}

TEST_F(BootRomTest, StatusReplacesTheLast) {
  // A shorter status clears the rest of the row.
  std::unique_ptr<Ozzy> ozzy = BootWithDrives();
  ASSERT_NE(ozzy, nullptr);
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive, kNoDisk);
  ozzy->GetDrive('A')->AttachImage(MakeImage(std::nullopt));
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive, kUnformatted);
}

//==============================================================================
// Booting
//==============================================================================

TEST_F(BootRomTest, BootsTheLoader) {
  std::unique_ptr<Ozzy> ozzy = BootWithDrives(MakeSystemImage());
  ASSERT_NE(ozzy, nullptr);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive);

  // Keys typed while it boots are thrown away, so none reach the loader.
  PressKeys(*ozzy, {kKeyEnter, 'x', kKeyEnter});
  ExpectLoaderRan(*ozzy);
}

TEST_F(BootRomTest, BootsTheLoaderWithLeastRam) {
  // Page 1 of bank 0 holds both the loader and the stack.
  OzzySettings settings;
  settings.banks = {0x0002};
  settings.drives.resize(1);
  std::unique_ptr<Ozzy> ozzy = Boot(settings, MakeSystemImage());
  ASSERT_NE(ozzy, nullptr);
  PressEnter(*ozzy);
  ExpectLoaderRan(*ozzy);
}

TEST_F(BootRomTest, RetryAfterInsertingADisk) {
  std::unique_ptr<Ozzy> ozzy = BootWithDrives();
  ASSERT_NE(ozzy, nullptr);
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive, kNoDisk);
  ozzy->GetDrive('A')->AttachImage(MakeSystemImage());
  PressEnter(*ozzy);
  ExpectLoaderRan(*ozzy);
}

//==============================================================================
// A loader that fails
//==============================================================================

TEST_F(BootRomTest, LoaderError) {
  // The boot ROM starts again, showing the banner and the loader's error code
  // as a hex byte, and waits for Enter. The code is a byte, so only R0's low
  // byte is shown.
  struct Case {
    int code;
    int shown;
  };
  for (Case c : {Case{0x01, 0x01}, Case{0x10, 0x10}, Case{0xF3, 0xF3},
                 Case{0xFF, 0xFF}, Case{0x1234, 0x34}}) {
    SCOPED_TRACE(c.code);
    std::unique_ptr<Ozzy> ozzy = BootWithDrives(MakeFailingImage(c.code));
    ASSERT_NE(ozzy, nullptr);
    PressEnter(*ozzy);
    ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive,
                  GetLoaderErrorStatus(c.shown));
  }
}

TEST_F(BootRomTest, LoaderErrorZeroIsPowerOn) {
  // A code of 0 is the boot ROM starting from power on, so it shows no error.
  std::unique_ptr<Ozzy> ozzy = BootWithDrives(MakeFailingImage(0));
  ASSERT_NE(ozzy, nullptr);
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive);
}

TEST_F(BootRomTest, RetryAfterLoaderError) {
  // Enter boots again, from whatever disk is in A: by then.
  std::unique_ptr<Ozzy> ozzy = BootWithDrives(MakeFailingImage(0x42));
  ASSERT_NE(ozzy, nullptr);
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive,
                GetLoaderErrorStatus(0x42));
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive,
                GetLoaderErrorStatus(0x42));

  // Another status replaces the error.
  ozzy->GetDrive('A')->DetachImage();
  PressEnter(*ozzy);
  ExpectWaiting(*ozzy, kDefaultHardware, kEveryDrive, kNoDisk);

  ozzy->GetDrive('A')->AttachImage(MakeSystemImage());
  PressEnter(*ozzy);
  ExpectLoaderRan(*ozzy);
}

TEST_F(BootRomTest, KeysBeforeLoaderErrorAreThrownAway) {
  // Keys typed while the boot fails, in the loader or after, don't count toward
  // the prompt the boot ROM shows after the error, so a second Enter doesn't
  // boot again.
  OzzySettings settings = GetTeletypeSettings(0);
  settings.drives.resize(1);
  std::unique_ptr<Ozzy> ozzy = Boot(settings, MakeFailingImage(0x42));
  ASSERT_NE(ozzy, nullptr);
  ozzy->GetTeletype()->TakeOutput();
  const std::string failed_boot = absl::StrCat(
      "\r\nLoading...\r\n", GetTeletypeBanner(kDefaultHardware, "Drives: A"),
      "\r\n", kBootPrompt, "\r\n", GetLoaderErrorStatus(0x42), "\r\n\r\n",
      kBootPrompt);
  PressKeys(*ozzy, {kKeyEnter, 'a', kKeyEnter});
  EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(), failed_boot);
  EXPECT_EQ(GetState(*ozzy), CpuCore::State::kWaiting);

  // The next Enter boots again.
  PressEnter(*ozzy);
  EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(), failed_boot);
}

TEST_F(BootRomTest, LoaderErrorWithNoScreenOrTeletype) {
  // It has nowhere to show the error, but still waits for Enter to boot again.
  OzzySettings settings;
  settings.screen.reset();
  settings.drives.resize(1);
  std::unique_ptr<Ozzy> ozzy = Boot(settings, MakeFailingImage(0x42));
  ASSERT_NE(ozzy, nullptr);
  PressEnter(*ozzy);
  EXPECT_EQ(GetState(*ozzy), CpuCore::State::kWaiting);
  ozzy->GetDrive('A')->AttachImage(MakeSystemImage());
  PressEnter(*ozzy);
  ExpectLoaderRan(*ozzy);
}

//==============================================================================
// The teletype
//==============================================================================

TEST_F(BootRomTest, TeletypeWithoutKeyboard) {
  // At 9600 baud, each character takes 1041 cycles, which the boot ROM waits
  // for. Without a keyboard, it halts after the banner.
  for (int baud : {0, 9600}) {
    SCOPED_TRACE(baud);
    OzzySettings settings = GetTeletypeSettings(baud);
    settings.keyboard = false;
    std::unique_ptr<Ozzy> ozzy = Boot(settings);
    ASSERT_NE(ozzy, nullptr);
    EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(),
              GetTeletypeBanner(kDefaultHardware, kNoDrives));
    EXPECT_TRUE(IsHalted(*ozzy));
  }
}

TEST_F(BootRomTest, Teletype) {
  // Each status is a line of its own, and an error is followed by a blank
  // line and the prompt again.
  for (int baud : {0, 9600}) {
    SCOPED_TRACE(baud);
    OzzySettings settings = GetTeletypeSettings(baud);
    settings.drives.resize(1);
    std::unique_ptr<Ozzy> ozzy = Boot(settings);
    ASSERT_NE(ozzy, nullptr);
    EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(),
              absl::StrCat(GetTeletypeBanner(kDefaultHardware, "Drives: A"),
                           "\r\n", kBootPrompt));
    EXPECT_EQ(GetState(*ozzy), CpuCore::State::kWaiting);

    PressEnter(*ozzy);
    EXPECT_EQ(
        ozzy->GetTeletype()->TakeOutput(),
        absl::StrCat("\r\nLoading...\r\n", kNoDisk, "\r\n\r\n", kBootPrompt));
    EXPECT_EQ(GetState(*ozzy), CpuCore::State::kWaiting);

    ozzy->GetDrive('A')->AttachImage(MakeSystemImage());
    PressEnter(*ozzy);
    EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(),
              absl::StrCat("\r\nLoading...\r\n"));
    ExpectLoaderRan(*ozzy);
  }
}

}  // namespace
}  // namespace oz3
