// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/ozzy.h"

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/time/civil_time.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "gb/base/context.h"
#include "gb/base/context_builder.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
#include "gb/file/memory_file_protocol.h"
#include "gb/parse/parse_error.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/date_time.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/lockable.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/memory_bank_config.h"
#include "oz3/core/port.h"
#include "oz3/core/processor.h"
#include "oz3/devices/disk_drive.h"
#include "oz3/devices/keyboard.h"
#include "oz3/devices/timer.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_status.h"
#include "oz3/instruction_sets/default_instruction_set.h"
#include "oz3/ozzy/machine_table.h"
#include "oz3/ozzy/ozzy_settings.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program_assembler.h"
#include "oz3/tools/program_loader.h"

namespace oz3 {
namespace {

using ::testing::Each;
using ::testing::ElementsAre;
using ::testing::HasSubstr;

// Returns `context` with a power request callback that ignores requests, as
// every Ozzy needs one.
gb::Context WithPowerCallback(gb::Context context = {}) {
  context.SetNew<PowerRequestCallback>([](PowerRequest) {});
  return context;
}

// Returns an Ozzy for `settings`, with `context` and a power request callback
// that ignores requests as its host's, failing the test if it can't be
// created.
std::unique_ptr<Ozzy> CreateOzzy(const OzzySettings& settings,
                                 gb::Context context = {}) {
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(settings, WithPowerCallback(std::move(context)));
  EXPECT_TRUE(ozzy.ok()) << ozzy.status();
  if (!ozzy.ok()) {
    return nullptr;
  }
  return std::move(*ozzy);
}

// Returns `count` words of the machine table from `offset`.
std::vector<uint16_t> GetTableWords(Ozzy& ozzy, int offset, int count) {
  absl::Span<const uint16_t> words = ozzy.GetProcessor()->GetMemory(0)->GetMem(
      kMachineTableAddress + offset, count);
  return std::vector<uint16_t>(words.begin(), words.end());
}

//==============================================================================
// The processor
//==============================================================================

TEST(OzzyTest, DefaultOzzy) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  Processor* processor = ozzy->GetProcessor();
  EXPECT_EQ(processor->GetClockHz(), 1'000'000);
  EXPECT_EQ(processor->GetNumCores(), 1);
  EXPECT_EQ(processor->GetNumPorts(), 48);
  EXPECT_EQ(processor->GetCycles(), 0);
  ASSERT_NE(ozzy->GetScreen(), nullptr);
  EXPECT_EQ(ozzy->GetScreen(), processor->GetDevice(Ozzy::kScreenName));
  ASSERT_NE(ozzy->GetKeyboard(), nullptr);
  EXPECT_EQ(ozzy->GetKeyboard(), processor->GetDevice(Ozzy::kKeyboardName));
  EXPECT_EQ(ozzy->GetTeletype(), nullptr);
  ASSERT_NE(ozzy->GetTimer(), nullptr);
  EXPECT_EQ(ozzy->GetTimer(), processor->GetDevice(Ozzy::kTimerName));
  for (const char letter : {'A', 'B', 'C', 'D'}) {
    EXPECT_EQ(ozzy->GetDrive(letter), nullptr) << letter;
  }
}

TEST(OzzyTest, PowerRequestsReachTheCallback) {
  std::vector<PowerRequest> requests;
  gb::Context context;
  context.SetNew<PowerRequestCallback>(
      [&requests](PowerRequest request) { requests.push_back(request); });
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(OzzySettings(), std::move(context));
  ASSERT_TRUE(ozzy.ok()) << ozzy.status();
  Processor* processor = (*ozzy)->GetProcessor();
  ASSERT_NE(processor->GetDevice(Ozzy::kPowerName), nullptr);

  // The power port is port 0, with the command in word 0 and the exit code in
  // word 1.
  {
    std::unique_ptr<Lock> lock = processor->LockPort(kOzzyPowerPort);
    ASSERT_TRUE(lock->IsLocked());
    PortWords::FromWords(1, 0xFFFD).Store(processor->GetPort(kOzzyPowerPort),
                                          *lock, Port::S);
  }
  (*ozzy)->Run(absl::Milliseconds(1));
  EXPECT_THAT(requests,
              ElementsAre(PowerRequest{.command = PowerCommand::kShutDown,
                                       .exit_code = -3}));
}

TEST(OzzyTest, TimerTicks) {
  OzzySettings settings;
  settings.timer->tick_hz = 100;
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);

  // The tick count is on port 3, at the timer's rate.
  ozzy->Run(absl::Milliseconds(25));
  EXPECT_EQ(ozzy->GetTimer()->GetTickCount(), 2);
  EXPECT_EQ(ozzy->GetProcessor()->GetPort(3).GetValue(0), 2);
}

// Asks the timer's clock port of `ozzy` for the time, and returns its answer.
std::vector<uint16_t> AskTime(Ozzy& ozzy) {
  Port& clock_port = ozzy.GetProcessor()->GetPort(kOzzyTimerPort + 1);
  {
    std::unique_ptr<Lock> lock =
        ozzy.GetProcessor()->LockPort(kOzzyTimerPort + 1);
    EXPECT_TRUE(lock->IsLocked());
    clock_port.StoreWord(*lock, Port::S, kTimerTime);
  }
  ozzy.Run(absl::Milliseconds(1));
  return {clock_port.GetValue(0), clock_port.GetValue(1)};
}

TEST(OzzyTest, TheTimerHasTheClock) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(
      OzzySettings(), gb::ContextBuilder()
                          .SetValue<HostClock>([] {
                            return absl::CivilSecond(2026, 11, 18, 9, 30, 15);
                          })
                          .Build());
  ASSERT_NE(ozzy, nullptr);
  EXPECT_THAT(AskTime(*ozzy), ElementsAre(9, 0x1E0F));
}

TEST(OzzyTest, TheTimerReadsZerosWithAnEmptyClock) {
  gb::Context context;
  context.SetValue<HostClock>(HostClock());
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(OzzySettings(), WithPowerCallback(std::move(context)));
  ASSERT_TRUE(ozzy.ok()) << ozzy.status();
  EXPECT_THAT(AskTime(**ozzy), ElementsAre(0, 0));
}

TEST(OzzyTest, KeyboardKeyState) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);

  // Key state is on ports 6 to 15: key code 08h (Backspace) is bit 8 of port
  // 6, and 9Bh (F12) bit 11 of port 15.
  ozzy->GetKeyboard()->SetKeyState(kKeyBackspace, true);
  ozzy->GetKeyboard()->SetKeyState(kKeyF12, true);
  ozzy->Run(absl::Milliseconds(1));
  Processor* processor = ozzy->GetProcessor();
  EXPECT_EQ(processor->GetPort(6).GetValue(0), 0x0100);
  EXPECT_EQ(processor->GetPort(15).GetValue(0), 0x0800);
}

TEST(OzzyTest, Processor) {
  OzzySettings settings;
  settings.clock_hz = 4'000'000;
  settings.cores = 4;
  settings.ports = 64;
  settings.screen.reset();
  settings.keyboard = false;
  settings.teletype = OzzyTeletypeSettings();
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  Processor* processor = ozzy->GetProcessor();
  EXPECT_EQ(processor->GetClockHz(), 4'000'000);
  EXPECT_EQ(processor->GetNumCores(), 4);
  EXPECT_EQ(processor->GetNumPorts(), 64);
  EXPECT_EQ(ozzy->GetScreen(), nullptr);
  EXPECT_EQ(ozzy->GetKeyboard(), nullptr);
  ASSERT_NE(ozzy->GetTeletype(), nullptr);
  EXPECT_EQ(ozzy->GetTeletype(), processor->GetDevice(Ozzy::kTeletypeName));
}

TEST(OzzyTest, NoTimer) {
  OzzySettings settings;
  settings.timer.reset();
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(ozzy->GetTimer(), nullptr);
  EXPECT_EQ(ozzy->GetProcessor()->GetDevice(Ozzy::kTimerName), nullptr);
}

TEST(OzzyTest, BankPages) {
  OzzySettings settings;
  settings.banks = {0x00FE, 0xFFFF, 0x0000};
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  Processor* processor = ozzy->GetProcessor();

  // Each bank has memory up to its last readable page.
  EXPECT_EQ(processor->GetMemory(0)->GetMemorySize(), 8 * kMemoryBankPageSize);
  EXPECT_EQ(processor->GetMemory(1)->GetMemorySize(), 16 * kMemoryBankPageSize);
  EXPECT_EQ(processor->GetMemory(2)->GetMemorySize(), 0);

  // Bank 0: the boot ROM's page is read only, the RAM pages read and write,
  // and page 15 is the screen's.
  MemoryPageMasks masks = processor->GetMemory(0)->GetMemPageMasks();
  EXPECT_EQ(masks.read_mask, 0x00FF);
  EXPECT_EQ(masks.write_mask, 0x00FE);
  masks = processor->GetMemory(0)->GetMapPageMasks();
  EXPECT_EQ(masks.read_mask, 0x8000);
  EXPECT_EQ(masks.write_mask, 0x8000);

  // Bank 1: all RAM.
  masks = processor->GetMemory(1)->GetMemPageMasks();
  EXPECT_EQ(masks.read_mask, 0xFFFF);
  EXPECT_EQ(masks.write_mask, 0xFFFF);

  // Bank 2: nothing.
  masks = processor->GetMemory(2)->GetMemPageMasks();
  EXPECT_EQ(masks.read_mask, 0);
  EXPECT_EQ(masks.write_mask, 0);
  EXPECT_EQ(processor->GetMemory(2)->GetMapPageMasks().read_mask, 0);

  // Banks the Ozzy doesn't have have no memory.
  EXPECT_EQ(processor->GetMemory(3)->GetMemorySize(), 0);
}

//==============================================================================
// The machine table
//==============================================================================

TEST(OzzyTest, MachineTableLayout) {
  // Programs find entries at these offsets, so they never move.
  EXPECT_EQ(kMachineTableBanks, 0x08);
  EXPECT_EQ(kMachineTableDevices, 0x28);
  EXPECT_EQ(kMachineInterruptShared, 0x8000);
}

TEST(OzzyTest, DefaultMachineTable) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  EXPECT_THAT(
      GetTableWords(*ozzy, 0, kMachineHeaderSize),
      ElementsAre(kMachineTableVersion, 0x4240, 0x000F, 1, 48, 1, 4, 0));
  EXPECT_THAT(GetTableWords(*ozzy, kMachineTableBanks, kMachineBankSize),
              ElementsAre(0x0001, 0x7FFE));

  // The banks the Ozzy doesn't have are 0.
  int offset = kMachineTableBanks + kMachineBankSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineTableDevices - offset),
              Each(0));
  offset = kMachineTableDevices;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(6, 0, 0xFFFF, 0, 0xFFFF, 0xFFFF, 0, 0));
  offset += kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(1, 0xFFFF, 6, 0x01, 0, 15, 80, 24));
  offset += kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(2, 1, 1, 0x8001, 0xFFFF, 0xFFFF, 6, 0));
  offset += kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(4, 3, 0, 0x01, 0xFFFF, 0xFFFF, 60, 0));
  offset += kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineTableSize - offset),
              Each(0));
}

TEST(OzzyTest, FullMachineTable) {
  OzzySettings settings;
  settings.clock_hz = OzzySettings::kMaxClockHz;  // 0x05F5E100
  settings.cores = 3;
  settings.ports = 256;
  settings.banks = {0x0002, 0xFFFF, 0x7FFF};
  settings.screen->columns = 40;
  settings.screen->rows = 12;
  settings.teletype = OzzyTeletypeSettings();
  settings.timer->tick_hz = 1000;
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_THAT(
      GetTableWords(*ozzy, 0, kMachineHeaderSize),
      ElementsAre(kMachineTableVersion, 0xE100, 0x05F5, 3, 256, 3, 5, 0));
  EXPECT_THAT(GetTableWords(*ozzy, kMachineTableBanks, 3 * kMachineBankSize),
              ElementsAre(0x0001, 0x0002, 0, 0xFFFF, 0, 0x7FFF));
  int offset = kMachineTableBanks + 3 * kMachineBankSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineTableDevices - offset),
              Each(0));
  offset = kMachineTableDevices;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(6, 0, 0xFFFF, 0, 0xFFFF, 0xFFFF, 0, 0));
  offset += kMachineDeviceSize;

  // The screen's, keyboard's, and timer's interrupts are on every core, which
  // is only the cores the Ozzy has. The keyboard's is shared.
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(1, 0xFFFF, 6, 0x07, 0, 15, 40, 12));
  offset += kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(2, 1, 1, 0x8007, 0xFFFF, 0xFFFF, 6, 0));
  offset += kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(3, 2, 0xFFFF, 0, 0xFFFF, 0xFFFF, 0, 0));
  offset += kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(4, 3, 0, 0x07, 0xFFFF, 0xFFFF, 1000, 0));
  offset += kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineTableSize - offset),
              Each(0));
}

TEST(OzzyTest, AllBanksMachineTable) {
  OzzySettings settings;
  settings.banks.assign(kMaxMemoryBanks, 0xFFFF);
  settings.banks[0] = 0x7FFE;
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(GetTableWords(*ozzy, kMachineHeaderBanks, 1)[0], kMaxMemoryBanks);
  const int last_bank =
      kMachineTableBanks + (kMaxMemoryBanks - 1) * kMachineBankSize;
  EXPECT_THAT(GetTableWords(*ozzy, last_bank, kMachineBankSize),
              ElementsAre(0, 0xFFFF));

  // The devices are where they always are, right after the last bank.
  EXPECT_EQ(last_bank + kMachineBankSize, kMachineTableDevices);
  EXPECT_THAT(GetTableWords(*ozzy, kMachineTableDevices, kMachineDeviceSize),
              ElementsAre(6, 0, 0xFFFF, 0, 0xFFFF, 0xFFFF, 0, 0));
}

TEST(OzzyTest, NoDevicesMachineTable) {
  // The power control is the one device every Ozzy has.
  OzzySettings settings;
  settings.screen.reset();
  settings.keyboard = false;
  settings.timer.reset();
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(GetTableWords(*ozzy, kMachineHeaderDevices, 1)[0], 1);
  EXPECT_THAT(GetTableWords(*ozzy, kMachineTableDevices, kMachineDeviceSize),
              ElementsAre(6, 0, 0xFFFF, 0, 0xFFFF, 0xFFFF, 0, 0));
  const int offset = kMachineTableDevices + kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineTableSize - offset),
              Each(0));
}

//==============================================================================
// Drives
//==============================================================================

// The sectors of the images the tests make: a 360K floppy's.
constexpr int kSectors = 720;

// Returns the words of `ozzy`'s machine table device entry `device`.
std::vector<uint16_t> GetDeviceEntry(Ozzy& ozzy, int device) {
  return GetTableWords(ozzy, kMachineTableDevices + device * kMachineDeviceSize,
                       kMachineDeviceSize);
}

// Tests of the drives, with a host file system in memory for their images,
// which outlives every Ozzy a test makes.
class OzzyDriveTest : public ::testing::Test {
 protected:
  OzzyDriveTest() {
    host_.Register(std::make_unique<gb::MemoryFileProtocol>());
  }

  // Makes a formatted image at `path`.
  void MakeImage(std::string_view path) {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&host_, path, kSectors);
    ASSERT_TRUE(image.ok()) << image.status();
    DiskFileSystem file_system({}, 1);
    file_system.AttachImage(*std::move(image));
    EXPECT_EQ(file_system.Format(*DiskLabel::Parse("WORK"), 0, DateTime()),
              DiskStatus::kOk);
  }

  // Returns the root's entry for `name` on the image at `path`, and its
  // words, or nullopt if there is no such file. Nothing else may have the
  // image open.
  std::optional<std::pair<DiskDirEntry, std::vector<uint16_t>>> ReadFile(
      std::string_view path, std::string_view name) {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&host_, path);
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
    if (file_system.Find(root, filename, &entry) != DiskStatus::kOk) {
      return std::nullopt;
    }
    EXPECT_EQ(file_system.Close(root, DateTime()), DiskStatus::kOk);
    DiskFile file = 0;
    EXPECT_EQ(file_system.Open(filename, &file), DiskStatus::kOk);
    std::vector<uint16_t> words(entry.GetSize().value_or(0));
    int read = 0;
    EXPECT_EQ(file_system.Read(file, absl::MakeSpan(words), &read),
              DiskStatus::kOk);
    words.resize(read);
    return std::make_pair(entry, std::move(words));
  }

  gb::FileSystem host_;
};

TEST_F(OzzyDriveTest, GetDrive) {
  OzzySettings settings;
  settings.drives.resize(2);
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  Processor* processor = ozzy->GetProcessor();
  ASSERT_NE(ozzy->GetDrive('A'), nullptr);
  EXPECT_EQ(ozzy->GetDrive('A'), processor->GetDevice("A:"));
  ASSERT_NE(ozzy->GetDrive('B'), nullptr);
  EXPECT_EQ(ozzy->GetDrive('B'), processor->GetDevice("B:"));
  EXPECT_EQ(ozzy->GetDrive('C'), nullptr);
  EXPECT_EQ(ozzy->GetDrive('D'), nullptr);
  for (const char letter : {'a', 'c', '@', 'E', 'Z', '\0'}) {
    EXPECT_EQ(ozzy->GetDrive(letter), nullptr) << letter;
  }
}

TEST_F(OzzyDriveTest, MachineTableWithSomeDrives) {
  // Drives come after the timer, in letter order, and their interrupts are
  // core 0's alone, however many cores the Ozzy has.
  OzzySettings settings;
  settings.cores = 4;
  settings.drives = {{.slots = 3}, {.slots = 255}};
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(GetTableWords(*ozzy, kMachineHeaderDevices, 1)[0], 6);
  EXPECT_EQ(GetDeviceEntry(*ozzy, 3)[kMachineDeviceType],
            static_cast<uint16_t>(MachineDeviceType::kTimer));
  EXPECT_THAT(GetDeviceEntry(*ozzy, 4),
              ElementsAre(5, 16, 2, 0x01, 0xFFFF, 0xFFFF, 'A', 3));
  EXPECT_THAT(GetDeviceEntry(*ozzy, 5),
              ElementsAre(5, 19, 3, 0x01, 0xFFFF, 0xFFFF, 'B', 255));
  const int offset = kMachineTableDevices + 6 * kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineTableSize - offset),
              Each(0));
}

TEST_F(OzzyDriveTest, MachineTableWithEveryDevice) {
  OzzySettings settings;
  settings.teletype = OzzyTeletypeSettings();
  settings.drives.resize(kOzzyDriveCount);
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(GetTableWords(*ozzy, kMachineHeaderDevices, 1)[0], 9);
  EXPECT_THAT(GetDeviceEntry(*ozzy, 5),
              ElementsAre(5, 16, 2, 0x01, 0xFFFF, 0xFFFF, 'A', 4));
  EXPECT_THAT(GetDeviceEntry(*ozzy, 6),
              ElementsAre(5, 19, 3, 0x01, 0xFFFF, 0xFFFF, 'B', 4));
  EXPECT_THAT(GetDeviceEntry(*ozzy, 7),
              ElementsAre(5, 22, 4, 0x01, 0xFFFF, 0xFFFF, 'C', 4));
  EXPECT_THAT(GetDeviceEntry(*ozzy, 8),
              ElementsAre(5, 25, 5, 0x01, 0xFFFF, 0xFFFF, 'D', 4));
  const int offset = kMachineTableDevices + 9 * kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineTableSize - offset),
              Each(0));
}

TEST_F(OzzyDriveTest, ImagesAreInTheirDrivesAtPowerOn) {
  MakeImage("mem:/a.ozd");
  MakeImage("mem:/d.ozd");
  OzzySettings settings;
  settings.drives = {{.image = "mem:/a.ozd"}, {}, {}, {.image = "mem:/d.ozd"}};
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(
      settings,
      WithPowerCallback(
          gb::ContextBuilder().SetPtr<gb::FileSystem>(&host_).Build()));
  ASSERT_NE(ozzy, nullptr);

  // Each control port shows whether its drive has a disk once it has read the
  // superblock, which takes its latency and a sector.
  ozzy->Run(absl::Milliseconds(200));
  Processor* processor = ozzy->GetProcessor();
  EXPECT_EQ(processor->GetPort(18).GetValue(0), DiskDrive::kDiskAttached);
  EXPECT_EQ(processor->GetPort(21).GetValue(0), 0);
  EXPECT_EQ(processor->GetPort(27).GetValue(0), DiskDrive::kDiskAttached);
  EXPECT_NE(ozzy->GetDrive('A')->DetachImage(), nullptr);
  EXPECT_EQ(ozzy->GetDrive('B')->DetachImage(), nullptr);
  EXPECT_NE(ozzy->GetDrive('D')->DetachImage(), nullptr);
}

TEST_F(OzzyDriveTest, AConfigsImagesAreFromItsFolder) {
  ASSERT_TRUE(
      host_.CreateFolder("mem:/configs/disks", gb::FolderMode::kRecursive));
  MakeImage("mem:/configs/disks/a.ozd");
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(
      "drives: [{ preset: 'floppy-1440k', image: 'disks/a.ozd' }]",
      "mem:/configs/work.ozzy");
  ASSERT_TRUE(settings.ok()) << settings.status();
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(
      *settings,
      WithPowerCallback(
          gb::ContextBuilder().SetPtr<gb::FileSystem>(&host_).Build()));
  ASSERT_NE(ozzy, nullptr);
  EXPECT_NE(ozzy->GetDrive('A')->DetachImage(), nullptr);
}

TEST_F(OzzyDriveTest, MissingImage) {
  MakeImage("mem:/a.ozd");
  OzzySettings settings;
  settings.drives = {{.image = "mem:/a.ozd"}, {.image = "mem:/b.ozd"}};
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy = Ozzy::Create(
      settings,
      WithPowerCallback(
          gb::ContextBuilder().SetPtr<gb::FileSystem>(&host_).Build()));
  ASSERT_FALSE(ozzy.ok());
  EXPECT_EQ(ozzy.status().code(), absl::StatusCode::kNotFound);
  EXPECT_EQ(ozzy.status().message(),
            "Drive B: Disk image mem:/b.ozd doesn't exist");
}

TEST_F(OzzyDriveTest, ImageWithoutAFileSystem) {
  OzzySettings settings;
  settings.drives = {{}, {}, {.image = "mem:/c.ozd"}};
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(settings, WithPowerCallback());
  ASSERT_FALSE(ozzy.ok());
  EXPECT_EQ(ozzy.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_EQ(ozzy.status().message(),
            "Drive C: has image mem:/c.ozd, but no file system to open it");
}

TEST_F(OzzyDriveTest, DriveErrorsNameTheDrive) {
  OzzySettings settings;
  settings.drives = {{}, {.slots = 0}};
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy = Ozzy::Create(
      settings,
      WithPowerCallback(
          gb::ContextBuilder().SetPtr<gb::FileSystem>(&host_).Build()));
  ASSERT_FALSE(ozzy.ok());
  EXPECT_THAT(ozzy.status().message(), HasSubstr("Disk drive B: has 0 slots"));
}

TEST_F(OzzyDriveTest, OneImageInTwoDrives) {
  MakeImage("mem:/a.ozd");
  OzzySettings settings;
  settings.drives = {{.image = "mem:/a.ozd"}, {}, {}, {.image = "mem:/a.ozd"}};
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy = Ozzy::Create(
      settings,
      WithPowerCallback(
          gb::ContextBuilder().SetPtr<gb::FileSystem>(&host_).Build()));
  ASSERT_FALSE(ozzy.ok());
  EXPECT_EQ(ozzy.status().message(),
            "drive D: image mem:/a.ozd is also drive A's");
}

// Creates the file LOG on drive A:, on its ports and interrupt, and writes the
// words 1 to 4 to it, without closing it. It halts with R5 0, or the step that
// failed.
constexpr std::string_view kWriteLogProgram = R"---(
    .STACK 16

    .SEGMENT main CODE
        SETI 2, handler         ; Reads a completion into R2, R3 fields
        MVQ R6, 0               ; Completions handled
        EI

    idle_create:
        IN 18, R0
        AND R0, 15              ; Phase 0
        JC NZ, idle_create
        MOV R2, create_completion
        MVQ R3, 2
        MOV R1, create          ; The command word, then the name's 4 dwords
        MVQ R7, 1
    send_create:
        OUTR.IW 16, (R1)
        JC NS, send_create
        MVQ R7, 4
    send_name:
        OUTR.ID 16, [R1]
        JC NS, send_name
    wait_create:
        IN 18, R0
        AND R0, 14              ; Phase 2 or higher
        JC Z, wait_create
        MOV R1, accept
        MVQ R7, 1
    read_create_accept:
        INR.IW 16, (R1)
        JC NS, read_create_accept
        MVQ R5, 1
        MOV R1, accept
        MOV R0, (R1)
        CMP R0, 0               ; OK
        JC NZ, fail
    wait_create_completion:
        CMP R6, 1
        JC NZ, wait_create_completion
        MVQ R5, 2
        MOV R1, create_completion
        MOV R0, (R1)
        CMP R0, 0               ; OK, in slot 0
        JC NZ, fail
        ADQ R1, 1
        MOV R0, (R1)
        CMP R0, 0
        JC NZ, fail

    idle_write:
        IN 18, R0
        AND R0, 15
        JC NZ, idle_write
        MOV R2, write_completion
        MVQ R3, 2
        MOV R1, write           ; WRITE 0 and its count
        MVQ R7, 2
    send_write:
        OUTR.IW 16, (R1)
        JC NS, send_write
    wait_write:
        IN 18, R0
        AND R0, 14
        JC Z, wait_write
        MOV R1, accept
        MVQ R7, 2
    read_write_accept:
        INR.IW 16, (R1)
        JC NS, read_write_accept
        MVQ R5, 3
        MOV R1, accept
        MOV R0, (R1)
        CMP R0, 0               ; OK, for 4 words
        JC NZ, fail
        ADQ R1, 1
        MOV R0, (R1)
        CMP R0, 4
        JC NZ, fail
        MOV R1, words
        MVQ R7, 4
    send_words:
        OUTR.IW 17, (R1)
        JC NS, send_words
    wait_write_completion:
        CMP R6, 2
        JC NZ, wait_write_completion
        MVQ R5, 4
        MOV R1, write_completion
        MOV R0, (R1)
        CMP R0, 0               ; OK, 4 written
        JC NZ, fail
        ADQ R1, 1
        MOV R0, (R1)
        CMP R0, 4
        JC NZ, fail
        MVQ R5, 0
    fail:
        HALT

    handler:
        MOV R7, R3
    read_completion:
        INR.IW 16, (R2)
        JC NS, read_completion
        ADQ R6, 1
        IRT

    .SEGMENT vars DATA
    create:
        .DW 1200h
        .DW SIZE(6) "LOG"
        .DW 0, 0
    write:
        .DW 2100h, 4
    words:
        .DW 1, 2, 3, 4
    accept:
        .DW 0FFFFh, 0FFFFh
    create_completion:
        .DW 0FFFFh, 0FFFFh
    write_completion:
        .DW 0FFFFh, 0FFFFh

    .SEGMENT stack STACK
)---";

// Assembles `text` in the default instruction set, adding a test failure for
// each error, loads it into `ozzy`, and resets core 0 to run it. Returns
// whether it did.
bool LoadOnCore0(Ozzy& ozzy, std::string_view text) {
  InstructionError error;
  std::unique_ptr<InstructionSetSyntax> syntax = InstructionSetSyntax::Create(
      GetDefaultInstructionSetDef(), GetDefaultInstructionSet().get(), &error);
  if (syntax == nullptr) {
    ADD_FAILURE() << error.message;
    return false;
  }
  std::vector<gb::ParseError> errors;
  std::unique_ptr<AsmProgram> program =
      AssembleProgram(*syntax, std::string(text), &errors);
  for (const gb::ParseError& parse_error : errors) {
    ADD_FAILURE() << parse_error.FormatMessage();
  }
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

TEST_F(OzzyDriveTest, AProgramWritesDriveAAndShuttingDownCommits) {
  MakeImage("mem:/a.ozd");
  OzzySettings settings;
  settings.drives = {{.image = "mem:/a.ozd"}};
  const absl::CivilSecond now(2026, 11, 18, 9, 30, 15);
  std::unique_ptr<Ozzy> ozzy =
      CreateOzzy(settings, gb::ContextBuilder()
                               .SetPtr<gb::FileSystem>(&host_)
                               .SetValue<HostClock>([now] { return now; })
                               .Build());
  ASSERT_NE(ozzy, nullptr);

  // The program runs on core 0 in place of the boot ROM.
  ASSERT_TRUE(LoadOnCore0(*ozzy, kWriteLogProgram));
  CpuCore* core = ozzy->GetProcessor()->GetCore(0);
  for (int step = 0; step < 10'000 && core->GetState() != CpuCore::State::kIdle;
       ++step) {
    ozzy->Run(absl::Milliseconds(1));
  }
  ASSERT_EQ(core->GetState(), CpuCore::State::kIdle);
  CpuCore::Registers registers;
  core->GetRegisters(registers);
  EXPECT_EQ(registers[CpuCore::R5], 0);

  // The file isn't closed, so its words are still in the drive.
  ozzy.reset();
  std::optional<std::pair<DiskDirEntry, std::vector<uint16_t>>> file =
      ReadFile("mem:/a.ozd", "LOG");
  ASSERT_TRUE(file.has_value());
  EXPECT_EQ(file->first.GetModified(), DateTime::FromCivil(now));
  EXPECT_THAT(file->second, ElementsAre(1, 2, 3, 4));
}

//==============================================================================
// Errors
//==============================================================================

TEST(OzzyTest, NoContext) {
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(OzzySettings(), static_cast<gb::Context*>(nullptr));
  ASSERT_FALSE(ozzy.ok());
  EXPECT_EQ(ozzy.status().code(), absl::StatusCode::kInvalidArgument);
}

TEST(OzzyTest, NoPowerRequestCallback) {
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(OzzySettings(), gb::Context());
  ASSERT_FALSE(ozzy.ok());
  EXPECT_EQ(ozzy.status().code(), absl::StatusCode::kInvalidArgument);
}

TEST(OzzyTest, InvalidSettings) {
  OzzySettings settings;
  settings.cores = 0;
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(settings, WithPowerCallback());
  ASSERT_FALSE(ozzy.ok());
  EXPECT_EQ(ozzy.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_EQ(ozzy.status().message(), "cores: must be 1 to 8");
}

TEST(OzzyTest, DeviceErrors) {
  OzzySettings settings;
  settings.screen->columns = 200;
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(settings, WithPowerCallback());
  EXPECT_FALSE(ozzy.ok());
  EXPECT_THAT(ozzy.status().message(), HasSubstr("Screen Screen"));

  settings = OzzySettings();
  settings.teletype = OzzyTeletypeSettings{.baud = -1};
  ozzy = Ozzy::Create(settings, WithPowerCallback());
  EXPECT_FALSE(ozzy.ok());
  EXPECT_THAT(ozzy.status().message(), HasSubstr("Teletype"));

  settings = OzzySettings();
  settings.timer->tick_hz = 0;
  ozzy = Ozzy::Create(settings, WithPowerCallback());
  EXPECT_FALSE(ozzy.ok());
  EXPECT_THAT(ozzy.status().message(), HasSubstr("Timer Timer"));
}

//==============================================================================
// Running
//==============================================================================

TEST(OzzyTest, RunAtClockRate) {
  OzzySettings settings;
  settings.clock_hz = 1000;
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(ozzy->Run(absl::Milliseconds(1)), 1);
  EXPECT_EQ(ozzy->Run(absl::Seconds(2)), 2000);
  EXPECT_EQ(ozzy->GetProcessor()->GetCycles(), 2001);
}

TEST(OzzyTest, ShortRunsAddUp) {
  // Without the timer, whose default rate is faster than these clocks.
  OzzySettings settings;
  settings.timer.reset();
  settings.clock_hz = 3;
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);

  // 20 ms is 0.06 of a cycle, so only some of the runs run one.
  Cycles cycles = 0;
  for (int i = 0; i < 50; ++i) {
    cycles += ozzy->Run(absl::Milliseconds(20));
  }
  EXPECT_EQ(cycles, 3);
  EXPECT_EQ(ozzy->GetProcessor()->GetCycles(), 3);

  // A long run keeps the fraction too.
  settings.clock_hz = 7;
  ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(ozzy->Run(absl::Milliseconds(10500)), 73);
  EXPECT_EQ(ozzy->Run(absl::Milliseconds(500)), 4);
}

TEST(OzzyTest, RunNothing) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(ozzy->Run(absl::ZeroDuration()), 0);
  EXPECT_EQ(ozzy->Run(absl::Milliseconds(-5)), 0);
  EXPECT_EQ(ozzy->GetProcessor()->GetCycles(), 0);
}

}  // namespace
}  // namespace oz3
