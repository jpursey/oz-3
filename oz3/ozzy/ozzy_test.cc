// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/ozzy.h"

#include <cstdint>
#include <memory>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/memory_bank_config.h"
#include "oz3/core/processor.h"
#include "oz3/ozzy/machine_table.h"
#include "oz3/ozzy/ozzy_settings.h"

namespace oz3 {
namespace {

using ::testing::Each;
using ::testing::ElementsAre;
using ::testing::HasSubstr;

// Returns an Ozzy for `settings`, failing the test if it can't be created.
std::unique_ptr<Ozzy> CreateOzzy(const OzzySettings& settings) {
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy = Ozzy::Create(settings);
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
  EXPECT_EQ(processor->GetNumPorts(), 32);
  EXPECT_EQ(processor->GetCycles(), 0);
  ASSERT_NE(ozzy->GetScreen(), nullptr);
  EXPECT_EQ(ozzy->GetScreen(), processor->GetDevice(Ozzy::kScreenName));
  ASSERT_NE(ozzy->GetKeyboard(), nullptr);
  EXPECT_EQ(ozzy->GetKeyboard(), processor->GetDevice(Ozzy::kKeyboardName));
  EXPECT_EQ(ozzy->GetTeletype(), nullptr);
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
      ElementsAre(kMachineTableVersion, 0x4240, 0x000F, 1, 32, 1, 2, 0));
  EXPECT_THAT(GetTableWords(*ozzy, kMachineTableBanks, kMachineBankSize),
              ElementsAre(0x0001, 0x7FFE));

  // The banks the Ozzy doesn't have are 0.
  int offset = kMachineTableBanks + kMachineBankSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineTableDevices - offset),
              Each(0));
  offset = kMachineTableDevices;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(1, 0xFFFF, 6, 0x01, 0, 15, 80, 24));
  offset += kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(2, 1, 1, 0x8001, 0xFFFF, 0xFFFF, 0, 0));
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
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_THAT(
      GetTableWords(*ozzy, 0, kMachineHeaderSize),
      ElementsAre(kMachineTableVersion, 0xE100, 0x05F5, 3, 256, 3, 3, 0));
  EXPECT_THAT(GetTableWords(*ozzy, kMachineTableBanks, 3 * kMachineBankSize),
              ElementsAre(0x0001, 0x0002, 0, 0xFFFF, 0, 0x7FFF));
  int offset = kMachineTableBanks + 3 * kMachineBankSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineTableDevices - offset),
              Each(0));
  offset = kMachineTableDevices;

  // The screen's and keyboard's interrupts are on every core, which is only the
  // cores the Ozzy has. The keyboard's is shared.
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(1, 0xFFFF, 6, 0x07, 0, 15, 40, 12));
  offset += kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(2, 1, 1, 0x8007, 0xFFFF, 0xFFFF, 0, 0));
  offset += kMachineDeviceSize;
  EXPECT_THAT(GetTableWords(*ozzy, offset, kMachineDeviceSize),
              ElementsAre(3, 2, 0xFFFF, 0, 0xFFFF, 0xFFFF, 0, 0));
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
              ElementsAre(1, 0xFFFF, 6, 0x01, 0, 15, 80, 24));
}

TEST(OzzyTest, NoDevicesMachineTable) {
  OzzySettings settings;
  settings.screen.reset();
  settings.keyboard = false;
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(GetTableWords(*ozzy, kMachineHeaderDevices, 1)[0], 0);
  EXPECT_THAT(GetTableWords(*ozzy, kMachineTableDevices,
                            kMachineTableSize - kMachineTableDevices),
              Each(0));
}

//==============================================================================
// Errors
//==============================================================================

TEST(OzzyTest, InvalidSettings) {
  OzzySettings settings;
  settings.cores = 0;
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy = Ozzy::Create(settings);
  ASSERT_FALSE(ozzy.ok());
  EXPECT_EQ(ozzy.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_EQ(ozzy.status().message(), "cores: must be 1 to 8");
}

TEST(OzzyTest, DeviceErrors) {
  OzzySettings settings;
  settings.screen->columns = 200;
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy = Ozzy::Create(settings);
  EXPECT_FALSE(ozzy.ok());
  EXPECT_THAT(ozzy.status().message(), HasSubstr("Screen Screen"));

  settings = OzzySettings();
  settings.teletype = OzzyTeletypeSettings{.baud = -1};
  ozzy = Ozzy::Create(settings);
  EXPECT_FALSE(ozzy.ok());
  EXPECT_THAT(ozzy.status().message(), HasSubstr("Teletype"));
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
  OzzySettings settings;
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
