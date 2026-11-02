// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/device.h"

#include <cstdint>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/types/span.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/base_core_test.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/fake_device.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/port.h"
#include "oz3/core/processor.h"
#include "oz3/core/processor_config.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;

// STW stores R0 at EXTRA address R1, and LDW loads R2 from it.
constexpr InstructionDef kTestInstructions[] = {
    {.op = 0, .op_name = "NOP", .code = "UL;"},
    {.op = 1, .op_name = "STW", .code = "UL;LK(EXTRA);ADR(R1);ST(R0);UL;"},
    {.op = 2, .op_name = "LDW", .code = "UL;LK(EXTRA);ADR(R1);LD(R2);UL;"},
};

std::shared_ptr<const InstructionSet> GetTestInstructionSet() {
  static std::shared_ptr<const InstructionSet> s_instruction_set =
      CompileInstructionSet({kTestInstructions});
  return s_instruction_set;
}

TEST(DeviceTest, FactoryCreatesDeviceForEachProcessor) {
  const ProcessorConfig config = ProcessorConfig().AddDevice(
      std::make_shared<FakeDeviceFactory>(DeviceConfig{.name = "Fake"}));
  Processor processor_a(config);
  Processor processor_b(config);

  Device* device_a = processor_a.GetDevice("Fake");
  Device* device_b = processor_b.GetDevice("Fake");
  ASSERT_NE(device_a, nullptr);
  ASSERT_NE(device_b, nullptr);
  EXPECT_NE(device_a, device_b);
  EXPECT_EQ(device_a->GetName(), "Fake");
  EXPECT_EQ(processor_a.GetDevice("Other"), nullptr);
}

TEST(DeviceTest, ProcessorFindsDevicesByName) {
  Processor processor(ProcessorConfig()
                          .AddDevice(std::make_shared<FakeDeviceFactory>(
                              DeviceConfig{.name = "A"}))
                          .AddDevice(std::make_shared<FakeDeviceFactory>(
                              DeviceConfig{.name = "B"})));
  ASSERT_NE(processor.GetDevice("A"), nullptr);
  ASSERT_NE(processor.GetDevice("B"), nullptr);
  EXPECT_EQ(processor.GetDevice("A")->GetName(), "A");
  EXPECT_EQ(processor.GetDevice("B")->GetName(), "B");
}

TEST(DeviceTest, DeviceOutlivesItsFactory) {
  auto factory = std::make_shared<FakeDeviceFactory>(
      DeviceConfig{.name = "Fake", .ports = {0}});
  std::weak_ptr<const FakeDeviceFactory> weak_factory = factory;
  Processor processor(
      ProcessorConfig().SetPortCount(1).AddDevice(std::move(factory)));
  EXPECT_TRUE(weak_factory.expired());

  FakeDevice* device = GetFakeDevice(processor, "Fake");
  ASSERT_EQ(device->GetPortCount(), 1);
  EXPECT_EQ(&device->GetPort(0), &processor.GetPort(0));
  processor.Execute(1);
  EXPECT_THAT(device->GetRunCycles(), ElementsAre(0));
}

TEST(DeviceTest, RunsInStepWithProcessor) {
  auto factory =
      std::make_shared<FakeDeviceFactory>(DeviceConfig{.name = "Fake"});
  Processor processor(ProcessorConfig().AddDevice(factory));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  device->SetStep([](FakeDevice&) { return 3; });

  processor.Execute(10);
  EXPECT_THAT(device->GetRunCycles(), ElementsAre(0, 3, 6, 9));
  EXPECT_EQ(device->GetCycles(), 12);
}

TEST(DeviceTest, RunsAfterCoresInOrderAdded) {
  auto factory_a =
      std::make_shared<FakeDeviceFactory>(DeviceConfig{.name = "A"});
  auto factory_b =
      std::make_shared<FakeDeviceFactory>(DeviceConfig{.name = "B"});
  Processor processor(ProcessorConfig::OneCore(GetTestInstructionSet())
                          .AddDevice(factory_a)
                          .AddDevice(factory_b));
  const CpuCore* core = processor.GetCore(0);
  std::vector<std::string_view> order;
  std::vector<Cycles> core_cycles;
  GetFakeDevice(processor, "A")->SetStep([&](FakeDevice&) {
    order.push_back("A");
    core_cycles.push_back(core->GetCycles());
    return 1;
  });
  GetFakeDevice(processor, "B")->SetStep([&](FakeDevice&) {
    order.push_back("B");
    return 1;
  });

  processor.Execute(2);
  EXPECT_THAT(order, ElementsAre("A", "B", "A", "B"));

  // The idle core runs a cycle at a time, and each cycle it has already run
  // before the device does.
  EXPECT_THAT(core_cycles, ElementsAre(1, 2));
}

TEST(DeviceTest, UsesItsPorts) {
  auto factory = std::make_shared<FakeDeviceFactory>(
      DeviceConfig{.name = "Fake", .ports = {5, 2}});
  Processor processor(ProcessorConfig().SetPortCount(8).AddDevice(factory));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  ASSERT_EQ(device->GetPortCount(), 2);
  EXPECT_EQ(&device->GetPort(0), &processor.GetPort(5));
  EXPECT_EQ(&device->GetPort(1), &processor.GetPort(2));

  // What the device stores, the host loads, and the reverse.
  device->SetStep([](FakeDevice& device) {
    auto lock = device.LockPort(0);
    EXPECT_TRUE(lock->IsLocked());
    uint16_t value = 0;
    device.GetPort(0).LoadWord(*lock, 0, value);
    device.GetPort(0).StoreWord(*lock, 0, value + 1);
    return 1;
  });
  {
    auto lock = processor.LockPort(5);
    processor.GetPort(5).StoreWord(*lock, 0, 41);
  }
  processor.Execute(1);
  auto lock = processor.LockPort(5);
  uint16_t value = 0;
  processor.GetPort(5).LoadWord(*lock, 0, value);
  EXPECT_EQ(value, 42);
}

TEST(DeviceTest, WaitsForPortHostHolds) {
  auto factory = std::make_shared<FakeDeviceFactory>(
      DeviceConfig{.name = "Fake", .ports = {0}});
  Processor processor(ProcessorConfig().SetPortCount(1).AddDevice(factory));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  std::unique_ptr<Lock> device_lock;
  std::vector<Cycles> locked_cycles;
  device->SetStep([&](FakeDevice& device) {
    if (device_lock == nullptr) {
      device_lock = device.LockPort(0);
    }
    if (!device_lock->IsLocked()) {
      return 1;
    }
    locked_cycles.push_back(device.GetCycles());
    device.GetPort(0).StoreWord(*device_lock, 0, 7);
    device_lock.reset();
    return 1;
  });

  auto host_lock = processor.LockPort(0);
  processor.Execute(3);
  EXPECT_TRUE(locked_cycles.empty());
  host_lock.reset();
  processor.Execute(1);
  EXPECT_THAT(locked_cycles, ElementsAre(3));
  EXPECT_EQ(processor.GetPort(0).GetValue(0), 7);
}

TEST(DeviceTest, UsesAnyBankAsSharedMemory) {
  auto factory =
      std::make_shared<FakeDeviceFactory>(DeviceConfig{.name = "Fake"});
  Processor processor(ProcessorConfig::MultiBank(2).AddDevice(factory));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  EXPECT_EQ(device->GetMemory(1), processor.GetMemory(1));

  std::unique_ptr<Lock> device_lock;
  device->SetStep([&](FakeDevice& device) {
    MemoryBank* bank = device.GetMemory(1);
    if (device_lock == nullptr) {
      device_lock = bank->RequestLock();
    }
    if (!device_lock->IsLocked()) {
      return Cycles{1};
    }
    Cycles cycles = bank->SetAddress(*device_lock, 100);
    cycles += bank->StoreWord(*device_lock, 0xABCD);
    device_lock.reset();
    return cycles;
  });

  // The device waits while the host holds the bank.
  auto host_lock = processor.GetMemory(1)->RequestLock();
  processor.Execute(2);
  EXPECT_EQ(processor.GetMemory(1)->GetMem(100, 1)[0], 0);
  host_lock.reset();

  // Then its write costs it the bank's cycles.
  processor.Execute(1);
  EXPECT_EQ(processor.GetMemory(1)->GetMem(100, 1)[0], 0xABCD);
  EXPECT_THAT(device->GetRunCycles(), ElementsAre(0, 1, 2));
  EXPECT_EQ(device->GetCycles(),
            2 + kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles);
}

TEST(DeviceTest, RaisesInterruptsOnCoresWiredTo) {
  auto factory = std::make_shared<FakeDeviceFactory>(DeviceConfig{
      .name = "Fake",
      .interrupts = {{.interrupt = 5, .cores = 0b1010}, {.interrupt = 6}}});
  Processor processor(ProcessorConfig::MultiCore(3, GetTestInstructionSet())
                          .AddDevice(factory));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  ASSERT_EQ(device->GetInterruptCount(), 2);

  // Only the cores the processor has.
  EXPECT_EQ(device->GetInterruptCores(0), 0b010);
  EXPECT_EQ(device->GetInterruptCores(1), 0b111);

  EXPECT_EQ(device->RaiseInterrupt(0), 0b010);
  EXPECT_EQ(processor.GetCore(0)->GetInterrupts(), 0);
  EXPECT_EQ(processor.GetCore(1)->GetInterrupts(), 1u << 5);
  EXPECT_EQ(processor.GetCore(2)->GetInterrupts(), 0);
  EXPECT_EQ(device->RaiseInterrupt(0), 0);

  processor.RaiseInterrupt(6, 0b001);
  EXPECT_EQ(device->RaiseInterrupt(1), 0b110);
}

TEST(DeviceTest, DevicesSharingALineMergeRaises) {
  auto factory_a = std::make_shared<FakeDeviceFactory>(
      DeviceConfig{.name = "A", .interrupts = {{.interrupt = 3}}});
  auto factory_b = std::make_shared<FakeDeviceFactory>(
      DeviceConfig{.name = "B", .interrupts = {{.interrupt = 3}}});
  Processor processor(ProcessorConfig::MultiCore(2, GetTestInstructionSet())
                          .AddDevice(factory_a)
                          .AddDevice(factory_b));

  EXPECT_EQ(GetFakeDevice(processor, "A")->RaiseInterrupt(0), 0b11);
  EXPECT_EQ(GetFakeDevice(processor, "B")->RaiseInterrupt(0), 0);
  EXPECT_EQ(processor.GetCore(0)->GetInterrupts(), 1u << 3);
  EXPECT_EQ(processor.GetCore(1)->GetInterrupts(), 1u << 3);
}

TEST(DeviceTest, PagesAreReadAndWrittenThroughTheirBank) {
  Processor processor(ProcessorConfig::MultiBank(1).AddDevice(
      std::make_shared<FakeDeviceFactory>(DeviceConfig{
          .name = "Fake",
          .memory = {.bank = 0,
                     .pages = {.start_page = 14, .page_count = 2}}})));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  MemoryBank* bank = processor.GetMemory(0);
  ASSERT_NE(device->GetMemoryMap(), nullptr);
  EXPECT_EQ(bank->GetMapPageMasks().read_mask, 0xC000);
  EXPECT_EQ(bank->GetMapPageMasks().write_mask, 0xC000);
  EXPECT_EQ(bank->GetMemPageMasks().read_mask, 0x3FFF);

  // The device's own addresses start at its first page.
  auto lock = bank->RequestLock();
  absl::Span<uint16_t> words = device->GetMemoryMap()->GetMem();
  bank->SetAddress(*lock, 0xE005);
  bank->StoreWord(*lock, 7);
  EXPECT_EQ(words[5], 7);
  words[kMemoryBankPageSize + 3] = 9;
  uint16_t value = 0;
  bank->SetAddress(*lock, 0xF003);
  bank->LoadWord(*lock, value);
  EXPECT_EQ(value, 9);

  // Physical memory under the pages is untouched.
  EXPECT_EQ(bank->GetMem(0xE005, 1)[0], 0);
}

TEST(DeviceTest, PagesHonorDeviceMasks) {
  // The device's first page is read-only, and its second write-only.
  Processor processor(ProcessorConfig::MultiBank(1).AddDevice(
      std::make_shared<FakeDeviceFactory>(DeviceConfig{
          .name = "Fake",
          .memory = {.bank = 0,
                     .pages = {.start_page = 4, .page_count = 2},
                     .masks = {.read_mask = 0b01, .write_mask = 0b10}}})));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  MemoryBank* bank = processor.GetMemory(0);
  EXPECT_EQ(bank->GetMapPageMasks().read_mask, 0x0010);
  EXPECT_EQ(bank->GetMapPageMasks().write_mask, 0x0020);

  auto lock = bank->RequestLock();
  absl::Span<uint16_t> words = device->GetMemoryMap()->GetMem();
  words[1] = 3;
  words[kMemoryBankPageSize + 1] = 4;
  bank->SetAddress(*lock, 0x4000);
  bank->StoreWord(*lock, 1);
  bank->SetAddress(*lock, 0x5000);
  bank->StoreWord(*lock, 2);
  EXPECT_EQ(words[0], 0);
  EXPECT_EQ(words[kMemoryBankPageSize], 2);

  uint16_t value = 0;
  bank->SetAddress(*lock, 0x4001);
  bank->LoadWord(*lock, value);
  EXPECT_EQ(value, 3);
  bank->SetAddress(*lock, 0x5001);
  bank->LoadWord(*lock, value);
  EXPECT_EQ(value, 0);
}

TEST(DeviceTest, DevicesHaveTheirOwnPagesInOneBank) {
  Processor processor(
      ProcessorConfig::MultiBank(1)
          .AddDevice(std::make_shared<FakeDeviceFactory>(DeviceConfig{
              .name = "A",
              .memory = {.bank = 0,
                         .pages = {.start_page = 2, .page_count = 1}}}))
          .AddDevice(std::make_shared<FakeDeviceFactory>(DeviceConfig{
              .name = "B",
              .memory = {.bank = 0,
                         .pages = {.start_page = 8, .page_count = 2}}})));
  MemoryBank* bank = processor.GetMemory(0);
  EXPECT_EQ(bank->GetMapPageMasks().read_mask, 0x0304);

  auto lock = bank->RequestLock();
  bank->SetAddress(*lock, 0x2000);
  bank->StoreWord(*lock, 1);
  bank->SetAddress(*lock, 0x8000);
  bank->StoreWord(*lock, 2);
  EXPECT_EQ(GetFakeDevice(processor, "A")->GetMemoryMap()->GetMem()[0], 1);
  EXPECT_EQ(GetFakeDevice(processor, "B")->GetMemoryMap()->GetMem()[0], 2);
}

TEST(DeviceTest, DeviceWithoutPagesLeavesBankAsConfigured) {
  Processor processor(ProcessorConfig::MultiBank(1).AddDevice(
      std::make_shared<FakeDeviceFactory>(DeviceConfig{.name = "Fake"})));
  EXPECT_EQ(GetFakeDevice(processor, "Fake")->GetMemoryMap(), nullptr);
  const MemoryBank* bank = processor.GetMemory(0);
  EXPECT_EQ(bank->GetMemPageMasks().read_mask, 0xFFFF);
  EXPECT_EQ(bank->GetMemPageMasks().write_mask, 0xFFFF);
  EXPECT_EQ(bank->GetMapPageMasks().read_mask, 0);
  EXPECT_EQ(bank->GetMapPageMasks().write_mask, 0);
}

// Runs core 0 with a device that has one page at page 15 of bank 1, the
// core's EXTRA bank, which the core addresses from 0.
//
// Each cycle, the cores run before the devices. So a lock a core releases is
// granted to a device waiting for it in the same cycle, and the device uses it
// then. A lock a device releases is granted to a core waiting for it in the
// same cycle too, but after the core ran, so it uses it a cycle later.
class DevicePagesTest : public BaseCoreTest {
 protected:
  static constexpr int kAddress = 5;

  DevicePagesTest()
      : BaseCoreTest({kTestInstructions}, GetTestInstructionSet()) {}

  void SetUp() override {
    ASSERT_TRUE(InitAndReset(
        {.num_memory_banks = 2,
         .devices = {std::make_shared<FakeDeviceFactory>(DeviceConfig{
             .name = "Fake",
             .memory = {.bank = 1,
                        .pages = {.start_page = 15, .page_count = 1}}})}}));
    GetState().SetRegisters({{CpuCore::BE, 0xF000}, {CpuCore::R1, kAddress}});
    device_ = GetFakeDevice(GetProcessor(), "Fake");
  }

  FakeDevice* device_ = nullptr;
};

TEST_F(DevicePagesTest, DeviceGetsPagesInCycleCoreReleasesThem) {
  CoreState& state = GetState();
  state.SetRegisters({{CpuCore::R0, 42}});
  const uint16_t end_ip = state.code.AddValue(Encode("STW")).AddNopGetAddress();

  // Each cycle, the device locks its pages, reads the word the core stores,
  // and releases them, or waits for the lock.
  struct LockedRead {
    Cycles cycle = 0;
    uint16_t word = 0;
    auto operator<=>(const LockedRead&) const = default;
  };
  std::unique_ptr<Lock> lock;
  std::vector<Cycles> wait_cycles;
  std::vector<LockedRead> reads;
  device_->SetStep([&](FakeDevice& device) {
    if (lock == nullptr) {
      lock = device.GetMemoryMap()->RequestLock();
    }
    if (!lock->IsLocked()) {
      wait_cycles.push_back(device.GetCycles());
      return 1;
    }
    reads.push_back({.cycle = device.GetCycles(),
                     .word = device.GetMemoryMap()->GetMem()[kAddress]});
    lock.reset();
    return 1;
  });

  // The core locks the bank once STW is fetched, and releases it after its
  // store. The device locked and released the free bank every cycle before,
  // which didn't delay the core.
  constexpr Cycles kLockCycle = kCpuCoreFetchAndDecodeCycles;
  constexpr Cycles kUnlockCycle =
      kLockCycle + kCpuCoreCycles_ADR + kCpuCoreCycles_ST;
  EXPECT_EQ(CyclesUntilIp(end_ip), kUnlockCycle);

  // The device waits while the core holds the bank, and gets it in the cycle
  // the core releases it, where it sees the core's store.
  std::vector<Cycles> expected_wait_cycles;
  for (Cycles cycle = kLockCycle; cycle < kUnlockCycle; ++cycle) {
    expected_wait_cycles.push_back(cycle);
  }
  EXPECT_EQ(wait_cycles, expected_wait_cycles);
  ASSERT_EQ(static_cast<Cycles>(reads.size()), kLockCycle + 1);
  EXPECT_EQ(reads[kLockCycle - 1],
            (LockedRead{.cycle = kLockCycle - 1, .word = 0}));
  EXPECT_EQ(reads.back(), (LockedRead{.cycle = kUnlockCycle, .word = 42}));
}

TEST_F(DevicePagesTest, CoreGetsPagesCycleAfterDeviceReleasesThem) {
  CoreState& state = GetState();
  const uint16_t end_ip = state.code.AddValue(Encode("LDW")).AddNopGetAddress();

  // The device holds its pages from cycle 0, and at kReleaseCycle writes the
  // word the core loads, and releases them.
  constexpr Cycles kReleaseCycle = kCpuCoreFetchAndDecodeCycles + 3;
  std::unique_ptr<Lock> lock;
  device_->SetStep([&](FakeDevice& device) {
    if (device.GetCycles() == 0) {
      lock = device.GetMemoryMap()->RequestLock();
      EXPECT_TRUE(lock->IsLocked());
    } else if (device.GetCycles() == kReleaseCycle) {
      device.GetMemoryMap()->GetMem()[kAddress] = 99;
      lock.reset();
    }
    return 1;
  });

  // The core waits from its LK, and is granted the bank in kReleaseCycle,
  // after it ran. So it loads in the next cycle, and sees the device's write.
  EXPECT_EQ(CyclesUntilIp(end_ip),
            kReleaseCycle + 1 + kCpuCoreCycles_ADR + kCpuCoreCycles_LD);
  EXPECT_EQ(state.r2, 99);
}

}  // namespace
}  // namespace oz3
