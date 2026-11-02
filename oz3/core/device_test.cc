// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/device.h"

#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
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

constexpr InstructionDef kNopInstructions[] = {
    {.op = 0, .op_name = "NOP", .code = "UL;"},
};

std::shared_ptr<const InstructionSet> GetNopInstructionSet() {
  static std::shared_ptr<const InstructionSet> s_instruction_set =
      CompileInstructionSet({kNopInstructions});
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
  Processor processor(ProcessorConfig::OneCore(GetNopInstructionSet())
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
  Processor processor(
      ProcessorConfig::MultiCore(3, GetNopInstructionSet()).AddDevice(factory));
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
  Processor processor(ProcessorConfig::MultiCore(2, GetNopInstructionSet())
                          .AddDevice(factory_a)
                          .AddDevice(factory_b));

  EXPECT_EQ(GetFakeDevice(processor, "A")->RaiseInterrupt(0), 0b11);
  EXPECT_EQ(GetFakeDevice(processor, "B")->RaiseInterrupt(0), 0);
  EXPECT_EQ(processor.GetCore(0)->GetInterrupts(), 1u << 3);
  EXPECT_EQ(processor.GetCore(1)->GetInterrupts(), 1u << 3);
}

}  // namespace
}  // namespace oz3
