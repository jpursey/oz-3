// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/execution_component.h"

#include <memory>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/device_config.h"
#include "oz3/core/fake_device.h"
#include "oz3/core/processor.h"
#include "oz3/core/processor_config.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;

std::shared_ptr<FakeDeviceFactory> MakeFactory(const char* name) {
  return std::make_shared<FakeDeviceFactory>(DeviceConfig{.name = name});
}

// A step that runs every cycle, and wakes `sleeper` at `cycle`.
FakeDevice::Step WakeAt(FakeDevice* sleeper, Cycles cycle) {
  return [sleeper, cycle](FakeDevice& device) {
    if (device.GetCycles() == cycle) {
      sleeper->Wake();
    }
    return 1;
  };
}

TEST(ExecutionComponentTest, SleepingComponentIsNotRun) {
  Processor processor(ProcessorConfig().AddDevice(MakeFactory("Fake")));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  device->SetStep(FakeDevice::SleepForever);

  processor.Execute(10);
  EXPECT_THAT(device->GetRunCycles(), ElementsAre(0));
  EXPECT_EQ(device->GetCycles(), 0);
  EXPECT_EQ(device->GetDueCycle(), kSleepForever);
}

TEST(ExecutionComponentTest, WakeBetweenExecuteCallsRunsAtNextCycle) {
  Processor processor(ProcessorConfig().AddDevice(MakeFactory("Fake")));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  device->SetStep(FakeDevice::SleepForever);
  processor.Execute(10);

  // Its cycles stay where it fell asleep until it runs, and then are caught up
  // to the processor's.
  device->Wake();
  EXPECT_EQ(device->GetCycles(), 0);
  EXPECT_EQ(device->GetDueCycle(), 10);
  processor.Execute(5);
  EXPECT_THAT(device->GetRunCycles(), ElementsAre(0, 10));
  EXPECT_EQ(device->GetCycles(), 10);
}

TEST(ExecutionComponentTest, WakeFromComponentBeforeOrAfterRunsAtNextCycle) {
  for (const bool waker_first : {true, false}) {
    SCOPED_TRACE(waker_first ? "Waker first" : "Sleeper first");
    ProcessorConfig config;
    config.AddDevice(MakeFactory(waker_first ? "Waker" : "Sleeper"))
        .AddDevice(MakeFactory(waker_first ? "Sleeper" : "Waker"));
    Processor processor(config);
    FakeDevice* sleeper = GetFakeDevice(processor, "Sleeper");
    sleeper->SetStep(FakeDevice::SleepForever);
    GetFakeDevice(processor, "Waker")->SetStep(WakeAt(sleeper, 5));

    processor.Execute(10);
    EXPECT_THAT(sleeper->GetRunCycles(), ElementsAre(0, 6));
  }
}

TEST(ExecutionComponentTest, DeadlineWakes) {
  Processor processor(ProcessorConfig().AddDevice(MakeFactory("Fake")));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  device->SetStep([](FakeDevice& device) {
    device.Sleep(4);
    return 0;
  });

  processor.Execute(10);
  EXPECT_THAT(device->GetRunCycles(), ElementsAre(0, 4, 8));
  EXPECT_EQ(device->GetDueCycle(), 12);
}

TEST(ExecutionComponentTest, WakeBeforeDeadline) {
  Processor processor(ProcessorConfig().AddDevice(MakeFactory("Fake")));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  device->SetStep([](FakeDevice& device) {
    device.Sleep(100);
    return 0;
  });
  processor.Execute(3);

  // The deadline is from where it fell asleep again.
  device->Wake();
  processor.Execute(1);
  EXPECT_THAT(device->GetRunCycles(), ElementsAre(0, 3));
  EXPECT_EQ(device->GetDueCycle(), 103);
}

TEST(ExecutionComponentTest, WakeWhenDueChangesNothing) {
  Processor processor(ProcessorConfig()
                          .AddDevice(MakeFactory("Waker"))
                          .AddDevice(MakeFactory("Sleeper")));
  FakeDevice* sleeper = GetFakeDevice(processor, "Sleeper");
  sleeper->SetStep([](FakeDevice& device) {
    device.Sleep(5);
    return 0;
  });

  // The sleeper is due at cycle 5, so a wake in that cycle, before it runs,
  // doesn't put it off to the next.
  GetFakeDevice(processor, "Waker")->SetStep(WakeAt(sleeper, 5));

  processor.Execute(8);
  EXPECT_THAT(sleeper->GetRunCycles(), ElementsAre(0, 5));
}

TEST(ExecutionComponentTest, WakeOnAwakeComponentChangesNothing) {
  Processor processor(ProcessorConfig().AddDevice(MakeFactory("Fake")));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  device->SetStep([](FakeDevice&) { return 3; });
  processor.Execute(2);

  // It is busy until cycle 3, ahead of the processor.
  device->Wake();
  EXPECT_EQ(device->GetDueCycle(), 3);
  processor.Execute(8);
  EXPECT_THAT(device->GetRunCycles(), ElementsAre(0, 3, 6, 9));
}

TEST(ExecutionComponentTest, WakeNeverMovesCyclesBackward) {
  Processor processor(ProcessorConfig().AddDevice(MakeFactory("Fake")));
  FakeDevice* device = GetFakeDevice(processor, "Fake");
  device->SetStep([](FakeDevice& device) {
    device.AdvanceCycles(5);
    device.Sleep();
    return 0;
  });
  processor.Execute(2);

  // It fell asleep at cycle 5, ahead of the processor, so it runs there.
  device->Wake();
  EXPECT_EQ(device->GetDueCycle(), 5);
  processor.Execute(5);
  EXPECT_THAT(device->GetRunCycles(), ElementsAre(0, 5));
  EXPECT_EQ(device->GetCycles(), 10);
}

TEST(ExecutionComponentTest, WakeWithoutProcessorDoesNothing) {
  FakeDevice device(DeviceConfig{.name = "Fake"});
  device.Wake();
  EXPECT_EQ(device.GetCycles(), 0);
  EXPECT_EQ(device.GetDueCycle(), 0);
}

}  // namespace
}  // namespace oz3
