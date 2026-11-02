// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor_config.h"

#include <memory>
#include <string>

#include "absl/status/status.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/fake_device.h"

namespace oz3 {
namespace {

using ::testing::AllOf;
using ::testing::HasSubstr;

TEST(ProcessorConfigTest, ValidateEmptyConfig) {
  EXPECT_TRUE(ProcessorConfig().Validate().ok());
}

TEST(ProcessorConfigTest, ValidateDevicesInAnyOrder) {
  auto factory_a = std::make_shared<FakeDeviceFactory>(
      DeviceConfig{.name = "A", .ports = {0, 3}});
  auto factory_b = std::make_shared<FakeDeviceFactory>(
      DeviceConfig{.name = "B", .ports = {3}});
  EXPECT_TRUE(ProcessorConfig()
                  .SetPortCount(4)
                  .AddDevice(factory_a)
                  .AddDevice(factory_b)
                  .Validate()
                  .ok());
  EXPECT_TRUE(ProcessorConfig()
                  .AddDevice(factory_a)
                  .AddDevice(factory_b)
                  .SetPortCount(4)
                  .Validate()
                  .ok());
}

TEST(ProcessorConfigTest, ValidateNamesDeviceForMissingPort) {
  const absl::Status status =
      ProcessorConfig()
          .SetPortCount(4)
          .AddDevice(
              std::make_shared<FakeDeviceFactory>(DeviceConfig{.name = "Fake"}))
          .AddDevice(std::make_shared<FakeDeviceFactory>(
              DeviceConfig{.name = "Keyboard", .ports = {1, 4}}))
          .Validate();
  EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(
      status.message(),
      AllOf(HasSubstr("Device 1"), HasSubstr("Keyboard"), HasSubstr("port 4")));

  const absl::Status negative_status =
      ProcessorConfig()
          .SetPortCount(4)
          .AddDevice(std::make_shared<FakeDeviceFactory>(
              DeviceConfig{.name = "Keyboard", .ports = {-1}}))
          .Validate();
  EXPECT_EQ(negative_status.code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(negative_status.message(),
              AllOf(HasSubstr("Device 0"), HasSubstr("port -1")));
}

TEST(ProcessorConfigTest, ValidateNamesDeviceForMissingInterrupt) {
  for (int interrupt : {-1, CpuCore::kInterruptCount}) {
    const absl::Status status =
        ProcessorConfig()
            .AddDevice(std::make_shared<FakeDeviceFactory>(DeviceConfig{
                .name = "Timer", .interrupts = {{.interrupt = interrupt}}}))
            .Validate();
    EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument);
    EXPECT_THAT(status.message(),
                AllOf(HasSubstr("Device 0"), HasSubstr("Timer"),
                      HasSubstr("interrupt " + std::to_string(interrupt))));
  }
}

TEST(ProcessorConfigTest, ValidateRejectsDeviceWithoutName) {
  const absl::Status status =
      ProcessorConfig()
          .AddDevice(
              std::make_shared<FakeDeviceFactory>(DeviceConfig{.name = "Fake"}))
          .AddDevice(std::make_shared<FakeDeviceFactory>(DeviceConfig{}))
          .Validate();
  EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(status.message(),
              AllOf(HasSubstr("Device 1"), HasSubstr("no name")));
}

TEST(ProcessorConfigTest, ValidateNamesDevicesWithSameName) {
  const absl::Status status =
      ProcessorConfig()
          .AddDevice(std::make_shared<FakeDeviceFactory>(
              DeviceConfig{.name = "Screen"}))
          .AddDevice(std::make_shared<FakeDeviceFactory>(
              DeviceConfig{.name = "Screen"}))
          .Validate();
  EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(
      status.message(),
      AllOf(HasSubstr("Device 1"), HasSubstr("Screen"), HasSubstr("device 0")));
}

TEST(ProcessorConfigTest, ValidateNamesFactoryAddedTwice) {
  auto factory =
      std::make_shared<FakeDeviceFactory>(DeviceConfig{.name = "Screen"});
  const absl::Status status =
      ProcessorConfig().AddDevice(factory).AddDevice(factory).Validate();
  EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(
      status.message(),
      AllOf(HasSubstr("Device 1"), HasSubstr("Screen"), HasSubstr("device 0")));
}

TEST(ProcessorConfigTest, ValidateRejectsDevicePages) {
  auto factory = std::make_shared<FakeDeviceFactory>(
      DeviceConfig{.name = "Screen", .memory = {.page_count = 1}});
  const absl::Status status = ProcessorConfig().AddDevice(factory).Validate();
  EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(status.message(), AllOf(HasSubstr("Device 0"),
                                      HasSubstr("Screen"), HasSubstr("pages")));
}

}  // namespace
}  // namespace oz3
