// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor_config.h"

#include <memory>
#include <string>
#include <string_view>

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

TEST(ProcessorConfigTest, ClockRateDefaultsTo1MHz) {
  EXPECT_EQ(ProcessorConfig().GetClockHz(), 1'000'000);
  EXPECT_EQ(ProcessorConfig::kDefaultClockHz, 1'000'000);
}

TEST(ProcessorConfigTest, ValidateClockRate) {
  EXPECT_TRUE(ProcessorConfig().SetClockHz(1).Validate().ok());
  for (int64_t clock_hz : {0, -1}) {
    const absl::Status status =
        ProcessorConfig().SetClockHz(clock_hz).Validate();
    EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument);
    EXPECT_THAT(status.message(), HasSubstr("clock rate"));
  }
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

TEST(ProcessorConfigTest, ValidateDevicePages) {
  // Pages next to another device's, or on the same pages of another bank, are
  // fine, and a device without pages can name any bank.
  const auto add_device = [](ProcessorConfig& config, std::string_view name,
                             DeviceMemory memory) -> ProcessorConfig& {
    return config.AddDevice(std::make_shared<FakeDeviceFactory>(
        DeviceConfig{.name = std::string(name), .memory = memory}));
  };
  ProcessorConfig config;
  add_device(config, "A",
             {.bank = 0, .pages = {.start_page = 0, .page_count = 4}});
  add_device(config, "B",
             {.bank = 0, .pages = {.start_page = 4, .page_count = 12}});
  add_device(config, "C",
             {.bank = 1, .pages = {.start_page = 0, .page_count = 4}});
  add_device(config, "D",
             {.bank = 99, .pages = {.start_page = -1, .page_count = 0}});
  EXPECT_TRUE(config.Validate().ok());
}

TEST(ProcessorConfigTest, ValidateNamesDeviceForPagesOutsideBank) {
  const DeviceMemory memories[] = {
      {.bank = kMaxMemoryBanks, .pages = {.page_count = 1}},
      {.bank = -1, .pages = {.page_count = 1}},
      {.pages = {.start_page = kMemoryBankPageCount - 1, .page_count = 2}},
      {.pages = {.start_page = -1, .page_count = 1}},
      {.pages = {.page_count = -1}},
  };
  for (const DeviceMemory& memory : memories) {
    const absl::Status status =
        ProcessorConfig()
            .AddDevice(std::make_shared<FakeDeviceFactory>(
                DeviceConfig{.name = "Screen", .memory = memory}))
            .Validate();
    EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument)
        << memory.bank << " " << memory.pages.start_page << " "
        << memory.pages.page_count;
    EXPECT_THAT(status.message(),
                AllOf(HasSubstr("Device 0"), HasSubstr("Screen")));
  }
}

TEST(ProcessorConfigTest, ValidateNamesDeviceForOverlappingPages) {
  const DeviceMemory memories[] = {
      {.bank = 1, .pages = {.start_page = 4, .page_count = 1}},
      {.bank = 1, .pages = {.start_page = 0, .page_count = 3}},
      {.bank = 1,
       .pages = {.start_page = 0, .page_count = kMemoryBankPageCount}},
  };
  for (const DeviceMemory& memory : memories) {
    const absl::Status status =
        ProcessorConfig()
            .AddDevice(std::make_shared<FakeDeviceFactory>(DeviceConfig{
                .name = "A",
                .memory = {.bank = 1,
                           .pages = {.start_page = 2, .page_count = 3}}}))
            .AddDevice(std::make_shared<FakeDeviceFactory>(
                DeviceConfig{.name = "B", .memory = memory}))
            .Validate();
    EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument)
        << memory.pages.start_page << " " << memory.pages.page_count;
    EXPECT_THAT(status.message(), AllOf(HasSubstr("Device 1"), HasSubstr("B"),
                                        HasSubstr("device 0")));
  }
}

}  // namespace
}  // namespace oz3
