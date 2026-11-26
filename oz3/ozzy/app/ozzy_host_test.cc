// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/app/ozzy_host.h"

#include <cstdint>
#include <memory>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/status.h"
#include "absl/time/time.h"
#include "gb/base/context.h"
#include "gb/base/context_builder.h"
#include "gb/file/file_system.h"
#include "gb/file/memory_file_protocol.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/devices/power_control.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_settings.h"
#include "oz3/ozzy/machine/ozzy_testing.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::Optional;

// The path of the image the drive tests give drive A:.
constexpr std::string_view kImagePath = "mem:/a.ozd";

// Tests of a host, with a file system in memory as its host's, which outlives
// every host a test makes.
class OzzyHostTest : public ::testing::Test {
 protected:
  OzzyHostTest() {
    file_system_.Register(std::make_unique<gb::MemoryFileProtocol>());
  }

  // Returns a host for `settings`, whose context has `file_system_`, failing
  // the test if it can't be created.
  std::unique_ptr<OzzyHost> CreateHost(OzzySettings settings = {}) {
    std::unique_ptr<OzzyHost> host = OzzyHost::Create(
        std::move(settings),
        gb::ContextBuilder().SetPtr<gb::FileSystem>(&file_system_).Build());
    EXPECT_NE(host, nullptr);
    return host;
  }

  // Returns a host that is on, with drive A: holding a formatted image, and
  // core 0 about to run kWriteLogProgram, or null after failing the test.
  std::unique_ptr<OzzyHost> CreateHostWritingTheLog() {
    MakeTestImage(&file_system_, kImagePath);
    OzzySettings settings;
    settings.drives = {{.image = std::string(kImagePath)}};
    std::unique_ptr<OzzyHost> host = CreateHost(std::move(settings));
    if (host == nullptr) {
      return nullptr;
    }
    const absl::Status status = host->PowerOn();
    EXPECT_TRUE(status.ok()) << status;
    if (!status.ok() || !LoadOnCore0(*host->GetOzzy(), kWriteLogProgram)) {
      return nullptr;
    }
    return host;
  }

  // Runs `host`'s Ozzy until kWriteLogProgram halts, expecting it to succeed.
  void RunTheLogProgram(OzzyHost& host) {
    ASSERT_TRUE(RunUntilCore0Halts(*host.GetOzzy()));
    EXPECT_EQ(GetRegisters(*host.GetOzzy())[CpuCore::R5], 0);
  }

  // Expects the image to hold the log kWriteLogProgram writes.
  void ExpectTheLog() {
    std::optional<std::pair<DiskDirEntry, std::vector<uint16_t>>> file =
        ReadTestFile(&file_system_, kImagePath, "LOG");
    ASSERT_TRUE(file.has_value());
    EXPECT_THAT(file->second, ElementsAre(1, 2, 3, 4));
  }

  gb::FileSystem file_system_;
};

TEST_F(OzzyHostTest, InvalidContract) {
  EXPECT_EQ(
      OzzyHost::Create(OzzySettings(), static_cast<gb::Context*>(nullptr)),
      nullptr);
}

TEST_F(OzzyHostTest, PowerCallbackIsInTheContextWhileTheHostLives) {
  gb::Context context;
  std::unique_ptr<OzzyHost> host = OzzyHost::Create(OzzySettings(), &context);
  ASSERT_NE(host, nullptr);
  EXPECT_NE(context.GetPtr<PowerRequestCallback>(), nullptr);
  host.reset();
  EXPECT_EQ(context.GetPtr<PowerRequestCallback>(), nullptr);
}

TEST_F(OzzyHostTest, ReplacesAPowerCallbackInTheContext) {
  std::vector<PowerRequest> requests;
  gb::Context context = WithPowerCallback(gb::Context(), &requests);
  context.SetPtr<gb::FileSystem>(&file_system_);
  std::unique_ptr<OzzyHost> host = OzzyHost::Create(OzzySettings(), &context);
  ASSERT_NE(host, nullptr);
  ASSERT_TRUE(host->PowerOn().ok());
  ASSERT_TRUE(LoadOnCore0(*host->GetOzzy(), R"---(
      .STACK 0
      OUT.ID 0, 0FFFD0001h
      HALT
  )---"));
  EXPECT_THAT(host->Run(absl::Milliseconds(1)).power_request,
              Optional(PowerRequest{.command = PowerCommand::kShutDown,
                                    .exit_code = -3}));
  EXPECT_THAT(requests, IsEmpty());
}

TEST_F(OzzyHostTest, KeepsTheSettings) {
  OzzySettings settings;
  settings.clock_hz = 123'000;
  std::unique_ptr<OzzyHost> host = CreateHost(settings);
  ASSERT_NE(host, nullptr);
  EXPECT_EQ(host->GetSettings().clock_hz, 123'000);
}

TEST_F(OzzyHostTest, OffUntilPoweredOn) {
  std::unique_ptr<OzzyHost> host = CreateHost();
  ASSERT_NE(host, nullptr);
  EXPECT_FALSE(host->IsOn());
  EXPECT_EQ(host->GetOzzy(), nullptr);

  ASSERT_TRUE(host->PowerOn().ok());
  EXPECT_TRUE(host->IsOn());
  EXPECT_NE(host->GetOzzy(), nullptr);

  host->PowerOff();
  EXPECT_FALSE(host->IsOn());
  EXPECT_EQ(host->GetOzzy(), nullptr);
}

TEST_F(OzzyHostTest, PowerOnWhileOnKeepsTheOzzy) {
  std::unique_ptr<OzzyHost> host = CreateHost();
  ASSERT_NE(host, nullptr);
  ASSERT_TRUE(host->PowerOn().ok());
  Ozzy* const ozzy = host->GetOzzy();
  ASSERT_TRUE(host->PowerOn().ok());
  EXPECT_EQ(host->GetOzzy(), ozzy);
}

TEST_F(OzzyHostTest, PowerOffWhileOff) {
  std::unique_ptr<OzzyHost> host = CreateHost();
  ASSERT_NE(host, nullptr);
  host->PowerOff();
  EXPECT_FALSE(host->IsOn());
}

TEST_F(OzzyHostTest, FailedPowerOnStaysOff) {
  OzzySettings settings;
  settings.drives = {{.image = "mem:/missing.ozd"}};
  std::unique_ptr<OzzyHost> host = CreateHost(std::move(settings));
  ASSERT_NE(host, nullptr);
  const absl::Status status = host->PowerOn();
  EXPECT_FALSE(status.ok());
  EXPECT_THAT(status.message(), HasSubstr("mem:/missing.ozd"));
  EXPECT_FALSE(host->IsOn());
  EXPECT_EQ(host->GetOzzy(), nullptr);
}

TEST_F(OzzyHostTest, PowerOnWithoutWhatTheOzzyNeeds) {
  // The default Ozzy has no drives, so this needs a drive's image.
  OzzySettings settings;
  settings.drives = {{.image = std::string(kImagePath)}};
  std::unique_ptr<OzzyHost> host =
      OzzyHost::Create(std::move(settings), gb::Context());
  ASSERT_NE(host, nullptr);
  EXPECT_FALSE(host->PowerOn().ok());
  EXPECT_FALSE(host->IsOn());
}

TEST_F(OzzyHostTest, RunReturnsTheCycles) {
  OzzySettings settings;
  settings.clock_hz = 1'000'000;
  std::unique_ptr<OzzyHost> host = CreateHost(std::move(settings));
  ASSERT_NE(host, nullptr);
  ASSERT_TRUE(host->PowerOn().ok());
  const OzzyHost::RunResult result = host->Run(absl::Milliseconds(2));
  EXPECT_EQ(result.cycles, 2'000);
  EXPECT_EQ(result.power_request, std::nullopt);
}

TEST_F(OzzyHostTest, RunReturnsAShutDown) {
  std::unique_ptr<OzzyHost> host = CreateHost();
  ASSERT_NE(host, nullptr);
  ASSERT_TRUE(host->PowerOn().ok());

  // The power port is port 0, with the command in word 0 and the exit code in
  // word 1.
  ASSERT_TRUE(LoadOnCore0(*host->GetOzzy(), R"---(
      .STACK 0
      OUT.ID 0, 0FFFD0001h
      HALT
  )---"));
  EXPECT_THAT(host->Run(absl::Milliseconds(1)).power_request,
              Optional(PowerRequest{.command = PowerCommand::kShutDown,
                                    .exit_code = -3}));

  // What the request does is up to the caller.
  EXPECT_TRUE(host->IsOn());
}

TEST_F(OzzyHostTest, RunReturnsARestart) {
  std::unique_ptr<OzzyHost> host = CreateHost();
  ASSERT_NE(host, nullptr);
  ASSERT_TRUE(host->PowerOn().ok());
  ASSERT_TRUE(LoadOnCore0(*host->GetOzzy(), R"---(
      .STACK 0
      OUT.ID 0, 2
      HALT
  )---"));
  EXPECT_THAT(host->Run(absl::Milliseconds(1)).power_request,
              Optional(PowerRequest{.command = PowerCommand::kRestart}));
}

TEST_F(OzzyHostTest, RunDropsRequestsFromOutsideIt) {
  std::unique_ptr<OzzyHost> host = CreateHost();
  ASSERT_NE(host, nullptr);
  ASSERT_TRUE(host->PowerOn().ok());
  ASSERT_TRUE(LoadOnCore0(*host->GetOzzy(), R"---(
      .STACK 0
      OUT.ID 0, 2
      HALT
  )---"));
  host->GetOzzy()->Run(absl::Milliseconds(1));
  EXPECT_EQ(host->Run(absl::Milliseconds(1)).power_request, std::nullopt);
}

TEST_F(OzzyHostTest, OnlyTheFirstRequestInARun) {
  std::unique_ptr<OzzyHost> host = CreateHost();
  ASSERT_NE(host, nullptr);
  ASSERT_TRUE(host->PowerOn().ok());
  ASSERT_TRUE(LoadOnCore0(*host->GetOzzy(), R"---(
      .STACK 0
      OUT.ID 0, 2
      OUT.ID 0, 0FFFD0001h
      HALT
  )---"));
  EXPECT_THAT(host->Run(absl::Milliseconds(1)).power_request,
              Optional(PowerRequest{.command = PowerCommand::kRestart}));

  // Each run starts with no request.
  EXPECT_EQ(host->Run(absl::Milliseconds(1)).power_request, std::nullopt);
}

TEST_F(OzzyHostTest, PowerOffCommitsTheDrives) {
  std::unique_ptr<OzzyHost> host = CreateHostWritingTheLog();
  ASSERT_NE(host, nullptr);
  RunTheLogProgram(*host);
  host->PowerOff();
  ExpectTheLog();
}

TEST_F(OzzyHostTest, DestroyingTheHostWhileOnCommitsTheDrives) {
  std::unique_ptr<OzzyHost> host = CreateHostWritingTheLog();
  ASSERT_NE(host, nullptr);
  RunTheLogProgram(*host);
  host.reset();
  ExpectTheLog();
}

TEST_F(OzzyHostTest, PowerOnAfterPowerOffStartsAgain) {
  std::unique_ptr<OzzyHost> host = CreateHostWritingTheLog();
  ASSERT_NE(host, nullptr);
  RunTheLogProgram(*host);
  host->PowerOff();
  ASSERT_TRUE(host->PowerOn().ok());

  // Core 0 is about to run the boot ROM, with none of the program's registers.
  EXPECT_FALSE(IsHalted(*host->GetOzzy()));
  EXPECT_EQ(GetRegisters(*host->GetOzzy())[CpuCore::R6], 0);
}

}  // namespace
}  // namespace oz3
