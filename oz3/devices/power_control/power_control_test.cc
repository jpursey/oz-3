// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/power_control/power_control.h"

#include <cstdint>
#include <memory>
#include <vector>

#include "gb/base/context.h"
#include "gb/base/validated_context.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/lockable.h"
#include "oz3/core/port.h"
#include "oz3/devices/device_program_test.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;

// The processor port the power control is on.
constexpr int kPort = 1;

class PowerControlTest : public DeviceProgramTest {
 protected:
  // Initializes a processor with a power control named "Power" on kPort, whose
  // callback adds each request to requests_. Returns whether it did.
  bool InitPowerControl() {
    gb::Context context;
    context.SetNew<PowerRequestCallback>(
        [this](PowerRequest request) { requests_.push_back(request); });
    return Init({.num_ports = kPort + 1,
                 .devices = {std::make_shared<PowerControlFactory>(
                     "Power", PowerControlSettings{.port = kPort})},
                 .context = std::move(context)});
  }

  // Returns the words of the power control's port.
  PortWords GetWords() {
    const Port& port = GetPort(kPort);
    return PortWords::FromWords(port.GetValue(0), port.GetValue(1));
  }

  // Writes `words` to the power control's port as OUT.ID does. No core may be
  // using the port, so its lock is granted at once.
  void WritePort(PortWords words) {
    std::unique_ptr<Lock> lock = LockPort(kPort);
    ASSERT_TRUE(lock->IsLocked());
    words.Store(GetPort(kPort), *lock, Port::S);
  }

  // The requests the power control passed to the callback, in order.
  std::vector<PowerRequest> requests_;
};

TEST_F(PowerControlTest, ShutDownPassesTheSignedExitCode) {
  ASSERT_TRUE(InitPowerControl());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        OUT.ID 1, 0FFFF0001h   ; Shut down, with exit code -1
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_THAT(requests_,
              ElementsAre(PowerRequest{.command = PowerCommand::kShutDown,
                                       .exit_code = -1}));
  EXPECT_EQ(GetPort(kPort).GetStatus(), 0);
  EXPECT_EQ(GetWords(), PortWords());
}

TEST_F(PowerControlTest, RestartHasNoExitCode) {
  ASSERT_TRUE(InitPowerControl());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        OUT.ID 1, 00050002h    ; Restart, with a 5 that is ignored
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_THAT(requests_,
              ElementsAre(PowerRequest{.command = PowerCommand::kRestart,
                                       .exit_code = 0}));
}

TEST_F(PowerControlTest, ReservedCommandsAreTakenAndIgnored) {
  ASSERT_TRUE(InitPowerControl());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        OUTS.ID 1, 00070000h   ; Each OUTS succeeds, as the last was taken
        JC NS, fail
        OUTS.ID 1, 00070003h
        JC NS, fail
        OUTS.ID 1, 0007FFFFh
        JC NS, fail
        HALT
    fail:
        MVQ R0, 1
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_EQ(GetState().r0, 0);
  EXPECT_THAT(requests_, IsEmpty());
  EXPECT_EQ(GetPort(kPort).GetStatus(), 0);
  EXPECT_EQ(GetWords(), PortWords());
}

TEST_F(PowerControlTest, WordWriteHasExitCodeZero) {
  ASSERT_TRUE(InitPowerControl());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        OUT.ID 1, 00070000h    ; Reserved, but word 1 is cleared once taken
        NOP
        OUT.IW 1, 1            ; Shut down, with word 1 as it is
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_THAT(requests_,
              ElementsAre(PowerRequest{.command = PowerCommand::kShutDown,
                                       .exit_code = 0}));
}

TEST_F(PowerControlTest, TakesTheWriteTheCycleAfter) {
  ASSERT_TRUE(InitPowerControl());
  WritePort(PortWords::FromWords(1, 4));
  EXPECT_THAT(requests_, IsEmpty());
  Execute(1);
  EXPECT_THAT(requests_,
              ElementsAre(PowerRequest{.command = PowerCommand::kShutDown,
                                       .exit_code = 4}));
  EXPECT_EQ(GetPort(kPort).GetStatus(), 0);
}

TEST_F(PowerControlTest, PassesEveryRequest) {
  ASSERT_TRUE(InitPowerControl());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        OUT.ID 1, 00020001h    ; Shut down, with exit code 2
        NOP
        OUT.ID 1, 00000002h    ; Restart
        NOP
        OUT.ID 1, 00030001h    ; Shut down, with exit code 3
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_THAT(
      requests_,
      ElementsAre(
          PowerRequest{.command = PowerCommand::kShutDown, .exit_code = 2},
          PowerRequest{.command = PowerCommand::kRestart, .exit_code = 0},
          PowerRequest{.command = PowerCommand::kShutDown, .exit_code = 3}));
}

TEST_F(PowerControlTest, FactoryRequiresTheCallback) {
  const PowerControlFactory factory("Power", {.port = kPort});
  gb::Context context;
  gb::ValidatedContext validated(&context, {});
  EXPECT_EQ(factory.CreateDevice(validated), nullptr);
  context.SetNew<PowerRequestCallback>([](PowerRequest) {});
  EXPECT_NE(factory.CreateDevice(validated), nullptr);
}

}  // namespace
}  // namespace oz3
