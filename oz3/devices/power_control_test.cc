// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/power_control.h"

#include <cstdint>
#include <memory>

#include "gb/base/context.h"
#include "gtest/gtest.h"
#include "oz3/core/lockable.h"
#include "oz3/core/port.h"
#include "oz3/devices/device_program_test.h"

namespace oz3 {
namespace {

// The processor port the power control is on.
constexpr int kPort = 1;

class PowerControlTest : public DeviceProgramTest {
 protected:
  // Initializes a processor with a power control named "Power" on kPort, with
  // host_context_ as its context. Returns whether it did.
  bool InitPowerControl() {
    return Init({.num_ports = kPort + 1,
                 .devices = {std::make_shared<PowerControlFactory>(
                     "Power", PowerControlSettings{.port = kPort})},
                 .context = host_context_});
  }

  // Returns whether the power control recorded a request.
  bool HasRequest() const { return host_context_->Exists<PowerRequest>(); }

  // Returns the request the power control recorded, which must exist.
  PowerRequest GetRequest() const {
    EXPECT_TRUE(HasRequest());
    return host_context_->GetValue<PowerRequest>();
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

  // The processor's context, which the test reads the request from. It is
  // shared, so it outlives the processor.
  std::shared_ptr<gb::Context> host_context_ = std::make_shared<gb::Context>();
};

TEST_F(PowerControlTest, ShutDownRecordsTheSignedExitCode) {
  ASSERT_TRUE(InitPowerControl());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        OUT.ID 1, 0FFFF0001h   ; Shut down, with exit code -1
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_EQ(GetRequest(), (PowerRequest{.command = PowerCommand::kShutDown,
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
  EXPECT_EQ(GetRequest(),
            (PowerRequest{.command = PowerCommand::kRestart, .exit_code = 0}));
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
  EXPECT_FALSE(HasRequest());
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
  EXPECT_EQ(GetRequest(),
            (PowerRequest{.command = PowerCommand::kShutDown, .exit_code = 0}));
}

TEST_F(PowerControlTest, TakesTheWriteTheCycleAfter) {
  ASSERT_TRUE(InitPowerControl());
  WritePort(PortWords::FromWords(1, 4));
  EXPECT_FALSE(HasRequest());
  Execute(1);
  EXPECT_EQ(GetRequest(),
            (PowerRequest{.command = PowerCommand::kShutDown, .exit_code = 4}));
  EXPECT_EQ(GetPort(kPort).GetStatus(), 0);
}

TEST_F(PowerControlTest, FirstRequestStandsUntilCleared) {
  ASSERT_TRUE(InitPowerControl());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        OUT.ID 1, 00020001h    ; Shut down, with exit code 2
        NOP
        OUT.ID 1, 00000002h    ; Restart, which is ignored
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_EQ(GetRequest(),
            (PowerRequest{.command = PowerCommand::kShutDown, .exit_code = 2}));
  EXPECT_EQ(GetPort(kPort).GetStatus(), 0);

  host_context_->Clear<PowerRequest>();
  WritePort(PortWords::FromWords(2, 0));
  Execute(1);
  EXPECT_EQ(GetRequest(),
            (PowerRequest{.command = PowerCommand::kRestart, .exit_code = 0}));
}

}  // namespace
}  // namespace oz3
