// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_POWER_CONTROL_H_
#define OZ3_DEVICES_POWER_CONTROL_H_

#include <cstdint>
#include <memory>
#include <string_view>

#include "gb/base/validated_context.h"
#include "oz3/core/device.h"
#include "oz3/core/device_config.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/lockable.h"

namespace oz3 {

//==============================================================================
// PowerRequest
//==============================================================================

// What a program asks the host to do with the machine's power, by writing it
// to port word 0 of the power control.
enum class PowerCommand : uint16_t {
  kShutDown = 1,  // Power off, with an exit code.
  kRestart = 2,   // Power off and on again.
};

// A program's request to shut down or restart, which the power control records
// in the processor's context for the host to act on.
struct PowerRequest {
  bool operator==(const PowerRequest&) const = default;

  PowerCommand command = PowerCommand::kShutDown;

  // The exit code the program shut down with, a signed word. A restart has
  // none, so it is 0.
  int exit_code = 0;
};

// The request the power control records in the processor's context. The host
// creates the Processor with a context it can read (such as by pointer), and
// looks for a request between Processor::Execute calls. The request stays
// until the host clears it: the power control records the first one, and
// ignores the rest until then.
GB_CONTEXT_CONSTRAINT(kConstraintPowerRequest, kOutOptional, PowerRequest);

//==============================================================================
// PowerControlSettings
//==============================================================================

// Everything a host sets about a power control but its name: its wiring.
struct PowerControlSettings {
  bool operator==(const PowerControlSettings&) const = default;

  // The processor port the power control is on.
  int port = 0;
};

//==============================================================================
// PowerControlFactory
//==============================================================================

// A power control on one port, which a Processor creates a PowerControl from.
class PowerControlFactory final : public DeviceFactory {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a factory for a power control named `name`.
  PowerControlFactory(std::string_view name,
                      const PowerControlSettings& settings);

  ~PowerControlFactory() override = default;

  //----------------------------------------------------------------------------
  // DeviceFactory implementation
  //----------------------------------------------------------------------------

  const DeviceConfig& GetConfig() const override { return config_; }
  std::unique_ptr<Device> CreateDevice(
      gb::ValidatedContext& context) const override;

 private:
  const DeviceConfig config_;
};

//==============================================================================
// PowerControl
//==============================================================================

// A power control lets a program shut the machine down, or restart it, by
// asking the host.
//
// A program writes the command (see PowerCommand) to port word 0, and for a
// shutdown, the exit code to word 1, as a signed word, together with OUT.ID,
// which sets the status. The cycle after, the power control takes the words,
// clears them and the status, and records the request in the processor's
// context (see kConstraintPowerRequest). Any other command is reserved, and is
// taken and does nothing.
//
// The power control only records the request: the machine runs on until the
// host acts on it, which it does between Processor::Execute calls. So a
// program that shuts down halts after writing the port, or loops, rather than
// expecting the next instruction not to run.
class PowerControl final : public Device {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // What the power control gives the processor's context: the request.
  using Contract = gb::ContextContract<kConstraintPowerRequest>;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  ~PowerControl() override = default;

 protected:
  //----------------------------------------------------------------------------
  // ExecutionComponent implementation
  //----------------------------------------------------------------------------

  void Execute() override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The factory constructs the power control with its validated context.
  friend class PowerControlFactory;

  PowerControl(DeviceConfig config, Contract contract);

  // Records the request for `command` and `exit_code`, as written to the port,
  // unless the command is reserved, or there is a request already.
  void Record(uint16_t command, uint16_t exit_code);

  gb::ValidatedContext context_;
  std::unique_ptr<Lock> lock_;  // The port's lock, while it is requested.
};

}  // namespace oz3

#endif  // OZ3_DEVICES_POWER_CONTROL_H_
