// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_FAKE_DEVICE_H_
#define OZ3_CORE_FAKE_DEVICE_H_

#include <functional>
#include <memory>
#include <string_view>
#include <vector>

#include "oz3/core/core_types.h"
#include "oz3/core/device.h"
#include "oz3/core/device_factory.h"

namespace oz3 {

//==============================================================================
// FakeDevice
//==============================================================================

// A device for tests, which runs whatever step a test gives it, and records
// the cycles it ran at.
//
// Its Device interface is public, so a step (or the test) can use its ports,
// memory, and interrupts directly.
class FakeDevice final : public Device {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // Does one step of the device's work, and returns the cycles it took, which
  // must be at least one.
  using Step = std::function<Cycles(FakeDevice& device)>;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  explicit FakeDevice(DeviceConfig config);
  ~FakeDevice() override = default;

  //----------------------------------------------------------------------------
  // Device interface
  //----------------------------------------------------------------------------

  using Device::GetInterruptCores;
  using Device::GetInterruptCount;
  using Device::GetMemory;
  using Device::GetPort;
  using Device::GetPortCount;
  using Device::LockPort;
  using Device::RaiseInterrupt;

  //----------------------------------------------------------------------------
  // Test interface
  //----------------------------------------------------------------------------

  // Sets the step the device runs each time it executes. By default, a step
  // does nothing for one cycle.
  void SetStep(Step step) { step_ = std::move(step); }

  // Returns the cycles the device ran at, in order.
  const std::vector<Cycles>& GetRunCycles() const { return run_cycles_; }

  //----------------------------------------------------------------------------
  // ExecutionComponent implementation
  //----------------------------------------------------------------------------

  void Execute() override;

 private:
  Step step_ = [](FakeDevice&) { return Cycles{1}; };
  std::vector<Cycles> run_cycles_;
};

//==============================================================================
// FakeDeviceFactory
//==============================================================================

// A factory for a FakeDevice with any name and wiring.
class FakeDeviceFactory final : public DeviceFactory {
 public:
  explicit FakeDeviceFactory(DeviceConfig config);
  ~FakeDeviceFactory() override = default;

  // DeviceFactory implementation.
  const DeviceConfig& GetConfig() const override { return config_; }
  std::unique_ptr<Device> CreateDevice() const override;

 private:
  const DeviceConfig config_;
};

// Returns the device named `name` in `processor`, which must be a FakeDevice,
// or null if it has none.
FakeDevice* GetFakeDevice(Processor& processor, std::string_view name);

}  // namespace oz3

#endif  // OZ3_CORE_FAKE_DEVICE_H_
