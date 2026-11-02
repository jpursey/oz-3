// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/fake_device.h"

#include <string_view>
#include <utility>

#include "absl/log/check.h"
#include "oz3/core/processor.h"

namespace oz3 {

FakeDevice::FakeDevice(DeviceConfig config) : Device(std::move(config)) {}

void FakeDevice::Execute() {
  run_cycles_.push_back(GetCycles());
  const Cycles cycles = step_(*this);
  CHECK(cycles > 0);
  AdvanceCycles(cycles);
}

FakeDeviceFactory::FakeDeviceFactory(DeviceConfig config)
    : config_(std::move(config)) {}

std::unique_ptr<Device> FakeDeviceFactory::CreateDevice() const {
  return std::make_unique<FakeDevice>(GetConfig());
}

FakeDevice* GetFakeDevice(Processor& processor, std::string_view name) {
  return static_cast<FakeDevice*>(processor.GetDevice(name));
}

}  // namespace oz3
