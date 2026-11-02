// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/fake_device.h"

#include <memory>
#include <string_view>
#include <utility>

#include "absl/log/check.h"
#include "oz3/core/processor.h"

namespace oz3 {

FakeDevice::FakeDevice(DeviceConfig config) : Device(config) {
  if (config.memory.pages.page_count > 0) {
    memory_map_ =
        std::make_unique<FakeMemoryMap>(config.memory.pages.page_count);
  }
}

void FakeDevice::Execute() {
  const Cycles start = GetCycles();
  run_cycles_.push_back(start);
  const Cycles cycles = step_(*this);
  CHECK(cycles >= 0);
  CHECK(cycles == 0 || GetDueCycle() == GetCycles())
      << "A step that sleeps must return 0";
  if (cycles > 0) {
    AdvanceCycles(cycles);
  }
  CHECK(GetDueCycle() > start) << "The step must advance its cycles or sleep";
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
