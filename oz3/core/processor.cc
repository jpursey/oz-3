// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor.h"

#include <cstdint>
#include <memory>
#include <string_view>

#include "absl/log/check.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/device.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/memory_bank.h"

namespace oz3 {

Processor::Processor(const ProcessorConfig& config)
    : ports_(config.GetPortCount()) {
  DCHECK_OK(config.Validate());
  for (const std::shared_ptr<const DeviceFactory>& factory :
       config.GetDevices()) {
    devices_.push_back(factory->CreateDevice());
    DCHECK(devices_.back() != nullptr) << factory->GetConfig().name;
  }
  auto bank_configs = config.GetMemoryBankConfigs();
  for (int i = 0; i < kMaxMemoryBanks; ++i) {
    banks_[i] = std::make_unique<MemoryBank>(bank_configs[i]);
    banks_[i]->AttachProcessor({}, this);
  }
  for (const CpuCoreConfig& core_config : config.GetCpuCoreConfigs()) {
    cores_[num_cores_] = std::make_unique<CpuCore>(core_config);
    cores_[num_cores_]->AttachProcessor({}, this);
    ++num_cores_;
  }
  for (const std::unique_ptr<Device>& device : devices_) {
    device->AttachProcessor({}, this);
  }
}

Processor::~Processor() = default;

Device* Processor::GetDevice(std::string_view name) {
  for (const std::unique_ptr<Device>& device : devices_) {
    if (device->GetName() == name) {
      return device.get();
    }
  }
  return nullptr;
}

void Processor::Execute(Cycles cycles) {
  while (cycles > 0) {
    --cycles;
    ++cycles_;
    for (int i = 0; i < num_cores_; ++i) {
      if (cores_[i]->GetCycles() < cycles_) {
        cores_[i]->Execute();
      }
    }
    for (const std::unique_ptr<Device>& device : devices_) {
      if (device->GetCycles() < cycles_) {
        device->Execute();
      }
    }
  }
}

uint8_t Processor::RaiseInterrupt(int interrupt, uint8_t cores) {
  DCHECK(interrupt >= 0 && interrupt < CpuCore::kInterruptCount);
  uint8_t raised = 0;
  for (int i = 0; i < num_cores_; ++i) {
    const uint8_t core_bit = static_cast<uint8_t>(1 << i);
    if ((cores & core_bit) != 0 && cores_[i]->RaiseInterrupt(interrupt)) {
      raised |= core_bit;
    }
  }
  return raised;
}

}  // namespace oz3
