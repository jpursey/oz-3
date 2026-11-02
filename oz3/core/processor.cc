// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor.h"

#include <array>
#include <cstdint>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/device.h"
#include "oz3/core/device_config.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/memory_map.h"

namespace oz3 {

Processor::Processor(const ProcessorConfig& config)
    : ports_(config.GetPortCount()) {
  DCHECK_OK(config.Validate());
  std::array<std::vector<MemoryMapPages>, kMaxMemoryBanks> bank_maps;
  for (const std::shared_ptr<const DeviceFactory>& factory :
       config.GetDevices()) {
    const DeviceConfig& device_config = factory->GetConfig();
    std::unique_ptr<Device> device = factory->CreateDevice();
    DCHECK(device != nullptr) << device_config.name;
    MemoryMap* const map = device->GetMemoryMap();
    const DeviceMemory& memory = device_config.memory;
    if (memory.pages.page_count > 0) {
      DCHECK(map != nullptr)
          << device_config.name << " has pages, but no memory map";
      bank_maps[memory.bank].push_back(
          {.map = map, .pages = memory.pages, .masks = memory.masks});
    } else {
      DCHECK(map == nullptr)
          << device_config.name << " has a memory map, but no pages";
    }
    for (int port : device_config.ports) {
      ports_.GetPort(port).AddComponent({}, device.get());
    }
    devices_.push_back(std::move(device));
  }
  auto bank_configs = config.GetMemoryBankConfigs();
  for (int i = 0; i < kMaxMemoryBanks; ++i) {
    banks_[i] = std::make_unique<MemoryBank>(bank_configs[i], bank_maps[i]);
    banks_[i]->AttachProcessor({}, this);
  }
  for (const CpuCoreConfig& core_config : config.GetCpuCoreConfigs()) {
    cores_[num_cores_] = std::make_unique<CpuCore>(core_config);
    cores_[num_cores_]->AttachProcessor({}, this);
    cores_[num_cores_]->AttachClock({}, &cycles_);
    ++num_cores_;
  }
  for (const std::unique_ptr<Device>& device : devices_) {
    device->AttachProcessor({}, this);
    device->AttachClock({}, &cycles_);
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
      if (cores_[i]->GetDueCycle() < cycles_) {
        cores_[i]->Step({});
      }
    }
    for (const std::unique_ptr<Device>& device : devices_) {
      if (device->GetDueCycle() < cycles_) {
        device->Step({});
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
