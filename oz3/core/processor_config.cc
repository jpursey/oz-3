// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor_config.h"

#include <algorithm>
#include <memory>
#include <string>
#include <utility>

#include "absl/log/check.h"
#include "absl/strings/str_cat.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/device_factory.h"

namespace oz3 {

ProcessorConfig ProcessorConfig::OneCore(
    std::shared_ptr<const InstructionSet> instructions) {
  return ProcessorConfig()
      .AddCpuCore(CpuCoreConfig(std::move(instructions)))
      .SetMemoryBank(0, MemoryBankConfig::MaxRam());
}

ProcessorConfig ProcessorConfig::MultiCore(
    int num_cores, std::shared_ptr<const InstructionSet> instructions) {
  ProcessorConfig config;
  num_cores = std::clamp(num_cores, 0, kMaxCores);
  for (int i = 0; i < num_cores; ++i) {
    config.AddCpuCore(CpuCoreConfig(instructions));
  }
  config.SetMemoryBank(0, MemoryBankConfig::MaxRam());
  return config;
}

ProcessorConfig ProcessorConfig::MultiBank(int num_banks) {
  ProcessorConfig config;
  num_banks = std::clamp(num_banks, 1, kMaxMemoryBanks);
  for (int i = 0; i < num_banks; ++i) {
    config.SetMemoryBank(i, MemoryBankConfig::MaxRam());
  }
  return config;
}

ProcessorConfig ProcessorConfig::MultiBankMultiCore(
    int num_banks, int num_cores,
    std::shared_ptr<const InstructionSet> instructions) {
  ProcessorConfig config = MultiBank(num_banks);
  num_cores = std::clamp(num_cores, 0, kMaxCores);
  for (int i = 0; i < num_cores; ++i) {
    config.AddCpuCore(CpuCoreConfig(instructions));
  }
  return config;
}

ProcessorConfig::ProcessorConfig() : banks_(kMaxMemoryBanks) {}

ProcessorConfig& ProcessorConfig::SetMemoryBank(int bank_index,
                                                MemoryBankConfig config) {
  DCHECK(bank_index >= 0 && bank_index < kMaxMemoryBanks);
  banks_[bank_index] = std::move(config);
  return *this;
}

ProcessorConfig& ProcessorConfig::AddCpuCore(CpuCoreConfig config) {
  DCHECK(cores_.size() < kMaxCores);
  cores_.push_back(std::move(config));
  return *this;
}

ProcessorConfig& ProcessorConfig::SetPortCount(int count) {
  DCHECK(count >= 0 && count <= kMaxPorts);
  port_count_ = count;
  return *this;
}

ProcessorConfig& ProcessorConfig::AddDevice(
    std::shared_ptr<const DeviceFactory> factory) {
  DCHECK(factory != nullptr);
  devices_.push_back(std::move(factory));
  return *this;
}

absl::Status ProcessorConfig::Validate() const {
  for (int i = 0; i < static_cast<int>(devices_.size()); ++i) {
    const DeviceConfig& device_config = devices_[i]->GetConfig();
    if (device_config.name.empty()) {
      return absl::InvalidArgumentError(
          absl::StrCat("Device ", i, " has no name"));
    }
    const std::string device =
        absl::StrCat("Device ", i, " (", device_config.name, ")");
    for (int port : device_config.ports) {
      if (port < 0 || port >= port_count_) {
        return absl::InvalidArgumentError(
            absl::StrCat(device, " uses port ", port, ", but there are only ",
                         port_count_, " ports"));
      }
    }
    for (const DeviceInterrupt& interrupt : device_config.interrupts) {
      if (interrupt.interrupt < 0 ||
          interrupt.interrupt >= CpuCore::kInterruptCount) {
        return absl::InvalidArgumentError(absl::StrCat(
            device, " uses interrupt ", interrupt.interrupt,
            ", but there are only ", CpuCore::kInterruptCount, " interrupts"));
      }
    }
    const DeviceMemory& memory = device_config.memory;
    if (memory.pages.page_count != 0) {
      if (memory.bank < 0 || memory.bank >= kMaxMemoryBanks) {
        return absl::InvalidArgumentError(
            absl::StrCat(device, " has pages in bank ", memory.bank,
                         ", but there are only ", kMaxMemoryBanks, " banks"));
      }
      if (!memory.pages.IsValid()) {
        return absl::InvalidArgumentError(absl::StrCat(
            device, " has ", memory.pages.page_count, " pages from page ",
            memory.pages.start_page, ", but a bank has only ",
            kMemoryBankPageCount, " pages"));
      }
    }
    for (int j = 0; j < i; ++j) {
      const DeviceConfig& other_config = devices_[j]->GetConfig();
      if (other_config.name == device_config.name) {
        return absl::InvalidArgumentError(
            absl::StrCat(device, " has the same name as device ", j));
      }
      const DeviceMemory& other = other_config.memory;
      if (memory.pages.page_count > 0 && other.pages.page_count > 0 &&
          memory.bank == other.bank &&
          (memory.pages.GetMask() & other.pages.GetMask()) != 0) {
        return absl::InvalidArgumentError(
            absl::StrCat(device, " has pages in bank ", memory.bank,
                         " that overlap the pages of device ", j));
      }
    }
  }
  return absl::OkStatus();
}

}  // namespace oz3
