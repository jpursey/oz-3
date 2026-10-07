// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor_config.h"

#include <algorithm>
#include <memory>
#include <utility>

#include "absl/log/check.h"

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

}  // namespace oz3
