// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor.h"

#include "oz3/core/cpu_core.h"
#include "oz3/core/memory_bank.h"

namespace oz3 {

Processor::Processor(const ProcessorConfig& config) {
  auto bank_configs = config.GetMemoryBankConfigs();
  for (int i = 0; i < kMaxMemoryBanks; ++i) {
    banks_[i] = std::make_unique<MemoryBank>(bank_configs[i]);
  }
  for (const CpuCoreConfig& core_config : config.GetCpuCoreConfigs()) {
    cores_[num_cores_++] = std::make_unique<CpuCore>(core_config);
  }
}

Processor::~Processor() = default;

void Processor::Execute(Cycles cycles) { cycles_ += cycles; }

}  // namespace oz3
