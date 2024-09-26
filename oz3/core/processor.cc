// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor.h"

#include "oz3/core/cpu_core.h"
#include "oz3/core/memory_bank.h"

namespace oz3 {

Processor::Processor(const ProcessorConfig& config)
    : ports_(config.GetPortCount()) {
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
}

Processor::~Processor() = default;

void Processor::Execute(Cycles cycles) {
  while (cycles > 0) {
    --cycles;
    ++cycles_;
    for (int i = 0; i < num_cores_; ++i) {
      if (cores_[i]->GetCycles() < cycles_) {
        cores_[i]->Execute();
      }
    }
  }
}

void Processor::RaiseInterrupt(int interrupt_index) {
  for (int i = 0; i < num_cores_; ++i) {
    cores_[i]->RaiseInterrupt(interrupt_index);
  }
}

}  // namespace oz3
