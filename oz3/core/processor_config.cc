// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor_config.h"

#include "glog/logging.h"

namespace oz3 {

ProcessorConfig::ProcessorConfig() : banks_(kMaxMemoryBanks) {}

ProcessorConfig& ProcessorConfig::SetMemoryBankConfig(
    int bank_index, const MemoryBankConfig& config) {
  DCHECK(bank_index >= 0 && bank_index < kMaxMemoryBanks);
  banks_[bank_index] = std::move(config);
  return *this;
}

}  // namespace oz3
