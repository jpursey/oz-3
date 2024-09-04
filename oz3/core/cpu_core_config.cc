// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/cpu_core_config.h"

#include <algorithm>

#include "oz3/core/memory_bank_config.h"

namespace oz3 {

inline CpuCoreConfig& CpuCoreConfig::SetBanks(const CpuCoreBanks& banks) {
  banks_.code = std::clamp(banks.code, 0, kMaxMemoryBanks - 1);
  banks_.stack = std::clamp(banks.stack, 0, kMaxMemoryBanks - 1);
  banks_.data = std::clamp(banks.data, 0, kMaxMemoryBanks - 1);
  banks_.extra = std::clamp(banks.extra, 0, kMaxMemoryBanks - 1);
  return *this;
}

}  // namespace oz3