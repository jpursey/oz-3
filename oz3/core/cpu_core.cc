// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/cpu_core.h"

namespace oz3 {

CpuCore::CpuCore(const CpuCoreConfig& config) : banks_(config.GetBanks()) {}

CpuCore::~CpuCore() = default;

void CpuCore::Execute() { AdvanceCycles(1); }

}  // namespace oz3
