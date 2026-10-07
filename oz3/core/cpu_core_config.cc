// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/cpu_core_config.h"

#include <utility>

#include "absl/log/check.h"

namespace oz3 {

CpuCoreConfig::CpuCoreConfig(std::shared_ptr<const InstructionSet> instructions)
    : instructions_(std::move(instructions)) {
  DCHECK(instructions_ != nullptr);
}

}  // namespace oz3