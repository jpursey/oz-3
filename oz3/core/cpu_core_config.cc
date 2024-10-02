// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/cpu_core_config.h"

#include <algorithm>

#include "absl/log/log.h"
#include "oz3/core/default_instruction_set.h"
#include "oz3/core/instruction_compiler.h"

namespace oz3 {

CpuCoreConfig::CpuCoreConfig() : instructions_(GetDefaultInstructionSet()) {}

CpuCoreConfig& CpuCoreConfig::SetInstructionSet(
    std::shared_ptr<const InstructionSet> instructions) {
  instructions_ = instructions;
  return *this;
}

}  // namespace oz3