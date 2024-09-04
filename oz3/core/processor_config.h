// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_PROCESSOR_CONFIG_H_
#define OZ3_CORE_PROCESSOR_CONFIG_H_

#include <vector>

#include "absl/types/span.h"
#include "oz3/core/memory_bank_config.h"
#include "oz3/core/cpu_core_config.h"

namespace oz3 {

//==============================================================================
// ProcessorConfig
//==============================================================================

// The ProcessorConfig class is used to configure a Processor.
class ProcessorConfig {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a new ProcessorConfig with no resources.
  ProcessorConfig();

  // Sets a specific MemoryBank configuration for a given bank index.
  ProcessorConfig& SetMemoryBankConfig(int bank_index,
                                       const MemoryBankConfig& config);

  // Sets the number of cores in the processor.
  ProcessorConfig& SetNumCores(int num_cores);

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  absl::Span<const MemoryBankConfig> GetMemoryBankConfigs() const {
    return banks_;
  }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  std::vector<MemoryBankConfig> banks_;
  int num_cores_ = 0;
};

}  // namespace oz3

#endif  // OZ3_CORE_PROCESSOR_CONFIG_H_
