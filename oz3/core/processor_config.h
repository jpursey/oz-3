// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_PROCESSOR_CONFIG_H_
#define OZ3_CORE_PROCESSOR_CONFIG_H_

#include <vector>

#include "absl/types/span.h"
#include "oz3/core/cpu_core_config.h"
#include "oz3/core/default_instruction_set.h"
#include "oz3/core/memory_bank_config.h"

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

  // Creates an empty ProcessorConfig with no resources.
  static ProcessorConfig Empty() { return {}; }

  // Creates a ProcessorConfig with a single core and a single memory bank with
  // max RAM.
  static ProcessorConfig OneCore(std::shared_ptr<const InstructionSet>
                                     instructions = GetDefaultInstructionSet());

  // Creates a ProcessorConfig with the specified number of cores (up to
  // kMaxCores) and a single memory bank with max RAM (can be overridden).
  static ProcessorConfig MultiCore(
      int num_cores, std::shared_ptr<const InstructionSet> instructions =
                         GetDefaultInstructionSet());

  // Creates a ProcessorConfig with the specified number of memory banks (up to
  // kMaxMemoryBanks) with max RAM (can be overridden) and specified number of
  // cores (up to kMaxCores).
  static ProcessorConfig MultiBankMultiCore(
      int num_banks, int num_cores,
      std::shared_ptr<const InstructionSet> instructions =
          GetDefaultInstructionSet());

  // Creates a new ProcessorConfig with no resources.
  ProcessorConfig();

  // Sets a specific MemoryBank configuration for a given bank index.
  ProcessorConfig& SetMemoryBank(int bank_index, MemoryBankConfig config);

  // Sets the number of cores in the processor.
  ProcessorConfig& AddCpuCore(CpuCoreConfig config);

  // Sets the number of ports in the processor.
  //
  // The port count must be in the range [0, kMaxPorts].
  ProcessorConfig& SetPortCount(int count);

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  absl::Span<const MemoryBankConfig> GetMemoryBankConfigs() const {
    return banks_;
  }

  absl::Span<const CpuCoreConfig> GetCpuCoreConfigs() const { return cores_; }

  int GetPortCount() const { return port_count_; }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  std::vector<MemoryBankConfig> banks_;
  std::vector<CpuCoreConfig> cores_;
  int port_count_ = 0;
};

}  // namespace oz3

#endif  // OZ3_CORE_PROCESSOR_CONFIG_H_
