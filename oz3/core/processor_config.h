// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_PROCESSOR_CONFIG_H_
#define OZ3_CORE_PROCESSOR_CONFIG_H_

#include <cstdint>
#include <memory>
#include <vector>

#include "absl/status/status.h"
#include "absl/types/span.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core_config.h"
#include "oz3/core/instruction_set.h"
#include "oz3/core/memory_bank_config.h"

namespace oz3 {

//==============================================================================
// ProcessorConfig
//==============================================================================

// The ProcessorConfig class is used to configure a Processor.
class ProcessorConfig {
 public:
  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The clock rate a config has until it is set (see SetClockHz): 1 MHz.
  static constexpr int64_t kDefaultClockHz = 1'000'000;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates an empty ProcessorConfig with no resources.
  static ProcessorConfig Empty() { return {}; }

  // Creates a ProcessorConfig with a single core running the specified
  // instruction set, and a single memory bank with max RAM.
  static ProcessorConfig OneCore(
      std::shared_ptr<const InstructionSet> instructions);

  // Creates a ProcessorConfig with the specified number of cores (up to
  // kMaxCores) all running the specified instruction set, and a single memory
  // bank with max RAM (can be overridden).
  static ProcessorConfig MultiCore(
      int num_cores, std::shared_ptr<const InstructionSet> instructions);

  // Creates a ProcessorConfig with the specified number of memory banks (up to
  // kMaxMemoryBanks) with max RAM (can be overridden), and no cores.
  static ProcessorConfig MultiBank(int num_banks);

  // Creates a ProcessorConfig with the specified number of memory banks (up to
  // kMaxMemoryBanks) with max RAM (can be overridden) and specified number of
  // cores (up to kMaxCores), all running the specified instruction set.
  static ProcessorConfig MultiBankMultiCore(
      int num_banks, int num_cores,
      std::shared_ptr<const InstructionSet> instructions);

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

  // Adds a device, which each Processor created from this config creates from
  // `factory`. The factory must not be null.
  ProcessorConfig& AddDevice(std::shared_ptr<const DeviceFactory> factory);

  // Sets the processor's clock rate, in cycles per second, which must be at
  // least 1 (see Validate). Everything still runs in cycles: the clock rate
  // is how devices turn rates in real time, such as a baud rate, into cycles,
  // so their settings work the same at any clock rate.
  ProcessorConfig& SetClockHz(int64_t clock_hz);

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Returns an error if the clock rate is less than 1, or one naming the device
  // if a device's config doesn't fit this one: no name, or a name another
  // device has (as a factory added twice does), a port the config doesn't
  // have, an interrupt the cores don't have, or pages that don't fit in their
  // bank or overlap another device's.
  //
  // This checks the config as a whole, so it doesn't matter what order it was
  // built in. A Processor must be created from a valid config.
  absl::Status Validate() const;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  absl::Span<const MemoryBankConfig> GetMemoryBankConfigs() const {
    return banks_;
  }

  absl::Span<const CpuCoreConfig> GetCpuCoreConfigs() const { return cores_; }

  int GetPortCount() const { return port_count_; }

  absl::Span<const std::shared_ptr<const DeviceFactory>> GetDevices() const {
    return devices_;
  }
  int64_t GetClockHz() const { return clock_hz_; }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  std::vector<MemoryBankConfig> banks_;
  std::vector<CpuCoreConfig> cores_;
  int port_count_ = 0;
  std::vector<std::shared_ptr<const DeviceFactory>> devices_;
  int64_t clock_hz_ = kDefaultClockHz;
};

}  // namespace oz3

#endif  // OZ3_CORE_PROCESSOR_CONFIG_H_
