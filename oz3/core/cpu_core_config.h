// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_CPU_CORE_CONFIG_H_
#define OZ3_CORE_CPU_CORE_CONFIG_H_

namespace oz3 {

//==============================================================================
// Constants
//==============================================================================

// The maximum number of cores that can managed by a single Processor.
inline constexpr int kMaxCores = 8;

//==============================================================================
// CpuCoreBankAssignemt
//==============================================================================

// This specifies which banks are mapped to which core purposes. These can be
// changed during execution only by core 0 executing the RST instruction.
struct CpuCoreBanks {
  int code;   // Index of bank where code is executed.
  int stack;  // Index of bank where stack is located.
  int data;   // Index of general purpose data bank.
  int extra;  // Index of second general purpose data bank.
};

//==============================================================================
// CpuCoreConfig
//==============================================================================

// The CpuCoreConfig struct contains configuration information for a CPU core.
class CpuCoreConfig {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Constructs a core with all banks mapped to memory bank 0.
  CpuCoreConfig() = default;

  // Sets the bank assignments for this core.
  //
  // Bank indexes must be in the range [0, kMaxMemoryBanks).
  CpuCoreConfig& SetBanks(const CpuCoreBanks& banks);

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the bank assignments for this core.
  const CpuCoreBanks& GetBanks() const { return banks_; }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  CpuCoreBanks banks_ = {0, 0, 0, 0};
};

}  // namespace oz3

#endif  // OZ3_CORE_CPU_CORE_CONFIG_H_
