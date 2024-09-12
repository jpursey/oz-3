// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_CPU_CORE_CONFIG_H_
#define OZ3_CORE_CPU_CORE_CONFIG_H_

#include "absl/types/span.h"
#include "oz3/core/instruction.h"
#include "oz3/core/memory_bank_config.h"

namespace oz3 {

//==============================================================================
// Constants
//==============================================================================

// The maximum number of cores that can managed by a single Processor.
inline constexpr int kMaxCores = 8;

// The number of cycles required to fetch and decode any instruction on an
// CpuCore. This is the minimum number of cycles an instruction can take (the
// NOP execution time).
inline constexpr int kCpuCoreFetchAndDecodeCycles =
    kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles + 1;

// The number of cycles required to execute each microcode operation.
inline constexpr int kCpuCoreCycles_MOV = 1;
inline constexpr int kCpuCoreCycles_MOVI = kCpuCoreCycles_MOV;
inline constexpr int kCpuCoreCycles_ADD = 1;
inline constexpr int kCpuCoreCycles_ADDI = kCpuCoreCycles_ADD;
inline constexpr int kCpuCoreCycles_SUB = 1;

//==============================================================================
// CpuCoreConfig
//==============================================================================

// The CpuCoreConfig struct contains configuration information for a CPU core.
class CpuCoreConfig {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Constructs a default core configuration using the base OZ-3 instruction
  // set.
  static CpuCoreConfig Default() { return CpuCoreConfig(); }

  // Constructs a core with all banks mapped to memory bank 0.
  CpuCoreConfig();

  // Sets the instruction set for this core.
  //
  // By default, this is the full instruction set defined by the OZ-3
  // specifications. Custom implementations may choose to override this with a
  // custom instruction set.
  CpuCoreConfig& SetInstructionSet(
      absl::Span<const InstructionDef> instructions);

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the instruction set for this core.
  absl::Span<const InstructionDef> GetInstructions() const {
    return instructions_;
  }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  absl::Span<const InstructionDef> instructions_;
};

}  // namespace oz3

#endif  // OZ3_CORE_CPU_CORE_CONFIG_H_
