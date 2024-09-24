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

// The maximum number of ports that can be managed by a single Processor.
inline constexpr int kMaxPorts = 256;

// The number of cycles required to fetch and decode any instruction on a
// CpuCore. This is the minimum number of cycles an instruction can take (the
// NOP execution time).
inline constexpr int kCpuCoreFetchAndDecodeCycles =
    kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles + 1;

// The number of cycles required to start an interrupt on a CpuCore.
inline constexpr int kCpuCoreStartInterruptCycles =
    kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles * 2;

// The number of cycles required to execute each microcode operation.
inline constexpr int kCpuCoreCycles_MSC = 0;
inline constexpr int kCpuCoreCycles_MSS = 0;
inline constexpr int kCpuCoreCycles_MSX = 0;
inline constexpr int kCpuCoreCycles_MSM = 1;
inline constexpr int kCpuCoreCycles_MSR = 0;
inline constexpr int kCpuCoreCycles_ADR = kMemoryBankSetAddressCycles;
inline constexpr int kCpuCoreCycles_LAD = 0;
inline constexpr int kCpuCoreCycles_LD = kMemoryBankAccessWordCycles;
inline constexpr int kCpuCoreCycles_ST = kMemoryBankAccessWordCycles;
inline constexpr int kCpuCoreCycles_STP = kMemoryBankAccessWordCycles;
inline constexpr int kCpuCoreCycles_MOV = 1;
inline constexpr int kCpuCoreCycles_MOVI = 1;
inline constexpr int kCpuCoreCycles_ADD = 1;
inline constexpr int kCpuCoreCycles_ADC = 1;
inline constexpr int kCpuCoreCycles_ADDI = 1;
inline constexpr int kCpuCoreCycles_SUB = 1;
inline constexpr int kCpuCoreCycles_SBC = 1;
inline constexpr int kCpuCoreCycles_NEG = 1;
inline constexpr int kCpuCoreCycles_CMP = 1;
inline constexpr int kCpuCoreCycles_TST = 1;
inline constexpr int kCpuCoreCycles_NOT = 1;
inline constexpr int kCpuCoreCycles_AND = 1;
inline constexpr int kCpuCoreCycles_OR = 1;
inline constexpr int kCpuCoreCycles_XOR = 1;
inline constexpr int kCpuCoreCycles_SL = 1;
inline constexpr int kCpuCoreCycles_SR = 1;
inline constexpr int kCpuCoreCycles_SRA = 1;
inline constexpr int kCpuCoreCycles_RL = 1;
inline constexpr int kCpuCoreCycles_RR = 1;
inline constexpr int kCpuCoreCycles_RLC = 1;
inline constexpr int kCpuCoreCycles_RRC = 1;
inline constexpr int kCpuCoreCycles_JP = 0;
inline constexpr int kCpuCoreCycles_JC_False = 0;
inline constexpr int kCpuCoreCycles_JC_True = 1;
inline constexpr int kCpuCoreCycles_JD_NonZero = 1;
inline constexpr int kCpuCoreCycles_JD_Zero = 1;
inline constexpr int kCpuCoreCycles_INT = 0;
inline constexpr int kCpuCoreCycles_ILD = 1;
inline constexpr int kCpuCoreCycles_IST = 1;
inline constexpr int kCpuCoreCycles_PLD = 1;
inline constexpr int kCpuCoreCycles_PST = 1;
inline constexpr int kCpuCoreCycles_CBK = 1;
inline constexpr int kCpuCoreCycles_CLD = 1;
inline constexpr int kCpuCoreCycles_CST = 1;

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
