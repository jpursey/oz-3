// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_CORE_TYPES_H_
#define OZ3_CORE_CORE_TYPES_H_

#include <cstdint>

#include "gb/base/access_token.h"

namespace oz3 {

//==============================================================================
// Types
//==============================================================================

// Defines a number of cycles over a period of time or since "boot".
using Cycles = int64_t;

//==============================================================================
// Constants
//==============================================================================

//------------------------------------------------------------------------------
// MemoryBank constants
//------------------------------------------------------------------------------

// The maximum number of memory banks that can managed by a single Processor.
inline constexpr int kMaxMemoryBanks = 16;

// Total number of MemoryBank pages.
inline constexpr int kMemoryBankPageCount = 16;

// Size in 16-bit words of a MemoryBank page.
inline constexpr int kMemoryBankPageSize = 4096;

// Max size in 16-bit words of a MemoryBank.
inline constexpr int kMemoryBankMaxSize =
    kMemoryBankPageCount * kMemoryBankPageSize;
static_assert(kMemoryBankMaxSize - 1 == std::numeric_limits<uint16_t>::max());

// Cycle timing constants for MemoryBank access
inline constexpr Cycles kMemoryBankSetAddressCycles = 1;
inline constexpr Cycles kMemoryBankAccessWordCycles = 1;

//------------------------------------------------------------------------------
// Processor constants
//------------------------------------------------------------------------------

// The maximum number of cores that can managed by a single Processor.
inline constexpr int kMaxCores = 8;

// The maximum number of ports that can be managed by a single Processor.
inline constexpr int kMaxPorts = 256;

//------------------------------------------------------------------------------
// CpuCore constants
//------------------------------------------------------------------------------

// The number of cycles required to fetch and decode any instruction on a
// CpuCore. This is the minimum number of cycles an instruction can take (the
// NOP execution time).
inline constexpr int kCpuCoreFetchAndDecodeCycles =
    kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles + 1;

// The number of cycles required to start an interrupt on a CpuCore.
inline constexpr int kCpuCoreStartInterruptCycles =
    kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles * 2;

// The number of cycles required to return from an interrupt on a CpuCore.
inline constexpr int kCpuCoreReturnFromInterruptCycles =
    kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles * 2;

// The number of cycles required to execute each microcode operation.
inline constexpr Cycles kCpuCoreCycles_MSC = 0;
inline constexpr Cycles kCpuCoreCycles_MSS = 0;
inline constexpr Cycles kCpuCoreCycles_MSX = 0;
inline constexpr Cycles kCpuCoreCycles_MSM = 1;
inline constexpr Cycles kCpuCoreCycles_MSR = 0;
inline constexpr Cycles kCpuCoreCycles_ADR = kMemoryBankSetAddressCycles;
inline constexpr Cycles kCpuCoreCycles_LAD = 0;
inline constexpr Cycles kCpuCoreCycles_LD = kMemoryBankAccessWordCycles;
inline constexpr Cycles kCpuCoreCycles_ST = kMemoryBankAccessWordCycles;
inline constexpr Cycles kCpuCoreCycles_STP = kMemoryBankAccessWordCycles;
inline constexpr Cycles kCpuCoreCycles_MOV = 1;
inline constexpr Cycles kCpuCoreCycles_MOVI = 1;
inline constexpr Cycles kCpuCoreCycles_MVB = 1;
inline constexpr Cycles kCpuCoreCycles_MVBI = 1;
inline constexpr Cycles kCpuCoreCycles_MVN = 1;
inline constexpr Cycles kCpuCoreCycles_MVNI = 1;
inline constexpr Cycles kCpuCoreCycles_ADD = 1;
inline constexpr Cycles kCpuCoreCycles_ADC = 1;
inline constexpr Cycles kCpuCoreCycles_ADDI = 1;
inline constexpr Cycles kCpuCoreCycles_SUB = 1;
inline constexpr Cycles kCpuCoreCycles_SBC = 1;
inline constexpr Cycles kCpuCoreCycles_NEG = 1;
inline constexpr Cycles kCpuCoreCycles_TST = 1;
inline constexpr Cycles kCpuCoreCycles_CMP = 1;
inline constexpr Cycles kCpuCoreCycles_NOT = 1;
inline constexpr Cycles kCpuCoreCycles_AND = 1;
inline constexpr Cycles kCpuCoreCycles_OR = 1;
inline constexpr Cycles kCpuCoreCycles_XOR = 1;
inline constexpr Cycles kCpuCoreCycles_SL = 1;
inline constexpr Cycles kCpuCoreCycles_SR = 1;
inline constexpr Cycles kCpuCoreCycles_SRA = 1;
inline constexpr Cycles kCpuCoreCycles_RL = 1;
inline constexpr Cycles kCpuCoreCycles_RR = 1;
inline constexpr Cycles kCpuCoreCycles_RLC = 1;
inline constexpr Cycles kCpuCoreCycles_RRC = 1;
inline constexpr Cycles kCpuCoreCycles_JP = 0;
inline constexpr Cycles kCpuCoreCycles_JC_False = 0;
inline constexpr Cycles kCpuCoreCycles_JC_True = 1;
inline constexpr Cycles kCpuCoreCycles_JD_NonZero = 1;
inline constexpr Cycles kCpuCoreCycles_JD_Zero = 1;
inline constexpr Cycles kCpuCoreCycles_INT = 0;
inline constexpr Cycles kCpuCoreCycles_ILD = 1;
inline constexpr Cycles kCpuCoreCycles_IST = 1;
inline constexpr Cycles kCpuCoreCycles_PLD = 1;
inline constexpr Cycles kCpuCoreCycles_PST = 1;
inline constexpr Cycles kCpuCoreCycles_CBK = 1;
inline constexpr Cycles kCpuCoreCycles_CLD = 1;
inline constexpr Cycles kCpuCoreCycles_CST = 1;

//==============================================================================
// Forward declarations
//==============================================================================

class Component;
class CpuCore;
class CpuCoreConfig;
class ExecutionComponent;
class InstructionCompiler;
class InstructionSet;
class Lock;
class Lockable;
class MemoryMap;
class MemoryBank;
class MemoryBankConfig;
class Port;
class PortBank;
class Processor;
class ProcessorConfig;

//==============================================================================
// Access Tokens
//==============================================================================

GB_BEGIN_ACCESS_TOKEN(CoreInternal)
friend class Processor;
friend class CpuCore;
GB_END_ACCESS_TOKEN()

}  // namespace oz3

#endif  // OZ3_CORE_CORE_TYPES_H_