// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_CPU_CORE_H_
#define OZ3_CORE_CPU_CORE_H_

#include <cstdint>

#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core_config.h"
#include "oz3/core/execution_component.h"
#include "oz3/core/micro_code.h"

namespace oz3 {

//==============================================================================
// CpuCore
//==============================================================================

// This class for a single OZ-3 CPU core in the system.
class CpuCore final : public ExecutionComponent {
 public:
  //----------------------------------------------------------------------------
  // Constants and Types
  //----------------------------------------------------------------------------

  // General purpose 16-bit registers
  static constexpr int R0 = 0;  // 16-bit register, DATA bank addressing
  static constexpr int R1 = 1;  // 16-bit register, DATA bank addressing
  static constexpr int R2 = 2;  // 16-bit register, DATA bank addressing
  static constexpr int R3 = 3;  // 16-bit register, DATA bank addressing
  static constexpr int R4 = 4;  // 16-bit register, EXTRA bank addressing
  static constexpr int R5 = 5;  // 16-bit register, EXTRA bank addressing
  static constexpr int R6 = 6;  // 16-bit register, STACK bank addressing
  static constexpr int R7 = 7;  // 16-bit register, STACK bank addressing

  // General purpose 32-bit registers
  static constexpr int D0 = R0;  // 32-bit register (R0,R1)
  static constexpr int D1 = R2;  // 32-bit register (R2,R3)
  static constexpr int D2 = R4;  // 32-bit register (R4,R5)
  static constexpr int D3 = R6;  // 32-bit register (R6,R7)

  // Cache registers
  static constexpr int C0 = 8;   // Cache register 0
  static constexpr int C1 = 9;   // Cache register 1
  static constexpr int CD = C0;  // 32-bit cache register (C0,C1)

  // Special purpose registers
  static constexpr int SP = 10;  // Stack pointer
  static constexpr int DP = 11;  // Data pointer
  static constexpr int SD = SP;  // 32-bit Stack+data pointer (SP,DP)
  static constexpr int PC = 12;  // Program counter
  static constexpr int ST = 13;  // Status flags register

  // Number of 16-bit registers.
  static constexpr int kRegisterCount = ST + 1;

  // Flags in the ST register
  static constexpr uint16_t Z = 1 << 0;            // Zero flag
  static constexpr uint16_t S = 1 << 1;            // Sign flag
  static constexpr uint16_t C = 1 << 2;            // Carry flag
  static constexpr uint16_t O = 1 << 3;            // Overflow flag
  static constexpr uint16_t ZSCO = Z | S | C | O;  // All ALU flags
  static constexpr uint16_t I = 1 << 4;            // Interrupt enable flag
  static constexpr uint16_t T = 1 << 8;            // Trace flag

  // Banks reference in the CPU core
  static constexpr int CODE = 0;   // Code bank
  static constexpr int STACK = 1;  // Stack bank
  static constexpr int DATA = 2;   // Data bank
  static constexpr int EXTRA = 3;  // Extra bank

  // Internal state of the processor.
  enum class State {
    // Initial state of a core, and whenever HALT instruction is executed.
    kIdle,

    // The core is currently executing a "wait" statement.
    kWaiting,

    // The core is ready to start a new instruction.
    kStartInstruction,

    // The core is going to fetchand decode the next instruction, when it is
    // able (memory lock completes).
    kFetchInstruction,

    // The core is currently executing an instruction.
    kRunInstruction,
  };

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  explicit CpuCore(const CpuCoreConfig& config);
  CpuCore(const CpuCore&) = delete;
  CpuCore& operator=(const CpuCore&) = delete;
  ~CpuCore();

  //----------------------------------------------------------------------------
  // Attributes
  //----------------------------------------------------------------------------

  State GetState() { return state_; }

  //----------------------------------------------------------------------------
  // ExecutionComponent implementation
  //----------------------------------------------------------------------------

  void AttachProcessor(CoreInternal, Processor* processor) override;
  void Execute() override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  void StartInstruction();
  void FetchInstruction();
  void RunInstruction();

  // Owning processor.
  Processor* processor_;

  // Current execution state.
  State state_ = State::kIdle;
  Cycles exec_cycles_ = 0;  // Cycles accumulated during Execute.

  // Bank assignments.
  CpuCoreBanks bank_assignment_;
  MemoryBank* banks_[4] = {};

  // 16-bit registers.
  uint16_t r_[kRegisterCount] = {};

  // Interrupt vector table and IT register.
  uint32_t it_ = 0;
  uint16_t ivec_[32] = {};

  // Lock for a dependent component the CpuCore requires.
  std::unique_ptr<ComponentLock> lock_;

  // Microcode implementation
  InstructionMicroCodes micro_codes_;
  DecodedInstruction instruction_ = {};
  int mc_index_ = 0;  // Micro code index into the current instruction.
};

}  // namespace oz3

#endif  // OZ3_CORE_CPU_CORE_H_
