// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_CPU_CORE_H_
#define OZ3_CORE_CPU_CORE_H_

#include <cstdint>

#include "gb/container/array.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core_config.h"
#include "oz3/core/execution_component.h"
#include "oz3/core/microcode.h"

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
  static constexpr int PC = 10;  // Program counter
  static constexpr int SP = 11;  // Stack pointer
  static constexpr int DP = 12;  // Data pointer
  static constexpr int SD = SP;  // 32-bit Stack+data pointer (SP,DP)
  static constexpr int ST = 13;  // Status flags register
  static constexpr int BM = 14;  // Bank mapping register

  // Number of 16-bit registers.
  static constexpr int kRegisterCount = BM + 1;

  // Flags in the ST register
  static constexpr uint16_t ZShift = 0;       // Zero flag shift
  static constexpr uint16_t SShift = 1;       // Sign flag shift
  static constexpr uint16_t CShift = 2;       // Carry flag shift
  static constexpr uint16_t OShift = 3;       // Overflow flag shift
  static constexpr uint16_t IShift = 4;       // Interrupt enable flag shift
  static constexpr uint16_t TShift = 8;       // Trace flag shift
  static constexpr uint16_t Z = 1 << ZShift;  // Zero flag
  static constexpr uint16_t S = 1 << SShift;  // Sign flag
  static constexpr uint16_t C = 1 << CShift;  // Carry flag
  static constexpr uint16_t O = 1 << OShift;  // Overflow flag
  static constexpr uint16_t ZSCO = Z | S | C | O;  // All ALU flags
  static constexpr uint16_t I = 1 << IShift;       // Interrupt enable flag
  static constexpr uint16_t T = 1 << TShift;       // Trace flag

  // Banks reference in the CPU core
  static constexpr int CODE = 0;   // Code bank
  static constexpr int STACK = 1;  // Stack bank
  static constexpr int DATA = 2;   // Data bank
  static constexpr int EXTRA = 3;  // Extra bank

  // Registers of the CPU core.
  using Registers = gb::Array<uint16_t, kRegisterCount>;

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

  // This specifies which banks are mapped to which core purposes. These can be
  // changed during execution only by calling the Reset function.
  struct Banks {
    static Banks Default() { return {0, 0, 0, 0}; }
    static Banks FromWord(uint16_t value) {
      return {.code = static_cast<int>(value & 0xF),
              .stack = static_cast<int>((value >> 4) & 0xF),
              .data = static_cast<int>((value >> 8) & 0xF),
              .extra = static_cast<int>((value >> 12) & 0xF)};
    }
    uint16_t ToWord() const {
      return static_cast<uint16_t>(code & 0xF) |
             (static_cast<uint16_t>(stack & 0xF) << 4) |
             (static_cast<uint16_t>(data & 0xF) << 8) |
             (static_cast<uint16_t>(extra & 0xF) << 12);
    }

    Banks& SetCode(int in_code) {
      code = in_code;
      return *this;
    }
    Banks& SetStack(int in_stack) {
      stack = in_stack;
      return *this;
    }
    Banks& SetData(int in_data) {
      data = in_data;
      return *this;
    }
    Banks& SetExtra(int in_extra) {
      extra = in_extra;
      return *this;
    }

    int code = 0;   // Index of bank where code is executed.
    int stack = 0;  // Index of bank where stack is located.
    int data = 0;   // Index of general purpose data bank.
    int extra = 0;  // Index of second general purpose data bank.
  };

  // Reset() parameters for the CPU core.
  struct ResetParams {
    // Bit flags for `reset_mask`.
    static constexpr uint16_t BC = 1 << 0;  // Set code bank.
    static constexpr uint16_t BS = 1 << 1;  // Set stack bank.
    static constexpr uint16_t BD = 1 << 2;  // Set data bank.
    static constexpr uint16_t BE = 1 << 3;  // Set extra bank.
    static constexpr uint16_t PC = 1 << 4;  // Set program counter.
    static constexpr uint16_t SP = 1 << 5;  // Set stack pointer.
    static constexpr uint16_t DP = 1 << 6;  // Set data pointer.
    static constexpr uint16_t ALL = BC | BS | BD | BE | PC | SP | DP;

    // Specifies which registers (and parts of registers) to reset.
    uint16_t mask = ALL;

    // Register values to set.
    uint16_t bm = 0;  // BM register. Requires reset_mask & (BC | BS | BD | BE).
    uint16_t pc = 0;  // PC register. Requires reset_mask & PC.
    uint16_t sp = 0;  // SP register. Requires reset_mask & SP.
    uint16_t dp = 0;  // DP register. Requires reset_mask & DP.
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

  // Returns the registers of the CPU core.
  //
  // If the core is ahead of the Processor, this may not be simulation accurate.
  // If that is needed, then the caller should call Lock() and wait for it to
  // be locked to ensure the CpuCore is not mid-instruction.
  void GetRegisters(gb::Array<uint16_t, kRegisterCount>& registers) const;

  // Returns the current MemoryBank for the specified bank index.
  //
  // This will be null until the core is attached to a processor.
  MemoryBank* GetMemoryBank(int bank_index) const { return banks_[bank_index]; }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Resets the CPU core, starting execution with the specified parameters.
  //
  // This function is only valid after the core has been attached to a
  // processor.
  void Reset(const ComponentLock& lock, const ResetParams& params);

  // Sets the specified 16-bit register value.
  //
  // It must be a valid register index: [0, kRegisterCount).
  void SetWordRegister(const ComponentLock& lock, int reg, uint16_t value);

  // Sets the specified 32-bit register value.
  //
  // It must be a valid 32-bit register index (D0, D1, D2, D3, CD, or SD).
  void SetDwordRegister(const ComponentLock& lock, int reg, uint32_t value);

  //----------------------------------------------------------------------------
  // ExecutionComponent implementation
  //----------------------------------------------------------------------------

  void AttachProcessor(CoreInternal, Processor* processor) override;
  void Execute() override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  void InitBanks();
  void StartInstruction();
  void FetchInstruction();
  void RunInstruction();

  // Owning processor.
  Processor* processor_ = nullptr;

  // Current execution state.
  State state_ = State::kIdle;
  Cycles exec_cycles_ = 0;  // Cycles accumulated during Execute.

  // Bank assignments.
  MemoryBank* banks_[4] = {};

  // 16-bit registers.
  uint16_t r_[kRegisterCount] = {};
  static_assert(sizeof(r_) == sizeof(Registers));

  // Interrupt vector table and IT register.
  uint32_t it_ = 0;
  uint16_t ivec_[32] = {};

  // Lock for a dependent component the CpuCore requires.
  std::unique_ptr<ComponentLock> lock_;
  int locked_bank_ = -1;  // Current locked bank index.

  // Microcode implementation
  InstructionMicrocodes micro_codes_;
  DecodedInstruction instruction_ = {};
  int mpc_ = 0;       // Microcode index into the current instruction.
  uint16_t mst_ = 0;  // Status flags from microcode (same ST register flags).
};

}  // namespace oz3

#endif  // OZ3_CORE_CPU_CORE_H_
