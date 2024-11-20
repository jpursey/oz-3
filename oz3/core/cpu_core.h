// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_CPU_CORE_H_
#define OZ3_CORE_CPU_CORE_H_

#include <cstdint>

#include "absl/log/check.h"
#include "absl/log/log.h"
#include "gb/container/array.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core_config.h"
#include "oz3/core/execution_component.h"
#include "oz3/core/instruction_set.h"
#include "oz3/core/microcode.h"

namespace oz3 {

//==============================================================================
// CpuCore
//==============================================================================

// This class for a single OZ-3 CPU core in the system.
class CpuCore final : public ExecutionComponent {
 public:
  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // Number of interrupts.
  static constexpr int kInterruptCount = 32;

  // Number of 16-bit registers.
  static constexpr int kRegisterCount = 20;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // General purpose registers (addressible with 3 bits)
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Special purpose registers (addressible with 4 bits)
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  static constexpr int BC = 8;   // Base pointer (code bank)
  static constexpr int BS = 9;   // Base pointer (stack bank)
  static constexpr int BD = 10;  // Base pointer (data bank)
  static constexpr int BE = 11;  // Base pointer (extra bank)
  static constexpr int MB = 12;  // Memory bank register (R/O outside of CBK)
  static constexpr int IP = 13;  // Instruction pointer (offset from BC)
  static constexpr int FP = 14;  // Frame pointer (offset from BS)
  static constexpr int SP = 15;  // Stack pointer (offset from BS)

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Special purpose registers (addressible only in microcode)
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // Status flags register
  static constexpr int ST = 19;  // Status register (ready-only outside of MSR)

  // Cache registers (set as noted at the beginning of each instruction)
  static constexpr int C0 = 16;  // Cache register 0 (first arg or zero)
  static constexpr int C1 = 17;  // Cache register 1 (second arg or zero)
  static constexpr int C2 = 18;  // Cache register 2 (zero)
  static constexpr int CD = C0;  // Cache 32-bit register (C0,C1)
 
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Instruction argument register indexes from microcode
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  static constexpr int8_t A = -1;   // 1st arg: (word or dword)
  static constexpr int8_t A0 = -1;  // 1st arg, low word: index in decoded r[0]
  static constexpr int8_t A1 = -3;  // 1st arg, high word: index in decoded r[2]
  static constexpr int8_t B = -2;   // 2nd arg: (word or dword)
  static constexpr int8_t B0 = -2;  // 2nd arg, low word: index in decoded r[1]
  static constexpr int8_t B1 = -4;  // 2nd arg, high word: index in decoded r[3]

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Virtual macro registers used with macro definitions
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  static constexpr int8_t MP = -5;   // Macro parameter (word or dword)
  static constexpr int8_t MP0 = -5;  // Macro parameter, low word
  static constexpr int8_t MP1 = -6;  // Macro parameter, high word

  static constexpr int8_t MR = -7;   // Macro return (word or dword)
  static constexpr int8_t MR0 = -7;  // Macro return, low word
  static constexpr int8_t MR1 = -8;  // Macro return, high word

  static constexpr int8_t MM = -9;    // Macro register argument (word or dword)
  static constexpr int8_t MM0 = -9;   // Macro register argument, low word
  static constexpr int8_t MM1 = -10;  // Macro register argument, high word

  static constexpr int8_t MI = -11;  // Macro immediate argument

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Constants for register index classes
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  static constexpr int8_t kMaxWordArgReg = A0;  // Min word register arg index
  static constexpr int8_t kMinWordArgReg = B1;  // Max word register arg index

  static constexpr int8_t kMaxDwordArgReg = A;  // Min dword register arg index
  static constexpr int8_t kMinDwordArgReg = B;  // Max dword register arg index

  static constexpr int8_t kMinArgReg =
      std::min(kMinWordArgReg, kMinDwordArgReg);  // Min register arg index

  static constexpr int8_t kMaxMacroReg = MP0;  // Min macro register index
  static constexpr int8_t kMinMacroReg = MI;   // Max macro register index

  static constexpr int8_t kMinVirtualReg = MI;  // Min virtual register index

  static constexpr int8_t kInvalidReg = kMinVirtualReg - 1;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Status flags in the ST register. The upper byte of the ST register is for
  // external control flags and cannot be changed by microcode.
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // Status flags
  static constexpr uint16_t ZShift = 0;       // Zero flag shift
  static constexpr uint16_t SShift = 1;       // Sign flag shift
  static constexpr uint16_t CShift = 2;       // Carry flag shift
  static constexpr uint16_t OShift = 3;       // Overflow flag shift
  static constexpr uint16_t IShift = 4;       // Interrupt enable flag shift
  static constexpr uint16_t Z = 1 << ZShift;  // Zero flag
  static constexpr uint16_t S = 1 << SShift;  // Sign flag
  static constexpr uint16_t C = 1 << CShift;  // Carry flag
  static constexpr uint16_t O = 1 << OShift;  // Overflow flag
  static constexpr uint16_t ZSCO = Z | S | C | O;  // All ALU flags
  static constexpr uint16_t I = 1 << IShift;       // Interrupt enable flag
  static constexpr uint16_t ZSCOI = ZSCO | I;      // All core-settable flags

  // Control flags
  static constexpr uint16_t TShift = 8;       // Trace flag shift
  static constexpr uint16_t WShift = 9;       // Wait flag shift
  static constexpr uint16_t T = 1 << TShift;  // Trace flag
  static constexpr uint16_t W = 1 << WShift;  // Wait flag

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Banks
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // Banks reference in the CPU core
  static constexpr int CODE = 0;   // Code bank
  static constexpr int STACK = 1;  // Stack bank
  static constexpr int DATA = 2;   // Data bank
  static constexpr int EXTRA = 3;  // Extra bank

  // Register / memory bank association.
  static constexpr int kRegisterBankMap[] = {
      DATA, DATA,  DATA, DATA, EXTRA, EXTRA, STACK, STACK, CODE, STACK,
      DATA, EXTRA, CODE, CODE, STACK, STACK, CODE,  CODE,  CODE, CODE,
  };
  static_assert(ABSL_ARRAYSIZE(kRegisterBankMap) == kRegisterCount);

  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // Registers of the CPU core.
  using Registers = gb::Array<uint16_t, kRegisterCount>;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Internal state of the processor.
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  enum class State {
    // Initial state of a core, and whenever HALT instruction is executed.
    // Transitions to:
    //   - kHandleInterrupt if an interrupt is raised and interrupts are enabled
    //   - kStartInstruction when Reset is called
    kIdle,

    // The core is currently executing a "wait" statement.
    // Transitions to:
    //   - kHandleInterrupt if an interrupt is raised and interrupts are enabled
    //   - kStartInstruction when Reset is called or wait is completed
    kWaiting,

    // The core is starting to handle an interrupt request.
    // Transitions to:
    //   - kPushInterruptState after locking STACK memory
    kHandleInterrupt,

    // The core is pushing the current state onto the stack.
    // Transitions to:
    //   - kStartInterrupt after pushing the state and updating registers.
    kPushInterruptState,

    // The core is starting execution of the interrupt.
    // Transitions to:
    //   - kFetchInstruction after locking CODE memory.
    kStartInterrupt,

    // The core is returning from an interrupt (reversing PushInterruptState).
    // Transitions to:
    //   - kStartInstruction after popping the state from the stack.
    kReturnFromInterrupt,

    // The core is ready to start a new instruction.
    // Transitions to:
    //   - kHandleInterrupt if an interrupt is raised and interrupts are enabled
    //   - kFetchInstruction after locking CODE memory.
    kStartInstruction,

    // The core is going to fetchand decode the next instruction, when it is
    // able (memory lock completes).
    // Transitions to:
    //   - kRunInstruction after fetching and decoding the instruction.
    kFetchInstruction,

    // The core is currently executing an instruction.
    // Transitions to:
    //   - kStartInstruction after the instruction completes.
    //   - kWaiting if WAIT microcode is executed and did not complete.
    //   - kIdle if HALT microcode is executed.
    //   - kReturnFromInterrupt if IRT microcode is executed.
    kRunInstruction,
  };

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Bank mapping conversion helper
  //
  // This is a helper struct that can be used to convert a 16-bit word (in the
  // format of the MB register) to and from the Banks struct. This is useful for
  // serialization and deserialization of the bank mapping.
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // ResetParams
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // Reset() parameters for the CPU core.
  //
  // Resets the CPU memory bank assignments and associated address registers.
  // Only the specified memory bank assignmends at registers are reset, and the
  // rest are left unchanged. In addition, the IP register is set to zero if the
  // BC register is reset, and the SP and FP registers are set to zero if the BS
  // register is reset.
  struct ResetParams {
    // Bit flags for `reset_mask`.
    static constexpr uint16_t MC = 1 << 0;   // Set code bank.
    static constexpr uint16_t MS = 1 << 1;   // Set stack bank.
    static constexpr uint16_t MD = 1 << 2;   // Set data bank.
    static constexpr uint16_t ME = 1 << 3;   // Set extra bank.
    static constexpr uint16_t RBC = 1 << 4;  // Set BC register.
    static constexpr uint16_t RBS = 1 << 5;  // Set BS register.
    static constexpr uint16_t RBD = 1 << 6;  // Set BD register.
    static constexpr uint16_t RBE = 1 << 7;  // Set BE register.
    static constexpr uint16_t ALL = MC | MS | MD | ME | RBC | RBS | RBD | RBE;

    // Specifies which registers (and parts of registers) to reset.
    uint16_t mask = ALL;

    // Register values to set.
    uint16_t mb = 0;  // MB register. Requires reset_mask & (MC | MS | MD | ME)
    uint16_t bc = 0;  // BC register. Requires reset_mask & RBC
    uint16_t bs = 0;  // BS register. Requires reset_mask & RBS
    uint16_t bd = 0;  // BD register. Requires reset_mask & RBD
    uint16_t be = 0;  // BE register. Requires reset_mask & RBE
  };

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  explicit CpuCore(const CpuCoreConfig& config);
  CpuCore(const CpuCore&) = delete;
  CpuCore& operator=(const CpuCore&) = delete;
  ~CpuCore();

  //----------------------------------------------------------------------------
  // Utility methods
  //----------------------------------------------------------------------------

  // Returns true if the index is a 16-bit register index.
  static bool IsWordReg(int reg) { return reg >= 0 && reg < kRegisterCount; }

  // Returns all names for the 16-bit registers.
  static absl::Span<const std::string_view> GetWordRegisterNames();

  // Returns the standard name for the specified 16-bit register, or "invalid"
  // if it is not a valid register.
  static std::string_view GetWordRegName(int reg);

  // Returns the 16-bit register index for the specified register name, or
  // kInvalidReg if the name is not a valid 16-bit register name.
  static int GetWordRegFromName(std::string_view name);

  // Returns the standard name for the specified 16-bit register whether it is
  // a physical register or a virtual register, or "invalid" if it is not a
  // valid register at all. Note that virtual word registers for some values can
  // have multiple names ("a" and "a0" for example). In these cases, both names
  // are returned in the string ("a/a0" for instance).
  static std::string_view GetVirtualWordRegName(int reg);

  // Returns the 16-bit register index for the specified register name including
  // virtual registers, or kInvalidReg if the name is not a valid 16-bit
  // register or virtual register name.
  static int GetVirtualWordRegFromName(std::string_view name);

  // Returns true if the index is a 32-bit register index.
  static bool IsDwordReg(int reg) {
    return reg == D0 || reg == D1 || reg == D2 || reg == D3 || reg == CD;
  }

  // Returns all names for the 32-bit registers.
  static absl::Span<const std::string_view> GetDwordRegisterNames();

  // Returns the standard name for the specified 32-bit register, or "invalid"
  // if it is not a valid register.
  static std::string_view GetDwordRegName(int reg);

  // Returns the 32-bit register index for the specified register name, or -1 if
  // the name is not a valid 32-bit register name.
  static int GetDwordRegFromName(std::string_view name);

  // Returns the standard name for the specified 32-bit register whether it is
  // a physical register or a virtual register, or "invalid" if it is not a
  // valid register at all.
  static std::string_view GetVirtualDwordRegName(int reg);

  // Returns the 32-bit register index for the specified register name including
  // virtual registers, or kInvalidReg if the name is not a valid 32-bit
  // register or virtual register name.
  static int GetVirtualDwordRegFromName(std::string_view name);

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
  MemoryBank* GetMemoryBank(int bank_index) const {
    DCHECK(bank_index >= 0 && bank_index < 4);
    return banks_[bank_index];
  }

  // Returns the current interrupt state.
  uint32_t GetInterrupts() const { return it_; }

  // Returns the current interrupt vector address for the specified interrupt.
  uint16_t GetInterruptAddress(int interrupt) const {
    DCHECK(interrupt >= 0 && interrupt < kInterruptCount);
    return ivec_[interrupt];
  }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Resets the CPU core, starting execution with the specified parameters.
  //
  // This function is only valid after the core has been attached to a
  // processor.
  void Reset(const Lock& lock, const ResetParams& params);

  // Sets the specified 16-bit register value.
  //
  // It must be a valid register index: [0, kRegisterCount).
  void SetWordRegister(const Lock& lock, int reg, uint16_t value);

  // Sets the specified 32-bit register value.
  //
  // It must be a valid 32-bit register index (see constants above).
  void SetDwordRegister(const Lock& lock, int reg, uint32_t value);

  // Sets the interrupt vector for the specified interrupt.
  //
  // It must be a valid interrupt index: [0, kInterruptCount).
  void RaiseInterrupt(int interrupt);

  // Returns true if an interrupt is pending (will be handled next).
  bool IsInterruptPending() const;

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

  // State handlers
  void Idle();
  void Waiting();
  void HandleInterrupt();
  void PushInterruptState();
  void StartInterrupt();
  void ReturnFromInterrupt();
  void StartInstruction();
  void FetchInstruction();
  void RunInstruction();
  void RunInstructionLoop();

  // Owning processor.
  Processor* processor_ = nullptr;

  // Current execution state.
  State state_ = State::kIdle;
  Cycles exec_cycles_ = 0;  // Cycles accumulated during Execute.
  Cycles fetch_start_ = 0;  // Cycle count when fetch started

  // Wait state
  Cycles wait_end_ = 0;    // Cycle count when wait should end
  uint16_t wait_pc_ = 0;   // IP value to return to after wait completes.
  uint16_t wait_reg_ = 0;  // Register that will receive completion info.

  // Bank assignments.
  MemoryBank* banks_[4] = {};

  // 16-bit registers.
  uint16_t r_[kRegisterCount] = {};
  static_assert(sizeof(r_) == sizeof(Registers));

  // Interrupt state.
  int interrupt_ = -1;                   // Current interrupt being started.
  uint32_t it_ = 0;                      // IT register
  uint16_t ivec_[kInterruptCount] = {};  // Interrupt vector table.
  static_assert(sizeof(it_) >= kInterruptCount / 8);

  // Lock for a dependent resource the CpuCore requires.
  std::unique_ptr<Lock> lock_;
  int locked_bank_ = -1;         // Current locked bank index.
  int locked_port_ = -1;         // Current locked port index.
  CpuCore* locked_core_ = this;  // Current locked core.

  // Microcode implementation
  std::shared_ptr<const InstructionSet> instructions_;
  DecodedInstruction instruction_ = {};
  int mip_ = 0;       // Microcode index into the current instruction.
  uint16_t mst_ = 0;  // Status flags from microcode (same ST register flags).
  uint16_t msr_ = 0;  // Status flags that are the result of the instruction.
  uint16_t mbm_ = 0;  // Bank mapping from microcode (same MB register).
};
static_assert(sizeof(CpuCore) < 512, "CpuCore too big");

inline bool CpuCore::IsInterruptPending() const {
  return it_ != 0 && (r_[ST] & I) != 0;
}

}  // namespace oz3

#endif  // OZ3_CORE_CPU_CORE_H_
