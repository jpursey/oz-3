// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/cpu_core.h"

#include "absl/cleanup/cleanup.h"
#include "glog/logging.h"
#include "oz3/core/instruction.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/port.h"
#include "oz3/core/processor.h"

namespace oz3 {

namespace {

constexpr std::string_view kWordRegNames[] = {
    "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7",  //
    "BC", "BS", "BD", "BE", "BM", "PC", "BP", "SP",  //
    "C0", "C1", "C2", "ST"};
static_assert(ABSL_ARRAYSIZE(kWordRegNames) == CpuCore::kRegisterCount);
static_assert(kWordRegNames[CpuCore::R0] == "R0");
static_assert(kWordRegNames[CpuCore::R1] == "R1");
static_assert(kWordRegNames[CpuCore::R2] == "R2");
static_assert(kWordRegNames[CpuCore::R3] == "R3");
static_assert(kWordRegNames[CpuCore::R4] == "R4");
static_assert(kWordRegNames[CpuCore::R5] == "R5");
static_assert(kWordRegNames[CpuCore::R6] == "R6");
static_assert(kWordRegNames[CpuCore::R7] == "R7");
static_assert(kWordRegNames[CpuCore::BM] == "BM");
static_assert(kWordRegNames[CpuCore::BC] == "BC");
static_assert(kWordRegNames[CpuCore::BS] == "BS");
static_assert(kWordRegNames[CpuCore::BD] == "BD");
static_assert(kWordRegNames[CpuCore::BE] == "BE");
static_assert(kWordRegNames[CpuCore::PC] == "PC");
static_assert(kWordRegNames[CpuCore::BP] == "BP");
static_assert(kWordRegNames[CpuCore::SP] == "SP");
static_assert(kWordRegNames[CpuCore::C0] == "C0");
static_assert(kWordRegNames[CpuCore::C1] == "C1");
static_assert(kWordRegNames[CpuCore::C2] == "C2");
static_assert(kWordRegNames[CpuCore::ST] == "ST");

constexpr std::string_view kDwordRegNames[CpuCore::kRegisterCount] = {
    "D0", "", "D1", "", "D2", "", "D3", "", "", "", "", "", "", "", "", "",
};
static_assert(ABSL_ARRAYSIZE(kDwordRegNames) == CpuCore::kRegisterCount);
static_assert(kDwordRegNames[CpuCore::D0] == "D0");
static_assert(kDwordRegNames[CpuCore::D1] == "D1");
static_assert(kDwordRegNames[CpuCore::D2] == "D2");
static_assert(kDwordRegNames[CpuCore::D3] == "D3");

constexpr std::string_view kDwordRegNamesCompressed[] = {
    "D0",
    "D1",
    "D2",
    "D3",
};

}  // namespace

absl::Span<const std::string_view> CpuCore::GetWordRegisterNames() {
  return kWordRegNames;
}

absl::Span<const std::string_view> CpuCore::GetDwordRegisterNames() {
  return kDwordRegNamesCompressed;
}

std::string_view CpuCore::GetWordRegName(int reg) {
  if (reg < 0 || reg >= kRegisterCount) {
    return "invalid";
  }
  return kWordRegNames[reg];
}

int CpuCore::GetWordRegFromName(std::string_view name) {
  for (int i = 0; i < kRegisterCount; ++i) {
    if (kWordRegNames[i] == name) {
      return i;
    }
  }
  return -1;
}

std::string_view CpuCore::GetDwordRegName(int reg) {
  if (reg < 0 || reg >= kRegisterCount) {
    return "invalid";
  }
  std::string_view name = kDwordRegNames[reg];
  return name.empty() ? "invalid" : name;
}

int CpuCore::GetDwordRegFromName(std::string_view name) {
  if (name.empty()) {
    return -1;
  }
  for (int i = 0; i < kRegisterCount; ++i) {
    if (kDwordRegNames[i] == name) {
      return i;
    }
  }
  return -1;
}

CpuCore::CpuCore(const CpuCoreConfig& config) {
  instructions_ = config.GetInstructions();
  DCHECK(instructions_ != nullptr);
}

CpuCore::~CpuCore() = default;

void CpuCore::AttachProcessor(CoreInternal, Processor* processor) {
  processor_ = processor;
  InitBanks();
}

void CpuCore::Reset(const Lock& lock, const ResetParams& params) {
  DCHECK(lock.IsLocked(*this));
  DCHECK(processor_ != nullptr);

  std::memset(&r_[R0], 0, sizeof(uint16_t) * 8);
  const uint16_t bank_mask = ((params.mask & ResetParams::MC) ? 0xF : 0) |
                             ((params.mask & ResetParams::MS) ? 0xF0 : 0) |
                             ((params.mask & ResetParams::MD) ? 0xF00 : 0) |
                             ((params.mask & ResetParams::ME) ? 0xF000 : 0);
  r_[BM] = (r_[BM] & ~bank_mask) | (params.bm & bank_mask);
  InitBanks();
  if (params.mask & ResetParams::RBC) {
    r_[BC] = params.bc;
    r_[PC] = 0;
  }
  if (params.mask & ResetParams::RBS) {
    r_[BS] = params.bs;
    r_[BP] = 0;
    r_[SP] = 0;
  }
  if (params.mask & ResetParams::RBD) {
    r_[BD] = params.bd;
  }
  if (params.mask & ResetParams::RBE) {
    r_[BE] = params.be;
  }
  r_[ST] &= ~W;
  state_ = State::kStartInstruction;
}

void CpuCore::SetWordRegister(const Lock& lock, int reg, uint16_t value) {
  DCHECK(lock.IsLocked(*this));
  DCHECK(reg >= 0 && reg < kRegisterCount);
  r_[reg] = value;
}

void CpuCore::SetDwordRegister(const Lock& lock, int reg, uint32_t value) {
  DCHECK(lock.IsLocked(*this));
  DCHECK(IsDwordReg(reg));
  r_[reg] = static_cast<uint16_t>(value & 0xFFFF);
  r_[reg + 1] = static_cast<uint16_t>(value >> 16);
}

void CpuCore::RaiseInterrupt(int interrupt) {
  DCHECK(interrupt >= 0 && interrupt < kInterruptCount);
  it_ |= 1 << interrupt;
}

void CpuCore::InitBanks() {
  const Banks banks = Banks::FromWord(r_[BM]);
  banks_[CODE] = processor_->GetMemory(banks.code);
  banks_[STACK] = processor_->GetMemory(banks.stack);
  banks_[DATA] = processor_->GetMemory(banks.data);
  banks_[EXTRA] = processor_->GetMemory(banks.extra);
}

void CpuCore::GetRegisters(
    gb::Array<uint16_t, kRegisterCount>& registers) const {
  std::memcpy(registers.data(), r_, sizeof(r_));
}

void CpuCore::Execute() {
  exec_cycles_ = 0;
  do {
    if (lock_ != nullptr && !lock_->IsLocked()) {
      exec_cycles_ += 1;
      break;
    }
    switch (state_) {
      case State::kIdle:
        Idle();
        break;
      case State::kWaiting:
        Waiting();
        break;
      case State::kHandleInterrupt:
        HandleInterrupt();
        break;
      case State::kPushInterruptState:
        PushInterruptState();
        break;
      case State::kStartInterrupt:
        StartInterrupt();
        break;
      case State::kReturnFromInterrupt:
        ReturnFromInterrupt();
        break;
      case State::kStartInstruction:
        StartInstruction();
        break;
      case State::kFetchInstruction:
        FetchInstruction();
        break;
      case State::kRunInstruction:
        RunInstruction();
        break;
    }
  } while (exec_cycles_ == 0);
  AdvanceCycles(exec_cycles_);
}

void CpuCore::Idle() {
  if (IsInterruptPending()) {
    state_ = State::kHandleInterrupt;
    return;
  }
  exec_cycles_ += 1;
}

void CpuCore::Waiting() {
  DCHECK(r_[ST] & W);
  if (IsInterruptPending()) {
    state_ = State::kHandleInterrupt;
    return;
  }
  exec_cycles_ += 1;
  const Cycles current_cycles = GetCycles() + exec_cycles_;
  if (current_cycles >= wait_end_) {
    r_[ST] &= ~W;
    r_[wait_reg_] =
        static_cast<uint16_t>((current_cycles - wait_end_) & 0xFFFF);
    state_ = State::kStartInstruction;
  }
}

void CpuCore::HandleInterrupt() {
  if (!PreventLock()) {
    exec_cycles_ += 1;
    return;
  }
  uint32_t mask = 1;
  for (interrupt_ = 0; interrupt_ < kInterruptCount; ++interrupt_, mask <<= 1) {
    if (it_ & mask) {
      it_ -= mask;
      if (ivec_[interrupt_] != 0) {
        break;
      }
    }
  }
  if (interrupt_ == kInterruptCount) {
    interrupt_ = -1;
    state_ = State::kStartInstruction;
    return;
  }
  DCHECK(lock_ == nullptr);
  lock_ = banks_[STACK]->RequestLock();
  locked_bank_ = STACK;
  if (!lock_->IsLocked()) {
    exec_cycles_ += 1;
  }
  state_ = State::kPushInterruptState;
}

void CpuCore::PushInterruptState() {
  DCHECK(lock_ != nullptr && locked_bank_ == STACK);
  DCHECK(interrupt_ >= 0 && interrupt_ < kInterruptCount &&
         ivec_[interrupt_] != 0);
  banks_[STACK]->SetAddress(*lock_, r_[BS] + r_[SP]);
  banks_[STACK]->PushWord(*lock_, r_[PC]);
  banks_[STACK]->PushWord(*lock_, r_[ST]);
  r_[SP] -= 2;
  r_[PC] = ivec_[interrupt_];
  r_[ST] &= 0xFF00;  // Clear status flags including I, but not control flags.
  exec_cycles_ += kCpuCoreStartInterruptCycles;
  state_ = State::kStartInterrupt;
}

void CpuCore::StartInterrupt() {
  DCHECK(lock_ != nullptr && locked_bank_ == STACK);
  lock_ = banks_[CODE]->RequestLock();
  locked_bank_ = CODE;
  if (!lock_->IsLocked()) {
    exec_cycles_ += 1;
  }
  state_ = State::kFetchInstruction;
}

void CpuCore::ReturnFromInterrupt() {
  DCHECK(lock_ != nullptr && locked_bank_ == STACK);
  banks_[STACK]->SetAddress(*lock_, r_[BS] + r_[SP]);
  banks_[STACK]->LoadWord(*lock_, msr_);
  r_[ST] = r_[ST] & 0xFF00 | (msr_ & 0x00FF);
  banks_[STACK]->LoadWord(*lock_, r_[PC]);
  lock_ = nullptr;
  locked_bank_ = -1;
  r_[SP] += 2;
  AllowLock();
  exec_cycles_ += kCpuCoreReturnFromInterruptCycles;
  if ((r_[ST] & W) != 0 && r_[PC] == wait_pc_) {
    state_ = State::kWaiting;
  } else {
    state_ = State::kStartInstruction;
  }
}

void CpuCore::StartInstruction() {
  if (IsInterruptPending()) {
    state_ = State::kHandleInterrupt;
    return;
  }
  if (!PreventLock()) {
    exec_cycles_ += 1;
    return;
  }
  DCHECK(lock_ == nullptr);
  lock_ = banks_[CODE]->RequestLock();
  locked_bank_ = CODE;
  if (!lock_->IsLocked()) {
    exec_cycles_ += 1;
  }
  state_ = State::kFetchInstruction;
}

void CpuCore::FetchInstruction() {
  DCHECK(lock_ != nullptr && locked_bank_ == CODE);
  banks_[CODE]->SetAddress(*lock_, r_[BC] + r_[PC]);
  uint16_t code;
  banks_[CODE]->LoadWord(*lock_, code);
  instructions_->Decode(code, instruction_);
  r_[PC] += instruction_.size;
  fetch_start_ = GetCycles();
  exec_cycles_ += kCpuCoreFetchAndDecodeCycles;
  std::memcpy(&r_[C0], instruction_.c, sizeof(instruction_.c));
  r_[C2] = 0;
  mpc_ = 0;
  mst_ = msr_ = r_[ST];
  mbm_ = r_[BM];
  state_ = State::kRunInstruction;
  RunInstruction();
}

void CpuCore::RunInstruction() {
  RunInstructionLoop();
  if (state_ != State::kRunInstruction) {
    r_[ST] = msr_;
    r_[BM] = mbm_;
    if (state_ != State::kReturnFromInterrupt) {
      AllowLock();
    }
  }
}

#define OZ3_INIT_REG1   \
  const uint16_t reg1 = \
      (code.arg1 < 0 ? instruction_.r[-code.arg1 - 1] : code.arg1)
#define OZ3_INIT_REG2   \
  const uint16_t reg2 = \
      (code.arg2 < 0 ? instruction_.r[-code.arg2 - 1] : code.arg2)
#define OZ3_MATH_OP(dst, src, func) \
  const uint16_t a1 = (dst);        \
  const uint16_t a2 = (src);        \
  const uint16_t r = (func);        \
  const uint16_t rs = (r >> 15);
#define OZ3_Z ((r >> ZShift) == 0)
#define OZ3_S (rs << SShift)
#define OZ3_C ((r < a1) << CShift)
#define OZ3_O ((~((a1 >> 15) ^ (a2 >> 15)) & ((a1 >> 15) ^ rs)) << OShift)
void CpuCore::RunInstructionLoop() {
  while (mpc_ < instruction_.code.size()) {
    const Microcode code = instruction_.code[mpc_++];
    switch (code.op) {
      case kMicro_MSC: {
        mst_ &= ~static_cast<uint16_t>(code.arg1);
        exec_cycles_ += kCpuCoreCycles_MSC;
      } break;
      case kMicro_MSS: {
        mst_ |= static_cast<uint16_t>(code.arg1);
        exec_cycles_ += kCpuCoreCycles_MSS;
      } break;
      case kMicro_MSX: {
        mst_ ^= static_cast<uint16_t>(code.arg1);
        exec_cycles_ += kCpuCoreCycles_MSX;
      } break;
      case kMicro_MSM: {
        OZ3_INIT_REG2;
        const uint16_t mask = static_cast<uint16_t>(code.arg1);
        mst_ = (mst_ & ~mask) | r_[reg2] & mask;
        exec_cycles_ += kCpuCoreCycles_MSM;
      } break;
      case kMicro_MSR: {
        msr_ &= mst_ | ~static_cast<uint16_t>(code.arg1);
        msr_ |= mst_ & static_cast<uint16_t>(code.arg2);
        r_[ST] = msr_;
        exec_cycles_ += kCpuCoreCycles_MSR;
      } break;
      case kMicro_WAIT: {
        // There can only be one WAIT instruction in progress at a time.
        if (mst_ & W) {
          msr_ |= O;
          state_ = State::kStartInstruction;
          return;
        }
        msr_ &= ~O;
        OZ3_INIT_REG1;
        wait_end_ = fetch_start_ + r_[reg1];
        wait_reg_ = reg1;
        const Cycles current_cycles = GetCycles();
        if (wait_end_ <= current_cycles) {
          r_[wait_reg_] =
              static_cast<uint16_t>((current_cycles - wait_end_) & 0xFFFF);
          state_ = State::kStartInstruction;
        } else {
          msr_ |= W;
          state_ = State::kWaiting;
          wait_pc_ = r_[PC];
        }
        return;
      } break;
      case kMicro_HALT: {
        state_ = State::kIdle;
        return;
      } break;
      case kMicro_LK: {
        DCHECK(lock_ == nullptr && locked_bank_ == -1);
        if (exec_cycles_ > 0) {
          --mpc_;
          return;
        }
        locked_bank_ = code.arg1;
        lock_ = banks_[locked_bank_]->RequestLock();
        if (!lock_->IsLocked()) {
          return;
        }
      } break;
      case kMicro_LKR: {
        DCHECK(lock_ == nullptr && locked_bank_ == -1);
        if (exec_cycles_ > 0) {
          --mpc_;
          return;
        }
        OZ3_INIT_REG1;
        locked_bank_ = kRegisterBankMap[reg1];
        lock_ = banks_[locked_bank_]->RequestLock();
        if (!lock_->IsLocked()) {
          return;
        }
      } break;
      case kMicro_UL: {
        DCHECK(lock_ != nullptr && locked_bank_ >= 0 &&
               lock_->IsLocked(*banks_[locked_bank_]));
        if (exec_cycles_ > 0) {
          --mpc_;
          return;
        }
        lock_ = nullptr;
        locked_bank_ = -1;
      } break;
      case kMicro_ADR: {
        DCHECK(lock_ != nullptr && locked_bank_ >= 0 &&
               lock_->IsLocked(*banks_[locked_bank_]));
        OZ3_INIT_REG1;
        banks_[locked_bank_]->SetAddress(*lock_,
                                         r_[BC + locked_bank_] + r_[reg1]);
        exec_cycles_ += kCpuCoreCycles_ADR;
      } break;
      case kMicro_LAD: {
        DCHECK(lock_ != nullptr && locked_bank_ >= 0 &&
               lock_->IsLocked(*banks_[locked_bank_]));
        OZ3_INIT_REG1;
        r_[reg1] =
            banks_[locked_bank_]->GetAddress(*lock_) - r_[BC + locked_bank_];
        exec_cycles_ += kCpuCoreCycles_LAD;
      } break;
      case kMicro_LD: {
        DCHECK(lock_ != nullptr && locked_bank_ >= 0 &&
               lock_->IsLocked(*banks_[locked_bank_]));
        OZ3_INIT_REG1;
        banks_[locked_bank_]->LoadWord(*lock_, r_[reg1]);
        exec_cycles_ += kCpuCoreCycles_LD;
      } break;
      case kMicro_ST: {
        DCHECK(lock_ != nullptr && locked_bank_ >= 0 &&
               lock_->IsLocked(*banks_[locked_bank_]));
        OZ3_INIT_REG1;
        banks_[locked_bank_]->StoreWord(*lock_, r_[reg1]);
        exec_cycles_ += kCpuCoreCycles_ST;
      } break;
      case kMicro_STP: {
        DCHECK(lock_ != nullptr && locked_bank_ >= 0 &&
               lock_->IsLocked(*banks_[locked_bank_]));
        OZ3_INIT_REG1;
        banks_[locked_bank_]->PushWord(*lock_, r_[reg1]);
        exec_cycles_ += kCpuCoreCycles_STP;
      } break;
      case kMicro_MOVI: {
        OZ3_INIT_REG1;
        r_[reg1] = code.arg2;
        exec_cycles_ += kCpuCoreCycles_MOVI;
      } break;
      case kMicro_MOV: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        r_[reg1] = r_[reg2];
        exec_cycles_ += kCpuCoreCycles_MOV;
      } break;
      case kMicro_ADDI: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], code.arg2, a1 + a2);
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | OZ3_C | OZ3_O;
        exec_cycles_ += kCpuCoreCycles_ADDI;
      } break;
      case kMicro_ADD: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], r_[reg2], a1 + a2);
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | OZ3_C | OZ3_O;
        exec_cycles_ += kCpuCoreCycles_ADD;
      } break;
      case kMicro_ADC: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], r_[reg2], a1 + a2 + ((mst_ >> CShift) & 1));
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | OZ3_C | OZ3_O |
               ((r == a1 && a2 != 0) << CShift);
        exec_cycles_ += kCpuCoreCycles_ADC;
      } break;
      case kMicro_SUB: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], -r_[reg2], a1 + a2);
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | OZ3_C | OZ3_O;
        exec_cycles_ += kCpuCoreCycles_SUB;
      } break;
      case kMicro_SBC: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], -r_[reg2], a1 + a2 - ((mst_ >> CShift) & 1));
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | OZ3_C | OZ3_O |
               ((r == a1 && a2 != 0) << CShift);
        exec_cycles_ += kCpuCoreCycles_SBC;
      } break;
      case kMicro_NEG: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(0, r_[reg2], -a2);
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | ((r == 0x8000) << OShift);
        exec_cycles_ += kCpuCoreCycles_NEG;
      } break;
      case kMicro_CMP: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], -r_[reg2], a1 + a2);
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | OZ3_C | OZ3_O;
        exec_cycles_ += kCpuCoreCycles_CMP;
      } break;
      case kMicro_NOT: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(0, r_[reg2], ~a2);
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S;
        exec_cycles_ += kCpuCoreCycles_NOT;
      } break;
      case kMicro_AND: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], r_[reg2], a1 & a2);
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S;
        exec_cycles_ += kCpuCoreCycles_AND;
      } break;
      case kMicro_OR: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], r_[reg2], a1 | a2);
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S;
        exec_cycles_ += kCpuCoreCycles_OR;
      } break;
      case kMicro_XOR: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], r_[reg2], a1 ^ a2);
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S;
        exec_cycles_ += kCpuCoreCycles_XOR;
      } break;
      case kMicro_SL: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, a1 << 1);
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | ((a1 >> 15) << CShift);
        exec_cycles_ += kCpuCoreCycles_SL;
      } break;
      case kMicro_SR: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, a1 >> 1);
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | ((a1 & 1) << CShift);
        exec_cycles_ += kCpuCoreCycles_SR;
      } break;
      case kMicro_SRA: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, (a1 >> 1) | (a1 & 0x8000));
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | ((a1 & 1) << CShift);
        exec_cycles_ += kCpuCoreCycles_SRA;
      } break;
      case kMicro_RL: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, (a1 << 1) | (a1 >> 15));
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | ((a1 >> 15) << CShift);
        exec_cycles_ += kCpuCoreCycles_RL;
      } break;
      case kMicro_RR: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, (a1 >> 1) | (a1 << 15));
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | ((a1 & 1) << CShift);
        exec_cycles_ += kCpuCoreCycles_RR;
      } break;
      case kMicro_RLC: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, (a1 << 1) | ((mst_ >> CShift) & 1));
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | ((a1 >> 15) << CShift);
        exec_cycles_ += kCpuCoreCycles_RLC;
      } break;
      case kMicro_RRC: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, (a1 >> 1) | ((mst_ >> CShift) << 15));
        r_[reg1] = r;
        mst_ = (mst_ & 0xFFF0) | OZ3_Z | OZ3_S | ((a1 & 1) << CShift);
        exec_cycles_ += kCpuCoreCycles_RRC;
      } break;
      case kMicro_JP: {
        mpc_ = std::max(0, mpc_ + code.arg1);
        exec_cycles_ += kCpuCoreCycles_JP;
      } break;
      case kMicro_JC: {
        if (((mst_ >> (code.arg1 & 3)) & 1) == (code.arg1 >> 2)) {
          mpc_ = std::max(0, mpc_ + code.arg2);
          exec_cycles_ += kCpuCoreCycles_JC_True;
        } else {
          exec_cycles_ += kCpuCoreCycles_JC_False;
        }
      } break;
      case kMicro_JD: {
        OZ3_INIT_REG1;
        if (--r_[reg1] != 0) {
          mpc_ = std::max(0, mpc_ + code.arg2);
          exec_cycles_ += kCpuCoreCycles_JD_NonZero;
        } else {
          exec_cycles_ += kCpuCoreCycles_JD_Zero;
        }
      } break;
      case kMicro_INT: {
        OZ3_INIT_REG1;
        static_assert((kInterruptCount & (kInterruptCount - 1)) == 0,
                      "kInterruptCount is not a power of two");
        if (locked_core_ != nullptr) {
          locked_core_->RaiseInterrupt(r_[reg1] & (kInterruptCount - 1));
        }
        exec_cycles_ += kCpuCoreCycles_INT;
      } break;
      case kMicro_ILD: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        if (locked_core_ != nullptr) {
          r_[reg2] = locked_core_->ivec_[r_[reg1]];
        }
        exec_cycles_ += kCpuCoreCycles_ILD;
      } break;
      case kMicro_IST: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        if (locked_core_ != nullptr) {
          locked_core_->ivec_[r_[reg1]] = r_[reg2];
        }
        exec_cycles_ += kCpuCoreCycles_IST;
      } break;
      case kMicro_IRT: {
        DCHECK(lock_ == nullptr && locked_bank_ == -1);
        if (exec_cycles_ > 0) {
          --mpc_;
          return;
        }
        locked_bank_ = STACK;
        lock_ = banks_[locked_bank_]->RequestLock();
        state_ = State::kReturnFromInterrupt;
        return;
      } break;
      case kMicro_PLK: {
        if (exec_cycles_ > 0) {
          --mpc_;
          return;
        }
        DCHECK(lock_ == nullptr && locked_port_ == -1);
        OZ3_INIT_REG1;
        locked_port_ = r_[reg1];
        if (locked_port_ >= processor_->GetNumPorts()) {
          locked_port_ = -1;
          break;
        }
        lock_ = processor_->LockPort(locked_port_);
        if (!lock_->IsLocked()) {
          return;
        }
      } break;
      case kMicro_PUL: {
        if (exec_cycles_ > 0) {
          --mpc_;
          return;
        }
        if (locked_port_ == -1) {
          break;
        }
        DCHECK(lock_ != nullptr);
        lock_ = nullptr;
        locked_port_ = -1;
      } break;
      case kMicro_PLD: {
        exec_cycles_ += kCpuCoreCycles_PLD;
        if (locked_port_ == -1) {
          break;
        }
        DCHECK(lock_ != nullptr && locked_port_ >= 0 &&
               locked_port_ < processor_->GetNumPorts());
        OZ3_INIT_REG2;
        Port& port = processor_->GetPort(locked_port_);
        mst_ = (mst_ & ~S) |
               (port.LoadWord(*lock_, code.arg1, r_[reg2]) << SShift);
      } break;
      case kMicro_PST: {
        exec_cycles_ += kCpuCoreCycles_PST;
        if (locked_port_ == -1) {
          break;
        }
        DCHECK(lock_ != nullptr && locked_port_ >= 0 &&
               locked_port_ < processor_->GetNumPorts());
        OZ3_INIT_REG2;
        Port& port = processor_->GetPort(locked_port_);
        mst_ = (mst_ & ~S) |
               (port.StoreWord(*lock_, code.arg1, r_[reg2]) << SShift);
      } break;
      case kMicro_CLK: {
        if (exec_cycles_ > 0) {
          --mpc_;
          return;
        }
        DCHECK(lock_ == nullptr && locked_core_ == this);
        OZ3_INIT_REG1;
        const int core_index = r_[reg1];
        if (core_index >= processor_->GetNumCores()) {
          locked_core_ = nullptr;
          break;
        }
        locked_core_ = processor_->GetCore(core_index);
        if (locked_core_ != this) {
          lock_ = locked_core_->RequestLock();
          if (!lock_->IsLocked()) {
            return;
          }
        }
      } break;
      case kMicro_CUL: {
        if (exec_cycles_ > 0) {
          --mpc_;
          return;
        }
        DCHECK(lock_ != nullptr || locked_core_ == this ||
               locked_core_ == nullptr);
        DCHECK(locked_bank_ == -1 && locked_port_ == -1);
        lock_ = nullptr;
        locked_core_ = this;
      } break;
      case kMicro_CBK: {
        exec_cycles_ += kCpuCoreCycles_CBK;
        if (locked_core_ == nullptr) {
          break;
        }
        DCHECK(lock_ != nullptr || locked_core_ == this);
        OZ3_INIT_REG2;
        const uint16_t bank_index = r_[reg2];
        if (bank_index >= kMaxMemoryBanks) {
          break;
        }
        locked_core_->banks_[code.arg1] = processor_->GetMemory(bank_index);
        const uint16_t bank_mask = uint16_t(0xF) << (code.arg1 * 4);
        locked_core_->r_[BM] = locked_core_->mbm_ =
            (locked_core_->r_[BM] & ~bank_mask) |
            (bank_index << (code.arg1 * 4));
        if (code.arg1 == CODE) {
          locked_core_->r_[ST] &= ~W;
          if (locked_core_ == this) {
            msr_ &= ~W;
          }
          if (locked_core_->state_ == State::kWaiting) {
            locked_core_->state_ = State::kStartInstruction;
          }
        }
      } break;
      case kMicro_CLD: {
        exec_cycles_ += kCpuCoreCycles_CLD;
        if (locked_core_ == nullptr) {
          break;
        }
        DCHECK(lock_ != nullptr || locked_core_ == this);
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        r_[reg2] = locked_core_->r_[reg1];
      } break;
      case kMicro_CST: {
        exec_cycles_ += kCpuCoreCycles_CST;
        if (locked_core_ == nullptr) {
          break;
        }
        DCHECK(lock_ != nullptr || locked_core_ == this);
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        if (reg1 != BM && reg1 != ST) {
          locked_core_->r_[reg1] = r_[reg2];
        }
      } break;
      case kMicro_END:
        state_ = State::kStartInstruction;
        return;
      default:
        LOG(DFATAL) << "Invalid microcode operation: " << code.op;
    }
  }

  state_ = State::kStartInstruction;
}
#undef OZ3_INIT_REG1
#undef OZ3_INIT_REG2
#undef OZ3_MATH_OP
#undef OZ3_Z
#undef OZ3_S
#undef OZ3_C
#undef OZ3_O

}  // namespace oz3
