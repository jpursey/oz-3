// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/cpu_core.h"

#include "absl/cleanup/cleanup.h"
#include "glog/logging.h"
#include "oz3/core/instruction.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/processor.h"

namespace oz3 {

CpuCore::CpuCore(const CpuCoreConfig& config) {
  micro_codes_.Compile(config.GetInstructions());
}

CpuCore::~CpuCore() = default;

void CpuCore::AttachProcessor(CoreInternal, Processor* processor) {
  processor_ = processor;
  InitBanks();
}

void CpuCore::Reset(const ComponentLock& lock, const ResetParams& params) {
  DCHECK(lock.IsLocked(*this));
  DCHECK(processor_ != nullptr);

  std::memset(&r_[R0], 0, sizeof(uint16_t) * 8);
  const uint16_t bank_mask = ((params.mask & ResetParams::BC) ? 0xF : 0) |
                             ((params.mask & ResetParams::BS) ? 0xF0 : 0) |
                             ((params.mask & ResetParams::BD) ? 0xF00 : 0) |
                             ((params.mask & ResetParams::BE) ? 0xF000 : 0);
  r_[BM] = (r_[BM] & ~bank_mask) | (params.bm & bank_mask);
  InitBanks();
  if (params.mask & ResetParams::PC) {
    r_[PC] = params.pc;
  }
  if (params.mask & ResetParams::SP) {
    r_[SP] = params.sp;
  }
  if (params.mask & ResetParams::DP) {
    r_[DP] = params.dp;
  }
  state_ = State::kStartInstruction;
}

void CpuCore::SetWordRegister(const ComponentLock& lock, int reg,
                              uint16_t value) {
  DCHECK(lock.IsLocked(*this));
  DCHECK(reg >= 0 && reg < kRegisterCount);
  r_[reg] = value;
}

void CpuCore::SetDwordRegister(const ComponentLock& lock, int reg,
                               uint32_t value) {
  DCHECK(lock.IsLocked(*this));
  DCHECK(reg == D0 || reg == D1 || reg == D2 || reg == D3 || reg == CD ||
         reg == SD);
  r_[reg] = static_cast<uint16_t>(value & 0xFFFF);
  r_[reg + 1] = static_cast<uint16_t>(value >> 16);
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
        exec_cycles_ += 1;
        break;
      case State::kWaiting:
        exec_cycles_ += 1;
        if (--r_[C0] == 0) {
          state_ = State::kStartInstruction;
        }
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

void CpuCore::StartInstruction() {
  if (!PreventLock()) {
    exec_cycles_ += 1;
    return;
  }
  state_ = State::kFetchInstruction;
  DCHECK(lock_ == nullptr);
  lock_ = banks_[CODE]->Lock();
  locked_bank_ = CODE;
  if (!lock_->IsLocked()) {
    exec_cycles_ += 1;
  }
}

void CpuCore::FetchInstruction() {
  DCHECK(lock_ != nullptr);
  banks_[CODE]->SetAddress(*lock_, r_[PC]);
  uint16_t code;
  banks_[CODE]->LoadWord(*lock_, code);
  micro_codes_.Decode(code, instruction_);
  r_[PC] += instruction_.size;
  exec_cycles_ += kCpuCoreFetchAndDecodeCycles;
  std::memcpy(&r_[CD], instruction_.c, sizeof(instruction_.c));
  mc_index_ = 0;
  mst_ = (r_[ST] & CpuCore::ZSCO);
  state_ = State::kRunInstruction;
  RunInstruction();
}

void CpuCore::RunInstruction() {
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

  while (mc_index_ < instruction_.code.size()) {
    const Microcode code = instruction_.code[mc_index_++];
    switch (code.op) {
      case kMicro_MSTC: {
        mst_ &= ~static_cast<uint16_t>(code.arg1);
      } break;
      case kMicro_MSTS: {
        mst_ |= static_cast<uint16_t>(code.arg1);
      } break;
      case kMicro_MSTX: {
        mst_ ^= static_cast<uint16_t>(code.arg1);
      } break;
      case kMicro_MSTR: {
        r_[ST] &= mst_ | ~static_cast<uint16_t>(code.arg1);
        r_[ST] |= mst_ & static_cast<uint16_t>(code.arg2);
      } break;
      case kMicro_WAIT: {
        OZ3_INIT_REG1;
        if (r_[reg1] <= kCpuCoreFetchAndDecodeCycles) {
          state_ = State::kStartInstruction;
        } else {
          r_[C0] = r_[reg1] - kCpuCoreFetchAndDecodeCycles;
          state_ = State::kWaiting;
        }
        AllowLock();
        return;
      } break;
      case kMicro_HALT: {
        state_ = State::kIdle;
        AllowLock();
        return;
      } break;
      case kMicro_LK: {
        DCHECK(lock_ == nullptr && locked_bank_ == -1);
        if (exec_cycles_ > 0) {
          --mc_index_;
          return;
        }
        locked_bank_ = code.arg1;
        lock_ = banks_[locked_bank_]->Lock();
        if (!lock_->IsLocked()) {
          return;
        }
      } break;
      case kMicro_UL: {
        DCHECK(lock_ != nullptr && locked_bank_ >= 0 &&
               lock_->IsLocked(*banks_[locked_bank_]));
        if (exec_cycles_ > 0) {
          --mc_index_;
          return;
        }
        lock_ = nullptr;
        locked_bank_ = -1;
      } break;
      case kMicro_ADR: {
        DCHECK(lock_ != nullptr && locked_bank_ >= 0 &&
               lock_->IsLocked(*banks_[locked_bank_]));
        OZ3_INIT_REG1;
        banks_[locked_bank_]->SetAddress(*lock_, r_[reg1]);
        exec_cycles_ += kMemoryBankSetAddressCycles;
      } break;
      case kMicro_LD: {
        DCHECK(lock_ != nullptr && locked_bank_ >= 0 &&
               lock_->IsLocked(*banks_[locked_bank_]));
        OZ3_INIT_REG1;
        banks_[locked_bank_]->LoadWord(*lock_, r_[reg1]);
        exec_cycles_ += kMemoryBankAccessWordCycles;
      } break;
      case kMicro_ST: {
        DCHECK(lock_ != nullptr && locked_bank_ >= 0 &&
               lock_->IsLocked(*banks_[locked_bank_]));
        OZ3_INIT_REG1;
        banks_[locked_bank_]->StoreWord(*lock_, r_[reg1]);
        exec_cycles_ += kMemoryBankAccessWordCycles;
      } break;
      case kMicro_STP: {
        DCHECK(lock_ != nullptr && locked_bank_ >= 0 &&
               lock_->IsLocked(*banks_[locked_bank_]));
        OZ3_INIT_REG1;
        banks_[locked_bank_]->PushWord(*lock_, r_[reg1]);
        exec_cycles_ += kMemoryBankAccessWordCycles;
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
        mst_ = OZ3_Z | OZ3_S | OZ3_C | OZ3_O;
        exec_cycles_ += kCpuCoreCycles_ADDI;
      } break;
      case kMicro_ADD: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], r_[reg2], a1 + a2);
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | OZ3_C | OZ3_O;
        exec_cycles_ += kCpuCoreCycles_ADD;
      } break;
      case kMicro_ADC: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], r_[reg2], a1 + a2 + ((mst_ >> CShift) & 1));
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | OZ3_C | OZ3_O | ((r == a1 && a2 != 0) << CShift);
        exec_cycles_ += kCpuCoreCycles_ADC;
      } break;
      case kMicro_SUB: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], -r_[reg2], a1 + a2);
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | OZ3_C | OZ3_O;
        exec_cycles_ += kCpuCoreCycles_SUB;
      } break;
      case kMicro_SBC: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], -r_[reg2], a1 + a2 - ((mst_ >> CShift) & 1));
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | OZ3_C | OZ3_O | ((r == a1 && a2 != 0) << CShift);
        exec_cycles_ += kCpuCoreCycles_SBC;
      } break;
      case kMicro_NEG: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(0, r_[reg2], -a2);
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | ((r == 0x8000) << OShift);
        exec_cycles_ += kCpuCoreCycles_NEG;
      } break;
      case kMicro_CMP: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], -r_[reg2], a1 + a2);
        mst_ = OZ3_Z | OZ3_S | OZ3_C | OZ3_O;
        exec_cycles_ += kCpuCoreCycles_CMP;
      } break;
      case kMicro_NOT: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(0, r_[reg2], ~a2);
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S;
        exec_cycles_ += kCpuCoreCycles_NOT;
      } break;
      case kMicro_AND: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], r_[reg2], a1 & a2);
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S;
        exec_cycles_ += kCpuCoreCycles_AND;
      } break;
      case kMicro_OR: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], r_[reg2], a1 | a2);
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S;
        exec_cycles_ += kCpuCoreCycles_OR;
      } break;
      case kMicro_XOR: {
        OZ3_INIT_REG1;
        OZ3_INIT_REG2;
        OZ3_MATH_OP(r_[reg1], r_[reg2], a1 ^ a2);
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S;
        exec_cycles_ += kCpuCoreCycles_XOR;
      } break;
      case kMicro_SL: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, a1 << 1);
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | ((a1 >> 15) << CShift);
        exec_cycles_ += kCpuCoreCycles_SL;
      } break;
      case kMicro_SR: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, a1 >> 1);
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | ((a1 & 1) << CShift);
        exec_cycles_ += kCpuCoreCycles_SR;
      } break;
      case kMicro_SRA: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, (a1 >> 1) | (a1 & 0x8000));
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | ((a1 & 1) << CShift);
        exec_cycles_ += kCpuCoreCycles_SRA;
      } break;
      case kMicro_RL: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, (a1 << 1) | (a1 >> 15));
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | ((a1 >> 15) << CShift);
        exec_cycles_ += kCpuCoreCycles_RL;
      } break;
      case kMicro_RR: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, (a1 >> 1) | (a1 << 15));
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | ((a1 & 1) << CShift);
        exec_cycles_ += kCpuCoreCycles_RR;
      } break;
      case kMicro_RLC: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, (a1 << 1) | ((mst_ >> CShift) & 1));
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | ((a1 >> 15) << CShift);
        exec_cycles_ += kCpuCoreCycles_RLC;
      } break;
      case kMicro_RRC: {
        OZ3_INIT_REG1;
        OZ3_MATH_OP(r_[reg1], 0, (a1 >> 1) | ((mst_ >> CShift) << 15));
        r_[reg1] = r;
        mst_ = OZ3_Z | OZ3_S | ((a1 & 1) << CShift);
        exec_cycles_ += kCpuCoreCycles_RRC;
      } break;
      default:
        LOG(DFATAL) << "Invalid microcode operation: " << code.op;
    }
  }

  state_ = State::kStartInstruction;
  AllowLock();

#undef OZ3_INIT_REG1
#undef OZ3_INIT_REG2
#undef OZ3_MATH_OP
#undef OZ3_Z
#undef OZ3_S
#undef OZ3_C
#undef OZ3_O
}

}  // namespace oz3
