// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/cpu_core.h"

#include "absl/cleanup/cleanup.h"
#include "oz3/core/instruction_set.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/processor.h"

namespace oz3 {

CpuCore::CpuCore(const CpuCoreConfig& config)
    : bank_assignment_(config.GetBanks()) {
  micro_codes_.Compile(GetInstructionSet());
}

CpuCore::~CpuCore() = default;

void CpuCore::AttachProcessor(CoreInternal, Processor* processor) {
  processor_ = processor;
  banks_[CODE] = processor_->GetMemory(bank_assignment_.code);
  banks_[STACK] = processor_->GetMemory(bank_assignment_.stack);
  banks_[DATA] = processor_->GetMemory(bank_assignment_.data);
  banks_[EXTRA] = processor_->GetMemory(bank_assignment_.extra);
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
        if (r_[C0]-- == 0) {
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
  state_ = State::kFetchInstruction;
  DCHECK(lock_ == nullptr);
  lock_ = banks_[CODE]->Lock();
  if (!lock_->IsLocked()) {
    exec_cycles_ += 1;
  } else {
    state_ = State::kFetchInstruction;
  }
}

void CpuCore::FetchInstruction() {
  DCHECK(lock_ != nullptr);
  banks_[CODE]->SetAddress(*lock_, r_[PC]);
  uint16_t code;
  banks_[CODE]->LoadWord(*lock_, code);
  micro_codes_.Decode(code, instruction_);
  exec_cycles_ += kCpuCoreFetchAndDecodeCycles;
  std::memcpy(&r_[CD], instruction_.c, sizeof(instruction_.c));
  mc_index_ = 0;
  state_ = State::kRunInstruction;
}

void CpuCore::RunInstruction() {
#define OZ3_INIT_REG1   \
  const uint16_t reg1 = \
      (code.arg1 < 0 ? instruction_.r[-code.arg1 - 1] : code.arg1)
#define OZ3_INIT_REG2   \
  const uint16_t reg2 = \
      (code.arg2 < 0 ? instruction_.r[-code.arg2 - 1] : code.arg2)

  while (mc_index_ < instruction_.code.size()) {
    MicroCode code = instruction_.code[mc_index_++];
    switch (code.op) {
      case MicroOp::LK: {
        DCHECK(lock_ == nullptr);
        if (exec_cycles_ > 0) {
          --mc_index_;
          return;
        }
        lock_ = banks_[code.bank]->Lock();
        if (!lock_->IsLocked()) {
          return;
        }
      } break;
      case MicroOp::UL: {
        DCHECK(lock_ != nullptr && lock_->IsLocked());
        if (exec_cycles_ > 0) {
          --mc_index_;
          return;
        }
        lock_ = nullptr;
      } break;
      case MicroOp::WAIT: {
        OZ3_INIT_REG1;
        r_[C0] = r_[reg1] - kCpuCoreFetchAndDecodeCycles;
        state_ = State::kWaiting;
        return;
      } break;
      case MicroOp::HALT: {
        state_ = State::kIdle;
        return;
      } break;
    }
  }

#undef OZ3_INIT_REG1
#undef OZ3_INIT_REG2

  state_ = State::kStartInstruction;
}

}  // namespace oz3
