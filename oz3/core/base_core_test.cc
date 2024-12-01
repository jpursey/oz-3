// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/base_core_test.h"

#include "oz3/core/default_instruction_set.h"
#include "oz3/core/instruction_compiler.h"

namespace oz3 {

BaseCoreTest::BaseCoreTest()
    : def_(GetDefaultInstructionSetDef()),
      instruction_set_(GetDefaultInstructionSet()) {}

bool BaseCoreTest::Init(InitConfig config) {
  if (instruction_set_ == nullptr) {
    InstructionError error;
    instruction_set_ = CompileInstructionSet(def_, &error);
    if (instruction_set_ == nullptr) {
      LOG(ERROR) << "Failed to compile instruction set: " << error.message;
      return false;
    }
  }

  ProcessorConfig processor_config;
  for (int i = 0; i < config.num_cores; ++i) {
    processor_config.AddCpuCore(
        CpuCoreConfig().SetInstructionSet(instruction_set_));
  }
  for (int i = 0; i < config.num_memory_banks; ++i) {
    processor_config.SetMemoryBank(i, MemoryBankConfig::MaxRam());
  }
  processor_config.SetPortCount(config.num_ports);
  processor_ = std::make_unique<Processor>(processor_config);
  states_.reserve(config.num_cores);
  for (int i = 0; i < config.num_cores; ++i) {
    states_.emplace_back(*processor_->GetCore(i));
  }
  return true;
}

void BaseCoreTest::CoreState::ResetCore(CpuCore::ResetParams reset_params) {
  auto lock = core.RequestLock();
  EXPECT_TRUE(lock->IsLocked());
  core.Reset(*lock, reset_params);
  lock.reset();
  Update();
}

void BaseCoreTest::CoreState::Update() {
  core.GetRegisters(r);
  auto* memory_bank = core.GetMemoryBank(CpuCore::CODE);
  if (memory_bank != code.GetBank()) {
    code.SetBank(memory_bank);
  }
  memory_bank = core.GetMemoryBank(CpuCore::STACK);
  if (memory_bank != stack.GetBank()) {
    stack.SetBank(memory_bank);
  }
  memory_bank = core.GetMemoryBank(CpuCore::DATA);
  if (memory_bank != data.GetBank()) {
    data.SetBank(memory_bank);
  }
  memory_bank = core.GetMemoryBank(CpuCore::EXTRA);
  if (memory_bank != extra.GetBank()) {
    extra.SetBank(memory_bank);
  }
}

void BaseCoreTest::CoreState::SetRegisters(
    absl::Span<const RegisterValue> values) {
  auto lock = core.RequestLock();
  CHECK(lock->IsLocked());
  for (const auto& [index,value] : values) {
    core.SetWordRegister(*lock, index, value);
  }
}

uint16_t BaseCoreTest::Encode(uint8_t op, uint16_t a, uint16_t b) {
  auto it =
      std::find_if(def_.instructions.begin(), def_.instructions.end(),
                   [op](const InstructionDef& def) { return def.op == op; });
  CHECK(it != def_.instructions.end())
      << "Unknown op: " << static_cast<int>(op);
  return it->Encode(a, b);
}

bool BaseCoreTest::ExecuteUntil(int core, gb::Callback<bool()> condition) {
  CHECK(core >= 0 && core < states_.size());
  auto& state = states_[core];
  do {
    processor_->Execute(1);
    if (state.core.GetState() == CpuCore::State::kIdle) {
      return false;
    }
    for (int i = 0; i < states_.size(); ++i) {
      states_[i].Update();
    }
    if (state.ip > 1000) {
      return false;
    }
  } while (!condition());
  return true;
}

}  // namespace oz3