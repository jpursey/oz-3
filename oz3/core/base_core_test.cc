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

  for (const auto& instruction_def : def_.instructions) {
    if (instruction_def.op_name.empty()) {
      continue;
    }
    if (!instruction_def.op_name.empty() &&
        instructions_by_name_.contains(instruction_def.op_name)) {
      LOG(ERROR) << "Duplicate instruction name: " << instruction_def.op_name;
      return false;
    }
    if (instructions_by_op_.contains(instruction_def.op)) {
      LOG(ERROR) << "Duplicate instruction op: " << instruction_def.op;
      return false;
    }
    instructions_by_op_[instruction_def.op] = instruction_def;
    if (!instruction_def.op_name.empty()) {
      instructions_by_name_[instruction_def.op_name] = instruction_def;
    }
  }

  for (const auto& macro_def : def_.macros) {
    if (macro_def.name.empty()) {
      continue;
    }
    if (macros_.contains(macro_def.name)) {
      LOG(ERROR) << "Duplicate macro: " << macro_def.name;
      return false;
    }
    macros_[macro_def.name] = macro_def;
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

bool BaseCoreTest::InitAndReset(InitConfig config) {
  if (!Init(config)) {
    return false;
  }
  int extra_bank_index = config.num_memory_banks - 1;
  int bank_index = 0;
  for (int core_index = 0; core_index < config.num_cores; ++core_index) {
    auto& state = states_[core_index];
    state.ResetCore({.mask = CpuCore::ResetParams::ALL,
                     .mb = CpuCore::Banks{.code = bank_index,
                                          .stack = bank_index,
                                          .data = bank_index,
                                          .extra = extra_bank_index}
                               .ToWord(),
                     .bd = 1000,
                     .be = 2000});
    state.SetRegisters({{CpuCore::ST, CpuCore::I}});
    state.data.SetAddress(1000);
    state.extra.SetAddress(2000);
    bank_index = (bank_index + 1) % config.num_memory_banks;
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
  for (const auto& [index, value] : values) {
    core.SetWordRegister(*lock, index, value);
  }
  lock.reset();
  Update();
}

uint16_t BaseCoreTest::DoEncode(const InstructionDef& instruction, Arg a,
                                Arg b) {
  if (instruction.arg1.type == ArgType::kMacro) {
    CHECK(!a.macro_code.empty())
        << "Argument 'a' does not specify a macro code for macro argument";
    CHECK(!instruction.arg_macro_name.empty())
        << instruction.op_name << "does not specify a macro name";
    auto macro_it = macros_.find(instruction.arg_macro_name);
    CHECK(macro_it != macros_.end())
        << "Unknown macro: " << instruction.arg_macro_name;
    auto& macro = macro_it->second;
    a = macro.Encode(a.macro_code, a.value);
  } else if (!a.macro_code.empty()) {
    LOG(FATAL) << "Argument 'a' specifies a macro code for non-macro argument";
  }
  if (instruction.arg2.type == ArgType::kMacro) {
    CHECK(!b.macro_code.empty())
        << "Argument 'b' does not specify a macro code for macro argument";
    CHECK(!instruction.arg_macro_name.empty())
        << instruction.op_name << "does not specify a macro name";
    auto macro_it = macros_.find(instruction.arg_macro_name);
    CHECK(macro_it != macros_.end())
        << "Unknown macro: " << instruction.arg_macro_name;
    auto& macro = macro_it->second;
    b = macro.Encode(b.macro_code, b.value);
  } else if (!b.macro_code.empty()) {
    LOG(FATAL) << "Argument 'b' specifies a macro code for non-macro argument";
  }
  return instruction.Encode(a.value, b.value);
}

uint16_t BaseCoreTest::Encode(uint8_t op, Arg a, Arg b) {
  auto it = instructions_by_op_.find(op);
  CHECK(it != instructions_by_op_.end())
      << "Unknown op: " << static_cast<int>(op);
  return DoEncode(it->second, a, b);
}

uint16_t BaseCoreTest::Encode(std::string_view op_name, Arg a, Arg b) {
  auto it = instructions_by_name_.find(op_name);
  CHECK(it != instructions_by_name_.end()) << "Unknown op name: " << op_name;
  return DoEncode(it->second, a, b);
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