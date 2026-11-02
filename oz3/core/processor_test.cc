// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor.h"

#include <memory>
#include <string_view>

#include "absl/types/span.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/memory_bank.h"

namespace oz3 {
namespace {

constexpr InstructionDef kNopInstructions[] = {
    {.op = 0, .op_name = "NOP", .code = "UL;"},
};

std::shared_ptr<const InstructionSet> GetNopInstructionSet() {
  static std::shared_ptr<const InstructionSet> s_instruction_set =
      CompileInstructionSet({kNopInstructions});
  return s_instruction_set;
}

TEST(ProcessorTest, CreateDefaultProcessor) {
  ProcessorConfig config;
  Processor processor(config);
  for (int i = 0; i < kMaxMemoryBanks; ++i) {
    EXPECT_EQ(processor.GetMemory(i)->GetMemorySize(), 0);
  }
  EXPECT_EQ(processor.GetNumCores(), 0);
  EXPECT_EQ(processor.GetNumPorts(), 0);
}

TEST(ProcessorTest, CreateProcessorWithMemoryBanks) {
  Processor processor(ProcessorConfig().SetMemoryBank(
      0, MemoryBankConfig().SetMemPages(MemoryPageRange::Max())));
  EXPECT_EQ(processor.GetMemory(0)->GetMemorySize(), kMemoryBankMaxSize);
}

TEST(ProcessorTest, CreateProcessorWithCpuCores) {
  Processor processor(
      ProcessorConfig().AddCpuCore(CpuCoreConfig(GetNopInstructionSet())));
  EXPECT_EQ(processor.GetNumCores(), 1);
}

TEST(ProcessorTest, CreateMultiBankProcessor) {
  Processor processor(ProcessorConfig::MultiBank(2));
  EXPECT_EQ(processor.GetMemory(0)->GetMemorySize(), kMemoryBankMaxSize);
  EXPECT_EQ(processor.GetMemory(1)->GetMemorySize(), kMemoryBankMaxSize);
  EXPECT_EQ(processor.GetMemory(2)->GetMemorySize(), 0);
  EXPECT_EQ(processor.GetNumCores(), 0);
}

TEST(ProcessorTest, ConfigFactoriesUseInstructionSet) {
  const std::shared_ptr<const InstructionSet> instructions =
      GetNopInstructionSet();
  struct Case {
    std::string_view name;
    ProcessorConfig config;
    int num_cores;
  };
  const Case cases[] = {
      {"OneCore", ProcessorConfig::OneCore(instructions), 1},
      {"MultiCore", ProcessorConfig::MultiCore(2, instructions), 2},
      {"MultiBankMultiCore",
       ProcessorConfig::MultiBankMultiCore(2, 3, instructions), 3},
  };
  for (const Case& test_case : cases) {
    absl::Span<const CpuCoreConfig> core_configs =
        test_case.config.GetCpuCoreConfigs();
    EXPECT_EQ(static_cast<int>(core_configs.size()), test_case.num_cores)
        << test_case.name;
    for (const CpuCoreConfig& core_config : core_configs) {
      EXPECT_EQ(core_config.GetInstructions(), instructions) << test_case.name;
    }
  }
}

TEST(ProcessorTest, CreateProcessorWithPorts) {
  Processor processor(ProcessorConfig().SetPortCount(1));
  EXPECT_EQ(processor.GetNumPorts(), 1);
}

TEST(ProcessorTest, Execute) {
  ProcessorConfig config;
  config.AddCpuCore(CpuCoreConfig(GetNopInstructionSet()));
  config.AddCpuCore(CpuCoreConfig(GetNopInstructionSet()));
  Processor processor(config);
  processor.Execute(10);
  EXPECT_EQ(processor.GetCycles(), 10);
  EXPECT_GE(processor.GetCore(0)->GetCycles(), 10);
  EXPECT_GE(processor.GetCore(1)->GetCycles(), 10);
}

TEST(ProcessorTest, RaiseInterrupt) {
  ProcessorConfig config;
  config.AddCpuCore(CpuCoreConfig(GetNopInstructionSet()));
  config.AddCpuCore(CpuCoreConfig(GetNopInstructionSet()));
  Processor processor(config);

  EXPECT_EQ(processor.RaiseInterrupt(1), 0b11);
  uint32_t interrupts = (1 << 1);
  EXPECT_EQ(processor.GetCore(0)->GetInterrupts(), interrupts);
  EXPECT_EQ(processor.GetCore(1)->GetInterrupts(), interrupts);

  EXPECT_EQ(processor.RaiseInterrupt(20), 0b11);
  interrupts |= (1 << 20);
  EXPECT_EQ(processor.GetCore(0)->GetInterrupts(), interrupts);
  EXPECT_EQ(processor.GetCore(1)->GetInterrupts(), interrupts);
}

TEST(ProcessorTest, RaiseInterruptOnSomeCores) {
  Processor processor(ProcessorConfig::MultiCore(3, GetNopInstructionSet()));

  EXPECT_EQ(processor.RaiseInterrupt(1, 0b101), 0b101);
  EXPECT_EQ(processor.GetCore(0)->GetInterrupts(), 1u << 1);
  EXPECT_EQ(processor.GetCore(1)->GetInterrupts(), 0);
  EXPECT_EQ(processor.GetCore(2)->GetInterrupts(), 1u << 1);
}

TEST(ProcessorTest, RaiseInterruptReturnsCoresNewlyRaisedOn) {
  Processor processor(ProcessorConfig::MultiCore(3, GetNopInstructionSet()));

  EXPECT_EQ(processor.RaiseInterrupt(1, 0b001), 0b001);
  EXPECT_EQ(processor.RaiseInterrupt(1), 0b110);
  EXPECT_EQ(processor.RaiseInterrupt(1), 0);
  EXPECT_EQ(processor.RaiseInterrupt(1, 0b010), 0);
  EXPECT_EQ(processor.RaiseInterrupt(2, 0b010), 0b010);
}

TEST(ProcessorTest, RaiseInterruptIgnoresMissingCores) {
  Processor processor(ProcessorConfig::MultiCore(2, GetNopInstructionSet()));

  EXPECT_EQ(processor.RaiseInterrupt(3, 0b1110), 0b0010);
  EXPECT_EQ(processor.GetCore(0)->GetInterrupts(), 0);
  EXPECT_EQ(processor.GetCore(1)->GetInterrupts(), 1u << 3);

  // Only missing cores, or none at all, is always a duplicate.
  EXPECT_EQ(processor.RaiseInterrupt(4, 0b1100), 0);
  EXPECT_EQ(processor.RaiseInterrupt(4, 0), 0);
  EXPECT_EQ(processor.GetCore(0)->GetInterrupts(), 0);
  EXPECT_EQ(processor.GetCore(1)->GetInterrupts(), 1u << 3);
}

}  // namespace
}  // namespace oz3
