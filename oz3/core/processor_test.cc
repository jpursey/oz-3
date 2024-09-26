// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor.h"

#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/memory_bank.h"

namespace oz3 {
namespace {

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
  Processor processor(ProcessorConfig().AddCpuCore(CpuCoreConfig()));
  EXPECT_EQ(processor.GetNumCores(), 1);
}

TEST(ProcessorTest, CreateProcessorWithPorts) {
  Processor processor(ProcessorConfig().SetPortCount(1));
  EXPECT_EQ(processor.GetNumPorts(), 1);
}

TEST(ProcessorTest, Execute) {
  ProcessorConfig config;
  config.AddCpuCore(CpuCoreConfig());
  config.AddCpuCore(CpuCoreConfig());
  Processor processor(config);
  processor.Execute(10);
  EXPECT_EQ(processor.GetCycles(), 10);
  EXPECT_GE(processor.GetCore(0)->GetCycles(), 10);
  EXPECT_GE(processor.GetCore(1)->GetCycles(), 10);
}

TEST(ProcessorTest, RaiseInterrupt) {
  ProcessorConfig config;
  config.AddCpuCore(CpuCoreConfig());
  config.AddCpuCore(CpuCoreConfig());
  Processor processor(config);

  processor.RaiseInterrupt(1);
  uint32_t interrupts = (1 << 1);
  EXPECT_EQ(processor.GetCore(0)->GetInterrupts(), interrupts);
  EXPECT_EQ(processor.GetCore(1)->GetInterrupts(), interrupts);

  processor.RaiseInterrupt(20);
  interrupts |= (1 << 20);
  EXPECT_EQ(processor.GetCore(0)->GetInterrupts(), interrupts);
  EXPECT_EQ(processor.GetCore(1)->GetInterrupts(), interrupts);
}

}  // namespace
}  // namespace oz3
