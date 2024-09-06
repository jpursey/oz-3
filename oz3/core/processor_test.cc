// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/processor.h"

#include "gtest/gtest.h"
#include "oz3/core/memory_bank.h"

namespace oz3 {
namespace {

TEST(ProcessorTest, CreateDefaultProcessor) {
  ProcessorConfig config;
  Processor processor(config);
  for (int i = 0; i < kMaxMemoryBanks; ++i) {
    MemoryBank& bank = processor.GetMemory(i);
    EXPECT_EQ(bank.GetMemorySize(), 0);
  }
  EXPECT_EQ(processor.GetNumCores(), 0);
}

TEST(ProcessorTest, CreateProcessorWithMemoryBanks) {
  Processor processor(ProcessorConfig().SetMemoryBank(
      0, MemoryBankConfig().SetMemPages(MemoryPageRange::Max())));
  MemoryBank& bank = processor.GetMemory(0);
  EXPECT_EQ(bank.GetMemorySize(), kMemoryBankMaxSize);
}

TEST(ProcessorTest, CreateProcessorWithCpuCores) {
  Processor processor(ProcessorConfig().AddCpuCore(CpuCoreConfig()));
  EXPECT_EQ(processor.GetNumCores(), 1);
}

TEST(ProcessorTest, Execute) {
  ProcessorConfig config;
  Processor processor(config);
  processor.Execute(10);
  EXPECT_EQ(processor.GetCycles(), 10);
}

}  // namespace
}  // namespace oz3
