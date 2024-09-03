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
}

TEST(ProcessorTest, CreateProcessorWithMemoryBanks) {
  Processor processor(ProcessorConfig().SetMemoryBankConfig(
      0, MemoryBankConfig().SetMemPages(MemoryPageRange::Max())));
  MemoryBank& bank = processor.GetMemory(0);
  EXPECT_EQ(bank.GetMemorySize(), kMemoryBankMaxSize);
}

TEST(ProcessorTest, AdvanceCycles) {
  ProcessorConfig config;
  Processor processor(config);
  for (int i = 0; i < kMaxMemoryBanks; ++i) {
    MemoryBank& bank = processor.GetMemory(i);
    auto lock = bank.Lock();
    lock.SetAddress(1000);
    lock.StoreWord(0x1234);
    lock.StoreWord(0x5678);
  }
  const Cycles expected_cycles =
      kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles * 2;
  for (int i = 0; i < kMaxMemoryBanks; ++i) {
    EXPECT_EQ(processor.GetMemory(i).GetRemainingCycles(), expected_cycles);
  }
  processor.AdvanceCycles(2);
  for (int i = 0; i < kMaxMemoryBanks; ++i) {
    EXPECT_EQ(processor.GetMemory(i).GetRemainingCycles(),
              expected_cycles - 2);
  }
  processor.AdvanceCycles(expected_cycles);
  for (int i = 0; i < kMaxMemoryBanks; ++i) {
    EXPECT_EQ(processor.GetMemory(i).GetRemainingCycles(), 0);
  }
}

}  // namespace
}  // namespace oz3
