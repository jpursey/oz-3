// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/memory_bank.h"

#include <cstring>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

using ::testing::Each;
using ::testing::ElementsAre;
using ::testing::ElementsAreArray;

// Matches 1,2,3,4,5,...,65536 pattern when referenced by page and index.
uint16_t GetInitWord(int page, int i) {
  return ((page * 0x1000) | (i & 0xFFF));
}

// Fills memory with 1,2,3,4,5,...,65536 pattern.
void InitMemory(absl::Span<uint16_t>& buffer) {
  for (int i = 0; i < buffer.size(); ++i) {
    buffer[i] = i;
  }
}

// Bitwise-inverted version of GetInitWord.
uint16_t GetAltInitWord(int page, int i) {
  return GetInitWord(page, i) ^ 0xFFFF;
}

// Bitwise-inverted version of InitMemory.
void AltInitMemory(absl::Span<uint16_t>& buffer) {
  for (int i = 0; i < buffer.size(); ++i) {
    buffer[i] =
        GetAltInitWord(i / kMemoryBankPageSize, i % kMemoryBankPageSize);
  }
}

class FakeMemoryMap : public MemoryMap {
 public:
  FakeMemoryMap() : mem_(kMemoryBankMaxSize) {}
  ~FakeMemoryMap() override = default;

  absl::Span<uint16_t> GetMem() {
    return absl::MakeSpan(mem_.data(), mem_.size());
  }

  void Read(int address, uint16_t* data, int size) {
    CHECK(address >= 0 && address + size <= mem_.size());
    std::memcpy(data, mem_.data() + address, size * sizeof(uint16_t));
  }

  void Write(int address, const uint16_t* data, int size) {
    CHECK(address >= 0 && address + size <= mem_.size());
    std::memcpy(mem_.data() + address, data, size * sizeof(uint16_t));
  }

 private:
  std::vector<uint16_t> mem_;
};

TEST(MemoryBankTest, CreateWithDefaultOptions) {
  auto bank = MemoryBank::Create(MemoryBank::Options());
  EXPECT_EQ(bank->GetMemoryStart(), 0);
  EXPECT_EQ(bank->GetMemorySize(), 0);
  EXPECT_FALSE(bank->IsLocked());
  EXPECT_EQ(bank->GetRemainingCycles(), 0);
  EXPECT_EQ(bank->GetMem(0, 1).size(), 0);
}

TEST(MemoryBankTest, CreateWithFullMemory) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  EXPECT_EQ(bank->GetMemoryStart(), 0);
  EXPECT_EQ(bank->GetMemorySize(), kMemoryBankMaxSize);
  EXPECT_EQ(bank->GetMem(0, kMemoryBankMaxSize).size(), kMemoryBankMaxSize);
}

TEST(MemoryBankTest, ConstGetMemMatchesNonConst) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  auto mem = bank->GetMem(1000, 3000);
  const auto& const_mem =
      const_cast<MemoryBank*>(bank.get())->GetMem(1000, 3000);
  EXPECT_EQ(mem, const_mem);
}

TEST(MemoryBankTest, CreateWithPartialMemory) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(2, 6));
  EXPECT_EQ(bank->GetMemoryStart(), 2 * kMemoryBankPageSize);
  EXPECT_EQ(bank->GetMemorySize(), 6 * kMemoryBankPageSize);
  EXPECT_EQ(bank->GetMem(0, 1).size(), 0);
  EXPECT_EQ(
      bank->GetMem(2 * kMemoryBankPageSize, 6 * kMemoryBankPageSize).size(),
      6 * kMemoryBankPageSize);
  EXPECT_EQ(bank->GetMem(8 * kMemoryBankPageSize, 1).size(), 0);
}

TEST(MemoryBankTest, UnintializedMemoryIsZero) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  auto mem = bank->GetMem(0, kMemoryBankMaxSize);
  EXPECT_THAT(mem, Each(0));
}

TEST(MemoryBankTest, LockMemory) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  EXPECT_FALSE(bank->IsLocked());
  {
    auto lock = bank->Lock();
    EXPECT_TRUE(bank->IsLocked());
    EXPECT_EQ(lock.GetMemoryBank(), bank.get());
    EXPECT_EQ(lock.GetAddress(), 0);
  }
  EXPECT_FALSE(bank->IsLocked());
}

TEST(MemoryBankTest, SetAddress) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  auto lock = bank->Lock();
  EXPECT_EQ(lock.GetAddress(), 0);
  EXPECT_EQ(bank->GetRemainingCycles(), 0);
  lock.SetAddress(1000);
  EXPECT_EQ(lock.GetAddress(), 1000);
  EXPECT_EQ(bank->GetRemainingCycles(), kMemoryBankSetAddressCycles);
}

TEST(MemoryBankTest, LoadWordAtAddress) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  bank->GetMem(1000, 1)[0] = 0x5678;
  auto lock = bank->Lock();
  lock.SetAddress(1000);
  EXPECT_EQ(lock.LoadWord(), 0x5678);
  EXPECT_EQ(bank->GetRemainingCycles(),
            kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles);
}

TEST(MemoryBankTest, StoreWordAtAddress) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  auto lock = bank->Lock();
  lock.SetAddress(1000);
  lock.StoreWord(0x5678);
  EXPECT_EQ(lock.GetAddress(), 1001);
  EXPECT_EQ(bank->GetMem(1000, 1)[0], 0x5678);
  EXPECT_EQ(bank->GetRemainingCycles(),
            kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles);
}

TEST(MemoryBankTest, PushWordAtAddress) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  auto lock = bank->Lock();
  lock.SetAddress(1000);
  lock.PushWord(0x5678);
  EXPECT_EQ(bank->GetMem(999, 1)[0], 0x5678);
  EXPECT_EQ(bank->GetRemainingCycles(),
            kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles);
}

TEST(MemoryBankTest, ReadWordsAtAddress) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  bank->GetMem(1000, 1)[0] = 0x5678;
  bank->GetMem(1001, 1)[0] = 0x1234;
  auto lock = bank->Lock();
  lock.SetAddress(999);
  uint16_t words[4];
  lock.ReadWords(words, ABSL_ARRAYSIZE(words));
  EXPECT_THAT(words, ElementsAre(0, 0x5678, 0x1234, 0));
  EXPECT_EQ(bank->GetRemainingCycles(),
            kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles * 4);
}

TEST(MemoryBankTest, WriteWordsAtAddress) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  auto lock = bank->Lock();
  lock.SetAddress(1000);
  uint16_t words[4] = {0x5678, 0x1234, 0x9ABC, 0xDEF0};
  lock.WriteWords(words, ABSL_ARRAYSIZE(words));
  EXPECT_EQ(bank->GetMem(1000, 1)[0], 0x5678);
  EXPECT_EQ(bank->GetMem(1001, 1)[0], 0x1234);
  EXPECT_EQ(bank->GetMem(1002, 1)[0], 0x9ABC);
  EXPECT_EQ(bank->GetMem(1003, 1)[0], 0xDEF0);
  EXPECT_EQ(bank->GetRemainingCycles(),
            kMemoryBankSetAddressCycles + kMemoryBankAccessWordCycles * 4);
}

TEST(MemoryBankTest, AdvanceCycles) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  auto lock = bank->Lock();
  uint16_t words[4] = {0x5678, 0x1234, 0x9ABC, 0xDEF0};
  lock.WriteWords(words, ABSL_ARRAYSIZE(words));
  EXPECT_EQ(bank->GetRemainingCycles(), kMemoryBankAccessWordCycles * 4);
  bank->AdvanceCycles(1);
  EXPECT_EQ(bank->GetRemainingCycles(), kMemoryBankAccessWordCycles * 4 - 1);
  bank->AdvanceCycles(kMemoryBankAccessWordCycles * 4 - 1);
  EXPECT_EQ(bank->GetRemainingCycles(), 0);
  bank->AdvanceCycles(1);
  EXPECT_EQ(bank->GetRemainingCycles(), 0);
}

TEST(MemoryBankTest, ReadWordFromWriteOnlyMemory) {
  auto bank = MemoryBank::Create(
      MemoryBank::Options().SetMemPages(0, 16).SetMemReadMask(0x0FF0));
  InitMemory(bank->GetMem(0, kMemoryBankMaxSize));
  auto lock = bank->Lock();
  int page = 0;
  for (; page < 4; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), 0) << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), 0) << "Page: " << page;
  }
  for (; page < 12; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), GetInitWord(page, 0)) << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), GetInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
  }
  for (; page < 16; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), 0) << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), 0) << "Page: " << page;
  }
}

TEST(MemoryBankTest, WriteWordFromReadOnlyMemory) {
  auto bank = MemoryBank::Create(
      MemoryBank::Options().SetMemPages(0, 16).SetMemWriteMask(0x0FF0));
  InitMemory(bank->GetMem(0, kMemoryBankMaxSize));
  auto lock = bank->Lock();
  int page = 0;
  for (; page < 4; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    lock.StoreWord(0x1234);
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), GetInitWord(page, 0)) << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    lock.StoreWord(0x5678);
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), GetInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
  }
  for (; page < 12; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    lock.StoreWord(0x1234);
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), 0x1234) << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    lock.StoreWord(0x5678);
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), 0x5678) << "Page: " << page;
  }
  for (; page < 16; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    lock.StoreWord(0x1234);
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), GetInitWord(page, 0)) << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    lock.StoreWord(0x5678);
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), GetInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
  }
}

TEST(MemoryBankTest, ReadWordsFromWriteOnlyMemory) {
  auto bank = MemoryBank::Create(
      MemoryBank::Options().SetMemPages(0, 16).SetMemReadMask(0x0002));
  InitMemory(bank->GetMem(0, kMemoryBankMaxSize));
  auto lock = bank->Lock();
  uint16_t words[4];

  std::memset(words, 0xFF, sizeof(words));
  lock.SetAddress(kMemoryBankPageSize - 2);
  lock.ReadWords(words, ABSL_ARRAYSIZE(words));
  EXPECT_THAT(words, ElementsAre(0, 0, GetInitWord(1, 0), GetInitWord(1, 1)));

  std::memset(words, 0xFF, sizeof(words));
  lock.SetAddress(kMemoryBankPageSize * 2 - 2);
  lock.ReadWords(words, ABSL_ARRAYSIZE(words));
  EXPECT_THAT(words,
              ElementsAre(GetInitWord(1, kMemoryBankPageSize - 2),
                          GetInitWord(1, kMemoryBankPageSize - 1), 0, 0));
}

TEST(MemoryBankTest, WriteWordsOverReadOnlyMemory) {
  auto bank = MemoryBank::Create(
      MemoryBank::Options().SetMemPages(0, 16).SetMemWriteMask(0x0002));
  InitMemory(bank->GetMem(0, kMemoryBankMaxSize));
  auto lock = bank->Lock();
  uint16_t words[4] = {0x1234, 0x5678, 0x9ABC, 0xDEF0};

  lock.SetAddress(kMemoryBankPageSize - 2);
  lock.WriteWords(words, ABSL_ARRAYSIZE(words));
  lock.SetAddress(kMemoryBankPageSize - 2);
  EXPECT_EQ(lock.LoadWord(), GetInitWord(0, kMemoryBankPageSize - 2));
  EXPECT_EQ(lock.LoadWord(), GetInitWord(0, kMemoryBankPageSize - 1));
  EXPECT_EQ(lock.LoadWord(), 0x9ABC);
  EXPECT_EQ(lock.LoadWord(), 0xDEF0);

  lock.SetAddress(kMemoryBankPageSize * 2 - 2);
  lock.WriteWords(words, ABSL_ARRAYSIZE(words));
  lock.SetAddress(kMemoryBankPageSize * 2 - 2);
  EXPECT_EQ(lock.LoadWord(), 0x1234);
  EXPECT_EQ(lock.LoadWord(), 0x5678);
  EXPECT_EQ(lock.LoadWord(), GetInitWord(2, 0));
  EXPECT_EQ(lock.LoadWord(), GetInitWord(2, 1));
}

TEST(MemoryBankTest, ReadWordsOverEndOfMemory) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  InitMemory(bank->GetMem(0, kMemoryBankMaxSize));
  auto lock = bank->Lock();
  uint16_t words[4];

  std::memset(words, 0xFF, sizeof(words));
  lock.SetAddress(kMemoryBankMaxSize - 2);
  lock.ReadWords(words, ABSL_ARRAYSIZE(words));
  EXPECT_THAT(words, ElementsAre(GetInitWord(15, kMemoryBankPageSize - 2),
                                 GetInitWord(15, kMemoryBankPageSize - 1),
                                 GetInitWord(0, 0), GetInitWord(0, 1)));
}

TEST(MemoryBankTest, WriteWordsOverEndOfMemory) {
  auto bank = MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16));
  auto lock = bank->Lock();
  uint16_t words[4] = {0x1234, 0x5678, 0x9ABC, 0xDEF0};

  lock.SetAddress(kMemoryBankMaxSize - 2);
  lock.WriteWords(words, ABSL_ARRAYSIZE(words));
  lock.SetAddress(kMemoryBankMaxSize - 2);
  EXPECT_EQ(lock.LoadWord(), 0x1234);
  EXPECT_EQ(lock.LoadWord(), 0x5678);
  EXPECT_EQ(lock.GetAddress(), 0);  // Address should wrap around
  EXPECT_EQ(lock.LoadWord(), 0x9ABC);
  EXPECT_EQ(lock.LoadWord(), 0xDEF0);
}

TEST(MemoryBankTest, MemoryMapMasksPhysicalMemory) {
  FakeMemoryMap memory_map;
  InitMemory(memory_map.GetMem());
  auto bank = MemoryBank::Create(
      MemoryBank::Options().SetMemPages(0, 16).SetMemoryMap(&memory_map));
  auto lock = bank->Lock();
  std::vector<uint16_t> words(kMemoryBankMaxSize);
  absl::Span<uint16_t> mapped_mem = memory_map.GetMem();

  int half_size = kMemoryBankMaxSize / 2;
  std::memset(words.data(), 0xFF, kMemoryBankMaxSize * sizeof(uint16_t));
  lock.SetAddress(half_size);
  lock.ReadWords(words.data(), kMemoryBankMaxSize);
  EXPECT_THAT(absl::MakeSpan(words.data(), half_size),
              ElementsAreArray(
                  absl::MakeSpan(mapped_mem.data() + half_size, half_size)));
  EXPECT_THAT(absl::MakeSpan(words.data() + half_size, half_size),
              ElementsAreArray(absl::MakeSpan(mapped_mem.data(), half_size)));

  std::memset(words.data(), 0xFF, kMemoryBankMaxSize * sizeof(uint16_t));
  lock.WriteWords(words.data(), kMemoryBankMaxSize);
  EXPECT_THAT(mapped_mem, Each(0xFFFF));
}

TEST(MemoryBankTest, ReadWordFromWriteOnlyMappedMemory) {
  FakeMemoryMap memory_map;
  AltInitMemory(memory_map.GetMem());
  auto bank =
      MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16).SetMemoryMap(
          &memory_map, /*read_mask=*/0x0FF0, /*write_mask=*/0xFFFF));
  InitMemory(bank->GetMem(0, kMemoryBankMaxSize));
  auto lock = bank->Lock();
  int page = 0;
  for (; page < 4; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), GetInitWord(page, 0)) << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), GetInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
  }
  for (; page < 12; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), GetAltInitWord(page, 0)) << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), GetAltInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
  }
  for (; page < 16; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), GetInitWord(page, 0)) << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), GetInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
  }
}

TEST(MemoryBankTest, WriteWordFromReadOnlyMappedMemory) {
  FakeMemoryMap memory_map;
  AltInitMemory(memory_map.GetMem());
  auto bank =
      MemoryBank::Create(MemoryBank::Options().SetMemPages(0, 16).SetMemoryMap(
          &memory_map, /*read_mask=*/0xFFFF, /*write_mask=*/0x0FF0));
  InitMemory(bank->GetMem(0, kMemoryBankMaxSize));
  auto lock = bank->Lock();
  int page = 0;
  for (; page < 4; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    lock.StoreWord(0x1234);
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), GetAltInitWord(page, 0)) << "Page: " << page;
    EXPECT_EQ(bank->GetMem(page * kMemoryBankPageSize, 1)[0], 0x1234)
        << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    lock.StoreWord(0x5678);
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), GetAltInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
    EXPECT_EQ(bank->GetMem((page + 1) * kMemoryBankPageSize - 1, 1)[0], 0x5678)
        << "Page: " << page;
  }
  for (; page < 12; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    lock.StoreWord(0x1234);
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), 0x1234) << "Page: " << page;
    EXPECT_EQ(bank->GetMem(page * kMemoryBankPageSize, 1)[0],
              GetInitWord(page, 0))
        << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    lock.StoreWord(0x5678);
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), 0x5678) << "Page: " << page;
    EXPECT_EQ(bank->GetMem((page + 1) * kMemoryBankPageSize - 1, 1)[0],
              GetInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
  }
  for (; page < 16; ++page) {
    lock.SetAddress(page * kMemoryBankPageSize);
    lock.StoreWord(0x1234);
    lock.SetAddress(page * kMemoryBankPageSize);
    EXPECT_EQ(lock.LoadWord(), GetAltInitWord(page, 0)) << "Page: " << page;
    EXPECT_EQ(bank->GetMem(page * kMemoryBankPageSize, 1)[0], 0x1234)
        << "Page: " << page;
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    lock.StoreWord(0x5678);
    lock.SetAddress((page + 1) * kMemoryBankPageSize - 1);
    EXPECT_EQ(lock.LoadWord(), GetAltInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
    EXPECT_EQ(bank->GetMem((page + 1) * kMemoryBankPageSize - 1, 1)[0], 0x5678)
        << "Page: " << page;
  }
}

}  // namespace
}  // namespace oz3