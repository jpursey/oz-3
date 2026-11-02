// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/memory_bank.h"

#include <cstring>
#include <memory>
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
void InitMemory(absl::Span<uint16_t> buffer) {
  for (int i = 0; i < buffer.size(); ++i) {
    buffer[i] = i;
  }
}

// Bitwise-inverted version of GetInitWord.
uint16_t GetAltInitWord(int page, int i) {
  return GetInitWord(page, i) ^ 0xFFFF;
}

// Bitwise-inverted version of InitMemory.
void AltInitMemory(absl::Span<uint16_t> buffer) {
  for (int i = 0; i < buffer.size(); ++i) {
    buffer[i] =
        GetAltInitWord(i / kMemoryBankPageSize, i % kMemoryBankPageSize);
  }
}

// A memory map with `page_count` pages of memory of its own.
class FakeMemoryMap : public MemoryMap {
 public:
  explicit FakeMemoryMap(int page_count = kMemoryBankPageCount)
      : mem_(page_count * kMemoryBankPageSize) {}
  ~FakeMemoryMap() override = default;

  absl::Span<uint16_t> GetMem() { return absl::MakeSpan(mem_); }

 protected:
  void Read(int address, uint16_t* data, int size) override {
    CHECK(address >= 0 && address + size <= mem_.size());
    std::memcpy(data, mem_.data() + address, size * sizeof(uint16_t));
  }

  void Write(int address, const uint16_t* data, int size) override {
    CHECK(address >= 0 && address + size <= mem_.size());
    std::memcpy(mem_.data() + address, data, size * sizeof(uint16_t));
  }

 private:
  std::vector<uint16_t> mem_;
};

TEST(MemoryBankTest, CreateWithDefaultConfig) {
  MemoryBank bank;  // Default options.
  EXPECT_EQ(bank.GetMemoryStart(), 0);
  EXPECT_EQ(bank.GetMemorySize(), 0);
  EXPECT_FALSE(bank.IsLocked());
  EXPECT_EQ(bank.GetMem(0, 1).size(), 0);
}

TEST(MemoryBankTest, CreateWithFullMemory) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  EXPECT_EQ(bank.GetMemoryStart(), 0);
  EXPECT_EQ(bank.GetMemorySize(), kMemoryBankMaxSize);
  EXPECT_EQ(bank.GetMem(0, kMemoryBankMaxSize).size(), kMemoryBankMaxSize);
}

TEST(MemoryBankTest, ConstGetMemMatchesNonConst) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  auto mem = bank.GetMem(1000, 3000);
  const auto& const_mem = const_cast<MemoryBank&>(bank).GetMem(1000, 3000);
  EXPECT_EQ(mem, const_mem);
}

TEST(MemoryBankTest, CreateWithPartialMemory) {
  MemoryBank bank(
      MemoryBankConfig().SetMemPages({.start_page = 2, .page_count = 6}));
  EXPECT_EQ(bank.GetMemoryStart(), 2 * kMemoryBankPageSize);
  EXPECT_EQ(bank.GetMemorySize(), 6 * kMemoryBankPageSize);
  EXPECT_EQ(bank.GetMem(0, 1).size(), 0);
  EXPECT_EQ(
      bank.GetMem(2 * kMemoryBankPageSize, 6 * kMemoryBankPageSize).size(),
      6 * kMemoryBankPageSize);
  EXPECT_EQ(bank.GetMem(8 * kMemoryBankPageSize, 1).size(), 0);
}

TEST(MemoryBankTest, UnintializedMemoryIsZero) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  auto mem = bank.GetMem(0, kMemoryBankMaxSize);
  EXPECT_THAT(mem, Each(0));
}

TEST(MemoryBankTest, LockMemory) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  EXPECT_FALSE(bank.IsLocked());
  {
    auto lock = bank.RequestLock();
    EXPECT_TRUE(bank.IsLocked());
    EXPECT_EQ(bank.GetAddress(*lock), 0);
  }
  EXPECT_FALSE(bank.IsLocked());
}

TEST(MemoryBankTest, SetAddress) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  auto lock = bank.RequestLock();
  EXPECT_EQ(bank.GetAddress(*lock), 0);
  EXPECT_EQ(bank.SetAddress(*lock, 1000), kMemoryBankSetAddressCycles);
  EXPECT_EQ(bank.GetAddress(*lock), 1000);
}

TEST(MemoryBankTest, LoadWordAtAddress) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  bank.GetMem(1000, 1)[0] = 0x5678;
  auto lock = bank.RequestLock();
  bank.SetAddress(*lock, 1000);
  uint16_t value;
  EXPECT_EQ(bank.LoadWord(*lock, value), kMemoryBankAccessWordCycles);
  EXPECT_EQ(value, 0x5678);
}

TEST(MemoryBankTest, StoreWordAtAddress) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  auto lock = bank.RequestLock();
  bank.SetAddress(*lock, 1000);
  EXPECT_EQ(bank.StoreWord(*lock, 0x5678), kMemoryBankAccessWordCycles);
  EXPECT_EQ(bank.GetAddress(*lock), 1001);
  EXPECT_EQ(bank.GetMem(1000, 1)[0], 0x5678);
}

TEST(MemoryBankTest, PushWordAtAddress) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  auto lock = bank.RequestLock();
  bank.SetAddress(*lock, 1000);
  EXPECT_EQ(bank.PushWord(*lock, 0x5678), kMemoryBankAccessWordCycles);
  EXPECT_EQ(bank.GetMem(999, 1)[0], 0x5678);
}

TEST(MemoryBankTest, ReadWordsAtAddress) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  bank.GetMem(1000, 1)[0] = 0x5678;
  bank.GetMem(1001, 1)[0] = 0x1234;
  auto lock = bank.RequestLock();
  bank.SetAddress(*lock, 999);
  uint16_t words[4];
  EXPECT_EQ(bank.ReadWords(*lock, words),
            ABSL_ARRAYSIZE(words) * kMemoryBankAccessWordCycles);
  EXPECT_THAT(words, ElementsAre(0, 0x5678, 0x1234, 0));
}

TEST(MemoryBankTest, WriteWordsAtAddress) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  auto lock = bank.RequestLock();
  bank.SetAddress(*lock, 1000);
  uint16_t words[4] = {0x5678, 0x1234, 0x9ABC, 0xDEF0};
  EXPECT_EQ(bank.WriteWords(*lock, words),
            ABSL_ARRAYSIZE(words) * kMemoryBankAccessWordCycles);
  EXPECT_EQ(bank.GetMem(1000, 1)[0], 0x5678);
  EXPECT_EQ(bank.GetMem(1001, 1)[0], 0x1234);
  EXPECT_EQ(bank.GetMem(1002, 1)[0], 0x9ABC);
  EXPECT_EQ(bank.GetMem(1003, 1)[0], 0xDEF0);
}

TEST(MemoryBankTest, ReadWordFromWriteOnlyMemory) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max(),
                                                 {.read_mask = 0x0FF0}));
  InitMemory(bank.GetMem(0, kMemoryBankMaxSize));
  auto lock = bank.RequestLock();
  int page = 0;
  uint16_t value;
  for (; page < 4; ++page) {
    bank.SetAddress(*lock, page * kMemoryBankPageSize);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, 0) << "Page: " << page;
    bank.SetAddress(*lock, (page + 1) * kMemoryBankPageSize - 1);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, 0) << "Page: " << page;
  }
  for (; page < 12; ++page) {
    bank.SetAddress(*lock, page * kMemoryBankPageSize);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, GetInitWord(page, 0)) << "Page: " << page;
    bank.SetAddress(*lock, (page + 1) * kMemoryBankPageSize - 1);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, GetInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
  }
  for (; page < 16; ++page) {
    bank.SetAddress(*lock, page * kMemoryBankPageSize);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, 0) << "Page: " << page;
    bank.SetAddress(*lock, (page + 1) * kMemoryBankPageSize - 1);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, 0) << "Page: " << page;
  }
}

TEST(MemoryBankTest, WriteWordFromReadOnlyMemory) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(
      MemoryPageRange::Max(), {.read_mask = 0xFFFF, .write_mask = 0x0FF0}));
  InitMemory(bank.GetMem(0, kMemoryBankMaxSize));
  auto lock = bank.RequestLock();
  int page = 0;
  uint16_t value;
  for (; page < 4; ++page) {
    bank.SetAddress(*lock, page * kMemoryBankPageSize);
    bank.StoreWord(*lock, 0x1234);
    bank.SetAddress(*lock, page * kMemoryBankPageSize);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, GetInitWord(page, 0)) << "Page: " << page;
    bank.SetAddress(*lock, (page + 1) * kMemoryBankPageSize - 1);
    bank.StoreWord(*lock, 0x5678);
    bank.SetAddress(*lock, (page + 1) * kMemoryBankPageSize - 1);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, GetInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
  }
  for (; page < 12; ++page) {
    bank.SetAddress(*lock, page * kMemoryBankPageSize);
    bank.StoreWord(*lock, 0x1234);
    bank.SetAddress(*lock, page * kMemoryBankPageSize);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, 0x1234) << "Page: " << page;
    bank.SetAddress(*lock, (page + 1) * kMemoryBankPageSize - 1);
    bank.StoreWord(*lock, 0x5678);
    bank.SetAddress(*lock, (page + 1) * kMemoryBankPageSize - 1);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, 0x5678) << "Page: " << page;
  }
  for (; page < 16; ++page) {
    bank.SetAddress(*lock, page * kMemoryBankPageSize);
    bank.StoreWord(*lock, 0x1234);
    bank.SetAddress(*lock, page * kMemoryBankPageSize);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, GetInitWord(page, 0)) << "Page: " << page;
    bank.SetAddress(*lock, (page + 1) * kMemoryBankPageSize - 1);
    bank.StoreWord(*lock, 0x5678);
    bank.SetAddress(*lock, (page + 1) * kMemoryBankPageSize - 1);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, GetInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
  }
}

TEST(MemoryBankTest, ReadWordsFromWriteOnlyMemory) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max(),
                                                 {.read_mask = 0x0002}));
  InitMemory(bank.GetMem(0, kMemoryBankMaxSize));
  auto lock = bank.RequestLock();
  uint16_t words[4];

  std::memset(words, 0xFF, sizeof(words));
  bank.SetAddress(*lock, kMemoryBankPageSize - 2);
  bank.ReadWords(*lock, words);
  EXPECT_THAT(words, ElementsAre(0, 0, GetInitWord(1, 0), GetInitWord(1, 1)));

  std::memset(words, 0xFF, sizeof(words));
  bank.SetAddress(*lock, kMemoryBankPageSize * 2 - 2);
  bank.ReadWords(*lock, words);
  EXPECT_THAT(words,
              ElementsAre(GetInitWord(1, kMemoryBankPageSize - 2),
                          GetInitWord(1, kMemoryBankPageSize - 1), 0, 0));
}

TEST(MemoryBankTest, WriteWordsOverReadOnlyMemory) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(
      MemoryPageRange::Max(), {.read_mask = 0xFFFF, .write_mask = 0x0002}));
  InitMemory(bank.GetMem(0, kMemoryBankMaxSize));
  auto lock = bank.RequestLock();
  uint16_t words[4] = {0x1234, 0x5678, 0x9ABC, 0xDEF0};

  bank.SetAddress(*lock, kMemoryBankPageSize - 2);
  bank.WriteWords(*lock, words);
  bank.SetAddress(*lock, kMemoryBankPageSize - 2);
  uint16_t value;
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, GetInitWord(0, kMemoryBankPageSize - 2));
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, GetInitWord(0, kMemoryBankPageSize - 1));
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, 0x9ABC);
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, 0xDEF0);

  bank.SetAddress(*lock, kMemoryBankPageSize * 2 - 2);
  bank.WriteWords(*lock, words);
  bank.SetAddress(*lock, kMemoryBankPageSize * 2 - 2);
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, 0x1234);
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, 0x5678);
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, GetInitWord(2, 0));
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, GetInitWord(2, 1));
}

TEST(MemoryBankTest, ReadWordsOverEndOfMemory) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  InitMemory(bank.GetMem(0, kMemoryBankMaxSize));
  auto lock = bank.RequestLock();
  uint16_t words[4];

  std::memset(words, 0xFF, sizeof(words));
  bank.SetAddress(*lock, kMemoryBankMaxSize - 2);
  bank.ReadWords(*lock, words);
  EXPECT_THAT(words, ElementsAre(GetInitWord(15, kMemoryBankPageSize - 2),
                                 GetInitWord(15, kMemoryBankPageSize - 1),
                                 GetInitWord(0, 0), GetInitWord(0, 1)));
}

TEST(MemoryBankTest, WriteWordsOverEndOfMemory) {
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max()));
  auto lock = bank.RequestLock();
  uint16_t words[4] = {0x1234, 0x5678, 0x9ABC, 0xDEF0};

  bank.SetAddress(*lock, kMemoryBankMaxSize - 2);
  bank.WriteWords(*lock, words);
  bank.SetAddress(*lock, kMemoryBankMaxSize - 2);
  uint16_t value;
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, 0x1234);
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, 0x5678);
  EXPECT_EQ(bank.GetAddress(*lock), 0);  // Address should wrap around
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, 0x9ABC);
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, 0xDEF0);
}

TEST(MemoryBankTest, WriteOnlyMasks) {
  const MemoryPageMasks masks = MemoryPageMasks::WriteOnly(0x00F0);
  EXPECT_EQ(masks.read_mask, 0);
  EXPECT_EQ(masks.write_mask, 0x00F0);
}

TEST(MemoryBankTest, MemoryMapMasksPhysicalMemory) {
  FakeMemoryMap memory_map;
  InitMemory(memory_map.GetMem());
  const MemoryMapPages maps[] = {
      {.map = &memory_map, .pages = MemoryPageRange::Max()}};
  MemoryBank bank(MemoryBankConfig::MaxRam(), maps);
  auto lock = bank.RequestLock();
  std::vector<uint16_t> words(kMemoryBankMaxSize);
  absl::Span<uint16_t> mapped_mem = memory_map.GetMem();

  int half_size = kMemoryBankMaxSize / 2;
  std::memset(words.data(), 0xFF, kMemoryBankMaxSize * sizeof(uint16_t));
  bank.SetAddress(*lock, half_size);
  bank.ReadWords(*lock, absl::MakeSpan(words));
  EXPECT_THAT(absl::MakeSpan(words.data(), half_size),
              ElementsAreArray(
                  absl::MakeSpan(mapped_mem.data() + half_size, half_size)));
  EXPECT_THAT(absl::MakeSpan(words.data() + half_size, half_size),
              ElementsAreArray(absl::MakeSpan(mapped_mem.data(), half_size)));

  std::memset(words.data(), 0xFF, kMemoryBankMaxSize * sizeof(uint16_t));
  bank.WriteWords(*lock, words);
  EXPECT_THAT(mapped_mem, Each(0xFFFF));
  EXPECT_THAT(bank.GetMem(0, kMemoryBankMaxSize), Each(0));
}

TEST(MemoryBankTest, MemoryMapMasksAreItsOwn) {
  FakeMemoryMap memory_map(3);
  const MemoryMapPages maps[] = {
      {.map = &memory_map,
       .pages = {.start_page = 2, .page_count = 3},
       .masks = {.read_mask = 0xFFFF, .write_mask = 0x0005}}};
  MemoryBank bank(MemoryBankConfig::MaxRam(), maps);
  EXPECT_EQ(bank.GetMapPageMasks().read_mask, 0x001C);
  EXPECT_EQ(bank.GetMapPageMasks().write_mask, 0x0014);
  EXPECT_EQ(bank.GetMemPageMasks().read_mask, 0xFFE3);
  EXPECT_EQ(bank.GetMemPageMasks().write_mask, 0xFFE3);
}

TEST(MemoryBankTest, ReadWordFromWriteOnlyMappedMemory) {
  FakeMemoryMap memory_map;
  AltInitMemory(memory_map.GetMem());
  const MemoryMapPages maps[] = {
      {.map = &memory_map,
       .pages = MemoryPageRange::Max(),
       .masks = {.read_mask = 0x0FF0, .write_mask = 0xFFFF}}};
  MemoryBank bank(MemoryBankConfig::MaxRam(), maps);
  InitMemory(bank.GetMem(0, kMemoryBankMaxSize));
  auto lock = bank.RequestLock();
  for (int page = 0; page < 16; ++page) {
    const bool readable = (page >= 4 && page < 12);
    uint16_t value;
    bank.SetAddress(*lock, page * kMemoryBankPageSize);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, readable ? GetAltInitWord(page, 0) : 0)
        << "Page: " << page;
    bank.SetAddress(*lock, (page + 1) * kMemoryBankPageSize - 1);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value,
              readable ? GetAltInitWord(page, kMemoryBankPageSize - 1) : 0)
        << "Page: " << page;
  }
}

TEST(MemoryBankTest, WriteWordFromReadOnlyMappedMemory) {
  FakeMemoryMap memory_map;
  AltInitMemory(memory_map.GetMem());
  const MemoryMapPages maps[] = {
      {.map = &memory_map,
       .pages = MemoryPageRange::Max(),
       .masks = {.read_mask = 0xFFFF, .write_mask = 0x0FF0}}};
  MemoryBank bank(MemoryBankConfig::MaxRam(), maps);
  InitMemory(bank.GetMem(0, kMemoryBankMaxSize));
  auto lock = bank.RequestLock();
  for (int page = 0; page < 16; ++page) {
    const bool writable = (page >= 4 && page < 12);
    const int first = page * kMemoryBankPageSize;
    const int last = first + kMemoryBankPageSize - 1;
    uint16_t value;
    bank.SetAddress(*lock, first);
    bank.StoreWord(*lock, 0x1234);
    bank.SetAddress(*lock, first);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value, writable ? 0x1234 : GetAltInitWord(page, 0))
        << "Page: " << page;
    bank.SetAddress(*lock, last);
    bank.StoreWord(*lock, 0x5678);
    bank.SetAddress(*lock, last);
    bank.LoadWord(*lock, value);
    EXPECT_EQ(value,
              writable ? 0x5678 : GetAltInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;

    // Physical memory under the map is never written.
    EXPECT_EQ(bank.GetMem(first, 1)[0], GetInitWord(page, 0))
        << "Page: " << page;
    EXPECT_EQ(bank.GetMem(last, 1)[0],
              GetInitWord(page, kMemoryBankPageSize - 1))
        << "Page: " << page;
  }
}

TEST(MemoryBankTest, MemoryMapsHaveTheirOwnAddresses) {
  FakeMemoryMap map_a(2);
  FakeMemoryMap map_b(4);
  const MemoryMapPages maps[] = {
      {.map = &map_a, .pages = {.start_page = 2, .page_count = 2}},
      {.map = &map_b, .pages = {.start_page = 8, .page_count = 4}}};
  MemoryBank bank(MemoryBankConfig::MaxRam(), maps);
  EXPECT_EQ(bank.GetMapPageMasks().read_mask, 0x0F0C);
  EXPECT_EQ(bank.GetMapPageMasks().write_mask, 0x0F0C);
  auto lock = bank.RequestLock();

  bank.SetAddress(*lock, 2 * kMemoryBankPageSize);
  bank.StoreWord(*lock, 0x1111);
  bank.SetAddress(*lock, 8 * kMemoryBankPageSize + 5);
  bank.StoreWord(*lock, 0x2222);
  bank.SetAddress(*lock, 12 * kMemoryBankPageSize - 1);
  bank.StoreWord(*lock, 0x3333);
  EXPECT_EQ(map_a.GetMem()[0], 0x1111);
  EXPECT_EQ(map_b.GetMem()[5], 0x2222);
  EXPECT_EQ(map_b.GetMem()[4 * kMemoryBankPageSize - 1], 0x3333);
  EXPECT_THAT(bank.GetMem(0, kMemoryBankMaxSize), Each(0));

  map_a.GetMem()[kMemoryBankPageSize] = 0x4444;
  map_b.GetMem()[0] = 0x5555;
  uint16_t value;
  bank.SetAddress(*lock, 3 * kMemoryBankPageSize);
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, 0x4444);
  bank.SetAddress(*lock, 8 * kMemoryBankPageSize);
  bank.LoadWord(*lock, value);
  EXPECT_EQ(value, 0x5555);
}

TEST(MemoryBankTest, ReadAndWriteWordsAcrossMemoryMaps) {
  FakeMemoryMap map_a(2);
  FakeMemoryMap map_b(2);
  const MemoryMapPages maps[] = {
      {.map = &map_a, .pages = {.start_page = 2, .page_count = 2}},
      {.map = &map_b, .pages = {.start_page = 4, .page_count = 2}}};
  MemoryBank bank(MemoryBankConfig::MaxRam(), maps);
  auto lock = bank.RequestLock();
  const uint16_t words[4] = {0x1234, 0x5678, 0x9ABC, 0xDEF0};
  uint16_t read[4];

  // From physical memory into map A.
  bank.SetAddress(*lock, 2 * kMemoryBankPageSize - 2);
  bank.WriteWords(*lock, words);
  EXPECT_THAT(bank.GetMem(2 * kMemoryBankPageSize - 2, 2),
              ElementsAre(0x1234, 0x5678));
  EXPECT_THAT(map_a.GetMem().subspan(0, 2), ElementsAre(0x9ABC, 0xDEF0));
  bank.SetAddress(*lock, 2 * kMemoryBankPageSize - 2);
  bank.ReadWords(*lock, read);
  EXPECT_THAT(read, ElementsAreArray(words));

  // From map A into map B.
  bank.SetAddress(*lock, 4 * kMemoryBankPageSize - 2);
  bank.WriteWords(*lock, words);
  EXPECT_THAT(map_a.GetMem().subspan(2 * kMemoryBankPageSize - 2, 2),
              ElementsAre(0x1234, 0x5678));
  EXPECT_THAT(map_b.GetMem().subspan(0, 2), ElementsAre(0x9ABC, 0xDEF0));
  bank.SetAddress(*lock, 4 * kMemoryBankPageSize - 2);
  bank.ReadWords(*lock, read);
  EXPECT_THAT(read, ElementsAreArray(words));

  // From map B into physical memory.
  bank.SetAddress(*lock, 6 * kMemoryBankPageSize - 2);
  bank.WriteWords(*lock, words);
  EXPECT_THAT(map_b.GetMem().subspan(2 * kMemoryBankPageSize - 2, 2),
              ElementsAre(0x1234, 0x5678));
  EXPECT_THAT(bank.GetMem(6 * kMemoryBankPageSize, 2),
              ElementsAre(0x9ABC, 0xDEF0));
  bank.SetAddress(*lock, 6 * kMemoryBankPageSize - 2);
  bank.ReadWords(*lock, read);
  EXPECT_THAT(read, ElementsAreArray(words));
}

TEST(MemoryBankTest, ReadAndWriteWordsWrapIntoDifferentPermissions) {
  FakeMemoryMap memory_map(1);
  const MemoryMapPages maps[] = {
      {.map = &memory_map, .pages = {.start_page = 15, .page_count = 1}}};
  MemoryBank bank(MemoryBankConfig().SetMemPages(MemoryPageRange::Max(),
                                                 MemoryPageMasks::ReadOnly()),
                  maps);
  InitMemory(bank.GetMem(0, kMemoryBankMaxSize));
  auto lock = bank.RequestLock();
  const uint16_t words[4] = {0x1234, 0x5678, 0x9ABC, 0xDEF0};
  uint16_t read[4];

  // Page 15 is the map's, and writable, but page 0 after the wrap is read-only
  // physical memory.
  bank.SetAddress(*lock, kMemoryBankMaxSize - 2);
  bank.WriteWords(*lock, words);
  EXPECT_THAT(memory_map.GetMem().subspan(kMemoryBankPageSize - 2, 2),
              ElementsAre(0x1234, 0x5678));
  EXPECT_THAT(bank.GetMem(0, 2), ElementsAre(0, 1));
  bank.SetAddress(*lock, kMemoryBankMaxSize - 2);
  bank.ReadWords(*lock, read);
  EXPECT_THAT(read, ElementsAre(0x1234, 0x5678, 0, 1));
}

TEST(MemoryBankTest, MemoryMapRequestLockLocksItsBank) {
  FakeMemoryMap memory_map(1);
  const MemoryMapPages maps[] = {
      {.map = &memory_map, .pages = {.start_page = 0, .page_count = 1}}};
  MemoryBank bank(MemoryBankConfig(), maps);
  MemoryBank other_bank;

  auto lock = memory_map.RequestLock();
  EXPECT_TRUE(lock->IsLocked(bank));
  EXPECT_TRUE(memory_map.IsLocked(*lock));
  auto other_lock = other_bank.RequestLock();
  EXPECT_TRUE(other_lock->IsLocked());
  EXPECT_FALSE(memory_map.IsLocked(*other_lock));

  // While the map's lock is held, the bank's next lock waits for it.
  auto bank_lock = bank.RequestLock();
  EXPECT_FALSE(bank_lock->IsLocked());
  lock.reset();
  EXPECT_TRUE(bank_lock->IsLocked());
  EXPECT_TRUE(memory_map.IsLocked(*bank_lock));

  // And the reverse.
  lock = memory_map.RequestLock();
  EXPECT_FALSE(lock->IsLocked());
  EXPECT_FALSE(memory_map.IsLocked(*lock));
  bank_lock.reset();
  EXPECT_TRUE(lock->IsLocked(bank));
  EXPECT_TRUE(memory_map.IsLocked(*lock));
}

TEST(MemoryBankTest, MemoryMapLeavesItsBankWithIt) {
  FakeMemoryMap memory_map(1);
  const MemoryMapPages maps[] = {
      {.map = &memory_map, .pages = {.start_page = 0, .page_count = 1}}};
  auto bank = std::make_unique<MemoryBank>(MemoryBankConfig(), maps);
  bank.reset();

  // The map can be in a new bank once its first bank is gone.
  MemoryBank new_bank(MemoryBankConfig(), maps);
  auto lock = memory_map.RequestLock();
  EXPECT_TRUE(lock->IsLocked(new_bank));
}

}  // namespace
}  // namespace oz3
