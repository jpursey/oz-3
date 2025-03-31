// Copyright (c) 2025 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_loader.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/processor.h"
#include "oz3/core/processor_config.h"
#include "oz3/tools/program.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

using PageSpan = ::oz3::ProgramLoader::PageSpan;
using BankPages = ::oz3::ProgramLoader::BankPages;
using LoadedProgram = ::oz3::ProgramLoader::LoadedProgram;

std::vector<BankPages> EmptyFreePages() {
  std::vector<BankPages> free_pages(kMaxMemoryBanks);
  for (int i = 0; i < kMaxMemoryBanks; ++i) {
    free_pages[i].bank = i;
  }
  return free_pages;
}

std::vector<uint16_t> GenerateData(uint16_t start, uint16_t count) {
  std::vector<uint16_t> data(count);
  for (int i = 0; i < count; ++i) {
    data[i] = start + i;
  }
  return data;
}

TEST(ProgramLoaderTest, EmptyProcessor) {
  Processor processor(ProcessorConfig::Empty());
  ProgramLoader loader(&processor);

  auto free_pages = loader.GetFreePages();
  ASSERT_EQ(free_pages.size(), kMaxMemoryBanks);
  for (int b = 0; b < kMaxMemoryBanks; ++b) {
    const auto& pages = free_pages[b];
    EXPECT_EQ(pages.bank, b) << "Bank: " << b;
    EXPECT_THAT(pages.read_only, IsEmpty()) << "Bank: " << b;
    EXPECT_THAT(pages.read_write, IsEmpty()) << "Bank: " << b;
  }
}

TEST(ProgramLoaderTest, FullMemoryProcessor) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(kMaxMemoryBanks, 0));
  ProgramLoader loader(&processor);

  auto free_pages = loader.GetFreePages();
  ASSERT_EQ(free_pages.size(), kMaxMemoryBanks);
  for (int b = 0; b < kMaxMemoryBanks; ++b) {
    const auto& pages = free_pages[b];
    EXPECT_EQ(pages.bank, b) << "Bank: " << b;
    EXPECT_THAT(pages.read_only, IsEmpty()) << "Bank: " << b;
    EXPECT_THAT(pages.read_write, ElementsAre(PageSpan(0, 16)))
        << "Bank: " << b;
  }
}

TEST(ProgramLoaderTest, RomRamMemoryProcessor) {
  static_assert(kMemoryBankPageCount / 3 > 0);
  const int rom_count = kMemoryBankPageCount / 3;
  const int ram_count = rom_count + 1;
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::RomRam(rom_count, ram_count)));
  ProgramLoader loader(&processor);

  auto free_pages = loader.GetFreePages();
  ASSERT_EQ(free_pages.size(), kMaxMemoryBanks);
  EXPECT_THAT(free_pages[0].read_only, ElementsAre(PageSpan(0, rom_count)));
  EXPECT_THAT(free_pages[0].read_write,
              ElementsAre(PageSpan(rom_count, rom_count + ram_count)));
}

TEST(ProgramLoaderTest, WriteOnlyMemoryProcessor) {
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::Empty().SetMemPages(MemoryPageRange::Max(),
                                               MemoryPageMasks::WriteOnly())));
  ProgramLoader loader(&processor);

  auto free_pages = loader.GetFreePages();
  ASSERT_EQ(free_pages.size(), kMaxMemoryBanks);
  EXPECT_THAT(free_pages[0].read_only, IsEmpty());
  EXPECT_THAT(free_pages[0].read_write, IsEmpty());
}

TEST(ProgramLoaderTest, ReadWriteInterlevedMemoryProcessor) {
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::Empty().SetMemPages(
             MemoryPageRange::Max(),
             {.read_mask = 0xFFFF, .write_mask = 0x0F0F})));
  ProgramLoader loader(&processor);

  auto free_pages = loader.GetFreePages();
  ASSERT_EQ(free_pages.size(), kMaxMemoryBanks);
  EXPECT_THAT(free_pages[0].read_only,
              ElementsAre(PageSpan(4, 8), PageSpan(12, 16)));
  EXPECT_THAT(free_pages[0].read_write,
              ElementsAre(PageSpan(0, 4), PageSpan(8, 12)));
}

TEST(ProgramLoaderTest, ReadOnlyMemoryMapProcessor) {
  MemoryMap map;
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::MaxRam().SetMemoryMap(
             &map, {.read_mask = 0x0FF0, .write_mask = 0})));
  ProgramLoader loader(&processor);

  auto free_pages = loader.GetFreePages();
  ASSERT_EQ(free_pages.size(), kMaxMemoryBanks);
  EXPECT_THAT(free_pages[0].read_only, IsEmpty());
  EXPECT_THAT(free_pages[0].read_write,
              ElementsAre(PageSpan(0, 4), PageSpan(12, 16)));
}

TEST(ProgramLoaderTest, WriteOnlyMemoryMapProcessor) {
  MemoryMap map;
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::MaxRam().SetMemoryMap(
             &map, {.read_mask = 0, .write_mask = 0x0FF0})));
  ProgramLoader loader(&processor);

  auto free_pages = loader.GetFreePages();
  ASSERT_EQ(free_pages.size(), kMaxMemoryBanks);
  EXPECT_THAT(free_pages[0].read_only, ElementsAre(PageSpan(4, 12)));
  EXPECT_THAT(free_pages[0].read_write,
              ElementsAre(PageSpan(0, 4), PageSpan(12, 16)));
}

TEST(ProgramLoaderTest, LoadEmptyProgramIntoEmptyProcessor) {
  Processor processor(ProcessorConfig::Empty());
  ProgramLoader loader(&processor);

  Program program = {};
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(loaded_program->pages, IsEmpty());
  EXPECT_THAT(loaded_program->instances, IsEmpty());
}

TEST(ProgramLoaderTest, ProgramHasTooManyBanks) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(kMaxMemoryBanks + 1);
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramHasMaxBanks) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(kMaxMemoryBanks);
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  BankPages expected_pages[kMaxMemoryBanks] = {};
  EXPECT_THAT(loaded_program->pages, ElementsAreArray(expected_pages));
  EXPECT_THAT(loaded_program->instances, IsEmpty());
}

TEST(ProgramLoaderTest, ProgramHasTooManySegments) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(kMaxMemoryBanks, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(kMaxMemoryBanks);
  program.segments.resize(kMaxMemoryBanks * kMemoryBankPageCount + 1);
  for (int i = 0; i < program.segments.size(); ++i) {
    program.segments[i].bank = i % kMaxMemoryBanks;
  }
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramHasMaxSegments) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(kMaxMemoryBanks, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(kMaxMemoryBanks);
  program.segments.resize(kMaxMemoryBanks * kMemoryBankPageCount);
  for (int i = 0; i < program.segments.size(); ++i) {
    program.segments[i].bank = i % kMaxMemoryBanks;
  }
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  BankPages expected_pages[kMaxMemoryBanks] = {};
  for (int i = 0; i < kMaxMemoryBanks; ++i) {
    expected_pages[i].bank = i;
    expected_pages[i].read_write = {PageSpan(0, 16)};
  }
  EXPECT_THAT(loaded_program->pages, ElementsAreArray(expected_pages));
  EXPECT_THAT(loaded_program->instances, IsEmpty());
}

TEST(ProgramLoaderTest, ProgramHasTooManyInstances) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, kMaxCores));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.instances.resize(kMaxCores + 1);
  program.code_segment = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramHasMaxInstances) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, kMaxCores));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.instances.resize(kMaxCores);
  program.code_segment = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 0, .read_write = {PageSpan(0, 1)}}));
  CpuCore::ResetParams expected_instances[kMaxCores];
  for (int i = 0; i < kMaxCores; ++i) {
    expected_instances[i].mask =
        CpuCore::ResetParams::MC | CpuCore::ResetParams::RBC;
    expected_instances[i].mb = CpuCore::Banks::Default().SetCode(0).ToWord();
  }
  EXPECT_THAT(loaded_program->instances, ElementsAreArray(expected_instances));
}

TEST(ProgramLoaderTest, ProgramSegmentHasInvalidBank) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 1;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramDynamicSegmentHasNoPages) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].page_count = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramDynamicSegmentHasTooManyPages) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].page_count = kMemoryBankPageCount + 1;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramDynamicSegmentHasMaxPages) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].page_count = kMemoryBankPageCount;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 0, .read_write = {PageSpan(0, 16)}}));
  EXPECT_THAT(loaded_program->instances, IsEmpty());
}

TEST(ProgramLoaderTest, ProgramFixedSegmentHasNoPages) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].page_start = 0;
  program.segments[0].page_count = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramFixedSegmentHasTooManyPages) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].page_start = kMemoryBankPageCount / 2;
  program.segments[0].page_count =
      kMemoryBankPageCount - program.segments[0].page_start + 1;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramFixedSegmentHasMaxPages) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].page_start = kMemoryBankPageCount / 2;
  program.segments[0].page_count =
      kMemoryBankPageCount - program.segments[0].page_start;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 0, .read_write = {PageSpan(8, 16)}}));
  EXPECT_THAT(loaded_program->instances, IsEmpty());
}

TEST(ProgramLoaderTest, ProgramFixedSegmentsWithOverlappingPages) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(2);
  program.segments[0].bank = 0;
  program.segments[0].page_start = 0;
  program.segments[0].page_count = 8;
  program.segments[1].bank = 0;
  program.segments[1].page_start = 7;
  program.segments[1].page_count = 4;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramFixedSegmentsWithNonOverlappingPages) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(2);
  program.segments[0].bank = 0;
  program.segments[0].page_start = 0;
  program.segments[0].page_count = 8;
  program.segments[1].bank = 0;
  program.segments[1].page_start = 8;
  program.segments[1].page_count = 4;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 0, .read_write = {PageSpan(0, 12)}}));
  EXPECT_THAT(loaded_program->instances, IsEmpty());
}

TEST(ProgramLoaderTest,
     ProgramFixedSegmentsWithOverlappingPagesDifferentBanks) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(2, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(2);
  program.segments.resize(2);
  program.segments[0].bank = 0;
  program.segments[0].page_start = 0;
  program.segments[0].page_count = 8;
  program.segments[1].bank = 1;
  program.segments[1].page_start = 7;
  program.segments[1].page_count = 4;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 0, .read_write = {PageSpan(0, 8)}},
                  BankPages{.bank = 1, .read_write = {PageSpan(7, 11)}}));
  EXPECT_THAT(loaded_program->instances, IsEmpty());
}

TEST(ProgramLoaderTest, ProgramSegmentsExceedMaxPagesForBank) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(2);
  program.segments[0].bank = 0;
  program.segments[0].page_count = 8;
  program.segments[1].bank = 0;
  program.segments[1].page_count = 9;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramSegmentDataExceedsSize) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].page_count = 1;
  program.segments[0].data.resize(2);
  program.segments[0].data[0].offset = 0;
  program.segments[0].data[0].data.resize(kMemoryBankPageSize / 4);
  program.segments[0].data[1].offset = kMemoryBankPageSize / 2;
  program.segments[0].data[1].data.resize(kMemoryBankPageSize / 2 + 1);
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramSegmentDataAtMaxSize) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].page_count = 1;
  program.segments[0].data.resize(2);
  program.segments[0].data[0].offset = 0;
  program.segments[0].data[0].data.resize(kMemoryBankPageSize / 4);
  program.segments[0].data[1].offset = kMemoryBankPageSize / 2;
  program.segments[0].data[1].data.resize(kMemoryBankPageSize / 2);
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 0, .read_write = {PageSpan(0, 1)}}));
  EXPECT_THAT(loaded_program->instances, IsEmpty());
}

TEST(ProgramLoaderTest, ProgramInvalidCodeSegment) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.code_segment = 1;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramInvalidStackSegment) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.stack_segment = 1;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramInvalidDataSegment) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.data_segment = 1;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramInvalidExtraSegment) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.extra_segment = 1;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramInstanceCodeOffsetTooBig) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.instances.resize(2);
  program.instances[0].code = 0;
  program.instances[1].code = kMemoryBankPageSize + 1;
  program.code_segment = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramInstanceCodeOffsetAtEnd) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.instances.resize(2);
  program.instances[0].code = 0;
  program.instances[1].code = kMemoryBankPageSize;
  program.code_segment = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  CpuCore::ResetParams expected_instances[] = {
      {.mask = CpuCore::ResetParams::MC | CpuCore::ResetParams::RBC,
       .mb = CpuCore::Banks::Default().SetCode(0).ToWord(),
       .bc = 0},
      {.mask = CpuCore::ResetParams::MC | CpuCore::ResetParams::RBC,
       .mb = CpuCore::Banks::Default().SetCode(0).ToWord(),
       .bc = kMemoryBankPageSize}};
  EXPECT_THAT(loaded_program->instances, ElementsAreArray(expected_instances));
}

TEST(ProgramLoaderTest, ProgramInstanceStackOffsetTooBig) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.instances.resize(2);
  program.instances[0].stack = 0;
  program.instances[1].stack = kMemoryBankPageSize + 1;
  program.stack_segment = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramInstanceStackOffsetAtEnd) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.instances.resize(2);
  program.instances[0].stack = 0;
  program.instances[1].stack = kMemoryBankPageSize;
  program.stack_segment = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  CpuCore::ResetParams expected_instances[] = {
      {.mask = CpuCore::ResetParams::MS | CpuCore::ResetParams::RBS,
       .mb = CpuCore::Banks::Default().SetStack(0).ToWord(),
       .bs = kMemoryBankPageSize},
      {.mask = CpuCore::ResetParams::MS | CpuCore::ResetParams::RBS,
       .mb = CpuCore::Banks::Default().SetStack(0).ToWord(),
       .bs = 0}};
  EXPECT_THAT(loaded_program->instances, ElementsAreArray(expected_instances));
}

TEST(ProgramLoaderTest, ProgramInstanceDataOffsetTooBig) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.instances.resize(2);
  program.instances[0].data = 0;
  program.instances[1].data = kMemoryBankPageSize + 1;
  program.data_segment = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramInstanceDataOffsetAtEnd) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.instances.resize(2);
  program.instances[0].data = 0;
  program.instances[1].data = kMemoryBankPageSize;
  program.data_segment = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  CpuCore::ResetParams expected_instances[] = {
      {.mask = CpuCore::ResetParams::MD | CpuCore::ResetParams::RBD,
       .mb = CpuCore::Banks::Default().SetData(0).ToWord(),
       .bd = 0},
      {.mask = CpuCore::ResetParams::MD | CpuCore::ResetParams::RBD,
       .mb = CpuCore::Banks::Default().SetData(0).ToWord(),
       .bd = kMemoryBankPageSize}};
  EXPECT_THAT(loaded_program->instances, ElementsAreArray(expected_instances));
}

TEST(ProgramLoaderTest, ProgramInstanceExtraOffsetTooBig) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.instances.resize(2);
  program.instances[0].extra = 0;
  program.instances[1].extra = kMemoryBankPageSize + 1;
  program.extra_segment = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  EXPECT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, ProgramInstanceExtraOffsetAtEnd) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);

  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.instances.resize(2);
  program.instances[0].extra = 0;
  program.instances[1].extra = kMemoryBankPageSize;
  program.extra_segment = 0;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  CpuCore::ResetParams expected_instances[] = {
      {.mask = CpuCore::ResetParams::ME | CpuCore::ResetParams::RBE,
       .mb = CpuCore::Banks::Default().SetExtra(0).ToWord(),
       .be = 0},
      {.mask = CpuCore::ResetParams::ME | CpuCore::ResetParams::RBE,
       .mb = CpuCore::Banks::Default().SetExtra(0).ToWord(),
       .be = kMemoryBankPageSize}};
  EXPECT_THAT(loaded_program->instances, ElementsAreArray(expected_instances));
}

TEST(ProgramLoaderTest, AllocateFailFixedReadOnlyFromSameBank) {
  Processor processor(
      ProcessorConfig::Empty()
          .SetMemoryBank(
              0, MemoryBankConfig::Empty().SetMemPages(
                     MemoryPageRange::Max(), MemoryPageMasks::ReadOnly(0xFFF0)))
          .SetMemoryBank(1, MemoryBankConfig::Empty().SetMemPages(
                                MemoryPageRange::Max(),
                                MemoryPageMasks::ReadOnly(0x0FFF))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(2);
  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadOnly;
  program.segments[0].page_start = 0;
  program.segments[0].page_count = kMemoryBankPageCount / 2;
  program.segments[1].bank = 0;
  program.segments[1].access = Program::Segment::Access::kReadOnly;
  program.segments[1].page_start = kMemoryBankPageCount / 2;
  program.segments[1].page_count = kMemoryBankPageCount / 2;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, AllocateFixedReadOnlyFromDifferentReadOnlyBanks) {
  Processor processor(
      ProcessorConfig::Empty()
          .SetMemoryBank(
              0, MemoryBankConfig::Empty().SetMemPages(
                     MemoryPageRange::Max(), MemoryPageMasks::ReadOnly(0xFFF0)))
          .SetMemoryBank(1, MemoryBankConfig::Empty().SetMemPages(
                                MemoryPageRange::Max(),
                                MemoryPageMasks::ReadOnly(0x0FFF))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(2);
  program.segments.resize(2);
  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadOnly;
  program.segments[0].page_start = 0;
  program.segments[0].page_count = kMemoryBankPageCount / 2;
  program.segments[1].bank = 1;
  program.segments[1].access = Program::Segment::Access::kReadOnly;
  program.segments[1].page_start = kMemoryBankPageCount / 2;
  program.segments[1].page_count = kMemoryBankPageCount / 2;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(
          BankPages{.bank = 1,
                    .read_only = {PageSpan(0, kMemoryBankPageCount / 2)}},
          BankPages{.bank = 0,
                    .read_only = {PageSpan(kMemoryBankPageCount / 2,
                                           kMemoryBankPageCount)}}));
  auto free_pages = EmptyFreePages();
  free_pages[0].read_only = {PageSpan(4, 8)};
  free_pages[1].read_only = {PageSpan(8, 12)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, AllocateFixedReadOnlyFromDifferentReadWriteBanks) {
  Processor processor(
      ProcessorConfig::Empty()
          .SetMemoryBank(0, MemoryBankConfig::Empty().SetMemPages(
                                MemoryPageRange::Max(),
                                MemoryPageMasks::ReadWrite(0xFFF0)))
          .SetMemoryBank(1, MemoryBankConfig::Empty().SetMemPages(
                                MemoryPageRange::Max(),
                                MemoryPageMasks::ReadWrite(0x0FFF))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(2);
  program.segments.resize(2);
  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadOnly;
  program.segments[0].page_start = 0;
  program.segments[0].page_count = kMemoryBankPageCount / 2;
  program.segments[1].bank = 1;
  program.segments[1].access = Program::Segment::Access::kReadOnly;
  program.segments[1].page_start = kMemoryBankPageCount / 2;
  program.segments[1].page_count = kMemoryBankPageCount / 2;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(
          BankPages{.bank = 1,
                    .read_write = {PageSpan(0, kMemoryBankPageCount / 2)}},
          BankPages{.bank = 0,
                    .read_write = {PageSpan(kMemoryBankPageCount / 2,
                                            kMemoryBankPageCount)}}));
  auto free_pages = EmptyFreePages();
  free_pages[0].read_write = {PageSpan(4, 8)};
  free_pages[1].read_write = {PageSpan(8, 12)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, AllocateFailFixedReadWriteFromSameBank) {
  Processor processor(
      ProcessorConfig::Empty()
          .SetMemoryBank(0, MemoryBankConfig::Empty().SetMemPages(
                                MemoryPageRange::Max(),
                                MemoryPageMasks::ReadWrite(0xFFF0)))
          .SetMemoryBank(1, MemoryBankConfig::Empty().SetMemPages(
                                MemoryPageRange::Max(),
                                MemoryPageMasks::ReadWrite(0x0FFF))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(2);
  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadWrite;
  program.segments[0].page_start = 0;
  program.segments[0].page_count = kMemoryBankPageCount / 2;
  program.segments[1].bank = 0;
  program.segments[1].access = Program::Segment::Access::kReadWrite;
  program.segments[1].page_start = kMemoryBankPageCount / 2;
  program.segments[1].page_count = kMemoryBankPageCount / 2;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, AllocateFailFixedReadWriteFromReadOnlyBanks) {
  Processor processor(
      ProcessorConfig::Empty()
          .SetMemoryBank(
              0, MemoryBankConfig::Empty().SetMemPages(
                     MemoryPageRange::Max(), MemoryPageMasks::ReadOnly(0xFFF0)))
          .SetMemoryBank(1, MemoryBankConfig::Empty().SetMemPages(
                                MemoryPageRange::Max(),
                                MemoryPageMasks::ReadOnly(0x0FFF))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(2);
  program.segments.resize(2);
  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadWrite;
  program.segments[0].page_start = 0;
  program.segments[0].page_count = kMemoryBankPageCount / 2;
  program.segments[1].bank = 1;
  program.segments[1].access = Program::Segment::Access::kReadWrite;
  program.segments[1].page_start = kMemoryBankPageCount / 2;
  program.segments[1].page_count = kMemoryBankPageCount / 2;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, AllocateFixedReadWriteFromDifferentReadWriteBanks) {
  Processor processor(
      ProcessorConfig::Empty()
          .SetMemoryBank(0, MemoryBankConfig::Empty().SetMemPages(
                                MemoryPageRange::Max(),
                                MemoryPageMasks::ReadWrite(0xFFF0)))
          .SetMemoryBank(1, MemoryBankConfig::Empty().SetMemPages(
                                MemoryPageRange::Max(),
                                MemoryPageMasks::ReadWrite(0x0FFF))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(2);
  program.segments.resize(2);
  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadWrite;
  program.segments[0].page_start = 0;
  program.segments[0].page_count = kMemoryBankPageCount / 2;
  program.segments[1].bank = 1;
  program.segments[1].access = Program::Segment::Access::kReadWrite;
  program.segments[1].page_start = kMemoryBankPageCount / 2;
  program.segments[1].page_count = kMemoryBankPageCount / 2;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(
          BankPages{.bank = 1,
                    .read_write = {PageSpan(0, kMemoryBankPageCount / 2)}},
          BankPages{.bank = 0,
                    .read_write = {PageSpan(kMemoryBankPageCount / 2,
                                            kMemoryBankPageCount)}}));
  auto free_pages = EmptyFreePages();
  free_pages[0].read_write = {PageSpan(4, 8)};
  free_pages[1].read_write = {PageSpan(8, 12)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, AllocateDynamicReadOnlyFailOnSize) {
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::Empty().SetMemPages(
             MemoryPageRange::Max(), MemoryPageMasks::ReadOnly(0xF371))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(3);
  for (int i = 0; i < program.segments.size(); ++i) {
    program.segments[i].bank = 0;
    program.segments[i].access = Program::Segment::Access::kReadOnly;
    program.segments[i].page_count = 3;
  }
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, AllocateDynamicReadOnlyInOneReadOnlyBank) {
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::Empty().SetMemPages(
             MemoryPageRange::Max(), MemoryPageMasks::ReadOnly(0xF371))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(2);
  for (int i = 0; i < program.segments.size(); ++i) {
    program.segments[i].bank = 0;
    program.segments[i].access = Program::Segment::Access::kReadOnly;
    program.segments[i].page_count = 3;
  }
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(loaded_program->pages,
              ElementsAre(BankPages{
                  .bank = 0, .read_only = {PageSpan(4, 7), PageSpan(12, 15)}}));
  auto free_pages = EmptyFreePages();
  free_pages[0].read_only = {PageSpan(0, 1), PageSpan(8, 10), PageSpan(15, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, AllocateDynamicReadOnlyInOneReadWriteBank) {
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::Empty().SetMemPages(
             MemoryPageRange::Max(), MemoryPageMasks::ReadWrite(0xF371))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(2);
  for (int i = 0; i < program.segments.size(); ++i) {
    program.segments[i].bank = 0;
    program.segments[i].access = Program::Segment::Access::kReadOnly;
    program.segments[i].page_count = 3;
  }
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 0,
                            .read_write = {PageSpan(4, 7), PageSpan(12, 15)}}));
  auto free_pages = EmptyFreePages();
  free_pages[0].read_write = {PageSpan(0, 1), PageSpan(8, 10),
                              PageSpan(15, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, AllocateDynamicReadWriteFailOnSize) {
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::Empty().SetMemPages(
             MemoryPageRange::Max(), MemoryPageMasks::ReadWrite(0xF371))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(3);
  for (int i = 0; i < program.segments.size(); ++i) {
    program.segments[i].bank = 0;
    program.segments[i].access = Program::Segment::Access::kReadWrite;
    program.segments[i].page_count = 3;
  }
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, AllocateDynamicReadWriteInOneReadOnlyBank) {
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::Empty().SetMemPages(
             MemoryPageRange::Max(), MemoryPageMasks::ReadOnly(0xF371))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(2);
  for (int i = 0; i < program.segments.size(); ++i) {
    program.segments[i].bank = 0;
    program.segments[i].access = Program::Segment::Access::kReadWrite;
    program.segments[i].page_count = 3;
  }
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, AllocateDynamicReadWriteInOneReadWriteBank) {
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::Empty().SetMemPages(
             MemoryPageRange::Max(), MemoryPageMasks::ReadWrite(0xF371))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(2);
  for (int i = 0; i < program.segments.size(); ++i) {
    program.segments[i].bank = 0;
    program.segments[i].access = Program::Segment::Access::kReadWrite;
    program.segments[i].page_count = 3;
  }
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 0,
                            .read_write = {PageSpan(4, 7), PageSpan(12, 15)}}));
  auto free_pages = EmptyFreePages();
  free_pages[0].read_write = {PageSpan(0, 1), PageSpan(8, 10),
                              PageSpan(15, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, AllocateFixedExactSize) {
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::Empty().SetMemPages(
             MemoryPageRange::Max(), MemoryPageMasks::ReadWrite(0xF0F0))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].page_start = 4;
  program.segments[0].page_count = 4;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 0, .read_write = {PageSpan(4, 8)}}));
  auto free_pages = EmptyFreePages();
  free_pages[0].read_write = {PageSpan(12, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, AllocateDynamicExactSize) {
  Processor processor(ProcessorConfig::Empty().SetMemoryBank(
      0, MemoryBankConfig::Empty().SetMemPages(
             MemoryPageRange::Max(), MemoryPageMasks::ReadWrite(0xF0F0))));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].page_count = 4;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 0, .read_write = {PageSpan(4, 8)}}));
  auto free_pages = EmptyFreePages();
  free_pages[0].read_write = {PageSpan(12, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, AllocateSegmentsAcrossMultipleBanks) {
  Processor processor(
      ProcessorConfig::Empty()
          .SetMemoryBank(0, MemoryBankConfig::Empty().SetMemPages(
                                MemoryPageRange::Max(),
                                {.read_mask = 0xFFFF, .write_mask = 0xFF00}))
          .SetMemoryBank(1, MemoryBankConfig::Empty().SetMemPages(
                                MemoryPageRange::Max(),
                                {.read_mask = 0xFFFF, .write_mask = 0x00FF})));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(4);
  program.segments.resize(4);
  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadWrite;
  program.segments[0].page_count = 5;
  program.segments[1].bank = 1;
  program.segments[1].access = Program::Segment::Access::kReadWrite;
  program.segments[1].page_count = 5;
  program.segments[2].bank = 2;
  program.segments[2].access = Program::Segment::Access::kReadOnly;
  program.segments[2].page_count = 5;
  program.segments[3].bank = 3;
  program.segments[3].access = Program::Segment::Access::kReadOnly;
  program.segments[3].page_count = 5;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 0, .read_write = {PageSpan(8, 13)}},
                  BankPages{.bank = 1, .read_write = {PageSpan(0, 5)}},
                  BankPages{.bank = 0, .read_only = {PageSpan(0, 5)}},
                  BankPages{.bank = 1, .read_only = {PageSpan(8, 13)}}));
  auto free_pages = EmptyFreePages();
  free_pages[0].read_write = {PageSpan(13, 16)};
  free_pages[0].read_only = {PageSpan(5, 8)};
  free_pages[1].read_write = {PageSpan(5, 8)};
  free_pages[1].read_only = {PageSpan(13, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, FailWithProgramBankMask) {
  Processor processor(ProcessorConfig::Empty()
                          .SetMemoryBank(0, MemoryBankConfig::MaxRam())
                          .SetMemoryBank(1, MemoryBankConfig::RomRam(0, 4))
                          .SetMemoryBank(2, MemoryBankConfig::RomRam(5, 4))
                          .SetMemoryBank(3, MemoryBankConfig::MaxRam()));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.banks[0].assign_mask = 0b0110;
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadWrite;
  program.segments[0].page_count = 5;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_EQ(handle, ProgramLoader::kInvalidHandle);
}

TEST(ProgramLoaderTest, SucceedWithProgramBankMask) {
  Processor processor(ProcessorConfig::Empty()
                          .SetMemoryBank(0, MemoryBankConfig::MaxRam())
                          .SetMemoryBank(1, MemoryBankConfig::RomRam(0, 4))
                          .SetMemoryBank(2, MemoryBankConfig::RomRam(4, 5))
                          .SetMemoryBank(3, MemoryBankConfig::MaxRam()));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.banks[0].assign_mask = 0b0110;
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadWrite;
  program.segments[0].page_count = 5;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  auto* loaded_program = loader.GetProgram(handle);
  ASSERT_NE(loaded_program, nullptr);
  EXPECT_THAT(
      loaded_program->pages,
      ElementsAre(BankPages{.bank = 2, .read_write = {PageSpan(4, 9)}}));
  auto free_pages = EmptyFreePages();
  free_pages[0].read_write = {PageSpan(0, 16)};
  free_pages[1].read_write = {PageSpan(0, 4)};
  free_pages[2].read_only = {PageSpan(0, 4)};
  free_pages[3].read_write = {PageSpan(0, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, SegmentsWithInitialData) {
  Processor processor(ProcessorConfig::Empty()
                          .SetMemoryBank(0, MemoryBankConfig::MaxRam())
                          .SetMemoryBank(1, MemoryBankConfig::RomRam(4)));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(2);
  program.banks[0].assign_mask = 1 << 0;
  program.banks[1].assign_mask = 1 << 1;
  program.segments.resize(4);

  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadWrite;
  program.segments[0].page_count = 8;
  program.segments[0].data = {
      Program::Segment::Data{.data = GenerateData(1, 16)}};

  program.segments[1].bank = 0;
  program.segments[1].access = Program::Segment::Access::kReadWrite;
  program.segments[1].page_count = 4;
  program.segments[1].data = {
      Program::Segment::Data{.offset = 16, .data = GenerateData(17, 32)},
      Program::Segment::Data{
          .offset = kMemoryBankPageSize,
          .data = GenerateData(kMemoryBankPageSize, kMemoryBankPageSize * 2)}};

  program.segments[2].bank = 1;
  program.segments[2].access = Program::Segment::Access::kReadWrite;
  program.segments[2].page_count = 4;
  program.segments[2].data = {
      Program::Segment::Data{.data = GenerateData(33, 64)},
      Program::Segment::Data{.offset = 1000, .data = GenerateData(1000, 2000)}};

  program.segments[3].bank = 1;
  program.segments[3].access = Program::Segment::Access::kReadOnly;
  program.segments[3].page_start = 1;
  program.segments[3].page_count = 2;
  program.segments[3].data = {
      Program::Segment::Data{.data = GenerateData(65, 128)},
      Program::Segment::Data{.offset = 256, .data = GenerateData(129, 256)}};

  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  EXPECT_EQ(processor.GetMemory(0)->GetMem(0, 16), GenerateData(1, 16));
  EXPECT_EQ(processor.GetMemory(0)->GetMem(8 * kMemoryBankPageSize + 16, 32),
            GenerateData(17, 32));
  EXPECT_EQ(processor.GetMemory(0)->GetMem(9 * kMemoryBankPageSize,
                                           kMemoryBankPageSize * 2),
            GenerateData(kMemoryBankPageSize, kMemoryBankPageSize * 2));
  EXPECT_EQ(processor.GetMemory(1)->GetMem(4 * kMemoryBankPageSize, 64),
            GenerateData(33, 64));
  EXPECT_EQ(
      processor.GetMemory(1)->GetMem(4 * kMemoryBankPageSize + 1000, 2000),
      GenerateData(1000, 2000));
  EXPECT_EQ(processor.GetMemory(1)->GetMem(kMemoryBankPageSize, 128),
            GenerateData(65, 128));
  EXPECT_EQ(processor.GetMemory(1)->GetMem(kMemoryBankPageSize + 256, 256),
            GenerateData(129, 256));
}

TEST(ProgramLoaderTest, UnloadInvalidProgram) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].page_start = 4;
  program.segments[0].page_count = 4;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  auto free_pages = EmptyFreePages();
  free_pages[0].read_write = {PageSpan(0, 4), PageSpan(8, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));

  loader.Unload(ProgramLoader::kInvalidHandle);
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, UnloadProgramInvalidatesHandle) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);
  ProgramLoader::Handle handle = loader.Load({});
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  ASSERT_NE(loader.GetProgram(handle), nullptr);
  loader.Unload(handle);
  ASSERT_EQ(loader.GetProgram(handle), nullptr);
}

TEST(ProgramLoaderTest, UnloadProgramWithReadWritePages) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadWrite;
  program.segments[0].page_start = 4;
  program.segments[0].page_count = 4;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  auto free_pages = EmptyFreePages();
  free_pages[0].read_write = {PageSpan(0, 4), PageSpan(8, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));

  loader.Unload(handle);
  free_pages[0].read_write = {PageSpan(0, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, UnloadProgramWithReadOnlyPages) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].access = Program::Segment::Access::kReadOnly;
  program.segments[0].page_start = 4;
  program.segments[0].page_count = 4;
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  auto free_pages = EmptyFreePages();
  free_pages[0].read_write = {PageSpan(0, 4), PageSpan(8, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));

  loader.Unload(handle);
  free_pages[0].read_write = {PageSpan(0, 16)};
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, ReloadProgram) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks.resize(1);
  program.segments.resize(1);
  program.segments[0].bank = 0;
  program.segments[0].page_count = kMemoryBankPageCount;
  ProgramLoader::Handle handle_1 = loader.Load(program);
  ASSERT_NE(handle_1, ProgramLoader::kInvalidHandle);
  auto free_pages = EmptyFreePages();
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));

  loader.Unload(handle_1);
  ProgramLoader::Handle handle_2 = loader.Load(program);
  EXPECT_NE(handle_2, handle_1);
  ASSERT_NE(handle_2, ProgramLoader::kInvalidHandle);
  ASSERT_EQ(loader.Load(program), ProgramLoader::kInvalidHandle);
  loader.Unload(handle_2);
  ProgramLoader::Handle handle_3 = loader.Load(program);
  EXPECT_NE(handle_3, handle_1);
  EXPECT_NE(handle_3, handle_2);
  ASSERT_NE(handle_3, ProgramLoader::kInvalidHandle);
  EXPECT_THAT(loader.GetFreePages(), ElementsAreArray(free_pages));
}

TEST(ProgramLoaderTest, GetResetParamsForInvalidHandle) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks = {Program::Bank{}};
  program.segments = {
      Program::Segment{.bank = 0, .page_start = 0, .page_count = 1}};
  program.code_segment = 0;
  program.instances = {Program::Instance{}};
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  EXPECT_EQ(loader.GetResetParams(ProgramLoader::kInvalidHandle),
            CpuCore::ResetParams{});
}

TEST(ProgramLoaderTest, GetResetParamsForInvalidInstance) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks = {Program::Bank{}};
  program.segments = {
      Program::Segment{.bank = 0, .page_start = 0, .page_count = 1}};
  program.code_segment = 0;
  program.instances = {Program::Instance{}};
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  EXPECT_EQ(loader.GetResetParams(handle, 1), CpuCore::ResetParams{});
}

TEST(ProgramLoaderTest, GetResetParamsWithCodeAndStack) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks = {Program::Bank{}};
  program.segments = {
      Program::Segment{.bank = 0, .page_start = 1, .page_count = 1},
      Program::Segment{.bank = 0, .page_start = 5, .page_count = 2}};
  program.code_segment = 0;
  program.stack_segment = 1;
  program.instances = {Program::Instance{.code = 0, .stack = 0},
                       Program::Instance{.code = 100, .stack = 200}};
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  CpuCore::ResetParams expected_params[] = {
      {.mask = CpuCore::ResetParams::MC | CpuCore::ResetParams::MS |
               CpuCore::ResetParams::RBC | CpuCore::ResetParams::RBS,
       .mb = CpuCore::Banks().SetCode(0).SetStack(0).ToWord(),
       .bc = kMemoryBankPageSize,
       .bs = kMemoryBankPageSize * 7},
      {.mask = CpuCore::ResetParams::MC | CpuCore::ResetParams::MS |
               CpuCore::ResetParams::RBC | CpuCore::ResetParams::RBS,
       .mb = CpuCore::Banks().SetCode(0).SetStack(0).ToWord(),
       .bc = kMemoryBankPageSize + 100,
       .bs = kMemoryBankPageSize * 7 - 200}};
  EXPECT_EQ(loader.GetResetParams(handle, 0), expected_params[0]);
  EXPECT_EQ(loader.GetResetParams(handle, 1), expected_params[1]);
}

TEST(ProgramLoaderTest, GetResetParamsWithDataAndExtra) {
  Processor processor(ProcessorConfig::MultiBankMultiCore(1, 0));
  ProgramLoader loader(&processor);
  Program program = {};
  program.banks = {Program::Bank{}};
  program.segments = {
      Program::Segment{.bank = 0, .page_start = 1, .page_count = 1},
      Program::Segment{.bank = 0, .page_start = 5, .page_count = 2}};
  program.data_segment = 0;
  program.extra_segment = 1;
  program.instances = {Program::Instance{.data = 0, .extra = 0},
                       Program::Instance{.data = 100, .extra = 200}};
  ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);

  CpuCore::ResetParams expected_params[] = {
      {.mask = CpuCore::ResetParams::MD | CpuCore::ResetParams::ME |
               CpuCore::ResetParams::RBD | CpuCore::ResetParams::RBE,
       .mb = CpuCore::Banks().SetData(0).SetExtra(0).ToWord(),
       .bd = kMemoryBankPageSize,
       .be = kMemoryBankPageSize * 5},
      {.mask = CpuCore::ResetParams::MD | CpuCore::ResetParams::ME |
               CpuCore::ResetParams::RBD | CpuCore::ResetParams::RBE,
       .mb = CpuCore::Banks().SetData(0).SetExtra(0).ToWord(),
       .bd = kMemoryBankPageSize + 100,
       .be = kMemoryBankPageSize * 5 + 200}};
  EXPECT_EQ(loader.GetResetParams(handle, 0), expected_params[0]);
  EXPECT_EQ(loader.GetResetParams(handle, 1), expected_params[1]);
}

}  // namespace
}  // namespace oz3
