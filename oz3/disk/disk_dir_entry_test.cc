// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_dir_entry.h"

#include <cstdint>
#include <optional>

#include "absl/time/civil_time.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

using ::testing::Each;
using ::testing::ElementsAre;

TEST(DiskDirEntryTest, DefaultIsFree) {
  const DiskDirEntry entry;
  EXPECT_TRUE(entry.IsFree());
  EXPECT_FALSE(entry.GetName().has_value());
  EXPECT_THAT(entry.GetWords(), Each(0));
}

TEST(DiskDirEntryTest, FieldsRoundTrip) {
  const DateTime modified =
      DateTime::FromCivil(absl::CivilSecond(2026, 11, 11, 13, 45, 30));
  DiskDirEntry entry;
  entry.SetName(*DiskFilename::Parse("HELLO.TXT"));
  entry.SetAttributes(kDiskAttribReadOnly | kDiskAttribHidden);
  entry.SetFirstCluster(0x1234);
  entry.SetSize(0x0005'6789);
  entry.SetModified(modified);

  EXPECT_FALSE(entry.IsFree());
  ASSERT_TRUE(entry.GetName().has_value());
  EXPECT_EQ(entry.GetName()->ToString(), "HELLO.TXT");
  EXPECT_EQ(entry.GetAttributes(), kDiskAttribReadOnly | kDiskAttribHidden);
  EXPECT_EQ(entry.GetFirstCluster(), 0x1234);
  EXPECT_EQ(entry.GetSize(), 0x0005'6789);
  EXPECT_EQ(entry.GetModified(), modified);

  const DateTime::Words& time = modified.GetWords();
  EXPECT_THAT(entry.GetWords(),
              ElementsAre('H' | 'E' << 8, 'L' | 'L' << 8, 'O', 0, 0, 0,
                          'T' | 'X' << 8, 'T', 3, 0x1234, 0x6789, 0x0005,
                          time[0], time[1], time[2], time[3]));

  EXPECT_EQ(DiskDirEntry::FromWords(entry.GetWords()), entry);
}

TEST(DiskDirEntryTest, SettersChangeOnlyTheirWords) {
  DiskDirEntry::Words words;
  for (int i = 0; i < DiskDirEntry::kWordCount; ++i) {
    words[i] = static_cast<uint16_t>(0xA000 + i);
  }
  DiskDirEntry entry = DiskDirEntry::FromWords(words);
  EXPECT_EQ(entry.GetWords(), words);

  entry.SetAttributes(0xFFFF);
  words[8] = 0xFFFF;
  EXPECT_EQ(entry.GetWords(), words);

  entry.SetFirstCluster(0);
  words[9] = 0;
  EXPECT_EQ(entry.GetWords(), words);

  entry.SetSize(1);
  words[10] = 1;
  words[11] = 0;
  EXPECT_EQ(entry.GetWords(), words);

  entry.SetModified(DateTime());
  words[12] = words[13] = words[14] = words[15] = 0;
  EXPECT_EQ(entry.GetWords(), words);

  entry.SetName(*DiskFilename::Parse("A"));
  words[0] = 'A';
  words[1] = words[2] = words[3] = words[4] = words[5] = 0;
  words[6] = words[7] = 0;
  EXPECT_EQ(entry.GetWords(), words);
}

TEST(DiskDirEntryTest, SizeLimits) {
  DiskDirEntry entry;
  // 65,533 clusters of 64 sectors, as the root has one of 65,534.
  EXPECT_EQ(DiskDirEntry::kMaxSize, 1'073'692'672);
  entry.SetSize(DiskDirEntry::kMaxSize);
  EXPECT_EQ(entry.GetSize(), 0x3FFF'4000);
  EXPECT_EQ(entry.GetWords()[10], 0x4000);
  EXPECT_EQ(entry.GetWords()[11], 0x3FFF);
  entry.SetSize(0x10000);
  EXPECT_EQ(entry.GetSize(), 0x10000);
  EXPECT_EQ(entry.GetWords()[10], 0);
  EXPECT_EQ(entry.GetWords()[11], 1);
}

TEST(DiskDirEntryTest, SizesNoDiskHoldsReadAsNone) {
  for (uint32_t size : {0x3FFF'4001u, 0xFFFF'FFFFu}) {
    SCOPED_TRACE(size);
    DiskDirEntry::Words words = {};
    words[10] = static_cast<uint16_t>(size);
    words[11] = static_cast<uint16_t>(size >> 16);
    EXPECT_EQ(DiskDirEntry::FromWords(words).GetSize(), std::nullopt);
  }
}

TEST(DiskDirEntryTest, FirstClusterLimit) {
  DiskDirEntry entry;
  entry.SetFirstCluster(DiskDirEntry::kMaxFirstCluster);
  EXPECT_EQ(entry.GetFirstCluster(), 0xFFFF);
}

TEST(DiskDirEntryTest, BrokenNameReadsAsNone) {
  DiskDirEntry::Words words = {};
  words[0] = '?';
  const DiskDirEntry entry = DiskDirEntry::FromWords(words);
  EXPECT_FALSE(entry.IsFree());
  EXPECT_FALSE(entry.GetName().has_value());
}

TEST(DiskDirEntryTest, FreeOnlyByFirstWord) {
  // A zero first word is free, whatever follows.
  DiskDirEntry::Words words = {};
  words[1] = 'A';
  words[10] = 5;
  EXPECT_TRUE(DiskDirEntry::FromWords(words).IsFree());
}

TEST(DiskDirEntryTest, LowerCaseNameReadsUpperCased) {
  DiskDirEntry::Words words = {};
  words[0] = 'a' | 'b' << 8;
  const DiskDirEntry entry = DiskDirEntry::FromWords(words);
  ASSERT_TRUE(entry.GetName().has_value());
  EXPECT_EQ(entry.GetName()->ToString(), "AB");
}

TEST(DiskDirEntryTest, AttributeBits) {
  EXPECT_EQ(kDiskAttribReadOnly, 1);
  EXPECT_EQ(kDiskAttribHidden, 2);
  EXPECT_EQ(kDiskAttribDirectory, 4);
  EXPECT_EQ(kDiskAttribSettable, 3);
}

}  // namespace
}  // namespace oz3
