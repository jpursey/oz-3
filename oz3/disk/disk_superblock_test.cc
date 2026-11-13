// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_superblock.h"

#include <cstdint>
#include <optional>

#include "absl/time/civil_time.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

using ::testing::Each;
using ::testing::Optional;

// A 360 KB floppy's layout: 716 clusters of one sector, after 3 FAT sectors.
constexpr DiskLayout kFloppyLayout = {
    .sectors_per_cluster = 1,
    .cluster_count = 716,
    .fat_sectors = 3,
};
constexpr int kFloppySectors = 720;

DateTime GetFormatted() {
  return DateTime::FromCivil(absl::CivilSecond(2026, 11, 13, 9, 30, 15));
}

// Returns the superblock formatting a floppy gives, labeled "WORK".
DiskSuperblock MakeFloppySuperblock() {
  return DiskSuperblock(kFloppyLayout, 1, *DiskLabel::Parse("WORK"),
                        GetFormatted());
}

// Returns `superblock` with word `index` set to `value`.
DiskSuperblock SetWord(const DiskSuperblock& superblock, int index,
                       uint16_t value) {
  DiskSuperblock::Words words = superblock.GetWords();
  words[index] = value;
  return DiskSuperblock::FromWords(words);
}

TEST(DiskSuperblockTest, DefaultIsBlank) {
  const DiskSuperblock superblock;
  EXPECT_THAT(superblock.GetWords(), Each(0));
  EXPECT_FALSE(superblock.IsValid(kFloppySectors));
}

TEST(DiskSuperblockTest, FormattingWritesEveryField) {
  const DiskSuperblock superblock = MakeFloppySuperblock();
  const DateTime::Words& formatted = GetFormatted().GetWords();
  DiskSuperblock::Words words = {};
  words[0] = 'O' | 'Z' << 8;  // "OZFS"
  words[1] = 'F' | 'S' << 8;
  words[2] = 1;    // Version
  words[3] = 1;    // Sectors per cluster
  words[4] = 716;  // Clusters
  words[5] = 3;    // FAT sectors
  words[6] = 1;    // Root cluster
  words[8] = 'W' | 'O' << 8;
  words[9] = 'R' | 'K' << 8;
  words[16] = formatted[0];
  words[17] = formatted[1];
  words[18] = formatted[2];
  words[19] = formatted[3];
  EXPECT_EQ(superblock.GetWords(), words);

  EXPECT_TRUE(superblock.IsValid(kFloppySectors));
  EXPECT_EQ(superblock.GetLayout(), kFloppyLayout);
  EXPECT_EQ(superblock.GetRootCluster(), 1);
  EXPECT_THAT(superblock.GetLabel(), Optional(*DiskLabel::Parse("WORK")));
  EXPECT_EQ(DiskSuperblock::FromWords(words), superblock);
}

TEST(DiskSuperblockTest, GettersReturnWhatTheWordsHold) {
  DiskSuperblock::Words words = {};
  words[3] = 3;
  words[4] = 0xFFFF;
  words[5] = 0;
  words[6] = 0x1234;
  const DiskSuperblock superblock = DiskSuperblock::FromWords(words);
  EXPECT_EQ(superblock.GetLayout(), (DiskLayout{.sectors_per_cluster = 3,
                                                .cluster_count = 0xFFFF,
                                                .fat_sectors = 0}));
  EXPECT_EQ(superblock.GetRootCluster(), 0x1234);
  // An empty label is valid.
  EXPECT_THAT(superblock.GetLabel(), Optional(DiskLabel()));
}

TEST(DiskSuperblockTest, SetLabelChangesOnlyItsWords) {
  DiskSuperblock::Words words;
  for (int i = 0; i < kDiskSectorWords; ++i) {
    words[i] = static_cast<uint16_t>(0xA000 + i);
  }
  DiskSuperblock superblock = DiskSuperblock::FromWords(words);
  EXPECT_EQ(superblock.GetWords(), words);

  superblock.SetLabel(*DiskLabel::Parse("ARCHIVE"));
  words[8] = 'A' | 'R' << 8;
  words[9] = 'C' | 'H' << 8;
  words[10] = 'I' | 'V' << 8;
  words[11] = 'E';
  words[12] = words[13] = words[14] = words[15] = 0;
  EXPECT_EQ(superblock.GetWords(), words);
}

TEST(DiskSuperblockTest, IsValidChecksEachField) {
  const DiskSuperblock superblock = MakeFloppySuperblock();
  EXPECT_FALSE(SetWord(superblock, 0, 0x4F5A).IsValid(kFloppySectors));
  EXPECT_FALSE(SetWord(superblock, 1, 0).IsValid(kFloppySectors));
  EXPECT_FALSE(SetWord(superblock, 2, 0).IsValid(kFloppySectors));
  EXPECT_FALSE(SetWord(superblock, 2, 2).IsValid(kFloppySectors));
  EXPECT_FALSE(SetWord(superblock, 3, 3).IsValid(kFloppySectors));
  EXPECT_FALSE(SetWord(superblock, 4, 717).IsValid(kFloppySectors));
  EXPECT_FALSE(SetWord(superblock, 5, 2).IsValid(kFloppySectors));
  EXPECT_FALSE(SetWord(superblock, 6, 0).IsValid(kFloppySectors));
  EXPECT_FALSE(SetWord(superblock, 6, 717).IsValid(kFloppySectors));
  EXPECT_TRUE(SetWord(superblock, 6, 716).IsValid(kFloppySectors));

  const DiskSuperblock bad_label = SetWord(superblock, 8, '.');
  EXPECT_EQ(bad_label.GetLabel(), std::nullopt);
  EXPECT_FALSE(bad_label.IsValid(kFloppySectors));
}

TEST(DiskSuperblockTest, IsValidChecksTheImageSize) {
  const DiskSuperblock superblock = MakeFloppySuperblock();
  EXPECT_TRUE(superblock.IsValid(kFloppySectors + 1));
  EXPECT_FALSE(superblock.IsValid(kFloppySectors - 1));
}

TEST(DiskSuperblockTest, IsValidIgnoresTheReservedWordsAndBootArea) {
  const DiskSuperblock superblock = MakeFloppySuperblock();
  EXPECT_TRUE(SetWord(superblock, 7, 0x1234).IsValid(kFloppySectors));
  EXPECT_TRUE(SetWord(superblock, 31, 0x1234).IsValid(kFloppySectors));
  EXPECT_TRUE(SetWord(superblock, 100, 0x5678).IsValid(kFloppySectors));
}

}  // namespace
}  // namespace oz3
