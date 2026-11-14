// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_layout.h"

#include <cstdint>
#include <optional>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

using ::testing::Optional;

DiskLayout MakeLayout(int sectors_per_cluster, int cluster_count,
                      int fat_sectors) {
  return {.sectors_per_cluster = sectors_per_cluster,
          .cluster_count = cluster_count,
          .fat_sectors = fat_sectors};
}

TEST(DiskLayoutTest, ChoosesTheDesignTable) {
  EXPECT_THAT(ChooseDiskLayout(720, 0), Optional(MakeLayout(1, 716, 3)));
  EXPECT_THAT(ChooseDiskLayout(1'440, 0), Optional(MakeLayout(1, 1'433, 6)));
  EXPECT_THAT(ChooseDiskLayout(2'880, 0), Optional(MakeLayout(1, 2'867, 12)));
  EXPECT_THAT(ChooseDiskLayout(20'480, 0), Optional(MakeLayout(1, 20'399, 80)));
  EXPECT_THAT(ChooseDiskLayout(40'960, 0),
              Optional(MakeLayout(1, 40'799, 160)));
  EXPECT_THAT(ChooseDiskLayout(81'920, 0),
              Optional(MakeLayout(2, 40'879, 160)));
  EXPECT_THAT(ChooseDiskLayout(262'144, 0),
              Optional(MakeLayout(4, 65'471, 256)));
}

TEST(DiskLayoutTest, ChoosesMoreSectorsPerClusterPastTheClusterLimit) {
  // 1 + 256 + 65,534 sectors is exactly the most clusters of one sector.
  EXPECT_THAT(ChooseDiskLayout(65'791, 0),
              Optional(MakeLayout(1, DiskLayout::kMaxClusterCount, 256)));
  EXPECT_THAT(ChooseDiskLayout(65'791, 1),
              Optional(MakeLayout(1, DiskLayout::kMaxClusterCount, 256)));

  // One more sector would take a 65,535th cluster.
  EXPECT_THAT(ChooseDiskLayout(65'792, 0),
              Optional(MakeLayout(2, 32'831, 129)));
  EXPECT_EQ(ChooseDiskLayout(65'792, 1), std::nullopt);
}

TEST(DiskLayoutTest, ChoosesTheMostSectorsPerCluster) {
  // 2 GiB: 4,194,304 sectors.
  const std::optional<DiskLayout> layout = ChooseDiskLayout(4'194'304, 0);
  ASSERT_TRUE(layout.has_value());
  EXPECT_EQ(layout->sectors_per_cluster, DiskLayout::kMaxSectorsPerCluster);
  EXPECT_TRUE(layout->IsValid(4'194'304));
  EXPECT_EQ(ChooseDiskLayout(4'194'304, 32), std::nullopt);

  // No sectors per cluster covers the biggest image.
  EXPECT_EQ(ChooseDiskLayout(0xFFFF'FFFF, 0), std::nullopt);
}

TEST(DiskLayoutTest, ChoosesTheMostClustersForTheSectorsPerCluster) {
  EXPECT_THAT(ChooseDiskLayout(720, 2), Optional(MakeLayout(2, 358, 2)));
  EXPECT_THAT(ChooseDiskLayout(720, 64), Optional(MakeLayout(64, 11, 1)));
}

TEST(DiskLayoutTest, RefusesBadSectorsPerCluster) {
  EXPECT_EQ(ChooseDiskLayout(720, -1), std::nullopt);
  EXPECT_EQ(ChooseDiskLayout(720, 3), std::nullopt);
  EXPECT_EQ(ChooseDiskLayout(720, 48), std::nullopt);
  EXPECT_EQ(ChooseDiskLayout(720, 128), std::nullopt);
}

TEST(DiskLayoutTest, RefusesImagesTooSmall) {
  EXPECT_THAT(ChooseDiskLayout(3, 0), Optional(MakeLayout(1, 1, 1)));
  EXPECT_EQ(ChooseDiskLayout(2, 0), std::nullopt);
  EXPECT_EQ(ChooseDiskLayout(0, 0), std::nullopt);
  EXPECT_EQ(ChooseDiskLayout(-1, 0), std::nullopt);
  EXPECT_EQ(ChooseDiskLayout(3, 2), std::nullopt);
}

TEST(DiskLayoutTest, IsValidChecksTheImageSize) {
  const DiskLayout layout = MakeLayout(1, 716, 3);
  EXPECT_EQ(layout.GetSectorCount(), 720);
  EXPECT_TRUE(layout.IsValid(720));
  EXPECT_TRUE(layout.IsValid(721));
  EXPECT_FALSE(layout.IsValid(719));
}

TEST(DiskLayoutTest, IsValidChecksSectorsPerCluster) {
  EXPECT_FALSE(MakeLayout(0, 1, 1).IsValid(1'000));
  EXPECT_FALSE(MakeLayout(3, 1, 1).IsValid(1'000));
  EXPECT_FALSE(MakeLayout(-1, 1, 1).IsValid(1'000));
  EXPECT_TRUE(MakeLayout(64, 1, 1).IsValid(1'000));
  EXPECT_FALSE(MakeLayout(128, 1, 1).IsValid(1'000));
}

TEST(DiskLayoutTest, IsValidChecksTheClusterCount) {
  EXPECT_FALSE(MakeLayout(1, 0, 1).IsValid(1'000));
  EXPECT_TRUE(MakeLayout(1, 1, 1).IsValid(1'000));
  EXPECT_TRUE(MakeLayout(1, DiskLayout::kMaxClusterCount, 256).IsValid(70'000));
  EXPECT_FALSE(
      MakeLayout(1, DiskLayout::kMaxClusterCount + 1, 257).IsValid(70'000));
}

TEST(DiskLayoutTest, TheLargestDiskUsesEverySector) {
  EXPECT_EQ(DiskLayout::kMaxSectorCount, 4'194'433);
  const std::optional<DiskLayout> layout =
      ChooseDiskLayout(DiskLayout::kMaxSectorCount, 0);
  ASSERT_TRUE(layout.has_value());
  EXPECT_EQ(*layout, MakeLayout(64, DiskLayout::kMaxClusterCount, 256));
  EXPECT_EQ(layout->GetSectorCount(), DiskLayout::kMaxSectorCount);
  EXPECT_TRUE(layout->IsValid(DiskLayout::kMaxSectorCount));
}

TEST(DiskLayoutTest, IsValidChecksTheFatSectors) {
  EXPECT_FALSE(MakeLayout(1, 255, 0).IsValid(1'000));
  EXPECT_TRUE(MakeLayout(1, 255, 1).IsValid(1'000));
  EXPECT_FALSE(MakeLayout(1, 256, 1).IsValid(1'000));
  EXPECT_TRUE(MakeLayout(1, 256, 2).IsValid(1'000));

  // More FAT sectors than needed, up to what the most clusters need.
  EXPECT_EQ(DiskLayout::kMaxFatSectors, 256);
  EXPECT_TRUE(MakeLayout(1, 716, 4).IsValid(721));
  EXPECT_TRUE(MakeLayout(1, 1, DiskLayout::kMaxFatSectors).IsValid(70'000));
  EXPECT_FALSE(
      MakeLayout(1, 1, DiskLayout::kMaxFatSectors + 1).IsValid(70'000));
}

TEST(DiskLayoutTest, ClusterSectors) {
  const DiskLayout floppy = MakeLayout(1, 716, 3);
  EXPECT_EQ(floppy.GetClusterSector(1), 4);
  EXPECT_EQ(floppy.GetClusterSector(716), 719);

  const DiskLayout hard_drive = MakeLayout(4, 65'471, 256);
  EXPECT_EQ(hard_drive.GetClusterSector(1), 257);
  EXPECT_EQ(hard_drive.GetClusterSector(65'471), 262'137);
  EXPECT_EQ(hard_drive.GetSectorCount(), 262'141);
}

TEST(DiskLayoutTest, ClusterWords) {
  EXPECT_EQ(MakeLayout(1, 716, 3).GetClusterWords(), 256);
  EXPECT_EQ(MakeLayout(4, 65'471, 256).GetClusterWords(), 1024);
  EXPECT_EQ(MakeLayout(64, 100, 1).GetClusterWords(), 16384);
}

TEST(DiskLayoutTest, FatSectors) {
  EXPECT_EQ(DiskLayout::kSuperblockSector, 0);
  EXPECT_EQ(DiskLayout::kFirstFatSector, 1);
  EXPECT_EQ(DiskLayout::GetFatSector(0), 1);
  EXPECT_EQ(DiskLayout::GetFatSector(255), 1);
  EXPECT_EQ(DiskLayout::GetFatSector(256), 2);
  EXPECT_EQ(DiskLayout::GetFatSector(DiskLayout::kMaxClusterCount), 256);
}

TEST(DiskLayoutTest, NextClusters) {
  const DiskLayout floppy = MakeLayout(1, 716, 3);
  EXPECT_EQ(DiskLayout::kFatEnd, 0xFFFF);

  // A link is the next cluster, up to the last.
  EXPECT_THAT(floppy.GetNextCluster(5, 9), Optional(9));
  EXPECT_THAT(floppy.GetNextCluster(5, 716), Optional(716));
  EXPECT_THAT(floppy.GetNextCluster(5, 5), Optional(5));

  // 0 is the cluster after, or the end on the last.
  EXPECT_THAT(floppy.GetNextCluster(5, 0), Optional(6));
  EXPECT_THAT(floppy.GetNextCluster(715, 0), Optional(716));
  EXPECT_THAT(floppy.GetNextCluster(716, 0), Optional(0));

  // FFFFh is the end.
  EXPECT_THAT(floppy.GetNextCluster(5, 0xFFFF), Optional(0));
  EXPECT_THAT(floppy.GetNextCluster(716, 0xFFFF), Optional(0));

  // Word 0 gives the free list's head the same way.
  EXPECT_THAT(floppy.GetNextCluster(0, 0), Optional(1));
  EXPECT_THAT(floppy.GetNextCluster(0, 2), Optional(2));
  EXPECT_THAT(floppy.GetNextCluster(0, 0xFFFF), Optional(0));

  // Past the last cluster is out of range.
  EXPECT_EQ(floppy.GetNextCluster(5, 717), std::nullopt);
  EXPECT_EQ(floppy.GetNextCluster(0, 0xFFFE), std::nullopt);

  // With one cluster, a 0 on it is the end.
  const DiskLayout tiny = MakeLayout(1, 1, 1);
  EXPECT_THAT(tiny.GetNextCluster(1, 0), Optional(0));
  EXPECT_THAT(tiny.GetNextCluster(0, 0), Optional(1));
}

}  // namespace
}  // namespace oz3
