// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_LAYOUT_H_
#define OZ3_DISK_DISK_LAYOUT_H_

#include <compare>
#include <optional>

namespace oz3 {

// The words in a sector.
inline constexpr int kDiskSectorWords = 256;

// Returns the fewest FAT sectors `cluster_count` clusters need: one word for
// each cluster, and word 0 for the free list's head.
constexpr int GetMinDiskFatSectors(int cluster_count) {
  return (cluster_count + kDiskSectorWords) / kDiskSectorWords;
}

// Where a formatted disk's parts are, as its superblock records them: the
// superblock in sector 0, then the FAT's sectors, then the clusters, numbered
// from 1. It is only arithmetic, with no disk.
struct DiskLayout {
  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The most sectors in a cluster. Sectors per cluster is a power of two, from
  // 1 to this.
  static constexpr int kMaxSectorsPerCluster = 64;

  // The most clusters on a disk. FFFFh is a chain's end, so the last cluster
  // number is one less.
  static constexpr int kMaxClusterCount = 0xFFFE;

  // The most FAT sectors: those the most clusters need.
  static constexpr int kMaxFatSectors = GetMinDiskFatSectors(kMaxClusterCount);

  // The most sectors a disk uses: the superblock, the most FAT sectors, and
  // the most clusters of the most sectors.
  static constexpr int kMaxSectorCount =
      1 + kMaxFatSectors + kMaxClusterCount * kMaxSectorsPerCluster;

  //----------------------------------------------------------------------------
  // Operators
  //----------------------------------------------------------------------------

  auto operator<=>(const DiskLayout&) const = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the sectors the disk uses: the superblock, the FAT, and the
  // clusters. Any sectors of the image past these are unused.
  int GetSectorCount() const {
    return 1 + fat_sectors + cluster_count * sectors_per_cluster;
  }

  // Returns the first sector of `cluster`, which must be 1 to cluster_count.
  int GetClusterSector(int cluster) const {
    return 1 + fat_sectors + (cluster - 1) * sectors_per_cluster;
  }

  // Returns the most words a file can have: what every cluster holds but the
  // root's first.
  constexpr int GetMaxFileSize() const {
    return (cluster_count - 1) * sectors_per_cluster * kDiskSectorWords;
  }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Returns whether every field is in range, and the disk fits in an image of
  // `sector_count` sectors. The FAT needs a word for each cluster, and word 0
  // for the free list's head, but may have more sectors than that, up to
  // kMaxFatSectors.
  bool IsValid(int sector_count) const;

  //----------------------------------------------------------------------------
  // Data
  //----------------------------------------------------------------------------

  int sectors_per_cluster = 0;
  int cluster_count = 0;
  int fat_sectors = 0;
};

// Returns the layout formatting an image of `sector_count` sectors gives, with
// `sectors_per_cluster` sectors in each cluster, or 0 to pick the fewest that
// cover the image. The layout has the fewest FAT sectors, and the most
// clusters that fit after them. Returns nullopt if `sectors_per_cluster` isn't
// 0 or a power of two up to kMaxSectorsPerCluster, if covering the image would
// take more than kMaxClusterCount clusters, or if not even one cluster fits.
std::optional<DiskLayout> ChooseDiskLayout(int sector_count,
                                           int sectors_per_cluster);

}  // namespace oz3

#endif  // OZ3_DISK_DISK_LAYOUT_H_
