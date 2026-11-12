// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_layout.h"

#include <bit>

#include "oz3/disk/disk_image.h"

namespace oz3 {

namespace {

// Returns whether `sectors_per_cluster` is a power of two in range.
bool IsValidSectorsPerCluster(int sectors_per_cluster) {
  return sectors_per_cluster >= 1 &&
         sectors_per_cluster <= DiskLayout::kMaxSectorsPerCluster &&
         std::has_single_bit(static_cast<unsigned>(sectors_per_cluster));
}

// Returns the fewest FAT sectors `cluster_count` clusters need: one word for
// each cluster, and word 0 for the free list's head.
int GetMinFatSectors(int cluster_count) {
  return (cluster_count + kDiskSectorWords) / kDiskSectorWords;
}

// Returns the most clusters of `sectors_per_cluster` sectors that fit in
// `sector_count` sectors after the superblock and the fewest FAT sectors, but
// no more than kMaxClusterCount + 1, which is too many.
int GetMostClusters(int64_t sector_count, int sectors_per_cluster) {
  const auto fits = [&](int cluster_count) {
    return 1 + GetMinFatSectors(cluster_count) +
               static_cast<int64_t>(cluster_count) * sectors_per_cluster <=
           sector_count;
  };
  // Every count up to `low` fits, and none from `high` does.
  int low = 0;
  int high = DiskLayout::kMaxClusterCount + 2;
  while (high - low > 1) {
    const int middle = low + (high - low) / 2;
    if (fits(middle)) {
      low = middle;
    } else {
      high = middle;
    }
  }
  return low;
}

}  // namespace

bool DiskLayout::IsValid(int64_t sector_count) const {
  return IsValidSectorsPerCluster(sectors_per_cluster) && cluster_count >= 1 &&
         cluster_count <= kMaxClusterCount &&
         fat_sectors >= GetMinFatSectors(cluster_count) &&
         fat_sectors <= kMaxFatSectors && GetSectorCount() <= sector_count;
}

std::optional<DiskLayout> ChooseDiskLayout(int64_t sector_count,
                                           int sectors_per_cluster) {
  if (sectors_per_cluster != 0 &&
      !IsValidSectorsPerCluster(sectors_per_cluster)) {
    return std::nullopt;
  }
  const int first = (sectors_per_cluster == 0 ? 1 : sectors_per_cluster);
  const int last = (sectors_per_cluster == 0 ? DiskLayout::kMaxSectorsPerCluster
                                             : sectors_per_cluster);
  for (int size = first; size <= last; size *= 2) {
    const int cluster_count = GetMostClusters(sector_count, size);
    if (cluster_count > DiskLayout::kMaxClusterCount) {
      continue;
    }
    if (cluster_count < 1) {
      return std::nullopt;
    }
    return DiskLayout{
        .sectors_per_cluster = size,
        .cluster_count = cluster_count,
        .fat_sectors = GetMinFatSectors(cluster_count),
    };
  }
  return std::nullopt;
}

}  // namespace oz3
