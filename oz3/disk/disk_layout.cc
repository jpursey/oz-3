// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_layout.h"

#include <bit>

namespace oz3 {

namespace {

// Returns whether `sectors_per_cluster` is a power of two in range.
bool IsValidSectorsPerCluster(int sectors_per_cluster) {
  return sectors_per_cluster >= 1 &&
         sectors_per_cluster <= DiskLayout::kMaxSectorsPerCluster &&
         std::has_single_bit(static_cast<unsigned>(sectors_per_cluster));
}

// Returns the most clusters of `sectors_per_cluster` sectors that fit in
// `sector_count` sectors after the superblock and the fewest FAT sectors, but
// no more than kMaxClusterCount + 1, which is too many.
int GetMostClusters(int sector_count, int sectors_per_cluster) {
  const auto fits = [&](int cluster_count) {
    const DiskLayout layout = {
        .sectors_per_cluster = sectors_per_cluster,
        .cluster_count = cluster_count,
        .fat_sectors = GetMinDiskFatSectors(cluster_count),
    };
    return layout.GetSectorCount() <= sector_count;
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

bool DiskLayout::IsValid(int sector_count) const {
  return IsValidSectorsPerCluster(sectors_per_cluster) && cluster_count >= 1 &&
         cluster_count <= kMaxClusterCount &&
         fat_sectors >= GetMinDiskFatSectors(cluster_count) &&
         fat_sectors <= kMaxFatSectors && GetSectorCount() <= sector_count;
}

std::optional<int> DiskLayout::GetNextCluster(int cluster,
                                              uint16_t link) const {
  if (link == kFatEnd || (link == 0 && cluster == cluster_count)) {
    return 0;
  }
  if (link == 0) {
    return cluster + 1;
  }
  if (link > cluster_count) {
    return std::nullopt;
  }
  return link;
}

std::optional<DiskLayout> ChooseDiskLayout(int sector_count,
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
        .fat_sectors = GetMinDiskFatSectors(cluster_count),
    };
  }
  return std::nullopt;
}

}  // namespace oz3
