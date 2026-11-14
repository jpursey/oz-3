// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_check.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <limits>
#include <optional>
#include <vector>

#include "absl/container/btree_map.h"
#include "absl/types/span.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_superblock.h"

namespace oz3 {

namespace {

using ChainProblem = DiskCheckReport::ChainProblem;
using Sector = std::array<uint16_t, kDiskSectorWords>;

// How walking a chain ends.
enum class ChainEnd {
  kEnd,        // At the chain's end.
  kContinues,  // After the clusters it was to claim, with the chain going on.
  kBroken,     // At a cluster out of range, or one the chain already claimed.
  kClaimed,    // At a cluster an earlier chain claimed.
};

// What walking a chain found.
struct ChainWalk {
  ChainEnd end = ChainEnd::kEnd;
  int count = 0;  // The clusters it claimed.
};

// The walk claiming a cluster no walk has. Walks are numbered from 1.
constexpr int kUnclaimed = 0;

// No limit on the clusters a walk claims.
constexpr int kNoClusterLimit = std::numeric_limits<int>::max();

// Returns what is wrong with a file's chain that `walk` claimed, for a size
// needing `needed` clusters, or nullopt if nothing is.
std::optional<ChainProblem> GetChainProblem(const ChainWalk& walk, int needed) {
  switch (walk.end) {
    case ChainEnd::kEnd:
      if (walk.count < needed) {
        return ChainProblem::kShort;
      }
      return std::nullopt;
    case ChainEnd::kContinues:
      return ChainProblem::kLong;
    case ChainEnd::kBroken:
      return ChainProblem::kBroken;
    case ChainEnd::kClaimed:
      return ChainProblem::kCrossLinked;
  }
  return std::nullopt;
}

// Checks one disk, claiming its clusters chain by chain into a report.
class DiskChecker {
 public:
  // Checks the disk of `layout` on `image` into `report`, which must both
  // outlive it.
  DiskChecker(DiskImage* image, const DiskLayout& layout,
              DiskCheckReport* report)
      : image_(image),
        layout_(layout),
        report_(report),
        claims_(layout.cluster_count + 1, kUnclaimed) {}

  // Checks the disk, whose root starts at `root_cluster`.
  DiskStatus Check(int root_cluster);

 private:
  // Reads the whole FAT into fat_.
  DiskStatus ReadFat();

  // Checks the root's entries, in `root_clusters`, and the files they hold.
  DiskStatus CheckFiles(absl::Span<const int> root_clusters);

  // Checks the file `entry`, claiming its chain.
  void CheckFile(const DiskDirEntry& entry);

  // Claims the chain from `first` in a new walk, up to `max_count` clusters,
  // adding each to `clusters` if it isn't null.
  ChainWalk ClaimChain(int first, int max_count,
                       std::vector<int>* clusters = nullptr);

  DiskImage* const image_;
  const DiskLayout layout_;
  DiskCheckReport* const report_;
  std::vector<uint16_t> fat_;
  std::vector<int> claims_;  // The walk claiming each cluster, from 1.
  int walk_count_ = 0;
  absl::btree_map<DiskFilename, int> name_counts_;
};

DiskStatus DiskChecker::Check(int root_cluster) {
  if (DiskStatus status = ReadFat(); status != DiskStatus::kOk) {
    return status;
  }
  std::vector<int> root_clusters;
  if (ClaimChain(root_cluster, kNoClusterLimit, &root_clusters).end !=
      ChainEnd::kEnd) {
    report_->broken_root = true;
  }
  if (DiskStatus status = CheckFiles(root_clusters);
      status != DiskStatus::kOk) {
    return status;
  }
  // Word 0 is the link of an imaginary cluster 0, to the free list's head.
  if (const std::optional<int> head = layout_.GetNextCluster(0, fat_[0]);
      !head.has_value() ||
      ClaimChain(*head, kNoClusterLimit).end != ChainEnd::kEnd) {
    report_->broken_free_list = true;
  }
  report_->orphaned_clusters = static_cast<int>(
      std::count(claims_.begin() + 1, claims_.end(), kUnclaimed));
  return DiskStatus::kOk;
}

DiskStatus DiskChecker::ReadFat() {
  fat_.resize(layout_.fat_sectors * kDiskSectorWords);
  for (int i = 0; i < layout_.fat_sectors; ++i) {
    if (DiskStatus status =
            image_->ReadSector(DiskLayout::kFirstFatSector + i,
                               absl::MakeSpan(fat_).subspan(
                                   i * kDiskSectorWords, kDiskSectorWords));
        status != DiskStatus::kOk) {
      return status;
    }
  }
  return DiskStatus::kOk;
}

DiskStatus DiskChecker::CheckFiles(absl::Span<const int> root_clusters) {
  Sector words;
  for (int cluster : root_clusters) {
    const int first_sector = layout_.GetClusterSector(cluster);
    for (int sector = first_sector;
         sector < first_sector + layout_.sectors_per_cluster; ++sector) {
      if (DiskStatus status = image_->ReadSector(sector, absl::MakeSpan(words));
          status != DiskStatus::kOk) {
        return status;
      }
      for (int index = 0; index < DiskDirEntry::kEntriesPerSector; ++index) {
        const DiskDirEntry entry =
            DiskDirEntry::FromWords(absl::MakeConstSpan(words).subspan(
                index * DiskDirEntry::kWordCount, DiskDirEntry::kWordCount));
        // Freeing an entry zeroes it all, so one with only its name zeroed is
        // a file with a bad name, which may still have a chain.
        if (entry != DiskDirEntry()) {
          CheckFile(entry);
        }
      }
    }
  }
  return DiskStatus::kOk;
}

void DiskChecker::CheckFile(const DiskDirEntry& entry) {
  if (const std::optional<DiskFilename> name = entry.GetName();
      !name.has_value()) {
    report_->bad_names.push_back(entry);
  } else if (++name_counts_[*name] == 2) {
    report_->duplicate_names.push_back(*name);
  }

  // A size no disk holds needs more clusters than any chain claims. So does a
  // size past this disk's largest file, as the root claims at least one.
  const int cluster_words = layout_.GetClusterWords();
  const std::optional<int> size = entry.GetSize();
  const int needed = size.has_value()
                         ? (*size + cluster_words - 1) / cluster_words
                         : kNoClusterLimit;
  if (const std::optional<ChainProblem> problem =
          GetChainProblem(ClaimChain(entry.GetFirstCluster(), needed), needed);
      problem.has_value()) {
    report_->file_problems.push_back({.entry = entry, .problem = *problem});
  }
}

ChainWalk DiskChecker::ClaimChain(int first, int max_count,
                                  std::vector<int>* clusters) {
  // GetNextCluster checks every link, but not the first cluster.
  if (first > layout_.cluster_count) {
    return {.end = ChainEnd::kBroken};
  }
  const int walk = ++walk_count_;
  int count = 0;
  for (int cluster = first; cluster != 0;) {
    if (count == max_count) {
      return {.end = ChainEnd::kContinues, .count = count};
    }
    if (claims_[cluster] != kUnclaimed) {
      // Back into this walk is a loop.
      return {.end = claims_[cluster] == walk ? ChainEnd::kBroken
                                              : ChainEnd::kClaimed,
              .count = count};
    }
    claims_[cluster] = walk;
    ++count;
    if (clusters != nullptr) {
      clusters->push_back(cluster);
    }
    const std::optional<int> next =
        layout_.GetNextCluster(cluster, fat_[cluster]);
    if (!next.has_value()) {
      return {.end = ChainEnd::kBroken, .count = count};
    }
    cluster = *next;
  }
  return {.end = ChainEnd::kEnd, .count = count};
}

}  // namespace

DiskStatus CheckDisk(DiskImage& image, DiskCheckReport* report) {
  *report = {};
  DiskSuperblock::Words words;
  if (DiskStatus status = image.ReadSector(DiskLayout::kSuperblockSector,
                                           absl::MakeSpan(words));
      status != DiskStatus::kOk) {
    return status;
  }
  const DiskSuperblock superblock = DiskSuperblock::FromWords(words);
  if (!superblock.IsValid(image.GetSectorCount())) {
    return DiskStatus::kUnformatted;
  }
  return DiskChecker(&image, superblock.GetLayout(), report)
      .Check(superblock.GetRootCluster());
}

}  // namespace oz3
