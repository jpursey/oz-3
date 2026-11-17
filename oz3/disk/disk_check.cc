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
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_superblock.h"

namespace oz3 {

namespace {

using ChainProblem = DiskCheckReport::ChainProblem;
using Sector = std::array<uint16_t, kDiskSectorWords>;

// Whether to repair a disk after checking it.
enum class CheckMode {
  kCheck,
  kRepair,
};

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
  int last = 0;   // The last cluster it claimed, or 0 if none.
};

// A root sector holding entries that repairing changes, with their sizes and
// first clusters as repairing leaves them.
struct DirSectorRepair {
  int sector = 0;
  Sector words = {};
  std::vector<int> bad_names;  // Its entries with bad names, by index.
};

// The walk claiming a cluster no walk has. Walks are numbered from 1.
constexpr int kUnclaimed = 0;

// No limit on the clusters a walk claims.
constexpr int kNoClusterLimit = std::numeric_limits<int>::max();

// The names a bad name can be given: FILE00000000.CHK to FILE99999999.CHK.
// Every other entry the root can hold may have one, and one is still unused.
constexpr int kFoundNameCount = 100'000'000;
static_assert(DiskLayout::kMaxClusterCount * DiskLayout::kMaxSectorsPerCluster *
                  DiskDirEntry::kEntriesPerSector <
              kFoundNameCount);

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

// Returns the FAT word of free `cluster` linking it to `next`, or ending the
// free list if `next` is 0, as a zeroed FAT has it where it can: 0 when `next`
// follows `cluster`, or ends the list at the last cluster. So a free list
// rebuilt over a zeroed part of the FAT leaves it zeroed.
uint16_t GetFreeLink(const DiskLayout& layout, int cluster, int next) {
  if (next == cluster + 1 || (next == 0 && cluster == layout.cluster_count)) {
    return 0;
  }
  if (next == 0) {
    return DiskLayout::kFatEnd;
  }
  return static_cast<uint16_t>(next);
}

// Checks one disk, claiming its clusters chain by chain into a report, and
// then repairs it if asked.
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

  // Repairs what Check found, which must have returned kOk.
  DiskStatus Repair();

 private:
  // Reads the whole FAT into fat_.
  DiskStatus ReadFat();

  // Checks the root's entries, in `root_clusters`, and the files they hold.
  DiskStatus CheckFiles(absl::Span<const int> root_clusters);

  // Checks the file `entry`, claiming its chain, and returns the entry as
  // repairing leaves it, but for its name.
  DiskDirEntry CheckFile(const DiskDirEntry& entry);

  // Claims the chain from `first` in a new walk, up to `max_count` clusters,
  // adding each to `clusters` if it isn't null.
  ChainWalk ClaimChain(int first, int max_count,
                       std::vector<int>* clusters = nullptr);

  // Returns the first found name no file has, which a file then has.
  DiskFilename TakeFoundName();

  // Writes each FAT sector where `fat` differs from fat_ into fat_ and to the
  // disk, sector 1 last, as it holds the free list's head.
  DiskStatus WriteFat(absl::Span<const uint16_t> fat);

  DiskImage* const image_;
  const DiskLayout layout_;
  DiskCheckReport* const report_;
  std::vector<uint16_t> fat_;  // As it is on the disk.
  std::vector<int> claims_;    // The walk claiming each cluster, from 1.
  int walk_count_ = 0;
  int last_chain_walk_ = 0;  // The root's and files' last. Later walks are the
                             // free list's.
  absl::btree_map<DiskFilename, int> name_counts_;
  int found_number_ = 0;  // The next found name to try.

  // What repairing writes, in order: root sectors, in directory order, then
  // the chains' new ends, at these clusters.
  std::vector<DirSectorRepair> dir_repairs_;
  std::vector<int> chain_ends_;
};

DiskStatus DiskChecker::Check(int root_cluster) {
  if (DiskStatus status = ReadFat(); status != DiskStatus::kOk) {
    return status;
  }
  std::vector<int> root_clusters;
  if (const ChainWalk root =
          ClaimChain(root_cluster, kNoClusterLimit, &root_clusters);
      root.end != ChainEnd::kEnd) {
    report_->broken_root = true;
    chain_ends_.push_back(root.last);
  }
  if (DiskStatus status = CheckFiles(root_clusters);
      status != DiskStatus::kOk) {
    return status;
  }
  last_chain_walk_ = walk_count_;
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

DiskStatus DiskChecker::Repair() {
  // Entries first, so a crash leaves no size past its chain's new end, and no
  // file reaching a cluster rebuilding frees.
  for (DirSectorRepair& repair : dir_repairs_) {
    for (int index : repair.bad_names) {
      DiskDirEntry entry = DiskDirEntry::FromSector(repair.words, index);
      entry.SetName(TakeFoundName());
      entry.CopyToSector(index, absl::MakeSpan(repair.words));
    }
    if (DiskStatus status = image_->WriteSector(repair.sector, repair.words);
        status != DiskStatus::kOk) {
      return status;
    }
  }

  // Then the chains' new ends, and emptying any free list to rebuild, so that
  // nothing reaches the clusters rebuilding links.
  std::vector<uint16_t> fat = fat_;
  for (int cluster : chain_ends_) {
    fat[cluster] = DiskLayout::kFatEnd;
  }
  const bool rebuild =
      report_->broken_free_list || report_->orphaned_clusters > 0;
  if (rebuild) {
    fat[0] = DiskLayout::kFatEnd;
  }
  if (DiskStatus status = WriteFat(fat); status != DiskStatus::kOk) {
    return status;
  }
  if (!rebuild) {
    return DiskStatus::kOk;
  }

  // Then the free list, of every cluster no chain claimed, in order, keeping
  // each word that already links to the right cluster.
  const auto link = [&](int cluster, int next) {
    if (layout_.GetNextCluster(cluster, fat[cluster]) != next) {
      fat[cluster] = GetFreeLink(layout_, cluster, next);
    }
  };
  int last = 0;
  for (int cluster = 1; cluster <= layout_.cluster_count; ++cluster) {
    if (claims_[cluster] == kUnclaimed || claims_[cluster] > last_chain_walk_) {
      link(last, cluster);
      last = cluster;
    }
  }
  link(last, 0);
  return WriteFat(fat);
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
  for (int cluster : root_clusters) {
    const int first_sector = layout_.GetClusterSector(cluster);
    for (int sector = first_sector;
         sector < first_sector + layout_.sectors_per_cluster; ++sector) {
      DirSectorRepair repair = {.sector = sector};
      if (DiskStatus status =
              image_->ReadSector(sector, absl::MakeSpan(repair.words));
          status != DiskStatus::kOk) {
        return status;
      }
      bool repaired = false;
      for (int index = 0; index < DiskDirEntry::kEntriesPerSector; ++index) {
        const DiskDirEntry entry =
            DiskDirEntry::FromSector(repair.words, index);
        // Freeing an entry zeroes it all, so one with only its name zeroed is
        // a file with a bad name, which may still have a chain.
        if (entry == DiskDirEntry()) {
          continue;
        }
        // Repairing renames bad names once every other name is known.
        if (!entry.GetName().has_value()) {
          repair.bad_names.push_back(index);
        }
        if (const DiskDirEntry repaired_entry = CheckFile(entry);
            repaired_entry != entry) {
          repaired_entry.CopyToSector(index, absl::MakeSpan(repair.words));
          repaired = true;
        }
      }
      if (repaired || !repair.bad_names.empty()) {
        dir_repairs_.push_back(repair);
      }
    }
  }
  return DiskStatus::kOk;
}

DiskDirEntry DiskChecker::CheckFile(const DiskDirEntry& entry) {
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
  const ChainWalk walk = ClaimChain(entry.GetFirstCluster(), needed);
  const std::optional<ChainProblem> problem = GetChainProblem(walk, needed);
  if (!problem.has_value()) {
    return entry;
  }
  report_->file_problems.push_back({.entry = entry, .problem = *problem});

  // Repairing cuts the chain and the size to what both cover.
  DiskDirEntry repaired = entry;
  if (walk.count == 0) {
    repaired.SetFirstCluster(0);
  } else if (walk.end != ChainEnd::kEnd) {
    chain_ends_.push_back(walk.last);
  }
  repaired.SetSize(std::min(size.value_or(DiskDirEntry::kMaxSize),
                            walk.count * cluster_words));
  return repaired;
}

ChainWalk DiskChecker::ClaimChain(int first, int max_count,
                                  std::vector<int>* clusters) {
  // GetNextCluster checks every link, but not the first cluster.
  if (first > layout_.cluster_count) {
    return {.end = ChainEnd::kBroken};
  }
  const int walk = ++walk_count_;
  int count = 0;
  int last = 0;
  for (int cluster = first; cluster != 0;) {
    if (count == max_count) {
      return {.end = ChainEnd::kContinues, .count = count, .last = last};
    }
    if (claims_[cluster] != kUnclaimed) {
      // Back into this walk is a loop.
      return {.end = claims_[cluster] == walk ? ChainEnd::kBroken
                                              : ChainEnd::kClaimed,
              .count = count,
              .last = last};
    }
    claims_[cluster] = walk;
    ++count;
    last = cluster;
    if (clusters != nullptr) {
      clusters->push_back(cluster);
    }
    const std::optional<int> next =
        layout_.GetNextCluster(cluster, fat_[cluster]);
    if (!next.has_value()) {
      return {.end = ChainEnd::kBroken, .count = count, .last = last};
    }
    cluster = *next;
  }
  return {.end = ChainEnd::kEnd, .count = count, .last = last};
}

DiskFilename DiskChecker::TakeFoundName() {
  // There are more found names than entries, so this ends.
  while (true) {
    const DiskFilename name = *DiskFilename::Parse(absl::StrCat(
        "FILE", absl::Dec(found_number_++, absl::kZeroPad8), ".CHK"));
    if (name_counts_.try_emplace(name, 1).second) {
      return name;
    }
  }
}

DiskStatus DiskChecker::WriteFat(absl::Span<const uint16_t> fat) {
  const auto write = [&](int fat_sector) {
    const int first = fat_sector * kDiskSectorWords;
    const absl::Span<const uint16_t> words =
        fat.subspan(first, kDiskSectorWords);
    if (words == absl::MakeConstSpan(fat_).subspan(first, kDiskSectorWords)) {
      return DiskStatus::kOk;
    }
    std::copy(words.begin(), words.end(), fat_.begin() + first);
    return image_->WriteSector(DiskLayout::kFirstFatSector + fat_sector, words);
  };
  for (int fat_sector = 1; fat_sector < layout_.fat_sectors; ++fat_sector) {
    if (DiskStatus status = write(fat_sector); status != DiskStatus::kOk) {
      return status;
    }
  }
  return write(0);
}

// Checks `image` into `report`, and then repairs it if `mode` is kRepair.
DiskStatus CheckImage(DiskImage& image, CheckMode mode,
                      DiskCheckReport* report) {
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
  DiskChecker checker(&image, superblock.GetLayout(), report);
  if (DiskStatus status = checker.Check(superblock.GetRootCluster());
      status != DiskStatus::kOk || mode == CheckMode::kCheck) {
    return status;
  }
  return checker.Repair();
}

}  // namespace

DiskStatus CheckDisk(DiskImage& image, DiskCheckReport* report) {
  return CheckImage(image, CheckMode::kCheck, report);
}

DiskStatus RepairDisk(DiskImage& image, DiskCheckReport* report) {
  return CheckImage(image, CheckMode::kRepair, report);
}

}  // namespace oz3
