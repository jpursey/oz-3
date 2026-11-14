// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_check.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/date_time.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/disk_superblock.h"
#include "oz3/disk/faulty_file_protocol.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;

using ChainProblem = DiskCheckReport::ChainProblem;
using FileProblem = DiskCheckReport::FileProblem;
using Sector = std::array<uint16_t, kDiskSectorWords>;

constexpr char kPath[] = "mem:/disk.img";

// A disk of 20 clusters of two sectors, after 1 FAT sector.
constexpr DiskLayout kLayout = {
    .sectors_per_cluster = 2,
    .cluster_count = 20,
    .fat_sectors = 1,
};

constexpr int kClusterWords = kLayout.GetClusterWords();

// The directory entries a cluster holds.
constexpr int kEntriesPerCluster =
    kLayout.sectors_per_cluster * DiskDirEntry::kEntriesPerSector;

DiskFilename GetName(std::string_view text) {
  return *DiskFilename::Parse(text);
}

// Returns the clusters from `first` to the last, in order.
std::vector<int> GetClustersFrom(int first) {
  std::vector<int> clusters;
  for (int cluster = first; cluster <= kLayout.cluster_count; ++cluster) {
    clusters.push_back(cluster);
  }
  return clusters;
}

// Returns a directory entry for the file `name`.
DiskDirEntry MakeEntry(std::string_view name, int first_cluster, int size) {
  DiskDirEntry entry;
  entry.SetName(GetName(name));
  entry.SetFirstCluster(first_cluster);
  entry.SetSize(size);
  return entry;
}

// Builds a disk of kLayout in memory, writes it to a new image, and checks it.
// It starts with the root in cluster 1, holding no files, and an empty free
// list.
class DiskCheckTest : public ::testing::Test {
 protected:
  DiskCheckTest() {
    std::unique_ptr<FaultyFileProtocol> protocol =
        std::make_unique<FaultyFileProtocol>();
    protocol_ = protocol.get();
    host_.Register(std::move(protocol));
    SetFreeList({});
    SetRootChain({1});
  }

  // Links `clusters` into a chain in the FAT, ending after the last.
  void SetChain(absl::Span<const int> clusters) {
    const int count = static_cast<int>(clusters.size());
    for (int i = 0; i < count; ++i) {
      fat_[clusters[i]] = i + 1 < count ? static_cast<uint16_t>(clusters[i + 1])
                                        : DiskLayout::kFatEnd;
      chained_[clusters[i]] = true;
    }
  }

  // Sets the root's chain to `clusters`, whose first is the superblock's root
  // cluster.
  void SetRootChain(std::vector<int> clusters) {
    SetChain(clusters);
    root_chain_ = std::move(clusters);
  }

  // Sets the free list to `clusters`, in order.
  void SetFreeList(absl::Span<const int> clusters) {
    fat_[0] = clusters.empty() ? DiskLayout::kFatEnd
                               : static_cast<uint16_t>(clusters[0]);
    SetChain(clusters);
  }

  // Sets the free list to every cluster no chain set so far has, in order.
  void FreeUnchained() {
    std::vector<int> clusters;
    for (int cluster = 1; cluster <= kLayout.cluster_count; ++cluster) {
      if (!chained_[cluster]) {
        clusters.push_back(cluster);
      }
    }
    SetFreeList(clusters);
  }

  // Adds the file `name` as the root's entry `index`, of `size` words in
  // `clusters`, and returns its entry.
  DiskDirEntry AddFile(int index, std::string_view name,
                       std::vector<int> clusters, int size) {
    SetChain(clusters);
    const DiskDirEntry entry =
        MakeEntry(name, clusters.empty() ? 0 : clusters[0], size);
    root_entries_[index] = entry;
    return entry;
  }

  // Replaces the root's entry `index`'s words from `word` with `words`, for
  // entries no setter makes, and returns the entry.
  DiskDirEntry PatchRootEntry(int index, int word,
                              absl::Span<const uint16_t> words) {
    DiskDirEntry::Words patched = root_entries_[index].GetWords();
    std::copy(words.begin(), words.end(), patched.begin() + word);
    root_entries_[index] = DiskDirEntry::FromWords(patched);
    return root_entries_[index];
  }

  // Writes the disk to a new image at kPath: its superblock, its FAT, and its
  // root's entries, in the root's chain.
  void MakeImage() {
    image_.reset();  // Closes any image already at kPath.
    protocol_->SetLimits({});
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&host_, kPath, kLayout.GetSectorCount());
    ASSERT_TRUE(image.ok()) << image.status();
    image_ = *std::move(image);
    const DiskSuperblock superblock(kLayout, root_chain_[0],
                                    *DiskLabel::Parse("WORK"), DateTime());
    ASSERT_EQ(image_->WriteSector(DiskLayout::kSuperblockSector,
                                  superblock.GetWords()),
              DiskStatus::kOk);
    ASSERT_EQ(image_->WriteSector(DiskLayout::kFirstFatSector, fat_),
              DiskStatus::kOk);
    std::vector<uint16_t> root(root_chain_.size() * kClusterWords);
    for (const auto& [index, entry] : root_entries_) {
      std::copy(entry.GetWords().begin(), entry.GetWords().end(),
                root.begin() + index * DiskDirEntry::kWordCount);
    }
    const absl::Span<const uint16_t> root_words = root;
    for (int i = 0; i < static_cast<int>(root_chain_.size()); ++i) {
      for (int j = 0; j < kLayout.sectors_per_cluster; ++j) {
        ASSERT_EQ(image_->WriteSector(
                      kLayout.GetClusterSector(root_chain_[i]) + j,
                      root_words.subspan((i * kLayout.sectors_per_cluster + j) *
                                             kDiskSectorWords,
                                         kDiskSectorWords)),
                  DiskStatus::kOk);
      }
    }
    protocol_->CollectWriteOffsets();
  }

  // Checks the image, expecting nothing written.
  DiskStatus CheckImage(DiskCheckReport* report) {
    const DiskStatus status = CheckDisk(*image_, report);
    EXPECT_THAT(protocol_->CollectWriteOffsets(), IsEmpty());
    return status;
  }

  // Writes the disk to a new image and checks it, expecting kOk, and returns
  // the report.
  DiskCheckReport Check() {
    MakeImage();
    DiskCheckReport report;
    EXPECT_EQ(CheckImage(&report), DiskStatus::kOk);
    return report;
  }

  gb::FileSystem host_;
  FaultyFileProtocol* protocol_ = nullptr;
  std::unique_ptr<DiskImage> image_;
  Sector fat_ = {};
  std::array<bool, kLayout.cluster_count + 1> chained_ = {};
  std::vector<int> root_chain_;
  std::map<int, DiskDirEntry> root_entries_;  // By index.
};

TEST_F(DiskCheckTest, ACleanDiskReportsNothing) {
  SetRootChain({1, 6});
  AddFile(0, "EMPTY", {}, 0);
  AddFile(1, "WHOLE", {3}, kClusterWords);
  AddFile(2, "PART", {5, 2, 7}, 2 * kClusterWords + 1);
  // The first entry of the root's second cluster.
  AddFile(kEntriesPerCluster, "LATER", {4}, 1);
  FreeUnchained();
  EXPECT_EQ(Check(), DiskCheckReport());
}

TEST_F(DiskCheckTest, AFullDiskReportsNothing) {
  AddFile(0, "BIG", GetClustersFrom(2), kLayout.GetMaxFileSize());
  EXPECT_EQ(Check(), DiskCheckReport());
}

TEST_F(DiskCheckTest, AnUnformattedImageIsUnformatted) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&host_, kPath, kLayout.GetSectorCount());
  ASSERT_TRUE(image.ok()) << image.status();
  image_ = *std::move(image);
  protocol_->CollectWriteOffsets();
  DiskCheckReport report = {.broken_root = true, .orphaned_clusters = 3};
  EXPECT_EQ(CheckImage(&report), DiskStatus::kUnformatted);
  EXPECT_EQ(report, DiskCheckReport());
}

TEST_F(DiskCheckTest, HostFailuresArePassedOn) {
  SetRootChain({1, 2});
  FreeUnchained();
  // The superblock, the FAT sector, and the root's four sectors.
  constexpr int kReads = 6;
  for (int reads = 0; reads < kReads; ++reads) {
    SCOPED_TRACE(reads);
    MakeImage();
    protocol_->SetLimits({.reads = reads});
    DiskCheckReport report;
    EXPECT_EQ(CheckImage(&report), DiskStatus::kIoError);
  }
  MakeImage();
  protocol_->SetLimits({.reads = kReads});
  DiskCheckReport report;
  EXPECT_EQ(CheckImage(&report), DiskStatus::kOk);
  EXPECT_EQ(report, DiskCheckReport());
}

TEST_F(DiskCheckTest, ALongChainsTailIsOrphaned) {
  const DiskDirEntry entry = AddFile(0, "LONG", {3, 4, 5}, kClusterWords);
  FreeUnchained();
  EXPECT_EQ(
      Check(),
      (DiskCheckReport{
          .orphaned_clusters = 2,
          .file_problems = {{.entry = entry, .problem = ChainProblem::kLong}},
      }));
}

TEST_F(DiskCheckTest, AnEmptyFileWithAChainIsLong) {
  const DiskDirEntry entry = AddFile(0, "EMPTY", {3}, 0);
  FreeUnchained();
  EXPECT_EQ(
      Check(),
      (DiskCheckReport{
          .orphaned_clusters = 1,
          .file_problems = {{.entry = entry, .problem = ChainProblem::kLong}},
      }));
}

TEST_F(DiskCheckTest, ALoopPastTheSizeIsOnlyLong) {
  const DiskDirEntry entry = AddFile(0, "LOOP", {3, 4}, kClusterWords);
  fat_[4] = 3;
  FreeUnchained();
  EXPECT_EQ(
      Check(),
      (DiskCheckReport{
          .orphaned_clusters = 1,
          .file_problems = {{.entry = entry, .problem = ChainProblem::kLong}},
      }));
}

TEST_F(DiskCheckTest, AShortChainIsShort) {
  const DiskDirEntry entry = AddFile(0, "SHORT", {3, 4}, 2 * kClusterWords + 1);
  FreeUnchained();
  EXPECT_THAT(Check().file_problems,
              ElementsAre(FileProblem{.entry = entry,
                                      .problem = ChainProblem::kShort}));
}

TEST_F(DiskCheckTest, ASizePastTheLargestFileIsShort) {
  const DiskDirEntry entry =
      AddFile(0, "BIG", GetClustersFrom(2), kLayout.GetMaxFileSize() + 1);
  EXPECT_EQ(
      Check(),
      (DiskCheckReport{
          .file_problems = {{.entry = entry, .problem = ChainProblem::kShort}},
      }));
}

TEST_F(DiskCheckTest, ASizeNoDiskHoldsIsShort) {
  // A size of FFFFFFFFh, past DiskDirEntry::kMaxSize.
  AddFile(0, "HUGE", {3}, 0);
  const DiskDirEntry entry = PatchRootEntry(0, 10, {{0xFFFF, 0xFFFF}});
  ASSERT_EQ(entry.GetSize(), std::nullopt);
  FreeUnchained();
  EXPECT_EQ(
      Check(),
      (DiskCheckReport{
          .file_problems = {{.entry = entry, .problem = ChainProblem::kShort}},
      }));
}

TEST_F(DiskCheckTest, ALinkOutOfRangeIsBroken) {
  const DiskDirEntry entry = AddFile(0, "BROKEN", {3, 4}, 3 * kClusterWords);
  fat_[4] = kLayout.cluster_count + 1;
  FreeUnchained();
  EXPECT_EQ(
      Check(),
      (DiskCheckReport{
          .file_problems = {{.entry = entry, .problem = ChainProblem::kBroken}},
      }));
}

TEST_F(DiskCheckTest, ALinkOutOfRangeAtTheSizeIsBroken) {
  const DiskDirEntry entry = AddFile(0, "BROKEN", {3}, kClusterWords);
  fat_[3] = kLayout.cluster_count + 1;
  FreeUnchained();
  EXPECT_EQ(
      Check(),
      (DiskCheckReport{
          .file_problems = {{.entry = entry, .problem = ChainProblem::kBroken}},
      }));
}

TEST_F(DiskCheckTest, AFirstClusterOutOfRangeIsBroken) {
  // Out of range even for an empty file, whose chain it otherwise makes long.
  const DiskDirEntry entry = MakeEntry("BROKEN", kLayout.cluster_count + 1, 1);
  const DiskDirEntry empty =
      MakeEntry("EMPTY", DiskDirEntry::kMaxFirstCluster, 0);
  root_entries_[0] = entry;
  root_entries_[1] = empty;
  FreeUnchained();
  EXPECT_EQ(
      Check(),
      (DiskCheckReport{
          .file_problems = {{.entry = entry, .problem = ChainProblem::kBroken},
                            {.entry = empty, .problem = ChainProblem::kBroken}},
      }));
}

TEST_F(DiskCheckTest, AFileLoopIsBroken) {
  const DiskDirEntry entry = AddFile(0, "LOOP", {3, 4}, 3 * kClusterWords);
  fat_[4] = 3;
  FreeUnchained();
  EXPECT_EQ(
      Check(),
      (DiskCheckReport{
          .file_problems = {{.entry = entry, .problem = ChainProblem::kBroken}},
      }));
}

TEST_F(DiskCheckTest, ACrossLinkIsTheLaterFilesProblem) {
  AddFile(0, "FIRST", {3, 4, 5}, 3 * kClusterWords);
  const DiskDirEntry later = AddFile(1, "LATER", {6, 4}, 2 * kClusterWords);
  fat_[4] = 5;  // Back to FIRST's link, which LATER's chain overwrote.
  FreeUnchained();
  EXPECT_EQ(Check(),
            (DiskCheckReport{
                .file_problems = {{.entry = later,
                                   .problem = ChainProblem::kCrossLinked}},
            }));
}

TEST_F(DiskCheckTest, AFileRunningIntoTheRootIsCrossLinked) {
  const DiskDirEntry entry = AddFile(0, "LINKED", {3, 1}, 2 * kClusterWords);
  FreeUnchained();
  EXPECT_EQ(Check(),
            (DiskCheckReport{
                .file_problems = {{.entry = entry,
                                   .problem = ChainProblem::kCrossLinked}},
            }));
}

TEST_F(DiskCheckTest, ABrokenRootIsReadUpToTheBreak) {
  SetRootChain({1, 2});
  fat_[2] = kLayout.cluster_count + 1;
  // In the root's last cluster, so it is read.
  const DiskDirEntry entry =
      AddFile(kEntriesPerCluster, "LONG", {3, 4}, kClusterWords);
  FreeUnchained();
  EXPECT_EQ(
      Check(),
      (DiskCheckReport{
          .broken_root = true,
          .orphaned_clusters = 1,
          .file_problems = {{.entry = entry, .problem = ChainProblem::kLong}},
      }));
}

TEST_F(DiskCheckTest, ARootLoopIsBroken) {
  SetRootChain({1, 2});
  fat_[2] = 1;
  const DiskDirEntry entry =
      AddFile(kEntriesPerCluster, "LONG", {3, 4}, kClusterWords);
  FreeUnchained();
  EXPECT_EQ(
      Check(),
      (DiskCheckReport{
          .broken_root = true,
          .orphaned_clusters = 1,
          .file_problems = {{.entry = entry, .problem = ChainProblem::kLong}},
      }));
}

TEST_F(DiskCheckTest, AFreeListLoopIsBroken) {
  FreeUnchained();
  fat_[kLayout.cluster_count] = 10;
  EXPECT_EQ(Check(), DiskCheckReport{.broken_free_list = true});
}

TEST_F(DiskCheckTest, AFreeListLinkOutOfRangeIsBroken) {
  FreeUnchained();
  fat_[kLayout.cluster_count] = kLayout.cluster_count + 1;
  EXPECT_EQ(Check(), DiskCheckReport{.broken_free_list = true});
}

TEST_F(DiskCheckTest, AFreeListHeadOutOfRangeOrphansTheList) {
  FreeUnchained();
  fat_[0] = kLayout.cluster_count + 1;
  EXPECT_EQ(Check(), (DiskCheckReport{
                         .broken_free_list = true,
                         .orphaned_clusters = kLayout.cluster_count - 1,
                     }));
}

TEST_F(DiskCheckTest, AClusterBothFreeAndInAChainStaysInTheChain) {
  // The free list runs from 2 into FILE's cluster 3, which ends both, so the
  // clusters it held after 3 are on no chain.
  SetFreeList(GetClustersFrom(2));
  AddFile(0, "FILE", {3}, 1);
  EXPECT_EQ(Check(), (DiskCheckReport{
                         .broken_free_list = true,
                         .orphaned_clusters = kLayout.cluster_count - 3,
                     }));
}

TEST_F(DiskCheckTest, ClustersOnNoChainAreOrphaned) {
  std::vector<int> free_clusters = GetClustersFrom(2);
  std::erase(free_clusters, 7);
  std::erase(free_clusters, 9);
  SetFreeList(free_clusters);
  EXPECT_EQ(Check(), DiskCheckReport{.orphaned_clusters = 2});
}

TEST_F(DiskCheckTest, BadNamesAreReportedAndWalked) {
  // "A." packed, as a dot is no name character.
  AddFile(0, "BAD", {3}, 1);
  const DiskDirEntry entry = PatchRootEntry(0, 0, {{'A' | ('.' << 8)}});
  ASSERT_EQ(entry.GetName(), std::nullopt);
  FreeUnchained();
  EXPECT_EQ(Check(), DiskCheckReport{.bad_names = {entry}});
}

TEST_F(DiskCheckTest, AZeroedNameIsABadName) {
  // Not free, as freeing an entry zeroes every word, so its chain is walked.
  AddFile(0, "GONE", {3, 4}, kClusterWords + 1);
  const DiskDirEntry gone = PatchRootEntry(0, 0, DiskFilename::Words());
  // Only its attributes are left, so it has no chain.
  root_entries_[1].SetAttributes(kDiskAttribHidden);
  const DiskDirEntry hidden = root_entries_[1];
  ASSERT_TRUE(gone.IsFree());
  ASSERT_TRUE(hidden.IsFree());
  FreeUnchained();
  EXPECT_EQ(Check(), (DiskCheckReport{.bad_names = {gone, hidden}}));
}

TEST_F(DiskCheckTest, DuplicateNamesAreReportedOnce) {
  AddFile(0, "A", {}, 0);
  AddFile(1, "B", {}, 0);
  AddFile(2, "C", {}, 0);
  AddFile(3, "B", {}, 0);
  AddFile(4, "A", {}, 0);
  AddFile(5, "B", {}, 0);
  FreeUnchained();
  EXPECT_EQ(Check(), (DiskCheckReport{
                         .duplicate_names = {GetName("B"), GetName("A")},
                     }));
}

}  // namespace
}  // namespace oz3
