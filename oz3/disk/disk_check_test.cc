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

// Where an image's sectors are.
constexpr int64_t kHeaderBytes = 16;
constexpr int64_t kSectorBytes = kDiskSectorWords * 2;

// A disk of 300 clusters of two sectors, after 2 FAT sectors.
constexpr DiskLayout kLayout = {
    .sectors_per_cluster = 2,
    .cluster_count = 300,
    .fat_sectors = 2,
};

constexpr int kClusterWords = kLayout.GetClusterWords();

// The directory entries a cluster holds.
constexpr int kEntriesPerCluster =
    kLayout.sectors_per_cluster * DiskDirEntry::kEntriesPerSector;

// The first cluster whose link is in the FAT's second sector.
constexpr int kSecondFatSectorCluster = kDiskSectorWords;

// The FAT sectors.
constexpr int kFirstFatSector = DiskLayout::kFirstFatSector;
constexpr int kSecondFatSector = DiskLayout::kFirstFatSector + 1;

using Fat = std::array<uint16_t, kLayout.fat_sectors * kDiskSectorWords>;

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

  // Sets the free list to every cluster no chain set so far has, in order,
  // but `orphans`.
  void FreeUnchained(absl::Span<const int> orphans = {}) {
    std::vector<int> clusters;
    for (int cluster = 1; cluster <= kLayout.cluster_count; ++cluster) {
      if (!chained_[cluster] &&
          std::find(orphans.begin(), orphans.end(), cluster) == orphans.end()) {
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
    for (int i = 0; i < kLayout.fat_sectors; ++i) {
      ASSERT_EQ(
          image_->WriteSector(kFirstFatSector + i,
                              absl::MakeConstSpan(fat_).subspan(
                                  i * kDiskSectorWords, kDiskSectorWords)),
          DiskStatus::kOk);
    }
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

  // Returns the sectors written since the last call, in order, or -1 for the
  // header.
  std::vector<int> CollectSectorWrites() {
    std::vector<int> sectors;
    for (int64_t offset : protocol_->CollectWriteOffsets()) {
      sectors.push_back(
          offset < kHeaderBytes
              ? -1
              : static_cast<int>((offset - kHeaderBytes) / kSectorBytes));
    }
    return sectors;
  }

  // Writes the disk to a new image and repairs it, expecting kOk and the
  // report a check gives, and then a check finding only the same duplicate
  // names. Returns the report, and keeps the sectors written in
  // repair_writes_.
  DiskCheckReport Repair() {
    const DiskCheckReport checked = Check();
    DiskCheckReport report;
    EXPECT_EQ(RepairDisk(*image_, &report), DiskStatus::kOk);
    EXPECT_EQ(report, checked);
    repair_writes_ = CollectSectorWrites();
    DiskCheckReport repaired;
    EXPECT_EQ(CheckImage(&repaired), DiskStatus::kOk);
    EXPECT_EQ(repaired,
              DiskCheckReport{.duplicate_names = report.duplicate_names});
    return report;
  }

  // Reopens the image at kPath with no limits, as after a crash.
  void ReopenImage() {
    image_.reset();
    protocol_->SetLimits({});
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&host_, kPath);
    ASSERT_TRUE(image.ok()) << image.status();
    image_ = *std::move(image);
  }

  // Returns the image's FAT.
  Fat ReadFat() {
    Fat fat = {};
    for (int i = 0; i < kLayout.fat_sectors; ++i) {
      EXPECT_EQ(image_->ReadSector(kFirstFatSector + i,
                                   absl::MakeSpan(fat).subspan(
                                       i * kDiskSectorWords, kDiskSectorWords)),
                DiskStatus::kOk);
    }
    return fat;
  }

  // Returns the image's root entry `index`, in the root's chain.
  DiskDirEntry ReadRootEntry(int index) {
    const int sector =
        kLayout.GetClusterSector(root_chain_[index / kEntriesPerCluster]) +
        index % kEntriesPerCluster / DiskDirEntry::kEntriesPerSector;
    Sector words;
    EXPECT_EQ(image_->ReadSector(sector, absl::MakeSpan(words)),
              DiskStatus::kOk);
    return DiskDirEntry::FromWords(absl::MakeConstSpan(words).subspan(
        index % DiskDirEntry::kEntriesPerSector * DiskDirEntry::kWordCount,
        DiskDirEntry::kWordCount));
  }

  // Returns the image's chain from `first`, stopping at a link out of range,
  // or after as many clusters as the disk has.
  std::vector<int> ReadChain(int first) {
    const Fat fat = ReadFat();
    std::vector<int> chain;
    for (std::optional<int> cluster = first;
         cluster.has_value() && *cluster != 0 &&
         *cluster <= kLayout.cluster_count &&
         static_cast<int>(chain.size()) < kLayout.cluster_count;
         cluster = kLayout.GetNextCluster(*cluster, fat[*cluster])) {
      chain.push_back(*cluster);
    }
    return chain;
  }

  // Returns the image's free list.
  std::vector<int> ReadFreeList() {
    return ReadChain(kLayout.GetNextCluster(0, ReadFat()[0]).value_or(0));
  }

  // Expects no cluster on the image to be on two chains, or twice on one,
  // taking each chain of the root, its files, and the free list whole. So a
  // long chain's tail counts, which CheckDisk leaves unclaimed.
  void ExpectNoSharedClusters() {
    std::vector<std::vector<int>> chains = {ReadChain(root_chain_[0]),
                                            ReadFreeList()};
    for (const auto& [index, entry] : root_entries_) {
      chains.push_back(ReadChain(ReadRootEntry(index).GetFirstCluster()));
    }
    std::vector<int> counts(kLayout.cluster_count + 1);
    for (const std::vector<int>& chain : chains) {
      for (int cluster : chain) {
        EXPECT_EQ(++counts[cluster], 1) << "Cluster " << cluster;
      }
    }
  }

  gb::FileSystem host_;
  FaultyFileProtocol* protocol_ = nullptr;
  std::unique_ptr<DiskImage> image_;
  Fat fat_ = {};
  std::array<bool, kLayout.cluster_count + 1> chained_ = {};
  std::vector<int> root_chain_;
  std::map<int, DiskDirEntry> root_entries_;  // By index.
  std::vector<int> repair_writes_;
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
  // The superblock, the FAT's two sectors, and the root's four sectors.
  constexpr int kReads = 7;
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
  const DiskDirEntry entry = PatchRootEntry(0, 0, {'A' | ('.' << 8)});
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

TEST_F(DiskCheckTest, RepairingACleanDiskWritesNothing) {
  SetRootChain({1, 6});
  AddFile(0, "EMPTY", {}, 0);
  AddFile(1, "PART", {5, 2, 7}, 2 * kClusterWords + 1);
  FreeUnchained();
  EXPECT_EQ(Repair(), DiskCheckReport());
  EXPECT_THAT(repair_writes_, IsEmpty());
}

TEST_F(DiskCheckTest, RepairingCutsALongChain) {
  const DiskDirEntry entry = AddFile(0, "LONG", {3, 4, 5}, kClusterWords);
  FreeUnchained();
  Repair();
  EXPECT_EQ(ReadRootEntry(0), entry);
  EXPECT_THAT(ReadChain(3), ElementsAre(3));
}

TEST_F(DiskCheckTest, RepairingAnEmptyFileDropsItsChain) {
  DiskDirEntry entry = AddFile(0, "EMPTY", {3, 4}, 0);
  FreeUnchained();
  Repair();
  entry.SetFirstCluster(0);
  EXPECT_EQ(ReadRootEntry(0), entry);
}

TEST_F(DiskCheckTest, RepairingAShortChainCutsTheSize) {
  DiskDirEntry entry = AddFile(0, "SHORT", {3, 4}, 2 * kClusterWords + 1);
  FreeUnchained();
  Repair();
  entry.SetSize(2 * kClusterWords);
  EXPECT_EQ(ReadRootEntry(0), entry);
  EXPECT_THAT(ReadChain(3), ElementsAre(3, 4));
  // Only the entry changes.
  EXPECT_THAT(repair_writes_, ElementsAre(kLayout.GetClusterSector(1)));
}

TEST_F(DiskCheckTest, RepairingASizeNoDiskHoldsCutsIt) {
  AddFile(0, "HUGE", {3, 4}, 0);
  DiskDirEntry entry = PatchRootEntry(0, 10, {{0xFFFF, 0xFFFF}});
  FreeUnchained();
  Repair();
  entry.SetSize(2 * kClusterWords);
  EXPECT_EQ(ReadRootEntry(0), entry);
}

TEST_F(DiskCheckTest, RepairingABrokenLinkEndsTheChainThere) {
  DiskDirEntry broken = AddFile(0, "BROKEN", {3, 4}, 3 * kClusterWords);
  fat_[4] = kLayout.cluster_count + 1;
  const DiskDirEntry at_size = AddFile(1, "ATSIZE", {6}, kClusterWords);
  fat_[6] = kLayout.cluster_count + 1;
  FreeUnchained();
  Repair();
  broken.SetSize(2 * kClusterWords);
  EXPECT_EQ(ReadRootEntry(0), broken);
  EXPECT_THAT(ReadChain(3), ElementsAre(3, 4));
  EXPECT_EQ(ReadRootEntry(1), at_size);
  EXPECT_THAT(ReadChain(6), ElementsAre(6));
}

TEST_F(DiskCheckTest, RepairingAFirstClusterOutOfRangeEmptiesTheFile) {
  DiskDirEntry broken = MakeEntry("BROKEN", kLayout.cluster_count + 1, 1);
  DiskDirEntry empty = MakeEntry("EMPTY", DiskDirEntry::kMaxFirstCluster, 0);
  root_entries_[0] = broken;
  root_entries_[1] = empty;
  FreeUnchained();
  Repair();
  broken.SetFirstCluster(0);
  broken.SetSize(0);
  empty.SetFirstCluster(0);
  EXPECT_EQ(ReadRootEntry(0), broken);
  EXPECT_EQ(ReadRootEntry(1), empty);
}

TEST_F(DiskCheckTest, RepairingAFileLoopEndsTheChain) {
  DiskDirEntry entry = AddFile(0, "LOOP", {3, 4}, 3 * kClusterWords);
  fat_[4] = 3;
  FreeUnchained();
  Repair();
  entry.SetSize(2 * kClusterWords);
  EXPECT_EQ(ReadRootEntry(0), entry);
  EXPECT_THAT(ReadChain(3), ElementsAre(3, 4));
}

TEST_F(DiskCheckTest, RepairingACrossLinkCutsTheLaterFile) {
  const DiskDirEntry first = AddFile(0, "FIRST", {3, 4, 5}, 3 * kClusterWords);
  DiskDirEntry later = AddFile(1, "LATER", {6, 4}, 2 * kClusterWords);
  fat_[4] = 5;
  FreeUnchained();
  Repair();
  EXPECT_EQ(ReadRootEntry(0), first);
  EXPECT_THAT(ReadChain(3), ElementsAre(3, 4, 5));
  later.SetSize(kClusterWords);
  EXPECT_EQ(ReadRootEntry(1), later);
  EXPECT_THAT(ReadChain(6), ElementsAre(6));
}

TEST_F(DiskCheckTest, RepairingABrokenRootEndsItAtTheBreak) {
  SetRootChain({1, 2});
  fat_[2] = kLayout.cluster_count + 1;
  const DiskDirEntry entry =
      AddFile(kEntriesPerCluster, "LONG", {3, 4}, kClusterWords);
  FreeUnchained();
  Repair();
  EXPECT_THAT(ReadChain(1), ElementsAre(1, 2));
  EXPECT_EQ(ReadRootEntry(kEntriesPerCluster), entry);
  EXPECT_THAT(ReadChain(3), ElementsAre(3));
}

TEST_F(DiskCheckTest, RepairingARootLoopEndsIt) {
  SetRootChain({1, 2});
  fat_[2] = 1;
  FreeUnchained();
  Repair();
  EXPECT_THAT(ReadChain(1), ElementsAre(1, 2));
}

TEST_F(DiskCheckTest, RepairingRebuildsABrokenFreeList) {
  const auto repair = [&](int index, uint16_t link) {
    SCOPED_TRACE(index);
    FreeUnchained();
    fat_[index] = link;
    Repair();
    EXPECT_EQ(ReadFreeList(), GetClustersFrom(2));
  };
  repair(kLayout.cluster_count, 10);                         // A loop.
  repair(kLayout.cluster_count, kLayout.cluster_count + 1);  // A broken link.
  repair(0, kLayout.cluster_count + 1);                      // A broken head.
}

TEST_F(DiskCheckTest, RepairingTakesAClusterBothFreeAndInAChainOffTheFreeList) {
  SetFreeList(GetClustersFrom(2));
  AddFile(0, "FILE", {3}, 1);
  Repair();
  std::vector<int> free_clusters = GetClustersFrom(2);
  std::erase(free_clusters, 3);
  EXPECT_EQ(ReadFreeList(), free_clusters);
  EXPECT_THAT(ReadChain(3), ElementsAre(3));
}

TEST_F(DiskCheckTest, RepairingRebuildsTheFreeListInOrder) {
  AddFile(0, "FIVE", {5}, 1);
  AddFile(1, "LAST", {kLayout.cluster_count}, 1);
  // Backward, as freeing leaves it, with 7 and 9 orphaned.
  std::vector<int> free_clusters = GetClustersFrom(2);
  std::erase(free_clusters, 5);
  std::erase(free_clusters, 7);
  std::erase(free_clusters, 9);
  std::erase(free_clusters, kLayout.cluster_count);
  std::reverse(free_clusters.begin(), free_clusters.end());
  SetFreeList(free_clusters);
  Repair();
  // A link to the next cluster is 0, as on a newly formatted disk, so only
  // the free list's head, the links around the files, and its end are not.
  Fat fat = {};
  fat[0] = 2;
  fat[1] = DiskLayout::kFatEnd;
  fat[4] = 6;
  fat[5] = DiskLayout::kFatEnd;
  fat[kLayout.cluster_count - 1] = DiskLayout::kFatEnd;
  fat[kLayout.cluster_count] = DiskLayout::kFatEnd;
  EXPECT_EQ(ReadFat(), fat);
  // The free list is emptied first, and its head is written last.
  EXPECT_THAT(repair_writes_,
              ElementsAre(kFirstFatSector, kSecondFatSector, kFirstFatSector));
}

TEST_F(DiskCheckTest, RepairingKeepsFreeLinksAlreadyInOrder) {
  // The free list's explicit links, in order, around orphaned 7.
  FreeUnchained({7});
  Repair();
  const Fat fat = ReadFat();
  EXPECT_EQ(fat[6], 0);  // Now to 7.
  EXPECT_EQ(fat[7], 0);  // Already to 8.
  EXPECT_EQ(fat[8], 9);  // Kept.
  // So only the FAT's first sector changes.
  EXPECT_THAT(repair_writes_, ElementsAre(kFirstFatSector, kFirstFatSector));
}

TEST_F(DiskCheckTest, RepairingAFullDisksFreeListEmptiesIt) {
  AddFile(0, "BIG", GetClustersFrom(2), kLayout.GetMaxFileSize());
  fat_[0] = 5;
  Repair();
  EXPECT_EQ(ReadFat()[0], DiskLayout::kFatEnd);
}

TEST_F(DiskCheckTest, RepairingRenamesBadNames) {
  AddFile(0, "FILE00000000.CHK", {}, 0);
  // A bad name with a short chain is cut too.
  AddFile(1, "BAD", {3}, 2 * kClusterWords);
  DiskDirEntry bad = PatchRootEntry(1, 0, {'A' | ('.' << 8)});
  AddFile(2, "GONE", {4, 5}, kClusterWords + 1);
  DiskDirEntry gone = PatchRootEntry(2, 0, DiskFilename::Words());
  // Taken, though it comes after the bad names.
  AddFile(3, "FILE00000002.CHK", {}, 0);
  root_entries_[4].SetAttributes(kDiskAttribHidden);
  DiskDirEntry hidden = root_entries_[4];
  FreeUnchained();
  Repair();
  bad.SetName(GetName("FILE00000001.CHK"));
  bad.SetSize(kClusterWords);
  gone.SetName(GetName("FILE00000003.CHK"));
  hidden.SetName(GetName("FILE00000004.CHK"));
  EXPECT_EQ(ReadRootEntry(1), bad);
  EXPECT_EQ(ReadRootEntry(2), gone);
  EXPECT_EQ(ReadRootEntry(4), hidden);
  EXPECT_THAT(ReadChain(4), ElementsAre(4, 5));
}

TEST_F(DiskCheckTest, RepairingKeepsDuplicateNames) {
  AddFile(0, "A", {}, 0);
  AddFile(1, "A", {}, 0);
  FreeUnchained();
  EXPECT_EQ(Repair(), DiskCheckReport{.duplicate_names = {GetName("A")}});
  EXPECT_THAT(repair_writes_, IsEmpty());
}

TEST_F(DiskCheckTest, RepairingWritesEntriesThenChainEndsThenTheFreeList) {
  SetRootChain({1, 2});
  // In the root's first sector, and in its last.
  AddFile(0, "EMPTY", {3}, 0);
  constexpr int kLastSectorIndex =
      kEntriesPerCluster + DiskDirEntry::kEntriesPerSector;
  AddFile(kLastSectorIndex, "SHORT", {4}, kClusterWords + 1);
  // Long chains, one ending in each FAT sector.
  AddFile(kLastSectorIndex + 1, "LONG1", {5, 6}, 1);
  AddFile(kLastSectorIndex + 2, "LONG2",
          {kSecondFatSectorCluster, kSecondFatSectorCluster + 1}, 1);
  FreeUnchained();
  Repair();
  EXPECT_THAT(repair_writes_,
              ElementsAre(kLayout.GetClusterSector(1),
                          kLayout.GetClusterSector(2) + 1, kSecondFatSector,
                          kFirstFatSector, kSecondFatSector, kFirstFatSector));
}

TEST_F(DiskCheckTest, RepairingAProtectedImageWritesNothing) {
  const DiskDirEntry entry = AddFile(0, "LONG", {3, 4}, kClusterWords);
  FreeUnchained();
  MakeImage();
  ASSERT_EQ(image_->SetWriteProtected(true), DiskStatus::kOk);
  CollectSectorWrites();
  DiskCheckReport report;
  EXPECT_EQ(RepairDisk(*image_, &report), DiskStatus::kWriteProtected);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  EXPECT_EQ(
      report,
      (DiskCheckReport{
          .orphaned_clusters = 1,
          .file_problems = {{.entry = entry, .problem = ChainProblem::kLong}},
      }));

  // Once repaired, it needs no writes, so protection doesn't matter.
  ASSERT_EQ(image_->SetWriteProtected(false), DiskStatus::kOk);
  ASSERT_EQ(RepairDisk(*image_, &report), DiskStatus::kOk);
  ASSERT_EQ(image_->SetWriteProtected(true), DiskStatus::kOk);
  CollectSectorWrites();
  EXPECT_EQ(RepairDisk(*image_, &report), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  EXPECT_EQ(report, DiskCheckReport());
}

TEST_F(DiskCheckTest, RepairingAnUnformattedImageWritesNothing) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&host_, kPath, kLayout.GetSectorCount());
  ASSERT_TRUE(image.ok()) << image.status();
  image_ = *std::move(image);
  CollectSectorWrites();
  DiskCheckReport report;
  EXPECT_EQ(RepairDisk(*image_, &report), DiskStatus::kUnformatted);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
}

TEST_F(DiskCheckTest, ACrashWhileRepairingLeavesWhatACrashMay) {
  // What a crash may leave: long chains, ending in each FAT sector, an empty
  // file with a chain, and orphans.
  SetRootChain({1, 2});
  AddFile(0, "EMPTY", {3}, 0);
  AddFile(1, "LONG1", {4, kSecondFatSectorCluster}, 1);
  AddFile(kEntriesPerCluster, "LONG2",
          {kSecondFatSectorCluster + 1, kSecondFatSectorCluster + 2, 5},
          kClusterWords + 1);
  FreeUnchained({7, kSecondFatSectorCluster + 10});
  MakeImage();
  DiskCheckReport report;
  ASSERT_EQ(RepairDisk(*image_, &report), DiskStatus::kOk);
  const int write_count = static_cast<int>(CollectSectorWrites().size());
  for (int writes = 0; writes <= write_count; ++writes) {
    SCOPED_TRACE(std::to_string(writes) + " of " + std::to_string(write_count) +
                 " writes");
    MakeImage();
    protocol_->SetLimits({.writes = writes});
    EXPECT_EQ(RepairDisk(*image_, &report),
              writes < write_count ? DiskStatus::kIoError : DiskStatus::kOk);
    ReopenImage();
    CollectSectorWrites();
    EXPECT_EQ(CheckImage(&report), DiskStatus::kOk);
    EXPECT_FALSE(report.broken_root);
    EXPECT_FALSE(report.broken_free_list);
    for (const FileProblem& problem : report.file_problems) {
      EXPECT_EQ(problem.problem, ChainProblem::kLong);
    }
    EXPECT_THAT(report.bad_names, IsEmpty());
    EXPECT_THAT(report.duplicate_names, IsEmpty());
    ExpectNoSharedClusters();

    // Repairing again finishes.
    EXPECT_EQ(RepairDisk(*image_, &report), DiskStatus::kOk);
    CollectSectorWrites();
    EXPECT_EQ(CheckImage(&report), DiskStatus::kOk);
    EXPECT_EQ(report, DiskCheckReport());
    EXPECT_EQ(ReadRootEntry(0).GetFirstCluster(), 0);
    EXPECT_THAT(ReadChain(4), ElementsAre(4));
    EXPECT_THAT(
        ReadChain(kSecondFatSectorCluster + 1),
        ElementsAre(kSecondFatSectorCluster + 1, kSecondFatSectorCluster + 2));
  }
}

}  // namespace
}  // namespace oz3
