// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_fat.h"

#include <array>
#include <cstdint>
#include <memory>
#include <optional>
#include <utility>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/disk/disk_hardware.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/faulty_file_protocol.h"

namespace oz3 {
namespace {

using ::testing::Each;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Optional;

using Sector = std::array<uint16_t, kDiskSectorWords>;
using WriteCaching = DiskHardware::WriteCaching;

constexpr char kPath[] = "mem:/disk.img";

// What reads take with timed settings: a sector read takes 1 second, and a
// seek 100 milliseconds.
constexpr absl::Duration kRead = absl::Seconds(1);
constexpr absl::Duration kLatency = absl::Milliseconds(100);

// Where an image's sectors are in its file, after the header.
constexpr int64_t kHeaderBytes = 16;
constexpr int64_t kSectorBytes = kDiskSectorWords * 2;

// A 360 KB floppy: 716 clusters of one sector, after 3 FAT sectors, so
// cluster 256 starts sector 2's words, and 512 sector 3's.
constexpr DiskLayout kFloppyLayout = {
    .sectors_per_cluster = 1,
    .cluster_count = 716,
    .fat_sectors = 3,
};

// A disk of 10 clusters of one sector, after 1 FAT sector.
constexpr DiskLayout kSmallLayout = {
    .sectors_per_cluster = 1,
    .cluster_count = 10,
    .fat_sectors = 1,
};

class DiskFatTest : public ::testing::Test {
 protected:
  DiskFatTest() {
    std::unique_ptr<FaultyFileProtocol> protocol =
        std::make_unique<FaultyFileProtocol>();
    protocol_ = protocol.get();
    host_.Register(std::move(protocol));
  }

  // Makes hardware_ with `settings`, attaches a new image to it for a disk of
  // `layout`, whose FAT sectors are filled with `fill`, and makes fat_ for it.
  // The host succeeds again, if a test had it fail.
  void MakeFat(const DiskLayout& layout, uint16_t fill = 0,
               const DiskHardwareSettings& settings = {}) {
    fat_.reset();
    hardware_.reset();  // Closes any image already at kPath.
    protocol_->SetLimits({});
    hardware_ = std::make_unique<DiskHardware>(settings);
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&host_, kPath, layout.GetSectorCount());
    ASSERT_TRUE(image.ok()) << image.status();
    Sector words;
    words.fill(fill);
    for (int sector = DiskLayout::kFirstFatSector; sector <= layout.fat_sectors;
         ++sector) {
      ASSERT_EQ((*image)->WriteSector(sector, words), DiskStatus::kOk);
    }
    hardware_->AttachImage(*std::move(image));
    RemakeFat(layout);
    CollectSectorWrites();
  }

  // Makes fat_ again for the disk of `layout`, forgetting its buffers, to see
  // the disk as it is.
  void RemakeFat(const DiskLayout& layout) {
    fat_.emplace(hardware_.get(), layout);
  }

  // Returns FAT sector `sector` as the disk has it.
  Sector ReadFatSector(int sector) {
    Sector words;
    EXPECT_EQ(hardware_->ReadSector(sector, absl::MakeSpan(words)),
              DiskStatus::kOk);
    return words;
  }

  // Returns the FAT's word `index` as the disk has it.
  uint16_t ReadFatWord(int index) {
    return ReadFatSector(
        DiskLayout::GetFatSector(index))[index % kDiskSectorWords];
  }

  // Sets the FAT's word `index` to `value` on the disk, behind fat_'s back, as
  // tests set up their disks. The write isn't collected.
  void SetFatWord(int index, uint16_t value) {
    const int sector = DiskLayout::GetFatSector(index);
    Sector words = ReadFatSector(sector);
    words[index % kDiskSectorWords] = value;
    EXPECT_EQ(hardware_->WriteSector(sector, words, WriteCaching::kSkip),
              DiskStatus::kOk);
    CollectSectorWrites();
  }

  // Links `clusters` into a chain on the disk, in order, ending after the
  // last.
  void SetChain(absl::Span<const int> clusters) {
    const int count = static_cast<int>(clusters.size());
    for (int i = 0; i < count; ++i) {
      SetFatWord(clusters[i], i + 1 < count
                                  ? static_cast<uint16_t>(clusters[i + 1])
                                  : DiskLayout::kFatEnd);
    }
  }

  // Returns the chain from `first` as fat_ walks it, buffers and all.
  std::vector<int> GetChain(int first) {
    std::vector<int> chain;
    for (int cluster = first; cluster != 0;) {
      chain.push_back(cluster);
      EXPECT_EQ(fat_->GetNextCluster(cluster, &cluster), DiskStatus::kOk);
    }
    return chain;
  }

  // Counts the free clusters, returning the count, or -1 if counting fails.
  int CountFree() {
    if (fat_->CountFreeClusters() != DiskStatus::kOk) {
      return -1;
    }
    return *fat_->GetFreeCount();
  }

  // Returns the cluster allocated after `last_cluster`, or -1 if it fails.
  int Allocate(int last_cluster) {
    int cluster = -1;
    EXPECT_EQ(fat_->AllocateCluster(last_cluster, &cluster), DiskStatus::kOk);
    return cluster;
  }

  // Returns the sectors written to the image since the last call, in order.
  std::vector<int> CollectSectorWrites() {
    std::vector<int> sectors;
    for (int64_t offset : protocol_->CollectWriteOffsets()) {
      sectors.push_back(
          static_cast<int>((offset - kHeaderBytes) / kSectorBytes));
    }
    return sectors;
  }

  gb::FileSystem host_;
  FaultyFileProtocol* protocol_ = nullptr;  // Owned by host_.
  std::unique_ptr<DiskHardware> hardware_;
  std::optional<DiskFat> fat_;
};

//------------------------------------------------------------------------------
// Format
//------------------------------------------------------------------------------

TEST_F(DiskFatTest, FormatTakesTheFirstClusterForTheRoot) {
  MakeFat(kFloppyLayout, 0xEEEE);
  int root_cluster = 0;
  ASSERT_EQ(fat_->Format(&root_cluster), DiskStatus::kOk);
  EXPECT_EQ(root_cluster, 1);
  EXPECT_THAT(fat_->GetFreeCount(), Optional(715));

  // Sector 1 first, then the others, zeroed.
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(1, 2, 3));
  Sector head = {};
  head[0] = 2;
  head[1] = DiskLayout::kFatEnd;
  EXPECT_EQ(ReadFatSector(1), head);
  EXPECT_THAT(ReadFatSector(2), Each(0));
  EXPECT_THAT(ReadFatSector(3), Each(0));

  // The disk's FAT agrees.
  RemakeFat(kFloppyLayout);
  EXPECT_EQ(CountFree(), 715);
  EXPECT_THAT(GetChain(1), ElementsAre(1));
}

TEST_F(DiskFatTest, FormatOfOneClusterIsFull) {
  constexpr DiskLayout kLayout = {
      .sectors_per_cluster = 1,
      .cluster_count = 1,
      .fat_sectors = 1,
  };
  MakeFat(kLayout, 0xEEEE);
  int root_cluster = 0;
  ASSERT_EQ(fat_->Format(&root_cluster), DiskStatus::kOk);
  EXPECT_EQ(root_cluster, 1);
  EXPECT_THAT(fat_->GetFreeCount(), Optional(0));
  EXPECT_EQ(ReadFatWord(0), DiskLayout::kFatEnd);
  EXPECT_EQ(ReadFatWord(1), DiskLayout::kFatEnd);

  RemakeFat(kLayout);
  EXPECT_EQ(CountFree(), 0);
}

TEST_F(DiskFatTest, FormatFailsWhenTheHostWriteFails) {
  MakeFat(kFloppyLayout);
  protocol_->SetLimits({.writes = 1});
  int root_cluster = 0;
  EXPECT_EQ(fat_->Format(&root_cluster), DiskStatus::kIoError);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(1));
}

//------------------------------------------------------------------------------
// Chains
//------------------------------------------------------------------------------

TEST_F(DiskFatTest, NextClustersFollowTheLinks) {
  // How each link reads is DiskLayoutTest's. These are the right words, from
  // each FAT sector.
  MakeFat(kFloppyLayout);
  SetFatWord(0, 300);
  SetFatWord(5, 600);
  SetFatWord(300, 450);
  SetFatWord(600, DiskLayout::kFatEnd);
  SetFatWord(7, 717);
  int next = -1;
  ASSERT_EQ(fat_->GetNextCluster(0, &next), DiskStatus::kOk);
  EXPECT_EQ(next, 300);
  ASSERT_EQ(fat_->GetNextCluster(5, &next), DiskStatus::kOk);
  EXPECT_EQ(next, 600);
  ASSERT_EQ(fat_->GetNextCluster(300, &next), DiskStatus::kOk);
  EXPECT_EQ(next, 450);
  ASSERT_EQ(fat_->GetNextCluster(600, &next), DiskStatus::kOk);
  EXPECT_EQ(next, 0);

  // A link past the last cluster leaves `next` alone.
  next = -1;
  EXPECT_EQ(fat_->GetNextCluster(7, &next), DiskStatus::kCorrupt);
  EXPECT_EQ(next, -1);
}

TEST_F(DiskFatTest, CountChainGivesTheCountAndLast) {
  MakeFat(kFloppyLayout);
  SetChain({300, 5, 600});
  int count = -1;
  int last = -1;
  ASSERT_EQ(fat_->CountChain(300, &count, &last), DiskStatus::kOk);
  EXPECT_EQ(count, 3);
  EXPECT_EQ(last, 600);

  // An empty chain.
  ASSERT_EQ(fat_->CountChain(0, &count, &last), DiskStatus::kOk);
  EXPECT_EQ(count, 0);
  EXPECT_EQ(last, 0);

  // A zero run to the last cluster, without asking for the last.
  ASSERT_EQ(fat_->CountChain(710, &count, /*chain_last=*/nullptr),
            DiskStatus::kOk);
  EXPECT_EQ(count, 7);
}

TEST_F(DiskFatTest, BrokenChainsAreCorrupt) {
  // The chain from 2 links past the last cluster, and the one from 4 loops.
  MakeFat(kSmallLayout);
  SetFatWord(3, 11);
  SetFatWord(5, 4);
  int count = -1;
  int last = -1;
  EXPECT_EQ(fat_->CountChain(11, &count, &last), DiskStatus::kCorrupt);
  EXPECT_EQ(fat_->CountChain(2, &count, &last), DiskStatus::kCorrupt);
  EXPECT_EQ(fat_->CountChain(4, &count, &last), DiskStatus::kCorrupt);
  EXPECT_EQ(count, -1);
  EXPECT_EQ(last, -1);
}

//------------------------------------------------------------------------------
// Free list
//------------------------------------------------------------------------------

TEST_F(DiskFatTest, FreeCountOfAFragmentedFreeList) {
  // Clusters 1, 2, 5, and 6 are taken. The free list is 3, 4 (a zero run),
  // then 7 to 10 (a link, then a zero run to the end).
  MakeFat(kSmallLayout);
  EXPECT_EQ(fat_->GetFreeCount(), std::nullopt);
  SetFatWord(0, 3);
  SetFatWord(1, DiskLayout::kFatEnd);
  SetFatWord(2, DiskLayout::kFatEnd);
  SetFatWord(4, 7);
  SetChain({5, 6});
  EXPECT_EQ(CountFree(), 6);
}

TEST_F(DiskFatTest, FreeCountFollowsLinksBackward) {
  // The free list is 10, 4, 3, then the end.
  MakeFat(kSmallLayout, DiskLayout::kFatEnd);
  SetFatWord(0, 10);
  SetChain({10, 4, 3});
  EXPECT_EQ(CountFree(), 3);
}

TEST_F(DiskFatTest, FreeCountOfAFullDisk) {
  MakeFat(kSmallLayout);
  SetFatWord(0, DiskLayout::kFatEnd);
  EXPECT_EQ(CountFree(), 0);
  int head = -1;
  EXPECT_EQ(fat_->GetFreeHead(&head), DiskStatus::kDiskFull);
}

TEST_F(DiskFatTest, TheFreeCountIsCountedOnce) {
  MakeFat(kSmallLayout);
  EXPECT_EQ(CountFree(), 10);
  SetFatWord(0, DiskLayout::kFatEnd);
  EXPECT_EQ(CountFree(), 10);
}

TEST_F(DiskFatTest, CountingReadsEachFatSectorOnceContinuously) {
  MakeFat(kFloppyLayout, 0,
          {
              .read_words_per_sec = kDiskSectorWords,
              .latency = kLatency,
          });
  hardware_->CollectBusyTime();
  EXPECT_EQ(CountFree(), 716);
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + 3 * kRead);
  EXPECT_EQ(CountFree(), 716);
  EXPECT_EQ(hardware_->CollectBusyTime(), absl::ZeroDuration());
}

TEST_F(DiskFatTest, CountingFailsWhenTheHostReadFails) {
  MakeFat(kSmallLayout);
  protocol_->SetLimits({.reads = 0});
  EXPECT_EQ(fat_->CountFreeClusters(), DiskStatus::kIoError);
  EXPECT_EQ(fat_->GetFreeCount(), std::nullopt);
}

TEST_F(DiskFatTest, BrokenFreeListsAreCorrupt) {
  struct Break {
    int index;
    uint16_t value;
  };
  const Break kBreaks[] = {
      {0, 11},  // A head out of range
      {5, 11},  // A link out of range
      {5, 3},   // A loop, from 5 back to 3
  };
  for (const Break& bad : kBreaks) {
    SCOPED_TRACE(::testing::Message()
                 << "Word " << bad.index << " = " << bad.value);
    MakeFat(kSmallLayout);
    SetFatWord(bad.index, bad.value);
    EXPECT_EQ(fat_->CountFreeClusters(), DiskStatus::kCorrupt);
    EXPECT_EQ(fat_->GetFreeCount(), std::nullopt);
  }
}

//------------------------------------------------------------------------------
// Allocating
//------------------------------------------------------------------------------

TEST_F(DiskFatTest, AllocatingTakesTheHead) {
  MakeFat(kSmallLayout);
  ASSERT_EQ(CountFree(), 10);
  EXPECT_EQ(Allocate(0), 1);
  EXPECT_EQ(Allocate(1), 2);
  EXPECT_EQ(Allocate(0), 3);
  EXPECT_THAT(fat_->GetFreeCount(), Optional(7));
  EXPECT_THAT(GetChain(1), ElementsAre(1, 2));
  EXPECT_THAT(GetChain(3), ElementsAre(3));
  int head = -1;
  ASSERT_EQ(fat_->GetFreeHead(&head), DiskStatus::kOk);
  EXPECT_EQ(head, 4);

  // The changes stay in sector 1's buffer until written.
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  EXPECT_EQ(ReadFatWord(0), 0);
  ASSERT_EQ(fat_->Write(), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(1));
  EXPECT_EQ(ReadFatWord(0), 4);
  EXPECT_EQ(ReadFatWord(1), 2);
  EXPECT_EQ(ReadFatWord(2), DiskLayout::kFatEnd);
  EXPECT_EQ(ReadFatWord(3), DiskLayout::kFatEnd);

  // Nothing is left to write.
  ASSERT_EQ(fat_->Write(), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
}

TEST_F(DiskFatTest, AllocatingTheLastFreeCluster) {
  MakeFat(kSmallLayout);
  SetFatWord(0, 10);
  ASSERT_EQ(CountFree(), 1);
  EXPECT_EQ(Allocate(0), 10);
  EXPECT_THAT(fat_->GetFreeCount(), Optional(0));

  // With none free, nothing changes.
  int cluster = -1;
  EXPECT_EQ(fat_->AllocateCluster(0, &cluster), DiskStatus::kDiskFull);
  EXPECT_EQ(cluster, -1);
  ASSERT_EQ(fat_->Write(), DiskStatus::kOk);
  EXPECT_EQ(ReadFatWord(0), DiskLayout::kFatEnd);
  EXPECT_EQ(ReadFatWord(10), DiskLayout::kFatEnd);
}

TEST_F(DiskFatTest, AllocatingWritesTheEndBeforeALinkInALaterSector) {
  // The head, 2, ends in sector 1, and links from 300, in sector 2.
  MakeFat(kFloppyLayout);
  SetFatWord(0, 2);
  ASSERT_EQ(CountFree(), 715);
  EXPECT_EQ(Allocate(300), 2);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(1));
  EXPECT_EQ(ReadFatWord(0), 3);
  EXPECT_EQ(ReadFatWord(2), DiskLayout::kFatEnd);

  ASSERT_EQ(fat_->Write(), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(2));
  EXPECT_EQ(ReadFatWord(300), 2);
}

TEST_F(DiskFatTest, AllocatingWritesANewEndInALaterSectorBeforeTheLink) {
  // The head, 400, ends in sector 2, and links from 1, in sector 1. Sector 1
  // is written before sector 2 too.
  MakeFat(kFloppyLayout);
  SetFatWord(0, 400);
  ASSERT_EQ(CountFree(), 317);
  EXPECT_EQ(Allocate(1), 400);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(1, 2));
  EXPECT_EQ(ReadFatWord(0), 401);
  EXPECT_EQ(ReadFatWord(400), DiskLayout::kFatEnd);

  ASSERT_EQ(fat_->Write(), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(1));
  EXPECT_EQ(ReadFatWord(1), 400);
}

TEST_F(DiskFatTest, ChainsInTheWorkingSectorWriteNothingUntilWritten) {
  // Two chains growing at once, in sector 2.
  MakeFat(kFloppyLayout);
  SetFatWord(0, 300);
  ASSERT_EQ(CountFree(), 417);
  EXPECT_EQ(Allocate(0), 300);
  EXPECT_EQ(Allocate(0), 301);
  EXPECT_EQ(Allocate(300), 302);
  EXPECT_EQ(Allocate(301), 303);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());

  // Sector 1 first.
  ASSERT_EQ(fat_->Write(), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(1, 2));
  RemakeFat(kFloppyLayout);
  EXPECT_THAT(GetChain(300), ElementsAre(300, 302));
  EXPECT_THAT(GetChain(301), ElementsAre(301, 303));
  EXPECT_EQ(CountFree(), 413);
}

TEST_F(DiskFatTest, MovingTheWorkingBufferWritesItAfterSectorOne) {
  MakeFat(kFloppyLayout);
  SetFatWord(0, 300);
  ASSERT_EQ(CountFree(), 417);
  EXPECT_EQ(Allocate(0), 300);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());

  // Reading sector 3 into the working buffer.
  int next = -1;
  ASSERT_EQ(fat_->GetNextCluster(600, &next), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(1, 2));
  EXPECT_EQ(ReadFatWord(0), 301);
  EXPECT_EQ(ReadFatWord(300), DiskLayout::kFatEnd);
}

//------------------------------------------------------------------------------
// Freeing
//------------------------------------------------------------------------------

TEST_F(DiskFatTest, FreeingAWholeChainOntoTheHead) {
  // Cluster 1 is taken, the chains are 2 to 4 and 5, and the free list starts
  // at 6.
  MakeFat(kSmallLayout);
  SetFatWord(1, DiskLayout::kFatEnd);
  SetChain({2, 3, 4});
  SetChain({5});
  SetFatWord(0, 6);

  // Nothing has counted the free clusters, and freeing doesn't.
  ASSERT_EQ(fat_->FreeChain(/*unlink_from=*/0, 2, 4, 3), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(1));
  EXPECT_EQ(fat_->GetFreeCount(), std::nullopt);
  EXPECT_EQ(ReadFatWord(0), 2);
  EXPECT_EQ(ReadFatWord(4), 6);

  // Once counted, freeing keeps the count.
  EXPECT_EQ(CountFree(), 8);
  ASSERT_EQ(fat_->FreeChain(/*unlink_from=*/0, 5, 5, 1), DiskStatus::kOk);
  EXPECT_THAT(fat_->GetFreeCount(), Optional(9));
  RemakeFat(kSmallLayout);
  EXPECT_THAT(GetChain(5), ElementsAre(5, 2, 3, 4, 6, 7, 8, 9, 10));
}

TEST_F(DiskFatTest, FreeingOntoAnEmptyFreeList) {
  // The chain already ends where the list does, so only the head changes.
  MakeFat(kSmallLayout, DiskLayout::kFatEnd);
  SetChain({4, 2});
  ASSERT_EQ(CountFree(), 0);
  ASSERT_EQ(fat_->FreeChain(/*unlink_from=*/0, 4, 2, 2), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(1));
  EXPECT_THAT(fat_->GetFreeCount(), Optional(2));
  RemakeFat(kSmallLayout);
  EXPECT_EQ(CountFree(), 2);
  EXPECT_THAT(GetChain(4), ElementsAre(4, 2));
}

TEST_F(DiskFatTest, FreeingWritesEachChangeInOrderAcrossSectors) {
  MakeFat(kFloppyLayout);
  SetChain({250, 300});
  SetChain({100, 520});
  SetChain({600, 601, 602});
  SetFatWord(0, 700);

  // Unlinking 300 from 250: the new end in sector 1, the link to the old head
  // in sector 2, then the head.
  ASSERT_EQ(fat_->FreeChain(250, 300, 300, 1), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(1, 2, 1));
  EXPECT_EQ(ReadFatWord(250), DiskLayout::kFatEnd);
  EXPECT_EQ(ReadFatWord(300), 700);
  EXPECT_EQ(ReadFatWord(0), 300);

  // A whole chain, whose last link is in sector 3.
  ASSERT_EQ(fat_->FreeChain(/*unlink_from=*/0, 100, 520, 2), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(3, 1));
  EXPECT_EQ(ReadFatWord(520), 300);
  EXPECT_EQ(ReadFatWord(0), 100);

  // The new end and the link next to each other in sector 3, in one write.
  ASSERT_EQ(fat_->FreeChain(600, 601, 602, 2), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(3, 1));
  EXPECT_EQ(ReadFatWord(600), DiskLayout::kFatEnd);
  EXPECT_EQ(ReadFatWord(602), 100);
  EXPECT_EQ(ReadFatWord(0), 601);
}

TEST_F(DiskFatTest, FreeingNothingWritesNothing) {
  MakeFat(kSmallLayout);
  ASSERT_EQ(CountFree(), 10);
  ASSERT_EQ(fat_->FreeChain(/*unlink_from=*/0, 0, 0, 0), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  EXPECT_THAT(fat_->GetFreeCount(), Optional(10));
}

//------------------------------------------------------------------------------
// Host failures
//------------------------------------------------------------------------------

TEST_F(DiskFatTest, WritesFailWhenTheHostWriteFails) {
  // Allocating a cluster whose end is in another sector than its link.
  MakeFat(kFloppyLayout);
  SetFatWord(0, 400);
  ASSERT_EQ(CountFree(), 317);
  protocol_->SetLimits({.writes = 0});
  int cluster = -1;
  EXPECT_EQ(fat_->AllocateCluster(1, &cluster), DiskStatus::kIoError);
  EXPECT_EQ(cluster, -1);

  // Writing the buffers.
  MakeFat(kSmallLayout);
  ASSERT_EQ(CountFree(), 10);
  Allocate(0);
  protocol_->SetLimits({.writes = 0});
  EXPECT_EQ(fat_->Write(), DiskStatus::kIoError);

  // Freeing.
  MakeFat(kSmallLayout);
  SetChain({2});
  protocol_->SetLimits({.writes = 0});
  EXPECT_EQ(fat_->FreeChain(/*unlink_from=*/0, 2, 2, 1), DiskStatus::kIoError);
}

}  // namespace
}  // namespace oz3
