// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_file_system.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <memory>
#include <string_view>
#include <utility>

#include "absl/status/statusor.h"
#include "absl/time/civil_time.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/date_time.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/faulty_file_protocol.h"

namespace oz3 {
namespace {

using ::testing::Each;

using Sector = std::array<uint16_t, kDiskSectorWords>;

constexpr char kPath[] = "mem:/disk.img";
constexpr char kOtherPath[] = "mem:/other.img";
constexpr int kMaxOpenFiles = 4;

// A 360 KB floppy: 716 clusters of one sector, after 3 FAT sectors.
constexpr int64_t kFloppySectors = 720;

// A disk of 10 clusters of one sector, after 1 FAT sector.
constexpr int64_t kSmallSectors = 12;
constexpr int kSmallClusters = 10;

// What reads take with timed settings: a sector read takes 1 second, and a
// seek 100 milliseconds.
constexpr absl::Duration kRead = absl::Seconds(1);
constexpr absl::Duration kLatency = absl::Milliseconds(100);

DateTime GetNow() {
  return DateTime::FromCivil(absl::CivilSecond(2026, 11, 11, 9, 30, 15));
}

DiskLabel GetLabel(std::string_view text) { return *DiskLabel::Parse(text); }

// Returns a sector whose words are all `value`.
Sector MakeFilledSector(uint16_t value) {
  Sector sector;
  sector.fill(value);
  return sector;
}

class DiskFileSystemTest : public ::testing::Test {
 protected:
  DiskFileSystemTest() : file_system_({}, kMaxOpenFiles) {
    std::unique_ptr<FaultyFileProtocol> protocol =
        std::make_unique<FaultyFileProtocol>();
    protocol_ = protocol.get();
    host_.Register(std::move(protocol));
  }

  // Returns a new blank image of `sector_count` sectors at `path`.
  std::unique_ptr<DiskImage> MakeImage(int64_t sector_count,
                                       std::string_view path = kPath) {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&host_, path, sector_count);
    EXPECT_TRUE(image.ok()) << image.status();
    return image.ok() ? *std::move(image) : nullptr;
  }

  // Attaches a new image of `sector_count` sectors, and formats it with the
  // label "WORK" and `sectors_per_cluster`.
  void AttachFormatted(int64_t sector_count, int sectors_per_cluster = 0) {
    EXPECT_EQ(file_system_.AttachImage(MakeImage(sector_count)),
              DiskStatus::kUnformatted);
    EXPECT_EQ(
        file_system_.Format(GetLabel("WORK"), sectors_per_cluster, GetNow()),
        DiskStatus::kOk);
  }

  // Detaches the image, sets word `index` of its sector `sector` to `value`,
  // and attaches it again, returning what attaching does.
  DiskStatus PatchWord(int64_t sector, int index, uint16_t value) {
    std::unique_ptr<DiskImage> image = file_system_.DetachImage();
    Sector words = ReadSector(*image, sector);
    words[index] = value;
    WriteSector(*image, sector, words);
    return file_system_.AttachImage(std::move(image));
  }

  static Sector ReadSector(DiskImage& image, int64_t sector) {
    Sector words;
    EXPECT_EQ(image.ReadSector(sector, absl::MakeSpan(words)), DiskStatus::kOk);
    return words;
  }

  static void WriteSector(DiskImage& image, int64_t sector,
                          const Sector& words) {
    EXPECT_EQ(image.WriteSector(sector, words), DiskStatus::kOk);
  }

  // Returns the free clusters GetVolumeInfo reports, or -1 if it fails.
  int GetFreeClusters() {
    DiskVolumeInfo info;
    if (file_system_.GetVolumeInfo(&info) != DiskStatus::kOk) {
      return -1;
    }
    return info.free_clusters;
  }

  gb::FileSystem host_;
  FaultyFileProtocol* protocol_ = nullptr;
  DiskFileSystem file_system_;
};

TEST_F(DiskFileSystemTest, Properties) {
  const DiskHardwareSettings settings = {.read_cache_sectors = 2};
  const DiskFileSystem file_system(settings, 7);
  EXPECT_EQ(file_system.GetSettings(), settings);
  EXPECT_EQ(file_system.GetMaxOpenFiles(), 7);
}

TEST_F(DiskFileSystemTest, EmptyDriveIsNoDisk) {
  EXPECT_FALSE(file_system_.HasImage());
  EXPECT_EQ(file_system_.DetachImage(), nullptr);
  EXPECT_EQ(file_system_.GetMedia(), nullptr);
  EXPECT_EQ(file_system_.Format(GetLabel("WORK"), 0, GetNow()),
            DiskStatus::kNoDisk);
  EXPECT_EQ(file_system_.SetLabel(GetLabel("WORK")), DiskStatus::kNoDisk);
  DiskVolumeInfo volume;
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kNoDisk);
}

TEST_F(DiskFileSystemTest, BlankImageIsUnformatted) {
  EXPECT_EQ(file_system_.AttachImage(MakeImage(kFloppySectors)),
            DiskStatus::kUnformatted);
  EXPECT_TRUE(file_system_.HasImage());
  DiskVolumeInfo volume;
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kUnformatted);
  EXPECT_EQ(file_system_.SetLabel(GetLabel("WORK")), DiskStatus::kUnformatted);
}

TEST_F(DiskFileSystemTest, MediaIsTheImageFormattedOrNot) {
  std::unique_ptr<DiskImage> image = MakeImage(kFloppySectors);
  const DiskMedia* const media = image.get();
  ASSERT_EQ(file_system_.AttachImage(std::move(image)),
            DiskStatus::kUnformatted);
  EXPECT_EQ(file_system_.GetMedia(), media);
  ASSERT_EQ(file_system_.Format(GetLabel("WORK"), 0, GetNow()),
            DiskStatus::kOk);
  EXPECT_EQ(file_system_.GetMedia(), media);
  EXPECT_EQ(media->GetSectorCount(), kFloppySectors);
}

TEST_F(DiskFileSystemTest, FormatWritesTheSuperblockFatAndRoot) {
  // Fill every sector, to see what formatting clears.
  std::unique_ptr<DiskImage> image = MakeImage(kFloppySectors);
  for (int64_t sector = 0; sector < kFloppySectors; ++sector) {
    WriteSector(*image, sector, MakeFilledSector(0xEEEE));
  }
  ASSERT_EQ(file_system_.AttachImage(std::move(image)),
            DiskStatus::kUnformatted);
  ASSERT_EQ(file_system_.Format(GetLabel("work"), 0, GetNow()),
            DiskStatus::kOk);
  image = file_system_.DetachImage();

  Sector superblock = {};
  superblock[0] = 0x5A4F;  // "OZFS"
  superblock[1] = 0x5346;
  superblock[2] = 1;    // Version
  superblock[3] = 1;    // Sectors per cluster
  superblock[4] = 716;  // Clusters
  superblock[5] = 3;    // FAT sectors
  superblock[6] = 1;    // Root cluster
  superblock[8] = 'W' | 'O' << 8;
  superblock[9] = 'R' | 'K' << 8;
  const DateTime::Words& now = GetNow().GetWords();
  std::copy(now.begin(), now.end(), superblock.begin() + 16);
  EXPECT_EQ(ReadSector(*image, 0), superblock);

  // The root took cluster 1, leaving the free list's head at 2.
  Sector fat_head = {};
  fat_head[0] = 2;
  fat_head[1] = 0xFFFF;
  EXPECT_EQ(ReadSector(*image, 1), fat_head);
  EXPECT_THAT(ReadSector(*image, 2), Each(0));
  EXPECT_THAT(ReadSector(*image, 3), Each(0));

  // The root's one sector is zeroed, and nothing after it is written.
  EXPECT_THAT(ReadSector(*image, 4), Each(0));
  EXPECT_THAT(ReadSector(*image, 5), Each(0xEEEE));
  EXPECT_THAT(ReadSector(*image, kFloppySectors - 1), Each(0xEEEE));
}

TEST_F(DiskFileSystemTest, FormatZeroesTheRootsWholeCluster) {
  std::unique_ptr<DiskImage> image = MakeImage(kFloppySectors);
  for (int64_t sector = 0; sector < 8; ++sector) {
    WriteSector(*image, sector, MakeFilledSector(0xEEEE));
  }
  ASSERT_EQ(file_system_.AttachImage(std::move(image)),
            DiskStatus::kUnformatted);
  ASSERT_EQ(file_system_.Format(GetLabel("WORK"), 4, GetNow()),
            DiskStatus::kOk);
  DiskVolumeInfo volume;
  ASSERT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(volume, (DiskVolumeInfo{.label = GetLabel("WORK"),
                                    .sectors_per_cluster = 4,
                                    .cluster_count = 179,
                                    .free_clusters = 178}));

  // One FAT sector, then the root's four.
  image = file_system_.DetachImage();
  EXPECT_EQ(ReadSector(*image, 0)[5], 1);
  for (int64_t sector = 2; sector < 6; ++sector) {
    EXPECT_THAT(ReadSector(*image, sector), Each(0)) << "Sector " << sector;
  }
  EXPECT_THAT(ReadSector(*image, 6), Each(0xEEEE));
}

TEST_F(DiskFileSystemTest, FormatOfOneClusterIsFull) {
  AttachFormatted(3);
  EXPECT_EQ(GetFreeClusters(), 0);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadSector(*image, 1)[0], 0xFFFF);
  EXPECT_EQ(ReadSector(*image, 1)[1], 0xFFFF);

  // Walking the free list agrees.
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), 0);
}

TEST_F(DiskFileSystemTest, FormatRefusesBadSectorsPerCluster) {
  ASSERT_EQ(file_system_.AttachImage(MakeImage(kFloppySectors)),
            DiskStatus::kUnformatted);
  EXPECT_EQ(file_system_.Format(GetLabel("WORK"), 3, GetNow()),
            DiskStatus::kBadArgument);
  EXPECT_EQ(file_system_.Format(GetLabel("WORK"), 128, GetNow()),
            DiskStatus::kBadArgument);
  EXPECT_EQ(file_system_.Format(GetLabel("WORK"), -1, GetNow()),
            DiskStatus::kBadArgument);
  DiskVolumeInfo volume;
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kUnformatted);

  // Too few to cover the image, and a formatted disk stays as it was.
  ASSERT_EQ(file_system_.AttachImage(MakeImage(65'792, kOtherPath)),
            DiskStatus::kUnformatted);
  ASSERT_EQ(file_system_.Format(GetLabel("WORK"), 0, GetNow()),
            DiskStatus::kOk);
  EXPECT_EQ(file_system_.Format(GetLabel("OTHER"), 1, GetNow()),
            DiskStatus::kBadArgument);
  ASSERT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(volume.label, GetLabel("WORK"));
  EXPECT_EQ(volume.sectors_per_cluster, 2);
}

TEST_F(DiskFileSystemTest, FormatReplacesAFileSystem) {
  AttachFormatted(kFloppySectors);
  ASSERT_EQ(file_system_.Format(GetLabel("OTHER"), 2, GetNow()),
            DiskStatus::kOk);
  DiskVolumeInfo volume;
  ASSERT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(volume, (DiskVolumeInfo{.label = GetLabel("OTHER"),
                                    .sectors_per_cluster = 2,
                                    .cluster_count = 358,
                                    .free_clusters = 357}));
}

TEST_F(DiskFileSystemTest, AttachingAFormattedDisk) {
  AttachFormatted(kFloppySectors);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  DiskFileSystem other({}, 1);
  ASSERT_EQ(other.AttachImage(std::move(image)), DiskStatus::kOk);
  DiskVolumeInfo volume;
  ASSERT_EQ(other.GetVolumeInfo(&volume), DiskStatus::kOk);

  // The free list runs across all three FAT sectors.
  EXPECT_EQ(volume, (DiskVolumeInfo{.label = GetLabel("WORK"),
                                    .sectors_per_cluster = 1,
                                    .cluster_count = 716,
                                    .free_clusters = 715}));
}

TEST_F(DiskFileSystemTest, AttachingAcceptsExtraFatSectors) {
  // 715 clusters after 4 FAT sectors still fit 720 sectors.
  AttachFormatted(kFloppySectors);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  Sector superblock = ReadSector(*image, 0);
  superblock[4] = 715;
  superblock[5] = 4;
  WriteSector(*image, 0, superblock);
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  DiskVolumeInfo volume;
  ASSERT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(volume.cluster_count, 715);
  EXPECT_EQ(volume.free_clusters, 714);
}

TEST_F(DiskFileSystemTest, EachWayADiskIsUnformatted) {
  struct Patch {
    int index;
    uint16_t value;
  };
  const Patch kPatches[] = {
      {0, 0x5A50},    // Format ID
      {1, 0x5347},    // Format ID
      {2, 0},         // Version
      {2, 2},         // Version
      {3, 0},         // Sectors per cluster
      {3, 3},         // Sectors per cluster
      {3, 128},       // Sectors per cluster
      {3, 2},         // Sectors per cluster, making the disk too big
      {4, 0},         // Cluster count
      {4, 717},       // Cluster count, making the disk too big
      {4, 0xFFFF},    // Cluster count
      {5, 2},         // FAT sectors, too few
      {5, 4},         // FAT sectors, making the disk too big
      {6, 0},         // Root cluster
      {6, 717},       // Root cluster
      {8, '.'},       // Label
      {8, 'A' << 8},  // Label, with a zero before a character
  };
  AttachFormatted(kFloppySectors);
  for (const Patch& patch : kPatches) {
    std::unique_ptr<DiskImage> image = file_system_.DetachImage();
    const Sector superblock = ReadSector(*image, 0);
    Sector patched = superblock;
    patched[patch.index] = patch.value;
    WriteSector(*image, 0, patched);
    EXPECT_EQ(file_system_.AttachImage(std::move(image)),
              DiskStatus::kUnformatted)
        << "Word " << patch.index << " = " << patch.value;
    DiskVolumeInfo volume;
    EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kUnformatted);

    image = file_system_.DetachImage();
    WriteSector(*image, 0, superblock);
    ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  }
}

TEST_F(DiskFileSystemTest, ADiskBiggerThanItsImageIsUnformatted) {
  AttachFormatted(kFloppySectors);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();

  // Copy the disk into an image one sector smaller.
  std::unique_ptr<DiskImage> smaller =
      MakeImage(kFloppySectors - 1, kOtherPath);
  for (int64_t sector = 0; sector < kFloppySectors - 1; ++sector) {
    WriteSector(*smaller, sector, ReadSector(*image, sector));
  }
  EXPECT_EQ(file_system_.AttachImage(std::move(smaller)),
            DiskStatus::kUnformatted);
}

TEST_F(DiskFileSystemTest, DetachReturnsTheImage) {
  std::unique_ptr<DiskImage> image = MakeImage(kFloppySectors);
  const DiskImage* const attached = image.get();
  ASSERT_EQ(file_system_.AttachImage(std::move(image)),
            DiskStatus::kUnformatted);
  ASSERT_EQ(file_system_.Format(GetLabel("WORK"), 0, GetNow()),
            DiskStatus::kOk);
  image = file_system_.DetachImage();
  EXPECT_EQ(image.get(), attached);
  EXPECT_FALSE(file_system_.HasImage());
  DiskVolumeInfo volume;
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kNoDisk);
  EXPECT_EQ(file_system_.SetLabel(GetLabel("WORK")), DiskStatus::kNoDisk);
}

TEST_F(DiskFileSystemTest, AttachingForgetsTheBuffersAndFreeCount) {
  AttachFormatted(kSmallSectors);
  EXPECT_EQ(GetFreeClusters(), kSmallClusters - 1);

  // Take cluster 2 off the free list behind the file system's back.
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  Sector fat = ReadSector(*image, 1);
  fat[0] = 3;
  fat[2] = 0xFFFF;
  WriteSector(*image, 1, fat);
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), kSmallClusters - 2);
}

TEST_F(DiskFileSystemTest, AttachingOverAnAttachedImage) {
  AttachFormatted(kFloppySectors);
  EXPECT_EQ(file_system_.AttachImage(MakeImage(kSmallSectors, kOtherPath)),
            DiskStatus::kUnformatted);
  ASSERT_NE(file_system_.GetMedia(), nullptr);
  EXPECT_EQ(file_system_.GetMedia()->GetSectorCount(), kSmallSectors);
  DiskVolumeInfo volume;
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kUnformatted);

  // The first image was destroyed, so its file can be opened again.
  absl::StatusOr<std::unique_ptr<DiskImage>> first =
      DiskImage::Open(&host_, kPath);
  ASSERT_TRUE(first.ok()) << first.status();
  ASSERT_EQ(file_system_.AttachImage(*std::move(first)), DiskStatus::kOk);
  ASSERT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(volume.label, GetLabel("WORK"));
}

TEST_F(DiskFileSystemTest, AttachingChargesTheSuperblockWithAFreshCache) {
  AttachFormatted(kFloppySectors);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();

  DiskFileSystem timed(
      {
          .read_cache_sectors = 4,
          .read_words_per_sec = kDiskSectorWords,
          .write_words_per_sec = kDiskSectorWords,
          .cache_words_per_sec = kDiskSectorWords * 100,
          .latency = kLatency,
      },
      kMaxOpenFiles);
  ASSERT_EQ(timed.AttachImage(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(timed.CollectBusyTime(), kLatency + kRead);
  EXPECT_EQ(timed.CollectBusyTime(), absl::ZeroDuration());

  // Attaching again reads the disk again, rather than the cache.
  ASSERT_EQ(timed.AttachImage(timed.DetachImage()), DiskStatus::kOk);
  EXPECT_EQ(timed.CollectBusyTime(), kLatency + kRead);

  // Counting the free clusters reads each FAT sector once, continuously.
  DiskVolumeInfo volume;
  ASSERT_EQ(timed.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(timed.CollectBusyTime(), 3 * kRead);
  ASSERT_EQ(timed.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(timed.CollectBusyTime(), absl::ZeroDuration());
}

TEST_F(DiskFileSystemTest, FreeCountOfAFragmentedFreeList) {
  // Clusters 1, 2, 5, and 6 are taken. The free list is 3, 4 (a zero run),
  // then 7 to 10 (a link, then a zero run to the end).
  AttachFormatted(kSmallSectors);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  Sector fat = {};
  fat[0] = 3;
  fat[1] = 0xFFFF;
  fat[2] = 0xFFFF;
  fat[4] = 7;
  fat[5] = 6;
  fat[6] = 0xFFFF;
  WriteSector(*image, 1, fat);
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), 6);
}

TEST_F(DiskFileSystemTest, FreeCountFollowsLinksBackward) {
  // The free list is 10, 4, 3, then the end.
  AttachFormatted(kSmallSectors);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  Sector fat = MakeFilledSector(0xFFFF);
  fat[0] = 10;
  fat[10] = 4;
  fat[4] = 3;
  WriteSector(*image, 1, fat);
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), 3);
}

TEST_F(DiskFileSystemTest, FreeCountOfAFullDisk) {
  AttachFormatted(kSmallSectors);
  ASSERT_EQ(PatchWord(1, 0, 0xFFFF), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), 0);
}

TEST_F(DiskFileSystemTest, BrokenFreeListsAreCorrupt) {
  AttachFormatted(kSmallSectors);
  DiskVolumeInfo volume;

  // A head out of range.
  ASSERT_EQ(PatchWord(1, 0, kSmallClusters + 1), DiskStatus::kOk);
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kCorrupt);
  ASSERT_EQ(PatchWord(1, 0, 2), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), kSmallClusters - 1);

  // A link out of range.
  ASSERT_EQ(PatchWord(1, 5, kSmallClusters + 1), DiskStatus::kOk);
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kCorrupt);

  // A loop, from 5 back to 3, and it stays corrupt.
  ASSERT_EQ(PatchWord(1, 5, 3), DiskStatus::kOk);
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kCorrupt);
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kCorrupt);
}

TEST_F(DiskFileSystemTest, SetLabelRewritesOnlyTheLabel) {
  AttachFormatted(kFloppySectors);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  Sector superblock = ReadSector(*image, 0);
  superblock[7] = 0x1234;    // Reserved flags
  superblock[100] = 0x5678;  // The boot area
  WriteSector(*image, 0, superblock);
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);

  ASSERT_EQ(file_system_.SetLabel(GetLabel("archive")), DiskStatus::kOk);
  DiskVolumeInfo volume;
  ASSERT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(volume.label, GetLabel("ARCHIVE"));

  image = file_system_.DetachImage();
  superblock[8] = 'A' | 'R' << 8;
  superblock[9] = 'C' | 'H' << 8;
  superblock[10] = 'I' | 'V' << 8;
  superblock[11] = 'E';
  EXPECT_EQ(ReadSector(*image, 0), superblock);

  // An empty label clears it.
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  ASSERT_EQ(file_system_.SetLabel(DiskLabel()), DiskStatus::kOk);
  ASSERT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(volume.label, DiskLabel());
  image = file_system_.DetachImage();
  EXPECT_EQ(ReadSector(*image, 0)[8], 0);
}

TEST_F(DiskFileSystemTest, WriteProtectionRefusesFormatAndLabel) {
  AttachFormatted(kFloppySectors);
  ASSERT_EQ(file_system_.GetMedia()->SetWriteProtected(true), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Format(GetLabel("OTHER"), 0, GetNow()),
            DiskStatus::kWriteProtected);
  EXPECT_EQ(file_system_.Format(GetLabel("OTHER"), 3, GetNow()),
            DiskStatus::kWriteProtected);
  EXPECT_EQ(file_system_.SetLabel(GetLabel("OTHER")),
            DiskStatus::kWriteProtected);
  DiskVolumeInfo volume;
  ASSERT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(volume.label, GetLabel("WORK"));

  // An unformatted disk is protected too.
  std::unique_ptr<DiskImage> blank = MakeImage(kFloppySectors, kOtherPath);
  ASSERT_EQ(blank->SetWriteProtected(true), DiskStatus::kOk);
  ASSERT_EQ(file_system_.AttachImage(std::move(blank)),
            DiskStatus::kUnformatted);
  EXPECT_EQ(file_system_.Format(GetLabel("OTHER"), 0, GetNow()),
            DiskStatus::kWriteProtected);
  EXPECT_EQ(file_system_.SetLabel(GetLabel("OTHER")),
            DiskStatus::kWriteProtected);
}

TEST_F(DiskFileSystemTest, AttachingFailsWhenTheHostReadFails) {
  std::unique_ptr<DiskImage> image = MakeImage(kFloppySectors);
  protocol_->SetLimits({.reads = 0});
  EXPECT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kIoError);
  EXPECT_TRUE(file_system_.HasImage());
  DiskVolumeInfo volume;
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kUnformatted);
}

TEST_F(DiskFileSystemTest, CountingFailsWhenTheHostReadFails) {
  AttachFormatted(kFloppySectors);
  ASSERT_EQ(file_system_.AttachImage(file_system_.DetachImage()),
            DiskStatus::kOk);
  protocol_->SetLimits({.reads = 0});
  DiskVolumeInfo volume;
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kIoError);
}

TEST_F(DiskFileSystemTest, FormatFailsWhenTheHostWriteFails) {
  ASSERT_EQ(file_system_.AttachImage(MakeImage(kFloppySectors)),
            DiskStatus::kUnformatted);
  protocol_->SetLimits({.writes = 2});
  EXPECT_EQ(file_system_.Format(GetLabel("WORK"), 0, GetNow()),
            DiskStatus::kIoError);
  DiskVolumeInfo volume;
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kUnformatted);
}

TEST_F(DiskFileSystemTest, FormatFailingPartwayLeavesTheDiskUnformatted) {
  // A floppy's format writes the blank superblock, 3 FAT sectors, the root's
  // one sector, and the superblock: 6 writes. Failing any after the first
  // leaves the disk unformatted, the last included.
  AttachFormatted(kFloppySectors);
  for (int writes = 1; writes < 6; ++writes) {
    SCOPED_TRACE(writes);
    protocol_->SetLimits({.writes = writes});
    EXPECT_EQ(file_system_.Format(GetLabel("OTHER"), 0, GetNow()),
              DiskStatus::kIoError);
    DiskVolumeInfo volume;
    EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kUnformatted);

    // The failure leaves the image unusable, so the file is opened again.
    file_system_.DetachImage().reset();
    protocol_->SetLimits({});
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&host_, kPath);
    ASSERT_TRUE(image.ok()) << image.status();
    EXPECT_EQ(file_system_.AttachImage(*std::move(image)),
              DiskStatus::kUnformatted);
    ASSERT_EQ(file_system_.Format(GetLabel("WORK"), 0, GetNow()),
              DiskStatus::kOk);
  }
}

TEST_F(DiskFileSystemTest, SetLabelFailingKeepsTheLabel) {
  AttachFormatted(kFloppySectors);
  protocol_->SetLimits({.writes = 0});
  EXPECT_EQ(file_system_.SetLabel(GetLabel("OTHER")), DiskStatus::kIoError);
  DiskVolumeInfo volume;
  ASSERT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(volume.label, GetLabel("WORK"));
}

}  // namespace
}  // namespace oz3
