// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_file_system.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/functional/function_ref.h"
#include "absl/status/statusor.h"
#include "absl/time/civil_time.h"
#include "absl/time/time.h"
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
#include "oz3/disk/faulty_file_protocol.h"

namespace oz3 {
namespace {

using ::testing::Each;
using ::testing::ElementsAre;
using ::testing::IsEmpty;

using Sector = std::array<uint16_t, kDiskSectorWords>;

constexpr char kPath[] = "mem:/disk.img";
constexpr char kOtherPath[] = "mem:/other.img";
constexpr int kMaxOpenFiles = 4;

// Where an image's sectors are in its file, after the header.
constexpr int64_t kHeaderBytes = 16;
constexpr int64_t kSectorBytes = kDiskSectorWords * 2;

// A 360 KB floppy: 716 clusters of one sector, after 3 FAT sectors.
constexpr int kFloppySectors = 720;

// A disk of 10 clusters of one sector, after 1 FAT sector.
constexpr int kSmallSectors = 12;
constexpr int kSmallClusters = 10;

// What reads take with timed settings: a sector read takes 1 second, and a
// seek 100 milliseconds.
constexpr absl::Duration kRead = absl::Seconds(1);
constexpr absl::Duration kLatency = absl::Milliseconds(100);

DateTime GetNow() {
  return DateTime::FromCivil(absl::CivilSecond(2026, 11, 11, 9, 30, 15));
}

// A time after GetNow(), for what is created after formatting.
DateTime GetLater() {
  return DateTime::FromCivil(absl::CivilSecond(2026, 11, 12, 14, 5, 45));
}

DiskLabel GetLabel(std::string_view text) { return *DiskLabel::Parse(text); }

DiskFilename GetName(std::string_view text) {
  return *DiskFilename::Parse(text);
}

DiskFilename GetPattern(std::string_view text) {
  return *DiskFilename::ParsePattern(text);
}

// Returns a sector whose words are all `value`.
Sector MakeFilledSector(uint16_t value) {
  Sector sector;
  sector.fill(value);
  return sector;
}

// Returns `count` words counting up from `first`, as a file's data.
std::vector<uint16_t> MakeCountingWords(uint16_t first, int count) {
  std::vector<uint16_t> words(count);
  for (int i = 0; i < count; ++i) {
    words[i] = static_cast<uint16_t>(first + i);
  }
  return words;
}

// Returns a directory entry for the file `name`, modified at `modified`.
DiskDirEntry MakeEntry(std::string_view name, int first_cluster, int size,
                       uint16_t attributes = 0, DateTime modified = GetNow()) {
  DiskDirEntry entry;
  entry.SetName(GetName(name));
  entry.SetAttributes(attributes);
  entry.SetFirstCluster(first_cluster);
  entry.SetSize(size);
  entry.SetModified(modified);
  return entry;
}

Sector ReadSector(DiskImage& image, int sector) {
  Sector words;
  EXPECT_EQ(image.ReadSector(sector, absl::MakeSpan(words)), DiskStatus::kOk);
  return words;
}

void WriteSector(DiskImage& image, int sector, const Sector& words) {
  EXPECT_EQ(image.WriteSector(sector, words), DiskStatus::kOk);
}

// Returns entry `index` of directory sector `sector`.
DiskDirEntry ReadEntry(DiskImage& image, int sector, int index) {
  const Sector words = ReadSector(image, sector);
  return DiskDirEntry::FromWords(absl::MakeConstSpan(words).subspan(
      index * DiskDirEntry::kWordCount, DiskDirEntry::kWordCount));
}

// Returns the entry Create makes for `name`, modified GetLater().
DiskDirEntry MakeCreatedEntry(std::string_view name) {
  return MakeEntry(name, 0, 0, 0, GetLater());
}

// Returns the name of the `index`th filler file, for filling directories.
std::string GetFillerName(int index) { return "F" + std::to_string(index); }

// Writes a formatted disk's parts straight into its image, as tests set up the
// files they read. It changes only what it is asked to, so the free list is
// left as it was.
class DiskEditor {
 public:
  explicit DiskEditor(DiskImage* image) : image_(*image) {
    const Sector superblock = ReadSector(image_, 0);
    layout_ = {
        .sectors_per_cluster = superblock[3],  // Sectors per cluster
        .cluster_count = superblock[4],        // Clusters
        .fat_sectors = superblock[5],          // FAT sectors
    };
  }

  const DiskLayout& GetLayout() const { return layout_; }

  // Sets the FAT's word `index` to `value`.
  void SetFatWord(int index, uint16_t value) {
    const int sector = 1 + index / kDiskSectorWords;
    Sector words = ReadSector(image_, sector);
    words[index % kDiskSectorWords] = value;
    WriteSector(image_, sector, words);
  }

  // Links `clusters` into a chain, in order, ending after the last.
  void SetChain(absl::Span<const int> clusters) {
    const int count = static_cast<int>(clusters.size());
    for (int i = 0; i < count; ++i) {
      SetFatWord(clusters[i], i + 1 < count
                                  ? static_cast<uint16_t>(clusters[i + 1])
                                  : 0xFFFF);
    }
  }

  // Sets entry `index` of directory sector `sector` to `entry`.
  void SetEntry(int sector, int index, const DiskDirEntry& entry) {
    Sector words = ReadSector(image_, sector);
    std::copy(entry.GetWords().begin(), entry.GetWords().end(),
              words.begin() + index * DiskDirEntry::kWordCount);
    WriteSector(image_, sector, words);
  }

  // Sets the root directory's entry `index`, in its first cluster, to `entry`.
  void SetRootEntry(int index, const DiskDirEntry& entry) {
    SetEntry(layout_.GetClusterSector(1) + index / kEntriesPerSector,
             index % kEntriesPerSector, entry);
  }

  // Fills directory sector `sector` with empty files, named from
  // GetFillerName(first_index) on.
  void FillSector(int sector, int first_index) {
    for (int i = 0; i < kEntriesPerSector; ++i) {
      SetEntry(sector, i, MakeEntry(GetFillerName(first_index + i), 0, 0));
    }
  }

  // Writes `size` words counting up from `first_word` into `clusters`, in
  // order, filling the rest of the last sector with EEEEh.
  void WriteData(absl::Span<const int> clusters, int size,
                 uint16_t first_word) {
    const int cluster_words = layout_.sectors_per_cluster * kDiskSectorWords;
    for (int start = 0; start < size; start += kDiskSectorWords) {
      Sector words = MakeFilledSector(0xEEEE);
      for (int i = 0; i < kDiskSectorWords && start + i < size; ++i) {
        words[i] = static_cast<uint16_t>(first_word + start + i);
      }
      const int cluster = clusters[static_cast<int>(start / cluster_words)];
      WriteSector(image_,
                  layout_.GetClusterSector(cluster) +
                      start % cluster_words / kDiskSectorWords,
                  words);
    }
  }

  // Adds the file `name` as the root's entry `index`, with `size` words
  // counting up from `first_word` in `clusters`, and returns its entry.
  DiskDirEntry AddFile(int index, std::string_view name,
                       std::vector<int> clusters, int size,
                       uint16_t first_word = 0, uint16_t attributes = 0) {
    SetChain(clusters);
    WriteData(clusters, size, first_word);
    const DiskDirEntry entry =
        MakeEntry(name, clusters.empty() ? 0 : clusters[0], size, attributes);
    SetRootEntry(index, entry);
    return entry;
  }

 private:
  static constexpr int kEntriesPerSector =
      kDiskSectorWords / DiskDirEntry::kWordCount;

  DiskImage& image_;
  DiskLayout layout_;
};

class DiskFileSystemTest : public ::testing::Test {
 protected:
  DiskFileSystemTest() : file_system_({}, kMaxOpenFiles) {
    std::unique_ptr<FaultyFileProtocol> protocol =
        std::make_unique<FaultyFileProtocol>();
    protocol_ = protocol.get();
    host_.Register(std::move(protocol));
  }

  // Returns a new blank image of `sector_count` sectors at `path`.
  std::unique_ptr<DiskImage> MakeImage(int sector_count,
                                       std::string_view path = kPath) {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&host_, path, sector_count);
    EXPECT_TRUE(image.ok()) << image.status();
    return image.ok() ? *std::move(image) : nullptr;
  }

  // Attaches a new image of `sector_count` sectors, and formats it with the
  // label "WORK" and `sectors_per_cluster`.
  void AttachFormatted(int sector_count, int sectors_per_cluster = 0) {
    EXPECT_EQ(file_system_.AttachImage(MakeImage(sector_count)),
              DiskStatus::kUnformatted);
    EXPECT_EQ(
        file_system_.Format(GetLabel("WORK"), sectors_per_cluster, GetNow()),
        DiskStatus::kOk);
  }

  // Detaches the formatted image, changes it with `edit`, and attaches it
  // again.
  void EditDisk(absl::FunctionRef<void(DiskEditor& editor)> edit) {
    std::unique_ptr<DiskImage> image = file_system_.DetachImage();
    DiskEditor editor(image.get());
    edit(editor);
    EXPECT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  }

  // Detaches the image, sets word `index` of its sector `sector` to `value`,
  // and attaches it again, returning what attaching does.
  DiskStatus PatchWord(int sector, int index, uint16_t value) {
    std::unique_ptr<DiskImage> image = file_system_.DetachImage();
    Sector words = ReadSector(*image, sector);
    words[index] = value;
    WriteSector(*image, sector, words);
    return file_system_.AttachImage(std::move(image));
  }

  // Detaches the image, sets its write protection, and attaches it again,
  // returning what attaching does, as changing a floppy's notch takes it out
  // of the drive.
  DiskStatus SetWriteProtected(bool write_protected) {
    std::unique_ptr<DiskImage> image = file_system_.DetachImage();
    EXPECT_EQ(image->SetWriteProtected(write_protected), DiskStatus::kOk);
    return file_system_.AttachImage(std::move(image));
  }

  // Detaches the image at kPath, discarding the buffers, lets the host
  // succeed again, and attaches the image reopened, as the retry after a host
  // failure is. Returns what attaching does.
  DiskStatus ReopenImage() {
    file_system_.DetachImage().reset();
    protocol_->SetLimits({});
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&host_, kPath);
    EXPECT_TRUE(image.ok()) << image.status();
    if (!image.ok()) {
      return DiskStatus::kNoDisk;
    }
    return file_system_.AttachImage(*std::move(image));
  }

  // Returns the free clusters GetVolumeInfo reports, or -1 if it fails.
  int GetFreeClusters() {
    DiskVolumeInfo info;
    if (file_system_.GetVolumeInfo(&info) != DiskStatus::kOk) {
      return -1;
    }
    return info.free_clusters;
  }

  // Opens `name`, returning its handle, or -1 if it fails.
  DiskFile OpenFile(std::string_view name) {
    DiskFile file = -1;
    EXPECT_EQ(file_system_.Open(GetName(name), &file), DiskStatus::kOk) << name;
    return file;
  }

  // Opens the root directory, returning its handle, or -1 if it fails.
  DiskFile OpenRoot() {
    DiskFile file = -1;
    EXPECT_EQ(file_system_.OpenRoot(&file), DiskStatus::kOk);
    return file;
  }

  // Creates `name`, modified GetLater(), returning its handle, or -1 if it
  // fails.
  DiskFile CreateFile(std::string_view name) {
    DiskFile file = -1;
    EXPECT_EQ(file_system_.Create(GetName(name), GetLater(), &file),
              DiskStatus::kOk)
        << name;
    return file;
  }

  // Returns the sectors written to images since the last call, in order, with
  // -1 for a write to an image's header.
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

  // Returns `file`'s position and size, or -1 for each if it fails.
  DiskFileInfo GetInfo(DiskFile file) {
    DiskFileInfo info = {.position = -1, .size = -1};
    EXPECT_EQ(file_system_.GetFileInfo(file, &info), DiskStatus::kOk);
    return info;
  }

  // Checks that every operation on `file` but Close returns `status`.
  void ExpectOperationsReturn(DiskFile file, DiskStatus status) {
    std::vector<uint16_t> words(10);
    int count = -1;
    EXPECT_EQ(file_system_.Read(file, absl::MakeSpan(words), &count), status);
    EXPECT_EQ(count, 0);
    EXPECT_EQ(file_system_.Seek(file, 0), status);
    DiskFileInfo info;
    EXPECT_EQ(file_system_.GetFileInfo(file, &info), status);
    DiskDirEntry entry;
    EXPECT_EQ(file_system_.Find(file, GetPattern("*.*"), &entry), status);
    count = -1;
    EXPECT_EQ(file_system_.Write(file, words, &count), status);
    EXPECT_EQ(count, 0);
    int writable = -1;
    EXPECT_EQ(file_system_.GetWritableWords(file, &writable), status);
  }

  // Reads `file` from its position to its end, asking for `chunk` words at a
  // time, and checks that each read stops at the end of its sector or the
  // file.
  std::vector<uint16_t> ReadToEnd(DiskFile file, int chunk) {
    std::vector<uint16_t> words;
    std::vector<uint16_t> buffer(chunk);
    while (true) {
      const DiskFileInfo info = GetInfo(file);
      const int expected =
          std::min({chunk, info.size - info.position,
                    kDiskSectorWords - info.position % kDiskSectorWords});
      int count = -1;
      EXPECT_EQ(file_system_.Read(file, absl::MakeSpan(buffer), &count),
                DiskStatus::kOk);
      EXPECT_EQ(count, expected) << "At " << info.position;
      if (count <= 0) {
        return words;
      }
      words.insert(words.end(), buffer.begin(), buffer.begin() + count);
    }
  }

  // Writes `words` to `file` from its position, checking that each write
  // stops at the end of its sector, and returns the first status that isn't
  // kOk, or kOk.
  DiskStatus WriteWords(DiskFile file, absl::Span<const uint16_t> words) {
    while (!words.empty()) {
      const int position = GetInfo(file).position;
      const int expected =
          std::min(static_cast<int>(words.size()),
                   kDiskSectorWords - position % kDiskSectorWords);
      int count = -1;
      const DiskStatus status = file_system_.Write(file, words, &count);
      if (status != DiskStatus::kOk) {
        EXPECT_EQ(count, 0);
        return status;
      }
      EXPECT_EQ(count, expected) << "At " << position;
      if (count <= 0) {
        return DiskStatus::kOk;
      }
      words.remove_prefix(count);
    }
    return DiskStatus::kOk;
  }

  // Returns `file`'s whole contents, read from its start.
  std::vector<uint16_t> ReadFile(DiskFile file) {
    EXPECT_EQ(file_system_.Seek(file, 0), DiskStatus::kOk);
    return ReadToEnd(file, kDiskSectorWords);
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
  for (int sector = 0; sector < kFloppySectors; ++sector) {
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
  for (int sector = 0; sector < 8; ++sector) {
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
  for (int sector = 2; sector < 6; ++sector) {
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
  for (int sector = 0; sector < kFloppySectors - 1; ++sector) {
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
  ASSERT_EQ(SetWriteProtected(true), DiskStatus::kOk);
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
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kIoError);
  EXPECT_EQ(ReopenImage(), DiskStatus::kUnformatted);
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
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kIoError);
  EXPECT_EQ(ReopenImage(), DiskStatus::kUnformatted);
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
    EXPECT_EQ(ReopenImage(), DiskStatus::kUnformatted);
    ASSERT_EQ(file_system_.Format(GetLabel("WORK"), 0, GetNow()),
              DiskStatus::kOk);
  }
}

TEST_F(DiskFileSystemTest, AFailedSetLabelLeavesTheDisksLabel) {
  AttachFormatted(kFloppySectors);
  protocol_->SetLimits({.writes = 0});
  EXPECT_EQ(file_system_.SetLabel(GetLabel("OTHER")), DiskStatus::kIoError);
  ASSERT_EQ(ReopenImage(), DiskStatus::kOk);
  DiskVolumeInfo volume;
  ASSERT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(volume.label, GetLabel("WORK"));
}

TEST_F(DiskFileSystemTest, OpeningNeedsAFileSystem) {
  DiskFile file = -1;
  EXPECT_EQ(file_system_.Open(GetName("HELLO.TXT"), &file),
            DiskStatus::kNoDisk);
  EXPECT_EQ(file_system_.OpenRoot(&file), DiskStatus::kNoDisk);
  ASSERT_EQ(file_system_.AttachImage(MakeImage(kSmallSectors)),
            DiskStatus::kUnformatted);
  EXPECT_EQ(file_system_.Open(GetName("HELLO.TXT"), &file),
            DiskStatus::kUnformatted);
  EXPECT_EQ(file_system_.OpenRoot(&file), DiskStatus::kUnformatted);
}

TEST_F(DiskFileSystemTest, OpenFindsOnlyExactNames) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.AddFile(3, "HELLO.TXT", {2}, 10); });

  DiskFile file = -1;
  EXPECT_EQ(file_system_.Open(GetName("HELLO"), &file), DiskStatus::kNotFound);
  EXPECT_EQ(file_system_.Open(GetName("HELLO.TX"), &file),
            DiskStatus::kNotFound);
  EXPECT_EQ(file_system_.Open(GetPattern("HELLO.*"), &file),
            DiskStatus::kBadName);
  ASSERT_EQ(file_system_.Open(GetName("hello.txt"), &file), DiskStatus::kOk);
  EXPECT_EQ(file, 0);
  EXPECT_EQ(GetInfo(file).size, 10);
}

TEST_F(DiskFileSystemTest, ReadingAWholeFile) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "HELLO.TXT", {2, 3, 4}, 600, 0x1000);
  });

  const DiskFile file = OpenFile("HELLO.TXT");
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 0, .size = 600}));

  // Each read stops at its sector's end, and at the file's end.
  EXPECT_EQ(ReadToEnd(file, 1000), MakeCountingWords(0x1000, 600));
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 600, .size = 600}));
  EXPECT_EQ(file_system_.Close(file), DiskStatus::kOk);
}

TEST_F(DiskFileSystemTest, ReadingAcrossClustersOfSeveralSectors) {
  // Four sectors a cluster, and a chain that goes back and forth.
  AttachFormatted(kFloppySectors, 4);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "DATA", {7, 3, 5}, 2500, 0x2000);
  });

  const DiskFile file = OpenFile("DATA");
  EXPECT_EQ(ReadToEnd(file, 100), MakeCountingWords(0x2000, 2500));
}

TEST_F(DiskFileSystemTest, ReadingAnEmptyFile) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.AddFile(0, "EMPTY", {}, 0); });

  const DiskFile file = OpenFile("EMPTY");
  EXPECT_EQ(GetInfo(file).size, 0);
  std::vector<uint16_t> words(10);
  int count = -1;
  EXPECT_EQ(file_system_.Read(file, absl::MakeSpan(words), &count),
            DiskStatus::kOk);
  EXPECT_EQ(count, 0);
}

TEST_F(DiskFileSystemTest, Seeking) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "HELLO.TXT", {2, 3, 4}, 600);
  });
  const DiskFile file = OpenFile("HELLO.TXT");
  std::vector<uint16_t> words(1000);
  int count = 0;

  // Into the second cluster, and partway into a sector.
  ASSERT_EQ(file_system_.Seek(file, 300), DiskStatus::kOk);
  EXPECT_EQ(GetInfo(file).position, 300);
  ASSERT_EQ(file_system_.Read(file, absl::MakeSpan(words), &count),
            DiskStatus::kOk);
  ASSERT_EQ(count, 212);
  EXPECT_EQ(words[0], 300);
  EXPECT_EQ(words[211], 511);

  // Back into the first cluster.
  ASSERT_EQ(file_system_.Seek(file, 10), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Read(file, absl::MakeSpan(words), &count),
            DiskStatus::kOk);
  ASSERT_EQ(count, 246);
  EXPECT_EQ(words[0], 10);

  // To the end, which reads nothing.
  ASSERT_EQ(file_system_.Seek(file, 600), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Read(file, absl::MakeSpan(words), &count),
            DiskStatus::kOk);
  EXPECT_EQ(count, 0);

  // Past the end, or before the start, the position stays.
  EXPECT_EQ(file_system_.Seek(file, 601), DiskStatus::kPastEnd);
  EXPECT_EQ(file_system_.Seek(file, -1), DiskStatus::kBadArgument);
  EXPECT_EQ(GetInfo(file).position, 600);

  // And back to the start.
  ASSERT_EQ(file_system_.Seek(file, 0), DiskStatus::kOk);
  EXPECT_EQ(ReadToEnd(file, kDiskSectorWords), MakeCountingWords(0, 600));
}

TEST_F(DiskFileSystemTest, AnOpenFileIsInUse) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "A", {2}, 10);
    editor.AddFile(1, "B", {3}, 10);
  });

  const DiskFile file = OpenFile("A");
  DiskFile other = -1;
  EXPECT_EQ(file_system_.Open(GetName("A"), &other), DiskStatus::kInUse);
  EXPECT_EQ(OpenFile("B"), 1);
  ASSERT_EQ(file_system_.Close(file), DiskStatus::kOk);
  EXPECT_EQ(OpenFile("A"), 0);
}

TEST_F(DiskFileSystemTest, AnOpenRootIsInUse) {
  AttachFormatted(kSmallSectors);
  DiskDirEntry entry;
  EditDisk([&entry](DiskEditor& editor) {
    entry = editor.AddFile(0, "HELLO.TXT", {2}, 10);
  });

  DiskFile root = -1;
  ASSERT_EQ(file_system_.OpenRoot(&root), DiskStatus::kOk);
  EXPECT_EQ(root, 0);
  EXPECT_EQ(GetInfo(root).size, kDiskSectorWords);
  DiskFile other = -1;
  EXPECT_EQ(file_system_.OpenRoot(&other), DiskStatus::kInUse);

  // It reads the raw entries.
  std::vector<uint16_t> words(DiskDirEntry::kWordCount);
  int count = 0;
  ASSERT_EQ(file_system_.Read(root, absl::MakeSpan(words), &count),
            DiskStatus::kOk);
  ASSERT_EQ(count, DiskDirEntry::kWordCount);
  EXPECT_EQ(DiskDirEntry::FromWords(words), entry);
  EXPECT_EQ(GetInfo(root).position, DiskDirEntry::kWordCount);

  // Files in it still open, and it opens again once closed.
  EXPECT_EQ(OpenFile("HELLO.TXT"), 1);
  ASSERT_EQ(file_system_.Close(root), DiskStatus::kOk);
  EXPECT_EQ(OpenRoot(), 0);
}

TEST_F(DiskFileSystemTest, SlotsRunOut) {
  AttachFormatted(kSmallSectors);
  const std::vector<std::string_view> names = {"A", "B", "C", "D", "E"};
  EditDisk([&names](DiskEditor& editor) {
    for (int i = 0; i < std::ssize(names); ++i) {
      editor.AddFile(i, names[i], {}, 0);
    }
  });

  for (int i = 0; i < kMaxOpenFiles; ++i) {
    EXPECT_EQ(OpenFile(names[i]), i);
  }
  DiskFile file = -1;
  EXPECT_EQ(file_system_.OpenRoot(&file), DiskStatus::kTooManyOpen);
  EXPECT_EQ(file_system_.Open(GetName(names[kMaxOpenFiles]), &file),
            DiskStatus::kTooManyOpen);

  // The lowest free slot is taken.
  ASSERT_EQ(file_system_.Close(2), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Close(1), DiskStatus::kOk);
  EXPECT_EQ(OpenRoot(), 1);
  EXPECT_EQ(OpenFile(names[kMaxOpenFiles]), 2);
}

TEST_F(DiskFileSystemTest, BadHandles) {
  AttachFormatted(kSmallSectors);
  ASSERT_EQ(OpenRoot(), 0);
  ASSERT_EQ(file_system_.Close(0), DiskStatus::kOk);

  for (DiskFile file : {-1, 0, kMaxOpenFiles}) {
    SCOPED_TRACE(file);
    ExpectOperationsReturn(file, DiskStatus::kBadHandle);
    EXPECT_EQ(file_system_.Close(file), DiskStatus::kBadHandle);
  }
}

TEST_F(DiskFileSystemTest, SlotsOpenAcrossADetachAreStaleUntilClosed) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.AddFile(0, "HELLO.TXT", {2}, 10); });
  ASSERT_EQ(OpenFile("HELLO.TXT"), 0);
  ASSERT_EQ(OpenRoot(), 1);

  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  for (DiskFile file : {0, 1}) {
    SCOPED_TRACE(file);
    ExpectOperationsReturn(file, DiskStatus::kDiskChanged);
  }

  // Attaching the same image again doesn't revive them, and they stay taken.
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  ExpectOperationsReturn(0, DiskStatus::kDiskChanged);
  EXPECT_EQ(OpenFile("HELLO.TXT"), 2);
  EXPECT_EQ(OpenRoot(), 3);
  DiskFile file = -1;
  EXPECT_EQ(file_system_.OpenRoot(&file), DiskStatus::kTooManyOpen);

  // Closing frees a stale slot, once.
  EXPECT_EQ(file_system_.Close(0), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Close(0), DiskStatus::kBadHandle);
  ASSERT_EQ(file_system_.Close(3), DiskStatus::kOk);
  EXPECT_EQ(OpenRoot(), 0);
}

TEST_F(DiskFileSystemTest, AttachingOverAnImageMakesItsSlotsStale) {
  AttachFormatted(kSmallSectors);
  ASSERT_EQ(OpenRoot(), 0);
  ASSERT_EQ(file_system_.AttachImage(MakeImage(kSmallSectors, kOtherPath)),
            DiskStatus::kUnformatted);
  EXPECT_EQ(file_system_.Seek(0, 0), DiskStatus::kDiskChanged);
}

TEST_F(DiskFileSystemTest, FormatIsInUseWhileAFileIsOpen) {
  AttachFormatted(kSmallSectors);
  ASSERT_EQ(OpenRoot(), 0);
  EXPECT_EQ(file_system_.Format(GetLabel("OTHER"), 0, GetNow()),
            DiskStatus::kInUse);
  ASSERT_EQ(file_system_.Close(0), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Format(GetLabel("OTHER"), 0, GetNow()),
            DiskStatus::kOk);

  // A stale slot is another disk's, so it doesn't stop a format, and stays
  // stale.
  ASSERT_EQ(OpenRoot(), 0);
  ASSERT_EQ(file_system_.AttachImage(file_system_.DetachImage()),
            DiskStatus::kOk);
  EXPECT_EQ(file_system_.Format(GetLabel("WORK"), 0, GetNow()),
            DiskStatus::kOk);
  EXPECT_EQ(file_system_.Seek(0, 0), DiskStatus::kDiskChanged);
}

TEST_F(DiskFileSystemTest, FindListsMatchesInOrder) {
  // Four sectors a cluster gives the root 64 entries over four sectors.
  AttachFormatted(kFloppySectors, 4);
  const DiskDirEntry a = MakeEntry("A.TXT", 0, 0);
  const DiskDirEntry b = MakeEntry("B.DAT", 0, 0);
  const DiskDirEntry c = MakeEntry("C.TXT", 0, 0);
  const DiskDirEntry hidden = MakeEntry("HIDDEN.TXT", 0, 0, kDiskAttribHidden);
  const DiskDirEntry last = MakeEntry("LAST.TXT", 0, 0);
  EditDisk([&](DiskEditor& editor) {
    editor.SetRootEntry(0, a);
    editor.SetRootEntry(5, b);
    editor.SetRootEntry(17, c);
    editor.SetRootEntry(40, hidden);
    editor.SetRootEntry(63, last);

    // An entry whose name breaks the rules never matches.
    DiskDirEntry::Words broken = {};
    broken[0] = '.';
    editor.SetRootEntry(20, DiskDirEntry::FromWords(broken));
  });

  DiskFile root = -1;
  ASSERT_EQ(file_system_.OpenRoot(&root), DiskStatus::kOk);
  const int size = GetInfo(root).size;
  ASSERT_EQ(size, 4 * kDiskSectorWords);
  DiskDirEntry entry;
  for (const DiskDirEntry& expected : {a, c, hidden, last}) {
    ASSERT_EQ(file_system_.Find(root, GetPattern("*.TXT"), &entry),
              DiskStatus::kOk);
    EXPECT_EQ(entry, expected) << entry.GetName()->ToString();
  }
  EXPECT_EQ(file_system_.Find(root, GetPattern("*.TXT"), &entry),
            DiskStatus::kNotFound);
  EXPECT_EQ(GetInfo(root).position, size);
  EXPECT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
            DiskStatus::kNotFound);

  // Seeking to 0 starts again, and the position is just past each match.
  ASSERT_EQ(file_system_.Seek(root, 0), DiskStatus::kOk);
  for (const DiskDirEntry& expected : {a, b, c, hidden, last}) {
    ASSERT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
              DiskStatus::kOk);
    EXPECT_EQ(entry, expected) << entry.GetName()->ToString();
  }
  EXPECT_EQ(GetInfo(root).position, size);

  // A plain name matches itself, and a position inside an entry starts at the
  // next one.
  ASSERT_EQ(file_system_.Seek(root, 0), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Find(root, GetName("B.DAT"), &entry), DiskStatus::kOk);
  EXPECT_EQ(entry, b);
  EXPECT_EQ(GetInfo(root).position, 6 * DiskDirEntry::kWordCount);
  ASSERT_EQ(file_system_.Seek(root, 1), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
            DiskStatus::kOk);
  EXPECT_EQ(entry, b);
}

TEST_F(DiskFileSystemTest, FindNeedsADirectory) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.AddFile(0, "HELLO.TXT", {2}, 10); });
  const DiskFile file = OpenFile("HELLO.TXT");
  DiskDirEntry entry;
  EXPECT_EQ(file_system_.Find(file, GetPattern("*.*"), &entry),
            DiskStatus::kWrongHandleType);
}

TEST_F(DiskFileSystemTest, TheRootOverSeveralClusters) {
  // The root's chain is 1 then 5, with a file in the second cluster.
  AttachFormatted(kSmallSectors);
  const DiskDirEntry far = MakeEntry("FAR.TXT", 2, 20);
  EditDisk([&far](DiskEditor& editor) {
    editor.SetChain({1, 5});
    editor.AddFile(1, "NEAR.TXT", {}, 0);
    editor.SetChain({2});
    editor.WriteData({2}, 20, 0x3000);
    editor.SetEntry(editor.GetLayout().GetClusterSector(5), 3, far);
  });

  DiskFile root = -1;
  ASSERT_EQ(file_system_.OpenRoot(&root), DiskStatus::kOk);
  EXPECT_EQ(GetInfo(root).size, 2 * kDiskSectorWords);
  DiskDirEntry entry;
  ASSERT_EQ(file_system_.Find(root, GetPattern("F*.*"), &entry),
            DiskStatus::kOk);
  EXPECT_EQ(entry, far);
  EXPECT_EQ(GetInfo(root).position,
            kDiskSectorWords + 4 * DiskDirEntry::kWordCount);

  const DiskFile file = OpenFile("FAR.TXT");
  EXPECT_EQ(ReadToEnd(file, kDiskSectorWords), MakeCountingWords(0x3000, 20));
}

TEST_F(DiskFileSystemTest, SequentialReadsFollowTheChainWithoutWalking) {
  // On a floppy with three FAT sectors, the chain 300, 600, 301 has its links
  // in FAT sectors 2 and 3, so each step to the next cluster loads one, and
  // walking from the start would load sector 2 again. The disk is protected,
  // so opening doesn't count the free list, which would load FAT sectors too.
  AttachFormatted(kFloppySectors);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  DiskEditor(image.get()).AddFile(0, "DATA", {300, 600, 301}, 768);
  ASSERT_EQ(image->SetWriteProtected(true), DiskStatus::kOk);

  DiskFileSystem timed(
      {
          .read_words_per_sec = kDiskSectorWords,
          .latency = kLatency,
      },
      kMaxOpenFiles);
  ASSERT_EQ(timed.AttachImage(std::move(image)), DiskStatus::kOk);
  DiskFile file = -1;
  ASSERT_EQ(timed.Open(GetName("DATA"), &file), DiskStatus::kOk);
  timed.CollectBusyTime();

  std::vector<uint16_t> words(kDiskSectorWords);
  int count = 0;
  for (int i = 0; i < 3; ++i) {
    ASSERT_EQ(timed.Read(file, absl::MakeSpan(words), &count), DiskStatus::kOk);
    ASSERT_EQ(count, kDiskSectorWords);
  }

  // The three data sectors, and FAT sectors 2 and 3, none continuous.
  EXPECT_EQ(timed.CollectBusyTime(), 5 * (kLatency + kRead));
}

TEST_F(DiskFileSystemTest, OpeningKeepsTheRootSectorUntilDetached) {
  // The disk is protected, so opening doesn't count the free list.
  AttachFormatted(kSmallSectors);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  DiskEditor editor(image.get());
  editor.AddFile(0, "A", {}, 0);
  editor.AddFile(1, "B", {}, 0);
  ASSERT_EQ(image->SetWriteProtected(true), DiskStatus::kOk);

  DiskFileSystem timed({.read_words_per_sec = kDiskSectorWords}, kMaxOpenFiles);
  ASSERT_EQ(timed.AttachImage(std::move(image)), DiskStatus::kOk);
  DiskFile file = -1;
  ASSERT_EQ(timed.Open(GetName("A"), &file), DiskStatus::kOk);
  EXPECT_GT(timed.CollectBusyTime(), absl::ZeroDuration());
  ASSERT_EQ(timed.Open(GetName("B"), &file), DiskStatus::kOk);
  EXPECT_EQ(timed.CollectBusyTime(), absl::ZeroDuration());

  // A detached disk's sector is forgotten.
  image = timed.DetachImage();
  ASSERT_EQ(image->SetWriteProtected(false), DiskStatus::kOk);
  DiskEditor(image.get()).AddFile(2, "C", {}, 0);
  ASSERT_EQ(timed.AttachImage(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(timed.Open(GetName("C"), &file), DiskStatus::kOk);
}

TEST_F(DiskFileSystemTest, BrokenChainsAreCorrupt) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "SHORT", {2, 5}, 300);
    editor.SetFatWord(2, 0xFFFF);
    editor.AddFile(1, "OUT", {3, 4}, 300);
    editor.SetFatWord(3, kSmallClusters + 1);
  });

  // Each fails moving into its second cluster, and stays where it was.
  std::vector<uint16_t> words(kDiskSectorWords);
  for (std::string_view name : {"SHORT", "OUT"}) {
    SCOPED_TRACE(name);
    const DiskFile file = OpenFile(name);
    int count = 0;
    ASSERT_EQ(file_system_.Read(file, absl::MakeSpan(words), &count),
              DiskStatus::kOk);
    EXPECT_EQ(file_system_.Read(file, absl::MakeSpan(words), &count),
              DiskStatus::kCorrupt);
    EXPECT_EQ(count, 0);
    EXPECT_EQ(GetInfo(file).position, kDiskSectorWords);
  }
}

TEST_F(DiskFileSystemTest, EntriesThatCantBeOnTheDiskAreCorrupt) {
  struct BadEntry {
    int first_cluster;
    int size;
  };
  const BadEntry kBadEntries[] = {
      {kSmallClusters + 1, 10},  // First cluster
      {2, 0},                    // A cluster but no size
      {0, 10},                   // A size but no cluster
      // Bigger than the disk without the root
      {2, (kSmallClusters - 1) * kDiskSectorWords + 1},
  };
  AttachFormatted(kSmallSectors);
  for (const BadEntry& bad : kBadEntries) {
    SCOPED_TRACE(bad.first_cluster);
    SCOPED_TRACE(bad.size);
    EditDisk([&bad](DiskEditor& editor) {
      editor.SetRootEntry(0, MakeEntry("BAD", bad.first_cluster, bad.size));
    });
    DiskFile file = -1;
    EXPECT_EQ(file_system_.Open(GetName("BAD"), &file), DiskStatus::kCorrupt);
  }

  // A size no disk holds can only come from the disk's own words.
  EditDisk([](DiskEditor& editor) {
    DiskDirEntry::Words words = MakeEntry("HUGE", 2, 10).GetWords();
    words[10] = words[11] = 0xFFFF;
    editor.SetRootEntry(0, DiskDirEntry::FromWords(words));
  });
  DiskFile file = -1;
  EXPECT_EQ(file_system_.Open(GetName("HUGE"), &file), DiskStatus::kCorrupt);

  // Every cluster but the root's is the most a file can have.
  EditDisk([](DiskEditor& editor) {
    editor.SetRootEntry(
        0, MakeEntry("BIG", 2, (kSmallClusters - 1) * kDiskSectorWords));
  });
  EXPECT_EQ(OpenFile("BIG"), 0);
}

TEST_F(DiskFileSystemTest, BrokenRootChainsAreCorrupt) {
  const uint16_t kBadLinks[] = {
      1,                   // A loop
      kSmallClusters + 1,  // Out of range
  };
  AttachFormatted(kSmallSectors);
  for (uint16_t link : kBadLinks) {
    SCOPED_TRACE(link);
    ASSERT_EQ(PatchWord(1, 1, link), DiskStatus::kOk);
    DiskFile file = -1;
    EXPECT_EQ(file_system_.OpenRoot(&file), DiskStatus::kCorrupt);
    EXPECT_EQ(file_system_.Open(GetName("HELLO.TXT"), &file),
              DiskStatus::kCorrupt);
  }
}

TEST_F(DiskFileSystemTest, OpenCountsTheFreeListOnlyOnAWritableDisk) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "HELLO.TXT", {}, 0);
    editor.SetFatWord(0, kSmallClusters + 1);
  });

  DiskFile file = -1;
  EXPECT_EQ(file_system_.Open(GetName("HELLO.TXT"), &file),
            DiskStatus::kCorrupt);
  ASSERT_EQ(SetWriteProtected(true), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Open(GetName("HELLO.TXT"), &file), DiskStatus::kOk);
}

TEST_F(DiskFileSystemTest, ReadFailsWhenTheHostReadFails) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.AddFile(0, "HELLO.TXT", {2}, 10); });
  const DiskFile file = OpenFile("HELLO.TXT");

  protocol_->SetLimits({.reads = 0});
  std::vector<uint16_t> words(10);
  int count = -1;
  EXPECT_EQ(file_system_.Read(file, absl::MakeSpan(words), &count),
            DiskStatus::kIoError);
  EXPECT_EQ(count, 0);
}

TEST_F(DiskFileSystemTest, AFailedImageFailsEveryOperationUntilReattached) {
  AttachFormatted(kSmallSectors);
  EditDisk(
      [](DiskEditor& editor) { editor.AddFile(0, "HELLO.TXT", {2}, 10, 1); });
  const std::vector<uint16_t> data = MakeCountingWords(1, 10);
  const DiskFile file = OpenFile("HELLO.TXT");
  const DiskFile root = OpenRoot();
  const DiskFile created = CreateFile("NEW");

  // Every buffer, and the free count, could answer what follows.
  ASSERT_EQ(ReadFile(file), data);
  DiskDirEntry entry;
  ASSERT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
            DiskStatus::kOk);
  int writable = 0;
  ASSERT_EQ(file_system_.GetWritableWords(created, &writable), DiskStatus::kOk);

  protocol_->SetLimits({.writes = 0});
  DiskFile other = -1;
  EXPECT_EQ(file_system_.Create(GetName("FAILED"), GetLater(), &other),
            DiskStatus::kIoError);
  protocol_->SetLimits({});

  ExpectOperationsReturn(file, DiskStatus::kIoError);
  ExpectOperationsReturn(root, DiskStatus::kIoError);
  ExpectOperationsReturn(created, DiskStatus::kIoError);
  EXPECT_EQ(file_system_.Open(GetName("HELLO.TXT"), &other),
            DiskStatus::kIoError);
  EXPECT_EQ(file_system_.Create(GetName("MORE"), GetLater(), &other),
            DiskStatus::kIoError);
  DiskVolumeInfo volume;
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kIoError);
  EXPECT_EQ(file_system_.SetLabel(GetLabel("OTHER")), DiskStatus::kIoError);
  EXPECT_EQ(file_system_.Format(GetLabel("OTHER"), 0, GetNow()),
            DiskStatus::kIoError);

  // Close still frees the slot.
  EXPECT_EQ(file_system_.Close(file), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Close(file), DiskStatus::kBadHandle);

  // Reattaching the reopened image is the retry: what reached the disk before
  // the failure is there, and the slots still open are stale.
  ASSERT_EQ(ReopenImage(), DiskStatus::kOk);
  ExpectOperationsReturn(root, DiskStatus::kDiskChanged);
  EXPECT_EQ(file_system_.Close(root), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Close(created), DiskStatus::kOk);
  EXPECT_EQ(ReadFile(OpenFile("HELLO.TXT")), data);
  EXPECT_EQ(GetInfo(OpenFile("NEW")).size, 0);
  EXPECT_EQ(file_system_.Open(GetName("FAILED"), &other),
            DiskStatus::kNotFound);
}

TEST_F(DiskFileSystemTest, CreateMakesAnEmptyOpenFile) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "FOO", {}, 0);
    editor.AddFile(2, "BAR", {}, 0);
  });
  const int free_clusters = GetFreeClusters();
  CollectSectorWrites();

  const DiskFile file = CreateFile("new.txt");
  EXPECT_EQ(file, 0);
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 0, .size = 0}));
  DiskFile other = -1;
  EXPECT_EQ(file_system_.Open(GetName("NEW.TXT"), &other), DiskStatus::kInUse);

  // Only the root's sector is written, and no cluster is taken.
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(2));
  EXPECT_EQ(GetFreeClusters(), free_clusters);

  ASSERT_EQ(file_system_.Close(file), DiskStatus::kOk);
  const DiskFile reopened = OpenFile("NEW.TXT");
  EXPECT_EQ(GetInfo(reopened).size, 0);
  EXPECT_THAT(ReadToEnd(reopened, 10), IsEmpty());

  // The entry went into the free entry between the others.
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 2, 1), MakeCreatedEntry("NEW.TXT"));
  EXPECT_EQ(ReadEntry(*image, 2, 2), MakeEntry("BAR", 0, 0));
  EXPECT_TRUE(ReadEntry(*image, 2, 3).IsFree());
}

TEST_F(DiskFileSystemTest, CreateFillsTheLastEntryOfASector) {
  // The root is one cluster of four sectors.
  AttachFormatted(kFloppySectors, 4);
  EditDisk([](DiskEditor& editor) {
    for (int i = 0; i < 15; ++i) {
      editor.AddFile(i, GetFillerName(i), {}, 0);
    }
  });
  CollectSectorWrites();
  ASSERT_EQ(file_system_.Close(CreateFile("LAST")), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(2));
  ASSERT_EQ(file_system_.Close(CreateFile("NEXT")), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(3));

  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 2, 15), MakeCreatedEntry("LAST"));
  EXPECT_EQ(ReadEntry(*image, 3, 0), MakeCreatedEntry("NEXT"));
}

TEST_F(DiskFileSystemTest, CreateRefusals) {
  DiskFile file = -1;
  EXPECT_EQ(file_system_.Create(GetName("A"), GetLater(), &file),
            DiskStatus::kNoDisk);
  ASSERT_EQ(file_system_.AttachImage(MakeImage(kSmallSectors, kOtherPath)),
            DiskStatus::kUnformatted);
  EXPECT_EQ(file_system_.Create(GetName("A"), GetLater(), &file),
            DiskStatus::kUnformatted);

  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.AddFile(0, "OLD", {}, 0); });
  CollectSectorWrites();
  ASSERT_EQ(SetWriteProtected(true), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Create(GetName("A"), GetLater(), &file),
            DiskStatus::kWriteProtected);
  ASSERT_EQ(SetWriteProtected(false), DiskStatus::kOk);
  CollectSectorWrites();

  EXPECT_EQ(file_system_.Create(GetPattern("A*"), GetLater(), &file),
            DiskStatus::kBadName);
  EXPECT_EQ(file_system_.Create(GetName("OLD"), GetLater(), &file),
            DiskStatus::kExists);

  // A name is taken whether its file is open or not.
  const DiskFile a = CreateFile("A");
  CollectSectorWrites();
  EXPECT_EQ(file_system_.Create(GetName("A"), GetLater(), &file),
            DiskStatus::kExists);
  ASSERT_EQ(file_system_.Close(a), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Create(GetName("A"), GetLater(), &file),
            DiskStatus::kExists);

  for (int i = 0; i < kMaxOpenFiles; ++i) {
    CreateFile(GetFillerName(i));
  }
  CollectSectorWrites();
  EXPECT_EQ(file_system_.Create(GetName("B"), GetLater(), &file),
            DiskStatus::kTooManyOpen);
  EXPECT_EQ(file, -1);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
}

TEST_F(DiskFileSystemTest, CreateNeedsTheFreeList) {
  AttachFormatted(kSmallSectors);
  EditDisk(
      [](DiskEditor& editor) { editor.SetFatWord(0, kSmallClusters + 1); });
  DiskFile file = -1;
  EXPECT_EQ(file_system_.Create(GetName("A"), GetLater(), &file),
            DiskStatus::kCorrupt);
}

TEST_F(DiskFileSystemTest, CreateGrowsAFullRoot) {
  // The free list's head is cluster 2, at sector 5, which holds old words.
  AttachFormatted(kFloppySectors);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  DiskEditor(image.get()).FillSector(4, 0);
  WriteSector(*image, 5, MakeFilledSector(0xEEEE));
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), 715);
  CollectSectorWrites();

  ASSERT_EQ(file_system_.Close(CreateFile("NEW")), DiskStatus::kOk);

  // The new cluster is written, zeroed but for the entry, then the FAT links
  // it.
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(5, 1));
  EXPECT_EQ(GetFreeClusters(), 714);

  image = file_system_.DetachImage();
  const Sector fat = ReadSector(*image, 1);
  EXPECT_EQ(fat[0], 3);
  EXPECT_EQ(fat[1], 2);
  EXPECT_EQ(fat[2], 0xFFFF);
  EXPECT_EQ(ReadEntry(*image, 5, 0), MakeCreatedEntry("NEW"));
  Sector rest = ReadSector(*image, 5);
  std::fill_n(rest.begin(), DiskDirEntry::kWordCount, 0);
  EXPECT_THAT(rest, Each(0));

  // It is all on the disk.
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), 714);
  EXPECT_EQ(GetInfo(OpenRoot()).size, 2 * kDiskSectorWords);
  OpenFile("NEW");
}

TEST_F(DiskFileSystemTest, CreateGrowsARootOfFourSectorClusters) {
  // The root is sectors 2 to 5, and the free list's head is 6 to 9.
  AttachFormatted(kFloppySectors, 4);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  for (int sector = 2; sector < 6; ++sector) {
    DiskEditor(image.get()).FillSector(sector, (sector - 2) * 16);
  }
  for (int sector = 6; sector < 10; ++sector) {
    WriteSector(*image, sector, MakeFilledSector(0xEEEE));
  }
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  CollectSectorWrites();

  ASSERT_EQ(file_system_.Close(CreateFile("NEW")), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(6, 7, 8, 9, 1));
  EXPECT_EQ(GetFreeClusters(), 177);

  image = file_system_.DetachImage();
  EXPECT_EQ(ReadSector(*image, 1)[1], 2);
  EXPECT_EQ(ReadEntry(*image, 6, 0), MakeCreatedEntry("NEW"));
  EXPECT_TRUE(ReadEntry(*image, 6, 1).IsFree());
  for (int sector = 7; sector < 10; ++sector) {
    EXPECT_THAT(ReadSector(*image, sector), Each(0)) << "Sector " << sector;
  }
}

TEST_F(DiskFileSystemTest, GrowingWritesTheNewEndBeforeALinkInALaterSector) {
  // The root's chain is 1 then 300, whose FAT word is in sector 2, and the
  // free list skips 300. The head, 2, ends in sector 1.
  AttachFormatted(kFloppySectors);
  EditDisk([](DiskEditor& editor) {
    editor.SetChain({1, 300});
    editor.SetFatWord(299, 301);
    editor.FillSector(4, 0);
    editor.FillSector(303, 16);
  });
  EXPECT_EQ(GetFreeClusters(), 714);
  CollectSectorWrites();

  ASSERT_EQ(file_system_.Close(CreateFile("NEW")), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(5, 1, 2));

  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadSector(*image, 1)[0], 3);
  EXPECT_EQ(ReadSector(*image, 1)[2], 0xFFFF);
  EXPECT_EQ(ReadSector(*image, 2)[300 - kDiskSectorWords], 2);
  EXPECT_EQ(ReadEntry(*image, 5, 0), MakeCreatedEntry("NEW"));
}

TEST_F(DiskFileSystemTest, GrowingWritesANewEndInALaterSectorBeforeTheLink) {
  // The free list starts at 400, whose FAT word is in sector 2, so the root's
  // link to it, in sector 1, must wait for its end.
  AttachFormatted(kFloppySectors);
  EditDisk([](DiskEditor& editor) {
    editor.SetFatWord(0, 400);
    editor.FillSector(4, 0);
  });
  EXPECT_EQ(GetFreeClusters(), 317);
  CollectSectorWrites();

  ASSERT_EQ(file_system_.Close(CreateFile("NEW")), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(403, 1, 2, 1));
  EXPECT_EQ(GetFreeClusters(), 316);

  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadSector(*image, 1)[0], 401);
  EXPECT_EQ(ReadSector(*image, 1)[1], 400);
  EXPECT_EQ(ReadSector(*image, 2)[400 - kDiskSectorWords], 0xFFFF);
  EXPECT_EQ(ReadEntry(*image, 403, 0), MakeCreatedEntry("NEW"));
}

TEST_F(DiskFileSystemTest, AnOpenRootSeesCreatedEntries) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.AddFile(0, "FOO", {}, 0); });
  const DiskFile root = OpenRoot();
  DiskDirEntry entry;
  ASSERT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
            DiskStatus::kOk);

  // The root handle holds the sector the entry is written to.
  ASSERT_EQ(file_system_.Close(CreateFile("BAR")), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
            DiskStatus::kOk);
  EXPECT_EQ(entry, MakeCreatedEntry("BAR"));

  // Filling the root's one sector, and growing it.
  for (int i = 2; i < 16; ++i) {
    ASSERT_EQ(file_system_.Close(CreateFile(GetFillerName(i))),
              DiskStatus::kOk);
  }
  EXPECT_EQ(GetInfo(root).size, kDiskSectorWords);
  ASSERT_EQ(file_system_.Close(CreateFile("GROWN")), DiskStatus::kOk);
  EXPECT_EQ(GetInfo(root).size, 2 * kDiskSectorWords);
  ASSERT_EQ(file_system_.Find(root, GetPattern("GROWN"), &entry),
            DiskStatus::kOk);
  EXPECT_EQ(GetInfo(root).position,
            kDiskSectorWords + DiskDirEntry::kWordCount);
}

TEST_F(DiskFileSystemTest, AFullRootOnAFullDiskIsDiskFull) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.FillSector(2, 0);
    editor.SetFatWord(0, 0xFFFF);
  });
  EXPECT_EQ(GetFreeClusters(), 0);
  CollectSectorWrites();

  DiskFile file = -1;
  EXPECT_EQ(file_system_.Create(GetName("NEW"), GetLater(), &file),
            DiskStatus::kDiskFull);
  EXPECT_EQ(file, -1);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  EXPECT_EQ(OpenRoot(), 0);
  EXPECT_EQ(GetInfo(0).size, kDiskSectorWords);
}

TEST_F(DiskFileSystemTest, CreateFailsWhenTheHostWriteFails) {
  AttachFormatted(kSmallSectors);
  protocol_->SetLimits({.writes = 0});
  DiskFile file = -1;
  EXPECT_EQ(file_system_.Create(GetName("NEW"), GetLater(), &file),
            DiskStatus::kIoError);
  EXPECT_EQ(file, -1);
  ASSERT_EQ(ReopenImage(), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Open(GetName("NEW"), &file), DiskStatus::kNotFound);
}

TEST_F(DiskFileSystemTest, AFailedGrowLeavesTheRootOnTheDisk) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.FillSector(2, 0); });
  const int free_clusters = GetFreeClusters();

  // The new cluster is written, but not the FAT linking it.
  protocol_->SetLimits({.writes = 1});
  DiskFile file = -1;
  EXPECT_EQ(file_system_.Create(GetName("NEW"), GetLater(), &file),
            DiskStatus::kIoError);
  ASSERT_EQ(ReopenImage(), DiskStatus::kOk);
  EXPECT_EQ(GetInfo(OpenRoot()).size, kDiskSectorWords);
  EXPECT_EQ(GetFreeClusters(), free_clusters);

  // The retry grows it.
  CreateFile("NEW");
  EXPECT_EQ(GetFreeClusters(), free_clusters - 1);
}

TEST_F(DiskFileSystemTest, AFailedCreateKeepsAnotherFilesClusters) {
  // The file fills the root's last entry, and then the disk.
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    for (int i = 0; i < 15; ++i) {
      editor.AddFile(i, GetFillerName(i), {}, 0);
    }
  });
  const DiskFile file = CreateFile("NEW");
  const std::vector<uint16_t> data =
      MakeCountingWords(1, (kSmallClusters - 1) * kDiskSectorWords);
  ASSERT_EQ(WriteWords(file, data), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), 0);

  // The root can't grow. The FAT buffer still holds the file's clusters,
  // which aren't on the disk yet.
  DiskFile other = -1;
  EXPECT_EQ(file_system_.Create(GetName("MORE"), GetLater(), &other),
            DiskStatus::kDiskFull);
  EXPECT_EQ(GetFreeClusters(), 0);
  EXPECT_EQ(ReadFile(file), data);
}

TEST_F(DiskFileSystemTest, WritingAcrossSectorsAndClusters) {
  // Clusters of four sectors.
  AttachFormatted(kFloppySectors, 4);
  const int free_clusters = GetFreeClusters();
  const DiskFile file = CreateFile("NEW");
  const std::vector<uint16_t> data = MakeCountingWords(1, 1500);
  for (int start = 0; start < 1500; start += 100) {
    ASSERT_EQ(WriteWords(file, absl::MakeConstSpan(data).subspan(start, 100)),
              DiskStatus::kOk);
  }
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 1500, .size = 1500}));
  EXPECT_EQ(GetFreeClusters(), free_clusters - 2);
  EXPECT_EQ(ReadFile(file), data);
}

TEST_F(DiskFileSystemTest, AnEmptyWriteChangesNothing) {
  AttachFormatted(kSmallSectors);
  const int free_clusters = GetFreeClusters();
  const DiskFile file = CreateFile("NEW");
  CollectSectorWrites();
  int count = -1;
  EXPECT_EQ(file_system_.Write(file, {}, &count), DiskStatus::kOk);
  EXPECT_EQ(count, 0);
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 0, .size = 0}));
  EXPECT_EQ(GetFreeClusters(), free_clusters);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
}

TEST_F(DiskFileSystemTest, AFreshSectorIsntRead) {
  // Clusters 2 and 3 are sectors 3 and 4.
  AttachFormatted(kSmallSectors);
  const DiskFile file = CreateFile("NEW");
  CollectSectorWrites();

  // The FAT's sector is already in its buffer.
  protocol_->SetLimits({.reads = 0});
  const std::vector<uint16_t> data = MakeCountingWords(1, 300);
  ASSERT_EQ(WriteWords(file, data), DiskStatus::kOk);

  // The first sector is written as the buffer moves on.
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(3));

  // Overwriting a whole sector doesn't read it either.
  ASSERT_EQ(file_system_.Seek(file, 0), DiskStatus::kOk);
  const std::vector<uint16_t> overwrite = MakeCountingWords(5000, 256);
  ASSERT_EQ(WriteWords(file, overwrite), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(4));

  protocol_->SetLimits({});
  std::vector<uint16_t> expected = overwrite;
  expected.insert(expected.end(), data.begin() + 256, data.end());
  EXPECT_EQ(ReadFile(file), expected);
}

TEST_F(DiskFileSystemTest, OverwritingToTheEndDoesntReadTheSector) {
  // Cluster 3 is sector 4, holding the file's last 44 words.
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OLD", {2, 3}, 300, 1000);
    editor.SetFatWord(0, 4);
  });
  const DiskFile file = OpenFile("OLD");
  ASSERT_EQ(file_system_.Seek(file, 256), DiskStatus::kOk);
  protocol_->SetLimits({.reads = 0});
  const std::vector<uint16_t> data = MakeCountingWords(1, 44);
  ASSERT_EQ(WriteWords(file, data), DiskStatus::kOk);
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 300, .size = 300}));

  protocol_->SetLimits({});
  std::vector<uint16_t> expected = MakeCountingWords(1000, 256);
  expected.insert(expected.end(), data.begin(), data.end());
  EXPECT_EQ(ReadFile(file), expected);
}

TEST_F(DiskFileSystemTest, OverwritingInsideAFile) {
  // Clusters 2 to 4 are sectors 3 to 5.
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OLD", {2, 3, 4}, 600, 1000);
    editor.SetFatWord(0, 5);
  });
  EXPECT_EQ(GetFreeClusters(), 6);
  const DiskFile file = OpenFile("OLD");
  CollectSectorWrites();

  // Both sectors are read, as the write covers only part of each.
  ASSERT_EQ(file_system_.Seek(file, 100), DiskStatus::kOk);
  const std::vector<uint16_t> data = MakeCountingWords(1, 300);
  ASSERT_EQ(WriteWords(file, data), DiskStatus::kOk);
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 400, .size = 600}));
  EXPECT_EQ(GetFreeClusters(), 6);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(3));

  std::vector<uint16_t> expected = MakeCountingWords(1000, 600);
  std::copy(data.begin(), data.end(), expected.begin() + 100);
  EXPECT_EQ(ReadFile(file), expected);
}

TEST_F(DiskFileSystemTest, WritingPastTheEndGrowsTheFile) {
  // Clusters 2 to 4 are sectors 3 to 5.
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OLD", {2, 3}, 300, 1000);
    editor.SetFatWord(0, 4);
  });
  EXPECT_EQ(GetFreeClusters(), 7);
  const DiskFile file = OpenFile("OLD");
  CollectSectorWrites();

  // The write fills the last cluster, then takes the free list's head.
  ASSERT_EQ(file_system_.Seek(file, 300), DiskStatus::kOk);
  const std::vector<uint16_t> data = MakeCountingWords(1, 300);
  ASSERT_EQ(WriteWords(file, data), DiskStatus::kOk);
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 600, .size = 600}));
  EXPECT_EQ(GetFreeClusters(), 6);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(4));

  std::vector<uint16_t> expected = MakeCountingWords(1000, 300);
  expected.insert(expected.end(), data.begin(), data.end());
  EXPECT_EQ(ReadFile(file), expected);
}

TEST_F(DiskFileSystemTest, GrowingFollowsTheChainPastTheSize) {
  // Cluster 4 is slack past the size, and the free list starts after it.
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OLD", {2, 3, 4}, 300, 1000);
    editor.SetFatWord(0, 5);
  });
  EXPECT_EQ(GetFreeClusters(), 6);
  const DiskFile file = OpenFile("OLD");
  CollectSectorWrites();

  ASSERT_EQ(file_system_.Seek(file, 300), DiskStatus::kOk);
  const std::vector<uint16_t> data = MakeCountingWords(1, 300);
  ASSERT_EQ(WriteWords(file, data), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), 6);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(4));

  std::vector<uint16_t> expected = MakeCountingWords(1000, 300);
  expected.insert(expected.end(), data.begin(), data.end());
  EXPECT_EQ(ReadFile(file), expected);
}

TEST_F(DiskFileSystemTest, FillingTheDisk) {
  AttachFormatted(kSmallSectors);
  const DiskFile file = CreateFile("NEW");
  constexpr int kDiskWords = (kSmallClusters - 1) * kDiskSectorWords;
  for (int written = 0; written < kDiskWords; written += 128) {
    int writable = -1;
    ASSERT_EQ(file_system_.GetWritableWords(file, &writable), DiskStatus::kOk);
    EXPECT_EQ(writable, kDiskWords - written);
    ASSERT_EQ(WriteWords(
                  file, MakeCountingWords(static_cast<uint16_t>(written), 128)),
              DiskStatus::kOk);
  }
  int writable = -1;
  ASSERT_EQ(file_system_.GetWritableWords(file, &writable), DiskStatus::kOk);
  EXPECT_EQ(writable, 0);
  EXPECT_EQ(GetFreeClusters(), 0);
  CollectSectorWrites();

  const std::vector<uint16_t> word = {7};
  int count = -1;
  EXPECT_EQ(file_system_.Write(file, word, &count), DiskStatus::kDiskFull);
  EXPECT_EQ(count, 0);
  EXPECT_EQ(GetInfo(file),
            (DiskFileInfo{.position = kDiskWords, .size = kDiskWords}));
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());

  // Overwriting needs no cluster.
  ASSERT_EQ(file_system_.Seek(file, 0), DiskStatus::kOk);
  ASSERT_EQ(file_system_.GetWritableWords(file, &writable), DiskStatus::kOk);
  EXPECT_EQ(writable, kDiskWords);
  EXPECT_EQ(file_system_.Write(file, word, &count), DiskStatus::kOk);
  EXPECT_EQ(count, 1);
}

TEST_F(DiskFileSystemTest, WritableWordsCountTheLastClustersRoom) {
  // Clusters of four sectors, and a file ending 100 words into its second.
  AttachFormatted(kFloppySectors, 4);
  const int free_clusters = GetFreeClusters();
  const DiskFile file = CreateFile("NEW");
  constexpr int kClusterWords = 4 * kDiskSectorWords;
  ASSERT_EQ(WriteWords(file, MakeCountingWords(1, kClusterWords + 100)),
            DiskStatus::kOk);
  ASSERT_EQ(file_system_.Seek(file, 50), DiskStatus::kOk);
  int writable = -1;
  ASSERT_EQ(file_system_.GetWritableWords(file, &writable), DiskStatus::kOk);
  EXPECT_EQ(writable,
            2 * kClusterWords - 50 + (free_clusters - 2) * kClusterWords);
}

TEST_F(DiskFileSystemTest, WriteRefusals) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "LOCKED", {}, 0, 0, kDiskAttribReadOnly);
    editor.AddFile(1, "OPEN", {}, 0);
  });
  const DiskFile root = OpenRoot();
  const DiskFile locked = OpenFile("LOCKED");
  CollectSectorWrites();

  const auto expect_refused = [this](DiskFile file, DiskStatus status) {
    const std::vector<uint16_t> words = {1, 2, 3};
    int count = -1;
    EXPECT_EQ(file_system_.Write(file, words, &count), status) << file;
    EXPECT_EQ(count, 0);
    int writable = -1;
    EXPECT_EQ(file_system_.GetWritableWords(file, &writable), status) << file;
  };
  expect_refused(root, DiskStatus::kWrongHandleType);
  expect_refused(locked, DiskStatus::kReadOnly);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  EXPECT_EQ(GetInfo(locked).size, 0);

  ASSERT_EQ(SetWriteProtected(true), DiskStatus::kOk);
  expect_refused(OpenFile("OPEN"), DiskStatus::kWriteProtected);
}

TEST_F(DiskFileSystemTest, TwoFilesWrittenAtOnceShareTheWorkingFatSector) {
  // The free list starts at 300, whose FAT word is in sector 2. Cluster n is
  // sector n + 3.
  AttachFormatted(kFloppySectors);
  EditDisk([](DiskEditor& editor) { editor.SetFatWord(0, 300); });
  EXPECT_EQ(GetFreeClusters(), 417);
  const DiskFile a = CreateFile("A");
  const DiskFile b = CreateFile("B");
  CollectSectorWrites();

  // A takes 300 and 302, and B 301 and 303.
  const std::vector<uint16_t> a_data = MakeCountingWords(1000, 266);
  const std::vector<uint16_t> b_data = MakeCountingWords(2000, 266);
  ASSERT_EQ(WriteWords(a, absl::MakeConstSpan(a_data).first(256)),
            DiskStatus::kOk);
  ASSERT_EQ(WriteWords(b, absl::MakeConstSpan(b_data).first(256)),
            DiskStatus::kOk);
  ASSERT_EQ(WriteWords(a, absl::MakeConstSpan(a_data).subspan(256)),
            DiskStatus::kOk);
  ASSERT_EQ(WriteWords(b, absl::MakeConstSpan(b_data).subspan(256)),
            DiskStatus::kOk);

  // Each file's first sector is written as its buffer moves on, and the FAT
  // sectors stay in their buffers.
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(303, 304));
  EXPECT_EQ(GetFreeClusters(), 413);
  EXPECT_EQ(ReadFile(a), a_data);
  EXPECT_EQ(ReadFile(b), b_data);
}

}  // namespace
}  // namespace oz3
