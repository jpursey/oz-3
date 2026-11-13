// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_file_system.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <memory>
#include <optional>
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
#include "oz3/disk/disk_superblock.h"
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

// The directory entries a sector holds.
constexpr int kEntriesPerSector = kDiskSectorWords / DiskDirEntry::kWordCount;

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

// A time after GetLater(), for what is written after creating.
DateTime GetLatest() {
  return DateTime::FromCivil(absl::CivilSecond(2026, 11, 13, 8, 20, 5));
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

// Returns an image's superblock.
DiskSuperblock ReadSuperblock(DiskImage& image) {
  return DiskSuperblock::FromWords(
      ReadSector(image, DiskLayout::kSuperblockSector));
}

// Returns the sector holding word `position` of a file whose chain is
// `clusters`.
int GetDataSector(const DiskLayout& layout, absl::Span<const int> clusters,
                  int position) {
  const int cluster_words = layout.sectors_per_cluster * kDiskSectorWords;
  return layout.GetClusterSector(clusters[position / cluster_words]) +
         position % cluster_words / kDiskSectorWords;
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
  explicit DiskEditor(DiskImage* image)
      : image_(*image), layout_(ReadSuperblock(*image).GetLayout()) {}

  const DiskLayout& GetLayout() const { return layout_; }

  // Sets the FAT's word `index` to `value`.
  void SetFatWord(int index, uint16_t value) {
    const int sector = DiskLayout::GetFatSector(index);
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
                                  : DiskLayout::kFatEnd);
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
    for (int start = 0; start < size; start += kDiskSectorWords) {
      Sector words = MakeFilledSector(0xEEEE);
      for (int i = 0; i < kDiskSectorWords && start + i < size; ++i) {
        words[i] = static_cast<uint16_t>(first_word + start + i);
      }
      WriteSector(image_, GetDataSector(layout_, clusters, start), words);
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
  DiskImage& image_;
  DiskLayout layout_;
};

// A file a crash test writes, whose word at each position is `first_word` plus
// the position, whatever a crash leaves of it, and whose size is `size` after
// the test's operations, without a crash, or none for a file they delete.
struct CrashFile {
  std::string_view name;
  uint16_t first_word = 0;
  std::optional<int> size;
};

// A file ReadDiskContents finds in the root.
struct FoundFile {
  std::string name;
  int size = 0;
};

// What ReadDiskContents finds on a disk.
struct DiskContents {
  // The root's files, in the order of their entries.
  std::vector<FoundFile> files;

  // Each cluster's owner, by cluster: "the free list", "the root", a file's
  // name, or empty for a cluster on no chain.
  std::vector<std::string> owners;

  // Returns the file `name`, or null if the root has none.
  const FoundFile* FindFile(std::string_view name) const {
    const auto file = std::find_if(
        files.begin(), files.end(),
        [&](const FoundFile& found) { return found.name == name; });
    return file == files.end() ? nullptr : &*file;
  }
};

// Returns what `image` holds, by walking its FAT and root directory itself,
// and checks that it is consistent: the free list, the root, and every file in
// the root each have a chain no other shares a cluster of, every file's chain
// covers its size, and no two files share a name. Each of `files` holds its
// words up to its size, and every other file is empty.
DiskContents ReadDiskContents(DiskImage& image,
                              absl::Span<const CrashFile> files) {
  const DiskSuperblock superblock = ReadSuperblock(image);
  const DiskLayout layout = superblock.GetLayout();
  const int cluster_words = layout.sectors_per_cluster * kDiskSectorWords;
  std::vector<uint16_t> fat;
  for (int i = 0; i < layout.fat_sectors; ++i) {
    const Sector words = ReadSector(image, DiskLayout::kFirstFatSector + i);
    fat.insert(fat.end(), words.begin(), words.end());
  }

  // Returns the cluster after `cluster`, 0 at the end, or one past the last
  // cluster for a link out of range.
  const auto get_next = [&](int cluster) {
    return layout.GetNextCluster(cluster, fat[cluster])
        .value_or(layout.cluster_count + 1);
  };

  DiskContents contents;
  std::vector<std::string>& owners = contents.owners;
  owners.resize(layout.cluster_count + 1);
  const auto take_chain = [&](const std::string& owner, int chain_first) {
    std::vector<int> chain;
    for (int cluster = chain_first; cluster != 0; cluster = get_next(cluster)) {
      if (cluster > layout.cluster_count) {
        ADD_FAILURE() << owner << "'s chain has a link past the last cluster";
        break;
      }
      if (owners[cluster] == owner) {
        ADD_FAILURE() << owner << "'s chain loops at cluster " << cluster;
        break;
      }
      if (!owners[cluster].empty()) {
        ADD_FAILURE() << owner << " and " << owners[cluster]
                      << " share cluster " << cluster;
        break;
      }
      owners[cluster] = owner;
      chain.push_back(cluster);
    }
    return chain;
  };

  // Word 0 is the link of an imaginary cluster 0, to the free list's head.
  take_chain("the free list", get_next(0));
  std::vector<DiskDirEntry> entries;
  for (int cluster : take_chain("the root", superblock.GetRootCluster())) {
    for (int i = 0; i < layout.sectors_per_cluster; ++i) {
      for (int index = 0; index < kEntriesPerSector; ++index) {
        entries.push_back(
            ReadEntry(image, layout.GetClusterSector(cluster) + i, index));
      }
    }
  }

  for (const DiskDirEntry& entry : entries) {
    if (entry.IsFree()) {
      continue;
    }
    const std::optional<DiskFilename> name = entry.GetName();
    const std::optional<int> size = entry.GetSize();
    if (!name.has_value() || !size.has_value()) {
      ADD_FAILURE() << "The root has a broken entry";
      continue;
    }
    const std::string text = name->ToString();
    if (contents.FindFile(text) != nullptr) {
      ADD_FAILURE() << "The root has two files named " << text;
      continue;
    }
    contents.files.push_back({.name = text, .size = *size});
    const std::vector<int> chain = take_chain(text, entry.GetFirstCluster());
    const int chain_count = static_cast<int>(chain.size());
    if (chain_count * cluster_words < *size) {
      ADD_FAILURE() << text << "'s chain of " << chain_count
                    << " clusters doesn't cover its size of " << *size;
      continue;
    }
    const auto file = std::find_if(
        files.begin(), files.end(),
        [&](const CrashFile& crash_file) { return crash_file.name == text; });
    if (file == files.end()) {
      EXPECT_EQ(*size, 0) << text;
      continue;
    }
    std::vector<uint16_t> data;
    for (int position = 0; position < *size; position += kDiskSectorWords) {
      const Sector words =
          ReadSector(image, GetDataSector(layout, chain, position));
      const int count = std::min(kDiskSectorWords, *size - position);
      data.insert(data.end(), words.begin(), words.begin() + count);
    }
    EXPECT_EQ(data, MakeCountingWords(file->first_word, *size)) << text;
  }
  return contents;
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

  // Runs `run` on a disk of `sector_count` sectors set up by `setup`, once to
  // count its sector writes, and then once for each number of them a crash
  // could let reach the disk, each on a new disk. After each, the image is
  // reopened, as it is after a crash, and checked with ReadDiskContents for
  // `files`. Without a crash, each of `files` has its size. A crash may
  // orphan only a cluster `run` moves from one owner to another, or one on no
  // chain before or after it, and may lose only a file `run` creates,
  // deletes, or renames, leaving no fewer files than both before and after.
  // `run` passes each status to KeepStatus, and closes every file it opens, as
  // a crash leaves each operation failing.
  void RunCrashTest(int sector_count,
                    absl::FunctionRef<void(DiskEditor& editor)> setup,
                    absl::FunctionRef<void()> run,
                    absl::Span<const CrashFile> files) {
    // Returns ReadDiskContents for the attached image.
    const auto read_contents = [&] {
      std::unique_ptr<DiskImage> image = file_system_.DetachImage();
      DiskContents contents = ReadDiskContents(*image, files);
      EXPECT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
      return contents;
    };
    const auto start = [&] {
      file_system_.DetachImage().reset();
      AttachFormatted(sector_count);
      EditDisk(setup);
      CollectSectorWrites();
      crash_status_ = DiskStatus::kOk;
    };
    start();
    const DiskContents before = read_contents();
    run();
    ASSERT_EQ(crash_status_, DiskStatus::kOk);
    const int write_count = static_cast<int>(CollectSectorWrites().size());
    const DiskContents after = read_contents();
    for (const CrashFile& file : files) {
      const FoundFile* found = after.FindFile(file.name);
      if (!file.size.has_value()) {
        EXPECT_EQ(found, nullptr) << file.name;
        continue;
      }
      ASSERT_NE(found, nullptr) << file.name;
      EXPECT_EQ(found->size, *file.size) << file.name;
    }
    for (int writes = 0; writes <= write_count; ++writes) {
      SCOPED_TRACE(std::to_string(writes) + " of " +
                   std::to_string(write_count) + " writes");
      start();
      protocol_->SetLimits({.writes = writes});
      run();
      EXPECT_EQ(crash_status_,
                writes < write_count ? DiskStatus::kIoError : DiskStatus::kOk);
      ASSERT_EQ(ReopenImage(), DiskStatus::kOk);
      const DiskContents contents = read_contents();
      for (int cluster = 1; cluster < static_cast<int>(contents.owners.size());
           ++cluster) {
        const std::string& owner = before.owners[cluster];
        if (contents.owners[cluster].empty() && !owner.empty() &&
            owner == after.owners[cluster]) {
          ADD_FAILURE() << "Cluster " << cluster << " of " << owner
                        << " is orphaned";
        }
      }
      for (const FoundFile& file : before.files) {
        if (after.FindFile(file.name) != nullptr) {
          EXPECT_NE(contents.FindFile(file.name), nullptr)
              << file.name << " is lost";
        }
      }
      // A renamed file is lost if neither name has it.
      EXPECT_GE(contents.files.size(),
                std::min(before.files.size(), after.files.size()))
          << "A file is lost";
    }
  }

  // Keeps `status` as the crash test's, if it is the first that isn't kOk.
  void KeepStatus(DiskStatus status) {
    if (crash_status_ == DiskStatus::kOk) {
      crash_status_ = status;
    }
  }

  // Create, Open, and Close for a crash test, which keep their statuses. A
  // failed Create or Open returns -1, which Close refuses.
  DiskFile CreateForCrash(std::string_view name) {
    DiskFile file = -1;
    KeepStatus(file_system_.Create(GetName(name), GetLater(), &file));
    return file;
  }
  DiskFile OpenForCrash(std::string_view name) {
    DiskFile file = -1;
    KeepStatus(file_system_.Open(GetName(name), &file));
    return file;
  }
  void CloseForCrash(DiskFile file) {
    KeepStatus(file_system_.Close(file, GetLatest()));
  }

  // Writes `words` to `file` from its position for a crash test, stopping at
  // the first failure.
  void WriteForCrash(DiskFile file, absl::Span<const uint16_t> words) {
    while (!words.empty()) {
      int count = 0;
      const DiskStatus status = file_system_.Write(file, words, &count);
      KeepStatus(status);
      if (status != DiskStatus::kOk) {
        return;
      }
      words.remove_prefix(count);
    }
  }

  gb::FileSystem host_;
  FaultyFileProtocol* protocol_ = nullptr;
  DiskFileSystem file_system_;
  DiskStatus crash_status_ = DiskStatus::kOk;  // See KeepStatus.
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
  // Fill the root's sector, to see formatting clear it.
  std::unique_ptr<DiskImage> image = MakeImage(kFloppySectors);
  WriteSector(*image, 4, MakeFilledSector(0xEEEE));
  ASSERT_EQ(file_system_.AttachImage(std::move(image)),
            DiskStatus::kUnformatted);
  CollectSectorWrites();
  ASSERT_EQ(file_system_.Format(GetLabel("work"), 0, GetNow()),
            DiskStatus::kOk);

  // The blank superblock, the FAT (see DiskFatTest), the root's one sector,
  // and the superblock, and nothing else.
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(0, 1, 2, 3, 4, 0));
  image = file_system_.DetachImage();
  constexpr DiskLayout kLayout = {
      .sectors_per_cluster = 1,
      .cluster_count = 716,
      .fat_sectors = 3,
  };
  EXPECT_EQ(ReadSuperblock(*image),
            DiskSuperblock(kLayout, 1, GetLabel("WORK"), GetNow()));
  EXPECT_THAT(ReadSector(*image, 4), Each(0));
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
  EXPECT_EQ(ReadSuperblock(*image).GetLayout().fat_sectors, 1);
  for (int sector = 2; sector < 6; ++sector) {
    EXPECT_THAT(ReadSector(*image, sector), Each(0)) << "Sector " << sector;
  }
  EXPECT_THAT(ReadSector(*image, 6), Each(0xEEEE));
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

TEST_F(DiskFileSystemTest, AnInvalidSuperblockIsUnformatted) {
  // Each way a superblock is invalid is DiskSuperblockTest's.
  AttachFormatted(kFloppySectors);
  ASSERT_EQ(PatchWord(DiskLayout::kSuperblockSector, 2, 2),
            DiskStatus::kUnformatted);
  DiskVolumeInfo volume;
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kUnformatted);
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

  // Attaching again reads the disk again, rather than the cache. How counting
  // the free clusters reads is DiskFatTest's.
  ASSERT_EQ(timed.AttachImage(timed.DetachImage()), DiskStatus::kOk);
  EXPECT_EQ(timed.CollectBusyTime(), kLatency + kRead);
}

TEST_F(DiskFileSystemTest, ABrokenFreeListIsCorrupt) {
  // Each way a free list is broken is DiskFatTest's.
  AttachFormatted(kSmallSectors);
  ASSERT_EQ(PatchWord(1, 0, kSmallClusters + 1), DiskStatus::kOk);
  DiskVolumeInfo volume;
  EXPECT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kCorrupt);
}

TEST_F(DiskFileSystemTest, SetLabelRewritesOnlyTheLabel) {
  // The superblock it attached, reserved flags and boot area included, with
  // only the label changed (see DiskSuperblockTest).
  AttachFormatted(kFloppySectors);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  Sector words = ReadSector(*image, 0);
  words[7] = 0x1234;    // Reserved flags
  words[100] = 0x5678;  // The boot area
  WriteSector(*image, 0, words);
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);

  ASSERT_EQ(file_system_.SetLabel(GetLabel("archive")), DiskStatus::kOk);
  DiskVolumeInfo volume;
  ASSERT_EQ(file_system_.GetVolumeInfo(&volume), DiskStatus::kOk);
  EXPECT_EQ(volume.label, GetLabel("ARCHIVE"));
  image = file_system_.DetachImage();
  DiskSuperblock expected = DiskSuperblock::FromWords(words);
  expected.SetLabel(GetLabel("ARCHIVE"));
  EXPECT_EQ(ReadSuperblock(*image), expected);
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
  EXPECT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kOk);
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
  ASSERT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kOk);
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
  ASSERT_EQ(file_system_.Close(root, GetLatest()), DiskStatus::kOk);
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
  ASSERT_EQ(file_system_.Close(2, GetLatest()), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Close(1, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(OpenRoot(), 1);
  EXPECT_EQ(OpenFile(names[kMaxOpenFiles]), 2);
}

TEST_F(DiskFileSystemTest, BadHandles) {
  AttachFormatted(kSmallSectors);
  ASSERT_EQ(OpenRoot(), 0);
  ASSERT_EQ(file_system_.Close(0, GetLatest()), DiskStatus::kOk);

  for (DiskFile file : {-1, 0, kMaxOpenFiles}) {
    SCOPED_TRACE(file);
    ExpectOperationsReturn(file, DiskStatus::kBadHandle);
    EXPECT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kBadHandle);
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
  EXPECT_EQ(file_system_.Close(0, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Close(0, GetLatest()), DiskStatus::kBadHandle);
  ASSERT_EQ(file_system_.Close(3, GetLatest()), DiskStatus::kOk);
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
  ASSERT_EQ(file_system_.Close(0, GetLatest()), DiskStatus::kOk);
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
    editor.SetFatWord(2, DiskLayout::kFatEnd);
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

TEST_F(DiskFileSystemTest, ABrokenRootChainIsCorrupt) {
  // A loop. Each way a chain is broken is DiskFatTest's.
  AttachFormatted(kSmallSectors);
  ASSERT_EQ(PatchWord(1, 1, 1), DiskStatus::kOk);
  DiskFile file = -1;
  EXPECT_EQ(file_system_.OpenRoot(&file), DiskStatus::kCorrupt);
  EXPECT_EQ(file_system_.Open(GetName("HELLO.TXT"), &file),
            DiskStatus::kCorrupt);
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
  EXPECT_EQ(file_system_.Delete(GetName("NEW")), DiskStatus::kIoError);
  EXPECT_EQ(file_system_.Rename(GetName("NEW"), GetName("OTHER")),
            DiskStatus::kIoError);
  EXPECT_EQ(file_system_.SetAttributes(GetName("NEW"), kDiskAttribHidden),
            DiskStatus::kIoError);

  // Close still frees the slot.
  EXPECT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kBadHandle);

  // Reattaching the reopened image is the retry: what reached the disk before
  // the failure is there, and the slots still open are stale.
  ASSERT_EQ(ReopenImage(), DiskStatus::kOk);
  ExpectOperationsReturn(root, DiskStatus::kDiskChanged);
  EXPECT_EQ(file_system_.Close(root, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Close(created, GetLatest()), DiskStatus::kOk);
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

  ASSERT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kOk);
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
  ASSERT_EQ(file_system_.Close(CreateFile("LAST"), GetLatest()),
            DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(2));
  ASSERT_EQ(file_system_.Close(CreateFile("NEXT"), GetLatest()),
            DiskStatus::kOk);
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
  ASSERT_EQ(file_system_.Close(a, GetLatest()), DiskStatus::kOk);
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

  ASSERT_EQ(file_system_.Close(CreateFile("NEW"), GetLatest()),
            DiskStatus::kOk);

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

  ASSERT_EQ(file_system_.Close(CreateFile("NEW"), GetLatest()),
            DiskStatus::kOk);
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

TEST_F(DiskFileSystemTest, GrowingARootOfTwoClustersLinksFromItsLast) {
  // The root's chain is 1 then 300, and the free list skips 300. The new
  // cluster, 2, is written while it is still free, before allocating it
  // writes sector 1 with its end, and then the link from 300 in sector 2 (see
  // DiskFatTest).
  AttachFormatted(kFloppySectors);
  EditDisk([](DiskEditor& editor) {
    editor.SetChain({1, 300});
    editor.SetFatWord(299, 301);
    editor.FillSector(4, 0);
    editor.FillSector(303, 16);
  });
  EXPECT_EQ(GetFreeClusters(), 714);
  CollectSectorWrites();

  ASSERT_EQ(file_system_.Close(CreateFile("NEW"), GetLatest()),
            DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(5, 1, 2));
  EXPECT_EQ(GetInfo(OpenRoot()).size, 3 * kDiskSectorWords);

  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadSector(*image, 2)[300 - kDiskSectorWords], 2);
  EXPECT_EQ(ReadEntry(*image, 5, 0), MakeCreatedEntry("NEW"));
}

TEST_F(DiskFileSystemTest, AnOpenRootSeesCreatedEntries) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.AddFile(0, "FOO", {}, 0); });
  const DiskFile root = OpenRoot();
  DiskDirEntry entry;
  ASSERT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
            DiskStatus::kOk);

  // The root handle holds the sector the entry is written to.
  ASSERT_EQ(file_system_.Close(CreateFile("BAR"), GetLatest()),
            DiskStatus::kOk);
  ASSERT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
            DiskStatus::kOk);
  EXPECT_EQ(entry, MakeCreatedEntry("BAR"));

  // Filling the root's one sector, and growing it.
  for (int i = 2; i < 16; ++i) {
    ASSERT_EQ(file_system_.Close(CreateFile(GetFillerName(i)), GetLatest()),
              DiskStatus::kOk);
  }
  EXPECT_EQ(GetInfo(root).size, kDiskSectorWords);
  ASSERT_EQ(file_system_.Close(CreateFile("GROWN"), GetLatest()),
            DiskStatus::kOk);
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
    editor.SetFatWord(0, DiskLayout::kFatEnd);
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

TEST_F(DiskFileSystemTest, CloseCommitsTheDataTheFatAndTheEntry) {
  // Clusters 2 and 3 are sectors 3 and 4, and the root is sector 2.
  AttachFormatted(kSmallSectors);
  const DiskFile file = CreateFile("NEW");
  const std::vector<uint16_t> data = MakeCountingWords(1, 300);
  ASSERT_EQ(WriteWords(file, data), DiskStatus::kOk);
  CollectSectorWrites();
  EXPECT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(4, 1, 2));

  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 2, 0), MakeEntry("NEW", 2, 300, 0, GetLatest()));
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(ReadFile(OpenFile("NEW")), data);
}

TEST_F(DiskFileSystemTest, FlushCommitsAndKeepsTheFileOpen) {
  // The free list starts at 300, whose FAT word is in sector 2. Cluster n is
  // sector n + 3, so the root is sector 4.
  AttachFormatted(kFloppySectors);
  EditDisk([](DiskEditor& editor) { editor.SetFatWord(0, 300); });
  const DiskFile file = CreateFile("NEW");
  const std::vector<uint16_t> data = MakeCountingWords(1, 300);
  ASSERT_EQ(WriteWords(file, absl::MakeConstSpan(data).first(200)),
            DiskStatus::kOk);
  CollectSectorWrites();

  // Data, then sector 1 with the free list's head, then the working FAT
  // sector, then the entry.
  EXPECT_EQ(file_system_.Flush(file, GetLatest()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(303, 1, 2, 4));
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 200, .size = 200}));

  // With nothing written since, there is nothing to commit.
  EXPECT_EQ(file_system_.Flush(file, GetNow()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre());

  // Writing on, the data sector moves to cluster 301, and the next commit
  // stamps the file again. Closing after it has nothing more to commit.
  ASSERT_EQ(WriteWords(file, absl::MakeConstSpan(data).subspan(200)),
            DiskStatus::kOk);
  EXPECT_EQ(file_system_.Flush(file, GetNow()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(303, 304, 1, 2, 4));
  EXPECT_EQ(file_system_.Close(file, GetLater()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre());

  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 4, 0), MakeEntry("NEW", 300, 300));
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(ReadFile(OpenFile("NEW")), data);
}

TEST_F(DiskFileSystemTest, AnUnwrittenFileCommitsNothing) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.AddFile(1, "OLD", {2}, 10); });
  const DiskFile created = CreateFile("NEW");
  const DiskFile old = OpenFile("OLD");
  ASSERT_EQ(ReadFile(old), MakeCountingWords(0, 10));
  const DiskFile root = OpenRoot();
  CollectSectorWrites();

  EXPECT_EQ(file_system_.Flush(root, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Flush(old, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Close(root, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Close(old, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Close(created, GetLatest()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre());

  // The created file keeps its creation stamp.
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 2, 0), MakeCreatedEntry("NEW"));
  EXPECT_EQ(ReadEntry(*image, 2, 1), MakeEntry("OLD", 2, 10));
}

TEST_F(DiskFileSystemTest, FindGivesTheCommittedEntry) {
  AttachFormatted(kSmallSectors);
  const DiskFile file = CreateFile("NEW");
  ASSERT_EQ(WriteWords(file, MakeCountingWords(1, 10)), DiskStatus::kOk);
  const DiskFile root = OpenRoot();
  DiskDirEntry entry;
  ASSERT_EQ(file_system_.Find(root, GetName("NEW"), &entry), DiskStatus::kOk);
  EXPECT_EQ(entry, MakeCreatedEntry("NEW"));

  // The open root's buffer sees the committed entry.
  ASSERT_EQ(file_system_.Flush(file, GetLatest()), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Seek(root, 0), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Find(root, GetName("NEW"), &entry), DiskStatus::kOk);
  EXPECT_EQ(entry, MakeEntry("NEW", 2, 10, 0, GetLatest()));
}

TEST_F(DiskFileSystemTest, FlushingEveryFile) {
  // A takes cluster 2 (sector 3), and B cluster 3 (sector 4). Committing A
  // writes the FAT for both, and the open root has nothing to commit.
  AttachFormatted(kSmallSectors);
  const DiskFile a = CreateFile("A");
  const DiskFile b = CreateFile("B");
  OpenRoot();
  ASSERT_EQ(WriteWords(a, MakeCountingWords(1, 10)), DiskStatus::kOk);
  ASSERT_EQ(WriteWords(b, MakeCountingWords(2, 20)), DiskStatus::kOk);
  CollectSectorWrites();
  EXPECT_EQ(file_system_.Flush(GetLatest()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(3, 1, 2, 4, 2));

  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 2, 0), MakeEntry("A", 2, 10, 0, GetLatest()));
  EXPECT_EQ(ReadEntry(*image, 2, 1), MakeEntry("B", 3, 20, 0, GetLatest()));

  // With nothing to flush, Flush still needs a file system.
  EXPECT_EQ(file_system_.Flush(GetLatest()), DiskStatus::kNoDisk);
  ASSERT_EQ(file_system_.AttachImage(MakeImage(kSmallSectors, kOtherPath)),
            DiskStatus::kUnformatted);
  EXPECT_EQ(file_system_.Flush(GetLatest()), DiskStatus::kUnformatted);
}

TEST_F(DiskFileSystemTest, CloseFreesTheSlotWhenItsCommitFails) {
  AttachFormatted(kSmallSectors);
  const DiskFile file = CreateFile("NEW");
  ASSERT_EQ(WriteWords(file, MakeCountingWords(1, 10)), DiskStatus::kOk);
  protocol_->SetLimits({.writes = 0});
  EXPECT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kIoError);
  EXPECT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kBadHandle);

  // The entry on the disk is as it was created.
  ASSERT_EQ(ReopenImage(), DiskStatus::kOk);
  EXPECT_EQ(GetInfo(OpenFile("NEW")).size, 0);
}

TEST_F(DiskFileSystemTest, ClosingAStaleWrittenSlotReportsItsLostWrites) {
  AttachFormatted(kSmallSectors);
  const DiskFile file = CreateFile("NEW");
  ASSERT_EQ(WriteWords(file, MakeCountingWords(1, 10)), DiskStatus::kOk);
  ASSERT_EQ(file_system_.AttachImage(file_system_.DetachImage()),
            DiskStatus::kOk);
  CollectSectorWrites();
  EXPECT_EQ(file_system_.Flush(file, GetLatest()), DiskStatus::kDiskChanged);
  // The writes are lost, which Close reports, but the slot is freed.
  EXPECT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kDiskChanged);
  EXPECT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kBadHandle);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre());
  EXPECT_EQ(GetInfo(OpenFile("NEW")).size, 0);
}

TEST_F(DiskFileSystemTest, TruncatingToEmptyFreesTheWholeChain) {
  // NEW takes clusters 2 and 3 (sectors 3 and 4), leaving the free list's
  // head at 4. The root is sector 2.
  AttachFormatted(kSmallSectors);
  const DiskFile file = CreateFile("NEW");
  ASSERT_EQ(WriteWords(file, MakeCountingWords(1, 300)), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), kSmallClusters - 3);
  CollectSectorWrites();

  // The file is flushed with the new size, as the data, then the FAT, then
  // the entry, and then the chain is freed onto the head in sector 1.
  ASSERT_EQ(file_system_.Seek(file, 0), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Truncate(file, GetLatest()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(4, 1, 2, 1));
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 0, .size = 0}));
  EXPECT_EQ(GetFreeClusters(), kSmallClusters - 1);

  // The truncate stamped it, so Close has nothing to write.
  EXPECT_EQ(file_system_.Close(file, GetNow()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 2, 0), MakeEntry("NEW", 0, 0, 0, GetLatest()));
  const Sector fat = ReadSector(*image, 1);
  EXPECT_EQ(fat[0], 2);
  EXPECT_EQ(fat[3], 4);
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);

  // Writing again takes the freed clusters.
  const DiskFile reopened = OpenFile("NEW");
  const std::vector<uint16_t> data = MakeCountingWords(7, 300);
  ASSERT_EQ(WriteWords(reopened, data), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), kSmallClusters - 3);
  EXPECT_EQ(ReadFile(reopened), data);
}

TEST_F(DiskFileSystemTest, TruncatingMidClusterAndOnABoundary) {
  // OLD is clusters 300 to 302, whose FAT words are in sector 2, and the free
  // list starts at 400. Cluster n is sector n + 3, so the root is sector 4.
  AttachFormatted(kFloppySectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OLD", {300, 301, 302}, 600, 1000);
    editor.SetFatWord(0, 400);
  });
  const int free_clusters = GetFreeClusters();
  const DiskFile file = OpenFile("OLD");
  CollectSectorWrites();

  // Mid-cluster, only 302 is freed: the entry, then sector 2 with both 301's
  // new end and 302's link to 400, then sector 1 with the head.
  ASSERT_EQ(file_system_.Seek(file, 300), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Truncate(file, GetLatest()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(4, 2, 1));
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 300, .size = 300}));
  EXPECT_EQ(GetFreeClusters(), free_clusters + 1);
  EXPECT_EQ(ReadFile(file), MakeCountingWords(1000, 300));

  // On a boundary, the cluster past it is freed too.
  ASSERT_EQ(file_system_.Seek(file, kDiskSectorWords), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Truncate(file, GetLatest()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(4, 2, 1));
  EXPECT_EQ(GetFreeClusters(), free_clusters + 2);
  EXPECT_EQ(ReadFile(file), MakeCountingWords(1000, kDiskSectorWords));

  // The entry is stamped, and the free list runs 301, 302, 400.
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 4, 0),
            MakeEntry("OLD", 300, kDiskSectorWords, 0, GetLatest()));
  EXPECT_EQ(ReadSector(*image, 1)[0], 301);
  const Sector fat = ReadSector(*image, 2);
  EXPECT_EQ(fat[300 - kDiskSectorWords], 0xFFFF);
  EXPECT_EQ(fat[301 - kDiskSectorWords], 302);
  EXPECT_EQ(fat[302 - kDiskSectorWords], 400);
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);

  // Writing past the end takes the head again.
  const DiskFile reopened = OpenFile("OLD");
  ASSERT_EQ(file_system_.Seek(reopened, kDiskSectorWords), DiskStatus::kOk);
  const std::vector<uint16_t> data = MakeCountingWords(1, 100);
  ASSERT_EQ(WriteWords(reopened, data), DiskStatus::kOk);
  EXPECT_EQ(GetFreeClusters(), free_clusters + 1);
  std::vector<uint16_t> expected = MakeCountingWords(1000, kDiskSectorWords);
  expected.insert(expected.end(), data.begin(), data.end());
  EXPECT_EQ(ReadFile(reopened), expected);
}

TEST_F(DiskFileSystemTest, TruncatingAtTheEndOnlyFlushes) {
  // OLD is cluster 2 (sector 3), and the free list starts after it. The root
  // is sector 2.
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OLD", {2}, 10, 1000);
    editor.SetFatWord(0, 3);
  });
  const int free_clusters = GetFreeClusters();
  const DiskFile file = OpenFile("OLD");
  CollectSectorWrites();

  // With nothing written, it writes nothing.
  ASSERT_EQ(file_system_.Seek(file, 10), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Truncate(file, GetLatest()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());

  // After a write, it flushes it, stamped.
  ASSERT_EQ(WriteWords(file, MakeCountingWords(1010, 5)), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Truncate(file, GetLatest()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(3, 2));
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 15, .size = 15}));
  EXPECT_EQ(GetFreeClusters(), free_clusters);

  // Nothing is left for Close to write.
  EXPECT_EQ(file_system_.Close(file, GetNow()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 2, 0), MakeEntry("OLD", 2, 15, 0, GetLatest()));
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(ReadFile(OpenFile("OLD")), MakeCountingWords(1000, 15));
}

TEST_F(DiskFileSystemTest, TruncatingFreesAChainLeftPastTheSize) {
  // Cluster 4 is slack past the size, and the free list starts after it.
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OLD", {2, 3, 4}, 300, 1000);
    editor.SetFatWord(0, 5);
  });
  EXPECT_EQ(GetFreeClusters(), 6);
  const DiskFile file = OpenFile("OLD");
  CollectSectorWrites();

  // The size is as it was, but freeing changes the file, so it is stamped.
  ASSERT_EQ(file_system_.Seek(file, 300), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Truncate(file, GetLatest()), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(2, 1));
  EXPECT_EQ(GetFreeClusters(), 7);

  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 2, 0), MakeEntry("OLD", 2, 300, 0, GetLatest()));
  const Sector fat = ReadSector(*image, 1);
  EXPECT_EQ(fat[0], 4);
  EXPECT_EQ(fat[3], 0xFFFF);
  EXPECT_EQ(fat[4], 5);
}

TEST_F(DiskFileSystemTest, ClosingAStaleSlotAfterTruncatingLosesNothing) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OLD", {2}, 10, 1000);
    editor.SetFatWord(0, 3);
  });
  const DiskFile file = OpenFile("OLD");
  ASSERT_EQ(WriteWords(file, MakeCountingWords(1, 5)), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Seek(file, 0), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Truncate(file, GetLatest()), DiskStatus::kOk);
  ASSERT_EQ(file_system_.AttachImage(file_system_.DetachImage()),
            DiskStatus::kOk);
  EXPECT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(GetInfo(OpenFile("OLD")).size, 0);
}

TEST_F(DiskFileSystemTest, TruncatingABrokenChainChangesNothing) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OLD", {2, 3}, 300, 1000);
    editor.SetFatWord(3, kSmallClusters + 1);
    editor.SetFatWord(0, 4);
  });
  const int free_clusters = GetFreeClusters();
  const DiskFile file = OpenFile("OLD");
  CollectSectorWrites();
  EXPECT_EQ(file_system_.Truncate(file, GetLatest()), DiskStatus::kCorrupt);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  EXPECT_EQ(GetInfo(file), (DiskFileInfo{.position = 0, .size = 300}));
  EXPECT_EQ(GetFreeClusters(), free_clusters);
}

TEST_F(DiskFileSystemTest, TruncateRefusals) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "LOCKED", {2}, 10, 0, kDiskAttribReadOnly);
    editor.AddFile(1, "OPEN", {3}, 10);
    editor.SetFatWord(0, 4);
  });
  const DiskFile root = OpenRoot();
  const DiskFile locked = OpenFile("LOCKED");
  CollectSectorWrites();
  EXPECT_EQ(file_system_.Truncate(root, GetLatest()),
            DiskStatus::kWrongHandleType);
  EXPECT_EQ(file_system_.Truncate(locked, GetLatest()), DiskStatus::kReadOnly);
  EXPECT_EQ(file_system_.Truncate(kMaxOpenFiles, GetLatest()),
            DiskStatus::kBadHandle);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  EXPECT_EQ(GetInfo(locked).size, 10);

  ASSERT_EQ(SetWriteProtected(true), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Truncate(locked, GetLatest()),
            DiskStatus::kDiskChanged);
  EXPECT_EQ(file_system_.Truncate(OpenFile("OPEN"), GetLatest()),
            DiskStatus::kWriteProtected);
}

TEST_F(DiskFileSystemTest, DeleteFreesTheEntryThenTheChain) {
  // OLD is clusters 2 to 4 and TWO is 5, and the free list starts at 6. The
  // root is sector 2.
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OLD", {2, 3, 4}, 2 * kDiskSectorWords + 100, 1000);
    editor.AddFile(1, "TWO", {5}, 10);
    editor.AddFile(2, "EMPTY", {}, 0);
    editor.SetFatWord(0, 6);
  });
  CollectSectorWrites();

  // An empty file only frees its entry.
  EXPECT_EQ(file_system_.Delete(GetName("EMPTY")), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(2));

  // The free entry is written, then the FAT with the chain on the free list's
  // head. Nothing has counted the free clusters, and deleting doesn't.
  EXPECT_EQ(file_system_.Delete(GetName("OLD")), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(2, 1));
  EXPECT_EQ(GetFreeClusters(), kSmallClusters - 2);

  // Once counted, deleting keeps the count.
  EXPECT_EQ(file_system_.Delete(GetName("TWO")), DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(2, 1));
  EXPECT_EQ(GetFreeClusters(), kSmallClusters - 1);
  DiskFile file = -1;
  for (std::string_view name : {"OLD", "TWO", "EMPTY"}) {
    EXPECT_EQ(file_system_.Open(GetName(name), &file), DiskStatus::kNotFound)
        << name;
  }

  // The free list runs 5, 2, 3, 4, 6, and the entries are zeroed.
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  const Sector fat = ReadSector(*image, 1);
  EXPECT_EQ(fat[0], 5);
  EXPECT_EQ(fat[5], 2);
  EXPECT_EQ(fat[2], 3);
  EXPECT_EQ(fat[3], 4);
  EXPECT_EQ(fat[4], 6);
  for (int index = 0; index < 3; ++index) {
    EXPECT_EQ(ReadEntry(*image, 2, index), DiskDirEntry()) << index;
  }
}

TEST_F(DiskFileSystemTest, DeletingACorruptFileChangesNothing) {
  // BROKEN's chain runs past the last cluster, OUT starts past it, and EMPTY
  // has no size but a first cluster, the free list's head, which Open
  // refuses too.
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "BROKEN", {2, 3}, 300, 1000);
    editor.SetFatWord(3, kSmallClusters + 1);
    editor.SetRootEntry(1, MakeEntry("OUT", kSmallClusters + 1, 10));
    editor.SetRootEntry(2, MakeEntry("EMPTY", 4, 0));
    editor.SetFatWord(0, 4);
  });
  CollectSectorWrites();
  for (std::string_view name : {"BROKEN", "OUT", "EMPTY"}) {
    EXPECT_EQ(file_system_.Delete(GetName(name)), DiskStatus::kCorrupt) << name;
  }
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  EXPECT_EQ(GetFreeClusters(), kSmallClusters - 3);
}

TEST_F(DiskFileSystemTest, DeleteRefusals) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "LOCKED", {2}, 10, 0, kDiskAttribReadOnly);
    editor.AddFile(1, "OPEN", {3}, 10);
    editor.SetFatWord(0, 4);
  });
  const DiskFile file = OpenFile("OPEN");
  CollectSectorWrites();
  EXPECT_EQ(file_system_.Delete(GetPattern("*")), DiskStatus::kBadName);
  EXPECT_EQ(file_system_.Delete(GetName("MISSING")), DiskStatus::kNotFound);
  EXPECT_EQ(file_system_.Delete(GetName("OPEN")), DiskStatus::kInUse);
  EXPECT_EQ(file_system_.Delete(GetName("LOCKED")), DiskStatus::kReadOnly);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());

  // Once closed, the file can be deleted.
  ASSERT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Delete(GetName("OPEN")), DiskStatus::kOk);
}

TEST_F(DiskFileSystemTest, RenameChangesOnlyTheName) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OTHER", {}, 0);
    editor.AddFile(1, "OLD", {2}, 10, 1000, kDiskAttribHidden);
    editor.SetFatWord(0, 3);
  });
  CollectSectorWrites();
  EXPECT_EQ(file_system_.Rename(GetName("old"), GetName("new.txt")),
            DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(2));
  DiskFile file = -1;
  EXPECT_EQ(file_system_.Open(GetName("OLD"), &file), DiskStatus::kNotFound);
  EXPECT_EQ(ReadFile(OpenFile("NEW.TXT")), MakeCountingWords(1000, 10));

  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 2, 1),
            MakeEntry("NEW.TXT", 2, 10, kDiskAttribHidden));
}

TEST_F(DiskFileSystemTest, RenameTakesTheFirstOfTwoEntriesWithTheName) {
  // Only a corrupt disk has two, and Open finds the first.
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.SetRootEntry(0, MakeEntry("OLD", 0, 0));
    editor.SetRootEntry(1, MakeEntry("OLD", 0, 0, kDiskAttribHidden));
  });
  EXPECT_EQ(file_system_.Rename(GetName("OLD"), GetName("NEW")),
            DiskStatus::kOk);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 2, 0), MakeEntry("NEW", 0, 0));
  EXPECT_EQ(ReadEntry(*image, 2, 1), MakeEntry("OLD", 0, 0, kDiskAttribHidden));
}

TEST_F(DiskFileSystemTest, RenameRefusals) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "A", {}, 0);
    editor.AddFile(1, "B", {}, 0);
    editor.AddFile(2, "OPEN", {}, 0);
  });
  const DiskFile file = OpenFile("OPEN");
  CollectSectorWrites();
  EXPECT_EQ(file_system_.Rename(GetPattern("A*"), GetName("C")),
            DiskStatus::kBadName);
  EXPECT_EQ(file_system_.Rename(GetName("A"), GetPattern("C?")),
            DiskStatus::kBadName);
  EXPECT_EQ(file_system_.Rename(GetName("MISSING"), GetName("C")),
            DiskStatus::kNotFound);
  EXPECT_EQ(file_system_.Rename(GetName("OPEN"), GetName("C")),
            DiskStatus::kInUse);
  EXPECT_EQ(file_system_.Rename(GetName("OPEN"), GetName("A")),
            DiskStatus::kInUse);

  // A name is taken by any file, before or after the one renamed, open or
  // not, or the file itself.
  EXPECT_EQ(file_system_.Rename(GetName("A"), GetName("B")),
            DiskStatus::kExists);
  EXPECT_EQ(file_system_.Rename(GetName("B"), GetName("A")),
            DiskStatus::kExists);
  EXPECT_EQ(file_system_.Rename(GetName("B"), GetName("OPEN")),
            DiskStatus::kExists);
  EXPECT_EQ(file_system_.Rename(GetName("A"), GetName("a")),
            DiskStatus::kExists);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());

  // Once closed, the file can be renamed.
  ASSERT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Rename(GetName("OPEN"), GetName("C")),
            DiskStatus::kOk);
}

TEST_F(DiskFileSystemTest, SetAttributesKeepsTheOtherBits) {
  // OLD has a reserved bit set, which setting its attributes keeps.
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "OLD", {2}, 10, 1000, 0x8000 | kDiskAttribHidden);
    editor.SetFatWord(0, 3);
  });
  CollectSectorWrites();
  EXPECT_EQ(file_system_.SetAttributes(GetName("OLD"), kDiskAttribReadOnly),
            DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), ElementsAre(2));

  // Setting them again writes nothing.
  EXPECT_EQ(file_system_.SetAttributes(GetName("OLD"), kDiskAttribReadOnly),
            DiskStatus::kOk);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());

  // Read-only, the file can be read, but not written or deleted.
  const DiskFile file = OpenFile("OLD");
  EXPECT_EQ(ReadFile(file), MakeCountingWords(1000, 10));
  int count = -1;
  EXPECT_EQ(file_system_.Write(file, MakeCountingWords(0, 1), &count),
            DiskStatus::kReadOnly);
  ASSERT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Delete(GetName("OLD")), DiskStatus::kReadOnly);

  EXPECT_EQ(file_system_.SetAttributes(GetName("OLD"), kDiskAttribSettable),
            DiskStatus::kOk);
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  EXPECT_EQ(ReadEntry(*image, 2, 0),
            MakeEntry("OLD", 2, 10, 0x8000 | kDiskAttribSettable));
  ASSERT_EQ(file_system_.AttachImage(std::move(image)), DiskStatus::kOk);

  EXPECT_EQ(file_system_.SetAttributes(GetName("OLD"), 0), DiskStatus::kOk);
  EXPECT_EQ(file_system_.Delete(GetName("OLD")), DiskStatus::kOk);
}

TEST_F(DiskFileSystemTest, SetAttributesRefusals) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.AddFile(0, "OPEN", {}, 0); });
  const DiskFile file = OpenFile("OPEN");
  CollectSectorWrites();
  EXPECT_EQ(file_system_.SetAttributes(GetPattern("*"), kDiskAttribHidden),
            DiskStatus::kBadName);
  for (uint16_t attributes :
       {kDiskAttribDirectory, uint16_t{1 << 3}, uint16_t{0x8000}}) {
    EXPECT_EQ(file_system_.SetAttributes(GetName("OPEN"), attributes),
              DiskStatus::kBadAttributes)
        << attributes;
  }
  EXPECT_EQ(file_system_.SetAttributes(GetName("MISSING"), kDiskAttribHidden),
            DiskStatus::kNotFound);
  EXPECT_EQ(file_system_.SetAttributes(GetName("OPEN"), kDiskAttribHidden),
            DiskStatus::kInUse);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());

  // Once closed, the file's attributes can be set.
  ASSERT_EQ(file_system_.Close(file, GetLatest()), DiskStatus::kOk);
  EXPECT_EQ(file_system_.SetAttributes(GetName("OPEN"), kDiskAttribHidden),
            DiskStatus::kOk);
}

TEST_F(DiskFileSystemTest, DirectoryChangesNeedAWritableFileSystem) {
  const auto expect_refused = [this](DiskStatus status) {
    EXPECT_EQ(file_system_.Delete(GetName("OLD")), status);
    EXPECT_EQ(file_system_.Rename(GetName("OLD"), GetName("NEW")), status);
    EXPECT_EQ(file_system_.SetAttributes(GetName("OLD"), kDiskAttribHidden),
              status);
  };
  expect_refused(DiskStatus::kNoDisk);
  ASSERT_EQ(file_system_.AttachImage(MakeImage(kSmallSectors, kOtherPath)),
            DiskStatus::kUnformatted);
  expect_refused(DiskStatus::kUnformatted);

  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) { editor.AddFile(0, "OLD", {2}, 10); });
  ASSERT_EQ(SetWriteProtected(true), DiskStatus::kOk);
  CollectSectorWrites();
  expect_refused(DiskStatus::kWriteProtected);
  EXPECT_THAT(CollectSectorWrites(), IsEmpty());
  EXPECT_EQ(GetInfo(OpenFile("OLD")).size, 10);
}

TEST_F(DiskFileSystemTest, WriteProtectionComesBeforeFormatting) {
  std::unique_ptr<DiskImage> image = MakeImage(kSmallSectors);
  ASSERT_EQ(image->SetWriteProtected(true), DiskStatus::kOk);
  ASSERT_EQ(file_system_.AttachImage(std::move(image)),
            DiskStatus::kUnformatted);
  DiskFile file = -1;
  EXPECT_EQ(file_system_.Format(GetLabel("WORK"), 0, GetNow()),
            DiskStatus::kWriteProtected);
  EXPECT_EQ(file_system_.SetLabel(GetLabel("WORK")),
            DiskStatus::kWriteProtected);
  EXPECT_EQ(file_system_.Create(GetName("A"), GetLater(), &file),
            DiskStatus::kWriteProtected);
  EXPECT_EQ(file_system_.Delete(GetName("A")), DiskStatus::kWriteProtected);
  EXPECT_EQ(file_system_.Rename(GetName("A"), GetName("B")),
            DiskStatus::kWriteProtected);
  EXPECT_EQ(file_system_.SetAttributes(GetName("A"), kDiskAttribHidden),
            DiskStatus::kWriteProtected);
}

TEST_F(DiskFileSystemTest, AnOpenRootKeepsItsPlaceThroughDirectoryChanges) {
  AttachFormatted(kSmallSectors);
  EditDisk([](DiskEditor& editor) {
    editor.AddFile(0, "A", {}, 0);
    editor.AddFile(1, "B", {}, 0);
    editor.AddFile(2, "C", {}, 0);
  });
  const DiskFile root = OpenRoot();
  DiskDirEntry entry;
  ASSERT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
            DiskStatus::kOk);
  EXPECT_EQ(entry, MakeEntry("A", 0, 0));

  // Entries stay in their places, so the root goes on from A's, seeing each
  // change.
  ASSERT_EQ(file_system_.Delete(GetName("A")), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Rename(GetName("B"), GetName("D")), DiskStatus::kOk);
  ASSERT_EQ(file_system_.SetAttributes(GetName("C"), kDiskAttribHidden),
            DiskStatus::kOk);
  for (const DiskDirEntry& expected :
       {MakeEntry("D", 0, 0), MakeEntry("C", 0, 0, kDiskAttribHidden)}) {
    ASSERT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
              DiskStatus::kOk);
    EXPECT_EQ(entry, expected) << entry.GetName()->ToString();
  }
  EXPECT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
            DiskStatus::kNotFound);

  // Starting again, A's entry is free.
  ASSERT_EQ(file_system_.Seek(root, 0), DiskStatus::kOk);
  ASSERT_EQ(file_system_.Find(root, GetPattern("*.*"), &entry),
            DiskStatus::kOk);
  EXPECT_EQ(entry, MakeEntry("D", 0, 0));
  EXPECT_EQ(GetInfo(root).position, 2 * DiskDirEntry::kWordCount);
}

TEST_F(DiskFileSystemTest, CrashWhileCreating) {
  RunCrashTest(
      kSmallSectors, [](DiskEditor&) {},
      [&] {
        const DiskFile file = CreateForCrash("NEW");
        CloseForCrash(file);
      },
      {{"NEW", 0, 0}});
}

TEST_F(DiskFileSystemTest, CrashWhileCreatingGrowsTheRoot) {
  // The free cluster the root takes, 2, holds old data, which the root must
  // never link to.
  RunCrashTest(
      kSmallSectors,
      [](DiskEditor& editor) {
        editor.FillSector(2, 0);
        editor.WriteData({2}, kDiskSectorWords, 0xE000);
      },
      [&] {
        const DiskFile file = CreateForCrash("NEW");
        CloseForCrash(file);
      },
      {{"NEW", 0, 0}});
}

TEST_F(DiskFileSystemTest, CrashWhileWritingAcrossClusters) {
  RunCrashTest(
      kSmallSectors, [](DiskEditor&) {},
      [&] {
        const DiskFile file = CreateForCrash("NEW");
        WriteForCrash(file, MakeCountingWords(1, 600));
        CloseForCrash(file);
      },
      {{"NEW", 1, 600}});
}

TEST_F(DiskFileSystemTest, CrashWhileWritingOverSlack) {
  // OLD's chain runs a cluster past its size.
  RunCrashTest(
      kSmallSectors,
      [](DiskEditor& editor) {
        editor.AddFile(0, "OLD", {2, 3, 4}, 300, 1000);
        editor.SetFatWord(0, 5);
      },
      [&] {
        const DiskFile file = OpenForCrash("OLD");
        KeepStatus(file_system_.Seek(file, 300));
        WriteForCrash(file, MakeCountingWords(1300, 500));
        CloseForCrash(file);
      },
      {{"OLD", 1000, 800}});
}

TEST_F(DiskFileSystemTest, CrashWhileWritingTwoFiles) {
  // Their clusters alternate.
  RunCrashTest(
      kSmallSectors, [](DiskEditor&) {},
      [&] {
        const DiskFile a = CreateForCrash("A");
        const DiskFile b = CreateForCrash("B");
        for (int position = 0; position < 600; position += 200) {
          WriteForCrash(a, MakeCountingWords(1 + position, 200));
          WriteForCrash(b, MakeCountingWords(5000 + position, 200));
        }
        CloseForCrash(a);
        CloseForCrash(b);
      },
      {{"A", 1, 600}, {"B", 5000, 600}});
}

TEST_F(DiskFileSystemTest, CrashWhileGrowingAcrossFatSectors) {
  // OLD's entry is on the disk, so its chain is reachable as it grows. The
  // free list jumps between FAT sectors, so most clusters taken change sector
  // 1 and another, linked from a third, but 260 and 270 share sector 2, which
  // is left dirty with sector 1 until taking 600 moves the working buffer.
  // 610 and 280 stay free.
  RunCrashTest(
      kFloppySectors,
      [](DiskEditor& editor) {
        editor.AddFile(0, "OLD", {250}, kDiskSectorWords, 1000);
        editor.SetChain({300, 520, 260, 270, 600, 610, 280});
        editor.SetFatWord(0, 300);
      },
      [&] {
        const DiskFile file = OpenForCrash("OLD");
        KeepStatus(file_system_.Seek(file, kDiskSectorWords));
        WriteForCrash(file, MakeCountingWords(1000 + kDiskSectorWords,
                                              5 * kDiskSectorWords));
        CloseForCrash(file);
      },
      {{"OLD", 1000, 6 * kDiskSectorWords}});
}

TEST_F(DiskFileSystemTest, CrashWhileTruncating) {
  // OLD's FAT words are in all three FAT sectors, so freeing changes each, and
  // the free list starts at 600. The first truncate frees 301 and 520, after
  // a write leaves its data sector dirty, and the second frees the rest.
  RunCrashTest(
      kFloppySectors,
      [](DiskEditor& editor) {
        editor.AddFile(0, "OLD", {250, 300, 301, 520},
                       3 * kDiskSectorWords + 100, 1000);
        editor.SetFatWord(0, 600);
      },
      [&] {
        const DiskFile file = OpenForCrash("OLD");
        WriteForCrash(file, MakeCountingWords(1000, 100));
        KeepStatus(file_system_.Seek(file, 300));
        KeepStatus(file_system_.Truncate(file, GetLatest()));
        KeepStatus(file_system_.Seek(file, 0));
        KeepStatus(file_system_.Truncate(file, GetLatest()));
        CloseForCrash(file);
      },
      {{"OLD", 1000, 0}});
}

TEST_F(DiskFileSystemTest, CrashWhileDeleting) {
  // OLD's FAT words are in all three FAT sectors, so freeing changes each, and
  // the free list starts at 600. NEW takes 600 and 601 first, leaving its
  // links dirty for deleting to write before OLD's free entry.
  RunCrashTest(
      kFloppySectors,
      [](DiskEditor& editor) {
        editor.AddFile(0, "OLD", {250, 300, 301, 520},
                       3 * kDiskSectorWords + 100, 1000);
        editor.SetFatWord(0, 600);
      },
      [&] {
        const DiskFile file = CreateForCrash("NEW");
        WriteForCrash(file, MakeCountingWords(1, 300));
        KeepStatus(file_system_.Delete(GetName("OLD")));
        CloseForCrash(file);
      },
      {{"OLD", 1000, std::nullopt}, {"NEW", 1, 300}});
}

TEST_F(DiskFileSystemTest, CrashWhileRenamingAndSettingAttributes) {
  RunCrashTest(
      kSmallSectors,
      [](DiskEditor& editor) {
        editor.AddFile(0, "OLD", {2}, 10, 1000);
        editor.SetFatWord(0, 3);
      },
      [&] {
        KeepStatus(file_system_.Rename(GetName("OLD"), GetName("NEW")));
        KeepStatus(
            file_system_.SetAttributes(GetName("NEW"), kDiskAttribReadOnly));
      },
      {{"OLD", 1000, std::nullopt}, {"NEW", 1000, 10}});
}

}  // namespace
}  // namespace oz3
