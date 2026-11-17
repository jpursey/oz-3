// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/disk_file_protocol.h"

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/time/civil_time.h"
#include "gb/file/file.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
#include "gb/file/memory_file_protocol.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/date_time.h"
#include "oz3/disk/disk_check.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/faulty_file_protocol.h"
#include "oz3/tools/program.h"
#include "oz3/tools/program_file.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;
using ::testing::UnorderedElementsAreArray;

constexpr char kImagePath[] = "host:/disk.ozd";
constexpr int kSectorCount = 720;
constexpr int kMaxOpenFiles = 4;
const absl::CivilSecond kNow(2026, 11, 17, 10, 30, 15);

class DiskFileProtocolTest : public ::testing::Test {
 protected:
  DiskFileProtocolTest() {
    auto host = std::make_unique<FaultyFileProtocol>();
    host_ = host.get();
    file_system_.Register(std::move(host), "host");
    auto protocol = std::make_unique<DiskFileProtocol>([this] { return now_; },
                                                       kMaxOpenFiles);
    protocol_ = protocol.get();
    EXPECT_TRUE(file_system_.Register(std::move(protocol)));
  }

  // Returns a new blank image of kSectorCount sectors.
  std::unique_ptr<DiskImage> MakeImage() {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&file_system_, kImagePath, kSectorCount);
    EXPECT_TRUE(image.ok()) << image.status();
    return image.ok() ? *std::move(image) : nullptr;
  }

  // Mounts a new image, and formats it.
  void MountFormatted() {
    EXPECT_EQ(protocol_->Mount(MakeImage()), DiskStatus::kUnformatted);
    EXPECT_EQ(protocol_->Format(*DiskLabel::Parse("WORK"), 0), DiskStatus::kOk);
  }

  // Unmounts the image, and returns what CheckDisk reports for it.
  DiskCheckReport UnmountAndCheck() {
    std::unique_ptr<DiskImage> image;
    EXPECT_EQ(protocol_->Unmount(&image), DiskStatus::kOk);
    DiskCheckReport report;
    EXPECT_EQ(CheckDisk(*image, &report), DiskStatus::kOk);
    return report;
  }

  // Returns the entry of the file `name` on the mounted disk, as it is on the
  // disk.
  std::optional<DiskDirEntry> GetEntry(std::string_view name) {
    DiskFileSystem* disk = protocol_->GetFileSystem();
    if (disk == nullptr ||
        disk->Seek(protocol_->GetRoot(), 0) != DiskStatus::kOk) {
      return std::nullopt;
    }
    DiskDirEntry entry;
    if (disk->Find(protocol_->GetRoot(), *DiskFilename::Parse(name), &entry) !=
        DiskStatus::kOk) {
      return std::nullopt;
    }
    return entry;
  }

  // Returns the words of the file `name` on the mounted disk.
  std::vector<uint16_t> ReadWords(std::string_view name) {
    DiskFileSystem* disk = protocol_->GetFileSystem();
    DiskFile file = -1;
    std::vector<uint16_t> words;
    EXPECT_EQ(disk->Open(*DiskFilename::Parse(name), &file), DiskStatus::kOk);
    uint16_t buffer[64];
    int count = 0;
    while (disk->Read(file, absl::MakeSpan(buffer), &count) ==
               DiskStatus::kOk &&
           count > 0) {
      words.insert(words.end(), buffer, buffer + count);
    }
    EXPECT_EQ(disk->Close(file, DateTime()), DiskStatus::kOk);
    return words;
  }

  // Writes `bytes` as the file at `path`, returning whether it succeeded.
  bool WriteBytes(std::string_view path, const std::vector<uint8_t>& bytes) {
    return file_system_.WriteFile(path, bytes);
  }

  // Returns the bytes of the file at `path`.
  std::vector<uint8_t> ReadBytes(std::string_view path) {
    std::vector<uint8_t> bytes;
    EXPECT_TRUE(file_system_.ReadFile(path, &bytes));
    return bytes;
  }

  absl::CivilSecond now_ = kNow;
  gb::FileSystem file_system_;
  FaultyFileProtocol* host_ = nullptr;
  DiskFileProtocol* protocol_ = nullptr;
};

//==============================================================================
// The protocol
//==============================================================================

TEST_F(DiskFileProtocolTest, Properties) {
  EXPECT_TRUE(file_system_.IsRegistered("ozdisk"));
  EXPECT_EQ(
      file_system_.GetFlags("ozdisk"),
      gb::FileProtocolFlags(
          {gb::FileProtocolFlag::kInfo, gb::FileProtocolFlag::kList,
           gb::FileProtocolFlag::kFileCreate, gb::FileProtocolFlag::kFileRead,
           gb::FileProtocolFlag::kFileWrite, gb::FileProtocolFlag::kMove}));
  EXPECT_EQ(file_system_.GetAttributeFlags("ozdisk"), gb::kAllFileAttributes);
}

TEST_F(DiskFileProtocolTest, NothingMountedHasNoPaths) {
  EXPECT_FALSE(protocol_->IsMounted());
  EXPECT_EQ(protocol_->GetFileSystem(), nullptr);
  EXPECT_EQ(protocol_->GetRoot(), -1);
  EXPECT_FALSE(file_system_.IsValidPath("ozdisk:/"));
  EXPECT_FALSE(file_system_.WriteFile("ozdisk:/HELLO.TXT", "hi"));
  EXPECT_EQ(protocol_->Unmount(), DiskStatus::kNoDisk);
  EXPECT_EQ(protocol_->Format(DiskLabel(), 0), DiskStatus::kNoDisk);
}

TEST_F(DiskFileProtocolTest, UnformattedHasAnEmptyRoot) {
  EXPECT_EQ(protocol_->Mount(MakeImage()), DiskStatus::kUnformatted);
  EXPECT_TRUE(protocol_->IsMounted());
  EXPECT_NE(protocol_->GetFileSystem(), nullptr);
  EXPECT_EQ(protocol_->GetRoot(), -1);
  EXPECT_TRUE(file_system_.IsValidFolder("ozdisk:/"));
  EXPECT_THAT(file_system_.List("ozdisk:/"), IsEmpty());
  EXPECT_FALSE(file_system_.WriteFile("ozdisk:/HELLO.TXT", "hi"));
  EXPECT_EQ(protocol_->Unmount(), DiskStatus::kOk);
}

TEST_F(DiskFileProtocolTest, FormatOpensTheRoot) {
  MountFormatted();
  EXPECT_GE(protocol_->GetRoot(), 0);
  DiskVolumeInfo info;
  EXPECT_EQ(protocol_->GetFileSystem()->GetVolumeInfo(&info), DiskStatus::kOk);
  EXPECT_EQ(info.label, *DiskLabel::Parse("WORK"));

  // The root is the protocol's, so the file system's own OpenRoot and Format
  // fail, but the protocol's Format formats again.
  DiskFile root = -1;
  EXPECT_EQ(protocol_->GetFileSystem()->OpenRoot(&root), DiskStatus::kInUse);
  EXPECT_EQ(protocol_->GetFileSystem()->Format(DiskLabel(), 0, DateTime()),
            DiskStatus::kInUse);
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/HELLO.TXT", "hi"));
  EXPECT_EQ(protocol_->Format(*DiskLabel::Parse("NEW"), 0), DiskStatus::kOk);
  EXPECT_GE(protocol_->GetRoot(), 0);
  EXPECT_THAT(file_system_.List("ozdisk:/"), IsEmpty());
}

TEST_F(DiskFileProtocolTest, FormatFailsWithAFileOpen) {
  MountFormatted();
  std::unique_ptr<gb::File> file =
      file_system_.OpenFile("ozdisk:/HELLO.TXT", gb::kNewFileFlags);
  ASSERT_NE(file, nullptr);
  EXPECT_EQ(protocol_->Format(DiskLabel(), 0), DiskStatus::kInUse);
  EXPECT_GE(protocol_->GetRoot(), 0);
  file.reset();
  EXPECT_TRUE(file_system_.IsValidFile("ozdisk:/HELLO.TXT"));
}

TEST_F(DiskFileProtocolTest, MountedImageOpensItsRoot) {
  MountFormatted();
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/HELLO.TXT", "hi"));
  std::unique_ptr<DiskImage> image;
  EXPECT_EQ(protocol_->Unmount(&image), DiskStatus::kOk);
  ASSERT_NE(image, nullptr);
  EXPECT_FALSE(protocol_->IsMounted());

  EXPECT_EQ(protocol_->Mount(std::move(image)), DiskStatus::kOk);
  EXPECT_GE(protocol_->GetRoot(), 0);
  EXPECT_TRUE(file_system_.IsValidFile("ozdisk:/HELLO.TXT"));
}

TEST_F(DiskFileProtocolTest, MountUnmountsFirst) {
  MountFormatted();
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/HELLO.TXT", "hi"));
  absl::StatusOr<std::unique_ptr<DiskImage>> other =
      DiskImage::Create(&file_system_, "host:/other.ozd", kSectorCount);
  ASSERT_TRUE(other.ok());
  EXPECT_EQ(protocol_->Mount(*std::move(other)), DiskStatus::kUnformatted);
  EXPECT_FALSE(file_system_.IsValidFile("ozdisk:/HELLO.TXT"));
}

TEST_F(DiskFileProtocolTest, FailedImageMountsAsAnIoError) {
  std::unique_ptr<DiskImage> image = MakeImage();
  host_->SetLimits({.reads = 0});
  EXPECT_EQ(protocol_->Mount(std::move(image)), DiskStatus::kIoError);
  EXPECT_TRUE(protocol_->IsMounted());
  EXPECT_EQ(protocol_->GetRoot(), -1);
  EXPECT_THAT(file_system_.List("ozdisk:/"), IsEmpty());
}

TEST_F(DiskFileProtocolTest, UnmountCommitsOpenFiles) {
  MountFormatted();
  std::unique_ptr<gb::File> file =
      file_system_.OpenFile("ozdisk:/HELLO.TXT", gb::kNewFileFlags);
  ASSERT_NE(file, nullptr);
  ASSERT_EQ(file->WriteString("hello!"), 6);
  ASSERT_TRUE(file->Flush());  // gb::File buffers nothing, but to be sure.

  // A second write isn't committed until unmount.
  ASSERT_EQ(file->WriteString("more"), 4);
  std::unique_ptr<DiskImage> image;
  EXPECT_EQ(protocol_->Unmount(&image), DiskStatus::kOk);

  // The file is stale now: it neither reads nor writes, and closes.
  EXPECT_EQ(file->WriteString("lost"), 0);
  file.reset();

  EXPECT_EQ(protocol_->Mount(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(ReadWords("HELLO.TXT"),
            std::vector<uint16_t>({0x6568, 0x6C6C, 0x216F, 0x6F6D, 0x6572}));
  EXPECT_EQ(UnmountAndCheck(), DiskCheckReport());
}

TEST_F(DiskFileProtocolTest, StaleFileNeverReachesTheNextImage) {
  MountFormatted();
  std::unique_ptr<gb::File> file =
      file_system_.OpenFile("ozdisk:/HELLO.TXT", gb::kNewFileFlags);
  ASSERT_NE(file, nullptr);
  std::unique_ptr<DiskImage> image;
  EXPECT_EQ(protocol_->Unmount(&image), DiskStatus::kOk);
  EXPECT_EQ(protocol_->Mount(std::move(image)), DiskStatus::kOk);
  EXPECT_EQ(file->WriteString("lost"), 0);
  file.reset();
  EXPECT_THAT(ReadWords("HELLO.TXT"), IsEmpty());
}

//==============================================================================
// Paths
//==============================================================================

TEST_F(DiskFileProtocolTest, PathsIgnoreCase) {
  MountFormatted();
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/hello.txt", "hi"));
  EXPECT_TRUE(file_system_.IsValidFile("ozdisk:/HELLO.TXT"));
  EXPECT_TRUE(file_system_.IsValidFile("ozdisk:/Hello.Txt"));
  EXPECT_THAT(file_system_.List("ozdisk:/"), ElementsAre("ozdisk:/HELLO.TXT"));
}

TEST_F(DiskFileProtocolTest, InvalidPaths) {
  MountFormatted();
  EXPECT_FALSE(file_system_.WriteFile("ozdisk:/a.b.c", "hi"));
  EXPECT_FALSE(file_system_.WriteFile("ozdisk:/thirteenchars", "hi"));
  EXPECT_FALSE(file_system_.WriteFile("ozdisk:/a*.txt", "hi"));
  EXPECT_FALSE(file_system_.WriteFile("ozdisk:/sub/a.txt", "hi"));
  EXPECT_FALSE(file_system_.IsValidPath("ozdisk:/sub/a.txt"));
  EXPECT_FALSE(file_system_.CreateFolder("ozdisk:/sub"));
  EXPECT_THAT(file_system_.List("ozdisk:/"), IsEmpty());
}

TEST_F(DiskFileProtocolTest, ListMatchesPatternsIgnoringCase) {
  MountFormatted();
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/A.TXT", "a"));
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/B.TXT", "b"));
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/C.OZ3", "c"));
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/README", "d"));
  EXPECT_THAT(file_system_.List("ozdisk:/"),
              ElementsAre("ozdisk:/A.TXT", "ozdisk:/B.TXT", "ozdisk:/C.OZ3",
                          "ozdisk:/README"));
  EXPECT_THAT(file_system_.List("ozdisk:/", "*.txt"),
              ElementsAre("ozdisk:/A.TXT", "ozdisk:/B.TXT"));
  EXPECT_THAT(file_system_.List("ozdisk:/", "c*", gb::FolderMode::kRecursive),
              ElementsAre("ozdisk:/C.OZ3"));
  EXPECT_THAT(file_system_.ListFolders("ozdisk:/"), IsEmpty());
  EXPECT_THAT(file_system_.ListFiles("ozdisk:/", "README"),
              ElementsAre("ozdisk:/README"));
}

TEST_F(DiskFileProtocolTest, InfoAndListNeedNoFreeSlot) {
  MountFormatted();
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/A.TXT", "a"));
  std::vector<std::unique_ptr<gb::File>> files;
  for (std::string_view path : {"ozdisk:/B", "ozdisk:/C", "ozdisk:/D"}) {
    files.push_back(file_system_.OpenFile(path, gb::kNewFileFlags));
    ASSERT_NE(files.back(), nullptr);
  }
  EXPECT_FALSE(protocol_->GetFileSystem()->HasFreeSlot());
  EXPECT_EQ(file_system_.OpenFile("ozdisk:/E", gb::kNewFileFlags), nullptr);
  EXPECT_TRUE(file_system_.IsValidFile("ozdisk:/A.TXT"));
  EXPECT_EQ(static_cast<int>(file_system_.List("ozdisk:/").size()), 4);
}

TEST_F(DiskFileProtocolTest, RootGrowsWhileOpen) {
  MountFormatted();

  // A cluster of the root holds 16 entries a sector, and a disk this size has
  // a sector a cluster.
  std::vector<std::string> paths;
  for (int i = 0; i < 20; ++i) {
    paths.push_back(absl::StrCat("ozdisk:/FILE", i));
    ASSERT_TRUE(file_system_.WriteFile(paths.back(), "x")) << i;
  }
  EXPECT_THAT(file_system_.List("ozdisk:/"), UnorderedElementsAreArray(paths));
  EXPECT_TRUE(file_system_.IsValidFile("ozdisk:/FILE19"));
  EXPECT_EQ(UnmountAndCheck(), DiskCheckReport());
}

//==============================================================================
// Bytes and words
//==============================================================================

TEST_F(DiskFileProtocolTest, WordsAreTwoBytesLowFirst) {
  MountFormatted();
  ASSERT_TRUE(WriteBytes("ozdisk:/A", {0x01, 0x02, 0x03, 0x04}));
  EXPECT_THAT(ReadWords("A"), ElementsAre(0x0201, 0x0403));
  EXPECT_EQ(file_system_.GetPathInfo("ozdisk:/A").size, 4);
  EXPECT_THAT(ReadBytes("ozdisk:/A"), ElementsAre(0x01, 0x02, 0x03, 0x04));
}

TEST_F(DiskFileProtocolTest, OddSizePadsTheLastWord) {
  MountFormatted();
  ASSERT_TRUE(WriteBytes("ozdisk:/A", {0x01, 0x02, 0x03}));
  EXPECT_THAT(ReadWords("A"), ElementsAre(0x0201, 0x0003));
  EXPECT_EQ(file_system_.GetPathInfo("ozdisk:/A").size, 4);
  EXPECT_THAT(ReadBytes("ozdisk:/A"), ElementsAre(0x01, 0x02, 0x03, 0x00));
}

TEST_F(DiskFileProtocolTest, BytesOneAtATimeWriteTheSameWords) {
  MountFormatted();
  std::unique_ptr<gb::File> file =
      file_system_.OpenFile("ozdisk:/A", gb::kNewFileFlags);
  ASSERT_NE(file, nullptr);
  for (uint8_t byte : {0x01, 0x02, 0x03, 0x04, 0x05}) {
    ASSERT_EQ(file->Write(&byte, 1), 1);
  }
  file.reset();
  EXPECT_THAT(ReadWords("A"), ElementsAre(0x0201, 0x0403, 0x0005));
}

TEST_F(DiskFileProtocolTest, OddOffsetsChangeOnlyTheirByte) {
  MountFormatted();
  ASSERT_TRUE(WriteBytes("ozdisk:/A", {0x01, 0x02, 0x03, 0x04, 0x05, 0x06}));
  std::unique_ptr<gb::File> file =
      file_system_.OpenFile("ozdisk:/A", gb::kReadWriteFileFlags);
  ASSERT_NE(file, nullptr);

  // Reading from an odd offset starts with the high byte.
  EXPECT_EQ(file->SeekTo(1), 1);
  uint8_t bytes[3] = {};
  EXPECT_EQ(file->Read(bytes, 3), 3);
  EXPECT_THAT(bytes, ElementsAre(0x02, 0x03, 0x04));

  // Writing from an odd offset to an even one keeps both ends' other bytes.
  EXPECT_EQ(file->SeekTo(1), 1);
  const uint8_t changes[] = {0xA2, 0xA3, 0xA4};
  EXPECT_EQ(file->Write(changes, 3), 3);
  file.reset();
  EXPECT_THAT(ReadWords("A"), ElementsAre(0xA201, 0xA4A3, 0x0605));
}

TEST_F(DiskFileProtocolTest, SeekStopsAtTheEnd) {
  MountFormatted();
  ASSERT_TRUE(WriteBytes("ozdisk:/A", {0x01, 0x02, 0x03}));
  std::unique_ptr<gb::File> file =
      file_system_.OpenFile("ozdisk:/A", gb::kReadFileFlags);
  ASSERT_NE(file, nullptr);
  EXPECT_EQ(file->SeekEnd(), 4);
  EXPECT_EQ(file->SeekTo(100), 4);
  uint8_t byte = 0;
  EXPECT_EQ(file->Read(&byte, 1), 0);
}

TEST_F(DiskFileProtocolTest, ReadsAndWritesAcrossSectorsAndClusters) {
  MountFormatted();
  std::vector<uint8_t> bytes(3001);
  for (int i = 0; i < static_cast<int>(bytes.size()); ++i) {
    bytes[i] = static_cast<uint8_t>(i * 7);
  }
  ASSERT_TRUE(WriteBytes("ozdisk:/BIG", bytes));
  EXPECT_EQ(file_system_.GetPathInfo("ozdisk:/BIG").size, 3002);
  bytes.push_back(0);
  EXPECT_EQ(ReadBytes("ozdisk:/BIG"), bytes);
  EXPECT_EQ(UnmountAndCheck(), DiskCheckReport());
}

TEST_F(DiskFileProtocolTest, ResetTruncates) {
  MountFormatted();
  ASSERT_TRUE(WriteBytes("ozdisk:/A", std::vector<uint8_t>(2000, 0x55)));
  ASSERT_TRUE(WriteBytes("ozdisk:/A", {0x01, 0x02}));
  EXPECT_THAT(ReadWords("A"), ElementsAre(0x0201));
  EXPECT_EQ(UnmountAndCheck(), DiskCheckReport());
}

TEST_F(DiskFileProtocolTest, ProgramFilesReadBackUnchanged) {
  MountFormatted();
  Program program;
  program.banks = {{.assign_mask = 0x0003}, {}};
  program.segments = {
      {.bank = 0,
       .access = Program::Segment::Access::kReadOnly,
       .page_start = 2,
       .page_count = 1,
       .data = {{.offset = 0x10, .data = {1, 2, 3, 4}}}},
      {.bank = 1,
       .access = Program::Segment::Access::kReadWrite,
       .page_count = 2,
       .data = {{.offset = 0, .data = {5, 6}}, {.offset = 6, .data = {7, 8}}}},
  };
  program.code_segment = 0;
  program.stack_segment = 1;
  program.data_segment = 1;
  program.instances = {{.data = 0x100}, {.stack = 0x80, .data = 0x200}};
  ASSERT_TRUE(WriteProgramFile(file_system_, "ozdisk:/TEST.OZ3", program).ok());
  absl::StatusOr<Program> read =
      ReadProgramFile(file_system_, "ozdisk:/test.oz3");
  ASSERT_TRUE(read.ok()) << read.status();
  EXPECT_EQ(*WriteProgramWords(*read), *WriteProgramWords(program));
  EXPECT_EQ(ReadWords("TEST.OZ3"), *WriteProgramWords(program));
}

//==============================================================================
// Times
//==============================================================================

TEST_F(DiskFileProtocolTest, FilesAreStampedWhenCommitted) {
  MountFormatted();
  std::unique_ptr<gb::File> file =
      file_system_.OpenFile("ozdisk:/A", gb::kNewFileFlags);
  ASSERT_NE(file, nullptr);
  ASSERT_EQ(file->WriteString("ab"), 2);
  now_ = absl::CivilSecond(2026, 11, 18, 9, 0, 0);
  file.reset();
  EXPECT_EQ(GetEntry("A")->GetModified(), DateTime::FromCivil(now_));
}

TEST_F(DiskFileProtocolTest, NoClockStampsNoDate) {
  gb::FileSystem file_system;
  ASSERT_TRUE(
      file_system.Register(std::make_unique<gb::MemoryFileProtocol>(), "host"));
  auto owned = std::make_unique<DiskFileProtocol>();
  DiskFileProtocol* protocol = owned.get();
  ASSERT_TRUE(file_system.Register(std::move(owned)));
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system, kImagePath, kSectorCount);
  ASSERT_TRUE(image.ok());
  EXPECT_EQ(protocol->Mount(*std::move(image)), DiskStatus::kUnformatted);
  EXPECT_EQ(protocol->Format(DiskLabel(), 0), DiskStatus::kOk);
  ASSERT_TRUE(file_system.WriteFile("ozdisk:/A", "ab"));
  DiskFileSystem* disk = protocol->GetFileSystem();
  ASSERT_EQ(disk->Seek(protocol->GetRoot(), 0), DiskStatus::kOk);
  DiskDirEntry entry;
  ASSERT_EQ(disk->Find(protocol->GetRoot(), *DiskFilename::Parse("A"), &entry),
            DiskStatus::kOk);
  EXPECT_EQ(entry.GetModified(), DateTime());
}

//==============================================================================
// Attributes
//==============================================================================

TEST_F(DiskFileProtocolTest, AttributesAreTheEntrys) {
  MountFormatted();
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/A", "ab"));
  EXPECT_TRUE(file_system_.GetPathInfo("ozdisk:/A").attributes.IsEmpty());
  EXPECT_TRUE(file_system_.GetPathInfo("ozdisk:/").attributes.IsEmpty());

  EXPECT_TRUE(
      file_system_.SetAttributes("ozdisk:/A", gb::FileAttribute::kHidden));
  EXPECT_EQ(file_system_.GetPathInfo("ozdisk:/A").attributes,
            gb::FileAttributes(gb::FileAttribute::kHidden));
  EXPECT_EQ(GetEntry("A")->GetAttributes(), kDiskAttribHidden);
  EXPECT_THAT(file_system_.List("ozdisk:/"), ElementsAre("ozdisk:/A"));

  EXPECT_TRUE(file_system_.SetAttributes("ozdisk:/A", gb::kAllFileAttributes));
  EXPECT_EQ(GetEntry("A")->GetAttributes(),
            kDiskAttribHidden | kDiskAttribReadOnly);
  EXPECT_TRUE(file_system_.SetAttributes("ozdisk:/A", {}));
  EXPECT_EQ(GetEntry("A")->GetAttributes(), 0);
  EXPECT_FALSE(file_system_.SetAttributes("ozdisk:/MISSING", {}));
}

TEST_F(DiskFileProtocolTest, ReadOnlyFilesRefuseWritesReplacesAndDeletes) {
  MountFormatted();
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/A", "ab"));
  ASSERT_TRUE(
      file_system_.SetAttributes("ozdisk:/A", gb::FileAttribute::kReadOnly));
  EXPECT_EQ(file_system_.OpenFile("ozdisk:/A", gb::kReadWriteFileFlags),
            nullptr);
  EXPECT_FALSE(file_system_.WriteFile("ozdisk:/A", "cd"));
  ASSERT_TRUE(file_system_.WriteFile("host:/b.txt", "cd"));
  EXPECT_FALSE(file_system_.CopyFile("host:/b.txt", "ozdisk:/A"));
  EXPECT_FALSE(file_system_.DeleteFile("ozdisk:/A"));
  EXPECT_THAT(ReadBytes("ozdisk:/A"), ElementsAre('a', 'b'));
}

TEST_F(DiskFileProtocolTest, OpenFilesRefuseDeletesRenamesAndAttributes) {
  MountFormatted();
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/A", "ab"));
  std::unique_ptr<gb::File> file =
      file_system_.OpenFile("ozdisk:/A", gb::kReadFileFlags);
  ASSERT_NE(file, nullptr);
  EXPECT_FALSE(file_system_.DeleteFile("ozdisk:/A"));
  EXPECT_FALSE(file_system_.MoveFile("ozdisk:/A", "ozdisk:/B"));
  EXPECT_FALSE(
      file_system_.SetAttributes("ozdisk:/A", gb::FileAttribute::kHidden));
  EXPECT_EQ(file_system_.OpenFile("ozdisk:/A", gb::kReadFileFlags), nullptr);
  file.reset();
  EXPECT_TRUE(file_system_.DeleteFile("ozdisk:/A"));
  EXPECT_FALSE(file_system_.IsValidPath("ozdisk:/A"));
  EXPECT_TRUE(file_system_.DeleteFile("ozdisk:/A"));
  EXPECT_EQ(UnmountAndCheck(), DiskCheckReport());
}

TEST_F(DiskFileProtocolTest, CopiesKeepAttributesBothWays) {
  MountFormatted();
  ASSERT_TRUE(file_system_.WriteFile("host:/a.txt", "abc"));
  ASSERT_TRUE(
      file_system_.SetAttributes("host:/a.txt", gb::kAllFileAttributes));
  ASSERT_TRUE(file_system_.CopyFile("host:/a.txt", "ozdisk:/A.TXT"));
  EXPECT_EQ(file_system_.GetPathInfo("ozdisk:/A.TXT").attributes,
            gb::kAllFileAttributes);
  EXPECT_THAT(ReadWords("A.TXT"), ElementsAre(0x6261, 0x0063));

  ASSERT_TRUE(file_system_.CopyFile("ozdisk:/A.TXT", "host:/b.txt"));
  EXPECT_EQ(file_system_.GetPathInfo("host:/b.txt").attributes,
            gb::kAllFileAttributes);
  EXPECT_THAT(ReadBytes("host:/b.txt"), ElementsAre('a', 'b', 'c', 0));
}

//==============================================================================
// Moves
//==============================================================================

TEST_F(DiskFileProtocolTest, MovesRename) {
  MountFormatted();
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/A", "ab"));
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/B", "cd"));
  EXPECT_FALSE(file_system_.MoveFile("ozdisk:/A", "ozdisk:/B"));
  EXPECT_TRUE(file_system_.MoveFile("ozdisk:/A", "ozdisk:/C.TXT"));
  EXPECT_THAT(file_system_.List("ozdisk:/"),
              UnorderedElementsAre("ozdisk:/B", "ozdisk:/C.TXT"));
  EXPECT_THAT(ReadBytes("ozdisk:/C.TXT"), ElementsAre('a', 'b'));

  // Names are upper case, so changing case changes nothing.
  EXPECT_TRUE(file_system_.MoveFile("ozdisk:/C.TXT", "ozdisk:/c.txt"));
  EXPECT_THAT(file_system_.List("ozdisk:/"),
              UnorderedElementsAre("ozdisk:/B", "ozdisk:/C.TXT"));
  EXPECT_FALSE(file_system_.MoveFile("ozdisk:/MISSING", "ozdisk:/D"));
  EXPECT_FALSE(file_system_.MoveFile("ozdisk:/B", "ozdisk:/sub/B"));
}

//==============================================================================
// Write protection and failures
//==============================================================================

TEST_F(DiskFileProtocolTest, WriteProtectedDiskOnlyReads) {
  MountFormatted();
  ASSERT_TRUE(file_system_.WriteFile("ozdisk:/A", "ab"));
  std::unique_ptr<DiskImage> image;
  ASSERT_EQ(protocol_->Unmount(&image), DiskStatus::kOk);
  ASSERT_EQ(image->SetWriteProtected(true), DiskStatus::kOk);
  ASSERT_EQ(protocol_->Mount(std::move(image)), DiskStatus::kOk);

  EXPECT_THAT(ReadBytes("ozdisk:/A"), ElementsAre('a', 'b'));
  EXPECT_EQ(file_system_.OpenFile("ozdisk:/A", gb::kReadWriteFileFlags),
            nullptr);
  EXPECT_FALSE(file_system_.WriteFile("ozdisk:/B", "cd"));
  EXPECT_FALSE(file_system_.DeleteFile("ozdisk:/A"));
  EXPECT_FALSE(file_system_.MoveFile("ozdisk:/A", "ozdisk:/B"));
  EXPECT_FALSE(
      file_system_.SetAttributes("ozdisk:/A", gb::FileAttribute::kHidden));
}

TEST_F(DiskFileProtocolTest, HostFailureFailsTheWrite) {
  MountFormatted();
  std::unique_ptr<gb::File> file =
      file_system_.OpenFile("ozdisk:/A", gb::kNewFileFlags);
  ASSERT_NE(file, nullptr);
  host_->SetLimits({.writes = 0});
  EXPECT_LT(file->Write(std::vector<uint8_t>(2000, 1)), 2000);
  EXPECT_FALSE(file->Flush());
  file.reset();
  EXPECT_EQ(protocol_->Unmount(), DiskStatus::kIoError);
}

}  // namespace
}  // namespace oz3
