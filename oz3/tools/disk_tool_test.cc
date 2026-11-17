// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/disk_tool.h"

#include <array>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_split.h"
#include "absl/time/civil_time.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
#include "gb/file/memory_file_protocol.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/disk/disk_check.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_media.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/faulty_file_protocol.h"
#include "oz3/tools/disk_file_protocol.h"

namespace oz3 {
namespace {

using ::testing::HasSubstr;

constexpr char kImagePath[] = "mem:/work.ozd";
constexpr int kSectorCount = 720;
const absl::CivilSecond kNow(2026, 11, 17, 10, 30, 15);

// The layout formatting kSectorCount sectors with the drive's choice gives.
const DiskLayout kLayout = *ChooseDiskLayout(kSectorCount, 0);

// Memory files whose attributes can never be set, though it supports them.
class FixedAttributesProtocol final : public gb::MemoryFileProtocol {
 protected:
  bool BasicSetAttributes(std::string_view protocol_name, std::string_view path,
                          gb::FileAttributes attributes) override {
    return false;
  }
};

// Memory files whose current folder is always /here.
class CurrentFolderProtocol final : public gb::MemoryFileProtocol {
 public:
  CurrentFolderProtocol()
      : MemoryFileProtocol(
            {kDefaultFlags, gb::FileProtocolFlag::kCurrentPath}) {}

 protected:
  std::string DoGetCurrentPath(std::string_view protocol_name) override {
    return "/here";
  }
};

class DiskToolTest : public ::testing::Test {
 protected:
  void SetUp() override {
    auto host = std::make_unique<FaultyFileProtocol>();
    host_ = host.get();
    file_system_.Register(std::move(host), "mem");
    absl::StatusOr<std::unique_ptr<DiskTool>> tool =
        DiskTool::Create(&file_system_, [] { return kNow; });
    ASSERT_TRUE(tool.ok()) << tool.status();
    tool_ = *std::move(tool);
  }

  // Makes a new image at kImagePath, formatted as WORK.
  void NewFormatted() {
    ASSERT_TRUE(tool_->New(kImagePath, kSectorCount).ok());
    ASSERT_TRUE(tool_->Format(kImagePath, *DiskLabel::Parse("WORK"), 0).ok());
  }

  // Returns what CheckDisk reports for the image at kImagePath.
  DiskCheckReport CheckImage() {
    DiskCheckReport report;
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&file_system_, kImagePath);
    EXPECT_TRUE(image.ok()) << image.status();
    if (image.ok()) {
      EXPECT_EQ(CheckDisk(**image, &report), DiskStatus::kOk);
    }
    return report;
  }

  // Sets FAT word `index` of the image, which must be in the FAT's first
  // sector.
  void SetFatWord(int index, uint16_t word) {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&file_system_, kImagePath);
    ASSERT_TRUE(image.ok()) << image.status();
    std::array<uint16_t, kDiskSectorWords> words;
    const int sector = DiskLayout::GetFatSector(0);
    ASSERT_EQ((*image)->ReadSector(sector, absl::MakeSpan(words)),
              DiskStatus::kOk);
    words[index] = word;
    ASSERT_EQ((*image)->WriteSector(sector, words), DiskStatus::kOk);
  }

  // Empties the image's free list, so every free cluster is on no chain.
  void BreakFreeList() { SetFatWord(0, DiskLayout::kFatEnd); }

  // Makes the image's free list loop, so the file system is corrupt.
  void LoopFreeList() {
    SetFatWord(0, 2);
    SetFatWord(2, 2);
  }

  // Writes `entries` as the only entries in the root's first sector, which a
  // disk formatted by the tool has in cluster 1.
  void SetRootEntries(absl::Span<const DiskDirEntry> entries) {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&file_system_, kImagePath);
    ASSERT_TRUE(image.ok()) << image.status();
    std::array<uint16_t, kDiskSectorWords> words = {};
    for (int i = 0; i < static_cast<int>(entries.size()); ++i) {
      absl::c_copy(entries[i].GetWords(),
                   words.begin() + i * DiskDirEntry::kWordCount);
    }
    ASSERT_EQ((*image)->WriteSector(kLayout.GetClusterSector(1), words),
              DiskStatus::kOk);
  }

  // Writes two empty files named `name` as the only entries of the root.
  void AddDuplicateFiles(std::string_view name) {
    DiskDirEntry entry;
    entry.SetName(*DiskFilename::Parse(name));
    SetRootEntries({entry, entry});
  }

  // Writes a file named `name` with a size but no clusters as the only entry of
  // the root, so the file system finds it corrupt.
  void AddCorruptFile(std::string_view name) {
    DiskDirEntry entry;
    entry.SetName(*DiskFilename::Parse(name));
    entry.SetSize(10);
    SetRootEntries({entry});
  }

  // Returns the text `result` holds, failing the test if it holds none.
  static std::string Text(const absl::StatusOr<std::string>& result) {
    EXPECT_TRUE(result.ok()) << result.status();
    return result.ok() ? *result : "";
  }

  // Returns the bytes of the host file at `path`.
  std::string ReadHost(std::string_view path) {
    std::string bytes;
    EXPECT_TRUE(file_system_.ReadFile(path, &bytes)) << path;
    return bytes;
  }

  // Writes `bytes` to the host file at `path`.
  void WriteHost(std::string_view path, std::string_view bytes) {
    ASSERT_TRUE(file_system_.WriteFile(path, bytes)) << path;
  }

  // Puts the host file at `path`, holding `bytes`, onto the disk as `name`.
  void PutFile(std::string_view name, std::string_view bytes) {
    WriteHost("mem:/put.bin", bytes);
    ASSERT_TRUE(tool_->Put(kImagePath, "mem:/put.bin", name, false).ok());
  }

  // Returns the bytes of the file `name` on the disk, or nullopt if it can't
  // be read.
  std::optional<std::string> GetFile(std::string_view name) {
    file_system_.DeleteFile("mem:/get.bin");
    if (!tool_->Get(kImagePath, name, "mem:/get.bin", false).ok()) {
      return std::nullopt;
    }
    return ReadHost("mem:/get.bin");
  }

  // Sets the attribute `attribute` of the file `name` on the disk.
  void SetDiskAttributes(std::string_view name, gb::FileAttribute attribute) {
    ASSERT_TRUE(tool_->Attrib(kImagePath, name, attribute, {}).ok());
  }

  // Returns the names of the files on the disk, hidden ones included.
  std::string AllNames() {
    absl::StatusOr<std::string> text = tool_->Dir(kImagePath, "", true);
    EXPECT_TRUE(text.ok()) << text.status();
    if (!text.ok() || *text == "no files\n") {
      return "";
    }
    std::string names;
    for (std::string_view line :
         absl::StrSplit(*text, '\n', absl::SkipEmpty())) {
      absl::StrAppend(&names, names.empty() ? "" : " ",
                      line.substr(0, line.find(' ')));
    }
    return names;
  }

  // Sets the image's write protection, as DiskTool::Protect does.
  void SetProtected(bool write_protected) {
    ASSERT_TRUE(tool_->Protect(kImagePath, write_protected).ok());
  }

  gb::FileSystem file_system_;
  FaultyFileProtocol* host_ = nullptr;
  std::unique_ptr<DiskTool> tool_;
};

//==============================================================================
// Construction
//==============================================================================

TEST_F(DiskToolTest, RegistersItsProtocol) {
  EXPECT_TRUE(file_system_.IsRegistered("ozdisk"));
}

TEST_F(DiskToolTest, ProtocolNameMustBeFree) {
  absl::StatusOr<std::unique_ptr<DiskTool>> tool =
      DiskTool::Create(&file_system_, nullptr, "mem");
  EXPECT_FALSE(tool.ok());
  EXPECT_THAT(tool.status().message(), HasSubstr("\"mem\""));
  EXPECT_TRUE(DiskTool::Create(&file_system_, nullptr, "other").ok());
}

//==============================================================================
// Sizes
//==============================================================================

TEST(DiskToolSizeTest, SizesInWords) {
  EXPECT_EQ(DiskTool::ParseSectorCount("256").value_or(-1), 1);
  EXPECT_EQ(DiskTool::ParseSectorCount("512").value_or(-1), 2);
  EXPECT_EQ(DiskTool::ParseSectorCount("720K").value_or(-1), 2880);
  EXPECT_EQ(DiskTool::ParseSectorCount("720k").value_or(-1), 2880);
  EXPECT_EQ(DiskTool::ParseSectorCount("1M").value_or(-1), 4096);
  EXPECT_EQ(DiskTool::ParseSectorCount("1m").value_or(-1), 4096);
  EXPECT_EQ(DiskTool::ParseSectorCount("0001K").value_or(-1), 4);
}

TEST(DiskToolSizeTest, LargestSize) {
  const std::string largest =
      absl::StrCat(int64_t{DiskMedia::kMaxSectorCount} * kDiskSectorWords);
  EXPECT_EQ(DiskTool::ParseSectorCount(largest).value_or(-1),
            DiskMedia::kMaxSectorCount);
  EXPECT_EQ(DiskTool::ParseSectorCount("1024M").value_or(-1), 4194304);
}

TEST(DiskToolSizeTest, SizesThatArentNumbers) {
  for (std::string_view text :
       {"", "K", "M", "-256", "+256", " 256", "256 ", "256KK", "256X", "2.5K",
        "0x100", "99999999999999999999K"}) {
    absl::StatusOr<int> size = DiskTool::ParseSectorCount(text);
    ASSERT_FALSE(size.ok()) << text;
    EXPECT_EQ(size.status().message(),
              absl::StrCat("Size \"", text,
                           "\" isn't a number of words, with an optional K or "
                           "M"));
  }
}

TEST(DiskToolSizeTest, SizesOutOfRange) {
  for (std::string_view text : {"0", "0K", "0M", "1025M", "4194434K"}) {
    absl::StatusOr<int> size = DiskTool::ParseSectorCount(text);
    ASSERT_FALSE(size.ok()) << text;
    EXPECT_EQ(size.status().message(),
              absl::StrCat("Size ", text,
                           " is out of range: an image has 1 to 4194433 "
                           "sectors of 256 words"));
  }
}

TEST(DiskToolSizeTest, SizesThatArentWholeSectors) {
  for (std::string_view text : {"1", "255", "257", "1000"}) {
    absl::StatusOr<int> size = DiskTool::ParseSectorCount(text);
    ASSERT_FALSE(size.ok()) << text;
    EXPECT_EQ(size.status().message(),
              absl::StrCat("Size ", text,
                           " isn't a whole number of sectors of 256 words"));
  }
}

//==============================================================================
// New
//==============================================================================

TEST_F(DiskToolTest, NewMakesABlankImage) {
  ASSERT_TRUE(tool_->New(kImagePath, kSectorCount).ok());
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kImagePath);
  ASSERT_TRUE(image.ok()) << image.status();
  EXPECT_EQ((*image)->GetSectorCount(), kSectorCount);
  EXPECT_FALSE((*image)->IsWriteProtected());
  DiskCheckReport report;
  EXPECT_EQ(CheckDisk(**image, &report), DiskStatus::kUnformatted);
}

TEST_F(DiskToolTest, NewNeverReplacesAFile) {
  ASSERT_TRUE(file_system_.WriteFile(kImagePath, "keep"));
  absl::Status status = tool_->New(kImagePath, kSectorCount);
  EXPECT_EQ(status.code(), absl::StatusCode::kAlreadyExists);
  EXPECT_EQ(status.message(), "mem:/work.ozd: already exists");
  std::string contents;
  EXPECT_TRUE(file_system_.ReadFile(kImagePath, &contents));
  EXPECT_EQ(contents, "keep");
}

TEST_F(DiskToolTest, NewNeverReplacesAnImage) {
  NewFormatted();
  EXPECT_FALSE(tool_->New(kImagePath, kSectorCount).ok());
  EXPECT_TRUE(CheckImage() == DiskCheckReport());
}

TEST_F(DiskToolTest, NewFailsWithNoSectors) {
  EXPECT_FALSE(tool_->New(kImagePath, 0).ok());
  EXPECT_FALSE(file_system_.IsValidPath(kImagePath));
}

//==============================================================================
// Format and Info
//==============================================================================

TEST_F(DiskToolTest, InfoOfAnUnformattedImage) {
  ASSERT_TRUE(tool_->New(kImagePath, kSectorCount).ok());
  EXPECT_EQ(Text(tool_->Info(kImagePath)),
            "Size:     180K words (720 sectors)\n"
            "Protect:  off\n"
            "Volume:   unformatted\n");
}

TEST_F(DiskToolTest, FormatWithTheDrivesClusterSize) {
  NewFormatted();
  EXPECT_EQ(kLayout.sectors_per_cluster, 1);
  EXPECT_EQ(Text(tool_->Info(kImagePath)),
            absl::StrCat("Size:     180K words (720 sectors)\n"
                         "Protect:  off\n"
                         "Volume:   WORK\n"
                         "Cluster:  256 words (1 sector)\n"
                         "Clusters: ",
                         kLayout.cluster_count,
                         "\nFree:     ", kLayout.cluster_count - 1, " (",
                         (kLayout.cluster_count - 1) * 256, " words)\n"));
  EXPECT_TRUE(CheckImage() == DiskCheckReport());
}

TEST_F(DiskToolTest, FormatWithAClusterSizeAndNoLabel) {
  ASSERT_TRUE(tool_->New(kImagePath, 4096).ok());
  ASSERT_TRUE(tool_->Format(kImagePath, DiskLabel(), 4).ok());
  const DiskLayout layout = *ChooseDiskLayout(4096, 4);
  EXPECT_EQ(Text(tool_->Info(kImagePath)),
            absl::StrCat("Size:     1M words (4096 sectors)\n"
                         "Protect:  off\n"
                         "Volume:   (no label)\n"
                         "Cluster:  1K words (4 sectors)\n"
                         "Clusters: ",
                         layout.cluster_count,
                         "\nFree:     ", layout.cluster_count - 1, " (",
                         layout.cluster_count - 1, "K words)\n"));
  EXPECT_TRUE(CheckImage() == DiskCheckReport());
}

TEST_F(DiskToolTest, FormatAgain) {
  NewFormatted();
  ASSERT_TRUE(tool_->Format(kImagePath, *DiskLabel::Parse("NEW"), 0).ok());
  EXPECT_THAT(Text(tool_->Info(kImagePath)), HasSubstr("Volume:   NEW\n"));
  EXPECT_TRUE(CheckImage() == DiskCheckReport());
}

TEST_F(DiskToolTest, FormatWithAClusterSizeWithNoLayout) {
  ASSERT_TRUE(tool_->New(kImagePath, kSectorCount).ok());
  absl::Status status = tool_->Format(kImagePath, DiskLabel(), 3);
  EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument);
  EXPECT_EQ(status.message(),
            "mem:/work.ozd: a disk of 720 sectors can't have clusters of 3 "
            "sectors");
  EXPECT_FALSE(tool_->Format(kImagePath, DiskLabel(), 128).ok());
  EXPECT_THAT(Text(tool_->Info(kImagePath)),
              HasSubstr("Volume:   unformatted"));
}

TEST_F(DiskToolTest, FormatAProtectedImage) {
  ASSERT_TRUE(tool_->New(kImagePath, kSectorCount).ok());
  SetProtected(true);
  absl::Status status = tool_->Format(kImagePath, DiskLabel(), 0);
  EXPECT_EQ(status.code(), absl::StatusCode::kFailedPrecondition);
  EXPECT_EQ(status.message(), "mem:/work.ozd: write protected");
  EXPECT_EQ(Text(tool_->Info(kImagePath)),
            "Size:     180K words (720 sectors)\n"
            "Protect:  on\n"
            "Volume:   unformatted\n");
}

TEST_F(DiskToolTest, CommandsOnAMissingImage) {
  EXPECT_EQ(tool_->Format(kImagePath, DiskLabel(), 0).code(),
            absl::StatusCode::kNotFound);
  EXPECT_EQ(tool_->Info(kImagePath).status().code(),
            absl::StatusCode::kNotFound);
  EXPECT_EQ(tool_->Label(kImagePath, DiskLabel()).code(),
            absl::StatusCode::kNotFound);
  EXPECT_EQ(tool_->Protect(kImagePath, true).code(),
            absl::StatusCode::kNotFound);
  EXPECT_EQ(tool_->Check(kImagePath, false).status().code(),
            absl::StatusCode::kNotFound);
}

TEST_F(DiskToolTest, CommandsOnAFileThatIsntAnImage) {
  ASSERT_TRUE(file_system_.WriteFile(kImagePath, "not an image at all"));
  absl::StatusOr<std::string> info = tool_->Info(kImagePath);
  ASSERT_FALSE(info.ok());
  EXPECT_THAT(info.status().message(), HasSubstr("mem:/work.ozd"));
}

//==============================================================================
// Label
//==============================================================================

TEST_F(DiskToolTest, LabelSetsTheLabel) {
  NewFormatted();
  ASSERT_TRUE(tool_->Label(kImagePath, *DiskLabel::Parse("games2")).ok());
  EXPECT_THAT(Text(tool_->Info(kImagePath)), HasSubstr("Volume:   GAMES2\n"));
  ASSERT_TRUE(tool_->Label(kImagePath, DiskLabel()).ok());
  EXPECT_THAT(Text(tool_->Info(kImagePath)),
              HasSubstr("Volume:   (no label)\n"));
}

TEST_F(DiskToolTest, LabelNeedsAFileSystem) {
  ASSERT_TRUE(tool_->New(kImagePath, kSectorCount).ok());
  EXPECT_EQ(tool_->Label(kImagePath, DiskLabel()).message(),
            "mem:/work.ozd: unformatted");
}

TEST_F(DiskToolTest, LabelOnAProtectedImage) {
  NewFormatted();
  SetProtected(true);
  EXPECT_EQ(tool_->Label(kImagePath, DiskLabel()).message(),
            "mem:/work.ozd: write protected");
  EXPECT_THAT(Text(tool_->Info(kImagePath)), HasSubstr("Volume:   WORK\n"));
}

//==============================================================================
// Protect
//==============================================================================

TEST_F(DiskToolTest, ProtectBothWays) {
  NewFormatted();
  SetProtected(true);
  EXPECT_THAT(Text(tool_->Info(kImagePath)), HasSubstr("Protect:  on\n"));
  SetProtected(true);
  SetProtected(false);
  EXPECT_THAT(Text(tool_->Info(kImagePath)), HasSubstr("Protect:  off\n"));
  ASSERT_TRUE(tool_->Label(kImagePath, DiskLabel()).ok());
}

TEST_F(DiskToolTest, ProtectOffFailsOnAReadOnlyImageFile) {
  NewFormatted();
  ASSERT_TRUE(
      file_system_.SetAttributes(kImagePath, gb::FileAttribute::kReadOnly));
  EXPECT_TRUE(tool_->Protect(kImagePath, true).ok());
  absl::Status status = tool_->Protect(kImagePath, false);
  EXPECT_EQ(status.code(), absl::StatusCode::kFailedPrecondition);
  EXPECT_EQ(status.message(),
            "mem:/work.ozd: the image file is read-only, so it is always "
            "write protected");
  EXPECT_THAT(Text(tool_->Info(kImagePath)), HasSubstr("Protect:  on\n"));
}

//==============================================================================
// Check
//==============================================================================

TEST_F(DiskToolTest, CheckASoundDisk) {
  NewFormatted();
  EXPECT_EQ(Text(tool_->Check(kImagePath, false)), "no problems\n");
  EXPECT_EQ(Text(tool_->Check(kImagePath, true)), "no problems\n");
}

TEST_F(DiskToolTest, CheckABrokenDiskWithoutFixing) {
  NewFormatted();
  BreakFreeList();
  const std::string expected =
      absl::StrCat(kLayout.cluster_count - 1, " clusters on no chain\n");
  EXPECT_EQ(Text(tool_->Check(kImagePath, false)), expected);
  EXPECT_EQ(Text(tool_->Check(kImagePath, false)), expected);
}

TEST_F(DiskToolTest, CheckABrokenDiskAndFix) {
  NewFormatted();
  BreakFreeList();
  EXPECT_EQ(Text(tool_->Check(kImagePath, true)),
            absl::StrCat(kLayout.cluster_count - 1,
                         " clusters on no chain\nrepaired\n"));
  EXPECT_EQ(Text(tool_->Check(kImagePath, false)), "no problems\n");
  EXPECT_TRUE(CheckImage() == DiskCheckReport());
}

TEST_F(DiskToolTest, CheckLeavesDuplicateNames) {
  NewFormatted();
  AddDuplicateFiles("A");
  EXPECT_EQ(Text(tool_->Check(kImagePath, false)), "A: more than one entry\n");
  EXPECT_EQ(Text(tool_->Check(kImagePath, true)), "A: more than one entry\n");
}

TEST_F(DiskToolTest, CheckAndFixWithDuplicateNames) {
  NewFormatted();
  AddDuplicateFiles("A");
  BreakFreeList();
  EXPECT_EQ(Text(tool_->Check(kImagePath, true)),
            absl::StrCat(kLayout.cluster_count - 1,
                         " clusters on no chain\nA: more than one entry\n"
                         "repaired\n"));
  EXPECT_EQ(Text(tool_->Check(kImagePath, false)), "A: more than one entry\n");
}

TEST_F(DiskToolTest, CheckCantFixAProtectedDisk) {
  NewFormatted();
  BreakFreeList();
  SetProtected(true);
  absl::StatusOr<std::string> result = tool_->Check(kImagePath, true);
  ASSERT_FALSE(result.ok());
  EXPECT_EQ(result.status().message(), "mem:/work.ozd: write protected");
  EXPECT_EQ(Text(tool_->Check(kImagePath, false)),
            absl::StrCat(kLayout.cluster_count - 1, " clusters on no chain\n"));
}

TEST_F(DiskToolTest, CheckAnUnformattedDisk) {
  ASSERT_TRUE(tool_->New(kImagePath, kSectorCount).ok());
  absl::StatusOr<std::string> result = tool_->Check(kImagePath, false);
  ASSERT_FALSE(result.ok());
  EXPECT_EQ(result.status().message(), "mem:/work.ozd: unformatted");
}

TEST_F(DiskToolTest, InfoOfAnEmptiedFreeList) {
  NewFormatted();
  BreakFreeList();
  EXPECT_THAT(Text(tool_->Info(kImagePath)),
              HasSubstr("Free:     0 (0 words)\n"));
}

TEST_F(DiskToolTest, InfoOfACorruptDisk) {
  NewFormatted();
  LoopFreeList();
  EXPECT_EQ(Text(tool_->Info(kImagePath)),
            "Size:     180K words (720 sectors)\n"
            "Protect:  off\n"
            "Volume:   corrupt\n");
}

//==============================================================================
// Attribute letters
//==============================================================================

TEST(DiskToolAttributesTest, Letters) {
  using gb::FileAttribute;
  const gb::FileAttributes both = {FileAttribute::kReadOnly,
                                   FileAttribute::kHidden};
  EXPECT_EQ(DiskTool::ParseAttributes("").value_or(both), gb::FileAttributes());
  EXPECT_EQ(DiskTool::ParseAttributes("r").value_or(both),
            gb::FileAttributes(FileAttribute::kReadOnly));
  EXPECT_EQ(DiskTool::ParseAttributes("H").value_or(both),
            gb::FileAttributes(FileAttribute::kHidden));
  EXPECT_EQ(DiskTool::ParseAttributes("rh").value_or(gb::FileAttributes()),
            both);
  EXPECT_EQ(DiskTool::ParseAttributes("hRr").value_or(gb::FileAttributes()),
            both);
}

TEST(DiskToolAttributesTest, OtherLettersFail) {
  for (std::string_view letters : {"x", "rx", " r", "-", "a"}) {
    absl::StatusOr<gb::FileAttributes> attributes =
        DiskTool::ParseAttributes(letters);
    ASSERT_FALSE(attributes.ok()) << letters;
    EXPECT_EQ(attributes.status().message(),
              absl::StrCat("Attributes \"", letters,
                           "\" can only hold r (read-only) and h (hidden)"));
  }
}

//==============================================================================
// Labels
//==============================================================================

TEST(DiskToolLabelTest, Labels) {
  EXPECT_EQ(DiskTool::ParseLabel("work").value_or(DiskLabel()).ToString(),
            "WORK");
  EXPECT_EQ(DiskTool::ParseLabel("").value_or(*DiskLabel::Parse("X")),
            DiskLabel());
}

TEST(DiskToolLabelTest, BadLabelsFail) {
  for (std::string_view text : {"has space", "A.B", "SEVENTEEN_LETTERS"}) {
    absl::StatusOr<DiskLabel> label = DiskTool::ParseLabel(text);
    ASSERT_FALSE(label.ok()) << text;
    EXPECT_EQ(label.status().code(), absl::StatusCode::kInvalidArgument);
    EXPECT_EQ(
        label.status().message(),
        absl::StrCat("Label \"", text, "\" isn't up to ", DiskLabel::kMaxLength,
                     " letters, digits, and ! # $ % & ' ( ) - @ ^ _ ` "
                     "{ } ~"));
  }
}

//==============================================================================
// Dir
//==============================================================================

TEST_F(DiskToolTest, DirListsMatches) {
  NewFormatted();
  PutFile("HELLO.TXT", "hello");
  PutFile("PROGRAM.OZ3", std::string(1000, 'x'));
  PutFile("README", "");
  PutFile("ABCDEFGHIJKL.TEXT", "1234");
  SetDiskAttributes("HELLO.TXT", gb::FileAttribute::kReadOnly);

  EXPECT_EQ(Text(tool_->Dir(kImagePath, "", false)),
            "HELLO.TXT                3  r-\n"
            "PROGRAM.OZ3            500  --\n"
            "README                   0  --\n"
            "ABCDEFGHIJKL.TEXT        2  --\n");
  EXPECT_EQ(Text(tool_->Dir(kImagePath, "*", false)),
            Text(tool_->Dir(kImagePath, "", false)));
  EXPECT_EQ(Text(tool_->Dir(kImagePath, "*.TXT", false)),
            "HELLO.TXT                3  r-\n");
  EXPECT_EQ(Text(tool_->Dir(kImagePath, "*.t*", false)),
            "HELLO.TXT                3  r-\n"
            "ABCDEFGHIJKL.TEXT        2  --\n");
  EXPECT_EQ(Text(tool_->Dir(kImagePath, "readme", false)),
            "README                   0  --\n");
  EXPECT_EQ(Text(tool_->Dir(kImagePath, "*.BIN", false)), "no files\n");
}

TEST_F(DiskToolTest, DirShowsHiddenFilesOnlyWithAll) {
  NewFormatted();
  PutFile("SHOWN", "a");
  PutFile("HIDDEN", "b");
  SetDiskAttributes("HIDDEN", gb::FileAttribute::kHidden);
  EXPECT_EQ(Text(tool_->Dir(kImagePath, "", false)),
            "SHOWN                    1  --\n");
  EXPECT_EQ(Text(tool_->Dir(kImagePath, "", true)),
            "SHOWN                    1  --\n"
            "HIDDEN                   1  -h\n");
  EXPECT_EQ(Text(tool_->Dir(kImagePath, "HIDDEN", false)), "no files\n");
}

TEST_F(DiskToolTest, DirOfAnEmptyDisk) {
  NewFormatted();
  EXPECT_EQ(Text(tool_->Dir(kImagePath, "", true)), "no files\n");
}

TEST_F(DiskToolTest, FileCommandsNeedAFileSystem) {
  ASSERT_TRUE(tool_->New(kImagePath, kSectorCount).ok());
  constexpr std::string_view kName = "HELLO.TXT";
  WriteHost("mem:/hello.txt", "hi");
  const std::string unformatted = "mem:/work.ozd: unformatted";
  EXPECT_EQ(tool_->Dir(kImagePath, kName, true).status().message(),
            unformatted);
  EXPECT_EQ(tool_->Put(kImagePath, "mem:/hello.txt", kName, false).message(),
            unformatted);
  EXPECT_EQ(tool_->Get(kImagePath, kName, "mem:/out.txt", false).message(),
            unformatted);
  EXPECT_EQ(tool_->Delete(kImagePath, kName).message(), unformatted);
  EXPECT_EQ(tool_->Rename(kImagePath, kName, kName).message(), unformatted);
  EXPECT_EQ(tool_->Attrib(kImagePath, kName, {}, {}).status().message(),
            unformatted);

  // A root whose chain loops can't be opened.
  ASSERT_TRUE(tool_->Format(kImagePath, DiskLabel(), 0).ok());
  SetFatWord(1, 1);
  const std::string corrupt = "mem:/work.ozd: corrupt";
  EXPECT_EQ(tool_->Dir(kImagePath, kName, true).status().message(), corrupt);
  EXPECT_EQ(tool_->Put(kImagePath, "mem:/hello.txt", kName, false).message(),
            corrupt);
  EXPECT_EQ(tool_->Get(kImagePath, kName, "mem:/out.txt", false).message(),
            corrupt);
  EXPECT_EQ(tool_->Delete(kImagePath, kName).message(), corrupt);
  EXPECT_EQ(tool_->Rename(kImagePath, kName, kName).message(), corrupt);
  EXPECT_EQ(tool_->Attrib(kImagePath, kName, {}, {}).status().message(),
            corrupt);
}

TEST_F(DiskToolTest, FileCommandsOnACorruptFileFail) {
  NewFormatted();
  AddCorruptFile("BAD");
  WriteHost("mem:/in.txt", "hi");
  absl::Status status = tool_->Put(kImagePath, "mem:/in.txt", "BAD", false);
  EXPECT_EQ(status.code(), absl::StatusCode::kUnknown);
  EXPECT_EQ(status.message(), "mem:/work.ozd: BAD: can't be written");
  EXPECT_EQ(tool_->Get(kImagePath, "BAD", "mem:/out.txt", false).message(),
            "mem:/work.ozd: BAD: can't be read");
  EXPECT_EQ(tool_->Delete(kImagePath, "BAD").message(),
            "mem:/work.ozd: BAD: can't be deleted");
  EXPECT_EQ(AllNames(), "BAD");
}

//==============================================================================
// Put and Get
//==============================================================================

TEST_F(DiskToolTest, PutAndGetBinary) {
  NewFormatted();
  const std::string bytes("\x34\x12\0\xFF\x7F", 5);
  WriteHost("mem:/data.bin", bytes);
  ASSERT_TRUE(tool_->Put(kImagePath, "mem:/data.bin", "DATA.BIN", false).ok());
  EXPECT_EQ(Text(tool_->Dir(kImagePath, "DATA.BIN", false)),
            "DATA.BIN                 3  --\n");

  // The odd last byte is padded with a zero.
  ASSERT_TRUE(tool_->Get(kImagePath, "data.bin", "mem:/out.bin", false).ok());
  EXPECT_EQ(ReadHost("mem:/out.bin"), bytes + std::string(1, '\0'));
  EXPECT_TRUE(CheckImage() == DiskCheckReport());
}

TEST_F(DiskToolTest, PutAndGetText) {
  NewFormatted();
  WriteHost("mem:/in.txt", "one\r\ntwo\rsix!\n");
  ASSERT_TRUE(tool_->Put(kImagePath, "mem:/in.txt", "TEXT.TXT", true).ok());

  // Binary shows the words: LF line endings, and the zero padding.
  EXPECT_EQ(GetFile("TEXT.TXT"), std::string("one\ntwo\nsix!\n\0", 14));
  ASSERT_TRUE(tool_->Get(kImagePath, "TEXT.TXT", "mem:/out.txt", true).ok());
  EXPECT_EQ(ReadHost("mem:/out.txt"), "one\ntwo\nsix!\n");
  EXPECT_TRUE(CheckImage() == DiskCheckReport());
}

TEST_F(DiskToolTest, PutAnEmptyFile) {
  NewFormatted();
  PutFile("EMPTY", "");
  EXPECT_EQ(GetFile("EMPTY"), "");
}

TEST_F(DiskToolTest, PutWithoutANameUsesTheHostFilesName) {
  NewFormatted();
  ASSERT_TRUE(file_system_.CreateFolder("mem:/src"));
  WriteHost("mem:/src/hello.txt", "hi");
  ASSERT_TRUE(tool_->Put(kImagePath, "mem:/src/hello.txt", "", false).ok());
  EXPECT_EQ(GetFile("HELLO.TXT"), "hi");

  // As Windows writes paths.
  WriteHost("mem:/src/other.txt", "ho");
  ASSERT_TRUE(tool_->Put(kImagePath, "mem:\\src\\other.txt", "", false).ok());
  EXPECT_EQ(GetFile("OTHER.TXT"), "ho");
}

TEST_F(DiskToolTest, PutFailsForAHostNameThatIsntADiskName) {
  NewFormatted();
  for (std::string_view name :
       {"a_very_long_name.txt", "hello.text1", "a.b.c", "bad name"}) {
    const std::string path = absl::StrCat("mem:/", name);
    WriteHost(path, "hi");
    absl::Status status = tool_->Put(kImagePath, path, "", false);
    EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument) << name;
    EXPECT_EQ(
        status.message(),
        absl::StrCat(path, ": \"", name,
                     "\" isn't a valid disk name, so it needs one given"));
  }
  EXPECT_EQ(AllNames(), "");
}

TEST_F(DiskToolTest, PutFailsForANameThatIsntADiskName) {
  NewFormatted();
  WriteHost("mem:/in.txt", "hi");
  for (std::string_view name :
       {"a_very_long_name.txt", "hello.text1", "bad name", "*.TXT"}) {
    absl::Status status = tool_->Put(kImagePath, "mem:/in.txt", name, false);
    EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument) << name;
    EXPECT_EQ(status.message(), absl::StrCat(name, ": bad name"));
  }
  EXPECT_EQ(AllNames(), "");
}

TEST_F(DiskToolTest, PutFailsWithNoHostFile) {
  NewFormatted();
  absl::Status status = tool_->Put(kImagePath, "mem:/missing.txt", "", false);
  EXPECT_EQ(status.code(), absl::StatusCode::kNotFound);
  EXPECT_EQ(status.message(), "mem:/missing.txt: not found");

  // Before the name it would be put as, which this one couldn't be.
  status = tool_->Put(kImagePath, "mem:/a_missing_long_name.txt", "", false);
  EXPECT_EQ(status.code(), absl::StatusCode::kNotFound);

  ASSERT_TRUE(file_system_.CreateFolder("mem:/folder"));
  status = tool_->Put(kImagePath, "mem:/folder", "FOLDER", false);
  EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument);
  EXPECT_EQ(status.message(), "mem:/folder: not a file");
  EXPECT_EQ(AllNames(), "");
}

TEST_F(DiskToolTest, PutReplacesAFile) {
  NewFormatted();
  PutFile("HELLO.TXT", std::string(2000, 'a'));
  PutFile("HELLO.TXT", "short");
  EXPECT_EQ(GetFile("HELLO.TXT"), std::string("short\0", 6));
  EXPECT_EQ(AllNames(), "HELLO.TXT");
  EXPECT_TRUE(CheckImage() == DiskCheckReport());
}

TEST_F(DiskToolTest, PutKeepsAReadOnlyFile) {
  NewFormatted();
  PutFile("HELLO.TXT", "old");
  SetDiskAttributes("HELLO.TXT", gb::FileAttribute::kReadOnly);
  WriteHost("mem:/new.txt", "new");
  absl::Status status =
      tool_->Put(kImagePath, "mem:/new.txt", "HELLO.TXT", false);
  EXPECT_EQ(status.code(), absl::StatusCode::kUnknown);
  EXPECT_EQ(status.message(), "mem:/work.ozd: HELLO.TXT: can't be written");
  EXPECT_EQ(GetFile("HELLO.TXT"), std::string("old\0", 4));
}

TEST_F(DiskToolTest, PutFailsOnAWriteProtectedDisk) {
  NewFormatted();
  SetProtected(true);
  WriteHost("mem:/new.txt", "new");
  EXPECT_EQ(tool_->Put(kImagePath, "mem:/new.txt", "NEW.TXT", false).message(),
            "mem:/work.ozd: NEW.TXT: can't be written");
  SetProtected(false);
  EXPECT_EQ(AllNames(), "");
}

TEST_F(DiskToolTest, PutFailsOnAFullDiskLeavingPartOfTheFile) {
  constexpr char kSmallPath[] = "mem:/small.ozd";
  ASSERT_TRUE(tool_->New(kSmallPath, 16).ok());
  ASSERT_TRUE(tool_->Format(kSmallPath, DiskLabel(), 1).ok());
  WriteHost("mem:/big.bin", std::string(16 * kDiskSectorWords * 2, 'x'));
  absl::Status status = tool_->Put(kSmallPath, "mem:/big.bin", "BIG", false);
  EXPECT_EQ(status.code(), absl::StatusCode::kUnknown);
  EXPECT_EQ(status.message(), "mem:/small.ozd: BIG: can't be written");
  EXPECT_EQ(Text(tool_->Dir(kSmallPath, "", true)),
            "BIG                   3328  --\n");
}

TEST_F(DiskToolTest, PutAndGetKeepAttributes) {
  NewFormatted();
  WriteHost("mem:/in.txt", "hi");
  ASSERT_TRUE(file_system_.SetAttributes(
      "mem:/in.txt",
      {gb::FileAttribute::kReadOnly, gb::FileAttribute::kHidden}));
  ASSERT_TRUE(tool_->Put(kImagePath, "mem:/in.txt", "IN.TXT", true).ok());
  EXPECT_EQ(Text(tool_->Attrib(kImagePath, "IN.TXT", {}, {})), "IN.TXT rh\n");

  for (bool text : {false, true}) {
    const std::string path = absl::StrCat("mem:/out", text, ".txt");
    ASSERT_TRUE(tool_->Get(kImagePath, "IN.TXT", path, text).ok());
    EXPECT_EQ(file_system_.GetPathInfo(path).attributes,
              gb::FileAttributes(
                  {gb::FileAttribute::kReadOnly, gb::FileAttribute::kHidden}))
        << path;
  }
}

TEST_F(DiskToolTest, GetFailsWithNoFile) {
  NewFormatted();
  PutFile("A.TXT", "a");
  for (std::string_view name :
       {"NOPE", "*.TXT", "bad name", "./A.TXT", "/A.TXT", "X/../A.TXT"}) {
    for (std::string_view host_path : {"mem:/out.bin", ""}) {
      absl::Status status = tool_->Get(kImagePath, name, host_path, false);
      EXPECT_EQ(status.code(), absl::StatusCode::kNotFound) << name;
      EXPECT_EQ(status.message(),
                absl::StrCat("mem:/work.ozd: ", name, ": not found"));
    }
  }
  EXPECT_FALSE(file_system_.IsValidPath("mem:/out.bin"));
}

TEST_F(DiskToolTest, GetWithoutAHostPathUsesTheDisksName) {
  NewFormatted();
  PutFile("HELLO.TXT", "hi");
  ASSERT_TRUE(
      file_system_.Register(std::make_unique<CurrentFolderProtocol>(), "cur"));
  ASSERT_TRUE(file_system_.SetDefaultProtocol("cur"));
  ASSERT_TRUE(file_system_.CreateFolder("cur:/here"));
  absl::Status status = tool_->Get(kImagePath, "hello.txt", "", false);
  ASSERT_TRUE(status.ok()) << status;
  EXPECT_EQ(ReadHost("cur:/here/HELLO.TXT"), "hi");
}

TEST_F(DiskToolTest, GetReplacesAHostFile) {
  NewFormatted();
  PutFile("HELLO.TXT", "new");
  WriteHost("mem:/out.txt", "an older and longer file");
  ASSERT_TRUE(tool_->Get(kImagePath, "HELLO.TXT", "mem:/out.txt", true).ok());
  EXPECT_EQ(ReadHost("mem:/out.txt"), "new");
}

TEST_F(DiskToolTest, GetFailsOntoAReadOnlyHostFile) {
  NewFormatted();
  PutFile("HELLO.TXT", "new");
  WriteHost("mem:/out.txt", "old");
  ASSERT_TRUE(
      file_system_.SetAttributes("mem:/out.txt", gb::FileAttribute::kReadOnly));
  absl::Status status =
      tool_->Get(kImagePath, "HELLO.TXT", "mem:/out.txt", false);
  EXPECT_EQ(status.code(), absl::StatusCode::kUnknown);
  EXPECT_EQ(status.message(), "mem:/out.txt: can't be written");
  EXPECT_EQ(ReadHost("mem:/out.txt"), "old");
}

TEST_F(DiskToolTest, GetFailsWhenItsAttributesCantBeSet) {
  NewFormatted();
  PutFile("HELLO.TXT", "hi");
  SetDiskAttributes("HELLO.TXT", gb::FileAttribute::kHidden);
  ASSERT_TRUE(file_system_.Register(std::make_unique<FixedAttributesProtocol>(),
                                    "fixed"));
  absl::Status status =
      tool_->Get(kImagePath, "HELLO.TXT", "fixed:/out.txt", false);
  EXPECT_EQ(status.code(), absl::StatusCode::kUnknown);
  EXPECT_EQ(status.message(), "fixed:/out.txt: its attributes can't be set");
  EXPECT_EQ(ReadHost("fixed:/out.txt"), "hi");
}

TEST_F(DiskToolTest, GetFailsOntoAHostFolder) {
  NewFormatted();
  PutFile("HELLO.TXT", "new");
  ASSERT_TRUE(file_system_.CreateFolder("mem:/out"));
  EXPECT_EQ(tool_->Get(kImagePath, "HELLO.TXT", "mem:/out", false).message(),
            "mem:/out: can't be written");
}

//==============================================================================
// Delete
//==============================================================================

TEST_F(DiskToolTest, DeleteDeletesEveryMatchHiddenOnesIncluded) {
  NewFormatted();
  PutFile("A.TXT", "a");
  PutFile("B.TXT", "b");
  PutFile("C.OZ3", "c");
  SetDiskAttributes("B.TXT", gb::FileAttribute::kHidden);
  ASSERT_TRUE(tool_->Delete(kImagePath, "*.txt").ok());
  EXPECT_EQ(AllNames(), "C.OZ3");
  ASSERT_TRUE(tool_->Delete(kImagePath, "C.OZ3").ok());
  EXPECT_EQ(AllNames(), "");
  EXPECT_TRUE(CheckImage() == DiskCheckReport());
}

TEST_F(DiskToolTest, DeleteReportsReadOnlyMatchesAndDeletesTheRest) {
  NewFormatted();
  PutFile("A.TXT", "a");
  PutFile("B.TXT", "b");
  PutFile("C.TXT", "c");
  PutFile("D.TXT", "d");
  for (std::string_view name : {"B.TXT", "D.TXT"}) {
    SetDiskAttributes(name, gb::FileAttribute::kReadOnly);
  }
  absl::Status status = tool_->Delete(kImagePath, "*.TXT");
  EXPECT_EQ(status.code(), absl::StatusCode::kUnknown);
  EXPECT_EQ(status.message(),
            "mem:/work.ozd: B.TXT: can't be deleted; D.TXT: can't be deleted");
  EXPECT_EQ(AllNames(), "B.TXT D.TXT");
}

TEST_F(DiskToolTest, DeleteFailsWithNoMatch) {
  NewFormatted();
  PutFile("A.TXT", "a");
  absl::Status status = tool_->Delete(kImagePath, "*.OZ3");
  EXPECT_EQ(status.code(), absl::StatusCode::kNotFound);
  EXPECT_EQ(status.message(), "mem:/work.ozd: *.OZ3: not found");
  EXPECT_EQ(AllNames(), "A.TXT");
}

TEST_F(DiskToolTest, DeleteFailsForAnEmptyPattern) {
  NewFormatted();
  PutFile("A.TXT", "a");
  absl::Status status = tool_->Delete(kImagePath, "");
  EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument);
  EXPECT_EQ(status.message(), "A pattern is needed, such as * for every file");
  EXPECT_EQ(AllNames(), "A.TXT");
}

TEST_F(DiskToolTest, DeleteFailsOnAWriteProtectedDisk) {
  NewFormatted();
  PutFile("A.TXT", "a");
  SetProtected(true);
  EXPECT_EQ(tool_->Delete(kImagePath, "A.TXT").message(),
            "mem:/work.ozd: A.TXT: can't be deleted");
  EXPECT_EQ(AllNames(), "A.TXT");
}

//==============================================================================
// Rename
//==============================================================================

TEST_F(DiskToolTest, RenameRenames) {
  NewFormatted();
  PutFile("OLD.TXT", "hi");
  ASSERT_TRUE(tool_->Rename(kImagePath, "old.txt", "NEW.TXT").ok());
  EXPECT_EQ(AllNames(), "NEW.TXT");
  EXPECT_EQ(GetFile("NEW.TXT"), "hi");

  // Renaming a file to its own name, in any case, leaves it.
  ASSERT_TRUE(tool_->Rename(kImagePath, "NEW.TXT", "NEW.TXT").ok());
  ASSERT_TRUE(tool_->Rename(kImagePath, "NEW.TXT", "new.txt").ok());
  EXPECT_EQ(AllNames(), "NEW.TXT");
  EXPECT_TRUE(CheckImage() == DiskCheckReport());
}

TEST_F(DiskToolTest, RenameOntoATakenNameFails) {
  NewFormatted();
  PutFile("A.TXT", "a");
  PutFile("B.TXT", "b");
  absl::Status status = tool_->Rename(kImagePath, "A.TXT", "B.TXT");
  EXPECT_EQ(status.code(), absl::StatusCode::kAlreadyExists);
  EXPECT_EQ(status.message(), "mem:/work.ozd: B.TXT: exists");
  EXPECT_EQ(GetFile("A.TXT"), std::string("a\0", 2));
  EXPECT_EQ(GetFile("B.TXT"), std::string("b\0", 2));
}

TEST_F(DiskToolTest, RenameToANameThatIsntADiskNameFails) {
  NewFormatted();
  PutFile("A.TXT", "a");
  for (std::string_view name : {"bad name", "*.TXT", "a_very_long_name.txt"}) {
    absl::Status status = tool_->Rename(kImagePath, "A.TXT", name);
    EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument) << name;
    EXPECT_EQ(status.message(),
              absl::StrCat("mem:/work.ozd: ", name, ": bad name"));
  }
  EXPECT_EQ(AllNames(), "A.TXT");
}

TEST_F(DiskToolTest, RenameOfNoFileFails) {
  NewFormatted();
  PutFile("A.TXT", "a");
  for (std::string_view name : {"B.TXT", "*.TXT", "bad name"}) {
    absl::Status status = tool_->Rename(kImagePath, name, "C.TXT");
    EXPECT_EQ(status.code(), absl::StatusCode::kNotFound) << name;
    EXPECT_EQ(status.message(),
              absl::StrCat("mem:/work.ozd: ", name, ": not found"));
  }
  EXPECT_EQ(AllNames(), "A.TXT");
}

//==============================================================================
// Attrib
//==============================================================================

TEST_F(DiskToolTest, AttribSetsClearsAndShows) {
  NewFormatted();
  PutFile("A.TXT", "a");
  constexpr std::string_view kName = "A.TXT";
  EXPECT_EQ(Text(tool_->Attrib(kImagePath, kName, {}, {})), "A.TXT --\n");
  EXPECT_EQ(
      Text(tool_->Attrib(
          kImagePath, kName,
          {gb::FileAttribute::kReadOnly, gb::FileAttribute::kHidden}, {})),
      "A.TXT rh\n");
  EXPECT_EQ(
      Text(tool_->Attrib(kImagePath, kName, {}, gb::FileAttribute::kReadOnly)),
      "A.TXT -h\n");
  EXPECT_EQ(Text(tool_->Attrib(kImagePath, kName, gb::FileAttribute::kReadOnly,
                               gb::FileAttribute::kHidden)),
            "A.TXT r-\n");

  // The name shown is the disk's.
  EXPECT_EQ(Text(tool_->Attrib(kImagePath, "a.txt", {}, {})), "A.TXT r-\n");
  EXPECT_TRUE(CheckImage() == DiskCheckReport());
}

TEST_F(DiskToolTest, AttribCantSetAndClearOneAttribute) {
  NewFormatted();
  PutFile("A.TXT", "a");
  absl::StatusOr<std::string> text =
      tool_->Attrib(kImagePath, "A.TXT", gb::FileAttribute::kHidden,
                    {gb::FileAttribute::kReadOnly, gb::FileAttribute::kHidden});
  ASSERT_FALSE(text.ok());
  EXPECT_EQ(text.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_EQ(text.status().message(),
            "An attribute can't be both set and cleared");
}

TEST_F(DiskToolTest, AttribOfNoFileFails) {
  NewFormatted();
  PutFile("A.TXT", "a");
  for (std::string_view name : {"B.TXT", "*.TXT", "bad name"}) {
    absl::StatusOr<std::string> text = tool_->Attrib(kImagePath, name, {}, {});
    ASSERT_FALSE(text.ok()) << name;
    EXPECT_EQ(text.status().code(), absl::StatusCode::kNotFound);
    EXPECT_EQ(text.status().message(),
              absl::StrCat("mem:/work.ozd: ", name, ": not found"));
  }
}

TEST_F(DiskToolTest, AttribShowsButCantChangeOnAWriteProtectedDisk) {
  NewFormatted();
  PutFile("A.TXT", "a");
  SetProtected(true);
  EXPECT_EQ(Text(tool_->Attrib(kImagePath, "A.TXT", {}, {})), "A.TXT --\n");
  absl::StatusOr<std::string> text =
      tool_->Attrib(kImagePath, "A.TXT", gb::FileAttribute::kHidden, {});
  ASSERT_FALSE(text.ok());
  EXPECT_EQ(text.status().code(), absl::StatusCode::kUnknown);
  EXPECT_EQ(text.status().message(),
            "mem:/work.ozd: A.TXT: its attributes can't be set");
}

//==============================================================================
// Host failures
//==============================================================================

TEST_F(DiskToolTest, HostFailureReadingTheImage) {
  NewFormatted();
  host_->SetLimits({.reads = 1});
  absl::StatusOr<std::string> info = tool_->Info(kImagePath);
  ASSERT_FALSE(info.ok());
  EXPECT_EQ(info.status().message(), "mem:/work.ozd: I/O error");
}

TEST_F(DiskToolTest, HostFailureWritingTheImage) {
  NewFormatted();
  host_->SetLimits({.writes = 0});
  absl::Status status = tool_->Label(kImagePath, DiskLabel());
  EXPECT_EQ(status.code(), absl::StatusCode::kDataLoss);
  EXPECT_EQ(status.message(), "mem:/work.ozd: I/O error");
  host_->SetLimits({});
  EXPECT_THAT(Text(tool_->Info(kImagePath)), HasSubstr("Volume:   WORK\n"));
}

TEST_F(DiskToolTest, GetFailsWhenTheHostFileCantBeWritten) {
  NewFormatted();
  PutFile("HELLO.TXT", "hi");
  host_->SetLimits({.writes = 0});
  absl::Status status =
      tool_->Get(kImagePath, "HELLO.TXT", "mem:/out.txt", false);
  EXPECT_EQ(status.code(), absl::StatusCode::kUnknown);
  EXPECT_EQ(status.message(), "mem:/out.txt: can't be written");
}

TEST_F(DiskToolTest, PutFailsWhenTheImageCantBeWritten) {
  NewFormatted();
  WriteHost("mem:/in.txt", "hi");
  host_->SetLimits({.writes = 0});
  absl::Status status = tool_->Put(kImagePath, "mem:/in.txt", "IN.TXT", false);
  EXPECT_EQ(status.code(), absl::StatusCode::kUnknown);
  EXPECT_EQ(status.message(), "mem:/work.ozd: IN.TXT: can't be written");
}

TEST_F(DiskToolTest, PutFailsWhenItsAttributesCantBeSetKeepingTheFile) {
  WriteHost("mem:/in.txt", "hi");
  ASSERT_TRUE(
      file_system_.SetAttributes("mem:/in.txt", gb::FileAttribute::kHidden));

  // Each try allows the image one more write, on a new image, until the file
  // is written but its attributes aren't.
  for (int writes = 0; writes < 100; ++writes) {
    const std::string image_path = absl::StrCat("mem:/try", writes, ".ozd");
    ASSERT_TRUE(tool_->New(image_path, kSectorCount).ok());
    ASSERT_TRUE(tool_->Format(image_path, DiskLabel(), 0).ok());
    host_->SetLimits({.writes = writes});
    absl::Status status =
        tool_->Put(image_path, "mem:/in.txt", "IN.TXT", false);
    host_->SetLimits({});
    ASSERT_FALSE(status.ok()) << "the attributes were never what failed";
    if (status.message() ==
        absl::StrCat(image_path, ": IN.TXT: its attributes can't be set")) {
      EXPECT_EQ(status.code(), absl::StatusCode::kUnknown);
      EXPECT_EQ(Text(tool_->Dir(image_path, "", true)),
                "IN.TXT                   1  --\n");
      return;
    }
  }
  FAIL() << "the attributes were never what failed";
}

}  // namespace
}  // namespace oz3
