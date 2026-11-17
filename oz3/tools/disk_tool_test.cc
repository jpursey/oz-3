// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/disk_tool.h"

#include <array>
#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <utility>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/time/civil_time.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
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

  // Writes two empty files named `name` as the first entries of the root, which
  // a disk formatted by the tool has in cluster 1.
  void AddDuplicateFiles(std::string_view name) {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&file_system_, kImagePath);
    ASSERT_TRUE(image.ok()) << image.status();
    DiskDirEntry entry;
    entry.SetName(*DiskFilename::Parse(name));
    std::array<uint16_t, kDiskSectorWords> words = {};
    absl::c_copy(entry.GetWords(), words.begin());
    absl::c_copy(entry.GetWords(), words.begin() + DiskDirEntry::kWordCount);
    ASSERT_EQ((*image)->WriteSector(kLayout.GetClusterSector(1), words),
              DiskStatus::kOk);
  }

  // Returns the text `result` holds, failing the test if it holds none.
  static std::string Text(const absl::StatusOr<std::string>& result) {
    EXPECT_TRUE(result.ok()) << result.status();
    return result.ok() ? *result : "";
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
  EXPECT_EQ(DiskTool::ParseSize("256").value_or(-1), 1);
  EXPECT_EQ(DiskTool::ParseSize("512").value_or(-1), 2);
  EXPECT_EQ(DiskTool::ParseSize("720K").value_or(-1), 2880);
  EXPECT_EQ(DiskTool::ParseSize("720k").value_or(-1), 2880);
  EXPECT_EQ(DiskTool::ParseSize("1M").value_or(-1), 4096);
  EXPECT_EQ(DiskTool::ParseSize("1m").value_or(-1), 4096);
  EXPECT_EQ(DiskTool::ParseSize("0001K").value_or(-1), 4);
}

TEST(DiskToolSizeTest, LargestSize) {
  const std::string largest =
      absl::StrCat(int64_t{DiskMedia::kMaxSectorCount} * kDiskSectorWords);
  EXPECT_EQ(DiskTool::ParseSize(largest).value_or(-1),
            DiskMedia::kMaxSectorCount);
  EXPECT_EQ(DiskTool::ParseSize("1024M").value_or(-1), 4194304);
}

TEST(DiskToolSizeTest, SizesThatArentNumbers) {
  for (std::string_view text :
       {"", "K", "M", "-256", "+256", " 256", "256 ", "256KK", "256X", "2.5K",
        "0x100", "99999999999999999999K"}) {
    absl::StatusOr<int> size = DiskTool::ParseSize(text);
    ASSERT_FALSE(size.ok()) << text;
    EXPECT_EQ(size.status().message(),
              absl::StrCat("Size \"", text,
                           "\" isn't a number of words, with an optional K or "
                           "M"));
  }
}

TEST(DiskToolSizeTest, SizesOutOfRange) {
  for (std::string_view text : {"0", "0K", "0M", "1025M", "4194434K"}) {
    absl::StatusOr<int> size = DiskTool::ParseSize(text);
    ASSERT_FALSE(size.ok()) << text;
    EXPECT_EQ(size.status().message(),
              absl::StrCat("Size ", text,
                           " is out of range: an image has 1 to 4194433 "
                           "sectors of 256 words"));
  }
}

TEST(DiskToolSizeTest, SizesThatArentWholeSectors) {
  for (std::string_view text : {"1", "255", "257", "1000"}) {
    absl::StatusOr<int> size = DiskTool::ParseSize(text);
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

}  // namespace
}  // namespace oz3
