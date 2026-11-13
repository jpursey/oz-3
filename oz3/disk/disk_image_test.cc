// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_image.h"

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <memory>
#include <utility>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/faulty_file_protocol.h"

namespace oz3 {
namespace {

using ::testing::Each;
using ::testing::Eq;
using ::testing::HasSubstr;

constexpr char kPath[] = "mem:/disk.img";

// The bytes of the header and of a sector in the host file.
constexpr int64_t kHeaderBytes = 16;
constexpr int64_t kSectorBytes = 512;

// A sector of words.
using Sector = std::vector<uint16_t>;

// Returns a sector whose words are `first`, `first` + 1, and so on.
Sector MakeSector(uint16_t first) {
  Sector sector(kDiskSectorWords);
  for (int i = 0; i < kDiskSectorWords; ++i) {
    sector[i] = static_cast<uint16_t>(first + i);
  }
  return sector;
}

// Returns `words` as the bytes of a host file, low byte first.
std::vector<std::byte> GetBytes(std::initializer_list<uint16_t> words) {
  std::vector<std::byte> bytes;
  for (uint16_t word : words) {
    bytes.push_back(static_cast<std::byte>(word & 0xFF));
    bytes.push_back(static_cast<std::byte>(word >> 8));
  }
  return bytes;
}

class DiskImageTest : public ::testing::Test {
 protected:
  DiskImageTest() {
    std::unique_ptr<FaultyFileProtocol> protocol =
        std::make_unique<FaultyFileProtocol>();
    protocol_ = protocol.get();
    file_system_.Register(std::move(protocol));
  }

  // Writes a host file at kPath holding `header` and then `sector_count`
  // zero sectors.
  void WriteImageFile(std::initializer_list<uint16_t> header,
                      int sector_count) {
    std::vector<std::byte> bytes = GetBytes(header);
    bytes.resize(bytes.size() + sector_count * kSectorBytes);
    ASSERT_TRUE(file_system_.WriteFile(kPath, bytes));
  }

  // Returns the bytes of the host file at kPath, which no image may hold open.
  std::vector<std::byte> ReadImageFile() {
    std::vector<std::byte> bytes;
    EXPECT_TRUE(file_system_.ReadFile(kPath, &bytes));
    return bytes;
  }

  gb::FileSystem file_system_;
  FaultyFileProtocol* protocol_ = nullptr;  // Owned by file_system_.
};

TEST_F(DiskImageTest, CreateWritesTheHeaderAndZeroSectors) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 3);
  ASSERT_TRUE(image.ok()) << image.status();
  EXPECT_EQ((*image)->GetSectorCount(), 3);
  EXPECT_EQ((*image)->GetFlags(), 0);
  EXPECT_FALSE((*image)->IsWriteProtected());
  image->reset();

  const std::vector<std::byte> bytes = ReadImageFile();
  ASSERT_EQ(bytes.size(), kHeaderBytes + 3 * kSectorBytes);
  EXPECT_EQ(std::vector<std::byte>(bytes.begin(), bytes.begin() + kHeaderBytes),
            GetBytes({0x5A4F, 0x4B44, 1, 256, 3, 0, 0, 0}));
  EXPECT_THAT(absl::MakeConstSpan(bytes).subspan(kHeaderBytes),
              Each(Eq(std::byte{0})));
}

TEST_F(DiskImageTest, CreateReplacesAnExistingFile) {
  WriteImageFile({1, 2, 3}, 4);
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 1);
  ASSERT_TRUE(image.ok()) << image.status();
  image->reset();
  EXPECT_EQ(ReadImageFile().size(), kHeaderBytes + kSectorBytes);
}

TEST_F(DiskImageTest, CreateFailsForBadSectorCounts) {
  for (int sector_count : {-1, 0, DiskImage::kMaxSectorCount + 1}) {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&file_system_, kPath, sector_count);
    EXPECT_EQ(image.status().code(), absl::StatusCode::kInvalidArgument)
        << sector_count;
  }
  EXPECT_FALSE(file_system_.IsValidFile(kPath));
}

TEST_F(DiskImageTest, CreateFailsForAnOpenFile) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 1);
  ASSERT_TRUE(image.ok()) << image.status();
  absl::StatusOr<std::unique_ptr<DiskImage>> other =
      DiskImage::Create(&file_system_, kPath, 1);
  EXPECT_EQ(other.status().code(), absl::StatusCode::kFailedPrecondition);
}

TEST_F(DiskImageTest, OpenReadsWhatWasWritten) {
  {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&file_system_, kPath, 4);
    ASSERT_TRUE(image.ok()) << image.status();
    EXPECT_EQ((*image)->WriteSector(2, MakeSector(100)), DiskStatus::kOk);
  }

  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  ASSERT_TRUE(image.ok()) << image.status();
  EXPECT_EQ((*image)->GetSectorCount(), 4);
  EXPECT_EQ((*image)->GetFlags(), 0);
  Sector sector(kDiskSectorWords);
  EXPECT_EQ((*image)->ReadSector(2, absl::MakeSpan(sector)), DiskStatus::kOk);
  EXPECT_EQ(sector, MakeSector(100));
  EXPECT_EQ((*image)->ReadSector(1, absl::MakeSpan(sector)), DiskStatus::kOk);
  EXPECT_EQ(sector, Sector(kDiskSectorWords, 0));
}

TEST_F(DiskImageTest, SectorCountIsADword) {
  const int sector_count = 0x10001;
  {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&file_system_, kPath, sector_count);
    ASSERT_TRUE(image.ok()) << image.status();
    EXPECT_EQ((*image)->WriteSector(sector_count - 1, MakeSector(7)),
              DiskStatus::kOk);
  }

  const std::vector<std::byte> bytes = ReadImageFile();
  ASSERT_EQ(bytes.size(), kHeaderBytes + sector_count * kSectorBytes);
  EXPECT_EQ(std::vector<std::byte>(bytes.begin() + 8, bytes.begin() + 12),
            GetBytes({1, 1}));

  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  ASSERT_TRUE(image.ok()) << image.status();
  EXPECT_EQ((*image)->GetSectorCount(), sector_count);
  Sector sector(kDiskSectorWords);
  EXPECT_EQ((*image)->ReadSector(sector_count - 1, absl::MakeSpan(sector)),
            DiskStatus::kOk);
  EXPECT_EQ(sector, MakeSector(7));
}

TEST_F(DiskImageTest, SectorWordsAreStoredLowByteFirst) {
  {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&file_system_, kPath, 2);
    ASSERT_TRUE(image.ok()) << image.status();
    Sector sector(kDiskSectorWords, 0);
    sector.front() = 0x1234;
    sector.back() = 0xABCD;
    EXPECT_EQ((*image)->WriteSector(1, sector), DiskStatus::kOk);
  }

  const std::vector<std::byte> bytes = ReadImageFile();
  ASSERT_EQ(bytes.size(), kHeaderBytes + 2 * kSectorBytes);
  const int64_t start = kHeaderBytes + kSectorBytes;
  EXPECT_EQ(bytes[start], std::byte{0x34});
  EXPECT_EQ(bytes[start + 1], std::byte{0x12});
  EXPECT_EQ(bytes[start + kSectorBytes - 2], std::byte{0xCD});
  EXPECT_EQ(bytes[start + kSectorBytes - 1], std::byte{0xAB});
}

TEST_F(DiskImageTest, ReadsAndWritesTheFirstAndLastSectors) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 5);
  ASSERT_TRUE(image.ok()) << image.status();
  EXPECT_EQ((*image)->WriteSector(0, MakeSector(1000)), DiskStatus::kOk);
  EXPECT_EQ((*image)->WriteSector(4, MakeSector(5000)), DiskStatus::kOk);
  Sector sector(kDiskSectorWords);
  EXPECT_EQ((*image)->ReadSector(0, absl::MakeSpan(sector)), DiskStatus::kOk);
  EXPECT_EQ(sector, MakeSector(1000));
  EXPECT_EQ((*image)->ReadSector(4, absl::MakeSpan(sector)), DiskStatus::kOk);
  EXPECT_EQ(sector, MakeSector(5000));
}

TEST_F(DiskImageTest, RejectsSectorsOutOfRange) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 5);
  ASSERT_TRUE(image.ok()) << image.status();
  Sector sector(kDiskSectorWords);
  for (int index : {-1, 5}) {
    EXPECT_EQ((*image)->ReadSector(index, absl::MakeSpan(sector)),
              DiskStatus::kBadArgument)
        << index;
    EXPECT_EQ((*image)->WriteSector(index, sector), DiskStatus::kBadArgument)
        << index;
  }
  image->reset();
  EXPECT_EQ(ReadImageFile().size(), kHeaderBytes + 5 * kSectorBytes);
}

TEST_F(DiskImageTest, RejectsBuffersThatArentASector) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 1);
  ASSERT_TRUE(image.ok()) << image.status();
  for (int size : {kDiskSectorWords - 1, kDiskSectorWords + 1}) {
    Sector sector(size);
    EXPECT_EQ((*image)->ReadSector(0, absl::MakeSpan(sector)),
              DiskStatus::kBadArgument)
        << size;
    EXPECT_EQ((*image)->WriteSector(0, sector), DiskStatus::kBadArgument)
        << size;
  }
}

TEST_F(DiskImageTest, OpenFailsForAMissingFile) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  EXPECT_EQ(image.status().code(), absl::StatusCode::kNotFound);
  EXPECT_THAT(image.status().message(), HasSubstr("doesn't exist"));
}

TEST_F(DiskImageTest, OpenFailsForAFileAlreadyOpen) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 1);
  ASSERT_TRUE(image.ok()) << image.status();
  absl::StatusOr<std::unique_ptr<DiskImage>> other =
      DiskImage::Open(&file_system_, kPath);
  EXPECT_EQ(other.status().code(), absl::StatusCode::kFailedPrecondition);
  EXPECT_THAT(other.status().message(),
              HasSubstr("can't be opened for reading and writing"));
}

TEST_F(DiskImageTest, OpenFailsForAFileTooShortForAHeader) {
  ASSERT_TRUE(file_system_.WriteFile(kPath, GetBytes({0x5A4F, 0x4B44, 1})));
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  EXPECT_EQ(image.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(image.status().message(), HasSubstr("isn't a disk image"));
}

TEST_F(DiskImageTest, OpenFailsForTheWrongId) {
  for (std::initializer_list<uint16_t> header :
       {std::initializer_list<uint16_t>{0x5A4E, 0x4B44, 1, 256, 1, 0, 0, 0},
        std::initializer_list<uint16_t>{0x5A4F, 0x4B45, 1, 256, 1, 0, 0, 0}}) {
    WriteImageFile(header, 1);
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&file_system_, kPath);
    EXPECT_EQ(image.status().code(), absl::StatusCode::kInvalidArgument);
    EXPECT_THAT(image.status().message(), HasSubstr("isn't a disk image"));
  }
}

TEST_F(DiskImageTest, OpenFailsForAnotherVersion) {
  WriteImageFile({0x5A4F, 0x4B44, 2, 256, 1, 0, 0, 0}, 1);
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  EXPECT_EQ(image.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(image.status().message(), HasSubstr("version 2"));
}

TEST_F(DiskImageTest, OpenFailsForAnotherSectorSize) {
  WriteImageFile({0x5A4F, 0x4B44, 1, 128, 2, 0, 0, 0}, 1);
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  EXPECT_EQ(image.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(image.status().message(), HasSubstr("sectors of 128 words"));
}

TEST_F(DiskImageTest, OpenFailsForNoSectors) {
  WriteImageFile({0x5A4F, 0x4B44, 1, 256, 0, 0, 0, 0}, 0);
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  EXPECT_EQ(image.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(image.status().message(), HasSubstr("has no sectors"));
}

TEST_F(DiskImageTest, OpenFailsForMoreSectorsThanTheMost) {
  // One more than the most, 4,194,434 = 00400082h.
  static_assert(DiskImage::kMaxSectorCount + 1 == 0x0040'0082);
  WriteImageFile({0x5A4F, 0x4B44, 1, 256, 0x0082, 0x0040, 0, 0}, 0);
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  EXPECT_EQ(image.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(image.status().message(), HasSubstr("has 4194434 sectors"));
}

TEST_F(DiskImageTest, OpenFailsForAFileTooShortForItsSectors) {
  std::vector<std::byte> bytes = GetBytes({0x5A4F, 0x4B44, 1, 256, 2, 0, 0, 0});
  bytes.resize(kHeaderBytes + 2 * kSectorBytes - 1);
  ASSERT_TRUE(file_system_.WriteFile(kPath, bytes));
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  EXPECT_EQ(image.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(image.status().message(), HasSubstr("too short for its 2"));
}

TEST_F(DiskImageTest, OpenReadsTheSectorCountHighWord) {
  // Too short, so the message shows the count without a huge file.
  WriteImageFile({0x5A4F, 0x4B44, 1, 256, 1, 1, 0, 0}, 1);
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  EXPECT_THAT(image.status().message(), HasSubstr("its 65537 sectors"));
}

TEST_F(DiskImageTest, OpenAllowsBytesPastTheLastSector) {
  std::vector<std::byte> bytes = GetBytes({0x5A4F, 0x4B44, 1, 256, 1, 0, 0, 0});
  bytes.resize(kHeaderBytes + kSectorBytes + 3);
  ASSERT_TRUE(file_system_.WriteFile(kPath, bytes));
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  ASSERT_TRUE(image.ok()) << image.status();
  EXPECT_EQ((*image)->GetSectorCount(), 1);
}

TEST_F(DiskImageTest, OpenReadsTheFlags) {
  WriteImageFile({0x5A4F, 0x4B44, 1, 256, 1, 0, 0x8001, 0}, 1);
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  ASSERT_TRUE(image.ok()) << image.status();
  EXPECT_EQ((*image)->GetFlags(), 0x8001);
  EXPECT_TRUE((*image)->IsWriteProtected());
}

TEST_F(DiskImageTest, WriteProtectionRefusesWrites) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 2);
  ASSERT_TRUE(image.ok()) << image.status();
  EXPECT_EQ((*image)->WriteSector(1, MakeSector(10)), DiskStatus::kOk);
  EXPECT_EQ((*image)->SetWriteProtected(true), DiskStatus::kOk);
  EXPECT_TRUE((*image)->IsWriteProtected());
  EXPECT_EQ((*image)->GetFlags(), DiskImage::kWriteProtectedFlag);

  EXPECT_EQ((*image)->WriteSector(1, MakeSector(20)),
            DiskStatus::kWriteProtected);
  Sector sector(kDiskSectorWords);
  EXPECT_EQ((*image)->ReadSector(1, absl::MakeSpan(sector)), DiskStatus::kOk);
  EXPECT_EQ(sector, MakeSector(10));

  EXPECT_EQ((*image)->SetWriteProtected(false), DiskStatus::kOk);
  EXPECT_FALSE((*image)->IsWriteProtected());
  EXPECT_EQ((*image)->WriteSector(1, MakeSector(20)), DiskStatus::kOk);
  EXPECT_EQ((*image)->ReadSector(1, absl::MakeSpan(sector)), DiskStatus::kOk);
  EXPECT_EQ(sector, MakeSector(20));
}

TEST_F(DiskImageTest, WriteProtectionIsStoredInTheHeader) {
  {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&file_system_, kPath, 1);
    ASSERT_TRUE(image.ok()) << image.status();
    EXPECT_EQ((*image)->SetWriteProtected(true), DiskStatus::kOk);
  }
  std::vector<std::byte> bytes = ReadImageFile();
  ASSERT_EQ(bytes.size(), kHeaderBytes + kSectorBytes);
  EXPECT_EQ(std::vector<std::byte>(bytes.begin(), bytes.begin() + kHeaderBytes),
            GetBytes({0x5A4F, 0x4B44, 1, 256, 1, 0, 1, 0}));

  {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&file_system_, kPath);
    ASSERT_TRUE(image.ok()) << image.status();
    EXPECT_TRUE((*image)->IsWriteProtected());
    EXPECT_EQ((*image)->SetWriteProtected(false), DiskStatus::kOk);
  }
  bytes = ReadImageFile();
  ASSERT_EQ(bytes.size(), kHeaderBytes + kSectorBytes);
  EXPECT_EQ(std::vector<std::byte>(bytes.begin(), bytes.begin() + kHeaderBytes),
            GetBytes({0x5A4F, 0x4B44, 1, 256, 1, 0, 0, 0}));
}

TEST_F(DiskImageTest, WriteProtectionKeepsReservedFlags) {
  WriteImageFile({0x5A4F, 0x4B44, 1, 256, 1, 0, 0x8000, 0}, 1);
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  ASSERT_TRUE(image.ok()) << image.status();
  EXPECT_EQ((*image)->SetWriteProtected(true), DiskStatus::kOk);
  EXPECT_EQ((*image)->GetFlags(), 0x8001);
  EXPECT_EQ((*image)->SetWriteProtected(false), DiskStatus::kOk);
  EXPECT_EQ((*image)->GetFlags(), 0x8000);
}

TEST_F(DiskImageTest, CreateFailsWhenTheHostWriteFails) {
  // The header, and then two of the four writes of zero sectors.
  protocol_->SetLimits({.writes = 3});
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 200);
  EXPECT_EQ(image.status().code(), absl::StatusCode::kDataLoss);
  EXPECT_THAT(image.status().message(), HasSubstr("couldn't be written"));
}

TEST_F(DiskImageTest, OpenFailsWhenTheHostReadFails) {
  WriteImageFile({0x5A4F, 0x4B44, 1, 256, 1, 0, 0, 0}, 1);
  protocol_->SetLimits({.reads = 0});
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  EXPECT_EQ(image.status().code(), absl::StatusCode::kDataLoss);
  EXPECT_THAT(image.status().message(), HasSubstr("couldn't be read"));
}

TEST_F(DiskImageTest, HostReadFailuresAreIoErrors) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 1);
  ASSERT_TRUE(image.ok()) << image.status();
  protocol_->SetLimits({.reads = 1});
  Sector sector(kDiskSectorWords);
  EXPECT_EQ((*image)->ReadSector(0, absl::MakeSpan(sector)), DiskStatus::kOk);
  EXPECT_EQ((*image)->ReadSector(0, absl::MakeSpan(sector)),
            DiskStatus::kIoError);
}

TEST_F(DiskImageTest, HostWriteFailuresAreIoErrors) {
  {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&file_system_, kPath, 2);
    ASSERT_TRUE(image.ok()) << image.status();
    protocol_->SetLimits({.writes = 1});
    EXPECT_EQ((*image)->WriteSector(0, MakeSector(1)), DiskStatus::kOk);
    EXPECT_EQ((*image)->WriteSector(1, MakeSector(2)), DiskStatus::kIoError);
  }
  protocol_->SetLimits({});

  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  ASSERT_TRUE(image.ok()) << image.status();
  Sector sector(kDiskSectorWords);
  EXPECT_EQ((*image)->ReadSector(0, absl::MakeSpan(sector)), DiskStatus::kOk);
  EXPECT_EQ(sector, MakeSector(1));
  EXPECT_EQ((*image)->ReadSector(1, absl::MakeSpan(sector)), DiskStatus::kOk);
  EXPECT_EQ(sector, Sector(kDiskSectorWords, 0));
}

TEST_F(DiskImageTest, HostWriteFailuresLeaveTheWriteProtectBit) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 1);
  ASSERT_TRUE(image.ok()) << image.status();
  protocol_->SetLimits({.writes = 0});
  EXPECT_EQ((*image)->SetWriteProtected(true), DiskStatus::kIoError);
  EXPECT_FALSE((*image)->IsWriteProtected());
  EXPECT_EQ((*image)->GetFlags(), 0);
}

TEST_F(DiskImageTest, EachWriteIsFlushed) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 2);
  ASSERT_TRUE(image.ok()) << image.status();
  protocol_->SetLimits({.flushes = 1});
  EXPECT_EQ((*image)->WriteSector(0, MakeSector(1)), DiskStatus::kOk);
  EXPECT_EQ((*image)->WriteSector(1, MakeSector(2)), DiskStatus::kIoError);
}

TEST_F(DiskImageTest, WriteProtectionIsFlushed) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 1);
  ASSERT_TRUE(image.ok()) << image.status();
  protocol_->SetLimits({.flushes = 0});
  EXPECT_EQ((*image)->SetWriteProtected(true), DiskStatus::kIoError);
  EXPECT_FALSE((*image)->IsWriteProtected());
}

TEST_F(DiskImageTest, EverythingFailsAfterAHostFailure) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Create(&file_system_, kPath, 1);
  ASSERT_TRUE(image.ok()) << image.status();
  protocol_->SetLimits({.flushes = 0});
  Sector sector(kDiskSectorWords, 0);
  EXPECT_EQ((*image)->WriteSector(0, sector), DiskStatus::kIoError);
  protocol_->SetLimits({});

  EXPECT_EQ((*image)->ReadSector(0, absl::MakeSpan(sector)),
            DiskStatus::kIoError);
  EXPECT_EQ((*image)->WriteSector(0, sector), DiskStatus::kIoError);
  EXPECT_EQ((*image)->SetWriteProtected(true), DiskStatus::kIoError);
}

}  // namespace
}  // namespace oz3
