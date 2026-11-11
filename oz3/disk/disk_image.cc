// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_image.h"

#include <algorithm>
#include <array>
#include <bit>
#include <utility>
#include <vector>

#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"

namespace oz3 {

namespace {

// The header, by word.
constexpr int kHeaderWords = 8;
constexpr int kIdWord0 = 0;
constexpr int kIdWord1 = 1;
constexpr int kVersionWord = 2;
constexpr int kSectorSizeWord = 3;
constexpr int kSectorCountWord = 4;  // A dword, the low word first.
constexpr int kFlagsWord = 6;

// "OZDK", packed low byte first.
constexpr uint16_t kImageId0 = 0x5A4F;
constexpr uint16_t kImageId1 = 0x4B44;
constexpr uint16_t kImageVersion = 1;

// Sizes and offsets in the host file, in bytes.
constexpr int64_t kHeaderBytes = kHeaderWords * 2;
constexpr int64_t kSectorBytes = kDiskSectorWords * 2;
constexpr int64_t kFlagsOffset = kFlagsWord * 2;

// The sectors Create writes at a time.
constexpr int64_t kCreateSectors = 64;

// Returns the host file offset of `sector`.
int64_t GetSectorOffset(int64_t sector) {
  return kHeaderBytes + sector * kSectorBytes;
}

// Swaps the bytes of each of `words` between the host's byte order and the
// image's, low byte first. Only a big-endian host changes anything.
void SwapToHostOrder(absl::Span<uint16_t> words) {
  if constexpr (std::endian::native == std::endian::big) {
    for (uint16_t& word : words) {
      word = static_cast<uint16_t>((word << 8) | (word >> 8));
    }
  }
}

}  // namespace

absl::StatusOr<std::unique_ptr<DiskImage>> DiskImage::Create(
    gb::FileSystem* file_system, std::string_view path, int64_t sector_count) {
  if (sector_count < 1 || sector_count > kMaxSectorCount) {
    return absl::InvalidArgumentError(
        absl::StrCat("Disk image ", path, " can't have ", sector_count,
                     " sectors: it must have 1 to ", kMaxSectorCount));
  }
  std::unique_ptr<gb::File> file =
      file_system->OpenFile(path, gb::kNewFileFlags + gb::FileFlag::kRead);
  if (file == nullptr) {
    return absl::FailedPreconditionError(
        absl::StrCat("Disk image ", path, " can't be created"));
  }

  std::array<uint16_t, kHeaderWords> header = {};
  header[kIdWord0] = kImageId0;
  header[kIdWord1] = kImageId1;
  header[kVersionWord] = kImageVersion;
  header[kSectorSizeWord] = kDiskSectorWords;
  header[kSectorCountWord] = static_cast<uint16_t>(sector_count);
  header[kSectorCountWord + 1] = static_cast<uint16_t>(sector_count >> 16);
  SwapToHostOrder(absl::MakeSpan(header));
  bool written = file->Write(header.data(), kHeaderWords) == kHeaderWords;

  const std::vector<uint16_t> zeros(kCreateSectors * kDiskSectorWords, 0);
  for (int64_t sector = 0; written && sector < sector_count;
       sector += kCreateSectors) {
    const int64_t count =
        std::min(kCreateSectors, sector_count - sector) * kDiskSectorWords;
    written = file->Write(zeros.data(), count) == count;
  }
  if (!written || !file->Flush()) {
    return absl::DataLossError(
        absl::StrCat("Disk image ", path, " couldn't be written"));
  }
  return absl::WrapUnique(
      new DiskImage(std::move(file), sector_count, /*flags=*/0));
}

absl::StatusOr<std::unique_ptr<DiskImage>> DiskImage::Open(
    gb::FileSystem* file_system, std::string_view path) {
  if (!file_system->IsValidFile(path)) {
    return absl::NotFoundError(
        absl::StrCat("Disk image ", path, " doesn't exist"));
  }
  std::unique_ptr<gb::File> file =
      file_system->OpenFile(path, gb::kReadWriteFileFlags);
  if (file == nullptr) {
    return absl::FailedPreconditionError(absl::StrCat(
        "Disk image ", path, " can't be opened for reading and writing"));
  }

  std::array<uint16_t, kHeaderWords> header = {};
  const int64_t header_words = file->Read(header.data(), kHeaderWords);
  if (!file->IsValid()) {
    return absl::DataLossError(
        absl::StrCat("Disk image ", path, " couldn't be read"));
  }
  SwapToHostOrder(absl::MakeSpan(header));
  if (header_words < kHeaderWords || header[kIdWord0] != kImageId0 ||
      header[kIdWord1] != kImageId1) {
    return absl::InvalidArgumentError(
        absl::StrCat(path, " isn't a disk image"));
  }
  if (header[kVersionWord] != kImageVersion) {
    return absl::InvalidArgumentError(
        absl::StrCat("Disk image ", path, " has version ", header[kVersionWord],
                     ", but only version ", kImageVersion, " is supported"));
  }
  if (header[kSectorSizeWord] != kDiskSectorWords) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Disk image ", path, " has sectors of ", header[kSectorSizeWord],
        " words, but only ", kDiskSectorWords, " is supported"));
  }
  const int64_t sector_count =
      header[kSectorCountWord] |
      static_cast<int64_t>(header[kSectorCountWord + 1]) << 16;
  if (sector_count == 0) {
    return absl::InvalidArgumentError(
        absl::StrCat("Disk image ", path, " has no sectors"));
  }
  const int64_t size = file->SeekEnd();
  if (size < GetSectorOffset(sector_count)) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Disk image ", path, " is ", size, " bytes, too short for its ",
        sector_count, " sectors (", GetSectorOffset(sector_count), " bytes)"));
  }
  return absl::WrapUnique(
      new DiskImage(std::move(file), sector_count, header[kFlagsWord]));
}

DiskImage::DiskImage(std::unique_ptr<gb::File> file, int64_t sector_count,
                     uint16_t flags)
    : file_(std::move(file)), sector_count_(sector_count), flags_(flags) {}

bool DiskImage::IsValidSector(int64_t sector, int64_t size) const {
  return sector >= 0 && sector < sector_count_ && size == kDiskSectorWords;
}

DiskStatus DiskImage::SetWriteProtected(bool write_protected) {
  const uint16_t flags =
      write_protected ? (flags_ | kWriteProtectedFlag)
                      : static_cast<uint16_t>(flags_ & ~kWriteProtectedFlag);
  uint16_t stored = flags;
  SwapToHostOrder({&stored, 1});
  if (file_->SeekTo(kFlagsOffset) != kFlagsOffset ||
      file_->Write(&stored, 1) != 1 || !file_->Flush()) {
    return DiskStatus::kIoError;
  }
  flags_ = flags;
  return DiskStatus::kOk;
}

DiskStatus DiskImage::ReadSector(int64_t sector, absl::Span<uint16_t> words) {
  if (!IsValidSector(sector, static_cast<int64_t>(words.size()))) {
    return DiskStatus::kBadArgument;
  }
  const int64_t offset = GetSectorOffset(sector);
  if (file_->SeekTo(offset) != offset ||
      file_->Read(words.data(), kDiskSectorWords) != kDiskSectorWords) {
    return DiskStatus::kIoError;
  }
  SwapToHostOrder(words);
  return DiskStatus::kOk;
}

DiskStatus DiskImage::WriteSector(int64_t sector,
                                  absl::Span<const uint16_t> words) {
  if (!IsValidSector(sector, static_cast<int64_t>(words.size()))) {
    return DiskStatus::kBadArgument;
  }
  if (IsWriteProtected()) {
    return DiskStatus::kWriteProtected;
  }
  const int64_t offset = GetSectorOffset(sector);
  if (file_->SeekTo(offset) != offset) {
    return DiskStatus::kIoError;
  }
  int64_t written = 0;
  if constexpr (std::endian::native == std::endian::big) {
    std::array<uint16_t, kDiskSectorWords> stored;
    std::copy(words.begin(), words.end(), stored.begin());
    SwapToHostOrder(absl::MakeSpan(stored));
    written = file_->Write(stored.data(), kDiskSectorWords);
  } else {
    written = file_->Write(words.data(), kDiskSectorWords);
  }
  if (written != kDiskSectorWords || !file_->Flush()) {
    return DiskStatus::kIoError;
  }
  return DiskStatus::kOk;
}

}  // namespace oz3
