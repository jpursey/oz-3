// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_file_system.h"

#include <algorithm>
#include <array>
#include <utility>

#include "absl/log/check.h"
#include "absl/types/span.h"

namespace oz3 {

namespace {

using WriteCaching = DiskHardware::WriteCaching;

// The superblock's fields, by word.
constexpr int kFormatIdWord = 0;  // Two words.
constexpr int kVersionWord = 2;
constexpr int kSectorsPerClusterWord = 3;
constexpr int kClusterCountWord = 4;
constexpr int kFatSectorsWord = 5;
constexpr int kRootClusterWord = 6;
constexpr int kLabelWord = 8;
constexpr int kFormattedWord = 16;

// "OZFS", packed.
constexpr uint16_t kFormatId[] = {'O' | 'Z' << 8, 'F' | 'S' << 8};
constexpr uint16_t kVersion = 1;

// The superblock and the FAT's first sector, which hold where everything else
// is and the free list's head.
constexpr int64_t kSuperblockSector = 0;
constexpr int64_t kFatHeadSector = 1;

// A FAT link ending a chain, or as the free list's head, a full disk.
constexpr uint16_t kFatEnd = 0xFFFF;

// The cluster formatting gives the root directory.
constexpr int kFormatRootCluster = 1;

constexpr std::array<uint16_t, kDiskSectorWords> kZeroSector = {};

}  // namespace

DiskFileSystem::DiskFileSystem(const DiskHardwareSettings& settings,
                               int max_open_files)
    : max_open_files_(max_open_files), hardware_(settings) {
  CHECK_GE(max_open_files, 1);
}

DiskStatus DiskFileSystem::AttachImage(std::unique_ptr<DiskImage> image) {
  CHECK(image != nullptr);
  ResetVolume();
  const int64_t sector_count = image->GetSectorCount();
  hardware_.AttachImage(std::move(image));
  const DiskStatus status =
      hardware_.ReadSector(kSuperblockSector, absl::MakeSpan(superblock_));
  if (status != DiskStatus::kOk) {
    return status;
  }
  if (!IsValidSuperblock(superblock_, sector_count)) {
    return DiskStatus::kUnformatted;
  }
  formatted_ = true;
  return DiskStatus::kOk;
}

std::unique_ptr<DiskImage> DiskFileSystem::DetachImage() {
  ResetVolume();
  return hardware_.DetachImage();
}

absl::Duration DiskFileSystem::CollectBusyTime() {
  return hardware_.CollectBusyTime();
}

DiskStatus DiskFileSystem::Format(const DiskLabel& label,
                                  int sectors_per_cluster, DateTime now) {
  if (DiskStatus status = CheckWritable(); status != DiskStatus::kOk) {
    return status;
  }
  const std::optional<DiskLayout> layout =
      ChooseDiskLayout(GetMedia()->GetSectorCount(), sectors_per_cluster);
  if (!layout.has_value()) {
    return DiskStatus::kBadArgument;
  }
  ResetVolume();

  // The superblock is blanked first and written last, so a format that fails
  // partway leaves the disk unformatted, rather than a file system over a
  // broken FAT.
  if (DiskStatus status = hardware_.WriteSector(kSuperblockSector, kZeroSector,
                                                WriteCaching::kSkip);
      status != DiskStatus::kOk) {
    return status;
  }

  // The root takes the first cluster from a zeroed FAT's free list, which
  // leaves the head at the next cluster, if there is one. ResetVolume zeroed
  // the buffer.
  fat_head_.words[0] = (layout->cluster_count > kFormatRootCluster
                            ? static_cast<uint16_t>(kFormatRootCluster + 1)
                            : kFatEnd);
  fat_head_.words[kFormatRootCluster] = kFatEnd;
  if (DiskStatus status = hardware_.WriteSector(kFatHeadSector, fat_head_.words,
                                                WriteCaching::kSkip);
      status != DiskStatus::kOk) {
    return status;
  }
  for (int64_t sector = kFatHeadSector + 1; sector <= layout->fat_sectors;
       ++sector) {
    if (DiskStatus status =
            hardware_.WriteSector(sector, kZeroSector, WriteCaching::kCache);
        status != DiskStatus::kOk) {
      return status;
    }
  }
  const int64_t root_sector = layout->GetClusterSector(kFormatRootCluster);
  for (int i = 0; i < layout->sectors_per_cluster; ++i) {
    if (DiskStatus status = hardware_.WriteSector(root_sector + i, kZeroSector,
                                                  WriteCaching::kCache);
        status != DiskStatus::kOk) {
      return status;
    }
  }

  std::copy(std::begin(kFormatId), std::end(kFormatId),
            superblock_.begin() + kFormatIdWord);
  superblock_[kVersionWord] = kVersion;
  superblock_[kSectorsPerClusterWord] =
      static_cast<uint16_t>(layout->sectors_per_cluster);
  superblock_[kClusterCountWord] = static_cast<uint16_t>(layout->cluster_count);
  superblock_[kFatSectorsWord] = static_cast<uint16_t>(layout->fat_sectors);
  superblock_[kRootClusterWord] = kFormatRootCluster;
  PutLabel(label, &superblock_);
  const DateTime::Words& formatted = now.GetWords();
  std::copy(formatted.begin(), formatted.end(),
            superblock_.begin() + kFormattedWord);
  if (DiskStatus status = hardware_.WriteSector(kSuperblockSector, superblock_,
                                                WriteCaching::kSkip);
      status != DiskStatus::kOk) {
    return status;
  }

  formatted_ = true;
  free_count_ = layout->cluster_count - 1;
  fat_head_.sector = kFatHeadSector;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::SetLabel(const DiskLabel& label) {
  if (DiskStatus status = CheckWritable(); status != DiskStatus::kOk) {
    return status;
  }
  if (!formatted_) {
    return DiskStatus::kUnformatted;
  }
  // The label was checked when the superblock was attached or written.
  const DiskLabel old_label = *GetLabel(superblock_);
  PutLabel(label, &superblock_);
  if (DiskStatus status = hardware_.WriteSector(kSuperblockSector, superblock_,
                                                WriteCaching::kSkip);
      status != DiskStatus::kOk) {
    PutLabel(old_label, &superblock_);
    return status;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::GetVolumeInfo(DiskVolumeInfo* info) {
  if (!HasImage()) {
    return DiskStatus::kNoDisk;
  }
  if (!formatted_) {
    return DiskStatus::kUnformatted;
  }
  if (DiskStatus status = CountFreeClusters(); status != DiskStatus::kOk) {
    return status;
  }
  const DiskLayout layout = GetLayout(superblock_);
  // The label was checked when the superblock was attached or written.
  *info = {
      .label = *GetLabel(superblock_),
      .sectors_per_cluster = layout.sectors_per_cluster,
      .cluster_count = layout.cluster_count,
      .free_clusters = *free_count_,
  };
  return DiskStatus::kOk;
}

DiskLayout DiskFileSystem::GetLayout(const Sector& superblock) {
  return {
      .sectors_per_cluster = superblock[kSectorsPerClusterWord],
      .cluster_count = superblock[kClusterCountWord],
      .fat_sectors = superblock[kFatSectorsWord],
  };
}

int DiskFileSystem::GetRootCluster(const Sector& superblock) {
  return superblock[kRootClusterWord];
}

std::optional<DiskLabel> DiskFileSystem::GetLabel(const Sector& superblock) {
  return DiskLabel::FromWords(absl::MakeConstSpan(superblock)
                                  .subspan(kLabelWord, DiskLabel::kWordCount));
}

void DiskFileSystem::PutLabel(const DiskLabel& label, Sector* superblock) {
  const DiskLabel::Words label_words = label.GetWords();
  std::copy(label_words.begin(), label_words.end(),
            superblock->begin() + kLabelWord);
}

bool DiskFileSystem::IsValidSuperblock(const Sector& superblock,
                                       int64_t sector_count) {
  if (!std::equal(std::begin(kFormatId), std::end(kFormatId),
                  superblock.begin() + kFormatIdWord) ||
      superblock[kVersionWord] != kVersion) {
    return false;
  }
  const DiskLayout layout = GetLayout(superblock);
  const int root_cluster = GetRootCluster(superblock);
  return layout.IsValid(sector_count) && root_cluster >= 1 &&
         root_cluster <= layout.cluster_count &&
         GetLabel(superblock).has_value();
}

void DiskFileSystem::ResetVolume() {
  formatted_ = false;
  superblock_ = {};
  free_count_.reset();
  fat_head_ = {};
  fat_work_ = {};
}

DiskStatus DiskFileSystem::CheckWritable() const {
  const DiskMedia* media = GetMedia();
  if (media == nullptr) {
    return DiskStatus::kNoDisk;
  }
  if (media->IsWriteProtected()) {
    return DiskStatus::kWriteProtected;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::ReadFatWord(int index, uint16_t* value) {
  CHECK_GE(index, 0);
  CHECK_LE(index, GetLayout(superblock_).cluster_count);
  const int64_t sector = kFatHeadSector + index / kDiskSectorWords;
  SectorBuffer& buffer = (sector == kFatHeadSector ? fat_head_ : fat_work_);
  if (buffer.sector != sector) {
    buffer.sector = -1;
    if (DiskStatus status =
            hardware_.ReadSector(sector, absl::MakeSpan(buffer.words));
        status != DiskStatus::kOk) {
      return status;
    }
    buffer.sector = sector;
  }
  *value = buffer.words[index % kDiskSectorWords];
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::CountFreeClusters() {
  if (free_count_.has_value()) {
    return DiskStatus::kOk;
  }
  // Word 0 is the link of an imaginary cluster 0, to the free list's head. A
  // link of 0 is the next cluster, or the end after the last one.
  const int cluster_count = GetLayout(superblock_).cluster_count;
  int cluster = 0;
  int count = 0;
  while (true) {
    uint16_t link = 0;
    if (DiskStatus status = ReadFatWord(cluster, &link);
        status != DiskStatus::kOk) {
      return status;
    }
    if (link == kFatEnd || (link == 0 && cluster == cluster_count)) {
      break;
    }
    if (link > cluster_count) {
      return DiskStatus::kCorrupt;
    }
    // A list longer than the disk loops.
    if (++count > cluster_count) {
      return DiskStatus::kCorrupt;
    }
    cluster = (link == 0 ? cluster + 1 : link);
  }
  free_count_ = count;
  return DiskStatus::kOk;
}

}  // namespace oz3
