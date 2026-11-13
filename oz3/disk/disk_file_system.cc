// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_file_system.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <iterator>
#include <optional>
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
constexpr int kSuperblockSector = 0;
constexpr int kFatHeadSector = 1;

// A FAT link ending a chain, or as the free list's head, a full disk.
constexpr uint16_t kFatEnd = 0xFFFF;

// The cluster formatting gives the root directory.
constexpr int kFormatRootCluster = 1;

constexpr std::array<uint16_t, kDiskSectorWords> kZeroSector = {};

}  // namespace

DiskFileSystem::DiskFileSystem(const DiskHardwareSettings& settings,
                               int max_open_files)
    : hardware_(settings) {
  CHECK_GE(max_open_files, 1);
  slots_.resize(max_open_files);
}

DiskStatus DiskFileSystem::AttachImage(std::unique_ptr<DiskImage> image) {
  CHECK(image != nullptr);
  DetachImage();
  const int sector_count = image->GetSectorCount();
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
  InvalidateOpenFiles();
  ResetVolume();
  return hardware_.DetachImage();
}

absl::Duration DiskFileSystem::CollectBusyTime() {
  return hardware_.CollectBusyTime();
}

DiskStatus DiskFileSystem::Format(const DiskLabel& label,
                                  int sectors_per_cluster, DateTime now) {
  if (DiskStatus status = GetWritableStatus(); status != DiskStatus::kOk) {
    return status;
  }
  if (std::any_of(slots_.begin(), slots_.end(), [](const Slot& slot) {
        return slot.state == SlotState::kOpen;
      })) {
    return DiskStatus::kInUse;
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
  for (int sector = kFatHeadSector + 1; sector <= layout->fat_sectors;
       ++sector) {
    if (DiskStatus status =
            hardware_.WriteSector(sector, kZeroSector, WriteCaching::kCache);
        status != DiskStatus::kOk) {
      return status;
    }
  }
  const int root_sector = layout->GetClusterSector(kFormatRootCluster);
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
  if (DiskStatus status = GetWritableStatus(); status != DiskStatus::kOk) {
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
  if (DiskStatus status = GetVolumeStatus(); status != DiskStatus::kOk) {
    return status;
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

DiskStatus DiskFileSystem::Open(const DiskFilename& name, DiskFile* file) {
  if (DiskStatus status = GetVolumeStatus(); status != DiskStatus::kOk) {
    return status;
  }
  if (name.IsPattern()) {
    return DiskStatus::kBadName;
  }
  const DiskFile free_slot = FindFreeSlot();
  if (free_slot < 0) {
    return DiskStatus::kTooManyOpen;
  }
  if (!GetMedia()->IsWriteProtected()) {
    if (DiskStatus status = CountFreeClusters(); status != DiskStatus::kOk) {
      return status;
    }
  }
  // The directory slot stays open, keeping its sector, until the volume is
  // reset.
  if (dir_.state == SlotState::kOpen) {
    dir_.position = 0;
  } else if (DiskStatus status = OpenRootSlot(&dir_);
             status != DiskStatus::kOk) {
    return status;
  }
  DiskDirEntry entry;
  EntryLocation location;
  if (DiskStatus status = FindNextEntry(
          &dir_,
          [&name](const DiskFilename& candidate) { return candidate == name; },
          &entry, &location);
      status != DiskStatus::kOk) {
    return status;
  }
  if (IsOpen(location)) {
    return DiskStatus::kInUse;
  }

  // A file has exactly the clusters its size needs, so only an empty file has
  // no first cluster.
  const int first_cluster = entry.GetFirstCluster();
  const std::optional<int> entry_size = entry.GetSize();
  const DiskLayout layout = GetLayout(superblock_);
  if (!entry_size.has_value() || first_cluster > layout.cluster_count ||
      (first_cluster == 0) != (*entry_size == 0) ||
      *entry_size > layout.GetMaxFileSize()) {
    return DiskStatus::kCorrupt;
  }
  OpenSlot(&slots_[free_slot], location, first_cluster, *entry_size);
  *file = free_slot;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::OpenRoot(DiskFile* file) {
  if (DiskStatus status = GetVolumeStatus(); status != DiskStatus::kOk) {
    return status;
  }
  const DiskFile free_slot = FindFreeSlot();
  if (free_slot < 0) {
    return DiskStatus::kTooManyOpen;
  }
  if (IsOpen(std::nullopt)) {
    return DiskStatus::kInUse;
  }
  if (DiskStatus status = OpenRootSlot(&slots_[free_slot]);
      status != DiskStatus::kOk) {
    return status;
  }
  *file = free_slot;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::Close(DiskFile file) {
  Slot* slot = nullptr;
  // A stale slot is freed too.
  if (GetSlot(file, &slot) == DiskStatus::kBadHandle) {
    return DiskStatus::kBadHandle;
  }
  slot->state = SlotState::kFree;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::Read(DiskFile file, absl::Span<uint16_t> words,
                                int* count) {
  *count = 0;
  Slot* slot = nullptr;
  if (DiskStatus status = GetSlot(file, &slot); status != DiskStatus::kOk) {
    return status;
  }
  const int offset = slot->position % kDiskSectorWords;
  const int left =
      std::min(slot->size - slot->position, kDiskSectorWords - offset);
  const int read_count =
      static_cast<int>(std::min<int64_t>(std::ssize(words), left));
  if (read_count == 0) {
    return DiskStatus::kOk;
  }
  if (DiskStatus status = LoadSlotSector(slot); status != DiskStatus::kOk) {
    return status;
  }
  std::copy_n(slot->buffer.words.begin() + offset, read_count, words.begin());
  slot->position += read_count;
  *count = read_count;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::Seek(DiskFile file, int position) {
  Slot* slot = nullptr;
  if (DiskStatus status = GetSlot(file, &slot); status != DiskStatus::kOk) {
    return status;
  }
  if (position < 0) {
    return DiskStatus::kBadArgument;
  }
  if (position > slot->size) {
    return DiskStatus::kPastEnd;
  }
  slot->position = position;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::GetFileInfo(DiskFile file, DiskFileInfo* info) {
  Slot* slot = nullptr;
  if (DiskStatus status = GetSlot(file, &slot); status != DiskStatus::kOk) {
    return status;
  }
  *info = {.position = slot->position, .size = slot->size};
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::Find(DiskFile directory, const DiskFilename& pattern,
                                DiskDirEntry* entry) {
  Slot* slot = nullptr;
  if (DiskStatus status = GetSlot(directory, &slot);
      status != DiskStatus::kOk) {
    return status;
  }
  if (slot->entry.has_value()) {
    return DiskStatus::kWrongHandleType;
  }
  return FindNextEntry(
      slot,
      [&pattern](const DiskFilename& name) { return pattern.Matches(name); },
      entry, /*location=*/nullptr);
}

//------------------------------------------------------------------------------
// Superblock
//------------------------------------------------------------------------------

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
                                       int sector_count) {
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

//------------------------------------------------------------------------------
// Volume
//------------------------------------------------------------------------------

void DiskFileSystem::ResetVolume() {
  formatted_ = false;
  superblock_ = {};
  free_count_.reset();
  dir_.state = SlotState::kFree;
  dir_.buffer.sector = -1;
  fat_head_ = {};
  fat_work_ = {};
}

DiskStatus DiskFileSystem::GetVolumeStatus() const {
  if (!HasImage()) {
    return DiskStatus::kNoDisk;
  }
  if (!formatted_) {
    return DiskStatus::kUnformatted;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::GetWritableStatus() const {
  const DiskMedia* media = GetMedia();
  if (media == nullptr) {
    return DiskStatus::kNoDisk;
  }
  if (media->IsWriteProtected()) {
    return DiskStatus::kWriteProtected;
  }
  return DiskStatus::kOk;
}

int DiskFileSystem::GetClusterSize() const {
  return GetLayout(superblock_).sectors_per_cluster * kDiskSectorWords;
}

DiskStatus DiskFileSystem::CountFreeClusters() {
  if (free_count_.has_value()) {
    return DiskStatus::kOk;
  }
  // Word 0 is the link of an imaginary cluster 0, to the free list's head.
  int head = 0;
  if (DiskStatus status = GetNextCluster(0, &head); status != DiskStatus::kOk) {
    return status;
  }
  int count = 0;
  if (DiskStatus status = CountChain(head, &count); status != DiskStatus::kOk) {
    return status;
  }
  free_count_ = count;
  return DiskStatus::kOk;
}

//------------------------------------------------------------------------------
// Slots
//------------------------------------------------------------------------------

void DiskFileSystem::InvalidateOpenFiles() {
  for (Slot& slot : slots_) {
    if (slot.state == SlotState::kOpen) {
      slot.state = SlotState::kStale;
    }
  }
}

DiskStatus DiskFileSystem::GetSlot(DiskFile file, Slot** slot) {
  if (file < 0 || file >= GetMaxOpenFiles() ||
      slots_[file].state == SlotState::kFree) {
    return DiskStatus::kBadHandle;
  }
  *slot = &slots_[file];
  if ((*slot)->state == SlotState::kStale) {
    return DiskStatus::kDiskChanged;
  }
  return DiskStatus::kOk;
}

DiskFile DiskFileSystem::FindFreeSlot() const {
  for (DiskFile file = 0; file < GetMaxOpenFiles(); ++file) {
    if (slots_[file].state == SlotState::kFree) {
      return file;
    }
  }
  return -1;
}

bool DiskFileSystem::IsOpen(const std::optional<EntryLocation>& entry) const {
  return std::any_of(slots_.begin(), slots_.end(), [&entry](const Slot& slot) {
    return slot.state == SlotState::kOpen && slot.entry == entry;
  });
}

void DiskFileSystem::OpenSlot(Slot* slot,
                              const std::optional<EntryLocation>& entry,
                              int first_cluster, int size) {
  slot->state = SlotState::kOpen;
  slot->entry = entry;
  slot->size = size;
  slot->position = 0;
  slot->first_cluster = first_cluster;
  slot->last_cluster = 0;
  slot->last_cluster_index = 0;
  slot->buffer.sector = -1;
}

DiskStatus DiskFileSystem::OpenRootSlot(Slot* slot) {
  const int root_cluster = GetRootCluster(superblock_);
  int chain_count = 0;
  if (DiskStatus status = CountChain(root_cluster, &chain_count);
      status != DiskStatus::kOk) {
    return status;
  }
  OpenSlot(slot, /*entry=*/std::nullopt, root_cluster,
           chain_count * GetClusterSize());
  return DiskStatus::kOk;
}

//------------------------------------------------------------------------------
// Sectors
//------------------------------------------------------------------------------

DiskStatus DiskFileSystem::LoadSector(int sector, SectorBuffer* buffer) {
  if (buffer->sector == sector) {
    return DiskStatus::kOk;
  }
  buffer->sector = -1;
  if (DiskStatus status =
          hardware_.ReadSector(sector, absl::MakeSpan(buffer->words));
      status != DiskStatus::kOk) {
    return status;
  }
  buffer->sector = sector;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::LoadSlotSector(Slot* slot) {
  const int cluster_size = GetClusterSize();
  const int cluster_index = slot->position / cluster_size;
  if (slot->last_cluster == 0 || cluster_index < slot->last_cluster_index) {
    slot->last_cluster = slot->first_cluster;
    slot->last_cluster_index = 0;
  }
  while (slot->last_cluster_index < cluster_index) {
    int next = 0;
    if (DiskStatus status = GetNextCluster(slot->last_cluster, &next);
        status != DiskStatus::kOk) {
      return status;
    }
    // The chain is shorter than the file's size.
    if (next == 0) {
      return DiskStatus::kCorrupt;
    }
    slot->last_cluster = next;
    ++slot->last_cluster_index;
  }
  const int sector =
      GetLayout(superblock_).GetClusterSector(slot->last_cluster) +
      slot->position % cluster_size / kDiskSectorWords;
  return LoadSector(sector, &slot->buffer);
}

//------------------------------------------------------------------------------
// FAT
//------------------------------------------------------------------------------

DiskStatus DiskFileSystem::ReadFatWord(int index, uint16_t* value) {
  CHECK_GE(index, 0);
  CHECK_LE(index, GetLayout(superblock_).cluster_count);
  const int sector = kFatHeadSector + index / kDiskSectorWords;
  SectorBuffer* buffer = (sector == kFatHeadSector ? &fat_head_ : &fat_work_);
  if (DiskStatus status = LoadSector(sector, buffer);
      status != DiskStatus::kOk) {
    return status;
  }
  *value = buffer->words[index % kDiskSectorWords];
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::GetNextCluster(int cluster, int* next) {
  uint16_t link = 0;
  if (DiskStatus status = ReadFatWord(cluster, &link);
      status != DiskStatus::kOk) {
    return status;
  }
  // A link of 0 is the next cluster, or the end after the last one.
  const int cluster_count = GetLayout(superblock_).cluster_count;
  if (link == kFatEnd || (link == 0 && cluster == cluster_count)) {
    *next = 0;
  } else if (link == 0) {
    *next = cluster + 1;
  } else if (link > cluster_count) {
    return DiskStatus::kCorrupt;
  } else {
    *next = link;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::CountChain(int first_cluster, int* count) {
  const int cluster_count = GetLayout(superblock_).cluster_count;
  int chain_count = 0;
  for (int cluster = first_cluster; cluster != 0;) {
    // A chain longer than the disk loops.
    if (++chain_count > cluster_count) {
      return DiskStatus::kCorrupt;
    }
    if (DiskStatus status = GetNextCluster(cluster, &cluster);
        status != DiskStatus::kOk) {
      return status;
    }
  }
  *count = chain_count;
  return DiskStatus::kOk;
}

//------------------------------------------------------------------------------
// Directory
//------------------------------------------------------------------------------

DiskStatus DiskFileSystem::FindNextEntry(
    Slot* slot, absl::FunctionRef<bool(const DiskFilename&)> matches,
    DiskDirEntry* entry, EntryLocation* location) {
  // Entries start at multiples of their size, and a directory's size is a
  // multiple of a sector's, so each sector is whole entries.
  slot->position += DiskDirEntry::kWordCount - 1;
  slot->position -= slot->position % DiskDirEntry::kWordCount;
  while (slot->position < slot->size) {
    if (DiskStatus status = LoadSlotSector(slot); status != DiskStatus::kOk) {
      return status;
    }
    const absl::Span<const uint16_t> words = slot->buffer.words;
    for (int offset = slot->position % kDiskSectorWords;
         offset < kDiskSectorWords; offset += DiskDirEntry::kWordCount) {
      slot->position += DiskDirEntry::kWordCount;
      // A free entry's first word is 0.
      if (words[offset] == 0) {
        continue;
      }
      const DiskDirEntry candidate = DiskDirEntry::FromWords(
          words.subspan(offset, DiskDirEntry::kWordCount));
      if (const std::optional<DiskFilename> name = candidate.GetName();
          name.has_value() && matches(*name)) {
        *entry = candidate;
        if (location != nullptr) {
          *location = {
              .sector = slot->buffer.sector,
              .index = offset / DiskDirEntry::kWordCount,
          };
        }
        return DiskStatus::kOk;
      }
    }
  }
  return DiskStatus::kNotFound;
}

}  // namespace oz3
