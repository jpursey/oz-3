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

// Returns the sector holding the FAT's word `index`.
int GetFatSector(int index) {
  return kFatHeadSector + index / kDiskSectorWords;
}

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
  if (DiskStatus status = RewindDirectory(); status != DiskStatus::kOk) {
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
  OpenSlot(&slots_[free_slot], location,
           (entry.GetAttributes() & kDiskAttribReadOnly) != 0, first_cluster,
           *entry_size);
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

DiskStatus DiskFileSystem::Create(const DiskFilename& name, DateTime now,
                                  DiskFile* file) {
  if (DiskStatus status = GetWritableStatus(); status != DiskStatus::kOk) {
    return status;
  }
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
  if (DiskStatus status = RewindDirectory(); status != DiskStatus::kOk) {
    return status;
  }

  // One pass finds both the name and the first free entry.
  std::optional<EntryLocation> free_entry;
  const DiskStatus find_status =
      VisitEntries(&dir_, [&name, &free_entry](const DiskDirEntry& entry,
                                               const EntryLocation& location) {
        if (entry.IsFree()) {
          if (!free_entry.has_value()) {
            free_entry = location;
          }
          return false;
        }
        return entry.GetName() == name;
      });
  if (find_status == DiskStatus::kOk) {
    return DiskStatus::kExists;
  }
  if (find_status != DiskStatus::kNotFound) {
    return find_status;
  }
  if (DiskStatus status = CountFreeClusters(); status != DiskStatus::kOk) {
    return status;
  }

  DiskDirEntry entry;
  entry.SetName(name);
  entry.SetModified(now);
  EntryLocation location;
  DiskStatus status = DiskStatus::kOk;
  if (free_entry.has_value()) {
    location = *free_entry;
    status = PutEntry(location, entry);
  } else {
    status = GrowRoot(entry, &location);
  }
  if (status == DiskStatus::kOk) {
    status = CommitDirectory();
  }
  if (status != DiskStatus::kOk) {
    // The directory buffer may hold the entry, unwritten, so it is read again
    // when next needed. Every other change a failure can leave follows a
    // failed host write, after which the image refuses everything. The FAT
    // buffers are kept, as they may hold other files' allocations.
    dir_.buffer.Reset();
    return status;
  }
  if (!free_entry.has_value()) {
    GrowRootSlots();
  }
  OpenSlot(&slots_[free_slot], location, /*read_only=*/false,
           /*first_cluster=*/0, /*size=*/0);
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

DiskStatus DiskFileSystem::Write(DiskFile file,
                                 absl::Span<const uint16_t> words, int* count) {
  *count = 0;
  Slot* slot = nullptr;
  if (DiskStatus status = GetWritableSlot(file, &slot);
      status != DiskStatus::kOk) {
    return status;
  }
  const int offset = slot->position % kDiskSectorWords;
  const int write_count = static_cast<int>(
      std::min<int64_t>(std::ssize(words), kDiskSectorWords - offset));
  if (write_count == 0) {
    return DiskStatus::kOk;
  }
  if (DiskStatus status = ReachWriteCluster(slot); status != DiskStatus::kOk) {
    return status;
  }
  // A sector the write covers up to the size or its end keeps nothing of the
  // file, so it isn't read, unless the buffer already holds it.
  const int sector = GetSlotSector(*slot);
  const bool replaces_sector = slot->buffer.sector != sector && offset == 0 &&
                               (write_count == kDiskSectorWords ||
                                slot->position + write_count >= slot->size);
  if (DiskStatus status = (replaces_sector ? BlankSector(sector, &slot->buffer)
                                           : LoadSector(sector, &slot->buffer));
      status != DiskStatus::kOk) {
    return status;
  }
  std::copy_n(words.begin(), write_count, slot->buffer.words.begin() + offset);
  slot->buffer.dirty = true;
  slot->position += write_count;
  slot->size = std::max(slot->size, slot->position);
  *count = write_count;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::GetWritableWords(DiskFile file, int* count) {
  Slot* slot = nullptr;
  if (DiskStatus status = GetWritableSlot(file, &slot);
      status != DiskStatus::kOk) {
    return status;
  }
  // Every file open on a writable disk has the free count.
  const int cluster_size = GetClusterSize();
  const int allocated =
      (slot->size + cluster_size - 1) / cluster_size * cluster_size;
  *count = allocated - slot->position + *free_count_ * cluster_size;
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
  dir_.buffer.Reset();
  fat_head_.Reset();
  fat_work_.Reset();
  // Formatting starts from a zeroed FAT head.
  fat_head_.words.fill(0);
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

DiskStatus DiskFileSystem::GetWritableSlot(DiskFile file, Slot** slot) {
  if (DiskStatus status = GetSlot(file, slot); status != DiskStatus::kOk) {
    return status;
  }
  if (!(*slot)->entry.has_value()) {
    return DiskStatus::kWrongHandleType;
  }
  if ((*slot)->read_only) {
    return DiskStatus::kReadOnly;
  }
  return GetWritableStatus();
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
                              bool read_only, int first_cluster, int size) {
  slot->state = SlotState::kOpen;
  slot->entry = entry;
  slot->read_only = read_only;
  slot->size = size;
  slot->position = 0;
  slot->first_cluster = first_cluster;
  slot->last_cluster = 0;
  slot->last_cluster_index = 0;
  slot->buffer.Reset();
}

DiskStatus DiskFileSystem::OpenRootSlot(Slot* slot) {
  const int root_cluster = GetRootCluster(superblock_);
  int chain_count = 0;
  if (DiskStatus status = CountChain(root_cluster, &chain_count);
      status != DiskStatus::kOk) {
    return status;
  }
  OpenSlot(slot, /*entry=*/std::nullopt, /*read_only=*/false, root_cluster,
           chain_count * GetClusterSize());
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::ReachCluster(Slot* slot, int cluster_index) {
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
    if (next == 0) {
      return DiskStatus::kCorrupt;
    }
    slot->last_cluster = next;
    ++slot->last_cluster_index;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::ReachWriteCluster(Slot* slot) {
  const int cluster_size = GetClusterSize();
  const int cluster_index = slot->position / cluster_size;
  // Only a write starting a cluster at the size can go past the chain the
  // size covers.
  if (slot->position < slot->size || slot->position % cluster_size != 0) {
    return ReachCluster(slot, cluster_index);
  }
  int cluster = slot->first_cluster;
  int last_cluster = 0;
  if (cluster_index > 0) {
    if (DiskStatus status = ReachCluster(slot, cluster_index - 1);
        status != DiskStatus::kOk) {
      return status;
    }
    last_cluster = slot->last_cluster;
    if (DiskStatus status = GetNextCluster(last_cluster, &cluster);
        status != DiskStatus::kOk) {
      return status;
    }
  }
  if (cluster == 0) {
    if (DiskStatus status = AllocateCluster(last_cluster, &cluster);
        status != DiskStatus::kOk) {
      return status;
    }
    if (cluster_index == 0) {
      slot->first_cluster = cluster;
    }
  }
  slot->last_cluster = cluster;
  slot->last_cluster_index = cluster_index;
  return DiskStatus::kOk;
}

//------------------------------------------------------------------------------
// Sectors
//------------------------------------------------------------------------------

DiskStatus DiskFileSystem::LoadSector(int sector, SectorBuffer* buffer) {
  if (buffer->sector == sector) {
    return DiskStatus::kOk;
  }
  if (DiskStatus status = WriteBuffer(buffer); status != DiskStatus::kOk) {
    return status;
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

DiskStatus DiskFileSystem::BlankSector(int sector, SectorBuffer* buffer) {
  if (DiskStatus status = WriteBuffer(buffer); status != DiskStatus::kOk) {
    return status;
  }
  buffer->sector = sector;
  buffer->dirty = true;
  buffer->words.fill(0);
  return DiskStatus::kOk;
}

int DiskFileSystem::GetSlotSector(const Slot& slot) const {
  return GetLayout(superblock_).GetClusterSector(slot.last_cluster) +
         slot.position % GetClusterSize() / kDiskSectorWords;
}

DiskStatus DiskFileSystem::LoadSlotSector(Slot* slot) {
  // A chain shorter than the file's size is corrupt.
  if (DiskStatus status = ReachCluster(slot, slot->position / GetClusterSize());
      status != DiskStatus::kOk) {
    return status;
  }
  return LoadSector(GetSlotSector(*slot), &slot->buffer);
}

DiskStatus DiskFileSystem::WriteBuffer(SectorBuffer* buffer) {
  if (!buffer->dirty) {
    return DiskStatus::kOk;
  }
  if (buffer == &fat_work_) {
    if (DiskStatus status = WriteBuffer(&fat_head_);
        status != DiskStatus::kOk) {
      return status;
    }
  }
  if (DiskStatus status = hardware_.WriteSector(
          buffer->sector, buffer->words,
          buffer == &fat_head_ ? WriteCaching::kSkip : WriteCaching::kCache);
      status != DiskStatus::kOk) {
    return status;
  }
  buffer->dirty = false;
  const auto update = [buffer](SectorBuffer& other) {
    if (&other != buffer && other.sector == buffer->sector) {
      other.words = buffer->words;
    }
  };
  update(dir_.buffer);
  for (Slot& slot : slots_) {
    if (slot.state == SlotState::kOpen) {
      update(slot.buffer);
    }
  }
  return DiskStatus::kOk;
}

//------------------------------------------------------------------------------
// FAT
//------------------------------------------------------------------------------

DiskStatus DiskFileSystem::WriteFat() {
  if (DiskStatus status = WriteBuffer(&fat_head_); status != DiskStatus::kOk) {
    return status;
  }
  return WriteBuffer(&fat_work_);
}

DiskStatus DiskFileSystem::LoadFatSector(int index, SectorBuffer** buffer) {
  CHECK_GE(index, 0);
  CHECK_LE(index, GetLayout(superblock_).cluster_count);
  const int sector = GetFatSector(index);
  *buffer = (sector == kFatHeadSector ? &fat_head_ : &fat_work_);
  return LoadSector(sector, *buffer);
}

DiskStatus DiskFileSystem::ReadFatWord(int index, uint16_t* value) {
  SectorBuffer* buffer = nullptr;
  if (DiskStatus status = LoadFatSector(index, &buffer);
      status != DiskStatus::kOk) {
    return status;
  }
  *value = buffer->words[index % kDiskSectorWords];
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::SetFatWord(int index, uint16_t value) {
  SectorBuffer* buffer = nullptr;
  if (DiskStatus status = LoadFatSector(index, &buffer);
      status != DiskStatus::kOk) {
    return status;
  }
  buffer->words[index % kDiskSectorWords] = value;
  buffer->dirty = true;
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

DiskStatus DiskFileSystem::GetFreeHead(int* head) {
  if (*free_count_ == 0) {
    return DiskStatus::kDiskFull;
  }
  // Word 0 is the link of an imaginary cluster 0, to the free list's head.
  if (DiskStatus status = GetNextCluster(0, head); status != DiskStatus::kOk) {
    return status;
  }
  // The count and the list disagree.
  if (*head == 0) {
    return DiskStatus::kCorrupt;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::AllocateCluster(int last_cluster, int* cluster) {
  int head = 0;
  if (DiskStatus status = GetFreeHead(&head); status != DiskStatus::kOk) {
    return status;
  }
  int next = 0;
  if (DiskStatus status = GetNextCluster(head, &next);
      status != DiskStatus::kOk) {
    return status;
  }
  if (DiskStatus status =
          SetFatWord(0, next == 0 ? kFatEnd : static_cast<uint16_t>(next));
      status != DiskStatus::kOk) {
    return status;
  }
  if (DiskStatus status = SetFatWord(head, kFatEnd);
      status != DiskStatus::kOk) {
    return status;
  }
  --*free_count_;
  if (last_cluster != 0) {
    // A link in another sector than the cluster's end could reach the disk
    // first, so the end is written now.
    if (GetFatSector(last_cluster) != GetFatSector(head)) {
      if (DiskStatus status = WriteFat(); status != DiskStatus::kOk) {
        return status;
      }
    }
    if (DiskStatus status =
            SetFatWord(last_cluster, static_cast<uint16_t>(head));
        status != DiskStatus::kOk) {
      return status;
    }
  }
  *cluster = head;
  return DiskStatus::kOk;
}

//------------------------------------------------------------------------------
// Directory
//------------------------------------------------------------------------------

DiskStatus DiskFileSystem::RewindDirectory() {
  if (dir_.state == SlotState::kOpen) {
    dir_.position = 0;
    return DiskStatus::kOk;
  }
  return OpenRootSlot(&dir_);
}

DiskStatus DiskFileSystem::VisitEntries(
    Slot* slot,
    absl::FunctionRef<bool(const DiskDirEntry&, const EntryLocation&)> visit) {
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
      if (visit(DiskDirEntry::FromWords(
                    words.subspan(offset, DiskDirEntry::kWordCount)),
                {
                    .sector = slot->buffer.sector,
                    .index = offset / DiskDirEntry::kWordCount,
                })) {
        return DiskStatus::kOk;
      }
    }
  }
  return DiskStatus::kNotFound;
}

DiskStatus DiskFileSystem::FindNextEntry(
    Slot* slot, absl::FunctionRef<bool(const DiskFilename&)> matches,
    DiskDirEntry* entry, EntryLocation* location) {
  return VisitEntries(slot, [&](const DiskDirEntry& candidate,
                                const EntryLocation& candidate_location) {
    if (candidate.IsFree()) {
      return false;
    }
    if (const std::optional<DiskFilename> name = candidate.GetName();
        !name.has_value() || !matches(*name)) {
      return false;
    }
    *entry = candidate;
    if (location != nullptr) {
      *location = candidate_location;
    }
    return true;
  });
}

DiskStatus DiskFileSystem::CommitDirectory() {
  if (DiskStatus status = WriteFat(); status != DiskStatus::kOk) {
    return status;
  }
  return WriteBuffer(&dir_.buffer);
}

DiskStatus DiskFileSystem::PutEntry(const EntryLocation& location,
                                    const DiskDirEntry& entry) {
  if (DiskStatus status = LoadSector(location.sector, &dir_.buffer);
      status != DiskStatus::kOk) {
    return status;
  }
  std::copy(
      entry.GetWords().begin(), entry.GetWords().end(),
      dir_.buffer.words.begin() + location.index * DiskDirEntry::kWordCount);
  dir_.buffer.dirty = true;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::GrowRoot(const DiskDirEntry& entry,
                                    EntryLocation* location) {
  const int cluster_size = GetClusterSize();
  if (DiskStatus status = ReachCluster(&dir_, dir_.size / cluster_size - 1);
      status != DiskStatus::kOk) {
    return status;
  }
  int cluster = 0;
  if (DiskStatus status = GetFreeHead(&cluster); status != DiskStatus::kOk) {
    return status;
  }

  // The cluster is written while it is still free, in order, so its sectors
  // are continuous. Blanking each sector writes the one before it.
  const DiskLayout layout = GetLayout(superblock_);
  const int first_sector = layout.GetClusterSector(cluster);
  *location = {.sector = first_sector, .index = 0};
  for (int sector = first_sector;
       sector < first_sector + layout.sectors_per_cluster; ++sector) {
    if (DiskStatus status = BlankSector(sector, &dir_.buffer);
        status != DiskStatus::kOk) {
      return status;
    }
    if (sector == first_sector) {
      if (DiskStatus status = PutEntry(*location, entry);
          status != DiskStatus::kOk) {
        return status;
      }
    }
  }
  if (DiskStatus status = WriteBuffer(&dir_.buffer);
      status != DiskStatus::kOk) {
    return status;
  }
  return AllocateCluster(dir_.last_cluster, &cluster);
}

void DiskFileSystem::GrowRootSlots() {
  const int cluster_size = GetClusterSize();
  dir_.size += cluster_size;
  for (Slot& slot : slots_) {
    if (slot.state == SlotState::kOpen && !slot.entry.has_value()) {
      slot.size += cluster_size;
    }
  }
}

}  // namespace oz3
