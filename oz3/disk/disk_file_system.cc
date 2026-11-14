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
  DiskSuperblock::Words superblock_words;
  if (DiskStatus status = hardware_.ReadSector(
          DiskLayout::kSuperblockSector, absl::MakeSpan(superblock_words));
      status != DiskStatus::kOk) {
    return status;
  }
  superblock_ = DiskSuperblock::FromWords(superblock_words);
  if (!superblock_.IsValid(sector_count)) {
    return DiskStatus::kUnformatted;
  }
  formatted_ = true;
  fat_.emplace(&hardware_, superblock_.GetLayout());
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
  // An unformatted disk is what formatting is for.
  if (DiskStatus status = GetWritableStatus();
      status != DiskStatus::kOk && status != DiskStatus::kUnformatted) {
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
  if (DiskStatus status = hardware_.WriteSector(
          DiskLayout::kSuperblockSector, kZeroSector, WriteCaching::kSkip);
      status != DiskStatus::kOk) {
    return status;
  }

  fat_.emplace(&hardware_, *layout);
  int root_cluster = 0;
  if (DiskStatus status = fat_->Format(&root_cluster);
      status != DiskStatus::kOk) {
    return status;
  }
  const int root_sector = layout->GetClusterSector(root_cluster);
  for (int i = 0; i < layout->sectors_per_cluster; ++i) {
    if (DiskStatus status = hardware_.WriteSector(root_sector + i, kZeroSector,
                                                  WriteCaching::kCache);
        status != DiskStatus::kOk) {
      return status;
    }
  }

  superblock_ = DiskSuperblock(*layout, root_cluster, label, now);
  if (DiskStatus status =
          hardware_.WriteSector(DiskLayout::kSuperblockSector,
                                superblock_.GetWords(), WriteCaching::kSkip);
      status != DiskStatus::kOk) {
    return status;
  }

  formatted_ = true;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::SetLabel(const DiskLabel& label) {
  if (DiskStatus status = GetWritableStatus(); status != DiskStatus::kOk) {
    return status;
  }
  superblock_.SetLabel(label);
  return hardware_.WriteSector(DiskLayout::kSuperblockSector,
                               superblock_.GetWords(), WriteCaching::kSkip);
}

DiskStatus DiskFileSystem::GetVolumeInfo(DiskVolumeInfo* info) {
  if (DiskStatus status = GetVolumeStatus(); status != DiskStatus::kOk) {
    return status;
  }
  if (DiskStatus status = fat_->CountFreeClusters();
      status != DiskStatus::kOk) {
    return status;
  }
  const DiskLayout layout = superblock_.GetLayout();
  // The label was checked when the superblock was attached or written.
  *info = {
      .label = *superblock_.GetLabel(),
      .sectors_per_cluster = layout.sectors_per_cluster,
      .cluster_count = layout.cluster_count,
      .free_clusters = *fat_->GetFreeCount(),
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
    if (DiskStatus status = fat_->CountFreeClusters();
        status != DiskStatus::kOk) {
      return status;
    }
  }
  DiskDirEntry entry;
  EntryLocation location;
  if (DiskStatus status = FindClosedFile(name, &entry, &location);
      status != DiskStatus::kOk) {
    return status;
  }

  if (DiskStatus status = GetFileEntryStatus(entry);
      status != DiskStatus::kOk) {
    return status;
  }
  OpenSlot(&slots_[free_slot], location,
           (entry.GetAttributes() & kDiskAttribReadOnly) != 0,
           entry.GetFirstCluster(), *entry.GetSize());
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
  if (DiskStatus status = fat_->CountFreeClusters();
      status != DiskStatus::kOk) {
    return status;
  }

  DiskDirEntry entry;
  entry.SetName(name);
  entry.SetModified(now);
  EntryLocation location;
  if (free_entry.has_value()) {
    location = *free_entry;
    if (DiskStatus status = PutEntry(location, entry);
        status != DiskStatus::kOk) {
      return status;
    }
  } else if (DiskStatus status = GrowRoot(entry, &location);
             status != DiskStatus::kOk) {
    return status;
  }
  if (DiskStatus status = CommitDirectory(); status != DiskStatus::kOk) {
    return status;
  }
  OpenSlot(&slots_[free_slot], location, /*read_only=*/false,
           /*first_cluster=*/0, /*size=*/0);
  *file = free_slot;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::Close(DiskFile file, DateTime now) {
  Slot* slot = nullptr;
  const DiskStatus slot_status = GetSlot(file, &slot);
  if (slot_status == DiskStatus::kBadHandle) {
    return slot_status;
  }
  // A stale slot's disk is gone, so its writes are lost. On a failed image,
  // a slot with nothing to commit closes cleanly, and one with writes fails.
  DiskStatus status = DiskStatus::kOk;
  if (slot_status != DiskStatus::kDiskChanged) {
    status = CommitSlot(slot, now);
  } else if (slot->written_since_stamp) {
    status = DiskStatus::kDiskChanged;
  }
  slot->state = SlotState::kFree;
  return status;
}

DiskStatus DiskFileSystem::Flush(DiskFile file, DateTime now) {
  Slot* slot = nullptr;
  if (DiskStatus status = GetSlot(file, &slot); status != DiskStatus::kOk) {
    return status;
  }
  return CommitSlot(slot, now);
}

DiskStatus DiskFileSystem::Flush(DateTime now) {
  if (DiskStatus status = GetVolumeStatus(); status != DiskStatus::kOk) {
    return status;
  }
  for (Slot& slot : slots_) {
    if (slot.state != SlotState::kOpen) {
      continue;
    }
    if (DiskStatus status = CommitSlot(&slot, now); status != DiskStatus::kOk) {
      return status;
    }
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::Truncate(DiskFile file, DateTime now) {
  Slot* slot = nullptr;
  if (DiskStatus status = GetWritableSlot(file, &slot);
      status != DiskStatus::kOk) {
    return status;
  }

  // The chain is walked before the file is committed, so a broken one leaves
  // the file as it was.
  const int cluster_size = GetClusterSize();
  const int kept_count = (slot->position + cluster_size - 1) / cluster_size;
  int kept_last = 0;
  int freed_first = 0;
  if (DiskStatus status =
          ReachClusterBefore(slot, kept_count, &kept_last, &freed_first);
      status != DiskStatus::kOk) {
    return status;
  }
  int freed_count = 0;
  int freed_last = 0;
  if (DiskStatus status =
          fat_->CountChain(freed_first, &freed_count, &freed_last);
      status != DiskStatus::kOk) {
    return status;
  }

  // The entry with the new size reaches the disk before the clusters are
  // freed, and the commit leaves every FAT buffer clean. One that changes
  // nothing only flushes the file.
  if (slot->position < slot->size || freed_count > 0) {
    slot->written_since_stamp = true;
  }
  slot->size = slot->position;
  if (kept_count == 0) {
    slot->first_cluster = 0;
    slot->last_cluster = 0;
    slot->last_cluster_index = 0;
  }
  if (DiskStatus status = CommitSlot(slot, now); status != DiskStatus::kOk) {
    return status;
  }
  return fat_->FreeChain(kept_last, freed_first, freed_last, freed_count);
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
  slot->written_since_stamp = true;
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
  *count = allocated - slot->position + *fat_->GetFreeCount() * cluster_size;
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

DiskStatus DiskFileSystem::Delete(const DiskFilename& name) {
  if (DiskStatus status = GetWritableStatus(); status != DiskStatus::kOk) {
    return status;
  }
  if (name.IsPattern()) {
    return DiskStatus::kBadName;
  }
  DiskDirEntry entry;
  EntryLocation location;
  if (DiskStatus status = FindClosedFile(name, &entry, &location);
      status != DiskStatus::kOk) {
    return status;
  }
  if ((entry.GetAttributes() & kDiskAttribReadOnly) != 0) {
    return DiskStatus::kReadOnly;
  }
  if (DiskStatus status = GetFileEntryStatus(entry);
      status != DiskStatus::kOk) {
    return status;
  }

  // The chain is walked before the entry is freed, so a broken one leaves the
  // file as it was.
  const int chain_first = entry.GetFirstCluster();
  int chain_count = 0;
  int chain_last = 0;
  if (DiskStatus status =
          fat_->CountChain(chain_first, &chain_count, &chain_last);
      status != DiskStatus::kOk) {
    return status;
  }

  // The free entry reaches the disk before the chain is freed, and the commit
  // leaves every FAT buffer clean.
  if (DiskStatus status = PutEntry(location, DiskDirEntry());
      status != DiskStatus::kOk) {
    return status;
  }
  if (DiskStatus status = CommitDirectory(); status != DiskStatus::kOk) {
    return status;
  }
  return fat_->FreeChain(/*unlink_from=*/0, chain_first, chain_last,
                         chain_count);
}

DiskStatus DiskFileSystem::Rename(const DiskFilename& old_name,
                                  const DiskFilename& new_name) {
  if (DiskStatus status = GetWritableStatus(); status != DiskStatus::kOk) {
    return status;
  }
  if (old_name.IsPattern() || new_name.IsPattern()) {
    return DiskStatus::kBadName;
  }
  if (DiskStatus status = RewindDirectory(); status != DiskStatus::kOk) {
    return status;
  }

  // One pass finds both the file, as the first entry with its name, and
  // whether the new name is taken. It stops once it knows the result, which
  // for an open file is kInUse whether or not the new name is taken.
  DiskDirEntry entry;
  std::optional<EntryLocation> location;
  bool file_open = false;
  bool new_name_taken = false;
  const DiskStatus find_status =
      VisitEntries(&dir_, [&](const DiskDirEntry& candidate,
                              const EntryLocation& candidate_location) {
        // A free entry, or one whose name breaks the rules, has no name.
        const std::optional<DiskFilename> name = candidate.GetName();
        if (!location.has_value() && name == old_name) {
          entry = candidate;
          location = candidate_location;
          file_open = IsOpen(location);
        }
        if (name == new_name) {
          new_name_taken = true;
        }
        return location.has_value() && (file_open || new_name_taken);
      });
  if (find_status != DiskStatus::kOk && find_status != DiskStatus::kNotFound) {
    return find_status;
  }
  if (!location.has_value()) {
    return DiskStatus::kNotFound;
  }
  if (file_open) {
    return DiskStatus::kInUse;
  }
  if (new_name_taken) {
    return DiskStatus::kExists;
  }
  entry.SetName(new_name);
  if (DiskStatus status = PutEntry(*location, entry);
      status != DiskStatus::kOk) {
    return status;
  }
  return CommitDirectory();
}

DiskStatus DiskFileSystem::SetAttributes(const DiskFilename& name,
                                         uint16_t attributes) {
  if (DiskStatus status = GetWritableStatus(); status != DiskStatus::kOk) {
    return status;
  }
  if (name.IsPattern()) {
    return DiskStatus::kBadName;
  }
  if ((attributes & ~kDiskAttribSettable) != 0) {
    return DiskStatus::kBadAttributes;
  }
  DiskDirEntry entry;
  EntryLocation location;
  if (DiskStatus status = FindClosedFile(name, &entry, &location);
      status != DiskStatus::kOk) {
    return status;
  }
  // The bits ATTRIB doesn't set keep their meaning.
  const uint16_t new_attributes = static_cast<uint16_t>(
      (entry.GetAttributes() & ~kDiskAttribSettable) | attributes);
  if (entry.GetAttributes() == new_attributes) {
    return DiskStatus::kOk;
  }
  entry.SetAttributes(new_attributes);
  if (DiskStatus status = PutEntry(location, entry);
      status != DiskStatus::kOk) {
    return status;
  }
  return CommitDirectory();
}

//------------------------------------------------------------------------------
// Volume
//------------------------------------------------------------------------------

void DiskFileSystem::ResetVolume() {
  formatted_ = false;
  superblock_ = {};
  fat_.reset();
  dir_.state = SlotState::kFree;
  dir_.buffer.Reset();
}

DiskStatus DiskFileSystem::GetVolumeStatus() const {
  if (DiskStatus status = hardware_.GetImageStatus();
      status != DiskStatus::kOk) {
    return status;
  }
  if (!formatted_) {
    return DiskStatus::kUnformatted;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::GetWritableStatus() const {
  if (DiskStatus status = hardware_.GetImageStatus();
      status != DiskStatus::kOk) {
    return status;
  }
  if (GetMedia()->IsWriteProtected()) {
    return DiskStatus::kWriteProtected;
  }
  if (!formatted_) {
    return DiskStatus::kUnformatted;
  }
  return DiskStatus::kOk;
}

int DiskFileSystem::GetClusterSize() const {
  return superblock_.GetLayout().GetClusterWords();
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
  // An open slot has an image, so this is only a failure.
  return hardware_.GetImageStatus();
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
  // GetSlot has checked the image, which the open slot's file is on.
  if (GetMedia()->IsWriteProtected()) {
    return DiskStatus::kWriteProtected;
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
                              bool read_only, int first_cluster, int size) {
  slot->state = SlotState::kOpen;
  slot->entry = entry;
  slot->read_only = read_only;
  slot->written_since_stamp = false;
  slot->size = size;
  slot->position = 0;
  slot->first_cluster = first_cluster;
  slot->last_cluster = 0;
  slot->last_cluster_index = 0;
  slot->buffer.Reset();
}

DiskStatus DiskFileSystem::OpenRootSlot(Slot* slot) {
  const int root_cluster = superblock_.GetRootCluster();
  int chain_count = 0;
  if (DiskStatus status = fat_->CountChain(root_cluster, &chain_count,
                                           /*chain_last=*/nullptr);
      status != DiskStatus::kOk) {
    return status;
  }
  OpenSlot(slot, /*entry=*/std::nullopt, /*read_only=*/false, root_cluster,
           chain_count * GetClusterSize());
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::CommitSlot(Slot* slot, DateTime now) {
  // Only a file is written, so a directory never has anything to commit.
  if (!slot->written_since_stamp) {
    return DiskStatus::kOk;
  }
  // Data before the size covering it, and CommitDirectory writes the links
  // before the entry using them.
  if (DiskStatus status = WriteBuffer(&slot->buffer);
      status != DiskStatus::kOk) {
    return status;
  }
  DiskDirEntry entry;
  if (DiskStatus status = GetEntry(*slot->entry, &entry);
      status != DiskStatus::kOk) {
    return status;
  }
  entry.SetFirstCluster(slot->first_cluster);
  entry.SetSize(slot->size);
  entry.SetModified(now);
  if (DiskStatus status = PutEntry(*slot->entry, entry);
      status != DiskStatus::kOk) {
    return status;
  }
  if (DiskStatus status = CommitDirectory(); status != DiskStatus::kOk) {
    return status;
  }
  slot->written_since_stamp = false;
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::ReachCluster(Slot* slot, int cluster_index) {
  if (slot->last_cluster == 0 || cluster_index < slot->last_cluster_index) {
    slot->last_cluster = slot->first_cluster;
    slot->last_cluster_index = 0;
  }
  while (slot->last_cluster_index < cluster_index) {
    int next = 0;
    if (DiskStatus status = fat_->GetNextCluster(slot->last_cluster, &next);
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

DiskStatus DiskFileSystem::ReachClusterBefore(Slot* slot, int cluster_index,
                                              int* previous, int* cluster) {
  if (cluster_index == 0) {
    *previous = 0;
    *cluster = slot->first_cluster;
    return DiskStatus::kOk;
  }
  if (DiskStatus status = ReachCluster(slot, cluster_index - 1);
      status != DiskStatus::kOk) {
    return status;
  }
  *previous = slot->last_cluster;
  return fat_->GetNextCluster(*previous, cluster);
}

DiskStatus DiskFileSystem::ReachWriteCluster(Slot* slot) {
  const int cluster_size = GetClusterSize();
  const int cluster_index = slot->position / cluster_size;
  // Only a write starting a cluster at the size can go past the chain the
  // size covers.
  if (slot->position < slot->size || slot->position % cluster_size != 0) {
    return ReachCluster(slot, cluster_index);
  }
  int last_cluster = 0;
  int cluster = 0;
  if (DiskStatus status =
          ReachClusterBefore(slot, cluster_index, &last_cluster, &cluster);
      status != DiskStatus::kOk) {
    return status;
  }
  if (cluster == 0) {
    if (DiskStatus status = fat_->AllocateCluster(last_cluster, &cluster);
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

DiskStatus DiskFileSystem::LoadSector(int sector, DiskSectorBuffer* buffer) {
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

DiskStatus DiskFileSystem::BlankSector(int sector, DiskSectorBuffer* buffer) {
  if (DiskStatus status = WriteBuffer(buffer); status != DiskStatus::kOk) {
    return status;
  }
  buffer->sector = sector;
  buffer->dirty = true;
  buffer->words.fill(0);
  return DiskStatus::kOk;
}

int DiskFileSystem::GetSlotSector(const Slot& slot) const {
  return superblock_.GetLayout().GetClusterSector(slot.last_cluster) +
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

DiskStatus DiskFileSystem::WriteBuffer(DiskSectorBuffer* buffer) {
  if (!buffer->dirty) {
    return DiskStatus::kOk;
  }
  if (DiskStatus status = hardware_.WriteSector(buffer->sector, buffer->words,
                                                WriteCaching::kCache);
      status != DiskStatus::kOk) {
    return status;
  }
  buffer->dirty = false;
  const auto update = [buffer](DiskSectorBuffer& other) {
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

DiskStatus DiskFileSystem::FindClosedFile(const DiskFilename& name,
                                          DiskDirEntry* entry,
                                          EntryLocation* location) {
  if (DiskStatus status = RewindDirectory(); status != DiskStatus::kOk) {
    return status;
  }
  if (DiskStatus status = FindNextEntry(
          &dir_,
          [&name](const DiskFilename& candidate) { return candidate == name; },
          entry, location);
      status != DiskStatus::kOk) {
    return status;
  }
  if (IsOpen(*location)) {
    return DiskStatus::kInUse;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::GetFileEntryStatus(const DiskDirEntry& entry) const {
  // A file has exactly the clusters its size needs, so only an empty file has
  // no first cluster.
  const int first_cluster = entry.GetFirstCluster();
  const std::optional<int> size = entry.GetSize();
  const DiskLayout layout = superblock_.GetLayout();
  if (!size.has_value() || first_cluster > layout.cluster_count ||
      (first_cluster == 0) != (*size == 0) || *size > layout.GetMaxFileSize()) {
    return DiskStatus::kCorrupt;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFileSystem::CommitDirectory() {
  if (DiskStatus status = fat_->Write(); status != DiskStatus::kOk) {
    return status;
  }
  return WriteBuffer(&dir_.buffer);
}

DiskStatus DiskFileSystem::GetEntry(const EntryLocation& location,
                                    DiskDirEntry* entry) {
  if (DiskStatus status = LoadSector(location.sector, &dir_.buffer);
      status != DiskStatus::kOk) {
    return status;
  }
  *entry = DiskDirEntry::FromWords(
      absl::MakeConstSpan(dir_.buffer.words)
          .subspan(location.index * DiskDirEntry::kWordCount,
                   DiskDirEntry::kWordCount));
  return DiskStatus::kOk;
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
  if (DiskStatus status = fat_->GetFreeHead(&cluster);
      status != DiskStatus::kOk) {
    return status;
  }

  // The cluster is written while it is still free, in order, so its sectors
  // are continuous. Blanking each sector writes the one before it.
  const DiskLayout layout = superblock_.GetLayout();
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
  if (DiskStatus status = fat_->AllocateCluster(dir_.last_cluster, &cluster);
      status != DiskStatus::kOk) {
    return status;
  }

  dir_.size += cluster_size;
  for (Slot& slot : slots_) {
    if (slot.state == SlotState::kOpen && !slot.entry.has_value()) {
      slot.size += cluster_size;
    }
  }
  return DiskStatus::kOk;
}

}  // namespace oz3
