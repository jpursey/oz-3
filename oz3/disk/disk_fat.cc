// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_fat.h"

#include <array>

#include "absl/log/check.h"
#include "absl/types/span.h"

namespace oz3 {

namespace {

using WriteCaching = DiskHardware::WriteCaching;

constexpr std::array<uint16_t, kDiskSectorWords> kZeroSector = {};

}  // namespace

DiskStatus DiskFat::Format(int* root_cluster) {
  CHECK(head_.sector == -1 && work_.sector == -1);
  // A zeroed FAT has every cluster on the free list, in order, and the root
  // takes its head. Sector 1 holds both words the allocation changes, so it is
  // all in the buffer.
  head_.sector = DiskLayout::kFirstFatSector;
  head_.words.fill(0);
  free_count_ = layout_.cluster_count;
  if (DiskStatus status = AllocateCluster(/*last_cluster=*/0, root_cluster);
      status != DiskStatus::kOk) {
    return status;
  }
  if (DiskStatus status = WriteBuffer(&head_); status != DiskStatus::kOk) {
    return status;
  }
  for (int sector = DiskLayout::kFirstFatSector + 1;
       sector <= layout_.fat_sectors; ++sector) {
    if (DiskStatus status =
            hardware_->WriteSector(sector, kZeroSector, WriteCaching::kCache);
        status != DiskStatus::kOk) {
      return status;
    }
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFat::Write() {
  if (DiskStatus status = WriteBuffer(&head_); status != DiskStatus::kOk) {
    return status;
  }
  return WriteBuffer(&work_);
}

DiskStatus DiskFat::CountFreeClusters() {
  if (free_count_.has_value()) {
    return DiskStatus::kOk;
  }
  // Word 0 is the link of an imaginary cluster 0, to the free list's head.
  int head = 0;
  if (DiskStatus status = GetNextCluster(0, &head); status != DiskStatus::kOk) {
    return status;
  }
  int count = 0;
  if (DiskStatus status = CountChain(head, &count, /*chain_last=*/nullptr);
      status != DiskStatus::kOk) {
    return status;
  }
  free_count_ = count;
  return DiskStatus::kOk;
}

DiskStatus DiskFat::GetFreeHead(int* head) {
  CHECK(free_count_.has_value());
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

DiskStatus DiskFat::AllocateCluster(int last_cluster, int* cluster) {
  int head = 0;
  if (DiskStatus status = GetFreeHead(&head); status != DiskStatus::kOk) {
    return status;
  }
  int next = 0;
  if (DiskStatus status = GetNextCluster(head, &next);
      status != DiskStatus::kOk) {
    return status;
  }
  if (DiskStatus status = SetFatWord(
          0, next == 0 ? DiskLayout::kFatEnd : static_cast<uint16_t>(next));
      status != DiskStatus::kOk) {
    return status;
  }
  if (DiskStatus status = SetFatWord(head, DiskLayout::kFatEnd);
      status != DiskStatus::kOk) {
    return status;
  }
  --*free_count_;
  if (last_cluster != 0) {
    // A link in another sector than the cluster's end could reach the disk
    // first, so the end is written now.
    if (DiskLayout::GetFatSector(last_cluster) !=
        DiskLayout::GetFatSector(head)) {
      if (DiskStatus status = Write(); status != DiskStatus::kOk) {
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

DiskStatus DiskFat::FreeChain(int unlink_from, int chain_first, int chain_last,
                              int chain_count) {
  if (chain_count == 0) {
    return DiskStatus::kOk;
  }
  CHECK(!head_.dirty && !work_.dirty);
  int head = 0;
  if (DiskStatus status = GetNextCluster(0, &head); status != DiskStatus::kOk) {
    return status;
  }
  // Each change first writes the other buffer, if a change left it dirty.
  // Moving the working buffer to another sector writes it too.
  const auto change = [this](int index, uint16_t value) {
    if (DiskStatus status = WriteBuffer(DiskLayout::GetFatSector(index) ==
                                                DiskLayout::kFirstFatSector
                                            ? &work_
                                            : &head_);
        status != DiskStatus::kOk) {
      return status;
    }
    return SetFatWord(index, value);
  };
  if (unlink_from != 0) {
    if (DiskStatus status = change(unlink_from, DiskLayout::kFatEnd);
        status != DiskStatus::kOk) {
      return status;
    }
  }
  // The freed chain already ends where the free list is empty.
  if (head != 0) {
    if (DiskStatus status = change(chain_last, static_cast<uint16_t>(head));
        status != DiskStatus::kOk) {
      return status;
    }
  }
  if (DiskStatus status = change(0, static_cast<uint16_t>(chain_first));
      status != DiskStatus::kOk) {
    return status;
  }
  if (DiskStatus status = Write(); status != DiskStatus::kOk) {
    return status;
  }
  if (free_count_.has_value()) {
    *free_count_ += chain_count;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFat::GetNextCluster(int cluster, int* next) {
  uint16_t link = 0;
  if (DiskStatus status = ReadFatWord(cluster, &link);
      status != DiskStatus::kOk) {
    return status;
  }
  const std::optional<int> link_next = layout_.GetNextCluster(cluster, link);
  if (!link_next.has_value()) {
    return DiskStatus::kCorrupt;
  }
  *next = *link_next;
  return DiskStatus::kOk;
}

DiskStatus DiskFat::CountChain(int chain_first, int* chain_count,
                               int* chain_last) {
  // GetNextCluster checks every link, but not the first cluster.
  if (chain_first > layout_.cluster_count) {
    return DiskStatus::kCorrupt;
  }
  int counted = 0;
  int last = 0;
  for (int cluster = chain_first; cluster != 0;) {
    // A chain longer than the disk loops.
    if (++counted > layout_.cluster_count) {
      return DiskStatus::kCorrupt;
    }
    last = cluster;
    if (DiskStatus status = GetNextCluster(cluster, &cluster);
        status != DiskStatus::kOk) {
      return status;
    }
  }
  *chain_count = counted;
  if (chain_last != nullptr) {
    *chain_last = last;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskFat::LoadFatSector(int index, DiskSectorBuffer** buffer) {
  CHECK_GE(index, 0);
  CHECK_LE(index, layout_.cluster_count);
  const int sector = DiskLayout::GetFatSector(index);
  *buffer = (sector == DiskLayout::kFirstFatSector ? &head_ : &work_);
  if ((*buffer)->sector == sector) {
    return DiskStatus::kOk;
  }
  if (DiskStatus status = WriteBuffer(*buffer); status != DiskStatus::kOk) {
    return status;
  }
  (*buffer)->sector = -1;
  if (DiskStatus status =
          hardware_->ReadSector(sector, absl::MakeSpan((*buffer)->words));
      status != DiskStatus::kOk) {
    return status;
  }
  (*buffer)->sector = sector;
  return DiskStatus::kOk;
}

DiskStatus DiskFat::ReadFatWord(int index, uint16_t* value) {
  DiskSectorBuffer* buffer = nullptr;
  if (DiskStatus status = LoadFatSector(index, &buffer);
      status != DiskStatus::kOk) {
    return status;
  }
  *value = buffer->words[index % kDiskSectorWords];
  return DiskStatus::kOk;
}

DiskStatus DiskFat::SetFatWord(int index, uint16_t value) {
  DiskSectorBuffer* buffer = nullptr;
  if (DiskStatus status = LoadFatSector(index, &buffer);
      status != DiskStatus::kOk) {
    return status;
  }
  buffer->words[index % kDiskSectorWords] = value;
  buffer->dirty = true;
  return DiskStatus::kOk;
}

DiskStatus DiskFat::WriteBuffer(DiskSectorBuffer* buffer) {
  if (!buffer->dirty) {
    return DiskStatus::kOk;
  }
  if (buffer == &work_) {
    if (DiskStatus status = WriteBuffer(&head_); status != DiskStatus::kOk) {
      return status;
    }
  }
  if (DiskStatus status = hardware_->WriteSector(
          buffer->sector, buffer->words,
          buffer == &head_ ? WriteCaching::kSkip : WriteCaching::kCache);
      status != DiskStatus::kOk) {
    return status;
  }
  buffer->dirty = false;
  return DiskStatus::kOk;
}

}  // namespace oz3
