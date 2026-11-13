// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_hardware.h"

#include <algorithm>
#include <utility>

#include "absl/log/check.h"

namespace oz3 {

namespace {

// Returns the time a sector takes at `words_per_sec`, or none for a rate of 0.
absl::Duration GetSectorTime(int64_t words_per_sec) {
  if (words_per_sec == 0) {
    return absl::ZeroDuration();
  }
  return absl::Seconds(kDiskSectorWords) / words_per_sec;
}

}  // namespace

DiskHardware::DiskHardware(const DiskHardwareSettings& settings)
    : settings_(settings),
      read_sector_time_(GetSectorTime(settings.read_words_per_sec)),
      write_sector_time_(GetSectorTime(settings.write_words_per_sec)),
      cache_sector_time_(GetSectorTime(settings.cache_words_per_sec)) {
  CHECK_GE(settings.read_cache_sectors, 0);
  CHECK_GE(settings.read_words_per_sec, 0);
  CHECK_GE(settings.write_words_per_sec, 0);
  CHECK_GE(settings.cache_words_per_sec, 0);
  CHECK_GE(settings.latency, absl::ZeroDuration());
  cache_.resize(settings.read_cache_sectors);
}

void DiskHardware::AttachImage(std::unique_ptr<DiskImage> image) {
  image_ = std::move(image);
  Reset();
}

std::unique_ptr<DiskImage> DiskHardware::DetachImage() {
  Reset();
  return std::move(image_);
}

DiskStatus DiskHardware::ReadSector(int sector, absl::Span<uint16_t> words) {
  if (image_ == nullptr) {
    return DiskStatus::kNoDisk;
  }
  if (words.size() != kDiskSectorWords) {
    return DiskStatus::kBadArgument;
  }
  if (CacheEntry* entry = FindCached(sector); entry != nullptr) {
    std::copy(entry->words.begin(), entry->words.end(), words.begin());
    entry->last_used = ++use_count_;
    busy_time_ += cache_sector_time_;
    return DiskStatus::kOk;
  }
  const DiskStatus status = image_->ReadSector(sector, words);
  if (status != DiskStatus::kOk) {
    return status;
  }
  Charge(sector, Transfer::kRead, read_sector_time_);
  AddCached(sector, words);
  return DiskStatus::kOk;
}

DiskStatus DiskHardware::WriteSector(int sector,
                                     absl::Span<const uint16_t> words,
                                     WriteCaching caching) {
  if (image_ == nullptr) {
    return DiskStatus::kNoDisk;
  }
  const DiskStatus status = image_->WriteSector(sector, words);
  if (status != DiskStatus::kOk) {
    return status;
  }
  Charge(sector, Transfer::kWrite, write_sector_time_);
  if (caching == WriteCaching::kCache) {
    AddCached(sector, words);
  } else if (CacheEntry* entry = FindCached(sector); entry != nullptr) {
    std::copy(words.begin(), words.end(), entry->words.begin());
  }
  return DiskStatus::kOk;
}

absl::Duration DiskHardware::CollectBusyTime() {
  return std::exchange(busy_time_, absl::ZeroDuration());
}

void DiskHardware::Reset() {
  for (CacheEntry& entry : cache_) {
    entry.sector = -1;
    entry.last_used = 0;
  }
  last_transfer_ = Transfer::kNone;
}

DiskHardware::CacheEntry* DiskHardware::FindCached(int sector) {
  for (CacheEntry& entry : cache_) {
    if (entry.sector == sector) {
      return &entry;
    }
  }
  return nullptr;
}

void DiskHardware::AddCached(int sector, absl::Span<const uint16_t> words) {
  if (cache_.empty()) {
    return;
  }
  CacheEntry* entry = FindCached(sector);
  if (entry == nullptr) {
    // Empty entries were never used, so they go first.
    entry = &*std::min_element(cache_.begin(), cache_.end(),
                               [](const CacheEntry& a, const CacheEntry& b) {
                                 return a.last_used < b.last_used;
                               });
    entry->sector = sector;
  }
  std::copy(words.begin(), words.end(), entry->words.begin());
  entry->last_used = ++use_count_;
}

void DiskHardware::Charge(int sector, Transfer transfer,
                          absl::Duration sector_time) {
  if (transfer != last_transfer_ || sector != last_sector_ + 1) {
    busy_time_ += settings_.latency;
  }
  busy_time_ += sector_time;
  last_transfer_ = transfer;
  last_sector_ = sector;
}

}  // namespace oz3
