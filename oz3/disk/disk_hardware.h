// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_HARDWARE_H_
#define OZ3_DISK_DISK_HARDWARE_H_

#include <array>
#include <cstdint>
#include <memory>
#include <vector>

#include "absl/time/time.h"
#include "absl/types/span.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_status.h"

namespace oz3 {

// How a DiskHardware behaves. The defaults cache nothing and take no time,
// which is what tools want.
struct DiskHardwareSettings {
  bool operator==(const DiskHardwareSettings&) const = default;

  // The sectors the read cache holds, or 0 for no cache. It must not be
  // negative.
  int read_cache_sectors = 0;

  // The rates sectors are read and written at, in words per second, or 0 to
  // take no time. They must not be negative.
  int64_t read_words_per_sec = 0;
  int64_t write_words_per_sec = 0;

  // The rate sectors are copied out of the read cache at, in words per second,
  // or 0 to take no time. It must not be negative.
  int64_t cache_words_per_sec = 0;

  // The time a transfer that isn't continuous with the previous one costs
  // first: a seek. It must not be negative.
  absl::Duration latency = absl::ZeroDuration();
};

// A dumb drive: sector reads and writes on whatever image is attached, through
// a write-through read cache, adding up the simulated time each takes. The
// drive owns its image while it is attached.
//
// Reading a sector costs kDiskSectorWords at the read rate, or at the cache
// rate if it is in the cache, with no latency. Writing a sector always costs
// kDiskSectorWords at the write rate. A transfer that isn't continuous with the
// previous one (a sector other than the next, or a switch between reading and
// writing) first costs the latency, and so does the first transfer. Cached
// reads don't move the head, so they don't change what is continuous. Only a
// transfer that succeeds takes time or moves the head.
class DiskHardware final {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // Whether a written sector is kept in the read cache.
  enum class WriteCaching {
    kCache,  // It may be read again, so it is cached as a read would be.
    kSkip,   // The caller keeps it, and won't read it from the disk. A cached
             // copy is still updated, but none is added.
  };

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Makes an empty drive, with no image. `settings` must be valid (see
  // DiskHardwareSettings).
  explicit DiskHardware(const DiskHardwareSettings& settings);
  DiskHardware(const DiskHardware&) = delete;
  DiskHardware& operator=(const DiskHardware&) = delete;
  ~DiskHardware() = default;

  //----------------------------------------------------------------------------
  // Properties
  //----------------------------------------------------------------------------

  const DiskHardwareSettings& GetSettings() const { return settings_; }

  //----------------------------------------------------------------------------
  // Image
  //----------------------------------------------------------------------------

  // Attaches `image`, destroying any image already attached. A null `image`
  // leaves the drive empty. Either way the cache is emptied, and the next
  // transfer costs the latency.
  void AttachImage(std::unique_ptr<DiskImage> image);

  // Detaches and returns the image, or null if the drive is empty. The cache
  // is emptied, and the next transfer costs the latency.
  std::unique_ptr<DiskImage> DetachImage();

  // Returns whether an image is attached.
  bool HasImage() const { return image_ != nullptr; }

  // Returns the attached image, for its medium (its sector count and flags),
  // or null if the drive is empty. Its sectors are only read and written
  // through the hardware, so the cache is never stale.
  DiskImage* GetImage() const { return image_.get(); }

  //----------------------------------------------------------------------------
  // Sectors
  //----------------------------------------------------------------------------

  // Reads sector `sector` into `words`, from the cache if it is there, or else
  // from the image, caching it. Returns kNoDisk if the drive is empty, and
  // otherwise what DiskImage::ReadSector does.
  DiskStatus ReadSector(int64_t sector, absl::Span<uint16_t> words);

  // Writes `words` to sector `sector` on the image, and then caches it as
  // `caching` says. Returns kNoDisk if the drive is empty, and otherwise what
  // DiskImage::WriteSector does.
  DiskStatus WriteSector(int64_t sector, absl::Span<const uint16_t> words,
                         WriteCaching caching);

  //----------------------------------------------------------------------------
  // Time
  //----------------------------------------------------------------------------

  // Returns the simulated time the reads and writes took since the last call,
  // and resets it.
  absl::Duration CollectBusyTime();

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // A direction of transfer, for continuity.
  enum class Transfer {
    kNone,  // Nothing yet, so the next transfer isn't continuous.
    kRead,
    kWrite,
  };

  // A sector in the read cache.
  struct CacheEntry {
    int64_t sector = -1;  // -1 for an empty entry.
    int64_t last_used = 0;
    std::array<uint16_t, kDiskSectorWords> words = {};
  };

  // Empties the cache, and forgets where the head is, as a disk change does.
  void Reset();

  // Returns the cache entry holding `sector`, or null.
  CacheEntry* FindCached(int64_t sector);

  // Puts `words` in the cache as `sector`, replacing the least recently used
  // entry if it isn't there. Does nothing with no cache.
  void AddCached(int64_t sector, absl::Span<const uint16_t> words);

  // Charges a transfer of `sector` in `transfer`'s direction, taking
  // `sector_time`, and moves the head there.
  void Charge(int64_t sector, Transfer transfer, absl::Duration sector_time);

  const DiskHardwareSettings settings_;
  std::unique_ptr<DiskImage> image_;
  const absl::Duration read_sector_time_;
  const absl::Duration write_sector_time_;
  const absl::Duration cache_sector_time_;
  std::vector<CacheEntry> cache_;
  int64_t use_count_ = 0;
  Transfer last_transfer_ = Transfer::kNone;
  int64_t last_sector_ = 0;
  absl::Duration busy_time_ = absl::ZeroDuration();
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_HARDWARE_H_
