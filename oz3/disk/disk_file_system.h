// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_FILE_SYSTEM_H_
#define OZ3_DISK_DISK_FILE_SYSTEM_H_

#include <array>
#include <cstdint>
#include <memory>
#include <optional>

#include "absl/time/time.h"
#include "oz3/core/date_time.h"
#include "oz3/disk/disk_hardware.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_media.h"
#include "oz3/disk/disk_status.h"

namespace oz3 {

// The file system on a disk, as the drive's VOLUME command reports it.
struct DiskVolumeInfo {
  bool operator==(const DiskVolumeInfo&) const = default;

  DiskLabel label;
  int sectors_per_cluster = 0;
  int cluster_count = 0;
  int free_clusters = 0;
};

// A drive's file system: the disk format in docs/disk_design.md, on whatever
// image is attached to its hardware. Every operation returns a DiskStatus, the
// drive's status codes, and kNoDisk while the drive is empty.
//
// The file system holds its own sector buffers, belonging to the attached
// image: the FAT's first sector (sector 1, with the free list's head) and one
// working FAT sector. They are read only when an operation first needs them.
// Detaching an image discards them, unwritten, as pulling a floppy would.
//
// It is called from one thread at a time.
class DiskFileSystem final {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Makes an empty drive, with no image, whose hardware has `settings` (which
  // must be valid; see DiskHardwareSettings). Up to `max_open_files` files may
  // be open at once, which must be at least 1.
  DiskFileSystem(const DiskHardwareSettings& settings, int max_open_files);
  DiskFileSystem(const DiskFileSystem&) = delete;
  DiskFileSystem& operator=(const DiskFileSystem&) = delete;
  ~DiskFileSystem() = default;

  //----------------------------------------------------------------------------
  // Properties
  //----------------------------------------------------------------------------

  const DiskHardwareSettings& GetSettings() const {
    return hardware_.GetSettings();
  }
  int GetMaxOpenFiles() const { return max_open_files_; }

  //----------------------------------------------------------------------------
  // Image
  //----------------------------------------------------------------------------

  // Attaches `image`, which must not be null, detaching any image already
  // attached first. Reads the superblock, charging its time, and returns kOk
  // for a formatted disk, kUnformatted for one with no valid file system (which
  // can still be formatted), or kIoError. The image stays attached for all
  // three.
  DiskStatus AttachImage(std::unique_ptr<DiskImage> image);

  // Detaches and returns the image, or null if the drive is empty. The
  // buffers are discarded unwritten.
  std::unique_ptr<DiskImage> DetachImage();

  // Returns whether an image is attached.
  bool HasImage() const { return hardware_.HasImage(); }

  // Returns the attached image's medium, formatted or not, or null if the
  // drive is empty.
  const DiskMedia* GetMedia() const { return hardware_.GetMedia(); }
  DiskMedia* GetMedia() { return hardware_.GetMedia(); }

  //----------------------------------------------------------------------------
  // Time
  //----------------------------------------------------------------------------

  // Returns the simulated time the disk spent on operations since the last
  // call, and resets it.
  absl::Duration CollectBusyTime();

  //----------------------------------------------------------------------------
  // Volume
  //----------------------------------------------------------------------------

  // Formats the disk with `label`, stamped `now`, over the image's whole
  // sector count, with `sectors_per_cluster`, or 0 to pick the fewest that
  // cover it (see ChooseDiskLayout). Blanks the superblock, writes the zeroed
  // FAT and the root directory's zeroed cluster, then writes the superblock,
  // so a format that fails partway leaves the disk unformatted. Returns
  // kWriteProtected on a protected disk, and kBadArgument if
  // `sectors_per_cluster` gives no layout.
  DiskStatus Format(const DiskLabel& label, int sectors_per_cluster,
                    DateTime now);

  // Rewrites the superblock's label. Returns kWriteProtected on a protected
  // disk, and kUnformatted with no file system. If the write fails, the file
  // system keeps the label it had.
  DiskStatus SetLabel(const DiskLabel& label);

  // Sets `info` to the file system, counting the free clusters if this is
  // the first operation since attaching to need them. Returns kUnformatted
  // with no file system, and kCorrupt for a broken free list.
  DiskStatus GetVolumeInfo(DiskVolumeInfo* info);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // A sector's words.
  using Sector = std::array<uint16_t, kDiskSectorWords>;

  // A buffer holding one sector.
  struct SectorBuffer {
    int64_t sector = -1;  // -1 for none.
    Sector words = {};
  };

  // Return a superblock's fields, as they are. The label is nullopt if it
  // isn't valid.
  static DiskLayout GetLayout(const Sector& superblock);
  static int GetRootCluster(const Sector& superblock);
  static std::optional<DiskLabel> GetLabel(const Sector& superblock);

  // Sets `superblock`'s label to `label`.
  static void PutLabel(const DiskLabel& label, Sector* superblock);

  // Returns whether `superblock` describes a file system on an image of
  // `sector_count` sectors, with every field valid.
  static bool IsValidSuperblock(const Sector& superblock, int64_t sector_count);

  // Forgets the attached disk's superblock, free count, and buffers.
  void ResetVolume();

  // Returns kNoDisk with no image, kWriteProtected for a protected one, or
  // else kOk, as every change to the disk checks first.
  DiskStatus CheckWritable() const;

  // Sets `value` to the FAT's word `index`, from its buffer, reading the
  // sector into it if needed.
  DiskStatus ReadFatWord(int index, uint16_t* value);

  // Sets free_count_, if it isn't already, by walking the free list.
  DiskStatus CountFreeClusters();

  const int max_open_files_;
  DiskHardware hardware_;
  bool formatted_ = false;

  // Only valid while formatted_.
  Sector superblock_ = {};
  std::optional<int> free_count_;
  SectorBuffer fat_head_;  // Sector 1 only.
  SectorBuffer fat_work_;  // Any other FAT sector.
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_FILE_SYSTEM_H_
