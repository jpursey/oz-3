// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_SUPERBLOCK_H_
#define OZ3_DISK_DISK_SUPERBLOCK_H_

#include <array>
#include <cstdint>
#include <optional>

#include "oz3/core/date_time.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_layout.h"

namespace oz3 {

// A disk's sector 0: the superblock's fields, in words 0-31, then the boot
// area. By word, the fields are the format ID, "OZFS" packed (0 and 1), the
// version (2), the layout (3-5), the root directory's first cluster (6),
// reserved flags (7), the volume label (8-15), and when it was formatted
// (16-19).
//
// It holds its words exactly, as the disk has them, so one read from a disk
// may describe no file system, which IsValid says. The getters say what it
// holds, and the setters change only their own words.
class DiskSuperblock final {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // The words of the sector, in order.
  using Words = std::array<uint16_t, kDiskSectorWords>;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // A blank sector, all zeros, which describes no file system.
  DiskSuperblock() = default;

  // The superblock formatting writes: the format ID and version, `layout`,
  // the root directory at `root_cluster`, `label`, and `formatted`, with every
  // other word 0. Every field must fit its words.
  DiskSuperblock(const DiskLayout& layout, int root_cluster,
                 const DiskLabel& label, DateTime formatted);

  // Returns `words` exactly as given.
  static DiskSuperblock FromWords(const Words& words) {
    return DiskSuperblock(words);
  }

  //----------------------------------------------------------------------------
  // Operators
  //----------------------------------------------------------------------------

  bool operator==(const DiskSuperblock&) const = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns whether it describes a file system on an image of `sector_count`
  // sectors: the format ID and version are known, the layout is valid for
  // the image (see DiskLayout::IsValid), the root cluster is in range, and the
  // label is valid.
  bool IsValid(int sector_count) const;

  // Returns the layout, as it is.
  DiskLayout GetLayout() const {
    return {
        .sectors_per_cluster = words_[kSectorsPerClusterWord],
        .cluster_count = words_[kClusterCountWord],
        .fat_sectors = words_[kFatSectorsWord],
    };
  }

  // Returns the root directory's first cluster, as it is.
  int GetRootCluster() const { return words_[kRootClusterWord]; }

  // Returns the volume label, or nullopt if it breaks the label rules.
  std::optional<DiskLabel> GetLabel() const;

  // Sets the volume label.
  void SetLabel(const DiskLabel& label);

  // Returns the words.
  const Words& GetWords() const { return words_; }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // Where each field starts.
  static constexpr int kFormatIdWord = 0;  // Two words.
  static constexpr int kVersionWord = 2;
  static constexpr int kSectorsPerClusterWord = 3;
  static constexpr int kClusterCountWord = 4;
  static constexpr int kFatSectorsWord = 5;
  static constexpr int kRootClusterWord = 6;
  static constexpr int kLabelWord = 8;
  static constexpr int kFormattedWord = 16;

  explicit DiskSuperblock(const Words& words) : words_(words) {}

  Words words_ = {};
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_SUPERBLOCK_H_
