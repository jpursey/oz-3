// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_CHECK_H_
#define OZ3_DISK_DISK_CHECK_H_

#include <vector>

#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_status.h"

namespace oz3 {

// What a check finds wrong with a disk. A crash leaves at most orphaned
// clusters and long chains (see disk_design.md's Committing).
//
// Each cluster belongs to the first chain to reach it: the root's, then each
// file's in directory order, bad names included, then the free list's. So a
// cross-link is the later file's problem, and a cluster both free and in a
// file stays the file's, with the free list broken there. A long chain's tail,
// past what its file's size needs, belongs to no chain.
struct DiskCheckReport {
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // What is wrong with a file's chain: the first problem along it.
  enum class ChainProblem {
    kLong,         // It goes on past the clusters its size needs.
    kShort,        // It ends before covering its size, which may be past the
                   // disk's largest file.
    kBroken,       // It has a cluster out of range, or loops.
    kCrossLinked,  // It runs into the root's chain or an earlier file's.
  };

  // A file whose chain doesn't match its size, with its entry as it is on
  // the disk.
  struct FileProblem {
    bool operator==(const FileProblem&) const = default;

    DiskDirEntry entry;
    ChainProblem problem = ChainProblem::kLong;
  };

  //----------------------------------------------------------------------------
  // Operators
  //----------------------------------------------------------------------------

  bool operator==(const DiskCheckReport&) const = default;

  //----------------------------------------------------------------------------
  // Data
  //----------------------------------------------------------------------------

  // The root's chain has a link out of range, or loops. Its entries are still
  // checked up to the break.
  bool broken_root = false;

  // The free list has a link out of range, loops, or runs into a chain.
  bool broken_free_list = false;

  // The clusters on no chain, and not free.
  int orphaned_clusters = 0;

  // The files whose chains don't match their sizes, in directory order.
  std::vector<FileProblem> file_problems;

  // The entries whose names break the name rules, in directory order. An
  // entry is free only if all its words are 0, so one whose name alone is
  // zeroed is here.
  std::vector<DiskDirEntry> bad_names;

  // The names more than one entry has, each once, in the order their second
  // entries are in the directory.
  std::vector<DiskFilename> duplicate_names;
};

// Sets `report` to what is wrong with the file system on `image`, writing
// nothing. Returns kUnformatted if the image has no file system, and kIoError
// if the host fails. Never returns kCorrupt, as corruption is what it reports.
DiskStatus CheckDisk(DiskImage& image, DiskCheckReport* report);

}  // namespace oz3

#endif  // OZ3_DISK_DISK_CHECK_H_
