// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_FAT_H_
#define OZ3_DISK_DISK_FAT_H_

#include <cstdint>
#include <optional>

#include "oz3/disk/disk_hardware.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_sector_buffer.h"
#include "oz3/disk/disk_status.h"

namespace oz3 {

// The FAT and free list of the disk attached to a DiskFileSystem's hardware,
// through two sector buffers: the FAT's first sector (sector 1, with the free
// list's head), and one working sector for any other. They are read only when
// an operation first needs them. The file system makes one for each disk it
// formats or attaches, and drops it, with its buffers unwritten, when the disk
// goes.
//
// Only allocating leaves the buffers dirty past the operation that changed
// them, so the order its writes keep is allocating's: the working sector's
// write first writes sector 1 if it is dirty, so the free list's head reaches
// the disk before a cluster taken from it is linked to. Freeing writes its own
// order, starting with both buffers clean. Sector 1 isn't cached, as its
// buffer always holds it.
//
// FAT sectors are only ever in these buffers, so nothing else needs its writes.
class DiskFat final {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Makes the FAT of the disk of `layout` attached to `hardware`, which must
  // outlive it, with nothing read and the free count not yet counted.
  DiskFat(DiskHardware* hardware, const DiskLayout& layout)
      : hardware_(hardware), layout_(layout) {}
  DiskFat(const DiskFat&) = delete;
  DiskFat& operator=(const DiskFat&) = delete;
  ~DiskFat() = default;

  //----------------------------------------------------------------------------
  // Disk
  //----------------------------------------------------------------------------

  // Writes a formatted disk's FAT, with every cluster free but the one the
  // root directory takes, as allocating would, setting `root_cluster` to it.
  // Writes sector 1 first, uncached, then the other FAT sectors, zeroed. Sets
  // the free count. Nothing may have read the FAT before it.
  DiskStatus Format(int* root_cluster);

  // Writes the dirty buffers, sector 1 first.
  DiskStatus Write();

  //----------------------------------------------------------------------------
  // Free list
  //----------------------------------------------------------------------------

  // Sets the free count, if it isn't already, by walking the free list.
  // Returns kCorrupt for a broken free list.
  DiskStatus CountFreeClusters();

  // Returns the free clusters, or nullopt if they haven't been counted.
  std::optional<int> GetFreeCount() const { return free_count_; }

  // Sets `head` to the free list's head, which the free count must be set
  // for. Returns kDiskFull with no cluster free, and kCorrupt for a broken
  // link or an empty list the count says isn't.
  DiskStatus GetFreeHead(int* head);

  // Takes the free list's head, ends a chain with it, and links it after
  // `last_cluster`, unless that is 0 for a new chain, setting `cluster` to it.
  // The cluster's end reaches the disk before the link to it, so a crash can't
  // leave the chain running on into the free list. The free count must be
  // set. Returns kDiskFull, changing nothing, with no cluster free, and
  // kCorrupt for a broken free list.
  DiskStatus AllocateCluster(int last_cluster, int* cluster);

  // Frees the `chain_count` clusters from `chain_first` to `chain_last` onto
  // the free list's head, unlinking them from the cluster `unlink_from` links
  // them from, or from nothing for an `unlink_from` of 0 (a whole chain), and
  // adds them to the free count if it is set. A `chain_count` of 0 frees
  // nothing. Both buffers must be clean, as the chain's entry must already be
  // on the disk without the clusters. Writes unlink_from's new end, then the
  // link from chain_last to the old head, then the new head, with one write
  // for changes next to each other in a sector, so a crash leaves the
  // clusters at worst orphaned, on neither the chain nor the free list.
  DiskStatus FreeChain(int unlink_from, int chain_first, int chain_last,
                       int chain_count);

  //----------------------------------------------------------------------------
  // Chains
  //----------------------------------------------------------------------------

  // Sets `next` to the cluster after `cluster` in its chain, or 0 at the
  // chain's end. A `cluster` of 0 gives the free list's head. Returns kCorrupt
  // for a link out of range.
  DiskStatus GetNextCluster(int cluster, int* next);

  // Sets `chain_count` to the clusters in the chain from `chain_first`, and
  // `chain_last` (if not null) to its last, or 0 for an empty chain. Returns
  // kCorrupt for a broken chain, starting at or linking to a cluster out of
  // range, or one longer than the disk (a loop).
  DiskStatus CountChain(int chain_first, int* chain_count, int* chain_last);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // Sets `buffer` to the buffer for the FAT's word `index`, reading its sector
  // into it if needed, first writing the sector it held if it is dirty.
  DiskStatus LoadFatSector(int index, DiskSectorBuffer** buffer);

  // Sets `value` to the FAT's word `index`, from its buffer.
  DiskStatus ReadFatWord(int index, uint16_t* value);

  // Sets the FAT's word `index` to `value`, in its buffer.
  DiskStatus SetFatWord(int index, uint16_t value);

  // Writes `buffer`'s sector if it is dirty, writing sector 1 first for the
  // working buffer. Every write of a buffer comes here, to keep the order.
  DiskStatus WriteBuffer(DiskSectorBuffer* buffer);

  //----------------------------------------------------------------------------
  // State
  //----------------------------------------------------------------------------

  DiskHardware* const hardware_;
  const DiskLayout layout_;
  std::optional<int> free_count_;
  DiskSectorBuffer head_;  // Sector 1 only.
  DiskSectorBuffer work_;  // Any other FAT sector.
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_FAT_H_
