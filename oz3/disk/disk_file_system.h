// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_FILE_SYSTEM_H_
#define OZ3_DISK_DISK_FILE_SYSTEM_H_

#include <cstdint>
#include <memory>
#include <optional>
#include <vector>

#include "absl/functional/function_ref.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "oz3/core/date_time.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_fat.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_hardware.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_media.h"
#include "oz3/disk/disk_sector_buffer.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/disk_superblock.h"

namespace oz3 {

// The file system on a disk, as the drive's VOLUME command reports it.
struct DiskVolumeInfo {
  bool operator==(const DiskVolumeInfo&) const = default;

  DiskLabel label;
  int sectors_per_cluster = 0;
  int cluster_count = 0;
  int free_clusters = 0;
};

// A slot in a DiskFileSystem's open-file table, 0 to max_open_files - 1, as
// the drive's handles are.
using DiskFile = int;

// An open file, as the drive's INFO command reports it.
struct DiskFileInfo {
  bool operator==(const DiskFileInfo&) const = default;

  int position = 0;
  int size = 0;
};

// A drive's file system: the disk format in docs/disk_design.md, on whatever
// image is attached to its hardware. Every operation returns a DiskStatus, the
// drive's status codes, and kNoDisk while the drive is empty.
//
// The file system holds its own sector buffers, belonging to the attached
// image: one per open file, a directory sector, and the FAT's two (see
// DiskFat). They are read only when an operation first needs them. Detaching an
// image discards them, unwritten, as pulling a floppy would.
//
// Once the host fails to read or write the image, every operation returns
// kIoError until an image is attached, even one the buffers could answer
// (see DiskHardware). Close still frees the slot. A failed operation leaves its
// changes in the buffers, so the retry is to detach the image, discarding
// them, and attach it again reopened.
//
// A DiskFile is only a slot number, so a handle kept past its Close reaches
// whatever is opened in the slot next. Operations on a handle return
// kBadHandle for a slot out of range or with nothing open. A slot open when
// its image was detached is stale: every operation on it returns kDiskChanged,
// except Close, which frees it.
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
  int GetMaxOpenFiles() const { return static_cast<int>(slots_.size()); }

  //----------------------------------------------------------------------------
  // Image
  //----------------------------------------------------------------------------

  // Attaches `image`, which must not be null, detaching any image already
  // attached first, as DetachImage does. Reads the superblock, charging its
  // time, and returns kOk for a formatted disk, kUnformatted for one with no
  // valid file system (which can still be formatted), or kIoError, which fails
  // the image. The image stays attached for all three.
  DiskStatus AttachImage(std::unique_ptr<DiskImage> image);

  // Detaches and returns the image, or null if the drive is empty. The
  // buffers are discarded unwritten, so a host wanting the disk to hold
  // everything written calls Flush first.
  std::unique_ptr<DiskImage> DetachImage();

  // Returns whether an image is attached.
  bool HasImage() const { return hardware_.HasImage(); }

  // Returns the attached image's medium, formatted or not, or null if the
  // drive is empty. Its write protection is changed only while it is
  // detached, as a floppy's notch is.
  const DiskMedia* GetMedia() const { return hardware_.GetMedia(); }

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
  // kWriteProtected on a protected disk, kInUse if any file is open (stale
  // slots aside), and kBadArgument if `sectors_per_cluster` gives no layout.
  DiskStatus Format(const DiskLabel& label, int sectors_per_cluster,
                    DateTime now);

  // Rewrites the superblock's label. Returns kWriteProtected on a protected
  // disk, and kUnformatted with no file system.
  DiskStatus SetLabel(const DiskLabel& label);

  // Sets `info` to the file system, counting the free clusters if this is
  // the first operation since attaching to need them. Returns kUnformatted
  // with no file system, and kCorrupt for a broken free list.
  DiskStatus GetVolumeInfo(DiskVolumeInfo* info);

  //----------------------------------------------------------------------------
  // Files
  //----------------------------------------------------------------------------

  // Opens the file `name` in the root directory, in the lowest free slot,
  // setting `file` to the slot. GetFileInfo gives its size. On a writable
  // disk, counts the free clusters if nothing has yet, so writes to the file
  // can be promised without the disk. Returns kUnformatted with no file
  // system, kBadName for a pattern, kTooManyOpen with every slot taken,
  // kNotFound, kInUse if the file is already open, and kCorrupt for an entry
  // that can't be on the disk or a broken root chain or free list.
  DiskStatus Open(const DiskFilename& name, DiskFile* file);

  // Opens the root directory as Open does a file, including kInUse if it is
  // already open. Its size is its chain's full length.
  DiskStatus OpenRoot(DiskFile* file);

  // Creates the empty file `name` in the root directory, modified `now`, and
  // opens it as Open does. With no free entry, the root grows by a cluster.
  // Commits before returning, so the file is on the disk. Returns
  // kWriteProtected on a protected disk, kUnformatted with no file system,
  // kBadName for a pattern, kTooManyOpen with every slot taken, kExists if the
  // name is taken, kDiskFull if the root is full with no cluster free, and
  // kCorrupt for a broken root chain or free list.
  DiskStatus Create(const DiskFilename& name, DateTime now, DiskFile* file);

  // Commits `file` as Flush does, and closes it. The slot is always freed,
  // even when Close fails, so a handle is never left open. Returns the
  // commit's failure, and for a stale slot, kDiskChanged if it was written
  // since its last commit, as those writes are lost, and otherwise kOk.
  DiskStatus Close(DiskFile file, DateTime now);

  // Commits `file` if it was written since it was last stamped: its dirty
  // data sector, then the dirty FAT buffers (sector 1 first), then its
  // directory entry, with its size, first cluster, and `now` as the modified
  // time. A file not written since, and a directory handle, write nothing.
  DiskStatus Flush(DiskFile file, DateTime now);

  // Commits every open file as Flush does, so the disk holds everything
  // written, stopping at the first failure. Returns kUnformatted with no file
  // system.
  DiskStatus Flush(DateTime now);

  // Flushes `file` as Flush does, after setting its size to its position and
  // before freeing the clusters past those the size needs, along with any
  // chain left past the size. A Truncate that changes the file counts as a
  // write, so its entry is stamped `now` with the new size. The freeing writes
  // the FAT ending its chain, then the freed chain's link to the free list,
  // then sector 1 with the freed chain as the free list's head. Returns what
  // Write does before writing, and kCorrupt for a broken chain or free list.
  DiskStatus Truncate(DiskFile file, DateTime now);

  // Reads up to `words.size()` words at `file`'s position, but not past the end
  // of its sector or of the file, setting `count` to the words read and moving
  // the position past them. At the end, `count` is 0. Returns kCorrupt if the
  // file's chain is broken or shorter than its size.
  DiskStatus Read(DiskFile file, absl::Span<uint16_t> words, int* count);

  // Writes up to `words.size()` words at `file`'s position, but not past the
  // end of its sector, setting `count` to the words written, moving the
  // position past them, and growing the size to cover them. A cluster is
  // allocated when its first word is written, after following any chain left
  // past the size. The file's sector and the FAT are written when their
  // buffers take other sectors, or at Flush or Close. Returns kWrongHandleType
  // for a directory's handle, kReadOnly for a read-only file, kWriteProtected
  // on a protected disk, kDiskFull if the write needs a cluster and none is
  // free, and kCorrupt for a broken chain or free list.
  DiskStatus Write(DiskFile file, absl::Span<const uint16_t> words, int* count);

  // Sets `count` to the words writes at `file`'s position are sure to fit,
  // without the disk: those up to its size, the rest of its last cluster, and
  // every free cluster's. Returns what Write does before writing.
  DiskStatus GetWritableWords(DiskFile file, int* count);

  // Moves `file`'s position to `position`, without the disk. Returns kPastEnd
  // beyond the file's size, and kBadArgument for a negative position.
  DiskStatus Seek(DiskFile file, int position);

  // Sets `info` to `file`'s position and size.
  DiskStatus GetFileInfo(DiskFile file, DiskFileInfo* info);

  //----------------------------------------------------------------------------
  // Directory
  //----------------------------------------------------------------------------

  // Finds the next entry of `directory` whose name `pattern` matches, from the
  // first entry starting at or after its position, setting `entry` to it and
  // moving the position just past it. The entry is as it is on the disk, so
  // an open file's is as it was last committed. Free entries, and entries
  // whose names break the rules, never match. Returns kNotFound at the end,
  // leaving the position there, and kWrongHandleType for a file's handle.
  DiskStatus Find(DiskFile directory, const DiskFilename& pattern,
                  DiskDirEntry* entry);

  // Deletes the file `name` from the root directory, freeing its entry, and
  // then its chain onto the free list's head. Its chain is walked first, then
  // the free entry is written, and then the chain is freed as Truncate frees,
  // so a crash leaves its clusters at worst orphaned. Returns kWriteProtected
  // on a protected disk, kUnformatted with no file system, kBadName for a
  // pattern, kNotFound, kInUse if the file is open, kReadOnly for a read-only
  // file, and kCorrupt for an entry that can't be on the disk (as Open) or a
  // broken chain or free list.
  DiskStatus Delete(const DiskFilename& name);

  // Renames the file `old_name` in the root directory to `new_name`, in its
  // own entry, keeping everything else in the entry. With two entries named
  // `old_name`, it renames the first, which Open finds. Returns kWriteProtected
  // on a protected disk, kUnformatted with no file system, kBadName for a
  // pattern, kNotFound if `old_name` isn't found, kInUse if the file is open,
  // and kExists if `new_name` is taken, even by the file itself.
  DiskStatus Rename(const DiskFilename& old_name, const DiskFilename& new_name);

  // Sets the attributes of the file `name` in the root directory that
  // kDiskAttribSettable holds to `attributes`, keeping the rest, and writing
  // nothing if they don't change. Returns
  // kWriteProtected on a protected disk, kUnformatted with no file system,
  // kBadName for a pattern, kBadAttributes for any other bit, kNotFound, and
  // kInUse if the file is open.
  DiskStatus SetAttributes(const DiskFilename& name, uint16_t attributes);

 private:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // Whether a slot is in use.
  enum class SlotState {
    kFree,
    kOpen,
    kStale,  // Open when its image was detached.
  };

  // Where a directory entry is: its sector, and its index in the sector.
  struct EntryLocation {
    auto operator<=>(const EntryLocation&) const = default;

    int sector = -1;
    int index = 0;
  };

  // A slot in the open-file table.
  struct Slot {
    SlotState state = SlotState::kFree;
    // A file's directory entry, or none for the root.
    std::optional<EntryLocation> entry;
    bool read_only = false;  // The file's read-only attribute, at open.
    bool written_since_stamp = false;
    int size = 0;
    int position = 0;

    // The chain, from its first cluster (0 for an empty file), and the cluster
    // the slot last reached in it (0 for none), by its index in the chain.
    int first_cluster = 0;
    int last_cluster = 0;
    int last_cluster_index = 0;

    DiskSectorBuffer buffer;  // The sector at the position, once read.
  };

  //----------------------------------------------------------------------------
  // Volume
  //----------------------------------------------------------------------------

  // Forgets the attached disk's superblock, free count, and buffers.
  void ResetVolume();

  // Returns kNoDisk with no image, kIoError for a failed one, kUnformatted
  // with no file system, or else kOk.
  DiskStatus GetVolumeStatus() const;

  // Returns kNoDisk with no image, kIoError for a failed one, kWriteProtected
  // for a protected one, kUnformatted with no file system, or else kOk, as
  // every change to the disk checks first. Protection comes before
  // formatting, as the drive finds it at accept and formatting at completion.
  DiskStatus GetWritableStatus() const;

  // Returns a cluster's size, in words.
  int GetClusterSize() const;

  //----------------------------------------------------------------------------
  // Slots
  //----------------------------------------------------------------------------

  // Makes every open slot stale, as its image is detached.
  void InvalidateOpenFiles();

  // Sets `slot` to `file`'s slot if it is taken. Returns kBadHandle if it is
  // out of range or free, and with `slot` set, kDiskChanged if it is stale,
  // and kIoError if the image has failed.
  DiskStatus GetSlot(DiskFile file, Slot** slot);

  // Sets `slot` to `file`'s slot if it can be written, as GetSlot does.
  // Returns kWrongHandleType for a directory's slot, kReadOnly for a read-only
  // file, and kWriteProtected on a protected disk.
  DiskStatus GetWritableSlot(DiskFile file, Slot** slot);

  // Returns the lowest free slot, or -1 if every slot is taken.
  DiskFile FindFreeSlot() const;

  // Returns whether a slot is open on the file whose directory entry is
  // `entry`, or on the root for none.
  bool IsOpen(const std::optional<EntryLocation>& entry) const;

  // Opens `slot` on the chain from `first_cluster`, of `size` words, at
  // position 0, with its buffer empty. `entry` is the file's directory entry,
  // or none for the root.
  void OpenSlot(Slot* slot, const std::optional<EntryLocation>& entry,
                bool read_only, int first_cluster, int size);

  // Opens `slot` on the root directory, whose size is its chain's full length.
  // Returns kCorrupt for a broken root chain.
  DiskStatus OpenRootSlot(Slot* slot);

  // Commits `slot`, stamping it `now`, as Flush describes.
  DiskStatus CommitSlot(Slot* slot, DateTime now);

  // Moves `slot`'s last cluster reached to the one `cluster_index` clusters
  // into its chain, following the chain from the cluster last reached, or from
  // the start to go back. Returns kCorrupt if the chain is shorter.
  DiskStatus ReachCluster(Slot* slot, int cluster_index);

  // Moves `slot`'s last cluster reached to the one before `cluster_index` in
  // its chain, setting `previous` to it (0 at index 0), and `cluster` to the
  // one at `cluster_index` (0 past the chain's end). Returns kCorrupt if the
  // chain is shorter than `cluster_index`.
  DiskStatus ReachClusterBefore(Slot* slot, int cluster_index, int* previous,
                                int* cluster);

  // Moves `slot`'s last cluster reached to the cluster at its position, for a
  // write there. One that starts a cluster at the size is the next in any
  // chain left past the size, or else one allocated onto the chain's end.
  // Returns kDiskFull, changing nothing, if it needs a cluster and none is
  // free, and kCorrupt for a broken chain or free list.
  DiskStatus ReachWriteCluster(Slot* slot);

  //----------------------------------------------------------------------------
  // Sectors
  //----------------------------------------------------------------------------

  // Reads `sector` into `buffer`, unless it already holds it, first writing
  // the sector it held if it is dirty.
  DiskStatus LoadSector(int sector, DiskSectorBuffer* buffer);

  // Loads `sector` into `buffer` as all zeros, without reading it, and marks
  // it dirty, first writing the sector it held if it is dirty.
  DiskStatus BlankSector(int sector, DiskSectorBuffer* buffer);

  // Returns the sector at `slot`'s position, in the cluster it last reached,
  // which must be the position's.
  int GetSlotSector(const Slot& slot) const;

  // Loads the sector at `slot`'s position, which must be before its end, into
  // its buffer.
  DiskStatus LoadSlotSector(Slot* slot);

  // Writes `buffer`'s sector if it is dirty. Every write of a buffer but the
  // FAT's comes here, to keep the order disk_design.md's Committing gives,
  // with the FAT's order kept by DiskFat.
  //
  // A sector written is copied into every other buffer holding it, so an open
  // root handle sees each entry as it is written. That relies on no sector
  // being dirty in two buffers: only a file's own slot holds its data, and
  // dir_ is clean between operations.
  DiskStatus WriteBuffer(DiskSectorBuffer* buffer);

  //----------------------------------------------------------------------------
  // Directory
  //----------------------------------------------------------------------------

  // Opens dir_ on the root if it isn't open, or else moves its position back
  // to the start. It stays open, keeping its sector, until the volume is
  // reset.
  DiskStatus RewindDirectory();

  // Calls `visit` with each entry of the directory open in `slot`, free ones
  // included, and where it is, from the first entry starting at or after its
  // position, moving the position past each before the call. Returns kOk once
  // `visit` returns true, and kNotFound at the end, leaving the position
  // there.
  DiskStatus VisitEntries(
      Slot* slot,
      absl::FunctionRef<bool(const DiskDirEntry&, const EntryLocation&)> visit);

  // Finds the next entry of the directory open in `slot` whose name `matches`
  // accepts, from the first entry starting at or after its position, setting
  // `entry` to it, `location` (if not null) to where it is, and moving the
  // position just past it. Free entries, and entries whose names break the
  // rules, are skipped. Returns kNotFound at the end, leaving the position
  // there.
  DiskStatus FindNextEntry(Slot* slot,
                           absl::FunctionRef<bool(const DiskFilename&)> matches,
                           DiskDirEntry* entry, EntryLocation* location);

  // Finds the file `name` in the root directory, setting `entry` to its entry
  // and `location` to where it is. Returns kNotFound, and kInUse if the file is
  // open.
  DiskStatus FindClosedFile(const DiskFilename& name, DiskDirEntry* entry,
                            EntryLocation* location);

  // Returns kCorrupt for a file's entry that can't be on the disk: a size
  // past the disk's largest file, a first cluster past its last, or a first
  // cluster without a size or a size without one. Otherwise returns kOk.
  DiskStatus GetFileEntryStatus(const DiskDirEntry& entry) const;

  // Writes the dirty FAT buffers, then the directory buffer, as every change
  // to the directory does before returning.
  DiskStatus CommitDirectory();

  // Sets `entry` to the entry at `location`, loading its sector into dir_'s
  // buffer if needed.
  DiskStatus GetEntry(const EntryLocation& location, DiskDirEntry* entry);

  // Puts `entry` at `location` in dir_'s buffer, loading its sector if
  // needed, for CommitDirectory to write.
  DiskStatus PutEntry(const EntryLocation& location, const DiskDirEntry& entry);

  // Grows the root, open in dir_, by a cluster holding `entry` as its first
  // entry, setting `location` to it. The cluster's sectors, zeroed but for the
  // entry, are written before the link that adds it, as a directory has no
  // size to stop reads. The link is left for CommitDirectory to write. Grows
  // dir_ and every open root slot by the cluster. Returns kDiskFull, changing
  // nothing, with no cluster free.
  DiskStatus GrowRoot(const DiskDirEntry& entry, EntryLocation* location);

  //----------------------------------------------------------------------------
  // State
  //----------------------------------------------------------------------------

  DiskHardware hardware_;
  std::vector<Slot> slots_;
  bool formatted_ = false;

  // Only valid while formatted_.
  DiskSuperblock superblock_;
  std::optional<DiskFat> fat_;
  Slot dir_;  // The root, for finding names, open once needed.
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_FILE_SYSTEM_H_
