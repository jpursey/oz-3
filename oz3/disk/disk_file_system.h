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
#include <vector>

#include "absl/functional/function_ref.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "oz3/core/date_time.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_filename.h"
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
// image: one per open file, a directory sector, the FAT's first sector (sector
// 1, with the free list's head), and one working FAT sector. They are read
// only when an operation first needs them. Detaching an image discards them,
// unwritten, as pulling a floppy would.
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
  // kWriteProtected on a protected disk, kInUse if any file is open (stale
  // slots aside), and kBadArgument if `sectors_per_cluster` gives no layout.
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

  // Closes `file`, freeing its slot. A stale slot is freed too.
  DiskStatus Close(DiskFile file);

  //----------------------------------------------------------------------------
  // Data
  //----------------------------------------------------------------------------

  // Reads up to `words.size()` words at `file`'s position, but not past the end
  // of its sector or of the file, setting `count` to the words read and moving
  // the position past them. At the end, `count` is 0. Returns kCorrupt if the
  // file's chain is broken or shorter than its size.
  DiskStatus Read(DiskFile file, absl::Span<uint16_t> words, int* count);

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
  // moving the position just past it. Free entries, and entries whose names
  // break the rules, never match. Returns kNotFound at the end, leaving the
  // position there, and kWrongHandleType for a file's handle.
  DiskStatus Find(DiskFile directory, const DiskFilename& pattern,
                  DiskDirEntry* entry);

 private:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // A sector's words.
  using Sector = std::array<uint16_t, kDiskSectorWords>;

  // A buffer holding one sector.
  struct SectorBuffer {
    // Makes the buffer hold no sector, discarding any change, without
    // clearing its words.
    void Reset() {
      sector = -1;
      dirty = false;
    }

    int sector = -1;     // -1 for none.
    bool dirty = false;  // Changed since it was read or written.
    Sector words = {};
  };

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
    int size = 0;
    int position = 0;

    // The chain, from its first cluster (0 for an empty file), and the cluster
    // the slot last reached in it (0 for none), by its index in the chain.
    int first_cluster = 0;
    int last_cluster = 0;
    int last_cluster_index = 0;

    SectorBuffer buffer;  // The sector at the position, once read.
  };

  //----------------------------------------------------------------------------
  // Superblock
  //----------------------------------------------------------------------------

  // Return a superblock's fields, as they are. The label is nullopt if it
  // isn't valid.
  static DiskLayout GetLayout(const Sector& superblock);
  static int GetRootCluster(const Sector& superblock);
  static std::optional<DiskLabel> GetLabel(const Sector& superblock);

  // Sets `superblock`'s label to `label`.
  static void PutLabel(const DiskLabel& label, Sector* superblock);

  // Returns whether `superblock` describes a file system on an image of
  // `sector_count` sectors, with every field valid.
  static bool IsValidSuperblock(const Sector& superblock, int sector_count);

  //----------------------------------------------------------------------------
  // Volume
  //----------------------------------------------------------------------------

  // Forgets the attached disk's superblock, free count, and buffers.
  void ResetVolume();

  // Forgets the free count and the directory and FAT buffers, unwritten, so
  // they are read again when next needed, as after a change that failed
  // partway.
  void DiscardBuffers();

  // Returns kNoDisk with no image, kUnformatted with no file system, or else
  // kOk.
  DiskStatus GetVolumeStatus() const;

  // Returns kNoDisk with no image, kWriteProtected for a protected one, or
  // else kOk, as every change to the disk checks first.
  DiskStatus GetWritableStatus() const;

  // Returns a cluster's size, in words.
  int GetClusterSize() const;

  // Sets free_count_, if it isn't already, by walking the free list.
  DiskStatus CountFreeClusters();

  //----------------------------------------------------------------------------
  // Slots
  //----------------------------------------------------------------------------

  // Makes every open slot stale, as its image is detached.
  void InvalidateOpenFiles();

  // Sets `slot` to `file`'s slot if it is taken. Returns kBadHandle if it is
  // out of range or free, and kDiskChanged if it is stale, with `slot` set.
  DiskStatus GetSlot(DiskFile file, Slot** slot);

  // Returns the lowest free slot, or -1 if every slot is taken.
  DiskFile FindFreeSlot() const;

  // Returns whether a slot is open on the file whose directory entry is
  // `entry`, or on the root for none.
  bool IsOpen(const std::optional<EntryLocation>& entry) const;

  // Opens `slot` on the chain from `first_cluster`, of `size` words, at
  // position 0, with its buffer empty. `entry` is the file's directory entry,
  // or none for the root.
  void OpenSlot(Slot* slot, const std::optional<EntryLocation>& entry,
                int first_cluster, int size);

  // Opens `slot` on the root directory, whose size is its chain's full length.
  // Returns kCorrupt for a broken root chain.
  DiskStatus OpenRootSlot(Slot* slot);

  // Moves `slot`'s last cluster reached to the one `cluster_index` clusters
  // into its chain, following the chain from the cluster last reached, or from
  // the start to go back. Returns kCorrupt if the chain is shorter.
  DiskStatus ReachCluster(Slot* slot, int cluster_index);

  //----------------------------------------------------------------------------
  // Sectors
  //----------------------------------------------------------------------------

  // Reads `sector` into `buffer`, unless it already holds it, first writing
  // the sector it held if it is dirty.
  DiskStatus LoadSector(int sector, SectorBuffer* buffer);

  // Loads `sector` into `buffer` as all zeros, without reading it, and marks
  // it dirty, first writing the sector it held if it is dirty.
  DiskStatus BlankSector(int sector, SectorBuffer* buffer);

  // Loads the sector at `slot`'s position, which must be before its end, into
  // its buffer.
  DiskStatus LoadSlotSector(Slot* slot);

  // Writes `buffer`'s sector if it is dirty. Every write of a buffer comes
  // here, to keep the order disk_design.md's Committing gives.
  //
  // Only allocating leaves the FAT buffers dirty past the operation that
  // changed them, so the order kept here is allocating's: the working FAT
  // sector's write first writes sector 1 if it is dirty, so the free list's
  // head reaches the disk before a cluster taken from it is linked to.
  // Freeing writes its own order, starting with every FAT buffer clean.
  //
  // Sector 1 isn't cached, as its buffer always holds it. A sector written is
  // copied into every other buffer holding it, so an open root handle sees
  // each entry as it is written. That relies on no sector being dirty in two
  // buffers: only a file's own slot holds its data, and dir_ is clean between
  // operations.
  DiskStatus WriteBuffer(SectorBuffer* buffer);

  //----------------------------------------------------------------------------
  // FAT
  //----------------------------------------------------------------------------

  // Writes the dirty FAT buffers, sector 1 first.
  DiskStatus WriteFat();

  // Sets `buffer` to the FAT buffer for the FAT's word `index`, reading its
  // sector into it if needed.
  DiskStatus LoadFatSector(int index, SectorBuffer** buffer);

  // Sets `value` to the FAT's word `index`, from its buffer, reading the
  // sector into it if needed.
  DiskStatus ReadFatWord(int index, uint16_t* value);

  // Sets the FAT's word `index` to `value`, in its buffer, reading the sector
  // into it if needed.
  DiskStatus SetFatWord(int index, uint16_t value);

  // Sets `next` to the cluster after `cluster` in its chain, or 0 at the
  // chain's end. A `cluster` of 0 gives the free list's head. Returns kCorrupt
  // for a link out of range.
  DiskStatus GetNextCluster(int cluster, int* next);

  // Sets `count` to the clusters in the chain from `first_cluster`. Returns
  // kCorrupt for a broken chain, or one longer than the disk (a loop).
  DiskStatus CountChain(int first_cluster, int* count);

  // Sets `head` to the free list's head, which free_count_ must be set for.
  // Returns kDiskFull with no cluster free, and kCorrupt for a broken link or
  // an empty list the count says isn't.
  DiskStatus GetFreeHead(int* head);

  // Takes the free list's head, ends a chain with it, and links it after
  // `last_cluster`, unless that is 0 for a new chain, setting `cluster` to it.
  // The cluster's end reaches the disk before the link to it, so a crash can't
  // leave the chain running on into the free list. Returns kDiskFull, changing
  // nothing, with no cluster free, and kCorrupt for a broken free list.
  DiskStatus AllocateCluster(int last_cluster, int* cluster);

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

  // Writes the dirty FAT buffers, then the directory buffer, as every change
  // to the directory does before returning.
  DiskStatus CommitDirectory();

  // Puts `entry` at `location` in dir_'s buffer, loading its sector if
  // needed, for CommitDirectory to write.
  DiskStatus PutEntry(const EntryLocation& location, const DiskDirEntry& entry);

  // Grows the root, open in dir_, by a cluster holding `entry` as its first
  // entry, setting `location` to it. The cluster's sectors, zeroed but for the
  // entry, are written before the link that adds it, as a directory has no
  // size to stop reads, and dir_ and any open root handle take the new size.
  // The link is left for CommitDirectory to write. Returns kDiskFull, changing
  // nothing, with no cluster free.
  DiskStatus GrowRoot(const DiskDirEntry& entry, EntryLocation* location);

  //----------------------------------------------------------------------------
  // State
  //----------------------------------------------------------------------------

  DiskHardware hardware_;
  std::vector<Slot> slots_;
  bool formatted_ = false;

  // Only valid while formatted_.
  Sector superblock_ = {};
  std::optional<int> free_count_;
  Slot dir_;               // The root, for finding names, open once needed.
  SectorBuffer fat_head_;  // Sector 1 only.
  SectorBuffer fat_work_;  // Any other FAT sector.
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_FILE_SYSTEM_H_
