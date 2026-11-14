# Disk library

The new `oz3/disk` library (`oz3_disk`) is the one implementation of the disk
format in [disk_design.md](../disk_design.md): disk images, the dumb drive's
cache and timing under them, and the file system on top, with names,
timestamps, directory entries, and consistency checks.
The drive device, `DiskFileProtocol`, and `oz3dsk` are all built on it later
(the backlog's *Disk drive device* and *Disk tools*), so they can never
disagree about what is on a disk.

There is no device and no tool yet, so nothing an OZ-3 program can see changes,
and no wiki page changes: the image and file system format are specified on
the drive's wiki page, which *Disk drive device* writes. The library is tested
on its own, over images in a `gb::MemoryFileProtocol`.

## Behavior

The library is everything disk_design.md's Disk image, File system, and Disk
library sections describe, so this plan doesn't repeat them. What it settles
is how code calls it:

- **Results are `DiskStatus`,** the drive's status codes, `kOk` included,
  returned by every file system and hardware operation, with results through
  out parameters. The drive passes them straight through as statuses, so the
  library can't report something the drive has no status for. Opening and
  creating an image is the one exception: it is the host's business, so it
  returns an `absl::StatusOr` whose message says what was wrong with the file.
- **Images come and go.** A `DiskFileSystem` is made with its hardware
  settings and open-file limit, and no image: an empty drive, where every
  operation on the disk is `kNoDisk`. It makes its `DiskHardware` from the
  settings once, empty too. `AttachImage` attaches an image to the hardware,
  and reads the superblock, returning `kOk`, `kUnformatted` (which can still
  be formatted), or `kIoError`. `DetachImage` detaches and returns the image,
  emptying the hardware's cache and discarding every buffer unwritten, as
  pulling a floppy would; a host wanting it consistent calls
  `Flush` first. Attaching while an image is attached detaches it first.
- **Stale handles are the library's.** A file open when its image is
  detached stays open but stale: every operation on it is `kDiskChanged`,
  except `Close`, which frees it (from CL11, failing with `kDiskChanged` if
  it had uncommitted writes, which are lost). So the drive's slots map one
  to one onto handles with no state of their own, and a tool swapping images
  can't write into the wrong disk either. disk_design.md's Disk library
  section changes to match, in CL6.
- **Time comes in as values.** `Format`, `Create`, `Close`, and `Flush` take
  the `DateTime` to stamp with. A file is stamped when it is created, so no
  entry on a disk has a zero time unless something wrote it so, and a file
  written since it was last stamped is stamped again at `Close` or `Flush`. So the drive passes its host clock's time,
  and a tool copying a host file in passes that file's time at `Close`.
- **Busy time** is the simulated time the disk spent on the operations since
  it was last collected. It accumulates as an `absl::Duration`, and
  `DiskFileSystem::CollectBusyTime()` returns it and resets it. Today it is
  only the hardware's sector transfers, but it belongs to the file system, so
  it could charge time of its own later. The drive turns it into cycles with
  its clock rate, as devices turn their other real-time rates.
- **The hardware is private.** Nothing outside needs `DiskHardware` but its
  settings and time, so the file system doesn't expose it. What callers need
  of the image is its medium, which the file system's `GetMedia` returns
  (null with no image): the sector count and header flags, as `MEDIA`
  reports them, without a file system on the disk. It is const: write
  protection is the floppy's notch, changed only with the disk out of the
  drive, so `oz3dsk protect` calls `SetWriteProtected` on an image no drive
  has attached (from CL9).
- **Writes allocate as they go.** `Write` allocates each cluster when its
  first word is written, after following any chain already past the file's
  size, rather than allocating a whole request's clusters up front.
  `GetWritableWords(file)` gives, without the disk, how much a write can be
  promised: the words from its position to its size, the room left in its
  last cluster, and the free clusters'. Only the drive changes the free
  count, so the promise holds. That leaves
  nothing to free after a write that stops short, and no begin and end calls
  to pair. disk_design.md's `WRITE` walkthrough (allocating right after
  accept) changes to match, in CL11.

## Design

### Names

Every public name has a `Disk` prefix, as disk_design.md chose, so none is
confused with Game Bits' `gb::File`, `gb::FileSystem`, `gb::RawFile`, or
`gb::FileProtocol`, or with OZ-3's `Program` and its files.

- **`DiskImage`**, **`DiskHardware`**, **`DiskFileSystem`**, **`DiskFilename`**,
  **`DiskDirEntry`**, and **`DiskFile`**, as disk_design.md's table names
  them.
- **`DiskStatus`**, for disk_design.md's `DiskError`, since `kOk` is one of
  its values. It matches the drive's "status codes" exactly. Alternatives:
  `DiskResult` (reads as a value carrying a result, like `absl::StatusOr`),
  keeping `DiskError` (wrong for `kOk`). It is prefixed, so it isn't confused
  with `absl::Status`. Recommended: `DiskStatus`. disk_design.md changes to
  match, in CL2.
- **`AttachImage`** and **`DetachImage`**, as disk_design.md's Images and
  disk changes says attaching and detaching. Alternatives: `Mount` and
  `Unmount` (but an unformatted image is attached with nothing to mount),
  `Insert` and `Eject` (the drive's words, not a file system's).
- **`DiskMedia`:** the medium, from the image's header: its sector count,
  flags, and write protection. It is `DiskImage`'s base, so the hardware and
  file system hand it out without exposing the image's sectors, and a new
  header field reaches every caller without changing them.
- **`DiskVolumeInfo`** and **`DiskFileInfo`:** each `...Info` is a snapshot
  of what one layer reports: the file system, and an open file.
- **`DiskLabel`:** the volume label, a value like `DiskFilename` but one
  16-character field with no extension. Alternatives: a `DiskFilename` mode
  (but a label isn't a name and can't be matched), or validating functions on
  raw words (leaving every caller to carry an unchecked array). Recommended:
  `DiskLabel`. A label isn't specific to a volume; a volume just has one, and
  anything else with a label would use the same type. So it isn't
  `DiskVolumeLabel`, and with only one label, `SetLabel` needs no `Volume`.
- **`DiskHardwareSettings`:** the cache size, read and write rates, and
  latency, as a device's `<Device>Settings` holds its behavior.
- **`DiskVolumeInfo`**, from `GetVolumeInfo`: the file system the superblock
  describes (the label, sectors per cluster, cluster count, and free
  clusters), as against the medium `DiskMedia` describes. "Disk" is the
  whole thing, image and all, so "volume" names the file system on it, as the
  superblock's volume label already does. The drive's `DISK` command becomes
  `VOLUME` to match (same opcode, 41h), in disk_design.md, in CL5.
  Alternatives: `DiskVolume` from `GetVolume` (shorter, but reads as an
  object you could act on rather than a snapshot), `DiskSuperblock` (the
  on-disk structure, not what callers care about). Recommended:
  `DiskVolumeInfo`. There is no `IsFormatted`: `AttachImage` already says
  `kUnformatted`, and after that `GetVolumeInfo` does, since the superblock
  it read at attach is what either would answer from.
- **`DiskFileInfo`:** a handle's position and size, as `INFO` reports them.
- **`DiskLayout`:** what a format chooses (sectors per cluster, cluster count,
  FAT sectors), from `ChooseDiskLayout`. Alternatives: "geometry" (suggests
  tracks and heads, which the disk doesn't have), "format" (the operation).
- **`DiskSuperblock`:** sector 0's words, with getters and setters, as
  `DiskDirEntry` is an entry's. Alternatives: `DiskVolumeHeader` (but
  "header" is the image's), keeping static functions in `DiskFileSystem`
  (which the checks couldn't share). Recommended: `DiskSuperblock`, the
  format's own word for it.
- **`DiskFat`:** the file system's FAT and free list, through its two FAT
  buffers. Alternatives: `DiskAllocator` (but it also walks chains),
  `DiskClusterTable` (long, and not the format's word). Recommended:
  `DiskFat`.
- **`CheckDisk`**, **`RepairDisk`**, and **`DiskCheckReport`:** what a check
  finds wrong with an image, and fixes. "Check" is the one exception to
  "Validate" for judging: a disk check (chkdsk) is a known thing, and it
  gives a full report rather than a pass or a status.
- **`CollectBusyTime()`:** returns the busy time since the last call, and
  resets it. "Busy time" says it's the disk's simulated time, not the
  caller's, and "collect" that it's gathered up and cleared, as a fare is.
  Alternatives: `DrainBusyTime()` (says plainly it empties, but reads oddly
  for time), `GetElapsed()` with a separate `ResetElapsed()` (a pair to
  remember). Recommended: `CollectBusyTime()`. The hardware has the same
  call, which the file system's adds to its own.
- Constants: `kDiskSectorWords` (256), and each value's own size,
  `DiskFilename::kWordCount` (8), `DiskLabel::kWordCount` (8), and
  `DiskDirEntry::kWordCount` (16). Names and labels being the same size is
  happenstance, so each has its own.

Words are `uint16_t`. Sector numbers, positions, sizes, and cluster numbers
are `int`: an image has at most the sectors a file system can use, so the
largest fits with room to spare (see CL7). Byte offsets in an image file are
`int64_t`, as the largest image is just over 2 GiB.

### core: DateTime

The timer and the disk share one layout for a date and time, so it is one
value in core, small enough not to need a library of its own:

```
// A date and time as the machine sees it, in four words: the year, the month
// (high byte) and day (low byte), the hour, and the minute (high byte) and
// second (low byte). All zeros is no date, since month 0 is invalid. The
// words compare in order chronologically.
class DateTime {
 public:
  DateTime() = default;  // No date.

  // The year keeps its low 16 bits, as the timer always has.
  static DateTime FromCivil(absl::CivilSecond time);
  static DateTime FromWords(const std::array<uint16_t, 4>& words);

  // nullopt with no date (month 0).
  std::optional<absl::CivilSecond> ToCivil() const;
  bool IsSet() const;
  const std::array<uint16_t, 4>& GetWords() const;

  auto operator<=>(const DateTime&) const = default;
};
```

- Names: `DateTime` (what it is, and the timer's `kTimerDate` and
  `kTimerTime` are its halves). Alternatives: `Timestamp` (suits the disk,
  but the timer's is a reading, not a stamp), `ClockTime` (could be read as
  the processor's clock). Recommended: `DateTime`.
- The timer latches a `DateTime` (no clock latches the default) and answers
  with its words, so the timer and the disk can't drift apart.
- `FromWords` keeps what it is given, so `ToCivil` of words a program wrote
  can be nullopt; tools showing a date check it.

### disk: DiskImage

```
// The medium, as an image's header describes it. Only DiskImage derives from
// it, setting the header's flags through WriteFlags.
class DiskMedia {
 public:
  int GetSectorCount() const;
  uint16_t GetFlags() const;  // The header's flags word, as MEDIA reports it.
  bool IsWriteProtected() const;

  // Sets the header's write-protect bit, writing nothing if it is unchanged.
  // Fails with kWriteProtected to clear it on a read-only host file.
  DiskStatus SetWriteProtected(bool write_protected);

 protected:
  DiskMedia(int sector_count, uint16_t flags);
  ~DiskMedia();

 private:
  virtual DiskStatus WriteFlags(uint16_t flags) = 0;
};

// A disk image: a host file holding a flat array of sectors behind its header.
class DiskImage final : public DiskMedia {
 public:
  // Creates a blank image of `sector_count` sectors at `path`, replacing any
  // file there. `file_system` must outlive the image.
  static absl::StatusOr<std::unique_ptr<DiskImage>> Create(
      gb::FileSystem* file_system, std::string_view path,
      int sector_count);

  // Opens an existing image, for reading and writing, or only reading, as a
  // write-protected image, if the host file is read-only.
  static absl::StatusOr<std::unique_ptr<DiskImage>> Open(
      gb::FileSystem* file_system, std::string_view path);

  // Reads or writes one whole sector. Fail with kIoError, and writes with
  // kWriteProtected on a protected image.
  DiskStatus ReadSector(int sector, absl::Span<uint16_t> words);
  DiskStatus WriteSector(int sector, absl::Span<const uint16_t> words);
};
```

- It owns its `gb::File`. On a little-endian host (`std::endian::native`),
  which is every host today, a sector's words already are its bytes on disk,
  so a sector is read or written as one 512-byte block, with no conversion.
  Only a big-endian host swaps each word's bytes, after reading and before
  writing.
- `Open` fails with an `absl::Status` naming the problem: the file is missing
  or can't be opened for reading and writing, it isn't an image (the wrong
  ID), its version or sector size isn't supported, its sector count is 0, or
  the file is too short to hold the sectors its header counts.
- A read-only host file (`gb::FileAttribute::kReadOnly` in its `PathInfo`) is
  a write-protected image, whatever its header says, from CL19. `Open` opens
  it for reading only, and its flags have the write-protect bit set, so
  `MEDIA` reports it. The header's bit is the floppy's notch, which travels
  with the image; the host's attribute is the host's own protection, and the
  image never changes it. Clearing protection on a read-only host file fails
  with kWriteProtected: the host file's protection is one the image can't
  change. It is checked before trying, as a failed host write would leave the
  image unusable (see below), and setting protection when already set writes
  nothing.
- `Create` writes the header and zero sectors, a few sectors at a time.
- It is write-through to the host: `WriteSector` and `SetWriteProtected`
  flush the host file before returning, so a host failure is the result of
  the write it failed, and there is no `Flush` to pair with writes. A flush is
  an OS write (the local protocol's `fflush`), not a sync to the device, so a
  flush per sector costs nothing at the drive's rates.
- Once a host read, write, or flush fails, `gb::File` is invalid, so every
  later operation on the image is `kIoError` too. From CL10, the hardware and
  file system fail with it until an image is attached again, so reattaching
  the reopened image is the retry (see CL10).
- Tests reach the `kIoError` paths through `FaultyFileProtocol`, a test-only
  `gb::MemoryFileProtocol` in `oz3_disk_test` whose files' reads, writes, and
  flushes fail once its `Limits` (how many more of each succeed) run out. So
  nothing for tests is in `DiskImage`. CL13's crash test uses it to stop the
  image's writes after a given number of sectors. Names:
  `FaultyFileProtocol`. Alternatives: `FailingFileProtocol` (reads as always
  failing), `FaultInjectionFileProtocol` (long). Recommended:
  `FaultyFileProtocol`.

### disk: DiskHardware

```
struct DiskHardwareSettings {
  int read_cache_sectors = 0;          // 0 for no cache.
  int64_t read_words_per_sec = 0;      // 0 takes no time.
  int64_t write_words_per_sec = 0;     // 0 takes no time.
  int64_t cache_words_per_sec = 0;     // 0 takes no time.
  absl::Duration latency = absl::ZeroDuration();
};

// A dumb drive: sector reads and writes on whatever image is attached, through
// a write-through read cache, adding up the time each takes.
class DiskHardware {
 public:
  // Whether a written sector is kept in the read cache.
  enum class WriteCaching {
    kCache,  // It may be read again.
    kSkip,   // The caller keeps it, and won't read it from the disk.
  };

  explicit DiskHardware(const DiskHardwareSettings& settings);  // Empty.

  const DiskHardwareSettings& GetSettings() const;

  // Changing the image (null for none) empties the cache and forgets the head.
  void AttachImage(std::unique_ptr<DiskImage> image);
  std::unique_ptr<DiskImage> DetachImage();  // Null if empty.
  bool HasImage() const;
  const DiskMedia* GetMedia() const;  // Null if empty.

  // kNoDisk with no image.
  DiskStatus ReadSector(int sector, absl::Span<uint16_t> words);
  DiskStatus WriteSector(int sector, absl::Span<const uint16_t> words,
                        WriteCaching caching);

  // Returns the simulated time the reads and writes took since the last call,
  // and resets it.
  absl::Duration CollectBusyTime();
};
```

- The cache holds recently read and written sectors, least recently used out
  first. A write goes straight to the image, and with `kCache` caches the
  sector as a read does, so a sector written and later read again (a file
  read and written at random, or a FAT or directory sector the file system's
  one buffer let go) is cheap to read.
- A write with `kSkip` updates the sector's cached copy if there is one, so
  the cache never holds stale words, but doesn't add one. The file system
  passes it for the two sectors it always holds: the superblock (sector 0),
  whose fields it keeps after mounting, and the FAT's first sector (sector
  1), written at nearly every commit that allocates or frees, which would
  otherwise keep a cache slot for nothing. Every other write is `kCache`.
  The choice is the caller's, as the hardware knows nothing of file systems.
- A cached read costs a sector at the cache rate (`cache_words_per_sec`), the
  drive copying it out of its memory, with no latency, and doesn't move the
  head. Any other transfer that isn't the sector after the last transfer, in
  the same direction, first costs the latency. The first transfer always
  does, as does the first after the image changes. A transfer that fails
  takes no time and doesn't move the head.
- The hardware is the drive, so it lives as long as the drive, with images
  attached and detached as disks are inserted and ejected. It owns the
  attached image. With none, every transfer is `kNoDisk`. Attaching over an
  attached image destroys the old one. The busy time is the drive's, so
  changing the image keeps it.
- Settings must not be negative, which the constructor checks. The drive
  validates its config before making hardware.
- The default settings take no time and cache nothing, which is what tools
  want. The drive's timing presets belong to *Disk drive device*; each gives a
  cache rate of 1,000,000 words per second (a 1 MHz OZ-3 class controller,
  about 256 µs a sector), as disk_design.md's Timing presets now say.

### disk: values

```
class DiskFilename {
 public:
  // Parses dotted text ("hello.txt"), upper-casing letters. Returns nullopt
  // for anything that breaks the name rules, including ? and *.
  static std::optional<DiskFilename> Parse(std::string_view text);

  // Parses a pattern ("*.TXT"), expanding each * to ?s.
  static std::optional<DiskFilename> ParsePattern(std::string_view text);

  // Validates an 8-word field, as a command argument or directory entry holds
  // it, upper-casing letters.
  static std::optional<DiskFilename> FromWords(absl::Span<const uint16_t> words);
  static std::optional<DiskFilename> PatternFromWords(
      absl::Span<const uint16_t> words);

  std::string ToString() const;  // "HELLO.TXT", or "HELLO" with no extension.
  Words GetWords() const;
  bool IsPattern() const;

  // Byte compare where this pattern's ? matches any byte.
  bool Matches(const DiskFilename& name) const;

  // Orders by the 16 bytes in reading order, as CMPT sorts.
  auto operator<=>(const DiskFilename&) const;
};
```

- `DiskLabel` is the same for the label (`Parse`, `FromWords`, `ToString`,
  `GetWords`), with 0 to 16 characters and no dot or patterns.
- Timestamps are core's `DateTime` (see core: DateTime above), not a disk
  type of their own.
- `DiskDirEntry` is the 16 words with getters and setters: the name (nullopt
  for a free entry or one that breaks the rules), attributes, first cluster,
  size, and modified time. It holds its words exactly as read, and `IsFree`
  is its first word being 0. The attribute bits are `kDiskAttribReadOnly`,
  `kDiskAttribHidden`, and `kDiskAttribDirectory`, and `kDiskAttribSettable`
  is the two `ATTRIB` sets.
- Dotted text with a trailing dot and no extension (`HELLO.`) is the name
  with no extension, as DOS reads it. A second dot is refused.

### disk: DiskLayout

`disk_layout.h` holds where a formatted disk's parts are, as pure arithmetic
with no file system, so disk_design.md's layout rules live in one place:

```
// Where a formatted disk's parts are: the FAT after the superblock, then the
// clusters, as the superblock records them.
struct DiskLayout {
  static constexpr int kMaxSectorsPerCluster = 64;
  static constexpr int kMaxClusterCount = 0xFFFE;
  static constexpr int kMaxFatSectors = 256;  // What the most clusters need.
  static constexpr int kMaxSectorCount = 4'194'433;  // The largest disk.

  auto operator<=>(const DiskLayout&) const = default;

  // Returns whether every field is in range, and the disk fits in an image of
  // `sector_count` sectors. The FAT needs ⌈(cluster_count + 1) ÷ 256⌉ sectors,
  // a word per cluster and the free list's head, but may have more.
  bool IsValid(int sector_count) const;

  // Returns the sectors the disk uses: 1 + FAT + clusters.
  int GetSectorCount() const;

  // Returns the first sector of `cluster` (1 to cluster_count).
  int GetClusterSector(int cluster) const;

  // Returns the most words a file can have: every cluster but the root's.
  constexpr int GetMaxFileSize() const;

  // From CL15: the FAT's link rule, so the file system and the checks share
  // it. A `cluster` of 0 is word 0, whose link is the free list's head.
  static constexpr int kSuperblockSector = 0;
  static constexpr int kFirstFatSector = 1;  // Holds the free list's head.
  static constexpr uint16_t kFatEnd = 0xFFFF;
  // Returns the sector holding the FAT's word `index`.
  static constexpr int GetFatSector(int index);
  // Returns the cluster `cluster`'s FAT word `link` gives next, 0 at the
  // chain's end, or nullopt for a link past the last cluster.
  std::optional<int> GetNextCluster(int cluster, uint16_t link) const;

  int sectors_per_cluster = 0;  // A power of two, 1 to 64.
  int cluster_count = 0;        // 1 to 65,534.
  int fat_sectors = 0;          // At least ⌈(cluster_count + 1) ÷ 256⌉.
};

// Returns the layout that formatting an image of `sector_count` sectors gives
// with `sectors_per_cluster` (0 picks the smallest that covers the image), or
// nullopt if none does.
std::optional<DiskLayout> ChooseDiskLayout(int sector_count,
                                           int sectors_per_cluster);
```

The file system uses it throughout, not only in tests:
- **`Format`** calls `ChooseDiskLayout` with the image's sector count and the
  requested sectors per cluster, refusing with `kBadArgument` on nullopt, and
  writes the result to the superblock.
- **`AttachImage`** reads the superblock's fields into a `DiskLayout`, and the
  disk is `kUnformatted` unless `IsValid` with the image's sector count. A
  superblock may record more FAT sectors than the minimum, as disk_design.md
  allows, so attaching checks the layout rather than recomputing it.
- **Every cluster access** turns a cluster into its sector with
  `GetClusterSector`, and `CheckDisk` uses the same layout for its range
  checks.

Being separate also lets its tests cover every row of disk_design.md's table,
up to 128 MiB, without making images that size.

### disk: DiskFileSystem

```
// A slot in a DiskFileSystem's open-file table, 0 to max_open_files - 1, as
// the drive's handles are.
using DiskFile = int;

class DiskFileSystem {
 public:
  // Makes an empty drive, with no image. Each image attached gets hardware
  // with `settings`. Up to `max_open_files` files may be open at once, each
  // with a buffer of its own.
  DiskFileSystem(const DiskHardwareSettings& settings, int max_open_files);

  const DiskHardwareSettings& GetSettings() const;
  int GetMaxOpenFiles() const;

  // Image. AttachImage returns kOk, kUnformatted, or kIoError, and the image
  // stays attached for all three. DetachImage returns null with no image.
  DiskStatus AttachImage(std::unique_ptr<DiskImage> image);
  std::unique_ptr<DiskImage> DetachImage();
  bool HasImage() const;
  const DiskMedia* GetMedia() const;  // Null if empty.

  // Returns the simulated time the disk spent since the last call, and
  // resets it.
  absl::Duration CollectBusyTime();

  // Volume. GetVolumeInfo returns kUnformatted with no file system.
  DiskStatus Format(const DiskLabel& label, int sectors_per_cluster,
                   DateTime now);  // 0 sectors per cluster to choose.
  DiskStatus SetLabel(const DiskLabel& label);
  DiskStatus GetVolumeInfo(DiskVolumeInfo* info);

  // Files.
  DiskStatus Open(const DiskFilename& name, DiskFile* file);
  DiskStatus OpenRoot(DiskFile* file);
  DiskStatus Create(const DiskFilename& name, DateTime now, DiskFile* file);
  DiskStatus Close(DiskFile file, DateTime now);
  DiskStatus Flush(DiskFile file, DateTime now);
  DiskStatus Flush(DateTime now);  // Every open file and buffer.
  DiskStatus Truncate(DiskFile file, DateTime now);

  // Read and Write move at most up to the next sector boundary.
  DiskStatus Read(DiskFile file, absl::Span<uint16_t> words, int* count);
  DiskStatus Write(DiskFile file, absl::Span<const uint16_t> words,
                   int* count);
  DiskStatus Seek(DiskFile file, int position);
  DiskStatus GetFileInfo(DiskFile file, DiskFileInfo* info);
  DiskStatus GetWritableWords(DiskFile file, int* words);

  // Directory.
  DiskStatus Find(DiskFile directory, const DiskFilename& pattern,
                 DiskDirEntry* entry);
  DiskStatus Delete(const DiskFilename& name);
  DiskStatus Rename(const DiskFilename& from, const DiskFilename& to);
  DiskStatus SetAttributes(const DiskFilename& name, uint16_t attributes);
};
```

- **Buffers** are fixed at construction: one sector per open file, one
  directory sector, a FAT sector holding sector 1, and one working FAT sector,
  as disk_design.md's Memory describes. A buffer that takes another sector
  writes its old one first if it is dirty. Every write goes through one
  private function that knows which buffer it is, so the commit orders in
  disk_design.md's Committing are kept in one place (from CL16, the FAT's
  in `DiskFat`, and the rest in the file system).
- **The free count** is counted by walking the free list the first time
  `GetVolumeInfo`, `Create`, or an `Open` on a writable disk needs it, and kept
  current from then on, so `GetWritableWords` never needs the disk.
- **Handles:** a `DiskFile` is only a slot number, as the drive's handles are,
  and the file system gives each open the lowest free slot. A slot out of
  range, or with nothing open, gets `kBadHandle`. Nothing tells a slot's file
  from a later one: a caller that keeps a handle past `Close` reaches
  whatever is opened there next, as software misusing a real drive's handles
  would, and that is the caller's bug to keep track of. A slot open when its
  image was detached is stale, and gets `kDiskChanged` (see Behavior), which
  the file system can know, since it is the slot's state rather than the
  handle's; it isn't free until it is closed, so the drive's slot and the
  file system's free together. A directory handle reads its chain's full
  length as its size, and refuses writes with `kWrongHandleType`.
- **Attaching** attaches the image to the hardware, which empties its cache
  and forgets its head, and charges the
  superblock's read to the busy time, as the drive charges attaching. Buffers
  and the free count belong to the attached image, so detaching discards
  them, with no write-back.
- **Corruption:** a link out of range, or a chain longer than the cluster
  count (a loop), is `kCorrupt`, never a crash or a hang.
- **Write protection** is checked by each changing operation before it does
  anything, as well as by the image.
- **Single-threaded.** It is called from the drive's update or a tool's main
  thread, never from two threads at once.

**Brittleness:** nothing is paired. Attaching reads the superblock itself,
commits happen inside the operations, a slot can't outlive its disk
unnoticed, and allocation needs no end call. A handle kept past its `Close`
isn't caught, by design: handles are slot numbers, as on the drive. What's
left is the contract that nothing else writes the image while a file system
has it, which ownership enforces within a process (the file system owns the
hardware, which owns the image), and which across processes is documented,
as disk_design.md says.

### disk: DiskSuperblock

From CL15, the superblock is a value, as an entry is, so the file system and
the checks read it the same way:

```
// A disk's sector 0: the superblock's fields (words 0-31), then the boot
// area. It holds its words exactly, as the disk has them, so one read from a
// disk may describe no file system, which IsValid says. The setters change
// only their own words.
class DiskSuperblock final {
 public:
  using Words = std::array<uint16_t, kDiskSectorWords>;

  DiskSuperblock() = default;  // All zeros, describing no file system.

  // What formatting writes: the format ID and version, `layout`, the root at
  // `root_cluster`, `label`, and `formatted`, with every other word 0.
  DiskSuperblock(const DiskLayout& layout, int root_cluster,
                 const DiskLabel& label, DateTime formatted);

  // Returns `words` exactly as given.
  static DiskSuperblock FromWords(const Words& words);

  bool operator==(const DiskSuperblock&) const = default;

  // Returns whether it describes a file system on an image of `sector_count`
  // sectors: the format ID and version known, the layout valid, the root
  // cluster in range, and the label valid.
  bool IsValid(int sector_count) const;

  DiskLayout GetLayout() const;
  int GetRootCluster() const;
  std::optional<DiskLabel> GetLabel() const;  // nullopt if it isn't valid.
  void SetLabel(const DiskLabel& label);
  const Words& GetWords() const;
};
```

- It replaces `DiskFileSystem`'s static superblock functions and word
  offsets. The file system keeps one as `superblock_`, and `SetLabel` writes
  its words, so the boot area and reserved words are kept.

### disk: DiskFat

From CL16, the FAT is a class of its own, which `DiskFileSystem` holds, as
the cleanest part of the file system to split out: everything about the FAT's
buffers and the order they are written in is in one place.

```
// The FAT and free list of the disk attached to `hardware`, through two
// sector buffers: sector 1, holding the free list's head, and one working
// sector for any other.
class DiskFat final {
 public:
  DiskFat(DiskHardware* hardware, const DiskLayout& layout);

  // Writes a formatted disk's FAT, with only the root's cluster taken, as
  // allocating would take it, setting `root_cluster` to it.
  DiskStatus Format(int* root_cluster);

  DiskStatus CountFreeClusters();          // Once, walking the free list.
  std::optional<int> GetFreeCount() const;

  DiskStatus GetNextCluster(int cluster, int* next);
  DiskStatus CountChain(int chain_first, int* chain_count, int* chain_last);
  DiskStatus GetFreeHead(int* head);
  DiskStatus AllocateCluster(int last_cluster, int* cluster);
  DiskStatus FreeChain(int unlink_from, int chain_first, int chain_last,
                       int chain_count);
  DiskStatus Write();  // The dirty buffers, sector 1 first.
};
```

- Each function is `DiskFileSystem`'s of the same name moved, with the same
  write orders. FAT sectors are only ever in the FAT's buffers, so its writes
  never need copying into the file system's other buffers.
- `SectorBuffer` moves to a header of its own, as both use it, as
  `DiskSectorBuffer` (`disk_sector_buffer.h`), now that it is in the `oz3`
  namespace.
- The file system holds it as a `std::optional<DiskFat>`, made with the
  superblock's layout when a disk is attached or formatted, and dropped with
  its buffers when the disk goes, so the layout has no second copy to keep
  in step.
- `Format` takes the root's cluster with `AllocateCluster`, on a zeroed sector
  1, so which cluster the root takes is the allocator's choice, with no rule
  of its own.
- What stays in `DiskFileSystem` is the order between the FAT and everything
  else: FAT before directory when linking, directory before FAT when
  freeing.
- Not split out: the directory. It is read through the same slot code as an
  open root handle, and growing the root grows every open root slot, so it
  would take the slots with it.

### disk: checks

`disk_check.h`, from CL17. The checks work on an image no file system has
attached, as the drive never checks a disk, and a host checking one in a
drive detaches it first. So no file can be open during a check, by
ownership rather than a status, and a repair leaves no free count or buffer
behind to go stale.

```
// What a check finds wrong with a disk. A crash leaves at most orphaned
// clusters and long chains (see disk_design.md's Committing).
struct DiskCheckReport {
  // What is wrong with a file's chain: the first problem walking it finds.
  enum class ChainProblem {
    kLong,         // It goes on past the clusters its size needs.
    kShort,        // It ends before covering its size.
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

  bool operator==(const DiskCheckReport&) const = default;

  bool broken_root = false;       // A link out of range, or a loop.
  bool broken_free_list = false;  // A link out of range, a loop, or a link
                                  // into a chain.
  int orphaned_clusters = 0;      // On no chain, and not free.
  std::vector<FileProblem> file_problems;      // In directory order.
  std::vector<DiskDirEntry> bad_names;         // Names breaking the rules.
  std::vector<DiskFilename> duplicate_names;   // Each one once.
};

// Sets `report` to what is wrong with the file system on `image`, writing
// nothing. Returns kUnformatted if the image has no file system, and kIoError
// if the host fails. Never returns kCorrupt, as corruption is what it reports.
DiskStatus CheckDisk(DiskImage& image, DiskCheckReport* report);

// Checks as CheckDisk does, then fixes what it found (from CL18). Returns
// kWriteProtected on a protected image.
DiskStatus RepairDisk(DiskImage& image, DiskCheckReport* report);
```

- **Reading:** the whole FAT is read into memory at once (64 Kiw at most),
  and the root's sectors as its chain reaches them, straight from the image.
  It takes no simulated time, which only the drive charges.
- **Claiming:** each cluster belongs to the first chain to reach it: the
  root's, then each file's in directory order, then the free list's. So a
  cross-link is the later file's problem, and a cluster both free and in a
  chain stays in the chain, with the free list broken there. A cluster no
  chain reaches is orphaned. The free list is last, as a crash never leaves
  a chain running into it. Each cluster records only the number of the walk
  that claimed it, as which file has it doesn't matter: reaching one this
  walk claimed is a loop, and one an earlier walk claimed is a cross-link.
- **A file's chain** is walked only as far as its size needs. A long chain's
  tail claims nothing, so it counts as orphaned too, and repairing frees it
  with the other orphans. A size past the disk's largest file is short. A
  first cluster out of range is broken, even for an empty file, whose chain
  in range is long. Entries with bad names are walked as files, so their
  clusters aren't orphaned. As the tail isn't walked, one running into
  another chain or the free list is only long, which repairing fixes by
  cutting it.
- **The root's entries** are read from the clusters its walk claimed, so a
  broken root is still read up to the break. An entry is free only if all
  its words are 0, as freeing writes, though the drive takes a first word of
  0 as free. One with only its name zeroed is a bad name, so its chain is
  walked, and repairing treats it as any other bad name.
- **Repairing** (CL18) writes, in order: the entries that change (a short or
  broken file's size cut to its chain, cross-linked files cut before the
  shared cluster, and first cluster 0 for an emptied file), then each chain's
  new end, then a free list rebuilt from every cluster nothing claims. The
  rebuild first empties the free list (word 0 to FFFFh), then links the
  clusters in order, then writes the head, so a crash in it leaves at worst
  orphans. Every write goes straight to the image, so this order is the
  code's. Duplicate names are only reported: `Delete` and `Rename` act on the
  first, so a user can fix them. What to do with bad names is settled in
  CL18: freeing them, or leaving them.
- disk_design.md changes to match, in CL17: checks are `CheckDisk` on an
  image, not `DiskFileSystem::Check`, and `DiskFileProtocol` doesn't offer
  them through `GetFileSystem()`.
- Names: **`RepairDisk`**, a call of its own. Alternatives: `CheckDisk(bool
  repair, ...)` (as first planned, but `CheckDisk(image, true, &report)`
  doesn't say what the bool does), `FixDisk` (as `oz3dsk check --fix`, but
  terse). Recommended: `RepairDisk`. **`kLong` and `kShort`**, a pair
  matching "a chain that doesn't match its size". Alternatives: `kSlack`
  (disk_design.md's word for the tail, but slack usually means the unused
  end of a last cluster), `kTooLong`. Recommended: `kLong` and `kShort`.

### To confirm

Confirmed in CL2: `gb::File::Write` past the end of a `MemoryFileProtocol`
file extends it, and `Read` at the end returns short rather than failing.
Game Bits' `TestProtocol` is for testing Game Bits' file system, so host
failures come from OZ-3's own `FaultyFileProtocol` instead.

## CLs

The status is `[ ]` not started, `[~]` in progress (set by the CL's first
edit), or `[x]` submitted (set in the CL's own commit).

### CL1 [x] core: dates and times

Depends on: nothing.

- `oz3/core/date_time.h` and `.cc`: `DateTime`.
- `oz3/devices/timer.h` and `.cc`: the timer latches a `DateTime`, and
  answers `kTimerDate` and `kTimerTime` with its words, rather than packing
  them itself.
- disk_design.md: `DiskTime` becomes core's `DateTime`, in the Disk library
  table and No clock.
- No visible change: the timer answers exactly as before.

**Verify**
- Standard checks.
- Unit tests: `DateTime`'s words from a civil time (the year, the month and
  day bytes, the hour, the minute and second bytes), back to the same civil
  time, the default and all zeros as no date (`ToCivil` nullopt), a month of 0
  as no date, ordering across each word, and a year past 65,535 keeping its
  low 16 bits as the timer did. The timer's existing tests pass unchanged.

### CL2 [x] disk: the library and disk images

Depends on: nothing.

- `oz3/disk/`: `CMakeLists.txt` (`oz3_disk`, on `gb_file` and Abseil; CL4
  adds `oz3_core` when it first uses `DateTime`), `disk_status.h`
  (`DiskStatus`, with every status code), `disk_image.h` and `.cc`, and the
  test-only `faulty_file_protocol.h` and `.cc`.
- disk_design.md: `DiskError` becomes `DiskStatus`, and the library uses
  `core` for `DateTime`.
- `oz3/CMakeLists.txt` adds `disk` after `core`.
- CLAUDE.md: the library order becomes `core` → `disk` → `tools` → ..., with a
  description of `disk` (it uses only `core`, for `DateTime`).
- Unused, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: creating and reopening images, the header's exact bytes, sector
  bytes low first, every header error, a file too short for its sectors,
  reads and writes at the first and last sectors and past the end, write
  protection set, cleared, and refusing writes, a missing or locked host file,
  and host read, write, and flush failures (through `FaultyFileProtocol`):
  `kIoError` from each operation and every one after, and a failed `Create`
  or `Open`.

### CL3 [x] disk: the hardware's cache and timing

Depends on: CL2.

- `disk_hardware.h` and `.cc`: `DiskHardwareSettings` and `DiskHardware`.
- disk_design.md: a cache rate, 1,000,000 words per second in every preset,
  so cached reads aren't free.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: exact times for continuous and non-continuous reads and writes,
  a switch of direction, the first transfer, cached reads costing the cache
  rate with no latency (nothing at a rate of 0), an empty drive as `kNoDisk`,
  attaching, detaching, and replacing images (emptying the cache and
  forgetting the head, but keeping the busy time), and
  not moving the head, least recently used eviction, `kCache` writes adding
  and updating cached sectors, `kSkip` writes updating but not adding them,
  a cache of 0, and zero rates taking no time.

### CL4 [x] disk: names, labels, and directory entries

Depends on: CL1, CL2.

- `disk_filename.h` and `.cc`, `disk_label.h` and `.cc`, and
  `disk_dir_entry.h` and `.cc`, with the internal `disk_packed_text.h` and
  `.cc` holding the packed text rules names and labels share (the
  characters, upper-casing, padding, and packing).
- `oz3_disk` depends on `oz3_core`, for `DateTime`.
- disk_design.md: `FromPattern` becomes `ParsePattern`, and the Disk library
  table gains `DiskLabel`.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: names at each limit (12 and 4 characters, an empty name, an
  empty extension), every allowed and refused character, a second dot, case,
  padding with a zero byte before a character, patterns (`*.*`, `*.TXT`,
  `A*.*`, `*`, `?` against padding, a character after `*`), parsing words
  with lowercase letters, ordering across the low and high bytes (as `CMPT`
  sorts), labels at 0 and 16 characters, and entries' fields round trip,
  the modified time included.

### CL5 [x] disk: attaching, formatting, and the free list

Depends on: CL3, CL4.

- `disk_layout.h` and `.cc`: `DiskLayout` and `ChooseDiskLayout`.
- `disk_media.h` and `.cc`: `DiskMedia`, the medium, split out of
  `DiskImage` as its base. `DiskHardware::GetImage` becomes `GetMedia`.
- `disk_file_system.h` and `.cc`: construction, `AttachImage`, `DetachImage`,
  `GetMedia`, `CollectBusyTime`, `Format`,
  `SetLabel`, and `GetVolumeInfo`, with the FAT buffers and walking the free
  list for the free count. Open files come in later CLs.
- disk_design.md: the `DISK` command becomes `VOLUME` (still 41h), in the
  command table, its notes, the free count, and the machine table.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: `ChooseDiskLayout` against every row of disk_design.md's table
  and the 65,534-cluster limit, refusing each bad sectors per cluster and an
  image too small for any layout; `IsValid` at each field's limits, extra
  FAT sectors, and a disk one sector bigger than its image;
  `GetClusterSector` for the first and last clusters; formatting's exact superblock, FAT, and root;
  `kNoDisk` for everything with no image; attaching a formatted disk, and
  each way a disk is unformatted (ID, version, each field out of range, a
  layout bigger than the image); detaching returning the image, with
  unwritten buffers discarded; attaching over an attached image; the busy
  time of attaching, with a fresh cache each time; the media and write
  protection with and without a file system; the free count of a fresh
  format, and of a hand-made fragmented free list (the zero runs and explicit
  links); a broken free list as `kCorrupt`; the label; and write protection
  refusing `Format` and `SetLabel`.

### CL6 [x] disk: opening and reading files

Depends on: CL5.

- `DiskFileSystem`: `Open`, `OpenRoot`, `Close`, `Read`, `Seek`,
  `GetFileInfo`, and `Find`, with the open-file table, stale slots, and
  directory handles. `Format` refuses with `kInUse` while a file is open.
  `Close` takes no time yet, as nothing can be written to stamp.
- disk_design.md: stale slots are the library's (see Behavior), and
  `DiskFile` is a slot number with no generation.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests, on disks whose files are written into the image by the test:
  reading whole files and across sectors and clusters, one sector per call at
  most, seeking (to the end, past it), reading at the end, `in use` for a
  second open of a file or the root, too many open, the lowest free slot
  taken, slots out of range or closed as `kBadHandle`, slots open across a
  detach as `kDiskChanged` until closed, and staying taken until then, `Find`
  with patterns over several directory sectors with free
  entries between, the hardware time of a sequential read (no walking), and
  broken chains as `kCorrupt`.

### CL7 [x] disk: int for sectors, sizes, and positions

Depends on: CL6.

- The limits come from `DiskLayout`'s: `DiskMedia::kMaxSectorCount` is the
  largest image a format can use, 4,194,433 sectors (the superblock, 256 FAT
  sectors, and 65,534 clusters of 64 sectors), and `DiskDirEntry::kMaxSize`
  is what the largest disk's clusters hold but for the root's first,
  1,073,692,672 words. To confirm in review:
  `DiskLayout::kMaxFatSectors` becomes 256, the FAT the most clusters need,
  so a superblock can't claim a FAT no format writes.
- Sector numbers and counts, sizes, positions, and transfer counts are `int`
  throughout the library (`DiskMedia`, `DiskImage`, `DiskHardware`,
  `DiskLayout`, `DiskDirEntry`, `DiskFileInfo`, and `DiskFileSystem`), and in
  this plan's interfaces above. Byte offsets in an image file stay `int64_t`,
  as the largest image is just over 2 GiB.
- Dwords read from a disk can exceed the limits, so they are checked before
  they become `int`: `DiskImage::Open` refuses a header's sector count above
  the limit, and `DiskDirEntry::GetSize` gives nullopt for a size above
  `kMaxSize`, as `GetName` does for a bad name, which `Open` returns as
  `kCorrupt`.
- disk_design.md: an image header's sector count is 1 to 4,194,433, and a
  directory entry's size is at most what the disk holds. The drive's dword
  fields are unchanged, so it checks a dword against the limit before it
  casts: a `SEEK` beyond the largest size is past the end, not negative.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: `Create` and `Open` refusing one sector past the limit, an
  entry on the disk one word past the biggest size as invalid, and `Open` of
  it as `kCorrupt`.

### CL8 [x] disk: creating files

Depends on: CL7.

- `DiskFileSystem::Create`: finds the name and the first free entry in one
  pass over the root, through the root slot `Open` keeps. Returns
  `kWriteProtected`, `kUnformatted`, `kBadName` for a pattern, `kTooManyOpen`,
  `kExists` if the name is taken (open or not), and `kDiskFull` if the root
  has no free entry and no cluster to grow by. Writes the new entry (the name,
  `now` as its modified time, and zeros for everything else) into the first
  free entry, and opens the empty file in the lowest free slot. Like `Open`,
  it counts the free clusters if nothing has yet, once the name is known to
  be free.
- Growing the root: with no free entry, it writes the free list's head
  cluster's sectors in order while the cluster is still free, zeroed but for
  the new entry in its first, then allocates it and links it onto the root's
  chain. Writing them in order keeps them continuous, so only the first pays
  the latency. The root slot `Open` keeps and any open root handle take the
  new size, so an open listing reaches the new entries.
- The write path: `SectorBuffer` gains a dirty flag, and every write goes
  through one private function that knows which buffer it is. It writes
  sector 1 with `kSkip` and every other with `kCache`; it writes sector 1
  first if it is dirty before the working FAT sector, so the free list's head
  reaches the disk before any link to a cluster taken from it (only
  allocating leaves FAT buffers dirty past its operation, so this is
  allocating's order); and a
  directory sector written through one buffer is copied into any other buffer
  holding it, so an open root handle sees each entry as it is written.
  Loading another sector into a dirty buffer writes it first.
- Allocating: takes the free list's head, sets the head to its next, writes
  the cluster's own word as FFFFh, and links the chain's old last cluster to
  it, as disk_design.md's FAT and free list says, keeping the free count.
  When the link is in another FAT sector than the cluster's own word, the
  FAT is written before the link is made, so the link can't reach the disk
  before the end, leaving the chain running on into the free list.
- `Create` commits before returning, in disk_design.md's order: the new root
  cluster's sectors, sector 1, then the working FAT sector; or without
  growing, just the entry's sector.
- disk_design.md: the drive stamps a file when it creates it, as well as when
  it closes it after a write, so no reachable entry is left with a zero time
  by the drive (Timestamps, and `CREATE`'s notes).
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: creating into a free entry between used ones and at the end of
  a sector, with the entry's exact words (its modified time `now`), then
  opening it (`kInUse`) and reading it empty after closing; each refusal; `kExists` for an open and a closed file; growing a full root,
  with the exact order of sector writes, a 1-sector and a 4-sector cluster,
  and the FAT's head and the new link in different FAT sectors; an open root
  handle seeing the new entry and the grown size; the free count after each;
  `kDiskFull` for a full root on a full disk, changing nothing; and the
  exact sector writes of a create that doesn't grow (just the entry's sector).

### CL9 [x] disk: writing files

Depends on: CL8.

- Write protection changes only with the disk out of the drive:
  `DiskHardware` and `DiskFileSystem` lose their non-const `GetMedia`, so
  `SetWriteProtected` is called only on an image no drive has attached.
  Attaching resets the free count, so every file open on a writable disk has
  it, and `GetWritableWords` never needs the disk.
- `DiskFileSystem`: `Write` and `GetWritableWords`.
- `Write` writes at most up to the next sector boundary, allocating each
  cluster when its first word is written, after following any chain already
  past the file's size (slack). A sector the write covers from its start up
  to the size or the sector's end starts zeroed rather than read, as it keeps
  nothing of the file. It returns
  `kWrongHandleType` for a directory handle, `kReadOnly` for a read-only file
  (the attribute is kept in the slot at open), `kWriteProtected`, and
  `kDiskFull` when the write needs a cluster and none is free. The data and
  FAT buffers it changes are written only when they take another sector, or
  by CL11's commits.
- `GetWritableWords`: the words from the file's position to its size, the
  room left in its last cluster, and the free clusters', without the disk,
  with the same refusals as `Write`.
- A failed `Create` no longer discards the buffers, which can now hold other
  files' allocations: it drops only the directory buffer, so the entry it
  filled is read again as the disk has it, and a grown root takes its new
  size only once the commit succeeds. Every other change it can leave follows a failed host
  write, after which the image refuses everything.
- `Close` still writes nothing, so until CL11 a write not yet written by a
  buffer taking another sector is lost at `Close`, and the clusters it
  allocated are orphaned once a later commit writes the FAT.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests, reading back through the same handle: writing across sector
  and cluster boundaries, overwriting inside a file, a fresh sector zeroed
  rather than read, growing over slack past the size, filling the disk to
  `kDiskFull` with `GetWritableWords` exact at each step, each refusal, the
  free count, two files written at once sharing the working FAT sector, the
  exact order of sector writes for a buffer taking another sector, and a
  failed `Create` leaving another file's allocation in place.

### CL10 [x] disk: failed images

Depends on: CL9.

- A host failure fails the image until it is attached again, in one place:
  `DiskHardware` remembers the first `kIoError` from its image, and returns
  `kIoError` for every later read and write, cached reads included, until an
  image is attached. `DiskFileSystem` checks it in `GetVolumeStatus` and
  `GetSlot`, so every operation returns `kIoError` from then on, including
  those its buffers could answer (`Seek`, `GetFileInfo`, `GetWritableWords`,
  and `Open` and `Find` of buffered sectors). `Close` still frees the slot,
  and a stale slot is still `kDiskChanged`.
- Reattaching is the retry: the host detaches the image, discarding the
  buffers unwritten, opens it again with `DiskImage::Open`, and attaches it.
  The commit orders leave the disk as a crash at that point would, with at
  worst orphaned clusters or slack, so the disk is consistent as it was last
  written. Retrying within an attachment would need each operation to undo
  its changes in memory, which this avoids.
- So a failed operation needs no cleanup of its own: `Create`'s failure
  branch from CL9 goes, `GrowRoot` grows the root's slots itself again, and
  `SetLabel` no longer puts back the old label. `Write`, `Close`, `Flush`, and `Truncate` need none either.
- disk_design.md: **I/O error** fails the disk until it is ejected and
  inserted again (the status codes, and Images and disk changes).
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: the hardware returning `kIoError` after a failed write, for a
  cached read too, until an image is attached; after a failed write, a
  cached read, `Seek`, `GetFileInfo`, `GetWritableWords`, `Find`, and `Open`
  from buffers all returning `kIoError`; `Close` freeing the slot; and
  reattaching the reopened image reading what was written before the
  failure.

### CL11 [x] disk: committing files

Depends on: CL10.

- `DiskFileSystem`: `Flush(file, now)`, `Flush(now)`, and `Close(file, now)`.
- A slot remembers it was written since its last stamp. `Close` and `Flush`
  of a written file commit it: its dirty data sector, then the FAT (sector 1
  first), then its entry, with the size, first cluster, and `now` as the
  modified time. A file not written since, and a directory handle, write
  nothing. `Close` always frees the slot, even when it fails, returning the
  commit's failure, or for a stale slot with uncommitted writes,
  `kDiskChanged`, as they are lost. `Flush(now)` commits every open file,
  stopping at the first failure. That leaves no buffer dirty, as only a
  write leaves the FAT dirty, and a write that fails partway fails the image.
- `Find` returns entries exactly as they are on the disk, so an open file
  lists as it was last committed.
- disk_design.md: the `WRITE` walkthrough allocates as it writes (see
  Behavior), and nothing is freed after a write that stops short; `FIND` and
  Committing's listings give the entry on the disk, not an open file's slot;
  `CLOSE` on a stale slot with uncommitted writes fails with **disk
  changed** (the Disk library section, Images and disk changes, `CLOSE`'s
  notes, and Disk changes and recovery).
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: data surviving `Close` and reattaching, stamps at `Close` and
  `Flush` only after writes (a created file closed unwritten keeps its
  creation stamp), `Flush(now)` with several files, `Close` freeing the slot
  when its commit fails, `Find` showing the committed size, and the exact
  order of sector writes for each commit.

### CL12 [x] disk: truncating files

Depends on: CL11.

- `DiskFileSystem::Truncate`: sets the file's size to its position and frees
  the clusters past what the size needs, and any chain left past the size,
  walking them before writing anything. It takes a stamp, as it changes the
  entry: one that changes the file counts as a write, and it flushes the
  file as `Flush` does, with the entry giving the new size (and first
  cluster 0 at size 0), then writes the FAT marking the chain's new end, the
  freed chain's last link to the old head, and sector 1 with the freed chain
  as the head, as disk_design.md's `TRUNCATE` orders them, with one write
  for changes next to each other in a sector. Freeing is the reverse of
  allocating's order, which the write path keeps for dirty FAT buffers (see
  CL8), so it starts with every FAT buffer clean and writes each in its
  turn. One at the end with nothing past it only flushes. Returns
  `kWrongHandleType` for a directory handle, `kReadOnly`, and
  `kWriteProtected`.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: `Truncate` at 0, mid-cluster, on a cluster boundary, and at the
  end (freeing nothing), the free count and the free list's order after each,
  reading and writing after it, each refusal, and the exact order of sector
  writes.

### CL13 [x] disk: the crash test

Depends on: CL12.

- A test only: for each operation sequence (creating, creating that grows
  the root, writing across clusters and closing, writing over slack, two
  files written at once, growing a file across FAT sectors, and truncating),
  stop the image's writes with `FaultyFileProtocol` after every possible
  number of sector writes, detach and reattach the image, and check that no
  cluster is cross-linked or on both the free list and a chain, and that
  every file's chain covers its size (with CL17's `CheckDisk` once it exists;
  here by walking the chains in the test).
- Each file's words are its position plus a fixed first word, so the test
  also checks every file's data up to its size, which catches a size reaching
  the disk before its data. A cluster may be orphaned only if the sequence
  moves it between owners, or it is on no chain before or after, so losing
  the free list fails too, and a file may be lost only if the sequence
  creates it. The root's new cluster starts with old data, so linking it
  before it is written fails. Without a crash, each file has the size the
  sequence gives it.
- Unused, so no visible change.

**Verify**
- Standard checks.
- The test passes, and fails if any commit's order is reversed.

### CL14 [x] disk: deleting, renaming, and attributes

Depends on: CL13.

- `DiskFileSystem`: `Delete`, `Rename`, and `SetAttributes`, each refusing
  an open file with `kInUse` and committing before returning. `Delete` walks
  the chain first, so a broken one changes nothing, then writes the free
  entry, then frees the chain with `FreeChain`, which leaves an unknown free
  count unknown and does nothing for an empty chain. `CountChain` returns
  `kCorrupt` for a first cluster out of range. `Rename` finds the file (the
  first entry with its name) and whether the new name is taken in one pass,
  and a name is taken even by the file itself. `SetAttributes` sets the
  read-only and hidden bits, keeping the rest, and writes nothing when they
  don't change. `Open` shares the lookup with them (`FindClosedFile`), and
  its entry checks with `Delete` (`GetFileEntryStatus`).
- `GetWritableStatus` also returns `kUnformatted`, after `kWriteProtected`,
  so every change checks once, and `Format` accepts `kUnformatted`.
- The crash test can expect a file to be deleted, and fails a crash that
  leaves fewer files than both before and after, as a lost rename would.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: each on a missing file, an open file (`in use`), and a
  read-only one (`Delete`), renaming onto an existing name, bad attributes,
  the freed clusters' order on the free list, entries staying in their slots
  for an open root handle, write protection, and the crash test for each.

### CL15 [x] disk: superblocks and FAT links

Depends on: CL14.

- `disk_superblock.h` and `.cc`: `DiskSuperblock` (see disk:
  DiskSuperblock), replacing `DiskFileSystem`'s static superblock functions.
- `DiskLayout`: the superblock's and the FAT's first sectors, `kFatEnd`,
  `GetFatSector`, and `GetNextCluster`, the FAT's link rule, which
  `DiskFileSystem` uses in place of its own.
- The file system tests' helpers read superblocks and links with them too.
- `DiskLayout`'s `GetMostClusters` (for `ChooseDiskLayout`) counts sectors
  with `GetSectorCount`, the same sum `IsValid` checks.
- No change in behavior.

**Verify**
- Standard checks.
- Unit tests: `DiskSuperblock`'s words from formatting's constructor, each
  field read back, `SetLabel` keeping every other word, and `IsValid` failing
  for each field out of range, as attaching did. `GetNextCluster` for a link,
  FFFFh, 0 before and on the last cluster, word 0, and a link past the last
  cluster. The file system's tests pass, with only their helpers changed.

### CL16 [x] disk: the FAT

Depends on: CL15.

- `disk_fat.h` and `.cc`: `DiskFat` (see disk: DiskFat), moved out of
  `DiskFileSystem`, with `SectorBuffer` in a header of its own as
  `DiskSectorBuffer`.
- No change in behavior.
- `disk_fat_test.cc`: `DiskFat`'s own tests, on hardware with nothing else
  attached. The file system's tests that only tested the FAT (free counts,
  broken free lists, a one-cluster format, and the FAT's write orders across
  sectors, and the time counting takes) move there. The superblock checks
  covered by `DiskSuperblock`'s and `DiskLayout`'s tests shrink to one case
  each. Growing a root of two clusters stays, as linking from the root's last
  cluster is the file system's.

**Verify**
- Standard checks.
- Unit tests: `Format`'s FAT and its writes, links and chains (counts, the
  last cluster, and each way one is broken), counting the free list once
  (fragmented, backward, full, and broken, reading each FAT sector once,
  continuously), host failures passed on, allocating (from the head, the
  last free cluster, and each write order across sectors), the working
  buffer writing sector 1 before itself as it moves, and freeing (onto the
  head, onto an empty list, each write order across sectors, and nothing).
- The file system's remaining tests pass unchanged, including every exact
  order of writes and the crash tests.

### CL17 [x] disk: consistency checks

Depends on: CL16.

- `disk_check.h` and `.cc`: `CheckDisk` and `DiskCheckReport` (see disk:
  checks): a broken root or free list, orphaned clusters, files whose chains
  are long, short, broken, or cross-linked, bad names, and duplicate names,
  writing nothing.
- The crash tests check every disk they read with it (each crash, and the
  disk before and after), which may hold only orphaned clusters and long
  chains. Before and after aren't clean, as the setups leave clusters off
  the free list, and writing over slack starts with a long chain. They keep
  their own walk of whole chains too, for the owners they compare and each
  file's data, which also fails a long chain's tail sharing a cluster, and
  leave the rest of the judging to `CheckDisk`.
- `DiskLayout::GetClusterWords` and `DiskDirEntry::kEntriesPerSector`, for
  the check and the file system and their tests, which each had their own.
- disk_design.md: checks are `CheckDisk` on a detached image.
- `disk_check_test.cc` builds its disks by hand, in memory, with no file
  system.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: a clean disk reporting nothing, each problem on a hand-made
  disk, a cluster both free and in a chain, a long chain's tail as orphaned,
  a loop in each of the root, a file, and the free list, links out of range,
  a size past the disk's largest file, an entry with only its name zeroed as
  a bad name, an unformatted image, and nothing written.

### CL18 [ ] disk: consistency repairs

Depends on: CL17.

- `RepairDisk` (see disk: checks): cuts files' sizes and chains, then
  rebuilds the free list from every cluster nothing claims.
- The crash tests repair after every crash, then expect a clean check with
  every file's data intact, and a crash test of repairing itself.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: each problem on a hand-made disk repaired, then checking clean,
  the exact order of writes, write protection, and a clean disk writing
  nothing.

### CL19 [ ] disk: read-only image files

Depends on: CL2.

- `DiskImage::Open` reads the host file's attributes, and opens a read-only
  one for reading only, as a write-protected image (see DiskImage above).
  `WriteFlags` refuses on it with kWriteProtected.
- `DiskMedia::SetWriteProtected` writes nothing when the bit is unchanged.
- A read-only image file attaches as a protected disk.

**Verify**
- Standard checks.
- Unit tests, with the attribute set through `gb::FileSystem::SetAttributes`
  on `FaultyFileProtocol` (a `gb::MemoryFileProtocol`, which supports it): a
  read-only image opens, reads, and reports itself protected with a header
  that isn't; its writes fail with kWriteProtected; clearing protection fails
  with kWriteProtected, leaving the image usable, and setting it succeeds, writing
  nothing (a write limit of 0 shows it);
  `Create` over a read-only file fails; and an unchanged `SetWriteProtected`
  writes nothing on a writable image too.
