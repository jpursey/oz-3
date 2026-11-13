# Disk library

The new `oz3/disk` library (`oz3_disk`) is the one implementation of the disk
format in [disk_design.md](../disk_design.md): disk images, the dumb drive's
cache and timing under them, and the file system on top, with names,
timestamps, directory entries, consistency checks, and host transfer helpers.
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
  except `Close`, which frees it and succeeds. So the drive's slots map one
  to one onto handles with no state of their own, and a tool swapping images
  can't write into the wrong disk either. disk_design.md's Disk library
  section changes to match, in CL6.
- **Time comes in as values.** `Format`, `Close`, and `Flush` take the
  `DateTime` to stamp with. A file written since it was last stamped is
  stamped at `Close` or `Flush`. So the drive passes its host clock's time,
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
  reports them, without a file system on the disk, and `SetWriteProtected`,
  for `oz3dsk protect`.
- **Writes allocate as they go.** `Write` allocates each cluster when its
  first word is written, after following any chain already past the file's
  size, rather than allocating a whole request's clusters up front.
  `GetWritableWords(file)` gives, without the disk, how much a write can be
  promised: the room left in the file's last cluster, plus the free clusters'.
  Only the drive changes the free count, so the promise holds. That leaves
  nothing to free after a write that stops short, and no begin and end calls
  to pair. disk_design.md's `WRITE` walkthrough (allocating right after
  accept) changes to match, in CL7.

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
- **`DiskCheckReport`:** what `Check` finds and repairs.
- **`CollectBusyTime()`:** returns the busy time since the last call, and
  resets it. "Busy time" says it's the disk's simulated time, not the
  caller's, and "collect" that it's gathered up and cleared, as a fare is.
  Alternatives: `DrainBusyTime()` (says plainly it empties, but reads oddly
  for time), `GetElapsed()` with a separate `ResetElapsed()` (a pair to
  remember). Recommended: `CollectBusyTime()`. The hardware has the same
  call, which the file system's adds to its own.
- **Host transfer:** `WordsFromBytes`, `BytesFromWords`, `WordsFromText`, and
  `TextFromWords`, in `disk_transfer.h`.
- Constants: `kDiskSectorWords` (256), and each value's own size,
  `DiskFilename::kWordCount` (8), `DiskLabel::kWordCount` (8), and
  `DiskDirEntry::kWordCount` (16). Names and labels being the same size is
  happenstance, so each has its own.

Words are `uint16_t`. Sector numbers, positions, and sizes are `int64_t`, as
they need 32 bits unsigned. Cluster numbers are `int`.

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
  int64_t GetSectorCount() const;
  uint16_t GetFlags() const;  // The header's flags word, as MEDIA reports it.
  bool IsWriteProtected() const;

  // Sets the header's write-protect bit, writing nothing if it is unchanged.
  // Fails with kWriteProtected to clear it on a read-only host file.
  DiskStatus SetWriteProtected(bool write_protected);

 protected:
  DiskMedia(int64_t sector_count, uint16_t flags);
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
      int64_t sector_count);

  // Opens an existing image, for reading and writing, or only reading, as a
  // write-protected image, if the host file is read-only.
  static absl::StatusOr<std::unique_ptr<DiskImage>> Open(
      gb::FileSystem* file_system, std::string_view path);

  // Reads or writes one whole sector. Fail with kIoError, and writes with
  // kWriteProtected on a protected image.
  DiskStatus ReadSector(int64_t sector, absl::Span<uint16_t> words);
  DiskStatus WriteSector(int64_t sector, absl::Span<const uint16_t> words);
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
  a write-protected image, whatever its header says, from CL11. `Open` opens
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
  later operation on the image is `kIoError` too.
- Tests reach the `kIoError` paths through `FaultyFileProtocol`, a test-only
  `gb::MemoryFileProtocol` in `oz3_disk_test` whose files' reads, writes, and
  flushes fail once its `Limits` (how many more of each succeed) run out. So
  nothing for tests is in `DiskImage`. CL7's crash test uses it to stop the
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
  DiskMedia* GetMedia();

  // kNoDisk with no image.
  DiskStatus ReadSector(int64_t sector, absl::Span<uint16_t> words);
  DiskStatus WriteSector(int64_t sector, absl::Span<const uint16_t> words,
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
  static constexpr int kMaxFatSectors = 0xFFFF;  // As a word holds.

  auto operator<=>(const DiskLayout&) const = default;

  // Returns whether every field is in range, and the disk fits in an image of
  // `sector_count` sectors. The FAT needs ⌈(cluster_count + 1) ÷ 256⌉ sectors,
  // a word per cluster and the free list's head, but may have more.
  bool IsValid(int64_t sector_count) const;

  // Returns the sectors the disk uses: 1 + FAT + clusters.
  int64_t GetSectorCount() const;

  // Returns the first sector of `cluster` (1 to cluster_count).
  int64_t GetClusterSector(int cluster) const;

  int sectors_per_cluster = 0;  // A power of two, 1 to 64.
  int cluster_count = 0;        // 1 to 65,534.
  int fat_sectors = 0;          // At least ⌈(cluster_count + 1) ÷ 256⌉.
};

// Returns the layout that formatting an image of `sector_count` sectors gives
// with `sectors_per_cluster` (0 picks the smallest that covers the image), or
// nullopt if none does.
std::optional<DiskLayout> ChooseDiskLayout(int64_t sector_count,
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
  `GetClusterSector`, and `Check` uses the same layout for its range checks.

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
  DiskMedia* GetMedia();

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
  DiskStatus Create(const DiskFilename& name, DiskFile* file);
  DiskStatus Close(DiskFile file, DateTime now);
  DiskStatus Flush(DiskFile file, DateTime now);
  DiskStatus Flush(DateTime now);  // Every open file and buffer.

  // Data. Read and Write move at most up to the next sector boundary.
  DiskStatus Read(DiskFile file, absl::Span<uint16_t> words, int64_t* count);
  DiskStatus Write(DiskFile file, absl::Span<const uint16_t> words,
                   int64_t* count);
  DiskStatus Seek(DiskFile file, int64_t position);
  DiskStatus GetFileInfo(DiskFile file, DiskFileInfo* info);
  DiskStatus GetWritableWords(DiskFile file, int64_t* words);
  DiskStatus Truncate(DiskFile file);

  // Directory.
  DiskStatus Find(DiskFile directory, const DiskFilename& pattern,
                 DiskDirEntry* entry);
  DiskStatus Delete(const DiskFilename& name);
  DiskStatus Rename(const DiskFilename& from, const DiskFilename& to);
  DiskStatus SetAttributes(const DiskFilename& name, uint16_t attributes);

  // Consistency.
  DiskStatus Check(bool repair, DiskCheckReport* report);
};
```

- **Buffers** are fixed at construction: one sector per open file, one
  directory sector, a FAT sector holding sector 1, and one working FAT sector,
  as disk_design.md's Memory describes. A buffer that takes another sector
  writes its old one first if it is dirty. Every write goes through one
  private function that knows which buffer it is, so the commit orders in
  disk_design.md's Committing are kept in one place.
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

### disk: host transfer

`disk_transfer.h`: `WordsFromBytes` (pads an odd last byte with zero) and
`BytesFromWords`, in `std::byte`, and `WordsFromText` (host line endings to
LF) and `TextFromWords` (dropping zero bytes). They know nothing about the
file system, for `oz3dsk` and any frontend.

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

### CL7 [ ] disk: creating and writing files

Depends on: CL6.

- `DiskFileSystem`: `Create`, `Write`, `GetWritableWords`, `Flush`, and
  `Truncate`, with allocation, freeing, directory growth, timestamps, and
  every commit in the crash-safe order. `Close` gains the time to stamp with.
  Growing the root updates the size of the file system's own root slot, which
  `Open` keeps open to find names, and of any open root handle. A directory
  sector written through one buffer is updated in any other buffer holding it.
- disk_design.md: the `WRITE` walkthrough allocates as it writes (see
  Behavior).
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: creating into a full directory (growing it), `exists`, writing
  across sector and cluster boundaries, overwriting inside a file, growing
  over slack past the size, filling the disk to `disk full` with
  `GetWritableWords` exact at each step, `Truncate` at 0, mid-cluster, and on
  a cluster boundary, the free count after each, stamps at `Close` and
  `Flush` only after writes, and the exact order of sector writes for each
  commit.
- A crash test: for each operation sequence, stop the image's writes with
  `FaultyFileProtocol` after every possible number of sector writes, detach and
  reattach the image, and check that no cluster is cross-linked or on both
  the free list and a chain (with CL9's `Check` once it exists; here by
  walking the chains in the test).

### CL8 [ ] disk: deleting, renaming, and attributes

Depends on: CL7.

- `DiskFileSystem`: `Delete`, `Rename`, and `SetAttributes`.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: each on a missing file, an open file (`in use`), and a
  read-only one (`Delete`), renaming onto an existing name, bad attributes,
  the freed clusters' order on the free list, entries staying in their slots
  for an open root handle, write protection, and the crash test for each.

### CL9 [ ] disk: consistency checks

Depends on: CL8.

- `DiskFileSystem::Check` and `DiskCheckReport`: orphaned clusters,
  cross-linked clusters, clusters both free and in a chain, chains that don't
  match their file's size, and duplicate names, with repair: orphans and
  slack join the free list, a short chain cuts its file's size, and a
  cross-link cuts the later file in directory order before the shared
  cluster. Fails with `in use` while any file is open.
- The crash tests in CL7 and CL8 use it.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: each problem on a hand-made disk, reported and then repaired,
  and a clean disk reporting nothing, before and after.

### CL10 [ ] disk: host transfer helpers

Depends on: CL2.

- `disk_transfer.h` and `.cc`.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: odd and even byte counts each way, CRLF, CR, and LF going in,
  and zero bytes dropped coming out.

### CL11 [ ] disk: read-only image files

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
