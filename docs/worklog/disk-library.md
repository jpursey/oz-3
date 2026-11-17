# Disk library

The `oz3/disk` library (`oz3_disk`) is the one implementation of the disk
format in [disk_design.md](../disk_design.md): disk images, the dumb drive's
cache and timing under them, and the file system on top, with names,
timestamps, directory entries, and consistency checks and repairs. The drive
device, `DiskFileProtocol`, and `oz3dsk` are to be built on it (the backlog's
*Disk drive device* and *Disk tools*), so they can never disagree about what is
on a disk.

```
gb::FileSystem host;  // With a protocol registered, such as "mem".
absl::StatusOr<std::unique_ptr<DiskImage>> image =
    DiskImage::Create(&host, "mem:/work.img", 2880);
if (!image.ok()) { ... }

const DateTime now = DateTime::FromCivil(absl::CivilSecond(2026, 11, 14));
DiskFileSystem disk(DiskHardwareSettings{}, /*max_open_files=*/4);
disk.AttachImage(*std::move(image));  // kUnformatted.
disk.Format(*DiskLabel::Parse("WORK"), /*sectors_per_cluster=*/0, now);
DiskFile file = -1;
disk.Create(*DiskFilename::Parse("hello.txt"), now, &file);
int count = 0;
disk.Write(file, words, &count);  // Up to the next sector boundary.
disk.Close(file, now);            // Commits the data, the FAT, and the entry.

std::unique_ptr<DiskImage> detached = disk.DetachImage();
DiskCheckReport report;
CheckDisk(*detached, &report);  // DiskCheckReport{}: nothing wrong.
```

There is no device and no tool yet, so nothing an OZ-3 program can see has
changed, and no wiki page has: the image and file system format are to be
specified on the drive's wiki page, which *Disk drive device* writes. The
library is tested on its own, over images in a `gb::MemoryFileProtocol`.

## Behavior

The library is everything disk_design.md's Disk image, File system, and Disk
library sections describe, so this summary doesn't repeat them. What it
settles is how code calls it:

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
  pulling a floppy would; a host wanting it consistent calls `Flush` first.
  Attaching while an image is attached detaches it first.
- **Stale handles are the library's.** A file open when its image is detached
  stays open but stale: every operation on it is `kDiskChanged`, except
  `Close`, which frees it, failing with `kDiskChanged` if it had uncommitted
  writes, which are lost. So the drive's slots map one to one onto handles
  with no state of their own, and a tool swapping images can't write into the
  wrong disk either.
- **Time comes in as values.** `Format`, `Create`, `Close`, `Flush`, and
  `Truncate` take the `DateTime` to stamp with. A file is stamped when it is
  created, so no entry on a disk has a zero time unless something wrote it so,
  and a file written since it was last stamped is stamped again at `Close` or
  `Flush`. So the drive passes its host clock's time, and a tool copying a host
  file in passes that file's time at `Close`.
- **Busy time** is the simulated time the disk spent on the operations since
  it was last collected. It accumulates as an `absl::Duration`, and
  `DiskFileSystem::CollectBusyTime()` returns it and resets it. Today it is
  only the hardware's sector transfers, but it belongs to the file system, so
  it could charge time of its own later. The drive is to turn it into cycles
  with its clock rate, as devices turn their other real-time rates.
- **The hardware is private.** Nothing outside needs `DiskHardware` but its
  settings and time, so the file system doesn't expose it. What callers need
  of the image is its medium, which the file system's `GetMedia` returns (null
  with no image): the sector count and header flags, as `MEDIA` reports them,
  without a file system on the disk. It is const: write protection is the
  floppy's notch, changed only with the disk out of the drive, so
  `oz3dsk protect` is to call `SetWriteProtected` on an image no drive has
  attached.
- **Writes allocate as they go.** `Write` allocates each cluster when its
  first word is written, after following any chain already past the file's
  size, rather than allocating a whole request's clusters up front.
  `GetWritableWords(file)` gives, without the disk, how much a write can be
  promised: the words from its position to its size, the room left in its
  last cluster, and the free clusters'. Only the drive changes the free count,
  so the promise holds. That leaves nothing to free after a write that stops
  short, and no begin and end calls to pair.
- **A host failure fails the disk** until it is attached again. Reattaching
  the reopened image is the retry: the commit orders leave the disk as a crash
  at that point would, with at worst orphaned clusters or slack, so it is
  consistent as it was last written. Retrying within an attachment would need
  each operation to undo its changes in memory, which this avoids, so a failed
  operation needs no cleanup of its own.
- **Checks work on a detached image.** `CheckDisk` reports what is wrong with
  a disk, writing nothing, and `RepairDisk` fixes it. The drive never checks a
  disk, and a host checking one in a drive detaches it first, so no file can
  be open during a check, and a repair leaves no free count or buffer behind
  to go stale.

## Design

### Names

Every public name has a `Disk` prefix, as disk_design.md chose, so none is
confused with Game Bits' `gb::File`, `gb::FileSystem`, `gb::RawFile`, or
`gb::FileProtocol`, or with OZ-3's `Program` and its files.

- **`DiskImage`**, **`DiskHardware`**, **`DiskFileSystem`**, **`DiskFilename`**,
  **`DiskDirEntry`**, and **`DiskFile`**, as disk_design.md's table names
  them.
- **`DiskStatus`**, the drive's "status codes", which `kOk` is one of, so it
  isn't `DiskError`. The prefix keeps it from being confused with
  `absl::Status`.
- **`AttachImage`** and **`DetachImage`**, as disk_design.md's Images and disk
  changes says attaching and detaching. An unformatted image is attached with
  nothing to mount, so they aren't `Mount` and `Unmount`.
- **`DiskMedia`:** the medium, from the image's header. It is `DiskImage`'s
  base, so the hardware and file system hand it out without exposing the
  image's sectors, and a new header field reaches every caller without
  changing them.
- **`DiskVolumeInfo`** and **`DiskFileInfo`:** each `...Info` is a snapshot of
  what one layer reports: the file system the superblock describes, and an
  open file. "Disk" is the whole thing, image and all, so "volume" names the
  file system on it, as the superblock's volume label already does, and the
  drive's `DISK` command became `VOLUME` (41h) to match.
- **`DiskLabel`:** the volume label, a value like `DiskFilename` but one
  16-character field with no extension. A label isn't specific to a volume (a
  volume just has one), so it isn't `DiskVolumeLabel`, and `SetLabel` needs no
  `Volume`.
- **`DiskHardwareSettings`:** the cache size, rates, and latency, as a
  device's `<Device>Settings` holds its behavior.
- **`DiskLayout`:** what a format chooses (sectors per cluster, cluster count,
  FAT sectors), from `ChooseDiskLayout`. "Geometry" would suggest tracks and
  heads, which the disk doesn't have.
- **`DiskSuperblock`** and **`DiskFat`:** the format's own words for sector 0
  and the FAT. The image's header is already the "header".
- **`CheckDisk`**, **`RepairDisk`**, and **`DiskCheckReport`:** a disk check
  (chkdsk) is a known thing, which gives a full report rather than a pass or a
  status, so it is the one exception to "Validate" for judging. Repairing is a
  call of its own, as `CheckDisk(image, true, &report)` wouldn't say what the
  bool does. **`kLong` and `kShort`** are a pair, for a chain that doesn't
  match its size; disk_design.md's "slack" is the tail itself.
- **`CollectBusyTime()`:** returns the busy time since the last call, and
  resets it. "Busy time" says it's the disk's simulated time, not the
  caller's, and "collect" that it's gathered up and cleared, as a fare is. The
  hardware has the same call, which the file system's adds to its own.
- **`FaultyFileProtocol`:** the test protocol whose files fail once told to.
- Constants: `kDiskSectorWords` (256), and each value's own size,
  `DiskFilename::kWordCount` (8), `DiskLabel::kWordCount` (8), and
  `DiskDirEntry::kWordCount` (16). Names and labels being the same size is
  happenstance, so each has its own.

Words are `uint16_t`. Sector numbers, positions, sizes, and cluster numbers
are `int`: an image has at most the sectors a file system can use (4,194,433:
the superblock, 256 FAT sectors, and 65,534 clusters of 64 sectors), so the
largest fits with room to spare, and a file holds at most 1,073,692,672 words
(`DiskDirEntry::kMaxSize`, the largest disk's clusters but for the root's
first). Byte offsets in an image file are `int64_t`, as the largest image is
just over 2 GiB. Dwords read from a disk can exceed these, so they are checked
before they become `int`: `DiskImage::Open` refuses a header's sector count
above the limit, and `DiskDirEntry::GetSize` gives nullopt for a size above
`kMaxSize`, which `Open` returns as `kCorrupt`. The drive's dword fields will
check against the limit before they cast, so a `SEEK` beyond the largest size
is past the end, not negative.

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

- The timer latches a `DateTime` (no clock latches the default) and answers
  `kTimerDate` and `kTimerTime` with its words, so the timer and the disk
  can't drift apart.
- `FromWords` keeps what it is given, so `ToCivil` of words a program wrote
  can be nullopt; tools showing a date check it.

### disk: the library

- `oz3/disk/`, the `oz3_disk` library, after `core` in the library order. It
  links `oz3_core` (for `DateTime`), `gb_file`, and Abseil. Its tests,
  `oz3_disk_test`, hold the test-only `FaultyFileProtocol`.
- `disk_status.h` holds `DiskStatus`, with every status code the drive has.
- `disk_packed_text.h` is internal: the packed text rules names and labels
  share (the characters, upper-casing, padding, and packing).
- `DiskSectorBuffer` (`disk_sector_buffer.h`) is a sector's words, its number,
  and whether it is dirty, which the file system and the FAT both buffer
  with.

### disk: DiskMedia and DiskImage

```
// The medium, as an image's header describes it. Only DiskImage derives from
// it, setting the header's flags through WriteFlags.
class DiskMedia {
 public:
  static constexpr uint16_t kWriteProtectedFlag = 0x0001;
  static constexpr int kMaxSectorCount = DiskLayout::kMaxSectorCount;

  int GetSectorCount() const;
  uint16_t GetFlags() const;  // The header's flags word, as MEDIA reports it.
  bool IsWriteProtected() const;

  // Sets or clears the write-protect bit, writing nothing if it is unchanged.
  // kWriteProtected if the medium can't be unprotected (a read-only image
  // file), or kIoError.
  DiskStatus SetWriteProtected(bool write_protected);

 protected:
  DiskMedia(int sector_count, uint16_t flags);
  ~DiskMedia() = default;

 private:
  virtual DiskStatus WriteFlags(uint16_t flags) = 0;
};

// A disk image: a host file holding a flat array of sectors behind its header.
class DiskImage final : public DiskMedia {
 public:
  // Creates a blank image of `sector_count` sectors at `path`, replacing any
  // file there. `file_system` must outlive the image.
  static absl::StatusOr<std::unique_ptr<DiskImage>> Create(
      gb::FileSystem* file_system, std::string_view path, int sector_count);

  // Opens an existing image, for reading and writing, or only reading, as a
  // write-protected image, if the host file is read-only.
  static absl::StatusOr<std::unique_ptr<DiskImage>> Open(
      gb::FileSystem* file_system, std::string_view path);

  // Reads or writes one whole sector. kBadArgument, kIoError, and for writes,
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
  or can't be opened (for reading only, if it is read-only, or for reading and
  writing), it isn't an image (the wrong ID), its version or sector size isn't
  supported, its sector count is 0 or past the limit, or the file is too short
  to hold the sectors its header counts.
- A read-only host file (`gb::FileAttribute::kReadOnly` in its `PathInfo`) is
  a write-protected image, whatever its header says. `Open` opens it for
  reading only, and its flags have the write-protect bit set, so `MEDIA`
  reports it. The header's bit is the floppy's notch, which travels with the
  image; the host's attribute is the host's own protection, and the image
  never changes it. So clearing protection on it fails with kWriteProtected,
  which `WriteFlags` decides from the host file's open flags
  (`gb::File::GetFlags`), with no flag of its own to keep in step. Without the
  check, it would be the kIoError of a write to a file not open for writing,
  which `gb::File` refuses without making the file unusable.
- `Create` writes the header and zero sectors, 64 sectors at a time. It fails
  over a read-only file, which can't be opened for writing.
- It is write-through to the host: `WriteSector` and `SetWriteProtected`
  flush the host file before returning, so a host failure is the result of
  the write it failed, and there is no `Flush` to pair with writes. A flush is
  an OS write (the local protocol's `fflush`), not a sync to the device, so a
  flush per sector costs nothing at the drive's rates.
- Once a host read, write, or flush fails, `gb::File` is invalid, so every
  later operation that reaches the host is `kIoError` too. One that never
  reaches it is unaffected: a write to a protected image is still
  kWriteProtected, and setting protection to what it already is still
  succeeds.

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
class DiskHardware final {
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
  DiskStatus GetImageStatus() const;  // kNoDisk, kIoError once failed, or kOk.
  const DiskMedia* GetMedia() const;  // Null if empty.

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
- **A host failure** is remembered: after the first `kIoError` from its image,
  every later read and write is `kIoError`, cached reads included, until an
  image is attached. So the file system fails in one place, through
  `GetImageStatus`.
- Settings must not be negative, which the constructor checks. The drive
  validates its config before making hardware.
- The default settings take no time and cache nothing, which is what tools
  want. The drive's timing presets belong to *Disk drive device*; each gives a
  cache rate of 1,000,000 words per second (a 1 MHz OZ-3 class controller,
  about 256 µs a sector), as disk_design.md's Timing presets say.

### disk: values

```
class DiskFilename final {
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

  // Orders by the 16 bytes in reading order, as CMPT sorts.
  auto operator<=>(const DiskFilename&) const = default;

  bool IsPattern() const;

  // Byte compare where this pattern's ? matches any byte.
  bool Matches(const DiskFilename& name) const;

  std::string ToString() const;  // "HELLO.TXT", or "HELLO" with no extension.
  Words GetWords() const;
};
```

- `DiskLabel` is the same for the label (`Parse`, `FromWords`, `ToString`,
  `GetWords`), with 0 to 16 characters and no dot or patterns.
- Timestamps are core's `DateTime`, not a disk type of their own.
- `DiskDirEntry` is the 16 words with getters and setters: the name (nullopt
  for a free entry or one that breaks the rules), attributes, first cluster,
  size (nullopt past `kMaxSize`), and modified time. It holds its words
  exactly as read, and `IsFree` is its first word being 0. The attribute bits
  are `kDiskAttribReadOnly`, `kDiskAttribHidden`, and `kDiskAttribDirectory`
  (always 0 until there are subdirectories), and `kDiskAttribSettable` is the
  two `ATTRIB` sets. `kEntriesPerSector` is 16, and `FromSector` and
  `CopyToSector` read and write entry `index` of a directory sector's words.
- Dotted text with a trailing dot and no extension (`HELLO.`) is the name
  with no extension, as DOS reads it. A second dot is refused.

### disk: DiskLayout

`disk_layout.h` holds where a formatted disk's parts are, as pure arithmetic
with no file system, so disk_design.md's layout rules live in one place:

```
// Where a formatted disk's parts are: the FAT after the superblock, then the
// clusters, as the superblock records them.
struct DiskLayout {
  static constexpr int kSuperblockSector = 0;
  static constexpr int kFirstFatSector = 1;  // Holds the free list's head.
  static constexpr uint16_t kFatEnd = 0xFFFF;
  static constexpr int kMaxSectorsPerCluster = 64;
  static constexpr int kMaxClusterCount = 0xFFFE;
  static constexpr int kMaxFatSectors = 256;  // What the most clusters need.
  static constexpr int kMaxSectorCount = 4'194'433;  // The largest disk.

  auto operator<=>(const DiskLayout&) const = default;

  int GetSectorCount() const;  // 1 + FAT + clusters.
  int GetClusterSector(int cluster) const;  // `cluster` is 1 to cluster_count.
  static constexpr int GetFatSector(int index);  // Holding FAT word `index`.
  constexpr int GetClusterWords() const;
  constexpr int GetMaxFileSize() const;  // Every cluster but the root's.

  // Returns whether every field is in range, and the disk fits in an image of
  // `sector_count` sectors. The FAT needs ⌈(cluster_count + 1) ÷ 256⌉ sectors,
  // a word per cluster and the free list's head, but may have more.
  bool IsValid(int sector_count) const;

  // Returns the cluster `cluster`'s FAT word `link` gives next, 0 at the
  // chain's end, or nullopt for a link past the last cluster. A `cluster` of 0
  // is word 0, whose link is the free list's head.
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

- **`Format`** calls `ChooseDiskLayout` with the image's sector count and the
  requested sectors per cluster, refusing with `kBadArgument` on nullopt.
- **Attaching** reads the superblock's layout, and the disk is `kUnformatted`
  unless it `IsValid` with the image's sector count. A superblock may record
  more FAT sectors than the minimum, as disk_design.md allows, so attaching
  checks the layout rather than recomputing it. `kMaxFatSectors` is the FAT the
  most clusters need, so a superblock can't claim a FAT no format writes.
- **The link rule** (`GetNextCluster`) is shared by the file system, the FAT,
  and the checks, as is every cluster's sector (`GetClusterSector`).
- Being separate lets its tests cover every row of disk_design.md's table, up
  to 128 MiB, without making images that size.

### disk: DiskSuperblock

The superblock is a value, as an entry is, so the file system and the checks
read it the same way:

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

  static DiskSuperblock FromWords(const Words& words);  // Exactly as given.

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

- The file system keeps the attached disk's as `superblock_`, and `SetLabel`
  writes its words, so the boot area and reserved words are kept.

### disk: DiskFat

The FAT is a class of its own, which `DiskFileSystem` holds, so everything
about the FAT's buffers and the order they are written in is in one place:

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
  DiskStatus Write();  // The dirty buffers, sector 1 first.

  DiskStatus CountFreeClusters();  // Once, walking the free list.
  std::optional<int> GetFreeCount() const;
  DiskStatus GetFreeHead(int* head);
  DiskStatus AllocateCluster(int last_cluster, int* cluster);
  DiskStatus FreeChain(int unlink_from, int chain_first, int chain_last,
                       int chain_count);

  DiskStatus GetNextCluster(int cluster, int* next);
  DiskStatus CountChain(int chain_first, int* chain_count, int* chain_last);
};
```

- FAT sectors are only ever in the FAT's buffers, so its writes never need
  copying into the file system's other buffers.
- **Allocating** takes the free list's head, sets the head to its next, writes
  the cluster's own word as FFFFh, and links the chain's old last cluster to
  it, as disk_design.md's FAT and free list says, keeping the free count.
  When the link is in another FAT sector than the cluster's own word, the FAT
  is written before the link is made, so the link can't reach the disk before
  the end, leaving the chain running on into the free list. A dirty sector 1
  is written before the working sector, whenever the working sector moves, so
  the head reaches the disk before any link to a cluster taken from it.
- **Freeing** is the reverse: the chain's new end, the freed chain's last link
  to the old head, then sector 1 with the freed chain as the head, with one
  write for changes next to each other in a sector. It starts with every FAT
  buffer clean and writes each in its turn. An unknown free count stays
  unknown, and an empty chain frees nothing.
- `Format` takes the root's cluster with `AllocateCluster`, on a zeroed sector
  1, so which cluster the root takes is the allocator's choice, with no rule
  of its own.
- The file system holds it as a `std::optional<DiskFat>`, made with the
  superblock's layout when a disk is attached or formatted, and dropped with
  its buffers when the disk goes, so the layout has no second copy to keep in
  step.

### disk: DiskFileSystem

```
// A slot in a DiskFileSystem's open-file table, 0 to max_open_files - 1, as
// the drive's handles are.
using DiskFile = int;

class DiskFileSystem final {
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
  DiskStatus Flush(DateTime now);  // Every open file.
  DiskStatus Truncate(DiskFile file, DateTime now);

  // Read and Write move at most up to the next sector boundary.
  DiskStatus Read(DiskFile file, absl::Span<uint16_t> words, int* count);
  DiskStatus Write(DiskFile file, absl::Span<const uint16_t> words,
                   int* count);
  DiskStatus GetWritableWords(DiskFile file, int* count);
  DiskStatus Seek(DiskFile file, int position);
  DiskStatus GetFileInfo(DiskFile file, DiskFileInfo* info);

  // Directory.
  DiskStatus Find(DiskFile directory, const DiskFilename& pattern,
                  DiskDirEntry* entry);
  DiskStatus Delete(const DiskFilename& name);
  DiskStatus Rename(const DiskFilename& old_name, const DiskFilename& new_name);
  DiskStatus SetAttributes(const DiskFilename& name, uint16_t attributes);
};
```

- **Buffers** are fixed at construction: one sector per open file, one
  directory sector, and the FAT's two, as disk_design.md's Memory describes. A
  buffer that takes another sector writes its old one first if it is dirty.
  Every write goes through one private function that knows which buffer it is:
  it writes sector 1 with `kSkip` and every other with `kCache`, and a
  directory sector written through one buffer is copied into any other buffer
  holding it, so an open root handle sees each entry as it is written. So the
  commit orders in disk_design.md's Committing are kept in two places: the
  FAT's in `DiskFat`, and the order between the FAT and everything else here
  (FAT before directory when linking, directory before FAT when freeing).
- **The free count** is counted by walking the free list the first time
  `GetVolumeInfo`, `Create`, or an `Open` on a writable disk needs it, and kept
  current from then on, so every file open on a writable disk has it, and
  `GetWritableWords` never needs the disk. Attaching forgets it.
- **Handles:** a `DiskFile` is only a slot number, as the drive's handles are,
  and each open takes the lowest free slot. A slot out of range, or with
  nothing open, gets `kBadHandle`. Nothing tells a slot's file from a later
  one: a caller that keeps a handle past `Close` reaches whatever is opened
  there next, as software misusing a real drive's handles would. A slot open
  when its image was detached is stale, and gets `kDiskChanged` (see
  Behavior); it isn't free until it is closed, so the drive's slot and the
  file system's free together. A directory handle reads its chain's full
  length as its size, and refuses writes with `kWrongHandleType`. A second
  open of a file or the root is `kInUse`, and `Format` refuses with `kInUse`
  while a file is open.
- **Attaching** attaches the image to the hardware, which empties its cache
  and forgets its head, and charges the superblock's read to the busy time, as
  the drive charges attaching. Buffers and the free count belong to the
  attached image, so detaching discards them, with no write-back.
- **Creating** finds the name and the first free entry in one pass over the
  root, refusing a name already taken, open or not, with `kExists`. It writes
  the new entry (the name, `now`, and zeros) and opens the empty file. With no
  free entry, it grows the root: it writes the free list's head cluster's
  sectors in order while the cluster is still free, zeroed but for the new
  entry in its first (in order, so only the first pays the latency), then
  allocates it and links it onto the root's chain, and every open root slot
  takes the new size. With no cluster either, it is `kDiskFull`, changing
  nothing. It commits before returning: the new root cluster's sectors, sector
  1, then the working FAT sector, or without growing, just the entry's sector.
- **Writing** allocates as it goes (see Behavior). A sector the write covers
  from its start up to the size or the sector's end starts zeroed rather than
  read, as it keeps nothing of the file. It refuses a read-only file (the
  attribute is kept in the slot at open) with `kReadOnly`. The data and FAT
  buffers it changes are written when they take another sector, or by a
  commit.
- **Committing:** a slot remembers it was written since its last stamp.
  `Close` and `Flush` of a written file commit it: its dirty data sector, then
  the FAT (sector 1 first), then its entry, with the size, first cluster, and
  `now`. A file not written since, and a directory handle, write nothing.
  `Close` always frees the slot, even when it fails, returning the commit's
  failure. `Flush(now)` commits every open file, stopping at the first
  failure, which leaves no buffer dirty. `Find` returns entries exactly as
  they are on the disk, so an open file lists as it was last committed.
- **Truncating** sets the size to the position and frees the clusters past
  what it needs, and any chain left past the size, walking them before writing
  anything. A truncate that changes the file counts as a write: it flushes the
  file with the entry giving the new size (first cluster 0 at size 0), then
  frees the chain as `DiskFat` does. One at the end with nothing past it only
  flushes.
- **Deleting, renaming, and attributes** refuse an open file with `kInUse`,
  and commit before returning. `Delete` refuses a read-only file, walks the
  chain first, so a broken one changes nothing, then writes the free entry,
  then frees the chain. `Rename` finds the file (the first entry with its
  name) and whether the new name is taken in one pass; a name is taken even by
  the file itself. `SetAttributes` sets the read-only and hidden bits, keeping
  the rest, and writes nothing when they don't change.
- **Corruption:** a link out of range, a chain longer than the cluster count
  (a loop), or an entry whose size or first cluster can't be right (past the
  disk, or only one of them 0) is `kCorrupt`, never a crash or a hang.
- **Write protection and formatting** are checked by each changing operation
  before it does anything (`kWriteProtected`, then `kUnformatted`, but for
  `Format`), as well as by the image.
- **A host failure:** once the hardware has failed, every operation returns
  `kIoError`, including those its buffers could answer (`Seek`,
  `GetFileInfo`, `GetWritableWords`, and `Open` and `Find` of buffered
  sectors). `Close` still frees the slot, and a stale slot is still
  `kDiskChanged`.
- **Single-threaded.** It is called from the drive's update or a tool's main
  thread, never from two threads at once.
- Not split out: the directory. It is read through the same slot code as an
  open root handle, and growing the root grows every open root slot, so it
  would take the slots with it.

**Brittleness:** nothing is paired. Attaching reads the superblock itself,
commits happen inside the operations, a slot can't outlive its disk
unnoticed, and allocation needs no end call. A handle kept past its `Close`
isn't caught, by design: handles are slot numbers, as on the drive. What's
left is the contract that nothing else writes the image while a file system
has it, which ownership enforces within a process (the file system owns the
hardware, which owns the image), and which across processes is documented,
as disk_design.md says.

### disk: checks

`disk_check.h` checks and repairs an image no file system has attached (see
Behavior):

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

// Checks `image` as CheckDisk does, setting `report` to what was wrong, then
// repairs it, so a check finds at most duplicate names, which renaming or
// deleting the first fixes. A file whose chain doesn't match its size keeps
// what both cover. An entry with a bad name keeps its file, under the first
// unused name of FILE00000000.CHK, FILE00000001.CHK, and so on. Every cluster
// on no chain is freed.
//
// If a crash interrupts repairing, repairing again finishes the job. If the
// disk had only problems a crash could leave (see DiskCheckReport), the crash
// adds no others.
//
// If `image` is write-protected and needs repairing, nothing is written and
// kWriteProtected is returned.
DiskStatus RepairDisk(DiskImage& image, DiskCheckReport* report);
```

- **Reading:** the whole FAT is read into memory at once (64 Kiw at most),
  and the root's sectors as its chain reaches them, straight from the image.
  It takes no simulated time, which only the drive charges.
- **Claiming:** each cluster belongs to the first chain to reach it: the
  root's, then each file's in directory order, then the free list's. So a
  cross-link is the later file's problem, and a cluster both free and in a
  chain stays in the chain, with the free list broken there. A cluster no
  chain reaches is orphaned. The free list is last, as a crash never leaves a
  chain running into it. Each cluster records only the number of the walk
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
  broken root is still read up to the break. An entry is free only if all its
  words are 0, as freeing writes, though the drive takes a first word of 0 as
  free. One with only its name zeroed is a bad name, so its chain is walked,
  and repairing treats it as any other bad name.
- **Repairing** cuts every file problem the same way: the chain ends at the
  last cluster its walk claimed, and the size is cut to what those clusters
  hold, with first cluster 0 if the walk claimed none. So a long chain loses
  its tail, a short or broken one keeps its size up to the break, and a
  cross-linked file ends before the shared cluster, which stays the earlier
  file's. A broken root ends at its break, losing any entries past it. It
  writes, in order: the root sectors whose entries change, in directory order,
  then each FAT sector holding a chain's new end, and if any cluster is
  orphaned or the free list is broken, then a free list rebuilt from every
  cluster no chain claims. Entries go first so that no entry reaches a cluster
  once it is freed. The rebuild first empties the free list (word 0 to FFFFh,
  with the chain ends), then links the clusters in order, writing the FAT's
  sectors in order with sector 1 last, for the head, so a crash in it leaves
  at worst orphans. Every write goes straight to the image, so this order is
  the code's. A clean disk writes nothing.
- **The rebuilt free list** keeps each word that already links to the right
  cluster, in either form, and writes any other as a zeroed FAT has it where
  it can: 0 for the next cluster in order, or for the end at the last
  cluster. So a free part of a newly formatted disk, or of a list already in
  order, is left as it is, and only sectors that change are written. The file
  system writes explicit links, which the format reads the same. A repair is a
  host tool's, whose writes cost no simulated time, so the free list is always
  rebuilt whole, rather than pushing only the orphans onto an intact list.
- **Bad names** keep their files, renamed `FILE00000000.CHK`,
  `FILE00000001.CHK`, and so on, each the first number no name on the disk
  has, in directory order, as chkdsk does with lost chains. The drive skips
  them in listings, `OPEN`, `DELETE`, and `RENAME`, so otherwise nothing could
  ever reach them or their clusters, and freeing them would lose data a user
  may want, such as a file whose name alone was zeroed. A root holds fewer
  than 10⁸ entries (65534 × 64 × 16), so eight digits never run out. Orphaned
  clusters are freed, not made into files, as nothing says where one chain of
  them starts.
- **Duplicate names** are only reported: `Delete` and `Rename` act on the
  first, so a user can fix them.
- **Write protection** needs no check of its own: the first write fails with
  kWriteProtected before anything reaches the image, and a disk needing no
  repairs writes nothing, so it repairs with kOk.

### Testing

- **`FaultyFileProtocol`** (test only) is a `gb::MemoryFileProtocol` whose
  files' reads, writes, and flushes fail once its `Limits` (how many more of
  each succeed) run out, and which records where each write lands, so tests
  check exact orders of writes. So nothing for tests is in `DiskImage`. Game
  Bits' `TestProtocol` is for testing Game Bits' file system, so host failures
  come from it instead. A `gb::File::Write` past the end of a memory file
  extends it, and a `Read` at the end returns short rather than failing.
  Read-only image files are made with `gb::FileSystem::SetAttributes`, which
  the memory protocol supports.
- **`disk_testing.h`** (test only, beside `FaultyFileProtocol` in
  `oz3_disk_testing`) works out what tests check without the library:
  `CollectSectorWrites` turns the protocol's write offsets into sectors, and
  `FollowFatChain` follows a chain in a FAT's words whole, stopping at a link
  past the last cluster (`FatChain::broken`) or after as many clusters as the
  disk has, and `FollowFreeList` follows the free list from word 0's head.
- `date_time_test.cc` (core): words from a civil time and back, no date, and
  ordering across each word. The timer's tests pass unchanged.
- `disk_image_test.cc`: the header's exact bytes, every header error, sector
  bytes low first, write protection set, cleared, and stored in the header,
  read-only image files (opened protected, refusing writes and unprotecting,
  leaving the host file alone, and `Create` over one), an unchanged
  `SetWriteProtected` writing nothing, and each host failure, with every
  operation after it.
- `disk_hardware_test.cc`: exact times for continuous and non-continuous
  transfers, direction switches, the first transfer, cached reads, least
  recently used eviction, `kCache` and `kSkip` writes, attaching and
  replacing images, and a failed image staying failed.
- `disk_filename_test.cc`, `disk_label_test.cc`, and `disk_dir_entry_test.cc`:
  names, patterns, and labels at each limit, every character, ordering as
  `CMPT` sorts, and entries' fields round trip.
- `disk_layout_test.cc` and `disk_superblock_test.cc`: `ChooseDiskLayout`
  against every row of disk_design.md's table, `IsValid` at each limit, the
  link rule's cases, and the superblock's fields.
- `disk_fat_test.cc`, on hardware with nothing else attached: `Format`, links
  and chains and each way one is broken, counting the free list once (reading
  each FAT sector once), allocating and freeing with each write order across
  sectors, and host failures passed on.
- `disk_file_system_test.cc`, on disks whose files the tests write into the
  image by hand (`DiskEditor`) or through the file system: each operation,
  every refusal, the free count, the busy time of attaching and of sequential
  reads, stale slots, host failures and reattaching, a read-only image file
  attaching protected, and the exact order of sector writes for every commit.
- **The crash test** (in `disk_file_system_test.cc`): for each operation
  sequence (creating, creating that grows the root, writing across clusters
  and closing, writing over slack, two files written at once, growing a file
  across FAT sectors, truncating, deleting, renaming, and attributes), it
  stops the image's writes after every possible number of sector writes,
  reattaches the image, and checks it. `CheckDisk` may find only orphaned
  clusters and long chains, and the test's own walk of whole chains checks
  owners and data: each file's words are its position plus a fixed first word,
  which catches a size reaching the disk before its data, and a file may be
  lost only if the sequence creates or deletes it. Then `RepairDisk` must leave
  a clean disk with the same files and data. It fails if any commit's order is
  reversed.
- `disk_check_test.cc` builds its disks by hand in memory, with no file
  system, on a 300-cluster disk with two FAT sectors: each problem checked and
  repaired, the exact order of a repair's writes and the rebuilt FAT's words,
  bad names renamed around names already taken, write protection, and a crash
  test of repairing itself (each crash leaves only what a crash may, with no
  cluster on two chains walked whole, and repairing again finishes). Writing
  the entries after the FAT fails it.
