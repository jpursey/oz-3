# Disk tools

The host side of disks, from [disk_design.md](../disk_design.md) (Tools), built
on the disk library ([disk-library.md](disk-library.md)). The library is a
complete file system, but a host can only reach it through `DiskFileSystem`'s
own calls, and nothing makes or fills an image short of writing C++. This adds
three things in `tools`, and one app:

- **`DiskFileProtocol`:** a `gb::FileProtocol` over a mounted image, so host
  code reaches the files on a disk as it reaches local or in-memory files:
  `gb::FileSystem::CopyFile("local:/hello.oz3", "ozdisk:/HELLO.OZ3")`, and the
  program loader and assembler read and write disks unchanged.
- **Host text transfer** (`disk_transfer.h`): host text to and from the
  bytes of an OZ-3 file, as the design's text convention has it.
- **`DiskTool`:** each of `oz3dsk`'s commands as a call on a `gb::FileSystem`,
  so they are tested with images in memory, as Ozzy's console frontend is in
  its library rather than its app.
- **`oz3dsk`:** the command line tool, in `apps`, which is only `gb_cli`
  wiring over `DiskTool`.

```
gb::FileSystem host;  // With "local" (or "mem", in tests) registered.
auto protocol = std::make_unique<DiskFileProtocol>(HostNow);
DiskFileProtocol* disk = protocol.get();
host.Register(std::move(protocol));  // As "ozdisk".
disk->Mount(*DiskImage::Open(&host, "local:/work.ozd"));
host.CopyFile("local:/hello.oz3", "ozdisk:/HELLO.OZ3");
disk->Unmount();  // Commits every open file, then detaches the image.
```

```
oz3dsk work.ozd new 720K
oz3dsk work.ozd format --label WORK
oz3dsk work.ozd put hello.oz3
oz3dsk work.ozd dir *.OZ3
```

The disk library gains status text for their messages. Nothing an OZ-3
program can see changes. The manual gains `oz3dsk` and a paragraph on the
protocol (see Manual).

## Behavior

The protocol and `oz3dsk` behave as disk_design.md's Tools section describes:
paths, flags, the byte view, attributes, moves, ownership, every command, sizes,
disk names, binary and text, and attributes in copies. This plan doesn't repeat
them. It settles the points below, and updates disk_design.md to match.

- **Copies don't keep file times.** disk_design.md had `put` stamp a file
  with the host file's modified time, and `get` set the host file's time from
  the entry. `gb::FileSystem` has no file times at all (`PathInfo` is only a
  type, a size, and attributes), so neither can be done through it. A file
  written through the protocol is stamped from the protocol's clock as it is
  committed, so `put` stamps the time of the copy, and `get` leaves the host
  file's time as the host sets it, which is all a disk copy needs.
- **One file system for the protocol's life.** The protocol owns one
  `DiskFileSystem`, made when the protocol is, and mounting attaches an image
  to it. So a `RawFile` open across an unmount holds a stale slot, as the
  drive's slots do: every read and write fails, and destroying it frees the
  slot. It can never write into the next image mounted.
- **Unmounting commits.** `Unmount` commits every open file (`Flush`), then
  detaches the image, returning it, so an image unmounted holds everything
  written through the protocol, and a caller can't forget to flush first.
  Mounting with an image mounted unmounts it first, and drops it.
- **The root stays open while mounted.** Mounting a formatted image opens the
  root in a slot of its own, kept until unmount, and the protocol finds names
  and lists through it, so `GetPathInfo` and `List` never need a free slot.
  The root's slot counts against the protocol's open files, so the default of
  16 leaves 15 for files. The file system itself isn't exposed, so nothing
  else can open the root or format under the protocol: the protocol passes
  on what the tool needs (`GetMedia`, `GetVolumeInfo`, and `SetLabel`).
- **Formatting is the protocol's.** The file system won't format with a file
  open, the root included, so the protocol has its own `Format`, which closes
  the root, formats, stamped by the clock, and opens the root again.
- **An unformatted image mounts.** `Mount` returns the file system's status:
  `kOk`, `kUnformatted`, or `kIoError`. An unformatted image stays mounted,
  with no root open, so `Format` can format it. Until it is formatted, the
  root is an empty folder, and creating a file fails.
- **Odd bytes are byte-exact within a file.** Reading from an odd byte offset
  starts with the high byte of the word it is in, and writing there changes
  only that byte, reading the word first. Only the end rounds up: writing an
  odd number of bytes at the end pads the last word with a zero high byte, and
  a file's size is its words × 2. So a `RawFile` written a byte at a time, as
  a `gb::File` may do, writes the same words as one written all at once.
- **Sizes are as committed.** `PathInfo`'s size, and `dir`'s, come from the
  file's directory entry, which for a file open for writing is as it was last
  committed, as the drive's `FIND` gives it.
- **`dir` lists through the protocol,** as disk_design.md has it: the names
  from `ListFiles`, and each one's size and attributes from `PathInfo`. It
  shows no times until `PathInfo` has them (Game Bits *File modified times*,
  then OZ-3 *Modified times in oz3dsk dir*).
- **`new` won't replace a file.** `DiskImage::Create` replaces any file at its
  path, but `oz3dsk new` fails if the image already exists, so a typo never
  blanks a disk. To remake one, delete it first.
- **`get`'s host file is optional,** as `put`'s disk name is: without it, the
  file is written in the current directory under its dotted disk name
  (`HELLO.TXT`).
- **`del` deletes every match** it can, hidden files included, as the drive's
  `DELETE` of each name would, and reports each it couldn't (such as a
  read-only file). It fails if any failed, or if nothing matched. An empty
  pattern fails, rather than matching every file as `dir`'s does, so a
  missing argument never empties a disk.
- **Messages.** Every failure names what failed: the image's path, the disk
  name, or the host file, then what went wrong. A failure of the image itself
  has a `DiskStatus` as its text (see `GetDiskStatusText`), such as
  `work.ozd: write protected`. A file command goes through `gb::FileSystem`,
  whose calls say only whether they failed, so a failure there says what
  couldn't be done to the file, such as `work.ozd: HELLO.TXT: can't be
  written`, whether the file is read-only, the disk write protected or full,
  or the disk broken. Only what the file system can be asked is told apart:
  a name that isn't there, one that is taken, or one that isn't a disk name.
  Failures give their reason once `gb::FileSystem` returns `absl::Status`
  (Game Bits *File system errors as absl::Status*, then OZ-3 *Disk tool
  failure reasons*).
- **File commands need a file system.** A file command on an unformatted or
  corrupt disk fails with the status `Mount` returns, before it does
  anything, rather than as a failure of each file.

### Manual

`oz3dsk` and the image format belong with the drive, so the tool gets a
section of its own on `3.5-Disk-Drive.md`, after the disk itself, as
`oz3asm` has one on Program Source. The section is the command table, sizes,
disk names, binary and text, and attributes, with the examples above, and a
paragraph on `DiskFileProtocol` for host applications. The tools in the
manual's `README.md` gain `oz3dsk`. The drive's page is written by *Disk drive
device*'s CL7, so this plan's manual CL comes after it.

## Design

### Names

- **`DiskFileProtocol`** and its default name **`ozdisk`**, as disk_design.md
  names them. Its `RawFile` is `DiskRawFile`, private to its `.cc`.
- **`DiskTool`**, in `disk_tool.h`: `oz3dsk`'s commands as calls.
  `DiskCommands` would read as the drive's commands (`DiskDriveCommand`).
  `ImageTool` would hide that it reaches files too. Recommended: `DiskTool`.
- **Mount** and **unmount** for the protocol, as disk_design.md says, which
  keeps them apart from the file system's and drive's attach and detach,
  which the protocol's mounting does under them.
- **`GetDiskStatusText`**, in `disk_status.h`: a status as the words a person
  reads, from disk_design.md's status table in lower case (`"read-only"`,
  `"disk full"`). "Name" would suggest the enumerator's spelling (`kReadOnly`).
- **Host text transfer:** `DiskTextFromHost` and `HostTextFromDisk`, in
  `disk_transfer.h`. The backlog named word conversions (`WordsFromBytes`,
  `WordsFromText`, and back), but the protocol's files are bytes, low byte
  first, so the only caller needs text to bytes and back, and nothing needs
  words.

### disk: status text

```
// Returns `status` as a person reads it, such as "read-only", for messages.
std::string_view GetDiskStatusText(DiskStatus status);
```

- The only change the disk library needs: the protocol finds names through
  its own root handle, with the file system's `Seek` and `Find`.

### tools: DiskFileProtocol

```
class DiskFileProtocol final : public gb::FileProtocol {
 public:
  // As the drive's: the host's date and time.
  using Clock = absl::AnyInvocable<absl::CivilSecond()>;

  static constexpr std::string_view kDefaultName = "ozdisk";
  static constexpr int kDefaultMaxOpenFiles = 16;

  // Files are stamped from `clock` as they are committed, or the host's local
  // time without one. Up to `max_open_files` files may be open at once, the
  // root's slot included.
  explicit DiskFileProtocol(Clock clock = nullptr,
                            int max_open_files = kDefaultMaxOpenFiles);

  // Unmounts as Unmount does.
  ~DiskFileProtocol() override;

  // Unmounts any image mounted, then mounts `image`, which must not be null,
  // opening its root. Returns kOk, kUnformatted (it is still mounted, with no
  // root, to format), kIoError, or opening the root's failure (kCorrupt),
  // which leaves it mounted with no root.
  DiskStatus Mount(std::unique_ptr<DiskImage> image);

  // Commits every open file and closes the root, then detaches the image,
  // setting `image` to it if not null. Files still open are stale (see
  // DiskFileSystem). Returns the first failure committing, kNoDisk with
  // nothing mounted, and otherwise kOk. The image is detached whatever it
  // returns.
  DiskStatus Unmount(std::unique_ptr<DiskImage>* image = nullptr);

  // Formats the mounted disk as DiskFileSystem::Format does, stamped by the
  // clock, closing the root first and opening it again after. Returns kNoDisk
  // with nothing mounted, and kInUse with any file open.
  DiskStatus Format(const DiskLabel& label, int sectors_per_cluster);

  // The mounted image, or null, and the file system's GetVolumeInfo and
  // SetLabel, returning kNoDisk with nothing mounted.
  const DiskMedia* GetMedia() const;
  DiskStatus GetVolumeInfo(DiskVolumeInfo* info);
  DiskStatus SetLabel(const DiskLabel& label);

  // gb::FileProtocol.
  gb::FileProtocolFlags GetFlags() const override;
  std::vector<std::string> GetDefaultNames() const override;
  std::string GetCanonicalPath(std::string_view protocol_name,
                               std::string_view path) const override;
  gb::FileAttributes GetAttributeFlags() const override;

 protected:
  gb::PathInfo DoGetPathInfo(...) override;
  std::vector<std::string> DoList(...) override;
  bool BasicSetAttributes(...) override;
  bool BasicDeleteFile(...) override;
  bool BasicMoveFile(...) override;
  std::unique_ptr<gb::RawFile> BasicOpenFile(...) override;
};
```

- **In `tools`,** as disk_design.md places it: it uses only the disk library
  and `gb_file`, and `tools` is the first library a host links for working
  with the OZ-3. `tools` gains `oz3_disk` in its deps.
- **Flags:** info, list, file create, file read, file write, and move. No
  folders and no current path. **Attributes:** read-only and hidden.
- **Paths** are `/` (the root folder, which has no attributes) and
  `/<name>`, a dotted name `DiskFilename::Parse` accepts. Anything else is
  invalid. `GetCanonicalPath` gives a name parsed and formatted again, so it
  is in upper case and `hello.txt` and `HELLO.TXT` are one file. Any other path
  is its own canonical path.
- **`DoGetPathInfo`** seeks the root to 0 and `Find`s the name, giving its
  entry's size × 2, and its attributes.
- **`DoList`** walks the root with `Find` from 0, matching each formatted name
  against the pattern with
  `gb::PathMatchesPattern`, the pattern turned to upper case so case is
  ignored. The disk's own patterns aren't used, as gb's patterns are what every
  protocol's callers write. Recursive mode is the same, with no folders.
- **The `Basic*` hooks** are overridden, leaving `gb::FileProtocol`'s `Do*`
  functions to check that a path exists, is a file, and has the root as its
  folder before calling them.
- **`BasicOpenFile`:** `Open`, or `Create` for a missing file with
  `FileFlag::kCreate`, then `Truncate` at 0 for `FileFlag::kReset`. A
  read-only file fails to open for writing, so it can't be replaced by a copy,
  which `gb::FileSystem::CopyFile` relies on.
- **`DiskRawFile`** holds its slot and a byte position. `Read` and `Write`
  loop over the file system's per-sector calls, converting words to bytes low
  byte first, with the odd byte at either end handled a word at a time (see
  Behavior). `SeekTo` past the end stops at the end, as `RawFile` allows.
  `Flush` is the file system's `Flush` of the slot, stamped by the clock, and
  destroying it closes the slot, stamped the same. A failure returns -1 or
  false, as `RawFile` asks; the `DiskStatus` is lost there, as `RawFile` has
  no room for it.
- **`BasicDeleteFile`, `BasicMoveFile`, and `BasicSetAttributes`** are
  `Delete`, `Rename`, and `SetAttributes`, each refused with the file open
  (`kInUse`), as on the drive. Names are stored in upper case, so a move onto
  the same file in other case is already done.
- **Locking:** `Lock` and `Unlock` aren't overridden. The tools are
  single-threaded, and `gb::FileSystem` calls a protocol from one thread at a
  time unless the host shares it, in which case the host locks around it.

**Brittleness:** the root's position is shared by every lookup, so each one
seeks it first. The file system is private, so nothing outside the protocol
can open the root or format under it. A `RawFile` kept past `Unmount` is safe (stale), and past the
protocol's destruction is a use after free, as for any protocol, since
`gb::File` documents that its protocol must outlive it.

### tools: host text transfer

```
// Each CRLF and lone CR becomes LF. Writing an odd number of bytes pads the
// file's last word with a zero.
std::string DiskTextFromHost(std::string_view host_text);

// Every zero byte is dropped, such as an odd length's padding.
std::string HostTextFromDisk(std::string_view disk_text);
```

- In `tools`, beside the protocol, as they know nothing of the disk format.
  A file's bytes through the protocol are its words, low byte first, as
  strings are packed, so text never needs reordering. `put --text` converts
  the host file's text and writes it through the protocol, which pads an odd
  length. `get --text` reads the bytes and converts them. So text conversion
  is in one place, and any frontend can share it.

### tools: DiskTool

```
// The commands of oz3dsk, each on the image at `image_path` in the file
// system, which is opened for the command and closed after. Host paths and
// image paths are paths in the file system. Failures are absl::Status
// messages naming what failed (see Behavior).
class DiskTool final {
 public:
  using Clock = DiskFileProtocol::Clock;

  // Registers a DiskFileProtocol in `file_system` as `protocol_name`, for
  // copies, stamping files from `clock`. Fails if the name is taken.
  // `file_system` must outlive the tool.
  static absl::StatusOr<std::unique_ptr<DiskTool>> Create(
      gb::FileSystem* file_system, Clock clock,
      std::string_view protocol_name = DiskFileProtocol::kDefaultName);

  absl::Status New(std::string_view image_path, int sector_count);
  absl::Status Format(std::string_view image_path, const DiskLabel& label,
                      int sectors_per_cluster);
  absl::StatusOr<std::string> Info(std::string_view image_path);
  absl::StatusOr<std::string> Dir(std::string_view image_path,
                                  std::string_view pattern, bool all);
  absl::Status Put(std::string_view image_path, std::string_view host_path,
                   std::string_view name, bool text);
  absl::Status Get(std::string_view image_path, std::string_view name,
                   std::string_view host_path, bool text);
  absl::Status Delete(std::string_view image_path, std::string_view pattern);
  absl::Status Rename(std::string_view image_path, std::string_view from,
                      std::string_view to);
  absl::StatusOr<std::string> Attrib(std::string_view image_path,
                                     std::string_view name,
                                     gb::FileAttributes set,
                                     gb::FileAttributes clear);
  absl::Status Label(std::string_view image_path, const DiskLabel& label);
  absl::Status Protect(std::string_view image_path, bool write_protected);
  absl::StatusOr<std::string> Check(std::string_view image_path, bool fix);

  // Parses a size in words, with K and M suffixes, into sectors. Fails if it
  // isn't a whole number of sectors from 1 to kMaxSectorCount.
  static absl::StatusOr<int> ParseSectorCount(std::string_view text);

  // Returns the attributes in `letters`: r for read-only and h for hidden, in
  // either case. Fails for any other character.
  static absl::StatusOr<gb::FileAttributes> ParseAttributes(
      std::string_view letters);

  // Returns the volume label in `text`. Fails if it breaks the label rules.
  static absl::StatusOr<DiskLabel> ParseLabel(std::string_view text);
};
```

- **In `tools`, not `apps`,** so every command is unit tested over images in
  a `gb::MemoryFileProtocol`, and apps stay untested wiring. The app parses
  its labels, sizes, and attributes into these types, so a bad one is a usage
  error before any image is touched. Disk names and patterns stay text, as
  any path in the file system is, and a bad one is the command's failure.
- **Text output is returned,** for the app to write, so tests check it
  exactly: `Info`'s medium and volume, `Dir`'s lines (name, size in words,
  and `r` and `h`), `Attrib`'s letters, and `Check`'s report, one line per
  problem.
- **What each command uses,** as disk_design.md has it: `put` and `get` read
  the whole file, convert text (see host text transfer), write it, then
  `SetAttributes` with the attributes both protocols support. They don't use
  `CopyFile`, so a failure says which side it was on. A copy that fails
  leaves whatever was written, such as part of a file on a full disk, and a
  copy whose attributes can't be set fails, leaving the file it wrote.
  `ren` is `MoveFile`, and `attrib` is `SetAttributes` and `GetPathInfo`. `dir` and
  `del` take the matching names from `ListFiles` (`del` deletes each, hidden
  ones included). `format` is the protocol's `Format`, and `info`
  and `label` use its `GetMedia`, `GetVolumeInfo`, and `SetLabel`. `new`, `protect`, and `check`
  use the image alone: `DiskImage::Create`, `SetWriteProtected` on the image
  with nothing attached, and `CheckDisk` or `RepairDisk`.
- **The image** is mounted by the command that needs it and unmounted at its
  end, so the tool holds nothing between commands, and an unmount's failure
  to commit is the command's failure.

**Brittleness:** the protocol is registered for the file system's life, as
`gb::FileSystem` has no way to unregister one, so one file system can have only
one tool under a name. The tool unmounts in every command, so the protocol is
left empty between them.

### apps: oz3dsk

- `oz3dsk.cc`: the program command with the required `image` argument, then
  a subcommand per row of disk_design.md's table, each with its arguments and
  flags, as `gb_cli`'s required positional arguments before subcommands allow.
  Paths are the operating system's, as `oz3asm`'s are, in
  `gb::CreateCliFileSystem`'s file system, and are passed to `DiskTool` as
  given, so messages name them as they were typed. The clock is the host's
  local time, `DiskTool`'s default, as `ozzy`'s is.
- Sizes, labels, and `--set` and `--clear`'s letters (from `rh`) are
  validated as the command line is parsed, with `DiskTool`'s own parsing, so
  a bad one is a usage error. `--cluster` is a power of two from 1 to 64, `protect` takes
  `on` or `off` as choices, `put`'s host file must exist, and `get`'s must be
  writable. An attribute both set and cleared is `Attrib`'s own failure.
- Without a host path, `Get` writes the file in the current folder under its
  name as the disk has it (`hello.txt` is written as `HELLO.TXT`), so `get`
  passes its host file only when one is given.

### Testing

- The protocol's tests go through a `gb::FileSystem`, as callers do, with the
  image in a `gb::MemoryFileProtocol`: the byte view (odd and even sizes and
  offsets, a byte at a time and all at once, reads across sectors and
  clusters), every flag and attribute, `CopyFile` both ways keeping
  attributes, `MoveFile` and case, read-only files refusing writes, replaces,
  and deletes, open files refusing deletes and renames, mounting unformatted
  and failed images (`FaultyFileProtocol`), and a `RawFile` across an unmount.
  The root: `GetPathInfo` and `List` with every other slot taken, a root that
  grows while open, `Format` formatting and reopening it, and `Format` with a
  file open.
  Game Bits' `CommonProtocolTest` isn't used: it is in `gb_file`'s tests, not
  a library, and it makes folders and names a disk can't hold.
- A `.oz3` written by `WriteProgramFile` into `ozdisk:` and read back with
  `ReadProgramFile` is the same program, as disk_design.md promises.
- `DiskTool`'s tests run each command and check its output and the image
  after, with `CheckDisk` finding nothing wrong after every change.

### To confirm

Checked against Game Bits' `main` while planning, so nothing is left open:
- `gb::PathMatchesPattern` matches the whole text, with only `*` as a
  wildcard, and case counts. So `DoList` matches each name (without its
  folder) against the pattern turned to upper case, as names are stored.
- `gb::FileSystem::CopyFile` between protocols opens the destination with
  `kCreate`, `kReset`, and `kWrite`, closes it once copied, and only then sets
  the attributes both protocols support. So a read-only copy is written before
  it becomes read-only, and a read-only destination fails to open.
- `gb_cli` takes a command's flags anywhere after its name, so
  `put hello.txt --text` works, and a command with subcommands takes its
  required arguments before the subcommand's name, so `oz3dsk work.ozd put`
  does.

## CLs

### CL1 [x] disk: status text

Depends on: nothing.

- `disk_status.h` and a new `disk_status.cc`: `GetDiskStatusText`.
- Unused, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: `GetDiskStatusText` of every status, each distinct.

### CL2 [x] tools: DiskFileProtocol

Depends on: CL1.

- `disk_file_protocol.h` and `.cc`: the protocol and `DiskRawFile`.
- `CMakeLists.txt`: `oz3_disk` in `oz3_tools`' deps, and `oz3_disk_testing`
  in its tests'.
- Unused by the apps, so no visible change.

**Verify**
- Standard checks.
- Unit tests: as Testing above.

### CL3 [x] tools: DiskTool on images

Depends on: CL2.

- `disk_tool.h` and `.cc`: `DiskTool` with `Create`, `ParseSectorCount`,
  `New`, `Format`, `Info`, `Label`, `Protect`, and `Check`.

**Verify**
- Standard checks.
- Unit tests: sizes (`K`, `M`, plain words, not whole sectors, 0, too big,
  junk); `New` over an existing file; `Format` with and without a cluster size
  and label, and a cluster size with no layout; `Info` of unformatted,
  formatted, protected, and corrupt images; `Label`; `Protect` both ways and
  on a read-only host file; `Check` of a sound and a broken image, and of
  duplicate names, with and without fixing, and its output; the host failing
  to read or write the image.

### CL4 [x] tools: DiskTool on files

Depends on: CL3.

- `disk_transfer.h` and `.cc`: host text transfer.
- `disk_tool.h` and `.cc`: `Dir`, `Put`, `Get`, `Delete`, `Rename`, and
  `Attrib`, and `ParseAttributes`.

**Verify**
- Standard checks.
- Unit tests: host text transfer with CRLF, CR, and LF going in, and zero
  bytes dropped coming out. `Put` and `Get` binary
  and text, with and without names, a host name that doesn't fit, replacing a
  file, keeping a read-only one, a full disk leaving part of the file, a write
  protected disk, and keeping attributes both ways; `Dir` with patterns, in
  any case, and `--all`; `Delete` of a pattern with a read-only match and
  with none; `Rename` onto a taken name and to a bad one; `Attrib` setting,
  clearing, and showing; every file command on an unformatted and a corrupt
  disk, and on a broken file; the host failing to read or write, or to set
  attributes; `Delete` of an empty pattern; `Put` of a missing host file and
  a host folder.

### CL5 [x] apps: oz3dsk

Depends on: CL4.

- `oz3dsk.cc` and `apps/CMakeLists.txt`.
- `disk_tool.h` and `.cc`: `ParseLabel`, for the app's label validator, and
  `Get` without a host path. `Get` finds no file for a name that isn't a disk
  name (such as `./HELLO.TXT`), like `Rename` and `Attrib`.

**Verify**
- Standard checks.
- Unit tests: `ParseLabel` of good and bad labels, and its message; `Get`
  without a host path, in a protocol with a current folder, and of paths that
  aren't disk names, with and without one.
- By hand on Linux: every command on a fresh image, `put` and `get` of a
  `.oz3` from `oz3asm` and of a CRLF text file, and each usage error. On
  Windows, left for the user, in the PR's comment.

### CL6 [x] manual: oz3dsk

Depends on: CL5, and *Disk drive device*'s CL7, which writes
`3.5-Disk-Drive.md`.

- Manual: `3.5-Disk-Drive.md`'s `oz3dsk` section and its paragraph on
  `DiskFileProtocol`, and `oz3dsk` in `README.md`'s tools.
- `disk_design.md`: the protocol's own calls in place of `GetFileSystem()`
  and `GetRoot()`, and `dir` without modified times, as built.

**Verify**
- The section against `oz3dsk --help` and this plan's Behavior.
- Its links resolve, by file name.
