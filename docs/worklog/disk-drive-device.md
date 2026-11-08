# Disk drive device

The Ozzy has a screen, a keyboard, and a timer, but nowhere to keep programs and
files, so nothing can load a program but the host. The disk drive is that
place: a **smart drive** that owns its file system, as the Commodore 1541 did,
so a program (and later the Ozzy OS) asks for files by name, and the drive
keeps the directory and the space. That keeps the OS small, where a CP/M BDOS
spent most of its size on the file system.

A **disk** is a folder on the host, through Game Bits' `gb::FileSystem`, so a
program is put on a disk by copying its `.oz3` file into the folder, and a text
file written on the host reads as packed text on the Ozzy. A program asks the
drive for something with a **request block** in memory, rings the drive's port
with its address, and the drive does the work in simulated time, moving words
straight between the file and memory, a few at a time under the bank's lock,
so it contends with the cores like anything else. It writes the results into
the block and raises its interrupt.

It is the backlog's *Disk drive device*, with its background in
[ozzy_design.md](../ozzy_design.md) (Disk drive, File names), which this plan
settles. It is general purpose, like the other devices: any processor can have
drives, and the Ozzy has up to two, `A:` and `B:`.

## Behavior

### devices: the disk drive

#### Wiring and settings

A drive is wired to one port and one interrupt, and has no pages.

```
struct DiskDriveSettings {
  int port = 0;                  // The request port.
  DeviceInterrupt interrupt;     // Raised as each request finishes.
  std::string folder;            // The disk: a folder path in the file system.
  int capacity_kiw = 512;        // The disk's size, in Kiw: 1 to 65535.
  int handles = 4;               // Files open at once: 1 to 16.
  int seek_ms = 10;              // Time each request takes: 0 or more.
  int words_per_sec = 32768;     // Transfer rate, or 0 for memory speed.
};

class DiskDriveFactory final : public DeviceFactory {
 public:
  // Returns an error if `settings` are invalid. `file_system` must outlive
  // every Processor created from the factory.
  static absl::StatusOr<std::unique_ptr<DiskDriveFactory>> Create(
      std::string_view name, const DiskDriveSettings& settings,
      gb::FileSystem* file_system);
};

class DiskDrive final : public Device { ... };  // No host interface yet.
```

- A folder that doesn't exist is an empty drive: every request but a bad one
  finishes with `NO DISK`. It is checked at each request, so a folder the host
  creates later is a disk from then on.
- The defaults are a 1 MB disk (512 Kiw), and a drive about as fast as a hard
  disk of the early 1980s: at 1 MHz, a request takes 10,000 cycles to seek, and
  a word 30.5 cycles to move, so a 16 Kiw program loads in about half a second.
- Two drives (or two processors made from one factory) must not have the same
  folder, as `gb::FileSystem` doesn't allow a file to be open twice. The Ozzy
  checks its two drives' folders differ.

#### Files and names

- **The directory:** each file in the folder whose name is a valid name is a
  file on the disk, and the rest (and any subfolders) don't show. The drive
  lists the folder at each request that needs the directory, so files the host
  adds, changes, or removes show at once.
- **Names** are 12.3: 1 to 12 characters, then optionally a `.` and 1 to 3
  more, of letters, digits, and the punctuation DOS allowed, `! # $ % & ' ( ) -
  @ ^ _ ` { } ~`. In a request, a name is a field of 8 words, packed as the
  assembler packs strings with the dot included, and padded with zero bytes
  (`.DW SIZE(8) "A.TXT"`). A 16 character name fills it.
- Names aren't case-sensitive: the drive turns them to upper case, and returns
  them so. A host file's name is turned to upper case to make its name, so
  `hello.txt` on the host is `HELLO.TXT`. If two host files make the same name
  (on a case-sensitive host), only the first by host name shows. A file the
  drive creates gets its upper case name on the host.
- **Patterns**, for finding files, are names that may also have `*` (any run of
  characters, the dot included) and `?` (any one character), matched against the
  whole name. So `*` is every file, `*.TXT` every text file, and `*.*` only the
  names with a dot.
- **Size:** a file's words are its host bytes two at a time, the low byte first,
  as in program files, so its size in words is its host size in bytes, halved
  and rounded up. An odd last byte reads as a word with a high byte of 0.
- **Space:** the disk's capacity is a setting, not the host's. The space used is
  the sum of its files' sizes in words, and a write that would go past the
  capacity writes what fits and finishes with `DISK FULL`. Host files that
  already fill more than the capacity leave no free space, but nothing is lost.

#### Requests

A request block is 26 words, anywhere in any bank:

| Word   | Field       | What                                                                     |
| ------ | ----------- | ------------------------------------------------------------------------ |
| 0      | Command     | What to do (see Commands)                                                |
| 1      | Status      | Written by the drive last: 0 for success, or an error (see Statuses)     |
| 2      | Handle      | An open file, from 1. `OPEN` writes it                                   |
| 3      | Mode        | How `OPEN` opens the file (see Modes)                                    |
| 4, 5   | Position    | A dword, low word first: where a read or write starts, and is left after |
| 6      | Bank        | The bank of the words a read or write moves                              |
| 7      | Address     | Their address                                                            |
| 8      | Length      | How many words to move                                                   |
| 9      | Count       | Written by the drive: how many words it moved                            |
| 10-17  | Name        | A name, or `FIND`'s pattern                                              |
| 18-25  | Second name | `RENAME`'s new name, or the name `FIND` found                            |

**Ringing the drive:** a program writes the block's address and bank to the
port as a dword (word 0 the address, word 1 the bank), with `OUTS.ID` or
`OUTS.RD`, which sets the status. This is the same as the teletype and the
timer's clock port:

- The drive takes each request the cycle after the write, and clears the
  status, so the port is free again at once.
- The drive runs **one request at a time**. An idle drive starts the request it
  takes. A busy drive finishes it at once with `BUSY`, writing only its status
  (the bank's lock for an address and a word), raises its interrupt, and goes
  on with the one it was running. A program that gets `BUSY` rings again after
  the next interrupt. Only ever holding one request leaves room for commands a
  busy drive would answer, such as cancelling the running request or asking how
  far it has got, which aren't in this plan.
- Reading the port with `IN` or `INS` before the drive takes a request takes it
  back, and the drive never sees it.
- A bank of 16 or more uses its low 4 bits.

**Finishing:** the drive writes the fields the command gives (each command says
which), then the status, all under one lock of the block's bank, and raises its
interrupt. So a program either handles the interrupt, or sets the status to a
value no request finishes with (`FFFFh`) and waits for it to change. The
interrupt says only that a request finished: two that finish before a core
handles the first are one interrupt, so a handler checks each block it rang.

**Timing:** a request is in steps, and the drive holds nothing between them:

1. It takes the request: the port's lock, the cycle after the write.
2. It reads the block: the bank's lock, for 27 cycles (an address, and 26
   words).
3. It seeks: `seek_ms` of the clock, for every request.
4. A read or write moves its words in **bursts** of 16: each burst is the
   buffer bank's lock for an address and its words (17 cycles for a full
   burst). At a transfer rate, burst `n` starts no sooner than
   `n * 16 * clock_hz / words_per_sec` cycles after the first, so the rate
   doesn't drift; at 0 they follow each other at once. A core changing memory
   while it moves sees half a transfer, which is a quirk to keep.
5. It writes the results: the bank's lock for an address and the words of each
   field it writes, status last.

The file on the host is read or written a burst at a time, so a host watching
the folder sees a write grow, and a write cut short leaves what was written.

#### Commands

| Value | Command  | Takes                                              | Gives                                                                    |
| ----- | -------- | -------------------------------------------------- | ------------------------------------------------------------------------ |
| 1     | `FIND`   | Name (a pattern), second name (the last found)     | Second name (the next found), position (its size in words)               |
| 2     | `OPEN`   | Name, mode                                         | Handle, position (0, or the file's end for append)                       |
| 3     | `CLOSE`  | Handle                                             | Nothing                                                                  |
| 4     | `READ`   | Handle, position, bank, address, length            | Count, position (after the words read)                                   |
| 5     | `WRITE`  | Handle, position, bank, address, length            | Count, position (after the words written)                                |
| 6     | `DELETE` | Name                                               | Nothing                                                                  |
| 7     | `RENAME` | Name, second name (the new name)                   | Nothing                                                                  |
| 8     | `SPACE`  | Nothing                                            | Position (the free space in words)                                       |

- **`FIND`** is stateless: it finds the first file matching the pattern whose
  name sorts after the second name, and writes it there. A second name of all
  zeros sorts first, so a program clears it to start, and rings the same block
  again for each next file, until `NOT FOUND`. Names sort as packed words do,
  which is alphabetical, so a listing comes out sorted, and files added or
  removed between two finds don't confuse it.
- **`READ` and `WRITE`** start at the block's position and leave it after the
  words they moved, so ringing the same block again goes on where it left off,
  and seeking is writing the position. A read past the end moves what there is,
  and reads none at the end (with success, and a count of 0). A write may start
  anywhere up to the end, and makes the file longer as it goes. The words wrap
  at the end of the bank, as cores' addresses do.
- **`CLOSE`** frees the handle. A file left open stays open until the machine
  is reset (a new Processor).
- **`DELETE` and `RENAME`** fail on a file that is open. Renaming a file to its
  own name (in any case) succeeds and does nothing.

#### Modes

| Mode  | Opens                                                                     |
| ----- | ------------------------------------------------------------------------- |
| 1     | Read: an existing file, to read                                           |
| 2     | Write: an existing file, to read and write, keeping what is in it         |
| 3     | Create: a new or emptied file, to read and write                          |
| 4     | Append: a new or existing file, to write at its end, whatever the position |
| +8000h | Text, with any of them (see Text files)                                  |

A file is open on one handle at a time.

#### Text files

A file opened as **text** is its host bytes, packed two to a word with the
first in the high byte, as packed strings are, so a text file written on the
host reads as packed text, and back:

- **Reading** packs the bytes, and pads an odd last byte with a zero byte.
- **Writing** writes each word's two bytes, the high byte first, and skips a
  zero byte, as the teletype does. So a packed string with its zero end (`.DW
  "Hello", 0`) writes as `Hello`, and a single character as itself (`0041h` is
  `A`).
- **The position** of a text file is in bytes, not words, so writes that skip
  zero bytes, and appends after an odd length, follow on with no gaps. The
  length and count are still in words.

#### Statuses

| Value | Status        | When                                                                  |
| ----- | ------------- | --------------------------------------------------------------------- |
| 0     | `OK`          | Success                                                               |
| 1     | `NOT FOUND`   | No file of the name, or no more matching the pattern                  |
| 2     | `BAD NAME`    | A name that isn't one, or a pattern where only a name goes            |
| 3     | `EXISTS`      | `RENAME` to a name another file has                                   |
| 4     | `IN USE`      | The file is open: opening it again, deleting it, or renaming it       |
| 5     | `NO HANDLE`   | Every handle is open                                                  |
| 6     | `BAD HANDLE`  | A handle that isn't an open file                                      |
| 7     | `BAD MODE`    | A mode that isn't one, or a read or write the file's mode doesn't allow |
| 8     | `BAD POSITION` | A write that starts past the end                                     |
| 9     | `DISK FULL`   | A write that went past the capacity, which wrote what fit             |
| 10    | `NO DISK`     | The folder doesn't exist                                              |
| 11    | `BAD COMMAND` | A command that isn't one                                              |
| 12    | `HOST ERROR`  | The host's file system failed                                         |
| 13    | `BUSY`        | The drive was running another request                                 |

`FFFFh` is never a status, for programs to mark a request they are waiting on.

**Wiki:** a new `3.5-Disk-Drive.md` page, written over CL2 to CL5, a row in
`3-Devices.md`'s table, and a line in `Home.md`.

### Ozzy

- An Ozzy has up to two drives, `A:` and `B:`, wired the same in every Ozzy:
  `A:` on port 2 with interrupt 2, and `B:` on port 3 with interrupt 3, each on
  every core and not shared. Two drives were the classic machine.
- **The device ports move** to make room, as the only free ones (0 and 5)
  aren't next to each other, and a drive's port and interrupt are then the same
  number:

  | Port | Device                | Interrupt |
  | ---- | --------------------- | --------- |
  | 0    | Teletype (was 2)      |           |
  | 1    | Keyboard              | 1         |
  | 2, 3 | `A:`, `B:`            | 2, 3      |
  | 4, 5 | Timer (was 3, 4)      | 0         |
  | 6-15 | Keyboard key state    |           |

  Programs that use the teletype or the timer on their old ports change with
  it, which is a break, but an early one.
- A finished request is news for
  whichever core rang it, so every core with a handler is told, and each checks
  its own blocks.
- A config file gives each drive its letter and folder, and any settings:
  `{ type: "drive", letter: "A", folder: "disks/system", capacity_kiw: 360 }`.
  `letter` is `"A"` or `"B"`, and each letter is one drive. A relative folder
  is from the config file's folder, so a config and its disks move together.
  A folder that doesn't exist is an error, as a typo would be. There is no
  drive by default, as there is no folder to give it.
- The machine table lists each drive after the timer, as type 5: its port,
  interrupt, and cores, no pages, and its letter (41h or 42h) and capacity in
  Kiw as its two words.
- `ozzy` gives the Ozzy the host's file system. The boot ROM doesn't change:
  booting from `A:` is the *Ozzy OS* item.
- **Wiki:** `4-Ozzy-Computer.md`'s config, devices table, errors, machine
  table, and running.

## Design

### Names

- **`DiskDrive`**, `DiskDriveFactory`, and `DiskDriveSettings`, in
  `disk_drive.h`. "Drive" alone collides with a host's drive letters and reads
  as a verb; `FileDrive` isn't a thing. The wiki page is "Disk Drive", and the
  Ozzy's config type is `"drive"`, as the design sketched.
- **Disk** for the folder, as the design's names table chose.
- **Request block**, as the design named it, against the OS's call block.
  Alternatives: "command block", "control block" (CP/M's FCB is a different
  thing). Recommended: request block.
- **Second name** for words 18 to 25, which are the new name for `RENAME` and
  the found name for `FIND`. Alternatives: "new name" (wrong for `FIND`),
  "other name". Recommended: second name.
- **Burst**, for the 16 words moved under one lock. Alternatives: "sector"
  (suggests disk geometry the drive doesn't have), "chunk" (a catch-all).
- **`DiskName`**, in `disk_name.h`: a parsed name, against `ProgramMap`'s
  labels and the host's paths.
- Settings: `capacity_kiw`, `handles` (against `files`, which reads as the
  directory), `seek_ms`, `words_per_sec` (units named, as the C++ style asks).
- Constants: `kDriveFind` to `kDriveSpace`, `kDriveOk` to `kDriveHostError`,
  `kDriveRead` to `kDriveAppend` and `kDriveText`, and `kDriveRequestSize`, as
  the timer has `kTimerCycles`.

### devices: DiskName

```
// A file name on a disk: 12.3, upper case, of the characters names allow.
class DiskName {
 public:
  static constexpr int kFieldWords = 8;

  // Parses a name from a request's 8 word field, or a host file's name. Returns
  // nullopt if it isn't a valid name, or (unless `pattern`) has * or ?.
  static std::optional<DiskName> FromField(absl::Span<const uint16_t> field,
                                           bool pattern = false);
  static std::optional<DiskName> FromHost(std::string_view host_name);

  std::string_view GetText() const;           // "HELLO.TXT"
  std::array<uint16_t, kFieldWords> GetField() const;
  bool Matches(const DiskName& pattern) const;

  auto operator<=>(const DiskName&) const;    // As packed words compare.
};
```

- Pure functions, with no device, so they are tested on their own.
- Ordering by the packed field (not the text) is what `FIND` sorts by. Packed
  words compare as the text does, but for a zero byte against a character,
  which sorts first in both, so they agree; the field is compared anyway, as
  it is what the spec promises.

### devices: DiskDrive

- `DiskDriveFactory::Create` validates the settings (a non-empty folder, and the
  ranges above) and holds the file system pointer; each `DiskDrive` holds it
  too. `oz3_devices` gains `gb_file`, a Game Bits library already in the build.
- The drive is a small state machine over a request: idle, taking, reading the
  block, seeking, moving bursts, writing results. Each step sets a due cycle
  and sleeps until it, or waits a cycle for a lock it requested, as the timer
  does. An idle drive sleeps until its port wakes it.
- Host work happens at a step, inside `Execute`, so it stays out of simulated
  time: listing the folder at the seek's end, opening, and each burst's read or
  write of the host file. A write request flushes the file before its results,
  so the directory's sizes are current.
- Handles hold the open `gb::File`, its mode, and its host path. The position
  lives in the block, so a handle has none of its own.
- Performance: the drive wakes once per burst, and does nothing between, so a
  transfer costs the processor a compare a cycle and a few host calls per 16
  words. Listing the folder for every `FIND` is linear in the folder, which is
  fine for disks of a few hundred files.

**Brittleness:**
- A file open twice is undefined in `gb::FileSystem`, which the drive enforces
  for itself (`IN USE`), but not across drives or processors sharing a folder.
  It is documented on `DiskDriveSettings::folder`, and the Ozzy checks it.
- The file system must outlive every processor. A `DiskDrive` holds the
  pointer, like the factory, and the Ozzy owns neither, so `ozzy_main` keeps
  its file system for the program's life.

### ozzy

- `OzzyDriveSettings` (all of `DiskDriveSettings` but the wiring), and
  `std::optional<OzzyDriveSettings> drives[2]` in `OzzySettings`, read from
  `{ type: "drive", letter, folder, ... }`.
- `Ozzy::Create` takes a `gb::FileSystem*`, which may be null for an Ozzy
  without drives, and checks each folder exists. Relative folders are made
  absolute by `ReadOzzySettings`'s caller, `ozzy_main`, from the config file's
  path, so the settings hold what the file system opens.
- `Ozzy::GetDrive(int index)` returns `A:` or `B:`, for tests and hosts.

### To confirm

- That `gb::FileSystem::ListFiles` on a local folder gives the host's case, and
  only the folder's own files with the non-recursive mode, and that
  `MemoryFileProtocol` has folders to list (CL2).
- That `File::SeekTo` at the end, then `Write`, extends a file, and how a short
  host write shows (CL3).
- That `GetPathInfo`'s size of an open file is current after `File::Flush`
  (CL3).
- Renaming: `gb::FileSystem` has no rename, so Game Bits gains one (its
  backlog's *Renaming files*), which CL5 waits for. Copying and deleting
  instead was turned down, as it is slow, and not atomic.

## CLs

### CL1 [ ] devices: disk names

Depends on: nothing.

- `disk_name.h` and `.cc`: `DiskName`, its parsing from fields and host names,
  its field, pattern matching, and ordering.
- Unused, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: valid and invalid names at each limit (12 and 3 characters, a
  16 character field with no zero byte, a second dot, an empty part, each
  allowed and refused character), case, host names, patterns (`*`, `?`, `*.*`
  against names with and without a dot), and ordering against packed fields.

### CL2 [ ] devices: the drive and its requests

Depends on: CL1.

- `disk_drive.h` and `.cc`: the settings, the factory, and the drive: taking a
  request from the port, reading the block, the seek, writing results, and the
  interrupt, with `FIND`, `SPACE`, `NO DISK`, `BAD COMMAND`, and `BUSY`.
- `CMakeLists.txt`: the new files, and `gb_file`.

**Verify**
- Standard checks.
- Unit tests, with programs ringing the drive (`DeviceProgramTest`) over a
  `MemoryFileProtocol` folder:
  - the factory's wiring, and its errors for each setting;
  - the port: taken the cycle after `OUTS`, a request while another runs
    finishing with `BUSY` and the first unharmed, and `IN` taking one back;
  - exact cycles for each step, with and without a seek time, and a core
    waiting on the block's bank while the drive reads it;
  - `FIND` through a folder, sorted, with patterns, hidden host files, two host
    names of one name, and files added between finds;
  - `SPACE` against the capacity, `NO DISK` for a missing folder, and a folder
    created later;
  - the interrupt at each finish, and the drive asleep while idle.
- Wiki: `3.5-Disk-Drive.md` (wiring, settings, names, requests, timing, `FIND`
  and `SPACE`), `3-Devices.md`, and `Home.md`.

### CL3 [ ] devices: reading and writing files

Depends on: CL2.

- `disk_drive.cc`: handles, `OPEN` (read, write, create, append), `CLOSE`,
  `READ`, and `WRITE` in bursts at the transfer rate, with the capacity.

**Verify**
- Standard checks.
- Unit tests: each mode, `IN USE`, `NO HANDLE`, `BAD HANDLE`, `BAD MODE`;
  reading across the end, an odd byte, and position following on; writing
  within, at the end, and past it (`BAD POSITION`), and to the capacity
  (`DISK FULL`); bursts' exact cycles and the rate's schedule, a core
  contending for the buffer bank between bursts, and wrapping at the bank's
  end; what the host file holds after each.
- Wiki: `3.5-Disk-Drive.md`'s commands and modes.

### CL4 [ ] devices: text files

Depends on: CL3.

- `disk_drive.cc`: the text mode, with positions in bytes.

**Verify**
- Standard checks.
- Unit tests: reading even and odd lengths, writing packed strings with zero
  ends and single characters, appending after an odd length, and seeking by
  bytes; the host bytes after each.
- Wiki: `3.5-Disk-Drive.md`'s text files.

### CL5 [ ] devices: deleting and renaming

Depends on: CL3, and Game Bits *Renaming files* (see To confirm).

- `disk_drive.cc`: `DELETE` and `RENAME`.

**Verify**
- Standard checks.
- Unit tests: each, on missing, open, and badly named files, `EXISTS`, a
  rename to its own name in another case, and the host folder after each.
- Wiki: `3.5-Disk-Drive.md`'s `DELETE` and `RENAME`.

### CL6 [ ] ozzy: the teletype and timer move

Depends on: nothing (it can land any time before CL7).

- `ozzy_settings.h`: the teletype on port 0, and the timer on ports 4 and 5.
- Every test and program that uses the old ports (`ozzy_test.cc`, the boot
  ROM if it uses either, and the comments that name them).

**Verify**
- Standard checks.
- Unit tests: the existing teletype and timer tests, on their new ports, and
  the machine table's entries for both.
- By hand: run `ozzy`, to see the teletype and timer still work.
- Wiki: `4-Ozzy-Computer.md`'s devices table, machine table, and examples.

### CL7 [ ] ozzy, apps: drives in the Ozzy

Depends on: CL3 and CL6 (CL4 and CL5 aren't needed to wire it).

- `ozzy_settings.h` and `.cc`: the wiring constants, `OzzyDriveSettings`, and
  reading `{ type: "drive" }`.
- `ozzy.h` and `.cc`: the file system, the drives, `GetDrive`, the folder
  check, and the machine table's entries.
- `machine_table.h`: type 5.
- `ozzy_main.cc`: the host's file system, and folders from the config's path.
- `configs/`: a drive in `console.ozzy`, commented out, as no disk comes with
  the repository until the *Ozzy OS*.
- Tests in `ozzy_settings_test.cc` and `ozzy_test.cc`.

**Verify**
- Standard checks.
- Unit tests: reading drives and their errors (a bad letter, two of a letter,
  a missing folder, two drives of one folder), the wiring, and the machine
  table with no drive, `A:`, and both.
- By hand: run `ozzy` with a drive, to see it starts, and a missing folder's
  error.
- Wiki: `4-Ozzy-Computer.md`.
