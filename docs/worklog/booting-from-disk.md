# Booting from disk

The Ozzy boots its operating system from the disk in `A:`, in two stages, as
the Osborne Executive's CP/M Plus did. The boot ROM loads a small **loader**
from the disk's boot area (words 32 to 255 of sector 0, beside the superblock)
and starts it. The loader is the disk's own code, and loads whatever it likes,
such as an OS file, with the drive's ordinary file commands. So copying an OS
file onto a disk doesn't make it bootable: its boot area must hold a loader
too, written by `oz3dsk boot` on the host, or by an OS's own `SYSGEN` with the
drive's new `WRITE BOOT` command.

At power on, the boot ROM shows its banner in a box and a prompt, as the
Executive did, and waits for Enter. Then it boots, or says why it can't and
waits again, always on the same status line, so the screen never scrolls. It
no longer echoes keys (the box is drawn with the cells' edge lines, shown
here with line characters):

```
┌────────────────────────┐
│ Ozzy 1.0               │
│                        │
│ 1 core  56K RAM  1 MHz │
│ Drives: A, B           │
└────────────────────────┘

Press Enter to Boot...

ERROR: Not a system disk
```

A loader is a default program (one with no segments) of at most 224 words,
ending in the boot signature, which the ROM checks before running it. The
assembler writes it as a **bare program**, a new and smaller program file
format for a default program, which a loader can place at any word rather
than on a page:

```
; boot.oza: assembled with oz3asm --bare, then written with
;   oz3dsk system.ozd boot boot.oz3
; The ROM starts it at IP 0, with the boot drive's command port in R0.
    .STACK 0                ; It runs on the ROM's stack's page
    ...                     ; Load OZOS.SYS after itself, and RST into it
    .DS 222 - $             ; Padding up to word 222
    .DW "BOOT"              ; The boot signature, in the last two words
```

The OS a loader loads can be a bare program too, so a loader can put it
straight after itself in the same page, which a page-placed program file
can't say.

This settles disk_design.md's open question of what the boot area holds, and
replaces ozzy_design.md's plan of a boot ROM that loads `OZZY.SYS` itself. The
loader and the OS after it are the user's to write (see the backlog's *Ozzy
OS*), so this feature ends where the loader starts.

## Behavior

### The boot area (disk)

- The boot area is words 32 to 255 of sector 0, 224 words. The file system
  never reads or writes it for itself: `FORMAT` zeroes it, as it already does,
  and `LABEL` keeps it.
- What it holds is up to whoever boots from it. The drive and `oz3dsk` treat it
  as plain words, and only the Ozzy's boot ROM gives it a meaning (see below).

### The drive (devices, manual's Disk Drive page)

Two commands join the Disk group:

| Code | Command      | Arguments | Accept | Data                 | Completion |
| ---- | ------------ | --------- | ------ | -------------------- | ---------- |
| 44h  | `READ BOOT`  | —         | status | OS reads 112 dwords  | status     |
| 45h  | `WRITE BOOT` | —         | status | OS writes 112 dwords | status     |

- **Always the whole boot area,** 224 words, so there is no count, and the
  accept and completion give no count back either. It moves as dwords, as
  `READ PACKED` and `WRITE PACKED` do, low word first, so the OS's loop is the
  dword form of `INR` or `OUTR` with `R7` at 112. Otherwise the data step works
  as theirs does. An accepted `READ BOOT` completes **ok**, as it needs no
  disk, unless the disk is changed before the OS has read its accept status
  and fails the accept's checks, such as with **no disk**: then the 112 dwords
  are zeros, and it completes with that status. A
  `WRITE BOOT` fails only at its one sector write, after its last word.
- **Accept** checks the disk as other handle-less commands do: **no disk**,
  **I/O error**, **write protected** for `WRITE BOOT`, then **unformatted**, as
  the boot area exists only on a formatted disk. Open files don't matter.
- **Timing:** `READ BOOT` takes no disk time, as the drive holds sector 0 from
  when the disk was inserted, as it does the label `VOLUME` gives. `WRITE BOOT`
  writes sector 0 once all 224 words are in, keeping the superblock, and is
  committed when it completes.
- **Reset:** a `WRITE BOOT` reset before its last word writes nothing, unlike a
  file `WRITE`, so a boot area is never left half old and half new.

### Bare programs (tools, apps, manual's Program Files and Program Source pages)

A bare program is a default program (see Program Source's *The default
program*) with none of a program file's page structure: one block of memory,
code and data from its start, and its stack after them, loaded at any word of
any bank. Every field is one word, and the header is 6 words, so the header, a
block's offset and size, and a block's words are each whole dwords, and a
loader can read the whole file with `READ PACKED`:

| Words | What                                                                  |
| ----- | --------------------------------------------------------------------- |
| 0     | The identifier, `424Fh` ("OB" on a host)                              |
| 1     | The version of the format, `1`                                        |
| 2     | The size of its contents, code and data, in words                     |
| 3     | The size of its stack, in words (`.STACK`)                            |
| 4     | The number of data blocks                                             |
| 5     | Reserved, 0                                                           |
| 6...  | Each data block, as in a program file: its offset and its size in units of two words, then its words |

- **Loading** at base address b: each block's words are copied to b plus its
  offset, and words outside every block (such as `.DS`) are left as they are.
  The program needs contents plus stack words from b, all in one bank.
- **Starting:** `BC` and `BD` are b, and `BS` is b plus contents plus stack,
  so the stack is empty at the end, with `SP` 0. `BE` is left as it is.
- **Its own identifier,** not a version of the program file's: each format
  versions on its own, and a program file reader rejects a bare program for
  the right reason.
- **`oz3asm --bare`** writes one, with the extension `.oz3` as usual. It fails
  for a program that declares segments (the default program already can't have
  more than one instance), or whose contents, rounded up to whole dwords, and
  stack pass 64 Kiw. Maps and listings are as for the default program, whose
  offsets are the same.
- In C++, `WriteBareProgramFile` and `ReadBareProgramFile` (and their `Words`
  forms) beside `WriteProgramFile`, holding a `BareProgram`: the sizes, and
  the data blocks as `Program::Segment::Data`.

### oz3dsk (tools, apps, manual's Disk Drive page)

```
oz3dsk <image> boot <bare program>
```

- Writes a bare program's words into the boot area, as loading it at the boot
  area's start would. Its contents must be at most 224 words; its stack isn't
  in the boot area, as the loader runs on the ROM's stack's page. Words no
  block sets are 0.
- It writes the words as they are: the boot signature is part of the program,
  so `oz3dsk` knows nothing of the Ozzy.

### The boot ROM (ozzy, manual's Ozzy Computer page)

At power on, core 0 runs the boot ROM. It shows the banner, then, with a
keyboard, the prompt, `Press Enter to Boot...`, and waits for Enter. Without a
keyboard it shows only the banner and halts, as nothing could answer.

**The banner** is the boot ROM's version and the Ozzy's hardware, from the
machine table:

- `Ozzy 1.0`, then a blank line.
- The cores, RAM, and clock rate, as `1 core  56K RAM  1 MHz`, written as the
  banner writes them today (`2 cores`, `4770 kHz`).
- The drives, by letter, as `Drives: A, B`, or `Drives: none`.

**On the screen,** the banner is in a box drawn with the cells' edge lines
(their top, bottom, left, and right line attributes), so it takes no cells of
its own. The box is the banner's rows, and its widest line with a blank column
each side: its top row's cells have the top line, its bottom row's the bottom
line, its first column's the left line, and its last column's the right line,
so the corners have two. Below the box come a blank line, the prompt, another
blank line, and then the **status line**: empty at first, `Loading...` once
Enter is pressed, then `ERROR:` and the reason, or `Ready` just before the
loader starts. Each status replaces the last, clearing the row, so the screen
never scrolls. The cursor blinks after the prompt while the ROM waits, and is
hidden from Enter on. On a screen too small for it, what passes the right edge
or the bottom row isn't drawn.

**On the teletype,** in an Ozzy without a screen, it is the same without the
box, and each status is a line of its own, straight after the prompt. After an
error it prints a blank line and the prompt again:

```
Ozzy 1.0

1 core  56K RAM  1 MHz
Drives: A

Press Enter to Boot...
Loading...
ERROR: No disk in drive A:

Press Enter to Boot...
Loading...
Ready
```

Each teletype line ends with a carriage return and line feed.

**Waiting:** it ignores every key but Enter, and echoes nothing. Only the key
counts, so Ctrl+M is Enter, as Ctrl+H is Backspace today.

**Booting:** on Enter, it shows `Loading...`, then reads the boot area from
`A:` with `READ BOOT` into the first 224 words of its stack's page (1000h to
10DFh by default), and checks that its last two words are the boot signature,
`"BOOT"` packed (4F42h, 544Fh). If they are, it shows `Ready` and starts the
loader. If anything fails, it shows why and waits for Enter again:

| Why                                     | Status                       |
| --------------------------------------- | ---------------------------- |
| The Ozzy has no drives                  | `ERROR: No drive A:`         |
| **no disk**                             | `ERROR: No disk in drive A:` |
| **unformatted**                         | `ERROR: Disk not formatted`  |
| The last two words aren't the signature | `ERROR: Not a system disk`   |
| Any other status, such as **I/O error** | `ERROR: Disk error 25h`      |

The ROM polls the drive's control port, and never takes its interrupt. Keys
typed while it boots are thrown away, so they neither count toward the next
prompt nor reach the loader.

**What the loader starts with.** The ROM starts it with `RST SELF, CODE |
STACK | DATA | EXTRA`, so:

- `CODE` and `DATA` are the bank of the ROM's stack's page, both based at the
  loader's first word: it starts at `IP` 0, and reads its own words as `D(n)`.
- `STACK` is the same bank, based at the end of the page (2000h by default),
  with `SP` 0, so its stack starts empty at the top of the page.
- `EXTRA` is bank 0 at base 0, so the machine table is at `E(F00h)`.
- `R0` is the boot drive's command port (16 for `A:`), and `R1` its number (0
  for `A:`).
- No interrupt handlers are set and interrupts are disabled, as `RST` of `CODE`
  leaves them, and its `N` flag is set, so its first `WAIT` ends at once.
- The keyboard is empty, and no interrupt the ROM's own work raised is
  pending, such as the drive's for `READ BOOT`, so the loader's first drive
  interrupt is for its own command.
- RAM is as at power on, but for the loader itself and the ROM's stack's page,
  which the loader owns.

The other cores are still idle, as at power on, until the loader or the OS
starts them.

## Design

### Names

- **Boot area**, as disk_design.md already calls it. "Boot sector" would
  suggest the whole sector, which holds the superblock too.
- **Loader**: the code in the boot area. "Boot loader" is fine in prose, but
  "loader" alone matches CP/M's `CPMLDR`. Distinct from the tools library's
  `ProgramLoader`, a host-side C++ class, which prose should name in full.
- **`READ BOOT`** and **`WRITE BOOT`**, after `READ PACKED` and `WRITE
  PACKED`, as they move dwords the same way.
- **`oz3dsk boot`**, after the boot area it writes, matching the drive's
  commands. A disk whose boot area holds a loader is a **system disk**.
- **Bare program** (`BareProgram`, `oz3asm --bare`): a program with nothing
  but its words. Not "raw", which the drive's raw directory reads, the
  console's raw mode, and Game Bits' `RawFile` already use, nor "flat" or
  "min". The **default program** is what it is assembled from; a bare program
  is its file format.
- **Boot signature**, and **system disk**, for a disk whose boot area has one.

### disk: the boot area

```
// DiskSuperblock, which holds sector 0's words:
static constexpr int kBootAreaWords = kDiskSectorWords - 32;  // From word 32
using BootArea = std::vector<uint16_t>;  // Always kBootAreaWords words
BootArea GetBootArea() const;
void SetBootArea(const BootArea& words);

// DiskFileSystem
// Gets the boot area as the superblock holds it, without reading the disk.
// Returns kUnformatted with no file system.
DiskStatus GetBootArea(DiskSuperblock::BootArea* words) const;

// Rewrites sector 0 with the boot area set, keeping the rest of the sector.
// Returns kWriteProtected on a protected disk, and kUnformatted with no file
// system.
DiskStatus SetBootArea(const DiskSuperblock::BootArea& words);
```

- `DiskSuperblock` already holds the whole sector exactly, so writing keeps the
  superblock's words, and the boot area rides along with `SetLabel`'s rewrite.
- `DiskCheck` ignores the boot area, as nothing about it can be wrong.

### devices: READ BOOT and WRITE BOOT

- `disk_drive_command` gains the two codes, with no arguments and no results,
  and a `DiskCommandKind` each (`kReadBoot` and `kWriteBoot`), as what follows
  their accept is neither a `READ`'s nor a `WRITE`'s data phase.
- The data phase of `READ` and `WRITE` moves words to and from a file through
  its handle, one file system call per word. The boot area has no handle, so
  each command gets a data state of its own, holding a `BootArea` as its
  buffer:
  - `READ BOOT`'s accept only checks `GetVolumeStatus`, and the state after
    the accept calls `GetBootArea` for its words, as `READ`'s calls
    `GetFileInfo` for its position. A disk changed while the OS read the
    accept status can fail that call, so the state then sends zeros and keeps
    the status for its completion. It puts each dword on the data port once the
    OS has taken the last, as `ReadingState` does, but with no disk call, then
    completes. A disk ejected after it starts doesn't stop it, as it already
    has its words.
  - `WRITE BOOT`'s accept only checks `GetWritableStatus`, and its state takes
    each dword into the buffer. After the 112th, it calls `SetBootArea`,
    charges its busy time as any disk call is, and sends its status as the
    completion. A reset before then drops the buffer, so nothing is written.
- Neither moves a count, so the states set 112 themselves rather than use
  `MakeTransfer` and `EndTransfer`, which take the count from the accept and
  send it back in the completion. Their port handling is written out rather
  than shared with `ReadingState` and `WritingState` behind a flag, as it is a
  dozen lines.

**Brittleness:** none new: the drive's phases already make a caller read the
accept before the data, and the data before the completion.

### tools and apps: bare programs

```
// oz3/tools/bare_program.h
struct BareProgram {
  int contents_words = 0;  // Code and data, at most 65,536 with the stack
  int stack_words = 0;
  std::vector<Program::Segment::Data> data;  // In order, as in a segment
};

absl::Status ValidateBareProgram(const BareProgram& program);

// AsmProgram, beside GetProgram: the default program as a bare program, or an
// error if the program declares segments.
const absl::StatusOr<BareProgram>& GetBareProgram() const;
```

- `ValidateBareProgram` checks what `ValidateProgram` checks of a segment's
  data, against the contents' size, and that each size fits in a word.
- The assembler builds the bare program beside the `Program`, as a `Program`
  doesn't say what a bare program needs: the stack's size (it is folded into
  the segment's pages), the contents' size where they end in `.DS` (which has
  no data block), or whether `.SEGMENT` declared the one segment. The contents
  are the segment's words before its stack, rounded up to whole dwords, as its
  data blocks are.

### tools and apps: oz3dsk boot

- `DiskFileProtocol` gains `GetBootArea` and `SetBootArea`, passing through
  to the file system, as `SetLabel` does.
- `DiskTool` gains `boot`, which reads the bare program, validates its size,
  lays its words into a `BootArea`, and writes it.

### ozzy: the boot ROM

- `boot_rom.oza` loses its echo code, keeping a keyboard handler only to end
  its `WAIT`, and gains the boxed banner with its drives line, the prompt, the status line, the wait for
  Enter, the drive's `READ BOOT` exchange on its three ports, and the
  signature check. It finds the drives in the machine table (type 5), so an
  Ozzy with no drives says so.
- The box is the banner's cells written with line attributes (1000h top,
  2000h bottom, 4000h left, 8000h right), each cell only the lines on the
  box's outside edges, as two neighboring cells' lines on a shared edge would
  draw it twice as thick. The status line is cleared with the screen's fill
  row, the fill cell being the space the ROM cleared the screen with.
- The ROM has room: it is one page, 4K words, less the machine table's 256.

### To confirm

- [x] `DiskSuperblock` holds the whole sector, and formatting writes 0 to the
  boot area (CL1).
- [x] `RST SELF` with every bank leaves nothing of the ROM running, and its
  `R0` and `R1` copy reaches the loader, with `R2` to `R7` free for the ROM to
  set the banks (CL5). It does: `R3` is the new `MB`, and `R4` to `R7` the
  base registers. `MB` can't be read, so the ROM finds its stack's bank in the
  machine table, as the lowest with RAM, where the Ozzy places it, and the
  page from `BS`.
- [x] How the ROM waits for Enter: halting with a keyboard handler, then
  leaving it to `RST` to clear, or polling the keyboard's port (CL5). Neither:
  `HALT` ends only with `RST`, and polling alone never rests the core. The
  ROM `WAIT`s with interrupts enabled, and the keyboard's handler only does
  `WAKE SELF`, which ends the `WAIT`, or makes the next one end at once if a
  key comes before it. The ROM then reads the key from the port. Interrupts
  are disabled from Enter on, and the keys typed while it boots are thrown
  away.
- [x] `.DS 222 - $` pads a loader to its signature (`$` is in the assembler's
  expressions; `.DS` must accept it), and `.DW 'BOOT'` packs to 4F42h, 544Fh
  (CL5's test loader). `.DS 222 - $` does, but single quotes give one
  character a word, so the signature is `.DW "BOOT"`, as the double quotes
  pack. A loader needs `.STACK 0` too, as a bare program states its stack.
- [x] The assembler's default program is a `Program` of one segment, all four
  roles, and one instance with zero offsets, so `GetBareProgram` can tell it
  from a program that declares one segment (CL3). It can't: a declared
  segment with all four roles is the same `Program`, and the `Program` has no
  stack size either, so the assembler builds the bare program itself.

## Open questions

- [x] The prompt and message wording, and the signature's value: as above,
  with the banner in a box on the screen, and the status on one line that is
  replaced rather than scrolled.
- [x] The bare program's name and identifier: "bare", and `424Fh` ("OB").
- [x] Whether the ROM is a public library (a fixed jump table of its console
  and drive routines, as the C64's KERNAL or the BBC Micro's MOS had), or only
  a boot ROM. Only a boot ROM: the Ozzy has one maker and fixed hardware, so
  there is no reason to split a BIOS from the OS, and the OS brings its own
  drivers, loaded after the loader in the same page.

## CLs

### CL1 [x] disk: the boot area

Depends on: nothing.

- `DiskSuperblock`: the boot area's size and words.
- `DiskFileSystem`: `GetBootArea` and `SetBootArea`.
- disk_design.md: the boot area's description, and its open question answered.
- Unused, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: reading a fresh format gives zeros; writing then reading round
  trips, keeping the label and everything else in the superblock; `SetLabel`
  keeps the boot area; each status (no disk, I/O error, write protected,
  unformatted); no time charged for getting it, and a sector write's for
  setting it.

### CL2 [x] devices: READ BOOT and WRITE BOOT

Depends on: CL1.

- `DiskDrive`: the two commands.
- Manual: Disk Drive's Disk commands, its Layout's boot area, and Where errors
  are found.

**Verify**
- Standard checks.
- Unit tests, as programs with `DeviceProgramTest`: a read and a write through
  the ports, as 112 dwords; each accept status; a write reset partway leaving
  the old boot area; a write failing at its sector write; a read completing ok
  though the disk is ejected partway; a write's timing, and a read's taking no
  disk time.
- Manual pages updated.

### CL3 [x] tools, apps: bare programs

Depends on: nothing.

- `bare_program.h`: `BareProgram` and `ValidateBareProgram`.
- `program_assembler.h`: `AsmProgram::GetBareProgram`.
- `program_file.h`: reading and writing bare program files and words.
- `oz3asm`: `--bare`.
- Manual: Program Files gains Bare programs, and Program Source's oz3asm and
  The default program mention `--bare`.

**Verify**
- Standard checks.
- Unit tests: words written and read back, matching the format table
  exactly; several blocks from `.DS` gaps; a program with segments, or more
  than one instance, rejected; reading a program file as bare, and a bare
  file as a program file, failing on the identifier; bad sizes, offsets, and
  block order rejected; `oz3asm --bare` end to end.
- Manual pages updated.

### CL4 [x] tools, apps: oz3dsk boot

Depends on: CL1, CL3.

- `DiskFileProtocol`: `GetBootArea` and `SetBootArea`.
- `DiskTool` and `oz3dsk`: `boot`.
- Manual: Disk Drive's oz3dsk table.

**Verify**
- Standard checks.
- Unit tests: a bare program written and read back through the protocol,
  with unset words 0; a program file rejected; contents past 224 words
  rejected; an unformatted or protected disk.
- Manual pages updated.

### CL5 [x] ozzy: boot from A:

Depends on: CL2, and CL4 to make system disks by hand.

- `boot_rom.oza`: the boxed banner and its drives line, the prompt, waiting
  for Enter, booting, and the status line; the echo removed; version 1.0.
- Manual: Ozzy Computer's boot ROM section, with Echoing keys replaced by
  booting, and the loader's contract (where it loads, its signature, and what
  it starts with). ozzy_design.md's Boot section to match.

**Verify**
- Standard checks.
- Unit tests in `boot_rom_test.cc`, with images made in the test: the boxed
  banner's cells and their line attributes, including the drives line with none,
  one, and four drives; the teletype's lines, with the blank line and prompt
  again after an error; no prompt without a keyboard; keys other than Enter
  ignored; each status (no drives, no disk, unformatted, no signature, a drive
  status), replacing a longer one on the same row; a small screen cutting the
  banner off rather than scrolling; a retry after inserting a disk; a test
  loader that boots and writes to the screen, checking its banks, `R0`, `R1`,
  and that interrupts are disabled.
- By hand, left for the user: `ozzy` with a config whose `A:` holds a system
  disk made with `oz3dsk boot`.
