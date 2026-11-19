# Booting from disk

The Ozzy boots its operating system from the disk in `A:`, in two stages, as
the Osborne Executive's CP/M Plus did. The boot ROM loads a small **loader**
from the disk's boot area (words 32 to 255 of sector 0, beside the superblock)
and starts it. The loader is the disk's own code, and loads whatever it likes,
such as an OS file, with the drive's ordinary file commands. So copying an OS
file onto a disk doesn't make it bootable: its boot area must hold a loader
too, written by `oz3dsk sys` on the host, or by an OS's own `SYSGEN` with the
drive's new `WRITE BOOT` command.

At power on, the boot ROM prints its banner and a prompt, as the Executive did,
and waits for Enter. Then it boots, or says why it can't and waits again. It no
longer echoes keys:

```
OZZY 0.2  1 MHz  1 core  56K RAM

Insert disk in drive A: and press Enter.
No disk in drive A:
Not a system disk
_
```

A loader is a default program (one with no segments) of at most 224 words,
ending in the boot signature, which the ROM checks before running it. The
assembler writes it as a **bare program**, a new and smaller program file
format for a default program, which a loader can place at any word rather
than on a page:

```
; boot.oza: assembled with oz3asm --bare, then written with
;   oz3dsk system.ozd sys boot.oz3
; The ROM starts it at IP 0, with the boot drive's command port in R0.
    ...                     ; Load OZOS.SYS after itself, and RST into it
    .DS 222 - $             ; Padding up to word 222
    .DW 'BOOT'              ; The boot signature, in the last two words
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

| Code | Command      | Arguments | Accept    | Data                | Completion           |
| ---- | ------------ | --------- | --------- | ------------------- | -------------------- |
| 44h  | `READ BOOT`  | —         | status, n | OS reads n words    | status, words read   |
| 45h  | `WRITE BOOT` | —         | status, n | OS writes n words   | status               |

- n is always 224, the whole boot area, so there is no count, and the data step
  works as `READ`'s and `WRITE`'s do, including early completion.
- **Accept** checks the disk as other handle-less commands do: **no disk**,
  **I/O error**, **write protected** for `WRITE BOOT`, then **unformatted**, as
  the boot area exists only on a formatted disk. Open files don't matter.
- **Timing:** `READ BOOT` reads sector 0 as any sector, through the read cache.
  `WRITE BOOT` writes sector 0 once all 224 words are in, keeping the
  superblock, and is committed when it completes.
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
  for a program that declares segments, or has more than one instance. Maps
  and listings are as for the default program, whose offsets are the same.
- In C++, `WriteBareProgramFile` and `ReadBareProgramFile` (and their `Words`
  forms) beside `WriteProgramFile`, holding a `BareProgram`: the sizes, and
  the data blocks as `Program::Segment::Data`.

### oz3dsk (tools, apps, manual's Disk Drive page)

```
oz3dsk <image> sys <bare program>
```

- Writes a bare program's words into the boot area, as loading it at the boot
  area's start would. Its contents must be at most 224 words; its stack isn't
  in the boot area, as the loader runs on the ROM's stack's page. Words no
  block sets are 0.
- It writes the words as they are: the boot signature is part of the program,
  so `oz3dsk` knows nothing of the Ozzy.

### The boot ROM (ozzy, manual's Ozzy Computer page)

At power on, core 0 runs the boot ROM, which:

1. Prints the banner as now, with the version raised to 0.2, then a blank line.
2. **With a keyboard**, prints the prompt, `Insert disk in drive A: and press
   Enter.`, and waits. Without one, it halts, as nothing could answer. On the
   teletype, each line ends with a carriage return and line feed.
3. **Ignores every key but Enter.** Nothing is echoed. Only the key counts, so
   Ctrl+M is Enter, as Ctrl+H is Backspace today.
4. **On Enter**, starts a new line and tries to boot from `A:`. If it can't, it
   prints why on its own line and waits for Enter again:

   | Why                                        | Message                   |
   | ------------------------------------------ | ------------------------- |
   | The Ozzy has no drives                     | `No drive A:`             |
   | **no disk**                                | `No disk in drive A:`     |
   | **unformatted**                            | `Disk not formatted`      |
   | The last two words aren't the signature    | `Not a system disk`       |
   | Any other status, such as **I/O error**    | `Disk error 21h`          |

5. **Boots:** reads the boot area with `READ BOOT` into the first 224 words of
   its stack's page (1000h to 10DFh by default), checks that its last two words
   are the boot signature, `'BOOT'` packed (4F42h, 544Fh), and starts it.

The ROM polls the drive's control port, and never takes its interrupt. Keys
typed after Enter wait in the keyboard for the loader.

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
  leaves them. Pending interrupts are kept, as `RST` keeps them.
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
- **`READ BOOT`** and **`WRITE BOOT`**, after `READ` and `WRITE`, as they move
  words the same way.
- **`oz3dsk sys`**, after DOS's `SYS` and CP/M's `SYSGEN`: making a disk a
  system disk.
- **Bare program** (`BareProgram`, `oz3asm --bare`): a program with nothing
  but its words. Not "raw", which the drive's raw directory reads, the
  console's raw mode, and Game Bits' `RawFile` already use, nor "flat" or
  "min". The **default program** is what it is assembled from; a bare program
  is its file format.
- **Boot signature**, and **system disk**, for a disk whose boot area has one.

### disk: the boot area

```
// In DiskLayout (or beside the superblock's constants):
static constexpr int kBootAreaOffset = 32;  // Word in the superblock's sector
static constexpr int kBootAreaWords = 224;

// DiskFileSystem
using BootArea = std::array<uint16_t, kBootAreaWords>;

// Reads the boot area, charging a sector read through the read cache.
// Returns GetVolumeStatus() if it isn't kOk.
DiskStatus ReadBootArea(BootArea* words);

// Writes the boot area, keeping the superblock, charging a sector write, and
// committing it. Returns GetWritableStatus() if it isn't kOk.
DiskStatus WriteBootArea(const BootArea& words);
```

- `DiskSuperblock` already holds the whole sector exactly, so writing keeps the
  superblock's words, and the boot area rides along with `SetLabel`'s rewrite.
- `DiskCheck` ignores the boot area, as nothing about it can be wrong.

### devices: READ BOOT and WRITE BOOT

- `disk_drive_command` gains the two codes. `READ BOOT` reads the boot area
  into the command's buffer once its disk work starts, and streams it as a
  `READ` streams a sector. `WRITE BOOT` takes words into the buffer, and does
  its disk work only after the 224th.

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

// Returns the default program `program` as a bare program, or an error if
// it declares segments or has more than one instance.
absl::StatusOr<BareProgram> GetBareProgram(const Program& program);
```

- `ValidateBareProgram` checks what `ValidateProgram` checks of a segment's
  data, against the contents' size.
- The program assembler already builds the default program as a `Program`,
  so `oz3asm --bare` converts it, and nothing in the assembler changes.

### tools and apps: oz3dsk sys

- `DiskFileProtocol` gains `ReadBootArea` and `WriteBootArea`, passing through
  to the file system, as `SetLabel` does.
- `DiskTool` gains `sys`, which reads the bare program, validates its size,
  lays its words into a `BootArea`, and writes it.

### ozzy: the boot ROM

- `boot_rom.oza` loses its keyboard handler and echo code, and gains the prompt,
  the wait for Enter, the drive's `READ BOOT` exchange on its three ports,
  the signature check, and the messages. It finds `A:` in the machine table
  (type 5, letter 41h), so an Ozzy with no drives says so.
- The ROM has room: it is one page, 4K words, less the machine table's 256.

### To confirm

- [ ] `DiskSuperblock` holds the whole sector, and formatting writes 0 to the
  boot area (CL1).
- [ ] `RST SELF` with every bank leaves nothing of the ROM running, and its
  `R0` and `R1` copy reaches the loader, with `R2` to `R7` free for the ROM to
  set the banks (CL5).
- [ ] How the ROM waits for Enter: halting with a keyboard handler, then
  leaving it to `RST` to clear, or polling the keyboard's port (CL5).
- [ ] `.DS 222 - $` pads a loader to its signature (`$` is in the assembler's
  expressions; `.DS` must accept it), and `.DW 'BOOT'` packs to 4F42h, 544Fh
  (CL5's test loader).
- [ ] The assembler's default program is a `Program` of one segment, all four
  roles, and one instance with zero offsets, so `GetBareProgram` can tell it
  from a program that declares one segment (CL3).

## Open questions

- [ ] The prompt and message wording, and the signature's value. The ones above
  are a first draft.
- [ ] The bare program's name and identifier. "Bare" and `424Fh` ("OB") are a
  first draft.
- [x] Whether the ROM is a public library (a fixed jump table of its console
  and drive routines, as the C64's KERNAL or the BBC Micro's MOS had), or only
  a boot ROM. Only a boot ROM: the Ozzy has one maker and fixed hardware, so
  there is no reason to split a BIOS from the OS, and the OS brings its own
  drivers, loaded after the loader in the same page.

## CLs

### CL1 [ ] disk: the boot area

Depends on: nothing.

- `DiskLayout`: the boot area's offset and size.
- `DiskFileSystem`: `ReadBootArea` and `WriteBootArea`.
- disk_design.md: the boot area's description, and its open question answered.
- Unused, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: reading a fresh format gives zeros; writing then reading round
  trips, keeping the label and everything else in the superblock; `SetLabel`
  keeps the boot area; each status (no disk, I/O error, write protected,
  unformatted); time charged for a read, a cached read, and a write.

### CL2 [ ] devices: READ BOOT and WRITE BOOT

Depends on: CL1.

- `DiskDrive`: the two commands.
- Manual: Disk Drive's Disk commands, its Layout's boot area, and Where errors
  are found.

**Verify**
- Standard checks.
- Unit tests, as programs with `DeviceProgramTest`: a read and a write through
  the ports, with n 224; each accept status; a write reset partway leaving the
  old boot area; a read and a write failing partway (early completion); the
  read cache's timing.
- Manual pages updated.

### CL3 [ ] tools, apps: bare programs

Depends on: nothing.

- `bare_program.h`: `BareProgram`, `GetBareProgram`, and
  `ValidateBareProgram`.
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

### CL4 [ ] tools, apps: oz3dsk sys

Depends on: CL1, CL3.

- `DiskFileProtocol`: `ReadBootArea` and `WriteBootArea`.
- `DiskTool` and `oz3dsk`: `sys`.
- Manual: Disk Drive's oz3dsk table.

**Verify**
- Standard checks.
- Unit tests: a bare program written and read back through the protocol,
  with unset words 0; a program file rejected; contents past 224 words
  rejected; an unformatted or protected disk.
- Manual pages updated.

### CL5 [ ] ozzy: boot from A:

Depends on: CL2, and CL4 to make system disks by hand.

- `boot_rom.oza`: the prompt, waiting for Enter, booting, and the messages; the
  echo removed; version 0.2.
- Manual: Ozzy Computer's boot ROM section, with Echoing keys replaced by
  booting, and the loader's contract (where it loads, its signature, and what
  it starts with). ozzy_design.md's Boot section to match.

**Verify**
- Standard checks.
- Unit tests in `boot_rom_test.cc`, with images made in the test: the prompt on
  the screen and on the teletype; no prompt without a keyboard; keys other than
  Enter ignored; each message (no drives, no disk, unformatted, no signature, a
  drive status); a retry after inserting a disk; a test loader that boots and
  writes to the screen, checking its banks, `R0`, `R1`, and that interrupts are
  disabled.
- By hand, left for the user: `ozzy` with a config whose `A:` holds a system
  disk made with `oz3dsk sys`.
