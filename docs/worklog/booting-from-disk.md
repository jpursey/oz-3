# Booting from disk

The Ozzy boots its operating system from the disk in `A:`, in two stages, as
the Osborne Executive's CP/M Plus did. The boot ROM loads a small **loader**
from the disk's boot area (words 32 to 255 of sector 0, beside the superblock)
and starts it. The loader is the disk's own code, and loads whatever it likes,
such as an OS file, with the drive's ordinary file commands. So copying an OS
file onto a disk doesn't make it bootable: its boot area must hold a loader
too, written by `oz3dsk boot` on the host, or by an OS's own `SYSGEN` with the
drive's `WRITE BOOT` command.

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

A loader is a **bare program**, a program file format for the default program
with none of a program file's page structure, so it can be placed at any word:

```
; boot.oza: assembled with oz3asm --bare, then written with
;   oz3dsk system.ozd boot boot.oz3
; The ROM starts it at IP 0, with the boot drive's command port in R0.
    .STACK 0                ; It runs on the ROM's stack's page
    ...                     ; Load OZOS.SYS after itself, and RST into it
    .DS 222 - $             ; Padding up to word 222
    .DW "BOOT"              ; The boot signature, in the last two words
```

The manual is the specification of what a program and a user see:
- [Disk Drive](../manual/3.5-Disk-Drive.md): the boot area in its Layout,
  `READ BOOT` and `WRITE BOOT` in its Disk commands, and `oz3dsk boot`.
- [Program Files](../manual/2.4-Program-Files.md#bare-programs): the bare
  program format, and [Program Source](../manual/2.5-Program-Source.md)'s
  `oz3asm --bare`.
- [Ozzy Computer](../manual/4-Ozzy-Computer.md#the-boot-rom): the boot ROM's
  banner, prompt, and statuses, what the loader starts with, and where the
  boot ROM's stack is.

disk_design.md's Layout (the boot area) and ozzy_design.md's Boot section
match it. The loader and the OS after it are the user's to write (see the
backlog's *Ozzy OS*), so this feature ends where the loader starts. This
summary covers how it is built, and the decisions behind it.

## Behavior

- **The boot area** is 224 words that the file system never gives a meaning:
  `FORMAT` zeroes it and `LABEL` keeps it. The drive and `oz3dsk` treat it as
  plain words, and only the Ozzy's boot ROM reads them as a loader with a boot
  signature, so `oz3dsk` knows nothing of the Ozzy.
- **`READ BOOT` and `WRITE BOOT`** (44h and 45h) always move the whole boot
  area, as 112 dwords low word first, so there is no count in the accept or
  the completion, and the OS's loop is the dword form of `INR` or `OUTR`.
  - Their accept checks the disk as other handle-less commands do (no disk,
    I/O error, write protected for `WRITE BOOT`, then unformatted). Open files
    don't matter.
  - `READ BOOT` takes no disk time, as the drive holds sector 0 from when the
    disk went in. It completes **ok** unless the disk was changed while the OS
    read its accept and fails those checks: then it sends zeros and completes
    with that status. A disk ejected after that doesn't stop it.
  - `WRITE BOOT` writes sector 0 once it has every word, keeping the
    superblock, so a reset before then writes nothing, and a boot area is
    never half old and half new. Its completion is that write's status.
- **Bare programs:** a 6-word header (identifier `424Fh`, version 1, contents
  size, stack size, block count, reserved), then data blocks as in a program
  file. Every field is a word and every block whole dwords, so a loader can
  read one with `READ PACKED`. Loaded at base b, `BC` and `BD` are b and `BS`
  is b plus contents plus stack, with `SP` 0. It has its own identifier rather
  than a program file version, so each format versions on its own and each
  reader rejects the other for the right reason. The OS a loader loads can be
  one too, placed straight after the loader in the same page, which a
  page-placed program file can't say. The assembler encodes a label in `S()`
  from the end of its segment's pages, and in `E()` from its start, where
  loading a program file puts `BS` and `BE`, so a program with a label in
  either can't be a bare one; `D()` addresses the same words either way.
- **`oz3dsk <image> boot <bare program>`** lays the program's words into the
  boot area as loading it at the area's start would, with unset words 0. Its
  contents must fit in 224 words; its stack doesn't count, as the loader runs
  on the ROM's stack's page.
- **The boot ROM** (version 1.0) shows its banner and, with a keyboard, the
  prompt, and waits for Enter; without one it halts after the banner. On the
  screen the banner is in a box drawn with the cells' edge lines, and every
  status replaces the last on one status row, so the screen never scrolls or
  wraps, and a screen too small for it is clipped. On the teletype each status
  is a line, and the prompt comes again after an error.
  - **Waiting:** it `WAIT`s with interrupts enabled, and the keyboard handler
    only does `WAKE SELF`. Only Enter counts, by its key alone, so modifiers
    don't matter. Keys typed during the banner count once it prompts.
  - **Booting:** it hides the cursor, shows `Loading...`, resets drive `A:`,
    and polls a `READ BOOT` into the first 224 words of its stack's page,
    never taking the drive's interrupt. It checks the signature (`"BOOT"`
    packed, 4F42h, 544Fh), then shows `Ready` or one of the five errors (no
    drive, no disk, unformatted, not a system disk, or the drive's status in
    hex). Keys typed while it boots are thrown away, after a failure and
    before the loader, so they neither count toward the next prompt nor reach
    the loader.
  - **Starting the loader:** it clears the keyboard's handler, throws away the
    keys, and drops its own pending interrupts (the drive's for `READ BOOT`)
    with `EI` then `DI` while no handler is set. Then `RST SELF, CODE | STACK
    | DATA | EXTRA`, with `R0` the drive's command port (16) and `R1` its
    number (0), gives the loader `CODE` and `DATA` at its first word, `STACK`
    at the end of the page with `SP` 0, `EXTRA` at bank 0 (the machine table at
    `E(F00h)`), no handlers, interrupts disabled, and `N` set.
  - **Its stack** is the first RAM page of the lowest bank with RAM. A program
    can't read `MB`, so the ROM finds the bank in the machine table by that
    rule, the one `Ozzy::Create` places it by.
- **Only a boot ROM,** not a public library of console and drive routines (as
  the C64's KERNAL was): the Ozzy has one maker and fixed hardware, so there is
  no reason to split a BIOS from the OS, and the OS brings its own drivers.

## Design

### Names

- **Boot area**, as disk_design.md already called it. "Boot sector" would
  suggest the whole sector, which holds the superblock too.
- **Loader**: the code in the boot area, after CP/M's `CPMLDR`. Distinct from
  the tools library's `ProgramLoader`, a host-side class, which prose names in
  full. A disk whose boot area holds one is a **system disk**, and its last
  two words are the **boot signature**.
- **`READ BOOT`** and **`WRITE BOOT`**, after `READ PACKED` and `WRITE
  PACKED`, as they move dwords the same way, and **`oz3dsk boot`** to match.
- **Bare program** (`BareProgram`, `oz3asm --bare`): a program with nothing
  but its words. Not "raw", which the drive's raw directory, the console's raw
  mode, and Game Bits' `RawFile` already use, nor "flat". The **default
  program** is what it is assembled from; a bare program is its file format.

### disk: the boot area

- `DiskSuperblock` already held sector 0's words exactly, so it gained
  `kBootAreaWords` (224), `BootArea` (a vector of that many words), and
  `GetBootArea`/`SetBootArea` over words 32 on. Writing keeps the superblock's
  words, and the boot area rides along with `SetLabel`'s rewrite.
- `DiskFileSystem::GetBootArea` reads nothing, and `SetBootArea` rewrites
  sector 0, returning **write protected** or **unformatted** as they apply.
- `DiskCheck` ignores the boot area, as nothing about it can be wrong.

### devices: READ BOOT and WRITE BOOT

- `disk_drive_command` has the two codes, with no arguments or results, and a
  `DiskCommandKind` each (`kReadBoot`, `kWriteBoot`), as their data phase is
  neither a `READ`'s nor a `WRITE`'s.
- Each has a data state of its own holding a `BootArea` as its buffer, as the
  boot area has no handle for `READ`'s and `WRITE`'s per-word file calls.
  `READ BOOT`'s accept checks only `GetVolumeStatus`, and the state after it
  calls `GetBootArea`, as `READ`'s calls `GetFileInfo`. `WRITE BOOT`'s accept
  checks `GetWritableStatus`, and its state calls `SetBootArea` after the
  112th dword, charging its busy time as any disk call is.
- Neither moves a count, so the states set 112 themselves rather than use
  `MakeTransfer` and `EndTransfer`. Their port handling is written out rather
  than shared with `ReadingState` and `WritingState` behind a flag, as it is a
  dozen lines.
- `DiskDrive::kCommandPort`, `kDataPort`, and `kControlPort`, the ports'
  offsets from the drive's first port, are public, for hosts and tests that
  drive one through its ports.

### tools and apps: bare programs

- `bare_program.h` holds `BareProgram` (contents and stack sizes, and the data
  blocks as `Program::Segment::Data`) and `ValidateBareProgram`, which checks
  what `ValidateProgram` checks of a segment's data, against the contents'
  size, and that each size fits in a word.
- `program_file.h` has `WriteBareProgramWords`/`ReadBareProgramWords` and
  their file forms beside the program file's, with `kBareProgramFileId` and
  `kBareProgramFileVersion`.
- `AsmProgram::GetBareProgram` returns the default program as a bare program,
  or an error for a program that declares segments, or whose contents (rounded
  up to whole dwords) and stack pass 64 Kiw. The assembler builds it beside the
  `Program`, as a `Program` can't say what it needs: the stack's size (folded
  into the segment's pages), contents that end in `.DS` (no data block), or
  whether `.SEGMENT` declared the one segment. Maps and listings are the
  default program's, whose offsets are the same.
- `oz3asm --bare` writes it, with the usual `.oz3` extension.

### tools and apps: oz3dsk boot

- `DiskFileProtocol` gains `GetBootArea` and `SetBootArea`, passing through to
  the file system, as `SetLabel` does.
- `DiskTool`'s `boot` reads the bare program, validates its size, lays its
  words into a `BootArea`, and writes it.

### ozzy: the boot ROM

- `boot_rom.oza` finds its devices in the machine table, drives included
  (type 5), and prints the banner, the box, the prompt, and each status with
  small routines (`draw_box`, `print_drives`, `show_status`, `prompt_again`).
  `read_boot_area` drives the drive's three ports, and `start_loader`,
  `find_stack_bank`, and `discard_keys` hand over to the loader. It no longer
  echoes keys.
- The box is the banner's cells with line attributes (1000h top, 2000h bottom,
  4000h left, 8000h right), each cell only the lines on the box's outer edges,
  as neighboring cells' lines on a shared edge would draw it twice as thick.
  The status row is cleared with the screen's fill row.
- The ROM is one page less the machine table's 256 words, which it fits with
  room to spare.
- `boot_rom.h` has `kBootSignature`, for hosts and tests that make system
  disks.
- `Ozzy::Create` `CHECK`s that the boot ROM's stack is in the lowest bank with
  RAM, the rule the ROM finds it by.

**Brittleness:**
- The boot ROM and `Ozzy::Create` each hold the rule for the stack's bank, the
  ROM because a program can't read `MB`. The `CHECK` keeps them together, and
  the backlog's *The boot ROM's stack in the machine table* would write it
  once.
- Nothing checks a loader's signature before the boot ROM does, as
  `oz3dsk boot` writes the words as they are.

### Testing

- `disk_superblock_test.cc` and `disk_file_system_test.cc`: a fresh format
  reads zeros; a write round trips, keeping the label and the rest of the
  superblock; `SetLabel` keeps it; each status; no time to get it, and a
  sector write's to set it.
- `disk_drive_test.cc`, through the ports and as `INR` and `OUTR` dword
  loops in programs: a read and a write as 112 dwords; each accept status; a
  reset partway leaving the old boot area; a failed sector write; a read
  completing ok with the disk removed partway, and sending zeros when the disk
  failed before its accept was read; the timing.
- `bare_program_test.cc`, `program_file_test.cc`, and
  `program_assembler_test.cc`: words matching the
  format exactly; `.DS` gaps as several blocks; segments rejected; each
  reader rejecting the other's file; bad sizes, offsets, and block order;
  `oz3asm --bare` end to end.
- `disk_file_protocol_test.cc` and `disk_tool_test.cc`: a bare program
  written and read back, with unset words
  0; a program file rejected; contents past 224 words; an unformatted or
  protected disk.
- `boot_rom_test.cc` (33 tests), with disk images made in the test: the box's
  cells and line attributes, and the drives line with none, one, and four
  drives; the teletype's lines; no prompt without a keyboard; keys other than
  Enter ignored, and keys while loading thrown away; each status, replacing a
  longer one; a small screen clipping; a retry after inserting a disk; and a
  test loader that boots and checks its banks, `R0`, `R1`, its interrupts,
  and that nothing the ROM raised is pending.
- By hand, left for the user: `ozzy` with a config whose `A:` holds a system
  disk made with `oz3dsk boot`.

## Building blocks

- `DiskSuperblock::GetBootArea`/`SetBootArea` and `DiskFileSystem`'s, for any
  code that reads or writes a disk's boot area.
- `READ BOOT` and `WRITE BOOT`, for the Ozzy OS's `SYSGEN`.
- Bare programs (`BareProgram`, `ValidateBareProgram`, the bare program file
  functions, `AsmProgram::GetBareProgram`, and `oz3asm --bare`), for any code
  placed at a word rather than a page, such as the OS a loader loads.
- `oz3dsk boot`, to make system disks.
- `kBootSignature` and `DiskDrive`'s public port offsets, for tests and hosts.
