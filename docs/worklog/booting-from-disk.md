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

A loader is an ordinary program of at most 224 words, ending in the boot
signature, which the ROM checks before running it:

```
; boot.oza: assembled with oz3asm, then written with
;   oz3dsk system.ozd sys boot.oz3
; The ROM starts it at IP 0, with the boot drive's command port in R0.
    .SEGMENT boot CODE
    ...                     ; OPEN OZZY.SYS, READ it into RAM, RST into it
    .DS ...                 ; Padding up to word 222
    .DW 'BOOT'              ; The boot signature, in the last two words
```

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

### oz3dsk (tools, apps, manual's Disk Drive page)

```
oz3dsk <image> sys <program>
```

- Writes a program file's words into the boot area. The program must be one
  segment, with all its data in its first 224 words. Words it doesn't set are
  0.
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

### tools and apps: oz3dsk sys

- `DiskFileProtocol` gains `ReadBootArea` and `WriteBootArea`, passing through
  to the file system, as `SetLabel` does.
- `DiskTool` gains `sys`, which reads the program file, validates its shape,
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
  set the banks (CL4).
- [ ] How the ROM waits for Enter: halting with a keyboard handler, then
  leaving it to `RST` to clear, or polling the keyboard's port (CL4).
- [ ] How a loader's source pads to word 222 for its signature: a `.DS` whose
  count comes from a label, if the assembler's expressions allow it, or else a
  hand-counted `.DS`, with any assembler addition left to the backlog. And that
  `.DW 'BOOT'` packs to 4F42h, 544Fh (CL4's test loader).

## Open questions

- [ ] The prompt and message wording, and the signature's value. The ones above
  are a first draft.
- [ ] Whether the ROM should exist as a public library (a fixed jump table of
  its console and drive routines that the OS calls, as the C64's KERNAL or the
  BBC Micro's MOS had), or stay only a boot ROM, with the OS bringing its own
  drivers, as CP/M's BIOS on disk did. This plan assumes the second; the first
  would be its own backlog item.

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

### CL3 [ ] tools, apps: oz3dsk sys

Depends on: CL1.

- `DiskFileProtocol`: `ReadBootArea` and `WriteBootArea`.
- `DiskTool` and `oz3dsk`: `sys`.
- Manual: Disk Drive's oz3dsk table.

**Verify**
- Standard checks.
- Unit tests: a program written and read back through the protocol; programs
  rejected for two segments or data past word 223; an unformatted or protected
  disk.
- Manual pages updated.

### CL4 [ ] ozzy: boot from A:

Depends on: CL2, and CL3 to make system disks by hand.

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
