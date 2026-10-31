# Ozzy Computer

Ozzy is a virtual computer built around the OZ-3, in the spirit of the CP/M
machines of the early 1980s (the Osborne Executive and the Kaypro): a
character screen, a keyboard, disk drives, and a small operating system that
loads programs from disk and runs them. It is what makes the OZ-3 a machine
to write fun programs for, rather than a processor in a test.

This doc defines Ozzy as a whole: its hardware, its devices, the instructions
the default instruction set gains for it, its operating system, and the host
application that runs it. It ties together the backlog items that build it
(see [Getting there](#getting-there)). Ozzy's own wiki page, the reference for
programmers (its devices, ports, and calls), is written by those items as they
land.

The concepts and rules here are what this doc decides. Layouts (request
blocks, control words, port and interrupt numbers) and spellings are a sketch,
to make the model concrete, and each is settled in the item that builds it.

## Principles

- **Inspired by CP/M, not a copy.** Ozzy does the same kinds of things: a
  prompt, drive letters, files, and programs loaded from disk and run. How it
  does them is its own, so programmers discover it rather than port to it.
- **The OZ-3 is the star.** Ozzy's distinctive features come from the OZ-3:
  several cores, banked memory, ports, and simulated locks. Most of all, the
  OS runs on one core, and each program on a core of its own, so several
  programs run at once without the OS scheduling anything.
- **Small OS, generous devices.** The OS fits in a single bank with its working
  space, and leaves most of that bank for programs. Devices do the work an OS
  would otherwise drown in: the disk drive owns its file system, as the
  Commodore 1541's drive did. What is left to the OS is the interesting part:
  the shell, loading programs, and running them.
- **System code is OZ-3 code.** The boot ROM and the OS are written in OZ-3
  assembly, and assembled by the program assembler. The host only builds the
  hardware, fills the boot ROM, and starts core 0.
- **Configurable hardware, discovered by software.** A config file picks the
  cores, the banks, the clock speed, and the devices. The OS finds out what it
  is running on from a machine table, never from assumptions.
- **Devices are host-agnostic.** A device holds its state (screen cells, a key
  queue, open files) and leaves presenting it to the host. So the same devices
  work for a game that embeds the OZ-3, and Ozzy's console frontend is just one
  host.
- **Just quirky enough.** Devices take simulated time and contend for banks
  like any other user of them. There is no memory protection between programs
  sharing a bank. These are features to play with, not problems to hide.
- **Packed text.** Text and names are packed two characters to a word, as the
  assembler's `"..."` strings already are, and a new byte swap instruction
  keeps working with them cheap (see [Packed strings](#packed-strings)).

## The machine at a glance

| Part             | What it is                                                                     | Where                   |
| ---------------- | ------------------------------------------------------------------------------ | ----------------------- |
| Device support   | Devices run by the `Processor` in step with the cores, on ports and interrupts | `core` (new)            |
| Screen           | A memory mapped page of character cells, each a character and an attribute     | `devices` (new library) |
| Keyboard         | A queue of key events read through a port, with an interrupt                   | `devices`               |
| Teletype         | A port that writes text to the host, for an Ozzy without a screen              | `devices`               |
| Timer            | A periodic interrupt and a clock                                               | `devices`               |
| Disk drive       | A smart drive: named files in a flat directory, read and written to memory     | `devices`               |
| New instructions | Byte swap, and signals to other cores (interrupt, wake)                        | `instruction_sets`      |
| Machine          | The hardware built from a config file, with its machine table and boot ROM     | `ozzy` (new library)    |
| Boot ROM, OS     | The system code, in OZ-3 assembly                                              | `ozzy`                  |
| `ozzy`           | The host application: the console frontend, real-time pacing                   | `apps`                  |

`devices` and `ozzy` come after `instruction_sets` in the dependency order, and
before `apps`, as CLAUDE.md already says.

## Hardware

### Cores

An Ozzy has 1 to 8 cores, all running the default instruction set (plus what
any coprocessor adds). Core 0 runs the OS. The others start idle, and the OS
starts a program on one with `RST`, which sets its banks and starts it at
`IP` 0. A one core Ozzy still works, running one program at a time on core 0,
as a CP/M machine did (see [Running programs](#running-programs)).

### Memory

Every configured bank is RAM, except the boot ROM. A bank's read and write
masks are fixed by the config, and OZ-3 code can't write a read-only page, so
an OS loader puts a program's ROM segments in RAM pages (which the program file
spec allows when there aren't enough read-only pages). The only ROM is the one
the host fills at power on.

The default layout is a single bank, which holds everything:

| Pages | What                                                        |
| ----- | ----------------------------------------------------------- |
| 0     | Boot ROM, and the machine table                             |
| 1-3   | The OS: its code, data, buffers, and stack (12 Kiw at most) |
| 4-14  | Programs                                                    |
| 15    | The screen                                                  |

More banks are all RAM, for programs. The OS allocates pages to programs,
honoring each program file's assign masks and page requests, and puts them in
the OS's own bank when there is room. Programs that share a bank can overwrite
each other, and the OS. That is the era, and a quirk to keep.

### Ports and interrupts

Each device in the config is given its ports and interrupt line. A sketch of
the defaults, in order of interrupt priority (the lowest line is handled
first):

| Interrupt | Port  | Device                             |
| --------- | ----- | ---------------------------------- |
| 0         | 0     | Timer                              |
| 1         | 1     | Keyboard                           |
| -         | 2     | Teletype (when there is no screen) |
| 2-5       | 8-11  | Disk drives `A:` to `D:`           |
| 6         | -     | Screen refresh                     |
| 24-31     | 16-23 | OS calls from cores 0 to 7         |

A device's interrupt goes to the cores its line is connected to, which is the
OS core by default (every core, for the screen's refresh). A program that
wants a device's interrupts directly (a game reading the keyboard, say) can be
connected too, and sets its own handler.

### Machine table

The OS learns the hardware from a **machine table** that the host writes into
the boot ROM at power on, at a fixed address, as a BIOS data area would be. It
lists:
- The clock speed, and the number of cores.
- Each bank: which pages are RAM, ROM, or mapped to a device.
- Each device: its type, its ports, its interrupt line, and its memory (bank
  and pages), plus what is particular to the type (the screen's size, a drive's
  letter).

Device types are numbers the Ozzy spec assigns. The table is words, laid out
like a program file: counts, then fixed-size entries.

### Configuration

A config file, read with Game Bits' `gb::Config` text format, describes the
machine. A sketch:

```
// hello.ozzy
clock_hz: 1000000
cores: 4
banks: [
  { rom: "0", ram: "1-14" },  // Bank 0: the boot ROM, the OS, and programs
  { ram: "0-15" },            // Bank 1: programs
]
devices: [
  { type: "screen", bank: 0, page: 15, columns: 80, rows: 24, interrupt: 6 },
  { type: "keyboard", port: 1, interrupt: 1 },
  { type: "timer", port: 0, interrupt: 0, hz: 60 },
  { type: "drive", letter: "A", port: 8, interrupt: 2, folder: "disks/system" },
  { type: "drive", letter: "B", port: 9, interrupt: 3, folder: "disks/work" },
]
```

Anything left out has a default, so an empty config is a one bank, one core
Ozzy with a screen, a keyboard, a timer, and drive `A:`. Coprocessors join the
config when they exist.

## Devices

### Device support in core

Today the `Processor` runs only its cores, and nothing attaches to its ports.
Core gains devices:
- A **device** is an `ExecutionComponent` that the `Processor` runs in step
  with the cores, cycle by cycle, as it runs cores. The host adds devices
  through `ProcessorConfig`.
- A device is given its ports, and uses them as a core does, through their
  locks.
- A device can lock any memory bank, as a core does, to read or write memory
  directly. Each lock costs it cycles and makes cores wait, so device
  transfers contend for banks like everything else. Memory mapped devices use
  `MemoryMap`, which exists already.
- A device raises its interrupt on the cores its line is connected to, and
  learns whether it was a duplicate, as the spec promises: it is a duplicate
  only if it was already pending on every one of those cores. This settles the
  question the *Coprocessors* and *Devices* backlog items share.

### Screen

A memory mapped page. An 80 by 24 screen is 1920 cells, which fits in one
4 Kiw page with room left for control words.
- **Cells:** one word per cell, row by row. The low byte is the character, and
  the high byte its attribute, so writing a plain character (`'A'`, 41h) shows
  it with no attributes.
- **Attributes:** inverse, underline, half intensity, and blink, as the Osborne
  Executive had. The rest of the byte is reserved.
- **Characters:** 0 to 127 are ASCII, and 128 to 255 are Ozzy's own graphics
  characters (box drawing, blocks, and the like), drawn by the console
  frontend with their nearest Unicode characters.
- **Control words**, after the cells: the cursor's column and row, whether it
  shows, and a **scroll row**, the row of the cells shown at the top of the
  screen. Scrolling a whole screen is then one write, rather than moving 1920
  words, which is a fun old-school trick, and fast without the DMA
  coprocessor.
- The size is configurable, as long as the cells and control words fit the
  page.

The CPU writes cells through the bank's lock, as any memory, and the screen
never contends for its bank. It **refreshes** at a configured rate (60 Hz by
default, counted in cycles of the Ozzy's clock, not host time): at each
refresh it takes what is in the page as the picture, and the host shows that
picture until the next one. So a program can change many parts of the screen
between two refreshes and have them all appear at once.
- A **frame counter** control word counts refreshes, so a program can wait for
  the next one by watching it change, then draw in the time until the one
  after.
- The screen also has an interrupt line, raised at each refresh, for programs
  that would rather be told (like a vertical blank interrupt). It is connected
  to every core by default, so a program on any core can sync to the screen
  by setting a handler. A core without one drops the interrupt at no cost.

### Keyboard

A port, holding the oldest key event in a small queue (16 events), with its
status set while it holds one, and an interrupt when a key arrives. The OS
reads it with `INS`.
- A key event is a word: the low byte is the character (ASCII, control codes
  for `Ctrl` combinations, and codes from 80h for arrows, function keys, and
  the like), and the high byte the modifier keys.
- A full queue drops new keys, as old keyboards did.

### Teletype

For an Ozzy without a screen, a port that writes text to the host. Each word
written is two packed characters, the high byte first, and a zero byte is
skipped. So a single character is written as itself (`'A'` is 0041h), and a
packed string can be sent straight from memory with `OUTR`. It handles carriage
return, line feed, backspace, and bell. Input still comes from the keyboard.

### Timer

A periodic interrupt at a configured rate (60 Hz by default), and a port to
read the cycle count and the host's date and time. It is what games and the OS
pace themselves with.

### Disk drive

A **smart drive**: it owns its file system, so the OS asks for files by name,
and the drive keeps the directory and allocates space. This keeps the OS small,
where a CP/M BDOS spent most of its size on the file system.
- **Disks** are folders on the host in the first version. Each file in the
  folder whose name is a valid Ozzy name (see
  [File names](#file-names)) is a file on the disk, and the rest don't show.
  So a program is put on a disk by copying its `.oz3` file into the folder. A
  single file disk image format can come later.
- **A flat directory**, one per drive, with drive letters. No subdirectories.
- **Requests** are blocks in memory. The OS fills one in, and writes its
  address and bank to the drive's port (one dword: word 0 the address, word 1
  the bank). The port acts as a doorbell: its status stays set while the drive
  is busy, so `OUTS` fails until it is done. The drive reads the block, does
  the work, writes the results and a status back into the block, and raises
  its interrupt.
- **Commands:** find (first and next, with `*` and `?` patterns), open (read,
  write, create, or append), close, read, write, seek, delete, rename, and the
  disk's free space. Files have handles, a few per drive.
- **Transfers** go straight between the file and memory, at the address the
  request gives. The program file format is designed for this: a loader can
  read each data block straight to where it goes.
- **Time:** each request takes simulated time, a configured seek time and a
  time per word, and the drive locks the bank a few words at a time, so it
  contends with the cores. This is what lets the OS core sleep while a drive
  works, and other programs keep running.
- **Words on the host:** a file is words, stored on the host low byte first,
  as program files are. A file opened as **text** is instead the host's bytes,
  packed two to a word with the first in the high byte (and a zero byte to pad
  an odd length). So a text file written in Notepad reads as packed text on
  the Ozzy, and back.

A request block sketch: command, status, handle, mode, position (a dword),
buffer bank and address, length in words, and a name or pattern (8 words), for
about 18 words.

## Packed strings

Text is packed two characters to a word, the first in the high byte, with the
last word padded with a zero byte, exactly as the assembler writes `"..."`
strings. A packed string in memory ends with a zero byte (`.DW "Hello", 0`),
and calls that move text take a length as well. Packed text is used for:
- File names.
- Text files on disk.
- The teletype, and lines read from the keyboard.
- The command line handed to a program.
- The OS's messages, in its code.

Two things are not packed: screen cells (a character and an attribute), and key
events (one key at a time, with its modifiers).

Putting the first character in the high byte means comparing packed strings
word by word, as unsigned values, sorts them alphabetically, so a sorted
directory listing, or a drive searching its directory, is cheap.

### File names

A file name is 12.3: up to 12 characters, then optionally a `.` and up to 3,
packed into a fixed field of 8 words with the dot included, padded with zero
bytes. A full 16 character name fills the field with no zero byte, so it is a
fixed field, not a zero terminated string.
- Names aren't case-sensitive. The drive stores and returns them in upper case.
- The drive rejects a name with a space, `*`, `?`, `:`, or a second `.`.
- `.OZ3` is a program.

The assembler writes `"A.TXT"` as 3 words, so an 8 word name field in source
needs zeros after it by hand. A fixed size string form for the assembler would
fix that, and is a small backlog item of its own.

## New instructions

The default instruction set gains a few instructions, at the end of the `.izm`
after `RST` (from opcode 8Ah), each built only from microcode ops that already
exist. No microcode changes, and no existing code word changes.

| Instruction                | What it does                                                    | Microcode            |
| -------------------------- | --------------------------------------------------------------- | -------------------- |
| `SWPB <reg>`               | Swaps a register's bytes, to unpack and pack text               | `MVB` three times    |
| `INTC <core>, <interrupt>` | Raises an interrupt on another core, to call the OS             | `CLK`, `INT`, `CUL`  |
| `WAKE <core>`              | Ends another core's `WAIT`, or starts it if idle, from its `IP` | `CLK`, `CRUN`, `CUL` |

A core is named as `RST` names it, with its `$LockCore` macro: a number, `R2`,
or `SELF`.

`SWPB` makes packed text cheap: getting the high byte takes a `SWPB` and an
`AND`, where a shift by 8 takes about 12 cycles. `INTC` and `WAKE` are how
programs call the OS, and how it answers. `RST` can't do `WAKE`'s job, as
resetting `CODE` sets `IP` to 0, and `HALT` can't be resumed, as the default
`HALT` moves `IP` back to stay halted.

`INTC` and `WAKE` are signals: they change nothing a core is working with, and
a core that isn't waiting for them is barely affected (an interrupt it has no
handler for is dropped, and a running core has no `WAIT` to end). Data between
cores goes through shared memory and ports. There are deliberately no
instructions to read or write another core's registers, which could catch it
anywhere in its work. `RST` stays the one hammer, for starting and stopping a
core.

## The Ozzy OS

### Boot

At power on, the host builds the machine, writes the boot ROM and the machine
table, and starts core 0 at `IP` 0 of bank 0. The boot ROM clears the screen,
prints a banner, and loads `OZZY.SYS` (the OS, a program file) from drive `A:`
into RAM pages, then jumps into it with `RST SELF`. The first milestone stops
short of disks: the boot ROM prints its banner and echoes keys.

### Calls

A program calls the OS with a **call block** in its own memory: the function,
its arguments, and room for its results. It writes the block's address to its
core's **call port**, raises its core's call interrupt on core 0 with `INTC`,
and `WAIT`s. The OS's handler reads the address from the port, and the block
from the program's `DATA` bank (which it knows, as it started the program).
It does the work, writes the results into the block, and resumes the caller
with `WAKE`. Calls that take time (a disk read, a key) let the OS go back to
sleep, and answer when the device's interrupt comes in, so other programs keep
running.

A call block also suits calls a register couldn't carry, such as a packed
file name or a line of text.

Between a program's `INTC` and its `WAIT` the OS could, in theory, answer
first, and the `WAKE` would find nothing to end. The call sequence has to make
that safe, such as by waiting in a loop until a result register changes
(`WAIT` times out, so this always ends), or by an `INTC` form that waits in the
same instruction. Which one is settled in the OS item.

The calls, as a sketch:
- **Console:** write a character, write a packed string, read a key, read a
  line.
- **Files:** find, open, close, read, write, seek, delete, rename, and free
  space.
- **Programs:** run a program (waiting for it, or in parallel), wait for one,
  list them, stop one, and exit.
- **System:** the machine table's address, the time, and allocating pages.

### Running programs

The OS loads a program file through the drive, allocates its banks' pages, and
starts it on a free core with `RST`, which also copies the OS core's `R0` and
`R1` into the new core: its core number, and the address of its command line.
- **In the foreground**, the prompt waits for the program to exit, and keys go
  to it.
- **In parallel**, the prompt comes back at once, and the program runs on its
  own core. A program can also ask the OS to run another in parallel.
- A program file's instances let one loaded program run on several cores at
  once, sharing its shared segments, as the program file spec intends.
- **Exiting** or **stopping** a program parks its core with `RST` on a `HALT`
  in the boot ROM, and frees its pages.
- With one core, a program runs on core 0 in place of the prompt, and
  "in parallel" isn't available.

While nothing is running in the foreground, the OS core sits halted at the
prompt, waking only for interrupts.

### Shell

A prompt with the current drive (`A>`), and a few built-in commands: changing
drive (`B:`), `DIR` (with patterns), `TYPE`, `DEL`, `REN`, `COPY`, and listing
and stopping running programs. Any other word runs the program of that name
from the disk, and a trailing `&` runs it in parallel.

### Size

The OS, with its buffers and stack, fits in pages 1 to 3 of its bank (12 Kiw),
and is meant to stay well inside that. The drive's file system and the machine
table keep it small.

## Host application

`ozzy <config>` builds the machine from a config file and runs it.
- **The console frontend** draws the screen in the Windows console with virtual
  terminal sequences (attributes and the cursor), and reads the keyboard from
  console input. What is Windows-only (console input, sizing) is behind a
  small interface in `win_` files, as the workflow's C++ style requires. For an
  Ozzy without a screen, it shows the teletype's output.
- **Real-time pacing:** each frame, it runs the cycles the clock speed gives
  for the time passed (16,667 cycles per 60 Hz frame at 1 MHz), and draws the
  screen if it changed. A config option runs as fast as the host can.

Measured in Release, with a tight loop of `ADD`, `MOV`, and `JP` on every core
and no devices, the emulator runs about 46 M cycles a second on one core, and
about 42 M core-cycles a second spread over 8 busy cores in their own banks
(5.3 MHz each). Cores waiting on a shared bank cost even less. So 1 to 4 MHz
with all 8 cores busy is real time on one host thread, with plenty left for
devices. Debug builds are slower, and weren't measured.

The boot ROM's and the OS's sources live in `oz3/ozzy`. The `ozzy` library
links `tools`, so it assembles the boot ROM in process at start up, and writes
it into the ROM page. The OS is `OZZY.SYS` on the system disk, a program file
built with `oz3asm`, as a real machine's OS was a file on its boot disk. So no
assembled system code is checked in.

## Names

| Name                   | Candidates                                | Avoids                                                          | Recommendation         |
| ---------------------- | ----------------------------------------- | --------------------------------------------------------------- | ---------------------- |
| Byte swap              | `SWPB`, `BSWP`, `XCHB`                    | `SWP` (swap a register with a value), which it reads beside     | `SWPB`                 |
| Interrupt a core       | `INTC`, `CINT`, `IRQ`                     | `INT` (this core); `IRQ` is a hardware line, not an instruction | `INTC`                 |
| Resume a core          | `WAKE`, `RUN`, `CRUN`                     | `CRUN` (the microcode op), `RST` (resets banks)                 | `WAKE`                 |
| OS call memory         | call block, parameter block, mailbox      | the drive's request block, CP/M's FCB                           | call block             |
| Console without screen | teletype, terminal, serial console        | the Windows console (the frontend), `Port`                      | teletype               |
| Hardware table         | machine table, system table, device table | `InstructionSet` tables, `ProgramMap`                           | machine table          |
| Config file            | `.ozzy`, `.ozc`, `.cfg`                   | `.oz3`, `.oza`, `.izm`                                          | `.ozzy` (`hello.ozzy`) |
| Drive and disk         | drive (the device), disk (its folder)     | `MemoryBank`                                                    | both, as they were     |

## To confirm

- The call sequence's race between `INTC` and `WAIT` (see [Calls](#calls)),
  settled in the *Ozzy OS* item.

## Getting there

The backlog items build this in order, each updating the wiki as it goes:
1. *Byte swap instruction* (instruction_sets, wiki): `SWPB`. Small, and
   independent.
2. *Core signal instructions* (instruction_sets, wiki): `INTC` and `WAKE`.
   Independent of the devices.
3. *Devices in core* (core, wiki): devices run by the `Processor`, their ports,
   and interrupt lines connected to cores, which report duplicates.
4. *Console devices* (devices): the screen, keyboard, and teletype, and the new
   `devices` library.
5. *Ozzy machine* (ozzy, apps, wiki): the config, the machine table, the
   console frontend and pacing, and a boot ROM that prints a banner and echoes
   keys. This is the first Ozzy anyone can run, and creates Ozzy's wiki page.
6. *Timer device* (devices, ozzy).
7. *Disk drive device* (devices, ozzy): the smart drive, on host folders.
8. *Ozzy OS* (ozzy, wiki): booting from disk, the calls, the loader, running
   programs in the foreground and in parallel, and the shell.

Then *Debugger (oz3dbg)*, which Ozzy integrates once there is a machine worth
debugging. *Fixed size strings* in the assembler can be done any time before
the OS needs them. Later, and waiting in the backlog: disk image files, and the
DMA coprocessor for fast screen and disk transfers.
