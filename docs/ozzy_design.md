# Ozzy Computer

Ozzy is a virtual computer built around the OZ-3, in the spirit of the CP/M
machines of the early 1980s: a character screen, a keyboard, disk drives, and
a small operating system that loads programs from disk and runs them. It is what makes the OZ-3 a machine
to write fun programs for, rather than a processor in a test.

This doc defines Ozzy as a whole: its hardware, its devices, the instructions
the default instruction set gains for it, its operating system, and the host
application that runs it. It ties together the backlog items that build it
(see [Getting there](#getting-there)). Ozzy's own manual page, the reference for
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
| New instructions | Byte swap, and signals to other cores (interrupt, wake)                        | `instructions`          |
| Machine          | The hardware built from a config file, with its machine table and boot ROM     | `ozzy` (new library)    |
| Boot ROM, OS     | The system code, in OZ-3 assembly                                              | `ozzy`                  |
| `ozzy`           | The host applications: in a window, and in a terminal or headless (`ozzyterm`) | `apps`                  |

`devices` and `ozzy` come after `instructions` in the dependency order, and
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
| 2-5       | 16-27 | Disk drives `A:` to `D:`, 3 each   |
| 6         | -     | Screen refresh                     |
| 24        | -     | OS calls from every core           |

A device's interrupt goes to the cores its line is connected to, which is the
OS core by default (every core, for the screen's refresh). A program that
wants a device's interrupts directly (a game reading the keyboard, say) can be
connected too, and sets its own handler.

The manual's Ozzy Computer page has the wiring as built, which moved from this
sketch, and port 0 is the power port (see [Power](#power)). OS calls need no
ports of their own, as they come through the cores'
[mailboxes](manual/2-Specifications.md#mailboxes) (see [Calls](#calls)).

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
  { type: "drive", letter: "A", image: "disks/system.ozd" },
  { type: "drive", letter: "B", image: "disks/work.ozd" },
]
```

Anything left out has a default, so an empty config is a one bank, one core
Ozzy with a screen, a keyboard, a timer, and drive `A:`. Coprocessors join the
config when they exist.

## Devices

### Device support in core

Core runs devices beside the cores (see
[Devices in core](worklog/devices-in-core.md)):
- A **device** is an `ExecutionComponent` that the `Processor` runs in step
  with the cores, cycle by cycle, as it runs cores. It is written as a
  `DeviceFactory`, which holds its wiring (ports, interrupt lines, and pages)
  and settings, and the `Device` it creates. The host adds factories to the
  `ProcessorConfig`, and each `Processor` creates and owns its own devices
  from them, and finds them by name.
- A device is given its ports, and uses them as a core does, through their
  locks.
- A device can lock any memory bank, as a core does, to read or write memory
  directly. Each lock costs it cycles and makes cores wait, so device
  transfers contend for banks like everything else.
- A memory mapped device owns pages of its own in a bank, through its
  `MemoryMap`, at its own addresses. It uses them under the bank's lock, but
  with no cycles, so it only waits while a core holds the bank, and no core
  sees a change before it happens.
- A device raises its interrupts on the cores each line is connected to, and
  learns which of them it was newly raised on, as the spec promises: it is a
  duplicate only if it was already pending on every one of those cores.
- A device with nothing to do sleeps until its ports, its pages, the host, or
  a deadline wake it, so an idle keyboard costs the `Processor` one compare a
  cycle.

### Screen

A memory mapped page. An 80 by 24 screen is 1920 cells, which fits in one
4 Kiw page with room left for control words.
- **Cells:** one word per cell, row by row. The low byte is the character, and
  the high byte its attribute, so writing a plain character (`'A'`, 41h) shows
  it with no attributes.
- **Attributes:** inverse, underline, half intensity, and blink. The rest of
  the byte is reserved.
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

The CPU writes cells through the bank's lock, as any memory. The screen
**refreshes** at a configured rate (60 Hz by default, counted in cycles of the
Ozzy's clock, not host time): at each refresh it takes what is in the page as
the picture, and the host shows that picture until the next one. So a program
can change many parts of the screen between two refreshes and have them all
appear at once.
- The screen takes the bank's lock for each refresh, so it never sees half of
  a core's write. It takes the lock, reads its page, and releases it in one
  step, with no cycles, so it waits only while a core holds the bank, and
  never makes a core wait.
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
written is two packed characters, the low byte first, and a zero byte is
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
where a CP/M BDOS spent most of its size on the file system. Its design, from
the disk image format to the drives in the Ozzy, is
[disk_design.md](disk_design.md), which replaces the sketch of a drive on host
folders that was here.

## Packed strings

Text is packed two characters to a word, the first in the low byte, with the
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

Putting the first character in the low byte means packed text on the host (two
bytes a word, low byte first, as in program files) reads in order, so text
files copy to and from disk images as plain binary. Comparing packed strings
word by word sorts them alphabetically once each word's bytes are swapped,
which the byte-swapped compare instruction does in one instruction a word (see
[disk_design.md](disk_design.md), Prerequisites), so a sorted directory
listing, or a drive searching its directory, stays cheap.

### File names

A file name is 12.4, a name of up to 12 characters and an extension of up to
4, in a fixed field of 8 words with no dot stored. See
[disk_design.md](disk_design.md) (Names and patterns). `.OZ3` is a program.

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
or `SELF`. `INTC` has two forms, `INTC.I` taking the interrupt as an integer
(for fixed lines), and `INTC.R` taking it in a register (for a line computed at
run time).

`SWPB` makes packed text cheap: getting the high byte takes a `SWPB` and an
`AND`, where a shift by 8 takes about 12 cycles. `INTC` and `WAKE` are how
programs call the OS, and how it answers. `RST` can't do `WAKE`'s job, as
resetting `CODE` sets `IP` to 0, and `HALT` can't be resumed, as the default
`HALT` moves `IP` back to stay halted.

`INTC` and `WAKE` are signals: they change nothing a core is working with, and
a core that isn't waiting for them is barely affected (an interrupt it has no
handler for is dropped, and on a running core, `WAKE` only ends its next
`WAIT` early). Data between
cores goes through shared memory and ports. There are deliberately no
instructions to read or write another core's registers, which could catch it
anywhere in its work. `RST` stays the one hammer, for starting and stopping a
core.

## The Ozzy OS

### Boot

At power on, the host builds the machine, writes the boot ROM and the machine
table, and starts core 0 at `IP` 0 of bank 0. The boot ROM clears the screen,
shows its banner in a box and a prompt, and waits for Enter. Then it boots in
two stages, as the Osborne Executive's CP/M Plus did: it reads a small
**loader** from the boot area of the disk in `A:` to 1000h, just below its
stack in page 1 of bank 0, checks the loader's boot signature, and starts it
with `RST SELF`. The loader is the disk's own code, written with `oz3dsk boot`,
and loads the OS (such as `OZOS.SYS`) with the drive's ordinary commands. If
the boot fails, the boot ROM says why and waits for Enter again, and a loader
that fails hands an error code back to the boot ROM's exit, which shows it. The manual's Ozzy Computer page says what
the loader starts with, and
[booting-from-disk.md](worklog/booting-from-disk.md) how it came to be.

### Calls

A program calls the OS with a dword in its
[mailbox](manual/2-Specifications.md#mailboxes) to core 0 (port 100h): a
**command** word, and the address of a **call block**, for a command that has
one. It writes them with `OUTS.ID`, and then raises the call interrupt on core
0 with `INTC.I 0`, always with its interrupts enabled (see below). Every core
calls with the same code, whichever core it is, and one interrupt line serves
every core, rather than using up one of the 32 lines for each.
- **Command:** the function, in the low word. Its high bit, **wake**, asks the
  OS to `WAKE` the caller when the call is done.
- **Call block:** the call's parameters, and room for its results, always in
  the caller's `DATA` bank. A command whose parameters fit in the command word,
  or that has none, ignores the address.

The OS's handler finds who called by which mailboxes are full: on core 0, port
`108h + n` is the mailbox from core `n`. It goes through every core's mailbox
in turn, taking each call it finds with `INS.ID`, which empties the mailbox,
and reads the call block from the caller's `DATA` bank (which it knows, as it
started the program). It keeps going round the mailboxes until a whole pass
finds none full, and only then returns from the interrupt, so calls that come
in while it works are handled in the same interrupt. Calls that take time (a
disk read, a key) are queued to finish when the device's interrupt comes in,
so the handler moves on, and other programs keep running.

When a call is done, the OS writes its results into the call block, and then
signals the caller through the **return mailbox**, its mailbox to the caller
(port `100h + n` on core 0, which the caller reads as port 108h): a completion
word, such as a status. Then, if the command asked for it, it `WAKE`s the
caller. A core makes one call at a time, taking its completion before making
the next, so both of its mailboxes are empty when it calls, and the OS can
always write the completion.

So a caller can wait for a call in two ways:
- **Wait:** set wake, `WAIT`, and then take the completion with `INS` from
  port 108h. If there is none yet, the `WAIT` ended early (from a stray
  `WAKE`), so it `WAIT`s again.
- **Poll:** leave wake clear, and go on working, checking port 108h with
  `TSTP` or `INS` now and then. This suits a program with work to do while a
  call takes time, such as a game reading a file.

Calls made at once raise the interrupt once, as a raise while it is pending is
a duplicate, but the handler still finds each of them in its mailbox. A call
made after the handler's last pass, but before it returns, raises the interrupt
again, so it is handled next. And a raise for a call the handler has already
taken runs the handler once more, finding nothing.

A program makes calls with its interrupts enabled. A program on core 0, which
the OS runs on, sends its call and gets its completion through the same
mailbox, its mailbox to itself (both port 100h and port 108h), and its
`INTC.I 0` interrupts its own core. With interrupts enabled, the call
interrupt is handled right after the `INTC`, before the program's next
instruction, so the handler has taken the call before the program can look for
the completion, and port 108h holds only the completion, never the call. So
the same code works on every core, and a program never needs to know which
core it runs on. A program that calls from its own interrupt handler enables
interrupts first (`EI`). The handler's `WAKE` of core 0 ends the program's
`WAIT`, or sets `N` if the interrupt came before the `WAIT`, as below.

A call block suits calls a register couldn't carry, such as a packed file name
or a line of text.

Between a program's `INTC` and its `WAIT` the OS could answer first. The
`WAKE` then finds no `WAIT` to end, so it sets the program's core's notify flag
(`N`), and the `WAIT` ends at once. A program clears `N` with `CLRN` before it
starts calling, as it can't count on `N` being clear when it starts.

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

Two apps run an Ozzy, built from one config file, on the same `ozzy` library:
- **`ozzy`** runs it in a window of its own, whatever devices it has. It is
  the one to start from the desktop, so it is a windowed program, which never
  opens a console window.
- **`ozzyterm`** runs it in a terminal (the Windows console, or a terminal on
  POSIX systems), or with `--headless`, with no terminal at all.

They are two programs, as one can't be both on Windows: a console program
started from the desktop flashes a console window open, and a windowed one
started from a shell has no console to write to, and the shell doesn't wait
for it. It also keeps `ozzyterm` to what a terminal needs: only `ozzy` links
SDL and Vulkan, and so only `ozzy` needs the Vulkan SDK to build. The terminal
app is `ozzyterm` before the window app exists, so `ozzy` is only ever the
window app.

### The machine, its state, and the panel

Both apps are three layers, and differ only in how they present the last:
- **The machine** is the `Ozzy`, which exists only while it is on. Powering
  on creates it, and powering off destroys it, which commits each drive as at
  a clean shutdown. Restarting is powering off and on again, so RAM is
  cleared, and every device starts as at power on.
- **The machine's state** is the host's, and outlives the machine: the
  settings from the config file, the disk in each drive, and whether the
  machine is on. A disk changed while the machine is on goes into the drive
  that is running, and into the state, so it is still in its drive after a
  restart, as a floppy would be.
- **The panel** is the host's controls for the machine, as the switches and
  drive slots on the front of a machine were: commands to power it on and off,
  restart it, change disks, and quit, with more to come (such as pausing, the
  speed, and the debugger). The commands, and what they do to the state, are
  in the `ozzy` library, tested against a fake `Console` as the rest of the
  frontend is, so each app only presents them.

### Power

Every Ozzy has a **power port**, port 0, which programs write to shut the
machine down or restart it. It is a port of two words: the first word is the
command, and the second the exit code, a signed word, which a program writes
with the command in one `OUT.ID`.

| Command | What                                                        |
| ------- | ----------------------------------------------------------- |
| 1       | Shut down: power off, with the exit code                    |
| 2       | Restart: power off and on again, ignoring the exit code     |

Any other command is reserved, and does nothing. The port is the power control
device's (see the manual's Power Control page), which passes each request to a
callback the app puts in the processor's context. The callback is called from
inside `Ozzy::Run`, so the app only notes the request there, and acts on it
once `Ozzy::Run` returns. So the machine runs on to the end of that run, and a
program halts after writing the command. The app decides which request wins if
a program makes more than one. The power control's wiring is the same in every
Ozzy, as every device's is, and a config file can't leave it out.

What powering off does is up to the app:
- `ozzyterm --headless` exits, with the exit code as its own. Windows keeps
  all of it, so -1 is -1. POSIX systems keep its low 8 bits, so -1 is 255, and
  codes from 0 to 255 are the same everywhere.
- Otherwise, only the machine is off. The panel says so, with the exit code
  ("Ozzy shut down, exit code 3"), and its `on` command powers it on again.
  Quitting from the panel exits with 0.

### The panel's commands

Ctrl+Alt+P opens the panel and closes it again, in both apps, and Ctrl+Alt+Q
still quits at once. While the panel is open, keys go to it rather than to the
keyboard, but the machine keeps running, as a machine doesn't stop when its
front panel is looked at. While the machine is off, there is nothing else for
keys to go to, so the panel stays open.

| Command                   | What                                               |
| ------------------------- | -------------------------------------------------- |
| `on`                      | Power the machine on                               |
| `off`                     | Power it off, as at a clean shutdown               |
| `restart`                 | Power it off and on again                          |
| `insert <drive> <image>`  | Put a disk image in a drive (`insert A: work.ozd`) |
| `eject <drive>`           | Take the disk out of a drive, as a floppy pulled   |
| `drives`                  | List each drive and the image in it                |
| `help`                    | List the commands                                  |
| `quit`                    | Quit the app, powering the machine off first       |

Paths go through the file system, as every path the apps open does. A path
with no protocol is as typed at a shell: absolute, or relative to the current
directory. The `config:` protocol is the config file's folder (or the current
directory, without a config file), which is also where the config's own image
paths are from by default. So `insert A: config:disks/work.ozd` takes a disk
from beside the config, and a config can name `file:` paths of its own. On
Windows, each drive letter is a protocol already, so `insert A: C:\disks\a.ozd`
opens as written.

### `ozzyterm`

In a terminal, it always takes the whole console, whatever devices the Ozzy
has, so how it is shown is chosen by the command line, never by the config:
- **With a screen**, the console shows the screen's picture.
- **Without a screen**, it shows the teletype view: the teletype's lines in a
  view the size of the console, which the Ozzy doesn't know of. They wrap at
  the console's width, scroll up from the bottom, and are drawn whole again on
  a resize, as the picture is. With neither, it shows a message saying so.

The panel takes the whole console in place of either: the machine's status at
the top (whether it is on, its clock rate and how well the host is keeping up,
and each drive's image), the panel's messages, and a command line at the
bottom. The console is given back as it was found once `ozzyterm` quits, so
the teletype's lines don't stay in the terminal; `--headless` is for a
transcript.

With **`--headless`**, it has no terminal, for running an Ozzy from a script
or a test: `ozzyterm --headless test.ozzy < keys.txt > out.txt`. The standard
input is typed keys, the teletype's output goes to the standard output, and a
screen isn't shown. There is no panel, so powering off exits.

### `ozzy`

It is always a window, whatever devices the Ozzy has. The window shows the
screen's picture as its pixels, or for an Ozzy without a screen, the
teletype's lines in a space of their own (or a message, with neither). The
panel is the same, opened over the machine's picture with the same key, and
menus run the same commands. While the machine is off, its picture is dark. A
config file with errors still opens the window, with the machine off and the
errors in the panel.

Later, it gains what a terminal can't have: other windows (such as the
debugger's), a mouse, and rendering styles and themes for the app and the
Ozzy's picture.

### Pacing

Each frame, the app runs the cycles the clock rate gives for the time passed
(16,667 cycles per 60 Hz frame at 1 MHz), up to 100 ms of them, and draws the
screen if it changed. So a host that falls behind slows the Ozzy, rather than
racing to catch up.

Measured in Release, with a tight loop of `ADD`, `MOV`, and `JP` on every core
and no devices, the emulator runs about 46 M cycles a second on one core, and
about 42 M core-cycles a second spread over 8 busy cores in their own banks
(5.3 MHz each). Cores waiting on a shared bank cost even less. So 1 to 4 MHz
with all 8 cores busy is real time on one host thread, with plenty left for
devices. Debug builds are slower, and weren't measured.

The boot ROM's and the OS's sources live in `oz3/ozzy/machine`. The
`oz3_ozzy_machine` library links `tools`, so it assembles the boot ROM in
process at start up, and writes it into the ROM page. The OS is `OZZY.SYS` on
the system disk, a program file built with `oz3asm`, as a real machine's OS was
a file on its boot disk. So no assembled system code is checked in.

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
| Host controls          | panel, front panel, monitor               | `Console`, the console devices, a ROM monitor on the machine    | panel                  |
| Terminal app           | `ozzyterm`, `ozzycon`, `ozzyw` (window)   | `Console`; `w` reads as Windows                                 | `ozzyterm`             |

## Getting there

The backlog items build this in order, each updating the manual as it goes:
1. *Byte swap instruction* (instructions, manual): `SWPB`. Small, and
   independent.
2. *Core signal instructions* (instructions, manual): `INTC` and `WAKE`.
   Independent of the devices.
3. *Devices in core* (core, manual): devices run by the `Processor`, their
   ports, and interrupt lines connected to cores, which report duplicates.
4. *Console devices* (devices): the screen, keyboard, and teletype, and the new
   `devices` library.
5. *Ozzy machine* (ozzy, apps, manual): the config, the machine table, the
   console frontend and pacing, and a boot ROM that prints a banner and echoes
   keys. This is the first Ozzy anyone can run, and creates Ozzy's manual page.
6. *Timer device* (devices, ozzy).
7. The disk drive, in the items [disk_design.md](disk_design.md) lists: disk
   images, the drive, its tools, and drives in the Ozzy.
8. *Booting from disk* (disk, devices, tools, ozzy): the boot area, the
   drive's `READ BOOT` and `WRITE BOOT`, bare programs, `oz3dsk boot`, and a
   boot ROM that starts a disk's loader.
9. *Ozzy OS* (ozzy, manual): the loader and the OS it loads, the calls,
   running programs in the foreground and in parallel, and the shell.

The host application (see [Host application](#host-application)) is built
alongside, independent of the OS, in its own items:
1. *Power port* (devices, ozzy, apps, manual): port 0, and the machine's state
   with the machine on or off, in the terminal app.
2. *Panel* (ozzy, apps, manual): the panel's commands, in both of the
   terminal app's forms.
3. *Headless ozzyterm* (ozzy, apps, manual): `--headless`.
4. *Window app* (ozzy, apps, manual): `ozzy` in a window.

Then *Debugger (oz3dbg)*, which Ozzy integrates once there is a machine worth
debugging. Later, and waiting in the backlog: the DMA coprocessor for fast
screen and disk transfers.
