# Ozzy Computer

The **Ozzy** is a computer built on the OZ-3: a processor with its cores, memory banks, and ports, a console of a [screen, a keyboard, and a teletype](3-Devices.md), a [timer](3.4-Timer.md), up to four [disk drives](3.5-Disk-Drive.md), `A:` to `D:`, and a [power control](3.6-Power-Control.md), which programs shut it down and restart it with.

Ozzys aren't all alike. A config file says how fast an Ozzy is, how many cores and how much memory it has, and which devices it has. Programs find out what they are running on from the **machine table**, which the Ozzy puts in memory at power on. How each device is wired (its port, interrupt, and memory) is the same in every Ozzy, so programs can rely on it.

### The config file

An Ozzy is described by a `.ozzy` file. Everything has a default, so an empty file is the default Ozzy: 1 MHz, one core, 48 ports, one bank with 56 Kiw of RAM, a screen, a keyboard, and a timer, with no drives, and the power control every Ozzy has.

```
// hello.ozzy
clock_hz: 1000000,  // 1 Hz to 100 MHz
cores: 4,           // 1 to 8
ports: 48,          // 32 to 256
banks: [
  { ram: 0x7FFE },  // Bank 0: pages 1 to 14 (page 0 is always the boot ROM)
  { ram: 0xFFFF }   // Bank 1: every page
],
devices: [
  { type: "screen", columns: 80, rows: 24 },
  { type: "keyboard" },
  { type: "timer", tick_hz: 60 }
],
drives: [
  { preset: "floppy-1440k", image: "disks/system.ozd" },  // A:
  { preset: "hd-20m", image: "disks/work.ozd" }           // B:
]
```

The format is like JSON, without the braces around the whole file: keys needn't be quoted, strings may be in single or double quotes, `//` and `/* */` comments are allowed, and numbers may be hexadecimal (`0x7FFE`). Unlike JSON, commas between entries are optional, so a file can have one entry a line without them, and a comma after the last entry is allowed.

| Key        | Default   | What                                                       |
| ---------- | --------- | ---------------------------------------------------------- |
| `clock_hz` | 1000000   | The clock rate, in Hz: 1 to 100,000,000                    |
| `cores`    | 1         | The cores: 1 to 8                                          |
| `ports`    | 48        | The ports: 32 to 256. Ports 0 to 31 are for devices.       |
| `banks`    | one bank  | The memory banks, 1 to 16 of them (see below)              |
| `devices`  | see below | The devices, each a map with a `type` and its own settings |
| `drives`   | none      | The disk drives, `A:` first, up to 4 (see below)           |

The cores' [mailboxes](2-Specifications.md#mailboxes) are as well as the `ports`, so every Ozzy has them, one from each core to each core.

#### Banks

`banks` is a list of the Ozzy's banks, from bank 0. Each is a map with one key, `ram`: its RAM pages, as a mask with bit `n` for page `n`, as the read and write masks state pages (see [Specifications](2-Specifications.md#overview)). In hexadecimal, each digit is four pages. A bank with no `ram` has none. The default is one bank, `{ ram: 0x7FFE }`.

Page 0 of bank 0 is always the boot ROM, so bank 0's mask can't have bit 0. Page 1 of bank 0 must be RAM, so bank 0's mask must have bit 1, as the boot ROM's stack and the loader are there (see [The boot ROM's stack](#the-boot-roms-stack)).

#### Devices

`devices` is a list of the Ozzy's devices, each a map with a `type` of `"screen"`, `"keyboard"`, `"teletype"`, or `"timer"`, and its settings. A setting left out takes its default, so `{ type: "screen" }` is an 80 by 24 screen. Without `devices`, the Ozzy has a screen, a keyboard, and a timer, each with its defaults, and `devices: []` is an Ozzy with none. Every Ozzy has a power control as well, which a config file doesn't list, as it has no settings and can't be left out.

An Ozzy has at most one of each device, as its console shows one screen and one teletype, it is typed on with one keyboard, and one timer is all its cores need.

Each device is wired the same way in every Ozzy, so a config file doesn't say how:

| Device   | Ports                                   | Interrupt               | Memory            |
| -------- | --------------------------------------- | ----------------------- | ----------------- |
| Power    | 0                                       | none                    | none              |
| Screen   | none                                    | 6, on every core        | Page 15 of bank 0 |
| Keyboard | 1 for typed keys, and 6 to 15 key state | 1, shared by every core | none              |
| Teletype | 2                                       | none                    | none              |
| Timer    | 3 for ticks, and 4 the clock            | 0, on every core        | none              |
| Drive A: | 16 to 18                                | 2, on core 0            | none              |
| Drive B: | 19 to 21                                | 3, on core 0            | none              |
| Drive C: | 22 to 24                                | 4, on core 0            | none              |
| Drive D: | 25 to 27                                | 5, on core 0            | none              |

The keyboard has both typed keys and key state (see [Keyboard](3.2-Keyboard.md)), and its interrupt is for typed keys. Its key state is on ports 6 to 15. Each drive has 3 ports, its command, data, and control ports (see [Ports](3.5-Disk-Drive.md#ports)), from port 16 for `A:`, leaving ports 5 and 28 to 31 for devices to come. An Ozzy's drives are lettered in order, from `A:`, so an Ozzy with drives always has an `A:`, which may be empty.

The keyboard's interrupt is shared, so each key is handled by only one core (see [Interrupts](2-Specifications.md#interrupts)). The screen's refresh and the timer's tick are news for every core, so they are not shared, and every core with a handler for them is told. A drive's completion is for core 0 alone, which boots the Ozzy and runs the operating system, as the operating system issues every drive command, and a completion can be read only once (see [The command protocol](3.5-Disk-Drive.md#the-command-protocol)). Another core can still use a drive by reading its control port, but only core 0 can wait for its interrupt.

So with a screen, page 15 of bank 0 must not be RAM.

A device's other keys are its settings, with the names, defaults, and ranges its own page gives: see [Screen](3.3-Screen.md#wiring-and-settings), [Teletype](3.1-Teletype.md#wiring-and-settings), and [Timer](3.4-Timer.md#wiring-and-settings). The keyboard has no settings, so it is only `{ type: "keyboard" }`. A timer's `tick_hz` is at most 65535, as the machine table holds it in a word, and at most `clock_hz`, as the timer ticks at most once a cycle. So an Ozzy slower than 60 Hz needs a slower timer than the default, or none.

#### Drives

`drives` is a list of the Ozzy's disk drives, lettered in order: the first is `A:`, the next `B:`, and so on, up to `D:`. They are a list of their own, as they are the one device an Ozzy has several of, and their order names them. A drive with no disk keeps its place, so an Ozzy whose only disk is in `B:` has an empty `A:` before it. There are no drives by default.

Each drive is a map with two keys of the Ozzy's own, and the drive's settings, with the names, defaults, and ranges its own page gives (see [Disk Drive](3.5-Disk-Drive.md#wiring-and-settings)):

| Key      | Default | What                                                                 |
| -------- | ------- | -------------------------------------------------------------------- |
| `preset` | none    | The drive's timing preset (below), which every drive names           |
| `image`  | none    | The disk image in the drive at power on, or none for an empty drive  |

The preset is the drive's kind, and the drive's other settings change what it gives, in any order, so `{ preset: "hd-20m", latency_ms: 30 }` is a 20 MB hard disk with a faster seek. The presets are the Disk Drive's [timing presets](3.5-Disk-Drive.md#timing-presets):

| Preset         | Approximates                            |
| -------------- | --------------------------------------- |
| `floppy-360k`  | 5¼″ 360 KB double-density floppy (1981) |
| `floppy-720k`  | 3½″ 720 KB double-density floppy (1986) |
| `floppy-1440k` | 3½″ 1.44 MB high-density floppy (1987)  |
| `hd-10m`       | 10 MB hard disk, PC/XT class (1983)     |
| `hd-20m`       | 20 MB hard disk (1984)                  |
| `hd-40m`       | 40 MB hard disk, AT class (1986)        |

An image is a host file, made with [`oz3dsk`](3.5-Disk-Drive.md#oz3dsk), whose path is written the host's way. A relative path is from the config file's folder, so a config and its disks move together. Two drives can't have the same image, and while an Ozzy runs, nothing else may change its images (see [Disks coming and going](3.5-Disk-Drive.md#disks-coming-and-going)). A read-only image is a write-protected disk.

#### Errors

A config file with an error doesn't make an Ozzy. An unknown key, a value of the wrong type, and a value out of range are all errors, so a typo never goes unnoticed. Each error names the file, the line and column of the value it is about, and its setting, by its keys and list indexes:

```
hello.ozzy(12:5): devices[1].port: unknown key
hello.ozzy(2:8): cores: must be an int
hello.ozzy(15:3): drives[1]: missing preset
```

The Ozzy's own rules are checked once the whole file is read, so their errors name the file and the setting, but not a line:

```
hello.ozzy: banks[0]: RAM can't be on the boot ROM's page
hello.ozzy: banks[0]: page 1 must be RAM, for the boot ROM's stack
hello.ozzy: screen: page 15 of bank 0 is RAM
hello.ozzy: timer: tick_hz must be at most 65535
hello.ozzy: drive B: image disks/a.ozd is also drive A's
```

A device's settings that don't fit together (such as a screen of more than 4080 cells) are errors naming the device: `Screen`, `Teletype`, `Timer`, or a drive's letter, as in `Disk drive A: has 0 slots, but it must have 1 to 255`. So is an image that doesn't open: `Drive A: Disk image disks/system.ozd doesn't exist`.

### Memory

Each page of a bank is RAM, the boot ROM, a device's, or empty. An empty page has no memory: it reads 0, and writes to it are ignored.

| Bank 0 pages | What                                                                     |
| ------------ | ------------------------------------------------------------------------ |
| 0            | The boot ROM, with the machine table at F00h                             |
| 1            | RAM, always, with the loader at 1000h and the boot ROM's stack below 1200h (see [The boot ROM's stack](#the-boot-roms-stack)) |
| 2 to 14      | RAM, by default                                                          |
| 15           | The screen, if the Ozzy has one                                          |

* **The boot ROM** is page 0 of bank 0, in every Ozzy. Cores can read it, but not write it.
* **RAM** is the pages the config's `ram` masks give, and no others. It is all 0 at power on. Page 1 of bank 0 is always RAM, for the boot ROM's stack and the loader (see [The boot ROM's stack](#the-boot-roms-stack)).
* **A device's pages** are its own, as the screen's page is (see [Screen](3.3-Screen.md)). They are never RAM or the boot ROM, so the machine table says plainly what each page is. Without a screen, page 15 of bank 0 may be RAM.

### The machine table

At power on, the Ozzy writes the **machine table** into the boot ROM's page, at F00h in bank 0, so the boot ROM, and the operating system after it, learn the hardware from it. It is 256 words, and everything in it has a fixed place, so a program finds what it needs at a constant address:

| Words      | What                                  |
| ---------- | ------------------------------------- |
| F00h-F07h  | The header                            |
| F08h-F27h  | 16 bank entries, of 2 words each      |
| F28h-FA7h  | 16 device entries, of 8 words each    |
| FA8h-FFFh  | Reserved: 0                           |

| Word | Header                                     |
| ---- | ------------------------------------------ |
| 0    | Version: 1                                 |
| 1, 2 | Clock rate, in Hz: a dword, low word first |
| 3    | Cores                                      |
| 4    | Ports                                      |
| 5    | Banks                                      |
| 6    | Devices                                    |
| 7    | Reserved: 0                                |

Bank `n`'s entry is at `F08h + 2n`, with bit `p` for page `p`. A bank the Ozzy doesn't have is 0, 0: no ROM, and no RAM.

| Word | Bank entry                                   |
| ---- | -------------------------------------------- |
| 0    | ROM pages: 0001h for bank 0, and 0 otherwise |
| 1    | RAM pages                                    |

Device entry `n` is at `F28h + 8n`. The Ozzy's devices come first, in a fixed order: the power control, the screen, the keyboard, the teletype, the timer, then the drives in letter order, for those it has. The entries after them are all 0.

| Word | Device entry                                                                                   |
| ---- | ---------------------------------------------------------------------------------------------- |
| 0    | Type: 1 for the screen, 2 the keyboard, 3 the teletype, 4 the timer, 5 a drive, 6 the power control, 0 for none |
| 1    | Port (the keyboard's typed keys, the timer's tick port, or a drive's command port), or FFFFh for none |
| 2    | Interrupt, or FFFFh for none                                                                   |
| 3    | Interrupt cores, with bit `n` for core `n`, and bit 15 if it is shared, or 0 for no interrupt  |
| 4    | Bank of the device's pages, or FFFFh for none                                                  |
| 5    | First page, or FFFFh for none                                                                  |
| 6, 7 | The type's own: the screen's columns and rows, the keyboard's first key state port (FFFFh for none) and 0, the timer's ticks a second and 0, a drive's letter (41h to 44h for `A` to `D`) and slots, and otherwise 0 |

* A device's port, interrupt, and pages are always the same (see [Devices](#devices)), but the table gives them anyway, so it describes the hardware whole.
* The interrupt cores are only cores the Ozzy has, so the screen's, every core, is 0001h on an Ozzy with one core, and the keyboard's, shared by every core, is 8001h.
* A program finds a device by its type, not its place in the table, as an Ozzy may not have every device. It reads the entries from F28h until the header's count of devices, or until an entry of type 0. Devices to come will have types of their own.
* What disk is in a drive, if any, isn't in the table, as it changes while the Ozzy runs. A program asks the drive (see [Disk](3.5-Disk-Drive.md#disk)).
* A program checks the version, which changes only if the table's layout does.

For example, the default Ozzy's table is:

| Words      | Values                               | What                                                            |
| ---------- | ------------------------------------ | --------------------------------------------------------------- |
| F00h-F07h  | 1, 4240h, 000Fh, 1, 30h, 1, 4, 0     | Version 1, 1 MHz, 1 core, 48 ports, 1 bank, and 4 devices       |
| F08h-F09h  | 0001h, 7FFEh                         | Bank 0: the boot ROM on page 0, and RAM on pages 1 to 14        |
| F0Ah-F27h  | 0                                    | Banks 1 to 15, which it doesn't have                            |
| F28h-F2Fh  | 6, 0, FFFFh, 0, FFFFh, FFFFh, 0, 0   | The power control, on port 0                                    |
| F30h-F37h  | 1, FFFFh, 6, 0001h, 0, 0Fh, 50h, 18h | The screen: interrupt 6 on core 0, page 15 of bank 0, 80 by 24  |
| F38h-F3Fh  | 2, 1, 1, 8001h, FFFFh, FFFFh, 6, 0   | The keyboard: typed keys on port 1 with interrupt 1 shared on core 0, and key state on ports 6 to 15 |
| F40h-F47h  | 4, 3, 0, 0001h, FFFFh, FFFFh, 3Ch, 0 | The timer: ticks on port 3 with interrupt 0 on core 0, and the clock on port 4, at 60 ticks a second |
| F48h-FFFh  | 0                                    | No more devices, and the reserved words                         |

### The boot ROM

At power on, core 0 runs the boot ROM from its start, and the other cores are idle. The boot ROM reads the machine table to find the screen, the keyboard, the teletype, and drive `A:`, shows the banner, and then:

* **With a keyboard**, it shows the prompt, `Press Enter to Boot...`, and waits for Enter, to boot from the disk in `A:` (see [Booting](#booting)).
* **Without one**, it halts, as nothing could answer a prompt.

It shows everything on the screen, or without one, on the teletype, or with neither, nowhere, though it still boots.

**The banner** is the boot ROM's version, then a blank line, then the Ozzy's hardware from the machine table: its cores, RAM, and clock rate, and its drives, by letter. The RAM is in K, as old machines wrote it: on an Ozzy, K is 1024 words (1 Kiw), so a page is 4K. The clock rate is in the largest of Hz, kHz, and MHz that it is a whole number of (`4770 kHz`, or `32768 Hz`). Without drives, the drives line is `Drives: none`.

**On the screen**, the boot ROM clears the screen, filling every cell with a space, and shows the banner at the top, each line a column in, in a box drawn with the cells' edge lines (see [Screen](3.3-Screen.md)), so the box takes no cells of its own. The box is the banner's four rows, from the first column to the blank column after its widest line: its top row's cells have the top line, its bottom row's the bottom line, its first column's the left line, and its last column's the right line, so the corners have two. Then come a blank row, the prompt, another blank row, and the **status row**, empty until Enter. While the boot ROM waits, the cursor, a slow blinking underline, is after the prompt. The default Ozzy shows this (with the box drawn here in line characters):

```
┌────────────────────────┐
│ Ozzy 1.0               │
│                        │
│ 1 core  56K RAM  1 MHz │
│ Drives: none           │
└────────────────────────┘

Press Enter to Boot...
```

The screen never scrolls, and no line wraps: on a screen too small for it, what passes the right edge or the bottom row isn't drawn.

**On the teletype**, the banner and the prompt are the same without the box, each line ending with a carriage return and a line feed, and each status is a line of its own after the prompt. After an error, it prints a blank line and the prompt again:

```
Ozzy 1.0

1 core  56K RAM  1 MHz
Drives: A

Press Enter to Boot...
Loading...
ERROR: No disk in drive A:

Press Enter to Boot...
Loading...
```

#### Waiting for Enter

With a keyboard, the boot ROM is the keyboard's interrupt handler, for interrupt 1 on core 0, and waits with `WAIT`, which each key's interrupt ends. The other cores have no handler for it, so each drops its own copy. It reads the keys from port 1 (see [Keyboard](3.2-Keyboard.md)), and ignores every one but Enter, echoing nothing. Only the key counts, not the modifier keys held with it, so `Ctrl` with `M`, which is Enter (0Dh), boots too. Keys pressed while the boot ROM shows the banner wait in the keyboard, and count once it prompts. Keys pressed while it boots are thrown away, so they neither count toward the next prompt nor reach the loader.

Interrupts are enabled only while it waits. The boot ROM sets no handler for any other interrupt, so a core 0 interrupt from another device, such as the screen's refresh or the timer's tick, is dropped.

#### Booting

On Enter, the boot ROM hides the cursor and shows `Loading...`. Then it reads the boot area of the disk in `A:` (see [Superblock](3.5-Disk-Drive.md#superblock)) with `READ BOOT` to 1000h to 10DFh of bank 0, just below its stack, and checks that its last two words are the **boot signature**, `"BOOT"` packed: 4F42h, 544Fh. If they are, it starts the code in the boot area, the **loader** (see [The loader](#the-loader)). If anything fails, it shows why and waits for Enter again:

| Why                                     | Status                       |
| --------------------------------------- | ---------------------------- |
| The Ozzy has no drive `A:`              | `ERROR: No drive A:`         |
| **no disk** (20h)                       | `ERROR: No disk in drive A:` |
| **unformatted** (23h)                   | `ERROR: Disk not formatted`  |
| The last two words aren't the signature | `ERROR: Not a system disk`   |
| Any other status, such as **I/O error** | `ERROR: Disk error 25h`      |

Each status replaces the last on the status row, which is cleared first. The boot ROM boots only from `A:`, whatever disks the other drives hold. It resets the drive first, so it starts from idle whatever ran before, and polls the drive's ports, never taking its interrupt (see [Disk Drive](3.5-Disk-Drive.md)). A disk can be put in `A:` while the boot ROM waits, and Enter tries again.

#### The loader

A disk whose boot area holds a loader is a **system disk**. The loader is the disk's own code, so what it loads, and how, is up to it, such as an operating system file, with the drive's ordinary commands. It is a bare program of at most 224 words, ending with the boot signature, which `oz3asm --bare` assembles and `oz3dsk boot` writes (see [Bare programs](2.4-Program-Files.md#bare-programs) and [oz3dsk](3.5-Disk-Drive.md#oz3dsk)):

```
; boot.oza: assembled with oz3asm --bare, then written with
;   oz3dsk system.ozd boot boot.oz3
    .STACK 0                ; It runs on the boot ROM's stack
    ...                     ; The loader
    .DS 222 - $             ; Padding up to word 222
    .DW "BOOT"              ; The boot signature, in the last two words
```

The boot ROM starts it with `RST SELF, CODE | STACK | DATA | EXTRA` (see [RST](2.2-Default-Instruction-Set.md#rst)), so:

* Every bank is bank 0.
* `BC` and `BD` are 1000h, the loader's first word: it starts at `IP` 0, and reads its own words as `D(n)`.
* `BS` is 1200h, the end of the boot ROM's stack, with `SP` 0, so the loader's stack starts empty at the top of the boot ROM's.
* `BE` is 0, so the machine table is at `E(F00h)`.
* `R0` is the boot drive's command port (16 for `A:`), and `R1` its number (0 for `A:`).
* No interrupt handlers are set and interrupts are disabled, as `RST` of `CODE` leaves them, and its notify flag (`N`) is set, so its first `WAIT` ends at once.
* The keyboard is empty, as the boot ROM throws away the keys pressed while it boots, and no interrupt its own work raised is pending, such as the drive's for `READ BOOT`, so the loader's first drive interrupt is for its own command.
* RAM is as at power on, but for the loader itself and the boot ROM's stack, which the loader now owns, so it may use all of 1000h to 11FFh.

The other cores are still idle, as at power on, until the loader or what it loads starts them.

A loader that fails hands back to the boot ROM: it puts an error code from 1 to FFh in `R0`, and jumps to the boot ROM's **exit**, at 0002h of bank 0, which is `JP 0F002h` from the loader's `BC` of 1000h. The exit restarts the boot ROM with `RST SELF, CODE | STACK | DATA`, keeping `R0`, so the boot ROM starts again as at power on, but for showing `ERROR: Loader error XXh`, the code as a hex byte (only `R0`'s low byte), as the status, below the prompt. Enter boots again, from whatever disk is in `A:` by then. Keys pressed while the loader ran are thrown away. RAM isn't cleared, and the loader may leave the drive busy, as the boot ROM resets it before it reads. The loader needs to know nothing about the screen, teletype, or keyboard to report its error, as the boot ROM shows it. An error code of 0 is the boot ROM starting at power on, so it shows no error.

#### The boot ROM's stack

The boot ROM's stack is in page 1 of bank 0, which is always RAM, and ends at 1200h, just after the loader's 224 words at 1000h.

* Core 0's `STACK` bank is bank 0, and `BS` is 1200h, so the stack grows down from 11FFh. The host sets `BS` at power on, and the boot ROM's exit sets it again.
* The boot ROM's variables are the first 9 words it pushes, at the top of the stack (11F7h to 11FFh), and its stack is below them, above the loader.
* The rest of RAM the boot ROM leaves alone, so all of it from 1200h is free for what the loader loads.

The boot ROM's source is `oz3/ozzy/machine/boot_rom.oza`, written for the [default instruction set](2.2-Default-Instruction-Set.md), which `oz3asm` assembles for a listing (see [Program Source](2.5-Program-Source.md#oz3asm)).

#### The default loader

The default loader, `oz3/ozzy/machine/loader.oza`, is the loader a system disk is given by default. It loads `OZOS.SYS`, the operating system, from the boot drive and starts it. The file is a [bare program](2.4-Program-Files.md#bare-programs), which the loader puts just after itself and the boot ROM's stack, in bank 0: from 1200h, through as many of the pages after page 1 as its contents and stack fill. If those pages aren't all RAM in the [machine table](#the-machine-table), or it would pass the end of bank 0, the loader puts it at 0 in the first of banks 1 to 15 whose pages are RAM from page 0 to where it ends. Either way, its contents and stack are at most FFFFh words, as the header says them in a word.

The loader opens the file, reads all of it with one `READ PACKED`, sending each data block's words straight to where they go as they arrive, and closes it (see [Disk Drive](3.5-Disk-Drive.md)). It writes the blocks with `EXTRA` set to where the operating system goes. Then it starts it as a bare program at its first word, `b` (1200h in bank 0, or 0 in another bank), with `RST SELF, CODE | STACK | DATA | EXTRA` (see [RST](2.2-Default-Instruction-Set.md#rst)), so:

* `CODE`, `STACK`, and `DATA` are its bank, which it can read from `MB`. `BC` and `BD` are `b`, and `BS` is `b` plus its contents and stack words, with `IP` and `SP` 0.
* `EXTRA` is bank 0 at base 0, so the machine table is at `E(F00h)`.
* `R0` and `R1` are the boot drive's command port and number, as the loader was given them.
* `R2` is the pages of its bank the operating system is in, from its first (page 1 in bank 0) to its last, a bit for each (bit 0 for page 0), so that it doesn't put programs over itself. The loader and the boot ROM's stack, at 1000h to 11FFh of bank 0, are free once it starts, and so is every RAM page `R2` leaves out.
* Interrupts are disabled with no handlers set, and the notify flag is set, as `RST` of `CODE` leaves them.

If it can't load the file, it fails with an error code, which the boot ROM shows as `ERROR: Loader error XXh` (see [The loader](#the-loader)):

| Error code | Why                                                                                       |
| ---------- | ----------------------------------------------------------------------------------------- |
| 01h to 25h | The drive's status for a command that failed, such as **not found** (10h) for no `OZOS.SYS`, or **I/O error** (25h) (see [Disk Drive](3.5-Disk-Drive.md)) |
| F1h        | The file isn't a bare program                                                             |
| F2h        | The file isn't exactly as long as its header and blocks say, or a block ends past its contents |
| F3h        | The file doesn't fit: not in bank 0 from 1200h, nor in any other bank from 0             |

A block that ends past the contents fails before any of its words are written, so the loader never writes outside the RAM it found for the operating system. It doesn't check the rest of what makes a bare program valid, such as blocks being in order and not overlapping, which can't write outside it.

### Running Ozzy

`ozzy` runs an Ozzy in the console (the Windows console, or a terminal on Linux and other POSIX systems), from its config file, or the default Ozzy without one:

```
ozzy [--max-speed] [--stats] [config file]
```

The repository's [configs](../../configs) folder has configs to start from: `console.ozzy`, the default Ozzy written out, and `teletype.ozzy`, an Ozzy with a teletype in place of its screen.

It runs at the Ozzy's clock rate, or with `--max-speed`, as fast as the host can. If the host falls behind, the Ozzy slows down rather than racing to catch up. A config file with an error prints it and doesn't run. With `--stats`, once it quits, it says how it ran: the frames it drew a second, the time each took on average, and the Ozzy's cycles a second.

* **The screen** is drawn as text, in place of what the console showed, which is back once `ozzy` quits. Each character is drawn as the Unicode character closest to its glyph, with the inverse, half intensity, blink, and top and bottom line attributes, and the cursor in its place, shape, and blinking. Text can't show the left and right lines, redefined glyphs, the alternate character set, or compact rows, so those are left out. The console needs to be at least as large as the screen: while it is smaller, it says how large it needs to be in place of the screen. A font with Unicode's sextants (such as Cascadia Mono) draws them.
* **The teletype**, in an Ozzy without a screen, prints in the console as it comes, and its lines stay once `ozzy` quits, which ends the last line if the teletype left it unfinished. Control characters other than bell, backspace, tab, line feed, and carriage return are dropped. In an Ozzy with a screen, the teletype's output isn't shown.
* **The keyboard** takes the console's keys, as typed keys, with the key events [Keyboard](3.2-Keyboard.md#key-events) lists, and as key state, each key held from its press to its release, by where it is on the keyboard (see [Key codes](3.2-Keyboard.md#key-codes)). Every key is typed, Ctrl+C too, but for the keys that quit, which are still in the key state. When the console loses focus, every key held is released, as the console doesn't see keys released elsewhere. A terminal sends only the characters typed, so there a character holds no Shift, and the key state is made up from each typed key: its key is where it is on a US keyboard (`!` is the 1 key), pressed and released at once, with the left Shift, Ctrl, and Alt its modifiers say. So a program sees each key pressed but never held, and a key held down pressed again as the terminal repeats it.
* **The drives** have the disks the config gives them from power on, and keep them until the Ozzy powers off, when each drive writes back to its image what it hadn't yet. Closing the Windows console's window gives `ozzy` only the few seconds Windows allows before ending it, which may not be long enough for a large write-back, so quit with Ctrl+Alt+Q to be sure. Disks can't be changed while it runs. Meanwhile, nothing else may change their images, not even `oz3dsk`.
* **The timer's date and time**, and the drives' file stamps, are the host's, in its local time zone. They keep the host's time even when the Ozzy doesn't keep up with its clock rate, or runs with `--max-speed`, so then they drift from the timer's ticks and milliseconds.
* **Powering off** is a program's to ask for, on the power port (see [Power Control](3.6-Power-Control.md)). A restart powers the Ozzy off and on again at once: RAM is cleared, every device starts as at power on, the drives write back and then have the config's disks again, and the boot ROM starts over. A shutdown powers it off, and leaves it off, saying so with the exit code (`Ozzy shut down, exit code 3.`), until `ozzy` quits, which exits with 0. If a program asks more than once before the Ozzy powers off, which takes up to a frame, the first request is the one acted on.
* **Quitting** is Ctrl+Alt+Q, Ctrl+Break in the Windows console, or closing the console's window. AltGr is Ctrl+Alt, so where AltGr with Q types a character (such as @), it types it rather than quitting.
