# Ozzy Computer

The **Ozzy** is a computer built on the OZ-3: a processor with its cores, memory banks, and ports, a console of a [screen, a keyboard, and a teletype](3-Devices.md), a [timer](3.4-Timer.md), and up to four [disk drives](3.5-Disk-Drive.md), `A:` to `D:`.

Ozzys aren't all alike. A config file says how fast an Ozzy is, how many cores and how much memory it has, and which devices it has. Programs find out what they are running on from the **machine table**, which the Ozzy puts in memory at power on. How each device is wired (its port, interrupt, and memory) is the same in every Ozzy, so programs can rely on it.

### The config file

An Ozzy is described by a `.ozzy` file. Everything has a default, so an empty file is the default Ozzy: 1 MHz, one core, 48 ports, one bank with 56 Kiw of RAM, a screen, a keyboard, and a timer, with no drives.

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

#### Banks

`banks` is a list of the Ozzy's banks, from bank 0. Each is a map with one key, `ram`: its RAM pages, as a mask with bit `n` for page `n`, as the read and write masks state pages (see [Specifications](2-Specifications.md#overview)). In hexadecimal, each digit is four pages. A bank with no `ram` has none. The default is one bank, `{ ram: 0x7FFE }`.

Page 0 of bank 0 is always the boot ROM, so bank 0's mask can't have bit 0. An Ozzy must have some RAM, in any bank, as the boot ROM needs a page of it for its stack.

#### Devices

`devices` is a list of the Ozzy's devices, each a map with a `type` of `"screen"`, `"keyboard"`, `"teletype"`, or `"timer"`, and its settings. A setting left out takes its default, so `{ type: "screen" }` is an 80 by 24 screen. Without `devices`, the Ozzy has a screen, a keyboard, and a timer, each with its defaults, and `devices: []` is an Ozzy with none.

An Ozzy has at most one of each device, as its console shows one screen and one teletype, it is typed on with one keyboard, and one timer is all its cores need.

Each device is wired the same way in every Ozzy, so a config file doesn't say how:

| Device   | Ports                                   | Interrupt               | Memory            |
| -------- | --------------------------------------- | ----------------------- | ----------------- |
| Screen   | none                                    | 6, on every core        | Page 15 of bank 0 |
| Keyboard | 1 for typed keys, and 6 to 15 key state | 1, shared by every core | none              |
| Teletype | 2                                       | none                    | none              |
| Timer    | 3 for ticks, and 4 the clock            | 0, on every core        | none              |
| Drive A: | 16 to 18                                | 2, on core 0            | none              |
| Drive B: | 19 to 21                                | 3, on core 0            | none              |
| Drive C: | 22 to 24                                | 4, on core 0            | none              |
| Drive D: | 25 to 27                                | 5, on core 0            | none              |

The keyboard has both typed keys and key state (see [Keyboard](3.2-Keyboard.md)), and its interrupt is for typed keys. Its key state is on ports 6 to 15. Each drive has 3 ports, its command, data, and control ports (see [Ports](3.5-Disk-Drive.md#ports)), from port 16 for `A:`, leaving ports 0, 5, and 28 to 31 for devices to come. An Ozzy's drives are lettered in order, from `A:`, so an Ozzy with drives always has an `A:`, which may be empty.

The keyboard's interrupt is shared, so each key is handled by only one core (see [Interrupts](2-Specifications.md#interrupts)). The screen's refresh and the timer's tick are news for every core, so they are not shared, and every core with a handler for them is told. A drive's completion is for core 0 alone, which boots the Ozzy and runs the operating system, as the operating system issues every drive command, and a completion can be read only once (see [The command protocol](3.5-Disk-Drive.md#the-command-protocol)). Another core can still use a drive by reading its control port, but only core 0 can wait for its interrupt.

So with a screen, page 15 of bank 0 must not be RAM.

A device's other keys are its settings, with the names, defaults, and ranges its own page gives: see [Screen](3.3-Screen.md#wiring-and-settings), [Teletype](3.1-Teletype.md#wiring-and-settings), and [Timer](3.4-Timer.md#wiring-and-settings). The keyboard has no settings, so it is only `{ type: "keyboard" }`. A timer's `tick_hz` is at most 65535, as the machine table holds it in a word, and at most `clock_hz`, as the timer ticks at most once a cycle. So an Ozzy slower than 60 Hz needs a slower timer than the default, or none.

#### Errors

A config file with an error doesn't make an Ozzy. An unknown key, a value of the wrong type, and a value out of range are all errors, so a typo never goes unnoticed. Each error names the file, the line and column of the value it is about, and its setting, by its keys and list indexes:

```
hello.ozzy(12:5): devices[1].port: unknown key
hello.ozzy(2:8): cores: must be an int
```

The Ozzy's own rules are checked once the whole file is read, so their errors name the file and the setting, but not a line:

```
hello.ozzy: banks[0]: RAM can't be on the boot ROM's page
hello.ozzy: screen: page 15 of bank 0 is RAM
hello.ozzy: timer: tick_hz must be at most 65535
```

A device's settings that don't fit together (such as a screen of more than 4080 cells) are errors naming the device: `Screen`, `Teletype`, or `Timer`.

### Memory

Each page of a bank is RAM, the boot ROM, a device's, or empty. An empty page has no memory: it reads 0, and writes to it are ignored.

| Bank 0 pages | What                                                                     |
| ------------ | ------------------------------------------------------------------------ |
| 0            | The boot ROM, with the machine table at F00h                             |
| 1            | RAM, by default, with the boot ROM's stack at its top (see [The boot ROM's stack](#the-boot-roms-stack)) |
| 2 to 14      | RAM, by default                                                          |
| 15           | The screen, if the Ozzy has one                                          |

* **The boot ROM** is page 0 of bank 0, in every Ozzy. Cores can read it, but not write it.
* **RAM** is the pages the config's `ram` masks give, and no others. It is all 0 at power on. The boot ROM's stack is in its first page (see [The boot ROM's stack](#the-boot-roms-stack)).
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

Device entry `n` is at `F28h + 8n`. The Ozzy's devices come first, in a fixed order: the screen, the keyboard, the teletype, the timer, then the drives in letter order, for those it has. The entries after them are all 0.

| Word | Device entry                                                                                   |
| ---- | ---------------------------------------------------------------------------------------------- |
| 0    | Type: 1 for the screen, 2 the keyboard, 3 the teletype, 4 the timer, 5 a drive, 0 for none    |
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
| F00h-F07h  | 1, 4240h, 000Fh, 1, 30h, 1, 3, 0     | Version 1, 1 MHz, 1 core, 48 ports, 1 bank, and 3 devices       |
| F08h-F09h  | 0001h, 7FFEh                         | Bank 0: the boot ROM on page 0, and RAM on pages 1 to 14        |
| F0Ah-F27h  | 0                                    | Banks 1 to 15, which it doesn't have                            |
| F28h-F2Fh  | 1, FFFFh, 6, 0001h, 0, 0Fh, 50h, 18h | The screen: interrupt 6 on core 0, page 15 of bank 0, 80 by 24  |
| F30h-F37h  | 2, 1, 1, 8001h, FFFFh, FFFFh, 6, 0   | The keyboard: typed keys on port 1 with interrupt 1 shared on core 0, and key state on ports 6 to 15 |
| F38h-F3Fh  | 4, 3, 0, 0001h, FFFFh, FFFFh, 3Ch, 0 | The timer: ticks on port 3 with interrupt 0 on core 0, and the clock on port 4, at 60 ticks a second |
| F40h-FFFh  | 0                                    | No more devices, and the reserved words                         |

### The boot ROM

At power on, core 0 runs the boot ROM from its start, and the other cores are idle. The boot ROM reads the machine table to find the screen and the teletype, and then:

* **With a screen**, it clears the screen, filling every cell with a space, and prints the banner at the top. Then it leaves a blank line, and shows the cursor, a slow blinking underline, at the start of the line after it.
* **Without one, with a teletype**, it prints the banner to the teletype, followed by a carriage return and a line feed, twice.
* **With neither**, it has nowhere to print.

Then it enables interrupts and halts. A halted core still handles interrupts, so from then on the boot ROM runs only to echo keys (see [Echoing keys](#echoing-keys)).

The banner names the machine: `OZZY`, the boot ROM's version, and the clock rate, cores, and RAM from the machine table. The clock rate is in the largest of Hz, kHz, and MHz that it is a whole number of (`4770 kHz`, or `32768 Hz`), and the RAM in K, as old machines wrote it: on an Ozzy, K is 1024 words (1 Kiw), so a page is 4K:

```
OZZY 0.1  1 MHz  1 core  56K RAM
```

On a screen too narrow for it, the banner wraps onto the next row at the right edge, and a new line on the bottom row scrolls the screen up a row, clearing the new bottom row (see [Scrolling and filling](3.3-Screen.md#scrolling-and-filling)). So a small screen shows what fits.

#### Echoing keys

With a keyboard, the boot ROM is the keyboard's interrupt handler, for interrupt 1 on core 0. The other cores have no handler for it, so each drops its own copy, and core 0 handles every key. Each time, it reads one key from port 1 (see [Keyboard](3.2-Keyboard.md)) and echoes it where the banner went, and the cursor follows:

* **A character**, 20h to 7Eh, prints at the cursor, which moves right. After a full row, the cursor stays on the row's last column, and the next character goes at the start of the next row.
* **Enter** starts a new line.
* **Backspace** erases the character before the cursor, which moves back onto it. It does nothing at the start of a row, so it never goes back to the row above.
* **Other keys** are ignored.

Only the key counts, not the modifier keys held with it: `Shift` with `A` is `A`, and `Ctrl` with `H` is Backspace (08h).

On the teletype, Enter is a carriage return and a line feed, and Backspace is a backspace, a space, and a backspace, even at the start of a line, as the boot ROM doesn't know where the teletype is on its line.

Keys pressed while the boot ROM prints the banner wait in the keyboard, and echo once it is done. The boot ROM sets no handler for any other interrupt, so a core 0 interrupt from another device, such as the screen's refresh or the timer's tick, is dropped.

#### The boot ROM's stack

The boot ROM's stack is the first page of RAM in the lowest bank that has any: page 1 of bank 0 by default, or else the lowest RAM page of bank 0, or of the first bank after it with RAM. So every Ozzy must have some RAM.

* Core 0's `STACK` bank is that bank, and `BS` is the end of the page (2000h for page 1), so the stack grows down from the page's last word (1FFFh by default).
* The boot ROM's variables are the first 6 words it pushes, at the top of the page (1FFAh to 1FFFh by default), and its stack is below them. It uses far less than the page.
* The rest of RAM, and the rest of that page, the boot ROM leaves alone, and it uses the page for as long as it runs.

The boot ROM's source is `oz3/ozzy/boot_rom.oza`, written for the [default instruction set](2.2-Default-Instruction-Set.md), which `oz3asm` assembles for a listing (see [Program Source](2.5-Program-Source.md#oz3asm)).

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
* **The timer's date and time** are the host's, in its local time zone. They keep the host's time even when the Ozzy doesn't keep up with its clock rate, or runs with `--max-speed`, so then they drift from the timer's ticks and milliseconds.
* **Quitting** is Ctrl+Alt+Q, Ctrl+Break in the Windows console, or closing the console's window. AltGr is Ctrl+Alt, so where AltGr with Q types a character (such as @), it types it rather than quitting.
