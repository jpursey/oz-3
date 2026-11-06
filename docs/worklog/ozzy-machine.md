# Ozzy Machine

The first Ozzy anyone can run. The new `ozzy` library builds a machine from a
config file: the processor, its banks and cores, and the console devices (see
[console-devices.md](console-devices.md)). It writes the **machine table** and
the **boot ROM** into page 0 of bank 0, and starts core 0, which clears the
screen, prints a banner, and echoes keys. The new `ozzy` app runs it in the
Windows console in real time:

```
ozzy                  // The default Ozzy: one core, a screen, and a keyboard
ozzy hello.ozzy       // The Ozzy hello.ozzy describes
```

A host that embeds Ozzy (a game, or a test) uses the library alone:

```
absl::StatusOr<OzzySettings> settings = ReadOzzySettings(text);
if (!settings.ok()) { ... }
absl::StatusOr<std::unique_ptr<Ozzy>> ozzy = Ozzy::Create(*settings);
if (!ozzy.ok()) { ... }
(*ozzy)->GetKeyboard()->PressKey('A');
(*ozzy)->Run(absl::Milliseconds(16));
const ScreenPicture& picture = (*ozzy)->GetScreen()->GetPicture();
```

This is item 5 of [ozzy_design.md](../ozzy_design.md)'s *Getting there*, and
settles what that doc leaves as a sketch for this item: the config file's
format, the machine table's layout, and the boot ROM. It also creates Ozzy's
wiki pages, which document the machine and its console devices for
programmers.

## Behavior

### The config file

A `.ozzy` file is Game Bits' text config format, rootless (no `{...}` around
it), with comments. Everything has a default, so an empty file (or no file) is
the default Ozzy:

```
// hello.ozzy
clock_hz: 1000000,  // 1 Hz to 100 MHz
cores: 4,           // 1 to 8
ports: 32,          // 16 to 256
banks: [
  { ram: 0x7FFE },  // Bank 0: pages 1 to 14 (page 0 is always the boot ROM)
  { ram: 0xFFFF }   // Bank 1: every page
],
devices: [
  { type: "screen", bank: 0, page: 15, interrupt: 6, columns: 80, rows: 24 },
  { type: "keyboard", port: 1, interrupt: 1, interrupt_cores: 0 }
]
```

As in JSON, entries are separated by commas, including the file's own, and
there is no comma after the last one.

- **Banks** are 1 to 16, and each gives its RAM pages as a mask, bit `n` for
  page `n`, as `MemoryPageMasks`, the machine table, and the assembler's
  `.BANK` masks all state pages. Game Bits' text config reads `0x` hex, so
  a mask reads as its pages. Page 0 of bank 0 is always the boot ROM, so bank
  0's mask can't have bit 0. The default is one bank, `0x7FFE`.
- **Devices** are a list, each with a `type`, and the settings of its
  `<Device>Settings` (see console-devices.md), in snake case as there. Any
  setting left out takes Ozzy's default for that type (the table below), so
  `{ type: "keyboard" }` is a keyboard on port 1. Without `devices`, the Ozzy
  has a screen and a keyboard. `devices: []` is an Ozzy with none.
- **`interrupt_cores`** is the cores a device's interrupt line is connected
  to: one core (`0`), a list of them (`[0, 2]`), or `"all"`. Unlike pages,
  it names cores rather than giving a mask, as a single integer reads as a
  core, and a list is as short as a mask for the few cores a line goes to. A
  core the Ozzy doesn't have is an error, though `"all"` is whatever cores it
  has.
- An Ozzy has at most one of each console device, as the console frontend
  shows one screen and one teletype, and types on one keyboard. Their names in
  the `Processor` are `Screen`, `Keyboard`, and `Teletype`.
- A device's pages must be in a bank the Ozzy has, and not be RAM or the boot
  ROM, so the machine table says plainly what each page is.

| Type       | Defaults                                                                       |
| ---------- | ------------------------------------------------------------------------------ |
| `screen`   | bank 0, page 15, interrupt 6 on every core, 80 by 24, and the device's own rates |
| `keyboard` | port 1, interrupt 1 on core 0                                                  |
| `teletype` | port 2, baud 0 (as fast as cores write)                                        |

- Errors name where they are, as gb::Config keeps no line numbers:
  `devices[1].interrupt_cores: the Ozzy has no core 2`. An unknown key, a
  value of the wrong type, and a value out of range are all errors, so a typo
  never goes unnoticed.

### Memory

| Bank 0 pages | What                                                         |
| ------------ | ------------------------------------------------------------ |
| 0            | The boot ROM, with the machine table at 0F00h                |
| 1-14         | RAM (by default). The boot ROM's stack is in the first page. |
| 15           | The screen (by default)                                      |

### The machine table

The host writes the machine table into the boot ROM's page at power on, at
0F00h in bank 0 (`kMachineTableAddress`), so the boot ROM, and later the OS,
learn the hardware from it. It is 256 words: a header of counts, then an
entry for every bank there can be (16), then an entry for every device there
can be (16, `kMaxMachineDevices`), each in a fixed place, so a program finds
bank `n`'s or device `n`'s entry at a constant address. A bank or device the
Ozzy doesn't have is an entry of 0, and the rest of the table is 0.

| Word | Header                                    |
| ---- | ----------------------------------------- |
| 0    | Version: 1                                |
| 1-2  | Clock rate, in Hz (a dword, low word first) |
| 3    | Cores                                     |
| 4    | Ports                                     |
| 5    | Banks                                     |
| 6    | Devices                                   |
| 7    | Reserved: 0                               |

Then, from word 8 (`kMachineTableBanks`), an entry of 2 words for each of the
16 banks: the ROM page mask and the RAM page mask (bit `n` is page `n`). Then,
from word 28h (`kMachineTableDevices`), an entry of 8 words for each of the 16
devices, the Ozzy's devices first:

| Word | Device entry                                                  |
| ---- | ------------------------------------------------------------- |
| 0    | Type: 1 screen, 2 keyboard, 3 teletype, or 0 for no device    |
| 1    | Port, or FFFFh for none                                       |
| 2    | Interrupt, or FFFFh for none                                  |
| 3    | Interrupt cores: one bit per core                             |
| 4    | Bank, or FFFFh for none                                       |
| 5    | First page, or FFFFh for none                                 |
| 6-7  | The type's own: a screen's columns and rows, otherwise 0      |

Device types are numbers this item assigns; the timer and the drive take the
next ones when they land.

### The boot ROM

At power on, core 0 starts at IP 0 of the boot ROM. It reads the machine
table, and:
- **With a screen**, it clears the screen (filled with spaces), prints the
  banner at the top, and shows an underline cursor below it.
- **Without one, with a teletype**, it prints the banner to the teletype.
- **With a keyboard**, it sets a handler for the keyboard's interrupt and
  halts, so it runs only when a key arrives (a halted core still handles
  interrupts). Each key echoes: characters 20h to 7Eh print at the cursor,
  Enter starts a new line, and Backspace erases the character before the
  cursor. Other keys are ignored. On the screen, a new line past the bottom
  scrolls with the scroll row and clear row, and the cursor wraps to the next
  line at the right edge. On the teletype, Enter is CR LF, and Backspace is
  BS, space, BS.
- With neither a screen nor a teletype, or without a keyboard once the banner
  is printed, it halts for good.

The banner is `OZZY` and the boot ROM's version, then the cores and RAM from
the machine table, such as `OZZY 0.1  4 cores  120 Kiw RAM`. Printing the
numbers is the boot ROM's own decimal printing of a word, the first real OZ-3
code Ozzy runs. The clock rate is left out, as it is a dword, and printing one
needs a dword divide the default instruction set doesn't have.

### `Ozzy`

- `Ozzy::Create(settings)` builds the processor, writes the machine table,
  assembles and loads the boot ROM, and starts core 0. It returns an error for
  settings `ProcessorConfig::Validate` or a device's `Create` rejects, naming
  the device.
- `GetProcessor()`, and `GetScreen()`, `GetKeyboard()`, and `GetTeletype()`,
  which return null for a device the Ozzy doesn't have.
- `Run(time)` runs the cycles that `time` is at the clock rate, carrying the
  fraction of a cycle left over to the next call, so many short runs add up to
  the clock rate exactly. It returns the cycles it ran. Time comes from the
  caller, so a test runs a machine without real time passing.

### The `ozzy` app

`ozzy [config]` runs an Ozzy in the Windows console.
- **Screen:** it draws the screen's picture as text, in the console's
  alternate screen buffer, with virtual terminal sequences: each cell's
  character as its nearest Unicode character, inverse, half intensity
  (faint), blink, the bottom line (underline), and the top line (overline).
  The cursor is the console's own, in its position, shape (block, underline,
  or bar for the left edge), and blinking. It draws only the rows that changed
  since the last picture.
- **What text can't show:** the left and right lines, redefined glyphs, the
  alternate character set (which is a copy of the first, until redefined),
  compact rows, and the exact blink timing. A graphical frontend would show
  the pixels as they are (see *Graphical frontend* in the backlog).
- **Teletype:** for an Ozzy without a screen, it writes the teletype's output
  to the console as it comes.
- **Keyboard:** console key presses become Ozzy key events, with the codes and
  modifiers console-devices.md lists. Ctrl+C is a key like any other (03h).
- **Quitting:** Ctrl+Break, or closing the console window. It restores the
  console as it found it.
- **Pacing:** each frame (60 a second), it runs the cycles for the real time
  that passed, at most 100 ms of them, so a host that falls behind slows the
  Ozzy rather than racing to catch up. `--max_speed` instead runs as many
  cycles as the host can between frames.
- **Errors:** a config that doesn't read or build prints its error and exits
  with 1.

### Wiki

Two new pages, linked from Home in place of the "planned" entries:
- **3-Ozzy-Computer:** what Ozzy is, the config file, memory, the machine
  table, the boot ROM, and running `ozzy`.
- **3.1-Ozzy-Devices:** the screen, keyboard, and teletype for programmers,
  including the screen's built-in character set: the ports, the control
  words, the key codes, the cell attributes, and the timings a program sees.

## Design

### Names

| Name                      | Candidates                                     | Avoids                                                      | Choice              |
| ------------------------- | ---------------------------------------------- | ----------------------------------------------------------- | ------------------- |
| The library's main class  | `Ozzy`, `Machine`, `Computer`                  | `Processor` (the CPU alone); `Machine` (generic, and a host may have its own) | `Ozzy`              |
| What a config reads into  | `OzzySettings`, `OzzyConfig`, `OzzyOptions`    | `ProcessorConfig`, `DeviceConfig`, `gb::Config`             | `OzzySettings`      |
| Reading a config          | `ReadOzzySettings`, `ParseOzzySettings`        | `ReadConfigFromText` (Game Bits' own step)                  | `ReadOzzySettings`  |
| Running for real time     | `Run`, `RunFor`, `Advance`                     | `Processor::Execute` (cycles)                               | `Run`               |
| Picture to console text   | `DrawConsoleText`, `PictureToText`, `RenderText` | `Screen::GetPicture`, "render" (Game Bits' renderer)      | `DrawConsoleText`   |
| The host console          | `Console`, `Terminal`, `HostConsole`           | the console devices (Ozzy's own), `Teletype`                | `Console`           |

Ozzy is the computer, the hardware product, as the Osborne 1 was; the OS it
runs will have a name of its own, as the Osborne ran CP/M. So the class is
`Ozzy` (CL2 named it `Machine`, and CL3 renamed it), while the machine table
keeps its name, as it describes the machine to the programs on it.
`OzzySettings` follows the devices' `<Device>Settings`: what a host sets,
which a config file entry parses into. "Console" is the Windows console, the
host's; the console devices are always named by their own names.

### ozzy: the library

- `oz3/ozzy/`, the `oz3_ozzy` library. It links `oz3_core`, `oz3_tools` (to
  assemble the boot ROM), `oz3_instruction_sets`, `oz3_devices`, and Game
  Bits' `gb_config`. It comes after `devices` in the dependency order, and
  before `apps`, as CLAUDE.md already says.
- **Target names:** every library's target has the `oz3_` prefix and every
  executable's doesn't (`oz3_tools` and `oz3asm`), so the library is
  `oz3_ozzy` (with `oz3_ozzy_test`, and `oz3_ozzy.lib`), and the app is
  `ozzy` (`bin/ozzy.exe`). No two targets or outputs share a name, and the
  source files don't either: the library's are `oz3/ozzy/*`, and the app's
  is `oz3/apps/ozzy.cc`.
- It holds the console frontend's pieces too (`console_text`, and the
  `Console` interface with its `win_` implementation), rather than the app,
  as executables have no tests. A host that embeds Ozzy doesn't call them,
  and as a static library, doesn't link them.

### ozzy: OzzySettings

```
// ozzy_settings.h
// Ozzy's settings for each console device, which a device in a config file
// starts from. Functions, as ScreenSettings isn't a constant expression.
ScreenSettings GetOzzyScreenSettings();
KeyboardSettings GetOzzyKeyboardSettings();
TeletypeSettings GetOzzyTeletypeSettings();

struct OzzySettings {
  static constexpr int64_t kMaxClockHz = 100'000'000;
  static constexpr int kMinPorts = 16;

  int64_t clock_hz = ProcessorConfig::kDefaultClockHz;
  int cores = 1;
  int ports = 32;
  std::vector<uint16_t> banks = {0x7FFE};  // The RAM pages of each bank.
  std::optional<ScreenSettings> screen = GetOzzyScreenSettings();
  std::optional<KeyboardSettings> keyboard = GetOzzyKeyboardSettings();
  std::optional<TeletypeSettings> teletype;
};

// Returns an error naming the first setting that breaks a rule of
// OzzySettings, such as `screen: page 3 of bank 0 is RAM`.
absl::Status ValidateOzzySettings(const OzzySettings& settings);

// Reads settings from a config file's text, returning an error naming where
// the first problem is.
absl::StatusOr<OzzySettings> ReadOzzySettings(std::string text);
```

- `ValidateOzzySettings` holds Ozzy's own rules (the ranges, bank 0's
  boot ROM page, and a device's pages in a bank the Ozzy has and not RAM or
  the ROM), so `ReadOzzySettings` and `Ozzy::Create` share them, and a
  host that builds settings in code gets the same checks. Its errors name the
  setting (`screen.page`), not the text.
- Reading checks what only the text knows: keys, types, values that fit their
  setting, one device of each type, and the cores an `interrupt_cores` names
  (as `"all"` is read there). What the devices and the processor check
  themselves (a screen's size and rates, ports in range) is left to
  `Ozzy::Create`, so no rule is written twice.
- A screen's `characters` is an array of words, whose length the screen
  checks.
- `kMinPorts` keeps ports 0 to 15 for devices, as ozzy_design.md lays them
  out; 32 leaves room for the OS's call ports (16 to 23).

**Brittleness:** none.

### ozzy: Ozzy

```
// machine_table.h
inline constexpr uint16_t kMachineTableAddress = 0x0F00;
inline constexpr int kMachineTableSize = 256;
inline constexpr int kMaxMachineDevices = 16;
enum class MachineDeviceType : uint16_t { kScreen = 1, kKeyboard, kTeletype };
// ... the header and entry offsets.

// ozzy.h
class Ozzy final {
 public:
  static constexpr std::string_view kScreenName = "Screen";  // And the others.

  static absl::StatusOr<std::unique_ptr<Ozzy>> Create(
      const OzzySettings& settings);

  Processor* GetProcessor();
  Screen* GetScreen();      // Null without one.
  Keyboard* GetKeyboard();  // Null without one.
  Teletype* GetTeletype();  // Null without one.

  // Runs the cycles `time` is at the clock rate, and returns them.
  Cycles Run(absl::Duration time);
};
```

- **Banks:** each is one `MemoryBankConfig` over pages 0 to 15, readable on its
  RAM pages and ROM page, writable on its RAM pages. Pages in neither read 0
  and ignore writes, as a bank with no memory there.
- **The boot ROM** is `boot_rom.oza`, real OZ-3 source in the default
  instruction set, assembled in process by `Create` and loaded with
  `ProgramLoader`: its code is a `ROM PAGE(0)` segment in bank 0, and its
  stack a RAM segment the loader places. A program file can't be written into
  ROM by OZ-3 code, but `ProgramLoader` writes memory directly, as a ROM
  programmer would. `Create` checks the ROM's code ends below the machine
  table, and the source names the table's address as a constant, which a test
  checks against `kMachineTableAddress`.
- **The source is embedded** in the library at build time, with Game Bits'
  `gb_embed_data` (Game Bits *Embedded files*): the build generates the bytes,
  rebuilt when the `.oza` changes, and a header declaring a
  `gb::EmbeddedData`, which `Ozzy` reads as text. The `.oza` gets
  `eol=lf` in `.gitattributes`, so its bytes are the same everywhere. Nothing assembled is checked in, as
  ozzy_design.md asks, and the `.oza` stays a file anyone can assemble with
  `oz3asm` for a listing.
- An assembly error in the boot ROM is a bug, not a setting: `Create`
  `CHECK`s it, and the tests catch it first.
- The table's interrupt cores are only the cores the Ozzy has, so `"all"`
  (and the screen's default, every core) is written as the Ozzy's own.
- Devices are in the table in a fixed order: the screen, the keyboard, then
  the teletype.
- `Run` splits `time` into whole seconds and the nanoseconds left, and keeps
  the nanoseconds-times-clock remainder from the last call, so nothing
  overflows however long a call is.

**Brittleness:** the boot ROM source and `machine_table.h` both know the
table's layout, which a test that boots machines of several shapes checks from
both sides.

### ozzy: the console frontend

```
// console_text.h
// Returns the text that draws `picture` from the console's top left, with
// virtual terminal sequences, redrawing only the rows that differ from `last`
// (all of them if it is null or a different size).
std::string DrawConsoleText(const ScreenPicture& picture,
                            const ScreenPicture* last);

// Returns the UTF-8 text closest to the built-in character set's character.
std::string_view GetCharacterText(uint8_t character);

// console.h
class Console {
 public:
  // Takes over the console (virtual terminal output, UTF-8, the alternate
  // screen buffer, and raw key input), or returns an error. It is given back
  // as it was when the Console is destroyed.
  static absl::StatusOr<std::unique_ptr<Console>> Create();
  virtual ~Console() = default;

  // Writes text, which may hold virtual terminal sequences.
  virtual void Write(std::string_view text) = 0;

  // Returns the key events pressed since the last call, in Ozzy's codes.
  virtual std::vector<uint16_t> TakeKeys() = 0;

  // Returns whether the user asked to quit (Ctrl+Break, or closing the window).
  virtual bool IsQuitRequested() const = 0;
};

// win_console.h: no Windows headers, so it can be tested.
// Returns the Ozzy key event for a Windows key press, or 0 for none.
uint16_t GetOzzyKey(uint16_t virtual_key, char32_t character,
                    uint32_t control_keys);
```

- `win_console.cc` is the only Windows implementation, picked by the build as
  the C++ style asks, and includes `<windows.h>` last. Only Windows builds, so
  there is no `gen_` one until another platform does.
- `GetOzzyKey` is all of the key translation, so `win_console_test.cc` tests it
  with the Windows virtual key constants. Reading and writing the console
  itself is tested by hand.

**Brittleness:** the console must be given back however the app exits, which
`Console`'s destructor does, and its handler for Ctrl+Break and closing only
sets a flag the app's loop checks.

### apps: ozzy

- `oz3/apps/ozzy.cc`, linking `oz3_ozzy`, `gb_cli` for its flags, and
  `gb_file` to read the config.
- The loop: take keys and press them, run the cycles for the time since the
  last frame (absl's clock, capped at 100 ms), draw the picture if it changed
  (or write the teletype's output), and sleep until the next frame.
- Performance: the default Ozzy at 1 MHz is 16,667 cycles a frame, which the
  emulator runs in well under a millisecond in Release. Drawing an 80 by 24
  picture's text is a few kilobytes. To be measured in CL9.

### To confirm

- **Quitting** (CL8): Ctrl+Break is missing on many laptop keyboards (some
  have it as Ctrl+Fn+B or Ctrl+Fn+Pause). The alternative is a key Ozzy never
  needs, such as Ctrl+Alt+End. Recommendation: Ctrl+Break and closing the
  window, adding another key only if Ctrl+Break proves awkward by hand.
- **Overline** (CL7): Windows Terminal draws `ESC[53m`, but the old console
  host may not. To check by hand.
- **Sextants** (CL7): Unicode's sextants (U+1FB00 on) need a font that has
  them, as Cascadia Mono does. To check by hand, falling back to the nearest
  block character otherwise.

## CLs

### CL1 [x] wiki: Ozzy's devices page

Depends on: nothing.

- `3.1-Ozzy-Devices.md`: the screen, keyboard, and teletype, as a programmer
  sees them, from console-devices.md's Behavior: ports, interrupts, the
  screen's addresses and control words, cells and attributes, the cursor,
  clearing rows, the character set and its window, compact rows, the built-in
  character set, refreshes and blinking, key codes, and teletype timing. Not
  the host APIs.
- Home: the Devices entry links the page.
- Here: the backlog item comes out, this plan goes in, and a new backlog item,
  *Graphical frontend*, takes what the console can't show (an Ozzy window
  drawing the picture's pixels, through Game Bits' SDL and renderer).

**Verify**
- The user reviews the page against console-devices.md and the code.
- Links resolve on GitHub's naming (lower case, no extension).

### CL2 [x] ozzy: the library, and MachineSettings

Depends on: nothing.

- `oz3/ozzy/CMakeLists.txt`, and `oz3/CMakeLists.txt` adds it after
  `devices`. CLAUDE.md's directory structure gains the library.
- `machine_settings.h/.cc`: `MachineSettings`, the Ozzy defaults for each
  device, and `ReadMachineSettings`.
- Unused, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: an empty config is the default Ozzy; each key and each device
  type read; a device's defaults fill what it leaves out; RAM masks in hex;
  `interrupt_cores` as one core, a list, and `"all"`; `devices: []`; and an
  error naming the place for an unknown key, a wrong type, each range, a core
  the Ozzy doesn't have, a second device of a type, a device's page that is
  RAM, and bank 0's page 0 as RAM.

### CL3 [x] ozzy: Ozzy, without the boot ROM

Depends on: CL2.

- CL2's `MachineSettings` becomes `OzzySettings` (`ozzy_settings.h/.cc`),
  with `ReadOzzySettings` (see Names).
- `ValidateOzzySettings`, taking Ozzy's rules out of the reader so
  `Ozzy::Create` shares them.
- `machine_table.h`, and `ozzy.h/.cc`: `Create` builds the processor and
  devices and writes the machine table, but starts no core; `Run`.

**Verify**
- Standard checks.
- Unit tests: the processor's banks (reads and writes on RAM, ROM, and empty
  pages), cores, ports, and clock; each device found, or null; the machine
  table's words for the default Ozzy and one with every device and several
  banks; errors from a device's `Create` and from `Validate` naming the
  device; `Run`'s cycles for many short calls adding up to the clock rate.

### CL4 [x] wiki: Ozzy's machine page

Depends on: CL3.

- `3-Ozzy-Computer.md`: what Ozzy is, the config file, memory, and the
  machine table. Home links it.
- The machine table gives every bank and device entry a fixed place
  (`kMachineTableBanks` and `kMachineTableDevices`), rather than packing the
  devices after the Ozzy's banks, so programs find them at constant
  addresses.

**Verify**
- The user reviews the page with CL2 and CL3's behavior.
- Standard checks, and unit tests: the entries' offsets pinned, the unused
  bank entries 0, and the devices in place for an Ozzy with all 16 banks.

### CL5 [ ] ozzy: the boot ROM's banner

Depends on: CL3, and Game Bits *Embedded files*.

- `boot_rom.oza`, embedded in the library, and `Ozzy::Create` loads it and
  starts core 0. It reads the table, clears the screen, and prints the banner
  with its numbers, to the screen or the teletype, then halts.
- The wiki's Ozzy page gains The boot ROM (the banner).

**Verify**
- Standard checks.
- Unit tests: the banner's cells (and cursor) for a few machine shapes,
  including the decimal numbers at their edges (1 core, 8 cores, a single page
  of RAM, and all 16 banks of it, 1024 Kiw); the banner on the teletype for an Ozzy without a
  screen; core 0 halted afterward, with nothing printed, for an Ozzy with
  neither; the source's table address against `kMachineTableAddress`.
- `oz3asm` assembles `boot_rom.oza` on its own.

### CL6 [ ] ozzy: the boot ROM echoes keys

Depends on: CL5.

- The keyboard handler in `boot_rom.oza`: echo, Enter, Backspace, wrapping,
  and scrolling, on the screen and the teletype.
- The wiki's Ozzy page describes the echo.

**Verify**
- Standard checks.
- Unit tests: keys pressed between runs appear on the next picture; Enter,
  Backspace (including at the start of a line), wrapping at the right edge,
  scrolling past the bottom row (the scroll row moves and the new bottom row is
  clear), ignored keys, and 16 keys at once; the same on the teletype.

### CL7 [ ] ozzy: console text

Depends on: CL2.

- `console_text.h/.cc`: `DrawConsoleText` and `GetCharacterText`.

**Verify**
- Standard checks.
- Unit tests: every one of the 256 characters has text; each attribute's
  sequence, and runs of cells sharing attributes written once; only changed
  rows redrawn; a different size redrawing all; the cursor's position, shape,
  blinking, and hiding when off.

### CL8 [ ] ozzy: the Windows console

Depends on: CL2.

- `console.h`, `win_console.h/.cc`: `Console` and `GetOzzyKey`.

**Verify**
- Standard checks.
- Unit tests (`win_console_test.cc`): `GetOzzyKey` for characters, Enter, Tab,
  Backspace, Escape, Delete, Ctrl with letters, the arrows, Home to Page Down,
  Insert, F1 to F12, each modifier, and keys that are none (Shift alone).

### CL9 [ ] apps: ozzy

Depends on: CL6, CL7, CL8.

- `oz3/apps/ozzy.cc`, and the `ozzy` executable in `oz3/apps/CMakeLists.txt`.
- The wiki's Ozzy page gains Running Ozzy, and Home's Ozzy entry is no longer
  planned. README's mention of `ozzy` matches.

**Verify**
- Standard checks.
- By hand, in Windows Terminal: the default Ozzy boots to its banner and
  echoes typing, Enter, Backspace, and scrolling; attributes and the cursor
  look right (with a test program written to the screen); a teletype-only
  config; a config with an error; `--max_speed`; Ctrl+Break and closing the
  window restore the console.
- Performance: the frame time of the default Ozzy in Release, and the cycles
  a second with `--max_speed`.
