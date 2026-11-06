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
  { type: "screen", columns: 80, rows: 24 },
  { type: "keyboard" }
]
```

As in JSON, entries are separated by commas, including the file's own, and
there is no comma after the last one.

- **Banks** are 1 to 16, and each gives its RAM pages as a mask, bit `n` for
  page `n`, as `MemoryPageMasks`, the machine table, and the assembler's
  `.BANK` masks all state pages. Game Bits' text config reads `0x` hex, so
  a mask reads as its pages. Page 0 of bank 0 is always the boot ROM, so bank
  0's mask can't have bit 0. The default is one bank, `0x7FFE`.
- **Devices** are a list, each with a `type`, and the settings of its own
  `<Device>Settings` (see console-devices.md), in snake case as there, apart
  from its wiring. Any setting left out takes the device's default, so
  `{ type: "keyboard" }` is the keyboard. Without `devices`, the Ozzy has a
  screen and a keyboard. `devices: []` is an Ozzy with none.
- **The wiring is fixed:** each device's port, interrupt, interrupt cores, and
  pages are the Ozzy's, as on real hardware, so programs (the boot ROM, the
  OS) know them as constants. Which devices an Ozzy has is what varies. The
  devices library still takes any wiring, for other hosts.
- An Ozzy has at most one of each console device, as the console frontend
  shows one screen and one teletype, and types on one keyboard. Their names in
  the `Processor` are `Screen`, `Keyboard`, and `Teletype`.
- With a screen, bank 0's page 15 must not be RAM, so the machine table says
  plainly what each page is.

| Type       | Wiring                                    | Settings, with their defaults                          |
| ---------- | ----------------------------------------- | ------------------------------------------------------ |
| `screen`   | page 15 of bank 0, interrupt 6 on every core | columns 80, rows 24, and the screen's own rates and characters |
| `keyboard` | port 1, interrupt 1 on core 0             | none                                                   |
| `teletype` | port 2                                    | baud 0 (as fast as cores write), buffer_size 4096      |

- Errors name where they are, as gb::Config keeps no line numbers:
  `devices[0].port: unknown key`. An unknown key, a value of the wrong type,
  and a value out of range are all errors, so a typo never goes unnoticed.

### Memory

| Bank 0 pages | What                                                         |
| ------------ | ------------------------------------------------------------ |
| 0            | The boot ROM, with the machine table at 0F00h                |
| 1-14         | RAM (by default). The boot ROM's stack is in the first page. |
| 15           | The screen, if the Ozzy has one                              |

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
next ones when they land. A device's wiring is fixed, so its entry always
holds the same port, interrupt, and pages, but the table still gives them, so
it describes the hardware whole.

### The boot ROM

At power on, core 0 starts at IP 0 of the boot ROM. It reads the machine
table, and:
- **With a screen**, it clears the screen (filled with spaces), prints the
  banner at the top, then a blank line, and shows a slow blinking underline
  cursor at the start of the line after. The banner wraps at the right edge,
  and new lines below the bottom row scroll, so a small screen shows what fits.
- **Without one, with a teletype**, it prints the banner to the teletype, then
  CR LF twice.
- Then it enables interrupts and halts, so it runs only when an interrupt
  comes (a halted core still handles interrupts).
- **With a keyboard**, it sets a handler for the keyboard's interrupt, which
  is always interrupt 1 on core 0, and reads port 1. Each key echoes, by its
  low byte (modifiers make no difference): characters 20h to 7Eh print at the
  cursor, Enter starts a new line, and Backspace erases the character before
  the cursor, doing nothing at the start of a row. Other keys are ignored. On
  the screen, a new line past the bottom scrolls with the scroll row and clear
  row, and after a full row the cursor stays on its last column until the
  next character wraps. On the teletype, Enter is CR LF, and Backspace is BS,
  space, BS.
- Without a keyboard, or with neither a screen nor a teletype, nothing
  echoes, and other devices' interrupts have no handler, so they are dropped.

The banner is `OZZY` and the boot ROM's version, then the clock rate, cores,
and RAM from the machine table, such as `OZZY 0.1  1 MHz  4 cores  120K RAM`
(`1 core` for one). RAM is in K, as old machines wrote it, which on an Ozzy is
1024 words (4K a page). The clock rate is in the largest of Hz, kHz, and MHz
it is a whole number of (`4770 kHz`, `32768 Hz`). Printing the numbers is the
boot ROM's own decimal printing of a dword (with `DIV.DW` and `MOD.DW`), the
first real OZ-3 code Ozzy runs.

The boot ROM's stack is in the first page of RAM of the lowest bank that has
any, which `ProgramLoader` picks, so an Ozzy must have some RAM. Its variables
are the first words it pushes, addressed as `S(-1)` down, as a label in the
stack segment can't be (see the backlog's *Labels in address forms*).

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
// The Ozzy's wiring, the same in every Ozzy.
inline constexpr int kOzzyScreenBank = 0;
inline constexpr int kOzzyScreenPage = 15;
inline constexpr int kOzzyScreenInterrupt = 6;
inline constexpr uint8_t kOzzyScreenInterruptCores = kAllCores;
inline constexpr int kOzzyKeyboardPort = 1;
inline constexpr int kOzzyKeyboardInterrupt = 1;
inline constexpr uint8_t kOzzyKeyboardInterruptCores = 0x01;  // Core 0.
inline constexpr int kOzzyTeletypePort = 2;

// The settings of each console device that an Ozzy leaves to the host: all
// but its wiring.
struct OzzyScreenSettings {
  int columns = ScreenSettings().columns;  // And so on, for each setting.
  int rows;
  int refresh_hz;
  int slow_blink_hz;
  int fast_blink_hz;
  std::optional<std::vector<uint16_t>> characters;
};
struct OzzyTeletypeSettings {
  int baud;
  int buffer_size;
};

// Returns the device's settings for an Ozzy's device: its own, and the wiring.
ScreenSettings GetOzzyScreenSettings(const OzzyScreenSettings& screen);
KeyboardSettings GetOzzyKeyboardSettings();
TeletypeSettings GetOzzyTeletypeSettings(const OzzyTeletypeSettings& teletype);

struct OzzySettings {
  static constexpr int64_t kMaxClockHz = 100'000'000;
  static constexpr int kMinPorts = 16;

  int64_t clock_hz = ProcessorConfig::kDefaultClockHz;
  int cores = 1;
  int ports = 32;
  std::vector<uint16_t> banks = {0x7FFE};  // The RAM pages of each bank.
  std::optional<OzzyScreenSettings> screen = OzzyScreenSettings();
  bool keyboard = true;
  std::optional<OzzyTeletypeSettings> teletype;
};

// Returns an error naming the first setting that breaks a rule of
// OzzySettings, such as `screen: page 3 of bank 0 is RAM`.
absl::Status ValidateOzzySettings(const OzzySettings& settings);

// Reads settings from a config file's text, returning an error naming where
// the first problem is.
absl::StatusOr<OzzySettings> ReadOzzySettings(std::string text);
```

- `ValidateOzzySettings` holds Ozzy's own rules (the ranges, bank 0's
  boot ROM page, some RAM, and the screen's page not RAM), so
  `ReadOzzySettings` and `Ozzy::Create` share them, and a host that builds
  settings in code gets the same checks. Its errors name the setting
  (`banks[0]`), not the text.
- Reading checks what only the text knows: keys, types, values that fit their
  setting, and one device of each type. What the devices and the processor
  check themselves (a screen's size and rates) is left to `Ozzy::Create`, so
  no rule is written twice.
- `OzzySettings` holds the Ozzy's own device settings, rather than the
  devices', so a host can't set the wiring either, and the wiring is written
  in one place. A keyboard has nothing to set, so it is only there or not.
  `OzzyScreenSettings` and `OzzyTeletypeSettings` take their defaults from
  `ScreenSettings` and `TeletypeSettings`.
- The wiring fits every Ozzy (the screen's page isn't the boot ROM's, the
  ports are below `kMinPorts`, and the interrupts are ones every core has),
  which `static_assert`s check, so no settings can make it invalid.
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
- The table's interrupt cores are only the cores the Ozzy has, so the
  screen's, every core, is written as the Ozzy's own.
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
  picture's text is a few kilobytes. To be measured in CL10.

### To confirm

- **Quitting** (CL9): Ctrl+Break is missing on many laptop keyboards (some
  have it as Ctrl+Fn+B or Ctrl+Fn+Pause). The alternative is a key Ozzy never
  needs, such as Ctrl+Alt+End. Recommendation: Ctrl+Break and closing the
  window, adding another key only if Ctrl+Break proves awkward by hand.
- **Overline** (CL8): Windows Terminal draws `ESC[53m`, but the old console
  host may not. To check by hand.
- **Sextants** (CL8): Unicode's sextants (U+1FB00 on) need a font that has
  them, as Cascadia Mono does. To check by hand, falling back to the nearest
  block character otherwise.
- **Character widths** (CL8): several characters (such as ○, ■, ♠, ½, and
  the shades) are East Asian "ambiguous" width, which a console may draw two
  cells wide, shifting the rest of the row. Windows Terminal draws them one
  cell wide by default. To check by hand; each row starts at its own
  position, so a wide character never spills into the next row.

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

### CL5 [x] ozzy: the boot ROM's banner

Depends on: CL3, and Game Bits *Embedded files*.

- `boot_rom.oza`, embedded in the library (as `boot_rom_source.h`), which
  `GetBootRom` (`boot_rom.h/.cc`) assembles once, and `Ozzy::Create` loads
  and starts core 0 on. It reads the table, clears the screen, and prints the
  banner with its numbers, to the screen or the teletype, then halts.
- Printing a character past the right edge wraps it (as a terminal does, when
  the next character comes, so a full line has no blank row after it), and a
  new line below the bottom row scrolls, as the banner needs them on a small
  screen, so CL7 only adds the keys.
- `ValidateOzzySettings` requires some RAM, for the boot ROM's stack.
- `oz3_ozzy` links `oz3_tools`, to assemble and load the boot ROM.
- The wiki's Ozzy page gains The boot ROM (the banner).
- The backlog gains *Labels in address forms*.

**Verify**
- Standard checks.
- Unit tests: the banner's cells (and cursor) for a few machine shapes,
  including the decimal numbers at their edges (1 core, 2 and 8 cores, a
  single page of RAM, and the most RAM with a screen, 1016K); clock rates
  in kHz and Hz, 100 MHz, and 99999999 Hz; the screen in another bank; a 10 by
  3 screen that wraps and scrolls, and one as wide as the banner; the stack's
  page; the banner on the teletype for an Ozzy without a screen, as fast as it
  prints and at 9600 baud; core 0 halted afterward for an Ozzy with neither;
  the source's machine table constants against `machine_table.h`.
- `oz3asm` assembles `boot_rom.oza` on its own.

### CL6 [x] ozzy: the Ozzy's fixed wiring

Depends on: CL5.

- Each device's port, interrupt, interrupt cores, and pages are fixed (see The
  config file), as constants in `ozzy_settings.h`. `OzzySettings` holds
  `OzzyScreenSettings` and `OzzyTeletypeSettings`, without the wiring, and
  `keyboard` is a bool. `GetOzzy<Device>Settings` add the wiring.
- The config file loses `bank`, `page`, `interrupt`, `interrupt_cores`, and
  `port`, so they are unknown keys, and `ValidateOzzySettings` checks only
  that page 15 of bank 0 isn't RAM when there is a screen.
- The machine table is unchanged.
- The wiki's Ozzy Computer page: the config file's device keys, the errors,
  and memory. The Ozzy Devices page gives each device's fixed wiring, which
  it now leaves to the machine.

**Verify**
- Standard checks.
- Unit tests: each device's settings with the wiring; the Ozzy's device
  settings' defaults match the devices'; the removed keys are unknown; the
  screen's page as RAM; the machine table for every device; the boot ROM's
  banner unchanged.

### CL7 [x] ozzy: the boot ROM echoes keys

Depends on: CL6.

- The keyboard handler in `boot_rom.oza`: echo, Enter, and Backspace, on the
  screen and the teletype. The boot ROM finds whether there is a keyboard in
  the machine table, and sets the handler with `SETI` on the keyboard's fixed
  interrupt, reading its fixed port, as constants in its source. It halts with
  interrupts enabled.
- After a full row, the cursor shows on the row's last column, rather than
  past the screen's edge.
- The wiki's Ozzy page describes the echo.

**Verify**
- Standard checks.
- Unit tests: keys pressed between runs appear on the next picture, and keys
  pressed before the banner after it; every character 20h to 7Eh, 16 (as
  many as the keyboard holds) at once; Enter;
  Backspace (including at the start of a row, and in a full row); wrapping
  at the right edge and scrolling past the bottom row (the new bottom row is
  clear); ignored keys and modifiers; the same on the teletype, as fast as it prints and at 9600 baud; the source's keyboard
  wiring and key constants against `ozzy_settings.h` and `keyboard.h`.

### CL8 [x] ozzy: console text

Depends on: CL2, and Game Bits *Code point conversions in gb_base*.

- `console_text.h/.cc`: `DrawConsoleText` and `GetCharacterText`.
- Each character's nearest Unicode character is a code point in a table, and
  the sextants are computed (Unicode's leave out the blank, the two halves,
  and full, which are a space, ▌, ▐, and █). Of 00h to 1Fh, 13h and 1Dh are
  ¼ and ½, and the rest are blocks, quadrants, triangles, heavy lines, and a
  dot. The source holds code points rather than the characters, as OZ-3
  compiles without `/utf-8`, and Game Bits' `gb::AppendUtf8` (Game Bits *Code
  point conversions in gb_base*) encodes them.
- Drawing every row clears the console first. The cursor is hidden while
  rows are drawn, and the style is the default between rows, so each call's
  text stands on its own. The inverse cursor is a block, and both blink rates
  are the console's own blink.

**Verify**
- Standard checks.
- Unit tests: every one of the 256 characters is one printable character;
  ASCII as itself, and the graphics, sextants, and symbols at their edges;
  each attribute's sequence, those text can't show left out, and runs of
  cells sharing attributes written once; only changed rows redrawn, and
  nothing for the same picture; a different size redrawing all; the cursor's
  position, every shape and blinking, hiding when off, changing alone, and put back after drawing rows.

### CL9 [ ] ozzy: the Windows console

Depends on: CL2.

- `console.h`, `win_console.h/.cc`: `Console` and `GetOzzyKey`.

**Verify**
- Standard checks.
- Unit tests (`win_console_test.cc`): `GetOzzyKey` for characters, Enter, Tab,
  Backspace, Escape, Delete, Ctrl with letters, the arrows, Home to Page Down,
  Insert, F1 to F12, each modifier, and keys that are none (Shift alone).

### CL10 [ ] apps: ozzy

Depends on: CL7, CL8, CL9.

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
