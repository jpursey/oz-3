# Console Devices

The first real devices, in the `devices` library: the **screen**, the
**keyboard**, and the **teletype** that Ozzy's console is made of (see
[ozzy_design.md](../ozzy_design.md)). Each is a `Device` and its
`DeviceFactory`, built on what *Devices in core* added (see
[devices-in-core.md](devices-in-core.md)), so a host adds them to a
`ProcessorConfig` and finds them by name:

```
absl::StatusOr<std::unique_ptr<ScreenFactory>> screen_factory =
    ScreenFactory::Create("Screen", {.bank = 0,
                                     .page = 15,
                                     .interrupt = {.interrupt = 6},
                                     .columns = 80,
                                     .rows = 24});
if (!screen_factory.ok()) { ... }

ProcessorConfig config = ProcessorConfig::OneCore(instructions);
config.SetClockHz(1'000'000)
    .SetPortCount(16)
    .AddDevice(std::make_shared<KeyboardFactory>(
        "Keyboard",
        KeyboardSettings{.port = 1, .interrupt = {.interrupt = 1}}))
    .AddDevice(*std::move(screen_factory));
if (absl::Status status = config.Validate(); !status.ok()) { ... }
Processor processor(config);

// The host named each device, so it knows each one's type.
auto* keyboard = static_cast<Keyboard*>(processor.GetDevice("Keyboard"));
auto* screen = static_cast<Screen*>(processor.GetDevice("Screen"));
keyboard->PressKey('A');
processor.Execute(33'334);  // One refresh, at the default 30 Hz.
const ScreenPicture& picture = screen->GetPicture();
```

The devices are host-agnostic: each holds its state (the screen's picture, the
key queue, the text written) and leaves presenting it to the host. The screen
draws its own pixels, so every host shows exactly the same thing. Ozzy's
console frontend, which shows the picture and reads the host's keyboard, is the
*Ozzy machine* item.

A device is configured in real time (a teletype's baud rate, a screen's
refresh rate), which it turns into cycles with the processor's **clock rate**,
in `ProcessorConfig`. So the same device settings work at any clock rate.

The devices themselves aren't in the specification: they are Ozzy's, and
Ozzy's wiki page, which documents them for programmers, is written by *Ozzy
machine*. The clock rate is, in a line of `2-Specifications.md`'s overview.

## Behavior

### Clock rate

- `ProcessorConfig::SetClockHz()` sets the processor's clock rate, in cycles
  per second, 1 MHz by default (`ProcessorConfig::kDefaultClockHz`).
  `Validate()` returns an error for a rate below 1 Hz.
- `Processor::GetClockHz()` returns it, as does a protected
  `Device::GetClockHz()`, for a device to turn its rates into cycles.
- Nothing else changes: everything still runs in cycles, and the clock rate
  only says how many make a second. A host paces the processor to real time
  with it (Ozzy's frontend), or ignores it.
- `BaseCoreTest::InitConfig` takes a `clock_hz`.
- Wiki: `2-Specifications.md`'s overview has a **Clock** bullet.

### Teletype

A port that writes text to the host, for an Ozzy without a screen.
- Each word a core writes to the port is two packed characters, the high byte
  first, and a zero byte is skipped. So a single character is written as itself
  (`'A'` is 0041h), and a packed string goes straight from memory with `OUTR`.
- A dword write is two words, word 0 first, so `OUTR.ID` writes a packed
  string from memory in half the executions of `OUTR.IW` (11 cycles a dword,
  against 9 a word). The teletype clears word 1 once it takes it, so a program
  that only writes words never needs dwords, and a word written after a dword
  isn't followed by the dword's characters again.
- The teletype takes what is written as soon as the port's status is set (by
  `OUTS`, `OUT`, or `OUTR`), and clears the status, so the next `OUTS`
  succeeds. It takes it the cycle after the store, so an `OUTR` of a packed
  string runs to the end without stopping: an `OUTR.IW` of 3 words takes 27
  cycles, as for a port that is always ready, and an `OUTR.ID` of 3 dwords 33.
- At a **baud rate** (0, the default, is as fast as cores write), it is busy
  printing what it took for 10 bits a character (a start bit, 8 data bits, and
  a stop bit), and takes nothing more until it is done. The port holds one more
  write meanwhile, as a serial line's holding register did, and then its
  status stays set: `OUTS` fails, and `OUTR` stops early. A program paces
  itself to the baud rate, or loops on `OUTS`. There is no interrupt for
  "ready": the baud rate is fixed for a machine, so a program can time itself.
  At 1 MHz, 9600 baud is 1041 cycles a character (rounded down for each take),
  and 300 baud 33,333.
- Characters are 7-bit ASCII: the eighth bit of each byte is dropped, as it was
  a teletype's parity bit, and a byte that is then 0 is skipped, taking no
  time. The rest go to the host as they are, control characters included. The
  host does what a terminal would with carriage return, line feed, backspace,
  and bell.
- The teletype keeps what has been written until the host takes it with
  `Teletype::TakeOutput()`, up to a buffer size (4096 characters by default).
  Characters reach it when they are taken, not as each one finishes printing,
  which no program can tell apart. Without room for a dword's characters (4),
  it takes nothing, so the port's status stays set, `OUTS` fails, and `OUTR`
  stops early, until the host takes the text.
- Reading the port is not the teletype's business: a core that reads it gets
  whatever is there, and clears the status, which loses a word not yet taken.

### Keyboard

A port, holding the oldest key event, with an interrupt when one arrives.
- The host presses a key with `Keyboard::PressKey(key)`, which queues it. The
  queue holds 16 key events waiting for the port, and a full queue drops new
  keys (`PressKey` returns false), as old keyboards did.
- Whenever the port's status is clear and a key is waiting, the keyboard writes
  the key to port word 0, sets the status, and raises its interrupt. So each
  key raises the interrupt once, when it becomes readable, and a handler that
  reads one key with `INS` is interrupted again for the next.
- A core reads the key with `INS` (or `IN`), which clears the status, and the
  keyboard puts the next key there the cycle after.
- A key event is a word: the low byte is the key, and the high byte the
  modifier keys.

| Key                             | Code                     |
| ------------------------------- | ------------------------ |
| Characters                      | ASCII, 20h to 7Eh        |
| Enter, Tab, Backspace, Escape   | 0Dh, 09h, 08h, 1Bh       |
| `Ctrl` with `A` to `Z`          | 01h to 1Ah               |
| Delete                          | 7Fh                      |
| Up, Down, Left, Right           | 80h, 81h, 82h, 83h       |
| Home, End, Page Up, Page Down   | 84h, 85h, 86h, 87h       |
| Insert                          | 88h                      |
| F1 to F12                       | 90h to 9Bh               |

| Modifier | Bit (in the word) |
| -------- | ----------------- |
| Shift    | 0100h             |
| Ctrl     | 0200h             |
| Alt      | 0400h             |

The keyboard doesn't interpret key events: the host makes them, and these are
constants in `keyboard.h` for it to use. A core that writes to the port sets
its status, which the keyboard takes as a key not yet read, so it waits until
the status is cleared. That is a quirk to keep.

### Screen

A memory mapped page of character cells and control words, which refreshes at
a configured rate, and draws a picture of pixels from its own character set.

| Address        | Word                                                          |
| -------------- | ------------------------------------------------------------- |
| 0 to cells - 1 | The cells, row by row                                         |
| up to FEFh     | Unused: plain memory a program may use                        |
| FF0h           | Columns (read only)                                           |
| FF1h           | Rows (read only)                                              |
| FF2h           | Frame counter (read only): refreshes since power on, wrapping |
| FF3h           | Scroll row: the row of cells shown at the top of the screen   |
| FF4h           | Cursor column                                                 |
| FF5h           | Cursor row                                                    |
| FF6h           | Cursor: its mode and style                                    |
| FF7h           | Fill: the cell a row is cleared to                            |
| FF8h           | Clear row: clears a row as it is written                      |
| FF9h           | Character set window: bit 0                                   |
| FFAh           | First compact row                                             |
| FFBh           | Compact rows: how many                                        |
| FFCh to FFFh   | Reserved: reads 0, and writes are ignored                     |

- Addresses are the screen's own, from the start of its page, wherever it is
  placed.
- **Cells:** one word per cell. The low byte is the character, and the high
  byte its attributes. The built-in character set draws characters 0 to 127 as
  ASCII, and 128 to 255 as Ozzy's graphics characters.

| Bit   | Attribute                                                       |
| ----- | --------------------------------------------------------------- |
| 0100h | Inverse                                                         |
| 0200h | Half intensity                                                  |
| 0400h | Blink                                                           |
| 0800h | Alternate character set: the character is one of a second 256   |
| 1000h | Line on the top edge                                            |
| 2000h | Line on the bottom edge (an underline)                          |
| 4000h | Line on the left edge                                           |
| 8000h | Line on the right edge                                          |

- **Lines** light every pixel on one edge of the cell, for drawing tables and
  boxes, not only underlines. Two neighboring cells' lines on their shared edge
  make a line twice as thick, which a program can use or avoid. As pixels are
  twice as tall as they are wide, a top or bottom line is twice as thick as a
  left or right one.
- **Control words** are at the end of the page, so a program finds them at the
  same address whatever the screen's size. That leaves room for 4080 cells (up
  to 80 by 51, or 132 by 30).
- **The size** is at most 132 columns (as wide as a wide terminal) and 64 rows
  (as tall as the cells allow at 63 columns), as well as 4080 cells, so a
  screen is never a long thin strip.
- **The scroll row** is taken modulo the rows, so the row shown at the top is
  the cells' row `scroll`, and the one below it `scroll + 1`, wrapping at the
  last row. Scrolling a whole screen is one write.
- **The cursor** is at its column and row in screen positions, as shown (after
  the scroll), and a cursor outside the screen is off. The cursor word is its
  mode in bits 0 and 1, ORed with its style in bits 2 and 3, and its other bits
  are reserved:

| Mode            | Value | Style           | Value | Draws                         |
| --------------- | ----- | --------------- | ----- | ----------------------------- |
| Off             | 0     | Block           | 0     | Every pixel of the cell lit   |
| On              | 1     | Inverse         | 4     | The cell's pixels inverted    |
| Slow blink      | 2     | Underline       | 8     | The bottom row of pixels lit  |
| Fast blink      | 3     | Left edge       | 0Ch   | The leftmost column lit       |

- At power on, the page is all 0 apart from the read-only words: a blank
  screen, with the cursor off at the top left, no compact rows, and the
  character set window closed.

The screen **refreshes** at `refresh_hz` (30 by default, the full frames a
second of an interlaced NTSC display): refresh `n` is at cycle
`n * clock_hz / refresh_hz`, so the schedule doesn't drift where a refresh
isn't a whole number of cycles (33,333⅓ at 30 Hz and 1 MHz).
- It takes its bank's lock, adds one to the frame counter, takes the page as
  the picture and draws its pixels, and releases the lock, all in one cycle,
  with no cycles of its own. So it never sees half of a core's write, and waits
  only while a core holds the bank. A refresh that waited is granted the bank
  as the core releases it, so a core asking for it again in that cycle (as the
  next instruction's fetch does) waits a cycle for the refresh: the lock
  handoff's cost, which *Order-independent lock handoff* in the backlog
  removes.
- Then it raises its interrupt (like a vertical blank interrupt).
- Refreshes are on a fixed schedule: one that waited for the lock doesn't delay
  the next, and refreshes it missed run a cycle apart until it is back on the
  schedule.

The host reads the picture with `Screen::GetPicture()`: the cells as shown
(with the scroll applied, row by row from the top), the cursor's position,
mode, and style, and the pixels, which are what the host shows (see Pixels). It
changes only at a refresh, so the host shows it until the next one. A picture
compares equal to another with the same contents, so a host can skip drawing
one that didn't change.

### Clearing rows

- **Fill** (FF7h) is the cell a cleared row is filled with, such as a space
  with attributes. It is 0 at power on.
- Writing a row to **clear row** (FF8h) fills that row with the fill word as
  the write happens: one word written clears a whole row, a hardware assist.
  The row is in screen positions, as shown (after the scroll), and a row
  outside the screen clears nothing. Clear row reads 0.
- So a terminal's line feed on the bottom row is two writes: add one to the
  scroll row, then clear row `rows - 1`. Clearing the screen is a write for
  each row.

### Character set

The screen draws each character from its own **character set**, which a
program can redefine, as on the Osborne 1. So a program can make its own
graphics, and animate them.
- There are 512 **glyphs**: the first 256 are for cells without the alternate
  bit, and the second 256 for cells with it.
- A glyph is 8 pixels wide and 10 tall, and its pixels are twice as tall as
  they are wide, so a cell is 8 by 20 square units, and an 80 by 24 screen is
  640 by 480.
- A glyph is 10 rows of one byte each, with bit 7 the leftmost pixel, packed
  two rows to a word, the high byte first (as the teletype packs characters):
  5 words a glyph. Glyph `n` (0 to 511) is at `n * 5`, so the whole character
  set is 2560 words, from 0 to 9FFh.
- **The character set window** (FF9h): while its bit 0 is set, writes to the
  screen's addresses 0 to 9FFh go to the character set rather than the cells.
  Reads still read the cells, so the character set can't be read back: it is
  write only, as the Osborne 1's was. Addresses from A00h are as they always
  are.
- A changed glyph shows at the next refresh, like everything else, so a program
  can change glyphs between refreshes to animate them.
- At power on, the character set is the built-in one (see below), which a host
  can replace with its own in the settings.

### Pixels

The screen draws the picture's **pixels** itself, so every host shows exactly
the same thing, and only scales the pixels (each twice as tall as it is wide)
and chooses their colors.
- The pixels are `columns * 8` wide and `rows * 10` tall, one byte each, row
  by row from the top: 0 is off, 1 dim, and 2 bright.
- Each cell is drawn from its glyph: its glyph's pixels are lit, bright, or dim
  with half intensity, and the rest are off.
- **Inverse** swaps the cell's lit and unlit pixels.
- **Lines** light their edge's pixels after inverse, so a line always shows.
- **Blink**, in its off phase, draws the whole cell off. So an inverse,
  blinking cell is a flashing block.
- **The cursor** is drawn on its cell after the cell, at the cell's level: a
  block lights every pixel, inverse swaps the lit and unlit pixels, an
  underline lights the bottom row, and a left edge the leftmost column. A
  blinking cursor isn't drawn in its off phase.
- **Blinking** is the screen's, by the frame count, so a program can time
  itself to it. There are two blink rates, set in the settings like the
  refresh rate: the slow blink (the blink attribute, and the slow blink
  cursor), once a second by default, and the fast blink, four times a second.
  A blink at `blink_hz` is on for the first half of each blink, and off for
  the second: on while `frames * 2 * blink_hz / refresh_hz` is even, where
  `frames` is the frame counter as the refresh sets it. So the picture a
  program sees the frame counter for is the one drawn with it, and at 30 Hz
  the slow blink is off for frames 15 to 29, on to 44, and so on.
- A blink rate must be at least 1, and at most half the refresh rate, as a
  faster one would never be seen off. `Create` returns an error otherwise, so
  the refresh rate is in effect at least 2.

### Compact rows

A band of **compact rows** packs rows 8 pixels apart rather than 10, for games,
and other drawing tricks. Glyphs don't change: a compact row's bottom two
pixel rows overlap the next row's top two.
- The band is the screen rows from the **first compact row** (FFAh), as many as
  **compact rows** (FFBh) says, in screen positions, so the band stays put as
  the cells scroll through it. Rows of the band outside the screen are
  ignored, and a count of 0 (at power on) is no band. Making every row compact
  is a band from 0 of `rows`.
- Where pixels overlap, each is the brighter of the two: two lit pixels are as
  bright as the brighter, and a lit pixel over an unlit one is as it is, as on
  a phosphor.
- A compact row on the bottom of the screen has nothing below it, so it is
  drawn whole.
- The picture's pixels stay `rows * 10` tall, with the rows packed from the
  top and the pixel rows they don't reach off. The picture also holds the
  height drawn: `rows * 10`, less 2 for each compact row but the bottom row.

### The built-in character set

The built-in character set is **Ozzy's font**, which every screen starts with
unless the host sets its own. It is in the style of the Osborne 1's: glyphs
drawn after the Osborne's character generator, not a copy of its ROM, and not
meant to match it exactly.

| Characters | Glyphs                                                        |
| ---------- | ------------------------------------------------------------- |
| 00h to 1Fh | Block and line graphics, in the Osborne's style               |
| 20h to 7Eh | ASCII, in the Osborne's style                                 |
| 7Fh        | A hatch                                                       |
| 80h to BFh | Sextants: 80h plus the sixths lit (see below)                 |
| C0h to CAh | Single lines: ─ │ ┌ ┐ └ ┘ ├ ┤ ┬ ┴ ┼                           |
| CBh to D5h | Double lines: ═ ║ ╔ ╗ ╚ ╝ ╠ ╣ ╦ ╩ ╬                           |
| D6h to D9h | Half lines: ╴ ╵ ╶ ╷                                           |
| DAh to DDh | Rounded corners: ╭ ╮ ╯ ╰                                      |
| DEh to E0h | Shades: ░ ▒ ▓                                                 |
| E1h, E2h   | Halves: ▀ ▄                                                   |
| E3h to E5h | Diagonals: ╱ ╲ ╳                                              |
| E6h to E9h | Triangles: ◢ ◣ ◤ ◥                                            |
| EAh to EDh | Arrows: ← ↑ → ↓                                               |
| EEh to F1h | Suits: ♠ ♥ ♦ ♣                                                |
| F2h to FFh | Symbols: • ● ○ ■ □ ✓ × ÷ ± ° £ ♪ ☺ ↕                          |
| 100h–1FFh  | The alternates: the same as 00h to FFh                        |

- The screen draws whatever is in a cell, so the control characters have
  glyphs like any other: the Osborne's graphics, where the Osborne had them. A
  program that prints text handles control characters before it writes cells.
- Text is drawn in the left 6 or 7 pixels of the cell, from pixel row 1, with
  descenders in rows 8 and 9, as on the Osborne.
- **Sextants** divide the cell into 2 columns of 4 pixels, and rows of 3, 4,
  and 3 pixels, so each sixth is about 4 by 7 square units: nearly square
  blocks, 160 by 72 of them on an 80 by 24 screen. Bit 0 of the character is
  the top left sixth, bit 1 the top right, and so on to bit 5, the bottom
  right, the order Unicode's sextants use. 80h is blank, and BFh full.
- **Lines** run through the middle of the cell, so they join from cell to cell:
  a single line is 2 pixels wide (columns 3 and 4) and 1 tall (row 4), and a
  double line two of them (columns 1, 2, 5, and 6, and rows 3 and 5).
- The alternates are a copy, so the alternate attribute changes nothing until a
  program redefines them. A program can take the alternates for its own
  graphics without losing a character of the font.

## Design

### Names

| Name                        | Candidates                                         | Avoids                                                        | Choice             |
| --------------------------- | -------------------------------------------------- | ------------------------------------------------------------- | ------------------ |
| What a host sets about one  | `ScreenSettings`, `ScreenOptions`, `ScreenParams`  | `DeviceConfig` (core's wiring), `CpuCore::ResetParams`        | `ScreenSettings`   |
| The processor's clock rate  | `ClockHz`, `ClockRate`, `CyclesPerSecond`          | `GetCycles` (a count), the Timer device's clock (the time)    | `SetClockHz`       |
| A teletype's speed          | `baud`, `baud_rate`, `bits_per_sec`                | `refresh_hz` (a rate of whole events)                         | `baud`             |
| What the host shows         | `ScreenPicture`, `ScreenFrame`, `ScreenImage`      | the frame counter (a count), stack frames in programs         | `ScreenPicture`    |
| The screen's page           | `ScreenMemory`, `ScreenPage`, `ScreenMap`          | `MemoryBank`, `FakeMemoryMap`                                 | `ScreenMemory`     |
| Host gives a key            | `PressKey`, `TypeKey`, `AddKey`                    | "Add" (the owner registers and keeps it)                      | `PressKey`         |
| Host takes teletype text    | `TakeOutput`, `TakeText`, `ReadOutput`             | `Port::LoadWord`, the `IN` instruction                        | `TakeOutput`       |
| The screen's glyphs         | character set, `Font`, `Glyphs`                    | "font" (a typeface, which a program doesn't redefine)         | character set      |

`DeviceFactory`'s comments already call a derived factory's own values its
"settings" ("such as a screen's size"), so `ScreenSettings` follows them.
`SetClockHz` names its unit, as the C++ style asks of a plain number, and
`baud` is a unit itself. "Picture" is the word
[ozzy_design.md](../ozzy_design.md) uses for what a refresh takes, and
`ScreenFrame` would read as one count of the frame counter. The built-in
character set is "Ozzy's font" in prose, as it is one typeface, but the API
calls it a character set, as a program can redefine it.

### core: the clock rate

```
// processor_config.h
static constexpr int64_t kDefaultClockHz = 1'000'000;
ProcessorConfig& SetClockHz(int64_t clock_hz);
int64_t GetClockHz() const;

// processor.h
int64_t GetClockHz() const;

// device.h (protected)
int64_t GetClockHz() const;
```

- A rate below 1 Hz is an error from `Validate()`, not a `DCHECK`, as a host
  sets it from data (Ozzy's `clock_hz`).
- The `Processor` copies it from its config. Nothing in core uses it.
- A device reads it whenever it turns a rate into cycles, which is cheap
  enough to do each time (a multiply and a divide), so a device caches
  nothing that could disagree with it.

**Brittleness:** none.

### devices: the library

- `oz3/devices/`, the `oz3_devices` library, which links `oz3_core` only. Its
  tests, `oz3_devices_test`, also link `oz3_core_testing`, `oz3_tools`, and
  `oz3_instruction_sets`, to run small OZ-3 programs in the default
  instruction set against the devices (see Testing).
- Each device is a `.h`/`.cc` with its settings, its factory, and the device.
- **Settings:** a device's name goes on its own, as its identity, which the
  host chooses to find it by. Everything else a host sets about it, its wiring
  (ports, interrupts, pages) and how it behaves (a baud rate, a size), is one
  `<Device>Settings` struct, which is what a config file entry parses into.
  The factory splits it: the wiring into core's `DeviceConfig`, which
  `ProcessorConfig::Validate` checks against the machine, and the rest to the
  device, which copies only what it uses. Settings are passed by const
  reference.
- **Factories:** a factory with settings that can be invalid is made only by
  its static `Create(name, settings)`, which returns an error naming the device
  and the setting, or the factory as a `std::unique_ptr` (which `AddDevice`
  takes, as a `std::shared_ptr` converts from it). Its constructor is private,
  so an invalid factory never exists, and holds nothing for a failure. A
  factory whose settings can't be invalid (the keyboard) has a public
  constructor taking the same two arguments.
- **Rates** are in real time (`baud`, `refresh_hz`), turned into cycles with
  `Device::GetClockHz()`.
- **Finding a device:** a host finds a device with `Processor::GetDevice(name)`
  and `static_cast`s it to the type it named. There are no typed lookups:
  without RTTI (which Google style and Game Bits avoid) one could only cast,
  hiding the same promise behind a function. The host built the config, so it
  knows what each name is.
- Every device's host API (`PressKey`, `TakeOutput`, `GetPicture`) is called
  between `Processor::Execute` calls, on the processor's thread, and calls
  `Wake()` where it changes what the device would do.

### devices: Teletype

```
struct TeletypeSettings {
  static constexpr int kMinBufferSize = 4;  // One dword's characters
  static constexpr int kBitsPerChar = 10;   // Start, 8 data, and stop bits

  int port = 0;
  int baud = 0;  // 0 is as fast as cores write; not negative
  int buffer_size = 4096;  // At least kMinBufferSize
};

class TeletypeFactory final : public DeviceFactory {
 public:
  // Returns an error if `settings` are invalid.
  static absl::StatusOr<std::unique_ptr<TeletypeFactory>> Create(
      std::string_view name, const TeletypeSettings& settings);
  ...
 private:
  TeletypeFactory(std::string_view name, const TeletypeSettings& settings);
};

class Teletype final : public Device {
 public:
  // Returns the text written since the last call, and makes room for more.
  std::string TakeOutput();
 protected:
  void Execute() override;
};
```

- `Execute()`: with room for four characters, it locks its port (one cycle a
  try, holding the request, until granted). If the status is set, it loads
  word 0 with `T | A` and word 1 with `S`, stores 0 to word 1, and appends
  both words' non-zero 7-bit characters. Then it releases the lock. At a baud
  rate, it advances its cycles by the characters' time
  (`chars * kBitsPerChar * clock_hz / baud`). Then it sleeps. With no room, it
  sleeps without looking.
- A store to the port wakes it, as does `TakeOutput()` when the buffer was full
  (so a host taking output every frame costs nothing otherwise). A wake while
  it is busy runs it once it is done, as a wake never moves a component's
  cycles back, so the busy time needs no state of its own.

**Brittleness:** none. Waking is by the port, and by `TakeOutput()` itself.

### devices: Keyboard

```
// Key codes and modifiers (see the tables in Behavior).
inline constexpr uint16_t kKeyUp = 0x80;  ...
inline constexpr uint16_t kKeyShift = 0x0100;  ...

struct KeyboardSettings {
  int port = 0;
  DeviceInterrupt interrupt;
};

class KeyboardFactory final : public DeviceFactory {
 public:
  KeyboardFactory(std::string_view name, const KeyboardSettings& settings);
};

class Keyboard final : public Device {
 public:
  static constexpr int kQueueSize = 16;

  // Queues a key event, and returns false if the queue is full, which drops it.
  bool PressKey(uint16_t key);
 protected:
  void Execute() override;
 private:
  gb::Queue<uint16_t> keys_;  // kQueueSize, with no grow capacity.
};
```

- `Execute()`: with a key waiting, it locks its port, and if the status is
  clear, stores the key with `S`, and raises its interrupt after releasing the
  lock. Then it sleeps. With no key waiting, it sleeps without looking. A load
  of the port (a core taking the key), or `PressKey`, wakes it.
- Its settings are only wiring, which `ProcessorConfig::Validate` checks, so a
  public constructor rather than `Create`. The queue's size is fixed, as on the
  hardware. A `gb::Queue` with no grow capacity is a fixed circular queue, and
  `PressKey` checks for room before pushing.

**Brittleness:** none. As for the teletype.

### devices: Screen

```
// screen.h
// Cell attributes, in the high byte.
inline constexpr uint16_t kScreenInverse = 0x0100;  ...

// The cursor word: a mode ORed with a style.
inline constexpr uint16_t kScreenCursorModeMask = 0x0003;
inline constexpr uint16_t kScreenCursorStyleMask = 0x000C;
enum class ScreenCursorMode : uint16_t { kOff, kOn, kSlowBlink, kFastBlink };
enum class ScreenCursorStyle : uint16_t {
  kBlock = 0x0000, kInverse = 0x0004, kUnderline = 0x0008, kLeftEdge = 0x000C };

// The character set.
inline constexpr int kScreenGlyphWidth = 8;     // Pixels, each 1 unit wide
inline constexpr int kScreenGlyphHeight = 10;   // Pixels, each 2 units tall
inline constexpr int kScreenGlyphCount = 512;   // 256, and 256 alternates
inline constexpr int kScreenGlyphSize = 5;      // Words: 2 rows a word
inline constexpr int kScreenCharacterSetSize =  // 2560 words
    kScreenGlyphCount * kScreenGlyphSize;
inline constexpr uint16_t kScreenCharacterWindowOpen = 0x0001;

// A pixel's level in the picture.
inline constexpr uint8_t kScreenPixelOff = 0;
inline constexpr uint8_t kScreenPixelDim = 1;
inline constexpr uint8_t kScreenPixelBright = 2;

struct ScreenSettings {
  static constexpr int kMaxColumns = 132;
  static constexpr int kMaxRows = 64;
  static constexpr int kMaxCells = 0xFF0;

  // The page, in its bank, and the interrupt raised at each refresh.
  int bank = 0;
  int page = 0;
  DeviceInterrupt interrupt;

  int columns = 80;     // 1 to kMaxColumns
  int rows = 24;        // 1 to kMaxRows, and at most kMaxCells cells
  int refresh_hz = 30;  // At least 1, and in effect 2

  // Blinks a second: each at least 1, and at most refresh_hz / 2.
  int slow_blink_hz = 1;
  int fast_blink_hz = 4;

  // kScreenCharacterSetSize words, or unset for the built-in one.
  std::optional<std::vector<uint16_t>> characters;
};

struct ScreenPicture {
  bool operator==(const ScreenPicture&) const = default;
  int columns = 0;
  int rows = 0;
  std::vector<uint16_t> cells;  // As shown, row by row from the top.
  int cursor_column = 0;
  int cursor_row = 0;
  ScreenCursorMode cursor_mode = ScreenCursorMode::kOff;  // Off off-screen
  ScreenCursorStyle cursor_style = ScreenCursorStyle::kBlock;
  int pixel_width = 0;          // columns * kScreenGlyphWidth
  int pixel_height = 0;         // rows * kScreenGlyphHeight
  std::vector<uint8_t> pixels;  // Row by row from the top
  int drawn_height = 0;  // pixel_height, less 2 a compact row but the bottom
};

// The screen's page, which cores read and write through its bank, and its
// character set.
class ScreenMemory final : public MemoryMap {
 public:
  // Addresses of the control words, in the screen's own addresses.
  static constexpr int kColumnsAddress = 0xFF0;  ...
  static constexpr int kReservedAddress = 0xFFC;

  ScreenMemory(int columns, int rows, absl::Span<const uint16_t> characters);

  // Under the bank's lock.
  absl::Span<const uint16_t> GetMem(const Lock& lock) const;
  absl::Span<const uint16_t> GetCharacters(const Lock& lock) const;
  void SetFrameCount(const Lock& lock, uint16_t count);
 protected:
  void Read(int address, uint16_t* data, int size) override;
  void Write(int address, const uint16_t* data, int size) override;
};

class ScreenFactory final : public DeviceFactory {
 public:
  // Returns an error if `settings` are invalid.
  static absl::StatusOr<std::unique_ptr<ScreenFactory>> Create(
      std::string_view name, const ScreenSettings& settings);
  ...
 private:
  ScreenFactory(std::string_view name, const ScreenSettings& settings);
};

class Screen final : public Device {
 public:
  MemoryMap* GetMemoryMap() override;

  // The picture taken at the last refresh, and the number of refreshes.
  const ScreenPicture& GetPicture() const;
  int64_t GetFrameCount() const;
 protected:
  void Execute() override;
};

// screen_characters.h
// The built-in character set, kScreenCharacterSetSize words.
absl::Span<const uint16_t> GetDefaultScreenCharacters();
```

- The cursor's mode and style are enums with their values in the cursor word,
  so the picture names them, and a host or test builds a word by ORing them.
- The factory's `DeviceConfig` has one page in `bank`, at `page`, readable and
  writable, one interrupt, and no ports. `Create` checks the size, the rates,
  and a set `characters`, which must be exactly `kScreenCharacterSetSize`
  words (an empty one is an error). The settings hold the character set by
  value, as a config file entry would parse into, and the factory keeps one
  copy for the screens it creates.
- `ScreenMemory` holds the whole page, and the character set beside it.
  `Write` ignores the read-only and reserved words, so only the screen sets
  them. It clears a row as it takes the write to clear row (which it can, as
  the size and the scroll row are in its page), and stores nothing there, so
  clear row reads 0; the clear is part of the one word's write, under the lock
  the core already holds, so a core sees the row cleared from its next access.
  While the window is open, it sends a write below `kScreenCharacterSetSize`
  to the character set, and `Read` never reads it. Its accessors take the
  bank's lock and `DCHECK` it, so the screen can't use its page without the
  lock.
- The `Screen` gives its `ScreenMemory` the settings' character set, or
  `GetDefaultScreenCharacters()` when it is unset.
- `Execute()`: at a refresh, it requests the lock (one cycle a try, holding
  the request, until granted), sets the frame counter, takes the cells and the
  cursor into the picture, draws the pixels, releases the lock, raises its
  interrupt, and advances to the next scheduled refresh,
  `(frames + 1) * clock_hz / refresh_hz`. It never sleeps: it is busy until
  the next refresh, which costs the `Processor` one compare a cycle.
- Drawing clears the pixels, then draws each cell's glyph rows (inverse, lines,
  blink, and the cursor applied to its rows, as Behavior's Pixels describes)
  at its row's top, keeping the brighter of what is there and what it draws.
  Each row's top is the last one's plus 8 below a compact row, and 10 below
  any other, and the last row's bottom is `drawn_height`.
- `screen_characters.cc` keeps the first 256 glyphs readable as pixel art:
  bands of 8 glyphs, each band 10 strings of the 8 glyphs' rows side by side
  (`#` lit and `.` unlit), under a comment naming the characters, with a
  `static_assert` on each string's length. `GetDefaultScreenCharacters()`
  packs them into words, and copies them to the alternates, at its first call.
  Its header comment lists what each character is, as Behavior does.
- The Osborne's shapes were drawn from its character generator (the late
  stock font, CRC 6c1eab0d), with the user's approval: the source holds its
  glyphs' pixels as pixel art, not its ROM image, and the font is meant to be
  thematically the Osborne's rather than an exact copy.
- The picture keeps the cells and the cursor, for a host (or a test, or a
  debugger) that wants the text. The pixels are what it shows, and a host can
  upload them straight to an 8-bit texture with a three-color palette, scaled
  twice as tall as wide.
- Performance: at 80 by 24, a refresh copies 1920 cells and draws 153,600
  pixels (150 KB), a glyph row at a time, 30 times a simulated second at the
  default rate: still nothing beside the cores.

**Brittleness:** none for a host. The picture is the screen's own copy, so the
host can read it whenever the processor isn't running. The control words are
the screen's own, so nothing in core knows about them.

### Testing

- `DeviceProgramTest` (`device_program_test.h`, test only) assembles a small
  program in the default instruction set with `AssembleProgram`, loads it for
  core 0 with `ProgramLoader`, and finds a label's IP, so a test runs it with
  `ExecuteUntilHalt` and times instructions with `CyclesUntilIp`. So the tests
  show what a program sees, with real `INS`, `OUTS`, and `OUTR` cycle counts.
  It duplicates the fixture in `default_instruction_set_assemble_test.cc`,
  which `devices` can't link.
- `BaseCoreTest::ExecuteUntil` stops at a million cycles, failing the test
  rather than hanging, and `ExecuteUntilHalt` returns whether the core halted.
- `teletype_test.cc`, `keyboard_test.cc`, and `screen_test.cc` test each device
  through programs and its host API, with exact cycles, and `Create` failing
  for each invalid setting. The screen's tests check pictures pixel by pixel,
  with `.`, `+`, and `#` for off, dim, and bright, using a test character set
  of their own, and draw all 512 built-in glyphs to check a screen with
  `characters` unset. `screen_characters_test.cc` checks the built-in set's
  size, its alternates, and a few glyphs' exact pixels.
