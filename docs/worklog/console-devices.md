# Console Devices

The first real devices, in a new `devices` library: the **screen**, the
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
key queue, the text written) and leaves presenting it to the host. Ozzy's
console frontend, which draws the picture and reads the host's keyboard, is the
*Ozzy machine* item.

A device is configured in real time (a teletype's baud rate, a screen's
refresh rate), which it turns into cycles with the processor's **clock rate**,
new in `ProcessorConfig`. So the same device settings work at any clock rate.

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
- `BaseCoreTest::InitConfig` gains a `clock_hz`.
- Wiki: `2-Specifications.md`'s overview gains a **Clock** bullet.

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
  string runs to the end without stopping.
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
a configured rate. CL4 is the screen itself, and CL5 to CL8 add to it, as
marked: until a CL adds its control words, they are reserved.

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
| FF7h           | Fill: the cell a row is cleared to (CL5)                      |
| FF8h           | Clear row: clears a row as it is written (CL5)                |
| FF9h           | Character set window: bit 0 (CL6)                             |
| FFAh           | First compact row (CL7)                                       |
| FFBh           | Compact rows: how many (CL7)                                  |
| FFCh to FFFh   | Reserved: reads 0, and writes are ignored                     |

- Addresses are the screen's own, from the start of its page, wherever it is
  placed.
- **Cells:** one word per cell. The low byte is the character, and the high
  byte its attributes. Characters 0 to 127 are ASCII, and 128 to 255 Ozzy's
  graphics characters (the built-in character set, CL8, defines them).

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
  screen is never a long thin strip (from CL6).
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
  screen, with the cursor off at the top left.

The screen **refreshes** at `refresh_hz` (30 by default, the full frames a
second of an interlaced NTSC display): refresh `n` is at cycle
`n * clock_hz / refresh_hz`, so the schedule doesn't drift where a refresh
isn't a whole number of cycles (33,333⅓ at 30 Hz and 1 MHz).
- It takes its bank's lock, adds one to the frame counter, takes the page as
  the picture (drawing its pixels, from CL6), and releases the lock, all in
  one cycle, with no cycles of
  its own. So it never sees half of a core's write, and waits only while a
  core holds the bank. A refresh that waited is granted the bank as the core
  releases it, so a core asking for it again in that cycle (as the next
  instruction's fetch does) waits a cycle for the refresh: the lock handoff's
  cost, which *Order-independent lock handoff* in the backlog removes.
- Then it raises its interrupt (like a vertical blank interrupt).
- Refreshes are on a fixed schedule: one that waited for the lock doesn't delay
  the next, and refreshes it missed run a cycle apart until it is back on the
  schedule.

The host reads the picture with `Screen::GetPicture()`: the cells as shown
(with the scroll applied, row by row from the top), and the cursor's position,
mode, and style. From CL6, it also holds the pixels, which are what the host
shows (see Pixels). It changes only at a refresh, so the host shows it until
the next one. A picture compares equal to another with the same contents, so a
host can skip drawing one that didn't change.

### Clearing rows (CL5)

- **Fill** (FF7h) is the cell a cleared row is filled with, such as a space
  with attributes. It is 0 at power on.
- Writing a row to **clear row** (FF8h) fills that row with the fill word as
  the write happens: one word written clears a whole row, a hardware assist.
  The row is in screen positions, as shown (after the scroll), and a row
  outside the screen clears nothing. Clear row reads 0.
- So a terminal's line feed on the bottom row is two writes: add one to the
  scroll row, then clear row `rows - 1`. Clearing the screen is a write for
  each row.

### Character set (CL6)

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
- At power on, the character set is the built-in one (CL8): Ozzy's font, which
  a host can replace with its own in the settings.

### Pixels (CL6)

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

### Compact rows (CL7)

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
`ScreenFrame` would read as one count of the frame counter.

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
  instruction set against the devices, assembled from source with
  `AssembleProgram` and loaded with `ProgramLoader`. So the tests show what a
  program sees, with real `INS`, `OUTS`, and `OUTR` cycle counts.
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
- CLAUDE.md's library list gains `devices`.

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
// Cell attributes, in the high byte.
inline constexpr uint16_t kScreenInverse = 0x0100;  ...

// The cursor word: a mode ORed with a style.
inline constexpr uint16_t kScreenCursorModeMask = 0x0003;
inline constexpr uint16_t kScreenCursorStyleMask = 0x000C;
enum class ScreenCursorMode : uint16_t { kOff, kOn, kSlowBlink, kFastBlink };
enum class ScreenCursorStyle : uint16_t {
  kBlock = 0x0000, kInverse = 0x0004, kUnderline = 0x0008, kLeftEdge = 0x000C };

struct ScreenSettings {
  // The page, in its bank, and the interrupt raised at each refresh.
  int bank = 0;
  int page = 0;
  DeviceInterrupt interrupt;

  // The size, whose cells must fit before the control words (4080 at most),
  // and from CL6, at most kMaxColumns (132) by kMaxRows (64).
  int columns = 80;
  int rows = 24;

  // Refreshes a second, at least 1.
  int refresh_hz = 30;
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
};

// The screen's page, which cores read and write through its bank.
class ScreenMemory final : public MemoryMap {
 public:
  // Addresses of the control words, in the screen's own addresses.
  static constexpr int kColumnsAddress = 0xFF0;  ...

  // The page's words, under the bank's lock.
  absl::Span<const uint16_t> GetMem(const Lock& lock) const;
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
```

- The cursor's mode and style are enums with their values in the cursor word,
  so the picture names them, and a host or test builds a word by ORing them.
- The factory's `DeviceConfig` has one page in `bank`, at `page`, readable and
  writable, one interrupt, and no ports.
- `ScreenMemory` holds the whole page. `Write` ignores the read-only and
  reserved words, so only the screen sets them. Its accessors take the bank's
  lock and `DCHECK` it, so the screen can't use its page without the lock.
- `Execute()`: at a refresh, it requests the lock (one cycle a try, holding
  the request, until granted), takes the picture, sets the frame counter,
  releases the lock, raises its interrupt, and advances to the next scheduled
  refresh, `(frames + 1) * clock_hz / refresh_hz`. It never sleeps: it is busy
  until the next refresh, which costs the `Processor` one compare a cycle.
- Performance: a refresh copies the cells into the picture, 30 times a
  simulated second at the default rate, which is nothing beside the cores.

**Brittleness:** none for a host. The picture is the screen's own copy, so
the host can read it whenever the processor isn't running.

### devices: clearing rows

```
class ScreenMemory final : public MemoryMap {
 public:
  static constexpr int kFillAddress = 0xFF7;
  static constexpr int kClearRowAddress = 0xFF8;
  static constexpr int kReservedAddress = 0xFF9;
};
```

- `ScreenMemory::Write` clears the row as it takes the write to clear row, so
  it needs the size and the scroll row (which it has, in its page), and stores
  nothing there, so clear row reads 0.
- The clear is part of the one word's write, under the lock the core already
  holds, so a core sees the row cleared from its next access, and the next
  refresh shows it.

**Brittleness:** none.

### devices: the character set and the pixels

```
// screen.h
inline constexpr int kScreenGlyphWidth = 8;     // Pixels, each 1 unit wide
inline constexpr int kScreenGlyphHeight = 10;   // Pixels, each 2 units tall
inline constexpr int kScreenGlyphCount = 512;   // 256, and 256 alternates
inline constexpr int kScreenGlyphSize = 5;      // Words: 2 rows a word
inline constexpr int kScreenCharacterSetSize =  // 2560 words
    kScreenGlyphCount * kScreenGlyphSize;

// The character set window word's bit that opens the window.
inline constexpr uint16_t kScreenCharacterWindowOpen = 0x0001;

// A pixel's level in the picture.
inline constexpr uint8_t kScreenPixelOff = 0;
inline constexpr uint8_t kScreenPixelDim = 1;
inline constexpr uint8_t kScreenPixelBright = 2;

struct ScreenSettings {
  ...
  // Blinks a second: each at least 1, and at most refresh_hz / 2.
  int slow_blink_hz = 1;
  int fast_blink_hz = 4;

  // The character set at power on, kScreenCharacterSetSize words, or unset
  // for the built-in one.
  std::optional<std::vector<uint16_t>> characters;
};

struct ScreenPicture {
  ...
  // columns * kScreenGlyphWidth by rows * kScreenGlyphHeight, row by row.
  int pixel_width = 0;
  int pixel_height = 0;
  std::vector<uint8_t> pixels;
};

class ScreenMemory final : public MemoryMap {
 public:
  static constexpr int kCharacterWindowAddress = 0xFF9;
  static constexpr int kReservedAddress = 0xFFA;

  ScreenMemory(int columns, int rows, absl::Span<const uint16_t> characters);

  // The character set, under the bank's lock.
  absl::Span<const uint16_t> GetCharacters(const Lock& lock) const;
};
```

- `ScreenMemory` holds the character set beside the page's words. `Write`
  sends a write below `kScreenCharacterSetSize` to the character set while
  the window's bit 0 is set, and `Read` never reads it.
- `Create` returns an error for a `characters` that is set to anything but
  `kScreenCharacterSetSize` words, an empty one included. Until CL8, an unset
  one is a blank character set.
- A refresh draws the pixels from the cells, the character set, and the
  cursor, under the lock, as Behavior's Pixels describes, with the blink
  phases from the frame count. At 80 by 24, that is 153,600 pixels (150 KB),
  drawn a glyph row at a time, about 20,000 rows a refresh: still nothing
  beside the cores, and a host can upload the pixels straight to an 8-bit
  texture with a three-color palette.
- The picture keeps the cells and the cursor, for a host (or a test, or a
  debugger) that wants the text. The pixels are what it shows.
- The settings hold the character set by value, as a config file entry would
  parse into, and the factory keeps one copy for the screens it creates.

**Brittleness:** none. The window is the screen's own control word, so nothing
in core changes.

### devices: compact rows

```
class ScreenMemory final : public MemoryMap {
 public:
  static constexpr int kCompactRowAddress = 0xFFA;
  static constexpr int kCompactRowCountAddress = 0xFFB;
  static constexpr int kReservedAddress = 0xFFC;
};

struct ScreenPicture {
  ...
  // The pixel rows drawn, from the top: pixel_height, less 2 for each compact
  // row but the bottom one.
  int drawn_height = 0;
};
```

- Drawing places each screen row at the pixel row below the last, 8 down after
  a compact row and 10 after a full one, and takes the brighter of each
  overlapping pair of pixels.

**Brittleness:** none.

### devices: the built-in character set

- `screen_characters.h`/`.cc`, with `GetDefaultScreenCharacters()`: 512 glyphs
  as a constant array, which an unset `ScreenSettings::characters` uses.
- Characters 0 to 127 are ASCII in the Osborne 1's glyph shapes, if a source
  for its character generator's pixels can be found, and its terms allow
  using them. Otherwise, a public domain 8 by 10 font. The control characters,
  128 to 255 (Ozzy's graphics characters), and the alternate 256 are decided
  in CL8, along with how the glyphs are kept readable in the source.

**Brittleness:** none.

### To confirm

- That an `OUTR` of a packed string to the teletype runs to its end without
  stopping, as the teletype takes each word the cycle after it is stored, and
  before the `OUTR`'s next word. **Confirmed in CL2:** an `OUTR.IW` of 3 words
  takes 27 cycles (9 a word, as for a port that is always ready), with `R7`
  left at 0, and an `OUTR.ID` of 3 dwords takes 33 (11 a dword).
- That `AssembleProgram` and `ProgramLoader` make small test programs simpler
  than encoding them by hand, as the instruction set tests do. **Confirmed in
  CL2:** `DeviceProgramTest` (`device_program_test.h`, test only) assembles a
  program, loads it for core 0, and finds a label's IP, so a test can run it
  with `ExecuteUntilHalt` and time instructions with `CyclesUntilIp`. The
  other devices' tests use it too. It duplicates the fixture in
  `default_instruction_set_assemble_test.cc`, which `devices` can't link, and
  that is fine.

## CLs

Before CL2, an ordinary change to `BaseCoreTest` (not part of this plan) caps
`ExecuteUntil` at a million cycles, failing the test rather than hanging, and
makes `ExecuteUntilHalt` return whether the core halted. The devices' tests
rely on both.

### CL1 [x] core: the processor's clock rate

Depends on: nothing.

- `ProcessorConfig::SetClockHz`, `GetClockHz`, and `kDefaultClockHz`, checked
  by `Validate()`. `Processor::GetClockHz`, and the protected
  `Device::GetClockHz` (public in `FakeDevice`).
- `BaseCoreTest::InitConfig::clock_hz`.
- Wiki: the **Clock** bullet in `2-Specifications.md`'s overview.
- Unused by any device yet, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: the default rate, and `Validate` rejecting 0 and negative rates
  (`processor_config_test.cc`); a device reading its processor's rate
  (`device_test.cc`).

### CL2 [x] devices: the library, and the teletype

Depends on: CL1.

- `oz3/devices/CMakeLists.txt`, added to `oz3/CMakeLists.txt` after
  `instruction_sets`.
- `teletype.h`/`.cc`: `TeletypeSettings`, `TeletypeFactory`, and `Teletype`.
- `device_program_test.h`: the `DeviceProgramTest` fixture.
- CLAUDE.md: `devices` in the directory structure's library list, in the
  dependency order, and the sentence on `devices` and `ozzy` changes to say
  only `ozzy` doesn't exist yet.

**Verify**
- Standard checks.
- Unit tests (`teletype_test.cc`): `OUTS` of single characters and of packed
  words, with zero bytes skipped; an `OUTR` of a packed string runs to the end
  with no stop, in its documented cycles, and so does an `OUTR.ID`, with a word
  written after it not followed by stale characters; control characters pass
  through, and the eighth bit is dropped; at a baud rate, the exact cycle each
  write is taken, with the port holding one write meanwhile, zero bytes taking
  no time, and twice the clock rate taking twice the cycles; an `OUTR` stopping
  early at a baud rate; a full buffer leaves the status set and `OUTS`
  failing, until `TakeOutput`; the teletype sleeps while nothing is written;
  and `Create` failing for invalid settings.

### CL3 [x] devices: the keyboard

Depends on: CL2.

- `keyboard.h`/`.cc`: the key code and modifier constants, `KeyboardSettings`,
  `KeyboardFactory`, and `Keyboard`.

**Verify**
- Standard checks.
- Unit tests (`keyboard_test.cc`): a pressed key is readable with `INS`, with
  its modifiers, and raises the interrupt once; keys come out in order, each
  the cycle after the last was read, each raising the interrupt; a full queue
  drops keys; an interrupt handler reading keys sees each one; and the
  keyboard sleeps with no keys waiting.

### CL4 [x] devices: the screen

Depends on: CL2.

- `screen.h`/`.cc`: the attribute and cursor constants, `ScreenSettings`,
  `ScreenPicture`, `ScreenMemory`, `ScreenFactory`, and `Screen`.

**Verify**
- Standard checks.
- Unit tests (`screen_test.cc`): cells written by a program appear in the
  picture only at the next refresh, attributes and all; refreshes on schedule,
  at the default rates and others, without drift where a refresh isn't a whole
  number of cycles, counting frames in the control word and `GetFrameCount`,
  and raising the interrupt; the scroll row, including values past the rows;
  the cursor word's modes and styles, with its reserved bits ignored, and a
  cursor outside the screen off; the read-only and reserved words ignore
  writes; unused words hold what is written; a refresh while a core holds the
  bank waits for it, costs the core only the lock handoff's cycle, and
  refreshes it missed catch up; the screen anywhere in a bank; and `Create`
  failing for invalid settings, and succeeding for the largest size that fits.

### CL5 [x] devices: clearing rows

Depends on: CL4.

- The fill and clear row control words, in `ScreenMemory`.
- The reserved words start at FF9h.

**Verify**
- Standard checks.
- Unit tests (`screen_test.cc`): clear row fills a row with the fill word, in
  screen positions after the scroll, and reads 0; a row outside the screen
  clears nothing; a line feed on the bottom row with two writes from a
  program; and the fill word 0 at power on.

### CL6 [x] devices: the character set and the pixels

Depends on: CL5.

- `screen.h`/`.cc`: the glyph and pixel constants, `ScreenSettings::characters`
  and the blink rates, the character set window control word, `ScreenMemory`'s
  character set, and the picture's pixels, drawn at each refresh.
- The reserved words start at FFAh.
- `ScreenSettings::kMaxColumns` (132) and `kMaxRows` (64), which `Create`
  checks along with `kMaxCells`.

**Verify**
- Standard checks.
- Unit tests (`screen_test.cc`): with the window set, a program's writes to
  0 to 9FFh change glyphs (at `n * 5`, two rows a word) and not the cells,
  while its reads still return the cells; writes from A00h are unchanged by
  the window; a changed glyph appears in the pixels only at the next refresh;
  the settings' character set is the one at power on; `Create` failing for a
  character set of the wrong size, and for a blink rate below 1 or above half
  the refresh rate; and the exact pixels of a cell plain, dim, inverse, with
  each line, alternate, blinking in each phase, and under each cursor style
  and mode, with the blink phases at the frames they change, at the default
  blink rates and others.

### CL7 [x] devices: compact rows

Depends on: CL6.

- The compact row control words, in `ScreenMemory`, and drawing them, with
  `ScreenPicture::drawn_height`.
- The reserved words start at FFCh.

**Verify**
- Standard checks.
- Unit tests (`screen_test.cc`): a band's rows packed 8 pixel rows apart, with
  overlapping pixels the brighter of the two, and the pixel rows below off; a
  compact bottom row drawn whole; the band staying put as the cells scroll;
  rows of the band outside the screen ignored; and `drawn_height`.

### CL8 [ ] devices: the built-in character set

Depends on: CL6.

- Find the Osborne 1's character generator pixels, and check their terms with
  the user before using them. Otherwise, a public domain 8 by 10 font.
- `screen_characters.h`/`.cc`: `GetDefaultScreenCharacters()`, and an unset
  `ScreenSettings::characters` uses it.
- Decide the control characters, the graphics characters (128 to 255), and the
  alternate 256 with the user, and update Behavior to match.

**Verify**
- Standard checks.
- Unit tests (`screen_characters_test.cc`): the character set's size, a few
  glyphs' exact pixels (such as `A`), and a screen with `characters` unset using
  it.
