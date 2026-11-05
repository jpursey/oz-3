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
processor.Execute(16'667);
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
a configured rate.

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
| FF6h           | Cursor shown: bit 0                                           |
| FF7h to FFFh   | Reserved: reads 0, and writes are ignored                     |

- Addresses are the screen's own, from the start of its page, wherever it is
  placed.
- **Cells:** one word per cell. The low byte is the character, and the high
  byte its attributes: inverse (0100h), underline (0200h), half intensity
  (0400h), and blink (0800h), with the rest reserved. Characters 0 to 127 are
  ASCII, and 128 to 255 Ozzy's graphics characters, which the frontend draws
  (*Ozzy machine* defines them).
- **Control words** are at the end of the page, so a program finds them at the
  same address whatever the screen's size. That leaves room for 4080 cells (up
  to 80 by 51, or 132 by 30).
- **The scroll row** is taken modulo the rows, so the row shown at the top is
  the cells' row `scroll`, and the one below it `scroll + 1`, wrapping at the
  last row. Scrolling a whole screen is one write.
- **The cursor** is in screen positions, as shown (after the scroll). A cursor
  outside the screen isn't shown.
- At power on, the page is all 0 apart from the read-only words: a blank
  screen, with the cursor hidden at the top left.

The screen **refreshes** at `refresh_hz` (60 by default): refresh `n` is at
cycle `n * clock_hz / refresh_hz`, so the schedule doesn't drift where a
refresh isn't a whole number of cycles (16,666⅔ at 60 Hz and 1 MHz).
- It takes its bank's lock, takes the page as the picture, adds one to the
  frame counter, and releases the lock, all in one cycle, with no cycles of
  its own. So it never sees half of a core's write, waits only while a core
  holds the bank, and never makes a core wait.
- Then it raises its interrupt (like a vertical blank interrupt).
- Refreshes are on a fixed schedule: one that waited for the lock doesn't delay
  the next.

The host reads the picture with `Screen::GetPicture()`: the cells as shown
(with the scroll applied, row by row from the top), and the cursor. It changes
only at a refresh, so the host shows it until the next one. A picture compares
equal to another with the same contents, so a host can skip drawing one that
didn't change.

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

struct ScreenSettings {
  // The page, in its bank, and the interrupt raised at each refresh.
  int bank = 0;
  int page = 0;
  DeviceInterrupt interrupt;

  // The size, whose cells must fit before the control words (4080 at most).
  int columns = 80;
  int rows = 24;

  // Refreshes a second, at least 1.
  int refresh_hz = 60;
};

struct ScreenPicture {
  int columns = 0;
  int rows = 0;
  std::vector<uint16_t> cells;  // As shown, row by row from the top.
  int cursor_column = 0;
  int cursor_row = 0;
  bool cursor_shown = false;
  bool operator==(const ScreenPicture&) const = default;
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
- Performance: a refresh copies one page (4096 words) and builds the picture,
  60 times a simulated second at the default rate, which is nothing beside the
  cores.

**Brittleness:** none for a host. The picture is the screen's own copy, so
the host can read it whenever the processor isn't running.

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

### CL4 [ ] devices: the screen

Depends on: CL2.

- `screen.h`/`.cc`: the attribute constants, `ScreenSettings`,
  `ScreenPicture`, `ScreenMemory`, `ScreenFactory`, and `Screen`.

**Verify**
- Standard checks.
- Unit tests (`screen_test.cc`): cells written by a program appear in the
  picture only at the next refresh; refreshes on schedule, at the clock rate,
  without drift where a refresh isn't a whole number of cycles, counting frames
  in the control word and `GetFrameCount`, and raising the interrupt; the
  scroll row, including values past the rows; the cursor, shown, hidden, and
  out of range; the read-only and reserved words ignore writes; unused words
  hold what is written; a refresh while a core holds the bank waits for it,
  and never makes the core wait; the screen anywhere in a bank; and `Create`
  failing for invalid settings, and succeeding for the largest size that fits.
