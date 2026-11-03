# Console Devices

The first real devices, in a new `devices` library: the **screen**, the
**keyboard**, and the **teletype** that Ozzy's console is made of (see
[ozzy_design.md](../ozzy_design.md)). Each is a `Device` and its
`DeviceFactory`, built on what *Devices in core* added (see
[devices-in-core.md](devices-in-core.md)), so a host adds them to a
`ProcessorConfig` and finds them by name:

```
absl::StatusOr<std::unique_ptr<ScreenFactory>> screen_factory =
    ScreenFactory::Create("Screen", /*bank=*/0, /*page=*/15,
                          DeviceInterrupt{.interrupt = 6},
                          ScreenSettings{.columns = 80, .rows = 24});
if (!screen_factory.ok()) { ... }

ProcessorConfig config = ProcessorConfig::OneCore(instructions);
config.SetPortCount(16)
    .AddDevice(std::make_shared<KeyboardFactory>(
        "Keyboard", /*port=*/1, DeviceInterrupt{.interrupt = 1}))
    .AddDevice(*std::move(screen_factory));
if (absl::Status status = config.Validate(); !status.ok()) { ... }
Processor processor(config);

Keyboard* keyboard = GetKeyboard(processor, "Keyboard");
Screen* screen = GetScreen(processor, "Screen");
keyboard->PressKey('A');
processor.Execute(16'667);
const ScreenPicture& picture = screen->GetPicture();
```

The devices are host-agnostic: each holds its state (the screen's picture, the
key queue, the text written) and leaves presenting it to the host. Ozzy's
console frontend, which draws the picture and reads the host's keyboard, is the
*Ozzy machine* item.

There is no change to the specification: these devices are Ozzy's, and Ozzy's
wiki page, which documents them for programmers, is written by *Ozzy machine*.
Nothing outside `devices` changes.

A factory whose settings can be invalid (a screen of 0 rows) is made by a
static `Create`, which returns an error for them, so an invalid factory never
exists (see [devices: the library](#devices-the-library)). Its wiring is
checked by `ProcessorConfig::Validate`, as for any device.

## Behavior

### Teletype

A port that writes text to the host, for an Ozzy without a screen.
- Each word a core writes to the port is two packed characters, the high byte
  first, and a zero byte is skipped. So a single character is written as itself
  (`'A'` is 0041h), and a packed string goes straight from memory with `OUTR`.
- The teletype takes a word as soon as the port's status is set (by `OUTS`,
  `OUT`, or `OUTR`), and clears the status, so the next `OUTS` succeeds. It
  takes it the cycle after the store, so an `OUTR` of a packed string runs to
  the end without stopping.
- The characters go to the host as they are, control characters included. The
  host does what a terminal would with carriage return, line feed, backspace,
  and bell.
- The teletype keeps what has been written until the host takes it with
  `Teletype::TakeOutput()`, up to a buffer size (4096 characters by default).
  While the buffer is full, it takes no more words, so the port's status stays
  set, `OUTS` fails, and `OUTR` stops early, until the host takes the text. A
  program that writes faster than the host reads loops on `OUTS`, as it would
  on a slow serial line.
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

The screen **refreshes** every `refresh_cycles` (16,667 by default: 60 Hz at
1 MHz), the first at cycle `refresh_cycles`:
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
| A device's own settings     | `ScreenSettings`, `ScreenOptions`, `ScreenParams`  | `DeviceConfig` (wiring), `CpuCore::ResetParams`               | `ScreenSettings`   |
| What the host shows         | `ScreenPicture`, `ScreenFrame`, `ScreenImage`      | the frame counter (a count), stack frames in programs         | `ScreenPicture`    |
| The screen's page           | `ScreenMemory`, `ScreenPage`, `ScreenMap`          | `MemoryBank`, `FakeMemoryMap`                                 | `ScreenMemory`     |
| Host gives a key            | `PressKey`, `TypeKey`, `AddKey`                    | "Add" (the owner registers and keeps it)                      | `PressKey`         |
| Host takes teletype text    | `TakeOutput`, `TakeText`, `ReadOutput`             | `Port::LoadWord`, the `IN` instruction                        | `TakeOutput`       |
| Typed lookups               | `GetScreen`, `FindScreen`                          | `Processor::GetDevice`, which they wrap                       | `GetScreen`        |

`DeviceFactory`'s comments already call a derived factory's own values its
"settings" ("such as a screen's size"), so `ScreenSettings` follows them.
"Picture" is the word
[ozzy_design.md](../ozzy_design.md) uses for what a refresh takes, and
`ScreenFrame` would read as one count of the frame counter.

### devices: the library

- `oz3/devices/`, the `oz3_devices` library, which links `oz3_core` only. Its
  tests, `oz3_devices_test`, also link `oz3_core_testing`, `oz3_tools`, and
  `oz3_instruction_sets`, to run small OZ-3 programs in the default
  instruction set against the devices, assembled from source with
  `AssembleProgram` and loaded with `ProgramLoader`. So the tests show what a
  program sees, with real `INS`, `OUTS`, and `OUTR` cycle counts.
- Each device is a `.h`/`.cc` with its factory, its settings, and a typed
  lookup (`GetScreen(processor, name)`), which returns null if the processor
  has no device of that name, or it isn't that type (`dynamic_cast`, the first
  in OZ-3; a wrong type is a host's mistake with a name from data, so it
  shouldn't be undefined behavior).
- A factory with settings that can be invalid is made only by its static
  `Create`, which returns an error naming the device and the setting, or the
  factory as a `std::unique_ptr` (which `AddDevice` takes, as a
  `std::shared_ptr` converts from it). Its constructor is private, so an
  invalid factory never exists, and holds nothing for a failure: a host
  building its machine from data gets the error at the one place that can
  fail, and the factory, the `Processor`, and the device all rely on valid
  settings. A factory with no such settings (the keyboard) has a public
  constructor, as the wiring is `ProcessorConfig::Validate`'s to check.
- Every device's host API (`PressKey`, `TakeOutput`, `GetPicture`) is called
  between `Processor::Execute` calls, on the processor's thread, and calls
  `Wake()` where it changes what the device would do.
- CLAUDE.md's library list gains `devices`.

### devices: Teletype

```
struct TeletypeSettings {
  // The most characters the teletype keeps for the host, at least 2.
  int buffer_size = 4096;
};

class TeletypeFactory final : public DeviceFactory {
 public:
  // Returns an error if `settings` are invalid.
  static absl::StatusOr<std::unique_ptr<TeletypeFactory>> Create(
      std::string_view name, int port, TeletypeSettings settings = {});
  ...
 private:
  TeletypeFactory(std::string_view name, int port, TeletypeSettings settings);
};

class Teletype final : public Device {
 public:
  // Returns the text written since the last call, and makes room for more.
  std::string TakeOutput();
 protected:
  void Execute() override;
};

Teletype* GetTeletype(Processor& processor, std::string_view name);
```

- `Execute()`: with room for two characters, it locks its port (one cycle a
  try, holding the request, until granted), loads with `T | S`, appends the
  word's non-zero bytes, releases the lock, advances one cycle, and sleeps.
  With no room, it sleeps without looking. A store to the port, or
  `TakeOutput()`, wakes it.

**Brittleness:** none. Waking is by the port, and by `TakeOutput()` itself.

### devices: Keyboard

```
// Key codes and modifiers (see the tables in Behavior).
inline constexpr uint16_t kKeyUp = 0x80;  ...
inline constexpr uint16_t kKeyShift = 0x0100;  ...

class KeyboardFactory final : public DeviceFactory {
 public:
  KeyboardFactory(std::string_view name, int port, DeviceInterrupt interrupt);
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

Keyboard* GetKeyboard(Processor& processor, std::string_view name);
```

- `Execute()`: with a key waiting, it locks its port, and if the status is
  clear, stores the key with `S`, and raises its interrupt after releasing the
  lock. Then it advances one cycle and sleeps. With no key waiting, it sleeps
  without looking. A load of the port (a core taking the key), or `PressKey`,
  wakes it.
- No settings, so a public constructor rather than `Create`: the queue's size
  is fixed, as on the hardware. A `gb::Queue` with no grow capacity is a fixed circular
  queue, and `PressKey` checks for room before pushing.

**Brittleness:** none. As for the teletype.

### devices: Screen

```
// Cell attributes, in the high byte.
inline constexpr uint16_t kScreenInverse = 0x0100;  ...

struct ScreenSettings {
  // The size, whose cells must fit before the control words (4080 at most).
  int columns = 80;
  int rows = 24;

  // The cycles between refreshes, at least 1.
  Cycles refresh_cycles = 16'667;
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
      std::string_view name, int bank, int page, DeviceInterrupt interrupt,
      ScreenSettings settings = {});
  ...
 private:
  ScreenFactory(std::string_view name, int bank, int page,
                DeviceInterrupt interrupt, ScreenSettings settings);
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

Screen* GetScreen(Processor& processor, std::string_view name);
```

- The factory's `DeviceConfig` has one page in `bank`, at `page`, readable and
  writable, one interrupt, and no ports.
- `ScreenMemory` holds the whole page. `Write` ignores the read-only and
  reserved words, so only the screen sets them. Its accessors take the bank's
  lock and `DCHECK` it, so the screen can't use its page without the lock.
- `Execute()`: at a refresh, it requests the lock (one cycle a try, holding
  the request, until granted), takes the picture, sets the frame counter,
  releases the lock, raises its interrupt, and advances to the next scheduled
  refresh. It never sleeps: it is busy until the next refresh, which costs
  the `Processor` one compare a cycle.
- Performance: a refresh copies one page (4096 words) and builds the picture,
  60 times a simulated second at the default rate, which is nothing beside the
  cores.

**Brittleness:** none for a host. The picture is the screen's own copy, so
the host can read it whenever the processor isn't running.

### To confirm

- That an `OUTR` of a packed string to the teletype runs to its end without
  stopping, as the teletype takes each word the cycle after it is stored, and
  before the `OUTR`'s next word. CL1's tests check it.
- That `AssembleProgram` and `ProgramLoader` make small test programs simpler
  than encoding them by hand, as the instruction set tests do. CL1 picks
  whichever is simpler for all the devices' tests.

## CLs

### CL1 [ ] devices: the library, and the teletype

Depends on: nothing.

- `oz3/devices/CMakeLists.txt`, added to `oz3/CMakeLists.txt` after
  `instruction_sets`.
- `teletype.h`/`.cc`: `TeletypeSettings`, `TeletypeFactory`, `Teletype`,
  `GetTeletype`.
- CLAUDE.md: `devices` in the directory structure's library list, in the
  dependency order, and the sentence on `devices` and `ozzy` changes to say
  only `ozzy` doesn't exist yet.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests (`teletype_test.cc`): `OUTS` of single characters and of packed
  words, with zero bytes skipped; an `OUTR` of a packed string runs to the end
  with no stop, in its documented cycles; control characters pass through;
  a full buffer leaves the status set and `OUTS` failing, until `TakeOutput`;
  the teletype sleeps while nothing is written; `Create` failing for invalid
  settings; and `GetTeletype` for a missing name and for another device type.

### CL2 [ ] devices: the keyboard

Depends on: CL1.

- `keyboard.h`/`.cc`: the key code and modifier constants, `KeyboardFactory`,
  `Keyboard`, `GetKeyboard`.

**Verify**
- Standard checks.
- Unit tests (`keyboard_test.cc`): a pressed key is readable with `INS`, with
  its modifiers, and raises the interrupt once; keys come out in order, each
  the cycle after the last was read, each raising the interrupt; a full queue
  drops keys; an interrupt handler reading keys sees each one; the keyboard
  sleeps with no keys waiting; and `GetKeyboard`.

### CL3 [ ] devices: the screen

Depends on: CL1.

- `screen.h`/`.cc`: the attribute constants, `ScreenSettings`,
  `ScreenPicture`, `ScreenMemory`, `ScreenFactory`, `Screen`, `GetScreen`.

**Verify**
- Standard checks.
- Unit tests (`screen_test.cc`): cells written by a program appear in the
  picture only at the next refresh; refreshes on schedule, counting frames in
  the control word and `GetFrameCount`, and raising the interrupt; the scroll
  row, including values past the rows; the cursor, shown, hidden, and out of
  range; the read-only and reserved words ignore writes; unused words hold
  what is written; a refresh while a core holds the bank waits for it, and
  never makes the core wait; the screen anywhere in a bank; `Create` failing
  for invalid settings, and succeeding for the largest size that fits; and
  `GetScreen`.
