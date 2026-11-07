# Keyboard key state

The keyboard gives typed keys: key presses only, as characters, with the
modifier keys held as a mask, and never a modifier key alone. That serves a
program reading text, but not a game, which wants the keys held, and the keys
pressed, by their place on the keyboard. A queue of press and release events
can't give that reliably: a release the program never reads leaves its key
held, and a queue nobody reads fills.

So the keyboard has **key state**: ports holding a bit for each key held, and
for each key pressed since the port was last read. It is state rather than
events, so nothing is lost when a program doesn't read it, and nothing waits to
be read. A keyboard's wiring chooses typed keys, key state, or both, and the
Ozzy's keyboard has both, with the console frontend reporting every key to
each. Memory mapping would take a whole page (4 Kiw) for 10 words, so key state
is in ports (see the backlog's *Shared I/O page*).

## Behavior

### Devices: the keyboard

**Wiring:** typed keys are a port and an interrupt. Key state is
`Keyboard::kKeyStatePorts` (10) consecutive ports from a first port, with no
interrupt. A keyboard has either or both.

**Key codes:** each key has a code, which is its bit in the key state: bit `i`
of key state port `n` is the key with code `16n + i`. A key's code is the typed
key's code for the key without Shift on a US keyboard, so one table serves
both, and a key keeps its code whatever the host's layout, as games want (`W`,
`A`, `S`, and `D` stay together on an AZERTY keyboard). Codes 0 to 9Fh fit 10
ports:

| Key                                            | Code               |
| ---------------------------------------------- | ------------------ |
| Backspace, Tab, Enter, Escape                  | 08h, 09h, 0Dh, 1Bh |
| Space                                          | 20h                |
| `'` `,` `-` `.` `/`                            | 27h, 2Ch to 2Fh    |
| `0` to `9`                                     | 30h to 39h         |
| `;` `=`                                        | 3Bh, 3Dh           |
| `[` `\` `]` `` ` ``                            | 5Bh to 5Dh, 60h    |
| `A` to `Z`                                     | 61h to 7Ah         |
| Delete                                         | 7Fh                |
| Up, Down, Left, Right                          | 80h to 83h         |
| Home, End, Page Up, Page Down, Insert          | 84h to 88h         |
| Left Shift, Right Shift                        | 89h, 8Ah           |
| Left Ctrl, Right Ctrl                          | 8Bh, 8Ch           |
| Left Alt, Right Alt                            | 8Dh, 8Eh           |
| F1 to F12                                      | 90h to 9Bh         |

Letters are their lower case codes, as `a` is the key without Shift. Keys
without a code (the keypad, Caps Lock, Num Lock, Scroll Lock, Print Screen,
Pause, the Windows and menu keys) aren't in the key state. The other codes are
never set, and 9Ch to 9Fh are free for keys added later.

**Reading:** each key state port holds 16 keys:

| Word | What                                                |
| ---- | --------------------------------------------------- |
| 0    | The keys held                                       |
| 1    | The keys pressed since the port was last read       |

- The status is set while word 1 isn't 0.
- Reading the port clears its status (`IN` does, as well as `INS`), which takes
  its pressed keys: the keyboard clears them from word 1, keeping any pressed
  since. So a program reads the dword, `IN.RD` (or `INS.RD`, which reads only
  when a key on the port was pressed), and gets both at once. Reading word 0
  alone takes the pressed keys without seeing them.
- A key pressed and released between two reads is pressed but not held, so a
  tap is never missed. A key held down presses once: the host's auto-repeat
  doesn't press it again.
- Each port has its own lock, so a program reading the ports while the keyboard
  updates them may see some ports from before a change and some from after.
  Keys on one port change together.
- The keyboard writes a port only when one of its keys changes, or its pressed
  keys are taken, with no cycles of its own, so it waits only while a core
  holds the port. A core's write to a key state port stays until the keyboard
  next writes it, and a write that sets the status makes `INS` read it.

The typed keys don't change for programs. The host translates every typed key
itself, by its own layout, and types it: the keyboard never makes typed keys
from key state (see the backlog's *Typed keys from key state*). A host
reporting a key to a keyboard with both calls both, `TypeKey` for the
character it types (if any) and `SetKeyState` for its code (if it has one).

**Host interface:**

```
// Key codes for the modifier keys, only in key state (typed keys hold the
// modifiers as bits, kKeyShift, kKeyCtrl, and kKeyAlt).
inline constexpr uint16_t kKeyLeftShift = 0x89;  ...  kKeyRightAlt = 0x8E;

struct KeyboardSettings {
  std::optional<int> state_port;      // First key state port, or none.
  std::optional<int> typed_port = 0;  // The typed keys' port, or none.
  DeviceInterrupt typed_interrupt;    // Raised as each typed key arrives.
};

class KeyboardFactory {
  // Fails for neither typed keys nor key state, or for the typed keys' port
  // among the key state's.
  static absl::StatusOr<std::unique_ptr<KeyboardFactory>> Create(
      std::string_view name, const KeyboardSettings& settings);
};

class Keyboard {
 public:
  static constexpr int kKeyStatePorts = 10;
  static constexpr int kKeyStateKeys = kKeyStatePorts * 16;

  // Queues a typed key event (was PressKey). Returns false if the queue is
  // full, or the keyboard has no typed keys.
  bool TypeKey(uint16_t key);

  // Sets whether the key with code `key` is held. A key that wasn't held is
  // pressed too. Does nothing without key state, or for a code out of range.
  void SetKeyState(int key, bool held);
};
```

The settings' `port` and `interrupt` became `typed_port` and `typed_interrupt`,
so each field says which keys it is for, and `typed_port` stays 0 by default,
so a default keyboard is as it was. `PressKey` became `TypeKey`, as "press" now
means a key going down in key state.

**Wiki:** `3.2-Keyboard.md` has the key state's wiring, reading, and key codes,
with the reading and key events under typed keys, and `3-Devices.md`'s table
gives the keyboard's ports as 1, 10, or 11.

### Ozzy

- The Ozzy's keyboard has key state on ports 6 to 15, the end of the ports
  kept for devices (0 to 15), leaving 0, 3, 4, and 5 for devices to come.
  `{ type: "keyboard" }` is unchanged, and has both.
- The machine table's keyboard entry gives the first key state port in its
  first data word (FFFFh for none), and its second stays 0. Version 1 is
  unchanged: the word was 0, and no program read it.
- The console frontend reports every key's presses and releases to the key
  state, by where the key is (its scan code), and releases them all when the
  console loses focus, which sends no releases for keys held then.
- Nothing in the Ozzy reads key state yet: the boot ROM reads only typed keys.
- **Wiki:** `4-Ozzy-Computer.md`'s device table, machine table, its example,
  and the console frontend's keyboard.

## Design

### Names

- **Typed keys** is the port of key events, and **key state** the ports of
  bits. "Key matrix" (the scanning hardware of old keyboards) and "held keys"
  (only word 0) were set aside. "Key event" stays the typed key's word.
- **`SetKeyState(key, held)`**, as the feature is key state, rather than
  `SetKeyHeld` (reads as always holding it), or `HoldKey` and `ReleaseKey`.
- **`TypeKey`**, renamed from `PressKey`, as the typed keys are typed.
- **`kKeyLeftShift` to `kKeyRightAlt`**: key codes, beside `kKeyShift`,
  `kKeyCtrl`, and `kKeyAlt`, which are the typed key event's modifier bits.
  The names are close, so the comments say which is which.
- **`state_port`**, **`typed_port`**, and **`typed_interrupt`** in
  `KeyboardSettings`, with the two ports together.
- **`KeyChange`** (`{key, held}`) and **`ConsoleKeys`** (`typed` and
  `changes`) in the console frontend, and **`ConsoleKeyState`**, the keys a
  Windows console reported held.
- **`GetKeyEvent`**, renamed from `GetOzzyKey`, and **`GetKeyCode`**: they give
  the keyboard device's typed key events and key codes, which aren't the
  Ozzy's own. They are in `win_console`, the only Windows host.

### devices: Keyboard

- `KeyboardFactory` wires the typed port (if any) and then the key state ports
  (if any), in that order, and the interrupt only with the typed port. Its
  settings can be invalid, so it is made by `Create`, as the teletype's and
  screen's are, which catches a typed port among the key state's (devices may
  share ports, so `ProcessorConfig::Validate` wouldn't). Port ranges are checked
  by `ProcessorConfig::Validate`. Like the teletype's, the factory keeps its
  settings, and the keyboard is constructed from them.
- The keyboard keeps, for each key state port: the keys held, the keys pressed
  and not yet taken, the pressed keys it last put in word 1, whether a key
  changed since, and its lock while requested.
  - `SetKeyState` sets or clears the held bit, and on a press of a key not held,
    its pressed bit, marks the port changed, and wakes the keyboard.
  - `Execute` runs the typed keys and the key state each time, so it waits for
    every lock it needs at once. It requests the lock of every changed port,
    and of every port it put pressed keys in whose status reads clear (a core
    read it). Under the lock, if the status is clear, the pressed keys it put
    there were taken, so it removes only those, keeping any the host pressed
    since. Then it writes word 0, and word 1 with the status if it isn't 0, and
    releases the lock. A requested lock is always used and released, so a port
    is never left locked. With nothing to write, it sleeps.
  - Reads and writes of its ports wake it (`Port` wakes the components wired
    to a port), which is how it learns of a read.
- Performance: a game polling 10 ports each frame wakes the keyboard 10 times a
  frame, each a check of 10 ports.

**Brittleness:** a press the host makes while a core's read is waiting to be
taken must survive it, which is why the keyboard removes only the pressed keys
it put in word 1, rather than clearing them all. A test presses a key between
a read and the keyboard taking it.

### ozzy: wiring and the machine table

- `kOzzyKeyboardKeyStatePort` (6) is beside `kOzzyKeyboardPort`, and
  `GetOzzyKeyboardSettings` sets it. Static asserts check that the key state's
  ports are in every Ozzy, and clear of the other devices' ports.
- The machine table's keyboard entry takes its first data word from the
  keyboard's settings.

### ozzy and apps: the console frontend

```
// console.h
struct KeyChange {
  int key = 0;        // Its key code.
  bool held = false;
};
struct ConsoleKeys {
  std::vector<uint16_t> typed;     // Typed key events.
  std::vector<KeyChange> changes;  // Keys pressed and released.
};
virtual ConsoleKeys TakeKeys() = 0;  // Was std::vector<uint16_t>.

// win_console.h: no Windows headers, so it can be tested.
int GetKeyCode(uint16_t scan_code, bool extended);
class ConsoleKeyState {
 public:
  void AddKeyEvent(uint16_t scan_code, bool key_down, uint32_t control_keys,
                   std::vector<KeyChange>& changes);
  void ReleaseKeys(std::vector<KeyChange>& changes);
};
```

- `GetKeyCode` is a fixed table of scan codes, which are by place: the main
  rows as strings of their characters from each row's first scan code, F1 to
  F10's range, and the other keys, with and without `ENHANCED_KEY`. The
  extended keys are the arrows and the six keys above them, Right Ctrl, Right
  Alt, and the keypad's Enter and `/`, which share scan codes with the keypad
  and the main keys. The keypad's keys have no code.
- `ConsoleKeyState` keeps the keys reported held. A key down of a key held is
  a repeat, and a key up of a key not held was released before (such as the
  Enter that started `ozzy`), so neither is a change.
- Ctrl and Alt are held as each record's control key state says, rather than
  by their scan codes, as Windows Terminal gives Right Alt's key up without
  `ENHANCED_KEY` (Left Alt's, by its scan code), which would leave it held. The
  control key state is right in both consoles, and is in every record, so a
  Ctrl or Alt pressed or released without focus is caught up on the next key,
  ahead of that key. It has one Shift for both, so Shift is by its scan code.
- `TakeKeys` gives every key event record to `ConsoleKeyState`, the keys that
  quit too, and releases every key held on a `FOCUS_EVENT` losing focus. The
  typed keys are as before.
- `ozzy_main` takes the keys every frame, with or without a keyboard, and
  types each typed key and sets each change.

### Confirmed by hand

With a throwaway viewer that printed each input record, in Windows Terminal and
the old console host:
- Both deliver `FOCUS_EVENT` records, and a key held while the console loses
  focus has no key up. In Windows Terminal, a key released after focus is back
  may still have its key up, which is ignored, as it is already released.
- Scan codes match the table for the letters, Space, Enter, the arrows, Insert,
  both Shifts, both Ctrls, and both Alts. The Windows key is extended 5Bh, so
  it has no code. Auto-repeat is more key downs, and no key ups.
- Windows Terminal gives Right Alt's key up without `ENHANCED_KEY`, and the old
  console with it (see the control key state above).
- The On-Screen Keyboard's keys have their real scan codes in both, so injected
  keys are in key state. Its Ctrl and Alt latch, held until clicked again.
- The Claude Code terminal panel gives each held key as a key down and key up
  pair at every repeat, so a held key is a stream of taps in key state there,
  and Ctrl+Alt+Q doesn't reach the program. `ozzy` is for a Windows console.

Not confirmed: AltGr (no layout with it was at hand), the keypad's Enter (no
keypad), and remapped keys.

### Testing

- `keyboard_test.cc`: the factory's wiring for typed keys only, key state only,
  and both, and `Create`'s errors; holding and releasing keys in the port and
  bit of their codes, with repeats not pressing again and taps pressed but not
  held; programs reading with `IN.RD` and `INS.RD`; a core's write staying
  until the next change; a press between a read and the keyboard taking it;
  out-of-range codes; the keyboard sleeping with nothing to write, its updates
  taking no cycles, and a port released when a core sets its status while the
  keyboard waits for it.
- `ozzy_settings_test.cc` and `ozzy_test.cc`: the keyboard's wiring, key state
  reaching ports 6 and 15 of a running Ozzy, and the machine table's keyboard
  entry.
- `win_console_test.cc`: `GetKeyCode` for each row of the table, the extended
  keys against their keypad and left twins, and keys without a code; and
  `ConsoleKeyState` with records copied from both consoles: repeats, key ups
  of keys not held, Shift by its scan code, Ctrl and Alt by the control key
  state (Windows Terminal's Right Alt too), catching up, and releasing every
  key on losing focus.
