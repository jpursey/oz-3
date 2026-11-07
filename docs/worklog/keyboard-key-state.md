# Keyboard key state

The keyboard gives typed keys: key presses only, as characters, with the
modifier keys held as a mask, and never a modifier key alone. That serves a
program reading text, but not a game, which wants the keys held, and the keys
pressed, by their place on the keyboard. A queue of press and release events
can't give that reliably: a release the program never reads leaves its key
held, and a queue nobody reads fills.

So the keyboard gains **key state**: ports holding a bit for each key held, and
for each key pressed since the port was last read. It is state rather than
events, so nothing is lost when a program doesn't read it, and nothing waits to
be read. A keyboard's wiring chooses typed keys, key state, or both, and the
Ozzy's keyboard has both. Memory mapping would take a whole page (4 Kiw) for 10
words, so key state is in ports (see the backlog's *Shared I/O page*).

## Behavior

### Devices: the keyboard

**Wiring:** typed keys are a port and an interrupt, as now. Key state is
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
  its pressed keys: the keyboard clears them from word 1 and the status, keeping
  any pressed since. So a program reads the dword, `IN.RD` (or `INS.RD`, which
  reads only when a key on the port was pressed), and gets both at once.
  Reading word 0 alone (`IN.RW`) takes the pressed keys without seeing them.
- A key pressed and released between two reads is pressed but not held, so a
  tap is never missed. A key held down presses once: the host's auto-repeat
  doesn't press it again.
- Each port has its own lock, so a program reading the ports while the keyboard
  updates them may see some ports from before a change and some from after.
  Every key the host reports in one frame changes at once on its own port.
- The keyboard writes a port only when one of its keys changes, or it is read
  with keys pressed, with no cycles of its own, so it waits only while a core
  holds the port. A core that writes to a key state port changes it until the
  keyboard next writes it, and a write that sets the status makes `INS` read
  it.

The typed keys don't change for programs. The host still translates every
typed key itself, by its own layout, and types it: the keyboard never makes
typed keys from key state. A host reporting a key to a keyboard with both calls
both, `TypeKey` for the character it types (if any) and `SetKeyState` for its
code (if it has one).

**Host interface:**

```
// Key codes for the modifier keys, only in key state (typed keys hold the
// modifiers as bits, kKeyShift, kKeyCtrl, and kKeyAlt).
inline constexpr uint8_t kKeyLeftShift = 0x89;  ...  kKeyRightAlt = 0x8E;

struct KeyboardSettings {
  // The first of Keyboard::kKeyStatePorts ports of key state, or none for a
  // keyboard without key state.
  std::optional<int> state_port;

  // The typed keys' port, or none for a keyboard without typed keys.
  std::optional<int> typed_port = 0;

  // The interrupt raised when a typed key is put in the port, which a keyboard
  // without typed keys doesn't have.
  DeviceInterrupt typed_interrupt;
};

class Keyboard {
 public:
  static constexpr int kKeyStatePorts = 10;
  static constexpr int kKeyStateKeys = kKeyStatePorts * 16;

  // Queues the typed key event `key` (was PressKey). Does nothing, returning
  // false, for a keyboard without typed keys.
  bool TypeKey(uint16_t key);

  // Sets whether the key with code `key` is held, which must be less than
  // kKeyStateKeys. A key that wasn't held is pressed too. Does nothing for a
  // keyboard without key state. Called between Processor::Execute calls.
  void SetKeyState(int key, bool held);
};
```

`port` and `interrupt` become `typed_port` and `typed_interrupt`, so each
field says which keys it is for, and `typed_port` stays 0 by default, so a
default keyboard is as it was.
`PressKey` becomes `TypeKey`, now that key state has presses of its own, which
renames its calls in `ozzy_main.cc` and the ozzy tests.

**Wiki:** `3.2-Keyboard.md` gains the key state's wiring, reading, and key
codes, with the existing reading and key events under typed keys, and
`3-Devices.md`'s table gives the keyboard's ports as 1 and 10.

### Ozzy

- The Ozzy's keyboard has key state on ports 6 to 15, the end of the ports
  kept for devices (0 to 15), leaving 0, 3, 4, and 5 for devices to come.
  `{ type: "keyboard" }` is unchanged, and has both.
- The machine table's keyboard entry gives the first key state port in its
  first data word (FFFFh for none), and its second stays 0. Version 1 is
  unchanged: the word was 0, and no program reads it yet.
- The console frontend reports every key's presses and releases to the key
  state, by where the key is (its scan code), and releases them all when the
  console loses focus, which sends no releases for keys held then.
- **Wiki:** `4-Ozzy-Computer.md`'s device table, machine table, its example,
  and the console frontend's keyboard.

## Design

### Names

- **Typed keys** is the existing port and its key events, and **key state** the
  new ports. Alternatives: "key matrix" (suggests the scanning hardware of old
  keyboards, which this isn't), "held keys" (only word 0). Recommended: key
  state. "Key event" stays the typed key's word.
- **`SetKeyState(key, held)`**, as the feature is key state. Set aside:
  `SetKeyHeld` (reads as always holding it), `HoldKey` and `ReleaseKey`, and
  `KeyDown` and `KeyUp` (not verbs).
- **`TypeKey`**, renamed from `PressKey`, as "press" now means a key going
  down in key state, and the typed keys are typed.
- **`kKeyLeftShift` to `kKeyRightAlt`**: key codes, beside `kKeyShift`,
  `kKeyCtrl`, and `kKeyAlt`, which are the typed key event's modifier bits.
  The names are close, so the comments say which is which. Renaming the bits
  (such as `kKeyShiftBit`) would be clearer, but changes every use of them;
  recommended to leave them.
- **`state_port`**, **`typed_port`**, and **`typed_interrupt`** in
  `KeyboardSettings`, with the two ports together, and `kKeyStatePorts` and
  `kKeyStateKeys` in `Keyboard`.
- **`KeyChange`** (`{key, held}`) and **`ConsoleKeys`** (`typed` and
  `changes`) in the console frontend.
- **`GetKeyEvent`**, renamed from `GetOzzyKey`, and **`GetKeyCode`**: they give
  the keyboard device's typed key events and key codes, which aren't the
  Ozzy's own. They stay in `win_console`, the only Windows host.

### devices: Keyboard

- `KeyboardFactory` wires the typed port (if any) and then the key state ports
  (if any), in that order, and the interrupt only with the typed port. Its
  settings can now be invalid, so it is made by a static `Create`, as the
  teletype's and screen's are, which fails for a keyboard with neither typed
  keys nor key state, or with the typed keys' port among the key state's
  (devices may share ports, so `ProcessorConfig::Validate` wouldn't catch it).
  Port ranges are checked by `ProcessorConfig::Validate`, as now. The Ozzy
  creates its keyboard with `Create`. Like the teletype, the factory keeps its
  settings, and the keyboard is constructed from them.
- The keyboard keeps, for each key state port: the keys held, the keys pressed
  and not yet taken, the pressed keys it last put in word 1, and whether the
  port needs writing.
  - `SetKeyState` sets or clears the held bit, and on a press of a key not held,
    its pressed bit, marking the port to write, and wakes the keyboard.
  - `Execute` requests the lock of every port to write, and of every port it
    put pressed keys in whose status reads clear (a core read it). As each is
    granted, it checks the status under the lock: if clear, the pressed keys it
    put there were taken, so it removes only those, keeping any the host
    pressed since. Then it writes word 0 and word 1 with no status mode, sets
    the status if word 1 isn't 0 (`StoreWord` with `S`, at word 1), and
    releases the lock. With nothing left to write, it sleeps.
  - Reads and writes of its ports wake it (`Port` wakes the components wired
    to a port), which is how it learns of a read. A woken keyboard with nothing
    to write sleeps again at once.
- Typed keys are unchanged but for `TypeKey`'s name, and use the typed port's
  index in the config, which is 0 whenever there is one.
- Performance: a game polling 10 ports each frame wakes the keyboard 10 times a
  frame, each a check of 10 ports.

**Brittleness:** a press the host makes while a core's read is waiting to be
taken must survive it, which is why the keyboard removes only the pressed keys
it put in word 1, rather than clearing them all. A test presses a key between
a read and the keyboard taking it.

### ozzy: wiring and the machine table

- `kOzzyKeyboardKeyStatePort = 6` beside `kOzzyKeyboardPort`, and
  `GetOzzyKeyboardSettings` sets it.
- `kMachineDeviceData` is the keyboard's first key state port.
- The boot ROM doesn't change, as it reads only typed keys.

### ozzy and apps: the console frontend

```
// A key pressed or released, by its key code (see keyboard.h).
struct KeyChange {
  int key = 0;
  bool held = false;
};

// The keys a console took since the last call, oldest first.
struct ConsoleKeys {
  std::vector<uint16_t> typed;     // Typed key events
  std::vector<KeyChange> changes;  // Key state changes
};

virtual ConsoleKeys TakeKeys() = 0;  // Replaces std::vector<uint16_t>.
```

- `GetKeyCode(scan_code, extended)` in `win_console.h` gives a key's code
  from the key event record's scan code, and whether it is an extended key (`E0`
  prefixed: the arrows and the six keys above them, Right Ctrl, Right Alt, and
  the keypad's Enter and `/`, which share scan codes with the keypad and the
  main keys), or 0 for a key
  without one. It is a fixed table of scan codes, which are by place, so it
  needs no Windows calls and is tested as `GetKeyEvent` is.
- `GetOzzyKey` becomes `GetKeyEvent`, unchanged otherwise.
- `TakeKeys` gives a change for each key down and key up record with a code,
  which `GetKeyEvent` and the quit keys don't affect. It keeps which keys it
  reported held, and on losing focus, releases them.
- `ozzy_main` types each typed key and sets each change.

### To confirm

- That the console delivers `FOCUS_EVENT` records when it loses focus, and that
  keys held then have no key up, in Windows Terminal and the old console host
  (CL3, by hand).
- That AltGr arrives as Left Ctrl and Right Alt, so it holds both in key state,
  as it does in Windows itself (CL3, by hand). That would be left as it is.
- Which `wVirtualScanCode` the console gives an injected or remapped key, which
  may be 0, and so no key state (CL3, by hand).

## CLs

### CL1 [x] devices: key state

Depends on: nothing.

- `keyboard.h` and `keyboard.cc`: the key codes, `KeyboardSettings`'s optional
  `state_port` and `typed_port`, and `typed_interrupt`, `KeyboardFactory::Create`,
  `kKeyStatePorts`, `kKeyStateKeys`, and
  `SetKeyState`, and the key state in `Execute`. `PressKey` becomes `TypeKey`,
  with its calls in `ozzy_main.cc`, `boot_rom_test.cc`, and
  `console_display_test.cc`.
- `keyboard_test.cc`: key state tests, beside the typed key tests.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests:
  - the factory's wiring: typed keys only, key state only, and both;
  - holding and releasing keys sets and clears word 0, in the port and bit of
    its code, the cycle after;
  - a press sets word 1 and the status, a held key's repeats don't press
    again, and a press and release before a read is pressed but not held;
  - a program reading with `IN.RD` gets both words and takes the pressed keys,
    keeping the held, and `INS.RD` reads only with keys pressed;
  - a press between a core's read and the keyboard taking it isn't lost;
  - typed keys and key state work together, and each alone;
  - the keyboard sleeps with nothing to write, and its updates take no cycles.
- Wiki: `3.2-Keyboard.md` and `3-Devices.md`.

### CL2 [x] ozzy: key state on ports 6 to 15

Depends on: CL1.

- `ozzy_settings.h` and `.cc`: `kOzzyKeyboardKeyStatePort`, in the keyboard's
  settings.
- `ozzy.cc`: the keyboard entry's first data word.
- Tests in `ozzy_settings_test.cc` and `ozzy_test.cc`.

**Verify**
- Standard checks.
- Unit tests: the settings' wiring, and the machine table's keyboard entry with
  and without a keyboard.
- Wiki: `4-Ozzy-Computer.md`'s devices table, machine table, and example.

### CL3 [ ] ozzy, apps: key state from the console

Depends on: CL2.

- `console.h`: `KeyChange`, `ConsoleKeys`, and `TakeKeys`.
- `win_console.h` and `.cc`: `GetKeyCode`, `GetOzzyKey` renamed to
  `GetKeyEvent`, and `TakeKeys`'s changes and focus.
- `ozzy_main.cc`: sets the changes.
- Tests in `win_console_test.cc`.

**Verify**
- Standard checks.
- Unit tests: `GetKeyCode` for each row of the table, the extended keys
  against their keypad and left twins, and keys without a code.
- By hand: run `ozzy`, and confirm the To confirm items. Nothing in the Ozzy
  reads key state yet, so a small test program, or the debugger later, is
  needed to see it.
- Wiki: `4-Ozzy-Computer.md`'s console frontend.
