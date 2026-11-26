# App Structure

The terminal app has grown inside `ozzy_main.cc`, each piece added where it
fit: reading the config, powering on and off, the frame loop and its pacing,
waiting while the machine is off, and the stats. This gives it the structure
[ozzy_design.md](../ozzy_design.md) describes (The machine, its state, and the
panel), in the `ozzy` library, where it is tested:
- **The machine** is the `Ozzy`, as it is now.
- **The machine's state** is an `OzzyHost`, which outlives the machine: the
  settings, the context the Ozzy is created in, and whether it is on. It
  powers the Ozzy on and off, runs it, and passes on what a program asked the
  power port for.
- **How it is presented** is a `ConsoleApp`, a `gb::Game`, which runs an
  `OzzyHost` in a `Console` frame by frame: keys to the keyboard, the Ozzy run
  for the frame's time, the display updated, and what to do when the machine
  powers off. Tests run it against a fake `Console` and a `gb::FakeClock`, so
  they run frames without real time.

`ozzy_main.cc` is left with the command line: it reads the config, creates
the console, runs a `ConsoleApp`, and writes any error and the stats once the
console is given back.

The *Teletype view*, *Panel*, and *Headless ozzyterm* build on this, as the
*Window app* does later, which presents the same `OzzyHost` in a window.

## Behavior

There is no change in what the `ozzy` app does, apart from two small
differences in pacing that come from `gb::Game`'s loop:
- A frame that ends more than a millisecond late starts the next at once, as
  now, but one that ends less than a millisecond late is absorbed into the next
  frame, rather than starting it at once.
- With `--max-speed`, a frame runs the Ozzy for 1/60 s of host time and then
  draws, so a frame is 1/60 s plus the draw, rather than 1/60 s in all. The
  draw is usually well under a millisecond in a terminal, so this is a few
  percent fewer frames a second at most.

## Design

### Names

- **`OzzyHost`**: the machine's state, the host's side of an Ozzy. Other
  candidates: `OzzyState`, which collides with `gb::GameState` and the
  *Panel*'s states, and `OzzyMachine`, which collides with "the machine", the
  `Ozzy` itself, and with the machine table. Recommended: `OzzyHost`, as
  ozzy_design.md calls the state "the host's". "Host" already means the
  computer running the emulator (the host's clock, time, and file system), and
  this is that host's side of the Ozzy, so the two meanings agree.
- **`ConsoleApp`**: the terminal app. Other candidates: `TerminalApp`, and
  `OzzytermApp`, which names an app that doesn't exist yet. Recommended:
  `ConsoleApp`, beside `Console` and `ConsoleDisplay`, which it runs.
- **`ConsoleAppStats`**: what `--stats` writes, as `RunStats` is now.

### OzzyHost (ozzy)

```
// The host's side of an Ozzy: its settings, the context it is created in,
// and whether it is on. The Ozzy exists only while it is on.
class OzzyHost final {
 public:
  // The file system the drives' images are opened in, which must outlive
  // the host (see Ozzy::kConstraintFileSystem).
  using Contract = gb::ContextContract<Ozzy::kConstraintFileSystem>;

  // Creates a host for an Ozzy with `settings`, which is off.
  OzzyHost(OzzySettings settings, Contract contract);

  // Destroying the host powers the Ozzy off.
  ~OzzyHost();

  const OzzySettings& GetSettings() const;

  // Returns the Ozzy, or null while it is off.
  Ozzy* GetOzzy();
  bool IsOn() const;

  // Powers the Ozzy on, creating it from the settings, if it is off. Returns
  // the error if it can't be created, and stays off.
  absl::Status PowerOn();

  // Powers the Ozzy off, if it is on, which commits each drive as at a clean
  // shutdown.
  void PowerOff();

  // Runs the Ozzy for `time` (see Ozzy::Run), which must be on, and returns
  // the cycles run, and the first request a program made on the power port
  // during the run, if any. Later requests in the same run are dropped. The
  // Ozzy is still on: what a request does is up to the app.
  struct RunResult {
    Cycles cycles = 0;
    std::optional<PowerRequest> power_request;
  };
  RunResult Run(absl::Duration time);
};
```

- It lives in the `ozzy` library, as both apps present the same state.
- It owns the context the Ozzy is created in, which outlives each Ozzy as the
  drives write to their images as it is destroyed: the file system, and the
  power callback, which notes the run's first request. The host's clock is
  left out, so programs see the host's local time, as now.
- A restart is `PowerOff()` then `PowerOn()`, so RAM is cleared and every
  device starts as at power on. There is no `Restart()`, as the app has to
  handle the Ozzy changing anyway (see Brittleness).
- Later, the *Panel* adds the disk in each drive to it, which outlives a
  restart.

**Brittleness:** the callback is called from inside `Ozzy::Run`, so it mustn't
destroy the Ozzy. The host owns the callback, and only notes the request in
it, so no caller can get this wrong. Each power on creates a new `Ozzy`, so
anything that holds one of its devices (such as a `ConsoleDisplay`) must be
made again after each power on. `ConsoleApp` keeps this in one private
function that powers on and makes the display together. Comparing `GetOzzy()`
pointers would not do, as a new Ozzy can be at the old one's address.

### ConsoleApp (ozzy)

```
struct ConsoleAppOptions {
  OzzySettings settings;

  // Runs the Ozzy as fast as the host can between frames (--max-speed).
  bool max_speed = false;
};

// How an Ozzy ran, from its first power on, through each restart, to its last
// power off (what --stats writes).
struct ConsoleAppStats {
  int64_t frames = 0;
  absl::Duration time;        // From each first frame's start to its last's end.
  absl::Duration frame_time;  // The time frames took, without waiting.
  Cycles cycles = 0;
};

// Returns the text --stats writes for `stats`.
std::string FormatConsoleAppStats(const ConsoleAppStats& stats);

// Runs an Ozzy in a Console, until the user quits.
class ConsoleApp final : public gb::Game {
 public:
  // `console` and `file_system` must outlive the app. The console is in the
  // mode GetOzzyConsoleMode gives for the settings.
  ConsoleApp(ConsoleAppOptions options, Console* console,
             gb::FileSystem* file_system);

  // The error that stopped the app, if Run returned false: the Ozzy failed
  // to be created at a power on.
  const absl::Status& GetError() const;

  const ConsoleAppStats& GetStats() const;

 protected:
  bool Init(const std::vector<std::string_view>& args) override;
  bool Update(absl::Duration delta_time) override;
  void CleanUp() override;
};
```

- `main` runs it from the command's run function, with no arguments, as
  `gb::RunCommandLine` has already parsed them: `Game::Run` only returns
  whether it succeeded, so parsing in `Init` would lose `gb_cli`'s exit codes
  for help (0) and a usage error (2). Tests run it with a context holding a
  `gb::FakeClock` (`Game::kConstraintClock`).
- `Init` turns off the screen's pixels in the settings (the console shows its
  cells), creates the `OzzyHost`, and powers it on. `CleanUp` powers it off.
- Each `Update`, while the Ozzy is on: takes the console's keys to the
  keyboard (and drops them without one), runs the Ozzy for the frame's
  `delta_time`, at most 100 ms, and updates the display. With `max_speed`, it
  runs the Ozzy in runs of 10,000 cycles until 1/60 s after the frame
  started, by the game's clock. A power request stops the frame without
  drawing it: a restart powers off and on, and a shutdown powers off and shows
  `ShowOzzyShutDown`. While the Ozzy is off, it only drops keys. Every frame
  returns false once the console's quit is requested, and the app keeps the
  stats as it goes.
- The frame rate is `gb::Game`'s default, 60 frames a second.
- The app's states are only on and off, so it has no `gb::GameStateMachine`.
  The *Panel* may need one, and adds it then.
- It is in the `ozzy` library, which gains `gb_game` as a dependency, as the
  backlog item planned. `gb_game` uses `gb_base` and `gb_pilot`, which uses
  `gb_image` (and `stb` through it), and no SDL or Vulkan.
- The fake console in `console_display_test.cc` moves to `ozzy_testing.h` as
  `FakeConsole`, so both tests use it, gaining keys a test gives it and a quit
  it requests after a number of frames (`TakeKeys` calls), so a test's
  `Run` ends.

**Brittleness:** with `max_speed`, `Update` runs until the clock reaches its
deadline, so a `FakeClock` must advance on its own (`SetAutoAdvance`), or the
frame never ends. `gb::Game`'s loop needs the same of a fake clock for its own
waits, so the tests set it once in their fixture.

### ozzy_main.cc (apps)

- Keeps the command line, and reads the config through a new
  `ReadOzzySettingsFile(gb::FileSystem&, std::string_view path)` in
  `ozzy_settings.h`, which reads the file and its settings, with the error
  for either. It is tested in `ozzy_settings_test.cc` with the file system's
  memory protocol.
- Creates the console in the settings' mode, runs a `ConsoleApp`, gives the
  console back, then writes the app's error (exit code 1) and, with `--stats`,
  its stats.

## CLs

### CL1 [ ] ozzy, apps: OzzyHost, the machine's state

Depends on: nothing.

- `ozzy_host.h`/`.cc`: `OzzyHost` as above.
- `ozzy_main.cc` runs its Ozzy through an `OzzyHost`, in place of its own
  context, power callback, and `Ozzy::Create`, so the host isn't unused. The
  frame loop is otherwise as it is.

**Verify**
- Standard checks (see CLAUDE.md).
- `ozzy_host_test.cc`: off until `PowerOn`, and off again after `PowerOff`;
  a `PowerOn` that fails (an image that doesn't exist) returns the error and
  stays off; `Run` returns the cycles, and a program's shutdown with its exit
  code, and its restart; only the first request in a run is returned; a
  power off commits a drive's image, and so does destroying the host while on.

### CL2 [ ] ozzy, apps: ConsoleApp on gb::Game

Depends on: CL1.

- `console_app.h`/`.cc`: `ConsoleApp`, `ConsoleAppOptions`,
  `ConsoleAppStats`, and `FormatConsoleAppStats`, as above. `oz3_ozzy` gains
  `gb_game`.
- `ozzy_settings.h`/`.cc`: `ReadOzzySettingsFile`.
- `ozzy_testing.h`/`.cc`: `FakeConsole`, moved from `console_display_test.cc`.
- `ozzy_main.cc` is left with the command line, as above.
- The backlog's *Teletype view*, *Panel*, and *Headless ozzyterm* are
  updated to name `OzzyHost` and `ConsoleApp` where they describe the code.

**Verify**
- Standard checks.
- `console_app_test.cc`, with a `FakeConsole` and a `gb::FakeClock`: the boot
  ROM's banner is drawn; typed keys reach the keyboard; a frame runs the
  Ozzy for its time, at most 100 ms; `max_speed` runs it for 1/60 s of clock
  time a frame; a program's restart starts it again, with a new display, and a
  shutdown shows the message and keeps the app running, off, until quit; a
  power on that fails stops the app with the error; the stats count frames,
  time, and cycles across a restart.
- `FormatConsoleAppStats`, including no frames.
- `ReadOzzySettingsFile`: a file that reads, one that doesn't exist, and one
  with errors.
- By hand (left for the user): `ozzy` with `configs/console.ozzy` and
  `configs/teletype.ozzy`, with and without `--max-speed` and `--stats`, and
  a program's restart and shutdown, look as they did.
