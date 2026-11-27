# App Structure

The terminal app had grown inside `ozzy_main.cc`, each piece added where it
fit: the command line, reading the config, powering on and off, the frame loop
and its pacing, waiting while the machine is off, and the stats. This gave it
the structure [ozzy_design.md](../ozzy_design.md) describes (The machine, its
state, and the panel), shared by both apps and tested:
- **The machine** is the `Ozzy`, as it was.
- **The machine's state** is an `OzzyHost`, which outlives the machine: the
  settings, the context the Ozzy is created in, and whether it is on.
- **The app** is an `OzzyApp`, a `gb::Game`, which is the whole of an app's
  logic, whichever app it is: its command line, reading the config, the
  `OzzyHost`, what powering off does, the frame loop, and the stats, and later
  the *Panel*'s commands. It is the same harness for `ozzyterm` and `ozzy`.
- **How it is presented** is a `Frontend`, which the app creates through the
  `FrontendType` in its context: what the user sees of the machine, and the
  keys and quit the user gives it. `ozzyterm`'s is a `TerminalFrontend`, built
  from the `Console` and `ConsoleDisplay` there were, and the *Window app*
  adds a window's.

So `ozzyterm_main.cc` only puts the file system and the console's frontend type in
a context, runs an `OzzyApp`, and returns the exit code the app leaves in the
context. Everything else is tested: the command line too, against a fake
frontend, a memory file system, and a `gb::FakeClock`.

`oz3/ozzy` is split into a directory and a library for each part, as it will
grow with the *Panel*, the *Window app*, and the debugger. The terminal app,
which was `ozzy`, is `ozzyterm`, so `ozzy` is the *Window app*'s from the
start.

The manual's [Ozzy Computer](../manual/4-Ozzy-Computer.md) page (Running
Ozzy) is the specification of what the app does. This summary covers how it
is built, and the decisions behind it.

## Behavior

Each frame runs the Ozzy for exactly the time that passed since the last, as
`gb::Game` gives it, at most 100 ms as before, so a host that falls behind
slows the Ozzy rather than racing to catch up. What the terminal app does is
otherwise unchanged, apart from its name, and:
- **`--max-speed` is gone.** It ran the Ozzy faster than real time, as many
  cycles as fit in each frame, which is the one thing that doesn't fit
  `gb::Game`'s model of running the time that passed, and nothing needs it.
  Raising a config's `clock_hz` until `--stats` shows the Ozzy falling behind
  measures the host's top speed much as it did. The manual's Ozzy Computer
  page, and ozzy_design.md (Pacing), change to match.
- A frame that ends less than a millisecond late is absorbed into the next
  frame, rather than starting it at once, as `gb::Game`'s loop does. One more
  than a millisecond late starts the next at once, as before.
- **Nothing that fails takes over the console.** A config with errors, a
  frontend's flags that are wrong, or an Ozzy that can't power on (such as a
  disk image that doesn't exist) is written to the console as it was, as the
  frontend is created only once the Ozzy is on.

The exit codes stay as `gb::RunCommandLine` gives them: 0 when it ran (or
wrote help), 1 when it failed, and 2 for a mistake on the command line.

## Design

### Directories and libraries

`oz3/ozzy` holds only a directory for each part, each its own library, and
each depends only on the ones before it:

| Directory          | Library            | What                                      |
| ------------------ | ------------------ | ----------------------------------------- |
| `oz3/ozzy/machine` | `oz3_ozzy_machine` | The machine: `Ozzy`, its settings, the machine table, the boot ROM, and the loader |
| `oz3/ozzy/app`     | `oz3_ozzy_app`     | What both apps share: `OzzyHost`, `OzzyApp`, `Frontend`, and later the *Panel*'s commands |
| `oz3/ozzy/term`    | `oz3_ozzy_term`    | `ozzyterm`'s frontend: `Console` (per platform), the terminal's keys, the console's text, `ConsoleDisplay`, and `TerminalFrontend` |

The machine is the part a host other than the two apps would embed, such as
a game with an Ozzy in it, which links `oz3_ozzy_machine` alone. Other
candidates for its directory: `vm`, which is the emulator as a whole rather
than the computer it emulates, and `hardware`, which leaves out the boot ROM
and the loader, as they are software. Recommended: `machine`, as
ozzy_design.md calls it "the machine".

The *Window app* adds `oz3/ozzy/window` (`oz3_ozzy_window`), the only one that
links SDL and Vulkan. `oz3_ozzy_testing`, in `oz3/ozzy/machine`, holds
`ozzy_testing.h` (an Ozzy with a power callback, programs assembled for it),
which the tests of all three share, as `oz3_core_testing` and `oz3_disk_testing` do for theirs. CLAUDE.md's
list of libraries and their order changes to match.

Headers are included by their path (`"oz3/ozzy/machine/ozzy.h"`,
`"oz3/ozzy/term/console.h"`), and their include guards name it
(`OZ3_OZZY_TERM_CONSOLE_H_`).

### Names

- **`OzzyHost`**: the machine's state, the host's side of an Ozzy. Other
  candidates: `OzzyState`, which collides with `gb::GameState` and the
  *Panel*'s states, and `OzzyMachine`, which collides with "the machine", the
  `Ozzy` itself, and with the machine table. Recommended: `OzzyHost`, as
  ozzy_design.md calls the state "the host's". "Host" already means the
  computer running the emulator (its clock, time, and file system), and this
  is that host's side of the Ozzy, so the two meanings agree.
- **`OzzyApp`**: the harness both apps run. Other candidates: `OzzyGame`,
  which it isn't, and `OzzyRunner`. Recommended: `OzzyApp`.
- **`Frontend`**, **`FrontendType`**, and **`FrontendFactory`**: how an app is
  presented, the kind of frontend an app has (which sets one up for a command
  line), and what creates it once set up. Other candidates: `OzzyView`, which collides with the *Teletype
  view*, and `Presenter`. Recommended: `Frontend`, which CLAUDE.md already
  uses for the console code ("the console frontend the `ozzy` app is built
  from"). `TerminalFrontend` is `ozzyterm`'s, and `WindowFrontend` the window
  app's.
- **`FrontendKeys`**: what `ConsoleKeys` was, as both frontends give keys
  the same way. `KeyChange` keeps its name.
- **`OzzyAppStats`**: what `--stats` writes, as `RunStats` was.
- **`ozzyterm`**: the terminal app, as ozzy_design.md names it, so `ozzy` is
  the window app's alone.

### OzzyHost (ozzy/app)

```
// The host's side of an Ozzy: its settings, the context it is created in,
// and whether it is on. The Ozzy exists only while it is on.
class OzzyHost final {
 public:
  // The host puts its power request callback in its context while it lives,
  // for each Ozzy it creates.
  static GB_CONTEXT_CONSTRAINT(kConstraintPowerRequestCallback, kScoped,
                               PowerRequestCallback);

  // Each Ozzy is created in the host's context, so it holds the rest of what
  // the Ozzy needs from the host (see Ozzy::Contract), such as the file
  // system.
  using Contract = gb::ContextContract<kConstraintPowerRequestCallback>;

  // Creates a host for an Ozzy with `settings`, which is off, or returns null
  // if the contract is invalid.
  static std::unique_ptr<OzzyHost> Create(OzzySettings settings,
                                          Contract contract);

  // Destroying the host powers the Ozzy off.
  ~OzzyHost();

  // Returns the Ozzy, or null while it is off.
  Ozzy* GetOzzy();
  bool IsOn() const;

  // Powers the Ozzy on, creating it from the settings, if it is off. Returns
  // the error if it can't be created (see Ozzy::Create), such as a context
  // without what it needs, and stays off.
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

- It keeps its `ValidatedContext`, which each Ozzy is created in, and puts the
  power callback in it as a `kScoped` value, so it is cleared when the host is
  destroyed. The callback notes the run's first request. The context outlives
  each Ozzy, as the drives write to their images as it is destroyed. The
  host's clock is left out of it, so programs see the host's local time, as
  now. `OzzyHost` passes
  the context on without declaring `Ozzy`'s constraints, as the Ozzy checks
  them itself.
- A restart is `PowerOff()` then `PowerOn()`, so RAM is cleared and every
  device starts as at power on.
- Later, the *Panel* adds the disk in each drive to it, which outlives a
  restart.

**Brittleness:** the callback is called from inside `Ozzy::Run`, so it mustn't
destroy the Ozzy. The host owns the callback, and only notes the request in
it, so no caller can get this wrong.

### Frontend (ozzy/app)

```
// The keys a frontend took since they were last taken, each oldest first.
struct FrontendKeys {
  std::vector<uint16_t> typed;     // Typed key events (see keyboard.h).
  std::vector<KeyChange> changes;  // Keys pressed and released.
};

// How an OzzyApp is presented: what the user sees of the machine, and the
// keys and quit the user gives it. It decides nothing about the machine,
// which is the app's.
class Frontend {
 public:
  virtual ~Frontend() = default;

  // Input.
  virtual FrontendKeys TakeKeys() = 0;
  virtual bool IsQuitRequested() const = 0;

  // Operations. Shows what changed since the last call, such as the attached
  // Ozzy. The app must call it every frame, whether or not an Ozzy is
  // attached.
  virtual void Update() = 0;

  // Attached Ozzy. Attach shows `ozzy`, and may hold its devices, until
  // Detach, which first shows what changed since the last Update. Each must
  // only be called while no Ozzy is attached, and while one is,
  // respectively, and `ozzy` must stay alive until Detach returns. With
  // `exit_code`, Detach shows that the Ozzy shut down with it, until the next
  // Attach.
  virtual void Attach(Ozzy* ozzy) = 0;
  virtual void Detach(std::optional<int> exit_code) = 0;
};

// Creates an app's frontend, as FrontendType::Configure set it up to.
class FrontendFactory {
 public:
  virtual ~FrontendFactory() = default;

  // Creates the frontend, taking over what it shows the Ozzy in (such as the
  // console). Returns an error if it can't (such as no console to take over),
  // which the app writes.
  virtual absl::StatusOr<std::unique_ptr<Frontend>> Create() = 0;
};

// How an app is presented: it gives the app the frontend's part of its command
// line, and sets up the frontend for the command line the user gave. An app's
// main puts one in the OzzyApp's context (OzzyApp::kConstraintFrontendType).
class FrontendType {
 public:
  virtual ~FrontendType() = default;

  // The app's name and help on its command line, which say how it is
  // presented ("Runs an Ozzy computer in the console, ...").
  virtual std::string_view GetProgramName() const = 0;
  virtual std::string_view GetProgramHelp() const = 0;

  // The frontend's own flags, which the app adds to its command line after
  // its own. The array stays as it is for the type's life, and each flag
  // has static storage, as gb_cli's definitions do.
  virtual absl::Span<const gb::CliAnyFlag> GetFlags() const = 0;

  // Sets up the frontend for the Ozzys with `settings`, reading its flags
  // from `command_line`, and returns the factory that creates it. It changes
  // the settings the frontend decides, such as whether the screen draws its
  // pixels (OzzyScreenSettings::draw_pixels), and takes nothing over. Returns
  // an error if the frontend can't show these Ozzys, or its flags are wrong,
  // which the app writes.
  virtual absl::StatusOr<std::unique_ptr<FrontendFactory>> Configure(
      OzzySettings* settings, const gb::CliCommandLine& command_line) = 0;
};
```

- `Attach` and `Detach` say only which Ozzy is shown, not why: the frontend
  doesn't care whether the Ozzy just booted or is about to restart. The app
  attaches each Ozzy as it powers on and detaches it before it powers off, in
  one place each, beside `OzzyHost::PowerOn` and `PowerOff`, so a frontend
  that holds its devices (as `ConsoleDisplay` does) always lets them go before
  the Ozzy is destroyed, and starts afresh for a new one.
- `Update` isn't tied to an Ozzy: while it is off, a frontend still has things
  to show, such as the *Panel*.
- **The command line** is the app's, made at run time from the frontend's
  part and the app's own: the type's name, help, and flags, then the app's
  flags and arguments (`--stats`, the config file, and later others, such as a
  disk to start with), and `OzzyApp::RunCommand` to run it. So a frontend
  lists only its own flags, and an app flag is added in one place. A
  `CliCommand`'s lists are fixed where it is written, so this needs Game Bits'
  *Command line flags added at run time*, which adds a span of flags given at
  run time beside them.
- The keys are the same in both apps: typed key events with their modifiers,
  and key changes, by key code. The window app makes them from SDL's events,
  as the console does from its own.
- **The *Panel*** fits in later as the app's, with only its drawing in the
  frontend: the panel's state (open or not, the machine's status, its
  messages, and the command line being typed) and its commands are in
  `oz3_ozzy_app`, and Ctrl+Alt+P and the keys typed while it is open are the
  app's to take from `TakeKeys`. The frontend gains one call to show the
  panel's state, which the console draws as text and the window over the
  picture. So the panel works the same in both apps, and is tested once, in
  the app.
- A fake frontend in `oz3_ozzy_app`'s tests records the calls and the Ozzy it
  shows, and gives keys and a quit a test sets, such as after a number of
  frames, so a test's `Run` ends.

### OzzyApp (ozzy/app)

```
// Runs an Ozzy until the user quits, presented by the frontend of its
// context's frontend type.
class OzzyApp final : public gb::Game {
 public:
  //----------------------------------------------------------------------------
  // Contract constraints
  //----------------------------------------------------------------------------

  // REQUIRED: How the app is presented.
  static GB_CONTEXT_CONSTRAINT(kConstraintFrontendType, kInRequired,
                               FrontendType);

  // REQUIRED: The clock gb::Game runs the frames by (its optional
  // kConstraintClock), which the app times them by too.
  static GB_CONTEXT_CONSTRAINT(kConstraintGameClock, kInRequired, gb::Clock);

  // OUTPUT: The app's exit code, which Run leaves in the context for main to
  // return: as gb::RunCommandLine returns, or 1 if the app failed once
  // started.
  static inline constexpr const char* kKeyExitCode = "ozzy_exit_code";
  static GB_CONTEXT_CONSTRAINT_NAMED(kConstraintExitCode, kOutRequired, int,
                                     kKeyExitCode);

  // SCOPED: The app, while it runs, which RunCommand finds it by.
  static GB_CONTEXT_CONSTRAINT(kConstraintOzzyApp, kScoped, OzzyApp);

  // What the app reads and writes: the above, and gb_cli's file system
  // (gb::kConstraintCliFileSystem), which the config file and the disk
  // images are opened in, and writer (gb::kConstraintCliWriter), which the
  // app's errors and stats go to as gb_cli's do.
  using Contract = gb::DerivedContextContract<
      GameContract, kConstraintGameClock, kConstraintFrontendType,
      kConstraintExitCode, kConstraintOzzyApp, gb::kConstraintCliFileSystem,
      gb::kConstraintCliWriter>;

  gb::ValidatedContext& Context() override { return context_; }

 protected:
  bool Init(const std::vector<std::string_view>& args) override;
  bool Update(absl::Duration delta_time) override;
  void CleanUp() override;
};

// How an Ozzy ran, from its first power on, through each restart, to its last
// power off (what --stats writes).
struct OzzyAppStats {
  int64_t frames = 0;         // The frames that ran the Ozzy.
  absl::Duration time;        // Each frame's time since the frame before it.
  absl::Duration frame_time;  // The time frames took, without waiting.
  Cycles cycles = 0;
};
```

- **`main`** is all that isn't tested:
  ```
  int main(int argc, char* argv[]) {
    auto context = std::make_shared<gb::Context>(
        gb::ContextBuilder()
            .SetPtr<gb::Clock>(gb::RealtimeClock::GetClock())
            .SetOwned<gb::FileSystem>(gb::CreateCliFileSystem())
            .SetOwned<oz3::FrontendType>(
                std::make_unique<oz3::TerminalFrontendType>())
            .Build());
    oz3::OzzyApp app;
    app.Run(context, gb::GetMainArgs(argc, argv));
    return context->GetValueOrDefault<int>(oz3::OzzyApp::kKeyExitCode, 1);
  }
  ```
  It shares the context with `Run`, to read the exit code once `Run` has
  completed it. An exit code that isn't there (a context that didn't meet the contract, which is logged) is 1.
- **`Init`** converts `Game::Context()` to its own `Contract` (`context_`),
  as block-world does, and returns false if it isn't valid. The contract
  requires the clock (`kConstraintGameClock`), which is the same context value
  as `gb::Game`'s optional `kConstraintClock`, so the frames are run and timed
  by one clock, the one `main` gives it. It sets itself as
  `kConstraintOzzyApp`, and makes the command line: the type's name, help,
  and flags (as its added flags), with the app's own (`--stats` and the config
  file), run by a private static `RunCommand`, which finds the app by
  `kConstraintOzzyApp`. It runs it with `gb::RunCommandLine` in
  `context_` (whose `gb_cli` constraints the contract already holds), and
  sets the exit code from it. So help, usage errors, and their exit codes are
  `gb_cli`'s own, and tested through `Run` with arguments. `Init` returns
  whether the command started the app, which help doesn't.
- **`RunCommand`** reads the config file, with the error for it (exit code
  1). Then it configures the frontend (`FrontendType::Configure`), which may
  change the settings, creates the `OzzyHost` from them, powers on, and only
  then creates the frontend and attaches the Ozzy. So a config, a frontend's
  flags, or an Ozzy that fails never touches the console. The *Window app*
  changes this where its frontend shows errors in the panel.
- **`Update`**, while the Ozzy is on: takes the frontend's keys to the
  keyboard (and drops them without one), runs the host for `delta_time`, at
  most 100 ms (`kMaxRunTime`), and updates the frontend. A power request is
  acted on before the update: a restart powers off and on, so the frame shows
  the new Ozzy, and a shutdown powers off and passes the exit code to the
  frontend, which `Detach` shows. A power on that fails there ends the app,
  with exit code 1. While the Ozzy is off, it drops keys and updates the
  frontend, as the frontend needs every frame. It returns false once the
  frontend's quit is requested, and keeps the stats as it goes. The frame rate is `gb::Game`'s default, 60
  frames a second.
- **`CleanUp`** detaches the Ozzy if a frontend shows it, destroys the
  frontend, which gives the console back, and then the host, which powers the
  Ozzy off. Then it writes the error that stopped the app, if one did, and
  with `--stats`, the stats, to the context's `gb::CliWriter`. Every failure
  once the command runs (the config, the frontend, the host, or a power on)
  goes through a private `Fail`, which keeps the error and sets exit code 1.
  So both go to the console's own buffer, as before, and tests read them from
  a writer of their own.
- The app's states are only on and off, so it has no `gb::GameStateMachine`.
  The *Panel* may need one, and adds it then.
- `oz3_ozzy_app` links `gb_game` and `gb_cli`. `gb_game` uses `gb_base` and
  `gb_pilot`, which uses `gb_image` (and `stb` through it), and no SDL or
  Vulkan. `gb_game` is a new dependency, as the backlog item planned.
- Tests build its context with `ContextBuilder`, as `main` does, with a
  `gb::FakeClock` (`kConstraintGameClock`), a memory file system (the disk
  library's `FaultyFileProtocol`, to fail an image at a restart), a writer
  that notes whether the frontend is alive at each write, and the fake
  frontend's type, and read the exit code from it after `Run`.
- `OzzyApp` creates its `OzzyHost` from its own `context_`, passing it on
  without declaring what the host or the Ozzy need, as each checks its own.

**Brittleness:**
- The exit code is only in the context once `Run` returns, so `main` reads it
  there rather than `Run`'s bool, which can't carry it. `kOutRequired` makes
  an app that forgot to set it fail its contract, in tests too.
- `RunCommand` is a plain function, as `gb_cli`'s run functions are, so it
  finds the app through the context (`kConstraintOzzyApp`), which `Init`
  sets and completing the context clears, so it can't outlive the app. The
  command lists `OzzyApp::kConstraintOzzyApp` among its constraints, so the
  run function may read it. The app makes the command itself, so a frontend
  type gives only its parts and can't leave the run function out.
- `gb::Game`'s loop waits for each frame with `Clock::SleepUntil`, which a
  `FakeClock` moves straight to. The tests also have it advance a little at
  each read (`SetAutoAdvance`), set once in their fixture, so a frame's run
  takes time, and the stats count it. So each frame is a little over 1/60 s,
  and tests of the frame rate allow for it.

### TerminalFrontend (ozzy/term)

- `TerminalFrontend` is a `Frontend` over a `Console`: `TakeKeys` and
  `IsQuitRequested` are the console's, `Attach` makes a `ConsoleDisplay` of
  the Ozzy, `Update` updates it while there is one, and `Detach` updates
  it once more, so what a program wrote just before shutting down is shown,
  then destroys it, and with an exit code, calls `ShowOzzyShutDown`.
- A `Console` knows the mode it was created in (`GetMode`), so the frontend
  and `ShowOzzyShutDown` take it from the console rather than being told it,
  and `ConsoleDisplay` checks it is the Ozzy's.
- `TerminalFrontendType` gives `ozzyterm`'s name and help, and its flags, of which there are none yet. Its `Configure`
  turns off the screen's pixels, and returns a `TerminalFrontendFactory`,
  which creates the console in the mode the settings give
  (`GetOzzyConsoleMode`).
- `TerminalFrontend` takes the `Console` it is given, so its tests use the fake
  console that was in `console_display_test.cc`, moved to `fake_console.h`.
- The *Teletype view* and *Headless ozzyterm* change only this library and
  the type's flags: the view replaces the lines mode, and
  `--headless` makes a frontend with no console.

## Testing

Everything but `ozzyterm_main.cc` is tested, each library against fakes of
what it is given:
- `ozzy_host_test.cc`: `Create` with an invalid contract returns null; the
  Ozzy is off until `PowerOn`, and off again after `PowerOff`; a `PowerOn`
  that fails (an image that doesn't exist) returns the error and stays off;
  `Run` returns the cycles, a program's shutdown with its exit code, and its
  restart, and only the first request in a run; powering off commits a
  drive's image, and so does destroying the host while on.
- `ozzy_app_test.cc`, with the fake frontend type, a memory file system, a
  writer that notes whether a frontend is alive at each write, and a
  `gb::FakeClock`, all through `Run` with arguments:
  - A context without a clock doesn't run, leaving no exit code.
  - The command line has the frontend type's name, help, and flags, and the
    app's own, and the frontend reads its flag in `Configure`. `--help`
    writes help with exit code 0; an unknown flag, or a config that doesn't
    exist, is exit code 2; a config with errors writes the error with exit
    code 1; none of them creates a frontend. No config runs the default
    Ozzy.
  - A frontend that can't be configured, or created, writes its error with
    exit code 1; a power on that fails creates no frontend, and its error is
    written once nothing holds the console.
  - The screen draws pixels when the frontend turns them on, and none when it
    doesn't.
  - Running: the frontend is attached with the Ozzy, and updated each frame;
    typed keys reach the keyboard; a frame runs the Ozzy for its time, at
    most 100 ms; quitting ends the run with exit code 0.
  - Power: a program's restart powers off and on, with a new Ozzy; a
    shutdown passes its exit code to the frontend and keeps the app running,
    off, until quit; a power on that fails at a restart ends the run with
    exit code 1.
  - `--stats` writes the frames, time, and cycles across a restart, once the
    frontend is gone, and that it quit before its first frame when it did.
- `terminal_frontend_test.cc`, with the fake console in `fake_console.h`:
  the screen is drawn after a run and `Update`, and nothing with no Ozzy
  attached; `Detach` shows what changed since the last `Update`, and with an
  exit code, then the shutdown message, in the console's mode; a second
  `Attach` draws the new Ozzy's screen whole; keys and quit are the
  console's; the frontend type's command line is `ozzyterm`'s, and its
  `Configure` turns off the screen's pixels, with or without a screen.
- `posix_console_test.cc`: a console's mode is the one it was created in.

By hand: the app with `configs/console.ozzy` and `configs/teletype.ozzy`,
with and without `--stats`, `--help`, a config with an error, and a program's
restart and shutdown, looked as it did before.
