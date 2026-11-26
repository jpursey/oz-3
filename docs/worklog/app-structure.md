# App Structure

The terminal app has grown inside `ozzy_main.cc`, each piece added where it
fit: the command line, reading the config, powering on and off, the frame loop
and its pacing, waiting while the machine is off, and the stats. This gives it
the structure [ozzy_design.md](../ozzy_design.md) describes (The machine, its
state, and the panel), shared by both apps and tested:
- **The machine** is the `Ozzy`, as now.
- **The machine's state** is an `OzzyHost`, which outlives the machine: the
  settings, the context the Ozzy is created in, and whether it is on.
- **The app** is an `OzzyApp`, a `gb::Game`, which is the whole of an app's
  logic, whichever app it is: its command line, reading the config, the
  `OzzyHost`, what powering off does, the frame loop, and the stats, and later
  the *Panel*'s commands. It is the same harness for `ozzyterm` and `ozzy`.
- **How it is presented** is a `Frontend`, which the app creates through a
  `FrontendFactory` in its context: what the user sees of the machine, and the
  keys and quit the user gives it. `ozzyterm`'s is a `ConsoleFrontend`, built
  from the `Console` and `ConsoleDisplay` there are now, and the *Window app*
  adds a window's.

So `ozzy_main.cc` only puts the file system and the console's factory in a
context, runs an `OzzyApp`, and returns the exit code the app leaves in the
context. Everything else is tested: the command line too, against a fake
frontend, a memory file system, and a `gb::FakeClock`.

`oz3/ozzy` is split into a directory and a library for each part, as it will
grow with the *Panel*, the *Window app*, and the debugger.

## Behavior

Each frame runs the Ozzy for exactly the time that passed since the last, as
`gb::Game` gives it, at most 100 ms as now, so a host that falls behind slows
the Ozzy rather than racing to catch up. What the `ozzy` app does is otherwise
unchanged, apart from:
- **`--max-speed` is gone.** It ran the Ozzy faster than real time, as many
  cycles as fit in each frame, which is the one thing that doesn't fit
  `gb::Game`'s model of running the time that passed, and nothing needs it.
  Raising a config's `clock_hz` until `--stats` shows the Ozzy falling behind
  measures the host's top speed much as it did. The manual's Ozzy Computer
  page, and ozzy_design.md (Pacing), change to match.
- A frame that ends less than a millisecond late is absorbed into the next
  frame, rather than starting it at once, as `gb::Game`'s loop does. One more
  than a millisecond late starts the next at once, as now.

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
| `oz3/ozzy/term`    | `oz3_ozzy_term`    | `ozzyterm`'s frontend: `Console` (per platform), the terminal's keys, the console's text, `ConsoleDisplay`, and `ConsoleFrontend` |

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
- **`Frontend`** and **`FrontendFactory`**: how an app is presented, and what
  creates it. Other candidates: `OzzyView`, which collides with the *Teletype
  view*, and `Presenter`. Recommended: `Frontend`, which CLAUDE.md already
  uses for the console code ("the console frontend the `ozzy` app is built
  from"). `ConsoleFrontend` is `ozzyterm`'s, and `WindowFrontend` the window
  app's.
- **`FrontendKeys`**: what `ConsoleKeys` is now, as both frontends give keys
  the same way. `KeyChange` keeps its name.
- **`OzzyAppStats`**: what `--stats` writes, as `RunStats` is now.

### OzzyHost (ozzy/app)

```
// The host's side of an Ozzy: its settings, the context it is created in,
// and whether it is on. The Ozzy exists only while it is on.
class OzzyHost final {
 public:
  // The host reads nothing from its context itself. Each Ozzy is created in a
  // child of it, which adds the power callback, so the context holds the rest
  // of what the Ozzy needs from the host (see Ozzy::Contract), such as the
  // file system.
  using Contract = gb::ContextContract<>;

  // Creates a host for an Ozzy with `settings`, which is off, or returns null
  // if the contract is invalid.
  static std::unique_ptr<OzzyHost> Create(OzzySettings settings,
                                          Contract contract);

  // Destroying the host powers the Ozzy off.
  ~OzzyHost();

  const OzzySettings& GetSettings() const;

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

- It keeps its `ValidatedContext`, and the child context each Ozzy is created
  in (`ContextBuilder().SetParent(context_)`), which add the power callback.
  The callback notes the run's first request. Both outlive each Ozzy, as the
  drives write to their images as it is destroyed. The host's clock is left
  out of it, so programs see the host's local time, as now. `OzzyHost` passes
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

  // The machine powered on, as `ozzy`, which is shown from now until
  // PowerOff. The frontend may hold its devices until then.
  virtual void PowerOn(Ozzy* ozzy) = 0;

  // Shows what changed in the machine since the last call. Only called while
  // it is on, between runs.
  virtual void Update() = 0;

  // The machine powered off, so the frontend lets go of the Ozzy before it
  // is destroyed. `exit_code` is the program's, when it shut down: shown
  // until the next PowerOn. Without one, it is about to power on again.
  virtual void PowerOff(std::optional<int> exit_code) = 0;
};

// Creates an app's frontend, and gives the app its command line. An app's
// main puts one in the OzzyApp's context (OzzyApp::kConstraintFrontendFactory).
class FrontendFactory {
 public:
  virtual ~FrontendFactory() = default;

  // The app's command line, which lists OzzyApp's flags and arguments
  // (kStatsFlag, kConfigFileArg) and the frontend's own, and runs
  // OzzyApp::RunCommand.
  virtual const gb::CliCommand* GetProgram() const = 0;

  // Creates the frontend for an Ozzy with `settings`, reading the frontend's
  // flags from `command_line`. Returns an error if it can't (such as no
  // console to take over), which the app writes.
  virtual absl::StatusOr<std::unique_ptr<Frontend>> Create(
      const OzzySettings& settings, const gb::CliCommandLine& command_line) = 0;
};
```

- `PowerOn` and `PowerOff` bracket each Ozzy's life, so a frontend that holds
  its devices (as `ConsoleDisplay` does) always lets them go before the Ozzy
  is destroyed, and starts afresh for a new one. The app calls them, in one
  place each, beside `OzzyHost::PowerOn` and `PowerOff`.
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
// Runs an Ozzy until the user quits, presented by the frontend its context's
// factory creates.
class OzzyApp final : public gb::Game {
 public:
  //----------------------------------------------------------------------------
  // Contract constraints
  //----------------------------------------------------------------------------

  // REQUIRED: The app's frontend factory.
  static GB_CONTEXT_CONSTRAINT(kConstraintFrontendFactory, kInRequired,
                               FrontendFactory);

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
      GameContract, kConstraintFrontendFactory, kConstraintExitCode,
      kConstraintOzzyApp, gb::kConstraintCliFileSystem,
      gb::kConstraintCliWriter>;

  // The flags and arguments every app's command line has.
  static const gb::CliFlag<bool> kStatsFlag;
  static const gb::CliArg<std::string_view> kConfigFileArg;

  // The run function of every app's command line (see FrontendFactory).
  static bool RunCommand(gb::ValidatedContext& context,
                         const gb::CliCommandLine& command_line);

  gb::ValidatedContext& Context() override { return context_; }

 protected:
  bool Init(const std::vector<std::string_view>& args) override;
  bool Update(absl::Duration delta_time) override;
  void CleanUp() override;
};

// How an Ozzy ran, from its first power on, through each restart, to its last
// power off (what --stats writes).
struct OzzyAppStats {
  int64_t frames = 0;
  absl::Duration time;        // From each first frame's start to its last's end.
  absl::Duration frame_time;  // The time frames took, without waiting.
  Cycles cycles = 0;
};
```

- **`main`** is all that isn't tested:
  ```
  int main(int argc, char* argv[]) {
    auto context = std::make_shared<gb::Context>(
        gb::ContextBuilder()
            .SetOwned<gb::FileSystem>(gb::CreateCliFileSystem())
            .SetOwned<oz3::FrontendFactory>(
                std::make_unique<oz3::ConsoleFrontendFactory>())
            .Build());
    oz3::OzzyApp app;
    app.Run(context, gb::GetMainArgs(argc, argv));
    return context->GetValueOrDefault<int>(oz3::OzzyApp::kKeyExitCode, 1);
  }
  ```
  It keeps the context by `shared_ptr` to read the exit code once `Run` has
  completed it. An exit code that isn't
  there (a context that didn't meet the contract, which is logged) is 1.
- **`Init`** converts `Game::Context()` to its own `Contract` (`context_`),
  as block-world does, and returns false if it isn't valid. It sets itself as
  `kConstraintOzzyApp`, runs the command line with `gb::RunCommandLine` in
  `context_` (whose `gb_cli` constraints the contract already holds), and
  sets the exit code from it. So help, usage errors, and their exit codes are
  `gb_cli`'s own, and tested through `Run` with arguments. `Init` returns
  whether the command started the app, which help doesn't.
- **`RunCommand`** reads the config file, with the error for it (exit code
  1), and turns off the screen's pixels, as no frontend draws them yet. Then it
  creates the `OzzyHost`, creates the frontend, and powers on, so a config or
  an Ozzy that fails never touches the console. The *Window app* changes this
  where its frontend shows errors in the panel.
- **`Update`**, while the Ozzy is on: takes the frontend's keys to the
  keyboard (and drops them without one), runs the host for `delta_time`, at
  most 100 ms, and updates the frontend. A power request stops the frame
  without drawing it: a restart powers off and on, and a shutdown powers off
  and passes the exit code to the frontend. While the Ozzy is off, it only
  drops keys. It returns false once the frontend's quit is requested, and
  keeps the stats as it goes. The frame rate is `gb::Game`'s default, 60
  frames a second.
- **`CleanUp`** powers off and destroys the frontend, which gives the console
  back, and then writes a power on's error, if one stopped the app (exit code
  1), and with `--stats`, the stats, to the context's `gb::CliWriter`. So both
  go to the console's own buffer, as now, and tests read them from a
  `gb::FakeCliWriter`.
- The app's states are only on and off, so it has no `gb::GameStateMachine`.
  The *Panel* may need one, and adds it then.
- `oz3_ozzy_app` links `gb_game` and `gb_cli`. `gb_game` uses `gb_base` and
  `gb_pilot`, which uses `gb_image` (and `stb` through it), and no SDL or
  Vulkan. `gb_game` is a new dependency, as the backlog item planned.
- Tests build its context with `ContextBuilder`, as `main` does, with a
  `gb::FakeClock` (`Game::kConstraintClock`), a memory file system, a
  `gb::FakeCliWriter`, and the fake frontend's factory, and read the exit
  code from it after `Run`.
- `OzzyApp` creates its `OzzyHost` from its own `context_`, passing it on
  without declaring what the host or the Ozzy need, as each checks its own.

**Brittleness:**
- The exit code is only in the context once `Run` returns, so `main` reads it
  there rather than `Run`'s bool, which can't carry it. `kOutRequired` makes
  an app that forgot to set it fail its contract, in tests too.
- `RunCommand` is a plain function, as `gb_cli`'s run functions are, so it
  finds the app through the context (`kConstraintOzzyApp`), which `Init`
  sets and completing the context clears, so it can't outlive the app. A
  factory whose command doesn't run `RunCommand` never starts the app, which
  the factories' own tests check, and its command lists
  `OzzyApp::kConstraintOzzyApp` among its constraints so the run function
  may read it.
- `gb::Game`'s loop waits for each frame by sleeping to within a millisecond
  of it, then reading the clock until it is due, so a `FakeClock` must advance
  on its own (`SetAutoAdvance`), or the loop never gets there. The tests set
  it once in their fixture.

### ConsoleFrontend (ozzy/term)

- `ConsoleFrontend` is a `Frontend` over a `Console`: `TakeKeys` and
  `IsQuitRequested` are the console's, `PowerOn` makes a `ConsoleDisplay` of
  the Ozzy, `Update` updates it, and `PowerOff` destroys it, and with an exit
  code, calls `ShowOzzyShutDown`.
- `ConsoleFrontendFactory` creates the console in the mode the settings give
  (`GetOzzyConsoleMode`), and gives `ozzyterm`'s command line, which is
  `ozzy`'s now, without `--max-speed`.
- `ConsoleFrontend` takes the `Console` it is given, so its tests use the fake
  console now in `console_display_test.cc`, which moves to a header of the
  term library's tests.
- The *Teletype view* and *Headless ozzyterm* change only this library and
  the factory's command line: the view replaces the lines mode, and
  `--headless` makes a frontend with no console.

## CLs

### CL1 [ ] ozzy, apps: Split oz3/ozzy into libraries

Depends on: nothing.

- The machine's files (`ozzy`, `ozzy_settings`, `machine_table`,
  `boot_rom`, `boot_rom.oza`, `loader.oza`, and their tests) move to
  `oz3/ozzy/machine`, as `oz3_ozzy_machine`, in place of `oz3_ozzy`.
  `ozzy_testing` becomes `oz3_ozzy_testing` there.
- The console's files (`console`, `console_display`, `console_text`,
  `terminal_keys`, `posix_console`, `win_console`, and their tests) move to
  `oz3/ozzy/term`, as `oz3_ozzy_term`.
- Their includes and include guards follow, and `ozzy_main.cc` links
  `oz3_ozzy_term`. The code is otherwise unchanged.
- CLAUDE.md's library list and order, and the manual or worklogs that name
  the files by path, change to match.

**Verify**
- Standard checks (see CLAUDE.md).
- `git diff -M` shows the moves as renames, with only includes, guards, and
  CMake changed.
- CI, as the Windows console's files move too.

### CL2 [ ] ozzy/app, apps: OzzyHost, the machine's state

Depends on: CL1.

- `oz3/ozzy/app`, as `oz3_ozzy_app`, with `ozzy_host.h`/`.cc`.
- `ozzy_main.cc` runs its Ozzy through an `OzzyHost`, created from the
  command's context, in place of its own context, power callback, and
  `Ozzy::Create`.

**Verify**
- Standard checks.
- `ozzy_host_test.cc`: `Create` with an invalid contract returns null; off
  until `PowerOn`, and off again after `PowerOff`;
  a `PowerOn` that fails (an image that doesn't exist) returns the error and
  stays off; `Run` returns the cycles, and a program's shutdown with its exit
  code, and its restart; only the first request in a run is returned; a
  power off commits a drive's image, and so does destroying the host while on.

### CL3 [ ] ozzy/app, ozzy/term, apps: Frontend, and the console's

Depends on: CL2.

- `oz3_ozzy_app`: `frontend.h`, with `Frontend`, `FrontendKeys`, and
  `KeyChange` (moved from `console.h`), and `FrontendFactory`.
- `oz3_ozzy_term`: `ConsoleFrontend` and `ConsoleFrontendFactory`. `Console`
  returns `FrontendKeys` in place of `ConsoleKeys`.
- `ozzy_main.cc` shows the Ozzy through a `ConsoleFrontend`, in place of its
  own `ConsoleDisplay` and `ShowOzzyShutDown`.

**Verify**
- Standard checks.
- `console_frontend_test.cc`, with the fake console: nothing shown before
  `PowerOn`; the screen drawn after a run and `Update`; `PowerOff` with an
  exit code shows the shutdown message, and without one shows nothing; a
  second `PowerOn` draws the new Ozzy's screen whole; keys and quit are the
  console's.

### CL4 [ ] ozzy/app, ozzy/term, apps, manual: OzzyApp on gb::Game

Depends on: CL3.

- `oz3_ozzy_app`: `ozzy_app.h`/`.cc`, with `OzzyApp` and `OzzyAppStats`, and
  the fake frontend for its tests. It gains `gb_game` and `gb_cli`.
- `oz3_ozzy_term`: the factory's command line, moved from `ozzy_main.cc`,
  without `--max-speed`.
- `ozzy_main.cc` is only `main`, as above.
- The manual's Ozzy Computer page and ozzy_design.md (Pacing) drop
  `--max-speed`.
- The backlog's *Teletype view*, *Panel*, *Headless ozzyterm*, and *Window
  app* name `OzzyApp`, `Frontend`, and the libraries where they describe the
  code.

**Verify**
- Standard checks.
- `ozzy_app_test.cc`, with the fake frontend, a memory file system, a
  `gb::FakeCliWriter`, and a `gb::FakeClock`, all through `Run` with
  arguments:
  - The command line: `--help` writes help with exit code 0, creating no
    frontend; an unknown flag is exit code 2; a config that doesn't exist, or
    has errors, writes the error with exit code 1, creating no frontend; no
    config runs the default Ozzy.
  - Running: the frontend is powered on with the Ozzy, and updated each
    frame; typed keys reach the keyboard; a frame runs the Ozzy for its time,
    at most 100 ms; quitting ends the run with exit code 0.
  - Power: a program's restart powers off and on, with a new Ozzy; a
    shutdown passes its exit code to the frontend and keeps the app running,
    off, until quit; a power on that fails ends the run, writing the error
    once the frontend is gone, with exit code 1.
  - `--stats` writes the frames, time, and cycles across a restart, once the
    frontend is gone, and that it quit before its first frame when it did.
- `console_frontend_test.cc`: the factory's command line runs
  `OzzyApp::RunCommand`, and has the shared flags and arguments.
- By hand (left for the user): `ozzy` with `configs/console.ozzy` and
  `configs/teletype.ozzy`, with and without `--stats`, `--help`, a config with
  an error, and a program's restart and shutdown, look as they did.
