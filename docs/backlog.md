# Backlog

Work that isn't being done yet. The worklogs in `docs/worklog/` describe what
was built; this is what has not been. See the workflow (imported by CLAUDE.md)
for how items are written and picked up.

The list is a rough stack rank: the order the items look worth doing, as a best
guess rather than a commitment. Re-order it freely. The large items near the
bottom are the rest of the roadmap in README.md and the manual.

Each item carries:

- **Layers**: the OZ-3 libraries it touches (`core`, `tools`,
  `instruction_sets`, and `apps` for executables), in dependency order, and the
  manual if it changes the specification. More than one usually means more than
  one CL.
- **Size**: a guess. *Small* is a single CL. *Medium* is a few. *Large* is
  many, usually after a design.
- **Feature workflow**: whether the item follows the feature workflow, with a
  design and a `docs/worklog/` plan of CLs. Anything that comes down to one or
  two simple CLs doesn't, and is done as an ordinary change.
- **Depends on**: other items that should come first, or "nothing". Items in
  another project's backlog are named with the project, such as Game Bits
  *Fiber-safe thread locals*.
- **Requested by**: the project and item that need it, for items another
  project asked for. For ranking only.
- **Background**: where the context is, if anywhere.

## Ozzy OS

- **Layers:** ozzy, manual
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** *Booting from disk*, *Core mailboxes*
- **Background:** [ozzy_design.md](ozzy_design.md) (The Ozzy OS),
  [booting-from-disk.md](worklog/booting-from-disk.md)

The OS, in OZ-3 assembly: the loader in the boot area, the calls, the program
loader, running programs in the foreground and in parallel on other cores, and
the shell.

## Teletype view

- **Layers:** ozzy, apps, manual
- **Size:** small
- **Feature workflow:** no
- **Depends on:** *App structure*
- **Background:** [ozzy_design.md](ozzy_design.md) (`ozzyterm`),
  [app-structure.md](worklog/app-structure.md);
  `oz3/ozzy/term/terminal_frontend.h`, `oz3/ozzy/term/console_display.h`

The terminal app's console (`TerminalFrontend`, in `ozzy/term`) is chosen by
its command line, its factory's flags, rather than inferred from the Ozzy's
devices: without `--headless`, it always takes the
whole console, and only `--headless` writes lines as they come (see
*Headless ozzyterm*). An Ozzy without a screen shows its teletype in a view
the size of the console, which the Ozzy doesn't know of: the teletype's output
wraps at the console's width, scrolls up from the bottom, and is drawn whole
again on a resize, as the screen's picture is. An Ozzy with neither shows the
message it does now. As the console no longer depends on the config, `OzzyApp`
powers the Ozzy on before creating the frontend, so an Ozzy that fails to
create never touches the console. The shutdown message takes the view's place as it
does the screen's, so `ShowOzzyShutDown` has one form. The teletype's lines no
longer stay in the terminal once the app quits, as the console is given back
as it was found; `--headless`'s standard output is the transcript instead. The
manual's Ozzy Computer page changes to match.

## Panel

- **Layers:** ozzy, apps, manual
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** *Teletype view*
- **Background:** [ozzy_design.md](ozzy_design.md) (Host application),
  [ozzy-drives.md](worklog/ozzy-drives.md); [disk_design.md](disk_design.md)
  (Open questions)

The host's controls for the machine, opened and closed with Ctrl+Alt+P, in the
terminal app: `on`, `off`, `restart`, `insert` and `eject` a disk, `drives`,
`help`, and `quit`. It takes the whole console, in place of the screen's
picture or the teletype view, with the machine's status, its messages, and a
command line. While it is open, keys go to it and the machine keeps running,
and while the machine is off, it stays open. The commands, and what they do to
the machine's state, are `OzzyApp`'s (`ozzy/app`), tested against the fake
frontend, and the `Frontend` gains a call to show the panel, which
`TerminalFrontend` draws as text, so the window app can present them too.
`OzzyApp` may need a `gb::GameStateMachine` for it. Paths are as typed at
a shell, and a `config:` protocol in the file system is the config file's
folder, which becomes the default for the config's own image paths. A disk
inserted while the machine is on goes into the running drive and stays in the
machine's state, so it is still there after a restart. Ejecting is a floppy
pulled, as `DiskDrive` has it: what the file system hadn't committed is lost,
and the OS commits with `CLOSE` and `FLUSH`. An image can't go into two drives,
as in the config.

## Headless ozzyterm

- **Layers:** ozzy, apps, manual
- **Size:** small
- **Feature workflow:** no
- **Depends on:** *App structure*
- **Background:** [ozzy_design.md](ozzy_design.md) (Host application,
  `ozzyterm`), [app-structure.md](worklog/app-structure.md);
  `oz3/ozzy/term/terminal_frontend.h`

`--headless`, a flag of `TerminalFrontendFactory`, runs an Ozzy in the
terminal app with no terminal, through a `Frontend` with no console, for scripts and
tests: `ozzyterm --headless test.ozzy < keys.txt > out.txt`. The standard input
is typed keys (its key state made up from them, as a terminal's is), the
teletype's output goes to the standard output, and a screen isn't shown. There
is no panel, so a shutdown from the power port exits with its exit code (the
frontend tells `OzzyApp` it wants that), and a
signal (such as SIGTERM) quits, committing the drives as quitting does. The end
of the standard input ends nothing, as a program may run long after its input.
Errors and `--stats` go to the standard error, so the output is only the
teletype's. Until the *Window app* takes the name `ozzy`, it is
`ozzy --headless`.

## Kitty keyboard protocol

- **Layers:** ozzy, wiki
- **Size:** medium
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** `oz3/ozzy/term/posix_console.cc`;
  https://sw.kovidgoyal.net/kitty/keyboard-protocol/

In a terminal, `ozzy` only sees what is typed, so the keyboard's key state is
made up of taps: each typed key is pressed and released at once, a held key is
pressed again at the terminal's auto-repeat rate, and Shift is never seen for
a character. Terminals with the kitty keyboard protocol (kitty, foot, WezTerm,
Ghostty, Alacritty, recent iTerm2) report every key's press, repeat, and
release, with its key in the US PC-101 layout, which is how key codes are
defined. Turn it on where the terminal answers its query (`CSI ? u`), with the
flags for event types, alternate keys, and all keys as escape codes, and read
its `CSI ... u` keys into both typed key events and real key state, falling
back to taps elsewhere. Give it back when the console is destroyed (`CSI < u`).
It adds no dependencies, only escape sequences, but needs trying in those
terminals by hand.

## Window app

- **Layers:** ozzy, apps, manual
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** *Panel*
- **Background:** [ozzy_design.md](ozzy_design.md) (Host application, `ozzy`);
  [app-structure.md](worklog/app-structure.md);
  [console-devices.md](worklog/console-devices.md) (Pixels)

`ozzy`, an Ozzy in a window of its own, whatever devices it has, as a windowed
program (`gb_add_win_executable`), so starting it from the desktop opens no
console window. The terminal app is renamed `ozzyterm`. The window is a
`Frontend` and its factory in a library of its own after `ozzy/term`, and the
app's `main` runs `OzzyApp` with that factory, as `ozzyterm`'s does with
`TerminalFrontendFactory`. The window draws the
screen's picture as its pixels are, through Game Bits' SDL and renderer: the
pixels as an 8-bit texture with a three-color palette, each pixel twice as
tall as it is wide. So it shows what the terminal can't: the left and right
lines, redefined glyphs, the alternate character set, compact rows, and the
exact blink timing. Without a screen, the teletype's lines have a space of
their own in the window. Keys come from the window. The *Panel* opens over the
picture with the same key, and menus run its commands; while the machine is
off, the picture is dark, and a config file with errors opens the window with
the machine off and the errors in the panel, so `OzzyApp` creates such a
frontend before reading the config. Only `ozzy` links SDL and Vulkan,
so it is built only where the Vulkan SDK is, and CI installs the SDK for it, as
Game Bits' CI does with its `vulkan-sdk` action.

## Debugger (oz3dbg)

- **Layers:** core, tools
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** the manual's `README.md`, and the `T` (trap) flag in
  `2-Specifications.md`

The debugger for OZ-3 programs, as a library and a tool: stepping (through the
`T` flag), breakpoints, watches, and source level debugging from `.oz3map`
files. Ozzy integrates the library: in the window app as windows of its own,
and in the terminal app as commands in the panel.

A program map works in segment offsets, so turning an address in a bank (such
as a memory view's) into source needs where each segment was loaded.
`ProgramLoader::LoadedProgram` only records each program bank's page spans, so
it gains each segment's physical bank and start page.

## Mouse

- **Layers:** devices, ozzy, apps, manual
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** [ozzy_design.md](ozzy_design.md) (Host application);
  `oz3/ozzy/term/posix_console.cc`

A mouse device, and the Ozzy wired to it, on ports from the ones left free
(28 to 31 are together). It reports where the mouse is, its buttons, and its
wheel, with an interrupt when they change. The window app moves it by the
screen's pixels, and the terminal app by its cells, with the terminal's mouse
reporting (SGR, `CSI ? 1006 h`) on POSIX systems and the console's mouse events
on Windows, so programs work with either, more coarsely in a terminal.

## Rendering styles and themes

- **Layers:** ozzy, apps
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** *Window app*
- **Background:** [ozzy_design.md](ozzy_design.md) (Host application, `ozzy`)

Looks for the window app, chosen in its settings rather than the config file,
as they are the host's and not the machine's: the screen's colors (such as a
green, amber, or white phosphor), how its picture is drawn (sharp pixels, or a
monitor's scanlines, glow, and curve), and themes for the app's own windows and
panel.

## Constants' text in STR

- **Layers:** tools, manual
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** the manual's Program Source page (`STR` and `PSTR` in
  Directives, Macros)

`STR` of a constant's name could be the text of the expression that defined
it, so `.CONST LIMIT = 4 * 2` makes `STR(LIMIT)` `4 * 2`. In a macro, a
parameter's argument must still win: `ENTRY LIMIT`, with `.DW STR(name)` in
the body, writes `LIMIT`, not `4 * 2`. Parameters are replaced as text before
the body is read, so the expansion has to say which text came from an
argument (`MacroExpansion` knows where each run of its text came from).

## Case-sensitive programs

- **Layers:** tools, manual
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [assembler_design.md](assembler_design.md) (Source)

An assembler option for case-sensitive names, which must then be written as
the instruction set and the specification write them. Without it, every name is
turned to upper case as it is read.

## Data only programs

- **Layers:** tools, manual
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** the manual's Program Files and Program Source pages

A program with no instances is data only, which no core starts (see Program
Files), but the assembler always gives a program at least one: `.INSTANCES` is
1 to 8, and 1 without it. Let a program say it has none (such as
`.INSTANCES 0`), which a program with no code or stack segment could, for data
an operating system loads for programs to share.

## Instruction set identity in program files

- **Layers:** tools, manual
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [assembler_design.md](assembler_design.md) (Program file);
  the manual's Program Files page; `oz3/tools/program_file.h`

A program file records which instruction set it was assembled for, such as a
fingerprint of its code word layout, so loading it for a core with a different
instruction set can be caught. It is a new version of the program file format,
which readers of version 1 reject.

## Same-file check through the file system

- **Layers:** ozzy, apps
- **Size:** small
- **Feature workflow:** no
- **Depends on:** Game Bits *Canonical local paths*
- **Background:** `ValidateOutputPaths()` in `oz3/apps/oz3asm.cc`, `Run()` in
  `oz3/apps/oz3ism.cc`, and `ValidateOzzySettings()` in
  `oz3/ozzy/machine/ozzy_settings.cc`

`oz3asm` won't write a file that is the source or another file it writes, nor
`oz3ism` one that is its input, but they only compare the paths as normalized
text. Two paths to one file that are
written differently get past the check, and the file is overwritten: a
relative and an absolute path, a drive and its `file:` form
(`file:/c/src/a.oza` is `C:\src\a.oza`), a difference in case where the file
system ignores it, or a link. The Ozzy's check that no two drives have one
image compares its paths the same way, so one image can get into two drives.
Ask the file system whether two paths are the same file instead, once Game
Bits' canonical paths resolve case and links, and `gb::FileSystem` lets a
caller ask for one.

## Sleeping cores

- **Layers:** core
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [devices-in-core.md](worklog/devices-in-core.md) (Sleeping);
  `CpuCore::State` in `oz3/core/cpu_core.h`

An idle core (`HALT`, or never started) still runs every cycle, only to find
it has nothing to do, as does a core in `WAIT`. Let them sleep as devices do:
an idle core until an interrupt, `CRUN`, or `Reset` wakes it, and a waiting
core also until its wait ends (a deadline), so a machine of mostly idle cores,
as an Ozzy usually is, costs little to run. Cycle counts don't change: a
woken core must run at the same cycle it would have by polling, which the
existing core and instruction set tests check.

## Waking on lock grants

- **Layers:** core
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [devices-in-core.md](worklog/devices-in-core.md) (Devices,
  the lock handoff; Sleeping); `oz3/core/lockable.h`

A device waiting for a lock checks it every cycle (`AdvanceCycles(1)`), as the
teletype does for its port. Let it sleep instead, woken when the lock is
granted: a lock can name the component that requested it (such as
`Device::LockPort` naming the device), and `Lockable` wakes it when it grants
a queued lock. This matters most for a device that contends with cores for a
memory bank, such as the screen. Cycle counts don't change, as with *Sleeping
cores*: a device must use the lock in the same cycle it would by checking. A
woken component runs at the cycle after the wake, which is a cycle late for a
device waiting on a core's lock (a core releases it earlier in the same
cycle), so a grant wakes a component that hasn't run yet this cycle in time
to run in it. The teletype's `OUTR` timing and `device_test.cc`'s handoff
tests check this. *Order-independent lock handoff* changes the same timing,
so whichever is done second follows the first.

## Order-independent lock handoff

- **Layers:** core
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** [devices-in-core.md](worklog/devices-in-core.md) (Devices,
  the lock handoff); `oz3/core/lockable.h`

Each cycle, the `Processor` runs its cores in index order, then its devices in
the order added. A lock released in a cycle is granted to the next component
waiting for it in that cycle, but one that has already run this cycle only
uses it in the next. So a core waiting on a lock a device (or a higher-index
core) releases loses a cycle that a device waiting on a core's lock doesn't.
Make the handoff the same whatever the order: whoever a released lock is
granted to can use it in that cycle. Do it only if it costs the processor loop
nothing measurable, as the run order was kept for speed. It changes cycle
counts where components contend for a lock, so the tests that pin the
handoff down change with it.

## Coprocessors

- **Layers:** core, manual
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** the Coprocessors section of `2-Specifications.md`

Support for coprocessors in `core`: components that run alongside the cores,
with their own opcodes, which a core fetches and decodes and then hands to the
coprocessor to run. Then the two standard coprocessors the spec names, whose
sections are still TODO there: the DMA processor (block copies of pages between
memory banks) and the math processor (floating point, fast integer multiply and
divide, trig functions). The spec is written first, then built.

How coprocessors add opcodes is still to be designed. The likely shape is to
reserve the high bytes `0xF0` to `0xFF` of the code word as coprocessor
identifiers (up to 16 coprocessors), with the low byte as the coprocessor's own
opcode (up to 256 each). That would leave the core instruction sets, including
the default one, below `0xF0`.

Coprocessors raise interrupts as devices do, learning which cores they were
newly raised on (see `Processor::RaiseInterrupt`), can sleep while they have
nothing to do (see `ExecutionComponent::Sleep`), and join Ozzy's config once
they exist. Beyond what devices do:
- **Raising on chosen cores:** a coprocessor serves every core, so it raises
  an interrupt on the cores it picks, such as the one whose request it
  finished (the DMA processor's request registers are per core), from among
  those its line is wired to. So each opcode a core hands it says which core
  it came from, and its raise takes a mask of cores, limited to its wiring. A
  device could raise the same way, but none needs to yet.
- **Results through locks:** a coprocessor is shared, so cores take turns at
  it through its lock, and a core that needs a result (the math processor's)
  waits on the lock until it is done, as an x87's `FWAIT` does. Interrupts are
  for work that finishes on its own time (a DMA transfer), and perhaps errors.
- **Any free core:** work any one core may take raises a shared interrupt
  (see `Processor::RaiseSharedInterrupt`), as a device wired shared does.

## Shared interrupts from programs

- **Layers:** core, instruction_sets, manual
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** the `INT` and `CLK` microcode in `2.1-Microcode.md`; `INTC`
  in `2.2-Default-Instruction-Set.md`; `Processor::RaiseSharedInterrupt`

Devices and the host can raise a shared interrupt, which only one of the cores
it is raised on handles, but a program can't. `INT` raises on one core, the
running one or the one locked with `CLK`, and `CLK` locks only one, so a raise
on a mask of cores can't be built from existing microcode. It needs a new op,
raising a shared interrupt on the cores in a register's mask, and a new
instruction at the end of the `.izm` that uses it. It would let a program hand
work to whichever core of a pool is free, such as a job queue waking one idle
worker. It adds microcode, so it waits until a program needs it.

## Shared I/O page

- **Layers:** core, manual
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** Main memory in the manual's `2-Specifications.md`, and its
  `3-Devices.md` (Wiring and settings)

A mapped page belongs wholly to its device, so a device with only a few words
to map takes a whole page, 4 Kiw of a bank's 64, and each such device takes
another. Ports suit small state a program reads and writes as registers (the
keyboard, the timer), and memory mapping suits bulk
data (the screen's cells), so no device needs this yet. One with more than
ports comfortably hold, such as sound or sprite registers, could share a page
instead: several devices mapped into one page, each at a range of words of its
own that it addresses from 0, as the C64's I/O area holds several chips. The
spec's "a mapped page belongs wholly to its device" becomes a range, and only a
shared page needs the range checked to find its device. Smaller pages aren't
an option, as a bank's 16-bit read and write masks fix it at 16 pages. It
waits until a device needs it.

## Typed keys from key state

- **Layers:** devices, manual
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [keyboard-key-state.md](worklog/keyboard-key-state.md)

A keyboard with typed keys needs its host to translate each key by its own
layout, and type it with `Keyboard::TypeKey`. A keyboard setting could have
the keyboard make its typed keys from its key state instead, as old keyboards
did from their own fixed layout, so a host (such as a game reporting only
where keys are) only sets key state. The keyboard would turn key codes into
characters by the US layout the codes are named for, with Shift and Ctrl
applied, and Caps Lock (which needs a key code and a lock state). It would
repeat a held key itself, after a delay and at a rate in real time turned into
cycles, so it repeats the same on every run. The cost is the layout: on any
other, keys type the US characters, so AZERTY's `A` types `q`.

## The boot ROM's stack in the machine table

- **Layers:** ozzy, manual
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [booting-from-disk.md](worklog/booting-from-disk.md)
  (Brittleness); the manual's `4-Ozzy-Computer.md` (The boot ROM's stack)

The boot ROM starts the loader with `RST SELF`, which needs the stack's bank,
but a program can't read `MB`. So the ROM finds the bank in the machine table
as the lowest with RAM, repeating the rule `Ozzy::Create` places the stack by,
which a `CHECK` there holds the two to. The machine table could give the boot
ROM's stack's bank and page itself, so the rule is written once, in the Ozzy,
and the ROM reads it as it reads the rest of its hardware. It changes the
machine table, which programs read, so it is best done before the Ozzy OS
reads it.

## Disk tool failure reasons

- **Layers:** tools
- **Size:** small
- **Feature workflow:** no
- **Depends on:** *Disk tools*, and Game Bits *File system errors as
  absl::Status*
- **Background:** [disk-tools.md](worklog/disk-tools.md) (Behavior)

`DiskTool`'s file commands go through `gb::FileSystem`, whose calls say only
whether they failed, so a failure says what couldn't be done to the file
("HELLO.TXT: can't be written") but not why: a read-only file, a write
protected or full disk, or a broken one. Once Game Bits returns `absl::Status`,
`DiskFileProtocol` returns its `DiskStatus` as each call's error, and the file
commands pass it on, as in "work.ozd: HELLO.TXT: read-only", with nothing
worked out ahead of the call.
