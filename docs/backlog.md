# Backlog

Work that isn't being done yet. The worklogs in `docs/worklog/` describe what
was built; this is what has not been. See the workflow (imported by CLAUDE.md)
for how items are written and picked up.

The list is a rough stack rank: the order the items look worth doing, as a best
guess rather than a commitment. Re-order it freely. The large items near the
bottom are the rest of the roadmap in README.md and the wiki.

Each item carries:

- **Layers**: the OZ-3 libraries it touches (`core`, `tools`,
  `instruction_sets`, and `apps` for executables), in dependency order, and the
  wiki if it changes the specification. More than one usually means more than
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

## Ozzy console polish

- **Layers:** ozzy, apps
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [ozzy-machine.md](worklog/ozzy-machine.md) (ozzy: the
  console frontend, apps: ozzy);
  [keyboard-key-state.md](worklog/keyboard-key-state.md) (Confirmed by hand)

What trying `ozzy` by hand left open:
- **Frame rate:** `absl::SleepFor` sleeps in Windows' 15.6 ms timer ticks, so
  frames land near 32 to 64 a second rather than 60 (the Ozzy's speed is right
  regardless). A high-resolution waitable timer in the Windows console code
  would make them even.
- **Console size:** a console smaller than the screen draws the rows past its
  bottom over its last row, and cuts off the columns past its edge. `ozzy`
  could say so on starting, or resize the console.
- **The teletype's last line:** quitting a teletype-only Ozzy mid-line leaves
  the shell's prompt on that line.
- **Unchecked by hand:** the overline (`ESC[53m`), the sextants' font, and the
  width of East Asian "ambiguous" characters (such as the shades and card
  suits), in the old console host as well as Windows Terminal; and the frame
  time of the default Ozzy in Release, and the cycles a second with
  `--max-speed`.
- **Key state unchecked by hand:** AltGr (held as the control key state says,
  expected to be Left Ctrl and Right Alt), the keypad's Enter (which has no
  code), and remapped keys (which may have no scan code, and so no key state).
- **Terminals that aren't consoles:** the Claude Code terminal panel keeps
  Ctrl+Alt+Q, so `ozzy` can't be quit there but from outside, and gives a held
  key as a stream of taps. `ozzy` could say it needs a Windows console, or have
  a quit key such terminals pass through.

## Screens without pixels

- **Layers:** devices, ozzy
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [console-devices.md](worklog/console-devices.md) (Pixels)

Every refresh draws the screen's picture into pixels (about 150 KB for 80 by
24), which the console frontend never shows, as it draws the cells as text. At
30 refreshes a second that is little, but with `--max-speed` it grows with the
speed. A screen setting, or a host call, that leaves the pixels undrawn would
save it for hosts that show text.

## Program lexer's code points through Game Bits

- **Layers:** tools
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** `oz3/tools/program_lexer.cc`

The program lexer decodes surrogate pairs by hand, where Game Bits'
`gb::ConsumeCodePoint` (`gb/base/unicode.h`) now does it.

## Timer device

- **Layers:** devices, ozzy
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [ozzy_design.md](ozzy_design.md) (Timer)

A periodic interrupt at a configured rate, and a port to read the cycle count
and the host's date and time.

## Typed keys from key state

- **Layers:** devices, wiki
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

## Disk drive device

- **Layers:** devices, ozzy, wiki
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** [ozzy_design.md](ozzy_design.md) (Disk drive, File names)

The smart drive, which owns its file system: a flat directory of 12.3 named
files on a host folder, requests as blocks in memory with the port as a
doorbell, transfers straight to memory in simulated time, and text files as
packed host bytes.

## Ozzy OS

- **Layers:** ozzy, wiki
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** *Timer device*, *Disk drive device*
- **Background:** [ozzy_design.md](ozzy_design.md) (The Ozzy OS)

The OS, in OZ-3 assembly: booting from drive `A:`, the calls, the program
loader, running programs in the foreground and in parallel on other cores, and
the shell.

## Graphical frontend

- **Layers:** ozzy, apps
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** [ozzy-machine.md](worklog/ozzy-machine.md) (The `ozzy`
  app); [console-devices.md](worklog/console-devices.md) (Pixels)

An Ozzy in a window of its own, drawing the screen's picture as its pixels
are, through Game Bits' SDL and renderer: the pixels as an 8-bit texture with
a three-color palette, each pixel twice as tall as it is wide. So it shows
what the console frontend can't: the left and right lines, redefined glyphs,
the alternate character set, compact rows, and the exact blink timing. Keys
come from the window rather than the console, and the teletype's output needs
somewhere to go in the window too.

## Labels in address forms

- **Layers:** tools, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** the wiki's `2.5-Program-Source.md` (Symbols, Segments) and
  `2.2-Default-Instruction-Set.md` (Address forms); `oz3/ozzy/boot_rom.oza`
  (its variables)

A label is its offset in its own segment, but `S(n)`, `D(n)`, and `E(n)`
address from a bank's base register, and the assembler checks neither, so a
label in one silently addresses the wrong words:
- `S(label)` is wrong even for a label in the stack segment, as `BS` is at the
  segment's end (`SP` starts at 0, and the first push wraps to its last word),
  so the label is off by the segment's size.
- `D(label)` and `E(label)` are right for a label in the `DATA` or `EXTRA`
  segment, whose base registers are at their starts, but a label in any other
  segment is an offset into the wrong segment, and maybe the wrong bank.

These are most likely errors: a label in `S()`, `D()`, or `E()` must be in the
segment with that role (with `S()` an error, or its label's offset from the
end, so it works). A register form (`MOV R1, label`, then `(R1)`) can't be
checked, which the Program Source page should say. The boot ROM works around
it by pushing its variables first, and addressing them as `S(-1)` down.

## Debugger (oz3dbg)

- **Layers:** core, tools
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** the wiki's Home page, and the `T` (trap) flag in
  `2-Specifications.md`

The debugger for OZ-3 programs, as a library and a tool: stepping (through the
`T` flag), breakpoints, watches, and source level debugging from `.oz3map`
files. Ozzy integrates the library.

A program map works in segment offsets, so turning an address in a bank (such
as a memory view's) into source needs where each segment was loaded.
`ProgramLoader::LoadedProgram` only records each program bank's page spans, so
it gains each segment's physical bank and start page.

## Program macros and conditional assembly

- **Layers:** tools, wiki
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** [assembler_design.md](assembler_design.md)

Macros in program source (`.MACRO`), for the boilerplate the instruction set
doesn't cover, and conditional assembly (`.IF`), such as for building one
source for different hardware configurations.

## Low and high words

- **Layers:** tools, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [assembler_design.md](assembler_design.md) (Expressions)

A way to write a dword's low or high word where a word is used, such as
`LOW(X)` and `HIGH(X)`, which evaluate `X` as a dword. An expression is
evaluated in the width where it is used, and a constant or number in it must
fit that width, so `X & 0FFFFh` is an error in a word wherever `X` doesn't fit
one. Functions would be the first names an expression reserves, besides the
registers.

## Case-sensitive programs

- **Layers:** tools, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [assembler_design.md](assembler_design.md) (Source)

An assembler option for case-sensitive names, which must then be written as
the instruction set and the specification write them. Without it, every name is
turned to upper case as it is read.

## Data only programs

- **Layers:** tools, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** the wiki's Program Files and Program Source pages

A program with no instances is data only, which no core starts (see Program
Files), but the assembler always gives a program at least one: `.INSTANCES` is
1 to 8, and 1 without it. Let a program say it has none (such as
`.INSTANCES 0`), which a program with no code or stack segment could, for data
an operating system loads for programs to share.

## Instruction set identity in program files

- **Layers:** tools, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [assembler_design.md](assembler_design.md) (Program file);
  the wiki's Program Files page; `oz3/tools/program_file.h`

A program file records which instruction set it was assembled for, such as a
fingerprint of its code word layout, so loading it for a core with a different
instruction set can be caught. It is a new version of the program file format,
which readers of version 1 reject.

## Same-file check through the file system

- **Layers:** apps
- **Size:** small
- **Feature workflow:** no
- **Depends on:** Game Bits *Canonical local paths*
- **Background:** `ValidateOutputPaths()` in `oz3/apps/oz3asm.cc`, and `Run()`
  in `oz3/apps/oz3ism.cc`

`oz3asm` won't write a file that is the source or another file it writes, nor
`oz3ism` one that is its input, but they only compare the paths as normalized
text. Two paths to one file that are
written differently get past the check, and the file is overwritten: a
relative and an absolute path, a drive and its `file:` form
(`file:/c/src/a.oza` is `C:\src\a.oza`), a difference in case where the file
system ignores it, or a link. Ask the file system whether two paths are the
same file instead, once Game Bits' canonical paths resolve case and links, and
`gb::FileSystem` lets a caller ask for one.

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

- **Layers:** core, wiki
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

- **Layers:** core, instruction_sets, wiki
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

- **Layers:** core, wiki
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** Main memory in the wiki's `2-Specifications.md`; the wiki's
  `3-Devices.md` (Wiring and settings)

A mapped page belongs wholly to its device, so a device with only a few words
to map takes a whole page, 4 Kiw of a bank's 64, and each such device takes
another. Ports suit small state a program reads and writes as registers (the
keyboard, *Timer device*), and memory mapping suits bulk
data (the screen's cells), so no device needs this yet. One with more than
ports comfortably hold, such as sound or sprite registers, could share a page
instead: several devices mapped into one page, each at a range of words of its
own that it addresses from 0, as the C64's I/O area holds several chips. The
spec's "a mapped page belongs wholly to its device" becomes a range, and only a
shared page needs the range checked to find its device. Smaller pages aren't
an option, as a bank's 16-bit read and write masks fix it at 16 pages. It
waits until a device needs it.
