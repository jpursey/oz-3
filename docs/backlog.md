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

## Ozzy machine

- **Layers:** ozzy (new), apps, wiki
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** *Console devices*
- **Background:** [ozzy_design.md](ozzy_design.md)

The first Ozzy anyone can run: the `ozzy` library builds a machine from a
`gb::Config` text file, writes the machine table and the boot ROM, and starts
core 0, and the `ozzy` app runs it in the Windows console in real time. The
boot ROM prints a banner and echoes keys. Creates Ozzy's wiki page, which
documents the console devices for programmers (see
[console-devices.md](worklog/console-devices.md)), and defines the screen's
graphics characters (128 to 255), which the frontend draws.

## Timer device

- **Layers:** devices, ozzy
- **Size:** small
- **Feature workflow:** no
- **Depends on:** *Ozzy machine*
- **Background:** [ozzy_design.md](ozzy_design.md) (Timer)

A periodic interrupt at a configured rate, and a port to read the cycle count
and the host's date and time.

## Disk drive device

- **Layers:** devices, ozzy, wiki
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** *Ozzy machine*
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
they exist.
