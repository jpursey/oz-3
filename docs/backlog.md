# Backlog

Work that isn't being done yet. The worklogs in `docs/worklog/` describe what
was built; this is what has not been. See the workflow (imported by CLAUDE.md)
for how items are written and picked up.

The list is a rough stack rank: the order the items look worth doing, as a best
guess rather than a commitment. Re-order it freely. The large items near the
bottom are the rest of the roadmap in README.md and the wiki.

Each item carries:

- **Layers**: the OZ-3 libraries it touches (`core`, `tools`), in dependency
  order, and the wiki if it changes the specification. More than one usually
  means more than one CL.
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

## Default instruction set tests

- **Layers:** core
- **Size:** medium
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** `oz3/core/instruction_test_*.cc`

Finish testing the default instruction set, continuing the existing tests: one
change per instruction (or closely related group), each checking results,
flags, and exact cycle counts. Instructions with no tests yet: `CALL`, `CALLR`,
`RET`, `RETC`, `FBGN`, `FEND`, `EI`, `DI`, `GETI`, `INT`, `IRTC`, `IN`, `INR`,
`INS`, `OUT`, `OUTR`, `OUTS`, and `RST`. Bugs the tests find are fixed in the
`.izm` as part of the same change.

## Generate the default instruction set in the build

- **Layers:** core, tools
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** Default instruction set in CLAUDE.md

`default_instruction_set.inc` is generated from `default_instruction_set.izm`
by running `oz3ism` by hand, and checked in. Generate it in the build instead,
so the two can't drift. `oz3ism` links `oz3_core`, which compiles the `.inc`,
so the build has to break that cycle first: for instance, by moving the default
instruction set out of `oz3_core` into a library of its own that `oz3ism`
doesn't need. Decide whether the generated file stays checked in (so the source
builds without running `oz3ism`) or moves to the build tree.

## Rotate counts larger than the register

- **Layers:** core, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** the four `TODO` comments in the `RLC` and `RRC` code in
  `default_instruction_set.izm` (word and double word, left and right)

`RLC` and `RRC` rotate through the `C` flag one bit at a time, so a count from
a register larger than the rotation (17 bits for a word, 33 for a double word)
just spins, costing cycles for no effect. Reduce the count modulo the rotation
size first, and record the behavior (and cycle counts) on the wiki. The `TODO`
comments come out in the same change.

## Reset the port address when a lock is granted

- **Layers:** core, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** `PortBank::LockPort` in `oz3/core/port.cc`; `Lockable` in
  `oz3/core/lockable.h`; the Ports description in `2-Specifications.md`

Each port lock starts at word 0, but `PortBank::LockPort` resets the port
address when a lock is *requested*, even if the request is only queued behind
the current holder. A holder partway through an `A` mode sequence then has its
address reset under it: for instance, a device that writes word 0 with `A`,
keeps the lock for a few cycles, and then writes word 1, ends up overwriting
word 0 if a core executes `PLK` on the port in between, and if the device
moves the address on again, the core's lock then starts at word 1. A core that
holds a port is unaffected, as all its port reads and writes happen in the step
that runs `PLK`, but a core queued behind a device that holds a port across
cycles is affected both ways.

Reset the address when the lock is granted instead: for instance, a protected
virtual `OnLocked()` in `Lockable`, called both on an immediate grant and when
`Unlock()` hands the lock to the next pending request, which `PortLockable`
overrides. Test a request made while the port is locked. The wiki doesn't say
the address resets to word 0 when a port is locked at all, so document that
too.

## Default instruction set reference

- **Layers:** wiki
- **Size:** medium
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** the Instruction Set section of `2-Specifications.md`, which
  has a TODO link for it

A wiki page documenting the default instruction set for programmers: each
instruction, its variants and addressing modes, flags, and cycle counts. The
header comments on each instruction in `default_instruction_set.izm` already
have most of it.

## Program assembler (oz3asm)

- **Layers:** tools
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** the wiki's Home page; `ProgramLoader` in
  `oz3/tools/program_loader.h`

The assembler for OZ-3 programs, as a library and a command line tool. It
assembles source for any instruction set (using the syntax the instruction set
defines) into RAM and ROM modules that `ProgramLoader` can load into memory,
plus a `.oz3map` file mapping addresses back to source for the debugger.

## Debugger (oz3dbg)

- **Layers:** core, tools
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** *Program assembler (oz3asm)*
- **Background:** the wiki's Home page, and the `T` (trap) flag in
  `2-Specifications.md`

The debugger for OZ-3 programs, as a library and a tool: stepping (through the
`T` flag), breakpoints, watches, and source level debugging from `.oz3map`
files. Ozzy integrates the library.

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

## Devices

- **Layers:** devices (new), wiki
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** README.md

A `devices` library of independent virtual devices that attach to the OZ-3
through ports, as used by the Ozzy computer. Which devices are needed comes
from Ozzy's design.

## Ozzy computer

- **Layers:** ozzy (new), wiki
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** *Devices*, *Debugger (oz3dbg)*
- **Background:** README.md, and the wiki's Home page

Ozzy is a virtual computer and "OS" built from the OZ-3 core and the devices,
that runs OZ-3 programs and integrates the debugger.
