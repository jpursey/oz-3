# Backlog

Work that isn't being done yet. The worklogs in `docs/worklog/` describe what
was built; this is what has not been. See the workflow (imported by CLAUDE.md)
for how items are written and picked up.

The list is a rough stack rank: the order the items look worth doing, as a best
guess rather than a commitment. Re-order it freely. The large items near the
bottom are the rest of the roadmap in README.md and the wiki.

Each item carries:

- **Layers**: the OZ-3 libraries it touches (`core`, `tools`,
  `instruction_sets`), in dependency order, and the wiki if it changes the
  specification. More than one usually means more than one CL.
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

## Cycle ranges for every instruction

- **Layers:** instruction_sets
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** the `## Cycles:` lines in the header comments of
  `default_instruction_set.izm`; `RLC` and `RRC`, which already give a range
  per variant; the `*_ByRegisterCycles` and `*_ByValueCycles` tests in
  `instruction_test_rotate.cc`, which pin them

Most instruction headers give only a minimum (`4+`, `5+`), and some stated
ranges look stale (`NEG` says 5-9, but `NEG.W` appears to take 4). Give every
instruction an overall minimum and maximum, and a range per variant, as `RLC`
and `RRC` do:

    ## Cycles: 4-138
    ...
    ## Variants:
    ##    RLC.W <reg>, <reg>      (6-66 cycles)
    ##    RLC.W <reg>, <1..16>    (4-19 cycles)

Each range is worked out from the microcode and pinned by tests at both ends
of every variant, in the instruction's `instruction_test_<group>.cc`. Ranges
assume no lock contention, since waiting for a memory bank, port, or core lock
has no bound. Where the cost grows with a value, as `INR` and `OUTR` with the
count in `R7` and `WAIT` with its register, the header gives the cost per unit
instead, and the tests pin it. The plan is a CL per instruction group.

## Default instruction set reference

- **Layers:** wiki
- **Size:** medium
- **Feature workflow:** no
- **Depends on:** *Cycle ranges for every instruction*
- **Background:** the Instruction Set section of `2-Specifications.md`, which
  has a TODO link for it

A wiki page documenting the default instruction set for programmers: each
instruction, its variants and addressing modes, flags, and cycle counts. The
header comments on each instruction in `default_instruction_set.izm` already
have most of it.

## Instruction set source reference

- **Layers:** wiki
- **Size:** medium
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** `InstructionAssembler` in
  `oz3/tools/instruction_assembler.h`, `oz3ism.cc`, and
  `default_instruction_set.izm` as the working example; the wiki's Home page
  lists `oz3ism`

A wiki page documenting how an instruction set is written: the `.izm` source
format (instruction and macro definitions, argument encoding and sizes, and
macro registers such as `p`, `m`, `r`, and `i`), and how `oz3ism` assembles it
into C++. The microcode page covers only the microcode itself.

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

## Invalid register arguments

- **Layers:** core, tools, instruction_sets
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** *Program assembler (oz3asm)*
- **Background:** the `MVI` and `MVD` headers in `default_instruction_set.izm`,
  and the Repeating instructions section of
  `docs/worklog/finish-default-instruction-set.md`

Some register arguments are encodable but never useful, and today they are
only documented, as doing whatever the microcode does:
- `MVI`, `MVD`, `MVIR`, and `MVDR` with the same register for both addresses
  copy the word to the next address and step the register by two. Copying in
  place and stepping once would cost a cycle on every move, as microcode can't
  tell when both arguments are the same register.
- The repeating instructions (`CPIR`, `CPDR`, `MVIR`, `MVDR`, and `INR` and
  `OUTR`) with `R7`, their count, as an address register.
- `INR` and `OUTR` with the same register for the port and the address, or
  with `R7` as the port. The port changes as the register is stepped or
  decremented (for `OUTR`, before the word is even written).

Let an instruction definition declare argument combinations as invalid (such
as that its two register arguments must differ, or that one can't be `R7`),
in `InstructionDef` and the `.izm` source, so the program assembler rejects
them. Then declare them on the instructions above.

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

Raising an interrupt must report whether it was a duplicate (already pending),
as the Interrupts section of `2-Specifications.md` promises. Today
`CpuCore::RaiseInterrupt` and `Processor::RaiseInterrupt` return nothing.
Since `Processor::RaiseInterrupt` raises on every core, what it reports when
the trigger is new on some cores and a duplicate on others is decided here or
in *Devices*, whichever comes first.

## Devices

- **Layers:** devices (new), wiki
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** README.md

A `devices` library of independent virtual devices that attach to the OZ-3
through ports, as used by the Ozzy computer. Which devices are needed comes
from Ozzy's design.

Raising an interrupt must report whether it was a duplicate, as the spec
promises (see *Coprocessors*, which shares this, for the details).

## Ozzy computer

- **Layers:** ozzy (new), wiki
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** *Devices*, *Debugger (oz3dbg)*
- **Background:** README.md, and the wiki's Home page

Ozzy is a virtual computer and "OS" built from the OZ-3 core and the devices,
that runs OZ-3 programs and integrates the debugger.
