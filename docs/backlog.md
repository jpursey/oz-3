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

## Finish the default instruction set

- **Layers:** core, wiki
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** `default_instruction_set.izm` and its `instruction_test_*.cc`
  tests; `INR` and `OUTR`, the existing repeat instructions; the Z80's `DJNZ`,
  `CPI`/`CPIR`, and `LDI`/`LDIR` families, which these riff on; the list of
  changes from earlier versions at the top of `2-Specifications.md`

Sixteen instructions the default instruction set still lacks, in four groups,
each likely a CL with its tests (results, flags, and exact cycle counts):

- **Multiply and divide:** `MUL.W` (unsigned multiply), `MULS.W` (signed
  multiply), `DIV.W` (unsigned divide), `DIVS.W` (signed divide), `MOD.W`
  (unsigned modulo), and `DVMD.W` (unsigned divide and modulo together). All
  are word sized, taking a word register and a word value, except `DVMD.W`,
  whose first operand is a dword register: it divides the register's low word,
  and gets the quotient in one word and the remainder in the other. There is
  deliberately no signed modulo. The microcode only adds, subtracts, and
  shifts, so these are shift and add or subtract loops, with cycle counts that
  depend on the operands. If the microcode stays reasonable, add `.DW` variants
  of multiply, divide, and modulo whose first operand is a full dword
  register.
- **Decrement and jump:** `JD` and `JDR` decrement a register and jump if it
  isn't zero, to an address or relative to `IP` (like `JP` and `JPR`). This is
  the Z80's `DJNZ` on any register, and the microcode's `JD` already does it.
- **Block compare:** `CPI` and `CPD` compare a register with a word in memory,
  step the address up or down, and decrement a count. `CPIR` and `CPDR` repeat
  until a match or the count runs out.
- **Block move:** `MVI` and `MVD` copy a word from memory to memory, step both
  addresses up or down, and decrement a count. `MVIR` and `MVDR` repeat until
  the count runs out.

The repeating forms (`CPIR`, `CPDR`, `MVIR`, and `MVDR`) do one word per
execution and then repeat the instruction until they are done, as the Z80 does,
for instance by moving `IP` back to it, so interrupts can be handled between
words. If that is practical, move `INR` and `OUTR` to the same approach, which
today run their whole loop within one instruction and hold off interrupts until
it finishes. Their cycle counts change, and so does stepping them with the `T`
flag.

`2-Specifications.md` says the default instruction set has no memory to memory
operations, and leaves block copies to the DMA coprocessor. Update it to say
the default instruction set has block moves, one word at a time, while the DMA
coprocessor copies blocks much faster over a memory bus of its own.

The design decides:

- The flags multiply and divide set (the spec has `O` double as the error flag,
  for instance on a divide by zero, and a word sized product can overflow),
  and which word of `DVMD.W`'s register gets the quotient.
- Which registers the block instructions use for the addresses and the count,
  and whether they are fixed (as `INR` and `OUTR` fix the count in `R7`) or
  encoded. Each register implies a memory bank (`R0` to `R3` are `DATA`, `R4`
  and `R5` are `EXTRA`), so the choice also decides which banks a block can
  move between. This needs the option space laid out with the user.

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
