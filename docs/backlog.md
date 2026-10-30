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

## Assemble the default instruction set

- **Layers:** instruction_sets
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [assembler_design.md](assembler_design.md), and the wiki's
  Default Instruction Set page

Tests that assemble every form of the default instruction set, as the wiki's
reference writes it, and check each against its code word, and that run small
assembled programs on a core.

## Invalid register arguments

- **Layers:** core, tools, instruction_sets
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
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

## Flag value sets

- **Layers:** core, tools, instruction_sets, wiki
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** Text in `docs/worklog/instruction-forms.md`, and the `Flags`
  and `Banks` value sets in `default_instruction_set.izm`

Some value sets name bits that a program combines, such as `Flags` and `Banks`
in the default instruction set (`CLRF Z | C`). `InstructionForm::Disassemble`
writes a value by its name only when one name has it, so `CLRF Z | C`
disassembles as `CLRF 5`, which assembles the same but reads worse. Let a value
set be marked as flags, in `ValueSetDef`, the `.izm` source, and the exporter,
so a value is disassembled as its names joined by `|`. Then mark the default
instruction set's flag sets, whose pin test fingerprint includes its value
sets.

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

## Program grammar on newer Game Bits parser features

- **Layers:** tools
- **Size:** small
- **Feature workflow:** no
- **Depends on:** Game Bits *Negated rule items and position markers*,
  *Adjacent rule items*, and *Case of user tokens*
- **Background:** `docs/worklog/program-assembler.md` (`ProgramGrammar`)

`ProgramGrammar` works around three things the Game Bits parser and lexer
can't yet say, which each of those items adds:
- An instruction's arguments are any tokens to the end of the line, with
  brackets balanced, which needs a generated `ArgToken` rule of every symbol
  and keyword. A negated rule item ("any token but a line break or closing
  bracket") makes it a fixed rule, and a position marker names where the
  arguments start, even when there are none.
- A directive's name and an instruction's extension can't have spaces around
  their `.`, which `GetDirectiveName` and `GetInstructionName` check by token
  positions (`IsAdjacent`). Adjacent rule items put it in the grammar.
- Local label tokens keep their case, so `Expression` and the assembler turn
  their names to upper case themselves. A per user token case option lets the
  lexer do it.

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

## Command lines on Game Bits

- **Layers:** apps, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** Game Bits *Command line library*
- **Background:** `oz3/apps/oz3ism.cc` and `oz3/apps/oz3asm.cc`

`oz3ism` and `oz3asm` parse their command lines by hand. Define each one's
interface with the Game Bits command line library instead, with its help text,
keeping the arguments the wiki documents.

## Native paths in the command line tools

- **Layers:** apps, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** Game Bits *Native file paths*
- **Background:** `oz3/apps/oz3ism.cc` and `oz3/apps/oz3asm.cc`; CLAUDE.md
  (Default instruction set), and the wiki's `oz3ism` and `oz3asm` sections

`oz3ism` and `oz3asm` only open paths relative to the current directory, as
their file system can't open an absolute path with a drive letter. Open native
paths with Game Bits instead, so they take any path, and drop the limit from
CLAUDE.md and the wiki.

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
