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

## Check instruction sizes against microcode

- **Layers:** core
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [program_assembler.md](program_assembler.md) (What the
  instruction set declares), and Arguments in the wiki's Instruction Set Source
  page

The program assembler sizes an instruction from its source pattern (the code
word, one word for each `$v`, and two for each `$V`), but nothing checks that
the microcode reads that many words. Compiling an instruction set checks, for
every instruction and macro code with a source, that the pattern's size is the
size the compiled instruction decodes to.

## Program files

- **Layers:** tools, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [program_assembler.md](program_assembler.md) (Program file);
  `Program` in `oz3/tools/program.h`

The OZ-3's executable format: a `Program` as a sequence of words (a header with
an identifying word and a version, then banks, segments and their data, role
assignments, and instances), which a loader written in OZ-3 code could read as
easily as C++. Reading and writing it, so a program can be assembled once and
loaded many times, and its specification in the wiki. Also fixes two comments: `ProgramLoader`
refers to a `program_exporter.h` that doesn't exist, and `Program::Instance`
says a stack offset should be negative, where the loader subtracts it from the
segment's end.

## Instruction forms

- **Layers:** tools
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** [program_assembler.md](program_assembler.md) (Instructions)

`InstructionSetSyntax`: every form of every instruction in an
`InstructionSetDef` (each instruction with each of its macro's codes), with its
code word layout and size. It matches a statement's tokens to exactly one form
and encodes it, ignoring case, reporting a statement that matches none or
several, and it turns a code word back into text for the debugger.

It parses source patterns, as the instruction assembler already does in two
places (`AssembleInstructionCodeSource` and `AssembleMacroCodeSource` in
`instruction_assembler.cc`), each creating the pattern parser and walking its
placeholders. One shared pattern parser in tools would serve all three, along
with the mapping from a placeholder to its `Argument`, which is split today
between `IsPlaceholderName` in core and the assembler's `AssembleArgument` and
`IsValuePlaceholder`. Once it exists, the default instruction set's pin test
(`default_instruction_set_test.cc`) can fingerprint each instruction's resolved
forms, through a test-only dependency on tools, rather than scanning patterns
for value sets itself.

## Program assembler

- **Layers:** tools, wiki
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** *Instruction forms*
- **Background:** [program_assembler.md](program_assembler.md)

The assembler library: source files and includes, statements, symbols (labels,
local labels, and constants), expressions sized by where they are used,
segments and their roles, banks, the stack, shared and per instance segments,
and data (including strings of one or two characters per word), assembled in
two passes into a `Program`, with every error in a pass reported. Its syntax is
in the style of Z80 and 8086 era assemblers (`;` comments, `.DW`, `0FFh`), and
names are case-insensitive. It creates the wiki's assembler page, the public
reference for the program syntax.

## Program maps

- **Layers:** tools
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** *Program assembler*, *Program files*
- **Background:** [program_assembler.md](program_assembler.md) (Program map)

The `.oz3map` file that the assembler writes beside a program: its source
files, its segments, where each statement's words came from, and every symbol.
Read back, it lets the debugger turn an address in a loaded program into source
and symbols.

## oz3asm

- **Layers:** apps (new), wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** *Program assembler*, *Program files*, *Program maps*
- **Background:** [program_assembler.md](program_assembler.md) (Library and
  tool)

The command line program assembler: a source file and an instruction set (the
default one unless given a `.izm` file) to a program file, and optionally its
map and a listing. It links the default instruction set, so it starts a new
`apps` directory for executables, after `instruction_sets` in the dependency
order, which CLAUDE.md's directory structure gains.

## Assemble the default instruction set

- **Layers:** instruction_sets
- **Size:** small
- **Feature workflow:** no
- **Depends on:** *Program assembler*
- **Background:** [program_assembler.md](program_assembler.md), and the wiki's
  Default Instruction Set page

Tests that assemble every form of the default instruction set, as the wiki's
reference writes it, and check each against its code word, and that run small
assembled programs on a core.

## Invalid register arguments

- **Layers:** core, tools, instruction_sets
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** *Instruction forms*
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
- **Depends on:** *Program maps*, *Instruction forms*
- **Background:** the wiki's Home page, and the `T` (trap) flag in
  `2-Specifications.md`

The debugger for OZ-3 programs, as a library and a tool: stepping (through the
`T` flag), breakpoints, watches, and source level debugging from `.oz3map`
files. Ozzy integrates the library.

## Program macros and conditional assembly

- **Layers:** tools, wiki
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** *Program assembler*
- **Background:** [program_assembler.md](program_assembler.md)

Macros in program source (`.MACRO`), for the boilerplate the instruction set
doesn't cover, and conditional assembly (`.IF`), such as for building one
source for different hardware configurations.

## Case-sensitive programs

- **Layers:** tools, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** *Program assembler*
- **Background:** [program_assembler.md](program_assembler.md) (Source)

An assembler option for case-sensitive names, which must then be written as
the instruction set and the specification write them. Without it, every name is
turned to upper case as it is read.

## Instruction set identity in program files

- **Layers:** tools
- **Size:** small
- **Feature workflow:** no
- **Depends on:** *Program files*
- **Background:** [program_assembler.md](program_assembler.md) (Program file)

A program file records which instruction set it was assembled for, such as a
fingerprint of its code word layout, so loading it for a core with a different
instruction set can be caught.

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
