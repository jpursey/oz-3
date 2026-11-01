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

## Devices in core

- **Layers:** core, wiki
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** nothing
- **Background:** [ozzy_design.md](ozzy_design.md) (Device support in core);
  the Interrupts section of `2-Specifications.md`

Devices that the `Processor` runs in step with the cores, added through
`ProcessorConfig`: each is given its ports, can lock memory banks to read and
write memory, and raises interrupts on the cores its line is connected to.

Raising an interrupt must report whether it was a duplicate (already pending),
as the Interrupts section of `2-Specifications.md` promises. Today
`CpuCore::RaiseInterrupt` and `Processor::RaiseInterrupt` return nothing. The
design's answer is that a raise is a duplicate only if it was already pending
on every core its line is connected to.

## Console devices

- **Layers:** devices (new)
- **Size:** medium
- **Feature workflow:** yes
- **Depends on:** *Devices in core*
- **Background:** [ozzy_design.md](ozzy_design.md) (Screen, Keyboard,
  Teletype)

The new `devices` library, with the memory mapped screen (character and
attribute cells, control words, a scroll row, and a refresh rate with a frame
counter), the keyboard (a queue of key
events on a port, with an interrupt), and the teletype (packed text written to
the host, for an Ozzy without a screen).

## Ozzy machine

- **Layers:** ozzy (new), apps, wiki
- **Size:** large
- **Feature workflow:** yes
- **Depends on:** *Console devices*
- **Background:** [ozzy_design.md](ozzy_design.md)

The first Ozzy anyone can run: the `ozzy` library builds a machine from a
`gb::Config` text file, writes the machine table and the boot ROM, and starts
core 0, and the `ozzy` app runs it in the Windows console in real time. The
boot ROM prints a banner and echoes keys. Creates Ozzy's wiki page.

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

## Fixed size strings

- **Layers:** tools, wiki
- **Size:** small
- **Feature workflow:** no
- **Depends on:** nothing
- **Background:** [ozzy_design.md](ozzy_design.md) (File names); the wiki's
  Program Source page (Values)

A way to write a string padded with zeros to a fixed number of words, such as
an 8 word file name field, where `.DW "A.TXT"` writes only 3 words today.

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

Coprocessors raise interrupts as devices do, reporting duplicates (see
*Devices in core*), and join Ozzy's config once they exist.
