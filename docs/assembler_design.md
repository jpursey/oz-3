# Program Assembler

The program assembler (`oz3asm`) turns OZ-3 assembly source into programs that
`ProgramLoader` can load, for any instruction set, using the syntax that
instruction set defines. This doc defines the assembler as a whole: its
principles, the model of a program it builds, how a statement is matched
against an instruction set, and what it writes. It ties together the backlog
items that build it (see [Getting there](#getting-there)), and it seeds the
wiki's assembler page, which becomes the public reference as those items land.

The concepts and rules here are what this doc decides. Spellings of directives
and options are a sketch, to make the model concrete, and each is settled in
the item that builds it.

## Principles

- **The instruction set owns instruction syntax.** The assembler knows no
  instruction, addressing mode, or condition. It matches each statement against
  the source patterns in the instruction set (`"$r, $m"` and the macro codes'
  forms), and encodes it with the instruction set's own `Encode()`. Anything
  else a program needs from an instruction set, such as names for conditions,
  is declared in the instruction set's source.
- **The hardware owns register names.** `$r` and `$R` mean registers by number,
  so `R0` to `R7`, `D0` to `D3`, and the special registers are named as the
  specification names them, for every instruction set.
- **The rest of the syntax is the assembler's, and never collides with a
  pattern.** Labels, directives, comments, and expressions use only what a
  pattern can't contain, or only where a pattern can't be. Where that needs a
  rule on patterns, the instruction set source gains the rule.
- **A statement means one thing.** A statement matches exactly one form of one
  instruction, or it is an error that lists every form it matched. The
  assembler never silently picks one.
- **Every address is a segment offset.** A core addresses each bank relative to
  its base register (`BC`, `BS`, `BD`, `BE`), and the loader points each base
  register at the start of its segment. So a label's value is its offset in its
  segment, and an assembled program needs no relocation: loading it anywhere
  changes none of its words.
- **The assembler writes what the loader takes.** Its output is a `Program`, so
  everything about placement (banks, segments, ROM and RAM, instances) is the
  `Program` model's, and anything assembled can be loaded by `ProgramLoader`.
- **The program file is the machine's own.** It is made of words, as the OZ-3
  has no bytes, so a loader written in OZ-3 code, such as an operating system's,
  can read it as easily as `ProgramLoader` does.
- **Every word traces back to its source.** The map records where each word
  came from, so the debugger can show source for any address.
- **Old school, and simple.** It reads like an assembler of the Z80 and 8086
  era: `;` comments, `label:`, `.DW`, `0FFh`, and names in any case. Line based
  statements, two passes, and no linker: a program is one source file and the
  files it includes.
- **Assembling is tested without running.** The library needs no `Processor`.
  Its tests use small instruction sets assembled from source text, and the
  default instruction set's syntax is tested where that set lives.

## The model at a glance

| Part             | What it is                                                                          | In code                      |
| ---------------- | ----------------------------------------------------------------------------------- | ---------------------------- |
| Program          | Banks, segments, the segments' roles, and instances                                 | `Program` (program.h)        |
| Segment          | An address space starting at 0, in a bank: ROM or RAM, fixed or anywhere            | `Program::Segment`           |
| Statement        | One line: a label, an instruction or a directive, and a comment                     | New                          |
| Symbol           | A label, a local label, or a constant: a name for an integer                        | New                          |
| Expression       | Integer arithmetic over numbers, characters, and symbols                            | New                          |
| Instruction form | An instruction with one of its macro's codes: a pattern, its encoding, and its size | New (`InstructionSetSyntax`) |
| Named values     | Names an instruction set gives the values of a code word argument                   | New, in `InstructionSetDef`  |
| Program file     | A `Program` as words, for any loader, including one written in OZ-3 code            | New                          |
| Program map      | Where each word came from, and every symbol, for the debugger                       | New                          |
| Listing          | The source beside the words it assembled to, as text                                | New                          |

## Programs

A program is what `Program` describes. The assembler adds nothing to it; it
fills it in from the source.

### Banks and segments

A program has one or more **banks**. These are the program's own, which the
loader assigns to physical banks, optionally limited to some of them
(`Program::Bank::assign_mask`). A program only names its banks if it has more
than one. Otherwise, every segment is in its one bank.

Each **segment** has:
- A **name**, unique in the program, which the source uses to switch to it.
- Its **bank**.
- Its **access**: RAM (read-write) or ROM (read-only). RAM is the default.
- Its **placement**: anywhere in the bank (the default), or starting at a fixed
  page.
- Whether it is **shared** by every instance (the default), or **per instance**
  (see [Instances](#instances)).
- Its **size** in pages: as many as its contents need (and its stack, if it has
  that role), or a number the source gives. Contents that don't fit are an
  error.
- Its **contents**: the words its statements assemble to, at their offsets.

Statements assemble into the **current segment**, which a directive switches,
so a segment's contents can be written in several places in the source and
include files.

Space the source reserves without giving its words (see [Data](#data)) is left
out of the segment's data, so the loader doesn't write it, and it holds
whatever was in memory. This is how RAM for variables costs nothing in the
program file. A segment's data is in blocks aligned to two words
(`Program::Segment::Data::kAlignment`), so where the source gives only one word
of an aligned pair, the other is written as 0.

### Roles

Each of the four bank roles (code, stack, data, and extra) is assigned to at
most one segment, and a segment can have several (which `ProgramLoader`
supports, though none of its tests covers it yet). A role the program doesn't
assign is left as it is when the program is loaded (`Program::code_segment`
and the others are `0xFFFF`), which is how an overlay or a data-only program
works.

- A program **starts at offset 0 of its code segment**, as resetting a core
  sets `IP` to 0.
- The **stack** grows down from the end of its segment, as `SP` starts at 0 and
  the first push wraps it to the last word.
- A program with a stack says how many words of stack each instance needs
  (`.STACK 256`), and its stack segment is sized to hold them, after its
  contents. Leaving it out is an error, as any program can use the stack: every
  `CALL`, `PUSH`, and interrupt does.

### The default program

A source that declares no segments is one RAM segment, placed anywhere, with
all four roles: code and data from offset 0, and the stack down from the end.
Its size is its contents and its stack, rounded up to whole pages, so it needs
no more than the program does, and loads into a bank of any size that has room.
It has one instance. This is the simplest program to write: instructions, data,
and `.STACK`.

### Instances

A program's **instances** are how many cores may run it at once, as an
operating system might run a program it loaded once as several processes. Each
instance has its own offsets for the base registers (`Program::Instance`), so
it has its own stack and variables, while sharing code, constant data, and
anything else the program wants shared.

- A segment is either **shared** by every instance or **per instance**. Code is
  always shared, so instances never offset `BC`. With more than one instance,
  the stack must be per instance, or the instances would share it. Data and
  extra are either: for example, a program might give each instance its own
  variables in data, and share a buffer between them in extra.
- The source describes **one instance's words** of a per instance segment, so
  its labels are offsets within them. The assembler sizes the segment to
  reserve those words for every instance, and sets each instance's offset to
  its own, so the same labels work for every instance. Nothing is copied.
- A per instance segment can only **reserve** space (`.DS`), never give words.
  The loader writes a program's data once, so only one instance could get it,
  and it is only fresh once anyway: an instance that runs again finds what the
  last run left. Each instance sets up its own variables, and initialized
  tables go in a shared segment.
- A program has one instance unless it says how many (`.INSTANCES 4`). Running
  several needs declared segments, as the default program's one segment is
  shared, holding its code and its stack.

## Source

- **Files:** a program is one source file, and the files it **includes**,
  each where the include is, as if its text were there. An included path is
  relative to the file that includes it. Including a file that is already being
  included is an error.
- **Lines:** a statement is one line: an optional label, then an optional
  instruction or directive, then an optional comment. A label on a line of its
  own labels the next statement in its segment.
- **Comments:** `;` starts a comment, which runs to the end of the line.
- **Case:** names are case-insensitive: instruction names, registers, the
  words patterns write literally, named values, directives, and symbols.
  `add r1, 5` is `ADD R1, 5`. Every name is turned to upper case as it is read,
  and that is the name a symbol has in the map and the listing. Strings and
  characters keep their case. An option for case-sensitive names, which then
  must be written as the instruction set and the specification write them, is
  a later backlog item.
- **Numbers:** decimal (`15`), hexadecimal with an `h` suffix (`0Fh`), or
  binary with a `b` suffix (`1111b`). A hexadecimal number starts with a digit,
  so `0FFh` and not `FFh`, which is a name. A suffix is in either case
  (`0FFH`).
- **Strings:** the OZ-3 has no bytes, so a string is either one character per
  word, or two packed into each word, and the quotes say which, one mark or
  two:
  - `'Hello'` is one character per word: five words.
  - `"Hello"` is two characters per word, the first in the high byte, and the
    last word padded with a zero byte: `4865h 6C6Ch 6F00h`. Each character must
    fit in a byte. Displays, ports, and code can unpack them as they like.

  A quote inside a string is doubled (`'It''s'`). Source files are UTF-8, and a
  character is its Unicode code point, which must fit in a word (or a byte,
  packed).
- **Characters:** in an expression, a string that makes one word is that word:
  `'A'` is 65, and `"AB"` is `4142h`.

### The assembler's own syntax

The instruction set decides how instructions are written. Everything else is
the assembler's, chosen so that it can't be mistaken for part of an
instruction:
- A **directive** starts with `.` (`.DW`), at the start of a statement, where
  an instruction name would be. Instruction names can't start with `.`, so no
  instruction set can collide with a directive, as it would with the bare
  directives of Intel and Zilog assemblers (`DW`, `ORG`).
- A **label** is a name followed by `:` at the start of a line.
- A **local label** starts with `@` (`@loop`), as microcode labels do, and is
  one token.
- A **comment** starts with `;`.

Patterns can't contain `;` or `@` (see [What the instruction set
declares](#what-the-instruction-set-declares)).

## Symbols

A **symbol** is a name for an integer. There are three kinds:
- A **label** is the offset of the statement it labels, in that statement's
  segment.
- A **local label** (`@loop`) is a label whose scope is between the labels
  around it, so every routine can have its own `@loop`. It is defined and used
  only in that scope.
- A **constant** is the value of an expression, given by a directive
  (`.CONST LINES = 25`).

`$` in an expression is the offset of the current statement, in its segment.
In a constant, it is the offset where the constant is defined.

A symbol's value is just a number. A label remembers its segment for the map
and the listing, but an expression can use any label with any other, as no
value needs relocating. Using a data label as a data address (`D(message)`) is
up to the program, as the assembler doesn't know which bank an address form
uses.

Symbols are global to the program, across include files, apart from local
labels. A symbol is defined once.

The only **reserved** names are the registers', which can't be defined as
symbols. They are a fixed set from the hardware, and a symbol named `R1` would
make `MOV R2, R1` match both a register and a value everywhere. Nothing else
needs reserving, as where a name is decides what it is:
- At the start of a statement, a label ends with `:`, and a directive starts
  with `.`, so anything else is an instruction name. A label can share a name
  with an instruction: `ADD: ADD R1, 5`.
- In an argument, a symbol that is also a word some pattern writes literally
  only collides where that pattern could match. With a constant `SELF`, `RST
  SELF, 1` matches both the `SELF` form and the `$#3` form, which is an error
  naming both, as any ambiguous statement is. `SELF` works as a symbol
  everywhere else, and a new instruction set version that adds a literal word
  can only break the statements that now collide.
- A named value is in scope only within its argument, where it takes
  precedence over a symbol of the same name, so a symbol named `C` never
  breaks `JC C, @done`.

## Expressions

An expression is integer arithmetic over numbers, characters, symbols, and `$`,
with C's operators and precedence: unary `-`, `~`, and `!`, then `*`, `/`, `%`,
`+`, `-`, `<<`, `>>`, `&`, `^`, and `|`, and parentheses.

As on the machine, arithmetic is in words unless it is in dwords, and **where a
value is used sets its width**, with no syntax for it:
- A **word**: `$v`, a code word argument (`$#5`, or named values), and `.DW`.
- A **dword**: `$V` and `.DD`.
- A **constant** is its expression, evaluated where it is used, in that width:
  `.CONST MASK = ~1` is `0FFFEh` as a word, and `0FFFFFFFEh` as a dword. Its
  value in the map is its dword value.

Every step of an expression, not just its result, must fit its width, or it is
an error, never a silent wrap. A word is -32768 to 65535, so `-1` is `0FFFFh`,
and a dword is -2^31 to 2^32 - 1. A code word argument must also fit its bits,
from 0 to its largest unsigned value. So `.DW A * B / C` is an error if `A * B`
doesn't fit a word, even if the result would. That makes a dword's low or high
word impossible to write in a word (`X & 0FFFFh` is an error where `X` doesn't
fit one), which is left to *Low and high words*.

## Instructions

### Forms

The assembler reads an instruction set's **forms** from its `InstructionSetDef`:
- An instruction without a macro argument has one form, its source pattern.
- An instruction with one has a form for each of its macro's codes: its source
  with the `$m` replaced by the code's source. `MOV.LW "$r, $m"` has 11 forms,
  `$r, $r` to `$r, E($v)`.

Each form knows its code word (from its opcode, its arguments, and its macro
code's prefix, through `InstructionDef::Encode()` and `MacroDef::Encode()`), and
its size: the code word, and one word for each `$v` (two for each `$V`), in the
order the pattern lists them. The same table of forms serves both ways: the
assembler matches statements to code words, and the debugger turns code words
back into text.

### Matching

A statement's instruction name picks the instructions it may be:
- A full name (`ADD.W`) is that instruction.
- A name without an extension (`ADD`) is every instruction with that name,
  with or without an extension (`ADD.W`, `ADD.D`, and `ADD.S`).

The rest of the statement is then matched against each form of each of those
instructions, token by token. Spacing doesn't matter.
- **Literal text** matches the same token, ignoring case for a word. A literal
  number matches any expression that has that value when the statement is
  reached (see [Passes](#passes)), so `SHL R1, BITS` works where `BITS` is 3,
  and the pattern has a code for `3`.
- **`$r` and `$R`** match a register name the argument can hold: `$r` is `R0`
  to `R7`, `$r2` only `R0` to `R3`, and `$r4` adds `BC`, `BS`, `BD`, `BE`,
  `MB`, `IP`, `FP`, and `SP`. `$R` is `D0` to `D3`.
- **`$#`, named values, `$v`, and `$V`** match an expression: the longest one
  at that point. A pattern can't follow a value with a binary operator, which
  would continue the expression, so a value always ends at the pattern's next
  literal. Registers are never part of an expression, so a register never
  matches a value.
- **Brackets are the pattern's.** A value can't be wrapped in brackets as a
  whole. Otherwise `JP (5)` would quietly match `JP $v` and jump to 5, where
  the program meant the word in memory at 5, which the default instruction set
  doesn't have. Brackets inside a value are fine (`(SP + (N + 1) * 2)`). As
  brackets around a whole value are never needed, a program can always leave
  them out, so there is no escape to force a value.

A statement must match exactly one form. Matching none is an error that lists
the instruction's forms. Matching more than one is an error that lists those it
matched, so the program names the one it means: `PUSH 5` matches both `PUSH.W
$v` and `PUSH.D $V`, so it is written `PUSH.W 5`. No form is preferred over
another, such as the one with fewer words. The default instruction set never
needs a preference, and a rule could be added if another instruction set does.

### Encoding

The matched form gives the code word, with each argument's value encoded in
its bits, followed by the words for each `$v` and `$V`, low word first for a
dword, as the specification stores dwords.

### What the instruction set declares

The instruction set source gains what programs need, without changing any of
the default instruction set's code words. Its pin test fingerprints syntax as
well as code words, as program source depends on both, so the fingerprint
changes once, which is safe only because no program has been assembled yet.
From then on, the fingerprint covers named values and relative instructions
too, so they never change either.
- **Named values.** An instruction set names the values of a code word
  argument, with a size in bits, and a pattern uses the names as a placeholder
  in place of `$#`. The default instruction set's conditions and flag masks
  become:

      values(bits:3) Condition { NZ = 0, NS = 1, NC = 2, NO = 3,
                                 Z = 4, S = 5, C = 6, O = 7 }
      values(bits:4) Flags { Z = 1, S = 2, C = 4, O = 8 }

      instruction JC "$Condition, $m" { ... }
      instruction CLRF "$Flags" { ... }

  The argument is still an expression, with the names in scope only within it,
  so a program can write `JC Z, @done` and `CLRF Z | C`, where `Z` is 4 in one
  and 1 in the other. It encodes exactly as `$#3` and `$#4` did.
- **Relative values.** `JPR`, `JCR`, `JDR`, and `CALLR` add their value to `IP`,
  which is past the instruction. An instruction marks itself relative
  (`instruction(relative) JPR "$m"`), and then a `$v` that is a whole argument
  is a target address, which the assembler encodes as its offset from the next
  instruction. So `JPR @loop` jumps to `@loop`, as
  `JP @loop` does. Its other forms are unchanged: `JPR D(table)` still reads
  the offset from memory. Marking the instruction, rather than the `$v`, keeps
  the relative instructions on `GetWord`, where marking the placeholder would
  need a copy of `GetWord` with the same layout, kept in sync by hand.
- **Patterns can't contain `;` or `@`**, so that comments and local labels never
  collide with an instruction's syntax.
- **A value can't be followed by a binary operator** (`+`, `-`, `*`, `/`, `%`,
  `&`, `|`, `^`, `<`, or `>`) in a pattern, as the value's expression would
  take it (see [Matching](#matching)).
- **Sizes match the microcode.** The assembler sizes an instruction from its
  pattern, so assembling an instruction set checks that each form's size in its
  pattern is the size the compiled instruction decodes to (see Arguments in the
  wiki's Instruction Set Source page). `CompileInstructionSet` doesn't, as it
  only checks what cores need to run an instruction set.

## Data

Directives put data into the current segment, named after Intel's `DW`,
`DD`, and `DS`, though the OZ-3 has no bytes:
- `.DW` (define words) and `.DD` (define dwords) write a list of expressions,
  one word or dword each. A string in a `.DW` list is its words (see
  [Source](#source)), so a zero terminated string is `.DW 'Hello', 0`, or
  packed, `.DW "Hello", 0`.
- `.DS` (define storage) reserves a number of words without writing them (see
  [Banks and segments](#banks-and-segments)). It is all a per instance segment
  can hold.

## Passes

The assembler makes two passes over the source:
1. **Layout:** reads every statement, matches each instruction to its form,
   and so gives every statement its size and offset, and every label its value.
2. **Encoding:** evaluates every expression, now that every label is known,
   and writes the words.

Matching happens in the first pass, so anything that decides a statement's form
or size must be known when the statement is reached: a code word argument
(`$#`, named values, or a literal number), and a `.DS` size. A constant used
there must be defined above it. Labels and constants used anywhere else, which
is nearly everywhere (`$v`, `$V`, and data), may be defined later.

### Errors

Errors give the file, line, and column, like the instruction set assembler's.
The assembler reports every error in a pass, up to a limit, rather than only
the first, as each statement is one line and an error in one doesn't confuse
the next. A pass with errors is the last one, as the second pass's errors would
mostly follow from the first's. A source with errors produces no output.

## Output

### Program

The assembler's result is a `Program`: its banks, its segments with their data
blocks, its role assignments, and its instances. A host application that
assembles at run time loads it straight into `ProgramLoader`.

### Program file

A program file holds one `Program`, so a program can be assembled once and
loaded many times. It is the OZ-3's executable format, as `.COM` and `.EXE`
files were for CP/M and DOS, so it is written for a loader in OZ-3 code as much
as for `ProgramLoader`:
- It is a sequence of words, as an OZ-3 program would read them from a storage
  device: a header with an identifying word and a version, then the banks, the
  segments, the role assignments, and the instances, and last the segments'
  data. Every field of `Program` is already a word.
- Everything a loader needs to place the program comes before the data, so it
  can allocate every segment, or give up, before it writes any memory, and then
  copy the data straight from the device to where it goes.
- It needs no relocation, so a loader only allocates pages, copies words, and
  sets base registers.
- Its format is specified in the wiki (Program Files), as part of the machine,
  since programs that load programs depend on it. On a host, each word is two
  bytes, low byte first.

`ProgramLoader` still loads a `Program`, and reading and writing the file is
separate (`oz3/tools/program_file.h`).

### Program map

The map is a separate file, so a program loads without it. Only the debugger
reads it, which is always a host tool written in C++, so it is a Game Bits
chunk file (`gb/file`'s chunk reader and writer), and holds:
- The source files, by path.
- Every segment's name.
- For every statement that wrote words: its segment, offset, size, and source
  location.
- Every symbol: its name, its value, its segment (for a label), and where it is
  defined.

With `ProgramLoader`'s record of where each segment was loaded, the debugger
can turn any address into a segment offset, and from there into source and
symbols.

### Listing

A listing is text for people: each source line, with its segment offset and the
words it assembled to. It is optional, and is what a programmer reads to check
what an instruction became.

## Library and tool

The assembler is a library in `tools`, which a host application can use on its
own, and `oz3asm` is the command line tool built on it.

```
// Every form of every instruction in an instruction set, for matching
// statements to code words, and code words back to text.
class InstructionSetSyntax final {
 public:
  static std::unique_ptr<InstructionSetSyntax> Create(
      const InstructionSetDef& def,
      const InstructionSet* instruction_set = nullptr,
      InstructionError* error = nullptr);
  ...
};

// An assembled program, and its map.
class AsmProgram final {
 public:
  const Program& GetProgram() const;
  const ProgramMap& GetMap() const;
};

std::unique_ptr<AsmProgram> AssembleProgramFile(
    const InstructionSetSyntax& syntax, gb::FileSystem& file_system,
    std::string_view path, std::vector<gb::ParseError>* errors);

bool WriteProgramFile(gb::FileSystem& file_system, std::string_view path,
                      const Program& program);
std::optional<Program> ReadProgramFile(gb::FileSystem& file_system,
                                       std::string_view path);
```

- The assembler takes an `InstructionSetSyntax`, built from any
  `InstructionSetDef`, so it works the same for the default instruction set
  (compiled in), and one assembled from source at run time
  (`AsmInstructionSet`). A host creates the syntax once for every program it
  assembles, and handles its error.
- `InstructionSetSyntax` is built once per instruction set, and indexes forms
  by instruction name, so matching a statement only tries that name's forms. It
  has no `Asm` prefix, as the debugger uses it too.
- The names a program writes from an instruction set (instructions and named
  values) are case-insensitive in instruction set source too, and are upper
  case in an assembled `InstructionSetDef`. A def written by hand can't have
  two that differ only in case, such as instructions `ADD` and `add`, which
  `InstructionSetSyntax` reports as an error, as a program couldn't tell them
  apart.
- `tools` tests assemble programs for small instruction sets written in their
  tests. `instruction_sets` tests assemble every form of the default
  instruction set and check its code words against the reference, and run small
  programs on a core.
- `oz3asm` takes a source file and an instruction set, and writes the program
  file, and optionally the map and the listing. With no instruction set given,
  it uses the default one, so it links `oz3_instruction_sets`, which `tools`
  can't. So it lives in a new `apps` directory, for executables, after
  `instruction_sets` in the dependency order, where `oz3dbg` goes too. The
  libraries keep their order, and `oz3ism` stays in `tools`, as it must build
  when the default instruction set doesn't.

## Example

A program for the default instruction set that writes a zero terminated
message to port 1:

```
; Writes a message to the console port, a word at a time.
.CONST CONSOLE = 1
.STACK 64

.SEGMENT main CODE
start:
    MOV R1, message     ; MOV.LW: R1 is the message's address in DATA
@next:
    MOV R2, (R1)        ; MOV.LW, the ($r) form
    TST R2              ; TST.W
    JC Z, @done         ; Z is the Condition value 4
    OUT CONSOLE, R2     ; OUT.IW: CONSOLE is the port number
    ADQ R1, 1
    JP @next
@done:
    HALT

.SEGMENT strings DATA ROM
message:
    .DW 'Hello', 0

.SEGMENT stack STACK
```

The stack segment has no contents, so it is one page, holding the 64 words of
`.STACK`. Every instruction names its root only, and each matches one form: `OUT
CONSOLE, R2` can't be `OUT.ID`, as `R2` isn't a dword register, and `OUT
CONSOLE, 5` would be an error, matching both `OUT.IW` and `OUT.ID`. Written
in lower case (`mov r1, message`), it assembles the same.

## Names

| Name             | Candidates                                    | Avoids                                                                       | Recommendation                                       |
| ---------------- | --------------------------------------------- | ---------------------------------------------------------------------------- | ---------------------------------------------------- |
| Source file      | `.oza`, `.asm`, `.oz3s`                       | `.izm` (instruction set source); `.asm` is every assembler's                 | `.oza`                                               |
| Program file     | `.oz3`, `.ozp`, `.oz3prg`                     | `.oz3map`, which it pairs with                                               | `.oz3` (`hello.oz3` and `hello.oz3map`)              |
| Table of forms   | `InstructionSetSyntax`, `InstructionSetForms` | `InstructionSet` (compiled microcode), `InstructionSetDef` (its source data) | `InstructionSetSyntax`, holding `InstructionForm`s   |
| Assembled result | `AsmProgram`, `AssembledProgram`              | `Program`, `LoadedProgram`                                                   | `AsmProgram`, after `AsmInstructionSet`              |
| Map              | `ProgramMap`, `SourceMap`, `DebugMap`         | `MemoryMap` (core's memory mapped devices)                                   | `ProgramMap`, after `.oz3map`                        |
| Named values     | `values`, `enum`, `names` (`ValueSetDef`)     | `$v`/`$V` placeholders, which are values in words                            | `values`, with `ValueSetDef` in `InstructionSetDef`  |
| Executables      | `apps`, `cli`, `cmd`                          | `bin/` (where executables are built), `tools` (libraries)                    | `apps`, as the debugger may not be command line only |

"Form" is already the wiki's word for one code of a macro ("each form is a
`code` block"), so an instruction form is the instruction with one of them.

## Getting there

The backlog items build this in order, each updating the wiki as it goes, the
first of them creating the assembler's page:
1. *Instruction set syntax for programs* (core, tools, instruction_sets, wiki):
   named values, relative instructions, and the pattern rules, with the default
   instruction set using them and its code words unchanged.
2. *Check instruction sizes against microcode* (tools): the instruction
   assembler checks each form's pattern size against its compiled microcode.
   Independent, and can be done any time before the assembler relies on
   pattern sizes.
3. *Program files* (tools, wiki): the word format, and its specification.
   Independent, and can be done first.
4. *Instruction forms* (tools): `InstructionSetSyntax`, its forms, encoding
   them, and turning code words back into text.
5. *Program assembler* (tools, wiki): statements, matching them to forms,
   symbols, expressions, segments, instances, data, and the two passes,
   producing a `Program`.
6. *Program maps* (tools): the map, written by the assembler, and read back.
7. *oz3asm* (apps, wiki): the command line tool, with the listing, and the new
   `apps` directory.
8. *Assemble the default instruction set* (instruction_sets): every form of
   the default instruction set, checked against its reference code words, and
   small programs run on a core.

Then *Invalid register arguments* and *Debugger (oz3dbg)* build on it. These
are left out of the first version, and wait in the backlog until they are
wanted: *Program macros and conditional assembly* (`.MACRO` and `.IF`, which
will almost certainly be wanted), *Instruction set identity in program files*,
and *Case-sensitive programs*.
