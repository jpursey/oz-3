# Program macros and conditional assembly

Program source has **macros**, for the boilerplate the instruction set
doesn't cover, **repeated blocks**, for tables and unrolled code, and
**conditional assembly**, for building one source for different hardware, as
the assemblers of the Z80 and 8086 era had them (`MACRO`, `REPT`, `IF`). A
macro is a named block of lines with parameters, invoked where an instruction
would be, and each invocation assembles its lines with the arguments in place
of the parameters:

```
.CONST KEYBOARD = 2

; Reads a port until its word is nonzero.
.MACRO WAITIN port, reg
@wait:
    IN port, reg
    TST reg
    JC Z, @wait
.ENDM

start:
    WAITIN KEYBOARD, R1
    WAITIN KEYBOARD, R2     ; Its own @wait, apart from the first's
    ...
```

A repeated block assembles its lines a number of times where it is written,
with its index, if it has one, in place of its name:

```
squares:
.REPT 16, i
    .DW i * i
.ENDR
```

Conditional assembly assembles a block of lines only if an expression is
nonzero, so a configuration can choose what a program has. Expressions have
C's comparisons for it, and a host, or `oz3asm -D`, can give a program
constants of its own, so one source builds for each configuration without a
file per configuration:

```
.INCLUDE "config.oza"       ; .CONST SCREEN = 1, for an Ozzy with a screen

.IF SCREEN
    CALL screen_init
.ELSEIF TELETYPE != 0
    OUT TELETYPE, 0Ch
.ENDIF
```

The manual's [Program Source](../manual/2.5-Program-Source.md) is the
specification: the comparisons in Expressions, the directives table,
Conditional assembly, Macros, Repeated blocks, the listing's rows for an
invocation, and Constants from the host and `oz3asm -D` in Assembling a
program. [Instruction Set Source](../manual/2.3-Instruction-Set-Source.md) has
the rule for symbols that join where a macro code meets its instruction's
pattern. Everything is in `tools` and `oz3asm`, and no program that assembled
before changed. This summary covers how it is built, and the decisions behind
it.

It was built as five CLs (comparisons, conditional assembly, macro text,
macros, and constants from the host). Repeated blocks were left out of the
plan and done after them as a change of their own, and the hand-rolled token
text and adjacency of the macro CLs were replaced by Game Bits' lexer
functions once those landed.

## Behavior

The decisions a reader of the manual might wonder about:
- **Comparisons are unsigned,** as `/` and `%` are, in the width the
  expression is used in, as the machine's `CMP` compares with `C`, and at C's
  precedence, so with the bitwise operators on 0 and 1 there is no need for
  `&&` and `||`. The lexer reads `<=`, `>=`, `==`, and `!=` as one symbol, in
  a program and in an instruction set's patterns alike.
- **An `.IF`'s expression is a word,** known as the `.IF` is read, as the
  other directives' values are. A dword constant is tested through `HIGH()`
  and `LOW()`, rather than through a dword `.IFD`, which would read close to
  `.IFDEF`.
- **Lines that aren't assembled aren't read,** except to find the directives
  that end their block, so they can hold anything. An `.IF` ends in the
  content it starts in: its file, or its invocation or repetition.
- **A macro's arguments are text,** split at commas outside brackets, and
  each parameter is replaced wherever its name is a word in the body, other
  than in a string or a local label. A macro can invoke other macros, but not
  itself, even through another.
- **Labels:** a local label a macro's body defines is each invocation's own,
  and one in an argument is the caller's. A repeated block renames the local
  labels its lines start with, so each repetition has its own, and any other
  is the one where the block is written. A body can't define a global label or
  a constant of its own, as every invocation or repetition would define it
  again. In the map, a renamed label is `WAITIN#2@WAIT` (the second invocation
  of `WAITIN`) or `REPT#3@WAIT` (the third repetition in the program).
- **Everything an invocation or a repetition writes is at the line that
  invokes it,** in the map and the listing, so the debugger shows the line the
  programmer wrote. An error in a body is at the token at fault where it was
  written, followed by an error at each invocation it is in ("In WAITIN", or
  "In .REPT"), from the innermost out, so a tool can jump to each.
- **A host's constants** are defined above the program's first line, so
  `.IF` and `.IFDEF` can test them, and aren't in the map, as no line defines
  them.

## Design

### Names

| Name                       | Chosen                                               | Avoids                                                                  |
| -------------------------- | ---------------------------------------------------- | ----------------------------------------------------------------------- |
| Directives                 | `.MACRO`/`.ENDM` and `.REPT`/`.ENDR`, MASM's         | Nothing: directives start with `.`                                      |
| Conditionals               | `.IF`/`.ELSEIF`/`.ELSE`/`.ENDIF`, `.IFDEF`/`.IFNDEF` | `.ELIF` (C's, not an assembler's)                                       |
| A macro, in C++            | `ProgramMacro`                                       | `MacroDef` and `$m`, the instruction set's macros (core)                |
| One invocation's text      | `MacroExpansion`                                     | "Instance" (a program's instances)                                      |
| A renamed label in the map | `WAITIN#2@WAIT`, `REPT#3@WAIT`                       | `START@LOOP`, a scope's local label; `.`, which reads like an extension |
| Host constants             | `AsmConstant`, after `AsmProgram`                    | `Constant` (the map's), `ConstantStatement`                             |
| Flag                       | `-D`/`--define`, as C compilers have it              | `-d`, which some tools use for debug                                    |

"Macro" already means the instruction set's macros, the `$m` argument and its
codes. The manual keeps the two apart by where they are: the Program Source
page's macros are always the program's, and it says once that they are
unrelated to an instruction's macro argument.

### Comparisons (`expression.cc`, `program_lexer.cc`, `source_pattern.cc`)

- `Expression`'s rules have `ExpressionCompare` (`<`, `<=`, `>`, `>=`) and
  `ExpressionEqual` (`==`, `!=`) between `ExpressionShift` and
  `ExpressionAnd`, folded left to right as the others are. A comparison's
  result is in no segment, and two offsets in different segments are an
  error, as for any other operator.
- A pattern's lexer has the same symbols of two characters as the program's,
  as binary operators a value can't be followed by. `SourcePattern`'s
  `ValidateParts` also rejects a symbol right before one it joins with, where
  a macro code meets an instruction's pattern (`"[!$m]"` with a code `"=$v"`).

### Conditional assembly (`program_grammar.cc`, `program_assembler.cc`)

- The grammar's `Directive` has the conditional directives as alternatives,
  so an assembled line reads them as it reads any other.
- `ReadContent` keeps a stack of the `.IF`s open in the content it reads
  (`OpenIf`), so each file, invocation, and repetition has its own, and one
  left open at the end of the content is an error there.
- While a block is skipped, `SkipLine` only scans each line with the lexer for
  a `.` with a conditional directive's name right after it, past a label if
  there is one, which is then an error once the directive is read. Nothing
  else on the line is lexed, and the lexer's errors are ignored. A directive
  that ends the block puts its first token back (`SetNextToken`), for the
  parser to read the line.

### Macros (`program_macro.h`, `program_assembler.cc`)

A macro is token substitution: its body is token ranges in the content it was
defined in, and an invocation's expansion is that text with each parameter
replaced by its argument's text. The assembler reads the expansion as it
reads an included file.

- **`ProgramMacro`** is a macro, or a repeated block (`Kind::kRepeat`, whose
  index is its one parameter). `ReadLine` reads the body a line at a time with
  the assembler's lexer, keeping each line's first and last token and each
  token an expansion may replace (`Replace`): a parameter's name, or a label.
  It validates everything a body's line can have where the body is defined:
  its labels, a `.CONST` whose name isn't a parameter, a parameter naming a
  directive, and `.MACRO`, `.ENDM`, and `.INCLUDE`. As an argument can't have
  a directive, an expansion's directives are all the body's, so the assembler
  validates none of them again.
- **`IsRenamed()`** decides which labels are a body's own. A macro renames
  every local label in its body. A repeated block renames the labels its
  lines start with, anywhere in its body, so a `.REPT` nested in it renames
  them again as its own. In an expansion, a macro's labels are already macro
  labels (`M#1@X`), which a `.REPT` renames by the local label after their
  `@`, so a `.REPT` in a macro never mistakes an argument's local label for
  its own.
- **`MacroExpansion`** is an invocation's text: each body line from its first
  token to its last (so without its comment), with each parameter replaced by
  its argument's text and each label of the body's own renamed. A run for each
  part of a line says where it was copied from, which `GetLocation` reads, and
  the runs are built with the text, so they can't disagree. A `MacroArg` is an
  argument's text and where it was written, or no location for a repeated
  block's index, which is then placed where the body names it.
- **Invocations** are found in `ReadInstruction`: a name with no extension
  that is a macro's is expanded, before any form is matched. The lexer goes
  back to the invocation's name, and the grammar's `MacroInvocation` rule
  reads its arguments, each one or more tokens up to a comma outside
  brackets. `ReadMacroInvocation` validates that no argument has a directive
  (other than an instruction's extension, `ADD.W`), and that an argument ends
  in a name where the body writes an extension after its parameter (`op.W`).
- **`ReadExpansion`** reads an expansion, of a macro or a repetition, as
  content of its own: it adds the text to the lexer (`gb::Lexer::AddContent`)
  and reads it with `ReadContent`. Each `Expansion` stays alive as long as
  the assembler, as its tokens do, and records the macro it expands and the
  token that invoked it. Following those tokens out to a file is how the
  assembler finds that a macro invokes itself, maps a token to its outermost
  invocation's line (`GetMapLocation`), and turns an error's location into
  where its text was written (`GetSourceLocation`), followed by an error at
  each invocation.
- **Renamed labels** are `kTokenMacroLabel` tokens, whose names are already
  unique, so `GetLabelKey` leaves them as they are and they never change the
  scope. A local label in an argument is copied as the caller wrote it, so it
  is in the caller's scope. Only an expansion may write a macro label, which
  the grammar can't tell, as an expansion is read as a file is. So the
  assembler validates a file's lines (`ValidateNoMacroLabel`) and a body's
  lines (`ReadBodyLine`), which know where a line comes from. A body read
  inside an expansion may use and define the expansion's macro labels.
- **Limits:** every content's lines count against the lexer's million or so
  lines, and a line can't be longer than its 4095 columns, as it keeps a
  token's column in 12 bits. An expansion's line can be much longer than any
  line written, as an argument is copied wherever its parameter is, so an
  invocation the lexer can't add is an error at the invocation ("This
  invocation of X has too many lines, or a line too long").
- **Token text and adjacency** come from Game Bits' lexer
  (`gb::Lexer::GetTokenRangeText` and `IsTokenAdjacent`).

### Repeated blocks (`program_assembler.cc`)

- `ReadRepeat` reads the body with `ReadMacroBody`, as `ReadMacro` does, into
  a `ProgramMacro` of `Kind::kRepeat` the assembler keeps (`repeats_`), then
  reads each repetition with `ReadExpansion`, invoked at the `.REPT`'s `.`.
  Repetitions are numbered by `repetitions_`, across every repeated block, as
  their renamed labels are all named `REPT`.
- A `.REPT`'s body counts the `.REPT`s and `.ENDR`s nested in it, to find its
  own `.ENDR`. A macro's body ends at its first `.ENDM` whatever it holds, and
  its `.REPT`s are matched to their `.ENDR`s where it is invoked, so an
  unclosed `.REPT` there is an error at the invocation rather than swallowing
  the `.ENDM`.
- Errors name a macro or a block, and its body, through `GetErrorName()` and
  `GetBodyName()`, so each message is written once for both. The same error
  in every repetition is reported once, but one that names a repetition's own
  label is reported for each.
- A macro can't be named `REPT`.

### Constants from the host (`program_assembler.h`, `oz3asm`)

- `AssembleProgramFile` and `AssembleProgram` have overloads that take
  `AsmConstant`s, each a name and a dword value, and the others take none.
- The assembler keeps the host's constants apart from the program's
  (`host_constants_`), by name with their values, which `GetSymbolValue`,
  `.IFDEF`, and the check that a name isn't defined again all read. So they
  are never in the map, and have no token or expression. The assembler
  validates the names by lexing each with the program's lexer, so a name is
  read just as the source reads one, and a reserved word (a register) is its
  keyword.
- `ParseAsmConstant` reads `NAME` or `NAME=value`, lexing the value as the
  source does, for `oz3asm -D` and any other host. `-D` is repeated, with Game
  Bits' `CliFlag::repeated` and `CliCommandLine::GetAll()`.

### Brittleness

- The assembler must turn an expansion's tokens into locations through its
  expansion, rather than through the lexer's location. It does so only in
  `AddError`, `GetMapLocation`, and a bare program's error, each through
  `GetSourceLocation` or `FindExpansion`.
- `ProgramMacro::ReadLine` reads a macro label as any other token, so a caller
  other than the assembler would have to validate that only an expansion
  writes one.
- A body's lines are validated where it is defined, which is why the
  assembler can trust an expansion's directives. A new directive a body can't
  have must be rejected in `ProgramMacro::ValidateDirective`.

### Testing

- `expression_test.cc`, `program_lexer_test.cc`, and `source_pattern_test.cc`:
  each comparison true and false, as words and dwords; unsigned comparison of
  negative values; precedence; offsets compared in one segment, and an error
  in two; each symbol of two characters read as one, in a program and a
  pattern; symbols that join where a macro code meets its pattern.
- `program_grammar_test.cc`: each new directive, and `MacroInvocation`'s
  arguments with brackets, strings, commas inside them, and empty ones.
- `program_macro_test.cc`: parameters replaced everywhere a line can have
  them, and not in strings or local labels; lines with no tokens dropped;
  every label and directive a body can and can't have; renamed labels, for a
  macro and a repeated block, nested and in a macro's expansion; each part's
  location, for the body, arguments, and renamed labels; an expansion inside
  an expansion.
- `program_assembler_test.cc`: every `.IF` block taken and skipped, nested,
  with invalid source skipped, and each unmatched directive; macros with and
  without parameters, invoking each other and themselves, with `.IF` in a
  body, their labels, and every definition and invocation error; errors two
  invocations deep, at their origins with their invocations; repeated blocks
  with an index, nested, with `.IF`, in a macro, with their own labels and the
  caller's, and their errors; lines too long; host constants in expressions,
  `.IF`, and `.IFDEF`, and their errors; and the map's statements, labels,
  and constants at an invocation's line.
- `program_listing_test.cc`: skipped lines, and the rows of an invocation and
  a `.REPT`.
- By hand, left for the user: `oz3asm -D` on a program with `.IF`s, as
  `oz3asm` has no tests of its own.

## Building blocks

- `ProgramMacro` and `MacroExpansion`, for any text substituted into source
  that must still report errors where the text was written.
- `kTokenMacroLabel`, a label already unique, so it never changes a scope.
- `ReadExpansion`, for any other text the assembler generates and reads as
  content of its own.
- `AsmConstant` and `ParseAsmConstant`, for any host that builds a program
  for its own configuration.
- Comparisons in every expression, not only `.IF`.
