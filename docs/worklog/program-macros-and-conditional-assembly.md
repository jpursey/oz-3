# Program macros and conditional assembly

Program source gains **macros**, for the boilerplate the instruction set
doesn't cover, and **conditional assembly**, for building one source for
different hardware, as the assemblers of the Z80 and 8086 era had them
(`MACRO`, `IF`). A macro is a named block of lines with parameters, used where
an instruction would be, and each use assembles its lines with the arguments
in place of the parameters:

```
.CONST KEYBOARD = 2

; Reads a port until its word is nonzero.
.MACRO WAITIN port, reg
@wait:
    IN port, reg
    TST reg
    JC Z, @wait
.ENDM

; Jumps to target if reg is zero.
.MACRO JZ reg, target
    TST reg
    JC Z, target
.ENDM

start:
    WAITIN KEYBOARD, R1
    WAITIN KEYBOARD, R2     ; Its own @wait, apart from the first's
    JZ R2, @done            ; start's @done, not one of the macro's
    ...
@done:
    HALT
```

Conditional assembly assembles a block of lines only if an expression is
nonzero, so a configuration can choose what a program has:

```
.INCLUDE "config.oza"       ; .CONST SCREEN = 1, for an Ozzy with a screen

.IF SCREEN
    CALL screen_init
.ELSEIF TELETYPE != 0
    OUT TELETYPE, 0Ch
.ENDIF
```

`.IF` needs comparisons, which expressions don't have yet, so they gain C's
`==`, `!=`, `<`, `<=`, `>`, and `>=`. A host, and `oz3asm -D`, can also give a
program constants of its own, so one source builds for each configuration
without a file per configuration.

Everything here is in `tools` and the manual's Program Source page. No program
that assembles today changes.

## Behavior

### Comparisons (manual's Program Source, Expressions)

| Precedence | Operators                                                |
| ---------- | -------------------------------------------------------- |
| Highest    | Unary `-` (negate), `~` (bitwise not), `!` (logical not) |
|            | `*`, `/`, `%`                                            |
|            | `+`, `-`                                                 |
|            | `<<`, `>>`                                               |
|            | `<`, `<=`, `>`, `>=` (new)                               |
|            | `==`, `!=` (new)                                         |
|            | `&`                                                      |
|            | `^`                                                      |
| Lowest     | `\|`                                                     |

- As in C, a comparison is 1 if it holds, and 0 otherwise, and is at C's
  precedence, so `A == 1 & B == 2` is `(A == 1) & (B == 2)`. With the bitwise
  operators on 0 and 1, there is no need for `&&` and `||`.
- **Unsigned,** as `/` and `%` are, in the width the expression is used in, as
  the machine's `CMP` with `C` would compare: `-1 > 0` is 1, and `-1 == 0FFFFh`
  is 1 as a word, but 0 as a dword.
- Comparisons work in every expression, not only in `.IF`. A value in an
  instruction still ends at the pattern's next literal: patterns already can't
  follow a value with `<` or `>`, and `=` and `!` on their own aren't
  operators, so no pattern rule changes.
- Labels in different segments can't be compared, as they can't be combined
  with any other operator. A comparison's result is a number, never an offset.

### Conditional assembly (manual's Program Source, new Conditional assembly section)

```
.IF <expression>
    ...
.ELSEIF <expression>
    ...
.ELSE
    ...
.ENDIF
```

- A block is assembled if its expression is nonzero, or for `.ELSE`, if no
  block before it in the `.IF` was. At most one block of an `.IF` is
  assembled. `.ELSEIF` and `.ELSE` are optional, and there can be any number of
  `.ELSEIF`s, but only one `.ELSE`, after them.
- **`.IFDEF <name>`** and **`.IFNDEF <name>`** start an `.IF` whose condition is
  whether a label or constant of that name is defined above it. They pair with
  constants from the host (see below), as in `.IFDEF DEBUG`.
- **The expression must be known as the `.IF` is read** (see Passes), as for
  `.DS`, so its symbols are defined above it. It is evaluated as a dword, so any
  constant can be tested, including one that doesn't fit a word.
- **Lines that aren't assembled aren't read,** except to find the `.IF`
  directives that end their block, so they can hold anything, even text that
  isn't valid source. An `.ELSEIF` after a block that was assembled isn't
  evaluated. Inside them, `.IF`s nest as usual, so their `.ENDIF`s pair up.
- **An `.IF` ends where it starts:** in the same file, or the same use of a
  macro. An `.IF` with no `.ENDIF` is an error at the `.IF`, and an `.ELSEIF`,
  `.ELSE`, or `.ENDIF` with no `.IF` is an error at itself.
- **No labels:** `.IF`, `.ELSEIF`, `.ELSE`, and `.ENDIF` can't have a label, as a
  label on a line that isn't assembled would be a puzzle. A label goes on the
  next line.
- The listing shows lines that aren't assembled as it shows a comment: with no
  location or words.

### Macros (manual's Program Source, new Macros section)

```
.MACRO <name> [<parameter>, ...]
    ...
.ENDM
```

**Defining:**
- The lines between `.MACRO` and `.ENDM` are the macro's **body**, which isn't
  assembled where it is written, only where it is used. Its lines are read
  only to find the `.ENDM`, and to report a token the lexer can't read.
- **The name** is a name (a C identifier), defined once, and global to the
  program, across include files, as symbols are. It can't be the name of an
  instruction, with or without an extension, as a statement means one thing:
  in the default instruction set, a macro can be `PUSHALL` but not `PUSH` or
  `ADD`. It can share its name with a symbol, as where a name is decides what
  it is.
- **The parameters** are names, each different. They can share a name with a
  symbol, which the macro's body can't then use.
- A macro must be defined above where it is used, as each use is assembled
  where it is read.
- `.MACRO` can't have a label, and `.ENDM` must be on a line of its own. A
  `.MACRO` with no `.ENDM` in its file is an error at the `.MACRO`, as is a
  `.MACRO` in a body, as macros can't be defined inside macros.

**Using:**
- A macro is used by its name where an instruction would be, with its
  arguments after it, separated by commas: `WAITIN KEYBOARD, R1`. A name with
  an extension is always an instruction's, so `WAITIN.W` is an unknown
  instruction.
- **Arguments are text:** each is the source between the commas, outside of
  brackets, so `D(table), (R1)` is two arguments. An argument can be anything a
  line can hold, such as a register, an expression, a string, or an
  instruction's name, but can't be empty. A use gives exactly as many arguments
  as the macro has parameters.
- **Each parameter is replaced** by its argument's text everywhere its name is
  a word in the body: in instructions and their extensions (`ADD.size`),
  directives, expressions, and labels. A name inside a string or a local label
  is never a parameter (`'reg'` and `@reg`). So name parameters apart from the
  words they would replace by accident, such as a directive's name or a word an
  instruction's pattern writes.
- The body's lines are then assembled where the macro is used, as if they were
  written there, so a label on the line labels the macro's first statement. A
  macro can use other macros, but not itself, even through another macro, as
  that would never end. A body can't include a file.

**Labels in a body:**
- **A local label the body writes** is that use's own: each use of the macro
  has its own `@wait`, which only that use's lines see. They don't start a
  scope, and don't end the caller's: after the macro, the caller's local labels
  go on as before.
- **A local label in an argument** is the caller's, as the caller wrote it, so
  `JZ R2, @done` jumps to the caller's `@done`, even if the macro has a
  `@done` of its own.
- **A label the body writes,** or that an argument gives it, is defined as if
  it were written where the macro is used: it is global, starts a scope for the
  caller's local labels after it, and a second use defining it again is an
  error. So a body defines a label from a parameter (`name:`) rather than
  writing one.

**Where its words are:** a macro is used like an instruction, so everything a
use defines and writes is at the line that uses it, in the map and the
listing. The listing shows the line, then a row for each further statement
with its location and words, as it does for a file included more than once.
A debugger shows the line for every word the macro wrote.

**Errors** inside a use are at the token at fault where it was written: in the
body, or in the argument at the use. Each says where the macro was used, from
the innermost use out: `macros.oza(5:8): PORTT is not defined (in WAITIN at
main.oza(12:5))`.

### Constants from the host (manual's Program Source, Assembling a program and oz3asm)

```
oz3asm -D SCREEN=1,CORES=2 main.oza
```

- `AssembleProgramFile` and `AssembleProgram` take constants from the host,
  each a name and a dword value, which the program sees as constants defined
  above its first line, so `.IF` and `.IFDEF` can test them.
- `oz3asm -D` (`--define`) takes a comma separated list of `NAME` or
  `NAME=value`, each value a number as the source writes one (`15`, `0Fh`, or
  `1111b`), or 1 if left out. A flag can only be given once (Game Bits' command
  line), hence the list.
- A name the program defines again is an error at the program's definition. A
  name that isn't a name, or is a register, is an error before assembling.
- They aren't in the map, as no source line defines them.

## Design

All of it is in `tools`: the expression, the program grammar, and the program
assembler, with macros' text handled by a new `program_macro.h`.

### Names

| Name                           | Candidates                                           | Avoids                                                                  | Recommendation                                       |
| ------------------------------ | ---------------------------------------------------- | ----------------------------------------------------------------------- | ---------------------------------------------------- |
| Directives                     | `.MACRO`/`.ENDM`, `.MACRO`/`.ENDMACRO`               | Nothing: directives start with `.`                                      | `.MACRO` and `.ENDM`, MASM's                         |
| Conditionals                   | `.IF`/`.ELSEIF`/`.ELSE`/`.ENDIF`, `.IFDEF`/`.IFNDEF` | `.ELIF` (C's, not an assembler's)                                       | MASM's, as listed                                    |
| A macro, in C++                | `ProgramMacro`, `SourceMacro`                        | `MacroDef` and `$m`, the instruction set's macros (core)                | `ProgramMacro`                                       |
| One use's text                 | `MacroExpansion`, `MacroUse`                         | "Instance" (a program's instances)                                      | `MacroExpansion`                                     |
| A use's local label in the map | `WAITIN#2@WAIT`, `WAITIN.2@WAIT`                     | `START@LOOP`, a scope's local label; `.`, which reads like an extension | `WAITIN#2@WAIT`: the macro, which use, and the label |
| Host constants                 | `AsmConstant`, `ProgramDefine`                       | `Constant` (the map's), `ConstantStatement`                             | `AsmConstant`, after `AsmProgram`                    |
| Flag                           | `-D`/`--define`, `-c`/`--const`                      | `-d`, which some tools use for debug                                    | `-D`/`--define`, as C compilers have it              |

"Macro" already means the instruction set's macros, the `$m` argument and its
codes. The manual's pages keep the two apart by where they are: the Program
Source page's macros are always the program's, and it says once that they are
unrelated to an instruction's macro argument.

### Comparisons

- `Expression`'s rules gain `ExpressionCompare` (`<`, `<=`, `>`, `>=`) and
  `ExpressionEqual` (`==`, `!=`) between `ExpressionShift` and
  `ExpressionAnd`, folded left to right as the others are. The program lexer
  gains `<=`, `>=`, `==`, and `!=` as symbols.
- A comparison's result is in no segment. Two offsets in different segments are
  an error, as for any other operator.

### Conditional assembly

- `ProgramAssembler::ReadContent` keeps a stack of the `.IF`s open in the
  content it reads, so each file, and each use of a macro, has its own, and
  one left open at the end of the content is an error there.
- While a block is skipped, each line is only scanned, with the lexer, for a
  `.` and an `IF`, `IFDEF`, `IFNDEF`, `ELSEIF`, `ELSE`, or `ENDIF` right after
  it, past a label if there is one, which is then an error once the directive
  is read. Nothing else on the line is lexed, and the lexer's errors are
  ignored.
- The grammar's `Directive` gains the new directives as alternatives, so an
  assembled line reads them as it reads any other.

### Macros

```
// oz3/tools/program_macro.h

// Where a piece of text came from: a place in a source file, and the scope
// its local labels are in.
struct SourceOrigin {
  gb::LexerLocation location;  // In a file's content, never an expansion's.
  std::string_view scope;      // The key prefix of its local labels.
};

// A macro defined with .MACRO: its name, parameters, and body.
class ProgramMacro final {
 public:
  // A line of the body: its text, without its comment, and each place a
  // parameter is in it, by column and size.
  struct Line {
    std::string text;
    gb::LexerLocation location;
    struct Parameter { int column; int size; int index; };
    std::vector<Parameter> parameters;
  };

  ProgramMacro(gb::Token name, std::vector<std::string> parameters,
               std::vector<Line> lines);

  std::string_view GetName() const;
  absl::Span<const std::string> GetParameters() const;
  absl::Span<const Line> GetLines() const;
};

// The text of one use of a macro, with where each piece of it came from.
class MacroExpansion final {
 public:
  // An argument: its text, and where each piece of it came from, which is
  // the caller's line's origins over its columns.
  struct Arg { ... };

  // Expands `macro` with `args`, one for each parameter. `scope` is this
  // use's own scope ("WAITIN#2"), for the local labels the body writes.
  MacroExpansion(const ProgramMacro* macro, absl::Span<const Arg> args,
                 std::string scope, SourceOrigin use);

  // The text, a line for each of the body's.
  const std::string& GetText() const;

  // Returns where the text at `line` and `column` came from.
  SourceOrigin GetOrigin(int line, int column) const;

  // Returns where the macro was used, in a file: the outermost use's line.
  const SourceOrigin& GetUse() const;
};
```

- **The body is lexed once,** when `.MACRO` is read, a line at a time with the
  assembler's lexer, to find `.ENDM`, and each identifier that is a
  parameter's name. Each line keeps its text up to its last token, so its
  comment is dropped.
- **An expansion is text,** built by copying each body line, with each
  parameter replaced by its argument's text. Each piece of the text keeps its
  origin: a body piece is its place in the body, in the macro's scope; an
  argument piece is its place where the caller wrote it, and the caller's
  scope. An argument from a line that is itself in an expansion takes that
  line's origins, so every origin is in a file, however deeply macros use
  macros, and is found without following a chain.
- **The assembler reads an expansion as content of its own:** it adds the text
  to its lexer (`gb::Lexer::AddContent`) and reads it with `ReadContent`, as it
  does an included file. Each expansion stays alive as long as the assembler,
  as its tokens do. Lines don't change in number, so a token's line in the
  expansion is its body line.
- **Local labels** are keyed by scope, as now (`GetLabelKey`). A token in a
  file is in the scope its statement is in, as now, and a token in an
  expansion is in its origin's scope: the use's own for the body's, and the
  caller's for an argument's. So `ExpressionContext::GetSymbolValue` gains the
  symbol's token, which `StatementContext` looks up through the expansion
  that has its content.
- **Errors** are added through `AddError`, which already sees every one. One
  whose location is in an expansion's content is moved to its origin, and
  gains each use it is in, from the innermost out.
- **The map** gets every location from `GetMapLocation`, which maps a token in
  an expansion to its outermost use's line. Nothing in the map, its file, or the
  listing changes: a line already can have several statements.
- **Uses** are found in `ReadInstruction`: a name with no extension that is a
  macro's is expanded, before any form is matched. The arguments are split at
  each comma outside brackets, from the instruction's `$args`.

**Brittleness:** each expansion's origins must cover its whole text, and the
assembler must look every expansion token up through them, rather than
through the lexer's location. Both are kept in one place each:
`MacroExpansion` builds the origins with the text, so they can't disagree, and
the assembler turns tokens into locations only in `AddError`,
`GetMapLocation`, and `StatementContext`, each through one helper.

### Constants from the host

```
// A constant the host gives a program, as if it were defined above its first
// line.
struct AsmConstant {
  std::string name;
  uint32_t value = 0;
};

std::unique_ptr<AsmProgram> AssembleProgramFile(
    const InstructionSetSyntax& syntax, gb::FileSystem& file_system,
    std::string_view path, absl::Span<const AsmConstant> constants,
    std::vector<gb::ParseError>* errors = nullptr);
```

- The existing overloads stay, with no constants. `AssembleProgram` gains the
  same overload.
- A host constant is a `Constant` with a value and no expression, so
  `GetSymbolValue` and `.IFDEF` see it as any other.

### To confirm

- [ ] A body line's tokens can be lexed without disturbing the parser: the
  lexer is at the line after `.MACRO` once it is parsed, and reading the body
  a token at a time with `NextToken` then `NextLine` leaves it at the line
  after `.ENDM` (CL2 for skipped lines, CL4 for bodies).
- [ ] The instruction's `$args` items give each argument's first and last
  token, so its text and columns can be copied, or the arguments are better
  read with a rule of their own (CL4).
- [ ] `gb::Lexer::AddContent` has room for many expansions: every content's
  lines count against about a million lines in all, and a use adds its body's
  lines (CL4).
- [ ] Adding `<=`, `>=`, `==`, and `!=` as symbols doesn't change how any
  pattern of the default instruction set, or any test's, is read (CL1).

## Open questions

- [ ] Whether a use's statements are at the line that uses it (as above), or
  at the body's lines, as an included file's are. At the use, the listing
  shows what each use became, and the debugger shows the line the programmer
  wrote. At the body, a debugger could step through a macro's lines, but the
  listing would show every use's words under the definition, and nothing at
  the use. Showing both, as MASM's listings did, needs the expanded lines in
  the map, a larger change for later if it is wanted.
- [ ] Whether `.IF` is evaluated as a dword (as above), so any constant can be
  tested, or as a word, as the other directives' values are.
- [ ] Whether constants from the host (CL5) are part of this feature, or a
  backlog item of their own: without them, a configuration is a small main
  file that defines its constants and includes the shared source.
- [ ] Left out, for the backlog when the feature is done: repeating a block
  (`.REPT`), defaults for macro arguments, joining a parameter to other text
  (`table_&n`), leaving a macro early (`.EXITM`), and comparing a macro
  argument's text (`.IFIDN` and `.IFDIF`, as MASM's `IFIDN` and `IFDIF`), such
  as for a sequence of its own when an argument is `R7`.

## CLs

### CL1 [ ] tools: comparisons in expressions

Depends on: nothing.

- `expression.cc`: the comparison rules and their evaluation.
- `program_lexer.cc`: the new symbols.
- Manual: Program Source's Expressions, with the new operators, and the
  arithmetic's rules for them.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: each operator true and false, as words and dwords; unsigned
  comparison of negative values; precedence against shifts, `&`, and each
  other; offsets in one segment compared, and in two segments an error; a
  comparison in an instruction's value and in `.DW`.
- Manual pages updated.

### CL2 [ ] tools: conditional assembly

Depends on: CL1, for tests that compare.

- `program_grammar.cc`: `.IF`, `.IFDEF`, `.IFNDEF`, `.ELSEIF`, `.ELSE`, and
  `.ENDIF`.
- `program_assembler.cc`: the stack of open `.IF`s in `ReadContent`, skipping
  lines, and the errors.
- Manual: Program Source's Conditional assembly, and the directives table.

**Verify**
- Standard checks.
- Unit tests: each block of `.IF`, `.ELSEIF`, and `.ELSE` taken; nesting, in
  taken and skipped blocks; invalid source in a skipped block; an `.ELSEIF`
  with an unknown symbol after a taken block; `.IFDEF` and `.IFNDEF` of a
  label and a constant above and below; a condition with a symbol defined
  below, an error; a dword condition; each unmatched directive; an `.IF` left
  open at the end of an included file; a label on each directive; skipped
  lines in the listing.
- Manual pages updated.

### CL3 [ ] tools: macro text

Depends on: nothing.

- `program_macro.h`: `ProgramMacro` and `MacroExpansion`.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: parameters replaced in each place a line can have them, and not
  in strings or local labels; arguments with brackets, strings, and commas
  inside them; each piece's origin, for the body and for arguments, through
  two levels of expansion; columns at the start, middle, and end of a line,
  and of a replaced parameter; a line with no parameters.

### CL4 [ ] tools: macros

Depends on: CL3, and CL2 for tests with `.IF` in a body.

- `program_grammar.cc`: `.MACRO` and `.ENDM`.
- `expression.h`: `ExpressionContext::GetSymbolValue` takes the symbol's token,
  and `ValueSetContext` passes it on.
- `program_assembler.cc`: reading definitions, expanding uses in
  `ReadInstruction`, local labels' scopes, errors' locations and uses, and
  map locations.
- `program_map.h`: the names of a use's own local labels.
- Manual: Program Source's Macros, the directives table, and the listing's
  rows for a macro's use.

**Verify**
- Standard checks.
- Unit tests: a macro with and without parameters; each use's own local
  labels, in the map as `WAITIN#2@WAIT`; a caller's local label as an
  argument, where the macro has one of the same name; a label from a
  parameter; a label on a use; a macro using a macro, and using itself
  directly and through another, an error; `.IF` on a parameter in a body, and
  one left open in a body; the wrong number of arguments, and an empty one; a
  name that is an instruction's, defined twice, or used above its definition;
  a missing `.ENDM`, an `.ENDM` with no `.MACRO`, and a `.MACRO` in a body; an
  `.INCLUDE` in a body; errors in a body and in an argument, at their origins
  with their uses, two levels deep; the map's statements, labels, and
  constants at the use's line, and the listing of a use.
- Manual pages updated.

### CL5 [ ] tools, apps: constants from the host

Depends on: CL2, for tests with `.IFDEF`.

- `program_assembler.h`: `AsmConstant`, and the overloads that take them.
- `oz3asm`: `-D`/`--define`.
- Manual: Program Source's Assembling a program, and oz3asm.

**Verify**
- Standard checks.
- Unit tests: a host constant in an expression, `.IF`, and `.IFDEF`; one
  defined again by the program, an error there; an invalid name and a register,
  errors; not in the map.
- By hand, left for the user: `oz3asm -D` on a program with `.IF`s, as `oz3asm`
  has no tests of its own.
