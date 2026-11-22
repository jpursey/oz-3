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
  operators.
- A program's lexer reads each of `<=`, `>=`, `==`, and `!=` as one symbol, as
  it does `<<` and `>>`, so a pattern reads them that way too, and
  `"$r <= $r"` matches `R1 <= R2`. Where a macro code meets its instruction's
  `$m`, a symbol can't be right before one it joins with (`"[!$m]"` with a code
  `"=$v"`). No pattern in the default instruction set has any of them.
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
  `.DS`, so its symbols are defined above it. It is evaluated as a word, as
  the other directives' values are, so a constant that doesn't fit one is an
  error there, and is tested through its halves instead: with `.CONST BIG =
  70000`, `.IF HIGH(BIG) != 0`.
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
  only to find the `.ENDM`, and to report a token the lexer can't read or a
  label the body can't define (see Labels in a body).
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
- **A label from a parameter** (`name:`) is the caller's, named by the
  argument, and is defined as if it were written where the macro is used: it
  is global, starts a scope for the caller's local labels after it (in the
  rest of the use's arguments too), and a second use defining it again is an
  error.
- **A label the body writes itself** (`loop:`) is an error where the macro is
  defined, as every use would define it again. So is a constant the body
  defines with a name of its own (`.CONST LIMIT = 5`), rather than a
  parameter's (`.CONST name = value`).

**Where its words are:** a macro is used like an instruction, so everything a
use defines and writes is at the line that uses it, in the map and the
listing. The listing shows the line, then a row for each further statement
with its location and words, as it does for a file included more than once.
A debugger shows the line for every word the macro wrote.

**Errors** inside a use are at the token at fault where it was written: in the
body, or in the argument at the use. Each is followed by where the macro was
used, from the innermost use out, so an error two macros deep lists the place
in the inner macro's body, the line in the outer macro's body that used it, and
the line in the file that used the outer macro:

```
macros.oza(5:8): PORTT is not defined
macros.oza(14:4): In WAITIN
main.oza(12:5): In WAITALL
```

### Constants from the host (manual's Program Source, Assembling a program and oz3asm)

```
oz3asm -D SCREEN -D CORES=2 main.oza
```

- `AssembleProgramFile` and `AssembleProgram` take constants from the host,
  each a name and a dword value, which the program sees as constants defined
  above its first line, so `.IF` and `.IFDEF` can test them.
- `oz3asm -D` (`--define`) takes `NAME` or `NAME=value`, with the value a
  number as the source writes one (`15`, `0Fh`, or `1111b`), or 1 if left out.
  It is given once for each constant, as Game Bits' command line now repeats
  flags (`CliFlag::repeated`, read with `CliCommandLine::GetAll()`).
- A name the program defines again is an error at the program's definition. A
  name that isn't a name, is a register, or is given twice, is an error before
  assembling.
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
- A pattern's lexer gains the symbols of two characters, as the program's
  does, and they are binary operators a value can't be followed by.
  `SourcePattern::CheckValueEnds` becomes `ValidateParts`, which also rejects a
  symbol right before one it joins with, where a macro code meets an
  instruction's pattern.

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

A macro is token substitution: its body is token ranges in the content it was
defined in, and a use replaces each parameter token with its argument's tokens'
text. The assembler reads the result as it reads an included file.

```
// oz3/tools/program_macro.h

// A macro defined with .MACRO: its name, parameters, and body, which are
// tokens of the content it is defined in.
class ProgramMacro final {
 public:
  // A token each use replaces: a parameter, or a local label.
  struct Replace {
    static constexpr int kLocalLabel = -1;
    gb::Token token;
    int parameter = kLocalLabel;
  };

  // A line of the body that has tokens.
  struct Line {
    gb::Token first;
    gb::Token last;
    std::vector<Replace> replaces;
  };

  ProgramMacro(gb::Token name, std::vector<gb::Token> parameters);

  const gb::Token& GetName() const;
  absl::Span<const gb::Token> GetParameters() const;
  absl::Span<const Line> GetLines() const;

  // Reads the lexer's next line as the body's next line, and moves past it.
  // A line with no tokens adds no line. Returns false, setting `error`, if a
  // token can't be read, or the line defines a label the body can't.
  bool ReadLine(gb::Lexer& lexer, gb::LexerContentId content,
                gb::ParseError* error);
};

// A macro argument: its first and last tokens, on the use's line.
struct MacroArg {
  gb::Token first;
  gb::Token last;
};

// The text of one use of a macro, and where each part of it came from.
class MacroExpansion final {
 public:
  // Expands use number `use` of `macro` with `args`, one for each parameter.
  MacroExpansion(const gb::Lexer& lexer, const ProgramMacro& macro, int use,
                 absl::Span<const MacroArg> args);

  const std::string& GetText() const;  // A line for each of the body's.
  int GetLineCount() const;

  // Returns where the text at `line` and `column` came from: in the body, or
  // in an argument's content.
  gb::LexerLocation GetLocation(int line, int column) const;
};
```

- **The body is lexed once,** when `.MACRO` is read, a line at a time with the
  assembler's lexer, to find `.ENDM`. Each line with tokens keeps its first
  and last token, and each token a use replaces: an identifier that is a
  parameter's name, and every local label.
- **An expansion is text:** each body line's text from its first token to its
  last (so without its comment), with each parameter replaced by its
  argument's text, and each local label renamed for the use (`@WAIT` in the
  second use of `WAITIN` is `WAITIN#2@WAIT`). A run for each part of a line
  says where it was copied from, which `GetLocation` reads.
- **A use's arguments** are token ranges in the use's content, so in an
  expansion's content for a use inside a macro. An expansion's locations are
  in the body, or in that content, so a location in an expansion's content is
  followed through it to the expansion that wrote it, and so on to a file.
- **The assembler reads an expansion as content of its own:** it adds the text
  to its lexer (`gb::Lexer::AddContent`) and reads it with `ReadContent`, with
  its own `.IF` stack, as it does an included file. Each expansion stays alive
  as long as the assembler, as its tokens do. The assembler keeps a stack of
  the macros being expanded, each with its use's location, which stops a macro
  using itself, and which an error lists from the innermost use out.
- **Local labels** need no scope of their own: a renamed label is a
  `kTokenMacroLabel` token, whose name is already unique, so `GetLabelKey`
  leaves it as it is, and it never changes the scope. A local label in an
  argument is copied as the caller wrote it, so it is in the scope the
  expansion is read in, which is the caller's. A macro label written in a file
  or a body is an error: `ReadLine` finds one in a body, and the assembler
  one in a file's line, including a use's arguments, which the grammar can't
  reject, as a renamed label is a valid argument in an expansion.
- **Errors** are added through `AddError`, which already sees every one. One
  whose location is in an expansion's content is moved to where that text
  came from, following the chain to a file, and is followed by an error at
  each use on the stack.
- **The map** gets every location from `GetMapLocation`, which maps a token in
  an expansion to its outermost use's line. Nothing in the map, its file, or the
  listing changes: a line already can have several statements.
- **Uses** are found in `ReadInstruction`: a name with no extension that is a
  macro's is expanded, before any form is matched. The line is parsed again
  with the grammar's `MacroInvocation` rule, whose arguments are each one or
  more tokens up to a comma outside brackets, read as `ArgToken` reads
  brackets, with an empty one an error at the token after its comma. Each
  gives its first token and its last, which is a `MacroArg`. A use with no
  arguments is its own alternative, so that a line such as `TWO , 2` is an
  empty argument, rather than a use with none followed by stray tokens:

  ```
  MacroInvocation {
    $name=%ident (EndOfLine | $args=MacroArg,+ EndOfLine);
  }
  MacroArg {
    $tokens=MacroArgToken+:"Macro argument is empty";
  }
  MacroArgToken {
    $last=!(%linebreak | ',' | '(' | ')' | '[' | ']' | '{' | '}');
    '(' ArgToken* $last=')';
    '[' ArgToken* $last=']';
    '{' ArgToken* $last='}';
  }
  ```

**Brittleness:** the assembler must turn an expansion's tokens into locations
through its expansion, rather than through the lexer's location. It does so
only in `AddError`, `GetMapLocation`, and a bare program's error, each through
one helper, and
`MacroExpansion` builds its runs with its text, so they can't disagree.

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

- [x] A body line's tokens can be lexed without disturbing the parser: the
  lexer is at the line after `.MACRO` once it is parsed, and reading the body
  a token at a time with `NextToken` then `NextLine` leaves it at the line
  after `.ENDM` (CL2 for skipped lines, CL4 for bodies). Confirmed for
  skipped lines in CL2: a skipped line is lexed a token at a time, then the
  lexer goes on with `NextLine`, or puts its first token back with
  `SetNextToken` for the parser to read a directive that ends the block.
  Confirmed for bodies in CL4, which read each line the same way, peeking its
  first token to put back for `ProgramMacro::ReadLine`.
- [x] The assembler can put the lexer back at the start of a use's line
  (`gb::Lexer::RewindLine`), to parse it again with `MacroInvocation` (CL4),
  once `Line` has parsed it as an instruction. No new lexer function is needed:
  `SetNextToken` goes back to the use's name, and on to the token after the
  line once `MacroInvocation` has parsed it.
- [x] Whether an error's uses are errors of their own after it, or part of its
  message (CL4): errors of their own ("In WAITIN"), as each is a location, so
  a tool can jump to each one. An error and its uses are one entry when
  dropping duplicates, so the same error in two uses is reported for each.
- [x] `gb::Lexer::AddContent` has room for many expansions: every content's
  lines count against about a million lines in all, and a use adds its body's
  lines (CL4). That is far more than a program is likely to need, and a use
  that doesn't fit is an error at the use, as a line too long is.
- [x] A line longer than the lexer's 4095 columns is an error (CL4): the
  lexer keeps a token's column in 12 bits, and an expansion's line can be
  much longer than any line written, as each argument is copied wherever its
  parameter is. `AddContent` fails for such a line, which is an error at the
  use: "This invocation of X has too many lines, or a line too long".
- [x] Adding `<=`, `>=`, `==`, and `!=` as symbols doesn't change how any
  pattern of the default instruction set, or any test's, is read (CL1): none
  writes two of `<`, `>`, `=`, and `!` in a row. Another instruction set's
  could, so a pattern now reads them as a program does (see Comparisons).

## Open questions

- [x] Whether a use's statements are at the line that uses it, or at the
  body's lines, as an included file's are: at the use, so the listing shows
  what each use became, and the debugger shows the line the programmer wrote.
- [x] Whether `.IF` is evaluated as a dword, or as a word, as the other
  directives' values are: a word, with a dword constant tested through
  `HIGH()` and `LOW()`, rather than a dword `.IFD` beside it, which would
  read close to `.IFDEF` and need an `.ELSEIFD` too.
- [x] Whether constants from the host are part of this feature, or a backlog
  item of their own: part of it, as CL5.
- [x] Left out, for the backlog when the feature is done: repeating a block
  (`.REPT`).

## CLs

### CL1 [x] tools: comparisons in expressions

Depends on: nothing.

- `expression.cc`: the comparison rules and their evaluation.
- `program_lexer.cc`: the new symbols.
- `source_pattern.cc`: the symbols of two characters, and `ValidateParts`.
- Manual: Program Source's Expressions, with the new operators, and the
  arithmetic's rules for them, and Instruction Set Source's rule for symbols
  that join.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: each operator true and false, as words and dwords; unsigned
  comparison of negative values; precedence against shifts, `&`, and each
  other; offsets in one segment compared, and in two segments an error; a
  comparison in an instruction's value and in `.DW`; each symbol of two
  characters read as one in a pattern, and matched; symbols that join where a
  macro code meets its instruction's pattern, with and without a space.
- Manual pages updated.

### CL2 [x] tools: conditional assembly

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
  below, an error; a constant that doesn't fit a word, an error, and its
  `HIGH()` tested; each unmatched directive; an `.IF` left
  open at the end of an included file; a label on each directive; skipped
  lines in the listing.
- Manual pages updated.

### CL3 [x] tools: macro text

Depends on: nothing.

- `program_lexer.h`: `kTokenMacroLabel`, a local label renamed for a use.
- `program_macro.h`: `ProgramMacro`, reading a body a line at a time with
  `ReadLine`; and `MacroExpansion`, a use's text and where each part of it
  came from.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests: a macro label lexed; parameters replaced in each place a line
  can have them, and not in strings or local labels; lines with no tokens
  dropped; a label from a parameter, a label the body writes (an error), and a
  macro label in a body (an error); local labels renamed; each part's
  location, for the body, arguments, and renamed labels, at the start, middle,
  and end of a line; a use inside an expansion, whose locations are in the
  outer expansion's content; a body line the lexer can't read; an argument
  whose last token is before its first.

### CL4 [x] tools: macros

Depends on: CL3, and CL2 for tests with `.IF` in a body.

- `program_grammar.cc`: `.MACRO` and `.ENDM`, and `MacroInvocation`, a
  use's arguments.
- `program_grammar.cc`: macro labels (`%macro_label`) as labels and in
  expressions.
- `program_assembler.cc`: reading definitions with `ProgramMacro::ReadLine`;
  expanding uses in `ReadInstruction`, parsed with `MacroInvocation`, read as
  content of their own with a stack of the uses being expanded; a macro label
  written in a file an error, and `GetLabelKey` leaving one as it is; a
  `.CONST` in a body with a name that isn't a parameter an error at the
  definition; a directive in an argument an error at the invocation, other
  than an instruction's extension (`ADD.W`); errors' locations followed
  through expansions to a file, then
  an error at each use on the stack; and map locations at the outermost use.
- `program_macro.cc`: a token range's text from `gb::Lexer`, if Game Bits
  *Lexer text of a token range* is done, in place of `GetTokensText`. It
  isn't yet, so `GetTokensText` stays.
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
  one left open in a body; arguments with brackets, strings, and commas
  inside them; the wrong number of arguments, and an empty one; a parameter
  named twice, an error at the second; a name that is an instruction's,
  defined twice, or used above its definition;
  a missing `.ENDM`, an `.ENDM` with no `.MACRO`, and a `.MACRO` in a body; an
  `.INCLUDE` in a body; errors in a body and in an argument, at their origins
  with their uses, two levels deep; the map's statements, labels, and
  constants at the use's line, and the listing of a use.
- Manual pages updated.

### CL5 [ ] tools, apps: constants from the host

Depends on: CL2, for tests with `.IFDEF`, and Game Bits *Repeated command line
flags*, which is done.

- `program_assembler.h`: `AsmConstant`, and the overloads that take them.
- `oz3asm`: `-D`/`--define`, a repeated flag.
- Manual: Program Source's Assembling a program, and oz3asm.

**Verify**
- Standard checks.
- Unit tests: a host constant in an expression, `.IF`, and `.IFDEF`; one
  defined again by the program, an error there; an invalid name, a register,
  and a name given twice, errors; not in the map.
- By hand, left for the user: `oz3asm -D` on a program with `.IF`s, as `oz3asm`
  has no tests of its own.
