# Program Assembler

The assembler library: it turns OZ-3 assembly source (`.oza`) into a `Program`
that `ProgramLoader` can load, for any instruction set, using the syntax that
instruction set defines. [assembler_design.md](../assembler_design.md) is the
design of the assembler as a whole, and this built the library: source files
and includes, statements, matching each instruction to one form, symbols,
expressions sized by where they are used, data, segments and their roles and
attributes, banks, the stack, instances, and the two passes, with every error
in a pass reported. The map, the command line tool, and the default instruction
set's own tests are later backlog items (*Program maps*, *oz3asm*, and
*Assemble the default instruction set*).

The wiki's Program Source page is the public reference for program syntax and
what it assembles to, and Instruction Set Source links to it. This summary
records what the design doc didn't settle, and how the library is built.

## Behavior

Everything in assembler_design.md holds, except where this says otherwise.

### API

```
// The most errors a pass of the assembler reports. It stops after this many,
// with a last error saying so.
inline constexpr int kMaxProgramErrors = 100;

// An assembled program.
class AsmProgram final {
 public:
  // Returns the program, which is valid (see ValidateProgram).
  const Program& GetProgram() const;
};

// Assembles the program whose main source file is at `path`. Returns null if
// the source has errors. If `errors` isn't null, it is set to the errors,
// which are none if the program assembles.
std::unique_ptr<AsmProgram> AssembleProgramFile(
    const InstructionSetSyntax& syntax, gb::FileSystem& file_system,
    std::string_view path, std::vector<gb::ParseError>* errors = nullptr);

// Assembles a program from source text, except that an .INCLUDE is an error.
// Errors are in the file "program".
std::unique_ptr<AsmProgram> AssembleProgram(
    const InstructionSetSyntax& syntax, std::string text,
    std::vector<gb::ParseError>* errors = nullptr);
```

- It takes an `InstructionSetSyntax`, rather than the `InstructionSetDef` the
  design doc sketched. The syntax is built once per instruction set (and
  creating it is what checks the def), so a host that assembles several
  programs creates it once, and handles its error itself.
- `AsmProgram` only holds the `Program` until *Program maps* adds `GetMap()`.

### Expressions

- **Widths:** an expression is evaluated in the width where it is used: a word
  in a `$v`, a code word argument, `.DW`, and the directives' values, and a
  dword in a `$V` and `.DD`.
- **Machine arithmetic** (replacing the design doc's "every step must fit,
  never a silent wrap"): a value is the width's bits, and each operator works
  on them as the CPU would. `+`, `-`, `*`, and unary `-` wrap, `/` and `%` are
  unsigned, `<<` and `>>` are logical and give 0 for a count of the width or
  more, and `!` is 1 for 0 and 0 otherwise. The only errors are a number,
  character, or symbol that doesn't fit the width, and dividing by zero.
- **A constant is its expression**, evaluated where it is used, in that width,
  so `.CONST MASK = ~1` is `0FFFEh` as a word, and `0FFFFFFFEh` as a dword. Its
  own value (for the map) is its dword value, which it must have even if it is
  unused. `$` and local labels in a constant are those where it is defined.
- **A constant that has a value, but can't be evaluated as a word, is an error
  where it is used as one** ("BIG can't be evaluated as a word: ..."), as the
  constant itself is fine. An error in a constant's own expression is reported
  once, where it is.
- A dword's low or high word can't be written in a word (*Low and high words*
  in the backlog).
- **First pass values** (code word arguments, literal numbers in patterns, and
  the directives' values) can only use symbols defined above them. Using one
  that isn't is Expression's own error ("X is not defined"), whether or not X
  is defined later, which has a classic feel.

### Instructions

- **A value is the longest expression at that point**, which replaces the
  design doc's "every token up to the pattern's next literal". The two agree
  because of a pattern rule this added: a value (a placeholder, or a literal
  number) can't be followed by a binary operator, which would continue the
  expression. The default instruction set follows it.
- A value wrapped in brackets as a whole doesn't match (`JP (5)`).
- **A form matches only if its first pass values have values**, and a literal
  number's is the number. So `RST SELF, 1` matches only the `SELF` form,
  unless `SELF` is a symbol, when it is ambiguous.
- No form matching lists the instruction's forms, and several matching lists
  those it matched. If no form matches, but one had a first pass value with an
  error, the statement's error is that value's.
- A code word value too big for its bits is an error at its expression.

### Segments, banks, and instances

- **The default program** (no `.SEGMENT`) is one shared RAM segment with every
  role, placed anywhere, sized to its contents and stack, with one instance.
  Once a program declares segments, nothing can be in a segment before the
  first `.SEGMENT`: the first label or statement there is an error. Constants
  and `.STACK` can be there, and a constant there sees `$` as 0.
- **Segments** are declared by their first `.SEGMENT`, with their attributes,
  and a later `.SEGMENT` only switches to them. A label on a `.SEGMENT` line is
  in the segment before. Each attribute is given at most once. Segment and
  bank names are their own, apart from symbols.
- **Roles:** each role is at most one segment's, and a segment can have
  several or none. A stack segment needs `.STACK`, and `.STACK` needs a stack
  segment.
- **Sizes:** a segment is its reserved words (with the stack after the stack
  segment's contents), times the instance count if it is per instance, in
  whole pages. `PAGES` must be at least that, and its extra space is at the
  end. A segment with `PAGE` can't pass the end of its bank. A segment that
  passes 64 Kiw is reported once, where it passes it.
- **Banks:** a mask is a first pass expression, and can't be 0. Each segment's
  bank is looked up when the first pass ends, so a `.BANK` can follow the
  segments that name it. With any `.BANK`, a segment without `BANK` is an
  error.
- **Instances:** a per instance segment can only reserve words, each
  statement that writes words there being an error, and can't have `CODE`.
  With more than one instance, the stack segment must be per instance. An
  instance's offset is 0 in a shared segment, the start of its own words in a
  per instance segment, and for the stack, from the segment's end to the end
  of its own words.
- `ROM` is allowed with any role, the stack's included, as the loader allows
  it.
- Banks, segments, and instances are in the program in the order declared.
  What the assembler doesn't check itself (more than 16 banks, or more pages
  than a bank holds between a bank's segments) is left to `ValidateProgram`,
  whose error is at the start of the main file.

### Errors

Each error is a `gb::ParseError` with the file, line, and column. The
assembler reports every error in a pass, carrying on at the next line after a
line that doesn't parse, and stops at `kMaxProgramErrors`. An error is
reported once, by its formatted message. A pass with errors is the last.

## Design

### Names

| Name                 | Candidates                                   | Avoids                                                  | Choice                                                                |
| -------------------- | -------------------------------------------- | ------------------------------------------------------- | --------------------------------------------------------------------- |
| Wiki page            | Program Source, Assembly Language, Assembler | Instruction Set Source (`.izm`), which it pairs with    | Program Source (`2.5-Program-Source.md`)                              |
| Per instance segment | `INSTANCE`, `EACH`, `COPIES`                 | `.INSTANCES` (how many), which it reads with            | `INSTANCE`                                                            |
| Segment size         | `PAGES(n)`, `SIZE(n)`                        | `.DS` sizes, which are words; `PAGE(n)`, the start page | `PAGES(n)`                                                            |
| Lexer file           | `program_lexer.h`, `program_tokens.h`        | `source_pattern.h` (patterns' own lexer)                | `program_lexer.h`                                                     |
| Expression           | `Expression`, `ProgramExpression`            | microcode's immediates, `$v` placeholders               | `Expression`, as the debugger's watches can use it too                |
| Grammar              | `ProgramGrammar`, `ProgramParser`            | `gb::Parser` and `gb::ParserProgram`, which it builds   | `ProgramGrammar`                                                      |
| Assembler            | `program_assembler.h`, `asm_program.h`       | `instruction_assembler.h` (`.izm`)                      | `program_assembler.h`, holding `AsmProgram` and `AssembleProgramFile` |

### Game Bits

The Game Bits lexer gained suffixes in either case and hex numbers that start
with a digit (Game Bits *Number suffixes in either case, and hex numbers that
start with a digit*), so every program number is an integer token. Three more
items later simplified the grammar (*Program grammar on newer Game Bits parser
features*): Game Bits *Negated rule items and position markers*, *Adjacent
rule items*, and *Case of user tokens*.

### Program lexer (`program_lexer.h`)

- `GetProgramLexerConfig()`: decimal, hex (`h`), and binary (`b`) integers of
  64 bits without signs; identifiers forced to upper case
  (`kIdentForceUpper`); the registers as case-insensitive keywords; single and
  double quoted strings with doubled quotes; `;` comments; line break tokens;
  and `@name` as one local label token (`kTokenLocalLabel`), whose value is
  the whole token in upper case, `@` and all (`LexerCase::kUpper`).
- `GetStringWords()` turns a string token into its words: one character per
  word for `'...'`, or two for `"..."`.
- `FormatHexNumber()` writes a number as a program would (`0FFFFh`), which
  `InstructionForm::Disassemble` uses too, so the two can't disagree.

### `Expression` (`expression.h`)

- `Expression::GetParserRules()` is the expression grammar for a parser
  program: each precedence level a rule with a repeated group, as the parser
  doesn't allow left recursion. `Create()` turns a parse into a vector of
  nodes, each with its token for errors, so an expression outlives its parse,
  and folds each level left to right.
- `Evaluate(width, context)` works on `uint64_t`s holding the width's bits,
  masking each step to the width. It returns a value, a symbol not defined
  (yet), or an error with its location.
- `ExpressionContext` gives `$` (`GetOffset`) and each symbol's value where it
  is used (`GetSymbolValue`). A symbol's error without a location is reported
  at its use. `IsBracketed()` and `IsString()` tell matching and `.DW` what
  the expression is as a whole.
- Named values aren't `Expression`'s: `ValueSetContext` (in
  `program_grammar.h`) looks a name up in a value set first, then in the
  context it wraps. That keeps `Expression` free of instruction sets, for the
  debugger's watches.

### Values followed by operators

`SourcePattern::Parse` rejects a value followed by a binary operator in each
pattern, and `InstructionSetSyntax::Create` checks each form again, where a
macro code's value meets the instruction's next literal, through
`SourcePattern::CheckValueEnds`.

### `ProgramGrammar` (`program_grammar.h`)

A parser program built from an `InstructionSetSyntax`:
- **`Line`** (`kLineRule`), the one rule the assembler parses with: an
  optional label, then a directive or an instruction, or nothing, then the end
  of the line. A program is parsed a line at a time, as a parse stops at its
  first error, and a line that doesn't parse leaves the lexer at its start, so
  the assembler goes on at the next line.
- **`Directive`** is `.`, no space (`^%here`, with its own error), and an
  alternative for each directive with its operands, including every segment
  attribute, so a directive with a bad operand gets that operand's error.
- **`Instruction`** is the name, an optional `.` and extension with no space
  around the `.` (`[^'.' ^%ident]`), and its arguments as any tokens to the end
  of the line, with brackets balanced (`ArgToken*`, built on a negated item),
  starting at a position marker (`$args=%here`). With a space around it, the
  `.` starts the arguments, so `ADD .W` is `ADD` with no form that matches.
- **A rule for each distinct form pattern**, from its parts in order: literal
  text, register rules by size, and an `Expression` for each value.

`MatchInstruction()` does what the parser can't: it reports every form that
matches. It parses the instruction's arguments with each distinct rule of its
name's forms (`gb::Lexer::SetNextToken`), and a parse is a match once no value
is bracketed as a whole, and its first pass values have values, which it keeps
(`InstructionArg::code_value`).

**Performance**, measured for the default instruction set (1335 forms):

| Build          | Syntax  | Grammar | A line (`Line`, then 17.4 forms per name) |
| -------------- | ------- | ------- | ----------------------------------------- |
| RelWithDebInfo | 7.5 ms  | 4.9 ms  | 34 µs (`Line` is 13 µs)                   |
| Debug          | 98 ms   | 75 ms   | 570 µs (`Line` is 170 µs)                 |

These were measured with a generated `ArgToken` of every token type, symbol,
and keyword. Trying a negated item first parses a line with long arguments
about 40% faster (26 µs rather than 43 µs in RelWithDebInfo).

Building the grammar costs less than creating the syntax, so it is built for
each assembly. A release build reads about 30,000 instruction lines a second.
If parsing ever becomes a bottleneck, the fix is to make the Game Bits parser
faster, not to work around it here.

### `ProgramAssembler` (`program_assembler.cc`)

The two passes:
1. **Reading** each file's lines with one `gb::Parser`, following includes
   (each file's content is added to the lexer once, and rewound to include it
   again), defining labels and constants, matching instructions, evaluating
   first pass values, and recording each statement for the second pass, with
   its segment, offset, and scope. Statements are a variant of an instruction
   (its form and arguments), data (each item a string's words or an
   expression), or a constant. When the pass ends, the stack is added to the
   stack segment, each segment's bank is looked up, and each segment's page
   count is set.
2. **Finishing** each recorded statement in source order: encoding
   instructions and data into its segment's words, marking which are written,
   and checking each constant's dword value. Then building the `Program`:
   banks, segments with their data blocks (each run of written words, widened
   to aligned pairs, with an unwritten word in a pair as 0), roles, and
   instances, checked last with `ValidateProgram`.

- **Symbols:** labels and constants are separate tables, and a name is one or
  the other. A local label's key is its scope's label and then its name, which
  starts with `@`, which no label's name can have. A scope is a view of its
  label's token, which lives as long as the lexer.
- **Constants** cache their results by width, except a symbol not yet
  defined, and a constant reached again while it is being evaluated depends on
  itself.
- **Segments** each have a reserved size, which is the offset of their next
  statement, and once the first pass ends their size (one instance's, if per
  instance). Their word buffers only go up to their written size, the end of
  their last statement that writes words, so reserved space costs nothing.
  The default program's segment is an unnamed segment with every role, which
  the first `.SEGMENT` replaces.
- **Roles** are indexed by `CpuCore`'s bank indices (`CpuCore::CODE` and the
  others).
- `EvaluateInRange()` evaluates a first pass value and checks its range, for
  `PAGE`, `PAGES`, and `.INSTANCES`.

The tests use a small instruction set written in the test, and include
programs with several segments, banks, and instances that load with
`ProgramLoader`. The design doc's example program, with its segments and
`ROM`, was assembled for the default instruction set with a temporary test,
and each instruction disassembled to the form it was written as.

### Brittleness

- Expressions and recorded statements hold tokens of the assembler's lexer,
  which lives as long as the assembly.
- An instruction named like a register (`SP`) can't be written, as the name
  is a keyword. `InstructionSetSyntax` doesn't reject one, and no instruction
  set has one; it can if one ever does.
- Two literal `<` (or `>`) parts in a row in a pattern lex as `<<` in a
  program, so they can't match. No pattern has them.
- A literal string in a pattern matches a program's string of the same text
  in either kind of quote, though the two make different words. No pattern
  has a string.
