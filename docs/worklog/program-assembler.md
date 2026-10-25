# Program Assembler

The assembler library: it turns OZ-3 assembly source (`.oza`) into a `Program`
that `ProgramLoader` can load, for any instruction set, using the syntax that
instruction set defines. [assembler_design.md](../assembler_design.md) is the
design of the assembler as a whole, and this is the part of it that builds the
library: source files and includes, statements, matching each instruction to
one form, symbols, expressions sized by where they are used, data, segments and
their roles, banks, the stack, instances, and the two passes, with every error
in a pass reported. The map, the command line tool, and the default
instruction set's own tests are the backlog items after it (*Program maps*,
*oz3asm*, and *Assemble the default instruction set*).

A host application assembles a program with `AssembleProgramFile` (or
`AssembleProgram`, for source text with no includes), giving it an
`InstructionSetSyntax`, and loads the `Program` it returns. It creates the
wiki's Program Source page, the public reference for the program syntax.

Program source is parsed with Game Bits' parser, from a grammar built for each
instruction set: the assembler's own statements and expressions, and a rule
for each way an instruction can be written.

## Behavior

Everything in assembler_design.md holds, with what this plan settles below.
Where this changes that doc, the plan's first commit changes the doc to match.

### API

```
// An assembled program.
class AsmProgram final {
 public:
  const Program& GetProgram() const;
};

// Assembles the program whose main source file is at `path`, for the
// instruction set `syntax` describes. Returns null, and appends every error
// to `errors` if it isn't null, if the source has errors.
std::unique_ptr<AsmProgram> AssembleProgramFile(
    const InstructionSetSyntax& syntax, gb::FileSystem& file_system,
    std::string_view path, std::vector<gb::ParseError>* errors = nullptr);

// Assembles a program from source text, as AssembleProgramFile does, except
// that an .INCLUDE is an error.
std::unique_ptr<AsmProgram> AssembleProgram(
    const InstructionSetSyntax& syntax, std::string text,
    std::vector<gb::ParseError>* errors = nullptr);
```

- It takes an `InstructionSetSyntax`, rather than the `InstructionSetDef` the
  design doc sketched. The syntax is built once per instruction set (and
  creating it is what checks the def), so a host that assembles several
  programs, and `oz3asm`, create it once and handle its error themselves.
- `AsmProgram` only holds the `Program` until *Program maps* adds `GetMap()`.

### Source

- **Numbers:** decimal, hexadecimal with an `h` suffix, or binary with a `b`
  suffix, where the suffix is either case (`0FFh` and `0FFH`), and a
  hexadecimal number starts with a digit. Numbers have no sign of their own:
  `-1` is unary minus applied to 1.
- **Registers** (`R0` to `R7`, `BC`, `BS`, `BD`, `BE`, `MB`, `IP`, `FP`, `SP`,
  and `D0` to `D3`) are keywords, so they are never names: not symbols, and
  not part of an expression.
- **Directives** (each one word after the `.`, in any case):

  | Directive              | Meaning                                                                 |
  | ---------------------- | ----------------------------------------------------------------------- |
  | `.INCLUDE "path"`      | The file's statements, here. The path is relative to this file.         |
  | `.CONST NAME = expr`   | Defines a constant.                                                     |
  | `.DW item, ...`        | A word for each expression, or a string's words.                        |
  | `.DD expr, ...`        | A dword for each expression, low word first.                            |
  | `.DS count`            | Reserves `count` words without writing them.                            |
  | `.STACK words`         | Words of stack each instance needs.                                     |
  | `.INSTANCES count`     | How many instances the program has (1 to 8). The default is 1.          |
  | `.BANK name[, mask]`   | Declares a bank, optionally with an assign mask (default `0FFFFh`).     |
  | `.SEGMENT name [attr]` | Switches to a segment, declaring it the first time with its attributes. |

  A segment's attributes, in any order, separated by spaces: its roles
  (`CODE`, `STACK`, `DATA`, `EXTRA`), `ROM` (RAM is the default), `INSTANCE`
  (per instance; shared is the default), `PAGE(n)` (a fixed start page),
  `PAGES(n)` (its size in pages), and `BANK(name)`. An attribute's parameter
  is in brackets, so each expression has a clear end, and the attributes
  without one read as old assemblers wrote them:

      .BANK video, VRAM0 | VRAM1
      .SEGMENT main CODE ROM
      .SEGMENT vars DATA INSTANCE
      .SEGMENT screen EXTRA PAGE(VIDEO_PAGE) PAGES(2) BANK(video)

  Attributes are only given where a segment is first declared; a later
  `.SEGMENT name` only switches to it. `PAGE`, `PAGES`, a `.BANK` mask, `.DS`,
  `.STACK`, and `.INSTANCES` take an expression, which must be known when it is
  reached (see Passes in the design doc).
- **Labels:** a label's value is the offset in the current segment where it is
  read, so a label is always in the segment it is written in. A label before
  the first `.SEGMENT`, in a program that declares segments, is an error, as is
  anything else that writes or reserves words there.
- **Local labels** are scoped from one label to the next, whichever files they
  are in, and a local label before the first label is in the scope that starts
  the program.

### Expressions

- **Widths:** an expression is evaluated in the width where it is used. So in
  a dword (`$V`, `.DD`), the arithmetic is dword arithmetic, and in a word it
  is word arithmetic.
- **Machine arithmetic** (replacing the design doc's "every step must fit, never
  a silent wrap"): a value is the width's bits, and each operator works on them
  as the CPU would. `+`, `-`, `*`, and unary `-` wrap (`-1` is `0FFFFh` as a
  word), `/` and `%` are unsigned, `<<` and `>>` are logical and give 0 for a
  count of the width or more, `~`, `&`, `|`, and `^` are bitwise, and `!` is 1
  for 0 and 0 otherwise. The only errors are a number, character, or symbol
  that doesn't fit the width, and dividing by zero.
- **A constant is evaluated where it is used, in that width.** A constant is
  its expression, so `.CONST MASK = ~1` is `0FFFEh` as a word, and
  `0FFFFFFFEh` as a dword. Its value as a symbol (for the map) is its dword
  value, which is checked once in the second pass, so an unused constant that
  doesn't fit a dword is still an error. A constant whose value depends on
  itself is an error.
- **`$` in a constant** is the offset where the constant is defined, not where
  it is used, so a constant means the same everywhere:

      table:
          .DW 1, 2, 3
      .CONST TABLE_SIZE = $ - table   ; 3, wherever TABLE_SIZE is used

- **A dword's low or high word can't be written in a word.** As a word,
  `X & 0FFFFh` is an error where `X` doesn't fit a word, as the constant is
  evaluated as a word. That is left to a backlog item, *Low and high words*,
  which suggests `LOW(X)` and `HIGH(X)`.
- An expression used in the first pass (a code word argument, a literal
  number in a pattern, and the directive values above) can only use labels and
  constants defined above it, and an expression that uses one that isn't is an
  error naming it, rather than a statement that doesn't match (see
  Instructions for where another form matches).

### Instructions

Matching follows the design doc's Matching section, except where a value ends:
- The instruction name is one token, or a name, a `.`, and its extension with
  no spaces between them (`ADD.W`).
- **A value is the longest expression at that point** (a `$#`, a value set, a
  `$v`, a `$V`, or a literal number in a pattern). This replaces the design
  doc's "every token up to the pattern's next literal, outside brackets", as
  it is how the parser reads an expression. The two agree for every pattern
  once patterns follow a new rule: **a value (a placeholder for one, or a
  literal number) can't be followed by a binary operator** (`+`, `-`, `*`,
  `/`, `%`, `&`, `|`, `^`, `<`, or `>`), which would continue the expression.
  Without it, a pattern like `"$v + 1"` could never match, as `$v` would take
  the `+ 1`. The default instruction set follows it: every value in it is
  followed by `)`, `]`, `,`, or the end.
- A value wrapped in brackets as a whole still doesn't match (`JP (5)`).
- No form matching lists the instruction's forms, as `ADD.W $r, $v`, and
  matching several lists those it matched.
- **A form matches only if its first pass values have values** (its code word
  values, and literal numbers, evaluated where the statement is), and a
  literal number's is the number. This is what makes the design doc's `SELF`
  example work: `RST SELF, CODE` matches only the `SELF` form, unless `SELF` is
  a symbol, when it matches the `$#3` form too, and is ambiguous. Without it,
  the default instruction set's `RST SELF` could never be written, as an
  undefined symbol would still match `$#3`.
- If no form matches, but a form had a first pass value with an error or a
  symbol not yet defined, the statement's error is that value's error, not
  "no form matches".

### Segments, instances, and the stack

- Each segment is shared (the default) or per instance (`INSTANCE`). A per
  instance segment can only reserve words, and can't have the `CODE` role.
- A shared segment with the `STACK` role is an error in a program with more
  than one instance, as the instances would share one stack. This is what
  lets the default program be one shared segment with all four roles: it has
  one instance, and can only have one.
- **Instance offsets:** in a shared segment, every instance's offset is 0:
  contents from the start, and the stack down from the end. In a per instance
  segment, each instance has its own `C` words: the segment's contents (which
  are only reserved), then its `.STACK` words if it has the stack role.
  Nothing is copied: the source describes one instance's words, and the
  assembler reserves `C` for each. Instance `i`'s data and extra offsets are
  `i * C`, and its stack offset is the segment's size minus `(i + 1) * C`, so
  its stack is at the top of its own words.
- **Sizes:** a segment's size is the end of its contents, plus `.STACK` words
  if it has the stack role, times the number of instances if it is per
  instance, rounded up to whole pages. `PAGES(n)` gives the size instead, which
  is an error if it is too small, and lays the segment out the same way, with
  the extra space at the end (so a shared stack gets it). A segment that would
  pass 64 Ki words is an error at the statement that passes it. Only the stack
  needs `.STACK`, as it is the only space no statement reserves: a per
  instance data or extra segment is sized by its `.DS` statements.
- Banks and segments are in the program in the order the source declares them,
  so a program orders them for the loader (see `Program::banks` and
  `Program::segments`). Without a `.BANK`, the program has one bank that may be
  any physical bank, and with them, a segment without `BANK` is an error.
- A program that gives a segment the stack role and has no `.STACK`, or has
  `.STACK` with no stack segment, is an error. Giving two segments one role is
  an error.
- Once assembled, the `Program` is checked with `ValidateProgram`, as a last
  check on rules the assembler doesn't report itself (such as a bank's pages
  adding up to more than 16), and an error there is reported at the start of
  the main file.

### Errors

Each error is a `gb::ParseError` with the file, line, and column. A pass stops
after 100 errors, with a last error saying so.

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

### Game Bits: lexer numbers

The Game Bits lexer's integer formats nearly fit, but its suffixes are matched
exactly (so `0FFH` and `1011B` aren't numbers), and a hex number doesn't need
a leading digit (so `FFh`, and a label `BEh`, would be numbers, as a number
wins a tie with an identifier). A Game Bits backlog item adds both as lexer
flags, which this feature depends on: *Number suffixes in either case, and hex
numbers that start with a digit*. With them, every number is an integer
token, and the longest match tells `1011b` (binary) from `1011` (decimal).

### tools: program lexer

```
// Returns the lexer configuration for program source.
const gb::LexerConfig& GetProgramLexerConfig();

// The local label token type (@loop).
inline constexpr gb::TokenType kTokenLocalLabel = gb::kTokenUser + 0;

// Returns a string token's words: one character per word for '...', or two
// for "...". Returns an error if a character doesn't fit.
absl::StatusOr<std::vector<uint16_t>> GetStringWords(...);

// Returns `value` as a program writes a hex number with `digits` digits,
// such as "0FFFFh".
std::string FormatHexNumber(uint32_t value, int digits);
```

- Decimal, hex (`h`) and binary (`b`) integers, 64 bits, without negative
  numbers, using the new Game Bits flags.
- Identifiers are forced to upper case (`kIdentForceUpper`), and the register
  names are case-insensitive keywords. Strings keep their case: single and
  double quoted, with doubled quotes (`kQuoteQuoteEscape`, decoded). Which
  quote a string used comes from its token text.
- `;` is a line comment, newlines are tokens (`kLineBreak`), and `@name` is a
  local label token, whose value is the whole token, `@` and all, so a local
  label's name can't be a label's.
- Symbols are the pattern lexer's (`source_pattern.cc`), `$` (the current
  offset), and `<<` and `>>`.
- `FormatHexNumber` is the rule `InstructionForm::Disassemble` uses, which moves
  here, so the two can't disagree.

### tools: `Expression`

```
enum class ExpressionWidth { kWord, kDword };

// The result of evaluating: a value, a symbol not defined (yet), or an error.
struct ExpressionResult { ... };

// What an expression's names and $ are, where it is evaluated.
class ExpressionContext {
 public:
  virtual ~ExpressionContext() = default;
  virtual uint16_t GetOffset() const = 0;  // $

  // Returns nullopt if there is no such symbol (yet), or else the symbol's
  // result, which passes through as it is (a constant's kUndefined names the
  // symbol its own expression is missing).
  virtual std::optional<ExpressionResult> GetSymbolValue(
      std::string_view name, ExpressionWidth width) const = 0;
};

// An integer expression in program source.
class Expression final {
 public:
  // Parser rules for an expression, as the rule "Expression", for a parser
  // program with the program lexer and its tokens (kProgramParserTokens).
  static std::string_view GetParserRules();

  // Creates the expression an "Expression" rule matched. `lexer` holds its
  // tokens, and must outlive the expression.
  static Expression Create(const gb::Lexer& lexer, const gb::ParsedItem& item);

  // Returns true if the expression is in brackets as a whole: "(5)".
  bool IsBracketed() const;

  ExpressionResult Evaluate(ExpressionWidth width,
                            const ExpressionContext& context) const;
};
```

- The grammar is a parser program's: each precedence level is a rule with a
  repeated group (`$args=Shift ($ops=('<<' | '>>') $args=Shift)*`), as the
  parser doesn't allow left recursion, and `Create` folds each level left to
  right. Primaries are numbers, strings, identifiers, local labels, `$`, and a
  bracketed expression.
- `Create` turns the parse tree into a vector of nodes, each with its token for
  errors, so an expression outlives its `ParseResult`, and evaluating doesn't
  walk the parse tree's maps.
- A string is a value if it makes one word (`GetStringWords`).
- Evaluating works on `uint64_t`s holding the width's bits, and masks each
  step's result to the width, so nothing wider than the machine shows through.
  Two values of 32 bits multiply within 64, so no step needs more.
- A number token's value is read as its 64 unsigned bits (the lexer gives a
  hex or binary number with its top bit set a negative int64), so it is
  checked against the width like any other number.
- Local label tokens aren't upper case (only identifiers are), so `Create`
  turns their names to upper case.
- Named values aren't `Expression`'s: a value set's names are in scope through
  a `ValueSetContext` (CL4), which looks a name up in the set first, and then
  in the context it wraps. That keeps `Expression` free of instruction sets,
  for the debugger's watches.
- Constants are evaluated where they are used, so the assembler's context
  (CL5) evaluates a constant's expression on each use, and may cache it by
  width. An error in a constant's own expression comes back at each use, so
  the assembler reports a constant's errors once, where it is defined, except
  one that is only an error as a word, which is the use's (see CL5's notes
  under program assembler). A symbol's error without a location is reported
  at its use.

### tools, wiki: values followed by operators

`SourcePattern` checks the new pattern rule (see Instructions) as it parses
each instruction and macro code pattern, as one of the rules every pattern
follows. `InstructionSetSyntax::Create` checks it again in each form, where a
macro code's value can be followed by the instruction's literal after its
`$m`, or the instruction's value by the macro code's first literal. The wiki's
Instruction Set Source page gains the rule.

### tools: `ProgramGrammar`

```
// One argument of a matched form: a register's number, or a value's
// expression, in the order of InstructionForm::GetArgs().
struct InstructionArg {
  int reg = 0;
  std::optional<Expression> value;
};

struct InstructionMatch {
  const InstructionForm* form;
  std::vector<InstructionArg> args;
};

// A context in which a value set's names are its values, ahead of any symbol
// of the same name in the context it wraps, for a code word argument that
// takes named values ($Condition).
class ValueSetContext final : public ExpressionContext { ... };

// The grammar of programs for one instruction set.
class ProgramGrammar final {
 public:
  // The rule for a line: an optional $label, then a $directive or an
  // $instruction (its name, and its $args: every token to the end of the
  // line, with brackets balanced), or nothing, then the $end of the line.
  static constexpr std::string_view kLineRule = "Line";

  // Creates the grammar for `syntax`, which must outlive it. Returns an error
  // if a pattern has literal text the program lexer can't read as one token.
  static absl::StatusOr<std::unique_ptr<ProgramGrammar>> Create(
      const InstructionSetSyntax& syntax);

  // Return a line's directive name ("CONST") or instruction name ("ADD.W"),
  // or empty with an error if it has a space around its '.'.
  static std::string_view GetDirectiveName(const gb::Lexer& lexer,
                                           const gb::ParsedItem& directive,
                                           gb::ParseError& error);
  static std::string_view GetInstructionName(const gb::Lexer& lexer,
                                             const gb::ParsedItem& instruction,
                                             gb::ParseError& error);

  // Returns the parser program, for a gb::Parser to parse with.
  std::shared_ptr<const gb::ParserProgram> GetParserProgram() const;

  // Matches the instruction of a parsed line against each of its name's
  // forms, from its first argument, evaluating first pass values in
  // `context`. Returns true with every form that matches, or false with an
  // error: in its name, an unknown name, or in a first pass value if no form
  // matches. Leaves the lexer where it was.
  bool MatchInstruction(gb::Parser& parser, gb::LexerContentId content,
                        const gb::ParsedItem& line,
                        const ExpressionContext& context,
                        std::vector<InstructionMatch>& matches,
                        gb::ParseError& error) const;
};
```

- `MatchInstruction`, `GetDirectiveName`, and `GetInstructionName` set a
  `gb::ParseError`, rather than returning an `absl::StatusOr`, so the error
  keeps its location, as `InstructionForm::Encode` returns a bool and an
  error. A directive's name and an instruction's name both have no spaces
  around their `.`, which the grammar can't say, so these check it. Game Bits
  *Adjacent rule items* (requested for this) would put it in the grammar, and
  then these two go.
- `GetInstructionName` returns the name as written, a view of the lexer's
  text from the name to the extension, which `InstructionSetSyntax::GetForms`
  looks up ignoring case, and errors quote as the program wrote it.
- `Create` returns an error rather than null, as a pattern's literal text could
  be something the program lexer doesn't read (a float, such as `2.5`).

The parser program's text is built from the syntax, so it is a grammar like
the instruction assembler's, written for one instruction set:
- **`Line`**, the one rule the assembler parses with: an optional `Label` (a
  name or local label, then `:`), then a `Directive` or an `Instruction`, or
  nothing, then `EndOfLine` (a line break or the end).
- **`Directive`** is `.` and one alternative for each directive, with its
  operands, so the parser tells the directives apart. As the parser reports
  the error that gets furthest into the line, a directive with a bad operand
  gets that operand's error (`.CONST X 5` expects `=` at the `5`), and an
  unknown one gets `Directive`'s own ("Unknown directive"). Its values are
  `Expression`, from `Expression::GetParserRules()`.
- **`Instruction`** is the name, with an optional `.` and extension, then its
  arguments as any tokens (`ArgToken*`), with brackets balanced. `ArgToken` is
  built from the program lexer's symbols and keywords, as the parser has no
  token type for any token. Game Bits *Negated rule items and position
  markers* (requested for this) would make it a fixed rule, with a negation
  for "any token but a line break or closing bracket", and a marker would name
  where the arguments start even when there are none. Once it lands, the
  generated rule goes.
- **Register rules** for each register size, as alternatives of the register
  keywords: `$r2` is `"R0"` to `"R3"`, `$r` adds `"R4"` to `"R7"`, `$r4` adds
  `"BC"` to `"SP"`, and `$R` is `"D0"` to `"D3"`.
- **A rule for each distinct form pattern**, from its parts in order, then
  `EndOfLine`. Literal text is a token literal (`"SP"`, `'('`), a register is
  its register rule, and a code word value, `$v`, `$V`, and a literal number
  are each an `Expression`, named by their order (`$a0`, `$a1`). Forms with the
  same pattern (`"$r, $m"` with the same macro code) share a rule.

A program is parsed with `Line` a line at a time, rather than as one parse of
the whole program, as a parse stops at its first error, and the assembler
reports every error in a pass. A line that doesn't parse leaves the lexer at
its start, so the assembler reports it and goes on at the next line
(`gb::Lexer::NextLine`).

`MatchInstruction` is what the parser can't do itself: it reports every form
that matches, rather than the first. For each of the name's forms, it sets the
lexer back to the instruction's first argument (`gb::Lexer::SetNextToken`),
and parses with the form's rule (once for each rule, for all the forms that
share it). A parse that succeeds is a match once no value is bracketed as a
whole, and its first pass values have values, with literal numbers' values the
numbers (see Instructions). A form whose rule doesn't parse doesn't match, and
its parse error isn't reported.

**Performance:** the grammar is built for each assembly, with a rule for each
distinct form pattern, and matching parses a line once with `Line`, and then
once for each distinct rule of its name's forms. CL4 measured both for the
default instruction set (1335 forms), with a temporary test that parses and
matches each form's disassembly:

| Build          | Syntax  | Grammar | A line (`Line`, then 17.4 forms per name) |
| -------------- | ------- | ------- | ----------------------------------------- |
| RelWithDebInfo | 7.5 ms  | 4.9 ms  | 34 µs (`Line` is 13 µs)                   |
| Debug          | 98 ms   | 75 ms   | 570 µs (`Line` is 170 µs)                 |

Building the grammar costs less than creating the syntax, so it stays per
assembly. A release build reads about 30,000 instruction lines a second. Every
form's disassembly matched its own form, and only it. If parsing ever becomes
a bottleneck, the fix is to make the Game Bits parser faster (a Game Bits
backlog item), not to work around it here.

### tools: program assembler

`ProgramAssembler` (in the `.cc`) runs the two passes:
1. Reads each file's lines with one `gb::Parser`, following includes (each
   file's content is added to the parser's lexer once, and rewound to include
   it again), matching instructions, evaluating first pass expressions,
   defining labels and constants, and recording each statement that writes
   words: its segment, offset, scope, and its form and arguments, or its data
   items. A line with a parse error is reported, and reading goes on at the
   next line.
2. Evaluates and encodes each recorded statement into its segment's words,
   marking which words are written, and then builds the `Program`: banks,
   segments with their data blocks (each run of written words, widened to
   aligned pairs, with unwritten words in a pair as 0), roles, and instances.

Labels are one table by upper case name, with local labels keyed by their
scope, and constants are another, in the order they are defined. A name is a
label or a constant, never both. Each recorded statement is an instruction or
data, with the context its expressions need (offset and scope).

CL5 settled the details:
- **Errors are reported once each**, by their formatted message, which is how
  a constant's error is reported once: each use evaluates the constant again,
  and gets the same error at the same place in the constant.
- **A `.CONST` is a statement too**, so the second pass checks each
  constant's dword value in the order of the source, along with encoding the
  statements that write words. Statements are a variant of an instruction,
  data, or a constant, which another kind of statement can join.
- **A scope of local labels is its label's name**, a view of the label's
  token, which lives as long as the lexer, so statements and constants keep
  their scope without a table of scopes. A local label's key is its scope's
  label and then its name, which starts with `@`, which no label's name can
  have, so no key is another's.
- **A constant that has a value, but can't be evaluated as a word, is an error
  where it is used as one**, as the constant itself is fine: `.CONST BIG =
  70000`, then `.DW BIG`, is "BIG can't be evaluated as a word: Value 70000
  does not fit in a word" at the `BIG` of each `.DW`. The assembler's context
  returns that error without a location, which `Expression` puts at the
  symbol's use. A constant with no dword value is reported once, at the
  constant.
- A constant's results are cached by width, except a symbol not yet defined,
  which can be defined later. A constant that is being evaluated when it is
  reached again depends on itself.
- **Code word values are evaluated once.** `MatchInstruction` keeps each one's
  value (`InstructionArg::code_value`), and the first pass reports one that
  doesn't fit its bits, at its expression. The second pass evaluates only `$v`
  and `$V`.
- `.DW` takes a string on its own as its words, which `Expression::IsString`
  tells it, and anything else as an expression.
- An absolute include path is read as written, and a relative one is joined to
  the including file's folder.
- A first pass use of a symbol not yet defined has `Expression`'s own message
  ("X is not defined"), whether or not X is defined later.
- `.SEGMENT`, `.BANK`, and `.INSTANCES` parse, but are errors ("not supported
  yet") until the CLs that add them (CL7 to CL10).
- With a temporary test (not checked in), the design doc's example program,
  without its segments, assembled for the default instruction set, and each
  instruction disassembled to the form it was written as.

CL7 settled segments' details:
- **The default program's segment is a segment like any other**, unnamed and
  with every role, which the first `.SEGMENT` replaces. The first label or
  statement placed in a segment is kept, and the first `.SEGMENT` reports it,
  once, as being before it.
- **Each segment has its own reserved size**, which is the offset of its next
  statement, so labels, `$`, and statements take their offsets from the
  current segment, and each statement records its segment.
- **The stack is part of the stack segment's size.** At the end of the first
  pass, its `.STACK` words are added to the segment's reserved size, after
  its contents, so a segment's size is its reserved size, which is what
  `PAGES` and per instance sizes build on.
- **A segment's words are only as long as its written size** (the end of its
  last statement that writes words), so a segment that only reserves words
  (the stack, `.DS`) has no buffer.
- **Roles are indexed by `CpuCore`'s bank indices** (`CpuCore::CODE` and the
  others), for their names in `.SEGMENT` and their segments.
- A constant before the first `.SEGMENT` is defined at offset 0, so `$` is 0
  in it.
- A label on a `.SEGMENT` line is read before the directive, so it is in the
  segment before.
- Segment names are their own, apart from symbols.

CL8 settled the segment attributes' details:
- **Each attribute is given at most once**, which one check covers for every
  attribute, roles included.
- `PAGE` and `PAGES` are first pass word expressions, evaluated at the
  segment's start, so `$` is 0 in them.
- **A segment's page count is set at the end of the first pass**, from
  `PAGES` or its reserved size, after the stack is added. `PAGES` too small
  is an error at `PAGES`, and a segment with `PAGE` past the end of the bank
  is an error at `PAGE`. A segment that passes 64 Kiw is only reported where
  it passes it.
- Segments with more than 16 pages in a bank between them, and overlapping
  fixed pages, are left to `ValidateProgram`.
- `ROM` is allowed with any role, the stack's included, as the loader allows
  it.
- With a temporary test (not checked in), the design doc's example program,
  with its segments and `ROM`, assembled for the default instruction set, and
  each instruction disassembled to the form it was written as.

CL9 settled banks' details:
- **A segment's bank is looked up when the first pass ends**, so a `.BANK`
  can be after the segments that name it, as `.BANK` changes no offset. A
  segment without `BANK` in a program with banks is an error then too, at
  the segment's name, and a program with banks and no segments is an error at
  its first `.BANK`.
- A mask is a first pass word expression, and a bank with an error in its
  mask is still declared, so its segments aren't errors too. A mask of 0 is
  an error, as the bank could never be loaded.
- Bank names are their own, apart from symbols and segments.
- More than 16 banks is left to `ValidateProgram`.

CL10 settled instances' details:
- **A per instance segment's reserved size is one instance's words**, and its
  size is that times the instance count, set with its page count when the
  first pass ends, once the count is known wherever `.INSTANCES` is. Its
  instances passing 64 Kiw is an error at its `INSTANCE`.
- **Each statement that writes words in a per instance segment is an error**,
  like every other statement error, rather than once for the segment.
- A segment with both `CODE` and `INSTANCE` is an error at `INSTANCE`, as the
  roles are known by the end of the attributes.
- A shared stack segment with more than one instance is an error at the
  segment's name, or for the default program, at `.INSTANCES`.
- An instance's offset is 0 in a shared segment. In a per instance segment it
  is the start of its words, and the stack's is from the segment's end to the
  end of its words, so `PAGES`' extra space is after the last instance.

**Brittleness:**
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

### To confirm

- A parser program's token literal that is a keyword (`"SP"`) matches the keyword
  token, and `%ident` doesn't match a keyword (CL4, as it needs a grammar).
  **Confirmed:** a token literal is read with the program lexer, so `"SP"` is
  the keyword and matches `sp`, `"SELF"` matches `self`, and `PUSH.W R1`
  doesn't match `$v`.
- Case-insensitive keywords with `kIdentForceUpper` match `sp` and `Sp`, and
  `R10` is an identifier, not `R1` and `0` (CL1). **Confirmed:** a keyword's
  value is the keyword as configured (`sp` is `SP`), and `R10`, `R8`, `D4`,
  and `SPX` are identifiers.
- `kIdentForceUpper` leaves strings and user tokens as written, and
  `kQuoteQuoteEscape` with `kDecodeEscape` decodes `'It''s'` (CL1).
  **Confirmed.**
- Newline tokens with `kLineBreak` come after a line comment (CL1).
  **Confirmed**, and every line ends with one, including the last.
- A repeated group that fails partway (`5 + R1` as an expression) rewinds to
  before the group, so the expression is `5`. The parser's `MatchGroup` reads
  as if it does (CL2). **Confirmed:** parsing `5 + R1` as an expression takes
  `5`, and leaves the lexer at the `+`.
- `Parser::Parse` of a form's rule leaves the lexer just past the match, and
  `SetNextToken` to `args` works after a failed parse (CL4). **Confirmed.**

## CLs

### CL1 [x] tools: program lexer

Depends on: Game Bits *Number suffixes in either case, and hex numbers that
start with a digit*.

- `program_lexer.h`/`.cc`: `GetProgramLexerConfig`, `kTokenLocalLabel`,
  `GetStringWords`, and `FormatHexNumber`.
- `InstructionForm::Disassemble` uses `FormatHexNumber`.
- Unused otherwise, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: each token kind; hex and binary in both suffix cases, with and
  without a leading digit, and numbers too big; names forced to upper case
  with strings unchanged; registers as keywords in any case; string words for
  one and two characters per word, odd lengths, doubled quotes, UTF-8, and
  characters that don't fit; comments and newlines; `FormatHexNumber`.
- The To confirm items for CL1, with the findings recorded here.
- Disassemble's tests pass unchanged.

### CL2 [x] tools: `Expression`

Depends on: CL1.

- `expression.h`/`.cc`: `Expression`, `ExpressionContext`.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests, parsing with a test grammar of the program lexer and
  `GetParserRules()`: every operator and precedence level, left to right, and
  brackets; `IsBracketed`; an expression ending where the next token can't
  continue it (`5 + R1`, `N ROM`); wrapping in each width; unsigned division;
  logical shifts, and counts of the width or more; numbers and symbols that
  don't fit; division by zero; characters and strings; `$`; symbols defined, undefined, and in
  error, and a symbol whose own expression is missing a symbol; registers
  rejected, with locations.

### CL3 [x] tools, wiki: values followed by operators

Depends on: nothing.

- `SourcePattern::Parse` rejects a value placeholder or literal number
  followed by a binary operator, through `SourcePattern::CheckValueEnds`, which
  `InstructionSetSyntax::Create` also calls on each form.
- Wiki: Instruction Set Source gains the rule.

**Verify**
- Standard checks.
- Unit tests: each operator after `$v`, `$V`, `$#`, a value set, and a
  number; one after a macro code's value, from the instruction's pattern, and
  the reverse; patterns that follow the rule (`")"`, `"]"`, `","`, a word).
- The default instruction set still builds its syntax (its tests pass).

### CL4 [x] tools: `ProgramGrammar`

Depends on: CL2, CL3.

- `program_grammar.h`/`.cc`: `ProgramGrammar`, `InstructionMatch`,
  `InstructionArg`, and `ValueSetContext`.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests, on small instruction sets assembled from `.izm` text in the
  test: lines parse (labels, each directive and its errors, instructions and
  their arguments, and the line after one with an error);
  full names and roots; registers by size (`$r2`, `$r4`, `$R`); literal words
  and registers in patterns, in any case; literal numbers by value, and with a
  symbol not yet defined; brackets inside a value, and around a whole value;
  a register never matching a value; value set names (`ValueSetContext`), in
  any case, ahead of a symbol of the same name; code word values with errors;
  forms sharing a rule; ambiguity across a root (`PUSH 5`) and through a
  literal word (`RST SELF, 1`, only where `SELF` is a symbol); no match; text
  after a matched form; where the lexer is left.
- The To confirm item for CL4.
- Performance: building the grammar for the default instruction set, and
  matching its forms, measured (with a temporary test in `instruction_sets`,
  not checked in), and recorded here.

### CL5 [x] tools, wiki: assembling the default program

Depends on: CL4.

- `program_assembler.h`/`.cc`: `AsmProgram`, `AssembleProgramFile`,
  `AssembleProgram`, and everything but declared segments, banks, and
  instances: statements, labels, local labels, constants, `$`, instructions,
  `.DW`, `.DD`, `.DS`, `.STACK`, `.CONST`, `.INCLUDE`, the two passes, the
  error limit, and the default program's one segment.
- Wiki: the new Program Source page, for all of the above, and a link from
  Home's `oz3asm` entry.

**Verify**
- Standard checks.
- Unit tests: each directive; labels and local labels, forward and back, and
  their scopes; constants evaluated in each width, `$` in a constant,
  recursive constants, and first pass uses defined below; relative
  instructions; strings in `.DW`; data blocks around `.DS` (aligned pairs,
  unwritten words left out); the default program's size, roles, and instance;
  includes (relative paths, including one file twice, a cycle, and
  `AssembleProgram` rejecting one); every error, with locations, every error
  in a pass, carrying on after a line with a parse error, and the limit; the
  output passing `ValidateProgram`.

### CL6 [x] tools: `ExpressionContext::GetSymbolValue`

Depends on: CL5.

- `ExpressionContext::GetSymbol` is `GetSymbolValue`, as it returns a symbol's
  value where it is used, not the symbol, and so are the assembler's and the
  tests' overrides. No other change.

**Verify**
- Standard checks.

### CL7 [x] tools, wiki: declared segments and roles

Depends on: CL6.

- `.SEGMENT name` with role attributes (`CODE`, `STACK`, `DATA`, `EXTRA`):
  declaring segments, and switching between them, each with its own offsets,
  words, and data blocks; roles; and the stack in the stack segment. A program
  without `.SEGMENT` is the default program, as before.
- The other attributes (`ROM`, `PAGE`, `PAGES`, `BANK`, `INSTANCE`), `.BANK`,
  and `.INSTANCES` stay errors ("not supported yet"), so a program has one
  bank and one instance.
- Wiki: Program Source's segments and roles.

**Verify**
- Standard checks.
- Unit tests: switching between segments, and back; attributes only where a
  segment is first declared; statements and labels before the first
  `.SEGMENT`, and constants and `.STACK` there; roles (none, several in one
  segment, one in two, one twice); sizes from contents and the stack, and too
  big; a stack segment without `.STACK`, and `.STACK` without one; a label's
  value in its segment; segments in the order declared.
- A program with several segments loads with `ProgramLoader` (in a `tools`
  test, as `program_loader_test.cc` does).

### CL8 [x] tools, wiki: `ROM`, `PAGE`, and `PAGES`

Depends on: CL7.

- The segment attributes `ROM`, `PAGE(n)` (a fixed start page), and `PAGES(n)`
  (its size, laid out the same, with the extra space at the end).
- Wiki: Program Source's segment attributes.

**Verify**
- Standard checks.
- Unit tests: each attribute, with expressions, used in the first pass;
  `PAGES` too small, and giving a shared stack the extra space; a page out of
  range; the design doc's example program; a program that `ValidateProgram`
  rejects (a fixed page and size past the end of a bank).

### CL9 [x] tools, wiki: banks

Depends on: CL8.

- `.BANK name[, mask]` and the segment attribute `BANK(name)`. Banks are in
  the program in the order declared, and with any `.BANK`, a segment without
  `BANK` is an error.
- Wiki: Program Source's banks.

**Verify**
- Standard checks.
- Unit tests: banks and masks, with expressions; the default mask; a bank
  declared twice, and one not declared; a segment without `BANK`; segments in
  several banks.

### CL10 [x] tools, wiki: instances

Depends on: CL9.

- `.INSTANCES count` and the segment attribute `INSTANCE`: per instance
  segments, their sizes, and each instance's offsets.
- Wiki: Program Source's instances. The assembler is complete with this CL, so
  Instruction Set Source's "[OZ-3 assembler](home) (planned)" links to Program
  Source, without "(planned)" (`oz3asm` stays planned until *oz3asm*).

**Verify**
- Standard checks.
- Unit tests: per instance rules (only reserving, never `CODE`); a shared
  stack with one and several instances; sizes from instances; instance offsets
  for each role, shared and per instance; the count's range.
- A program with several segments and instances loads with `ProgramLoader`.
