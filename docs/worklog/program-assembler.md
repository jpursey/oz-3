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

- **Widths:** an expression is evaluated in the width where it is used, and
  each step must fit that width. So in a dword (`$V`, `.DD`), the arithmetic is
  dword arithmetic, and in a word it is word arithmetic.
- Arithmetic (unary `-`, `*`, `/`, `%`, `+`, `-`) is on the integers as
  written. `/` and `%` round toward zero, as in C, and dividing by zero is an
  error.
- Bitwise operators (`~`, `&`, `|`, `^`, `<<`, `>>`) work on the bits of the
  width: each operand is first taken as its unsigned bits in the width (so `-1`
  is `0FFFFh` as a word), and the result is unsigned. So `~0FF00h` is `00FFh`
  as a word, and `-1 & 0FFh` is `0FFh`. `<<` is an error if it shifts out a set
  bit, and `>>` is logical. `!` is 1 for 0 and 0 otherwise.
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
  `X & 0FFFFh` is an error where `X` doesn't fit a word, as `X` is a step of
  the word expression. Evaluating the steps as dwords and checking only the
  result would fix that, but bitwise operators take their bits from the width,
  so `~MASK` as a word would then be `0FFFFFFxxh`, which doesn't fit. Masks are
  far more common than splitting dwords, so that is left to a backlog item,
  *Low and high words*, which suggests `LOW(X)` and `HIGH(X)`.
- An expression used in the first pass (a code word argument, a literal
  number in a pattern, and the directive values above) can only use labels and
  constants defined above it, and an expression that uses one that isn't is an
  error naming it, rather than a statement that doesn't match.

### Instructions

Matching follows the design doc's Matching section, except where a value ends:
- The instruction name is one token, or a name, a `.`, and its extension with
  no spaces between them (`ADD.W`).
- **A value is the longest expression at that point** (a `$#`, a value set, a
  `$v`, a `$V`, or a literal number in a pattern). This replaces the design
  doc's "every token up to the pattern's next literal, outside brackets", as
  it is how the parser reads an expression. The two agree for every pattern
  once patterns follow a new rule: **a placeholder for a value can't be
  followed by a binary operator** (`+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `<`,
  or `>`), which would continue the expression. Without it, a pattern like
  `"$v + 1"` could never match, as `$v` would take the `+ 1`. The default
  instruction set follows it: every value in it is followed by `)`, `]`, `,`,
  or the end.
- A value wrapped in brackets as a whole still doesn't match (`JP (5)`).
- No form matching lists the instruction's forms, as `ADD.W $r, $v`, and
  matching several lists those it matched.
- An error in a value that decides the match (a literal number's expression
  using a symbol not yet defined) is that error, not "no form matches".

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

The gb lexer's integer formats nearly fit, but its suffixes are matched
exactly (so `0FFH` and `1011B` aren't numbers), and a hex number doesn't need
a leading digit (so `FFh`, and a label `BEh`, would be numbers, as a number
wins a tie with an identifier). A Game Bits backlog item adds both as lexer
flags, which this feature depends on: *Number suffixes in either case, and hex
numbers that start with a digit*. With them, every number is a gb integer
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
  local label token. Its regex captures the whole token, `@` and all, as
  `source_pattern.cc`'s placeholders do: a user token read again after the
  parser backtracks, which matching every form does, has its whole text as
  its value rather than its capture (Game Bits *Re-read user tokens keep their
  capture*).
- Symbols are the pattern lexer's (`source_pattern.cc`), `$` (the current
  offset), and `<<` and `>>`.
- `FormatHexNumber` is the rule `InstructionForm::Disassemble` uses, which moves
  here, so the two can't disagree.

### tools: `Expression`

```
// An integer expression in program source.
class Expression final {
 public:
  enum class Width { kWord, kDword };

  // The result of evaluating: a value, a symbol not defined (yet), or an
  // error.
  struct Result { ... };

  // Parser rules for an expression, as the rule "Expression", for a parser
  // program with the program lexer.
  static constexpr std::string_view kParserRules = ...;

  // Creates the expression an "Expression" rule matched. `lexer` holds its
  // tokens, and must outlive the expression.
  static Expression Create(const gb::Lexer& lexer, const gb::ParsedItem& item);

  // Returns true if the expression is in brackets as a whole: "(5)".
  bool IsBracketed() const;

  // Evaluates the expression in `width`. A name in `value_set` is its value,
  // ahead of any symbol.
  Result Evaluate(Width width, const ExpressionContext& context,
                  const ValueSetDef* value_set = nullptr) const;
};

// What an expression's names and $ are, where it is evaluated.
class ExpressionContext {
 public:
  virtual ~ExpressionContext() = default;
  virtual uint16_t GetOffset() const = 0;  // $
  virtual Expression::Result GetSymbol(std::string_view name,
                                       Expression::Width width) const = 0;
};
```

- The grammar is the gb parser's: each precedence level is a rule with a
  repeated group (`$args=Shift ($ops=('<<' | '>>') $args=Shift)*`), as the
  parser doesn't allow left recursion, and `Create` folds each level left to
  right. Primaries are numbers, strings, identifiers, local labels, `$`, and a
  bracketed expression.
- `Create` turns the parse tree into a vector of nodes, each with its token for
  errors, so an expression outlives its `ParseResult`, and evaluating doesn't
  walk the parse tree's maps.
- A string is a value if it makes one word (`GetStringWords`).
- Evaluating checks every node's result against the width.

### tools, wiki: values followed by operators

`InstructionSetSyntax::Create` checks the new pattern rule (see Instructions),
in instruction and macro code patterns, and in each form, where a macro code's
value can be followed by the instruction's literal after its `$m`. The wiki's
Instruction Set Source page gains the rule.

### tools: `ProgramGrammar`

```
// One argument of a matched form: a register's number, or a value's
// expression, in the order of InstructionForm::GetArgs().
struct InstructionArg { ... };

struct InstructionMatch {
  const InstructionForm* form;
  std::vector<InstructionArg> args;
};

// The grammar of programs for one instruction set.
class ProgramGrammar final {
 public:
  // Creates the grammar for `syntax`, which must outlive it.
  static std::unique_ptr<ProgramGrammar> Create(
      const InstructionSetSyntax& syntax);

  // Returns the parser program, for a gb::Parser to parse with.
  std::shared_ptr<const gb::ParserProgram> GetParserProgram() const;

  // Matches the arguments of an instruction named `name` ("ADD" or
  // "ADD.W"), which start at `args` (the token after the name) and run to the
  // end of the line, against each of the name's forms. Literal numbers in a
  // pattern are evaluated in `context`. Returns every form that matches, or
  // the error in a value that decides a match.
  absl::StatusOr<std::vector<InstructionMatch>> MatchInstruction(
      gb::Parser& parser, gb::LexerContentId content, std::string_view name,
      gb::Token args, const ExpressionContext& context) const;
};
```

The parser program's text is built from the syntax, so it is a gb grammar like
the instruction assembler's, written for one instruction set:
- **The assembler's rules:** `Label` (a name or local label, then `:`),
  `Directive` (`.` and each directive's operands, with `Expression` from
  `Expression::kParserRules`), `InstructionName` (a name, with an optional `.`
  and extension), and `EndOfLine` (a line break or the end).
- **Register rules** for each register size, as alternatives of the register
  keywords: `$r2` is `"R0"` to `"R3"`, `$r` adds `"R4"` to `"R7"`, `$r4` adds
  `"BC"` to `"SP"`, and `$R` is `"D0"` to `"D3"`.
- **A rule for each distinct form pattern**, from its parts in order, then
  `EndOfLine`. Literal text is a token literal (`"SP"`, `'('`), a register is
  its register rule, and a code word value, `$v`, `$V`, and a literal number
  are each an `Expression`, named by their order (`$a0`, `$a1`). Forms with the
  same pattern (`"$r, $m"` with the same macro code) share a rule.

`MatchInstruction` is what the gb parser can't do itself: it reports every
alternative that matches, rather than the first. For each of the name's forms,
it sets the lexer back to `args` (`gb::Lexer::SetNextToken`), and parses with
the form's rule. A parse that succeeds is a match once its literal numbers
have their values and no value is bracketed as a whole. A form whose rule
doesn't parse doesn't match, and its parse error isn't reported.

The assembler reads a line as: an optional `Label`, then a `Directive`, or an
`InstructionName` and its forms, or nothing, then `EndOfLine`. Each is a
separate parse from where the last ended, so the grammar never needs one rule
for every instruction.

**Performance:** the grammar is built for each assembly, with a rule for each
distinct form pattern, and matching parses a line once for each of its name's
forms. CL4 measures both for the default instruction set. If
building is slow, a host could create the `ProgramGrammar` once and pass it
in, in place of the syntax.

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

Symbols are one table by upper case name, with local labels keyed by their
scope. Each recorded statement has the context its expressions need (offset
and scope).

**Brittleness:**
- Expressions and recorded statements hold tokens of the assembler's lexer,
  which lives as long as the assembly.
- An instruction named like a register (`SP`) can't be written, as the name
  is a keyword. `InstructionSetSyntax` doesn't reject one, and no instruction
  set has one; it can if one ever does.
- Two literal `<` (or `>`) parts in a row in a pattern lex as `<<` in a
  program, so they can't match. No pattern has them.

### To confirm

- A gb parser token literal that is a keyword (`"SP"`) matches the keyword
  token, and `%ident` doesn't match a keyword (CL1).
- Case-insensitive keywords with `kIdentForceUpper` match `sp` and `Sp`, and
  `R10` is an identifier, not `R1` and `0` (CL1).
- `kIdentForceUpper` leaves strings and user tokens as written, and
  `kQuoteQuoteEscape` with `kDecodeEscape` decodes `'It''s'` (CL1).
- Newline tokens with `kLineBreak` come after a line comment (CL1).
- A repeated group that fails partway (`5 + R1` as an expression) rewinds to
  before the group, so the expression is `5`. The parser's `MatchGroup` reads
  as if it does (CL2).
- `Parser::Parse` of a form's rule leaves the lexer just past the match, and
  `SetNextToken` to `args` works after a failed parse (CL4).

## CLs

### CL1 [ ] tools: program lexer

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

### CL2 [ ] tools: `Expression`

Depends on: CL1.

- `expression.h`/`.cc`: `Expression`, `ExpressionContext`.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests, parsing with a test grammar of the program lexer and
  `kParserRules`: every operator and precedence level, left to right, and
  brackets; `IsBracketed`; an expression ending where the next token can't
  continue it (`5 + R1`, `N ROM`); each width's limits, at every step
  (`A * B / C`); bitwise operators on negative values; division by zero and
  shifts out; characters and strings; `$`; symbols defined, undefined, and in
  error; a value set ahead of a symbol; registers rejected, with locations.

### CL3 [ ] tools, wiki: values followed by operators

Depends on: nothing.

- `InstructionSetSyntax::Create` rejects a value placeholder followed by a
  binary operator, in an instruction's or macro code's pattern, or in a form.
- Wiki: Instruction Set Source gains the rule.

**Verify**
- Standard checks.
- Unit tests: each operator after `$v`, `$V`, `$#`, and a value set; one
  after a macro code's value, from the instruction's pattern; patterns that
  follow the rule (`")"`, `"]"`, `","`, a word).
- The default instruction set still builds its syntax (its tests pass).

### CL4 [ ] tools: `ProgramGrammar`

Depends on: CL2, CL3.

- `program_grammar.h`/`.cc`: `ProgramGrammar`, `InstructionMatch`, and
  `InstructionArg`.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests, on small instruction sets assembled from `.izm` text in the
  test: the generated rules parse (labels, each directive, instruction names);
  full names and roots; registers by size (`$r2`, `$r4`, `$R`); literal words
  and registers in patterns, in any case; literal numbers by value, and with a
  symbol not yet defined; brackets inside a value, and around a whole value;
  a register never matching a value; value set names, and a symbol of the same
  name; forms sharing a rule; ambiguity across a root (`PUSH 5`) and through a
  literal word (`RST SELF, 1`); no match; text after a matched form.
- The To confirm item for CL4.
- Performance: building the grammar for the default instruction set, and
  matching its forms, measured (with a temporary test in `instruction_sets`,
  not checked in), and recorded here.

### CL5 [ ] tools, wiki: assembling the default program

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

### CL6 [ ] tools, wiki: segments, banks, and instances

Depends on: CL5.

- `.SEGMENT`, `.BANK`, and `.INSTANCES`: declared segments and their
  attributes, roles, sizes, per instance segments, and instance offsets.
- Wiki: Program Source's segments, banks, and instances.

**Verify**
- Standard checks.
- Unit tests: each attribute, with expressions; switching between segments;
  statements before the first `.SEGMENT`; roles (none, several in one segment,
  one in two); sizes from contents, stack, instances, and `PAGES`, and too
  big; per instance rules; a shared stack with one and several instances;
  instance offsets for each role; banks and masks; the design doc's example
  program; a program that `ValidateProgram` rejects.
- A program with several segments and instances loads with `ProgramLoader`
  (in a `tools` test, as `program_loader_test.cc` does).
