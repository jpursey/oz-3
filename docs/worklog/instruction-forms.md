# Instruction forms

The program assembler and the debugger both need an instruction set's syntax as
a table: every way each instruction can be written, what code word each one
is, and how many words it takes (see
[program_assembler.md](../program_assembler.md), Instructions). This adds that
table to `tools` as `InstructionSetSyntax`, built from any `InstructionSetDef`.
Each form encodes its arguments into words, and turns its words back into
text. Matching a statement to a form is left to *Program assembler*, which owns
the statement and expression grammar, so this gives it each name's forms with
their parsed patterns.

Building it is also what checks that the tools can use an `InstructionSetDef`,
whether it was assembled from `.izm` source or written by hand.
`CompileInstructionSet` only checks what cores need, so the rules for patterns,
value sets, and sizes that the instruction assembler checks today move into
`InstructionSetSyntax`, and the instruction assembler builds one to check its
result. The two places that parse patterns today share one pattern parser.

Programs are case-insensitive, so the names they write from an instruction set
become case-insensitive in `.izm` source too.

Nothing assembles programs yet, so the only visible changes are to `.izm`
source (see Behavior).

This goes beyond the backlog item's **Layers** (tools only): it adds a value
set location to `InstructionError` in `core`, and updates the wiki's
Instruction Set Source page.

The backlog item suggested the default instruction set's pin test fingerprint
forms instead of scanning patterns for value sets itself. It stays as it is:
its fingerprint already covers everything forms are built from, so any change
to a form changes it, and fingerprinting forms would tie the pin to how
`tools` presents them, so a refactor there could break it with no code word
changed. `IsPlaceholderName` stays in `core` for it, where it states a rule of
`ValueSetDef`, and `SourcePattern` uses it.

## Behavior

### Instruction set source (`.izm`)

- **Names programs write are case-insensitive.** Instruction names (with their
  extension) and value names are turned to upper case as they are read, so
  `add.w` is `ADD.W`, and defining both is an "already defined" error. Macro
  names, value set names, and microcode stay case-sensitive, as programs never
  write them, and microcode tells `a` from `A` (as patterns tell `$r` from
  `$R`). Literal words in patterns are kept as written, and matched ignoring
  case.
- **A macro's codes can't be written the same way.** This was "each code's
  source must be unique in the macro, ignoring spacing", which missed codes a
  program still can't tell apart. Two codes are the same if their patterns
  have the same parts, where literal words are compared ignoring case, a
  register placeholder is the same as any other of its kind (`$r` and `$r2`
  both match `R1`), and code word values, `$v`, and `$V` are the same as each
  other and as any literal number (`"$#4"`, `"$v"`, and `"3"` all match `3`).
  Codes that differ only in literal numbers (`"1"` and `"2"`) are different.
  Since an instruction uses one macro, this is what keeps every statement that
  names a full instruction to one form. Different instructions with the same
  root may still overlap, and a program names the full instruction there.

The default instruction set already follows both, and its def doesn't change.

### Forms

An instruction without a macro argument has one form, its source pattern. One
with a macro argument has a form for each of its macro's codes, in the macro's
order: the instruction's pattern with `$m` replaced by the code's.
`MOV.LW "$r, $m"` has 11 forms, `$r, $r` to `$r, E($v)`.

Each form knows:
- Its instruction, and its macro code if it has one.
- Its pattern (`$r, ($r + $v)`), and its parts: the parsed pattern, with the
  macro code's parts in place of `$m`.
- Its arguments, which are its placeholders in order: each register and code
  word value, with its bits, where they are in the code word, and its value set
  if it has one, and each `$v` and `$V`, with whether it is relative.
- Its code word with every argument 0, and the mask of the bits that say which
  form it is (the opcode, and the macro code's prefix).
- Its size in words: the code word, one for each `$v`, and two for each `$V`.

### Checking an instruction set

`InstructionSetSyntax::Create` fails with an `InstructionError` if the def
breaks any rule the tools need, which are the rules the wiki's Instruction Set
Source page gives, now checked for any `InstructionSetDef`:
- Every instruction's and macro code's pattern parses, with balanced brackets,
  and none of `;`, `@`, or a `#` outside `$#`.
- Each pattern's placeholders agree with the def: an instruction's code word
  placeholders are its `arg1` and `arg2`, in order, of the same types and sizes;
  it has a `$m` exactly when it has a macro, which exists, and whose size it
  matches; and a macro code's one placeholder, if any, is its `arg`. (The
  instruction assembler derives these from the pattern, so for `.izm` source
  they always agree. A def written by hand could disagree.)
- Every value set a pattern names exists, and every value set follows the value
  set rules: a unique name that isn't a placeholder's, 1 to 8 bits, at least
  one value, unique value names, and values that fit.
- Instruction names, and value names in their set, are unique ignoring case. A
  def written by hand can have lower case names, which the tools take as
  they are.
- A macro's codes can't be written the same way (see above).
- A relative instruction has a form where `$v` is a whole argument.
- Each form's size is the size its compiled microcode decodes to.

### Encoding

```
InstructionForm::Encode(values, address, &words, &error)
```

Takes a value for each argument, in the form's order, and the address of the
statement, and writes the form's words: the code word with each register and
code word value in its bits, then each `$v` and `$V`, low word first for a
dword. A register is its number, and must be one the argument can hold. A code
word value must fit its bits (0 to its largest unsigned value), a word must be
-32768 to 65535, and a dword -2^31 to 2^32 - 1, or it is an error. A relative
`$v` is a target address, and is encoded as its offset from the next
instruction (`address` plus the form's size), wrapping as `IP` does.

### Text

```
InstructionSetSyntax::FindForm(code_word)
InstructionForm::Disassemble(words, address)
```

`FindForm` returns the form a code word is, or null if it is none (a code word
no instruction or macro code uses, which a core runs as `NOP`). `Disassemble`
writes the form's words as a program would write them, with its full name and
its pattern's own spacing: `MOV.LW R1, (SP + 0005h)`. Assembled at the same
address, the text gives the same words.
- Registers are their names.
- A code word value is decimal, or the name with that value if its value set
  has one (the first, if several do). Without a name, it is the number, as a
  named value is still an expression, so `CLRF 5` assembles as `CLRF Z | C`
  does.
- `$v` is four hex digits, and `$V` eight, with the `h` suffix, and a leading
  `0` when the first digit is a letter (`0FFFFh`, five characters).
- A relative `$v` is its target address (`JPR 0120h`), which a program writes
  too.

## Design

### Names

| Name           | Candidates                                | Avoids                                                | Recommendation                                      |
| -------------- | ----------------------------------------- | ----------------------------------------------------- | --------------------------------------------------- |
| Parsed pattern | `SourcePattern`, `ArgPattern`, `Pattern`  | `InstructionDef::source` (the text), microcode `code` | `SourcePattern`, after "source pattern" in the docs |
| Words to text  | `Disassemble`, `ToText`, `Format`         | `InstructionSet::Decode` (to microcode)               | `Disassemble`                                       |

`InstructionSetSyntax` and `InstructionForm` are already decided in
program_assembler.md.

### core: `InstructionError`

`InstructionError::Def` gains `kValueSet`, with `def_name` the set's name.
`InstructionSetSyntax::Create` reports where its errors are with it, so the
instruction assembler maps them to its source with the code it already has for
compile errors. An error in one value of a set is at the set.

### tools: `SourcePattern`

```
// A source pattern, parsed: how an instruction's arguments, or a macro code's,
// are written in a program (InstructionDef::source, MacroCodeDef::source).
class SourcePattern final {
 public:
  // One part of a pattern: literal text, or a placeholder.
  struct Part {
    enum class Kind {
      kLiteral,   // An identifier, number, string, or symbol, written as is.
      kArgument,  // A code word argument: $r, $R, $#, $m, or a value set.
      kWord,      // $v
      kDword,     // $V
    };
    Kind kind = Kind::kLiteral;
    gb::TokenType literal_type = gb::kTokenNone;  // For kLiteral.
    gb::TokenValue literal;                       // For kLiteral.
    Argument arg;                                 // For kArgument.
    const ValueSetDef* value_set = nullptr;       // For a value set.
    bool whole_arg = false;  // It is all of one comma-separated argument.
  };

  // Parses `source`, which may name any of `value_sets`. Returns nullopt and
  // sets `error` if it breaks a pattern rule, or names an unknown placeholder.
  static std::optional<SourcePattern> Parse(
      std::string_view source, absl::Span<const ValueSetDef> value_sets,
      std::string* error);

  absl::Span<const Part> GetParts() const;
  int GetValueWords() const;  // Words its $v and $V add after the code word.
};
```

- In `tools`, beside the instruction assembler, which uses it for both
  instruction and macro code patterns, replacing its two copies of the parse
  and its `AssembleArgument`, `IsValuePlaceholder`, and `GetWholeArgPlaceholder`.
  `InstructionSetSyntax` uses it too, and its forms give their parts to the
  program assembler for matching.
- It tells a placeholder's own name from a value set's with `core`'s
  `IsPlaceholderName`.
- Parsing creates a parser from the shared parser program each time, as the
  assembler does today. That is once per instruction and macro code.

### tools: `InstructionSetSyntax`

```
// One form of an instruction: the instruction with one of its macro's codes,
// or the instruction alone if it has no macro.
class InstructionForm final {
 public:
  // An argument: one of the form's placeholders, in order.
  struct Arg {
    enum class Kind { kWordReg, kDwordReg, kCodeValue, kWord, kDword };
    Kind kind;
    int bits = 0;   // For registers and code values: its bits in the code word.
    int shift = 0;  // For registers and code values: where its bits start.
    const ValueSetDef* value_set = nullptr;  // For a named code value.
    bool relative = false;                   // For a target address $v.
  };

  const InstructionDef& GetInstruction() const;
  const MacroCodeDef* GetMacroCode() const;  // Null without a macro.
  std::string_view GetPattern() const;       // "$r, ($r + $v)"
  absl::Span<const SourcePattern::Part> GetParts() const;  // Without $m.
  absl::Span<const Arg> GetArgs() const;  // By placeholder part, in order.
  uint16_t GetCodeWord() const;           // With every argument 0.
  uint16_t GetCodeWordMask() const;       // The bits that say which form.
  int GetSize() const;                    // In words.

  // Writes the form's words for the statement at `address`, given a value for
  // each of GetArgs(). Returns false, and sets `error`, if a value doesn't fit.
  bool Encode(absl::Span<const int64_t> values, uint16_t address,
              std::vector<uint16_t>& words, std::string* error) const;

  // Returns the instruction at `address` as a program writes it. `words` are
  // its GetSize() words, starting with a code word of this form.
  std::string Disassemble(absl::Span<const uint16_t> words,
                          uint16_t address) const;
};

// Every form of every instruction in an instruction set, for matching
// statements to code words, and code words back to text.
class InstructionSetSyntax final {
 public:
  // Returns null and sets `error` if the tools can't use `def`. Checking it
  // compiles it, unless `instruction_set` gives it already compiled (which is
  // only used for the check, and not kept). `def` must outlive this.
  static std::unique_ptr<InstructionSetSyntax> Create(
      const InstructionSetDef& def,
      const InstructionSet* instruction_set = nullptr,
      InstructionError* error = nullptr);

  // Every form, by instruction in the def's order, then by macro code.
  absl::Span<const InstructionForm> GetForms() const;

  // The forms of a full name, or of every instruction with a root, ignoring
  // case.
  std::vector<const InstructionForm*> GetForms(std::string_view name) const;

  // Returns the form of a code word, or null if it is no form.
  const InstructionForm* FindForm(uint16_t code_word) const;
};
```

- In `tools`, as the assembler and debugger are. It has no `Asm` prefix, as the
  debugger uses it too.
- `Create` checks each form's size by decoding its code word with the compiled
  instruction set. It takes the def, rather than an `AsmInstructionSet`, as the
  default instruction set isn't one, and `instruction_sets` can't link `tools`
  to make one. Without a compiled set, it compiles the def, failing on
  anything `CompileInstructionSet` would. The instruction assembler, which has
  just compiled the def, passes its compiled set so it isn't compiled twice.
- Forms are indexed by upper case full name and root, and by opcode for
  `FindForm`, which checks each of that opcode's forms against its mask.
- Const after `Create`, so it is safe to share between threads.

**Brittleness:**
- An `instruction_set` given to `Create` must be compiled from `def`. A
  mismatch would most likely fail the size check, but isn't guaranteed to.
  Leaving it out avoids this, at the cost of compiling.
- `Encode` takes values in the order of `GetArgs()`, which the caller must
  keep, as the program assembler will when it matches parts to arguments.
- The def must outlive the syntax, as for every other user of an
  `InstructionSetDef`.

### tools: instruction assembler

`InstructionSetAssembler` still derives each instruction's `arg1`, `arg2`, and
macro, and each macro code's `arg`, from its pattern, now through
`SourcePattern`. It turns instruction and value names to upper case as it reads
them. After compiling, it creates an `InstructionSetSyntax` to check the
result, passing the compiled set, and reports its error at the definition,
macro code, or microcode the
`InstructionError` names. It drops `CheckSizes`, the rules in
`AssembleValueSet` (keeping only what it needs to read a set, such as `bits`
fitting in a byte), and its macro code source key check.

Errors stay at the same places, mostly with the same messages, apart from an
error in one value of a set, which is now at the set.

### To confirm

- Whether any of the instruction assembler's tests use lower case instruction
  or value names, or macro codes that are the same under the new rule (CL3,
  CL4).

## CLs

### CL1 [x] core: value set errors

Depends on: nothing.

- `InstructionError::Def::kValueSet`.
- Unused, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).

### CL2 [x] tools: `SourcePattern`

Depends on: nothing.

- `source_pattern.h`/`.cc`: `SourcePattern`.
- The instruction assembler parses instruction and macro code patterns with it.
- No visible change.

**Verify**
- Standard checks.
- Unit tests: every part kind, literal types, whole arguments, value words,
  value sets, and each pattern error (bad syntax, `;`,
  `@`, `#`, unknown placeholder, bad size).
- The instruction assembler's tests pass unchanged.

### CL3 [ ] tools, wiki: case-insensitive names in `.izm`

Depends on: nothing.

- The instruction assembler turns instruction names, extensions, and value
  names to upper case as it reads them.
- Wiki: Instruction Set Source says which names are case-insensitive.

**Verify**
- Standard checks.
- Unit tests: lower and mixed case instruction and value names assemble to
  upper case, and names that differ only in case are already defined.
- `default_instruction_set.inc` is unchanged.

### CL4 [ ] tools, wiki: `InstructionSetSyntax` and its checks

Depends on: CL1, CL2.

- `instruction_set_syntax.h`/`.cc`: `InstructionForm` (without `Encode` and
  `Disassemble`) and `InstructionSetSyntax::Create`, `GetForms`, and
  `FindForm`, with every check in Checking an instruction set.
- The instruction assembler creates one after compiling, passing its compiled
  set, and maps its errors,
  dropping `CheckSizes`, the value set rules, and its macro code source check.
- Wiki: Instruction Set Source's rule for macro code sources becomes the rule
  that a macro's codes can't be written the same way.

**Verify**
- Standard checks.
- Unit tests: forms for instructions with and without macros, and a macro as
  either argument (patterns, parts, args, code words, masks, sizes); names
  ignoring case; `FindForm` for every form and for unused code words; each
  check, on defs written in the test, including defs whose patterns disagree
  with their args, and lower case names; `Create` with and without a compiled
  set.
- The instruction assembler's tests for the moved checks still pass, at the
  same locations (apart from errors in one value of a set).

### CL5 [ ] tools: encoding and disassembly

Depends on: CL4.

- `InstructionForm::Encode` and `InstructionForm::Disassemble`.

**Verify**
- Standard checks.
- Unit tests: encoding every argument kind, relative values (forward,
  backward, and wrapping), and values that don't fit; disassembling registers,
  code values, named values (including a value with no name, and one with
  several), `$v` and `$V` with and without a leading `0`, and relative
  targets; and that disassembling the words of an encoded form gives back its
  values.
