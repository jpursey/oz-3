# Instruction forms

The program assembler and the debugger both need an instruction set's syntax as
a table: every way each instruction can be written, what code word each one
is, and how many words it takes (see
[assembler_design.md](../assembler_design.md), Instructions). This added that
table to `tools` as `InstructionSetSyntax`, built from any `InstructionSetDef`.
Each of its forms encodes argument values into words, and turns its words back
into text. Matching a statement to a form is left to *Program assembler*, which
owns the statement and expression grammar, and gets each name's forms with
their parsed patterns.

Building it is also what checks that the tools can use an `InstructionSetDef`,
whether it was assembled from `.izm` source or written by hand.
`CompileInstructionSet` only checks what cores need, so the rules for patterns,
value sets, and sizes that the instruction assembler checked moved into
`InstructionSetSyntax`, and the instruction assembler builds one to check its
result. The two places that parsed patterns now share one pattern parser,
`SourcePattern`.

Programs are case-insensitive, so the names they write from an instruction set
became case-insensitive in `.izm` source too.

Nothing assembles programs yet, so the only visible changes are to `.izm`
source (see Behavior), and the wiki's Instruction Set Source page describes
them. It went beyond the backlog item's **Layers** (tools only) by adding a
value set location to `InstructionError` in `core`.

The backlog item suggested the default instruction set's pin test fingerprint
forms instead of scanning patterns for value sets itself. It stayed as it was:
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
- **A duplicate value name, or a value that doesn't fit its set's bits, is an
  error at the set**, rather than at the value.

The default instruction set already followed both rules, and its def didn't
change.

### Forms

An instruction without a macro argument has one form, its source pattern. One
with a macro argument has a form for each of its macro's codes, in the macro's
order: the instruction's pattern with `$m` replaced by the code's.
`MOV.LW "$r, $m"` has 11 forms, `$r, $r` to `$r, E($v)`.

Each form knows:
- Its instruction, and its macro code if it has one.
- Its pattern (`$r, ($r + $v)`), and its parts: the parsed pattern, with the
  macro code's parts in place of `$m`, and offsets into the form's pattern. A
  macro code's placeholder is a whole argument only if the `$m` was.
- Its arguments, which are its placeholders in order: each register and code
  word value, with its bits, where they start in the code word, and its value
  set if it has one, and each `$v` and `$V`, with whether it is relative (a
  `$v` that is a whole argument of a relative instruction).
- Its code word with every argument 0, and the mask of the bits that say which
  form it is (the opcode, and the macro code's prefix).
- Its size in words: the code word, one for each `$v`, and two for each `$V`.

`InstructionSetSyntax` gives every form in the def's order, the forms of a full
name (`ADD.W`) or of a root (`ADD`, for every `ADD` instruction), ignoring
case, and the form of a code word (`FindForm`), or null for a code word no
form uses (which a core runs as `NOP`).

### Checking an instruction set

`InstructionSetSyntax::Create` fails with an `InstructionError` if the def
breaks any rule the tools need, which are the rules the wiki's Instruction Set
Source page gives, checked for any `InstructionSetDef`:
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

`InstructionForm::Encode(values, address, words, error)` takes a value for each
argument, in the form's order, and the address of the statement, and appends
the form's words: the code word with each register and code word value in its
bits, then each `$v` and `$V`, low word first for a dword. A register is its
number (`R1` and `D1` are both 1). A register or code word value must fit its
bits, a word must be -32768 to 65535, and a dword -2^31 to 2^32 - 1, and the
number of values must match, or it returns false, sets the error ("Register 8
is not R0 to R7", "Value 32 does not fit in 5 bits"), and appends nothing. A
relative `$v` is a target address, and is encoded as its offset from the next
instruction (`address` plus the form's size), wrapping as `IP` does.

### Text

`InstructionForm::Disassemble(words, address)` writes the form's `GetSize()`
words as a program would write them, with its full name and its pattern's own
spacing: `MOV.LW R1, (SP + 0005h)`. Assembled at the same address, the text
gives the same words.
- Registers are their names, as `InstructionSet::Decode` decodes them.
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

| Name           | Candidates                                    | Avoids                                                     | Choice                                                                   |
| -------------- | --------------------------------------------- | ---------------------------------------------------------- | ------------------------------------------------------------------------ |
| Table of forms | `InstructionSetSyntax`, `InstructionSetForms` | `InstructionSet` (compiled microcode), `InstructionSetDef` | `InstructionSetSyntax`, holding `InstructionForm`s (from the design doc) |
| Parsed pattern | `SourcePattern`, `ArgPattern`, `Pattern`      | `InstructionDef::source` (the text), microcode `code`      | `SourcePattern`, after "source pattern" in the docs                      |
| Words to text  | `Disassemble`, `ToText`, `Format`             | `InstructionSet::Decode` (to microcode)                    | `Disassemble`                                                            |

### core

`InstructionError::Def` has `kValueSet`, with `def_name` the set's name, so
`InstructionSetSyntax` can say where its errors are, and the instruction
assembler maps them back to its source with the code it has for compile
errors. `CompileInstructionSet` never returns it.

### tools: `SourcePattern`

`source_pattern.h` parses a source pattern (`InstructionDef::source` or
`MacroCodeDef::source`) into its parts, with the gb parser to check its syntax,
and then lexes it again for the parts:
- `ParseInstruction` and `ParseMacroCode` own the rules for how many code word
  arguments each kind of pattern has (at most two, one of them `$m`, for an
  instruction, and one, not `$m`, for a macro code), so the instruction
  assembler (deriving the def's arguments) and `InstructionSetSyntax` (checking
  them) share them.
- Each `Part` is a literal (with its token type and value), a code word
  argument (`$r`, `$R`, `$#`, `$m`, or a value set, with its `Argument` and
  `ValueSetDef`), `$v`, or `$V`, with its offset and size in the source, and
  whether it is a whole argument. `GetArgs()` gives the code word arguments in
  order, and `GetValueWords()` the words its `$v` and `$V` add.
- A placeholder name is one of `core`'s (`IsPlaceholderName`), or else a value
  set's, or else an error.
- Parsing creates a parser from the shared parser program each time, which is
  once per instruction and macro code.

The gb lexer returns a re-read user token's whole text rather than its capture,
so the placeholder regex captures the whole token, and the `$` is dropped from
it. Game Bits' backlog has the fix (*Re-read user tokens keep their capture*).

### tools: `InstructionSetSyntax`

`instruction_set_syntax.h` holds `InstructionForm` and `InstructionSetSyntax`.
`Create` builds it with an `InstructionSetSyntaxBuilder` (in the `.cc`), which:
1. Compiles the def, unless given its compiled set (the instruction assembler,
   which has just compiled it, passes it).
2. Checks the value sets and instruction names.
3. Parses each macro's code patterns, checking each against its `arg`, and
   that no two are written the same way.
4. Parses each instruction's pattern, checks it against the def, and adds a
   form for each macro code. A form's pattern splices the code's source in
   place of `$m`, shifting the offsets of the parts after it. Its code word
   comes from `InstructionDef::Encode` and `MacroDef::Encode`, so the layout
   rule stays in `core`.
5. Checks each form's size by decoding its code word with the compiled set,
   and that a relative instruction has a relative `$v`.
6. Indexes the forms by upper case full name and root, and by opcode for
   `FindForm`, which checks each of that opcode's forms against its mask.

It is const after `Create`, so it is safe to share between threads. The def
must outlive it.

The building blocks the program assembler and debugger use are a form's parts
and arguments (for matching a statement, and filling in its values), `Encode`,
`Disassemble`, `GetForms(name)`, and `FindForm`.

### tools: instruction assembler

`InstructionSetAssembler` derives each instruction's `arg1`, `arg2`, and macro,
and each macro code's `arg`, from its pattern with `SourcePattern`, and turns
instruction and value names to upper case as it reads them. After compiling,
it creates an `InstructionSetSyntax` to check the result, passing the compiled
set, and reports its error at the value set, instruction, macro code, or
microcode the `InstructionError` names.

It keeps what it needs before compiling: valid bits for a value set, as a
pattern that uses it takes them as its argument's size; values that fit in a
byte; and instruction names and macro code sources that are exactly unique,
as errors are mapped back to the source by them.

### Brittleness

- An `instruction_set` given to `Create` must be compiled from the def. A
  mismatch would most likely fail the size check, but isn't guaranteed to.
  Leaving it out avoids this, at the cost of compiling.
- `Encode` takes values in the order of `GetArgs()`, which the program
  assembler keeps by matching parts to arguments in order.
- `Disassemble` needs the form's `GetSize()` words, which is only checked in
  debug builds.
