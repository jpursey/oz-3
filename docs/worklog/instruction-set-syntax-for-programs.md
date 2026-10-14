# Instruction set syntax for programs

The program assembler matches a program's statements against the source
patterns of an instruction set, and needs the instruction set to declare
everything about its syntax that a program can't otherwise know (see
[assembler_design.md](../assembler_design.md), What the instruction set
declares). This added three things to instruction set source, in
`InstructionSetDef`, the `.izm` syntax, and the exporter:
- **Named values:** names for the values of a code word argument, which a
  pattern uses as a placeholder in place of `$#` (`"$Condition, $m"`), so a
  program writes `JC Z, @done` rather than `JC 4, @done`.
- **Relative instructions:** an instruction marked relative, where a `$v` that
  is a whole argument is written in a program as a target address, which the
  assembler encodes as its offset from the next instruction.
- **Pattern rules:** patterns don't allow `;` or `@`, which programs use for
  comments and local labels.

The default instruction set uses all three, with none of its code words
changed. Nothing reads the new declarations until *Instruction forms* builds
`InstructionSetSyntax`, so there was no change in what a core runs. The wiki's
Instruction Set Source and Default Instruction Set pages describe them.

## Behavior

### Instruction set source (`.izm`)

A value set is a top level definition, in any order with macros and
instructions:

    values(bits:3) Condition { NZ = 0, NS = 1, NC = 2, NO = 3,
                               Z = 4, S = 5, C = 6, O = 7 }

- `bits` is required, from 1 to 8. It is the argument's size, which isn't
  derived from the names as a macro's is, since names needn't cover every
  value (a flag mask names single flags, and a program combines them).
- The set's name and each value's name are C identifiers. Set names are unique
  in the instruction set, and value names are unique in their set. Two names
  may have the same value. A set has at least one value, and each value fits
  its bits.
- A set's name can't be a placeholder's (`r`, `r1` to `r4`, `R`, `R1`, `R2`,
  `m`, `m1` to `m8`, `v`, or `V`).

A pattern (an instruction's source, or a macro code's) uses a set as
`$<name>`, wherever it could use `$#`. It is an immediate argument of the
set's size, so `"$Condition, $m"` encodes exactly as `"$#3, $m"`. A size can't
be given (`$Condition3` is a set named `Condition3`). An unknown name is an
error, as is a built-in placeholder with an invalid size (`$r5`), which used to
lex silently as `$r` then `5`.

An instruction marks itself relative in its config, alongside `opcode`, in
either order:

    instruction(relative) JPR "$m" { ... }
    instruction(opcode:5, relative) JPR "$m" { ... }

A placeholder is a **whole argument** when it is all of one of the pattern's
comma-separated arguments, with nothing else in it: the `$v` in `"$r, $v"` is,
and the ones in `"D($v)"` and `"$v + 1"` aren't. A relative instruction must
have a form where `$v` is a whole argument: either in its own source, or in a
macro code whose source is just `"$v"`, taking the place of a `$m` that is a
whole argument. Otherwise it is an error, as the mark would mean nothing. Every
`$v` that is a whole argument of a relative instruction is relative. `$V` never
is.

Patterns can't contain `;`, `@`, or `#` outside of `$#`. The pattern grammar
already rejected them, though the wiki listed them as allowed; now the pattern
lexer drops them too, and the wiki says so.

A macro code's source has at most one code word argument in all, where it used
to be one per comma-separated argument, so `"$r, $#3"` was accepted with the
second overwriting the first.

### `InstructionSetDef`

```
// A name for one value of a code word argument.
struct NamedValueDef {
  std::string_view name;
  uint8_t value = 0;
};

// Names for the values of a code word argument. A source pattern uses them as
// a placeholder ("$Condition") in place of "$#".
struct ValueSetDef {
  std::string_view name;
  uint8_t size = 0;  // In bits, 1 to 8.
  absl::Span<const NamedValueDef> values;
};

struct InstructionDef {
  ...
  std::string_view source;
  // If true, a "$v" that is a whole argument in any form of this instruction is
  // written as a target address, and encoded as its offset from the next
  // instruction.
  bool relative = false;
  ...
};

struct InstructionSetDef {
  absl::Span<const InstructionDef> instructions;
  absl::Span<const MacroDef> macros;
  absl::Span<const ValueSetDef> value_sets;
};
```

`CompileInstructionSet` checks only what cores need to run the instructions,
so it ignores value sets, patterns, and relative marks: an `InstructionSetDef`
that compiles may not be one the assembler can use. The instruction assembler
checks the source rules above for `.izm` source, including that a pattern's
`$<name>` names a set, and that a relative instruction has a `$v` that is a
whole argument. `InstructionSetSyntax` will check them for any
`InstructionSetDef`.

The new fields have defaults, so existing `InstructionSetDef`s, written by
hand or exported, compile unchanged.

### Exporter

`oz3ism` writes each set's values as `k<prefix>Values_<Name>`, the sets as
`k<prefix>ValueSets`, `.relative = true` on relative instructions, and
`.value_sets` in the `InstructionSetDef`. A def without value sets exports
none of these.

### Default instruction set

```
values(bits:3) Condition { NZ = 0, NS = 1, NC = 2, NO = 3,
                           Z = 4, S = 5, C = 6, O = 7 }
values(bits:4) Flags { Z = 1, S = 2, C = 4, O = 8 }
values(bits:4) Banks { CODE = 1, STACK = 2, DATA = 4, EXTRA = 8 }
```

- `$Condition` replaces `$#3` in `JC`, `JCR`, `RETC`, `RETC.A`, `IRTC`, and
  `IRTC.A`.
- `$Flags` replaces `$#4` in `CLRF`, `SETF`, and `NOTF`.
- `$Banks` replaces `$#4` in `RST`, so a program writes `RST 2, CODE | STACK`.
  This went beyond the backlog item, which named only conditions and flag
  masks, as a pinned instruction's syntax can't change later.
- `JPR`, `JCR`, `JDR`, and `CALLR` are relative.

No code word changed. The pin test's fingerprint changed, safely, as no
program had been assembled yet. It changed once more when the size check found
that `MOV.SDV`'s `StoreDwordValue` patterns wrote its dword value as `$v`,
which became `$V`, before any program could depend on the wrong size.

## Design

### Names

| Name                  | Candidates                                     | Avoids                                                          | Choice           |
| --------------------- | ---------------------------------------------- | --------------------------------------------------------------- | ---------------- |
| Source keyword        | `values`, `enum`, `names`                      | `$v`/`$V`, which are values in words; C++'s `enum`              | `values` (from the design doc) |
| A set of names        | `ValueSetDef`                                  | `MacroDef`, `InstructionDef`                                    | `ValueSetDef` (from the design doc) |
| One name and value    | `NamedValueDef`, `ValueNameDef`, `ValueDef`    | `ValueDef` reads as a `$v` value                                | `NamedValueDef`  |
| Relative mark         | `relative`, `ip_relative`, `relative_value`    | Microcode's relative jump labels                                | `relative` (from the design doc) |
| An argument that is one placeholder | "bare", "whole argument"         | "Bare" doesn't say what it is bare of                           | "whole argument" |

### core

`NamedValueDef`, `ValueSetDef`, `InstructionDef::relative`, and
`InstructionSetDef::value_sets` are in `instruction_def.h`, and the pattern
comments on `InstructionDef::source` and `MacroCodeDef::source` include named
value placeholders. `IsPlaceholderName` (also in `instruction_def.h`) is the
one list of the built-in placeholder names, derived from `Argument::IsValid`'s
sizes, which the instruction assembler and the pin test share. It stays in
core, beside the patterns it describes, though nothing in core checks them.

### tools

`InstructionSetAssembler`:
- Assembles value sets before macros and instructions, so they may be written
  in any order. It checks every value set rule, with the token's location: a
  duplicate set name, a set named as a built-in placeholder, `bits`, a set
  without values, a duplicate value name, and that each value fits (before
  narrowing it to `uint8_t`). The grammar makes the names identifiers.
- After compiling the set, checks each form's size (see Arguments in the
  wiki's Instruction Set Source page): the code word and the words of its
  pattern's `$v` and `$V`, counted as the patterns are parsed, must be the
  size the compiled form decodes to.
- The pattern lexer's placeholder token is `\$(#[1-8]|[a-zA-Z_]\w*)`, and
  `AssembleArgument` classifies the name: a built-in placeholder
  (`IsPlaceholderName`), or else a value set, or else an error.
- The pattern grammar names each argument's terms (`Arg { $terms=Term+; }`),
  so a placeholder that is a whole argument is found from the parse
  (`GetWholeArgPlaceholder`), rather than by splitting text, which a string in
  a pattern could break.
- A relative instruction's whole argument `$v` is checked once its macro is
  known, after its microcode is assembled: a `$v` that is a whole argument of
  its source, or a `$m` that is a whole argument, whose macro has a code where
  `$v` is a whole argument.

`InstructionDefExporter` writes the new fields, and has a test
(`instruction_def_exporter_test.cc`) that exports to a memory file system and
checks the exact text.

### Pin test

`default_instruction_set_test.cc`'s layout of each pinned instruction includes
its relative mark, and the size and values of each value set its source and
its macro's codes use. Only the sets an instruction uses are in its layout, so
a new set for a new instruction leaves the fingerprint alone. The test finds
the sets by scanning each pattern for `$<name>`, and fails if a name is neither
a built-in placeholder nor a set, so a set's values can't silently drop out of
the fingerprint.

### Brittleness

A pattern refers to its value set by name, inside a string, so a hand-written
`InstructionSetDef` can name a set that doesn't exist, and
`CompileInstructionSet` won't notice. The instruction assembler catches it for
`.izm` source, and `InstructionSetSyntax` will for any def, before a program
can use it. Keeping the link in the pattern, rather than in `Argument`, keeps
the pattern the one place that says how an instruction is written.
