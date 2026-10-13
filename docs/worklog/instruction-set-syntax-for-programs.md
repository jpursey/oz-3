# Instruction set syntax for programs

The program assembler matches a program's statements against the source
patterns of an instruction set, and needs the instruction set to declare
everything about its syntax that a program can't otherwise know (see
[program_assembler.md](../program_assembler.md), What the instruction set
declares). This adds three things to instruction set source, in
`InstructionSetDef`, the `.izm` syntax, and the exporter:
- **Named values:** names for the values of a code word argument, which a
  pattern uses as a placeholder in place of `$#` (`"$Condition, $m"`), so a
  program writes `JC Z, @done` rather than `JC 4, @done`.
- **Relative instructions:** an instruction marked relative, where a `$v` that
  is a whole argument is written in a program as a target address, which the
  assembler encodes as its offset from the next instruction.
- **Pattern rules:** patterns no longer allow `;` or `@`, which programs use
  for comments and local labels.

The default instruction set then uses them, with none of its code words
changed. Nothing reads the new declarations until *Instruction forms* builds
`InstructionSetSyntax`, so there is no change in what a core runs.

## Behavior

### Instruction set source (`.izm`)

A new top level definition, in any order with macros and instructions:

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
error.

An instruction marks itself relative in its config, alongside `opcode`:

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

Patterns can't contain `;` or `@`. These, and `#` outside of `$#`, were in the
pattern lexer's symbols, but the pattern grammar already rejected them, though
the wiki listed them as allowed. Now the lexer drops them too, and the wiki
says so.

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

`CompileInstructionSet` checks every value set as the source rules above say,
reporting `InstructionError::Def::kValueSet` with the set's name. It doesn't
parse patterns, so it doesn't check that a pattern's `$<name>` names a set, or
that a relative instruction has a `$v` that is a whole argument: the
instruction assembler checks
both, and `InstructionSetSyntax` will for any `InstructionSetDef`.

The changes add fields with defaults, so existing `InstructionSetDef`s,
written by hand or exported, still compile unchanged.

### Exporter

`oz3ism` writes each set's values as `k<prefix>Values_<Name>`, the sets as
`k<prefix>ValueSets`, `.relative = true` on relative instructions, and
`.value_sets` in the `InstructionSetDef`.

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
  It goes beyond the backlog item, which named only conditions and flag masks,
  as a pinned instruction's syntax can't change later.
- `JPR`, `JCR`, `JDR`, and `CALLR` are relative.

No code word changes, which `git diff` of the `.inc` shows. The pin test's
layout gains each instruction's relative mark, and the value sets its source
and its macro's codes use, so the fingerprint changes once, safely, as no
program has been assembled yet. Only the sets a pinned instruction uses are in
its layout, so a new set for a new instruction leaves the fingerprint alone.

### Wiki

- Instruction Set Source (`2.3-Instruction-Set-Source.md`): named values,
  relative instructions, and the symbols a pattern may use.
- Default Instruction Set (`2.2-Default-Instruction-Set.md`): conditions, flag
  masks, and `RST`'s bank mask are the `Condition`, `Flags`, and `Banks` named
  values, and a relative
  instruction's integer is written as its target address.

## Design

### Names

| Name                  | Candidates                                     | Avoids                                                          | Choice           |
| --------------------- | ---------------------------------------------- | --------------------------------------------------------------- | ---------------- |
| Source keyword        | `values`, `enum`, `names`                      | `$v`/`$V`, which are values in words; C++'s `enum`              | `values` (from the design doc) |
| A set of names        | `ValueSetDef`                                  | `MacroDef`, `InstructionDef`                                    | `ValueSetDef` (from the design doc) |
| One name and value    | `NamedValueDef`, `ValueNameDef`, `ValueDef`    | `ValueDef` reads as a `$v` value                                | `NamedValueDef`  |
| Relative mark         | `relative`, `ip_relative`, `relative_value`    | Microcode's relative jump labels                                | `relative` (from the design doc) |

### core

`NamedValueDef`, `ValueSetDef`, `InstructionDef::relative`, and
`InstructionSetDef::value_sets` in `instruction_def.h`, with the pattern
comments on `InstructionDef::source` and `MacroCodeDef::source` gaining named
value placeholders. `IsPlaceholderName` (also in `instruction_def.h`) is the
one list of the built-in placeholder names, from `Argument::IsValid`'s sizes,
which the value set checks and the instruction assembler share.

`CompileInstructionSet` checks the value sets first, in a free function rather
than in `InstructionCompiler`, as they have no microcode and nothing compiled
depends on them.

### tools

`InstructionSetAssembler`:
- Assembles value sets before macros and instructions, so they may be written
  in any order. It checks what it must before building the set, with the
  token's location: a duplicate set name (for its own lookup), `bits`, and
  that each value fits them (before narrowing it to `uint8_t`).
  `CompileInstructionSet` checks the rest, and the assembler maps its
  `kValueSet` error to the set's location, as it does for macros and
  instructions.
- The pattern lexer's placeholder token becomes `\$(#[1-8]|[a-zA-Z_]\w*)`, and
  `AssembleArgument` classifies the name: a built-in placeholder
  (`IsPlaceholderName`), or else a value set, or else an error. This also
  turns today's silent splits, such as `$r5` lexing as `$r` then `5`, into
  errors.
- The pattern grammar names each argument's terms (`Arg { $terms=Term+; }`),
  so a placeholder that is a whole argument is found from the parse, rather
  than by splitting text, which a string in a pattern could break.
- A relative instruction's whole argument `$v` is checked once its macro is
  known, after its microcode is assembled: a `$v` that is a whole argument of
  its source, or a `$m` that is a whole argument, whose macro has a code where
  `$v` is a whole argument.
- A macro code's source allowed one code word placeholder per argument, rather
  than per source, so `"$r, $#3"` was accepted with the second overwriting the
  first. Its check now covers the whole source.

`InstructionDefExporter` writes the new fields, and gains a test
(`instruction_def_exporter_test.cc`) that exports to a memory file system.

**Brittleness:** a pattern refers to its value set by name, inside a string, so
a hand-written `InstructionSetDef` can name a set that doesn't exist, and
`CompileInstructionSet` won't notice. The instruction assembler catches it for
`.izm` source, and `InstructionSetSyntax` will for any def, before a program
can use it. Keeping the link in the pattern, rather than in `Argument`, keeps
the pattern the one place that says how an instruction is written.

### To confirm

- That Game Bits' parser can capture a keyword on its own in an alternative
  (`$relative="relative"`) within the comma-separated config list. CL2:
  confirmed, in either order with `opcode`.
- That the `.izm` lexer can add `=` as a symbol without changing how existing
  source lexes. CL2: confirmed, as the default instruction set's `.inc`
  regenerates unchanged.

## CLs

### CL1 [x] core: Named values and relative instructions in InstructionSetDef

Depends on: nothing.

- `NamedValueDef`, `ValueSetDef`, `InstructionDef::relative`, and
  `InstructionSetDef::value_sets` (`instruction_def.h`), with the pattern
  comments updated, and `IsPlaceholderName`.
- `CompileInstructionSet` checks value sets, reporting
  `InstructionError::Def::kValueSet` (`instruction_compiler.h/.cc`).
- Unused by anything else, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests (`instruction_def_test.cc`, new): `IsPlaceholderName` for every
  placeholder name, and for names just outside them.
- Unit tests (`instruction_compiler_test.cc`): valid sets compile, and each
  rule fails with a `kValueSet` error naming the set: a missing or
  non-identifier name, a placeholder's name, a duplicate set name, a size of 0
  or 9, no values, a non-identifier or duplicate value name, and a value that
  doesn't fit. Aliased values compile.

### CL2 [x] tools: Value sets, relative instructions, and pattern rules in .izm

Depends on: CL1.

- `InstructionSetAssembler`: `values(bits:N) Name { ... }`,
  `instruction(relative)`, `$<name>` placeholders in instruction and macro
  code patterns, the relative check, and no `;` or `@` in patterns.
- `InstructionDefExporter` writes value sets and `relative`.
- Wiki: Instruction Set Source.
- [program_assembler.md](../program_assembler.md): the note that patterns may
  still contain `;` and `@` becomes that they can't.

**Verify**
- Standard checks.
- Unit tests (`instruction_assembler_test.cc`): value sets in any order with
  their uses, a `$<name>` encoding as `$#` of the set's size in an instruction
  and in a macro code, an unknown `$<name>`, each value set rule with its
  location, `relative` alone and with `opcode`, a relative instruction with a
  whole argument `$v` directly and through `$m`, and one without (`"D($v)"`)
  failing,
  and `;` and `@` in patterns failing.
- Unit tests (`instruction_def_exporter_test.cc`, new): the exported text for a
  small def with a value set and a relative instruction, and one without
  either, which writes no `value_sets`.
- The default instruction set's `.inc` regenerates unchanged.

### CL3 [x] instruction_sets: The default instruction set's named values and relative instructions

Depends on: CL2.

- `default_instruction_set.izm`: `Condition`, `Flags`, and `Banks`, used by
  the instructions above, and `JPR`, `JCR`, `JDR`, and `CALLR` marked relative.
  Regenerate the `.inc`.
- `default_instruction_set_test.cc`: the layout gains the relative mark and
  the value sets each pinned instruction uses, and the new fingerprint.
- Wiki: Default Instruction Set.
- Backlog: once `InstructionSetSyntax` exists, the pin test could fingerprint
  its resolved forms (through a test-only dependency on tools) instead of
  scanning patterns for value sets itself.

**Verify**
- Standard checks, including the `.inc` check: `git diff` shows only patterns,
  `.relative`, and the value sets, with no opcode or code word changed.
- The pin test passes with the new fingerprint, and fails if a value's name or
  number, a set's size, or a relative mark changes (checked by hand, not
  checked in).
