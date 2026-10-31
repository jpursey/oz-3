# Flag Value Sets

Some value sets name bits that a program combines, such as `Flags` and `Banks`
in the default instruction set (`CLRF Z | C`). `InstructionForm::Disassemble`
writes a code word value as the first name with exactly that value, or else as
a number, so `CLRF Z | C` disassembles as `CLRF 5`. That assembles the same, but
reads worse, and the debugger (planned) will show it.

A value set can now be marked as flags. `Disassemble` writes a value of a flag
set as its names joined by `|` (`CLRF Z | C`), and the default instruction set
marks `Flags` and `Banks`. Programs are unchanged: they already write
expressions, and the mark only changes how a value is written back as text.

## Behavior

### Instruction set source

A value set's options take `flags` after its bits:

```
values(bits:4, flags) Flags { Z = 1, S = 2, C = 4, O = 8 }
```

It has no rules of its own: a flag set's values can be anything a value set's
can, including 0 and values with several bits (`ALL = 15`).

### Disassembly

A value of a flag set is written as the fewest names it can greedily, joined
by `" | "`:
1. Of the names whose value is not 0 and whose bits are all still left in the
   value, take the one with the most bits set, or the first in the set's order
   if several have that many. Clear its bits from what is left, and repeat
   until no name fits.
2. Write the names in the order they were taken, so names covering more bits
   come first, and single bits come in the set's order. Then any bits no name
   took, as one decimal number (`Z | 4`).
3. A value of 0 is the first name with value 0, if there is one, or `0`. A
   value no name fits at all is the number alone (`CLRF 12` if the set named
   only `Z` and `S`).

So with `Z = 1, S = 2, C = 4, ZS = 3`, 3 is `ZS`, and 7 is `ZS | C`, wherever
`ZS` is in the set. An exact name is always taken whole, as it has every bit of
the value. Greedy isn't always the fewest names (with `A = 7, B = 12, C = 3`,
15 is `A | 8` rather than `B | C`), but it is simple to predict from the set,
and the set's order only breaks ties.

Assembled at the same address, the text gives the same words, as each name is
its value in the expression, and the terms share no bits, so `|` is their sum.

A value set without the mark is written as before.

### Wiki

- *Instruction Set Source* (`2.3-Instruction-Set-Source.md`), *Named values*:
  the `flags` option in the syntax, a **Flags** bullet saying what it marks and
  that tools write such a value as its names joined by `|`, and the example
  updated to match the default instruction set's source.

The default instruction set's page already says programs combine `Flags` with
`|`. Nothing there changes.

## Design

### Names

| Name              | Candidates                       | Avoids                                                                                            | Choice  |
| ----------------- | -------------------------------- | ------------------------------------------------------------------------------------------------- | ------- |
| The mark          | `flags`, `combined`, `bit_mask`  | `mask` (a microcode register's `:mask`, a form's code word mask), `bits` (the set's size)         | `flags` |

`flags` is the backlog's word, and what C# calls the same idea (`[Flags]`). It
is close to the CPU's status flags, and the default instruction set's `Flags`
set (`values(bits:4, flags) Flags`), but it is only ever a set's option or a
`ValueSetDef` field, so the two don't meet in code. The field and the `.izm`
keyword are both `flags`.

### core

```
struct ValueSetDef {
  ...
  // Whether the values name bits that a program combines with "|", such as
  // status flags, so the tools write a value as its names joined by "|".
  bool flags = false;
};
```

`CompileInstructionSet` ignores it, as it does the rest of a value set.

### tools

- `InstructionForm::Disassemble` writes a code word value of a flag set as
  above, in a private helper beside `GetRegisterName`.
- The instruction assembler's grammar takes an optional `flags` after a value
  set's bits: `"values" "(" "bits" ":" $bits=%int ["," $flags="flags"] ")"`.
  Bits stay first and required, as every set needs them, so the options don't
  become a list in any order like an instruction's.
- `InstructionDefExporter` writes `.flags = true` only when set, as it does
  `.relative`.
- `InstructionSetSyntax` checks nothing new.

### instruction_sets

`Flags` and `Banks` are marked as flags. `Condition` isn't: its values are
choices, not bits.

The pin test fingerprint (`default_instruction_set_test.cc`) covers each value
set's size and named values, which programs write. It leaves out the mark, as
the mark doesn't change a code word or what a program writes, so marking a set
(or unmarking it) is free, as microcode fixes are. `kPinnedFingerprint` stays
as it is.

**Brittleness:** none new. The mark is one field that only `Disassemble`
reads, and the round trip holds for any values.

## CLs

### CL1 [x] core: `ValueSetDef::flags`

Depends on: nothing.

- `instruction_def.h`: `ValueSetDef::flags`, and the set's comment says that
  the tools use value sets to write values too, not only to read them.
- Unused, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).

### CL2 [x] tools: flag sets in `.izm`, the exporter, and disassembly

Depends on: CL1.

- `instruction_set_syntax.cc`: `Disassemble` writes flag set values as names
  joined by `|`.
- `instruction_assembler.cc`: the `flags` option.
- `instruction_def_exporter.cc`: `.flags = true`.
- Wiki: *Instruction Set Source*, *Named values*, apart from the example.

**Verify**
- Standard checks.
- Unit tests (`instruction_set_syntax_test.cc`): a flag set's value written as
  an exact name (including 0 and a several-bit name defined after its single
  bits), as several names with single bits in the set's order, with the
  most-bits name taken first wherever it is in the set (`ZS | C` above), with
  ties going to the set's order, with leftover bits as a number, and as a
  number alone. A set without the mark still writes `5`.
- Unit tests (`instruction_assembler_test.cc`): `flags` sets the field, it is
  false without it, and `values(flags, bits:4)` is an error.
- Unit tests (`instruction_def_exporter_test.cc`): `.flags = true` is written
  for a flag set, and nothing for one without the mark.

### CL3 [ ] instruction_sets: mark `Flags` and `Banks`

Depends on: CL2.

- `default_instruction_set.izm`: `values(bits:4, flags)` for `Flags` and
  `Banks`, and the `.inc` regenerated.
- Wiki: *Instruction Set Source*'s example of `Flags` shows the option.

**Verify**
- Standard checks, including the `.inc` diff showing only the two `.flags =
  true` lines, and the pin test passing with `kPinnedFingerprint` unchanged.
- Unit tests (`program_assembler_test.cc`, which disassembles with the default
  instruction set): `RST SELF, 2` and `RST.C 5, 3` now disassemble as
  `RST SELF, STACK` and `RST.C 5, CODE | STACK`, and new cases cover `CLRF Z |
  C` and a value with no name (`CLRF 0`). Each disassembled text assembles back
  to the same words.
