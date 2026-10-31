# Flag Value Sets

Some value sets name bits that a program combines, such as `Flags` and `Banks`
in the default instruction set (`CLRF Z | C`). `InstructionForm::Disassemble`
used to write a code word value only as the first name with exactly that
value, or else as a number, so `CLRF Z | C` disassembled as `CLRF 5`.

A value set can now be marked as flags. `Disassemble` writes a value of a flag
set as names joined by `|` (`CLRF Z | C`), and the default instruction set
marks `Flags` and `Banks`. Programs are unchanged: they already write
expressions, and the mark only changes how a value is written back as text.

## Behavior

### Instruction set source

A value set's options take `flags` after its bits:

```
values(bits:4, flags) Flags { Z = 1, S = 2, C = 4, O = 8 }
```

`bits` stays first and required, so `values(flags, bits:4)` is an error. A
flag set has no rules of its own: its values can be anything a value set's
can, including 0 and values with several bits (`ALL = 15`).

### Disassembly

A value of 0, or any value of a set without the mark, is written as before: the
first name with that value, or else the number.

A nonzero value of a flag set is written greedily, joined by `" | "`:
1. Of the names whose value is not 0 and whose bits are all still left in the
   value, take the one with the most set bits, or the first in the set's order
   if several have that many. Clear its bits from what is left, and repeat
   until no name fits.
2. Write the names in the order they were taken, then any bits no name took,
   as one decimal number (`Z | 4`), or the number alone if no name took any.

So with `Z = 1, S = 2, C = 4, ZS = 3`, 3 is `ZS`, 5 is `Z | C`, 7 is `ZS | C`
wherever `ZS` is in the set, and 13 is `Z | C | 8`. An exact name is always
taken whole, as it has every bit of the value. Greedy isn't always the fewest
names (with `A = 7, B = 12, C = 3`, 15 is `A | 8` rather than `B | C`), but such
sets aren't expected in practice, and the result is simple to predict from the
set.

Assembled at the same address, the text gives the same words, as each name is
its value in the expression, and the terms share no bits.

### Default instruction set

`Flags` and `Banks` are marked as flags; `Condition` isn't, as its values are
choices, not bits. So `CLRF Z | C` and `RST 0, CODE | DATA` disassemble as
written, with each set's names in its order (`NOTF O | S | Z | C` is
`NOTF Z | S | C | O`).

No code word changed. The pin test's fingerprint covers each value set's size
and named values, which programs write, and leaves out the mark, which changes
neither a code word nor what a program writes. So marking or unmarking a set
is free, as microcode fixes are, and `kPinnedFingerprint` didn't change.

### Wiki

- `2.3-Instruction-Set-Source.md`, *Named values*: the `flags` option in the
  syntax, a **Flags** bullet with the disassembly rule and examples, and the
  `Flags` example marked to match the default instruction set's source.

## Design

### Names

- `ValueSetDef::flags`, and the `.izm` option `flags`: the backlog's word, and
  what C# calls the same idea (`[Flags]`). It is close to the CPU's status
  flags and the default instruction set's `Flags` set, but is only ever a
  set's option or field, so they don't meet in code. Other candidates were
  `combined` (vaguer) and `bit_mask` (collides with `mask`, a microcode
  register's `:mask` and a form's code word mask).

### core: ValueSetDef

```
struct ValueSetDef {
  ...
  // Whether the names are bits that a program combines with "|" (such as
  // status flags), rather than one choice each (such as a condition). The
  // tools write a value of a flag set as names joined by "|", taking the name
  // with the most of its bits first, instead of as one name or a number.
  bool flags = false;
};
```

`CompileInstructionSet` ignores it, as it does the rest of a value set.

### tools

- `InstructionForm::Disassemble` writes each code value with
  `FormatCodeValue` (file-local in `instruction_set_syntax.cc`), which holds
  both the exact-name lookup and the greedy rule.
  `FindFlagWithMostSetBits` picks each name.
- `InstructionSetSyntax` validates nothing new for a flag set.
- The instruction assembler's grammar is
  `"values" "(" "bits" ":" $bits=%int ["," $flags="flags"] ")"`. Unlike an
  instruction's or macro's options, a value set's are positional. A third
  option would be the time to make them a list in any order, with an error
  when `bits` is missing.
- `InstructionDefExporter` writes `.flags = true` only when set, as it does
  `.relative`.

### Brittleness

None new. The mark is one field that only `Disassemble` reads, and the round
trip holds for any values.
