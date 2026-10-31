# Invalid Register Arguments

Some register arguments are encodable but never useful. `MVI R1, R1` copies a
word to the next address and steps `R1` twice, and `MVIR (R7), (R1)` uses its
own count as an address. Microcode can't tell these apart from the useful
combinations without costing a cycle on every word, so today they are only
documented, as doing whatever the microcode does.

This lets an instruction definition declare which register arguments a program
can't write: that its two register arguments must differ, or that one can't be
a given register. The program assembler rejects a statement that breaks a rule,
pointing at the register. Then the default instruction set declares them on its
block move and repeating instructions.

The rules are only for the tools. A core still runs any code word as its
microcode says, so a program that writes one with `.DW` gets the documented
behavior, and the wiki keeps describing it.

## Behavior

### Instruction set source

An instruction's options gain register rules, in the same forms as `relative`
and `opcode:<number>`, and in any order with them:

```
instruction(distinct, exclude_a:R7, exclude_b:R7) MVIR "($r), ($r)" { ... }
instruction(exclude_b:R7) CPIR "$r, ($r)" { ... }
```

- `distinct`: the two register arguments can't be the same register. Both
  must be word registers (`$r`), or both dword registers (`$R`).
- `exclude_a:<register>`, `exclude_b:<register>`: the first or second argument
  can't be that register. It names a register the argument can hold: `R0` to
  `R7` for `$r`, the 4-bit registers (`SP` and so on) too for `$r4`, and `D0`
  to `D3` for `$R`. Each takes one register, and is repeated for more
  (`exclude_a:R6, exclude_a:R7`). Registers the argument's bits can't hold are
  already out of reach, so they are never listed (`R4` to `R7` of a `$r2`).
- `a` and `b` are the instruction's own code word arguments, as the microcode
  names them (`a`/`A` for the first, `b`/`B` for the second), and must be
  registers in its source. A macro code's register (the `$r` in `($r)` of
  `GetWord`) can't have a rule.

Each rule is an error in the `.izm` if it names an argument that isn't a
register, a register the argument can't hold, the same register twice, or
leaves an argument no register at all, or is `distinct` for arguments that
aren't two registers of the same type.

### Programs

A statement that breaks a rule is an error at the register:

```
MVI (R1), (R1)       ; MVI can't use R1 for both registers
MVIR (R7), (R1)      ; MVIR can't use R7 for this argument
```

`InstructionForm::Encode` fails the same way. `InstructionForm::Disassemble`
still turns such a code word into text, as the debugger may find one in
memory, and ends it with a comment saying why it doesn't assemble:
`MVI (R1), (R1) ; MVI can't use R1 for both registers`.

### Default instruction set

| Instructions                             | Rules                                     |
| ---------------------------------------- | ----------------------------------------- |
| `MVI`, `MVD`                             | `distinct`                                |
| `MVIR`, `MVDR`                           | `distinct`, `exclude_a:R7`, `exclude_b:R7` |
| `CPIR`, `CPDR`                           | `exclude_b:R7` (the address)              |
| `INR.RW`, `INR.RD`, `OUTR.RW`, `OUTR.RD` | `distinct`, `exclude_a:R7`, `exclude_b:R7` |
| `INR.IW`, `INR.ID`, `OUTR.IW`, `OUTR.ID` | `exclude_a:R7` (the address)              |

`CPIR R7, (R1)` (comparing each word against the remaining count) stays
allowed: it is odd, but does something a program could want.

No code word changes, so no pinned code word or fingerprint changes.

### Wiki

- `2.3-Instruction-Set-Source.md`: the options, and a *Register rules*
  section under *Arguments*.
- `2.5-Program-Source.md`: *Matching* says a matched statement must keep its
  instruction's register rules.
- `2.2-Default-Instruction-Set.md`: `MVI`, `MVD`, the repeating instructions,
  `INR`, and `OUTR` say which registers the assembler rejects, keeping what a
  code word with them does.

## Design

### Names

- `InstructionDef::arg1_excluded`, `arg2_excluded`: the registers each argument
  can't be. "Excluded" rather than "invalid", which collides with
  `Argument::IsValid()` (whether the argument's type and size are valid).
- `InstructionDef::distinct_args`: the two register arguments must differ.
- `.izm` options `distinct`, `exclude_a`, and `exclude_b`: named for the
  fields, with the arguments by their microcode names. Other candidates were
  `not_a:R7` (shorter, but reads as a negated flag rather than a list of
  registers) and `exclude1:R7` (matches `arg1`, but nothing in the `.izm`
  numbers arguments).
- *Register rules*: the wiki's and the `.izm`'s name for both together.
  Distinct from the source *rules* the wiki already lists for patterns, as
  these are about the registers a program names, not how it writes them.

### core: InstructionDef

```
struct InstructionDef {
  ...
  // Registers a program can't write for each argument, which must be a
  // register: bit n is set if it can't be register n (R7 is bit 7, D3 is bit
  // 3). Only the OZ-3 assembler uses these, and checks their rules (see
  // InstructionSetSyntax). CompileInstructionSet ignores them, and a core runs
  // any code word.
  uint16_t arg1_excluded = 0;
  uint16_t arg2_excluded = 0;

  // If true, a program can't write the same register for both arguments,
  // which must be registers of the same type. Only the OZ-3 assembler uses
  // this, as for arg1_excluded.
  bool distinct_args = false;
};
```

- Flat fields, so the exporter writes them as constexpr designated
  initializers, and leaves them out when they are the default.
- 16 bits holds every register a `$r4` can be.

### tools: InstructionSetSyntax

```
struct InstructionForm::Arg {
  ...
  // For a register, the registers it can't be (bit n for register n).
  uint16_t excluded = 0;

  // For a register, whether it can't be the same register as the form's other
  // distinct argument. A form has two of these, or none.
  bool distinct = false;
};
```

- Building a form copies the instruction's rules onto its own register
  arguments. A macro code's arguments get none. `distinct` is set on both of
  the instruction's register arguments, which are the only two in the form
  with it, and the error points at the second of them.
- `InstructionSetSyntax::Create` validates the rules: each names a register
  argument of the instruction's source, `excluded` has no bits past the
  argument's size and leaves at least one register, and `distinct_args` has
  two register arguments of the same type.
- `InstructionForm::ValidateCodeWordValues(values, &arg_index, &error)`
  validates every code word value: that it fits its bits (which `Encode`
  checked itself before), and keeps the register rules. It ignores the values
  of `$v` and `$V`, which matching doesn't know yet, and says which argument is
  at fault. `Encode` calls it before writing anything.
- The exporter writes the three fields.

### tools: InstructionAssembler and ProgramAssembler

- `InstructionConfig` gets `$distinct="distinct"`,
  `"exclude_a" ":" $exclude_a=%ident`, and the same for `exclude_b`, beside
  `opcode` and `relative`. The assembler looks up each register name through
  `CpuCore` (as a word or dword register, by the argument's type), and reports
  errors at the instruction, as for `opcode`. `InstructionSetSyntax` reports
  the rest, as for any def.
- `InstructionArg` gets the argument's token, and `ReadInstruction` replaces
  its own check that code values fit with `ValidateCodeWordValues`, so each
  error points at the value or register at fault. Encoding checks them again,
  as a backstop.

**Brittleness:** the rules are enforced only where a program is assembled, and
a hand-built `InstructionSetDef` gets them checked by `InstructionSetSyntax`
like every other tools rule. Nothing needs to stay in sync by hand: the forms
copy the rules from the def they are built from.

### To confirm

- That nothing in the existing tests assembles one of the now excluded
  combinations (CL4 runs them).

## CLs

### CL1 [x] core: register rules in InstructionDef

Depends on: nothing.

- `instruction_def.h`: `arg1_excluded`, `arg2_excluded`, and
  `distinct_args`.
- Unused, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).

### CL2 [x] tools: validate and encode register rules

Depends on: CL1.

- `InstructionSetSyntax`: validates the rules, and copies them onto each
  form's arguments. `ValidateCodeWordValues` checks code word values, and
  `Encode` rejects values it fails.
- `InstructionDefExporter`: writes the fields.
- Nothing sets them yet, so no visible change.

**Verify**
- Standard checks.
- Unit tests: each validation error; `Encode` and `ValidateCodeWordValues`
  with an excluded register and with the same register twice, for word and
  dword arguments, a macro code's register being unaffected, and `$v` and `$V`
  being ignored; the exporter writing the fields only when set.

### CL3 [x] tools: register rules in .izm and programs

Depends on: CL2.

- `InstructionAssembler`: the option syntax, and its errors.
- `ProgramAssembler`: the error at the register, with `InstructionArg`'s
  token, from `ValidateCodeWordValues` in place of its own check that code
  values fit.
- `GetRegisterNumber` (in `instruction_set_syntax.h`): a register's number
  from its name, which the `.izm` options and program matching share.

**Verify**
- Standard checks.
- Unit tests: assembling each kind of rule, and each `.izm` error at its
  location; a program breaking each kind of rule, with the error at the
  register.
- Wiki: `2.3-Instruction-Set-Source.md` and `2.5-Program-Source.md`.

### CL4 [ ] instruction_sets: declare the default instruction set's rules

Depends on: CL3.

- `default_instruction_set.izm`: the rules in the table above, and the
  headers of those instructions say which registers the assembler rejects.
  Regenerate the `.inc`.
- `finish-default-instruction-set.md`: its note on this backlog item says the
  assembler now rejects them.

**Verify**
- Standard checks, including that `git diff` of the `.inc` adds only the rule
  fields, and the fingerprint test passes unchanged.
- Unit tests: `default_instruction_set_assemble_test.cc` rejects a program
  for each rule.
- Wiki: `2.2-Default-Instruction-Set.md`.
