# Cycle ranges for every instruction

Every instruction's header in `default_instruction_set.izm` gives its overall
cycle range, and a range for each variant. Each range is worked out from the
microcode and pinned by tests at both ends. Most headers today give only a
minimum (`4+`, `5+`), and some are stale. This is groundwork for the *Default
instruction set reference* wiki page, which will be built from these headers.

There is no change in behavior. Only the header comments and the tests change.
The `.inc` doesn't hold comments, so it doesn't change either.

## Behavior

An OZ-3 program sees no difference. The headers follow the form `RLC` and `RRC`
already use:

    ## Cycles: 4-137
    ...
    ## Variants:
    ##    RLC.W <reg>, <reg>      (6-65 cycles)
    ##    RLC.W <reg>, <1..16>    (4-19 cycles)

- `## Cycles:` is the overall minimum and maximum, or a single number when
  every variant costs the same.
- Each variant line ends with its own range, or a single number, aligned
  within the header.
- A variant whose value comes from the `GetWord`, `GetDword`, or `LoadWord`
  macros, or a matching store macro, is split by the kind of value: `<reg>`
  (or `<dreg>`), `<integer>`, and `<word-address>` (or `<dword-address>`), as
  `MUL` already does. One `<word-address>` line covers every memory form, from
  the cheapest (such as `(R1)` or `(SP)`) to the most expensive (such as
  `(R1 + 4)`).
- When a variant's cost depends on a value, the header's description says on
  what, as `NEG` does for `NEG.D`.
- When the cost grows without a fixed bound, the header gives the cost per
  unit instead: per word for the repeating block and port instructions, and
  per cycle waited for `WAIT`.
- Ranges assume no lock contention, since waiting for a memory bank, port, or
  core lock has no bound.

## Design

Everything is in `oz3/instruction_sets`: the headers in
`default_instruction_set.izm`, and the tests in `instruction_test_<group>.cc`
and `instruction_test.h`.

### Working out the ranges

Each range comes from the microcode, using the costs in the microcode wiki
page: 3 cycles for the code word, then 1 for most microcode ops, 0 for `UL`,
`LK`, `LKR`, `MSR`, `MSC`, `MSS`, `MSX`, `JP`, `END`, and `LAD`, and 1 for a
taken `JC` (0 untaken). The value macros add a fixed cost per form, which
makes most ranges come down to arithmetic. For example, `GetWord` adds 0 for
`$r`, 1 for `$v`, 2 for `($r)`, `(SP)`, and `(FP)`, 3 for `S($v)`, `D($v)`,
and `E($v)`, and 4 for the `+ $v` forms. Loops and branches are worked out by
hand, with the worst case for each.

The tests are the check on this arithmetic: a range is only written into a
header once a test pins both of its ends.

### Tests

Most groups check results and flags, but not cycles. A table of cases pins
the ends of each variant compactly:

```
// An instruction and the cycles it takes. See InstructionTest::RunCycleCases.
struct CycleCase {
  std::string_view name;          // For failures, such as "ADD.W R0, (R1 + 1)"
  uint16_t code;                  // From Encode()
  std::vector<uint16_t> words;    // Any words after the code word
  Cycles cycles;
};

// Runs each case's instruction in turn, and expects its cycles.
void RunCycleCases(absl::Span<const CycleCase> cases);
```

- It lives in `InstructionTest` (in `instruction_test.h`), beside
  `RunCountCases`, so every group can use it.
- The cases run in order in one program, without resetting registers, so a
  table picks registers that keep its addresses valid. The costs being pinned
  don't depend on the values, only on the form.
- Where the cost depends on a value (shifts, rotates, `NEG.D`, bit masks), the
  existing tables (`RunCountCases`, `MulDivTest`) or hand-written tests pin
  the ends instead.
- Instructions that jump (branches, calls, returns, interrupts) keep their own
  tests, as the cases assume each instruction falls through to the next.

**Brittleness:** a table that loads into a register it later uses as an
address can make a case read from a different bank, but no case's cost depends
on the address, so the cycles stay right.

### To confirm

- Which headers are already right. Multiply (from *Finish the default
  instruction set*), `RLC` and `RRC`, `JD` and `JDR`, `NEG`, and the
  repeating block and port instructions already state ranges, and most are
  pinned. Each CL checks the stated ranges against the microcode and the
  tests, and fills in any end that isn't pinned.
- `WAIT`'s header says `3+ ()`. CL1 checks what it costs for each value of its
  register.
- `RST`'s 11-28. CL10 checks what makes it vary.

## CLs

Each CL covers one instruction group: its headers in the `.izm`, and its
tests in `instruction_test_<group>.cc`. The groups go in `.izm` order. Every CL
is `instruction_sets` only, and none changes the wiki.

### CL1 [ ] instruction_sets: RunCycleCases, misc, and load and store

Depends on: nothing.

- `CycleCase` and `InstructionTest::RunCycleCases` in `instruction_test.h`.
- Headers for `NOP`, `HALT`, `WAIT`, `MOV`, `MVQ`, `PUSH`, `POP`, and `SWP`.

**Verify**
- Standard checks (see CLAUDE.md). The regenerated `.inc` is unchanged.
- `instruction_test_misc.cc` and `instruction_test_load_store.cc` pin both
  ends of every variant.

### CL2 [ ] instruction_sets: math

Depends on: CL1.

- Headers for `NEG`, `ADD`, `ADQ`, `ADC`, `SUB`, `SBQ`, `SBC`, `TST`, and `CMP`.

**Verify**
- Standard checks. The regenerated `.inc` is unchanged.
- `instruction_test_math.cc` pins both ends of every variant.

### CL3 [ ] instruction_sets: logic

Depends on: CL1.

- Headers for `NOT`, `AND`, `OR`, and `XOR`.

**Verify**
- Standard checks. The regenerated `.inc` is unchanged.
- `instruction_test_logic.cc` pins both ends of every variant.

### CL4 [ ] instruction_sets: shift

Depends on: CL1.

- Headers for `SHL`, `SHR`, and `SRA`, including the drop in cost at 16 bits
  for the dword forms.

**Verify**
- Standard checks. The regenerated `.inc` is unchanged.
- `instruction_test_shift.cc` pins both ends of every variant.

### CL5 [ ] instruction_sets: rotate

Depends on: CL1.

- Headers for `ROL` and `ROR`, including the zero count by register costing
  more than other small counts. `RLC` and `RRC` are checked.

**Verify**
- Standard checks. The regenerated `.inc` is unchanged.
- `instruction_test_rotate.cc` pins both ends of every variant.

### CL6 [ ] instruction_sets: bits and flags

Depends on: CL1.

- Headers for `CLRB`, `SETB`, `NOTB`, `TSTB`, `CLRF`, `SETF`, and `NOTF`.

**Verify**
- Standard checks. The regenerated `.inc` is unchanged.
- `instruction_test_bits.cc` pins both ends of every variant.

### CL7 [ ] instruction_sets: branch

Depends on: CL1.

- Headers for `JP`, `JPR`, `JC`, `JCR`, `CALL`, `CALLR`, `FBGN`, `FEND`, `RET`,
  and `RETC`, taken and not taken. `JD` and `JDR` are checked.

**Verify**
- Standard checks. The regenerated `.inc` is unchanged.
- `instruction_test_branch.cc` pins both ends of every variant.

### CL8 [ ] instruction_sets: interrupt

Depends on: CL1.

- Headers for `EI`, `DI`, `GETI`, `SETI`, `INT`, `IRT`, and `IRTC`.

**Verify**
- Standard checks. The regenerated `.inc` is unchanged.
- `instruction_test_interrupt.cc` pins both ends of every variant.

### CL9 [ ] instruction_sets: port

Depends on: CL1.

- Headers for `IN`, `INS`, `OUT`, and `OUTS`, including the port being ready
  or not. `INR` and `OUTR` are checked.

**Verify**
- Standard checks. The regenerated `.inc` is unchanged.
- `instruction_test_port.cc` pins both ends of every variant.

### CL10 [ ] instruction_sets: multiply, block, and core

Depends on: CL1.

- These headers already state ranges. Check `MUL`, `MULS`, `DIV`, `DIVS`,
  `MOD`, `DVMD`, `MVI`, `MVD`, `MVIR`, `MVDR`, `CPI`, `CPD`, `CPIR`, `CPDR`,
  and `RST`, and fix any that are wrong.

**Verify**
- Standard checks. The regenerated `.inc` is unchanged.
- `instruction_test_multiply.cc`, `instruction_test_block.cc`, and
  `instruction_test_core.cc` pin both ends of every variant.
