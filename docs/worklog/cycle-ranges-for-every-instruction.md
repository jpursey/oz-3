# Cycle ranges for every instruction

Every instruction's header in `default_instruction_set.izm` gives its overall
cycle range, and a range for each variant. Each range was worked out from the
microcode and is pinned by tests at both ends. Before, most headers gave only
a minimum (`4+`, `5+`), and some were stale. This is groundwork for the
*Default instruction set reference* wiki page, which will be built from these
headers.

There was no change in behavior. Only the header comments and the tests
changed, and the `.inc` (which holds no comments) is unchanged.

## Behavior

The headers all take the same form:

    ## Cycles: 4-10
    ...
    ## Variants:
    ##    ADD.W <reg>, <reg>               (4 cycles)
    ##    ADD.W <reg>, <integer>           (5 cycles)
    ##    ADD.W <reg>, <word-address>      (6-8 cycles)

- `## Cycles:` is the overall minimum and maximum, or a single number when
  every variant costs the same.
- Each variant line ends with its own range, or a single number, aligned
  within the header.
- A variant that takes a value through the `GetWord`, `GetDword`, `LoadWord`,
  or store macros is split by the kind of value: `<reg>` (or `<dreg>`),
  `<integer>`, and `<word-address>` (or `<dword-address>`). One address line
  covers every memory form, from the cheapest (such as `(R1)` or `(SP)`) to
  the most expensive (such as `(R1 + 4)`).
- A range covers every input, including the cheap error and fall-through
  cases: dividing by zero, a branch or return not taken, a port not ready.
- When a variant's cost depends on a value, the header's description says on
  what: the number of bits for shifts, rotates, and bit operations, the banks
  for `RST`, the value for multiply, and that a divide by anything but zero
  takes the top of its range.
- The repeating instructions (`MVIR`, `MVDR`, `CPIR`, `CPDR`, `INR`, `OUTR`)
  give their cycles per execution, which is one word, as their cost grows
  with `R7`.
- Ranges assume no lock contention, since waiting for a memory bank, port, or
  core lock has no bound.

Some headers were wrong, and are now right: `HALT` takes 4 cycles to go idle
(not 3), `WAIT` takes its register's value but at least 3 (3-65535), `NOT`
takes 4-5 (not 5-9), and the divide variants included dividing by zero. A few
descriptions were also fixed (`ADC`, `SUB`, `SBQ`, and `SBC`), and the bit
operations' variants now say their positions are a register or an immediate.

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
and `E($v)`, and 4 for the `+ $v` forms. Loops and branches were worked out
by hand, with the worst case for each.

The tests are the check on this arithmetic: a range was only written into a
header once a test pinned both of its ends. For the shifts and rotates, whose
immediate forms are unrolled into a form per count, a temporary test measured
every count and value kind as a check on the hand-worked costs. It wasn't
checked in, as the pinned ends are what matter.

### Tests

`InstructionTest::RunCycleCases` (in `instruction_test.h`) pins the ends of
each variant as a table:

```
// An instruction and the cycles it takes. See InstructionTest::RunCycleCases.
struct CycleCase {
  std::string_view name;       // For failures, such as "ADD.W R0, R1"
  Cycles cycles;
  std::vector<uint16_t> code;  // From Encode(), then any words after it
};

// Adds each case's instruction to the code, then runs them in turn and
// expects each one's cycles. Call after InitAndReset().
void RunCycleCases(absl::Span<const CycleCase> cases);
```

The cycles come right after the name, so a row reads as a line of a timing
table:

```
{"PUSH.W (R1 + 1)", 9, {Encode("PUSH.W", {"($r + $v)", CpuCore::R1}), 1}},
```

- Each case starts at a NOP and ends at the next case's NOP, so timing one
  case leaves the core ready to time the next.
- The cases run in order in one program, without resetting registers. A test
  sets any registers and memory a table needs before calling it, and picks
  registers that keep its addresses valid. Most costs pinned this way depend
  only on the form, not the values.
- Each instruction must fall through to the next. A relative jump (`JPR`,
  `JCR`, `CALLR`) by 0 does, while still paying for the jump. Absolute jumps
  and calls jump to fixed addresses in their own tests, as `JP_Cycles` does.
- Where the cost depends on a value (shifts, rotates, `NEG.D`, bit masks,
  multiply and divide), `RunCountCases`, `MulDivTest`, or hand-written tests
  pin the ends instead.
- Each group's cycle tests follow the instruction's other tests, named
  `<instruction>_Cycles` (or by what they time, such as `JC_TakenCycles` or
  `DivideByZero_Cycles`).

**Brittleness:** a table that loads into a register it later uses as an
address can make a case read from a different bank, but no case's cost
depends on the address, so the cycles stay right. The NOP-bracketed timing
loop is now in `RunCycleCases`, `RunCountCases`, and `MulDivTest`'s two
helpers. The others put untimed setup between cases and check results, so
sharing it would need hooks for both, which costs more than the few lines
each copy is.
