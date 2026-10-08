# Finish the default instruction set

The default instruction set gains the instructions it still lacks: multiply and
divide, decrement and jump, and block compare and move. The repeating block
instructions do one word per execution and then repeat themselves by moving
`IP` back, as the Z80 does, so interrupts are handled between words. `INR` and
`OUTR` move to the same approach, which changes their addressing.

Everything here is in `default_instruction_set.izm` and its tests. The core is
unchanged: these are all built from existing microcode.

## Behavior

Each new instruction's header in the `.izm` gives its cycles as an overall
range and a range per variant, as `RLC` and `RRC` do, pinned by tests at both
ends of each variant. Cycle figures below are estimates, and the CLs settle
them.

### Multiply

    MUL.W  <reg>,  <word-value>    unsigned, reg = reg * value
    MULS.W <reg>,  <word-value>    signed, reg = reg * value
    MUL.DW <dreg>, <word-value>    unsigned, dreg = dreg * value

- `Z` and `S` come from the result (the whole dword for `MUL.DW`).
- `C` and `O` are both set when the full product doesn't fit in the
  register (unsigned for `MUL`, signed for `MULS`), and both cleared
  otherwise, as x86 does. The register holds the low bits of the product
  either way.
- A widening multiply (word by word to dword) is `MUL.DW` on a register
  whose high word is zero.
- Shift and add loops, about 5 cycles per bit of the register: roughly 85-95
  cycles for the word forms (more for `MULS.W`, which works on absolute
  values), and 190-200 for `MUL.DW`, plus fetching the value.

### Divide and modulo

    DIV.W  <reg>,  <word-value>    unsigned, reg = reg / value
    DIVS.W <reg>,  <word-value>    signed, reg = reg / value
    MOD.W  <reg>,  <word-value>    unsigned, reg = reg % value
    DVMD.W <dreg>, <word-value>    unsigned, low word / value:
                                   low word = quotient, high word = remainder
    DIV.DW <dreg>, <word-value>    unsigned, dreg = dreg / value
    MOD.DW <dreg>, <word-value>    unsigned, dreg = dreg % value

- Signed division rounds toward zero. There is deliberately no signed modulo,
  and no signed dword forms.
- `DVMD.W` ignores the register's high word on input, and puts the quotient in
  the low word and the remainder in the high word (as x86 puts them in `AX`
  and `DX`).
- `MOD.DW`'s remainder always fits in a word, so its high word is zero.
- On success, `Z` and `S` come from the result (the quotient for `DVMD.W`),
  and `C` and `O` are cleared.
- On an error, the register is unchanged, `O` is set, and `Z`, `S`, and `C`
  are cleared. The errors are dividing by zero, and `DIVS.W` of -32768 by -1
  (whose quotient doesn't fit).
- Shift and subtract loops, about 6-7 cycles per bit: roughly 100-115 cycles
  for the word forms and 210-230 for the dword forms, plus fetching the value.
  An error exits early.

### Decrement and jump

    JD  <reg>, <word-value>    reg -= 1; if reg != 0, IP = value
    JDR <reg>, <word-value>    reg -= 1; if reg != 0, IP += value

The Z80's `DJNZ` on any word register, built on the microcode's `JD`. No flags
change. A register of 0 wraps to 0xFFFF and jumps, so it loops 65536 times.

### Block compare

    CPI  <reg>, (<reg>)    compare, then step the address up
    CPD  <reg>, (<reg>)    compare, then step the address down
    CPIR <reg>, (<reg>)    CPI, repeated until a match or R7 is 0
    CPDR <reg>, (<reg>)    CPD, repeated until a match or R7 is 0

Compares the first register with the word at the address in the second
register (in the bank that register implies), as `CMP.W` does, then steps the
address register by one and decrements `R7`.
- `Z`, `S`, `C`, and `O` are set as `CMP.W` sets them, so `Z` means the last
  word compared matched. After a repeat, the address register points past the
  match.
- If `R7` is 0 at the start, nothing is compared and `Z`, `S`, `C`, and `O`
  are cleared.

### Block move

    MVI  (<reg>), (<reg>)    copy, then step both addresses up
    MVD  (<reg>), (<reg>)    copy, then step both addresses down
    MVIR (<reg>), (<reg>)    MVI, repeated until R7 is 0
    MVDR (<reg>), (<reg>)    MVD, repeated until R7 is 0

Copies the word at the address in the second register to the address in the
first (each in the bank its register implies, so any pair of `DATA`, `EXTRA`,
and `STACK`), then steps both address registers by one and decrements `R7`.
- `Z` is set if `R7` is 0 afterward, and cleared otherwise, so a loop around
  `MVI` needs no `TST`. Other flags are unchanged.
- If `R7` is 0 at the start, nothing is copied (and `Z` is set).
- About 10 cycles per word, including fetching the instruction again for each
  word.

### Repeating instructions

`CPIR`, `CPDR`, `MVIR`, `MVDR`, `INR`, and `OUTR` each do one word (or dword)
per execution. When they aren't done, they move `IP` back to themselves, so the
next instruction is the same one again:
- Interrupts are handled between words, and return to the instruction to carry
  on.
- Once the `T` flag is implemented (with the debugger), each step will be one
  word.
- Each word pays for fetching the instruction again.
- `R7` is the count for every block instruction, and the address registers
  are encoded, so `R7` can also be named as an address. That is allowed, and
  does what the microcode does: it is used as the address, then stepped and
  decremented.

### INR and OUTR

    INR.RW  <reg>,    (<reg>)      INR.RD  <reg>,    [<reg>]
    INR.IW  <0..255>, (<reg>)      INR.ID  <0..255>, [<reg>]
    OUTR.RW <reg>,    (<reg>)      OUTR.RD <reg>,    [<reg>]
    OUTR.IW <0..255>, (<reg>)      OUTR.ID <0..255>, [<reg>]

This is a breaking change, with no programs yet to break:
- The memory operand is a register only. The `$r + $v`, `SP`, `FP`, and
  immediate address forms are gone, as there is nowhere to keep an address
  that moves between executions.
- The address register now advances past each word (two for a dword) and is
  left there, like the block instructions' registers. Today it is left
  unchanged.
- One word (or dword) per execution, as above. The count in `R7` and the `S`
  flag mean what they do today: `R7` is what is left, and `S` is set once
  `R7` is 0. It stops early, with `S` clear, when the port isn't ready.
- Cycle counts change, as each word pays for a fetch.

### Wiki

`2-Specifications.md` says the default instruction set has no memory to memory
operations and leaves block copies to the DMA coprocessor. It changes to say
that the default instruction set has block moves, one word at a time, while
the DMA coprocessor copies blocks much faster over a memory bus of its own.
Nothing else in the wiki documents individual instructions yet (see the
backlog's *Default instruction set reference*).

## Design

All of it is microcode in `default_instruction_set.izm`, with the `.inc`
regenerated, and tests in the `instruction_test_*.cc` files.

### Names

- Instruction names come from the backlog: `MUL`, `MULS`, `DIV`, `DIVS`,
  `MOD`, `DVMD`, `JD`, `JDR`, `CPI`, `CPD`, `CPIR`, `CPDR`, `MVI`, `MVD`,
  `MVIR`, and `MVDR`.
- `.DW` is a new variant suffix: a dword register with a word value. `.D`
  already means a dword register with a dword value.
- `JD` shares its name with the microcode's `JD`, as `JC` and `IRT` already
  do.
- `MVI` is distinct from `MVQ` (move an immediate) and from microcode
  `MVBI`/`MVNI`, which nothing in a program sees.

### Placement and encoding

- Opcodes are numbered in file order, so each instruction goes beside its
  family: multiply and divide after `SBC`, block move after `SWP`, block
  compare after `CMP`, and `JD`/`JDR` after `JCR`. Every instruction after an
  insertion is renumbered in the `.inc`. That breaks nothing, as nothing
  depends on the numbers yet, but it makes the `.inc` diff longer than the
  change.
- Nineteen new opcodes, for 138 of 256.
- The block instructions are one word: two 3-bit register arguments (`a` and
  `b`), with each instruction its own opcode.

### Repeating

A repeating instruction that isn't done ends with `ADDI(IP,-n)`, where `n` is
its size in words, as `HALT` moves `IP` back with `ADDI(IP,-1)`. `IP` is past
the whole instruction once it is decoded, so `n` is fixed per variant: 1 for
the block instructions and the register port forms of `INR`/`OUTR`, and 2 for
the immediate port forms, which load the port from a following word.

**Brittleness:** each repeating variant moves `IP` back by its own size, which
the microcode states by hand. Every variant's tests run it through at least
two repeats and check where it ends, so a wrong size fails a test. `R7` being
both the count and a possible address is documented rather than prevented.

### Multiply

The right shift multiply: the register being multiplied is the product's low
word, `C1` accumulates the high word, and each of 16 steps adds the value to
`C1` when the low word's bottom bit is set, then rotates `C1` and the low word
right through carry. After 16 steps, `C1` and the register hold the 32-bit
product, so overflow is a test of `C1`. `MUL.DW` does the same over 32 steps
with a 48-bit product (`C1`, `a1`, and `a0`).

`MULS.W` multiplies the absolute values, then negates the result if the signs
differed, checking that it fits in a signed word. It needs more scratch than
`C0` to `C2`, so it also uses `ST` and `MB`. Both are restored when the
instruction ends. `INR.IW` already uses `MB` this way.

### Divide

The restoring shift and subtract divide: each step shifts the dividend left
into a remainder register, and subtracts the divisor when it fits, setting the
quotient bit. The dividend register becomes the quotient. For `DVMD.W` the
remainder register is `a1`, so the remainder lands in the high word for free.
A divisor of 0x8000 or more can push a 17th bit out of the remainder, which
counts as fitting. `DIVS.W` divides absolute values and fixes the sign, as
`MULS.W` does.

### To confirm

- `ST` and `MB` work as scratch for the signed forms. `MSR` overwrites `ST`,
  so nothing can be kept in `ST` across an `MSR`. (CL2)
- The loops stay well under 255 microcodes an instruction. (CL2, CL3)
- `InstructionAssembler` accepts formats with literal parentheses around both
  arguments, such as `"($r), ($r)"`. (CL4)
- An interrupt raised during a repeat is handled between words and returns
  to the instruction. (CL4)

## CLs

Each CL is in `instruction_sets`, regenerates `default_instruction_set.inc`
from the `.izm`, and has tests of results, flags, and exact cycle counts at
both ends of every variant.

### CL1 [x] instruction_sets: JD and JDR

Depends on: nothing.

- `JD` and `JDR` after `JCR` in the `.izm`.
- Tests in `instruction_test_branch.cc`.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: jumps when the decremented register isn't zero, falls through
  at zero, wraps from 0, flags unchanged, the register as its own target,
  and both ends of each variant's cycle range.

### CL2 [ ] instruction_sets: Multiply

Depends on: nothing.

- `MUL.W`, `MULS.W`, and `MUL.DW` after `SBC`.
- Tests in a new `instruction_test_multiply.cc`, added to `CMakeLists.txt`.

**Verify**
- Standard checks.
- Unit tests: products that fit and that overflow, with `C` and `O`; signed
  combinations of signs, including -32768; zero operands; operands with every
  bit set for the longest case.
- Confirm the `ST` and `MB` scratch, and record the findings above.

### CL3 [ ] instruction_sets: Divide and modulo

Depends on: CL2 (shares the test file).

- `DIV.W`, `DIVS.W`, `MOD.W`, `DVMD.W`, `DIV.DW`, and `MOD.DW` after the
  multiplies.
- Tests in `instruction_test_multiply.cc`.

**Verify**
- Standard checks.
- Unit tests: quotients and remainders, divisors of 0x8000 and more, signed
  rounding toward zero, divide by zero and `DIVS.W` -32768 / -1 (register
  unchanged, `O` set), `DVMD.W` ignoring the high word.

### CL4 [ ] instruction_sets: Block compare

Depends on: nothing.

- `CPI`, `CPD`, `CPIR`, and `CPDR` after `CMP`.
- Tests in a new `instruction_test_block.cc`, added to `CMakeLists.txt`.

**Verify**
- Standard checks.
- Unit tests: each bank (`DATA`, `EXTRA`, `STACK` registers), up and down,
  a match, no match, a match on the last word, `R7` of 0; for the repeats,
  where `IP` ends, and an interrupt between words.

### CL5 [ ] instruction_sets, wiki: Block move

Depends on: CL4 (shares the test file).

- `MVI`, `MVD`, `MVIR`, and `MVDR` after `SWP`.
- Tests in `instruction_test_block.cc`.
- `2-Specifications.md` says the default instruction set has block moves (see
  Wiki above).

**Verify**
- Standard checks.
- Unit tests: copies between each pair of banks and within one, up and down,
  overlapping ranges in each direction, `R7` of 0, the `Z` flag; for the
  repeats, where `IP` ends, and an interrupt between words.
- Wiki updated.

### CL6 [ ] instruction_sets: INR and OUTR one word per execution

Depends on: CL4 (the repeat approach and its tests).

- `INR` and `OUTR` take a register address that advances, and do one word or
  dword per execution, moving `IP` back until done.
- Their tests in `instruction_test_port.cc` change to match, dropping the
  removed address forms.

**Verify**
- Standard checks.
- Unit tests: each variant (register and immediate port, word and dword)
  reads or writes all of `R7`, stops early when the port isn't ready,
  handles `R7` of 0, leaves the address register past the last word, moves
  `IP` back by the right size, and handles an interrupt between words.
