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
- Shift and add loops over the value's bits, which stop after its highest
  set bit, as the 68000's `MULU` takes longer for values with more bits set.
  Multiplying by a small value is cheap: each bit up to the highest set bit
  costs 2 cycles, and each set bit 2 more (3 each for `MUL.DW`), plus one
  cycle if the product doesn't fit. `MUL.W` takes 7-68 cycles, `MULS.W`
  12-71, and `MUL.DW` 8-99, plus fetching the value.

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
- Shift and subtract loops, 5 cycles per bit: 86-87 cycles for `DIV.W`,
  `MOD.W`, and `DVMD.W`, 91-94 for `DIVS.W`, and 168-169 for the dword forms,
  plus fetching the value. Dividing by zero exits early, in 5-10 cycles.

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
address register by one. The repeats also decrement the count in `R7` for each
word.
- `Z`, `S`, `C`, and `O` are set as `CMP.W` sets them, so `Z` means the last
  word compared matched. After a repeat, the address register points past the
  match.
- `CPI` and `CPD` don't use `R7`. A loop around them keeps its own count, such
  as with `JD`.
- If `R7` is 0 at the start of a repeat, nothing is compared and `Z`, `S`,
  `C`, and `O` are cleared.
- `CPI` takes 6 cycles and `CPD` 7, as stepping down takes an add while
  stepping up comes free with the load. `CPIR` takes 9 cycles a word and
  `CPDR` 10, including fetching the instruction again, and the last word (when
  `R7` reaches 0) one less. An `R7` of 0 takes 5.

### Block move

    MVI  (<reg>), (<reg>)    copy, then step both addresses up
    MVD  (<reg>), (<reg>)    copy, then step both addresses down
    MVIR (<reg>), (<reg>)    MVI, repeated until R7 is 0
    MVDR (<reg>), (<reg>)    MVD, repeated until R7 is 0

Copies the word at the address in the second register to the address in the
first (each in the bank its register implies, so any pair of `DATA`, `EXTRA`,
and `STACK`), then steps both address registers by one. The repeats also
decrement the count in `R7` for each word.
- No flags change.
- `MVI` and `MVD` don't use `R7`, as with `CPI` and `CPD`.
- If `R7` is 0 at the start of a repeat, nothing is copied.
- The source register is stepped before the destination register is used,
  which costs nothing. So if both are the same register, the word is copied
  to the next address (up or down), and the register is stepped by two.
- `MVI` takes 7 cycles and `MVD` 9, as stepping down takes an add for each
  register. `MVIR` takes 9 cycles a word and `MVDR` 11, including fetching
  the instruction again. An `R7` of 0 takes 6.

### Repeating instructions

`CPIR`, `CPDR`, `MVIR`, `MVDR`, `INR`, and `OUTR` each do one word (or dword)
per execution. When they aren't done, they move `IP` back to themselves, so the
next instruction is the same one again:
- Interrupts are handled between words, and return to the instruction to carry
  on.
- Once the `T` flag is implemented (with the debugger), each step will be one
  word.
- Each word pays for fetching the instruction again.
- `MVIR`, `MVDR`, `INR`, and `OUTR` decrement `R7` first, which also tests
  it for 0 and for the last word, and restore it when nothing moves. That
  makes each word a cycle cheaper and an execution that moves nothing a
  cycle dearer, favoring moving data over polling a port that isn't ready.
  `CPIR` and `CPDR` test `R7` and decrement it after the compare instead, as
  `Z` must come from the compare.
- `R7` is the count for every repeating instruction, and the address
  registers are encoded, so `R7` can also be named as an address. That is
  allowed, and does what the microcode does: `CPIR` and `CPDR` use it as the
  address and then step and decrement it, while the others decrement it
  first. For the instructions that don't repeat, `R7` is an address like any
  other.

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
- Cycle counts change, as each word pays for a fetch. A word takes 8 cycles
  for the register port forms and 9 for the immediate ones (10 and 11 for a
  dword), and an `R7` of 0 takes 6 or 7.
- When the port isn't ready, `INR` stops before touching memory, in 7 to 9
  cycles. `OUTR` has already loaded the word by then, so it takes two cycles
  more than a word, and steps the address register back.

### Wiki

`2-Specifications.md` says the default instruction set has no memory to memory
operations and leaves block copies to the DMA coprocessor. It changes to say
that the default instruction set has block moves, one word at a time, while
copying whole pages is much faster with the DMA coprocessor.
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

The left shift multiply, with the value as the multiplier: each step shifts
the next bit out of a copy of the value in `C1`, adds the register into the
product in `C0` (which starts as zero) if the bit is set, and shifts the
register left. It stops once no bits of the value are left, so the last step
always adds, into the register itself, and needn't shift. A `JC` that isn't
taken and a `JP` cost nothing, so a zero bit costs 2 cycles (the two shifts)
and a set bit 4.

The product never needs more than the register's width. It doesn't fit if an
add carries, or if a bit is shifted out of the register while bits of the
value are left (as a later set bit would have added it), and nothing else
makes it too big. Either jumps to a copy of the loop without those checks,
which costs one cycle once, and then sets `C` and `O` at the end.

- `MUL.DW` does the same with the value in `C2`, the product in `C0` and
  `C1`, and a 32-bit add and shift, so each bit costs 3 cycles and each set
  bit 3 more.
- `MULS.W` multiplies the absolute values as `MUL.W` does, with the value in
  `C2`. `C1` starts as zero, so XORing the register into it copies it and
  tests its sign, and XORing the value in leaves the product's sign. If the
  unsigned product fits, the signed one fits when it is below 0x8000, or at
  most 0x8000 if it is negative, and the result is negated if negative.

CL2 first built a fixed cost right shift multiply, which took 71-72 cycles for
`MUL.W` whatever the operands. CL4 replaced it with this one, so multiplying by
a small value is cheap, as on a real CPU.

### Divide

The restoring shift and subtract divide: each step shifts the dividend left
out of the register into a remainder in `C1`, and subtracts the divisor when
it fits. Each quotient bit is left in `C` (the compare's borrow, flipped with
`MSX`), and shifted into the register as the next dividend bit is shifted
out, so a step costs 5 cycles whether or not it subtracts, and the register
becomes the quotient. A divisor of 0x8000 or more can push a 17th bit out of
the remainder, which counts as fitting. As in multiply, the last shift leaves
the flags a `TST` of the result would. `C2` starts as zero, so `OR(C2,r)`
copies and tests the divisor in one cycle. `MOD` doesn't need the quotient,
so its loops skip tracking it.

- For `DVMD.W` the remainder register is `a1`, so the remainder lands in the
  high word for free.
- The dword forms divide the high word, then the low word starting from the
  high word's remainder, as long division does, in two 16-step loops.
- `DIVS.W` divides the absolute values, keeping the quotient's sign in `MB`,
  and negates the quotient if it is negative. The only quotient that doesn't
  fit is 32768 from -32768 / -1, and in that case the register already holds
  -32768 again, so checking the result leaves it unchanged without restoring
  anything.

### To confirm

- `ST` and `MB` work as scratch for the signed forms. `MSR` overwrites `ST`,
  so nothing can be kept in `ST` across an `MSR`. (CL2) **Confirmed:**
  `DIVS.W` keeps the quotient's sign in `MB`, and the tests check that `MB` is
  unchanged afterward for every multiply and divide. Nothing needed more
  scratch, so `ST` is unused.
- The loops stay well under 255 microcodes an instruction. (CL2, CL3)
  **Confirmed:** the loops aren't unrolled, so each instruction is a few
  dozen microcodes.
- `InstructionAssembler` accepts formats with literal parentheses around both
  arguments, such as `"($r), ($r)"`. (CL5) **Confirmed:** `CPI` and the others
  are `"$r, ($r)"`, which assembles to two 3-bit register arguments.
- An interrupt raised during a repeat is handled between words and returns
  to the instruction. (CL5) **Confirmed:** the tests raise one during the
  first word of `CPIR` and `CPDR`, and it is handled once that word is done,
  with the instruction's own address pushed, so `IRT` carries on with the
  next word.

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

### CL2 [x] instruction_sets: Multiply

Depends on: nothing.

- `MUL.W`, `MULS.W`, and `MUL.DW` after `SBC`.
- Tests in a new `instruction_test_multiply.cc`, added to `CMakeLists.txt`.

**Verify**
- Standard checks.
- Unit tests: products that fit and that overflow, with `C` and `O`; signed
  combinations of signs, including -32768; zero operands; operands with every
  bit set for the longest case.
- Confirm the `ST` and `MB` scratch, and record the findings above.

### CL3 [x] instruction_sets: Divide and modulo

Depends on: CL2 (shares the test file).

- `DIV.W`, `DIVS.W`, `MOD.W`, `DVMD.W`, `DIV.DW`, and `MOD.DW` after the
  multiplies.
- Tests in `instruction_test_multiply.cc`.

**Verify**
- Standard checks.
- Unit tests: quotients and remainders, divisors of 0x8000 and more, signed
  rounding toward zero, divide by zero and `DIVS.W` -32768 / -1 (register
  unchanged, `O` set), `DVMD.W` ignoring the high word.

### CL4 [x] instruction_sets: Multiply by the value's bits

Depends on: CL2.

- `MUL.W`, `MULS.W`, and `MUL.DW` use the left shift multiply (see Multiply
  above), so their cycles depend on the value's highest set bit and how many
  bits it has set.
- Their tests in `instruction_test_multiply.cc` change to match.

**Verify**
- Standard checks.
- Unit tests: values of 0 and 1, values with few and many bits set, and
  0xFFFF (0x7FFF for `MULS.W`) for the longest case; overflow found by a
  carry in the loop, by a carry in the last add, and by a bit shifted out of
  the register; the other cases from CL2.

### CL5 [x] instruction_sets: Block compare

Depends on: nothing.

- `CPI`, `CPD`, `CPIR`, and `CPDR` after `CMP`.
- Tests in a new `instruction_test_block.cc`, added to `CMakeLists.txt`.

**Verify**
- Standard checks.
- Unit tests: each bank (`DATA`, `EXTRA`, `STACK` registers), up and down,
  a match, no match; for the repeats, a match on the last word, `R7` of 0,
  where `IP` ends, and an interrupt between words.

### CL6 [x] instruction_sets: Block move

Depends on: CL5 (shares the test file).

- `MVI`, `MVD`, `MVIR`, and `MVDR` after `SWP`.
- Tests in `instruction_test_block.cc`.
- The wiki change for block moves comes after landing (see below).

**Verify**
- Standard checks.
- Unit tests: copies between each pair of banks and within one, up and down,
  overlapping ranges in each direction, flags unchanged; for the repeats,
  `R7` of 0, where `IP` ends, and an interrupt between words.

### CL7 [x] instruction_sets: INR and OUTR one word per execution

Depends on: CL5 (the repeat approach and its tests).

- `INR` and `OUTR` take a register address that advances, and do one word or
  dword per execution, moving `IP` back until done.
- Their tests in `instruction_test_port.cc` change to match, dropping the
  removed address forms.

**Verify**
- Standard checks.
- Unit tests: each variant (register and immediate port, word and dword)
  reads or writes all of `R7`, stops early when the port isn't ready,
  handles `R7` of 0, leaves the address register past the last word, and
  moves `IP` back by the right size. An interrupt between words for a word
  and a dword variant of each, covering both instruction sizes.

### CL8 [x] instruction_sets: MVIR and MVDR decrement R7 first

Depends on: CL6, CL7 (the same approach).

- `MVIR` and `MVDR` decrement `R7` first, as `INR` and `OUTR` do, which also
  tests it for 0 and for the last word. Each word costs a cycle less, except
  the last, which costs the same, and an `R7` of 0 costs one more. `MVDR`
  steps its addresses down with `JD`, which leaves the flags alone, as `CPDR`
  does.
- Their tests in `instruction_test_block.cc` change to match.

**Verify**
- Standard checks.
- Unit tests: the existing `MVIR` and `MVDR` tests with the new counts, and
  `R7` as the address register.

### After landing [x] wiki: Block moves

Depends on: CL6, and the branch landed on `main`.

The session works in a worktree, which can't edit the main checkout's `wiki/`,
so the wiki change waits until the branch has landed and the session has left
the worktree.
- `2-Specifications.md` says the default instruction set has block moves (see
  Wiki above). In the "Overhaul addressing modes" bullet, the sentence about
  the DMA coprocessor becomes: "The exception is block moves (`MVI`, `MVD`,
  `MVIR`, and `MVDR`), which copy one word at a time between addresses held
  in registers. Copying whole pages is much faster with the DMA
  coprocessor."

**Verify**
- The user reviews the wiki change, and it is committed in `wiki/` with only
  `2-Specifications.md` added, and not pushed.
