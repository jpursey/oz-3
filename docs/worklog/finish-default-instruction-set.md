# Finish the default instruction set

The default instruction set gained the instructions it lacked: multiply and
divide, decrement and jump, and block compare and move. The repeating block
instructions do one word per execution and then repeat themselves by moving
`IP` back, as the Z80 does, so interrupts are handled between words. `INR` and
`OUTR` moved to the same approach, which changed their addressing.

Everything is microcode in `default_instruction_set.izm` (with the `.inc`
regenerated from it) and its tests. The core is unchanged.

## Behavior

Each instruction's header in the `.izm` gives its cycles, by variant where they
differ, and the tests pin both ends of every variant. Cycle counts below are
for a register value; fetching the value from an immediate or memory adds 1 to
4 cycles, as for every other instruction.

### Multiply

    MUL.W  <reg>,  <word-value>    unsigned, reg = reg * value
    MULS.W <reg>,  <word-value>    signed, reg = reg * value
    MUL.DW <dreg>, <word-value>    unsigned, dreg = dreg * value

- `Z` and `S` come from the result (the whole dword for `MUL.DW`).
- `C` and `O` are both set when the full product doesn't fit in the register
  (unsigned for `MUL`, signed for `MULS`), and both cleared otherwise, as x86
  does. The register holds the low bits of the product either way.
- A widening multiply (word by word to dword) is `MUL.DW` on a register whose
  high word is zero.
- The cost depends on the value, as the 68000's `MULU` does: each bit up to
  the value's highest set bit costs 2 cycles, and each set bit other than the
  highest 2 more (3 each for `MUL.DW`), plus one cycle if the product doesn't
  fit. `MUL.W` takes 7-68 cycles, `MULS.W` 12-71, and `MUL.DW` 8-99.

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
- 5 cycles per bit, whatever the operands: `DIV.W` takes 86 cycles, `MOD.W`
  and `DVMD.W` 87, `DIVS.W` 91-94 (depending on the signs), `DIV.DW`
  168, and `MOD.DW` 169. Dividing by zero stops early, in 5 (6 for `DIVS.W`).

### Decrement and jump

    JD  <reg>, <word-value>    reg -= 1; if reg != 0, IP = value
    JDR <reg>, <word-value>    reg -= 1; if reg != 0, IP += value

The Z80's `DJNZ` on any word register, built on the microcode's `JD`, in 4-5
cycles. No flags change. A register of 0 wraps to 0xFFFF and jumps, so it loops
65536 times.

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
- The register is compared before the address is stepped, so it may be the
  address register.
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
  Copying in place and stepping once would cost a cycle on every move, as
  microcode can't tell when both arguments are the same register.
- Overlapping copies behave as a word-at-a-time copy does: `MVIR` with the
  destination one above the source repeats the first word (a fill).
- `MVI` takes 7 cycles and `MVD` 9, as stepping down takes an add for each
  register. `MVIR` takes 9 cycles a word and `MVDR` 11, including fetching the
  instruction again. An `R7` of 0 takes 6.

### INR and OUTR

    INR.RW  <reg>,    (<reg>)      INR.RD  <reg>,    [<reg>]
    INR.IW  <0..255>, (<reg>)      INR.ID  <0..255>, [<reg>]
    OUTR.RW <reg>,    (<reg>)      OUTR.RD <reg>,    [<reg>]
    OUTR.IW <0..255>, (<reg>)      OUTR.ID <0..255>, [<reg>]

This was a breaking change, with no programs yet to break:
- The memory operand is a register only. The `$r + $v`, `SP`, `FP`, and
  immediate address forms are gone, as there is nowhere to keep an address
  that moves between executions.
- The address register advances past each word (two for a dword) and is left
  there, like the block instructions' registers. It used to be left
  unchanged.
- One word (or dword) per execution. The count in `R7` and the `S` flag mean
  what they did: `R7` is what is left, and `S` is set once `R7` is 0. It stops
  early, with `S` clear, when the port isn't ready, leaving `R7` and the
  address register at the first word not moved.
- A word takes 8 cycles for the register port forms and 9 for the immediate
  ones (10 and 11 for a dword), and an `R7` of 0 takes 6 or 7.
- When the port isn't ready, `INR` stops before touching memory, in 7 to 9
  cycles. `OUTR` has already loaded the word by then, so it takes two cycles
  more than a word, and steps the address register back.

### Repeating instructions

`CPIR`, `CPDR`, `MVIR`, `MVDR`, `INR`, and `OUTR` each do one word (or dword)
per execution. When they aren't done, they move `IP` back to themselves, so the
next instruction is the same one again:
- Interrupts are handled between words, and return to the instruction to carry
  on.
- Once the `T` flag is implemented (with the debugger), each step will be one
  word.
- Each word pays for fetching the instruction again.
- `MVIR`, `MVDR`, `INR`, and `OUTR` decrement `R7` first, which also tests it
  for 0 and for the last word, and restore it when nothing moves. That makes
  each word a cycle cheaper and an execution that moves nothing a cycle
  dearer, favoring moving data over polling a port that isn't ready. `CPIR`
  and `CPDR` test `R7` and decrement it after the compare instead, as `Z` must
  come from the compare.
- `R7` is the count for every repeating instruction, and the address registers
  are encoded, so `R7` can also be named as an address. That is allowed, and
  does what the microcode does: `CPIR` and `CPDR` use it as the address and
  then step and decrement it, while the others decrement it first. For the
  instructions that don't repeat, `R7` is an address like any other. The
  backlog's *Invalid register arguments* would let the assembler reject this,
  and the other combinations that are never useful.

### Wiki

`2-Specifications.md` says that block moves are the exception to the default
instruction set having no memory-to-memory operations, while copying whole
pages is much faster with the DMA coprocessor. Nothing else in the wiki
documents individual instructions yet (see the backlog's *Default instruction
set reference*).

## Design

### Names

- `.DW` is a variant suffix for a dword register with a word value. `.D`
  means a dword register with a dword value.
- `JD` shares its name with the microcode's `JD`, as `JC` and `IRT` do.
- `MVI` is distinct from `MVQ` (move an immediate) and from microcode
  `MVBI`/`MVNI`, which nothing in a program sees.

### Placement and encoding

- Opcodes are numbered in file order, so each instruction sits beside its
  family: multiply and divide after `SBC`, block move after `SWP`, block
  compare after `CMP`, and `JD`/`JDR` after `JCR`. Each insertion renumbered
  every instruction after it, which breaks nothing, as nothing depends on the
  numbers yet.
- The feature added nineteen opcodes, for 138 of 256.
- The block instructions are one word: two 3-bit register arguments (`a` and
  `b`), written `"$r, ($r)"` or `"($r), ($r)"`, with each instruction its own
  opcode. The same goes for the `INR` and `OUTR` register port forms, and the
  immediate port forms have one register argument and a following word.
- Each root instruction has its own header section in the `.izm`, listing its
  variants.

### Repeating

A repeating instruction that isn't done ends with `ADDI(IP,-n)`, where `n` is
its size in words, as `HALT` moves `IP` back with `ADDI(IP,-1)`. `IP` is past
the whole instruction once it is decoded, so `n` is fixed per variant: 1 for
the block instructions and the register port forms of `INR`/`OUTR`, and 2 for
the immediate port forms, which load the port from a following word.

The R7-first form starts with `ADDI(R7,-1)`: `C` is clear only if `R7` was 0
(`JC(NC,@restore)`, where `ADDI(R7,1)` puts it back), and `Z` is set if this
is the last word (`JC(Z,@done)` skips moving `IP` back). Port and memory
microcode and the locks leave `Z` and `C` alone, so the tests still hold after
the word is moved.

**Brittleness:** each repeating variant moves `IP` back by its own size, which
the microcode states by hand. Every variant's tests run it through at least
two repeats and check where it ends, and the interrupt tests check the pushed
`IP` for both sizes, so a wrong size fails a test. The R7-first form relies on
nothing between the decrement and the last jump changing `Z` or `C`, which is
why `MVDR` steps down with `JD` rather than `ADDI`.

### Microcode idioms

These came up across the feature, and are worth reusing in other instructions:
- **Free step up:** after `LD` or `ST`, the memory bank's address has
  advanced, and `LAD(r)` (0 cycles) stores it back into the address register.
- **Step down without touching flags:** `JD(r,@next)` with `@next` on the very
  next microcode decrements `r` in 1 cycle and lands there either way, leaving
  `MST` alone, where `ADDI(r,-1)` would change it.
- **Scratch:** `C0` and `C1` hold the immediate arguments (0 when there are
  none) and `C2` is 0 at the start of each instruction, so `OR(C2,r)` or
  `XOR(C1,r)` copies a value and tests it in one cycle. `MB` is restored when
  the instruction ends, so it is scratch too (`DIVS.W` keeps the quotient's
  sign there). `ST` isn't, as `MSR` overwrites it.
- **Flags for free:** a final `RLC` or `RRC` that shifts out a known zero
  leaves the flags a `TST` of the result would.
- **Rare paths in their own copy:** where a check is only needed until it
  fails once (multiply's overflow), jump to a copy of the loop without it, so
  the check costs nothing on the common path and one cycle once.
- **Undo on the rare path:** `OUTR` steps its address while memory is locked
  (free), and steps it back only when the port turns out to be busy.

### Multiply

The left shift multiply, with the value as the multiplier: each step shifts
the next bit out of a copy of the value in `C1`, adds the register into the
product in `C0` (which starts as zero) if the bit is set, and shifts the
register left. It stops once no bits of the value are left, so the last step
always adds, into the register itself, and needn't shift. A `JC` that isn't
taken and a `JP` cost nothing, so a zero bit costs 2 cycles (the two shifts)
and a set bit 4.

The product never needs more than the register's width. It doesn't fit if an
add carries, or if a bit is shifted out of the register while bits of the value
are left (as a later set bit would have added it), and nothing else makes it
too big. Either jumps to a copy of the loop without those checks, which costs
one cycle once, and then sets `C` and `O` at the end.

- `MUL.DW` does the same with the value in `C2`, the product in `C0` and `C1`,
  and a 32-bit add and shift, so each bit costs 3 cycles and each set bit 3
  more.
- `MULS.W` multiplies the absolute values as `MUL.W` does, with the value in
  `C2`. XORing the register into `C1` copies it and tests its sign, and XORing
  the value in leaves the product's sign. If the unsigned product fits, the
  signed one fits when it is below 0x8000, or at most 0x8000 if it is
  negative, and the result is negated if negative.

A fixed cost right shift multiply (71-72 cycles for `MUL.W` whatever the
operands) came first, and was replaced by this one so multiplying by a small
value is cheap, as on a real CPU.

### Divide

The restoring shift and subtract divide: each step shifts the dividend left out
of the register into a remainder in `C1`, and subtracts the divisor when it
fits. Each quotient bit is left in `C` (the compare's borrow, flipped with
`MSX`), and shifted into the register as the next dividend bit is shifted out,
so a step costs 5 cycles whether or not it subtracts, and the register becomes
the quotient. A divisor of 0x8000 or more can push a 17th bit out of the
remainder, which counts as fitting. The last shift leaves the flags a `TST` of
the result would. `MOD` doesn't need the quotient, so its loops skip tracking
it.

- For `DVMD.W` the remainder register is `a1`, so the remainder lands in the
  high word for free.
- The dword forms divide the high word, then the low word starting from the
  high word's remainder, as long division does, in two 16-step loops.
- `DIVS.W` divides the absolute values, keeping the quotient's sign in `MB`,
  and negates the quotient if it is negative. The only quotient that doesn't
  fit is 32768 from -32768 / -1, and in that case the register already holds
  -32768 again, so checking the result leaves it unchanged without restoring
  anything.

The loops aren't unrolled, so each instruction stays a few dozen microcodes,
well under the 255 an instruction can have.

### Tests

Tests are by instruction group, on `InstructionTest`, and check results, flags,
and exact cycle counts at both ends of each variant:
- `instruction_test_multiply.cc`: multiply and divide, with tables of cases
  run by `MulDivTest::RunWordCases` and `RunDwordCases`, which also check
  that `MB` is restored, and an `_Operands` test per instruction for the
  value's addressing forms.
- `instruction_test_branch.cc`: `JD` and `JDR`.
- `instruction_test_block.cc`: block compare and move, including each bank,
  wrapping addresses, `R7` as an address, and interrupts between words.
- `instruction_test_port.cc`: `INR` and `OUTR`, with `PortFeeder` and
  `PortDrainer` as the device.

`InstructionTest::InterruptRaiser` (in `instruction_test.h`) raises an
interrupt once, when `R7` reaches a given value, to interrupt a repeating
instruction between words. It is called each cycle through `CyclesUntilIp`,
as the port fakes are.
