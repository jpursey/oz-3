# Core Mailboxes

Cores pass messages to each other through **mailboxes**: 64 ports, one from
each core to each core, that every core reaches on ports 100h to 10Fh,
numbered from its own side. Port `100h + n` sends to core `n`, and `108h + n`
receives from it, whichever core the code runs on. So an operating system runs
the same code on every core, with no port numbers worked out from the core's
own index, and each pair of cores has a channel in each direction that no other
core can touch or contend for.

Port numbers become a word whose high byte picks a set of ports: 0 for the
processor's ports, and 1 for the mailboxes, with the rest left for sets to
come. To make the mailboxes as easy to reach as any port, `TSTP.I` takes its
port in the word after the code word, like the other immediate port
instructions, rather than in 8 bits of the code word.

## Behavior

What a program sees is in the manual: [Mailboxes](../manual/2-Specifications.md#mailboxes)
in the specifications, `PLK` in [Microcode](../manual/2.1-Microcode.md#plkr),
and [Ports](../manual/2.2-Default-Instruction-Set.md#ports) and
[TSTP](../manual/2.2-Default-Instruction-Set.md#tstp) in the default
instruction set. In short:
- **Port numbers:** 0 to FFh are the processor's ports, up to its configured
  count. 100h to 107h are this core's mailboxes to cores 0 to 7, and 108h to
  10Fh its mailboxes from them. 110h to FFFFh don't exist, so they are never
  ready.
- **Mailboxes:** Each works like a port (two words, a status, the `T`, `S`, and
  `A` mode flags, and a lock reset to word 0 when granted). The mailbox from
  core `a` to core `b` is port `100h + b` on core `a`, and port `108h + a` on
  core `b`. It exists only when both cores do. A core's mailbox to itself is
  both `100h + n` and `108h + n`. Mailboxes raise no interrupts, aren't counted
  in the configured ports, and can't have devices wired to them.
- **`TSTP.I`:** Takes its port in the word after the code word, so it reaches
  every port number. It costs 5 cycles rather than 4, and its code word is
  `0x9000` alone, rather than `0x9000` to `0x90FF`.

### Breaking changes

`TSTP.I`'s code words change, which the default instruction set otherwise
never allows. Nothing outside this repository has assembled programs yet, and
nothing in it uses `TSTP.I` (the boot ROM uses `TSTP.R`, and is assembled from
source when an Ozzy is created), so this is the last chance to change it
cheaply. Its opcode, `0x90`, is unchanged.

## Design

### Names

- **Mailbox:** a port joining two cores. It doesn't collide with anything in
  the code. The Ozzy OS's calls come through each core's mailbox to core 0,
  on one interrupt line (see Calls in `docs/ozzy_design.md`), which replaces
  the design's earlier per-core **call port** and call interrupt.
- **Port number:** what a core names a port by (0 to FFFFh), as against a port
  *index* in the processor's `PortBank`, which hosts and devices use.

### Processor (core)

```
// Returns the mailbox from `from_core` to `to_core`. Both must be in the range
// [0, GetNumCores()).
Port& GetMailbox(int from_core, int to_core);

// Locks the mailbox from `from_core` to `to_core`, as for GetMailbox.
std::unique_ptr<Lock> LockMailbox(int from_core, int to_core);
```

- The mailboxes are a second `PortBank` of `kMaxCores * kMaxCores` ports,
  indexed `from_core * kMaxCores + to_core`. A `PortBank` only creates a port's
  lockable when it is first locked, so the unused ones cost one `Port` each.
- `GetNumPorts`, `GetPort`, and `LockPort` stay the processor's ports alone, so
  devices and `ProcessorConfig` don't change. `kMaxPorts` stays 256.

### CpuCore (core)

- `PLK` resolves the port number into a `Port*`: below the port count, the
  processor's port; 100h to 10Fh, the mailbox from or to the core in its low 3
  bits, if that core exists; and otherwise none. `locked_port_` becomes a
  `Port*` (null for none), so `PLD` and `PST` don't resolve it again.
- The resolving is a private helper beside `PLK`, the one place port numbers
  are turned into ports.
- A core didn't know its own index before, so `CpuCore` takes it at
  construction, from `Processor`, which knows it.

**Brittleness:** the mailbox's index depends on which core runs the
microcode, not just the number, so a core that resolved a port number on
another core's behalf would get the wrong mailbox. Only `PLK` resolves port
numbers, always for the running core. The controlled core (`CLK`) can't affect
it, as a core holds one lock at a time, so `PLK` never runs while another core
is locked.

### Default instruction set (instruction_sets)

- `TSTP.I` becomes `"$v"`: `LD(C0); UL; PLK(C0); PLD(_,C1); MSR(S,S); PUL;`,
  5 cycles.
- `default_instruction_set_test.cc` pins it: `kPinnedFingerprint` changes for
  this one instruction. The commit says why, as a comment describes the code as
  it is, not its history.

### To confirm

- That the cycle count of `TSTP.I` with a word operand is 5, as `IN.IW` is one
  more than `IN.RW` (CL2). Confirmed: `TSTP_I` times it at 5.
- That nothing else in the tree decodes `TSTP.I`'s port from its code word,
  such as a disassembler or listing (CL2). Confirmed: only the `.izm`, the
  `.inc`, and the instruction set tests name `TSTP.I`, and the assembler and
  listings work from the instruction's source pattern.

## CLs

### CL1 [x] core: mailboxes

Depends on: nothing.

- `Processor`: the mailbox `PortBank`, `GetMailbox`, and `LockMailbox`.
- `CpuCore`: `PLK` resolves port numbers into the processor's ports and the
  running core's mailboxes. `locked_port_` becomes a `Port*`. The core takes
  its index at construction.
- Manual: Mailboxes in `2-Specifications.md`, `PLK` in `2.1-Microcode.md`, the
  port numbers and the `<port>` operand in `2.2-Default-Instruction-Set.md`
  (the instructions reach the mailboxes already, through `.IW`, `.ID`, and the
  register forms), and the note on `ports` in `4-Ozzy-Computer.md`.

**Verify**
- Standard checks (see CLAUDE.md).
- `cpu_core_test.cc`: a word sent from one core and received on another, on
  each side's own port numbers, as `GetMailbox` sees it. The same port number
  on two cores is two mailboxes. A core's mailbox to itself. Mailboxes to and
  from a core the processor doesn't have, and port numbers past the mailboxes,
  are never ready. Mailboxes work with no configured ports. A locked mailbox
  blocks a core that wants it, but not one that wants another mailbox, and
  blocks both of its cores, each on its own port number for it. (Dwords are
  only two words through `PLD` and `PST`, which the port tests cover.)
- Manual pages updated.

### CL2 [x] instruction_sets: TSTP.I takes a word

Depends on: CL1, for its mailbox test.

- `default_instruction_set.izm`: `TSTP.I` takes `$v`, and its comment says
  `<port>`. The port variants' comments say `<port>` rather than `<0..255>`.
  Regenerate `default_instruction_set.inc`.
- `default_instruction_set_test.cc`: the new fingerprint.
- `instruction_test_port.cc`: `TSTP.I` tests for the new encoding and 5
  cycles, and a test that sends between two cores with `OUTS.IW` and `INS.IW`
  on mailbox port numbers, and waits with `TSTP.I`.
- `default_instruction_set_assemble_test.cc`: `TSTP.I` assembles with a
  mailbox port number, which needs the word.
- Manual: `TSTP` in `2.2-Default-Instruction-Set.md` (`<port>`, 5 cycles),
  already written with the spec.

**Verify**
- Standard checks.
- `git diff` of the `.inc` changes only `TSTP.I`.
- The boot ROM still assembles and boots (`boot_rom_test.cc`).
