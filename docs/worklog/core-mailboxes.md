# Core Mailboxes

Cores pass messages to each other through **mailboxes**: 64 ports, one from
each core to each core, that every core reaches on port numbers 100h to 10Fh,
numbered from its own side. Port `100h + n` sends to core `n`, and `108h + n`
receives from it, whichever core the code runs on. So an operating system runs
the same code on every core, with no port numbers worked out from the core's
own index, and each pair of cores has a channel in each direction that no other
core can touch or contend for. The Ozzy OS's calls are designed on them (see
Calls in [ozzy_design.md](../ozzy_design.md)).

A port number is a word whose high byte picks a set of ports: 0 for the
processor's ports, and 1 for the mailboxes, with the rest left for sets to
come. `TSTP.I` takes its port in the word after the code word, like the other
immediate port instructions, so it reaches the mailboxes too.

## Behavior

What a program sees is in the manual:
[Mailboxes](../manual/2-Specifications.md#mailboxes) in the specifications,
`PLK` in [Microcode](../manual/2.1-Microcode.md#plkr), and
[Ports](../manual/2.2-Default-Instruction-Set.md#ports) and
[TSTP](../manual/2.2-Default-Instruction-Set.md#tstp) in the default
instruction set. In short:
- **Port numbers:** 0 to FFh are the processor's ports, up to its configured
  count. 100h to 107h are the running core's mailboxes to cores 0 to 7, and
  108h to 10Fh its mailboxes from them. Any other number is a port that
  doesn't exist, which is never ready.
- **Mailboxes:** Each works like a port: two words, a status, the `T`, `S`,
  and `A` mode flags, and a lock that resets it to word 0 when granted. The
  mailbox from core `a` to core `b` is port `100h + b` on core `a`, and port
  `108h + a` on core `b`. It exists only when both cores do. A core's mailbox
  to itself is both `100h + n` and `108h + n`. Mailboxes raise no interrupts,
  aren't counted in the configured ports, and can't have devices wired to
  them.
- **`TSTP.I`:** Takes its port in the word after the code word, at 5 cycles.
  Its code word is `0x9000` alone. It used to hold the port in 8 bits of the
  code word, at 4 cycles. This was the one change ever made to a pinned
  instruction's code words, allowed because no assembled programs existed yet
  and nothing used `TSTP.I`.

## Structure

### core

- **`Processor`** holds the mailboxes as a second `PortBank` of
  `kMaxCores * kMaxCores` ports, apart from its configured ports, indexed
  `from_core * kMaxCores + to_core`. `GetMailbox(from_core, to_core)` and
  `LockMailbox(from_core, to_core)` reach them, for both cores' sides and for
  the host. `GetNumPorts`, `GetPort`, and `LockPort` are still the configured
  ports alone, so devices and `ProcessorConfig` were unchanged. A `PortBank`
  only creates a port's lockable when it is first locked, so the mailboxes of
  cores a processor doesn't have cost one `Port` each.
- **`CpuCore`** takes its index in its processor at construction
  (`core_index_`). `CpuCore::LockPortNumber`, a private helper behind `PLK`,
  is the one place a port number becomes a port: a configured port, one of the
  running core's mailboxes, or none. It sets `lock_` and `locked_port_` (a
  `Port*`, null for a port that doesn't exist) together, so `PLD` and `PST` use
  the port without looking it up again.
- **`core_types.h`** names the port numbers, `kMailboxSendPort` and
  `kMailboxReceivePort` (`kMailboxSendPort + kMaxCores`). A `static_assert`
  keeps `kMaxPorts` at or below `kMailboxSendPort`, so a configured port can
  never hide a mailbox.

A mailbox depends on which core resolves its port number, so only `PLK`
resolves port numbers, always for the running core. The controlled core
(`CLK`) can't affect it, as a core holds one lock at a time.

### instructions

- **`TSTP.I`** is `"$v"`: `LD(C0); UL; PLK(C0); PLD(_,C1); MSR(S,S); PUL;`.
  The `.izm`'s comments for every port instruction say `<port>`, as the manual
  does.

## Building blocks

- **A new set of ports** goes in the free high bytes of a port number (2 and
  up), resolved in `CpuCore::LockPortNumber` beside the mailboxes.
- **`BaseCoreTest::GetMailbox` and `LockMailbox`** check their cores and reach
  a mailbox from a test, as `GetPort` and `LockPort` do a port.
- **Tests of programs on two cores** run them from separate banks, each core
  reset with its own `CODE` bank, as `MailboxSendsToAnotherCore`
  (`cpu_core_test.cc`) and `MailboxBetweenCores`
  (`instruction_test_port.cc`) do.
