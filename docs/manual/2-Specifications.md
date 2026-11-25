# Specifications

## Design philosophy

The OZ-3 is a virtual CPU, building on the general architecture ideas and quirks of the OZ-1 and OZ-2 (see [History](1-History.md)). 

### Just quirky enough

The goal of the OZ-3 is to be the heart of a "computer" exposed more or less directly in games and toy applications. It has the following top-level goals:

* *Old school:* The OZ-3 is intended to "feel" like an old 16-bit style CPU from the 1980s and 1990s. However, it is deliberately not an actual copy, so programmers (gamers) have a sense of discovery and can't just take existing code and run it.
* *Unique:* It has unique or uncommon features that diverge from most/any other CPU. In the OZ-3, the prime example is the separate and configurable memory banks for code, stack, and two general purpose memory banks (data and extra).
* *Simple:* While the CPU inspiration is certainly on the Z80/x86 side, there is more uniformity in the register layout and opcodes in the default instruction set. This allows for fewer workarounds when programming (for instance, due to limited addressing modes supported by an opcode). It also makes programs shorter as a result, which is important with the relatively small memory footprint.
* *Less boilerplate:* Assembly programming is always going to be verbose, but the OZ-3 has a generous amount of opcode space, and so in true CISC style, the default instruction set supports several complex opcodes for commonly needed tasks (pushing/popping register sets, counting, looping, etc). This also adds to the discovery and options when it comes to optimization.
* *Extensible:* The CPU is intended to be embeddable within a game, and so the architecture and runtime are designed to support external devices, coprocessors (including expanding opcodes), etc. without needing to change the core. You can also provide your own instruction set to either limit or expand upon the default instruction set. In fact, each core can have its own unique instruction set for asymmetric use cases.
* *Fun:* The processor is ridiculous and would never exist as a real processor. The bus requirements combined with exclusive locking semantics on components would be highly impractical in real silicon. The cycle counts are self-consistent and kinda real-ish, but not really. Overall, the design is likely actively hostile to an efficient hardware implementation. But that's not the point. It has the veneer of being real, but the decisions are based more on what would be fun and easy to reason about -- especially in the context of a game. There are even a few "hacks" that have documented/guaranteed behavior for those interested in building their own instruction sets for the OZ-3.

### Backward compatibility

While its intended use cases are the same as the OZ-1 and OZ-2, it is not backward compatible at all. The obvious reason for this is that there are no existing OZ-1 and OZ-2 programs -- so why bother. However, this is a nostalgia passion project, and backwards compatibility can be "fun" (after all I did make the OZ-2 backward compatible with the OZ-1). So the more nitty gritty reasons for breaking backward compatibility are:

* Simplify the organization of the opcodes (grouping similar opcodes together).
* Be flexible in terms of what the instruction set is. While the OZ-3 provides a general-purpose 80's-era instruction set, it is implemented in terms of OZ-3 microcode. This both allows for new bespoke instruction sets for games, as well as providing "realistic" cycle counts based on the microcode implementation. In fact, the OZ-1 and OZ-2 instruction sets could be supported with a custom instruction set (if combined with the math coprocessor).
* Replace the "error" flag with an "overflow" flag in the Z80 style which better supports signed integer use cases. The overflow flag can still double as an error flag when signed overflow is not meaningful (for instance, a divide by zero).
* Change floating point operations to be more of a coprocessor-style approach (separate external registers and parallel execution). The underlying OZ-3 CPU core does not directly support anything except 16-bit add, subtract, and bitwise operations. Of course, the default instruction set implements 32-bit operations and extended operations like integer multiply and divide, but they are implemented against the limitations of the base microcode. Supporting IEEE floating point would be... impractical, to say the least.
* Remove the separate concept of an "address stack" and just use the single "stack" memory. The (now single) stack also follows the more typical pattern of pushing onto the stack resulting in decrementing the stack pointer.
* Unify the memory model by adding 16 banks of memory (64 Kiw each). The code, stack, data, and extra banks for an OZ-3 core can all be assigned to separate (or the same) bank of memory. This also plays nicely with multiple cores.
* Overhaul addressing modes in the default instruction set, supporting index style addressing directly and removing the ability to do memory-to-memory operations. A register always holds a result, and a `MOV` to memory is only supported from a register. The exception is block moves (`MVI`, `MVD`, `MVIR`, and `MVDR`), which copy one word at a time between addresses held in registers. Copying whole pages is much faster with the DMA coprocessor. Note, this is only a limitation of the default instruction set. Custom instruction sets can do what they like.

## Hardware specification

### Overview

The OZ-3 is modeled as a 16-bit processor with up to 8 cores, up to 16 banks of memory, up to 256 double-word ports, a mailbox from each core to each core, and 32 interrupt lines. The following lists the various OZ-3 hardware components and how they work together.

* **Main memory:** The OZ-3 supports up to 16 banks of memory each with 64 Kiw of address space. The memory is general purpose, and is used for code, data, and device memory mapping. Each memory bank has a dedicated 16-bit address bus and 16-bit data bus, which is shared across any cores, coprocessors, and external devices that support memory mapping. Each memory bank is logically divided into 16 pages (4 Kiw each), and is statically configured with a 16-bit read mask and a 16-bit write mask indicating the read/write access permissions for each page. In addition, a memory bank's pages can be memory mapped to several devices or coprocessors (or really, anything), each on a contiguous range of pages of its own. A mapped page belongs wholly to its device, which decides what reading and writing it does, and hides any physical memory on that page. Each device addresses its pages from 0 at its first page, so it works the same wherever it is mapped. The read/write masks cannot be changed by OZ-3 code at runtime; they are part of the system configuration.
* **Cores:** The OZ-3 CPU supports up to 8 cores that run in parallel. Each core has its own dedicated set of registers, interrupt vector, and ability to fetch, decode, and execute opcodes. However, all cores share main memory, ports, and coprocessors. Each core is associated with up to four memory banks simultaneously: one for code (`CODE`), one for the stack (`STACK`), and two for general purpose data (`DATA` and `EXTRA`).
* **Ports:** The OZ-3 supports up to 256 logical I/O ports. Each I/O port caches two 16-bit words and a single status word (which may be zero or one). A processor may be configured with fewer ports, and a port it doesn't have is never ready: it has nothing to read, and can't be written. Reading and writing to ports is controlled by three mode flags which may be freely set, cleared, or combined:
  * Test (T): This tests the status of the port and either performs the read/write or not as follows: read only if the status is 1, and write only if the status is 0.
  * Status (S): This updates the status of the port as follows: read sets the status to 0, and write sets the status to 1.
  * Address (A): This updates the internal address of the port after the read/write to the other 16-bit word. For instance, if the `A` flag is set the first read will be the first word and the second read will be the second word. If a third read occurs, it will be the first word again. The address is reset to the first word whenever a core, coprocessor, or device is granted the port's lock, so every lock starts at the first word. A request that waits behind the current holder doesn't change the address until it is granted.
* **Mailboxes:** Each core has a mailbox to every core, itself included, for passing words from one core to another. A mailbox works just like a port (two words, a status, the same mode flags, and a lock), but it joins exactly two cores: no device is wired to it, and it isn't one of the processor's configured ports. A core reaches its mailboxes on ports 100h to 10Fh, which it numbers from its own side, so every core runs the same code to reach them (see [Mailboxes](#mailboxes)).
* **Interrupts:** The OZ-3 has 32 interrupts triggered via dedicated interrupt lines per core. Cores, coprocessors, and devices (or the host application) can trigger interrupts on the lines they are connected to. Multiple devices and coprocessors can be connected to the same line; however, since they are on one line, they cannot raise interrupts simultaneously. Each interrupt is independently handled by each OZ-3 core, with the address stored in an interrupt vector settable by individual cores. This means two cores may both handle the same interrupt when it is raised, or cores may be assigned to different interrupts (as determined by their software). An interrupt may instead be raised as a *shared* interrupt, which only one of the cores it is raised on handles: the first to take it with a handler for it (see [Interrupts](#interrupts)).
* **Devices:** Devices are hardware outside the OZ-3 itself (such as a screen, a keyboard, or a disk drive) that the host application attaches to the processor, and that run in step with the cores, cycle by cycle. Each device is wired to the ports and interrupt lines it uses, can lock any memory bank to use it as shared memory, and can have pages of its own in a memory bank (see main memory above). A device uses these the same way a core does: through their locks, contending with the cores and any other devices. Each cycle, the cores run first, and then the devices in a fixed order, so a machine always runs the same way. The OZ-3 comes with ready-made [devices](3-Devices.md) that any processor can have.
* **Clock:** A processor runs at a clock rate set by the host application, 1 MHz unless it says otherwise. Everything in the OZ-3 is measured in cycles, so the clock rate only says how many cycles are a second of real time. A device configured with a rate in real time (a baud rate, a refresh rate) uses it to turn that rate into cycles, so the device works the same at any clock rate.

### Registers

Registers define the working set memory and status for each OZ-3 core. They are unique to each core, and generally are only directly accessible by code running within the core (cores can access each other's registers in a limited fashion via multi-core instructions).

* **General purpose registers:** The general registers in the OZ-3 are fully interchangeable across opcodes for a given bit-depth. Notably (compared to the Z80/8080 and friends) there are no "accumulator" or "index" registers.
  * *16-bit registers:* The OZ-3 has 8 general purpose 16-bit registers: `R0` to `R7`.
  * *32-bit registers:* The OZ-3 combines the 16-bit registers into pairs, yielding 4 general purpose 32-bit registers: `D0` to `D3`. These are aliased over the 16-bit registers in little endian fashion, with `R0` being the low 16-bits of `D0` and `R1` being the high 16-bits of `D0`.
* **Special purpose address registers:** The OZ-3 also has several dedicated special purpose registers. These refer to addresses in each memory data bank and may be offset with a scalar value when referencing memory. Unlike the general purpose registers, they also have meaning tied to dedicated opcodes, and are manipulated directly by the OZ-3 as part of general operation.
  * *Base address register:* Each memory bank has a dedicated 16-bit register which is treated as the base (zero) address for each memory bank: `BC`, `BS`, `BD`, and `BE` are the base address for the `CODE`, `STACK`, `DATA`, and `EXTRA` memory banks respectively. All memory accesses are implicitly relative to these register values. 
  * *Instruction Pointer:* The `IP` register indicates where the next instruction is located that the OZ-3 core will execute in the `CODE` memory bank (relative to the `BC` register). The `IP` register wraps around if execution passes the top of memory.
  * *Stack Pointer:* The `SP` register indicates where the top of the stack is in the `STACK` memory bank (relative to the `BS` register). Like the Z80, the top of the stack points to the address of the last pushed value (not the one after it). The stack pointer register starts at zero and wraps around.
  * *Frame Pointer:* The `FP` register is a secondary pointer into the `STACK` memory bank (relative to the `BS` register). It is commonly used to specify the base address of the stack "frame" for a function being run (the previous function's `SP` value).
* **Memory Bank:** The `MB` register is a 16-bit register which indicates the banks being used by the core. OZ-3 code can configure the memory banks with the `RST` instruction in the default instruction set. There are four bank assignments, each represented by 4 bits. Bank assignments can all refer to the same physical memory bank.
  * *`CODE` (bits 0-3):* This specifies the bank the `IP` register references, and where instructions and their immediate operands are fetched from.
  * *`STACK` (bits 4-7):* This specifies the bank the `SP` and `FP` registers reference, and where stack data is stored and retrieved. The general purpose registers `R6` and `R7` also refer to addresses in this bank.
  * *`DATA` (bits 8-11):* This specifies the primary data bank for all direct and indirect addressing of main memory (data that isn't the stack). The general purpose registers `R0` to `R3` refer to addresses in this bank. In many cases, the `DATA` bank and the `STACK` bank may map to the same physical memory.
  * *`EXTRA` (bits 12-15):* This specifies the secondary data bank for general purpose use. The general purpose registers `R4` and `R5` refer to addresses in this bank. This may be used to access additional memory, or to provide additional register addressing into the same bank as is mapped to `STACK` or `DATA`.
* **Status/Control register:** The `ST` register is a 16-bit register that contains all status flags for the core. It cannot be accessed directly at all, except internally by instructions via microcode, but individual flags can be set, cleared, or tested. See [Flags](#flags) for details.
* **Interrupts:**
  * *Interrupt trigger:* The `IT` register is a 32-bit register that contains one bit per interrupt, indicating whether that interrupt was raised. It is set by the triggering code (usually an external device or coprocessor), and then it is cleared when the core handles the interrupt (whether it is mapped to a handler or not). Beside it, each core has 32 *shared* trigger bits, one per interrupt, set when the interrupt is raised on it as a shared interrupt. A core clears its own shared bit when it handles the interrupt without a handler, and the core that handles it with a handler clears the shared bit on every core (see [Interrupts](#interrupts)).
  * *Interrupt vector:* Each core has its own vector of 32 16-bit addresses which specify where the interrupt handler is located. If the address is zero, then no handler will be called (address zero is never called). Otherwise the address is called within the `CODE` bank (relative to `BC`) when the interrupt fires. The current `IP` and `ST` registers are pushed onto the stack (pointed to by `SP`), so they can be restored later (by the `IRT` instruction in the default instruction set, which uses the `IRT` microcode). The status flags in `ST` are also cleared, including `I` (so interrupts are disabled while the handler runs). Starting a new program on a core, by resetting its `BC` (from the host application, or with `RST` of `CODE` in the default instruction set), clears its whole vector and its status flags, including `I`, so the program starts with no handlers and interrupts disabled. Its pending interrupts and its trap flag (`T`) are kept.

All 16-bit registers have a well defined association with a memory bank. This is used by the default instruction set for operations that support memory addressing by register (with or without an offset).

| Register | Memory bank association |
| -------- | ----------------------- |
|   `R0`   |         `DATA`          |
|   `R1`   |         `DATA`          |
|   `R2`   |         `DATA`          |
|   `R3`   |         `DATA`          |
|   `R4`   |         `EXTRA`         |
|   `R5`   |         `EXTRA`         |
|   `R6`   |         `STACK`         |
|   `R7`   |         `STACK`         |
|   `BC`   |         `CODE`          |
|   `BS`   |         `STACK`         |
|   `BD`   |         `DATA`          |
|   `BE`   |         `EXTRA`         |
|   `IP`   |         `CODE`          |
|   `SP`   |         `STACK`         |
|   `FP`   |         `STACK`         |
|   `MB`   |         `CODE`          |


### Flags

Each OZ-3 core has several flags stored in the `ST` status and control register. There are two types of flags:

* **Status:** Status flags can be modified by instructions (or by user programs if the configured instruction set exposes that). These have standard meanings, and the underlying OZ-3 microcode and default instruction set operates against these.
* **Control:** Control flags cannot be changed by instructions, or by user programs (no matter what the instruction set). However, they can be read by microcode (and thus user programs, if exposed). Control flags may represent internal control state of the CPU (the `W` flag, for instance), or may be set by external code (the `T` flag is set by the OZ-3 debugger for instance, to step through code and handle breakpoints).

| Bit | Flag | Name      | Type    | Description                                                   |
| --- | ---- | --------- | ------- | ------------------------------------------------------------- |
|  0  |  Z   | Zero      | Status  | Set when an operation results in zero                         |
|  1  |  S   | Sign      | Status  | Set when an operation results in the high bit set             |
|  2  |  C   | Carry     | Status  | Set when an operation causes an unsigned overflow             |
|  3  |  O   | Overflow  | Status  | Set when an operation causes a signed overflow or an error    |
|  4  |  I   | Interrupt | Status  | When set, interrupts are enabled                              |
|  8  |  T   | Trap      | Control | When set, the core automatically halts after each instruction |
|  9  |  W   | Wait      | Control | When set, the core has a WAIT instruction active              |
| 10  |  N   | Notify    | Control | When set, the core's next WAIT ends at once                   |

The `N` flag latches a wake that comes while the core is running. `CRUN` (the default instruction set's `WAKE`) on a running core that has no `WAIT` to end sets it, and the next `WAIT` clears it and ends at once. So a core that asks another for something and then `WAIT`s for the answer gets it, even if the answer comes before the `WAIT` starts. Resetting the core from the host clears it.

### Mailboxes

A port number is a word. Its high byte picks a set of ports, and its low byte a port in the set:

| Port numbers  | Ports                                                                                   |
| ------------- | --------------------------------------------------------------------------------------- |
| 0 to FFh      | The processor's ports, which devices are wired to, up to its configured count           |
| 100h to 107h  | This core's mailboxes *to* cores 0 to 7: what this core writes there, that core reads   |
| 108h to 10Fh  | This core's mailboxes *from* cores 0 to 7: what that core writes, this core reads there |
| 110h to FFFFh | None, left for sets of ports to come                                                    |

A port number that names none of these is a port that doesn't exist, which is never ready (see [Overview](#overview)).

There are 64 mailboxes, one from each core to each core, and each core names them from its own side. So the same port number is a different mailbox on each core, and the same mailbox has a different port number on each of its two cores. On core 2, port 105h is the mailbox from core 2 to core 5, and on core 5, port 10Ah is the same mailbox. A program sends to core `n` on port `100h + n`, and receives from it on `108h + n`, whichever core it runs on. The host application and the debugger see every mailbox, named by its sending and receiving cores.

* **Cores that don't exist:** A mailbox exists only when both of its cores do. On a processor with 2 cores, ports 102h to 107h and 10Ah to 10Fh don't exist, so a word sent to a core that isn't there is never taken, as for a port that doesn't exist.
* **A core's own mailbox:** Core `n`'s mailbox to itself is both port `100h + n` and port `108h + n`, so a core can post words to itself to handle later.
* **Handshake:** A mailbox holds one message: its two words and its status. Written with the `T` and `S` mode flags (`OUTS` in the default instruction set), a message goes in only when the mailbox is empty, and fills it. Read the same way (`INS`), a message comes out only when the mailbox is full, and empties it. A dword moves both words in one go.
* **Contention:** Each direction has its own mailbox, so two cores sending to each other never wait on the same mailbox, and only a mailbox's two cores ever lock it, so other cores never wait on it either.
* **Direction:** Nothing in the hardware makes a mailbox one way. Either of its cores can read or write it, so a sender can take back a message the receiver hasn't taken yet by reading it, but programs normally use each one in one direction only.
* **Signals:** A mailbox raises no interrupt. A core that wants the other core to know about a message, rather than having it check, raises an interrupt on it or wakes it (`INTC` and `WAKE` in the default instruction set).

The mailboxes don't depend on the processor's configured ports: a processor has them however many ports it has, and they aren't counted among them. No device can be wired to one.

## Instruction Set

The OZ-3 core does not have a fixed instruction set. Instead, each OZ-3 core can be configured with either the OZ-3 [default instruction set](2.2-Default-Instruction-Set.md), an extension of the instruction set, or a completely different application-specific instruction set. Instructions within an instruction set are implemented in terms of OZ-3 [microcode assembly](2.1-Microcode.md) which defines the execution engine for the core. 

Instruction sets may be defined explicitly in host application code, or externally via an [instruction set source](2.3-Instruction-Set-Source.md) file. Each instruction set can have up to 256 instructions (one for each value of the code word's high byte), and each instruction can divide its low byte into variants with their own microcode, for up to 65536 distinct code words (lots!). A code word that isn't an instruction in the core's instruction set, or one added by a [coprocessor](#coprocessors), runs as a `NOP`: it takes the 3 cycles to fetch and decode the code word, and does nothing else. The OZ-3 assembler supports all defined syntax for the instruction set and can generate machine code that an OZ-3 core using the same instruction set will be able to run.

## Program Files

Programs are loaded into memory from [program files](2.4-Program-Files.md), the OZ-3's executable format. A program file is made of words rather than bytes, so OZ-3 code can load programs as easily as a host application can.

## Synchronization

The OZ-3 has several cores, coprocessors, and devices all with access to shared resources like memory, ports, CPU cores, and coprocessors. This means there are lots of race conditions and possible synchronization issues, which is not *fun*. To address this, the OZ-3 provides very strong synchronization guarantees at the microcode level (for CPU cores), and host API level (for application code, devices, and coprocessors).

Each operation that uses a shared resource requires a `Lock` object, which guarantees exclusive access to the resource. If multiple pieces of code attempt to lock the same object at the same time, they are automatically queued for access in a FIFO fashion. In microcode, this is done via lock and unlock operations (such as `LK` and `UL`), and only one lock may be held at a time (preventing instruction-level deadlock possibilities). In the host application API (for devices and coprocessor code), it is done explicitly by calling `RequestLock` on an `oz3::Lockable`, or an equivalent function. Locks do not *literally* block the calling code, as they are simulated locks; the calling code must wait until the lock indicates it is locked before using the resource. Destroying the lock allows the resource to be locked by other code. See `oz3/core/lockable.h` for details.

The approach optimizes for ease of understanding over maximizing (virtual) performance in the interest of fun. In fact, due to the heavy synchronization guarantees, it provides an interesting opportunity for programmers (gamers) to find optimal ways to use the shared resources. It is also easy for host applications to design new devices and coprocessors that easily comply with the OZ-3 locking semantics.

### Concurrent memory access

Each core and coprocessor may be configured to be able to access one or more of the available memory data banks. Fetch and store actions are implicitly fully synchronized, such that no two components are accessing the same memory bank at the same time. This is true for the duration the operation requires the memory bank (both reads and writes). Any other cores and coprocessors will be put into a blocked state until the preceding operation is complete for the specified memory bank operation.

As a simple example, a core may execute the `MOV R1, D(1234)` instruction from the default instruction set (copy the 16-bit value at address `1234` in the *DATA* bank to register `R1`). This will result in the following instruction microcode:

| **Microcode** | **Cycles** | **Description**
| ------------- | ---------- | ---------------
| *cpu fetch*   |     ---    | Lock *CODE* memory bank.
| *cpu fetch*   |      1     | Set *CODE* address bus to `IP`.
| *cpu fetch*   |      1     | Read the code word from the *CODE* data bus.
| *cpu decode*  |      1     | Decode the code word, selecting `R1` as the destination register. This also advances `IP` by 2.
| `LD(C2)`      |      1     | Read the 16-bit word containing `1234` from the *CODE* data bus into register `C2`.
| `UL`          |     ---    | Unlock *CODE* memory bank.
| `LK(DATA)`    |     ---    | Lock *DATA* memory bank.
| `ADR(C2)`     |      1     | Set *DATA* address bus to `1234` stored in `C2`.
| `LD(R1)`      |      1     | Read the 16-bit word from the *DATA* data bus into register `R1`.
| `UL`          |     ---    | Unlock *DATA* memory bank.

The total execution time of this instruction is 6 cycles (memory bank locks and unlocks are logically instantaneous). Other cores and coprocessors are blocked from accessing the *CODE* memory bank for the first four cycles and the *DATA* memory bank for the last two cycles. If the OZ-3 core has mapped the *CODE* and *DATA* memory banks to the same physical bank, then other cores and coprocessors may still execute between the "fetch" portion and "load" portion of execution. Whenever a `UL` is executed, the "next in line" will automatically claim the lock and the subsequent `LK` would block the CPU core.

### Concurrent port access

A port is a shared connection between cores, coprocessors, and devices. Coprocessors and devices are mapped to specific ports at construction, and general purpose cores can access all ports, as well as their own [mailboxes](#mailboxes). Each port and mailbox is uniquely lockable. Therefore, the only contention comes from multiple cores attempting to read or write to the same port at the same time, a core attempting to read or write at the same time as a mapped coprocessor or device, or the two cores of a mailbox using it at the same time. Like main memory, all port reads and writes are synchronized. Cores and coprocessors may enter a (likely very brief) blocked state when they read or write a port. There is no synchronization between instructions, however, so it may be important for a core or coprocessor to read/write both the status and value of a port within a single instruction. In the OZ-3 default instruction set, this is done with the `INS` / `OUTS` instructions instead of the `IN` / `OUT` instructions which ignore the port status.

### Interrupts

Coprocessors, cores, and external devices can all trigger interrupts, and so can attempt to trigger an interrupt at the same time. Triggering an interrupt takes no time (in a simulated sense).  It is uniquely triggered per core and it can only be cleared by a core handling it (the core itself, or for a shared interrupt, the core that takes it), so there is no race condition for the actual setting and clearing of the interrupt trigger. However, there are race conditions when triggering an interrupt while an interrupt is in progress. The OZ-3 takes a relatively simple approach to this:

1. *Interrupt trigger:* Any time (0 cycles)
   * Sets the associated bit for the interrupt in the `IT` register (interrupt vector trigger)
   * Duplicate triggers (before handling) of the same interrupt are ignored. This is per core: a trigger is a duplicate on a core where the interrupt is already pending (its `IT` bit is still set), whether or not interrupts are enabled there.
   * Coprocessors and devices trigger an interrupt on every core their line is connected to, and are told which of those cores it was newly triggered on. So a trigger is a duplicate for them only if the interrupt was already pending on all of those cores.
   * A *shared* trigger (which a device's wiring, or the host, chooses) sets the interrupt's shared bit instead, on each of the cores. It is a duplicate on a core where the shared bit is already set, whatever its `IT` bit is.
2. *Interrupt detection:* After each instruction completes (0 cycles)
   * If the interrupt enable flag `I` is not set then control flow continues (interrupts are disabled).
   * If no bits in the `IT` register or the shared bits are set, then control flow continues (there are no interrupts).
   * Continues to interrupt handler mapping (#3)
3. *Interrupt handler mapping:* (0 cycles)
   * The lowest interrupt with its `IT` bit or its shared bit set is found, and both bits are cleared on this core. An interrupt pending both ways is one interrupt.
   * If the handler for that interrupt is not set, the interrupt is dropped on this core alone, so a shared interrupt stays pending on the others:
     * If no other interrupt is pending, then control flow continues normally: a running core starts its next instruction, a waiting core keeps waiting, and an idle core stays idle.
     * If any other interrupt is pending, restart interrupt handler mapping (#3)
   * If the handler is set and the shared bit was set, the shared bit is cleared on every core, so this core takes the interrupt from the others. The first core with a handler and `I` set to reach this step takes it. A running core reaches it after each instruction, but an idle or waiting one every cycle, so an idle core usually wins. Cores that reach it in the same cycle go in index order, so the lowest wins.
   * Continues to interrupt handling (#4)
4. *Interrupt handling:* (3 cycles)
   * The current `IP` and `ST` registers are pushed onto the stack
   * Status flag `I` is cleared. This disables interrupts. The `Z`, `S`, `C`, and `O` status flags are also cleared.
   * Using the OZ-3 default instruction set, the code can then optionally call `EI` and `DI` to enable and disable interrupts for the duration of this interrupt handler.
5. *Interrupt handling end:* An instruction (like `IRT` from the default instruction set) calls the `IRT` microcode (min 6 cycles)
   * The `IRT` (or equivalent) instruction is fetched, and `IRT` microcode is executed.
   * The `ST` status flags (not the control flags) and the `IP` register are popped from the stack.
   * This will restore interrupt handling to what it was before the interrupt began. If the interrupt occurred during a `WAIT`, the `WAIT` resumes, unless it was ended while the interrupt was handled, in which case the `O` flag is set.

## Coprocessors

The OZ-3 core supports additional low-level functionality through coprocessors. Coprocessors run independently of the general purpose cores and provide additional functionality through dedicated reserved opcodes. Each coprocessor has its own separate opcodes which are fetched and decoded by an OZ-3 core, and then passed with their arguments to the coprocessor for execution. Coprocessors may have direct access to shared resources (memory, ports, interrupts). Some standard OZ-3 defined coprocessors are defined here. However, specific virtual computer implementations may also define their own coprocessors, with their own behavior and opcodes.

### DMA processor

The OZ-3 supports a coprocessor core for doing block memory copies of pages between memory banks. It has eight 16-bit DMA request registers, which are mapped 1-to-1 with the general purpose cores of the OZ-3. 

TODO

### Math processor

The OZ-3 also supports a separate math coprocessor that supports basic floating point operations. It also supports higher level operations like faster integer multiply/divide, and standard trig functions (sin, cos, etc.)

TODO

