# Devices in Core

Today the `Processor` runs only its cores, and nothing but a core or the host
uses its ports, memory banks, or interrupts. Core gains **devices**: hardware
outside the OZ-3 (a screen, a keyboard, a disk drive) that runs in step with
the cores, cycle by cycle.

A device is written as two classes: a `DeviceFactory`, which is one configured
device (its wiring to ports, interrupt lines, and pages, plus its own
settings), and the `Device` it creates. The host adds `DeviceFactory`s to the
`ProcessorConfig`, and each `Processor` creates and owns its own devices from
them. A device:
- Uses its ports through their locks, as a core does.
- Owns pages of its own that cores see in a bank, such as a screen's cells,
  with its own address space and its own rules for reading and writing. It
  uses them under its bank's lock, as cores do, so no one sees a change before
  it happens.
- Can lock any memory bank to read and write shared memory, contending with
  the cores, such as a disk drive transferring a file.
- Raises interrupts on the cores its lines are connected to, and learns which
  of them it was raised on, as the Interrupts section of `2-Specifications.md`
  promises.
- Sleeps while it has nothing to do, costing the `Processor` one compare a
  cycle, until its ports, its pages, the host, or a deadline wake it. Sleeping
  belongs to every `ExecutionComponent`, so coprocessors can use it too, and
  cores could later.

No device exists yet: the screen, keyboard, and teletype come next, in the
*Console devices* item, in a new `devices` library.

## Behavior

### Interrupts

- `CpuCore::RaiseInterrupt(interrupt)` returns `true` if the interrupt was
  raised, or `false` if it was already pending on the core (a duplicate).
  "Pending" is its bit being set in `IT`: raised, and not yet taken by the
  core's interrupt handler mapping, whether or not interrupts are enabled.
- `Processor::RaiseInterrupt(interrupt, cores)` gains a mask of the cores to
  raise it on, which defaults to every core (as today). It returns the mask of
  cores it was newly raised on, so 0 is a duplicate on all of them. Bits for
  cores the processor doesn't have are ignored, and are never in the result.
- The `INT` microcode ignores the result, so no instruction changes.

Both returned nothing before, so existing callers are unaffected.

### Memory maps

A memory map now covers a range of pages, and a bank can have several, each on
pages of its own:
- `MemoryBankConfig::AddMemoryMap(map, pages, masks)` replaces
  `SetMemoryMap(map, masks)`. The masks are the map's own, with bit 0 for its
  first page.
- A map is given its own addresses: 0 is the start of its first page, so a
  device's memory works the same wherever it is placed.
- A mapped page belongs wholly to its map. If the map doesn't allow reading
  it, it reads as 0, and if it doesn't allow writing it, writes are ignored.
  The bank's physical memory on that page is hidden, as before, but no longer
  shows through for whichever of reads or writes the map leaves out.
- A map knows the bank it is in, and locks it with `RequestLock()`. What its
  owner does with the map's storage takes the lock, as reading and writing a
  `MemoryBank` or a `Port` does, so the owner never needs to know which bank
  the map is in.
- The bank calls a map's `Read` and `Write` under the bank's lock, so a `Read`
  can have side effects (a register that clears when read): the map's owner
  only sees them once the core has released the bank.
- Fixed in passing: a read or write that wraps past `FFFFh` used the starting
  page's permissions and map for everything after the wrap.

Only `MemoryBank`'s own tests set a map today, so nothing else changes.

### Devices

```
// A keyboard on port 1, raising interrupt 1 on core 0.
auto keyboard = std::make_shared<KeyboardFactory>(
    /*port=*/1, DeviceInterrupt{.interrupt = 1, .cores = 0b0000'0001});

// A screen in page 15 of bank 0, raising interrupt 6 on every core.
auto screen = std::make_shared<ScreenFactory>(
    DeviceMemory{.bank = 0, .start_page = 15}, DeviceInterrupt{.interrupt = 6},
    ScreenOptions{.columns = 80, .rows = 24});

ProcessorConfig config = ProcessorConfig::OneCore(instructions);
config.SetPortCount(16).AddDevice(keyboard).AddDevice(screen);
if (absl::Status status = config.Validate(); !status.ok()) { ... }
Processor processor(config);
Screen* screen_device = screen->GetScreen(processor);
```

- A derived `DeviceFactory`'s constructor takes its wiring in its own terms (a
  keyboard takes one port and one interrupt), so the wiring always matches
  what the device needs. It `DCHECK`s what it can check alone, such as a port
  below `kMaxPorts`, or a screen that fits its pages.
- `ProcessorConfig::Validate()` checks what needs the whole machine: each
  device's ports exist, its pages are in range and don't overlap another
  device's, and no `DeviceFactory` is added twice. It returns an error naming
  the device. The `Processor` `DCHECK`s that its config is valid.
- A `DeviceFactory` is immutable, and shared: the config holds a
  `std::shared_ptr<const DeviceFactory>`, as `CpuCoreConfig` holds its
  instruction set. So `ProcessorConfig` stays copyable, and each `Processor`
  built from it creates its own device from each factory.
- A factory has fixed ports and pages, so a processor has at most one device
  per factory, and the factory finds it: `Processor::GetDevice(factory)`, and
  a derived factory's typed version (`ScreenFactory::GetScreen`).
- Each cycle, the `Processor` runs its cores in index order, then its devices
  in the order they were added. A device runs whenever the processor has
  reached its cycle count, exactly as a core does, so a device that is busy
  for a while (a screen between refreshes) advances its cycles past it, and a
  device waiting for something sleeps (see [Sleeping](#sleeping)). Either way
  it costs one compare a cycle.
- A device sees only what the `Device` base gives it: its ports, its own pages,
  any memory bank, and its interrupts. It doesn't see the cores, other
  devices, or ports it isn't wired to.
- A device keeps the `ExecutionComponent` contract, as a core does: anything
  that depends on others or that others see (taking and releasing locks,
  using its ports and its pages, raising interrupts) happens at its current
  cycle, at the start of an `Execute()`, and then it advances its cycles past
  what it did. A core runs ahead of the processor's clock only while it holds
  a lock, where no one else can look, so this keeps every device and core in
  step.
- A device uses its own pages under its bank's lock, through its map, but
  with no cycles, as if it had its own bus to them. The lock only puts it in
  turn with the cores. Taking the lock while the bank is free, using its
  pages, and releasing it all in one `Execute()` costs the cores nothing. When
  a core holds the bank, the device waits its turn, and the cores behind it
  wait about a cycle.

### Sleeping

An `ExecutionComponent` can end an `Execute()` asleep, and isn't run again
until it is woken. Devices are the first to use it: a keyboard or teletype has
nothing to do until a core uses its port, and would otherwise run every cycle
just to look.

```
// execution_component.h
// Wakes the component if it is asleep, so it runs at the next cycle. Safe to
// call from anywhere. Does nothing if it is awake.
void Wake();

protected:
// Ends this Execute() asleep: the component runs again only once woken, or
// after `max_cycles` if given. Waking catches its cycles up to the
// processor's.
void Sleep(Cycles max_cycles = kSleepForever);
```

What wakes a sleeping component:
- **Its ports:** any `LoadWord` or `StoreWord` on a port it is wired to, by
  anyone. Loads count, as a keyboard waits for a core to read its key before
  it puts the next one there. A device using its own port is already awake.
- **Its pages:** a device's own `MemoryMap` overrides call `Wake()` when it
  matters to the device, such as a write to a doorbell word.
- **The host:** a device's public API calls `Wake()`, between `Execute()`
  calls (`Keyboard::PressKey`).
- **A deadline:** `Sleep(max_cycles)` wakes on its own, for a device that waits
  for a core but gives up, or paces itself (a teletype at a baud rate). A
  component that is only busy for a while still uses `AdvanceCycles`, which
  nothing interrupts.

A woken component runs at the cycle after the wake, whoever woke it: a core,
a device before or after it in the order, or the host between `Execute()`
calls. That is one cycle later than a component polling every cycle would
see it, and it is the same rule for every source. The contract still holds: a
core uses a port while it runs ahead of the clock, so the woken device finds
the port locked, and waits its turn as for any lock. Locks still decide the
order; waking only spares a component the cycles with nothing to do.

While a component sleeps, its `GetCycles()` stays where it fell asleep, and
waking catches it up to the processor, so its cycles never go backward. Cores
never sleep yet: an idle or waiting core still runs every cycle, as today (see
the backlog's *Sleeping cores*, added in the wrap-up).

### Wiki

- `2-Specifications.md`, *Overview*: the **Main memory** bullet says a bank's
  pages can be mapped to several devices or coprocessors, each on its own
  range of pages, rather than one range to a single one. A new **Devices**
  bullet says what a device is and how it runs.
- `2-Specifications.md`, *Interrupts*: step 1 says a raise is a duplicate on
  each core it was already pending on, and a device learns which cores it was
  raised on.

## Design

### Names

| Name                   | Candidates                                                     | Avoids                                                                     | Choice            |
| ---------------------- | -------------------------------------------------------------- | -------------------------------------------------------------------------- | ----------------- |
| The device             | `Device`, `Peripheral`, `Hardware`                             | `Component` (cores and banks too), `Coprocessor` (has opcodes)             | `Device`          |
| What creates a device  | `DeviceFactory`, `DeviceDef`, `DeviceInstance`, `DeviceType`   | the `Device` itself (the instance), Ozzy's `type: "screen"` (the class)    | `DeviceFactory`   |
| Its wiring             | `DeviceConfig`, `DeviceWiring`, `DeviceConnections`            | a device's own settings (a screen's size), Ozzy's config file entries      | `DeviceConfig`    |
| One interrupt's wiring | `DeviceInterrupt`, `InterruptLine`, `InterruptWiring`          | `CpuCore::kInterruptCount` lines, the `INT` microcode                      | `DeviceInterrupt` |
| Its pages' wiring      | `DeviceMemory`, `DevicePages`, `MemoryPlacement`               | `MemoryBank`, `MemoryPageRange` (which it holds), `MemoryMap`              | `DeviceMemory`    |
| Resting until needed   | `Sleep`/`Wake`, `Suspend`/`Resume`, `Park`/`Unpark`            | `CpuCore::State::kIdle` (halted), the `WAIT` instruction                   | `Sleep`/`Wake`    |

`Device` is the spec's word, and the comments in `memory_bank.h` and
`memory_map.h` already use it. `DeviceFactory` says what it does: each
`Processor` calls its `CreateDevice()` for a `unique_ptr` to a device of its
own, which is what "Create" means for a factory. It also holds the device's
wiring and settings, which its class comment says. `DeviceInstance` would
read as the `Device` itself, which is the instance, one per processor.
`DeviceType` would read as the class of device, as Ozzy's config and machine
table use "type". `DeviceDef` would follow `InstructionSetDef`, but reads as
passive data, where this is what creates the device.
`DeviceConfig` matches `CpuCoreConfig` and `MemoryBankConfig`, which also hold
what a component is built with. `InterruptLine` would read as one line of one
core, where a `DeviceInterrupt` is one line across several cores.

`Wake` is close to the `WAKE` instruction, which ends a core's `WAIT`, but
means the same for a component, and the two never meet in code: one is C++,
the other an instruction. `Suspend`/`Resume` would avoid it, but read as
something done to a component, where a component chooses to sleep.

### core: interrupts

```
// cpu_core.h
// Raises the interrupt, which must be in [0, kInterruptCount). Returns false if
// it was already pending (a duplicate).
bool RaiseInterrupt(int interrupt);

// core_types.h
// A mask of cores, one bit for each core index.
inline constexpr uint8_t kAllCores = 0xFF;
static_assert(kMaxCores <= 8);

// processor.h
// Raises the interrupt on the cores in `cores`, ignoring bits for cores the
// processor doesn't have. Returns the cores it was newly raised on, so 0 is a
// duplicate on all of them.
uint8_t RaiseInterrupt(int interrupt, uint8_t cores = kAllCores);
```

The result is a mask of the cores raised on, rather than of the ones missed,
as a missed mask only means something next to which cores exist. A device
that wants the misses has its wiring resolved to the existing cores (see
`Device::GetInterruptCores`).

**Brittleness:** none. The result is computed where each bit is set.

### core: memory maps

```
// memory_bank_config.h
// Adds a memory map over `pages`, which must not overlap another map's.
// `masks` are the map's own, with bit 0 for its first page. The map is given
// addresses from 0 at its first page, and must outlive the MemoryBank.
MemoryBankConfig& AddMemoryMap(MemoryMap* map, MemoryPageRange pages,
                               MemoryPageMasks masks = {});

// memory_map.h
class MemoryMap {
 public:
  // Locks the bank this map is in. Valid once the map is attached to a bank.
  std::unique_ptr<Lock> RequestLock();

 protected:
  // Whether `lock` holds this map's bank, for a derived map's lock-guarded
  // accessors to DCHECK, as MemoryBank and Port do.
  bool IsLocked(const Lock& lock) const;

  // Called by the bank, under its lock, in the map's own addresses.
  virtual void Read(int address, uint16_t* data, int size);
  virtual void Write(int address, const uint16_t* data, int size);

 private:
  friend class MemoryBank;  // Attaches itself, and calls Read and Write.

  MemoryBank* bank_ = nullptr;
};
```

- A derived map takes a `const Lock&` for everything its owner does with its
  storage (`ScreenMemory::GetCells(lock)`), and `DCHECK`s it with `IsLocked`,
  the pattern `Lockable` describes. So the owner can't use its pages without
  the lock.
- The `MemoryBank` constructor attaches each map its config added, and a map
  can be attached to only one bank (a `DCHECK`).
- `Read` and `Write` become protected, as virtuals meant to be overridden are,
  and only the bank calls them.
- `MemoryBank` keeps, for each page, the map that owns it and the map's first
  address, plus bank-wide masks of the pages mapped and of the mapped pages
  readable and writable. Reading or writing a page checks one mask bit, as
  now, and a mapped page subtracts its map's first address.
- Overlapping maps are a `DCHECK` in `AddMemoryMap`, as the rest of
  `MemoryBankConfig` clamps or checks its input there.
- The wrap fix: `Read` and `Write` take the starting page from their `address`
  parameter rather than the bank's address register.

**Brittleness:**
- A map must outlive its bank, as before. Device maps are owned by devices
  that the `Processor` owns and destroys after its banks (see below), so only
  a host that adds a map of its own has to keep this rule.
- A map's owner can't forget the lock, as its storage is only reachable with
  one. A host's own map can be in only one bank, so a `ProcessorConfig` with
  one builds only one `Processor`, which the attach `DCHECK` catches. Device
  maps don't have this limit, as each processor creates its own devices.

### core: DeviceConfig

```
// device_config.h
struct DeviceInterrupt {
  // The interrupt line, in [0, CpuCore::kInterruptCount).
  int interrupt = 0;

  // The cores the line is connected to, one bit per core index. Bits for cores
  // the processor doesn't have are ignored.
  uint8_t cores = kAllCores;
};

struct DeviceMemory {
  // Where the device's pages start: a bank in [0, kMaxMemoryBanks), and its
  // first page.
  int bank = 0;
  int start_page = 0;

  // How many pages the device has (0 for none), and which of them cores can
  // read and write, with bit 0 for its first page. Set by the device's factory.
  int page_count = 0;
  MemoryPageMasks masks;
};

struct DeviceConfig {
  // The processor port for each of the device's ports.
  std::vector<int> ports;

  // The wiring of each of the device's interrupts.
  std::vector<DeviceInterrupt> interrupts;

  // The device's own pages.
  DeviceMemory memory;
};
```

A derived factory builds its `DeviceConfig` from its constructor's arguments,
and fills in the parts that are the device's to decide (its page count and
masks). Two devices may share a port or an interrupt line, as the spec
allows; sharing a line is how two devices' raises merge. Pages can't be
shared, as each page has one map.

### core: DeviceFactory

```
// device_factory.h
// Creates a device for each Processor, and holds what it is created with: its
// wiring, and the settings of a derived factory (a screen's size).
class DeviceFactory {
 public:
  DeviceFactory(const DeviceFactory&) = delete;
  DeviceFactory& operator=(const DeviceFactory&) = delete;
  virtual ~DeviceFactory();

  // The device's name, such as "Keyboard", for errors and the debugger.
  std::string_view GetName() const;

  // The device's wiring.
  const DeviceConfig& GetConfig() const;

  // Creates a device for one Processor, which keeps this factory alive as long
  // as the device.
  virtual std::unique_ptr<Device> CreateDevice() const = 0;

 protected:
  DeviceFactory(std::string_view name, DeviceConfig config);

  // Returns the device `processor` created from this factory, or null if it
  // has none. For a derived factory's typed lookup.
  Device* GetDevice(Processor& processor) const;
};
```

- A derived factory is immutable once constructed, so it can be shared
  between configs, and processors.
- `CreateDevice()` does nothing with the processor; the device is attached
  after the processor has created its banks and cores.

### core: Device

```
// device.h
class Device : public ExecutionComponent {
 public:
  Device(const Device&) = delete;
  Device& operator=(const Device&) = delete;
  ~Device() override;

  // Component implementation.
  void AttachProcessor(CoreInternal, Processor* processor) final;

  // ExecutionComponent::Execute() stays pure virtual: the device does its
  // work, and advances its cycles by at least one.

 protected:
  // `factory` is the factory creating this device.
  explicit Device(const DeviceFactory* factory);

  const DeviceFactory* GetFactory() const;

  // The device's ports, as numbered in its DeviceConfig.
  int GetPortCount() const;
  Port& GetPort(int port);
  std::unique_ptr<Lock> LockPort(int port);

  // The device's own pages: the map cores read and write them through, and
  // the device locks to use them, or null for a device without pages. Called
  // once, before the device is attached.
  virtual MemoryMap* GetMemoryMap();

  // Any memory bank, in [0, kMaxMemoryBanks), to lock and use as shared
  // memory.
  MemoryBank* GetMemory(int bank);

  // The device's interrupts, as numbered in its DeviceConfig. The cores an
  // interrupt's line is connected to, among those the processor has, and
  // raising it on them, returning the cores it was newly raised on.
  int GetInterruptCount() const;
  uint8_t GetInterruptCores(int interrupt) const;
  uint8_t RaiseInterrupt(int interrupt);

 private:
  friend class Processor;  // For GetMemoryMap().

  const DeviceFactory* const factory_;
  Processor* processor_ = nullptr;
  std::vector<uint8_t> interrupt_cores_;  // Resolved when attached.
};
```

- Lives in `core`, beside `CpuCore`, as the `Processor` runs it and its
  interface is built from core's ports, banks, and interrupts.
- A derived device keeps its own typed pointer to its factory, from its
  constructor, for its settings (a screen's size).
- The accessors are protected, as they are for the device's own use. Each one
  `DCHECK`s that the device is attached.
- The port, memory, and interrupt accessors are thin calls to the `Processor`,
  and each port and interrupt is a vector index, so a device pays nothing for
  the indirection beyond what a core pays.

### core: ProcessorConfig and Processor

```
// processor_config.h
// Adds a device, which each Processor created from this config creates from
// `factory`.
ProcessorConfig& AddDevice(std::shared_ptr<const DeviceFactory> factory);
absl::Span<const std::shared_ptr<const DeviceFactory>> GetDevices() const;

// Returns an error if a device's wiring doesn't fit this config: a port it
// doesn't have, pages past the end of a bank or overlapping another device's,
// or a factory added twice.
absl::Status Validate() const;

// processor.h
// Returns the device created from `factory`, or null if this processor has
// none. See DeviceFactory::GetDevice for a typed lookup.
Device* GetDevice(const DeviceFactory& factory);
```

- `ProcessorConfig` stays copyable, so every existing caller is unchanged.
- The `Processor` constructor `DCHECK`s `config.Validate()`. Then it creates
  each device from its factory, adds each device's memory map to a copy of its
  bank's `MemoryBankConfig`, creates the banks and cores, and attaches the
  devices last.
- It keeps each factory with its device, so the factory lives as long as the
  device. The devices are declared before the banks, so they are destroyed
  after them, and their maps outlive the banks. Locks a device still holds on a
  bank or port are safe, as a `Lockable`'s destructor clears its lock.
- `Execute()` runs the devices after the cores, and `DCHECK`s that each
  device's `Execute()` advanced its cycles, as one that doesn't would run every
  cycle with its clock stopped. The check moves into `ExecutionComponent`'s
  step in CL5, where it covers cores too.
- `Component::AttachProcessor`'s comment still holds: a device is owned solely
  by its processor.
- `oz3_core` gains `absl::status`, for `Validate()`.
- Performance: with no devices, `Execute()` gains an empty loop per cycle,
  which isn't expected to be measurable. CL3 measures a tight loop in Release
  against the ~46 M cycles a second in [ozzy_design.md](../ozzy_design.md).

**Brittleness:**
- A derived factory's constructor makes its wiring, so its wiring can't
  disagree with what its device needs.
- Machine-wide wiring is checked by `Validate()` as a whole, so it doesn't
  depend on the order the config was built in (`SetPortCount` before or after
  `AddDevice`). A host that builds a config in code and skips `Validate()` is
  caught by the `Processor`'s `DCHECK`.
- `GetMemoryMap()` is called once, before banks exist, so a device must have
  its map ready when constructed. The `Processor` `DCHECK`s that a device with
  pages returns one, and one without returns null.

### core: sleeping components

```
// execution_component.h
class ExecutionComponent : public Component {
 public:
  Cycles GetCycles() const;

  // Wakes the component if it is asleep (see Behavior).
  void Wake();

  // For the Processor: gives the component the processor's cycle count, which
  // Wake() needs, and runs one step when the processor reaches GetRunCycle():
  // catches a woken component's cycles up, then calls Execute(), and DCHECKs
  // that it advanced its cycles or went to sleep.
  void AttachClock(CoreInternal, const Cycles* processor_cycles);
  Cycles GetRunCycle() const;
  void Step(CoreInternal);

  virtual void Execute() = 0;

 protected:
  void AdvanceCycles(Cycles cycles);
  void Sleep(Cycles max_cycles = kSleepForever);

 private:
  Cycles cycles_ = 0;      // Cycles executed.
  Cycles run_cycle_ = 0;   // When to run next: cycles_, unless asleep.
  bool asleep_ = false;
  const Cycles* clock_ = nullptr;
};
```

- `run_cycle_` is the one value the `Processor` loop compares, for cores and
  devices alike. `AdvanceCycles` keeps it equal to `cycles_`, one extra store.
  `Sleep` sets it to the deadline, and `Wake` to the processor's next cycle if
  that is sooner. So the loop needs no `IsAsleep()`: a sleeping component's
  run cycle is simply later (`kSleepForever` is the largest `Cycles`), and
  the one compare covers both. `asleep_` is only for `Step`, to know to catch
  the cycles up. Nothing outside needs to ask, so there is no public
  `IsAsleep()` until something does (the debugger, say).
- `Step` replaces the `Processor`'s direct calls to `Execute()`, so the
  catch-up and the `DCHECK` live in one place for every component.
  `AdvanceCycles` is the only way cycles move, so they never go backward.
- `AttachClock` is separate from `Component::AttachProcessor`, and non-virtual,
  so no derived class has to remember to call a base version. The `Processor`
  calls both for each core and device. `Wake()` on a component that has no
  clock (not attached to a processor) does nothing.
- Each `Port` holds the `ExecutionComponent`s wired to it, which the
  `Processor` sets when it attaches devices, and `LoadWord` and `StoreWord`
  wake them. `port.h` includes `execution_component.h`, with no cycle.
- Performance: the cores' check in the loop reads `run_cycle_` instead of
  `cycles_`, and `AdvanceCycles` stores both. CL5 measures this in Release,
  with no devices and with one sleeping device.

**Brittleness:**
- A component can't fall asleep and miss its own work: `Sleep` only ends the
  current `Execute()`, and whatever woke it is still there to see when it
  runs.
- A source that changes what a sleeping component would see must wake it.
  Ports do this themselves. A device's map and its host-facing API call
  `Wake()` themselves, in the device's own code, which is the code that knows
  what matters to it. A device that forgets still runs at its deadline, if it
  gave one, and otherwise sleeps until a port wakes it, which its tests show.

### To confirm

- That an empty device loop costs nothing measurable in `Execute()` (CL3).
- That the cores' loop reading `run_cycle_` costs nothing measurable (CL5).

## CLs

### CL1 [x] core: interrupts report the cores they were raised on

Depends on: nothing.

- `cpu_core.h/.cc`: `RaiseInterrupt` returns whether it was raised.
- `core_types.h`: `kAllCores`.
- `processor.h/.cc`: `RaiseInterrupt(interrupt, cores)`, returning the cores it
  was newly raised on.
- Wiki: `2-Specifications.md`, *Interrupts*, step 1.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests (`cpu_core_test.cc`): a raise returns true, a second raise before
  the core takes it returns false (with interrupts enabled and disabled), and
  a raise after the core takes it (handled, or dropped with no handler)
  returns true again.
- Unit tests (`processor_test.cc`): raising on a subset of cores sets only
  their `IT` bits; the result has only the cores it wasn't already pending on;
  bits for missing cores are ignored and never in the result; a mask with no
  existing cores returns 0.

### CL2 [ ] core: memory maps over page ranges

Depends on: nothing.

- `memory_bank_config.h/.cc`: `AddMemoryMap` replaces `SetMemoryMap`.
- `memory_bank.h/.cc`: a map for each page, the map's own addresses and
  masks, mapped pages that belong wholly to their map, and the wrap fix.
- `memory_map.h/.cc` (new `.cc`): the map knows its bank, with
  `RequestLock` and `IsLocked`, and `Read` and `Write` become protected. Its
  comment says addresses are the map's own, and the bank calls them under its
  lock.
- Wiki: `2-Specifications.md`, *Overview*, the **Main memory** bullet.

**Verify**
- Standard checks.
- Unit tests (`memory_bank_test.cc`, its map tests moved to `AddMemoryMap`):
  two maps in one bank, each given addresses from 0; a map's read-only and
  write-only pages read 0 and ignore writes, and physical memory under them
  doesn't show through; a read and a write across the boundary of two maps,
  and of a map and physical memory; a read and a write that wrap past `FFFFh`
  into a page with different permissions (the wrap fix).
- Unit tests (`memory_bank_test.cc`): a map's `RequestLock` locks its bank,
  and waits while someone else holds it; `IsLocked` is true only for a lock
  on its own bank.

### CL3 [ ] core: devices

Depends on: CL1.

- New `device_config.h` (`DeviceInterrupt`, `DeviceMemory`, `DeviceConfig`),
  `device_factory.h/.cc` (`DeviceFactory`), and `device.h/.cc` (`Device`),
  added to `CMakeLists.txt` with `absl::status`, and `Device` and
  `DeviceFactory` added to `core_types.h`'s forward declarations.
- `processor_config.h/.cc`: `AddDevice`, `GetDevices`, `Validate`, which
  checks ports and factories added twice.
- `processor.h/.cc`: creates, attaches, and runs devices, and `GetDevice`.
- Devices have ports, shared memory, and interrupts. Their own pages come in
  CL4: `DeviceMemory` is in `DeviceConfig`, and `Validate` rejects a device
  with pages until then.
- Wiki: `2-Specifications.md`, *Overview*, the **Devices** bullet.

**Verify**
- Standard checks.
- Unit tests (`device_test.cc`), with a fake factory and device, which holds
  what it did (cycles it ran at, words it read and wrote):
  - A factory creates a device for each processor built from a config, and
    `GetDevice` finds each processor's own, and null for a factory it doesn't
    have.
  - It runs in step with the cores: a device advancing 3 cycles a step runs at
    cycles 0, 3, 6, and 9 in `Execute(10)`, and two devices run in the order
    added, after the cores.
  - Its port 0 is the processor port it is wired to: what it stores, the host
    loads from that port, and the reverse. It waits for a port the host holds
    locked, and gets it once the host lets go.
  - It locks a bank as shared memory, waits while the host holds it, and its
    reads and writes cost it the bank's cycles.
  - Its interrupts raise on the cores they are wired to, among those that
    exist, returning the cores newly raised on, and two devices sharing a line
    merge their raises.
- Unit tests (`processor_config_test.cc`, new): `Validate` accepts a valid
  config whichever order it was built in, and names the device for a port it
  doesn't have and a factory added twice.
- Performance: a tight `ADD`/`MOV`/`JP` loop on one core in Release, before
  and after, with no devices.

### CL4 [ ] core: device pages

Depends on: CL2, CL3.

- `device.h/.cc`: `GetMemoryMap`.
- `processor_config.cc`: `Validate` checks each device's pages fit its bank,
  and don't overlap another device's.
- `processor.cc`: adds each device's map to its bank's config.

**Verify**
- Standard checks.
- Unit tests (`device_test.cc`): a core reads and writes a device's pages
  through the bank, at the device's own addresses, honoring the device's
  masks; two devices in one bank; a device without pages leaves the bank as
  configured.
- Unit tests (`device_test.cc`), on timing: the device's next lock sees a
  core's write; the device waits for the lock while a core holds the bank,
  and gets it in the cycle the core releases it; a device that locks a free
  bank and releases it in one `Execute()` doesn't delay a core's next lock;
  the next core to lock the bank sees a write the device made under its lock.
- Unit tests (`processor_config_test.cc`): `Validate` names the device for
  pages past the end of the bank, and for pages overlapping another device's.

### CL5 [ ] core: sleeping components

Depends on: CL3.

- `execution_component.h/.cc` (new `.cc`): `Sleep`, `Wake`, `AttachClock`,
  `GetRunCycle`, and `Step`, and the `DCHECK` moved from the `Processor`.
- `processor.cc`: attaches each component's clock, compares `GetRunCycle()`,
  and calls `Step()`. Sets each port's components when it attaches devices.
- `port.h/.cc`: a port's components, woken by `LoadWord` and `StoreWord`.
- `core_types.h`: `kSleepForever`.

**Verify**
- Standard checks.
- Unit tests (`execution_component_test.cc`, new), with a fake component run
  by a `Processor`: a sleeping component isn't run; `Wake` runs it at the next
  cycle with its cycles caught up, whether called during a core's step,
  another component's step before or after it, or between `Execute()` calls;
  a deadline wakes it; `Wake` on an awake component changes nothing.
- Unit tests (`device_test.cc`): a sleeping device is woken by a core's store
  to its port and by the host's load from it, by its own map's `Write`, and by
  a call from the host; two devices on one port both wake.
- Unit tests: the existing core and instruction set tests pass unchanged, as
  cores never sleep.
- Performance: the tight loop from CL3, in Release, before and after, with no
  devices and with one sleeping device.

### CL6 [ ] Wrap-up: summary

Depends on: CL4, CL5.

- This doc's plan becomes a summary of what was built.
- [ozzy_design.md](../ozzy_design.md)'s *Device support in core* matches what
  was built: devices from `DeviceFactory`s, owned by the `Processor`, pages
  used under their bank's lock, and raises that report the cores they
  reached. Its *Screen* section no longer says the screen never contends for
  its bank: it takes a short lock at each refresh.
- The backlog's *Coprocessors* item still points at this one for duplicate
  reporting, which is now done; it names `Processor::RaiseInterrupt` instead,
  and says coprocessors can sleep.
- A new backlog item, *Sleeping cores*: an idle core sleeps until an
  interrupt, `CRUN`, or `Reset` wakes it, and a waiting core also until its
  wait ends, so a machine of mostly idle cores costs little to run.

**Verify**
- Docs only.
