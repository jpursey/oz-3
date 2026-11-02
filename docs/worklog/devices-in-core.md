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
- Maps leave `MemoryBankConfig`: `SetMemoryMap(map, masks)` is gone, and the
  `MemoryBank` constructor takes the maps over its pages instead, beside its
  config. Only the `Processor` creates banks, and from CL4 it passes each bank
  its devices' maps, so a host never adds a map of its own. Each
  `MemoryMapPages` holds a map, its pages, and its masks, which are the map's
  own, with bit 0 for its first page.
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

So `MemoryBankConfig`, and with it `ProcessorConfig`, is plain data again,
with no pointers in it. Only `MemoryBank`'s tests set a map until CL4. The one
other user of maps is `ProgramLoader`, which loads programs only into pages
that physical memory backs: a mapped page is no longer free for it whatever
the map's masks, as `MemoryBank`'s `GetMemPageMasks()` now leaves mapped pages
out. Its tests of maps can't set one up through a `Processor` until CL4, so
they move there, with a fake device.

### Devices

```
// A keyboard on port 1, raising interrupt 1 on core 0.
auto keyboard = std::make_shared<KeyboardFactory>(
    "Keyboard", /*port=*/1,
    DeviceInterrupt{.interrupt = 1, .cores = 0b0000'0001});

// A screen in page 15 of bank 0, raising interrupt 6 on every core.
auto screen = std::make_shared<ScreenFactory>(
    "Screen", DeviceMemory{.bank = 0, .start_page = 15},
    DeviceInterrupt{.interrupt = 6}, ScreenOptions{.columns = 80, .rows = 24});

ProcessorConfig config = ProcessorConfig::OneCore(instructions);
config.SetPortCount(16).AddDevice(keyboard).AddDevice(screen);
if (absl::Status status = config.Validate(); !status.ok()) { ... }
Processor processor(config);
Screen* screen_device = GetScreen(processor, "Screen");
```

- A derived `DeviceFactory`'s constructor takes its wiring in its own terms (a
  keyboard takes one port and one interrupt), so the wiring always matches
  what the device needs. `DeviceFactory` is an interface: a derived factory
  checks its own rules, such as a screen that fits its pages, then builds its
  `DeviceConfig` in its constructor's body, and returns it from `GetConfig()`.
- `ProcessorConfig::Validate()` checks every `DeviceConfig`: each device has a
  name no other device has, its ports and interrupts exist, and its pages are
  in range and don't overlap another device's. It returns an error naming the
  device, so a host building its machine from data gets an error, not a
  `DCHECK`. The `Processor` `DCHECK`s that its config is valid.
- A `DeviceFactory` is immutable, and shared: the config holds a
  `std::shared_ptr<const DeviceFactory>`, as `CpuCoreConfig` holds its
  instruction set. So `ProcessorConfig` stays copyable, and each `Processor`
  built from it creates its own device from each factory.
- Each device has a name in its `DeviceConfig` ("Keyboard", "Screen 2"),
  which no other device in the config has, and `Processor::GetDevice(name)`
  finds it, so a host doesn't hold onto factories to find its devices. A
  device type may wrap it in a typed lookup (`GetScreen(processor, name)`).
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
  a core holds the bank, the device waits its turn.
- A lock's handoff depends on the order components run in each cycle, cores
  first. A lock a core releases is granted to a device waiting for it, which
  uses it in that same cycle. A lock a device releases is granted to a core
  waiting for it in that cycle too, but after the core ran, so the core uses
  it a cycle later. The same holds between cores, by index, and between
  devices, by the order added. This is as cores already behave, and CL4's
  tests pin it down; making the handoff independent of the order is a backlog
  item (see CL6).

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
  anyone, even one that its mode skips (a `T` load with the status clear).
  Loads count, as a keyboard waits for a core to read its key before it puts
  the next one there. A device using its own port is already awake, so that
  doesn't wake it once it sleeps.
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
| When to run next       | `GetDueCycle`, `GetRunCycle`, `GetNextRunCycle`                | `FakeDevice::GetRunCycles` (cycles it ran at), `GetWakeCycle` (awake)      | `GetDueCycle`     |

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

`GetDueCycle` says when a component is scheduled to run, whether it is awake
(its cycles) or asleep (its deadline, or the cycle after a wake). `GetRunCycle`
would read as when it ran, beside `FakeDevice::GetRunCycles`, the cycles it
did run at. `GetWakeCycle` would be wrong for an awake component.

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
// memory_bank.h
struct MemoryMapPages {
  MemoryMap* map = nullptr;
  MemoryPageRange pages;
  MemoryPageMasks masks;  // The map's own, with bit 0 for its first page.
};

// Constructs a memory bank, with the memory maps over its pages, which must
// not overlap. Each map is given addresses from 0 at its first page, and must
// outlive the MemoryBank.
explicit MemoryBank(const MemoryBankConfig& config = {},
                    absl::Span<const MemoryMapPages> maps = {});

// memory_map.h
class MemoryMap {
 public:
  // Locks the bank this map is in. Valid once the map is attached to a bank.
  std::unique_ptr<Lock> RequestLock();

  // Whether `lock` holds this map's bank, for a derived map's lock-guarded
  // accessors to DCHECK, as MemoryBank and Port do.
  bool IsLocked(const Lock& lock) const;

 protected:
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
- Maps are not part of `MemoryBankConfig`, as a config is data a host builds
  and copies, and a map is a live object one bank uses. The `Processor`, which
  owns both the devices and the banks, joins them when it creates the banks.
- The `MemoryBank` constructor attaches each map it is given, and a map can be
  in only one bank at a time (a `DCHECK`): the bank detaches its maps when it
  is destroyed.
- `Read` and `Write` become protected, as virtuals meant to be overridden are,
  and only the bank calls them.
- `MemoryBank` keeps, for each page, the map that owns it and the map's first
  address, plus bank-wide masks of the mapped pages readable and writable,
  and physical memory masks that leave out mapped pages. Reading or writing a
  page checks one mask bit, as now, and a mapped page subtracts its map's
  first address.
- Overlapping maps, and pages out of range, are a `DCHECK` in the `MemoryBank`
  constructor, which also clamps each map's masks to its pages. For devices,
  `ProcessorConfig::Validate` reports them first, naming the device (CL4).
- The wrap fix: `Read` and `Write` take the starting page from their `address`
  parameter rather than the bank's address register.

**Brittleness:**
- A map must outlive its bank. Every map is a device's, owned by a device
  that the `Processor` owns and destroys after its banks (see below), so no
  host has to keep this rule.
- A map's owner can't forget the lock, as its storage is only reachable with
  one. A map can be in only one bank at a time, which the attach `DCHECK`
  checks, and each processor creates its own devices, so one config still
  builds any number of live processors.

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
  // The bank the device's pages are in, in [0, kMaxMemoryBanks), and its
  // pages (an empty range for none).
  int bank = 0;
  MemoryPageRange pages = {.start_page = 0, .page_count = 0};

  // Which of the device's pages cores can read and write, with bit 0 for its
  // first page. Set by the device's factory, as is its page count.
  MemoryPageMasks masks;
};

struct DeviceConfig {
  // The device's name, which the processor finds it by. Not empty, and unique
  // in its ProcessorConfig.
  std::string name;

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

  // The device's name and wiring, which is the same config for the factory's
  // whole life. ProcessorConfig::Validate checks it.
  virtual const DeviceConfig& GetConfig() const = 0;

  // Creates a device for one Processor, wired as GetConfig(). The device holds
  // its own copy of what it needs, so it doesn't depend on this factory.
  virtual std::unique_ptr<Device> CreateDevice() const = 0;

 protected:
  DeviceFactory() = default;
};
```

- A derived factory is immutable once constructed, so it can be shared
  between configs, and processors.
- `GetConfig()` is virtual, so a derived factory can build its config in its
  constructor's body rather than before its base, at the cost of a `config_`
  member and a one-line override. It is called only by `Validate()` and when
  a `Processor` creates the device, never while it runs.
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

  // The device's name, from its DeviceConfig.
  std::string_view GetName() const;

  // The device's own pages: the map cores read and write them through, and
  // the device locks to use them, or null for a device without pages. The
  // Processor puts it in its bank as it creates the banks, before the device
  // is attached.
  virtual MemoryMap* GetMemoryMap();

  // Component implementation.
  void AttachProcessor(CoreInternal, Processor* processor) final;

  // ExecutionComponent::Execute() stays pure virtual: the device does its
  // work, and advances its cycles by at least one.

 protected:
  // `config` is the config of the factory creating this device.
  explicit Device(DeviceConfig config);

  // The device's ports, as numbered in its DeviceConfig.
  int GetPortCount() const;
  Port& GetPort(int port);
  std::unique_ptr<Lock> LockPort(int port);

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
  const DeviceConfig config_;
  Processor* processor_ = nullptr;
};
```

- Lives in `core`, beside `CpuCore`, as the `Processor` runs it and its
  interface is built from core's ports, banks, and interrupts.
- A device holds its own copy of its wiring, and a derived device is given its
  settings (a screen's size) by its factory's `CreateDevice()`, so a device
  never depends on its factory.
- The accessors are protected, as they are for the device's own use. Each one
  that uses the processor `DCHECK`s that the device is attached.
  `GetMemoryMap` is public, as the `Processor` uses it.
- The port, memory, and interrupt accessors are inline, thin calls to the
  `Processor`, and each port and interrupt is an index into its
  `DeviceConfig`, so a device pays nothing for the indirection beyond what a
  core pays. `RaiseInterrupt` passes the line's cores as wired, as the
  `Processor` ignores cores it doesn't have, and `GetInterruptCores` masks
  them to the processor's cores when asked.

### core: ProcessorConfig and Processor

```
// processor_config.h
// Adds a device, which each Processor created from this config creates from
// `factory`.
ProcessorConfig& AddDevice(std::shared_ptr<const DeviceFactory> factory);
absl::Span<const std::shared_ptr<const DeviceFactory>> GetDevices() const;

// Returns an error if a device's config doesn't fit this one: no name, or a
// name another device has, a port or interrupt that doesn't exist, or pages
// past the end of a bank or overlapping another device's.
absl::Status Validate() const;

// processor.h
// Returns the device with the specified name, or null if this processor has
// none.
Device* GetDevice(std::string_view name);
```

- `ProcessorConfig` stays copyable, so every existing caller is unchanged.
- The `Processor` constructor `DCHECK`s `config.Validate()`. Then it creates
  each device from its factory, creates each bank with the memory maps of the
  devices in it, creates the cores, and attaches the devices last.
- It uses the factories only to create the devices, and keeps just the
  devices. The devices are declared before the banks, so they are destroyed
  after them, and their maps outlive the banks. Locks a device still holds on a
  bank or port are safe, as a `Lockable`'s destructor clears its lock.
- `Execute()` runs the devices after the cores. It doesn't check that a
  device's `Execute()` advanced its cycles: that depends on the device's own
  state, so it is weak, and each device's own tests cover it.
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
  // Wake() needs, and runs one step when the processor reaches GetDueCycle():
  // catches a woken component's cycles up, then calls Execute().
  void AttachClock(CoreInternal, const Cycles* processor_cycles);
  Cycles GetDueCycle() const;
  void Step(CoreInternal);

 protected:
  // Called only by Step().
  virtual void Execute() = 0;

  void AdvanceCycles(Cycles cycles);

  // Called last in Execute(), after any AdvanceCycles(). `max_cycles` must be
  // at least one.
  void Sleep(Cycles max_cycles = kSleepForever);

 private:
  Cycles cycles_ = 0;      // Cycles executed.
  Cycles due_cycle_ = 0;   // When to run next: cycles_, unless asleep.
  const Cycles* clock_ = nullptr;
};
```

- `due_cycle_` is the one value the `Processor` loop compares, for cores and
  devices alike. `AdvanceCycles` keeps it equal to `cycles_`, one extra store.
  `Sleep` sets it to the deadline, and `Wake` to the processor's next cycle if
  that is sooner, but never before the cycle the component fell asleep at (a
  component can advance its cycles, then sleep). So the loop needs no
  `IsAsleep()`: a sleeping component's due cycle is simply later
  (`kSleepForever` is the largest `Cycles`), and the one compare covers both.
- Asleep is `due_cycle_ > cycles_`, as a sleep is at least one cycle, so no
  flag tracks it. `Sleep` is called last, after any `AdvanceCycles`. Nothing
  outside needs to ask, so there is no public `IsAsleep()` until something
  does (the debugger, say).
- `Step` replaces the `Processor`'s direct calls to `Execute()`, which is now
  protected, as nothing else calls it. `Step` sets `cycles_` to `due_cycle_`
  before each, which catches a woken component up and changes nothing for an
  awake one. So the catch-up lives in one place for every component, with no
  branch. `Wake` is inline, as ports call it on every load and store.
  `AdvanceCycles` is the only way cycles move, so they never go backward.
- `AttachClock` is separate from `Component::AttachProcessor`, and non-virtual,
  so no derived class has to remember to call a base version. The `Processor`
  calls both for each core and device. `Wake()` on a component that has no
  clock (not attached to a processor) does nothing.
- Each `Port` holds the `ExecutionComponent`s wired to it, which the
  `Processor` adds with `Port::AddComponent` as it creates each device (once
  per port, even for a device wired to it twice), and `LoadWord` and `StoreWord`
  wake them. `port.h` includes `execution_component.h`, with no cycle.
- Performance: the cores' check in the loop reads `due_cycle_` instead of
  `cycles_`, and `AdvanceCycles` stores both. Measured in CL5: nothing
  measurable.

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

- ~~That an empty device loop costs nothing measurable in `Execute()` (CL3).~~
  Confirmed: see CL3.
- ~~That the cores' loop reading `due_cycle_` costs nothing measurable (CL5).~~
  Confirmed: see CL5.

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

### CL2 [x] core: memory maps over page ranges

Depends on: nothing.

- `memory_bank_config.h/.cc`: `SetMemoryMap` and the map are gone, and
  `MemoryPageRange::GetMask()` replaces the mask math repeated across it.
  Fixed in passing: `MemoryPageMasks::WriteOnly` ignored its mask.
- `memory_bank.h/.cc`: `MemoryMapPages`, and the constructor takes the maps.
  A map for each page, the map's own addresses and masks, mapped pages that
  belong wholly to their map, and the wrap fix. `GetMemPageMasks()` leaves out
  mapped pages, and `GetMapPageMasks()` is in the bank's pages. A bank
  detaches its maps when it is destroyed, so a map is in one bank at a time.
- `program_loader.cc`: free pages come from `GetMemPageMasks()` alone. Its
  two map tests are removed, and come back in CL4.
- `memory_map.h/.cc` (new `.cc`): the map knows its bank, with
  `RequestLock` and `IsLocked`, and `Read` and `Write` become protected. Its
  comment says addresses are the map's own, and the bank calls them under its
  lock.
- Wiki: `2-Specifications.md`, *Overview*, the **Main memory** bullet.

**Verify**
- Standard checks.
- Unit tests (`memory_bank_test.cc`, its map tests moved to the constructor):
  two maps in one bank, each given addresses from 0; a map's read-only and
  write-only pages read 0 and ignore writes, and physical memory under them
  doesn't show through; a read and a write across the boundary of two maps,
  and of a map and physical memory; a read and a write that wrap past `FFFFh`
  into a page with different permissions (the wrap fix).
- Unit tests (`memory_bank_test.cc`): a map's `RequestLock` locks its bank,
  and waits while someone else holds it; `IsLocked` is true only for a lock
  on its own bank; a map's masks are clamped to its pages; a map can be in a
  new bank once its first is destroyed.

### CL3 [x] core: devices

Depends on: CL1.

- New `device_config.h` (`DeviceInterrupt`, `DeviceMemory`, `DeviceConfig`),
  `device_factory.h` (`DeviceFactory`), and `device.h/.cc` (`Device`),
  added to `CMakeLists.txt` with `absl::status`, and `Device` and
  `DeviceFactory` added to `core_types.h`'s forward declarations.
- `processor_config.h/.cc`: `AddDevice`, `GetDevices`, `Validate`, which
  checks names, ports, and interrupts.
- `processor.h/.cc`: creates, attaches, and runs devices, and `GetDevice`,
  which finds a device by name.
- Devices have ports, shared memory, and interrupts. Their own pages come in
  CL4: `DeviceMemory` is in `DeviceConfig`, and `Validate` rejects a device
  with pages until then.
- New `fake_device.h/.cc` in `oz3_core_testing`: `FakeDevice`, which runs a
  step its test gives it and records the cycles it ran at, with its `Device`
  interface public, and `FakeDeviceFactory` for any wiring. It is shared, as
  CL4's `ProgramLoader` tests in `tools` need it too.
- Wiki: `2-Specifications.md`, *Overview*, the **Devices** bullet.

**Verify**
- Standard checks.
- Unit tests (`device_test.cc`), with a fake factory and device, which holds
  what it did (cycles it ran at, words it read and wrote):
  - A factory creates a device for each processor built from a config, and
    `GetDevice` finds each processor's own by name, and null for a name it
    doesn't have. A device keeps working after its factory is gone.
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
  config whichever order it was built in, rejects a device without a name,
  and names the device for a port or interrupt that doesn't exist (negative or
  past the end), and for a name another device has (two factories, or one
  added twice).
- Performance: a tight `ADD`/`MOV`/`JP` loop on one core in Release, before
  and after, with no devices. Measured with a temporary test, not checked in:
  49.2 to 49.4 M cycles a second before, and 49.3 to 49.9 after, so the empty
  device loop costs nothing measurable.

### CL4 [x] core: device pages

Depends on: CL2, CL3.

- `device.h`: `GetMemoryMap`, null by default.
- `device_config.h`: `DeviceMemory` holds its pages as a `MemoryPageRange`,
  empty by default.
- `memory_bank_config.h`: `MemoryPageRange::IsValid`, which `Validate` and
  the `MemoryBank` constructor's `DCHECK` share.
- `processor_config.cc`: `Validate` checks each device's pages fit its bank,
  and don't overlap another device's (by their masks).
- `processor.cc`: creates each bank with the maps of the devices in it, and
  `DCHECK`s that a device has a map exactly when it has pages.
- New `fake_memory_map.h/.cc` in `oz3_core_testing`: `FakeMemoryMap`, moved
  from `memory_bank_test.cc`, pages that hold what is written to them. A
  `FakeDevice` has one if its config gives it pages.
- `base_core_test.h/.cc`: `InitConfig` takes devices, so a test can run cores
  with them.
- `program_loader_test.cc`: its two map tests are back, with a `FakeDevice`,
  and `oz3_tools_test` links `oz3_core_testing`.

**Verify**
- Standard checks.
- Unit tests (`device_test.cc`): the host reads and writes a device's pages
  through the bank, as a core does, at the device's own addresses, honoring
  the device's masks; two devices in one bank; a device without pages leaves
  the bank as configured.
- Unit tests (`device_test.cc`), on timing, with a core: a device that locks
  the free bank and releases it each cycle doesn't delay the core's lock; it
  waits while the core holds the bank, and gets it in the cycle the core
  releases it, seeing the core's store; a core waiting for a bank the device
  holds gets it the cycle after the device releases it, and loads what the
  device wrote under its lock.
- Unit tests (`processor_config_test.cc`): `Validate` names the device for
  pages past the end of the bank, and for pages overlapping another device's.
- Unit tests (`program_loader_test.cc`), back from CL2 with a fake device: a
  device's pages are not free, whether its map is read-only or write-only.

### CL5 [x] core: sleeping components

Depends on: CL3.

- `execution_component.h`: `Sleep`, `Wake`, `AttachClock`,
  `GetDueCycle`, and `Step`.
- `processor.cc`: attaches each component's clock, compares `GetDueCycle()`,
  and calls `Step()`. Adds each device to its ports as it creates it.
- `port.h/.cc`: a port's components, from `AddComponent`, woken by `LoadWord`
  and `StoreWord`.
- `core_types.h`: `kSleepForever`.
- `fake_device.h/.cc`: a step can sleep, or advance its own cycles first, and
  then returns 0. `fake_memory_map.h/.cc`: `SetOnWrite`, so a test can wake
  the map's device from its `Write`.

**Verify**
- Standard checks.
- Unit tests (`execution_component_test.cc`, new), with a fake device run by a
  `Processor`: a sleeping component isn't run; `Wake` runs it at the next
  cycle with its cycles caught up, whether called during another component's
  step before or after it, or between `Execute()` calls (the host's call); a
  deadline wakes it, and a wake before it; a wake in the cycle it is due
  doesn't put it off; `Wake` on an awake component changes nothing; a wake
  never moves cycles backward; `Wake` without a processor does nothing.
- Unit tests (`device_test.cc`): a sleeping device is woken by a core's store
  to its port, by the host's load from it, and by its own map's `Write` from a
  core's store, where it then waits for the core to release the bank; a port
  wakes every device wired to it and no other; a device's store wakes the
  others on its port, but not itself.
- Unit tests: the existing core and instruction set tests pass unchanged, as
  cores never sleep.
- Performance: a tight loop like CL3's (two `ADD.W`s and a `JP`), in
  Release, before and after. Measured with a temporary test, not checked in,
  outside one noisy run: with no devices, 48.4 to 49.4 M cycles a second
  before, and 48.2 to 49.2 after; with one idle device (advanced far ahead
  before, asleep after), 48.6 to 49.0 before, and 47.8 to 49.1 after. So
  sleeping costs nothing measurable.

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
- A new backlog item, *Order-independent lock handoff*: a lock released in a
  cycle can be used in that cycle by whichever component it is granted to,
  whatever order the components run in, so a core waiting behind a device
  (or a lower-index core) no longer loses a cycle. Only if it costs the
  processor loop nothing measurable, as the run order was kept for speed.

**Verify**
- Docs only.
