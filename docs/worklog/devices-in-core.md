# Devices in Core

The `Processor` runs **devices** beside its cores: hardware outside the OZ-3 (a
screen, a keyboard, a disk drive) that runs in step with the cores, cycle by
cycle.

A device is written as two classes: a `DeviceFactory`, which is one configured
device (its wiring to ports, interrupt lines, and pages, plus its own
settings), and the `Device` it creates. The host adds `DeviceFactory`s to the
`ProcessorConfig`, and each `Processor` creates and owns its own devices from
them. A device:
- Uses its ports through their locks, as a core does.
- Owns pages of its own that cores see in a bank, such as a screen's cells,
  with its own addresses and its own rules for reading and writing. It uses
  them under its bank's lock, as cores do, so no one sees a change before it
  happens.
- Can lock any memory bank to read and write shared memory, contending with
  the cores, such as a disk drive transferring a file.
- Raises interrupts on the cores its lines are connected to, and learns which
  of them it was raised on, as the Interrupts section of `2-Specifications.md`
  promises.
- Sleeps while it has nothing to do, costing the `Processor` one compare a
  cycle, until its ports, its pages, the host, or a deadline wake it. Sleeping
  belongs to every `ExecutionComponent`, so coprocessors can use it too, and
  cores could later (the backlog's *Sleeping cores*).

No real device exists yet: the screen, keyboard, and teletype are the
backlog's *Console devices*, in a new `devices` library. Core's own tests use a
fake device (see [Testing](#core-testing)).

## Behavior

### Interrupts

- `CpuCore::RaiseInterrupt(interrupt)` returns `true` if the interrupt was
  raised, or `false` if it was already pending on the core (a duplicate).
  "Pending" is its bit being set in `IT`: raised, and not yet taken by the
  core's interrupt handler mapping, whether or not interrupts are enabled.
- `Processor::RaiseInterrupt(interrupt, cores)` takes a mask of the cores to
  raise it on, which defaults to every core. It returns the mask of cores it
  was newly raised on, so 0 is a duplicate on all of them. Bits for cores the
  processor doesn't have are ignored, and are never in the result.
- The `INT` microcode ignores the result, so no instruction changed.

Both returned nothing before, so no existing caller changed.

### Memory maps

A memory map covers a range of pages, and a bank can have several, each on
pages of its own:
- Maps aren't part of `MemoryBankConfig`, so it, and with it
  `ProcessorConfig`, is plain data with no pointers in it. The `MemoryBank`
  constructor takes the maps over its pages beside its config, each as a
  `MemoryMapPages`: a map, its pages, and its masks, which are the map's own,
  with bit 0 for its first page. Only the `Processor` creates banks, and it
  passes each bank its devices' maps, so a host never adds a map of its own.
- A map is given its own addresses: 0 is the start of its first page, so a
  device's memory works the same wherever it is placed.
- A mapped page belongs wholly to its map. If the map doesn't allow reading
  it, it reads as 0, and if it doesn't allow writing it, writes are ignored.
  The bank's physical memory on that page is hidden, and never shows through.
- A map knows the bank it is in, and locks it with `RequestLock()`. What its
  owner does with the map's storage takes the lock, as reading and writing a
  `MemoryBank` or a `Port` does, so the owner never needs to know which bank
  the map is in.
- The bank calls a map's `Read` and `Write` under the bank's lock, so a `Read`
  can have side effects (a register that clears when read): the map's owner
  only sees them once the core has released the bank.
- A read or write that wraps past `FFFFh` uses each page's own permissions and
  map. Before, it used the starting page's for everything after the wrap.
- `ProgramLoader` loads programs only into pages that physical memory backs: a
  mapped page is never free for it, whatever the map's masks, as
  `MemoryBank::GetMemPageMasks()` leaves mapped pages out.
  `GetMapPageMasks()` has them.

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
  reached its due cycle, exactly as a core does, so a device that is busy for
  a while (a screen between refreshes) advances its cycles past it, and a
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
  devices, by the order added. This is as cores already behaved, and
  `device_test.cc`'s timing tests pin it down. Making the handoff independent
  of the order is the backlog's *Order-independent lock handoff*.

### Sleeping

An `ExecutionComponent` can end an `Execute()` asleep, and isn't run again
until it is woken. Devices are the first to use it: a keyboard or teletype has
nothing to do until a core uses its port, and would otherwise run every cycle
just to look.

```
// execution_component.h
// Wakes the component if it is asleep, so it runs at the next cycle. Safe to
// call from anywhere. Does nothing if it is awake, or not attached to a
// processor.
void Wake();

protected:
// Ends this Execute() asleep: the component runs again only once woken, or
// after `max_cycles` (which must be at least one) if it is given. Called last
// in Execute(), after any AdvanceCycles().
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
never sleep yet: an idle or waiting core still runs every cycle (see the
backlog's *Sleeping cores*).

### Wiki

- `2-Specifications.md`, *Overview*: the **Main memory** bullet says a bank's
  pages can be mapped to several devices or coprocessors, each on its own
  range of pages, rather than one range to a single one. The **Devices**
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
`memory_map.h` already used it. `DeviceFactory` says what it does: each
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
// A mask of cores, with one bit for each core index, that names every core.
static_assert(kMaxCores <= 8, "Core masks are 8 bits");
inline constexpr uint8_t kAllCores = static_cast<uint8_t>((1 << kMaxCores) - 1);

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
  // Locks the bank this map is in. The map must be in a bank.
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
- `Read` and `Write` are protected, as virtuals meant to be overridden are,
  and only the bank calls them.
- `MemoryBank` keeps, for each page, the map that owns it and the map's first
  address, plus bank-wide masks of the mapped pages readable and writable,
  and physical memory masks that leave out mapped pages. Reading or writing a
  page checks one mask bit, and a mapped page subtracts its map's first
  address. `MemoryPageRange::GetMask()` and `IsValid()` are shared by the
  bank, its config, and `ProcessorConfig::Validate`.
- Overlapping maps, and pages out of range, are a `DCHECK` in the `MemoryBank`
  constructor, which also clamps each map's masks to its pages. For devices,
  `ProcessorConfig::Validate` reports them first, naming the device.
- `Read` and `Write` take the starting page from their `address` parameter
  rather than the bank's address register, which is the wrap fix.

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
  virtual ~DeviceFactory() = default;

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
  ~Device() override = default;

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
  // work, and advances its cycles by at least one, or sleeps.

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
// that don't fit in their bank or overlap another device's.
absl::Status Validate() const;

// processor.h
// Returns the device with the specified name, or null if this processor has
// none.
Device* GetDevice(std::string_view name);
```

- `ProcessorConfig` stays copyable, so every existing caller is unchanged.
  `oz3_core` links `absl::status`, for `Validate()`.
- The `Processor` constructor `DCHECK`s `config.Validate()`. Then it creates
  each device from its factory and adds it to its ports, creates each bank
  with the memory maps of the devices in it, creates the cores, and attaches
  the devices last. It `DCHECK`s that a device has a map exactly when it has
  pages.
- It uses the factories only to create the devices, and keeps just the
  devices. The devices are declared before the banks, so they are destroyed
  after them, and their maps outlive the banks. Locks a device still holds on a
  bank or port are safe, as a `Lockable`'s destructor clears its lock.
- `Execute()` runs the devices after the cores. It doesn't check that a
  device's `Execute()` advanced its cycles or slept: that depends on the
  device's own state, so it is weak, and each device's own tests cover it.
- `Component::AttachProcessor`'s comment still holds: a device is owned solely
  by its processor.
- Performance: with no devices, `Execute()` gains an empty loop per cycle. A
  tight loop on one core in Release ran at 49.2 to 49.4 M cycles a second
  before, and 49.3 to 49.9 after, so it costs nothing measurable (measured
  with a temporary test, not checked in).

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

// port.h
// Wires a component to this port, so LoadWord and StoreWord wake it.
void AddComponent(CoreInternal, ExecutionComponent* component);
```

- `due_cycle_` is the one value the `Processor` loop compares, for cores and
  devices alike. `AdvanceCycles` keeps it equal to `cycles_`, one extra store.
  `Sleep` sets it to the deadline, and `Wake` to the processor's next cycle if
  that is sooner, but never before the cycle the component fell asleep at (a
  component can advance its cycles, then sleep). So the loop needs no
  `IsAsleep()`: a sleeping component's due cycle is simply later
  (`kSleepForever`, in `core_types.h`, is the largest `Cycles`), and the one
  compare covers both.
- Asleep is `due_cycle_ > cycles_`, as a sleep is at least one cycle, so no
  flag tracks it. Nothing outside needs to ask, so there is no public
  `IsAsleep()` until something does (the debugger, say).
- `Step` replaced the `Processor`'s direct calls to `Execute()`, which is
  protected, in `CpuCore` too, as nothing else calls it. `Step` sets `cycles_`
  to `due_cycle_` before each, which catches a woken component up and changes
  nothing for an awake one. So the catch-up lives in one place for every
  component, with no branch. `Wake`, `Sleep`, and `Step` are inline, as ports
  call `Wake` on every load and store. `AdvanceCycles` is the only way cycles
  move, so they never go backward.
- `AttachClock` is separate from `Component::AttachProcessor`, and non-virtual,
  so no derived class has to remember to call a base version. The `Processor`
  calls both for each core and device. `Wake()` on a component that has no
  clock (not attached to a processor) does nothing.
- Each `Port` holds the `ExecutionComponent`s wired to it, which the
  `Processor` adds with `Port::AddComponent` as it creates each device (once
  per port, even for a device wired to it twice), and `LoadWord` and `StoreWord`
  wake them. `port.h` includes `execution_component.h`, with no cycle.
- Performance: the cores' check in the loop reads `due_cycle_` instead of
  `cycles_`, and `AdvanceCycles` stores both. A tight loop on one core in
  Release (two `ADD.W`s and a `JP`), measured with a temporary test outside one
  noisy run: with no devices, 48.4 to 49.4 M cycles a second before, and 48.2
  to 49.2 after; with one idle device (advanced far ahead before, asleep
  after), 48.6 to 49.0 before, and 47.8 to 49.1 after. So sleeping costs
  nothing measurable.

**Brittleness:**
- A component can't fall asleep and miss its own work: `Sleep` only ends the
  current `Execute()`, and whatever woke it is still there to see when it
  runs.
- A source that changes what a sleeping component would see must wake it.
  Ports do this themselves. A device's map and its host-facing API call
  `Wake()` themselves, in the device's own code, which is the code that knows
  what matters to it. A device that forgets still runs at its deadline, if it
  gave one, and otherwise sleeps until a port wakes it, which its tests show.

### core: testing

`oz3_core_testing` has what a device's tests, and tests of other code with
devices, build on:
- `FakeDevice` (`fake_device.h`) runs whatever step a test gives it, and
  records the cycles it ran at (`GetRunCycles()`). A step returns the cycles
  it took, or advances its cycles or sleeps itself and returns 0
  (`FakeDevice::SleepForever` is a step that sleeps). Its `Device` interface,
  `AdvanceCycles`, and `Sleep` are public, so a step or the test can use them
  directly. It checks that each step advanced its cycles or slept.
- `FakeDeviceFactory` creates a `FakeDevice` with any name and wiring, and
  `GetFakeDevice(processor, name)` finds one.
- `FakeMemoryMap` (`fake_memory_map.h`) is pages that hold what is written to
  them, which a `FakeDevice` has if its config gives it pages. `SetOnWrite`
  runs a function after each write, such as to wake its device.
- `BaseCoreTest::InitConfig` takes devices, so a test can run cores with them.
  `oz3_tools_test` links `oz3_core_testing`, for `ProgramLoader`'s tests of
  device pages.

The tests are in `device_test.cc` (devices, their pages, timing with cores,
and what wakes them), `execution_component_test.cc` (sleeping and waking),
`processor_config_test.cc` (`Validate`), `memory_bank_test.cc` (maps), and
`cpu_core_test.cc` and `processor_test.cc` (interrupts).
