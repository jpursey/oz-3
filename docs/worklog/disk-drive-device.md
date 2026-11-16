# Disk drive device

The smart drive from [disk_design.md](../disk_design.md) (Drive device), built
on the disk library ([disk-library.md](disk-library.md)). The disk library is
a complete file system, but only host code can call it. The drive makes it
usable by a program. A drive has three ports and an interrupt: the OS writes
commands to the command port, moves file data through the data port, and
watches or resets the drive through the control port. The drive runs each
command on its `DiskFileSystem` and charges the disk's busy time in cycles.

Any processor can have drives, as with the other devices. The host attaches and
detaches images while the machine runs. The Ozzy's four drives are the next
item (*Ozzy drives*), so nothing in the Ozzy changes here.

```
absl::StatusOr<std::unique_ptr<DiskDriveFactory>> factory =
    DiskDriveFactory::Create("Drive", {.port = 16,
                                       .interrupt = {.interrupt = 2}});
ProcessorConfig config = {..., .devices = {*std::move(factory)}};
auto processor = std::make_unique<Processor>(config);
auto* drive = static_cast<DiskDrive*>(processor->GetDevice("Drive"));
drive->SetClock([] { return HostNow(); });
drive->AttachImage(*DiskImage::Open(&host, "local:/work.ozd"));
processor->Execute(...);  // Programs open, read, and write files.
processor.reset();        // The drive writes back what it hasn't committed.
```

## Behavior

### What programs see

The drive behaves as disk_design.md's Drive device section describes: the
configuration and timing presets, memory, committing, timing, images and disk
changes, the ports, the command protocol and aborts, every command, reading and
writing with early completion and packed transfers, and the status codes. This
plan doesn't repeat them. It settles the points below, and updates
disk_design.md to match.

- **Only reset aborts.** disk_design.md also let a command word abort the
  command in progress in phases 2, 4, and 5. But that worked only in some
  phases, so an OS couldn't count on it, and reset already does the same in
  every phase. Now the drive reads the command port only while it waits for a
  command's words, in phases 0 and 1. A word the OS writes in phase 2, 4, or 5
  is lost when the drive puts its completion's status over it. An OS cuts a
  failed transfer short with a reset.
- **The drive is busy while the disk is.** The drive does a command's disk
  work by calling the file system once, which does all of that work at once.
  The run that made the call then advances the drive's own cycles by the busy
  time the file system reports, rounded up to whole cycles. Until those cycles
  have passed, the drive does nothing at all: it takes no words, sends no
  fields, and handles no requests. The disk couldn't act on a reset partway
  through its work either, so a reset waits too, and `TSTP` shows it pending
  until the drive gets to it. Accepting a command never touches the disk. A
  latent command's work runs once the OS has read its accept status, so the
  drive shows phase 2 while the disk is busy, then posts the completion. Disk
  work the host starts, the superblock read at attach, is charged at the start
  of the drive's next run, which does nothing else, so a command that arrives
  meanwhile waits for it, as disk_design.md first had it.
- **The OS waits for idle before a command word.** A command ends only once
  the drive sees that the OS has taken its last field, which may be after disk
  work the host started. Until then the phase shows 3, and a command word
  written meanwhile would set the command port's flag again, hiding that the
  OS took the field. So the OS reads the control port until the phase is 0
  before each command word, and the drive never has to see the take within a
  set time.
- **A write takes the first word of a sector before loading the sector.**
  disk_design.md had the drive load the sector at the write's position right
  after accept, and leave the first word in the port until the load was done.
  The drive could still leave the word in the port during the load, reading it
  without clearing the port's flag. But the file system loads a sector for
  writing only as it writes a word into it. So during the load, the word would
  already be in the file while the port still showed it untaken. A reset then
  would keep a word disk_design.md says is discarded. So the drive takes each
  word as it arrives. A word that starts a sector loads that sector (or blanks
  it), and a word that starts a cluster allocates the cluster. The drive then
  takes no more words until that disk work is done. The stall at each sector
  is the same length as before, and comes one word later. A reset never leaves
  a word the drive took unwritten.
- **A read loads each sector when its first word is due,** as disk_design.md
  describes. The drive reads one word (or dword) ahead into the data port. So
  after a reset that withdraws a word from the port, the drive seeks the handle
  back past that word, and the position is just past the words the OS took.
- **Detaching an image** has no special case. The host detaches between
  processor runs, so whatever the drive has done is done. A latent command
  whose work has run completes with the status that work had, and one whose
  work hasn't started finds no disk when it does. A transfer's next word fails
  in the file system, which is an early completion with **no disk** (CL5).
  Detaching discards whatever the file system hadn't committed, so the OS can't
  rely on the outcome, as with a floppy pulled mid-command. disk_design.md had
  a command in progress complete with no disk, which needed the drive to
  replace a completion it already had.
- **Fields are both of a port's words.** The drive always loads and stores both
  words of a port, as a dword instruction does. A word field it sends has 0 in
  word 1, and it ignores word 1 of a word field it takes. So a field's width
  matters only to the command that builds or reads it, and the OS may move a
  word field with either width.
- **Accept checks in a fixed order:**
  1. The arguments, in the order they arrive, starting with the handle in the
     command word: bad handle, disk changed, I/O error, and wrong handle type
     for the handle, then bad name, bad pattern, bad label, bad attributes, bad
     argument, and past end.
  2. The disk, for a command with no handle: no disk, then I/O error. A handle
     only reaches the disk through its open slot, which the handle checks
     already cover.
  3. What the command needs: read-only, write protected, too many open, in use
     for `FORMAT`, and disk full for a `WRITE`.
- **`CLOSE` of every slot** (h = FFh) closes each open slot in order. It
  completes with the first failure, or OK. Free slots are skipped. A single
  `CLOSE` of a free slot is bad handle at accept.
- **Slots** are 1 to 255, since handle FFh means every slot.
- **Response time:** the drive runs the cycle after a core reads or writes one
  of its ports, which wakes it. So it takes a word, or puts the next one in a
  port, before the next instruction can get the port's lock, as the timer
  answers its clock port. Between port accesses it sleeps until a core wakes
  it, and while the disk is busy it doesn't run.

### Host interface

```
class DiskDrive final : public Device {
 public:
  using Clock = absl::AnyInvocable<absl::CivilSecond()>;  // As the timer's.

  static constexpr int kPorts = 3;  // Command, data, and control.

  // Writes back everything the file system hasn't committed, stamped with the
  // clock, as at a clean shutdown. A failure leaves the disk as a crash there
  // would.
  ~DiskDrive() override;

  // Called between Processor::Execute calls.
  void SetClock(Clock clock);  // None stamps no date.

  // Attaches `image`, first detaching any image attached, as DetachImage
  // does. The drive reads its superblock, and is busy for that time from its
  // next run. A program finds out what the disk holds, such as no file system,
  // from its commands.
  void AttachImage(std::unique_ptr<DiskImage> image);

  // Detaches and returns the image, or null if the drive is empty, discarding
  // what the file system hadn't committed, as pulling a floppy would.
  std::unique_ptr<DiskImage> DetachImage();
};
```

- The image is owned by the drive while it is attached, so nothing else can
  write it, and an image can't be in two drives at once.
- `AttachImage` returns nothing. The drive only ever reports a disk's state to
  the program, at its next command, and a host that wants to tell its user
  what is wrong with an image opens it with `DiskImage::Open` first, which
  gives an `absl::Status` with a message. The file system's own `AttachImage`
  keeps its status, for tools.
- Each processor made from a factory has its own drive, empty until the host
  attaches an image.
- There is no `Flush` for the host to call, as nothing needs it while the
  processor runs: detaching is a floppy pulled, and shutting down is the drive
  destroyed. A host that wants to eject cleanly can be given one when it
  needs it.

### Wiki

A new `3.5-Disk-Drive.md` page, as a program sees the drive. It has the
wiring, settings, and presets; the disk itself (images, the file system, and
names and patterns, which disk-library.md left for this page); and the ports,
the protocol, the commands, and the statuses. It is written in one piece at
the end of the feature, in CL7, once the drive's behavior has settled, rather
than piece by piece with CL3 to CL6. CL7 also adds a row to `3-Devices.md`'s
table and a line to `Home.md`.

## Design

### Names

- **`DiskDrive`**, `DiskDriveFactory`, and `DiskDriveSettings`, in
  `disk_drive.h`, after the wiki page's "Disk Drive". "Drive" alone would
  collide with the Ozzy's drive letters and with host drives. `DiskDevice`
  would describe every device. Recommended: `DiskDrive`.
- **`DiskDriveCommand`**: an enum of the command codes (`kOpen` = 10h to
  `kLabel` = 43h). **`DiskDrivePhase`**: an enum of the control port's phases
  (`kIdle` to `kWriting`). Statuses are the library's `DiskStatus`. The
  timer's codes are `kTimer...` constants because its indexes are plain words.
  The drive's are typed so a test can't pass a phase as a command.
- **`DiskCommandDef`**: one command's definition. It has its code, whether it
  takes a handle, its kind, and the fields it moves at each step, as an
  `InstructionDef` defines an instruction. `DiskCommandLayout` would collide
  with the library's `DiskLayout`. "Form" would collide with instruction forms.
- **`PortWords`**, in core's `port.h`: both words of a port as a value, with
  word and dword accessors, and `Load` and `Store` that move both under a lock.
  The timer's tests already had a struct of that name, and the timer stores
  dwords too. `PortValue` would read as one word, as `Port::GetValue` gives
  one; `PortPair` doesn't say what it holds. The plan says "a field" or "a
  port's words" where disk_design.md says "handoff".
- **Kinds:** `kSynchronous`, `kLatent`, `kRead`, and `kWrite`, using
  disk_design.md's words.
- **`DiskDrivePreset`** and **`GetDiskDrivePreset`**: the timing presets.
  "Preset" is disk_design.md's word. "Profile" and "model" would suggest
  something more than the four numbers.
- **`State`**: the drive's private `std::variant` of where it is, one struct
  per state (`IdleState`, `ArgumentsState`, and so on), each naming the
  `DiskDrivePhase` the control port shows for it. A separate name, since there
  can be more states than phases where that's cleaner (see State machine).
- **The control word**: what the control port shows, its phase, disk bit, and
  command code. "State word" collided with the drive's `State`; "status"
  would collide with a port's status flag and with `DiskStatus`.
- **`Command`** and **`CommandResponse`**: the command the OS wrote (its word,
  definition, and arguments), and an accept or completion (its status, results,
  and how many fields have gone on the port). "Response" covers both, where
  "step" didn't say which.
- **`TakeControlRequest`**: taking a request off the control port, and doing
  a reset, before the state's step. The CL3 draft's `ExecuteRequest` read as running a
  command.
- Settings: `slots` and `hardware`, a `DiskHardwareSettings`, as the
  library's own type rather than a copy of its fields.
  `DiskDriveSettings::kMaxLatency` is the longest latency the factory takes.
  `kDiskDriveReadCacheSectors` and `kDiskDriveCacheWordsPerSec` are the
  presets' cache, as public constants.

### disk: what accept needs

Accept answers without touching the disk, so the drive asks the file system
only what it already knows. Three of the things accept needs are missing:

```
// DiskFileSystem additions.

// Returns kNoDisk with no image, kIoError once it has failed, or else kOk.
DiskStatus GetImageStatus() const;

// Returns whether a slot is free for Open, OpenRoot, or Create.
bool HasFreeSlot() const;

// Returns whether a file is open, stale slots aside, as Format refuses.
bool HasOpenFiles() const;

struct DiskFileInfo {
  int position = 0;
  int size = 0;
  bool directory = false;  // New: a directory's handle, for FIND.
};
```

- `GetWritableWords` already gives everything `WRITE` and `TRUNCATE` need at
  accept: wrong handle type, read-only, and write protected, and how many words
  fit. `GetFileInfo` gives bad handle, disk changed, and I/O error for every
  other handle.
- The file system's own early checks stay, for tools, though the drive has
  already ruled them out at accept. The rules that are written twice are few
  (a pattern where a name goes, the settable attributes, and the layout), so
  shared `Check...` functions aren't worth it.
- `FaultyFileProtocol` moves from `oz3_disk_test` into a new test library,
  `oz3_disk_testing`, as `oz3_core_testing` holds `BaseCoreTest`, so the
  drive's tests can fail images too.

### core: PortWords

```
// The two words of a port, as a device moves them: both at once, word 0 and
// then word 1. Word 0 is a dword's low word.
class PortWords final {
 public:
  PortWords() = default;
  static PortWords FromWord(uint16_t word);  // Word 1 is 0.
  static PortWords FromWords(uint16_t word0, uint16_t word1);
  static PortWords FromDword(uint32_t value);

  bool operator==(const PortWords&) const = default;

  uint16_t GetWord(int index = 0) const;  // 0 or 1.
  uint32_t GetDword() const;

  // Load or store both words under `lock`, starting at the port's address and
  // leaving it where it was. `mode` applies to the pair: with T, neither word
  // moves unless the status allows, S applies after the second word, and A is
  // ignored. Return the status before, as Port::LoadWord and StoreWord do.
  uint16_t Load(Port& port, const Lock& lock, uint16_t mode);
  uint16_t Store(Port& port, const Lock& lock, uint16_t mode) const;
};
```

- A value class, so `Load` and `Store` are its own members, rather than
  additions to `Port`.
- `mode` applies as it does to one word, so T can't move one word without the
  other. A is ignored: moving both words always passes through the other word,
  so the address always ends where it started, which is word 0 unless
  something moved it since the lock was granted.
- The constructor taking both words is private, so each value says how it is
  built: `FromWord`, `FromWords`, or `FromDword`.
- Every device that moves both words moves onto it: the timer's tick count
  and clock answers, the keyboard's key state, and the teletype's characters.
  So do the timer's tests, which compare `PortWords` instead of a struct of
  their own, and the instruction tests' 32-bit port helpers.
- No setters, since a value is built whole (`FromDword` for a position, pairs
  of words for a name) or loaded, and no ordering, which two words don't have.
- It prints as `{abcdh, 0001h}` with `AbslStringify`, so a test's failures show
  the words.

### devices: the command table

```
enum class DiskCommandKind {
  kSynchronous,  // Answers at accept.
  kLatent,       // Completes later, with the interrupt.
  kRead,         // A data phase out of the drive, then a completion.
  kWrite,        // A data phase into the drive, then a completion.
};

struct DiskCommandDef {
  DiskDriveCommand command;
  bool takes_handle = false;
  DiskCommandKind kind = DiskCommandKind::kSynchronous;
  bool packed = false;  // Data moves in dwords.
  int arguments = 0;           // Fields after the command word.
  int accept_results = 0;      // Fields after an OK accept status.
  int completion_results = 0;  // Fields after an OK completion status.
};

// Returns the definition of command `code`, or null for an unknown code.
const DiskCommandDef* GetDiskCommandDef(uint8_t code);
```

- One constant array, one row per row of disk_design.md's Commands tables, in
  `disk_drive_command.h`. The drive's protocol reads it for how many fields
  each step moves, so where a command's words end is written once, and a test
  can walk the table. A field is one `PortWords`: a name is 4, and `FIND`'s
  entry 8. Each command builds its results as `PortWords` itself (`FromDword`
  for a position, pairs of words for a name), so widths live only there.

### devices: DiskDrive

```
enum class DiskDrivePreset {
  kFloppy360k, kFloppy720k, kFloppy1440k,
  kHardDisk10m, kHardDisk20m, kHardDisk40m,
};

// The read cache every preset has.
inline constexpr int kDiskDriveReadCacheSectors = 4;
inline constexpr int64_t kDiskDriveCacheWordsPerSec = 1'000'000;

// The preset's rates and latency, with the read cache above.
constexpr DiskHardwareSettings GetDiskDrivePreset(DiskDrivePreset preset);

struct DiskDriveSettings {
  static constexpr int kMaxSlots = 255;
  static constexpr absl::Duration kMaxLatency = absl::Minutes(1);

  int port = 0;               // The first of DiskDrive::kPorts.
  DeviceInterrupt interrupt;  // Raised at each completion.
  int slots = 4;              // Files open at once: 1 to 255.
  DiskHardwareSettings hardware =
      GetDiskDrivePreset(DiskDrivePreset::kFloppy1440k);
};

class DiskDriveFactory final : public DeviceFactory {
 public:
  // Returns an error if `settings` are invalid: slots out of range, any
  // hardware setting negative, or a latency over kMaxLatency.
  static absl::StatusOr<std::unique_ptr<DiskDriveFactory>> Create(
      std::string_view name, const DiskDriveSettings& settings);
};
```

- **The presets' names** (`floppy-1440k` and so on) belong to the Ozzy's
  config, so *Ozzy drives* maps them to `DiskDrivePreset`. The wiki's drive
  page lists the presets with what each approximates and its numbers.
- **One class.** The drive owns its `DiskFileSystem` and runs each command
  itself. The protocol has to know each command's kind and fields to manage
  its phases, so a separate class for the commands would track the same
  command in progress a second time, and add little more than a switch.
  Instead, each command is a pair of private member functions: one for its
  accept, which decodes its fields (`DiskFilename::FromWords`,
  `PatternFromWords`, `DiskLabel::FromWords`, and dwords checked against
  `DiskDirEntry::kMaxSize` before they become `int`) and makes its checks; and
  one for its work, which calls the one file system operation and packs its
  results. A switch on the command calls them. Synchronous commands (`MEDIA`,
  `SEEK`, and `INFO`) need only the accept.
- **State machine.** The drive's `State` is a `std::variant`, and each
  struct holds only what its state works on, so what every state shares (the
  file system, the clock, the locks, and the control word shown) is the only
  state on the drive itself. Each state has one `ExecuteState` overload, its
  step, which says what it waits for and does, and returns whether it moved to
  another state:
  - `IdleState`: take a command word;
  - `ArgumentsState`: take argument fields, then accept;
  - `SendingAcceptState`: put the accept's fields on the command port, one
    each time the OS takes the last. Once the OS has taken the last field, a
    latent command with an OK accept goes on to running its work; anything
    else goes idle;
  - `RunningWorkState`: run the latent command's work, with its arguments from
    the `Command`, so it needs no port, and go on to sending its completion;
  - `SendingCompletionState`: put the completion's fields on the command port,
    as for the accept, raising the interrupt with the first, then go idle. An
    error response is its status alone;
  - in CL5, reading and writing, which finish into `SendingCompletionState`.

  Each struct also returns the phase and the command word it shows
  (`GetPhase()` and `GetCommandWord()`), so `GetControlWord()` is one
  `std::visit`, and a new state that leaves either out doesn't compile. Phase
  3 means a field from the drive is on the command port, so both sending
  states show phase 2 until their first field is there. That field goes out
  once the drive holds the command and control ports, with phase 3, and for a
  completion the interrupt. Only steps move the drive to another state, apart
  from a reset, so `Accept` returns the accept's state rather than setting it.
  A step returns a `StateResult`: `kContinue` if it did something, so the
  drive runs the step of its state again, or `kWait` if it can't go on until
  the OS reads or writes a port, or a lock it needs is granted.
- **A run** first charges any disk time the host started, and ends there if
  there was some. It then takes any control request, and runs the state's
  step in a loop until it waits. Before each step, it shows the control word,
  locking the control port if the word has changed, so each step runs with
  its own phase shown. Every run takes at least a cycle: disk work's time is
  charged as soon as its step returns, and a run that waits takes one cycle.
  While a lock it asked for isn't granted, it keeps its locks and runs again
  the next cycle; otherwise it releases them and sleeps until a port access
  wakes it. So work that takes no time, such as a `CLOSE` with nothing
  to write, posts its completion in the same run.
- **Reset** happens as the drive takes the request. `HasControlRequest`
  checks the control port's flag without its lock, and `TakeControlRequest`
  then locks the port and reads the request. A reset also locks the command
  and data ports, withdraws their words, and goes idle, before clearing the
  request, so while it waits for a lock, `TSTP` still sees the request.
  Clearing a request shows the control word in its place at once, so the OS
  never reads its own request back.
  The state is still in hand until the reset replaces it, so CL5's abort of a
  read or write can do what its state needs first. A request with no reset bit
  is cleared at once, and the state carries on, so there is never a state to
  return to.
- **Locks:** a device can read a port's flag without its lock, as the timer
  does, since a flag only changes under the lock. The control port's flag is
  checked that way each run, and each step checks its port's flag the same way
  before it locks the port, for the rest of the run, only once it has
  something to do: the command port in the idle, arguments, and sending
  states, none while running work, and in CL5, the data port while reading and
  writing. So a core using a port the drive has nothing to do with doesn't
  delay it. A run that has to wait for a lock keeps the ones it has and tries
  again the next cycle, so it gathers them all.
- **The control word follows the ports.** The OS can't read the control port
  while the drive holds its lock, and the drive holds it from showing a
  state's control word until the run ends. So the phase can lag the ports,
  but never run ahead of them. That is the order the OS's check relies on:
  when it sees phase 2 or higher, the drive has already taken the OS's last
  word.
- **Time:** the file system's busy time is charged with `AdvanceCycles`, in
  cycles: ⌈ns × Hz ÷ 10⁹⌉, in `absl::int128`, so the rounding is visible and
  nothing can overflow. The drive releases every lock first, so it never holds
  a port while the disk is busy. A step's disk work is charged as soon as the
  step returns. A host call's (the superblock read at attach) is charged at
  the start of the next run, which then does nothing else, so a command word
  or request that arrives meanwhile waits for it. The drive can't run again
  until its cycles have passed, so a wake while it is busy only takes effect
  after.
- **Transfers** move one word or dword at a time, with `Read` and `Write` of
  one or two words. A file system call stops at a sector boundary, so a dword
  that crosses a sector is two calls. Calling once per word keeps each
  sector's disk work at the word that starts it, which is where the drive
  stalls (see Performance). Packed reads give a last dword with a zero high
  word at an odd end.
- **The interrupt** is raised as a completion's status goes on the command
  port, whether it is a latent command's, a transfer's, or an early one.
- **Host calls** wake the drive, as the teletype's `TakeOutput` does, since
  attaching and detaching change the control word, and attaching charges
  time.

**Brittleness:**
- The OS's side of the protocol is what disk_design.md describes: wait for
  phase 0 before writing a command word, and for phase 2 or higher before
  reading the command port after a command's last word, never `OUT` to the
  command port, and never `IN` the control port while a request is pending. The drive
  can't catch an OS that breaks these. Reset is the way out, as designed.
- One OS core issues commands. Two cores sharing a drive have to take turns
  themselves, as with the timer's clock port.
- Images are attached and detached only between `Processor::Execute` calls, as
  every host call is.
- The clock is called in the destructor, so whatever it uses must outlive the
  processor. The Ozzy's host clock does.

### Testing

The drive's tests play the OS from the test, through the ports, as the timer's
tests ask its clock port. A `DiskDriveTest` fixture moves fields as
`PortWords`, as many as the command table gives each step, and runs a command
to the end of each step, returning the fields the drive sent. So every command
and error is tested through the real protocol, as concisely as a direct call.
Assembled programs (`DeviceProgramTest`) check the OS's real loops (`OUTR`,
`INR`, the dword forms, and `WAIT` on the interrupt) and exact cycles. Images
are in a `gb::MemoryFileProtocol`, or `FaultyFileProtocol` to fail.

### Performance

A transfer costs, per word, one drive run with a port lock, a file system
`Read` or `Write` of one word, and collecting the busy time. An OS's streaming
loop moves about 100 Kiw/s at 1 MHz (see disk_design.md's Timing presets), so
this should cost well under a microsecond of host time per simulated word. CL5
measures a 64 Kiw `READ`. If that is ever a problem, the drive can move a
sector's words at a time, keeping the stall where it is.

### To confirm

- That a one-word `Read` or `Write` costs the whole sector's time at the word
  that loads, blanks, or allocates, and nothing for the words after it, so
  per-word calls charge exactly what per-sector calls would (CL5).
- That the drive, taking a word the cycle after an `INR` or `OUTR` releases the
  data port, keeps up with the 8 cycles a word of the loop, so a loop over a
  loaded sector never stalls (CL5: disk_design.md's 300-word `READ` example, in
  exact cycles).
- That the drive gets the command port's lock in the cycle after an `INS` takes
  its last word, before the next instruction's `PLK`. Otherwise the OS's next
  command word, written straight after, would be taken for the drive's own
  word, still unread (CL3; the timer relies on the same).

## CLs

### CL1 [x] disk: what the drive's accept needs

Depends on: nothing.

- `disk_file_system.h` and `.cc`: `GetImageStatus`, `HasFreeSlot`,
  `HasOpenFiles`, and `DiskFileInfo::directory`.
- `CMakeLists.txt`: `oz3_disk_testing`, holding `faulty_file_protocol.h` and
  `.cc`, which `oz3_disk_test` links.
- Unused, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: each query with no image, a failed image, every slot taken,
  stale slots (taken, but not open files), and `directory` for the root and a
  file.

### CL2 [x] core: PortWords

Depends on: nothing.

- `port.h` and `.cc`: `PortWords`.
- `timer.cc`, `keyboard.cc`, and `teletype.cc`: their pairs of port words on
  `PortWords`, as are `timer_test.cc`'s and `instruction_test.h`'s.
- disk_design.md: the drive busy while the disk is, attaching and detaching to
  match, and fields moving both of a port's words.

**Verify**
- Standard checks.
- Unit tests: the accessors, equality, and printing; `Load` and `Store` in
  modes 0 and T|S, with T stopping both words and not just one, A ignored,
  and both starting and ending at the port's address. The teletype clearing
  word 1 once it takes the characters. The timer's, keyboard's, teletype's, and
  instructions' tests pass unchanged in what they check.

### CL3 [x] devices: the drive, its ports, and the protocol

Depends on: CL1, CL2.

- `disk_drive_command.h`: `DiskDriveCommand`, `DiskCommandKind`,
  `DiskCommandDef`, and the table, with every command. A command the drive
  doesn't run yet is bad command.
- `disk_drive.h` and `.cc`: the presets, settings, factory, and drive: the
  ports and their locks, the control port's control word and reset, the state
  machine, the command protocol (command words, arguments and results, accept
  and completion, and the interrupt), busy time, the host interface and
  destructor, and the first commands, `MEDIA`, `OPEN ROOT`, and `CLOSE`, one
  of each kind but transfers.
- `CMakeLists.txt`: the new files, and `oz3_disk`, with `oz3_disk_testing` for
  the tests. CLAUDE.md's library list says `devices` links `core` and `disk`.

**Verify**
- Standard checks.
- Unit tests:
  - the table: every code from disk_design.md, its handle, kind, and field
    counts, and null for every other code;
  - the factory's wiring, and its errors for each setting, including a latency
    just over `kMaxLatency`; the presets' numbers against disk_design.md's
    table;
  - the control word in every phase, and with and without a disk;
  - reset in every phase, withdrawing words on both ports, and `TSTP` seeing
    the request done; a request of 0; reserved bits ignored;
  - the phase 0 wait before a command word, and a response taken as the host
    attaches ending once the superblock is read;
  - the phase 2 wait before an accept status, bad command after the command
    word alone, and `MEDIA`'s results on an unformatted and a write-protected
    image;
  - accept's order of checks, on `OPEN ROOT` and `CLOSE`;
  - latent completions in exact cycles, with the work starting once the OS has
    read the accept status, and phase 2 shown until the completion is posted;
  - the drive doing nothing while busy: a command word, a field read, and a
    reset all waiting for the superblock read at attach, and for a latent
    command's work;
  - a command word written in phase 2 lost under the completion, one written in
    phase 1 taken as an argument, and `OUTS` refused in phase 3;
  - detaching mid-command: a latent command whose work has run completing with
    its real status, and one whose work hasn't started with no disk;
    attaching over an image, and the disk bit;
  - the drive locking only the ports it uses, and only once it has something
    to do, a core holding a port delaying it, phase 3 shown only once a
    completion's status is on the port, and the drive asleep while idle;
  - an assembled program opening and closing the root with `OUTR`, `INR`, and
    `WAIT` on the interrupt.

### CL4 [ ] devices: file commands

Depends on: CL3.

- `disk_drive.cc`: `OPEN`, `CREATE`, `CLOSE` of every slot, `FLUSH`, `SEEK`,
  and `INFO`.

**Verify**
- Standard checks.
- Unit tests: every accept error each command can give, and the order of
  checks where two can happen at once; each command's completion and results
  against the file system and the image after it (`CREATE` stamped with the
  clock, `CLOSE` of every slot with a failure partway, `SEEK` past the end and
  past the largest size); stale slots and failed images through each command.

### CL5 [ ] devices: reading and writing

Depends on: CL4.

- `disk_drive.cc`: `READ`, `WRITE`, `READ PACKED`, `WRITE PACKED`, and
  `TRUNCATE`, with the data phase, early completion (zeros out, words thrown
  away), and resets while reading and writing.

**Verify**
- Standard checks.
- Unit tests:
  - n at accept against the size, the writable words, and disk full, both for
    words and dwords; every accept error; an odd end in packed reads;
    `TRUNCATE`'s completion and image;
  - per-word busy time matching a sector's (see To confirm);
  - disk_design.md's `READ` and `WRITE` examples in exact cycles, with an
    assembled `INR` and `OUTR` loop and with the dword forms; the stall at
    each sector and each new cluster; n = 0 completing at once;
  - early completion from a failed image, a broken chain, and a detach, in
    every phase it can show, with the OS's loop running out its n words and
    with a reset cutting it short;
  - resets while reading (position just past the words taken, with and without
    a word in the port) and while writing (the size covering the words taken);
  - the destructor writing back a file written and not yet committed, stamped
    with the clock (the first command that leaves anything uncommitted).
- Performance: a 64 Kiw `READ` in a program, its host time measured in
  Release, and the result recorded here.

### CL6 [ ] devices: directory and disk commands

Depends on: CL4.

- `disk_drive.cc`: `FIND`, `DELETE`, `RENAME`, `ATTRIB`, `VOLUME`, `FORMAT`,
  and `LABEL`.

**Verify**
- Standard checks.
- Unit tests: each command's accept errors (bad pattern, bad label, bad
  attributes, a `FORMAT` sectors per cluster with no layout, in use for
  `FORMAT`, write protected) and completion errors (not found, exists, in use,
  read-only, unformatted, disk full for a root that can't grow); `FIND` through
  a listing and restarting with `SEEK`; `VOLUME`'s free count; and the image
  after each change. One assembled program that formats, lists with `FIND`,
  and reads the entries with `INR.ID`.

### CL7 [ ] wiki: the disk drive

Depends on: CL3 to CL6.

- Wiki: `3.5-Disk-Drive.md`, the whole page, for the finished drive: wiring,
  settings, and presets; the disk (images, the file system, and names and
  patterns); the ports, the protocol, aborts, timing, and disks coming and
  going; every command, reading and writing, early completion, packed
  transfers, the free count, and the OS examples' listing; and the statuses.
  A row in `3-Devices.md`, and a line in `Home.md`. A draft of the page as of
  CL3 is in a comment on CL3's PR (#4), to start from.
- In the repository, this CL only marks itself done. The wiki patch is in a
  comment on its PR.

**Verify**
- The page against the code and docs/disk_design.md, section by section.
- Its links resolve, by page name in lower case.
- Wiki: the pages above, as a patch on the PR.
