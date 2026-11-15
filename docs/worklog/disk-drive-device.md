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
drive->AttachImage(*DiskImage::Open(&host, "local:/work.ozd"));  // kOk.
processor->Execute(...);  // Programs open, read, and write files.
drive->Flush();           // At a clean shutdown.
```

## Behavior

### What programs see

The drive behaves as disk_design.md's Drive device section describes: the
configuration and timing presets, memory, committing, timing, images and disk
changes, the ports, the command protocol and aborts, every command, reading and
writing with early completion and packed transfers, and the status codes. This
plan doesn't repeat them. It settles the points below, and updates
disk_design.md to match.

- **Disk work is on one timeline.** The drive does a command's disk work by
  calling the file system once, which does all of that work at once, and then
  the drive charges the busy time it reports. Accepting a command never touches
  the disk, so accepting never waits. Disk work starts once the disk has
  finished the work before it, which may be the superblock read at attach or
  the rest of an aborted command. A completion is posted when its work ends on
  that timeline. Busy time is rounded up to whole cycles, so any disk work
  takes at least one cycle.
- **A write takes the first word of a sector before loading the sector.**
  disk_design.md had the drive load the sector at the write's position right
  after accept, and leave the first word in the port until the load was done.
  But the file system loads a sector for writing only as it writes a word into
  it, so there is no way to load one ahead of its first word. So the drive takes
  each word as it arrives. A word that starts a sector loads that sector (or
  blanks it), and a word that starts a cluster allocates the cluster. The drive
  then takes no more words until that disk work is done. So the stall at each
  sector is the same length as before, and comes one word later, and an abort
  never leaves a word the drive took unwritten.
- **A read loads each sector when its first word is due,** as disk_design.md
  describes. The drive reads one word (or dword) ahead into the data port. So
  after an abort that leaves a word in the port, the drive seeks the handle back
  past that word, and the position is just past the words the OS took.
- **Detaching an image** while a command is in progress completes that command
  with **no disk**, and the drive does nothing more for it. A latent command's
  work was done on the old image when it started, and detaching discards
  whatever the file system hadn't committed, so the OS can't rely on the
  outcome, as with a floppy pulled mid-command. The completion follows any
  accept words the OS hasn't read. A transfer gets it as an early completion.
- **Accept checks in a fixed order:**
  1. The handle: bad handle, disk changed, and I/O error, then wrong handle
     type, then read-only and write protected for `WRITE` and `TRUNCATE`.
  2. The arguments, in the order they arrive: bad name, bad pattern, bad label,
     bad attributes, bad argument, and past end.
  3. The disk, for a command with no handle: no disk and I/O error, then write
     protected for a command that changes the disk.
  4. The slots: too many open, or in use for `FORMAT`.
  5. Disk full, for a `WRITE`.
- **`CLOSE` of every slot** (h = FFh) closes each open slot in order. It
  completes with the first failure, or OK. Free slots are skipped. A single
  `CLOSE` of a free slot is bad handle at accept.
- **Slots** are 1 to 255, since handle FFh means every slot.
- **The drive's port locks:** each run, the drive requests the locks of its
  three ports, and acts only once it holds all three. So the state word, the
  ports' flags, and the drive's phase always change together, and the OS never
  sees a phase that is out of step with the flags. A core holding one of the
  ports delays the drive by a cycle at a time, as it would delay another core.
- **Response time:** the drive runs the cycle after a core reads or writes one
  of its ports, which wakes it. So it takes a word, or puts the next one in a
  port, before the next instruction can get the port's lock, as the timer
  answers its clock port. Between port accesses it sleeps until its disk work
  is next due.

### Host interface

```
class DiskDrive final : public Device {
 public:
  using Clock = absl::AnyInvocable<absl::CivilSecond()>;  // As the timer's.

  static constexpr int kPorts = 3;  // Command, data, and control.

  // Called between Processor::Execute calls.
  void SetClock(Clock clock);  // None stamps no date.

  // Attaches `image`, first detaching any image attached, as DetachImage
  // does. The superblock is read on the drive's timeline, as disk work. Returns
  // kOk, kUnformatted, or kIoError, as DiskFileSystem::AttachImage does.
  DiskStatus AttachImage(std::unique_ptr<DiskImage> image);

  // Detaches and returns the image, or null if the drive is empty, discarding
  // what the file system hadn't committed. A command in progress completes
  // with kNoDisk.
  std::unique_ptr<DiskImage> DetachImage();

  // Writes back everything the file system hasn't committed, as at a clean
  // shutdown, stamped with the clock. Takes no simulated time.
  DiskStatus Flush();
};
```

- The image is owned by the drive while it is attached, so nothing else can
  write it, and an image can't be in two drives at once.
- Each processor made from a factory has its own drive, empty until the host
  attaches an image.

### Wiki

A new `3.5-Disk-Drive.md` page, as a program sees the drive. It has the
wiring, settings, and presets; the disk itself (images, the file system, and
names and patterns, which disk-library.md left for this page); and the ports,
the protocol, the commands, and the statuses. It is written over CL3 to CL5.
CL5 also adds a row to `3-Devices.md`'s table and a line to `Home.md`.

## Design

### Names

- **`DiskDrive`**, `DiskDriveFactory`, and `DiskDriveSettings`, in
  `disk_drive.h`, after the wiki page's "Disk Drive". "Drive" alone would
  collide with the Ozzy's drive letters and with host drives. `DiskDevice`
  would describe every device. Recommended: `DiskDrive`.
- **`DiskController`**, in `disk_controller.h`: the drive's commands on its
  file system, with no ports and no time. A real drive's controller board was
  what ran its commands, and disk_design.md's cache rate is already "an OZ-3
  class controller". It could be read as a game controller, but not in
  `devices`, where the keyboard is the input. Alternatives: `DiskCommands`
  (reads as a list of them), `DiskDriveLogic` (a catch-all). Recommended:
  `DiskController`.
- **`DiskDriveCommand`**: an enum of the command codes (`kOpen` = 10h to
  `kLabel` = 43h). **`DiskDrivePhase`**: an enum of the control port's phases
  (`kIdle` to `kWriting`). Statuses are the library's `DiskStatus`. The
  timer's codes are `kTimer...` constants because its indexes are plain words.
  The drive's are typed so a test can't pass a phase as a command.
- **`DiskCommandDef`**: one command's definition. It has its code, whether it
  takes a handle, its kind, and the fields it moves at each step, as an
  `InstructionDef` defines an instruction. `DiskCommandLayout` would collide
  with the library's `DiskLayout`. "Form" would collide with instruction forms.
- **`DiskHandoff`**: `kWord` or `kDword`, how a field moves on the command
  port, using disk_design.md's own word "handoff".
- **Kinds:** `kSynchronous`, `kLatent`, `kRead`, and `kWrite`, using
  disk_design.md's words.
- **`DiskDrivePreset`** and **`GetDiskDrivePreset`**: the timing presets.
  "Preset" is disk_design.md's word. "Profile" and "model" would suggest
  something more than the four numbers.
- **The disk's timeline:** in code, `disk_free_cycle_`, the cycle when the disk
  finishes the work it has been given. `busy_until_` was the alternative, but
  "busy" already names the library's busy time, which is a duration.
- Settings: `slots` and `hardware`, a `DiskHardwareSettings`, as the
  library's own type rather than a copy of its fields.

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

### devices: DiskController

```
enum class DiskHandoff { kWord, kDword };

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
  bool packed = false;  // Data handoffs are dwords.
  absl::Span<const DiskHandoff> arguments;
  absl::Span<const DiskHandoff> accept_results;      // After an OK status.
  absl::Span<const DiskHandoff> completion_results;  // After an OK status.
};

// Returns the definition of command `code`, or null for an unknown code.
const DiskCommandDef* GetDiskCommandDef(uint8_t code);

// The drive's commands on its file system, with no ports and no time: it
// accepts each command, does its work, and gives the words for the command
// port. Busy time is the file system's, which the drive collects.
class DiskController final {
 public:
  DiskController(const DiskHardwareSettings& settings, int slots);

  DiskFileSystem* GetFileSystem();

  // Accepts `command_word`, whose definition `def` is, with its argument
  // words, as many as def's arguments hold. Returns the accept status, with
  // the accept results in `results` on kOk, after which a latent command or a
  // transfer is in progress.
  DiskStatus Accept(const DiskCommandDef& def, uint16_t command_word,
                    absl::Span<const uint16_t> arguments,
                    std::vector<uint16_t>* results);

  // Does the latent command in progress, all at once, and returns its
  // completion status, with its results in `results` on kOk.
  DiskStatus Complete(DateTime now, std::vector<uint16_t>* results);

  // The transfer in progress: its n handoffs, each a word or (packed) a dword.
  DiskStatus ReadHandoff(std::array<uint16_t, 2>* words);
  DiskStatus WriteHandoff(const std::array<uint16_t, 2>& words);
  DiskStatus CompleteTransfer(std::vector<uint16_t>* results);

  // Ends the command in progress for an abort. `untaken` is how many handoffs
  // ReadHandoff gave that the OS never took (0 or 1), so the position goes
  // back to just past the words the OS took.
  void Abort(int untaken);
};
```

- **The command table** is one constant array of `DiskCommandDef`s, one per
  row of disk_design.md's Commands tables. The drive reads it to know how many
  handoffs each step has. So the drive and the controller can't disagree about
  where a command's words end, and a test can walk the table.
- **Accept** decodes the handle and the fields (`DiskFilename::FromWords`,
  `PatternFromWords`, `DiskLabel::FromWords`, dwords checked against
  `DiskDirEntry::kMaxSize` before they become `int`), and makes the checks in
  the order Behavior gives. It keeps what the work needs, such as a decoded
  name. Synchronous commands (`MEDIA`, `SEEK`, `INFO`) do their work here.
- **Complete** calls the one file system operation, such as `Create`, and packs
  its results. A handle is its slot's number, so `OPEN`'s handle is the
  `DiskFile`.
- **Transfers** move one handoff per call, a word or two, with `Read` and
  `Write` of one or two words. A file system call stops at a sector boundary,
  so a dword that crosses a sector is two calls. Calling once per word keeps
  each sector's disk work at the word that starts it, which is where the drive
  stalls. The cost is two cheap calls per word (see Performance). Packed reads
  give a last dword with a zero high word at an odd end.
- It knows nothing of the drive's ports or phases, so its tests call it
  directly, command by command, with images in a `gb::MemoryFileProtocol`.

**Brittleness:** `Accept` needs exactly the argument words its definition
holds, and the transfer calls need a transfer in progress, so a caller can get
them wrong. The drive is the only caller, and it takes both counts from the
same definition, and the controller `CHECK`s them. A handle being a slot number
is by design (see disk-library.md).

### devices: DiskDrive

```
enum class DiskDrivePreset {
  kFloppy360k, kFloppy720k, kFloppy1440k, kHd10m, kHd20m, kHd40m,
};

// The preset's rates and latency, with the drive's default read cache of 4
// sectors and a cache rate of 1,000,000 words per second.
constexpr DiskHardwareSettings GetDiskDrivePreset(DiskDrivePreset preset);

struct DiskDriveSettings {
  int port = 0;               // The first of DiskDrive::kPorts.
  DeviceInterrupt interrupt;  // Raised at each completion.
  int slots = 4;              // Files open at once: 1 to 255.
  DiskHardwareSettings hardware =
      GetDiskDrivePreset(DiskDrivePreset::kFloppy1440k);
};

class DiskDriveFactory final : public DeviceFactory {
 public:
  // Returns an error if `settings` are invalid: slots out of range, or any
  // hardware setting negative.
  static absl::StatusOr<std::unique_ptr<DiskDriveFactory>> Create(
      std::string_view name, const DiskDriveSettings& settings);
};
```

- **The presets' names** (`floppy-1440k` and so on) belong to the Ozzy's
  config, so *Ozzy drives* maps them to `DiskDrivePreset`. The wiki's drive
  page lists the presets with what each approximates and its numbers.
- **The drive** owns a `DiskController`, and its own state: the phase, the
  command in progress and its definition, the argument words so far, the
  queue of words to send on the command port, the data phase's handoffs left
  (and whether it failed, after which it moves zeros out or throws words away),
  and `disk_free_cycle_`.
- **Each run** holds all three ports' locks (see Behavior), then works through
  them in order:
  1. A request on the control port: reset, then the state word restored and
     the flag cleared in the same step.
  2. The command port: in phases 0 and 1, take a word; in phase 3, send the
     next handoff once the OS has taken the last one; in phases 2, 4, and 5, a
     word means a new command, aborting the current one.
  3. The data port, in phases 4 and 5, once the disk is free: put the next
     handoff in, or take the next one.
  4. A latent command's completion, once its work is done on the timeline.
  5. The state word, if it changed.
  Then it sleeps until it is woken, or until the disk's next work is due.
- **The phase** that the control port shows is worked out from the drive's
  state rather than kept separately. Arguments still to come is 1. Words
  waiting to be sent is 3, which is how an early completion shows over a data
  phase. A data phase is 4 or 5. Latent work is 2. Anything else is idle.
- **Time:** after each file system call, the drive collects the busy time,
  turns it into cycles at the clock rate, rounded up, and adds it to
  `max(disk_free_cycle_, now)`. A completion, or the next data handoff, waits
  for `disk_free_cycle_`.
- **The interrupt** is raised as a completion's status goes on the command
  port, whether it is a latent command's, a transfer's, or an early one.
- **Host calls** wake the drive, as the teletype's `TakeOutput` does, since
  attaching and detaching change the state word and may complete a command.

**Brittleness:**
- The OS's side of the protocol is what disk_design.md describes: wait for
  phase 2 or higher before reading the command port, never `OUT` to the command
  port, and never `IN` the control port while a request is pending. The drive
  can't catch an OS that breaks these. Reset is the way out, as designed.
- One OS core issues commands. Two cores sharing a drive have to take turns
  themselves, as with the timer's clock port.
- Images are attached and detached only between `Processor::Execute` calls, as
  every host call is.

### Performance

A transfer costs, per word, one drive run with three lock requests, a file
system `Read` or `Write` of one word, and collecting the busy time. An OS's
streaming loop moves about 100 Kiw/s at 1 MHz (see disk_design.md's Timing
presets), so this should cost well under a microsecond of host time per
simulated word. CL4 measures a 64 Kiw `READ`. If that is ever a problem, the
controller can move a sector's words at a time, keeping the stall where it
is.

### To confirm

- That a one-word `Read` or `Write` costs the whole sector's time at the word
  that loads, blanks, or allocates, and nothing for the words after it, so
  per-word calls charge exactly what per-sector calls would (CL4, in the
  controller's tests).
- That the drive, taking a word the cycle after an `INR` or `OUTR` releases the
  data port, keeps up with the 8 cycles a word of the loop, so a loop over a
  loaded sector never stalls (CL4: disk_design.md's 300-word `READ` example, in
  exact cycles).
- That the drive gets the command port's lock in the cycle after an `INS`,
  before the next instruction's `PLK`, so a command word written right after
  reading a status can't be taken for the drive's own (CL3; the timer relies on
  the same).

## CLs

### CL1 [ ] disk: what the drive's accept needs

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

### CL2 [ ] devices: the controller and file commands

Depends on: CL1.

- `disk_drive_command.h`: `DiskDriveCommand`, `DiskHandoff`,
  `DiskCommandKind`, `DiskCommandDef`, and the table, with every command, so
  the drive in CL3 knows them all. A command the controller doesn't run yet is
  bad command.
- `disk_controller.h` and `.cc`: accept and completion for `OPEN`, `OPEN
  ROOT`, `CREATE`, `CLOSE` (one slot and every slot), `FLUSH`, `SEEK`, `INFO`,
  and `MEDIA`.
- `CMakeLists.txt`: the new files, and `oz3_disk`, with `oz3_disk_testing` for
  the tests. CLAUDE.md's library list says `devices` links `core` and `disk`.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests, on images in a `gb::MemoryFileProtocol`:
  - the table: every code from disk_design.md, its handle, kind, and field
    widths, and null for every other code;
  - accept's order of checks, a case for each pair that can happen at once,
    and every accept error each command can give;
  - each command's completion and results, as words, against the file system
    and the image after it (`CREATE` stamped with the clock, `CLOSE` of every
    slot with a failure partway, `SEEK` past the end and past the largest
    size, `MEDIA` on an unformatted and a write-protected image);
  - stale slots and failed images (`FaultyFileProtocol`) through each command.

### CL3 [ ] devices: the drive, its ports, and the protocol

Depends on: CL2.

- `disk_drive.h` and `.cc`: the presets, settings, factory, and drive: the
  three ports and the run order, the control port's state word and reset, the
  command protocol (command words, argument and result handoffs, accept and
  completion, and the interrupt), aborts by command word in phase 2, the
  timeline, and the host interface, running CL2's commands.
- Wiki: `3.5-Disk-Drive.md`: wiring, settings, presets, the disk (images, the
  file system, names and patterns), the ports, the protocol, aborts, the
  commands so far, and the statuses.

**Verify**
- Standard checks.
- Unit tests, with the OS's side played from the test through the ports (as
  the timer's tests ask its clock port), and an assembled program
  (`DeviceProgramTest`) for the whole sequence of `OPEN` and `CLOSE` with
  `OUTR`, `INR`, and `WAIT` on the interrupt:
  - the factory's wiring, and its errors for each setting;
  - the presets' numbers against disk_design.md's table;
  - the state word in every phase, and with and without a disk;
  - reset in every phase, withdrawing words on both ports, and `TSTP` seeing
    the request done; a request of 0; reserved bits ignored;
  - the phase 2 wait before an accept status, bad command after the command
    word alone, and synchronous results;
  - latent completions in exact cycles on the timeline, after the superblock
    read at attach, and after an aborted command's work;
  - aborts by command word in phase 2, and none in phases 1 and 3 (the word
    taken as an argument, and `OUTS` refused);
  - detaching mid-command (no disk, after unread accept words), attaching over
    an image, the disk bit, `Flush`, and the clock's stamps;
  - a core holding a port delaying the drive, and the drive asleep while idle.
- Wiki: the pages above, as a patch on the PR.

### CL4 [ ] devices: reading and writing

Depends on: CL3.

- `disk_controller.cc`: `READ`, `WRITE`, `READ PACKED`, `WRITE PACKED`, and
  `TRUNCATE`.
- `disk_drive.cc`: the data phase, early completion (zeros out, words thrown
  away), and aborts in phases 4 and 5.
- Wiki: `3.5-Disk-Drive.md`'s data commands, reading and writing, early
  completion, and packed transfers.

**Verify**
- Standard checks.
- Unit tests:
  - the controller: n at accept against the size, the writable words, and
    disk full, both for words and dwords; every accept error; per-word busy
    time matching a sector's (see To confirm); an odd end in packed reads;
    `TRUNCATE`'s completion and image;
  - the drive: disk_design.md's `READ` and `WRITE` examples in exact cycles,
    with an assembled `INR` and `OUTR` loop and with the dword forms; the
    stall at each sector and each new cluster; n = 0 completing at once;
    early completion from a failed image, a broken chain, and a detach, in
    every phase it can show, with the OS's loop running out its n handoffs
    and with it cutting the loop short; aborts by command word and by reset
    while reading (position just past the words taken, with and without a word
    in the port) and while writing (the size covering the words taken).
- Performance: a 64 Kiw `READ` in a program, its host time measured in
  Release, and the result recorded here.
- Wiki: the sections above, as a patch on the PR.

### CL5 [ ] devices: directory and disk commands

Depends on: CL4 (CL3 for the drive; only the wiki page's order needs CL4).

- `disk_controller.cc`: `FIND`, `DELETE`, `RENAME`, `ATTRIB`, `VOLUME`,
  `FORMAT`, and `LABEL`.
- Wiki: `3.5-Disk-Drive.md`'s directory and disk commands, the free count, and
  the OS examples' listing; a row in `3-Devices.md`; a line in `Home.md`.

**Verify**
- Standard checks.
- Unit tests, in the controller: each command's accept errors (bad pattern,
  bad label, bad attributes, a `FORMAT` sectors per cluster with no layout, in
  use for `FORMAT`, write protected) and completion errors (not found, exists,
  in use, read-only, unformatted, disk full for a root that can't grow);
  `FIND` through a listing and restarting with `SEEK`; `VOLUME`'s free count;
  and the image after each change. In the drive, one program that formats,
  lists with `FIND`, and reads the entries with `INR.ID`.
- Wiki: the pages above, as a patch on the PR.
