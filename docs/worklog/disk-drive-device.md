# Disk drive device

`DiskDrive` (`oz3/devices/disk_drive.h`) is the smart drive from
[disk_design.md](../disk_design.md) (Drive device). It is built on the disk
library ([disk-library.md](disk-library.md)), so a program can use the file
system that only host code could call before. A drive has three ports and an
interrupt:
- the OS writes commands to the command port, and reads their statuses and
  results back;
- it moves file data through the data port;
- it watches or resets the drive through the control port.

The drive runs each command on its own `DiskFileSystem`, and is busy for the
disk's time, in the processor's cycles.

Any processor can have drives, as with the other devices. The host attaches and
detaches images between runs. The Ozzy doesn't have drives yet: its four drives
are the backlog's *Ozzy drives*.

```
absl::StatusOr<std::unique_ptr<DiskDriveFactory>> factory =
    DiskDriveFactory::Create("Drive", {.port = 16,
                                       .interrupt = {.interrupt = 2}});
ProcessorConfig config = {..., .devices = {*std::move(factory)}};
gb::Context context;
context.SetValue<HostClock>([] { return HostNow(); });  // Stamps files.
auto processor = std::make_unique<Processor>(config, std::move(context));
auto* drive = static_cast<DiskDrive*>(processor->GetDevice("Drive"));
drive->AttachImage(*DiskImage::Open(&host, "local:/work.ozd"));
processor->Execute(...);  // Programs open, read, and write files.
processor.reset();        // The drive writes back what it hasn't committed.
```

The manual's [Disk Drive](../manual/3.5-Disk-Drive.md) page is the
specification of everything a program sees: wiring, settings, and presets; the
disk's image and file system format; the ports, the command protocol, aborts,
and timing; every command and where its errors are found; reading and writing;
and the statuses. disk_design.md's Drive device section matches it. This
summary covers how the drive is built.

## Behavior

The rules below shape the implementation most. The manual states each from a
program's side.

- **Only reset aborts.** The drive reads the command port only while it waits
  for a command's words, in phases 0 and 1. A word the OS writes in phase 2, 4,
  or 5 is lost when the drive puts its completion's status over it.
- **The drive is busy while the disk is.** A command's disk work is one call
  to the file system, which does all of it at once. The run that made the call
  then advances the drive's cycles by the busy time the file system reports,
  rounded up to whole cycles. Until those cycles have passed, the drive does
  nothing: it takes no words, sends no fields, and handles no requests, not
  even a reset.
  - Accepting a command never touches the disk.
  - A latent command's work runs once the OS has read its accept status, so
    the drive shows phase 2 until it posts the completion.
  - Disk work the host starts, the superblock read at attach, is charged at the
    start of the drive's next run, which does nothing else.
- **The OS waits for phase 0 before a command word.** A command ends only once
  the drive sees that the OS has taken its last field. A command word written
  before then would set the command port's flag again and hide the take.
- **A write takes each word as it arrives.** A word that starts a sector loads
  that sector (or blanks it), and a word that starts a cluster allocates the
  cluster. The drive then takes no more words until that disk work is done. So
  a reset never leaves a word the drive took unwritten.
- **A read runs one transfer ahead of the data port,** reading the next as soon
  as the last goes on the port, so a sector's load overlaps the OS taking the
  word before it. After a reset, the drive seeks the handle back past the
  transfer read ahead and any still in the port, so the position is just past
  what the OS took.
- **Detaching has no special case.** The host detaches between processor runs,
  so whatever the drive did is done.
  - A latent command whose work has run completes with its own status, and one
    whose work hasn't run finds no disk.
  - A transfer's next word fails in the file system, as its slot is stale, so
    the transfer completes early with **disk changed**.
  - Detaching discards what the file system hadn't committed, as pulling a
    floppy would.
- **Fields are both of a port's words.** The drive always loads and stores both
  words of a port, as a dword instruction does. A word field it sends has 0 in
  word 1, and it ignores word 1 of a word field it takes. So the OS may move a
  word field with either width.
- **Slots** are 1 to 255, since handle FFh (`kAllSlots`) means every slot for
  `CLOSE`. `CLOSE` of every slot closes each open one in order, skipping free
  ones, and completes with the first failure, or OK.
- **Response time:** a core's read or write of one of the drive's ports wakes
  it, and it runs the next cycle. So it takes a word, or puts the next one on a
  port, before the next instruction can get the port's lock, as the timer
  answers its clock port. Between port accesses it sleeps.

### Host interface

- **The host's clock**, which the drive stamps files with, is in the
  processor's context (`DiskDrive::Contract`), as the timer's is: the host's
  local time unless the host gives another. With an empty one, files get no
  date.
- **`AttachImage`** takes ownership of the image, first detaching any attached.
  It returns nothing: a program finds out what the disk holds from its
  commands, and a host that wants to know first opens the image with
  `DiskImage::Open`, which gives a status with a message.
- **`DetachImage`** returns the image, or null if the drive is empty.
- **The destructor** writes back everything uncommitted, stamped with the
  clock, as at a clean shutdown. A failure leaves the disk as a crash there
  would.
- Each processor made from a factory has its own drive, empty until the host
  attaches an image. There is no host `Flush`: detaching is a floppy pulled,
  and shutting down is the drive destroyed.

## Design

### Names

- **`DiskDrive`**, `DiskDriveFactory`, and `DiskDriveSettings`, after the
  manual's "Disk Drive". "Drive" alone would collide with the Ozzy's drive
  letters and with host drives.
- **`DiskDriveCommand`** and **`DiskDrivePhase`**: typed enums of the command
  codes and the control port's phases, so a test can't pass a phase as a
  command. Statuses are the disk library's `DiskStatus`.
- **`DiskCommandDef`**: one command's definition, as an `InstructionDef` is an
  instruction's.
- **`PortWords`**: both words of a port, as a value. A word or dword moved
  through the data port is a transfer.
- **`DiskDrivePreset`**: the timing presets, disk_design.md's word.
- **The control word**: what the control port shows (phase, disk bit, and
  command code), so it doesn't collide with `DiskStatus` or a port's status
  flag.

### disk: what accept needs

Accept answers without touching the disk, so `DiskFileSystem` answers from what
it already knows:
- `GetImageStatus` gives no disk, I/O error, or OK.
- `GetVolumeStatus` and `GetWritableStatus` give the disk checks of the
  commands with no handle, for reading (no disk, I/O error, unformatted) and
  for writing (write protected before unformatted).
- `GetFileStatus` gives bad handle, disk changed, and I/O error for a handle.
- `GetWritableWords` gives wrong handle type, read-only, and write protected,
  and how many words fit, for `WRITE` and `TRUNCATE`.
- `HasFreeSlot` says whether a file can be opened.
- `GetFormatStatus` gives what `Format` would fail with before it touches the
  disk, which `Format` itself checks with: no disk, I/O error, write protected,
  in use while a file is open (stale slots aside), then bad argument for a
  sectors per cluster with no layout.
- `DiskFileInfo::directory` marks a directory's handle, for `FIND`.

The file system keeps its own early checks, for tools, though the drive has
already ruled them out at accept.

### core: PortWords

`PortWords` (`oz3/core/port.h`) is the two words of a port as a device moves
them, built with `FromWord`, `FromWords`, or `FromDword`, and read with
`GetWord` and `GetDword`. `Load` and `Store` move both words under a lock,
starting at the port's address and leaving it there: a mode's T moves neither
word unless the status allows, S applies after the second word, and A is
ignored. It prints with `AbslStringify` as `{abcdh, 0001h}`. The timer, the
keyboard's key state, the teletype, and the instruction tests' 32-bit port
helpers all use it.

### devices: the command table

`disk_drive_command.h` holds `DiskDriveCommand`, `DiskDrivePhase`,
`DiskCommandKind` (`kSynchronous`, `kLatent`, `kRead`, and `kWrite`), and
`DiskCommandDef`: a command's code, whether it takes a handle, its kind, whether
its data is packed, and how many fields its arguments, accept results, and
completion results are. `GetDiskCommandDef` finds a code's row, or null. The
protocol reads the table for how many fields each step moves, so where a
command's words end is written once. A field is one `PortWords`, so a name is
`kDiskNameFields` (4) fields. Each command builds and decodes its own fields,
so their widths live only there.

### devices: DiskDrive

- **Settings:** `port`, `interrupt`, `slots` (1 to `kMaxSlots`, 255), and
  `hardware`, the library's own `DiskHardwareSettings`.
  `DiskDriveFactory::Create` refuses slots out of range, any negative hardware
  setting, or a latency over `kMaxLatency` (a minute). `GetDiskDrivePreset`
  gives each `DiskDrivePreset`'s rates and latency, with the read cache every
  preset has (`kDiskDriveReadCacheSectors` and `kDiskDriveCacheWordsPerSec`).
  The presets' config names belong to the Ozzy.
- **One class.** The drive owns its `DiskFileSystem` and runs each command
  itself. Each command is a pair of private member functions:
  - its accept (`AcceptOpen`, ...), which decodes its fields and makes its
    checks;
  - its work (`RunOpen`, ...), which makes the one file system call and packs
    the results.

  `AcceptCommand` and `RunCommand` switch on the command. The synchronous
  commands (`MEDIA`, `SEEK`, and `INFO`) need only an accept.
- **State machine.** The drive's `State` is a `std::variant`, one struct per
  state, each holding only what it works on. So what every state shares (the
  file system, the clock, the locks, and the control word shown) is the only
  state on the drive itself. Each state has one `ExecuteState` overload, its
  step:
  - `IdleState` takes a command word.
  - `ArgumentsState` takes the argument fields, then accepts.
  - `SendingAcceptState` and `SendingCompletionState` put a
    `CommandResponse`'s fields on the command port, one each time the OS takes
    the last. The interrupt is raised with a completion's first field. A
    latent command with an OK accept goes on to `RunningWorkState`, and a
    transfer goes on to its data phase. Anything else goes idle.
  - `RunningWorkState` runs a latent command's work from its `Command`.
  - `ReadingState` and `WritingState` move a `Transfer` through the data port.
    A disk call that fails moves them to `FailedReadingState` or
    `FailedWritingState`. These send the early completion first, then move
    zeros out or throw words away until the accept's count is done.

  Each state struct returns the phase and command word it shows
  (`GetPhase()` and `GetCommandWord()`), so `GetControlWord()` is one
  `std::visit`, and a new state that leaves either out doesn't compile. A step
  returns a `StateResult`: `kContinue` to run the step of the drive's state
  again, or `kWait` until the OS reads or writes a port, or a lock is granted.
  Only steps change the state, apart from a reset.
- **A run** goes in this order:
  1. It charges any disk time the host started (`ChargeBusyTime`), and ends
     there if there was some.
  2. It takes any control request (`HandleAnyControlRequest`).
  3. It runs the state's step in a loop until the step waits. Before each step,
     it shows the control word (`UpdateControlWord`), locking the control port
     if the word changed, so each step runs with its own phase shown.

  Every run takes at least a cycle. While a lock it asked for isn't granted,
  it keeps its locks and runs again the next cycle. Otherwise it releases them
  and sleeps until a port access wakes it. So work that takes no time, such as
  a `CLOSE` with nothing to write, posts its completion in the same run.
- **Reset** happens as the drive takes the request. It locks the command and
  data ports, withdraws their words, seeks a read's handle back to just past
  what the OS took (`AbortRead`), and goes idle, before clearing the request.
  A write needs nothing undone, as its file already holds every word taken.
  So while it waits for a lock, `TSTP` still sees the request. Clearing a
  request shows the control word in its place at once, so the OS never reads
  its own request back. A request with no reset bit is cleared at once.
- **Locks:** a flag only changes under its port's lock, so the drive reads
  flags without the lock, as the timer does, and locks a port only once it has
  something to do there. So a core using a port the drive has nothing to do
  with doesn't delay it.
- **The control word follows the ports.** The drive holds the control port's
  lock from showing a step's control word until the run ends, so the phase can
  lag the ports but never run ahead of them. So when the OS sees phase 2 or
  higher, the drive has already taken its last word.
- **Time:** the file system's busy time is charged with `AdvanceCycles` as
  ⌈ns × Hz ÷ 10⁹⌉, in `absl::int128`, so nothing can overflow. The drive
  releases every lock first, so it never holds a port while the disk is busy.
- **Transfers** move one word or dword at a time, with a file system `Read` or
  `Write` of one word for each of its words. So each sector's disk work falls
  at the word that starts it, which is where the drive stalls. A packed read
  at an odd end gives a last dword with a zero high word.
- **Host calls** wake the drive, as the teletype's `TakeOutput` does, since
  attaching and detaching change the control word, and attaching charges time.

**Brittleness:**
- The OS's side of the protocol is what the manual describes. It waits for
  phase 0 before writing a command word, and for phase 2 or higher before
  reading the command port after a command's last word. It never `OUT`s to the
  command port, and never `IN`s the control port while a request is pending.
  The drive can't catch an OS that breaks these, and reset is the way out.
- One OS core issues commands. Two cores sharing a drive have to take turns
  themselves, as with the timer's clock port.
- Images are attached and detached only between `Processor::Execute` calls, as
  every host call is.
- The clock is called in the destructor, so whatever it uses must outlive the
  processor, and the file system the image is on must outlive the drive.

### Testing

- **`oz3_disk_testing`** holds `FaultyFileProtocol`, so the drive's tests can
  fail images as the disk library's do.
- **`DiskDriveTest`** plays the OS from the test, through the ports, moving
  fields as `PortWords`, as many as the command table gives each step:
  - `Start`, `SendField`, `ReceiveAccept`, and `Complete` run a command a step
    at a time;
  - `ReadData` and `WriteData` run a data phase, recording each stall;
  - helpers such as `Seek`, `Info`, `Find`, and `Volume` run a whole command.

  So every command and error is tested through the real protocol, as concisely
  as a direct call. Images are in a `gb::MemoryFileProtocol`, or a
  `FaultyFileProtocol` to fail. Helpers build images with files of known words
  (`AddFile`, `FileWord`), break their chains, and read back what the drive
  wrote (`DetachAndReadFile`, `DetachAndFindEntry`, `ReopenImage`).
- `DiskCommandDefTest` checks the table against the manual, and
  `DiskDriveFactoryTest` and `DiskDrivePresetTest` check each setting's limits
  and the presets' numbers.
- `DiskDriveTest` covers:
  - the control word in every phase, with and without a disk;
  - reset in every phase, withdrawing both ports' words;
  - the phase 0 and phase 2 waits;
  - a word written in the wrong phase;
  - the drive doing nothing while busy;
  - detaching mid-command;
  - locks taken only when needed;
  - every command's accept errors in order, and its completion against the
    image after it;
  - stale slots and failed images through each command;
  - the manual's `READ` and `WRITE` examples in exact cycles, with the stall
    at each sector and each new cluster;
  - early completion from a failed image, a broken chain, and a detach, in
    every phase it can show;
  - resets while reading and writing;
  - committing on `FLUSH`, `CLOSE`, and the destructor.
- Assembled programs (`DeviceProgramTest`) run the OS's real loops in exact
  cycles:
  - `AProgramOpensAndClosesTheRoot` uses `OUTR`, `INR`, and `WAIT` on the
    interrupt;
  - `AProgramFormatsAndListsTheDisk` lists with `FIND` and reads entries with
    `INR.ID`;
  - the `DiskDriveProgramTest`s read and write with word and dword loops, and
    log every stall, showing that the drive keeps up with a loop over a loaded
    sector.

### Performance

A transfer costs, per word, one drive run with a port lock, a file system
`Read` or `Write` of one word, and collecting the busy time. A one-word call
costs the whole sector's time at the word that loads, blanks, or allocates, and
nothing for the words after it, so per-word calls charge exactly what
per-sector calls would. `AProgramReads64KiWords`, a 65,535-word `READ` by an
`INR.IW` loop of about 850,000 cycles, takes about 22 ms of host time in
RelWithDebInfo with clang on Linux (4 cores), core included. That is about
0.33 µs a word, so at 1 MHz the machine runs nearly 40 times faster than real
time while streaming.
