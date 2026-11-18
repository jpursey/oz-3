# Ozzy drives

Drives `A:` to `D:` in the Ozzy, from [disk_design.md](../disk_design.md)
(Ozzy). Each is a `DiskDrive` ([disk-drive-device.md](disk-drive-device.md)),
wired the same in every Ozzy as its other devices are, so the Ozzy OS (the next
item) can boot from `A:` knowing the drive's ports and interrupt as constants.
A config file lists the drives in letter order, giving each its timing and the
disk image in it at power on, the machine table lists the drives, and the
`ozzy` app runs an Ozzy with its disks in:

```
// work.ozzy
drives: [
  { preset: "floppy-1440k", image: "disks/system.ozd" },  // A:
  { preset: "hd-20m", image: "disks/work.ozd" },          // B:
]
```

A host that embeds Ozzy opens the images through its own file system, and can
change disks between runs:

```
absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
    Ozzy::Create(*settings, &file_system);  // Opens and attaches the images.
(*ozzy)->SetClock([] { return HostNow(); });  // The timer's and the drives'.
(*ozzy)->Run(absl::Milliseconds(16));
DiskDrive* b = (*ozzy)->GetDrive('B');
b->AttachImage(*DiskImage::Open(&file_system, "disks/other.ozd"));
ozzy->reset();  // Each drive writes back what it hasn't committed.
```

Changing disks while the `ozzy` app runs is left for a later item: the app's
console frontend has only its quit keys, and it needs a way to pick a drive and
an image (see the backlog's *Changing disks in ozzy*). Ejecting stays as the
drive has it, a floppy pulled: what the file system hadn't committed is lost,
as the OS commits with `CLOSE` and `FLUSH`. So `DiskDrive` gets no host
`Flush`.

## Behavior

### Wiring

| Drive | Ports    | Interrupt         |
| ----- | -------- | ----------------- |
| `A:`  | 16 to 18 | 2, on core 0      |
| `B:`  | 19 to 21 | 3, on core 0      |
| `C:`  | 22 to 24 | 4, on core 0      |
| `D:`  | 25 to 27 | 5, on core 0      |

- **The ports kept for devices grow from 0 to 15 to 0 to 31,** so `ports`'
  minimum rises from 16 to 32, and its default from 32 to 48, leaving programs
  the same 16 free ports by default. Ports 0, 5, and 28 to 31 are left for
  devices to come. ozzy_design.md's OS call ports move from 16 to 23 to 32 to
  39, the first ports past the devices'.
- **The interrupts are on core 0 alone,** the OS's core, which boots and owns
  the drives, issuing every command (see disk_design.md's OS examples). A
  completion can be read only once, so on every core, a program's handler
  could take it from the OS. Another core can still use a drive by polling its
  control port, but only core 0 can wait on its interrupt.
- **Drives are lettered in order,** from `A:`, so an Ozzy with drives always has
  an `A:`, the one it boots from. An empty drive, with no image, keeps a
  drive's place: an Ozzy whose only disk is in `B:` has an empty `A:`. An
  Ozzy has at most 4, and none by default.

### The config file

The drives are their own list, `drives`, beside `devices`, as they are the one
device an Ozzy has several of, in an order that names them: the first is `A:`.
Each is a map of the drive's settings, with the names the manual's Disk Drive
page gives them. Only `preset` is needed, so `{ preset: "floppy-1440k" }` is an
empty floppy drive:
- `preset`: the timing preset, which sets the read and write rates, the
  latency, and the read cache. It has no default, as a drive's kind is the
  first thing to say about it, and a floppy and a hard disk differ more than
  any default could hide. The names are the Ozzy's: `floppy-360k`,
  `floppy-720k`, `floppy-1440k`, `hd-10m`, `hd-20m`, and `hd-40m`.
- `read_cache_sectors`, `read_words_per_sec`, `write_words_per_sec`,
  `cache_words_per_sec`, and `latency_ms`: each changes what the preset gives,
  whatever order the keys are in, so `{ preset: "hd-20m", latency_ms: 30 }`
  is a 20 MB hard disk with a faster seek. The Disk Drive page's `latency`
  becomes `latency_ms`, as a config's number names its unit (as `tick_hz`
  does).
- `slots`, 4 by default.
- `image`: the path of the image in the drive at power on, or none for an
  empty drive. A relative path is from the config file's folder, so a config
  and its disks move together.

Errors, as the other devices' are:
- Read errors, with a line and column: more than 4 drives (`drives: must have
  0 to 4 values`, as Game Bits' `ValidateArray` words it), a drive with no
  `preset` (`drives[1]: missing preset`, as a device with no `type` is), a
  `preset` that isn't one of the names, an empty `image`, and the usual
  unknown keys and wrong types.
- The Ozzy's own rules, with no line: more than 4 drives in settings made in
  code (`drives: must have at most 4 drives`), and two drives with one image
  (`drive B: image disks/a.ozd is also drive A's`), comparing the paths as
  normalized text.
- The drive's own checks (slots, and rates and latency in range), from
  `DiskDriveFactory::Create`, naming the drive: `Disk drive A: has 0 slots,
  but it must have 1 to 255`.
- An image that doesn't open, from `Ozzy::Create`, naming the drive and
  giving `DiskImage::Open`'s message:
  `Drive A: Disk image disks/a.ozd doesn't exist`. The app prints it as any
  config error, and exits with 1.

### The machine table

Each drive is a device entry of type 5 (`MachineDeviceType::kDrive`), after
the timer, in letter order: its first port (the command port), its interrupt
and its cores (0001h, not shared), no bank or page, and as its two words of its
own, its letter (41h to 44h) and its slots. What disk is in it, if any, is the
drive's to say (`MEDIA` and `VOLUME`), as it changes while the Ozzy runs. An
Ozzy has at most 8 devices, well within the table's 16.

### `Ozzy`

- **`Ozzy::Create(settings, file_system)`** opens each drive's image in
  `file_system` with `DiskImage::Open`, before building anything, and attaches
  it to its drive at power on, so the drive reads the superblock in its first
  cycles. `file_system` must outlive the Ozzy, as the drives write to their
  images as it is destroyed. It may be null when no drive has an image, and a
  drive with an image and no file system is an error. A read-only host file is
  a write-protected disk, as `DiskImage::Open` has it.
- **`GetDrive(letter)`** returns the drive for `'A'` to `'D'`, or null if the
  Ozzy doesn't have it (or for any other character). The host attaches and
  detaches images through it between runs, as `DiskDrive`'s host interface
  says.
- **`SetClock(clock)`** sets the host's date and time for the timer and every
  drive, replacing `GetTimer()->SetClock` as the way a host gives it, so a
  host can't give the timer a clock and forget the drives. With none, the
  timer reads zeros and the drives stamp no date.
- **Shutting down:** destroying the Ozzy destroys its drives, and each writes
  back everything it hadn't committed, stamped with the clock, so the images
  stay consistent after a clean exit.

### The `ozzy` app

- It opens the config's images through its command line file system, so a
  path in the config is the operating system's (relative paths from the
  config's folder), and gives the Ozzy the host's local time with
  `SetClock`.
- The disks stay in their drives while it runs, and the drives write back
  what they hold when it quits, however it quits short of a crash (its quit
  keys, closing the window, or a signal, which all end its loop).
- While it runs, nothing else may change an image in a drive, including
  `oz3dsk`, as the manual's Disk Drive page says for any host.

### Manual

`4-Ozzy-Computer.md`: the drives in the introduction, the config's `ports`,
the wiring table, the `drives` list with the drive's keys and presets, errors,
the machine table's type 5 and its words, the default table's ports (30h), and
running `ozzy` with disks. `3.5-Disk-Drive.md`: `latency` becomes
`latency_ms`, and a line on where the Ozzy has its drives.

## Design

### Names

| Name                          | Candidates                                   | Avoids                                                       | Choice              |
| ----------------------------- | -------------------------------------------- | ------------------------------------------------------------ | ------------------- |
| A drive's settings in an Ozzy | `OzzyDriveSettings`, `OzzyDiskSettings`      | `DiskDriveSettings` (the device's, with its wiring)          | `OzzyDriveSettings` |
| The drives in `OzzySettings`  | `drives`, `disk_drives`                      | `devices` (the config's list of every device)                | `drives`            |
| Finding a drive               | `GetDrive(letter)`, `GetDiskDrive(letter)`   | `Processor::GetDevice` (by name)                             | `GetDrive`          |
| The drives' device names      | `A:` to `D:`, `Drive A` to `Drive D`         | `Disk drive` (the device's own word in its errors)           | `A:` to `D:`        |
| The host's time               | `SetClock`, `SetHostClock`                   | `Timer::SetClock` and `DiskDrive::SetClock` (one device's)   | `SetClock`          |
| The machine table's type      | `kDrive`, `kDiskDrive`                       | `kScreen` and the others, which are named for what they are  | `kDrive`            |

- The device names are the Ozzy's names for its drives, so the drive's own
  errors read as the user would say them: `Disk drive A: has 0 slots`.
- `GetDrive` takes the letter, as a host thinks of a drive by its letter, while
  `OzzySettings::drives` is a list in letter order, whose index is the drive's
  number, 0 for `A:`, as the config's `drives` is. The machine table's letter
  word is `'A'` plus that number.
- The config's preset names are the Ozzy's, as disk_design.md has them: the
  devices library names its presets only in C++ (`DiskDrivePreset`).

### ozzy: OzzyDriveSettings and the wiring

```
// ozzy_settings.h
// The drives, A: to D:. Drive n's ports start at kOzzyDrivePort +
// n * DiskDrive::kPorts, and its interrupt is kOzzyDriveInterrupt + n.
inline constexpr int kOzzyDriveCount = 4;
inline constexpr int kOzzyDrivePort = 16;
inline constexpr int kOzzyDriveInterrupt = 2;
inline constexpr uint8_t kOzzyDriveInterruptCores = 0x01;  // Core 0.

// The settings of an Ozzy's drive that a host sets: all of DiskDriveSettings
// but its wiring, with the same defaults, and the image in it at power on.
struct OzzyDriveSettings {
  int slots = DiskDriveSettings().slots;
  DiskHardwareSettings hardware = DiskDriveSettings().hardware;

  // The path of the disk image in the drive at power on, in the file system
  // Ozzy::Create is given, or empty for none.
  std::string image;
};

DiskDriveSettings GetOzzyDriveSettings(int drive,
                                       const OzzyDriveSettings& settings);

// Returns the letter of drive `drive`: 'A' for drive 0.
constexpr char GetOzzyDriveLetter(int drive);

struct OzzySettings {
  static constexpr int kMinPorts = 32;  // Ports 0 to 31 are for devices.
  int ports = 48;
  ...
  // The drives, A: first, at most kOzzyDriveCount of them.
  std::vector<OzzyDriveSettings> drives;
};
```

- `static_assert`s check that every drive's ports are below `kMinPorts` and
  above the keyboard's key state, and that the interrupts are every core's and
  differ from the other devices', as for the devices now.
- `ValidateOzzySettings` adds the drives' rules: at most `kOzzyDriveCount`,
  and no two drives with an image whose normalized paths are the same.

### ozzy: Ozzy

```
class Ozzy final {
 public:
  using Clock = absl::AnyInvocable<absl::CivilSecond()>;

  // The drives' names in the Processor: "A:" to "D:".
  static constexpr std::array<std::string_view, kOzzyDriveCount>
      kDriveNames = {"A:", "B:", "C:", "D:"};

  static absl::StatusOr<std::unique_ptr<Ozzy>> Create(
      const OzzySettings& settings, gb::FileSystem* file_system = nullptr);

  DiskDrive* GetDrive(char letter);  // Null without one.
  void SetClock(Clock clock);
};
```

- **Create** opens every image first, so a missing one fails before the
  processor is built, then adds the drives' factories after the timer's, in
  order, and attaches each image once the processor is made.
- **The machine table** is written from the processor config's devices in
  their order, as now: a drive's entry is found by its name in `kDriveNames`,
  for its letter, and its slots come from its settings.
- **The clock:** the Ozzy keeps the host's clock, and gives the timer and each
  drive a clock that calls it, or none when it has none, so "no clock" stays
  the devices' own "no date". The clock is declared before the processor, so
  it outlives the drives, which call it as they are destroyed.

**Brittleness:**
- `file_system` must outlive the Ozzy, which its comment says, as for
  `DiskImage::Open` and `DiskDrive`.
- A host that attaches images itself with `GetDrive` must not put one image
  in two drives; only the settings are checked.
- Paths are compared as text, so two spellings of one file (a relative and an
  absolute path, a difference in case where the file system ignores it, or a
  link) get past the check, as in `oz3asm` (see the backlog's *Same-file check
  through the file system*, which this item adds to).

### ozzy: reading drives

- `ReadOzzySettings(text, path)`: the second argument becomes the config
  file's path, which errors begin with, as now, and which relative image paths
  are from. Each image is normalized (`gb::NormalizePath`), rejected if it
  doesn't normalize, and joined to the config's folder (`gb::RemoveFilename`
  and `gb::JoinPath`) unless it is absolute (`gb::IsPathAbsolute`). A config
  read with no path keeps its relative paths as they are.
- `ReadDrives` reads the list with `ValidateArray(0, kOzzyDriveCount)`, and
  each drive reads `preset` into the hardware settings with
  `GetDiskDrivePreset` first, rejecting the drive if it has none, and only
  then each setting that changes it, as a map has no order.
- `OzzyDriveSettings` keeps its default hardware (the 1.44 MB floppy's, as
  `DiskDriveSettings` has it), so a host that builds settings in code needn't
  name a preset: requiring one is the config file's rule, where a drive's kind
  is spelled out.
- `latency_ms` reads as an int and becomes `absl::Milliseconds`. Ranges are
  the factory's to check, so no rule is written twice.

### apps: ozzy

`Run` passes the command line file system to `Ozzy::Create` (in CL2), and gives
the host time with `Ozzy::SetClock` in place of the timer's (in CL1).
`kProgram`'s help says the drives write back their disks when it quits.

### Testing

- `ozzy_settings_test.cc`: each drive's wiring from `GetOzzyDriveSettings`;
  `ports` of 31 rejected and 32 accepted, and 48 by default; two drives with
  one image rejected, also when written differently but normalizing the same
  (`disks/./a.ozd`), and two different images accepted; more than 4 drives
  rejected, and none, one, and 4 accepted. With CL2: reading
  every drive key; each preset name giving its `DiskDrivePreset`; a setting
  changing the preset whatever the keys' order; relative images from the
  config's folder, absolute ones kept, and kept as they are with no path; and
  an error naming the place for each mistake, including a fifth drive and a
  drive with no preset.
- `ozzy_test.cc`: `GetDrive` for every letter, null without one and for other
  characters; the machine table's drive entries (which give each drive's
  ports, interrupt, and cores as the processor has them), after the timer and
  in letter order, on core 0 however many cores, with two and with all four;
  an image attached at power on (the control port shows the disk once the
  drive has run); errors for a missing image, an image with no file system,
  and a drive setting the factory rejects, each naming the drive; `SetClock`
  reaching the timer's clock port, and none giving zeros. Images are in a
  `gb::MemoryFileProtocol`, formatted with `DiskFileSystem`.
- **A program on drive `A:`:** an assembled program on core 0 creates a file
  on `A:` and writes words to it through ports 16 to 18, without closing it.
  Destroying the Ozzy commits it: the file on the image has the words, stamped
  with the Ozzy's clock. So the wiring a program relies on, attaching at power
  on, and writing back at shutdown are tested end to end.
- `boot_rom_test.cc`: an Ozzy with all four drives still boots to its banner,
  with 8 devices in the table.
- By hand, with a disk made by `oz3dsk`: `ozzy` with a drive config runs, and
  a config with a missing image prints its error and exits with 1. Nothing
  reads the disks until the Ozzy OS.

### Performance

An idle drive sleeps until a port access wakes it, so drives cost nothing
until a program uses them, apart from the superblock read at power on.
Nothing is measured.

### To confirm

- That `gb::IsPathAbsolute` takes a Windows drive (`C:\disks\a.ozd`, which the
  command line file system reads as protocol `c`) as absolute, and that
  `gb::JoinPath` of an empty folder gives the path unchanged (CL2).

## CLs

### CL1 [x] ozzy: drives in the Ozzy

Depends on: nothing.

- `ozzy_settings.h` and `.cc`: the wiring, `OzzyDriveSettings`,
  `GetOzzyDriveSettings`, `drives`, `kMinPorts` 32, `ports` 48, and the image
  rule.
- `machine_table.h`: `MachineDeviceType::kDrive`, and the drive's words.
- `ozzy.h` and `.cc`: `Create` with a file system, `GetDrive`, `SetClock`,
  and the drives' machine table entries.
- `CMakeLists.txt`: `oz3_disk` and `gb_file` in `oz3_ozzy`'s deps.
- `configs/*.ozzy`: `ports: 48`, and the comment's range.
- `ozzy_main.cc`: the host's time with `Ozzy::SetClock`, in place of the
  timer's, so the app never gives the timer a clock and not the drives.
- Manual: the wiring, `ports`, and the machine table.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: as Testing above, but reading drives.

### CL2 [~] ozzy, apps: drives in config files and the ozzy app

Depends on: CL1.

- `ozzy_settings.cc`: reading the `drives` list, with presets by name and
  image paths from the config's folder.
- `ozzy_main.cc`: the file system to `Create`.
- Manual: the drive's keys and presets, the errors, running `ozzy` with
  disks, and `latency_ms` in the Disk Drive page.

**Verify**
- Standard checks.
- Unit tests: reading drives, as Testing above.
- By hand: as Testing above, left to the user.
