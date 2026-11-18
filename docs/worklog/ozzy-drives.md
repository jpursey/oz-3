# Ozzy drives

Drives `A:` to `D:` in the Ozzy, from [disk_design.md](../disk_design.md)
(Ozzy). Each is a `DiskDrive` ([disk-drive-device.md](disk-drive-device.md)),
wired the same in every Ozzy as its other devices are, so the Ozzy OS can boot
from `A:` knowing the drive's ports and interrupt as constants. A config file
lists the drives in letter order, giving each its timing and the disk image in
it at power on, the machine table lists them, and the `ozzy` app runs an Ozzy
with its disks in:

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
absl::StatusOr<OzzySettings> settings = ReadOzzySettings(text, config_path);
gb::Context context;
context.SetPtr<gb::FileSystem>(&file_system);  // The images are opened in it.
context.SetValue<HostClock>([] { return HostNow(); });  // The timer's and drives'.
absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
    Ozzy::Create(*settings, &context);  // Opens and attaches the images.
(*ozzy)->Run(absl::Milliseconds(16));
DiskDrive* b = (*ozzy)->GetDrive('B');
b->AttachImage(*DiskImage::Open(&file_system, "disks/other.ozd"));
ozzy->reset();  // Each drive writes back what it hasn't committed.
```

The manual's [Ozzy Computer](../manual/4-Ozzy-Computer.md) page is the
specification of what a config and a program see: the drives' wiring, the
`drives` list and its errors, the machine table's drive entries, and running
`ozzy` with disks. disk_design.md's Ozzy section matches it. This summary
covers how it is built, and the decisions behind it.

## Behavior

- **Wiring:** drive n is on ports 16 + 3n to 18 + 3n (its command, data, and
  control ports) and interrupt 2 + n. So the ports kept for devices grew from
  0 to 15 to 0 to 31: `ports`' minimum is 32 and its default 48, leaving
  programs the same 16 free ports by default, and ports 0, 5, and 28 to 31 for
  devices to come. ozzy_design.md's OS call ports moved to 32 to 39.
- **Interrupts on core 0 alone,** not shared: the OS's core, which boots and
  issues every drive command. A completion can be read only once, so on every
  core, a program's handler could take it from the OS. Another core can still
  use a drive by polling its control port.
- **Lettered in order:** the drives are their own list, `drives`, beside
  `devices`, as they are the one device an Ozzy has several of, and their order
  names them. So an Ozzy with drives always has an `A:`. A drive with no image
  keeps its place. An Ozzy has at most 4, and none by default.
- **Every drive in a config names its `preset`** (`floppy-360k`,
  `floppy-720k`, `floppy-1440k`, `hd-10m`, `hd-20m`, or `hd-40m`), as a
  drive's kind is the first thing to say about it. The drive's other settings
  (`read_cache_sectors`, `read_words_per_sec`, `write_words_per_sec`,
  `cache_words_per_sec`, `latency_ms`, and `slots`) change what it gives, in
  any key order, and a preset's latency is kept exactly unless `latency_ms` is
  given. In C++, `OzzyDriveSettings` defaults to the 1.44 MB floppy, so a host
  building settings in code needn't name one. The Disk Drive page's `latency`
  became `latency_ms`, as a config's number names its unit.
- **Images:** a relative `image` is from the config file's folder, so a config
  and its disks move together. `Ozzy::Create` opens every image before it
  builds the processor, and attaches them at power on, so each drive reads its
  superblock in its first cycles. A read-only host file is a write-protected
  disk.
- **Errors:**
  - read errors, with a line: more than 4 drives, a missing or unknown
    preset, an empty image, one that doesn't normalize, or one that can't be
    joined to the config's folder;
  - the Ozzy's own rules, with no line: more than 4 drives in settings made
    in code, and one image in two drives, comparing normalized paths;
  - the drive's own range checks, from `DiskDriveFactory::Create`, naming
    the drive (`Disk drive A: has 0 slots, ...`), as for every other device;
  - an image that doesn't open, from `Ozzy::Create`
    (`Drive A: Disk image disks/a.ozd doesn't exist`), or that has no file
    system to open it.
- **Machine table:** each drive is type 5 (`MachineDeviceType::kDrive`), after
  the timer, in letter order, with its command port, its interrupt on core 0
  (0001h), no pages, and its letter (41h to 44h) and slots. What disk is in it
  is the drive's to say, as it changes while the Ozzy runs.
- **The host's time:** the host puts one clock, or none, in the context it
  gives `Ozzy::Create`, which is also its processor's, where the timer and
  every drive find it, so a host can't give the timer one and forget the
  drives. The `ozzy` app gives the
  host's local time this way.
- **Shutting down:** destroying the Ozzy destroys its drives, each writing back
  what it hadn't committed, stamped with the clock. The `ozzy` app's quit keys
  and signals end its loop, so the drives write back. Closing the Windows
  console's window leaves only the few seconds Windows allows, which the
  manual warns of.
- **Ejecting** is a floppy pulled, as `DiskDrive` has it: what the file system
  hadn't committed is lost, as the OS commits with `CLOSE` and `FLUSH`, so
  `DiskDrive` has no host `Flush`. The `ozzy` app can't change disks while it
  runs (see the backlog's *Changing disks in ozzy*).

## Design

### Names

- **`OzzyDriveSettings`**, after `OzzyTimerSettings`: what a host sets about
  an Ozzy's drive, apart from the device's own `DiskDriveSettings`, which
  holds the wiring.
- **`OzzySettings::drives`**, beside the config's `devices`.
- **The drives' device names are `A:` to `D:`** (`Ozzy::kDriveNames`), the
  Ozzy's own names for them, so the drive's errors read as the user would say
  them: `Disk drive A: has 0 slots`.
- **`GetDrive(letter)`** takes the letter, as a host thinks of a drive by it,
  while `OzzySettings::drives` is indexed by the drive's number, 0 for `A:`,
  as the config's list is. `GetOzzyDriveLetter` turns one into the other.
- **The preset names** are the Ozzy's, as disk_design.md has them: the devices
  library names its presets only in C++ (`DiskDrivePreset`).

### ozzy: settings and wiring

- `ozzy_settings.h` holds the wiring with the other devices':
  `kOzzyDriveCount`, `kOzzyDrivePort`, `kOzzyDriveInterrupt`, and
  `kOzzyDriveInterruptCores` (core 0), with `GetOzzyDriveSettings` adding it
  to an `OzzyDriveSettings`. `static_assert`s check that the drives' ports sit
  after the keyboard's key state and below `kMinPorts`, and that their
  interrupts sit between the other devices'.
- `ValidateOzzySettings` adds the drives' two rules: at most
  `kOzzyDriveCount`, and no image in two drives.
- **Reading:** `ReadOzzySettings(text, path)` takes the config file's path,
  which errors begin with and whose folder relative images are from (none for
  an empty path or a file name alone, so they stay relative).
  `OzzySettingsReader` keeps the folder, and its `ReadDrive` reads `preset`
  first, then each setting that changes it, as a map has no order.
  `ReadImage` normalizes the path, and joins a relative one to the folder
  with `gb::JoinPath`, normalizing again for its `..`, as
  `ProgramAssembler::ReadInclude` does for includes. Ranges are left to the
  drive's factory, so no rule is written twice.

### ozzy: Ozzy

- `Ozzy::Create(settings, contract)` adds the drives' factories after the
  timer's, in order, opens every image (`OpenDriveImages`) once the processor
  config is valid and before the processor is built, and attaches them once
  the Ozzy is made. The file system they are opened in is the only thing the
  Ozzy's own contract declares (`kConstraintFileSystem`). It may be absent
  when no drive has an image, and must otherwise outlive the Ozzy.
- **The machine table** is written from the processor config's devices in
  order, a drive's entry found by its name in `kDriveNames`. A device it
  doesn't know is a `CHECK`, as indexing the drives past their end would be
  worse.
- **The clock** is in the context (`kConstraintHostClock`), which the Ozzy
  gives its processor, and keeps until after the processor's devices are
  destroyed, as the drives call it then. The Ozzy doesn't declare the clock,
  as only the devices need it: each declares only what it needs.

**Brittleness:**
- `file_system` must outlive the Ozzy, which its comment says.
- A host that attaches images itself with `GetDrive` must not put one image in
  two drives; only the settings are checked.
- Paths are compared as text, so two spellings of one file get past the check
  (see the backlog's *Same-file check through the file system*).

### apps: ozzy

`Run` passes its command line context to `Ozzy::Create`, which holds the
operating system's file system, so a config's paths are the operating system's,
and the host's local time as its clock, which `main` puts there. Its help says
the drives write back their disks when it quits.

### Testing

- `ozzy_settings_test.cc`: each drive's wiring from `GetOzzyDriveSettings`;
  `ports` of 31 and 32; 0 to 5 drives; one image in two drives, also written
  differently; and reading: every drive key, each preset name, settings before
  the preset still changing it, relative images from relative and Windows
  folders (with `..` and backslashes), absolute ones kept, no folder keeping
  them relative, and each error.
- `ozzy_test.cc`: `GetDrive` for every letter and other characters; the
  machine table's drive entries with two drives on a 4-core Ozzy and with
  every device; images in their drives at power on, with empty drives
  between; a config's images opened from its folder; each `Create` error;
  the clock given to `Create` reaching the timer's clock port, and none giving
  zeros. Images
  are in a `gb::MemoryFileProtocol`, formatted with `DiskFileSystem`.
- **End to end:** an assembled program on core 0 creates a file on `A:` through
  ports 16 to 18 and interrupt 2, and writes 4 words without closing it.
  Destroying the Ozzy commits the file, with its words, stamped with the
  Ozzy's clock. So the wiring a program relies on, attaching at power on, and
  writing back at shutdown are tested together.
- `boot_rom_test.cc`: an Ozzy with every device (8 in the table) boots to its
  banner.
- By hand: `ozzy` with a drive config made with `oz3dsk`, and its errors for a
  missing image and a missing preset. Nothing reads the disks until the Ozzy
  OS.

### Performance

An idle drive sleeps until a port access wakes it, so drives cost nothing until
a program uses them, apart from the superblock read at power on. The new tests
take under 200 ms in Debug. Nothing else was measured.
