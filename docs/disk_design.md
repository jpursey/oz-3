# OZ-3 Disk Drive

The disk drive is a smart drive in the spirit of the Commodore 1541: it owns the file system, and the processor talks to it in files, not sectors. It reads and writes disk images on the host, which the host attaches and detaches while the Ozzy runs.

This is the design the backlog items in Getting there build, as [assembler_design.md](assembler_design.md) and [ozzy_design.md](ozzy_design.md) are for theirs. It replaces ozzy_design.md's sketch of a drive on host folders. Each item settles its details in its own worklog plan.

The design has five parts, each built on the ones before, and then the Ozzy's drives:

- **Disk image:** a host file holding a flat array of 256-word sectors, numbered from 0 with 32-bit sector numbers. It knows nothing about files.
- **File system:** a superblock, a FAT with a built-in free list, and directories of 16-word entries, reached only by reading and writing whole sectors.
- **Disk library:** the one implementation of the image and file system, in `oz3/disk`.
- **Drive device:** three ports (command, data, and control) and an interrupt line, through which the OS opens, reads, writes, and manages files. It's built on the disk library.
- **Tools:** a `gb::FileProtocol` for host code and the `oz3dsk` command line tool, also built on the disk library.

A last section sketches how an OS might use the drive, as a check that the device is sufficient, without prescribing the OS's design.

The file system lives in the drive for now. Because it only touches the disk through sectors, a later "dumb" drive that exposes sectors, with the file system in the Ozzy OS, could use the same images unchanged.

The drive is meant to be driven by the OS: the OS issues every command and moves every word, streaming data into and out of a program's memory through the EXTRA bank. Programs ask the OS through calls. The drive trusts software and enforces no ownership, as era hardware did.

Conventions used throughout:

- A word is 16 bits. Sizes are in words (Kiw, Miw), with bytes (KiB, MiB) in parentheses where useful.
- Every size, position, and length on the disk and in the protocol is in words. The OZ-3 has no byte addressing.
- A dword is two words, low word first.
- Packed text is two characters per word, the first in the **low** byte, so a word's bytes on the host (low byte first) read in order. This design requires the assembler to pack strings this way, a change from its current high-byte-first packing (see Prerequisites).
- Reserved fields are 0 when written and ignored when read.

## Disk image

An image file is an 8-word container header followed by the sectors in order. The header describes the physical medium, and is never visible to the OZ-3 as data. Words are stored on the host two bytes each, low byte first, as in `.oz3` program files, so sector n starts at host byte offset 16 + 512 × n.

| Words | Field | What |
| --- | --- | --- |
| 0, 1 | Image ID | `"OZDK"` packed: 5A4Fh, 4B44h |
| 2 | Version | 1 |
| 3 | Sector size | In words; 256 |
| 4, 5 | Sector count | A dword; 1 to 4,194,433 |
| 6 | Flags | Bit 0: write-protected. Others reserved |
| 7 | Reserved |  |

- Sector size is fixed at 256 words (512 bytes). It is recorded so another size remains possible in a later version.
- The sector count is the disk's physical size. A file system may use fewer sectors; any beyond it are unused. The most is 4,194,433, the most a file system uses (the superblock, 256 FAT sectors, and 65,534 clusters of 64 sectors), since a bigger disk can't be formatted.
- Write protection belongs to the medium, like a floppy's notch, so it lives here rather than in the drive's configuration. The drive reports it through `MEDIA` and refuses changes to a protected disk.
- A read-only host file is a write-protected image too, whatever its header says: it is opened for reading only, and its flags have bit 0 set. The header's bit is the notch, which travels with the image, and the host's read-only attribute is the host's own protection, which the image never changes. Protection can't be cleared on a read-only host file.
- A blank image is all zero sectors: an unformatted disk.

## File system

### Disk layout

A formatted disk is the superblock in sector 0, then the FAT, then the clusters. Only the FAT's size varies, and the superblock records it.

| Sectors | What |
| --- | --- |
| 0 | Superblock (words 0–31) and boot area (words 32–255) |
| 1 to F | The FAT, F sectors |
| F + 1 to the end | Clusters 1 to the cluster count, including the root directory |

The file system allocates in clusters of 2^k sectors, set at format time, so 16-bit cluster numbers cover small floppies and large hard drives alike. Cluster n starts at sector 1 + F + (n − 1) × sectors per cluster. The disk uses 1 + F + cluster count × sectors per cluster sectors, which must not exceed the image's sector count.

| Disk | Size | Sectors per cluster | Clusters | FAT sectors |
| --- | --- | --- | --- | --- |
| 360 KB floppy | 180 Kiw (360 KiB), 720 sectors | 1 | 716 | 3 |
| 720 KB floppy | 360 Kiw (720 KiB), 1,440 sectors | 1 | 1,433 | 6 |
| 1.44 MB floppy | 720 Kiw (1440 KiB), 2,880 sectors | 1 | 2,867 | 12 |
| 10 MiB hard drive | 5 Miw (10 MiB), 20,480 sectors | 1 | 20,399 | 80 |
| 20 MiB hard drive | 10 Miw (20 MiB), 40,960 sectors | 1 | 40,799 | 160 |
| 40 MiB hard drive | 20 Miw (40 MiB), 81,920 sectors | 2 | 40,879 | 160 |
| 128 MiB hard drive | 64 Miw (128 MiB), 262,144 sectors | 4 | 65,471 | 256 |

Each row uses the fewest sectors per cluster that cover the whole image within 65,534 clusters, which is what `FORMAT` picks by default, and the most clusters that fit after the superblock and FAT: the largest C with 1 + ⌈(C + 1) ÷ 256⌉ + C × sectors per cluster at most the sector count. At most a few sectors are left over. With 64 sectors (16 Kiw) per cluster, the format reaches about 1 Giw (2 GiB).

### Superblock

The superblock is words 0–31 of sector 0, and records everything needed to find the rest of the file system.

| Words | Field | What |
| --- | --- | --- |
| 0, 1 | Format ID | `"OZFS"` packed: 5A4Fh, 5346h |
| 2 | Version | 1 |
| 3 | Sectors per cluster | A power of two, 1 to 64 |
| 4 | Cluster count | 1 to 65,534 |
| 5 | FAT sectors | At least (cluster count + 1) ÷ 256, rounded up, and at most 256 |
| 6 | Root cluster | The root directory's first cluster |
| 7 | Flags | Reserved |
| 8–15 | Volume label | Packed text of 0–16 characters from the name set, zero-padded |
| 16–19 | Formatted | Date and time in the timer's layout, or all zeros |
| 20–31 | Reserved |  |

Words 32–255 of sector 0 are the boot area: 224 words for future boot code, 0 for now.

- Sectors per cluster is stored as a count, and must be a power of two so code can shift.
- The label is plain packed text, not split into name and extension, with letters in upper case.
- No free-cluster count is stored, as it would go stale after a crash. A free count is derived by walking the free list.
- A disk is unformatted if its format ID or version is unknown, or its fields are out of range or don't fit the image.

### FAT and free list

The FAT is one word per cluster number. Every chain, whether a file's or the free list's, follows the same rules, and a zeroed FAT is a complete free list of every cluster.

| Word | Value | Meaning |
| --- | --- | --- |
| 0 | 0 | The free list starts at cluster 1 |
| 0 | n | The free list starts at cluster n |
| 0 | FFFFh | No free clusters: the disk is full |
| n ≥ 1 | 0 | Next is n + 1, or the end if n is the last cluster |
| n ≥ 1 | m | Next is cluster m |
| n ≥ 1 | FFFFh | The end of this chain |

Clusters are numbered from 1, so no real link is ever 0, and a 0 is never ambiguous. Word 0 is treated as the link of an imaginary cluster 0, so the same rule gives the free list's head. Whether a cluster is free depends only on which list reaches it.

- **Allocating:** take the head cluster, set the head to that cluster's next, and write the cluster's own entry (FFFFh, or a link once its file grows). Writing the entry is required: it keeps a 0 from surviving on an allocated cluster, so a 0 at n always means n + 1 is free and next.
- **Freeing a chain:** walk to its last entry, set that entry to the old head's cluster number, and make the chain's first cluster the new head. The old head is the cluster word 0 refers to, not its raw value, or FFFFh when the disk was full. On a formatted disk word 0 is never 0, as formatting gives cluster 1 to the root, which is never freed, so the head is always 2 or more.
- **Formatting:** zero the FAT, allocate the root as cluster 1 (FFFFh at word 1, and 2 at word 0), and zero the root's sectors.
- **Reaching a position:** walk the chain one cluster per (cluster size) words. Each open file remembers its current cluster, so sequential access never walks.

There is one FAT and no copy. An image has no bad sectors, and a copy written the same way would usually be corrupted the same way. Entries don't say whether they are free, so a consistency check walks the free list and every file to find orphaned or cross-linked clusters, and compares each chain's length with its file's size.

### Directories

A directory is a cluster chain of 16-word entries, 16 to a sector. The root directory is the only one for now, and starts at the superblock's root cluster. Its size is its chain's full length.

| Words | Field | What |
| --- | --- | --- |
| 0–7 | Name | The 12.4 name field (see Names and patterns) |
| 8 | Attributes | Bits, below |
| 9 | First cluster | 0 for an empty file |
| 10, 11 | Size | A dword, in words, at most what the clusters other than the root's first hold |
| 12–15 | Modified | Date and time in the timer's layout, or all zeros |

| Attribute bit | Meaning |
| --- | --- |
| 0 | Read-only |
| 1 | Hidden |
| 2 | Directory: reserved, always 0 in version 1 |
| 3–15 | Reserved |

- **Free entries** have a first name word of 0. No valid name starts with a zero byte, so a zeroed cluster is all free entries, and free entries may sit anywhere in the chain.
- **Entries never move.** An entry stays in its slot for its file's life, so an open directory handle stays valid while files are created or deleted.
- **Growing:** a directory with no free entry gains a zeroed cluster from the free list. Directories never shrink in version 1.
- **Files:** a file of s words has exactly ⌈s ÷ cluster size⌉ clusters in its chain. A size of 0 has first cluster 0 and no chain.
- **Uniqueness:** no two entries in a directory have the same name field.

#### Timestamps

The timer gives the date as two words (the year, then the month in the high byte and the day in the low) and the time as two (the hour, then the minute high and the second low). Stored date first, the four words compare in order chronologically, and a program can copy the timer's values straight in.

- Only the modified time is kept. A creation time would push entries to 20 words.
- All zeros means no date, since month 0 is invalid. A drive with no clock, or a tool that doesn't know the date, writes zeros.
- The drive stamps a file when it closes it after a write, using the host clock, so a file written in many requests gets one stamp.

#### Subdirectories later

The format leaves room for subdirectories without changing version 1 disks. A subdirectory would be an entry with the directory bit set, whose chain holds entries in exactly this format, opened with `OPEN` like any file. Because the root is already a chain rather than a fixed region, the code that reads the root reads any directory, and `/` and `\` are kept out of names for use as path separators.

### Names and patterns

A name is two separate packed fields, a 1–12 character name and a 0–4 character extension, each zero-padded to whole words. No dot is stored. The same 8-word field appears in directory entries and in command arguments, so the drive never parses dotted text.

| Words | Field | What |
| --- | --- | --- |
| 0–5 | Name | 1–12 characters, zero-padded |
| 6, 7 | Extension | 0–4 characters, zero-padded |

- **Characters:** letters, digits, and the DOS punctuation ``! # $ % & ' ( ) - @ ^ _ ` { } ~``. Letters are stored in upper case, and names are matched without regard to case.
- **Excluded:** `.`, `/`, `\`, spaces, and everything else. The dot exists only in display and in text a program parses.
- **Padding** is zero bytes, and no zero byte may come before a character within a field.
- **Order:** names sort by comparing their 16 bytes in order, which sorts by name and then extension. Since the first character of each pair is in the low byte, comparing the words themselves doesn't sort; code compares each word with `CMPT`, which swaps both words' bytes before comparing, so it is one instruction a word.
- In assembly, a name field is two directives, `.DW SIZE(6) "HELLO"` then `.DW SIZE(2) "TXT"`, displayed as `HELLO.TXT`.

A **pattern** is a name field that may also hold `?` and `*`, matched against each field separately, byte by byte:

- `?` matches any one byte, padding included.
- `*` fills the rest of its field with `?`. Any character after a `*` in the same field makes the pattern invalid.
- So `*.*` matches every file, `*.TXT` every file with extension TXT, `A*.*` every name starting with A, and `*` alone every file with no extension.

Every name and pattern character is below 80h, and the matching is a fixed loop over 16 bytes, simple enough for an OS to do itself if it reads directories raw.

## Disk library

One library, `oz3/disk` (`oz3_disk`), is the only implementation of the format. The drive device, `DiskFileProtocol`, and `oz3dsk` all use it, so they can never disagree about what's on a disk. Its names carry a `Disk` prefix, so they aren't confused with general file concepts.

It uses only `core` in OZ-3, for `DateTime`, and Game Bits (`gb_file`). It is its own library rather than part of `devices` only so that `tools` can use it. So it comes right after `core` in the libraries' order: `core` → `disk` → `tools` → `instruction_sets` → `devices` → `ozzy`, then `apps`.

| Class | What |
| --- | --- |
| `DiskMedia` | The medium, from the image's header: the sector count and flags, and setting write protection. The base of `DiskImage`, and what the hardware and file system hand out, so nothing else reads or writes the image's sectors |
| `DiskImage` | An image: created or opened with a `gb::FileSystem` and a path, owning its `gb::File`. Sector reads and writes, and its header as a `DiskMedia` |
| `DiskHardware` | Owns the attached `DiskImage`, if any, and behaves as a dumb drive: sector reads and writes through an optional write-through read cache, and the time of each, from the read rate, write rate, cache rate, and latency. A future raw drive device would be this behind ports |
| `DiskFileSystem` | Owns a `DiskHardware`, and takes the open-file limit. Holds its own buffers, and does mounting, formatting, the FAT and free list, directories, the open-file table, and every file operation |
| `DiskFilename` | A value: the 8-word 12.4 field. Parses and formats dotted text, validates, and matches |
| `DiskLabel` | A value: the 8-word volume label. Parses, formats, and validates |
| `DateTime` | In core: the 4-word date and time, as the timer answers it and timestamps store it |
| `DiskDirEntry` | A value: a 16-word directory entry, with getters and setters, returning its name as a `DiskFilename` |
| `DiskFile` | A value: a handle to an open file, the number of its slot in the open-file table |
| `DiskStatus` | An enum of the drive's status codes, `kOk` included |

The public surface is `DiskMedia`, `DiskImage`, `DiskHardware`, `DiskFileSystem`, and the value types. The layers split cleanly. `DiskHardware` is a dumb drive, always consistent because every write goes straight through, and it has no notion of pinning or committing. `DiskFileSystem` decides what it holds in memory, when it writes, and in what order.

- **One owner per image.** `DiskFileSystem` owns the `DiskHardware`, which owns the `DiskImage`, so nothing else can write the image behind the file system's back.
- **Patterns are filenames.** `DiskFilename::ParsePattern("*.TXT")` expands each `*` into `?` bytes at construction, and `Matches(const DiskFilename& name)` is a byte compare where `?` matches anything. A plain parse rejects `?` and `*`, so a pattern can't be mistaken for a name.
- **Files are handles, like `FILE*`.** `DiskFileSystem` keeps the open-file table and does every operation, as in `Read(DiskFile, span)`, so it can refuse to delete an open file, give open files' sizes in listings, and write back at close. The caller sets the table's size: the drive passes its slot count, and tools a large one. A handle is only its slot's number, as the drive's are, so a handle kept past its close reaches whatever is opened in that slot next, which is the caller's bug to avoid.
- **Stale slots are the library's.** A file open when its image is detached stays open but stale: every operation on it fails with **disk changed**, except closing it, which frees the slot. So a tool swapping images can't write into the wrong disk, and the drive's slots need no state of their own.
- **Buffers are explicit.** `DiskFileSystem` holds one sector per open file, one directory sector, and two FAT sectors, one fixed on sector 1. There's no generic cache between it and the hardware, so it always knows exactly what it holds and when it's written, and its memory is fixed by its open-file limit.
- **Commits are explicit.** `DiskFileSystem` writes its buffers at the points the drive needs: at the end of every directory-changing operation, in `Close`, `Flush(DiskFile)`, and `Truncate(DiskFile)`, and when a dirty buffer is reused, always in the crash-safe order. `Flush()` with no file writes everything, for a clean shutdown.
- **Per-sector work.** A read or write moves at most up to the next sector boundary per call, so the drive can stream a sector at a time, and a failure lands at the right point.
- **Time belongs to the hardware.** `DiskHardware` adds up the time of the operations it performs, without waiting. The drive reads and resets that total after each file system call, and waits that long before going on. Tools ignore it.
- **No clock.** Timestamps come in as core's `DateTime` values, from the host clock in the drive and from host file times in tools.
- **Consistency checks** are `DiskFileSystem::Check`, reporting orphaned and cross-linked clusters and chains that don't match sizes, and optionally repairing them.
- **Host transfer helpers** convert between host bytes and words, two bytes per word, low byte first, which is also how strings are packed. Text conversion only adds line endings (host endings to LF going in) and dropping the zero bytes that pad an odd length, for `oz3dsk` and any frontend.

The drive maps its slots one to one onto `DiskFile` handles, and its `DiskFileSystem` lives as long as it does, with images attached and detached as disks are inserted and ejected. Detaching an image discards the file system's buffers with no write-back, and leaves its open slots stale.

## Drive device

### The drive

#### Configuration

| Setting | What | Default |
| --- | --- | --- |
| Port | The base port; the drive uses three ports from it | — |
| Interrupt | The interrupt line raised on completions | — |
| Slots | How many files can be open at once | 4 |
| Read cache | Sectors in the hardware's write-through read cache; may be 0 | 4 |
| Read rate | Words per second read from the disk | Preset |
| Write rate | Words per second written to the disk | Preset |
| Cache rate | Words per second copied out of the read cache | Preset |
| Latency | Fixed time for a transfer that isn't continuous with the previous one | Preset |

The drive has no drive letter, which is the Ozzy's concept, and no image path. The host attaches and detaches images at runtime. The drive also takes a clock from the host, the same as the timer's, for timestamps. A drive with no clock stamps zeros, as a timer with no clock reads zeros (see Prerequisites).

#### Timing presets

A drive's configuration can name a preset for its read rate, write rate, cache rate, and latency, and override any of the four individually. Real drives varied a lot with controller, interleave, and model, so these are rough approximations of typical effective speeds, not any specific drive. A drive with no timing settings uses `floppy-1440k`.

| Preset | Approximates | Image size | Read rate | Write rate | Latency |
| --- | --- | --- | --- | --- | --- |
| `floppy-360k` | 5¼″ 360 KB double-density floppy (1981) | 180 Kiw | 8 Kiw/s | 6 Kiw/s | 150 ms |
| `floppy-720k` | 3½″ 720 KB double-density floppy (1986) | 360 Kiw | 10 Kiw/s | 8 Kiw/s | 120 ms |
| `floppy-1440k` | 3½″ 1.44 MB high-density floppy (1987) | 720 Kiw | 20 Kiw/s | 16 Kiw/s | 100 ms |
| `hd-10m` | 10 MB hard disk, PC/XT class (1983) | 5 Miw | 40 Kiw/s | 40 Kiw/s | 85 ms |
| `hd-20m` | 20 MB hard disk (1984) | 10 Miw | 64 Kiw/s | 64 Kiw/s | 65 ms |
| `hd-40m` | 40 MB hard disk, AT class (1986) | 20 Miw | 128 Kiw/s | 128 Kiw/s | 40 ms |

- **Rates** are sustained transfer rates, after interleave and rotation, not the raw bit rate of the medium.
- **Latency** is a typical average access: a seek plus half a rotation.
- **Cache rate** is 1,000,000 words per second (976 Kiw/s) for every preset, a 1 MHz OZ-3 class controller copying a word a cycle: about 256 µs a sector. It is small next to any disk, but not free.
- **Image size** is the matching size for `oz3dsk new`. The drive itself takes its size from the image, so a preset doesn't limit which images it accepts.
- At the Ozzy's default 1 MHz, the OS's streaming loop moves roughly 100 Kiw/s a word at a time, or about twice that with packed transfers. The floppies are far slower than the processor, while the fastest hard disk presets approach its limits, much as on real machines of the time.

#### Memory

The drive is no more powerful than the machine it serves. Its memory is a small fixed budget in two layers, matching the library's: the file system's own buffers, and the hardware's cache. That's about 2.75 Kiw with the defaults.

- **The file system's buffers:** one sector per slot, one directory sector, and two FAT sectors, one of which always holds sector 1, where the free list's head lives. That's slots + 3 sectors, 7 with the defaults. These are the only places the file system holds sectors, and nothing writes them but the file system itself.
- **The hardware's read cache:** a write-through cache of recently read and written sectors, 4 by default and possibly 0. Every write goes straight to the disk, so the cache only saves repeated reads, and the disk is always exactly what has been written to it.
- **A few words of state per slot:** the file's position, size, current cluster, and where its directory entry lives.

A slot's buffer holds the sector at its position: file data, or for a directory handle, directory entries. The directory buffer serves directory changes and the entries of files being opened and closed. The FAT's working buffer serves every chain walk and allocation, for all files. Before a buffer takes another sector, the file system writes it if it's dirty. A big directory is slow to search, since its sectors pass through one buffer, though the read cache softens repeated passes.

#### Committing

The file system's dirty buffers are the drive's only unwritten state, so a **commit** is simply the file system writing them. It happens at known points:

- **Every command that changes a directory** (`CREATE`, `DELETE`, `RENAME`, `ATTRIB`, `LABEL`, and `FORMAT`) writes its dirty directory and FAT buffers before completing. Work spanning many entries, like a wildcard delete in a tool, does everything it can within each directory sector before writing it.
- **`CLOSE`** writes the file's dirty buffer, the dirty FAT buffers, and its directory entry.
- **`FLUSH`** does the same as `CLOSE` without closing, so a program can make sure its data is on the disk while keeping the file open.
- **`TRUNCATE`** writes the file's new size, the FAT marking its chain's new end, and the freed clusters, in that order, before completing, since it frees clusters (see Data commands).
- **`WRITE`** doesn't commit by itself, so a run of writes isn't charged a commit each time.
- **Reusing a buffer** for another sector writes it first if it's dirty. This is a forced commit of that one buffer, and the only way a `WRITE`'s data reaches the disk before `FLUSH` or `CLOSE`.

The file system orders its writes so a crash can't cross-link files. The principle: a cluster is taken off the free list on disk before anything links to it, and is unlinked from everything on disk before it joins the free list. Since nothing but the file system writes its buffers, the order of its writes is the order they reach the disk:

- **Sector 1 before the working FAT sector:** after an allocation has changed both, the free list's head is written first.
- **FAT before directory** when clusters are newly linked into a file.
- **Directory before FAT** when clusters are freed: `DELETE` and `TRUNCATE` write the directory entry before freeing the chain.
- **Data before size:** a file's data is written before the directory entry that grows its size to cover it, so a file's commit writes its data, then the FAT, then its entry. An open file's entry is updated only by its own `CLOSE` or `FLUSH`, never by another file's commit.
- **Directory clusters before their links:** a new directory cluster is zeroed and written before the FAT link that adds it to the directory's chain, since a directory has no size to stop reads.

The worst a crash can leave is orphaned clusters or slack, which a check reclaims. Each write is charged as usual.

Files written at once share the FAT's working buffer, so writing it for one file can write another's new links before that file's data or size. That is safe: the other file's recorded size hasn't grown, so its chain on disk is simply longer than its size requires. Nothing reads past the size, and the extra clusters aren't on the free list, so a crash leaves them as slack that a check trims. When a file grows, the drive follows any links already past its size before allocating new clusters, so slack left this way is reused rather than leaked.

An open file's size lives in its slot, and its directory entry is written at `CLOSE`. When a listing meets the entry of an open file, the drive gives the slot's size instead, so nobody sees a stale size.

#### Timing

Time is charged by the hardware layer, for sector transfers and reads from its cache only. Moving words between buffers and ports, searching memory, and editing buffers are free.

- Reading a sector costs 256 ÷ read rate, or 256 ÷ cache rate if it's in the read cache, with no latency. Writing a sector always costs 256 ÷ write rate.
- A transfer that isn't continuous with the previous one, meaning a different sector than the next in order or a switch between reading and writing, first costs the fixed latency. This is a simplified seek, with no tracks. Reads from the cache don't move anything, so they don't affect continuity.
- While a sector is in a slot's buffer, the drive puts each next word in the data port, or takes each word from it, before the processor can ask. A streaming loop only waits at sector loads and writes.

#### Images and disk changes

- **Attaching:** the drive reads the image's container header right away, for `MEDIA` and write protection, then reads and checks the superblock, charging its time as usual. A command that arrives meanwhile simply waits for the read to finish. FAT sectors are read only when an operation first needs them, typically when clusters are allocated or freed.
- **Detaching:** the drive discards its caches and buffers, including unwritten data, as pulling a real floppy would. Every open slot becomes stale, and a command in progress completes with **no disk**.
- **Stale slots:** every command on a stale slot fails with **disk changed**, except `CLOSE`, which frees the slot and succeeds. A program can't keep writing into a different disk by accident.
- **Shutting down:** when the Ozzy app shuts down cleanly, the drive writes back everything dirty, so the image stays consistent.
- **No outside changes:** while an image is attached to a drive, nothing else may modify it, including `oz3dsk` or another program on the host. This is a documented contract, not enforced across processes. The drive's buffers, open files, and free count all assume it's the only writer. A way for a running Ozzy to cooperate with `oz3dsk` is out of scope for this design.

### Ports

| Port | Use |
| --- | --- |
| base | Command: command words, arguments, statuses, and results |
| base + 1 | Data: the words of a `READ` or `WRITE` |
| base + 2 | Control: the drive's state when read; requests when written |

The command and data ports use the OZ-3's port handshake. A write (`OUTS`, `OUTR`) succeeds only when the port's flag is clear, and sets it; the drive takes the word and clears the flag. A read (`INS`, `INR`) succeeds only when the flag is set, and clears it; the drive then puts the next word there. Each port is a single mailbox, so a word waiting in it can't be skipped or overwritten.

On the data port, ordinary transfers use word 0 of the port, one word per handoff. Packed transfers use both words, the low word in word 0.

On the command port, each field moves in its natural width. A word field (a command word, a status, a count) is one handoff in word 0. A dword field (a position or a size) is one dword handoff, the low word in word 0. The 8-word fields (names, patterns, and labels) are 4 dword handoffs, and `FIND`'s 16-word directory entry is 8, with the lower-numbered word of each pair in word 0. Dword and multi-word fields are only ever moved whole, so word handoffs would gain nothing.

#### Control port: reading

The drive keeps the control port's value current without setting its flag, so a plain `IN` reads the drive's state at any time and disturbs nothing.

| Bits | Field | What |
| --- | --- | --- |
| 0–3 | Phase | Where the current command is, below |
| 4 | Disk | 1 if an image is attached |
| 5–7 | — | Reserved |
| 8–15 | Command | The current command's code, or 0 when idle |

| Phase | Meaning |
| --- | --- |
| 0 | Idle: waiting for a command word |
| 1 | Arguments: waiting for more argument words |
| 2 | Working: busy, with nothing to send yet |
| 3 | Sending: words waiting on the command port |
| 4 | Reading: streaming data to the data port |
| 5 | Writing: taking data from the data port |

#### Control port: requests

The OS writes a request word with `OUTS`, setting the port's flag. Each bit is a request; the drive acts on them, clears the flag, and restores the state word. Writing 0 requests nothing.

| Bit | Request |
| --- | --- |
| 0 | Reset |
| 1–15 | Reserved, such as a future interrupt enable |

**Reset** aborts whatever is in progress, silently and with the same outcomes as an abort by a command word (see Aborts), withdraws any words waiting on the command and data ports, clears both ports' flags, and returns the drive to idle. It works in every phase, so it is the one sure way back to a known state.

To know a request is done, the OS tests the port's flag with `TSTP`, which sets **S** to a port's flag without loading or clearing anything. The drive never sets the control port's flag itself, so a set flag can only mean the OS's request is still waiting. When the drive finishes a request, it restores the state word and clears the flag in the same step. So a reset is:

1. The OS writes 1 to the control port with `OUTS`.
2. `TSTP` on the control port until **S** is clear. The reset is complete, the drive is idle, and the state word is current.

Don't `IN` the control port while a request is pending: `IN` clears the port's flag, which would hide the request. This is also why the handshake needs `TSTP` rather than a "resetting" bit in the state word: an `IN` that ran before the drive took the request would clear the flag, so the drive would never see it, and would read back the request word the OS just wrote instead of the state.

### Command protocol

A command word holds the command code in its high byte and a handle in its low byte, or 0 for commands without one. A **handle** is a slot index, 0 to slots − 1; FFh means every slot, for `CLOSE` only. Each command takes a fixed number of argument words, and returns a fixed number of result words after an OK status, so the OS and the drive never disagree about where a sequence ends. A status other than OK ends the command's words on the command port: no results, n, or completion follow it. The only words that can come after one are on the data port, when a transfer fails partway and its data phase runs out its n handoffs (see Early completion).

A command runs in up to three phases:

1. **Accept.** The OS writes the command word and its arguments. The drive checks everything it can without the disk (the handle, the arguments, the slots, write protection) and puts a status on the command port for the OS to read, with no interrupt. An error ends the command. **Synchronous** commands, which never need the disk, put their results here too and end.
2. **Data.** For `READ` and `WRITE` only: the words of the transfer move through the data port.
3. **Completion.** For **latent** commands: the drive does the work, then puts the completion status and any results on the command port for the OS to read, and **raises its interrupt**.

**Waiting for the accept status.** The command port's single flag serves both directions, so right after the OS writes a command's last word, the flag is set by that word, and an `INS` could read it back as if it were the status. So after the last word (the command word itself, for a command with no arguments), the OS reads the control port until its phase is 2 or higher, meaning the drive has taken the word. Only then does it read the command port. The control port's state word never sets a flag, so this check can't race. Later reads in the same command don't need it, since the flag is then only ever set by the drive.

The interrupt has exactly one meaning: the current command's completion is ready. Only one command is ever in flight, and the OS can't write a new command word over an unread status, so the interrupt always belongs to the last command the OS issued. An OS can `WAIT` after a latent command's accept, and let the drive's interrupt handler wake it.

#### Aborts

The reliable way to abandon a command is a **reset** through the control port (see Control port: requests), which works in every phase: it withdraws any word waiting on the command and data ports, clears both ports' flags, and returns the drive to idle.

A new command word also aborts the current command, but only when the command port is idle, with no word waiting in it from either side. That is in phase 2 (working), 4 (reading), and 5 (writing). If the OS's `OUTS` of a command word succeeds in one of those phases, the drive abandons the current command, silently, and starts the new one with that word. The OS chose to abandon it, so it isn't told, and no completion is sent for it.

In other phases a new command word can't abort:

- **Phase 3 (sending):** a word from the drive is waiting on the command port, so `OUTS` fails with **S** clear. The OS reads the waiting words first, or resets. An unconditional `OUT` replaces the waiting word, but the port's flag was already set, so the drive can't tell the port changed hands: it treats the word as its own, still unread, and the command word is lost. The OS never `OUT`s to the command port.
- **Phase 1 (arguments):** the drive takes the word as the command's next argument, since nothing tells the two apart. A confused OS that stops partway through a command's arguments can't be caught this way, which is what reset is for.
- **Phase 0 (idle):** there is nothing to abort, and the word starts a command as usual.

Whether by a command word or a reset, an abort has the same outcomes:

- **Unsent statuses and results** are discarded.
- **`READ`:** streaming stops, any word waiting on the data port is withdrawn, and the handle's position is just past the words the OS actually took.
- **`WRITE`:** the words the drive has taken are kept, as if the write had been for that many words, and the size is updated. A word still waiting untaken in the data port is discarded.
- **Any other latent command** finishes its disk work, so a change is never left half done, and its completion is dropped.

The OS can check what happened afterward, with `INFO` or `FIND`.

#### Moving words

`OUTR` and `INR` suit every phase: the OS lays out a command's words in memory and sends each run of fields with the matching width, such as a name field with one `OUTR.ID` of 4 dwords, and reads statuses and results the same way, such as a directory entry with one `INR.ID` of 8 dwords. Each stops when the port isn't ready and sets **S** only when R7 reaches 0, so a loop of the instruction and a jump on **S** is enough for arguments and results, which the drive always delivers in full once started. The data phase needs one more check, described in Reading and writing.

### Commands

Codes are grouped by kind, with the low nibble numbering commands within a group. Code 0 is never a command, so the control port's command field reads 0 when idle, and an unknown code fails at accept with **bad command**. "h" marks commands that take a handle in the command word's low byte; for the rest it is 0.

#### Files

| Code | Command | Arguments (OS writes) | Accept (OS reads) | Completion (OS reads) |
| --- | --- | --- | --- | --- |
| 10h | OPEN | name (8) | status | status, handle |
| 11h | OPEN ROOT | — | status | status, handle |
| 12h | CREATE | name (8) | status | status, handle |
| 13h | CLOSE h | — | status | status |
| 14h | FLUSH h | — | status | status |

- `OPEN` of a file that is already open fails with **in use**; two slots on one file would clobber each other. `OPEN ROOT` is the same for the root, which is open in at most one slot like any file.
- `CREATE` fails with **exists** if the name is taken; to replace a file, delete it first. It makes an empty file and opens it.
- `CLOSE` writes back the slot's dirty sectors and its directory entry, stamping the modified time if the file was written. `CLOSE` with h = FFh closes every slot. `CLOSE` on a stale slot frees it and succeeds.

`FLUSH` commits the slot's dirty data sector, the FAT sectors its writes changed, and its directory entry, leaving the file open (see Committing). On a directory handle it does nothing and succeeds.

#### Data

| Code | Command | Arguments (OS writes) | Accept (OS reads) | Data | Completion (OS reads) |
| --- | --- | --- | --- | --- | --- |
| 20h | READ h | count | status, n | OS reads n words | status, words valid |
| 21h | WRITE h | count | status, n | OS writes n words | status, words written |
| 22h | READ PACKED h | count | status, n | OS reads n dwords | status, dwords valid |
| 23h | WRITE PACKED h | count | status, n | OS writes n dwords | status, dwords written |
| 24h | SEEK h | position (dword) | status | — | — |
| 25h | INFO h | — | status, position (dword), size (dword) | — | — |
| 26h | TRUNCATE h | — | status | — | status |

- Counts are 0–65,535 words, or dwords for the packed commands. Directory handles can be read but not written.
- `SEEK` and `INFO` are synchronous. `SEEK` only checks and records the position, which may be anywhere from 0 to the size; the next transfer walks the chain to it, charging that time then.

`TRUNCATE` sets the file's size to its current position and frees the clusters beyond what that size needs. Since `SEEK` can't pass the end, it only ever shrinks a file; truncating to empty is `TRUNCATE` at position 0. It fails at accept on a directory handle, a read-only file, or a write-protected disk. The words between the new size and the end of its last sector aren't cleared: reads stop at the size, and growing the file again writes over them before the size covers them.

`TRUNCATE` commits before completing, unlike `WRITE`. If the freed clusters joined the free list only in memory, another file could take one and commit while this file's entry and chain on disk still claimed it, and a crash would leave a cross-link. So it writes the directory entry with the new size, then the FAT marking the chain's new end, then the free list taking the freed clusters.

#### Directory

| Code | Command | Arguments (OS writes) | Accept (OS reads) | Completion (OS reads) |
| --- | --- | --- | --- | --- |
| 30h | FIND h | pattern (8) | status | status, entry (16) |
| 31h | DELETE | name (8) | status | status |
| 32h | RENAME | old (8), new (8) | status | status |
| 33h | ATTRIB | name (8), attributes | status | status |

- `FIND` takes a directory handle and scans forward from its position for the next entry matching the pattern. It returns the raw 16-word entry, with an open file's current size, and leaves the position just past it. At the end it fails with **not found**. `SEEK h, 0` restarts a listing.
- `DELETE`, `RENAME`, and `ATTRIB` fail with **in use** if the file is open. `DELETE` also fails on a read-only file.
- `ATTRIB` sets read-only and hidden; any other bit is **bad attributes**.

#### Disk

| Code | Command | Arguments (OS writes) | Accept (OS reads) | Completion (OS reads) |
| --- | --- | --- | --- | --- |
| 40h | MEDIA | — | status, sector count (dword), flags | — |
| 41h | VOLUME | — | status | status, label (8), sectors per cluster, cluster count, free clusters |
| 42h | FORMAT | label (8), sectors per cluster | status | status |
| 43h | LABEL | label (8) | status | status |

- `MEDIA` is synchronous and describes the medium from the image's header: its sector count and the header's flags word, with bit 0 meaning write-protected. It works on unformatted disks.
- `VOLUME` describes the file system, and fails with **unformatted** if there isn't one. Its free clusters come from the free count (see Free count below), so it is quick unless it is the first command to need the count.
- `FORMAT` uses the image's full sector count, blanking the superblock, then writing the zeroed FAT, the root cluster, and last the superblock (with the formatted date), each sector charged. A format that fails partway leaves the disk unformatted. Sectors per cluster must be a power of two from 1 to 64, large enough that at most 65,534 clusters cover the image; 0 lets the drive pick the smallest that does. It fails with **in use** if any slot is open.
- `LABEL` rewrites the superblock's label.

#### Free count

The drive keeps a count of free clusters in memory, never on the disk. The first command after an image is attached that needs it counts them by walking the whole free list, which is slow on a big, fragmented disk, as DOS's was, and from then on the drive keeps it current as it allocates and frees. The commands that need it are `OPEN`, `CREATE`, and `VOLUME` (and `FORMAT`, which sets it), so every file handle is opened with the count known, and a `WRITE` can work out at accept how much fits without touching the disk. An `OPEN` on a write-protected disk doesn't need it, since nothing can be written.

#### Where errors land

- **At accept:** anything knowable without the disk: bad command, bad handle, disk changed, bad name, bad pattern, bad label, bad attributes, too many open, past end, wrong handle type, read-only (for `WRITE` and `TRUNCATE`), write protected, in use (for `FORMAT`), no disk, and disk full for a `WRITE` where n would be 0.
- **At completion:** anything that needs the disk: not found, exists, in use, read-only (for `DELETE`), disk full (a partial write, or a directory that can't grow), unformatted, corrupt, I/O error, and no disk if the image disappears mid-command. For `READ` and `WRITE`, a failure in the disk work, allocating included, is an early completion.

### Reading and writing

#### READ

An example: handle 1 is open on a 300-word file at position 0, and the OS asks for 500 words.

1. The OS writes `READ` with handle 1, then the count, 500, to the command port.
2. The drive works out n, the lesser of the count and the words left in the file: 300. This needs no disk access. The OS reads OK, then 300, from the command port. No interrupt.
3. The drive loads the sector holding the position into the slot's buffer, charging the latency if the transfer isn't continuous, plus the sector's read time. The data port stays empty meanwhile.
4. The drive streams words 0–255 through the data port, each ready before the OS asks. Then it loads the next sector, and the port goes quiet again, then streams words 256–299.
5. After the OS takes the last word, the drive puts OK, then 300 words valid, on the command port, and **raises its interrupt**.

The handle's position is now 300. A count of 0, or a position at the end, gives n = 0, with no data phase and an immediate completion.

#### WRITE

An example: handle 2 is a 500-word file positioned at its end, the OS writes 100 words, and clusters are one sector.

1. The OS writes `WRITE` with handle 2, then the count, 100, to the command port.
2. The drive checks the handle, the file's read-only bit, the handle's type, and write protection. It then works out n without the disk: the file's last cluster has 12 words left, the rest needs one more cluster, and the free count (known since the file was opened; see Free count) says there is one. n is the words that fit, here all 100. The OS reads OK, then 100, from the command port.
3. Right after accept, the drive allocates the cluster the write needs: it pops the free list's head and links it onto the file's chain, reading the FAT sectors that takes. Position 500 is partway into a sector, so it then loads that sector. The OS's first `OUTS` to the data port succeeds, but the drive doesn't take that word until the sector is loaded, so the OS's next write waits. The drive stalls the OS just by not taking the word in the port.
4. The drive takes words 0–11, filling that sector, and writes it back. It takes words 12–99 into a zeroed buffer for the new cluster, and writes that sector at the end of the command (or holds it, if a later write continues it).
5. The slot records the new size, 600. The drive puts OK, then 100 words written, on the command port, and **raises its interrupt**.

Because n comes from the free count, which only this drive changes, n is a promise, which stays true even if a later drive streams several files at once. If only some of the count fits, accept still says OK with a smaller n, the OS sends only n words, and completion reports **disk full**. If none fits, accept fails with **disk full**. A failure while allocating, such as a broken free list, is an early completion (see below). Clusters allocated for words that never arrive, after an abort or failure, are freed at completion, so a chain is always exactly as long as its file's size requires.

Writing at a position before the end overwrites, needing no allocation unless the write runs past the end. Files only grow by writing at or across their end.

#### Early completion

If something fails mid-transfer (a broken chain, a host I/O error, or the image disappearing), the drive posts the completion at once, before the data phase is over: the error status alone, since an error carries no further words, and **raises its interrupt**. A `WRITE` first flushes what it has taken. The handle's position, which `INFO` reports, shows how much really moved. Until the OS reads the completion, the control port shows phase 3; once it has, it shows phase 4 (or 5 for `WRITE`) again until the data phase ends.

The data phase still honors n, so the OS's loop never hangs: the rest of a read comes through as zeros, and the rest of a write is taken and discarded. These words move at port speed, with no disk time. The OS may run its loop to the end and then read the completion, or read the completion first and then cut the loop short by issuing its next command, which ends the data phase as an abort does (see Aborts), or reset at any point.

The OS can also notice the failure early, without extra cost on the fast path. Its loop on the data port only stalls at sector boundaries, and only then does it check the control port:

1. Set R7 to n, and point R4 or R5 at the buffer, which puts it in the EXTRA bank, so the buffer can be the calling program's memory.
2. `INR` from the data port (or `OUTR` to it, for `WRITE`).
3. If **S** is set, all n handoffs are done, and the completion is on the command port. If the transfer failed, it may have been waiting there for a while.
4. Otherwise, `IN` the control port. Phase 4 (or 5 for `WRITE`) means the drive is moving a sector, so go back to step 2. Phase 3 means a completion is already waiting, so the transfer failed: read it, then either finish the loop or issue the next command.

#### Packed transfers

`READ PACKED` and `WRITE PACKED` work exactly as `READ` and `WRITE`, but in dwords. Each handoff on the data port carries two words, the low word in word 0, for the dword forms of `INR` and `OUTR`, and the count, n, and the completion's count are all in dwords, so they match the OS's loop count directly. Positions and sizes stay in words.

- **Reads:** n is the lesser of the count and the dwords needed for the rest of the file, rounded up. If an odd number of words remain, the last dword's high word is 0, and the position stops at the file's size rather than one past it.
- **Writes:** always write whole dwords, so a file of odd length ends with a plain `WRITE` of its last word.

### Status codes

Statuses are grouped: 01h–0Fh are problems with the request itself, 10h–1Fh with files and slots, and 20h–2Fh with the disk.

| Code | Status | Meaning |
| --- | --- | --- |
| 00h | OK |  |
| 01h | Bad command | Unknown command code |
| 02h | Bad handle | Not a slot, or a slot with nothing open |
| 03h | Bad name | A name field that breaks the name rules |
| 04h | Bad pattern | A pattern that breaks the pattern rules |
| 05h | Bad label | A label that breaks the label rules |
| 06h | Bad attributes | Reserved attribute bits set |
| 07h | Bad argument | Any other argument out of range, such as sectors per cluster |
| 08h | Past end | A `SEEK` beyond the file's size |
| 09h | Wrong handle type | A directory handle where a file is needed, or the reverse |
| 10h | Not found | No such file, or no further match for `FIND` |
| 11h | Exists | The name is already taken |
| 12h | In use | The file is open, or for `FORMAT`, any file is |
| 13h | Too many open | Every slot is in use |
| 14h | Read-only | The file's read-only attribute is set |
| 15h | Disk changed | The slot was opened on a disk that has since been removed |
| 20h | No disk | No image is attached |
| 21h | Write protected | The image is write-protected |
| 22h | Disk full | No free clusters for the data or directory growth |
| 23h | Unformatted | No valid file system on the disk |
| 24h | Corrupt | The file system is inconsistent, such as a broken chain |
| 25h | I/O error | The host failed to read or write the image |

## Tools

### DiskFileProtocol

`DiskFileProtocol` is a `gb::FileProtocol` over a mounted image, so host code reaches files inside a disk exactly as it reaches local or in-memory files. It's for the host app and tools, not the drive device. Its default protocol name is `ozdisk`, so its paths look like `ozdisk:/HELLO.OZ3`. A host mounting several images at once registers each under a name of its own.

- **Paths** are dotted names in the root, matched without regard to case. There are no folders until subdirectories exist.
- **Flags:** info, list, file create, file read, and file write.
- **The byte view is the binary convention:** each word is two bytes, low byte first, and a file's size is its words × 2. Reading at an odd byte offset reads the containing word, and writing an odd number of bytes pads the last word with a zero high byte. A `.oz3` stored on a disk is the same words as on the host, so the program loader, the assembler, and anything else on `gb::FileSystem` can read from and write into images unchanged.
- **Attributes:** it supports `gb::FileAttribute::kReadOnly` and `kHidden`, the disk's read-only and hidden bits. `PathInfo` reports them from the file's entry, and `gb::FileSystem::SetAttributes` sets them as `ATTRIB` does, failing with the file open. A read-only file can't be opened for writing or deleted, as on the drive, and a hidden one is still listed. Copies and moves keep them, as for any protocol, so a read-only host file copied in stays read-only, and back out again.
- **Moves** within the protocol are renames, through `gb::FileSystem::MoveFile`.
- **Timestamps:** the protocol stamps written files using a clock given when it's created, or zeros without one.
- **Text conversion** stays outside, in the library's host transfer helpers, since a `RawFile` has no notion of text.

#### Ownership

The protocol owns its `DiskFileSystem`. It mounts one, unmounts it, and can swap one image for another. With nothing mounted, every path is invalid.

Everything beyond the protocol (the label, write protection, and checks) goes through `GetFileSystem()`, which returns the mounted `DiskFileSystem`, or null. No separate interface is needed, and since the protocol and direct calls share one file system, they share one open-file table and one set of buffers: deleting a file the protocol has open still fails with **in use**, and every view stays consistent.

The host keeps a pointer to the protocol before passing it to `gb::FileSystem::Register`, which owns it from then on. Protocols live as long as their file system, so the pointer stays valid. Direct calls bypass the protocol's `Lock` and `Unlock`, so a multithreaded host needs its own locking around them; the OZ-3 tools are single-threaded.

An image has one owner at a time: attaching it to a drive fails while a protocol has it mounted, and the reverse.

### oz3dsk

`oz3dsk` manages disk images from the host, following `oz3asm` and `oz3ism`. Images use the `.ozd` extension. The image comes first, as a required positional argument, then the command, so a line reads "on this disk, do this":

```
oz3dsk work.ozd new 720K
oz3dsk work.ozd format --label WORK
oz3dsk work.ozd put hello.oz3
oz3dsk work.ozd dir *.OZ3
```

| Command | Arguments and flags | Does |
| --- | --- | --- |
| `new` | size | Creates a blank image of that size |
| `format` | `--cluster n`, `--label L` | Formats the image as the drive's `FORMAT` does; with no `--cluster`, the drive's choice |
| `info` |  | Shows the medium (size, write protection) and the file system (label, clusters, free) |
| `dir` | pattern, `--all` | Lists name, size in words, attributes, and modified time; `--all` includes hidden files |
| `put` | host file, disk name, `--text` | Copies a host file in, replacing any file of that name |
| `get` | disk name, host file, `--text` | Copies a file out |
| `del` | pattern | Deletes matching files |
| `ren` | old name, new name | Renames a file |
| `attrib` | name, `--set letters`, `--clear letters` | Sets or clears `r` (read-only) and `h` (hidden); with neither flag, shows them |
| `label` | label | Sets the volume label |
| `protect` | `on` or `off` | Sets the image's write protection |
| `check` | `--fix` | Checks consistency, optionally repairing |

- **Sizes** are in words, with `K` and `M` suffixes: `720K` is 720 Kiw. A size must be a whole number of sectors.
- **`new` always makes a blank image;** `format` is a separate step.
- **Disk names** are dotted text, converted to the 12.4 fields. `put` with no disk name uses the host file's name, failing if it doesn't fit.
- **Binary (the default):** two host bytes per word, low byte first, as in `.oz3` files. An odd last byte is padded with a zero, and `get` writes two bytes per word.
- **Text (`--text`):** the same byte order as binary, since strings are packed low byte first, so text never needs reordering. Going in, host line endings become LF (0Ah). Coming out, `get --text` drops zero bytes, such as the padding in an odd-length file's last word. Files mixing binary fields and text copy correctly as plain binary.
- **Timestamps:** `put` stamps the file with the host file's modified time, and `get` sets the host file's time from the entry.
- **Attributes:** `put` and `get` keep read-only and hidden, as any copy does, with or without `--text`, so neither replaces a read-only file. They keep only the attributes both protocols support, as `gb::FileSystem::GetAttributeFlags` reports them, so a host without a hidden attribute keeps only read-only.
- **`protect off`** fails on a read-only image file, which is always protected (see Image file).

`put` and `get` copy between `local:` and a `DiskFileProtocol` mounted on the image, converting text on the way when asked. `ren` uses `gb::FileSystem::MoveFile` and `attrib` uses `gb::FileSystem::SetAttributes` through the protocol, and `dir` reads attributes from its `PathInfo`. The rest use the library directly through the protocol's `GetFileSystem()`.

### Game Bits dependencies

This work depends on four Game Bits changes, made ahead of it:

- **`gb_cli`: required positional arguments with subcommands.** A command may have required positional arguments and subcommands together, with the arguments always before the subcommand's name. Only required arguments are allowed there, so the parser always takes exactly that many words before looking for a subcommand, and an argument can never be mistaken for one.
- **`gb_cli`: required flags.** A flag can be marked required; the default stays optional.
- **`gb::FileSystem`: `MoveFile` and `MoveFolder`.** They take the same parameters as `CopyFile` and `CopyFolder`, except that moving between different protocols isn't allowed.
- **`gb::FileSystem`: file attributes.** Read-only and hidden, reported in `PathInfo` and changed with `SetAttributes`, which copies and moves keep. The disk library uses them for read-only image files, and `DiskFileProtocol` for the disk's own attributes.

## Ozzy

An Ozzy has up to four drives, `A:` to `D:`, each a drive device with its three ports and interrupt, wired the same in every Ozzy, as its other devices are:

| Drive | Ports | Interrupt |
| --- | --- | --- |
| `A:` | 16–18 | 2, on every core |
| `B:` | 19–21 | 3, on every core |
| `C:` | 22–24 | 4, on every core |
| `D:` | 25–27 | 5, on every core |

- **Ports:** the existing devices keep their ports (the keyboard 1 and 6–15, the teletype 2, and the timer 3 and 4), and the ports kept for devices grow from 0–15 to 0–31. So `ports`' minimum rises from 16 to 32, and its default from 32 to 48, leaving programs the same 16 free ports by default. Ports 0, 5, and 28–31 are left for devices to come.
- **Interrupts:** a completion is news for whichever core issued the command, normally the OS's, so the interrupt is on every core and not shared, and every core with a handler is told, as with the timer.
- **Config:** each drive is `{ type: "drive", letter: "A", ... }`, and only `type` and `letter` are required. The rest are the drive's settings, each optional:
  - `preset`: one of the timing presets (see Timing presets), `floppy-1440k` if left out. It gives the read rate, write rate, and latency.
  - `read_rate`, `write_rate`, `cache_rate`, and `latency_ms`: each overrides what the preset gives, so `{ type: "drive", letter: "B", preset: "hd-20m", latency_ms: 30 }` is a 20 MB hard disk with a faster seek.
  - `slots` and `read_cache`, with the drive's defaults.
  - `image`: an image attached at power on, or none for an empty drive. The disk's size is the image's, so a preset's image size only matters to `oz3dsk new`.

  A relative image path is from the config file's folder, so a config and its disks move together. An image that doesn't exist is a config error, as a typo would be, and so are two drives of one letter or one image. An Ozzy has at most one of each other device, but one drive per letter. There is no drive by default.
- **Machine table:** each drive is a device entry of type 5, after the timer, in letter order: its first port, interrupt, and cores, no pages, and its letter (41h to 44h) and slots as its two words. What disk is in it, if any, is the drive's to say (`MEDIA` and `VOLUME`), since it changes while the Ozzy runs.
- **Host:** `Ozzy::GetDrive(letter)` returns the drive, or null, for the host to attach and detach images and for tests. The Ozzy gives each drive the host's clock, as it gives the timer, and flushes every drive when it shuts down cleanly.
- **The `ozzy` app** attaches the config's images at power on. Changing disks while it runs needs the console frontend's help (see Open questions).
- **Wiki:** `4-Ozzy-Computer.md`'s config, devices table, ports, machine table, and errors.

## Prerequisites

This design depends on changes made ahead of it, each a backlog item of its own:

- **Packed strings low byte first.** The assembler packs strings with the first character of each pair in the low byte, and the teletype and any other code handling packed text follow suit, as does the boot ROM's text and every test that packs text. Host files then hold packed text in reading order, so text and mixed binary-and-text files copy to and from images as plain binary, on any host. Single characters still write as themselves (0041h is "A"). It breaks every program with packed text, which is why it comes before there are many.
- **Port status and byte-swapped compare instructions** in the default instruction set, at its end as new instructions are:
  - `TSTP`: sets **S** to a port's flag without loading or clearing anything, for the control port's request handshake.
  - `CMPT`: a compare that swaps both words' bytes first, so packed text compares in reading order with one instruction a word, for sorting names and other packed text.
- **No date without a clock.** A timer with no host clock reads zeros for the date and time, rather than 1970-01-01 00:00:00, so timers and drives agree that zeros mean no date.
- **Game Bits changes:** required positional arguments with subcommands and required flags in `gb_cli`, and `MoveFile`, `MoveFolder`, and file attributes in `gb::FileSystem` (see Game Bits dependencies).

## Getting there

The backlog items that build this, in order, each updating the wiki as it goes. The first three are independent of each other.

1. *Packed strings low byte first* (tools, devices, ozzy, wiki).
2. *Port status and byte-swapped compare instructions* (instruction_sets, wiki).
3. *No date without a clock* (devices, wiki).
4. *Disk library* (disk): the image, the hardware's timing, the file system, names, and checks, with no device. Depends on 1, so names pack as the assembler does.
5. *Disk drive device* (devices, wiki): the ports, the command protocol, and every command, on the disk library. Depends on 2 and 4.
6. *Disk tools* (tools, apps, wiki): `DiskFileProtocol` and `oz3dsk`. Depends on 4.
7. *Ozzy drives* (ozzy, apps, wiki): the wiring, config, and machine table above. Depends on 3 and 5, and on 6 to make disks to try by hand.

Then the *Ozzy OS*, which boots from `A:`.

## OS examples

These sketches show one way an Ozzy OS could use the drive. They aren't part of the device's design, and the OS is free to differ.

### Owning the drive

One OS core issues every drive command, so only one is ever in flight. Requests from programs, which arrive as OS calls through interrupts, wait in a queue while the drive is busy. The OS maps its own program-facing file handles onto drive slots, so it decides who may use which file, and can give programs whatever handle values it likes.

### A program reads a file

1. The program fills a call block with its file handle, a count, and a buffer address, and raises the OS's call interrupt. It then waits, or carries on, depending on the call.
2. The OS core looks up the drive slot, and sets its EXTRA bank to the program's bank.
3. It sends `READ` with the slot and count using `OUTR`, and waits for the control port's phase to reach 2, then reads the accept status and n with `INR`.
4. It points R4 at the program's buffer, sets R7 to n, and loops on `INR` from the data port, reading the control port only when `INR` stops early.
5. It reads the completion status and words valid, writes them into the call block, and wakes the program, or signals it.

For latent commands with no data phase, like `OPEN` or `DELETE`, the OS can `WAIT` after the accept status, and let the drive's interrupt handler wake it when the completion is ready.

### Listing a directory

`OPEN ROOT`, then `FIND` with the pattern repeatedly until **not found**, then `CLOSE`. Each `FIND` gives a whole entry, so a listing shows names, sizes, attributes, and dates without parsing the directory. An OS that prefers can instead `READ` the directory raw and match entries itself.

### Loading a program

The loader opens the `.oz3` file, reads its header with `READ`, then reads each section straight into the target bank with `READ PACKED` and the dword form of `INR`, which halves the instructions per word.

### Disk changes and recovery

- A **disk changed** status means the slot's disk is gone: the OS closes the slot, which frees it, and reports the failure to the program that owned the file.
- If the OS ever loses track of the protocol, such as after a fault in its drive code, it resets through the control port and reissues the command. The control port's phase shows whether that's needed.

## Open questions

- [ ] Whether a data-ready interrupt is worth adding later, so the OS can `WAIT` through sector loads instead of polling. It would need its own enable bit in the control port, to keep the interrupt's single meaning by default.
- [ ] Subdirectory commands, such as opening a directory entry as a directory handle, when the directory bit becomes real. Commands that take a name (`OPEN`, `CREATE`, `DELETE`, `RENAME`, and `ATTRIB`) could resolve it against a current directory, with a directory handle or a path as an option, so version 1 commands keep working unchanged.
- [ ] What the boot area holds, with the Ozzy OS and booting from a drive.
- [ ] How the `ozzy` app changes disks while it runs: the console frontend has only its quit keys today, so it needs a way to pick a drive and an image (or none), such as a key chord and a prompt line. Settled in *Ozzy drives*, or a later item of its own.
- [ ] Assembler addition: allow `SIZE(n)` anywhere in a `.DW` list, applying until the next `SIZE`, so a name field fits in one directive.
- [ ] More file system buffers, if FAT thrashing shows up in practice: configurable counts of FAT and directory buffers, reused least recently used first. Several files growing at once in different FAT sectors currently force the single working FAT buffer to be written at each switch. Since the file system already orders its own writes, more buffers change nothing about correctness, only RAM.
- [ ] A write-back mode for `DiskHardware`, as a performance feature. Write order only matters if the image is left partly written, so a write-back cache can merge repeated writes freely, as long as it always writes out everything it holds when the image is detached and when the app shuts down. Then the image always ends as exactly what the file system wrote, and only a host app crash, or another host process touching the image (already ruled out by contract), could leave it inconsistent. Merging repeated writes would also absorb FAT thrashing. Its time would be charged as the cache actually writes, on a timer or when it needs room.
