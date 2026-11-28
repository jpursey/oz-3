# Library Tiers

`oz3/tools` has grown into everything that isn't the machine: the program
model and its files, the source syntax both assemblers share, the program
assembler, the instruction set assembler, and the disk tool. The debugger
would land there too, and pull in all of it, though it needs only program maps,
the loader, and the source syntax (for disassembly). `oz3/devices` is one
library for six devices that share nothing. And the libraries are described as
one chain (`core` → `disk` → `tools` → ...), which says less than it seems:
most of them don't use the one before.

This splits both into libraries that each do one thing, gives the executables'
command lines libraries of their own so they are tested, and replaces the chain
with tiers, as Game Bits' README describes its own: a library depends freely on
lower tiers, as little as possible within its tier, and never on a higher one.

There is no change in behavior. Every executable takes the same command line
and does the same thing. Headers move, so every include of a moved header
changes, as does its include guard.

## Design

### Tiers

| Tier | Libraries | Rules |
| ---- | --------- | ----- |
| 0, foundation | `core` | Uses nothing in OZ-3. |
| 1, common | `disk`, `program`, `instructions`, `devices/*` | The parts of a machine, and the formats it uses. Dependencies within the tier are few, and each is named in CLAUDE.md (such as `devices/disk_drive` → `disk`, and `disk`'s tests → `program`). |
| 2, tools | `syntax`, `asm`, `microasm`, and later `debugger` | Used by the tools and the machine's hosts. They use each other as they need to, without cycles. |
| 3, hosts | `cli/*`, `ozzy/*` | `cli/*` is each executable's command line. `ozzy/*` keeps its chain (`machine` → `app` → `term`). The two never use each other. |
| 4, apps | `apps` | Each executable is a `main` over its library, linking only what it uses. |

A library's tests, and its `*_testing` library, may also use tier 2 when the
library is in tier 1: devices and instruction sets are tested by assembling
programs. This is the one way anything reaches up a tier.

Nothing checks the tiers but review (CMake catches only cycles). CLAUDE.md
lists each library's tier and the libraries it uses, so a CL that adds a
dependency changes that list, where review sees it.

### Libraries

Every directory is flat under `oz3/` except `devices/`, `cli/`, and `ozzy/`,
which group libraries of one kind.

| Directory | Library | Tier | What | Uses |
| --------- | ------- | ---- | ---- | ---- |
| `core` | `oz3_core` | 0 | The runtime (unchanged) | |
| `disk` | `oz3_disk` | 1 | The disk format, as now, plus `DiskFileProtocol` and the host text transfer (`DiskTextFromHost`, `HostTextFromDisk`), from `tools` | `core` |
| `program` | `oz3_program` | 1 | A program once assembled, its files, and loading it: `Program`, `BareProgram`, `ProgramMap`, program and map files, listings, `ProgramLoader` | `core` |
| `instructions` | `oz3_instructions` | 1 | Ready-made instruction sets, the default one (renamed from `instruction_sets`) | `core` |
| `devices/<device>` | `oz3_devices_<device>` | 1 | One device each: `teletype`, `keyboard`, `screen` (with its characters), `timer`, `disk_drive` (with its commands), `power_control` | `core`; `disk_drive` also `disk` |
| `syntax` | `oz3_syntax` | 2 | How instructions are written in source: `InstructionSetSyntax`, `source_pattern`, `program_lexer` | `core` |
| `asm` | `oz3_asm` | 2 | The program assembler: `AssembleProgramFile` and the rest of `program_assembler`, `expression`, `program_grammar`, `program_macro` | `core`, `program`, `syntax` |
| `microasm` | `oz3_microasm` | 2 | The instruction set assembler: `InstructionAssembler`, `InstructionDefExporter` | `core`, `syntax` |
| `cli/disk` | `oz3_cli_disk` | 3 | `oz3dsk`'s command line, and `DiskTool` | `disk`, `program` |
| `cli/asm` | `oz3_cli_asm` | 3 | `oz3asm`'s command line | `program`, `syntax`, `asm`, `microasm`, `instructions` |
| `cli/microasm` | `oz3_cli_microasm` | 3 | `oz3ism`'s command line | `microasm` (never `instructions`, so `oz3ism` builds when the `.inc` doesn't) |
| `ozzy/*` | as now | 3 | As now | `ozzy/machine` uses `program`, `syntax`, and `asm` where it used `tools` |

The host text transfer goes into `disk` rather than `cli/disk`: it uses
nothing, and is how any host turns its text into a disk's bytes, as the manual's
Disk Drive page offers it, not only `oz3dsk`.

`oz3_devices_testing`, in `oz3/devices` itself, holds `DeviceProgramTest`, which
every device's tests share, as `oz3_disk_testing` does for the disk's. Each
device's tests are their own binary (`oz3_devices_screen_test`, ...).

Headers are included by their path (`"oz3/program/program.h"`,
`"oz3/devices/screen/screen.h"`, `"oz3/cli/asm/oz3asm.h"`), and their guards
name it (`OZ3_PROGRAM_PROGRAM_H_`, `OZ3_DEVICES_SCREEN_SCREEN_H_`). Classes,
functions, and file names don't change.

### Names

- **`instructions`**: ready-made instruction sets. `core`'s
  `instruction_def.h` and `instruction_compiler.h` are files, not a library, so
  they don't collide. Set aside: `isa` (too abstract), `isets` (an
  abbreviation of nothing), `opcodes` (it holds microcode and macros too).
- **`asm`**: the program assembler, the library behind `oz3asm`. Set aside:
  `assembler`, `program_assembler` (too long).
- **`microasm`**: the instruction set assembler, a microassembler as the tool
  that assembled microcode was called. Set aside: `microcode`, which is `core`'s
  `microcode.h`, and `instruction_assembler` (too long). Its tool stays
  `oz3ism`.
- **`syntax`**: how instructions are written in source, after
  `InstructionSetSyntax`, which is in it.
- **`program`**: a program as assembled, after `Program`, which is in it.
- **`cli`**: the executables' command lines, built on Game Bits' `gb_cli`.
  Each is named for what its tool works on (`cli/disk`, `cli/asm`,
  `cli/microasm`), and its library has `cli` in its name, so `oz3_cli_disk`
  doesn't read as `oz3_disk`. Set aside: `dsk` (too close to `disk`), and
  `disk/cli` (it would put each tool's command line in a different place).

### Command line libraries

Each executable's command line (its flags, arguments, help, validators, and
what running it does) moves out of its `.cc` into its library, so it is tested:

```
// oz3/cli/asm/oz3asm.h
// oz3asm's command line, which gb::RunCommandLine runs. Its context holds the
// gb::FileSystem its paths are in (gb::kConstraintCliFileSystem), and may hold
// the gb::CliWriter it writes to.
const gb::CliCommand& GetOz3asmCommand();
```

`GetOz3dskCommand()` in `oz3/cli/disk/oz3dsk.h` and `GetOz3ismCommand()` in
`oz3/cli/microasm/oz3ism.h` are the same. A function rather than a `const`
global, as the command points at the other globals of its file, and a global
in another file would depend on the order they are initialized.

So `apps/oz3asm_main.cc` (from `oz3asm.cc`, and so on) only builds the file
system as it does now, and returns what `gb::RunCommandLine` does with the
command, as `ozzyterm_main.cc` does with `OzzyApp`. Each library is tested
through `gb::RunCommandLine`, with a memory file system and Game Bits'
`FakeCliWriter`.

**Brittleness:** none new. The command and its globals are private to one
file, as they are now.

### To confirm

- Confirmed in CL3: `gb_add_library` doesn't take a library of headers only.
  It calls `add_library` without `INTERFACE`, and CMake can't determine the
  linker language of a library with no `.cc`. So `oz3_devices_testing` has
  `device_program_test.cc`, which `DeviceProgramTest`'s longer bodies (`Load`,
  `GetLabel`, `GetData`, `SetData`, `GetHostContext`, and `CreateSyntax`) moved
  into.
- Confirmed in CL4: `oz3_disk` had `gb_file` but not `gb_base`, which
  `DiskFileProtocol` uses for its context and the host's time zone, so it adds
  it. `DiskFileProtocol`'s tests write and read a program file through the
  protocol, so `oz3_disk`'s tests use `tools` (a tier 1 library's tests reaching
  up to tier 2) until CL5, when they use `program` instead.
- Confirmed in CL5: `program_listing_test.cc` assembles the programs it lists,
  with `InstructionAssembler`, `InstructionSetSyntax`, and the program
  assembler, so `oz3_program`'s tests use `tools`, a tier 1 library's tests
  reaching up to tier 2. As those move, its tests use `syntax`, `asm`, and
  `microasm` in their place.
- Confirmed in CL6: `instruction_set_syntax_test.cc` builds its instruction
  sets with `InstructionAssembler`, so `oz3_syntax`'s tests use `tools`, a
  library's tests reaching to another in its tier (`tools` itself uses
  `syntax`, so not a cycle of libraries). When the instruction assembler moves
  in CL8, they use `microasm` in its place.
- Confirmed in CL7: `program_assembler_test.cc` and `program_grammar_test.cc`
  build their instruction sets with `InstructionAssembler`, so `oz3_asm`'s
  tests use `tools`, as `syntax`'s do, until CL8, when they use `microasm`.
  `oz3asm` and `program`'s tests use `asm` and keep `tools` for the
  instruction assembler. `tools`' tests drop `oz3_core_testing`,
  which nothing in them had used since `ProgramLoader` moved in CL5.
- Confirmed in CL8: `oz3_syntax`'s, `oz3_asm`'s, and `oz3_program`'s tests,
  and `oz3asm`, use `microasm` where they used `tools`, and `oz3ism` uses
  `core` and `microasm`. `microasm`'s tests use nothing but `microasm` itself.
  `tools` keeps only `disk_tool`, on `disk` and `program`, and drops `core`,
  `syntax`, `gb_parse`, and the absl targets (and `absl::check` and
  `absl::log`) that only the instruction assembler used.
- Nothing outside CMake names a test binary or a library (CI runs `ctest`, and
  `.claude/settings.json` and `CMakeSettings.json` name no library).

## CLs

Each CL moves its files with `git mv`, so history follows them, and updates
every reference to what it moves: includes, include guards, the CMake files,
CLAUDE.md, the manual's paths, the backlog's **Layers**, and the design and
worklog docs. Until the last CL, `tools` stays in tier 2 for what hasn't moved.

### CL1 [x] CLAUDE.md: library tiers

Depends on: nothing.

- CLAUDE.md's Directory Structure describes the tiers above for the libraries
  as they are (`tools` in tier 2, `devices` in tier 1, `instruction_sets` in
  tier 1, `apps` in tier 4), each library with its tier and what it uses, in
  place of the one chain.
- The rules for tests reaching up a tier, and for dependencies within a tier.

**Verify**
- Docs only, so no build or CI.
- Every library's listed dependencies match its `CMakeLists.txt`.

### CL2 [x] instructions: renamed from instruction_sets

Depends on: CL1.

- `oz3/instruction_sets` → `oz3/instructions`, `oz3_instruction_sets` →
  `oz3_instructions`, and its users (`ozzy/machine`, `apps`, and the tests
  of `devices` and `ozzy/machine`).
- The `oz3ism` command in CLAUDE.md regenerates
  `oz3/instructions/default_instruction_set.inc`.

**Verify**
- Standard checks (see CLAUDE.md).
- Regenerating the `.inc` with the new path gives the same bytes.

### CL3 [x] devices: a library for each device

Depends on: CL2.

- `oz3/devices/<device>/`, each with its own `CMakeLists.txt` and tests, and
  `oz3/devices/CMakeLists.txt` adding them.
- `oz3_devices_testing` (`device_program_test.h`, and a `.cc` if needed).
- `ozzy/machine`, `ozzy/app`, and `ozzy/term` link the devices they use, not
  one `oz3_devices`.

**Verify**
- Standard checks.
- Each device's tests run in its own binary, and the total test count matches
  `oz3_devices_test`'s before the change.

### CL4 [x] disk: DiskFileProtocol and host text transfer

Depends on: CL1.

- `disk_file_protocol` and `disk_transfer`, and their tests, move from `tools`
  to `disk`.
- The manual's Disk Drive page names their new paths.

**Verify**
- Standard checks.

### CL5 [x] program: programs, their files, and loading them

Depends on: CL1.

- `program`, `bare_program`, `program_map`, `program_map_chunks`,
  `program_file`, `program_map_file`, `program_listing`, and `program_loader`,
  and their tests, move from `tools` to `program`.
- The manual's Program Files and Program Source pages name their new paths.

**Verify**
- Standard checks.

### CL6 [x] syntax: how instructions are written in source

Depends on: CL5.

- `instruction_set_syntax`, `source_pattern`, and `program_lexer`, and their
  tests, move from `tools` to `syntax`.
- The manual's Instruction Set Source and Program Source pages name their new
  paths.

**Verify**
- Standard checks.

### CL7 [x] asm: the program assembler

Depends on: CL6.

- `program_assembler`, `expression`, `program_grammar`, and `program_macro`,
  and their tests, move from `tools` to `asm`.
- CLAUDE.md's Editor grammars name `asm/program_grammar.cc` and
  `asm/expression.cc`.

**Verify**
- Standard checks.

### CL8 [x] microasm: the instruction set assembler

Depends on: CL6.

- `instruction_assembler` and `instruction_def_exporter`, and their tests, move
  from `tools` to `microasm`.

**Verify**
- Standard checks.
- Regenerating the default instruction set's `.inc` gives the same bytes.

### CL9 [ ] cli/disk: oz3dsk's command line

Depends on: CL4, CL5.

- `disk_tool` and its tests move from `tools` to `cli/disk`, which leaves
  `tools` empty, so it is removed, along with tier 2's mention of it.
- `oz3dsk`'s command line moves from `apps/oz3dsk.cc` to `cli/disk/oz3dsk.cc`,
  behind `GetOz3dskCommand()`, and `apps/oz3dsk_main.cc` runs it.
- The manual's Disk Drive page names `DiskTool`'s new path.

**Verify**
- Standard checks.
- Unit tests: each command through the command line, against an image in a
  memory file system: what it writes, and its errors and exit codes, as the
  validators (size, cluster, label, attributes) and the reports give them.
- `bin/oz3dsk --help` and a command run by hand give what they did before.

### CL10 [ ] cli/asm: oz3asm's command line

Depends on: CL7, CL8.

- `oz3asm`'s command line moves from `apps/oz3asm.cc` to `cli/asm/oz3asm.cc`,
  behind `GetOz3asmCommand()`, and `apps/oz3asm_main.cc` runs it.

**Verify**
- Standard checks.
- Unit tests: a source file assembled to a program file, a bare program, a map,
  and a listing; `--define`; `--instruction-set` with an `.izm`; output paths
  that collide; and source and instruction set errors, as written and as exit
  codes.
- `bin/oz3asm --help` gives what it did before.

### CL11 [ ] cli/microasm: oz3ism's command line

Depends on: CL8.

- `oz3ism`'s command line moves from `apps/oz3ism.cc` to
  `cli/microasm/oz3ism.cc`, behind `GetOz3ismCommand()`, and
  `apps/oz3ism_main.cc` runs it.

**Verify**
- Standard checks.
- Unit tests: an `.izm` assembled to an `.inc`, and errors in reading,
  assembling, and writing, as written and as exit codes.
- Regenerating the default instruction set's `.inc` gives the same bytes.
