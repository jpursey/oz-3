# OZ-3

The OZ-3 is a 16-bit virtual CPU, made to be embedded in games as a programmable execution engine that "feels" like an old school CPU, where the CPU architecture is itself part of the gameplay. It is not a scripting language or an engine for one. The runtime simulates cores, memory banks, ports, and interrupts, with cycle counts derived from each instruction's microcode. Instruction sets are not fixed: they are written in microcode assembly, and each core can run its own.

This project depends only on the Game Bits shared C++ library (location defined by the GB_DIR environment variable), which must be present on the machine. It is developed on Windows, built with Visual Studio 2022 Community and CMake, and also builds and passes its tests on Linux with clang (see Linux below), where the `ozzy` app runs in a terminal. Nothing else builds against OZ-3, so it is developed freely in parallel git worktrees (see Local sessions), or in cloud sessions (see Cloud sessions).

See README.md for an overview, and the manual (see Manual) for the specification.

## Workflow

@E:/Projects/game-bits/docs/workflow.md

The workflow above is shared with Game Bits and every project built on it, and lives in Game Bits as `docs/workflow.md`. Imports can't read environment variables, so it is imported by absolute path. Game Bits is always checked out beside this project, so the path is the directory holding both repositories (`E:/Projects` today), followed by `game-bits/docs/workflow.md`, which is also `$GB_DIR/docs/workflow.md`. The extra directories in `.claude/settings.json` are absolute for the same reason, so they also work from a worktree. If the drive or machine changes, update the paths to match. A cloud session has no `E:` drive, so the import doesn't load there: it reads the workflow from its Game Bits clone, `../game-bits/docs/workflow.md`, before anything else (see Cloud sessions). Everything below is specific to OZ-3.

## Directory Structure

This is a CMake project, starting at the root. The directory structure is as follows:
```
  oz3/          -- All source code in this project, separated into libraries
                   (see below)
  docs/         -- Design docs, feature plans (worklog/), and the backlog
  docs/manual/  -- The manual, which holds the specification (see Manual)
  configs/      -- Ozzy configs (.ozzy) for common machines, to run with ozzy.
                   oz3_ozzy_test embeds and reads each one, so a new config
                   gets a test there too.
  bin/          -- Compiled binary files used by compilation or execution
  out/          -- Generated output from building locally. This is transient
                   and can get deleted at any time.
```

Libraries depend strictly in the order `core` → `disk` → `tools` → `instruction_sets` → `devices` → `ozzy`, and `apps` comes last (each may use the ones before it, and `core` uses nothing in OZ-3):
- `core` (`oz3_core`): The runtime. `Processor` simulates a whole OZ-3 machine: its `CpuCore`s, `MemoryBank`s, and `Port`s, configured by `ProcessorConfig`. Shared resources are `Lockable`, and every access to one goes through a simulated lock. An `InstructionSet` is compiled from an `InstructionSetDef` (instructions and macros written in microcode) by the instruction compiler. Core has no instruction set of its own: every `CpuCoreConfig` is given one. `oz3_core_testing` holds `BaseCoreTest`, the test fixture for running cores, which other libraries' tests share. `DateTime` is the machine's four-word date and time, which the timer answers with and the disk stamps files with.
- `disk` (`oz3_disk`): The one implementation of the disk format in `docs/disk_design.md` (see `docs/worklog/disk-library.md`). `DiskImage` is an image file of sectors behind a header, and results are `DiskStatus`, the drive's status codes. It uses only `core` in OZ-3, for `DateTime`, and is its own library so that `tools` can use it as well as the drive device.
- `tools` (`oz3_tools`): Libraries and tools for working with the OZ-3 that a host application can use on its own: `InstructionAssembler` (instruction set source files to an `InstructionSetDef`), `InstructionDefExporter` (an `InstructionSetDef` to C++), and `ProgramLoader` (loading a `Program` into a `Processor`'s memory). The program assembler (`AssembleProgramFile`) turns program source into a `Program` and its `ProgramMap`, which program files, map files, and listings are written from.
- `instruction_sets` (`oz3_instruction_sets`): Ready-made instruction sets, currently the default one (see Default instruction set). It links only `core`.
- `devices` (`oz3_devices`): Ready-made devices for a `ProcessorConfig`, currently the teletype, the keyboard, and the screen (the console devices Ozzy is built from; see `docs/worklog/console-devices.md`), the timer, the disk drive (`DiskDrive`, on the disk library; see `docs/worklog/disk-drive-device.md`), and the power control (`PowerControl`), which records a program's request to shut down or restart as a `PowerRequest` in the processor's context, for the host to read. Each is a `Device` and its `DeviceFactory`, configured by a name and one `<Device>Settings` struct holding its wiring and behavior, with rates in real time that it turns into cycles with the processor's clock rate (a factory whose settings can be invalid is made by a static `Create`). A host finds a device with `Processor::GetDevice` and casts it to the type it named. It links only `core` and `disk`; its tests also link `tools` and `instruction_sets`, to run programs against the devices with `DeviceProgramTest`, and `oz3_disk_testing`, to fail disk images.
- `ozzy` (`oz3_ozzy`): The Ozzy computer, a whole virtual machine built from the other libraries (see `docs/worklog/ozzy-machine.md`). `Ozzy` is one, created from `OzzySettings`, which `ReadOzzySettings` reads from a `.ozzy` config file (Game Bits' text config), and its machine table (`machine_table.h`) describes its hardware to the programs it runs. It also holds the console frontend the `ozzy` app is built from (`ConsoleDisplay`, `DrawConsoleText`, and the `Console` interface, implemented per platform; see Linux), so it is tested.
- `apps`: Every executable, and no libraries. `oz3ism` is the command line instruction set assembler, `oz3asm` the command line program assembler, `oz3dsk` the command line disk image tool, and `ozzy` (`ozzy_main.cc`) runs an Ozzy in the console. Each links only what it needs: `oz3asm` links `instruction_sets` for its default instruction set, `oz3ism` doesn't (see Default instruction set), and `oz3dsk` links only `disk` and `tools`.

### Default instruction set

The default instruction set's source is `oz3/instruction_sets/default_instruction_set.izm`, written in the microcode assembly the manual describes. `oz3ism` assembles it into C++, `default_instruction_set.inc`, which is checked in and compiled into `oz3_instruction_sets`. After changing the `.izm`, build, then regenerate the `.inc` from the repository root and check it in with the change:

```
bin/oz3ism.exe oz3/instruction_sets/default_instruction_set.izm oz3/instruction_sets/default_instruction_set.inc
```

On Linux the executable is `bin/oz3ism`, with no `.exe`, and needs nothing on PATH. It writes the same `.inc` byte for byte.

The build doesn't do this step, as running a Debug `oz3ism` needs the debug CRT. `oz3ism` doesn't link `oz3_instruction_sets`, so it still builds when the `.inc` doesn't compile. Debug and Release both write `bin/oz3ism.exe`, so it is whichever was built last; a Debug one needs the debug CRT on PATH (see Test below).

Programs depend on the default instruction set's code words, so they never change. Opcodes are numbered in the order instructions appear in the `.izm`, and each variant's code word comes from its instruction's argument and macro layout. So existing instructions are never reordered, removed, or given new variants, and their arguments and macros keep their layout. A new instruction goes at the end of the `.izm`, after the last opcode. `default_instruction_set_test.cc` fails if a pinned instruction's code words change, and new instructions are pinned too by raising `kPinnedInstructionCount` and updating `kPinnedFingerprint`. Microcode changes that keep every code word (fixes, cycle changes) are fine.

## Commands

Everything is driven directly by CMake using the Ninja generator, which is exactly what Visual Studio's "open a local folder" CMake integration does (see CMakeSettings.json). Command line builds, IDE builds, and CI all use the same build.

### Developer environment (once per shell)

CMake and Ninja need an x64 MSVC developer environment; nothing below works without it.

```
# PowerShell
Import-Module "C:\Program Files\Microsoft Visual Studio\2022\Community\Common7\Tools\Microsoft.VisualStudio.DevShell.dll"
Enter-VsDevShell -VsInstallPath "C:\Program Files\Microsoft Visual Studio\2022\Community" -DevCmdArguments "-arch=x64 -host_arch=x64" -SkipAutomaticLocation
```

To also run Debug test binaries, add the debug CRT to PATH in the same shell (see Test below).

### Configure

These match the `x64-Debug` and `x64-Release` configurations in CMakeSettings.json, so the IDE picks up whatever the command line configures and vice versa. Note that Visual Studio's "x64-Release" is `RelWithDebInfo`, not `Release`.

```
cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -S . -B out/build/x64-Debug
cmake -G Ninja -DCMAKE_BUILD_TYPE=RelWithDebInfo -S . -B out/build/x64-Release
```

Configuring takes a few seconds. CMake re-runs it automatically when a `CMakeLists.txt` changes.

If configure or build fails with a missing `cl.exe`, `rc.exe`, or Windows SDK path, the tree's cache is left over from an older Visual Studio or Windows SDK version. Delete the build tree and configure again (in the IDE this is "Delete Cache and Reconfigure").

### Build

```
cmake --build out/build/x64-Debug
cmake --build out/build/x64-Debug --target oz3_core_test
```

Debug is the default build. The build includes the Game Bits libraries OZ-3 uses, and only those (Game Bits is added with `EXCLUDE_FROM_ALL`), so a full build takes under a minute; incremental builds take a few seconds.

OZ-3 code (everything under `oz3/`) compiles with warnings as errors (`/WX` on MSVC, `-Werror` on Clang).

### Test

Tests are GoogleTest binaries registered with ctest, one per library (`oz3_core_test`, `oz3_tools_test`, `oz3_instruction_sets_test`, `oz3_devices_test`, `oz3_ozzy_test`).

```
ctest --test-dir out/build/x64-Debug --output-on-failure
ctest --test-dir out/build/x64-Debug --output-on-failure -R oz3_core_test
```

To use GoogleTest flags, run the binary directly:

```
out/build/x64-Debug/oz3/instruction_sets/oz3_instruction_sets_test.exe --gtest_filter=InstructionTest.*
```

Debug binaries link the non-redistributable debug CRT, which is not on PATH even in a developer shell, so every test exits with `0xc0000135` (DLL not found) until it is added:

```
# PowerShell
$env:PATH = "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Redist\MSVC\14.44.35112\debug_nonredist\x64\Microsoft.VC143.DebugCRT;" + $env:PATH
```

The `14.44.35112` version directory changes with Visual Studio updates. Only Debug needs this; the release CRT is already in System32.

### Linux

OZ-3 also builds and passes its tests on Linux with clang, as Game Bits does (see Linux in Game Bits' CLAUDE.md). It needs clang, CMake, and Ninja (Ubuntu's `clang`, `cmake`, and `ninja-build` packages), and no developer environment or PATH changes:

```
cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -S . -B out/build/linux-clang-Debug
cmake -G Ninja -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -S . -B out/build/linux-clang-RelWithDebInfo
cmake --build out/build/linux-clang-Debug
ctest --test-dir out/build/linux-clang-Debug --output-on-failure
out/build/linux-clang-Debug/oz3/instruction_sets/oz3_instruction_sets_test --gtest_filter=InstructionTest.*
```

Game Bits is found through `GB_DIR` as on Windows, or, when it isn't set, beside this checkout as `../game-bits` (see the root `CMakeLists.txt`), with its submodules checked out. A full build is about 430 steps, Game Bits' libraries included, and takes about 3 minutes on 4 cores. Executables and test binaries have no `.exe`. Every file is portable apart from the console: `win_console.cc` is only built on Windows, and `posix_console.cc`, a terminal with termios, everywhere else.

Clang rejects some things MSVC accepts, such as narrowing a non-constant in a braced initializer, braces around a scalar, and a `switch` over an enum that doesn't name every value, so a change made on Windows can still fail here (and in CI).

### Format

The reference clang-format is the one that ships with Visual Studio (currently 19.1.5), which is also what Visual Studio's Format Document uses:

```
# PowerShell
$clangFormat = "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\Llvm\x64\bin\clang-format.exe"
& $clangFormat -i <files>                  # format in place
& $clangFormat --dry-run -Werror <files>   # check only
```

A `clang-format` on PATH may be a different version, so use this one. A cloud session installs the same version from PyPI (see Cloud sessions). Style comes from `oz3/.clang-format` (Google style); clang-format finds it automatically for any file under `oz3/`. Only format files you actually touch.

### Checks

Every change is checked as follows:
- It builds cleanly (warnings are errors) in both Debug and Release.
- `ctest` passes in Debug.
- A local session checks these on Windows, and a cloud session on Linux, leaving Windows to CI (see Cloud sessions).
- Touched files pass the clang-format check (see Format above).
- New and changed behavior has unit tests.
- A change to the default instruction set's `.izm` regenerates the `.inc` (see Default instruction set), and `git diff` of the `.inc` shows only what the change meant to change, with no existing opcode or code word changed.
- A change to behavior the manual specifies updates the manual to match (see Manual).

## Manual

The specification lives in the manual, `docs/manual/`, whose `README.md` lists its pages: the history, the hardware specification (`2-Specifications.md`), the microcode reference (`2.1-Microcode.md`), the file formats, the devices, and the Ozzy computer. Read it before changing behavior it covers. Where the code and the manual disagree, say so rather than silently picking one.
- A change to specified behavior (the hardware model, microcode, the default instruction set, or tools the manual documents) updates the manual's pages in the same change, so the user reviews both together.
- Pages are numbered by section (`3-Devices.md`, `3.1-Teletype.md`), so they sort in order. Each starts with a `#` title, and a new page is added to the contents in `README.md`, and to its section's own list if it has one.
- Pages link to each other by file name (`[History](1-History.md)`, `[CMPT](2.2-Default-Instruction-Set.md#cmpt)`), and to the rest of the repository by relative path.

## Build system

- Each library or executable is defined by a `CMakeLists.txt` in its own directory using the `gb_add_library` / `gb_add_executable` commands from Game Bits (see `$GB_DIR/CMake/GameBitsTargetCommands.cmake`).
- New source and test files must be added to their module's `CMakeLists.txt` (`<target>_SOURCE` and `<target>_TEST_SOURCE`) or they will not be compiled.
- Defining `<target>_TEST_SOURCE` automatically creates a `<target>_test` executable that links GoogleTest/GoogleMock and registers a ctest test of the same name.
- `<target>_DEPS` is for other CMake targets in the build, which are built first and linked (Game Bits libraries like `gb_container`, and `absl::*`); `<target>_LIBS` is only linked (the existing libraries list `absl::check` and `absl::log` there).
- Executables are written to `bin/`.

## Conventions

These add to the C++ style in the workflow.
- Formatting strictly driven by clang-format in Google style via oz3/.clang-format
- All OZ-3 code is in the "oz3" namespace.
- Every file starts with the four line MIT copyright comment used everywhere in the tree, with the year the file was created.
- Headers use include guards of the form `OZ3_<DIR>_<FILE>_H_` (not `#pragma once`), and end with `}  // namespace oz3` followed by `#endif  // OZ3_<DIR>_<FILE>_H_`.
- Include order: the file's own header first, then C/C++ standard headers in angle brackets, then third-party, Game Bits, and OZ-3 headers in quotes (`"absl/..."`, `"gtest/gtest.h"`, `"gb/..."`, `"oz3/..."`), with blank lines between groups.
- Unit tests live next to the code they test as `<file>_test.cc`, written with GoogleTest/GoogleMock inside `namespace oz3 { namespace { ... } }`. Tests of the default instruction set are split by instruction group (`instruction_test_<group>.cc`), built on `InstructionTest` in `instruction_test.h`, and check results, flags, and exact cycle counts.
- Game Bits' libraries are in `$GB_DIR/src/gb/` (such as `gb/container`, `gb/file`, and `gb/parse`), and the third-party libraries it vendors are in `$GB_DIR/third_party/`.
- Some code predates the workflow's C++ style (`size_t` in a couple of places). Bring it in line when otherwise changing it, rather than in sweeping cleanups.
- C++20, built with MSVC on Windows and clang on Linux.

## Local sessions

These add to Local sessions in the workflow. A cloud session follows Cloud sessions below instead.

### Parallel sessions

These add to Parallel sessions in the workflow. Nothing builds against OZ-3's checkout, and there is nothing that only the main checkout can test, so work isn't tied to it:
- A session may work in the main checkout, or in its own git worktree. Use a worktree whenever another session may be working at the same time.
- Each session lands its own commits. Once the user approves a change and it is committed on the worktree's branch, land it on `main` from the main checkout: `git merge --ff-only <branch>`, or a cherry-pick if `main` has moved. Then tell the session named in the prompt, if there is one, as the workflow describes.

#### Worktrees

Create the worktree from the local `main`, not `origin/main`, which is behind whenever the user hasn't pushed. Then switch the session into it (EnterWorktree with its `path`; EnterWorktree's `name` form branches from `origin/main`):

```
git worktree add -b <branch> .claude/worktrees/<branch> main
```

EnterWorktree looks for the worktree in the repository that holds the session's working directory, and a `cd` in the shell moves that directory. Game Bits is a separate repository, so run git in it with `git -C <dir>` instead of `cd`. If the shell has left the main checkout, `cd` back before calling EnterWorktree.

The worktree builds against Game Bits through `GB_DIR` like the main checkout, so it needs no other setup. It has its own `out/` and `bin/`, so its first build in each configuration is a full build.

To land the change, leave the worktree first, keeping it (ExitWorktree with `keep`), since a session in a worktree can't run git against the main checkout. Then merge from the main checkout as above.

Once landed, remove the worktree from the main checkout. First check that `git status` in the worktree is clean, and that no shell is still inside it:

```
git worktree remove .claude/worktrees/<branch>
git branch -d <branch>
```

If removal fails with "Permission denied", git has already unregistered the worktree. Delete the leftover folder, then run `git worktree prune`.

## Cloud sessions

These add to Cloud sessions in the workflow, which a cloud session reads from `../game-bits/docs/workflow.md` (see Workflow). It works in its own clone, so it needs no worktree, and its PRs reach `main` when the user merges them. It builds and tests on Linux with the commands above (see Linux), leaving Windows to CI:
- Game Bits is cloned beside this clone, as `../game-bits`, at `origin/main`, with its submodules: `git clone https://github.com/jpursey/game-bits ../game-bits`, then `git -C ../game-bits submodule update --init --depth 1`. CMake finds it there without `GB_DIR`. Game Bits' own docs changes (a backlog item or the workflow) are PRs in Game Bits, which needs it attached to the session for push.
- It runs as root, so it runs the tests as an ordinary user it adds, who owns the build tree, as Game Bits does: `useradd -m gbtest` once, then `chown -R gbtest out/build/linux-clang-Debug` and `su gbtest -c "ctest --test-dir out/build/linux-clang-Debug --output-on-failure"` from the clone. Root ignores read-only permissions, so a test of a read-only file passes as root when it shouldn't, as in Game Bits.
- It formats with clang-format 19.1.5 from PyPI, installed in a virtual environment in the session's scratchpad: `python3 -m venv <dir>`, then `<dir>/bin/pip install clang-format==19.1.5`, then `<dir>/bin/clang-format` as in Format above.
- CI (`.github/workflows/cmake.yml`) builds and tests in Debug and Release on both Windows and Linux (with clang), as four parallel jobs (`build-windows-debug`, `build-windows-release`, `build-linux-debug`, `build-linux-release`), each against Game Bits' `main`. It runs by itself for pushes to `main` that change more than docs. On a PR's branch it runs only when started by hand (a `workflow_dispatch` run of the CMake workflow on that branch), which the session does as the workflow's Cloud sessions describes. A comment on the PR says that anything checked by running `ozzy` by hand is left for the user.

## Don't
- Don't add or modify code outside `oz3/` without asking. Game Bits code is changed only in Game Bits sessions (see the workflow).
- Don't edit `default_instruction_set.inc` by hand; regenerate it from the `.izm`.
- Don't generate or build Visual Studio solutions (`-G "Visual Studio 17 2022"`); build with Ninja as described above.
