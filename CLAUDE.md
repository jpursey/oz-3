# OZ-3

The OZ-3 is a 16-bit virtual CPU, made to be embedded in games as a programmable execution engine that "feels" like an old school CPU, where the CPU architecture is itself part of the gameplay. It is not a scripting language or an engine for one. The runtime simulates cores, memory banks, ports, and interrupts, with cycle counts derived from each instruction's microcode. Instruction sets are not fixed: they are written in microcode assembly, and each core can run its own.

This project depends only on the Game Bits shared C++ library (location defined by the GB_DIR environment variable), which must be present on the machine. It currently only works on Windows, and is built with Visual Studio 2022 Community and CMake. Nothing else builds against OZ-3, so it is developed freely in parallel git worktrees (see Parallel sessions).

See README.md for an overview, and the wiki (see Wiki) for the specification.

## Workflow

@E:/Projects/game-bits/docs/workflow.md

The workflow above is shared with Game Bits and every project built on it, and lives in Game Bits as `docs/workflow.md`. Imports can't read environment variables, so it is imported by absolute path. Game Bits is always checked out beside this project, so the path is the directory holding both repositories (`E:/Projects` today), followed by `game-bits/docs/workflow.md`, which is also `$GB_DIR/docs/workflow.md`. The extra directories in `.claude/settings.json` are absolute for the same reason, so they also work from a worktree. If the drive or machine changes, update the paths to match. Everything below is specific to OZ-3.

## Directory Structure

This is a CMake project, starting at the root. The directory structure is as follows:
```
  oz3/          -- All source code in this project, separated into libraries
                   (see below)
  docs/         -- Feature plans (worklog/) and the backlog
  wiki/         -- A clone of the GitHub wiki, which holds the specification
                   (see Wiki). It is its own git repository, and is ignored by
                   this one.
  bin/          -- Compiled binary files used by compilation or execution
  out/          -- Generated output from building locally. This is transient
                   and can get deleted at any time.
```

Libraries depend strictly in the order `core` → `tools` → `instruction_sets` (each may use the ones before it, and `core` uses nothing in OZ-3):
- `core` (`oz3_core`): The runtime. `Processor` simulates a whole OZ-3 machine: its `CpuCore`s, `MemoryBank`s, and `Port`s, configured by `ProcessorConfig`. Shared resources are `Lockable`, and every access to one goes through a simulated lock. An `InstructionSet` is compiled from an `InstructionSetDef` (instructions and macros written in microcode) by the instruction compiler. Core has no instruction set of its own: every `CpuCoreConfig` is given one. `oz3_core_testing` holds `BaseCoreTest`, the test fixture for running cores, which other libraries' tests share.
- `tools` (`oz3_tools`): Libraries and tools for working with the OZ-3 that a host application can use on its own: `InstructionAssembler` (instruction set source files to an `InstructionSetDef`), `InstructionDefExporter` (an `InstructionSetDef` to C++), and `ProgramLoader` (loading a `Program` into a `Processor`'s memory). `oz3ism` is the command line instruction set assembler, built from the two.
- `instruction_sets` (`oz3_instruction_sets`): Ready-made instruction sets, currently the default one (see Default instruction set). It links only `core`.

The README also describes `devices` (virtual devices attached through ports) and `ozzy` (a virtual computer and "OS" built from the other libraries, with the debugger). Neither exists yet (see the backlog). When they are added, they come after `instruction_sets` in the dependency order.

### Default instruction set

The default instruction set's source is `oz3/instruction_sets/default_instruction_set.izm`, written in the microcode assembly the wiki describes. `oz3ism` assembles it into C++, `default_instruction_set.inc`, which is checked in and compiled into `oz3_instruction_sets`. After changing the `.izm`, build, then regenerate the `.inc` from the repository root and check it in with the change:

```
bin/oz3ism.exe oz3/instruction_sets/default_instruction_set.izm oz3/instruction_sets/default_instruction_set.inc
```

Both paths must be relative to the current directory; `oz3ism` can't read absolute paths. The build doesn't do this step, as running a Debug `oz3ism` needs the debug CRT. `oz3ism` doesn't link `oz3_instruction_sets`, so it still builds when the `.inc` doesn't compile. Debug and Release both write `bin/oz3ism.exe`, so it is whichever was built last; a Debug one needs the debug CRT on PATH (see Test below).

## Commands

Everything is driven directly by CMake using the Ninja generator, which is exactly what Visual Studio's "open a local folder" CMake integration does (see CMakeSettings.json). Command line builds and IDE builds use the same build.

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

Debug is the default build. The build includes the Game Bits libraries OZ-3 uses, so a full build is about 500 steps and takes about 30 seconds; incremental builds take a few seconds.

OZ-3 code (everything under `oz3/`) compiles with warnings as errors (`/WX` on MSVC, `-Werror` on Clang).

### Test

Tests are GoogleTest binaries registered with ctest, one per library (`oz3_core_test`, `oz3_tools_test`, `oz3_instruction_sets_test`).

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

### Format

The reference clang-format is the one that ships with Visual Studio (currently 19.1.5), which is also what Visual Studio's Format Document uses:

```
# PowerShell
$clangFormat = "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\Llvm\x64\bin\clang-format.exe"
& $clangFormat -i <files>                  # format in place
& $clangFormat --dry-run -Werror <files>   # check only
```

A `clang-format` on PATH may be a different version, so use this one. Style comes from `oz3/.clang-format` (Google style); clang-format finds it automatically for any file under `oz3/`. Only format files you actually touch.

### Checks

Every change is checked as follows:
- It builds cleanly (warnings are errors) in both Debug and Release.
- `ctest` passes in Debug.
- Touched files pass the clang-format check (see Format above).
- New and changed behavior has unit tests.
- A change to the default instruction set's `.izm` regenerates the `.inc` (see Default instruction set), and `git diff` of the `.inc` shows only what the change meant to change.
- A change to behavior the wiki specifies updates the wiki to match (see Wiki).

## Wiki

The specification lives in the GitHub wiki (https://github.com/jpursey/oz-3/wiki), cloned in the main checkout as `wiki/`: its history, the hardware specification (`2-Specifications.md`), and the microcode reference (`2.1-Microcode.md`). Read it before changing behavior the spec covers. Where the code and the wiki disagree, say so rather than silently picking one.
- A change to specified behavior (the hardware model, microcode, the default instruction set, or tools the wiki documents) updates the wiki pages in the same piece of work, and the user reviews both together.
- Wiki edits are made in the main checkout's `wiki/` (`E:/Projects/oz-3/wiki`), even from a worktree, since `wiki/` is not part of this repository. Once the user approves, commit them there, adding only the pages you changed, as other sessions may have edits of their own in progress.
- Don't push the wiki, just as with this repository. The user pushes.
- Wiki pages link to each other by page name in lower case without the extension (`[History](1-history)`), which is how GitHub wikis resolve links.

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
- Some code predates the workflow's C++ style (iostreams in `oz3ism.cc`, `size_t` in a couple of places). Bring it in line when otherwise changing it, rather than in sweeping cleanups.
- C++20, built with both MSVC and clang-cl.

## Parallel sessions

These add to Parallel sessions in the workflow. Nothing builds against OZ-3's checkout, and there is nothing that only the main checkout can test, so work isn't tied to it:
- A session may work in the main checkout, or in its own git worktree. Use a worktree whenever another session may be working at the same time.
- Each session lands its own commits. Once the user approves a change and it is committed on the worktree's branch, land it on `main` from the main checkout: `git merge --ff-only <branch>`, or a cherry-pick if `main` has moved. Then tell the session named in the prompt, if there is one, as the workflow describes.
- Wiki edits always happen in the main checkout's `wiki/` (see Wiki).

### Worktrees

Create the worktree from the local `main`, not `origin/main`, which is behind whenever the user hasn't pushed. Then switch the session into it (EnterWorktree with its `path`; EnterWorktree's `name` form branches from `origin/main`):

```
git worktree add -b <branch> .claude/worktrees/<branch> main
```

The worktree builds against Game Bits through `GB_DIR` like the main checkout, so it needs no other setup. It has its own `out/` and `bin/`, so its first build in each configuration is a full build.

To land the change, leave the worktree first, keeping it (ExitWorktree with `keep`), since a session in a worktree can't run git against the main checkout. Then merge from the main checkout as above.

Once landed, remove the worktree from the main checkout. First check that `git status` in the worktree is clean, and that no shell is still inside it:

```
git worktree remove .claude/worktrees/<branch>
git branch -d <branch>
```

If removal fails with "Permission denied", git has already unregistered the worktree. Delete the leftover folder, then run `git worktree prune`.

## Don't
- Don't add or modify code outside `oz3/` without asking. Game Bits code is changed only in Game Bits sessions (see the workflow).
- Don't edit `default_instruction_set.inc` by hand; regenerate it from the `.izm`.
- Don't generate or build Visual Studio solutions (`-G "Visual Studio 17 2022"`); build with Ninja as described above.
