# OZ-3 Virtual CPU

The OZ-3 is an 16-bit virtual CPU made up just for fun. It's intended purpose is as a programmable execution engine within a game, that "feels" like a old school CPU, and where CPU architecture is actually part of the gameplay. More specifically, it is not a scripting language, or intended to be the execution engine for a scripting language (it almost certainly is way too limited).

## What's here

This repository has everything to write, build, and run OZ-3 programs. The source folder is "oz3" and it contains the following:

   - **core:** This folder contains the core runtime library for the OZ-3. It contains the execution engine, memory, interrupts, and port interfaces for integrating the OZ-3 into another program. 
   - **program:** This folder contains the library for assembled programs: their program and map files, their listings, and loading them into an OZ-3's memory.
   - **syntax:** This folder contains the library for how instructions are written in source, which the program and instruction set assemblers share: every form of an instruction set's instructions, their source patterns, and the lexer for program source.
   - **asm:** This folder contains the program assembler library, which turns program source into programs and their maps, and which a host application can use on its own.
   - **microasm:** This folder contains the instruction set assembler library, which turns instruction set source into an instruction set, or into C++ for one, and which a host application can use on its own.
   - **tools:** This folder contains general purpose tools and libraries for use with the OZ-3, currently the library behind the disk image tool. These can be used independently or as libraries which can be integrated into another application or game.
   - **instructions:** This folder contains ready-made instruction sets for OZ-3 cores, including the default OZ-3 instruction set. Each is written in microcode assembly and assembled into C++ by the instruction set assembler.
   - **devices:** This folder contains independent virtual device libraries that can be attached to the OZ-3 via ports. In practice devices are very specific to application. These devices in particular are used by the Ozzy virtual computer.
   - **ozzy:** Ozzy is a virtual computer built out of the OZ-3 core and provided devices. A config file says what hardware it has, and its boot ROM starts it up.
   - **apps:** The command line programs: `oz3ism` (the instruction set assembler), `oz3asm` (the program assembler), `oz3dsk` (the disk image tool), and `ozzyterm`, which runs an Ozzy in the console.

## Building OZ-3 code

The OZ-3 project is built using [GameBits](https://github.com/jpursey/game-bits), with CMake and Ninja. It builds on Windows with Visual Studio 2022, and on Linux with clang, where everything builds and passes its tests, and `ozzyterm` runs in a terminal. In order to build any OZ-3 code, you must have GameBits downloaded, either beside this repository as `game-bits`, or anywhere with the `GB_DIR` environment variable set to its root directory. CLAUDE.md has the build and test commands for each platform.

### In a devcontainer

[.devcontainer](.devcontainer) defines an Ubuntu 24.04 container with everything a Linux build needs, as in Game Bits' own devcontainer, including the Vulkan SDK for graphical programs. It can be opened as a GitHub Codespace (Code > Codespaces), or with Visual Studio Code's Dev Containers extension on an x86_64 machine (the Vulkan SDK isn't built for ARM Linux). Game Bits is cloned beside the checkout as `../game-bits` when it is created, so the Linux commands in CLAUDE.md work as they are, and `CC` and `CXX` are set to clang. The reference clang-format is on the PATH.

It also runs a light desktop, viewed in the browser. Open the forwarded "Desktop" port (6080) from the Ports panel, choose Connect, and enter the password `vscode`. The desktop is 1920x1080, so in a smaller browser window, set noVNC's Scaling mode (in its settings) to Local scaling. Right-click the desktop for its menu, where OZ-3 Terminal opens a terminal in the checkout, with the same build environment as VS Code's. Run `ozzyterm` there rather than in VS Code's terminal, which keeps the keys VS Code has shortcuts for: the desktop's window manager takes no keys, and its terminal only Ctrl+Shift+C and Ctrl+Shift+V to copy and paste, and Shift+Insert to paste. Keys the browser or your own computer take for themselves (such as Ctrl+W, or Alt+Tab) still don't reach it. A graphical program run from either terminal appears on the desktop in a window of its own, rendered on the CPU with Mesa's lavapipe driver.

In VS Code, the CMake Tools extension builds, runs, debugs, and tests from its CMake panel and the status bar. On its first configure, choose the [Unspecified] kit, which uses clang. It builds in the same directories as the Linux commands in CLAUDE.md. To debug `ozzyterm`, choose "ozzyterm" in the Run and Debug view, or "ozzyterm: console.ozzy" or "ozzyterm: teletype.ozzy" for one of the configs in [configs](configs): it builds `ozzyterm`, then runs it in a terminal of its own on the desktop, so it gets the desktop's keys while VS Code debugs it. Command line programs debug from CMake Tools as usual, in VS Code's terminal. The Testing view lists each test, to run or debug on its own, once the tests are built. `.oza` and `.izm` files are highlighted by the [OZ-3 Languages](editors/vscode) extension, which the container packages from the checkout and installs each time VS Code attaches. These settings are in `devcontainer.json`, so they apply only in the container.

## Documentation

All documentation for the OZ-3 is in the [OZ-3 Manual](docs/manual/README.md).
