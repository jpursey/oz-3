# OZ-3 Virtual Computer

The OZ-3 is an 16-bit virtual computer made up just for fun. It's intended purpose is as a programmable execution engine within a game, that "feels" like a old school CPU, and where CPU architecture is actually part of the gameplay. More specifically, it is not a scripting language, or intended to be the execution engine for a scripting language (it almost certainly is way too limited).

## What's here

This repository has everything to write, build, and run OZ-3 programs:

   - **core:** This folder contains the core runtime library for the OZ-3. It contains the execution engine, memory, interrupts, and port interfaces for integrating the OZ-3 into another program. 
   - **tools:** This folder contains general purpose tools and libraries for use with the OZ-3. Tools include an assembler and debugger library and tools. These are can be used independently or as libraries which can be integrated into another application or game.
   - **devices:** This folder contains independent virtual device libraries that can be attached to the OZ-3 via ports. In practice devices are very specific to application. These devices in particular are used by the Ozzy virtual computer.
   - **ozzy:** Ozzy is a standalone executable 

## Building OZ-3 code

The OZ-3 project is built using [GameBits](https://github.com/jpursey/game-bits). At the moment, it only has been built for Windows, but in theory both GameBits and the OZ-3 are not written to be platform specific (I just have not made the effort to port it). In order to build any OZ-3 code, you must have GameBits downloaded, and the `GB_DIR` environment variable must be set to the root directory of GameBits.

