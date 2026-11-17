# History

Here is the history of the OZ-3, in roughly chronological order.

## Osborne Executive (1983)

My first computer was the [Osborne Executive](https://en.wikipedia.org/wiki/Osborne_Executive) which we got in 1983. Well, more precisely, it was my parents' computer, as I was still a kid. It was a fascinating 8-bit machine, with a Z80 chip running at a whopping 4MHz. It came with a fully detailed instruction manual, and I eventually got into playing around in Z80 assembly. I really liked the simplicity (CPUs were so much simpler back then), and how much direct control over the machine I had.

## Robot War (late 1980s)

Much later, I made a DOS game in high school called "Robot War". The basic idea of the game was that each player would build a virtual robot by buying weapons, armor, sensors, etc., and most importantly, one or more "CPUs" which could be programmed in a simple language.

The players would then each submit their robot into a virtual arena where robots would then earn virtual cash based on their performance which they could use to buy and upgrade equipment, change programming etc.

The robots had one to four virtual CPUs and RAM (lines of code), both of which the players had to buy. The lines of code were instructions like "Radar 1" (to scan for a target for one cycle), "Move toward target 3" (to move toward any found target 3 feet), and "hit with spike" (to attack whatever is in front of the robot with a spike).

## The OZ-1 (early 1990s)
 
The original Robot War was a great success, and so late in high school, I decided to make a sequel. The primary thing that I wanted to do was to make the game more "real", and a big part of that was having a CPU that actually worked similarly to a real CPU. Something like the Z80, but with fewer idiosyncrasies, and a 16-bit processor. Thus was born the OZ-1. This pretty much consumed all my interest and time, and Robot War 2 never really got off the ground.

The OZ-1 was still a weird processor:

* *No bytes:* There was no 8-bit access or operations at all, just 16-bit words. With 64K addresses, that amounted to 128K of ROM+RAM address space. 
* *Separate stacks:* I also didn't really understand how actual software stacks worked in practice at the time. So to make programming easy, the OZ-1 had two fully separate memory spaces for an address stack (for call/return) and a data stack (for parameters). Each had a capacity of up to 128K (!)
* *Input/output ports:* There were 128 input/output ports (but again, 16-bit data words). However there was no signaling or handshaking or device specifications around them.

The OZ-1 got as far as a complete specification, runtime, and assembler written in C++. A close friend of mine even made an optimized (in assembly) runtime from the spec.

## The OZ-2 (mid 1990s)

The OZ-1 never really became part of Robot War, but there were some things I wanted to add in preparation for being part of Robot War 3 (I no longer recall what happened to Robot War 2). Specifically, I wanted the following things:

* *Interrupts:* Interrupts provide a better way for the program to respond to inputs from outside without wasting space (and time) on polling ports. The model the OZ-2 went with was something close to Mode 2 interrupts on the Z80. Essentially there is a 64-slot interrupt vector table that a device could send an index to with the interrupt. The program could then set handler addresses on any interrupts they were interested in.
* *32-bit support:* While still a 16-bit CPU, limited access to 32-bit values was added (like the Z80 could do with 16-bit values). This basically involved ganging the eight 16-bit registers into pairs to make four 32-bit registers (again like the Z80).
* *Floating point:* So much of Robot War (and especially Robot War 3) was going to require more precise angles and positioning. Supporting floating point in a limited coprocessor-like fashion was useful (again doubling up on the 32-bit registers).
* *Relative addressing:* Relative addressing was added for main memory, and both address and data stacks. This made relocatable code and local variables in functions much easier to write.

## The OZ-3 (2024)

I found old printouts and floppies of all this stuff. Some of it even ran in DOSBox. This sparked my interest again, and here it is starting up again.

