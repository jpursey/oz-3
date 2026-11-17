# Devices

A **device** is hardware outside the OZ-3 itself, such as a screen or a keyboard, that runs beside the cores (see [Specifications](2-Specifications.md)). These pages describe the ready-made devices that come with the OZ-3, as an OZ-3 program sees them: their ports, interrupts, memory, and timing.

The ready-made devices are general purpose. Any OZ-3 processor can have them, with whatever wiring and settings suit it. The [Ozzy Computer](4-Ozzy-Computer.md) is one machine built from them, with its own fixed wiring.

| Device                          | Ports        | Interrupts | Pages | What                                                                         |
| ------------------------------- | ------------ | ---------- | ----- | ---------------------------------------------------------------------------- |
| [Teletype](3.1-Teletype.md)     | 1            | none       | none  | A port that writes text to the host                                          |
| [Keyboard](3.2-Keyboard.md)     | 1, 10, or 11 | 1 or none  | none  | A port of typed keys with an interrupt for each, ports of key state, or both |
| [Screen](3.3-Screen.md)         | none         | 1          | 1     | A page of character cells and control words                                  |
| [Timer](3.4-Timer.md)           | 2            | 1          | none  | A periodic interrupt, and a port of the cycle count, time, and date          |
| [Disk Drive](3.5-Disk-Drive.md) | 3            | 1          | none  | A smart drive that a program asks for files by name, on disk images          |

### Wiring and settings

The host application makes each device with its **wiring** and its **settings**:

* **The wiring** is what the device is connected to: its ports, its interrupt lines, the cores each is connected to, and whether it raises a shared interrupt (which only one of those cores handles, see [Interrupts](2-Specifications.md#interrupts)), and the bank and page of its memory. It is the machine's choice, not the device's, so a device works the same wherever it is wired. Each device's page says what it needs.
* **The settings** are how the device behaves: a screen's size, or a teletype's baud rate. Each device's page lists them, with their defaults.

A program learns how its machine's devices are wired from the machine itself, such as from the Ozzy's [machine table](4-Ozzy-Computer.md#the-machine-table).

### Timing

Devices run in step with the cores, cycle by cycle, and use ports and memory banks through their locks as cores do (see [Synchronization](2-Specifications.md#synchronization)), so everything a device does is in cycles of the machine's clock. A setting in real time, such as a refresh rate or a baud rate, is turned into cycles with the processor's clock rate, so a device works the same at any clock rate.
