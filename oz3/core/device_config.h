// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_DEVICE_CONFIG_H_
#define OZ3_CORE_DEVICE_CONFIG_H_

#include <cstdint>
#include <string>
#include <vector>

#include "oz3/core/core_types.h"
#include "oz3/core/memory_bank_config.h"

namespace oz3 {

//==============================================================================
// DeviceInterrupt
//==============================================================================

// The wiring of one of a device's interrupts.
struct DeviceInterrupt {
  // The interrupt line, in the range [0, CpuCore::kInterruptCount).
  int interrupt = 0;

  // The cores the line is connected to, with one bit for each core index. Bits
  // for cores the processor doesn't have are ignored.
  uint8_t cores = kAllCores;
};

//==============================================================================
// DeviceMemory
//==============================================================================

// The wiring of a device's own pages in a memory bank.
struct DeviceMemory {
  // The bank the device's pages are in, in the range [0, kMaxMemoryBanks).
  int bank = 0;

  // The bank page the device's pages start at.
  int start_page = 0;

  // The number of pages the device has, which is 0 for a device without
  // pages. This is set by the device's factory.
  int page_count = 0;

  // Which of the device's pages cores can read and write, with bit 0 for its
  // first page. This is set by the device's factory.
  MemoryPageMasks masks;
};

//==============================================================================
// DeviceConfig
//==============================================================================

// A device's name, and its wiring to a Processor: its ports, interrupts, and
// pages.
//
// A derived DeviceFactory builds this from its constructor's arguments, so it
// always matches what its device needs. Two devices may share a port or an
// interrupt line, but not pages.
struct DeviceConfig {
  // The device's name, such as "Keyboard", which a Processor finds the device
  // by (see Processor::GetDevice). It must not be empty, and each device in a
  // ProcessorConfig must have its own name (see ProcessorConfig::Validate).
  std::string name;

  // The processor port for each of the device's ports.
  std::vector<int> ports;

  // The wiring of each of the device's interrupts.
  std::vector<DeviceInterrupt> interrupts;

  // The device's own pages.
  DeviceMemory memory;
};

}  // namespace oz3

#endif  // OZ3_CORE_DEVICE_CONFIG_H_
