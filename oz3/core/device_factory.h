// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_DEVICE_FACTORY_H_
#define OZ3_CORE_DEVICE_FACTORY_H_

#include <memory>

#include "oz3/core/core_types.h"
#include "oz3/core/device_config.h"

namespace oz3 {

//==============================================================================
// DeviceFactory
//==============================================================================

// A DeviceFactory is one configured device: it holds the device's name and
// wiring, and its own settings (such as a screen's size), and creates the
// Device for each Processor built with it.
//
// A factory is immutable once constructed, so a ProcessorConfig holds it as a
// shared pointer, and any number of configs can share it. A Processor finds
// the device it created by the device's name (see Processor::GetDevice).
class DeviceFactory {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  DeviceFactory(const DeviceFactory&) = delete;
  DeviceFactory& operator=(const DeviceFactory&) = delete;
  virtual ~DeviceFactory() = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the device's name and wiring, which is the same config for the
  // factory's whole life. ProcessorConfig::Validate checks it.
  virtual const DeviceConfig& GetConfig() const = 0;

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Creates a device for one Processor, wired as GetConfig(). The device holds
  // its own copy of what it needs, so it doesn't depend on this factory.
  virtual std::unique_ptr<Device> CreateDevice() const = 0;

 protected:
  DeviceFactory() = default;
};

}  // namespace oz3

#endif  // OZ3_CORE_DEVICE_FACTORY_H_
