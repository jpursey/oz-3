// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_TELETYPE_H_
#define OZ3_DEVICES_TELETYPE_H_

#include <memory>
#include <string>
#include <string_view>

#include "absl/status/statusor.h"
#include "oz3/core/device.h"
#include "oz3/core/device_config.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/lockable.h"

namespace oz3 {

//==============================================================================
// TeletypeSettings
//==============================================================================

// Everything a host sets about a teletype but its name: its wiring, and how it
// behaves.
struct TeletypeSettings {
  // The smallest buffer size, which holds one dword's characters.
  static constexpr int kMinBufferSize = 4;

  // The bits sent for each character at the baud rate: a start bit, 8 data
  // bits, and a stop bit.
  static constexpr int kBitsPerChar = 10;

  // The processor port the teletype is on.
  int port = 0;

  // The rate the teletype prints at, in bits per second, with kBitsPerChar
  // bits for each character, or 0 to print as fast as cores write. It must not
  // be negative.
  int baud = 0;

  // The most characters the teletype keeps for the host, which must be at
  // least kMinBufferSize.
  int buffer_size = 4096;
};

//==============================================================================
// TeletypeFactory
//==============================================================================

// A teletype on one port, which a Processor creates a Teletype from.
class TeletypeFactory final : public DeviceFactory {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a factory for a teletype named `name`. Returns an error if
  // `settings` are invalid.
  static absl::StatusOr<std::unique_ptr<TeletypeFactory>> Create(
      std::string_view name, const TeletypeSettings& settings);

  ~TeletypeFactory() override = default;

  //----------------------------------------------------------------------------
  // DeviceFactory implementation
  //----------------------------------------------------------------------------

  const DeviceConfig& GetConfig() const override { return config_; }
  std::unique_ptr<Device> CreateDevice() const override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  TeletypeFactory(std::string_view name, const TeletypeSettings& settings);

  const DeviceConfig config_;
  const TeletypeSettings settings_;
};

//==============================================================================
// Teletype
//==============================================================================

// A teletype writes text to the host, for a machine without a screen.
//
// Each word a core writes to its port is two packed characters, the high byte
// first, and a zero byte is skipped, so a single character is written as
// itself ('A' is 0041h), and a packed string can be written straight from
// memory with OUTR. A dword write is two words, word 0 first, so OUTR.ID
// writes a packed string from memory twice as fast. The teletype takes what is
// written as soon as the port's status is set, clears the status, so the next
// OUTS succeeds, and clears word 1, so a word written later isn't followed by
// an earlier dword's characters.
//
// At a baud rate, the teletype is busy printing what it took, and takes
// nothing more until it is done. The port holds one more write meanwhile, as a
// serial line's holding register did, and then its status stays set: OUTS
// fails, and OUTR stops early. A program paces itself to the baud rate, or
// loops.
//
// Characters are 7 bits: the eighth bit of each byte is dropped (it was a
// teletype's parity bit), and a byte that is then 0 is skipped, taking no
// time. The rest go to the host as they are, control characters included, and
// the teletype keeps them until the host takes them with TakeOutput(). Without
// room for a dword's characters, it takes nothing, so the port's status stays
// set until the host makes room.
class Teletype final : public Device {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  ~Teletype() override = default;

  //----------------------------------------------------------------------------
  // Host interface
  //----------------------------------------------------------------------------

  // Returns the characters written since the last call, which makes room for
  // more. Called between Processor::Execute calls.
  std::string TakeOutput();

 protected:
  //----------------------------------------------------------------------------
  // ExecutionComponent implementation
  //----------------------------------------------------------------------------

  void Execute() override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The factory constructs the teletype with its validated settings.
  friend class TeletypeFactory;

  Teletype(DeviceConfig config, const TeletypeSettings& settings);

  // Returns true if there is no room for another dword's characters.
  bool IsFull() const;

  const int baud_;
  const int buffer_size_;
  std::string output_;          // Characters the host hasn't taken yet.
  std::unique_ptr<Lock> lock_;  // The port's lock, while it is requested.
};

}  // namespace oz3

#endif  // OZ3_DEVICES_TELETYPE_H_
