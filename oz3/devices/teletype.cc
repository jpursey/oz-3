// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/teletype.h"

#include <cstdint>
#include <utility>

#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"
#include "oz3/core/core_types.h"
#include "oz3/core/port.h"

namespace oz3 {

//==============================================================================
// TeletypeFactory
//==============================================================================

absl::StatusOr<std::unique_ptr<TeletypeFactory>> TeletypeFactory::Create(
    std::string_view name, const TeletypeSettings& settings) {
  if (settings.baud < 0) {
    return absl::InvalidArgumentError(
        absl::StrCat("Teletype ", name, " has a baud rate of ", settings.baud,
                     ", but it must not be negative"));
  }
  if (settings.buffer_size < TeletypeSettings::kMinBufferSize) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Teletype ", name, " has a buffer size of ", settings.buffer_size,
        ", but it must be at least ", TeletypeSettings::kMinBufferSize));
  }
  return absl::WrapUnique(new TeletypeFactory(name, settings));
}

TeletypeFactory::TeletypeFactory(std::string_view name,
                                 const TeletypeSettings& settings)
    : config_({.name = std::string(name), .ports = {settings.port}}),
      settings_(settings) {}

std::unique_ptr<Device> TeletypeFactory::CreateDevice() const {
  return absl::WrapUnique(new Teletype(config_, settings_));
}

//==============================================================================
// Teletype
//==============================================================================

Teletype::Teletype(DeviceConfig config, const TeletypeSettings& settings)
    : Device(std::move(config)),
      baud_(settings.baud),
      buffer_size_(settings.buffer_size) {}

bool Teletype::IsFull() const {
  return buffer_size_ - static_cast<int>(output_.size()) <
         TeletypeSettings::kMinBufferSize;
}

std::string Teletype::TakeOutput() {
  if (IsFull()) {
    Wake();
  }
  return std::exchange(output_, {});
}

void Teletype::Execute() {
  // A full teletype leaves the port as it is until the host takes its output.
  if (IsFull()) {
    Sleep();
    return;
  }
  if (lock_ == nullptr) {
    lock_ = LockPort(0);
  }
  if (!lock_->IsLocked()) {
    AdvanceCycles(1);
    return;
  }
  const int start_size = static_cast<int>(output_.size());
  Port& port = GetPort(0);
  uint16_t words[2] = {0, 0};
  if (port.LoadWord(*lock_, Port::T | Port::A, words[0]) != 0) {
    port.LoadWord(*lock_, Port::S, words[1]);

    // Word 1 is cleared once taken, so a word written later (which leaves word
    // 1 as it is) doesn't write its characters again.
    port.StoreWord(*lock_, 0, 0);
    for (const uint16_t word : words) {
      // Characters are 7 bits, as the eighth was a teletype's parity bit.
      for (const int c : {word & 0x7F, (word >> 8) & 0x7F}) {
        if (c != 0) {
          output_ += static_cast<char>(c);
        }
      }
    }
  }
  lock_.reset();

  // At a baud rate, the teletype is busy printing what it took. A wake while
  // it is busy (a core writing the port) runs it once it is done, as a wake
  // never moves a component's cycles back.
  if (baud_ > 0) {
    const int64_t chars = static_cast<int>(output_.size()) - start_size;
    AdvanceCycles(chars * TeletypeSettings::kBitsPerChar * GetClockHz() /
                  baud_);
  }
  Sleep();
}

}  // namespace oz3
