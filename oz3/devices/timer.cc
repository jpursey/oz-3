// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/timer.h"

#include <algorithm>
#include <string>
#include <utility>

#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"
#include "oz3/core/port.h"

namespace oz3 {

namespace {

// The timer's ports, as numbered in its DeviceConfig.
constexpr int kTickPort = 0;
constexpr int kClockPort = 1;

// Returns `low` and `high` as the low and high words of a dword.
uint32_t GetDword(uint16_t low, uint16_t high) {
  return low | static_cast<uint32_t>(high) << 16;
}

// Stores `value` to `port` with `lock`, which holds it: the low word first,
// and then the high word with `mode`.
void StoreDword(Port& port, const Lock& lock, uint16_t mode, uint32_t value) {
  port.StoreWord(lock, Port::A, static_cast<uint16_t>(value));
  port.StoreWord(lock, mode, static_cast<uint16_t>(value >> 16));
}

}  // namespace

//==============================================================================
// TimerFactory
//==============================================================================

absl::StatusOr<std::unique_ptr<TimerFactory>> TimerFactory::Create(
    std::string_view name, const TimerSettings& settings) {
  if (settings.tick_hz < 1) {
    return absl::InvalidArgumentError(
        absl::StrCat("Timer ", name, " has a tick rate of ", settings.tick_hz,
                     ", but it must be at least 1"));
  }
  return absl::WrapUnique(new TimerFactory(name, settings));
}

TimerFactory::TimerFactory(std::string_view name, const TimerSettings& settings)
    : config_({.name = std::string(name),
               .ports = {settings.port, settings.port + 1},
               .interrupts = {settings.interrupt}}),
      settings_(settings) {}

std::unique_ptr<Device> TimerFactory::CreateDevice() const {
  return absl::WrapUnique(new Timer(config_, settings_));
}

//==============================================================================
// Timer
//==============================================================================

Timer::Timer(DeviceConfig config, const TimerSettings& settings)
    : Device(std::move(config)), tick_hz_(settings.tick_hz) {}

Cycles Timer::GetTickCycle() const {
  return (tick_count_ + 1) * GetClockHz() / tick_hz_;
}

void Timer::Execute() {
  // Both run each time, so the timer waits for every lock it needs at once.
  const bool tick_waiting = ExecuteTick();
  const bool clock_waiting = ExecuteClock();
  if (tick_waiting || clock_waiting) {
    AdvanceCycles(1);
    return;
  }

  // The next tick is on the schedule, however long this one waited.
  Sleep(std::max<Cycles>(GetTickCycle() - GetCycles(), 1));
}

bool Timer::ExecuteTick() {
  if (GetCycles() < GetTickCycle()) {
    return false;
  }
  if (tick_lock_ == nullptr) {
    tick_lock_ = LockPort(kTickPort);
  }
  if (!tick_lock_->IsLocked()) {
    return true;
  }
  ++tick_count_;
  StoreDword(GetPort(kTickPort), *tick_lock_, Port::S,
             static_cast<uint32_t>(tick_count_));
  tick_lock_.reset();
  RaiseInterrupt(0);
  return false;
}

bool Timer::ExecuteClock() {
  if (clock_lock_ == nullptr) {
    // A write sets the status, and it only changes under the port's lock, so
    // a port with its status clear needs no lock to know it wasn't written.
    // Once the lock is requested, it is released when it is granted.
    if (GetPort(kClockPort).GetStatus() == 0) {
      return false;
    }
    clock_lock_ = LockPort(kClockPort);
  }
  if (!clock_lock_->IsLocked()) {
    return true;
  }

  // A read while the timer waited for the lock takes the index back.
  Port& port = GetPort(kClockPort);
  uint16_t index = 0;
  if (port.LoadWord(*clock_lock_, Port::T | Port::S, index) != 0) {
    AnswerClock(*clock_lock_, index);
  }
  clock_lock_.reset();
  return false;
}

void Timer::AnswerClock(const Lock& lock, uint16_t index) {
  uint32_t answer = 0;
  switch (index) {
    case kTimerCycles:
      answer = static_cast<uint32_t>(GetCycles());
      break;
    case kTimerMilliseconds: {
      // Whole seconds first, so the product can't overflow.
      const int64_t clock_hz = GetClockHz();
      answer = static_cast<uint32_t>(GetCycles() / clock_hz * 1000 +
                                     GetCycles() % clock_hz * 1000 / clock_hz);
    } break;
    case kTimerDate: {
      const DateTime::Words& words = latched_.GetWords();
      answer = GetDword(words[0], words[1]);
    } break;
    case kTimerTime: {
      LatchDateTime();
      const DateTime::Words& words = latched_.GetWords();
      answer = GetDword(words[2], words[3]);
    } break;
  }

  // The status stays clear, as the timer took the index.
  StoreDword(GetPort(kClockPort), lock, 0, answer);
}

void Timer::LatchDateTime() {
  latched_ = clock_ != nullptr ? DateTime::FromCivil(clock_()) : DateTime();
}

}  // namespace oz3
