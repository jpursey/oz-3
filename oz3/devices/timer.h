// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_TIMER_H_
#define OZ3_DEVICES_TIMER_H_

#include <cstdint>
#include <memory>
#include <string_view>
#include <utility>

#include "absl/functional/any_invocable.h"
#include "absl/status/statusor.h"
#include "absl/time/civil_time.h"
#include "oz3/core/core_types.h"
#include "oz3/core/device.h"
#include "oz3/core/device_config.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/lockable.h"

namespace oz3 {

//==============================================================================
// Clock indexes
//==============================================================================

// What a program asks the clock port for, by writing it to port word 0. The
// timer answers with a dword, and answers any other index with 0.

// The low 32 bits of the cycle count: word 0 the low word.
inline constexpr uint16_t kTimerCycles = 0;

// The low 32 bits of the milliseconds since power on, in the machine's time:
// the cycle count at the clock rate, so it is the same at any clock rate.
inline constexpr uint16_t kTimerMilliseconds = 1;

// The host's date, as latched by the last kTimerTime: word 0 the year, and word
// 1 the month (1 to 12) in the high byte and the day (1 to 31) in the low byte.
// It is 0 before the first kTimerTime, or without a clock.
inline constexpr uint16_t kTimerDate = 2;

// The host's time: word 0 the hour (0 to 23), and word 1 the minute in the high
// byte and the second in the low byte. Asking for the time latches the date and
// time, which kTimerDate then answers with.
inline constexpr uint16_t kTimerTime = 3;

//==============================================================================
// TimerSettings
//==============================================================================

// Everything a host sets about a timer but its name: its wiring, and how often
// it ticks.
struct TimerSettings {
  bool operator==(const TimerSettings&) const = default;

  // The first of Timer::kPorts consecutive processor ports: the tick port, and
  // then the clock port.
  int port = 0;

  // The interrupt raised at each tick.
  DeviceInterrupt interrupt;

  // The ticks a second, which must be at least 1.
  int tick_hz = 60;
};

//==============================================================================
// TimerFactory
//==============================================================================

// A timer on its ports, which a Processor creates a Timer from.
class TimerFactory final : public DeviceFactory {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a factory for a timer named `name`. Returns an error if `settings`
  // are invalid. The ports must also be ones the processor has, which
  // ProcessorConfig::Validate checks.
  static absl::StatusOr<std::unique_ptr<TimerFactory>> Create(
      std::string_view name, const TimerSettings& settings);

  ~TimerFactory() override = default;

  //----------------------------------------------------------------------------
  // DeviceFactory implementation
  //----------------------------------------------------------------------------

  const DeviceConfig& GetConfig() const override { return config_; }
  std::unique_ptr<Device> CreateDevice() const override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  TimerFactory(std::string_view name, const TimerSettings& settings);

  const DeviceConfig config_;
  const TimerSettings settings_;
};

//==============================================================================
// Timer
//==============================================================================

// A timer ticks at a fixed rate, and tells a program the cycle count, the
// milliseconds since power on, and the host's date and time.
//
// The timer ticks tick_hz times a second: tick n is at cycle
// n * clock_hz / tick_hz, so the schedule doesn't drift where a tick isn't a
// whole number of cycles. At a tick, it writes the ticks since power on to the
// tick port, as a dword with the low word first, sets its status, and raises
// its interrupt. So INS on the tick port reads only when a tick has passed
// since it was last read. A tick waits while a core holds the port, and one
// that waited doesn't delay the next. At most one tick is in a cycle.
//
// The clock port answers what a program asks for, by index (see kTimerCycles,
// kTimerMilliseconds, kTimerDate, and kTimerTime). A program writes the index
// to port word 0, such as with OUT, which sets the status. The cycle after, the
// timer takes the index, clearing the status, and writes its answer to the
// port, where IN reads it. As the timer requests the port's lock before the
// next instruction can, an IN right after the OUT reads the answer. The cycle
// count is the processor's when the timer answers.
//
// The port holds one question at a time. A core that writes an index while
// another core's is still waiting for the timer replaces it, and both read the
// answer to the second, so cores that share the timer take turns asking.
//
// The date and time are the host's, from the clock it sets with SetClock(),
// which the timer calls only when a program asks for the time. Without a
// clock, both are zeros, which means no date, since month 0 is invalid. They
// are the host's time, not the machine's: they have nothing to do with the
// cycle count, ticks, or milliseconds, so when a host runs the machine slower
// or faster than its clock rate, the date and time drift from them.
//
// The timer sleeps until its next tick, or a write to the clock port.
class Timer final : public Device {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // Returns the host's date and time, in the time zone the host wants programs
  // to see. A year outside 0 to 65535 reads as its low 16 bits.
  using Clock = absl::AnyInvocable<absl::CivilSecond()>;

  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The ports the timer is wired to.
  static constexpr int kPorts = 2;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  ~Timer() override = default;

  //----------------------------------------------------------------------------
  // Host interface
  //
  // Called between Processor::Execute calls.
  //----------------------------------------------------------------------------

  // Sets the clock the timer gets the date and time from, replacing any it had,
  // or none for the timer to answer with zeros for both. The timer calls
  // it from inside Processor::Execute, each time a program asks for the time.
  void SetClock(Clock clock) { clock_ = std::move(clock); }

  // Returns the number of ticks since power on, which the tick port holds the
  // low 32 bits of.
  int64_t GetTickCount() const { return tick_count_; }

 protected:
  //----------------------------------------------------------------------------
  // ExecutionComponent implementation
  //----------------------------------------------------------------------------

  void Execute() override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The factory constructs the timer with its validated settings.
  friend class TimerFactory;

  Timer(DeviceConfig config, const TimerSettings& settings);

  // Returns the cycle of the next tick.
  Cycles GetTickCycle() const;

  // Ticks if a tick is due. Returns whether the timer is waiting for the tick
  // port's lock.
  bool ExecuteTick();

  // Answers the clock port if a program asked it something. Returns whether
  // the timer is waiting for the clock port's lock.
  bool ExecuteClock();

  // Answers `index` on the clock port, with `lock` holding it.
  void AnswerClock(const Lock& lock, uint16_t index);

  // Latches the clock's date and time as kTimerDate and kTimerTime answer
  // them, or zeros without a clock.
  void LatchDateTime();

  const int tick_hz_;
  int64_t tick_count_ = 0;
  Clock clock_;
  uint32_t latched_date_ = 0;         // At the last kTimerTime.
  uint32_t latched_time_ = 0;         // At the last kTimerTime.
  std::unique_ptr<Lock> tick_lock_;   // While it is requested.
  std::unique_ptr<Lock> clock_lock_;  // While it is requested.
};

}  // namespace oz3

#endif  // OZ3_DEVICES_TIMER_H_
