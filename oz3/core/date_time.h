// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_DATE_TIME_H_
#define OZ3_CORE_DATE_TIME_H_

#include <array>
#include <compare>
#include <cstdint>
#include <optional>

#include "absl/functional/any_invocable.h"
#include "absl/time/civil_time.h"
#include "gb/base/validated_context.h"

namespace oz3 {

//==============================================================================
// HostClock
//==============================================================================

// Returns the host's date and time, in the time zone the host wants programs to
// see.
using HostClock = absl::AnyInvocable<absl::CivilSecond()>;

// The host's clock, which a host gives the devices that need the date and time
// in the context it creates its Processor with (see DeviceFactory). It is
// optional: a device without a clock sees no date.
GB_CONTEXT_CONSTRAINT(kConstraintHostClock, kInOptional, HostClock);

//==============================================================================
// DateTime
//==============================================================================

// A date and time as the machine sees it, in four words: the year, the month
// (high byte) and day (low byte), the hour, and the minute (high byte) and
// second (low byte). All zeros is no date, since month 0 is invalid. The words
// compare in order chronologically.
//
// The timer answers programs with a DateTime, and the disk stamps files with
// one, so both use the same layout.
class DateTime {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // The words of a DateTime, in order.
  using Words = std::array<uint16_t, 4>;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // No date.
  DateTime() = default;

  // Returns `time`, with the year keeping its low 16 bits, as the timer always
  // has.
  static DateTime FromCivil(absl::CivilSecond time);

  // Returns `words` exactly as given, even if they aren't a valid date and
  // time, as words a program wrote may not be.
  static DateTime FromWords(const Words& words);

  // Returns the date and time from the host clock in `context`, or no date if
  // it has none. The context must allow reading kConstraintHostClock.
  static DateTime FromHostClock(const gb::ValidatedContext& context);

  //----------------------------------------------------------------------------
  // Operators
  //----------------------------------------------------------------------------

  auto operator<=>(const DateTime&) const = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns whether this is a date: its month isn't 0.
  bool IsSet() const { return (words_[1] >> 8) != 0; }

  // Returns the date and time, or nullopt with no date (month 0). Other values
  // out of range normalize as absl::CivilSecond does, so a day of 0 is the
  // last day of the month before.
  std::optional<absl::CivilSecond> ToCivil() const;

  // Returns the four words.
  const Words& GetWords() const { return words_; }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  explicit DateTime(const Words& words) : words_(words) {}

  Words words_ = {};
};

}  // namespace oz3

#endif  // OZ3_CORE_DATE_TIME_H_
