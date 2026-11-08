// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_OZZY_H_
#define OZ3_OZZY_OZZY_H_

#include <memory>
#include <string_view>

#include "absl/status/statusor.h"
#include "absl/time/time.h"
#include "oz3/core/core_types.h"
#include "oz3/core/processor.h"
#include "oz3/devices/keyboard.h"
#include "oz3/devices/screen.h"
#include "oz3/devices/teletype.h"
#include "oz3/devices/timer.h"
#include "oz3/ozzy/ozzy_settings.h"

namespace oz3 {

//==============================================================================
// Ozzy
//==============================================================================

// An Ozzy: a Processor with the banks, cores, and devices its
// OzzySettings describe, the boot ROM (boot_rom.oza) in page 0 of bank 0, and
// the machine table in the boot ROM's page (see machine_table.h). Each bank is
// readable on its RAM pages and its ROM page, and writable on its RAM pages.
// Its other pages read 0 and ignore writes, but for a device's.
class Ozzy final {
 public:
  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The names of the devices in the Processor.
  static constexpr std::string_view kScreenName = "Screen";
  static constexpr std::string_view kKeyboardName = "Keyboard";
  static constexpr std::string_view kTeletypeName = "Teletype";
  static constexpr std::string_view kTimerName = "Timer";

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates an Ozzy from `settings`, powered on: core 0 is about to run the
  // boot ROM, and the other cores are idle. Returns an error if
  // the settings are invalid (see ValidateOzzySettings), or if a device or
  // the processor rejects them, naming the device.
  static absl::StatusOr<std::unique_ptr<Ozzy>> Create(
      const OzzySettings& settings);

  Ozzy(const Ozzy&) = delete;
  Ozzy& operator=(const Ozzy&) = delete;
  ~Ozzy();

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  Processor* GetProcessor() { return processor_.get(); }

  // Return the devices, each null if the Ozzy doesn't have one. A host gives
  // the timer its clock (see Timer::SetClock), as it has none of its own. The
  // clock's date and time move with the host's time, while ticks and
  // milliseconds move with the time given to Run, so the two only keep pace
  // while the host runs the Ozzy in real time.
  Screen* GetScreen() { return screen_; }
  Keyboard* GetKeyboard() { return keyboard_; }
  Teletype* GetTeletype() { return teletype_; }
  Timer* GetTimer() { return timer_; }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Runs the cycles that `time` is at the clock rate, and returns them. The
  // fraction of a cycle left over is carried to the next call, so many short
  // runs add up to the clock rate exactly. A `time` that isn't positive runs
  // nothing. It must be a real time between frames or steps, not anywhere
  // near the thousands of years whose cycles would overflow.
  Cycles Run(absl::Duration time);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  explicit Ozzy(std::unique_ptr<Processor> processor);

  const std::unique_ptr<Processor> processor_;
  Screen* const screen_;
  Keyboard* const keyboard_;
  Teletype* const teletype_;
  Timer* const timer_;

  // The time times the clock rate left over from the last Run, which is less
  // than a cycle.
  absl::Duration run_remainder_;
};

}  // namespace oz3

#endif  // OZ3_OZZY_OZZY_H_
