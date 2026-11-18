// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_OZZY_H_
#define OZ3_OZZY_OZZY_H_

#include <array>
#include <memory>
#include <string_view>

#include "absl/functional/any_invocable.h"
#include "absl/status/statusor.h"
#include "absl/time/civil_time.h"
#include "absl/time/time.h"
#include "gb/file/file_system.h"
#include "oz3/core/core_types.h"
#include "oz3/core/processor.h"
#include "oz3/devices/disk_drive.h"
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
  // Types
  //----------------------------------------------------------------------------

  // Returns the host's date and time, as the timer's and the drives' clocks
  // do.
  using Clock = absl::AnyInvocable<absl::CivilSecond()>;

  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The names of the devices in the Processor.
  static constexpr std::string_view kScreenName = "Screen";
  static constexpr std::string_view kKeyboardName = "Keyboard";
  static constexpr std::string_view kTeletypeName = "Teletype";
  static constexpr std::string_view kTimerName = "Timer";
  static constexpr std::array<std::string_view, kOzzyDriveCount> kDriveNames = {
      "A:", "B:", "C:", "D:"};

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates an Ozzy from `settings`, powered on: core 0 is about to run the
  // boot ROM, the other cores are idle, and each drive has the image its
  // settings give, opened in `file_system`. Returns an error if the settings
  // are invalid (see ValidateOzzySettings), if a device or the processor
  // rejects them, naming the device, or if an image doesn't open, naming its
  // drive.
  //
  // `file_system` may be null if no drive has an image. Otherwise it must
  // outlive the Ozzy, as the drives write to their images as it is destroyed.
  static absl::StatusOr<std::unique_ptr<Ozzy>> Create(
      const OzzySettings& settings, gb::FileSystem* file_system = nullptr);

  Ozzy(const Ozzy&) = delete;
  Ozzy& operator=(const Ozzy&) = delete;

  // Each drive writes back everything it hasn't committed to its image,
  // stamped with the clock, as at a clean shutdown.
  ~Ozzy();

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  Processor* GetProcessor() { return processor_.get(); }

  // Return the devices, each null if the Ozzy doesn't have one.
  Screen* GetScreen() { return screen_; }
  Keyboard* GetKeyboard() { return keyboard_; }
  Teletype* GetTeletype() { return teletype_; }
  Timer* GetTimer() { return timer_; }

  // Returns the drive with `letter`, 'A' to 'D', or null if the Ozzy doesn't
  // have it, or for any other character. The host may attach and detach its
  // images between runs (see DiskDrive), and must not attach one image to two
  // drives.
  DiskDrive* GetDrive(char letter);

  //----------------------------------------------------------------------------
  // Host interface
  //----------------------------------------------------------------------------

  // Sets the host's date and time for the timer and every drive, replacing
  // any clock it had, or none for the timer to read zeros and the drives to
  // stamp no date. The Ozzy has no clock of its own, so a host gives it one.
  // The clock's date and time move with the host's time, while the timer's
  // ticks and milliseconds move with the time given to Run, so the two only
  // keep pace while the host runs the Ozzy in real time. The drives call it as
  // the Ozzy is destroyed.
  void SetClock(Clock clock);

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

  // Returns a clock for a device that calls clock_, or none if there is none.
  Clock GetDeviceClock();

  // The host's clock, which the timer and the drives call. It is before the
  // processor, so it outlives the drives, which call it as they are destroyed.
  Clock clock_;

  const std::unique_ptr<Processor> processor_;
  Screen* const screen_;
  Keyboard* const keyboard_;
  Teletype* const teletype_;
  Timer* const timer_;
  std::array<DiskDrive*, kOzzyDriveCount> drives_;

  // The time times the clock rate left over from the last Run, which is less
  // than a cycle.
  absl::Duration run_remainder_;
};

}  // namespace oz3

#endif  // OZ3_OZZY_OZZY_H_
