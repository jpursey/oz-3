// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_MACHINE_OZZY_H_
#define OZ3_OZZY_MACHINE_OZZY_H_

#include <array>
#include <memory>
#include <string_view>

#include "absl/status/statusor.h"
#include "absl/time/time.h"
#include "gb/base/context.h"
#include "gb/base/validated_context.h"
#include "gb/file/file_system.h"
#include "oz3/core/core_types.h"
#include "oz3/core/processor.h"
#include "oz3/devices/disk_drive/disk_drive.h"
#include "oz3/devices/keyboard/keyboard.h"
#include "oz3/devices/power_control/power_control.h"
#include "oz3/devices/screen/screen.h"
#include "oz3/devices/teletype/teletype.h"
#include "oz3/devices/timer/timer.h"
#include "oz3/ozzy/machine/ozzy_settings.h"

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
  // Contract
  //----------------------------------------------------------------------------

  // The file system the drives' images are opened in, which only an Ozzy whose
  // settings give a drive an image needs. It must outlive the Ozzy, as the
  // drives write to their images as it is destroyed.
  static GB_CONTEXT_CONSTRAINT(kConstraintFileSystem, kInOptional,
                               gb::FileSystem);

  // What the Ozzy takes from its context: the file system, and the host's
  // callback for the requests programs make on the power port to shut down or
  // restart (PowerControl::kConstraintPowerRequestCallback), which every Ozzy
  // needs. The callback is called from inside Run, so it must not destroy the
  // Ozzy or run it: the host notes the request, and acts on it once Run
  // returns. The context is also the processor's, so it holds what the devices
  // need from the host too (see Create).
  using Contract =
      gb::ContextContract<kConstraintFileSystem,
                          PowerControl::kConstraintPowerRequestCallback>;

  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The names of the devices in the Processor.
  static constexpr std::string_view kPowerName = "Power";
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
  // settings give, opened in the context's file system. Returns an error if the
  // context doesn't meet the contract, if the settings are invalid (see
  // ValidateOzzySettings), if a device or the processor rejects them, naming
  // the device, or if an image doesn't open, naming its drive.
  //
  // The Ozzy gives the context to its processor, so it is also where the
  // devices find the host's clock (kConstraintHostClock): the date
  // and time the timer answers programs with and the drives stamp files with.
  // Without one, it is the host's local time, and with an empty one, the timer
  // reads zeros and the drives stamp no date. The clock's date and time move
  // with the host's time, while the timer's ticks and milliseconds move with
  // the time given to Run, so the two only keep pace while the host runs the
  // Ozzy in real time. The drives call it as the Ozzy is destroyed, so
  // whatever it uses must outlive the Ozzy, as must the context itself unless
  // the contract owns it.
  static absl::StatusOr<std::unique_ptr<Ozzy>> Create(
      const OzzySettings& settings, Contract contract);

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
  std::array<DiskDrive*, kOzzyDriveCount> drives_;

  // The time times the clock rate left over from the last Run, which is less
  // than a cycle.
  absl::Duration run_remainder_;
};

}  // namespace oz3

#endif  // OZ3_OZZY_MACHINE_OZZY_H_
