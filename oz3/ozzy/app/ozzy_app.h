// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_APP_OZZY_APP_H_
#define OZ3_OZZY_APP_OZZY_APP_H_

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/time/time.h"
#include "gb/base/clock.h"
#include "gb/base/validated_context.h"
#include "gb/cli/cli_command_line.h"
#include "gb/cli/cli_path.h"
#include "gb/game/game.h"
#include "oz3/core/core_types.h"
#include "oz3/devices/power_control/power_control.h"
#include "oz3/ozzy/app/frontend.h"
#include "oz3/ozzy/app/ozzy_host.h"

namespace oz3 {

//==============================================================================
// OzzyAppStats
//==============================================================================

// How an app's Ozzy ran, from its first power on, through each restart, to its
// last power off (what --stats writes).
struct OzzyAppStats {
  // The frames that ran the Ozzy.
  int64_t frames = 0;

  // The time from the frame before each of those frames to it, even where it
  // ran the Ozzy for less (at most OzzyApp::kMaxRunTime). So the rates show how
  // fast the Ozzy ran in real time.
  absl::Duration time;

  // The time those frames took, without waiting for the next.
  absl::Duration frame_time;

  // The cycles those frames ran.
  Cycles cycles = 0;
};

//==============================================================================
// OzzyApp
//==============================================================================

// Runs an Ozzy until the user quits, presented by the frontend of its
// context's frontend type. It is the whole of an Ozzy app's logic, whichever
// frontend presents it: its command line, reading the config, the OzzyHost,
// what a program's power requests do, the frame loop, and the stats.
//
// Each frame runs the Ozzy for the time since the last, at most kMaxRunTime,
// so a host that falls behind slows the Ozzy rather than racing to catch up.
// The frame rate is gb::Game's (kConstraintMaxFps).
class OzzyApp final : public gb::Game {
 public:
  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The most time a frame runs the Ozzy for.
  static constexpr absl::Duration kMaxRunTime = absl::Milliseconds(100);

  //----------------------------------------------------------------------------
  // Contract constraints
  //----------------------------------------------------------------------------

  // REQUIRED: The clock gb::Game runs the frames by (its optional
  // kConstraintClock), which the app times them by too. main gives it the
  // real time clock.
  static GB_CONTEXT_CONSTRAINT(kConstraintGameClock, kInRequired, gb::Clock);

  // REQUIRED: How the app is presented.
  static GB_CONTEXT_CONSTRAINT(kConstraintFrontendType, kInRequired,
                               FrontendType);

  // OUTPUT: The app's exit code, which Run leaves in the context for main to
  // return: as gb::RunCommandLine returns it, or 1 if the app failed once it
  // started.
  static inline constexpr const char* kKeyExitCode = "ozzy_exit_code";
  static GB_CONTEXT_CONSTRAINT_NAMED(kConstraintExitCode, kOutRequired, int,
                                     kKeyExitCode);

  // SCOPED: The app, while its command line runs, which is how the command
  // finds it.
  static GB_CONTEXT_CONSTRAINT(kConstraintOzzyApp, kScoped, OzzyApp);

  // What the app reads and writes: the above, and gb_cli's file system
  // (gb::kConstraintCliFileSystem), which the config file and the disk images
  // are opened in, and writer (gb::kConstraintCliWriter), which the app's
  // errors and stats go to, as gb_cli's help and errors do.
  using Contract = gb::DerivedContextContract<
      GameContract, kConstraintGameClock, kConstraintFrontendType,
      kConstraintExitCode, kConstraintOzzyApp, gb::kConstraintCliFileSystem,
      gb::kConstraintCliWriter>;

  //----------------------------------------------------------------------------
  // gb::Game interface
  //----------------------------------------------------------------------------

  gb::ValidatedContext& Context() override { return context_; }

 protected:
  bool Init(const std::vector<std::string_view>& args) override;
  bool Update(absl::Duration delta_time) override;
  void CleanUp() override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The command line's run function, which starts the app found in
  // `context`.
  static bool RunCommand(gb::ValidatedContext& context,
                         const gb::CliCommandLine& command_line);

  // Starts the app as `command_line` asks: reads the config, sets up the
  // frontend, creates the host, powers the Ozzy on, and creates the frontend
  // to show it. Returns whether it started, failing (see Fail) if it didn't.
  bool Start(const gb::CliCommandLine& command_line);

  // Stops the app with exit code 1, keeping `message` for CleanUp to write
  // once the frontend is gone. Returns false.
  bool Fail(std::string_view message);

  // Powers the host's Ozzy on, or fails, without attaching it to the frontend.
  // Only Start calls it directly, as it creates the frontend in between.
  bool PowerOnHost();

  // Powers the Ozzy on and attaches it to the frontend, or fails.
  bool PowerOn();

  // Detaches the Ozzy from the frontend, with `exit_code` if a program shut
  // it down, and powers it off.
  void PowerOff(std::optional<int> exit_code);

  // Does what a program's power request asks. Returns false if the app can't
  // go on.
  bool DoPowerRequest(const PowerRequest& request);

  // Gives the frontend's keys to the keyboard, or drops them without one.
  void HandleKeys();

  // Writes the error that stopped the app, if one did, and the stats if they
  // were asked for.
  void WriteResults();

  gb::ValidatedContext context_;
  gb::Clock* clock_ = nullptr;

  // Set by Start, once it has created them.
  std::unique_ptr<OzzyHost> host_;
  std::unique_ptr<Frontend> frontend_;
  bool write_stats_ = false;

  // Why the app failed, if it did (see Fail).
  std::string error_;

  OzzyAppStats stats_;
};

}  // namespace oz3

#endif  // OZ3_OZZY_APP_OZZY_APP_H_
