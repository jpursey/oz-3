// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <algorithm>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/time/clock.h"
#include "absl/time/time.h"
#include "gb/base/context.h"
#include "gb/base/sleep.h"
#include "gb/base/validated_context.h"
#include "gb/cli/cli.h"
#include "gb/cli/cli_command_line.h"
#include "gb/cli/cli_path.h"
#include "gb/cli/cli_writer.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "gb/file/path.h"
#include "oz3/core/core_types.h"
#include "oz3/core/processor.h"
#include "oz3/devices/keyboard.h"
#include "oz3/devices/power_control.h"
#include "oz3/ozzy/app/ozzy_host.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_settings.h"
#include "oz3/ozzy/term/console.h"
#include "oz3/ozzy/term/console_display.h"

namespace oz3 {
namespace {

//==============================================================================
// Command line
//==============================================================================

const gb::CliFlag<bool> kMaxSpeed = {
    .name = "max-speed",
    .help =
        "Runs the Ozzy as fast as the host can, rather than at its clock "
        "rate.",
};
const gb::CliFlag<bool> kStats = {
    .name = "stats",
    .help =
        "Writes how the Ozzy ran once it quits: the frames a second, the time "
        "each took on average to run the Ozzy and draw the console (which "
        "with --max-speed is the whole frame), and the cycles a second.",
};
const gb::CliArg<std::string_view> kConfigFile = {
    .name = "config file",
    .help =
        "The Ozzy to run (.ozzy). Without one, it runs the default Ozzy, "
        "which an empty config file describes.",
    .optional = true,
    .validators = {&gb::kCliFileExists},
};

bool Run(gb::ValidatedContext& context, const gb::CliCommandLine& command_line);

const gb::CliCommand kProgram = {
    .name = "ozzy",
    .help =
        "Runs an Ozzy computer in the console, until Ctrl+Alt+Q or "
        "Ctrl+Break, or the console's window is closed. A program on it can "
        "restart it, or shut it down, which leaves it off until it quits. Its "
        "drives have the disks the config gives them, and write back what "
        "they hold when it quits or the Ozzy powers off. Paths are written the "
        "operating system's way, absolute or "
        "relative to the current directory, and a config's disk images "
        "relative to its folder.",
    .flags = {&kMaxSpeed, &kStats},
    .args = {&kConfigFile},
    .constraints = {&gb::kConstraintCliFileSystem},
    .run = Run,
};

//==============================================================================
// Running
//==============================================================================

// The time between frames, at each of which the console is drawn.
const absl::Duration kFrameTime = absl::Seconds(1) / 60;

// The most time a frame runs the Ozzy for, so a host that falls behind slows
// the Ozzy rather than racing to catch up.
constexpr absl::Duration kMaxRunTime = absl::Milliseconds(100);

// With --max-speed, the cycles each run is, which is short enough to check the
// clock often, whatever the clock rate.
constexpr int64_t kMaxSpeedRunCycles = 10'000;

// Returns the settings of the Ozzy the command line asks for, or nothing after
// writing the error.
std::optional<OzzySettings> ReadSettings(
    const gb::CliWriter& writer, gb::FileSystem& file_system,
    const gb::CliCommandLine& command_line) {
  const std::optional<std::string_view> config_file =
      command_line.Get(kConfigFile);
  if (!config_file.has_value()) {
    return OzzySettings();
  }

  // The validator of the config file checks that its path normalizes.
  const std::string path = gb::NormalizePath(*config_file);
  std::string text;
  if (!file_system.ReadFile(path, &text)) {
    writer(gb::CliText::kError, absl::StrCat("Failed to read ", path));
    return std::nullopt;
  }
  absl::StatusOr<OzzySettings> settings =
      ReadOzzySettings(std::move(text), path);
  if (!settings.ok()) {
    writer(gb::CliText::kError, settings.status().message());
    return std::nullopt;
  }
  return *std::move(settings);
}

// How the runs of the Ozzy went, from its first power on, through each
// restart, to the last power off.
struct RunStats {
  int64_t frames = 0;
  absl::Duration time;  // From each first frame's start to its last's end.
  absl::Duration frame_time;  // The time frames took, without waiting.
  Cycles cycles = 0;
};

// Runs `host`'s Ozzy, which is on, in `console` until the user quits or a
// program asks to power off, each frame for the time since the last, or with
// `max_speed_run_time`, in runs of that time until the next frame. Returns the
// program's request, if one ended the run, and adds how it ran to `stats`.
std::optional<PowerRequest> RunOzzy(
    OzzyHost& host, Console& console,
    std::optional<absl::Duration> max_speed_run_time, RunStats* stats) {
  Ozzy* const ozzy = host.GetOzzy();
  Keyboard* const keyboard = ozzy->GetKeyboard();
  ConsoleDisplay display(*ozzy, console);
  std::optional<PowerRequest> power_request;
  const Cycles start_cycles = ozzy->GetProcessor()->GetCycles();
  const absl::Time start_time = absl::Now();
  absl::Time last_time = start_time;
  absl::Time next_frame = last_time;
  while (!console.IsQuitRequested()) {
    const absl::Time frame_start = absl::Now();

    // Keys are taken without a keyboard too, so they don't pile up.
    const ConsoleKeys keys = console.TakeKeys();
    if (keyboard != nullptr) {
      for (const uint16_t key : keys.typed) {
        keyboard->TypeKey(key);
      }
      for (const KeyChange& change : keys.changes) {
        keyboard->SetKeyState(change.key, change.held);
      }
    }

    next_frame += kFrameTime;
    absl::Time now = absl::Now();
    if (max_speed_run_time.has_value()) {
      while (now < next_frame && !power_request.has_value()) {
        power_request = host.Run(*max_speed_run_time).power_request;
        now = absl::Now();
      }
    } else {
      power_request =
          host.Run(std::min(now - last_time, kMaxRunTime)).power_request;
    }
    if (power_request.has_value()) {
      // The Ozzy is about to power off, so this frame isn't shown.
      break;
    }
    last_time = now;
    display.Update();

    // A frame that ends late starts the next at once, rather than catching
    // up with frames run back to back.
    now = absl::Now();
    ++stats->frames;
    stats->frame_time += now - frame_start;
    if (next_frame > now) {
      gb::PreciseSleepFor(next_frame - now);
    } else {
      next_frame = now;
    }
  }
  stats->time += absl::Now() - start_time;
  stats->cycles += ozzy->GetProcessor()->GetCycles() - start_cycles;
  return power_request;
}

// Waits until the user quits, with the Ozzy off, dropping the keys typed
// meanwhile.
void WaitForQuit(Console& console) {
  while (!console.IsQuitRequested()) {
    console.TakeKeys();
    gb::PreciseSleepFor(kFrameTime);
  }
}

// Writes `stats` to the standard output.
void WriteStats(const RunStats& stats) {
  if (stats.frames == 0) {
    absl::PrintF("The Ozzy quit before its first frame.\n");
    return;
  }
  const double secs = absl::ToDoubleSeconds(stats.time);
  absl::PrintF("%d frames in %.2f s: %.2f frames a second\n", stats.frames,
               secs, stats.frames / secs);
  absl::PrintF("Each frame took %.3f ms on average to run and draw\n",
               absl::ToDoubleMilliseconds(stats.frame_time / stats.frames));
  absl::PrintF("%d cycles: %.0f cycles a second\n", stats.cycles,
               stats.cycles / secs);
}

bool Run(gb::ValidatedContext& context,
         const gb::CliCommandLine& command_line) {
  gb::FileSystem* file_system = context.GetPtr<gb::FileSystem>();
  const gb::CliWriter writer = *context.GetPtr<gb::CliWriter>();
  std::optional<OzzySettings> settings =
      ReadSettings(writer, *file_system, command_line);
  if (!settings.has_value()) {
    return false;
  }

  // The console shows the screen's cells as text, so it never needs pixels.
  if (settings->screen.has_value()) {
    settings->screen->draw_pixels = false;
  }
  std::optional<absl::Duration> max_speed_run_time;
  if (command_line.Get(kMaxSpeed)) {
    max_speed_run_time = absl::Seconds(kMaxSpeedRunCycles) / settings->clock_hz;
  }

  // Each Ozzy is created in the command's context, which has the file system
  // for the drives' images.
  //
  // Programs see the date and time where the user is, and files are stamped
  // with it, as the context has no clock of its own (see kConstraintHostClock).
  // It is the host's time, so when the Ozzy can't keep up with its clock rate
  // (or runs at max speed), it drifts from the timer's ticks and milliseconds.
  std::unique_ptr<OzzyHost> host =
      OzzyHost::Create(*std::move(settings), context);
  if (host == nullptr) {
    writer(gb::CliText::kError, "Failed to create the Ozzy's host");
    return false;
  }

  // The console shows every power on, as its mode comes from the settings.
  const ConsoleMode mode = GetOzzyConsoleMode(host->GetSettings());
  absl::StatusOr<std::unique_ptr<Console>> console = Console::Create(mode);
  if (!console.ok()) {
    writer(gb::CliText::kError,
           absl::StrCat("Failed to take over the console: ",
                        console.status().message()));
    return false;
  }

  // Each power on creates the Ozzy from its settings, and each power off
  // destroys it, which writes back its drives.
  RunStats stats;
  while (true) {
    if (const absl::Status status = host->PowerOn(); !status.ok()) {
      console->reset();
      writer(gb::CliText::kError,
             absl::StrCat("Failed to create the Ozzy: ", status.message()));
      return false;
    }
    const std::optional<PowerRequest> power_request =
        RunOzzy(*host, **console, max_speed_run_time, &stats);
    host->PowerOff();
    if (!power_request.has_value()) {
      break;
    }
    if (power_request->command == PowerCommand::kShutDown) {
      ShowOzzyShutDown(**console, mode, power_request->exit_code);
      WaitForQuit(**console);
      break;
    }
  }

  // The console is given back first, so the stats are in its own buffer.
  console->reset();
  if (command_line.Get(kStats)) {
    WriteStats(stats);
  }
  return true;
}

}  // namespace
}  // namespace oz3
int main(int argc, char* argv[]) {
  const std::unique_ptr<gb::FileSystem> file_system = gb::CreateCliFileSystem();
  gb::Context context;
  context.SetPtr<gb::FileSystem>(file_system.get());
  return gb::RunCommandLine(&context, &oz3::kProgram,
                            gb::GetMainArgs(argc, argv));
}
