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

#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/time/clock.h"
#include "absl/time/time.h"
#include "gb/base/context.h"
#include "gb/base/validated_context.h"
#include "gb/cli/cli.h"
#include "gb/cli/cli_command_line.h"
#include "gb/cli/cli_path.h"
#include "gb/cli/cli_writer.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "gb/file/path.h"
#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/console.h"
#include "oz3/ozzy/console_display.h"
#include "oz3/ozzy/ozzy.h"
#include "oz3/ozzy/ozzy_settings.h"

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
        "Ctrl+Break, or the console's window is closed. Paths are written the "
        "operating "
        "system's way, absolute or relative to the current directory.",
    .flags = {&kMaxSpeed},
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

// Runs `ozzy` in `console` until the user quits, each frame for the time since
// the last, or with `max_speed_run_time`, in runs of that time until the next
// frame.
void RunOzzy(Ozzy& ozzy, Console& console,
             std::optional<absl::Duration> max_speed_run_time) {
  Keyboard* const keyboard = ozzy.GetKeyboard();
  ConsoleDisplay display(ozzy, console);
  absl::Time last_time = absl::Now();
  absl::Time next_frame = last_time;
  while (!console.IsQuitRequested()) {
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
      while (now < next_frame) {
        ozzy.Run(*max_speed_run_time);
        now = absl::Now();
      }
    } else {
      ozzy.Run(std::min(now - last_time, kMaxRunTime));
    }
    last_time = now;
    display.Update();

    // A frame that ends late starts the next at once, rather than catching
    // up with frames run back to back.
    now = absl::Now();
    if (next_frame > now) {
      absl::SleepFor(next_frame - now);
    } else {
      next_frame = now;
    }
  }
}

bool Run(gb::ValidatedContext& context,
         const gb::CliCommandLine& command_line) {
  gb::FileSystem* file_system = context.GetPtr<gb::FileSystem>();
  const gb::CliWriter writer = *context.GetPtr<gb::CliWriter>();
  const std::optional<OzzySettings> settings =
      ReadSettings(writer, *file_system, command_line);
  if (!settings.has_value()) {
    return false;
  }
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy = Ozzy::Create(*settings);
  if (!ozzy.ok()) {
    writer(gb::CliText::kError, absl::StrCat("Failed to create the Ozzy: ",
                                             ozzy.status().message()));
    return false;
  }
  absl::StatusOr<std::unique_ptr<Console>> console =
      Console::Create(GetOzzyConsoleMode(**ozzy));
  if (!console.ok()) {
    writer(gb::CliText::kError,
           absl::StrCat("Failed to take over the console: ",
                        console.status().message()));
    return false;
  }
  std::optional<absl::Duration> max_speed_run_time;
  if (command_line.Get(kMaxSpeed)) {
    max_speed_run_time = absl::Seconds(kMaxSpeedRunCycles) / settings->clock_hz;
  }
  RunOzzy(**ozzy, **console, max_speed_run_time);
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
