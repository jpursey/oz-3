// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/app/ozzy_app.h"

#include <algorithm>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "absl/log/log.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/time/time.h"
#include "gb/cli/cli.h"
#include "gb/cli/cli_writer.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "gb/file/path.h"
#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_settings.h"

namespace oz3 {

namespace {

//==============================================================================
// Command line
//==============================================================================

const gb::CliFlag<bool> kStats = {
    .name = "stats",
    .help =
        "Writes how the Ozzy ran once it quits: the frames a second, the time "
        "each took on average to run the Ozzy and show it, and the cycles a "
        "second.",
};

const gb::CliArg<std::string_view> kConfigFile = {
    .name = "config file",
    .help =
        "The Ozzy to run (.ozzy). Without one, it runs the default Ozzy, "
        "which an empty config file describes.",
    .optional = true,
    .validators = {&gb::kCliFileExists},
};

// Returns the settings of the Ozzy `command_line` asks for, or the error for
// the user.
absl::StatusOr<OzzySettings> ReadSettings(
    gb::FileSystem& file_system, const gb::CliCommandLine& command_line) {
  const std::optional<std::string_view> config_file =
      command_line.Get(kConfigFile);
  if (!config_file.has_value()) {
    return OzzySettings();
  }

  // The validator of the config file checks that its path normalizes.
  const std::string path = gb::NormalizePath(*config_file);
  std::string text;
  if (!file_system.ReadFile(path, &text)) {
    return absl::NotFoundError(absl::StrCat("Failed to read ", path));
  }
  return ReadOzzySettings(std::move(text), path);
}

// Returns `stats` as --stats writes them, ending with a newline.
std::string FormatStats(const OzzyAppStats& stats) {
  if (stats.frames == 0) {
    return "The Ozzy quit before its first frame.\n";
  }
  const double secs = absl::ToDoubleSeconds(stats.time);
  return absl::StrFormat(
      "%d frames in %.2f s: %.2f frames a second\n"
      "Each frame took %.3f ms on average to run and show\n"
      "%d cycles: %.0f cycles a second\n",
      stats.frames, secs, stats.frames / secs,
      absl::ToDoubleMilliseconds(stats.frame_time / stats.frames), stats.cycles,
      stats.cycles / secs);
}

}  // namespace

//==============================================================================
// OzzyApp
//==============================================================================

bool OzzyApp::Init(const std::vector<std::string_view>& args) {
  context_ = Contract(gb::Game::Context());
  if (!context_.IsValid() || !context_.SetPtr<OzzyApp>(this)) {
    return false;
  }
  clock_ = context_.GetPtr<gb::Clock>();

  // The command line is the frontend's name, help, and flags, and the app's
  // own flags and arguments. It is a local, which keeps the arrays of its
  // braced lists while it runs.
  const FrontendFactory* factory = context_.GetPtr<FrontendFactory>();
  const gb::CliCommand command = {
      .name = factory->GetProgramName(),
      .help = factory->GetProgramHelp(),
      .flags = {&kStats},
      .added_flags = factory->GetFlags(),
      .args = {&kConfigFile},
      .constraints = {&gb::kConstraintCliFileSystem, &kConstraintOzzyApp},
      .run = RunCommand,
  };
  context_.SetValue<int>(kKeyExitCode,
                         gb::RunCommandLine(context_, &command, args));

  // The command started the app if it created the frontend without failing,
  // which help or a mistake doesn't.
  return frontend_ != nullptr && error_.empty();
}

bool OzzyApp::Update(absl::Duration delta_time) {
  if (frontend_->IsQuitRequested()) {
    return false;
  }
  HandleKeys();
  if (!host_->IsOn()) {
    frontend_->Update();
    return true;
  }

  const absl::Time frame_start = clock_->Now();
  const OzzyHost::RunResult result =
      host_->Run(std::min(delta_time, kMaxRunTime));
  ++stats_.frames;
  stats_.time += delta_time;
  stats_.cycles += result.cycles;
  if (result.power_request.has_value() &&
      !DoPowerRequest(*result.power_request)) {
    return false;
  }
  frontend_->Update();
  stats_.frame_time += clock_->Now() - frame_start;
  return true;
}

void OzzyApp::CleanUp() {
  if (!context_.IsValid()) {
    return;
  }
  if (host_ != nullptr && host_->IsOn()) {
    PowerOff(std::nullopt);
  }

  // The frontend gives the console back first, so the results are in the
  // console's own buffer.
  frontend_.reset();
  host_.reset();
  WriteResults();
  LOG_IF(ERROR, !context_.Complete()) << "OzzyApp's contract was violated";
}

bool OzzyApp::RunCommand(gb::ValidatedContext& context,
                         const gb::CliCommandLine& command_line) {
  OzzyApp* const app = context.GetPtr<OzzyApp>();
  CHECK(app != nullptr);
  return app->Start(command_line);
}

bool OzzyApp::Start(const gb::CliCommandLine& command_line) {
  absl::StatusOr<OzzySettings> settings =
      ReadSettings(*context_.GetPtr<gb::FileSystem>(), command_line);
  if (!settings.ok()) {
    return Fail(settings.status().message());
  }

  // The frontend is created first, as it says whether the screens draw their
  // pixels. It shows every power on, as the settings are the same for each.
  absl::StatusOr<std::unique_ptr<Frontend>> frontend =
      context_.GetPtr<FrontendFactory>()->Create(*settings, command_line);
  if (!frontend.ok()) {
    return Fail(frontend.status().message());
  }
  frontend_ = *std::move(frontend);
  if (settings->screen.has_value()) {
    settings->screen->draw_pixels = frontend_->ShowsPixels();
  }

  // Each Ozzy is created in the app's context, which has the file system for
  // the drives' images, and the host's power request callback while it lives.
  //
  // Programs see the date and time where the user is, and files are stamped
  // with it, as the context has no clock for them (see kConstraintHostClock).
  // It is the host's time, so when the Ozzy can't keep up with its clock
  // rate, it drifts from the timer's ticks and milliseconds.
  host_ = OzzyHost::Create(*std::move(settings), context_);
  if (host_ == nullptr) {
    return Fail("Failed to create the Ozzy's host");
  }
  write_stats_ = command_line.Get(kStats);
  return PowerOn();
}

bool OzzyApp::Fail(std::string_view message) {
  error_ = message;
  context_.SetValue<int>(kKeyExitCode, 1);
  return false;
}

bool OzzyApp::PowerOn() {
  if (const absl::Status status = host_->PowerOn(); !status.ok()) {
    return Fail(absl::StrCat("Failed to create the Ozzy: ", status.message()));
  }
  frontend_->Attach(host_->GetOzzy());
  return true;
}

void OzzyApp::PowerOff(std::optional<int> exit_code) {
  frontend_->Detach(exit_code);
  host_->PowerOff();
}

bool OzzyApp::DoPowerRequest(const PowerRequest& request) {
  switch (request.command) {
    case PowerCommand::kShutDown:
      // The app stays open, with the Ozzy off, until the user quits.
      PowerOff(request.exit_code);
      return true;
    case PowerCommand::kRestart:
      PowerOff(std::nullopt);
      return PowerOn();
  }
  return true;
}

void OzzyApp::HandleKeys() {
  // Keys are taken while the Ozzy is off or has no keyboard too, so they
  // don't pile up.
  const FrontendKeys keys = frontend_->TakeKeys();
  Keyboard* const keyboard =
      host_->IsOn() ? host_->GetOzzy()->GetKeyboard() : nullptr;
  if (keyboard == nullptr) {
    return;
  }
  for (const uint16_t key : keys.typed) {
    keyboard->TypeKey(key);
  }
  for (const KeyChange& change : keys.changes) {
    keyboard->SetKeyState(change.key, change.held);
  }
}

void OzzyApp::WriteResults() {
  const gb::CliWriter& writer = *context_.GetPtr<gb::CliWriter>();
  if (!error_.empty()) {
    writer(gb::CliText::kError, error_);
    return;
  }

  // The stats were asked for, so they are written as help is.
  if (write_stats_) {
    writer(gb::CliText::kHelp, FormatStats(stats_));
  }
}

}  // namespace oz3
