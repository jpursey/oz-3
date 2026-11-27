// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/term/terminal_frontend.h"

#include <memory>
#include <optional>
#include <string_view>
#include <utility>

#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/cli/cli.h"
#include "gb/cli/cli_command_line.h"

namespace oz3 {

//==============================================================================
// TerminalFrontend
//==============================================================================

TerminalFrontend::TerminalFrontend(std::unique_ptr<Console> console)
    : console_(std::move(console)) {
  CHECK(console_ != nullptr);
}

FrontendKeys TerminalFrontend::TakeKeys() { return console_->TakeKeys(); }

bool TerminalFrontend::IsQuitRequested() const {
  return console_->IsQuitRequested();
}

void TerminalFrontend::Update() {
  if (display_ != nullptr) {
    display_->Update();
  }
}

void TerminalFrontend::Attach(Ozzy* ozzy) {
  CHECK(ozzy != nullptr);
  CHECK(display_ == nullptr) << "An Ozzy is already attached";
  display_ = std::make_unique<ConsoleDisplay>(ozzy, console_.get());
}

void TerminalFrontend::Detach(std::optional<int> exit_code) {
  CHECK(display_ != nullptr) << "No Ozzy is attached";
  display_->Update();
  display_.reset();
  if (exit_code.has_value()) {
    ShowOzzyShutDown(*console_, *exit_code);
  }
}

//==============================================================================
// TerminalFrontendFactory
//==============================================================================

absl::StatusOr<std::unique_ptr<Frontend>> TerminalFrontendFactory::Create() {
  absl::StatusOr<std::unique_ptr<Console>> console = Console::Create(mode_);
  if (!console.ok()) {
    return absl::Status(console.status().code(),
                        absl::StrCat("Failed to take over the console: ",
                                     console.status().message()));
  }
  return std::make_unique<TerminalFrontend>(*std::move(console));
}

//==============================================================================
// TerminalFrontendType
//==============================================================================

std::string_view TerminalFrontendType::GetProgramName() const {
  return "ozzyterm";
}

std::string_view TerminalFrontendType::GetProgramHelp() const {
  return "Runs an Ozzy computer in the console, until Ctrl+Alt+Q or "
         "Ctrl+Break, or the console's window is closed. A program on it can "
         "restart it, or shut it down, which leaves it off until it quits. Its "
         "drives have the disks the config gives them, and write back what "
         "they hold when it quits or the Ozzy powers off. Paths are written "
         "the operating system's way, absolute or relative to the current "
         "directory, and a config's disk images relative to its folder.";
}

absl::Span<const gb::CliAnyFlag> TerminalFrontendType::GetFlags() const {
  return {};
}

absl::StatusOr<std::unique_ptr<FrontendFactory>>
TerminalFrontendType::Configure(OzzySettings* settings,
                                const gb::CliCommandLine& command_line) {
  if (settings->screen.has_value()) {
    settings->screen->draw_pixels = false;
  }
  return std::make_unique<TerminalFrontendFactory>(
      GetOzzyConsoleMode(*settings));
}

}  // namespace oz3
