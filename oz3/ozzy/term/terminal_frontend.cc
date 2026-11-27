// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/term/terminal_frontend.h"

#include <memory>
#include <optional>
#include <utility>

#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"

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

void TerminalFrontend::OnPowerOn(Ozzy* ozzy) {
  CHECK(ozzy != nullptr);
  CHECK(display_ == nullptr) << "An Ozzy is already shown";
  display_ = std::make_unique<ConsoleDisplay>(ozzy, console_.get());
}

void TerminalFrontend::OnPowerOff(std::optional<int> exit_code) {
  CHECK(display_ != nullptr) << "No Ozzy is shown";
  display_->Update();
  display_.reset();
  if (exit_code.has_value()) {
    ShowOzzyShutDown(*console_, *exit_code);
  }
}

//==============================================================================
// TerminalFrontendFactory
//==============================================================================

absl::StatusOr<std::unique_ptr<Frontend>> TerminalFrontendFactory::Create(
    const OzzySettings& settings, const gb::CliCommandLine& command_line) {
  absl::StatusOr<std::unique_ptr<Console>> console =
      Console::Create(GetOzzyConsoleMode(settings));
  if (!console.ok()) {
    return absl::Status(console.status().code(),
                        absl::StrCat("Failed to take over the console: ",
                                     console.status().message()));
  }
  return std::make_unique<TerminalFrontend>(*std::move(console));
}

}  // namespace oz3
