// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/term/console_frontend.h"

#include <memory>
#include <optional>
#include <utility>

#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"

namespace oz3 {

//==============================================================================
// ConsoleFrontend
//==============================================================================

ConsoleFrontend::ConsoleFrontend(std::unique_ptr<Console> console,
                                 ConsoleMode mode)
    : console_(std::move(console)), mode_(mode) {
  CHECK(console_ != nullptr);
}

FrontendKeys ConsoleFrontend::TakeKeys() { return console_->TakeKeys(); }

bool ConsoleFrontend::IsQuitRequested() const {
  return console_->IsQuitRequested();
}

void ConsoleFrontend::PowerOn(Ozzy* ozzy) {
  CHECK(display_ == nullptr) << "An Ozzy is already shown";
  display_ = std::make_unique<ConsoleDisplay>(*ozzy, *console_);
}

void ConsoleFrontend::Update() {
  if (display_ != nullptr) {
    display_->Update();
  }
}

void ConsoleFrontend::PowerOff(std::optional<int> exit_code) {
  display_.reset();
  if (exit_code.has_value()) {
    ShowOzzyShutDown(*console_, mode_, *exit_code);
  }
}

//==============================================================================
// ConsoleFrontendFactory
//==============================================================================

absl::StatusOr<std::unique_ptr<Frontend>> ConsoleFrontendFactory::Create(
    const OzzySettings& settings, const gb::CliCommandLine& command_line) {
  const ConsoleMode mode = GetOzzyConsoleMode(settings);
  absl::StatusOr<std::unique_ptr<Console>> console = Console::Create(mode);
  if (!console.ok()) {
    return absl::Status(console.status().code(),
                        absl::StrCat("Failed to take over the console: ",
                                     console.status().message()));
  }
  return std::make_unique<ConsoleFrontend>(*std::move(console), mode);
}

}  // namespace oz3
