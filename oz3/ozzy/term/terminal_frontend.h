// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_TERM_TERMINAL_FRONTEND_H_
#define OZ3_OZZY_TERM_TERMINAL_FRONTEND_H_

#include <memory>
#include <optional>
#include <string_view>

#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "gb/cli/cli.h"
#include "gb/cli/cli_command_line.h"
#include "oz3/ozzy/app/frontend.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_settings.h"
#include "oz3/ozzy/term/console.h"
#include "oz3/ozzy/term/console_display.h"

namespace oz3 {

//==============================================================================
// TerminalFrontend
//==============================================================================

// Presents an app in a Console, as ozzyterm does: the Ozzy attached is shown
// with a ConsoleDisplay, and the keys and quit are the console's.
class TerminalFrontend final : public Frontend {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Presents an app in `console`, whose mode must be the one
  // GetOzzyConsoleMode returns for the Ozzys it shows. Destroying the frontend
  // gives the console back.
  explicit TerminalFrontend(std::unique_ptr<Console> console);

  //----------------------------------------------------------------------------
  // Frontend interface
  //----------------------------------------------------------------------------

  bool ShowsPixels() const override;
  FrontendKeys TakeKeys() override;
  bool IsQuitRequested() const override;
  void Update() override;
  void Attach(Ozzy* ozzy) override;
  void Detach(std::optional<int> exit_code) override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  const std::unique_ptr<Console> console_;

  // The display of the Ozzy shown, or null while there is none.
  std::unique_ptr<ConsoleDisplay> display_;
};

//==============================================================================
// TerminalFrontendFactory
//==============================================================================

// Creates a TerminalFrontend, taking over the host's console (see
// Console::Create).
class TerminalFrontendFactory final : public FrontendFactory {
 public:
  //----------------------------------------------------------------------------
  // FrontendFactory interface
  //----------------------------------------------------------------------------

  // The app is `ozzy`, and the console has no flags of its own.
  std::string_view GetProgramName() const override;
  std::string_view GetProgramHelp() const override;
  absl::Span<const gb::CliAnyFlag> GetFlags() const override;

  // Takes over the console in the mode GetOzzyConsoleMode returns for
  // `settings`, or returns why it can't.
  absl::StatusOr<std::unique_ptr<Frontend>> Create(
      const OzzySettings& settings,
      const gb::CliCommandLine& command_line) override;
};

}  // namespace oz3

#endif  // OZ3_OZZY_TERM_TERMINAL_FRONTEND_H_
