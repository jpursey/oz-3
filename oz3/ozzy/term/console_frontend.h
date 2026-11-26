// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_TERM_CONSOLE_FRONTEND_H_
#define OZ3_OZZY_TERM_CONSOLE_FRONTEND_H_

#include <memory>
#include <optional>

#include "absl/status/statusor.h"
#include "gb/cli/cli_command_line.h"
#include "oz3/ozzy/app/frontend.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_settings.h"
#include "oz3/ozzy/term/console.h"
#include "oz3/ozzy/term/console_display.h"

namespace oz3 {

//==============================================================================
// ConsoleFrontend
//==============================================================================

// Presents an app in a Console: each Ozzy is shown with a ConsoleDisplay, and
// the keys and quit are the console's.
class ConsoleFrontend final : public Frontend {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Presents an app in `console`, whose mode is the one GetOzzyConsoleMode
  // returns for the Ozzys it shows. Destroying the frontend gives the console
  // back.
  explicit ConsoleFrontend(std::unique_ptr<Console> console);

  //----------------------------------------------------------------------------
  // Frontend interface
  //----------------------------------------------------------------------------

  FrontendKeys TakeKeys() override;
  bool IsQuitRequested() const override;

  void PowerOn(Ozzy* ozzy) override;
  void Update() override;

  // With `exit_code`, shows that the Ozzy shut down (see ShowOzzyShutDown).
  void PowerOff(std::optional<int> exit_code) override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  const std::unique_ptr<Console> console_;

  // The display of the Ozzy shown, or null while there is none.
  std::unique_ptr<ConsoleDisplay> display_;
};

//==============================================================================
// ConsoleFrontendFactory
//==============================================================================

// Creates a ConsoleFrontend, taking over the host's console (see
// Console::Create).
class ConsoleFrontendFactory final : public FrontendFactory {
 public:
  //----------------------------------------------------------------------------
  // FrontendFactory interface
  //----------------------------------------------------------------------------

  // Takes over the console in the mode GetOzzyConsoleMode returns for
  // `settings`, or returns why it can't. The console has no flags of its own.
  absl::StatusOr<std::unique_ptr<Frontend>> Create(
      const OzzySettings& settings,
      const gb::CliCommandLine& command_line) override;
};

}  // namespace oz3

#endif  // OZ3_OZZY_TERM_CONSOLE_FRONTEND_H_
