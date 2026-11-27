// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_APP_FRONTEND_H_
#define OZ3_OZZY_APP_FRONTEND_H_

#include <memory>
#include <optional>
#include <string_view>

#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "gb/cli/cli.h"
#include "gb/cli/cli_command_line.h"
#include "oz3/ozzy/app/frontend_keys.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_settings.h"

namespace oz3 {

//==============================================================================
// Frontend
//==============================================================================

// How an app is presented: what the user sees of the Ozzy, and the keys and
// quit the user gives it. It decides nothing about the Ozzy, which is the
// app's.
//
// The app attaches the Ozzy to show, and detaches it before the Ozzy is
// destroyed, so a frontend that holds the Ozzy's devices lets go of them in
// time.
class Frontend {
 public:
  Frontend(const Frontend&) = delete;
  Frontend& operator=(const Frontend&) = delete;
  virtual ~Frontend() = default;

  //----------------------------------------------------------------------------
  // Input
  //----------------------------------------------------------------------------

  // Returns the keys since the last call. It doesn't wait for keys.
  virtual FrontendKeys TakeKeys() = 0;

  // Returns whether the user asked to quit.
  virtual bool IsQuitRequested() const = 0;

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Shows what changed since the last call, such as the attached Ozzy. The app
  // must call it every frame, whether or not an Ozzy is attached, and while
  // one is, between its runs.
  virtual void Update() = 0;

  //----------------------------------------------------------------------------
  // Attached Ozzy
  //----------------------------------------------------------------------------

  // Shows `ozzy` until Detach. It must only be called while no Ozzy is
  // attached, and `ozzy` must stay alive until Detach returns.
  virtual void Attach(Ozzy* ozzy) = 0;

  // Shows what changed since the last Update, then stops showing the attached
  // Ozzy. With `exit_code`, it shows that the Ozzy shut down with it, until the
  // next Attach. It must only be called while an Ozzy is attached.
  virtual void Detach(std::optional<int> exit_code) = 0;

 protected:
  Frontend() = default;
};

//==============================================================================
// FrontendFactory
//==============================================================================

// Creates an app's frontend, as FrontendType::Configure set it up to.
class FrontendFactory {
 public:
  FrontendFactory(const FrontendFactory&) = delete;
  FrontendFactory& operator=(const FrontendFactory&) = delete;
  virtual ~FrontendFactory() = default;

  // Creates the frontend, taking over what it shows the Ozzy in (such as the
  // console). Returns an error if it can't (such as no console to take over),
  // whose message says so for the user.
  virtual absl::StatusOr<std::unique_ptr<Frontend>> Create() = 0;

 protected:
  FrontendFactory() = default;
};

//==============================================================================
// FrontendType
//==============================================================================

// How an app is presented: it gives the app the frontend's part of its command
// line, and sets up the frontend for the command line the user gave. An app's
// main puts one in the OzzyApp's context (see
// OzzyApp::kConstraintFrontendType).
class FrontendType {
 public:
  FrontendType(const FrontendType&) = delete;
  FrontendType& operator=(const FrontendType&) = delete;
  virtual ~FrontendType() = default;

  //----------------------------------------------------------------------------
  // Command line
  //----------------------------------------------------------------------------

  // Returns the app's name and help on its command line, which say how it is
  // presented ("Runs an Ozzy computer in the console, ...").
  virtual std::string_view GetProgramName() const = 0;
  virtual std::string_view GetProgramHelp() const = 0;

  // Returns the frontend's own flags, which the app adds to its command line
  // after its own. The array must stay as it is for the type's life, and each
  // flag is a definition with static storage, as gb_cli's are. Their names
  // must not be those of the app's flags.
  virtual absl::Span<const gb::CliAnyFlag> GetFlags() const = 0;

  //----------------------------------------------------------------------------
  // Configuration
  //----------------------------------------------------------------------------

  // Sets up the frontend for the Ozzys with `settings`, reading its flags from
  // `command_line`, and returns the factory that creates it. It changes the
  // settings the frontend decides, such as whether the screen draws its pixels
  // (OzzyScreenSettings::draw_pixels), which the Ozzys are then created with.
  // It takes nothing over, so the app can create the Ozzy before the frontend.
  // Returns an error if the frontend can't show these Ozzys, or its flags are
  // wrong, whose message says so for the user.
  virtual absl::StatusOr<std::unique_ptr<FrontendFactory>> Configure(
      OzzySettings* settings, const gb::CliCommandLine& command_line) = 0;

 protected:
  FrontendType() = default;
};

}  // namespace oz3

#endif  // OZ3_OZZY_APP_FRONTEND_H_
