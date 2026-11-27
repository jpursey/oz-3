// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_APP_FRONTEND_H_
#define OZ3_OZZY_APP_FRONTEND_H_

#include <memory>
#include <optional>

#include "absl/status/statusor.h"
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
// The app brackets each Ozzy's life with OnPowerOn and OnPowerOff, so a
// frontend that holds the Ozzy's devices lets go of them before the Ozzy is
// destroyed.
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

  // Shows what changed since the last call, such as in the Ozzy shown. The app
  // must call it every frame, whether or not an Ozzy is on, and while one is,
  // between its runs.
  virtual void Update() = 0;

  //----------------------------------------------------------------------------
  // Ozzy events
  //----------------------------------------------------------------------------

  // The app calls this when an Ozzy powers on, as `ozzy`, which the frontend
  // shows until OnPowerOff. It must only be called while no Ozzy is shown, and
  // `ozzy` must outlive the OnPowerOff that follows.
  virtual void OnPowerOn(Ozzy* ozzy) = 0;

  // The app calls this when the Ozzy shown is about to power off. The frontend
  // shows what changed since the last Update, such as what a program wrote
  // before it shut down, then lets go of the Ozzy. With `exit_code`, the
  // program shut it down, which is shown until the next OnPowerOn. Without
  // one, it is about to power on again, or the app is quitting. It must only
  // be called while an Ozzy is shown, before that Ozzy is destroyed.
  virtual void OnPowerOff(std::optional<int> exit_code) = 0;

 protected:
  Frontend() = default;
};

//==============================================================================
// FrontendFactory
//==============================================================================

// Creates an app's frontend.
class FrontendFactory {
 public:
  FrontendFactory(const FrontendFactory&) = delete;
  FrontendFactory& operator=(const FrontendFactory&) = delete;
  virtual ~FrontendFactory() = default;

  // Creates the frontend for an Ozzy with `settings`, reading the frontend's
  // flags from `command_line`. Returns an error if it can't (such as no
  // console to take over), whose message says so for the user.
  virtual absl::StatusOr<std::unique_ptr<Frontend>> Create(
      const OzzySettings& settings, const gb::CliCommandLine& command_line) = 0;

 protected:
  FrontendFactory() = default;
};

}  // namespace oz3

#endif  // OZ3_OZZY_APP_FRONTEND_H_
