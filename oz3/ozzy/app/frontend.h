// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_APP_FRONTEND_H_
#define OZ3_OZZY_APP_FRONTEND_H_

#include <memory>
#include <optional>

#include "absl/status/statusor.h"
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

// Creates an app's frontend.
class FrontendFactory {
 public:
  FrontendFactory(const FrontendFactory&) = delete;
  FrontendFactory& operator=(const FrontendFactory&) = delete;
  virtual ~FrontendFactory() = default;

  // Creates the frontend for the Ozzys with `settings`. Returns an error if it
  // can't (such as no console to take over), whose message says so for the
  // user.
  virtual absl::StatusOr<std::unique_ptr<Frontend>> Create(
      const OzzySettings& settings) = 0;

 protected:
  FrontendFactory() = default;
};

}  // namespace oz3

#endif  // OZ3_OZZY_APP_FRONTEND_H_
