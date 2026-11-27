// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_APP_OZZY_HOST_H_
#define OZ3_OZZY_APP_OZZY_HOST_H_

#include <memory>
#include <optional>

#include "absl/status/status.h"
#include "absl/time/time.h"
#include "gb/base/validated_context.h"
#include "oz3/core/core_types.h"
#include "oz3/devices/power_control.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_settings.h"

namespace oz3 {

//==============================================================================
// OzzyHost
//==============================================================================

// The host's side of an Ozzy: its settings, the context it is created in, and
// whether it is on. The Ozzy exists only while it is on.
class OzzyHost final {
 public:
  //----------------------------------------------------------------------------
  // Contract
  //----------------------------------------------------------------------------

  // The host puts its power request callback in its context while it lives,
  // replacing any already there, for each Ozzy it creates (see
  // PowerControl::kConstraintPowerRequestCallback).
  static GB_CONTEXT_CONSTRAINT(kConstraintPowerRequestCallback, kScoped,
                               PowerRequestCallback);

  // Each Ozzy is created in the host's context, so it holds the rest of what
  // the Ozzy needs from the host (see Ozzy::Contract), such as the file system.
  // It must outlive the host unless the contract owns it.
  using Contract = gb::ContextContract<kConstraintPowerRequestCallback>;

  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // What a Run did.
  struct RunResult {
    // The cycles run.
    Cycles cycles = 0;

    // The first request a program made on the power port during the run, if
    // any. Later requests in the same run are dropped.
    std::optional<PowerRequest> power_request;
  };

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a host for an Ozzy with `settings`, which is off, or returns null
  // if the contract is invalid. The settings are only checked at PowerOn.
  static std::unique_ptr<OzzyHost> Create(OzzySettings settings,
                                          Contract contract);

  OzzyHost(const OzzyHost&) = delete;
  OzzyHost& operator=(const OzzyHost&) = delete;

  // Powers the Ozzy off, if it is on.
  ~OzzyHost();

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the Ozzy, or null while it is off. It is destroyed at PowerOff.
  Ozzy* GetOzzy() { return ozzy_.get(); }

  bool IsOn() const { return ozzy_ != nullptr; }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Powers the Ozzy on, creating it from the settings, if it is off. Returns
  // the error if it can't be created (see Ozzy::Create), such as a context
  // without what it needs, and stays off.
  absl::Status PowerOn();

  // Powers the Ozzy off, if it is on, which destroys it, committing each drive
  // as at a clean shutdown. A restart is PowerOff then PowerOn, so RAM is
  // cleared and every device starts as at power on.
  void PowerOff();

  // Runs the Ozzy for `time` (see Ozzy::Run), which must be on. The Ozzy is
  // still on after a program's power request: what a request does is up to
  // the caller. Only the requests made during this run are returned, so a
  // caller that runs the Ozzy itself, rather than through Run, never sees
  // them.
  RunResult Run(absl::Duration time);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  OzzyHost(OzzySettings settings, gb::ValidatedContext context);

  // Called by the power control, from inside Ozzy::Run.
  void OnPowerRequest(PowerRequest request);

  const OzzySettings settings_;

  // Each Ozzy is created in this, so it outlives each Ozzy, as the drives write
  // to their images as it is destroyed.
  gb::ValidatedContext context_;

  // The first power request of the current Run.
  std::optional<PowerRequest> power_request_;

  std::unique_ptr<Ozzy> ozzy_;
};

}  // namespace oz3

#endif  // OZ3_OZZY_APP_OZZY_HOST_H_
