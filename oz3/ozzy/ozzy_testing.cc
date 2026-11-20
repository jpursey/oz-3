// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/ozzy_testing.h"

#include "oz3/devices/power_control.h"

namespace oz3 {

gb::Context WithPowerCallback(gb::Context context) {
  context.SetNew<PowerRequestCallback>([](PowerRequest) {});
  return context;
}

}  // namespace oz3
