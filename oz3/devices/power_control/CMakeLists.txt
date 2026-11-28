## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_devices_power_control_SOURCE
  power_control.cc power_control.h
)
set(oz3_devices_power_control_DEPS
  absl::any_invocable
  absl::memory
  gb_base
  oz3_core
)
set(oz3_devices_power_control_LIBS
  absl::check
)
set(oz3_devices_power_control_TEST_SOURCE
  power_control_test.cc
)
set(oz3_devices_power_control_TEST_DEPS
  oz3_devices_testing
)

gb_add_library(oz3_devices_power_control)
