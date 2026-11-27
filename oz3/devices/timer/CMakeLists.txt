## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_devices_timer_SOURCE
  timer.cc timer.h
)
set(oz3_devices_timer_DEPS
  absl::civil_time
  absl::memory
  absl::status
  absl::statusor
  absl::strings
  gb_base
  oz3_core
)
set(oz3_devices_timer_TEST_SOURCE
  timer_test.cc
)
set(oz3_devices_timer_TEST_DEPS
  oz3_devices_testing
)

gb_add_library(oz3_devices_timer)
