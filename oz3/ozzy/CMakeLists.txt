## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_ozzy_SOURCE
  machine_settings.cc machine_settings.h
)
set(oz3_ozzy_DEPS
  absl::algorithm_container
  absl::span
  absl::status
  absl::statusor
  absl::strings
  gb_config
  oz3_core
  oz3_devices
)
set(oz3_ozzy_TEST_SOURCE
  machine_settings_test.cc
)

gb_add_library(oz3_ozzy)
