## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_ozzy_SOURCE
  machine_table.h
  ozzy.cc ozzy.h
  ozzy_settings.cc ozzy_settings.h
)
set(oz3_ozzy_DEPS
  absl::algorithm_container
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::strings
  absl::time
  gb_config
  oz3_core
  oz3_devices
  oz3_instruction_sets
)
set(oz3_ozzy_LIBS
  absl::check
)
set(oz3_ozzy_TEST_SOURCE
  ozzy_settings_test.cc
  ozzy_test.cc
)

gb_add_library(oz3_ozzy)
