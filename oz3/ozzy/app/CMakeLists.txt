## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_ozzy_app_SOURCE
  frontend.h
  frontend_keys.h
  ozzy_host.cc ozzy_host.h
)
set(oz3_ozzy_app_DEPS
  absl::memory
  absl::status
  absl::statusor
  absl::time
  gb_base
  gb_cli
  oz3_core
  oz3_devices
  oz3_ozzy_machine
)
set(oz3_ozzy_app_LIBS
  absl::check
)
set(oz3_ozzy_app_TEST_SOURCE
  ozzy_host_test.cc
)
set(oz3_ozzy_app_TEST_DEPS
  gb_file
  oz3_ozzy_testing
)

gb_add_library(oz3_ozzy_app)
