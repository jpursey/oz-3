## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_ozzy_app_SOURCE
  frontend.h
  frontend_keys.h
  ozzy_app.cc ozzy_app.h
  ozzy_host.cc ozzy_host.h
)
set(oz3_ozzy_app_DEPS
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::str_format
  absl::strings
  absl::time
  gb_base
  gb_cli
  gb_file
  gb_game
  oz3_core
  oz3_devices
  oz3_ozzy_machine
)
set(oz3_ozzy_app_LIBS
  absl::check
  absl::log
)
set(oz3_ozzy_app_TEST_SOURCE
  fake_frontend.h
  ozzy_app_test.cc
  ozzy_host_test.cc
)
set(oz3_ozzy_app_TEST_DEPS
  absl::algorithm_container
  absl::any_invocable
  gb_file
  oz3_disk_testing
  oz3_ozzy_testing
)

gb_add_library(oz3_ozzy_app)
