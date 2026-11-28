## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_devices_screen_SOURCE
  screen.cc screen.h
  screen_characters.cc screen_characters.h
)
set(oz3_devices_screen_DEPS
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::strings
  gb_base
  oz3_core
)
set(oz3_devices_screen_LIBS
  absl::check
)
set(oz3_devices_screen_TEST_SOURCE
  screen_characters_test.cc
  screen_test.cc
)
set(oz3_devices_screen_TEST_DEPS
  oz3_devices_testing
)

gb_add_library(oz3_devices_screen)
