## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_devices_SOURCE
  keyboard.cc keyboard.h
  screen.cc screen.h
  teletype.cc teletype.h
)
set(oz3_devices_DEPS
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::strings
  gb_container
  oz3_core
)
set(oz3_devices_LIBS
  absl::check
)
set(oz3_devices_TEST_SOURCE
  device_program_test.h
  keyboard_test.cc
  screen_test.cc
  teletype_test.cc
)
set(oz3_devices_TEST_DEPS
  gb_parse
  oz3_core_testing
  oz3_instruction_sets
  oz3_tools
)
set(oz3_devices_TEST_LIBS
  absl::check
)

gb_add_library(oz3_devices)
