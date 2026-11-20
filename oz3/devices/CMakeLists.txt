## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_devices_SOURCE
  disk_drive.cc disk_drive.h
  disk_drive_command.cc disk_drive_command.h
  keyboard.cc keyboard.h
  power_control.cc power_control.h
  screen.cc screen.h
  screen_characters.cc screen_characters.h
  teletype.cc teletype.h
  timer.cc timer.h
)
set(oz3_devices_DEPS
  absl::any_invocable
  absl::civil_time
  absl::int128
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::strings
  absl::time
  gb_base
  gb_container
  oz3_core
  oz3_disk
)
set(oz3_devices_LIBS
  absl::check
  absl::log
)
set(oz3_devices_TEST_SOURCE
  device_program_test.h
  disk_drive_test.cc
  keyboard_test.cc
  power_control_test.cc
  screen_characters_test.cc
  screen_test.cc
  teletype_test.cc
  timer_test.cc
)
set(oz3_devices_TEST_DEPS
  gb_file
  gb_parse
  oz3_disk_testing
  oz3_core_testing
  oz3_instruction_sets
  oz3_tools
)
set(oz3_devices_TEST_LIBS
  absl::check
)

gb_add_library(oz3_devices)
