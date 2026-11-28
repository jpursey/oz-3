## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

# Test support: DeviceProgramTest, which runs programs against devices, for
# every device's tests.
set(oz3_devices_testing_SOURCE
  device_program_test.cc device_program_test.h
)
set(oz3_devices_testing_DEPS
  absl::civil_time
  absl::span
  gb_base
  gb_parse
  gtest
  oz3_core
  oz3_core_testing
  oz3_instructions
  oz3_tools
)
set(oz3_devices_testing_LIBS
  absl::check
)

gb_add_library(oz3_devices_testing)

add_subdirectory(disk_drive)
add_subdirectory(keyboard)
add_subdirectory(power_control)
add_subdirectory(screen)
add_subdirectory(teletype)
add_subdirectory(timer)
