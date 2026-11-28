## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_devices_disk_drive_SOURCE
  disk_drive.cc disk_drive.h
  disk_drive_command.cc disk_drive_command.h
)
set(oz3_devices_disk_drive_DEPS
  absl::int128
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::strings
  absl::time
  gb_base
  oz3_core
  oz3_disk
)
set(oz3_devices_disk_drive_LIBS
  absl::check
  absl::log
)
set(oz3_devices_disk_drive_TEST_SOURCE
  disk_drive_test.cc
)
set(oz3_devices_disk_drive_TEST_DEPS
  absl::civil_time
  absl::function_ref
  gb_file
  oz3_devices_testing
  oz3_disk_testing
)
set(oz3_devices_disk_drive_TEST_LIBS
  absl::check
)

gb_add_library(oz3_devices_disk_drive)
