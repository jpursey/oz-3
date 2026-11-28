## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_cli_disk_SOURCE
  disk_tool.cc disk_tool.h
  oz3dsk.cc oz3dsk.h
)
set(oz3_cli_disk_DEPS
  absl::algorithm_container
  absl::function_ref
  absl::status
  absl::statusor
  absl::str_format
  absl::strings
  absl::time
  gb_base
  gb_cli
  gb_file
  oz3_disk
  oz3_program
)

set(oz3_cli_disk_TEST_SOURCE
  disk_tool_test.cc
  oz3dsk_test.cc
)
set(oz3_cli_disk_TEST_DEPS
  oz3_disk_testing
)

gb_add_library(oz3_cli_disk)
