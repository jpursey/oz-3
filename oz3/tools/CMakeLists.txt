## Copyright (c) 2024 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_tools_SOURCE
  disk_tool.cc disk_tool.h
)
set(oz3_tools_DEPS
  absl::algorithm_container
  absl::function_ref
  absl::status
  absl::statusor
  absl::str_format
  absl::strings
  absl::time
  gb_base
  gb_file
  oz3_disk
  oz3_program
)

set(oz3_tools_TEST_SOURCE
  disk_tool_test.cc
)
set(oz3_tools_TEST_DEPS
  oz3_disk_testing
)

gb_add_library(oz3_tools)
