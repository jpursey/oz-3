## Copyright (c) 2024 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_tools_SOURCE
  instruction_set_parser.cc instruction_set_parser.h
)
set(oz3_tools_DEPS
  gb_parse
  oz3_core
)
set(oz3_tools_LIBS
  absl::check
  absl::log
)

set(oz3_tools_TEST_SOURCE
  instruction_set_parser_test.cc
)

gb_add_library(oz3_tools)
