## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

# oz3_cli_microasm doesn't use oz3_instructions, so oz3ism builds even when the
# default instruction set's generated source doesn't compile.
set(oz3_cli_microasm_SOURCE
  oz3ism.cc oz3ism.h
)
set(oz3_cli_microasm_DEPS
  absl::strings
  gb_base
  gb_cli
  gb_file
  gb_parse
  oz3_microasm
)

set(oz3_cli_microasm_TEST_SOURCE
  oz3ism_test.cc
)

gb_add_library(oz3_cli_microasm)
