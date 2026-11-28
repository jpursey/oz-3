## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_cli_asm_SOURCE
  oz3asm.cc oz3asm.h
)
set(oz3_cli_asm_DEPS
  absl::algorithm_container
  absl::status
  absl::statusor
  absl::strings
  gb_base
  gb_cli
  gb_file
  gb_parse
  oz3_asm
  oz3_core
  oz3_instructions
  oz3_microasm
  oz3_program
  oz3_syntax
)

set(oz3_cli_asm_TEST_SOURCE
  oz3asm_test.cc
)

gb_add_library(oz3_cli_asm)
