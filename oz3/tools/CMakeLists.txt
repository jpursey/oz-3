## Copyright (c) 2024 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_tools_SOURCE
  instruction_assembler.cc instruction_assembler.h
  instruction_def_exporter.cc instruction_def_exporter.h
  program.h
  program_loader.h program_loader.cc
)
set(oz3_tools_DEPS
  gb_file
  gb_parse
  oz3_core
)

set(oz3_tools_LIBS
  absl::check
  absl::log
)

set(oz3_tools_TEST_SOURCE
  instruction_assembler_test.cc
  program_loader_test.cc
)

gb_add_library(oz3_tools)

set(oz3ism_SOURCE
  oz3ism.cc
)

set(oz3ism_DEPS
  oz3_core
  oz3_tools
)

set(oz3ism_LIBS
  absl::check
  absl::log
)

gb_add_executable(oz3ism)
