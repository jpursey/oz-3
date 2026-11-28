## Copyright (c) 2024 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_tools_SOURCE
  disk_tool.cc disk_tool.h
  expression.cc expression.h
  instruction_assembler.cc instruction_assembler.h
  instruction_def_exporter.cc instruction_def_exporter.h
  instruction_set_syntax.cc instruction_set_syntax.h
  program_assembler.h program_assembler.cc
  program_grammar.h program_grammar.cc
  program_lexer.h program_lexer.cc
  program_macro.h program_macro.cc
  source_pattern.h source_pattern.cc
)
set(oz3_tools_DEPS
  absl::algorithm_container
  absl::flat_hash_map
  absl::flat_hash_set
  absl::function_ref
  absl::status
  absl::statusor
  absl::str_format
  absl::strings
  absl::time
  gb_base
  gb_file
  gb_parse
  oz3_core
  oz3_disk
  oz3_program
)

set(oz3_tools_LIBS
  absl::check
  absl::log
)

set(oz3_tools_TEST_SOURCE
  disk_tool_test.cc
  expression_test.cc
  instruction_assembler_test.cc
  instruction_def_exporter_test.cc
  instruction_set_syntax_test.cc
  program_assembler_test.cc
  program_grammar_test.cc
  program_lexer_test.cc
  program_macro_test.cc
  source_pattern_test.cc
)
set(oz3_tools_TEST_DEPS
  absl::flat_hash_map
  oz3_core_testing
  oz3_disk_testing
)

gb_add_library(oz3_tools)
