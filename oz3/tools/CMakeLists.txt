## Copyright (c) 2024 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_tools_SOURCE
  bare_program.cc bare_program.h
  disk_file_protocol.cc disk_file_protocol.h
  disk_tool.cc disk_tool.h
  disk_transfer.cc disk_transfer.h
  expression.cc expression.h
  instruction_assembler.cc instruction_assembler.h
  instruction_def_exporter.cc instruction_def_exporter.h
  instruction_set_syntax.cc instruction_set_syntax.h
  program.h program.cc
  program_assembler.h program_assembler.cc
  program_file.h program_file.cc
  program_grammar.h program_grammar.cc
  program_lexer.h program_lexer.cc
  program_listing.h program_listing.cc
  program_loader.h program_loader.cc
  program_macro.h program_macro.cc
  program_map.h program_map.cc
  program_map_chunks.h
  program_map_file.h program_map_file.cc
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
)

set(oz3_tools_LIBS
  absl::check
  absl::log
)

set(oz3_tools_TEST_SOURCE
  bare_program_test.cc
  disk_file_protocol_test.cc
  disk_tool_test.cc
  disk_transfer_test.cc
  expression_test.cc
  instruction_assembler_test.cc
  instruction_def_exporter_test.cc
  instruction_set_syntax_test.cc
  program_assembler_test.cc
  program_file_test.cc
  program_grammar_test.cc
  program_lexer_test.cc
  program_listing_test.cc
  program_loader_test.cc
  program_macro_test.cc
  program_map_file_test.cc
  program_map_test.cc
  program_test.cc
  source_pattern_test.cc
)
set(oz3_tools_TEST_DEPS
  absl::flat_hash_map
  oz3_core_testing
  oz3_disk_testing
)

gb_add_library(oz3_tools)
