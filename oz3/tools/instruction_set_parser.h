// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_INSTRUCTION_SET_PARSER_H_
#define OZ3_TOOLS_INSTRUCTION_SET_PARSER_H_

#include <string_view>

#include "gb/file/file_system.h"
#include "gb/parse/parse_result.h"

namespace oz3 {

// Parses an instruction set from the provided text.
gb::ParseResult ParseInstructionSet(std::string text);

// Parses an instruction set from the file at the specified path.
gb::ParseResult ParseInstructionSetFile(gb::FileSystem& file_system,
                                        std::string_view path);

}  // namespace oz3

#endif  // OZ3_TOOLS_INSTRUCTION_SET_PARSER_H_
