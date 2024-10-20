// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_INSTRUCTION_SET_PARSER_H_
#define OZ3_TOOLS_INSTRUCTION_SET_PARSER_H_

#include <string_view>

namespace oz3 {

// Parses an instruction set from the provided text.
bool ParseInstructionSet(std::string_view text);

// Parses an instruction set from the provided file.
bool ParseInstructionSetFile(std::string_view filename);

}  // namespace oz3

#endif  // OZ3_TOOLS_INSTRUCTION_SET_PARSER_H_
