// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_INSTRUCTION_DEF_EXPORTER_H_
#define OZ3_TOOLS_INSTRUCTION_DEF_EXPORTER_H_

#include <string_view>

#include "gb/file/file_system.h"
#include "oz3/core/instruction_def.h"

namespace oz3 {

// Exports an instruction set definition to a C++ source file snippet defining
// the InstructionSetDef as a constant.
//
// The exported snippet is not a full file, but a snippet that can be included
// in a larger file via cut-and-paste or literal inclusion. The snippet will
// define all constants as constexpr with the specified `const_prefix`.
//
// The specified `filename` should be a full path, including extension and be
// valid for the specified file system.
bool ExportInstructionSetDef(gb::FileSystem& file_system,
                             std::string_view filename,
                             const InstructionSetDef& instruction_set,
                             std::string_view const_prefix = "k");

}  // namespace oz3

#endif  // OZ3_TOOLS_INSTRUCTION_DEF_EXPORTER_H_
