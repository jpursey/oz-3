// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CLI_MICROASM_OZ3ISM_H_
#define OZ3_CLI_MICROASM_OZ3ISM_H_

#include "gb/cli/cli.h"

namespace oz3 {

// oz3ism's command line, which gb::RunCommandLine runs. It assembles an
// instruction set from source, and writes it as C++ for an InstructionSetDef.
// Its context holds the gb::FileSystem its paths are in
// (gb::kConstraintCliFileSystem), and may hold the gb::CliWriter it writes to.
// It writes only errors (gb::CliText::kError), and help when asked for it.
const gb::CliCommand& GetOz3ismCommand();

}  // namespace oz3

#endif  // OZ3_CLI_MICROASM_OZ3ISM_H_
