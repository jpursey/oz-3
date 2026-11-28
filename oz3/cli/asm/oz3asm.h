// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CLI_ASM_OZ3ASM_H_
#define OZ3_CLI_ASM_OZ3ASM_H_

#include "gb/cli/cli.h"

namespace oz3 {

// oz3asm's command line, which gb::RunCommandLine runs. It assembles a program
// for the default instruction set, or one assembled from source, and writes its
// program file, and its map and listing if asked. Its context holds the
// gb::FileSystem its paths are in (gb::kConstraintCliFileSystem), and may hold
// the gb::CliWriter it writes to. It writes only errors (gb::CliText::kError),
// and help when asked for it.
const gb::CliCommand& GetOz3asmCommand();

}  // namespace oz3

#endif  // OZ3_CLI_ASM_OZ3ASM_H_
