// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CLI_DISK_OZ3DSK_H_
#define OZ3_CLI_DISK_OZ3DSK_H_

#include "gb/cli/cli.h"

namespace oz3 {

// oz3dsk's command line, which gb::RunCommandLine runs, each command a call on
// a DiskTool. Its context holds the gb::FileSystem its paths are in
// (gb::kConstraintCliFileSystem), and may hold the gb::CliWriter it writes to,
// and what DiskTool takes from its context, such as the clock it stamps files
// with. A command writes what it reports as help (gb::CliText::kHelp), as it
// was asked for, and its failures as errors.
const gb::CliCommand& GetOz3dskCommand();

}  // namespace oz3

#endif  // OZ3_CLI_DISK_OZ3DSK_H_
