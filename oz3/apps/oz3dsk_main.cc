// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "gb/base/context.h"
#include "gb/cli/cli_path.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "oz3/cli/disk/oz3dsk.h"

int main(int argc, char* argv[]) {
  gb::Context context;
  context.SetOwned(gb::CreateCliFileSystem());
  return gb::RunCommandLine(&context, &oz3::GetOz3dskCommand(),
                            gb::GetMainArgs(argc, argv));
}
