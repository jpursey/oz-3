// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "gb/base/context.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "gb/file/local_file_protocol.h"
#include "oz3/cli/microasm/oz3ism.h"

int main(int argc, char* argv[]) {
  gb::FileSystem file_system;
  file_system.Register(gb::LocalFileProtocol::CreateOsRoot());
  file_system.SetDefaultProtocol("file");
  gb::Context context;
  context.SetPtr<gb::FileSystem>(&file_system);
  return gb::RunCommandLine(&context, &oz3::GetOz3ismCommand(),
                            gb::GetMainArgs(argc, argv));
}
