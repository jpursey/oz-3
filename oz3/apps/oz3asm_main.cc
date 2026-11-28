// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "gb/base/context_builder.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "gb/file/local_file_protocol.h"
#include "oz3/cli/asm/oz3asm.h"

int main(int argc, char* argv[]) {
  gb::FileSystem file_system;
  file_system.Register(gb::LocalFileProtocol::CreateOsRoot());
  file_system.SetDefaultProtocol("file");
  return gb::RunCommandLine(
      gb::ContextBuilder().SetPtr<gb::FileSystem>(&file_system).Build(),
      &oz3::GetOz3asmCommand(), gb::GetMainArgs(argc, argv));
}
