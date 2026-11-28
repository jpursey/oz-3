// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "gb/base/context_builder.h"
#include "gb/cli/cli_path.h"
#include "gb/cli/run_command_line.h"
#include "oz3/cli/microasm/oz3ism.h"

int main(int argc, char* argv[]) {
  return gb::RunCommandLine(
      gb::ContextBuilder().SetOwned(gb::CreateCliFileSystem()).Build(),
      &oz3::GetOz3ismCommand(), gb::GetMainArgs(argc, argv));
}
