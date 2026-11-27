// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <memory>

#include "gb/base/clock.h"
#include "gb/base/context.h"
#include "gb/base/context_builder.h"
#include "gb/cli/cli_path.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "oz3/ozzy/app/frontend.h"
#include "oz3/ozzy/app/ozzy_app.h"
#include "oz3/ozzy/term/terminal_frontend.h"

int main(int argc, char* argv[]) {
  // The context is shared with the app, which keeps it while it runs, and
  // read here once it is done.
  auto context = std::make_shared<gb::Context>(
      gb::ContextBuilder()
          .SetPtr<gb::Clock>(gb::RealtimeClock::GetClock())
          .SetOwned<gb::FileSystem>(gb::CreateCliFileSystem())
          .SetOwned<oz3::FrontendFactory>(
              std::make_unique<oz3::TerminalFrontendFactory>())
          .Build());
  oz3::OzzyApp app;
  app.Run(context, gb::GetMainArgs(argc, argv));
  return context->GetValueOrDefault<int>(oz3::OzzyApp::kKeyExitCode, 1);
}
