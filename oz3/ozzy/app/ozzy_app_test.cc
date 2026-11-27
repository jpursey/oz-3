// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/app/ozzy_app.h"

#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"
#include "absl/time/time.h"
#include "gb/base/context.h"
#include "gb/base/context_builder.h"
#include "gb/base/fake_clock.h"
#include "gb/cli/cli_writer.h"
#include "gb/file/file_system.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/processor_config.h"
#include "oz3/devices/screen/screen.h"
#include "oz3/disk/faulty_file_protocol.h"
#include "oz3/ozzy/app/fake_frontend.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_testing.h"

namespace oz3 {
namespace {

using ::testing::AllOf;
using ::testing::ContainsRegex;
using ::testing::ElementsAre;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::Optional;

// The exit code RunApp returns if the app left none in the context.
constexpr int kNoExitCode = -100;

// The config file the tests write.
constexpr std::string_view kConfigPath = "mem:/test.ozzy";

// The image of drive A: in the tests that give the Ozzy one, which is beside
// the config file.
constexpr std::string_view kImagePath = "mem:/a.ozd";

// Restarts.
constexpr std::string_view kRestart = R"---(
    .STACK 0
    OUT.ID 0, 2
    HALT
)---";

// Shuts down with the exit code -3.
constexpr std::string_view kShutDownWithMinus3 = R"---(
    .STACK 0
    OUT.ID 0, 0FFFD0001h
    HALT
)---";

// Waits for a typed key, then shuts down with it as the exit code.
constexpr std::string_view kShutDownWithTypedKey = R"---(
    .STACK 0
    wait:
        INS 1, R1
        JC NS, wait
        MVQ R0, 1
        OUT.ID 0, D0
        HALT
)---";

// Waits for a key in the first key state port to be pressed, then shuts down
// with the port's held keys as the exit code.
constexpr std::string_view kShutDownWithKeysHeld = R"---(
    .STACK 0
    wait:
        INS 6, R1
        JC NS, wait
        MVQ R0, 1
        OUT.ID 0, D0
        HALT
)---";

// Tests of the app, run with a fake frontend and clock, and a file system in
// memory whose files can be made to fail, which outlive each app a test runs.
class OzzyAppTest : public ::testing::Test {
 protected:
  OzzyAppTest() {
    auto protocol = std::make_unique<FaultyFileProtocol>();
    protocol_ = protocol.get();
    file_system_.Register(std::move(protocol));
    file_system_.SetDefaultProtocol("mem");

    // The game loop reads the clock until each frame is due.
    clock_.SetAutoAdvance(absl::Microseconds(10));
  }

  // Runs an app with `args`, and returns the exit code it left in its
  // context.
  int RunApp(const std::vector<std::string_view>& args) {
    auto context = std::make_shared<gb::Context>(
        gb::ContextBuilder()
            .SetPtr<gb::Clock>(&clock_)
            .SetPtr<gb::FileSystem>(&file_system_)
            .SetPtr<FrontendType>(&type_)
            .SetValue<gb::CliWriter>(
                [this](gb::CliText type, std::string_view text) {
                  Write(type, text);
                })
            .Build());
    OzzyApp app;
    app.Run(context, args);
    return context->GetValueOrDefault<int>(OzzyApp::kKeyExitCode, kNoExitCode);
  }

  // Runs the app with no arguments, which must exit with 0, and returns the
  // pixel width of the Ozzy's screen, as it was when attached.
  int RunAndGetPixelWidth() {
    int pixel_width = -1;
    state().on_attach = [&pixel_width](Ozzy& ozzy) {
      ASSERT_NE(ozzy.GetScreen(), nullptr);
      pixel_width = ozzy.GetScreen()->GetPicture().pixel_width;
    };
    EXPECT_EQ(RunApp({}), 0);
    state().on_attach = nullptr;
    return pixel_width;
  }

  // Writes `config` as the config file.
  void WriteConfig(std::string_view config) {
    ASSERT_TRUE(file_system_.WriteFile(kConfigPath, config));
  }

  // Loads `program` on core 0 at the first Attach, leaving the Ozzys of later
  // ones to boot as they would.
  void LoadAtFirstAttach(std::string_view program) {
    state().on_attach = [this, program](Ozzy& ozzy) {
      if (absl::c_count(state().calls, "Attach") == 1) {
        EXPECT_TRUE(LoadOnCore0(ozzy, program));
      }
    };
  }

  FakeFrontendState& state() { return type_.GetState(); }

  gb::FakeClock clock_;
  gb::FileSystem file_system_;
  FaultyFileProtocol* protocol_ = nullptr;  // Owned by file_system_.
  FakeFrontendType type_;

  // What the app wrote, and whether any of it was written while the frontend
  // was alive.
  std::string help_;
  std::string error_;
  bool wrote_with_frontend_ = false;

 private:
  void Write(gb::CliText type, std::string_view text) {
    absl::StrAppend(type == gb::CliText::kHelp ? &help_ : &error_, text);
    wrote_with_frontend_ = wrote_with_frontend_ || state().alive;
  }
};

//------------------------------------------------------------------------------
// Command line
//------------------------------------------------------------------------------

TEST_F(OzzyAppTest, HelpIsTheFrontendsAndTheApps) {
  EXPECT_EQ(RunApp({"--help"}), 0);
  EXPECT_THAT(help_, AllOf(HasSubstr("fake-ozzy"),
                           HasSubstr(FakeFrontendType::kProgramHelp),
                           HasSubstr("--stats"), HasSubstr("--fake-flag"),
                           HasSubstr("<config file>")));
  EXPECT_EQ(error_, "");
  EXPECT_FALSE(state().settings.has_value());
}

TEST_F(OzzyAppTest, NoClockDoesntRun) {
  auto context = std::make_shared<gb::Context>(
      gb::ContextBuilder()
          .SetPtr<gb::FileSystem>(&file_system_)
          .SetPtr<FrontendType>(&type_)
          .SetValue<gb::CliWriter>([](gb::CliText, std::string_view) {})
          .Build());
  OzzyApp app;
  app.Run(context, {});
  EXPECT_FALSE(context->Exists<int>(OzzyApp::kKeyExitCode));
  EXPECT_FALSE(state().settings.has_value());
}

TEST_F(OzzyAppTest, UnknownFlag) {
  EXPECT_EQ(RunApp({"--bogus"}), 2);
  EXPECT_THAT(error_, HasSubstr("bogus"));
  EXPECT_FALSE(state().settings.has_value());
}

TEST_F(OzzyAppTest, MissingConfigFile) {
  EXPECT_EQ(RunApp({"mem:/missing.ozzy"}), 2);
  EXPECT_THAT(error_, HasSubstr("missing.ozzy"));
  EXPECT_FALSE(state().settings.has_value());
}

TEST_F(OzzyAppTest, BadConfigFile) {
  WriteConfig("clock_hz: \"fast\"");
  EXPECT_EQ(RunApp({kConfigPath}), 1);
  EXPECT_THAT(error_, HasSubstr("test.ozzy"));
  EXPECT_FALSE(state().settings.has_value());
}

TEST_F(OzzyAppTest, NoConfigFileRunsTheDefaultOzzy) {
  EXPECT_EQ(RunApp({}), 0);
  ASSERT_TRUE(state().settings.has_value());
  EXPECT_EQ(state().settings->clock_hz, ProcessorConfig::kDefaultClockHz);
  EXPECT_TRUE(state().settings->screen.has_value());
  EXPECT_THAT(state().calls, ElementsAre("Attach", "Update", "Detach"));
  EXPECT_EQ(error_, "");
}

TEST_F(OzzyAppTest, RunsTheConfigFilesOzzy) {
  WriteConfig("clock_hz: 1000");
  EXPECT_EQ(RunApp({kConfigPath}), 0);
  ASSERT_TRUE(state().settings.has_value());
  EXPECT_EQ(state().settings->clock_hz, 1000);
  EXPECT_THAT(state().calls, ElementsAre("Attach", "Update", "Detach"));
}

TEST_F(OzzyAppTest, ScreenDrawsPixelsWhenTheFrontendShowsThem) {
  state().shows_pixels = true;
  EXPECT_GT(RunAndGetPixelWidth(), 0);
}

TEST_F(OzzyAppTest, ScreenDrawsNoPixelsWhenTheFrontendDoesnt) {
  EXPECT_EQ(RunAndGetPixelWidth(), 0);
}

TEST_F(OzzyAppTest, FrontendReadsItsFlag) {
  EXPECT_EQ(RunApp({"--fake-flag", "value"}), 0);
  EXPECT_THAT(state().flag, Optional(std::string("value")));
}

TEST_F(OzzyAppTest, FrontendThatCantBeConfiguredIsWritten) {
  state().configure_error = absl::InvalidArgumentError("Bad frontend");
  EXPECT_EQ(RunApp({}), 1);
  EXPECT_EQ(error_, "Bad frontend");
  EXPECT_FALSE(state().created);
}

TEST_F(OzzyAppTest, FrontendThatFailsIsWritten) {
  state().create_error = absl::UnavailableError("No frontend");
  EXPECT_EQ(RunApp({}), 1);
  EXPECT_EQ(error_, "No frontend");
  EXPECT_FALSE(state().created);
  EXPECT_FALSE(wrote_with_frontend_);
}

//------------------------------------------------------------------------------
// Running
//------------------------------------------------------------------------------

TEST_F(OzzyAppTest, EachFrameUpdatesTheFrontendUntilQuit) {
  state().quit_after_updates = 3;
  EXPECT_EQ(RunApp({}), 0);
  EXPECT_THAT(state().calls,
              ElementsAre("Attach", "Update", "Update", "Update", "Detach"));
  EXPECT_FALSE(state().alive);
}

TEST_F(OzzyAppTest, TypedKeysReachTheKeyboard) {
  LoadAtFirstAttach(kShutDownWithTypedKey);
  state().keys.typed = {'a'};
  EXPECT_EQ(RunApp({}), 0);
  EXPECT_THAT(state().calls, ElementsAre("Attach", "Detach(97)", "Update"));
}

TEST_F(OzzyAppTest, KeyChangesReachTheKeyboard) {
  LoadAtFirstAttach(kShutDownWithKeysHeld);
  state().keys.changes = {{.key = 3, .held = true}};
  EXPECT_EQ(RunApp({}), 0);
  EXPECT_THAT(state().calls, ElementsAre("Attach", "Detach(8)", "Update"));
}

TEST_F(OzzyAppTest, KeysWithoutAKeyboardAreDropped) {
  WriteConfig(R"---(devices: [{ type: "timer" }])---");
  state().keys.typed = {'a'};
  state().quit_after_updates = 2;
  EXPECT_EQ(RunApp({kConfigPath}), 0);
  EXPECT_THAT(state().keys.typed, IsEmpty());
}

TEST_F(OzzyAppTest, RunTimeIsCapped) {
  // Each wait for a frame takes a second longer than asked, so the first and
  // third frames are over a second after the last, and run for the most a
  // frame runs, which is 100 cycles at 1 kHz. The second frame starts at once,
  // as the first ended late, so it runs for almost no time.
  clock_.SetSleepOffset(absl::Seconds(1));
  WriteConfig("clock_hz: 1000");
  state().quit_after_updates = 3;
  EXPECT_EQ(RunApp({"--stats", kConfigPath}), 0);
  EXPECT_THAT(help_, AllOf(HasSubstr("3 frames in 2.03 s: "),
                           HasSubstr("\n200 cycles: ")));
}

//------------------------------------------------------------------------------
// Power
//------------------------------------------------------------------------------

TEST_F(OzzyAppTest, RestartPowersOnANewOzzy) {
  LoadAtFirstAttach(kRestart);
  EXPECT_EQ(RunApp({}), 0);
  EXPECT_THAT(state().calls,
              ElementsAre("Attach", "Detach", "Attach", "Update", "Detach"));
}

TEST_F(OzzyAppTest, ShutDownShowsTheExitCodeUntilQuit) {
  LoadAtFirstAttach(kShutDownWithMinus3);
  state().quit_after_updates = 3;
  EXPECT_EQ(RunApp({}), 0);
  EXPECT_THAT(state().calls, ElementsAre("Attach", "Detach(-3)", "Update",
                                         "Update", "Update"));
}

TEST_F(OzzyAppTest, FailedPowerOnCreatesNoFrontend) {
  WriteConfig(
      R"---(drives: [{ preset: "floppy-360k", image: "missing.ozd" }])---");
  EXPECT_EQ(RunApp({kConfigPath}), 1);
  EXPECT_THAT(error_, AllOf(HasSubstr("Failed to create the Ozzy: "),
                            HasSubstr("missing.ozd")));
  EXPECT_FALSE(state().created);
  EXPECT_FALSE(wrote_with_frontend_);
}

TEST_F(OzzyAppTest, FailedPowerOnAtRestartEndsTheApp) {
  MakeTestImage(&file_system_, kImagePath);
  WriteConfig(R"---(drives: [{ preset: "floppy-360k", image: "a.ozd" }])---");

  // The image can't be read by the time the Ozzy restarts.
  state().on_attach = [this](Ozzy& ozzy) {
    EXPECT_TRUE(LoadOnCore0(ozzy, kRestart));
    protocol_->SetLimits({.reads = 0});
  };
  state().quit_after_updates = 3;
  EXPECT_EQ(RunApp({kConfigPath}), 1);
  EXPECT_THAT(error_, AllOf(HasSubstr("Failed to create the Ozzy: "),
                            HasSubstr("a.ozd")));
  EXPECT_FALSE(wrote_with_frontend_);
  EXPECT_THAT(state().calls, ElementsAre("Attach", "Detach"));
}

//------------------------------------------------------------------------------
// Stats
//------------------------------------------------------------------------------

TEST_F(OzzyAppTest, NoStatsWithoutTheFlag) {
  EXPECT_EQ(RunApp({}), 0);
  EXPECT_EQ(help_, "");
}

TEST_F(OzzyAppTest, StatsAcrossARestart) {
  // At 6 kHz, each frame of about 1/60 s runs about 100 cycles, so both Ozzys'
  // are counted.
  WriteConfig("clock_hz: 6000");
  LoadAtFirstAttach(kRestart);
  state().quit_after_updates = 2;
  EXPECT_EQ(RunApp({"--stats", kConfigPath}), 0);
  EXPECT_THAT(state().calls, ElementsAre("Attach", "Detach", "Attach", "Update",
                                         "Update", "Detach"));
  EXPECT_FALSE(wrote_with_frontend_);
  // Each frame's time is its 1/60 s, and the fake clock's own advance at each
  // read, so the rate is a little under 60.
  EXPECT_THAT(
      help_,
      AllOf(ContainsRegex(
                "2 frames in 0\\.03 s: (59\\.9.|60\\.00) frames a second\n"),
            HasSubstr("Each frame took "),
            ContainsRegex("\n(198|199|200) cycles: ")));
}

TEST_F(OzzyAppTest, StatsWhenQuitBeforeTheFirstFrame) {
  state().quit_after_updates = 0;
  EXPECT_EQ(RunApp({"--stats"}), 0);
  EXPECT_EQ(help_, "The Ozzy quit before its first frame.\n");
  EXPECT_THAT(state().calls, ElementsAre("Attach", "Detach"));
}

}  // namespace
}  // namespace oz3
