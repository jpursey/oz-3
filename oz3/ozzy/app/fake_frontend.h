// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_APP_FAKE_FRONTEND_H_
#define OZ3_OZZY_APP_FAKE_FRONTEND_H_

#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/cli/cli.h"
#include "gb/cli/cli_command_line.h"
#include "oz3/ozzy/app/frontend.h"
#include "oz3/ozzy/app/frontend_keys.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_settings.h"

namespace oz3 {

//==============================================================================
// FakeFrontendState
//==============================================================================

// What a fake frontend gives the app, which a test sets, and what the app did
// with it, which a test reads. Its factory keeps it, so it outlives the
// frontend.
struct FakeFrontendState {
  //----------------------------------------------------------------------------
  // Set by the test
  //----------------------------------------------------------------------------

  // The quit is requested once Update has been called this many times.
  int quit_after_updates = 1;

  // The keys the next TakeKeys returns.
  FrontendKeys keys;

  // Called at each Attach with the Ozzy, such as to load a program on it.
  std::function<void(Ozzy& ozzy)> on_attach;

  //----------------------------------------------------------------------------
  // Set by the frontend and its factory
  //----------------------------------------------------------------------------

  // The settings and flag the frontend was created with, if it was.
  std::optional<OzzySettings> settings;
  std::optional<std::string> flag;

  // Whether the frontend is alive.
  bool alive = false;

  // The calls the app made, in order: "Attach", "Update", "Detach", and
  // "Detach(<exit code>)".
  std::vector<std::string> calls;
};

//==============================================================================
// FakeFrontend
//==============================================================================

// A frontend that records the app's calls in a FakeFrontendState, and gives it
// the keys and quit the state holds.
class FakeFrontend final : public Frontend {
 public:
  // `state` must outlive this.
  explicit FakeFrontend(FakeFrontendState* state) : state_(state) {
    state_->alive = true;
  }
  ~FakeFrontend() override { state_->alive = false; }

  FrontendKeys TakeKeys() override { return std::exchange(state_->keys, {}); }

  bool IsQuitRequested() const override {
    return absl::c_count(state_->calls, "Update") >= state_->quit_after_updates;
  }

  void Update() override { state_->calls.push_back("Update"); }

  void Attach(Ozzy* ozzy) override {
    state_->calls.push_back("Attach");
    if (state_->on_attach) {
      state_->on_attach(*ozzy);
    }
  }

  void Detach(std::optional<int> exit_code) override {
    state_->calls.push_back(exit_code.has_value()
                                ? absl::StrCat("Detach(", *exit_code, ")")
                                : "Detach");
  }

 private:
  FakeFrontendState* const state_;
};

//==============================================================================
// FakeFrontendFactory
//==============================================================================

// Creates FakeFrontends, which share the factory's state. It has one flag of
// its own, --fake-flag, which Create records.
class FakeFrontendFactory final : public FrontendFactory {
 public:
  static constexpr std::string_view kProgramName = "fake-ozzy";
  static constexpr std::string_view kProgramHelp = "Runs a fake Ozzy.";

  static inline const gb::CliFlag<std::string_view> kFakeFlag = {
      .name = "fake-flag",
      .help = "A flag of the fake frontend.",
  };
  static inline const gb::CliAnyFlag kFlags[] = {&kFakeFlag};

  FakeFrontendState& GetState() { return state_; }

  // Sets the error Create returns, or OK for it to create the frontend.
  void SetCreateError(absl::Status error) { create_error_ = std::move(error); }

  std::string_view GetProgramName() const override { return kProgramName; }
  std::string_view GetProgramHelp() const override { return kProgramHelp; }
  absl::Span<const gb::CliAnyFlag> GetFlags() const override { return kFlags; }

  absl::StatusOr<std::unique_ptr<Frontend>> Create(
      const OzzySettings& settings,
      const gb::CliCommandLine& command_line) override {
    if (!create_error_.ok()) {
      return create_error_;
    }
    state_.settings = settings;
    if (const std::optional<std::string_view> flag =
            command_line.Get(kFakeFlag);
        flag.has_value()) {
      state_.flag = std::string(*flag);
    }
    return std::make_unique<FakeFrontend>(&state_);
  }

 private:
  absl::Status create_error_;
  FakeFrontendState state_;
};

}  // namespace oz3

#endif  // OZ3_OZZY_APP_FAKE_FRONTEND_H_
