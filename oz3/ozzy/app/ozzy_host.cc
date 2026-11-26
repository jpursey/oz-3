// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/app/ozzy_host.h"

#include <memory>
#include <optional>
#include <utility>

#include "absl/log/check.h"
#include "absl/memory/memory.h"
#include "absl/status/statusor.h"
#include "gb/base/context_builder.h"

namespace oz3 {

std::unique_ptr<OzzyHost> OzzyHost::Create(OzzySettings settings,
                                           Contract contract) {
  gb::ValidatedContext context(std::move(contract));
  if (!context.IsValid()) {
    return nullptr;
  }
  return absl::WrapUnique(
      new OzzyHost(std::move(settings), std::move(context)));
}

OzzyHost::OzzyHost(OzzySettings settings, gb::ValidatedContext context)
    : settings_(std::move(settings)),
      context_(std::move(context)),
      ozzy_context_(std::make_shared<gb::Context>(
          gb::ContextBuilder()
              .SetParent(context_)
              .SetNew<PowerRequestCallback>(
                  [this](PowerRequest request) { OnPowerRequest(request); })
              .Build())) {}

OzzyHost::~OzzyHost() { PowerOff(); }

absl::Status OzzyHost::PowerOn() {
  if (ozzy_ != nullptr) {
    return absl::OkStatus();
  }
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(settings_, ozzy_context_);
  if (!ozzy.ok()) {
    return ozzy.status();
  }
  ozzy_ = *std::move(ozzy);
  return absl::OkStatus();
}

void OzzyHost::PowerOff() { ozzy_.reset(); }

OzzyHost::RunResult OzzyHost::Run(absl::Duration time) {
  CHECK(ozzy_ != nullptr) << "The Ozzy is off";
  // A request from running the Ozzy other than through Run is dropped.
  power_request_.reset();
  RunResult result;
  result.cycles = ozzy_->Run(time);
  result.power_request = power_request_;
  return result;
}

void OzzyHost::OnPowerRequest(PowerRequest request) {
  if (!power_request_.has_value()) {
    power_request_ = request;
  }
}

}  // namespace oz3
