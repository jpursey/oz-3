// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/device.h"

#include <utility>

#include "absl/log/check.h"

namespace oz3 {

Device::Device(DeviceConfig config) : config_(std::move(config)) {}

void Device::AttachProcessor(CoreInternal, Processor* processor) {
  DCHECK(processor_ == nullptr);
  processor_ = processor;
}

}  // namespace oz3
