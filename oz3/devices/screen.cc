// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/screen.h"

#include <algorithm>
#include <string>
#include <utility>

#include "absl/log/check.h"
#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"
#include "oz3/core/memory_bank_config.h"

namespace oz3 {

//==============================================================================
// ScreenMemory
//==============================================================================

ScreenMemory::ScreenMemory(int columns, int rows) {
  mem_[kColumnsAddress] = static_cast<uint16_t>(columns);
  mem_[kRowsAddress] = static_cast<uint16_t>(rows);
}

absl::Span<const uint16_t> ScreenMemory::GetMem(const Lock& lock) const {
  DCHECK(IsLocked(lock));
  return mem_;
}

void ScreenMemory::SetFrameCount(const Lock& lock, uint16_t count) {
  DCHECK(IsLocked(lock));
  mem_[kFrameCountAddress] = count;
}

void ScreenMemory::Read(int address, uint16_t* data, int size) {
  std::copy_n(mem_.begin() + address, size, data);
}

void ScreenMemory::Write(int address, const uint16_t* data, int size) {
  for (int i = 0; i < size; ++i, ++address) {
    // The read-only and reserved words are only the screen's to set.
    if (address < kColumnsAddress ||
        (address >= kScrollRowAddress && address < kReservedAddress)) {
      mem_[address] = data[i];
    }
  }
}

//==============================================================================
// ScreenFactory
//==============================================================================

absl::StatusOr<std::unique_ptr<ScreenFactory>> ScreenFactory::Create(
    std::string_view name, const ScreenSettings& settings) {
  if (settings.columns < 1 || settings.rows < 1) {
    return absl::InvalidArgumentError(
        absl::StrCat("Screen ", name, " is ", settings.columns, " by ",
                     settings.rows, ", but each must be at least 1"));
  }
  if (int64_t{settings.columns} * settings.rows > ScreenSettings::kMaxCells) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Screen ", name, " is ", settings.columns, " by ", settings.rows,
        ", but it can have at most ", ScreenSettings::kMaxCells, " cells"));
  }
  if (settings.refresh_hz < 1) {
    return absl::InvalidArgumentError(
        absl::StrCat("Screen ", name, " has a refresh rate of ",
                     settings.refresh_hz, ", but it must be at least 1"));
  }
  return absl::WrapUnique(new ScreenFactory(name, settings));
}

ScreenFactory::ScreenFactory(std::string_view name,
                             const ScreenSettings& settings)
    : config_(
          {.name = std::string(name),
           .interrupts = {settings.interrupt},
           .memory = {.bank = settings.bank,
                      .pages = {.start_page = settings.page, .page_count = 1},
                      .masks = MemoryPageMasks::ReadWrite(0b1)}}),
      settings_(settings) {}

std::unique_ptr<Device> ScreenFactory::CreateDevice() const {
  return absl::WrapUnique(new Screen(config_, settings_));
}

//==============================================================================
// Screen
//==============================================================================

Screen::Screen(DeviceConfig config, const ScreenSettings& settings)
    : Device(std::move(config)),
      refresh_hz_(settings.refresh_hz),
      memory_(settings.columns, settings.rows),
      picture_(
          {.columns = settings.columns,
           .rows = settings.rows,
           .cells = std::vector<uint16_t>(settings.columns * settings.rows)}) {}

Cycles Screen::GetRefreshCycle() const {
  return (frame_count_ + 1) * GetClockHz() / refresh_hz_;
}

void Screen::Refresh(const Lock& lock) {
  const absl::Span<const uint16_t> mem = memory_.GetMem(lock);
  const int columns = picture_.columns;
  const int rows = picture_.rows;

  // The scroll row is the cells' row shown at the top, wrapping at the last.
  const int scroll_row = mem[ScreenMemory::kScrollRowAddress] % rows;
  std::rotate_copy(mem.begin(), mem.begin() + scroll_row * columns,
                   mem.begin() + rows * columns, picture_.cells.begin());

  const uint16_t cursor = mem[ScreenMemory::kCursorAddress];
  picture_.cursor_column = mem[ScreenMemory::kCursorColumnAddress];
  picture_.cursor_row = mem[ScreenMemory::kCursorRowAddress];
  picture_.cursor_mode =
      picture_.cursor_column < columns && picture_.cursor_row < rows
          ? static_cast<ScreenCursorMode>(cursor & kScreenCursorModeMask)
          : ScreenCursorMode::kOff;
  picture_.cursor_style =
      static_cast<ScreenCursorStyle>(cursor & kScreenCursorStyleMask);

  ++frame_count_;
  memory_.SetFrameCount(lock, static_cast<uint16_t>(frame_count_));
}

void Screen::Execute() {
  if (GetCycles() >= GetRefreshCycle()) {
    if (lock_ == nullptr) {
      lock_ = memory_.RequestLock();
    }
    if (!lock_->IsLocked()) {
      AdvanceCycles(1);
      return;
    }
    Refresh(*lock_);
    lock_.reset();
    RaiseInterrupt(0);
  }

  // The next refresh is on the schedule, however long this one waited.
  AdvanceCycles(std::max<Cycles>(GetRefreshCycle() - GetCycles(), 1));
}

}  // namespace oz3
