// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/screen.h"

#include <algorithm>
#include <array>
#include <string>
#include <utility>

#include "absl/log/check.h"
#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"
#include "oz3/core/memory_bank_config.h"
#include "oz3/devices/screen_characters.h"

namespace oz3 {

namespace {

// A cell's rows of pixels, one byte a row, with bit 7 the leftmost pixel.
using CellRows = std::array<uint8_t, kScreenGlyphHeight>;

constexpr uint8_t kAllPixels = 0xFF;
constexpr uint8_t kLeftPixel = 0x80;
constexpr uint8_t kRightPixel = 0x01;

// The pixel rows from a compact row's top to the next row's.
constexpr int kCompactRowHeight = 8;

// Swaps the lit and unlit pixels of a cell's `rows`.
void Invert(CellRows& rows) {
  for (uint8_t& row : rows) {
    row = static_cast<uint8_t>(~row);
  }
}

// Returns the rows of `cell`, drawn from its glyph in `characters` with its
// inverse and lines, or all off if it blinks and `blink_on` is false.
CellRows DrawCell(absl::Span<const uint16_t> characters, uint16_t cell,
                  bool blink_on) {
  CellRows rows = {};
  if ((cell & kScreenBlink) != 0 && !blink_on) {
    return rows;
  }
  const int glyph = (cell & 0xFF) + ((cell & kScreenAlternate) != 0 ? 256 : 0);
  const uint16_t* words = characters.data() + glyph * kScreenGlyphSize;
  for (int i = 0; i < kScreenGlyphHeight; ++i) {
    const uint16_t word = words[i / 2];
    rows[i] = static_cast<uint8_t>(i % 2 == 0 ? word >> 8 : word);
  }
  if ((cell & kScreenInverse) != 0) {
    Invert(rows);
  }
  if ((cell & kScreenLineTop) != 0) {
    rows.front() = kAllPixels;
  }
  if ((cell & kScreenLineBottom) != 0) {
    rows.back() = kAllPixels;
  }
  const uint8_t sides =
      static_cast<uint8_t>(((cell & kScreenLineLeft) != 0 ? kLeftPixel : 0) |
                           ((cell & kScreenLineRight) != 0 ? kRightPixel : 0));
  for (uint8_t& row : rows) {
    row |= sides;
  }
  return rows;
}

// Draws a cursor of `style` over a cell's `rows`.
void DrawCursor(ScreenCursorStyle style, CellRows& rows) {
  switch (style) {
    case ScreenCursorStyle::kBlock:
      rows.fill(kAllPixels);
      break;
    case ScreenCursorStyle::kInverse:
      Invert(rows);
      break;
    case ScreenCursorStyle::kUnderline:
      rows.back() = kAllPixels;
      break;
    case ScreenCursorStyle::kLeftEdge:
      for (uint8_t& row : rows) {
        row |= kLeftPixel;
      }
      break;
  }
}

}  // namespace

//==============================================================================
// ScreenMemory
//==============================================================================

ScreenMemory::ScreenMemory(int columns, int rows,
                           absl::Span<const uint16_t> characters) {
  mem_[kColumnsAddress] = static_cast<uint16_t>(columns);
  mem_[kRowsAddress] = static_cast<uint16_t>(rows);
  DCHECK(characters.size() == characters_.size());
  std::copy(characters.begin(), characters.end(), characters_.begin());
}

absl::Span<const uint16_t> ScreenMemory::GetMem(const Lock& lock) const {
  DCHECK(IsLocked(lock));
  return mem_;
}

absl::Span<const uint16_t> ScreenMemory::GetCharacters(const Lock& lock) const {
  DCHECK(IsLocked(lock));
  return characters_;
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
    // The open window sends writes to the character set, clear row stores
    // nothing, and the read-only and reserved words are only the screen's to
    // set.
    if (address < kScreenCharacterSetSize &&
        (mem_[kCharacterWindowAddress] & kScreenCharacterWindowOpen) != 0) {
      characters_[address] = data[i];
    } else if (address == kClearRowAddress) {
      ClearRow(data[i]);
    } else if (address < kColumnsAddress ||
               (address >= kScrollRowAddress && address < kReservedAddress)) {
      mem_[address] = data[i];
    }
  }
}

void ScreenMemory::ClearRow(int row) {
  const int columns = mem_[kColumnsAddress];
  const int rows = mem_[kRowsAddress];
  if (row >= rows) {
    return;
  }
  const int cells_row = (mem_[kScrollRowAddress] + row) % rows;
  std::fill_n(mem_.begin() + cells_row * columns, columns, mem_[kFillAddress]);
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
  if (settings.columns > ScreenSettings::kMaxColumns ||
      settings.rows > ScreenSettings::kMaxRows) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Screen ", name, " is ", settings.columns, " by ", settings.rows,
        ", but it can have at most ", ScreenSettings::kMaxColumns,
        " columns and ", ScreenSettings::kMaxRows, " rows"));
  }
  if (settings.columns * settings.rows > ScreenSettings::kMaxCells) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Screen ", name, " is ", settings.columns, " by ", settings.rows,
        ", but it can have at most ", ScreenSettings::kMaxCells, " cells"));
  }
  if (settings.refresh_hz < 1) {
    return absl::InvalidArgumentError(
        absl::StrCat("Screen ", name, " has a refresh rate of ",
                     settings.refresh_hz, ", but it must be at least 1"));
  }
  for (const auto& [blink, blink_hz] :
       {std::pair{"slow", settings.slow_blink_hz},
        std::pair{"fast", settings.fast_blink_hz}}) {
    if (blink_hz < 1 || blink_hz > settings.refresh_hz / 2) {
      return absl::InvalidArgumentError(absl::StrCat(
          "Screen ", name, " has a ", blink, " blink rate of ", blink_hz,
          ", but it must be at least 1, and at most half the refresh rate of ",
          settings.refresh_hz));
    }
  }
  if (settings.characters.has_value() &&
      settings.characters->size() != kScreenCharacterSetSize) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Screen ", name, " has a character set of ",
        settings.characters->size(), " words, but it must have ",
        kScreenCharacterSetSize, ", or be unset for the built-in one"));
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
      slow_blink_hz_(settings.slow_blink_hz),
      fast_blink_hz_(settings.fast_blink_hz),
      memory_(settings.columns, settings.rows,
              settings.characters.has_value()
                  ? absl::Span<const uint16_t>(*settings.characters)
                  : GetDefaultScreenCharacters()),
      picture_(
          {.columns = settings.columns,
           .rows = settings.rows,
           .cells = std::vector<uint16_t>(settings.columns * settings.rows),
           .pixel_width = settings.columns * kScreenGlyphWidth,
           .pixel_height = settings.rows * kScreenGlyphHeight,
           .pixels = std::vector<uint8_t>(settings.columns * kScreenGlyphWidth *
                                          settings.rows * kScreenGlyphHeight),
           .drawn_height = settings.rows * kScreenGlyphHeight}) {}

Cycles Screen::GetRefreshCycle() const {
  return (frame_count_ + 1) * GetClockHz() / refresh_hz_;
}

void Screen::Refresh(const Lock& lock) {
  ++frame_count_;
  memory_.SetFrameCount(lock, static_cast<uint16_t>(frame_count_));

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

  DrawPixels(memory_.GetCharacters(lock), mem[ScreenMemory::kCompactRowAddress],
             mem[ScreenMemory::kCompactRowCountAddress]);
}

bool Screen::IsBlinkOn(int blink_hz) const {
  return frame_count_ * 2 * blink_hz / refresh_hz_ % 2 == 0;
}

bool Screen::IsCursorOn() const {
  switch (picture_.cursor_mode) {
    case ScreenCursorMode::kOff:
      return false;
    case ScreenCursorMode::kOn:
      return true;
    case ScreenCursorMode::kSlowBlink:
      return IsBlinkOn(slow_blink_hz_);
    case ScreenCursorMode::kFastBlink:
      return IsBlinkOn(fast_blink_hz_);
  }
  return false;
}

void Screen::DrawPixels(absl::Span<const uint16_t> characters, int compact_row,
                        int compact_row_count) {
  const bool slow_blink_on = IsBlinkOn(slow_blink_hz_);
  const bool cursor_on = IsCursorOn();
  const int columns = picture_.columns;
  const int rows = picture_.rows;
  const int width = picture_.pixel_width;

  // Compact rows overlap the row below, so each pixel is lit to the brightest
  // level drawn on it, and the pixel rows no row reaches stay off.
  std::ranges::fill(picture_.pixels, kScreenPixelOff);
  int top = 0;  // The row's first pixel row.
  for (int row = 0; row < rows; ++row) {
    for (int column = 0; column < columns; ++column) {
      const uint16_t cell = picture_.cells[row * columns + column];
      CellRows cell_rows = DrawCell(characters, cell, slow_blink_on);
      if (cursor_on && column == picture_.cursor_column &&
          row == picture_.cursor_row) {
        DrawCursor(picture_.cursor_style, cell_rows);
      }

      // Each pixel row of the cell is a screen's width below the last.
      const uint8_t level = (cell & kScreenHalfIntensity) != 0
                                ? kScreenPixelDim
                                : kScreenPixelBright;
      int index = top * width + column * kScreenGlyphWidth;
      for (const uint8_t pixel_row : cell_rows) {
        for (int x = 0; x < kScreenGlyphWidth; ++x) {
          uint8_t& pixel = picture_.pixels[index + x];
          pixel = std::max(pixel, (pixel_row & (kLeftPixel >> x)) != 0
                                      ? level
                                      : kScreenPixelOff);
        }
        index += width;
      }
    }

    // The drawing reaches the bottom of the last row, compact or not.
    picture_.drawn_height = top + kScreenGlyphHeight;
    const bool compact =
        row >= compact_row && row < compact_row + compact_row_count;
    top += compact ? kCompactRowHeight : kScreenGlyphHeight;
  }
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
