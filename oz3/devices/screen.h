// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_SCREEN_H_
#define OZ3_DEVICES_SCREEN_H_

#include <array>
#include <cstdint>
#include <memory>
#include <string_view>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "oz3/core/core_types.h"
#include "oz3/core/device.h"
#include "oz3/core/device_config.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/lockable.h"
#include "oz3/core/memory_map.h"

namespace oz3 {

//==============================================================================
// Cell attributes
//==============================================================================

// A cell is a word: the low byte is the character, and the high byte its
// attributes. Characters 0 to 127 are ASCII, and 128 to 255 graphics
// characters, which the host draws.

// Styles, which the host applies to the character's pixels.
inline constexpr uint16_t kScreenInverse = 0x0100;
inline constexpr uint16_t kScreenHalfIntensity = 0x0200;
inline constexpr uint16_t kScreenBlink = 0x0400;

// Draws the character from the alternate character set: a second 256
// characters.
inline constexpr uint16_t kScreenAlternate = 0x0800;

// Lines, which set every pixel on one edge of the cell. A line on each of two
// neighboring cells' shared edges is twice as thick, and kScreenLineBottom is
// an underline.
inline constexpr uint16_t kScreenLineTop = 0x1000;
inline constexpr uint16_t kScreenLineBottom = 0x2000;
inline constexpr uint16_t kScreenLineLeft = 0x4000;
inline constexpr uint16_t kScreenLineRight = 0x8000;

//==============================================================================
// Cursor
//==============================================================================

// The cursor word is the cursor's mode in bits 0 and 1, ORed with its style in
// bits 2 and 3. The other bits are reserved.
inline constexpr uint16_t kScreenCursorModeMask = 0x0003;
inline constexpr uint16_t kScreenCursorStyleMask = 0x000C;

// Whether the cursor is shown, and how it blinks.
enum class ScreenCursorMode : uint16_t {
  kOff = 0x0000,
  kOn = 0x0001,
  kSlowBlink = 0x0002,
  kFastBlink = 0x0003,
};

// How the cursor is drawn on its cell.
enum class ScreenCursorStyle : uint16_t {
  kBlock = 0x0000,      // Every pixel lit
  kInverse = 0x0004,    // The cell's pixels inverted
  kUnderline = 0x0008,  // The bottom row of pixels lit
  kLeftEdge = 0x000C,   // The leftmost column of pixels lit
};

//==============================================================================
// ScreenSettings
//==============================================================================

// Everything a host sets about a screen but its name: its wiring, its size,
// and how often it refreshes.
struct ScreenSettings {
  // The most cells, which fit in the page before the control words.
  static constexpr int kMaxCells = 0xFF0;

  // The bank the screen's page is in, and the page in it.
  int bank = 0;
  int page = 0;

  // The interrupt raised at each refresh.
  DeviceInterrupt interrupt;

  // The size, in cells, which must each be at least 1, with no more than
  // kMaxCells cells.
  int columns = 80;
  int rows = 24;

  // The refreshes a second, which must be at least 1. The default is the full
  // frames a second of an interlaced NTSC display.
  int refresh_hz = 30;
};

//==============================================================================
// ScreenPicture
//==============================================================================

// What a screen shows, as taken at a refresh, for the host to draw.
struct ScreenPicture {
  bool operator==(const ScreenPicture&) const = default;

  // The size, in cells.
  int columns = 0;
  int rows = 0;

  // The cells as shown, with the scroll row applied, row by row from the top.
  std::vector<uint16_t> cells;

  // The cursor, in screen positions, with its mode and style. A cursor outside
  // the screen is off.
  int cursor_column = 0;
  int cursor_row = 0;
  ScreenCursorMode cursor_mode = ScreenCursorMode::kOff;
  ScreenCursorStyle cursor_style = ScreenCursorStyle::kBlock;
};

//==============================================================================
// ScreenMemory
//==============================================================================

// The screen's page, which cores read and write through its bank: the cells,
// row by row, then unused words a program may use, then the control words at
// the end of the page, so a program finds them at the same address whatever
// the screen's size.
//
// Writes to the read-only and reserved words are ignored, so only the screen
// sets them. Reserved words read 0.
class ScreenMemory final : public MemoryMap {
 public:
  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // Addresses of the control words, in the screen's own addresses. Columns,
  // rows, and the frame counter are read only. The scroll row is the row of
  // cells shown at the top of the screen, the cursor's position is in screen
  // positions, and the cursor word is its mode and style. Writing a row in
  // screen positions to clear row fills it with the fill word as it is written,
  // and clear row reads 0. The rest of the page, from kReservedAddress, is
  // reserved.
  static constexpr int kColumnsAddress = ScreenSettings::kMaxCells;
  static constexpr int kRowsAddress = 0xFF1;
  static constexpr int kFrameCountAddress = 0xFF2;
  static constexpr int kScrollRowAddress = 0xFF3;
  static constexpr int kCursorColumnAddress = 0xFF4;
  static constexpr int kCursorRowAddress = 0xFF5;
  static constexpr int kCursorAddress = 0xFF6;
  static constexpr int kFillAddress = 0xFF7;
  static constexpr int kClearRowAddress = 0xFF8;
  static constexpr int kReservedAddress = 0xFF9;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Constructs a blank page for a screen of `columns` by `rows` cells.
  ScreenMemory(int columns, int rows);
  ~ScreenMemory() override = default;

  //----------------------------------------------------------------------------
  // Lock-required operations
  //----------------------------------------------------------------------------

  // Returns the page's words. `lock` must be a lock on the page's bank.
  absl::Span<const uint16_t> GetMem(const Lock& lock) const;

  // Sets the frame counter. `lock` must be a lock on the page's bank.
  void SetFrameCount(const Lock& lock, uint16_t count);

 protected:
  //----------------------------------------------------------------------------
  // MemoryMap implementation
  //----------------------------------------------------------------------------

  void Read(int address, uint16_t* data, int size) override;
  void Write(int address, const uint16_t* data, int size) override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // Fills `row`, in screen positions, with the fill word. A row outside the
  // screen clears nothing.
  void ClearRow(int row);

  std::array<uint16_t, kMemoryBankPageSize> mem_ = {};
};

//==============================================================================
// ScreenFactory
//==============================================================================

// A screen on one page of a bank, which a Processor creates a Screen from.
class ScreenFactory final : public DeviceFactory {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a factory for a screen named `name`. Returns an error if
  // `settings` are invalid.
  static absl::StatusOr<std::unique_ptr<ScreenFactory>> Create(
      std::string_view name, const ScreenSettings& settings);

  ~ScreenFactory() override = default;

  //----------------------------------------------------------------------------
  // DeviceFactory implementation
  //----------------------------------------------------------------------------

  const DeviceConfig& GetConfig() const override { return config_; }
  std::unique_ptr<Device> CreateDevice() const override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  ScreenFactory(std::string_view name, const ScreenSettings& settings);

  const DeviceConfig config_;
  const ScreenSettings settings_;
};

//==============================================================================
// Screen
//==============================================================================

// A screen shows a page of character cells (see ScreenMemory), which cores
// write through its bank.
//
// The screen refreshes refresh_hz times a second: refresh n is at cycle
// n * clock_hz / refresh_hz, so the schedule doesn't drift where a refresh
// isn't a whole number of cycles. At a refresh, it takes its bank's lock,
// takes the page as the picture, adds one to the frame counter, and releases
// the lock, all in one cycle, so it never sees half of a core's write. Then it
// raises its interrupt. A refresh waits while a core holds the bank, and one
// that waited doesn't delay the next. At most one refresh is in a cycle.
//
// The host reads the picture with GetPicture(), which changes only at a
// refresh.
class Screen final : public Device {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  ~Screen() override = default;

  //----------------------------------------------------------------------------
  // Device implementation
  //----------------------------------------------------------------------------

  MemoryMap* GetMemoryMap() override { return &memory_; }

  //----------------------------------------------------------------------------
  // Host interface
  //----------------------------------------------------------------------------

  // Returns the picture taken at the last refresh, which is blank before the
  // first. Called between Processor::Execute calls.
  const ScreenPicture& GetPicture() const { return picture_; }

  // Returns the number of refreshes since power on, which the frame counter
  // holds the low 16 bits of.
  int64_t GetFrameCount() const { return frame_count_; }

 protected:
  //----------------------------------------------------------------------------
  // ExecutionComponent implementation
  //----------------------------------------------------------------------------

  void Execute() override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The factory constructs the screen with its validated settings.
  friend class ScreenFactory;

  Screen(DeviceConfig config, const ScreenSettings& settings);

  // Returns the cycle of the next refresh.
  Cycles GetRefreshCycle() const;

  // Takes the picture from the page, and adds one to the frame counter.
  void Refresh(const Lock& lock);

  const int refresh_hz_;
  ScreenMemory memory_;
  ScreenPicture picture_;
  int64_t frame_count_ = 0;
  std::unique_ptr<Lock> lock_;  // The bank's lock, while it is requested.
};

}  // namespace oz3

#endif  // OZ3_DEVICES_SCREEN_H_
