// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_INSTRUCTION_SETS_INSTRUCTION_TEST_H_
#define OZ3_INSTRUCTION_SETS_INSTRUCTION_TEST_H_

#include <cstdint>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "oz3/core/base_core_test.h"
#include "oz3/core/port.h"
#include "oz3/instruction_sets/default_instruction_set.h"

namespace oz3 {

// An operation on a register with a count (such as a shift), and what it
// produces. See InstructionTest::RunCountCases.
struct CountCase {
  uint32_t value;
  uint16_t count;
  uint32_t result;
  uint16_t st;
  Cycles cycles;
};

// Returns a case for `value` with each of `counts`, which all produce `result`
// and `st` in `cycles`.
inline std::vector<CountCase> SameCountCases(absl::Span<const uint16_t> counts,
                                             uint32_t value, uint32_t result,
                                             uint16_t st, Cycles cycles) {
  std::vector<CountCase> cases;
  for (uint16_t count : counts) {
    cases.push_back({value, count, result, st, cycles});
  }
  return cases;
}

// An instruction and the cycles it takes. See InstructionTest::RunCycleCases.
struct CycleCase {
  std::string_view name;  // For failures, such as "ADD.W R0, R1"
  Cycles cycles;
  std::vector<uint16_t> code;  // From Encode(), then any words after it
};

class InstructionTest : public BaseCoreTest {
 protected:
  // The size of the values a fake device reads or writes on a port.
  enum class PortSize { kWord, kDword };

  // A fake device that feeds values to a port one at a time: whenever the port
  // is unlocked and its status is clear, it writes the next value and sets the
  // status, as WritePort or WritePort32 do. Update() must be called after
  // every cycle (see CyclesUntilIp).
  class PortFeeder {
   public:
    // `test` must outlive this.
    PortFeeder(InstructionTest* test, int port, PortSize size,
               std::vector<uint32_t> values)
        : test_(test), port_(port), size_(size), values_(std::move(values)) {}

    void Update() {
      if (next_ == static_cast<int>(values_.size())) {
        return;
      }
      const Port& port = test_->GetPort(port_);
      if (port.IsLocked() || port.GetStatus() != 0) {
        return;
      }
      if (size_ == PortSize::kWord) {
        test_->WritePort(port_, static_cast<uint16_t>(values_[next_]));
      } else {
        test_->WritePort32(port_, values_[next_]);
      }
      ++next_;
    }

   private:
    InstructionTest* const test_;
    const int port_;
    const PortSize size_;
    const std::vector<uint32_t> values_;
    int next_ = 0;
  };

  // A fake device that drains values from a port one at a time: whenever the
  // port is unlocked and its status is set, it reads the value and clears the
  // status, as ReadPort or ReadPort32 do, until it has read `count` values.
  // Update() must be called after every cycle (see CyclesUntilIp).
  class PortDrainer {
   public:
    // `test` must outlive this.
    PortDrainer(InstructionTest* test, int port, PortSize size, int count)
        : test_(test), port_(port), size_(size), count_(count) {}

    // Returns the values read so far.
    const std::vector<uint32_t>& GetValues() const { return values_; }

    void Update() {
      if (static_cast<int>(values_.size()) == count_) {
        return;
      }
      const Port& port = test_->GetPort(port_);
      if (port.IsLocked() || port.GetStatus() == 0) {
        return;
      }
      if (size_ == PortSize::kWord) {
        values_.push_back(test_->ReadPort(port_));
      } else {
        values_.push_back(test_->ReadPort32(port_));
      }
    }

   private:
    InstructionTest* const test_;
    const int port_;
    const PortSize size_;
    const int count_;
    std::vector<uint32_t> values_;
  };

  // Raises an interrupt on core 0 once, as soon as R7 reaches `r7`, such as to
  // interrupt a repeating instruction between words. Update() must be called
  // after every cycle (see CyclesUntilIp).
  class InterruptRaiser {
   public:
    // `test` must outlive this.
    InterruptRaiser(InstructionTest* test, int interrupt, uint16_t r7)
        : test_(test), interrupt_(interrupt), r7_(r7) {}

    void Update() {
      if (raised_) {
        return;
      }
      CoreState& state = test_->GetState();
      if (state.r7 != r7_) {
        return;
      }
      state.core.RaiseInterrupt(interrupt_);
      raised_ = true;
    }

   private:
    InstructionTest* const test_;
    const int interrupt_;
    const uint16_t r7_;
    bool raised_ = false;
  };

  InstructionTest()
      : BaseCoreTest(GetDefaultInstructionSetDef(),
                     GetDefaultInstructionSet()) {}

  // How RunCountCases passes the count.
  enum class CountArg {
    kRegister,   // In R2, as "$r"
    kImmediate,  // As the macro code for the count, such as "16"
  };

  // Runs `op` (such as "SHL.D") on each case's value with the case's count,
  // and expects the case's result, flags, and cycles. The value is in D0 for
  // an op ending in ".D", and in R0 otherwise. Every flag is clear before each
  // case.
  void RunCountCases(std::string_view op, CountArg arg,
                     absl::Span<const CountCase> cases) {
    ASSERT_TRUE(InitAndReset());
    auto& state = GetState();
    state.SetRegisters({{CpuCore::ST, 0}});
    const bool dword = op.ends_with(".D");

    std::vector<uint16_t> start_ips;
    std::vector<uint16_t> end_ips;
    for (const CountCase& c : cases) {
      if (dword) {
        state.code.AddValue(Encode("MOV.LD", 0, "$V")).AddValue32(c.value);
      } else {
        state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
            .AddValue(static_cast<uint16_t>(c.value));
      }
      if (arg == CountArg::kRegister) {
        state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v"))
            .AddValue(c.count);
      }
      state.code.AddValue(
          Encode("CLRF", CpuCore::Z | CpuCore::S | CpuCore::C | CpuCore::O));
      start_ips.push_back(state.code.AddNopGetAddress());
      if (arg == CountArg::kRegister) {
        state.code.AddValue(Encode(op, 0, {"$r", CpuCore::R2}));
      } else {
        const std::string count = absl::StrCat(c.count);
        state.code.AddValue(Encode(op, 0, Arg(count)));
      }
      end_ips.push_back(state.code.AddNopGetAddress());
    }
    state.code.AddValue(Encode("HALT"));

    for (int i = 0; i < static_cast<int>(cases.size()); ++i) {
      const CountCase& c = cases[i];
      SCOPED_TRACE(absl::StrCat(op, " ", c.value,
                                arg == CountArg::kRegister ? ", R2=" : ", ",
                                c.count));
      ASSERT_TRUE(ExecuteUntilIp(start_ips[i]));
      EXPECT_EQ(CyclesUntilIp(end_ips[i]), c.cycles);
      EXPECT_EQ(dword ? state.d0() : state.r0, c.result);
      EXPECT_EQ(state.st, c.st);
    }
  }

  // Adds each case's instruction to the code after whatever is there, then
  // runs them in turn and expects each one's cycles. Registers and memory are
  // whatever the test (and the cases before) left them, so each case's
  // instruction must fall through to the next. Call after InitAndReset().
  void RunCycleCases(absl::Span<const CycleCase> cases) {
    auto& state = GetState();

    // Each case starts at a NOP, and ends at the next case's NOP, so timing
    // one case leaves the core ready to time the next.
    std::vector<uint16_t> ips;
    for (const CycleCase& c : cases) {
      ips.push_back(state.code.AddNopGetAddress());
      for (uint16_t word : c.code) {
        state.code.AddValue(word);
      }
    }
    ips.push_back(state.code.AddNopGetAddress());
    state.code.AddValue(Encode("HALT"));

    ASSERT_TRUE(ExecuteUntilIp(ips[0]));
    for (int i = 0; i < static_cast<int>(cases.size()); ++i) {
      SCOPED_TRACE(cases[i].name);
      EXPECT_EQ(CyclesUntilIp(ips[i + 1]), cases[i].cycles);
    }
  }

  // Writes `value` to word 0 of the port and sets the port status, as a device
  // would. The port must not be locked.
  void WritePort(int port, uint16_t value) {
    auto lock = LockPort(port);
    CHECK(lock->IsLocked());
    GetPort(port).StoreWord(*lock, Port::S, value);
  }

  // Writes the low word of `value` to word 0 of the port and the high word to
  // word 1, and sets the port status, as a device would. The port must not be
  // locked.
  void WritePort32(int port_index, uint32_t value) {
    auto lock = LockPort(port_index);
    CHECK(lock->IsLocked());
    Port& port = GetPort(port_index);
    port.StoreWord(*lock, Port::A, value & 0xFFFF);
    port.StoreWord(*lock, Port::S, value >> 16);
  }

  // Reads word 0 of the port and clears the port status, as a device would.
  // The port must not be locked.
  uint16_t ReadPort(int port) {
    auto lock = LockPort(port);
    CHECK(lock->IsLocked());
    uint16_t value = 0;
    GetPort(port).LoadWord(*lock, Port::S, value);
    return value;
  }

  // Reads word 0 of the port as the low word and word 1 as the high word, and
  // clears the port status, as a device would. The port must not be locked.
  uint32_t ReadPort32(int port_index) {
    auto lock = LockPort(port_index);
    CHECK(lock->IsLocked());
    Port& port = GetPort(port_index);
    uint16_t low = 0;
    uint16_t high = 0;
    port.LoadWord(*lock, Port::A, low);
    port.LoadWord(*lock, Port::S, high);
    return low | (static_cast<uint32_t>(high) << 16);
  }
};

}  // namespace oz3

#endif  // OZ3_INSTRUCTION_SETS_INSTRUCTION_TEST_H_
