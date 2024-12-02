// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_BASE_CORE_TEST_H_
#define OZ3_CORE_BASE_CORE_TEST_H_

#include "absl/container/flat_hash_map.h"
#include "absl/log/log.h"
#include "gb/base/callback.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/processor.h"

namespace oz3 {

class BaseCoreTest : public testing::Test {
 protected:
  struct InitConfig {
    int num_cores = 1;
    int num_memory_banks = 1;
    int num_ports = 0;
  };

  struct RegisterValue {
    int index;
    uint16_t value;
  };

  // Helper class to sequentially write code and read/write data ina MemoryBank.
  class MemAccessor {
   public:
    MemAccessor() = default;
    MemAccessor(MemoryBank* memory_bank, uint16_t address = 0)
        : memory_bank_(memory_bank),
          mem_(memory_bank->GetMem(0, memory_bank->GetMemorySize())),
          address_(address) {
      CHECK(memory_bank->GetMemoryStart() == 0);
    }

    void SetBank(MemoryBank* memory_bank, uint16_t address = 0) {
      CHECK(memory_bank->GetMemoryStart() == 0);
      memory_bank_ = memory_bank;
      mem_ = memory_bank->GetMem(0, memory_bank->GetMemorySize());
      address_ = address;
    }

    MemoryBank* GetBank() { return memory_bank_; }
    const MemoryBank* GetBank() const { return memory_bank_; }

    uint16_t GetAddress() const { return address_; }

    MemAccessor& SetAddress(uint16_t address) {
      address_ = address;
      return *this;
    }

    MemAccessor& AddValue(uint16_t value) {
      mem_[address_++] = value;
      return *this;
    }

    uint16_t AddNopGetAddress() {
      mem_[address_++] = 0;
      return address_;
    }

    uint16_t GetValue() { return mem_[address_++]; }

   private:
    MemoryBank* memory_bank_ = nullptr;
    absl::Span<uint16_t> mem_;
    uint16_t address_ = 0;
  };

  // Helper class to fetch and update the state of a CpuCore.
  struct CoreState {
    explicit CoreState(CpuCore& core) : core(core) { Update(); }

    // Locks and then resets the core.
    void ResetCore(CpuCore::ResetParams reset_params = {});

    // Locks and then sets the specified register values.
    void SetRegisters(absl::Span<const RegisterValue> values);

    // Updates the cached state of the core. This is done automatically when the
    // Execute*() methods are called.
    void Update();

    // Direct access to the core
    CpuCore& core;

    // Cached state of the core
    CpuCore::Registers r;
    const uint16_t& r0 = r[CpuCore::R0];
    const uint16_t& r1 = r[CpuCore::R1];
    const uint16_t& r2 = r[CpuCore::R2];
    const uint16_t& r3 = r[CpuCore::R3];
    const uint16_t& r4 = r[CpuCore::R4];
    const uint16_t& r5 = r[CpuCore::R5];
    const uint16_t& r6 = r[CpuCore::R6];
    const uint16_t& r7 = r[CpuCore::R7];
    uint32_t d0() const {
      return static_cast<uint32_t>(r[CpuCore::D0]) |
             (static_cast<uint32_t>(r[CpuCore::D0 + 1]) << 16);
    }
    uint32_t d1() const {
      return static_cast<uint32_t>(r[CpuCore::D1]) |
             (static_cast<uint32_t>(r[CpuCore::D1 + 1]) << 16);
    }
    uint32_t d2() const {
      return static_cast<uint32_t>(r[CpuCore::D2]) |
             (static_cast<uint32_t>(r[CpuCore::D2 + 1]) << 16);
    }
    uint32_t d3() const {
      return static_cast<uint32_t>(r[CpuCore::D3]) |
             (static_cast<uint32_t>(r[CpuCore::D3 + 1]) << 16);
    }
    const uint16_t& bc = r[CpuCore::BC];
    const uint16_t& bs = r[CpuCore::BS];
    const uint16_t& bd = r[CpuCore::BD];
    const uint16_t& be = r[CpuCore::BE];
    const uint16_t& mb = r[CpuCore::MB];
    const uint16_t& ip = r[CpuCore::IP];
    const uint16_t& fp = r[CpuCore::FP];
    const uint16_t& sp = r[CpuCore::SP];

    const uint16_t& st = r[CpuCore::ST];

    const uint16_t& c0 = r[CpuCore::C0];
    const uint16_t& c1 = r[CpuCore::C1];
    const uint16_t& c2 = r[CpuCore::C2];

    // Accessors for the memory banks accessed by this core.
    MemAccessor code;
    MemAccessor stack;
    MemAccessor data;
    MemAccessor extra;
  };

  BaseCoreTest();
  BaseCoreTest(const InstructionSetDef& def,
               std::shared_ptr<const InstructionSet> instruction_set)
      : def_(def), instruction_set_(std::move(instruction_set)) {}
  BaseCoreTest(const BaseCoreTest&) = delete;
  BaseCoreTest& operator=(const BaseCoreTest&) = delete;
  ~BaseCoreTest() = default;

  // Initializes the processor and cores. This must be called first before any
  // other calls can be made.
  bool Init(InitConfig config = {});

  // Returns the core state for the specified core
  CoreState& GetState(int core_index = 0) {
    CHECK(core_index >= 0 && core_index < processor_->GetNumCores());
    return states_[core_index];
  }

  // Returns the memory bank for the specified index.
  MemAccessor GetMemory(int bank_index = 0) {
    CHECK(bank_index >= 0 && bank_index < kMaxMemoryBanks);
    return MemAccessor(processor_->GetMemory(bank_index));
  }

  // Returns the port for the specified index.
  Port& GetPort(int port_index) {
    CHECK(port_index >= 0 && port_index < processor_->GetNumPorts());
    return processor_->GetPort(port_index);
  }

  // Locks the port at the specified index.
  std::unique_ptr<Lock> LockPort(int port_index) {
    CHECK(port_index >= 0 && port_index < processor_->GetNumPorts());
    return processor_->LockPort(port_index);
  }

  // Encodes an instruction based on the instruction set.
  uint16_t Encode(uint8_t op, uint16_t a = 0, uint16_t b = 0);
  uint16_t Encode(std::string_view op_name, uint16_t a = 0, uint16_t b = 0);
  uint16_t Encode(std::string_view op_name, std::string_view macro_code,
                  uint16_t macro_arg = 0, uint16_t b = 0);
  uint16_t Encode(std::string_view op_name, uint16_t a,
                  std::string_view macro_code, uint16_t macro_arg = 0);

  // Returns the number of cycles executed so far by the processor.
  Cycles GetCycles() const { return processor_->GetCycles(); }

  // Executes the specified number of cycles.
  void Execute(Cycles cycles) { processor_->Execute(cycles); }

  // Executes the requested processor until a specified condition is met.
  bool ExecuteUntil(int core_index, gb::Callback<bool()> condition);
  bool ExecuteUntil(gb::Callback<bool()> condition) {
    return ExecuteUntil(0, std::move(condition));
  }
  bool ExecuteUntilIp(int core_index, uint16_t ip) {
    CHECK(core_index >= 0 && core_index < states_.size());
    auto& state = states_[core_index];
    return ExecuteUntil(core_index, [&] { return state.ip == ip; });
  }
  bool ExecuteUntilIp(uint16_t ip) { return ExecuteUntilIp(0, ip); }
  void ExecuteUntilHalt(int core_index = 0) {
    ExecuteUntil(core_index, [] { return false; });
  }

 private:
  const InstructionSetDef def_;
  std::shared_ptr<const InstructionSet> instruction_set_;
  absl::flat_hash_map<uint8_t, InstructionDef> instructions_by_op_;
  absl::flat_hash_map<std::string_view, InstructionDef> instructions_by_name_;
  absl::flat_hash_map<std::string_view, MacroDef> macros_;
  std::unique_ptr<Processor> processor_;
  std::vector<CoreState> states_;
};

}  // namespace oz3

#endif  // OZ3_CORE_BASE_CORE_TEST_H_
