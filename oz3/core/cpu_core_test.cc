// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/cpu_core.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core_config.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/memory_bank_config.h"
#include "oz3/core/processor.h"
#include "oz3/core/processor_config.h"

namespace oz3 {
namespace {

using ::testing::IsEmpty;

enum MicroTestOp : uint8_t {
  kTestOp_NOP = 0,
  kTestOp_WAIT,
  kTestOp_HALT,
  kTestOp_LWORD,
  kTestOp_LDWORD,
  kTestOp_LCODE,
  kTestOp_LSTACK,
  kTestOp_LDATA,
  kTestOp_LEXTRA,
  kTestOp_SCODE,
  kTestOp_SSTACK,
  kTestOp_SDATA,
  kTestOp_SEXTRA,
  kTestOp_MOVST,
  kTestOp_MOVI,
  kTestOp_ADD,
  kTestOp_SUB,
  kTestOp_ADDI,
  kTestOp_SUBI,
};

const InstructionDef kMicroTestInstructions[] = {
    {kTestOp_NOP, {"NOP"}, "UL;"},
    {kTestOp_WAIT,
     {"WAIT", kArgWordRegA},
     "UL;"
     "WAIT(a);"},
    {kTestOp_HALT,
     {"HALT"},
     "UL;"
     "HALT;"},
    {kTestOp_LWORD,
     {"LCODE", kArgWordRegA},
     "LD(a);"
     "UL;"},
    {kTestOp_LDWORD,
     {"LCODE", kArgDwordRegA},
     "LD(a0);"
     "LD(a1);"
     "UL;"},
    {kTestOp_LCODE,
     {"LCODE", kArgWordRegA, kArgImmValue5},
     "ADR(C1);"
     "LD(a);"
     "UL;"},
    {kTestOp_LSTACK,
     {"LSTACK", kArgWordRegA, kArgImmValue5},
     "UL;"
     "LK(STACK);"
     "ADR(C1);"
     "LD(a);"
     "UL;"},
    {kTestOp_LDATA,
     {"LDATA", kArgWordRegA, kArgImmValue5},
     "UL;"
     "LK(DATA);"
     "ADR(C1);"
     "LD(a);"
     "UL;"},
    {kTestOp_LEXTRA,
     {"LEXTRA", kArgWordRegA, kArgImmValue5},
     "UL;"
     "LK(EXTRA);"
     "ADR(C1);"
     "LD(a);"
     "UL;"},
    {kTestOp_SCODE,
     {"SCODE", kArgImmValue5, kArgWordRegB},
     "ADR(C0);"
     "ST(b);"
     "UL;"},
    {kTestOp_SSTACK,
     {"SSTACK", kArgImmValue5, kArgWordRegB},
     "UL;"
     "LK(STACK);"
     "ADR(C0);"
     "ST(b);"
     "UL;"},
    {kTestOp_SDATA,
     {"SDATA", kArgImmValue5, kArgWordRegB},
     "UL;"
     "LK(DATA);"
     "ADR(C0);"
     "ST(b);"
     "UL;"},
    {kTestOp_SEXTRA,
     {"SEXTRA", kArgImmValue5, kArgWordRegB},
     "UL;"
     "LK(EXTRA);"
     "ADR(C0);"
     "ST(b);"
     "UL;"},
    {kTestOp_MOVST,
     {"MOVST", kArgImmValue5, kArgWordRegB},
     "UL;"
     "MOV(C1,b);"
     "LK(DATA);"
     "ADR(C0);"
     "ST(C1);"
     "UL;"},
    {kTestOp_MOVI,
     {"MOVI", kArgWordRegA},
     "UL;"
     "MOVI(a,42);"},
    {kTestOp_ADD,
     {"ADD", kArgWordRegA, kArgWordRegB},
     "UL;"
     "ADD(a,b);"},
    {kTestOp_SUB,
     {"SUB", kArgWordRegA, kArgWordRegB},
     "UL;"
     "SUB(a,b);"},
    {kTestOp_ADDI,
     {"ADDI", kArgWordRegA},
     "UL;"
     "ADDI(a,10);"},
    {kTestOp_SUBI,
     {"SUBI", kArgWordRegA},
     "UL;"
     "ADDI(a,-10);"},
};

// Helper class to fetch and update the state of a CpuCore.
struct CoreState {
  explicit CoreState(CpuCore& core) : core(core) { Update(); }

  void ResetCore(CpuCore::ResetParams reset_params = {}) {
    auto lock = core.Lock();
    EXPECT_TRUE(lock->IsLocked());
    core.Reset(*lock, reset_params);
    lock.reset();
  }

  void Update() {
    core.GetRegisters(r);
    code_bank = core.GetMemoryBank(CpuCore::CODE);
    stack_bank = core.GetMemoryBank(CpuCore::STACK);
    data_bank = core.GetMemoryBank(CpuCore::DATA);
    extra_bank = core.GetMemoryBank(CpuCore::EXTRA);
  }

  CpuCore& core;

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
  const uint16_t& c0 = r[CpuCore::C0];
  const uint16_t& c1 = r[CpuCore::C1];
  uint32_t cd0() const {
    return static_cast<uint32_t>(r[CpuCore::CD]) |
           (static_cast<uint32_t>(r[CpuCore::CD + 1]) << 16);
  }
  const uint16_t& pc = r[CpuCore::PC];
  const uint16_t& sp = r[CpuCore::SP];
  const uint16_t& dp = r[CpuCore::DP];
  uint32_t sd() const {
    return static_cast<uint32_t>(r[CpuCore::SD]) |
           (static_cast<uint32_t>(r[CpuCore::SD + 1]) << 16);
  }
  const uint16_t& st = r[CpuCore::ST];
  const uint16_t& bm = r[CpuCore::BM];

  MemoryBank* code_bank = nullptr;
  MemoryBank* stack_bank = nullptr;
  MemoryBank* data_bank = nullptr;
  MemoryBank* extra_bank = nullptr;
};

// Helper class to sequentially write code and read/write data ina MemoryBank.
class MemAccessor {
 public:
  MemAccessor(MemoryBank& memory_bank, uint16_t address = 0)
      : mem_(memory_bank.GetMem(0, kMemoryBankMaxSize)), address_(address) {}

  MemAccessor& SetAddress(uint16_t address) {
    address_ = address;
    return *this;
  }

  void AddValue(uint16_t value) { mem_[address_++] = value; }

  void AddCode(uint8_t op, uint16_t a = 0, uint16_t b = 0) {
    absl::Span<const InstructionDef> instructions(kMicroTestInstructions);
    auto it =
        std::find_if(instructions.begin(), instructions.end(),
                     [op](const InstructionDef& def) { return def.op == op; });
    CHECK(it != instructions.end()) << "Unknown op: " << static_cast<int>(op);
    mem_[address_++] = it->Encode(a, b);
  }

  uint16_t GetValue() { return mem_[address_++]; }

 private:
  absl::Span<uint16_t> mem_;
  uint16_t address_;
};

TEST(CpuCoreTest, CpuCoreInitialState) {
  CpuCore core(CpuCoreConfig::Default());

  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);

  CoreState state(core);
  for (int i = 0; i < CpuCore::kRegisterCount; ++i) {
    EXPECT_EQ(state.r[i], 0) << "r[" << i << "]";
  }
  EXPECT_EQ(state.code_bank, nullptr);
  EXPECT_EQ(state.stack_bank, nullptr);
  EXPECT_EQ(state.data_bank, nullptr);
  EXPECT_EQ(state.extra_bank, nullptr);
}

TEST(CpuCoreTest, BanksSetAfterAttachProcessor) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  EXPECT_EQ(state.code_bank, processor.GetMemory(0));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(0));
  EXPECT_EQ(state.data_bank, processor.GetMemory(0));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(0));
}

TEST(CpuCoreTest, ResetStartsCoreRunning) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);

  CoreState state(core);
  state.ResetCore();
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);

  state.Update();
  for (int i = 0; i < CpuCore::kRegisterCount; ++i) {
    EXPECT_EQ(state.r[i], 0) << "r[" << i << "]";
  }
}

TEST(CpuCoreTest, ResetWithSetRegisters) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);

  CpuCore::Banks banks = {.code = 1, .stack = 2, .data = 3, .extra = 4};
  state.ResetCore(CpuCore::ResetParams{.mask = CpuCore::ResetParams::ALL,
                                       .bm = banks.ToWord(),
                                       .pc = 5,
                                       .sp = 6,
                                       .dp = 7});

  state.Update();
  EXPECT_EQ(state.bm, banks.ToWord());
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.sp, 6);
  EXPECT_EQ(state.dp, 7);
  EXPECT_EQ(state.code_bank, processor.GetMemory(1));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(2));
  EXPECT_EQ(state.data_bank, processor.GetMemory(3));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(4));

  // Change only the code bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::BC,
      .bm = CpuCore::Banks::Default().SetCode(15).ToWord()});
  state.Update();
  EXPECT_EQ(state.bm, banks.SetCode(15).ToWord());
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.sp, 6);
  EXPECT_EQ(state.dp, 7);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(2));
  EXPECT_EQ(state.data_bank, processor.GetMemory(3));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(4));

  // Change only the stack bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::BS,
      .bm = CpuCore::Banks::Default().SetStack(14).ToWord()});
  state.Update();
  EXPECT_EQ(state.bm, banks.SetStack(14).ToWord());
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.sp, 6);
  EXPECT_EQ(state.dp, 7);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(3));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(4));

  // Change only the data bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::BD,
      .bm = CpuCore::Banks::Default().SetData(13).ToWord()});
  state.Update();
  EXPECT_EQ(state.bm, banks.SetData(13).ToWord());
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.sp, 6);
  EXPECT_EQ(state.dp, 7);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(13));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(4));

  // Change only the extra bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::BE,
      .bm = CpuCore::Banks::Default().SetExtra(12).ToWord()});
  state.Update();
  EXPECT_EQ(state.bm, banks.SetExtra(12).ToWord());
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.sp, 6);
  EXPECT_EQ(state.dp, 7);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(13));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(12));

  // Change only the program counter and reset again.
  state.ResetCore(
      CpuCore::ResetParams{.mask = CpuCore::ResetParams::PC, .pc = 10});
  state.Update();
  EXPECT_EQ(state.bm, banks.ToWord());
  EXPECT_EQ(state.pc, 10);
  EXPECT_EQ(state.sp, 6);
  EXPECT_EQ(state.dp, 7);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(13));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(12));

  // Change only the stack pointer and reset again.
  state.ResetCore(
      CpuCore::ResetParams{.mask = CpuCore::ResetParams::SP, .sp = 9});
  state.Update();
  EXPECT_EQ(state.bm, banks.ToWord());
  EXPECT_EQ(state.pc, 10);
  EXPECT_EQ(state.sp, 9);
  EXPECT_EQ(state.dp, 7);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(13));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(12));

  // Change only the data pointer and reset again.
  state.ResetCore(
      CpuCore::ResetParams{.mask = CpuCore::ResetParams::DP, .dp = 8});
  state.Update();
  EXPECT_EQ(state.bm, banks.ToWord());
  EXPECT_EQ(state.pc, 10);
  EXPECT_EQ(state.sp, 9);
  EXPECT_EQ(state.dp, 8);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(13));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(12));
}

TEST(CpuCoreTest, RunsAfterReset) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  // This runs the CPU to start executing the first instruction (NOP) until it
  // hits its first synchronization point which is UL.
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_TRUE(memory_bank.IsLocked());
  state.Update();
  EXPECT_EQ(state.pc, 1);

  // The processor has only advanced one cycle
  EXPECT_EQ(processor.GetCycles(), 1);

  // Executing one cycle at a time, will not hit the processor (no cycles
  // increase) until the processor reaches the CpuCore's cycles.
  while (core.GetCycles() > processor.GetCycles()) {
    processor.Execute(1);
    EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction)
        << "Cycles: " << processor.GetCycles();
    EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles)
        << "Cycles: " << processor.GetCycles();
    EXPECT_TRUE(memory_bank.IsLocked()) << "Cycles: " << processor.GetCycles();
  }

  // Executing one more cycle will trigger the next instruction (another NOP).
  // This will result in a back to back unlock and lock, which succeed trivially
  // as there is only one core.
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2);
  EXPECT_TRUE(memory_bank.IsLocked());
  state.Update();
  EXPECT_EQ(state.pc, 2);
}

TEST(CpuCoreTest, CoreLockPreventsExecution) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);

  auto lock = core.Lock();
  EXPECT_TRUE(lock->IsLocked());
  core.Reset(*lock, CpuCore::ResetParams{});

  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);

  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);
  EXPECT_EQ(core.GetCycles(), 1);
  EXPECT_FALSE(memory_bank.IsLocked());

  lock.reset();

  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_TRUE(memory_bank.IsLocked());
}

TEST(CpuCoreTest, CoreLockDeferredUntilEndOfInstruction) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);

  // We are now running an instruction, so the core cannot be locked until it
  // completes.
  auto lock = core.Lock();
  EXPECT_FALSE(lock->IsLocked());

  // Run enough cycles to start the next instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_TRUE(lock->IsLocked());
}

TEST(CpuCoreTest, MemoryLockBlocksFetch) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  // Lock the memory bank, which will prevent the core from fetching the next
  // instruction.
  auto memory_lock = memory_bank.Lock();
  EXPECT_TRUE(memory_lock->IsLocked());

  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kFetchInstruction);
  EXPECT_EQ(core.GetCycles(), 1);

  // Unlock the memory bank, which will allow the core to run
  memory_lock.reset();
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_TRUE(memory_bank.IsLocked());
}

TEST(CpuCoreTest, MultiCoreRoundRobinsExecution) {
  Processor processor(ProcessorConfig::MultiCore(3));
  MemoryBank& memory_bank = *processor.GetMemory(0);
  CoreState state0(*processor.GetCore(0));
  state0.ResetCore({.pc = 10});
  CoreState state1(*processor.GetCore(1));
  state1.ResetCore({.pc = 20});
  CoreState state2(*processor.GetCore(2));
  state2.ResetCore({.pc = 30});

  // Run the processor for one fetch and decode cycle. This will result in the
  // first core locking memory and decoding the first NOP. The other cores
  // should be blocked waiting on the first core to release its lock on the CODE
  // memory bank.
  processor.Execute(kCpuCoreFetchAndDecodeCycles);
  state0.Update();
  EXPECT_EQ(state0.pc, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kRunInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 20);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 30);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // Run the processor for one more cycle. This will allow the first core to
  // finish executing the NOP, and the second core to lock memory and decode the
  // first NOP. This blocks the first core from fetching the next instruction.
  processor.Execute(1);
  state0.Update();
  EXPECT_EQ(state0.pc, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kFetchInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kRunInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 30);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // Run the processor for a fetch and decode cycle. This will allow the second
  // core to finish executing the NOP, and the third core to lock memory and
  // decode the first NOP. This blocks the second core from fetching the next
  // instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles);
  state0.Update();
  EXPECT_EQ(state0.pc, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kFetchInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 1);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 31);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kRunInstruction);

  // One more round will cycle back to the first core. However, as the first
  // core executed first, it needs to wait another cycle to gain the lock.
  processor.Execute(kCpuCoreFetchAndDecodeCycles);
  state0.Update();
  EXPECT_EQ(state0.pc, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kFetchInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 1);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 31);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 1);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // One more cycle gives the first core the lock, and it can execute the next
  // instruction.
  processor.Execute(1);
  state0.Update();
  EXPECT_EQ(state0.pc, 12);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kRunInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 2);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 31);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 2);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // Finish fetch and decode for the first core, to catch all cores up to the
  // first core.
  processor.Execute(kCpuCoreFetchAndDecodeCycles - 1);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // Every kCpuCoreFetchAndDecodeCycles * 3 + 1 cycles will result in a full
  // round. Run for 5 more rounds to verify.
  const int start_cycles = kCpuCoreFetchAndDecodeCycles * 4 + 1;
  const int round_cycles = kCpuCoreFetchAndDecodeCycles * 3 + 1;
  processor.Execute(round_cycles * 5);
  state0.Update();
  EXPECT_EQ(state0.pc, 17);
  EXPECT_EQ(state0.core.GetCycles(), start_cycles + round_cycles * 5);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kRunInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 26);
  EXPECT_EQ(state1.core.GetCycles(), start_cycles + round_cycles * 5);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 36);
  EXPECT_EQ(state2.core.GetCycles(), start_cycles + round_cycles * 5);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);
}

TEST(CpuCoreTest, MicroTestInstructionsCompile) {
  InstructionMicroCodes micro_codes;
  std::string error;
  EXPECT_TRUE(micro_codes.Compile(kMicroTestInstructions, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(CpuCoreTest, LdOpInFetchExtendsCodeSize) {
  Processor processor(ProcessorConfig::OneCore(kMicroTestInstructions));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  MemAccessor mem(memory_bank);
  mem.AddCode(kTestOp_LWORD, CpuCore::R0);
  mem.AddValue(42);
  mem.AddCode(kTestOp_LDWORD, CpuCore::D0);
  mem.AddValue(0);
  mem.AddValue(1);
  mem.AddCode(kTestOp_HALT);

  // Run the processor for one fetch and decode cycle. Since the LWORD OP does
  // an LD microcode before unlocking the CODE memory bank, it will execute as
  // well.
  processor.Execute(1);
  state.Update();
  EXPECT_EQ(state.pc, 2);  // Advances 2 due to LD
  EXPECT_EQ(state.r0, 42);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);

  // Run the processor for one fetch and decode cycle. Since the LWORD OP does
  // an LD microcode before unlocking the CODE memory bank, it will execute as
  // well.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 5);  // Advances 3 due to two LDs
  EXPECT_EQ(state.d0(), 0x10000);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 3);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);

  // Complete this instruction and run the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 3);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, HaltOp) {
  Processor processor(ProcessorConfig::OneCore(kMicroTestInstructions));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  MemAccessor mem(memory_bank);
  mem.AddCode(kTestOp_HALT);

  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);

  // The core should not execute any more instructions.
  processor.Execute(100);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 101);
  state.Update();
  EXPECT_EQ(state.pc, 1);
}

TEST(CpuCoreTest, WaitOp) {
  Processor processor(ProcessorConfig::OneCore(kMicroTestInstructions));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  auto lock = core.Lock();
  core.SetWordRegister(*lock, CpuCore::R0, 0);
  core.SetWordRegister(*lock, CpuCore::R1, 1);
  core.SetWordRegister(*lock, CpuCore::R2, kCpuCoreFetchAndDecodeCycles);
  core.SetWordRegister(*lock, CpuCore::R3, kCpuCoreFetchAndDecodeCycles + 1);
  core.SetWordRegister(*lock, CpuCore::R4, kCpuCoreFetchAndDecodeCycles + 2);
  lock.reset();

  MemAccessor mem(memory_bank);
  mem.AddCode(kTestOp_WAIT, CpuCore::R0);
  mem.AddCode(kTestOp_WAIT, CpuCore::R1);
  mem.AddCode(kTestOp_WAIT, CpuCore::R2);
  mem.AddCode(kTestOp_WAIT, CpuCore::R3);
  mem.AddCode(kTestOp_WAIT, CpuCore::R4);
  mem.AddCode(kTestOp_HALT);

  // Execute through the first three WAITs which should actually put the CPU
  // into a wait state, as the wait times are too small.
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 4);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4);

  // The next wait (which is already fetched and decoded) should take exactly
  // one cycle, and then return the CPU to the kStartInstruction state.
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);

  // The final wait takes two cycles, so we wait 1 extra cycle to get through
  // the fetch+decode and the first cycle of waiting.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kWaiting);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 2);

  // The final cycle of waiting will return the CPU to the kStartInstruction
  // state.
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 3);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 4);
}

TEST(CpuCoreTest, AdrLdStOps) {
  Processor processor(ProcessorConfig::OneCore(kMicroTestInstructions)
                          .SetMemoryBank(1, MemoryBankConfig::MaxRam())
                          .SetMemoryBank(2, MemoryBankConfig::MaxRam())
                          .SetMemoryBank(3, MemoryBankConfig::MaxRam()));
  CpuCore& core = *processor.GetCore(0);
  CpuCore::Banks banks = {.code = 0, .stack = 1, .data = 2, .extra = 3};
  MemoryBank& code_bank = *processor.GetMemory(banks.code);
  MemoryBank& stack_bank = *processor.GetMemory(banks.stack);
  MemoryBank& data_bank = *processor.GetMemory(banks.data);
  MemoryBank& extra_bank = *processor.GetMemory(banks.extra);
  CoreState state(core);
  state.ResetCore({.bm = banks.ToWord()});

  MemAccessor code_mem(code_bank);
  code_mem.AddCode(kTestOp_LCODE, CpuCore::R0, 15);
  code_mem.AddCode(kTestOp_LSTACK, CpuCore::R1, 15);
  code_mem.AddCode(kTestOp_LDATA, CpuCore::R2, 15);
  code_mem.AddCode(kTestOp_LEXTRA, CpuCore::R3, 15);
  code_mem.AddCode(kTestOp_SCODE, 20, CpuCore::R1);
  code_mem.AddCode(kTestOp_SSTACK, 20, CpuCore::R2);
  code_mem.AddCode(kTestOp_SDATA, 20, CpuCore::R3);
  code_mem.AddCode(kTestOp_SEXTRA, 20, CpuCore::R0);
  code_mem.AddCode(kTestOp_HALT);

  code_mem.SetAddress(15).AddValue(0x1234);

  MemAccessor stack_mem(stack_bank, 15);
  stack_mem.AddValue(0x5678);

  MemAccessor data_mem(data_bank, 15);
  data_mem.AddValue(0x9abc);

  MemAccessor extra_mem(extra_bank, 15);
  extra_mem.AddValue(0xdef0);

  // Execute the LCODE instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 2);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x1234);

  // Execute the LSTACK instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x5678);

  // Execute the LDATA instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0x9abc);

  // Execute the LEXTRA instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0xdef0);

  // Execute the SCODE instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 10);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(code_mem.SetAddress(20).GetValue(), 0x5678);

  // Execute the SSTACK instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 12);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(stack_mem.SetAddress(20).GetValue(), 0x9abc);

  // Execute the SDATA instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 7 + 14);
  state.Update();
  EXPECT_EQ(state.pc, 7);
  EXPECT_EQ(data_mem.SetAddress(20).GetValue(), 0xdef0);

  // Execute the SEXTRA instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 16);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(extra_mem.SetAddress(20).GetValue(), 0x1234);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, LkWhenLocked) {
  Processor processor(ProcessorConfig::OneCore(kMicroTestInstructions)
                          .SetMemoryBank(1, MemoryBankConfig::MaxRam()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  CpuCore::Banks banks = {.code = 0, .data = 1};
  MemoryBank& code_bank = *processor.GetMemory(banks.code);
  MemoryBank& data_bank = *processor.GetMemory(banks.data);
  state.ResetCore({.bm = banks.ToWord()});

  MemAccessor code_mem(code_bank);
  code_mem.AddCode(kTestOp_LDATA, CpuCore::R0, 10);
  code_mem.AddCode(kTestOp_HALT);

  MemAccessor data_mem(data_bank, 10);
  data_mem.SetAddress(10).AddValue(0x1234);

  // Lock the memory bank, which will prevent the core from fetching the next
  // instruction.
  auto data_lock = data_bank.Lock();
  EXPECT_TRUE(data_lock->IsLocked());

  processor.Execute(kCpuCoreFetchAndDecodeCycles + 10);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0);

  // Unlock the memory bank, which will allow the core to run
  data_lock.reset();
  processor.Execute(2);  // ADR, LD, UL
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x1234);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, MovStOp) {
  Processor processor(ProcessorConfig::OneCore(kMicroTestInstructions)
                          .SetMemoryBank(1, MemoryBankConfig::MaxRam()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  CpuCore::Banks banks = {.code = 0, .data = 1};
  MemoryBank& code_bank = *processor.GetMemory(banks.code);
  MemoryBank& data_bank = *processor.GetMemory(banks.data);
  state.ResetCore({.bm = banks.ToWord()});

  auto lock = core.Lock();
  EXPECT_TRUE(lock->IsLocked());
  core.SetWordRegister(*lock, CpuCore::R0, 42);
  lock.reset();

  MemAccessor code_mem(code_bank);
  code_mem.AddCode(kTestOp_MOVST, 10, CpuCore::R0);
  code_mem.AddCode(kTestOp_HALT);

  MemAccessor data_mem(data_bank, 10);
  data_mem.SetAddress(10).AddValue(0x1234);

  // Execute through the MOV instruction. This should not execute the LK.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_FALSE(data_bank.IsLocked());

  // Execute the LK instruction, and everything up to the UL.
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 3);
  EXPECT_TRUE(data_bank.IsLocked());
  EXPECT_EQ(data_mem.SetAddress(10).GetValue(), 42);

  // Process the UL instruction, and fetch the HALT instruction
  processor.Execute(2);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 3);
  EXPECT_FALSE(data_bank.IsLocked());

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, MoviOp) {
  Processor processor(ProcessorConfig::OneCore(kMicroTestInstructions));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_MOVI, CpuCore::R3);
  mem.AddCode(kTestOp_HALT);

  // Execute the MOVI instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r3, 42);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

}  // namespace
}  // namespace oz3
