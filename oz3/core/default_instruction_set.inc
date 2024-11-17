constexpr oz3::InstructionDef kInstructions[] = {
  {.op=0, .op_name="NOP",
   .code="UL;"},
  {.op=1, .op_name="HALT",
   .code="UL;HALT;"},
  {.op=2, .op_name="WAIT", .arg_source ="$r", .arg1={oz3::ArgType::kWordReg, 3},
   .code="UL;WAIT(a);"},
};

constexpr oz3::InstructionSetDef kInstructionSet = {
  .instructions=kInstructions,
};
