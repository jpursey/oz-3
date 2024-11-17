// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_def_exporter.h"

#include "absl/log/log.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_split.h"
#include "oz3/core/cpu_core.h"

namespace oz3 {

namespace {

std::string_view ArgTypeConstName(ArgType type) {
  switch (type) {
    case ArgType::kNone:
      return "oz3::ArgType::kNone";
    case ArgType::kImmediate:
      return "oz3::ArgType::kImmediate";
    case ArgType::kWordReg:
      return "oz3::ArgType::kWordReg";
    case ArgType::kDwordReg:
      return "oz3::ArgType::kDwordReg";
    case ArgType::kMacro:
      return "oz3::ArgType::kMacro";
  }
  LOG(FATAL) << "Unknown ArgType: " << static_cast<int>(type);
  return "kNone";
}

class InstructionDefExporter {
 public:
  InstructionDefExporter(std::string_view const_prefix,
                         const InstructionSetDef& instruction_set)
      : const_prefix_(const_prefix), instruction_set_(instruction_set) {}

  std::string Export();

 private:
  void ExportArgumentField(std::string field_name, const Argument& arg);
  void ExportMacroCode(const MacroDef& macro, const MacroCodeDef& code);
  void ExportMacro(const MacroDef& macro);
  void ExportInstruction(const InstructionDef& instruction);
  void ExportMicrocode(std::string_view code);

  std::string_view const_prefix_;
  const InstructionSetDef& instruction_set_;
  std::string result_;
};

void InstructionDefExporter::ExportArgumentField(std::string field_name,
                                                 const Argument& arg) {
  if (arg.type == ArgType::kNone) {
    return;
  }
  absl::StrAppend(&result_, ", .", field_name, "={", ArgTypeConstName(arg.type),
                  ", ", arg.size, "}");
}

void InstructionDefExporter::ExportMacroCode(const MacroDef& macro,
                                             const MacroCodeDef& code) {
  absl::StrAppend(&result_, "  {.source=\"", code.source, "\", .prefix={",
                  code.prefix.value, ", ", code.prefix.size, "}");
  ExportArgumentField("arg", code.arg);
  std::string_view ret_name;
  switch (macro.ret) {
    case ArgType::kWordReg:
      if (code.ret == CpuCore::P0) {
        ret_name = (macro.param == ArgType::kWordReg ? "P" : "P0");
      } else if (code.ret == CpuCore::P1) {
        ret_name = "P1";
      } else {
        ret_name = CpuCore::GetWordRegName(code.ret);
      }
      break;
    case ArgType::kDwordReg:
      if (code.ret == CpuCore::P) {
        ret_name = "P";
      } else {
        ret_name = CpuCore::GetDwordRegName(code.ret);
      }
      break;
  }
  if (!ret_name.empty()) {
    absl::StrAppend(&result_, ", .ret=CpuCore::", ret_name);
  }
  absl::StrAppend(&result_, ",\n   .code=");
  ExportMicrocode(code.code);
  absl::StrAppend(&result_, "},\n");
}

void InstructionDefExporter::ExportMacro(const MacroDef& macro) {
  absl::StrAppend(&result_, "  {.name=\"", macro.name, "\"");
  if (macro.param != ArgType::kNone) {
    absl::StrAppend(&result_, ", .param=", ArgTypeConstName(macro.param));
  }
  if (macro.ret != ArgType::kNone) {
    absl::StrAppend(&result_, ", .ret=", ArgTypeConstName(macro.ret));
  }
  absl::StrAppend(&result_, ", .size=", macro.size, ", .code=", const_prefix_,
                  "MacroCode_", macro.name, "},\n");
}

void InstructionDefExporter::ExportInstruction(
    const InstructionDef& instruction) {
  absl::StrAppend(&result_, "  {.op=", instruction.op, ", .op_name=\"",
                  instruction.op_name, "\"");
  if (!instruction.arg_source.empty()) {
    absl::StrAppend(&result_, ", .arg_source =\"", instruction.arg_source,
                    "\"");
  }
  ExportArgumentField("arg1", instruction.arg1);
  ExportArgumentField("arg2", instruction.arg2);
  absl::StrAppend(&result_, ",\n   .code=");
  ExportMicrocode(instruction.code);
  absl::StrAppend(&result_, "},\n");
}

void InstructionDefExporter::ExportMicrocode(std::string_view code) {
  std::vector<std::string_view> codes =
      absl::StrSplit(code, ';', absl::SkipEmpty());
  absl::StrAppend(&result_, "\"");
  int size = 0;
  for (const auto& code : codes) {
    if (size + code.size() > 60) {
      absl::StrAppend(&result_, "\"\n   \"");
      size = 0;
    }
    absl::StrAppend(&result_, code, ";");
    size += code.size() + 1;
  }
  absl::StrAppend(&result_, "\"");
}

std::string InstructionDefExporter::Export() {
  const bool has_macros = !instruction_set_.macros.empty();
  if (has_macros) {
    for (const auto& macro : instruction_set_.macros) {
      absl::StrAppend(&result_, "constexpr oz3::MacroCodeDef ", const_prefix_,
                      "MacroCode_", macro.name, "[] = {\n");
      for (const auto& code : macro.code) {
        ExportMacroCode(macro, code);
      }
      absl::StrAppend(&result_, "};\n\n");
    }
    absl::StrAppend(&result_, "constexpr oz3::MacroDef ", const_prefix_,
                    "Macros[] = {\n");
    for (const auto& macro : instruction_set_.macros) {
      ExportMacro(macro);
    }
    absl::StrAppend(&result_, "};\n\n");
  }
  const bool has_instructions = !instruction_set_.instructions.empty();
  if (has_instructions) {
    absl::StrAppend(&result_, "constexpr oz3::InstructionDef ", const_prefix_,
                    "Instructions[] = {\n");
    for (const auto& instruction : instruction_set_.instructions) {
      ExportInstruction(instruction);
    }
    absl::StrAppend(&result_, "};\n\n");
  }
  absl::StrAppend(&result_, "constexpr oz3::InstructionSetDef ", const_prefix_,
                  "InstructionSet = {\n");
  if (has_instructions) {
    absl::StrAppend(&result_, "  .instructions=", const_prefix_,
                    "Instructions,\n");
  }
  if (has_macros) {
    absl::StrAppend(&result_, "  .macros=", const_prefix_, "Macros\n");
  }
  absl::StrAppend(&result_, "};\n");
  return result_;
}

}  // namespace

bool ExportInstructionSetDef(gb::FileSystem& file_system,
                             std::string_view filename,
                             const InstructionSetDef& instruction_set,
                             std::string_view const_prefix) {
  InstructionDefExporter exporter(const_prefix, instruction_set);
  return file_system.WriteFile(filename, exporter.Export());
}

}  // namespace oz3