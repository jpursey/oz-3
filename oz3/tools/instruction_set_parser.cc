// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_set_parser.h"

#include <memory>

#include "absl/log/log.h"
#include "absl/memory/memory.h"
#include "gb/file/path.h"
#include "gb/parse/lexer.h"

namespace oz3 {

namespace {

class Parser {
 public:
  static std::unique_ptr<Parser> Create(std::string_view filename,
                                        std::string text);

  bool Parse();

 private:
  Parser(std::unique_ptr<gb::Lexer> lexer, gb::LexerContentId content)
      : lexer_(std::move(lexer)), content_(content) {}

  gb::Token NextToken() { return lexer_->NextToken(content_); }

  bool Error(gb::Token token, std::string_view message) {
    LOG(ERROR) << lexer_->GetTokenLocation(token) << " \""
               << lexer_->GetTokenText(token) << "\": " << message;
    return false;
  }

  bool ParseMacro();
  bool ParseMacroCode();
  bool ParseInstruction();
  bool ParseCode();

  const std::unique_ptr<gb::Lexer> lexer_;
  const gb::LexerContentId content_;
};

std::unique_ptr<Parser> Parser::Create(std::string_view filename,
                                       std::string text) {
  static constexpr gb::Symbol kSymbols[] = {'(', ')', '{', '}', '@',
                                            '$', ':', ';', ',', '.'};
  static constexpr std::string_view kLineComments[] = {"#"};
  auto lexer = gb::Lexer::Create({
      .flags =
          {gb::kLexerFlags_AllPositiveIntegers, gb::LexerFlag::kBinaryIntegers,
           gb::LexerFlag::kHexUpperIntegers, gb::LexerFlag::kHexLowerIntegers,
           gb::LexerFlag::kDoubleQuoteString, gb::LexerFlag::kIdentLower,
           gb::LexerFlag::kIdentUpper, gb::LexerFlag::kIdentNonLeadUnderscore,
           gb::LexerFlag::kIdentNonLeadDigit},
      .binary_prefix = "b",
      .hex_prefix = "x",
      .line_comments = kLineComments,
      .symbols = kSymbols,
  });
  DCHECK(lexer != nullptr);

  gb::LexerContentId content = lexer->AddFileContent(filename, std::move(text));
  if (content == gb::kNoLexerContent) {
    return nullptr;
  }
  return absl::WrapUnique(new Parser(std::move(lexer), content));
}

bool Parser::Parse() {
  for (gb::Token token = NextToken(); token.GetType() != gb::kTokenEnd;
       token = NextToken()) {
    if (token.GetType() == gb::kTokenIdentifier &&
        token.GetString() == "MACRO") {
      if (!ParseMacro()) {
        return false;
      }
    } else if (token.GetType() == gb::kTokenIdentifier &&
               token.GetString() == "INSTRUCTION") {
      if (!ParseInstruction()) {
        return false;
      }
    } else {
      return Error(token, "Expected MACRO or INSTRUCTION");
    }
  }
  return true;
}

bool Parser::ParseMacro() {
  gb::Token token = NextToken();
  if (token.GetSymbol() == '(') {
    token = NextToken();
    if (token.GetType() != gb::kTokenIdentifier ||
        token.GetString() != "bits") {
      return Error(token, "Expected macro config parameter: bits");
    }
    token = NextToken();
    if (token.GetSymbol() != ':') {
      return Error(token, "Expected ':' after macro config parameter");
    }
    token = NextToken();
    if (token.GetType() != gb::kTokenInt) {
      return Error(token, "Expected integer value for bits");
    }
    token = NextToken();
    if (token.GetSymbol() != ')') {
      return Error(token, "Expected ')' after macro config value");
    }
    token = NextToken();
  }

  if (token.GetType() != gb::kTokenIdentifier) {
    return Error(token, "Expected macro name");
  }

  token = NextToken();
  if (token.GetSymbol() == '(') {
    token = NextToken();
    if (token.GetType() != gb::kTokenIdentifier) {
      return Error(token, "Expected macro parameter type: p or P");
    }
    if (token.GetString() != "p" && token.GetString() != "P") {
      return Error(token, "Invalid macro parameter type. It must be: p or P");
    }
    token = NextToken();
    if (token.GetSymbol() != ')') {
      return Error(token, "Expected ')' after macro parameter type");
    }
    token = NextToken();
  }
  if (token.GetSymbol() == ':') {
    token = NextToken();
    if (token.GetType() != gb::kTokenIdentifier) {
      return Error(token, "Expected macro return type: r or R");
    }
    if (token.GetString() != "r" && token.GetString() != "R") {
      return Error(token, "Invalid macro return type. It must be: r or R");
    }
    token = NextToken();
  }

  if (token.GetSymbol() != '{') {
    return Error(token, "Expected '{' for macro definition");
  }

  for (token = NextToken(); token.GetType() != gb::kTokenEnd;
       token = NextToken()) {
    if (token.GetSymbol() == '}') {
      return true;
    }
    if (token.GetType() != gb::kTokenIdentifier ||
        token.GetString() != "CODE") {
      return Error(token, "Expected CODE in macro definition");
    }
    if (!ParseMacroCode()) {
      return false;
    }
  }

  return Error(token, "Expected '}' for MACRO");
}

bool Parser::ParseMacroCode() {
  gb::Token token = NextToken();
  if (token.GetType() == gb::kTokenInt) {
    // Bit prefix
    token = NextToken();
  }
  if (token.GetSymbol() == ':') {
    token = NextToken();
    if (token.GetType() != gb::kTokenIdentifier) {
      return Error(token, "Expected return register for CODE");
    }
    // TODO: Validate the register name
    token = NextToken();
  }
  if (token.GetType() != gb::kTokenString) {
    return Error(token, "Expected source string for CODE");
  }
  token = NextToken();
  if (token.GetSymbol() != '{') {
    return Error(token, "Expected '{' for CODE");
  }

  for (token = NextToken(); token.GetType() != gb::kTokenEnd;
       token = NextToken()) {
    if (token.GetSymbol() == '}') {
      return true;
    }
    lexer_->RewindToken(content_);
    if (!ParseCode()) {
      return false;
    }
  }

  return Error(token, "Expected '}' for CODE");
}

bool Parser::ParseInstruction() {
  gb::Token token = NextToken();
  if (token.GetSymbol() == '(') {
    token = NextToken();
    if (token.GetType() != gb::kTokenIdentifier ||
        token.GetString() != "opcode") {
      return Error(token, "Expected instruction config parameter: opcode");
    }
    token = NextToken();
    if (token.GetSymbol() != ':') {
      return Error(token, "Expected ':' after instruction config parameter");
    }
    token = NextToken();
    if (token.GetType() != gb::kTokenInt) {
      return Error(token, "Expected integer value for opcode");
    }
    token = NextToken();
    if (token.GetSymbol() != ')') {
      return Error(token, "Expected ')' after instruction parameter");
    }
    token = NextToken();
  }

  if (token.GetType() != gb::kTokenIdentifier) {
    return Error(token, "Expected name for INSTRUCTION");
  }
  token = NextToken();
  if (token.GetSymbol() == '.') {
    token = NextToken();
    if (token.GetType() != gb::kTokenIdentifier) {
      return Error(token, "Expected specifier for instruction");
    }
    token = NextToken();
  }

  if (token.GetType() == gb::kTokenString) {
    // Source parameters for instruction
    token = NextToken();
  }

  if (token.GetSymbol() != '{') {
    return Error(token, "Expected '{' for INSTRUCTION");
  }
  for (token = NextToken(); token.GetType() != gb::kTokenEnd;
       token = NextToken()) {
    if (token.GetSymbol() == '}') {
      return true;
    }
    lexer_->RewindToken(content_);
    if (!ParseCode()) {
      return false;
    }
  }

  return Error(token, "Expected '}' for INSTRUCTION");
}

bool Parser::ParseCode() {
  gb::Token token = NextToken();
  if (token.GetSymbol() == '@') {
    token = NextToken();
    if (token.GetType() != gb::kTokenIdentifier) {
      return Error(token, "Expected code label");
    }
    token = NextToken();
    if (token.GetSymbol() != ':') {
      return Error(token, "Expected ':' after code label");
    }
    token = NextToken();
  }
  if (token.GetSymbol() == '$') {
    token = NextToken();
    if (token.GetType() != gb::kTokenIdentifier) {
      return Error(token, "Expected macro name");
    }
    token = NextToken();
    if (token.GetSymbol() == '(') {
      token = NextToken();
      if (token.GetType() != gb::kTokenIdentifier) {
        return Error(token, "Expected register as macro parameter.");
      }
      token = NextToken();
      if (token.GetSymbol() != ')') {
        return Error(token, "Expected ')' after macro parameter.");
      }
      token = NextToken();
    }
  } else {
    if (token.GetType() != gb::kTokenIdentifier) {
      return Error(token, "Expected microcode opcode.");
    }
    token = NextToken();
    if (token.GetSymbol() == '(') {
      token = NextToken();
      int param_count = 0;
      while (param_count < 2) {
        ++param_count;
        if (token.GetSymbol() == '@') {
          token = NextToken();
          if (token.GetType() != gb::kTokenIdentifier) {
            return Error(token, "Expected code label");
          }
        } else if (token.GetType() != gb::kTokenIdentifier &&
                   token.GetType() != gb::kTokenInt) {
          return Error(token, "Invalid argument for opcode.");
        }
        token = NextToken();
        if (token.GetSymbol() != ',') {
          break;
        }
        token = NextToken();
      }
      if (token.GetSymbol() != ')') {
        return Error(token, "Expected ')' after opcode parameters.");
      }
      token = NextToken();
    }
  }
  if (token.GetSymbol() != ';') {
    return Error(token, "Expected ';' after microcode statement");
  }
  return true;
}

}  // namespace

bool ParseInstructionSet(std::string text) {
  auto parser = Parser::Create("", std::move(text));
  return parser != nullptr && parser->Parse();
}

bool ParseInstructionSetFile(gb::FileSystem& file_system,
                             std::string_view path) {
  std::string normalized_path = gb::NormalizePath(path);
  std::string text;
  if (!file_system.ReadFile(normalized_path, &text)) {
    return false;
  }
  auto parser =
      Parser::Create(gb::RemoveProtocol(normalized_path), std::move(text));
  return parser != nullptr && parser->Parse();
}

}  // namespace oz3
