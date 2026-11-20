// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_macro.h"

#include <algorithm>
#include <utility>

#include "absl/log/check.h"

namespace oz3 {

//==============================================================================
// ProgramMacro
//==============================================================================

ProgramMacro::ProgramMacro(std::string name,
                           std::vector<std::string> parameters)
    : name_(std::move(name)), parameters_(std::move(parameters)) {}

bool ProgramMacro::ReadLine(gb::Lexer& lexer, gb::LexerContentId content,
                            gb::Token* error) {
  const int line_index = lexer.GetCurrentLine(content);
  Line line = {.location = lexer.GetLineLocation(content, line_index)};
  int end = 0;
  for (gb::Token token = lexer.NextToken(content);
       token.GetType() != gb::kTokenLineBreak && !token.IsEnd();
       token = lexer.NextToken(content)) {
    if (token.IsError()) {
      *error = token;
      SkipLine(lexer, content, line_index);
      return false;
    }
    const int column = lexer.GetTokenLocation(token).column;
    const int size = static_cast<int>(lexer.GetTokenText(token).size());
    end = column + size;
    if (!token.IsIdent()) {
      continue;
    }
    const auto it =
        std::find(parameters_.begin(), parameters_.end(), token.GetString());
    if (it != parameters_.end()) {
      line.parameters.push_back(
          {.column = column,
           .size = size,
           .index = static_cast<int>(it - parameters_.begin())});
    }
  }
  SkipLine(lexer, content, line_index);
  line.text =
      std::string(lexer.GetLineText(content, line_index).substr(0, end));
  lines_.push_back(std::move(line));
  return true;
}

void ProgramMacro::SkipLine(gb::Lexer& lexer, gb::LexerContentId content,
                            int line_index) {
  // The lexer stays on a line after its line break, until the next token.
  if (lexer.GetCurrentLine(content) == line_index) {
    lexer.NextLine(content);
  }
}

//==============================================================================
// Macro arguments
//==============================================================================

std::vector<MacroArgSpan> ReadMacroArgs(gb::Lexer& lexer,
                                        gb::LexerContentId content) {
  std::vector<MacroArgSpan> args;
  gb::Token token = lexer.NextToken(content);
  if (token.GetType() == gb::kTokenLineBreak || token.IsEnd()) {
    return args;
  }
  MacroArgSpan arg;
  bool empty = true;
  int depth = 0;
  for (;; token = lexer.NextToken(content)) {
    const bool line_end =
        token.GetType() == gb::kTokenLineBreak || token.IsEnd();
    if (line_end || (depth == 0 && token.IsSymbol(','))) {
      if (empty) {
        arg = {.column = lexer.GetTokenLocation(token).column, .token = token};
      }
      args.push_back(arg);
      if (line_end) {
        break;
      }
      empty = true;
      continue;
    }
    if (token.IsSymbol('(') || token.IsSymbol('[') || token.IsSymbol('{')) {
      ++depth;
    } else if ((token.IsSymbol(')') || token.IsSymbol(']') ||
                token.IsSymbol('}')) &&
               depth > 0) {
      --depth;
    }
    const int column = lexer.GetTokenLocation(token).column;
    const int end = column + static_cast<int>(lexer.GetTokenText(token).size());
    if (empty) {
      arg = {.column = column, .token = token};
      empty = false;
    }
    arg.size = end - arg.column;
  }
  return args;
}

//==============================================================================
// MacroExpansion
//==============================================================================

MacroExpansion::MacroExpansion(const ProgramMacro* macro,
                               absl::Span<const SourceText> args,
                               std::string scope, SourceOrigin use)
    : macro_(macro), scope_(std::move(scope)), use_(std::move(use)) {
  CHECK_EQ(args.size(), macro_->GetParameters().size());
  for (const ProgramMacro::Line& body_line : macro_->GetLines()) {
    SourceText& line = lines_.emplace_back();
    int column = 0;
    for (const ProgramMacro::Line::Parameter& parameter :
         body_line.parameters) {
      if (parameter.column > column) {
        line.pieces.push_back({.column = static_cast<int>(line.text.size()),
                               .origin = GetBodyOrigin(body_line, column)});
        line.text.append(body_line.text, column, parameter.column - column);
      }
      const SourceText& arg = args[parameter.index];
      const int arg_column = static_cast<int>(line.text.size());
      for (const SourceText::Piece& piece : arg.pieces) {
        line.pieces.push_back(
            {.column = arg_column + piece.column, .origin = piece.origin});
      }
      line.text.append(arg.text);
      column = parameter.column + parameter.size;
    }
    const int size = static_cast<int>(body_line.text.size());
    if (column < size || line.pieces.empty()) {
      line.pieces.push_back({.column = static_cast<int>(line.text.size()),
                             .origin = GetBodyOrigin(body_line, column)});
      line.text.append(body_line.text, column);
    }
    text_.append(line.text);
    text_.push_back('\n');
  }
}

SourceOrigin MacroExpansion::GetBodyOrigin(const ProgramMacro::Line& line,
                                           int column) const {
  SourceOrigin origin = {.location = line.location, .scope = scope_};
  origin.location.column += column;
  return origin;
}

SourceOrigin MacroExpansion::GetOrigin(int line, int column) const {
  if (line < 0 || line >= static_cast<int>(lines_.size())) {
    return use_;
  }
  // Every line has a piece at column 0.
  const std::vector<SourceText::Piece>& pieces = lines_[line].pieces;
  int index = static_cast<int>(pieces.size()) - 1;
  while (index > 0 && pieces[index].column > column) {
    --index;
  }
  SourceOrigin origin = pieces[index].origin;
  origin.location.column += std::max(column - pieces[index].column, 0);
  return origin;
}

SourceText MacroExpansion::GetSourceText(int line, int column, int size) const {
  SourceText text;
  if (line < 0 || line >= static_cast<int>(lines_.size())) {
    return text;
  }
  const std::string& line_text = lines_[line].text;
  column = std::clamp(column, 0, static_cast<int>(line_text.size()));
  text.text = line_text.substr(column, size);
  text.pieces.push_back({.column = 0, .origin = GetOrigin(line, column)});
  for (const SourceText::Piece& piece : lines_[line].pieces) {
    if (piece.column > column && piece.column < column + size) {
      text.pieces.push_back(
          {.column = piece.column - column, .origin = piece.origin});
    }
  }
  return text;
}

}  // namespace oz3
