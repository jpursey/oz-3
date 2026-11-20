// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_macro.h"

#include <utility>

#include "absl/algorithm/container.h"
#include "absl/log/check.h"

namespace oz3 {

namespace {

bool IsLineEnd(const gb::Token& token) {
  return token.GetType() == gb::kTokenLineBreak || token.IsEnd();
}

// Returns the origin of `column` of a macro body's `line`, in `scope`.
SourceOrigin GetBodyOrigin(const ProgramMacro::Line& line, int column,
                           std::string_view scope) {
  SourceOrigin origin = {.location = line.location, .scope = scope};
  origin.location.column += column;
  return origin;
}

}  // namespace

//==============================================================================
// SourceText
//==============================================================================

SourceText::SourceText(std::string_view text, const SourceOrigin& origin) {
  Append(text, origin);
}

void SourceText::Append(std::string_view text, const SourceOrigin& origin) {
  AddPiece(static_cast<int>(text_.size()), origin);
  text_.append(text);
}

void SourceText::Append(const SourceText& text) {
  CHECK_NE(&text, this);
  const int column = static_cast<int>(text_.size());
  for (const Piece& piece : text.pieces_) {
    AddPiece(column + piece.column, piece.origin);
  }
  text_.append(text.text_);
}

void SourceText::AddPiece(int column, const SourceOrigin& origin) {
  if (!pieces_.empty() && pieces_.back().column == column) {
    pieces_.back().origin = origin;
  } else {
    pieces_.push_back({.column = column, .origin = origin});
  }
}

SourceOrigin SourceText::GetOrigin(int column) const {
  CHECK_GE(column, 0);
  CHECK(!pieces_.empty());

  // The first piece is at column 0, so the piece at `column` is the one
  // before the first piece after it.
  const auto it = absl::c_upper_bound(
      pieces_, column,
      [](int column, const Piece& piece) { return column < piece.column; });
  const Piece& piece = *(it - 1);
  SourceOrigin origin = piece.origin;
  origin.location.column += column - piece.column;
  return origin;
}

SourceText SourceText::Substr(int column, int size) const {
  CHECK(column >= 0 && column <= static_cast<int>(text_.size()));
  SourceText text;
  text.text_ = text_.substr(column, size);
  text.pieces_.push_back({.column = 0, .origin = GetOrigin(column)});
  const int end = column + static_cast<int>(text.text_.size());
  for (const Piece& piece : pieces_) {
    if (piece.column > column && piece.column < end) {
      text.pieces_.push_back(
          {.column = piece.column - column, .origin = piece.origin});
    }
  }
  return text;
}

//==============================================================================
// ProgramMacro
//==============================================================================

ProgramMacro::ProgramMacro(std::string_view name,
                           std::vector<std::string> parameters)
    : name_(name), parameters_(std::move(parameters)) {}

bool ProgramMacro::ReadLine(gb::Lexer& lexer, gb::LexerContentId content,
                            gb::Token* error) {
  gb::Token token = lexer.NextToken(content);
  if (token.IsEnd()) {
    *error = token;
    return false;
  }
  const int line_index = lexer.GetCurrentLine(content);
  Line line = {.location = lexer.GetLineLocation(content, line_index)};
  int end = 0;
  for (; !IsLineEnd(token); token = lexer.NextToken(content)) {
    if (token.IsError()) {
      *error = token;
      lexer.NextLine(content);
      return false;
    }
    const int column = lexer.GetTokenLocation(token).column;
    const int size = static_cast<int>(lexer.GetTokenText(token).size());
    end = column + size;
    if (!token.IsIdent()) {
      continue;
    }
    const auto it = absl::c_find(parameters_, token.GetString());
    if (it != parameters_.end()) {
      line.parameters.push_back(
          {.column = column,
           .size = size,
           .index = static_cast<int>(it - parameters_.begin())});
    }
  }
  line.text =
      std::string(lexer.GetLineText(content, line_index).substr(0, end));
  lines_.push_back(std::move(line));
  return true;
}

//==============================================================================
// Macro arguments
//==============================================================================

bool ReadMacroArgs(gb::Lexer& lexer, gb::LexerContentId content,
                   std::vector<MacroArgSpan>* args, gb::Token* error) {
  args->clear();
  gb::Token token = lexer.NextToken(content);
  if (IsLineEnd(token)) {
    return true;
  }
  int depth = 0;
  int end = 0;  // Of the last token read.
  while (true) {
    // The token starts the argument, or for an empty one, ends it.
    MacroArgSpan& arg = args->emplace_back(MacroArgSpan{
        .column = lexer.GetTokenLocation(token).column, .token = token});
    for (; !IsLineEnd(token) && !(depth == 0 && token.IsSymbol(','));
         token = lexer.NextToken(content)) {
      if (token.IsError()) {
        *error = token;
        lexer.NextLine(content);
        args->clear();
        return false;
      }
      if (token.IsSymbol('(') || token.IsSymbol('[') || token.IsSymbol('{')) {
        ++depth;
      } else if ((token.IsSymbol(')') || token.IsSymbol(']') ||
                  token.IsSymbol('}')) &&
                 depth > 0) {
        --depth;
      }
      end = lexer.GetTokenLocation(token).column +
            static_cast<int>(lexer.GetTokenText(token).size());
      arg.size = end - arg.column;
    }
    if (IsLineEnd(token)) {
      // The line break is after any comment, so an empty last argument is
      // just after its comma instead.
      if (arg.size == 0) {
        arg.column = end;
      }
      return true;
    }
    end = lexer.GetTokenLocation(token).column + 1;
    token = lexer.NextToken(content);
  }
}

//==============================================================================
// MacroExpansion
//==============================================================================

MacroExpansion::MacroExpansion(const ProgramMacro& macro,
                               absl::Span<const SourceText> args,
                               std::string_view scope, SourceOrigin use)
    : scope_(scope), use_(std::move(use)) {
  CHECK_EQ(args.size(), macro.GetParameters().size());
  lines_.reserve(macro.GetLines().size());
  for (const ProgramMacro::Line& body_line : macro.GetLines()) {
    SourceText& line = lines_.emplace_back();
    const std::string_view body_text = body_line.text;
    int column = 0;
    for (const ProgramMacro::Line::Parameter& parameter :
         body_line.parameters) {
      line.Append(body_text.substr(column, parameter.column - column),
                  GetBodyOrigin(body_line, column, scope_));
      line.Append(args[parameter.index]);
      column = parameter.column + parameter.size;
    }
    line.Append(body_text.substr(column),
                GetBodyOrigin(body_line, column, scope_));
  }
}

std::string MacroExpansion::GetText() const {
  std::string text;
  for (const SourceText& line : lines_) {
    text.append(line.GetText());
    text.push_back('\n');
  }
  return text;
}

const SourceText& MacroExpansion::GetLine(int line) const {
  CHECK(line >= 0 && line < static_cast<int>(lines_.size()));
  return lines_[line];
}

SourceOrigin MacroExpansion::GetOrigin(int line, int column) const {
  if (line < 0 || line >= static_cast<int>(lines_.size())) {
    return use_;
  }
  return lines_[line].GetOrigin(column);
}

}  // namespace oz3
