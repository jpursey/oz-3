// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_macro.h"

#include <utility>

#include "absl/algorithm/container.h"
#include "absl/log/check.h"
#include "absl/strings/str_cat.h"
#include "oz3/tools/program_lexer.h"

namespace oz3 {

namespace {

// Returns `location`, moved right by `columns`.
gb::LexerLocation AdjustColumn(gb::LexerLocation location, int columns) {
  location.column += columns;
  return location;
}

// Sets `error` to `message` at `token`, and returns false.
bool Fail(const gb::Lexer& lexer, const gb::Token& token,
          std::string_view message, gb::ParseError* error) {
  *error = gb::ParseError(lexer.GetTokenLocation(token), message);
  return false;
}

}  // namespace

//==============================================================================
// ProgramMacro
//==============================================================================

ProgramMacro::ProgramMacro(gb::Token name, std::vector<gb::Token> parameters)
    : name_(name),
      parameters_(std::move(parameters)),
      extended_(parameters_.size(), false) {}

int ProgramMacro::FindParameter(const gb::Token& token) const {
  if (!token.IsIdent()) {
    return -1;
  }
  const int count = static_cast<int>(parameters_.size());
  for (int i = 0; i < count; ++i) {
    if (token.IsIdent(parameters_[i].GetString())) {
      return i;
    }
  }
  return -1;
}

bool ProgramMacro::ReadLine(gb::Lexer& lexer, gb::LexerContentId content,
                            gb::ParseError* error) {
  CHECK_LT(lexer.GetCurrentLine(content), lexer.GetLineCount(content));
  std::vector<gb::Token> tokens;
  for (gb::Token token = lexer.NextToken(content); !IsLineEnd(token);
       token = lexer.NextToken(content)) {
    tokens.push_back(token);
  }
  if (tokens.empty()) {
    return true;
  }
  if (!ValidateLine(lexer, tokens, error)) {
    return false;
  }
  Line line = {.first = tokens.front(), .last = tokens.back()};
  const int count = static_cast<int>(tokens.size());
  for (int i = 0; i < count; ++i) {
    const gb::Token& token = tokens[i];
    if (token.GetType() == kTokenLocalLabel) {
      line.replaces.push_back({.token = token});
    } else if (const int parameter = FindParameter(token); parameter >= 0) {
      line.replaces.push_back({.token = token, .parameter = parameter});
      if (i + 1 < count && tokens[i + 1].IsSymbol('.') &&
          IsNextTo(lexer, token, tokens[i + 1])) {
        extended_[parameter] = true;
      }
    }
  }
  lines_.push_back(std::move(line));
  return true;
}

bool ProgramMacro::ValidateLine(const gb::Lexer& lexer,
                                absl::Span<const gb::Token> tokens,
                                gb::ParseError* error) const {
  const int count = static_cast<int>(tokens.size());
  if (count >= 2 && tokens[0].IsIdent() && FindParameter(tokens[0]) < 0 &&
      tokens[1].IsSymbol(':')) {
    return Fail(
        lexer, tokens[0],
        absl::StrCat("Macro ", name_.GetString(), " can't define label ",
                     lexer.GetTokenText(tokens[0]),
                     ", only local labels and its parameters"),
        error);
  }
  for (int i = 0; i < count; ++i) {
    const gb::Token& token = tokens[i];
    if (token.IsError()) {
      return Fail(lexer, token, token.GetString(), error);
    }
    if (token.GetType() == kTokenMacroLabel) {
      return Fail(lexer, token,
                  absl::StrCat("Macro label ", lexer.GetTokenText(token),
                               " can only be written by a macro's expansion"),
                  error);
    }
    if (token.IsSymbol('.') && !ValidateDirective(lexer, tokens, i, error)) {
      return false;
    }
  }
  return true;
}

bool ProgramMacro::ValidateDirective(const gb::Lexer& lexer,
                                     absl::Span<const gb::Token> tokens,
                                     int index, gb::ParseError* error) const {
  const int count = static_cast<int>(tokens.size());
  const gb::Token& dot = tokens[index];
  if (index + 1 == count || !tokens[index + 1].IsIdent() ||
      !IsNextTo(lexer, dot, tokens[index + 1])) {
    return true;
  }
  const gb::Token& name = tokens[index + 1];

  // A '.' right after a name is its extension ("ADD.W"). After a parameter,
  // its argument ends in a name (see IsExtended()), so it is one there too.
  if (index > 0 && tokens[index - 1].IsIdent() &&
      IsNextTo(lexer, tokens[index - 1], dot)) {
    return true;
  }
  if (FindParameter(name) >= 0) {
    return Fail(lexer, name,
                absl::StrCat("Macro ", name_.GetString(),
                             " can't name a directive with its parameter ",
                             name.GetString()),
                error);
  }
  const std::string_view directive = name.GetString();
  if (directive == "MACRO") {
    return Fail(lexer, dot, ".MACRO can't be in a macro's body", error);
  }
  if (directive == "ENDM") {
    return Fail(lexer, dot, ".ENDM must be on a line of its own", error);
  }
  if (directive == "INCLUDE") {
    return Fail(lexer, dot, "A macro's body can't include a file", error);
  }

  // A constant's name is defined again at each invocation, so only a
  // parameter can name it.
  if (directive == "CONST" && index + 2 < count &&
      tokens[index + 2].IsIdent() && FindParameter(tokens[index + 2]) < 0) {
    return Fail(
        lexer, tokens[index + 2],
        absl::StrCat("Macro ", name_.GetString(), " can't define constant ",
                     tokens[index + 2].GetString(), ", only its parameters"),
        error);
  }
  return true;
}

//==============================================================================
// MacroExpansion
//==============================================================================

MacroExpansion::MacroExpansion(const gb::Lexer& lexer,
                               const ProgramMacro& macro, int invocation,
                               absl::Span<const MacroArg> args) {
  CHECK_EQ(args.size(), macro.GetParameters().size());
  lines_.reserve(macro.GetLines().size());
  for (const ProgramMacro::Line& body_line : macro.GetLines()) {
    const gb::LexerLocation start = lexer.GetTokenLocation(body_line.first);
    const std::string_view body =
        lexer.GetTokenRangeText(body_line.first, body_line.last);
    lines_.emplace_back();
    int column = 0;  // In `body`, after the last token replaced.
    for (const ProgramMacro::Replace& replace : body_line.replaces) {
      gb::LexerLocation location = lexer.GetTokenLocation(replace.token);
      const int token_column = location.column - start.column;
      Append(body.substr(column, token_column - column),
             AdjustColumn(start, column));
      if (replace.parameter == ProgramMacro::Replace::kLocalLabel) {
        Append(absl::StrCat(macro.GetName().GetString(), "#", invocation,
                            replace.token.GetString()),
               std::move(location));
      } else {
        const MacroArg& arg = args[replace.parameter];
        Append(lexer.GetTokenRangeText(arg.first, arg.last),
               lexer.GetTokenLocation(arg.first));
      }
      column = token_column +
               static_cast<int>(lexer.GetTokenText(replace.token).size());
    }
    Append(body.substr(column), AdjustColumn(start, column));
    text_.push_back('\n');
  }
}

void MacroExpansion::Append(std::string_view text, gb::LexerLocation location) {
  lines_.back().push_back({.offset = static_cast<int>(text_.size()),
                           .location = std::move(location)});
  text_.append(text);
}

gb::LexerLocation MacroExpansion::GetLocation(int line, int column) const {
  CHECK(line >= 0 && line < GetLineCount());
  CHECK_GE(column, 0);

  // Every line starts with a run, so the run at `column` is the one before
  // the first run after it. That is the last of any runs at the same offset,
  // as the ones before it are empty.
  const std::vector<Run>& runs = lines_[line];
  const int offset = runs.front().offset + column;
  const auto it = absl::c_upper_bound(
      runs, offset,
      [](int offset, const Run& run) { return offset < run.offset; });
  const Run& run = *(it - 1);
  return AdjustColumn(run.location, offset - run.offset);
}

}  // namespace oz3
