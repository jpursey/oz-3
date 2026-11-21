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

bool IsLineEnd(const gb::Token& token) {
  return token.GetType() == gb::kTokenLineBreak || token.IsEnd();
}

// Returns the text from the start of `first` to the end of `last`, which are
// on the same line.
std::string_view GetTokensText(const gb::Lexer& lexer, const gb::Token& first,
                               const gb::Token& last) {
  // Each token's text is in its line's text.
  const std::string_view first_text = lexer.GetTokenText(first);
  const std::string_view last_text = lexer.GetTokenText(last);
  DCHECK(first_text.data() <= last_text.data());
  return std::string_view(first_text.data(),
                          last_text.data() + last_text.size());
}

// Returns `location`, moved right by `columns`.
gb::LexerLocation GetColumn(gb::LexerLocation location, int columns) {
  location.column += columns;
  return location;
}

// Sets `error` to `message` at `token`, and moves `lexer` past the line, unless
// `token` already ended it.
void FailLine(gb::Lexer& lexer, gb::LexerContentId content,
              const gb::Token& token, std::string_view message,
              gb::ParseError* error) {
  *error = gb::ParseError(lexer.GetTokenLocation(token), message);
  if (!IsLineEnd(token)) {
    lexer.NextLine(content);
  }
}

}  // namespace

//==============================================================================
// ProgramMacro
//==============================================================================

ProgramMacro::ProgramMacro(std::string_view name,
                           std::vector<std::string> parameters)
    : name_(name), parameters_(std::move(parameters)) {}

int ProgramMacro::FindParameter(const gb::Token& token) const {
  if (!token.IsIdent()) {
    return -1;
  }
  const auto it = absl::c_find(parameters_, token.GetString());
  return it == parameters_.end() ? -1
                                 : static_cast<int>(it - parameters_.begin());
}

bool ProgramMacro::ReadLine(gb::Lexer& lexer, gb::LexerContentId content,
                            gb::ParseError* error) {
  CHECK_LT(lexer.GetCurrentLine(content), lexer.GetLineCount(content));
  gb::Token token = lexer.NextToken(content);
  if (IsLineEnd(token)) {
    return true;
  }
  if (token.IsIdent() && FindParameter(token) < 0 &&
      lexer.NextToken(content, /*advance=*/false).IsSymbol(':')) {
    FailLine(lexer, content, token,
             absl::StrCat("Macro ", name_, " can't define label ",
                          lexer.GetTokenText(token),
                          ", only local labels and its parameters"),
             error);
    return false;
  }
  Line line = {.first = token};
  for (; !IsLineEnd(token); token = lexer.NextToken(content)) {
    if (token.IsError()) {
      FailLine(lexer, content, token, token.GetString(), error);
      return false;
    }
    if (token.GetType() == kTokenMacroLabel) {
      FailLine(lexer, content, token,
               absl::StrCat("Macro label ", lexer.GetTokenText(token),
                            " can only be written by a macro's expansion"),
               error);
      return false;
    }
    if (token.GetType() == kTokenLocalLabel) {
      line.replaces.push_back({.token = token});
    } else if (const int parameter = FindParameter(token); parameter >= 0) {
      line.replaces.push_back({.token = token, .parameter = parameter});
    }
    line.last = token;
  }
  lines_.push_back(std::move(line));
  return true;
}

//==============================================================================
// Macro arguments
//==============================================================================

bool ReadMacroArgs(gb::Lexer& lexer, gb::LexerContentId content,
                   std::vector<MacroArg>* args, gb::ParseError* error) {
  args->clear();
  gb::Token token = lexer.NextToken(content);
  if (IsLineEnd(token)) {
    return true;
  }
  std::vector<MacroArg> read_args;
  gb::Token comma;  // The comma before the argument being read, if any.
  while (true) {
    if (IsLineEnd(token) || token.IsSymbol(',')) {
      // The token is the comma after the argument, or the end of the line
      // after the last one's comma.
      FailLine(lexer, content, IsLineEnd(token) ? comma : token,
               "Macro argument is empty", error);
      return false;
    }
    MacroArg arg = {.first = token};
    int depth = 0;
    for (; !IsLineEnd(token) && !(depth == 0 && token.IsSymbol(','));
         token = lexer.NextToken(content)) {
      if (token.IsError()) {
        FailLine(lexer, content, token, token.GetString(), error);
        return false;
      }
      if (token.IsSymbol('(') || token.IsSymbol('[') || token.IsSymbol('{')) {
        ++depth;
      } else if ((token.IsSymbol(')') || token.IsSymbol(']') ||
                  token.IsSymbol('}')) &&
                 depth > 0) {
        --depth;
      }
      arg.last = token;
    }
    read_args.push_back(arg);
    if (IsLineEnd(token)) {
      *args = std::move(read_args);
      return true;
    }
    comma = token;
    token = lexer.NextToken(content);
  }
}

//==============================================================================
// MacroExpansion
//==============================================================================

MacroExpansion::MacroExpansion(const gb::Lexer& lexer,
                               const ProgramMacro& macro, int use,
                               absl::Span<const MacroArg> args) {
  CHECK_EQ(args.size(), macro.GetParameters().size());
  lines_.reserve(macro.GetLines().size());
  for (const ProgramMacro::Line& body_line : macro.GetLines()) {
    const gb::LexerLocation start = lexer.GetTokenLocation(body_line.first);
    const std::string_view body =
        GetTokensText(lexer, body_line.first, body_line.last);
    lines_.emplace_back();
    int column = 0;  // In `body`, after the last token replaced.
    for (const ProgramMacro::Replace& replace : body_line.replaces) {
      gb::LexerLocation location = lexer.GetTokenLocation(replace.token);
      const int token_column = location.column - start.column;
      Append(body.substr(column, token_column - column),
             GetColumn(start, column));
      if (replace.parameter == ProgramMacro::Replace::kLocalLabel) {
        Append(
            absl::StrCat(macro.GetName(), "#", use, replace.token.GetString()),
            std::move(location));
      } else {
        const MacroArg& arg = args[replace.parameter];
        Append(GetTokensText(lexer, arg.first, arg.last),
               lexer.GetTokenLocation(arg.first));
      }
      column = token_column +
               static_cast<int>(lexer.GetTokenText(replace.token).size());
    }
    Append(body.substr(column), GetColumn(start, column));
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
  return GetColumn(run.location, offset - run.offset);
}

}  // namespace oz3
