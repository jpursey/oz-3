// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_MACRO_H_
#define OZ3_TOOLS_PROGRAM_MACRO_H_

#include <string>
#include <string_view>
#include <vector>

#include "absl/types/span.h"
#include "gb/parse/lexer.h"
#include "gb/parse/parse_types.h"
#include "gb/parse/token.h"

namespace oz3 {

//==============================================================================
// SourceText
//==============================================================================

// Where a piece of text came from: a place in a source file, and the scope
// its local labels are in.
struct SourceOrigin {
  gb::LexerLocation location;  // In a file's content, never an expansion's.
  std::string_view scope;      // The key prefix of its local labels.

  bool operator==(const SourceOrigin&) const = default;
};

// Text whose pieces each came from a place in a source file, such as a line
// of a macro's expansion, or a macro argument.
class SourceText final {
 public:
  // A piece of the text, from `column` up to the next piece's: its first
  // character is at `origin`, and each one after it at the next column.
  struct Piece {
    int column = 0;
    SourceOrigin origin;

    bool operator==(const Piece&) const = default;
  };

  // Creates empty text with no pieces, which has no origins until something
  // is appended.
  SourceText() = default;

  // Creates `text`, all from `origin`.
  SourceText(std::string_view text, const SourceOrigin& origin);

  const std::string& GetText() const { return text_; }

  // Returns the pieces in column order, the first at column 0. A piece is
  // never empty, apart from one at the end of the text.
  absl::Span<const Piece> GetPieces() const { return pieces_; }

  // Appends `text`, all from `origin`. Empty text still sets the origin of
  // the end of the text, until something more is appended.
  void Append(std::string_view text, const SourceOrigin& origin);

  // Appends `text`, which must be another SourceText, with its pieces'
  // origins.
  void Append(const SourceText& text);

  // Returns where the text at `column`, which must not be negative, came
  // from. A column at or past the end is past the last piece's end. The
  // text must have a piece.
  SourceOrigin GetOrigin(int column) const;

  // Returns `size` characters of the text from `column`, which must be in
  // the text or at its end, with their origins. Its origins refer to the same
  // scopes as this text's.
  SourceText Substr(int column, int size) const;

 private:
  // Adds a piece at `column`, replacing an empty piece there.
  void AddPiece(int column, const SourceOrigin& origin);

  std::string text_;
  std::vector<Piece> pieces_;
};

//==============================================================================
// ProgramMacro
//==============================================================================

// A macro defined with .MACRO: its name, parameters, and body.
class ProgramMacro final {
 public:
  // A line of the body: its text, without its comment or line break, where
  // it is, and each place a parameter's name is a word in it, in column
  // order.
  struct Line {
    struct Parameter {
      int column = 0;
      int size = 0;
      int index = 0;  // In the macro's parameters.

      bool operator==(const Parameter&) const = default;
    };

    std::string text;
    gb::LexerLocation location;  // Of its first column.
    std::vector<Parameter> parameters;
  };

  // Creates a macro with no lines yet. Its name and parameters are
  // identifiers, so not register names, as the program lexer reads them, in
  // upper case.
  ProgramMacro(std::string_view name, std::vector<std::string> parameters);

  ProgramMacro(const ProgramMacro&) = delete;
  ProgramMacro& operator=(const ProgramMacro&) = delete;
  ~ProgramMacro() = default;

  std::string_view GetName() const { return name_; }
  absl::Span<const std::string> GetParameters() const { return parameters_; }
  absl::Span<const Line> GetLines() const { return lines_; }

  // Reads the next line `lexer` reads in `content`, which the program lexer
  // reads, as the body's next line, and moves past it, as ReadMacroArgs does.
  // An identifier that is a parameter's name is that parameter, but a name
  // inside a string or a local label ('reg' or @reg) is not. Returns false,
  // without adding the line, if a token on it can't be read, setting `error`
  // to that token, or if `content` has no lines left, setting `error` to its
  // end token.
  bool ReadLine(gb::Lexer& lexer, gb::LexerContentId content, gb::Token* error);

 private:
  std::string name_;
  std::vector<std::string> parameters_;
  std::vector<Line> lines_;
};

//==============================================================================
// Macro arguments
//==============================================================================

// Where a macro argument is in the line that uses the macro.
struct MacroArgSpan {
  int column = 0;
  int size = 0;  // 0 for an empty argument.

  // Its first token, or for an empty argument, the token after it (the comma
  // or the line break), for errors.
  gb::Token token;
};

// Reads a macro use's arguments into `args`, from the next token `lexer`
// reads in `content`, which the program lexer reads, to the end of the line,
// which it moves past. Each argument is the text between commas that aren't
// inside brackets ('(', '[', or '{') or a string, so "D(table), (R1)" is two.
// The line's brackets must match, as the program grammar's instructions
// require. A line with nothing more on it has no arguments, and one with a
// comma has an argument on each side of it, which may be empty: before the
// comma after it, or after the last comma. Returns false, with no arguments,
// if a token on the line can't be read, setting `error` to that token.
bool ReadMacroArgs(gb::Lexer& lexer, gb::LexerContentId content,
                   std::vector<MacroArgSpan>* args, gb::Token* error);

//==============================================================================
// MacroExpansion
//==============================================================================

// The text of one use of a macro, with where each piece of it came from.
//
// The text has a line for each of the body's, with each parameter replaced
// by its argument's text. Its pieces from the body are at their place in the
// body, in the use's own scope, and its pieces from arguments are where the
// argument's text came from. Every origin is in a file, however deeply macros
// use macros, as an argument from an expansion's line carries that line's
// origins (see GetLine).
class MacroExpansion final {
 public:
  // Expands `macro` with `args`, one for each of its parameters. `scope` is
  // this use's own scope ("WAITIN#2"), for the local labels its body writes,
  // and `use` is the place in a file where it is used (for a use in another
  // expansion, that one's use).
  MacroExpansion(const ProgramMacro& macro, absl::Span<const SourceText> args,
                 std::string_view scope, SourceOrigin use);

  // Its origins refer to its scope, so it stays where it is.
  MacroExpansion(const MacroExpansion&) = delete;
  MacroExpansion& operator=(const MacroExpansion&) = delete;
  ~MacroExpansion() = default;

  const SourceOrigin& GetUse() const { return use_; }

  // Returns the text, with a line break after each line.
  std::string GetText() const;

  // Returns line `line` of the text, which must be one of its lines, with
  // where each piece of it came from. An argument of a use in this expansion
  // is a Substr of it, and refers to this expansion's scope, so this must
  // outlive the expansion it is an argument of.
  const SourceText& GetLine(int line) const;

  // Returns where the text at `line` and `column` came from. A column past
  // the end of its line is past the end of the body's line, and a line past
  // the last is at the use.
  SourceOrigin GetOrigin(int line, int column) const;

 private:
  const std::string scope_;
  const SourceOrigin use_;
  std::vector<SourceText> lines_;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_MACRO_H_
