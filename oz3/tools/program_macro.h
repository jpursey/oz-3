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
#include "gb/parse/parse_error.h"
#include "gb/parse/parse_types.h"
#include "gb/parse/token.h"

namespace oz3 {

//==============================================================================
// ProgramMacro
//==============================================================================

// A macro defined with .MACRO: its name, parameters, and body.
//
// The body is the tokens of its lines, in the content the macro is defined
// in, so the lexer that read them must outlive the macro.
class ProgramMacro final {
 public:
  // A token in a line of the body that each use of the macro replaces: a
  // parameter's name, or a local label the body writes.
  struct Replace {
    static constexpr int kLocalLabel = -1;

    gb::Token token;
    int parameter = kLocalLabel;  // Its index in the parameters, if it is one.
  };

  // A line of the body that has tokens: its first and last, and the ones each
  // use replaces, in order.
  struct Line {
    gb::Token first;
    gb::Token last;
    std::vector<Replace> replaces;
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
  // reads, as the body's next line, and moves past it. A line with no tokens,
  // or only a comment, adds no line. `content` must have a line left, so a
  // caller looks for the line that ends the body first.
  //
  // An identifier that is a parameter's name is that parameter, but one
  // inside a string or a local label ('reg' or @reg) is not. Every local label
  // is the body's own. Returns false, without adding the line, if a token on
  // it can't be read, or it starts with a label that is neither a local label
  // nor a parameter, setting `error` to say so.
  bool ReadLine(gb::Lexer& lexer, gb::LexerContentId content,
                gb::ParseError* error);

 private:
  // Returns the index of the parameter `token` names, or -1 if it isn't one.
  int FindParameter(const gb::Token& token) const;

  std::string name_;
  std::vector<std::string> parameters_;
  std::vector<Line> lines_;
};

//==============================================================================
// Macro arguments
//==============================================================================

// A macro argument: its first and last tokens, in the content of the line that
// uses the macro.
struct MacroArg {
  gb::Token first;
  gb::Token last;
};

// Reads a macro use's arguments into `args`, from the next token `lexer` reads
// in `content`, which the program lexer reads, to the end of the line, which
// it moves past.
//
// Each argument is the tokens between commas that aren't inside brackets ('(',
// '[', or '{'), so "D(table), (R1)" is two arguments. A line with nothing more
// on it has no arguments, which is valid. Returns false, leaving `args` empty,
// if a token on the line can't be read, or an argument is empty (at the comma
// after it, or for the last one, the comma before it), setting `error` to say
// so.
bool ReadMacroArgs(gb::Lexer& lexer, gb::LexerContentId content,
                   std::vector<MacroArg>* args, gb::ParseError* error);

//==============================================================================
// MacroExpansion
//==============================================================================

// The text of one use of a macro, and where each part of it came from.
//
// The text has a line for each of the body's lines. Each parameter is
// replaced by its argument's text, and each local label the body writes is
// renamed for this use (@WAIT in the second use of WAITIN is WAITIN#2@WAIT,
// a kTokenMacroLabel). The rest of the line is copied from the body, from its
// first token to its last, so its comment is dropped.
class MacroExpansion final {
 public:
  // Expands `macro`, which `lexer` read, as its use number `use` (counting
  // from 1), with `args`, one for each of its parameters, which `lexer` also
  // read.
  MacroExpansion(const gb::Lexer& lexer, const ProgramMacro& macro, int use,
                 absl::Span<const MacroArg> args);
  MacroExpansion(const MacroExpansion&) = delete;
  MacroExpansion& operator=(const MacroExpansion&) = delete;
  ~MacroExpansion() = default;

  // Returns the text, with a line break after each line.
  const std::string& GetText() const { return text_; }

  int GetLineCount() const { return static_cast<int>(lines_.size()); }

  // Returns where the text at `line` and `column` came from: in the macro's
  // body, or in an argument, where the use wrote it. `line` must be from 0 to
  // GetLineCount() - 1, and `column` must not be negative.
  //
  // Each character of the text copied from the body or an argument is at the
  // place it was copied from, and a renamed local label starts at the label it
  // renames. A column past the end of the line is past the end of the body's
  // line.
  gb::LexerLocation GetLocation(int line, int column) const;

 private:
  // A run of a line's text from `column` up to the next run's, copied from
  // `location` onward.
  struct Run {
    int column = 0;
    gb::LexerLocation location;
  };

  // A line of the text: where it starts, and its runs, the first at column 0
  // and the last at the end of the body's line.
  struct Line {
    int start = 0;
    std::vector<Run> runs;
  };

  // Appends `text`, copied from `location` onward, to the last line.
  void Append(std::string_view text, gb::LexerLocation location);

  std::string text_;
  std::vector<Line> lines_;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_MACRO_H_
