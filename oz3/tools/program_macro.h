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
// Each is tokens of the content the macro is defined in, so the lexer that
// read them must outlive the macro.
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
  // identifier tokens, so not register names, as the program lexer reads
  // them, in upper case.
  ProgramMacro(gb::Token name, std::vector<gb::Token> parameters);
  ProgramMacro(const ProgramMacro&) = delete;
  ProgramMacro& operator=(const ProgramMacro&) = delete;
  ~ProgramMacro() = default;

  const gb::Token& GetName() const { return name_; }
  absl::Span<const gb::Token> GetParameters() const { return parameters_; }
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

  gb::Token name_;
  std::vector<gb::Token> parameters_;
  std::vector<Line> lines_;
};

//==============================================================================
// MacroExpansion
//==============================================================================

// A macro argument: its first and last tokens, on the line that uses the
// macro.
struct MacroArg {
  gb::Token first;
  gb::Token last;
};

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
  // A run of the text from `offset` up to the next run's, copied from
  // `location` onward.
  struct Run {
    int offset = 0;
    gb::LexerLocation location;
  };

  // Appends `text`, copied from `location` onward, to the last line.
  void Append(std::string_view text, gb::LexerLocation location);

  std::string text_;

  // The runs of each line, the first at its start, and the last the body after
  // its last token replaced, which may be empty.
  std::vector<std::vector<Run>> lines_;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_MACRO_H_
