// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_assembler.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <iterator>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/log/check.h"
#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gb/file/path.h"
#include "gb/parse/lexer.h"
#include "gb/parse/parse_error.h"
#include "gb/parse/parse_result.h"
#include "gb/parse/parser.h"
#include "gb/parse/token.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/tools/bare_program.h"
#include "oz3/tools/expression.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program.h"
#include "oz3/tools/program_grammar.h"
#include "oz3/tools/program_lexer.h"
#include "oz3/tools/program_macro.h"
#include "oz3/tools/program_map.h"
#include "oz3/tools/source_pattern.h"

namespace oz3 {

namespace {

using ArgKind = InstructionForm::Arg::Kind;

// The filename of a program assembled from text.
constexpr std::string_view kTextFilename = "program";

// Returns a form as an error lists it ("ADD.W $r, $v").
std::string GetFormText(const InstructionForm& form) {
  return absl::StrCat(form.GetInstruction().op_name,
                      form.GetPattern().empty() ? "" : " ", form.GetPattern());
}

// Returns a value for each of a form's arguments, as InstructionForm takes
// them: the register or code word value from `args`, and 0 for each $v and $V,
// which are evaluated separately.
std::vector<int64_t> GetCodeWordValues(const InstructionForm& form,
                                       absl::Span<const InstructionArg> args) {
  std::vector<int64_t> values;
  values.reserve(args.size());
  for (int i = 0; i < static_cast<int>(args.size()); ++i) {
    switch (form.GetArgs()[i].kind) {
      case ArgKind::kWordReg:
      case ArgKind::kDwordReg:
        values.push_back(args[i].reg);
        break;
      case ArgKind::kCodeValue:
        values.push_back(args[i].code_value);
        break;
      case ArgKind::kWord:
      case ArgKind::kDword:
        values.push_back(0);
        break;
    }
  }
  return values;
}

// A directive of conditional assembly, by what it does.
enum class Conditional { kNone, kIf, kElseIf, kElse, kEndIf };

// Returns which directive of conditional assembly `name` is, if any.
Conditional GetConditional(std::string_view name) {
  if (name == "IF" || name == "IFDEF" || name == "IFNDEF") {
    return Conditional::kIf;
  }
  if (name == "ELSEIF") {
    return Conditional::kElseIf;
  }
  if (name == "ELSE") {
    return Conditional::kElse;
  }
  if (name == "ENDIF") {
    return Conditional::kEndIf;
  }
  return Conditional::kNone;
}

}  // namespace

// Assembles one program, in two passes:
// 1. Reads each line of the source, following includes and expanding macros,
//    defining labels and constants, and matching each instruction to its form.
//    Each statement that writes words is recorded, with its segment and offset,
//    so that every label has its value by the end, as is each constant.
// 2. Finishes each recorded statement, now that every symbol is defined:
//    encodes each one that writes words, and checks each constant's value.
//    Then builds the program from each segment's words, and its map.
class ProgramAssembler final {
 public:
  // `syntax`, and `file_system` if it isn't null, must outlive this.
  ProgramAssembler(const InstructionSetSyntax* syntax,
                   gb::FileSystem* file_system)
      : syntax_(syntax), file_system_(file_system) {}

  // Assembles the program whose main file is at `path` in the file system, or
  // whose text is `text`, which can't include files, given the host's
  // `constants`. Returns null if there are errors, and sets `errors` to them
  // if it isn't null.
  std::unique_ptr<AsmProgram> AssembleFile(
      std::string_view path, absl::Span<const AsmConstant> constants,
      std::vector<gb::ParseError>* errors);
  std::unique_ptr<AsmProgram> AssembleText(
      std::string text, absl::Span<const AsmConstant> constants,
      std::vector<gb::ParseError>* errors);

 private:
  // A label or local label, whose value is its offset in its segment.
  struct Label {
    gb::Token token;  // Its name where it is defined.
    int segment = 0;
    int64_t offset = 0;
  };

  // A constant, whose value is its expression, evaluated with the segment and
  // offset ("$") and the scope of local labels where it is defined.
  //
  // A constant defined before the first .SEGMENT is in the default program's
  // segment until that .SEGMENT replaces it, and then in no segment (-1).
  // Nothing can be written before it, so a "$" in it is then just the number 0,
  // which is never checked as an address.
  struct Constant {
    gb::Token token;  // Its name where it is defined.
    int segment = -1;
    int64_t offset = 0;
    std::string_view scope;
    Expression expression;

    // Its results by width, once it has one that doesn't depend on a symbol
    // not yet defined, and whether it is being evaluated, to find one that
    // depends on itself.
    std::array<std::optional<ExpressionResult>, 2> results;
    bool evaluating = false;
  };

  // An instruction, matched to its form.
  struct InstructionStatement {
    const InstructionForm* form = nullptr;
    std::vector<InstructionArg> args;
  };

  // Zero words that fill a .DW or .DD field to its SIZE.
  struct DataPadding {
    int64_t words = 0;
  };

  // Data of `width` (.DW or .DD): each item is a string's words, an
  // expression, or padding.
  using DataItem = std::variant<std::vector<uint16_t>, Expression, DataPadding>;
  struct DataStatement {
    ExpressionWidth width = ExpressionWidth::kWord;
    std::vector<DataItem> items;
  };

  // A constant's definition (.CONST), whose value the second pass checks.
  struct ConstantStatement {
    gb::Token name;
  };

  // A statement the second pass finishes, in the order they are read: one
  // that writes words, which it encodes, or a constant.
  using StatementContents =
      std::variant<InstructionStatement, DataStatement, ConstantStatement>;
  struct Statement {
    gb::Token token;  // Its first token, for errors.
    int segment = 0;
    uint16_t offset = 0;
    int size = 0;  // The words it writes, if any.
    std::string_view scope;
    StatementContents contents;
  };

  // A segment declared with .SEGMENT, or the default program's one segment,
  // which has no name.
  struct Segment {
    std::optional<gb::Token> name;  // Its name where it is declared.

    // Its bank's name (BANK), and the bank's index, which the first pass
    // looks up when it ends, as a bank can be declared after the segment.
    std::optional<gb::Token> bank_name;
    uint16_t bank = 0;

    // Its access (ROM), and its fixed start page (PAGE), or 0xFFFF for any.
    Program::Segment::Access access = Program::Segment::Access::kReadWrite;
    uint16_t page_start = 0xFFFF;
    std::optional<gb::Token> page_token;

    // Its size in pages, which PAGES gives, or else its reserved size does
    // once the first pass ends.
    uint16_t page_count = 0;
    std::optional<gb::Token> pages_token;

    // Where INSTANCE makes it per instance, if it does.
    std::optional<gb::Token> instance_token;

    // Its size in words so far: every word its statements reserve, written or
    // not, which is the offset of its next statement. Once the first pass
    // ends, it is the segment's size, with the stack in the stack segment, or
    // if it is per instance, one instance's words.
    int64_t reserved_size = 0;

    // Its size up to the end of its last statement that writes words.
    int64_t written_size = 0;

    // Its words up to `written_size`, and which are written, which the second
    // pass fills in.
    std::vector<uint16_t> words;
    std::vector<bool> written;
  };

  // A bank declared with .BANK.
  struct Bank {
    gb::Token name;  // Its name where it is declared.
    uint16_t assign_mask = 0xFFFF;
  };

  // Whether an open .IF has assembled one of its blocks yet. Before it has
  // (kUnsatisfied), its next .ELSEIF's condition is evaluated. Once it has
  // (kAssembling for that block's lines, then kSatisfied), its later blocks are
  // skipped. An .IF in a skipped block, or after one of its conditional
  // directives had an error (kDisabled), assembles none of its blocks, and its
  // directives are read as in a skipped block.
  enum class IfState { kUnsatisfied, kAssembling, kSatisfied, kDisabled };

  // An .IF, .IFDEF, or .IFNDEF whose .ENDIF isn't read yet.
  struct OpenIf {
    gb::Token token;        // Its '.', for errors.
    std::string_view name;  // Its directive's name ("IF").
    IfState state = IfState::kUnsatisfied;
    bool has_else = false;
  };

  // A macro defined with .MACRO, and the number of times it is invoked, which
  // numbers each invocation. A macro with an error in its definition is still
  // defined, so its invocations aren't errors too, but they expand to nothing.
  struct Macro {
    std::unique_ptr<ProgramMacro> macro;
    bool valid = true;
    int invocations = 0;
  };

  // An invocation's expansion, which is read as content of its own, the macro
  // it expands, and the macro's name where it is invoked.
  struct Expansion {
    std::unique_ptr<MacroExpansion> text;
    const ProgramMacro* macro = nullptr;
    gb::Token invocation;
  };

  // What SkipLine() found on a line that isn't assembled.
  enum class SkippedLine {
    kSkipped,  // The line is skipped, and the lexer is at the next one.
    kRead,     // The line ends a block, so it is read, from its start.
    kEnd,      // The content ended.
  };

  // The context of a statement's expressions: its segment and offset, and the
  // scope of its local labels.
  class StatementContext final : public ExpressionContext {
   public:
    // `assembler` must outlive this.
    StatementContext(ProgramAssembler* assembler, int segment, int64_t offset,
                     std::string_view scope)
        : assembler_(assembler),
          segment_(segment),
          offset_(offset),
          scope_(scope) {}

    uint16_t GetOffset() const override {
      return static_cast<uint16_t>(offset_);
    }
    int GetSegment() const override { return segment_; }
    std::optional<ExpressionResult> GetSymbolValue(
        std::string_view name, ExpressionWidth width) const override {
      return assembler_->GetSymbolValue(scope_, name, width);
    }

   private:
    ProgramAssembler* const assembler_;
    const int segment_;
    const int64_t offset_;
    const std::string_view scope_;
  };

  gb::Lexer& GetLexer() { return parser_->GetLexer(); }
  const gb::Lexer& GetLexer() const { return parser_->GetLexer(); }

  // Returns the current segment, which statements assemble into.
  Segment& GetSegment() { return segments_[segment_]; }

  // Adds an error, unless it is already reported (as an error in a constant
  // comes back at each use), and stops once there are kMaxProgramErrors.
  //
  // An error in a macro's expansion is at the text it was copied from, and is
  // followed by an error at each invocation it is in ("In WAITIN"), from the
  // innermost out.
  void AddError(gb::ParseError error);
  void Error(const gb::Token& token, std::string_view message);

  // Returns where the text at `location` was written: `location` itself, or
  // if it is in a macro's expansion, where that text was copied from,
  // followed through each expansion it is in to a file.
  gb::LexerLocation GetSourceLocation(gb::LexerLocation location) const;

  // Returns the expansion read as `content`, or null if it isn't one.
  const Expansion* FindExpansion(gb::LexerContentId content) const;

  // Returns the start of the main file, for errors in the program as a whole.
  gb::LexerLocation GetMainLocation() const;

  // Creates the parser, and defines the host's `constants`, or reports why it
  // can't and returns false.
  //
  // DefineHostConstants() defines the host's `constants`, or reports each one
  // that is invalid and returns false.
  bool Init(absl::Span<const AsmConstant> constants);
  bool DefineHostConstants(absl::Span<const AsmConstant> constants);

  // Returns the content of a file, which is read once, and read again from
  // its start each time it is included, or reports an error at `location`
  // and returns kNoLexerContent. AddContent() adds the `text` read from
  // `path`, which is one of the map's files.
  gb::LexerContentId LoadFile(std::string_view path,
                              const gb::LexerLocation& location);
  gb::LexerContentId AddContent(std::string_view path, std::string text,
                                const gb::LexerLocation& location);

  // Assembles the main file's content, and returns the program, or null if
  // there are errors.
  std::unique_ptr<AsmProgram> Assemble(gb::LexerContentId content);
  std::unique_ptr<AsmProgram> Finish(std::unique_ptr<AsmProgram> program,
                                     std::vector<gb::ParseError>* errors);

  // Returns the key of a label's upper case name in `scope`: the name itself,
  // or for a local label, the scope's label, then the name. A name can't have
  // an "@" in it, so a local label's own "@" separates the two. A macro's
  // renamed local label ("WAITIN#2@WAIT") is its name, whose "#" no name has,
  // so no label's key is another's.
  std::string GetLabelKey(std::string_view scope, std::string_view name) const;

  // Returns the value of the symbol with an upper case name, used in `scope`.
  std::optional<ExpressionResult> GetSymbolValue(std::string_view scope,
                                                 std::string_view name,
                                                 ExpressionWidth width);
  ExpressionResult EvaluateConstant(Constant& constant, ExpressionWidth width);

  // Defines a label or constant at its name's `token`, with the upper case
  // `name`, and returns true, or reports an error and returns false if a
  // label or constant already has the name.
  bool DefineLabel(const gb::Token& token, std::string_view name);
  bool DefineConstant(const gb::Token& token, const gb::ParsedItem& value);
  bool CheckNotDefined(const gb::Token& token, std::string_view key,
                       std::string_view name);
  bool IsDefined(std::string_view key) const;

  // Returns the value of `expression` as `width` in `context`, or reports its
  // error and returns nullopt. If `segment` isn't null, it is set to the
  // segment the value is an offset in (see ExpressionResult::segment).
  std::optional<uint32_t> Evaluate(const Expression& expression,
                                   ExpressionWidth width,
                                   const ExpressionContext& context,
                                   int* segment = nullptr);

  // Returns the value of a first pass expression, which is a word evaluated
  // where the statement is.
  std::optional<uint32_t> EvaluateNow(const gb::ParsedItem& item);

  // Returns the value of a first pass expression (see EvaluateNow), or reports
  // an error and returns nullopt if it has none, or if it isn't from `min` to
  // `max`, which names it as `what` ("A segment's page").
  std::optional<int> EvaluateInRange(const gb::ParsedItem& item, int min,
                                     int max, std::string_view what);

  // First pass.
  void ReadContent(gb::LexerContentId content);
  void ReadLines(gb::LexerContentId content, std::vector<OpenIf>* ifs);
  void ReadLine(gb::LexerContentId content, const gb::ParsedItem& line,
                std::vector<OpenIf>* ifs);
  void ReadLabel(const gb::ParsedItem& label);

  // Reads a line that doesn't parse as far as a directive's name, for the
  // directives the lines after it depend on, and moves to the next line.
  // Returns false if the content has ended.
  bool ReadInvalidLine(gb::LexerContentId content, std::vector<OpenIf>* ifs);

  // Returns true if the next line has no macro label, or else reports one and
  // returns false. Only a macro's expansion writes one, which the grammar
  // can't tell, as an expansion's lines are read as a file's are. The lexer is
  // left at the start of the line.
  bool ValidateNoMacroLabel(gb::LexerContentId content);

  // Conditional assembly. Each content has its own open .IFs, `ifs`,
  // innermost last. A line in a block that isn't assembled is skipped, with
  // only its tokens up to a directive's name read, unless it is an .ELSEIF,
  // .ELSE, or .ENDIF of the innermost .IF that isn't disabled, which is read
  // as any other line.
  //
  // GetIfState() returns the state of an .IF from the condition of the block
  // it is starting, which is nullopt if the condition had an error.
  static IfState GetIfState(std::optional<bool> condition);
  SkippedLine SkipLine(gb::LexerContentId content, std::vector<OpenIf>* ifs);
  void ReadConditional(Conditional conditional, const gb::ParsedItem& directive,
                       std::vector<OpenIf>* ifs);
  void ReadInvalidConditional(Conditional conditional,
                              const gb::ParsedItem& directive,
                              std::vector<OpenIf>* ifs);
  std::optional<bool> ReadCondition(const gb::ParsedItem& directive);
  void ReadDirective(const gb::ParsedItem& directive);
  void ReadInclude(const gb::ParsedItem& directive);

  // Macros. ReadMacro() defines a macro from its .MACRO, and reads its body
  // to its .ENDM with ReadMacroBody(), which adds the body's lines to `macro`
  // and returns true, or reports each error and returns false. A null `macro`
  // only skips the body, for a .MACRO whose line is an error. `token` is the
  // .MACRO's, for errors.
  void ReadMacro(gb::LexerContentId content, const gb::ParsedItem& directive);
  bool ReadMacroBody(gb::LexerContentId content, const gb::Token& token,
                     ProgramMacro* macro);

  // Reads a line's instruction if it is a macro's invocation, by assembling
  // its expansion, and returns false if it isn't one.
  //
  // ValidateNoArgDirective() validates that no argument of the invocation
  // whose name is `name` has a directive in it, reporting each one. With the
  // directives a body can have validated where it is defined (see
  // ProgramMacro::ReadLine()), an expansion's directives are all valid.
  bool ReadMacroInvocation(gb::LexerContentId content,
                           const gb::ParsedItem& instruction);
  bool ValidateNoArgDirective(gb::LexerContentId content,
                              const gb::Token& name);
  void ReadConst(const gb::ParsedItem& directive);
  void ReadData(const gb::ParsedItem& directive, ExpressionWidth width);
  void EndDataField(std::optional<int64_t> field_end, int64_t* size,
                    DataStatement* data);
  void ReadStorage(const gb::ParsedItem& directive);
  void ReadStack(const gb::ParsedItem& directive);
  void ReadInstances(const gb::ParsedItem& directive);
  void ReadBank(const gb::ParsedItem& directive);
  void ReadSegment(const gb::ParsedItem& directive);
  void ReadSegmentRole(const gb::Token& role);
  void ReadInstruction(gb::LexerContentId content,
                       const gb::ParsedItem& instruction);

  // Adds a statement at the current offset, which writes `size` words.
  void AddStatement(const gb::Token& token, StatementContents contents,
                    int64_t size);

  // Notes that the statement or label at `token` is in a segment, which is an
  // error if it is before the first .SEGMENT.
  void UseSegment(const gb::Token& token);

  // Adds `size` words of the statement at `token` to the current segment's
  // reserved size. Reserve() is for words the statement only reserves (.DS),
  // and Write() for words it writes, which adds the statement with its
  // `contents`, and sets the written size too.
  void Reserve(int64_t size, const gb::Token& token);
  void Write(int64_t size, const gb::Token& token, StatementContents contents);

  // Reports an error at `token`, about `what`, if `size` more words take the
  // segment's reserved size past the end of a segment, where it wasn't before.
  void CheckSegmentEnd(const Segment& segment, int64_t size,
                       const gb::Token& token, std::string_view what);

  // Ends the first pass, with each of the functions after it.
  void EndFirstPass();

  // Adds the stack to the stack segment's reserved size. The stack segment
  // must be per instance if the program has more than one instance.
  void ReserveStack();

  // Looks up the segment's bank, which BANK names.
  void SetBank(Segment& segment);

  // Sets the segment's page count from its size, which is for every instance
  // if it is per instance. It can't be more than PAGES gives, or pass the end
  // of the bank from PAGE.
  void SetPageCount(Segment& segment);

  // Second pass.
  void FinishStatement(const Statement& statement);
  void CheckConstant(const ConstantStatement& constant);
  void EncodeInstruction(const Statement& statement,
                         const InstructionStatement& instruction,
                         std::vector<uint16_t>& words);
  void EncodeData(const Statement& statement, const DataStatement& data,
                  std::vector<uint16_t>& words);

  // Returns the value of `expression`, an instruction's address from the base
  // register of bank index `bank`, or reports its error and returns nullopt.
  // An offset in a segment (a label) must be in the segment with the bank's
  // role, and is made an address from the base register, which for the stack
  // is at the end of the segment, or of an instance's words.
  std::optional<uint32_t> EvaluateAddress(const Expression& expression,
                                          int bank,
                                          const ExpressionContext& context);
  std::unique_ptr<AsmProgram> BuildProgram();
  absl::StatusOr<BareProgram> BuildBareProgram(const Program& program) const;
  Program::Segment BuildSegment(int index) const;
  ProgramMap BuildMap();

  // Returns where `token` is, as the map has it, which for a token in a
  // macro's expansion is the line of its outermost invocation.
  ProgramMap::Location GetMapLocation(const gb::Token& token) const;

  // Returns the segment with the role of bank index `bank`, as a Program has
  // it: its index, or 0xFFFF for none.
  uint16_t GetRoleSegment(int bank) const;

  // Returns `instance`'s offset for the role of bank index `bank` (see
  // Program::Instance).
  uint16_t GetInstanceOffset(int bank, int instance) const;

  const InstructionSetSyntax* const syntax_;
  gb::FileSystem* const file_system_;
  std::unique_ptr<ProgramGrammar> grammar_;
  std::unique_ptr<gb::Parser> parser_;
  gb::LexerContentId main_content_ = gb::kNoLexerContent;

  // Each file's content by path, and the paths of the files being read, from
  // the main file to the current one (none for text).
  absl::flat_hash_map<std::string, gb::LexerContentId> files_;
  std::vector<std::string> including_;

  // The path of each content in the order added, which are the map's files,
  // and each content's index in them.
  std::vector<std::string> file_paths_;
  absl::flat_hash_map<gb::LexerContentId, int> file_indices_;

  // Macros by name, and each expansion by its content. A name is a view of its
  // token.
  absl::flat_hash_map<std::string_view, Macro> macros_;
  absl::flat_hash_map<gb::LexerContentId, Expansion> expansions_;

  // Errors, and each one's formatted message, to report each error once. An
  // error's invocations follow it in `errors_`, but only it counts, in
  // `error_count_`.
  std::vector<gb::ParseError> errors_;
  absl::flat_hash_set<std::string> error_messages_;
  int error_count_ = 0;
  bool stopped_ = false;

  // Labels by key (see GetLabelKey), constants by name, and the values of
  // the host's constants by their upper case name. No two have the same name.
  absl::flat_hash_map<std::string, Label> labels_;
  absl::flat_hash_map<std::string, Constant> constants_;
  absl::flat_hash_map<std::string, uint32_t> host_constants_;

  // The current scope of local labels: the name of the label that starts it,
  // or empty for the scope that starts the program. A scope is a view of its
  // label's token, which lives as long as the lexer.
  std::string_view scope_;

  // The segments in the order they are declared, starting as the default
  // program's one segment, until a .SEGMENT replaces it. Then each segment's
  // index by name, and the current segment.
  std::vector<Segment> segments_ = std::vector<Segment>(1);
  absl::flat_hash_map<std::string_view, int> segment_indices_;
  int segment_ = 0;

  // The banks in the order they are declared, and each one's index by name.
  // Without any, the program has one bank, which may be any physical bank.
  std::vector<Bank> banks_;
  absl::flat_hash_map<std::string_view, int> bank_indices_;

  // The first statement or label in a segment. The first .SEGMENT reports it,
  // as it was in the default program's segment.
  std::optional<gb::Token> first_segment_use_;

  // The segment with each role, by bank index, or -1 for none.
  // The default program's segment has every role.
  std::array<int, CpuCore::kBankCount> roles_ = {0, 0, 0, 0};

  // The statements the second pass finishes, and the stack's size (.STACK)
  // and where it is given.
  std::vector<Statement> statements_;
  uint16_t stack_words_ = 0;
  std::optional<gb::Token> stack_token_;

  // Why the default program can't be a bare program, if it can't for one of
  // its statements: the first label in an address from BS or BE, which a bare
  // program doesn't start where this program's offsets need them.
  std::optional<std::string> bare_error_;

  // The number of instances (.INSTANCES), and where it is given.
  int instance_count_ = 1;
  std::optional<gb::Token> instances_token_;
};

std::unique_ptr<AsmProgram> ProgramAssembler::AssembleFile(
    std::string_view path, absl::Span<const AsmConstant> constants,
    std::vector<gb::ParseError>* errors) {
  if (!Init(constants)) {
    return Finish(nullptr, errors);
  }
  // A path that can't be normalized is read as it is, so that the error
  // names it.
  std::string normalized_path = gb::NormalizePath(path);
  if (normalized_path.empty()) {
    normalized_path = path;
  }
  main_content_ = LoadFile(normalized_path, {});
  if (main_content_ == gb::kNoLexerContent) {
    return Finish(nullptr, errors);
  }
  including_.push_back(std::move(normalized_path));
  return Finish(Assemble(main_content_), errors);
}

std::unique_ptr<AsmProgram> ProgramAssembler::AssembleText(
    std::string text, absl::Span<const AsmConstant> constants,
    std::vector<gb::ParseError>* errors) {
  if (!Init(constants)) {
    return Finish(nullptr, errors);
  }
  main_content_ = AddContent(kTextFilename, std::move(text), {});
  if (main_content_ == gb::kNoLexerContent) {
    return Finish(nullptr, errors);
  }
  return Finish(Assemble(main_content_), errors);
}

bool ProgramAssembler::Init(absl::Span<const AsmConstant> constants) {
  absl::StatusOr<std::unique_ptr<ProgramGrammar>> grammar =
      ProgramGrammar::Create(*syntax_);
  if (!grammar.ok()) {
    AddError(gb::ParseError(grammar.status().message()));
    return false;
  }
  grammar_ = *std::move(grammar);
  parser_ = gb::Parser::Create(grammar_->GetParserProgram());
  return DefineHostConstants(constants);
}

bool ProgramAssembler::DefineHostConstants(
    absl::Span<const AsmConstant> constants) {
  // Each name is read as one token of source, so it is in upper case, and a
  // register is a keyword.
  bool valid = true;
  for (const AsmConstant& constant : constants) {
    const gb::Token token = GetLexer().ParseTokenText(constant.name);
    if (token.GetType() == gb::kTokenKeyword) {
      AddError(gb::ParseError(absl::StrCat(
          "The host's constant ", token.GetString(), " is a register")));
      valid = false;
      continue;
    }
    if (!token.IsIdent()) {
      AddError(gb::ParseError(absl::StrCat("The host's constant \"",
                                           constant.name, "\" isn't a name")));
      valid = false;
      continue;
    }
    const bool added =
        host_constants_.try_emplace(token.GetString(), constant.value).second;
    if (!added) {
      AddError(gb::ParseError(absl::StrCat(
          "The host's constant ", token.GetString(), " is given twice")));
      valid = false;
    }
  }
  return valid;
}

gb::LexerContentId ProgramAssembler::LoadFile(
    std::string_view path, const gb::LexerLocation& location) {
  if (auto it = files_.find(path); it != files_.end()) {
    GetLexer().RewindContent(it->second);
    return it->second;
  }
  std::string text;
  if (!file_system_->ReadFile(path, &text)) {
    AddError(
        gb::ParseError(location, absl::StrCat("Could not read file: ", path)));
    return gb::kNoLexerContent;
  }
  const gb::LexerContentId content =
      AddContent(path, std::move(text), location);
  if (content != gb::kNoLexerContent) {
    files_[path] = content;
  }
  return content;
}

gb::LexerContentId ProgramAssembler::AddContent(
    std::string_view path, std::string text,
    const gb::LexerLocation& location) {
  const gb::LexerContentId content =
      GetLexer().AddFileContent(path, std::move(text));
  if (content == gb::kNoLexerContent) {
    AddError(gb::ParseError(
        location,
        absl::StrCat(path, " has too many lines, or a line too long")));
    return content;
  }
  file_indices_[content] = static_cast<int>(file_paths_.size());
  file_paths_.emplace_back(path);
  return content;
}

std::unique_ptr<AsmProgram> ProgramAssembler::Assemble(
    gb::LexerContentId content) {
  // First pass.
  ReadContent(content);
  EndFirstPass();
  if (!errors_.empty()) {
    return nullptr;
  }

  // Second pass. Each segment's words are whole aligned pairs (see
  // BuildSegment).
  constexpr int kAlignment = Program::Segment::Data::kAlignment;
  for (Segment& segment : segments_) {
    const int size = static_cast<int>(
        ((segment.written_size + kAlignment - 1) / kAlignment) * kAlignment);
    segment.words.resize(size);
    segment.written.resize(size);
  }
  for (const Statement& statement : statements_) {
    if (stopped_) {
      break;
    }
    FinishStatement(statement);
  }
  if (!errors_.empty()) {
    return nullptr;
  }
  return BuildProgram();
}

std::unique_ptr<AsmProgram> ProgramAssembler::Finish(
    std::unique_ptr<AsmProgram> program, std::vector<gb::ParseError>* errors) {
  if (errors != nullptr) {
    *errors = std::move(errors_);
  }
  return program;
}

void ProgramAssembler::AddError(gb::ParseError error) {
  if (stopped_) {
    return;
  }
  std::vector<gb::ParseError> errors;
  gb::LexerContentId content = error.GetLocation().id;
  errors.emplace_back(GetSourceLocation(error.GetLocation()),
                      error.GetMessage());
  while (const Expansion* expansion = FindExpansion(content)) {
    const gb::LexerLocation location =
        GetLexer().GetTokenLocation(expansion->invocation);
    errors.emplace_back(GetSourceLocation(location),
                        absl::StrCat("In ", expansion->invocation.GetString()));
    content = location.id;
  }

  // An error is the same as another only with the same invocations.
  std::string key;
  for (const gb::ParseError& added : errors) {
    absl::StrAppend(&key, added.FormatMessage(), "\n");
  }
  if (!error_messages_.insert(std::move(key)).second) {
    return;
  }
  absl::c_move(errors, std::back_inserter(errors_));
  if (++error_count_ >= kMaxProgramErrors) {
    errors_.emplace_back(
        errors_.back().GetLocation(),
        absl::StrCat("Stopped after ", kMaxProgramErrors, " errors"));
    stopped_ = true;
  }
}

void ProgramAssembler::Error(const gb::Token& token, std::string_view message) {
  AddError(gb::ParseError(GetLexer().GetTokenLocation(token), message));
}

gb::LexerLocation ProgramAssembler::GetSourceLocation(
    gb::LexerLocation location) const {
  while (const Expansion* expansion = FindExpansion(location.id)) {
    // An error can be at the end of an expansion, past its last line.
    const MacroExpansion& text = *expansion->text;
    location =
        text.GetLocation(std::clamp(location.line, 0, text.GetLineCount() - 1),
                         std::max(location.column, 0));
  }
  return location;
}

const ProgramAssembler::Expansion* ProgramAssembler::FindExpansion(
    gb::LexerContentId content) const {
  auto it = expansions_.find(content);
  return it != expansions_.end() ? &it->second : nullptr;
}

gb::LexerLocation ProgramAssembler::GetMainLocation() const {
  return GetLexer().GetLineLocation(main_content_, 0);
}

std::string ProgramAssembler::GetLabelKey(std::string_view scope,
                                          std::string_view name) const {
  if (name.starts_with('@')) {
    return absl::StrCat(scope, name);
  }
  return std::string(name);
}

std::optional<ExpressionResult> ProgramAssembler::GetSymbolValue(
    std::string_view scope, std::string_view name, ExpressionWidth width) {
  // Only a local label needs a key built, and a constant is never local.
  auto label = name.starts_with('@') ? labels_.find(GetLabelKey(scope, name))
                                     : labels_.find(name);
  if (label != labels_.end()) {
    return ExpressionResult{
        .value = static_cast<uint32_t>(label->second.offset),
        .segment = label->second.segment};
  }

  // A constant with a value that can't be evaluated as a word, only as a
  // dword, is an error where it is used as one, without a location (see
  // ExpressionContext).
  ExpressionResult result;
  bool only_dword = false;
  if (auto it = constants_.find(name); it != constants_.end()) {
    result = EvaluateConstant(it->second, width);
    only_dword = (width == ExpressionWidth::kWord &&
                  result.kind == ExpressionResult::Kind::kError &&
                  EvaluateConstant(it->second, ExpressionWidth::kDword).kind ==
                      ExpressionResult::Kind::kValue);
  } else if (auto host = host_constants_.find(name);
             host != host_constants_.end()) {
    if (width == ExpressionWidth::kWord && host->second > 0xFFFF) {
      return ExpressionResult{.kind = ExpressionResult::Kind::kError,
                              .error = gb::ParseError(absl::StrCat(
                                  name, " can't be evaluated as a word: Value ",
                                  host->second, " does not fit in a word"))};
    }
    return ExpressionResult{.value = host->second};
  } else {
    return std::nullopt;
  }
  if (only_dword) {
    result.error = gb::ParseError(absl::StrCat(
        name, " can't be evaluated as a word: ", result.error.GetMessage()));
  }
  return result;
}

ExpressionResult ProgramAssembler::EvaluateConstant(Constant& constant,
                                                    ExpressionWidth width) {
  std::optional<ExpressionResult>& cached =
      constant.results[static_cast<int>(width)];
  if (cached.has_value()) {
    return *cached;
  }
  if (constant.evaluating) {
    return {.kind = ExpressionResult::Kind::kError,
            .error = gb::ParseError(
                GetLexer().GetTokenLocation(constant.token),
                absl::StrCat("The value of ", constant.token.GetString(),
                             " depends on itself"))};
  }
  constant.evaluating = true;
  ExpressionResult result = constant.expression.Evaluate(
      width, StatementContext(this, constant.segment, constant.offset,
                              constant.scope));
  constant.evaluating = false;

  // A symbol not yet defined may be defined later, so that isn't kept.
  if (result.kind != ExpressionResult::Kind::kUndefined) {
    cached = result;
  }
  return result;
}

bool ProgramAssembler::DefineLabel(const gb::Token& token,
                                   std::string_view name) {
  std::string key = GetLabelKey(scope_, name);
  if (!CheckNotDefined(token, key, name)) {
    return false;
  }
  UseSegment(token);
  labels_[std::move(key)] = {.token = token,
                             .segment = segment_,
                             .offset = GetSegment().reserved_size};
  return true;
}

bool ProgramAssembler::DefineConstant(const gb::Token& token,
                                      const gb::ParsedItem& value) {
  const std::string_view name = token.GetString();
  if (!CheckNotDefined(token, name, name)) {
    return false;
  }
  constants_.try_emplace(
      name, Constant{.token = token,
                     .segment = segment_,
                     .offset = GetSegment().reserved_size,
                     .scope = scope_,
                     .expression = Expression::Create(GetLexer(), value)});
  return true;
}

bool ProgramAssembler::CheckNotDefined(const gb::Token& token,
                                       std::string_view key,
                                       std::string_view name) {
  if (host_constants_.contains(key)) {
    Error(token, absl::StrCat(name, " is already defined by the host"));
    return false;
  }
  if (IsDefined(key)) {
    Error(token, absl::StrCat(name, " is already defined"));
    return false;
  }
  return true;
}

bool ProgramAssembler::IsDefined(std::string_view key) const {
  return labels_.contains(key) || constants_.contains(key) ||
         host_constants_.contains(key);
}

std::optional<uint32_t> ProgramAssembler::Evaluate(
    const Expression& expression, ExpressionWidth width,
    const ExpressionContext& context, int* segment) {
  ExpressionResult result = expression.Evaluate(width, context);
  if (result.kind != ExpressionResult::Kind::kValue) {
    AddError(std::move(result.error));
    return std::nullopt;
  }
  if (segment != nullptr) {
    *segment = result.segment;
  }
  return result.value;
}

std::optional<uint32_t> ProgramAssembler::EvaluateNow(
    const gb::ParsedItem& item) {
  return Evaluate(
      Expression::Create(GetLexer(), item), ExpressionWidth::kWord,
      StatementContext(this, segment_, GetSegment().reserved_size, scope_));
}

std::optional<int> ProgramAssembler::EvaluateInRange(const gb::ParsedItem& item,
                                                     int min, int max,
                                                     std::string_view what) {
  const std::optional<uint32_t> value = EvaluateNow(item);
  if (!value.has_value()) {
    return std::nullopt;
  }
  if (*value < static_cast<uint32_t>(min) ||
      *value > static_cast<uint32_t>(max)) {
    Error(item.GetToken(),
          absl::StrCat(what, " must be from ", min, " to ", max));
    return std::nullopt;
  }
  return static_cast<int>(*value);
}

//------------------------------------------------------------------------------
// First pass
//------------------------------------------------------------------------------

void ProgramAssembler::ReadContent(gb::LexerContentId content) {
  // An .IF ends in the content it starts in.
  std::vector<OpenIf> ifs;
  ReadLines(content, &ifs);
  for (int i = static_cast<int>(ifs.size()) - 1; i >= 0; --i) {
    Error(ifs[i].token, absl::StrCat(".", ifs[i].name, " has no .ENDIF"));
  }
}

void ProgramAssembler::ReadLines(gb::LexerContentId content,
                                 std::vector<OpenIf>* ifs) {
  while (!stopped_) {
    if (!ifs->empty() && ifs->back().state != IfState::kAssembling) {
      const SkippedLine skipped = SkipLine(content, ifs);
      if (skipped == SkippedLine::kEnd) {
        return;
      }
      if (skipped == SkippedLine::kSkipped) {
        continue;
      }
    }
    if (!expansions_.contains(content) && !ValidateNoMacroLabel(content)) {
      if (!ReadInvalidLine(content, ifs)) {
        return;
      }
      continue;
    }
    gb::ParseResult line = parser_->Parse(content, ProgramGrammar::kLineRule);
    if (!line.IsOk()) {
      // The lexer is left at the start of the line.
      AddError(line.GetError());
      if (!ReadInvalidLine(content, ifs)) {
        return;
      }
      continue;
    }
    ReadLine(content, *line, ifs);
    if (line->GetToken("end").IsEnd()) {
      return;
    }
  }
}

void ProgramAssembler::ReadLine(gb::LexerContentId content,
                                const gb::ParsedItem& line,
                                std::vector<OpenIf>* ifs) {
  const gb::ParsedItem* label = line.GetItem("label");
  const gb::ParsedItem* directive = line.GetItem("directive");
  if (directive != nullptr) {
    const std::string_view name = directive->GetString("directive");
    if (const Conditional conditional = GetConditional(name);
        conditional != Conditional::kNone) {
      if (label != nullptr) {
        Error(label->GetToken(),
              absl::StrCat(".", name, " can't have a label"));
      }
      ReadConditional(conditional, *directive, ifs);
      return;
    }
    if (name == "MACRO") {
      if (label != nullptr) {
        Error(label->GetToken(), ".MACRO can't have a label");
      }
      ReadMacro(content, *directive);
      return;
    }
    if (name == "ENDM") {
      Error(directive->GetToken(), ".ENDM has no .MACRO");
      if (label != nullptr) {
        ReadLabel(*label);
      }
      return;
    }
  }
  if (label != nullptr) {
    ReadLabel(*label);
  }
  if (directive != nullptr) {
    ReadDirective(*directive);
  } else if (const gb::ParsedItem* instruction = line.GetItem("instruction");
             instruction != nullptr) {
    ReadInstruction(content, *instruction);
  }
}

bool ProgramAssembler::ReadInvalidLine(gb::LexerContentId content,
                                       std::vector<OpenIf>* ifs) {
  // A line that doesn't parse is still read as far as a directive's name, for
  // the directives the lines after it depend on. A line that doesn't start
  // with a directive doesn't parse as a skipped line either, and is only moved
  // past, as is any other.
  gb::Lexer& lexer = GetLexer();
  const int line_index = lexer.GetCurrentLine(content);
  gb::ParseResult line =
      parser_->Parse(content, ProgramGrammar::kSkippedLineRule);
  const gb::ParsedItem* directive =
      line.IsOk() ? line->GetItem("directive") : nullptr;
  const std::string_view directive_name =
      directive != nullptr ? directive->GetString("directive") : "";
  const gb::Token next = lexer.NextToken(content, /*advance=*/false);
  lexer.NextLine(content);
  if (const Conditional conditional = GetConditional(directive_name);
      conditional != Conditional::kNone) {
    ReadInvalidConditional(conditional, *directive, ifs);
  } else if (directive_name == "MACRO") {
    // A .MACRO is still a body's start, which isn't assembled. Its name, if
    // it has one, is still defined, as an invalid macro, so its invocations
    // aren't errors too.
    ReadMacroBody(content, directive->GetToken(), nullptr);
    if (next.IsIdent() && syntax_->GetForms(next.GetString()).empty()) {
      macros_.try_emplace(next.GetString(),
                          Macro{.macro = std::make_unique<ProgramMacro>(
                                    next, std::vector<gb::Token>()),
                                .valid = false});
    }
  }
  return lexer.GetCurrentLine(content) != line_index;
}

bool ProgramAssembler::ValidateNoMacroLabel(gb::LexerContentId content) {
  gb::Lexer& lexer = GetLexer();
  const gb::Token start = lexer.NextToken(content, /*advance=*/false);
  bool valid = true;
  for (gb::Token token = lexer.NextToken(content); !IsLineEnd(token);
       token = lexer.NextToken(content)) {
    if (token.GetType() == kTokenMacroLabel) {
      Error(token, absl::StrCat("Macro label ", lexer.GetTokenText(token),
                                " can only be written by a macro's expansion"));
      valid = false;
      break;
    }
  }
  lexer.SetNextToken(start);
  return valid;
}

void ProgramAssembler::ReadLabel(const gb::ParsedItem& label) {
  if (const gb::ParsedItem* name = label.GetItem("name"); name != nullptr) {
    const gb::Token token = name->GetToken();
    if (DefineLabel(token, token.GetString())) {
      scope_ = token.GetString();
    }
    return;
  }
  const gb::Token token = label.GetToken("local_label");
  DefineLabel(token, token.GetString());
}

void ProgramAssembler::ReadDirective(const gb::ParsedItem& directive) {
  const std::string_view name = directive.GetString("directive");
  if (name == "INCLUDE") {
    ReadInclude(directive);
  } else if (name == "CONST") {
    ReadConst(directive);
  } else if (name == "DW") {
    ReadData(directive, ExpressionWidth::kWord);
  } else if (name == "DD") {
    ReadData(directive, ExpressionWidth::kDword);
  } else if (name == "DS") {
    ReadStorage(directive);
  } else if (name == "STACK") {
    ReadStack(directive);
  } else if (name == "BANK") {
    ReadBank(directive);
  } else if (name == "SEGMENT") {
    ReadSegment(directive);
  } else {
    DCHECK_EQ(name, "INSTANCES");
    ReadInstances(directive);
  }
}

void ProgramAssembler::ReadInclude(const gb::ParsedItem& directive) {
  const gb::Token token = directive.GetToken("path");
  if (file_system_ == nullptr) {
    Error(token, "A program assembled from text can't include files");
    return;
  }

  // A relative path is relative to the file that includes it.
  std::string path = gb::NormalizePath(token.GetString());
  if (!gb::IsPathAbsolute(path)) {
    path = gb::NormalizePath(
        gb::JoinPath(gb::RemoveFilename(including_.back()), path));
  }
  if (absl::c_linear_search(including_, path)) {
    Error(token, absl::StrCat(path, " is already being included"));
    return;
  }
  const gb::LexerContentId content =
      LoadFile(path, GetLexer().GetTokenLocation(token));
  if (content == gb::kNoLexerContent) {
    return;
  }
  including_.push_back(std::move(path));
  ReadContent(content);
  including_.pop_back();
}

void ProgramAssembler::ReadMacro(gb::LexerContentId content,
                                 const gb::ParsedItem& directive) {
  const gb::Token token = directive.GetToken();
  const gb::Token name = directive.GetToken("name");
  std::vector<gb::Token> parameters;
  for (const gb::ParsedItem& parameter : directive.GetItems("parameters")) {
    parameters.push_back(parameter.GetToken());
  }
  auto macro = std::make_unique<ProgramMacro>(name, std::move(parameters));
  bool valid = true;
  const absl::Span<const gb::Token> parameter_tokens = macro->GetParameters();
  for (int i = 0; i < static_cast<int>(parameter_tokens.size()); ++i) {
    if (macro->FindParameter(parameter_tokens[i]) != i) {
      Error(parameter_tokens[i], absl::StrCat("Macro ", name.GetString(),
                                              " already has the parameter ",
                                              parameter_tokens[i].GetString()));
      valid = false;
    }
  }

  // A macro that can't be defined still has its body read to its .ENDM.
  bool define = true;
  if (!syntax_->GetForms(name.GetString()).empty()) {
    Error(name, absl::StrCat("Macro ", name.GetString(),
                             " can't have an instruction's name"));
    define = false;
  } else if (macros_.contains(name.GetString())) {
    Error(name,
          absl::StrCat("Macro ", name.GetString(), " is already defined"));
    define = false;
  }
  valid = ReadMacroBody(content, token, macro.get()) && valid;
  if (define) {
    macros_.try_emplace(name.GetString(),
                        Macro{.macro = std::move(macro), .valid = valid});
  }
}

bool ProgramAssembler::ReadMacroBody(gb::LexerContentId content,
                                     const gb::Token& token,
                                     ProgramMacro* macro) {
  // A .MACRO in the body is an error, and its lines are skipped to its own
  // .ENDM, so the .ENDM after that still ends this body.
  gb::Lexer& lexer = GetLexer();
  bool valid = true;
  int nested = 0;
  while (true) {
    const gb::Token start = lexer.NextToken(content, /*advance=*/false);
    if (start.IsEnd()) {
      Error(token, ".MACRO has no .ENDM");
      return false;
    }

    // A line that doesn't start with a directive doesn't parse as a skipped
    // line, so it has no directive's name.
    gb::ParseResult line =
        parser_->Parse(content, ProgramGrammar::kSkippedLineRule);
    const gb::ParsedItem* directive =
        line.IsOk() ? line->GetItem("directive") : nullptr;
    const std::string_view directive_name =
        directive != nullptr ? directive->GetString("directive") : "";
    if (directive_name == "MACRO") {
      // Only the outermost .MACRO in the body is reported, as the ones in it
      // are in the lines skipped for its error.
      if (nested == 0) {
        Error(directive->GetToken(), ".MACRO can't be in a macro's body");
      }
      ++nested;
      valid = false;
    } else if (directive_name == "ENDM") {
      if (nested == 0) {
        if (line->GetItem("label") != nullptr ||
            !IsLineEnd(lexer.NextToken(content, /*advance=*/false))) {
          Error(start, ".ENDM must be on a line of its own");
        }
        lexer.NextLine(content);
        return valid;
      }
      --nested;
    } else if (nested == 0 && macro != nullptr) {
      // ReadLine() validates what the line can have, such as its labels and
      // directives, and moves past it.
      lexer.SetNextToken(start);
      if (gb::ParseError error; !macro->ReadLine(lexer, content, &error)) {
        AddError(std::move(error));
        valid = false;
      }
      continue;
    }
    lexer.NextLine(content);
  }
}

bool ProgramAssembler::ReadMacroInvocation(gb::LexerContentId content,
                                           const gb::ParsedItem& instruction) {
  // A name with an extension is always an instruction's.
  if (instruction.GetItem("extension") != nullptr) {
    return false;
  }
  const gb::Token name = instruction.GetToken("name");
  auto it = macros_.find(name.GetString());
  if (it == macros_.end()) {
    return false;
  }

  // The line is parsed again for the invocation's arguments, and the lexer is
  // put back after it.
  gb::Lexer& lexer = GetLexer();
  const gb::Token next = lexer.NextToken(content, /*advance=*/false);
  lexer.SetNextToken(name);
  gb::ParseResult invocation =
      parser_->Parse(content, ProgramGrammar::kMacroInvocationRule);
  lexer.SetNextToken(next);
  if (!invocation.IsOk()) {
    AddError(invocation.GetError());
    return true;
  }
  if (!ValidateNoArgDirective(content, name)) {
    return true;
  }
  std::vector<MacroArg> args;
  for (const gb::ParsedItem& arg : invocation->GetItems("args")) {
    args.push_back({.first = arg.GetToken(),
                    .last = arg.GetItems("tokens").back().GetToken("last")});
  }

  // An invalid macro's invocations expand to nothing, as its error is already
  // reported.
  Macro& macro = it->second;
  if (!macro.valid) {
    return true;
  }
  const ProgramMacro* definition = macro.macro.get();
  const int arg_count = static_cast<int>(args.size());
  const int parameter_count =
      static_cast<int>(definition->GetParameters().size());
  if (arg_count != parameter_count) {
    Error(name,
          absl::StrCat("Macro ", name.GetString(), " takes ", parameter_count,
                       parameter_count == 1 ? " argument" : " arguments",
                       ", not ", arg_count));
    return true;
  }
  bool args_valid = true;
  for (int i = 0; i < arg_count; ++i) {
    if (definition->IsExtended(i) && !args[i].last.IsIdent()) {
      Error(args[i].last,
            absl::StrCat("Macro ", name.GetString(),
                         " writes an extension after ",
                         definition->GetParameters()[i].GetString(),
                         ", so its argument must end in a name"));
      args_valid = false;
    }
  }
  if (!args_valid) {
    return true;
  }
  for (const Expansion* expansion = FindExpansion(content);
       expansion != nullptr;
       expansion =
           FindExpansion(lexer.GetTokenLocation(expansion->invocation).id)) {
    if (expansion->macro == definition) {
      Error(name,
            absl::StrCat("Macro ", name.GetString(), " can't invoke itself"));
      return true;
    }
  }
  auto expansion = std::make_unique<MacroExpansion>(lexer, *definition,
                                                    ++macro.invocations, args);
  if (expansion->GetLineCount() == 0) {
    return true;
  }
  const gb::LexerContentId expansion_content =
      lexer.AddContent(expansion->GetText());
  if (expansion_content == gb::kNoLexerContent) {
    Error(name, absl::StrCat("This invocation of ", name.GetString(),
                             " has too many lines, or a line too long"));
    return true;
  }
  expansions_[expansion_content] = {
      .text = std::move(expansion), .macro = definition, .invocation = name};
  ReadContent(expansion_content);
  return true;
}

bool ProgramAssembler::ValidateNoArgDirective(gb::LexerContentId content,
                                              const gb::Token& name) {
  // A '.' with a name right after it is a directive, unless it is right after
  // a name too, as an instruction's extension is ("ADD.W").
  gb::Lexer& lexer = GetLexer();
  const gb::Token next = lexer.NextToken(content, /*advance=*/false);
  lexer.SetNextToken(name);
  bool valid = true;
  gb::Token previous = lexer.NextToken(content);
  for (gb::Token token = lexer.NextToken(content); !IsLineEnd(token);
       previous = token, token = lexer.NextToken(content)) {
    if (!token.IsSymbol('.')) {
      continue;
    }
    const gb::Token after = lexer.NextToken(content, /*advance=*/false);
    if (after.IsIdent() && IsNextTo(lexer, token, after) &&
        !(previous.IsIdent() && IsNextTo(lexer, previous, token))) {
      Error(token, "A macro's argument can't have a directive");
      valid = false;
    }
  }
  lexer.SetNextToken(next);
  return valid;
}

void ProgramAssembler::ReadConst(const gb::ParsedItem& directive) {
  const gb::Token name = directive.GetToken("name");
  if (DefineConstant(name, *directive.GetItem("value"))) {
    AddStatement(directive.GetToken(), ConstantStatement{.name = name}, 0);
  }
}

void ProgramAssembler::ReadData(const gb::ParsedItem& directive,
                                ExpressionWidth width) {
  // An item with SIZE starts a field of that many values of the width, which
  // holds it and the items after it up to the next SIZE, and then zeros. The
  // items must fit in their field. Items before the first SIZE, or after a
  // SIZE with an error, are only their own words.
  DataStatement data = {.width = width};
  const int value_size = (width == ExpressionWidth::kWord ? 1 : 2);
  int64_t size = 0;
  uint32_t field_count = 0;
  std::optional<int64_t> field_end;
  for (const gb::ParsedItem& item : directive.GetItems("items")) {
    if (const gb::ParsedItem* size_item = item.GetItem("size");
        size_item != nullptr) {
      EndDataField(field_end, &size, &data);
      field_end.reset();
      if (const std::optional<uint32_t> count = EvaluateNow(*size_item);
          count.has_value()) {
        field_count = *count;
        field_end = size + int64_t{*count} * value_size;
      }
    }

    Expression expression =
        Expression::Create(GetLexer(), *item.GetItem("value"));
    const gb::Token token = expression.GetToken();
    const int64_t start = size;
    if (width == ExpressionWidth::kWord && expression.IsString()) {
      absl::StatusOr<std::vector<uint16_t>> words =
          GetStringWords(GetLexer(), token);
      if (!words.ok()) {
        Error(token, words.status().message());
        continue;
      }
      size += static_cast<int64_t>(words->size());
      data.items.emplace_back(*std::move(words));
    } else {
      size += value_size;
      data.items.emplace_back(std::move(expression));
    }
    if (field_end.has_value() && start <= *field_end && size > *field_end) {
      Error(token, absl::StrCat("This passes the end of its field, at its "
                                "SIZE of ",
                                field_count));
    }
  }
  EndDataField(field_end, &size, &data);
  Write(size, directive.GetToken(), std::move(data));
}

void ProgramAssembler::EndDataField(std::optional<int64_t> field_end,
                                    int64_t* size, DataStatement* data) {
  // A field's items that pass its end are an error, and the field is still
  // its SIZE, so the statements after it are where they would be.
  if (!field_end.has_value()) {
    return;
  }
  if (*size < *field_end) {
    data->items.emplace_back(DataPadding{.words = *field_end - *size});
  }
  *size = *field_end;
}

void ProgramAssembler::ReadStorage(const gb::ParsedItem& directive) {
  const std::optional<uint32_t> count =
      EvaluateNow(*directive.GetItem("count"));
  if (count.has_value()) {
    Reserve(*count, directive.GetToken());
  }
}

void ProgramAssembler::ReadStack(const gb::ParsedItem& directive) {
  const gb::Token token = directive.GetToken();
  if (stack_token_.has_value()) {
    Error(token, "The program's .STACK is already given");
    return;
  }
  stack_token_ = token;
  const std::optional<uint32_t> words =
      EvaluateNow(*directive.GetItem("words"));
  if (words.has_value()) {
    stack_words_ = static_cast<uint16_t>(*words);
  }
}

void ProgramAssembler::ReadInstances(const gb::ParsedItem& directive) {
  const gb::Token token = directive.GetToken();
  if (instances_token_.has_value()) {
    Error(token, "The program's .INSTANCES is already given");
    return;
  }
  instances_token_ = token;
  const std::optional<int> count = EvaluateInRange(
      *directive.GetItem("count"), 1, kMaxCores, "The number of instances");
  if (count.has_value()) {
    instance_count_ = *count;
  }
}

void ProgramAssembler::ReadBank(const gb::ParsedItem& directive) {
  const gb::Token name = directive.GetToken("name");
  if (bank_indices_.contains(name.GetString())) {
    Error(name,
          absl::StrCat("The bank ", name.GetString(), " is already declared"));
    return;
  }

  // A mask with an error is reported, and the bank is still declared, so its
  // segments aren't errors too.
  Bank bank = {.name = name};
  if (const gb::ParsedItem* mask = directive.GetItem("mask"); mask != nullptr) {
    const std::optional<uint32_t> value = EvaluateNow(*mask);
    if (value.has_value() && *value == 0) {
      Error(mask->GetToken(),
            "A bank's mask needs a bit for at least one physical bank");
    } else if (value.has_value()) {
      bank.assign_mask = static_cast<uint16_t>(*value);
    }
  }
  bank_indices_[name.GetString()] = static_cast<int>(banks_.size());
  banks_.push_back(bank);
}

void ProgramAssembler::ReadSegment(const gb::ParsedItem& directive) {
  const gb::Token name = directive.GetToken("name");
  const absl::Span<const gb::ParsedItem> attributes =
      directive.GetItems("attributes");
  if (auto it = segment_indices_.find(name.GetString());
      it != segment_indices_.end()) {
    segment_ = it->second;
    if (!attributes.empty()) {
      Error(attributes.front().GetToken(),
            absl::StrCat("A segment's attributes are only given where it is "
                         "first declared, and ",
                         name.GetString(), " is already declared"));
    }
    return;
  }

  // The first declared segment replaces the default program's.
  if (segment_indices_.empty()) {
    if (first_segment_use_.has_value()) {
      Error(*first_segment_use_,
            "The program declares segments, so this must be in one, after a "
            ".SEGMENT");
    }
    segments_.clear();
    roles_.fill(-1);

    // Only constants can be before the first .SEGMENT, and they are in no
    // segment once the default program's is gone, even where a first pass
    // value already used them.
    for (auto& [name, constant] : constants_) {
      constant.segment = -1;
      constant.results = {};
    }
  }
  segment_ = static_cast<int>(segments_.size());
  Segment& segment = segments_.emplace_back(Segment{.name = name});
  segment_indices_[name.GetString()] = segment_;

  absl::flat_hash_set<std::string_view> given;
  for (const gb::ParsedItem& attribute : attributes) {
    const gb::Token token = attribute.GetToken();
    if (!given.insert(token.GetString()).second) {
      Error(token, absl::StrCat(token.GetString(), " is already given"));
    } else if (attribute.GetItem("role") != nullptr) {
      ReadSegmentRole(token);
    } else if (attribute.GetItem("rom") != nullptr) {
      segment.access = Program::Segment::Access::kReadOnly;
    } else if (const gb::ParsedItem* page = attribute.GetItem("page");
               page != nullptr) {
      const std::optional<int> value = EvaluateInRange(
          *page, 0, kMemoryBankPageCount - 1, "A segment's page");
      if (value.has_value()) {
        segment.page_start = static_cast<uint16_t>(*value);
        segment.page_token = token;
      }
    } else if (const gb::ParsedItem* pages = attribute.GetItem("pages");
               pages != nullptr) {
      const std::optional<int> value = EvaluateInRange(
          *pages, 1, kMemoryBankPageCount, "A segment's size in pages");
      if (value.has_value()) {
        segment.page_count = static_cast<uint16_t>(*value);
        segment.pages_token = token;
      }
    } else if (attribute.GetItem("instance") != nullptr) {
      segment.instance_token = token;
    } else {
      segment.bank_name = attribute.GetToken("bank");
    }
  }

  // Code is shared by every instance, so instances never offset BC.
  if (segment.instance_token.has_value() && roles_[CpuCore::CODE] == segment_) {
    Error(*segment.instance_token,
          absl::StrCat("Code is shared by every instance, so ",
                       name.GetString(), " can't have both CODE and INSTANCE"));
  }
}

void ProgramAssembler::ReadSegmentRole(const gb::Token& role) {
  const std::string_view name = role.GetString();
  const int index = CpuCore::GetBankFromName(name);
  DCHECK_GE(index, 0);
  const int other = roles_[index];
  if (other >= 0) {
    Error(role, absl::StrCat("The ", name, " role is already the segment ",
                             segments_[other].name->GetString(), "'s"));
  } else {
    roles_[index] = segment_;
  }
}

void ProgramAssembler::ReadInstruction(gb::LexerContentId content,
                                       const gb::ParsedItem& instruction) {
  if (ReadMacroInvocation(content, instruction)) {
    return;
  }
  std::vector<InstructionMatch> matches;
  gb::ParseError error;
  if (!grammar_->MatchInstruction(
          *parser_, content, instruction,
          StatementContext(this, segment_, GetSegment().reserved_size, scope_),
          matches, error)) {
    AddError(std::move(error));
    return;
  }
  if (matches.size() != 1) {
    const std::string_view name =
        ProgramGrammar::GetInstructionName(GetLexer(), instruction);
    std::vector<const InstructionForm*> forms;
    std::string message;
    if (matches.empty()) {
      message = absl::StrCat("No form of ", name, " matches. Its forms are:");
      absl::c_copy(syntax_->GetForms(name), std::back_inserter(forms));
    } else {
      message = absl::StrCat("More than one form of ", name, " matches:");
      for (const InstructionMatch& match : matches) {
        forms.push_back(match.form);
      }
    }
    for (const InstructionForm* form : forms) {
      absl::StrAppend(&message, "\n  ", GetFormText(*form));
    }
    Error(instruction.GetToken(), message);
    return;
  }

  // Encode() validates the code word values too, but this says which value or
  // register is at fault. The values after the code word aren't known yet.
  InstructionMatch& match = matches.front();
  int arg_index = -1;
  if (std::string error; !match.form->ValidateCodeWordValues(
          GetCodeWordValues(*match.form, match.args), &arg_index, &error)) {
    DCHECK_GE(arg_index, 0);
    Error(match.args[arg_index].token, error);
  }
  Write(
      match.form->GetSize(), instruction.GetToken(),
      InstructionStatement{.form = match.form, .args = std::move(match.args)});
}

void ProgramAssembler::AddStatement(const gb::Token& token,
                                    StatementContents contents, int64_t size) {
  statements_.push_back(
      {.token = token,
       .segment = segment_,
       .offset = static_cast<uint16_t>(GetSegment().reserved_size),
       .size = static_cast<int>(size),
       .scope = scope_,
       .contents = std::move(contents)});
}

void ProgramAssembler::UseSegment(const gb::Token& token) {
  if (!first_segment_use_.has_value()) {
    first_segment_use_ = token;
  }
}

void ProgramAssembler::Reserve(int64_t size, const gb::Token& token) {
  UseSegment(token);
  Segment& segment = GetSegment();
  CheckSegmentEnd(segment, size, token, "This");
  segment.reserved_size += size;
}

void ProgramAssembler::Write(int64_t size, const gb::Token& token,
                             StatementContents contents) {
  AddStatement(token, std::move(contents), size);
  Reserve(size, token);
  Segment& segment = GetSegment();
  segment.written_size = segment.reserved_size;

  // The loader writes a program's words once, so only one instance would get
  // them.
  if (segment.instance_token.has_value()) {
    Error(token,
          absl::StrCat("This writes words, but ", segment.name->GetString(),
                       " is per instance, so it can only reserve them, "
                       "with .DS"));
  }
}

void ProgramAssembler::CheckSegmentEnd(const Segment& segment, int64_t size,
                                       const gb::Token& token,
                                       std::string_view what) {
  if (segment.reserved_size <= kMemoryBankMaxSize &&
      segment.reserved_size + size > kMemoryBankMaxSize) {
    Error(token, absl::StrCat(what, " passes the end of the segment, at ",
                              kMemoryBankMaxSize, " words"));
  }
}

void ProgramAssembler::EndFirstPass() {
  ReserveStack();
  for (Segment& segment : segments_) {
    SetBank(segment);
    SetPageCount(segment);
  }
}

void ProgramAssembler::ReserveStack() {
  // The stack segment needs a size for the stack, which is after its
  // contents, and only it has one.
  const int stack_segment = roles_[CpuCore::STACK];
  if (stack_segment < 0) {
    if (stack_token_.has_value()) {
      Error(*stack_token_,
            "No segment has the STACK role, so the program can't have .STACK");
    }
    return;
  }
  Segment& segment = segments_[stack_segment];

  // With more than one instance, a shared stack would be every instance's.
  if (instance_count_ > 1 && !segment.instance_token.has_value()) {
    if (segment.name.has_value()) {
      Error(*segment.name,
            absl::StrCat("The program has ", instance_count_,
                         " instances, so its stack segment, ",
                         segment.name->GetString(), ", needs INSTANCE"));
    } else {
      Error(*instances_token_,
            absl::StrCat("The program has ", instance_count_,
                         " instances, so it needs segments, with a per "
                         "instance stack segment"));
    }
  }
  if (!stack_token_.has_value()) {
    constexpr std::string_view kMessage =
        "The program has a stack, so it needs its size, with .STACK";
    if (segment.name.has_value()) {
      Error(*segment.name, kMessage);
    } else {
      AddError(gb::ParseError(GetMainLocation(), kMessage));
    }
    return;
  }
  CheckSegmentEnd(segment, stack_words_, *stack_token_, "The stack");
  segment.reserved_size += stack_words_;
}

void ProgramAssembler::SetBank(Segment& segment) {
  // Without .BANK, every segment is in the program's one bank. With it, each
  // segment names its bank.
  if (!segment.bank_name.has_value()) {
    if (banks_.empty()) {
      return;
    }
    if (segment.name.has_value()) {
      Error(*segment.name,
            absl::StrCat("The program declares banks, so ",
                         segment.name->GetString(), " needs a BANK"));
    } else {
      Error(banks_.front().name,
            "The program declares banks, so it needs segments, each with a "
            "BANK");
    }
    return;
  }
  const std::string_view name = segment.bank_name->GetString();
  auto it = bank_indices_.find(name);
  if (it == bank_indices_.end()) {
    Error(*segment.bank_name, absl::StrCat("No bank ", name, " is declared"));
    return;
  }
  segment.bank = static_cast<uint16_t>(it->second);
}

void ProgramAssembler::SetPageCount(Segment& segment) {
  // A segment past the end of a bank is already reported where it passes it.
  if (segment.reserved_size > kMemoryBankMaxSize) {
    return;
  }

  // Its size is its reserved size, for each instance if it is per instance.
  int64_t size = segment.reserved_size;
  if (segment.instance_token.has_value()) {
    size *= instance_count_;
    if (size > kMemoryBankMaxSize) {
      Error(*segment.instance_token,
            absl::StrCat(segment.name->GetString(), "'s ", instance_count_,
                         " instances of ", segment.reserved_size,
                         " words pass the end of the segment, at ",
                         kMemoryBankMaxSize, " words"));
      return;
    }
  }

  // Its size in whole pages, which PAGES may give more of, at the end.
  const int64_t pages = std::max<int64_t>(
      1, (size + kMemoryBankPageSize - 1) / kMemoryBankPageSize);
  if (!segment.pages_token.has_value()) {
    segment.page_count = static_cast<uint16_t>(pages);
  } else if (pages > segment.page_count) {
    Error(
        *segment.pages_token,
        absl::StrCat(segment.name->GetString(), " needs ", pages, " pages (",
                     size, " words), but PAGES gives it ", segment.page_count));
  }
  if (segment.page_token.has_value() &&
      segment.page_start + segment.page_count > kMemoryBankPageCount) {
    Error(*segment.page_token,
          absl::StrCat(segment.name->GetString(), "'s ", segment.page_count,
                       " pages from page ", segment.page_start,
                       " pass the end of the bank, at page ",
                       kMemoryBankPageCount));
  }
}

//------------------------------------------------------------------------------
// Conditional assembly
//------------------------------------------------------------------------------

ProgramAssembler::IfState ProgramAssembler::GetIfState(
    std::optional<bool> condition) {
  if (!condition.has_value()) {
    return IfState::kDisabled;
  }
  return *condition ? IfState::kAssembling : IfState::kUnsatisfied;
}

ProgramAssembler::SkippedLine ProgramAssembler::SkipLine(
    gb::LexerContentId content, std::vector<OpenIf>* ifs) {
  gb::Lexer& lexer = GetLexer();
  const gb::Token start = lexer.NextToken(content, false);
  if (start.IsEnd()) {
    return SkippedLine::kEnd;
  }

  // A line that doesn't start with a directive leaves the lexer at its start.
  gb::ParseResult line =
      parser_->Parse(content, ProgramGrammar::kSkippedLineRule);
  const gb::ParsedItem* directive =
      line.IsOk() ? line->GetItem("directive") : nullptr;
  const std::string_view name =
      directive != nullptr ? directive->GetString("directive") : "";
  const Conditional conditional = GetConditional(name);
  if (conditional != Conditional::kNone) {
    OpenIf& open = ifs->back();
    if (open.state != IfState::kDisabled && conditional != Conditional::kIf) {
      lexer.SetNextToken(start);
      return SkippedLine::kRead;
    }
    if (const gb::ParsedItem* label = line->GetItem("label");
        label != nullptr) {
      Error(label->GetToken(), absl::StrCat(".", name, " can't have a label"));
    }

    // A disabled .IF's directives are checked as any others, but it assembles
    // none of its blocks.
    if (conditional == Conditional::kElseIf ||
        conditional == Conditional::kElse) {
      if (open.has_else) {
        Error(directive->GetToken(),
              absl::StrCat(".", name, " can't come after .ELSE"));
      }
      open.has_else = open.has_else || conditional == Conditional::kElse;
    } else if (conditional == Conditional::kIf) {
      ifs->push_back({.token = directive->GetToken(),
                      .name = name,
                      .state = IfState::kDisabled});
    } else if (conditional == Conditional::kEndIf) {
      ifs->pop_back();
    }
  }
  lexer.NextLine(content);
  return SkippedLine::kSkipped;
}

void ProgramAssembler::ReadConditional(Conditional conditional,
                                       const gb::ParsedItem& directive,
                                       std::vector<OpenIf>* ifs) {
  const std::string_view name = directive.GetString("directive");
  const gb::Token token = directive.GetToken();
  // An .IF assembles its first block if its condition is nonzero.
  if (conditional == Conditional::kIf) {
    ifs->push_back({.token = token,
                    .name = name,
                    .state = GetIfState(ReadCondition(directive))});
    return;
  }
  if (ifs->empty()) {
    Error(token, absl::StrCat(".", name, " has no .IF"));
    return;
  }

  // A disabled .IF's directives are read by SkipLine() instead.
  OpenIf& open = ifs->back();
  DCHECK(open.state != IfState::kDisabled);
  if (conditional == Conditional::kEndIf) {
    ifs->pop_back();
    return;
  }

  // The rest is an .ELSEIF or .ELSE, which starts the .IF's next block.
  if (open.has_else) {
    Error(token, absl::StrCat(".", name, " can't come after .ELSE"));
    open.state = IfState::kDisabled;
    return;
  }
  open.has_else = conditional == Conditional::kElse;

  // Once the .IF has assembled a block (it is kAssembling or kSatisfied), its
  // later blocks are skipped, and their conditions aren't read.
  if (open.state != IfState::kUnsatisfied) {
    open.state = IfState::kSatisfied;
    return;
  }

  // Until then, an .ELSE's block is assembled, and an .ELSEIF's is if its
  // condition is nonzero.
  if (conditional == Conditional::kElse) {
    open.state = IfState::kAssembling;
    return;
  }
  open.state = GetIfState(ReadCondition(directive));
}

void ProgramAssembler::ReadInvalidConditional(Conditional conditional,
                                              const gb::ParsedItem& directive,
                                              std::vector<OpenIf>* ifs) {
  // A conditional directive is still an .IF's start, block, or end if the rest
  // of its line is invalid, so the lines after it are read as they would be,
  // as for a condition with an error.
  const std::string_view name = directive.GetString("directive");
  if (conditional == Conditional::kIf) {
    ifs->push_back({.token = directive.GetToken(),
                    .name = name,
                    .state = IfState::kDisabled});
    return;
  }
  if (ifs->empty()) {
    return;
  }
  if (conditional == Conditional::kEndIf) {
    ifs->pop_back();
    return;
  }
  OpenIf& open = ifs->back();
  open.state = IfState::kDisabled;
  open.has_else = open.has_else || conditional == Conditional::kElse;
}

std::optional<bool> ProgramAssembler::ReadCondition(
    const gb::ParsedItem& directive) {
  if (const gb::ParsedItem* condition = directive.GetItem("condition");
      condition != nullptr) {
    const std::optional<uint32_t> value = EvaluateNow(*condition);
    if (!value.has_value()) {
      return std::nullopt;
    }
    return *value != 0;
  }

  // .IFDEF or .IFNDEF, of a label or constant defined above.
  const bool defined = IsDefined(directive.GetToken("name").GetString());
  return directive.GetString("directive") == "IFDEF" ? defined : !defined;
}

//------------------------------------------------------------------------------
// Second pass
//------------------------------------------------------------------------------

void ProgramAssembler::FinishStatement(const Statement& statement) {
  std::vector<uint16_t> words;
  if (const auto* instruction =
          std::get_if<InstructionStatement>(&statement.contents);
      instruction != nullptr) {
    EncodeInstruction(statement, *instruction, words);
  } else if (const auto* data = std::get_if<DataStatement>(&statement.contents);
             data != nullptr) {
    EncodeData(statement, *data, words);
  } else if (const auto* constant =
                 std::get_if<ConstantStatement>(&statement.contents);
             constant != nullptr) {
    CheckConstant(*constant);
  }
  Segment& segment = segments_[statement.segment];
  for (int i = 0; i < static_cast<int>(words.size()); ++i) {
    segment.words[statement.offset + i] = words[i];
    segment.written[statement.offset + i] = true;
  }
}

void ProgramAssembler::CheckConstant(const ConstantStatement& constant) {
  // A constant's value as a symbol is its dword value, so it must have one,
  // whether it is used or not.
  auto it = constants_.find(constant.name.GetString());
  if (it == constants_.end()) {
    return;
  }
  ExpressionResult result =
      EvaluateConstant(it->second, ExpressionWidth::kDword);
  if (result.kind != ExpressionResult::Kind::kValue) {
    AddError(std::move(result.error));
  }
}

void ProgramAssembler::EncodeInstruction(
    const Statement& statement, const InstructionStatement& instruction,
    std::vector<uint16_t>& words) {
  // Registers and code word values are known from the first pass, and only $v
  // and $V are evaluated now.
  const InstructionForm& form = *instruction.form;
  const StatementContext context(this, statement.segment, statement.offset,
                                 statement.scope);
  std::vector<int64_t> values = GetCodeWordValues(form, instruction.args);
  bool ok = true;
  for (int i = 0; i < static_cast<int>(instruction.args.size()); ++i) {
    const ArgKind kind = form.GetArgs()[i].kind;
    if (kind != ArgKind::kWord && kind != ArgKind::kDword) {
      continue;
    }
    const int bank = GetValueTypeBank(form.GetArgs()[i].value_type);
    const std::optional<uint32_t> value =
        bank >= 0 ? EvaluateAddress(*instruction.args[i].value, bank, context)
                  : Evaluate(*instruction.args[i].value,
                             kind == ArgKind::kWord ? ExpressionWidth::kWord
                                                    : ExpressionWidth::kDword,
                             context);
    ok = ok && value.has_value();
    values[i] = value.value_or(0);
  }
  std::string error;
  if (ok && !form.Encode(values, statement.offset, words, &error)) {
    Error(statement.token, error);
  }
}

std::optional<uint32_t> ProgramAssembler::EvaluateAddress(
    const Expression& expression, int bank, const ExpressionContext& context) {
  int segment = -1;
  const std::optional<uint32_t> value =
      Evaluate(expression, ExpressionWidth::kWord, context, &segment);
  if (!value.has_value() || segment < 0) {
    return value;
  }

  // The default program's one segment has every role, so a segment that
  // doesn't have this one is a declared segment, with a name.
  const int role_segment = roles_[bank];
  if (segment != role_segment) {
    const std::string_view bank_name = CpuCore::GetBankName(bank);
    const std::string expected =
        role_segment < 0
            ? absl::StrCat("the segment with the ", bank_name,
                           " role, and no segment has it")
            : absl::StrCat(segments_[role_segment].name->GetString(),
                           ", the segment with the ", bank_name, " role");
    Error(
        expression.GetToken(),
        absl::StrCat("The address is in ", segments_[segment].name->GetString(),
                     ", but must be in ", expected));
    return std::nullopt;
  }

  // A bare program starts BC and BD at its start, as loading a program file
  // does, but BS at the end of its stack words, which isn't on a page, and
  // leaves BE as it is.
  if (!segments_[role_segment].name.has_value() && !bare_error_.has_value() &&
      (bank == CpuCore::STACK || bank == CpuCore::EXTRA)) {
    const gb::LexerLocation location =
        GetSourceLocation(GetLexer().GetTokenLocation(expression.GetToken()));
    bare_error_ = absl::StrCat(
        "The label at ", location.filename, "(", location.line + 1, ":",
        location.column + 1, ") is in an address from ",
        bank == CpuCore::STACK
            ? "BS, which a bare program starts at the end of its stack "
              "words, not its pages"
            : "BE, which a bare program leaves as it is",
        ", so it can't be a bare program");
  }

  // Only BS isn't at the start of its segment, or of an instance's words. It
  // is the stack's offset (as the loader has it) down from the segment's end,
  // which is the same distance past the start of every instance's words.
  if (bank != CpuCore::STACK) {
    return value;
  }
  const int64_t base =
      int64_t{segments_[role_segment].page_count} * kMemoryBankPageSize -
      GetInstanceOffset(CpuCore::STACK, 0);
  return static_cast<uint16_t>(*value - base);
}

void ProgramAssembler::EncodeData(const Statement& statement,
                                  const DataStatement& data,
                                  std::vector<uint16_t>& words) {
  const StatementContext context(this, statement.segment, statement.offset,
                                 statement.scope);
  words.reserve(statement.size);
  for (const DataItem& item : data.items) {
    if (const auto* string_words = std::get_if<std::vector<uint16_t>>(&item);
        string_words != nullptr) {
      words.insert(words.end(), string_words->begin(), string_words->end());
    } else if (const auto* expression = std::get_if<Expression>(&item);
               expression != nullptr) {
      // A value with an error is reported, and written as zero, so every item
      // writes exactly its own words.
      const uint32_t value =
          Evaluate(*expression, data.width, context).value_or(0);

      // A dword is written low word first.
      words.push_back(static_cast<uint16_t>(value));
      if (data.width == ExpressionWidth::kDword) {
        words.push_back(static_cast<uint16_t>(value >> 16));
      }
    } else if (const auto* padding = std::get_if<DataPadding>(&item);
               padding != nullptr) {
      words.resize(words.size() + padding->words, 0);
    }
  }

  // Items that pass their field's end are a first pass error, so there is no
  // second pass.
  DCHECK_EQ(static_cast<int>(words.size()), statement.size);
}

std::unique_ptr<AsmProgram> ProgramAssembler::BuildProgram() {
  // The declared banks, or one bank that may be any physical bank, the
  // segments, their roles, and the instances.
  auto result = absl::WrapUnique(new AsmProgram);
  Program& program = result->program_;
  if (banks_.empty()) {
    program.banks.emplace_back();
  }
  for (const Bank& bank : banks_) {
    program.banks.push_back({.assign_mask = bank.assign_mask});
  }
  for (int i = 0; i < static_cast<int>(segments_.size()); ++i) {
    program.segments.push_back(BuildSegment(i));
  }
  program.code_segment = GetRoleSegment(CpuCore::CODE);
  program.stack_segment = GetRoleSegment(CpuCore::STACK);
  program.data_segment = GetRoleSegment(CpuCore::DATA);
  program.extra_segment = GetRoleSegment(CpuCore::EXTRA);
  for (int i = 0; i < instance_count_; ++i) {
    program.instances.push_back(
        {.code = GetInstanceOffset(CpuCore::CODE, i),
         .stack = GetInstanceOffset(CpuCore::STACK, i),
         .data = GetInstanceOffset(CpuCore::DATA, i),
         .extra = GetInstanceOffset(CpuCore::EXTRA, i)});
  }

  // The assembler reports what a program breaks itself, but this is a last
  // check of the rest.
  if (absl::Status status = ValidateProgram(program); !status.ok()) {
    AddError(gb::ParseError(
        GetMainLocation(),
        absl::StrCat("The program isn't valid: ", status.message())));
    return nullptr;
  }
  result->bare_program_ = BuildBareProgram(program);
  result->map_ = BuildMap();
  return result;
}

absl::StatusOr<BareProgram> ProgramAssembler::BuildBareProgram(
    const Program& program) const {
  // Only the default program can be a bare program. It can't declare banks or
  // more than one instance either, as the first pass reports: .BANK and
  // .INSTANCES above 1 each need segments.
  const Segment& segment = segments_.front();
  if (segment.name.has_value()) {
    return absl::FailedPreconditionError(
        "The program declares segments, so it can't be a bare program");
  }
  if (bare_error_.has_value()) {
    return absl::FailedPreconditionError(*bare_error_);
  }

  // Its contents are the segment's words before its stack, rounded up to
  // whole units, as its data blocks are.
  constexpr int kAlignment = Program::Segment::Data::kAlignment;
  const int contents_words =
      static_cast<int>(segment.reserved_size) - stack_words_;
  BareProgram bare_program = {
      .contents_words =
          (contents_words + kAlignment - 1) / kAlignment * kAlignment,
      .stack_words = stack_words_,
      .data = program.segments.front().data,
  };
  if (absl::Status status = ValidateBareProgram(bare_program); !status.ok()) {
    return absl::FailedPreconditionError(absl::StrCat(
        "The program can't be a bare program: ", status.message()));
  }
  return bare_program;
}

Program::Segment ProgramAssembler::BuildSegment(int index) const {
  const Segment& segment = segments_[index];
  Program::Segment result = {.bank = segment.bank,
                             .access = segment.access,
                             .page_start = segment.page_start,
                             .page_count = segment.page_count};

  // The data is each run of aligned pairs of words with a word written, as
  // blocks. The words are whole pairs, so a word in a pair that isn't written
  // is 0.
  constexpr int kAlignment = Program::Segment::Data::kAlignment;
  const int word_count = static_cast<int>(segment.words.size());
  for (int offset = 0; offset < word_count; offset += kAlignment) {
    const auto start = segment.written.begin() + offset;
    if (std::find(start, start + kAlignment, true) == start + kAlignment) {
      continue;
    }
    if (result.data.empty() ||
        result.data.back().offset +
                static_cast<int>(result.data.back().data.size()) !=
            offset) {
      result.data.push_back({.offset = static_cast<uint16_t>(offset)});
    }
    std::vector<uint16_t>& data = result.data.back().data;
    data.insert(data.end(), segment.words.begin() + offset,
                segment.words.begin() + offset + kAlignment);
  }
  return result;
}

ProgramMap ProgramAssembler::BuildMap() {
  std::vector<std::string> segments;
  segments.reserve(segments_.size());
  for (const Segment& segment : segments_) {
    segments.emplace_back(segment.name.has_value() ? segment.name->GetString()
                                                   : "");
  }

  // Statements that write no words, such as .CONST, aren't in the map.
  std::vector<ProgramMap::Statement> statements;
  statements.reserve(statements_.size());
  for (const Statement& statement : statements_) {
    if (statement.size > 0) {
      statements.push_back({.segment = statement.segment,
                            .offset = statement.offset,
                            .size = statement.size,
                            .location = GetMapLocation(statement.token)});
    }
  }

  std::vector<ProgramMap::Label> labels;
  labels.reserve(labels_.size());
  for (const auto& [key, label] : labels_) {
    labels.push_back({.name = key,
                      .segment = label.segment,
                      .offset = static_cast<int>(label.offset),
                      .location = GetMapLocation(label.token)});
  }

  // Every constant has a dword value, which CheckConstant() evaluated, but a
  // word value is only an error where it is used as a word.
  std::vector<ProgramMap::Constant> constants;
  constants.reserve(constants_.size());
  for (auto& [name, constant] : constants_) {
    const ExpressionResult dword =
        EvaluateConstant(constant, ExpressionWidth::kDword);
    CHECK(dword.kind == ExpressionResult::Kind::kValue);
    const ExpressionResult word =
        EvaluateConstant(constant, ExpressionWidth::kWord);
    std::optional<uint16_t> word_value;
    if (word.kind == ExpressionResult::Kind::kValue) {
      word_value = static_cast<uint16_t>(word.value);
    }
    constants.push_back({.name = name,
                         .word_value = word_value,
                         .dword_value = dword.value,
                         .location = GetMapLocation(constant.token)});
  }

  // The assembler made the map, so it is valid.
  absl::StatusOr<ProgramMap> map = ProgramMap::Create(
      file_paths_, std::move(segments), std::move(statements),
      std::move(labels), std::move(constants));
  CHECK_OK(map.status());
  return *std::move(map);
}

ProgramMap::Location ProgramAssembler::GetMapLocation(
    const gb::Token& token) const {
  gb::LexerLocation location = GetLexer().GetTokenLocation(token);
  while (const Expansion* expansion = FindExpansion(location.id)) {
    location = GetLexer().GetTokenLocation(expansion->invocation);
  }

  // Every token outside an expansion is in a content AddContent() added.
  auto it = file_indices_.find(location.id);
  CHECK(it != file_indices_.end());
  return {.file = it->second, .line = location.line};
}

uint16_t ProgramAssembler::GetRoleSegment(int bank) const {
  const int segment = roles_[bank];
  return segment < 0 ? 0xFFFF : static_cast<uint16_t>(segment);
}

uint16_t ProgramAssembler::GetInstanceOffset(int bank, int instance) const {
  // Every instance's offset in a shared segment is 0: code and data from its
  // start, and the stack down from its end.
  const int index = roles_[bank];
  if (index < 0 || !segments_[index].instance_token.has_value()) {
    return 0;
  }

  // A per instance segment has each instance's words in turn, so an
  // instance's offset is the start of its words, and the stack's offset from
  // the segment's end is to the end of its words.
  const Segment& segment = segments_[index];
  if (bank == CpuCore::STACK) {
    return static_cast<uint16_t>(segment.page_count * kMemoryBankPageSize -
                                 (instance + 1) * segment.reserved_size);
  }
  return static_cast<uint16_t>(instance * segment.reserved_size);
}

//==============================================================================
// AssembleProgramFile and AssembleProgram
//==============================================================================

absl::StatusOr<AsmConstant> ParseAsmConstant(std::string_view text) {
  const std::string_view::size_type equals = text.find('=');
  if (equals == std::string_view::npos) {
    return AsmConstant{.name = std::string(text), .value = 1};
  }

  // The value is read as one token of source, which must be a number.
  AsmConstant constant = {.name = std::string(text.substr(0, equals))};
  const std::string_view value = text.substr(equals + 1);
  const std::unique_ptr<gb::Lexer> lexer =
      gb::Lexer::Create(GetProgramLexerProgram());
  const gb::Token token = lexer->ParseTokenText(value);
  if (!token.IsInt()) {
    return absl::InvalidArgumentError(absl::StrCat(
        "The value of ", constant.name, " isn't a number: \"", value, "\""));
  }
  if (token.GetInt() < 0 || token.GetInt() > 0xFFFFFFFF) {
    return absl::InvalidArgumentError(
        absl::StrCat("The value of ", constant.name, " doesn't fit a dword"));
  }
  constant.value = static_cast<uint32_t>(token.GetInt());
  return constant;
}

std::unique_ptr<AsmProgram> AssembleProgramFile(
    const InstructionSetSyntax& syntax, gb::FileSystem& file_system,
    std::string_view path, std::vector<gb::ParseError>* errors) {
  return AssembleProgramFile(syntax, file_system, path, {}, errors);
}

std::unique_ptr<AsmProgram> AssembleProgramFile(
    const InstructionSetSyntax& syntax, gb::FileSystem& file_system,
    std::string_view path, absl::Span<const AsmConstant> constants,
    std::vector<gb::ParseError>* errors) {
  return ProgramAssembler(&syntax, &file_system)
      .AssembleFile(path, constants, errors);
}

std::unique_ptr<AsmProgram> AssembleProgram(
    const InstructionSetSyntax& syntax, std::string text,
    std::vector<gb::ParseError>* errors) {
  return AssembleProgram(syntax, std::move(text), {}, errors);
}

std::unique_ptr<AsmProgram> AssembleProgram(
    const InstructionSetSyntax& syntax, std::string text,
    absl::Span<const AsmConstant> constants,
    std::vector<gb::ParseError>* errors) {
  return ProgramAssembler(&syntax, nullptr)
      .AssembleText(std::move(text), constants, errors);
}

}  // namespace oz3
