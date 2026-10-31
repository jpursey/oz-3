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
#include "absl/strings/ascii.h"
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
#include "oz3/tools/expression.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program.h"
#include "oz3/tools/program_grammar.h"
#include "oz3/tools/program_lexer.h"
#include "oz3/tools/program_map.h"

namespace oz3 {

namespace {

using ArgKind = InstructionForm::Arg::Kind;

// The filename of a program assembled from text.
constexpr std::string_view kTextFilename = "program";

// The names of the four bank roles in .SEGMENT, by bank index (CpuCore::CODE
// to CpuCore::EXTRA).
constexpr std::array<std::string_view, 4> kRoleNames = {"CODE", "STACK", "DATA",
                                                        "EXTRA"};

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

}  // namespace

// Assembles one program, in two passes:
// 1. Reads each line of the source, following includes, defining labels and
//    constants, and matching each instruction to its form. Each statement
//    that writes words is recorded, with its segment and offset, so that
//    every label has its value by the end, as is each constant.
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
  // whose text is `text`, which can't include files. Returns null if there
  // are errors, and sets `errors` to them if it isn't null.
  std::unique_ptr<AsmProgram> AssembleFile(std::string_view path,
                                           std::vector<gb::ParseError>* errors);
  std::unique_ptr<AsmProgram> AssembleText(std::string text,
                                           std::vector<gb::ParseError>* errors);

 private:
  // A label or local label, whose value is its offset in its segment.
  struct Label {
    gb::Token token;  // Its name where it is defined.
    int segment = 0;
    int64_t offset = 0;
  };

  // A constant, whose value is its expression, evaluated with the offset
  // ("$") and the scope of local labels where it is defined.
  struct Constant {
    gb::Token token;  // Its name where it is defined.
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

  // Data of `width` (.DW or .DD): each item is a string's words, or an
  // expression.
  using DataItem = std::variant<std::vector<uint16_t>, Expression>;
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

  // The context of a statement's expressions: its offset, and the scope of
  // its local labels.
  class StatementContext final : public ExpressionContext {
   public:
    // `assembler` must outlive this.
    StatementContext(ProgramAssembler* assembler, int64_t offset,
                     std::string_view scope)
        : assembler_(assembler), offset_(offset), scope_(scope) {}

    uint16_t GetOffset() const override {
      return static_cast<uint16_t>(offset_);
    }
    std::optional<ExpressionResult> GetSymbolValue(
        std::string_view name, ExpressionWidth width) const override {
      return assembler_->GetSymbolValue(scope_, name, width);
    }

   private:
    ProgramAssembler* const assembler_;
    const int64_t offset_;
    const std::string_view scope_;
  };

  gb::Lexer& GetLexer() { return parser_->GetLexer(); }
  const gb::Lexer& GetLexer() const { return parser_->GetLexer(); }

  // Returns the current segment, which statements assemble into.
  Segment& GetSegment() { return segments_[segment_]; }

  // Adds an error, unless it is already reported (as an error in a constant
  // comes back at each use), and stops once there are kMaxProgramErrors.
  void AddError(gb::ParseError error);
  void Error(const gb::Token& token, std::string_view message);

  // Returns the start of the main file, for errors in the program as a whole.
  gb::LexerLocation GetMainLocation() const;

  // Creates the parser, and returns false if it can't.
  bool Init();

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
  // an "@" in it, so a local label's own "@" separates the two, and no label's
  // key is another's.
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

  // Returns the value of `expression` as `width` in `context`, or reports its
  // error and returns nullopt.
  std::optional<uint32_t> Evaluate(const Expression& expression,
                                   ExpressionWidth width,
                                   const ExpressionContext& context);

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
  void ReadLine(gb::LexerContentId content, const gb::ParsedItem& line);
  void ReadLabel(const gb::ParsedItem& label);
  void ReadDirective(const gb::ParsedItem& directive);
  void ReadInclude(const gb::ParsedItem& directive);
  void ReadConst(const gb::ParsedItem& directive);
  void ReadData(const gb::ParsedItem& directive, ExpressionWidth width);
  void ReadStorage(const gb::ParsedItem& directive);
  void ReadStack(const gb::ParsedItem& directive);
  void ReadInstances(const gb::ParsedItem& directive);
  void ReadBank(const gb::ParsedItem& directive);
  void ReadSegment(const gb::ParsedItem& directive);
  void ReadSegmentRole(const gb::Token& role);
  void ReadInstruction(gb::LexerContentId content, const gb::ParsedItem& line);

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
  std::unique_ptr<AsmProgram> BuildProgram();
  Program::Segment BuildSegment(int index) const;
  ProgramMap BuildMap();

  // Returns where `token` is, as the map has it.
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

  // Errors, and each one's formatted message, to report each error once.
  std::vector<gb::ParseError> errors_;
  absl::flat_hash_set<std::string> error_messages_;
  bool stopped_ = false;

  // Labels by key (see GetLabelKey), and constants by name. A label and a
  // constant can't have the same name.
  absl::flat_hash_map<std::string, Label> labels_;
  absl::flat_hash_map<std::string, Constant> constants_;

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
  std::array<int, 4> roles_ = {0, 0, 0, 0};

  // The statements the second pass finishes, and the stack's size (.STACK)
  // and where it is given.
  std::vector<Statement> statements_;
  uint16_t stack_words_ = 0;
  std::optional<gb::Token> stack_token_;

  // The number of instances (.INSTANCES), and where it is given.
  int instance_count_ = 1;
  std::optional<gb::Token> instances_token_;
};

std::unique_ptr<AsmProgram> ProgramAssembler::AssembleFile(
    std::string_view path, std::vector<gb::ParseError>* errors) {
  if (!Init()) {
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
    std::string text, std::vector<gb::ParseError>* errors) {
  if (!Init()) {
    return Finish(nullptr, errors);
  }
  main_content_ = AddContent(kTextFilename, std::move(text), {});
  if (main_content_ == gb::kNoLexerContent) {
    return Finish(nullptr, errors);
  }
  return Finish(Assemble(main_content_), errors);
}

bool ProgramAssembler::Init() {
  absl::StatusOr<std::unique_ptr<ProgramGrammar>> grammar =
      ProgramGrammar::Create(*syntax_);
  if (!grammar.ok()) {
    AddError(gb::ParseError(grammar.status().message()));
    return false;
  }
  grammar_ = *std::move(grammar);
  parser_ = gb::Parser::Create(grammar_->GetParserProgram());
  return true;
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
  // Errors name the file without its protocol, but the map has the path it
  // was read from.
  const std::string_view filename = gb::RemoveProtocol(path);
  const gb::LexerContentId content =
      GetLexer().AddFileContent(filename, std::move(text));
  if (content == gb::kNoLexerContent) {
    AddError(gb::ParseError(
        location,
        absl::StrCat(filename, " has too many lines, or a line too long")));
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
  if (stopped_ || !error_messages_.insert(error.FormatMessage()).second) {
    return;
  }
  errors_.push_back(std::move(error));
  if (static_cast<int>(errors_.size()) == kMaxProgramErrors) {
    errors_.emplace_back(
        errors_.back().GetLocation(),
        absl::StrCat("Stopped after ", kMaxProgramErrors, " errors"));
    stopped_ = true;
  }
}

void ProgramAssembler::Error(const gb::Token& token, std::string_view message) {
  AddError(gb::ParseError(GetLexer().GetTokenLocation(token), message));
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
    return ExpressionResult{.value =
                                static_cast<uint32_t>(label->second.offset)};
  }
  auto it = constants_.find(name);
  if (it == constants_.end()) {
    return std::nullopt;
  }
  Constant& constant = it->second;

  // A constant with a value that can't be evaluated as a word is an error
  // where it is used as one, without a location (see ExpressionContext).
  ExpressionResult result = EvaluateConstant(constant, width);
  if (width == ExpressionWidth::kWord &&
      result.kind == ExpressionResult::Kind::kError &&
      EvaluateConstant(constant, ExpressionWidth::kDword).kind ==
          ExpressionResult::Kind::kValue) {
    result.error = gb::ParseError(absl::StrCat(
        constant.token.GetString(),
        " can't be evaluated as a word: ", result.error.GetMessage()));
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
      width, StatementContext(this, constant.offset, constant.scope));
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
                     .offset = GetSegment().reserved_size,
                     .scope = scope_,
                     .expression = Expression::Create(GetLexer(), value)});
  return true;
}

bool ProgramAssembler::CheckNotDefined(const gb::Token& token,
                                       std::string_view key,
                                       std::string_view name) {
  if (labels_.contains(key) || constants_.contains(key)) {
    Error(token, absl::StrCat(name, " is already defined"));
    return false;
  }
  return true;
}

std::optional<uint32_t> ProgramAssembler::Evaluate(
    const Expression& expression, ExpressionWidth width,
    const ExpressionContext& context) {
  ExpressionResult result = expression.Evaluate(width, context);
  if (result.kind != ExpressionResult::Kind::kValue) {
    AddError(std::move(result.error));
    return std::nullopt;
  }
  return result.value;
}

std::optional<uint32_t> ProgramAssembler::EvaluateNow(
    const gb::ParsedItem& item) {
  return Evaluate(Expression::Create(GetLexer(), item), ExpressionWidth::kWord,
                  StatementContext(this, GetSegment().reserved_size, scope_));
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
  gb::Lexer& lexer = GetLexer();
  while (!stopped_) {
    gb::ParseResult line = parser_->Parse(content, ProgramGrammar::kLineRule);
    if (!line.IsOk()) {
      // The lexer is left at the start of the line, so reading goes on at the
      // next.
      AddError(line.GetError());
      const int line_index = lexer.GetCurrentLine(content);
      lexer.NextLine(content);
      if (lexer.GetCurrentLine(content) == line_index) {
        return;
      }
      continue;
    }
    ReadLine(content, *line);
    if (line->GetToken("end").IsEnd()) {
      return;
    }
  }
}

void ProgramAssembler::ReadLine(gb::LexerContentId content,
                                const gb::ParsedItem& line) {
  if (const gb::ParsedItem* label = line.GetItem("label"); label != nullptr) {
    ReadLabel(*label);
  }
  if (const gb::ParsedItem* directive = line.GetItem("directive");
      directive != nullptr) {
    ReadDirective(*directive);
  } else if (line.GetItem("instruction") != nullptr) {
    ReadInstruction(content, line);
  }
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
  DefineLabel(token, absl::AsciiStrToUpper(token.GetString()));
}

void ProgramAssembler::ReadDirective(const gb::ParsedItem& directive) {
  gb::ParseError error;
  const std::string_view name =
      ProgramGrammar::GetDirectiveName(GetLexer(), directive, error);
  if (name.empty()) {
    AddError(std::move(error));
  } else if (name == "INCLUDE") {
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

void ProgramAssembler::ReadConst(const gb::ParsedItem& directive) {
  const gb::Token name = directive.GetToken("name");
  if (DefineConstant(name, *directive.GetItem("value"))) {
    AddStatement(directive.GetToken(), ConstantStatement{.name = name}, 0);
  }
}

void ProgramAssembler::ReadData(const gb::ParsedItem& directive,
                                ExpressionWidth width) {
  DataStatement data = {.width = width};
  const int value_size = (width == ExpressionWidth::kWord ? 1 : 2);
  int64_t size = 0;
  for (const gb::ParsedItem& item : directive.GetItems("items")) {
    Expression expression = Expression::Create(GetLexer(), item);
    if (width == ExpressionWidth::kWord && expression.IsString()) {
      absl::StatusOr<std::vector<uint16_t>> words =
          GetStringWords(GetLexer(), expression.GetToken());
      if (!words.ok()) {
        Error(expression.GetToken(), words.status().message());
        continue;
      }
      size += static_cast<int64_t>(words->size());
      data.items.emplace_back(*std::move(words));
    } else {
      size += value_size;
      data.items.emplace_back(std::move(expression));
    }
  }
  Write(size, directive.GetToken(), std::move(data));
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
  const int index =
      static_cast<int>(absl::c_find(kRoleNames, name) - kRoleNames.begin());
  DCHECK_LT(index, static_cast<int>(kRoleNames.size()));
  const int other = roles_[index];
  if (other >= 0) {
    Error(role, absl::StrCat("The ", name, " role is already the segment ",
                             segments_[other].name->GetString(), "'s"));
  } else {
    roles_[index] = segment_;
  }
}

void ProgramAssembler::ReadInstruction(gb::LexerContentId content,
                                       const gb::ParsedItem& line) {
  std::vector<InstructionMatch> matches;
  gb::ParseError error;
  if (!grammar_->MatchInstruction(
          *parser_, content, line,
          StatementContext(this, GetSegment().reserved_size, scope_), matches,
          error)) {
    AddError(std::move(error));
    return;
  }
  const gb::ParsedItem* instruction = line.GetItem("instruction");
  if (matches.size() != 1) {
    const std::string_view name =
        ProgramGrammar::GetInstructionName(GetLexer(), *instruction, error);
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
    Error(instruction->GetToken(), message);
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
      match.form->GetSize(), instruction->GetToken(),
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
  const StatementContext context(this, statement.offset, statement.scope);
  std::vector<int64_t> values = GetCodeWordValues(form, instruction.args);
  bool ok = true;
  for (int i = 0; i < static_cast<int>(instruction.args.size()); ++i) {
    const ArgKind kind = form.GetArgs()[i].kind;
    if (kind != ArgKind::kWord && kind != ArgKind::kDword) {
      continue;
    }
    const std::optional<uint32_t> value =
        Evaluate(*instruction.args[i].value,
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

void ProgramAssembler::EncodeData(const Statement& statement,
                                  const DataStatement& data,
                                  std::vector<uint16_t>& words) {
  const StatementContext context(this, statement.offset, statement.scope);
  for (const DataItem& item : data.items) {
    if (const auto* string_words = std::get_if<std::vector<uint16_t>>(&item);
        string_words != nullptr) {
      words.insert(words.end(), string_words->begin(), string_words->end());
    } else if (const auto* expression = std::get_if<Expression>(&item);
               expression != nullptr) {
      const std::optional<uint32_t> value =
          Evaluate(*expression, data.width, context);
      if (!value.has_value()) {
        continue;
      }

      // A dword is written low word first.
      words.push_back(static_cast<uint16_t>(*value));
      if (data.width == ExpressionWidth::kDword) {
        words.push_back(static_cast<uint16_t>(*value >> 16));
      }
    }
  }
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
  result->map_ = BuildMap();
  return result;
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
  // Every token is in a content AddContent() added.
  const gb::LexerLocation location = GetLexer().GetTokenLocation(token);
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

std::unique_ptr<AsmProgram> AssembleProgramFile(
    const InstructionSetSyntax& syntax, gb::FileSystem& file_system,
    std::string_view path, std::vector<gb::ParseError>* errors) {
  return ProgramAssembler(&syntax, &file_system).AssembleFile(path, errors);
}

std::unique_ptr<AsmProgram> AssembleProgram(
    const InstructionSetSyntax& syntax, std::string text,
    std::vector<gb::ParseError>* errors) {
  return ProgramAssembler(&syntax, nullptr)
      .AssembleText(std::move(text), errors);
}

}  // namespace oz3
