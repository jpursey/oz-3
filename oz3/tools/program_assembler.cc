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
#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "gb/file/file_system.h"
#include "gb/file/path.h"
#include "gb/parse/lexer.h"
#include "gb/parse/parse_error.h"
#include "gb/parse/parse_result.h"
#include "gb/parse/parser.h"
#include "gb/parse/token.h"
#include "oz3/core/core_types.h"
#include "oz3/tools/expression.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program.h"
#include "oz3/tools/program_grammar.h"
#include "oz3/tools/program_lexer.h"

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

}  // namespace

// Assembles one program, in two passes:
// 1. Reads each line of the source, following includes, defining labels and
//    constants, and matching each instruction to its form. Each statement
//    that writes words is recorded, with its offset, so that every label has
//    its value by the end, as is each constant.
// 2. Finishes each recorded statement, now that every symbol is defined:
//    encodes each one that writes words, and checks each constant's value.
//    Then builds the program from the words.
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
  // A label or local label, whose value is its offset.
  struct Label {
    gb::Token token;  // Its name where it is defined.
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
    uint16_t offset = 0;
    std::string_view scope;
    StatementContents contents;
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
    std::optional<ExpressionResult> GetSymbol(
        std::string_view name, ExpressionWidth width) const override {
      return assembler_->GetSymbol(scope_, name, width);
    }

   private:
    ProgramAssembler* const assembler_;
    const int64_t offset_;
    const std::string_view scope_;
  };

  gb::Lexer& GetLexer() { return parser_->GetLexer(); }
  const gb::Lexer& GetLexer() const { return parser_->GetLexer(); }

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
  // and returns kNoLexerContent.
  gb::LexerContentId LoadFile(const std::string& path,
                              const gb::LexerLocation& location);
  gb::LexerContentId AddContent(std::string_view filename, std::string text,
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
  std::optional<ExpressionResult> GetSymbol(std::string_view scope,
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
  void ReadInstruction(gb::LexerContentId content, const gb::ParsedItem& line);
  void AddStatement(const gb::Token& token, StatementContents contents);

  // Moves the offset past `size` words of the statement at `token`.
  void Reserve(int64_t size, const gb::Token& token);

  // Reports an error at `token`, about `what`, if `size` words from the offset
  // pass the end of the segment, where they didn't before.
  void CheckSegmentEnd(int64_t size, const gb::Token& token,
                       std::string_view what);
  void EndFirstPass();

  // Second pass.
  void FinishStatement(const Statement& statement);
  void CheckConstant(const ConstantStatement& constant);
  void EncodeInstruction(const Statement& statement,
                         const InstructionStatement& instruction,
                         std::vector<uint16_t>& words);
  void EncodeData(const Statement& statement, const DataStatement& data,
                  std::vector<uint16_t>& words);
  std::unique_ptr<AsmProgram> BuildProgram();

  const InstructionSetSyntax* const syntax_;
  gb::FileSystem* const file_system_;
  std::unique_ptr<ProgramGrammar> grammar_;
  std::unique_ptr<gb::Parser> parser_;
  gb::LexerContentId main_content_ = gb::kNoLexerContent;

  // Each file's content by path, and the paths of the files being read, from
  // the main file to the current one (none for text).
  absl::flat_hash_map<std::string, gb::LexerContentId> files_;
  std::vector<std::string> including_;

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

  // The offset of the next statement, the statements the second pass
  // finishes, and the stack's size (.STACK) and where it is given.
  int64_t offset_ = 0;
  std::vector<Statement> statements_;
  uint16_t stack_words_ = 0;
  std::optional<gb::Token> stack_token_;

  // The words, and which are written, which the second pass fills in.
  std::vector<uint16_t> words_;
  std::vector<bool> written_;
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
    const std::string& path, const gb::LexerLocation& location) {
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
      AddContent(gb::RemoveProtocol(path), std::move(text), location);
  if (content != gb::kNoLexerContent) {
    files_[path] = content;
  }
  return content;
}

gb::LexerContentId ProgramAssembler::AddContent(
    std::string_view filename, std::string text,
    const gb::LexerLocation& location) {
  const gb::LexerContentId content =
      GetLexer().AddFileContent(filename, std::move(text));
  if (content == gb::kNoLexerContent) {
    AddError(gb::ParseError(
        location,
        absl::StrCat(filename, " has too many lines, or a line too long")));
  }
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

  // Second pass. The words are whole aligned pairs (see BuildProgram).
  constexpr int kAlignment = Program::Segment::Data::kAlignment;
  const int size =
      static_cast<int>((offset_ + kAlignment - 1) / kAlignment * kAlignment);
  words_.resize(size);
  written_.resize(size);
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

std::optional<ExpressionResult> ProgramAssembler::GetSymbol(
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
  labels_[std::move(key)] = {.token = token, .offset = offset_};
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
                     .offset = offset_,
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
                  StatementContext(this, offset_, scope_));
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
  } else {
    Error(directive.GetToken(),
          absl::StrCat(".", name, " is not supported yet"));
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
    AddStatement(directive.GetToken(), ConstantStatement{.name = name});
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
  AddStatement(directive.GetToken(), std::move(data));
  Reserve(size, directive.GetToken());
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

void ProgramAssembler::ReadInstruction(gb::LexerContentId content,
                                       const gb::ParsedItem& line) {
  std::vector<InstructionMatch> matches;
  gb::ParseError error;
  if (!grammar_->MatchInstruction(*parser_, content, line,
                                  StatementContext(this, offset_, scope_),
                                  matches, error)) {
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

  // Encode() checks that a code word value fits its bits too, but this says
  // which value doesn't.
  InstructionMatch& match = matches.front();
  for (int i = 0; i < static_cast<int>(match.args.size()); ++i) {
    const InstructionForm::Arg& arg = match.form->GetArgs()[i];
    const uint32_t value = match.args[i].code_value;
    if (arg.kind == ArgKind::kCodeValue && value >= (uint32_t{1} << arg.bits)) {
      Error(match.args[i].value->GetToken(),
            absl::StrCat("Value ", value, " does not fit in ", arg.bits,
                         " bits"));
    }
  }
  const int size = match.form->GetSize();
  AddStatement(
      instruction->GetToken(),
      InstructionStatement{.form = match.form, .args = std::move(match.args)});
  Reserve(size, instruction->GetToken());
}

void ProgramAssembler::AddStatement(const gb::Token& token,
                                    StatementContents contents) {
  statements_.push_back({.token = token,
                         .offset = static_cast<uint16_t>(offset_),
                         .scope = scope_,
                         .contents = std::move(contents)});
}

void ProgramAssembler::Reserve(int64_t size, const gb::Token& token) {
  CheckSegmentEnd(size, token, "This");
  offset_ += size;
}

void ProgramAssembler::CheckSegmentEnd(int64_t size, const gb::Token& token,
                                       std::string_view what) {
  if (offset_ <= kMemoryBankMaxSize && offset_ + size > kMemoryBankMaxSize) {
    Error(token, absl::StrCat(what, " passes the end of the segment, at ",
                              kMemoryBankMaxSize, " words"));
  }
}

void ProgramAssembler::EndFirstPass() {
  // The program's one segment has the stack role, so it needs a size for it.
  if (!stack_token_.has_value()) {
    AddError(gb::ParseError(
        GetMainLocation(),
        "The program has a stack, so it needs its size, with .STACK"));
    return;
  }
  CheckSegmentEnd(stack_words_, *stack_token_, "The stack");
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
  for (int i = 0; i < static_cast<int>(words.size()); ++i) {
    words_[statement.offset + i] = words[i];
    written_[statement.offset + i] = true;
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
  std::vector<int64_t> values;
  values.reserve(instruction.args.size());
  bool ok = true;
  for (int i = 0; i < static_cast<int>(instruction.args.size()); ++i) {
    const InstructionArg& arg = instruction.args[i];
    switch (form.GetArgs()[i].kind) {
      case ArgKind::kWordReg:
      case ArgKind::kDwordReg:
        values.push_back(arg.reg);
        break;
      case ArgKind::kCodeValue:
        values.push_back(arg.code_value);
        break;
      case ArgKind::kWord:
      case ArgKind::kDword: {
        const std::optional<uint32_t> value = Evaluate(
            *arg.value,
            form.GetArgs()[i].kind == ArgKind::kWord ? ExpressionWidth::kWord
                                                     : ExpressionWidth::kDword,
            context);
        ok = ok && value.has_value();
        values.push_back(value.value_or(0));
      } break;
    }
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
  // The default program: one segment in one bank, placed anywhere, with every
  // role, and one instance, whose stack is at the end of the segment.
  const int64_t size = offset_ + stack_words_;
  Program::Segment segment = {
      .page_count = static_cast<uint16_t>(std::max<int64_t>(
          1, (size + kMemoryBankPageSize - 1) / kMemoryBankPageSize))};

  // The data is each run of aligned pairs of words with a word written, as
  // blocks. The words are whole pairs, so a word in a pair that isn't written
  // is 0.
  constexpr int kAlignment = Program::Segment::Data::kAlignment;
  const int word_count = static_cast<int>(words_.size());
  for (int offset = 0; offset < word_count; offset += kAlignment) {
    const auto start = written_.begin() + offset;
    if (std::find(start, start + kAlignment, true) == start + kAlignment) {
      continue;
    }
    if (segment.data.empty() ||
        segment.data.back().offset +
                static_cast<int>(segment.data.back().data.size()) !=
            offset) {
      segment.data.push_back({.offset = static_cast<uint16_t>(offset)});
    }
    std::vector<uint16_t>& data = segment.data.back().data;
    data.insert(data.end(), words_.begin() + offset,
                words_.begin() + offset + kAlignment);
  }

  auto result = absl::WrapUnique(new AsmProgram);
  Program& program = result->program_;
  program.banks.emplace_back();
  program.segments.push_back(std::move(segment));
  program.code_segment = 0;
  program.stack_segment = 0;
  program.data_segment = 0;
  program.extra_segment = 0;
  program.instances.emplace_back();

  // The assembler reports what a program breaks itself, but this is a last
  // check of the rest.
  if (absl::Status status = ValidateProgram(program); !status.ok()) {
    AddError(gb::ParseError(
        GetMainLocation(),
        absl::StrCat("The program isn't valid: ", status.message())));
    return nullptr;
  }
  return result;
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
