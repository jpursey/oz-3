// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_map.h"

#include <limits>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::Field;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::Pointee;

using Constant = ProgramMap::Constant;
using Label = ProgramMap::Label;
using Statement = ProgramMap::Statement;

// The parts of a map, which tests change before creating it.
struct MapParts {
  std::vector<std::string> files = {"main.oza", "include.oza"};
  std::vector<std::string> segments = {"code", "data"};
  std::vector<Statement> statements;
  std::vector<Label> labels;
  std::vector<Constant> constants;
};

absl::StatusOr<ProgramMap> CreateMap(const MapParts& parts) {
  return ProgramMap::Create(parts.files, parts.segments, parts.statements,
                            parts.labels, parts.constants);
}

void ExpectInvalid(const MapParts& parts, std::string_view message) {
  absl::StatusOr<ProgramMap> map = CreateMap(parts);
  ASSERT_FALSE(map.ok());
  EXPECT_THAT(map.status().message(), HasSubstr(message));
}

// Matches a pointer to a label named `name`.
auto IsLabel(std::string_view name) {
  return Pointee(Field(&Label::name, name));
}

// The statements of MakeMap()'s map.
constexpr Statement kStart = {
    .segment = 0, .offset = 0, .size = 2, .location = {.file = 0, .line = 1}};
constexpr Statement kNext = {
    .segment = 0, .offset = 2, .size = 1, .location = {.file = 0, .line = 2}};
constexpr Statement kEnd = {
    .segment = 0, .offset = 6, .size = 3, .location = {.file = 0, .line = 4}};
constexpr Statement kTable = {
    .segment = 1, .offset = 4, .size = 2, .location = {.file = 1, .line = 3}};

// Returns a map with two segments:
// - code: statements at 0 (2 words), 2 (1 word), and 6 (3 words), with labels
//   START and START@LOOP at 0, START@NEXT at 2, and END at 9.
// - data: a statement at 4 (2 words), with the label TABLE at 4.
ProgramMap MakeMap() {
  MapParts parts;
  parts.statements = {kTable, kEnd, kStart, kNext};
  parts.labels = {
      {.name = "TABLE", .segment = 1, .offset = 4},
      {.name = "END", .segment = 0, .offset = 9},
      {.name = "START@NEXT", .segment = 0, .offset = 2},
      {.name = "START@LOOP", .segment = 0, .offset = 0},
      {.name = "START", .segment = 0, .offset = 0},
  };
  absl::StatusOr<ProgramMap> map = CreateMap(parts);
  CHECK(map.ok()) << map.status();
  return *std::move(map);
}

TEST(ProgramMapTest, EmptyMap) {
  const ProgramMap map;
  EXPECT_THAT(map.GetFiles(), IsEmpty());
  EXPECT_THAT(map.GetSegments(), IsEmpty());
  EXPECT_THAT(map.GetStatements(), IsEmpty());
  EXPECT_THAT(map.GetLabels(), IsEmpty());
  EXPECT_THAT(map.GetConstants(), IsEmpty());
  EXPECT_EQ(map.FindStatement(0, 0), nullptr);
  EXPECT_EQ(map.FindLabel(0, 0), nullptr);

  absl::StatusOr<ProgramMap> created = ProgramMap::Create({}, {}, {}, {}, {});
  ASSERT_TRUE(created.ok()) << created.status();
  EXPECT_THAT(created->GetFiles(), IsEmpty());
}

TEST(ProgramMapTest, CreateKeepsFilesAndSegmentsInOrder) {
  absl::StatusOr<ProgramMap> map =
      ProgramMap::Create({"b.oza", "a.oza"}, {"z", "y"}, {}, {}, {});
  ASSERT_TRUE(map.ok()) << map.status();
  EXPECT_THAT(map->GetFiles(), ElementsAre("b.oza", "a.oza"));
  EXPECT_THAT(map->GetSegments(), ElementsAre("z", "y"));
}

TEST(ProgramMapTest, CreateSorts) {
  MapParts parts;
  parts.statements = {
      {.segment = 1, .offset = 0, .size = 1},
      {.segment = 0, .offset = 5, .size = 1},
      {.segment = 0, .offset = 1, .size = 1},
  };
  parts.labels = {
      {.name = "C", .segment = 1, .offset = 0},
      {.name = "B", .segment = 0, .offset = 5},
      {.name = "Z", .segment = 0, .offset = 1},
      {.name = "A", .segment = 0, .offset = 1},
  };
  parts.constants = {
      {.name = "Y", .word_value = 1, .dword_value = 1},
      {.name = "X", .dword_value = 0x10000},
  };
  absl::StatusOr<ProgramMap> map = CreateMap(parts);
  ASSERT_TRUE(map.ok()) << map.status();
  EXPECT_THAT(map->GetStatements(),
              ElementsAre(Statement{.segment = 0, .offset = 1, .size = 1},
                          Statement{.segment = 0, .offset = 5, .size = 1},
                          Statement{.segment = 1, .offset = 0, .size = 1}));
  EXPECT_THAT(map->GetLabels(),
              ElementsAre(Label{.name = "A", .segment = 0, .offset = 1},
                          Label{.name = "Z", .segment = 0, .offset = 1},
                          Label{.name = "B", .segment = 0, .offset = 5},
                          Label{.name = "C", .segment = 1, .offset = 0}));
  EXPECT_THAT(
      map->GetConstants(),
      ElementsAre(Constant{.name = "X", .dword_value = 0x10000},
                  Constant{.name = "Y", .word_value = 1, .dword_value = 1}));
}

TEST(ProgramMapTest, DefaultProgramSegmentHasNoName) {
  MapParts parts;
  parts.segments = {""};
  parts.statements = {{.segment = 0, .offset = 0, .size = 1}};
  parts.labels = {{.name = "START", .segment = 0, .offset = 0}};
  EXPECT_TRUE(CreateMap(parts).ok());
}

TEST(ProgramMapTest, SegmentWithNoNameIsNotTheOnlyOne) {
  MapParts parts;
  parts.segments = {"code", ""};
  ExpectInvalid(parts, "Segment 1 has no name");
}

TEST(ProgramMapTest, SegmentNamesAreUnique) {
  MapParts parts;
  parts.segments = {"code", "data", "code"};
  ExpectInvalid(parts, "Segment 2 is named code");
}

TEST(ProgramMapTest, StatementSegmentOutOfRange) {
  MapParts parts;
  parts.statements = {{.segment = 0, .offset = 0, .size = 1},
                      {.segment = 2, .offset = 0, .size = 1}};
  ExpectInvalid(parts, "Statement 1 is in segment 2, but the map has 2");
  parts.statements[1].segment = -1;
  ExpectInvalid(parts, "Statement 1 is in segment -1");
}

TEST(ProgramMapTest, StatementSize) {
  MapParts parts;
  parts.statements = {{.segment = 0, .offset = 0, .size = 0}};
  ExpectInvalid(parts, "Statement 0 has 0 words");
  parts.statements[0].size = -1;
  ExpectInvalid(parts, "Statement 0 has -1 words");
}

TEST(ProgramMapTest, StatementInSegment) {
  MapParts parts;
  parts.statements = {
      {.segment = 0, .offset = kMemoryBankMaxSize - 2, .size = 2}};
  EXPECT_TRUE(CreateMap(parts).ok());
  parts.statements[0].size = 3;
  ExpectInvalid(parts, "Statement 0 has 3 words at offset 65534");
  parts.statements[0] = {.segment = 0,
                         .offset = std::numeric_limits<int>::max(),
                         .size = std::numeric_limits<int>::max()};
  ExpectInvalid(parts, "Statement 0 has 2147483647 words");
  parts.statements[0] = {.segment = 0, .offset = -1, .size = 1};
  ExpectInvalid(parts, "Statement 0 has 1 words at offset -1");
}

TEST(ProgramMapTest, StatementsOverlap) {
  MapParts parts;
  parts.statements = {{.segment = 0, .offset = 3, .size = 1},
                      {.segment = 0, .offset = 0, .size = 3},
                      {.segment = 1, .offset = 1, .size = 1}};
  EXPECT_TRUE(CreateMap(parts).ok());
  parts.statements[1].size = 4;
  ExpectInvalid(parts, "statements at offsets 0 and 3 in segment 0 overlap");
  parts.statements[1] = {.segment = 0, .offset = 3, .size = 1};
  ExpectInvalid(parts, "statements at offsets 3 and 3 in segment 0 overlap");
}

TEST(ProgramMapTest, StatementLocation) {
  MapParts parts;
  parts.statements = {
      {.segment = 0, .offset = 0, .size = 1, .location = {.file = 2}}};
  ExpectInvalid(parts, "Statement 0 is in file 2, but the map has 2");
  parts.statements[0].location = {.file = -1};
  ExpectInvalid(parts, "Statement 0 is in file -1");
  parts.statements[0].location = {.file = 1, .line = -1};
  ExpectInvalid(parts, "Statement 0 is on line -1");
}

TEST(ProgramMapTest, LabelSegmentOutOfRange) {
  MapParts parts;
  parts.labels = {{.name = "X", .segment = 2}};
  ExpectInvalid(parts, "Label X is in segment 2, but the map has 2");
  parts.labels[0].segment = -1;
  ExpectInvalid(parts, "Label X is in segment -1");
}

TEST(ProgramMapTest, LabelInSegment) {
  MapParts parts;
  parts.labels = {{.name = "X", .segment = 0, .offset = kMemoryBankMaxSize}};
  EXPECT_TRUE(CreateMap(parts).ok());
  parts.labels[0].offset = kMemoryBankMaxSize + 1;
  ExpectInvalid(parts, "Label X is at offset 65537");
  parts.labels[0].offset = -1;
  ExpectInvalid(parts, "Label X is at offset -1");
}

TEST(ProgramMapTest, LabelLocation) {
  MapParts parts;
  parts.labels = {{.name = "X", .location = {.file = 2}}};
  ExpectInvalid(parts, "Label X is in file 2");
  parts.labels[0].location = {.file = 0, .line = -1};
  ExpectInvalid(parts, "Label X is on line -1");
}

TEST(ProgramMapTest, ConstantLocation) {
  MapParts parts;
  parts.constants = {{.name = "N", .location = {.file = 2}}};
  ExpectInvalid(parts, "Constant N is in file 2");
  parts.constants[0].location = {.file = 0, .line = -1};
  ExpectInvalid(parts, "Constant N is on line -1");
}

TEST(ProgramMapTest, LabelNamesAreNotEmpty) {
  MapParts parts;
  parts.labels = {{.name = "X"}, {.name = ""}};
  ExpectInvalid(parts, "Label 1 has no name");
}

TEST(ProgramMapTest, ConstantNamesAreNotEmpty) {
  MapParts parts;
  parts.constants = {{.name = ""}};
  ExpectInvalid(parts, "Constant 0 has no name");
}

TEST(ProgramMapTest, LabelNamesAreUnique) {
  MapParts parts;
  parts.labels = {{.name = "X"}, {.name = "X", .offset = 1}};
  ExpectInvalid(parts, "Label X has the name of another label or constant");
}

TEST(ProgramMapTest, ConstantNamesAreUnique) {
  MapParts parts;
  parts.constants = {{.name = "X"}, {.name = "X"}};
  ExpectInvalid(parts, "Constant X has the name of another label or constant");
}

TEST(ProgramMapTest, LabelAndConstantNamesAreUnique) {
  MapParts parts;
  parts.labels = {{.name = "X"}};
  parts.constants = {{.name = "X"}};
  ExpectInvalid(parts, "Constant X has the name of another label or constant");
}

TEST(ProgramMapTest, FindStatement) {
  const ProgramMap map = MakeMap();
  EXPECT_THAT(map.FindStatement(0, 0), Pointee(kStart));
  EXPECT_THAT(map.FindStatement(0, 1), Pointee(kStart));
  EXPECT_THAT(map.FindStatement(0, 2), Pointee(kNext));

  // Words 3 to 5 are reserved by no statement.
  EXPECT_EQ(map.FindStatement(0, 3), nullptr);
  EXPECT_EQ(map.FindStatement(0, 5), nullptr);

  EXPECT_THAT(map.FindStatement(0, 8), Pointee(kEnd));
  EXPECT_EQ(map.FindStatement(0, 9), nullptr);
  EXPECT_EQ(map.FindStatement(0, -1), nullptr);
}

TEST(ProgramMapTest, FindStatementInSegment) {
  const ProgramMap map = MakeMap();

  // The data segment's statement is at 4, and the code segment's statements
  // before 4 aren't in it.
  EXPECT_EQ(map.FindStatement(1, 0), nullptr);
  EXPECT_EQ(map.FindStatement(1, 3), nullptr);
  EXPECT_THAT(map.FindStatement(1, 5), Pointee(kTable));
  EXPECT_EQ(map.FindStatement(1, 6), nullptr);
  EXPECT_EQ(map.FindStatement(2, 0), nullptr);
}

TEST(ProgramMapTest, FindLabel) {
  const ProgramMap map = MakeMap();

  // START comes before the local label START@LOOP at the same offset.
  EXPECT_THAT(map.FindLabel(0, 0), IsLabel("START"));
  EXPECT_THAT(map.FindLabel(0, 1), IsLabel("START"));

  // The local label START@NEXT is nearer than START.
  EXPECT_THAT(map.FindLabel(0, 2), IsLabel("START@NEXT"));
  EXPECT_THAT(map.FindLabel(0, 8), IsLabel("START@NEXT"));

  // END is at the end of the code, and the nearest label to everything after.
  EXPECT_THAT(map.FindLabel(0, 9), IsLabel("END"));
  EXPECT_THAT(map.FindLabel(0, kMemoryBankMaxSize - 1), IsLabel("END"));
  EXPECT_EQ(map.FindLabel(0, -1), nullptr);
}

TEST(ProgramMapTest, FindLabelInSegment) {
  const ProgramMap map = MakeMap();

  // The code segment's labels aren't in the data segment.
  EXPECT_EQ(map.FindLabel(1, 0), nullptr);
  EXPECT_EQ(map.FindLabel(1, 3), nullptr);
  EXPECT_THAT(map.FindLabel(1, 4), IsLabel("TABLE"));

  // There is no segment 2, so the data segment's label isn't found.
  EXPECT_EQ(map.FindLabel(2, 0), nullptr);
}

}  // namespace
}  // namespace oz3
