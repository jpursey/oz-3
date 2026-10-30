# Program Maps

The map of an assembled program: its source files, its segments, where each
statement's words came from, and every label and constant. The assembler builds
a `ProgramMap` along with the `Program` (`AsmProgram::GetMap()`), a host writes
it to a map file (`.oz3map`, for `oz3asm`), and the debugger reads it back.
With it, a segment offset, such as `IP` in the code segment or a label's value,
becomes the source line that wrote the word there, and the label it is at.

The map works in segment offsets, as every address in a program is one (see
Principles in [assembler_design.md](../assembler_design.md)). Turning an
address in a bank into a segment offset needs to know where each segment was
loaded, which `ProgramLoader` doesn't record yet. That is part of the backlog's
*Debugger (oz3dbg)*, the first to have such addresses.

The map is only read by host tools, so it is a Game Bits chunk file, and isn't
part of the specification: there were no wiki changes.

## Behavior

### API

All of it is in `tools`, beside the program assembler and program files.

```
// program_map.h
class ProgramMap final {
 public:
  // A line in a source file, numbered from 0 as gb::LexerLocation is.
  struct Location {
    int file = 0;  // An index into GetFiles().
    int line = 0;
  };

  // A statement that wrote words: an instruction, .DW, or .DD.
  struct Statement {
    int segment = 0;  // An index into GetSegments().
    int offset = 0;
    int size = 0;  // In words, at least 1.
    Location location;
  };

  // A label or local label, whose value is its offset in its segment.
  struct Label {
    std::string name;
    int segment = 0;
    int offset = 0;  // Up to the end of a segment, 64 Kiw.
    Location location;
  };

  // A constant, with its value in each width it has one.
  struct Constant {
    std::string name;
    std::optional<uint16_t> word_value;
    uint32_t dword_value = 0;
    Location location;
  };

  static absl::StatusOr<ProgramMap> Create(std::vector<std::string> files,
                                           std::vector<std::string> segments,
                                           std::vector<Statement> statements,
                                           std::vector<Label> labels,
                                           std::vector<Constant> constants);

  absl::Span<const std::string> GetFiles() const;
  absl::Span<const std::string> GetSegments() const;
  absl::Span<const Statement> GetStatements() const;  // By segment, offset.
  absl::Span<const Label> GetLabels() const;  // By segment, offset, name.
  absl::Span<const Constant> GetConstants() const;  // By name.

  const Statement* FindStatement(int segment, int offset) const;
  const Label* FindLabel(int segment, int offset) const;
};

// program_map_file.h
absl::Status WriteProgramMapFile(gb::FileSystem& file_system,
                                 std::string_view path, const ProgramMap& map);
absl::StatusOr<ProgramMap> ReadProgramMapFile(gb::FileSystem& file_system,
                                              std::string_view path);

// program_assembler.h
class AsmProgram final {
 public:
  const Program& GetProgram() const;
  const ProgramMap& GetMap() const;
};
```

- `FindStatement` returns the statement that wrote the word at an offset, or
  null where none did (such as words `.DS` reserves).
- `FindLabel` returns the label nearest at or before an offset, for writing an
  address as `label+n`. Of several at one offset, it returns the first by name,
  so a label comes before the local labels in its scope.
- A default constructed map is empty. Maps are copyable and movable.

### What the assembler records

- **Files:** every file the assembler read, by the path it read it from, with
  its protocol (`mem:/lib/a.oza`), in the order first read, once each however
  often it is included. Errors name a file without its protocol, as the lexer
  does, so a map's path and an error's filename differ by it. A program
  assembled from text has one file, `program`, as its errors do.
- **Segments:** each segment's name, in the program's order, so segment `i` in
  the map is `Program::segments[i]`. The default program's one segment has no
  name.
- **Statements:** each instruction, `.DW`, and `.DD` that wrote words, with
  its segment, offset, size in words, and line. A statement in a file included
  twice is in the map twice, at the same line. `.DS` and the stack only
  reserve words, so they aren't in the map, and nor is a `.DW` or `.DD` with
  no words.
- **Labels**, each at its name's line, with its segment and offset. Labels and
  local labels are one kind: a local label's name is its scope's label and
  then its own (`START@LOOP`), or just its own before the program's first
  label (`@LOOP`), which is already unique. A debugger can tell a local label
  by its `@`.
- **Constants**, each at its name's line, with its value in each width. A
  constant is its expression, evaluated where it is used, and its word value
  isn't always its dword value's low word, wherever `/`, `%`, `>>`, or `!`
  bring down bits from above it: `(0FFFFh + 1) / 2` is 0 as a word, where the
  sum wraps, and `8000h` as a dword. Its `$` and local labels are fixed where
  it is defined, so these two values are all it can ever be. Every constant
  has a dword value, but not always a word value (`10000h` is only usable as a
  dword), and one whose word value has an error just has none in the map, as
  the program still assembled.

Locations are lines, not columns: the assembler is line based, and a debugger's
breakpoints and source view are too.

### `Create`'s validation

So that the lookups never index out of range, and a map read from a file is as
sound as one assembled, `Create` validates that:
- Every location's file is in range, and its line isn't negative.
- Every statement's segment is in range, its size is at least 1, it doesn't
  pass the end of a segment (64 Kiw), and no two statements in a segment
  overlap.
- Every label's segment is in range, and its offset is from 0 to the end of a
  segment (a label can be at the end of a full segment).
- Label and constant names aren't empty, and each is used once among both, as
  in the assembler. Segment names are each used once, and only the only
  segment can have no name.

It doesn't check the map against a `Program`, which a map file doesn't hold.

### Map files

- A Game Bits chunk file of type `OZMP`, with at most one chunk each for the
  files (`FILE`), segments (`SEGM`), statements (`STMT`), labels (`LABL`), and
  constants (`CNST`), each at version 1. The writer writes all five, and a
  chunk the file doesn't have reads as empty. Strings are null terminated
  `ChunkPtr<const char>`s (an empty one is written as its terminator alone,
  and a null one reads as empty), and the rest are fixed size fields, with a
  constant's word value beside a flag for whether it has one.
- Reading fails if the file can't be opened, isn't a chunk file or is
  corrupt, or is another type of chunk file; if a chunk is given twice, is a
  version this doesn't read, or is too small for its count of entries; if a
  string isn't in its chunk or doesn't end there; if a word value flag isn't
  0 or 1; or if `Create` rejects the map. Other chunk types are skipped, so a
  later version can add chunks that older readers ignore.
- `ChunkReader` validates entries and strings against the chunk's size (Game
  Bits *Validate chunk contents*), so a corrupt map file fails to read rather
  than reading out of bounds.
- Writing fails if the file can't be opened or written.
- The `.oz3map` extension is for `oz3asm` to choose paths with. Nothing here
  depends on it.

## Design

### Names

| Name           | Candidates                                            | Avoids                                                      | Choice                                                      |
| -------------- | ----------------------------------------------------- | ----------------------------------------------------------- | ----------------------------------------------------------- |
| The map        | `ProgramMap`, `SourceMap`, `DebugMap`                 | `MemoryMap` (core's memory mapped devices)                  | `ProgramMap`, as the design doc chose, after `.oz3map`      |
| Map file       | `program_map_file.h`, in `program_map.h`              | `program_file.h` (program files), which it pairs with       | `program_map_file.h`, as `program_file.h` is to `program.h` |
| Map statement  | `Statement`, `Span`, `Source`                         | the assembler's private `Statement`, which it is built from | `ProgramMap::Statement`, as the design doc says statements  |
| Label lookup   | `FindLabel`, `FindNearestLabel`, `GetLabelAt`         | `FindStatement`, which finds only the exact word            | `FindLabel`, with "nearest at or before" in its comment     |
| Chunk types    | `OZMP` (file), `FILE`, `SEGM`, `STMT`, `LABL`, `CNST` | Game Bits' own chunk types, which start with `GB`           | as listed                                                   |

### `ProgramMap` (`program_map.h`)

- Holds the five vectors, sorted by `Create`, so `FindStatement` and
  `FindLabel` are binary searches with no other index. Statements and labels
  sort by a `SegmentOffset` struct with a defaulted `<=>`.
- Validation errors name what is wrong by a kind and an id ("Statement 2",
  "Label START"), which are only formatted for an error.

### Map files (`program_map_file.h`, `program_map_chunks.h`)

- Each chunk's entries are trivially copyable structs of sized integers, laid
  out in 8-byte blocks as `ChunkWriter`'s comment asks. They and the chunk
  types are in `program_map_chunks.h`, as the format's documentation, and so
  tests can write corrupt chunks. It is apart from `program_map_file.h` (as
  Game Bits' `resource_chunks.h` is), so users of the API don't see the
  format.
- Writing opens the file through `gb::FileSystem::OpenFile` and writes it with
  `gb::WriteChunkFile`.
- Reading is a private `MapFileReader`, which holds the path, the error, and
  each chunk type's entries and whether it has read one. It reads every chunk
  with `gb::ReadChunkFile`, then reads each by its type. `GetEntries`
  validates a chunk (given once, its version, and its size) and returns its
  entries, and `ReadString` validates each string with `ConvertToPtr`. Last,
  it calls `Create`.

### The assembler's map (`program_assembler.cc`)

- `AddContent` records each content's path, in the order added, and its index
  among them, which are the map's files.
- The private `Statement` has its size in words. `Write()` adds the statement
  that writes the words, with their size, so the size is given once.
- The private `Label` has its segment.
- `BuildProgram` ends with `BuildMap`, which builds the map from the recorded
  statements that wrote words, `labels_` (whose keys are already the map's
  names), and `constants_`. A constant's dword value is cached from
  `CheckConstant`, and its word value is evaluated with `EvaluateConstant`,
  which returns an error rather than reporting it. A map `Create` rejects is
  an internal error (`CHECK`), as the assembler made it.

### Brittleness

- The map's statements and the program's words come from the same recorded
  statements, so they can't disagree.
- A map holds segment indices, not the segments themselves, so a debugger must
  pair a map with the program assembled with it. Nothing checks that the two
  belong together; a fingerprint of the program in the map could, if it turns
  out to be needed.
