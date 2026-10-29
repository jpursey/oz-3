# Program Maps

The `.oz3map` file that the assembler writes beside a program: its source
files, its segments, where each statement's words came from, and every symbol.
The assembler builds a `ProgramMap` along with the `Program`
(`AsmProgram::GetMap()`), a host writes it to a map file, and the debugger reads
it back. With it, a segment offset, such as `IP` in the code segment or a
label's value, becomes the source line that wrote the word there, and the label
it is at.

The map works in segment offsets, as every address in a program is one (see
Principles in [assembler_design.md](../assembler_design.md)). Turning an
address in a bank into a segment offset needs to know where each segment was
loaded, which `ProgramLoader` doesn't record yet. That is left to *Debugger
(oz3dbg)*, which is the first to have such addresses.

The map is only read by host tools, so it is a Game Bits chunk file, and isn't
part of the specification: there are no wiki changes.

## Behavior

### API

```
// Where the words of an assembled program came from, and its symbols, for a
// debugger.
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

  // A label or local label, whose value is its offset in its segment. A local
  // label's name is its scope's label and then its own ("START@LOOP").
  struct Label {
    std::string name;
    int segment = 0;  // An index into GetSegments().
    int offset = 0;   // Up to the end of a segment, 64 Kiw.
    Location location;  // Where it is defined.
  };

  // A constant, which has a value in each width it can be evaluated in, as
  // where it is used sets its width. Every constant has a dword value.
  struct Constant {
    std::string name;
    std::optional<uint16_t> word_value;
    uint32_t dword_value = 0;
    Location location;  // Where it is defined.
  };

  // Returns a map of these, or an error naming the first thing that is wrong
  // (see Create below). They can be in any order.
  static absl::StatusOr<ProgramMap> Create(std::vector<std::string> files,
                                           std::vector<std::string> segments,
                                           std::vector<Statement> statements,
                                           std::vector<Label> labels,
                                           std::vector<Constant> constants);

  // Returns the paths of the source files, the main file first.
  absl::Span<const std::string> GetFiles() const;

  // Returns the segments' names, as the program orders them. The default
  // program's one segment has no name.
  absl::Span<const std::string> GetSegments() const;

  // Returns the statements by segment, then offset.
  absl::Span<const Statement> GetStatements() const;

  // Returns the labels by segment, then offset, then name, and the constants
  // by name.
  absl::Span<const Label> GetLabels() const;
  absl::Span<const Constant> GetConstants() const;

  // Returns the statement that wrote the word at `offset` in `segment`, or null
  // if no statement did (such as words .DS reserves).
  const Statement* FindStatement(int segment, int offset) const;

  // Returns the label in `segment` nearest at or before `offset`, for writing
  // an address as "label+n", or null if there is none. Of several at the same
  // offset, it is the first by name, so a label comes before the local labels
  // in its scope.
  const Label* FindLabel(int segment, int offset) const;
};

// Writes `map` as a map file at `path`.
absl::Status WriteProgramMapFile(gb::FileSystem& file_system,
                                 std::string_view path, const ProgramMap& map);

// Reads the map in the map file at `path`, as written by WriteProgramMapFile.
absl::StatusOr<ProgramMap> ReadProgramMapFile(gb::FileSystem& file_system,
                                              std::string_view path);

class AsmProgram final {
 public:
  const Program& GetProgram() const;
  const ProgramMap& GetMap() const;  // New.
};
```

### What the assembler records

- **Files:** every file the assembler read, by the path it read it from (the
  main file as given to `AssembleProgramFile`, normalized, and each include
  resolved against the file that includes it), in the order first read, once
  each however often it is included. A program assembled from text has one
  file, `program`, as its errors do.
- **Segments:** each segment's name, in the program's order, so segment `i` in
  the map is `Program::segments[i]`.
- **Statements:** each instruction, `.DW`, and `.DD`, with its segment, offset,
  size in words, and its line. A statement in a file included twice is in the
  map twice, at the same line. `.DS` and the stack only reserve words, so they
  aren't statements in the map, and nor is a `.DW` or `.DD` with no words.
- **Labels**, each at its name's line, with its segment and offset. Labels and
  local labels are one kind, as a local label's name in the map is already
  unique: its scope's label and then its own (`START@LOOP`), or just its own
  before the program's first label (`@LOOP`). A debugger can tell a local
  label by its `@`.
- **Constants**, each at its name's line, with its value in each width. A
  constant is its expression, evaluated where it is used, and the word value
  isn't always the dword value's low word, wherever `/`, `%`, `>>`, or `!`
  bring down bits from above it: `.CONST X = (0FFFFh + 1) / 2` is 0 as a
  word, where the sum wraps, and `8000h` as a dword. Its `$` and local labels
  are fixed where it is
  defined, so these two values are all it can ever be. Every constant has a
  dword value (the assembler checks that), but not always a word value, such
  as `.CONST BIG = 10000h`, which is only usable as a dword. The assembler
  evaluates the word value for the map, and one that has an error is just
  left out, as the program assembled.

Locations are lines, not columns: the assembler is line based, and a debugger's
breakpoints and source view are too.

### `Create`'s checks

So that the lookups never index out of range, and a map read from a file is as
sound as one assembled, `Create` checks that:
- Every location's file is in range, and its line isn't negative.
- Every statement's segment is in range, its size is at least 1, it doesn't
  pass the end of a segment (64 Kiw), and no two statements in a segment
  overlap.
- Every label's segment is in range, and its offset is from 0 to the end of a
  segment (a label can be at the end of a full segment).
- Label and constant names aren't empty, and each is used once among both, as
  in the assembler. Segment names are each used once, apart from the default
  program's one unnamed segment.

It doesn't check the map against a `Program`, which a map file doesn't hold.

### Map files

- A Game Bits chunk file (`gb/file`'s `WriteChunkFile` and `ReadChunkFile`) of
  type `OZMP`, with at most one chunk each for the files (`FILE`), segments
  (`SEGM`), statements (`STMT`), labels (`LABL`), and constants (`CNST`), each
  at version 1. A chunk the file doesn't have is empty; the writer writes all
  five. Strings are null terminated `ChunkPtr<const char>`s (an empty
  one is written as just its terminator, and a null one reads as empty), and
  the rest are fixed size fields (a constant's word value with a flag for
  whether it has one).
- Reading fails if a chunk is given twice, is a version this
  doesn't read, is too small for its count of entries, or has a string that
  isn't in the chunk or doesn't end there, or if `Create` rejects the map.
  Other chunk types are skipped, so a later version can add chunks that older
  readers ignore.
- `ChunkReader` checks entries and strings against the chunk's size (Game Bits
  *Validate chunk contents*, which landed while this was planned), so a
  corrupt map file fails to read rather than reading out of bounds. That was
  the backlog's *Corrupt program maps*, which CL2 does.
- The `.oz3map` extension is for `oz3asm` to choose paths with. Nothing here
  depends on it.

## Design

All of it is in `tools`, beside the program assembler and program files.

### Names

| Name           | Candidates                                     | Avoids                                                                  | Choice                                                     |
| -------------- | ---------------------------------------------- | ----------------------------------------------------------------------- | ---------------------------------------------------------- |
| The map        | `ProgramMap`, `SourceMap`, `DebugMap`          | `MemoryMap` (core's memory mapped devices)                              | `ProgramMap`, as the design doc chose, after `.oz3map`     |
| Map file       | `program_map_file.h`, in `program_map.h`       | `program_file.h` (program files), which it pairs with                   | `program_map_file.h`, as `program_file.h` is to `program.h` |
| Map statement  | `Statement`, `Span`, `Source`                  | the assembler's private `Statement`, which it is built from             | `ProgramMap::Statement`, as the design doc says statements |
| Label lookup   | `FindLabel`, `FindNearestLabel`, `GetLabelAt`  | `FindStatement`, which finds only the exact word                        | `FindLabel`, with "nearest at or before" in its comment    |
| Chunk types    | `OZMP` (file), `FILE`, `SEGM`, `STMT`, `LABL`, `CNST` | Game Bits' own chunk types, which start with `GB`                | as listed                                                  |

### `ProgramMap` (`program_map.h`)

- Holds the five vectors, sorted by `Create` (statements by segment and
  offset, labels by segment, offset, and name, and constants by name), so
  `FindStatement` and `FindLabel` are binary searches with no other index.
- Copyable and movable, as `Program` is, so `StatusOr` and `AsmProgram` hold it
  by value.

### Map files (`program_map_file.h`)

- Each chunk's entries are trivially copyable structs of sized integers, laid
  out in 8-byte blocks as `ChunkWriter`'s comment asks. They and the chunk
  types are in `program_map_chunks.h`, as the format's documentation, and so
  tests can write corrupt chunks. It is apart from `program_map_file.h` (as
  Game Bits' `resource_chunks.h` is), so users of the API don't see the
  format.
- Writing opens the file through `gb::FileSystem::OpenFile`. Reading reads
  every chunk with `gb::ReadChunkFile`, then reads each by its type, with a
  flag for each type already read, through `ChunkReader::GetChunkData` and
  `ConvertToPtr`, which fail on a corrupt chunk. A private `MapFileReader`
  keeps the path and the error.

### The assembler's map (`program_assembler.cc`)

- The private `Statement` gains its size in words, which `Write()` already
  has, so the second pass doesn't recompute it.
- `BuildProgram` builds the map with the program: files from each lexer
  content's index in the order added, segment names, statements from the
  recorded statements that wrote words, labels from `labels_` (whose keys are
  already the map's names), and constants from `constants_` with their dword
  value, which `CheckConstant` evaluated, and their word value, evaluated now
  (`EvaluateConstant` returns an error rather than reporting it). It calls
  `Create`, and a map it rejects is an internal error (`CHECK`), as the
  assembler made it.

**Brittleness:** the map's statements and the program's words come from the
same recorded statements, so they can't disagree. A map holds segment indices,
not the segments themselves, so a debugger must pair a map with the program
assembled with it. Nothing checks that the two belong together; a fingerprint
of the program in the map could, if it turns out to be needed.

### To confirm

- `ChunkWriter::AddString("")` gives a null `ChunkPtr`, and `ConvertToPtr`
  turns it back into null, which the reader reads as an empty string (CL2).
  **Found:** it doesn't. `AddString("")` writes the terminator alone, at a
  real offset, so an empty string round trips as one. The reader still reads
  a null `ChunkPtr` as empty.
- A memory file system (`gb/file/memory_file_protocol.h`) can open a file for
  writing and reading chunks, for the tests (CL2). **Found:** it can, through
  `OpenFile`, with `WriteChunkFile` and `ReadChunkFile`.

## CLs

The status is `[ ]` not started, `[~]` in progress (set by the CL's first
edit), or `[x]` submitted (set in the CL's own commit).

### CL1 [x] tools: `ProgramMap`

Depends on: nothing.

- `program_map.h`/`.cc`: `ProgramMap`, `Create`, and its lookups.
- Unused, so no visible change.

**Verify**
- Standard checks (see CLAUDE.md).
- Unit tests: each of `Create`'s checks, including a label and a constant
  with the same name; statements, labels, and constants given out of order
  come back sorted; `FindStatement` at a statement's first and last word, in
  a gap, past the last statement, and in another segment; `FindLabel` before
  the first label, at a label, between labels, with a local label nearer than
  a label, a label and its local label at one offset, and ignoring other
  segments' labels.

### CL2 [x] tools: map files

Depends on: CL1, and Game Bits *Validate chunk contents*.

- `program_map_file.h`/`.cc`: `WriteProgramMapFile` and `ReadProgramMapFile`.
- `program_map_chunks.h`: the chunk types and entries of the format.
- *Corrupt program maps* comes out of the backlog, as this does it.
- Unused, so no visible change.

**Verify**
- Standard checks.
- Unit tests, on a memory file system: a map round trips, including an empty
  segment name, a constant with and without a word value, and an empty map;
  reading fails for a missing file, a file that isn't a chunk file, a
  truncated one, another chunk file type, a repeated chunk, an unknown
  version, a chunk too small for its count, a string past the end of its
  chunk or with no terminator, a bad word value flag, and a map `Create`
  rejects; missing chunks (or none) read as empty; a null string reads as
  empty; unknown chunks are skipped; writing to a path that can't be opened
  fails.
- The To confirm items for CL2, with the findings recorded here.

### CL3 [ ] tools: the assembler's map

Depends on: CL1.

- `AsmProgram::GetMap()`, built by `ProgramAssembler` (see Design).
- The design doc's Program map section and API sketch match what was built.

**Verify**
- Standard checks.
- Unit tests: the map of a default program from text (one file, `program`,
  one unnamed segment); of a program with includes on a memory file system,
  including a file included twice; with several segments, so statements and
  labels are in the right segment; instructions with `$v` and `$V` sized
  right, `.DW` strings, and `.DD`; `.DS` and the stack not in the map, so
  `FindStatement` there is null; lines of statements, labels, and constants in
  each file; labels, local labels in and before a scope, and constants with
  both values (`.CONST MASK = ~1` is `0FFFEh` and `0FFFFFFFEh`, and
  `(0FFFFh + 1) / 2` is 0 and `8000h`), and with only a dword value
  (`10000h`).
