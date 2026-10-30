// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_LISTING_H_
#define OZ3_TOOLS_PROGRAM_LISTING_H_

#include <string>

#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "oz3/tools/program.h"
#include "oz3/tools/program_map.h"

namespace oz3 {

// Returns a listing of an assembled program, for people to read: every line of
// its source, with where the line's words are and what they are. `map` is the
// program's map, and `sources` is the text of each of the map's files, in its
// order.
//
// The listing has each file in turn, the main file first, as its path (without
// its protocol) and then a row for each line, with a blank line between files.
// A row is a location, up to four words in hex, and the source line:
//
//   CODE:0000  0105 0003            start:  ADD R1, 3
//
// The location is the segment's name and the offset, or just the offset in the
// default program's one segment, which has no name. A name of more than eight
// characters is cut to its first seven and a `~` (`INTERRU~:0000`). A line that
// writes more than four words carries on in rows with no source line, each at
// the offset of its first word. A line that writes no words but defines a label
// has the label's location, and any other line has neither a location nor
// words. A file included more than once has a row for each statement a line
// wrote, with the source line in only the first.
//
// Returns an error if `sources` isn't one text for each of the map's files, or
// if the map doesn't fit the program and the sources: the map's segments aren't
// the program's, a statement's words aren't in the program's data, or a
// statement or label is on a line that its file doesn't have.
absl::StatusOr<std::string> FormatProgramListing(
    const Program& program, const ProgramMap& map,
    absl::Span<const std::string> sources);

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_LISTING_H_
