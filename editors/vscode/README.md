# OZ-3 Languages for VS Code

Syntax highlighting for the OZ-3's two source languages:

- **OZ-3 Program** (`.oza`): program source, as the manual's
  [Program Source](../../docs/manual/2.5-Program-Source.md) describes it.
  It highlights comments, labels and local labels, instructions and their
  extensions, directives (`.CONST`, `.SEGMENT`, `.IF`, `.MACRO`, `.REPT`, and
  the rest), registers, numbers (`15`, `0Fh`, `1111b`), both kinds of string,
  the address forms `S()`, `D()`, and `E()`, `LOW()`, `HIGH()`, `SIZE()`,
  segment attributes, `$`, and operators. A directive the assembler doesn't
  have is shown as an error.
- **OZ-3 Instruction Set** (`.izm`): instruction set source, as the manual's
  [Instruction Set Source](../../docs/manual/2.3-Instruction-Set-Source.md) and
  [Microcode](../../docs/manual/2.1-Microcode.md) describe it. It highlights
  `instruction`, `macro`, `values`, and `code` definitions and their options,
  source patterns and their placeholders and value types, microcode
  operations, registers (with byte and nibble parts, such as `a:H`), banks,
  status flags, conditions, port modes, labels, and macro calls.

An instruction set's mnemonics come from its `.izm`, not the assembler, so in a
`.oza` the first word of a statement is shown as an instruction, whatever it
is. A macro invocation looks the same.

It also sets each language's comment character, so Toggle Line Comment works,
and brackets, auto-closing quotes, and folding of `.MACRO`, `.REPT`, and `.IF`
blocks.

## Installing

In the [devcontainer](../../.devcontainer), such as a GitHub Codespace, it is
installed already: each time VS Code attaches, the container packages it from
the checkout and installs it, so a change to it arrives on the next attach. If
the extension doesn't show in a new container, run Developer: Reload Window.

Elsewhere, the extension is just these files, with nothing to build. Either:

- **Link it into VS Code's extensions.** On Windows, from the repository root
  in PowerShell (a junction needs no admin rights):

  ```
  New-Item -ItemType Junction -Path "$env:USERPROFILE\.vscode\extensions\oz3.oz3-languages-0.1.0" -Target "$PWD\editors\vscode"
  ```

  On Linux or macOS:

  ```
  ln -s "$PWD/editors/vscode" ~/.vscode/extensions/oz3.oz3-languages-0.1.0
  ```

  Then restart VS Code. Changes to the grammars show after Developer: Reload
  Window.
- **Or package it** with `python3 editors/vscode/package_vsix.py` from the
  repository root, which needs nothing but Python and writes
  `out/oz3-languages.vsix`, and install that with
  `code --install-extension out/oz3-languages.vsix --force`, or Extensions:
  Install from VSIX. `npx @vscode/vsce package` in this directory makes one
  too, with Node.

## Changing it

The grammars are TextMate grammars, in `syntaxes/`. Developer: Inspect Editor
Tokens and Scopes shows the scopes a token is given. Scope names follow VS
Code's conventions (`keyword.other.instruction`, `variable.language.register`,
and so on), so every color theme colors them.
