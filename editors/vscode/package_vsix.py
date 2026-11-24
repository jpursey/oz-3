# Copyright (c) 2026 John Pursey
#
# Use of this source code is governed by an MIT-style License that can be found
# in the LICENSE file or at https://opensource.org/licenses/MIT.

"""Packages the OZ-3 Languages extension as a .vsix, without Node or vsce.

    python3 editors/vscode/package_vsix.py [<output file>]

The output file defaults to out/oz3-languages.vsix in the repository. Install
it with `code --install-extension <output file> --force`. The devcontainer does
both each time VS Code attaches to it.
"""

import json
import pathlib
import sys
import zipfile
from xml.sax.saxutils import escape, quoteattr

EXTENSION_DIR = pathlib.Path(__file__).resolve().parent
REPOSITORY_DIR = EXTENSION_DIR.parent.parent
DEFAULT_OUTPUT = REPOSITORY_DIR / "out" / "oz3-languages.vsix"

# The extension's own files: everything VS Code reads is JSON, plus the README.
PACKAGED_SUFFIXES = (".json", ".md")

CONTENT_TYPES = (
    '<?xml version="1.0" encoding="utf-8"?>'
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
    '<Default Extension=".json" ContentType="application/json"/>'
    '<Default Extension=".vsixmanifest" ContentType="text/xml"/>'
    '<Default Extension=".md" ContentType="text/markdown"/>'
    "</Types>"
)


def make_manifest(package):
    """Returns the extension.vsixmanifest for the extension's package.json."""
    return f"""<?xml version="1.0" encoding="utf-8"?>
<PackageManifest Version="2.0.0" xmlns="http://schemas.microsoft.com/developer/vsx-schema/2011">
  <Metadata>
    <Identity Language="en-US" Id={quoteattr(package["name"])} Version={quoteattr(package["version"])} Publisher={quoteattr(package["publisher"])} />
    <DisplayName>{escape(package["displayName"])}</DisplayName>
    <Description xml:space="preserve">{escape(package["description"])}</Description>
    <Categories>Programming Languages</Categories>
    <Properties>
      <Property Id="Microsoft.VisualStudio.Code.Engine" Value={quoteattr(package["engines"]["vscode"])} />
    </Properties>
  </Metadata>
  <Installation>
    <InstallationTarget Id="Microsoft.VisualStudio.Code"/>
  </Installation>
  <Dependencies/>
  <Assets>
    <Asset Type="Microsoft.VisualStudio.Code.Manifest" Path="extension/package.json" Addressable="true" />
  </Assets>
</PackageManifest>
"""


def main(argv):
    if len(argv) > 2:
        sys.exit(__doc__)
    output = pathlib.Path(argv[1]) if len(argv) == 2 else DEFAULT_OUTPUT
    package = json.loads((EXTENSION_DIR / "package.json").read_text("utf-8"))
    output.parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(output, "w", zipfile.ZIP_DEFLATED) as vsix:
        vsix.writestr("[Content_Types].xml", CONTENT_TYPES)
        vsix.writestr("extension.vsixmanifest", make_manifest(package))
        for path in sorted(EXTENSION_DIR.rglob("*")):
            if path.is_file() and path.suffix in PACKAGED_SUFFIXES:
                name = path.relative_to(EXTENSION_DIR).as_posix()
                vsix.write(path, "extension/" + name)
    print(f"Wrote {output}")


if __name__ == "__main__":
    main(sys.argv)
