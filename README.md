paladinfox
----------

#### **NOTE: WORK IN PROGRESS**
_The code isn't complete yet, don't expect it to run_

paladinfox is a PDF assembly format, and an assembler/disassembler.

The PDF format is designed to be edited by append: Instead of changing
the contents of the file, PDF editors attach new or modified elements
to the end of the existing file. This allows for edits on append-only
filesystems, and it preserves file history, but it also means that most
edits are just overlays on old pages.

Sometimes we want to edit the PDF directly, change the underlying data
without affecting the structure, or the other parts of the file. This is
where paladinfox comes in.

### The Format

When you run paladinfox on a PDF, you will get a folder containing a
number of files:
- A YAML file, which encodes the structural data of the PDF
- Visual page data, encoded as sequences of SVG files
- Any other embedded files, such as images, fonts, etc

_TODO_
