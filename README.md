[//]: # ( -*- mode: markdown; fill-column: 72; -*- )

# `FURBY.ASM`

Please see:
- https://archive.org/details/furby-source
- https://twitter.com/bobpoekert/status/1028763109471580160
- http://www.seanriddle.com/furbysource.pdf

Unfortunately the PDF seems to be a rather low resolution greyscale scan
of a printout.  [`pdftohtml`][pdftohtml] → [`tesseract`][tesseract] did
me little good, and unfortunately the "ABBYY FineReader 11.0 (Extended
OCR)" version [over on archive.org][furby-archive-org] was not much more
legible.

Some manual correction seems to be in order.

[pdftohtml]:
  https://poppler.freedesktop.org

[tesseract]:
  https://github.com/tesseract-ocr/tesseract

[furby-archive-org]:
  https://archive.org/details/furby-source


# Conventions

1. unix line endings
2. [emacs page break convention][emacs-pagebreaks] (`^L` / formfeed) at
   the end of each page for easier navigation
3. as close to vertically lined up as possible

[emacs-pagebreaks]:
  https://www.gnu.org/software/emacs/manual/html_node/emacs/Pages.html


# Assumptions and knowledge


## Character encoding [mojibake][mojibake] issues

The file seems to have been stored as [DOS codepage 437][CP437], then
interpreted as [ISO-8859-1][ISO-8859-1] or [Windows-1252][Windows-1252].
[Box-drawing characters][box-chars] that would have appeared in the
`B0`-`DF` range are instead rendered as extended Latin characters,
eg. *Ä* (`0xC4` in Windows-1252) instead of *─* (`0xC4` in CP437).

[mojibake]:
  https://en.wikipedia.org/wiki/Mojibake

[CP437]:
  https://en.wikipedia.org/wiki/Code_page_437

[ISO-8859-1]:
  https://en.wikipedia.org/wiki/ISO/IEC_8859-1

[Windows-1252]:
  https://en.wikipedia.org/wiki/Windows-1252

[box-chars]:
  https://en.wikipedia.org/wiki/Box-drawing_character#DOS


## Column wrapping issues

Long lines in the file are often wrapped at column 72/73 in places that
would have syntactic errors at compilation time.  The length of hanging
wrapped lines and the right margin positioning in some lines makes me
think the original file was intended to be wrapped at columns 82/83
(**assumption**), though of course column 80 would make the most sense.


## Programmatic correctness

I assume this was a printout of a working program, which implies that it
should be possible to find an assembler that can parse the input file
for at least syntactic validity.  This would be an excellent check of
transcription correctness, though of course not a perfect one (eg. typos
in comment blocks).


# Goals

Let's get all 297 pages of this PDF transcribed and committed to version
control so we have a useful resource where it's possible to hyperlink to
specific lines and file sections, perhaps eventually recover other
versions of the source code, and learn more about the design of the
device.

## Todo

- [ ] transcribe all 297 pages
- [x] correct CP1252/CP437 transliteration errors
  - [x] ...with a programmatic transformation step
- [x] correct column 72 / column 80 wrapping errors
  - [x] ...with a programmatic transformation step
- [ ] identify an assembler that can (attempt to) compile the source
- [ ] figure out what kind of license makes sense for this..?
