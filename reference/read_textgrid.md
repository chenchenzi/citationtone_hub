# Read a Praat TextGrid in any text encoding

Reads a TextGrid saved as a Praat text file (long or short format) and
parses it with
[`rPraat::tg.read()`](https://rdrr.io/pkg/rPraat/man/tg.read.html).
Praat saves a TextGrid as UTF-16 whenever a label or tier name is not
plain ASCII (Chinese characters or IPA, for example), and
[`rPraat::tg.read()`](https://rdrr.io/pkg/rPraat/man/tg.read.html)
cannot read those files on its own. Here the file is converted to UTF-8
first, so UTF-16 (with or without a byte-order mark), UTF-8 and older
8-bit encodings all read the same way.

## Usage

``` r
read_textgrid(path, name = basename(path))
```

## Arguments

- path:

  Path to a `.TextGrid` file.

- name:

  File name to use in the note. Defaults to the file's own name; give
  the original name when `path` is a temporary copy, as with a Shiny
  upload.

## Value

A list with five elements:

- `tg`: the TextGrid as returned by
  [`rPraat::tg.read()`](https://rdrr.io/pkg/rPraat/man/tg.read.html) (a
  named list of tiers), or `NULL` when the file could not be read.

- `encoding`: the encoding the file was read as, such as `"UTF-16BE"`,
  `"UTF-8"` or a guessed code page (`"CP1252"`, `"GB18030"`, `"BIG5"`);
  `NA` when the file could not be decoded.

- `reason`: `NULL` on success; otherwise a short phrase saying why the
  file could not be read, such as "it is in Praat's binary format; in
  Praat, use Save as text file".

- `note`: `NULL`, or a sentence for the user when the code page was
  guessed or bytes that are not UTF-8 were replaced.

- `stray_lines`: `NULL`, or the numbers of the lines that held bytes
  that are not UTF-8 in a file otherwise read as UTF-8.

## Details

A file with no byte-order mark that is not valid UTF-8 was saved in an
8-bit code page, which has to be guessed, as for
[`read_csv_utf8()`](https://chenchenzi.github.io/citationtone_hub/reference/read_csv_utf8.md).
Praat's own 8-bit files (its "try ISO Latin-1" preference) read as
Windows-1252, and files with mostly Chinese text as GBK or Big5. The
result then has a `note` that says so. A UTF-8 file with a few bytes
that are not UTF-8 stays UTF-8 instead, as in
[`read_csv_utf8()`](https://chenchenzi.github.io/citationtone_hub/reference/read_csv_utf8.md):
those bytes are shown as U+FFFD (the replacement character), and `note`
and `stray_lines` say where.

Praat writes a double quote inside a label or tier name as two (`""`);
these are turned back into one, as Praat shows them. Labels are
otherwise kept exactly as written: no Unicode normalisation is applied.

Praat's binary and chronological TextGrid formats cannot be read. Open
such a file in Praat and use Save as text file.

## Examples

``` r
# A short-format TextGrid with a Chinese label, saved as UTF-16BE with a
# byte-order mark, as Praat does by default
txt <- paste('File type = "ooTextFile"', 'Object class = "TextGrid"', "",
             "0", "0.5", "<exists>", "1", '"IntervalTier"', '"syllable"',
             "0", "0.5", "1", "0", "0.5",
             paste0('"', intToUtf8(0x5988), '"'), sep = "\n")
f <- tempfile(fileext = ".TextGrid")
writeBin(c(as.raw(c(0xFE, 0xFF)),
           iconv(txt, "UTF-8", "UTF-16BE", toRaw = TRUE)[[1]]), f)
if (requireNamespace("rPraat", quietly = TRUE)) {
  r <- read_textgrid(f)
  r$encoding
  r$tg$syllable$label
}
#> [1] "妈"
```
