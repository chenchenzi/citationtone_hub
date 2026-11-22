# Read a CSV in any Unicode encoding

Reads a CSV (or tab-separated) file saved as UTF-8 (with or without a
byte-order mark), UTF-16 or UTF-32, and returns its text as UTF-8. This
covers Praat's tables, which Praat saves as UTF-16 whenever any cell is
not plain ASCII (for example Chinese token names), and Excel's "CSV
UTF-8" and "Unicode Text" formats. The app reads every uploaded CSV this
way.

## Usage

``` r
read_csv_utf8(file, ..., sep = NULL, name = basename(file))
```

## Arguments

- file:

  Path to the file.

- ...:

  Passed to
  [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html), e.g.
  `stringsAsFactors`, `check.names`, `colClasses`, `na.strings`. Not
  `fileEncoding`, `encoding` or `text`, which are set here.

- sep:

  Field separator. `NULL` (default) picks a comma or a tab from the
  first line, as described in Details.

- name:

  File name to use in messages. Defaults to the file's own name; give
  the original name when `file` is a temporary copy, as with a Shiny
  upload.

## Value

A data frame as from
[`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html), with all
text (values and column names) in UTF-8. Attribute `"encoding"` gives
the encoding the file was read as (e.g. `"UTF-8"`, `"UTF-16BE"`,
`"GB18030"`), and attribute `"encoding_note"` a sentence for the user
when a code page had to be guessed or bytes that are not UTF-8 were
replaced (otherwise it is absent). On failure it stops with an error of
class `"shinytone_read_error"` whose message names the file.

## Details

The encoding is found from the byte-order mark, or, without one, from
where the zero bytes fall (UTF-16 stores each ASCII character with a
zero byte). A file with neither is read as UTF-8. If its text is not
valid UTF-8, it was saved in an older 8-bit code page. It is read as
Chinese GBK/GB18030 (as Excel on Chinese Windows saves CSVs) or Big5
when most of its other characters are common Chinese characters,
punctuation or pinyin in that code page, and as Windows-1252 (Western
European) otherwise. Either way the result carries a note, as this is a
guess.

A UTF-8 file can also hold a few bytes that are not UTF-8, such as one
cell pasted in from a Latin-1 source. When its valid UTF-8 characters
far outnumber those bytes and it does not read as Chinese GBK or Big5,
it stays UTF-8: only those bytes are replaced, each with U+FFFD (the
replacement character), and the note names their lines. This also
applies to a file with a UTF-8 byte-order mark.

Zero bytes in a text file, such as the padding a crashed save can leave
at its end, are dropped.

The separator is a comma unless the first line has tabs and no commas,
as in Praat's "Save as tab-separated file".

Excel workbooks (.xlsx, .xls), other binary files, empty files and
UTF-16 files with broken characters are refused with an error that says
what to do, rather than read in part.

## Examples

``` r
# A UTF-16LE file with a byte-order mark, as Excel's "Unicode Text" writes
f <- tempfile(fileext = ".csv")
txt <- "token,f0\nma1,210\nma2,180\n"
writeBin(c(as.raw(c(0xFF, 0xFE)),
           iconv(txt, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), f)
df <- read_csv_utf8(f)
df
#>   token  f0
#> 1   ma1 210
#> 2   ma2 180
attr(df, "encoding")
#> [1] "UTF-16LE"
```
