# Interval-tier names across a set of TextGrids

Reads up to `max_read` TextGrid files and returns the union of their
interval-tier names. Point tiers are ignored: landmarks come from
interval boundaries. Tiers are usually uniform across a corpus, so a
small sample is enough to populate a tier selector without reading
thousands of files.

## Usage

``` r
tg_interval_tiers(tg_paths, max_read = 12)
```

## Arguments

- tg_paths:

  Character vector of `.TextGrid` paths (NA / "" entries skipped).

- max_read:

  Maximum number of files to read.

## Value

Sorted character vector of unique interval-tier names (possibly empty).
When some of the files read could not be parsed, attribute
`"unreadable"` gives the reason for each, named by path (see
[`read_textgrid()`](https://chenchenzi.github.io/citationtone_hub/reference/read_textgrid.md)).
When some were read in a guessed 8-bit code page, attribute `"guessed"`
names that code page for the user (such as "Western European
(Windows-1252)"), named by path. When some were read as UTF-8 with bytes
that are not UTF-8 replaced, attribute `"stray"` gives those lines (such
as "line 12"), named by path.

## Details

Files are read with
[`read_textgrid()`](https://chenchenzi.github.io/citationtone_hub/reference/read_textgrid.md),
so UTF-16 TextGrids work too.
