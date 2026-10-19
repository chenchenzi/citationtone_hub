# Tier names across a set of TextGrids, read quickly

Scans each file's text for tier headers instead of parsing it with
[`rPraat::tg.read()`](https://rdrr.io/pkg/rPraat/man/tg.read.html),
which takes tens of milliseconds per file, so it is fast enough to run
over every TextGrid in a corpus. Reads Praat's long and short text
formats, in UTF-8 or UTF-16 (with a byte-order mark). Files that cannot
be read, or are in Praat's binary format, contribute no names.

## Usage

``` r
tg_tier_names(tg_paths)
```

## Arguments

- tg_paths:

  Character vector of `.TextGrid` paths (NA / "" entries skipped).

## Value

Character vector of unique tier names (interval and point tiers), in the
order first seen.
