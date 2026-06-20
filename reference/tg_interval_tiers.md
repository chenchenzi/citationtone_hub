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
