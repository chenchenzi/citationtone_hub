# Flag tokens whose f0 has gaps inside the measured region

Adds two token-level columns so unvoiced stretches are visible in the
exported data rather than being silently scattered `NA`s: `n_missing`,
the number of that token's rows with no f0, and `has_gap`, `TRUE` when
at least one of those rows sits *between* two rows that do have f0.

## Usage

``` r
flag_f0_gaps(df, token = "token", f0 = "f0", time = "time")
```

## Arguments

- df:

  Long-format f0 data frame.

- token, f0, time:

  Column names. Defaults `"token"`, `"f0"`, `"time"`.

## Value

`df` with the `n_missing` and `has_gap` columns added.

## Details

The distinction matters because a missing value at the edge of a region
usually just means the region ran slightly past the voicing, while a
missing value in the middle means voicing was interrupted (a medial
voiceless stop, a creaky patch, or a tracking failure). Only the latter
sets `has_gap`, so `has_gap` marks the tokens worth looking at in F0
Correction. Run this on the native frames rather than on a resampled
grid: resampling can fill a short dropout from its nearer frame, leaving
no `NA` behind for this to find.

A token with no f0 at all gets `has_gap = FALSE`: there is no interior
to speak of. Existing columns of either name are replaced.

## See also

[`resample_f0_equal()`](https://chenchenzi.github.io/citationtone_hub/reference/resample_f0_equal.md),
[`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md).
