# Flag tokens whose f0 has gaps inside the measured region

Adds two token-level columns so unvoiced stretches are visible in the
exported data rather than being silently scattered `NA`s: `n_missing`,
the number of that token's rows with no f0, and `has_gap`, `TRUE` when
the token's voicing is interrupted somewhere between its first and last
f0.

## Usage

``` r
flag_f0_gaps(df, token = "token", f0 = "f0", time = "time", run = "voiced_run")
```

## Arguments

- df:

  Long-format f0 data frame.

- token, f0, time:

  Column names. Defaults `"token"`, `"f0"`, `"time"`. Without the `time`
  column, rows are taken in their order in `df` and only rows without f0
  mark a gap.

- run:

  Name of a column that numbers the runs of f0 within each token, such
  as `voiced_run`. Used only when the column is present; `NULL` ignores
  it.

## Value

`df` with the `n_missing` and `has_gap` columns added. Columns of these
names already in `df` are replaced, keeping their values for the tokens
with no rows without f0 (see Details).

## Details

The distinction matters because a missing value at the edge of a region
usually just means the region ran slightly past the voicing, while a
missing value in the middle means voicing was interrupted (a medial
voiceless stop, a creaky patch, or a tracking failure). Only the latter
sets `has_gap`, so `has_gap` marks the tokens worth looking at in F0
Correction. Run this on the native frames rather than on a resampled
grid: resampling can fill a short dropout from its nearer frame, leaving
no `NA` behind for this to find, and its equal time steps show no gap
either.

A gap is one of these:

- rows without f0 between two rows with f0;

- a time step longer than 1.5 times the token's usual step, so the gaps
  still show once the rows without f0 are removed. This needs a regular
  frame grid and a `time` column;

- a change in the `run` column (`voiced_run`), when the data have one.

Once the rows without f0 are removed, `n_missing` can no longer count
them. So for a token with no rows without f0, the values the data
already hold in `n_missing` and `has_gap` are kept, as they may record
rows removed earlier. `has_gap` is also set to `TRUE` there when the
token has a gap (above). Other tokens get both columns afresh. On data
cleaned beforehand that hold neither column and no `run` column, on a
grid that is not regular, no gap can be seen: such a token gets
`n_missing = 0` and `has_gap = FALSE`.

A token with no f0 at all gets `has_gap = FALSE`: there is no interior
to speak of.

## See also

[`resample_f0_equal()`](https://chenchenzi.github.io/citationtone_hub/reference/resample_f0_equal.md),
[`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md);
[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
for the gaps counted per token without changing the data.
