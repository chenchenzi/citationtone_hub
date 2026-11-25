# Trim each token to its voiced region

Keeps only the rows between a token's first and last voiced frame, so a
downstream measurement spans the syllable rather than the whole
recording. Frame-based trackers emit a frame every step across the
entire file, with `NA` (or 0 Hz) f0 in silence; without trimming, "21
equidistant points across the token" spends part of the grid on leading
and trailing silence.

## Usage

``` r
trim_to_voiced(
  df,
  token = "token",
  time = "time",
  f0 = "f0",
  min_run = 2,
  run = "voiced_run"
)
```

## Arguments

- df:

  Long-format f0 data frame.

- token, time, f0:

  Column names. Defaults `"token"`, `"time"`, `"f0"`. `time` must be the
  real time column, not a 0-1 axis.

- min_run:

  Minimum number of voiced frames in a row that an edge anchor must
  belong to. Default `2`.

- run:

  Name of a column that numbers the runs of f0 within each token, such
  as `voiced_run`. Used only when the column is present; `NULL` ignores
  it.

## Value

`df` with out-of-region rows removed, row order preserved.

## Details

The edges are anchored on a *run* of at least `min_run` voiced frames in
a row, so one stray voiced frame in silence (a common octave-error
artefact) cannot stretch the region. If no run is that long, the first
and last voiced frames are used. Tokens with no voiced frame at all keep
no rows, so count them before and after if their loss should be
reported. Rows with no time are dropped too.

A run ends at a gap. A gap is one of these:

- rows without f0 between two voiced frames;

- a time step longer than 1.5 times the token's usual step, so input
  with the rows without f0 already removed gives the same region. This
  needs a regular frame grid, so it does not apply to sparse input such
  as a `.PitchTier`;

- a change in the `run` column (`voiced_run`, which the Start page adds
  when it removes rows without f0 or marks gaps), when the data have
  one.

Frames *inside* the region are kept whether or not they are voiced, so
an internal unvoiced stretch (a medial voiceless stop, a creaky patch)
stays in place and still reads as `NA`.

Data whose rows without f0 were removed beforehand have no silence left
to trim. Only a short run cut off by a gap at a token's start or end can
still go, and only where the time steps (on a regular grid, see
[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md))
or the `run` column show the gap. Without either, every row is kept.

## See also

[`resample_f0_equal()`](https://chenchenzi.github.io/citationtone_hub/reference/resample_f0_equal.md),
which is normally applied after this;
[`filter_interval_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/filter_interval_rows.md)
for a TextGrid-interval region instead;
[`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md),
which measures the same region.
