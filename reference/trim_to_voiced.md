# Trim each token to its voiced region

Keeps only the rows between a token's first and last voiced frame, so a
downstream measurement spans the syllable rather than the whole
recording. Frame-based trackers emit a frame every step across the
entire file, with `NA` (or 0 Hz) f0 in silence; without trimming, "21
equidistant points across the token" spends part of the grid on leading
and trailing silence. Sparse input such as a `.PitchTier` has no silent
rows to trim, so this is a no-op there.

## Usage

``` r
trim_to_voiced(df, token = "token", time = "time", f0 = "f0", min_run = 2)
```

## Arguments

- df:

  Long-format f0 data frame.

- token, time, f0:

  Column names. Defaults `"token"`, `"time"`, `"f0"`.

- min_run:

  Minimum number of consecutive voiced frames an edge anchor must belong
  to. Default `2`.

## Value

`df` with out-of-region rows removed, row order preserved.

## Details

The edges are anchored on a *run* of at least `min_run` consecutive
voiced frames, so one stray voiced frame in silence (a common
octave-error artefact) cannot stretch the region. If no run is that
long, the first and last voiced frames are used. Tokens with no voiced
frame at all keep no rows — count them before and after if their loss
should be reported.

Frames *inside* the region are kept whether or not they are voiced, so
an internal unvoiced stretch (a medial voiceless stop, a creaky patch)
stays in place and still reads as `NA`.

## See also

[`resample_f0_equal()`](https://chenchenzi.github.io/citationtone_hub/reference/resample_f0_equal.md),
which is normally applied after this;
[`filter_interval_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/filter_interval_rows.md)
for a TextGrid-interval region instead.
