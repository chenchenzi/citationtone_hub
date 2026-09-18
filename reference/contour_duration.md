# Duration of each f0 contour

Adds the time from the first to the last frame with a measured f0 in
each unit: a whole token, or each labelled segment of a landmark set
(e.g. each syllable) within a token. This is the span the contour
covers, not the length of the unit's interval: unmeasured frames at
either edge do not count, and unmeasured frames inside the span (a gap)
do not shorten it.

## Usage

``` r
contour_duration(
  df,
  token = "token",
  time = "time",
  f0 = "f0",
  set = NULL,
  name = NULL,
  min_run = 2
)
```

## Arguments

- df:

  Long-format f0 data frame.

- token, time, f0:

  Column names. Defaults `"token"`, `"time"`, `"f0"`.

- set:

  Landmark-set base name (e.g. `"syllable"`) to measure each segment of
  that set, from its `<set>_start` / `<set>_end` columns (see
  [`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md)).
  `NULL` (default) measures whole tokens.

- name:

  Name of the new column. Default `"token_f0_dur"`, or `"<set>_f0_dur"`
  with `set`.

- min_run:

  Minimum number of consecutive measured frames the first and last frame
  must belong to. Default `2`; `1` takes the first and last measured
  frames as they are.

## Value

`df` with the duration column appended, in the units of the `time`
column. Row order is preserved.

## Details

A frame counts as measured when its f0 is a number other than 0; `NA`
and 0 (which pitch trackers write for unvoiced frames) do not count.
Negative values do, so a normalised f0 column (semitones, z-scores)
works as well as Hz. A unit with a single measured frame gets 0, and a
unit with none gets `NA`.

The first and last frames are anchored on a *run* of at least `min_run`
consecutive measured frames, the rule
[`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md)
uses for the voiced region. A lone measured frame cut off from the
contour by unmeasured frames, such as a tracking artefact in the
silence, then cannot stretch the duration. If no run is that long, the
first and last measured frames are used. On native frames the default
makes a whole token's duration equal to its voiced span (`voiced_s` in
the F0 Extraction summary). Input without unmeasured rows (a
`.PitchTier`, or an export with those rows dropped) has no gaps to see,
so there every measured frame counts.

With `set`, frames are grouped by token and by the segment's `_start` /
`_end` boundaries. Only labelled segments are measured: frames in an
empty-label interval (silence, as written by
[`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md))
or outside the tier get `NA`.

The value is repeated on every row of its unit, so the column can serve
as a token- or segment-level predictor directly.

## See also

[`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md)
for the voiced span as a row filter;
[`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)
for a time axis within the same segments.

## Examples

``` r
df <- data.frame(token = "a", time = seq(0, 0.5, by = 0.1),
                 f0 = c(NA, 200, 210, NA, 190, NA))
contour_duration(df)$token_f0_dur   # 0.4 - 0.1 = 0.3, on every row
#> [1] 0.1 0.1 0.1 0.1 0.1 0.1
```
