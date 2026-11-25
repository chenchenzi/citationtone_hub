# Duration of each f0 contour

Adds the time from the first to the last frame with a measured f0 in
each unit: a whole token, or each labelled segment of a landmark set
(e.g. each syllable) within a token. This is the span the contour
covers, not the length of the unit's interval: rows without f0 at either
edge do not count, and rows without f0 inside the span (a gap) do not
shorten it.

## Usage

``` r
contour_duration(
  df,
  token = "token",
  time = "time",
  f0 = "f0",
  set = NULL,
  name = NULL,
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

- set:

  Landmark-set base name (e.g. `"syllable"`) to measure each segment of
  that set, from its `<set>_start` / `<set>_end` columns (see
  [`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md)).
  `NULL` (default) measures whole tokens.

- name:

  Name of the new column. Default `"token_f0_dur"`, or `"<set>_f0_dur"`
  with `set`.

- min_run:

  Minimum number of measured frames in a row that the first and last
  frame must belong to. Default `2`; `1` takes the first and last
  measured frames as they are.

- run:

  Name of a column that numbers the runs of f0 within each token, such
  as `voiced_run`. Used only when the column is present; `NULL` ignores
  it.

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
measured frames in a row, the rule
[`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md)
uses for the voiced region. A lone measured frame cut off from the
contour by a gap, such as a tracking artefact in the silence, then
cannot stretch the duration. If no run is that long, the first and last
measured frames are used. On native frames the default makes a whole
token's duration equal to its voiced span (`voiced_s` in the F0
Extraction summary).

A run ends at a gap. A gap is one of these:

- rows without f0 between two measured frames;

- a time step longer than 1.5 times the token's usual step. So data with
  the rows without f0 removed give the same durations as data with them
  kept. This needs a regular frame grid (see
  [`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)),
  so it does not apply to sparse input such as a `.PitchTier`;

- a change in the `run` column (`voiced_run`, which the Start page adds
  when it removes rows without f0 or marks gaps), when the data have
  one. It keeps the gaps on any grid.

Gaps are found over the whole token, from the real time column. With
`set`, each segment then counts only its own rows, so a run that crosses
a segment boundary is cut there. Frames are grouped by token and by the
segment's `_start` / `_end` boundaries, and rows with no token get `NA`.
Only labelled segments are measured: frames in an empty-label interval
(silence, as written by
[`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md))
or outside the tier get `NA`.

The value is repeated on every row of its unit, so the column can serve
as a token- or segment-level predictor directly.

## See also

[`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md)
for the voiced span as a row filter;
[`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)
for a 0-1 time axis over the same segments, from their first f0 to their
last, taken as they are (like `min_run = 1`, except that 0 in a signed
or normalised column counts as f0 there but not here).

## Examples

``` r
df <- data.frame(token = "a", time = seq(0, 0.6, by = 0.1),
                 f0 = c(NA, 200, 210, NA, 190, 195, NA))
contour_duration(df)$token_f0_dur   # 0.5 - 0.1 = 0.4, on every row
#> [1] 0.4 0.4 0.4 0.4 0.4 0.4 0.4

# A lone frame at 0.025 s, cut off by a gap, does not count. Removing the
# rows without f0 gives the same duration: the time step shows the gap.
df <- data.frame(token = "b", time = seq(0.005, 0.105, by = 0.01),
                 f0 = c(NA, NA, 300, NA, NA, 200, 205, 210, 212, 215, NA))
contour_duration(df)$token_f0_dur[1]                   # 0.095 - 0.055
#> [1] 0.04
contour_duration(df[!is.na(df$f0), ])$token_f0_dur[1]  # the same
#> [1] 0.04
```
