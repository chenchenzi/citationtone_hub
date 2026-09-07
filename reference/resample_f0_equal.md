# Resample each token's f0 contour to N equidistant points

For each token, replaces the native pitch-frame grid with `n` points
equally spaced between the token's first and last frame time, so every
token contributes the same number of samples at the same proportional
positions (with `n = 21`, one point every 5% of the token's duration;
with `n = 11`, every 10%). Intended for monosyllabic (single-contour)
tokens; for multisyllabic data, TextGrid landmark axes (see
[`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md))
are usually the better route.

## Usage

``` r
resample_f0_equal(
  df,
  n = 21,
  token = "token",
  time = "time",
  f0 = "f0",
  intensity = "intensity",
  method = c("linear", "nearest")
)
```

## Arguments

- df:

  Long-format f0 data frame.

- n:

  Number of points per token (at least 2). Default `21`.

- token, time, f0:

  Column names. Defaults `"token"`, `"time"`, `"f0"`.

- intensity:

  Name of the optional intensity column; used only when present in `df`.
  Default `"intensity"`.

- method:

  How each point takes its value, mirroring the two interpolation
  choices Praat offers in `Pitch: Get value at time...`: `"linear"`
  (default, and Praat's own default) applies the rule in Details;
  `"nearest"` always takes the value of the frame the point falls in,
  and is `NA` when that frame is unvoiced, so nothing is ever computed.

## Value

A data frame with the `token`, `point` (measurement number, `1`...`n`),
`time` (seconds, on the new grid), `time_prop` (proportional position
0-1 within the token), and `f0` columns, plus `intensity` (when present)
and any carried token-constant columns. Input columns named `point` or
`time_prop` are dropped, since those names are computed here. Rows are
sorted by time within each token; tokens keep their order of first
appearance. The attribute `dropped_columns` names any columns that could
not be carried.

## Details

f0 at each new point follows Praat's own rule for
`Pitch: Get value at time... Linear` (`Sampled_getValueAtX()` in Praat's
source), so a contour resampled here matches one extracted by a Praat
script querying the same times. Of the two native frames bracketing the
point, call the closer one *near* and the other *far*:

- both voiced: the value is linearly interpolated between them;

- *far* unvoiced: the point takes the *near* frame's measured value, so
  no value is blended across an unvoiced frame while a usable
  measurement at the edge of one is not thrown away;

- *near* unvoiced: the point is `NA`, since nothing was measured there.

A point landing exactly on a frame has a phase of 0 and therefore takes
that frame's value exactly. At a point falling exactly midway between
two frames the choice of *near* is a floating-point tie and may fall
either way; it only changes the answer when exactly one of the two is
unvoiced.

This rests on unvoiced frames being **present as `NA` (or 0 Hz) rows**,
which is how the wrassp and `.Pitch` paths deliver them. Sparse input
carries no such rows: a `.PitchTier`, or a CSV listing only voiced
samples, represents an unvoiced stretch as nothing more than a wide gap
between two voiced anchors, so both bracketing frames are voiced and the
point is interpolated straight across. Praat does the same on a
PitchTier. Add explicit `NA` rows if such stretches should read as
unvoiced.

With `method = "nearest"` (Praat's other option) the middle case goes
away: a point always takes the nearer frame's measured value, or `NA`
when that frame is unvoiced. Every exported value is then a number the
tracker produced, at the cost of a timing error of up to half a frame
step.

If an `intensity` column is present, it is linearly interpolated across
all frames with finite intensity (ends extended, matching how the
extraction aligns the intensity track to the f0 frames).

Any other column is carried through when it is constant within every
token (metadata, `token_dropped`, and similar token-level columns);
per-frame columns that vary within a token cannot survive a change of
grid and are dropped — their names are recorded in the `dropped_columns`
attribute of the result.

Rows with a missing time are dropped, and rows sharing a frame time are
collapsed to the first of them, so interpolation has strictly increasing
anchors. A token left with fewer than two distinct frame times cannot be
resampled and keeps its single remaining row, with `time_prop = 0.5`.

## See also

[`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md)
for adding a proportional time column without changing the sampling
grid.
