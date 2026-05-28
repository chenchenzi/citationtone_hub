# Flag sample-to-sample f0 jumps within tokens

Within each token, compute the semitone difference between consecutive
f0 samples. Flag samples where the rate of change exceeds physiological
plausibility (Sundberg 1973), where the Hz ratio crosses an octave
boundary, or where the sample is a *carryover* of an upstream error
(Steffman & Cole 2022).

## Usage

``` r
flag_pitch_jumps(
  data,
  f0 = "f0",
  token = "token",
  time = "time",
  time_unit = c("s", "ms", "norm"),
  rise_threshold = 1.263,
  fall_threshold = 1.714,
  octave_bounds = c(0.49, 1.99),
  carryover_mult = 1.5
)
```

## Arguments

- data:

  A long-format data frame, one row per f0 sample.

- f0:

  Column name of f0 (Hz). Default `"f0"`.

- token:

  Column name of token ID. Default `"token"`.

- time:

  Column name of time. Default `"time"`.

- time_unit:

  One of `"s"`, `"ms"`, `"norm"`. Default `"s"`. The "norm" option
  treats each step as one 10 ms-equivalent unit.

- rise_threshold:

  Maximum plausible rise in ST per 10 ms. Default `1.263` (Sundberg
  1973).

- fall_threshold:

  Maximum plausible fall in ST per 10 ms. Default `1.714` (Sundberg
  1973).

- octave_bounds:

  Hz-ratio bounds outside which a step is flagged as an octave jump.
  Default `c(0.49, 1.99)` (halving / doubling).

- carryover_mult:

  Carryover band as a multiple of the rise/fall threshold (in
  semitones). `0` disables carryover. Default `1.5`.

## Value

The input data frame with two appended columns: `flagged_jump` (logical)
and `jump_note` (character; one of `"jump (rise)"`, `"jump (fall)"`,
`"octave jump"`, `"carryover"`, or compound notes joined by `"; "`).

## Details

Rise / fall thresholds are expressed as semitones per 10 ms; the
function rescales the observed rate using `time_unit` to compare against
them. Zero-valued f0 samples (unvoiced markers from pitch trackers) are
treated as missing before any computation.
