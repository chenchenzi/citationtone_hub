# Inspect f0 data for token-level outliers and sample-level jumps

Wrapper that runs
[`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
(by-speaker z-score outlier detection on per-token max / min) and
[`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
(sample-to-sample jumps, octave jumps, carryover), then combines their
results into a single long-format data frame with one row per f0 sample.

## Usage

``` r
inspect_f0(
  data,
  f0 = "f0",
  token = "token",
  time = "time",
  speaker = "speaker",
  tone = "tone",
  z_threshold = 3,
  rise_threshold = 1.263,
  fall_threshold = 1.714,
  octave_bounds = c(0.49, 1.99),
  carryover_mult = 1.5,
  time_unit = c("s", "ms", "norm")
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

- speaker:

  Column name of speaker ID. Default `"speaker"`.

- tone:

  Column name of tone category. Default `"tone"`.

- z_threshold:

  Absolute z-score above which a token is flagged. Default `3`, covering
  99.7% of a normal distribution.

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

- time_unit:

  One of `"s"`, `"ms"`, `"norm"`. Default `"s"`. The "norm" option
  treats each step as one 10 ms-equivalent unit.

## Value

A long-format data frame containing the original `token`, `time`, `f0`,
`speaker`, `tone` columns plus:

- `f0_token_max`, `f0_token_min`, `f0_token_mean`, `f0_token_sd` —
  per-token summaries

- `flagged_jump` — logical, sample-level jump flag

- `flagged_token` — logical, TRUE if the token has any extreme value or
  any sample-level jump

- `flag_notes` — human-readable concatenation of the reasons a sample
  was flagged
