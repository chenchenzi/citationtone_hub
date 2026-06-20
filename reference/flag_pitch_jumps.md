# Flag sample-to-sample f0 jumps within tokens

Detects sample-level pitch-tracking artefacts inside each token. Three
classes of artefact are flagged: rate-of-change violations (faster rises
or falls than human vocal folds can plausibly produce), octave jumps
(pitch halving or doubling), and carryover samples that follow a flagged
frame and stay close enough to the error to be suspected of being part
of the same artefact.

Used as the sample-level counterpart of
[`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
in the Inspect tab of the Shiny app and inside
[`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md).

## Usage

``` r
flag_pitch_jumps(
  data,
  f0 = "f0",
  token = "token",
  time = "time",
  time_unit = c("s", "ms"),
  rise_threshold = 1.263,
  fall_threshold = 1.714,
  octave_bounds = c(0.49, 1.99),
  carryover_mult = 1.5
)
```

## Arguments

- data:

  A long-format data frame with one row per f0 sample.

- f0:

  Column name of f0 in Hz. Default `"f0"`.

- token:

  Column name of token ID. Default `"token"`.

- time:

  Column name of time. Default `"time"`.

- time_unit:

  One of `"s"` or `"ms"`. Default `"s"`. Inspection is meant to run on
  real-time data; a normalised-time option was removed because the
  physiological rate thresholds below lose their meaning once real time
  is discarded.

- rise_threshold:

  Maximum plausible rise in ST per 10 ms. Default `1.263` (Sundberg
  1973).

- fall_threshold:

  Maximum plausible fall in ST per 10 ms. Default `1.714` (Sundberg
  1973).

- octave_bounds:

  Hz-ratio bounds outside which a step is flagged as an octave jump.
  Default `c(0.49, 1.99)` (halving or doubling).

- carryover_mult:

  Carryover band as a multiple of the rise/fall threshold (in
  semitones). `0` disables carryover. Default `1.5`.

## Value

The input data frame with two appended columns:

- `flagged_jump`: logical, `TRUE` for samples flagged as artefacts or
  carryover frames.

- `jump_note`: character describing each flag (e.g., `"jump (rise)"`,
  `"jump (fall)"`, `"octave jump"`, `"carryover"`, or compound notes
  joined by `"; "`).

## Details

### What the function does internally

1.  Treat f0 = 0 as `NA` (pitch trackers commonly mark unvoiced frames
    with 0 Hz).

2.  Within each token, compute the semitone difference between
    consecutive f0 samples and the Hz ratio between them.

3.  Rescale the observed rate of change to a semitones-per-10-ms
    equivalent using `time_unit`, so the result can be compared to a
    fixed threshold (Sundberg 1973).

4.  When a jump is detected between samples *i* and *i + 1*, flag
    whichever side is FARTHER from the token's median f0 (in semitones).
    The Steffman & Cole (2022) convention always flags the landing
    sample, which mis-identifies the artefact when it sits at the start
    of a token (e.g., an octave doubling on the first frame). The
    median-distance rule reduces to landing-side flagging when the
    landing sample is the outlier and to source-side flagging when the
    source sample is the outlier.

5.  Walk both forward and backward from each flagged sample as a chain
    of carryover frames that stay within `carryover_mult * threshold`
    semitones of the artefact's f0 (adapted from Steffman & Cole 2022).

### Choosing the thresholds

Default `rise_threshold` and `fall_threshold` come from Sundberg's
(1973) classic study of the maximum rate of f0 change achievable in
singing. Changes exceeding these rates are physiologically implausible
and almost always tracking errors. Tightening these values catches more
borderline cases at the cost of more false positives.

Default `octave_bounds = c(0.49, 1.99)` flags any successive Hz ratio
more extreme than approximate halving or doubling, the dominant failure
mode of autocorrelation-based pitch trackers.

Setting `carryover_mult = 0` disables the carryover extension and flags
only the landing sample of each jump. The default of `1.5` follows
Steffman & Cole (2022).

## References

Steffman, J., & Cole, J. (2022). An automated method for detecting f0
measurement jumps based on sample-to-sample differences. *JASA Express
Letters*, 2(11), 115201.
[doi:10.1121/10.0015045](https://doi.org/10.1121/10.0015045)

Sundberg, J. (1973). The acoustics of the singing voice. *Scientific
American*, 229(3), 82–91.

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

- [`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
  for the complementary token-level outlier check.

- [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  for the wrapper that joins both.
