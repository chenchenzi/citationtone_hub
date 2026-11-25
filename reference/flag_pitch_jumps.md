# Flag sample-to-sample f0 jumps within tokens

Detects sample-level pitch-tracking artefacts inside each token. Three
classes of artefact are flagged: rate-of-change violations (faster rises
or falls than human vocal folds can plausibly produce), octave jumps
(pitch halving or doubling), and carryover samples on either side of a
flagged sample that stay close enough to the error to be suspected of
being part of the same artefact.

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

  Column name of time. Default `"time"`. Use the real time column, not a
  0-1, `*_tseq` or `*_tbound` axis.

- time_unit:

  One of `"s"` or `"ms"`. Default `"s"`. Inspection is meant to run on
  real-time data; a normalised-time option was removed because the
  physiological rate thresholds below lose their meaning once real time
  is discarded. The unit also sets the gap length in the notes. Check it
  against the data: times in ms read as seconds make every step look
  1000 times slower, so the rate test then flags almost nothing. A
  median step below 1 within tokens usually means seconds.

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
  joined by `"; "`). The part for a jump across a gap ends in
  `" across a N ms gap"`, e.g. `"jump (rise) across a 20 ms gap"`.

The rows come back sorted by token and time, with the token column
first.

## Details

### What the function does internally

1.  Set aside the rows without f0 and the rows with no time. A row has
    f0 when its value is above 0 Hz, so `NA`, 0 and negative markers
    such as -1 all count as no f0. These rows are never flagged: they
    get `flagged_jump = FALSE` and an empty note. The check therefore
    gives the same flags whether or not the data still contain rows
    without f0, and whatever the row order. Take any share of flagged
    samples over the samples with f0, so it does not change either.

2.  Within each token, in time order, compare each sample with the next
    sample that has f0: the semitone difference and the Hz ratio.

3.  Rescale the rate of change to a semitones-per-10-ms equivalent using
    the real time step between the two samples and `time_unit`, so the
    result can be compared to a fixed threshold (Sundberg 1973).

4.  When a jump is detected between samples *i* and *i + 1*, flag
    whichever side is FARTHER from the token's median f0 (in semitones).
    The Steffman & Cole (2022) convention always flags the landing
    sample, which mis-identifies the artefact when it sits at the start
    of a token (e.g., an octave doubling on the first frame). The
    median-distance rule reduces to landing-side flagging when the
    landing sample is the outlier and to source-side flagging when the
    source sample is the outlier. Across a gap a short fragment is
    flagged instead (see below).

5.  Walk both forward and backward from each flagged sample as a chain
    of carryover frames that stay within `carryover_mult * threshold`
    semitones of the artefact's f0 (adapted from Steffman & Cole 2022).
    A chain can continue across a gap.

### Gaps inside a token

A gap is a stretch inside a token with no f0. The function finds gaps
from the time steps between samples with f0, so it finds the same gaps
whether or not the rows without f0 are still there. A step is a gap when
it is more than 1.5 times the token's usual step. The usual step is the
token's median step. A token with fewer than 5 steps uses the file's
common step instead, when that is smaller.

Gaps are read only when the time grid is regular across the file, as in
[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md).
It is not regular, for example, when samples are timed at each glottal
pulse. Times written to the step itself, such as 10 ms frames written to
2 decimals, are read in whole steps (a step of 0.02 or more is a gap),
but only when at most 1 step in 100 is not exactly 0.01. When the grid
is not regular, no gaps are marked, and no note or placement below
applies. A token where two samples have the same time gets no gaps
either.

When the two samples of a jump lie on either side of a gap, the note
says so, for example `"jump (rise) across a 20 ms gap"`. The 20 ms is
the step minus the usual step, so a one-frame gap in 10 ms frames reads
as 10 ms. The rate test is still scaled by the real step, and the octave
test is the same as for any other pair.

Across a gap, the flag goes on the side whose run of samples (the
samples between gaps) is shorter, when that run has at most 5 samples. A
short fragment cut off by a gap is the likelier tracking error. When
both runs have the same length, or both are longer than 5 samples, the
side farther from the token median is flagged as usual.

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
only the one sample chosen for each jump. The default of `1.5` follows
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
