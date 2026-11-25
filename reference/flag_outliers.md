# Flag per-token f0 outliers using by-speaker z-scores

Identifies tokens whose maximum f0 is unusually high, or whose minimum
f0 unusually low, for a speaker's per-token f0 distribution. Suitable as
a first pass at detecting tracking errors, mis-segmentations, or
genuinely unusual productions before fitting contour models. Token-level
outlier removal is a recommended cleaning step prior to citation-tone
analysis (Xu & Zhang 2024).

## Usage

``` r
flag_outliers(
  data,
  f0 = "f0",
  token = "token",
  speaker = "speaker",
  z_threshold = 3,
  time = NULL
)
```

## Arguments

- data:

  A long-format data frame with one row per f0 sample.

- f0:

  Column name of f0 in Hz. Default `"f0"`.

- token:

  Column name of token ID. Default `"token"`.

- speaker:

  Column name of speaker ID. Default `"speaker"`.

- z_threshold:

  Signed z-score cutoff: a token is flagged when its per-token max
  exceeds `+z_threshold` (too high) or its min falls below
  `-z_threshold` (too low). Default `3`.

- time:

  Column name of time, or `NULL` (the default). Used only to break a tie
  between two speaker values that are equally frequent within a token:
  the one that comes first in time wins.
  [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  passes its `time`, so it picks the same speaker as
  [`fit_polynomial()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_polynomial.md).

## Value

A token-level data frame (one row per token) with columns:
`f0_token_max`, `f0_token_min`, `f0_token_mean`, `f0_token_sd`, `z_max`,
`z_min`, `flag_too_high`, `flag_too_low`, plus the original `token` and
`speaker` columns.

## Details

### What the function does internally

1.  Drop the rows without f0. A row has f0 when its value is above 0 Hz,
    so `NA`, 0 (pitch trackers commonly output 0 for unvoiced frames)
    and negative markers such as -1 are all left out.

2.  Compute the per-token maximum, minimum, mean, and SD of `f0`. Each
    token's speaker is the most frequent non-empty value of `speaker` on
    its rows with f0. A tie goes to the value that comes first in `time`
    (when given), then to the first in alphabetical (C-locale) order, so
    neither the row order nor the rows without f0 change it.

3.  Within each speaker, z-score the per-token maxima against each other
    and the per-token minima against each other (so a speaker with
    consistently high f0 isn't flagged as an outlier of the corpus).

4.  Flag a token when its maximum is unusually high
    (`z_max > z_threshold`, `flag_too_high`) or its minimum unusually
    low (`z_min < -z_threshold`, `flag_too_low`). Each test is
    one-sided: the gross tracking errors this coarse screen targets are
    directional (octave-doubling or a spurious spike inflates the
    maximum; octave-halving, a subharmonic, or creak deflates the
    minimum), so only the outer tails are treated as errors.

Because the maxima and minima are pooled across all of a speaker's
tones, the opposite tails are occupied by legitimate tone identity
rather than error (a low ceiling is what a genuine low tone looks like;
a high floor is what a genuine high tone looks like), so they are
deliberately not flagged here. The rarer artefacts that do sit in those
tails (a truncated, abnormally low maximum or a floored, abnormally high
minimum) are left to the finer screens
[`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md)
(within speaker and tone) and
[`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
(sample level).

### Choosing `z_threshold`

The default of `3` corresponds to the standard convention that ±3 SDs
cover 99.7% of a normal distribution. Because each criterion is
one-sided (`z_max > 3`, or `z_min < -3`), under that assumption only
about 0.1% of tokens lie beyond either cutoff. Lower thresholds (e.g.,
`2` or `2.5`) are more aggressive, useful when manual review of every
flagged token is feasible. Higher thresholds (`4`+) are more
conservative.

Token z-scoring per speaker assumes each speaker contributes enough
tokens for the SD to be meaningfully estimated; results for speakers
with only a handful of tokens should be treated as advisory.

## References

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

- [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
  for complementary sample-level artefact detection (octave jumps,
  rate-of-change violations).

- [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  for the convenience wrapper that runs both and joins the results.

## Examples

``` r
data(sample_f0)
out <- flag_outliers(sample_f0,
                     f0      = "f0_Hz",
                     token   = "token",
                     speaker = "speaker",
                     z_threshold = 3)
table(out$flag_too_high, useNA = "ifany")
#> 
#> FALSE  TRUE 
#>  1839     9 
table(out$flag_too_low,  useNA = "ifany")
#> 
#> FALSE  TRUE 
#>  1846     2 
```
