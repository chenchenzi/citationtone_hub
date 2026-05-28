# Flag per-token f0 outliers using by-speaker z-scores

Identifies tokens whose maximum or minimum f0 lies far from the centre
of a speaker's per-token f0 distribution. Suitable as a first pass at
detecting tracking errors, mis-segmentations, or genuinely unusual
productions before fitting contour models. Token-level outlier removal
is a recommended cleaning step prior to citation-tone analysis (Xu &
Zhang 2024).

## Usage

``` r
flag_outliers(
  data,
  f0 = "f0",
  token = "token",
  speaker = "speaker",
  z_threshold = 3
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

  Absolute z-score above which a token is flagged. Default `3`, covering
  about 99.7% of a normal distribution.

## Value

A token-level data frame (one row per token) with columns:
`f0_token_max`, `f0_token_min`, `f0_token_mean`, `f0_token_sd`, `z_max`,
`z_min`, `flag_too_high`, `flag_too_low`, plus the original `token` and
`speaker` columns.

## Details

### What the function does internally

1.  Drop samples where `f0` is `NA` or `0` (pitch trackers commonly
    output 0 for unvoiced frames).

2.  Compute the per-token maximum, minimum, mean, and SD of `f0`.

3.  Within each speaker, z-score the per-token maxima against each other
    and the per-token minima against each other (so a speaker with
    consistently high f0 isn't flagged as an outlier of the corpus).

4.  Flag a token if `|z_max| > z_threshold` (the per-token max is too
    high or too low for this speaker) or `|z_min| > z_threshold`.

### Choosing `z_threshold`

The default of `3` corresponds to the standard convention that ±3 SDs
covers 99.7% of a normal distribution, so under that assumption only
about 0.3% of tokens are flagged. Lower thresholds (e.g., `2` or `2.5`)
are more aggressive, useful when manual review of every flagged token is
feasible. Higher thresholds (`4`+) are more conservative.

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
#>  1845     3 
```
