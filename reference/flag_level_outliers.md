# Flag tokens whose overall f0 level is unusual for their speaker and tone

Detects tokens whose *whole contour* sits too high or too low compared
with other tokens of the **same speaker and same tone**, even when the
contour is smooth (no sample-to-sample jumps) and not extreme for the
speaker overall. This catches the "right shape, wrong height" case that
[`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
(which pools all tones within a speaker) and
[`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
(which only sees within-token movement) both miss — for example a
low-tone token mis-tracked up into the mid-tone band.

## Usage

``` r
flag_level_outliers(
  data,
  f0 = "f0",
  token = "token",
  speaker = "speaker",
  tone = "tone",
  level_threshold = 3.5,
  min_tokens = 5
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

- tone:

  Column name of tone category. Default `"tone"`.

- level_threshold:

  Absolute modified z-score above which a token's level is flagged.
  Default `3.5` (Iglewicz & Hoaglin 1993).

- min_tokens:

  Minimum number of same-speaker-same-tone tokens required to run the
  check for a group. Default `5`. More tokens give a more reliable
  estimate of the group's spread.

## Value

A token-level data frame (one row per token with at least one voiced
sample) with columns: `f0_token_median`, `level_st`, `n_tone_peers`,
`level_modz`, `flag_level_high`, `flag_level_low`, plus the original
`token`, `speaker`, and `tone` columns.

## Details

### What the function does internally

1.  Drop samples where `f0` is `NA` or `0` (unvoiced frames).

2.  Compute each token's **median f0**, converted to semitones
    (`12 * log2(Hz)`). The median (rather than the mean) is used so that
    a handful of mis-tracked frames — which
    [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
    already catches — do not shift a token's apparent level.

3.  Within each speaker x tone group, compute the **modified z-score**
    (Iglewicz & Hoaglin 1993) of each token's level:
    `0.6745 * (level - median) / MAD`, where MAD is the median absolute
    deviation. The modified z-score is on the same scale as an ordinary
    z-score but is built from the robust median/MAD, so a few deviant
    tokens cannot inflate the spread and mask themselves. When the MAD
    is zero (more than half the peers share an identical level), the
    function falls back to the mean-absolute-deviation form,
    `(level - median) / (1.2533 * meanAD)`.

4.  Flag a token as `level too high` / `level too low` when its modified
    z-score exceeds `+level_threshold` / falls below `-level_threshold`.

### Minimum group size

A spread cannot be estimated from a near-empty group, so the check only
runs for speaker x tone groups with at least `min_tokens` tokens; tokens
in smaller groups are returned with `level_modz = NA` and are never
flagged. Detection becomes more reliable as the number of
same-speaker-same-tone tokens grows: with very few tokens the median and
MAD are coarse estimates, so a borderline outlier can fall just under
the threshold. Citation-tone designs usually supply several such tokens
per cell (repetitions, items of differing syllable structure, ...), so
most speaker x tone groups comfortably exceed the minimum.

This is a token-level cleaning step intended to run on **raw f0 (Hz)
before normalisation**: the within speaker x tone comparison is
self-normalising, and cleaning before normalisation keeps mis-tracked
tokens from corrupting the by-speaker normalisation statistics.

## References

Iglewicz, B., & Hoaglin, D. C. (1993). *How to Detect and Handle
Outliers*. ASQC Quality Press.

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

- [`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
  for pooled per-speaker max/min outliers.

- [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
  for sample-level artefact detection.

- [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  for the wrapper that runs all three.

## Examples

``` r
data(sample_f0)
lvl <- flag_level_outliers(sample_f0,
                           f0      = "f0_Hz",
                           token   = "token",
                           speaker = "speaker",
                           tone    = "tone")
table(lvl$flag_level_high, lvl$flag_level_low, useNA = "ifany")
#>        
#>         FALSE TRUE
#>   FALSE  1755   22
#>   TRUE     71    0
```
