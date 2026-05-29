# Inspect f0 data for token-level outliers and sample-level jumps

One-call convenience wrapper that runs both
[`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
and
[`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
on the same dataset, joins their results back into one long-format data
frame, and produces a single `flag_notes` column with human-readable
reasons for each flag. This is the function that backs the Inspect tab
of the Shiny app.

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

  A long-format data frame with one row per f0 sample.

- f0:

  Column name of f0 in Hz. Default `"f0"`.

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
  about 99.7% of a normal distribution.

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

- time_unit:

  One of `"s"`, `"ms"`, `"norm"`. Default `"s"`. The `"norm"` option
  treats each step as one 10 ms-equivalent unit.

## Value

A long-format data frame containing the original `token`, `time`, `f0`,
`speaker`, `tone` columns plus:

- `f0_token_max`, `f0_token_min`, `f0_token_mean`, `f0_token_sd`:
  per-token summary statistics.

- `flagged_jump`: logical, sample-level jump flag.

- `flagged_token`: logical, `TRUE` if the token has any extreme value or
  any sample-level jump.

- `flag_notes`: human-readable concatenation of the reasons a sample was
  flagged.

## Details

### What the function does internally

1.  Call
    [`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
    to get token-level outlier flags (`flag_too_high`, `flag_too_low`)
    plus per-token summary statistics.

2.  Call
    [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
    to get sample-level pitch-tracking artefact flags (`flagged_jump`,
    `jump_note`).

3.  Left-join the token-level flags onto the long-format jump output, so
    every sample carries both kinds of information.

4.  Set `flagged_token` to `TRUE` for any token that has a max/min
    z-outlier or contains at least one sample-level jump.

5.  Concatenate the human-readable reasons into a single `flag_notes`
    column for display.

Use the individual functions
([`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md),
[`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md))
if you want only one kind of check or want to combine the outputs
yourself in a non-standard way.

## References

Steffman, J., & Cole, J. (2022). Pitch tracking artefacts and the
detection of voicing errors in spontaneous speech.

Sundberg, J. (1973). The acoustics of the singing voice. *Scientific
American*, 229(3), 82–91.

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

- [`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
  and
  [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md),
  the two components.

- [`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)
  for the upstream normalisation step.

## Examples

``` r
data(sample_f0)
result <- inspect_f0(sample_f0,
                     f0      = "f0_Hz",
                     token   = "token",
                     time    = "time",
                     speaker = "speaker",
                     tone    = "tone")

# How many tokens were flagged at default thresholds?
table(unique(result[, c("token", "flagged_token")])$flagged_token)
#> 
#> FALSE  TRUE 
#>  1667   181 

# Inspect the first few flagged samples
head(result[result$flagged_jump, c("token", "time", "f0_Hz", "flag_notes")])
#> # A tibble: 6 × 4
#>   token             time f0_Hz flag_notes 
#>   <chr>            <dbl> <dbl> <chr>      
#> 1 dc102地4s46.6425 0.37   286. jump (rise)
#> 2 dc103笛4s50.6725 0.11   249. jump (fall)
#> 3 dc103雾2s71.2525 0.366  255. carryover  
#> 4 dc103雾2s71.2525 0.38   221. jump (fall)
#> 5 dc103题3s33.4525 0.115  222. jump (fall)
#> 6 dc103骂3s97.2325 0.08   247. jump (fall)
```
