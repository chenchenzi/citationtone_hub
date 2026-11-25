# Inspect f0 data for token-level outliers and sample-level jumps

One-call convenience wrapper that runs
[`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md),
[`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md),
and (when `tone` is supplied)
[`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md)
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
  level_threshold = 3.5,
  min_tokens = 5,
  intensity = NULL,
  intensity_drop = 15,
  time_unit = c("s", "ms")
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

  Column name of tone category, or `NULL`. Default `"tone"`. When
  `NULL`, the token-level (speaker-by-tone) level check
  ([`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md))
  is skipped and the `tone` column is omitted from the output, so the
  wrapper can run before tone categories are known (e.g. the
  [`cluster_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_f0.md)
  tone-discovery workflow); the speaker-level and sample-level checks
  still run.

- z_threshold:

  Signed z-score cutoff: a token is flagged when its per-token max
  exceeds `+z_threshold` (too high) or its min falls below
  `-z_threshold` (too low). Default `3`.

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

- level_threshold:

  Absolute modified z-score above which a token's level is flagged.
  Default `3.5` (Iglewicz & Hoaglin 1993).

- min_tokens:

  Minimum number of same-speaker-same-tone tokens required to run the
  check for a group. Default `5`. More tokens give a more reliable
  estimate of the group's spread.

- intensity:

  Optional column name of intensity in dB. When supplied, the
  sample-level low-intensity check
  ([`flag_low_intensity()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_low_intensity.md))
  is run and its results are added (see Value). `NULL` (default) skips
  the check.

- intensity_drop:

  Flag voiced samples whose intensity is more than this many dB below
  the token's peak intensity. Default `15`. Ignored when `intensity` is
  `NULL`.

- time_unit:

  One of `"s"` or `"ms"`. Default `"s"`. Inspection is meant to run on
  real-time data; a normalised-time option was removed because the
  physiological rate thresholds below lose their meaning once real time
  is discarded. The unit also sets the gap length in the notes. Check it
  against the data: times in ms read as seconds make every step look
  1000 times slower, so the rate test then flags almost nothing. A
  median step below 1 within tokens usually means seconds.

## Value

A long-format data frame containing the original `token`, `time`, `f0`,
`speaker`, `tone` columns plus:

- `f0_token_max`, `f0_token_min`, `f0_token_mean`, `f0_token_sd`:
  per-token summary statistics.

- `flagged_jump`: logical, sample-level jump flag.

- `flagged_token`: logical, `TRUE` if the token has any extreme value,
  any sample-level jump, or an unusual overall level for its tone.

- `flag_notes`: human-readable concatenation of the reasons a sample was
  flagged (e.g. `"max too high"`, `"jump (rise)"`,
  `"jump (rise) across a 20 ms gap"`, `"level too high"`,
  `"low intensity"`), or `"no f0"` for each row of a token with no f0.

The rows come back sorted by token and time. When the input has a
`voiced_run` column (from
[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)),
it is kept, after the f0 column.

When `tone` is `NULL` the `tone` column is omitted and the level check
is not run, so `flag_notes` never contains `"level too high"` /
`"level too low"` and those checks do not contribute to `flagged_token`.
When `tone` is given, the attribute `"level_groups"` holds one row per
speaker x tone group, with `n_tokens` (its tokens with f0) and `checked`
(`TRUE` when `n_tokens` is at least `min_tokens`, so the level check ran
for the group). The attribute `"token_groups"` holds one row per token
with f0: its `token`, `speaker` and `tone`, as the level check read them
(the most frequent non-empty value on the token's rows with f0).

When `intensity` is supplied, the result additionally carries the
`intensity` column and a sample-level `flag_low_intensity` logical. The
low-intensity flag is advisory: it appears in `flag_notes` but does not
by itself set `flagged_token`.

## Details

### What the function does internally

1.  Call
    [`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
    for the speaker-level extreme-value check (`flag_too_high`,
    `flag_too_low`) plus per-token summary statistics.

2.  When `tone` is supplied, call
    [`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md)
    for the token-level (speaker-by-tone) level check
    (`flag_level_high`, `flag_level_low`). With `tone = NULL` this
    screen is skipped and both flags are set to `FALSE`.

3.  Call
    [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
    for the sample-level pitch-tracking artefact flags (`flagged_jump`,
    `jump_note`). It compares each sample with the next sample that has
    f0, with the rate scaled by the real time step. A jump across a gap
    (a stretch with no f0) gets `" across a N ms gap"` in its note, and
    the flag goes on a short run of at most 5 samples cut off by the
    gap; otherwise on the side farther from the token median.

4.  Left-join the token-level flags onto the long-format jump output, so
    every sample carries all three kinds of information.

5.  Set `flagged_token` to `TRUE` for any token that is a speaker-level
    extreme, an unusual level for its tone, or contains at least one
    sample-level jump.

6.  Concatenate the human-readable reasons into a single `flag_notes`
    column for display.

### Rows without f0

A row has f0 when its value is above 0 Hz. Rows without f0 (`NA`, 0 or a
negative marker such as -1) stay in the output but are never flagged as
samples: `flagged_jump` is `FALSE` on them, and no check uses their
values. They do carry their token's `flagged_token` and its token-level
notes, since the token-level flags are joined by token. So the flags on
the rows with f0 are the same whether or not the data still have these
rows (for example after
[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)).
Count shares of flagged samples over the samples with f0, as the Inspect
tab does, so they do not change either.

A token with no f0 at all cannot be checked. Its flags are all `FALSE`,
and each of its rows gets the note `"no f0"`. It does not count towards
the size of its speaker x tone group in the level check.

Use the individual functions
([`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md),
[`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md),
[`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md))
if you want only one kind of check or want to combine the outputs
yourself in a non-standard way.

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

- [`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md),
  [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md),
  and
  [`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md),
  the three components.

- [`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)
  for the downstream normalisation step.

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
#>  1586   262 

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

# Share of flagged samples, over the samples with f0
with_f0 <- !is.na(result$f0_Hz) & result$f0_Hz > 0
mean(result$flagged_jump[with_f0])
#> [1] 0.0170068
```
