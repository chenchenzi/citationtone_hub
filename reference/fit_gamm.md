# Fit a Generalised Additive Mixed Model (GAMM) to f0 contours

Fits a Generalised Additive Mixed Model with smooth (non-polynomial)
tone-by-time interactions. GAMMs are an increasingly common alternative
to GCA for dynamic speech analysis because they capture arbitrary smooth
contour shapes without requiring the user to choose a polynomial degree
(Wood 2017; Sóskuthy 2021; Xu & Zhang 2024).

This is a convenience wrapper around
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) that prepares the
data, builds a smooth-based formula with optional difference smooths and
speaker random smooths, fits the model, and optionally refits with an
AR1 correlation. For non-standard model structures, call
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) or
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) directly.

## Usage

``` r
fit_gamm(
  data,
  f0 = "f0",
  time = "time",
  token = "token",
  tone = "tone",
  speaker = "speaker",
  item = "item",
  duration = NULL,
  k = 10,
  bs = "tp",
  smooth_type = c("separate", "difference"),
  random_intercept_speaker = TRUE,
  random_intercept_item = TRUE,
  random_smooth = c("none", "speaker", "speaker_tone", "speaker_by_tone", "ref_diff"),
  use_ar1 = FALSE,
  time_normalised = c("auto", "no", "yes", "sequential")
)
```

## Arguments

- data:

  A long-format data frame with one row per f0 sample.

- f0, time, token, tone, speaker, item:

  Column names. Tone levels follow a factor's own levels, or else
  [`sort_tones()`](https://chenchenzi.github.io/citationtone_hub/reference/sort_tones.md)
  with `natural = FALSE`, which puts traditional Chinese tone names in
  their traditional order; the first level is the reference.

- duration:

  Optional column name with a per-sample duration covariate. Use `NULL`
  to omit the duration smooth.

- k:

  Basis dimension for the time smooth. Default `10`.

- bs:

  Spline basis type passed to
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html). Default `"tp"`
  (thin-plate regression spline).

- smooth_type:

  Either `"separate"` (one smooth per tone via `s(time, by = tone)`) or
  `"difference"` (reference smooth plus difference smooths via an
  ordered factor).

- random_intercept_speaker, random_intercept_item:

  Logical; include the corresponding `s(..., bs = "re")` random
  intercept.

- random_smooth:

  One of `"none"`, `"speaker"`, `"speaker_tone"`, `"speaker_by_tone"`,
  or `"ref_diff"`. See Details.

- use_ar1:

  Logical. If `TRUE`, fit once, estimate `rho` from the residuals' lag-1
  autocorrelation, then refit with that `rho`.

- time_normalised:

  One of `"auto"` (default: detect an already-normalised `[0, 1]` time
  column, or a `<tier>_tseq` or `<tier>_tbound` column, and use it
  as-is), `"no"` (always rescale to `[0, 1]` within each token), `"yes"`
  (declare the column already normalised to `[0, 1]`), or `"sequential"`
  (declare a sequential landmark axis running from 0 to the number of
  segments, whatever its name). See
  [`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md).

## Value

An S3 object of class `"shinytone_gamm"`, a list with:

- `model`: the fitted
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) object.

- `formula_str`: user-readable formula string.

- `rho`: `NULL` or the AR1 rho value.

- `convergence_warning`: `NULL` or the captured warning message.

- `smooth_type`, `random_smooth`: passed through (needed by
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)).

- `col_names`: original column names, for back-mapping.

- `time_prenormalised`: logical; was the time column used as-is because
  it was already normalised (to `[0, 1]`, or as a sequential landmark
  axis)?

- `time_axis`: `"unit"` for a `[0, 1]` axis, or `"sequential"` for a
  landmark axis kept on its `0`-to-`n` scale.

- `time_range`: the range of the time axis over the rows the model fits
  (rows with f0), which
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
  predicts across; `c(0, 1)` on a unit axis.

- `n_segments`: segments on a sequential axis, counted over the rows the
  model fits (`1` on a unit axis).

- `time_coverage`: per tone, the first and last time on the axis over
  the rows the model fits, as a data frame with `tone`, `t_first` and
  `t_last`. On a rescaled axis every tone runs from 0 to 1. On a time
  column used as it is, such as `time_prop` with no f0 at an end point,
  a tone can stop short of 0 or 1, and
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
  then extrapolates its curve to the ends.

## Details

### What the function does internally

1.  Normalise time to `[0, 1]` per token, from its first f0 (0) to its
    last f0 (1), so rows without f0 at a token's edges do not move it
    (see
    [`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md)).
    If the time column is *already* normalised (e.g. `token_t01` from
    [`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md)),
    it is detected with
    [`time_already_normalised()`](https://chenchenzi.github.io/citationtone_hub/reference/time_already_normalised.md)
    and used as it is; override with `time_normalised`. A sequential
    landmark axis (`<tier>_tseq` or `<tier>_tbound` from
    [`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md),
    where segment `i` runs from `i - 1` to `i`) is recognised by name
    and also used as it is, on its own `0`-to-`n` scale, so the segment
    boundaries stay at whole numbers. A per-token rescale would put them
    at 1/n, 2/n, ..., and would stretch a token whose first or last
    segment has no f0.

2.  Coerce `tone`, `speaker`, `item`, and (for difference smooths) an
    ordered `tone_ord` to factors with treatment contrasts.

3.  Build the formula based on `smooth_type` and `random_smooth`.

4.  Fit with [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html)
    using `discrete = TRUE` for speed on large datasets, capturing
    warnings.

5.  If `use_ar1 = TRUE`, estimate `rho` from the lag-1 autocorrelation
    of the residuals computed *within* tokens (pooling only genuine
    within-token neighbours, not across token boundaries), then refit
    with that `rho` plus per-token `AR.start` to correct for
    within-token correlation. `AR.start` marks each token's first row
    that is fitted, so rows without f0 at its start do not hide it.
    Inside a token the AR1 series does not restart at a gap:
    [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) leaves out
    the rows without f0, so the samples on either side of a gap count as
    neighbours, whether or not those rows are kept.

Rows are put in order of token and normalised time. Rows at the same
time keep their order in `data`, so give the rows of each token in time
order: the AR1 correction runs along that order.

### On `<tier>_tseq`, an f0 jump at each boundary

On a `<tier>_tseq` from
[`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md),
a stretch without f0 between two syllables takes no room: syllable 1's
last f0 and syllable 2's first f0 are both at 1. A falling syllable
followed by a high level one jumps in f0 there, with no time to make the
jump. One smooth per tone cannot follow such a jump, and raising `k`
does not help: the curve runs through the middle of the jump and misses
both syllables near the boundary. To model the whole word, use
`<tier>_tbound`: it keeps the stretch without f0 at each boundary, so
the two sides of a jump are a gap apart and one smooth can follow it,
and the boundaries still sit at whole numbers. `token_t01` from
[`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md)
keeps the stretch too (and the syllables' duration ratio), but the
boundary falls at a different point in each token. Or model each
syllable on its own, with one token per syllable on `<tier>_t01` and a
label for its tone pattern and place.

### Choosing `smooth_type`

- `"separate"` (default) fits one smooth per tone with
  `s(time, by = tone)`. Each tone curve is estimated independently.

- `"difference"` fits a reference smooth plus difference smooths via an
  ordered factor. Lets you test whether each non-reference tone
  significantly differs from the reference at any point along the time
  axis.

### Random smooth strategies

Following Sóskuthy (2021), several patterns are supported via
`random_smooth`:

- `"speaker"`: single by-speaker factor smooth
  `s(time, speaker, bs = "fs", m = 1)`. Fastest, basic random-curve
  modelling.

- `"speaker_tone"`: interaction factor smooth
  `s(time, speaker.tone, bs = "fs", m = 1)` letting each speaker-tone
  combination have its own random curve.

- `"speaker_by_tone"`: by-speaker factor smooths separately per tone.

- `"ref_diff"`: reference by-speaker smooth plus tone-difference random
  smooths, matched to `smooth_type = "difference"`.

- `"none"`: only the random-effect intercepts (if any) are included.

Refer to Sóskuthy (2021) for guidance on choosing between these.

## References

Sóskuthy, M. (2021). Evaluating generalised additive mixed modelling
strategies for dynamic speech analysis. *Journal of Phonetics*, 84,
101017.
[doi:10.1016/j.wocn.2020.101017](https://doi.org/10.1016/j.wocn.2020.101017)

Wood, S. N. (2017). *Generalized Additive Models: An Introduction with
R* (2nd ed.). Chapman and Hall/CRC.

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

- [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
  for per-tone prediction on a time grid.

- [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
  for the polynomial-based mixed-effects alternative.

## Examples

``` r
if (FALSE) { # \dontrun{
data(sample_f0)
normed <- normalise_f0(sample_f0,
                       f0      = "f0_Hz",
                       speaker = "speaker",
                       tone    = "tone")
gamm <- fit_gamm(normed,
                 f0          = "f0_st",
                 time        = "time",
                 token       = "token",
                 tone        = "tone",
                 speaker     = "speaker",
                 item        = "char",
                 k           = 10,
                 smooth_type = "separate",
                 random_smooth = "speaker",
                 use_ar1     = TRUE)
predict_gamm(gamm, n = 200)
} # }
```
