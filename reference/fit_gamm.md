# Fit a Generalised Additive Mixed Model (GAMM) to f0 contours

Fits a Generalised Additive Mixed Model with smooth (non-polynomial)
tone-by-time interactions. GAMMs are an increasingly common alternative
to GCA for dynamic speech analysis because they capture arbitrary smooth
contour shapes without requiring the analyst to choose a polynomial
degree (Wood 2017; Sóskuthy 2021; Xu & Zhang 2024).

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
  use_ar1 = FALSE
)
```

## Arguments

- data:

  A long-format data frame with one row per f0 sample.

- f0, time, token, tone, speaker, item:

  Column names.

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

## Details

### What the function does internally

1.  Normalise time to `[0, 1]` per token.

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
    within-token correlation.

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
