# Fit a Generalised Additive Mixed Model (GAMM) to f0 contours

Convenience wrapper around
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) for the
citation-tone GAMM use case. Prepares the data, builds a smooth-based
formula with optional difference smooths and speaker random smooths,
fits the model, and can refit with an AR1 correlation if requested.

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

  Long-format data frame, one row per f0 sample.

- f0, time, token, tone, speaker, item:

  Column names.

- duration:

  Optional column name with a per-sample duration covariate. Use `NULL`
  to omit the duration smooth.

- k:

  Basis dimension for the time smooth. Default `10`.

- bs:

  Spline basis type passed to
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html). Default `"tp"`.

- smooth_type:

  Either `"separate"` (one smooth per tone via `s(time, by = tone)`) or
  `"difference"` (reference smooth + difference smooths via an ordered
  factor).

- random_intercept_speaker, random_intercept_item:

  Logical, include the corresponding `s(..., bs = "re")` random
  intercept.

- random_smooth:

  One of `"none"`, `"speaker"`, `"speaker_tone"`, `"speaker_by_tone"`,
  or `"ref_diff"`. See *Random smooth strategies* in details.

- use_ar1:

  Logical. If `TRUE`, fit once, estimate `rho` from the residuals' lag-1
  autocorrelation, then refit with that `rho`.

## Value

An S3 object of class `"shinytone_gamm"`, a list with:

- `model` — the fitted
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) object

- `formula_str` — user-readable formula string

- `rho` — `NULL` or the AR1 rho value

- `convergence_warning` — `NULL` or the captured warning message

- `smooth_type`, `random_smooth` — passed through (needed by
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md))

- `col_names` — original column names, for back-mapping

## Details

For non-standard model structures, use
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) /
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) directly.

Random smooth strategies (after Sóskuthy 2021):

- `"speaker"` — single by-speaker factor smooth

- `"speaker_tone"` — interaction factor smooth over speaker × tone

- `"speaker_by_tone"` — by-speaker factor smooth, separately per tone

- `"ref_diff"` — reference by-speaker smooth plus tone-difference
  smooths (matches `smooth_type = "difference"`)

## See also

[`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
for per-tone prediction on a time grid.
