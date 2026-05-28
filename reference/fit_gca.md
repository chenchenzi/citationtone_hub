# Fit a Growth Curve Analysis (GCA) model to f0 contours

Fits a Growth Curve Analysis model to f0 contours: a linear
mixed-effects regression of f0 on orthogonal polynomial terms of time,
interacted with tone, with conventional by-speaker and by-item random
effects. GCA is a standard analysis framework for time-varying dynamic
speech data (Mirman 2014; Xu & Zhang 2024) and is the recommended
approach when you want statistical inference about per-tone shape
parameters such as intercept, slope, and curvature.

This is a convenience wrapper around
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) that prepares
the data the way the citation-tone use case typically wants. For
non-standard model structures (custom contrasts, additional fixed
effects, alternative random-effects structures), call
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) directly.

## Usage

``` r
fit_gca(
  data,
  f0 = "f0",
  time = "time",
  token = "token",
  tone = "tone",
  speaker = "speaker",
  item = "item",
  degree = 2,
  random_intercept_speaker = TRUE,
  random_intercept_item = TRUE,
  random_slope_speaker = TRUE,
  random_slope_item = FALSE
)
```

## Arguments

- data:

  A long-format data frame with one row per f0 sample.

- f0, time, token, tone, speaker, item:

  Column names for f0, time within token, token ID, tone category,
  speaker ID, and item or word.

- degree:

  Polynomial degree, one of `1`, `2`, or `3`. Default `2`.

- random_intercept_speaker, random_intercept_item:

  Logical; include a by-speaker or by-item random intercept. Default
  `TRUE`.

- random_slope_speaker, random_slope_item:

  Logical; include by-speaker or by-item random slopes on the
  orthogonal-polynomial terms (`ot1` ... `otK`). Default `TRUE` for
  speaker, `FALSE` for item.

## Value

An S3 object of class `"shinytone_gca"`, a list with:

- `model`: the fitted
  [lme4::lmerMod](https://rdrr.io/pkg/lme4/man/merMod-class.html)
  object.

- `formula_str`: user-readable formula string (with original column
  names).

- `poly_coefs`: coefs from
  [`stats::poly()`](https://rdrr.io/r/stats/poly.html) needed by
  [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md).

- `degree`: polynomial degree.

- `col_names`: original column names (for back-mapping in summary
  tables).

- `convergence_warning`: `NULL` or the captured warning message.

## Details

### What the function does internally

1.  Within each token, normalise the time axis to `[0, 1]`.

2.  Build orthogonal polynomial terms `ot1`, `ot2`, ..., `otK` on the
    normalised time using
    [`stats::poly()`](https://rdrr.io/r/stats/poly.html), caching the
    basis coefficients so that
    [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md)
    can reuse them.

3.  Construct an `lmer` formula of the form
    `f0 ~ (ot1 + ... + otK) * tone + (random effects)` using the options
    below to decide which random effects to include.

4.  Fit the model with REML, capturing any convergence warning so the
    caller can decide whether to act on it.

5.  Return everything bundled as an S3 object of class
    `"shinytone_gca"`, so downstream helpers
    ([`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md),
    [`summary()`](https://rdrr.io/r/base/summary.html), etc.) have what
    they need.

### Choosing the polynomial degree

- `degree = 1`: linear (intercept + slope). Sufficient for purely rising
  or falling contours.

- `degree = 2` (default): linear + quadratic. Captures
  rising/falling/dipping/peaking shapes. The usual choice.

- `degree = 3`: adds a cubic term for sigmoid or S-shaped contours.
  Useful for languages with more elaborate contour shapes.

Higher degrees increase model complexity and the risk of over-fitting;
they also slow the fit and can degrade convergence.

### Choosing the random-effects structure

The defaults follow conventional GCA practice for citation-tone work:
by-speaker random intercepts and slopes on the polynomial terms, plus a
by-item random intercept. Add by-item slopes if items vary
systematically in shape, or drop random slopes when convergence is
difficult.

## References

Mirman, D. (2014). *Growth Curve Analysis and Visualization Using R*.
Chapman and Hall/CRC.

Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear
mixed-effects models using lme4. *Journal of Statistical Software*,
67(1), 1–48.
[doi:10.18637/jss.v067.i01](https://doi.org/10.18637/jss.v067.i01)

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

- [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md)
  for prediction on a time grid.

- [`fit_polynomial()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_polynomial.md)
  for a per-token (non-mixed-effects) alternative.

- [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
  for the GAMM-based alternative when smooth (non-polynomial) tone
  shapes are preferred.

## Examples

``` r
if (FALSE) { # \dontrun{
data(sample_f0)
normed <- normalise_f0(sample_f0,
                       f0      = "f0_Hz",
                       speaker = "speaker",
                       tone    = "tone")
gca <- fit_gca(normed,
               f0      = "f0_st",
               time    = "time",
               token   = "token",
               tone    = "tone",
               speaker = "speaker",
               item    = "char",
               degree  = 2,
               random_slope_speaker = FALSE,
               random_slope_item    = FALSE)
predict_gca(gca, n = 100)
} # }
```
