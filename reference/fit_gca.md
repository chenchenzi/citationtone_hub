# Fit a Growth Curve Analysis (GCA) model to f0 contours

Convenience wrapper around
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) for the
citation-tone GCA use case. Prepares the data (per-token time
normalisation, orthogonal polynomial basis of the requested degree),
builds a conventional formula
`(ot1 + ... + otK) * tone + (random effects)`, and fits the model with
warning capture.

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

  Long-format data frame, one row per f0 sample.

- f0, time, token, tone, speaker, item:

  Column names for f0, time within token, token ID, tone category,
  speaker ID, and item / word.

- degree:

  Polynomial degree (1, 2, or 3). Default `2`.

- random_intercept_speaker, random_intercept_item:

  Logical, include a by-speaker / by-item random intercept. Default
  `TRUE`.

- random_slope_speaker, random_slope_item:

  Logical, include by-speaker / by-item random slopes on the
  orthogonal-polynomial terms (`ot1` .. `otK`). Default `TRUE` for
  speaker, `FALSE` for item.

## Value

An S3 object of class `"shinytone_gca"`, which is a list with:

- `model` — the fitted
  [lme4::lmerMod](https://rdrr.io/pkg/lme4/man/merMod-class.html) object

- `formula_str` — user-readable formula string (with original column
  names)

- `poly_coefs` — coefs from
  [`stats::poly()`](https://rdrr.io/r/stats/poly.html) needed by
  [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md)

- `degree` — polynomial degree

- `col_names` — original column names (for back-mapping in summary
  tables)

- `convergence_warning` — `NULL` or the captured warning message

## Details

For non-standard model structures (custom contrasts, additional fixed
effects, alternative RE structures), use
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) directly.

## See also

[`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md)
for prediction on a time grid.
