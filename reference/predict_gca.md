# Predict population-level f0 contours from a GCA fit

Builds a (time, tone) prediction grid using the polynomial basis cached
inside the
[`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
result, then predicts via lme4 with `re.form = NA` so the curves reflect
the population-average fixed effects.

## Usage

``` r
predict_gca(gca_obj, n = 100)
```

## Arguments

- gca_obj:

  An object of class `"shinytone_gca"` returned by
  [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md).

- n:

  Number of time points across `[0, 1]`. Default `100`.

## Value

A data frame with columns `time`, `f0_predicted`, `tone`.
