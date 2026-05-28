# Predict population-level f0 smooth curves from a GAMM fit

Builds a per-tone time grid, sets random-effect columns to reference
levels, and predicts with the random-effect smooths excluded so that the
result is the population-average curve per tone.

## Usage

``` r
predict_gamm(gamm_obj, n = 200)
```

## Arguments

- gamm_obj:

  An object of class `"shinytone_gamm"` from
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md).

- n:

  Number of time points across `[0, 1]`. Default `200`.

## Value

A data frame with columns `time`, `f0_predicted`, `se`, `tone`.
