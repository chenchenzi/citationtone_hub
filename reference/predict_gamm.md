# Predict population-level f0 smooth curves from a GAMM fit

Generates per-tone population-average smooth curves from a fitted
[`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
model. Useful for plotting predicted contours with confidence bands,
comparing tones at a glance, or feeding into downstream Chao numeral
summarisation via
[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md).

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

## Details

Internally:

1.  Build a per-tone time grid with `n` evenly-spaced points across
    `[0, 1]`.

2.  Set random-effect columns (speaker, item, and any random-smooth
    grouping factors) to the first level of their respective factors;
    these reference values are placeholders that don't affect the
    prediction once the corresponding terms are excluded.

3.  Identify the random-effect terms that need to be excluded so the
    prediction reflects only the population-average fixed smooths.

4.  Call [`stats::predict()`](https://rdrr.io/r/stats/predict.html) on
    the mgcv model with `exclude = <random terms>` and `se.fit = TRUE`
    to also return standard errors.

Predictions are on the scale of the f0 column used to fit the model
(typically semitones if you passed `f0 = "f0_st"` from
[`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)).

## References

Sóskuthy, M. (2021). Evaluating generalised additive mixed modelling
strategies for dynamic speech analysis. *Journal of Phonetics*, 84,
101017.
[doi:10.1016/j.wocn.2020.101017](https://doi.org/10.1016/j.wocn.2020.101017)

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

[`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
for the model fit.
[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
for converting the predicted contours to Chao numerals.
