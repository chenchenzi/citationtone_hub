# Predict population-level f0 contours from a GCA fit

Generates population-average per-tone f0 curves from a fitted
[`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
model. Useful for plotting predicted contours, comparing tones at a
glance, or feeding into downstream Chao numeral summarisation via
[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md).

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

## Details

Internally:

1.  Build a `(time, tone)` grid with `n` evenly-spaced points across
    `[0, 1]` for every tone level the model knows about.

2.  Re-compute the orthogonal polynomial basis on that grid using the
    cached coefficients from
    [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
    (this ensures the basis matches what the model was fit with, not a
    fresh one).

3.  Call [`stats::predict()`](https://rdrr.io/r/stats/predict.html) on
    the lme4 model with `re.form = NA`, so only fixed effects contribute
    (random effects are integrated out to the population mean).

Note that the returned predictions are on the scale of the f0 column
used to fit the model (typically semitones, if you passed `f0 = "f0_st"`
from
[`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)).

## References

Mirman, D. (2014). *Growth Curve Analysis and Visualization Using R*.
Chapman and Hall/CRC.

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

[`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
for the model fit.
[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
for converting the predicted contours to Chao numerals.
