# Compute the per-tone mean f0 contour from long-format data

Aggregates a long-format f0 dataset into a per-tone mean contour
suitable for plotting, comparison, or Chao numeral summarisation via
[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md).
The function is used internally by the Summarise tab of the Shiny app
when the user chooses the raw or normalised dataset as the input.

## Usage

``` r
compute_mean_contour(
  data,
  token = "token",
  f0 = "f0",
  time = "time",
  tone = "tone",
  n_bins = 50
)
```

## Arguments

- data:

  A long-format data frame with one row per f0 sample.

- token, f0, time, tone:

  Column names.

- n_bins:

  Number of evenly-spaced time bins across `[0, 1]`. Default `50`.
  Larger values give a smoother contour at the cost of noisier per-bin
  estimates if some bins are sparsely populated.

## Value

A data frame with columns `tone`, `time`, `f0_predicted`, one row per
(tone, time-bin).

## Details

Internally:

1.  Within each token, normalise the time axis to `[0, 1]` so tokens of
    different durations can be averaged across the same grid.

2.  Round each sample's normalised time onto one of `n_bins`
    equally-spaced bins.

3.  For each (tone, time-bin) cell, compute the mean of `f0` across all
    tokens of that tone with samples in the bin.

4.  Return the long-format mean contour with columns `tone`, `time`,
    `f0_predicted` (the column name `f0_predicted` matches the
    convention used by
    [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md)
    and
    [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
    so the same downstream tools can consume either model predictions or
    raw mean contours).

## References

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
for converting mean contours to Chao numerals.
[`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md)
and
[`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
for model-based alternatives that produce the same column structure.

## Examples

``` r
data(sample_f0)
mc <- compute_mean_contour(sample_f0,
                           token = "token", f0 = "f0_Hz",
                           time  = "time",  tone = "tone")
head(mc)
#>   tone       time f0_predicted
#> 1    1 0.00000000     202.3695
#> 2    1 0.04081633     197.7551
#> 3    1 0.10204082     196.7470
#> 4    1 0.14285714     196.2067
#> 5    1 0.20408163     195.8127
#> 6    1 0.24489796     195.5778
```
