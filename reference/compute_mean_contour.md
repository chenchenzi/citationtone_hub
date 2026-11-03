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
  n_points = NULL,
  time_normalised = c("auto", "no", "yes"),
  n_bins = NULL
)
```

## Arguments

- data:

  A long-format data frame with one row per f0 sample.

- token, f0, time, tone:

  Column names.

- n_points:

  Number of equally spaced time points across `[0, 1]` at which every
  token is resampled. The default, `NULL`, uses the tokens' own number
  of points when they all have the same number, so equally spaced
  measurements come back unchanged, and otherwise `21`, one point every
  5% of the duration, as in the app's f0 extraction and clustering.

- time_normalised:

  One of `"auto"` (default: detect an already-normalised `[0, 1]` time
  column and use it as-is), `"no"` (always rescale to `[0, 1]` within
  each token), or `"yes"` (declare the column already normalised). See
  [`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md).

- n_bins:

  Deprecated; use `n_points`. Samples used to be assigned to `n_bins`
  time bins; a value given here is used as `n_points`, with a warning.

## Value

A data frame with columns `tone`, `time`, `f0_predicted`, one row per
(tone, time point).

## Details

Internally:

1.  Within each token, normalise the time axis to `[0, 1]` so tokens of
    different durations can be averaged across the same grid. A time
    column that is *already* normalised is detected with
    [`time_already_normalised()`](https://chenchenzi.github.io/citationtone_hub/reference/time_already_normalised.md)
    and used as-is (override with `time_normalised`), matching what
    [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
    and
    [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
    do.

2.  Resample every token, by linear interpolation, at the same
    `n_points` equally spaced time points, so each token counts once at
    every point however many f0 samples it has. Missing f0 values are
    skipped, points outside a token's measured span are left out rather
    than extrapolated, and tokens with fewer than two f0 values are
    dropped, with a message.

3.  At each time point, take the mean of `f0` across the tokens of each
    tone.

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
#>   tone time f0_predicted
#> 1    1 0.00     202.3695
#> 2    1 0.05     197.7551
#> 3    1 0.10     196.7470
#> 4    1 0.15     196.2067
#> 5    1 0.20     195.8127
#> 6    1 0.25     195.5778
```
