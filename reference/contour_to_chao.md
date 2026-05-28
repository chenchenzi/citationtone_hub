# Convert per-tone contours to Chao tone numerals

Summarises per-tone contours as Chao tone numerals (Chao 1930): 5-level
digit strings like `"55"`, `"35"`, or `"214"` that describe each contour
by its endpoints and any interior turning point. Three conversion
methods are computed in one pass so the analyst can compare them or pick
whichever is most appropriate for the corpus (Xu & Zhang 2024).

## Usage

``` r
contour_to_chao(
  contour_data,
  tone_col = "tone",
  time_col = "time",
  f0_col = "f0_predicted",
  threshold = 0.5,
  raw_data = NULL,
  raw_token = "token",
  raw_f0 = "f0",
  raw_tone = "tone"
)
```

## Arguments

- contour_data:

  Per-tone contour data, typically from
  [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md),
  [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md),
  or
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md).
  Must contain `tone`, `time`, and `f0_predicted` columns (or rename via
  the `tone_col`, `time_col`, `f0_col` arguments).

- tone_col, time_col, f0_col:

  Column names within `contour_data`.

- threshold:

  Chao-units threshold for turning-point detection. Default `0.5`. See
  Details.

- raw_data:

  Optional long-format raw data, used to compute the robust FOR. If
  `NULL`, the `robust` column is left as `NA`.

- raw_token, raw_f0, raw_tone:

  Column names in `raw_data`.

## Value

A data frame with one row per tone, containing:

- `tone`: tone label.

- `n_digits`: number of digits in the numeral (`2` or `3`).

- `refline`: reference-line method numeral, e.g. `"55"`.

- `interval`: interval-based numeral, e.g. `"45"`.

- `robust`: robust FOR numeral (or `NA` if `raw_data` is not given).

- `shape`: shape name from
  [`classify_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/classify_contour.md)
  applied to `refline`.

- `sample_times`, `ref_continuous`, `int_continuous`, `rob_continuous`:
  list columns of the per-sample numeric values underlying the digit
  strings, useful for plotting or further analysis.

Two attributes are attached to the returned data frame: `f0_min` and
`f0_max` of the input contour, plus a `robust_stats` list containing the
means and standard deviations used for the robust method (or `NULL` if
`raw_data` was not supplied).

## Details

### The three conversion methods

All three use Fraction of Range (FOR) normalisation, which maps f0 onto
a bounded scale representing the speaker's (or corpus's) pitch range.
They differ in how the range is defined and how scaled values are mapped
to Chao digits.

- **Reference-line FOR on `[1, 5]`**:
  `f0' = (f0 - f0_min) / (f0_max - f0_min) * 4 + 1`, then rounded. Five
  reference lines at integer values; each sampled point snaps to the
  nearest integer.

- **Interval-based FOR on `(0, 5]`**:
  `f0' = (f0 - f0_min) / (f0_max - f0_min) * 5`, then ceilinged. Five
  equal-width intervals; each sampled point is assigned to its interval
  via [`ceiling()`](https://rdrr.io/r/base/Round.html).

- **Robust FOR on `(0, 5]`** (only if `raw_data` is supplied): same
  interval formula, but the range is defined by μ ± σ of per-token f0
  peaks from the highest-pitched tone (max) and per-token f0 valleys
  from the lowest-pitched tone (min), following Zhu (1999) and the
  recommendation of Xu & Zhang (2024). More robust to outliers than
  using the corpus-wide min and max.

### Turning-point detection

For each tone, the function samples the contour either at its two
endpoints (yielding a 2-digit numeral) or at the two endpoints plus one
interior turning point (yielding a 3-digit numeral). The 3-digit case
applies when either:

- The interior minimum is more than `threshold` Chao units below both
  endpoints (a *dip*), or

- The interior maximum is more than `threshold` Chao units above both
  endpoints (a *peak*).

The default `threshold = 0.5` (half a Chao unit on the `[1, 5]` scale)
is a conservative choice. Lower thresholds produce more 3-digit
numerals, useful for tonally rich languages.

## References

Chao, Y. R. (1930). A system of tone-letters. *Le Maître Phonétique*,
30, 24–27.

Zhu, X. (1999). *Shanghai Tonetics*. LINCOM Europa.

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

- [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md)
  for one common upstream step.

- [`classify_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/classify_contour.md)
  for the shape classification helper.

- [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md)
  and
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
  for model-based contour sources.

## Examples

``` r
data(sample_f0)
mc <- compute_mean_contour(sample_f0,
                           token = "token", f0 = "f0_Hz",
                           time  = "time",  tone = "tone")
chao <- contour_to_chao(mc,
                        raw_data  = sample_f0,
                        raw_token = "token",
                        raw_f0    = "f0_Hz",
                        raw_tone  = "tone")
chao[, c("tone", "refline", "interval", "robust", "shape")]
#>   tone refline interval robust         shape
#> 1    1      22       22     33 mid-low level
#> 2    2     212      111    222       dipping
#> 3    3      32       32     33       falling
#> 4    4      45       45     44        rising
#> 5    5      22       21     32 mid-low level
#> 6    6      23       13     23        rising
```
