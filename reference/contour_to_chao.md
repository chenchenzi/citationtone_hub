# Convert per-tone contours to Chao tone numerals

For each tone in `contour_data`, sample the contour at its turning
points (or at the endpoints if no turning point is detected), then map
each sample to a Chao digit (1-5) using up to three methods:

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

  Chao-units threshold for turning-point detection. Default `0.5`.

- raw_data:

  Optional long-format raw data, used to compute the robust FOR. If
  `NULL`, the `robust` column is left as `NA`.

- raw_token, raw_f0, raw_tone:

  Column names in `raw_data`.

## Value

A data frame with one row per tone, containing:

- `tone` — tone label

- `n_digits` — number of digits in the numeral (2 or 3)

- `refline` — reference-line method numeral, e.g. `"55"`

- `interval` — interval-based numeral, e.g. `"45"`

- `robust` — robust FOR numeral (or `NA` if `raw_data` not given)

- `shape` — shape name from
  [`classify_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/classify_contour.md)
  applied to `refline`

- `sample_times`, `ref_continuous`, `int_continuous`, `rob_continuous` —
  list columns of the per-sample numeric values

## Details

- **Reference-line FOR `[1, 5]`** —
  `f0' = (f0 - f0_min) / (f0_max - f0_min) * 4 + 1`, rounded.

- **Interval-based FOR `(0, 5]`** —
  `f0' = (f0 - f0_min) / (f0_max - f0_min) * 5`, ceiling.

- **Robust FOR `(0, 5]`** (optional) — same formula but with the range
  defined by `mu ± sigma` of per-token f0 peaks from the highest-pitched
  tone (max) and per-token f0 valleys from the lowest-pitched tone
  (min). Computed only if `raw_data` is supplied.

The turning-point detection uses a Chao-units `threshold` (default
`0.5`) on the `[1, 5]` reference scale: a tone gets 3 digits if either a
dip or peak exceeds the threshold relative to its endpoints, otherwise 2
digits.
