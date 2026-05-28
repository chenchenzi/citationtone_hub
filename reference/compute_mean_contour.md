# Compute the per-tone mean f0 contour from long-format data

Normalise time to `[0, 1]` per token, bin into 50 equally-spaced time
points, then average f0 across all tokens of each tone. Returns the
long-format mean contour expected by
[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md).

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

  Long-format data frame, one row per f0 sample.

- token, f0, time, tone:

  Column names.

- n_bins:

  Number of evenly-spaced time bins across `[0, 1]`. Default `50`.

## Value

A data frame with columns `tone`, `time`, `f0_predicted`, one row per
(tone, time-bin).
