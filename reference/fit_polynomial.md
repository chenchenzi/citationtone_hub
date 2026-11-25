# Fit Legendre polynomials to f0 contours, token by token

For each token, normalises the time axis to `[-1, 1]`, constructs a
Legendre polynomial basis of the requested degree, and fits ordinary
least squares. Returns one row per token with the fitted coefficients
`c0`, `c1`, ..., `c{degree}`. Useful as a compact, interpretable summary
of contour shape, or as features for downstream classifiers and
regression models (Mirman 2014; Xu & Zhang 2024).

## Usage

``` r
fit_polynomial(
  data,
  f0 = "f0",
  token = "token",
  time = "time",
  speaker = "speaker",
  tone = "tone",
  degree = 2,
  time_normalised = c("auto", "no", "yes")
)
```

## Arguments

- data:

  A long-format data frame with one row per f0 sample.

- f0:

  Column name of f0 values. Default `"f0"`. Using normalised f0 (e.g.
  `"f0_st"` from
  [`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md))
  is recommended for cross-speaker comparison.

- token:

  Column name of token ID. Default `"token"`.

- time:

  Column name of time within each token. Default `"time"`.

- speaker:

  Column name of speaker ID, carried through to the output (see "Speaker
  and tone" below). Default `"speaker"`.

- tone:

  Column name of tone category, carried through to the output (see
  "Speaker and tone" below). Default `"tone"`.

- degree:

  Polynomial degree. One of `1`, `2`, or `3`. Default `2`.

- time_normalised:

  One of `"auto"` (default: detect an already-normalised `[0, 1]` time
  column and map it to `[-1, 1]` with the fixed transform `2 * t - 1`),
  `"no"` (always rescale to `[-1, 1]` within each token), or `"yes"`
  (declare the column already normalised to `[0, 1]`). See
  [`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md).

## Value

A token-level data frame with one row per token, containing the `token`,
`speaker`, `tone`, and coefficient columns `c0`, `c1`, ..., `c{degree}`.
The attribute `time_prenormalised` records whether the time column was
treated as already normalised.

## Details

### What the function does internally

For each token:

1.  Rescale each token's `time` linearly to `[-1, 1]`, from its first f0
    (-1) to its last f0 (1), which is where Legendre polynomials are
    orthogonal. Rows without f0 at a token's edges do not move it. If
    the time column is *already* normalised to `[0, 1]` (detected with
    [`time_already_normalised()`](https://chenchenzi.github.io/citationtone_hub/reference/time_already_normalised.md),
    or declared via `time_normalised`), the fixed map `2 * t - 1` is
    used instead, so tokens whose samples do not span the full unit
    interval are not stretched a second time. A `<tier>_tseq` (syllables
    in sequence) or `<tier>_tbound` (in sequence, gap kept) is rescaled
    per token too: a token whose first or last syllable has no values on
    the axis (on a `<tier>_tseq`, f0 at fewer than two times; on a
    `<tier>_tbound`, no f0) is stretched, and its other syllables fill
    the axis.

2.  Build the Legendre basis up to the requested `degree`.

3.  Solve the ordinary least-squares problem with
    [`stats::lm.fit()`](https://rdrr.io/r/stats/lmfit.html) to recover
    the coefficients.

### Interpreting the coefficients

Because Legendre polynomials are orthogonal on `[-1, 1]`, the fitted
coefficients have a tidy shape interpretation:

- `c0`: token-level mean f0.

- `c1`: linear slope across the contour. Positive means rising.

- `c2`: quadratic curvature. Positive means U-shaped (dipping), negative
  means inverted-U (peaking).

- `c3`: cubic shape. Differentiates sigmoid from reverse-sigmoid
  contours.

### Edge cases

Samples with a missing or infinite f0 or time are skipped (`-Inf` comes
from taking the log of 0 Hz), and so are 0 values in a column in Hz (one
that
[`hz_f0_columns()`](https://chenchenzi.github.io/citationtone_hub/reference/hz_f0_columns.md)
finds), which have no f0. The checks below count only the samples that
remain, so rows without f0 do not change them. A token whose samples all
share one time point keeps `c0` as the mean of their f0 and sets the
higher-order coefficients to `NA`. Any other token with fewer than
`degree + 1` samples gets `NA` for all coefficients. So does a token
with no samples.

### Speaker and tone

`speaker` and `tone` are read once per token. Each token takes the most
frequent non-empty value among its rows with f0 (a finite f0, and above
0 in a column in Hz, as in "Edge cases"). A tie goes to the value that
comes first in time. So a label column that changes within a token, such
as a tone tier from a TextGrid with `""` in the silences, gives the same
result in any row order, with or without rows without f0.

## References

Mirman, D. (2014). *Growth Curve Analysis and Visualization Using R*.
Chapman and Hall/CRC.

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

- [`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)
  for the upstream normalisation step.

- [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
  for a population-level mixed-effects extension of polynomial contour
  modelling.

## Examples

``` r
data(sample_f0)
normed <- normalise_f0(sample_f0,
                       f0      = "f0_Hz",
                       speaker = "speaker",
                       tone    = "tone")
coefs <- fit_polynomial(normed,
                        f0      = "f0_st",
                        token   = "token",
                        time    = "time",
                        speaker = "speaker",
                        tone    = "tone",
                        degree  = 2)
head(coefs)
#> # A tibble: 6 × 6
#>   token            speaker  tone    c0    c1    c2
#>   <chr>            <chr>   <int> <dbl> <dbl> <dbl>
#> 1 dc102一1s22.3525 dc102       6 -1.53  3.86 0.114
#> 2 dc102一2s22.9425 dc102       6 -1.07  2.62 0.282
#> 3 dc102一3s23.5725 dc102       6 -1.37  2.79 0.474
#> 4 dc102一4s24.1425 dc102       6 -1.14  1.76 0.732
#> 5 dc102一5s24.7925 dc102       6 -1.09  1.90 0.296
#> 6 dc102一6s25.3125 dc102       6 -1.87  2.14 1.28 
```
