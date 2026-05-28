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
  degree = 2
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

  Column name of speaker ID, carried through to the output. Default
  `"speaker"`.

- tone:

  Column name of tone category, carried through to the output. Default
  `"tone"`.

- degree:

  Polynomial degree. One of `1`, `2`, or `3`. Default `2`.

## Value

A token-level data frame with one row per token, containing the `token`,
`speaker`, `tone`, and coefficient columns `c0`, `c1`, ..., `c{degree}`.

## Details

### What the function does internally

For each token:

1.  Compute the per-token range of `time` and rescale it linearly to the
    interval `[-1, 1]`, which is where Legendre polynomials are
    orthogonal.

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

Tokens with fewer than `degree + 1` valid samples receive `NA` for all
coefficients. Tokens with a single unique time point keep `c0` as the
mean f0 and set the higher-order coefficients to `NA`.

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
