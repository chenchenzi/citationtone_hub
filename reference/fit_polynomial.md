# Fit Legendre polynomials to f0 contours, token by token

For each token (a unique value of the `token` column), normalise the
time axis to `[-1, 1]`, construct a Legendre polynomial basis of the
requested degree, and fit ordinary least squares. Returns one row per
token with the fitted coefficients `c0`, `c1`, ..., `c{degree}`.

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

  Long-format data frame with one row per f0 sample.

- f0:

  Column name of f0 values. Default `"f0"`. Normalised f0 (`"f0_st"`,
  `"f0_zscore"`) is recommended for cross-speaker comparison.

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

A token-level data frame with one row per token, containing: `token`,
`speaker`, `tone`, and the coefficients `c0`, `c1`, ..., `c{degree}`.

## Details

Legendre polynomials are orthogonal on `[-1, 1]`, so the fitted
coefficients have a tidy shape interpretation:

- `c0` — token-level mean f0

- `c1` — linear slope across the contour

- `c2` — quadratic curvature (rise-fall vs fall-rise)

- `c3` — cubic shape (sigmoid vs reverse-sigmoid)

Tokens with fewer than `degree + 1` valid samples receive `NA` for all
coefficients. Tokens with a single unique time point keep `c0` as the
mean f0 and set the higher-order coefficients to `NA`.

## Examples

``` r
df <- data.frame(
  token   = rep(c("t1", "t2"), each = 5),
  time    = rep(seq(0, 1, length.out = 5), 2),
  f0      = c(150, 160, 170, 165, 155,
              140, 145, 150, 145, 140),
  speaker = rep("S01", 10),
  tone    = rep(c("T1", "T2"), each = 5)
)
fit_polynomial(df, degree = 2)
#> # A tibble: 2 × 6
#>   token speaker tone     c0       c1     c2
#>   <chr> <chr>   <chr> <dbl>    <dbl>  <dbl>
#> 1 t1    S01     T1     163. 3.00e+ 0 -10.5 
#> 2 t2    S01     T2     145. 9.15e-15  -5.71
```
