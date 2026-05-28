# Flag per-token f0 outliers using by-speaker z-scores

For each token (a unique value of the `token` column), compute the
maximum and minimum f0. Within each speaker, z-score those per-token
extremes. A token is flagged whenever the absolute z-score of either its
max or its min exceeds `z_threshold`.

## Usage

``` r
flag_outliers(
  data,
  f0 = "f0",
  token = "token",
  speaker = "speaker",
  z_threshold = 3
)
```

## Arguments

- data:

  A long-format data frame, one row per f0 sample.

- f0:

  Column name of f0 (Hz). Default `"f0"`.

- token:

  Column name of token ID. Default `"token"`.

- speaker:

  Column name of speaker ID. Default `"speaker"`.

- z_threshold:

  Absolute z-score above which a token is flagged. Default `3`, covering
  99.7% of a normal distribution.

## Value

A *token-level* data frame (one row per token) with columns:
`f0_token_max`, `f0_token_min`, `f0_token_mean`, `f0_token_sd`, `z_max`,
`z_min`, `flag_too_high`, `flag_too_low`, plus the original `token` and
`speaker` columns.

## Examples

``` r
df <- data.frame(
  token   = rep(c("t1", "t2", "t3"), each = 4),
  speaker = rep("S01", 12),
  f0      = c(150, 160, 155, 158,
              152, 159, 154, 157,
              400, 410, 405, 408)   # t3 is clearly a tracking error
)
flag_outliers(df, z_threshold = 1.5)
#> # A tibble: 3 × 10
#>   token f0_token_max f0_token_min f0_token_mean f0_token_sd speaker  z_max
#>   <chr>        <dbl>        <dbl>         <dbl>       <dbl> <chr>    <dbl>
#> 1 t1             160          150          156.        4.35 S01     -0.574
#> 2 t2             159          152          156.        3.11 S01     -0.581
#> 3 t3             410          400          406.        4.35 S01      1.15 
#> # ℹ 3 more variables: z_min <dbl>, flag_too_high <lgl>, flag_too_low <lgl>
```
