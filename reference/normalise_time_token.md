# Whole-token 0-1 time normalisation

Rescales each token's time to the interval 0-1, treating the whole token
as a single segment. No landmarks are needed, so it works for
monosyllabic data, or to put many tokens on one common 0-1 axis. Adds a
single column, `token_t01`.

## Usage

``` r
normalise_time_token(df, time, token)
```

## Arguments

- df:

  Data frame with the `time` and `token` columns.

- time:

  Name of the raw time column (seconds).

- token:

  Name of the token-ID column (rows are grouped by it).

## Value

`df` with `token_t01` appended. Returned unchanged when the required
columns are absent.
