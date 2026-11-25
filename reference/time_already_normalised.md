# Does a time column look already normalised to \[0, 1\]?

Heuristic used by the modelling functions (via
[`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md))
to decide whether a time column is already proportional (per-token
normalised to the unit interval) and should be used as-is rather than
rescaled within each token again.

## Usage

``` r
time_already_normalised(time_values, tokens, eps = 1e-08, name = NULL)
```

## Arguments

- time_values:

  Numeric vector of time values.

- tokens:

  Vector of token IDs, same length as `time_values`.

- eps:

  Numeric slop allowed beyond the `[0, 1]` bounds. Default `1e-8`.

- name:

  The time column's name, or `NULL` (the default) to decide by value
  alone. See "By name" in Details.

## Value

`TRUE` if the column looks already normalised, else `FALSE`.

## Details

### By name

Three column names mark a 0-1 time axis: `<tier>_t01` from
[`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md),
`token_t01` from
[`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md)
and `time_prop` from
[`resample_f0_equal()`](https://chenchenzi.github.io/citationtone_hub/reference/resample_f0_equal.md).
When `name` is one of these, the column counts as already normalised as
soon as every finite value lies in `[0, 1]` (within `eps`). Its tokens
do not need to cover the interval. The first two run from the first f0
to the last and are `NA` on the other rows, so removing rows never
changes them. The name matters for `time_prop`, which runs over the
frames of the extracted region, for columns made by older versions,
which ran from the first row or interval boundary to the last, and for a
subset taken after a column was made.

A sequential landmark axis (`<tier>_tseq` or `<tier>_tbound`) is not
recognised by its name. It runs from 0 to the number of segments, so it
goes through the value test below, as before.

### By value

Any other column, or a call without `name`, counts as already normalised
only when all three hold:

1.  Every finite value lies in `[0, 1]` (within `eps`).

2.  The pooled values actually use the unit scale: pooled minimum at or
    below 0.05 and pooled maximum at or above 0.95.

3.  Tokens individually cover the interval: the median per-token span
    (`max - min`) is at least 0.9. The span runs over every row with a
    finite time, rows without f0 included, so for a column without such
    a name, removing rows at a token's edges can change the decision.
    The app's Start page says so when removal would change it for its
    time column.

Condition 3 is the load-bearing one: raw time in *seconds* for citation
tones (durations of, say, 0.2 to 0.9 s) can satisfy the first two, but
its per-token spans equal the (variable, well under 0.9) durations, so
it is correctly treated as unnormalised. Millisecond-scale time fails
condition 1 outright, as do sequential landmark axes (`<tier>_tseq` and
`<tier>_tbound`, which run from 0 to the number of segments).

Two limits of the value test are worth knowing, both following from the
fact that no value-based test can separate these cases:

- **Not detected**: a set in which *every* token is a partial span of
  the proportional axis (e.g. a vowel-only subset where each token
  covers `[0.3, 0.7]`) looks exactly like ordinary variable-duration
  time, so it is rescaled per token as before, unless its name marks it
  (see "By name"). Pass `time_normalised = "yes"` to
  [`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md)
  (or to the fitters) to keep such an axis intact.

- **Detected**: raw seconds in which every token happens to last just
  under one second, with a median duration of 0.9 s or more, satisfies
  all three conditions and is used as-is. Pass `time_normalised = "no"`
  to force the per-token rescale for such data.

## See also

[`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md),
which applies this decision.
