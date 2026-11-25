# Per-token normalised time, honouring an already-normalised column

Builds the `[0, 1]` time axis the modelling functions fit on. By default
(`time_normalised = "auto"`) the column and its name are passed to
[`time_already_normalised()`](https://chenchenzi.github.io/citationtone_hub/reference/time_already_normalised.md):
a column that is already proportional is used as-is (clamped to
`[0, 1]`); anything else is rescaled to `[0, 1]` within each token, from
its first f0 to its last when `f0` is given.

## Usage

``` r
resolve_time_norm(
  data,
  time,
  token,
  time_normalised = c("auto", "no", "yes"),
  quiet = FALSE,
  f0 = NULL
)
```

## Arguments

- data:

  A data frame containing the `time` and `token` columns.

- time:

  Name of the time column.

- token:

  Name of the token-ID column.

- time_normalised:

  One of `"auto"` (default; detect and use an already-normalised column
  as-is), `"no"` (always rescale per token), or `"yes"` (declare the
  column already normalised to `[0, 1]`; values outside that interval
  are an error).

- quiet:

  Suppress the message emitted when auto-detection decides the column is
  already normalised. Default `FALSE`.

- f0:

  Name of the f0 column, or `NULL` (the default). With `f0`, the rescale
  runs from each token's first f0 to its last; without it, from each
  token's first row to its last.

## Value

A list with `time_norm` (numeric vector, one value per row of `data`)
and `prenormalised` (logical: was the column used as-is?). A missing
`time`, `token` or `f0` column is an error.

## Details

In the rescaling path with `f0`, each token runs from its first f0 (0)
to its last f0 (1), as in
[`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md).
Rows have f0 as there: a row has f0 when its `f0` value is a number, and
in a column in Hz (one that
[`hz_f0_columns()`](https://chenchenzi.github.io/citationtone_hub/reference/hz_f0_columns.md)
finds) 0 is no f0. A column with negative markers, such as -1, is not
read as Hz, so those values count as f0: set them to `NA` first (see
[`suggest_unvoiced()`](https://chenchenzi.github.io/citationtone_hub/reference/suggest_unvoiced.md)).
Rows before the first f0 or after the last get `NA`, so rows without f0
at a token's edges do not move the axis, and removing them changes
nothing. A token whose f0 is all at one time gets `0.5` on the rows at
that time, and a token with no f0 gets `NA`. Rows with an `NA` token
form one token of their own, as in the rescale without `f0`.
[`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md),
[`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md),
[`fit_polynomial()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_polynomial.md)
and
[`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md)
pass their f0 column.

Without `f0`, each token runs from its first row with a time to its
last, rows without f0 included. A token whose times are all the same
gets `0.5` on every row, rows with no time included. Other `NA` times
stay `NA`. On data with no rows without f0 at the token edges, the two
rescales are the same.

In `"auto"` mode, a `<tier>_t01`, `token_t01` or `time_prop` column
whose values all lie in `[0, 1]` is used as it is, whatever rows it has
left, so removing rows does not switch it to the rescale. Columns made
by
[`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md)
and
[`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)
already run from the first f0 to the last. On `time_prop`, a column made
by an older version or a subset taken after a column was made, the f0
may not reach 0 or 1. A `<tier>_tseq` or `<tier>_tbound` column is
judged by its values only, so one that spans several segments is
rescaled per token here; for tokens with the same number of segments the
boundaries then sit at 1/n, 2/n and so on. A token whose first or last
segment has no values on the axis is stretched: its other segments fill
0 to 1. On a `<tier>_tseq` that is a segment with f0 at fewer than two
times; on a `<tier>_tbound`, one with no f0.
([`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
keeps a `<tier>_tseq` or `<tier>_tbound` on its own scale instead.)

## See also

[`time_already_normalised()`](https://chenchenzi.github.io/citationtone_hub/reference/time_already_normalised.md)
for the detection rule.
