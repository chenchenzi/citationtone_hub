# Per-token normalised time, honouring an already-normalised column

Builds the `[0, 1]` time axis the modelling functions fit on. By default
(`time_normalised = "auto"`) the column is inspected with
[`time_already_normalised()`](https://chenchenzi.github.io/citationtone_hub/reference/time_already_normalised.md):
a column that is already proportional is used as-is (clamped to
`[0, 1]`), anything else is min-max rescaled to `[0, 1]` within each
token exactly as before.

## Usage

``` r
resolve_time_norm(
  data,
  time,
  token,
  time_normalised = c("auto", "no", "yes"),
  quiet = FALSE
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

## Value

A list with `time_norm` (numeric vector, one value per row of `data`)
and `prenormalised` (logical: was the column used as-is?).

## Details

In the rescaling path, a token whose time has zero range (a single
sample, or all-identical times) gets `0.5` for every row, and `NA` times
inside an otherwise valid token propagate as `NA` — matching the
behaviour the modelling functions have always had.

## See also

[`time_already_normalised()`](https://chenchenzi.github.io/citationtone_hub/reference/time_already_normalised.md)
for the detection rule.
