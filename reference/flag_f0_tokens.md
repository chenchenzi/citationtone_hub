# Flag tokens with too few samples with f0, or with points missing

Adds a column, `screen_flag`, that says in a few words what is wrong
with each token: `""` (empty) for a token with no issue, otherwise its
reasons, such as `"only 2 samples with f0"`, the same text on every row
of the token. No row is removed: keep the tokens with no issue with
`data[data$screen_flag == "", ]` if wanted. This is what the app's Start
page does before its *Drop the N flagged tokens* box.

## Usage

``` r
flag_f0_tokens(
  data,
  token = "token",
  time = "time",
  f0 = "f0",
  gap_ratio = 1.5,
  run = "voiced_run",
  min_f0 = 3,
  min_run = 1,
  flag = "screen_flag"
)
```

## Arguments

- data:

  A data frame in long format: one row per f0 sample.

- token, time, f0:

  Column names. Defaults `"token"`, `"time"`, `"f0"`. `time` must be the
  real time column. `f0` is the one column that decides which rows have
  f0.

- gap_ratio:

  A time step longer than this many times the token's usual step is a
  gap. Default 1.5. Must be above 1. `Inf` reads gaps from rows without
  f0 only.

- run:

  Name of a column that marks gaps already, such as the `voiced_run`
  column
  [`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
  adds. Default `"voiced_run"`. Used only when the column is present.
  `NULL` ignores it.

- min_f0:

  A token with fewer rows with f0 than this is flagged (reason 2).
  Default `3`. Must be at least 1.

- min_run:

  Samples a run at the start or end of a token must have to be counted,
  as in
  [`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md).
  Default `1` counts every row with f0.

- flag:

  Name of the column to add. Default `"screen_flag"`. A column of that
  name already in `data` is replaced.

## Value

`data` with the `flag` column, a character column, added or replaced, of
the same class (a data frame or a tibble), with the rows in their
original order and every other column as it was. Attribute `flagged` is
a data frame with one row per flagged token, in order of first
appearance in `data`: the token, in a column named as the `token`
column, and `reason`, the text in the `flag` column. It has no rows when
no token is flagged.

## Details

**Reasons**, in this order, joined by `"; "`:

1.  `"no f0"`: no row of the token has f0. No other reason is given.

2.  `"only 2 samples with f0"` or `"only 1 sample with f0"`: fewer rows
    with f0 than `min_f0` (with the default 3, 1 or 2 rows). A contour
    needs at least 3 points to show a turn. No other reason is given
    either, as such a token cannot be used whatever else is missing.
    With `min_run` above 1, the rows that
    [`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
    removes as short edge runs with the same `min_run` are not counted.

3.  Equal-point data only: `"no f0 at the first point"`,
    `"no f0 at the last point"` or
    `"no f0 at the first and last points"`.

4.  Equal-point data only: `"3 points without f0 inside"` or
    `"1 point without f0 inside"`: every row without f0 between the
    token's first and last samples with f0, whatever the length of the
    gap.

**Equal-point data.** When there are at least two tokens and all have
the same number of rows, as for 21 equidistant points from resampling,
F0 Extraction or a Praat script (`equal_rows` in
[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)),
each token should have f0 at every point, so reasons 3 and 4 apply too,
and the reasons say "points". In frame-by-frame data (tokens of
different lengths), rows without f0 at a token's start or end or inside
it are normal, and
[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
removes them. So only reasons 1 and 2 apply, and the reasons say
"samples".

**The data as given.** The reasons are read from `data` as it is. A row
has no f0 when its value in the `f0` column is `NA`, `NaN` or infinite;
0 and negative values count as f0 here. So run
[`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md)
first, and set values such as -1 that a tracker writes for unvoiced
frames to `NA`. Rows are taken in time order within each token, whatever
their order in `data`. A row with no time (`NA`) counts in reason 2 when
it has f0, but cannot be placed for reasons 3 and 4. A row whose token
is `NA` belongs to a token of its own.

**`min_run`.** It changes reason 2 only when it is below `min_f0`: a
token with a run of at least `min_run` samples keeps that run, and a
token with no such run keeps all its samples. So with `min_run = 3` and
the default `min_f0 = 3`, the flags are the same as with `min_run = 1`.

## See also

[`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md)
to run first;
[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
for the counts;
[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
to remove the rows without f0.

## Examples

``` r
# 7 equidistant points per token
df <- data.frame(
  token = rep(c("a", "b", "c", "d"), each = 7),
  time  = rep(seq(0, 0.3, length.out = 7), 4),
  f0    = c(200, 205, 210, 215, 220, 225, 230,
            NA, 181, 183, NA, NA, NA, 190,
            NA, NA, 150, NA, NA, 154, NA,
            rep(NA, 7)))
# b misses its first point and 3 inside; c has only 2 points with f0,
# which is its only reason; d has no f0
out <- flag_f0_tokens(df)
unique(out[c("token", "screen_flag")])
#>    token                                          screen_flag
#> 1      a                                                     
#> 8      b no f0 at the first point; 3 points without f0 inside
#> 15     c                                only 2 points with f0
#> 22     d                                                no f0
attr(out, "flagged")
#>   token                                               reason
#> 1     b no f0 at the first point; 3 points without f0 inside
#> 2     c                                only 2 points with f0
#> 3     d                                                no f0
# Keep the tokens with no issue
out[out$screen_flag == "", ]
#>   token time  f0 screen_flag
#> 1     a 0.00 200            
#> 2     a 0.05 205            
#> 3     a 0.10 210            
#> 4     a 0.15 215            
#> 5     a 0.20 220            
#> 6     a 0.25 225            
#> 7     a 0.30 230            

# Frame-by-frame data: only too few samples with f0 count
fr <- data.frame(token = rep(c("x", "y"), c(8, 5)),
                 time  = c(1:8, 1:5) / 100,
                 f0    = c(NA, 200, 202, 204, NA, 206, 208, NA,
                           NA, 150, NA, 152, NA))
attr(flag_f0_tokens(fr), "flagged")
#>   token                 reason
#> 1     y only 2 samples with f0
```
