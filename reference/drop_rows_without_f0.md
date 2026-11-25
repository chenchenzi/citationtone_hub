# Remove rows without f0 and mark the gaps they leave

Removes the rows whose f0 is missing, and adds a column, `voiced_run`,
that numbers the stretches with f0 in each token: 1 up to the token's
first gap, 2 after it, and so on. The time column still shows each gap
as a longer step, and `voiced_run` keeps it whatever is done to the rows
later. This is what the app's Start page does when "Remove rows without
f0" is ticked.
[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
gives the counts first.

## Usage

``` r
drop_rows_without_f0(
  data,
  token = "token",
  time = "time",
  f0 = "f0",
  run = "voiced_run",
  gap_ratio = 1.5,
  min_run = 1,
  unvoiced = NULL
)
```

## Arguments

- data:

  A data frame in long format: one row per f0 sample.

- token, time, f0:

  Column names. Defaults `"token"`, `"time"`, `"f0"`. `time` must be the
  real time column. `f0` is the one column that decides which rows have
  f0.

- run:

  Name of the column to add. Default `"voiced_run"`. When the column is
  there already, its gaps are kept (see Details). `NULL` adds no column
  and reads none.

- gap_ratio:

  A time step longer than this many times the token's usual step is a
  gap. Default 1.5. Must be above 1. `Inf` reads gaps from rows without
  f0 only.

- min_run:

  Samples a run at the start or end of a token must have to be kept.
  Default `1` keeps all samples with f0. `3` also removes runs of 1 or 2
  samples cut off by a gap (see Details).

- unvoiced:

  Values in the `f0` column to read as no f0, such as `-1` or
  `c(0, -1)`, for a column in Hz whose tracker marks unvoiced frames
  this way. Default `NULL`: only `NA` counts as no f0.

## Value

`data` without the rows without f0, of the same class (a data frame or a
tibble), with the rows in their original order and all columns kept. The
`run` column, an integer, is added as described in Details. Attribute
`rows_without_f0` is a list:

- `f0` and `unvoiced`: the column screened and the values read as no f0.

- `rows`: rows in `data`.

- `removed`: rows removed in all.

- `start`, `end`, `inside`: rows without f0 removed at the start, at the
  end, and inside tokens. `no_f0`: rows removed with the tokens with no
  f0. `no_time`: rows without f0 that cannot be placed, as they or the
  token's samples with f0 have no time.

- `short_runs`: samples with f0 removed by `min_run`.

- `tokens_lost`: the tokens removed entirely, as text.

- `gaps_rows`: gaps in the output from rows that were removed.

- `gaps_time_only`: gaps in the output found only from the time steps,
  as in the summary of
  [`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md).

- `gaps_run`: gaps in the output that only the existing `run` column
  marked.

- `grid`: the time-step details, as in
  [`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md).

## Details

**Which rows go.** Run
[`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md)
first, so that 0 Hz counts as no f0 in Hz columns. Then the values in
`unvoiced` are read as `NA` in the `f0` column, and every row whose f0
is `NA` (or `NaN` or infinite) is removed. Only the `f0` column decides.
0 and negative values count as no f0 only through `unvoiced`, because in
a signed or normalised column they are real values. A token with no f0
at all is removed entirely. A row with f0 but no time (`NA`) is kept,
with `voiced_run` `NA`. On a regular grid its neighbours are then two
steps apart, which reads as a gap, so with `min_run = 3` a short run
next to it at the token's start or end can go. A row whose token is `NA`
belongs to a token of its own.

**Short edge runs.** With `min_run = 3`, runs of 1 or 2 samples that a
gap cuts off at the start or end of a token go too: the samples before
the token's first run of at least `min_run` samples, and after its last
one. Runs are split at every gap, as in the run column below. A token
with no run that long keeps all its samples. The default `min_run = 1`
removes no samples with f0. These samples have f0, so removing them
moves the token's first or last f0, and with it the 0-1 time axes
(`token_t01`, `<tier>_t01` and the models' per-token rescale). Removing
rows without f0 alone never moves those axes. Only runs at the token's
start and end go: a short run next to a gap inside the token stays.

**The run column.** A gap is one of these:

- one or more rows without f0 between two kept samples;

- when the time steps form a regular grid, a step more than `gap_ratio`
  times the token's usual step, in a token that is not set aside (see
  [`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md));

- a change in the value of an existing `run` column, such as the
  `voiced_run` of an earlier removal.

So data whose rows without f0 were removed beforehand get the same
`voiced_run` as data with them kept, as long as the grid is regular and
the token's usual step is one frame. And running this again on its own
output, after more rows have lost their f0, keeps the gaps the first run
marked, on any grid.

The column is added when rows are removed, or when the gaps make some
token's runs go above 1. When neither happens, `data` is returned as it
is, and a `voiced_run` column already there is left alone. Otherwise a
column of that name is replaced, with a message, by one that still marks
its gaps. On a grid that is not regular, the time steps cannot show the
gaps, so run this on data that still has its rows without f0 or its
`voiced_run` column.

[`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md),
[`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md)
and
[`flag_f0_gaps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_gaps.md)
read the `voiced_run` column, so they give the same results on the
output as on the data with the rows kept. Other columns, including
`has_gap` and `n_missing`, are kept as they are.

## See also

[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
for the counts before removal;
[`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md)
to run first;
[`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md),
[`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md)
and
[`flag_f0_gaps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_gaps.md),
which read `voiced_run`.

## Examples

``` r
df <- data.frame(
  token = rep(c("a", "b"), each = 10),
  time  = rep(0.005 + 0.01 * (0:9), 2),
  f0    = c(NA, 200, 202, NA, NA, 206, 208, 209, NA, NA,
            NA, NA, 181, 183, 185, 187, 188, NA, 190, NA))
out <- drop_rows_without_f0(df)
out
#>    token  time  f0 voiced_run
#> 1      a 0.015 200          1
#> 2      a 0.025 202          1
#> 3      a 0.055 206          2
#> 4      a 0.065 208          2
#> 5      a 0.075 209          2
#> 6      b 0.025 181          1
#> 7      b 0.035 183          1
#> 8      b 0.045 185          1
#> 9      b 0.055 187          1
#> 10     b 0.065 188          1
#> 11     b 0.085 190          2
attr(out, "rows_without_f0")[c("removed", "start", "end", "inside")]
#> $removed
#> [1] 9
#> 
#> $start
#> [1] 3
#> 
#> $end
#> [1] 3
#> 
#> $inside
#> [1] 3
#> 

# The same voiced_run when the rows were removed beforehand: the time
# steps show the gaps
clean <- df[!is.na(df$f0), ]
identical(drop_rows_without_f0(clean)$voiced_run, out$voiced_run)
#> [1] TRUE

# Also remove the runs of 1 or 2 samples cut off at the start or end
drop_rows_without_f0(df, min_run = 3)
#>   token  time  f0 voiced_run
#> 1     a 0.055 206          1
#> 2     a 0.065 208          1
#> 3     a 0.075 209          1
#> 4     b 0.025 181          1
#> 5     b 0.035 183          1
#> 6     b 0.045 185          1
#> 7     b 0.055 187          1
#> 8     b 0.065 188          1

# A tracker that writes -1 for unvoiced frames
df2 <- data.frame(token = "c", time = 0.005 + 0.01 * (0:5),
                  f0 = c(-1, 150, 152, -1, 155, -1))
drop_rows_without_f0(df2, unvoiced = -1)
#>   token  time  f0 voiced_run
#> 1     c 0.015 150          1
#> 2     c 0.025 152          1
#> 3     c 0.045 155          2
```
