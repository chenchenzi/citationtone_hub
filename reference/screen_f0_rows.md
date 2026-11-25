# Screen f0 data for rows without f0

Counts the rows without f0 in each token: at the start of the token, at
its end, and inside it, in gaps between two samples with f0. It also
counts the gaps that the time steps show, and the short runs of samples
that a gap cuts off at the start or end of a token. Nothing is changed:
[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
removes the rows, and the counts here say what it does with the same
arguments. The app's Start page shows these counts when a file is
uploaded.

## Usage

``` r
screen_f0_rows(
  data,
  token = "token",
  time = "time",
  f0 = "f0",
  gap_ratio = 1.5,
  unvoiced = NULL,
  run = "voiced_run"
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

- unvoiced:

  Values in the `f0` column to read as no f0, such as `-1` or
  `c(0, -1)`, for a column in Hz whose tracker marks unvoiced frames
  this way. Default `NULL`: only `NA` counts as no f0.

- run:

  Name of a column that marks gaps already, such as the `voiced_run`
  column
  [`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
  adds. Default `"voiced_run"`. Used only when the column is present.
  `NULL` ignores it.

## Value

A data frame with one row per token, in order of first appearance in
`data`. Its first column holds the token and is named as the `token`
column. The other columns are:

- `n_rows`: rows of the token.

- `n_f0`: rows with f0. These are the rows left after removal.

- `n_start`, `n_end`, `n_inside`: rows without f0 at the start, at the
  end, and inside the token.

- `n_gaps`: gaps from rows without f0.

- `n_gaps_time`: gaps that the time steps between samples with f0 show.
  A gap from rows usually shows here too, as a long step, but not in a
  token whose usual step is more than one frame (see Details). Always 0
  when the grid is not regular.

- `frag_start`, `frag_end`: samples in short runs cut off at the start
  and end (see Details).

Attributes:

- `summary`: a named integer vector of counts over the whole file:
  `rows`, `tokens`, `rows_f0`, `rows_without_f0`, `unvoiced` (rows read
  as no f0 through `unvoiced`), `start` and `start_tokens`, `end` and
  `end_tokens`, `inside`, `gaps` and `gap_tokens` (gaps from rows),
  `gaps_time` and `gaps_time_tokens` (gaps from time steps, as in
  `n_gaps_time`), `gaps_time_only` and `gaps_time_only_tokens` (gaps
  from time steps with no rows without f0 in them, and the tokens with
  one), `gaps_run` and `gaps_run_tokens` (gaps that only the `run`
  column marks), `no_f0_rows` and `no_f0_tokens` (tokens with no f0),
  `no_time` (rows without f0 that cannot be placed, as they or the
  token's samples with f0 have no time), `rows_after` and `tokens_after`
  (left after removal), `run_tokens` and `max_run` (tokens whose
  `voiced_run` would go above 1, and its highest value), `few_tokens`
  (tokens left with 1 or 2 samples), `short_runs`, `short_runs_start`,
  `short_runs_end`, `short_rows`, `short_tokens`, `short_tokens_start`,
  `short_tokens_end` and `short_tokens_both` (short edge runs, their
  samples, and the tokens with one, at the start, at the end, or at both
  ends), `rows_after_short` (rows left after removal with
  `min_run = 3`), `repeated_tokens` (tokens with a repeated time, as in
  `repeated_time`), `other_step_tokens` (tokens set aside for their
  2-unit steps, as in `other_step`), and `edge_tokens` and `edge_rows`
  (tokens with f0 whose first or last row has no f0, and all their
  rows). The `voiced_run` column that
  [`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
  adds marks `gaps + gaps_time_only + gaps_run` gaps. A token can end
  with two short runs, so `short_runs` can be more than
  `short_tokens_start + short_tokens_end`.

- `grid`: a list with the time-step details. `regular` is `TRUE` when
  gaps were read from the time steps (in the tokens not set aside).
  `step` is the file's usual step and `common` is `TRUE` when most
  tokens share it. `ambiguous` counts steps between 1.25 and 1.75 times
  the usual step, `nonincreasing` counts steps of 0 (repeated times),
  and `resolution` is how finely the times are written. `units` is
  `TRUE` when the times are written to the step itself, so steps were
  read in whole units of `resolution`. `off_unit` then counts the steps
  that are not exactly one unit, and `units_ok` is `TRUE` when they are
  at most 1% of the steps. `n_steps` counts the steps judged,
  `n_tokens_read` the tokens whose steps can be read, `repeated` and
  `repeated_tokens` the tokens with a repeated time, `other_step` and
  `other_step_tokens` the tokens set aside for their 2-unit steps,
  `n_gaps_rows` the gaps from rows, and `n_gaps_time` the steps above
  `gap_ratio` times the usual step, regular or not. `why` gives the
  reasons the grid is not regular, as sentences named `"no_steps"`,
  `"ambiguous"`, `"nonincreasing"` (every token has a repeated time),
  `"set_aside"` (every token is set aside, some for their 2-unit steps),
  `"units"` (times written to the step, but more than 1% of the steps
  are not exactly one unit) and `"resolution"` (times written too
  coarsely). It is empty for a regular grid.

- `equal_rows`: `TRUE` when there are at least two tokens and all have
  the same number of rows. A single token is not counted, so that one
  token of frame data is not taken for resampled points.

- `stretch`: a list. `warn` is `TRUE` when `equal_rows` is `TRUE` and
  some token with f0 has no f0 in its first or last row. `n_rows` is the
  number of rows each token has (`NA` unless `equal_rows`), `n_tokens`
  the number of tokens with f0 that have no f0 in their first or last
  row, and `factor` the least stretch of such a token on the axes from
  the first f0 to the last, `(N - 1) / (N - 2)`, whether or not the row
  is removed. `warn_short` and `n_tokens_short` say the same for the
  samples with f0 that `min_run = 3` would remove at a token's start or
  end.

- `no_f0`: the tokens with no f0, as text. Removal drops them entirely.

- `repeated_time`: the tokens where two samples with f0 have the same
  time, as text. No gaps are read from their time steps.

- `other_step`: the tokens set aside because more than 1 step in 8 is
  exactly 2 units, as text (see Details). No gaps are read from their
  time steps.

- `few_samples`: the tokens left with 1 or 2 samples after removal, as a
  named integer vector of the samples left.
  [`flag_f0_tokens()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_tokens.md)
  flags them.

- `edge_missing`: the tokens with f0 whose first or last row has no f0,
  as text (see Details).

- `unvoiced_suggested`: a data frame with columns `value` (the values
  [`suggest_unvoiced()`](https://chenchenzi.github.io/citationtone_hub/reference/suggest_unvoiced.md)
  finds in the `f0` column, such as 0 and -1) and `rows` (the rows
  holding each value). It has no rows when there is nothing to suggest.

- `settings`: the arguments used, as a list.

## Details

**Rows without f0.** A row has no f0 when its value in the `f0` column
is `NA`, `NaN` or infinite, or is one of the `unvoiced` values. Only
this one column counts. Run
[`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md)
first, so that 0 Hz counts as no f0 in Hz columns. Other 0 and negative
values count as f0 unless they are listed in `unvoiced`: in a signed or
normalised column (semitones, z-scores) they are real values.

**Where they are.** Rows are taken in time order within each token,
whatever their order in `data`. A row without f0 is at the start when
its time is before the token's first sample with f0, at the end when it
is after the last one, and inside otherwise. A row without f0 at the
same time as a sample with f0 counts as inside, but it makes no gap. A
token with no f0 at all is counted apart: its rows are none of start,
end or inside. A row with no time (`NA`) cannot be placed either. It
still counts in `n_rows`, and in `n_f0` when it has f0. A row whose
token is `NA` belongs to a token of its own.

**Gaps.** A gap is a stretch inside a token with no f0. A gap from rows
has one or more rows without f0 between two samples with f0. A gap from
time steps is a step between two samples with f0 that is more than
`gap_ratio` times the token's usual step. Gaps from time steps are read
only when the time steps form a regular grid. Then the same gaps show
whether or not the rows without f0 are still in the data, for tokens
whose usual step is one frame. (In a token where every other frame has
no f0, the usual step is two frames, so once those rows are removed the
steps show no gap.) The attribute `grid` says whether the grid is
regular, and why not. Samples timed at each glottal pulse are not
regular. Give the real time column, never a `*_t01`, `*_tseq` or
`*_tbound` axis: those restart or change step at every segment.

The default `gap_ratio = 1.5` already means "one frame or more missing":
a gap of one frame doubles the step, and 1.5 leaves room for jitter and
floating-point noise in the times.

**Times written to the step.** When the times are written about as
finely as the step, as for a 10 ms grid written to 2 decimals (0.01,
0.02, ...), the steps are read in whole units of 0.01: a step of 0.02 or
more is a gap. This needs at least 99% of the steps to be exactly one
unit. A true step that is not a whole number of units fails this, such
as 12.5 ms or 7.5 ms written to 2 decimals: rounding gives steps of 0 or
0.02 in a regular pattern, not gaps. So does a file with more than 1 gap
in 100 steps, as the two cannot be told apart. Times written more
coarsely than the step, such as a 25 ms grid written to 2 decimals, give
no gaps from the time steps. A true step within 1% of the unit, such as
10.05 ms written to 2 decimals, gives too few 2-unit steps to be told
from real gaps, so write the times to at least 3 decimals when the step
is not exactly 10 ms.

**Tokens set aside.** No gaps are read from the time steps of these
tokens. Their gaps from rows still count, and the other tokens are read
as usual.

- A token where two samples with f0 have the same time: the samples are
  not one series of frames. The attribute `repeated_time` lists these
  tokens.

- With times written to the step, a token where at least 2 steps, and
  more than 1 in 8, are exactly 2 units (0.02): its frames are probably
  a little more than one unit apart, as for 12.5 ms frames among 10 ms
  ones. A 10 ms token with that many gaps is set aside too, as the two
  cannot be told apart. The attribute `other_step` lists these tokens.

A sample with f0 but no time leaves a hole in the time steps. On a
regular grid its neighbours are then two steps apart, which reads as a
gap.

**Gaps an earlier removal marked.** When the data already have a `run`
column (`voiced_run`, from an earlier
[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
or from F0 Extraction), a change in its value inside a token is a gap
too. This keeps the gaps from rows removed earlier, which the time steps
cannot show on a grid that is not regular. `run = NULL` ignores the
column.

**Short edge runs.** A run is a stretch of samples with f0 between two
gaps, or between a gap and the start or end of the token. A run of 1 or
2 samples that a gap cuts off at the start or end of a token is often a
tracking error in the silence. It holds the token's first or last f0, so
it sets where the 0-1 time axes start or end, whether or not the rows
without f0 are removed. `frag_start` and `frag_end` count these samples:
those before the token's first run of at least 3 samples, and those
after its last. A token with no run that long has none.
`drop_rows_without_f0(min_run = 3)` removes them.

**Equal row counts.** When every token has the same number of rows `N`,
as after resampling to `N` points, and a token's first or last row has
no f0, removing that row leaves the token with fewer than `N` points.
The time axes that run from the first f0 to the last (`token_t01`,
`<tier>_t01` and the models' per-token rescale) start or end such a
token one point in whether or not the row is removed, so its points sit
off the `N`-point grid, stretched by at least `(N - 1) / (N - 2)`, 5%
for 21 points. `time_prop` is used as it is and keeps the grid. The
attribute `stretch` records this case, and the same for the short edge
runs that `min_run = 3` removes, which do move the first or last f0.

**Tokens missing an end point.** A token with f0 whose first or last row
has no f0 is missing an end point. When every token has `N` rows, such a
token cannot have `N` points over the part with f0. The attribute
`edge_missing` lists these tokens, and the summary counts them and their
rows. They are counted for any data, but matter most when every token
has the same number of points, as after resampling.

**Values that may mark no f0.** Some trackers write 0 or a value such as
-1 for unvoiced frames, in a column
[`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md)
does not pick. The attribute `unvoiced_suggested` gives the values
[`suggest_unvoiced()`](https://chenchenzi.github.io/citationtone_hub/reference/suggest_unvoiced.md)
finds in the `f0` column, and the rows that hold each. It is read from
the column as it is, before `unvoiced`, so it stays the same once the
values are passed to `unvoiced`.

## See also

[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
to remove the rows;
[`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md)
to run first;
[`suggest_unvoiced()`](https://chenchenzi.github.io/citationtone_hub/reference/suggest_unvoiced.md)
for values that may mark no f0;
[`flag_f0_tokens()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_tokens.md)
to flag the tokens with an issue;
[`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md)
and
[`flag_f0_gaps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_gaps.md).

## Examples

``` r
df <- data.frame(
  token = rep(c("a", "b"), each = 10),
  time  = rep(0.005 + 0.01 * (0:9), 2),
  f0    = c(NA, 200, 202, NA, NA, 206, 208, 209, NA, NA,
            NA, NA, 181, 183, 185, 187, 188, NA, 190, NA))
scr <- screen_f0_rows(df)
scr
#>   token n_rows n_f0 n_start n_end n_inside n_gaps n_gaps_time frag_start
#> 1     a     10    5       1     2        2      1           1          2
#> 2     b     10    6       2     1        1      1           1          0
#>   frag_end
#> 1        0
#> 2        1
attr(scr, "summary")[c("rows_without_f0", "start", "end", "inside")]
#> rows_without_f0           start             end          inside 
#>               9               3               3               3 
attr(scr, "grid")$regular
#> [1] TRUE
# A gap cuts off 2 samples at the start of token a, and 1 at the end of b
scr[c("token", "frag_start", "frag_end")]
#>   token frag_start frag_end
#> 1     a          2        0
#> 2     b          0        1
# Tokens whose first or last row has no f0
attr(scr, "summary")[c("edge_tokens", "edge_rows")]
#> edge_tokens   edge_rows 
#>           2          20 
attr(scr, "edge_missing")
#> [1] "a" "b"
```
