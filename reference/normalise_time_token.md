# Whole-token 0-1 time

Puts each token on one 0-1 time axis that runs from its first f0 to its
last f0, so contour shapes can be compared. Adds the column `token_t01`.
No landmarks are needed.

## Usage

``` r
normalise_time_token(df, time, token, f0 = "f0")
```

## Arguments

- df:

  Data frame with the `time`, `token` and `f0` columns.

- time:

  Name of the time column. Only the order and spacing of times within a
  token matter.

- token:

  Name of the token-ID column.

- f0:

  Name of the f0 column that says which rows have f0. Default `"f0"`.
  For F0 Correction output, pass `f0 = "f0_corrected"`: left at the
  default, the tracked f0 would decide, with a message.

## Value

`df` with `token_t01` added (or replaced). A missing column is an error.

## Details

Rows have f0 as in
[`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md):
in a column in Hz, 0 is no f0, and a column with negative markers such
as -1 is not read as Hz, so set those to `NA` first. Rows without f0
between the first and last f0 keep their place by time. Rows before the
first f0 or after the last get `NA`, not 0 or 1, and so does every row
of a token with f0 at fewer than two times and every row with no token.
So removing rows without f0 does not change the column. A lone f0 sample
in the silence, cut off by a gap, does become the first or last f0.
[`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md)
anchors its span on runs of at least `min_run = 2` samples by default,
so for such a token its `token_f0_dur` is shorter than the stretch
`token_t01` covers. The Start page's option for short edge runs, or
`drop_rows_without_f0(min_run = 3)`, removes such samples first.

For a word of several syllables, `token_t01` is the whole word: the
stretch without f0 between the syllables and the syllables' duration
ratio are kept. So the syllable boundary falls at a different point in
each token and blurs when tokens are averaged. `<tier>_tseq` from
[`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)
puts the syllables in sequence instead, so the boundaries line up, and
`<tier>_tbound` puts them in sequence with the gap kept, so the TextGrid
boundaries line up. A word tier's `<word>_t01` gives the same values as
`token_t01` when all of the token's f0 lies inside the word interval.

The model fits rescale a time column the same way
([`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md)
with `f0`), so a fit on the time column equals a fit on `token_t01`,
apart from two cases. Tokens with f0 at one time only: the fits keep
those at 0.5, while `token_t01` is `NA` on them. And rows with no token:
the fits treat them as one token of their own, while `token_t01` is `NA`
on them.

Columns made by shinytone 1.0.0 ran from each token's first row to its
last, rows without f0 included. Make them again with this function.

## See also

[`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)
for each syllable on its own axis, or the syllables in sequence.

## Examples

``` r
df <- data.frame(token = rep(c("a", "b"), c(6, 5)),
                 time  = c(0:5, 0:4) / 100,
                 f0    = c(NA, 210, 215, NA, 220, NA, 180, 185, 190, 195, NA))
normalise_time_token(df, "time", "token")
#>    token time  f0 token_t01
#> 1      a 0.00  NA        NA
#> 2      a 0.01 210 0.0000000
#> 3      a 0.02 215 0.3333333
#> 4      a 0.03  NA 0.6666667
#> 5      a 0.04 220 1.0000000
#> 6      a 0.05  NA        NA
#> 7      b 0.00 180 0.0000000
#> 8      b 0.01 185 0.3333333
#> 9      b 0.02 190 0.6666667
#> 10     b 0.03 195 1.0000000
#> 11     b 0.04  NA        NA
```
