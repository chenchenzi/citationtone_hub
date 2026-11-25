# Landmark-normalised time columns

Puts each segment of a landmark set, such as each syllable, on its own
0-1 time axis that runs from the segment's first f0 to its last f0, so
contour shapes can be compared. Adds three columns, named from the set:

- `<set>_t01`: 0 at the segment's first f0 and 1 at its last f0.

- `<set>_tseq`: the segments in sequence, `(<set>_i - 1) + <set>_t01`.
  Segment 1's f0 fills 0 to 1, segment 2's fills 1 to 2, and so on. A
  stretch without f0 between two segments, such as a voiceless consonant
  at a syllable boundary, takes no room, so every boundary sits at a
  whole number: segment 1's last f0 and segment 2's first f0 are both at
  1.

- `<set>_tbound`: the segments in sequence, gap kept: they meet at the
  TextGrid boundaries, and the stretch without f0 between them is kept.
  The boundaries between the token's segments are at 1, 2, .... Its
  first f0 is at 0 and its last f0 at `n`, the number of segments. A
  segment with no f0 at all at the token's start or end gets `NA`, so
  the token then starts at 1 (or ends at `n - 1`), and its boundaries
  still line up with other tokens'. A stretch without f0 at a boundary
  keeps its length, split at the boundary between the segments on either
  side.

## Usage

``` r
normalise_time_landmarks(df, time, set, f0 = "f0", token = "token")
```

## Arguments

- df:

  Data frame with the `time`, `f0` and `token` columns and the set's
  `<set>_start` and `<set>_end` columns (and usually `<set>_i`), as from
  [`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md).

- time:

  Name of the time column.

- set:

  Landmark-set prefix (e.g. `"syllable"`).

- f0:

  Name of the f0 column that says which rows have f0. Default `"f0"`.
  For F0 Correction output, pass `f0 = "f0_corrected"`: left at the
  default, the tracked f0 would decide, with a message.

- token:

  Name of the token-ID column. Default `"token"`. With time counted
  within each token, two tokens can have the same boundary values, so
  segments are always taken within a token.

## Value

`df` with `<set>_t01` added (or replaced), and `<set>_tseq` and
`<set>_tbound` when `df` has a `<set>_i` column. Without `<set>_i`,
these two are not added, with a message. A missing column is an error.

## Details

A segment is one interval of the tier within one token: the rows that
share the token and the `<set>_start` and `<set>_end` values that
[`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md)
wrote. On `<set>_t01` and `<set>_tseq`, the boundaries only say which
rows belong to which segment, and the axis comes from where f0 is. On
`<set>_tbound`, the boundaries between segments also set the whole
numbers: segment `i` runs from `i - 1` to `i`, from its start boundary
to its end boundary, except that the token's first segment with f0
starts at its first f0 and its last segment with f0 ends at its last f0.
Only labelled segments count, as in
[`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md)
with `set`, so rows in an empty-label interval (silence), rows outside
the tier and rows with no token get `NA`.

A row has f0 when its `f0` value is a number. In a column in Hz (one
that
[`hz_f0_columns()`](https://chenchenzi.github.io/citationtone_hub/reference/hz_f0_columns.md)
finds), 0 is no f0. A column with negative markers, such as -1, is not
read as Hz, so those values count as f0: set them to `NA` first (see
[`suggest_unvoiced()`](https://chenchenzi.github.io/citationtone_hub/reference/suggest_unvoiced.md)).
In a signed or normalised column, such as semitones or z-scores, 0 and
negative values are f0.

Rows without f0 between a segment's first and last f0 keep their place
by time. Rows before the first f0 or after the last get `NA`, not 0 or
1, and so does every row of a segment with f0 at fewer than two times.
On `<set>_tbound` the same holds for the token as a whole: rows before
its first f0 or after its last get `NA`, and so do segments with no f0
before its first segment with f0 or after its last. A segment with no f0
between two segments with f0 keeps its place, from boundary to boundary.
When only one segment of a token has f0, its `<set>_tbound` equals its
`<set>_tseq`. So removing rows without f0, or reordering the rows, does
not change any of the three columns.

**Two rows at each boundary.** On `<set>_tseq`, segment 1's last f0 and
segment 2's first f0 are both at 1, so the axis has two rows at each
whole number, often with different f0.
[`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md)
and
[`cluster_features()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_features.md)
read the next segment's first f0 at the boundary, and never average the
two. They tell the two rows apart by `<set>_t01` (1 on the last f0, 0 on
the first) or `<set>_i`, so the row order does not matter while either
column is kept. For
[`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md),
whose AR1 correction runs along the rows, keep the rows in time order
within each token. On `<set>_tbound` the stretch without f0 keeps its
room, so a whole number has a row only where a row sits exactly on a
boundary.

**Modelling a whole word.** One smooth over `<set>_tseq`, as in
[`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md),
cannot follow an f0 jump at a boundary: both sides of it are at the same
time. On `<set>_tbound` the two sides are a gap apart, so one smooth can
follow the jump, and the boundaries still line up across tokens.
`token_t01` from
[`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md)
keeps the gap too, but the boundary falls at a different point in each
token. Or model each segment on `<set>_t01`, with one token per segment.

The first and last f0 are taken as they are: a lone sample cut off from
the rest of the segment's contour by a gap counts.
[`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md)
instead anchors its span on runs of at least `min_run = 2` samples by
default, so the two can disagree about such a segment. The Start page's
option for short edge runs, or `drop_rows_without_f0(min_run = 3)`,
removes such samples first, but only at a token's start and end: a lone
sample next to a boundary inside the token still sets that segment's 0
or 1.

For `<set>_t01` and `<set>_tseq`, only the order and spacing of `time`
within a segment matter, so the time column does not have to be in the
TextGrid's units. `<set>_tbound` compares `time` with the boundaries, so
it needs `time`, `<set>_start` and `<set>_end` on the same clock: in the
same units and counted from the same zero, as
[`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md)
writes them. In a token with f0 in two or more segments whose time lies
outside its own segments' boundaries (by more than a millionth of the
token's time range), `<set>_tbound` is `NA`, with a message.

Columns made by shinytone 1.0.0 ran from each interval's start boundary
to its end boundary. Make them again with this function.

## See also

[`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md)
for the whole token, which for a word keeps the gap between syllables
and their duration ratio;
[`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md)
with `set` for the duration of the same segments.

## Examples

``` r
# Two syllables, with no f0 at the boundary
df <- data.frame(token = "w1", time = seq(0.01, 0.10, by = 0.01),
                 f0 = c(NA, 200, 205, 210, NA, NA, 190, 185, 180, NA),
                 syllable_start = rep(c(0, 0.05), each = 5),
                 syllable_end   = rep(c(0.05, 0.11), each = 5),
                 syllable_i     = rep(1:2, each = 5))
out <- normalise_time_landmarks(df, "time", "syllable")
out[, c("time", "f0", "syllable_t01", "syllable_tseq", "syllable_tbound")]
#>    time  f0 syllable_t01 syllable_tseq syllable_tbound
#> 1  0.01  NA           NA            NA              NA
#> 2  0.02 200          0.0           0.0       0.0000000
#> 3  0.03 205          0.5           0.5       0.3333333
#> 4  0.04 210          1.0           1.0       0.6666667
#> 5  0.05  NA           NA            NA       1.0000000
#> 6  0.06  NA           NA            NA       1.2500000
#> 7  0.07 190          0.0           1.0       1.5000000
#> 8  0.08 185          0.5           1.5       1.7500000
#> 9  0.09 180          1.0           2.0       2.0000000
#> 10 0.10  NA           NA            NA              NA
# Removing the rows without f0 gives the same values
kept <- !is.na(df$f0)
identical(normalise_time_landmarks(df[kept, ], "time", "syllable")$syllable_tbound,
          out$syllable_tbound[kept])
#> [1] TRUE
```
