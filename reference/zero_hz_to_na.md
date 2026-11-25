# Read 0 Hz f0 values as missing

Pitch trackers write 0 Hz for unvoiced frames, where no pitch was
measured. A 0 left in the data is read as a very low pitch: it pulls
mean contours and fits towards 0, and gives `-Inf` in semitones. This
sets 0 to `NA` in the f0 columns of `df`. The app's Start page does this
to every CSV it reads. Its screen
([`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md))
then counts the rows without f0 in the chosen f0 column, and a box
removes them with
[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md).
The box is ticked by default, except for files where every token has the
same number of points and some token's first or last point has no f0.

## Usage

``` r
zero_hz_to_na(df, cols = hz_f0_columns(df))
```

## Arguments

- df:

  A data frame.

- cols:

  Names of the columns to clean. Defaults to the f0 columns found as
  described in Details.

## Value

`df` with 0 set to `NA` in `cols`. Attribute `"zero_hz"` gives the
number of values changed, named by column, for the columns with at least
one; it is an empty integer vector when there were none.

## Details

By default the f0 columns are found by name and by value. A column is
used when:

- its name has the word `f0`, `pitch` or `hz`, or one of VoiceSauce's f0
  names (`strF0`, `sF0`, `pF0`, `shrF0`, `oF0`), and its other words are
  ones that keep the values in Hz: `raw`, `corrected`, `original`,
  `edited`, `smoothed`, `interp`, `praat`, `reaper`, `mean`, `median`,
  `max`, `min`, `peak` and the like, or a number. So `f0`, `F0_Hz`,
  `pitch`, `f0_corrected` and `mean_f0` are used, but not `f0_st`,
  `f0_dur`, `f0_peak_loc` or `pitch_rank`, where 0 is a real value.
  Words are split at any character other than a letter or digit;

- it is numeric with no negative values, so a normalised column (which
  has negatives) is never touched;

- the median of its positive values lies between 30 and 2000, a
  plausible range for f0 in Hz.

Negative values, which some trackers use for unvoiced frames, are left
as they are, and a column that has any is not used. Give `cols` to
choose the columns yourself. For a marker such as -1, or 0 in a column
in Hz that is not found (such as `f0_rapt`),
[`suggest_unvoiced()`](https://chenchenzi.github.io/citationtone_hub/reference/suggest_unvoiced.md)
finds the values, and the `unvoiced` argument of
[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
and
[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
reads them as no f0. The Start page offers to read them as no f0.

## See also

[`hz_f0_columns()`](https://chenchenzi.github.io/citationtone_hub/reference/hz_f0_columns.md)
for the columns used;
[`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md),
which also reads f0 of 0 or below as `NA`;
[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
and
[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
to run next.

## Examples

``` r
df <- data.frame(token = "ma1", time = 1:5 / 100,
                 f0 = c(0, 212, 215, 0, 210),
                 intensity = c(0, 68, 70, 66, 0))
out <- zero_hz_to_na(df)
out$f0          # NA where the tracker wrote 0
#> [1]  NA 212 215  NA 210
out$intensity   # unchanged
#> [1]  0 68 70 66  0
attr(out, "zero_hz")
#> f0 
#>  2 
```
