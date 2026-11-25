# Find the f0 columns in Hz

Returns the names of the columns of `df` that hold f0 in Hz. These are
the columns where
[`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md)
reads 0 as `NA`. The app's Start page also takes its default f0 column
from them.

## Usage

``` r
hz_f0_columns(df)
```

## Arguments

- df:

  A data frame.

## Value

The names of the f0 columns in Hz, in the order of `df`. An empty
character vector when there are none.

## Details

A column is found by its name and by its values:

- The name has the word `f0`, `pitch` or `hz` (so `f0hz` and `pitchhz`
  count too), or a VoiceSauce f0 name: `strF0`, `sF0`, `pF0`, `shrF0` or
  `oF0`. Words are split at any character other than a letter or digit.

- Every other word in the name keeps the values in Hz: a processing step
  (`raw`, `corrected`, `original`, `edited`, `manual`, `final`, `clean`,
  `smoothed`, `interp`, `filtered`), a tracker (`praat`, `reaper`), a
  summary (`mean`, `median`, `max`, `min`, `peak`), or a number. So
  `f0_corrected` and `mean_f0` are found, but not `f0_st`, `f0_dur`,
  `f0_peak_loc` or `pitch_rank`, where 0 is a real value.

- The values are numeric and never negative. A normalised column has
  negative values, so it is never found.

- The median of the positive values is between 30 and 2000.

A column in Hz with another name, such as `f0_rapt`, is not found, and
nor is one that marks unvoiced frames with a negative number such as -1.
Use
[`suggest_unvoiced()`](https://chenchenzi.github.io/citationtone_hub/reference/suggest_unvoiced.md)
on such a column.

## See also

[`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md),
which reads 0 as `NA` in these columns;
[`suggest_unvoiced()`](https://chenchenzi.github.io/citationtone_hub/reference/suggest_unvoiced.md)
for values that may mark no f0 in other columns.

## Examples

``` r
df <- data.frame(token = "a", time = 1:4 / 100,
                 f0 = c(0, 212, 215, 0), f0_corrected = c(0, 212, 0, 0),
                 f0_st = c(NA, 0.1, -0.2, NA), intensity = c(40, 68, 70, 41))
hz_f0_columns(df)
#> [1] "f0"           "f0_corrected"
```
