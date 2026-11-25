# Values that may mark rows without f0 in an f0 column in Hz

Some pitch trackers write 0, or a value such as -1, for frames where
they found no f0.
[`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md)
reads 0 as `NA` only in the columns it recognises as Hz, so a column
such as `f0_rapt` can keep its 0s. This looks at the values of one
column and returns those that probably mark no f0, to pass to the
`unvoiced` argument of
[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
and
[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md).
The app's Start page offers to read them as no f0.
[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
also reports them, with their row counts.

## Usage

``` r
suggest_unvoiced(x)
```

## Arguments

- x:

  A vector of f0 values. A factor is read by its labels, and text
  through [`as.numeric()`](https://rdrr.io/r/base/numeric.html), so
  labels such as `"--undefined--"` are ignored.

## Value

A numeric vector of the values that may mark no f0, such as `c(0, -1)`,
or `NULL` when there are none.

## Details

A column looks like Hz when the median of its positive values is 30 to
2000, and it has no negative values apart from one value that occurs
more than once, such as -1. In such a column, 0 and that negative value
are returned, 0 first.

Nothing is returned (`NULL`) for:

- a signed or normalised column, such as semitones or z-scores: it has
  several different negative values, or a median outside 30 to 2000;

- a column with a single negative value. One value cannot be told from a
  real one, so the column is not taken for Hz, and its 0s are not
  offered either;

- a column with no 0 and no negative marker.

The check looks only at the values. A scale that is never negative but
is not Hz, such as cents above each speaker's lowest f0, can have its 0s
returned too, although they are real values there. Use this on columns
in Hz only.

## See also

[`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
and
[`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md),
whose `unvoiced` argument takes these values;
[`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md).

## Examples

``` r
# 0 and -1 for unvoiced frames
suggest_unvoiced(c(0, 0, 182.5, 190.1, -1, -1, 201.3))
#> [1]  0 -1
# A column zero_hz_to_na() has already read: nothing left to offer
suggest_unvoiced(c(NA, NA, 182.5, 190.1, 201.3))
#> NULL
# Semitones: 0 and negative values are real f0
suggest_unvoiced(c(-2.5, 0, 1.2, -0.3, 3.1, 0))
#> NULL

df <- data.frame(token = "a", time = 0.005 + 0.01 * (0:5),
                 f0_rapt = c(0, 150, 152, 0, 155, 0))
unv <- suggest_unvoiced(df$f0_rapt)
drop_rows_without_f0(df, f0 = "f0_rapt", unvoiced = unv)
#>   token  time f0_rapt voiced_run
#> 1     a 0.015     150          1
#> 2     a 0.025     152          1
#> 3     a 0.045     155          2
```
