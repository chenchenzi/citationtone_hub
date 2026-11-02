# Write Chao tone numerals as IPA tone letters

Converts Chao tone numerals to the IPA tone letters that Chao (1930)
designed them for, one letter per digit: 1 = ˩ (extra low), 2 = ˨, 3 =
˧, 4 = ˦, 5 = ˥ (extra high). For example `"35"` becomes ˧˥ and `"214"`
becomes ˨˩˦.

## Usage

``` r
chao_to_letters(x)
```

## Arguments

- x:

  A character (or integer) vector of Chao numerals, e.g.
  `c("55", "35", "214", "51")`, such as the `refline` or `interval`
  column of
  [`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md).

## Value

A character vector the same length as `x`. Elements that are `NA`, or
that contain anything other than the digits 1 to 5, become `NA` (the
latter with a warning).

## Details

Each digit becomes one letter, so a level tone keeps both of its digits:
`"55"` becomes ˥˥. Some descriptions write a level tone with a single
letter; `substr(x, 1, 1)` gives that form.

Fonts with tone-letter support (Charis SIL, Doulos SIL, Gentium) join a
sequence of letters into a single contour line; other fonts show the
letters as separate bars. The characters are the same either way.

## References

Chao, Y. R. (1930). A system of tone-letters. *Le Maître Phonétique*,
30, 24–27.

## See also

[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md),
whose numeral columns this converts, and
[`classify_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/classify_contour.md)
for a shape name.

## Examples

``` r
chao_to_letters(c("55", "35", "214", "51"))
#> [1] "˥˥"  "˧˥"  "˨˩˦" "˥˩" 
```
