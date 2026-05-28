# Classify a Chao tone numeral string as a shape

Returns a short human-readable shape name (e.g., `"level"`, `"rising"`,
`"falling"`, `"dipping"`, `"peaking"`, or one of the finer-grained
`"low level"` / `"mid level"` / `"high level"` labels for repeated-digit
strings). Used by
[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
to add a `shape` column to its output, but also exposed as a standalone
utility for classifying numerals from external sources.

## Usage

``` r
classify_contour(chao_str)
```

## Arguments

- chao_str:

  A character string of Chao digits, e.g. `"55"`, `"214"`, `"35"`.

## Value

A character string naming the shape.

## Details

Classification rules:

- All digits identical: one of `"low level"`, `"mid-low level"`,
  `"mid level"`, `"mid-high level"`, `"high level"` according to the
  digit value.

- Three or more digits with an interior minimum below both endpoints:
  `"dipping"`.

- Three or more digits with an interior maximum above both endpoints:
  `"peaking"`.

- Otherwise, `"rising"` if the last digit is higher than the first,
  `"falling"` if lower, `"level"` if equal.

## References

Chao, Y. R. (1930). A system of tone-letters. *Le Maître Phonétique*,
30, 24–27.

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
which calls this internally to populate the `shape` column.

## Examples

``` r
classify_contour("55")    # "high level"
#> [1] "high level"
classify_contour("214")   # "dipping"
#> [1] "dipping"
classify_contour("35")    # "rising"
#> [1] "rising"
classify_contour("51")    # "falling"
#> [1] "falling"
```
