# Classify a Chao tone numeral string as a shape

Returns a short shape name ("level", "rising", "falling", "dipping",
"peaking", or a finer "low level" / "mid level" / "high level" for
single-digit / repeated-digit strings).

## Usage

``` r
classify_contour(chao_str)
```

## Arguments

- chao_str:

  A character string of Chao digits (1-5), e.g. `"55"`, `"214"`, `"35"`.

## Value

A character string naming the shape.

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
