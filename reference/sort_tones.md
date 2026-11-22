# Sort tone labels for display

Returns the distinct tone labels in `x` in the order to show them, for
example in a plot legend, facets or a table. The traditional Chinese
tone category names come first, in the traditional order: ping, shang,
qu, ru (level, rising, departing, entering), with yin before yang within
each. So Mandarin's yinping, yangping, shangsheng and qusheng, written
in Chinese characters, keep that order instead of the reverse order that
sorting by code point gives. Other labels, such as `T1`, `tone2` or
`55`, follow in natural order.

## Usage

``` r
sort_tones(x, natural = TRUE)
```

## Arguments

- x:

  A vector of tone labels. A factor's levels are returned as they are,
  so a factor sets its own order.

- natural:

  If `TRUE` (default), labels that are not Chinese tone names are in
  natural order; if `FALSE`, in the order
  [`factor()`](https://rdrr.io/r/base/factor.html) gives them.

## Value

A character vector of the distinct non-missing labels of `x`, or, for a
factor, its levels.

## Details

A Chinese tone name is a category character (ping, shang, qu or ru),
optionally preceded by yin or yang and followed by sheng or diao, in
simplified or traditional characters (the yinping of simplified and of
traditional characters both count). The entering tone may also be zhong,
shangyin or xiayin (as in Cantonese), placed between yin and yang.
Within a category, a name without yin or yang comes first. Space around
a label is ignored when matching. Anything else, such as the neutral
tone qingsheng or tone numbers written in Chinese, sorts with the other
labels.

Natural order compares runs of digits as numbers, ignoring case, so `T2`
comes before `T10`. With `natural = FALSE` the other labels are in the
order [`factor()`](https://rdrr.io/r/base/factor.html) gives them:
alphabetical in the current locale, or numeric for a numeric `x`.

## Examples

``` r
sort_tones(c("T10", "T2", "T1"))
#> [1] "T1"  "T2"  "T10"

# Mandarin tone names: yinping, yangping, shangsheng, qusheng
x <- c("\u4e0a\u58f0", "\u53bb\u58f0", "\u9633\u5e73", "\u9634\u5e73")
sort(x)          # locale order, often by code point
#> [1] "上声" "去声" "阳平" "阴平"
sort_tones(x)
#> [1] "阴平" "阳平" "上声" "去声"

# Use the result as factor levels, e.g. for a ggplot2 legend
factor(x, levels = sort_tones(x))
#> [1] 上声 去声 阳平 阴平
#> Levels: 阴平 阳平 上声 去声
```
