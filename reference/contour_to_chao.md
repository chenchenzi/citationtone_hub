# Convert per-tone contours to Chao tone numerals

Summarises per-tone contours as Chao tone numerals (Chao 1930): 5-level
digit strings like `"55"`, `"35"`, or `"214"` that describe each contour
by its endpoints and any interior turning point. Two conversion methods
are computed in one pass so the user can compare them (Xu & Zhang 2024).

## Usage

``` r
contour_to_chao(
  contour_data,
  tone_col = "tone",
  time_col = "time",
  f0_col = "f0_predicted",
  threshold = 0.5,
  raw_data = NULL,
  raw_token = "token",
  raw_f0 = "f0",
  raw_tone = "tone"
)
```

## Arguments

- contour_data:

  Per-tone contour data, typically from
  [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md),
  [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md),
  or
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md).
  Must contain `tone`, `time`, and `f0_predicted` columns (or rename via
  the `tone_col`, `time_col`, `f0_col` arguments).

- tone_col, time_col, f0_col:

  Column names within `contour_data`.

- threshold:

  Chao-units threshold for turning-point detection. Default `0.5`. See
  Details.

- raw_data, raw_token, raw_f0, raw_tone:

  Deprecated and ignored, with a warning. They supplied token-level data
  for a third, "robust" method, which has been removed: its range, one
  SD beyond the mean token extremes, squeezed averaged contours towards
  the middle digits and moved more with pitch-tracking errors than the
  range of the contours themselves.

## Value

A data frame with one row per tone, containing:

- `tone`: tone label.

- `n_digits`, `n_digits_int`: number of digits (`2` or `3`) in the
  reference-line and interval-based numerals.

- `refline`: reference-line method numeral, e.g. `"55"`.

- `interval`: interval-based numeral, e.g. `"45"`.

- `shape`: shape name from
  [`classify_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/classify_contour.md)
  applied to `refline`.

- `dip`, `peak`: how far the lowest (highest) point between 10% and 90%
  of the contour lies below (above) both endpoints, in Chao units on the
  reference-line scale; zero or negative when the contour has no dip
  (peak). The larger of the two is sampled as a turning point when it
  exceeds `threshold`; it adds a middle digit where that digit shows the
  turn (see Details).

- `sample_times`, `ref_continuous`, `int_continuous`: list columns of
  the per-sample numeric values underlying the digit strings, useful for
  plotting or further analysis. When a turning point is sampled these
  have three values even if a numeral keeps only two digits.

Three attributes are attached to the returned data frame: `f0_min` and
`f0_max`, the lowest and highest points of `contour_data`, which set the
range, and `threshold`.

## Details

### The two conversion methods

Both use Fraction of Range (FOR) normalisation, which maps f0 onto a
bounded scale between `f0_min` and `f0_max`, the lowest and highest
points of `contour_data`. With mean contours from
[`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md),
each of these points is itself an average over tokens, not a single f0
measurement. The two methods differ in how scaled values are mapped to
Chao digits.

- **Reference-line FOR on `[1, 5]`**:
  `f0' = (f0 - f0_min) / (f0_max - f0_min) * 4 + 1`, then rounded. Five
  reference lines at integer values; each sampled point snaps to the
  nearest integer.

- **Interval-based FOR on `(0, 5]`**:
  `f0' = (f0 - f0_min) / (f0_max - f0_min) * 5`, then ceilinged. Five
  equal-width intervals; each sampled point is assigned to its interval
  via [`ceiling()`](https://rdrr.io/r/base/Round.html).

### Input scale

Both methods are linear in the units of the input, so the input sets the
pitch scale of the numerals. Semitones and LZ from
[`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)
give a log-frequency scale, on which equal steps are equal pitch ratios,
as pitch is heard; Hz and z-scores give a scale linear in Hz. Negative
values, as in speaker-normalised f0, need no special handling.

f0 in Hz from several speakers should be normalised by speaker first,
for example to LZ, the log z-score of Zhu (1999): averaging Hz mixes
higher and lower voices, which shifts the mean contours. The app's Chao
tab converts raw f0 to LZ by speaker automatically.

### Turning-point detection

For each tone, the function samples the contour either at its two
endpoints or at the two endpoints plus one interior turning point. The
turning point is the lowest point between 10% and 90% of the contour if
it lies below both endpoints (a *dip*), or the highest point if it lies
above both (a *peak*), whichever goes further, and it is sampled when it
goes more than `threshold` Chao units beyond the nearer endpoint.

Each scale then keeps the middle digit only if it shows the turn: lower
than both endpoint digits for a dip, higher than both for a peak. If it
rounds to the same digit as an endpoint, the turn is too shallow to show
at five levels and that numeral keeps two digits (`23`, not `223`, which
would read as a late rise). The two scales round differently, so a tone
can get two digits on one and three on the other.

The default `threshold = 0.5` (half a Chao unit on the `[1, 5]` scale)
is a conservative choice. Lower thresholds sample more turning points
and so give more 3-digit numerals, useful for tonally rich languages (a
sampled turn too shallow to show is still left out). Any value from 4
(the height of the scale) up means no turning point is sampled. The
`dip` and `peak` columns report how far each tone's turning point goes,
so a tone close to the threshold, whose digit count depends on it, can
be spotted.

## References

Chao, Y. R. (1930). A system of tone-letters. *Le Maître Phonétique*,
30, 24–27.

Zhu, X. (1999). *Shanghai Tonetics*. LINCOM Europa.

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

- [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md)
  for one common upstream step.

- [`classify_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/classify_contour.md)
  for the shape classification helper.

- [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md)
  and
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
  for model-based contour sources.

## Examples

``` r
data(sample_f0)
# Normalise by speaker first (here LZ), so that higher and lower voices
# are averaged on a common scale.
lz <- normalise_f0(sample_f0, f0 = "f0_Hz", speaker = "speaker",
                   tone = "tone", method = "lz")
mc <- compute_mean_contour(lz, token = "token", f0 = "f0_lz",
                           time = "time", tone = "tone")
chao <- contour_to_chao(mc)
chao[, c("tone", "refline", "interval", "shape")]
#>   tone refline interval   shape
#> 1    1      32       22 falling
#> 2    2     212       12 dipping
#> 3    3      32       32 falling
#> 4    4      45       55  rising
#> 5    5      31       21 falling
#> 6    6      23       23  rising

# Semitones also give numerals on a log-frequency scale, but keep each
# speaker's own pitch range.
st <- normalise_f0(sample_f0, f0 = "f0_Hz", speaker = "speaker",
                   tone = "tone", method = "semitone")
mc_st <- compute_mean_contour(st, token = "token", f0 = "f0_st",
                              time = "time", tone = "tone")
chao_st <- contour_to_chao(mc_st)
chao_st[, c("tone", "refline", "interval", "shape")]
#>   tone refline interval         shape
#> 1    1      22       22 mid-low level
#> 2    2     212       12       dipping
#> 3    3      33       32     mid level
#> 4    4      45       55        rising
#> 5    5      31       21       falling
#> 6    6      23       23        rising
```
