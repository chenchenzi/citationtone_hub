# Normalise f0 by speaker

Adds a within-speaker normalised f0 column to a long-format f0 data
frame. Speaker normalisation puts contours from male and female speakers
on a comparable scale, and is a near-universal pre-processing step
before cross-speaker tone modelling, plotting, or contour summarisation
(Rose 1987; Zhu 1999).

Three formulae are available. Semitones above or below the speaker's
mean give a perceptually uniform scale. By-speaker z-scores combine
centring with unit-variance scaling. Log z-scores (LZ) do both on a
logarithmic scale: they are z-scores of log f0. Every formula relies on
a per-speaker mean (and the z-scores on a per-speaker SD), which can be
computed in two ways: over all observations alike, or so that every tone
contributes equally whatever its number of tokens.

## Usage

``` r
normalise_f0(
  data,
  f0 = "f0",
  speaker = "speaker",
  tone = "tone",
  method = c("semitone", "zscore", "lz"),
  mean_method = c("weighted", "simple"),
  sd_correction = TRUE
)
```

## Arguments

- data:

  A long-format data frame with one row per f0 sample. Must contain at
  least the f0, speaker, and (for `"weighted"` means) tone columns named
  below.

- f0:

  Name of the column with raw f0 values, in Hz. Default `"f0"`.

- speaker:

  Name of the speaker ID column. Default `"speaker"`.

- tone:

  Name of the tone category column. Default `"tone"`. Only read when
  `mean_method = "weighted"`.

- method:

  Normalisation formula. One of:

  - `"semitone"` (default): `ST = 12 * log2(f0 / speaker_mean)`. Result
    is 0 at the speaker mean and ±12 at one octave above or below.

  - `"zscore"`: `Z = (f0 - speaker_mean) / s`, where `s` is the
    speaker's SD of f0.

  - `"lz"`: `LZ = (log10(f0) - m) / s`, where `m` and `s` are the
    speaker's mean and SD of log10 f0, as in Zhu (1999). Any base gives
    the same LZ, since it only rescales the logs.

  With `mean_method = "simple"` both z-scores have mean 0 and SD 1
  within each speaker (SD 1 as measured with the same `sd_correction`).
  With `"weighted"` the same holds with every tone counting equally: the
  per-tone means average to 0, while the plain mean over all
  observations moves away from 0 when tones have unequal numbers of
  observations.

- mean_method:

  How the per-speaker mean, and the SD for the z-scores, are computed:
  `"weighted"` (default; every tone counts equally) or `"simple"` (all
  observations alike; the classic formula).

- sd_correction:

  Logical: apply Bessel's correction to the SD. `TRUE` (default) uses
  the corrected (sample) SD, dividing by n - 1; `FALSE` the uncorrected
  (population) SD, dividing by n. Only affects `"zscore"` and `"lz"`.
  See "The SD correction" below.

## Value

The input data frame with two added columns:

- `speaker_mean`: the per-speaker mean used for normalisation, in Hz.
  For `"lz"` this is the geometric mean, `10^m`.

- `f0_st` (if `method = "semitone"`), `f0_zscore` (if
  `method = "zscore"`) or `f0_lz` (if `method = "lz"`).

## Details

### What the function does internally

1.  Compute a per-speaker mean of `f0` (of log f0 for `"lz"`) using
    `mean_method`, and for the z-scores a per-speaker SD the same way
    (see below).

2.  Add that mean to `data` as a new column called `speaker_mean`.

3.  Apply the selected normalisation formula sample by sample and return
    the augmented data frame.

### Choosing `method`

- `"semitone"` is the standard for citation-tone work. The scale is
  perceptually uniform (one semitone is roughly one just-noticeable
  pitch step), centred on the speaker's own range, and preserves the
  logarithmic structure of musical pitch perception.

- `"zscore"` is useful when both centring and scaling are desired, i.e.,
  when you want to remove each speaker's baseline *and* equalise their
  f0 variability. Useful for cross-speaker statistical models where the
  raw variance differs strongly across speakers.

- `"lz"` (log z-score) is the z-score of log f0 (Zhu 1999). Like
  semitones it works on ratios, so the same interval counts the same in
  a low and a high voice; like `"zscore"` it also divides by each
  speaker's spread, so speakers with narrow and wide pitch ranges end up
  on one scale. It equals semitones measured from the speaker's
  geometric mean (the `speaker_mean` that `"lz"` reports) divided by the
  speaker's SD in semitones. `f0_st` is measured from the arithmetic
  mean instead: with `mean_method = "simple"`, `f0_st / sd(f0_st)` has
  the same shape as LZ, shifted by a small per-speaker constant; with
  `"weighted"`, LZ also divides by the tone-weighted SD, so the two
  differ slightly in scale as well. It is the usual "z-score" in studies
  of tone in languages of China: 37 of the 136 studies reviewed by Xu
  and Zhang (2024) used it, against 4 that used a plain z-score. Because
  it equalises ranges, prefer semitones when differences in range are
  themselves of interest (e.g. comparing the size of tone spaces across
  speakers or varieties).

### Choosing `mean_method`

- `"weighted"` (default) lets every tone count once, regardless of how
  many tokens it has. The centre is the mean of the per-tone means. For
  the z-scores the SD is weighted the same way: each observation gets
  weight 1 / (number of tones x observations of its tone), as in
  [`stats::cov.wt()`](https://rdrr.io/r/stats/cov.wt.html). This is the
  right default for unbalanced designs in which token counts differ
  across tones, common in fieldwork and naturalistic corpora. It is this
  package's tone-balanced variant.

- `"simple"` takes the mean (and SD) of every observation for a speaker.
  This is the classic formula: Rose's (1987) z-score and Zhu's (1999)
  LZ. Choose it to reproduce published values.

The two give identical results when every tone has the same number of
observations.

### The SD correction

With `sd_correction = TRUE` (default) the z-scores use the corrected
(sample) SD, dividing by n - 1 (Bessel's correction) as
[`stats::sd()`](https://rdrr.io/r/stats/sd.html) and the published
formulas do; with `"weighted"` this is the reliability-weights
correction of `stats::cov.wt(method = "unbiased")`, which reduces to n -
1 when every tone has the same number of observations. `FALSE` gives the
uncorrected (population) SD, dividing by n. With `"simple"`, or equal
numbers of observations per tone, the two differ by a factor of sqrt(n /
(n - 1)), negligible for a speaker with hundreds of observations. With
`"weighted"` the factor is 1 / sqrt(1 - sum(w^2)), which is set mostly
by the tone with the fewest observations: a tone with only a few frames
can make it several percent. Semitones use no SD, so the argument does
not affect them.

### NA handling

All summary statistics use `na.rm = TRUE`. Sample-level `NA` f0 values
propagate as `NA` in the normalised column. For `"lz"`, f0 values of 0
or below (sometimes used to mark unvoiced frames) have no logarithm, so
they are treated as `NA` too.

## References

Rose, P. (1987). Considerations in the normalisation of the fundamental
frequency of linguistic tone. *Speech Communication*, 6(4), 343–352.
[doi:10.1016/0167-6393(87)90008-1](https://doi.org/10.1016/0167-6393%2887%2990008-1)

Zhu, X. (1999). *Shanghai Tonetics*. LINCOM Europa.

Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation tone
production studies: Methodology and recommendations. *The Journal of the
Acoustical Society of America*, 156(4), 2538–2565.
[doi:10.1121/10.0032356](https://doi.org/10.1121/10.0032356)

## See also

- [`fit_polynomial()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_polynomial.md),
  [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md),
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
  for downstream contour modelling that typically operates on the
  normalised column.

- [`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
  for converting mean contours to Chao tone numerals, often called on
  `normalise_f0()` output.

## Examples

``` r
data(sample_f0)

out <- normalise_f0(sample_f0,
                    f0      = "f0_Hz",
                    speaker = "speaker",
                    tone    = "tone",
                    method  = "semitone")
head(out[, c("speaker", "tone", "f0_Hz", "speaker_mean", "f0_st")])
#>   speaker tone    f0_Hz speaker_mean     f0_st
#> 1   dc102    3 245.7078     237.0675 0.6197450
#> 2   dc102    3 246.4516     237.0675 0.6720735
#> 3   dc102    3 247.5111     237.0675 0.7463436
#> 4   dc102    3 248.5496     237.0675 0.8188289
#> 5   dc102    3 249.7015     237.0675 0.8988754
#> 6   dc102    3 251.6647     237.0675 1.0344568

# Same data, z-score instead of semitones:
out_z <- normalise_f0(sample_f0,
                      f0      = "f0_Hz",
                      speaker = "speaker",
                      tone    = "tone",
                      method  = "zscore")
head(out_z[, c("speaker", "f0_Hz", "speaker_mean", "f0_zscore")])
#>   speaker    f0_Hz speaker_mean f0_zscore
#> 1   dc102 245.7078     237.0675 0.1935695
#> 2   dc102 246.4516     237.0675 0.2102331
#> 3   dc102 247.5111     237.0675 0.2339705
#> 4   dc102 248.5496     237.0675 0.2572359
#> 5   dc102 249.7015     237.0675 0.2830415
#> 6   dc102 251.6647     237.0675 0.3270239

# Log z-score (LZ), common in studies of Chinese tone:
out_lz <- normalise_f0(sample_f0,
                       f0      = "f0_Hz",
                       speaker = "speaker",
                       tone    = "tone",
                       method  = "lz")
head(out_lz[, c("speaker", "f0_Hz", "speaker_mean", "f0_lz")])
#>   speaker    f0_Hz speaker_mean     f0_lz
#> 1   dc102 245.7078     233.0639 0.2886142
#> 2   dc102 246.4516     233.0639 0.3051268
#> 3   dc102 247.5111     233.0639 0.3285633
#> 4   dc102 248.5496     233.0639 0.3514366
#> 5   dc102 249.7015     233.0639 0.3766959
#> 6   dc102 251.6647     233.0639 0.4194796
```
