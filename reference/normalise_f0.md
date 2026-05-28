# Normalise f0 by speaker

Adds a within-speaker normalised f0 column to a long-format f0 data
frame. Speaker normalisation puts contours from male and female speakers
on a comparable scale, and is a near-universal pre-processing step
before cross-speaker tone modelling, plotting, or contour summarisation
(Rose 1987; Zhu 1999).

Two formulae are available. Semitones above or below the speaker's mean
give a perceptually uniform scale. By-speaker z-scores combine centring
with unit-variance scaling. Both formulae rely on a per-speaker mean of
f0, which can itself be computed in two ways: a simple arithmetic mean
of all observations, or a mean of per-tone means so that every tone
contributes equally to the speaker's centre.

## Usage

``` r
normalise_f0(
  data,
  f0 = "f0",
  speaker = "speaker",
  tone = "tone",
  method = c("semitone", "zscore"),
  mean_method = c("weighted", "simple")
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

  - `"zscore"`: `Z = (f0 - speaker_mean) / sd(speaker_f0)`. Result has
    mean approximately 0 and SD approximately 1 within each speaker.

- mean_method:

  How the per-speaker mean used by `method` is computed: `"weighted"`
  (default; mean of per-tone means) or `"simple"` (arithmetic mean
  across all observations).

## Value

The input data frame with two added columns:

- `speaker_mean`: the per-speaker mean used for normalisation.

- `f0_st` (if `method = "semitone"`) or `f0_zscore` (if
  `method = "zscore"`).

## Details

### What the function does internally

1.  Compute a per-speaker mean of `f0` using `mean_method` (see below).

2.  Join that mean back to `data` as a new column called `speaker_mean`.

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

### Choosing `mean_method`

- `"weighted"` (default) first computes a mean f0 per (speaker, tone)
  cell, then averages those per-tone means. Each tone contributes
  equally to the speaker's centre regardless of how many tokens it has.
  This is the right default for unbalanced designs in which token counts
  differ across tones, common in fieldwork and naturalistic corpora.

- `"simple"` takes the arithmetic mean of every f0 observation for a
  speaker. Faster, and equivalent to weighted when the design is
  balanced.

### NA handling

All summary statistics use `na.rm = TRUE`. Sample-level `NA` f0 values
propagate as `NA` in the normalised column.

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
#> # A tibble: 6 × 4
#>   speaker f0_Hz speaker_mean f0_zscore
#>   <chr>   <dbl>        <dbl>     <dbl>
#> 1 dc102    246.         237.     0.194
#> 2 dc102    246.         237.     0.210
#> 3 dc102    248.         237.     0.234
#> 4 dc102    249.         237.     0.257
#> 5 dc102    250.         237.     0.283
#> 6 dc102    252.         237.     0.327
```
