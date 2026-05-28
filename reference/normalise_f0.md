# Normalise f0 by speaker

Add a normalised f0 column to `data`, computed per speaker. Two methods
are available: semitones referenced on a per-speaker mean (perceptually
uniform), or per-speaker z-scores (statistical centring and scaling).

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

  A data frame containing at least the f0, speaker, and tone columns.

- f0:

  Name of the column with raw f0 values (Hz). Default `"f0"`.

- speaker:

  Name of the speaker ID column. Default `"speaker"`.

- tone:

  Name of the tone category column. Default `"tone"`. Used only when
  `mean_method = "weighted"`.

- method:

  Normalisation formula. One of:

  - `"semitone"` (default): `ST = 12 * log2(f0 / speaker_mean)`.

  - `"zscore"`: `Z = (f0 - speaker_mean) / sd(f0_speaker)`.

- mean_method:

  How the per-speaker mean is computed. One of `"weighted"` (default) or
  `"simple"`. See Details.

## Value

The input data frame with two added columns:

- `speaker_mean` — the per-speaker mean used for normalisation

- `f0_st` (if `method = "semitone"`) or `f0_zscore` (if
  `method = "zscore"`)

## Details

Per-speaker means can be computed in two ways:

- `"weighted"` (default): mean of per-tone means. Each tone contributes
  equally to the centre of the speaker's tonal space, regardless of how
  many tokens it has. Preferred for unbalanced designs.

- `"simple"`: arithmetic mean of all f0 observations for that speaker.

## Examples

``` r
df <- data.frame(
  speaker = rep(c("S01", "S02"), each = 6),
  tone    = rep(c("T1", "T2", "T3"), times = 4),
  f0      = c(150, 180, 130, 160, 190, 140,
              200, 240, 170, 210, 250, 175)
)
head(normalise_f0(df, method = "semitone"))
#>   speaker tone  f0 speaker_mean      f0_st
#> 1     S01   T1 150     158.3333 -0.9360301
#> 2     S01   T2 180     158.3333  2.2203827
#> 3     S01   T3 130     158.3333 -3.4134407
#> 4     S01   T1 160     158.3333  0.1812827
#> 5     S01   T2 190     158.3333  3.1564129
#> 6     S01   T3 140     158.3333 -2.1304582
head(normalise_f0(df, method = "zscore"))
#> # A tibble: 6 × 5
#>   speaker tone     f0 speaker_mean f0_zscore
#>   <chr>   <chr> <dbl>        <dbl>     <dbl>
#> 1 S01     T1      150         158.   -0.360 
#> 2 S01     T2      180         158.    0.935 
#> 3 S01     T3      130         158.   -1.22  
#> 4 S01     T1      160         158.    0.0719
#> 5 S01     T2      190         158.    1.37  
#> 6 S01     T3      140         158.   -0.791 
```
