# Sample f0 contour dataset

A subset of citation-tone f0 recordings, used as the running example
across vignettes, function `@examples` blocks, and the Shiny app's "Try
with our sample data" button. Available after
[`library(shinytone)`](https://chenchenzi.github.io/citationtone_hub/)
thanks to `LazyData: true` in `DESCRIPTION`.

## Usage

``` r
sample_f0
```

## Format

A data frame with 38,808 rows and 12 columns:

- token:

  Unique identifier for each recorded token / syllable. Rows belonging
  to the same contour share a token.

- index:

  Sample index within each token, starting from 1.

- time:

  Time in seconds within the token.

- f0_Hz:

  Fundamental frequency in Hz.

- intensity:

  Sample intensity (dB).

- speaker:

  Speaker ID (13 speakers in total).

- char:

  Chinese character corresponding to the syllable.

- position:

  Position of the syllable in its source utterance.

- start_time:

  Start time of the token within the source recording.

- tone:

  Tone category (integer codes 1-6 in this dataset).

- ipa:

  IPA transcription of the syllable's vowel.

- vowel:

  Coarse vowel category.

## Source

Xu, C. (2025). *Plastic Mandarin tones: regional identity in prosody.*
Phonetica.
[doi:10.1515/phon-2025-0001](https://doi.org/10.1515/phon-2025-0001)

## Examples

``` r
data(sample_f0)
str(sample_f0)
#> 'data.frame':    38808 obs. of  12 variables:
#>  $ token     : chr  "dc102椅1s10.0125" "dc102椅1s10.0125" "dc102椅1s10.0125" "dc102椅1s10.0125" ...
#>  $ index     : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ time      : num  0.09 0.105 0.12 0.135 0.15 0.165 0.18 0.195 0.21 0.225 ...
#>  $ f0_Hz     : num  246 246 248 249 250 ...
#>  $ intensity : num  69.4 71.5 72.5 72.9 73.2 ...
#>  $ speaker   : chr  "dc102" "dc102" "dc102" "dc102" ...
#>  $ char      : chr  "椅" "椅" "椅" "椅" ...
#>  $ position  : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ start_time: num  10 10 10 10 10 ...
#>  $ tone      : int  3 3 3 3 3 3 3 3 3 3 ...
#>  $ ipa       : chr  "i" "i" "i" "i" ...
#>  $ vowel     : chr  "i" "i" "i" "i" ...

# Per-speaker token counts
table(sample_f0$speaker)
#> 
#> dc102 dc103 dc104 dc105 dc106 dc107 dc110 dc111 dc112 dc113 dc114 dc115 dc117 
#>  3024  3003  2982  2478  3024  3024  3024  2961  3024  3108  3045  3024  3087 
```
