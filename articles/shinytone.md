# Getting started with shinytone

``` r

library(shinytone)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
```

## What is shinytone?

**shinytone** is a research hub for *citation tone analysis* in tone
languages. It provides:

- An interactive Shiny app that walks you through the full workflow —
  pitch extraction, by-speaker f0 normalisation, outlier detection,
  growth-curve and generalised additive mixed models, and Chao tone
  numeral summarisation.
- A small set of pure R functions that implement the analytical core, so
  you can run the same analyses scripted from RMarkdown or another
  package.

This vignette walks through the package’s scripted side using a small
toy dataset. If you prefer a graphical workflow, install the package and
run
[`shinytone::run_app()`](https://chenchenzi.github.io/citationtone_hub/reference/run_app.md),
or visit the hosted app at <https://chenzixu.shinyapps.io/shinytone/>.

## A toy dataset

The package functions expect long-format data — one row per f0 sample.
For this vignette we build a tiny synthetic corpus: 2 speakers × 3 tones
× 4 tokens per tone × 20 frames per token.

``` r

set.seed(1)

make_toy_corpus <- function(n_frames = 20) {
  spec <- expand.grid(
    speaker = c("S01", "S02"),
    tone    = c("T1", "T2", "T3"),
    rep     = 1:4,
    stringsAsFactors = FALSE
  )
  spec$token <- paste(spec$speaker, spec$tone, spec$rep, sep = "_")
  spec$item  <- paste0("w", spec$rep)

  do.call(rbind, lapply(seq_len(nrow(spec)), function(i) {
    r <- spec[i, ]
    t <- seq(0, 1, length.out = n_frames)
    # Speaker baseline: female speaker S01 a bit higher than S02
    base <- if (r$speaker == "S01") 220 else 130
    # Tone shape: T1 level high, T2 rising, T3 falling
    shape <- switch(r$tone,
      T1 = rep(0,  n_frames),
      T2 = 20 * t,
      T3 = -25 * t
    )
    data.frame(
      speaker = r$speaker,
      tone    = r$tone,
      item    = r$item,
      token   = r$token,
      time    = t,
      f0      = base + shape + rnorm(n_frames, sd = 3),
      stringsAsFactors = FALSE
    )
  }))
}

toy <- make_toy_corpus()
head(toy)
#>   speaker tone item    token       time       f0
#> 1     S01   T1   w1 S01_T1_1 0.00000000 218.1206
#> 2     S01   T1   w1 S01_T1_1 0.05263158 220.5509
#> 3     S01   T1   w1 S01_T1_1 0.10526316 217.4931
#> 4     S01   T1   w1 S01_T1_1 0.15789474 224.7858
#> 5     S01   T1   w1 S01_T1_1 0.21052632 220.9885
#> 6     S01   T1   w1 S01_T1_1 0.26315789 217.5386
```

Two speakers means the absolute Hz values differ — that’s the situation
[`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)
is designed for.

## Normalise f0 by speaker

[`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)
adds a `speaker_mean` column plus either `f0_st` (semitones, default) or
`f0_zscore`, computed per speaker:

``` r

normed <- normalise_f0(toy,
                       f0          = "f0",
                       speaker     = "speaker",
                       tone        = "tone",
                       method      = "semitone",
                       mean_method = "weighted")
head(normed)
#>   speaker tone item    token       time       f0 speaker_mean       f0_st
#> 1     S01   T1   w1 S01_T1_1 0.00000000 218.1206     219.3491 -0.09723267
#> 2     S01   T1   w1 S01_T1_1 0.05263158 220.5509     219.3491  0.09459405
#> 3     S01   T1   w1 S01_T1_1 0.10526316 217.4931     219.3491 -0.14711136
#> 4     S01   T1   w1 S01_T1_1 0.15789474 224.7858     219.3491  0.42386601
#> 5     S01   T1   w1 S01_T1_1 0.21052632 220.9885     219.3491  0.12890929
#> 6     S01   T1   w1 S01_T1_1 0.26315789 217.5386     219.3491 -0.14349150
```

Now `f0_st` is on a comparable scale across speakers. A quick visual:

``` r

ggplot(normed, aes(time, f0_st, colour = tone, group = token)) +
  geom_line(alpha = 0.6) +
  facet_wrap(~ speaker) +
  labs(y = "f0 (semitones, relative to speaker mean)")
```

![](shinytone_files/figure-html/unnamed-chunk-4-1.png)

## Inspect for outliers and pitch-tracking artefacts

[`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
flags two kinds of issues: tokens whose per-token max or min sits more
than `z_threshold` SDs from the speaker’s mean, and individual samples
where the rate of f0 change exceeds physiological plausibility (Sundberg
1973, Steffman & Cole 2022).

``` r

inspected <- inspect_f0(toy,
                        f0      = "f0",
                        token   = "token",
                        time    = "time",
                        speaker = "speaker",
                        tone    = "tone")
inspected |>
  distinct(token, .keep_all = TRUE) |>
  count(flagged_token)
#> # A tibble: 1 × 2
#>   flagged_token     n
#>   <lgl>         <int>
#> 1 FALSE            24
```

For the toy data, nothing is flagged (it’s too clean). On real data,
`flagged_token` and the human-readable `flag_notes` column tell you
which tokens to investigate.

## Fit token-level polynomials

[`fit_polynomial()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_polynomial.md)
returns one row per token with Legendre polynomial coefficients (`c0`,
`c1`, `c2`, …). Useful as inputs to downstream classifiers or regression
models.

``` r

poly_coefs <- fit_polynomial(normed,
                             f0      = "f0_st",
                             token   = "token",
                             time    = "time",
                             speaker = "speaker",
                             tone    = "tone",
                             degree  = 2)
head(poly_coefs)
#> # A tibble: 6 × 6
#>   token    speaker tone        c0      c1      c2
#>   <chr>    <chr>   <chr>    <dbl>   <dbl>   <dbl>
#> 1 S01_T1_1 S01     T1     0.0944   0.0482  0.0109
#> 2 S01_T1_2 S01     T1    -0.00218 -0.0975  0.0269
#> 3 S01_T1_3 S01     T1     0.0535  -0.0328  0.0399
#> 4 S01_T1_4 S01     T1     0.0104   0.0844  0.0342
#> 5 S01_T2_1 S01     T2     0.848    0.769  -0.0552
#> 6 S01_T2_2 S01     T2     0.916    0.823   0.0944
```

`c0` ≈ token-level mean f0 (in semitones). `c1` ≈ linear slope (positive
for rising, negative for falling). `c2` ≈ curvature.

## Fit a Growth Curve Analysis (GCA)

[`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
is a thin wrapper around
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) with sensible
defaults for tone-shape modelling — orthogonal polynomials on per-token
normalised time, plus conventional random effects on speaker and item.

``` r

gca <- fit_gca(normed,
               f0      = "f0_st",
               time    = "time",
               token   = "token",
               tone    = "tone",
               speaker = "speaker",
               item    = "item",
               degree  = 2,
               random_slope_speaker = FALSE,
               random_slope_item    = FALSE)

# Population-level per-tone curves
preds <- predict_gca(gca, n = 60)
ggplot(preds, aes(time, f0_predicted, colour = tone)) +
  geom_line(linewidth = 1)
```

For non-standard model structures (custom contrasts, alternative random
effects), call [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html)
directly —
[`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
is just a convenience wrapper for the common case.

## Convert to Chao tone numerals

[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
reduces per-tone mean (or model-predicted) contours to Chao numerals —
5-level digit strings like `55`, `35`, or `214`.

``` r

# Pre-aggregate the toy data into a per-tone mean contour
mean_contour <- compute_mean_contour(toy,
                                     token = "token", f0 = "f0",
                                     time  = "time",  tone = "tone")

chao <- contour_to_chao(mean_contour, raw_data = toy,
                        raw_token = "token", raw_f0 = "f0",
                        raw_tone  = "tone")
chao[, c("tone", "refline", "interval", "robust", "shape")]
#>   tone refline interval robust     shape
#> 1   T1      33       33     44 mid level
#> 2   T2      35       35     44    rising
#> 3   T3      31       31     43   falling
```

Three conversion methods are computed in one pass:

- **`refline`** — reference-line FOR, rounded on the `[1, 5]` scale
- **`interval`** — interval-based FOR, ceiling on the `(0, 5]` scale
- **`robust`** — robust FOR using μ ± σ of the highest- and lowest-tone
  extremes (requires `raw_data`)

The `shape` column summarises the contour (“rising”, “falling”,
“dipping”, etc.) and is derived from the reference-line numeral via
[`classify_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/classify_contour.md).

## Where to go next

- Browse the [function
  reference](https://chenchenzi.github.io/citationtone_hub/reference/index.md)
  for the full API.
- Run
  [`shinytone::run_app()`](https://chenchenzi.github.io/citationtone_hub/reference/run_app.md)
  locally for the full graphical workflow, including audio handling and
  a Praat script generator.
- Cite the package with `citation("shinytone")` in any work that uses
  it.

## References

- Steffman, J., & Cole, J. (2022). Pitch tracking artefacts and the
  detection of voicing errors in spontaneous speech.
- Sundberg, J. (1973). Data on maximum speed of pitch changes.
- Xu, C. (2025). Tone production in Changsha. *Phonetica*.
