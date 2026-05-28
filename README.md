# shinytone

> A Citation Tone Research Hub — interactive Shiny app and accompanying
> R functions for citation tone analysis across tone languages.

Developed by [Chenzi Xu](https://chenzixu.rbind.io/). Co-authored with
[Cong Zhang](https://congzhang-linguist.github.io).

<!-- badges: start -->
<!-- badges: end -->

Shinytone integrates the full citation tone analysis workflow into one place:
pitch extraction, by-speaker f0 normalisation, growth curve analysis,
generalised additive mixed models, outlier inspection, and Chao tone numeral
summarisation.

## Two ways to use it

**1. Online app — no install needed**

The hosted version runs at
<https://chenzixu.shinyapps.io/shinytone/>. Best for classroom demos,
trying out the workflow without setup, and any analysis where per-upload
batches stay under ~100 MB.

**2. Local R package — full control**

Install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("chenchenzi/citationtone_hub")
```

Launch the same UI offline (no upload limits, no memory caps, recordings
stay on your machine):

```r
shinytone::run_app()
```

Or call the analytical functions directly from a script or RMarkdown
document:

```r
library(shinytone)
# Programmatic API will be exposed in subsequent releases:
# df <- normalise_f0(df, method = "semitone", speaker = "speaker_id")
# gca <- fit_gca(df, time = "time_norm", tone = "tone")
```

## When to use which

| Use case | Online app | Local package |
|---|:---:|:---:|
| Trying it out / teaching | ✓ | ✓ |
| Typical research datasets (hundreds to a few thousand tokens) | ✓ | ✓ |
| Per-upload batch over 100 MB | ✗ | ✓ |
| Ethics-restricted recordings (children, clinical, etc.) | ✗ | ✓ |
| Scripted / batch / RMarkdown analysis | ✗ | ✓ |
| Three+ simultaneous collaborators | limited | ✓ |

## Citation

If you use Shinytone in published work, please cite:

```r
citation("shinytone")
```

## License

MIT (code). Long-form tutorials and vignettes are CC BY-NC 4.0; see
individual files for details.
