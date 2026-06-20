# Diagnostics for choosing the number of clusters (candidate tones)

Runs k-means across a range of `k` and reports the elbow (total
within-cluster sum of squares), average silhouette width, and – when the
`cluster` / `mclust` packages are available – the gap statistic and the
GMM/BIC-preferred `k`. No single index is authoritative; use the spread
to read off a plausible *range* for the number of tones.

## Usage

``` r
choose_k_f0(feat, k_range = 2:8, nstart = 25L, gap = TRUE, bending = 1)
```

## Arguments

- feat:

  A list from
  [`cluster_features()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_features.md),
  or a numeric feature matrix.

- k_range:

  Integer vector of cluster counts to evaluate. Default `2:8`.

- nstart:

  k-means restarts. Default 25.

- gap:

  Whether to compute the gap statistic (needs the `cluster` package).
  Default TRUE.

- bending:

  MDL bending factor (\>= 1; Kaland & Ellison 2023). Higher values
  down-weight the model/residual cost so fewer clusters are preferred;
  tune until the MDL (information-cost) curve is U-shaped. Default 1.

## Value

A list with `table` (k, wss, silhouette, gap, gap_se, mdl), suggestions
`k_silhouette`, `k_gap`, `k_gmm`, `k_mdl` (any may be `NA`), and
`assignments` (the per-k k-means cluster vectors).

## References

Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the
interpretation and validation of cluster analysis. *Journal of
Computational and Applied Mathematics*, 20, 53–65.

Tibshirani, R., Walther, G., & Hastie, T. (2001). Estimating the number
of clusters in a data set via the gap statistic. *Journal of the Royal
Statistical Society B*, 63(2), 411–423.

Kaland, C., & Ellison, T. M. (2023). Evaluating cluster analysis on f0
contours: An information theoretic approach on three languages.
*Proceedings of the 20th International Congress of Phonetic Sciences*,
3448–3452.
