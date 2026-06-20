# Cluster f0 contours into candidate tone categories

Cluster f0 contours into candidate tone categories

## Usage

``` r
cluster_f0(
  feat,
  method = c("kmeans", "hclust", "gmm"),
  k,
  nstart = 25L,
  hclust_method = "ward.D2"
)
```

## Arguments

- feat:

  A list from
  [`cluster_features()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_features.md)
  (needs `features`+`contours`).

- method:

  `"kmeans"`, `"hclust"` (Ward), or `"gmm"` (model-based, needs
  `mclust`).

- k:

  Number of clusters.

- nstart:

  k-means restarts. Default 25.

- hclust_method:

  Linkage for `"hclust"`. Default `"ward.D2"`.

## Value

A list with `assignment` (named integer per token), `method`, `k`,
`sizes`, `cluster_means` (k x n_points mean contour per cluster),
`tokens`, `uncertainty` (per-token 1 - max posterior, GMM only; else
`NULL`), and `tree` (the fitted `hclust` object for method `"hclust"`,
else `NULL`).

## References

Kaland, C. (2023). Contour clustering: A field-data-driven approach for
documenting and analysing prototypical f0 contours. *Journal of the
International Phonetic Association*, 53(1), 159–188.
[doi:10.1017/S0025100321000049](https://doi.org/10.1017/S0025100321000049)

Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means
clustering algorithm. *Applied Statistics*, 28(1), 100–108.

Ward, J. H. (1963). Hierarchical grouping to optimize an objective
function. *Journal of the American Statistical Association*, 58(301),
236–244.

Scrucca, L., Fop, M., Murphy, T. B., & Raftery, A. E. (2016). mclust 5:
Clustering, classification and density estimation using Gaussian finite
mixture models. *The R Journal*, 8(1), 289–317.
