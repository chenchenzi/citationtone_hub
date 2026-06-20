# Agreement between clusters and known tone labels

Agreement between clusters and known tone labels

## Usage

``` r
cluster_agreement(clusters, labels)
```

## Arguments

- clusters:

  Integer/character cluster assignment per token.

- labels:

  Known tone labels per token (same length/order).

## Value

A list with `table` (clusters x labels contingency table) and `ari`
(adjusted Rand index; 1 = perfect, ~0 = chance).
