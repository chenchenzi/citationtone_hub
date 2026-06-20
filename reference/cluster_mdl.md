# Recompute MDL information cost for stored clusterings at a given bending

A light helper so the app can re-evaluate the MDL curve live as the user
tunes the bending factor, without re-running the (slow) gap/GMM
diagnostics.

## Usage

``` r
cluster_mdl(contours, assignments, bending = 1)
```

## Arguments

- contours:

  Token x n_points matrix of resampled f0 (e.g.
  `cluster_features(...)$contours`).

- assignments:

  A named list of cluster vectors, one per k (e.g.
  `choose_k_f0(...)$assignments`).

- bending:

  MDL bending factor (\>= 1); higher down-weights model/residual cost
  relative to the partition cost.

## Value

Named numeric vector of MDL costs, one per element of `assignments`.
