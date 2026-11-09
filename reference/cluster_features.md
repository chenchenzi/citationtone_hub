# Build per-token feature vectors for f0-contour clustering

Resamples every token's f0 contour to a common length and turns it into
a fixed-length feature vector suitable for clustering. f0 should already
be speaker-normalised (e.g. `f0_st` from
[`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md))
so that clusters reflect tone shape and register rather than who has a
high voice.

## Usage

``` r
cluster_features(
  data,
  f0 = "f0",
  token = "token",
  time = "time",
  speaker = NULL,
  tone = NULL,
  n_points = 21L,
  features = c("points", "legendre", "dct", "derivative"),
  degree = 4L,
  register = c("level", "shape")
)
```

## Arguments

- data:

  Long-format data frame, one row per f0 sample.

- f0, token, time:

  Column names. `time` is normalised per token.

- speaker:

  Optional speaker column, carried through as metadata.

- tone:

  Optional tone column, carried through for later validation.

- n_points:

  Number of points to resample each contour to, spread evenly from its
  first to its last f0 value. Default 21 (one point every 5%), as in
  [`resample_f0_equal()`](https://chenchenzi.github.io/citationtone_hub/reference/resample_f0_equal.md)
  and the app's Cluster tab.

- features:

  Feature representation: `"points"` (the resampled contour),
  `"legendre"` (orthogonal-polynomial coefficients of `degree`), `"dct"`
  (low-order discrete-cosine coefficients), or `"derivative"` (first
  difference: cluster by rate of change / movement, discarding height).

- degree:

  Order for `"legendre"`/`"dct"` features. Default 4.

- register:

  `"level"` keeps each contour's height (so high vs low level tones
  separate); `"shape"` centres each contour to mean 0 (cluster on shape
  only).

## Value

A list with `features` (token x p matrix used for clustering),
`contours` (token x n_points resampled f0, centred per token when
`register = "shape"`, for plotting cluster means), `tokens`, `meta`
(token / speaker / tone), and the settings used: `n_points`, `register`
and `feature_type`.
