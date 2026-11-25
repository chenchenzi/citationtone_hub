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

  Column names. Each token is resampled from its first f0 to its last on
  the `time` axis, as the 0-1 axes are built: in a column in Hz (one
  that
  [`hz_f0_columns()`](https://chenchenzi.github.io/citationtone_hub/reference/hz_f0_columns.md)
  finds), 0 is no f0 and is skipped. At each boundary of a `<tier>_tseq`
  (syllables in sequence), one syllable's last f0 and the next one's
  first f0 share a time (on a `<tier>_tbound`, only rows exactly on a
  boundary can). The next syllable's f0 is used there, and the two are
  never averaged. The syllable is read from `<tier>_i` (or, on a
  `<tier>_tseq`, `<tier>_t01`), so the row order does not matter;
  without either column, give each token's rows in time order. Other
  rows of a token at the same time, such as two recordings that share a
  token name, are averaged, with a message. A token whose first or last
  syllable has no values on a `<tier>_tseq` or `<tier>_tbound` is
  stretched: its other syllables fill the grid. On a `<tier>_tseq` that
  is a syllable with f0 at fewer than two times; on a `<tier>_tbound`,
  one with no f0.

- speaker:

  Optional speaker column, carried through as metadata. Each token takes
  the most frequent non-empty value among its rows with f0, so a label
  that changes within a token gives the same result in any row order,
  with or without rows without f0. A tie goes to the value that comes
  first in time.

- tone:

  Optional tone column, carried through for later validation. Read per
  token in the same way as `speaker`.

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
