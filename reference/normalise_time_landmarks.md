# Landmark-normalised time columns

Rescales a time column within each segment of a landmark set so
multisyllabic contours share a syllable-aware time axis. Adds two
columns, named from the set:

## Usage

``` r
normalise_time_landmarks(df, time, set)
```

## Arguments

- df:

  Data frame with the `time` column and the set's `_start` / `_end` (and
  optionally `_i`) columns, e.g. from
  [`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md).

- time:

  Name of the raw time column (seconds, matching the landmark units).

- set:

  Landmark-set base name (e.g. "syllable").

## Value

`df` with the two columns appended. Returned unchanged when the required
columns are absent.

## Details

- `<set>_t01` — time rescaled to 0-1 *within* each segment (segments
  overlap; good for comparing segment shapes against each other).

- `<set>_tseq` — *sequential* time, `(<set>_i - 1) + <set>_t01`, so
  segments lie end to end across the word (a word-level time axis that
  keeps order).
