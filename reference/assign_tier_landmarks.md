# Assign each time to its interval in a TextGrid interval tier

Assign each time to its interval in a TextGrid interval tier

## Usage

``` r
assign_tier_landmarks(times, t1, t2, labels)
```

## Arguments

- times:

  Numeric vector of frame times (seconds).

- t1, t2:

  Interval start/end times (from an rPraat interval tier).

- labels:

  Interval labels (same length as `t1`/`t2`).

## Value

A data.frame with one row per `times` value: `label`, `start`, `end`,
and `idx` — the 1-based ordinal of the interval among non-empty-labelled
intervals (NA inside empty/unlabelled intervals or outside the tier's
span).
