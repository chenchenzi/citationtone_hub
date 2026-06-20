# Attach TextGrid landmark columns to a long-format f0 data frame

For each selected interval tier and each token, reads the token's
TextGrid and tags every f0 frame with the interval it falls in. Adds
four columns per tier, named from a sanitised tier name `p`: `p`
(interval label), `p_start`, `p_end` (interval boundaries, seconds), and
`p_i` (1-based segment index among labelled intervals — e.g. syllable 1,
2, 3 ...).

## Usage

``` r
attach_landmarks(df, audio, tier_names, strip_ext = TRUE)
```

## Arguments

- df:

  Long-format f0 data frame with `token` and `time` columns.

- audio:

  Data frame with `basename` and `tg_path` columns (fp_audio_data).

- tier_names:

  Character vector of interval-tier names to attach.

- strip_ext:

  Strip file extensions when matching `token` to `basename`.

## Value

`df` with the landmark columns appended. Returned unchanged when there
are no tiers, no token/time columns, or no matching TextGrids.
