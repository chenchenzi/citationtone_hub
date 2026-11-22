# Attach TextGrid landmark columns to a long-format f0 data frame

For each selected interval tier and each token, reads the token's
TextGrid (with
[`read_textgrid()`](https://chenchenzi.github.io/citationtone_hub/reference/read_textgrid.md))
and tags every f0 frame with the interval it falls in. Adds four columns
per tier, named from the tier's prefix `p` (see
[`landmark_prefixes()`](https://chenchenzi.github.io/citationtone_hub/reference/landmark_prefixes.md)):
`p` (interval label), `p_start`, `p_end` (interval boundaries, seconds),
and `p_i` (1-based segment index among labelled intervals, e.g. syllable
1, 2, 3 ...).

## Usage

``` r
attach_landmarks(
  df,
  audio,
  tier_names,
  strip_ext = TRUE,
  all_tiers = tier_names
)
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

- all_tiers:

  Every tier name the columns could come from, such as all interval
  tiers of the corpus from
  [`tg_interval_tiers()`](https://chenchenzi.github.io/citationtone_hub/reference/tg_interval_tiers.md);
  passed to
  [`landmark_prefixes()`](https://chenchenzi.github.io/citationtone_hub/reference/landmark_prefixes.md).
  Give the same set in every call, so a tier always gets the same
  columns.

## Value

`df` with the landmark columns appended. Returned unchanged when there
are no tiers, no `token`/`time` columns, or no `basename`/`tg_path`
columns in `audio`. A token with no matching TextGrid gets `NA` in these
columns, so when no TextGrid matches at all, the columns are added and
are all `NA`. These attributes are present only when there is something
to report:

- `"tg_unreadable"`: a token whose TextGrid could not be read keeps `NA`
  landmarks; this gives the reason for each such token, named by token.

- `"tg_guessed"`: for each TextGrid read in a guessed 8-bit code page
  (see
  [`read_textgrid()`](https://chenchenzi.github.io/citationtone_hub/reference/read_textgrid.md)),
  that code page named for the user (such as "Western European
  (Windows-1252)"), named by token.

- `"tg_stray"`: for each TextGrid read as UTF-8 with bytes that are not
  UTF-8 shown as U+FFFD (see
  [`read_textgrid()`](https://chenchenzi.github.io/citationtone_hub/reference/read_textgrid.md)),
  the lines that held them (such as "line 12"), named by token.

- `"renamed_tiers"`: the prefix of each tier that got `_2`, `_3` and so
  on, or `"tier"` for a name with no letters or digits, named by tier.
  Lowercasing and `_` for other characters are not reported, so
  `"Syllable"` giving `syllable` is not listed.

- `"missing_tiers"`: the tiers that none of the TextGrids read has as an
  interval tier. Their columns are all `NA`, and a warning says so.
