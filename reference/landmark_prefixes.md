# Column names for TextGrid landmark tiers

The column prefix `p` that
[`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md)
uses for each tier, whose columns are then `p`, `p_start`, `p_end` and
`p_i`. A tier gets the same prefix whichever tiers are attached with it,
and in whatever order, as long as `all_tiers` is the same.

## Usage

``` r
landmark_prefixes(
  tier_names,
  all_tiers = tier_names,
  reserved = c("token", "time", "f0", "intensity", "f0_corrected", "f0_original",
    "edited", "token_dropped", "n_missing", "has_gap", "point", "time_prop",
    "voiced_run")
)
```

## Arguments

- tier_names:

  Tier names to give prefixes.

- all_tiers:

  Every tier name the columns could come from, such as all interval
  tiers of the corpus from
  [`tg_interval_tiers()`](https://chenchenzi.github.io/citationtone_hub/reference/tg_interval_tiers.md).
  The numbering depends only on this set, so pass the same one in every
  call. Names in `tier_names` but not in `all_tiers` are numbered after
  it.

- reserved:

  Column names a tier must not take. Defaults to the columns that F0
  Extraction, F0 Correction and the Start page write into the f0 data,
  `voiced_run` included.

## Value

A character vector of prefixes, named by `tier_names`.

## Details

The prefix keeps the letters, in any script, and the digits of the tier
name. ASCII letters are lowercased. Any other run of characters becomes
`_`, and `_` at either end is dropped; a name with nothing left becomes
`"tier"`. So `"Syllable (broad)"` gives `syllable_broad`, and a tier
named in Chinese characters or IPA keeps its name. Names are first put
in Unicode NFC form (when the utf8 package is installed) and combining
marks are then dropped, so in a UTF-8 locale every prefix is a syntactic
R name unless it starts with a digit.

A prefix already in use gets `_2`, `_3` and so on: when two tiers give
the same prefix, or when a prefix or one of its columns is in
`reserved`. A tier whose name is already its own prefix keeps it, and
the others are numbered in the byte order of their names. So tiers
`syllable`, `Syllable` and `syllable!` give `syllable`, `syllable_2` and
`syllable_3`, and a tier named `time` gives `time_2`. Columns made later
from a prefix (`p_t01`, `p_tseq`, `p_tbound`, `p_f0_dur`) are kept apart
too.

## Examples

``` r
landmark_prefixes(c("syllable", "Syllable (broad)", "time"))
#>         syllable Syllable (broad)             time 
#>       "syllable" "syllable_broad"         "time_2" 
# The same prefix whichever of the tiers is attached
tiers <- c("syllable", "Syllable", "syllable!")
landmark_prefixes("Syllable", all_tiers = tiers)
#>     Syllable 
#> "syllable_2" 
landmark_prefixes(tiers)
#>     syllable     Syllable    syllable! 
#>   "syllable" "syllable_2" "syllable_3" 
# A tier named in Chinese characters keeps its name
landmark_prefixes(intToUtf8(c(0x58F0, 0x8C03)))
#>   声调 
#> "声调" 
```
