# Keep only f0 frames that fall inside chosen TextGrid intervals

Subsets a long-format f0 data frame — with landmark columns already
attached by
[`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md)
— to the rows whose time falls inside selected intervals of one tier:
automatically detected vowel intervals, the rhyme (first vowel interval
to the end of the token; monosyllabic data only), or an explicit set of
labels.

## Usage

``` r
filter_interval_rows(
  df,
  set,
  mode = c("vowel", "rhyme", "labels"),
  labels = NULL,
  token = "token"
)
```

## Arguments

- df:

  Long-format data frame with `token` plus the tier's landmark columns
  (`<set>`, `<set>_i`, ...) from
  [`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md).

- set:

  Landmark-set base name, i.e. the sanitised tier name used as the label
  column (e.g. `"vowel"`, `"segment"`).

- mode:

  One of `"vowel"`, `"rhyme"`, or `"labels"`. See Details.

- labels:

  Character vector of interval labels to keep (only for
  `mode = "labels"`).

- token:

  Name of the token-ID column (used by `mode = "rhyme"`). Default
  `"token"`.

## Value

The subset of `df`, row order preserved.

## Details

The three modes:

- `"vowel"`: keep rows whose interval label passes
  [`ipa_vowel_label()`](https://chenchenzi.github.io/citationtone_hub/reference/ipa_vowel_label.md)
  — all vowel intervals, whatever the syllable count.

- `"rhyme"`: keep rows in labelled intervals from the *first* vowel
  interval of each token to the token's end (vowel + coda). This assumes
  each token is one syllable; for multisyllabic tokens it would span
  from the first vowel across every following syllable. Tokens with no
  vowel-labelled interval are dropped entirely.

- `"labels"`: keep rows whose (whitespace-trimmed) label equals one of
  `labels` — for tiers where the region of interest is marked explicitly
  (e.g. a `rhyme` tier, or non-IPA label schemes).

Rows with an `NA` label — tokens with no matching TextGrid, or frames
outside the tier's span — are always excluded, as are frames in
empty-labelled (silence) intervals. Callers should tell users how many
tokens were lost that way rather than let them vanish silently.

## See also

[`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md)
to add the landmark columns;
[`ipa_vowel_label()`](https://chenchenzi.github.io/citationtone_hub/reference/ipa_vowel_label.md)
for the vowel test.
