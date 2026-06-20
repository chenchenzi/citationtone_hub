# Apply tone re-labels and exclusions to a long-format f0 table

Adds annotation-curation columns to a long-format f0 data frame without
touching the original tone labels. Tokens named in `relabel` receive a
new value in `tone_relabelled`; tokens named in `exclude` are marked
`TRUE` in `excluded`. All other tokens keep their original tone in
`tone_relabelled` and `FALSE` in `excluded`. Curation operates at the
token level: every row belonging to a token receives that token's
relabel / exclusion.

## Usage

``` r
apply_relabels(
  data,
  token = "token",
  tone = "tone",
  relabel = NULL,
  exclude = character(0),
  note = NULL
)
```

## Arguments

- data:

  A long-format data frame with one row per f0 sample.

- token:

  Column name of token ID. Default `"token"`.

- tone:

  Column name of the (original) tone category. Default `"tone"`.

- relabel:

  A named character vector mapping token IDs (names) to their new tone
  label (values); or `NULL` for no relabelling. Tokens not named keep
  their original tone.

- exclude:

  A character vector of token IDs to mark as excluded; or an empty
  vector for none.

- note:

  A named character vector mapping token IDs (names) to a free-text
  reason for the curation decision (e.g. `"literary form"`,
  `"sandhi form"`); or `NULL` for none. Tokens not named get `NA`.

## Value

`data` with three appended columns:

- `tone_relabelled`: character, the curated tone label (original tone
  where no relabel applies).

- `excluded`: logical, `TRUE` for tokens in `exclude`.

- `curate_note`: character, the reason recorded for a token (or `NA`).

## Details

The original `tone` column is never modified, so a curation decision is
always reversible and auditable. Downstream analyses can then either
group by `tone_relabelled` (treating a variant as its own category, e.g.
`T4-colloquial` vs `T4-literary`) and/or drop rows where `excluded` is
`TRUE`.

This is an annotation step, not a signal-repair step: it assumes the f0
values are already clean (see
[`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
and the F0 Correction tab for repairing mis-tracked f0). Variant
patterns are often discovered visually or surfaced by the token-level
register check of
[`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md),
which flags tokens whose overall level is unusual for their speaker and
tone — a natural seed for relabelling.

## See also

[`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md)
for the register check that seeds variant discovery;
[`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
for the full diagnostic pass.

## Examples

``` r
data(sample_f0)
# Relabel two tokens to a literary-reading variant, exclude one mis-elicited
out <- apply_relabels(sample_f0,
                      token   = "token",
                      tone    = "tone",
                      relabel = c("t4_03" = "T4lit", "t4_07" = "T4lit"),
                      exclude = c("t5_11"))
table(out$tone, out$tone_relabelled, useNA = "ifany")
#>    
#>        1    2    3    4    5    6
#>   1 6510    0    0    0    0    0
#>   2    0 6531    0    0    0    0
#>   3    0    0 6426    0    0    0
#>   4    0    0    0 6468    0    0
#>   5    0    0    0    0 6384    0
#>   6    0    0    0    0    0 6489
```
