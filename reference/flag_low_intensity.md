# Flag low-intensity f0 samples within tokens

Detects f0 samples that fall in a low-energy region of their token,
where pitch estimates are least reliable — voicing onsets and offsets,
devoiced or breathy segments, and creaky utterance-final tails.
Intensity is an *independent* cue from f0: a smoothly drifting
mis-tracked tail at a vowel edge need not trip the rate-of-change check
in
[`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md),
but it will sit in an intensity dip. The flag is therefore most useful
(a) on its own, to surface unreliable edge frames, and (b) alongside a
jump flag, where co-occurring low intensity raises confidence that the
jump is a tracking error rather than a real pitch event.

## Usage

``` r
flag_low_intensity(
  data,
  f0 = "f0",
  token = "token",
  intensity = "intensity",
  intensity_drop = 15
)
```

## Arguments

- data:

  A long-format data frame with one row per f0 sample.

- f0:

  Column name of f0 in Hz. Default `"f0"`.

- token:

  Column name of token ID. Default `"token"`.

- intensity:

  Column name of intensity in dB. Default `"intensity"`.

- intensity_drop:

  Flag voiced samples whose intensity is more than this many dB below
  the token's peak intensity. Default `15`.

## Value

The input data frame with one appended logical column,
`flag_low_intensity`, `TRUE` for low-energy voiced samples.

## Details

For each token the function takes the **peak intensity across that
token's voiced samples** and flags every voiced sample whose intensity
is more than `intensity_drop` dB below that peak. The comparison is
relative-to-peak (not an absolute dB threshold), so it transfers across
recordings regardless of microphone distance or gain. Unvoiced samples
(`f0` is `NA` or `0`) and samples with missing intensity are never
flagged, and tokens with no voiced sample are left unflagged.

Low intensity does not by itself mean the f0 is wrong — a softly but
modally produced frame tracks fine — so this is an advisory,
inspect-don't-delete signal, consistent with the rest of the Inspect
tab.

## See also

- [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
  for the sample-level jump / octave / carryover check.

- [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  for the wrapper that runs every check.

## Examples

``` r
df <- data.frame(
  token     = rep("t1", 5),
  f0        = c(120, 122, 121, 119, 118),
  intensity = c(40, 62, 64, 63, 41)   # quiet at the edges
)
flag_low_intensity(df, intensity_drop = 15)$flag_low_intensity
#> [1]  TRUE FALSE FALSE FALSE  TRUE
```
