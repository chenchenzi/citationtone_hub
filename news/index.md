# Changelog

## shinytone (development version)

### shinytone 0.1.1

- [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
  (and therefore
  [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  and the Inspect tab): when a sample-to-sample jump is detected, the
  flag is now placed on whichever side of the jump is *farther from the
  token’s median f0*, rather than always on the landing sample. This
  correctly identifies the artefact whether it sits at the start or the
  end of a sequence (e.g. an octave doubling on the first frame of a
  token, which the previous landing-only logic mis-flagged).
- Carryover now walks both forward AND backward from each flagged
  sample, so artefact runs that begin or end the token are extended in
  both directions.
- Inspect-tab guide text and
  [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)’s
  function docs describe the new median-aware logic as an adaptation of
  the rate-of-change + carryover approach in Steffman & Cole (2022).

### shinytone 0.1.0

- First public release as an R package, alongside the existing online
  Shiny app at <https://chenzixu.shinyapps.io/shinytone/>.
- Package skin only at this stage: the Shiny app continues to run
  unchanged. Standalone analytical functions
  ([`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md),
  [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md),
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md),
  [`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md),
  …) will be extracted in subsequent releases.
