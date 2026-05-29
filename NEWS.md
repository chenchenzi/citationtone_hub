# shinytone (development version)

## shinytone 0.1.1

* `flag_pitch_jumps()` (and therefore `inspect_f0()` and the Inspect tab):
  when a sample-to-sample jump is detected, the flag is now placed on
  whichever side of the jump is *farther from the token's median f0*,
  rather than always on the landing sample. This correctly identifies
  the artefact whether it sits at the start or the end of a sequence
  (e.g. an octave doubling on the first frame of a token, which the
  previous landing-only logic mis-flagged).
* Carryover now walks both forward AND backward from each flagged
  sample, so artefact runs that begin or end the token are extended in
  both directions.
* Inspect-tab guide text and `flag_pitch_jumps()`'s function docs
  describe the new median-aware logic as an adaptation of the
  rate-of-change + carryover approach in Steffman & Cole (2022).

## shinytone 0.1.0

* First public release as an R package, alongside the existing online Shiny
  app at <https://chenzixu.shinyapps.io/shinytone/>.
* Package skin only at this stage: the Shiny app continues to run unchanged.
  Standalone analytical functions (`normalise_f0()`, `fit_gca()`,
  `fit_gamm()`, `contour_to_chao()`, ...) will be extracted in subsequent
  releases.
