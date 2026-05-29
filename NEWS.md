# shinytone (development version)

## shinytone 0.1.2

* New `flag_level_outliers()`: a third inspection layer that compares each
  token's overall level (its median f0, in semitones) against other tokens
  of the *same speaker and same tone* using a robust modified z-score
  (median/MAD; Iglewicz & Hoaglin 1993, cutoff 3.5). It flags smoothly
  shifted contours — e.g. a low-tone token mis-tracked up into the mid-tone
  band — that the pooled max/min check and the sample-level jump check both
  miss. Surfaced through `inspect_f0()` (new `level_threshold` and
  `min_tokens` arguments, `level too high` / `level too low` notes) and the
  Inspect tab.
* The `"norm"` (normalised-time) option was removed from `time_unit` in
  `flag_pitch_jumps()`, `inspect_f0()`, and the Inspect tab: the rate-of-
  change thresholds are physiological (ST per 10 ms) and have no meaning
  once real time is discarded. Inspection runs on real-time data (s / ms).
* Inspect-tab guide rewritten around three complementary layers, and the
  "Pitch-tracking quality check" workflow now marks Normalise as optional
  (inspection runs on raw f0).

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
