# Changelog

## shinytone (development version)

- **GAMM diagnostics** (new).
  [`diagnose_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/diagnose_gamm.md)
  and a “Model diagnostics” section on the GAMM tab: after fitting, one
  click reports the basis-dimension check (k’, edf, k-index, p-value,
  flagging under-resourced time smooths), the four `gam.check()`
  residual panels (Q–Q, residuals vs fitted, histogram, observed vs
  fitted), the residual ACF, and the concurvity table, with a text
  download of all diagnostics. The ACF is computed per token (in the
  spirit of `itsadug::acf_resid()`) and AR1-whitened when the fit used
  an AR1 correction, so it shows whether the correction actually worked.
- The GAMM tab’s **AR1 correction is now on by default**:
  densely-sampled f0 frames are strongly autocorrelated, so smooth
  p-values are anticonservative without it.
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
  (whose `use_ar1` argument still defaults to `FALSE`) now estimates
  `rho` from the lag-1 autocorrelation *within* tokens rather than
  across the flat concatenation, so token boundaries no longer bias the
  estimate.
- [`run_app()`](https://chenchenzi.github.io/citationtone_hub/reference/run_app.md)
  now checks GitHub once per launch for a newer shinytone release
  (2-second timeout, silent when offline) and, when one exists, prints
  the update command in the console and shows a one-time notification in
  the app. Disable with `options(shinytone.check_updates = FALSE)`.

## shinytone 1.0.0

First stable release. The Shiny app covers the citation-tone workflow
end to end (collect, extract, inspect, correct, curate, normalise,
cluster, visualise, model, Chao numerals), and the analytical functions
behind each step are exported and documented. Highlights since the 0.1.x
line:

- **Contour clustering** (new). An unsupervised “tone discovery”
  workflow that groups tokens by f0-contour shape when the number of
  tone categories is unknown (Kaland 2023). New functions:
  [`cluster_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_f0.md)
  (k-means, hierarchical Ward, or Gaussian-mixture clustering),
  [`cluster_features()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_features.md)
  (represent each contour as resampled points, Legendre / DCT
  coefficients, or its derivative),
  [`choose_k_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/choose_k_f0.md)
  (suggest the number of groups via silhouette, gap statistic, and a
  minimum-description-length cost),
  [`cluster_mdl()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_mdl.md),
  and
  [`cluster_agreement()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_agreement.md)
  (adjusted Rand index against provisional labels). Surfaced through the
  new **Cluster** tab.
- **Contour sonification** (new).
  [`sonify_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/sonify_f0.md)
  renders an f0 contour as an audible waveform: a pure tone, a complex
  tone (12 harmonics, band-limited below Nyquist), or a source-filter
  synthesised vowel (`a` / `i` / `u`), with the pitch gliding along the
  contour. An optional `intensity` argument shapes the loudness envelope
  from per-frame dB. The Cluster tab’s “Listen to the contours” panel
  plays each candidate cluster’s mean contour back, so prototypical
  tones can be heard, not only seen (faithful Hz when an Hz column is
  present, or shape-only on a chosen base pitch).
- **Hear your corrections** (new). The F0 Correction tab sonifies the
  contour you are editing as an *Extracted vs Corrected* A/B pair,
  played at the token’s own duration with loudness following the
  measured intensity, so you can hear whether an edit fixed the pitch
  track.
- **Intensity made visible** (new). In F0 Correction, the f0 dots are
  sized by per-frame intensity (louder = bigger), with a size legend and
  per-frame dB on hover.
- **Curate** (new).
  [`apply_relabels()`](https://chenchenzi.github.io/citationtone_hub/reference/apply_relabels.md)
  re-labels tone-category variants (splits or mergers, colloquial
  vs. literary readings, sandhi) or excludes mis-elicited tokens without
  overwriting the original labels, surfaced through the new **Curate**
  tab.
- **TextGrid landmarks** (new). When Praat `.TextGrid` files are
  supplied, the F0 Extraction step can attach per-frame landmark columns
  from a chosen interval tier: `<tier>`, `<tier>_start`, `<tier>_end`,
  and `<tier>_i` (segment index). New functions:
  [`tg_interval_tiers()`](https://chenchenzi.github.io/citationtone_hub/reference/tg_interval_tiers.md),
  [`assign_tier_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/assign_tier_landmarks.md),
  and
  [`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md).
  The Visualise tab can then align contours by these landmarks,
  including syllable by syllable for multisyllabic words.
- **Landmark time normalisation** (new).
  [`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)
  rescales time within each segment, adding a within-segment 0–1 axis
  (`<tier>_t01`) and a sequential, word-level axis (`<tier>_tseq`). A
  [`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md)
  companion rescales the whole token to 0–1 (no landmarks needed, for
  monosyllabic data). Surfaced through a new “Time Normalisation”
  section on the Normalise tab. The model tabs default the Time variable
  to `<tier>_tseq` when present and steer multisyllabic analyses toward
  GAMM.
- **F0 Processing.** The Start preview now flags audio files too short
  to yield an f0 frame and skips them during extraction.
  [`flag_low_intensity()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_low_intensity.md)
  (the intensity-based inspection check) is now exported and documented.
- **Visualise palette** (new). The tone colour scale is a 12-colour set
  matched to the app theme, replacing the pale default so every tone
  reads clearly on a white background.
- **Faster start-up** (new). A one-time “loading analysis tools” toast
  appears the instant the page connects while the heavy analysis
  packages load in the background, so the landing page paints
  immediately.
- **Praat extraction script** (new). The bundled script writes readable
  text `.Pitch` files, samples per-frame intensity into the CSV, and
  resolves the chosen CSV output path correctly on Windows.

### shinytone 0.1.2

- New
  [`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md):
  a third inspection layer that compares each token’s overall level (its
  median f0, in semitones) against other tokens of the *same speaker and
  same tone* using a robust modified z-score (median/MAD; Iglewicz &
  Hoaglin 1993, cutoff 3.5). It flags smoothly shifted contours — e.g. a
  low-tone token mis-tracked up into the mid-tone band — that the pooled
  max/min check and the sample-level jump check both miss. Surfaced
  through
  [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  (new `level_threshold` and `min_tokens` arguments, `level too high` /
  `level too low` notes) and the Inspect tab.
- The `"norm"` (normalised-time) option was removed from `time_unit` in
  [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md),
  [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md),
  and the Inspect tab: the rate-of- change thresholds are physiological
  (ST per 10 ms) and have no meaning once real time is discarded.
  Inspection runs on real-time data (s / ms).
- Inspect-tab guide rewritten around three complementary layers, and the
  “Pitch-tracking quality check” workflow now marks Normalise as
  optional (inspection runs on raw f0).

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
