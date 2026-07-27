# shinytone (development version)

* **F0 Correction tab: whole-token discard.** A new "Whole token" edit group
  adds **Discard token** / **Restore token** for tokens that are beyond
  repair: instead of fixing frames, the whole token is marked as dropped.
  Non-destructive, since the f0 values are kept and both downloads gain a
  `token_dropped` column (`TRUE` for discarded tokens) to filter on
  downstream. Discarded tokens show a ✗ in the token picker and a banner
  above the plot, appear in the edit log (Undo restores, as does the Restore
  button), survive the save/re-upload resume cycle via the new column, and a
  "Kept + discarded / Only kept / Only discarded" filter joins the
  edit-status drawer. Keyboard: `X` discards the current token, or restores
  it if already discarded, so a review pass can run entirely on `,` `.` and
  `X`. The sidebar progress line and every discard
  notification report the running share of the corpus discarded, e.g.
  "Discarded: 812 of 8000 (10.2%)".
* **F0 Correction tab: bulk discard of flagged tokens.** Once an Inspect-tab
  CSV is loaded in the filter drawer, a **Discard all flagged tokens** button
  marks the entire flagged set as discarded in one click. The confirmation
  dialog reports how many tokens that is and what share of the corpus they
  represent. This is the fast route for large corpora: discard the flagged
  set, then optionally review it ("Only discarded") and Restore any worth
  repairing. A **Restore all** button next to the discard toggle un-discards
  *every* discarded token, bulk and manual alike, and removes their edit-log
  rows.
* **F0 Correction tab: discard-share breakdown by speaker and tone.** The
  corpus-wide discarded share can look harmless while the discards pile up
  in one speaker or tone — worst case, a whole speaker × tone cell is
  emptied and vanishes from the retained data. When the filter drawer's
  speaker / tone columns are picked (auto-guessed from the uploaded Inspect
  CSV), the bulk-discard confirmation dialog shows what the discard set
  would look like per speaker × tone cell with marginals ("After this
  discard"), and a **Breakdown** button next to the discard toggle reopens
  the same table for the current state. Cells are `discarded/total (%)`,
  tinted red when a group would be fully discarded and amber at half or
  more, with tokens absent from the CSV grouped as "(no metadata)". With
  only one of the two columns picked the table reduces to that margin;
  with neither, the dialog shows the total plus a hint to pick them.
* **GCA and GAMM guides: pointer to model structures the UI does not cover.**
  Both guide boxes now close with a **Beyond the built-in options** note. The
  checkboxes and dropdowns cover the structures most often used for tone
  contours, but `lme4` and `mgcv` support many more — by-speaker tone slopes
  `(1 + (ot1 + ot2) * tone | speaker)` or uncorrelated terms via `||` for GCA;
  tensor-product interactions such as `te(time_norm, duration)`, or smooths
  varying by a further factor, for GAMMs. The note directs users to **Show R
  code**, which already emits a runnable script reproducing the current fit,
  as the starting point for editing the model formula.
* **F0 Correction tab: filter by flag type.** When the uploaded Inspect CSV
  carries `flag_notes`, a **Keep flag types** checkbox group lists the
  artefact classes present (extreme value, level, octave jump, jump by rate
  of change, carryover, low intensity). Unticking a type hides tokens that
  carry none of the ticked ones and narrows **Discard all flagged tokens** to
  the same subset, so a corpus can be worked one artefact class at a time
  (e.g. discard the octave jumps, review the level outliers by hand). It
  combines with the discard-status filter, so the discarded set can be
  reviewed one flag type at a time.
* **F0 Extraction: Praat is the default f0 source when pitch files are
  uploaded.** Uploading `.Pitch` / `.PitchTier` files alongside the audio now
  selects "Use uploaded .Pitch / .PitchTier (Praat)" automatically and says
  so, instead of leaving the radio on wrassp and silently extracting without
  Praat's per-frame candidate lists. It fires once per session, so a later
  manual choice is never overridden.
* **F0 Correction: Praat candidates promoted to an edit group.** Picking a
  candidate writes to the contour, pushes undo history and logs an edit row,
  so the block now sits with the other edit groups (after Manual entry)
  rather than below the Display checkboxes. When no `.Pitch` data is loaded
  it shows a short hint explaining how to enable the option, so the feature
  is discoverable from a `.wav`-only session. The candidate list now spells
  out that `s` is Praat's strength and that the tick marks the frame's
  current value, and the empty state mentions that the grey numbered dots on
  the plot can be clicked directly. The Display checkbox "Top-3 Praat
  candidates on f0 plot" is greyed out and unticked when the data carries no
  candidates, rather than sitting ticked over an overlay that cannot appear,
  and its caption explains that dot 1 is the value Praat chose (2 and 3 are
  its next alternatives), that not every frame has three candidates, and that
  the unvoiced candidate is not numbered.
* **F0 Correction: plot mark legend.** A "Marks:" strip above the plot names
  every non-obvious mark: f0 value, selected frame, Praat candidates (dot 1
  being Praat's own pick, click to apply), the two sample-level flags in
  parallel wording ("jump or carryover", red fill; "low intensity", a bare
  amber ring that can sit on any fill colour), edited frames, and the
  outlined circle showing a frame's value before the edit. Entries appear only when the corresponding
  data exists. When an Inspect CSV is loaded, a tinted status box under the
  key summarises the current token: light coral (dot-red border) whenever the
  token is flagged, with the frame counts and flag classes, or, for a
  token-level flag with no flagged frames (extreme value / level), a
  pointer to look at the whole contour; amber when the only signal is the
  advisory low-intensity ring; green with an explicit "nothing flagged by
  Inspect". Flagged states add the reminder "Flags are leads, not
  errors: verify by eye and ear before editing" on its own line.
* **F0 Correction: low-intensity frames marked on the plot.** Frames the
  Inspect tab flagged as low intensity now carry an amber marker ring
  (reading `flag_low_intensity` when present, else the `flag_notes` text).
  Kept deliberately distinct from the red fill: red means a probable
  tracking error, the amber ring only means the f0 estimate there is less
  reliable. The ring co-exists with the red/blue fills, hover text says
  "low intensity (f0 here is less reliable)", the legend names it "flagged
  by low intensity", and the "Keep flag types"
  filter's Low intensity class now has a visible counterpart on the plot.
* **F0 Correction: the idle reminder dismisses itself.** The "Still
  working?" note that appears after ten idle minutes used to stay until
  closed by hand; the first interaction after it fires now takes it down
  (it returns after the next ten idle minutes).
* **Curate tab: "Flagged" now covers every Inspect check.** The amber
  highlight, the "Flagged" quick-select, and the flagged-count chip now use
  `flagged_token` (any check: extreme max/min, unusual level, frame-level
  jumps) instead of only the "level too high / low" notes, so the exclude
  machinery can also serve as a whole-token disposal path for
  artefact-flagged tokens.
* **Start tab: discarded tokens honoured.** Uploading a CSV that carries the
  F0 Correction tab's `token_dropped` column (e.g. `all_correctedf0.csv`) now
  pops a notification reporting how many tokens are marked discarded and
  excludes those rows (and the flag column) from the working dataset by
  default; a sidebar checkbox ("Exclude discarded tokens") restores them.
  The uploaded file itself is never modified. Previously the discarded
  tokens flowed silently into every downstream tab (Normalise, Inspect,
  Visualise, the models, Summarise).
* **Bulk-discard guidance.** An illustrated "Big corpus, small flagged set?
  Bulk discard and review" guide joins the F0 Correction tab, walking through
  flagging in Inspect, discarding the set, and the optional review pass. When
  a corpus is large but lightly flagged (at least 1000 tokens with at most
  15% flagged; thresholds in `offer_bulk_triage()`), the Inspect summary and
  the F0 Correction flagged-CSV loader suggest that route proactively.
* `flag_outliers()` (the speaker-level extreme-value screen) is now
  **one-sided**: a token is flagged `too_high` only when its per-token
  maximum is unusually high (`z_max > z_threshold`), and `too_low` only when
  its minimum is unusually low (`z_min < -z_threshold`), replacing the
  previous two-sided `abs(z) > z_threshold`. Gross tracking errors are
  directional (octave-doubling or a spurious spike inflates the maximum;
  octave-halving, a subharmonic, or creak deflates the minimum), and because
  the screen pools all of a speaker's tones the opposite tails hold
  legitimate low/high tones rather than errors. This makes the
  `flag_too_high` / `flag_too_low` columns — and the "max too high" /
  "min too low" notes from `inspect_f0()` — directionally correct; before,
  a token with an unusually *high* floor could be mislabelled "too low". The
  set of flagged tokens is essentially unchanged on clean data (the rarer
  truncated-max / floored-min cases are left to `flag_level_outliers()` and
  `flag_pitch_jumps()`).
* `inspect_f0()` now accepts `tone = NULL`, which skips the tone-relative
  token-level check (`flag_level_outliers()`) and omits the `tone` column from
  the output, so it can run before tone categories are known (e.g. the
  clustering / tone-discovery workflow). The speaker-level extreme-value and
  sample-level jump checks still run, and the default remains `tone = "tone"`,
  so existing calls are unchanged.
* **Inspect tab: optional tone.** The tone selector now offers `— none —`,
  which runs the two tone-free screens (speaker-level extreme-value and
  sample-level jumps) without a tone column, for the pre-tone-discovery
  workflow. Backed by `inspect_f0(tone = NULL)`.
* **GAMM tab: on-demand diagnostics.** The "Run model diagnostics" button is
  shown in the sidebar from the start (disabled until a model is fitted), and
  diagnostics run when it is clicked rather than automatically after every
  fit, so fitting stays fast.
* **GAMM diagnostics** (new). `diagnose_gamm()` and a "Model diagnostics"
  section on the GAMM tab: after fitting, one click reports the
  basis-dimension check (k', edf, k-index, p-value, flagging under-resourced
  time smooths), the four `gam.check()` residual panels (Q–Q, residuals vs
  fitted, histogram, observed vs fitted), the residual ACF, and the
  concurvity table, with a text download of all diagnostics. The ACF is
  computed per token (in the spirit of `itsadug::acf_resid()`) and
  AR1-whitened when the fit used an AR1 correction, so it shows whether the
  correction actually worked.
* **GCA fit-over-data overlay** (new). The GCA tab now optionally overlays the
  observed per-tone mean contour (semi-transparent points, computed with
  `compute_mean_contour()`) on the fitted polynomial curves, so you can judge
  how well the chosen degree tracks the data — the standard GCA model-adequacy
  check (Mirman 2014). Toggle it with the "Overlay observed per-tone means"
  checkbox; turn it off for a cleaner plot on busy data. The overlay is
  included in the downloaded plot.
* The GAMM tab's **AR1 correction is now on by default**: densely-sampled f0
  frames are strongly autocorrelated, so smooth p-values are anticonservative
  without it. `fit_gamm()` (whose `use_ar1` argument still defaults to
  `FALSE`) now estimates `rho` from the lag-1 autocorrelation *within* tokens
  rather than across the flat concatenation, so token boundaries no longer
  bias the estimate.
* `run_app()` now checks GitHub once per launch for a newer shinytone release
  (2-second timeout, silent when offline) and, when one exists, prints the
  update command in the console and shows a one-time notification in the app.
  Disable with `options(shinytone.check_updates = FALSE)`.

# shinytone 1.0.0

First stable release. The Shiny app covers the citation-tone workflow end to end
(collect, extract, inspect, correct, curate, normalise, cluster, visualise,
model, Chao numerals), and the analytical functions behind each step are
exported and documented. Highlights since the 0.1.x line:

* **Contour clustering** (new). An unsupervised "tone discovery" workflow that
  groups tokens by f0-contour shape when the number of tone categories is
  unknown (Kaland 2023). New functions: `cluster_f0()` (k-means, hierarchical
  Ward, or Gaussian-mixture clustering), `cluster_features()` (represent each
  contour as resampled points, Legendre / DCT coefficients, or its derivative),
  `choose_k_f0()` (suggest the number of groups via silhouette, gap statistic,
  and a minimum-description-length cost), `cluster_mdl()`, and
  `cluster_agreement()` (adjusted Rand index against provisional labels).
  Surfaced through the new **Cluster** tab.
* **Contour sonification** (new). `sonify_f0()` renders an f0 contour as an
  audible waveform: a pure tone, a complex tone (12 harmonics, band-limited below
  Nyquist), or a source-filter synthesised vowel (`a` / `i` / `u`), with the
  pitch gliding along the contour. An optional `intensity` argument shapes the
  loudness envelope from per-frame dB. The Cluster tab's "Listen to the contours"
  panel plays each candidate cluster's mean contour back, so prototypical tones
  can be heard, not only seen (faithful Hz when an Hz column is present, or
  shape-only on a chosen base pitch).
* **Hear your corrections** (new). The F0 Correction tab sonifies the contour you
  are editing as an *Extracted vs Corrected* A/B pair, played at the token's own
  duration with loudness following the measured intensity, so you can hear
  whether an edit fixed the pitch track.
* **Intensity made visible** (new). In F0 Correction, the f0 dots are sized by
  per-frame intensity (louder = bigger), with a size legend and per-frame dB on
  hover.
* **Curate** (new). `apply_relabels()` re-labels tone-category variants (splits
  or mergers, colloquial vs. literary readings, sandhi) or excludes mis-elicited
  tokens without overwriting the original labels, surfaced through the new
  **Curate** tab.
* **TextGrid landmarks** (new). When Praat `.TextGrid` files are supplied, the
  F0 Extraction step can attach per-frame landmark columns from a chosen
  interval tier: `<tier>`, `<tier>_start`, `<tier>_end`, and `<tier>_i`
  (segment index). New functions: `tg_interval_tiers()`,
  `assign_tier_landmarks()`, and `attach_landmarks()`. The Visualise tab can
  then align contours by these landmarks, including syllable by syllable for
  multisyllabic words.
* **Landmark time normalisation** (new). `normalise_time_landmarks()` rescales
  time within each segment, adding a within-segment 0–1 axis (`<tier>_t01`) and
  a sequential, word-level axis (`<tier>_tseq`). A `normalise_time_token()`
  companion rescales the whole token to 0–1 (no landmarks needed, for
  monosyllabic data). Surfaced through a new "Time Normalisation" section on the
  Normalise tab. The model tabs default the Time variable to `<tier>_tseq` when
  present and steer multisyllabic analyses toward GAMM.
* **F0 Processing.** The Start preview now flags audio files too short to yield
  an f0 frame and skips them during extraction. `flag_low_intensity()` (the
  intensity-based inspection check) is now exported and documented.
* **Visualise palette** (new). The tone colour scale is a 12-colour set matched
  to the app theme, replacing the pale default so every tone reads clearly on a
  white background.
* **Faster start-up** (new). A one-time "loading analysis tools" toast appears
  the instant the page connects while the heavy analysis packages load in the
  background, so the landing page paints immediately.
* **Praat extraction script** (new). The bundled script writes readable text
  `.Pitch` files, samples per-frame intensity into the CSV, and resolves the
  chosen CSV output path correctly on Windows.

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
