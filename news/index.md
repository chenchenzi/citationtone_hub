# Changelog

## shinytone (development version)

- **Start page: rows without f0 are removed, and the gaps are marked.**
  A new *Data screening* box in the Start sidebar has token, time and f0
  pickers, and screens the chosen f0 variable when a file is uploaded
  (before an upload it shows its heading and a one-line note). Its
  *Remove N rows without f0 (recommended)* box is ticked by default, for
  pitch-tracker frames and equidistant points alike. The rows go before
  any tab sees the data; the uploaded file is not changed, and unticking
  the box brings them back. A new integer column, `voiced_run`, numbers
  the stretches with f0 in each token (1 up to the first gap, 2 after
  it, and so on), so the gaps stay marked once the rows are gone. Other
  boxes appear only when they have something to do: *Also remove runs of
  1–2 samples cut off by a gap at a token’s start or end* (unticked; not
  offered when every token has the same number of points, where such a
  gap flags its token); *Read 0 / -1 as no f0* (ticked), for an Hz
  column such as `f0_rapt` that
  [`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md)
  does not pick; and, when every row has f0 but the time steps show
  gaps, *Mark the N gaps in a voiced_run column* (ticked), which removes
  nothing. Without a token variable, the box counts the rows without f0
  over the whole file and asks for the token variable only when it needs
  one. One message on upload sums it up, with the 0 Hz count. The other
  tabs start on its f0 and token variables. New exported
  [`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
  (the counts),
  [`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
  (the removal),
  [`suggest_unvoiced()`](https://chenchenzi.github.io/citationtone_hub/reference/suggest_unvoiced.md)
  (values that may mark no f0) and
  [`hz_f0_columns()`](https://chenchenzi.github.io/citationtone_hub/reference/hz_f0_columns.md).
  [`landmark_prefixes()`](https://chenchenzi.github.io/citationtone_hub/reference/landmark_prefixes.md)
  reserves `voiced_run`.
- **Start page: a report on the upload.** Above the Data Preview, a
  *Your data* card gives the tokens, speakers, tones and rows, how the
  f0 is sampled (pitch-tracker frames, their step and how many tokens
  have a gap in the middle; N equidistant points per token with the
  tokens that have fewer or more; or uneven points), and the variables
  used. A *Data screening* card, the same for frames and equidistant
  points, gives the share of rows without f0, says where they are (at a
  token’s start or end, or inside it) and what the boxes do: how many
  rows removal takes (“Removed: 16 rows without f0”), or “All good:
  every row has f0” when there are none to take. A folded *Your raw file
  is not changed* note, shown whenever the app’s copy differs from the
  file, says to download that copy in Normalise or Curate. The card also
  names short runs cut off at a token’s edge, other Hz columns and the
  rows where they differ, rows deleted in F0 Correction, token-level
  columns (speaker, tone, metadata) that change within a token, and a
  time column whose 0-1 decision would change with the rows removed. A
  warning that gaps cannot be found from the time steps appears only
  when it matters: when neither rows without f0 inside the tokens nor a
  `voiced_run` column show the gaps, and never for equidistant points.
  Frames with the same number in every token count as points in both
  cards.
- **Start page: tokens with an issue are flagged, and can be dropped.**
  A new column, `screen_flag`, gives each flagged token’s reasons on
  every row (“” for the other tokens): no f0, or fewer than 3 samples
  with f0 (either reason stands alone), and, when every token has the
  same number of points N, no f0 at the first or last point, or points
  without f0 inside (a gap of any length). In such a file a token is
  flagged when it has fewer than N points with f0. The column is added
  when a token is flagged and the drop is unticked, and it replaces a
  `screen_flag` column the file already has (left out when no token is
  flagged now). One box, *Drop the N flagged tokens* (unticked), leaves
  them all out; it appears when a token is flagged. A token with no f0
  is flagged like the rest, whatever the removal box says, and counts in
  the box, the message and the report. When every flagged token has no
  f0 and removal is ticked, the report says that removal takes their
  rows. The report says what the flagged tokens share (“5 tokens
  flagged: fewer than 11 points with f0”). For equidistant points it
  lists them grouped by reason, one line each: points without f0 inside,
  no f0 at the first or last point, and fewer than 3 points with f0
  (tokens with no f0 among them), with at most 5 tokens per line. For
  frames it names them in brackets. The flags and the drop come before
  the removal of rows without f0, and the R code repeats them. New
  exported
  [`flag_f0_tokens()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_tokens.md).
  [`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
  also lists the tokens left with 1 or 2 samples (`few_samples`,
  `few_tokens`) and the tokens missing an end point (`edge_missing`,
  `edge_tokens`, `edge_rows`).
- **Normalise: *Download Normalised Data* saves every column** of the
  data it normalised, with the new columns at the end, as the other tabs
  get it. It used to save only the columns shown in the table, so an
  f0-only run had no token or time column.
- **Start page: what each tab does comes first.** The welcome text is
  followed by a folded *Continuing from the F0 Correction tab?* box and
  then the list of tabs, which now includes Cluster and Curate; *What
  the data look like* follows. Both Start pages (F0 Analysis and F0
  Processing) list their tabs with one Font Awesome icon each, on a pale
  circle coloured by the stage of the work (data, prepare, explore,
  model, summarise), in place of emoji.
- **F0 Processing Start page: a syllable tier for multisyllabic words.**
  Its companion-files note now says that a `.TextGrid` with a syllable
  tier is highly recommended for multisyllabic words, so the analysis
  can line up the syllables across tokens. The note is set off by a soft
  yellow bar, and *Already have f0 extracted with another tool?* is a
  folded guide box.
- **A larger top menu, and what a token is.** The top menu (About, F0
  Analysis, F0 Processing, Data Collection) is now 17 px, a step larger
  than the sub-tabs below it. Data Collection \> Filename says in one
  sentence what a token is: one recording of one syllable or word by one
  speaker, that is one `.wav` file and the rows of f0 data that share
  its token ID.
- **About page: an optional export step in workflow 3.** In
  *Pitch-tracking correction*, a dashed (optional) *F0 Extraction
  (export)* step now follows F0 Correction. F0 Correction saves the
  cleaned CSV directly. For another region (the whole token or a
  TextGrid interval) or sampling (native frames or equidistant points),
  export it from F0 Extraction instead, which picks up the corrections
  made in the session. Workflow 2 no longer shows the Start page as a
  step, like the others.
- **Pickers say “variable” throughout.** The Start page’s pickers (Data
  screening, contour duration and the metadata pickers) and the F0
  Processing tabs’ pickers now say “variable” (e.g. *f0 variable*,
  *Token ID variable*, *Time variable*, *Token / filename variable*), as
  the F0 Analysis tabs already did, and so does the text that names
  them. Text about a column the app adds, such as `voiced_run`, or a
  column of a metadata CSV still says column.
- **Gaps read from the time steps.** Once rows without f0 are removed, a
  gap shows only as a longer time step. A step more than 1.5 times the
  token’s usual step is a gap: a one-frame gap doubles the step, so a 2x
  rule would miss it. The usual step is the token’s median step or, in a
  token with fewer than 5 steps, the file’s common step when that is
  smaller. Steps are read only on a regular grid, where one step and two
  are clearly apart and the times are written finely enough, so points
  timed at each glottal pulse are not read. A token with two samples
  with f0 at the same time is set aside, and the other tokens are still
  read. Times written to the step itself, such as 10 ms frames written
  to 2 decimals, are read in whole units: a step of 0.02 s or more is a
  gap. This needs at least 99% of the steps to be exactly one unit. So
  such a file is not read from its time steps when more than 1 step in
  100 is a gap, or when its frames are not 10 ms apart (12.5 ms, say);
  when that matters, the Start page says how many decimals to save the
  times with (3 for 10 ms frames, 4 for 12.5 ms). A token where at least
  2 steps, and more than 1 in 8, are exactly 2 units is set aside. The
  same rule serves the Start page, the jump check,
  [`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md),
  [`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md)
  and
  [`flag_f0_gaps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_gaps.md).
- **0-1 time axes run from the first f0 to the last.** A 0-1 axis is
  there to compare contour shapes, and stretches without f0 carry no
  tone.
  [`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md)
  now runs each token from its first f0 (0) to its last f0 (1), and
  [`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)
  runs each segment of a tier, such as each syllable, from its first f0
  to its last; the TextGrid boundaries only say which rows belong to
  which syllable. Before, they ran from the token’s first row to its
  last and from each interval’s start boundary to its end, so a
  voiceless onset put the first f0 at, say, 0.2. `<tier>_tseq` puts the
  syllables in sequence: syllable 1’s f0 fills 0 to 1, syllable 2’s
  fills 1 to 2, and a stretch without f0 at the boundary takes no room.
  `token_t01` is the whole-word option: it keeps the gap between the
  syllables and their duration ratio. Rows before the first f0 or after
  the last are `NA`, not 0 or 1, and so are segments with f0 at fewer
  than two times, rows in empty-label intervals and rows outside the
  tier. So removing rows without f0 changes none of these columns. A
  lone sample cut off by a gap at a token’s edge still sets its 0 or 1;
  the Start page’s short-run box removes such samples. Both functions
  gain `f0` (default `"f0"`), and
  [`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)
  gains `token` (default `"token"`), as segments are taken within each
  token. In a column in Hz, 0 is no f0. A column with negative markers
  such as -1 is not read as Hz, so its -1 and 0 values count as f0: set
  them to `NA` first, or tick the Start page’s box that reads them as no
  f0. A missing column is now an error rather than a silent no-op,
  `<tier>_tseq` is no longer added without a `<tier>_i` column, and data
  with an `f0_corrected` column get a message to pass
  `f0 = "f0_corrected"`. **Columns saved by earlier versions keep their
  old values** and are still used as they are by their names: make them
  again in Normalise.
- **`<tier>_tbound`: syllables in sequence, with the gap kept.**
  [`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)
  adds a third column next to `<tier>_t01` and `<tier>_tseq`. The
  TextGrid boundaries between a word’s syllables are at 1, 2, …, so they
  line up across tokens, as on `<tier>_tseq`. A stretch without f0 at a
  boundary keeps its length, as on `token_t01`. The word’s first f0 is
  at 0 and its last f0 at n, the number of syllables, so stretches
  without f0 at the word’s edges take no room. A syllable with no f0 at
  all at the word’s start or end gets `NA`, and the word then starts at
  1 (or ends at n - 1). One GAMM smooth over `<tier>_tbound` can follow
  an f0 jump across the gap; on `<tier>_tseq` both sides of the jump sit
  at the same time. Removing rows without f0 or reordering the rows
  changes nothing. The column needs `<tier>_i`, and it needs `time` and
  the boundaries on the same clock, as F0 Extraction writes them: a
  token whose time lies outside its own intervals gets `NA`, with a
  message.
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
  keeps it on its own 0 to n scale, like `<tier>_tseq`; the other fits
  and
  [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md)
  rescale it per token from the first f0 to the last.
  [`landmark_prefixes()`](https://chenchenzi.github.io/citationtone_hub/reference/landmark_prefixes.md)
  keeps `<tier>_tbound` free for the tier.
- **The fits rescale time from the first f0 to the last too.**
  [`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md)
  gains `f0`, and
  [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md),
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md),
  [`fit_polynomial()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_polynomial.md)
  and
  [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md)
  pass their f0 column, so rows without f0 at a token’s edges no longer
  move the time axis. Fits, mean contours, Chao numerals and Timing are
  now the same with or without these rows, and a fit on the time column
  matches a fit on `token_t01`. On a test set of 1,853 tokens with rows
  without f0 at every edge, the Chao numerals with the rows kept were
  32, 212, 323, 54, 315/415 and 214; they are now 32, 212, 32, 45/55, 31
  and 213, as with the rows removed. These functions and
  [`cluster_features()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_features.md)
  also read 0 in an Hz column as no f0, as the axes do, rather than as a
  pitch of 0. Without `f0`,
  [`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md)
  keeps its first-row-to-last-row rescale.
- **Syllables in sequence: two rows at each boundary.** On
  `<tier>_tseq`, one syllable’s last f0 and the next one’s first f0
  share a whole number, often with an f0 jump between them.
  [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md)
  and
  [`cluster_features()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_features.md)
  never average the two: at the boundary and just after it they use the
  next syllable’s f0, and just before it the earlier one’s. They tell
  the rows apart by `<tier>_t01` or `<tier>_i`, so the row order does
  not matter. Other rows of a token at one time, such as two recordings
  that share a token name, are averaged, with a message that names the
  tokens. One GAMM smooth over `<tier>_tseq` cannot follow a jump at a
  boundary; the
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
  help suggests `<tier>_tbound` or `token_t01` for the whole word, or
  one token per syllable on `<tier>_t01`.
- **Landmark axes in auto mode.**
  [`time_already_normalised()`](https://chenchenzi.github.io/citationtone_hub/reference/time_already_normalised.md)
  gains `name`. A column named `<tier>_t01`, `token_t01` or `time_prop`
  whose values all lie in \[0, 1\] counts as already normalised by its
  name, so removing rows cannot switch the fits to a rescale.
  [`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md)
  and the app pass the name. A `<tier>_tseq` is still judged by its
  values. On the Model, GCA and GAMM tabs, a `<tier>_t01` with one part
  per token (a word, rhyme or vowel tier) counts as a whole-token axis,
  like `token_t01`: it gets the “already on 0-1” note, not the
  multisyllabic warning.
- **Normalise: three ways to put a word on one axis.** Time
  normalisation reads which rows have f0 from the f0 (Hz) variable the
  tab normalises, offers the token picker for tiers too, and no longer
  offers a 0-1 axis as the time variable. A tier adds `<tier>_t01`,
  `<tier>_tseq` and `<tier>_tbound`, and the message after the run names
  all three. The guide says what the new column is and why tokens need
  it, and names the tier columns by their endings
  (`<syllable_tier>_t01`, `_tseq`, `_tbound`; `token_t01` keeps its name
  whatever the token variable is called). Its pictures show one token in
  time on top, with a ruler for each axis, then, in a row, the three
  choices for a word, each with a card under it saying when to use it:
  *In sequence* (`_tseq`: the boundaries line up), *In sequence, gap
  kept* (`_tbound`: the boundaries line up and the gap is kept) and
  *Whole word* (`token_t01`: the gap and the syllables’ relative
  durations are kept, so the boundary moves from token to token). The
  layout follows the guide’s width: three in a row, then each picture
  beside its card, then each above it (on a phone); on narrow screens
  Token A switches to a narrower drawing. The *Whole word* card says to
  pick a word tier with only the target word labelled when a token holds
  more than the word, such as a carrier phrase. The first tip names
  every tab that takes the new column. The table shows the time, token
  and `<tier>_i` columns next to the new axes. When time and the
  boundaries are not on the same clock, a second message says why
  `<tier>_tbound` is `NA`.
- **Visualise: in sequence, gap kept.** *Align by TextGrid tier*
  (renamed from *Align time by landmark*) offers three layouts, named
  after the chosen tier: *In sequence (`<tier>_tseq`)*, *In sequence,
  gap kept (`<tier>_tbound`)* and *Overlaid (`<tier>_t01`)* (*In
  sequence* was *Side by side*). *In sequence, gap kept* is new and
  plots `<tier>_tbound`. Both layouts in sequence draw dashed lines at
  the boundaries and now name each part at its middle (“syllable 1”, not
  “1”). *Show R code* makes the same axis. A tier without `<tier>_i`
  offers *Overlaid* only; a tier with it offers all three again, with
  the layout picked before.
- **GAMM: a note for `<tier>_tbound`.** On a `<tier>_tbound` with
  several syllables per token, the GAMM tab says that the fit keeps its
  0 to n scale and that the gap is kept, so one smooth can follow an f0
  jump across it. The note on `<tier>_tseq`, where one smooth cannot
  follow such a jump, now suggests `<tier>_tbound` or `token_t01` for
  the whole word, and so do the red warning on a `<tier>_t01` and the
  multisyllabic guide. Model and GCA give a `<tier>_tbound` the same
  nudge to GAMM as a `<tier>_tseq`. With several syllables per token,
  the GAMM tab now starts on `<tier>_tbound`, where one smooth can
  follow an f0 jump at the boundary; Model and GCA still start on
  `<tier>_tseq`. When `<tier>_tbound` is empty for some tokens (time and
  the boundaries on different clocks), GAMM starts on `<tier>_tseq` too.
- **Jump check: the same flags with or without rows without f0.**
  [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md),
  and so
  [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  and the Inspect tab, now set aside rows without f0 (`NA`, 0 or a
  negative marker) and rows with no time, check the rest in time order
  and map the flags back. Rows set aside are never flagged, so the flags
  are the same with or without them and in any row order. Each sample is
  compared with the next sample that has f0, with the rate scaled by the
  real time step. On a regular grid, a jump across a gap gets ” across a
  N ms gap” in its note, where N is the step minus the usual step, so a
  one-frame gap in 10 ms frames reads as 10 ms. Across a gap, the flag
  goes on the shorter side when it is a run of at most 5 samples, since
  a short fragment cut off by a gap is the likelier error; otherwise on
  the side farther from the token median, as before. Negative values and
  `NA` times no longer stop the check.
  [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  gives a token with no f0 the note “no f0” and no flags, counts the
  level check’s speaker x tone groups over tokens with f0, and keeps a
  `voiced_run` column.
- **[`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md),
  [`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md)
  and
  [`flag_f0_gaps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_gaps.md)
  find gaps without the rows.** A run of f0 now also ends at a time step
  longer than 1.5 times the usual step (on a regular grid) and where a
  `voiced_run` column changes, so these functions give the same result
  with or without the rows without f0. Before, removing them could join
  a stray frame in the silence to the contour: 6 of the 1,853 test
  tokens gained 20 to 60 ms of duration. With `set`,
  [`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md)
  finds the gaps over the whole token, then counts each segment’s own
  rows. For a token with no rows without f0,
  [`flag_f0_gaps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_gaps.md)
  keeps the file’s own `n_missing` and `has_gap`, which may record rows
  removed earlier, instead of resetting them, and sets `has_gap` where
  the time steps or `voiced_run` show a gap. All three gain `run`
  (default `"voiced_run"`).
- **GCA: the polynomial basis is built on the rows the model fits.**
  [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
  resolves the time axis on all rows, then keeps the rows the model fits
  (a finite time, f0 and tone, and the speaker and item the model uses)
  and builds [`poly()`](https://rdrr.io/r/stats/poly.html) on those.
  Before, rows without f0 inside tokens shifted the basis and so the
  coefficients, and a landmark axis with `NA` time stopped with “missing
  values are not allowed in ‘poly’”. On data with no rows without f0,
  the fit is unchanged. Too few rows for the degree now give a clear
  error. The fit records `time_coverage`, each tone’s first and last
  fitted time, and
  [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md)
  carries it. The GCA tab’s *Show R code* keeps the same rows before
  [`poly()`](https://rdrr.io/r/stats/poly.html).
- **GAMM: the AR1 correction starts afresh at every token.** With
  `use_ar1 = TRUE`,
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
  put each token’s `AR.start` on its first row by time. When that row
  had no f0, as at the edges of a whole-file track, `bam()` left the row
  out and the marker with it, so the AR1 correction ran on from one
  token into the next. The marker now sits on each token’s first row
  that the model fits. On the test set, all 1,853 markers had been lost,
  which moved the curves by up to 1.2 LZ. On a `<tier>_tseq`,
  `time_range` and `n_segments` now come from the fitted rows, so
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
  spans the part of the axis the model was fitted to (0 to n on a
  `<tier>_tseq` from
  [`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)),
  not rows without f0 at the edges; its tones and reference speaker and
  item come from the fitted rows too. Rows at the same time keep their
  input order, so give each token’s rows in time order.
  [`diagnose_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/diagnose_gamm.md)
  whitens the residuals along the model’s own AR starts. The fit records
  `time_coverage`, which
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
  carries. The GAMM tab’s *Show R code* keeps the fitted rows, puts the
  marker on the first of them, estimates rho within tokens, and runs
  with a speaker-by-tone random smooth, where its prediction step used
  to fail.
- **[`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
  checks the time axis.** Time must run from 0 at the onset to 1 at the
  offset. Time outside \[0, 1\], such as milliseconds, point numbers or
  a GAMM across several segments of a `<tier>_tseq`, is now an error;
  such input used to give numerals read at the wrong points. A contour
  that starts after 0.25 or ends before 0.75 gives a warning. A factor
  time column is read by its labels. Rows with no tone, time or f0 no
  longer set the f0 range, and a tone with f0 at fewer than two time
  points is left out, with a warning. New columns `t_first` and `t_last`
  give where each tone’s contour starts and stops. When it misses 0 or
  1, its onset or offset is read from the nearest measured point, held
  flat, and `move`, `progress` and `timing` use that value; `progress`
  is still read at 0.5 on the time axis. Summarise marks such a tone
  with a dagger (†) and a note, and does the same for GCA and GAMM
  predictions whose fitted rows stop short of 0 or 1.
- **Summarise fixes.** An error in the mean contours or the Chao
  conversion, such as a text column chosen as Time, now shows a message
  and clears the old table and plot, instead of closing the app;
  warnings from the conversion show as messages too. The guide opens
  with “One syllable per token”: for a multisyllabic word, make each
  syllable its own token on `<syllable_tier>_t01` (numbered by
  `<syllable_tier>_i`), labelled by the word’s tone pattern and its
  place. When the tab declines Chao numerals for a GAMM fit across
  several segments, the message now says to model each syllable as its
  own token on `<syllable_tier>_t01`. The Timing guide says the middle
  of the tone is halfway between its first and last f0. The curated
  route takes its token and f0 variables from the Start page, and the
  menus keep the user’s picks when the data change. The Praat script
  page says that its CSV covers whole files, silence included, and that
  Data screening on the Start page removes the rows without f0 by
  default.
- **Model, GCA, GAMM and Summarise: a notice only when it matters.** An
  amber notice at the top of these tabs appears when a time column used
  as it is (already on 0-1) leaves some token’s f0 short of 0 or 1:
  `time_prop` over a region whose end points have no f0, or a 0-1 column
  made by an older version. It counts those tokens (or syllables, on a
  `<tier>_t01`), says whether removing rows changes it, and how to fix
  it. It never appears on the real time column, which the fits rescale
  from the first f0 to the last.
- **Token labels from the rows with f0.**
  [`fit_polynomial()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_polynomial.md),
  [`cluster_features()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_features.md),
  [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md),
  [`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
  and
  [`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md)
  take a token’s speaker and tone as the most frequent non-empty value
  on its rows with f0 (a tie goes to the value that comes first in
  time), not from its first row. A label column read from a TextGrid
  tier, such as a `tone` tier with `""` in the silences, can change
  within a token, so the first row used to depend on the row order and
  on the rows without f0.
  [`fit_polynomial()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_polynomial.md)’s
  checks for tokens with too few samples now count only rows with f0 and
  time.
- **Results from old rows are cleared.** A new upload already cleared
  every result computed from the previous file. Now so does a change on
  the Start page that changes the rows the tabs read: a box under *Data
  screening*, the screened variables, *Exclude discarded tokens*,
  metadata or duration columns. Normalised, inspected, clustered and
  curated + normalised data, GCA and GAMM predictions and each tab’s
  shown results are cleared, with a message that names them and the
  change. Curate’s relabels and exclusions are kept, as they are keyed
  by token. A change that leaves the rows as they were clears nothing.
- **Variable defaults follow the Start page.** The token and f0 menus of
  the F0 Analysis tabs default to the Start page’s token and f0
  variables, so an F0 Correction file is analysed on `f0_corrected`, not
  the tracked `f0`. Normalised columns still come first where a tab
  prefers them, and time defaults are as before. The Start page’s token
  picker never falls back to another column (a file of points with no
  token column was grouped by its point number), and its time picker
  leaves out `*_t01`, `*_tseq` and `*_tbound`. A menu keeps the user’s
  own pick. When a tab’s f0 variable is not the screened one and still
  has rows without f0, a note under the menu says so.
- **Show R code repeats the Start page.** The R code of Visualise,
  Model, GCA, GAMM and Summarise now rebuilds the rows the app used, in
  the order the Start page runs its steps: the read and
  [`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md),
  the exclusion of tokens discarded in F0 Correction (missing before),
  the metadata join (as code when the file and key are known, otherwise
  as a comment), the
  [`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md)
  lines, and
  [`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md)
  with `min_run` and `unvoiced` when used. Comments give the counts, and
  a commented
  [`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
  line reproduces the report. The code starts with
  `stopifnot(utils::packageVersion("shinytone") >= ...)` for the version
  the app runs on, since these functions need it. The Model, GCA and
  GAMM scripts rescale time from each token’s first f0 to its last, as
  the app does.
- **Inspect tab.** Shares of flagged samples are counted over the
  samples with f0, so they are the same whether or not the Start page
  removed rows. The time unit is set from the data (a median step within
  tokens below 1 means seconds) and shown under the buttons, e.g. “Frame
  step 10 ms”, with a warning when the chosen unit does not fit. Before,
  millisecond data left at “s” silently turned off the rate test. A
  failed run shows a message instead of closing the app. The guide
  describes the comparison with the next sample that has f0, the gap
  note and where the flag goes across a gap.
- **Plot lines break at gaps.** Curate’s plots and Visualise’s *Align by
  TextGrid tier* plot draw one line per voiced run when the data have
  `voiced_run`, so a line no longer bridges a gap where the Start page
  removed rows without f0. Curate’s selection, click and hover still
  work by token, and Visualise’s *Show R code* groups the lines the same
  way. The aligned plot now calls
  [`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md),
  so each part runs from its first f0 to its last, as in `<tier>_t01`
  and `<tier>_tseq`, and its *Show R code* prints that call. Which rows
  have f0 comes from the Start page’s f0 variable, even when Y is
  another variable. Alignment now needs a token variable.
- **F0 Extraction: *Drop rows without f0* marks the gaps.** The export
  option now uses
  [`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md),
  so the file carries a `voiced_run` column. With equidistant points or
  a TextGrid interval region, only the dropped rows mark gaps. The help
  says that Curate and Visualise break the line at each gap either way,
  and that other plotting tools join the line across the removed rows
  unless they group by `voiced_run`. A re-uploaded file’s `voiced_run`
  is not carried over: the export makes it again.
- **Changed arguments.** New arguments come last, so positional calls
  still work.
  [`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md),
  [`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md)
  and
  [`flag_f0_gaps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_gaps.md)
  gain `run = "voiced_run"`.
  [`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
  and
  [`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md)
  gain `time = NULL`, used only to break a tie between labels.
  [`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)
  gains `f0 = "f0"` and `token = "token"`, and
  [`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md)
  gains `f0 = "f0"`; a missing column is now an error.
  [`resolve_time_norm()`](https://chenchenzi.github.io/citationtone_hub/reference/resolve_time_norm.md)
  gains `f0 = NULL`, and
  [`time_already_normalised()`](https://chenchenzi.github.io/citationtone_hub/reference/time_already_normalised.md)
  gains `name = NULL`.
  [`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
  returns `t_first` and `t_last`, and stops on time outside \[0, 1\].
  [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
  stops with “Too few rows to fit” when fewer than `degree + 1` distinct
  time points remain.
  [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
  and
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
  return `time_coverage`, which
  [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md)
  and
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
  carry as an attribute. New exports:
  [`screen_f0_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/screen_f0_rows.md)
  (its `run` reads the gaps an earlier removal marked),
  [`drop_rows_without_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/drop_rows_without_f0.md),
  [`suggest_unvoiced()`](https://chenchenzi.github.io/citationtone_hub/reference/suggest_unvoiced.md)
  and
  [`hz_f0_columns()`](https://chenchenzi.github.io/citationtone_hub/reference/hz_f0_columns.md).
- **CSVs and TextGrids in UTF-16 and other encodings.** Praat saves
  tables and TextGrids as UTF-16 whenever any text is not plain ASCII,
  such as Chinese token names or IPA labels. A UTF-16 CSV used to close
  the app, and a UTF-16 TextGrid was silently ignored (no tiers, no
  TextGrid strip, empty landmark columns). Every upload now reads UTF-8,
  UTF-16 and UTF-32, with or without a byte-order mark. A file in an
  older code page is read as Chinese (GBK, Big5) or Western European
  text, with a note to check it. A file that cannot be read, such as an
  Excel workbook or a binary TextGrid, gives a message saying why, and
  the app keeps running. Praat’s `--undefined--` reads as NA. The new
  [`read_csv_utf8()`](https://chenchenzi.github.io/citationtone_hub/reference/read_csv_utf8.md)
  and
  [`read_textgrid()`](https://chenchenzi.github.io/citationtone_hub/reference/read_textgrid.md)
  read files the same way in R.
- **Landmark columns keep non-ASCII tier names.** Tiers named 声调 and
  音节 now give `声调_start` and `音节_start`, not `tier_start` and
  `tier_1_start`, and F0 Extraction no longer leaves the second one out
  of the download. A clash (two tiers with the same name once cleaned,
  or a tier named `time`) gets `_2`, `_3` in a fixed order, and the
  notification says so. ASCII tier names keep their old columns unless
  two differ only in case. New
  [`landmark_prefixes()`](https://chenchenzi.github.io/citationtone_hub/reference/landmark_prefixes.md);
  [`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md)
  gains `all_tiers`.
- **0 Hz f0 is read as missing.** Pitch trackers write 0 Hz for unvoiced
  frames. In a CSV uploaded in the Start tab these zeros counted as
  pitch values: they pulled contours, fits and plots towards 0 and gave
  `-Inf` in semitones, which could close the app in the Model tab. The
  Start tab now reads 0 Hz in f0 columns as NA and says how many rows
  changed; other columns, including normalised ones, are left alone.
  [`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)
  treats f0 of 0 or below as NA for every method, and the Model tab
  shows an error instead of closing. Show R code reads the file the same
  way. New
  [`zero_hz_to_na()`](https://chenchenzi.github.io/citationtone_hub/reference/zero_hz_to_na.md).
- **Chinese text in static plots.** Visualise, GCA, GAMM and Summarise
  plots and their downloads now show Chinese tone, speaker and tier
  names. When labels contain Chinese characters, the plot uses a Chinese
  font installed on the machine, so 阴平 and 阳平 no longer both appear
  as 平. If none is found, the app says so once. A font file placed in
  `inst/app/cjk-font/` is used first. Interactive plots are unaffected.
- **Chinese tone names in their usual order.** Legends, facets, tables
  and menus sorted 阴平 阳平 上声 去声 by code point, as 上声 去声 阳平
  阴平. Traditional tone names, in simplified or traditional characters,
  now sort 平 上 去 入 with 阴 before 阳, so GCA and GAMM fits take 阴平
  as the reference tone. A factor keeps its own levels, and other labels
  keep their order. New
  [`sort_tones()`](https://chenchenzi.github.io/citationtone_hub/reference/sort_tones.md).
- **GCA and GAMM plots: readable axis numbers.** In the app, the axis
  numbers of the GCA and GAMM prediction plots were tiny or missing, and
  the text was not in the app font. Both plots now use the same theme as
  the Visualise plot, on screen and in the downloaded PNG.
- **Small fixes.** The Start-tab preview says when a file could not be
  read and shows the real row count for files under 10 rows. Faceted
  Visualise plots have more room between panels.
- **Chao numerals: timing of rises and falls.** The numerals give pitch
  levels, not when a tone moves, so an early and a late rise between the
  same levels get the same digits.
  [`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
  now also returns `move` (the change from onset to offset), `progress`
  (the share of it done at 0.5 on the time axis, the middle of the tone)
  and `timing`: `"early"` above 0.75, `"late"` below 0.25 and `"mid"` in
  between. They are given for a rise or fall of at least half a unit
  with no sampled turning point, and are `NA` otherwise. The numerals
  themselves are unchanged. In the app, the results table has a Timing
  column, in red when the value at 0.5 is within the fuzzy margin of a
  cut-off, and the CSV and the R code carry it. On the sample data,
  tones 3 and 6 are late and tone 5 is mid.
- **Non-ASCII tone labels.**
  [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md)
  no longer stops with “Character encoding must be UTF-8, Latin-1 or
  bytes” when tone labels are non-ASCII text read by
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html), as the app
  reads uploads (for example Chinese tone names). This affected the
  Summarise and GCA tabs.
- **Summarise guide: shorter.** The Chao numeral guide is shorter, and
  its data source list marks GCA / GAMM predictions and normalised f0 as
  recommended with a thumbs-up. The sidebar help, the parameters line
  and the note on red values are shorter too.
- **Summarise: new Chao figure.** Both panels label every sampled point
  (onset, any dip or peak, offset) with its digit in the tone’s colour,
  moved apart with a short leader line when they would overlap. The
  reference-line panel marks lines 1 to 5 with digit chips at the right;
  the interval panel shows each band’s digit as a large watermark. The
  pink zones mark digit boundaries ± the fuzzy boundary threshold, with
  a legend entry. Colours are colour-blind safe, the two panels line up
  exactly, and the downloaded PNG uses Open Sans like the app. The plot
  code lives in `inst/app/ui/summarise_plot.R`.
- **Summarise: turning-point threshold.** A new “Turning-point
  threshold” input (default 0.5 Chao units, as before) sets how far a
  dip or peak must go beyond both endpoints to add a middle digit. The
  results table has a “Dip / peak” column with each tone’s dip or peak,
  in red when within the fuzzy margin of the threshold, and a note gives
  the numerals the other side of it would produce. On the sample data,
  tone 6 dips by 0.43, so it is 23 at the default and 213 at 0.4. The
  CSV and the R code carry the threshold.
  [`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
  now returns `dip` and `peak` columns and a `threshold` attribute, and
  checks `threshold`.
- **Chao numerals: a middle digit only where it shows the turn.** Each
  scale keeps the middle digit only if it differs from both endpoint
  digits. A middle digit that repeats an endpoint digit merges into it,
  as the turn is too shallow to show at five levels: `23`, not `223`
  (which reads as a late rise), and `22`, not `222`. The two scales
  round differently, so a tone can get two digits on one and three on
  the other;
  [`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
  gains `n_digits_int` for the interval numeral (`n_digits` is the
  reference-line one). When a contour both dips and peaks beyond its
  endpoints, the larger turn is now sampled (the dip used to be checked
  first), so a lower threshold never removes a digit. On the sample
  data, tone 2’s interval numeral changes from 112 to 12. In the app,
  the left-out value is shown in grey brackets in the Scaled column, a
  note lists those tones, the figure marks the point with a hollow dot,
  and the near-threshold flag now only marks tones whose numerals would
  change. Clearing the fuzzy boundary threshold box no longer stops the
  conversion, and the R code panel handles flat contours (zero range) as
  the app does.
- **Normalise again after Curate.** The Normalise tab can now read the
  Curated data (new “Data source” menu, shown once you have curated).
  This recomputes each speaker’s mean (and SD) on the kept tokens and
  the new tone labels, and creates a new **Curated + normalised data**
  set. Visualise, Model, GCA, GAMM and Summarise list it; Model, GCA and
  GAMM switch to it after each such run, and Summarise picks it unless
  model predictions exist. The curated data itself is not changed.
  Normalised columns left over from the first run are dropped, so only
  the column this run made remains (e.g. `f0_st`, not a stale `f0_lz`).
  The set is cleared if the curated tokens or labels change afterwards
  (or the curation is reset), with a note to normalise again;
  re-normalising the uploaded data does not clear it. Downstream tone
  and speaker menus default to the variables used in Curate and
  Normalise (e.g. `cluster`). Normalise’s variable menus now have their
  own input IDs; they shared `tone_var` and `speaker_var` with
  Visualise, so one tab’s choice could silently change the other’s. Once
  curated data exists, the Normalise tab starts on it; Curate’s route
  boxes add a step 4, “Normalise again”, and Model, GCA and GAMM show a
  reminder while “Curated data” is chosen. Path 5 of the About page
  workflow (and the paper’s copy, `paper/figures/workflow_overview`) now
  reads Curate, Normalise, Model, Summarise: the normalisation used for
  analysis comes after curation, and normalising in path 2 only helps
  Curate’s display. Figure boxes drop “(optional)”; the dashed border
  and legend say it.
- **A new upload starts afresh.** Uploading a file (including “Try with
  our sample data”) now clears everything computed from the previous
  file: normalised, inspected, clustered, curated and curated +
  normalised data, GCA/GAMM predictions, and each tab’s shown results
  (Normalise table, polynomial, GCA and GAMM fits, clustering, Chao
  conversion), with a note saying so (and a line under the Start tab’s
  sample-data button says it up front). Before, the previous file’s
  normalised data stayed in the menus, and Model, GCA and GAMM selected
  it by default, so trying the sample data and then uploading your own
  could model the sample. Curate’s relabels and exclusions are reset
  too; they used to carry over and, on a re-uploaded curated CSV,
  overwrote `<tone>_original`.
- **Summarise: normalise first.** The data source menu lists Normalised
  f0 (and Curated + normalised f0) first. The Hz routes are now labelled
  “Uploaded f0 (converted to LZ here)” and “Curated f0 (converted to LZ
  here)”, with a tip to normalise first so Chao numerals and models
  share one scale. Picking a z-score column shows a note that z-scores
  are linear in Hz; on the sample data, Hz or z-score against semitones
  or LZ changed 4 of 6 tones’ numerals. The curated route now uses the
  tone variable chosen in Curate (e.g. `cluster`).
- **[`cluster_features()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_features.md)
  resamples to 21 points by default (was 20)**, one every 5% of the
  stretch from each contour’s first to its last f0 value. This is the
  same default as the app’s Cluster tab,
  [`resample_f0_equal()`](https://chenchenzi.github.io/citationtone_hub/reference/resample_f0_equal.md)
  and
  [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md).
- **[`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md)
  resamples tokens instead of binning their samples.** It used to assign
  each sample to the nearest of 50 time bins and average the samples in
  each bin, so with unequal numbers of f0 points per token each bin
  averaged a different subset of tokens, and a token with more than 50
  points counted more than once in some bins. Every token is now
  interpolated at the same time points, by default 21 (one every 5%, as
  in
  [`cluster_features()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_features.md)),
  and counts at most once at each: nothing is extrapolated beyond a
  token’s first and last f0 value. Measured points that fall on the
  grid, such as 11 or 21 equally spaced points per token, keep their
  values when each token’s first and last points have f0, or on
  `time_prop`. `n_bins` is deprecated in favour of `n_points`. The mean
  contours of the Chao tab and the GCA tab’s observed overlay use the
  new method.
- **Summarise: the robust Chao method is removed.** Its range, one SD
  beyond the mean peak of the highest tone and the mean valley of the
  lowest, suited single tokens but not the averaged contours the
  numerals are read from: it squeezed them towards the middle digits, so
  5 and 1 were rarely reached. It also moved more with pitch-tracking
  errors than the range of the averaged contours used by the other two
  methods, and Zhu (1999), the source cited for it, had tested this
  range and found it worse than the plain extremes.
  [`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
  no longer returns the `robust` and `rob_continuous` columns or the
  `robust_stats` attribute; `raw_data`, `raw_token`, `raw_f0` and
  `raw_tone` are deprecated and ignored, with a warning. The Chao tab,
  its CSV download, plot and generated R code show the reference-line
  and interval methods.
- **Summarise: raw f0 is converted to LZ by speaker before Chao
  conversion.** The Raw f0 and Curated f0 sources averaged Hz over all
  speakers, so higher and lower voices were mixed in the mean contours.
  Each speaker’s f0 is now converted to LZ (log z-score, Zhu 1999)
  before the mean contours are taken. The Raw f0 source has a speaker
  selector (or none, for one speaker), and curated data find the speaker
  column by name. The summary says how many speakers were normalised and
  gives the ranges in LZ, the generated R code includes the step, and a
  raw source whose f0 has negative values is refused as already
  normalised. The README, the vignette and the
  [`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
  examples now normalise before converting.
- **Summarise: reference-line values near a rounding edge are flagged.**
  For the reference-line method, the red highlighting in the Chao table
  and the grey zones in its plot marked values near the reference lines
  1 to 5, where the digit is clear, instead of near the rounding edges
  at 1.5, 2.5, 3.5 and 4.5, where it is not. Both now mark the rounding
  edges, using the fuzzy boundary threshold chosen in the sidebar. The
  note under the table states that threshold instead of a fixed ±0.1,
  and the guide says where each method’s digit boundaries lie.
- **Summarise: generated R code finds turning points.** The R code shown
  under the Chao table now detects turning points as the app does, so a
  dipping or peaking tone gets its three-digit numeral there too; it
  used to sample only the onset and offset, so every numeral had two
  digits. The guide also states that the conversion is linear in the
  units of the input, so the input sets the pitch scale: semitones and
  LZ give a log-frequency scale, Hz and z-scores a linear one.
- **Summarise: IPA tone letters.** The Chao numeral table has a
  “Letters” column for each method (35 as ˧˥, 214 as ˨˩˦), and the CSV
  download has matching columns. The app bundles a 6 KB tone-letter
  subset of Charis SIL (renamed under its Open Font License,
  `inst/app/www/fonts/OFL.txt`), so a run of letters joins into one
  contour line for every visitor. The new
  [`chao_to_letters()`](https://chenchenzi.github.io/citationtone_hub/reference/chao_to_letters.md)
  converts numerals to letters in the package, one letter per digit, so
  a level tone keeps both (55 as ˥˥).
- **About: “Community & reach”.** A new section at the bottom of the
  About page shows the project’s GitHub stars, as a line built from the
  date of each star, and the views and downloads of the Zenodo archive.
  A daily GitHub Action (`.github/workflows/usage-stats.yaml`) collects
  these public figures into `usage.json` on the `usage-data` branch, and
  the app’s server reads that file (cached for 6 hours), so the page
  never waits on it, new figures appear without redeploying, and
  visitors’ browsers do not contact GitHub or Zenodo. Zenodo reports
  only running totals, so its line starts on the first day of recording.
- **Normalise: log z-score (LZ) option.** `normalise_f0(method = "lz")`,
  and “By-speaker log Z-score (LZ)” in the Normalise tab, add an `f0_lz`
  column: the z-score of log10 f0, `(log10 f0 - mean) / sd` with the
  speaker’s mean and SD of log10 f0 (Zhu 1999); any other base gives the
  same LZ. It works on ratios like semitones and also evens out
  speakers’ pitch ranges. It is the usual “z-score” in studies of tone
  in languages of China: 37 of the 136 studies in Xu & Zhang (2024) used
  it, against 4 that used a plain z-score. `speaker_mean` is then the
  geometric mean in Hz, and f0 of 0 or below counts as missing. The
  default stays `"semitone"`. In the Normalise and Inspect tabs, the “f0
  (Hz)” selector now prefers an Hz column over normalised ones. The
  About page no longer lists ERB, which
  [`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)
  never offered.
- **Normalise: “equally weighted by tone” now also weights the z-score
  SD, and the SD correction is a choice.** With
  `mean_method = "weighted"` (the default) every tone already counted
  once in the speaker’s centre, but the z-scores still divided by an SD
  pooled over all frames, so a tone with many tokens shaped the scale:
  two speakers with identical tones but 50/50 versus 90/10 token counts
  got LZ tone means of ±0.99 versus ±1.65. The SD is now weighted the
  same way (as
  [`stats::cov.wt()`](https://rdrr.io/r/stats/cov.wt.html)), for both
  `"zscore"` and `"lz"`. `mean_method = "simple"` keeps the classic Rose
  1987. z-score and Zhu (1999) LZ, and the two agree exactly when every
        tone has the same number of f0 values. A new `sd_correction`
        argument (default `TRUE`: Bessel’s correction, the n - 1 sample
        SD; `FALSE` divides by n), shown in the Normalise tab as a
        “Bessel’s correction for the SD (n − 1)” checkbox for the
        z-scores, makes that step visible. **This changes `"zscore"`
        output with the default weighted mean** whenever tones have
        unequal numbers of f0 values (unequal token counts, unvoiced
        frames, or tokens of different lengths): on `sample_f0`, where
        some speakers have a token or two more or fewer of some tones,
        each speaker’s SD moves by −0.9% to +2.2%. Semitones are
        unchanged, and `"zscore"` now returns the same class as its
        input (it used to return a tibble).
- **F0 Correction tab: TextGrid tier names are no longer cut off.** The
  plot’s left margin was fixed at 60 px, so a name longer than about
  seven characters lost its start (“phoneme” showed as “honeme”). The
  margin now fits the longest tier name across the uploaded TextGrids,
  the same for every token, up to 150 px; a longer name is shortened
  with “…” and shows in full on hover. The names come from the new
  [`tg_tier_names()`](https://chenchenzi.github.io/citationtone_hub/reference/tg_tier_names.md),
  which scans TextGrid text for tier headers (long or short format,
  UTF-8 or UTF-16) instead of parsing each file, so it is quick enough
  for a whole corpus. The selection band on the plot is also a lighter
  grey.
- **F0 Correction tab: play part of a token, as in Praat.** Select a
  stretch on the plot and it is shaded light grey across the waveform,
  f0 and TextGrid, staying shaded through redraws, zooming and panning.
  One selection serves both listening and editing: a box or lasso on the
  f0 dots selects the dots inside it, while a drag on the waveform or a
  click on a TextGrid interval (`Shift`-click, or drag along the tier,
  to span several) selects every dot in that stretch, so the dots the
  edits act on always lie inside the grey band. A click on one dot
  selects just that frame; a double-click or `Q` clears the selection.
  (Before, a drag on the waveform, or a box with no dots in it, left the
  earlier dots selected, so `Delete` could hit frames outside the
  stretch in view; and clicking the same dot again after selecting
  elsewhere was ignored.) Plotly’s dashed box now shows only while
  dragging. Then press `Tab` (or `P`), or **Selection** on the new play
  bar above the plot, to hear just that part; with no stretch selected
  it plays the visible part, which is Praat’s rule. `Tab` again stops,
  and a red playhead follows the playback. The play bar also has
  **Visible** and **Stop**, and reads out the selection’s start, end and
  duration. The audio player above still plays the whole file. Playback
  goes through the Web Audio API, so the slice is sample-accurate rather
  than limited by the ~250 ms granularity of an `<audio>` element’s time
  updates, and each slice fades in and out over 5 ms (and fades out when
  stopped part-way), so cutting the sound mid-wave does not click. The
  synthesised contour has its own **Selection** button, mapped onto its
  own clock, and `S` plays the same stretch from it (the corrected
  contour once you have edited), so the recording and the f0 track can
  be compared over the same stretch.
- **F0 Correction tab: the synthesised contour now plays in real time.**
  [`sonify_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/sonify_f0.md)
  synthesises only the stretch from the first to the last voiced frame,
  but the duration it was given was the span of every frame, silence
  included, so the voiced contour played slowed down to fill the whole
  file (1.7x on a token with 0.37 s of edge silence). It now runs at the
  voiced stretch’s own duration, so it lines up with the recording.
- **Model: GAMM tab keeps a `<tier>_tseq` axis on its own scale.** For
  multisyllabic words the guide routes you to fit on `syllable_tseq`
  (syllable 1’s f0 fills 0–1, syllable 2’s fills 1–2), but
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
  rescaled each token’s time to 0–1 like any other column. That moved
  the syllable boundaries away from whole numbers, and the plot showed a
  0–1 axis.
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
  now recognises a `_tseq` column and uses it as-is
  (`time_normalised = "sequential"` declares any such axis).
  [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
  predicts across the range of the rows it fits, and the fit records
  `time_axis`, `time_range` and `n_segments`. The GAMM plot keeps the
  0-to-n axis, marks each segment boundary with a dashed line, and
  labels the segments. The tab explains the axis when `_tseq` is chosen,
  and *Show R code* reproduces all of it. Summarise now declines Chao
  numerals for such a fit, since they describe one syllable on a 0–1
  axis.
- **Axis titles: `f₀ (st)` for semitones.** The GAMM plot labelled its y
  axis with the raw column name (`Predicted f0_st`); it now uses the
  same title as the other tabs (`Predicted f₀ (st)`). Semitone axes read
  `f₀ (st)` rather than `f₀ (semitone)` throughout: Visualise, Curate,
  Cluster, GCA and GAMM.
- **F0 Analysis Start tab: add a contour duration column.** A new *Add
  contour duration* block under *Attach metadata* adds how long each f0
  contour lasts: the time from the first to the last frame with f0, per
  token (`token_f0_dur`) or per landmark segment such as each syllable
  (`syllable_f0_dur`), repeated on every row of the unit. Unvoiced
  frames at the edges do not count, and unvoiced frames inside the
  contour do not shorten it. The ends are anchored on two voiced frames
  in a row with no gap between them, the rule the *Whole token* export
  region uses, so a lone stray frame in the silence cannot stretch the
  duration; a whole token’s value then equals the `voiced_s` that F0
  Extraction reports. The value is the same whether or not the Start
  page removes the rows without f0, and when it reads 0 or -1 as no f0
  in the same column, so does the duration. The column reaches every F0
  Analysis tab. Also scriptable as
  [`contour_duration()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_duration.md)
  (`min_run = 1` for the literal first-to-last frame), which counts
  negative values as measured, so it works on semitone or z-score
  columns too.
- **F0 Extraction tab: *Drop rows without f0* export option.** The
  *Whole token* region already leaves out leading and trailing silence
  but keeps unvoiced gaps inside the contour as `f0 = NA` rows. A new
  checkbox (off by default) drops those rows too, for tools that expect
  measured values only, and marks the gaps in `voiced_run` (see above).
  It runs last, so resampling still sees the gaps and `has_gap` /
  `n_missing` still flag the tokens. With equidistant points, a token
  with a gap then keeps fewer than N points, each still labelled by
  `point` and `time_prop`.
- **F0 Extraction tab: 0 Hz in an uploaded f0 CSV reads as unvoiced.**
  The wrassp and `.Pitch` paths already turned 0 Hz into `NA`, but a
  CSV’s zeros passed through as values and could reach the export inside
  the region.
- **F0 Processing: edit history survives the F0 Data Export round
  trip.** The export writes corrected values into `f0` but recorded
  nothing about which frames were corrected. Re-uploading it therefore
  loaded the corrected values as the tracker’s originals, and the next
  `all_correctedf0.csv` read `edited = FALSE` everywhere. When
  corrections exist, the export now also writes `f0_original` (the
  tracker’s value) and `edited`, and re-uploading either file restores
  the edits. This works whichever f0 variable the picker is set to:
  choosing `f0_corrected` from `all_correctedf0.csv` used to lose the
  history the same way. Re-uploading an older export without these
  columns now shows a note explaining that its corrections cannot be
  told apart. The export also stops copying correction columns from a
  re-uploaded file: a token restored in a later session kept a stale
  `token_dropped = TRUE`, so F0 Analysis silently excluded it.
- **F0 Correction tab: downloads carry landmark and metadata columns.**
  `all_correctedf0.csv` and `<token>_f0.csv` held only the f0 frames
  plus `f0_corrected` / `edited` / `token_dropped`, so the metadata
  (uploaded, or derived from filenames) and TextGrid landmark columns
  set up in F0 Extraction were missing after correction. Both downloads
  now add them the same way the F0 Data Export does. Re-uploading
  `all_correctedf0.csv` to resume keeps these columns, and attaching the
  same metadata again does not duplicate them: a metadata column the
  data already holds with the same values is skipped. This also drops
  the redundant `token.meta` column that filename-derived metadata used
  to add.
- **F0 Extraction tab: landmark tiers ticked after extraction reach the
  export.** Landmark columns were attached only when *Run extraction*
  was clicked, and the *Landmarks from TextGrid* picker sits below that
  button, so ticking `syllable` afterwards (or in *Upload existing f0
  CSV* mode) left the download without `syllable_start` / `syllable_end`
  / `syllable_i`. The Normalise tab lists a tier only when those columns
  exist, so multisyllabic data had no tier to build `<tier>_tseq` from.
  The export now attaches any ticked tier the data lacks. An amber note
  also warns when *Equidistant points* drops the landmark columns (their
  boundaries change within a multisyllabic token), and the Normalise tab
  explains why no tier is listed.
- **F0 Correction tab: two fixes from classroom use.** Deleting an
  outlying frame could leave its “ghost” marker (the original value)
  drawn off-screen: the f0 panel’s y-range was framed on the corrected
  values only, and the y-axis is fixed, so there was no way to scroll
  the original back into view. The range now also anchors on the
  original values of edited frames. And the Delete button sometimes
  appeared to do nothing: a click after a box/lasso selection silently
  kept the stale box (plotly never clears that input on a plain click),
  so the edit hit the old (often already deleted) frames. The selection
  now follows whichever of click or box-select happened last, and
  switching tokens clears it.
- **F0 Correction tab: a `note` column in the edit log.** A *Note
  (optional)* box in the sidebar. Whatever it holds is saved with the
  next edit or discard as the `note` column of the edit log (table, CSV
  download, and log re-upload alike), then cleared so it cannot leak
  onto later, unrelated actions. For recording *why*: a reason to
  discard a token, or why frames were removed.
- **F0 Extraction tab: export notes restyled as a banner.** The
  data-quality notes under the export summary (short voiced span,
  unvoiced gaps, …) were easy to miss as small grey text at the bottom
  of the sidebar; they are now an amber message box in the same idiom as
  the Correction tab’s banners.
- **Praat script: version guard for the newer pitch methods.** The
  *filtered ac / filtered cc / raw ac / raw cc* methods exist only since
  Praat 6.4 (November 2023); on an older Praat the batch died mid-run
  with a cryptic “Command not available for current selection” error.
  The script now checks `praatVersion` up front and exits with the
  running version and the fix (update Praat, or pick *ac* / *cc* /
  *shs*).
- **F0 Extraction tab: F0 Data Export (region + sampling).** A new block
  decides what the downloaded dataset contains, without touching the
  data in the app: F0 Correction keeps working on the extraction exactly
  as it arrived, so changing these settings can never disturb frame
  edits. **Region** is what the measurement covers: *Whole token* runs
  from a token’s first to its last voiced frame, so leading and trailing
  silence (which carries no f0) is excluded, and an edge needs two
  consecutive voiced frames so a lone voiced frame stranded in silence
  cannot stretch it; or a *TextGrid interval*. **Sampling** is how
  densely: every *native frame time*, or *N equidistant points* across
  the region, which is time normalisation applied at the sampling stage
  and adds `point` (1…N) and `time_prop` (0–1). An **export preview**
  shows the first rows of the file the Download button will write,
  metadata join included. New exported functions:
  [`resample_f0_equal()`](https://chenchenzi.github.io/citationtone_hub/reference/resample_f0_equal.md),
  [`trim_to_voiced()`](https://chenchenzi.github.io/citationtone_hub/reference/trim_to_voiced.md),
  [`flag_f0_gaps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_gaps.md).
- **Resampling follows Praat’s own rule.**
  [`resample_f0_equal()`](https://chenchenzi.github.io/citationtone_hub/reference/resample_f0_equal.md)
  ports `Sampled_getValueAtX()`, which is what
  `Pitch: Get value at time...` runs, so a contour resampled here
  matches one a Praat script gets querying the same times. Of the two
  frames bracketing a point, the nearer is *near* and the other *far*:
  both voiced blends them, *far* unvoiced keeps *near*’s measured value,
  *near* unvoiced gives `NA`. The `method` argument offers Praat’s two
  choices, `"linear"` (default) and `"nearest"`. Unvoiced stretches are
  respected only when the input marks them as `NA` (or 0 Hz) rows;
  sparse input such as a `.PitchTier` has no such rows, so a silent
  stretch there is a plain gap between anchors and is interpolated
  across, exactly as Praat does.
- **F0 Extraction tab: subset f0 by TextGrid interval.** Export only the
  f0 falling inside chosen intervals: vowels found automatically, the
  rhyme (first vowel to the end of the token; monosyllables only), or
  labels you type. New exported
  [`ipa_vowel_label()`](https://chenchenzi.github.io/citationtone_hub/reference/ipa_vowel_label.md)
  and
  [`filter_interval_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/filter_interval_rows.md).
  [`ipa_vowel_label()`](https://chenchenzi.github.io/citationtone_hub/reference/ipa_vowel_label.md)
  handles length marks, stress marks, tone digits and Chao letters,
  combining diacritics, precomposed pinyin tone marks (`ā ǎ ū ǔ` …) in
  either Unicode normalisation, di- and triphthongs including
  `j`/`w`/`ɥ` offglide spellings (`aj`, `ɔw`) as well as `ai` and `au`,
  and syllabic nasals (`m̩`, `n̩`, `ŋ̩`) as vowel-equivalent nuclei.
- **Two checks that make silent problems visible.**
  [`flag_f0_gaps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_f0_gaps.md)
  adds `n_missing` and `has_gap`, marking tokens whose voicing was
  interrupted mid-region, measured on the native frames *before*
  resampling, since resampling can fill a short dropout from the nearer
  frame. (On data whose rows without f0 were removed, it reads the gaps
  from the time steps or `voiced_run` and keeps the file’s own values;
  see above.) And the per-token summary reports `voiced_s` with
  `voiced_prop` (`voiced_s / duration_s`), shading and naming tokens
  voiced across a far shorter span than the rest of the corpus: with
  equidistant sampling their percentage positions are squeezed into
  whatever was tracked, so they are not comparable with the other
  tokens.
- **Modelling: an already-normalised time column is used as-is.**
  [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md),
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md),
  [`fit_polynomial()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_polynomial.md)
  and
  [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md)
  gained `time_normalised` (`"auto"`, `"no"`, `"yes"`). Under the
  default `"auto"`, a column that is already proportional (`token_t01`
  from
  [`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md),
  or `time_prop` from equal-point extraction) is detected by the new
  [`time_already_normalised()`](https://chenchenzi.github.io/citationtone_hub/reference/time_already_normalised.md)
  and used as-is, rather than rescaled a second time, which stretched
  any token not spanning the full unit interval. The Model, GCA and GAMM
  tabs say when this applies and their **Show R code** output reflects
  whichever branch ran;
  [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
  and
  [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
  record `time_prenormalised` in the returned object,
  [`fit_polynomial()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_polynomial.md)
  as an attribute. Columns named `<tier>_t01`, `token_t01` or
  `time_prop` are recognised by their name (see above). For other
  columns, detection is deliberately conservative and has two documented
  limits: a set in which *every* token is a partial span cannot be
  distinguished from ordinary variable-duration time and is rescaled as
  before (pass `"yes"`), and raw seconds in which every token lasts just
  under one second is detected as proportional (pass `"no"`).
- **0 Hz counts as unvoiced** in the new f0 helpers, matching
  [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  and the extraction paths, so trackers that code unvoiced frames as 0
  Hz are handled rather than having silence blended into speech.
- **The multisyllabic warning no longer fires on `token_t01`.** On the
  Model, GCA and GAMM tabs it is now raised only for a landmark tier’s
  `<tier>_t01` with more than one part per token, which resets at every
  segment boundary, and not for the whole-token proportional axis, which
  is a perfectly good time variable.
- **F0 Correction tab: whole-token discard.** A new “Whole token” edit
  group adds **Discard token** / **Restore token** for tokens that are
  beyond repair: instead of fixing frames, the whole token is marked as
  dropped. Non-destructive, since the f0 values are kept and both
  downloads gain a `token_dropped` column (`TRUE` for discarded tokens)
  to filter on downstream. Discarded tokens show a ✗ in the token picker
  and a banner above the plot, appear in the edit log (Undo restores, as
  does the Restore button), survive the save/re-upload resume cycle via
  the new column, and a “Kept + discarded / Only kept / Only discarded”
  filter joins the edit-status drawer. Keyboard: `X` discards the
  current token, or restores it if already discarded, so a review pass
  can run entirely on `,` `.` and `X`. The sidebar progress line and
  every discard notification report the running share of the corpus
  discarded, e.g. “Discarded: 812 of 8000 (10.2%)”.
- **F0 Correction tab: bulk discard of flagged tokens.** Once an
  Inspect-tab CSV is loaded in the filter drawer, a **Discard all
  flagged tokens** button marks the entire flagged set as discarded in
  one click. The confirmation dialog reports how many tokens that is and
  what share of the corpus they represent. This is the fast route for
  large corpora: discard the flagged set, then optionally review it
  (“Only discarded”) and Restore any worth repairing. A **Restore all**
  button next to the discard toggle un-discards *every* discarded token,
  bulk and manual alike, and removes their edit-log rows.
- **F0 Correction tab: discard-share breakdown by speaker and tone.**
  The corpus-wide discarded share can look harmless while the discards
  pile up in one speaker or tone; in the worst case a whole speaker ×
  tone cell is emptied and vanishes from the retained data. When the
  filter drawer’s speaker / tone columns are picked (auto-guessed from
  the uploaded Inspect CSV), the bulk-discard confirmation dialog shows
  what the discard set would look like per speaker × tone cell with
  marginals (“After this discard”), and a **Breakdown** button next to
  the discard toggle reopens the same table for the current state. Cells
  are `discarded/total (%)`, tinted red when a group would be fully
  discarded and amber at half or more, with tokens absent from the CSV
  grouped as “(no metadata)”. With only one of the two columns picked
  the table reduces to that margin; with neither, the dialog shows the
  total plus a hint to pick them.
- **GCA and GAMM guides: pointer to model structures the UI does not
  cover.** Both guide boxes now close with a **Beyond the built-in
  options** note. The checkboxes and dropdowns cover the structures most
  often used for tone contours, but `lme4` and `mgcv` support many more:
  by-speaker tone slopes `(1 + (ot1 + ot2) * tone | speaker)` or
  uncorrelated terms via `||` for GCA; tensor-product interactions such
  as `te(time_norm, duration)`, or smooths varying by a further factor,
  for GAMMs. The note directs users to **Show R code**, which already
  emits a runnable script reproducing the current fit, as the starting
  point for editing the model formula.
- **Model: Polynomials tab’s Show R code now reproduces the coefficient
  scatter.** The tab’s generated script used to stop at the coefficient
  table, leaving the coefficient-space scatter as the one part of the
  tab with no reproducible counterpart outside the app. The snippet now
  ends with a plotting section that rebuilds the scatter (one point per
  token, coloured by tone) using the axes currently selected in the tab
  (falling back to the plot’s own defaults) and axis labels carrying the
  phonetic gloss (`c1` slope, `c2` curvature, …). With no Z axis it
  emits a static `ggplot2` scatter; with a Z axis selected it emits the
  matching 3-D `plotly::plot_ly(type = "scatter3d")` call instead,
  mirroring what the tab shows.
- **F0 Correction tab: filter by flag type.** When the uploaded Inspect
  CSV carries `flag_notes`, a **Keep flag types** checkbox group lists
  the artefact classes present (extreme value, level, octave jump, jump
  by rate of change, carryover, low intensity). Unticking a type hides
  tokens that carry none of the ticked ones and narrows **Discard all
  flagged tokens** to the same subset, so a corpus can be worked one
  artefact class at a time (e.g. discard the octave jumps, review the
  level outliers by hand). It combines with the discard-status filter,
  so the discarded set can be reviewed one flag type at a time.
- **F0 Extraction: Praat is the default f0 source when pitch files are
  uploaded.** Uploading `.Pitch` / `.PitchTier` files alongside the
  audio now selects “Use uploaded .Pitch / .PitchTier (Praat)”
  automatically and says so, instead of leaving the radio on wrassp and
  silently extracting without Praat’s per-frame candidate lists. It
  fires once per session, so a later manual choice is never overridden.
- **F0 Correction: Praat candidates promoted to an edit group.** Picking
  a candidate writes to the contour, pushes undo history and logs an
  edit row, so the block now sits with the other edit groups (after
  Manual entry) rather than below the Display checkboxes. When no
  `.Pitch` data is loaded it shows a short hint explaining how to enable
  the option, so the feature is discoverable from a `.wav`-only session.
  The candidate list now spells out that `s` is Praat’s strength and
  that the tick marks the frame’s current value, and the empty state
  mentions that the grey numbered dots on the plot can be clicked
  directly. The Display checkbox “Top-3 Praat candidates on f0 plot” is
  greyed out and unticked when the data carries no candidates, rather
  than sitting ticked over an overlay that cannot appear, and its
  caption explains that dot 1 is the value Praat chose (2 and 3 are its
  next alternatives), that not every frame has three candidates, and
  that the unvoiced candidate is not numbered.
- **F0 Correction: plot mark legend.** A “Marks:” strip above the plot
  names every non-obvious mark: f0 value, selected frame, Praat
  candidates (dot 1 being Praat’s own pick, click to apply), the two
  sample-level flags in parallel wording (“jump or carryover”, red fill;
  “low intensity”, a bare amber ring that can sit on any fill colour),
  edited frames, and the outlined circle showing a frame’s value before
  the edit. Entries appear only when the corresponding data exists. When
  an Inspect CSV is loaded, a tinted status box under the key summarises
  the current token: light coral (dot-red border) whenever the token is
  flagged, with the frame counts and flag classes, or, for a token-level
  flag with no flagged frames (extreme value / level), a pointer to look
  at the whole contour; amber when the only signal is the advisory
  low-intensity ring; green with an explicit “nothing flagged by
  Inspect”. Flagged states add the reminder “Flags are leads, not
  errors: verify by eye and ear before editing” on its own line.
- **F0 Correction: low-intensity frames marked on the plot.** Frames the
  Inspect tab flagged as low intensity now carry an amber marker ring
  (reading `flag_low_intensity` when present, else the `flag_notes`
  text). Kept deliberately distinct from the red fill: red means a
  probable tracking error, the amber ring only means the f0 estimate
  there is less reliable. The ring co-exists with the red/blue fills,
  hover text says “low intensity (f0 here is less reliable)”, the legend
  names it “flagged by low intensity”, and the “Keep flag types”
  filter’s Low intensity class now has a visible counterpart on the
  plot.
- **F0 Correction: re-extraction no longer strands earlier edits.**
  After re-extracting on a different engine or frame grid (e.g. Praat
  first, then wrassp), edits made on the old grid were kept keyed by
  token name: the tab silently served the stale contour, and because the
  row counts no longer matched, ghost markers, the amber halo, and the
  “N frames edited” banner all vanished for that token while edits still
  appeared to apply. Frame edits that no longer align with the data are
  now cleared on re-extraction (with a notification); whole-token
  discards, which are name-keyed and still meaningful, survive.
- **F0 Correction: the idle reminder dismisses itself.** The “Still
  working?” note that appears after ten idle minutes used to stay until
  closed by hand; the first interaction after it fires now takes it down
  (it returns after the next ten idle minutes).
- **Curate tab: “Flagged” now covers every Inspect check.** The amber
  highlight, the “Flagged” quick-select, and the flagged-count chip now
  use `flagged_token` (any check: extreme max/min, unusual level,
  frame-level jumps) instead of only the “level too high / low” notes,
  so the exclude machinery can also serve as a whole-token disposal path
  for artefact-flagged tokens.
- **Start tab: discarded tokens honoured.** Uploading a CSV that carries
  the F0 Correction tab’s `token_dropped` column
  (e.g. `all_correctedf0.csv`) now pops a notification reporting how
  many tokens are marked discarded and excludes those rows (and the flag
  column) from the working dataset by default; a sidebar checkbox
  (“Exclude discarded tokens”) restores them. The uploaded file itself
  is never modified. Previously the discarded tokens flowed silently
  into every downstream tab (Normalise, Inspect, Visualise, the models,
  Summarise).
- **Bulk-discard guidance.** An illustrated “Big corpus, small flagged
  set? Bulk discard and review” guide joins the F0 Correction tab,
  walking through flagging in Inspect, discarding the set, and the
  optional review pass. When a corpus is large but lightly flagged (at
  least 1000 tokens with at most 15% flagged; thresholds in
  `offer_bulk_triage()`), the Inspect summary and the F0 Correction
  flagged-CSV loader suggest that route proactively.
- [`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
  (the speaker-level extreme-value screen) is now **one-sided**: a token
  is flagged `too_high` only when its per-token maximum is unusually
  high (`z_max > z_threshold`), and `too_low` only when its minimum is
  unusually low (`z_min < -z_threshold`), replacing the previous
  two-sided `abs(z) > z_threshold`. Gross tracking errors are
  directional (octave-doubling or a spurious spike inflates the maximum;
  octave-halving, a subharmonic, or creak deflates the minimum), and
  because the screen pools all of a speaker’s tones the opposite tails
  hold legitimate low/high tones rather than errors. This makes the
  `flag_too_high` / `flag_too_low` columns, and the “max too high” /
  “min too low” notes from
  [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md),
  directionally correct; before, a token with an unusually *high* floor
  could be mislabelled “too low”. The set of flagged tokens is
  essentially unchanged on clean data (the rarer truncated-max /
  floored-min cases are left to
  [`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md)
  and
  [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)).
- [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  now accepts `tone = NULL`, which skips the tone-relative token-level
  check
  ([`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md))
  and omits the `tone` column from the output, so it can run before tone
  categories are known (e.g. the clustering / tone-discovery workflow).
  The speaker-level extreme-value and sample-level jump checks still
  run, and the default remains `tone = "tone"`, so existing calls are
  unchanged.
- **Inspect tab: optional tone.** The tone selector now offers `(none)`,
  which runs the two tone-free screens (speaker-level extreme-value and
  sample-level jumps) without a tone column, for the pre-tone-discovery
  workflow. Backed by `inspect_f0(tone = NULL)`.
- **GAMM tab: on-demand diagnostics.** The “Run model diagnostics”
  button is shown in the sidebar from the start (disabled until a model
  is fitted), and diagnostics run when it is clicked rather than
  automatically after every fit, so fitting stays fast.
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
- **GCA fit-over-data overlay** (new). The GCA tab now optionally
  overlays the observed per-tone mean contour (semi-transparent points,
  computed with
  [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md))
  on the fitted polynomial curves, so you can judge how well the chosen
  degree tracks the data (the standard GCA model-adequacy check, Mirman
  2014). Toggle it with the “Overlay observed per-tone means” checkbox;
  turn it off for a cleaner plot on busy data. The overlay is included
  in the downloaded plot.
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
