# Package index

## Launch the app

Run the bundled Shiny UI locally — same interface as the hosted version,
no upload limits, recordings stay on your machine.

- [`run_app()`](https://chenchenzi.github.io/citationtone_hub/reference/run_app.md)
  : Launch the Shinytone app locally

## Normalisation

By-speaker semitone or z-score f0 normalisation, plus landmark-based
time normalisation for multisyllabic words.

- [`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)
  : Normalise f0 by speaker
- [`normalise_time_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_landmarks.md)
  : Landmark-normalised time columns
- [`normalise_time_token()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_time_token.md)
  : Whole-token 0-1 time normalisation

## TextGrid landmarks

Read interval tiers from Praat TextGrids and attach per-frame segment
labels and boundaries (e.g. syllables) to long-format f0 data.

- [`tg_interval_tiers()`](https://chenchenzi.github.io/citationtone_hub/reference/tg_interval_tiers.md)
  : Interval-tier names across a set of TextGrids
- [`assign_tier_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/assign_tier_landmarks.md)
  : Assign each time to its interval in a TextGrid interval tier
- [`attach_landmarks()`](https://chenchenzi.github.io/citationtone_hub/reference/attach_landmarks.md)
  : Attach TextGrid landmark columns to a long-format f0 data frame

## Outlier and artefact inspection

Token-level outlier detection by speaker z-score and by speaker x tone
level, plus sample-level pitch-tracking artefacts and a low-intensity
check (Sundberg 1973, Steffman & Cole 2022).

- [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  : Inspect f0 data for token-level outliers and sample-level jumps
- [`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
  : Flag per-token f0 outliers using by-speaker z-scores
- [`flag_level_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_level_outliers.md)
  : Flag tokens whose overall f0 level is unusual for their speaker and
  tone
- [`flag_low_intensity()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_low_intensity.md)
  : Flag low-intensity f0 samples within tokens
- [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
  : Flag sample-to-sample f0 jumps within tokens

## Contour clustering

Unsupervised grouping of tokens by f0-contour shape to discover
candidate tone categories when the number of tones is unknown (Kaland
2023), with feature extraction and a choice of how many groups.

- [`cluster_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_f0.md)
  : Cluster f0 contours into candidate tone categories
- [`cluster_features()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_features.md)
  : Build per-token feature vectors for f0-contour clustering
- [`choose_k_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/choose_k_f0.md)
  : Diagnostics for choosing the number of clusters (candidate tones)
- [`cluster_mdl()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_mdl.md)
  : Recompute MDL information cost for stored clusterings at a given
  bending
- [`cluster_agreement()`](https://chenchenzi.github.io/citationtone_hub/reference/cluster_agreement.md)
  : Agreement between clusters and known tone labels

## Sonification

Render an f0 contour as an audible waveform (pure tone, complex tone, or
a source-filter synthesised vowel) so prototypical tones can be heard,
not only seen.

- [`sonify_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/sonify_f0.md)
  : Sonify an f0 contour

## Curate tone labels

Re-label tone-category variants or exclude tokens, without overwriting
the original labels.

- [`apply_relabels()`](https://chenchenzi.github.io/citationtone_hub/reference/apply_relabels.md)
  : Apply tone re-labels and exclusions to a long-format f0 table

## Contour modelling

Token-level polynomial fits, mixed-effects growth-curve analysis, and
GAMMs over tone contours, with AR1 correction and model-checking
diagnostics.

- [`fit_polynomial()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_polynomial.md)
  : Fit Legendre polynomials to f0 contours, token by token
- [`fit_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gca.md)
  : Fit a Growth Curve Analysis (GCA) model to f0 contours
- [`predict_gca()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gca.md)
  : Predict population-level f0 contours from a GCA fit
- [`fit_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/fit_gamm.md)
  : Fit a Generalised Additive Mixed Model (GAMM) to f0 contours
- [`predict_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/predict_gamm.md)
  : Predict population-level f0 smooth curves from a GAMM fit
- [`diagnose_gamm()`](https://chenchenzi.github.io/citationtone_hub/reference/diagnose_gamm.md)
  : Diagnose a fitted GAMM

## Chao tone numerals

Convert mean or predicted contours into Chao tone numerals
(reference-line, interval-based, robust FOR methods).

- [`compute_mean_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/compute_mean_contour.md)
  : Compute the per-tone mean f0 contour from long-format data
- [`contour_to_chao()`](https://chenchenzi.github.io/citationtone_hub/reference/contour_to_chao.md)
  : Convert per-tone contours to Chao tone numerals
- [`classify_contour()`](https://chenchenzi.github.io/citationtone_hub/reference/classify_contour.md)
  : Classify a Chao tone numeral string as a shape

## Bundled data

A small citation-tone corpus that ships with the package for examples,
vignettes, and the Shiny app’s “Try with our sample data” button.

- [`sample_f0`](https://chenchenzi.github.io/citationtone_hub/reference/sample_f0.md)
  : Sample f0 contour dataset

## Package overview

- [`shinytone`](https://chenchenzi.github.io/citationtone_hub/reference/shinytone-package.md)
  [`shinytone-package`](https://chenchenzi.github.io/citationtone_hub/reference/shinytone-package.md)
  : shinytone: A Citation Tone Research Hub
