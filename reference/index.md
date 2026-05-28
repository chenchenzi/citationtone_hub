# Package index

## Launch the app

Run the bundled Shiny UI locally — same interface as the hosted version,
no upload limits, recordings stay on your machine.

- [`run_app()`](https://chenchenzi.github.io/citationtone_hub/reference/run_app.md)
  : Launch the Shinytone app locally

## f0 normalisation

By-speaker semitone or z-score normalisation.

- [`normalise_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/normalise_f0.md)
  : Normalise f0 by speaker

## Outlier and artefact inspection

Token-level outlier detection by speaker z-score, plus sample-level
pitch-tracking artefacts (Sundberg 1973, Steffman & Cole 2022).

- [`inspect_f0()`](https://chenchenzi.github.io/citationtone_hub/reference/inspect_f0.md)
  : Inspect f0 data for token-level outliers and sample-level jumps
- [`flag_outliers()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_outliers.md)
  : Flag per-token f0 outliers using by-speaker z-scores
- [`flag_pitch_jumps()`](https://chenchenzi.github.io/citationtone_hub/reference/flag_pitch_jumps.md)
  : Flag sample-to-sample f0 jumps within tokens

## Contour modelling

Token-level polynomial fits, mixed-effects growth-curve analysis, and
GAMMs over tone contours.

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
