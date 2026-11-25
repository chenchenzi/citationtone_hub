#' shinytone: A Citation Tone Research Hub
#'
#' An interactive Shiny application and accompanying R functions for citation
#' tone research across tone languages. Integrates the full workflow of pitch
#' extraction, by-speaker f0 normalisation, growth-curve and generalised
#' additive mixed modelling, outlier inspection, and Chao tone numeral
#' summarisation.
#'
#' @section Online app:
#' The hosted version of the app is available at
#' <https://chenzixu.shinyapps.io/shinytone/>. Run the same UI locally with
#' [run_app()]: it has no upload limits, and recordings stay on your
#' machine.
#'
#' @section Programmatic API:
#' Standalone analytical functions (`normalise_f0()`, `fit_gca()`,
#' `fit_gamm()`, `contour_to_chao()`, ...) are being extracted from the Shiny
#' app in stages. See `NEWS.md` for the current state.
#'
#' @keywords internal
"_PACKAGE"
