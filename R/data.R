#' Sample f0 contour dataset
#'
#' A subset of citation-tone f0 recordings, used as the running example
#' across vignettes, function `@examples` blocks, and the Shiny app's
#' "Try with our sample data" button. Available after
#' `library(shinytone)` thanks to `LazyData: true` in `DESCRIPTION`.
#'
#' @format A data frame with 38,808 rows and 12 columns:
#' \describe{
#'   \item{token}{Unique identifier for each recorded token / syllable.
#'     Rows belonging to the same contour share a token.}
#'   \item{index}{Sample index within each token, starting from 1.}
#'   \item{time}{Time in seconds within the token.}
#'   \item{f0_Hz}{Fundamental frequency in Hz.}
#'   \item{intensity}{Sample intensity (dB).}
#'   \item{speaker}{Speaker ID (13 speakers in total).}
#'   \item{char}{Chinese character corresponding to the syllable.}
#'   \item{position}{Position of the syllable in its source utterance.}
#'   \item{start_time}{Start time of the token within the source recording.}
#'   \item{tone}{Tone category (integer codes 1-6 in this dataset).}
#'   \item{ipa}{IPA transcription of the syllable's vowel.}
#'   \item{vowel}{Coarse vowel category.}
#' }
#'
#' @source Xu, C. (2025). Plastic Mandarin tones: regional identity in
#'   prosody. \emph{Phonetica}, 82(5), 331–362.
#'   \doi{10.1515/phon-2025-0001}
#'
#' @examples
#' data(sample_f0)
#' str(sample_f0)
#'
#' # Per-speaker token counts
#' table(sample_f0$speaker)
"sample_f0"
