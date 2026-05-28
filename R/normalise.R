#' Normalise f0 by speaker
#'
#' @description
#' Adds a within-speaker normalised f0 column to a long-format f0 data
#' frame. Speaker normalisation puts contours from male and female
#' speakers on a comparable scale, and is a near-universal pre-processing
#' step before cross-speaker tone modelling, plotting, or contour
#' summarisation (Rose 1987; Zhu 1999).
#'
#' Two formulae are available. Semitones above or below the speaker's
#' mean give a perceptually uniform scale. By-speaker z-scores combine
#' centring with unit-variance scaling. Both formulae rely on a
#' per-speaker mean of f0, which can itself be computed in two ways:
#' a simple arithmetic mean of all observations, or a mean of per-tone
#' means so that every tone contributes equally to the speaker's centre.
#'
#' @details
#' ## What the function does internally
#'
#' 1. Compute a per-speaker mean of `f0` using `mean_method` (see below).
#' 2. Join that mean back to `data` as a new column called `speaker_mean`.
#' 3. Apply the selected normalisation formula sample by sample and
#'    return the augmented data frame.
#'
#' ## Choosing `method`
#'
#' * `"semitone"` is the standard for citation-tone work. The scale is
#'   perceptually uniform (one semitone is roughly one just-noticeable
#'   pitch step), centred on the speaker's own range, and preserves the
#'   logarithmic structure of musical pitch perception.
#' * `"zscore"` is useful when both centring and scaling are desired,
#'   i.e., when you want to remove each speaker's baseline *and* equalise
#'   their f0 variability. Useful for cross-speaker statistical models
#'   where the raw variance differs strongly across speakers.
#'
#' ## Choosing `mean_method`
#'
#' * `"weighted"` (default) first computes a mean f0 per (speaker, tone)
#'   cell, then averages those per-tone means. Each tone contributes
#'   equally to the speaker's centre regardless of how many tokens it
#'   has. This is the right default for unbalanced designs in which
#'   token counts differ across tones, common in fieldwork and
#'   naturalistic corpora.
#' * `"simple"` takes the arithmetic mean of every f0 observation for a
#'   speaker. Faster, and equivalent to weighted when the design is
#'   balanced.
#'
#' ## NA handling
#'
#' All summary statistics use `na.rm = TRUE`. Sample-level `NA` f0
#' values propagate as `NA` in the normalised column.
#'
#' @param data A long-format data frame with one row per f0 sample. Must
#'   contain at least the f0, speaker, and (for `"weighted"` means) tone
#'   columns named below.
#' @param f0 Name of the column with raw f0 values, in Hz. Default `"f0"`.
#' @param speaker Name of the speaker ID column. Default `"speaker"`.
#' @param tone Name of the tone category column. Default `"tone"`. Only
#'   read when `mean_method = "weighted"`.
#' @param method Normalisation formula. One of:
#'   * `"semitone"` (default): `ST = 12 * log2(f0 / speaker_mean)`.
#'     Result is 0 at the speaker mean and ±12 at one octave above or
#'     below.
#'   * `"zscore"`: `Z = (f0 - speaker_mean) / sd(speaker_f0)`. Result has
#'     mean approximately 0 and SD approximately 1 within each speaker.
#' @param mean_method How the per-speaker mean used by `method` is
#'   computed: `"weighted"` (default; mean of per-tone means) or
#'   `"simple"` (arithmetic mean across all observations).
#'
#' @return The input data frame with two added columns:
#' * `speaker_mean`: the per-speaker mean used for normalisation.
#' * `f0_st` (if `method = "semitone"`) or `f0_zscore` (if
#'   `method = "zscore"`).
#'
#' @seealso
#' * [fit_polynomial()], [fit_gca()], [fit_gamm()] for downstream contour
#'   modelling that typically operates on the normalised column.
#' * [contour_to_chao()] for converting mean contours to Chao tone
#'   numerals, often called on `normalise_f0()` output.
#'
#' @references
#' Rose, P. (1987). Considerations in the normalisation of the
#' fundamental frequency of linguistic tone. \emph{Speech Communication},
#' 6(4), 343–352. \doi{10.1016/0167-6393(87)90008-1}
#'
#' Zhu, X. (1999). \emph{Shanghai Tonetics}. LINCOM Europa.
#'
#' Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation
#' tone production studies: Methodology and recommendations.
#' \emph{The Journal of the Acoustical Society of America}, 156(4),
#' 2538–2565. \doi{10.1121/10.0032356}
#'
#' @examples
#' data(sample_f0)
#'
#' out <- normalise_f0(sample_f0,
#'                     f0      = "f0_Hz",
#'                     speaker = "speaker",
#'                     tone    = "tone",
#'                     method  = "semitone")
#' head(out[, c("speaker", "tone", "f0_Hz", "speaker_mean", "f0_st")])
#'
#' # Same data, z-score instead of semitones:
#' out_z <- normalise_f0(sample_f0,
#'                       f0      = "f0_Hz",
#'                       speaker = "speaker",
#'                       tone    = "tone",
#'                       method  = "zscore")
#' head(out_z[, c("speaker", "f0_Hz", "speaker_mean", "f0_zscore")])
#'
#' @export
#' @importFrom dplyr group_by summarise left_join mutate ungroup
#' @importFrom rlang .data :=
#' @importFrom stats sd
normalise_f0 <- function(data,
                         f0          = "f0",
                         speaker     = "speaker",
                         tone        = "tone",
                         method      = c("semitone", "zscore"),
                         mean_method = c("weighted", "simple")) {

  method      <- match.arg(method)
  mean_method <- match.arg(mean_method)

  # --- Input validation ---------------------------------------------------
  required <- c(f0, speaker)
  if (mean_method == "weighted") required <- c(required, tone)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[f0]])) {
    stop("Column '", f0, "' must be numeric (f0 in Hz).", call. = FALSE)
  }

  # --- Per-speaker mean ---------------------------------------------------
  if (mean_method == "simple") {
    speaker_means <- data |>
      dplyr::group_by(.data[[speaker]]) |>
      dplyr::summarise(speaker_mean = mean(.data[[f0]], na.rm = TRUE),
                       .groups = "drop")
  } else {
    speaker_means <- data |>
      dplyr::group_by(.data[[speaker]], .data[[tone]]) |>
      dplyr::summarise(tone_mean = mean(.data[[f0]], na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::group_by(.data[[speaker]]) |>
      dplyr::summarise(speaker_mean = mean(.data$tone_mean, na.rm = TRUE),
                       .groups = "drop")
  }

  data <- dplyr::left_join(data, speaker_means, by = speaker)

  # --- Normalised column --------------------------------------------------
  norm_col <- if (method == "zscore") "f0_zscore" else "f0_st"

  if (method == "zscore") {
    data <- data |>
      dplyr::group_by(.data[[speaker]]) |>
      dplyr::mutate(!!norm_col := (.data[[f0]] - .data$speaker_mean) /
                                   stats::sd(.data[[f0]], na.rm = TRUE)) |>
      dplyr::ungroup()
  } else {
    data <- data |>
      dplyr::mutate(!!norm_col := 12 * log2(.data[[f0]] / .data$speaker_mean))
  }

  data
}
