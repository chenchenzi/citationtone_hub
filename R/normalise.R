#' Normalise f0 by speaker
#'
#' Add a normalised f0 column to `data`, computed per speaker. Two methods
#' are available: semitones referenced on a per-speaker mean (perceptually
#' uniform), or per-speaker z-scores (statistical centring and scaling).
#'
#' Per-speaker means can be computed in two ways:
#' * `"weighted"` (default): mean of per-tone means. Each tone contributes
#'   equally to the centre of the speaker's tonal space, regardless of how
#'   many tokens it has. Preferred for unbalanced designs.
#' * `"simple"`: arithmetic mean of all f0 observations for that speaker.
#'
#' @param data A data frame containing at least the f0, speaker, and tone
#'   columns.
#' @param f0 Name of the column with raw f0 values (Hz). Default `"f0"`.
#' @param speaker Name of the speaker ID column. Default `"speaker"`.
#' @param tone Name of the tone category column. Default `"tone"`. Used only
#'   when `mean_method = "weighted"`.
#' @param method Normalisation formula. One of:
#'   * `"semitone"` (default): `ST = 12 * log2(f0 / speaker_mean)`.
#'   * `"zscore"`: `Z = (f0 - speaker_mean) / sd(f0_speaker)`.
#' @param mean_method How the per-speaker mean is computed. One of
#'   `"weighted"` (default) or `"simple"`. See Details.
#'
#' @return The input data frame with two added columns:
#' * `speaker_mean` — the per-speaker mean used for normalisation
#' * `f0_st` (if `method = "semitone"`) or `f0_zscore` (if `method = "zscore"`)
#'
#' @examples
#' df <- data.frame(
#'   speaker = rep(c("S01", "S02"), each = 6),
#'   tone    = rep(c("T1", "T2", "T3"), times = 4),
#'   f0      = c(150, 180, 130, 160, 190, 140,
#'               200, 240, 170, 210, 250, 175)
#' )
#' head(normalise_f0(df, method = "semitone"))
#' head(normalise_f0(df, method = "zscore"))
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
