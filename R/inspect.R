# =============================================================================
# F0 outlier and artefact detection.
#
# Three exported entry points:
#   * flag_outliers()      token-level outliers by speaker z-score
#   * flag_pitch_jumps()   sample-to-sample jump / octave / carryover flags
#   * inspect_f0()         composes both, returns the long-format tagged df
#     used by the Shiny Inspect tab.
# =============================================================================


#' Flag per-token f0 outliers using by-speaker z-scores
#'
#' For each token (a unique value of the `token` column), compute the maximum
#' and minimum f0. Within each speaker, z-score those per-token extremes. A
#' token is flagged whenever the absolute z-score of either its max or its
#' min exceeds `z_threshold`.
#'
#' @param data A long-format data frame, one row per f0 sample.
#' @param f0 Column name of f0 (Hz). Default `"f0"`.
#' @param token Column name of token ID. Default `"token"`.
#' @param speaker Column name of speaker ID. Default `"speaker"`.
#' @param z_threshold Absolute z-score above which a token is flagged.
#'   Default `3`, covering 99.7% of a normal distribution.
#'
#' @return A *token-level* data frame (one row per token) with columns:
#'   `f0_token_max`, `f0_token_min`, `f0_token_mean`, `f0_token_sd`,
#'   `z_max`, `z_min`, `flag_too_high`, `flag_too_low`, plus the original
#'   `token` and `speaker` columns.
#'
#' @examples
#' df <- data.frame(
#'   token   = rep(c("t1", "t2", "t3"), each = 4),
#'   speaker = rep("S01", 12),
#'   f0      = c(150, 160, 155, 158,
#'               152, 159, 154, 157,
#'               400, 410, 405, 408)   # t3 is clearly a tracking error
#' )
#' flag_outliers(df, z_threshold = 1.5)
#'
#' @export
#' @importFrom dplyr filter group_by summarise distinct left_join mutate ungroup all_of select
#' @importFrom rlang .data
#' @importFrom stats sd
flag_outliers <- function(data,
                          f0          = "f0",
                          token       = "token",
                          speaker     = "speaker",
                          z_threshold = 3) {
  required <- c(f0, token, speaker)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[f0]])) {
    stop("Column '", f0, "' must be numeric (f0 in Hz).", call. = FALSE)
  }

  # Per-token f0 summaries (excluding NA and zero, which pitch trackers
  # often emit for unvoiced frames).
  token_stats <- data |>
    dplyr::filter(!is.na(.data[[f0]]) & .data[[f0]] != 0) |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::summarise(
      f0_token_max  = max(.data[[f0]], na.rm = TRUE),
      f0_token_min  = min(.data[[f0]], na.rm = TRUE),
      f0_token_mean = mean(.data[[f0]], na.rm = TRUE),
      f0_token_sd   = stats::sd(.data[[f0]], na.rm = TRUE),
      .groups = "drop"
    )

  token_speaker <- data |>
    dplyr::distinct(.data[[token]], .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(c(token, speaker)))

  token_stats |>
    dplyr::left_join(token_speaker, by = token) |>
    dplyr::group_by(.data[[speaker]]) |>
    dplyr::mutate(
      z_max = (.data$f0_token_max - mean(.data$f0_token_max, na.rm = TRUE)) /
                stats::sd(.data$f0_token_max, na.rm = TRUE),
      z_min = (.data$f0_token_min - mean(.data$f0_token_min, na.rm = TRUE)) /
                stats::sd(.data$f0_token_min, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      flag_too_high = !is.na(.data$z_max) & abs(.data$z_max) > z_threshold,
      flag_too_low  = !is.na(.data$z_min) & abs(.data$z_min) > z_threshold
    )
}


# -----------------------------------------------------------------------------
# Internal carryover helper.
# Given the sample-wise jump flags (already shifted to the landing sample),
# extend each flagged jump forward as long as following samples stay within
# `mult * threshold` semitones of the error frame's f0 (in semitones).
# The band uses rise_threshold when the immediate next sample is HIGHER and
# fall_threshold when it is LOWER, matching Steffman & Cole (2022).
# Setting mult <= 0 disables carryover entirely.
# -----------------------------------------------------------------------------
detect_carryover_chain <- function(flagged_jump, f0_st,
                                   rise_threshold, fall_threshold, mult) {
  n <- length(flagged_jump)
  carryover      <- rep(FALSE, n)
  carryover_note <- rep("",    n)
  if (n < 2 || mult <= 0) {
    return(list(carryover = carryover, carryover_note = carryover_note))
  }

  error_f0 <- NA_real_
  for (i in 2:n) {
    if (isTRUE(flagged_jump[i]) && !is.na(f0_st[i])) {
      error_f0 <- f0_st[i]
    } else if (!is.na(error_f0) && !is.na(f0_st[i]) &&
               (isTRUE(flagged_jump[i - 1]) || carryover[i - 1])) {
      band <- if (i < n && !is.na(f0_st[i + 1])) {
        if (f0_st[i + 1] > f0_st[i]) rise_threshold * mult
        else                          fall_threshold * mult
      } else {
        max(rise_threshold, fall_threshold) * mult
      }
      if (abs(f0_st[i] - error_f0) <= band) {
        carryover[i]      <- TRUE
        carryover_note[i] <- "carryover"
      } else {
        error_f0 <- NA_real_   # chain broken
      }
    }
  }
  list(carryover = carryover, carryover_note = carryover_note)
}


#' Flag sample-to-sample f0 jumps within tokens
#'
#' Within each token, compute the semitone difference between consecutive
#' f0 samples. Flag samples where the rate of change exceeds physiological
#' plausibility (Sundberg 1973), where the Hz ratio crosses an octave
#' boundary, or where the sample is a *carryover* of an upstream error
#' (Steffman & Cole 2022).
#'
#' Rise / fall thresholds are expressed as semitones per 10 ms; the
#' function rescales the observed rate using `time_unit` to compare against
#' them. Zero-valued f0 samples (unvoiced markers from pitch trackers) are
#' treated as missing before any computation.
#'
#' @param data A long-format data frame, one row per f0 sample.
#' @param f0 Column name of f0 (Hz). Default `"f0"`.
#' @param token Column name of token ID. Default `"token"`.
#' @param time Column name of time. Default `"time"`.
#' @param time_unit One of `"s"`, `"ms"`, `"norm"`. Default `"s"`. The
#'   "norm" option treats each step as one 10 ms-equivalent unit.
#' @param rise_threshold Maximum plausible rise in ST per 10 ms. Default
#'   `1.263` (Sundberg 1973).
#' @param fall_threshold Maximum plausible fall in ST per 10 ms. Default
#'   `1.714` (Sundberg 1973).
#' @param octave_bounds Hz-ratio bounds outside which a step is flagged as
#'   an octave jump. Default `c(0.49, 1.99)` (halving / doubling).
#' @param carryover_mult Carryover band as a multiple of the rise/fall
#'   threshold (in semitones). `0` disables carryover. Default `1.5`.
#'
#' @return The input data frame with two appended columns:
#'   `flagged_jump` (logical) and `jump_note` (character; one of
#'   `"jump (rise)"`, `"jump (fall)"`, `"octave jump"`, `"carryover"`, or
#'   compound notes joined by `"; "`).
#'
#' @export
#' @importFrom dplyr arrange group_by ungroup mutate lead lag case_when group_modify any_of select all_of
#' @importFrom rlang .data :=
flag_pitch_jumps <- function(data,
                             f0             = "f0",
                             token          = "token",
                             time           = "time",
                             time_unit      = c("s", "ms", "norm"),
                             rise_threshold = 1.263,
                             fall_threshold = 1.714,
                             octave_bounds  = c(0.49, 1.99),
                             carryover_mult = 1.5) {
  time_unit <- match.arg(time_unit)

  required <- c(f0, token, time)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[f0]])) {
    stop("Column '", f0, "' must be numeric (f0 in Hz).", call. = FALSE)
  }
  if (!is.numeric(data[[time]])) {
    stop("Column '", time, "' must be numeric.", call. = FALSE)
  }

  # Treat f0 == 0 as NA (pitch trackers emit 0 for unvoiced).
  data <- data |>
    dplyr::mutate(!!f0 := ifelse(.data[[f0]] == 0, NA_real_, .data[[f0]]))

  # Per-sample diffs.
  result <- data |>
    dplyr::arrange(.data[[token]], .data[[time]]) |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::mutate(
      .f0_st     = 12 * log2(.data[[f0]]),
      .lead_st   = dplyr::lead(.data$.f0_st,    order_by = .data[[time]]),
      .lead_hz   = dplyr::lead(.data[[f0]],     order_by = .data[[time]]),
      .diff_st   = .data$.lead_st - .data$.f0_st,
      .diff_time = dplyr::lead(.data[[time]], order_by = .data[[time]]) -
                   .data[[time]],
      .hz_ratio  = .data$.lead_hz / .data[[f0]]
    ) |>
    dplyr::ungroup()

  # Time conversion to semitones-per-10ms equivalent.
  result <- result |>
    dplyr::mutate(
      .time_factor = dplyr::case_when(
        time_unit == "ms"   ~ ifelse(is.na(.data$.diff_time) | .data$.diff_time == 0,
                                     1, .data$.diff_time / 10),
        time_unit == "s"    ~ ifelse(is.na(.data$.diff_time) | .data$.diff_time == 0,
                                     1, (.data$.diff_time * 1000) / 10),
        time_unit == "norm" ~ 1
      )
    )

  # Jump flags, then shift to the LANDING sample (where the error actually
  # appears).
  result <- result |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::mutate(
      .oct_jump  = !is.na(.data$.hz_ratio) &
                   (.data$.hz_ratio < octave_bounds[1] |
                    .data$.hz_ratio > octave_bounds[2]),
      .jump_rise = !is.na(.data$.diff_st) & .data$.diff_st > 0 &
                   (abs(.data$.diff_st) / .data$.time_factor) > rise_threshold,
      .jump_fall = !is.na(.data$.diff_st) & .data$.diff_st < 0 &
                   (abs(.data$.diff_st) / .data$.time_factor) > fall_threshold,
      .flag_landing = dplyr::lag(.data$.oct_jump | .data$.jump_rise | .data$.jump_fall,
                                 default = FALSE),
      .note_landing = dplyr::case_when(
        dplyr::lag(.data$.oct_jump,  default = FALSE) &
        dplyr::lag(.data$.jump_rise, default = FALSE) ~ "octave jump; jump (rise)",
        dplyr::lag(.data$.oct_jump,  default = FALSE) &
        dplyr::lag(.data$.jump_fall, default = FALSE) ~ "octave jump; jump (fall)",
        dplyr::lag(.data$.oct_jump,  default = FALSE) ~ "octave jump",
        dplyr::lag(.data$.jump_rise, default = FALSE) ~ "jump (rise)",
        dplyr::lag(.data$.jump_fall, default = FALSE) ~ "jump (fall)",
        TRUE ~ ""
      )
    ) |>
    dplyr::ungroup()

  # Per-token carryover extension.
  result <- result |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::group_modify(~ {
      co <- detect_carryover_chain(.x$.flag_landing, .x$.f0_st,
                                   rise_threshold, fall_threshold,
                                   carryover_mult)
      .x$.carryover      <- co$carryover
      .x$.carryover_note <- co$carryover_note
      .x
    }) |>
    dplyr::ungroup()

  # Final flag + note combining jumps and carryover.
  result <- result |>
    dplyr::mutate(
      flagged_jump = .data$.flag_landing | .data$.carryover,
      jump_note = dplyr::case_when(
        nchar(.data$.carryover_note) > 0 & nchar(.data$.note_landing) > 0 ~
          paste0(.data$.note_landing, "; ", .data$.carryover_note),
        nchar(.data$.carryover_note) > 0 ~ .data$.carryover_note,
        TRUE ~ .data$.note_landing
      )
    )

  # Drop all the dotted internal columns.
  result |>
    dplyr::select(-dplyr::any_of(c(".f0_st", ".lead_st", ".lead_hz",
                                   ".diff_st", ".diff_time", ".hz_ratio",
                                   ".time_factor", ".oct_jump",
                                   ".jump_rise", ".jump_fall",
                                   ".flag_landing", ".note_landing",
                                   ".carryover", ".carryover_note")))
}


#' Inspect f0 data for token-level outliers and sample-level jumps
#'
#' Wrapper that runs [flag_outliers()] (by-speaker z-score outlier detection
#' on per-token max / min) and [flag_pitch_jumps()] (sample-to-sample jumps,
#' octave jumps, carryover), then combines their results into a single
#' long-format data frame with one row per f0 sample.
#'
#' @inheritParams flag_outliers
#' @inheritParams flag_pitch_jumps
#' @param time Column name of time. Default `"time"`.
#' @param tone Column name of tone category. Default `"tone"`.
#'
#' @return A long-format data frame containing the original `token`, `time`,
#'   `f0`, `speaker`, `tone` columns plus:
#'   * `f0_token_max`, `f0_token_min`, `f0_token_mean`, `f0_token_sd` —
#'     per-token summaries
#'   * `flagged_jump` — logical, sample-level jump flag
#'   * `flagged_token` — logical, TRUE if the token has any extreme value
#'     or any sample-level jump
#'   * `flag_notes` — human-readable concatenation of the reasons a sample
#'     was flagged
#'
#' @export
#' @importFrom dplyr left_join all_of group_by ungroup mutate select
#' @importFrom rlang .data
inspect_f0 <- function(data,
                       f0             = "f0",
                       token          = "token",
                       time           = "time",
                       speaker        = "speaker",
                       tone           = "tone",
                       z_threshold    = 3,
                       rise_threshold = 1.263,
                       fall_threshold = 1.714,
                       octave_bounds  = c(0.49, 1.99),
                       carryover_mult = 1.5,
                       time_unit      = c("s", "ms", "norm")) {
  time_unit <- match.arg(time_unit)

  outliers <- flag_outliers(data, f0 = f0, token = token, speaker = speaker,
                            z_threshold = z_threshold)

  result <- flag_pitch_jumps(data, f0 = f0, token = token, time = time,
                             time_unit      = time_unit,
                             rise_threshold = rise_threshold,
                             fall_threshold = fall_threshold,
                             octave_bounds  = octave_bounds,
                             carryover_mult = carryover_mult)

  # Long-format result + per-token flags.
  result <- result |>
    dplyr::left_join(
      outliers |> dplyr::select(dplyr::all_of(token),
                                "f0_token_max", "f0_token_min",
                                "f0_token_mean", "f0_token_sd",
                                "z_max", "z_min",
                                "flag_too_high", "flag_too_low"),
      by = token
    )

  result <- result |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::mutate(has_jump_in_token = any(.data$flagged_jump, na.rm = TRUE)) |>
    dplyr::ungroup()

  result <- result |>
    dplyr::mutate(
      flag_notes = {
        notes <- rep("", dplyr::n())
        notes <- ifelse(.data$flag_too_high, "max too high", notes)
        notes <- ifelse(.data$flag_too_low,
                        ifelse(nchar(notes) > 0,
                               paste0(notes, "; min too low"),
                               "min too low"),
                        notes)
        notes <- ifelse(nchar(.data$jump_note) > 0,
                        ifelse(nchar(notes) > 0,
                               paste0(notes, "; ", .data$jump_note),
                               .data$jump_note),
                        notes)
        notes
      },
      flagged_token = .data$flag_too_high | .data$flag_too_low |
                      .data$has_jump_in_token
    )

  # Final selected columns, matching what the Shiny Inspect tab expects.
  result |>
    dplyr::select(
      dplyr::all_of(c(token, time, f0, speaker, tone)),
      "f0_token_max", "f0_token_min", "f0_token_mean", "f0_token_sd",
      "flagged_token", "flagged_jump", "flag_notes"
    )
}
