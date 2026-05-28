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
#' @description
#' Identifies tokens whose maximum or minimum f0 lies far from the centre
#' of a speaker's per-token f0 distribution. Suitable as a first pass at
#' detecting tracking errors, mis-segmentations, or genuinely unusual
#' productions before fitting contour models. Token-level outlier removal
#' is a recommended cleaning step prior to citation-tone analysis
#' (Xu & Zhang 2024).
#'
#' @details
#' ## What the function does internally
#'
#' 1. Drop samples where `f0` is `NA` or `0` (pitch trackers commonly
#'    output 0 for unvoiced frames).
#' 2. Compute the per-token maximum, minimum, mean, and SD of `f0`.
#' 3. Within each speaker, z-score the per-token maxima against each
#'    other and the per-token minima against each other (so a speaker
#'    with consistently high f0 isn't flagged as an outlier of the corpus).
#' 4. Flag a token if `|z_max| > z_threshold` (the per-token max is too
#'    high or too low for this speaker) or `|z_min| > z_threshold`.
#'
#' ## Choosing `z_threshold`
#'
#' The default of `3` corresponds to the standard convention that ±3 SDs
#' covers 99.7% of a normal distribution, so under that assumption only
#' about 0.3% of tokens are flagged. Lower thresholds (e.g., `2` or `2.5`)
#' are more aggressive, useful when manual review of every flagged token
#' is feasible. Higher thresholds (`4`+) are more conservative.
#'
#' Token z-scoring per speaker assumes each speaker contributes enough
#' tokens for the SD to be meaningfully estimated; results for speakers
#' with only a handful of tokens should be treated as advisory.
#'
#' @param data A long-format data frame with one row per f0 sample.
#' @param f0 Column name of f0 in Hz. Default `"f0"`.
#' @param token Column name of token ID. Default `"token"`.
#' @param speaker Column name of speaker ID. Default `"speaker"`.
#' @param z_threshold Absolute z-score above which a token is flagged.
#'   Default `3`, covering about 99.7% of a normal distribution.
#'
#' @return A token-level data frame (one row per token) with columns:
#'   `f0_token_max`, `f0_token_min`, `f0_token_mean`, `f0_token_sd`,
#'   `z_max`, `z_min`, `flag_too_high`, `flag_too_low`, plus the original
#'   `token` and `speaker` columns.
#'
#' @seealso
#' * [flag_pitch_jumps()] for complementary sample-level artefact
#'   detection (octave jumps, rate-of-change violations).
#' * [inspect_f0()] for the convenience wrapper that runs both and joins
#'   the results.
#'
#' @references
#' Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation
#' tone production studies: Methodology and recommendations.
#' \emph{The Journal of the Acoustical Society of America}, 156(4),
#' 2538–2565. \doi{10.1121/10.0032356}
#'
#' @examples
#' data(sample_f0)
#' out <- flag_outliers(sample_f0,
#'                      f0      = "f0_Hz",
#'                      token   = "token",
#'                      speaker = "speaker",
#'                      z_threshold = 3)
#' table(out$flag_too_high, useNA = "ifany")
#' table(out$flag_too_low,  useNA = "ifany")
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
#' @description
#' Detects sample-level pitch-tracking artefacts inside each token.
#' Three classes of artefact are flagged: rate-of-change violations
#' (faster rises or falls than human vocal folds can plausibly produce),
#' octave jumps (pitch halving or doubling), and carryover samples that
#' follow a flagged frame and stay close enough to the error to be
#' suspected of being part of the same artefact.
#'
#' Used as the sample-level counterpart of [flag_outliers()] in the
#' Inspect tab of the Shiny app and inside [inspect_f0()].
#'
#' @details
#' ## What the function does internally
#'
#' 1. Treat f0 = 0 as `NA` (pitch trackers commonly mark unvoiced frames
#'    with 0 Hz).
#' 2. Within each token, compute the semitone difference between
#'    consecutive f0 samples and the Hz ratio between them.
#' 3. Rescale the observed rate of change to a semitones-per-10-ms
#'    equivalent using `time_unit`, so the result can be compared to a
#'    fixed threshold (Sundberg 1973).
#' 4. Flag the *landing* sample (i.e., the sample immediately after the
#'    jump) for octave jumps, threshold-violating rises, and
#'    threshold-violating falls.
#' 5. Extend each flagged sample forward as a chain of carryover frames
#'    that stay within `carryover_mult * threshold` semitones of the
#'    error sample's f0 (Steffman & Cole 2022).
#'
#' ## Choosing the thresholds
#'
#' Default `rise_threshold` and `fall_threshold` come from Sundberg's
#' (1973) classic study of the maximum rate of f0 change achievable in
#' singing. Changes exceeding these rates are physiologically
#' implausible and almost always tracking errors. Tightening these
#' values catches more borderline cases at the cost of more false
#' positives.
#'
#' Default `octave_bounds = c(0.49, 1.99)` flags any successive Hz ratio
#' more extreme than approximate halving or doubling, the dominant
#' failure mode of autocorrelation-based pitch trackers.
#'
#' Setting `carryover_mult = 0` disables the carryover extension and
#' flags only the landing sample of each jump. The default of `1.5`
#' follows Steffman & Cole (2022).
#'
#' @param data A long-format data frame with one row per f0 sample.
#' @param f0 Column name of f0 in Hz. Default `"f0"`.
#' @param token Column name of token ID. Default `"token"`.
#' @param time Column name of time. Default `"time"`.
#' @param time_unit One of `"s"`, `"ms"`, `"norm"`. Default `"s"`. The
#'   `"norm"` option treats each step as one 10 ms-equivalent unit.
#' @param rise_threshold Maximum plausible rise in ST per 10 ms.
#'   Default `1.263` (Sundberg 1973).
#' @param fall_threshold Maximum plausible fall in ST per 10 ms.
#'   Default `1.714` (Sundberg 1973).
#' @param octave_bounds Hz-ratio bounds outside which a step is flagged
#'   as an octave jump. Default `c(0.49, 1.99)` (halving or doubling).
#' @param carryover_mult Carryover band as a multiple of the rise/fall
#'   threshold (in semitones). `0` disables carryover. Default `1.5`.
#'
#' @return The input data frame with two appended columns:
#' * `flagged_jump`: logical, `TRUE` for samples flagged as artefacts or
#'   carryover frames.
#' * `jump_note`: character describing each flag (e.g., `"jump (rise)"`,
#'   `"jump (fall)"`, `"octave jump"`, `"carryover"`, or compound notes
#'   joined by `"; "`).
#'
#' @seealso
#' * [flag_outliers()] for the complementary token-level outlier check.
#' * [inspect_f0()] for the wrapper that joins both.
#'
#' @references
#' Steffman, J., & Cole, J. (2022). Pitch tracking artefacts and the
#' detection of voicing errors in spontaneous speech.
#'
#' Sundberg, J. (1973). The acoustics of the singing voice.
#' \emph{Scientific American}, 229(3), 82–91.
#'
#' Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation
#' tone production studies: Methodology and recommendations.
#' \emph{The Journal of the Acoustical Society of America}, 156(4),
#' 2538–2565. \doi{10.1121/10.0032356}
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
  # `time_unit` is a scalar argument fixed for the whole call, so we
  # branch with if/else rather than case_when() (which dplyr 1.2.0
  # deprecates for scalar LHS inputs).
  time_factor <- if (time_unit == "ms") {
    ifelse(is.na(result$.diff_time) | result$.diff_time == 0,
           1, result$.diff_time / 10)
  } else if (time_unit == "s") {
    ifelse(is.na(result$.diff_time) | result$.diff_time == 0,
           1, (result$.diff_time * 1000) / 10)
  } else {
    # "norm": treat each step as one 10 ms-equivalent unit
    rep(1, nrow(result))
  }
  result$.time_factor <- time_factor

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
#' @description
#' One-call convenience wrapper that runs both [flag_outliers()] and
#' [flag_pitch_jumps()] on the same dataset, joins their results back
#' into one long-format data frame, and produces a single
#' `flag_notes` column with human-readable reasons for each flag. This
#' is the function that backs the Inspect tab of the Shiny app.
#'
#' @details
#' ## What the function does internally
#'
#' 1. Call [flag_outliers()] to get token-level outlier flags
#'    (`flag_too_high`, `flag_too_low`) plus per-token summary
#'    statistics.
#' 2. Call [flag_pitch_jumps()] to get sample-level pitch-tracking
#'    artefact flags (`flagged_jump`, `jump_note`).
#' 3. Left-join the token-level flags onto the long-format jump output,
#'    so every sample carries both kinds of information.
#' 4. Set `flagged_token` to `TRUE` for any token that has a max/min
#'    z-outlier or contains at least one sample-level jump.
#' 5. Concatenate the human-readable reasons into a single
#'    `flag_notes` column for display.
#'
#' Use the individual functions ([flag_outliers()], [flag_pitch_jumps()])
#' if you want only one kind of check or want to combine the outputs
#' yourself in a non-standard way.
#'
#' @inheritParams flag_outliers
#' @inheritParams flag_pitch_jumps
#' @param time Column name of time. Default `"time"`.
#' @param tone Column name of tone category. Default `"tone"`.
#'
#' @return A long-format data frame containing the original `token`,
#'   `time`, `f0`, `speaker`, `tone` columns plus:
#' * `f0_token_max`, `f0_token_min`, `f0_token_mean`, `f0_token_sd`:
#'   per-token summary statistics.
#' * `flagged_jump`: logical, sample-level jump flag.
#' * `flagged_token`: logical, `TRUE` if the token has any extreme value
#'   or any sample-level jump.
#' * `flag_notes`: human-readable concatenation of the reasons a sample
#'   was flagged.
#'
#' @seealso
#' * [flag_outliers()] and [flag_pitch_jumps()], the two components.
#' * [normalise_f0()] for the upstream normalisation step.
#'
#' @references
#' Steffman, J., & Cole, J. (2022). Pitch tracking artefacts and the
#' detection of voicing errors in spontaneous speech.
#'
#' Sundberg, J. (1973). The acoustics of the singing voice.
#' \emph{Scientific American}, 229(3), 82–91.
#'
#' Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation
#' tone production studies: Methodology and recommendations.
#' \emph{The Journal of the Acoustical Society of America}, 156(4),
#' 2538–2565. \doi{10.1121/10.0032356}
#'
#' @examples
#' data(sample_f0)
#' result <- inspect_f0(sample_f0,
#'                      f0      = "f0_Hz",
#'                      token   = "token",
#'                      time    = "time",
#'                      speaker = "speaker",
#'                      tone    = "tone")
#'
#' # How many tokens were flagged at default thresholds?
#' table(unique(result[, c("token", "flagged_token")])$flagged_token)
#'
#' # Inspect the first few flagged samples
#' head(result[result$flagged_jump, c("token", "time", "f0_Hz", "flag_notes")])
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
