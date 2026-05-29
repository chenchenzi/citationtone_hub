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


#' Flag tokens whose overall f0 level is unusual for their speaker and tone
#'
#' @description
#' Detects tokens whose *whole contour* sits too high or too low compared
#' with other tokens of the **same speaker and same tone**, even when the
#' contour is smooth (no sample-to-sample jumps) and not extreme for the
#' speaker overall. This catches the "right shape, wrong height" case that
#' [flag_outliers()] (which pools all tones within a speaker) and
#' [flag_pitch_jumps()] (which only sees within-token movement) both miss
#' --- for example a low-tone token mis-tracked up into the mid-tone band.
#'
#' @details
#' ## What the function does internally
#'
#' 1. Drop samples where `f0` is `NA` or `0` (unvoiced frames).
#' 2. Compute each token's **median f0**, converted to semitones
#'    (`12 * log2(Hz)`). The median (rather than the mean) is used so that a
#'    handful of mis-tracked frames --- which [flag_pitch_jumps()] already
#'    catches --- do not shift a token's apparent level.
#' 3. Within each speaker x tone group, compute the **modified z-score**
#'    (Iglewicz & Hoaglin 1993) of each token's level:
#'    `0.6745 * (level - median) / MAD`, where MAD is the median absolute
#'    deviation. The modified z-score is on the same scale as an ordinary
#'    z-score but is built from the robust median/MAD, so a few deviant
#'    tokens cannot inflate the spread and mask themselves. When the MAD is
#'    zero (more than half the peers share an identical level), the function
#'    falls back to the mean-absolute-deviation form,
#'    `(level - median) / (1.2533 * meanAD)`.
#' 4. Flag a token as `level too high` / `level too low` when its modified
#'    z-score exceeds `+level_threshold` / falls below `-level_threshold`.
#'
#' ## Minimum group size
#'
#' A spread cannot be estimated from a near-empty group, so the check only
#' runs for speaker x tone groups with at least `min_tokens` tokens; tokens
#' in smaller groups are returned with `level_modz = NA` and are never
#' flagged.
#'
#' This is a token-level cleaning step intended to run on **raw f0 (Hz)
#' before normalisation**: the within speaker x tone comparison is
#' self-normalising, and cleaning before normalisation keeps mis-tracked
#' tokens from corrupting the by-speaker normalisation statistics.
#'
#' @param data A long-format data frame with one row per f0 sample.
#' @param f0 Column name of f0 in Hz. Default `"f0"`.
#' @param token Column name of token ID. Default `"token"`.
#' @param speaker Column name of speaker ID. Default `"speaker"`.
#' @param tone Column name of tone category. Default `"tone"`.
#' @param level_threshold Absolute modified z-score above which a token's
#'   level is flagged. Default `3.5` (Iglewicz & Hoaglin 1993).
#' @param min_tokens Minimum number of same-speaker-same-tone tokens
#'   required to run the check for a group. Default `4`.
#'
#' @return A token-level data frame (one row per token with at least one
#'   voiced sample) with columns: `f0_token_median`, `level_st`,
#'   `n_tone_peers`, `level_modz`, `flag_level_high`, `flag_level_low`,
#'   plus the original `token`, `speaker`, and `tone` columns.
#'
#' @seealso
#' * [flag_outliers()] for pooled per-speaker max/min outliers.
#' * [flag_pitch_jumps()] for sample-level artefact detection.
#' * [inspect_f0()] for the wrapper that runs all three.
#'
#' @references
#' Iglewicz, B., & Hoaglin, D. C. (1993). \emph{How to Detect and Handle
#' Outliers}. ASQC Quality Press.
#'
#' Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation
#' tone production studies: Methodology and recommendations.
#' \emph{The Journal of the Acoustical Society of America}, 156(4),
#' 2538–2565. \doi{10.1121/10.0032356}
#'
#' @examples
#' data(sample_f0)
#' lvl <- flag_level_outliers(sample_f0,
#'                            f0      = "f0_Hz",
#'                            token   = "token",
#'                            speaker = "speaker",
#'                            tone    = "tone")
#' table(lvl$flag_level_high, lvl$flag_level_low, useNA = "ifany")
#'
#' @export
#' @importFrom dplyr filter group_by summarise distinct left_join mutate ungroup all_of select n
#' @importFrom rlang .data
#' @importFrom stats median
flag_level_outliers <- function(data,
                                f0              = "f0",
                                token           = "token",
                                speaker         = "speaker",
                                tone            = "tone",
                                level_threshold = 3.5,
                                min_tokens      = 4) {
  required <- c(f0, token, speaker, tone)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[f0]])) {
    stop("Column '", f0, "' must be numeric (f0 in Hz).", call. = FALSE)
  }

  # Per-token median f0 (excluding NA / zero), expressed in semitones.
  token_stats <- data |>
    dplyr::filter(!is.na(.data[[f0]]) & .data[[f0]] != 0) |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::summarise(
      f0_token_median = stats::median(.data[[f0]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(level_st = 12 * log2(.data$f0_token_median))

  token_meta <- data |>
    dplyr::distinct(.data[[token]], .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(c(token, speaker, tone)))

  # Modified z-score (Iglewicz & Hoaglin 1993), with a mean-absolute-
  # deviation fallback for the degenerate MAD == 0 case (more than half the
  # peers at an identical level, common for very consistent speakers).
  modz <- function(x) {
    med  <- stats::median(x, na.rm = TRUE)
    madv <- stats::median(abs(x - med), na.rm = TRUE)
    if (!is.na(madv) && madv > 0) {
      return(0.6745 * (x - med) / madv)
    }
    meanad <- mean(abs(x - med), na.rm = TRUE)
    if (!is.na(meanad) && meanad > 0) {
      return((x - med) / (1.253314 * meanad))
    }
    rep(NA_real_, length(x))   # all peers identical: nothing to compare
  }

  token_stats |>
    dplyr::left_join(token_meta, by = token) |>
    dplyr::group_by(.data[[speaker]], .data[[tone]]) |>
    dplyr::mutate(
      n_tone_peers = sum(!is.na(.data$level_st)),
      level_modz   = if (sum(!is.na(.data$level_st)) >= min_tokens)
                       modz(.data$level_st)
                     else rep(NA_real_, dplyr::n())
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      flag_level_high = !is.na(.data$level_modz) &
                          .data$level_modz >  level_threshold,
      flag_level_low  = !is.na(.data$level_modz) &
                          .data$level_modz < -level_threshold
    )
}


# -----------------------------------------------------------------------------
# Internal per-token artefact detector.
#
# Two-pass algorithm, adapted from the rate-of-change + carryover approach
# of Steffman & Cole (2022):
#
#   Pass 1 (jump detection).  For each consecutive pair (i, i+1), check
#   the semitone rate of change against rise / fall thresholds and the Hz
#   ratio against the octave bounds. When a jump is detected, the FLAG
#   is placed on whichever side (i or i+1) is FARTHER from the token's
#   median f0 in semitones. This is the shinytone-specific adaptation:
#   the Steffman & Cole convention always flags the landing sample,
#   which mis-identifies the artefact when it sits at the start of a
#   sequence (e.g. an octave doubling on the first frame of a token).
#   The median-distance rule reduces to landing-side flagging when the
#   landing sample is the outlier, and to source-side flagging when the
#   source sample is the outlier.
#
#   Pass 2 (carryover).  Walk both forward and backward from each
#   primary-flagged sample, marking neighbours that stay within
#   `mult * threshold` semitones of the artefact's f0. The chain breaks
#   the first time a neighbour falls outside the band. Setting
#   carryover_mult <= 0 disables carryover.
#
# Returns a list with `flagged_jump` (logical) and `jump_note`
# (character: one of "octave jump" / "jump (rise)" / "jump (fall)" /
# "carryover", or a "; "-joined composite when multiple apply).
# -----------------------------------------------------------------------------
detect_token_artefacts <- function(f0_hz, time, time_unit,
                                   rise_threshold, fall_threshold,
                                   octave_bounds, carryover_mult) {
  n      <- length(f0_hz)
  primary <- rep(FALSE, n)
  notes   <- rep("",    n)

  if (n < 2) {
    return(list(flagged_jump = primary, jump_note = notes))
  }

  f0_st  <- suppressWarnings(12 * log2(f0_hz))
  valid  <- !is.na(f0_st)
  if (sum(valid) < 2) {
    return(list(flagged_jump = primary, jump_note = notes))
  }
  token_median_st <- stats::median(f0_st[valid])

  # ---------- Pass 1: detect jumps, choose median-farther side ----------
  for (i in seq_len(n - 1)) {
    if (is.na(f0_hz[i]) || is.na(f0_hz[i + 1])) next

    diff_st   <- f0_st[i + 1] - f0_st[i]
    diff_time <- time[i + 1]  - time[i]
    tf <- if (time_unit == "ms") max(diff_time / 10,        1e-9)
          else                   max((diff_time * 1000) / 10, 1e-9)  # "s"
    rate     <- abs(diff_st) / tf
    hz_ratio <- f0_hz[i + 1] / f0_hz[i]

    is_oct  <- !is.na(hz_ratio) &&
               (hz_ratio < octave_bounds[1] || hz_ratio > octave_bounds[2])
    is_rise <- diff_st > 0 && rate > rise_threshold
    is_fall <- diff_st < 0 && rate > fall_threshold
    if (!(is_oct || is_rise || is_fall)) next

    # Pick the side farther from the token median; ties go to the landing
    # sample to preserve historical behaviour for symmetric jumps.
    dist_i  <- abs(f0_st[i]     - token_median_st)
    dist_i1 <- abs(f0_st[i + 1] - token_median_st)
    flag_idx <- if (dist_i > dist_i1) i else i + 1
    primary[flag_idx] <- TRUE

    parts <- character(0)
    if (is_oct)  parts <- c(parts, "octave jump")
    if (is_rise) parts <- c(parts, "jump (rise)")
    if (is_fall) parts <- c(parts, "jump (fall)")
    new_note <- paste(parts, collapse = "; ")
    notes[flag_idx] <- if (nchar(notes[flag_idx]) > 0)
                         paste(notes[flag_idx], new_note, sep = "; ")
                       else new_note
  }

  # ---------- Pass 2: bidirectional carryover ----------
  carry <- rep(FALSE, n)
  if (carryover_mult > 0) {

    extend_chain <- function(start_idx, direction) {
      # direction: +1 walks forward (start_idx + 1, +2, ...),
      # -1 walks backward (start_idx - 1, -2, ...).
      error_f0 <- f0_st[start_idx]
      if (is.na(error_f0)) return(invisible(NULL))
      idx <- start_idx + direction
      while (idx >= 1 && idx <= n) {
        if (is.na(f0_st[idx]) || primary[idx]) break
        prev_idx <- idx - direction
        band <- if (prev_idx >= 1 && prev_idx <= n && !is.na(f0_st[prev_idx])) {
          if (f0_st[prev_idx] > f0_st[idx]) rise_threshold * carryover_mult
          else                              fall_threshold * carryover_mult
        } else {
          max(rise_threshold, fall_threshold) * carryover_mult
        }
        if (abs(f0_st[idx] - error_f0) > band) break
        carry[idx] <<- TRUE
        idx <- idx + direction
      }
    }

    for (i in which(primary)) {
      extend_chain(i, +1L)
      extend_chain(i, -1L)
    }
  }

  flagged_jump <- primary | carry
  jump_note    <- ifelse(primary, notes,
                         ifelse(carry, "carryover", ""))
  list(flagged_jump = flagged_jump, jump_note = jump_note)
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
#' 4. When a jump is detected between samples *i* and *i + 1*, flag
#'    whichever side is FARTHER from the token's median f0 (in
#'    semitones). The Steffman & Cole (2022) convention always flags
#'    the landing sample, which mis-identifies the artefact when it
#'    sits at the start of a token (e.g., an octave doubling on the
#'    first frame). The median-distance rule reduces to landing-side
#'    flagging when the landing sample is the outlier and to
#'    source-side flagging when the source sample is the outlier.
#' 5. Walk both forward and backward from each flagged sample as a
#'    chain of carryover frames that stay within `carryover_mult *
#'    threshold` semitones of the artefact's f0 (adapted from
#'    Steffman & Cole 2022).
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
#' @param time_unit One of `"s"` or `"ms"`. Default `"s"`. Inspection is
#'   meant to run on real-time data; a normalised-time option was removed
#'   because the physiological rate thresholds below lose their meaning once
#'   real time is discarded.
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
                             time_unit      = c("s", "ms"),
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

  # All per-token logic (jump detection + carryover) lives in
  # detect_token_artefacts(); see its header for the algorithm and the
  # rationale for the median-aware flag placement.
  data |>
    dplyr::arrange(.data[[token]], .data[[time]]) |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::group_modify(~ {
      out <- detect_token_artefacts(
        f0_hz          = .x[[f0]],
        time           = .x[[time]],
        time_unit      = time_unit,
        rise_threshold = rise_threshold,
        fall_threshold = fall_threshold,
        octave_bounds  = octave_bounds,
        carryover_mult = carryover_mult
      )
      .x$flagged_jump <- out$flagged_jump
      .x$jump_note    <- out$jump_note
      .x
    }) |>
    dplyr::ungroup()
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
#' @inheritParams flag_level_outliers
#' @param time Column name of time. Default `"time"`.
#' @param tone Column name of tone category. Default `"tone"`.
#'
#' @return A long-format data frame containing the original `token`,
#'   `time`, `f0`, `speaker`, `tone` columns plus:
#' * `f0_token_max`, `f0_token_min`, `f0_token_mean`, `f0_token_sd`:
#'   per-token summary statistics.
#' * `flagged_jump`: logical, sample-level jump flag.
#' * `flagged_token`: logical, `TRUE` if the token has any extreme value,
#'   any sample-level jump, or an unusual overall level for its tone.
#' * `flag_notes`: human-readable concatenation of the reasons a sample
#'   was flagged (e.g. `"max too high"`, `"jump (rise)"`,
#'   `"level too high"`).
#'
#' @seealso
#' * [flag_outliers()], [flag_pitch_jumps()], and [flag_level_outliers()],
#'   the three components.
#' * [normalise_f0()] for the downstream normalisation step.
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
                       octave_bounds   = c(0.49, 1.99),
                       carryover_mult  = 1.5,
                       level_threshold = 3.5,
                       min_tokens      = 4,
                       time_unit       = c("s", "ms")) {
  time_unit <- match.arg(time_unit)

  outliers <- flag_outliers(data, f0 = f0, token = token, speaker = speaker,
                            z_threshold = z_threshold)

  levels <- flag_level_outliers(data, f0 = f0, token = token,
                                speaker = speaker, tone = tone,
                                level_threshold = level_threshold,
                                min_tokens = min_tokens)

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
    ) |>
    dplyr::left_join(
      levels |> dplyr::select(dplyr::all_of(token),
                              "flag_level_high", "flag_level_low"),
      by = token
    ) |>
    # Tokens with no voiced samples are absent from the level table; treat
    # their (joined-NA) level flags as FALSE.
    dplyr::mutate(
      flag_level_high = !is.na(.data$flag_level_high) & .data$flag_level_high,
      flag_level_low  = !is.na(.data$flag_level_low)  & .data$flag_level_low
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
        notes <- ifelse(.data$flag_level_high,
                        ifelse(nchar(notes) > 0,
                               paste0(notes, "; level too high"),
                               "level too high"),
                        notes)
        notes <- ifelse(.data$flag_level_low,
                        ifelse(nchar(notes) > 0,
                               paste0(notes, "; level too low"),
                               "level too low"),
                        notes)
        notes <- ifelse(nchar(.data$jump_note) > 0,
                        ifelse(nchar(notes) > 0,
                               paste0(notes, "; ", .data$jump_note),
                               .data$jump_note),
                        notes)
        notes
      },
      flagged_token = .data$flag_too_high | .data$flag_too_low |
                      .data$flag_level_high | .data$flag_level_low |
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
