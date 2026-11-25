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
#' Identifies tokens whose maximum f0 is unusually high, or whose minimum
#' f0 unusually low, for a speaker's per-token f0 distribution. Suitable as
#' a first pass at detecting tracking errors, mis-segmentations, or
#' genuinely unusual productions before fitting contour models. Token-level
#' outlier removal is a recommended cleaning step prior to citation-tone
#' analysis (Xu & Zhang 2024).
#'
#' @details
#' ## What the function does internally
#'
#' 1. Drop the rows without f0. A row has f0 when its value is above 0 Hz,
#'    so `NA`, 0 (pitch trackers commonly output 0 for unvoiced frames) and
#'    negative markers such as -1 are all left out.
#' 2. Compute the per-token maximum, minimum, mean, and SD of `f0`. Each
#'    token's speaker is the most frequent non-empty value of `speaker` on
#'    its rows with f0. A tie goes to the value that comes first in `time`
#'    (when given), then to the first in alphabetical (C-locale) order, so
#'    neither the row order nor the rows without f0 change it.
#' 3. Within each speaker, z-score the per-token maxima against each
#'    other and the per-token minima against each other (so a speaker
#'    with consistently high f0 isn't flagged as an outlier of the corpus).
#' 4. Flag a token when its maximum is unusually high (`z_max >
#'    z_threshold`, `flag_too_high`) or its minimum unusually low
#'    (`z_min < -z_threshold`, `flag_too_low`). Each test is one-sided:
#'    the gross tracking errors this coarse screen targets are directional
#'    (octave-doubling or a spurious spike inflates the maximum;
#'    octave-halving, a subharmonic, or creak deflates the minimum), so
#'    only the outer tails are treated as errors.
#'
#' Because the maxima and minima are pooled across all of a speaker's tones,
#' the opposite tails are occupied by legitimate tone identity rather than
#' error (a low ceiling is what a genuine low tone looks like; a high floor
#' is what a genuine high tone looks like), so they are deliberately not
#' flagged here. The rarer artefacts that do sit in those tails (a
#' truncated, abnormally low maximum or a floored, abnormally high minimum)
#' are left to the finer screens [flag_level_outliers()] (within speaker
#' and tone) and [flag_pitch_jumps()] (sample level).
#'
#' ## Choosing `z_threshold`
#'
#' The default of `3` corresponds to the standard convention that ±3 SDs
#' cover 99.7% of a normal distribution. Because each criterion is one-sided
#' (`z_max > 3`, or `z_min < -3`), under that assumption only about 0.1% of
#' tokens lie beyond either cutoff. Lower thresholds (e.g., `2` or `2.5`)
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
#' @param z_threshold Signed z-score cutoff: a token is flagged when its
#'   per-token max exceeds `+z_threshold` (too high) or its min falls below
#'   `-z_threshold` (too low). Default `3`.
#' @param time Column name of time, or `NULL` (the default). Used only to
#'   break a tie between two speaker values that are equally frequent
#'   within a token: the one that comes first in time wins. [inspect_f0()]
#'   passes its `time`, so it picks the same speaker as [fit_polynomial()].
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
                          z_threshold = 3,
                          time        = NULL) {
  required <- c(f0, token, speaker, time)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[f0]])) {
    stop("Column '", f0, "' must be numeric (f0 in Hz).", call. = FALSE)
  }

  # Per-token f0 summaries over the rows with f0 (above 0 Hz: NA, 0 and
  # negative markers such as -1 are no f0).
  token_stats <- data |>
    dplyr::filter(has_f0(.data[[f0]])) |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::summarise(
      f0_token_max  = max(.data[[f0]], na.rm = TRUE),
      f0_token_min  = min(.data[[f0]], na.rm = TRUE),
      f0_token_mean = mean(.data[[f0]], na.rm = TRUE),
      f0_token_sd   = stats::sd(.data[[f0]], na.rm = TRUE),
      .groups = "drop"
    )

  # Each token's speaker: the most frequent non-empty value on its rows with
  # f0, ties to the first in time, then in C-locale order. The same rule as
  # fit_polynomial() and cluster_features() (pick_token_values() in
  # polynomial.R), so neither the row order nor rows without f0 change it.
  token_speaker <- token_level_meta(data, token, speaker, f0, time)

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
      flag_too_high = !is.na(.data$z_max) & .data$z_max >  z_threshold,
      flag_too_low  = !is.na(.data$z_min) & .data$z_min < -z_threshold
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
#' [flag_pitch_jumps()] (which only sees within-token movement) both miss,
#' for example a low-tone token mis-tracked up into the mid-tone band.
#'
#' @details
#' ## What the function does internally
#'
#' 1. Drop the rows without f0 (`NA`, 0 or a negative marker; only values
#'    above 0 Hz count). Each token's speaker and tone are the most frequent
#'    non-empty values on its rows with f0, so a tone column that changes
#'    within a token (for example one read from a TextGrid tier) still gives
#'    one tone per token. A tie goes to the value that comes first in `time`
#'    (when given), then to the first in alphabetical (C-locale) order, so
#'    neither the row order nor the rows without f0 change it.
#' 2. Compute each token's **median f0**, converted to semitones
#'    (`12 * log2(Hz)`). The median (rather than the mean) is used so that a
#'    handful of mis-tracked frames, which [flag_pitch_jumps()] already
#'    catches, do not shift a token's apparent level.
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
#' flagged. Detection becomes more reliable as the number of
#' same-speaker-same-tone tokens grows: with very few tokens the median and
#' MAD are coarse estimates, so a borderline outlier can fall just under the
#' threshold. Citation-tone designs usually supply several such tokens per
#' cell (repetitions, items of differing syllable structure, ...), so most
#' speaker x tone groups comfortably exceed the minimum.
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
#'   required to run the check for a group. Default `5`. More tokens give a
#'   more reliable estimate of the group's spread.
#' @param time Column name of time, or `NULL` (the default). Used only to
#'   break a tie between two speaker or tone values that are equally
#'   frequent within a token: the one that comes first in time wins.
#'   [inspect_f0()] passes its `time`, so it picks the same tone as
#'   [fit_polynomial()].
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
                                min_tokens      = 5,
                                time            = NULL) {
  required <- c(f0, token, speaker, tone, time)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[f0]])) {
    stop("Column '", f0, "' must be numeric (f0 in Hz).", call. = FALSE)
  }

  # Per-token median f0 over the rows with f0, expressed in semitones.
  token_stats <- data |>
    dplyr::filter(has_f0(.data[[f0]])) |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::summarise(
      f0_token_median = stats::median(.data[[f0]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(level_st = 12 * log2(.data$f0_token_median))

  # Each token's speaker and tone: the most frequent non-empty value on its
  # rows with f0 (a tone column read from a TextGrid tier can change within
  # a token), with the same tie rule as flag_outliers().
  token_meta <- token_level_meta(data, token, c(speaker, tone), f0, time)

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


#' Flag low-intensity f0 samples within tokens
#'
#' @description
#' Detects f0 samples that fall in a low-energy region of their token, where
#' pitch estimates are least reliable: voicing onsets and offsets,
#' devoiced or breathy segments, and creaky utterance-final tails. Intensity
#' is an *independent* cue from f0: a smoothly drifting mis-tracked tail at a
#' vowel edge need not trip the rate-of-change check in [flag_pitch_jumps()],
#' but it will sit in an intensity dip. The flag is therefore most useful
#' (a) on its own, to surface unreliable edge frames, and (b) alongside a
#' jump flag, where co-occurring low intensity raises confidence that the
#' jump is a tracking error rather than a real pitch event.
#'
#' @details
#' For each token the function takes the **peak intensity across that token's
#' voiced samples** and flags every voiced sample whose intensity is more
#' than `intensity_drop` dB below that peak. The comparison is
#' relative-to-peak (not an absolute dB threshold), so it transfers across
#' recordings regardless of microphone distance or gain. Rows without f0
#' (`NA`, 0 or a negative marker) and samples with missing intensity are
#' never flagged, and tokens with no voiced sample are left unflagged.
#'
#' Low intensity does not by itself mean the f0 is wrong (a softly but
#' modally produced frame tracks fine), so this is an advisory,
#' inspect-don't-delete signal, consistent with the rest of the Inspect tab.
#'
#' @param data A long-format data frame with one row per f0 sample.
#' @param f0 Column name of f0 in Hz. Default `"f0"`.
#' @param token Column name of token ID. Default `"token"`.
#' @param intensity Column name of intensity in dB. Default `"intensity"`.
#' @param intensity_drop Flag voiced samples whose intensity is more than
#'   this many dB below the token's peak intensity. Default `15`.
#'
#' @return The input data frame with one appended logical column,
#'   `flag_low_intensity`, `TRUE` for low-energy voiced samples.
#'
#' @seealso
#' * [flag_pitch_jumps()] for the sample-level jump / octave / carryover check.
#' * [inspect_f0()] for the wrapper that runs every check.
#'
#' @examples
#' df <- data.frame(
#'   token     = rep("t1", 5),
#'   f0        = c(120, 122, 121, 119, 118),
#'   intensity = c(40, 62, 64, 63, 41)   # quiet at the edges
#' )
#' flag_low_intensity(df, intensity_drop = 15)$flag_low_intensity
#'
#' @export
#' @importFrom dplyr group_by mutate ungroup select
#' @importFrom rlang .data
flag_low_intensity <- function(data,
                               f0             = "f0",
                               token          = "token",
                               intensity      = "intensity",
                               intensity_drop = 15) {
  required <- c(f0, token, intensity)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[intensity]])) {
    stop("Column '", intensity, "' must be numeric (intensity in dB).",
         call. = FALSE)
  }

  data |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::mutate(
      .peak_intensity = {
        voiced <- has_f0(.data[[f0]]) & !is.na(.data[[intensity]])
        if (any(voiced)) max(.data[[intensity]][voiced]) else NA_real_
      },
      flag_low_intensity =
        !is.na(.data[[intensity]]) &
        has_f0(.data[[f0]]) &
        !is.na(.data$.peak_intensity) &
        .data[[intensity]] < (.data$.peak_intensity - intensity_drop)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-".peak_intensity")
}


# -----------------------------------------------------------------------------
# Token-level values of row-level columns, for flag_outliers() and
# flag_level_outliers().
#
# One row per token (in order of first appearance), with the token column
# and each column in `cols`. This is pick_token_values() (polynomial.R), the
# rule fit_polynomial() and cluster_features() use: the most frequent
# non-empty value on the token's rows with f0 (has_f0(), above 0 Hz), ties
# to the first in `time` (when given), then to the first in C-locale order.
# A token whose rows with f0 are all unlabelled stays unlabelled; only a
# token with no f0 at all looks at its other rows. So neither the row order
# nor the rows without f0 change the result. Values keep the column's type.
# -----------------------------------------------------------------------------
token_level_meta <- function(data, token, cols, f0, time = NULL) {
  # Time only breaks ties, so a time column that is not numeric (text) is
  # read as NA here rather than stopping the check.
  pick_token_values(data, setdiff(unique(cols), token), token,
                    use  = has_f0(data[[f0]]),
                    time = if (!is.null(time)) gaps_as_numeric(data[[time]]))
}


# -----------------------------------------------------------------------------
# Internal per-token artefact detector.
#
# Two-pass algorithm, adapted from the rate-of-change + carryover approach
# of Steffman & Cole (2022). It works on the samples with f0 only: rows
# without f0 (NA, 0 or a negative marker) and rows with no time are set
# aside, the rest are put in time order, and the flags are mapped back.
# Rows set aside get FALSE and an empty note. So the result is the same
# whether or not rows without f0 are present, and whatever the row order.
#
#   Pass 1 (jump detection). Compare each sample with the next sample that
#   has f0. Check the semitone rate of change, scaled by the real time
#   step, against the rise / fall thresholds, and the Hz ratio against the
#   octave bounds. When a jump is found, the flag goes on whichever side
#   (i or i+1) is FARTHER from the token's median f0 in semitones. This is
#   the shinytone-specific adaptation: the Steffman & Cole convention
#   always flags the landing sample, which mis-identifies the artefact when
#   it sits at the start of a sequence (e.g. an octave doubling on the
#   first frame of a token). The median-distance rule reduces to
#   landing-side flagging when the landing sample is the outlier, and to
#   source-side flagging when the source sample is the outlier.
#
#   Gaps. `run` numbers the runs of samples between gaps (from f0_runs():
#   a gap is a time step over 1.5 times the token's reference step, on a
#   regular grid). A pair across a gap gets " across a N ms gap" in its
#   note, where N is the step minus the reference step. For such a pair,
#   when the two runs differ in length and the shorter one has at most
#   `max_fragment` samples, the flag goes on the shorter run's side
#   instead: a fragment cut off by a gap is the likelier error. With
#   `max_fragment = 0` the median rule always applies.
#
#   Pass 2 (carryover). Walk both forward and backward from each
#   primary-flagged sample, marking neighbours that stay within
#   `mult * threshold` semitones of the artefact's f0. The chain breaks
#   the first time a neighbour falls outside the band. It walks across
#   gaps. Setting carryover_mult <= 0 disables carryover.
#
# `run` is the run index per row (NA where the row is set aside), and
# `common_step` the file's common step (NA if none); flag_pitch_jumps()
# finds both over the whole file. Without `run`, the token is checked on
# its own.
#
# Returns a list with `flagged_jump` (logical) and `jump_note`
# (character: one of "octave jump" / "jump (rise)" / "jump (fall)" /
# "carryover", or a "; "-joined composite when multiple apply, each
# jump part ending in " across a N ms gap" when its pair spans a gap).
# -----------------------------------------------------------------------------
detect_token_artefacts <- function(f0_hz, time, time_unit,
                                   rise_threshold, fall_threshold,
                                   octave_bounds, carryover_mult,
                                   run = NULL, common_step = NA_real_,
                                   max_fragment = 5L) {
  n_all       <- length(f0_hz)
  flagged_all <- rep(FALSE, n_all)
  notes_all   <- rep("",    n_all)

  # Samples with f0 and a time, in time order (ties by f0, so the order of
  # the input rows never matters).
  tv <- gaps_as_numeric(time)
  ok <- which(has_f0(f0_hz) & is.finite(tv))
  if (length(ok) < 2) {
    return(list(flagged_jump = flagged_all, jump_note = notes_all))
  }
  f_all <- gaps_as_numeric(f0_hz)
  ok <- ok[order(tv[ok], f_all[ok])]
  f0 <- f_all[ok]
  tt <- tv[ok]
  n  <- length(ok)

  f0_st <- 12 * log2(f0)
  token_median_st <- stats::median(f0_st)
  to_ms <- if (time_unit == "ms") 1 else 1000
  dt <- diff(tt)

  # Runs between gaps, the reference step, and the gap length in ms.
  run_k <- if (is.null(run)) f0_runs(1L, tt, TRUE) else run[ok]
  run_k <- as.integer(run_k)
  if (anyNA(run_k)) run_k <- rep(1L, n)
  is_gap <- run_k[-1L] != run_k[-n]
  run_n  <- tabulate(run_k)[run_k]
  pos_dt <- dt[dt > 0]
  ref    <- if (length(pos_dt)) stats::median(pos_dt) else NA_real_
  if (length(pos_dt) < 5L && is.finite(common_step)) {
    ref <- min(ref, common_step)
  }
  gap_ms <- (dt - ref) * to_ms

  # ---------- Pass 1: detect jumps, choose the side to flag ----------
  primary <- rep(FALSE, n)
  notes   <- rep("",    n)
  for (i in seq_len(n - 1)) {
    diff_st <- f0_st[i + 1] - f0_st[i]
    tf <- max(dt[i] * to_ms / 10, 1e-9)    # time step in units of 10 ms
    rate     <- abs(diff_st) / tf
    hz_ratio <- f0[i + 1] / f0[i]

    is_oct  <- hz_ratio < octave_bounds[1] || hz_ratio > octave_bounds[2]
    is_rise <- diff_st > 0 && rate > rise_threshold
    is_fall <- diff_st < 0 && rate > fall_threshold
    if (!(is_oct || is_rise || is_fall)) next

    # Pick the side farther from the token median; ties go to the landing
    # sample to preserve historical behaviour for symmetric jumps.
    dist_i  <- abs(f0_st[i]     - token_median_st)
    dist_i1 <- abs(f0_st[i + 1] - token_median_st)
    flag_idx <- if (dist_i > dist_i1) i else i + 1
    # Across a gap, a short run on one side is the likelier error.
    if (is_gap[i]) {
      n_i  <- run_n[i]
      n_i1 <- run_n[i + 1]
      if (n_i != n_i1 && min(n_i, n_i1) <= max_fragment) {
        flag_idx <- if (n_i < n_i1) i else i + 1
      }
    }
    primary[flag_idx] <- TRUE

    parts <- character(0)
    if (is_oct)  parts <- c(parts, "octave jump")
    if (is_rise) parts <- c(parts, "jump (rise)")
    if (is_fall) parts <- c(parts, "jump (fall)")
    new_note <- paste(parts, collapse = "; ")
    if (is_gap[i]) {
      new_note <- sprintf("%s across a %d ms gap", new_note,
                          as.integer(round(gap_ms[i])))
    }
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
      idx <- start_idx + direction
      while (idx >= 1 && idx <= n) {
        if (primary[idx]) break
        prev_idx <- idx - direction
        band <- if (f0_st[prev_idx] > f0_st[idx]) rise_threshold * carryover_mult
                else                              fall_threshold * carryover_mult
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

  flagged_all[ok] <- primary | carry
  notes_all[ok]   <- ifelse(primary, notes, ifelse(carry, "carryover", ""))
  list(flagged_jump = flagged_all, jump_note = notes_all)
}


#' Flag sample-to-sample f0 jumps within tokens
#'
#' @description
#' Detects sample-level pitch-tracking artefacts inside each token.
#' Three classes of artefact are flagged: rate-of-change violations
#' (faster rises or falls than human vocal folds can plausibly produce),
#' octave jumps (pitch halving or doubling), and carryover samples on
#' either side of a flagged sample that stay close enough to the error to
#' be suspected of being part of the same artefact.
#'
#' Used as the sample-level counterpart of [flag_outliers()] in the
#' Inspect tab of the Shiny app and inside [inspect_f0()].
#'
#' @details
#' ## What the function does internally
#'
#' 1. Set aside the rows without f0 and the rows with no time. A row has f0
#'    when its value is above 0 Hz, so `NA`, 0 and negative markers such as
#'    -1 all count as no f0. These rows are never flagged: they get
#'    `flagged_jump = FALSE` and an empty note. The check therefore gives the
#'    same flags whether or not the data still contain rows without f0, and
#'    whatever the row order. Take any share of flagged samples over the
#'    samples with f0, so it does not change either.
#' 2. Within each token, in time order, compare each sample with the next
#'    sample that has f0: the semitone difference and the Hz ratio.
#' 3. Rescale the rate of change to a semitones-per-10-ms equivalent using
#'    the real time step between the two samples and `time_unit`, so the
#'    result can be compared to a fixed threshold (Sundberg 1973).
#' 4. When a jump is detected between samples *i* and *i + 1*, flag
#'    whichever side is FARTHER from the token's median f0 (in
#'    semitones). The Steffman & Cole (2022) convention always flags
#'    the landing sample, which mis-identifies the artefact when it
#'    sits at the start of a token (e.g., an octave doubling on the
#'    first frame). The median-distance rule reduces to landing-side
#'    flagging when the landing sample is the outlier and to
#'    source-side flagging when the source sample is the outlier. Across
#'    a gap a short fragment is flagged instead (see below).
#' 5. Walk both forward and backward from each flagged sample as a
#'    chain of carryover frames that stay within `carryover_mult *
#'    threshold` semitones of the artefact's f0 (adapted from
#'    Steffman & Cole 2022). A chain can continue across a gap.
#'
#' ## Gaps inside a token
#'
#' A gap is a stretch inside a token with no f0. The function finds gaps
#' from the time steps between samples with f0, so it finds the same gaps
#' whether or not the rows without f0 are still there. A step is a gap when
#' it is more than 1.5 times the token's usual step. The usual step is the
#' token's median step. A token with fewer than 5 steps uses the file's
#' common step instead, when that is smaller.
#'
#' Gaps are read only when the time grid is regular across the file, as in
#' [screen_f0_rows()]. It is not regular, for example, when samples are
#' timed at each glottal pulse. Times written to the step itself, such as
#' 10 ms frames written to 2 decimals, are read in whole steps (a step of
#' 0.02 or more is a gap), but only when at most 1 step in 100 is not
#' exactly 0.01. When the grid is not regular, no gaps are marked, and no
#' note or placement below applies. A token where two samples have the same
#' time gets no gaps either.
#'
#' When the two samples of a jump lie on either side of a gap, the note says
#' so, for example `"jump (rise) across a 20 ms gap"`. The 20 ms is the step
#' minus the usual step, so a one-frame gap in 10 ms frames reads as 10 ms.
#' The rate test is still scaled by the real step, and the octave test is
#' the same as for any other pair.
#'
#' Across a gap, the flag goes on the side whose run of samples (the
#' samples between gaps) is shorter, when that run has at most 5 samples. A
#' short fragment cut off by a gap is the likelier tracking error. When
#' both runs have the same length, or both are longer than 5 samples, the
#' side farther from the token median is flagged as usual.
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
#' flags only the one sample chosen for each jump. The default of `1.5`
#' follows Steffman & Cole (2022).
#'
#' @param data A long-format data frame with one row per f0 sample.
#' @param f0 Column name of f0 in Hz. Default `"f0"`.
#' @param token Column name of token ID. Default `"token"`.
#' @param time Column name of time. Default `"time"`. Use the real time
#'   column, not a 0-1, `*_tseq` or `*_tbound` axis.
#' @param time_unit One of `"s"` or `"ms"`. Default `"s"`. Inspection is
#'   meant to run on real-time data; a normalised-time option was removed
#'   because the physiological rate thresholds below lose their meaning once
#'   real time is discarded. The unit also sets the gap length in the notes.
#'   Check it against the data: times in ms read as seconds make every step
#'   look 1000 times slower, so the rate test then flags almost nothing. A
#'   median step below 1 within tokens usually means seconds.
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
#'   joined by `"; "`). The part for a jump across a gap ends in
#'   `" across a N ms gap"`, e.g. `"jump (rise) across a 20 ms gap"`.
#'
#' The rows come back sorted by token and time, with the token column
#' first.
#'
#' @seealso
#' * [flag_outliers()] for the complementary token-level outlier check.
#' * [inspect_f0()] for the wrapper that joins both.
#'
#' @references
#' Steffman, J., & Cole, J. (2022). An automated method for detecting f0
#' measurement jumps based on sample-to-sample differences. \emph{JASA
#' Express Letters}, 2(11), 115201. \doi{10.1121/10.0015045}
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

  # Gaps inside tokens, read over the whole file from the time steps between
  # samples with f0 (f0_runs(): a step over 1.5 times the token's reference
  # step, on a regular grid). Rows without f0 are left out here, so they
  # cannot change where the gaps are.
  ok  <- has_f0(data[[f0]]) & is.finite(data[[time]])
  run <- rep(NA_integer_, nrow(data))
  common_step <- NA_real_
  if (any(ok)) {
    runs <- f0_runs(data[[token]][ok], data[[time]][ok], TRUE)
    run[ok] <- as.integer(runs)
    grid <- attr(runs, "grid")
    if (isTRUE(grid$common)) common_step <- grid$step
  }
  run_col <- ".jump_run"
  while (run_col %in% names(data)) run_col <- paste0(run_col, "_")
  data[[run_col]] <- run

  # All per-token logic (jump detection + carryover) lives in
  # detect_token_artefacts(); see its header for the algorithm, the gap
  # note and the flag placement.
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
        carryover_mult = carryover_mult,
        run            = .x[[run_col]],
        common_step    = common_step
      )
      .x[[run_col]]   <- NULL
      .x$flagged_jump <- out$flagged_jump
      .x$jump_note    <- out$jump_note
      .x
    }) |>
    dplyr::ungroup()
}


#' Inspect f0 data for token-level outliers and sample-level jumps
#'
#' @description
#' One-call convenience wrapper that runs [flag_outliers()],
#' [flag_pitch_jumps()], and (when `tone` is supplied) [flag_level_outliers()]
#' on the same dataset, joins their results back into one long-format data
#' frame, and produces a single `flag_notes` column with human-readable
#' reasons for each flag. This is the function that backs the Inspect tab of
#' the Shiny app.
#'
#' @details
#' ## What the function does internally
#'
#' 1. Call [flag_outliers()] for the speaker-level extreme-value check
#'    (`flag_too_high`, `flag_too_low`) plus per-token summary statistics.
#' 2. When `tone` is supplied, call [flag_level_outliers()] for the
#'    token-level (speaker-by-tone) level check (`flag_level_high`,
#'    `flag_level_low`). With `tone = NULL` this screen is skipped and both
#'    flags are set to `FALSE`.
#' 3. Call [flag_pitch_jumps()] for the sample-level pitch-tracking artefact
#'    flags (`flagged_jump`, `jump_note`). It compares each sample with the
#'    next sample that has f0, with the rate scaled by the real time step. A
#'    jump across a gap (a stretch with no f0) gets `" across a N ms gap"`
#'    in its note, and the flag goes on a short run of at most 5 samples cut
#'    off by the gap; otherwise on the side farther from the token median.
#' 4. Left-join the token-level flags onto the long-format jump output, so
#'    every sample carries all three kinds of information.
#' 5. Set `flagged_token` to `TRUE` for any token that is a speaker-level
#'    extreme, an unusual level for its tone, or contains at least one
#'    sample-level jump.
#' 6. Concatenate the human-readable reasons into a single `flag_notes`
#'    column for display.
#'
#' ## Rows without f0
#'
#' A row has f0 when its value is above 0 Hz. Rows without f0 (`NA`, 0 or a
#' negative marker such as -1) stay in the output but are never flagged as
#' samples: `flagged_jump` is `FALSE` on them, and no check uses their
#' values. They do carry their token's `flagged_token` and its token-level
#' notes, since the token-level flags are joined by token. So the flags on
#' the rows with f0 are the same whether or not the data still have these
#' rows (for example after [drop_rows_without_f0()]). Count shares of
#' flagged samples over the samples with f0, as the Inspect tab does, so
#' they do not change either.
#'
#' A token with no f0 at all cannot be checked. Its flags are all `FALSE`,
#' and each of its rows gets the note `"no f0"`. It does not count towards
#' the size of its speaker x tone group in the level check.
#'
#' Use the individual functions ([flag_outliers()], [flag_level_outliers()],
#' [flag_pitch_jumps()]) if you want only one kind of check or want to combine
#' the outputs yourself in a non-standard way.
#'
#' @inheritParams flag_outliers
#' @inheritParams flag_pitch_jumps
#' @inheritParams flag_level_outliers
#' @param time Column name of time. Default `"time"`.
#' @param tone Column name of tone category, or `NULL`. Default `"tone"`.
#'   When `NULL`, the token-level (speaker-by-tone) level check
#'   ([flag_level_outliers()]) is skipped and the `tone` column is omitted
#'   from the output, so the wrapper can run before tone categories are known
#'   (e.g. the [cluster_f0()] tone-discovery workflow); the speaker-level and
#'   sample-level checks still run.
#' @param intensity Optional column name of intensity in dB. When supplied,
#'   the sample-level low-intensity check ([flag_low_intensity()]) is run and
#'   its results are added (see Value). `NULL` (default) skips the check.
#' @param intensity_drop Flag voiced samples whose intensity is more than this
#'   many dB below the token's peak intensity. Default `15`. Ignored when
#'   `intensity` is `NULL`.
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
#'   `"jump (rise) across a 20 ms gap"`, `"level too high"`,
#'   `"low intensity"`), or `"no f0"` for each row of a token with no f0.
#'
#' The rows come back sorted by token and time. When the input has a
#' `voiced_run` column (from [drop_rows_without_f0()]), it is kept, after
#' the f0 column.
#'
#' When `tone` is `NULL` the `tone` column is omitted and the level check is
#' not run, so `flag_notes` never contains `"level too high"` /
#' `"level too low"` and those checks do not contribute to `flagged_token`.
#' When `tone` is given, the attribute `"level_groups"` holds one row per
#' speaker x tone group, with `n_tokens` (its tokens with f0) and `checked`
#' (`TRUE` when `n_tokens` is at least `min_tokens`, so the level check ran
#' for the group). The attribute `"token_groups"` holds one row per token
#' with f0: its `token`, `speaker` and `tone`, as the level check read them
#' (the most frequent non-empty value on the token's rows with f0).
#'
#' When `intensity` is supplied, the result additionally carries the
#' `intensity` column and a sample-level `flag_low_intensity` logical. The
#' low-intensity flag is advisory: it appears in `flag_notes` but does not by
#' itself set `flagged_token`.
#'
#' @seealso
#' * [flag_outliers()], [flag_pitch_jumps()], and [flag_level_outliers()],
#'   the three components.
#' * [normalise_f0()] for the downstream normalisation step.
#'
#' @references
#' Steffman, J., & Cole, J. (2022). An automated method for detecting f0
#' measurement jumps based on sample-to-sample differences. \emph{JASA
#' Express Letters}, 2(11), 115201. \doi{10.1121/10.0015045}
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
#' # Share of flagged samples, over the samples with f0
#' with_f0 <- !is.na(result$f0_Hz) & result$f0_Hz > 0
#' mean(result$flagged_jump[with_f0])
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
                       min_tokens      = 5,
                       intensity       = NULL,
                       intensity_drop  = 15,
                       time_unit       = c("s", "ms")) {
  time_unit <- match.arg(time_unit)

  outliers <- flag_outliers(data, f0 = f0, token = token, speaker = speaker,
                            z_threshold = z_threshold, time = time)

  # The token-level (speaker-by-tone) check needs tone labels; skip it when
  # tone is NULL (e.g. before tone categories are known).
  levels <- if (!is.null(tone)) {
    flag_level_outliers(data, f0 = f0, token = token,
                        speaker = speaker, tone = tone,
                        level_threshold = level_threshold,
                        min_tokens = min_tokens, time = time)
  } else {
    NULL
  }

  result <- flag_pitch_jumps(data, f0 = f0, token = token, time = time,
                             time_unit      = time_unit,
                             rise_threshold = rise_threshold,
                             fall_threshold = fall_threshold,
                             octave_bounds  = octave_bounds,
                             carryover_mult = carryover_mult)

  # Long-format result + per-token flags. A token with no f0 has no row in
  # the outlier table, so its joined flags are NA: read them as FALSE, as
  # for the level flags below.
  result <- result |>
    dplyr::left_join(
      outliers |> dplyr::select(dplyr::all_of(token),
                                "f0_token_max", "f0_token_min",
                                "f0_token_mean", "f0_token_sd",
                                "z_max", "z_min",
                                "flag_too_high", "flag_too_low"),
      by = token
    ) |>
    dplyr::mutate(
      flag_too_high = !is.na(.data$flag_too_high) & .data$flag_too_high,
      flag_too_low  = !is.na(.data$flag_too_low)  & .data$flag_too_low
    )

  if (!is.null(tone)) {
    result <- result |>
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
  } else {
    # tone = NULL: the speaker-by-tone level check was skipped, so its flags
    # never fire (set to a length-n FALSE via mutate so downstream code is
    # unchanged).
    result <- result |>
      dplyr::mutate(flag_level_high = FALSE, flag_level_low = FALSE)
  }

  # A token with no f0 (every row NA, 0 or a negative marker) cannot be
  # checked. It gets the note "no f0" on each of its rows and is not flagged.
  result <- result |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::mutate(
      has_jump_in_token = any(.data$flagged_jump, na.rm = TRUE),
      .no_f0_token      = !any(has_f0(.data[[f0]]))
    ) |>
    dplyr::ungroup()

  result <- result |>
    dplyr::mutate(
      flag_notes = {
        notes <- ifelse(.data$.no_f0_token, "no f0", "")
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

  # Optional intensity check (sample-level, advisory). When an intensity
  # column is supplied, flag voiced samples sitting in a low-energy region of
  # their token and note them. Intensity does not change flagged_token: a
  # quiet-but-modal frame can track fine, so this is an inspect-don't-delete
  # cue, most useful where it co-occurs with a jump.
  if (!is.null(intensity)) {
    result <- flag_low_intensity(result, f0 = f0, token = token,
                                 intensity = intensity,
                                 intensity_drop = intensity_drop)
    result <- result |>
      dplyr::mutate(
        flag_notes = ifelse(
          .data$flag_low_intensity,
          ifelse(nchar(.data$flag_notes) > 0,
                 paste0(.data$flag_notes, "; low intensity"),
                 "low intensity"),
          .data$flag_notes)
      )
  }

  # Final selected columns, matching what the Shiny Inspect tab expects.
  # The intensity column and its flag are included only when requested, and
  # the gap column voiced_run (from drop_rows_without_f0()) when the input
  # has it.
  keep <- c(token, time, f0)
  if (!is.null(intensity)) keep <- c(keep, intensity)
  if ("voiced_run" %in% names(data)) keep <- c(keep, "voiced_run")
  keep <- c(keep, speaker, tone,
            "f0_token_max", "f0_token_min", "f0_token_mean", "f0_token_sd",
            "flagged_token", "flagged_jump", "flag_notes")
  if (!is.null(intensity)) keep <- c(keep, "flag_low_intensity")

  out <- result |>
    dplyr::select(dplyr::all_of(unique(keep)))

  # The speaker x tone groups of the level check, counted on the tokens with
  # f0 (the tokens flag_level_outliers() compares), so a token with no f0
  # never makes a group look big enough. The Inspect tab reads this for its
  # note on the groups too small to check. "token_groups" gives each token
  # with f0 the speaker and tone the level check used, so the tab's
  # per-tone counts group tokens the same way (a tone column read from a
  # TextGrid tier can change within a token).
  if (!is.null(levels)) {
    grp <- levels |>
      dplyr::count(dplyr::across(dplyr::all_of(unique(c(speaker, tone)))),
                   name = "n_tokens") |>
      as.data.frame()
    grp$checked <- grp$n_tokens >= min_tokens
    attr(out, "level_groups") <- grp
    attr(out, "token_groups") <- levels |>
      dplyr::select(dplyr::all_of(unique(c(token, speaker, tone)))) |>
      as.data.frame()
  }
  out
}
