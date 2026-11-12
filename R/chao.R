# =============================================================================
# Chao tone numeral summarisation.
#
# Four exported functions:
#   * compute_mean_contour()  long-format f0 -> per-tone mean contour
#   * classify_contour()      Chao digit string -> shape name
#   * chao_to_letters()       Chao digit string -> IPA tone letters
#   * contour_to_chao()       per-tone contour -> Chao numerals (all 3 methods)
# =============================================================================


#' Compute the per-tone mean f0 contour from long-format data
#'
#' @description
#' Aggregates a long-format f0 dataset into a per-tone mean contour
#' suitable for plotting, comparison, or Chao numeral summarisation
#' via [contour_to_chao()]. The function is used internally by the
#' Summarise tab of the Shiny app when the user chooses the raw or
#' normalised dataset as the input.
#'
#' @details
#' Internally:
#'
#' 1. Within each token, normalise the time axis to `[0, 1]` so tokens
#'    of different durations can be averaged across the same grid. A
#'    time column that is *already* normalised is detected with
#'    [time_already_normalised()] and used as-is (override with
#'    `time_normalised`), matching what [fit_gca()] and [fit_gamm()] do.
#' 2. Resample every token, by linear interpolation, at the same
#'    `n_points` equally spaced time points, so each token counts once at
#'    every point however many f0 samples it has. Missing f0 values are
#'    skipped. A token is left out at points beyond its first or last f0
#'    value rather than extrapolated; a point within a tenth of a grid step
#'    of that value takes it, so rounded times do not drop a measured end
#'    point. Tokens with f0 at fewer than two time points are dropped, with
#'    a message.
#' 3. At each time point, take the mean of `f0` across the tokens of each
#'    tone.
#' 4. Return the long-format mean contour with columns `tone`, `time`,
#'    `f0_predicted` (the column name `f0_predicted` matches the
#'    convention used by [predict_gca()] and [predict_gamm()] so the
#'    same downstream tools can consume either model predictions or
#'    raw mean contours).
#'
#' @param data A long-format data frame with one row per f0 sample.
#' @param token,f0,time,tone Column names.
#' @param n_points Number of equally spaced time points across `[0, 1]`
#'   at which every token is resampled. Default 21, one point every 5%, as
#'   in [resample_f0_equal()] and [cluster_features()]. Measured points
#'   that fall on this grid, such as 11 or 21 equally spaced points per
#'   token, keep their values; for denser data, raise `n_points`.
#' @param time_normalised One of `"auto"` (default: detect an
#'   already-normalised `[0, 1]` time column and use it as-is), `"no"`
#'   (always rescale to `[0, 1]` within each token), or `"yes"` (declare
#'   the column already normalised). See [resolve_time_norm()].
#' @param n_bins Deprecated; use `n_points`. Samples used to be assigned
#'   to `n_bins` time bins; a value given here is used as `n_points`, with
#'   a warning.
#'
#' @return A data frame with columns `tone`, `time`, `f0_predicted`,
#'   one row per (tone, time point).
#'
#' @seealso [contour_to_chao()] for converting mean contours to Chao
#'   numerals. [predict_gca()] and [predict_gamm()] for model-based
#'   alternatives that produce the same column structure.
#'
#' @references
#' Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation
#' tone production studies: Methodology and recommendations.
#' \emph{The Journal of the Acoustical Society of America}, 156(4),
#' 2538–2565. \doi{10.1121/10.0032356}
#'
#' @examples
#' data(sample_f0)
#' mc <- compute_mean_contour(sample_f0,
#'                            token = "token", f0 = "f0_Hz",
#'                            time  = "time",  tone = "tone")
#' head(mc)
#'
#' @export
#' @importFrom dplyr group_by mutate ungroup summarise arrange n
#' @importFrom rlang .data
compute_mean_contour <- function(data,
                                 token = "token",
                                 f0    = "f0",
                                 time  = "time",
                                 tone  = "tone",
                                 n_points = 21L,
                                 time_normalised = c("auto", "no", "yes"),
                                 n_bins = NULL) {
  time_normalised <- match.arg(time_normalised)
  required <- c(token, f0, time, tone)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!is.null(n_bins)) {
    warning("`n_bins` is deprecated: use `n_points`. Tokens are now ",
            "resampled at equally spaced time points rather than binned.",
            call. = FALSE)
    if (missing(n_points)) n_points <- n_bins
  }

  # Same time handling as the model fits (see resolve_time_norm), so the
  # observed mean contours share the fitted models' time axis: per-token
  # min-max rescale, unless the column is already normalised to [0, 1].
  tn  <- resolve_time_norm(data, time, token, time_normalised)$time_norm
  y   <- suppressWarnings(as.numeric(data[[f0]]))
  key <- token_group_key(data[[token]])
  ok  <- is.finite(tn) & is.finite(y)
  by_token <- split(which(ok), key[ok])
  usable   <- vapply(by_token, function(i) length(unique(tn[i])) >= 2, logical(1))
  n_drop   <- length(unique(key)) - sum(usable)
  if (n_drop > 0) {
    message("Dropped ", n_drop, " token(s) with f0 at fewer than two time points.")
  }
  by_token <- by_token[usable]
  if (length(by_token) == 0) {
    stop("No token has f0 at two or more time points to average.", call. = FALSE)
  }

  if (is.null(n_points)) n_points <- 21L
  n_points <- suppressWarnings(as.integer(n_points))
  if (length(n_points) != 1 || is.na(n_points) || n_points < 2) {
    stop("`n_points` must be a single number of at least 2.", call. = FALSE)
  }
  # Resample every token at the same time points, so each counts once at
  # every point however many f0 samples it has. Nothing is extrapolated past
  # a token's first or last f0 value, but a grid point within a tenth of a
  # step of it takes that value, so times rounded to the millisecond (or
  # floating-point error) do not drop a measured end point.
  grid <- seq(0, 1, length.out = n_points)
  tol  <- 0.1 / (n_points - 1)
  vals <- vapply(by_token, function(i) {
    v <- stats::approx(tn[i], y[i], xout = grid, ties = mean, rule = 2)$y
    v[grid < min(tn[i]) - tol | grid > max(tn[i]) + tol] <- NA
    v
  }, numeric(n_points))
  vals <- matrix(vals, nrow = n_points)   # time points x tokens
  tok_tone <- vapply(by_token, function(i) as.character(data[[tone]][i[1]]),
                     character(1))

  # Mean across each tone's tokens at every time point; points no token
  # covers are dropped. Tones in C-locale order, as dplyr::arrange() gives.
  # The radix sort stops on non-ASCII labels in the native encoding, such as
  # Chinese tone names read by read.csv(), so it orders their UTF-8 form; the
  # labels themselves are kept.
  tone_lv <- unique(tok_tone)
  tone_lv <- tone_lv[order(enc2utf8(tone_lv), method = "radix")]
  out <- do.call(rbind, lapply(tone_lv, function(tv) {
    data.frame(tone = tv, time = grid,
               f0_predicted = rowMeans(vals[, tok_tone == tv, drop = FALSE],
                                       na.rm = TRUE),
               stringsAsFactors = FALSE)
  }))
  out <- out[is.finite(out$f0_predicted), , drop = FALSE]
  rownames(out) <- NULL
  out
}


#' Classify a Chao tone numeral string as a shape
#'
#' @description
#' Returns a short human-readable shape name (e.g., `"level"`,
#' `"rising"`, `"falling"`, `"dipping"`, `"peaking"`, or one of the
#' finer-grained `"low level"` / `"mid level"` / `"high level"` labels
#' for repeated-digit strings). Used by [contour_to_chao()] to add a
#' `shape` column to its output, but also exposed as a standalone
#' utility for classifying numerals from external sources.
#'
#' @details
#' Classification rules:
#'
#' * All digits identical: one of `"low level"`, `"mid-low level"`,
#'   `"mid level"`, `"mid-high level"`, `"high level"` according to
#'   the digit value.
#' * Three or more digits with an interior minimum below both
#'   endpoints: `"dipping"`.
#' * Three or more digits with an interior maximum above both
#'   endpoints: `"peaking"`.
#' * Otherwise, `"rising"` if the last digit is higher than the first,
#'   `"falling"` if lower, `"level"` if equal.
#'
#' @param chao_str A character string of Chao digits, e.g. `"55"`,
#'   `"214"`, `"35"`.
#'
#' @return A character string naming the shape.
#'
#' @seealso [contour_to_chao()] which calls this internally to populate
#'   the `shape` column.
#'
#' @references
#' Chao, Y. R. (1930). A system of tone-letters. \emph{Le Maître
#' Phonétique}, 30, 24–27.
#'
#' Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation
#' tone production studies: Methodology and recommendations.
#' \emph{The Journal of the Acoustical Society of America}, 156(4),
#' 2538–2565. \doi{10.1121/10.0032356}
#'
#' @examples
#' classify_contour("55")    # "high level"
#' classify_contour("214")   # "dipping"
#' classify_contour("35")    # "rising"
#' classify_contour("51")    # "falling"
#'
#' @export
classify_contour <- function(chao_str) {
  digits <- as.integer(strsplit(as.character(chao_str), "")[[1]])
  n <- length(digits)
  if (n < 2) return("level")

  first <- digits[1]
  last  <- digits[n]

  if (all(digits == digits[1])) {
    level_names <- c("1" = "low level", "2" = "mid-low level",
                     "3" = "mid level", "4" = "mid-high level",
                     "5" = "high level")
    return(unname(level_names[as.character(digits[1])]))
  }

  if (n >= 3) {
    interior <- digits[2:(n - 1)]
    if (min(interior) < first && min(interior) < last) return("dipping")
    if (max(interior) > first && max(interior) > last) return("peaking")
  }

  if (last > first) return("rising")
  if (last < first) return("falling")
  "level"
}


#' Write Chao tone numerals as IPA tone letters
#'
#' @description
#' Converts Chao tone numerals to the IPA tone letters that Chao (1930)
#' designed them for, one letter per digit: 1 = ˩ (extra low),
#' 2 = ˨, 3 = ˧, 4 = ˦, 5 = ˥ (extra high). For example
#' `"35"` becomes ˧˥ and `"214"` becomes ˨˩˦.
#'
#' @details
#' Each digit becomes one letter, so a level tone keeps both of its digits:
#' `"55"` becomes ˥˥. Some descriptions write a level tone with a
#' single letter; `substr(x, 1, 1)` gives that form.
#'
#' Fonts with tone-letter support (Charis SIL, Doulos SIL, Gentium) join a
#' sequence of letters into a single contour line; other fonts show the
#' letters as separate bars. The characters are the same either way.
#'
#' @param x A character (or integer) vector of Chao numerals, e.g.
#'   `c("55", "35", "214", "51")`, such as the `refline` or `interval`
#'   column of [contour_to_chao()].
#'
#' @return A character vector the same length as `x`. Elements that are
#'   `NA`, or that contain anything other than the digits 1 to 5, become
#'   `NA` (the latter with a warning).
#'
#' @seealso [contour_to_chao()], whose numeral columns this converts, and
#'   [classify_contour()] for a shape name.
#'
#' @references
#' Chao, Y. R. (1930). A system of tone-letters. \emph{Le Maître
#' Phonétique}, 30, 24–27.
#'
#' @examples
#' chao_to_letters(c("55", "35", "214", "51"))
#'
#' @export
chao_to_letters <- function(x) {
  x <- as.character(x)
  bad <- !is.na(x) & !grepl("^[1-5]+$", x)
  if (any(bad)) {
    warning("Not a Chao numeral (digits 1 to 5 only): ",
            paste(unique(x[bad]), collapse = ", "), call. = FALSE)
    x[bad] <- NA_character_
  }
  chartr("12345", "\u02e9\u02e8\u02e7\u02e6\u02e5", x)
}


#' Convert per-tone contours to Chao tone numerals
#'
#' @description
#' Summarises per-tone contours as Chao tone numerals (Chao 1930): 5-level
#' digit strings like `"55"`, `"35"`, or `"214"` that describe each
#' contour by its endpoints and any interior turning point. Two
#' conversion methods are computed in one pass so the user can compare
#' them (Xu & Zhang 2024).
#'
#' @details
#' ## The two conversion methods
#'
#' Both use Fraction of Range (FOR) normalisation, which maps f0 onto a
#' bounded scale between `f0_min` and `f0_max`, the lowest and highest
#' points of `contour_data`. With mean contours from
#' [compute_mean_contour()], each of these points is itself an average
#' over tokens, not a single f0 measurement. The two methods differ in
#' how scaled values are mapped to Chao digits.
#'
#' * **Reference-line FOR on `[1, 5]`**:
#'   `f0' = (f0 - f0_min) / (f0_max - f0_min) * 4 + 1`, then rounded.
#'   Five reference lines at integer values; each sampled point snaps
#'   to the nearest integer.
#' * **Interval-based FOR on `(0, 5]`**:
#'   `f0' = (f0 - f0_min) / (f0_max - f0_min) * 5`, then ceilinged.
#'   Five equal-width intervals; each sampled point is assigned to its
#'   interval via `ceiling()`.
#'
#' ## Input scale
#'
#' Both methods are linear in the units of the input, so the input
#' sets the pitch scale of the numerals. Semitones and LZ from
#' [normalise_f0()] give a log-frequency scale, on which equal steps are
#' equal pitch ratios, as pitch is heard; Hz and z-scores give a scale
#' linear in Hz. Negative values, as in speaker-normalised f0, need no
#' special handling.
#'
#' f0 in Hz from several speakers should be normalised by speaker first,
#' for example to LZ, the log z-score of Zhu (1999): averaging Hz mixes
#' higher and lower voices, which shifts the mean contours. The app's Chao
#' tab converts raw f0 to LZ by speaker automatically.
#'
#' ## Turning-point detection
#'
#' For each tone, the function samples the contour either at its two
#' endpoints or at the two endpoints plus one interior turning point. The
#' turning point is the lowest point between 10% and 90% of the contour if
#' it lies below both endpoints (a *dip*), or the highest point if it lies
#' above both (a *peak*), whichever goes further, and it is sampled when it
#' goes more than `threshold` Chao units beyond the nearer endpoint.
#'
#' Each scale then keeps the middle digit only if it differs from both
#' endpoint digits. A middle digit that repeats an endpoint digit merges
#' into it, since the turn is too shallow to show at five levels: `23`, not
#' `223` (which would read as a late rise). The turning point lies beyond
#' both endpoints, so its digit can only match an endpoint digit or go past
#' it. The two scales round differently, so a tone can get two digits on one
#' and three on the other.
#'
#' The default `threshold = 0.5` (half a Chao unit on the `[1, 5]`
#' scale) is a conservative choice. Lower thresholds sample more turning
#' points and so give more 3-digit numerals, useful for tonally rich
#' languages (a sampled turn too shallow to show is still left out). Any
#' value from 4
#' (the height of the scale) up means no turning point is sampled. The
#' `dip` and `peak` columns report how far each tone's turning point goes,
#' so a tone close to the threshold, whose digit count depends on it, can
#' be spotted.
#'
#' ## Timing of rises and falls
#'
#' The numerals give pitch levels, not when a contour moves: an early and a
#' late rise between the same levels get the same digits. So for a rise or
#' a fall the function also reports how much of the movement is done at the
#' middle of the contour. `move` is the change from onset to offset, and
#' `progress` is the share of it covered at 50% of the time axis: 0.5 for a
#' movement spread evenly over the contour, 0 for a contour that stays level
#' until 50% and then moves, and 1 for one that has finished moving by 50%.
#' `timing` names it, with cut-offs midway between these: `"early"` above
#' 0.75, `"late"` below 0.25 and `"mid"` in between. A contour that stays
#' level and then moves in a straight line is late once the level part lasts
#' more than a third of it. One that dips slightly before it rises is late
#' too, so check `dip`.
#'
#' Both are `NA` for a movement of less than half a Chao unit, too small to
#' time, and for a tone with a sampled turning point, which is not a simple
#' rise or fall. They describe the contour on normalised time, so they give
#' proportional, not absolute, timing, and tokens that differ in timing
#' average towards `"mid"`.
#'
#' @param contour_data Per-tone contour data, typically from
#'   [compute_mean_contour()], [predict_gca()], or [predict_gamm()].
#'   Must contain `tone`, `time`, and `f0_predicted` columns (or rename
#'   via the `tone_col`, `time_col`, `f0_col` arguments).
#' @param tone_col,time_col,f0_col Column names within `contour_data`.
#' @param threshold Chao-units threshold for turning-point detection.
#'   Default `0.5`. See Details.
#' @param raw_data,raw_token,raw_f0,raw_tone Deprecated and ignored, with
#'   a warning. They supplied token-level data for a third, "robust"
#'   method, which has been removed: its range, one SD beyond the mean
#'   token extremes, squeezed averaged contours towards the middle digits
#'   and moved more with pitch-tracking errors than the range of the
#'   contours themselves.
#'
#' @return A data frame with one row per tone, containing:
#' * `tone`: tone label.
#' * `n_digits`, `n_digits_int`: number of digits (`2` or `3`) in the
#'   reference-line and interval-based numerals.
#' * `refline`: reference-line method numeral, e.g. `"55"`.
#' * `interval`: interval-based numeral, e.g. `"45"`.
#' * `shape`: shape name from [classify_contour()] applied to `refline`.
#' * `dip`, `peak`: how far the lowest (highest) point between 10% and
#'   90% of the contour lies below (above) both endpoints, in Chao units
#'   on the reference-line scale; zero or negative when the contour has no
#'   dip (peak). The larger of the two is sampled as a turning point when
#'   it exceeds `threshold`; it adds a middle digit where that digit shows
#'   the turn (see Details).
#' * `move`: the change from onset to offset, in Chao units on the
#'   reference-line scale (positive for a rise, negative for a fall).
#' * `progress`, `timing`: the share of `move` done at 50% of the contour,
#'   and its name, `"early"` (above 0.75), `"mid"` or `"late"` (below
#'   0.25). `NA` for a movement under half a unit or a sampled turning
#'   point (see Details).
#' * `sample_times`, `ref_continuous`, `int_continuous`: list columns of
#'   the per-sample numeric values underlying the digit strings, useful
#'   for plotting or further analysis. When a turning point is sampled
#'   these have three values even if a numeral keeps only two digits.
#'
#' Three attributes are attached to the returned data frame: `f0_min` and
#' `f0_max`, the lowest and highest points of `contour_data`, which set
#' the range, and `threshold`.
#'
#' @seealso
#' * [compute_mean_contour()] for one common upstream step.
#' * [classify_contour()] for the shape classification helper.
#' * [predict_gca()] and [predict_gamm()] for model-based contour
#'   sources.
#'
#' @references
#' Chao, Y. R. (1930). A system of tone-letters. \emph{Le Maître
#' Phonétique}, 30, 24–27.
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
#' # Normalise by speaker first (here LZ), so that higher and lower voices
#' # are averaged on a common scale.
#' lz <- normalise_f0(sample_f0, f0 = "f0_Hz", speaker = "speaker",
#'                    tone = "tone", method = "lz")
#' mc <- compute_mean_contour(lz, token = "token", f0 = "f0_lz",
#'                            time = "time", tone = "tone")
#' chao <- contour_to_chao(mc)
#' chao[, c("tone", "refline", "interval", "shape")]
#'
#' # Semitones also give numerals on a log-frequency scale, but keep each
#' # speaker's own pitch range.
#' st <- normalise_f0(sample_f0, f0 = "f0_Hz", speaker = "speaker",
#'                    tone = "tone", method = "semitone")
#' mc_st <- compute_mean_contour(st, token = "token", f0 = "f0_st",
#'                               time = "time", tone = "tone")
#' chao_st <- contour_to_chao(mc_st)
#' chao_st[, c("tone", "refline", "interval", "shape")]
#'
#' @export
#' @importFrom dplyr filter group_by summarise
#' @importFrom rlang .data
#' @importFrom stats approxfun sd
contour_to_chao <- function(contour_data,
                            tone_col  = "tone",
                            time_col  = "time",
                            f0_col    = "f0_predicted",
                            threshold = 0.5,
                            raw_data  = NULL,
                            raw_token = "token",
                            raw_f0    = "f0",
                            raw_tone  = "tone") {

  required <- c(tone_col, time_col, f0_col)
  missing_cols <- setdiff(required, names(contour_data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in contour_data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold) ||
      threshold < 0) {
    stop("`threshold` must be a single number, 0 or more.", call. = FALSE)
  }
  if (!is.null(raw_data) || !missing(raw_token) || !missing(raw_f0) ||
      !missing(raw_tone)) {
    warning("`raw_data`, `raw_token`, `raw_f0` and `raw_tone` are deprecated ",
            "and ignored: the robust method that used them has been removed.",
            call. = FALSE)
  }

  # Normalise input column names internally.
  cd <- data.frame(
    tone         = as.character(contour_data[[tone_col]]),
    time         = as.numeric(contour_data[[time_col]]),
    f0_predicted = as.numeric(contour_data[[f0_col]]),
    stringsAsFactors = FALSE
  )

  # --- Reference-line + interval continuous scales ------------------------
  f0_min <- min(cd$f0_predicted, na.rm = TRUE)
  f0_max <- max(cd$f0_predicted, na.rm = TRUE)
  if (f0_max == f0_min) {
    cd$chao_ref <- 3      # [1, 5] midpoint
    cd$chao_int <- 2.5    # (0, 5] midpoint
  } else {
    cd$chao_ref <- (cd$f0_predicted - f0_min) / (f0_max - f0_min) * 4 + 1
    cd$chao_int <- (cd$f0_predicted - f0_min) / (f0_max - f0_min) * 5
  }

  # --- Per-tone numeral extraction ----------------------------------------
  tone_levels <- unique(cd$tone)
  rows <- vector("list", length(tone_levels))

  for (i in seq_along(tone_levels)) {
    tv <- tone_levels[i]
    td <- cd[cd$tone == tv & !is.na(cd$chao_ref) & !is.na(cd$time), ]
    td <- td[order(td$time), ]
    if (nrow(td) < 2) next

    interp_ref <- stats::approxfun(td$time, td$chao_ref, rule = 2)
    interp_int <- stats::approxfun(td$time, td$chao_int, rule = 2)

    onset_ref  <- interp_ref(0)
    offset_ref <- interp_ref(1)

    fine_time <- seq(0, 1, length.out = 200)
    fine_ref  <- interp_ref(fine_time)
    interior_min_idx <- which.min(fine_ref[20:180]) + 19
    interior_max_idx <- which.max(fine_ref[20:180]) + 19
    interior_min <- fine_ref[interior_min_idx]
    interior_max <- fine_ref[interior_max_idx]

    # How far the turning point lies beyond BOTH endpoints (Chao units).
    dip  <- min(onset_ref, offset_ref) - interior_min
    peak <- interior_max - max(onset_ref, offset_ref)
    # Sample the larger of the two turns when it exceeds the threshold, so a
    # lower threshold never swaps one turn for another (only adds one).
    use_dip <- dip >= peak
    turn    <- if (use_dip) dip else peak
    sample_times <- if (turn <= threshold) c(0, 1)
                    else if (use_dip) c(0, fine_time[interior_min_idx], 1)
                    else c(0, fine_time[interior_max_idx], 1)

    # Timing of a rise or fall: the share of the onset-to-offset movement
    # done at the middle of the contour (0.5 for an even movement). Not for
    # a movement under half a unit, too small to time, or a sampled turn.
    move     <- offset_ref - onset_ref
    progress <- if (abs(move) >= 0.5 && length(sample_times) == 2)
                  (interp_ref(0.5) - onset_ref) / move else NA_real_
    timing   <- if (is.na(progress)) NA_character_
                else if (progress > 0.75) "early"
                else if (progress < 0.25) "late"
                else "mid"
    ref_vals <- interp_ref(sample_times)
    int_vals <- interp_int(sample_times)
    ref_dig  <- pmax(1L, pmin(5L, round(ref_vals)))
    int_dig  <- pmax(1L, pmin(5L, ceiling(int_vals)))

    # Each scale keeps the middle digit only if it differs from both endpoint
    # digits; one that repeats an endpoint digit merges into it (223 -> 23,
    # 222 -> 22). The turn lies beyond both endpoints, so its digit can only
    # match an endpoint digit or go past it: "differs" means "lower than both"
    # for a dip and "higher than both" for a peak.
    if (length(sample_times) == 3) {
      shows <- function(d) d[2] != d[1] && d[2] != d[3]
      if (!shows(ref_dig)) ref_dig <- ref_dig[-2]
      if (!shows(int_dig)) int_dig <- int_dig[-2]
    }
    refline_str  <- paste0(ref_dig, collapse = "")
    interval_str <- paste0(int_dig, collapse = "")

    rows[[i]] <- data.frame(
      tone     = tv,
      n_digits = length(ref_dig),
      n_digits_int = length(int_dig),
      refline  = refline_str,
      interval = interval_str,
      shape    = classify_contour(refline_str),
      dip      = dip,
      peak     = peak,
      move     = move,
      progress = progress,
      timing   = timing,
      stringsAsFactors = FALSE
    )
    # List columns with the per-sample numeric values (for downstream plots).
    rows[[i]]$sample_times    <- I(list(sample_times))
    rows[[i]]$ref_continuous  <- I(list(ref_vals))
    rows[[i]]$int_continuous  <- I(list(int_vals))
  }

  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  rownames(out) <- NULL

  # Attach summary stats as attributes so callers (e.g. the Shiny UI)
  # can display them without recomputing.
  attr(out, "f0_min") <- f0_min
  attr(out, "f0_max") <- f0_max
  attr(out, "threshold") <- threshold
  out
}
