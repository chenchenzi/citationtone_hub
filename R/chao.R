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
  out <- do.call(rbind, lapply(sort(unique(tok_tone), method = "radix"), function(tv) {
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
#' endpoints (yielding a 2-digit numeral) or at the two endpoints plus
#' one interior turning point (yielding a 3-digit numeral). The 3-digit
#' case applies when either:
#'
#' * The interior minimum is more than `threshold` Chao units below
#'   both endpoints (a *dip*), or
#' * The interior maximum is more than `threshold` Chao units above
#'   both endpoints (a *peak*).
#'
#' The default `threshold = 0.5` (half a Chao unit on the `[1, 5]`
#' scale) is a conservative choice. Lower thresholds produce more
#' 3-digit numerals, useful for tonally rich languages.
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
#' * `n_digits`: number of digits in the numeral (`2` or `3`).
#' * `refline`: reference-line method numeral, e.g. `"55"`.
#' * `interval`: interval-based numeral, e.g. `"45"`.
#' * `shape`: shape name from [classify_contour()] applied to `refline`.
#' * `sample_times`, `ref_continuous`, `int_continuous`: list columns of
#'   the per-sample numeric values underlying the digit strings, useful
#'   for plotting or further analysis.
#'
#' Two attributes are attached to the returned data frame: `f0_min` and
#' `f0_max`, the lowest and highest points of `contour_data`, which set
#' the range.
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

    has_dip  <- (onset_ref - interior_min  > threshold) &&
                (offset_ref - interior_min > threshold)
    has_peak <- (interior_max - onset_ref  > threshold) &&
                (interior_max - offset_ref > threshold)

    sample_times <- if (has_dip)  c(0, fine_time[interior_min_idx], 1)
                    else if (has_peak) c(0, fine_time[interior_max_idx], 1)
                    else c(0, 1)
    n_digits <- length(sample_times)

    ref_vals <- interp_ref(sample_times)
    int_vals <- interp_int(sample_times)

    refline_str  <- paste0(pmax(1L, pmin(5L, round(ref_vals))),   collapse = "")
    interval_str <- paste0(pmax(1L, pmin(5L, ceiling(int_vals))), collapse = "")

    rows[[i]] <- data.frame(
      tone     = tv,
      n_digits = n_digits,
      refline  = refline_str,
      interval = interval_str,
      shape    = classify_contour(refline_str),
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
  out
}
