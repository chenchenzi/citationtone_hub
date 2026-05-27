# =============================================================================
# Chao tone numeral summarisation.
#
# Three exported functions:
#   * compute_mean_contour()  long-format f0 -> per-tone mean contour
#   * classify_contour()      Chao digit string -> shape name
#   * contour_to_chao()       per-tone contour -> Chao numerals (all 3 methods)
# =============================================================================


#' Compute the per-tone mean f0 contour from long-format data
#'
#' Normalise time to `[0, 1]` per token, bin into 50 equally-spaced time
#' points, then average f0 across all tokens of each tone. Returns the
#' long-format mean contour expected by [contour_to_chao()].
#'
#' @param data Long-format data frame, one row per f0 sample.
#' @param token,f0,time,tone Column names.
#' @param n_bins Number of evenly-spaced time bins across `[0, 1]`.
#'   Default `50`.
#'
#' @return A data frame with columns `tone`, `time`, `f0_predicted`,
#'   one row per (tone, time-bin).
#'
#' @export
#' @importFrom dplyr group_by mutate ungroup summarise arrange n
#' @importFrom rlang .data
compute_mean_contour <- function(data,
                                 token = "token",
                                 f0    = "f0",
                                 time  = "time",
                                 tone  = "tone",
                                 n_bins = 50) {
  required <- c(token, f0, time, tone)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  dat <- data |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::mutate(
      .time_norm = {
        t_raw <- .data[[time]]
        t_min <- min(t_raw, na.rm = TRUE)
        t_max <- max(t_raw, na.rm = TRUE)
        if (is.na(t_min) || t_max == t_min) rep(0.5, dplyr::n())
        else (t_raw - t_min) / (t_max - t_min)
      }
    ) |>
    dplyr::ungroup()

  dat$.time_bin <- round(dat$.time_norm * (n_bins - 1)) / (n_bins - 1)
  dat$.f0       <- as.numeric(dat[[f0]])
  dat$.tone     <- as.character(dat[[tone]])

  dat |>
    dplyr::group_by(tone = .data$.tone, time = .data$.time_bin) |>
    dplyr::summarise(f0_predicted = mean(.data$.f0, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::arrange(.data$tone, .data$time) |>
    as.data.frame()
}


#' Classify a Chao tone numeral string as a shape
#'
#' Returns a short shape name ("level", "rising", "falling", "dipping",
#' "peaking", or a finer "low level" / "mid level" / "high level" for
#' single-digit / repeated-digit strings).
#'
#' @param chao_str A character string of Chao digits (1-5), e.g. `"55"`,
#'   `"214"`, `"35"`.
#'
#' @return A character string naming the shape.
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


#' Convert per-tone contours to Chao tone numerals
#'
#' For each tone in `contour_data`, sample the contour at its turning
#' points (or at the endpoints if no turning point is detected), then map
#' each sample to a Chao digit (1-5) using up to three methods:
#'
#' * **Reference-line FOR `[1, 5]`** — `f0' = (f0 - f0_min) / (f0_max - f0_min) * 4 + 1`,
#'   rounded.
#' * **Interval-based FOR `(0, 5]`** — `f0' = (f0 - f0_min) / (f0_max - f0_min) * 5`,
#'   ceiling.
#' * **Robust FOR `(0, 5]`** (optional) — same formula but with the range
#'   defined by `mu ± sigma` of per-token f0 peaks from the highest-pitched
#'   tone (max) and per-token f0 valleys from the lowest-pitched tone
#'   (min). Computed only if `raw_data` is supplied.
#'
#' The turning-point detection uses a Chao-units `threshold` (default
#' `0.5`) on the `[1, 5]` reference scale: a tone gets 3 digits if either
#' a dip or peak exceeds the threshold relative to its endpoints,
#' otherwise 2 digits.
#'
#' @param contour_data Per-tone contour data, typically from
#'   [compute_mean_contour()], [predict_gca()], or [predict_gamm()]. Must
#'   contain `tone`, `time`, and `f0_predicted` columns (or rename via the
#'   `tone_col`, `time_col`, `f0_col` arguments).
#' @param tone_col,time_col,f0_col Column names within `contour_data`.
#' @param threshold Chao-units threshold for turning-point detection.
#'   Default `0.5`.
#' @param raw_data Optional long-format raw data, used to compute the
#'   robust FOR. If `NULL`, the `robust` column is left as `NA`.
#' @param raw_token,raw_f0,raw_tone Column names in `raw_data`.
#'
#' @return A data frame with one row per tone, containing:
#' * `tone` — tone label
#' * `n_digits` — number of digits in the numeral (2 or 3)
#' * `refline` — reference-line method numeral, e.g. `"55"`
#' * `interval` — interval-based numeral, e.g. `"45"`
#' * `robust` — robust FOR numeral (or `NA` if `raw_data` not given)
#' * `shape` — shape name from [classify_contour()] applied to `refline`
#' * `sample_times`, `ref_continuous`, `int_continuous`, `rob_continuous`
#'   — list columns of the per-sample numeric values
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
  if (!is.null(raw_data)) {
    raw_required <- c(raw_token, raw_f0, raw_tone)
    raw_missing  <- setdiff(raw_required, names(raw_data))
    if (length(raw_missing) > 0) {
      stop("Column(s) not found in raw_data: ",
           paste(raw_missing, collapse = ", "), call. = FALSE)
    }
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

  # --- Robust FOR (optional) ----------------------------------------------
  has_robust <- !is.null(raw_data)
  cd$chao_rob <- NA_real_
  if (has_robust) {
    raw <- raw_data
    raw$.f0 <- suppressWarnings(as.numeric(raw[[raw_f0]]))
    raw <- raw[!is.na(raw$.f0) & raw$.f0 > 0, , drop = FALSE]

    tone_means <- raw |>
      dplyr::group_by(.data[[raw_tone]]) |>
      dplyr::summarise(.m = mean(.data$.f0, na.rm = TRUE), .groups = "drop")
    highest <- tone_means[[raw_tone]][which.max(tone_means$.m)]
    lowest  <- tone_means[[raw_tone]][which.min(tone_means$.m)]

    peaks <- raw[raw[[raw_tone]] == highest, ] |>
      dplyr::group_by(.data[[raw_token]]) |>
      dplyr::summarise(.pk = max(.data$.f0, na.rm = TRUE), .groups = "drop")
    valleys <- raw[raw[[raw_tone]] == lowest, ] |>
      dplyr::group_by(.data[[raw_token]]) |>
      dplyr::summarise(.vl = min(.data$.f0, na.rm = TRUE), .groups = "drop")

    mu_max    <- mean(peaks$.pk,   na.rm = TRUE)
    sigma_max <- stats::sd(peaks$.pk,   na.rm = TRUE)
    mu_min    <- mean(valleys$.vl, na.rm = TRUE)
    sigma_min <- stats::sd(valleys$.vl, na.rm = TRUE)
    upper     <- mu_max + sigma_max
    lower     <- max(mu_min - sigma_min, 1)  # floor at 1 Hz to avoid log(0)
    log_range <- log(upper) - log(lower)
    if (log_range > 0) {
      cd$chao_rob <- pmax(0, pmin(5,
        (log(cd$f0_predicted) - log(lower)) / log_range * 5))
    } else {
      has_robust <- FALSE
    }
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
    interp_rob <- if (has_robust) stats::approxfun(td$time, td$chao_rob, rule = 2)
                  else NULL

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
    rob_vals <- if (has_robust) pmax(0, pmin(5, interp_rob(sample_times)))
                else rep(NA_real_, n_digits)

    refline_str  <- paste0(pmax(1L, pmin(5L, round(ref_vals))),   collapse = "")
    interval_str <- paste0(pmax(1L, pmin(5L, ceiling(int_vals))), collapse = "")
    robust_str   <- if (has_robust)
                      paste0(pmax(1L, pmin(5L, ceiling(rob_vals))), collapse = "")
                    else NA_character_

    rows[[i]] <- data.frame(
      tone     = tv,
      n_digits = n_digits,
      refline  = refline_str,
      interval = interval_str,
      robust   = robust_str,
      shape    = classify_contour(refline_str),
      stringsAsFactors = FALSE
    )
    # List columns with the per-sample numeric values (for downstream plots).
    rows[[i]]$sample_times    <- I(list(sample_times))
    rows[[i]]$ref_continuous  <- I(list(ref_vals))
    rows[[i]]$int_continuous  <- I(list(int_vals))
    rows[[i]]$rob_continuous  <- I(list(rob_vals))
  }

  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  rownames(out) <- NULL

  # Attach summary stats as attributes so callers (e.g. the Shiny UI)
  # can display them without recomputing.
  attr(out, "f0_min") <- f0_min
  attr(out, "f0_max") <- f0_max
  if (has_robust) {
    attr(out, "robust_stats") <- list(
      highest_tone = highest, lowest_tone = lowest,
      mu_max = mu_max, sigma_max = sigma_max,
      mu_min = mu_min, sigma_min = sigma_min,
      upper  = upper,  lower      = lower
    )
  } else {
    attr(out, "robust_stats") <- NULL
  }
  out
}
