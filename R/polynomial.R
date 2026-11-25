# =============================================================================
# Per-token Legendre polynomial fitting for f0 contours.
# =============================================================================

#' Fit Legendre polynomials to f0 contours, token by token
#'
#' @description
#' For each token, normalises the time axis to `[-1, 1]`, constructs a
#' Legendre polynomial basis of the requested degree, and fits ordinary
#' least squares. Returns one row per token with the fitted
#' coefficients `c0`, `c1`, ..., `c{degree}`. Useful as a compact,
#' interpretable summary of contour shape, or as features for downstream
#' classifiers and regression models (Mirman 2014; Xu & Zhang 2024).
#'
#' @details
#' ## What the function does internally
#'
#' For each token:
#'
#' 1. Rescale each token's `time` linearly to `[-1, 1]`, from its first f0
#'    (-1) to its last f0 (1), which is where Legendre polynomials are
#'    orthogonal. Rows without f0 at a token's edges do not move it. If the
#'    time column is *already* normalised to `[0, 1]`
#'    (detected with [time_already_normalised()], or declared via
#'    `time_normalised`), the fixed map `2 * t - 1` is used instead, so
#'    tokens whose samples do not span the full unit interval are not
#'    stretched a second time. A `<tier>_tseq` (syllables in sequence) or
#'    `<tier>_tbound` (in sequence, gap kept) is rescaled per token too: a
#'    token whose first or last syllable has
#'    no values on the axis (on a `<tier>_tseq`, f0 at fewer than two times;
#'    on a `<tier>_tbound`, no f0) is stretched, and its other syllables
#'    fill the axis.
#' 2. Build the Legendre basis up to the requested `degree`.
#' 3. Solve the ordinary least-squares problem with [stats::lm.fit()] to
#'    recover the coefficients.
#'
#' ## Interpreting the coefficients
#'
#' Because Legendre polynomials are orthogonal on `[-1, 1]`, the fitted
#' coefficients have a tidy shape interpretation:
#'
#' * `c0`: token-level mean f0.
#' * `c1`: linear slope across the contour. Positive means rising.
#' * `c2`: quadratic curvature. Positive means U-shaped (dipping),
#'   negative means inverted-U (peaking).
#' * `c3`: cubic shape. Differentiates sigmoid from reverse-sigmoid
#'   contours.
#'
#' ## Edge cases
#'
#' Samples with a missing or infinite f0 or time are skipped (`-Inf` comes
#' from taking the log of 0 Hz), and so are 0 values in a column in Hz (one
#' that [hz_f0_columns()] finds), which have no f0. The checks below count
#' only the samples that remain, so rows without f0 do not change them. A
#' token whose samples all share one time point keeps `c0` as the mean of
#' their f0 and sets the higher-order coefficients to `NA`. Any other token
#' with fewer than `degree + 1` samples gets `NA` for all coefficients. So
#' does a token with no samples.
#'
#' ## Speaker and tone
#'
#' `speaker` and `tone` are read once per token. Each token takes the most
#' frequent non-empty value among its rows with f0 (a finite f0, and above 0
#' in a column in Hz, as in "Edge cases"). A tie goes to the value that comes
#' first in time. So a label column that changes within
#' a token, such as a tone tier from a TextGrid with `""` in the silences,
#' gives the same result in any row order, with or without rows without f0.
#'
#' @param data A long-format data frame with one row per f0 sample.
#' @param f0 Column name of f0 values. Default `"f0"`. Using normalised
#'   f0 (e.g. `"f0_st"` from [normalise_f0()]) is recommended for
#'   cross-speaker comparison.
#' @param token Column name of token ID. Default `"token"`.
#' @param time Column name of time within each token. Default `"time"`.
#' @param speaker Column name of speaker ID, carried through to the
#'   output (see "Speaker and tone" below). Default `"speaker"`.
#' @param tone Column name of tone category, carried through to the
#'   output (see "Speaker and tone" below). Default `"tone"`.
#' @param degree Polynomial degree. One of `1`, `2`, or `3`. Default `2`.
#' @param time_normalised One of `"auto"` (default: detect an
#'   already-normalised `[0, 1]` time column and map it to `[-1, 1]`
#'   with the fixed transform `2 * t - 1`), `"no"` (always rescale to
#'   `[-1, 1]` within each token), or `"yes"` (declare the column
#'   already normalised to `[0, 1]`). See [resolve_time_norm()].
#'
#' @return A token-level data frame with one row per token, containing
#'   the `token`, `speaker`, `tone`, and coefficient columns `c0`,
#'   `c1`, ..., `c{degree}`. The attribute `time_prenormalised` records
#'   whether the time column was treated as already normalised.
#'
#' @seealso
#' * [normalise_f0()] for the upstream normalisation step.
#' * [fit_gca()] for a population-level mixed-effects extension of
#'   polynomial contour modelling.
#'
#' @references
#' Mirman, D. (2014). \emph{Growth Curve Analysis and Visualization Using
#' R}. Chapman and Hall/CRC.
#'
#' Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation
#' tone production studies: Methodology and recommendations.
#' \emph{The Journal of the Acoustical Society of America}, 156(4),
#' 2538–2565. \doi{10.1121/10.0032356}
#'
#' @examples
#' data(sample_f0)
#' normed <- normalise_f0(sample_f0,
#'                        f0      = "f0_Hz",
#'                        speaker = "speaker",
#'                        tone    = "tone")
#' coefs <- fit_polynomial(normed,
#'                         f0      = "f0_st",
#'                         token   = "token",
#'                         time    = "time",
#'                         speaker = "speaker",
#'                         tone    = "tone",
#'                         degree  = 2)
#' head(coefs)
#'
#' @export
#' @importFrom dplyr arrange group_by group_modify summarise across all_of first left_join select ungroup mutate
#' @importFrom rlang .data
#' @importFrom stats lm.fit setNames
fit_polynomial <- function(data,
                           f0      = "f0",
                           token   = "token",
                           time    = "time",
                           speaker = "speaker",
                           tone    = "tone",
                           degree  = 2,
                           time_normalised = c("auto", "no", "yes")) {

  time_normalised <- match.arg(time_normalised)
  if (!degree %in% 1:3) {
    stop("`degree` must be 1, 2, or 3.", call. = FALSE)
  }

  required <- c(f0, token, time, speaker, tone)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[f0]])) {
    stop("Column '", f0, "' must be numeric.", call. = FALSE)
  }
  if (!is.numeric(data[[time]])) {
    stop("Column '", time, "' must be numeric.", call. = FALSE)
  }

  # Tag f0 internally so the per-group worker doesn't have to re-evaluate
  # the column name on every group. Time is resolved once to [0, 1]: either
  # the column as it is (already normalised) or a per-token rescale from the
  # token's first f0 to its last, so the worker's fixed 2*t - 1 map lands on
  # [-1, 1] either way. The original f0 name is passed, before .f0_fit is
  # made, so the Hz rule (0 is no f0) applies.
  ts <- resolve_time_norm(data, time, token, time_normalised, f0 = f0)
  data <- data |>
    dplyr::mutate(.f0_fit = .data[[f0]])
  # Values follow the axis's rule: 0 in a column in Hz is no f0, so it is
  # skipped, as the rows without f0 at the edges are.
  data$.f0_fit[!f0_rows(data, f0)] <- NA
  data$.t01 <- ts$time_norm
  # A row with no time is not a sample, even where the rescale above gave a
  # one-time token 0.5 on every row.
  data$.t01[!is.finite(data[[time]])] <- NA_real_

  # Per-token metadata: the most frequent non-empty speaker and tone among
  # the token's rows with f0, so neither row order nor rows without f0
  # change them.
  token_meta <- pick_token_values(data, unique(c(speaker, tone)), token,
                                  use = is.finite(data$.f0_fit),
                                  time = data[[time]])

  # Per-token OLS fit on the Legendre basis.
  coef_df <- data |>
    dplyr::arrange(.data[[token]], .data[[time]]) |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::group_modify(
      ~ fit_one_token_polynomial(.x, degree = degree, time_col = ".t01")
    ) |>
    dplyr::ungroup()

  # Attach metadata and fix column order.
  coef_names <- paste0("c", 0:degree)
  out <- coef_df |>
    dplyr::left_join(token_meta, by = token) |>
    dplyr::select(dplyr::all_of(c(token, speaker, tone, coef_names)))
  attr(out, "time_prenormalised") <- ts$prenormalised
  out
}


# -----------------------------------------------------------------------------
# Internal: Legendre polynomial basis on `[-1, 1]` up to a given degree.
# Used by fit_polynomial(); not exported.
# -----------------------------------------------------------------------------
legendre_basis <- function(t, degree) {
  n <- length(t)
  B <- matrix(NA_real_, nrow = n, ncol = degree + 1)
  B[, 1] <- 1                                  # P0
  if (degree >= 1) B[, 2] <- t                 # P1
  # Bonnet recurrence for higher orders: P_k = ((2k-1) t P_{k-1} - (k-1) P_{k-2}) / k.
  # Identical to the closed forms for P2 = (3t^2-1)/2 and P3 = (5t^3-3t)/2,
  # but extends cleanly to any degree (used by the clustering feature space).
  if (degree >= 2) {
    for (k in 2:degree) {
      B[, k + 1] <- ((2 * k - 1) * t * B[, k] - (k - 1) * B[, k - 1]) / k
    }
  }
  colnames(B) <- paste0("c", 0:degree)
  B
}


# -----------------------------------------------------------------------------
# Internal: fit one token. Returns a 1-row data.frame of coefficients.
# `time_col` holds the resolved [0, 1] time from resolve_time_norm(), so the
# fixed 2*t - 1 map below yields the per-token [-1, 1] rescale when time was
# unnormalised, and the untouched proportional positions when it was.
# Used by fit_polynomial() via group_modify(); not exported.
# -----------------------------------------------------------------------------
fit_one_token_polynomial <- function(token_data, degree, time_col) {
  coef_names <- paste0("c", 0:degree)
  result <- stats::setNames(rep(NA_real_, degree + 1), coef_names)

  # Samples are the rows with finite f0 and time: -Inf (log of 0 Hz) breaks
  # lm.fit. Every check below looks at these rows only, so rows without f0
  # never decide which case applies (a token with one sample among rows
  # without f0 gets the same result as that sample alone). The per-token
  # rescale in resolve_time_norm() runs from the first f0 to the last, so
  # rows without f0 never move it.
  t_raw <- token_data[[time_col]]
  y     <- token_data$.f0_fit
  valid <- is.finite(t_raw) & is.finite(y)
  if (!any(valid)) return(as.data.frame(as.list(result)))
  t_raw <- t_raw[valid]
  y     <- y[valid]

  # Degenerate: all samples at one time point. Keep their mean as c0.
  if (max(t_raw) == min(t_raw)) {
    result["c0"] <- mean(y)
    return(as.data.frame(as.list(result)))
  }

  if (length(y) < degree + 1) return(as.data.frame(as.list(result)))

  B   <- legendre_basis(2 * t_raw - 1, degree)
  fit <- stats::lm.fit(B, y)
  as.data.frame(as.list(stats::setNames(fit$coefficients, coef_names)))
}


# -----------------------------------------------------------------------------
# Internal: one value per token for token-level columns such as speaker and
# tone. Each token takes the most frequent non-empty value among its rows in
# `use` (the rows with f0). Empty means NA, or text that is blank once
# trimmed: the "" a TextGrid tier gives an empty interval.
#
# * A token with no row in `use` (no f0 at all) looks at all its rows.
# * When every candidate value is empty, the most frequent empty value wins
#   ("" or NA), so a token whose rows with f0 are all unlabelled stays
#   unlabelled rather than borrowing a label from a row without f0.
# * A tie goes to the value that comes first in `time` (rows with no time
#   last), then to the first in C-locale sort order.
#
# The result depends on neither row order nor the rows outside `use`, so
# removing rows without f0 cannot change it. Returns a data frame with the
# `token` column and `cols`, one row per token in order of first appearance;
# values keep their type (factor levels included). Used by fit_polynomial(),
# cluster_features(), compute_mean_contour(), flag_outliers() and
# flag_level_outliers(); not exported.
# -----------------------------------------------------------------------------
pick_token_values <- function(data, cols, token, use, time = NULL) {
  tok <- data[[token]]
  tk  <- token_group_key(tok)
  n   <- length(tk)
  ntok <- if (n) max(tk) else 0L
  use <- as.logical(use)
  use[is.na(use)] <- FALSE

  # Rows each token picks from: its rows with f0, or all rows when it has none.
  pool <- use | !(tabulate(tk[use], nbins = ntok) > 0)[tk]

  tt <- if (is.null(time)) rep(0, n) else as_time_numeric(time)
  tt[!is.finite(tt)] <- NA_real_

  first_row <- !duplicated(tk)
  out <- data.frame(seq_len(sum(first_row)))
  out[[1]] <- tok[first_row]
  names(out) <- token

  for (col in cols) {
    x   <- data[[col]]
    key <- as.character(x)
    is_na <- is.na(x) | is.na(key)
    empty <- is_na | !nzchar(trimws(key))
    has_full <- (tabulate(tk[pool & !empty], nbins = ntok) > 0)[tk]
    cand <- which(pool & (!empty | !has_full))

    # Count each value within its token. The NA flag keeps a real "NA"
    # string apart from a missing value.
    pair <- paste0(tk[cand], ":", is_na[cand], ":", key[cand])
    pid  <- match(pair, unique(pair))
    cnt  <- tabulate(pid)[pid]

    o <- order(tk[cand], -cnt, tt[cand], key[cand],
               method = "radix", na.last = TRUE)
    pick <- cand[o][!duplicated(tk[cand][o])]
    row  <- integer(ntok)
    row[tk[pick]] <- pick
    out[[col]] <- x[row[tk[first_row]]]
  }
  out
}
