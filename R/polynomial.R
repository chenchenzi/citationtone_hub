# =============================================================================
# Per-token Legendre polynomial fitting for f0 contours.
# =============================================================================

#' Fit Legendre polynomials to f0 contours, token by token
#'
#' For each token (a unique value of the `token` column), normalise the
#' time axis to `[-1, 1]`, construct a Legendre polynomial basis of the
#' requested degree, and fit ordinary least squares. Returns one row per
#' token with the fitted coefficients `c0`, `c1`, ..., `c{degree}`.
#'
#' Legendre polynomials are orthogonal on `[-1, 1]`, so the fitted
#' coefficients have a tidy shape interpretation:
#' * `c0` — token-level mean f0
#' * `c1` — linear slope across the contour
#' * `c2` — quadratic curvature (rise-fall vs fall-rise)
#' * `c3` — cubic shape (sigmoid vs reverse-sigmoid)
#'
#' Tokens with fewer than `degree + 1` valid samples receive `NA` for all
#' coefficients. Tokens with a single unique time point keep `c0` as the
#' mean f0 and set the higher-order coefficients to `NA`.
#'
#' @param data Long-format data frame with one row per f0 sample.
#' @param f0 Column name of f0 values. Default `"f0"`. Normalised f0
#'   (`"f0_st"`, `"f0_zscore"`) is recommended for cross-speaker comparison.
#' @param token Column name of token ID. Default `"token"`.
#' @param time Column name of time within each token. Default `"time"`.
#' @param speaker Column name of speaker ID, carried through to the
#'   output. Default `"speaker"`.
#' @param tone Column name of tone category, carried through to the
#'   output. Default `"tone"`.
#' @param degree Polynomial degree. One of `1`, `2`, or `3`. Default `2`.
#'
#' @return A token-level data frame with one row per token, containing:
#'   `token`, `speaker`, `tone`, and the coefficients `c0`, `c1`, ...,
#'   `c{degree}`.
#'
#' @examples
#' df <- data.frame(
#'   token   = rep(c("t1", "t2"), each = 5),
#'   time    = rep(seq(0, 1, length.out = 5), 2),
#'   f0      = c(150, 160, 170, 165, 155,
#'               140, 145, 150, 145, 140),
#'   speaker = rep("S01", 10),
#'   tone    = rep(c("T1", "T2"), each = 5)
#' )
#' fit_polynomial(df, degree = 2)
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
                           degree  = 2) {

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
  # the column name on every group.
  data <- data |>
    dplyr::mutate(.f0_fit = .data[[f0]])

  # Per-token metadata (first occurrence of speaker, tone).
  token_meta <- data |>
    dplyr::arrange(.data[[token]], .data[[time]]) |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(c(speaker, tone)), dplyr::first),
      .groups = "drop"
    )

  # Per-token OLS fit on the Legendre basis.
  coef_df <- data |>
    dplyr::arrange(.data[[token]], .data[[time]]) |>
    dplyr::group_by(.data[[token]]) |>
    dplyr::group_modify(
      ~ fit_one_token_polynomial(.x, degree = degree, time_col = time)
    ) |>
    dplyr::ungroup()

  # Attach metadata and fix column order.
  coef_names <- paste0("c", 0:degree)
  coef_df |>
    dplyr::left_join(token_meta, by = token) |>
    dplyr::select(dplyr::all_of(c(token, speaker, tone, coef_names)))
}


# -----------------------------------------------------------------------------
# Internal: Legendre polynomial basis on `[-1, 1]` up to a given degree.
# Used by fit_polynomial(); not exported.
# -----------------------------------------------------------------------------
legendre_basis <- function(t, degree) {
  n <- length(t)
  B <- matrix(NA_real_, nrow = n, ncol = degree + 1)
  B[, 1] <- 1                                       # P0
  if (degree >= 1) B[, 2] <- t                      # P1
  if (degree >= 2) B[, 3] <- (3 * t^2 - 1) / 2     # P2
  if (degree >= 3) B[, 4] <- (5 * t^3 - 3 * t) / 2 # P3
  colnames(B) <- paste0("c", 0:degree)
  B
}


# -----------------------------------------------------------------------------
# Internal: fit one token. Returns a 1-row data.frame of coefficients.
# Used by fit_polynomial() via group_modify(); not exported.
# -----------------------------------------------------------------------------
fit_one_token_polynomial <- function(token_data, degree, time_col) {
  t_raw <- token_data[[time_col]]
  t_min <- min(t_raw, na.rm = TRUE)
  t_max <- max(t_raw, na.rm = TRUE)
  coef_names <- paste0("c", 0:degree)

  # Degenerate: zero range (single unique time, all-NA, etc.)
  if (is.na(t_min) || is.na(t_max) || t_max == t_min) {
    result <- stats::setNames(rep(NA_real_, degree + 1), coef_names)
    result["c0"] <- mean(token_data$.f0_fit, na.rm = TRUE)
    return(as.data.frame(as.list(result)))
  }

  t_norm <- 2 * (t_raw - t_min) / (t_max - t_min) - 1
  y <- token_data$.f0_fit
  valid <- !is.na(t_norm) & !is.na(y)

  if (sum(valid) < degree + 1) {
    result <- stats::setNames(rep(NA_real_, degree + 1), coef_names)
    return(as.data.frame(as.list(result)))
  }

  B   <- legendre_basis(t_norm[valid], degree)
  fit <- stats::lm.fit(B, y[valid])
  as.data.frame(as.list(stats::setNames(fit$coefficients, coef_names)))
}
