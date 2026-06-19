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
#' 1. Compute the per-token range of `time` and rescale it linearly to
#'    the interval `[-1, 1]`, which is where Legendre polynomials are
#'    orthogonal.
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
#' Tokens with fewer than `degree + 1` valid samples receive `NA` for
#' all coefficients. Tokens with a single unique time point keep `c0`
#' as the mean f0 and set the higher-order coefficients to `NA`.
#'
#' @param data A long-format data frame with one row per f0 sample.
#' @param f0 Column name of f0 values. Default `"f0"`. Using normalised
#'   f0 (e.g. `"f0_st"` from [normalise_f0()]) is recommended for
#'   cross-speaker comparison.
#' @param token Column name of token ID. Default `"token"`.
#' @param time Column name of time within each token. Default `"time"`.
#' @param speaker Column name of speaker ID, carried through to the
#'   output. Default `"speaker"`.
#' @param tone Column name of tone category, carried through to the
#'   output. Default `"tone"`.
#' @param degree Polynomial degree. One of `1`, `2`, or `3`. Default `2`.
#'
#' @return A token-level data frame with one row per token, containing
#'   the `token`, `speaker`, `tone`, and coefficient columns `c0`,
#'   `c1`, ..., `c{degree}`.
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
