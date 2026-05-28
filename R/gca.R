# =============================================================================
# Growth Curve Analysis (GCA) fitting.
#
# Thin convenience wrapper around lme4::lmer() that prepares the data the way
# citation-tone GCA typically wants:
#   * time normalised to [0, 1] within each token
#   * orthogonal polynomials of the requested degree on normalised time
#   * fixed effects: (ot1 + ... + otK) * tone
#   * conventional random effects on speaker and item
#
# For non-standard model structures, use lme4::lmer() directly.
# =============================================================================

#' Fit a Growth Curve Analysis (GCA) model to f0 contours
#'
#' @description
#' Fits a Growth Curve Analysis model to f0 contours: a linear mixed-effects
#' regression of f0 on orthogonal polynomial terms of time, interacted with
#' tone, with conventional by-speaker and by-item random effects. GCA is a
#' standard analysis framework for time-varying dynamic speech data
#' (Mirman 2014; Xu & Zhang 2024) and is the recommended approach when
#' you want statistical inference about per-tone shape parameters such as
#' intercept, slope, and curvature.
#'
#' This is a convenience wrapper around [lme4::lmer()] that prepares the
#' data the way the citation-tone use case typically wants. For
#' non-standard model structures (custom contrasts, additional fixed
#' effects, alternative random-effects structures), call [lme4::lmer()]
#' directly.
#'
#' @details
#' ## What the function does internally
#'
#' 1. Within each token, normalise the time axis to `[0, 1]`.
#' 2. Build orthogonal polynomial terms `ot1`, `ot2`, ..., `otK` on the
#'    normalised time using [stats::poly()], caching the basis
#'    coefficients so that [predict_gca()] can reuse them.
#' 3. Construct an `lmer` formula of the form
#'    `f0 ~ (ot1 + ... + otK) * tone + (random effects)` using the
#'    options below to decide which random effects to include.
#' 4. Fit the model with REML, capturing any convergence warning so the
#'    caller can decide whether to act on it.
#' 5. Return everything bundled as an S3 object of class
#'    `"shinytone_gca"`, so downstream helpers ([predict_gca()],
#'    `summary()`, etc.) have what they need.
#'
#' ## Choosing the polynomial degree
#'
#' * `degree = 1`: linear (intercept + slope). Sufficient for purely
#'   rising or falling contours.
#' * `degree = 2` (default): linear + quadratic. Captures
#'   rising/falling/dipping/peaking shapes. The usual choice.
#' * `degree = 3`: adds a cubic term for sigmoid or S-shaped contours.
#'   Useful for languages with more elaborate contour shapes.
#'
#' Higher degrees increase model complexity and the risk of
#' over-fitting; they also slow the fit and can degrade convergence.
#'
#' ## Choosing the random-effects structure
#'
#' The defaults follow conventional GCA practice for citation-tone work:
#' by-speaker random intercepts and slopes on the polynomial terms,
#' plus a by-item random intercept. Add by-item slopes if items vary
#' systematically in shape, or drop random slopes when convergence is
#' difficult.
#'
#' @param data A long-format data frame with one row per f0 sample.
#' @param f0,time,token,tone,speaker,item Column names for f0, time
#'   within token, token ID, tone category, speaker ID, and item or
#'   word.
#' @param degree Polynomial degree, one of `1`, `2`, or `3`. Default
#'   `2`.
#' @param random_intercept_speaker,random_intercept_item Logical;
#'   include a by-speaker or by-item random intercept. Default `TRUE`.
#' @param random_slope_speaker,random_slope_item Logical; include
#'   by-speaker or by-item random slopes on the orthogonal-polynomial
#'   terms (`ot1` ... `otK`). Default `TRUE` for speaker, `FALSE` for
#'   item.
#'
#' @return An S3 object of class `"shinytone_gca"`, a list with:
#' * `model`: the fitted [lme4::lmerMod-class] object.
#' * `formula_str`: user-readable formula string (with original column
#'   names).
#' * `poly_coefs`: coefs from [stats::poly()] needed by [predict_gca()].
#' * `degree`: polynomial degree.
#' * `col_names`: original column names (for back-mapping in summary
#'   tables).
#' * `convergence_warning`: `NULL` or the captured warning message.
#'
#' @seealso
#' * [predict_gca()] for prediction on a time grid.
#' * [fit_polynomial()] for a per-token (non-mixed-effects) alternative.
#' * [fit_gamm()] for the GAMM-based alternative when smooth (non-polynomial)
#'   tone shapes are preferred.
#'
#' @references
#' Mirman, D. (2014). \emph{Growth Curve Analysis and Visualization Using
#' R}. Chapman and Hall/CRC.
#'
#' Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting
#' linear mixed-effects models using lme4. \emph{Journal of Statistical
#' Software}, 67(1), 1–48. \doi{10.18637/jss.v067.i01}
#'
#' Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation
#' tone production studies: Methodology and recommendations.
#' \emph{The Journal of the Acoustical Society of America}, 156(4),
#' 2538–2565. \doi{10.1121/10.0032356}
#'
#' @examples
#' \dontrun{
#' data(sample_f0)
#' normed <- normalise_f0(sample_f0,
#'                        f0      = "f0_Hz",
#'                        speaker = "speaker",
#'                        tone    = "tone")
#' gca <- fit_gca(normed,
#'                f0      = "f0_st",
#'                time    = "time",
#'                token   = "token",
#'                tone    = "tone",
#'                speaker = "speaker",
#'                item    = "char",
#'                degree  = 2,
#'                random_slope_speaker = FALSE,
#'                random_slope_item    = FALSE)
#' predict_gca(gca, n = 100)
#' }
#' @export
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom rlang .data
#' @importFrom stats poly as.formula
fit_gca <- function(data,
                    f0      = "f0",
                    time    = "time",
                    token   = "token",
                    tone    = "tone",
                    speaker = "speaker",
                    item    = "item",
                    degree                    = 2,
                    random_intercept_speaker  = TRUE,
                    random_intercept_item     = TRUE,
                    random_slope_speaker      = TRUE,
                    random_slope_item         = FALSE) {

  if (!degree %in% 1:3) {
    stop("`degree` must be 1, 2, or 3.", call. = FALSE)
  }
  required <- c(f0, time, token, tone, speaker, item)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("lme4 is required to fit GCA models. Install with install.packages('lme4').",
         call. = FALSE)
  }

  # --- Data prep: per-token time normalisation to [0, 1] ---------------------
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

  # Orthogonal polynomials; cache coefs so predict_gca() can reuse the basis.
  poly_matrix <- stats::poly(dat$.time_norm, degree)
  poly_coefs  <- attr(poly_matrix, "coefs")
  ot_names    <- paste0("ot", seq_len(degree))
  for (i in seq_len(degree)) dat[[ot_names[i]]] <- poly_matrix[, i]

  # Internal short names for formula building. Original columns are retained
  # so users can still locate them after the fit.
  dat$.f0      <- dat[[f0]]
  dat$.tone    <- as.factor(dat[[tone]])
  dat$.speaker <- as.factor(dat[[speaker]])
  dat$.item    <- as.factor(dat[[item]])

  # --- Formula construction --------------------------------------------------
  ot_terms   <- paste(ot_names, collapse = " + ")
  fixed_part <- paste0(".f0 ~ (", ot_terms, ") * .tone")

  random_parts <- character(0)
  if (random_slope_speaker) {
    random_parts <- c(random_parts,
                      paste0("(1 + ", ot_terms, " | .speaker)"))
  } else if (random_intercept_speaker) {
    random_parts <- c(random_parts, "(1 | .speaker)")
  }
  if (random_slope_item) {
    random_parts <- c(random_parts,
                      paste0("(1 + ", ot_terms, " | .item)"))
  } else if (random_intercept_item) {
    random_parts <- c(random_parts, "(1 | .item)")
  }
  if (length(random_parts) == 0) random_parts <- "(1 | .item)"

  formula_str <- paste0(fixed_part, " + ", paste(random_parts, collapse = " + "))

  # Display formula with original column names (for the Shiny summary box,
  # for citation("shinytone"), or for reporting in papers).
  display_random <- character(0)
  if (random_slope_speaker) {
    display_random <- c(display_random,
                        paste0("(1 + ", ot_terms, " | ", speaker, ")"))
  } else if (random_intercept_speaker) {
    display_random <- c(display_random, paste0("(1 | ", speaker, ")"))
  }
  if (random_slope_item) {
    display_random <- c(display_random,
                        paste0("(1 + ", ot_terms, " | ", item, ")"))
  } else if (random_intercept_item) {
    display_random <- c(display_random, paste0("(1 | ", item, ")"))
  }
  if (length(display_random) == 0) display_random <- paste0("(1 | ", item, ")")
  display_formula <- paste0(f0, " ~ (", ot_terms, ") * ", tone,
                            " + ", paste(display_random, collapse = " + "))

  # --- Fit with warning capture ---------------------------------------------
  warn_msg <- NULL
  model <- withCallingHandlers(
    lme4::lmer(stats::as.formula(formula_str), data = dat, REML = TRUE),
    warning = function(w) {
      warn_msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  structure(
    list(
      model               = model,
      formula_str         = display_formula,
      poly_coefs          = poly_coefs,
      degree              = degree,
      col_names           = list(f0 = f0, time = time, token = token,
                                 tone = tone, speaker = speaker, item = item),
      convergence_warning = warn_msg
    ),
    class = "shinytone_gca"
  )
}


#' Predict population-level f0 contours from a GCA fit
#'
#' @description
#' Generates population-average per-tone f0 curves from a fitted
#' [fit_gca()] model. Useful for plotting predicted contours, comparing
#' tones at a glance, or feeding into downstream Chao numeral
#' summarisation via [contour_to_chao()].
#'
#' @details
#' Internally:
#'
#' 1. Build a `(time, tone)` grid with `n` evenly-spaced points across
#'    `[0, 1]` for every tone level the model knows about.
#' 2. Re-compute the orthogonal polynomial basis on that grid using the
#'    cached coefficients from [fit_gca()] (this ensures the basis
#'    matches what the model was fit with, not a fresh one).
#' 3. Call [stats::predict()] on the lme4 model with `re.form = NA`, so
#'    only fixed effects contribute (random effects are integrated out
#'    to the population mean).
#'
#' Note that the returned predictions are on the scale of the f0 column
#' used to fit the model (typically semitones, if you passed
#' `f0 = "f0_st"` from [normalise_f0()]).
#'
#' @param gca_obj An object of class `"shinytone_gca"` returned by
#'   [fit_gca()].
#' @param n Number of time points across `[0, 1]`. Default `100`.
#'
#' @return A data frame with columns `time`, `f0_predicted`, `tone`.
#'
#' @seealso [fit_gca()] for the model fit. [contour_to_chao()] for
#'   converting the predicted contours to Chao numerals.
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
#' @export
predict_gca <- function(gca_obj, n = 100) {
  if (!inherits(gca_obj, "shinytone_gca")) {
    stop("`gca_obj` must be a shinytone_gca object from fit_gca().",
         call. = FALSE)
  }
  model      <- gca_obj$model
  degree     <- gca_obj$degree
  poly_coefs <- gca_obj$poly_coefs

  dat <- model@frame
  tone_levels <- levels(dat$.tone)

  time_seq <- seq(0, 1, length.out = n)
  newdata  <- expand.grid(.time_norm = time_seq, .tone = tone_levels,
                          stringsAsFactors = FALSE)
  newdata$.tone <- factor(newdata$.tone, levels = tone_levels)

  poly_pred <- stats::poly(newdata$.time_norm, degree, coefs = poly_coefs)
  for (k in seq_len(degree)) {
    newdata[[paste0("ot", k)]] <- poly_pred[, k]
  }

  # lme4 needs the grouping factors present even with re.form = NA;
  # the values are ignored once REs are dropped.
  newdata$.speaker <- dat$.speaker[1]
  newdata$.item    <- dat$.item[1]

  newdata$f0_predicted <- stats::predict(model, newdata = newdata,
                                          re.form = NA,
                                          allow.new.levels = TRUE)

  data.frame(
    time         = newdata$.time_norm,
    f0_predicted = newdata$f0_predicted,
    tone         = as.character(newdata$.tone),
    stringsAsFactors = FALSE
  )
}
