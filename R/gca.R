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
#' Convenience wrapper around [lme4::lmer()] for the citation-tone GCA use
#' case. Prepares the data (per-token time normalisation, orthogonal
#' polynomial basis of the requested degree), builds a conventional
#' formula `(ot1 + ... + otK) * tone + (random effects)`, and fits the
#' model with warning capture.
#'
#' For non-standard model structures (custom contrasts, additional fixed
#' effects, alternative RE structures), use [lme4::lmer()] directly.
#'
#' @param data Long-format data frame, one row per f0 sample.
#' @param f0,time,token,tone,speaker,item Column names for f0, time within
#'   token, token ID, tone category, speaker ID, and item / word.
#' @param degree Polynomial degree (1, 2, or 3). Default `2`.
#' @param random_intercept_speaker,random_intercept_item Logical, include
#'   a by-speaker / by-item random intercept. Default `TRUE`.
#' @param random_slope_speaker,random_slope_item Logical, include by-speaker
#'   / by-item random slopes on the orthogonal-polynomial terms (`ot1` ..
#'   `otK`). Default `TRUE` for speaker, `FALSE` for item.
#'
#' @return An S3 object of class `"shinytone_gca"`, which is a list with:
#' * `model` — the fitted [lme4::lmerMod-class] object
#' * `formula_str` — user-readable formula string (with original column names)
#' * `poly_coefs` — coefs from [stats::poly()] needed by [predict_gca()]
#' * `degree` — polynomial degree
#' * `col_names` — original column names (for back-mapping in summary tables)
#' * `convergence_warning` — `NULL` or the captured warning message
#'
#' @seealso [predict_gca()] for prediction on a time grid.
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
#' Builds a (time, tone) prediction grid using the polynomial basis cached
#' inside the [fit_gca()] result, then predicts via lme4 with `re.form = NA`
#' so the curves reflect the population-average fixed effects.
#'
#' @param gca_obj An object of class `"shinytone_gca"` returned by
#'   [fit_gca()].
#' @param n Number of time points across `[0, 1]`. Default `100`.
#'
#' @return A data frame with columns `time`, `f0_predicted`, `tone`.
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
