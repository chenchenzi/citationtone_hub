# =============================================================================
# Generalised Additive Mixed Model (GAMM) fitting for f0 contours.
#
# Thin convenience wrapper around mgcv::bam() that prepares the data the way
# citation-tone GAMM analysis typically wants:
#   * time normalised to [0, 1] within each token
#   * tone-by-time smooths (separate or reference-plus-difference)
#   * optional speaker random smooths (Soskuthy 2021 strategies)
#   * optional AR1 autocorrelation correction
#
# For non-standard model structures, use mgcv::bam() / mgcv::gam() directly.
# =============================================================================

#' Fit a Generalised Additive Mixed Model (GAMM) to f0 contours
#'
#' Convenience wrapper around [mgcv::bam()] for the citation-tone GAMM use
#' case. Prepares the data, builds a smooth-based formula with optional
#' difference smooths and speaker random smooths, fits the model, and can
#' refit with an AR1 correlation if requested.
#'
#' For non-standard model structures, use [mgcv::bam()] / [mgcv::gam()]
#' directly.
#'
#' @param data Long-format data frame, one row per f0 sample.
#' @param f0,time,token,tone,speaker,item Column names.
#' @param duration Optional column name with a per-sample duration covariate.
#'   Use `NULL` to omit the duration smooth.
#' @param k Basis dimension for the time smooth. Default `10`.
#' @param bs Spline basis type passed to [mgcv::s()]. Default `"tp"`.
#' @param smooth_type Either `"separate"` (one smooth per tone via
#'   `s(time, by = tone)`) or `"difference"` (reference smooth +
#'   difference smooths via an ordered factor).
#' @param random_intercept_speaker,random_intercept_item Logical, include
#'   the corresponding `s(..., bs = "re")` random intercept.
#' @param random_smooth One of `"none"`, `"speaker"`, `"speaker_tone"`,
#'   `"speaker_by_tone"`, or `"ref_diff"`. See *Random smooth strategies*
#'   in details.
#' @param use_ar1 Logical. If `TRUE`, fit once, estimate `rho` from the
#'   residuals' lag-1 autocorrelation, then refit with that `rho`.
#'
#' @details
#' Random smooth strategies (after Sóskuthy 2021):
#' * `"speaker"` — single by-speaker factor smooth
#' * `"speaker_tone"` — interaction factor smooth over speaker × tone
#' * `"speaker_by_tone"` — by-speaker factor smooth, separately per tone
#' * `"ref_diff"` — reference by-speaker smooth plus tone-difference
#'   smooths (matches `smooth_type = "difference"`)
#'
#' @return An S3 object of class `"shinytone_gamm"`, a list with:
#' * `model` — the fitted [mgcv::bam()] object
#' * `formula_str` — user-readable formula string
#' * `rho` — `NULL` or the AR1 rho value
#' * `convergence_warning` — `NULL` or the captured warning message
#' * `smooth_type`, `random_smooth` — passed through (needed by
#'   [predict_gamm()])
#' * `col_names` — original column names, for back-mapping
#'
#' @seealso [predict_gamm()] for per-tone prediction on a time grid.
#' @export
#' @importFrom dplyr group_by mutate ungroup arrange n
#' @importFrom rlang .data
#' @importFrom stats as.formula resid acf contrasts<- predict
fit_gamm <- function(data,
                     f0       = "f0",
                     time     = "time",
                     token    = "token",
                     tone     = "tone",
                     speaker  = "speaker",
                     item     = "item",
                     duration = NULL,
                     k                        = 10,
                     bs                       = "tp",
                     smooth_type              = c("separate", "difference"),
                     random_intercept_speaker = TRUE,
                     random_intercept_item    = TRUE,
                     random_smooth            = c("none", "speaker",
                                                  "speaker_tone",
                                                  "speaker_by_tone",
                                                  "ref_diff"),
                     use_ar1                  = FALSE) {

  smooth_type   <- match.arg(smooth_type)
  random_smooth <- match.arg(random_smooth)

  required <- c(f0, time, token, tone, speaker, item)
  if (!is.null(duration)) required <- c(required, duration)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv is required to fit GAMM models. Install with install.packages('mgcv').",
         call. = FALSE)
  }

  # --- Data prep -----------------------------------------------------------
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

  dat$.f0      <- dat[[f0]]
  dat$.tone    <- as.factor(dat[[tone]])
  dat$.speaker <- as.factor(dat[[speaker]])
  dat$.item    <- as.factor(dat[[item]])
  dat$.token   <- as.factor(dat[[token]])
  if (!is.null(duration)) dat$.duration <- as.numeric(dat[[duration]])

  dat <- as.data.frame(dat[order(dat$.token, dat$.time_norm), ])

  # Start-event marker for AR1.
  dat$.start_event <- !duplicated(dat$.token)

  if (smooth_type == "difference") {
    dat$.tone_ord <- ordered(dat$.tone, levels = levels(dat$.tone))
    stats::contrasts(dat$.tone_ord) <- "contr.treatment"
  }
  if (random_smooth == "speaker_tone") {
    dat$.speaker_tone <- interaction(dat$.speaker, dat$.tone, drop = TRUE)
  }

  # --- Formula construction ------------------------------------------------
  if (smooth_type == "separate") {
    smooth_term <- sprintf('s(.time_norm, by = .tone, k = %d, bs = "%s")', k, bs)
    fixed_part  <- paste0(".f0 ~ .tone + ", smooth_term)
  } else {
    ref_smooth  <- sprintf('s(.time_norm, k = %d, bs = "%s")', k, bs)
    diff_smooth <- sprintf('s(.time_norm, by = .tone_ord, k = %d, bs = "%s")', k, bs)
    fixed_part  <- paste0(".f0 ~ .tone_ord + ", ref_smooth, " + ", diff_smooth)
  }
  dur_term <- if (!is.null(duration)) " + s(.duration)" else ""

  random_parts <- character(0)
  if (random_smooth == "speaker") {
    random_parts <- sprintf('s(.time_norm, .speaker, bs = "fs", m = 1, k = %d)', k)
  } else if (random_smooth == "speaker_tone") {
    random_parts <- sprintf('s(.time_norm, .speaker_tone, bs = "fs", m = 1, k = %d)', k)
  } else if (random_smooth == "speaker_by_tone") {
    random_parts <- sprintf('s(.time_norm, .speaker, by = .tone, bs = "fs", m = 1, k = %d)', k)
  } else if (random_smooth == "ref_diff") {
    random_parts <- c(
      sprintf('s(.time_norm, .speaker, bs = "fs", m = 1, k = %d)', k),
      if (smooth_type == "difference")
        sprintf('s(.time_norm, .speaker, by = .tone_ord, bs = "fs", m = 1, k = %d)', k)
      else
        sprintf('s(.time_norm, .speaker, by = .tone, bs = "fs", m = 1, k = %d)', k)
    )
  } else if (random_intercept_speaker) {
    random_parts <- 's(.speaker, bs = "re")'
  }
  if (random_intercept_item) {
    random_parts <- c(random_parts, 's(.item, bs = "re")')
  }

  formula_str <- paste0(fixed_part, dur_term)
  if (length(random_parts) > 0) {
    formula_str <- paste0(formula_str, " + ", paste(random_parts, collapse = " + "))
  }

  display_formula <- build_gamm_display_formula(
    smooth_type, random_smooth,
    f0, time, tone, speaker, item, duration,
    k, bs, random_intercept_speaker, random_intercept_item
  )

  # --- Fit -----------------------------------------------------------------
  warn_msg <- NULL
  model <- withCallingHandlers(
    mgcv::bam(stats::as.formula(formula_str), data = dat, discrete = TRUE),
    warning = function(w) {
      warn_msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  rho_val <- NULL
  if (use_ar1) {
    rho_val <- stats::acf(stats::resid(model), plot = FALSE)$acf[2]
    model <- withCallingHandlers(
      mgcv::bam(stats::as.formula(formula_str), data = dat,
                rho = rho_val, AR.start = dat$.start_event, discrete = TRUE),
      warning = function(w) {
        warn_msg <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  }

  structure(
    list(
      model               = model,
      formula_str         = display_formula,
      rho                 = rho_val,
      convergence_warning = warn_msg,
      smooth_type         = smooth_type,
      random_smooth       = random_smooth,
      col_names           = list(f0 = f0, time = time, token = token,
                                 tone = tone, speaker = speaker, item = item,
                                 duration = duration),
      data_for_predict    = dat
    ),
    class = "shinytone_gamm"
  )
}


# Build the user-readable display formula. Internal helper used by fit_gamm().
build_gamm_display_formula <- function(smooth_type, random_smooth,
                                       f0, time, tone, speaker, item, duration,
                                       k, bs,
                                       random_intercept_speaker,
                                       random_intercept_item) {
  tone_display <- if (smooth_type == "difference") paste0(tone, ".ord") else tone

  if (smooth_type == "separate") {
    fixed <- sprintf('%s ~ %s + s(%s, by = %s, k = %d, bs = "%s")',
                     f0, tone, time, tone, k, bs)
  } else {
    fixed <- sprintf('%s ~ %s.ord + s(%s, k = %d, bs = "%s") + s(%s, by = %s.ord, k = %d, bs = "%s")',
                     f0, tone, time, k, bs, time, tone, k, bs)
  }
  if (!is.null(duration)) fixed <- paste0(fixed, sprintf(" + s(%s)", duration))

  randoms <- character(0)
  if (random_smooth == "speaker") {
    randoms <- sprintf('s(%s, %s, bs = "fs", m = 1, k = %d)', time, speaker, k)
  } else if (random_smooth == "speaker_tone") {
    randoms <- sprintf('s(%s, %s.%s, bs = "fs", m = 1, k = %d)', time, speaker, tone, k)
  } else if (random_smooth == "speaker_by_tone") {
    randoms <- sprintf('s(%s, %s, by = %s, bs = "fs", m = 1, k = %d)',
                       time, speaker, tone_display, k)
  } else if (random_smooth == "ref_diff") {
    randoms <- c(
      sprintf('s(%s, %s, bs = "fs", m = 1, k = %d)', time, speaker, k),
      sprintf('s(%s, %s, by = %s, bs = "fs", m = 1, k = %d)',
              time, speaker, tone_display, k)
    )
  } else if (random_intercept_speaker) {
    randoms <- sprintf('s(%s, bs = "re")', speaker)
  }
  if (random_intercept_item) {
    randoms <- c(randoms, sprintf('s(%s, bs = "re")', item))
  }
  if (length(randoms) > 0) {
    fixed <- paste0(fixed, " + ", paste(randoms, collapse = " + "))
  }
  fixed
}


#' Predict population-level f0 smooth curves from a GAMM fit
#'
#' Builds a per-tone time grid, sets random-effect columns to reference
#' levels, and predicts with the random-effect smooths excluded so that
#' the result is the population-average curve per tone.
#'
#' @param gamm_obj An object of class `"shinytone_gamm"` from [fit_gamm()].
#' @param n Number of time points across `[0, 1]`. Default `200`.
#'
#' @return A data frame with columns `time`, `f0_predicted`, `se`, `tone`.
#'
#' @export
predict_gamm <- function(gamm_obj, n = 200) {
  if (!inherits(gamm_obj, "shinytone_gamm")) {
    stop("`gamm_obj` must be a shinytone_gamm object from fit_gamm().",
         call. = FALSE)
  }
  model         <- gamm_obj$model
  smooth_type   <- gamm_obj$smooth_type
  random_smooth <- gamm_obj$random_smooth
  duration      <- gamm_obj$col_names$duration
  dat           <- gamm_obj$data_for_predict

  tone_levels <- levels(dat$.tone)
  time_seq    <- seq(0, 1, length.out = n)
  ref_speaker <- levels(dat$.speaker)[1]
  ref_item    <- levels(dat$.item)[1]

  # Random-effect terms to exclude from prediction (population-average).
  exclude_terms <- character(0)
  if (random_smooth == "speaker")          exclude_terms <- "s(.time_norm,.speaker)"
  else if (random_smooth == "speaker_tone") exclude_terms <- "s(.time_norm,.speaker_tone)"
  else if (random_smooth == "speaker_by_tone") exclude_terms <- "s(.time_norm,.speaker)"
  else if (random_smooth == "ref_diff")    exclude_terms <- "s(.time_norm,.speaker)"
  else if ("s(.speaker)" %in% sapply(model$smooth, function(s) s$label))
    exclude_terms <- "s(.speaker)"
  if ("s(.item)" %in% sapply(model$smooth, function(s) s$label))
    exclude_terms <- c(exclude_terms, "s(.item)")
  if (!is.null(duration))
    exclude_terms <- c(exclude_terms, "s(.duration)")

  plot_rows <- vector("list", length(tone_levels))
  for (i in seq_along(tone_levels)) {
    tone_i <- tone_levels[i]
    nd <- data.frame(
      .time_norm = time_seq,
      .speaker   = factor(rep(ref_speaker, n), levels = levels(dat$.speaker)),
      .item      = factor(rep(ref_item,    n), levels = levels(dat$.item)),
      stringsAsFactors = FALSE
    )
    if (smooth_type == "separate") {
      nd$.tone <- factor(rep(tone_i, n), levels = tone_levels)
    } else {
      nd$.tone_ord <- ordered(rep(tone_i, n), levels = tone_levels)
      stats::contrasts(nd$.tone_ord) <- "contr.treatment"
      if (random_smooth == "speaker_by_tone") {
        nd$.tone <- factor(rep(tone_i, n), levels = tone_levels)
      }
    }
    if (random_smooth == "speaker_tone") {
      nd$.speaker_tone <- factor(
        as.character(interaction(nd$.speaker,
                                 factor(rep(tone_i, n), levels = tone_levels),
                                 drop = FALSE)),
        levels = levels(dat$.speaker_tone))
    }
    if (!is.null(duration)) {
      nd$.duration <- rep(mean(dat$.duration, na.rm = TRUE), n)
    }
    preds <- stats::predict(model, newdata = nd, type = "response",
                            exclude = exclude_terms, se.fit = TRUE)
    plot_rows[[i]] <- data.frame(
      time         = time_seq,
      f0_predicted = preds$fit,
      se           = preds$se.fit,
      tone         = tone_i,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, plot_rows)
}
