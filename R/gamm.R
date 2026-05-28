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
#' @description
#' Fits a Generalised Additive Mixed Model with smooth (non-polynomial)
#' tone-by-time interactions. GAMMs are an increasingly common
#' alternative to GCA for dynamic speech analysis because they capture
#' arbitrary smooth contour shapes without requiring the analyst to
#' choose a polynomial degree (Wood 2017; Sóskuthy 2021;
#' Xu & Zhang 2024).
#'
#' This is a convenience wrapper around [mgcv::bam()] that prepares the
#' data, builds a smooth-based formula with optional difference smooths
#' and speaker random smooths, fits the model, and optionally refits
#' with an AR1 correlation. For non-standard model structures, call
#' [mgcv::bam()] or [mgcv::gam()] directly.
#'
#' @details
#' ## What the function does internally
#'
#' 1. Normalise time to `[0, 1]` per token.
#' 2. Coerce `tone`, `speaker`, `item`, and (for difference smooths) an
#'    ordered `tone_ord` to factors with treatment contrasts.
#' 3. Build the formula based on `smooth_type` and `random_smooth`.
#' 4. Fit with [mgcv::bam()] using `discrete = TRUE` for speed on large
#'    datasets, capturing warnings.
#' 5. If `use_ar1 = TRUE`, estimate `rho` from the lag-1 autocorrelation
#'    of the residuals and refit with that `rho` plus per-token
#'    `AR.start` to correct for within-token correlation.
#'
#' ## Choosing `smooth_type`
#'
#' * `"separate"` (default) fits one smooth per tone with `s(time,
#'   by = tone)`. Each tone curve is estimated independently.
#' * `"difference"` fits a reference smooth plus difference smooths via
#'   an ordered factor. Lets you test whether each non-reference tone
#'   significantly differs from the reference at any point along the
#'   time axis.
#'
#' ## Random smooth strategies
#'
#' Following Sóskuthy (2021), several patterns are supported via
#' `random_smooth`:
#'
#' * `"speaker"`: single by-speaker factor smooth `s(time, speaker,
#'   bs = "fs", m = 1)`. Fastest, basic random-curve modelling.
#' * `"speaker_tone"`: interaction factor smooth `s(time, speaker.tone,
#'   bs = "fs", m = 1)` letting each speaker-tone combination have its
#'   own random curve.
#' * `"speaker_by_tone"`: by-speaker factor smooths separately per tone.
#' * `"ref_diff"`: reference by-speaker smooth plus tone-difference
#'   random smooths, matched to `smooth_type = "difference"`.
#' * `"none"`: only the random-effect intercepts (if any) are included.
#'
#' Refer to Sóskuthy (2021) for guidance on choosing between these.
#'
#' @param data A long-format data frame with one row per f0 sample.
#' @param f0,time,token,tone,speaker,item Column names.
#' @param duration Optional column name with a per-sample duration
#'   covariate. Use `NULL` to omit the duration smooth.
#' @param k Basis dimension for the time smooth. Default `10`.
#' @param bs Spline basis type passed to [mgcv::s()]. Default `"tp"`
#'   (thin-plate regression spline).
#' @param smooth_type Either `"separate"` (one smooth per tone via
#'   `s(time, by = tone)`) or `"difference"` (reference smooth plus
#'   difference smooths via an ordered factor).
#' @param random_intercept_speaker,random_intercept_item Logical;
#'   include the corresponding `s(..., bs = "re")` random intercept.
#' @param random_smooth One of `"none"`, `"speaker"`, `"speaker_tone"`,
#'   `"speaker_by_tone"`, or `"ref_diff"`. See Details.
#' @param use_ar1 Logical. If `TRUE`, fit once, estimate `rho` from the
#'   residuals' lag-1 autocorrelation, then refit with that `rho`.
#'
#' @return An S3 object of class `"shinytone_gamm"`, a list with:
#' * `model`: the fitted [mgcv::bam()] object.
#' * `formula_str`: user-readable formula string.
#' * `rho`: `NULL` or the AR1 rho value.
#' * `convergence_warning`: `NULL` or the captured warning message.
#' * `smooth_type`, `random_smooth`: passed through (needed by
#'   [predict_gamm()]).
#' * `col_names`: original column names, for back-mapping.
#'
#' @seealso
#' * [predict_gamm()] for per-tone prediction on a time grid.
#' * [fit_gca()] for the polynomial-based mixed-effects alternative.
#'
#' @references
#' Sóskuthy, M. (2021). Evaluating generalised additive mixed modelling
#' strategies for dynamic speech analysis. \emph{Journal of Phonetics},
#' 84, 101017. \doi{10.1016/j.wocn.2020.101017}
#'
#' Wood, S. N. (2017). \emph{Generalized Additive Models: An Introduction
#' with R} (2nd ed.). Chapman and Hall/CRC.
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
#' gamm <- fit_gamm(normed,
#'                  f0          = "f0_st",
#'                  time        = "time",
#'                  token       = "token",
#'                  tone        = "tone",
#'                  speaker     = "speaker",
#'                  item        = "char",
#'                  k           = 10,
#'                  smooth_type = "separate",
#'                  random_smooth = "speaker",
#'                  use_ar1     = TRUE)
#' predict_gamm(gamm, n = 200)
#' }
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
#' @description
#' Generates per-tone population-average smooth curves from a fitted
#' [fit_gamm()] model. Useful for plotting predicted contours with
#' confidence bands, comparing tones at a glance, or feeding into
#' downstream Chao numeral summarisation via [contour_to_chao()].
#'
#' @details
#' Internally:
#'
#' 1. Build a per-tone time grid with `n` evenly-spaced points across
#'    `[0, 1]`.
#' 2. Set random-effect columns (speaker, item, and any random-smooth
#'    grouping factors) to the first level of their respective factors;
#'    these reference values are placeholders that don't affect the
#'    prediction once the corresponding terms are excluded.
#' 3. Identify the random-effect terms that need to be excluded so the
#'    prediction reflects only the population-average fixed smooths.
#' 4. Call [stats::predict()] on the mgcv model with `exclude =
#'    <random terms>` and `se.fit = TRUE` to also return standard
#'    errors.
#'
#' Predictions are on the scale of the f0 column used to fit the model
#' (typically semitones if you passed `f0 = "f0_st"` from
#' [normalise_f0()]).
#'
#' @param gamm_obj An object of class `"shinytone_gamm"` from
#'   [fit_gamm()].
#' @param n Number of time points across `[0, 1]`. Default `200`.
#'
#' @return A data frame with columns `time`, `f0_predicted`, `se`,
#'   `tone`.
#'
#' @seealso [fit_gamm()] for the model fit. [contour_to_chao()] for
#'   converting the predicted contours to Chao numerals.
#'
#' @references
#' Sóskuthy, M. (2021). Evaluating generalised additive mixed modelling
#' strategies for dynamic speech analysis. \emph{Journal of Phonetics},
#' 84, 101017. \doi{10.1016/j.wocn.2020.101017}
#'
#' Xu, C., & Zhang, C. (2024). A cross-linguistic review of citation
#' tone production studies: Methodology and recommendations.
#' \emph{The Journal of the Acoustical Society of America}, 156(4),
#' 2538–2565. \doi{10.1121/10.0032356}
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
