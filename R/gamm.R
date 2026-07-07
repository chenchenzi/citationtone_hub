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
#'    of the residuals computed *within* tokens (pooling only genuine
#'    within-token neighbours, not across token boundaries), then refit
#'    with that `rho` plus per-token `AR.start` to correct for
#'    within-token correlation.
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
    # Estimate rho from the lag-1 autocorrelation *within* tokens, not across
    # the flat concatenation: a plain acf() would count every token boundary as
    # an adjacent pair and bias rho. grouped_acf() pools only genuine
    # within-token neighbours (the same series bam's AR.start whitens).
    init_res <- stats::resid(model)
    init_tok <- as.character(dat$.token)
    if (!is.null(model$na.action)) {
      init_tok <- init_tok[-as.integer(model$na.action)]
    }
    rho_val <- if (length(init_tok) == length(init_res) && !anyNA(init_res)) {
      grouped_acf(init_res, init_tok, max_lag = 1)$acf[2]
    } else {
      stats::acf(init_res, plot = FALSE, na.action = stats::na.pass)$acf[2]
    }
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


# Back-map the internal dotted term names (.time_norm, .tone_ord, .speaker, ...)
# used inside the fitted model onto the user's original column names, so
# diagnostic tables read in the same vocabulary as the rest of the app. Order
# matters: the compound tokens (.tone_ord, .speaker_tone) must be substituted
# before their prefixes (.tone, .speaker). Internal helper for diagnose_gamm().
# Per-token residual autocorrelation, the way dynamic-speech GAMM workflows
# want it (cf. itsadug::acf_resid). Plain stats::acf() on the fitted residuals
# would treat the whole dataset as one series and let every token boundary
# contaminate the low lags — badly so for short tokens. Instead we split the
# residuals by token and pool the within-token lag products, so only genuinely
# adjacent frames contribute. When the model was fit with an AR1 correction we
# first whiten each token series (e_i - rho * e_{i-1}, reset at each token
# start, exactly as bam's AR.start does), so the plot shows whether the AR1
# term actually removed the autocorrelation. Internal helper for
# diagnose_gamm().
grouped_acf <- function(res, tok, rho = NULL, max_lag = 40) {
  groups <- split(res, tok)
  if (!is.null(rho) && is.finite(rho)) {
    groups <- lapply(groups, function(e) {
      if (length(e) < 2) e else c(e[1], e[-1] - rho * e[-length(e)])
    })
  }
  res2 <- unlist(groups, use.names = FALSE)
  gm   <- mean(res2)
  den  <- sum((res2 - gm)^2)
  lens <- vapply(groups, length, integer(1))
  max_lag <- max(1L, min(as.integer(max_lag), max(lens) - 1L))

  acf_vals <- numeric(max_lag + 1L)
  acf_vals[1] <- 1  # lag 0
  for (L in seq_len(max_lag)) {
    num <- 0
    for (e in groups) {
      n <- length(e)
      if (n > L) num <- num + sum((e[seq_len(n - L)] - gm) * (e[(1 + L):n] - gm))
    }
    acf_vals[L + 1L] <- if (den > 0) num / den else NA_real_
  }
  data.frame(lag = 0:max_lag, acf = acf_vals)
}


backmap_gamm_terms <- function(x, col_names) {
  x <- gsub("\\.time_norm",    col_names$time,                                x)
  x <- gsub("\\.tone_ord",     paste0(col_names$tone, ".ord"),                x)
  x <- gsub("\\.tone",         col_names$tone,                                x)
  x <- gsub("\\.speaker_tone", paste0(col_names$speaker, ".", col_names$tone), x)
  x <- gsub("\\.speaker",      col_names$speaker,                             x)
  x <- gsub("\\.item",         col_names$item,                                x)
  if (!is.null(col_names$duration)) {
    x <- gsub("\\.duration", col_names$duration, x)
  }
  x
}


#' Diagnose a fitted GAMM
#'
#' @description
#' Extracts the standard model-checking diagnostics for a fitted
#' [fit_gamm()] object, packaged as plain numeric tables and data frames
#' so a caller (e.g. the Shiny app) can render them however it likes.
#' Mirrors what [mgcv::gam.check()] reports, but returns the pieces as
#' data rather than drawing base-graphics plots.
#'
#' @details
#' The returned diagnostics answer the three questions that decide
#' whether a GAMM fit can be trusted:
#'
#' * **Are the residuals well-behaved?** `resid_df` holds the deviance
#'   residuals, fitted values, and observed response, enough to draw a
#'   Q-Q plot, a residuals-vs-fitted plot, a residual histogram, and an
#'   observed-vs-fitted plot.
#' * **Is the basis dimension `k` large enough?** `k_check` is
#'   [mgcv::k.check()]'s table (k', edf, k-index, p-value) with the term
#'   names mapped back to the user's columns. A `k_flag` column marks the
#'   spline smooths where a low k-index together with a small p-value
#'   suggests `k` is too low and the model should be refitted with a
#'   larger `k`. Random-effect terms (by-speaker / by-item) are flagged
#'   `"na"` because the k-index is not a meaningful check for them.
#' * **Is there leftover temporal autocorrelation?** `acf` is the lag /
#'   autocorrelation of the residuals with the white-noise band in
#'   `acf_ci`. Autocorrelation decaying slowly across lags is the signal
#'   that an AR1 correction (`use_ar1 = TRUE` in [fit_gamm()]) is worth
#'   turning on.
#'
#' `concurvity` is [mgcv::concurvity()]'s full table (0-1, higher means
#' more confounding between smooths) when it can be computed.
#'
#' The ACF is computed **per token** (the residuals are split by token and
#' the within-token lag products are pooled, in the spirit of
#' [itsadug::acf_resid()]) so that token boundaries do not contaminate the
#' low lags — important because f0 tokens are short. When the fit used an
#' AR1 correction the residuals are first whitened within each token
#' (`e_i - rho * e_{i-1}`, reset at each token start), so the ACF then
#' shows whether the AR1 term actually removed the autocorrelation rather
#' than the pre-correction picture. `acf_grouped` / `acf_whitened` record
#' which path was taken (a plain concatenated ACF is used only as a
#' fallback if residuals cannot be aligned to tokens).
#'
#' @param gamm_obj An object of class `"shinytone_gamm"` from
#'   [fit_gamm()].
#'
#' @return An S3 object of class `"shinytone_gamm_diag"`, a list with:
#' * `resid_df`: data frame with `residual` (deviance), `fitted`, and
#'   `observed`.
#' * `k_check`: data frame version of [mgcv::k.check()] with a `Smooth`
#'   column and a `k_flag` column (`"ok"`, `"low"`, or `"na"`), or `NULL`
#'   if it could not be computed.
#' * `concurvity`: data frame version of [mgcv::concurvity()], or `NULL`.
#' * `acf`: data frame with `lag` and `acf` (per token; AR1-whitened when
#'   the fit used an AR1 correction).
#' * `acf_ci`: half-width of the white-noise confidence band.
#' * `acf_grouped`, `acf_whitened`: logicals recording whether the ACF was
#'   computed per token and whether it was AR1-whitened.
#' * `n`: number of residuals.
#' * `rho`, `use_ar1`: AR1 information carried over from the fit.
#' * `family`: the model's error family.
#'
#' @seealso [fit_gamm()] for the model fit, [mgcv::gam.check()] and
#'   [mgcv::k.check()] for the underlying diagnostics.
#'
#' @references
#' Wood, S. N. (2017). \emph{Generalized Additive Models: An Introduction
#' with R} (2nd ed.). Chapman and Hall/CRC.
#'
#' @examples
#' \dontrun{
#' data(sample_f0)
#' normed <- normalise_f0(sample_f0, f0 = "f0_Hz",
#'                        speaker = "speaker", tone = "tone")
#' gamm <- fit_gamm(normed, f0 = "f0_st", time = "time", token = "token",
#'                  tone = "tone", speaker = "speaker", item = "char")
#' diag <- diagnose_gamm(gamm)
#' head(diag$k_check)
#' }
#' @export
#' @importFrom stats residuals fitted acf
diagnose_gamm <- function(gamm_obj) {
  if (!inherits(gamm_obj, "shinytone_gamm")) {
    stop("`gamm_obj` must be a shinytone_gamm object from fit_gamm().",
         call. = FALSE)
  }
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv is required for GAMM diagnostics. Install with install.packages('mgcv').",
         call. = FALSE)
  }

  model     <- gamm_obj$model
  col_names <- gamm_obj$col_names

  # --- Residuals / fitted (the four gam.check panels) ----------------------
  res_dev  <- as.numeric(stats::residuals(model, type = "deviance"))
  res_resp <- as.numeric(stats::residuals(model, type = "response"))
  fit_vals <- as.numeric(stats::fitted(model))
  observed <- fit_vals + res_resp
  resid_df <- data.frame(
    residual = res_dev,
    fitted   = fit_vals,
    observed = observed,
    stringsAsFactors = FALSE
  )

  # --- Basis-dimension (k) check -------------------------------------------
  # k.check re-simulates, so guard it; some model structures can trip it up.
  k_check <- tryCatch({
    kc <- mgcv::k.check(model)
    if (is.null(kc) || nrow(kc) == 0) {
      NULL
    } else {
      orig  <- rownames(kc)
      kidx  <- suppressWarnings(as.numeric(kc[, "k-index"]))
      pval  <- suppressWarnings(as.numeric(kc[, "p-value"]))
      # A term is a spline smooth (where the k-index diagnostic is meaningful)
      # when it is a smooth of the time axis or duration and NOT a grouped
      # factor smooth "s(x,group)" random effect (those carry a comma).
      is_spline <- !grepl(",", orig, fixed = TRUE) &
                   grepl("\\.time_norm|\\.duration", orig)
      k_flag <- ifelse(
        !is_spline, "na",
        ifelse(!is.na(kidx) & kidx < 1 & !is.na(pval) & pval < 0.05,
               "low", "ok"))
      df <- data.frame(Smooth = backmap_gamm_terms(orig, col_names),
                       stringsAsFactors = FALSE)
      df <- cbind(df, as.data.frame(kc, check.names = FALSE))
      df$k_flag <- k_flag
      rownames(df) <- NULL
      df
    }
  }, error = function(e) NULL)

  # --- Concurvity (confounding between smooths) ----------------------------
  concurvity <- tryCatch({
    cc <- mgcv::concurvity(model, full = TRUE)
    df <- data.frame(Measure = rownames(cc),
                     as.data.frame(cc, check.names = FALSE),
                     check.names = FALSE, stringsAsFactors = FALSE)
    names(df)[-1] <- backmap_gamm_terms(names(df)[-1], col_names)
    rownames(df) <- NULL
    df
  }, error = function(e) NULL)

  # --- Residual autocorrelation --------------------------------------------
  # Prefer a per-token ACF (see grouped_acf) so token boundaries do not
  # contaminate the lags, and whiten by the AR1 rho when one was used so the
  # panel can actually show whether the correction worked. Fall back to a plain
  # ACF only if the residuals cannot be aligned to their tokens.
  n_resid <- length(res_dev)
  dat     <- gamm_obj$data_for_predict
  tok     <- if (!is.null(dat) && ".token" %in% names(dat))
               as.character(dat$.token) else NULL
  if (!is.null(tok) && !is.null(model$na.action)) {
    tok <- tok[-as.integer(model$na.action)]
  }
  whiten_rho <- if (!is.null(gamm_obj$rho) && is.finite(gamm_obj$rho))
                  gamm_obj$rho else NULL

  if (!is.null(tok) && length(tok) == n_resid && !anyNA(res_dev)) {
    acf_df       <- grouped_acf(res_dev, tok, rho = whiten_rho)
    acf_grouped  <- TRUE
    acf_whitened <- !is.null(whiten_rho)
  } else {
    acf_obj      <- stats::acf(res_dev, plot = FALSE, na.action = stats::na.pass)
    acf_df       <- data.frame(lag = as.numeric(acf_obj$lag),
                               acf = as.numeric(acf_obj$acf))
    acf_grouped  <- FALSE
    acf_whitened <- FALSE
  }
  acf_ci <- if (n_resid > 0) 1.96 / sqrt(n_resid) else NA_real_

  structure(
    list(
      resid_df     = resid_df,
      k_check      = k_check,
      concurvity   = concurvity,
      acf          = acf_df,
      acf_ci       = acf_ci,
      acf_grouped  = acf_grouped,
      acf_whitened = acf_whitened,
      n            = n_resid,
      rho          = gamm_obj$rho,
      use_ar1      = !is.null(gamm_obj$rho),
      family       = tryCatch(model$family$family, error = function(e) NA_character_),
      col_names    = col_names
    ),
    class = "shinytone_gamm_diag"
  )
}
