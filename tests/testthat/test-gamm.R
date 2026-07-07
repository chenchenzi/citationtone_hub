skip_if_not_installed("mgcv")

# Reuse the same toy generator as test-gca.R but inline (testthat files are
# self-contained).
make_gamm_data <- function(n_speakers = 4, n_tones = 3, n_items = 4,
                           n_reps = 2, n_frames = 12) {
  set.seed(42)
  out <- expand.grid(
    speaker = paste0("S", seq_len(n_speakers)),
    tone    = paste0("T", seq_len(n_tones)),
    item    = paste0("w", seq_len(n_items)),
    rep     = seq_len(n_reps),
    stringsAsFactors = FALSE
  )
  out$token <- paste(out$speaker, out$tone, out$item, out$rep, sep = "_")
  do.call(rbind, lapply(seq_len(nrow(out)), function(i) {
    row <- out[i, ]
    t   <- seq(0, 1, length.out = n_frames)
    base <- as.numeric(factor(row$tone)) * 30 + 100
    f0  <- base + 20 * sin(pi * t) + rnorm(n_frames, sd = 5)
    data.frame(speaker = row$speaker, tone = row$tone, item = row$item,
               token = row$token, time = t, f0 = f0,
               stringsAsFactors = FALSE)
  }))
}

test_that("fit_gamm returns a shinytone_gamm object (separate smooths)", {
  df <- make_gamm_data()
  fit <- suppressMessages(suppressWarnings(
    fit_gamm(df, k = 5, smooth_type = "separate",
             random_smooth = "none",
             random_intercept_speaker = TRUE,
             random_intercept_item    = TRUE,
             use_ar1 = FALSE)
  ))
  expect_s3_class(fit, "shinytone_gamm")
  expect_true(all(c("model", "formula_str", "smooth_type",
                    "random_smooth", "col_names") %in% names(fit)))
  expect_equal(fit$smooth_type, "separate")
})

test_that("fit_gamm handles difference smooths", {
  df <- make_gamm_data()
  fit <- suppressMessages(suppressWarnings(
    fit_gamm(df, k = 5, smooth_type = "difference",
             random_smooth = "none",
             random_intercept_speaker = TRUE,
             random_intercept_item    = TRUE)
  ))
  expect_s3_class(fit, "shinytone_gamm")
  expect_match(fit$formula_str, "tone.ord")
})

test_that("predict_gamm returns the expected shape", {
  df <- make_gamm_data()
  fit <- suppressMessages(suppressWarnings(
    fit_gamm(df, k = 5, smooth_type = "separate",
             random_smooth = "none",
             random_intercept_speaker = TRUE,
             random_intercept_item    = TRUE)
  ))
  preds <- predict_gamm(fit, n = 60)
  expect_true(all(c("time", "f0_predicted", "se", "tone") %in% names(preds)))
  expect_equal(nrow(preds), 60 * length(unique(df$tone)))
})

test_that("fit_gamm errors on missing column", {
  df <- make_gamm_data()
  expect_error(fit_gamm(df, f0 = "nonexistent"), "Column.*not found")
})

test_that("predict_gamm rejects non-shinytone_gamm input", {
  expect_error(predict_gamm(list(model = NULL)), "shinytone_gamm")
})

test_that("diagnose_gamm returns the expected structure", {
  df <- make_gamm_data()
  fit <- suppressMessages(suppressWarnings(
    fit_gamm(df, k = 5, smooth_type = "separate",
             random_smooth = "speaker",
             random_intercept_item = TRUE)
  ))
  diag <- suppressWarnings(diagnose_gamm(fit))

  expect_s3_class(diag, "shinytone_gamm_diag")
  expect_true(all(c("resid_df", "k_check", "concurvity", "acf",
                    "acf_ci", "n", "rho", "use_ar1") %in% names(diag)))

  # Residual frame has one row per fitted observation.
  expect_true(all(c("residual", "fitted", "observed") %in% names(diag$resid_df)))
  expect_equal(nrow(diag$resid_df), nrow(fit$model$model))
  expect_false(any(is.na(diag$resid_df$residual)))

  # ACF band matches n.
  expect_equal(diag$n, nrow(diag$resid_df))
  expect_equal(diag$acf_ci, 1.96 / sqrt(diag$n))
  expect_false(diag$use_ar1)
})

test_that("diagnose_gamm k_check back-maps terms and flags spline smooths", {
  df <- make_gamm_data()
  fit <- suppressMessages(suppressWarnings(
    fit_gamm(df, k = 5, smooth_type = "separate",
             random_smooth = "speaker",
             random_intercept_item = TRUE)
  ))
  diag <- suppressWarnings(diagnose_gamm(fit))

  expect_false(is.null(diag$k_check))
  expect_true(all(c("Smooth", "k_flag") %in% names(diag$k_check)))
  # Internal dotted names are mapped back to the user's columns.
  expect_false(any(grepl("\\.time_norm", diag$k_check$Smooth)))
  expect_true(any(grepl("time", diag$k_check$Smooth)))
  # k_flag is only ever one of the three allowed states.
  expect_true(all(diag$k_check$k_flag %in% c("ok", "low", "na")))
  # The by-speaker random smooth carries a comma, so it is not a spline check.
  rs_row <- diag$k_check[grepl(",", diag$k_check$Smooth), , drop = FALSE]
  if (nrow(rs_row) > 0) expect_true(all(rs_row$k_flag == "na"))
})

test_that("diagnose_gamm carries AR1 information through", {
  df <- make_gamm_data()
  fit <- suppressMessages(suppressWarnings(
    fit_gamm(df, k = 5, smooth_type = "separate",
             random_smooth = "none",
             random_intercept_item = TRUE, use_ar1 = TRUE)
  ))
  diag <- suppressWarnings(diagnose_gamm(fit))
  expect_true(diag$use_ar1)
  expect_true(is.numeric(diag$rho))
})

test_that("diagnose_gamm ACF is per token, and AR1-whitened when applicable", {
  # Data with genuine within-token AR(1) noise so an AR1 correction is real.
  set.seed(99)
  base <- expand.grid(speaker = paste0("S", 1:5), tone = paste0("T", 1:2),
                      item = paste0("w", 1:4), rep = 1:2, stringsAsFactors = FALSE)
  base$token <- paste(base$speaker, base$tone, base$item, base$rep, sep = "_")
  n_fr <- 20
  df <- do.call(rbind, lapply(seq_len(nrow(base)), function(i) {
    r <- base[i, ]; t <- seq(0, 1, length.out = n_fr)
    e <- numeric(n_fr); e[1] <- rnorm(1)
    for (j in 2:n_fr) e[j] <- 0.6 * e[j - 1] + rnorm(1)
    f0 <- as.numeric(factor(r$tone)) * 30 + 100 + 15 * sin(pi * t) + 4 * e
    data.frame(speaker = r$speaker, tone = r$tone, item = r$item,
               token = r$token, time = t, f0 = f0, stringsAsFactors = FALSE)
  }))

  # No AR1: grouped ACF, not whitened, exposes the autocorrelation.
  fit0  <- suppressWarnings(fit_gamm(df, k = 6, smooth_type = "separate",
                                     random_smooth = "none",
                                     random_intercept_item = TRUE))
  diag0 <- suppressWarnings(diagnose_gamm(fit0))
  expect_true(diag0$acf_grouped)
  expect_false(diag0$acf_whitened)
  expect_equal(diag0$acf$acf[1], 1)                 # lag 0 is 1 by construction
  expect_gt(diag0$acf$acf[2], 0.2)                  # lag-1 autocorrelation present

  # AR1: whitened per-token ACF should pull the lag-1 autocorrelation toward 0,
  # i.e. the panel can now confirm the correction worked.
  fit1  <- suppressWarnings(fit_gamm(df, k = 6, smooth_type = "separate",
                                     random_smooth = "none",
                                     random_intercept_item = TRUE, use_ar1 = TRUE))
  diag1 <- suppressWarnings(diagnose_gamm(fit1))
  expect_true(diag1$acf_whitened)
  expect_lt(abs(diag1$acf$acf[2]), 0.15)            # near white noise after whitening
  expect_lt(abs(diag1$acf$acf[2]), diag0$acf$acf[2])
})

test_that("fit_gamm estimates AR1 rho per token, not from the flat concatenation", {
  df <- make_gamm_data()
  fit_ar1 <- suppressMessages(suppressWarnings(
    fit_gamm(df, k = 5, smooth_type = "separate", random_smooth = "none",
             random_intercept_item = TRUE, use_ar1 = TRUE)
  ))
  # The stored rho must equal the within-token lag-1 autocorrelation of the
  # initial (no-AR1) fit's residuals — the per-token grouped estimate.
  fit0 <- suppressMessages(suppressWarnings(
    fit_gamm(df, k = 5, smooth_type = "separate", random_smooth = "none",
             random_intercept_item = TRUE, use_ar1 = FALSE)
  ))
  r0  <- stats::resid(fit0$model)
  tok <- as.character(fit0$data_for_predict$.token)
  if (!is.null(fit0$model$na.action)) tok <- tok[-as.integer(fit0$model$na.action)]
  rho_grouped <- grouped_acf(r0, tok, max_lag = 1)$acf[2]

  expect_equal(fit_ar1$rho, rho_grouped, tolerance = 1e-8)
  expect_true(fit_ar1$rho > -1 && fit_ar1$rho < 1)
})

test_that("diagnose_gamm falls back to a plain ACF without token grouping", {
  df  <- make_gamm_data()
  fit <- suppressMessages(suppressWarnings(
    fit_gamm(df, k = 5, smooth_type = "separate",
             random_smooth = "none", random_intercept_item = TRUE)
  ))
  fit$data_for_predict$.token <- NULL   # remove the grouping key
  diag <- suppressWarnings(diagnose_gamm(fit))
  expect_false(diag$acf_grouped)
  expect_true(all(c("lag", "acf") %in% names(diag$acf)))
})

test_that("diagnose_gamm rejects non-shinytone_gamm input", {
  expect_error(diagnose_gamm(list(model = NULL)), "shinytone_gamm")
})
