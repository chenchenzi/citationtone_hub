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
