skip_if_not_installed("lme4")

# Small synthetic dataset that lme4 can fit quickly.
make_gca_data <- function(n_speakers = 4, n_tones = 3, n_items = 5,
                          n_reps = 2, n_frames = 10) {
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
    f0  <- base + 20 * t + rnorm(n_frames, sd = 5)
    data.frame(
      speaker = row$speaker, tone = row$tone, item = row$item,
      token = row$token, time = t, f0 = f0,
      stringsAsFactors = FALSE
    )
  }))
}

test_that("fit_gca returns a shinytone_gca object with required fields", {
  df <- make_gca_data()
  fit <- suppressMessages(suppressWarnings(
    fit_gca(df, degree = 2,
            random_intercept_speaker = TRUE, random_slope_speaker = FALSE,
            random_intercept_item = TRUE,    random_slope_item    = FALSE)
  ))
  expect_s3_class(fit, "shinytone_gca")
  expect_true(all(c("model", "formula_str", "poly_coefs", "degree",
                    "col_names") %in% names(fit)))
  expect_equal(fit$degree, 2)
})

test_that("fit_gca display formula uses original column names", {
  df <- make_gca_data()
  fit <- suppressMessages(suppressWarnings(
    fit_gca(df, degree = 2,
            random_slope_speaker = FALSE, random_slope_item = FALSE)
  ))
  expect_match(fit$formula_str, "f0")
  expect_match(fit$formula_str, "tone")
  expect_match(fit$formula_str, "speaker")
  expect_match(fit$formula_str, "item")
  expect_false(grepl("\\.tone", fit$formula_str))   # internal names cleaned
})

test_that("predict_gca returns the expected shape", {
  df <- make_gca_data()
  fit <- suppressMessages(suppressWarnings(
    fit_gca(df, degree = 2,
            random_slope_speaker = FALSE, random_slope_item = FALSE)
  ))
  preds <- predict_gca(fit, n = 50)
  expect_true(all(c("time", "f0_predicted", "tone") %in% names(preds)))
  expect_equal(nrow(preds), 50 * length(unique(df$tone)))
  expect_true(all(preds$time >= 0 & preds$time <= 1))
})

test_that("fit_gca errors on missing columns", {
  df <- make_gca_data()
  expect_error(fit_gca(df, f0 = "nonexistent"), "Column.*not found")
})

test_that("fit_gca errors on invalid degree", {
  df <- make_gca_data()
  expect_error(fit_gca(df, degree = 0), "degree")
  expect_error(fit_gca(df, degree = 4), "degree")
})

test_that("predict_gca rejects non-shinytone_gca input", {
  expect_error(predict_gca(list(model = NULL)), "shinytone_gca")
})
