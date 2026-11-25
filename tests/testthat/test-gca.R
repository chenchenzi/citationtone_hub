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

test_that("fit_gca builds the basis on the rows it fits", {
  df <- make_gca_data()
  # Rows lmer() leaves out, inside the tokens: no f0 between two frames, and
  # a frame with f0 but no item. Neither changes any token's first or last
  # time, so the time axis is the same.
  extra <- do.call(rbind, lapply(split(df, df$token), function(d) {
    gap <- d[c(3, 3, 6), ]
    gap$time <- gap$time + c(0.01, 0.02, 0.01)
    gap$f0   <- c(NA, NA, 150)
    gap$item[3] <- NA
    gap
  }))
  padded <- rbind(df, extra)
  padded <- padded[order(padded$token, padded$time), ]
  fit_gca2 <- function(d, ...) suppressMessages(suppressWarnings(
    fit_gca(d, degree = 2, random_slope_speaker = FALSE, ...)))
  fit_df  <- fit_gca2(df)
  fit_pad <- fit_gca2(padded)
  expect_equal(nrow(fit_pad$model@frame), nrow(df))
  expect_equal(fit_pad$poly_coefs, fit_df$poly_coefs, tolerance = 1e-12)
  expect_equal(lme4::fixef(fit_pad$model), lme4::fixef(fit_df$model),
               tolerance = 1e-10)
  expect_equal(predict_gca(fit_pad)$f0_predicted,
               predict_gca(fit_df)$f0_predicted, tolerance = 1e-10)

  # With every row fitted, the basis is the one poly() gives on all rows.
  tn <- suppressMessages(resolve_time_norm(df, "time", "token"))$time_norm
  expect_equal(fit_df$poly_coefs, attr(stats::poly(tn, 2), "coefs"))
})

test_that("fit_gca fits a landmark axis with no time outside the tier", {
  df <- make_gca_data()
  # Each token gets a frame before and after the labelled interval, where
  # the landmark axis has no value; one of them also has no f0.
  lm_df <- do.call(rbind, lapply(split(df, df$token), function(d) {
    out <- d[c(1, seq_len(nrow(d)), nrow(d)), ]
    out$syl_tseq <- c(NA, d$time, NA)
    out$f0[1] <- NA
    out
  }))
  fit_gca2 <- function(d, ...) suppressMessages(suppressWarnings(
    fit_gca(d, time = "syl_tseq", degree = 2, random_slope_speaker = FALSE,
            time_normalised = "yes", ...)))
  fit_na <- NULL
  expect_no_error(fit_na <- fit_gca2(lm_df))
  expect_equal(nrow(fit_na$model@frame), nrow(df))
  fit_in <- fit_gca2(lm_df[!is.na(lm_df$syl_tseq), ])
  expect_equal(lme4::fixef(fit_na$model), lme4::fixef(fit_in$model),
               tolerance = 1e-10)
})

test_that("fit_gca says plainly when too few rows have f0", {
  df <- make_gca_data()
  df$f0[df$time > 0.15] <- NA     # two time points left, a degree 2 curve needs 3
  expect_error(suppressMessages(fit_gca(df, degree = 2)), "Too few rows to fit")
})

test_that("fit_gca keeps rows whose item or speaker is NA when the model has no term for it", {
  skip_if_not_installed("lme4")
  set.seed(11)
  d <- do.call(rbind, lapply(1:40, function(i) {
    tone <- c("T1", "T2")[1 + i %% 2]
    t <- seq(0, 0.2, length.out = 11)
    data.frame(token = paste0("k", i), speaker = paste0("s", 1 + i %% 4),
               item = paste0("w", 1 + i %% 5), tone = tone, time = t,
               f0 = 150 + (tone == "T2") * 200 * t + stats::rnorm(11, sd = 2))
  }))
  fit <- function(x, ...) suppressMessages(suppressWarnings(fit_gca(
    x, f0 = "f0", time = "time", token = "token", tone = "tone",
    speaker = "speaker", item = "item", degree = 2, ...)))
  na_item <- d
  na_item$item[na_item$token %in% c("k3", "k7", "k11")] <- NA
  # No item term: every row is fitted, as with the item filled in.
  a <- fit(na_item, random_intercept_item = FALSE)
  b <- fit(d, random_intercept_item = FALSE)
  expect_identical(nrow(a$model@frame), 440L)
  expect_identical(a$poly_coefs, b$poly_coefs)
  expect_equal(lme4::fixef(a$model), lme4::fixef(b$model))
  # With an item term the rows with no item go, and the basis follows them.
  c2 <- fit(na_item, random_intercept_item = TRUE)
  expect_identical(nrow(c2$model@frame), 407L)
  expect_equal(c2$poly_coefs,
               attr(stats::poly(rep(seq(0, 1, by = 0.1), 37), 2), "coefs"))
  # With no random effect chosen the model gets (1 | item), so they go too.
  e <- fit(na_item, random_intercept_speaker = FALSE, random_slope_speaker = FALSE,
           random_intercept_item = FALSE)
  expect_match(e$formula_str, "(1 | item)", fixed = TRUE)
  expect_identical(nrow(e$model@frame), 407L)
  # The same for speaker.
  na_spk <- d
  na_spk$speaker[na_spk$token %in% c("k2", "k5")] <- NA
  f <- fit(na_spk, random_intercept_speaker = FALSE, random_slope_speaker = FALSE)
  expect_identical(nrow(f$model@frame), 440L)
  g <- fit(na_spk, random_slope_speaker = FALSE)
  expect_identical(nrow(g$model@frame), 418L)
})

test_that("fit_gca on raw time: rows without f0 at the edges change nothing", {
  df  <- make_gca_data()
  pad <- do.call(rbind, lapply(split(df, df$token), function(d) {
    edge <- d[c(1, 1, nrow(d)), ]
    edge$f0   <- NA
    edge$time <- c(-0.2, -0.1, 1.1)
    rbind(edge[1:2, ], d, edge[3, ])
  }))
  fit <- function(d) suppressMessages(suppressWarnings(
    fit_gca(d, degree = 2, random_slope_speaker = FALSE, time_normalised = "no")))
  f_pad <- fit(pad)
  f_f0  <- fit(pad[!is.na(pad$f0), ])
  expect_false(f_pad$time_prenormalised)
  expect_equal(lme4::fixef(f_pad$model), lme4::fixef(f_f0$model), tolerance = 1e-10)
  expect_equal(f_pad$poly_coefs, f_f0$poly_coefs)
  expect_identical(nrow(f_pad$model@frame), nrow(df))
  # Every tone runs from 0 to 1 on the rescaled axis
  expect_equal(f_pad$time_coverage$t_first, rep(0, 3))
  expect_equal(f_pad$time_coverage$t_last, rep(1, 3))
})

test_that("fit_gca reads 0 Hz as no f0, as the time axis does", {
  df <- make_gca_data()
  z  <- df
  inner <- ave(z$time, z$token, FUN = seq_along) %in% c(1, 5, 10)
  z$f0[inner] <- 0              # 0 Hz at both edges and once inside each token
  fit <- function(d) suppressMessages(suppressWarnings(
    fit_gca(d, degree = 2, random_slope_speaker = FALSE, time_normalised = "no")))
  f_z  <- fit(z)
  f_na <- fit(z[z$f0 > 0, ])
  expect_equal(lme4::fixef(f_z$model), lme4::fixef(f_na$model), tolerance = 1e-10)
  expect_identical(nrow(f_z$model@frame), sum(z$f0 > 0))
})

test_that("fit_gca records where each tone's fitted rows start and end", {
  df <- make_gca_data()
  names(df)[names(df) == "time"] <- "time_prop"
  # time_prop is used as it is; T1 has no f0 at its first point
  df$f0[df$tone == "T1" & df$time_prop == 0] <- NA
  fit <- suppressMessages(suppressWarnings(
    fit_gca(df, time = "time_prop", degree = 2, random_slope_speaker = FALSE)))
  expect_true(fit$time_prenormalised)
  cov <- fit$time_coverage
  expect_identical(cov$tone, c("T1", "T2", "T3"))
  expect_equal(cov$t_first, c(1 / 9, 0, 0))
  expect_equal(cov$t_last, c(1, 1, 1))
  pred <- predict_gca(fit, n = 11)
  expect_identical(attr(pred, "time_coverage"), cov)
  expect_equal(range(pred$time), c(0, 1))    # the curve is extrapolated to 0
  old <- fit
  old$time_coverage <- NULL                  # a fit made by an earlier version
  expect_null(attr(predict_gca(old, n = 11), "time_coverage"))
})
