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
  # initial (no-AR1) fit's residuals: the per-token grouped estimate.
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

# Rows without f0 at both edges of every token, as a track over padded
# audio has for silence or a voiceless consonant.
pad_edges <- function(df) {
  do.call(rbind, lapply(split(df, df$token), function(d) {
    edge <- d[c(1, 1, nrow(d)), ]
    edge$f0   <- NA
    edge$time <- c(-0.2, -0.1, 1.1)
    rbind(edge[1:2, ], d, edge[3, ])
  }))
}

test_that("fit_gamm starts the AR1 correction on each token's first row with f0", {
  df <- make_gamm_data()
  pad <- pad_edges(df)
  pad$tn <- resolve_time_norm(pad, "time", "token", f0 = "f0")$time_norm
  fit_ar1 <- function(d, ...) suppressMessages(suppressWarnings(
    fit_gamm(d, k = 5, smooth_type = "separate", random_smooth = "none",
             random_intercept_item = TRUE, use_ar1 = TRUE, ...)))
  fit_pad <- fit_ar1(pad)
  mf <- fit_pad$model$model
  ar_start <- mf[[grep("AR.start", names(mf), fixed = TRUE)]]
  expect_equal(sum(ar_start), length(unique(df$token)))
  # ... each on its token's first row with f0
  tok <- as.character(fit_pad$data_for_predict$.token)[-as.integer(fit_pad$model$na.action)]
  expect_identical(which(ar_start), which(!duplicated(tok)))

  # The same fit as on the rows with f0 alone, on the same time axis.
  fit_f0 <- fit_ar1(pad[!is.na(pad$f0), ], time = "tn", time_normalised = "yes")
  expect_equal(fit_pad$rho, fit_f0$rho, tolerance = 1e-8)
  expect_equal(predict_gamm(fit_pad)$f0_predicted,
               predict_gamm(fit_f0)$f0_predicted, tolerance = 1e-6)
})

test_that("fit_gamm starts the AR1 correction after a first row without f0 on a column used as it is", {
  # On raw time, the edge rows without f0 get no time and sort last, so each
  # token's first row has f0 anyway. A 0-1 column used as it is keeps them
  # first: here each token's first row has no f0, and bam() leaves it out.
  df <- make_gamm_data()
  names(df)[names(df) == "time"] <- "time_prop"
  df$f0[!duplicated(df$token)] <- NA
  fit_ar1 <- function(d) suppressMessages(suppressWarnings(
    fit_gamm(d, time = "time_prop", time_normalised = "yes", k = 5,
             smooth_type = "separate", random_smooth = "none",
             random_intercept_item = TRUE, use_ar1 = TRUE)))
  fit_na <- fit_ar1(df)
  expect_true(fit_na$time_prenormalised)
  mf <- fit_na$model$model
  ar_start <- mf[[grep("AR.start", names(mf), fixed = TRUE)]]
  expect_equal(sum(ar_start), length(unique(df$token)))
  tok <- as.character(fit_na$data_for_predict$.token)[-as.integer(fit_na$model$na.action)]
  expect_identical(which(ar_start), which(!duplicated(tok)))

  # The same fit as on the rows with f0 alone
  fit_f0 <- fit_ar1(df[!is.na(df$f0), ])
  expect_equal(fit_na$rho, fit_f0$rho, tolerance = 1e-8)
  expect_equal(predict_gamm(fit_na)$f0_predicted,
               predict_gamm(fit_f0)$f0_predicted, tolerance = 1e-6)
})

test_that("fit_gamm predicts a sequential axis over the part with f0", {
  df <- make_gamm_data(n_frames = 21)
  # Two syllables per token, end to end; no f0 before 0.19 on the axis. A
  # _tseq made elsewhere: one from normalise_time_landmarks() has no row
  # without f0 at its edges, as its first f0 is at 0.
  two <- do.call(rbind, lapply(split(df, df$token), function(d) {
    d2 <- d
    d2$time <- d$time + 1
    out <- rbind(d, d2)
    out$f0[out$time < 0.19] <- NA
    out
  }))
  names(two)[names(two) == "time"] <- "syl_tseq"
  fit <- suppressMessages(suppressWarnings(
    fit_gamm(two, time = "syl_tseq", k = 5, random_smooth = "none")))
  expect_equal(fit$time_range, c(0.2, 2), tolerance = 1e-8)
  expect_equal(fit$n_segments, 2L)
  expect_gte(min(predict_gamm(fit, n = 50)$time), 0.2 - 1e-8)
})

test_that("fit_gamm counts segments over the rows it fits", {
  df <- make_gamm_data(n_frames = 21)
  # A second segment after each syllable, with no f0 at all: a _tseq made
  # elsewhere, since normalise_time_landmarks() leaves such a segment NA
  two <- do.call(rbind, lapply(split(df, df$token), function(d) {
    d2 <- d
    d2$time <- d$time + 1
    d2$f0   <- NA
    rbind(d, d2)
  }))
  names(two)[names(two) == "time"] <- "syl_tseq"
  fit <- NULL
  expect_message(
    fit <- suppressWarnings(
      fit_gamm(two, time = "syl_tseq", k = 5, random_smooth = "none")),
    "(0 to 1)", fixed = TRUE)
  expect_equal(fit$n_segments, 1L)
  expect_equal(fit$time_range, c(0, 1), tolerance = 1e-8)
  expect_equal(attr(predict_gamm(fit, n = 20), "n_segments"), 1L)
})

test_that("raw time: rows without f0 at the edges change nothing", {
  df  <- make_gamm_data()
  pad <- pad_edges(df)
  fit <- function(d) suppressMessages(suppressWarnings(
    fit_gamm(d, k = 5, random_smooth = "none")))
  fit_pad <- fit(pad)
  expect_false(fit_pad$time_prenormalised)
  expect_equal(predict_gamm(fit_pad)$f0_predicted,
               predict_gamm(fit(pad[!is.na(pad$f0), ]))$f0_predicted,
               tolerance = 1e-6)
  expect_equal(predict_gamm(fit_pad)$f0_predicted,
               predict_gamm(fit(df))$f0_predicted, tolerance = 1e-6)
  cov <- fit_pad$time_coverage
  expect_identical(cov$tone, c("T1", "T2", "T3"))
  expect_equal(cov$t_first, rep(0, 3))
  expect_equal(cov$t_last, rep(1, 3))
  expect_identical(attr(predict_gamm(fit_pad), "time_coverage"), cov)
  # On time_prop used as it is, a tone with no f0 at its first point stops short
  tp <- df
  names(tp)[names(tp) == "time"] <- "time_prop"
  tp$f0[tp$tone == "T2" & tp$time_prop == 0] <- NA
  cov_tp <- suppressMessages(suppressWarnings(
    fit_gamm(tp, time = "time_prop", k = 5, random_smooth = "none")))$time_coverage
  expect_equal(cov_tp$t_first, c(0, 1 / 11, 0))
})

test_that("fit_gamm reads 0 Hz as no f0, as the time axis does", {
  df <- make_gamm_data()
  z  <- df
  z$f0[ave(z$time, z$token, FUN = seq_along) %in% c(1, 6, 12)] <- 0
  # "no": by value alone, removing the edge rows would change the decision
  fit <- function(d) suppressMessages(suppressWarnings(
    fit_gamm(d, k = 5, random_smooth = "none", time_normalised = "no")))
  f_z <- fit(z)
  expect_equal(predict_gamm(f_z)$f0_predicted,
               predict_gamm(fit(z[z$f0 > 0, ]))$f0_predicted, tolerance = 1e-6)
  expect_equal(nrow(f_z$model$model), sum(z$f0 > 0))
  # AR1 starts on each token's first row with f0 above 0
  f_ar <- suppressMessages(suppressWarnings(
    fit_gamm(z, k = 5, random_smooth = "none", use_ar1 = TRUE,
             time_normalised = "no")))
  st <- f_ar$data_for_predict$.start_event
  expect_equal(sum(st), length(unique(z$token)))
  expect_true(all(f_ar$data_for_predict$.f0[st] > 0))
})

# Disyllabic tokens with syllable landmark columns: syllable 1's f0 runs
# 0.05-0.20 s and syllable 2's 0.25-0.40 s, with rows without f0 at the
# token's edges and on both sides of the boundary at 0.22 s.
make_syllable_data <- function() {
  df <- make_gamm_data()
  do.call(rbind, lapply(split(df, df$token), function(d) {
    s1 <- d
    s1$time <- 0.05 + 0.15 * d$time
    s2 <- d
    s2$time <- 0.25 + 0.15 * d$time
    s2$f0   <- d$f0 + 40 * d$time
    gap <- d[rep(1, 5), ]
    gap$f0   <- NA
    gap$time <- c(0.01, 0.03, 0.21, 0.23, 0.43)
    out <- rbind(s1, s2, gap)
    out <- out[order(out$time), ]
    out$syllable_i     <- ifelse(out$time < 0.22, 1L, 2L)
    out$syllable_start <- ifelse(out$time < 0.22, 0, 0.22)
    out$syllable_end   <- ifelse(out$time < 0.22, 0.22, 0.45)
    out
  }))
}

test_that("fit_gamm on a <tier>_tseq from normalise_time_landmarks()", {
  syl <- normalise_time_landmarks(make_syllable_data(), "time", "syllable")
  fit <- function(d) suppressMessages(suppressWarnings(
    fit_gamm(d, time = "syllable_tseq", k = 8, random_smooth = "none")))
  f_all <- fit(syl)
  expect_equal(f_all$time_axis, "sequential")
  expect_equal(f_all$time_range, c(0, 2))
  expect_equal(f_all$n_segments, 2L)
  f_f0 <- fit(syl[!is.na(syl$f0), ])
  expect_equal(predict_gamm(f_all)$f0_predicted, predict_gamm(f_f0)$f0_predicted,
               tolerance = 1e-8)
  # The two rows at each boundary keep their order in the data: syllable
  # 1's last f0, then syllable 2's first
  dp <- f_all$data_for_predict
  at1 <- dp[which(dp$.time_norm == 1), ]
  expect_equal(nrow(at1), 2L * length(unique(syl$token)))
  expect_identical(at1$syllable_i, rep(1:2, length(unique(syl$token))))
  shuffled <- syl[rev(seq_len(nrow(syl))), ]
  dp2 <- fit(shuffled)$data_for_predict
  expect_identical(dp2$syllable_i[which(dp2$.time_norm == 1)],
                   rep(2:1, length(unique(syl$token))))
})

test_that("fit_gamm keeps a <tier>_tbound on its own scale", {
  syl <- normalise_time_landmarks(make_syllable_data(), "time", "syllable")
  # 0 at the first f0 (0.05 s), 1 at the boundary (0.22 s), 2 at the last
  # f0 (0.40 s); the rows without f0 at 0.21 and 0.23 s keep their place
  k <- !is.na(syl$f0)
  expect_equal(range(syl$syllable_tbound[k]), c(0, 2))
  expect_equal(unique(syl$syllable_tbound[syl$time == 0.21]), 1 - 0.01 / 0.17)
  expect_equal(unique(syl$syllable_tbound[syl$time == 0.23]), 1 + 0.01 / 0.18)
  fit <- function(d) suppressWarnings(
    fit_gamm(d, time = "syllable_tbound", k = 8, random_smooth = "none"))
  f_all <- NULL
  expect_message(f_all <- fit(syl), "(0 to 2)", fixed = TRUE)
  expect_equal(f_all$time_axis, "sequential")
  expect_identical(f_all$data_for_predict$.time_norm,
                   f_all$data_for_predict$syllable_tbound)
  expect_equal(f_all$time_range, c(0, 2))
  expect_equal(f_all$n_segments, 2L)
  pr <- predict_gamm(f_all, n = 41)
  expect_equal(range(pr$time), c(0, 2))
  # The same fit with the rows without f0 removed
  f_f0 <- suppressMessages(fit(syl[k, ]))
  expect_equal(pr$f0_predicted, predict_gamm(f_f0, n = 41)$f0_predicted,
               tolerance = 1e-8)
  # "no" still rescales it per token, to 0-1
  f_no <- suppressMessages(suppressWarnings(
    fit_gamm(syl, time = "syllable_tbound", k = 8, random_smooth = "none",
             time_normalised = "no")))
  expect_equal(f_no$time_axis, "unit")
})

test_that("grouped_acf restarts the whitening at the starts it is given", {
  set.seed(3)
  res <- rnorm(30)
  tok <- rep(c("a", "b", "c"), each = 10)
  expect_equal(grouped_acf(res, tok, rho = 0.5, starts = !duplicated(tok)),
               grouped_acf(res, tok, rho = 0.5))
  one <- grouped_acf(res, tok, rho = 0.5, starts = rep(FALSE, 30))
  expect_false(isTRUE(all.equal(one, grouped_acf(res, tok, rho = 0.5))))
  # Without a rho there is nothing to whiten, so the starts do not matter.
  expect_equal(grouped_acf(res, tok, starts = rep(FALSE, 30)),
               grouped_acf(res, tok))
})

test_that("diagnose_gamm whitens along the model's AR starts", {
  df <- make_gamm_data()
  # Rows without f0 at the start of every token
  pad <- do.call(rbind, lapply(split(df, df$token), function(d) {
    edge <- d[c(1, 1), ]
    edge$f0   <- NA
    edge$time <- c(-0.2, -0.1)
    rbind(edge, d)
  }))
  fit <- suppressMessages(suppressWarnings(
    fit_gamm(pad, k = 5, smooth_type = "separate", random_smooth = "none",
             random_intercept_item = TRUE, use_ar1 = TRUE)))
  res <- as.numeric(stats::residuals(fit$model, type = "deviance"))
  tok <- as.character(fit$data_for_predict$.token)[-as.integer(fit$model$na.action)]
  # One start per token, on its first fitted row, so the whitening restarts
  # at each token.
  diag <- suppressWarnings(diagnose_gamm(fit))
  expect_true(diag$acf_whitened)
  expect_equal(diag$acf, grouped_acf(res, tok, rho = fit$rho))

  # A model whose starts were lost (they sat on rows bam() left out) was
  # whitened as one series, and the diagnostics follow it.
  lost <- fit
  lost$model$model[["(AR.start)"]] <- rep(FALSE, length(res))
  diag_lost <- suppressWarnings(diagnose_gamm(lost))
  expect_equal(diag_lost$acf,
               grouped_acf(res, tok, rho = fit$rho, starts = rep(FALSE, length(res))))
  expect_false(isTRUE(all.equal(diag_lost$acf, diag$acf)))
})

test_that("fit_gamm puts an AR start on every token of f0_long.csv", {
  skip_on_cran()
  path <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(path), "test/f0_long.csv not found")
  fl <- zero_hz_to_na(utils::read.csv(path, encoding = "UTF-8",
                                      stringsAsFactors = FALSE))
  # Every token starts and ends with rows without f0. Speaker and the
  # repetition number are read from the token ID; the repetition stands in
  # for tone, since any grouping will do for the AR starts.
  fl$speaker <- sub("^([A-Za-z]+[0-9]+).*$", "\\1", fl$token, perl = TRUE)
  fl$tone    <- paste0("R", sub("^.*?([0-9]+)s[0-9.]+$", "\\1", fl$token,
                                perl = TRUE))
  fl$item    <- fl$token
  fit <- suppressMessages(suppressWarnings(
    fit_gamm(fl, k = 5, random_smooth = "none", random_intercept_speaker = TRUE,
             random_intercept_item = FALSE, use_ar1 = TRUE)))
  ar_start <- fit$model$model[["(AR.start)"]]
  expect_equal(length(unique(fl$token)), 1853L)
  expect_equal(nrow(fit$model$model), sum(!is.na(fl$f0)))
  expect_equal(sum(ar_start), 1853L)
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
