# ---------- time_already_normalised ------------------------------------------

test_that("detects a proportional [0,1] column, including partial-span tokens", {
  tok <- rep(c("a", "b", "c", "d"), each = 11)
  t01 <- c(seq(0, 1, length.out = 11), seq(0, 1, length.out = 11),
           seq(0, 1, length.out = 11), seq(0, 0.95, length.out = 11))
  expect_true(time_already_normalised(t01, tok))
})

test_that("rejects seconds-scale, ms-scale, and sequential-landmark time", {
  tok <- rep(c("a", "b"), each = 11)
  secs <- c(seq(0.01, 0.45, length.out = 11), seq(0.01, 0.92, length.out = 11))
  expect_false(time_already_normalised(secs, tok))          # durations << 1
  expect_false(time_already_normalised(secs * 1000, tok))   # ms scale
  tseq <- c(seq(0, 2, length.out = 11), seq(0, 2, length.out = 11))
  expect_false(time_already_normalised(tseq, tok))          # 0..n_segments
  expect_false(time_already_normalised(rep(NA_real_, 4), c("a", "a", "b", "b")))
})

test_that("rejects when tokens are variable sub-spans of [0,1]", {
  # All values inside [0,1] and pooled range covers it, but the median token
  # span is small — e.g. raw seconds with one near-1 s token.
  tok <- rep(c("a", "b", "c"), each = 5)
  tv  <- c(seq(0, 0.3, length.out = 5), seq(0.2, 0.5, length.out = 5),
           seq(0.6, 1, length.out = 5))
  expect_false(time_already_normalised(tv, tok))
})

# ---------- resolve_time_norm ------------------------------------------------

test_that("rescale path reproduces the per-token min-max, with 0.5 degenerates", {
  df <- data.frame(token = c("a", "a", "a", "b", "b", "c"),
                   time  = c(0.10, 0.20, 0.30, 5.0, 7.0, 2.0))
  res <- resolve_time_norm(df, "time", "token", "no")
  expect_false(res$prenormalised)
  expect_equal(res$time_norm, c(0, 0.5, 1, 0, 1, 0.5))  # c: single sample -> 0.5
})

test_that("NA times propagate as NA in otherwise valid tokens", {
  df <- data.frame(token = c("a", "a", "a"), time = c(0, NA, 1))
  res <- resolve_time_norm(df, "time", "token", "no")
  expect_equal(res$time_norm, c(0, NA, 1))
})

test_that("rescale path matches the historical per-token dplyr block exactly", {
  # The block resolve_time_norm() replaced in fit_gca/fit_gamm/compute_mean_contour.
  old_block <- function(data, time, token) {
    d <- data |>
      dplyr::group_by(.data[[token]]) |>
      dplyr::mutate(.tn = {
        t_raw <- .data[[time]]
        t_min <- min(t_raw, na.rm = TRUE)
        t_max <- max(t_raw, na.rm = TRUE)
        if (is.na(t_min) || t_max == t_min) rep(0.5, dplyr::n())
        else (t_raw - t_min) / (t_max - t_min)
      }) |>
      dplyr::ungroup()
    d$.tn
  }
  cases <- list(
    normal   = data.frame(token = c("a", "a", "b", "b"), time = c(0, 2, 5, 9)),
    all_na   = data.frame(token = c("a", "a", "b", "b"), time = c(NA, NA, 5, 9)),
    some_na  = data.frame(token = c("a", "a", "a", "b"), time = c(0, NA, 4, 7)),
    zero_rng = data.frame(token = c("a", "a", "b"), time = c(3, 3, 8)),
    int_time = data.frame(token = c("a", "a", "b", "b"), time = c(0L, 10L, 3L, 9L)),
    num_tok  = data.frame(token = c(2, 2, 10, 10), time = c(0, 4, 1, 3)),
    fct_tok  = data.frame(token = factor(c("b", "b", "a", "a")), time = c(0, 4, 1, 3))
  )
  for (nm in names(cases)) {
    d <- cases[[nm]]
    expect_equal(resolve_time_norm(d, "time", "token", "no")$time_norm,
                 as.numeric(suppressWarnings(old_block(d, "time", "token"))),
                 info = nm)
  }
})

test_that("prenormalised path uses values as-is and messages on auto", {
  tok <- rep(c("a", "b"), each = 11)
  df <- data.frame(token = tok,
                   time  = c(seq(0, 1, length.out = 11),
                             seq(0, 0.95, length.out = 11)))
  expect_message(res <- resolve_time_norm(df, "time", "token", "auto"),
                 "already normalised")
  expect_true(res$prenormalised)
  expect_equal(res$time_norm, df$time)   # partial token NOT stretched
  expect_silent(resolve_time_norm(df, "time", "token", "auto", quiet = TRUE))
})

test_that("time_normalised = 'yes' enforces the [0,1] range", {
  df <- data.frame(token = c("a", "a"), time = c(0.1, 1.4))
  expect_error(resolve_time_norm(df, "time", "token", "yes"),
               "outside \\[0, 1\\]")
  df2 <- data.frame(token = c("a", "a"), time = c(0.2, 0.8))
  res <- resolve_time_norm(df2, "time", "token", "yes")
  expect_true(res$prenormalised)
  expect_equal(res$time_norm, c(0.2, 0.8))
})

# ---------- fit_polynomial integration ---------------------------------------

test_that("fit_polynomial does not re-stretch an already-proportional token", {
  # Three full-span tokens plus one sampled only over [0, 0.5] of the same
  # proportional axis, all on the same line f0 = 100 + 50 * t.
  mk <- function(tok, tmax, np) {
    t <- seq(0, tmax, length.out = np)
    data.frame(token = tok, time = t, f0 = 100 + 50 * t,
               speaker = "s1", tone = "T1", stringsAsFactors = FALSE)
  }
  df <- rbind(mk("a", 1, 11), mk("b", 1, 11), mk("c", 1, 11), mk("d", 0.5, 6))

  co_auto <- suppressMessages(
    fit_polynomial(df, f0 = "f0", token = "token", time = "time",
                   speaker = "speaker", tone = "tone", degree = 1))
  # Same underlying line => identical coefficients for every token, including
  # the partial-span one: c0 = 125 (mean at t = 0.5), c1 = 25.
  expect_true(isTRUE(attr(co_auto, "time_prenormalised")))
  expect_equal(co_auto$c0, rep(125, 4))
  expect_equal(co_auto$c1, rep(25, 4))

  co_no <- fit_polynomial(df, f0 = "f0", token = "token", time = "time",
                          speaker = "speaker", tone = "tone", degree = 1,
                          time_normalised = "no")
  # Forced per-token rescale stretches token d over the full basis: its
  # slope halves relative to the shared line.
  expect_false(isTRUE(attr(co_no, "time_prenormalised")))
  expect_equal(co_no$c1[co_no$token == "d"], 12.5)
  expect_equal(co_no$c1[co_no$token != "d"], rep(25, 3))
})

test_that("fit_polynomial on unnormalised time is unchanged by the new default", {
  df <- data.frame(
    token   = rep(c("a", "b"), each = 5),
    time    = c(seq(0, 0.4, by = 0.1), seq(0, 0.2, by = 0.05)),
    f0      = c(100, 110, 120, 130, 140, 200, 205, 210, 215, 220),
    speaker = "s1", tone = "T1", stringsAsFactors = FALSE)
  co  <- fit_polynomial(df, degree = 1)
  co2 <- fit_polynomial(df, degree = 1, time_normalised = "no")
  expect_equal(co$c0, co2$c0)
  expect_equal(co$c1, co2$c1)
  expect_equal(co$c0, c(120, 210))
})

# ---------- compute_mean_contour integration ---------------------------------

test_that("compute_mean_contour keeps proportional positions under auto", {
  # Three full-span tokens (f0 = 100) so the median-span condition detects the
  # proportional axis, plus one token sampled only over [0, 0.5] (f0 = 200).
  tok <- rep(c("a1", "a2", "a3", "b"), each = 11)
  df <- data.frame(
    token = tok,
    time  = c(rep(seq(0, 1, length.out = 11), 3), seq(0, 0.5, length.out = 11)),
    f0    = rep(c(100, 100, 100, 200), each = 11),
    tone  = "T1", stringsAsFactors = FALSE)

  mc_auto <- suppressMessages(compute_mean_contour(df, n_bins = 3))
  # Token b's samples stay within [0, 0.5]: the final bin holds only the
  # full-span tokens.
  expect_equal(mc_auto$f0_predicted[mc_auto$time == 1], 100)

  mc_no <- compute_mean_contour(df, n_bins = 3, time_normalised = "no")
  # Rescaled per token, b reaches the final bin too: 9 samples at 100 plus 3
  # at 200.
  expect_equal(mc_no$f0_predicted[mc_no$time == 1], 125)
})

# ---------- fit_gca / fit_gamm record the decision ---------------------------

test_that("fit_gca records time_prenormalised and predicts on [0,1]", {
  skip_if_not_installed("lme4")
  set.seed(42)
  grid <- expand.grid(speaker = c("s1", "s2"), tone = c("T1", "T2"),
                      item = c("w1", "w2"), rep = 1:2,
                      stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, ]
    t <- seq(0, 1, length.out = 8)
    data.frame(token = paste(g$speaker, g$tone, g$item, g$rep, sep = "_"),
               time = t,
               f0 = 100 + ifelse(g$tone == "T1", 10, -10) * t + rnorm(8, sd = 2),
               speaker = g$speaker, tone = g$tone, item = g$item,
               stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, rows)

  fit_auto <- suppressMessages(suppressWarnings(
    fit_gca(df, degree = 1, random_slope_speaker = FALSE)))
  expect_true(fit_auto$time_prenormalised)
  preds <- predict_gca(fit_auto, n = 20)
  expect_true(all(preds$time >= 0 & preds$time <= 1))

  fit_no <- suppressWarnings(
    fit_gca(df, degree = 1, random_slope_speaker = FALSE,
            time_normalised = "no"))
  expect_false(fit_no$time_prenormalised)
  # Full-span tokens: skipping the rescale is numerically a no-op.
  expect_equal(lme4::fixef(fit_auto$model), lme4::fixef(fit_no$model),
               tolerance = 1e-8)
})

test_that("fit_gamm records time_prenormalised", {
  skip_if_not_installed("mgcv")
  set.seed(42)
  grid <- expand.grid(speaker = c("s1", "s2"), tone = c("T1", "T2"),
                      item = c("w1", "w2"), rep = 1:2,
                      stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, ]
    t <- seq(0, 1, length.out = 8)
    data.frame(token = paste(g$speaker, g$tone, g$item, g$rep, sep = "_"),
               time = t,
               f0 = 100 + ifelse(g$tone == "T1", 10, -10) * sin(pi * t) +
                    rnorm(8, sd = 2),
               speaker = g$speaker, tone = g$tone, item = g$item,
               stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, rows)

  fit_auto <- suppressMessages(suppressWarnings(
    fit_gamm(df, k = 4, random_smooth = "none",
             random_intercept_speaker = FALSE,
             random_intercept_item = FALSE)))
  expect_true(fit_auto$time_prenormalised)

  fit_no <- suppressWarnings(
    fit_gamm(df, k = 4, random_smooth = "none",
             random_intercept_speaker = FALSE,
             random_intercept_item = FALSE, time_normalised = "no"))
  expect_false(fit_no$time_prenormalised)
})

# ---------- input validation and grouping edge cases -------------------------

test_that("a factor time column is read by value, not by level code", {
  # Levels sort lexicographically ("10.5" before "2.0"), so level codes are not
  # even monotone in real time. Before this guard the fitters silently used them.
  df <- data.frame(token = "a",
                   time  = factor(c("0.15", "0.9", "2.0", "10.5")),
                   f0    = c(1, 2, 3, 4))
  res <- resolve_time_norm(df, "time", "token", "no")
  expect_equal(res$time_norm, (c(0.15, 0.9, 2.0, 10.5) - 0.15) / (10.5 - 0.15))
})

test_that("a non-numeric time column is a loud error, not silent nonsense", {
  df <- data.frame(token = "a", time = factor(c("high", "low")), f0 = c(1, 2))
  expect_error(resolve_time_norm(df, "time", "token", "no"), "not numeric")
  expect_false(time_already_normalised(df$time, df$token))
})

test_that("NA token IDs form their own group instead of collapsing to 0.5", {
  df <- data.frame(token = c("a", "a", NA, NA, NA), time = c(0, 1, 2, 4, 6))
  expect_equal(resolve_time_norm(df, "time", "token", "no")$time_norm,
               c(0, 1, 0, 0.5, 1))
})

test_that("token IDs are grouped by value, not by printed form", {
  # 0.1 + 0.2 and 0.3 both print as "0.3" but are distinct doubles.
  df <- data.frame(token = c(0.1 + 0.2, 0.1 + 0.2, 0.3, 0.3),
                   time  = c(0, 1, 10, 30))
  expect_equal(resolve_time_norm(df, "time", "token", "no")$time_norm,
               c(0, 1, 0, 1))
  # Numeric-looking IDs must not be ordered as strings ("10" before "2").
  df2 <- data.frame(token = c(1, 1, 2, 2, 10, 10), time = c(0, 2, 0, 4, 0, 8))
  expect_equal(resolve_time_norm(df2, "time", "token", "no")$time_norm,
               c(0, 1, 0, 1, 0, 1))
})

test_that("time_normalised = 'yes' rejects infinite values", {
  df <- data.frame(token = "a", time = c(0.2, 0.8, Inf))
  expect_error(resolve_time_norm(df, "time", "token", "yes"),
               "outside \\[0, 1\\]")
})
