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

  mc_auto <- suppressMessages(compute_mean_contour(df, n_points = 3))
  # Token b's samples stay within [0, 0.5], so it has no value at the final
  # time point, which averages only the full-span tokens.
  expect_equal(mc_auto$f0_predicted[mc_auto$time == 1], 100)

  mc_no <- compute_mean_contour(df, n_points = 3, time_normalised = "no")
  # Rescaled per token, b reaches the final time point too: three tokens at
  # 100 plus one at 200.
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

# ---------- sequential landmark axis (<tier>_tseq) in fit_gamm ---------------

# Disyllabic tokens on a syllable_tseq axis: syllable 1 spans [0, 1],
# syllable 2 spans [1, 2]. Tokens start and end at slightly different
# points: a _tseq made elsewhere. One from normalise_time_landmarks() starts
# at exactly 0 and ends at exactly n.
make_tseq_data <- function() {
  set.seed(7)
  grid <- expand.grid(speaker = c("s1", "s2"), tone = c("HL", "LH"),
                      item = c("w1", "w2"), rep = 1:2,
                      stringsAsFactors = FALSE)
  do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    g  <- grid[i, ]
    ts <- seq(runif(1, 0.02, 0.2), runif(1, 1.8, 1.98), length.out = 16)
    shape <- if (g$tone == "HL") ifelse(ts < 1, 10, 10 - 20 * (ts - 1))
             else ifelse(ts < 1, -10, -10 + 20 * (ts - 1))
    data.frame(token = paste(g$speaker, g$tone, g$item, g$rep, sep = "_"),
               syllable_tseq = ts, f0 = 100 + shape + rnorm(16, sd = 1),
               speaker = g$speaker, tone = g$tone, item = g$item,
               stringsAsFactors = FALSE)
  }))
}
fit_tseq <- function(df, time = "syllable_tseq", ...) {
  suppressWarnings(fit_gamm(df, time = time, k = 6, random_smooth = "none",
                            random_intercept_speaker = FALSE,
                            random_intercept_item = FALSE, ...))
}

test_that("fit_gamm keeps a <tier>_tseq axis on its own 0-to-n scale", {
  skip_if_not_installed("mgcv")
  df <- make_tseq_data()
  expect_message(fit <- fit_tseq(df), "sequential landmark axis")
  expect_equal(fit$time_axis, "sequential")
  expect_equal(fit$n_segments, 2L)
  expect_true(fit$time_prenormalised)
  expect_equal(fit$time_range, range(df$syllable_tseq))
  # Used as-is: no per-token rescale, so the boundary stays at 1 for all.
  dp <- fit$data_for_predict
  expect_equal(dp$.time_norm, dp$syllable_tseq)
})

test_that("predict_gamm predicts across the observed sequential range", {
  skip_if_not_installed("mgcv")
  df <- make_tseq_data()
  preds <- predict_gamm(suppressMessages(fit_tseq(df)), n = 50)
  per_tone <- split(preds$time, preds$tone)
  for (tt in per_tone) expect_equal(range(tt), range(df$syllable_tseq))
  expect_equal(attr(preds, "time_axis"), "sequential")
  expect_equal(attr(preds, "n_segments"), 2L)
  # The HL tone is level in syllable 1 and falls in syllable 2.
  hl <- preds[preds$tone == "HL", ]
  expect_gt(hl$f0_predicted[which.min(abs(hl$time - 0.5))],
            hl$f0_predicted[which.min(abs(hl$time - 1.8))] + 10)
})

test_that("time_normalised overrides the sequential detection both ways", {
  skip_if_not_installed("mgcv")
  df <- make_tseq_data()
  fit_no <- fit_tseq(df, time_normalised = "no")
  expect_equal(fit_no$time_axis, "unit")
  expect_equal(range(fit_no$data_for_predict$.time_norm), c(0, 1))
  df$word_axis <- df$syllable_tseq                 # not named *_tseq
  fit_seq <- fit_tseq(df, time = "word_axis", time_normalised = "sequential")
  expect_equal(fit_seq$time_axis, "sequential")
  expect_equal(fit_seq$n_segments, 2L)
})

test_that("a sequential axis with negative values is refused", {
  skip_if_not_installed("mgcv")
  df <- make_tseq_data(); df$syllable_tseq[1] <- -0.5
  expect_error(fit_tseq(df), "negative")
})

test_that("predict_gamm on a fit without time_range (older object) uses [0, 1]", {
  skip_if_not_installed("mgcv")
  df <- make_tseq_data()
  fit <- fit_tseq(df, time_normalised = "no")
  fit$time_range <- NULL; fit$time_axis <- NULL; fit$n_segments <- NULL
  preds <- predict_gamm(fit, n = 20)
  expect_equal(range(preds$time), c(0, 1))
  expect_equal(attr(preds, "time_axis"), "unit")
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

# ---------- 0-1 axes recognised by name ----------------------------------------

# Six tokens of 21 rows on a 0-1 axis, with rows without f0 at both edges
# (3-5 at the start, 4-5 at the end), so the f0 covers only the middle of
# each token's axis: a 0-1 column built over every row, such as time_prop or
# a token_t01 from shinytone 1.0.0. `axis` names the 0-1 column.
make_padded_t01 <- function(axis = "token_t01") {
  d <- do.call(rbind, lapply(1:6, function(i) {
    t01 <- seq(0, 1, length.out = 21)
    f0  <- 150 + 30 * t01 + i
    f0[seq_len(3 + i %% 3)]      <- NA
    f0[22 - seq_len(4 + i %% 2)] <- NA
    data.frame(token = paste0("tok", i), time = 0.05 * i + 0.25 * t01,
               t01 = t01, f0 = f0, tone = "T1", speaker = "s1")
  }))
  names(d)[names(d) == "t01"] <- axis
  d
}
axis_names <- c("token_t01", "time_prop", "syl_t01")

test_that("only <tier>_t01, token_t01 and time_prop are 0-1 axes by name", {
  expect_true(all(vapply(axis_names, t01_axis_name, logical(1))))
  for (nm in list("time", "syl_tseq", "token_t01_x", "t01", NULL, NA_character_,
                  c("token_t01", "time_prop"), 1)) {
    expect_false(t01_axis_name(nm), info = paste(format(nm), collapse = " "))
  }
})

test_that("a _t01 column with all rows kept behaves as at HEAD", {
  for (nm in axis_names) {
    d <- make_padded_t01(nm)
    # The value test alone (HEAD) already says TRUE when every row is kept.
    expect_true(time_already_normalised(d[[nm]], d$token))
    expect_true(time_already_normalised(d[[nm]], d$token, name = nm))
    d$other <- d[[nm]]                    # the same values under a plain name
    expect_identical(resolve_time_norm(d, nm, "token", quiet = TRUE),
                     resolve_time_norm(d, "other", "token", quiet = TRUE))
    expect_equal(suppressMessages(compute_mean_contour(d, time = nm)),
                 suppressMessages(compute_mean_contour(d, time = "other")))
  }
})

test_that("after removing rows without f0, the decision does not flip", {
  for (nm in axis_names) {
    d <- make_padded_t01(nm)
    r <- d[!is.na(d$f0), ]
    spans <- tapply(r[[nm]], r$token, function(x) diff(range(x)))
    expect_lt(stats::median(spans), 0.9)
    # Without the name the value test flips; with it the decision holds.
    expect_false(time_already_normalised(r[[nm]], r$token))
    expect_true(time_already_normalised(r[[nm]], r$token, name = nm))
    expect_message(res <- resolve_time_norm(r, nm, "token"), "by its name")
    expect_true(res$prenormalised)
    expect_equal(res$time_norm, r[[nm]])          # positions kept, not stretched
    # So the mean contour is the same with and without those rows.
    expect_equal(suppressMessages(compute_mean_contour(r, time = nm)),
                 suppressMessages(compute_mean_contour(d, time = nm)))
    co <- suppressMessages(fit_polynomial(r, f0 = "f0", token = "token",
                                          time = nm, speaker = "speaker",
                                          tone = "tone", degree = 1))
    expect_true(isTRUE(attr(co, "time_prenormalised")))
    # "no" still forces the per-token rescale.
    expect_false(resolve_time_norm(r, nm, "token", "no")$prenormalised)
  }
})

test_that("the name counts only when every value lies in [0, 1]", {
  d <- make_padded_t01()
  expect_false(time_already_normalised(d$token_t01 * 100, d$token, name = "token_t01"))
  expect_false(time_already_normalised(c(0, 0.5, 1.4), "a", name = "time_prop"))
  expect_false(time_already_normalised(c(-0.2, 0.5, 1), "a", name = "syl_t01"))
  expect_false(time_already_normalised(rep(NA_real_, 3), "a", name = "token_t01"))
  # NA outside a tier's intervals is no obstacle.
  expect_true(time_already_normalised(c(NA, 0.4, 0.6, NA), "a", name = "syl_t01"))
  df <- data.frame(token = "a", time_prop = c(0.1, 1.3))
  expect_false(resolve_time_norm(df, "time_prop", "token")$prenormalised)
})

test_that("a multi-segment _tseq is unchanged: still rescaled per token in auto mode", {
  df  <- make_tseq_data()                     # syllable_tseq runs to about 2
  res <- resolve_time_norm(df, "syllable_tseq", "token", "auto")
  expect_false(res$prenormalised)
  expect_identical(res, resolve_time_norm(df, "syllable_tseq", "token", "no"))
  expect_equal(as.vector(tapply(res$time_norm, df$token, min)), rep(0, 16))
  expect_equal(as.vector(tapply(res$time_norm, df$token, max)), rep(1, 16))
  # After rows are removed it is still rescaled, never clamped to [0, 1].
  r    <- df[ave(df$syllable_tseq, df$token, FUN = seq_along) > 3, ]
  res2 <- resolve_time_norm(r, "syllable_tseq", "token", "auto")
  expect_false(res2$prenormalised)
  expect_identical(res2, resolve_time_norm(r, "syllable_tseq", "token", "no"))
  expect_error(resolve_time_norm(df, "syllable_tseq", "token", "yes"),
               "outside \\[0, 1\\]")
})

test_that("a one-segment _tseq is judged by its values, as before", {
  d <- make_padded_t01("syl_tseq")
  r <- d[!is.na(d$f0), ]
  for (x in list(d, r)) {
    expect_identical(time_already_normalised(x$syl_tseq, x$token, name = "syl_tseq"),
                     time_already_normalised(x$syl_tseq, x$token))
  }
  expect_true(resolve_time_norm(d, "syl_tseq", "token", quiet = TRUE)$prenormalised)
  expect_false(resolve_time_norm(r, "syl_tseq", "token")$prenormalised)
})

test_that("token_t01 is the same built before or after removal on f0_long.csv", {
  path <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(path), "test/f0_long.csv not found")
  d <- utils::read.csv(path, encoding = "UTF-8")
  a <- normalise_time_token(d, "time", "token", f0 = "f0")
  has <- is.finite(d$f0) & d$f0 > 0
  r <- d[has, ]
  b <- normalise_time_token(r, "time", "token", f0 = "f0")
  expect_equal(nrow(r), 52431L)
  expect_identical(a$token_t01[has], b$token_t01)
  # 14,023 rows before each token's first f0 and 16,584 after its last
  expect_equal(sum(is.na(a$token_t01)), 30607L)
  expect_equal(sum(!has & is.finite(a$token_t01)), 263L)
  expect_true(all(tapply(b$token_t01, b$token, min) == 0))
  expect_true(all(tapply(b$token_t01, b$token, max) == 1))
  # So the value test alone says 0-1 too, with the rows kept or removed
  expect_true(time_already_normalised(a$token_t01, a$token))
  expect_true(time_already_normalised(b$token_t01, b$token))
  res <- resolve_time_norm(b, "token_t01", "token", quiet = TRUE)
  expect_true(res$prenormalised)
  expect_identical(res$time_norm, b$token_t01)
  # The fits' own rescale of the time column gives the same axis
  expect_identical(resolve_time_norm(d, "time", "token", f0 = "f0")$time_norm, a$token_t01)
  expect_identical(resolve_time_norm(r, "time", "token", f0 = "f0")$time_norm, b$token_t01)
})

test_that("resolve_time_norm(f0 =) runs from the first f0 to the last", {
  d <- make_padded_t01()
  k <- !is.na(d$f0)
  res <- resolve_time_norm(d, "time", "token", f0 = "f0")
  expect_false(res$prenormalised)
  tn <- res$time_norm
  expect_equal(as.vector(tapply(tn[k], d$token[k], min)), rep(0, 6))
  expect_equal(as.vector(tapply(tn[k], d$token[k], max)), rep(1, 6))
  expect_true(all(is.na(tn[!k])))                 # all rows without f0 are at the edges
  # Removing the rows without f0 changes nothing, and it is token_t01
  r <- d[k, ]
  expect_identical(resolve_time_norm(r, "time", "token", f0 = "f0")$time_norm, tn[k])
  expect_identical(tn, normalise_time_token(d, "time", "token", f0 = "f0")$token_t01)
  # Without f0, the old first-row-to-last-row rescale
  old <- resolve_time_norm(d, "time", "token")$time_norm
  expect_equal(old, as.numeric(ave(d$time, d$token, FUN = function(x) (x - min(x)) / diff(range(x)))))
  expect_false(isTRUE(all.equal(old[k], tn[k])))
  # A token with f0 at one time keeps 0.5 there; a token with no f0 is NA
  deg <- data.frame(token = rep(c("one", "none", "ok"), each = 3),
                    time  = rep(c(0.1, 0.2, 0.3), 3),
                    f0    = c(NA, 200, NA, NA, NA, NA, 200, 210, 220))
  expect_equal(resolve_time_norm(deg, "time", "token", "no", f0 = "f0")$time_norm,
               c(NA, 0.5, NA, NA, NA, NA, 0, 0.5, 1))
  # A missing f0 column is an error
  expect_error(resolve_time_norm(deg, "time", "token", f0 = "f0_corrected"),
               "Column(s) not found in data: f0_corrected", fixed = TRUE)
  # In a signed column 0 is f0; in Hz it is not
  sg <- data.frame(token = "a", time = 1:5 / 10, f0_st = c(0, -1, 0.5, 2, 0),
                   f0 = c(0, 150, 160, 170, 0))
  expect_equal(resolve_time_norm(sg, "time", "token", "no", f0 = "f0_st")$time_norm,
               c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(resolve_time_norm(sg, "time", "token", "no", f0 = "f0")$time_norm,
               c(NA, 0, 0.5, 1, NA))
  # A column used as it is is not touched by f0
  expect_identical(resolve_time_norm(d, "token_t01", "token", quiet = TRUE, f0 = "f0"),
                   resolve_time_norm(d, "token_t01", "token", quiet = TRUE))
  # A missing time or token column is an error too
  expect_error(resolve_time_norm(deg, "tim", "token", f0 = "f0"),
               "Column(s) not found in data: tim", fixed = TRUE)
  expect_error(resolve_time_norm(deg, "time", "tok", f0 = "f0"),
               "Column(s) not found in data: tok", fixed = TRUE)
  expect_error(resolve_time_norm(deg, "time", "tok"),
               "Column(s) not found in data: tok", fixed = TRUE)
  # -1 markers are f0: a column with them is not read as Hz (as documented)
  neg <- data.frame(token = "a", time = 0:5 / 100, f0 = c(-1, 210, 215, 220, 225, -1))
  expect_equal(resolve_time_norm(neg, "time", "token", f0 = "f0")$time_norm, 0:5 / 5)
  neg$f0[neg$f0 < 0] <- NA
  expect_equal(resolve_time_norm(neg, "time", "token", f0 = "f0")$time_norm,
               c(NA, 0, 1/3, 2/3, 1, NA))
})

test_that("resolve_time_norm: a token with one time gets 0.5 on every row without f0", {
  # As documented: rows with no time included; other NA times stay NA.
  dd <- data.frame(token = c("z", "z", "z", "y", "y", "y"),
                   time = c(0.1, 0.1, NA, 0.1, NA, 0.3),
                   f0 = c(200, 210, 220, 200, 210, 220))
  expect_equal(resolve_time_norm(dd, "time", "token")$time_norm,
               c(0.5, 0.5, 0.5, 0, NA, 1))
  expect_equal(resolve_time_norm(dd, "time", "token", f0 = "f0")$time_norm,
               c(0.5, 0.5, NA, 0, NA, 1))
})

test_that("a token whose first syllable has no f0 is stretched on a rescaled _tseq", {
  # As resolve_time_norm(), compute_mean_contour() and the fits document.
  mk <- function(tok, s1_f0 = TRUE) {
    t  <- seq(0, 0.40, by = 0.01)
    s1 <- t < 0.20
    f0 <- ifelse(s1, 250 - 100 * (t / 0.17), 240)
    f0[t > 0.17 & t < 0.23] <- NA
    if (!s1_f0) f0[s1] <- NA
    data.frame(token = tok, time = t, f0 = f0,
               syllable_start = ifelse(s1, 0, 0.20),
               syllable_end = ifelse(s1, 0.20, 0.41),
               syllable_i = ifelse(s1, 1L, 2L))
  }
  d  <- normalise_time_landmarks(rbind(mk("w1"), mk("w3", FALSE)), "time", "syllable")
  tn <- resolve_time_norm(d, "syllable_tseq", "token", f0 = "f0")$time_norm
  s2 <- d$syllable_i == 2 & !is.na(d$f0)
  expect_equal(min(tn[s2 & d$token == "w1"]), 0.5)
  expect_equal(min(tn[s2 & d$token == "w3"]), 0)
  # fit_gamm keeps the axis on its own scale, so there w3 stays at 1 to 2
  expect_equal(range(d$syllable_tseq[s2 & d$token == "w3"]), c(1, 2))
})

test_that("interp_no_blend steps between segments and averages within one", {
  ib <- shinytone:::interp_no_blend
  # No ties: approx(), bit for bit
  set.seed(2)
  tt <- sort(runif(50)); yy <- rnorm(50)
  xo <- seq(-0.1, 1.1, length.out = 40)
  expect_identical(ib(tt, yy, xo), stats::approx(tt, yy, xout = xo)$y)
  expect_identical(ib(tt, yy, xo, rule = 2), stats::approx(tt, yy, xout = xo, rule = 2)$y)
  # Ties without segments are averaged, as approx(ties = mean)
  t2 <- c(0, 0.5, 0.5, 1); y2 <- c(10, 20, 40, 10)
  expect_equal(ib(t2, y2, c(0.25, 0.5, 0.75)),
               stats::approx(t2, y2, xout = c(0.25, 0.5, 0.75), ties = mean)$y)
  # Ties across segments step: the later segment at the time and after it,
  # the earlier one before it, in any row order
  t3 <- c(0, 1, 1, 2); y3 <- c(250, 150, 240, 240); s3 <- c(0, 0, 1, 1)
  for (o in list(1:4, 4:1, c(3, 1, 4, 2))) {
    expect_equal(ib(t3[o], y3[o], c(0.5, 0.999, 1, 1.5), seg = s3[o]),
                 c(200, 150.1, 240, 240), tolerance = 1e-6)
  }
  # Rows of one segment at one time are averaged, then stepped across
  t4 <- c(0, 1, 1, 1, 1, 2); y4 <- c(250, 150, 160, 240, 230, 240)
  s4 <- c(0, 0, 0, 1, 1, 1)
  expect_equal(ib(t4, y4, c(1 - 1e-9, 1), seg = s4), c(155, 235), tolerance = 1e-6)
})

test_that("stack_segments reads the segment from _t01, then _i, then the row order", {
  ss <- shinytone:::stack_segments
  d <- data.frame(token = "w", syllable_tseq = c(0, 0.5, 1, 1, 1.5, 2),
                  syllable_t01 = c(0, 0.5, 1, 0, 0.5, 1),
                  syllable_i = c(1, 1, 1, 2, 2, 2))
  expect_equal(ss(d, "syllable_tseq", "token"), c(0, 0, 0, 1, 1, 1))
  expect_equal(ss(d[6:1, ], "syllable_tseq", "token"), c(1, 1, 1, 0, 0, 0))
  expect_equal(ss(d[, -3], "syllable_tseq", "token"), c(0, 0, 0, 1, 1, 1))
  # Neither column: the last row at an inner whole number starts the next
  # segment; the ends of the range are not boundaries
  expect_equal(ss(d[, 1:2], "syllable_tseq", "token"), c(0, 0, 0, 1, 1, 2))
  expect_null(ss(d, "time", "token"))
  expect_null(ss(d, "syllable_t01", "token"))
})

test_that("stack_segments tells apart the rows at a boundary on a _tbound", {
  ss <- shinytone:::stack_segments
  # Two intervals that do not touch: syllable 1 [0.06, 0.18] and syllable 2
  # [0.24, 0.40], with a row on each end of the pause, so both sit at 1.
  d <- data.frame(token = "w", time = c(0.10, 0.14, 0.18, 0.24, 0.28, 0.32),
                  f0 = c(250, 200, 150, 240, 240, 240),
                  syllable_start = rep(c(0.06, 0.24), each = 3),
                  syllable_end = rep(c(0.18, 0.40), each = 3),
                  syllable_i = rep(1:2, each = 3))
  d <- normalise_time_landmarks(d, "time", "syllable")
  expect_equal(d$syllable_tbound, c(0, 0.5, 1, 1, 1.5, 2))
  expect_equal(ss(d, "syllable_tbound", "token"), c(0, 0, 0, 1, 1, 1))
  expect_equal(ss(d[6:1, ], "syllable_tbound", "token"), c(1, 1, 1, 0, 0, 0))
  # Read from _i, not from _tbound - _t01, which is no whole number here:
  # syllable 1's last f0 (0.12 s) is well before its end boundary (0.18 s),
  # so the difference does not even round to 0
  e <- d
  e$time[e$time == 0.14] <- 0.12
  e$f0[e$time == 0.18] <- NA
  e <- normalise_time_landmarks(e, "time", "syllable")
  expect_equal(e$syllable_tbound[e$time == 0.12] - e$syllable_t01[e$time == 0.12], -0.75)
  expect_equal(ss(e, "syllable_tbound", "token"), c(0, 0, 0, 1, 1, 1))
  # Without _i, the row order: the last row at 1 starts syllable 2
  bare <- d[c("token", "syllable_tbound")]
  expect_equal(ss(bare, "syllable_tbound", "token"), c(0, 0, 0, 1, 1, 2))
  # So the curve steps at 1 rather than averaging 150 and 240 there
  ib <- shinytone:::interp_no_blend
  for (o in list(1:6, 6:1)) {
    expect_equal(ib(d$syllable_tbound[o], d$f0[o], c(0.999, 1, 1.25),
                    seg = ss(d[o, ], "syllable_tbound", "token")),
                 c(150.1, 240, 240), tolerance = 1e-6)
  }
})

test_that("a _tbound is rescaled per token, from its first f0 to its last", {
  mk <- function(tok, b, s2_first) {
    t  <- seq(0, 0.40, by = 0.01)
    s1 <- t < b
    f0 <- ifelse(s1, 250 - 100 * t, 240)
    f0[t > 0.17 & t < s2_first - 1e-9] <- NA
    data.frame(token = tok, time = t, f0 = f0,
               syllable_start = ifelse(s1, 0, b), syllable_end = ifelse(s1, b, 0.41),
               syllable_i = ifelse(s1, 1L, 2L))
  }
  d <- normalise_time_landmarks(rbind(mk("w1", 0.20, 0.23), mk("w2", 0.25, 0.30)),
                                "time", "syllable")
  # 0 to 2, so not a 0-1 axis by value or by name
  expect_false(time_already_normalised(d$syllable_tbound, d$token, name = "syllable_tbound"))
  r <- resolve_time_norm(d, "syllable_tbound", "token", f0 = "f0")
  expect_false(r$prenormalised)
  expect_equal(r$time_norm, d$syllable_tbound / 2)
  # The boundary sits at 0.5 in both tokens, each with its own gap around it
  at_b <- (d$token == "w1" & d$time == 0.20) | (d$token == "w2" & d$time == 0.25)
  expect_equal(r$time_norm[at_b], c(0.5, 0.5))
})

# ---------- the app's notes and exported scripts agree with the fit ----------

test_that("every app call to time_already_normalised() passes the column name", {
  # resolve_time_norm() decides with the name, so a note or an exported
  # script that leaves it out can take the other branch once rows without
  # f0 are removed.
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  files <- c(file.path(app, "global.R"),
             list.files(file.path(app, "ui"), "\\.R$", full.names = TRUE))
  n <- 0L
  for (f in files) {
    pd  <- utils::getParseData(parse(f, keep.source = TRUE, encoding = "UTF-8"))
    fn  <- pd$id[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "time_already_normalised"]
    for (call in pd$parent[match(pd$parent[match(fn, pd$id)], pd$id)]) {
      n <- n + 1L
      args <- pd$text[pd$parent == call & pd$token == "SYMBOL_SUB"]
      expect_true("name" %in% args,
                  info = paste0(basename(f), ": ", utils::getParseText(pd, call)))
    }
  }
  expect_gte(n, 7L)   # global.R 1; Model, GCA and GAMM 2 each
})

# global.R's definitions and one module, with shiny's exports, bound to its
# data for shiny::testServer().
time_module <- function(file, name, data) {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  pkgs <- new.env(parent = globalenv())
  for (nm in getNamespaceExports("shiny")) assign(nm, getExportedValue("shiny", nm), envir = pkgs)
  env  <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE,
                               encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  sys.source(file.path(app, "ui", file), env, keep.source = FALSE)
  bound <- function(input, output, session) NULL
  body(bound) <- body(env[[name]])
  environment(bound) <- list2env(list(
    dataset = shiny::reactiveVal(data), normalised_data = shiny::reactiveVal(NULL),
    gca_pred_data = shiny::reactiveVal(NULL), gamm_pred_data = shiny::reactiveVal(NULL),
    curated_data = NULL, cluster_data = NULL, curated_norm_data = NULL), parent = env)
  bound
}

test_that("Model, GCA and GAMM notes and scripts take the fit's branch after removal", {
  skip_if_not_installed("shiny")
  d <- make_padded_t01()
  d$item <- d$token
  r <- d[!is.na(d$f0), ]
  r$other <- r$token_t01                  # the same values under a plain name
  html <- function(x) paste(as.character(x$html), collapse = "")
  # The fit's own decision on these data.
  expect_true(resolve_time_norm(r, "token_t01", "token", quiet = TRUE)$prenormalised)
  expect_false(resolve_time_norm(r, "other", "token")$prenormalised)

  shiny::testServer(time_module("model_ui.R", "model_ui", r), {
    session$setInputs(model_token_var = "token", model_f0_var = "f0",
                      model_time_var = "token_t01", model_speaker_var = "speaker",
                      model_tone_var = "tone", model_degree = "2", model_show_code = 1)
    expect_match(html(output$model_timenorm_note), "its name marks a 0&ndash;1 axis")
    expect_match(html(output$model_r_code), "t_norm &lt;- 2 * t_raw - 1", fixed = TRUE)
    session$setInputs(model_time_var = "other")
    expect_no_match(html(output$model_r_code), "t_norm &lt;- 2 * t_raw - 1", fixed = TRUE)
  })

  shiny::testServer(time_module("gca_ui.R", "gca_ui", r), {
    session$setInputs(gca_token_var = "token", gca_f0_var = "f0",
                      gca_time_var = "token_t01", gca_speaker_var = "speaker",
                      gca_tone_var = "tone", gca_item_var = "item", gca_degree = "2",
                      gca_ri_speaker = TRUE, gca_ri_item = FALSE,
                      gca_rs_speaker = FALSE, gca_rs_item = FALSE, gca_show_code = 1)
    expect_match(html(output$gca_timenorm_note), "its name marks a 0&ndash;1 axis")
    expect_match(html(output$gca_r_code), "pmin(pmax(dat$token_t01, 0), 1)", fixed = TRUE)
    session$setInputs(gca_time_var = "other")
    expect_no_match(html(output$gca_r_code), "pmin(pmax(dat$other, 0), 1)", fixed = TRUE)
  })

  shiny::testServer(time_module("gamm_ui.R", "gamm_ui", r), {
    session$setInputs(gamm_token_var = "token", gamm_f0_var = "f0",
                      gamm_time_var = "token_t01", gamm_speaker_var = "speaker",
                      gamm_tone_var = "tone", gamm_item_var = "item",
                      gamm_dur_var = "none", gamm_k = "4", gamm_bs = "tp",
                      gamm_smooth_type = "separate", gamm_rs_type = "none",
                      gamm_ri_speaker = TRUE, gamm_ri_item = FALSE, gamm_ar1 = FALSE,
                      gamm_show_code = 1)
    expect_match(html(output$gamm_timenorm_note), "its name marks a 0&ndash;1 axis")
    expect_match(html(output$gamm_r_code), "pmin(pmax(dat$token_t01, 0), 1)", fixed = TRUE)
    session$setInputs(gamm_time_var = "other")
    expect_no_match(html(output$gamm_r_code), "pmin(pmax(dat$other, 0), 1)", fixed = TRUE)
    expect_null(output$gamm_timenorm_note)
  })
})
