# sort_tones() and the tone order of compute_mean_contour(), fit_gca() and
# fit_gamm(). Chinese characters as \u escapes, so the tests read the same in
# any locale.

zh <- c(yinping   = "\u9634\u5e73", yangping = "\u9633\u5e73",
        shang     = "\u4e0a\u58f0", qu       = "\u53bb\u58f0",
        ru        = "\u5165\u58f0")
mandarin <- unname(zh[c("yinping", "yangping", "shang", "qu")])

# The order the natural sort in the Summarise plot gave before sort_tones().
old_natural <- function(x) {
  u <- unique(as.character(x[!is.na(x)]))
  key <- tolower(u)
  m <- gregexpr("[0-9]+", key)
  regmatches(key, m) <- lapply(regmatches(key, m), function(d)
    formatC(as.numeric(d), width = 12, format = "d", flag = "0"))
  u[order(key)]
}

test_that("sort_tones() puts Mandarin tone names in traditional order", {
  for (x in list(rev(mandarin), mandarin[c(3, 1, 4, 2)], sort(mandarin))) {
    expect_identical(sort_tones(x), mandarin)
    expect_identical(sort_tones(x, natural = FALSE), mandarin)
  }
  expect_identical(sort_tones(rep(rev(mandarin), 3)), mandarin)   # long format
  expect_identical(sort_tones(c(zh[["ru"]], rev(mandarin))), c(mandarin, zh[["ru"]]))
})

test_that("sort_tones() knows traditional characters and yin/yang splits", {
  trad <- c("\u9670\u5e73", "\u967d\u5e73", "\u4e0a\u8072", "\u53bb\u8072", "\u5165\u8072")
  expect_identical(sort_tones(rev(trad)), trad)
  # Cantonese: yin before yang within ping, shang, qu, ru; zhong ru between
  splits <- c("\u9634\u5e73", "\u9633\u5e73", "\u9634\u4e0a", "\u9633\u4e0a",
              "\u9634\u53bb", "\u9633\u53bb", "\u9634\u5165", "\u4e2d\u5165", "\u9633\u5165")
  expect_identical(sort_tones(rev(splits)), splits)
  expect_identical(sort_tones(c("\u9633\u5165", "\u4e0b\u9634\u5165", "\u4e0a\u9634\u5165")),
                   c("\u4e0a\u9634\u5165", "\u4e0b\u9634\u5165", "\u9633\u5165"))
  # Optional sheng / diao, either script; space around a label is ignored
  expect_identical(sort_tones(c("\u53bb", "\u9633\u5e73\u8c03", " \u9634\u5e73 ")),
                   c(" \u9634\u5e73 ", "\u9633\u5e73\u8c03", "\u53bb"))
})

test_that("sort_tones() puts other labels after the tone names, in natural order", {
  qing <- "\u8f7b\u58f0"                                     # neutral tone: not matched
  zhongping <- "\u4e2d\u5e73"                               # mid level: not matched
  expect_identical(sort_tones(c(qing, "T2", rev(mandarin))), c(mandarin, sort_tones(c(qing, "T2"))))
  expect_true(is.na(shinytone:::zh_tone_rank(zhongping)))
  expect_true(is.na(shinytone:::zh_tone_rank("\u4e0a\u4e0a")))
  expect_identical(sort_tones(c("T10", "T2", "t1", NA)), c("t1", "T2", "T10"))
})

test_that("sort_tones() leaves Latin labels in their old order", {
  sets <- list(c("T3", "T1", "T4", "T2"), c("T10", "T2", "T1"), c("tone2", "tone1", "Tone3"),
               c("55", "35", "214", "51"), c("H", "l", "M", "LH"), c(3L, 1L, 10L, 2L),
               c(2.5, 1, 10), c("b", NA, "a"))
  for (x in sets) {
    expect_identical(sort_tones(x), old_natural(x))
    expect_identical(sort_tones(x, natural = FALSE), levels(as.factor(x)))
  }
  expect_identical(sort_tones(character(0)), character(0))
  expect_identical(sort_tones(NA), character(0))
})

test_that("sort_tones() keeps a factor's own levels", {
  f <- factor(mandarin, levels = rev(mandarin))
  expect_identical(sort_tones(f), rev(mandarin))
  expect_identical(sort_tones(factor(c("T2", "T1"), levels = c("T2", "T1", "T3"))),
                   c("T2", "T1", "T3"))
})

# ---------- compute_mean_contour ------------------------------------------------

mean_contour_data <- function(tones) {
  data.frame(token = rep(paste0("tok", seq_along(tones)), each = 5),
             time  = rep(seq(0, 1, length.out = 5), length(tones)),
             f0    = rep(seq_along(tones), each = 5) * 10 + 100,
             tone  = rep(tones, each = 5), stringsAsFactors = FALSE)
}

test_that("compute_mean_contour() lists Chinese tone names in traditional order", {
  out <- compute_mean_contour(mean_contour_data(rev(mandarin)), time_normalised = "no")
  expect_identical(unique(out$tone), mandarin)
  chao <- contour_to_chao(out)
  expect_identical(chao$tone, mandarin)
})

test_that("compute_mean_contour() keeps the old order for Latin labels", {
  tones <- c("T2", "T10", "a", "B", "T1")
  out <- compute_mean_contour(mean_contour_data(tones), time_normalised = "no")
  expect_identical(unique(out$tone), c("B", "T1", "T10", "T2", "a"))   # C locale
})

test_that("compute_mean_contour() follows a factor's levels", {
  d <- mean_contour_data(c("T1", "T2", "T3"))
  d$tone <- factor(d$tone, levels = c("T3", "T1", "T2"))
  expect_identical(unique(compute_mean_contour(d, time_normalised = "no")$tone), c("T3", "T1", "T2"))
})

# ---------- model fits ----------------------------------------------------------

fit_data <- function(tones) {
  set.seed(1)
  g <- expand.grid(speaker = paste0("S", 1:4), tone = tones, item = paste0("w", 1:4),
                   rep = 1:2, stringsAsFactors = FALSE)
  g$token <- paste(g$speaker, match(g$tone, tones), g$item, g$rep, sep = "_")
  do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    t <- seq(0, 1, length.out = 10)
    data.frame(speaker = g$speaker[i], tone = g$tone[i], item = g$item[i],
               token = g$token[i], time = t,
               f0 = match(g$tone[i], tones) * 30 + 100 + 20 * t + stats::rnorm(10, sd = 5),
               stringsAsFactors = FALSE)
  }))
}

test_that("fit_gca() and predict_gca() use the traditional tone order", {
  skip_if_not_installed("lme4")
  fit <- suppressMessages(suppressWarnings(
    fit_gca(fit_data(rev(mandarin)), degree = 2,
            random_slope_speaker = FALSE, random_slope_item = FALSE)))
  expect_identical(levels(fit$model@frame$.tone), mandarin)       # reference: yinping
  expect_identical(unique(predict_gca(fit, n = 5)$tone), mandarin)
})

test_that("fit_gca() keeps the old tone levels for Latin labels", {
  skip_if_not_installed("lme4")
  d <- fit_data(c("T2", "T1", "T3"))
  fit <- suppressMessages(suppressWarnings(
    fit_gca(d, degree = 1, random_slope_speaker = FALSE, random_slope_item = FALSE)))
  expect_identical(levels(fit$model@frame$.tone), levels(as.factor(d$tone)))
})

test_that("fit_gamm() and predict_gamm() use the traditional tone order", {
  skip_if_not_installed("mgcv")
  d <- fit_data(rev(mandarin))
  for (st in c("separate", "difference")) {
    fit <- suppressMessages(suppressWarnings(
      fit_gamm(d, k = 4, smooth_type = st, random_smooth = "none", use_ar1 = FALSE)))
    expect_identical(unique(predict_gamm(fit, n = 5)$tone), mandarin)
  }
})
