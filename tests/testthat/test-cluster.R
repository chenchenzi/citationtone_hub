# Tests for f0-contour clustering (cluster_features / choose_k_f0 /
# cluster_f0 / cluster_agreement).

# Synthetic data: three clearly separable contour shapes (rising / falling /
# level), several tokens each, so a correct clustering recovers the shapes.
make_synth <- function(n_per = 15, np = 20, seed = 1) {
  set.seed(seed)
  shapes <- list(
    rising  = function(t) 2 * t,
    falling = function(t) 2 * (1 - t),
    level   = function(t) rep(1, length(t))
  )
  out <- list(); tok <- 0L
  for (tone in names(shapes)) {
    for (j in seq_len(n_per)) {
      tok <- tok + 1L
      t <- seq(0, 1, length.out = np)
      out[[length(out) + 1L]] <- data.frame(
        token = sprintf("tk%03d", tok), time = t,
        f0 = shapes[[tone]](t) + stats::rnorm(np, 0, 0.08),
        tone = tone, speaker = "s1", stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

test_that("cluster_features returns the expected shapes", {
  d <- make_synth(n_per = 12, np = 20)
  f <- cluster_features(d, f0 = "f0", token = "token", time = "time",
                        speaker = "speaker", tone = "tone",
                        n_points = 20, features = "points", register = "level")
  expect_equal(length(f$tokens), 36)
  expect_equal(dim(f$contours), c(36, 20))
  expect_equal(nrow(f$features), 36)
  expect_true(all(is.finite(f$features)))
  expect_true(all(c("token", "speaker", "tone") %in% names(f$meta)))
})

test_that("cluster_features resamples to 21 points by default", {
  # 13 frames per token, resampled to the default 21 (one every 5%).
  d <- make_synth(n_per = 4, np = 13)
  f <- cluster_features(d, f0 = "f0", features = "points")
  expect_equal(ncol(f$contours), 21)
  expect_equal(f$n_points, 21L)
})

test_that("legendre / dct feature spaces have the right width", {
  d <- make_synth(n_per = 8)
  fl <- cluster_features(d, f0 = "f0", n_points = 20, features = "legendre", degree = 4)
  fd <- cluster_features(d, f0 = "f0", n_points = 20, features = "dct", degree = 3)
  fv <- cluster_features(d, f0 = "f0", n_points = 20, features = "derivative")
  expect_equal(ncol(fl$features), 5)   # c0..c4
  expect_equal(ncol(fd$features), 4)   # d0..d3
  expect_equal(ncol(fv$features), 19)  # n_points - 1 first differences
  expect_true(all(is.finite(fl$features)))
  expect_true(all(is.finite(fd$features)))
  expect_true(all(is.finite(fv$features)))
})

test_that("derivative features separate rising / falling / level by movement", {
  d <- make_synth(n_per = 15, np = 20)
  f <- cluster_features(d, f0 = "f0", n_points = 20, features = "derivative", tone = "tone")
  cl <- cluster_f0(f, method = "kmeans", k = 3)
  ag <- cluster_agreement(cl$assignment[f$tokens], f$meta$tone)
  expect_gt(ag$ari, 0.9)
})

test_that("kmeans and hclust recover the three shapes (high ARI)", {
  d <- make_synth(n_per = 15, np = 20)
  f <- cluster_features(d, f0 = "f0", n_points = 20, features = "points", tone = "tone")
  for (m in c("kmeans", "hclust")) {
    cl <- cluster_f0(f, method = m, k = 3)
    expect_length(cl$sizes, 3)
    expect_equal(sum(cl$sizes), length(f$tokens))
    ag <- cluster_agreement(cl$assignment[f$tokens], f$meta$tone)
    expect_gt(ag$ari, 0.9)
  }
})

test_that("hclust returns the fitted merge tree; other methods return NULL", {
  d <- make_synth(n_per = 12, np = 20)
  f <- cluster_features(d, f0 = "f0", n_points = 20, features = "points", tone = "tone")
  ch <- cluster_f0(f, method = "hclust", k = 3)
  expect_s3_class(ch$tree, "hclust")
  expect_equal(length(ch$tree$order), length(f$tokens))   # one leaf per token
  ck <- cluster_f0(f, method = "kmeans", k = 3)
  expect_null(ck$tree)
})

test_that("hclust linkage methods all run and yield k groups", {
  d <- make_synth(n_per = 12, np = 20)
  f <- cluster_features(d, f0 = "f0", n_points = 20, features = "points", tone = "tone")
  for (lk in c("ward.D2", "complete", "average", "single")) {
    cl <- cluster_f0(f, method = "hclust", k = 4, hclust_method = lk)
    expect_s3_class(cl$tree, "hclust")
    expect_equal(cl$tree$method, lk)
    expect_length(cl$sizes, 4)
    expect_equal(sum(cl$sizes), length(f$tokens))
  }
})

test_that("gmm clustering works when mclust is available", {
  testthat::skip_if_not_installed("mclust")
  d <- make_synth(n_per = 15, np = 20)
  f <- cluster_features(d, f0 = "f0", n_points = 16, features = "points")
  cl <- cluster_f0(f, method = "gmm", k = 3)
  expect_length(cl$sizes, 3)
  expect_equal(sum(cl$sizes), length(f$tokens))
})

test_that("cluster means are ordered low -> high and sized k x n_points", {
  d <- make_synth(n_per = 12)
  f <- cluster_features(d, f0 = "f0", n_points = 16, features = "points")
  cl <- cluster_f0(f, method = "kmeans", k = 3)
  expect_equal(dim(cl$cluster_means), c(3, 16))
  expect_true(all(diff(rowMeans(cl$cluster_means)) >= 0))  # non-decreasing height
})

test_that("choose_k_f0 returns a table over the requested k range", {
  d <- make_synth(n_per = 12)
  f <- cluster_features(d, f0 = "f0", n_points = 16, features = "points")
  ck <- choose_k_f0(f, k_range = 2:6, gap = FALSE)
  expect_equal(ck$table$k, 2:6)
  expect_true(all(diff(ck$table$wss) <= 0))          # wss decreases with k
  if (requireNamespace("cluster", quietly = TRUE)) {
    expect_true(all(is.finite(ck$table$silhouette)))
    expect_equal(ck$k_silhouette, 3)                 # 3 well-separated shapes
  }
})

test_that("MDL information cost is reported and responds to the bending factor", {
  d <- make_synth(n_per = 20, np = 20)
  f <- cluster_features(d, f0 = "f0", n_points = 20, features = "points")
  ck <- choose_k_f0(f, k_range = 2:6, gap = FALSE, bending = 5)
  expect_true("mdl" %in% names(ck$table))
  expect_true(is.finite(ck$k_mdl))
  expect_length(ck$assignments, length(2:6))
  # cluster_mdl() reproduces the table's mdl at the same bending
  m <- cluster_mdl(f$contours, ck$assignments, bending = 5)
  expect_equal(unname(m), ck$table$mdl, tolerance = 1e-6)
  # higher bending never prefers more clusters
  lo <- choose_k_f0(f, k_range = 2:6, gap = FALSE, bending = 1)$k_mdl
  hi <- choose_k_f0(f, k_range = 2:6, gap = FALSE, bending = 60)$k_mdl
  expect_lte(hi, lo)
})

test_that("cluster_agreement: perfect = 1, random ~ 0", {
  lab <- rep(c("a", "b", "c"), each = 20)
  expect_equal(cluster_agreement(lab, lab)$ari, 1)
  set.seed(7)
  rand <- sample(1:3, length(lab), replace = TRUE)
  expect_lt(abs(cluster_agreement(rand, lab)$ari), 0.2)
  ag <- cluster_agreement(c(1, 1, 2, 2), c("x", "x", "y", "y"))
  expect_equal(dim(ag$table), c(2, 2))
})

test_that("input validation errors are raised", {
  d <- make_synth(n_per = 5)
  expect_error(cluster_features(d, f0 = "nope"), "not found")
  expect_error(cluster_features(d, f0 = "f0", n_points = 2), "at least 4")
  expect_error(cluster_features(d, f0 = "f0", n_points = 6, features = "legendre", degree = 10),
               "too high")
  f <- cluster_features(d, f0 = "f0", n_points = 16, features = "points")
  expect_error(cluster_f0(f, method = "kmeans", k = 1), "between 2")
  expect_error(cluster_agreement(1:3, 1:4), "same length")
})

test_that("cluster_features: token-level values do not change when rows without f0 are removed or rows are reordered", {
  # A tone tier from a TextGrid: "" in the silences at both edges, the tone
  # inside, and a stray second label. Rows without f0 at the edges and inside.
  d <- make_synth(n_per = 4, np = 20)
  d$tone_tier <- d$tone
  d$speaker   <- ifelse(d$token %in% c("tk001", "tk005"), "s2", "s1")
  n_tok <- length(unique(d$token))
  pos <- stats::ave(seq_len(nrow(d)), d$token, FUN = seq_along)
  d$f0[pos %in% c(1:3, 10, 19:20)] <- NA
  d$tone_tier[pos %in% c(1:4, 18:20)] <- ""
  d$tone_tier[pos == 6] <- "other"
  d$speaker[pos == 1] <- "s9"                     # first row only
  expect_false(all(tapply(d$tone_tier, d$token, function(x) length(unique(x)) == 1)))

  cf <- function(x) {
    m <- cluster_features(x, f0 = "f0", token = "token", time = "time",
                          speaker = "speaker", tone = "tone_tier",
                          n_points = 10)$meta
    m <- m[order(m$token), ]
    rownames(m) <- NULL
    m
  }
  raw  <- cf(d)
  nona <- cf(d[!is.na(d$f0), ])
  set.seed(3)
  shuf <- cf(d[sample(nrow(d)), ])

  expect_equal(nrow(raw), n_tok)
  expect_identical(raw$tone, ifelse(raw$token <= "tk004", "rising",
                                    ifelse(raw$token <= "tk008", "falling", "level")))
  expect_identical(raw$speaker, ifelse(raw$token %in% c("tk001", "tk005"), "s2", "s1"))
  expect_identical(nona, raw)
  expect_identical(shuf, raw)
})

test_that("cluster_features: speaker and tone stay optional", {
  d <- make_synth(n_per = 3, np = 12)
  f <- cluster_features(d, f0 = "f0", n_points = 8)
  expect_identical(names(f$meta), "token")
  f <- cluster_features(d, f0 = "f0", n_points = 8, tone = "tone")
  expect_identical(names(f$meta), c("token", "tone"))
  f <- cluster_features(d, f0 = "f0", n_points = 8, speaker = "nope", tone = "tone")
  expect_identical(names(f$meta), c("token", "tone"))
})

test_that("cluster_features never averages two rows at a syllable boundary", {
  # T4 + T1 disyllables on syllable_tseq: syllable 1 falls to 150 Hz, a
  # voiceless closure follows, and syllable 2 is level at 240 Hz. Both
  # syllables have a row at 1; a blend of the two (195 Hz) is neither.
  mk <- function(tok) {
    t  <- seq(0, 0.40, by = 0.01)
    s1 <- t < 0.20
    f0 <- ifelse(s1, 250 - 100 * (t / 0.17), 240)
    f0[t > 0.17 & t < 0.23] <- NA
    data.frame(token = tok, time = t, f0 = f0,
               syllable_start = ifelse(s1, 0, 0.20),
               syllable_end = ifelse(s1, 0.20, 0.41),
               syllable_i = ifelse(s1, 1L, 2L))
  }
  d <- normalise_time_landmarks(do.call(rbind, lapply(paste0("w", 1:4), mk)),
                                "time", "syllable")
  f <- cluster_features(d, f0 = "f0", token = "token", time = "syllable_tseq",
                        n_points = 21, features = "points")
  # Point 11 is at 1: the next syllable's first f0
  expect_equal(unname(f$contours[, 11]), rep(240, 4))
  # Point 10, at 0.9, is syllable 1 only
  expect_equal(unname(f$contours[, 10]), rep(250 - 100 * (0.9 * 0.17) / 0.17, 4))
  # The syllable is read from syllable_t01 or syllable_i, so the row order
  # does not matter
  set.seed(5)
  for (x in list(d[nrow(d):1, ], d[sample(nrow(d)), ])) {
    g <- cluster_features(x, f0 = "f0", token = "token", time = "syllable_tseq",
                          n_points = 21, features = "points")
    expect_equal(g$contours[rownames(f$contours), ], f$contours)
  }
  x <- d[sample(nrow(d)), ]
  x$syllable_t01 <- NULL
  g <- cluster_features(x, f0 = "f0", token = "token", time = "syllable_tseq",
                        n_points = 21, features = "points")
  expect_equal(g$contours[rownames(f$contours), ], f$contours)
})

test_that("cluster_features on a <tier>_tbound runs from the first f0 to the last", {
  # Two intervals that do not touch, with a row with f0 on each end of the
  # pause, so both sit at 1 on syllable_tbound: syllable 2's is used there
  mk <- function(tok) {
    t <- c(seq(0.06, 0.18, by = 0.02), seq(0.24, 0.40, by = 0.02))
    s1 <- t < 0.2
    data.frame(token = tok, time = t,
               f0 = ifelse(s1, 250 - 500 * (t - 0.06), 240),
               syllable_start = ifelse(s1, 0.06, 0.24),
               syllable_end = ifelse(s1, 0.18, 0.40),
               syllable_i = ifelse(s1, 1L, 2L))
  }
  d <- normalise_time_landmarks(rbind(mk("a"), mk("b")), "time", "syllable")
  expect_equal(sum(d$syllable_tbound == 1), 4L)   # two rows at 1 per token
  f <- cluster_features(d, time = "syllable_tbound", n_points = 21, features = "points")
  # Point 11 is at 1
  expect_equal(unname(f$contours[, 11]), rep(240, 2))
  expect_equal(unname(f$contours[, 1]), rep(250, 2))
  set.seed(9)
  g <- cluster_features(d[sample(nrow(d)), ], time = "syllable_tbound", n_points = 21,
                        features = "points")
  expect_equal(g$contours[rownames(f$contours), ], f$contours)
})

test_that("cluster_features averages two recordings that share a token name", {
  tp <- seq(0, 1, by = 0.05)
  d <- rbind(data.frame(token = "dup", time_prop = tp, f0 = 200 + 40 * tp),
             data.frame(token = "dup", time_prop = tp, f0 = 240 - 40 * tp),
             data.frame(token = "one", time_prop = tp, f0 = 200 + 40 * tp))
  expect_message(f <- cluster_features(d, time = "time_prop", n_points = 41),
                 "1 token(s) (dup)", fixed = TRUE)
  expect_equal(unname(f$contours["dup", ]), rep(220, 41))
  expect_no_message(cluster_features(d[d$token == "one", ], time = "time_prop"))
})

test_that("cluster_features skips 0 Hz as no f0, as the time axes do", {
  mk <- function(tok, base) data.frame(token = tok, time = 0:10 / 100,
                                       f0 = c(0, base + 0:8, 0))
  d <- rbind(mk("a", 200), mk("b", 210))
  d$f0[d$token == "a" & d$time == 0.05] <- 0       # a 0 Hz frame inside token a
  f <- cluster_features(d, n_points = 11)
  # From the first f0 above 0 to the last; the inner 0 is skipped
  expect_equal(unname(f$contours["a", ]), seq(200, 208, length.out = 11))
  expect_equal(f$contours, cluster_features(d[d$f0 > 0, ], n_points = 11)$contours)
  # In a column that is not in Hz, 0 is a value
  d$f0_st <- c(rep(0, 11), 1:11)
  expect_equal(unname(cluster_features(d, f0 = "f0_st", n_points = 11)$contours["a", ]),
               rep(0, 11))
})
