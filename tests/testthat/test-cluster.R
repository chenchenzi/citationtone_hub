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
