test_that("semitone normalisation matches the formula", {
  df <- data.frame(
    speaker = rep("S01", 4),
    tone    = c("T1", "T1", "T2", "T2"),
    f0      = c(100, 200, 100, 200)
  )

  out <- normalise_f0(df, method = "semitone", mean_method = "simple")

  expect_true(all(c("speaker_mean", "f0_st") %in% names(out)))
  expect_equal(unique(out$speaker_mean), 150)
  expect_equal(out$f0_st, 12 * log2(c(100, 200, 100, 200) / 150))
})

test_that("zscore normalisation matches the formula", {
  df <- data.frame(
    speaker = rep("S01", 4),
    tone    = c("T1", "T1", "T2", "T2"),
    f0      = c(100, 200, 100, 200)
  )

  out <- normalise_f0(df, method = "zscore", mean_method = "simple")

  expect_true("f0_zscore" %in% names(out))
  expect_equal(unique(out$speaker_mean), 150)

  # By construction, the within-speaker z-score has mean 0 and the
  # given sd. For a balanced 2-value set around the mean: z = +/- 1
  # when divided by its own sample sd.
  s <- sd(df$f0)
  expect_equal(out$f0_zscore, (df$f0 - 150) / s)
})

test_that("weighted vs simple speaker means differ on unbalanced designs", {
  # T1 has 4 tokens (low f0), T2 has 2 tokens (high f0)
  df <- data.frame(
    speaker = rep("S01", 6),
    tone    = c("T1", "T1", "T1", "T1", "T2", "T2"),
    f0      = c(100, 110, 120, 130, 200, 210)
  )

  simple   <- normalise_f0(df, method = "semitone", mean_method = "simple")
  weighted <- normalise_f0(df, method = "semitone", mean_method = "weighted")

  # Simple = mean of all 6: (100+110+120+130+200+210)/6 = 145
  expect_equal(unique(simple$speaker_mean), 145)
  # Weighted = mean of per-tone means: mean(115, 205) = 160
  expect_equal(unique(weighted$speaker_mean), 160)
})

test_that("per-speaker normalisation is independent across speakers", {
  df <- data.frame(
    speaker = rep(c("S01", "S02"), each = 4),
    tone    = rep(c("T1", "T2"), times = 4),
    f0      = c(100, 200, 100, 200,   # S01 mean = 150
                300, 400, 300, 400)   # S02 mean = 350
  )

  out <- normalise_f0(df, method = "semitone", mean_method = "simple")

  expect_equal(unique(out$speaker_mean[out$speaker == "S01"]), 150)
  expect_equal(unique(out$speaker_mean[out$speaker == "S02"]), 350)
})

test_that("custom column names are supported", {
  df <- data.frame(spk = "S01", t = "T1", hz = c(100, 200))
  out <- normalise_f0(df, f0 = "hz", speaker = "spk", tone = "t",
                      method = "semitone", mean_method = "simple")
  expect_true("f0_st" %in% names(out))
  expect_equal(unique(out$speaker_mean), 150)
})

test_that("missing or non-numeric f0 column gives a clear error", {
  df <- data.frame(speaker = "S01", tone = "T1", f0 = 100)
  expect_error(normalise_f0(df, f0 = "nonexistent"),
               "Column.*not found")

  df2 <- data.frame(speaker = "S01", tone = "T1", f0 = "not_numeric")
  expect_error(normalise_f0(df2, f0 = "f0"),
               "must be numeric")
})

test_that("lz normalisation is the z-score of log f0", {
  df <- data.frame(
    speaker = rep("S01", 4),
    tone    = c("T1", "T1", "T2", "T2"),
    f0      = c(100, 200, 120, 180)
  )
  out <- normalise_f0(df, method = "lz", mean_method = "simple")

  expect_true(all(c("speaker_mean", "f0_lz") %in% names(out)))
  lf <- log(df$f0)
  expect_equal(out$f0_lz, (lf - mean(lf)) / sd(lf))
  # speaker_mean is reported in Hz: the geometric mean.
  expect_equal(unique(out$speaker_mean), exp(mean(lf)))
  # Within a speaker: mean 0, SD 1.
  expect_equal(mean(out$f0_lz), 0)
  expect_equal(sd(out$f0_lz), 1)
})

test_that("lz equals semitones divided by their SD, and ignores scale", {
  df <- data.frame(
    speaker = rep(c("S01", "S02"), each = 5),
    tone    = rep(c("T1", "T2", "T1", "T2", "T1"), 2),
    f0      = c(110, 150, 130, 190, 120,
                220, 300, 260, 380, 240)   # S02 = S01 an octave up
  )
  lz <- normalise_f0(df, method = "lz", mean_method = "simple")
  # Same contour shape an octave apart gives identical LZ values.
  expect_equal(lz$f0_lz[lz$speaker == "S01"], lz$f0_lz[lz$speaker == "S02"])
  # Centred on the geometric mean, semitones / their SD = LZ.
  s1 <- df$f0[1:5]
  st <- 12 * log2(s1 / exp(mean(log(s1))))
  expect_equal(lz$f0_lz[1:5], st / sd(st))
})

test_that("lz weighted centre is the mean of per-tone mean log f0", {
  df <- data.frame(
    speaker = rep("S01", 6),
    tone    = c("T1", "T1", "T1", "T1", "T2", "T2"),
    f0      = c(100, 110, 120, 130, 200, 210)
  )
  out <- normalise_f0(df, method = "lz", mean_method = "weighted")
  lf <- log(df$f0)
  centre <- mean(c(mean(lf[1:4]), mean(lf[5:6])))
  expect_equal(unique(out$speaker_mean), exp(centre))
  # SD weighted the same way: every tone counts once (as stats::cov.wt).
  w  <- rep(c(1 / 8, 1 / 4), c(4, 2))
  cw <- stats::cov.wt(matrix(lf), wt = w, method = "unbiased")
  expect_equal(cw$center, centre)
  expect_equal(out$f0_lz, (lf - centre) / sqrt(cw$cov[1, 1]))
})

test_that("weighted z-scores do not depend on how many tokens each tone has", {
  # Same productions per tone; speaker B just has far more T1 tokens.
  tok <- function(spk, n1, n2) data.frame(
    speaker = spk,
    tone    = rep(c("T1", "T2"), c(n1, n2)),
    f0      = c(rep(c(118, 122), n1 / 2), rep(c(198, 202), n2 / 2)))
  df <- rbind(tok("A", 50, 50), tok("B", 90, 10))
  for (m in c("zscore", "lz")) {
    col <- if (m == "zscore") "f0_zscore" else "f0_lz"
    # Population SD: exactly invariant.
    out <- normalise_f0(df, method = m, sd_correction = FALSE)
    tm  <- tapply(out[[col]], paste(out$speaker, out$tone), mean)
    expect_equal(unname(tm[c("A T1", "A T2")]), unname(tm[c("B T1", "B T2")]))
    # Sample SD: invariant up to the small n - 1 correction.
    out <- normalise_f0(df, method = m)
    tm  <- tapply(out[[col]], paste(out$speaker, out$tone), mean)
    expect_equal(unname(tm[c("A T1", "A T2")]), unname(tm[c("B T1", "B T2")]),
                 tolerance = 0.01)
  }
  # The simple (classic) formula is not: B's lone T2 tokens stretch out.
  out <- normalise_f0(df, method = "lz", mean_method = "simple")
  tm  <- tapply(out$f0_lz, paste(out$speaker, out$tone), mean)
  expect_gt(tm[["B T2"]] - tm[["A T2"]], 0.5)
})

test_that("weighted and simple agree exactly on balanced data", {
  df <- data.frame(
    speaker = rep(c("S01", "S02"), each = 6),
    tone    = rep(c("T1", "T1", "T2", "T2", "T3", "T3"), 2),
    f0      = c(100, 104, 150, 158, 200, 190, 210, 220, 260, 255, 330, 340))
  for (m in c("semitone", "zscore", "lz")) for (corr in c(TRUE, FALSE)) {
    a <- normalise_f0(df, method = m, mean_method = "simple", sd_correction = corr)
    b <- normalise_f0(df, method = m, mean_method = "weighted", sd_correction = corr)
    expect_equal(a, b)
  }
})

test_that("sd_correction switches between the sample and population SD", {
  df <- data.frame(speaker = "S01", tone = "T1", f0 = c(100, 130, 150, 170, 220))
  z1 <- normalise_f0(df, method = "zscore", mean_method = "simple")
  z0 <- normalise_f0(df, method = "zscore", mean_method = "simple", sd_correction = FALSE)
  expect_equal(sd(z1$f0_zscore), 1)
  expect_equal(mean(z0$f0_zscore^2), 1)              # population SD 1
  expect_equal(z0$f0_zscore, z1$f0_zscore * sqrt(5 / 4))
  # Semitones have no SD, so the argument changes nothing.
  expect_equal(normalise_f0(df, method = "semitone", sd_correction = FALSE),
               normalise_f0(df, method = "semitone"))
  expect_error(normalise_f0(df, sd_correction = NA), "TRUE or FALSE")
  expect_error(normalise_f0(df, sd_correction = "yes"), "TRUE or FALSE")
})

test_that("lz treats NA and non-positive f0 as missing", {
  df <- data.frame(
    speaker = rep("S01", 6),
    tone    = "T1",
    f0      = c(100, 0, NA, 200, 150, -1)   # -1: some trackers' unvoiced code
  )
  expect_no_warning(out <- normalise_f0(df, method = "lz", mean_method = "simple"))
  expect_true(all(is.na(out$f0_lz[c(2, 3, 6)])))
  lf <- log(c(100, 200, 150))
  expect_equal(out$f0_lz[c(1, 4, 5)], (lf - mean(lf)) / sd(lf))
})

test_that("existing methods are unchanged by the shared centre code", {
  df <- data.frame(
    speaker = rep(c("S01", "S02"), each = 4),
    tone    = rep(c("T1", "T1", "T2", "T2"), 2),
    f0      = c(100, 110, 200, 210, 150, 160, 300, 320)
  )
  w <- normalise_f0(df, method = "zscore", mean_method = "weighted")
  for (s in c("S01", "S02")) {
    f <- df$f0[df$speaker == s]
    centre <- mean(c(mean(f[1:2]), mean(f[3:4])))
    expect_equal(w$speaker_mean[w$speaker == s], rep(centre, 4))
    expect_equal(w$f0_zscore[w$speaker == s], (f - centre) / sd(f))
  }
  # A pre-existing speaker_mean column is replaced, not duplicated.
  again <- normalise_f0(w, method = "semitone", mean_method = "simple")
  expect_false(any(grepl("speaker_mean.", names(again), fixed = TRUE)))
})

test_that("an f0 column named speaker_mean is read before it is overwritten", {
  df <- data.frame(speaker = "A", tone = c("T1", "T2", "T1", "T2"),
                   speaker_mean = c(100, 120, 140, 160))
  out <- normalise_f0(df, f0 = "speaker_mean", method = "zscore", mean_method = "simple")
  x <- c(100, 120, 140, 160)
  expect_equal(out$f0_zscore, (x - mean(x)) / sd(x))
})
