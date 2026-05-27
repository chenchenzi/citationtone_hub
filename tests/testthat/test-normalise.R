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
