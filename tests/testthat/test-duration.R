# ---------- contour_duration --------------------------------------------------

test_that("whole token: first to last measured frame; edges and gaps ignored", {
  df <- data.frame(token = rep(c("a", "b"), each = 7),
                   time  = rep(seq(0, 0.6, by = 0.1), 2),
                   f0    = c(NA, 200, 205, NA, 190, 195, NA,   # a: 0.1 .. 0.5
                             0, 0, 150, 160, 170, 180, 0),     # b: 0 Hz unvoiced, 0.2 .. 0.5
                   stringsAsFactors = FALSE)
  out <- contour_duration(df)
  expect_equal(out$token_f0_dur, rep(c(0.4, 0.3), each = 7))
  expect_equal(out[, names(df)], df)                  # other columns untouched
})

test_that("a token with no measured f0 gets NA, a single frame gets 0", {
  df <- data.frame(token = c("a", "a", "b", "b", "b"),
                   time  = c(0.1, 0.2, 0.1, 0.2, 0.3),
                   f0    = c(NA, 0, NA, 180, NA))
  out <- contour_duration(df)
  expect_true(all(is.na(out$token_f0_dur[1:2])))
  expect_equal(out$token_f0_dur[3:5], rep(0, 3))
})

test_that("row order does not matter and is preserved", {
  df <- data.frame(token = rep(c("a", "b"), each = 4),
                   time  = rep(c(0.1, 0.2, 0.3, 0.4), 2),
                   f0    = c(100, 110, 120, NA, NA, NA, 95, 99))
  shuffled <- df[c(5, 2, 8, 1, 7, 4, 3, 6), ]
  out <- contour_duration(shuffled)
  expect_equal(out$token, shuffled$token)
  expect_equal(out$token_f0_dur, ifelse(shuffled$token == "a", 0.2, 0.1))
})

test_that("negative (normalised) f0 values count as measured", {
  df <- data.frame(token = "a", time = seq(0.1, 0.5, by = 0.1),
                   f0_st = c(-2.5, -1, NA, 1.5, 2))
  expect_equal(contour_duration(df, f0 = "f0_st")$token_f0_dur, rep(0.4, 5))
})

test_that("a lone frame cut off from the contour does not stretch it", {
  # Stray voiced frames in the silence at both ends (e.g. octave errors).
  df <- data.frame(token = "a", time = seq(0, 0.9, by = 0.1),
                   f0 = c(300, NA, NA, 200, 210, 220, 215, NA, NA, 90))
  expect_equal(contour_duration(df)$token_f0_dur[1], 0.3)               # 0.3 .. 0.6
  expect_equal(contour_duration(df, min_run = 1)$token_f0_dur[1], 0.9)  # literal
})

test_that("with no run long enough, the first and last measured frames are used", {
  df <- data.frame(token = "a", time = seq(0, 0.4, by = 0.1),
                   f0 = c(200, NA, 210, NA, 220))
  expect_equal(contour_duration(df)$token_f0_dur[1], 0.4)
})

test_that("whole-token duration equals the voiced span trim_to_voiced() keeps", {
  df <- data.frame(token = rep(c("a", "b"), each = 10),
                   time  = rep(seq(0, 0.9, by = 0.1), 2),
                   f0    = c(300, NA, NA, 200, 210, NA, 215, 220, NA, 90,
                             NA, 150, 160, 170, NA, NA, NA, 180, 185, NA))
  kept <- trim_to_voiced(df)
  span <- tapply(kept$time, kept$token, function(x) max(x) - min(x))
  out  <- contour_duration(df)
  expect_equal(tapply(out$token_f0_dur, out$token, `[`, 1), span)
})

# A disyllable: silence | s1 | s2 | silence, with voiced frames in both
# silences that must not count, and an unvoiced gap inside s2.
landmarked <- function() {
  data.frame(
    token          = "w",
    time           = seq(0, 1.2, by = 0.1),
    f0             = c(190, 200, 210, 220, NA, 180, 175, NA, NA, 170, 160, NA, 150),
    syllable       = c("", rep("s1", 4), rep("s2", 7), ""),
    syllable_start = c(0, rep(0.1, 4), rep(0.5, 7), 1.2),
    syllable_end   = c(0.1, rep(0.5, 4), rep(1.2, 7), 1.3),
    syllable_i     = c(NA, rep(1, 4), rep(2, 7), NA),
    stringsAsFactors = FALSE)
}
want_syl <- c(NA, rep(0.2, 4), rep(0.5, 7), NA)   # s1 0.1..0.3, s2 0.5..1.0

test_that("set: each labelled segment measured on its own; silence gets NA", {
  out <- contour_duration(landmarked(), set = "syllable")
  expect_true("syllable_f0_dur" %in% names(out))
  expect_equal(out$syllable_f0_dur, want_syl)
})

test_that("set without an index column falls back to the segment label", {
  df <- landmarked(); df$syllable_i <- NULL
  expect_equal(contour_duration(df, set = "syllable")$syllable_f0_dur, want_syl)
})

test_that("tokens sharing segment boundaries are measured separately", {
  a <- landmarked(); b <- landmarked()
  b$token <- "v"; b$f0[2:4] <- c(NA, NA, 220)       # v's s1: a single frame
  out <- contour_duration(rbind(a, b), set = "syllable")
  expect_equal(out$syllable_f0_dur[out$token == "w"][2], 0.2)
  expect_equal(out$syllable_f0_dur[out$token == "v"][2], 0)
})

test_that("custom name, and missing columns are reported", {
  df <- data.frame(token = "a", time = c(0, 0.1), f0 = c(100, 110))
  expect_true("dur" %in% names(contour_duration(df, name = "dur")))
  expect_error(contour_duration(df, f0 = "pitch"), "pitch")
  expect_error(contour_duration(df, set = "syllable"), "syllable_start")
})
