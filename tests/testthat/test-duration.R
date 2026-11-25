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

test_that("contour_duration(set =) and normalise_time_landmarks() use the same units", {
  a <- landmarked(); b <- landmarked()
  b$token <- "v"
  d <- rbind(a, b, transform(a[2:4, ], token = NA))
  dur <- contour_duration(d, set = "syllable", min_run = 1)
  ax  <- normalise_time_landmarks(d, "time", "syllable")
  # Rows in an empty-label interval and rows with no token are in no unit
  none <- is.na(d$syllable_i) | is.na(d$token)
  expect_true(all(is.na(dur$syllable_f0_dur[none])))
  expect_true(all(is.na(ax$syllable_t01[none])))
  # Every other row's unit has a duration, and an axis over its f0 rows
  unit <- paste(d$token, d$syllable_start)[!none]
  has  <- !is.na(d$f0[!none])
  expect_false(anyNA(dur$syllable_f0_dur[!none]))
  # Where the unit's span has length, it is the span of its axis
  span <- tapply(d$time[!none][has], unit[has], function(x) diff(range(x)))
  t01  <- tapply(ax$syllable_t01[!none][has], unit[has], function(x) diff(range(x)))
  expect_equal(as.vector(span[span > 0]),
               as.vector(tapply(dur$syllable_f0_dur[!none], unit, `[`, 1))[span > 0])
  expect_true(all(t01[span > 0] == 1))
})

test_that("custom name, and missing columns are reported", {
  df <- data.frame(token = "a", time = c(0, 0.1), f0 = c(100, 110))
  expect_true("dur" %in% names(contour_duration(df, name = "dur")))
  expect_error(contour_duration(df, f0 = "pitch"), "pitch")
  expect_error(contour_duration(df, set = "syllable"), "syllable_start")
})

# ---------- the same durations with the rows without f0 removed --------------
# Once those rows are gone, a gap shows as a time step longer than 1.5 times
# the token's usual step. That needs times written finely enough to tell one
# step from two (here 10 ms frames written to 3 decimals), or a voiced_run
# column.

frames <- function(f0, token = "a") {
  data.frame(token = token, time = 0.005 + 0.01 * (seq_along(f0) - 1),
             f0 = f0, stringsAsFactors = FALSE)
}
drop_na_f0 <- function(df, f0 = "f0") {
  out <- df[!is.na(df[[f0]]) & df[[f0]] != 0, , drop = FALSE]
  rownames(out) <- NULL
  out
}
dur1 <- function(df, ...) contour_duration(df, ...)$token_f0_dur[1]

test_that("rows removed: whole-token durations as with the rows kept", {
  df <- data.frame(token = rep(c("a", "b"), each = 7),
                   time  = rep(seq(0.005, 0.065, by = 0.01), 2),
                   f0    = c(NA, 200, 205, NA, 190, 195, NA,
                             0, 0, 150, 160, 170, 180, 0),
                   stringsAsFactors = FALSE)
  kept <- contour_duration(df)
  gone <- contour_duration(drop_na_f0(df))
  expect_equal(unique(kept$token_f0_dur), c(0.04, 0.03))
  expect_equal(tapply(gone$token_f0_dur, gone$token, `[`, 1),
               tapply(kept$token_f0_dur, kept$token, `[`, 1))
})

test_that("rows removed: a lone frame cut off by a gap still does not count", {
  df <- frames(c(300, NA, NA, 200, 210, 220, 215, NA, NA, 90))
  gone <- drop_na_f0(df)
  expect_equal(dur1(df), 0.03)                       # 0.035 .. 0.065
  expect_equal(dur1(gone), 0.03)                     # the time steps show the gaps
  expect_equal(dur1(gone, min_run = 1), dur1(df, min_run = 1))   # 0.09, literal
  # A one-row gap gives a step of exactly 2 frames, which is a gap too.
  one <- frames(c(NA, 300, NA, 200, 210, 220, NA))
  expect_equal(dur1(drop_na_f0(one)), dur1(one))
  expect_equal(dur1(one), 0.02)
})

test_that("rows removed: the duration equals the span trim_to_voiced() keeps", {
  df <- rbind(frames(c(300, NA, NA, 200, 210, 212, NA, 215, 220, 225, 230,
                       NA, NA, 90), "a"),
              frames(c(NA, 150, 160, 170, NA, NA, NA, 180, 185, 190, NA), "b"))
  gone <- drop_na_f0(df)
  kept <- trim_to_voiced(gone)
  span <- tapply(kept$time, kept$token, function(x) max(x) - min(x))
  out  <- contour_duration(gone)
  expect_equal(tapply(out$token_f0_dur, out$token, `[`, 1), span)
  expect_equal(tapply(out$token_f0_dur, out$token, `[`, 1),
               tapply(contour_duration(df)$token_f0_dur, df$token, `[`, 1))
})

test_that("rows removed: row order does not matter and is preserved", {
  df <- rbind(frames(c(300, NA, NA, 200, 210, 220, 215, NA, NA, 90), "a"),
              frames(c(NA, 150, 160, 170, NA, 180, 185, NA, NA, NA), "b"))
  gone <- drop_na_f0(df)
  set.seed(7)
  shuffled <- gone[sample(nrow(gone)), ]
  out <- contour_duration(shuffled)
  expect_equal(out$token, shuffled$token)
  expect_equal(out$time, shuffled$time)
  want <- c(a = 0.03, b = 0.05)
  expect_equal(out$token_f0_dur, unname(want[shuffled$token]))
  expect_equal(unique(contour_duration(df)$token_f0_dur), unname(want))
})

test_that("rows removed: negative (normalised) values still count as measured", {
  df <- data.frame(token = "a", time = 0.005 + 0.01 * (0:9),
                   f0_st = c(4, NA, NA, -2.5, -1, 0.5, 1.5, 2, NA, -3))
  gone <- df[!is.na(df$f0_st), ]
  expect_equal(dur1(df, f0 = "f0_st"), 0.04)        # 0.035 .. 0.075
  expect_equal(dur1(gone, f0 = "f0_st"), 0.04)
})

test_that("rows removed: a voiced_run column marks the gaps the time steps cannot", {
  # Times written to 2 decimals at 10 ms frames: one step and two cannot be
  # told apart reliably, so the time steps mark no gaps. voiced_run still does.
  df <- data.frame(token = "a", time = 0.01 * (1:10),
                   f0 = c(300, NA, NA, 200, 210, 220, 215, NA, NA, 90))
  gone <- df[!is.na(df$f0), ]
  expect_equal(dur1(df), 0.03)
  expect_equal(dur1(gone), 0.09)                     # no gap to see
  gone$voiced_run <- c(1L, 2L, 2L, 2L, 2L, 3L)
  expect_equal(dur1(gone), 0.03)
  expect_equal(dur1(gone, run = NULL), 0.09)         # the column ignored
  expect_equal(dur1(gone, run = "voiced_run", min_run = 1), 0.09)
})

test_that("rows removed: each segment of a set counts only its own rows", {
  df <- landmarked()
  df$time <- df$time + 0.005
  for (cl in c("syllable_start", "syllable_end")) df[[cl]] <- df[[cl]] + 0.005
  gone <- df[!is.na(df$f0), ]
  kept <- contour_duration(df, set = "syllable")
  out  <- contour_duration(gone, set = "syllable")
  expect_equal(kept$syllable_f0_dur, want_syl)
  expect_equal(out$syllable_f0_dur, want_syl[!is.na(df$f0)])
})

test_that("f0_long.csv: the same durations with and without the rows without f0", {
  path <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(path), "test/f0_long.csv not found")
  fl <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  per_tok <- function(d) {
    x <- d[!duplicated(d$token), ]
    stats::setNames(x$token_f0_dur, x$token)
  }
  kept <- per_tok(contour_duration(fl))
  gone <- per_tok(contour_duration(fl[!is.na(fl$f0), ]))[names(kept)]
  expect_length(kept, 1853L)
  expect_identical(gone, kept)
  # A token with a lone edge frame cut off by a gap (0.135 s, then 0.165 s on)
  tok <- paste0("dc102\u5988", "5s86.4125")
  expect_equal(unname(gone[tok]), 0.32)
  expect_equal(unname(per_tok(contour_duration(fl[!is.na(fl$f0), ],
                                               min_run = 1))[tok]), 0.38)
})
