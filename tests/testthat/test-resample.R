# ---------- resample_f0_equal ------------------------------------------------

test_that("resamples each token to n equidistant points with point + time_prop", {
  df <- data.frame(token = "a",
                   time  = seq(0.1, 0.6, by = 0.05),
                   f0    = seq(100, 200, length.out = 11),
                   stringsAsFactors = FALSE)
  out <- resample_f0_equal(df, n = 21)
  expect_equal(nrow(out), 21)
  expect_equal(out$point, 1:21)
  expect_equal(out$time, seq(0.1, 0.6, length.out = 21))
  expect_equal(out$time_prop, seq(0, 1, length.out = 21))
  # A linear contour is reproduced exactly by linear interpolation.
  expect_equal(out$f0, seq(100, 200, length.out = 21))
})

test_that("no value is ever blended across an unvoiced frame", {
  df <- data.frame(token = "a",
                   time  = seq(0, 0.5, by = 0.05),
                   f0    = c(100, 105, NA, NA, 120, 125, 130, NA, 140, 145, 150))
  out <- resample_f0_equal(df, n = 21)
  # Points at/between the NA native frames stay NA; voiced-run points are
  # interpolated. Exact frame hits keep the frame's value. (Indexing by
  # position: point i sits at time (i - 1) * 0.025.)
  expect_equal(out$f0[2], 102.5)      # within the first voiced run
  expect_equal(out$f0[3], 105)        # exact voiced frame at 0.05
  expect_true(is.na(out$f0[4]))       # between voiced 0.05 and unvoiced 0.10
  expect_true(is.na(out$f0[5]))       # exact unvoiced frame at 0.10
  expect_true(is.na(out$f0[6]))       # inside the unvoiced gap
  expect_equal(out$f0[10], 122.5)     # within the second voiced run
  # t = 0.375 sits exactly midway between the unvoiced 0.35 and the voiced
  # 0.40. Praat's `phase < 0.5` test is false at a tie, so the RIGHT frame is
  # the near one: its measured value is kept, and nothing is blended across
  # the unvoiced frame.
  expect_equal(out$f0[16], 140)
})

test_that("intensity is interpolated with extended ends", {
  df <- data.frame(token = "a",
                   time  = c(0, 0.1, 0.2),
                   f0    = c(100, 110, 120),
                   intensity = c(NA, 60, 70))
  out <- resample_f0_equal(df, n = 5)
  # Grid 0, 0.05, 0.10, 0.15, 0.20; finite anchors at 0.1 (60) and 0.2 (70).
  # rule = 2: points before the first finite anchor take its value.
  expect_equal(out$intensity, c(60, 60, 60, 65, 70))
})

test_that("token-constant columns are carried; per-frame columns are dropped", {
  df <- data.frame(token = rep(c("a", "b"), each = 3),
                   time  = rep(c(0, 0.1, 0.2), 2),
                   f0    = c(100, 110, 120, 200, 210, 220),
                   speakr = rep(c("s1", "s2"), each = 3),
                   frame_note = as.character(1:6),
                   stringsAsFactors = FALSE)
  out <- resample_f0_equal(df, n = 5)
  expect_equal(nrow(out), 10)
  expect_equal(out$speakr, rep(c("s1", "s2"), each = 5))
  expect_false("frame_note" %in% names(out))
  expect_equal(attr(out, "dropped_columns"), "frame_note")
})

test_that("degenerate tokens keep their rows; times stay sorted per token", {
  df <- data.frame(token = c("a", "b", "b", "b"),
                   time  = c(0.2, 0.3, 0.1, 0.2),
                   f0    = c(100, 230, 210, 220),
                   stringsAsFactors = FALSE)
  out <- resample_f0_equal(df, n = 3)
  a <- out[out$token == "a", ]
  expect_equal(nrow(a), 1)                 # single frame: not resampled
  expect_equal(a$time_prop, 0.5)
  b <- out[out$token == "b", ]
  expect_equal(nrow(b), 3)
  expect_false(is.unsorted(b$time))
  expect_equal(b$f0, c(210, 220, 230))     # sorted by time before resampling
})

test_that("input validation", {
  df <- data.frame(token = "a", time = 0.1, f0 = 100)
  expect_error(resample_f0_equal(df, n = 1), "at least 2")
  expect_error(resample_f0_equal(data.frame(a = 1)), "not found")
})

test_that("non-numeric time / f0 columns are rejected", {
  df <- data.frame(token = "a",
                   time  = factor(c("0.9", "10.5", "2.0", "0.15")),
                   f0    = factor(c("100", "90", "80", "110")))
  expect_error(resample_f0_equal(df, n = 5), "must be numeric")
})

test_that("NA token IDs are resampled as their own token", {
  df <- data.frame(token = c("a", "a", "a", NA, NA, NA),
                   time  = c(0, 0.1, 0.2, 0, 0.1, 0.2),
                   f0    = c(100, 110, 120, 200, 210, 220),
                   stringsAsFactors = FALSE)
  out <- resample_f0_equal(df, n = 3)
  expect_equal(nrow(out), 6)
  expect_equal(sum(is.na(out$token)), 3)
  expect_equal(out$f0[is.na(out$token)], c(200, 210, 220))
})

test_that("a grid point landing on a frame is not lost to float rounding", {
  # n == number of native frames: every output time coincides with a frame,
  # but seq() can miss by an ulp. Frame 2 is unvoiced; frame 3 must survive.
  tt <- 0.0732878986 + (0:35) * 0.0106675905
  ff <- rep(150, 36); ff[2] <- NA
  out <- resample_f0_equal(data.frame(token = "x", time = tt, f0 = ff), n = 36)
  expect_equal(sum(is.na(out$f0)), 1)
  expect_equal(out$f0[3], 150)
})

# ---------- trim_to_voiced ---------------------------------------------------

test_that("trim_to_voiced keeps the span between first and last voiced frame", {
  tt <- seq(0, 0.30, by = 0.01)
  f  <- rep(NA_real_, 31)
  f[11:21] <- 120      # voiced 0.10-0.20
  f[16]    <- NA       # unvoiced patch inside the region
  df  <- data.frame(token = "x", time = tt, f0 = f)
  out <- trim_to_voiced(df)
  expect_equal(range(out$time), c(0.10, 0.20))
  expect_equal(sum(is.na(out$f0)), 1)   # internal gap kept, not trimmed away
})

test_that("a stray voiced frame in silence does not stretch the region", {
  tt <- seq(0, 0.30, by = 0.01)
  f  <- rep(NA_real_, 31)
  f[11:21] <- 120
  f[3]     <- 300      # isolated octave-error frame in the leading silence
  df  <- data.frame(token = "x", time = tt, f0 = f)
  expect_equal(range(trim_to_voiced(df)$time), c(0.10, 0.20))
  # min_run = 1 opts into the naive behaviour
  expect_equal(min(trim_to_voiced(df, min_run = 1)$time), 0.02)
})

test_that("trim_to_voiced drops tokens with no voiced frame and is per token", {
  df <- data.frame(
    token = rep(c("a", "b"), each = 5),
    time  = rep(seq(0, 0.04, by = 0.01), 2),
    f0    = c(NA, 100, 110, 120, NA,        # a: voiced 0.01-0.03
              NA, NA, NA, NA, NA),          # b: never voiced
    stringsAsFactors = FALSE)
  out <- trim_to_voiced(df)
  expect_equal(unique(out$token), "a")
  expect_equal(range(out$time), c(0.01, 0.03))
})

test_that("trim then resample puts real f0 at both endpoints", {
  tt <- seq(0, 0.30, by = 0.01)
  f  <- rep(NA_real_, 31); f[11:21] <- seq(100, 150, length.out = 11)
  out <- resample_f0_equal(trim_to_voiced(data.frame(token = "x", time = tt, f0 = f)),
                           n = 11)
  expect_equal(nrow(out), 11)
  expect_equal(out$f0[1], 100)
  expect_equal(out$f0[11], 150)
  expect_equal(out$time_prop, seq(0, 1, length.out = 11))
})

test_that("point numbers restart at 1 for every token", {
  df <- data.frame(token = rep(c("a", "b"), each = 4),
                   time  = rep(c(0, 0.1, 0.2, 0.3), 2),
                   f0    = c(100, 110, 120, 130, 200, 210, 220, 230),
                   stringsAsFactors = FALSE)
  out <- resample_f0_equal(df, n = 5)
  expect_equal(out$point, rep(1:5, 2))
  expect_equal(out$point[out$token == "b"], 1:5)
  # A degenerate single-frame token still gets point 1.
  one <- resample_f0_equal(data.frame(token = "z", time = 0.2, f0 = 100), n = 5)
  expect_equal(one$point, 1L)
})

test_that("input columns named point / time_prop cannot clobber the computed ones", {
  df <- data.frame(token = "a", time = c(0, 0.1, 0.2), f0 = c(100, 110, 120),
                   point = 99L, time_prop = 42, stringsAsFactors = FALSE)
  out <- resample_f0_equal(df, n = 3)
  expect_equal(out$point, 1:3)
  expect_equal(out$time_prop, c(0, 0.5, 1))
})

# ---------- flag_f0_gaps -----------------------------------------------------

test_that("flag_f0_gaps marks only interrupted voicing, not missing edges", {
  df <- data.frame(
    token = rep(c("clean", "gap", "edge", "none"), each = 5),
    time  = rep(1:5, 4),
    f0    = c(100, 101, 102, 103, 104,     # no missing values at all
              100,  NA,  NA, 103, 104,     # voicing interrupted mid-token
               NA, 101, 102, 103,  NA,     # missing only at the edges
               NA,  NA,  NA,  NA,  NA),    # no f0 anywhere
    stringsAsFactors = FALSE)
  out <- unique(flag_f0_gaps(df)[, c("token", "n_missing", "has_gap")])
  expect_equal(out$n_missing, c(0L, 2L, 2L, 5L))
  expect_equal(out$has_gap,   c(FALSE, TRUE, FALSE, FALSE))
})

test_that("flag_f0_gaps judges gaps in time order, not row order", {
  df <- data.frame(token = "a", time = c(3, 1, 2), f0 = c(120, 100, NA),
                   stringsAsFactors = FALSE)
  out <- flag_f0_gaps(df)
  expect_true(all(out$has_gap))     # sorted: 100, NA, 120 -> interior gap
  expect_equal(unique(out$n_missing), 1L)
})

test_that("flag_f0_gaps handles empty input and validates columns", {
  e <- flag_f0_gaps(data.frame(token = character(0), time = numeric(0), f0 = numeric(0)))
  expect_equal(nrow(e), 0L)
  expect_true(all(c("n_missing", "has_gap") %in% names(e)))
  expect_error(flag_f0_gaps(data.frame(a = 1)), "not found")
})

test_that("Praat rule: a point keeps the near frame when only the far one is unvoiced", {
  # Frames 0 / 0.1 / 0.2 with the last unvoiced; n = 4 avoids exact 0.5 ties.
  df  <- data.frame(token = "a", time = c(0, 0.1, 0.2), f0 = c(100, 200, NA))
  out <- resample_f0_equal(df, n = 4)
  expect_equal(out$time, seq(0, 0.2, length.out = 4))
  # point 2: both neighbours voiced -> blended, weighted from the nearer frame
  expect_equal(out$f0[2], 200 + (1/3) * (100 - 200))
  # point 3: nearer frame (0.1) voiced, far frame (0.2) unvoiced -> keeps 200.
  # The stricter pre-Praat rule returned NA here.
  expect_equal(out$f0[3], 200)
  # point 4: lands on the unvoiced frame itself -> nothing measured
  expect_true(is.na(out$f0[4]))
})

test_that("Praat rule: interpolation still never blends across an unvoiced frame", {
  # Voiced, unvoiced, voiced. No output point may sit between 100 and 300.
  df  <- data.frame(token = "a", time = c(0, 0.1, 0.2), f0 = c(100, NA, 300))
  out <- resample_f0_equal(df, n = 7)
  vals <- out$f0[!is.na(out$f0)]
  expect_true(all(vals %in% c(100, 300)))
})

test_that("Praat rule: a point on a frame takes that frame value exactly", {
  df  <- data.frame(token = "a", time = c(0, 0.25, 0.5, 0.75, 1),
                    f0 = c(100, 140, 180, 220, 260))
  out <- resample_f0_equal(df, n = 5)   # grid coincides with every frame
  expect_equal(out$f0, c(100, 140, 180, 220, 260))
})

test_that("method = 'nearest' takes the frame the point falls in, never blending", {
  df <- data.frame(token = "a", time = c(0, 0.1, 0.2), f0 = c(100, 200, NA))
  out <- resample_f0_equal(df, n = 4, method = "nearest")
  # point 2 sits nearer the 0.1 frame, so it takes 200 rather than a blend
  expect_equal(out$f0, c(100, 200, 200, NA))
  # linear blends the same point instead
  lin <- resample_f0_equal(df, n = 4, method = "linear")
  expect_equal(lin$f0[2], 200 + (1/3) * (100 - 200))
})

test_that("nearest never invents a value: every f0 came from a frame", {
  set.seed(11)
  tt <- seq(0, 0.4, by = 0.01)
  ff <- 120 + rnorm(length(tt), sd = 8); ff[10:14] <- NA
  out <- resample_f0_equal(data.frame(token = "a", time = tt, f0 = ff),
                           n = 17, method = "nearest")
  got <- out$f0[!is.na(out$f0)]
  expect_true(all(got %in% ff[!is.na(ff)]))
})

test_that("nearest applies to intensity as well, so the row stays self-consistent", {
  df <- data.frame(token = "a", time = c(0, 0.1, 0.2), f0 = c(100, 200, 300),
                   intensity = c(50, 60, 70))
  out <- resample_f0_equal(df, n = 4, method = "nearest")
  expect_true(all(out$intensity %in% c(50, 60, 70)))
})
