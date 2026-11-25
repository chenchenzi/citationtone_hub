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

# Input with the rows without f0 removed. A gap then shows as a time step
# longer than 1.5 times the token's usual step, which needs times written
# finely enough (here 10 ms frames to 3 decimals), or as a voiced_run column.
frames10 <- function(f0, token = "x") {
  data.frame(token = token, time = 0.005 + 0.01 * (seq_along(f0) - 1),
             f0 = f0, stringsAsFactors = FALSE)
}
voiced_rows <- function(df) {
  out <- df[!is.na(df$f0) & df$f0 > 0, , drop = FALSE]
  rownames(out) <- NULL
  out
}

test_that("rows removed: a stray frame cut off by a gap is still trimmed", {
  df   <- frames10(c(NA, 300, NA, NA, 200, 210, 220, 215, 205, NA, NA, 90, NA))
  gone <- voiced_rows(df)
  expect_equal(range(trim_to_voiced(df)$time), c(0.045, 0.085))
  expect_equal(trim_to_voiced(gone), voiced_rows(trim_to_voiced(df)))
  expect_equal(range(trim_to_voiced(gone)$time), c(0.045, 0.085))
  # min_run = 1 keeps every voiced frame, with or without the rows
  expect_equal(trim_to_voiced(gone, min_run = 1), gone)
  expect_equal(voiced_rows(trim_to_voiced(df, min_run = 1)), gone)
})

test_that("rows removed: trim_to_voiced is per token and keeps the input order", {
  df <- rbind(frames10(c(300, NA, NA, 200, 210, 220, 215, NA, NA, 90), "a"),
              frames10(c(NA, 150, 160, 170, NA, 180, 185, NA, NA, NA), "b"),
              frames10(rep(NA_real_, 6), "c"))
  gone <- voiced_rows(df)
  set.seed(3)
  shuffled <- gone[sample(nrow(gone)), ]
  out  <- trim_to_voiced(shuffled)
  want <- voiced_rows(trim_to_voiced(df))
  expect_setequal(paste(out$token, out$time), paste(want$token, want$time))
  kept <- paste(shuffled$token, shuffled$time) %in% paste(want$token, want$time)
  expect_equal(paste(out$token, out$time),
               paste(shuffled$token, shuffled$time)[kept])
  expect_false("c" %in% trim_to_voiced(df)$token)   # no voiced frame: dropped
})

test_that("rows removed: trim_to_voiced reads the gaps in a voiced_run column", {
  # Times written to 2 decimals at 10 ms frames: the time steps cannot mark
  # the gaps, but voiced_run does.
  df   <- data.frame(token = "x", time = 0.01 * (1:10),
                     f0 = c(300, NA, NA, 200, 210, 220, 215, NA, NA, 90))
  gone <- df[!is.na(df$f0), ]
  expect_equal(range(trim_to_voiced(gone)$time), c(0.01, 0.10))
  gone$voiced_run <- c(1L, 2L, 2L, 2L, 2L, 3L)
  expect_equal(range(trim_to_voiced(gone)$time), c(0.04, 0.07))
  expect_equal(range(trim_to_voiced(gone, run = NULL)$time), c(0.01, 0.10))
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

test_that("rows removed: flag_f0_gaps reads has_gap from the time steps", {
  df <- rbind(frames10(c(NA, 200, 210, NA, NA, 220, 215, 205, NA), "gap"),
              frames10(c(NA, 200, 210, 220, 215, 205, 200, NA, NA), "clean"))
  gone <- voiced_rows(df)
  out  <- unique(flag_f0_gaps(gone)[, c("token", "n_missing", "has_gap")])
  expect_equal(out$has_gap, c(TRUE, FALSE))
  expect_equal(out$n_missing, c(0L, 0L))            # no rows left to count
  # the same has_gap as with the rows kept
  kept <- unique(flag_f0_gaps(df)[, c("token", "has_gap")])
  expect_equal(out$has_gap, kept$has_gap)
})

test_that("rows removed: flag_f0_gaps keeps the data's own has_gap and n_missing", {
  df <- rbind(frames10(c(200, 210, NA, NA, 220, 215, 205), "gap"),
              frames10(c(200, 210, 220, 215, 205, 200), "clean"))
  flagged <- flag_f0_gaps(df)
  gone <- voiced_rows(flagged)
  out  <- flag_f0_gaps(gone)
  expect_equal(out$has_gap, gone$has_gap)
  expect_equal(out$n_missing, gone$n_missing)
  expect_equal(unique(out$n_missing[out$token == "gap"]), 2L)
  # kept even where the time steps cannot show the gap (2-decimal times)
  gone$time <- round(gone$time, 2)
  out2 <- flag_f0_gaps(gone)
  expect_equal(out2$has_gap, gone$has_gap)
  expect_equal(out2$n_missing, gone$n_missing)
  # columns read from a CSV as text still count
  txt <- gone
  txt$has_gap <- as.character(txt$has_gap)
  expect_equal(flag_f0_gaps(txt)$has_gap, gone$has_gap)
})

test_that("flag_f0_gaps recomputes tokens that still have rows without f0", {
  df <- rbind(frames10(c(200, NA, 210, 220), "a"),
              frames10(c(200, 205, 210, 220), "b"))
  df$n_missing <- 9L
  df$has_gap   <- c(rep(FALSE, 4), rep(TRUE, 4))
  out <- unique(flag_f0_gaps(df)[, c("token", "n_missing", "has_gap")])
  expect_equal(out$n_missing, c(1L, 9L))    # a: recounted; b: kept
  expect_equal(out$has_gap, c(TRUE, TRUE))  # a: recomputed; b: kept
})

test_that("flag_f0_gaps reads gaps from a voiced_run column", {
  df <- data.frame(token = rep(c("a", "b"), each = 4),
                   time  = rep(c(0.01, 0.02, 0.05, 0.06), 2),
                   f0    = 200,
                   voiced_run = c(1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L))
  out <- unique(flag_f0_gaps(df, time = "none")[, c("token", "has_gap")])
  expect_equal(out$has_gap, c(TRUE, FALSE))
  expect_false(any(flag_f0_gaps(df, time = "none", run = NULL)$has_gap))
})

test_that("without a time column only rows without f0 mark a gap", {
  # Six tokens interleaved, with token z's rows in the middle: row order is
  # no time axis, so the longer 'step' across z's rows must not read as a
  # gap in a to f. z has a real gap, a row without f0.
  tok <- c(rep(letters[1:6], 3), rep("z", 6), rep(letters[1:6], 3))
  df  <- data.frame(token = tok, f0 = 200, stringsAsFactors = FALSE)
  df$f0[which(tok == "z")[3]] <- NA
  out <- unique(flag_f0_gaps(df)[, c("token", "has_gap")])
  expect_equal(out$has_gap, c(rep(FALSE, 6), TRUE))
})

test_that("f0_long.csv: trim_to_voiced and flag_f0_gaps with the rows removed", {
  path <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(path), "test/f0_long.csv not found")
  fl <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  gone <- voiced_rows(fl)
  # trim_to_voiced: the same 7 stray edge rows (6 tokens) go either way
  tk <- trim_to_voiced(fl)
  tg <- trim_to_voiced(gone)
  expect_equal(nrow(gone) - nrow(tg), 7L)
  expect_identical(paste(tg$token, tg$time),
                   paste(tk$token, tk$time)[!is.na(tk$f0)])
  # flag_f0_gaps: F0 Extraction's default export, then 'Drop rows without f0'
  ext  <- flag_f0_gaps(tk)
  drop <- voiced_rows(ext)
  n_tok <- function(d) length(unique(d$token[d$has_gap]))
  expect_equal(n_tok(ext), 99L)
  out <- flag_f0_gaps(drop)
  expect_identical(out$has_gap, drop$has_gap)
  expect_identical(out$n_missing, drop$n_missing)
  # without the export's columns, the time steps find the same 99 tokens
  drop$has_gap <- NULL
  drop$n_missing <- NULL
  expect_setequal(unique(flag_f0_gaps(drop)$token[flag_f0_gaps(drop)$has_gap]),
                  unique(ext$token[ext$has_gap]))
  # and with every row without f0 removed, the 105 tokens with a gap inside
  expect_equal(n_tok(flag_f0_gaps(gone)), 105L)
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

test_that("a single-frame token at 0 Hz is exported as NA, not 0", {
  out <- resample_f0_equal(data.frame(token = "a", time = 1, f0 = 0), n = 5)
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$f0))
})

# ---- 0 Hz read as missing: hz_f0_columns() and zero_hz_to_na() ------------

test_that("hz_f0_columns finds f0 columns in Hz under common names", {
  hz <- c(0, 0, 210, 215, 0, 220)
  df <- data.frame(f0 = hz, F0_Hz = hz, f0_corrected = hz, f0_original = hz,
                   pitch = hz, pitch_hz = hz, F0..Hz. = hz, f0Hz = hz,
                   f_0 = hz, Hz = hz, mean_f0 = hz, check.names = FALSE)
  expect_equal(hz_f0_columns(df), names(df))
})

test_that("hz_f0_columns finds VoiceSauce's f0 columns", {
  # VoiceSauce writes 0 for unvoiced frames in each tracker's f0 column.
  hz <- c(0, 0, 210, 215, 0, 220)
  vs <- data.frame(Filename = "a.mat", Label = "ma1", seg_Start = 0, seg_End = 60,
                   t_ms = seq(0, 50, 10),
                   strF0 = hz, sF0 = hz, pF0 = hz, shrF0 = hz, oF0 = hz,
                   strF0_mean = hz, H1c = c(0, 0, 12, 14, 0, 11),
                   sF0_st = c(0, 0, -1.2, 0.4, 0, 0.8),   # normalised: kept
                   pF0_dur = c(0, 0, 120, 120, 0, 120),    # not a VoiceSauce name
                   CPP = c(0, 0, 21, 22, 0, 20), check.names = FALSE)
  expect_equal(hz_f0_columns(vs),
               c("strF0", "sF0", "pF0", "shrF0", "oF0", "strF0_mean"))
  out <- zero_hz_to_na(vs)
  expect_equal(out$sF0, c(NA, NA, 210, 215, NA, 220))
  expect_equal(out$H1c, vs$H1c)
  expect_equal(out$CPP, vs$CPP)
  expect_equal(out$t_ms, vs$t_ms)
  expect_equal(attr(out, "zero_hz"),
               c(strF0 = 3L, sF0 = 3L, pF0 = 3L, shrF0 = 3L, oF0 = 3L, strF0_mean = 3L))
  # Only those exact words: other letters before f0 still do not count.
  near <- data.frame(af0 = hz, xf0 = hz, strf0x = hz, f0s = hz, sf = hz, sf00 = hz)
  expect_equal(hz_f0_columns(near), character(0))
})

test_that("hz_f0_columns leaves normalised, time, duration and intensity columns alone", {
  set.seed(3)
  n  <- 40
  hz <- c(0, 0, runif(n - 2, 180, 260))
  df <- data.frame(
    f0            = hz,
    f0_st         = c(NA, NA, rnorm(n - 2)),            # normalised: negatives
    f0_zscore     = c(NA, NA, rnorm(n - 2)),
    f0_lz         = c(NA, NA, rnorm(n - 2)),
    speaker_mean  = 220,
    token_f0_dur  = rep(c(0, 250, 310, 420), 10),       # ms, 0 is valid
    syllable_f0_dur = rep(c(0, 0.18), 20),              # s
    f0_dur_ms     = rep(c(0, 250, 310, 420), 10),
    time          = seq(0, 1950, length.out = n),       # ms from 0
    time_ms       = seq(0, 1950, length.out = n),
    pitch_start   = rep(c(0, 35, 70, 105), 10),         # landmark seconds
    pitch_end     = rep(c(35, 70, 105, 140), 10),
    pitch_i       = rep(1:4, 10),
    pitch_tseq    = seq(0, 3, length.out = n),
    intensity     = c(0, 0, runif(n - 2, 55, 80)),      # dB
    f0_intensity  = c(0, 0, runif(n - 2, 55, 80)),
    f0_db         = c(0, 0, runif(n - 2, 55, 80)),
    f0_range      = rep(c(0, 45, 60, 80), 10),          # 0 = flat token
    f0_sd         = rep(c(0, 35, 40, 50), 10),
    F1_hz         = c(0, 0, runif(n - 2, 400, 800)),    # formant
    freq          = rep(c(0, 120, 340, 900), 10),       # word frequency
    f0_rel        = abs(rnorm(n, 5)),                   # st above the minimum
    f0_chao       = rep(c(0, 1, 3, 5), 10),             # Chao numerals
    f0_chr        = c("--undefined--", rep("210", n - 1)),
    f0_neg        = c(-1, -1, runif(n - 2, 180, 260)),  # REAPER-style code
    check.names = FALSE, stringsAsFactors = FALSE)
  expect_equal(hz_f0_columns(df), "f0")
  expect_equal(hz_f0_columns(df[0, ]), character(0))
  expect_equal(hz_f0_columns(data.frame()), character(0))
})

test_that("hz_f0_columns leaves derived f0 measures alone, where 0 is a value", {
  set.seed(4)
  n  <- 40
  hz <- c(0, 0, runif(n - 2, 180, 260))
  df <- data.frame(
    f0            = hz,
    F0_Hz_smoothed = hz,
    f0_max        = hz,
    f0_2          = hz,
    f0_peak_loc   = rep(c(0, 150, 220, 90), 10),        # ms; 0 = peak at onset
    f0_peak_location_ms = rep(c(0, 150, 220, 90), 10),
    f0_excursion  = rep(c(0, 40, 60, 90), 10),          # Hz, 0 = flat
    f0_span       = rep(c(0, 40, 60, 90), 10),
    pitch_span    = rep(c(0, 40, 60, 90), 10),
    f0_change     = rep(c(0, 40, 60, 90), 10),
    pitch_rank    = rep(c(0, 50, 100, 150), 10),
    f0_percentile = rep(c(0, 50, 75, 99), 10),
    f0_position   = rep(c(0, 150, 300, 450), 10),
    f0_step       = rep(c(0, 50, 100, 150), 10),
    f0_sample     = rep(c(0, 50, 100, 150), 10),
    f0_target_height = hz,
    check.names = FALSE)
  expect_equal(hz_f0_columns(df), c("f0", "F0_Hz_smoothed", "f0_max", "f0_2"))
  out <- zero_hz_to_na(df)
  expect_equal(out$f0_peak_loc, df$f0_peak_loc)
  expect_equal(out$f0_excursion, df$f0_excursion)
  expect_equal(out$pitch_rank, df$pitch_rank)
  expect_equal(names(attr(out, "zero_hz")), c("f0", "F0_Hz_smoothed", "f0_max", "f0_2"))
})

test_that("zero_hz_to_na sets 0 Hz to NA and counts them", {
  df <- data.frame(token = rep(c("a", "b"), each = 4),
                   time  = rep(c(0, 0.01, 0.02, 0.03), 2),
                   f0    = c(0, 210, 215, 0, 0, 120, NA, 118),
                   f0_corrected = c(0, 210, 215, 212, 130, 120, 119, 118),
                   intensity = c(0, 70, 71, 0, 0, 65, 66, 0))
  out <- zero_hz_to_na(df)
  expect_equal(out$f0, c(NA, 210, 215, NA, NA, 120, NA, 118))
  expect_equal(out$f0_corrected, c(NA, 210, 215, 212, 130, 120, 119, 118))
  expect_equal(out$intensity, df$intensity)            # 0 dB is a value
  expect_equal(out$time, df$time)
  expect_equal(attr(out, "zero_hz"), c(f0 = 3L, f0_corrected = 1L))
  # Same as reading NA in the first place
  twin <- df
  twin$f0[twin$f0 %in% 0] <- NA
  twin$f0_corrected[twin$f0_corrected %in% 0] <- NA
  expect_equal(unclass(out)[names(df)], unclass(twin)[names(df)])
})

test_that("zero_hz_to_na leaves clean, normalised and negative-coded data alone", {
  na_df <- data.frame(f0 = c(NA, 210, 215), time = c(0, 0.01, 0.02))
  out <- zero_hz_to_na(na_df)
  expect_equal(out$f0, na_df$f0)
  expect_length(attr(out, "zero_hz"), 0)
  # A legitimate 0.00 on a normalised scale is kept
  st <- data.frame(f0_st = c(-1.2, 0, 2.3), speaker_mean = 200)
  expect_equal(zero_hz_to_na(st)$f0_st, c(-1.2, 0, 2.3))
  # Negative unvoiced codes: the column is not touched at all
  neg <- data.frame(f0 = c(-1, 0, 210, 220))
  expect_equal(zero_hz_to_na(neg)$f0, c(-1, 0, 210, 220))
})

test_that("zero_hz_to_na takes columns by name and keeps the data frame's attributes", {
  df <- data.frame(F0praat = c(0L, 200L, 210L), f0 = c(0, 200, 210))
  attr(df, "encoding") <- "UTF-16BE"
  out <- zero_hz_to_na(df, cols = c("F0praat", "missing"))
  expect_equal(out$F0praat, c(NA, 200L, 210L))
  expect_equal(out$f0, c(0, 200, 210))
  expect_equal(attr(out, "zero_hz"), c(F0praat = 1L))
  expect_equal(attr(out, "encoding"), "UTF-16BE")
  expect_s3_class(zero_hz_to_na(dplyr::tibble(f0 = c(0, 200))), "tbl_df")
})

test_that("the attribute survives read_csv_utf8() then zero_hz_to_na()", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  writeLines(c("token,time,f0", "a,0,0", "a,0.01,210", "a,0.02,--undefined--"), f)
  df <- zero_hz_to_na(read_csv_utf8(f, na.strings = c("NA", "--undefined--")))
  expect_equal(df$f0, c(NA, 210, NA))
  expect_equal(attr(df, "zero_hz"), c(f0 = 1L))
  expect_equal(attr(df, "encoding"), "UTF-8")
})
