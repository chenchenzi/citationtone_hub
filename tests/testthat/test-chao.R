# ---------- classify_contour -------------------------------------------------

test_that("classify_contour names level shapes by height", {
  expect_equal(classify_contour("55"),  "high level")
  expect_equal(classify_contour("33"),  "mid level")
  expect_equal(classify_contour("11"),  "low level")
})

test_that("classify_contour names directional shapes", {
  expect_equal(classify_contour("35"), "rising")
  expect_equal(classify_contour("51"), "falling")
})

test_that("classify_contour names dipping and peaking shapes", {
  expect_equal(classify_contour("214"), "dipping")
  expect_equal(classify_contour("353"), "peaking")
})


# ---------- compute_mean_contour --------------------------------------------

test_that("compute_mean_contour returns tone × time-point grid", {
  df <- data.frame(
    token = rep(c("t1", "t2"), each = 5),
    time  = rep(seq(0, 1, length.out = 5), 2),
    f0    = c(150, 160, 170, 165, 155,
              155, 165, 175, 170, 160),
    tone  = rep("T1", 10)
  )
  out <- compute_mean_contour(df)
  expect_true(all(c("tone", "time", "f0_predicted") %in% names(out)))
  expect_equal(unique(out$tone), "T1")
})

test_that("compute_mean_contour averages per tone", {
  df <- data.frame(
    token = c("a", "a", "b", "b"),
    time  = c(0, 1, 0, 1),
    f0    = c(100, 200, 200, 300),
    tone  = c("T1", "T1", "T2", "T2")
  )
  out <- compute_mean_contour(df, n_points = 2)
  # T1 mean at time=0 should be 100, at time=1 should be 200
  expect_equal(out$f0_predicted[out$tone == "T1" & out$time == 0], 100)
  expect_equal(out$f0_predicted[out$tone == "T2" & out$time == 1], 300)
})

test_that("compute_mean_contour counts every token once at each time point", {
  # Two rising tokens of one tone, with 3 and 5 f0 samples: unequal counts,
  # so both are resampled at 21 points and both count at every point.
  df <- data.frame(
    token = c(rep("a", 3), rep("b", 5)),
    time  = c(seq(0, 1, length.out = 3), seq(0, 1, length.out = 5)),
    f0    = c(seq(100, 200, length.out = 3), seq(200, 300, length.out = 5)),
    tone  = "T1"
  )
  out <- suppressMessages(compute_mean_contour(df))
  expect_equal(nrow(out), 21)
  expect_equal(out$f0_predicted, seq(150, 250, length.out = 21))
})

test_that("compute_mean_contour keeps measured points on its grid", {
  # 11 points per token (every 10%) lie on the default 21-point grid.
  tt <- seq(0, 1, by = 0.1)
  df <- data.frame(token = rep(c("a", "b"), each = 11), time = rep(tt, 2),
                   f0 = c(100 + 50 * sin(pi * tt), 110 + 50 * sin(pi * tt)),
                   tone = "T1")
  out <- suppressMessages(compute_mean_contour(df))
  expect_equal(nrow(out), 21)
  at  <- match(round(tt, 9), round(out$time, 9))
  expect_false(anyNA(at))
  expect_equal(out$f0_predicted[at], 105 + 50 * sin(pi * tt))
})

test_that("compute_mean_contour keeps measured points on a partial span", {
  # Vowel-only subset on an already-normalised axis: 9 points at 30-70%.
  v  <- seq(0.3, 0.7, by = 0.05)
  df <- rbind(data.frame(token = "a", time = v, f0 = 150 + 100 * v, tone = "T1"),
              data.frame(token = "b", time = v, f0 = 170 + 100 * v, tone = "T1"))
  out <- compute_mean_contour(df, time_normalised = "yes")
  expect_equal(out$time, v)
  expect_equal(out$f0_predicted, 160 + 100 * v)
})

test_that("compute_mean_contour keeps the measured grid when f0 is missing at the start", {
  # Equally spaced 21-point tokens of different durations, none with f0 at
  # 0%, on a 0-1 column used as it is (time_prop): the default grid stays
  # at the measured 5% positions.
  mk <- function(tok, dur) data.frame(
    token = tok, time_prop = seq(0, dur, length.out = 21) / dur,
    f0 = c(NA, 100 + 100 * seq(0.05, 1, by = 0.05)), tone = "T1")
  df  <- rbind(mk("a", 300), mk("b", 220), mk("c", 410))
  out <- suppressMessages(compute_mean_contour(df, time = "time_prop"))
  expect_equal(out$time, seq(0.05, 1, by = 0.05))
  expect_equal(out$f0_predicted, 100 + 100 * seq(0.05, 1, by = 0.05))
})

test_that("on raw time, each token runs from its first f0 to its last", {
  # The same tokens on their raw time: the rescale starts at the first f0,
  # so the contour reaches 0, and the row without f0 changes nothing.
  mk <- function(tok, dur) data.frame(
    token = tok, time = seq(0, dur, length.out = 21),
    f0 = c(NA, 100 + 100 * seq(0.05, 1, by = 0.05)), tone = "T1")
  df  <- rbind(mk("a", 300), mk("b", 220), mk("c", 410))
  out <- compute_mean_contour(df)
  expect_equal(out$time, seq(0, 1, by = 0.05))
  expect_equal(out$f0_predicted[c(1, 21)], c(105, 200))
  expect_equal(compute_mean_contour(df[!is.na(df$f0), ]), out)
})

test_that("21-point data with no f0 in a token's first row: the time axis stretches it by 20/19, kept or removed", {
  # Two equidistant 21-point tokens, as resample_f0_equal() gives; token a
  # has no f0 at its first point. On the time column the axis runs from the
  # first f0 (0) to the last f0 (1), so a's 20 samples sit 1/19 apart, not
  # 1/20: a stretch of (N - 1) / (N - 2) = 20/19, the factor the screen
  # reports. Removing the row changes nothing. time_prop is used as it is
  # and keeps the grid, with the row or without it.
  p  <- seq(0, 1, length.out = 21)
  mk <- function(tok, tone, dur) data.frame(
    token = tok, tone = tone, time = 0.1 + dur * p, time_prop = p,
    f0 = 100 + 100 * p)
  df <- rbind(mk("a", "T1", 0.30), mk("b", "T2", 0.25))
  df$f0[1] <- NA
  gone <- df[!is.na(df$f0), ]

  ax <- normalise_time_token(df, "time", "token")$token_t01[df$token == "a"]
  expect_true(is.na(ax[1]))
  expect_equal(ax[-1], (0:19) / 19)
  st <- attr(screen_f0_rows(df), "stretch")
  expect_identical(st$n_tokens, 1L)
  expect_equal(st$factor, (1 / 19) / (1 / 20))

  out <- compute_mean_contour(df)
  expect_equal(compute_mean_contour(gone), out)
  a <- out[out$tone == "T1", ]
  expect_equal(a$time, p)
  expect_equal(a$f0_predicted, 105 + 95 * p)      # 105 Hz at 0, 200 Hz at 1
  expect_equal(a$f0_predicted[11], 152.5)         # 150 Hz sits at 9/19, not 0.5
  expect_equal(out$f0_predicted[out$tone == "T2"], 100 + 100 * p)

  tp <- suppressMessages(compute_mean_contour(df, time = "time_prop"))
  expect_equal(suppressMessages(compute_mean_contour(gone, time = "time_prop")), tp)
  a_tp <- tp[tp$tone == "T1", ]
  expect_equal(a_tp$time, p[-1])
  expect_equal(a_tp$f0_predicted, 100 + 100 * p[-1])

  # Timing: an even rise on the time column; on time_prop the contour starts
  # at 0.05, its onset is held flat from 105 Hz, and 50% of the axis is
  # 9/19 of the way from onset to offset.
  ch <- contour_to_chao(out)
  expect_equal(ch$progress[ch$tone == "T1"], 0.5)
  expect_equal(ch$t_first[ch$tone == "T1"], 0)
  ch_tp <- contour_to_chao(tp)
  expect_equal(ch_tp$t_first[ch_tp$tone == "T1"], 0.05)
  expect_equal(ch_tp$progress[ch_tp$tone == "T1"], 9 / 19)
})

test_that("compute_mean_contour keeps an end point missed only by rounding", {
  # No f0 at 100% in any token, on time_prop: grid point 20 of 21 is a hair
  # above the tokens' last time, 19/20, but the measured 95% point must stay.
  mk <- function(tok, dur) data.frame(
    token = tok, time_prop = seq(0, dur, length.out = 21) / dur,
    f0 = c(100 + 100 * seq(0, 0.95, by = 0.05), NA), tone = "T1")
  df  <- rbind(mk("a", 300), mk("b", 220), mk("c", 410))
  out <- suppressMessages(compute_mean_contour(df, time = "time_prop"))
  expect_equal(nrow(out), 20)
  expect_equal(max(out$time), 0.95)
  expect_equal(out$f0_predicted[20], 195)
})

test_that("compute_mean_contour keeps an end point when times are rounded", {
  # Times stored to the millisecond, no f0 at 0%, on a column used as it is:
  # every token's measured 5% point lands a hair off the grid, but all of
  # them must count there.
  mk <- function(tok, dur, base) data.frame(
    token = tok, tp = round(seq(0, dur, length.out = 21), 3) / dur,
    f0 = c(NA, base + 1:20), tone = "T1")
  df  <- rbind(mk("a", 0.287, 100), mk("b", 0.293, 200), mk("c", 0.311, 300))
  out <- suppressMessages(compute_mean_contour(df, time = "tp", time_normalised = "yes"))
  expect_equal(out$time[1], 0.05)
  expect_lt(abs(out$f0_predicted[1] - 201), 0.1)
})

test_that("compute_mean_contour drops tokens with fewer than two f0 values", {
  df <- data.frame(token = c("a", "a", "b"), time = c(0, 1, 0.5),
                   f0 = c(100, 200, 150), tone = "T1")
  expect_message(out <- compute_mean_contour(df, time_normalised = "no"),
                 "Dropped 1 token")
  expect_equal(out$f0_predicted, seq(100, 200, length.out = 21))
})

test_that("compute_mean_contour warns that n_bins is deprecated", {
  df <- data.frame(token = "a", time = c(0, 0.5, 1), f0 = c(100, 150, 200),
                   tone = "T1")
  expect_warning(out <- suppressMessages(compute_mean_contour(df, n_bins = 5)),
                 "deprecated")
  expect_equal(nrow(out), 5)
})


test_that("compute_mean_contour takes non-ASCII tone labels in the native encoding", {
  # read.csv() leaves Chinese tone names unmarked ("unknown") in a UTF-8
  # locale, and a radix sort of such labels used to stop with an error.
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]))
  lab <- c("\u9634\u5e73", "\u9633\u5e73")
  Encoding(lab) <- "unknown"
  df <- data.frame(token = rep(c("a", "b"), each = 3), time = rep(c(0, 0.5, 1), 2),
                   f0 = c(200, 210, 220, 180, 170, 200), tone = rep(lab, each = 3))
  out <- compute_mean_contour(df, n_points = 3, time_normalised = "yes")
  expect_equal(nrow(out), 6)
  expect_setequal(unique(out$tone), lab)
})


# ---------- chao_to_letters -------------------------------------------------

test_that("chao_to_letters writes one tone letter per digit", {
  expect_equal(chao_to_letters(c("55", "35", "214", "51")),
               c("\u02e5\u02e5", "\u02e7\u02e5", "\u02e8\u02e9\u02e6", "\u02e5\u02e9"))
  expect_equal(chao_to_letters(13L), "\u02e9\u02e7")
})

test_that("chao_to_letters keeps NA and rejects non-numerals", {
  expect_equal(chao_to_letters(c("33", NA)), c("\u02e7\u02e7", NA))
  expect_warning(out <- chao_to_letters(c("35", "06", "T1")), "06, T1")
  expect_equal(out, c("\u02e7\u02e5", NA, NA))
  expect_identical(chao_to_letters(character()), character())
})


# ---------- contour_to_chao -------------------------------------------------

test_that("contour_to_chao returns one row per tone with refline + interval", {
  contour <- data.frame(
    tone         = rep(c("T1", "T2"), each = 11),
    time         = rep(seq(0, 1, length.out = 11), 2),
    f0_predicted = c(rep(200, 11),       # flat high
                     seq(100, 200, length.out = 11))   # rising
  )
  out <- contour_to_chao(contour)
  expect_equal(nrow(out), 2)
  expect_true(all(c("tone", "n_digits", "refline", "interval",
                    "shape") %in% names(out)))
  expect_false(any(c("robust", "rob_continuous") %in% names(out)))
  expect_null(attr(out, "robust_stats"))
})

test_that("contour_to_chao detects rising and falling shapes", {
  contour <- data.frame(
    tone         = c(rep("rise", 11), rep("fall", 11)),
    time         = rep(seq(0, 1, length.out = 11), 2),
    f0_predicted = c(seq(100, 200, length.out = 11),
                     seq(200, 100, length.out = 11))
  )
  out <- contour_to_chao(contour)
  expect_equal(out$shape[out$tone == "rise"], "rising")
  expect_equal(out$shape[out$tone == "fall"], "falling")
})

test_that("contour_to_chao detects a dipping shape with 3 digits", {
  # V-shaped contour: starts high, dips deeply, returns high
  contour <- data.frame(
    tone         = rep("dip", 21),
    time         = seq(0, 1, length.out = 21),
    f0_predicted = c(seq(200, 100, length.out = 11),
                     seq(110, 200, length.out = 10))
  )
  out <- contour_to_chao(contour)
  expect_equal(out$shape, "dipping")
  expect_equal(out$n_digits, 3)
})

test_that("contour_to_chao reports dip and peak sizes, and the threshold sets the digit count", {
  # A shallow dip: from 3 down to about 2.6 and back up to 3.4 (reference-line
  # scale), plus a high level tone that sets the top of the range.
  tt <- seq(0, 1, length.out = 21)
  contour <- data.frame(
    tone = rep(c("dip", "hi"), each = 21),
    time = rep(tt, 2),
    f0_predicted = c(100 + 50 * ((tt - 0.5)^2 * 4) - 20 + 20 * tt,   # dips mid-way
                     rep(200, 21))
  )
  base <- contour_to_chao(contour)
  d <- base$dip[base$tone == "dip"]
  expect_true(d > 0)
  expect_equal(attr(base, "threshold"), 0.5)
  # The dip adds a middle digit only when it exceeds the threshold
  below <- contour_to_chao(contour, threshold = d - 0.01)
  above <- contour_to_chao(contour, threshold = d + 0.01)
  expect_equal(below$n_digits[below$tone == "dip"], 3)
  expect_equal(above$n_digits[above$tone == "dip"], 2)
  # A level tone has no dip or peak beyond its endpoints
  expect_lte(base$peak[base$tone == "hi"], 0)
  expect_lte(base$dip[base$tone == "hi"], 0)
  expect_error(contour_to_chao(contour, threshold = -1), "threshold")
  expect_error(contour_to_chao(contour, threshold = c(0.3, 0.5)), "threshold")
})

test_that("contour_to_chao keeps a middle digit only where it shows the turn", {
  # Piecewise-linear contours given directly on the reference-line scale: a
  # low and a high level tone pin the range to f0 1..5, so chao_ref = f0 and
  # the interval value is (f0 - 1) * 1.25.
  tt <- seq(0, 1, length.out = 21)
  shape3 <- function(name, a, b, c)
    data.frame(tone = name, time = tt,
               f0_predicted = stats::approx(c(0, 0.5, 1), c(a, b, c), xout = tt)$y)
  contour <- rbind(
    shape3("flat_dip", 2.45, 1.90, 2.45),   # dip 0.55: 2 2 2 on both scales
    shape3("late_rise", 2.45, 1.90, 3.00),  # dip 0.55: 2 2 3 on both scales
    shape3("split", 2.45, 1.75, 2.45),      # dip 0.70: 2 2 2 (ref), 2 1 2 (int)
    shape3("deep_dip", 3.00, 1.40, 4.00),   # dip 1.60: 3 1 4 (ref), 3 1 4 (int)
    shape3("flat_peak", 3.55, 4.20, 3.55),  # peak 0.65: 4 4 4 on both scales
    shape3("peak", 2.00, 3.60, 2.00),       # peak 1.60: 2 4 2 on both scales
    data.frame(tone = "lo", time = tt, f0_predicted = 1),
    data.frame(tone = "hi", time = tt, f0_predicted = 5)
  )
  out <- contour_to_chao(contour)
  row <- function(tn) out[out$tone == tn, ]

  # Turn sampled (dip > 0.5) but the middle digit matches an endpoint: two digits
  expect_equal(row("flat_dip")$refline, "22")
  expect_equal(row("flat_dip")$interval, "22")
  expect_length(row("flat_dip")$sample_times[[1]], 3)
  expect_equal(row("late_rise")$refline, "23")
  expect_equal(row("late_rise")$interval, "23")
  expect_equal(row("late_rise")$shape, "rising")
  # The scales round differently, so they can disagree on the digit count
  expect_equal(row("split")$refline, "22")
  expect_equal(row("split")$interval, "212")
  expect_equal(row("split")$n_digits, 2)
  expect_equal(row("split")$n_digits_int, 3)
  # A middle digit below (above) both endpoint digits stays
  expect_equal(row("deep_dip")$refline, "314")
  expect_equal(row("deep_dip")$n_digits, 3)
  expect_equal(row("flat_peak")$refline, "44")
  expect_equal(row("flat_peak")$interval, "44")
  expect_equal(row("peak")$refline, "242")
  expect_equal(row("peak")$interval, "242")
  # Level tones: two digits and no sampled turn
  expect_equal(row("lo")$refline, "11")
  expect_length(row("lo")$sample_times[[1]], 2)
})

test_that("contour_to_chao samples the larger of a dip and a peak", {
  tt <- seq(0, 1, length.out = 21)
  shape4 <- function(name, v)
    data.frame(tone = name, time = tt,
               f0_predicted = stats::approx(c(0, 0.25, 0.75, 1), v, xout = tt)$y)
  contour <- rbind(
    shape4("big_peak", c(3.4, 2.85, 4.8, 3.4)),   # dip 0.55 (rounds away), peak 1.4
    shape4("both", c(3.0, 2.6, 4.0, 3.2)),        # dip 0.4, peak 0.8
    data.frame(tone = "lo", time = tt, f0_predicted = 1),
    data.frame(tone = "hi", time = tt, f0_predicted = 5)
  )
  out <- contour_to_chao(contour)
  # The peak, not the shallow dip, is sampled: a rise-fall, not a level tone
  expect_equal(out$refline[out$tone == "big_peak"], "353")
  expect_equal(out$shape[out$tone == "big_peak"], "peaking")
  # Lowering the threshold never removes a digit
  ths <- c(0, 0.2, 0.3, 0.39, 0.45, 0.5, 0.79, 0.81, 1, 4)
  n_ref <- vapply(ths, function(th) {
    o <- contour_to_chao(contour, threshold = th); o$n_digits[o$tone == "both"] }, 1)
  n_int <- vapply(ths, function(th) {
    o <- contour_to_chao(contour, threshold = th); o$n_digits_int[o$tone == "both"] }, 1)
  expect_true(all(diff(n_ref) <= 0) && all(diff(n_int) <= 0))
  expect_equal(n_ref[ths == 0.3], 3)
  expect_equal(n_ref[ths == 1], 2)
})

test_that("contour_to_chao times rises and falls by the share done at 50%", {
  # Piecewise-linear contours on the reference-line scale: a low and a high
  # level tone pin the range to f0 1..5, so chao_ref = f0.
  tt <- seq(0, 1, length.out = 21)
  shape <- function(name, tk, v)
    data.frame(tone = name, time = tt,
               f0_predicted = stats::approx(tk, v, xout = tt)$y)
  contour <- rbind(
    shape("early_rise", c(0, 0.5, 1), c(2, 4, 4)),
    shape("even_rise", c(0, 1), c(2, 4)),
    shape("late_rise", c(0, 0.5, 1), c(2, 2, 4)),
    shape("late_fall", c(0, 0.5, 1), c(4, 4, 2)),
    shape("early_fall", c(0, 0.5, 1), c(4, 2, 2)),
    shape("level_third", c(0, 0.34, 1), c(2, 2, 4)),   # level for over a third
    shape("level_less", c(0, 0.32, 1), c(2, 2, 4)),    # level for under a third
    shape("small", c(0, 1), c(3, 3.4)),                # moves 0.4: too small to time
    shape("dip", c(0, 0.5, 1), c(2, 1, 4)),            # sampled turn: 214
    data.frame(tone = "lo", time = tt, f0_predicted = 1),
    data.frame(tone = "hi", time = tt, f0_predicted = 5)
  )
  out <- contour_to_chao(contour)
  row <- function(tn) out[out$tone == tn, ]

  # The digits are the same whatever the timing; the timing columns differ
  rises <- c("early_rise", "even_rise", "late_rise")
  expect_equal(unique(out$refline[out$tone %in% rises]), "24")
  expect_equal(out$progress[match(rises, out$tone)], c(1, 0.5, 0))
  expect_equal(out$timing[match(rises, out$tone)], c("early", "mid", "late"))
  expect_equal(row("late_fall")$move, -2)
  expect_equal(row("late_fall")$timing, "late")
  expect_equal(row("early_fall")$timing, "early")
  # A level start followed by a straight movement is late beyond a third
  expect_equal(row("level_third")$timing, "late")
  expect_equal(row("level_less")$timing, "mid")
  # No timing for a small movement, a sampled turn or a level tone
  expect_equal(row("small")$move, 0.4)
  expect_true(is.na(row("small")$progress) && is.na(row("small")$timing))
  expect_equal(row("dip")$refline, "214")
  expect_true(is.na(row("dip")$progress) && is.na(row("dip")$timing))
  expect_true(is.na(row("lo")$timing))
})

test_that("contour_to_chao takes its range from the contours' lowest and highest points", {
  contour <- data.frame(
    tone         = rep(c("lo", "hi"), each = 11),
    time         = rep(seq(0, 1, length.out = 11), 2),
    f0_predicted = c(rep(100, 11), rep(200, 11))
  )
  out <- contour_to_chao(contour)
  expect_equal(attr(out, "f0_min"), 100)
  expect_equal(attr(out, "f0_max"), 200)
  expect_equal(out$refline,  c("11", "55"))
  expect_equal(out$interval, c("11", "55"))
})

test_that("contour_to_chao is linear in the units of the input", {
  contour <- data.frame(
    tone         = rep(c("lo", "mid", "hi"), each = 11),
    time         = rep(seq(0, 1, length.out = 11), 3),
    f0_predicted = rep(c(120, 140, 180), each = 11)
  )
  out <- contour_to_chao(contour)
  mid <- which(out$tone == "mid")
  # 140 Hz is a third of the way up 120-180 Hz; a log scale would put it lower.
  expect_equal(out$int_continuous[[mid]][1], (140 - 120) / (180 - 120) * 5)
  expect_equal(out$interval[mid], "22")
})

test_that("contour_to_chao handles negative (normalised) f0", {
  contour <- data.frame(
    tone         = rep(c("lo", "hi"), each = 11),
    time         = rep(seq(0, 1, length.out = 11), 2),
    f0_predicted = c(rep(-4, 11), rep(4, 11))
  )
  out <- contour_to_chao(contour)
  expect_equal(out$refline,  c("11", "55"))
  expect_equal(out$interval, c("11", "55"))
})

test_that("contour_to_chao runs on normalise_f0() output", {
  data(sample_f0, package = "shinytone", envir = environment())
  for (m in c("semitone", "zscore", "lz")) {
    n   <- normalise_f0(sample_f0, f0 = "f0_Hz", speaker = "speaker",
                        tone = "tone", method = m)
    col <- c(semitone = "f0_st", zscore = "f0_zscore", lz = "f0_lz")[[m]]
    mc  <- compute_mean_contour(n, token = "token", f0 = col,
                                time = "time", tone = "tone")
    out <- contour_to_chao(mc)
    expect_true(all(grepl("^[1-5]{2,3}$", c(out$refline, out$interval))))
  }
})

test_that("contour_to_chao warns that the raw_* arguments are deprecated", {
  contour <- data.frame(tone = rep("T1", 11), time = seq(0, 1, length.out = 11),
                        f0_predicted = seq(120, 180, length.out = 11))
  raw <- data.frame(token = "a", f0 = 150, tone = "T1")
  expect_warning(out <- contour_to_chao(contour, raw_data = raw), "deprecated")
  expect_identical(out, contour_to_chao(contour))
  expect_warning(contour_to_chao(contour, raw_f0 = "f0"), "deprecated")
})

test_that("contour_to_chao errors on missing columns", {
  contour <- data.frame(tone = "T1", time = 0, f0_predicted = 100)
  expect_error(contour_to_chao(contour, tone_col = "nonexistent"),
               "Column.*not found")
})

# Recovered: tests appended to tests/testthat/test-chao.R (after the last test)

# Four straight contours on a given time axis, for the time-axis checks.
lo_hi <- function(tt) rbind(
  data.frame(tone = "rise", time = tt, f0_predicted = 2 + 2 * tt),
  data.frame(tone = "fall", time = tt, f0_predicted = 4 - 2 * tt),
  data.frame(tone = "lo",   time = tt, f0_predicted = 1),
  data.frame(tone = "hi",   time = tt, f0_predicted = 5))

test_that("contour_to_chao refuses a time axis that is not [0, 1]", {
  tt <- seq(0, 1, length.out = 21)
  for (f in list(function(t) t * 300,         # ms
                 function(t) t * 100,         # percent
                 function(t) 1 + t * 20,      # point number 1 to 21
                 function(t) 1 + t,           # second segment of a sequential axis
                 function(t) 10 + t * 0.3)) { # seconds from the start of the file
    expect_error(contour_to_chao(lo_hi(f(tt))), "normalised time")
  }
  d <- lo_hi(tt)
  d$time[d$tone == "rise" & d$time == 1] <- Inf
  expect_error(contour_to_chao(d), "normalised time")
})

test_that("contour_to_chao refuses contours across several segments", {
  d <- structure(lo_hi(seq(0, 2, length.out = 41)),
                 time_axis = "sequential", n_segments = 2L)
  expect_error(contour_to_chao(d), "2 segments")
})

test_that("contour_to_chao warns when contours stop short of the axis", {
  tt <- seq(0, 1, length.out = 21)
  expect_warning(contour_to_chao(lo_hi(tt * 0.3)), "stop short")   # seconds
  # Not for a dropped end point, a late start, or rounding at an end.
  expect_no_warning(contour_to_chao(lo_hi(seq(0.05, 1, by = 0.05))))
  expect_no_warning(contour_to_chao(lo_hi(seq(0.2, 1, length.out = 50))))
  expect_no_warning(contour_to_chao(lo_hi(tt + c(rep(0, 20), 1e-12))))
})

test_that("contour_to_chao checks the lower end of the axis and late starts", {
  tt <- seq(0, 1, length.out = 21)
  expect_error(contour_to_chao(lo_hi(tt - 0.1)), "normalised time")
  expect_warning(contour_to_chao(lo_hi(0.3 + 0.7 * tt)), "stop short")
})

test_that("a tone left out for too few time points does not set the range", {
  d <- rbind(lo_hi(seq(0, 1, length.out = 21)),
             data.frame(tone = "dot", time = 0.5, f0_predicted = 9))
  expect_warning(out <- contour_to_chao(d), "fewer than two time points: dot")
  expect_equal(attr(out, "f0_max"), 5)
  expect_equal(out$refline, c("24", "42", "11", "55"))
  one <- data.frame(tone = c("a", "b"), time = 0.5, f0_predicted = c(1, 2))
  expect_error(contour_to_chao(one), "No tone has f0 at two or more time points")
})

test_that("contour_to_chao reads a factor time column by value", {
  d <- lo_hi(seq(0, 1, length.out = 21))
  f <- d
  f$time <- factor(f$time)
  expect_equal(contour_to_chao(f)$refline, contour_to_chao(d)$refline)
  expect_equal(contour_to_chao(d)$refline, c("24", "42", "11", "55"))
})

test_that("rows on no contour do not set the range", {
  d <- lo_hi(seq(0, 1, length.out = 21))
  extra <- rbind(d, data.frame(tone = "rise", time = NA, f0_predicted = 9),
                 data.frame(tone = NA, time = 0.5, f0_predicted = -3))
  out <- contour_to_chao(extra)
  expect_equal(attr(out, "f0_max"), 5)
  expect_equal(attr(out, "f0_min"), 1)
  expect_equal(out$refline, contour_to_chao(d)$refline)
})

test_that("contour_to_chao leaves out a tone with fewer than two time points", {
  d <- rbind(lo_hi(seq(0, 1, length.out = 21)),
             data.frame(tone = "dot", time = c(0.5, 0.5), f0_predicted = c(3, 3.1)))
  expect_warning(out <- contour_to_chao(d), "fewer than two time points")
  expect_false("dot" %in% out$tone)
  d2 <- lo_hi(seq(0, 1, length.out = 21))
  d2$time[d2$tone == "rise"] <- NA
  expect_warning(out2 <- contour_to_chao(d2), "fewer than two time points: rise")
  expect_equal(out2$tone, c("fall", "lo", "hi"))
  d$time <- NA_real_
  expect_error(contour_to_chao(d), "No contour has f0")
})

# ---------- where each contour starts and stops (t_first, t_last) ------------

test_that("contour_to_chao reports where a contour spanning only 0.1-0.9 starts and stops", {
  # The mean contours of tokens whose edge rows have no f0 cover only part of
  # the 0-1 axis. The onset and offset are then held flat from the first and
  # last points, and t_first and t_last say where each contour starts and stops.
  part <- lo_hi(seq(0.1, 0.9, length.out = 17))
  expect_no_warning(out <- contour_to_chao(part))   # within 0.25 and 0.75
  expect_equal(out$t_first, rep(0.1, 4))
  expect_equal(out$t_last,  rep(0.9, 4))
  rise <- out[out$tone == "rise", ]
  # f0_min 1 and f0_max 5 make the reference-line scale equal to f0, so the
  # onset and offset are the values at 0.1 and 0.9, not at 0 and 1.
  expect_equal(rise$ref_continuous[[1]], c(2.2, 3.8))
  expect_equal(rise$refline, "24")
  expect_equal(rise$move, 1.6)
  expect_equal(rise$progress, 0.5)
  expect_equal(rise$timing, "mid")

  full <- contour_to_chao(lo_hi(seq(0, 1, length.out = 21)))
  expect_equal(full$t_first, rep(0, 4))
  expect_equal(full$t_last,  rep(1, 4))
  expect_equal(full$move[full$tone == "rise"], 2)
  expect_equal(names(full)[names(full) %in% c("timing", "t_first", "t_last")],
               c("timing", "t_first", "t_last"))

  # Per tone, and a row with no f0 at 0 or 1 does not count as reaching it.
  mixed <- rbind(lo_hi(seq(0, 1, length.out = 21))[1:21, ],
                 part[part$tone == "fall", ],
                 data.frame(tone = "fall", time = c(0, 1), f0_predicted = NA))
  m <- contour_to_chao(mixed)
  expect_equal(m$tone, c("rise", "fall"))
  expect_equal(m$t_first, c(0, 0.1))
  expect_equal(m$t_last,  c(1, 0.9))
})

test_that("Timing of a contour that misses 0 or 1: read at 50% of the axis, its ends held flat", {
  # A rise with f0 from 0.2 and a fall with f0 up to 0.8, as on a time_prop
  # column with no f0 at one end. lo and hi pin the reference-line scale to
  # f0, so the values are Chao units.
  rise <- data.frame(tone = "rise", time = seq(0.2, 1, by = 0.05))
  rise$f0_predicted <- 2 + 2 * (rise$time - 0.2) / 0.8
  fall <- data.frame(tone = "fall", time = seq(0, 0.8, by = 0.05))
  fall$f0_predicted <- 4 - 2 * fall$time / 0.8
  tt <- seq(0, 1, by = 0.05)
  d <- rbind(rise, fall, data.frame(tone = "lo", time = tt, f0_predicted = 1),
             data.frame(tone = "hi", time = tt, f0_predicted = 5))
  expect_no_warning(out <- contour_to_chao(d))   # starts by 0.25, ends from 0.75
  r <- out[out$tone == "rise", ]
  f <- out[out$tone == "fall", ]
  expect_equal(c(r$t_first, r$t_last), c(0.2, 1))
  expect_equal(c(f$t_first, f$t_last), c(0, 0.8))
  # The onset and offset are the first and last values, held flat, not
  # extended along the slope (which would give 1.5 and 1.5)
  expect_equal(r$ref_continuous[[1]], c(2, 4))
  expect_equal(f$ref_continuous[[1]], c(4, 2))
  # ... and on the interval scale too, so no end reads NA
  expect_equal(c(r$interval, f$interval), c("24", "42"))
  expect_equal(r$int_continuous[[1]], c(1.25, 3.75))
  expect_equal(f$int_continuous[[1]], c(3.75, 1.25))
  expect_equal(c(r$move, f$move), c(2, -2))
  # 50% of the axis is 0.3/0.8 of the way through the rise and 0.5/0.8
  # through the fall. Read at the contour's own middle, both would be 0.5.
  expect_equal(r$progress, 0.375)
  expect_equal(f$progress, 0.625)
  expect_equal(c(r$timing, f$timing), c("mid", "mid"))

  # A rise with f0 only from 0.4 is late at 50% of the axis, with the
  # stop-short warning: 0.1/0.6 of it is done.
  late <- d[d$tone != "rise", ]
  r4 <- data.frame(tone = "rise", time = seq(0.4, 1, by = 0.05))
  r4$f0_predicted <- 2 + 2 * (r4$time - 0.4) / 0.6
  expect_warning(out4 <- contour_to_chao(rbind(r4, late)), "stop short")
  expect_equal(out4$t_first[out4$tone == "rise"], 0.4)
  expect_equal(out4$progress[out4$tone == "rise"], 1 / 6)
  expect_equal(out4$timing[out4$tone == "rise"], "late")
})

test_that("mean contours reach 0 and 1 with or without rows without f0 at the edges", {
  # Each token's first and last rows have no f0. The axis runs from its
  # first f0 to its last, so the mean contours reach 0 and 1 either way.
  mk <- function(tok, dur, tone, slope) data.frame(
    token = tok, time = seq(0, dur, length.out = 11), tone = tone,
    f0 = c(NA, 150 + slope * seq(0.1, 0.9, by = 0.1), NA))
  raw <- rbind(mk("a", 0.30, "rise", 50), mk("b", 0.25, "rise", 50),
               mk("c", 0.32, "fall", -50), mk("d", 0.28, "fall", -50))
  out_raw <- contour_to_chao(compute_mean_contour(raw, n_points = 11))
  expect_equal(out_raw$t_first, c(0, 0))
  expect_equal(out_raw$t_last,  c(1, 1))
  kept <- raw[!is.na(raw$f0), ]
  expect_equal(compute_mean_contour(kept, n_points = 11),
               compute_mean_contour(raw, n_points = 11))
  # On a 0-1 column built over every row and used as it is, the f0 still
  # covers only 0.1 to 0.9 of the axis.
  raw$time_prop <- ave(raw$time, raw$token, FUN = function(t) (t - min(t)) / diff(range(t)))
  out_tp <- contour_to_chao(suppressMessages(
    compute_mean_contour(raw, time = "time_prop", n_points = 11)))
  expect_equal(out_tp$t_first, c(0.1, 0.1))
  expect_equal(out_tp$t_last,  c(0.9, 0.9))
})

test_that("a stacked axis is never averaged across a syllable boundary", {
  # T4 + T1 disyllables: syllable 1 falls from 250 to about 150 Hz, a
  # voiceless closure follows, and syllable 2 is level at 240 Hz. On
  # syllable_tseq, syllable 1's last f0 and syllable 2's first f0 are both
  # at 1, so a blend of the two (about 195 Hz) would be a value neither
  # syllable has.
  mk <- function(tok) {
    t  <- seq(0, 0.40, by = 0.01)
    s1 <- t < 0.20
    f0 <- ifelse(s1, 250 - 100 * (t / 0.17), 240 + 2 * (t - 0.2))
    f0[t > 0.17 & t < 0.23] <- NA
    data.frame(token = tok, time = t, f0 = f0, tone = "T4T1",
               syllable_start = ifelse(s1, 0, 0.20),
               syllable_end = ifelse(s1, 0.20, 0.41),
               syllable_i = ifelse(s1, 1L, 2L))
  }
  d <- normalise_time_landmarks(do.call(rbind, lapply(paste0("w", 1:3), mk)),
                                "time", "syllable")
  at1 <- d[which(d$syllable_tseq == 1), ]
  expect_equal(nrow(at1), 6L)                     # two rows per token at 1
  s1_last  <- at1$f0[at1$syllable_i == 1][1]      # 150
  s2_first <- at1$f0[at1$syllable_i == 2][1]      # 240.06
  # The tseq runs 0 to 2; rescaled per token, the boundary is grid point 0.5
  mc <- compute_mean_contour(d, time = "syllable_tseq", n_points = 21)
  expect_equal(mc$f0_predicted[mc$time == 0.5], s2_first)
  # Just before the boundary, only syllable 1: 0.475 lies between its
  # points at tseq 16/17 and 1
  mc41 <- compute_mean_contour(d, time = "syllable_tseq", n_points = 41)
  s1 <- d[d$syllable_i == 1 & !is.na(d$syllable_tseq) & d$token == "w1", ]
  expect_equal(mc41$f0_predicted[which.min(abs(mc41$time - 0.475))],
               stats::approx(s1$syllable_tseq / 2, s1$f0, xout = 0.475)$y)
  # The syllable is read from syllable_t01 or syllable_i, so the row order
  # does not matter: tokens reversed, rows reversed or shuffled
  rev_tok <- d[order(-as.integer(factor(d$token)), d$time), ]
  expect_equal(compute_mean_contour(rev_tok, time = "syllable_tseq", n_points = 21), mc)
  expect_equal(compute_mean_contour(d[nrow(d):1, ], time = "syllable_tseq", n_points = 21), mc)
  set.seed(5)
  shuf <- d[sample(nrow(d)), ]
  expect_equal(compute_mean_contour(shuf, time = "syllable_tseq", n_points = 21), mc)
  expect_equal(compute_mean_contour(shuf, time = "syllable_tseq", n_points = 41), mc41)
  # With syllable_i alone, and with neither column (rows in time order)
  no_t01 <- shuf; no_t01$syllable_t01 <- NULL
  expect_equal(compute_mean_contour(no_t01, time = "syllable_tseq", n_points = 21), mc)
  bare <- d; bare$syllable_t01 <- NULL; bare$syllable_i <- NULL
  expect_equal(compute_mean_contour(bare, time = "syllable_tseq", n_points = 21), mc)
  # No message about averaged rows: the two rows at 1 are two syllables
  expect_no_message(compute_mean_contour(shuf, time = "syllable_tseq", n_points = 21))
})

test_that("compute_mean_contour on a <tier>_tbound: boundaries aligned, gap kept", {
  # T4 + T1 disyllables whose boundary and voiceless gap differ by token
  mk <- function(tok, b, gap) {
    t  <- seq(0, 0.40, by = 0.01)
    s1 <- t < b - 1e-9
    f0 <- ifelse(s1, 250 - 100 * (t / b), 240 + 20 * (t - b))
    f0[t > gap[1] + 1e-9 & t < gap[2] - 1e-9] <- NA
    data.frame(token = tok, time = t, f0 = f0, tone = "T4T1",
               syllable_start = ifelse(s1, 0, b), syllable_end = ifelse(s1, b, 0.41),
               syllable_i = ifelse(s1, 1L, 2L))
  }
  d <- normalise_time_landmarks(rbind(mk("w1", 0.20, c(0.17, 0.23)),
                                      mk("w2", 0.22, c(0.19, 0.27)),
                                      mk("w3", 0.18, c(0.16, 0.20))),
                                "time", "syllable")
  # Each token runs 0 to 2 with its boundary at 1, so rescaled per token the
  # boundary is grid point 0.5 in every token
  expect_equal(as.vector(tapply(d$syllable_tbound, d$token, max, na.rm = TRUE)), c(2, 2, 2))
  mc <- compute_mean_contour(d, time = "syllable_tbound", n_points = 21)
  grid <- seq(0, 1, length.out = 21)
  want <- rowMeans(sapply(split(d, d$token), function(x) {
    x <- x[!is.na(x$f0) & !is.na(x$syllable_tbound), ]
    stats::approx(x$syllable_tbound / 2, x$f0, xout = grid)$y
  }))
  expect_equal(mc$time, grid)
  expect_equal(mc$f0_predicted, want)
  # At the boundary the curve runs across the gap, between syllable 1's
  # last f0 and syllable 2's first, rather than jumping
  s1_last  <- mean(tapply(d$f0[d$syllable_i == 1], d$token[d$syllable_i == 1], function(f) f[max(which(!is.na(f)))]))
  s2_first <- mean(tapply(d$f0[d$syllable_i == 2], d$token[d$syllable_i == 2], function(f) f[min(which(!is.na(f)))]))
  at_b <- mc$f0_predicted[mc$time == 0.5]
  expect_true(at_b > s1_last && at_b < s2_first)
  # The same without the rows without f0, and in any row order
  expect_equal(compute_mean_contour(d[!is.na(d$f0), ], time = "syllable_tbound", n_points = 21), mc)
  set.seed(7)
  expect_no_message(sh <- compute_mean_contour(d[sample(nrow(d)), ], time = "syllable_tbound",
                                               n_points = 21))
  expect_equal(sh, mc)
})

test_that("two recordings that share a token name are averaged, not stepped", {
  # The same name on two 21-point recordings, one rising and one falling.
  # Every time is tied; a step at each one would keep only one recording,
  # or zigzag between the two.
  tp <- seq(0, 1, by = 0.05)
  d <- rbind(data.frame(token = "dup", time_prop = tp, f0 = 200 + 40 * tp, tone = "T2"),
             data.frame(token = "dup", time_prop = tp, f0 = 240 - 40 * tp, tone = "T2"))
  for (n in c(11, 21, 41)) {
    expect_message(mc <- compute_mean_contour(d, time = "time_prop", n_points = n),
                   "1 token(s) (dup), two or more rows have the same time",
                   fixed = TRUE)
    expect_equal(mc$f0_predicted, rep(220, n))
  }
  # On frames, too: 200 Hz and 100 Hz on the same times average to 150.
  fr <- seq(0, 0.2, by = 0.01)
  dd <- rbind(data.frame(token = "dup", time = fr, f0 = 200, tone = "T1"),
              data.frame(token = "dup", time = fr, f0 = 100, tone = "T1"))
  for (n in c(21, 30)) {
    mc <- suppressMessages(compute_mean_contour(dd, n_points = n))
    expect_equal(mc$f0_predicted, rep(150, n))
  }
  # Two recordings on a stacked axis: each syllable is averaged, and the
  # boundary still takes the next syllable's first f0.
  mk <- function(tok, s2) {
    t  <- seq(0, 0.40, by = 0.01)
    s1 <- t < 0.20
    f0 <- ifelse(s1, 250 - 100 * (t / 0.17), s2)
    f0[t > 0.17 & t < 0.23] <- NA
    data.frame(token = tok, time = t, f0 = f0, tone = "T4T1",
               syllable_start = ifelse(s1, 0, 0.20),
               syllable_end = ifelse(s1, 0.20, 0.41),
               syllable_i = ifelse(s1, 1L, 2L))
  }
  st <- normalise_time_landmarks(rbind(mk("w", 240), mk("w", 230)), "time", "syllable")
  ms <- suppressMessages(compute_mean_contour(st, time = "syllable_tseq", n_points = 21))
  expect_equal(ms$f0_predicted[ms$time == 0.5], 235)
  expect_equal(ms$f0_predicted[1], 250)
})

test_that("compute_mean_contour skips 0 Hz as no f0, as the time axis does", {
  mk <- function(tok, base) data.frame(token = tok, time = 0:10 / 100, tone = "T1",
                                       f0 = c(0, base + 0:8, 0))
  d <- rbind(mk("a", 200), mk("b", 210))
  d$f0[d$token == "a" & d$time == 0.05] <- 0       # a 0 Hz frame inside token a
  na <- d
  na$f0[na$f0 == 0] <- NA
  ref <- compute_mean_contour(na, n_points = 11)
  expect_equal(compute_mean_contour(d, n_points = 11), ref)
  expect_equal(compute_mean_contour(d[d$f0 > 0, ], n_points = 11), ref)
  expect_gt(min(ref$f0_predicted), 200)
  # In a column that is not in Hz, such as semitones, 0 is a value.
  d$f0_st <- 0
  st <- compute_mean_contour(d, f0 = "f0_st", n_points = 11)
  expect_equal(st$f0_predicted, rep(0, 11))
})

test_that("compute_mean_contour takes each token's tone as the other fits do", {
  # A tone column read from a TextGrid tier: the label covers the middle of
  # each token, "" covers the rest, and the first row is "".
  mk <- function(tok, tone, base, slope, dur) data.frame(
    token = tok, time = seq(0, dur, length.out = 11),
    f0 = base + slope * seq(0, 1, by = 0.1),
    tier = c(rep("", 3), rep(tone, 5), rep("", 3)), tone = tone)
  d <- rbind(mk("a", "T1", 200, 0, 0.30), mk("b", "T1", 202, 0, 0.25),
             mk("c", "T2", 150, 60, 0.32), mk("d", "T2", 155, 60, 0.28))
  ref <- compute_mean_contour(d, tone = "tone", n_points = 11)
  out <- compute_mean_contour(d, tone = "tier", n_points = 11)
  expect_identical(unique(out$tone), c("T1", "T2"))
  expect_equal(out$f0_predicted, ref$f0_predicted)
  # Neither the row order nor the rows without f0 change it.
  set.seed(3)
  expect_equal(compute_mean_contour(d[sample(nrow(d)), ], tone = "tier", n_points = 11), out)
  expect_equal(compute_mean_contour(d[nrow(d):1, ], tone = "tier", n_points = 11), out)
  pad <- d
  pad$f0[pad$time == 0] <- NA
  pad$tier[pad$time == 0] <- "T9"          # a label on a row without f0
  expect_identical(unique(compute_mean_contour(pad, tone = "tier", n_points = 11)$tone),
                   c("T1", "T2"))
  # The same tone per token as fit_polynomial().
  co <- suppressMessages(fit_polynomial(d[sample(nrow(d)), ], tone = "tier",
                                        speaker = "token", degree = 1))
  expect_identical(co$tier[order(co$token)], c("T1", "T1", "T2", "T2"))
  # A tie goes to the label that comes first in time.
  tie <- d[d$token == "a", ]
  tie$tier <- c(rep("T2", 5), rep("T1", 5), "")
  expect_identical(unique(compute_mean_contour(tie[11:1, ], tone = "tier")$tone), "T2")
})
