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

test_that("compute_mean_contour returns tone × time-bin grid", {
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
  out <- compute_mean_contour(df, n_bins = 2)
  # T1 mean at time=0 should be 100, at time=1 should be 200
  expect_equal(out$f0_predicted[out$tone == "T1" & out$time == 0], 100)
  expect_equal(out$f0_predicted[out$tone == "T2" & out$time == 1], 300)
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
