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
                    "robust", "shape") %in% names(out)))
  expect_true(is.na(out$robust[1]))   # no raw_data given
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

test_that("contour_to_chao computes robust FOR when raw_data is supplied", {
  contour <- data.frame(
    tone         = rep("T1", 11),
    time         = seq(0, 1, length.out = 11),
    f0_predicted = seq(120, 180, length.out = 11)
  )
  raw <- data.frame(
    token = rep(c("a", "b", "c", "d"), each = 10),
    f0    = c(rep(120, 10), rep(180, 10), rep(120, 10), rep(180, 10)),
    tone  = rep(c("T_low", "T_high", "T_low", "T_high"), each = 10)
  )
  out <- contour_to_chao(contour, raw_data = raw,
                         raw_token = "token", raw_f0 = "f0",
                         raw_tone  = "tone")
  expect_false(is.na(out$robust))
  expect_true(nchar(out$robust) >= 2)
})

test_that("contour_to_chao errors on missing columns", {
  contour <- data.frame(tone = "T1", time = 0, f0_predicted = 100)
  expect_error(contour_to_chao(contour, tone_col = "nonexistent"),
               "Column.*not found")
})
