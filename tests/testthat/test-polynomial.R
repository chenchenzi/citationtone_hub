test_that("fit_polynomial returns one row per token with named coefficients", {
  df <- data.frame(
    token   = rep(c("t1", "t2", "t3"), each = 5),
    time    = rep(seq(0, 1, length.out = 5), 3),
    f0      = c(150, 160, 170, 165, 155,
                140, 150, 160, 155, 145,
                155, 165, 175, 170, 160),
    speaker = rep("S01", 15),
    tone    = rep(c("T1", "T2", "T3"), each = 5)
  )
  out <- fit_polynomial(df, degree = 2)

  expect_equal(nrow(out), 3)
  expect_true(all(c("token", "speaker", "tone", "c0", "c1", "c2") %in%
                  names(out)))
  expect_false("c3" %in% names(out))   # degree = 2 must not have c3
})

test_that("c0 captures token mean for a flat contour", {
  df <- data.frame(
    token   = rep("t1", 5),
    time    = seq(0, 1, length.out = 5),
    f0      = rep(150, 5),                # flat
    speaker = "S01",
    tone    = "T1"
  )
  out <- fit_polynomial(df, degree = 2)
  expect_equal(out$c0, 150)
})

test_that("c1 sign reflects linear slope direction", {
  base <- data.frame(
    token = rep("t1", 5),
    time  = seq(0, 1, length.out = 5),
    speaker = "S01",
    tone    = "T1"
  )

  rising  <- base; rising$f0  <- c(100, 120, 140, 160, 180)
  falling <- base; falling$f0 <- rev(rising$f0)

  expect_gt(fit_polynomial(rising,  degree = 1)$c1, 0)
  expect_lt(fit_polynomial(falling, degree = 1)$c1, 0)
})

test_that("degree must be 1, 2, or 3", {
  df <- data.frame(token = rep("t1", 4), time = 0:3,
                    f0 = c(100, 120, 140, 160),
                    speaker = "S01", tone = "T1")
  expect_error(fit_polynomial(df, degree = 0), "degree")
  expect_error(fit_polynomial(df, degree = 4), "degree")
  expect_no_error(fit_polynomial(df, degree = 1))
  expect_no_error(fit_polynomial(df, degree = 3))
})

test_that("degenerate token (one sample) keeps c0 = mean, higher coefs NA", {
  df <- data.frame(
    token   = c("t1", "t2", "t2", "t2"),
    time    = c(0,    0,    1,    2),
    f0      = c(150,  100,  120,  140),
    speaker = rep("S01", 4),
    tone    = rep("T1",  4)
  )
  out <- fit_polynomial(df, degree = 1)
  expect_equal(out$c0[out$token == "t1"], 150)
  expect_true(is.na(out$c1[out$token == "t1"]))
})

test_that("token with fewer valid samples than degree+1 returns all NA", {
  df <- data.frame(
    token   = rep("t1", 3),
    time    = c(0, 0.5, 1),
    f0      = c(150, NA, NA),    # only 1 valid sample, degree 2 needs 3
    speaker = "S01",
    tone    = "T1"
  )
  out <- fit_polynomial(df, degree = 2)
  expect_true(is.na(out$c0))
  expect_true(is.na(out$c1))
  expect_true(is.na(out$c2))
})

test_that("missing column gives a clear error", {
  df <- data.frame(token = "t1", time = 0, f0 = 150,
                    speaker = "S01", tone = "T1")
  expect_error(fit_polynomial(df, f0 = "nonexistent"),
               "Column.*not found")
})

test_that("non-numeric f0 column gives a clear error", {
  df <- data.frame(token = "t1", time = 0, f0 = "not_numeric",
                    speaker = "S01", tone = "T1")
  expect_error(fit_polynomial(df, f0 = "f0"),
               "must be numeric")
})
