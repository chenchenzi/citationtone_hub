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
    f0      = c(150, NA, 160),   # 2 samples at 2 times, degree 2 needs 3
    speaker = "S01",
    tone    = "T1"
  )
  out <- fit_polynomial(df, degree = 2, time_normalised = "no")
  expect_true(is.na(out$c0))
  expect_true(is.na(out$c1))
  expect_true(is.na(out$c2))
  # Same without the row without f0.
  expect_equal(fit_polynomial(df[!is.na(df$f0), ], degree = 2,
                              time_normalised = "no"), out)
})

test_that("degenerate check looks only at rows with f0 and time", {
  # One sample among rows without f0 is a one-time-point token, as it is
  # once those rows are removed: c0 is the sample, the rest NA.
  df <- data.frame(
    token   = rep("t1", 3),
    time    = c(0, 0.5, 1),
    f0      = c(150, NA, NA),
    speaker = "S01",
    tone    = "T1"
  )
  for (deg in 1:3) {
    out <- fit_polynomial(df, degree = deg, time_normalised = "no")
    expect_equal(out$c0, 150)
    expect_true(all(is.na(unlist(out[paste0("c", seq_len(deg))]))))
    expect_equal(out, fit_polynomial(df[1, ], degree = deg,
                                     time_normalised = "no"))
  }

  # A row with f0 but no time is not a sample.
  nt <- data.frame(token = "t1", time = c(0.2, NA, 0.2), f0 = c(1, 5, 3),
                   speaker = "S01", tone = "T1")
  out <- fit_polynomial(nt, degree = 1, time_normalised = "no")
  expect_equal(out$c0, 2)
  expect_true(is.na(out$c1))

  # No sample at all: every coefficient NA.
  none <- data.frame(token = "t1", time = c(0, 0.5, 1), f0 = NA_real_,
                     speaker = "S01", tone = "T1")
  expect_true(all(is.na(unlist(fit_polynomial(none, degree = 2)[c("c0", "c1", "c2")]))))
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

test_that("non-finite f0 (-Inf from log of 0 Hz) is skipped, not an error", {
  df <- data.frame(token = "t1", time = seq(0, 1, length.out = 6),
                   f0 = c(1.0, -Inf, 0.8, 0.5, -Inf, 1.1),
                   speaker = "S01", tone = "T1")
  expect_no_error(out <- suppressMessages(fit_polynomial(df, degree = 2)))
  ref <- suppressMessages(fit_polynomial(df[is.finite(df$f0), ], degree = 2))
  expect_equal(out, ref)
})

test_that("0 Hz is no f0: skipped as the time axis skips it", {
  df <- data.frame(token = rep(c("a", "b"), each = 11), time = rep(0:10 / 100, 2),
                   f0 = c(0, 200 + 0:8, 0, 0, 210 + 2 * (0:8), 0),
                   speaker = "S01", tone = "T1")
  df$f0[df$token == "a" & df$time == 0.05] <- 0   # a 0 Hz frame inside token a
  out <- suppressMessages(fit_polynomial(df, degree = 2))
  expect_equal(out, suppressMessages(fit_polynomial(df[df$f0 > 0, ], degree = 2)))
  expect_equal(out$c1[out$token == "a"], 4)       # 200 to 208 across [-1, 1]
  # In a column that is not in Hz, 0 is a value
  df$f0_st <- c(rep(0, 11), 1:11)
  st <- suppressMessages(fit_polynomial(df, f0 = "f0_st", degree = 1))
  expect_equal(st$c0[st$token == "a"], 0)
})

test_that("single-time-point token takes c0 from finite f0 only", {
  df <- data.frame(token = c("t1", "t1", "t1", "t2"), time = c(0.2, 0.2, 0.2, 0.4),
                   f0 = c(-Inf, 1.5, 2.5, -Inf), speaker = "S01", tone = "T1")
  out <- fit_polynomial(df, degree = 1, time_normalised = "no")
  expect_equal(out$c0[out$token == "t1"], 2)
  expect_true(is.na(out$c0[out$token == "t2"]))   # no finite f0 left
  expect_true(all(is.na(out$c1)))
})

# A tone tier read from a TextGrid: "" in the silences around the vowel, the
# tone label inside it, and a stray second label on a few rows. Rows without
# f0 sit at both edges and inside each token.
make_tier_data <- function() {
  tok <- function(id, spk, lab, other, f0) {
    n <- length(f0)
    tier <- rep(lab, n)
    tier[c(1:2, n - 1, n)] <- ""
    tier[4] <- other
    data.frame(token = id, time = round(seq(0, by = 0.01, length.out = n), 2),
               f0 = f0, speaker = spk, tone_tier = tier,
               stringsAsFactors = FALSE)
  }
  rbind(
    tok("a", "S1", "T1", "T9",
        c(NA, NA, NA, 200, 205, NA, 212, 215, 220, 224, NA, NA)),
    tok("b", "S2", "T2", "T8",
        c(NA, NA, 180, 176, 171, 168, 163, NA, 158, 152, 150, NA)),
    tok("c", "S1", "T3", "T7",
        c(NA, NA, NA, NA, 140, 150, 160, 150, 140, NA, NA, NA))
  )
}

test_that("speaker and tone come from the most frequent non-empty value among rows with f0", {
  df <- make_tier_data()
  out <- fit_polynomial(df, tone = "tone_tier", degree = 2,
                        time_normalised = "no")
  expect_equal(out$tone_tier, c("T1", "T2", "T3"))
  expect_equal(out$speaker, c("S1", "S2", "S1"))
  # At HEAD the first row gave "" to every token.
  expect_false(any(out$tone_tier == ""))
})

test_that("token-level values do not change when rows without f0 are removed or rows are reordered", {
  df   <- make_tier_data()
  fp   <- function(d) fit_polynomial(d, tone = "tone_tier", degree = 2,
                                     time_normalised = "no")
  meta <- function(o) as.data.frame(o[order(o$token), c("token", "speaker", "tone_tier")])

  raw   <- fp(df)
  nona  <- fp(df[!is.na(df$f0), ])
  set.seed(11)
  shuf  <- fp(df[sample(nrow(df)), ])
  set.seed(12)
  shuf2 <- fp(df[!is.na(df$f0), ][sample(sum(!is.na(df$f0))), ])

  expect_identical(meta(nona), meta(raw))
  expect_identical(meta(shuf), meta(raw))
  expect_identical(meta(shuf2), meta(raw))
  # Row order changes nothing at all.
  expect_identical(shuf, raw)

  # With the edges trimmed, removing the rows without f0 inside the tokens
  # changes nothing either, coefficients included.
  first_last <- function(d) {
    keep <- unlist(lapply(split(seq_len(nrow(d)), d$token), function(ix) {
      v <- which(!is.na(d$f0[ix]))
      ix[min(v):max(v)]
    }))
    d[sort(keep), ]
  }
  expect_identical(fp(first_last(df)), nona)
})

test_that("a tie goes to the label first in time, in any row order", {
  df <- data.frame(token = "t1", time = (0:5) / 100,
                   f0 = c(NA, 200, 201, 202, 203, NA),
                   speaker = "S1",
                   tone_tier = c("X", "B", "A", "B", "A", "X"),
                   stringsAsFactors = FALSE)
  fp <- function(d) fit_polynomial(d, tone = "tone_tier", degree = 1,
                                   time_normalised = "no")$tone_tier
  expect_equal(fp(df), "B")
  expect_equal(fp(df[6:1, ]), "B")
  expect_equal(fp(df[c(3, 5, 1, 2, 6, 4), ]), "B")
})

test_that("rows with f0 that are all unlabelled do not borrow a label", {
  df <- data.frame(token = rep(c("t1", "t2"), each = 4),
                   time = rep((0:3) / 100, 2),
                   f0 = c(NA, 200, 210, NA,   NA, NA, NA, NA),
                   speaker = "S1",
                   tone_tier = c("T1", "", " ", "T1",   "", "T4", NA, "T4"),
                   stringsAsFactors = FALSE)
  out <- fit_polynomial(df, tone = "tone_tier", degree = 1,
                        time_normalised = "no")
  # t1: its rows with f0 are blank, so it stays blank (the most frequent
  # blank value, here a tie between "" and " " that "" wins by time).
  expect_equal(out$tone_tier[out$token == "t1"], "")
  # t2 has no f0 at all, so all its rows count.
  expect_equal(out$tone_tier[out$token == "t2"], "T4")
})

test_that("a factor tone keeps its levels", {
  df <- make_tier_data()
  df$tone_tier <- factor(df$tone_tier, levels = c("", "T1", "T2", "T3", "T7", "T8", "T9"))
  out <- fit_polynomial(df, tone = "tone_tier", degree = 2,
                        time_normalised = "no")
  expect_s3_class(out$tone_tier, "factor")
  expect_identical(levels(out$tone_tier), levels(df$tone_tier))
  expect_equal(as.character(out$tone_tier), c("T1", "T2", "T3"))
})

test_that("f0_long.csv: a tier that changes within tokens gives the same tones with or without rows without f0", {
  path <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(path), "test/f0_long.csv not found")
  fl <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  fl <- zero_hz_to_na(fl)
  # A tier labelled inside the middle half of each token's span with f0 and
  # "" elsewhere, so the first row of a token is always "".
  rng <- tapply(fl$time[!is.na(fl$f0)], fl$token[!is.na(fl$f0)], range)
  lo  <- vapply(rng, function(r) r[1] + 0.25 * diff(r), numeric(1))[fl$token]
  hi  <- vapply(rng, function(r) r[1] + 0.75 * diff(r), numeric(1))[fl$token]
  fl$speaker   <- "S1"
  fl$tone_tier <- ifelse(fl$time >= lo & fl$time <= hi,
                         paste0("L", match(fl$token, unique(fl$token)) %% 6), "")
  expect_true(all(tapply(fl$tone_tier, fl$token,
                         function(x) length(unique(x)) > 1)))
  fp <- function(d) {
    o <- suppressMessages(fit_polynomial(d, tone = "tone_tier", degree = 2))
    as.data.frame(o[order(o$token), c("token", "tone_tier")])
  }
  raw  <- fp(fl)
  nona <- fp(fl[!is.na(fl$f0), ])
  expect_equal(nrow(raw), 1853L)
  expect_equal(sum(raw$tone_tier == ""), 0L)
  expect_identical(nona, raw)
})

test_that("fit_polynomial on raw time: rows without f0 at the edges change nothing", {
  set.seed(5)
  df <- do.call(rbind, lapply(1:6, function(i) {
    t <- seq(0, 0.3, length.out = 12) + i
    data.frame(token = paste0("k", i), time = t,
               f0 = 150 + 80 * (t - i) + 30 * (t - i)^2 + rnorm(12),
               speaker = "s1", tone = paste0("T", i %% 2))
  }))
  pad <- do.call(rbind, lapply(split(df, df$token), function(d) {
    edge <- d[c(1, 1, nrow(d)), ]
    edge$f0   <- NA
    edge$time <- d$time[1] + c(-0.06, -0.03, 0.33)
    rbind(edge[1:2, ], d, edge[3, ])
  }))
  a <- fit_polynomial(pad, degree = 3)
  b <- fit_polynomial(pad[!is.na(pad$f0), ], degree = 3)
  expect_equal(a, b)
  expect_false(isTRUE(attr(a, "time_prenormalised")))
  # The same as rescaling from the first row to the last on the rows with f0
  expect_equal(a, fit_polynomial(df, degree = 3))
})
