# ---------- assign_tier_landmarks --------------------------------------------

# A simple 4-interval tier: silence | S1 | S2 | silence
tier_t1  <- c(0.0, 0.1, 0.3, 0.6)
tier_t2  <- c(0.1, 0.3, 0.6, 0.8)
tier_lab <- c("",  "S1", "S2", "")

test_that("assign_tier_landmarks tags each time with its interval", {
  res <- assign_tier_landmarks(c(0.2, 0.45), tier_t1, tier_t2, tier_lab)
  expect_equal(res$label, c("S1", "S2"))
  expect_equal(res$start, c(0.1, 0.3))
  expect_equal(res$end,   c(0.3, 0.6))
  expect_equal(res$idx,   c(1L, 2L))     # 1-based index among labelled intervals
})

test_that("empty intervals get an NA segment index but real boundaries", {
  res <- assign_tier_landmarks(c(0.05, 0.7), tier_t1, tier_t2, tier_lab)
  expect_equal(res$label, c("", ""))
  expect_equal(res$start, c(0.0, 0.6))
  expect_equal(res$end,   c(0.1, 0.8))
  expect_true(all(is.na(res$idx)))
})

test_that("times outside the tier span return all-NA", {
  res <- assign_tier_landmarks(c(-0.1, 0.9), tier_t1, tier_t2, tier_lab)
  expect_true(all(is.na(res$label)))
  expect_true(all(is.na(res$start)))
  expect_true(all(is.na(res$idx)))
})

test_that("assign_tier_landmarks handles empty inputs", {
  expect_equal(nrow(assign_tier_landmarks(numeric(0), tier_t1, tier_t2, tier_lab)), 0L)
  res <- assign_tier_landmarks(c(0.2, 0.4), numeric(0), numeric(0), character(0))
  expect_equal(nrow(res), 2L)
  expect_true(all(is.na(res$idx)))
})

# ---------- attach_landmarks -------------------------------------------------

test_that("attach_landmarks adds <tier>/_start/_end/_i columns from a TextGrid", {
  skip_if_not_installed("rPraat")
  tg <- rPraat::tg.createNewTextGrid(0, 0.8)
  tg <- rPraat::tg.insertNewIntervalTier(tg, 1, "syllable")
  tg <- rPraat::tg.insertInterval(tg, "syllable", 0.1, 0.3, "S1")
  tg <- rPraat::tg.insertInterval(tg, "syllable", 0.3, 0.6, "S2")
  f <- tempfile(fileext = ".TextGrid")
  rPraat::tg.write(tg, f)

  df <- data.frame(token = "w1",
                   time  = c(0.2, 0.45, 0.7),
                   f0    = c(120, 130, 140),
                   stringsAsFactors = FALSE)
  audio <- data.frame(basename = "w1", tg_path = f, stringsAsFactors = FALSE)

  out <- attach_landmarks(df, audio, "syllable")
  expect_true(all(c("syllable", "syllable_start", "syllable_end", "syllable_i") %in% names(out)))
  expect_equal(out$syllable, c("S1", "S2", ""))
  expect_equal(out$syllable_i, c(1L, 2L, NA_integer_))
})

test_that("attach_landmarks is a no-op without tiers or token/time columns", {
  df <- data.frame(token = "w1", time = 0.2, f0 = 100)
  audio <- data.frame(basename = "w1", tg_path = NA_character_)
  expect_identical(attach_landmarks(df, audio, character(0)), df)
  expect_identical(attach_landmarks(data.frame(a = 1), audio, "syllable"), data.frame(a = 1))
})

# ---------- normalise_time_landmarks -----------------------------------------

test_that("normalise_time_landmarks adds within-segment and sequential time", {
  # syllable 1 over [0.0, 0.2], syllable 2 over [0.2, 0.6]
  df <- data.frame(
    time           = c(0.05, 0.10, 0.40),
    syllable_start = c(0.0,  0.0,  0.2),
    syllable_end   = c(0.2,  0.2,  0.6),
    syllable_i     = c(1L,   1L,   2L)
  )
  out <- normalise_time_landmarks(df, "time", "syllable")
  # within-segment 0-1
  expect_equal(out$syllable_t01, c(0.25, 0.50, 0.50))
  # sequential: segment 1 keeps 0-1, segment 2 shifted into 1-2
  expect_equal(out$syllable_tseq, c(0.25, 0.50, 1.50))
})

test_that("normalise_time_token rescales each token's time to 0-1", {
  df <- data.frame(
    token = c("a", "a", "a", "b", "b"),
    time  = c(0.0, 0.5, 1.0, 2.0, 2.4)
  )
  out <- normalise_time_token(df, "time", "token")
  expect_equal(out$token_t01, c(0, 0.5, 1, 0, 1))   # per-token min->0, max->1
  expect_identical(normalise_time_token(df, "time", "nope"), df)  # no-op, missing col
})

test_that("normalise_time_landmarks clamps to 0-1 and is a no-op when columns missing", {
  df <- data.frame(time = c(-1, 5), syllable_start = c(0, 0), syllable_end = c(1, 1))
  out <- normalise_time_landmarks(df, "time", "syllable")
  expect_equal(out$syllable_t01, c(0, 1))                 # clamped
  expect_equal(out$syllable_tseq, c(0, 1))                # no _i -> equals t01
  expect_identical(normalise_time_landmarks(data.frame(a = 1), "time", "syllable"),
                   data.frame(a = 1))
})
