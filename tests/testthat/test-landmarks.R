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

# ---------- normalise_time_landmarks / normalise_time_token -----------------

# Five disyllabic tokens on 10 ms frames: syllable 1 is [0.00, 0.20),
# syllable 2 is [0.20, 0.40], with an unlabelled interval at each end.
#   A: a voiceless onset, and a voiceless gap at the middle boundary
#   B: a gap inside syllable 1
#   C: syllable 2 has one f0 point
#   D: syllable 2 has no f0
#   E: f0 on every frame, also in the unlabelled intervals
lm_token <- function(tok, f0_fun, na_at = integer(0)) {
  t  <- round(seq(-0.045, 0.445, by = 0.01), 3)
  f0 <- f0_fun(t)
  f0[na_at] <- NA
  sy <- ifelse(t < 0, 0L, ifelse(t < 0.2, 1L, ifelse(t <= 0.4, 2L, 0L)))
  st <- c(-0.05, 0.00, 0.20)[sy + 1L]; st[t > 0.4] <- 0.40
  en <- c(0.00, 0.20, 0.40)[sy + 1L];  en[t > 0.4] <- 0.45
  lab <- c("", "S1", "S2")[sy + 1L];   lab[t > 0.4] <- ""
  data.frame(token = tok, time = t, f0 = f0, syllable = lab,
             syllable_start = st, syllable_end = en,
             syllable_i = ifelse(lab == "", NA_integer_, sy),
             stringsAsFactors = FALSE)
}
lm_fixture <- function() {
  t <- round(seq(-0.045, 0.445, by = 0.01), 3)
  w <- function(a, b) which(t >= a - 1e-9 & t <= b + 1e-9)
  rbind(
    lm_token("A", function(t) 200 + 100 * t, c(w(-0.05, 0.06), w(0.16, 0.26), w(0.39, 0.45))),
    lm_token("B", function(t) 180 - 50 * t, c(w(-0.05, -0.001), w(0.09, 0.11), w(0.40, 0.45))),
    lm_token("C", function(t) 220 + 0 * t, c(w(-0.05, 0.02), setdiff(w(0.20, 0.45), w(0.295, 0.295)))),
    lm_token("D", function(t) 150 + 200 * t, c(w(-0.05, 0.03), w(0.17, 0.45))),
    lm_token("E", function(t) 210 + 30 * sin(10 * t), integer(0)))
}
lm_axes <- function(d) {
  normalise_time_token(normalise_time_landmarks(d, "time", "syllable"), "time", "token")
}

test_that("normalise_time_landmarks runs each syllable from its first f0 to its last", {
  df <- data.frame(token = "w1", time = seq(0.01, 0.10, by = 0.01),
                   f0 = c(NA, 200, 205, 210, NA, NA, 190, 185, 180, NA),
                   syllable_start = rep(c(0, 0.05), each = 5),
                   syllable_end   = rep(c(0.05, 0.11), each = 5),
                   syllable_i     = rep(1:2, each = 5))
  out <- normalise_time_landmarks(df, "time", "syllable")
  expect_equal(out$syllable_t01, c(NA, 0, 0.5, 1, NA, NA, 0, 0.5, 1, NA))
  # Stacked: the voiceless boundary takes no room, so both 1.0s meet there
  expect_equal(out$syllable_tseq, c(NA, 0, 0.5, 1, NA, NA, 1, 1.5, 2, NA))
  kept <- !is.na(df$f0)
  expect_identical(normalise_time_landmarks(df[kept, ], "time", "syllable")$syllable_tseq,
                   out$syllable_tseq[kept])
})

test_that("removing rows without f0 or reordering rows changes no axis", {
  fx <- lm_fixture()
  a  <- lm_axes(fx)
  b  <- lm_axes(suppressMessages(drop_rows_without_f0(fx)))
  k  <- !is.na(fx$f0)
  set.seed(3)
  o  <- sample(nrow(fx))
  s  <- lm_axes(fx[o, ])[order(o), ]
  for (cn in c("syllable_t01", "syllable_tseq", "syllable_tbound", "token_t01")) {
    expect_identical(b[[cn]], a[[cn]][k], info = cn)
    expect_identical(s[[cn]], a[[cn]], info = cn)
  }
  # Each syllable's first f0 sits at i - 1 and its last at i on _tseq
  v <- a[k & !is.na(a$syllable_tseq), ]
  lo <- tapply(v$syllable_tseq, list(v$token, v$syllable_i), min)
  hi <- tapply(v$syllable_tseq, list(v$token, v$syllable_i), max)
  for (tk in rownames(lo)) for (i in colnames(lo)) {
    if (is.na(lo[tk, i])) next
    expect_equal(c(lo[tk, i], hi[tk, i]), as.numeric(i) - c(1, 0), info = paste(tk, i))
  }
  expect_equal(sum(!is.na(lo)), 8L)           # C and D have no syllable 2 axis
  # f0 in the unlabelled intervals (token E) is outside the tier
  silence <- k & is.na(fx$syllable_i)
  expect_equal(sum(silence), 10L)
  expect_true(all(is.na(a$syllable_t01[silence])))
  # A syllable with one f0 point (C) or none (D) has no span
  expect_true(all(is.na(a$syllable_t01[a$token %in% c("C", "D") & a$syllable_i %in% 2])))
  # Rows without f0 inside a span keep their place by time
  expect_equal(sum(!k & is.finite(a$syllable_t01)), 2L)
  expect_equal(sum(!k & is.finite(a$token_t01)), 21L)
  # token_t01 runs from each token's first f0 to its last
  expect_equal(as.vector(tapply(a$token_t01[k], a$token[k], min)), rep(0, 5))
  expect_equal(as.vector(tapply(a$token_t01[k], a$token[k], max)), rep(1, 5))
})

test_that("tokens with the same boundaries are kept apart", {
  tw <- data.frame(token = rep(c("x", "y"), each = 4),
                   time = rep(c(0.1, 0.2, 0.3, 0.4), 2),
                   f0 = c(NA, 200, 210, 220, 200, 205, 210, NA),
                   syllable_start = 0, syllable_end = 0.5, syllable_i = 1L)
  out <- normalise_time_landmarks(tw, "time", "syllable")
  expect_equal(out$syllable_t01[out$token == "x"], c(NA, 0, 0.5, 1))
  expect_equal(out$syllable_t01[out$token == "y"], c(0, 0.5, 1, NA))
})

test_that("0 counts as no f0 only in Hz", {
  sg <- data.frame(token = "s", time = 1:6 / 100, f0_st = c(NA, -1, 0, 2, 0, NA),
                   syllable_start = 0, syllable_end = 1, syllable_i = 1L)
  expect_equal(normalise_time_landmarks(sg, "time", "syllable", f0 = "f0_st")$syllable_t01,
               c(NA, 0, 1/3, 2/3, 1, NA))
  hz <- data.frame(token = "h", time = 1:6 / 100, f0 = c(0, 210, 0, 220, 230, 0))
  expect_equal(normalise_time_token(hz, "time", "token")$token_t01,
               c(NA, 0, 1/3, 2/3, 1, NA))
  # The same Hz rule in normalise_time_landmarks()
  hz$syllable_start <- 0; hz$syllable_end <- 1; hz$syllable_i <- 1L
  out <- normalise_time_landmarks(hz, "time", "syllable")
  expect_equal(out$syllable_t01, c(NA, 0, 1/3, 2/3, 1, NA))
  expect_equal(out$syllable_tseq, c(NA, 0, 1/3, 2/3, 1, NA))
  # A column with negative markers is not read as Hz, so -1 counts as f0,
  # as the docs say; set the markers to NA first
  neg <- hz
  neg$f0 <- c(-1, 210, 215, 220, 225, -1)
  expect_identical(hz_f0_columns(neg["f0"]), character(0))
  expect_equal(normalise_time_landmarks(neg, "time", "syllable")$syllable_t01, 0:5 / 5)
  expect_equal(normalise_time_token(neg, "time", "token")$token_t01, 0:5 / 5)
  neg$f0[neg$f0 < 0] <- NA
  expect_equal(normalise_time_landmarks(neg, "time", "syllable")$syllable_t01,
               c(NA, 0, 1/3, 2/3, 1, NA))
})

test_that("time in other units gives the same axis", {
  fx <- lm_fixture()
  ms <- fx
  ms$time <- ms$time * 1000
  a <- lm_axes(fx)
  b <- suppressMessages(lm_axes(ms))   # _tbound: see below
  expect_equal(b$syllable_t01, a$syllable_t01)
  expect_equal(b$syllable_tseq, a$syllable_tseq)
  expect_equal(b$token_t01, a$token_t01)
  # _tbound reads the boundaries, so they go to ms too
  expect_message(c1 <- lm_axes(ms), "same clock")
  expect_true(all(is.na(c1$syllable_tbound[c1$token != "D"])))
  ms$syllable_start <- ms$syllable_start * 1000
  ms$syllable_end   <- ms$syllable_end * 1000
  expect_silent(c2 <- normalise_time_landmarks(ms, "time", "syllable"))
  expect_equal(c2$syllable_tbound, a$syllable_tbound)
})

test_that("missing columns are an error; no _i means no _tseq or _tbound", {
  fx <- lm_fixture()
  no_f0 <- fx[setdiff(names(fx), "f0")]
  expect_error(normalise_time_landmarks(no_f0, "time", "syllable"),
               "Column(s) not found in data: f0", fixed = TRUE)
  expect_error(normalise_time_token(no_f0, "time", "token"),
               "Column(s) not found in data: f0", fixed = TRUE)
  expect_error(normalise_time_landmarks(fx, "time", "syllable", token = "word"),
               "Column(s) not found in data: word", fixed = TRUE)
  expect_error(normalise_time_landmarks(fx, "time", "rhyme"),
               "rhyme_start, rhyme_end", fixed = TRUE)
  expect_null(normalise_time_landmarks(NULL, "time", "syllable"))
  no_i <- fx[setdiff(names(fx), "syllable_i")]
  expect_message(out <- normalise_time_landmarks(no_i, "time", "syllable"),
                 "No syllable_i column, so syllable_tseq and syllable_tbound are not added",
                 fixed = TRUE)
  expect_false(any(c("syllable_tseq", "syllable_tbound") %in% names(out)))
  # Without _i, the label marks the labelled segments
  expect_equal(out$syllable_t01, lm_axes(fx)$syllable_t01)
})

test_that("F0 Correction output: the default f0 says to pass f0_corrected", {
  fx <- lm_fixture()
  fx$f0_corrected <- fx$f0
  fx$f0_corrected[fx$token == "E" & fx$time < 0.05] <- NA
  expect_message(normalise_time_token(fx, "time", "token"), "f0_corrected")
  expect_message(normalise_time_landmarks(fx, "time", "syllable"), "f0_corrected")
  expect_silent(out <- normalise_time_token(fx, "time", "token", f0 = "f0_corrected"))
  e <- out[out$token == "E", ]
  expect_equal(e$token_t01[e$time == 0.055], 0)
})

test_that("normalise_time_token runs each token from its first f0 to its last", {
  df <- data.frame(token = rep(c("a", "b"), c(6, 5)),
                   time  = c(0:5, 0:4) / 100,
                   f0    = c(NA, 210, 215, NA, 220, NA, 180, 185, 190, 195, NA))
  out <- normalise_time_token(df, "time", "token")
  expect_equal(out$token_t01, c(NA, 0, 1/3, 2/3, 1, NA, 0, 1/3, 2/3, 1, NA))
  # A token with f0 at one time has no span; rows with no token get NA
  df2 <- data.frame(token = c("c", "c", "c", NA, NA, NA),
                    time  = c(0, 0.01, 0.02, 0, 0.01, 0.02),
                    f0    = c(NA, 200, NA, 190, 195, 200))
  expect_true(all(is.na(normalise_time_token(df2, "time", "token")$token_t01)))
  # The fits' rescale treats the rows with no token as one token of their
  # own, as the docs say, while token_t01 is NA on them
  expect_equal(resolve_time_norm(df2, "time", "token", f0 = "f0")$time_norm,
               c(NA, 0.5, NA, 0, 0.5, 1))
  # Factor tokens and factor times are read by value
  df3 <- df
  df3$token <- factor(df3$token)
  df3$time  <- factor(df3$time)
  expect_equal(normalise_time_token(df3, "time", "token")$token_t01, out$token_t01)
})

test_that("a word tier gives token_t01 when all f0 lies inside the word", {
  t  <- seq(0, 0.5, by = 0.01)
  f0 <- ifelse(t >= 0.08 & t <= 0.42, 200 - 80 * t, NA)
  f0[t %in% c(0.20, 0.21)] <- NA              # a voiceless stop inside the word
  inside <- t >= 0.05 & t < 0.45
  df <- data.frame(token = "w", time = t, f0 = f0,
                   word = ifelse(inside, "pata", ""),
                   word_start = ifelse(inside, 0.05, ifelse(t < 0.05, 0, 0.45)),
                   word_end   = ifelse(inside, 0.45, ifelse(t < 0.05, 0.05, 0.5)),
                   word_i     = ifelse(inside, 1L, NA_integer_))
  out <- normalise_time_token(normalise_time_landmarks(df, "time", "word"), "time", "token")
  k <- !is.na(df$f0)
  expect_equal(out$word_t01[k], out$token_t01[k])
  expect_equal(out$word_tseq[k], out$token_t01[k])
  expect_identical(out$word_tbound, out$word_tseq)
  expect_equal(range(out$token_t01[k]), c(0, 1))
})

# ---------- <set>_tbound -------------------------------------------------------

# One token, two syllables on 10 ms frames: syllable 1 has f0 from 0.10 to
# 0.30 s, the TextGrid boundary is at 0.31 s, syllable 2 has f0 from 0.38 to
# 0.60 s. The tier runs from 0.05 to 0.65 s.
tb_two <- function(tok = "A", s1 = c(0.10, 0.30), s2 = c(0.38, 0.60), b = 0.31) {
  t  <- round(seq(0.06, 0.64, by = 0.01), 3)
  on <- function(r) t >= r[1] - 1e-9 & t <= r[2] + 1e-9
  in1 <- t < b
  data.frame(token = tok, time = t, f0 = ifelse(on(s1) | on(s2), 200 + 100 * t, NA),
             syllable = ifelse(in1, "S1", "S2"),
             syllable_start = ifelse(in1, 0.05, b), syllable_end = ifelse(in1, b, 0.65),
             syllable_i = ifelse(in1, 1L, 2L), stringsAsFactors = FALSE)
}
# Three syllables of 0.2 s on 10 ms frames (times 0.005, 0.015, ...), with
# boundaries at 0.2 and 0.4 s. `voiced` gives the stretch with f0 in each
# syllable, or NULL for a syllable with no f0.
tb_three <- function(tok, voiced) {
  t  <- round(seq(0.005, 0.595, by = 0.01), 3)
  i  <- findInterval(t, c(0, 0.2, 0.4))
  f0 <- rep(NA_real_, length(t))
  for (k in seq_along(voiced)) {
    if (is.null(voiced[[k]])) next
    w <- i == k & t >= voiced[[k]][1] - 1e-9 & t <= voiced[[k]][2] + 1e-9
    f0[w] <- 200 + 40 * k + 100 * t[w]
  }
  data.frame(token = tok, time = t, f0 = f0, syllable = paste0("S", i),
             syllable_start = (i - 1) * 0.2, syllable_end = i * 0.2,
             syllable_i = i, stringsAsFactors = FALSE)
}
tb_fixture <- function() {
  rbind(tb_two("A"),
        tb_two("B", s1 = c(0.08, 0.29), s2 = c(0.33, 0.62), b = 0.31),
        tb_three("first_none", list(NULL, c(0.255, 0.345), c(0.455, 0.545))),
        tb_three("mid_none",   list(c(0.055, 0.155), NULL, c(0.455, 0.545))),
        tb_three("one_s1",     list(c(0.055, 0.155), NULL, NULL)),
        tb_three("one_s2",     list(NULL, c(0.255, 0.345), NULL)),
        tb_three("lone",       list(c(0.105, 0.105), c(0.205, 0.395), NULL)),
        tb_three("single",     list(NULL, c(0.305, 0.305), NULL)))
}

test_that("<set>_tbound: 0 at the first f0, 1 at the boundary, 2 at the last f0", {
  out <- normalise_time_token(normalise_time_landmarks(tb_two(), "time", "syllable"),
                              "time", "token")
  r <- match(c(0.10, 0.30, 0.31, 0.38, 0.60), out$time)
  expect_equal(out$syllable_tbound[r],
               c(0, (0.30 - 0.10) / (0.31 - 0.10), 1, 1 + (0.38 - 0.31) / (0.60 - 0.31), 2))
  expect_equal(round(out$syllable_tbound[r], 3), c(0, 0.952, 1, 1.241, 2))
  # Exactly 0, 1 and 2 at the first f0, the boundary and the last f0
  expect_identical(out$syllable_tbound[r[c(1, 3, 5)]], c(0, 1, 2))
  # The other axes on the same rows (the boundary row has no f0)
  expect_equal(out$syllable_tseq[r[-3]], c(0, 1, 1, 2))
  expect_equal(out$token_t01[r[-3]], c(0, 0.4, 0.56, 1))
  # NA before the first f0 and after the last; the gap keeps its room
  expect_true(all(is.na(out$syllable_tbound[out$time < 0.10 | out$time > 0.60])))
  gap <- out$time > 0.30 & out$time < 0.38
  expect_true(all(out$syllable_tbound[gap] > 0.952 & out$syllable_tbound[gap] < 1.241))
  v <- out$syllable_tbound[!is.na(out$syllable_tbound)]
  expect_false(is.unsorted(v, strictly = TRUE))
})

test_that("<set>_tbound does not change with rows without f0 removed, rows reordered or ms", {
  fx <- tb_fixture()
  a  <- normalise_time_landmarks(fx, "time", "syllable")
  k  <- !is.na(fx$f0)
  b  <- normalise_time_landmarks(fx[k, ], "time", "syllable")
  expect_identical(b$syllable_tbound, a$syllable_tbound[k])
  set.seed(11)
  o <- sample(nrow(fx))
  s <- normalise_time_landmarks(fx[o, ], "time", "syllable")[order(o), ]
  expect_identical(s$syllable_tbound, a$syllable_tbound)
  ms <- fx
  for (cn in c("time", "syllable_start", "syllable_end")) ms[[cn]] <- ms[[cn]] * 1000
  expect_silent(m <- normalise_time_landmarks(ms, "time", "syllable"))
  expect_equal(m$syllable_tbound, a$syllable_tbound)
  # _t01 and _tseq are as before
  expect_identical(a$syllable_tseq, (a$syllable_i - 1L) + a$syllable_t01)
  # Within each token the values rise with time
  for (tk in unique(a$token)) {
    v <- a$syllable_tbound[a$token == tk]
    expect_false(is.unsorted(v[!is.na(v)]), info = tk)
  }
})

test_that("<set>_tbound with a syllable without f0, or f0 in one syllable only", {
  a <- normalise_time_landmarks(tb_fixture(), "time", "syllable")
  g <- function(tk) a[a$token == tk, ]
  # First syllable with no f0: syllable 2 is the first, and its first f0 is 1
  x <- g("first_none")
  expect_true(all(is.na(x$syllable_tbound[x$syllable_i == 1])))
  expect_equal(x$syllable_tbound[x$time == 0.255], 1)
  expect_equal(x$syllable_tbound[x$time == 0.395], 1 + (0.395 - 0.255) / (0.4 - 0.255))
  expect_equal(x$syllable_tbound[x$time == 0.405], 2 + 0.005 / (0.545 - 0.4))
  expect_equal(range(x$syllable_tbound, na.rm = TRUE), c(1, 3))
  # A middle syllable with no f0 runs from boundary to boundary
  y <- g("mid_none")
  s2 <- y$syllable_i == 2
  expect_equal(y$syllable_tbound[s2], 1 + (y$time[s2] - 0.2) / 0.2)
  expect_equal(y$syllable_tbound[y$time == 0.055], 0)
  expect_equal(y$syllable_tbound[y$time == 0.545], 3)
  expect_equal(y$syllable_tbound[y$time == 0.155], (0.155 - 0.055) / (0.2 - 0.055))
  # f0 in one syllable only: the same as _tseq. With f0 at one time only
  # the span is empty, so every row is NA (not NaN)
  for (tk in c("one_s1", "one_s2", "single")) {
    z <- g(tk)
    expect_identical(z$syllable_tbound, z$syllable_tseq, info = tk)
  }
  expect_identical(g("single")$syllable_tbound, rep(NA_real_, 60))
  expect_equal(range(g("one_s2")$syllable_tbound, na.rm = TRUE), c(1, 2))
  # A first syllable with f0 at one time still runs to its boundary
  l <- g("lone")
  expect_true(all(is.na(l$syllable_tseq[l$syllable_i == 1])))
  expect_equal(l$syllable_tbound[l$syllable_i == 1 & l$time >= 0.105],
               (c(0.105, 0.115, 0.125, 0.135, 0.145, 0.155, 0.165, 0.175, 0.185, 0.195) - 0.105) /
                 (0.2 - 0.105))
  expect_equal(l$syllable_tbound[l$time == 0.395], 2)
})

test_that("<set>_tbound: empty-label intervals, intervals that do not touch, no token", {
  # Syllable 1 [0.05, 0.20], a pause [0.20, 0.26] with an empty label,
  # syllable 2 [0.26, 0.45]. f0 runs on in the pause and after the tier.
  t  <- round(seq(0.06, 0.50, by = 0.01), 3)
  i  <- ifelse(t <= 0.20, 1L, ifelse(t < 0.26, NA, ifelse(t <= 0.45, 2L, NA)))
  d  <- data.frame(token = "p", time = t, f0 = 200 + 50 * t,
                   syllable = ifelse(is.na(i), "", paste0("S", i)),
                   syllable_start = ifelse(is.na(i), ifelse(t < 0.26, 0.20, 0.45),
                                           ifelse(i == 1, 0.05, 0.26)),
                   syllable_end = ifelse(is.na(i), ifelse(t < 0.26, 0.26, 0.50),
                                         ifelse(i == 1, 0.20, 0.45)),
                   syllable_i = i, stringsAsFactors = FALSE)
  out <- normalise_time_landmarks(d, "time", "syllable")
  expect_true(all(is.na(out$syllable_tbound[is.na(i)])))
  # Both ends of the pause sit at 1: it takes no room
  expect_identical(out$syllable_tbound[t %in% c(0.20, 0.26)], c(1, 1))
  expect_equal(out$syllable_tbound[t == 0.06], 0)
  expect_equal(out$syllable_tbound[t == 0.45], 2)
  expect_equal(out$syllable_tbound[t == 0.13], (0.13 - 0.06) / (0.20 - 0.06))
  # Rows with no token or no time get NA, and the rest of the token keeps
  # its values
  d2 <- d
  d2$token[t == 0.10] <- NA
  d2$time[t == 0.12]  <- NA
  out2 <- normalise_time_landmarks(d2, "time", "syllable")
  expect_true(all(is.na(out2$syllable_tbound[t %in% c(0.10, 0.12)])))
  keep <- !t %in% c(0.10, 0.12)
  expect_identical(out2$syllable_tbound[keep], out$syllable_tbound[keep])
})

test_that("<set>_tbound needs time and the boundaries on the same clock", {
  fx <- rbind(tb_two("A"), tb_two("B"), tb_three("one_s1", list(c(0.055, 0.155), NULL, NULL)))
  good <- normalise_time_landmarks(fx, "time", "syllable")
  # Token A in ms, its boundaries still in seconds; one_s1 too, but with f0
  # in one syllable only it reads no boundary
  ms <- fx
  w  <- ms$token %in% c("A", "one_s1")
  ms$time[w] <- ms$time[w] * 1000
  expect_message(out <- normalise_time_landmarks(ms, "time", "syllable"),
                 "syllable_tbound is NA in 1 token(s)", fixed = TRUE)
  expect_message(normalise_time_landmarks(ms, "time", "syllable"), "same clock")
  expect_true(all(is.na(out$syllable_tbound[out$token == "A"])))
  expect_identical(out$syllable_tbound[out$token == "B"],
                   good$syllable_tbound[good$token == "B"])
  expect_equal(out$syllable_tbound[out$token == "one_s1"],
               good$syllable_tbound[good$token == "one_s1"])
  # _t01 and _tseq only read the order and spacing of time
  expect_equal(out$syllable_tseq, good$syllable_tseq)
  # Time counted from the token's start, with the file's boundaries
  sh <- fx
  sh$time[sh$token == "B"] <- sh$time[sh$token == "B"] - 0.05
  expect_message(out <- normalise_time_landmarks(sh, "time", "syllable"), "1 token(s)",
                 fixed = TRUE)
  expect_true(all(is.na(out$syllable_tbound[out$token == "B"])))
  # Rounding: a row a hair outside its interval is not another clock
  hair <- fx
  hair$syllable_end[hair$token == "A" & hair$syllable_i == 1] <- 0.31 - 1e-12
  hair$time[hair$token == "A" & hair$time == 0.30] <- 0.31 - 1e-13
  expect_silent(out <- normalise_time_landmarks(hair, "time", "syllable"))
  expect_equal(range(out$syllable_tbound[out$token == "A"], na.rm = TRUE), c(0, 2))
  # In ms, a row 1e-7 ms outside its interval is past 1e-9 but well inside a
  # millionth of the token's time range (580 ms): still the same clock
  hms <- fx
  for (cn in c("time", "syllable_start", "syllable_end")) hms[[cn]] <- hms[[cn]] * 1000
  hms$syllable_end[hms$token == "A" & hms$syllable_i == 1] <- 300 - 1e-7
  expect_silent(out <- normalise_time_landmarks(hms, "time", "syllable"))
  expect_equal(range(out$syllable_tbound[out$token == "A"], na.rm = TRUE), c(0, 2))
  # The tolerance is per token: token B far along the file's clock does not
  # widen it for token A, whose row is 0.1 ms outside its interval
  far <- fx
  wB  <- far$token == "B"
  for (cn in c("time", "syllable_start", "syllable_end")) far[[cn]][wB] <- far[[cn]][wB] + 1000
  far$syllable_end[far$token == "A" & far$syllable_i == 1] <- 0.30 - 1e-4
  expect_message(out <- normalise_time_landmarks(far, "time", "syllable"), "1 token(s)",
                 fixed = TRUE)
  expect_true(all(is.na(out$syllable_tbound[out$token == "A"])))
  expect_equal(range(out$syllable_tbound[out$token == "B"], na.rm = TRUE), c(0, 2))
})

# ---------- ipa_vowel_label --------------------------------------------------

test_that("ipa_vowel_label recognises vowels with length, diacritics, diphthongs", {
  expect_true(all(ipa_vowel_label(
    c("a", "aː", "ai", "iau", "ã", "ə̃", "aʊ", "ɤ", "ɚ", "A", "AI", "@", "a51", "u2"))))
  expect_false(any(ipa_vowel_label(
    c("ang", "pa", "n", "p", "s", "", " ", NA, "aŋ", "kʰ"))))
})

# ---------- filter_interval_rows ---------------------------------------------

# Frames for two tokens over a segment tier p | a | n (token m1, monosyllable
# with coda) and a token m2 with no vowel interval; token m3 has NA landmarks
# (no TextGrid).
filter_df <- data.frame(
  token       = c(rep("m1", 5), rep("m2", 2), "m3"),
  time        = c(0.05, 0.15, 0.25, 0.35, 0.45, 0.05, 0.10, 0.05),
  f0          = c(100, 110, 120, 130, NA, 90, 95, 80),
  segment     = c("p", "a", "a", "n", "", "s", "s", NA),
  segment_i   = c(1L, 2L, 2L, 3L, NA, 1L, 1L, NA),
  stringsAsFactors = FALSE
)

test_that("vowel mode keeps only vowel-labelled frames", {
  out <- filter_interval_rows(filter_df, "segment", mode = "vowel")
  expect_equal(out$token, c("m1", "m1"))
  expect_equal(out$time, c(0.15, 0.25))
})

test_that("rhyme mode keeps labelled frames from the first vowel to token end", {
  out <- filter_interval_rows(filter_df, "segment", mode = "rhyme")
  expect_equal(out$token, c("m1", "m1", "m1"))
  expect_equal(out$segment, c("a", "a", "n"))   # vowel + coda; onset excluded
})

test_that("labels mode keeps the requested labels only", {
  out <- filter_interval_rows(filter_df, "segment", mode = "labels",
                              labels = c("a", "n"))
  expect_equal(out$segment, c("a", "a", "n"))
  out2 <- filter_interval_rows(filter_df, "segment", mode = "labels",
                               labels = " s ")
  expect_equal(out2$token, c("m2", "m2"))       # labels are trimmed
  expect_error(filter_interval_rows(filter_df, "segment", mode = "labels"),
               "non-empty")
})

test_that("NA-landmark frames (no TextGrid) are always excluded", {
  for (m in c("vowel", "rhyme")) {
    out <- filter_interval_rows(filter_df, "segment", mode = m)
    expect_false("m3" %in% out$token)
  }
})

test_that("filter_interval_rows validates its columns", {
  expect_error(filter_interval_rows(filter_df, "nope", mode = "vowel"),
               "not found")
  df2 <- filter_df[, setdiff(names(filter_df), "segment_i")]
  expect_error(filter_interval_rows(df2, "segment", mode = "rhyme"),
               "segment_i")
})

test_that("rhyme mode spans to the token end, so it is monosyllables-only", {
  # A disyllabic token p-a-n-t-a: "rhyme" runs from the FIRST vowel to the end,
  # which crosses the second syllable. Documented as a monosyllabic-only mode;
  # this pins the behaviour so the caveat stays honest.
  df <- data.frame(token = "w1", time = seq(0.1, 0.5, by = 0.1), f0 = 100:104,
                   seg = c("p", "a", "n", "t", "a"), seg_i = 1:5,
                   stringsAsFactors = FALSE)
  expect_equal(filter_interval_rows(df, "seg", "vowel")$seg, c("a", "a"))
  expect_equal(filter_interval_rows(df, "seg", "rhyme")$seg, c("a", "n", "t", "a"))
})

test_that("ipa_vowel_label accepts pinyin tone marks in both Unicode forms", {
  # NFC precomposed (what most TextGrid editors write) and NFD decomposed.
  expect_true(all(ipa_vowel_label(
    c("ā", "ǎ", "ū", "ǔ", "ē", "ě",
      "ī", "ǐ", "ō", "ǒ", "ǖ", "ǜ"))))
  expect_true(all(ipa_vowel_label(c("ā", "ǎ", "ū", "ǔ"))))
  # Whole pinyin syllables and consonant letters are still not vowels.
  expect_false(any(ipa_vowel_label(
    c("ang", "āng", "zhī", "shǔ", "ng", "ć", "č",
      "ń", "ř", "ž", "ł"))))
})

test_that("syllabic nasals count as nuclei, plain nasals do not", {
  # Tone-bearing in e.g. Cantonese (\u5514 m\u0329, \u4E94 \u014B\u0329).
  expect_true(all(ipa_vowel_label(c("m\u0329", "n\u0329", "\u014B\u0329", "m\u030D",
                                    "\u014B\u03295", "m\u0329\u02E8\u02E9"))))
  expect_false(any(ipa_vowel_label(c("m", "n", "\u014B", "ma", "am"))))
})

test_that("di- and triphthongs match, including j/w offglide spellings", {
  expect_true(all(ipa_vowel_label(
    c("ai", "au", "ei", "ou", "iau", "uai", "ɔi", "aːi", "ai̯"))))
  # Many traditions spell the offglide as a consonant letter: a glide counts
  # inside a nucleus that also contains a vowel letter.
  expect_true(all(ipa_vowel_label(
    c("aj", "aw", "ja", "jaw", "waj", "aɥ", "ɥa"))))
  # A bare glide is an onset, not a nucleus; other consonants never match.
  expect_false(any(ipa_vowel_label(
    c("j", "w", "ɥ", "jw", "ang", "an", "ai n"))))
})

# ---------- tg_tier_names -----------------------------------------------------

test_that("tg_tier_names reads long-format tier names, interval and point", {
  skip_if_not_installed("rPraat")
  tg <- rPraat::tg.createNewTextGrid(0, 0.8)
  tg <- rPraat::tg.insertNewIntervalTier(tg, 1, "syllable")
  tg <- rPraat::tg.insertNewPointTier(tg, 2, "tone_target")
  f <- tempfile(fileext = ".TextGrid")
  rPraat::tg.write(tg, f)                       # long format by default
  expect_equal(tg_tier_names(f), c("syllable", "tone_target"))
  # Names match a full parse.
  expect_equal(tg_tier_names(f), unname(vapply(rPraat::tg.read(f), `[[`, "", "name")))
})

test_that("tg_tier_names reads the short format, UTF-16 and escaped quotes", {
  short <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', "",
             "0", "0.8", "<exists>", "2",
             '"IntervalTier"', '"say ""hi"""', "0", "0.8", "1", "0", "0.8", '""',
             '"TextTier"', '"声调"', "0", "0.8", "0")
  f1 <- tempfile(fileext = ".TextGrid")
  writeLines(enc2utf8(short), f1, useBytes = TRUE)
  expect_equal(tg_tier_names(f1), c('say "hi"', "声调"))

  long <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', "",
            "xmin = 0", "xmax = 0.8", "tiers? <exists>", "size = 1", "item []:",
            "    item [1]:", '        class = "IntervalTier"', '        name = "ségment"',
            "        xmin = 0", "        xmax = 0.8", "        intervals: size = 1",
            "        intervals [1]:", "            xmin = 0", "            xmax = 0.8",
            '            text = "name = x"')
  f2 <- tempfile(fileext = ".TextGrid")
  con <- file(f2, "wb")
  writeBin(as.raw(c(0xff, 0xfe)), con)
  writeBin(iconv(paste(long, collapse = "\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]], con)
  close(con)
  expect_equal(tg_tier_names(f2), "ségment")

  expect_equal(tg_tier_names(c(f1, f2, f1, NA, "")),
               c('say "hi"', "声调", "ségment"))
})

test_that("tg_tier_names skips unreadable files", {
  expect_equal(tg_tier_names(c(tempfile(), NA, "")), character(0))
})

# ---------- read_textgrid -----------------------------------------------------

# Praat-written TextGrids. Praat saves any file with a non-ASCII character as
# UTF-16BE with a byte-order mark (its default "try ASCII, then UTF-16"); the
# *_utf8 twins are the same TextGrids saved with the UTF-8 preference.
tg_fixture <- function(name) test_path("fixtures", paste0("tg_", name, ".TextGrid"))
# Tier contents only: the class attribute carries the file name.
tg_tiers <- function(tg) lapply(unclass(tg), function(t)
  t[c("name", "type", intersect(c("t1", "t2", "t", "label"), names(t)))])

test_that("read_textgrid reads Praat's UTF-16BE files like their UTF-8 twins", {
  skip_if_not_installed("rPraat")
  expect_identical(readBin(tg_fixture("zh_long_utf16be"), "raw", 2L), as.raw(c(0xFE, 0xFF)))
  pairs <- list(c("zh_long_utf16be",     "zh_long_utf8"),
                c("zh_short_utf16be",    "zh_long_utf8"),
                c("zhname_short_utf16be", "zhname_long_utf8"),
                c("ipa_long_utf16be",    "ipa_long_utf8"),
                c("nfd_long_utf16be",    "nfd_long_utf8"))
  for (p in pairs) {
    a <- read_textgrid(tg_fixture(p[1]))
    b <- read_textgrid(tg_fixture(p[2]))
    expect_null(a$reason)
    expect_identical(a$encoding, "UTF-16BE")
    expect_identical(b$encoding, "UTF-8")
    expect_identical(tg_tiers(a$tg), tg_tiers(b$tg), label = p[1])
  }
})

test_that("read_textgrid returns Praat's labels and tier names exactly", {
  skip_if_not_installed("rPraat")
  tg <- read_textgrid(tg_fixture("zh_long_utf16be"))$tg
  expect_identical(names(tg), c("phone", "word", "tone", "event"))
  expect_identical(tg$phone$label, c("", "pʰ", "ɔ", ""))
  expect_identical(tg$word$label,  c("", "怕", ""))
  expect_identical(tg$tone$label,  c("", "阴平", ""))
  expect_identical(tg$event$label, "高")
  expect_identical(class(tg)[["name"]], "tg_zh_long_utf16be.TextGrid")

  zn <- read_textgrid(tg_fixture("zhname_short_utf16be"))$tg
  expect_identical(names(zn), c("phone", "汉字", "声调", "事件"))

  ipa <- read_textgrid(tg_fixture("ipa_long_utf16be"))$tg$phone$label
  expect_identical(ipa[-1], c("pʰ", "ã", "mā", "mā",
                              "a˥˩", "ŋ̍", "aː"))
  # Decomposed (NFD) labels are kept as written, not normalised.
  nfd <- read_textgrid(tg_fixture("nfd_long_utf16be"))$tg$phone$label
  expect_identical(nfd, c("", "ã", "mā", "mā"))
})

test_that("read_textgrid undoes Praat's doubled quotes and keeps multi-line labels", {
  skip_if_not_installed("rPraat")
  tg <- read_textgrid(tg_fixture("quote_newline_utf16be"))$tg
  expect_identical(tg$word$label[2:3],
                   c("说 \"好\"", "第一行\n第二行"))

  short <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', "",
             "0", "0.8", "<exists>", "1",
             '"IntervalTier"', '"say ""hi"""', "0", "0.8", "2",
             "0", "0.4", '"a ""b"""', "0.4", "0.8", '""')
  f <- tempfile(fileext = ".TextGrid")
  writeLines(short, f)
  expect_identical(names(read_textgrid(f)$tg), 'say "hi"')
  expect_identical(read_textgrid(f)$tg[[1]]$label, c('a "b"', ""))
  expect_identical(rPraat::tg.read(f)[[1]]$label, c('a ""b""', ""))
})

test_that("read_textgrid handles other encodings and line ends", {
  skip_if_not_installed("rPraat")
  ref <- tg_tiers(read_textgrid(tg_fixture("zh_long_utf8"))$tg)
  be  <- readBin(tg_fixture("zh_long_utf16be"), "raw", 1e5)
  txt <- rawToChar(readBin(tg_fixture("zh_long_utf8"), "raw", 1e5))
  Encoding(txt) <- "UTF-8"
  le  <- iconv(txt, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]
  variants <- list(
    "UTF-16BE" = be[-(1:2)],                                    # no BOM
    "UTF-16LE" = c(as.raw(c(0xFF, 0xFE)), le),
    "UTF-16LE" = le,                                            # no BOM
    "UTF-8"    = c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(txt)),
    "UTF-8"    = charToRaw(gsub("\n", "\r\n", txt, fixed = TRUE)))
  for (i in seq_along(variants)) {
    f <- tempfile(fileext = ".TextGrid")
    writeBin(variants[[i]], f)
    r <- read_textgrid(f)
    expect_identical(r$encoding, names(variants)[i])
    expect_identical(tg_tiers(r$tg), ref)
  }
  # An 8-bit file (Praat's "try ISO Latin-1" preference).
  lat <- read_textgrid(tg_fixture("latin1"))
  expect_null(lat$reason)
  expect_identical(lat$tg[[1]]$label[nzchar(lat$tg[[1]]$label)], "é")
})

test_that("read_textgrid says why a file cannot be read", {
  skip_if_not_installed("rPraat")
  bin <- read_textgrid(tg_fixture("binary"))
  expect_null(bin$tg)
  expect_match(bin$reason, "Praat's binary format; in Praat, use Save as text file", fixed = TRUE)
  chr <- read_textgrid(tg_fixture("chrono_utf16be"))
  expect_null(chr$tg)
  expect_match(chr$reason, "Praat's chronological format", fixed = TRUE)

  expect_match(read_textgrid(tempfile())$reason, "not found")
  empty <- tempfile(fileext = ".TextGrid")
  file.create(empty)
  expect_match(read_textgrid(empty)$reason, "empty")
  other <- tempfile(fileext = ".TextGrid")
  writeLines(c("token,f0", "a,1"), other)
  expect_match(read_textgrid(other)$reason, "not a Praat TextGrid text file")
  # A TextGrid header with a body rPraat cannot parse.
  broken <- tempfile(fileext = ".TextGrid")
  writeLines(c('File type = "ooTextFile"', 'Object class = "TextGrid"', "", "xmin = 0"), broken)
  r <- read_textgrid(broken)
  expect_null(r$tg)
  expect_match(r$reason, "could not be parsed")
})

test_that("read_textgrid matches rPraat::tg.read on UTF-8 TextGrids", {
  skip_if_not_installed("rPraat")
  # rPraat cannot open the non-ASCII file names outside a UTF-8 locale.
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "needs a UTF-8 locale")
  dirs <- test_path("..", "..", "test", c("cs_wavs", "cong_wavs"))
  files <- list.files(dirs, "\\.TextGrid$", full.names = TRUE)
  skip_if(length(files) == 0, "sample TextGrids not available")
  unquote <- function(x) if (is.character(x)) gsub('""', '"', x, fixed = TRUE) else x
  for (f in files) {
    ref <- rPraat::tg.read(f)
    for (k in seq_along(ref)) {
      ref[[k]]$name  <- unquote(ref[[k]]$name)
      ref[[k]]$label <- unquote(ref[[k]]$label)
    }
    names(ref) <- vapply(ref, `[[`, "", "name")
    r <- read_textgrid(f)
    expect_identical(r$encoding, "UTF-8")
    expect_identical(r$tg, ref, label = basename(f))
  }
})

test_that("tg_interval_tiers reads UTF-16 TextGrids and reports unreadable ones", {
  skip_if_not_installed("rPraat")
  expect_identical(tg_interval_tiers(tg_fixture("zh_long_utf16be")),
                   c("phone", "tone", "word"))
  expect_setequal(tg_interval_tiers(tg_fixture("zhname_short_utf16be")),
                  c("phone", "汉字", "声调"))

  bin <- tg_fixture("binary")
  chr <- tg_fixture("chrono_utf16be")
  none <- tg_interval_tiers(c(bin, chr))
  expect_length(none, 0)
  bad <- attr(none, "unreadable")
  expect_named(bad, c(bin, chr))
  expect_match(bad[[1]], "binary format")
  expect_match(bad[[2]], "chronological format")

  mixed <- tg_interval_tiers(c(tg_fixture("zh_long_utf16be"), bin))
  expect_equal(as.vector(mixed), c("phone", "tone", "word"))
  expect_named(attr(mixed, "unreadable"), bin)
})

test_that("tg_tier_names agrees with read_textgrid, BOM or not", {
  skip_if_not_installed("rPraat")
  f <- tg_fixture("zhname_short_utf16be")
  expect_identical(tg_tier_names(f), names(read_textgrid(f)$tg))
  nobom <- tempfile(fileext = ".TextGrid")
  writeBin(readBin(f, "raw", 1e5)[-(1:2)], nobom)
  expect_identical(tg_tier_names(nobom), names(read_textgrid(f)$tg))
  expect_identical(tg_tier_names(c(tg_fixture("binary"), tg_fixture("chrono_utf16be"))),
                   character(0))
})

test_that("attach_landmarks reads UTF-16 TextGrids and reports unreadable ones", {
  skip_if_not_installed("rPraat")
  times <- seq(0, 0.5, by = 0.01)
  df <- data.frame(token = rep(c("be", "u8", "bin"), each = length(times)),
                   time = rep(times, 3), f0 = 200)
  audio <- data.frame(basename = c("be", "u8", "bin"),
                      tg_path = c(tg_fixture("zh_long_utf16be"), tg_fixture("zh_long_utf8"),
                                  tg_fixture("binary")))
  out <- attach_landmarks(df, audio, c("phone", "word"))
  be <- out[out$token == "be", -1]
  u8 <- out[out$token == "u8", -1]
  rownames(be) <- rownames(u8) <- NULL
  expect_identical(be, u8)
  expect_true(all(!is.na(be$phone)))
  expect_setequal(unique(be$word), c("", "怕"))
  expect_true(all(is.na(out$phone[out$token == "bin"])))
  bad <- attr(out, "tg_unreadable")
  expect_named(bad, "bin")
  expect_match(bad[["bin"]], "binary format")

  # No attribute when every TextGrid could be read.
  ok <- attach_landmarks(df[df$token != "bin", ], audio, "phone")
  expect_null(attr(ok, "tg_unreadable"))
  # Numeric tokens name the reason by token, not by position.
  num <- attach_landmarks(data.frame(token = 7, time = 0.1),
                          data.frame(basename = "7", tg_path = tg_fixture("binary")),
                          "phone")
  expect_named(attr(num, "tg_unreadable"), "7")
})

# Long-format TextGrid with one interval tier, written in `enc`, no BOM.
tg_8bit <- function(labels, tier = "mot", enc = "latin1") {
  n <- length(labels)
  b <- seq(0, 1, length.out = n + 1)
  txt <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', "", "xmin = 0 ",
           "xmax = 1 ", "tiers? <exists> ", "size = 1 ", "item []: ", "    item [1]:",
           '        class = "IntervalTier" ', sprintf('        name = "%s" ', tier),
           "        xmin = 0 ", "        xmax = 1 ", sprintf("        intervals: size = %d ", n),
           unlist(lapply(seq_len(n), function(i) c(
             sprintf("        intervals [%d]:", i), sprintf("            xmin = %s ", b[i]),
             sprintf("            xmax = %s ", b[i + 1]),
             sprintf('            text = "%s" ', labels[i])))))
  f <- tempfile(fileext = ".TextGrid")
  writeBin(iconv(paste0(paste(txt, collapse = "\n"), "\n"), "UTF-8", enc, toRaw = TRUE)[[1]], f)
  f
}

test_that("8-bit TextGrids read in the right code page, with a note", {
  skip_if_not_installed("rPraat")
  # Accented letters inside words also form valid GBK pairs (Müller would
  # read as M + a rare hanzi + ler); Praat's Latin-1 files must stay Latin.
  for (labs in list(c("Müller", "Straße"), c("señor", "año"),
                    c("élève", "étés"), c("café", "ça"))) {
    f <- tg_8bit(labs)
    r <- read_textgrid(f)
    expect_identical(r$encoding, "CP1252", info = labs[1])
    expect_identical(r$tg$mot$label, labs)
    expect_match(r$note, "so it was read as Western European (Windows-1252)", fixed = TRUE)
    expect_match(r$note, sprintf("'%s' is not saved as UTF-8 or UTF-16", basename(f)), fixed = TRUE)
  }
  expect_match(read_textgrid(f, name = "ma1.TextGrid")$note, "'ma1.TextGrid' is not", fixed = TRUE)
  # A Latin-1 tier name: the fast scan used to read it so, and still does.
  f <- tg_8bit(c("a", "b"), tier = "Wörter")
  expect_identical(tg_tier_names(f), "Wörter")
  expect_identical(names(read_textgrid(f)$tg), "Wörter")

  skip_if(is.na(iconv("声", "UTF-8", "GB18030")), "GB18030 not supported by iconv here")
  gb <- read_textgrid(tg_8bit(c("声调", "mā"), tier = "声调", enc = "GB18030"))
  expect_identical(gb$encoding, "GB18030")
  expect_identical(gb$tg[[1]]$label, c("声调", "mā"))
  skip_if(is.na(iconv("聲", "UTF-8", "BIG5")), "BIG5 not supported by iconv here")
  b5 <- read_textgrid(tg_8bit(c("聲調", "音節"), tier = "聲調", enc = "BIG5"))
  expect_identical(b5$encoding, "BIG5")
  expect_identical(names(b5$tg), "聲調")
  expect_match(b5$note, "Chinese (Big5)", fixed = TRUE)

  # UTF-8 and UTF-16 need no guess, so no note.
  expect_null(read_textgrid(tg_fixture("zh_long_utf8"))$note)
  expect_null(read_textgrid(tg_fixture("zh_long_utf16be"))$note)
})

test_that("tg_interval_tiers and attach_landmarks report guessed code pages", {
  skip_if_not_installed("rPraat")
  lat <- tg_8bit(c("Müller", "Straße"))
  u8  <- tg_fixture("zh_long_utf8")
  tiers <- tg_interval_tiers(c(lat, u8))
  expect_identical(attr(tiers, "guessed"),
                   stats::setNames("Western European (Windows-1252)", lat))
  expect_null(attr(tg_interval_tiers(u8), "guessed"))

  df <- data.frame(token = rep(c("lat", "u8"), each = 3), time = rep(c(0.1, 0.3, 0.7), 2))
  audio <- data.frame(basename = c("lat", "u8"), tg_path = c(lat, u8))
  out <- suppressWarnings(attach_landmarks(df, audio, "mot"))
  expect_identical(attr(out, "tg_guessed"), c(lat = "Western European (Windows-1252)"))
  expect_identical(out$mot[1:3], c("Müller", "Müller", "Straße"))
  expect_null(attr(attach_landmarks(df[4:6, ], audio, "phone"), "tg_guessed"))
})

test_that("a UTF-8 TextGrid with a stray byte keeps its UTF-8 labels", {
  skip_if_not_installed("rPraat")
  # One label typed in Latin-1 (caf + 0xE9): reading the whole file as
  # Windows-1252 would garble the Chinese label.
  f <- tg_8bit(c("阴平", "cafX"), tier = "声调", enc = "UTF-8")
  b <- readBin(f, "raw", 1e4)
  b[b == charToRaw("X")] <- as.raw(0xE9)
  for (with_bom in c(FALSE, TRUE)) {
    writeBin(c(if (with_bom) as.raw(c(0xEF, 0xBB, 0xBF)), b), f)
    r <- read_textgrid(f, name = "ma1.TextGrid")
    expect_identical(r$encoding, "UTF-8")
    expect_identical(names(r$tg), "声调")
    expect_identical(r$tg[[1]]$label, c("阴平", "caf�"))
    expect_identical(r$stray_lines, 22L)
    expect_identical(r$note, paste("'ma1.TextGrid' is UTF-8 except on line 22, where some text",
                                   "is not valid UTF-8 and is shown as �."))
    expect_identical(tg_tier_names(f), "声调")
  }
  tiers <- tg_interval_tiers(f)
  expect_identical(attr(tiers, "stray"), stats::setNames("line 22", f))
  expect_null(attr(tiers, "guessed"))
  out <- attach_landmarks(data.frame(token = "ma1", time = c(0.2, 0.7)),
                          data.frame(basename = "ma1", tg_path = f), "声调")
  expect_identical(out[["声调"]], c("阴平", "caf�"))
  expect_identical(attr(out, "tg_stray"), c(ma1 = "line 22"))
  expect_null(attr(out, "tg_guessed"))
  # A file with a UTF-8 mark that is mostly not UTF-8 is still refused.
  lat <- readBin(tg_8bit(c("Müller", "Straße")), "raw", 1e4)
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), lat), f)
  expect_match(read_textgrid(f)$reason, "not valid UTF-8", fixed = TRUE)
})

test_that("a TextGrid with zero-byte padding reads", {
  skip_if_not_installed("rPraat")
  f <- tempfile(fileext = ".TextGrid")
  writeBin(c(readBin(tg_fixture("zh_long_utf8"), "raw", 1e5), as.raw(rep(0, 32))), f)
  expect_identical(tg_tiers(read_textgrid(f)$tg),
                   tg_tiers(read_textgrid(tg_fixture("zh_long_utf8"))$tg))
  # Padding that starts past the first 4 KB
  long <- tempfile(fileext = ".TextGrid")
  writeBin(c(readBin(tg_fixture("zh_long_utf8"), "raw", 1e5), charToRaw(strrep(" ", 5000)),
             as.raw(rep(0, 32))), long)
  expect_identical(tg_tiers(read_textgrid(long)$tg),
                   tg_tiers(read_textgrid(tg_fixture("zh_long_utf8"))$tg))
})

test_that("plain UTF-8 TextGrids go to rPraat as they are, others via a UTF-8 copy", {
  skip_if_not_installed("rPraat")
  direct <- function(f) isTRUE(attr(tg_decode_text(f), "direct"))
  expect_true(direct(tg_fixture("zh_long_utf8")))
  expect_false(direct(tg_fixture("zh_long_utf16be")))
  expect_false(direct(tg_8bit("café")))
  # The old short header has the same layout but rPraat wants the new one.
  short <- tempfile(fileext = ".TextGrid")
  writeLines(c('File type = "ooTextFile short"', '"TextGrid"', "", "0", "0.8", "<exists>", "1",
               '"IntervalTier"', '"w"', "0", "0.8", "1", "0", "0.8", '"a"'), short)
  expect_false(direct(short))
  expect_identical(read_textgrid(short)$tg$w$label, "a")
  # Old Mac line ends (CR only)
  cr <- tempfile(fileext = ".TextGrid")
  writeBin(charToRaw(gsub("\n", "\r", rawToChar(readBin(tg_fixture("zh_long_utf8"), "raw", 1e5)),
                          fixed = TRUE)), cr)
  expect_false(direct(cr))
  expect_identical(tg_tiers(read_textgrid(cr)$tg),
                   tg_tiers(read_textgrid(tg_fixture("zh_long_utf8"))$tg))
})

# ---------- landmark_prefixes -------------------------------------------------

# One-token TextGrid with the given interval tiers; tier k is labelled
# "<k>a" over 0.1-0.3 s and "<k>b" over 0.3-0.6 s.
tg_with_tiers <- function(tiers) {
  tg <- rPraat::tg.createNewTextGrid(0, 0.8)
  for (k in seq_along(tiers)) {
    tg <- rPraat::tg.insertNewIntervalTier(tg, k, tiers[k])
    tg <- rPraat::tg.insertInterval(tg, k, 0.1, 0.3, paste0(k, "a"))
    tg <- rPraat::tg.insertInterval(tg, k, 0.3, 0.6, paste0(k, "b"))
  }
  f <- tempfile(fileext = ".TextGrid")
  rPraat::tg.write(tg, f)
  f
}
lm_df    <- data.frame(token = "w1", time = c(0.2, 0.45, 0.7), f0 = c(120, 130, 140))
lm_audio <- function(f) data.frame(basename = "w1", tg_path = f)

test_that("landmark_prefixes keeps tier names in any script", {
  zh <- c("声调", "音节")
  expect_identical(landmark_prefixes(zh), stats::setNames(zh, zh))
  expect_identical(landmark_prefixes(rev(zh)), stats::setNames(rev(zh), rev(zh)))
  ipa <- c("ʂəŋtjɑʊ", "tʰa", "ʔa")
  expect_identical(landmark_prefixes(ipa), stats::setNames(ipa, ipa))
  expect_identical(unname(landmark_prefixes(c("Tone 声调", "声调 (2)", "!!"))),
                   c("tone_声调", "声调_2", "tier"))
  # A tier named like another tier's column does not take it.
  expect_identical(unname(landmark_prefixes(c("声调_start", "声调"))),
                   c("声调_start_2", "声调"))
})

test_that("tiers with the same prefix are numbered whatever the order", {
  tiers <- c("syllable", "Syllable", "syllable!")
  want  <- c(syllable = "syllable", Syllable = "syllable_2", "syllable!" = "syllable_3")
  for (o in list(1:3, 3:1, c(2L, 3L, 1L))) {
    expect_identical(landmark_prefixes(tiers[o]), want[tiers[o]])
  }
  # A subset gets the same prefixes when all_tiers is the same.
  for (t in tiers) expect_identical(landmark_prefixes(t, all_tiers = rev(tiers)), want[t])
  expect_identical(landmark_prefixes(c("syllable!", "Syllable"), all_tiers = tiers),
                   want[c("syllable!", "Syllable")])
  # Without the corpus's tiers, a lone tier keeps the plain prefix.
  expect_identical(landmark_prefixes("Syllable"), c(Syllable = "syllable"))
})

test_that("tiers named after core columns get _2 and leave them intact", {
  expect_identical(landmark_prefixes(c("time", "token", "f0", "Time")),
                   c(time = "time_2", token = "token_2", f0 = "f0_2", Time = "time_3"))
  skip_if_not_installed("rPraat")
  f <- tg_with_tiers(c("time", "token", "f0"))
  out <- attach_landmarks(lm_df, lm_audio(f), c("time", "token", "f0"))
  expect_identical(out$token, lm_df$token)
  expect_identical(out$time, lm_df$time)
  expect_identical(out$f0, lm_df$f0)
  expect_equal(out$time_2, c("1a", "1b", ""))
  expect_equal(out$token_2, c("2a", "2b", ""))
  expect_equal(out$f0_2, c("3a", "3b", ""))
  expect_identical(attr(out, "renamed_tiers"),
                   c(time = "time_2", token = "token_2", f0 = "f0_2"))
})

test_that("a tier named like another tier's axis column does not take it", {
  # syllable_t01, syllable_tseq and syllable_tbound belong to tier syllable
  for (sfx in c("_t01", "_tseq", "_tbound", "_f0_dur")) {
    nm <- paste0("syllable", sfx)
    expect_identical(unname(landmark_prefixes(c(nm, "syllable"))),
                     c(paste0(nm, "_2"), "syllable"), info = sfx)
  }
  expect_identical(unname(landmark_prefixes("syllable_tbound")), "syllable_tbound")
})

test_that("a tier named voiced_run leaves the voiced_run column intact", {
  # drop_rows_without_f0() adds voiced_run, so a tier must not take it
  expect_true("voiced_run" %in% eval(formals(landmark_prefixes)$reserved))
  expect_identical(landmark_prefixes(c("voiced_run", "Voiced run")),
                   c(voiced_run = "voiced_run_2",
                     "Voiced run" = "voiced_run_3"))
  # a tier named voiced keeps its name: none of its columns is voiced_run
  expect_identical(landmark_prefixes("voiced"), c(voiced = "voiced"))
  skip_if_not_installed("rPraat")
  f <- tg_with_tiers("voiced_run")
  df <- data.frame(token = "w1", time = c(0.2, 0.45, 0.7),
                   f0 = c(120, 130, 140), voiced_run = 1L)
  out <- attach_landmarks(df, lm_audio(f), "voiced_run")
  expect_identical(out$voiced_run, df$voiced_run)
  expect_equal(out$voiced_run_2, c("1a", "1b", ""))
  expect_identical(attr(out, "renamed_tiers"), c(voiced_run = "voiced_run_2"))
})

test_that("ASCII tier names keep the column names of earlier versions", {
  old <- function(nm) {   # the rule before landmark_prefixes()
    p <- tolower(gsub("[^A-Za-z0-9]+", "_", nm)); p <- gsub("^_+|_+$", "", p)
    if (!nzchar(p)) "tier" else p
  }
  asc <- c("syllable", "Syllable (broad)", "phones", "words", "rhyme", "Tone-2",
           "1st", " vowel ", "Seg.Tier", "!!!")
  expect_identical(unname(landmark_prefixes(asc)), unname(vapply(asc, old, "")))
  # Every three-character name from a mix of letters, digits and symbols, on
  # its own, unless it clashes with a core column.
  ch  <- c("a", "Z", "0", "_", "-", " ", "!", ".")
  nms <- apply(expand.grid(ch, ch, ch, stringsAsFactors = FALSE), 1, paste, collapse = "")
  nms <- nms[!vapply(nms, old, "") %in% eval(formals(landmark_prefixes)$reserved)]
  new <- vapply(nms, function(x) landmark_prefixes(x)[[1]], "")
  expect_identical(unname(new), unname(vapply(nms, old, "")))
})

test_that("prefixes from CJK, IPA and Thai names are syntactic", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "needs a UTF-8 locale")
  nms <- c("声调", "音节", "성조", "ʂəŋtjɑʊ", "tʰa", "ˈsyl", "ʔa",
           "ɕɥɛ̃", "n̩", "ã", "a¹",
           "วรรณยุกต์",   # Thai, with marks
           "स्वर")                                  # Devanagari
  p <- landmark_prefixes(nms)
  expect_identical(make.names(p), unname(p))
  expect_length(unique(p), length(nms))
})

test_that("landmark_prefixes puts names in NFC before dropping marks", {
  skip_if_not_installed("utf8")
  expect_identical(unname(landmark_prefixes("ã")), "ã")
  expect_identical(unname(landmark_prefixes(c("ã", "ã"))),
                   c("ã_2", "ã"))
})

test_that("attach_landmarks adds a column set for each Chinese tier", {
  skip_if_not_installed("rPraat")
  df <- data.frame(token = "zh", time = c(0.03, 0.1, 0.3, 0.5), f0 = 200)
  cols <- paste0(rep(c("汉字", "声调"), each = 4), c("", "_start", "_end", "_i"))
  for (f in c(tg_fixture("zhname_long_utf8"), tg_fixture("zhname_short_utf16be"))) {
    audio <- data.frame(basename = "zh", tg_path = f)
    for (tiers in list(c("汉字", "声调"), c("声调", "汉字"))) {
      out <- attach_landmarks(df, audio, tiers, all_tiers = c("phone", "汉字", "声调"))
      expect_setequal(setdiff(names(out), names(df)), cols)
      expect_equal(out[["汉字"]], c("", "怕", "怕", ""))
      expect_equal(out[["声调"]], c("", "阴平", "阴平", ""))
      expect_equal(out[["声调_start"]], c(0, 0.063, 0.063, 0.442))
      expect_equal(out[["声调_i"]], c(NA, 1L, 1L, NA))
      expect_null(attr(out, "renamed_tiers"))
      expect_null(attr(out, "missing_tiers"))
    }
  }
})

test_that("attach_landmarks reports renamed tiers and tiers in no TextGrid", {
  skip_if_not_installed("rPraat")
  f <- tg_with_tiers(c("syllable", "Syllable"))
  expect_warning(
    out <- attach_landmarks(lm_df, lm_audio(f), c("Syllable", "音节"),
                            all_tiers = c("syllable", "Syllable")),
    "No TextGrid has an interval tier")
  expect_equal(out$syllable_2, c("2a", "2b", ""))
  expect_false("syllable" %in% names(out))
  expect_true(all(is.na(out[["音节"]])))
  expect_identical(attr(out, "renamed_tiers"), c(Syllable = "syllable_2"))
  expect_identical(attr(out, "missing_tiers"), "音节")

  # No such warning when no TextGrid could be read at all.
  expect_no_warning(bin <- attach_landmarks(lm_df, lm_audio(tg_fixture("binary")), "音节"))
  expect_null(attr(bin, "missing_tiers"))
})

test_that("renamed_tiers lists only tiers given _2 or the 'tier' fallback", {
  no_tg <- data.frame(basename = "w1", tg_path = NA_character_)
  nfd   <- intToUtf8(c(0x61, 0x303))   # "a" + combining tilde; NFC is one letter
  # Lowercasing, "_" for other characters and NFC are not reported.
  tiers <- c("Syllable", "Tone", "Syllable (broad)", nfd, "tier", "Tier 2")
  out <- attach_landmarks(lm_df, no_tg, tiers)
  expect_null(attr(out, "renamed_tiers"))
  # No TextGrid matched: the columns are still added, all NA.
  for (p in c("syllable", "tone", "syllable_broad", "tier", "tier_2")) {
    expect_true(all(is.na(out[[p]])), label = p)
    expect_true(all(is.na(out[[paste0(p, "_i")]])), label = p)
  }
  expect_identical(out[names(lm_df)], lm_df)

  out <- attach_landmarks(lm_df, no_tg, c("Syllable", "syllable", "!!!", "tier", "Time"))
  expect_identical(attr(out, "renamed_tiers"),
                   c(Syllable = "syllable_2", "!!!" = "tier_2", Time = "time_2"))
  expect_identical(attr(attach_landmarks(lm_df, no_tg, "!!!"), "renamed_tiers"),
                   c("!!!" = "tier"))
})
