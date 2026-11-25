# Tests for the gap helpers in R/gaps.R: has_f0() and f0_runs().

# Frame times as a file stores them: written to 4 decimals and read back.
as_written <- function(x) as.numeric(sprintf("%.4f", x))

# Last run index of each token, in order of first appearance.
last_run <- function(token, run) {
  as.integer(tapply(run, factor(token, levels = unique(token)), max))
}

test_that("has_f0() counts only positive numbers in Hz, any number otherwise", {
  x <- c(200, 0, -1, NA, Inf, NaN, -3.5)
  expect_identical(has_f0(x),
                   c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(has_f0(x, hz = FALSE),
                   c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE))
  # text and factors are read by value
  expect_identical(has_f0(c("200", "", "NA", "-1")),
                   c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(has_f0(factor(c("150.5", "0", "--undefined--"))),
                   c(TRUE, FALSE, FALSE))
  expect_identical(has_f0(numeric(0)), logical(0))
})

test_that("f0_runs() finds one-row gaps, whose step ratio is exactly 2 or just under", {
  # 10 ms frames written to 4 decimals at several offsets, as in f0_long.csv;
  # each token misses one frame. The step across the gap is then exactly 2 or
  # just under 2 times the usual step, so a 2x rule would miss most of them.
  offs <- c(0.02, 0.025, 0.0201, 0.0206, 0.0212, 0.0228, 0.0241, 0.0325)
  cases <- expand.grid(off = offs, drop = 3:25)
  d <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i) {
    tt <- as_written(cases$off[i] + 0.01 * (0:29))
    data.frame(token = paste0("t", i), time = tt[-cases$drop[i]],
               before = seq_len(29) < cases$drop[i])
  }))
  ratio <- vapply(split(d$time, factor(d$token, levels = unique(d$token))),
                  function(tt) max(diff(tt)) / stats::median(diff(tt)),
                  numeric(1))
  expect_true(any(ratio == 2))
  expect_gt(mean(ratio <= 2), 0.8)

  r <- f0_runs(d$token, d$time, TRUE)
  g <- attr(r, "grid")
  expect_true(g$regular)
  expect_true(g$common)
  expect_equal(g$step, 0.01, tolerance = 1e-9)
  expect_identical(as.integer(r), ifelse(d$before, 1L, 2L))
  expect_identical(g$n_gaps_time, nrow(cases))

  # whole milliseconds: 10, 20, 40 gives a ratio of exactly 2
  ms <- c(10, 20, 30, 40, 50, 70, 80, 90)
  expect_identical(as.integer(f0_runs("a", ms, TRUE)),
                   c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L))
  # a gap ratio of 2 misses it
  expect_identical(as.integer(f0_runs("a", ms, TRUE, gap_ratio = 2)),
                   rep(1L, 8))
})

test_that("f0_runs() marks a step longer than 1.5 times the reference, and not one of exactly 1.5", {
  # 150 tokens of 25 frames 10 ms apart, in ms (halves are exact in binary).
  # In three tokens one step is longer: 14.5 ms (1.45 x), 15 ms (1.5 x) and
  # 15.5 ms (1.55 x). Three such steps in 3,600 are within the 0.1% that may
  # lie between 1.25 and 1.75 times the reference, so the grid is regular.
  mk <- function(extra) {
    do.call(rbind, lapply(seq_along(extra), function(i) {
      tt <- 5 + 10 * (0:24)
      tt[13:25] <- tt[13:25] + extra[i]
      data.frame(token = sprintf("k%03d", i), time = tt)
    }))
  }
  d <- mk(c(4.5, 5, 5.5, rep(0, 147)))
  r <- f0_runs(d$token, d$time, TRUE)
  g <- attr(r, "grid")
  expect_true(g$regular)
  expect_equal(g$step, 10)
  expect_identical(g$n_steps, 3600L)
  expect_identical(g$ambiguous, 3L)
  expect_identical(g$n_gaps_time, 1L)
  expect_identical(last_run(d$token, r), c(1L, 1L, 2L, rep(1L, 147)))
  expect_identical(as.integer(r[d$token == "k003"]), rep(1:2, c(12, 13)))
  # gap_ratio sets the limit
  expect_identical(last_run(d$token, f0_runs(d$token, d$time, TRUE,
                                             gap_ratio = 1.4))[1:4],
                   c(2L, 2L, 2L, 1L))
  expect_identical(last_run(d$token, f0_runs(d$token, d$time, TRUE,
                                             gap_ratio = 1.6))[1:4],
                   c(1L, 1L, 1L, 1L))
  # The same from the screen: one gap, read from the time steps
  d$f0 <- 200
  s <- attr(screen_f0_rows(d), "summary")
  expect_identical(s[["gaps_time"]], 1L)

  # A fourth such step (4 in 3,600) puts one step and two too close: no
  # gaps are read from the time steps, though the 15.5 ms step is still
  # counted
  d4 <- mk(c(4.5, 5, 5.5, 3, rep(0, 146)))
  r4 <- f0_runs(d4$token, d4$time, TRUE)
  expect_false(attr(r4, "grid")$regular)
  expect_identical(attr(r4, "grid")$ambiguous, 4L)
  expect_identical(attr(r4, "grid")$n_gaps_time, 1L)
  expect_true(all(r4 == 1L))
})

test_that("f0_runs() gives the same runs from rows without f0 and from time steps", {
  tt <- as_written(0.0325 + 0.01 * (0:39))
  f0 <- c(NA, NA, 200:209, NA, 211:220, NA, NA, NA, 224:235, NA, NA)
  d <- data.frame(token = rep(c("a", "b"), each = 40), time = c(tt, tt),
                  f0 = c(f0, rev(f0)))
  keep <- has_f0(d$f0)
  kept <- d[keep, ]

  r_rows <- f0_runs(d$token, d$time, keep)
  r_time <- f0_runs(kept$token, kept$time, TRUE)
  expect_identical(as.integer(r_rows[keep]), as.integer(r_time))
  expect_true(all(is.na(r_rows[!keep])))
  expect_identical(last_run(kept$token, r_time), c(3L, 3L))
  expect_identical(attr(r_rows, "grid")$n_gaps_rows, 4L)
  expect_identical(attr(r_time, "grid")$n_gaps_rows, 0L)
  expect_identical(attr(r_time, "grid")$n_gaps_time, 4L)

  # gap_ratio = Inf marks only the gaps from rows
  expect_identical(as.integer(f0_runs(d$token, d$time, keep,
                                      gap_ratio = Inf)[keep]),
                   as.integer(r_time))
  expect_identical(as.integer(f0_runs(kept$token, kept$time, TRUE,
                                      gap_ratio = Inf)),
                   rep(1L, nrow(kept)))
})

test_that("f0_runs() keeps the input order and gives NA where time is NA", {
  tt <- as_written(0.025 + 0.01 * (0:19))
  d <- data.frame(token = rep(c("x", "y", "z"), each = 20),
                  time = rep(tt, 3),
                  f0 = c(rep(200, 20),
                         c(NA, 200, 201, NA, NA, 202:216),
                         c(rep(190, 8), NA, rep(191, 11))))
  keep <- has_f0(d$f0)
  r <- f0_runs(d$token, d$time, keep)
  expect_identical(as.integer(r[d$token == "y" & keep]),
                   c(1L, 1L, rep(2L, 15)))

  set.seed(42)
  p <- sample(nrow(d))
  rp <- f0_runs(d$token[p], d$time[p], keep[p])
  expect_identical(as.integer(rp), as.integer(r[p]))
  expect_identical(attr(rp, "grid"), attr(r, "grid"))

  # tokens given as a factor, or interleaved, give the same runs
  expect_identical(as.integer(f0_runs(factor(d$token), d$time, keep)),
                   as.integer(r))

  # a row with NA time gets NA, and the other rows get the runs they would
  # get without it: on a sample with f0 (row 5) the time step then shows a
  # gap; inside a gap (row 24) nothing changes
  na_t <- c(5, 24)
  d2 <- d
  d2$time[na_t] <- NA
  r2 <- f0_runs(d2$token, d2$time, keep)
  expect_true(all(is.na(r2[na_t])))
  expect_identical(as.integer(r2[-na_t]),
                   as.integer(f0_runs(d$token[-na_t], d$time[-na_t],
                                      keep[-na_t])))
  expect_identical(as.integer(r2[d$token == "x"]),
                   c(1L, 1L, 1L, 1L, NA, rep(2L, 15)))
  expect_identical(as.integer(r2[d$token == "y"]),
                   as.integer(r[d$token == "y"]))

  # a row without f0 at the same time as a sample with f0 is not a gap
  tie <- data.frame(time = c(0.01, 0.02, 0.02, 0.03, 0.04, 0.05),
                    f0 = c(200, 200, NA, 200, 200, 200))
  expect_identical(as.integer(f0_runs("a", tie$time, has_f0(tie$f0))),
                   c(1L, 1L, NA, 1L, 1L, 1L))

  # nothing to number
  r0 <- f0_runs(c("a", "a"), c(0.1, 0.2), FALSE)
  expect_identical(as.integer(r0), c(NA_integer_, NA_integer_))
  expect_false(attr(r0, "grid")$regular)
  expect_identical(attr(f0_runs(character(0), numeric(0), logical(0)),
                        "grid")$n_steps, 0L)
})

test_that("f0_runs() checks its arguments", {
  expect_error(f0_runs(c("a", "b"), c(0.1, 0.2, 0.3), TRUE), "same length")
  expect_error(f0_runs("a", c(0.1, 0.2), TRUE, gap_ratio = 1), "above 1")
  expect_error(f0_runs("a", c(0.1, 0.2), TRUE, gap_ratio = "x"), "above 1")
})

test_that("f0_runs() finds no gaps in equal steps of 4-24 ms (21 points per token)", {
  # like dc21f0.csv: 21 equidistant points per token, the step set by each
  # token's length, so no common step across the file
  set.seed(7)
  span <- c(seq(0.08, 0.48, by = 0.02), runif(80, 0.08, 0.48))
  d <- do.call(rbind, lapply(seq_along(span), function(i) {
    st <- runif(1, 0.02, 0.1)
    data.frame(token = paste0("s", i),
               time = st + span[i] * (0:20) / 20)
  }))
  r <- f0_runs(d$token, d$time, TRUE)
  g <- attr(r, "grid")
  expect_true(all(r == 1L))
  expect_true(g$regular)
  expect_false(g$common)
  expect_identical(g$ambiguous, 0L)
  expect_identical(g$n_gaps_time, 0L)

  # an inner point removed in a few tokens is found in each of them
  out <- which(d$token %in% paste0("s", 1:10) &
                 stats::ave(seq_len(nrow(d)), d$token, FUN = seq_along) == 11)
  r2 <- f0_runs(d$token[-out], d$time[-out], TRUE)
  expect_true(attr(r2, "grid")$regular)
  expect_identical(last_run(d$token[-out], r2),
                   c(rep(2L, 10), rep(1L, length(span) - 10)))
})

test_that("f0_runs() reads no false gaps under +/-10% jitter", {
  for (seed in 1:20) {
    set.seed(seed)
    d <- do.call(rbind, lapply(1:60, function(i) {
      n <- sample(20:60, 1)
      fr <- seq_len(n)
      if (i %% 3 == 0) fr <- fr[-sample(3:(n - 2), sample(1:3, 1))]
      data.frame(token = paste0("j", i), frame = fr,
                 time = 0.02 + 0.01 * (fr - 1) + runif(length(fr), -0.001, 0.001))
    }))
    truth <- stats::ave(c(0, diff(d$frame) > 1) * (d$frame != 1), d$token,
                        FUN = cumsum) + 1
    r <- f0_runs(d$token, d$time, TRUE)
    expect_true(attr(r, "grid")$regular)
    expect_identical(as.integer(r), as.integer(truth))
  }
})

test_that("pulse-timed points and times rounded to 2 decimals are not a regular grid", {
  # one point per glottal pulse: the step follows 1/f0, so it changes along
  # the token
  set.seed(1)
  pulse <- do.call(rbind, lapply(1:60, function(i) {
    a <- runif(1, 100, 320); b <- runif(1, 100, 320); dur <- runif(1, 0.2, 0.4)
    f <- function(t) a + (b - a) * t / dur + 40 * sin(pi * t / dur) * (i %% 2)
    t <- 0; tt <- numeric(0)
    while (t <= dur) { tt <- c(tt, t); t <- t + 1 / f(t) }
    data.frame(token = paste0("p", i), time = 0.05 + tt)
  }))
  r <- f0_runs(pulse$token, pulse$time, TRUE)
  g <- attr(r, "grid")
  expect_false(g$regular)
  expect_gt(g$ambiguous, 0.001 * g$n_steps)
  expect_gt(g$n_gaps_time, 0L)         # these would be false gaps
  expect_true(all(r == 1L))            # none is marked

  # 21 points over 80-480 ms, rounded to 2 decimals: steps of 0 appear
  set.seed(2)
  d21 <- do.call(rbind, lapply(1:100, function(i) {
    data.frame(token = paste0("s", i),
               time = round(runif(1, 0.02, 0.1) + runif(1, 0.08, 0.48) * (0:20) / 20, 2))
  }))
  r <- f0_runs(d21$token, d21$time, TRUE)
  g <- attr(r, "grid")
  expect_false(g$regular)
  expect_gt(g$nonincreasing, 0L)
  expect_equal(g$resolution, 0.01)
  expect_true(all(r == 1L))

  # a 12.5 ms grid rounded to 2 decimals: steps of 10 and 20 ms, none
  # ambiguous and none of 0. The times are written to the usual step, so the
  # steps are read in whole units of 0.01, and a quarter of them are 2 units:
  # far more than the 5% that may be.
  d125 <- do.call(rbind, lapply(1:50, function(i) {
    data.frame(token = i, time = round(0.003 * i + 0.0125 * (0:24), 2))
  }))
  r <- f0_runs(d125$token, d125$time, TRUE)
  g <- attr(r, "grid")
  expect_false(g$regular)
  expect_true(g$units)
  expect_identical(g$ambiguous, 0L)
  expect_identical(g$nonincreasing, 0L)
  expect_equal(g$off_unit / g$n_steps, 0.25, tolerance = 0.02)
  expect_gt(g$n_gaps_time, 0L)
  expect_true(all(r == 1L))

  # rows without f0 still give their gaps on an irregular grid
  tt <- round(0.0125 * (0:24), 2)
  f0 <- c(rep(200, 10), NA, NA, rep(200, 13))
  r <- f0_runs(rep(c("a", "b"), each = 25), c(tt, tt), has_f0(c(f0, f0)))
  expect_false(attr(r, "grid")$regular)
  expect_identical(as.integer(r[1:25]),
                   c(rep(1L, 10), NA, NA, rep(2L, 13)))
})

test_that("f0_runs() uses the common step for tokens of 2-5 rows", {
  # 1,500 long tokens on a 10 ms grid set the common step (at least 90% of
  # tokens must share it, and the short tokens below mostly do not)
  long <- data.frame(token = rep(sprintf("L%04d", 1:1500), each = 30),
                     frame = rep(1:30, 1500),
                     time = rep(as_written(0.025 + 0.01 * (0:29)), 1500))
  # every way to keep 2-5 frames out of the first 2-9, first and last kept
  pats <- list()
  for (n in 2:5) for (last in n:9) {
    mid <- seq.int(2, length.out = last - 2)
    inner <- if (n == 2) list(integer(0)) else
      lapply(seq_len(choose(length(mid), n - 2)), function(j)
        mid[utils::combn(length(mid), n - 2)[, j]])
    for (ii in inner) pats[[length(pats) + 1]] <- c(1L, ii, last)
  }
  short <- do.call(rbind, lapply(seq_along(pats), function(j) {
    data.frame(token = sprintf("S%03d", j), frame = pats[[j]],
               time = as_written(0.025 + 0.01 * (pats[[j]] - 1)))
  }))
  d <- rbind(long, short)
  truth <- stats::ave(c(0, diff(d$frame) > 1) * (d$frame != 1), d$token,
                      FUN = cumsum) + 1
  r <- f0_runs(d$token, d$time, TRUE)
  g <- attr(r, "grid")
  expect_true(g$regular)
  expect_true(g$common)
  expect_identical(as.integer(r), as.integer(truth))
  # 162 short tokens with 294 gaps, every one found and none extra
  is_short <- d$token %in% short$token
  expect_identical(length(pats), 162L)
  expect_identical(sum(diff(truth[is_short]) > 0), 294L)

  # a token with 5 or more steps uses its own median: 20 ms is not a gap
  slow <- data.frame(token = "slow", frame = 1:8,
                     time = as_written(0.025 + 0.02 * (0:7)))
  d2 <- rbind(long, slow)
  r2 <- f0_runs(d2$token, d2$time, TRUE)
  expect_true(all(r2 == 1L))

  # without a common step, a token of 2 rows cannot show a gap
  set.seed(3)
  mixed <- do.call(rbind, lapply(1:40, function(i) {
    data.frame(token = paste0("m", i), frame = 1:21,
               time = runif(1, 0.02, 0.1) + runif(1, 0.08, 0.48) * (0:20) / 20)
  }))
  two <- data.frame(token = "two", frame = c(1, 4), time = c(0.03, 0.06))
  d3 <- rbind(mixed, two)
  r3 <- f0_runs(d3$token, d3$time, TRUE)
  expect_false(attr(r3, "grid")$common)
  expect_true(all(r3 == 1L))
})

test_that("f0_runs() on f0_long.csv: 109 gaps in 105 tokens from the time steps", {
  path <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(path), "test/f0_long.csv not found")
  fl <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  keep <- has_f0(fl$f0)
  expect_identical(nrow(fl), 83301L)
  expect_identical(sum(!keep), 30870L)

  # rows without f0 dropped beforehand: gaps from the time steps only
  d <- fl[keep, ]
  r <- f0_runs(d$token, d$time, TRUE)
  g <- attr(r, "grid")
  expect_true(g$regular)
  expect_true(g$common)
  expect_equal(g$step, 0.01, tolerance = 1e-9)
  expect_identical(g$ambiguous, 0L)
  expect_identical(g$nonincreasing, 0L)
  mx <- last_run(d$token, r)
  expect_identical(length(mx), 1853L)
  expect_identical(sum(mx - 1L), 109L)
  expect_identical(sum(mx > 1L), 105L)
  expect_identical(max(r), 3L)

  # the file as uploaded gives the same runs, from its rows without f0
  r0 <- f0_runs(fl$token, fl$time, keep)
  expect_identical(attr(r0, "grid")$n_gaps_rows, 109L)
  expect_identical(as.integer(r0[keep]), as.integer(r))
  expect_true(all(is.na(r0[!keep])))
})

test_that("f0_runs() on dc21f0.csv: a regular grid with no gaps", {
  path <- test_path("..", "..", "test", "dc21f0.csv")
  skip_if_not(file.exists(path), "test/dc21f0.csv not found")
  dc <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  tok <- paste(dc$speaker, dc$char, dc$position, dc$start_time)
  expect_identical(length(unique(tok)), 1848L)
  keep <- has_f0(dc$f0_Hz)
  expect_true(all(keep))

  r <- f0_runs(tok, dc$time, keep)
  g <- attr(r, "grid")
  expect_true(g$regular)
  expect_identical(g$ambiguous, 0L)
  expect_identical(g$nonincreasing, 0L)
  expect_identical(g$n_gaps_time, 0L)
  expect_true(all(r == 1L))
})

# ---------- screen_f0_rows() and drop_rows_without_f0() ----------------------

# One token on a 10 ms grid, its times written to 4 decimals. `f0` gives the
# values row by row, NA for rows without f0.
gap_tok <- function(name, f0, start = 0.0125) {
  data.frame(token = name, time = as_written(start + 0.01 * (seq_along(f0) - 1)),
             f0 = f0, stringsAsFactors = FALSE)
}

# Ten clean tokens of 12 rows, so the file has a common 10 ms step.
clean_toks <- function() {
  do.call(rbind, lapply(sprintf("q%02d", 1:10), function(nm)
    gap_tok(nm, 150 + 0:11)))
}

# Tokens padded with rows without f0 at both edges, with gaps inside, a
# token with no f0, and a short token.
padded_fixture <- function() {
  f <- function(n) 200 + seq_len(n)
  rbind(
    gap_tok("p1", c(NA, NA, NA, f(5), NA, NA, f(7), NA, NA, NA)),
    gap_tok("p2", c(NA, NA, f(2), NA, NA, f(12), NA, NA)),
    gap_tok("p3", rep(NA_real_, 10)),
    gap_tok("p4", f(15)),
    gap_tok("p5", c(NA, f(2), NA, 210, NA)),
    clean_toks())
}

test_that("screen_f0_rows() counts rows without f0 on a padded fixture", {
  fx  <- padded_fixture()
  scr <- screen_f0_rows(fx)
  expect_s3_class(scr, "data.frame")
  expect_identical(names(scr),
                   c("token", "n_rows", "n_f0", "n_start", "n_end", "n_inside",
                     "n_gaps", "n_gaps_time", "frag_start", "frag_end"))
  expect_identical(scr$token, unique(fx$token))
  p <- scr[1:5, ]
  expect_identical(p$n_rows,      c(20L, 20L, 10L, 15L, 6L))
  expect_identical(p$n_f0,        c(12L, 14L, 0L, 15L, 3L))
  expect_identical(p$n_start,     c(3L, 2L, 0L, 0L, 1L))
  expect_identical(p$n_end,       c(3L, 2L, 0L, 0L, 1L))
  expect_identical(p$n_inside,    c(2L, 2L, 0L, 0L, 1L))
  expect_identical(p$n_gaps,      c(1L, 1L, 0L, 0L, 1L))
  expect_identical(p$n_gaps_time, c(1L, 1L, 0L, 0L, 1L))
  expect_identical(p$frag_start,  c(0L, 2L, 0L, 0L, 0L))
  expect_identical(p$frag_end,    c(0L, 0L, 0L, 0L, 0L))
  expect_true(all(scr$n_rows[6:15] == 12L & scr$n_f0[6:15] == 12L))

  s <- attr(scr, "summary")
  expect_type(s, "integer")
  want <- c(rows = 191L, tokens = 15L, rows_f0 = 164L, rows_without_f0 = 27L,
            unvoiced = 0L, start = 6L, start_tokens = 3L, end = 6L,
            end_tokens = 3L, inside = 5L, gaps = 3L, gap_tokens = 3L,
            gaps_time = 3L, gaps_time_tokens = 3L, gaps_time_only = 0L,
            gaps_run = 0L, gaps_run_tokens = 0L, no_f0_rows = 10L,
            no_f0_tokens = 1L, no_time = 0L, rows_after = 164L, tokens_after = 14L, run_tokens = 3L,
            max_run = 2L, few_tokens = 0L, short_runs = 1L,
            short_runs_start = 1L, short_runs_end = 0L, short_rows = 2L,
            short_tokens = 1L, short_tokens_start = 1L, short_tokens_end = 0L,
            short_tokens_both = 0L, rows_after_short = 162L)
  expect_identical(s[names(want)], want)
  # every row without f0 is in exactly one place
  expect_identical(s[["start"]] + s[["end"]] + s[["inside"]] +
                     s[["no_f0_rows"]] + s[["no_time"]],
                   s[["rows_without_f0"]])

  g <- attr(scr, "grid")
  expect_true(g$regular)
  expect_true(g$common)
  expect_equal(g$step, 0.01, tolerance = 1e-9)
  expect_length(g$why, 0L)
  expect_identical(attr(scr, "no_f0"), "p3")
  # p5 keeps 3 samples: enough (only 1 or 2 count as few)
  expect_identical(attr(scr, "few_samples"), stats::setNames(integer(0), character(0)))
  expect_false(attr(scr, "equal_rows"))
  expect_false(attr(scr, "stretch")$warn)
  expect_false(attr(scr, "stretch")$warn_short)
  expect_identical(attr(scr, "stretch")$n_tokens_short, 1L)
  expect_identical(attr(scr, "settings")$f0, "f0")
  expect_identical(attr(scr, "settings")$run, "voiced_run")
  # nothing that may mark no f0
  expect_identical(attr(scr, "unvoiced_suggested"),
                   data.frame(value = numeric(0), rows = integer(0)))
})

test_that("drop_rows_without_f0() removes the rows and numbers the runs", {
  fx  <- padded_fixture()
  out <- drop_rows_without_f0(fx)
  expect_identical(nrow(out), 164L)
  expect_false(anyNA(out$f0))
  expect_false("p3" %in% out$token)
  expect_identical(names(out), c(names(fx), "voiced_run"))
  expect_type(out$voiced_run, "integer")
  expect_null(attributes(out$voiced_run))
  vr <- split(out$voiced_run, factor(out$token, levels = unique(out$token)))
  expect_identical(vr$p1, rep(1:2, c(5, 7)))
  expect_identical(vr$p2, rep(1:2, c(2, 12)))
  expect_identical(vr$p4, rep(1L, 15))
  expect_identical(vr$p5, c(1L, 1L, 2L))
  expect_identical(rownames(out), as.character(seq_len(nrow(out))))

  a <- attr(out, "rows_without_f0")
  expect_identical(a$f0, "f0")
  expect_null(a$unvoiced)
  expect_identical(a[c("rows", "removed", "start", "end", "inside", "no_f0",
                       "no_time", "short_runs", "gaps_rows",
                       "gaps_time_only", "gaps_run")],
                   list(rows = 191L, removed = 27L, start = 6L, end = 6L,
                        inside = 5L, no_f0 = 10L, no_time = 0L,
                        short_runs = 0L, gaps_rows = 3L, gaps_time_only = 0L,
                        gaps_run = 0L))
  expect_identical(a$tokens_lost, "p3")
  expect_true(a$grid$regular)
})

test_that("voiced_run is the same from removed rows and from the time steps", {
  fx    <- padded_fixture()
  out   <- drop_rows_without_f0(fx)
  clean <- fx[!is.na(fx$f0), ]
  out2  <- expect_silent(drop_rows_without_f0(clean))
  expect_identical(out2$voiced_run, out$voiced_run)
  a2 <- attr(out2, "rows_without_f0")
  expect_identical(a2$removed, 0L)
  expect_identical(a2$gaps_rows, 0L)
  expect_identical(a2$gaps_time_only, 3L)
  expect_identical(attr(out, "rows_without_f0")$gaps_rows, 3L)
  # the screen of the cleaned data sees the gaps in the time steps only
  s2 <- attr(screen_f0_rows(clean), "summary")
  expect_identical(s2[c("rows_without_f0", "gaps", "gaps_time",
                        "gaps_time_only", "run_tokens")],
                   c(rows_without_f0 = 0L, gaps = 0L, gaps_time = 3L,
                     gaps_time_only = 3L, run_tokens = 3L))
  # running it again gives the same runs, from the time steps
  expect_message(again <- drop_rows_without_f0(out), "Replaced")
  expect_identical(again$voiced_run, out$voiced_run)
})

# 21 points over each token, as after resampling: every token has 21 rows.
points21 <- function(n_tok = 30, seed = 11) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n_tok), function(i) {
    data.frame(token = sprintf("e%02d", i),
               time = runif(1, 0.02, 0.1) + runif(1, 0.08, 0.48) * (0:20) / 20,
               f0 = 180 + 20 * sin((0:20) / 5 + i))
  }))
}

test_that("21-point data with every row voiced: nothing to do", {
  d <- points21()
  out <- expect_silent(drop_rows_without_f0(d))
  expect_false("voiced_run" %in% names(out))
  expect_identical(structure(out, rows_without_f0 = NULL), d)
  a <- attr(out, "rows_without_f0")
  expect_identical(a$removed, 0L)
  expect_identical(a$gaps_rows + a$gaps_time_only + a$gaps_run, 0L)

  scr <- screen_f0_rows(d)
  s <- attr(scr, "summary")
  expect_identical(s[["rows_without_f0"]], 0L)
  expect_identical(s[["gaps_time"]], 0L)
  expect_true(attr(scr, "grid")$regular)
  expect_true(attr(scr, "equal_rows"))
  expect_false(attr(scr, "stretch")$warn)
  expect_identical(attr(scr, "stretch")$n_rows, 21L)

  # a voiced_run column already there is left alone, without a message
  d$voiced_run <- 1L
  out <- expect_silent(drop_rows_without_f0(d))
  expect_identical(structure(out, rows_without_f0 = NULL), d)
})

test_that("equal row counts, and the stretch when a token loses an edge row", {
  d <- points21()
  # a row without f0 inside one token: no edge row is lost
  d_in <- d
  d_in$f0[d_in$token == "e03"][11] <- NA
  st <- attr(screen_f0_rows(d_in), "stretch")
  expect_true(attr(screen_f0_rows(d_in), "equal_rows"))
  expect_false(st$warn)
  expect_identical(st$n_tokens, 0L)
  out <- drop_rows_without_f0(d_in)
  expect_identical(out$voiced_run[out$token == "e03"], rep(1:2, c(10, 10)))

  # the first row of one token and the last of another have no f0
  d_edge <- d
  d_edge$f0[d_edge$token == "e03"][1] <- NA
  d_edge$f0[d_edge$token == "e07"][21] <- NA
  scr <- screen_f0_rows(d_edge)
  st <- attr(scr, "stretch")
  expect_true(attr(scr, "equal_rows"))
  expect_true(st$warn)
  expect_identical(st$n_rows, 21L)
  expect_identical(st$n_tokens, 2L)
  expect_equal(st$factor, 20 / 19)
  expect_false(st$warn_short)
  expect_identical(st$n_tokens_short, 0L)

  # the second row has no f0: removal keeps every edge row, but min_run = 3
  # removes the first sample, so the token is stretched only then
  d_frag <- d
  d_frag$f0[d_frag$token == "e05"][2] <- NA
  scr <- screen_f0_rows(d_frag)
  st <- attr(scr, "stretch")
  expect_false(st$warn)
  expect_true(st$warn_short)
  expect_identical(st$n_tokens_short, 1L)
  expect_identical(scr$frag_start[scr$token == "e05"], 1L)
  t5 <- d_frag$time[d_frag$token == "e05"]
  out3 <- drop_rows_without_f0(d_frag, min_run = 3)
  expect_identical(range(out3$time[out3$token == "e05"]), c(t5[3], t5[21]))

  # tokens with different numbers of rows, or a single token
  d_uneq <- d_edge[-1, ]
  expect_false(attr(screen_f0_rows(d_uneq), "equal_rows"))
  expect_false(attr(screen_f0_rows(d_uneq), "stretch")$warn)
  expect_true(is.na(attr(screen_f0_rows(d_uneq), "stretch")$factor))
  one <- d_edge[d_edge$token == "e03", ]
  expect_false(attr(screen_f0_rows(one), "equal_rows"))
})

test_that("min_run = 3 removes short runs cut off at a token's start or end", {
  f <- function(n) 200 + seq_len(n)
  d <- rbind(
    gap_tok("r1", c(NA, f(2), NA, f(6), NA, NA, 220, NA, NA)),  # 2 at start, 1 at end
    gap_tok("r2", c(f(5), NA, f(2), NA, f(1), NA)),            # two short runs at end
    gap_tok("r3", c(NA, f(1), NA, f(2), NA)),                  # no run of 3: keep all
    gap_tok("r4", c(f(2), NA, f(4), NA, 230, NA, f(4), NA, f(2))), # inner run of 1 kept
    clean_toks())
  scr <- screen_f0_rows(d)
  expect_identical(scr$frag_start[1:4], c(2L, 0L, 0L, 2L))
  expect_identical(scr$frag_end[1:4],   c(1L, 3L, 0L, 2L))
  s <- attr(scr, "summary")
  expect_identical(s[c("short_runs", "short_runs_start", "short_runs_end",
                       "short_rows", "short_tokens", "short_tokens_start",
                       "short_tokens_end", "short_tokens_both")],
                   c(short_runs = 6L, short_runs_start = 2L,
                     short_runs_end = 4L, short_rows = 10L, short_tokens = 3L,
                     short_tokens_start = 2L, short_tokens_end = 3L,
                     short_tokens_both = 2L))

  out <- drop_rows_without_f0(d, min_run = 3)
  expect_identical(nrow(out), s[["rows_after_short"]])
  a <- attr(out, "rows_without_f0")
  expect_identical(a$short_runs, 10L)
  vr <- split(out$voiced_run, factor(out$token, levels = unique(out$token)))
  expect_identical(vr$r1, rep(1L, 6))
  expect_identical(vr$r2, rep(1L, 5))
  expect_identical(vr$r3, c(1L, 2L, 2L))               # all samples kept
  expect_identical(vr$r4, rep(1:3, c(4, 1, 4)))
  expect_identical(out$f0[out$token == "r4"], c(f(4), 230, f(4)))
  # only the gaps between the runs kept are counted
  expect_identical(a$gaps_rows, 3L)

  # min_run = 1 keeps them all
  out1 <- drop_rows_without_f0(d)
  expect_identical(nrow(out1), s[["rows_after"]])
  expect_identical(attr(out1, "rows_without_f0")$short_runs, 0L)
  expect_identical(max(out1$voiced_run[out1$token == "r2"]), 3L)
})

test_that("unvoiced values are read as no f0, and only those", {
  d <- rbind(gap_tok("u1", c(-1, 200, 201, -1, 203, 0, 205, -1)),
             gap_tok("u2", c(0, 0, 190, 191, 192, -1, -1)),
             clean_toks())
  # by default 0 and -1 are values like any other
  out0 <- drop_rows_without_f0(d)
  expect_identical(nrow(out0), nrow(d))
  expect_false("voiced_run" %in% names(out0))
  expect_identical(attr(screen_f0_rows(d), "summary")[["rows_without_f0"]], 0L)

  # unvoiced = -1
  d_na <- d
  d_na$f0[d_na$f0 == -1] <- NA
  out1 <- drop_rows_without_f0(d, unvoiced = -1)
  ref1 <- drop_rows_without_f0(d_na)
  expect_identical(structure(out1, rows_without_f0 = NULL),
                   structure(ref1, rows_without_f0 = NULL))
  expect_true(0 %in% out1$f0)
  expect_identical(attr(out1, "rows_without_f0")$unvoiced, -1)
  s1 <- attr(screen_f0_rows(d, unvoiced = -1), "summary")
  expect_identical(s1[["unvoiced"]], 5L)
  expect_identical(s1[["rows_without_f0"]], 5L)
  expect_identical(attr(screen_f0_rows(d, unvoiced = -1), "settings")$unvoiced, -1)

  # unvoiced = c(0, -1)
  d_na$f0[d_na$f0 %in% 0] <- NA
  out2 <- drop_rows_without_f0(d, unvoiced = c(0, -1))
  ref2 <- drop_rows_without_f0(d_na)
  expect_identical(structure(out2, rows_without_f0 = NULL),
                   structure(ref2, rows_without_f0 = NULL))
  expect_false(any(out2$f0 <= 0))
  expect_identical(out2$voiced_run[out2$token == "u1"], c(1L, 1L, 2L, 3L))
  s2 <- attr(screen_f0_rows(d, unvoiced = c(0, -1)), "summary")
  expect_identical(s2[c("unvoiced", "start", "end", "inside")],
                   c(unvoiced = 8L, start = 3L, end = 3L, inside = 2L))
})

test_that("the screen reports the values that may mark no f0", {
  d <- rbind(gap_tok("u1", c(-1, 200, 201, -1, 203, 0, 205, -1)),
             gap_tok("u2", c(0, 0, 190, 191, 192, -1, -1)),
             clean_toks())
  want <- data.frame(value = c(0, -1), rows = c(3L, 5L))
  expect_identical(suggest_unvoiced(d$f0), c(0, -1))
  expect_identical(attr(screen_f0_rows(d), "unvoiced_suggested"), want)
  # read from the column before `unvoiced`, so it stays once taken up
  scr <- screen_f0_rows(d, unvoiced = c(0, -1))
  expect_identical(attr(scr, "unvoiced_suggested"), want)
  expect_identical(attr(scr, "summary")[["unvoiced"]], sum(want$rows))
  # a signed column: nothing to suggest
  st <- d
  st$f0 <- round((d$f0 - 170) / 10, 1)
  expect_null(suggest_unvoiced(st$f0))
  expect_identical(nrow(attr(screen_f0_rows(st), "unvoiced_suggested")), 0L)
  # exported, for the Start page
  ns <- test_path("..", "..", "NAMESPACE")
  skip_if_not(file.exists(ns), "NAMESPACE not found")
  expect_true("export(suggest_unvoiced)" %in% readLines(ns))
})

test_that("a signed column keeps its 0s and negative values", {
  st <- c(NA, -2.5, 0, 1.2, -0.3, NA, 3, 0, -1, -1, NA)
  d <- rbind(gap_tok("s1", st), gap_tok("s2", rev(st)))
  d <- rbind(d, transform(clean_toks(), f0 = f0 - 160))
  out <- drop_rows_without_f0(d)
  expect_identical(nrow(out), sum(!is.na(d$f0)))
  expect_identical(out$f0[out$token == "s1"], st[!is.na(st)])
  expect_identical(out$voiced_run[out$token == "s1"], rep(1:2, c(4, 4)))
  scr <- screen_f0_rows(d)
  expect_identical(scr$n_f0[1:2], c(8L, 8L))
  expect_identical(scr$n_inside[1:2], c(1L, 1L))
})

test_that("the row order is kept, and the screen does not depend on it", {
  fx <- padded_fixture()
  fx$id <- seq_len(nrow(fx))
  out <- drop_rows_without_f0(fx)
  set.seed(5)
  p <- sample(nrow(fx))
  sh <- fx[p, ]
  outp <- drop_rows_without_f0(sh)
  expect_identical(outp$id, sh$id[sh$id %in% out$id])
  expect_identical(outp$voiced_run, out$voiced_run[match(outp$id, out$id)])
  same <- setdiff(names(attr(out, "rows_without_f0")), "tokens_lost")
  expect_identical(attr(outp, "rows_without_f0")[same],
                   attr(out, "rows_without_f0")[same])
  expect_setequal(attr(outp, "rows_without_f0")$tokens_lost, "p3")

  scr  <- screen_f0_rows(fx)
  scrp <- screen_f0_rows(sh)
  expect_identical(scrp$token, unique(sh$token))
  m <- match(scr$token, scrp$token)
  expect_identical(scrp[m, -1], structure(scr[, -1], row.names = m))
  expect_identical(attr(scrp, "summary"), attr(scr, "summary"))
  expect_identical(attr(scrp, "grid"), attr(scr, "grid"))
})

test_that("an existing voiced_run is replaced with a message", {
  fx <- padded_fixture()
  fx$voiced_run <- 99L
  expect_message(out <- drop_rows_without_f0(fx), "Replaced the existing")
  ref <- drop_rows_without_f0(padded_fixture())
  expect_identical(out$voiced_run, ref$voiced_run)
  expect_identical(names(out), names(fx))
  # another name, or none
  out2 <- drop_rows_without_f0(padded_fixture(), run = "seg_run")
  expect_identical(out2$seg_run, ref$voiced_run)
  out3 <- drop_rows_without_f0(padded_fixture(), run = NULL)
  expect_identical(names(out3), names(padded_fixture()))
  expect_identical(nrow(out3), nrow(ref))
})

# Tokens on a 12.5 ms grid whose times are written to 2 decimals: steps of
# 0.01 and 0.02 in turn, so the grid is not regular and the time steps show
# no gaps.
irr_tok <- function(name, f0) {
  data.frame(token = name, time = round(0.0125 * seq_along(f0), 2), f0 = f0,
             stringsAsFactors = FALSE)
}
irr_fixture <- function() {
  rbind(irr_tok("a", c(NA, 120, 121, 122, NA, NA, 125, 126, 127, 128, 129, NA)),
        irr_tok("b", c(NA, 130:139, NA)),
        irr_tok("c", c(NA, 140, 141, NA, 143:150, NA)))
}

test_that("a second removal keeps the gaps an earlier one marked, on an irregular grid", {
  d <- irr_fixture()
  step1 <- drop_rows_without_f0(d)
  expect_false(attr(step1, "rows_without_f0")$grid$regular)
  vr1 <- split(step1$voiced_run, step1$token)
  expect_identical(vr1$a, rep(1:2, c(3, 5)))
  expect_identical(vr1$c, rep(1:2, c(2, 8)))

  # later a sample of b loses its f0 (as in F0 Correction), and the rows
  # without f0 are removed again
  again <- step1
  again$f0[again$token == "b"][5] <- NA
  expect_message(step2 <- drop_rows_without_f0(again), "Replaced the existing")
  vr2 <- split(step2$voiced_run, step2$token)
  expect_identical(vr2$a, vr1$a)
  expect_identical(vr2$c, vr1$c)
  expect_identical(vr2$b, rep(1:2, c(4, 5)))
  a2 <- attr(step2, "rows_without_f0")
  expect_identical(a2[c("removed", "gaps_rows", "gaps_time_only", "gaps_run")],
                   list(removed = 1L, gaps_rows = 1L, gaps_time_only = 0L,
                        gaps_run = 2L))
  # the same as removing all of those rows from the original data at once
  d2 <- d
  d2$f0[d2$token == "b"][6] <- NA
  ref <- drop_rows_without_f0(d2)
  expect_identical(step2[c("token", "time", "f0", "voiced_run")],
                   ref[c("token", "time", "f0", "voiced_run")])

  # the screen sees those gaps too, and agrees with the removal
  s <- attr(screen_f0_rows(again), "summary")
  expect_identical(s[c("rows_without_f0", "gaps", "gaps_time_only", "gaps_run",
                       "gaps_run_tokens", "run_tokens", "max_run")],
                   c(rows_without_f0 = 1L, gaps = 1L, gaps_time_only = 0L,
                     gaps_run = 2L, gaps_run_tokens = 2L, run_tokens = 3L,
                     max_run = 2L))
  s0 <- attr(screen_f0_rows(again, run = NULL), "summary")
  expect_identical(s0[c("gaps_run", "run_tokens")],
                   c(gaps_run = 0L, run_tokens = 1L))

  # contour_duration() and trim_to_voiced() give the same on input and output
  dur <- function(x) {
    y <- contour_duration(x, min_run = 3)
    tapply(y$token_f0_dur, y$token, `[`, 1)
  }
  expect_identical(dur(step2), dur(again))
  tr_in  <- trim_to_voiced(again, min_run = 3)
  tr_out <- trim_to_voiced(step2, min_run = 3)
  expect_identical(tr_out$time, tr_in$time[!is.na(tr_in$f0)])

  # run = NULL reads no column and adds none: the old one is kept as it is
  out0 <- expect_silent(drop_rows_without_f0(again, run = NULL))
  expect_identical(out0$voiced_run, again$voiced_run[!is.na(again$f0)])
})

test_that("an existing voiced_run keeps gaps the time steps cannot show on a regular grid", {
  # every other frame has no f0, so once those rows are removed the token's
  # usual step is two frames and the time steps show no gap there
  x <- rbind(clean_toks(),
             gap_tok("cr", c(NA, 110, 108, NA, 104, NA, 98, NA, 92, NA, 88, 86,
                             NA, 84, 83, NA)))
  ex <- drop_rows_without_f0(x)
  expect_identical(ex$voiced_run[ex$token == "cr"],
                   c(1L, 1L, 2L, 3L, 4L, 5L, 5L, 6L, 6L))
  scr <- screen_f0_rows(ex)
  expect_true(attr(scr, "grid")$regular)
  s <- attr(scr, "summary")
  expect_identical(s[c("rows_without_f0", "gaps", "gaps_time_only", "gaps_run",
                       "gaps_run_tokens", "run_tokens", "max_run")],
                   c(rows_without_f0 = 0L, gaps = 0L, gaps_time_only = 0L,
                     gaps_run = 5L, gaps_run_tokens = 1L, run_tokens = 1L,
                     max_run = 6L))
  expect_identical(scr$n_gaps_time[scr$token == "cr"], 0L)
  expect_message(again <- drop_rows_without_f0(ex), "Replaced the existing")
  expect_identical(again$voiced_run, ex$voiced_run)
  expect_identical(attr(again, "rows_without_f0")$gaps_run, 5L)
})

test_that("the screen and removal split runs as voiced_run_ids() does", {
  d <- irr_fixture()
  step1 <- drop_rows_without_f0(d)
  step1$f0[step1$token == "b"][5] <- NA
  # a sample with no voiced_run (a row added later) is a value of its own
  step1$voiced_run[step1$token == "c"][6] <- NA
  shuffled <- step1[c(seq(2, nrow(step1), 2), seq(1, nrow(step1), 2)), ]
  for (x in list(step1, shuffled)) {
    core <- f0_rows_core(x, "token", "time", "f0", 1.5, NULL, 1,
                         run = "voiced_run")
    ids <- voiced_run_ids(x$token, x$time, has_f0(x$f0), run = x$voiced_run)
    expect_identical(core$runs, ids)
  }
  core <- f0_rows_core(step1, "token", "time", "f0", 1.5, NULL, 1,
                       run = "voiced_run")
  expect_identical(core$runs[step1$token == "c"],
                   c(1L, 1L, 2L, 2L, 2L, 3L, 4L, 4L, 4L, 4L))
  expect_error(screen_f0_rows(step1, run = "f0"), "must not be")
  expect_error(screen_f0_rows(step1, run = c("a", "b")), "single column")
})

test_that("has_gap, n_missing and other columns are kept", {
  fx <- padded_fixture()
  fx$n_missing <- stats::ave(is.na(fx$f0), fx$token, FUN = sum)
  fx$has_gap <- fx$token %in% c("p1", "p2", "p5")
  fx$intensity <- seq_len(nrow(fx)) / 10
  out <- drop_rows_without_f0(fx)
  keep <- !is.na(fx$f0)
  expect_identical(out$n_missing, fx$n_missing[keep])
  expect_identical(out$has_gap, fx$has_gap[keep])
  expect_identical(out$intensity, fx$intensity[keep])
  # flag_f0_gaps() keeps the file's own values on the output
  fl <- flag_f0_gaps(out)
  expect_identical(fl$n_missing, out$n_missing)
  expect_identical(fl$has_gap, out$has_gap)
})

test_that("a tibble stays a tibble", {
  tb <- dplyr::as_tibble(padded_fixture())
  out <- drop_rows_without_f0(tb)
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 164L)
  expect_identical(out$voiced_run,
                   drop_rows_without_f0(padded_fixture())$voiced_run)
  scr <- screen_f0_rows(tb)
  expect_identical(attr(scr, "summary"),
                   attr(screen_f0_rows(padded_fixture()), "summary"))
  # nothing to do: returned as it is
  tb21 <- dplyr::as_tibble(points21())
  out21 <- drop_rows_without_f0(tb21)
  expect_s3_class(out21, "tbl_df")
  expect_identical(structure(out21, rows_without_f0 = NULL), tb21)
})

test_that("rows with no token or no time are handled", {
  fx <- padded_fixture()
  # rows whose token is NA are a token of their own
  na_tok <- gap_tok(NA_character_, c(NA, 200, 201, 202, NA))
  # a row with f0 and one without, both with no time, in token p4 and p1
  d <- rbind(fx, na_tok)
  d$time[d$token %in% "p4"][8] <- NA     # a sample with f0
  d$time[d$token %in% "p1"][10] <- NA    # a row without f0, inside a gap
  scr <- expect_silent(screen_f0_rows(d))
  expect_identical(nrow(scr), 16L)
  expect_true(is.na(scr$token[16]))
  expect_identical(unlist(scr[16, -1]),
                   c(n_rows = 5L, n_f0 = 3L, n_start = 1L, n_end = 1L,
                     n_inside = 0L, n_gaps = 0L, n_gaps_time = 0L,
                     frag_start = 0L, frag_end = 0L))
  s <- attr(scr, "summary")
  expect_identical(s[["no_time"]], 1L)
  expect_identical(s[["inside"]], 4L)
  expect_identical(scr$n_inside[1], 1L)
  expect_identical(scr$n_f0[4], 15L)

  out <- expect_silent(drop_rows_without_f0(d))
  # the sample with f0 and no time is kept, with no run; the row without f0
  # and no time is removed
  expect_identical(nrow(out), s[["rows_after"]])
  p4 <- out[out$token %in% "p4", ]
  expect_identical(nrow(p4), 15L)
  # without that sample's time, the step across it is 20 ms: a gap
  expect_identical(p4$voiced_run, c(rep(1L, 7), NA, rep(2L, 7)))
  expect_identical(scr$n_gaps_time[4], 1L)
  expect_identical(out$voiced_run[is.na(out$token)], c(1L, 1L, 1L))
  a <- attr(out, "rows_without_f0")
  expect_identical(a$no_time, 1L)
  expect_identical(a$removed, s[["rows_without_f0"]])
})

test_that("screen_f0_rows() agrees with drop_rows_without_f0()", {
  check <- function(d, ...) {
    scr <- screen_f0_rows(d, ...)
    s <- attr(scr, "summary")
    for (mr in c(1, 3)) {
      out <- suppressMessages(drop_rows_without_f0(d, min_run = mr, ...))
      a <- attr(out, "rows_without_f0")
      expect_identical(nrow(out),
                       s[[if (mr == 1) "rows_after" else "rows_after_short"]])
      expect_identical(a$removed, nrow(d) - nrow(out))
      expect_identical(a$tokens_lost, attr(scr, "no_f0"))
      expect_identical(a$grid, attr(scr, "grid"))
      expect_identical(a[c("start", "end", "inside")],
                       as.list(s[c("start", "end", "inside")]))
      expect_identical(a$short_runs,
                       if (mr == 1) 0L else s[["short_rows"]])
      if (mr == 1) {
        vr <- if ("voiced_run" %in% names(out)) out$voiced_run else
          rep(1L, nrow(out))
        mx <- tapply(vr, factor(out$token, exclude = NULL), max, na.rm = TRUE)
        expect_identical(sum(mx > 1L), s[["run_tokens"]])
        expect_identical(a$gaps_rows, s[["gaps"]])
        expect_identical(a$gaps_time_only, s[["gaps_time_only"]])
        expect_identical(a$gaps_run, s[["gaps_run"]])
        left <- table(factor(out$token, exclude = NULL))
        expect_identical(sum(left < 3L), s[["few_tokens"]])
      }
    }
  }
  check(padded_fixture())
  fx <- padded_fixture()
  check(fx[!is.na(fx$f0), ])
  check(padded_fixture(), gap_ratio = Inf)
  check(rbind(gap_tok("u1", c(-1, 200, 201, -1, 203, 0, 205, -1)),
              clean_toks()), unvoiced = c(0, -1))
  check(points21())
})

test_that("contour_duration() gives the same durations after removal", {
  fx <- padded_fixture()
  out <- drop_rows_without_f0(fx)
  per_tok <- function(d) {
    x <- contour_duration(d)
    tapply(x$token_f0_dur, x$token, `[`, 1)
  }
  raw <- per_tok(fx)
  new <- per_tok(out)
  expect_identical(new, raw[names(new)])
  expect_true(is.na(raw[["p3"]]))
  # with min_run = 1 the lone sample at p2's start counts in both
  x1 <- contour_duration(fx, min_run = 1)
  y1 <- contour_duration(out, min_run = 1)
  expect_identical(y1$token_f0_dur, x1$token_f0_dur[!is.na(fx$f0)])
  # trim_to_voiced() too
  expect_identical(trim_to_voiced(out)$time,
                   trim_to_voiced(fx)$time[!is.na(trim_to_voiced(fx)$f0)])
})

test_that("drop_rows_without_f0() and screen_f0_rows() check their arguments", {
  fx <- padded_fixture()
  expect_error(screen_f0_rows(fx, f0 = "pitch"), "not found.*pitch")
  expect_error(drop_rows_without_f0(fx, time = "t"), "not found")
  expect_error(screen_f0_rows(fx, gap_ratio = 1), "above 1")
  expect_error(drop_rows_without_f0(fx, min_run = 0), "at least 1")
  expect_error(drop_rows_without_f0(fx, unvoiced = "-1"), "numbers")
  expect_error(drop_rows_without_f0(fx, run = "f0"), "must not be")
  expect_error(drop_rows_without_f0(fx, run = c("a", "b")), "single column")
  expect_error(screen_f0_rows(as.list(fx)), "data frame")
  expect_error(screen_f0_rows(fx, token = c("token", "time")), "single column")
  # no rows at all
  e <- fx[0, ]
  scr <- screen_f0_rows(e)
  expect_identical(nrow(scr), 0L)
  expect_identical(attr(scr, "summary")[["rows"]], 0L)
  expect_named(attr(scr, "grid")$why, "no_steps")
  out <- drop_rows_without_f0(e)
  expect_identical(structure(out, rows_without_f0 = NULL), e)
})

test_that("the grid reports why gaps cannot be read from the time steps", {
  # times rounded to 2 decimals on a 12.5 ms grid
  d <- do.call(rbind, lapply(1:30, function(i)
    data.frame(token = i, time = round(0.003 * i + 0.0125 * (0:24), 2),
               f0 = 200)))
  d$f0[d$token == 3][12:13] <- NA
  scr <- screen_f0_rows(d)
  g <- attr(scr, "grid")
  expect_false(g$regular)
  expect_false(g$units_ok)
  expect_true("units" %in% names(g$why))
  expect_match(g$why[["units"]], "written to the nearest 0.01, the same as the usual step",
               fixed = TRUE)
  # the why names no single cause: frames off 0.01, or many gaps
  expect_match(g$why[["units"]], paste(
    "So either the frames are not exactly 0.01 apart, or more than 1 step in",
    "100 is a gap. From times written this way, the two cannot be told apart."),
    fixed = TRUE)
  expect_no_match(g$why[["units"]], "probably", fixed = TRUE)
  expect_identical(sum(scr$n_gaps_time), 0L)
  expect_identical(attr(scr, "summary")[["gaps"]], 1L)
  out <- drop_rows_without_f0(d)
  expect_identical(max(out$voiced_run), 2L)     # the gap from rows
  # with the rows removed beforehand, the gap cannot be read: no column
  out2 <- drop_rows_without_f0(d[!is.na(d$f0), ])
  expect_false("voiced_run" %in% names(out2))
  expect_identical(attr(out2, "rows_without_f0")$gaps_time_only, 0L)

  # a repeated time sets only its own token aside; with one in every token,
  # no gaps are read from the time steps
  d2 <- rbind(clean_toks(), gap_tok("dup", c(200, 201, 202, 203, 204)))
  d2$time[d2$token == "dup"][3] <- d2$time[d2$token == "dup"][2]
  g2 <- attr(screen_f0_rows(d2), "grid")
  expect_true(g2$regular)
  expect_identical(g2$nonincreasing, 1L)
  expect_length(g2$why, 0L)
  d2b <- d2[d2$token == "dup", ]
  g2b <- attr(screen_f0_rows(d2b), "grid")
  expect_false(g2b$regular)
  expect_named(g2b$why, "nonincreasing")
  expect_match(g2b$why[["nonincreasing"]],
               "In every token, two samples with f0 have the same time", fixed = TRUE)
  # 2 samples at one time: no step to judge, but the reason is the repeat
  g2c <- attr(screen_f0_rows(data.frame(token = "a", time = 0.1, f0 = c(200, 201))),
              "grid")
  expect_identical(g2c$n_steps, 0L)
  expect_named(g2c$why, "nonincreasing")

  # a 25 ms grid written to 2 decimals: written more coarsely than a fifth
  # of the step, and not at the step itself
  d4 <- do.call(rbind, lapply(1:30, function(i)
    data.frame(token = i, time = round(0.02 + 0.025 * (0:20), 2), f0 = 200)))
  g4 <- attr(screen_f0_rows(d4), "grid")
  expect_false(g4$regular)
  expect_false(g4$units)
  expect_true("resolution" %in% names(g4$why))
  expect_match(g4$why[["resolution"]], "nearest 0.01. That is more than a fifth",
               fixed = TRUE)

  # pulse-like steps: many between 1.25 and 1.75 times the usual step
  set.seed(9)
  d3 <- do.call(rbind, lapply(1:20, function(i)
    data.frame(token = i, time = cumsum(runif(30, 0.004, 0.009)), f0 = 150)))
  g3 <- attr(screen_f0_rows(d3), "grid")
  expect_false(g3$regular)
  expect_true("ambiguous" %in% names(g3$why))
  # one token with a repeated time does not add "nonincreasing": the other
  # tokens can still be read, so it is not why the grid fails
  d3$time[d3$token == 1][5] <- d3$time[d3$token == 1][4]
  g3b <- attr(screen_f0_rows(d3), "grid")
  expect_identical(g3b$repeated, 1L)
  expect_named(g3b$why, "ambiguous")
})

test_that("a share just under 99% is not printed as 99.0%", {
  # 100 tokens of 101 frames on a 10 ms grid written to 2 decimals, with 101
  # inner frames missing: 101 of 9,899 steps are 0.02, so 98.98% are 0.01
  d <- do.call(rbind, lapply(1:100, function(i) {
    fr <- seq_len(101)
    if (i == 1) fr <- fr[-c(10, 20)] else fr <- fr[-(10 + i %% 50)]
    data.frame(token = sprintf("t%03d", i),
               time = as.numeric(sprintf("%.2f", 0.02 + 0.01 * (fr - 1))),
               f0 = 200)
  }))
  g <- attr(screen_f0_rows(d), "grid")
  expect_identical(c(g$off_unit, g$n_steps), c(101L, 9899L))
  expect_false(g$regular)
  expect_identical(sprintf("%.1f%%", 100 * (1 - 101 / 9899)), "99.0%")
  expect_match(g$why[["units"]],
               "But only 98.9% of the steps are exactly 0.01, and at least 99% must be.",
               fixed = TRUE)
  # with 2 of the frames back, 99 of 9,901 steps are 0.02: just within the
  # limit, so the grid is read
  d2 <- rbind(d, data.frame(token = c("t001", "t002"), time = c(0.21, 0.13),
                            f0 = 200))
  g2 <- attr(screen_f0_rows(d2), "grid")
  expect_identical(c(g2$off_unit, g2$n_steps), c(99L, 9901L))
  expect_true(g2$regular)
  expect_true(g2$units_ok)
  expect_length(g2$why, 0L)
  # exactly 1 in 100 (one token a frame shorter at its end): still read
  d3 <- d2[!(d2$token == "t050" & abs(d2$time - 1.02) < 1e-9), ]
  g3 <- attr(screen_f0_rows(d3), "grid")
  expect_identical(c(g3$off_unit, g3$n_steps), c(99L, 9900L))
  expect_true(g3$regular)
})

# Tokens of 15 to 45 frames on a grid of `step_ms`, each from a random
# offset, with the times written to `decimals` decimals as a file stores
# them. A share `drop` of the inner frames has no f0 (NA), and `truth` is the
# run each frame should get.
grid_file <- function(step_ms, decimals = 2, drop = 0.01, n_tok = 120,
                      seed = 1) {
  set.seed(seed)
  lens <- sample(15:45, n_tok, replace = TRUE)
  d <- do.call(rbind, lapply(seq_len(n_tok), function(i) {
    fr <- seq_len(lens[i])
    tt <- stats::runif(1, 0.01, 0.06) + step_ms / 1000 * (fr - 1)
    data.frame(token = sprintf("t%03d", i), frame = fr,
               time = as.numeric(sprintf(paste0("%.", decimals, "f"), tt)))
  }))
  inner <- which(d$frame > 1 & d$frame < lens[match(d$token, unique(d$token))])
  d$f0 <- 200
  d$f0[sample(inner, round(drop * length(inner)))] <- NA
  kept <- !is.na(d$f0)
  d$truth <- NA_integer_
  fr <- d$frame[kept]
  d$truth[kept] <- as.integer(stats::ave(
    c(0, diff(fr) > 1) * (fr != 1), d$token[kept], FUN = cumsum) + 1)
  d
}

test_that("a repeated time sets aside only its own token, which is listed", {
  # g1 and g2 have gaps of 1 and 3 frames; dup has a gap of 2 frames and a
  # repeated time (its 3rd sample at the time of its 2nd)
  raw <- rbind(gap_tok("g1", c(200:204, NA, 206:215)),
               gap_tok("g2", c(200:209, NA, NA, NA, 213:220)),
               clean_toks(),
               gap_tok("dup", c(200:205, NA, NA, 208:215)))
  raw$time[raw$token == "dup"][3] <- raw$time[raw$token == "dup"][2]
  keep <- !is.na(raw$f0)
  kept <- raw[keep, ]

  r_raw  <- f0_runs(raw$token, raw$time, keep)
  r_kept <- f0_runs(kept$token, kept$time, TRUE)
  g <- attr(r_kept, "grid")
  expect_true(g$regular)
  expect_false(g$units)
  expect_identical(g$nonincreasing, 1L)
  expect_identical(g$repeated, 1L)
  expect_identical(g$repeated_tokens, "dup")
  expect_identical(g$n_tokens_read, 12L)
  # the other tokens keep their gaps from the time steps
  by_tok <- function(r, tok) split(as.integer(r), factor(tok, levels = unique(tok)))
  rk <- by_tok(r_kept, kept$token)
  expect_identical(rk$g1, rep(1:2, c(5, 10)))
  expect_identical(rk$g2, rep(1:2, c(10, 8)))
  # dup: no gap from its time steps, but its rows without f0 still split it
  expect_identical(rk$dup, rep(1L, 14))
  rr <- by_tok(r_raw, raw$token)
  expect_identical(rr$dup, c(rep(1L, 6), NA, NA, rep(2L, 8)))
  expect_identical(rr$g1[!is.na(rr$g1)], rk$g1)
  expect_identical(rr$g2[!is.na(rr$g2)], rk$g2)

  # the screen lists the token, and removal keeps its gap from rows
  scr <- screen_f0_rows(kept)
  expect_identical(attr(scr, "repeated_time"), "dup")
  expect_identical(attr(scr, "summary")[["repeated_tokens"]], 1L)
  expect_length(attr(scr, "grid")$why, 0L)
  expect_identical(scr$n_gaps_time[scr$token %in% c("g1", "g2", "dup")],
                   c(1L, 1L, 0L))
  out <- drop_rows_without_f0(raw)
  vr <- split(out$voiced_run, out$token)
  expect_identical(vr$dup, rep(1:2, c(6, 8)))
  expect_identical(vr$g1, rk$g1)
  expect_identical(attr(screen_f0_rows(raw), "repeated_time"), "dup")

  # the same with the times written to 2 decimals on the same 10 ms grid,
  # with 40 more clean tokens so that at most 1 step in 100 is 0 or 0.02
  more <- do.call(rbind, lapply(sprintf("c%02d", 1:40), function(nm)
    gap_tok(nm, 150 + 0:11)))
  raw2 <- rbind(raw, more)
  raw2$time <- round(raw2$time - 0.0025, 2)
  kept2 <- raw2[c(keep, rep(TRUE, nrow(more))), ]
  r2 <- f0_runs(kept2$token, kept2$time, TRUE)
  g2 <- attr(r2, "grid")
  expect_true(g2$regular)
  expect_true(g2$units)
  expect_identical(g2$repeated_tokens, "dup")
  expect_identical(as.integer(r2)[seq_len(nrow(kept))], as.integer(r_kept))

  # a row without f0 at the time of a sample is not a repeat
  tie <- gap_tok("tie", c(200:204, NA, 206:210))
  tie$time[6] <- tie$time[5]
  expect_identical(attr(f0_runs(c(raw$token, tie$token), c(raw$time, tie$time),
                                has_f0(c(raw$f0, tie$f0))), "grid")$repeated_tokens,
                   "dup")

  # floating-point noise: 0.3 and 0.1 * 3 (0.30000000000000004) are one time
  tt <- c(0.1, 0.2, 0.3, 0.1 * 3, 0.4, 0.5, 0.6)
  expect_false(identical(tt[3], tt[4]))
  gn <- attr(f0_runs("n", tt, TRUE), "grid")
  expect_identical(gn$repeated, 1L)
  expect_identical(gn$nonincreasing, 1L)
  # the same with times written to 4 decimals, read as they are: 0.0337 and
  # 0.0537 - 0.02 differ only by floating-point noise
  tf <- 0.0237 + 0.01 * (0:9)
  tf <- c(tf[1:2], 0.0537 - 0.02, tf[3:10])
  expect_false(identical(tf[2], tf[3]))
  gf <- attr(f0_runs("n", tf, TRUE), "grid")
  expect_identical(gf$resolution, 1e-4)
  expect_false(gf$units)
  expect_identical(gf$repeated, 1L)
  expect_identical(gf$nonincreasing, 1L)
})

test_that("a 10 ms grid written to 2 decimals: one-frame gaps are found, no false ones", {
  for (q in c(0, 0.002, 0.005)) {
    d <- grid_file(10, 2, q, n_tok = 200, seed = 11)
    keep <- !is.na(d$f0)
    kept <- d[keep, ]
    r <- f0_runs(kept$token, kept$time, TRUE)
    g <- attr(r, "grid")
    expect_true(g$regular)
    expect_true(g$units)
    expect_true(g$units_ok)
    expect_identical(g$resolution, 0.01)
    expect_identical(g$step, 0.01)
    expect_identical(g$repeated, 0L)
    expect_identical(g$other_step, 0L)
    expect_identical(as.integer(r), kept$truth)
    # the same runs with the rows without f0 kept
    expect_identical(as.integer(f0_runs(d$token, d$time, keep)[keep]),
                     kept$truth)
  }
  # at 0.5%, the 2-unit steps are the gaps: none is rounding
  expect_identical(g$off_unit, as.integer(sum(diff(kept$truth) == 1L)))
  expect_lt(g$off_unit / g$n_steps, 0.01)
  # one-frame gaps (steps of exactly 0.02) are among them
  st <- diff(kept$time)[diff(kept$truth) == 1L]
  expect_gt(sum(abs(st - 0.02) < 1e-9), 20L)

  # screen and removal: the same voiced_run with the rows kept or removed
  scr <- screen_f0_rows(kept)
  expect_identical(attr(scr, "summary")[["gaps_time"]],
                   as.integer(sum(diff(kept$truth) == 1L)))
  a <- drop_rows_without_f0(d)
  b <- drop_rows_without_f0(kept)
  expect_identical(b$voiced_run, a$voiced_run)
  expect_identical(b$voiced_run, kept$truth)

  # at 1%, still read. A token with 3 gaps in 19 steps is set aside (more
  # than 1 step in 8 is 0.02): its gaps are missed, but every other gap is
  # found, and no gap is false
  d1 <- grid_file(10, 2, 0.01, n_tok = 200, seed = 11)
  k1 <- d1[!is.na(d1$f0), ]
  r1 <- f0_runs(k1$token, k1$time, TRUE)
  g1 <- attr(r1, "grid")
  expect_true(g1$regular)
  expect_identical(g1$other_step, 1L)
  aside <- k1$token %in% g1$other_step_tokens
  expect_identical(as.integer(r1)[!aside], k1$truth[!aside])
  expect_true(all(r1[aside] == 1L))
  expect_identical(sum(diff(k1$truth[aside]) == 1L), 3L)
  expect_identical(sum(aside) - 1L, 19L)

  # more than 1 step in 100 not exactly one unit: not read, and the why
  # says so. 2% lies between the limit and the 5% an earlier version used.
  d2 <- grid_file(10, 2, 0.02, n_tok = 200, seed = 11)
  k2 <- d2[!is.na(d2$f0), ]
  scr2 <- screen_f0_rows(k2)
  g2 <- attr(scr2, "grid")
  expect_false(g2$regular)
  expect_true(g2$units)
  expect_false(g2$units_ok)
  expect_gt(g2$off_unit / g2$n_steps, 0.01)
  expect_lt(g2$off_unit / g2$n_steps, 0.05)
  expect_named(g2$why, "units")
  expect_identical(sum(scr2$n_gaps_time), 0L)
  expect_false("voiced_run" %in% names(drop_rows_without_f0(k2)))
  # with the rows without f0 kept, their gaps are still read
  expect_identical(drop_rows_without_f0(d2)$voiced_run, k2$truth)
})

test_that("grids that are not whole hundredths, written to 2 decimals, are not read", {
  # Praat's automatic steps for pitch floors of 100, 60 and 50 Hz (7.5, 12.5
  # and 15 ms), and others: rounding to 2 decimals gives steps of 0 or 0.02
  # in a regular pattern. None may be read as a gap.
  # Also Praat's steps for floors of 72 to 74 Hz (10.135 to 10.417 ms), just
  # over 10 ms: a share of 1.4% to 4.2% of the steps are 0.02.
  for (st in c(5, 6.25, 7.5, 8, 10.135, 10.274, 10.417, 12.5, 15)) {
    for (q in c(0, 0.002, 0.01, 0.03)) {
      for (n in c(120, 400)) {
        d <- grid_file(st, 2, q, n_tok = n)
        kept <- d[!is.na(d$f0), ]
        r <- f0_runs(kept$token, kept$time, TRUE)
        lab <- paste(st, "ms,", q, ",", n, "tokens")
        expect_false(attr(r, "grid")$regular, label = lab)
        expect_true(all(r == 1L), label = lab)
        # a grid that is not read always says why
        expect_gt(length(attr(screen_f0_rows(kept), "grid")$why), 0L, label = lab)
      }
    }
  }
  why <- function(st) attr(screen_f0_rows(grid_file(st, 2, 0.01)), "grid")$why
  # 12.5 ms: steps of 0.01, 0.01, 0.01, 0.02 in every token, so every token
  # is set aside, and only 74.5% of the steps are 0.01
  w125 <- why(12.5)
  expect_named(w125, c("set_aside", "units"))
  expect_identical(w125[["set_aside"]], paste(
    "No token can be read from its time steps: in every token, more than 1",
    "step in 8 is exactly 0.02, so the frames are probably not 0.01 apart."))
  expect_identical(w125[["units"]], paste(
    "The times are written to the nearest 0.01, the same as the usual step,",
    "so a step of 0.02 or more could be a gap. But only 74.5% of the steps",
    "are exactly 0.01, and at least 99% must be. So either the frames are",
    "not exactly 0.01 apart, or more than 1 step in 100 is a gap. From times",
    "written this way, the two cannot be told apart."))
  # 7.5 and 5 ms: steps of 0 (repeated times) in every token. The usual step
  # leaves out the steps of 0, so it is one unit, and the why names the
  # units check
  for (st in c(7.5, 5)) {
    w <- why(st)
    expect_named(w, c("nonincreasing", "units"))
    expect_match(w[["units"]], "So either the frames are not exactly 0.01 apart",
                 fixed = TRUE)
  }
  expect_match(why(5)[["units"]], "But only 50.3% of the steps", fixed = TRUE)
  # 15 ms: steps of 0.01 and 0.02 in turn give a usual step of 0.015
  w15 <- why(15)
  expect_named(w15, c("ambiguous", "resolution"))
  expect_match(w15[["resolution"]], "nearest 0.01. That is more than a fifth of the usual step (0.015)",
               fixed = TRUE)

  # written to 3 or 4 decimals, the same grids are read as they are: every
  # gap, and no other
  for (st in c(5, 6.25, 7.5, 8, 10, 12.5, 15)) {
    for (dd in 3:4) {
      d <- grid_file(st, dd, 0.03)
      kept <- d[!is.na(d$f0), ]
      r <- f0_runs(kept$token, kept$time, TRUE)
      expect_true(attr(r, "grid")$regular, label = paste(st, "ms,", dd))
      expect_false(attr(r, "grid")$units, label = paste(st, "ms,", dd))
      expect_identical(as.integer(r), kept$truth, label = paste(st, "ms,", dd))
    }
  }
})

test_that("tokens at 12.5 ms among 10 ms ones, written to 2 decimals, are set aside", {
  # A pitch floor set per speaker with Praat's automatic step: most tokens
  # at 10 ms, a few at 12.5 ms (steps 0.01, 0.01, 0.01, 0.02). No frame is
  # missing in the 12.5 ms tokens, so any gap read there is false.
  mk <- function(tok, frames, step = 0.01, off = 0.02) {
    data.frame(token = tok, f0 = 200 + frames,
               time = as.numeric(sprintf("%.2f", off + step * (frames - 1))),
               stringsAsFactors = FALSE)
  }
  set.seed(5)
  mixed <- function(n_slow) {
    rbind(do.call(rbind, lapply(seq_len(100 - n_slow), function(i)
            mk(sprintf("m%03d", i), if (i <= 4) setdiff(1:40, 15 + i) else 1:40,
               off = stats::runif(1, 0, 0.3)))),
          do.call(rbind, lapply(seq_len(n_slow), function(i)
            mk(sprintf("f%03d", i), 1:40, step = 0.0125,
               off = stats::runif(1, 0, 0.3)))))
  }
  # 2 tokens of 100: the grid is read, the 12.5 ms tokens are set aside, and
  # the 4 real gaps are found
  d <- mixed(2)
  scr <- screen_f0_rows(d)
  g <- attr(scr, "grid")
  expect_true(g$regular)
  expect_true(g$units_ok)
  expect_identical(g$other_step, 2L)
  expect_identical(g$other_step_tokens, c("f001", "f002"))
  expect_identical(g$n_tokens_read, 98L)
  expect_identical(attr(scr, "other_step"), c("f001", "f002"))
  expect_identical(attr(scr, "summary")[["other_step_tokens"]], 2L)
  expect_identical(attr(scr, "repeated_time"), character(0))
  expect_identical(scr$n_gaps_time[grepl("^f", scr$token)], c(0L, 0L))
  expect_identical(scr$n_gaps_time[scr$token %in% sprintf("m%03d", 1:4)], rep(1L, 4))
  expect_identical(sum(scr$n_gaps_time), 4L)
  out <- drop_rows_without_f0(d)
  expect_identical(as.integer(tapply(out$voiced_run, out$token, max)[c("m001", "m005", "f001")]),
                   c(2L, 1L, 1L))
  # 10 tokens of 100 (2.5% of the steps are 0.02): not read at all, as
  # before, and no false gap
  d10 <- mixed(10)
  g10 <- attr(screen_f0_rows(d10), "grid")
  expect_false(g10$regular)
  expect_named(g10$why, "units")
  expect_identical(max(as.integer(f0_runs(d10$token, d10$time, TRUE))), 1L)
  # a 10 ms token with 2 gaps in 29 steps is not set aside, nor is a token
  # with every other frame missing: its usual step is 2 units, so its steps
  # of 0.02 are not gaps, and its step of 0.04 is one
  # Nor are a short token with 1 gap in 4 steps (one 0.02 step is not
  # enough), or a token whose 2 gaps of 2 frames give steps of 0.03.
  half <- mk("half", c(seq(1, 19, by = 2), 23, 25))
  d3 <- rbind(mixed(0), mk("two", setdiff(1:32, c(10, 20))), half,
              mk("short", c(1:3, 5:6)), mk("long2", setdiff(1:16, c(5, 6, 11, 12))))
  scr3 <- screen_f0_rows(d3)
  g3 <- attr(scr3, "grid")
  expect_true(g3$regular)
  expect_identical(g3$other_step, 0L)
  r3 <- as.integer(f0_runs(d3$token, d3$time, TRUE))
  expect_identical(max(r3[d3$token == "two"]), 3L)
  expect_identical(r3[d3$token == "half"], rep(1:2, c(10, 2)))
  expect_identical(r3[d3$token == "short"], c(1L, 1L, 1L, 2L, 2L))
  expect_identical(max(r3[d3$token == "long2"]), 3L)
  expect_identical(attr(scr3, "summary")[c("gaps_time", "gaps_time_tokens",
                                          "gaps_time_only", "gaps_time_only_tokens")],
                   c(gaps_time = 10L, gaps_time_tokens = 8L,
                     gaps_time_only = 10L, gaps_time_only_tokens = 8L))
  # written to 4 decimals, each token's own step is read: no token is set
  # aside and nothing is false
  d4 <- mixed(10)
  d4$time <- NULL
  d4 <- do.call(rbind, lapply(split(d4, factor(d4$token, levels = unique(d4$token))), function(x) {
    st <- if (startsWith(x$token[1], "f")) 0.0125 else 0.01
    x$time <- as.numeric(sprintf("%.4f", 0.0237 + st * (x$f0 - 201)))
    x
  }))
  g4 <- attr(screen_f0_rows(d4), "grid")
  expect_true(g4$regular)
  expect_false(g4$units)
  expect_identical(g4$other_step, 0L)
  expect_identical(attr(screen_f0_rows(d4), "summary")[["gaps_time"]], 4L)
})

test_that("floating-point noise in the times changes nothing", {
  # times summed in R carry noise (0.1 * 3 is 0.30000000000000004); written
  # to a file and read back they do not
  d <- grid_file(10, 2, 0.005, n_tok = 150, seed = 4)
  kept <- d[!is.na(d$f0), ]
  expect_gt(sum(diff(kept$truth) == 1L), 10L)
  for (dec in c(2, 4)) {
    off <- if (dec == 2) 0.02 else 0.0237
    noisy <- off + stats::ave(0.01 * (kept$frame - 1), kept$token,
                              FUN = function(x) cumsum(c(x[1], diff(x))))
    noisy <- noisy + 0.01 * (match(kept$token, unique(kept$token)) %% 3)
    clean <- as.numeric(sprintf(paste0("%.", dec, "f"), noisy))
    expect_false(identical(noisy, clean))
    expect_equal(noisy, clean, tolerance = 1e-12)
    rn <- f0_runs(kept$token, noisy, TRUE)
    rc <- f0_runs(kept$token, clean, TRUE)
    expect_identical(as.integer(rn), as.integer(rc))
    expect_identical(as.integer(rn), kept$truth)
    gn <- attr(rn, "grid")
    gc <- attr(rc, "grid")
    keys <- c("regular", "units", "units_ok", "common", "ambiguous",
              "nonincreasing", "resolution", "off_unit", "n_steps", "repeated",
              "other_step", "n_gaps_time")
    expect_identical(gn[keys], gc[keys])
    expect_equal(gn$step, gc$step, tolerance = 1e-12)
    expect_identical(gn$units, dec == 2)
  }
})

test_that("suggest_unvoiced() offers 0 and one negative marker in an Hz-like column", {
  expect_identical(suggest_unvoiced(c(0, 210, 220, -1, -1, 0, 230)), c(0, -1))
  expect_identical(suggest_unvoiced(c(0, 210, 220, NA, 0, 230)), 0)
  expect_identical(suggest_unvoiced(c(-1, 210, 220, -1, 230)), -1)
  expect_identical(suggest_unvoiced(c(-200, 110, 120, -200, 0)), c(0, -200))
  # text read by value
  expect_identical(suggest_unvoiced(c("0", "210", "220", "--undefined--")), 0)
  # nothing to read
  expect_null(suggest_unvoiced(c(210, 220, NA, 230)))
  expect_null(suggest_unvoiced(c(NA, NA)))
  expect_null(suggest_unvoiced(numeric(0)))
  # signed or normalised columns
  expect_null(suggest_unvoiced(c(-2.5, 0, 1.2, -0.3, 3, 0, -1)))       # semitones
  expect_null(suggest_unvoiced(c(-1.2, 0, 0.8, 1.5, -0.4, 0)))         # z-scores
  expect_null(suggest_unvoiced(c(-1, -2, 200, 210, 0)))                # two markers
  expect_null(suggest_unvoiced(c(-1, 200, 210, 0)))                    # not repeated
  expect_null(suggest_unvoiced(c(0, 9000, 9100, 9200)))                # cents
  expect_null(suggest_unvoiced(c(0, 20, 25, 28)))                      # median < 30
})

test_that("screen and removal on f0_long.csv: the acceptance counts", {
  path <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(path), "test/f0_long.csv not found")
  fl <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  fl <- zero_hz_to_na(fl)
  expect_length(attr(fl, "zero_hz"), 0L)

  scr <- screen_f0_rows(fl)
  s <- attr(scr, "summary")
  expect_identical(s[c("rows", "tokens", "no_f0_tokens", "rows_without_f0",
                       "start", "start_tokens", "end", "end_tokens",
                       "inside", "gaps", "gap_tokens", "rows_after",
                       "few_tokens", "run_tokens", "max_run")],
                   c(rows = 83301L, tokens = 1853L, no_f0_tokens = 0L,
                     rows_without_f0 = 30870L, start = 14023L,
                     start_tokens = 1853L, end = 16584L, end_tokens = 1853L,
                     inside = 263L, gaps = 109L, gap_tokens = 105L,
                     rows_after = 52431L, few_tokens = 0L, run_tokens = 105L,
                     max_run = 3L))
  expect_identical(range(scr$n_start), c(3L, 20L))
  expect_equal(stats::median(scr$n_start), 7)
  expect_identical(range(scr$n_end), c(2L, 29L))
  expect_equal(stats::median(scr$n_end), 9)
  expect_identical(as.vector(table(scr$n_gaps[scr$n_gaps > 0])), c(101L, 4L))
  # The fewest samples a token keeps is 3, in dc114\u{6015}1s107.2025: enough
  expect_identical(attr(scr, "few_samples"), stats::setNames(integer(0), character(0)))
  expect_identical(min(scr$n_f0), 3L)
  expect_identical(scr$n_f0[scr$token == "dc114\u{6015}1s107.2025"], 3L)
  expect_identical(attr(scr, "no_f0"), character(0))
  # short edge runs: 50 tokens, 13 at the start and 38 at the end, 1 with
  # both. One token ends with two of them, so there are 52 runs.
  expect_identical(s[c("short_tokens", "short_tokens_start",
                       "short_tokens_end", "short_tokens_both",
                       "short_runs", "short_runs_start", "short_runs_end")],
                   c(short_tokens = 50L, short_tokens_start = 13L,
                     short_tokens_end = 38L, short_tokens_both = 1L,
                     short_runs = 52L, short_runs_start = 13L,
                     short_runs_end = 39L))
  expect_identical(scr$frag_end[scr$token == "dc103\u{9898}4s34.0725"], 4L)
  g <- attr(scr, "grid")
  expect_true(g$regular)
  expect_false(attr(scr, "equal_rows"))

  out <- drop_rows_without_f0(fl)
  expect_identical(nrow(out), 52431L)
  expect_identical(length(unique(out$token)), 1853L)
  mx <- tapply(out$voiced_run, out$token, max)
  expect_identical(sum(mx > 1L), 105L)
  expect_identical(sum(mx - 1L), 109L)
  a <- attr(out, "rows_without_f0")
  expect_identical(a$gaps_rows, 109L)
  expect_identical(a$gaps_time_only, 0L)
  # the token with a BOM inside its name is kept as it is
  bom <- "dc102\u{8863}\u{feff}1s1.4325"
  expect_true(bom %in% out$token)

  # rows dropped beforehand: the time steps show the same 109 gaps in 105
  # tokens, and give an identical voiced_run
  clean <- fl[!is.na(fl$f0), ]
  out2 <- drop_rows_without_f0(clean)
  expect_identical(out2$voiced_run, out$voiced_run)
  a2 <- attr(out2, "rows_without_f0")
  expect_identical(c(a2$removed, a2$gaps_rows, a2$gaps_time_only),
                   c(0L, 0L, 109L))
  s2 <- attr(screen_f0_rows(clean), "summary")
  expect_identical(s2[c("gaps_time", "gaps_time_tokens")],
                   c(gaps_time = 109L, gaps_time_tokens = 105L))

  # min_run = 3
  out3 <- drop_rows_without_f0(fl, min_run = 3)
  expect_identical(nrow(out3), s[["rows_after_short"]])
  expect_identical(attr(out3, "rows_without_f0")$short_runs, s[["short_rows"]])

  # durations are the same before and after removal
  dur <- function(d) {
    x <- contour_duration(d)
    tapply(x$token_f0_dur, x$token, `[`, 1)
  }
  expect_identical(dur(out), dur(fl))

  # a 12.5 ms grid written to 2 decimals (not a regular grid): a second
  # removal, after 20 tokens with no gap lose their last sample, keeps the
  # 109 gaps the first one marked
  ir <- fl[order(match(fl$token, unique(fl$token)), fl$time), ]
  k <- stats::ave(seq_len(nrow(ir)), ir$token, FUN = seq_along)
  ir$time <- round(0.02 + 0.0125 * (k - 1), 2)
  first <- drop_rows_without_f0(ir)
  expect_false(attr(first, "rows_without_f0")$grid$regular)
  mx1 <- tapply(first$voiced_run, first$token, max)
  expect_identical(sum(mx1 > 1L), 105L)
  tk <- names(mx1)[mx1 == 1L][seq(1, 400, by = 20)]
  last <- !duplicated(first$token, fromLast = TRUE) & first$token %in% tk
  first$f0[last] <- NA
  second <- suppressMessages(drop_rows_without_f0(first))
  mx2 <- tapply(second$voiced_run, second$token, max)
  expect_identical(sum(mx2 > 1L), 105L)
  expect_identical(attr(second, "rows_without_f0")$gaps_run, 109L)
  expect_identical(nrow(trim_to_voiced(second, min_run = 3)),
                   sum(!is.na(trim_to_voiced(first, min_run = 3)$f0)))
})

test_that("f0_long.csv with its times written to 2 decimals", {
  path <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(path), "test/f0_long.csv not found")
  fl <- zero_hz_to_na(utils::read.csv(path, encoding = "UTF-8",
                                      stringsAsFactors = FALSE))
  ref <- drop_rows_without_f0(fl)            # times written to 4 decimals
  # gap starts in time order within each token
  breaks <- function(d) {
    o <- order(match(d$token, unique(d$token)), d$time)
    tk <- d$token[o]
    vr <- d$voiced_run[o]
    same <- tk[-1L] == tk[-length(tk)]
    out <- logical(nrow(d))
    out[o[-1L]] <- same & vr[-1L] != vr[-length(vr)]
    out
  }

  # every row moved to an exact 10 ms grid written to 2 decimals: the same
  # 109 gaps in 105 tokens, whether or not the rows without f0 are kept
  k <- stats::ave(fl$time, fl$token, FUN = rank)
  g2 <- fl
  g2$time <- round(0.02 + 0.01 * (k - 1), 2)
  out <- drop_rows_without_f0(g2)
  gr <- attr(out, "rows_without_f0")$grid
  expect_true(gr$regular)
  expect_true(gr$units)
  expect_identical(out$voiced_run, ref$voiced_run)
  clean <- g2[!is.na(g2$f0), ]
  out2 <- drop_rows_without_f0(clean)
  expect_identical(out2$voiced_run, ref$voiced_run)
  expect_identical(attr(out2, "rows_without_f0")$gaps_time_only, 109L)
  s2 <- attr(screen_f0_rows(clean), "summary")
  expect_identical(s2[c("gaps_time", "gaps_time_tokens", "max_run",
                        "repeated_tokens")],
                   c(gaps_time = 109L, gaps_time_tokens = 105L, max_run = 3L,
                     repeated_tokens = 0L))

  # the file's own times written to 2 decimals. 130 tokens have frames half
  # way between two hundredths, which round unevenly into steps of 0 and
  # 0.02. These are 4.6% of all steps, more than the 1% allowed, so no gaps
  # are read from the time steps: none can be false. With the rows without
  # f0 kept, their gaps are still read.
  t2 <- fl
  t2$time <- as.numeric(sprintf("%.2f", fl$time))
  c2 <- t2[!is.na(t2$f0), ]
  scr <- screen_f0_rows(c2)
  g <- attr(scr, "grid")
  expect_false(g$regular)
  expect_true(g$units)
  expect_false(g$units_ok)
  expect_identical(g$repeated, 130L)
  expect_identical(g$other_step, 0L)
  expect_equal(g$off_unit / g$n_steps, 0.0456, tolerance = 0.01)
  expect_named(g$why, "units")
  expect_identical(attr(scr, "summary")[["gaps_time"]], 0L)
  expect_length(attr(scr, "repeated_time"), 130L)
  out3 <- drop_rows_without_f0(c2)
  expect_false("voiced_run" %in% names(out3))
  out4 <- drop_rows_without_f0(t2)
  b_ref <- breaks(ref)
  b4 <- breaks(out4)
  expect_true(all(b_ref[b4]))
  expect_identical(attr(out4, "rows_without_f0")$gaps_time_only, 0L)
})

test_that("the bundled sample: a regular grid with no gaps", {
  data(sample_f0, package = "shinytone", envir = environment())
  scr <- screen_f0_rows(sample_f0, f0 = "f0_Hz")
  s <- attr(scr, "summary")
  expect_identical(s[c("rows_without_f0", "gaps", "gaps_time", "run_tokens",
                       "repeated_tokens")],
                   c(rows_without_f0 = 0L, gaps = 0L, gaps_time = 0L,
                     run_tokens = 0L, repeated_tokens = 0L))
  expect_true(attr(scr, "grid")$regular)
  expect_false(attr(scr, "grid")$units)
  out <- drop_rows_without_f0(sample_f0, f0 = "f0_Hz")
  expect_false("voiced_run" %in% names(out))
})

test_that("dc21f0.csv: every row has f0, a regular grid and no gaps", {
  path <- test_path("..", "..", "test", "dc21f0.csv")
  skip_if_not(file.exists(path), "test/dc21f0.csv not found")
  dc <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  dc$token <- paste(dc$speaker, dc$char, dc$position, dc$start_time)
  dc <- zero_hz_to_na(dc)
  scr <- screen_f0_rows(dc, f0 = "f0_Hz")
  s <- attr(scr, "summary")
  expect_identical(s[c("rows", "tokens", "rows_without_f0", "gaps",
                       "gaps_time", "short_runs")],
                   c(rows = 38808L, tokens = 1848L, rows_without_f0 = 0L,
                     gaps = 0L, gaps_time = 0L, short_runs = 0L))
  expect_true(attr(scr, "grid")$regular)
  expect_true(attr(scr, "equal_rows"))
  expect_false(attr(scr, "stretch")$warn)
  out <- drop_rows_without_f0(dc, f0 = "f0_Hz")
  expect_false("voiced_run" %in% names(out))
  expect_identical(structure(out, rows_without_f0 = NULL), dc)
})

# ---- Tokens missing an end point ----------------------------------------

# Points at uneven times: token a has gaps of 1, 2 and 3 points inside and
# none at its ends; token b misses its first and last points and has a
# 1-point gap.
edge_fixture <- function() {
  data.frame(
    token = rep(c("a", "b"), c(12, 7)),
    time  = c(0, 0.1, 0.3, 0.4, 0.5, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.5,
              0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
    f0    = c(200, NA, 220, 230, NA, NA, 250, NA, NA, NA, 290, 300,
              NA, 181, NA, 185, 187, 189, NA),
    other = letters[1:19],
    stringsAsFactors = FALSE)
}

test_that("the fill is gone: no fill_f0_gaps(), no max_gap and no fill counts", {
  expect_false("fill_f0_gaps" %in% getNamespaceExports("shinytone"))
  expect_false(exists("fill_f0_gaps", envir = asNamespace("shinytone"), inherits = FALSE))
  expect_false("max_gap" %in% names(formals(screen_f0_rows)))
  scr <- screen_f0_rows(edge_fixture())
  s <- attr(scr, "summary")
  expect_false(any(grepl("^(fill|long_gap)", names(s))))
  expect_null(attr(scr, "fillable"))
  expect_null(attr(scr, "long_gap"))
  expect_false("max_gap" %in% names(attr(scr, "settings")))
  expect_error(screen_f0_rows(edge_fixture(), max_gap = 2), "unused argument")
})

test_that("screen_f0_rows() counts the tokens missing an end point", {
  d <- edge_fixture()
  scr <- screen_f0_rows(d)
  s <- attr(scr, "summary")
  expect_identical(s[c("edge_tokens", "edge_rows")], c(edge_tokens = 1L, edge_rows = 7L))
  expect_identical(attr(scr, "edge_missing"), "b")
  expect_identical(s[c("inside", "gaps", "gap_tokens")], c(inside = 7L, gaps = 4L, gap_tokens = 2L))
  # Values read as no f0 count too
  du <- d
  du$f0[is.na(du$f0)] <- -1
  su <- attr(screen_f0_rows(du, unvoiced = -1), "summary")
  expect_identical(su[c("edge_tokens", "edge_rows", "inside")], s[c("edge_tokens", "edge_rows", "inside")])
})

test_that("on 21 points, the tokens missing an end point are the ones the stretch counts", {
  d <- points21()
  d$f0[d$token == "e03"][1] <- NA          # first point
  d$f0[d$token == "e07"][c(10, 21)] <- NA  # a gap and the last point
  d$f0[d$token == "e09"][c(5, 6)] <- NA    # a 2-point gap
  d$f0[d$token == "e11"][c(5, 6, 7)] <- NA # a 3-point gap
  scr <- screen_f0_rows(d)
  s <- attr(scr, "summary")
  expect_identical(attr(scr, "edge_missing"), c("e03", "e07"))
  expect_identical(s[["edge_tokens"]], attr(scr, "stretch")$n_tokens)
  expect_identical(s[["edge_rows"]], 42L)
  # Removal takes every row without f0, and voiced_run marks each gap inside
  rm <- drop_rows_without_f0(d)
  expect_identical(nrow(rm), nrow(d) - 8L)
  expect_identical(max(rm$voiced_run[rm$token == "e11"]), 2L)
  expect_identical(max(rm$voiced_run[rm$token == "e09"]), 2L)
  expect_identical(max(rm$voiced_run[rm$token == "e07"]), 2L)
  # The flags: every point without f0 inside counts, whatever the gap
  fl <- attr(flag_f0_tokens(d), "flagged")
  expect_identical(fl$token, c("e03", "e07", "e09", "e11"))
  expect_identical(fl$reason, c("no f0 at the first point",
                                "no f0 at the last point; 1 point without f0 inside",
                                "2 points without f0 inside", "3 points without f0 inside"))
})

test_that("screen_f0_rows(): few_samples and few_tokens are the tokens left with 1 or 2 samples", {
  d <- rbind(clean_toks(),
             gap_tok("two", c(NA, 200, NA, 202, NA)),
             gap_tok("one", c(NA, NA, 210, NA)),
             gap_tok("three", c(220, 221, NA, 222)),
             gap_tok("none", c(NA, NA)))
  scr <- screen_f0_rows(d)
  expect_identical(attr(scr, "few_samples"), c(two = 2L, one = 1L))
  expect_identical(attr(scr, "summary")[["few_tokens"]], 2L)
})

# ---- flag_f0_tokens(): the tokens with an issue, in a screen_flag column --

# 7 equidistant points per token: one of each reason, and a token with all
# of reasons 2 to 4
flag_points <- function() {
  f <- list(
    ok    = c(200, 205, 210, 215, 220, 225, 230),
    first = c(NA, 205, 210, 215, 220, 225, 230),
    last  = c(200, 205, 210, 215, 220, 225, NA),
    both  = c(NA, 205, 210, 215, 220, 225, NA),
    gap1  = c(200, 205, NA, 215, 220, 225, 230),
    gap3  = c(200, NA, NA, NA, 220, 225, 230),
    two   = c(NA, 205, NA, NA, 220, NA, NA),
    one   = c(NA, NA, NA, 215, NA, NA, NA),
    none  = rep(NA_real_, 7))
  data.frame(token = rep(names(f), each = 7), time = rep(seq(0, 0.3, length.out = 7), length(f)),
             f0 = unlist(f, use.names = FALSE), other = seq_len(7 * length(f)),
             stringsAsFactors = FALSE)
}

test_that("flag_f0_tokens() gives each reason, in order, joined by a semicolon", {
  d <- flag_points()
  out <- flag_f0_tokens(d)
  why <- tapply(out$screen_flag, factor(out$token, levels = unique(out$token)), unique)
  expect_identical(as.vector(why), c(
    "",
    "no f0 at the first point",
    "no f0 at the last point",
    "no f0 at the first and last points",
    "1 point without f0 inside",
    "3 points without f0 inside",
    "only 2 points with f0",
    "only 1 point with f0",
    "no f0"))
  # The same text on every row of the token
  expect_true(all(tapply(out$screen_flag, out$token, function(x) length(unique(x))) == 1L))
  expect_type(out$screen_flag, "character")
  # Nothing else changes: the rows, their order and the other columns
  expect_identical(out[names(d)], d)
  expect_identical(names(out), c(names(d), "screen_flag"))
  # The attribute lists the flagged tokens in order, with the same text
  fl <- attr(out, "flagged")
  expect_identical(names(fl), c("token", "reason"))
  expect_identical(fl$token, unique(d$token)[-1])
  expect_identical(fl$reason, as.vector(why)[-1])
  # "only N" stands alone, as "no f0" does: two and one also miss their
  # first and last points and points inside, which are not added
  expect_false(any(grepl("only [0-9]+ points? with f0;", fl$reason)))
  # With min_f0 = 2, two has enough points, so its other reasons show
  fl2 <- attr(flag_f0_tokens(d, min_f0 = 2), "flagged")
  expect_identical(fl2$reason[fl2$token == "two"],
                   "no f0 at the first and last points; 2 points without f0 inside")
  expect_identical(fl2$reason[fl2$token == "one"], "only 1 point with f0")
  # The row order does not matter
  set.seed(3)
  o <- sample(nrow(d))
  expect_identical(flag_f0_tokens(d[o, ])$screen_flag, out$screen_flag[o])
})

test_that("flag_f0_tokens(): frames get only no f0 and too few samples", {
  # Tokens of different lengths: rows without f0 at the edges or inside are
  # normal there
  d <- rbind(gap_tok("long", c(NA, NA, 200, 201, NA, 203, 204, NA)),
             gap_tok("two", c(NA, 150, NA, NA, 152, NA)),
             gap_tok("one", c(NA, 170, NA)),
             gap_tok("three", c(180, 181, 182)),
             gap_tok("none", c(NA, NA, NA, NA)))
  fl <- attr(flag_f0_tokens(d), "flagged")
  expect_identical(fl$token, c("two", "one", "none"))
  expect_identical(fl$reason, c("only 2 samples with f0", "only 1 sample with f0", "no f0"))
  # The same tokens as few_samples and no_f0 in the screen
  scr <- screen_f0_rows(d)
  expect_identical(sort(fl$token), sort(c(names(attr(scr, "few_samples")), attr(scr, "no_f0"))))
  # A single token is frames, even with no f0 at its first point
  one_tok <- data.frame(token = "x", time = 1:5, f0 = c(NA, 200, 201, 202, NA))
  expect_identical(unique(flag_f0_tokens(one_tok)$screen_flag), "")
  # min_f0 moves the threshold
  fl4 <- attr(flag_f0_tokens(d, min_f0 = 4), "flagged")
  expect_identical(fl4$reason[fl4$token == "three"], "only 3 samples with f0")
  expect_false("long" %in% fl4$token)
  expect_identical(nrow(attr(flag_f0_tokens(d, min_f0 = 1), "flagged")), 1L)
})

test_that("flag_f0_tokens(): min_run counts only the samples drop_rows_without_f0() keeps", {
  # a: a run of 2 cut off by a gap, then 1, then 2 (no run of 3): kept whole
  # b: 1 sample, a gap, then a run of 2
  d <- rbind(gap_tok("a", c(200, 201, NA, 203, NA, 205, 206)),
             gap_tok("b", c(150, NA, NA, 153, 154, NA)),
             gap_tok("c", c(170, NA, 172, 173, 174, 175)),
             clean_toks())
  base <- attr(flag_f0_tokens(d), "flagged")
  expect_identical(nrow(base), 0L)
  # With min_run = 3 (as the app's short-run box) and min_f0 = 3, the flags
  # never change: a token keeps its run of 3 or more, or all its samples
  expect_identical(flag_f0_tokens(d, min_run = 3)$screen_flag, flag_f0_tokens(d)$screen_flag)
  # With min_run = 2, b keeps only its run of 2, and c its run of 4
  fl2 <- attr(flag_f0_tokens(d, min_run = 2), "flagged")
  expect_identical(fl2$token, "b")
  expect_identical(fl2$reason, "only 2 samples with f0")
  kept <- drop_rows_without_f0(d, min_run = 2)
  expect_identical(sum(kept$token == "b"), 2L)
  # On a grid that is not read from the time steps, only rows split runs
  expect_identical(attr(flag_f0_tokens(d, min_run = 2, gap_ratio = Inf), "flagged")$token, "b")
})

test_that("flag_f0_tokens() replaces the column, and keeps tibbles, NA tokens and empty data", {
  d <- flag_points()
  d$screen_flag <- "old"
  out <- flag_f0_tokens(d)
  expect_identical(names(out), names(d))
  expect_false(any(out$screen_flag == "old"))
  # Another name
  out2 <- flag_f0_tokens(flag_points(), flag = "issue")
  expect_identical(out2$issue, flag_f0_tokens(flag_points())$screen_flag)
  # A tibble stays a tibble
  tb <- dplyr::as_tibble(flag_points())
  out_tb <- flag_f0_tokens(tb)
  expect_s3_class(out_tb, "tbl_df")
  expect_identical(out_tb$screen_flag, flag_f0_tokens(flag_points())$screen_flag)
  # A row whose token is NA belongs to a token of its own
  dn <- flag_points()
  dn$token[dn$token == "none"] <- NA
  fln <- attr(flag_f0_tokens(dn), "flagged")
  expect_true(is.na(fln$token[nrow(fln)]))
  expect_identical(fln$reason[nrow(fln)], "no f0")
  expect_identical(unique(flag_f0_tokens(dn)$screen_flag[is.na(dn$token)]), "no f0")
  # A token column of another name names the attribute's column
  dk <- flag_points()
  names(dk)[1] <- "tk"
  expect_identical(names(attr(flag_f0_tokens(dk, token = "tk"), "flagged")), c("tk", "reason"))
  # No rows: an empty column and no flagged token
  z <- flag_f0_tokens(flag_points()[0, ])
  expect_identical(nrow(z), 0L)
  expect_identical(z$screen_flag, character(0))
  expect_identical(nrow(attr(z, "flagged")), 0L)
  # A row with no time: counted in reason 2 when it has f0, never placed
  dt <- data.frame(token = rep(c("a", "b"), each = 4), time = c(1, 2, NA, 4, 1:4),
                   f0 = c(NA, 200, 201, NA, 100, 101, 102, 103))
  expect_identical(attr(flag_f0_tokens(dt), "flagged")$reason, "only 2 points with f0")
})

test_that("flag_f0_tokens() checks its arguments", {
  d <- flag_points()
  expect_error(flag_f0_tokens(as.list(d)), "must be a data frame")
  expect_error(flag_f0_tokens(d, f0 = "pitch"), "not found in data: pitch")
  expect_error(flag_f0_tokens(d, min_f0 = 0), "`min_f0` must be")
  expect_error(flag_f0_tokens(d, min_f0 = NA), "`min_f0` must be")
  expect_error(flag_f0_tokens(d, min_run = 0), "`min_run` must be")
  expect_error(flag_f0_tokens(d, flag = "f0"), "must not be the token, time, f0 or run")
  expect_error(flag_f0_tokens(d, flag = "voiced_run"), "must not be the token, time, f0 or run")
  expect_error(flag_f0_tokens(d, flag = NULL), "`flag` must be a single column name")
  expect_error(flag_f0_tokens(d, flag = ""), "`flag` must be a single column name")
})
