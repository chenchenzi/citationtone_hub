# gaps.R - rows without f0, and gaps inside tokens.
#
# Internal helpers shared by the jump check, contour_duration(),
# trim_to_voiced(), flag_f0_gaps() and the Start-page screen, and the
# exported screen_f0_rows(), drop_rows_without_f0(), flag_f0_tokens() and
# suggest_unvoiced() that the screen uses. A gap is a
# stretch inside a token with no f0. It shows up in one of two ways: rows
# without f0 between two samples with f0, or, once such rows are removed, a
# time step that is longer than the token's usual step.

# Limits for times written at the step itself, such as a 10 ms grid written
# to 2 decimals (see f0_runs()), kept here so f0_runs() and grid_why() agree:
# at most this share of all steps may be other than exactly one unit, and
# a token where at least 2 steps, and more than this share of its steps, are
# exactly 2 units is set aside.
units_max_off   <- 0.01
units_tok_share <- 1 / 8

#' Which values are f0
#'
#' @description
#' `TRUE` where `x` holds an f0 value. In Hz only positive numbers count, so
#' `NA`, 0 and negative markers such as -1 are no f0. In a signed or
#' normalised column (semitones, z-scores) 0 and negative values are real f0,
#' so only `NA`, `NaN` and infinite values are no f0 there.
#'
#' @param x A vector of f0 values. A factor is read by its labels, and other
#'   types go through `as.numeric()`.
#' @param hz `TRUE` (default) when `x` is in Hz. `FALSE` for a signed or
#'   normalised column.
#' @return A logical vector as long as `x`, never `NA`.
#' @noRd
has_f0 <- function(x, hz = TRUE) {
  x <- gaps_as_numeric(x)
  if (isTRUE(hz)) is.finite(x) & x > 0 else is.finite(x)
}

#' Runs of samples with f0 inside each token
#'
#' @description
#' Numbers the runs of samples with f0 within each token: 1 up to the token's
#' first gap, 2 after it, and so on. This is the `voiced_run` column. Rows
#' without f0, and rows with no time, get `NA`.
#'
#' @details
#' Rows are taken in time order within each token. The input order is kept:
#' the result lines up with the input rows, whatever their order.
#'
#' A run ends at a gap. There are two kinds:
#'
#' * **From rows.** One or more rows without f0 lie between two samples with
#'   f0. A row without f0 at the same time as a sample with f0 does not count.
#'   These gaps are always marked.
#' * **From time steps.** The step between two samples with f0 is more than
#'   `gap_ratio` times the token's reference step. These gaps are marked only
#'   when the grid is regular (see below). They let data with the rows
#'   without f0 already removed give the same runs as data with them kept,
#'   in tokens whose usual step is one frame.
#'
#' Steps are taken between consecutive samples with f0, so rows without f0
#' change neither the reference step nor the grid check.
#'
#' **Reference step.** The token's median step when the token has at least 5
#' steps. A shorter token uses the smaller of its median step and the file's
#' common step, so a gap shows even in a token of 2 rows. The file has a
#' common step when at least 90% of tokens have a median step within 5% of
#' the median over tokens. Without one, short tokens also use their own
#' median, and a token with only 2 samples then cannot show a gap.
#'
#' The default `gap_ratio = 1.5` sits halfway between one step and two. Do not
#' use 2: with times written to a few decimals, a gap of one row gives a step
#' ratio of exactly 2 or just under it, so a 2x rule misses all of them. So
#' 1.5 already means "one frame or more missing", with room for jitter and
#' floating-point noise in the times. It is not a looser rule than "2 frames
#' or more".
#'
#' **Regular grid.** Gaps are read from the time steps only when all of these
#' hold:
#'
#' * at most 0.1% of steps are between 1.25 and 1.75 times the reference
#'   step, so one step and two steps are clearly apart;
#' * the times are written finely enough (see below);
#' * at least one token can be read: it has a step and is not set aside
#'   (below).
#'
#' **Repeated times.** A token where two samples with f0 have the same time
#' (up to floating-point noise) is not one series of frames, so it is set
#' aside: no gaps are read from its time steps. Its runs still split at rows
#' without f0. Every other token is read as usual. Only samples with f0
#' count, so a row without f0 at the time of a sample does not make a
#' repeat.
#'
#' **How finely the times are written.** The resolution is the step to which
#' the times are written: 0.01 for times written to 2 decimals, 1 for whole
#' numbers, 0 for more than 9 decimals.
#'
#' * When the resolution is at most a fifth of the file's step, the steps are
#'   read as they are, against `gap_ratio`.
#' * When the resolution is the step itself, as for a 10 ms grid written to 2
#'   decimals, the steps are read in whole units of the resolution
#'   (`round(step / resolution)`, which also removes floating-point noise).
#'   One unit is one frame, so a step of 2 units or more is a gap. This needs
#'   nearly all steps to be exactly 1 unit: at most 1% (1 in 100) of the
#'   steps, counted over all tokens (those set aside too), may be 0 units or
#'   2 or more. A true step off a whole number of units fails this. For
#'   example 12.5 ms written to 2 decimals gives steps of 0.01 and 0.02 in
#'   turn (25% are 2 units), and 7.5 ms gives steps of 0 and 0.01 (25% are 0
#'   units). Such steps are rounding, not gaps. Written to 2 decimals, a true
#'   step of `10 * (1 + e)` ms gives a share `e` of 2-unit steps, so 1%
#'   rejects true steps from about 10.1 ms up, such as 10.135 ms (Praat's
#'   automatic step for a 74 Hz floor), in all but small files. A true step
#'   closer to 10 ms cannot be told from real gaps. When the grid is read,
#'   at most 1% of the steps can be false gaps.
#' * In whole units, a token whose usual step is 1 unit but where at least 2
#'   steps, and more than 1 in 8, are exactly 2 units is set aside, as for a
#'   repeated time. Its frames are probably a little more than one unit
#'   apart, as for 12.5 ms frames among 10 ms ones (a pitch floor set per
#'   speaker). The 1% limit above still counts its steps.
#' * Otherwise, for example a 25 ms grid written to 2 decimals, no gaps are
#'   read from the time steps.
#'
#' Samples timed at each glottal pulse fail the first check. When the grid is
#' not regular, only gaps from rows are marked.
#'
#' Give the real time column, never a `*_t01`, `*_tseq` or `*_tbound` axis:
#' those restart or change step at every segment, which reads as false gaps.
#'
#' @param token Token ID for each row. Length 1 is recycled. `NA` is its own
#'   token.
#' @param time Time of each row, numeric. A factor is read by its labels.
#' @param has_f0 Logical, `TRUE` where the row has f0 (see `has_f0()`).
#'   Length 1 is recycled, and `NA` counts as no f0.
#' @param gap_ratio A step longer than this many reference steps is a gap.
#'   Must be above 1. `Inf` marks only the gaps from rows.
#' @return An integer vector as long as `time`. Its attribute `"grid"` is a
#'   list with the grid details:
#'   * `regular`: `TRUE` when gaps were read from the time steps (above), in
#'     the tokens not set aside. `FALSE` when there is no step to judge.
#'   * `step`: the file's step, the median over tokens of each token's median
#'     step, in time units. `NA` when there is no step.
#'   * `common`: `TRUE` when `step` is the file's common step (above).
#'   * `ambiguous`: number of steps between 1.25 and 1.75 times the reference.
#'   * `nonincreasing`: number of steps of 0, where two samples with f0 of a
#'     token have the same time.
#'   * `resolution`: how finely the times are written (above).
#'   * `units`: `TRUE` when the times are written at the step itself, so the
#'     steps are read in whole units of `resolution` (above).
#'   * `off_unit`: with `units`, the number of steps that are not exactly 1
#'     unit (0 units, or 2 or more), in all tokens. `NA` otherwise.
#'   * `units_ok`: `TRUE` when `units` and `off_unit` is at most 1% of
#'     `n_steps`.
#'   * `n_steps`: number of steps with a reference step.
#'   * `n_tokens_read`: number of tokens whose time steps can be read: they
#'     have such a step and are not set aside.
#'   * `repeated`: number of tokens with a repeated time (above).
#'   * `repeated_tokens`: those tokens, as given in `token`.
#'   * `other_step`: with `units`, number of tokens set aside because more
#'     than 1 step in 8 is exactly 2 units (above). 0 otherwise.
#'   * `other_step_tokens`: those tokens, as given in `token`.
#'   * `n_gaps_rows`: number of gaps from rows.
#'   * `n_gaps_time`: number of steps above `gap_ratio` times the reference,
#'     counted whether or not the grid is regular, whether or not the token
#'     is set aside, and whether or not rows without f0 lie in the gap.
#'
#'   Use `as.integer()` to drop the attribute before storing the runs in a
#'   column.
#' @noRd
f0_runs <- function(token, time, has_f0, gap_ratio = 1.5) {
  n <- length(time)
  if (length(token) == 1L) token <- rep(token, n)
  if (length(has_f0) == 1L) has_f0 <- rep(has_f0, n)
  if (length(token) != n || length(has_f0) != n) {
    stop("`token`, `time` and `has_f0` must have the same length.",
         call. = FALSE)
  }
  if (!is.numeric(gap_ratio) || length(gap_ratio) != 1L ||
      is.na(gap_ratio) || gap_ratio <= 1) {
    stop("`gap_ratio` must be a single number above 1.", call. = FALSE)
  }

  tv  <- gaps_as_numeric(time)
  hv  <- as.logical(has_f0)
  hv[is.na(hv)] <- FALSE
  utok <- unique(token)
  tid <- match(token, utok)

  run  <- rep(NA_integer_, n)
  grid <- list(regular = FALSE, step = NA_real_, common = FALSE,
               ambiguous = 0L, nonincreasing = 0L, resolution = NA_real_,
               units = FALSE, off_unit = NA_integer_, units_ok = FALSE,
               n_steps = 0L, n_tokens_read = 0L, repeated = 0L,
               repeated_tokens = utok[0L], other_step = 0L,
               other_step_tokens = utok[0L], n_gaps_rows = 0L,
               n_gaps_time = 0L)

  # Rows in time order within each token. At equal times, rows with f0 come
  # first, so a row without f0 always sits before a strictly later sample.
  idx <- which(is.finite(tv))
  o   <- idx[order(tid[idx], tv[idx], !hv[idx])]
  m   <- length(o)
  if (m == 0L || !any(hv[o])) {
    attr(run, "grid") <- grid
    return(run)
  }
  g   <- tid[o]
  t   <- tv[o]
  h   <- hv[o]
  pos <- seq_len(m)
  gstart <- cummax(ifelse(c(TRUE, g[-1L] != g[-m]), pos, 0L))

  # Gaps from rows: a row without f0 strictly after the token's previous
  # sample with f0, followed by another sample with f0.
  last <- cummax(ifelse(h, pos, 0L))       # latest sample with f0 so far
  last[last < gstart] <- 0L
  prev_t <- rep(NA_real_, m)
  prev_t[last > 0L] <- t[last[last > 0L]]
  opens <- !h & !is.na(prev_t) & t > prev_t
  cv <- c(0L, cumsum(opens))
  prev_f0 <- c(0L, last[-m])               # latest sample with f0 before
  prev_f0[prev_f0 < gstart] <- 0L
  gap_rows <- h & prev_f0 > 0L & (cv[pos + 1L] - cv[prev_f0 + 1L]) > 0L

  # Gaps from time steps, between consecutive samples with f0.
  f  <- which(h)
  k  <- length(f)
  gf <- g[f]
  tf <- t[f]
  first_f <- c(TRUE, gf[-1L] != gf[-k])
  st <- c(NA_real_, diff(tf))
  st[first_f] <- NA_real_
  has_st <- !is.na(st)
  ug <- unique(gf)
  gi <- match(gf, ug)                      # token of each step, 1 to nu
  nu <- length(ug)
  # A step of 0, up to floating-point noise, is a repeated time.
  zero <- has_st & st <= 1e-9 * pmax(1, abs(tf))
  resolution <- time_resolution(tf)

  ref  <- step_reference(st, has_st & !zero, gi, nu)
  fine <- !is.na(resolution) && is.finite(ref$step) &&
    resolution <= ref$step / 5 * (1 + 1e-9)
  # Times written at the step itself (a 10 ms grid written to 2 decimals):
  # read the steps in whole units of the resolution.
  units <- FALSE
  x <- st
  if (!fine && isTRUE(resolution > 0) && is.finite(ref$step)) {
    xu <- round(st / resolution)
    zu <- has_st & xu == 0
    ru <- step_reference(xu, has_st & !zu, gi, nu)
    if (isTRUE(ru$step == 1)) {
      units <- TRUE
      x <- xu
      zero <- zu
      ref <- ru
    }
  }
  rep_tok <- tabulate(gi[zero], nu) > 0L

  r <- x / ref$ref[gi]
  has_r <- !is.na(r)
  # In whole units, a token whose usual step is 1 unit but where at least 2
  # steps, and more than 1 in 8, are exactly 2 units has frames a little
  # more than a unit apart (12.5 ms written to 2 decimals gives 0.01, 0.01,
  # 0.01, 0.02): set it aside, as for a repeated time.
  oth_tok <- logical(nu)
  if (units) {
    k2 <- tabulate(gi[has_r & x == 2 & ref$ref[gi] == 1], nu)
    oth_tok <- !rep_tok & k2 >= 2L &
      k2 > units_tok_share * tabulate(gi[has_r], nu)
  }
  aside <- rep_tok | oth_tok
  gap_time  <- has_r & r > gap_ratio
  ambiguous <- sum(has_r & r > 1.25 & r < 1.75)
  n_steps   <- sum(has_r)
  n_read    <- sum(tabulate(gi[has_r], nu) > 0L & !aside)
  # The share counts every token's steps, those set aside too: their 0- and
  # 2-unit steps are evidence that the frames are not one unit apart.
  off_unit  <- if (units) sum(has_r & x != 1) else NA_integer_
  units_ok  <- units && off_unit <= units_max_off * n_steps
  regular <- n_steps > 0L && ambiguous <= 0.001 * n_steps && n_read > 0L &&
    (fine || units_ok)

  # Number the runs within each token and map them back to the input rows.
  brk <- gap_rows[f] | (regular & gap_time & !aside[gi])
  cs  <- cumsum(brk)
  fpos <- cummax(ifelse(first_f, seq_len(k), 0L))
  run[o[f]] <- as.integer(1L + cs - cs[fpos])

  grid$regular       <- regular
  grid$step          <- if (units) ref$step * resolution else ref$step
  grid$common        <- ref$common
  grid$ambiguous     <- as.integer(ambiguous)
  grid$nonincreasing <- as.integer(sum(zero))
  grid$resolution    <- resolution
  grid$units         <- units
  grid$off_unit      <- as.integer(off_unit)
  grid$units_ok      <- units_ok
  grid$n_steps       <- as.integer(n_steps)
  grid$n_tokens_read <- as.integer(n_read)
  grid$repeated      <- as.integer(sum(rep_tok))
  grid$repeated_tokens <- utok[sort(ug[rep_tok])]
  grid$other_step    <- as.integer(sum(oth_tok))
  grid$other_step_tokens <- utok[sort(ug[oth_tok])]
  grid$n_gaps_rows   <- as.integer(sum(gap_rows))
  grid$n_gaps_time   <- as.integer(sum(gap_time))
  attr(run, "grid") <- grid
  run
}

# Each token's reference step, from its steps `x` where `use` is TRUE: the
# token's median step when it has at least 5 steps, else the smaller of its
# median and the file's common step (when there is one). `gi` gives each
# step's token, 1 to `nu`. Also returns the file's step (the median over
# tokens of their median steps) and whether it is common.
step_reference <- function(x, use, gi, nu) {
  sp  <- split(x[use], factor(gi[use], levels = seq_len(nu)))
  med <- vapply(sp, function(v) if (length(v)) stats::median(v) else NA_real_,
                numeric(1), USE.NAMES = FALSE)
  nst <- lengths(sp, use.names = FALSE)
  tm  <- med[!is.na(med)]
  step   <- if (length(tm)) stats::median(tm) else NA_real_
  common <- length(tm) > 0L && mean(abs(tm / step - 1) <= 0.05) >= 0.9
  ref <- med
  if (common) {
    short <- nst < 5L & !is.na(med)
    ref[short] <- pmin(med[short], step)
  }
  list(step = step, common = common, ref = ref)
}

# How finely the times are written: 10^-d for the fewest decimals d (0 to 9)
# that every time has, 1 for whole numbers, 0 when they need more than 9.
time_resolution <- function(x) {
  x <- unique(x[is.finite(x)])
  if (!length(x)) return(NA_real_)
  for (d in 0:9) {
    s <- x * 10^d
    if (all(abs(s - round(s)) <= 1e-6)) return(10^-d)
  }
  0
}

# Numbers from a column of any type: a factor by its labels, text through
# as.numeric() (anything else becomes NA).
gaps_as_numeric <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(x))
}

#' Screen f0 data for rows without f0
#'
#' @description
#' Counts the rows without f0 in each token: at the start of the token,
#' at its end, and inside it, in gaps between two samples with f0. It also
#' counts the gaps that the time steps show, and the short runs of samples
#' that a gap cuts off at the start or end of a token. Nothing is changed:
#' [drop_rows_without_f0()] removes the rows, and the counts here say what it
#' does with the same arguments. The app's Start page shows these counts when
#' a file is uploaded.
#'
#' @details
#' **Rows without f0.** A row has no f0 when its value in the `f0` column is
#' `NA`, `NaN` or infinite, or is one of the `unvoiced` values. Only this one
#' column counts. Run [zero_hz_to_na()] first, so that 0 Hz counts as no f0
#' in Hz columns. Other 0 and negative values count as f0 unless they are
#' listed in `unvoiced`: in a signed or normalised column (semitones,
#' z-scores) they are real values.
#'
#' **Where they are.** Rows are taken in time order within each token,
#' whatever their order in `data`. A row without f0 is at the start when its
#' time is before the token's first sample with f0, at the end when it is
#' after the last one, and inside otherwise. A row without f0 at the same
#' time as a sample with f0 counts as inside, but it makes no gap. A token
#' with no f0 at all is counted apart: its rows are none of start, end or
#' inside. A row with no time (`NA`) cannot be placed either. It still counts
#' in `n_rows`, and in `n_f0` when it has f0. A row whose token is `NA`
#' belongs to a token of its own.
#'
#' **Gaps.** A gap is a stretch inside a token with no f0. A gap from rows has
#' one or more rows without f0 between two samples with f0. A gap from time
#' steps is a step between two samples with f0 that is more than `gap_ratio`
#' times the token's usual step. Gaps from time steps are read only when the
#' time steps form a regular grid. Then the same gaps show whether or not the
#' rows without f0 are still in the data, for tokens whose usual step is one
#' frame. (In a token where every other frame has no f0, the usual step is
#' two frames, so once those rows are removed the steps show no gap.) The
#' attribute `grid` says whether the grid is regular, and why not. Samples
#' timed at each glottal pulse are not regular. Give the real time column,
#' never a `*_t01`, `*_tseq` or `*_tbound` axis: those restart or change
#' step at every segment.
#'
#' The default `gap_ratio = 1.5` already means "one frame or more missing":
#' a gap of one frame doubles the step, and 1.5 leaves room for jitter and
#' floating-point noise in the times.
#'
#' **Times written to the step.** When the times are written about as
#' finely as the step, as for a 10 ms grid written to 2 decimals (0.01, 0.02,
#' ...), the steps are read in whole units of 0.01: a step of 0.02 or more is
#' a gap. This needs at least 99% of the steps to be exactly one unit. A
#' true step that is not a whole number of units fails this, such as 12.5 ms
#' or 7.5 ms written to 2 decimals: rounding gives steps of 0 or 0.02 in a
#' regular pattern, not gaps. So does a file with more than 1 gap in 100
#' steps, as the two cannot be told apart. Times written more coarsely than
#' the step, such as a 25 ms grid written to 2 decimals, give no gaps from
#' the time steps. A true step within 1% of the unit, such as 10.05 ms
#' written to 2 decimals, gives too few 2-unit steps to be told from real
#' gaps, so write the times to at least 3 decimals when the step is not
#' exactly 10 ms.
#'
#' **Tokens set aside.** No gaps are read from the time steps of these
#' tokens. Their gaps from rows still count, and the other tokens are read as
#' usual.
#'
#' * A token where two samples with f0 have the same time: the samples are
#'   not one series of frames. The attribute `repeated_time` lists these
#'   tokens.
#' * With times written to the step, a token where at least 2 steps, and
#'   more than 1 in 8, are exactly 2 units (0.02): its frames are probably
#'   a little more than one unit apart, as for 12.5 ms frames among 10 ms
#'   ones. A 10 ms token with that many gaps is set aside too, as the two
#'   cannot be told apart. The attribute `other_step` lists these tokens.
#'
#' A sample with f0 but no time leaves a hole in the time steps. On a regular
#' grid its neighbours are then two steps apart, which reads as a gap.
#'
#' **Gaps an earlier removal marked.** When the data already have a `run`
#' column (`voiced_run`, from an earlier [drop_rows_without_f0()] or from F0
#' Extraction), a change in its value inside a token is a gap too. This keeps
#' the gaps from rows removed earlier, which the time steps cannot show on a
#' grid that is not regular. `run = NULL` ignores the column.
#'
#' **Short edge runs.** A run is a stretch of samples with f0 between two
#' gaps, or between a gap and the start or end of the token. A run of 1 or 2
#' samples that a gap cuts off at the start or end of a token is often a
#' tracking error in the silence. It holds the token's first or last f0, so
#' it sets where the 0-1 time axes start or end, whether or not the rows
#' without f0 are removed. `frag_start` and `frag_end` count these samples:
#' those before the token's first run of at least 3 samples, and those after
#' its last. A token with no run that long has none.
#' `drop_rows_without_f0(min_run = 3)` removes them.
#'
#' **Equal row counts.** When every token has the same number of rows `N`,
#' as after resampling to `N` points, and a token's first or last row has no
#' f0, removing that row leaves the token with fewer than `N` points. The
#' time axes that run from the first f0 to the last (`token_t01`,
#' `<tier>_t01` and the models' per-token rescale) start or end such a token
#' one point in whether or not the row is removed, so its points sit off the
#' `N`-point grid, stretched by at least `(N - 1) / (N - 2)`, 5% for 21
#' points. `time_prop` is used as it is and keeps the grid. The attribute
#' `stretch` records this case, and the same for the short edge runs that
#' `min_run = 3` removes, which do move the first or last f0.
#'
#' **Tokens missing an end point.** A token with f0 whose first or last row
#' has no f0 is missing an end point. When every token has `N` rows, such a
#' token cannot have `N` points over the part with f0. The attribute
#' `edge_missing` lists these tokens, and the summary counts them and their
#' rows. They are counted for any data, but matter most when every token has
#' the same number of points, as after resampling.
#'
#' **Values that may mark no f0.** Some trackers write 0 or a value such as
#' -1 for unvoiced frames, in a column [zero_hz_to_na()] does not pick. The
#' attribute `unvoiced_suggested` gives the values [suggest_unvoiced()] finds
#' in the `f0` column, and the rows that hold each. It is read from the
#' column as it is, before `unvoiced`, so it stays the same once the values
#' are passed to `unvoiced`.
#'
#' @param data A data frame in long format: one row per f0 sample.
#' @param token,time,f0 Column names. Defaults `"token"`, `"time"`, `"f0"`.
#'   `time` must be the real time column. `f0` is the one column that decides
#'   which rows have f0.
#' @param gap_ratio A time step longer than this many times the token's usual
#'   step is a gap. Default 1.5. Must be above 1. `Inf` reads gaps from rows
#'   without f0 only.
#' @param unvoiced Values in the `f0` column to read as no f0, such as `-1`
#'   or `c(0, -1)`, for a column in Hz whose tracker marks unvoiced frames
#'   this way. Default `NULL`: only `NA` counts as no f0.
#' @param run Name of a column that marks gaps already, such as the
#'   `voiced_run` column [drop_rows_without_f0()] adds. Default
#'   `"voiced_run"`. Used only when the column is present. `NULL` ignores it.
#' @return A data frame with one row per token, in order of first appearance
#'   in `data`. Its first column holds the token and is named as the `token`
#'   column. The other columns are:
#'   * `n_rows`: rows of the token.
#'   * `n_f0`: rows with f0. These are the rows left after removal.
#'   * `n_start`, `n_end`, `n_inside`: rows without f0 at the start, at the
#'     end, and inside the token.
#'   * `n_gaps`: gaps from rows without f0.
#'   * `n_gaps_time`: gaps that the time steps between samples with f0 show.
#'     A gap from rows usually shows here too, as a long step, but not in a
#'     token whose usual step is more than one frame (see Details). Always 0
#'     when the grid is not regular.
#'   * `frag_start`, `frag_end`: samples in short runs cut off at the start
#'     and end (see Details).
#'
#'   Attributes:
#'   * `summary`: a named integer vector of counts over the whole file:
#'     `rows`, `tokens`, `rows_f0`, `rows_without_f0`, `unvoiced` (rows read
#'     as no f0 through `unvoiced`), `start` and `start_tokens`, `end` and
#'     `end_tokens`, `inside`, `gaps` and `gap_tokens` (gaps from rows),
#'     `gaps_time` and `gaps_time_tokens` (gaps from time steps, as in
#'     `n_gaps_time`), `gaps_time_only` and `gaps_time_only_tokens` (gaps
#'     from time steps with no rows without f0 in them, and the tokens with
#'     one), `gaps_run` and `gaps_run_tokens` (gaps that only
#'     the `run` column marks), `no_f0_rows` and `no_f0_tokens` (tokens with
#'     no f0), `no_time` (rows without f0 that cannot be placed, as they or
#'     the token's samples with f0 have no time), `rows_after` and
#'     `tokens_after` (left after removal), `run_tokens` and `max_run`
#'     (tokens whose `voiced_run` would go above 1, and its highest value),
#'     `few_tokens` (tokens left with 1 or 2 samples), `short_runs`,
#'     `short_runs_start`, `short_runs_end`, `short_rows`, `short_tokens`,
#'     `short_tokens_start`, `short_tokens_end` and `short_tokens_both`
#'     (short edge runs, their samples, and the tokens with one, at the
#'     start, at the end, or at both ends), `rows_after_short` (rows left
#'     after removal with `min_run = 3`), `repeated_tokens` (tokens with a
#'     repeated time, as in `repeated_time`), `other_step_tokens`
#'     (tokens set aside for their 2-unit steps, as in `other_step`), and
#'     `edge_tokens` and `edge_rows` (tokens with f0 whose first or last row
#'     has no f0, and all their rows). The `voiced_run` column that
#'     [drop_rows_without_f0()] adds marks `gaps + gaps_time_only +
#'     gaps_run` gaps. A token can end with two short runs, so `short_runs`
#'     can be more than `short_tokens_start + short_tokens_end`.
#'   * `grid`: a list with the time-step details. `regular` is `TRUE` when
#'     gaps were read from the time steps (in the tokens not set aside).
#'     `step` is the file's usual step and `common` is `TRUE` when most
#'     tokens share it. `ambiguous` counts steps between 1.25 and 1.75 times
#'     the usual step, `nonincreasing` counts steps of 0 (repeated times), and
#'     `resolution` is how finely the times are written. `units` is `TRUE`
#'     when the times are written to the step itself, so steps were read in
#'     whole units of `resolution`. `off_unit` then counts the steps that are
#'     not exactly one unit, and `units_ok` is `TRUE` when they are at most
#'     1% of the steps. `n_steps` counts the steps judged, `n_tokens_read`
#'     the tokens whose steps can be read, `repeated` and `repeated_tokens`
#'     the tokens with a repeated time, `other_step` and `other_step_tokens`
#'     the tokens set aside for their 2-unit steps, `n_gaps_rows` the gaps
#'     from rows, and `n_gaps_time` the steps above `gap_ratio` times the
#'     usual step, regular or not. `why` gives the reasons the grid is not
#'     regular, as sentences named `"no_steps"`, `"ambiguous"`,
#'     `"nonincreasing"` (every token has a repeated time), `"set_aside"`
#'     (every token is set aside, some for their 2-unit steps), `"units"`
#'     (times written to the step, but more than 1% of the steps are not
#'     exactly one unit) and `"resolution"` (times written too coarsely). It
#'     is empty for a regular grid.
#'   * `equal_rows`: `TRUE` when there are at least two tokens and all have
#'     the same number of rows. A single token is not counted, so that one
#'     token of frame data is not taken for resampled points.
#'   * `stretch`: a list. `warn` is `TRUE` when `equal_rows` is `TRUE` and
#'     some token with f0 has no f0 in its first or last row. `n_rows` is
#'     the number of rows each token has (`NA` unless `equal_rows`),
#'     `n_tokens` the number of tokens with f0 that have no f0 in their
#'     first or last row, and `factor` the least stretch of such a token on
#'     the axes from the first f0 to the last, `(N - 1) / (N - 2)`, whether
#'     or not the row is removed. `warn_short` and
#'     `n_tokens_short` say the same for the samples with f0 that
#'     `min_run = 3` would remove at a token's start or end.
#'   * `no_f0`: the tokens with no f0, as text. Removal drops them entirely.
#'   * `repeated_time`: the tokens where two samples with f0 have the same
#'     time, as text. No gaps are read from their time steps.
#'   * `other_step`: the tokens set aside because more than 1 step in 8 is
#'     exactly 2 units, as text (see Details). No gaps are read from their
#'     time steps.
#'   * `few_samples`: the tokens left with 1 or 2 samples after removal, as
#'     a named integer vector of the samples left. [flag_f0_tokens()] flags
#'     them.
#'   * `edge_missing`: the tokens with f0 whose first or last row has no
#'     f0, as text (see Details).
#'   * `unvoiced_suggested`: a data frame with columns `value` (the values
#'     [suggest_unvoiced()] finds in the `f0` column, such as 0 and -1) and
#'     `rows` (the rows holding each value). It has no rows when there is
#'     nothing to suggest.
#'   * `settings`: the arguments used, as a list.
#' @seealso [drop_rows_without_f0()] to remove the rows; [zero_hz_to_na()]
#'   to run first; [suggest_unvoiced()] for values that may mark no f0;
#'   [flag_f0_tokens()] to flag the tokens with an issue; [trim_to_voiced()]
#'   and [flag_f0_gaps()].
#' @examples
#' df <- data.frame(
#'   token = rep(c("a", "b"), each = 10),
#'   time  = rep(0.005 + 0.01 * (0:9), 2),
#'   f0    = c(NA, 200, 202, NA, NA, 206, 208, 209, NA, NA,
#'             NA, NA, 181, 183, 185, 187, 188, NA, 190, NA))
#' scr <- screen_f0_rows(df)
#' scr
#' attr(scr, "summary")[c("rows_without_f0", "start", "end", "inside")]
#' attr(scr, "grid")$regular
#' # A gap cuts off 2 samples at the start of token a, and 1 at the end of b
#' scr[c("token", "frag_start", "frag_end")]
#' # Tokens whose first or last row has no f0
#' attr(scr, "summary")[c("edge_tokens", "edge_rows")]
#' attr(scr, "edge_missing")
#' @export
screen_f0_rows <- function(data, token = "token", time = "time", f0 = "f0",
                           gap_ratio = 1.5, unvoiced = NULL,
                           run = "voiced_run") {
  core <- f0_rows_core(data, token, time, f0, gap_ratio, unvoiced,
                       min_run = 3, run = run)
  nt  <- core$nt
  tid <- core$tid
  has <- core$has
  tv  <- core$tv

  n_rows <- tabulate(tid, nt)
  n_f0   <- tabulate(tid[has], nt)
  n_gaps <- tabulate(core$stok[core$brk_row], nt)

  # Gaps from the time steps, between samples with f0 only.
  hf <- which(has)
  rt <- as.integer(f0_runs(tid[hf], tv[hf], TRUE, gap_ratio))
  n_gaps_time <- pmax(run_max(tid[hf], rt, nt) - 1L, 0L)

  frag_start <- tabulate(tid[core$short_start], nt)
  frag_end   <- tabulate(tid[core$short_end], nt)
  max_run    <- run_max(tid, core$runs, nt)

  per <- data.frame(token = core$tok[match(seq_len(nt), tid)],
                    n_rows = n_rows, n_f0 = n_f0,
                    n_start = core$n_start, n_end = core$n_end,
                    n_inside = core$n_inside, n_gaps = n_gaps,
                    n_gaps_time = n_gaps_time,
                    frag_start = frag_start, frag_end = frag_end,
                    stringsAsFactors = FALSE)
  names(per)[1L] <- token

  with_f0 <- n_f0 > 0L
  short_s <- core$short_runs_start
  short_e <- core$short_runs_end
  left    <- n_f0[with_f0]
  col_only <- core$brk_col & !core$brk_step     # gaps only the run column has
  time_only <- core$brk_step & !core$brk_row    # gaps only the time steps show

  # Tokens with f0 whose first or last row has no f0
  edge_loss <- with_f0 & (core$n_start > 0L | core$n_end > 0L)
  summ <- c(
    rows             = length(tid),
    tokens           = nt,
    rows_f0          = sum(has),
    rows_without_f0  = sum(!has),
    unvoiced         = core$n_unvoiced,
    start            = sum(core$n_start),
    start_tokens     = sum(core$n_start > 0L),
    end              = sum(core$n_end),
    end_tokens       = sum(core$n_end > 0L),
    inside           = sum(core$n_inside),
    gaps             = sum(n_gaps),
    gap_tokens       = sum(n_gaps > 0L),
    gaps_time        = sum(n_gaps_time),
    gaps_time_tokens = sum(n_gaps_time > 0L),
    gaps_time_only   = sum(time_only),
    gaps_time_only_tokens = sum(tabulate(core$stok[time_only], nt) > 0L),
    gaps_run         = sum(col_only),
    gaps_run_tokens  = sum(tabulate(core$stok[col_only], nt) > 0L),
    no_f0_rows       = sum(n_rows[!with_f0]),
    no_f0_tokens     = sum(!with_f0),
    no_time          = core$n_unplaced,
    rows_after       = sum(has),
    tokens_after     = sum(with_f0),
    run_tokens       = sum(max_run > 1L),
    max_run          = max(c(0L, max_run)),
    few_tokens       = sum(left < 3L),
    short_runs       = sum(short_s) + sum(short_e),
    short_runs_start = sum(short_s),
    short_runs_end   = sum(short_e),
    short_rows       = sum(frag_start) + sum(frag_end),
    short_tokens     = sum(short_s > 0L | short_e > 0L),
    short_tokens_start = sum(short_s > 0L),
    short_tokens_end = sum(short_e > 0L),
    short_tokens_both = sum(short_s > 0L & short_e > 0L),
    rows_after_short = sum(has) - sum(frag_start) - sum(frag_end),
    repeated_tokens  = length(core$grid$repeated_tokens),
    other_step_tokens = length(core$grid$other_step_tokens),
    edge_tokens      = sum(edge_loss),
    edge_rows        = sum(n_rows[edge_loss]))
  storage.mode(summ) <- "integer"

  equal_rows <- nt >= 2L && all(n_rows == n_rows[1L])
  edge_short <- with_f0 & (frag_start > 0L | frag_end > 0L)
  n_pts      <- if (equal_rows) n_rows[1L] else NA_integer_
  stretch <- list(
    warn     = equal_rows && any(edge_loss),
    n_rows   = n_pts,
    n_tokens = sum(edge_loss),
    factor   = if (!is.na(n_pts) && n_pts > 2L) (n_pts - 1) / (n_pts - 2) else
      NA_real_,
    warn_short     = equal_rows && any(edge_short),
    n_tokens_short = sum(edge_short))

  few <- with_f0 & n_f0 < 3L
  few_samples <- stats::setNames(n_f0[few], as.character(per[[1L]][few]))

  # Values that may mark no f0 in an Hz column, read from the column as it is,
  # before `unvoiced`, so the suggestion stays the same once it is taken up.
  raw <- gaps_as_numeric(data[[f0]])
  sug <- suggest_unvoiced(raw)
  unv_sug <- data.frame(
    value = as.numeric(sug),
    rows  = vapply(as.numeric(sug), function(v) sum(raw %in% v), integer(1)))

  attr(per, "summary")     <- summ
  attr(per, "grid")        <- core$grid
  attr(per, "equal_rows")  <- equal_rows
  attr(per, "stretch")     <- stretch
  attr(per, "no_f0")       <- as.character(per[[1L]][!with_f0])
  attr(per, "repeated_time") <- as.character(core$grid$repeated_tokens)
  attr(per, "other_step")  <- as.character(core$grid$other_step_tokens)
  attr(per, "few_samples") <- few_samples
  attr(per, "edge_missing") <- as.character(per[[1L]][edge_loss])
  attr(per, "unvoiced_suggested") <- unv_sug
  attr(per, "settings")    <- list(token = token, time = time, f0 = f0,
                                   gap_ratio = gap_ratio,
                                   unvoiced = core$unvoiced, run = run)
  per
}

#' Remove rows without f0 and mark the gaps they leave
#'
#' @description
#' Removes the rows whose f0 is missing, and adds a column, `voiced_run`, that
#' numbers the stretches with f0 in each token: 1 up to the token's first
#' gap, 2 after it, and so on. The time column still shows each gap as a
#' longer step, and `voiced_run` keeps it whatever is done to the rows later.
#' This is what the app's Start page does when "Remove rows without f0" is
#' ticked. [screen_f0_rows()] gives the counts first.
#'
#' @details
#' **Which rows go.** Run [zero_hz_to_na()] first, so that 0 Hz counts as no
#' f0 in Hz columns. Then the values in `unvoiced` are read as `NA` in the
#' `f0` column, and every row whose f0 is `NA` (or `NaN` or infinite) is
#' removed. Only the `f0` column decides. 0 and negative values count as no
#' f0 only through `unvoiced`, because in a signed or normalised column they
#' are real values. A token with no f0 at all is removed entirely. A row with
#' f0 but no time (`NA`) is kept, with `voiced_run` `NA`. On a regular grid
#' its neighbours are then two steps apart, which reads as a gap, so with
#' `min_run = 3` a short run next to it at the token's start or end can go.
#' A row whose token is `NA` belongs to a token of its own.
#'
#' **Short edge runs.** With `min_run = 3`, runs of 1 or 2 samples that a gap
#' cuts off at the start or end of a token go too: the samples before the
#' token's first run of at least `min_run` samples, and after its last one.
#' Runs are split at every gap, as in the run column below. A token with no
#' run that long keeps all its samples. The default `min_run = 1` removes no
#' samples with f0. These samples have f0, so removing them moves the
#' token's first or last f0, and with it the 0-1 time axes (`token_t01`,
#' `<tier>_t01` and the models' per-token rescale). Removing rows without
#' f0 alone never moves those axes. Only runs at the token's start and end
#' go: a short run next to a gap inside the token stays.
#'
#' **The run column.** A gap is one of these:
#'
#' * one or more rows without f0 between two kept samples;
#' * when the time steps form a regular grid, a step more than `gap_ratio`
#'   times the token's usual step, in a token that is not set aside (see
#'   [screen_f0_rows()]);
#' * a change in the value of an existing `run` column, such as the
#'   `voiced_run` of an earlier removal.
#'
#' So data whose rows without f0 were removed beforehand get the same
#' `voiced_run` as data with them kept, as long as the grid is regular and
#' the token's usual step is one frame. And running this again on its own
#' output, after more rows have lost their f0, keeps the gaps the first run
#' marked, on any grid.
#'
#' The column is added when rows are removed, or when the gaps make some
#' token's runs go above 1. When neither happens, `data` is returned as it
#' is, and a `voiced_run` column already there is left alone. Otherwise a
#' column of that name is replaced, with a message, by one that still marks
#' its gaps. On a grid that is not regular, the time steps cannot show the
#' gaps, so run this on data that still has its rows without f0 or its
#' `voiced_run` column.
#'
#' [contour_duration()], [trim_to_voiced()] and [flag_f0_gaps()] read the
#' `voiced_run` column, so they give the same results on the output as on
#' the data with the rows kept. Other columns, including `has_gap` and
#' `n_missing`, are kept as they are.
#'
#' @inheritParams screen_f0_rows
#' @param run Name of the column to add. Default `"voiced_run"`. When the
#'   column is there already, its gaps are kept (see Details). `NULL` adds no
#'   column and reads none.
#' @param min_run Samples a run at the start or end of a token must have to be
#'   kept. Default `1` keeps all samples with f0. `3` also removes runs of 1
#'   or 2 samples cut off by a gap (see Details).
#' @return `data` without the rows without f0, of the same class (a data
#'   frame or a tibble), with the rows in their original order and all
#'   columns kept. The `run` column, an integer, is added as described in
#'   Details. Attribute `rows_without_f0` is a list:
#'   * `f0` and `unvoiced`: the column screened and the values read as no
#'     f0.
#'   * `rows`: rows in `data`.
#'   * `removed`: rows removed in all.
#'   * `start`, `end`, `inside`: rows without f0 removed at the start, at the
#'     end, and inside tokens. `no_f0`: rows removed with the tokens with no
#'     f0. `no_time`: rows without f0 that cannot be placed, as they or the
#'     token's samples with f0 have no time.
#'   * `short_runs`: samples with f0 removed by `min_run`.
#'   * `tokens_lost`: the tokens removed entirely, as text.
#'   * `gaps_rows`: gaps in the output from rows that were removed.
#'   * `gaps_time_only`: gaps in the output found only from the time steps,
#'     as in the summary of [screen_f0_rows()].
#'   * `gaps_run`: gaps in the output that only the existing `run` column
#'     marked.
#'   * `grid`: the time-step details, as in [screen_f0_rows()].
#' @seealso [screen_f0_rows()] for the counts before removal;
#'   [zero_hz_to_na()] to run first; [contour_duration()],
#'   [trim_to_voiced()] and [flag_f0_gaps()], which read `voiced_run`.
#' @examples
#' df <- data.frame(
#'   token = rep(c("a", "b"), each = 10),
#'   time  = rep(0.005 + 0.01 * (0:9), 2),
#'   f0    = c(NA, 200, 202, NA, NA, 206, 208, 209, NA, NA,
#'             NA, NA, 181, 183, 185, 187, 188, NA, 190, NA))
#' out <- drop_rows_without_f0(df)
#' out
#' attr(out, "rows_without_f0")[c("removed", "start", "end", "inside")]
#'
#' # The same voiced_run when the rows were removed beforehand: the time
#' # steps show the gaps
#' clean <- df[!is.na(df$f0), ]
#' identical(drop_rows_without_f0(clean)$voiced_run, out$voiced_run)
#'
#' # Also remove the runs of 1 or 2 samples cut off at the start or end
#' drop_rows_without_f0(df, min_run = 3)
#'
#' # A tracker that writes -1 for unvoiced frames
#' df2 <- data.frame(token = "c", time = 0.005 + 0.01 * (0:5),
#'                   f0 = c(-1, 150, 152, -1, 155, -1))
#' drop_rows_without_f0(df2, unvoiced = -1)
#' @export
drop_rows_without_f0 <- function(data, token = "token", time = "time",
                                 f0 = "f0", run = "voiced_run",
                                 gap_ratio = 1.5, min_run = 1,
                                 unvoiced = NULL) {
  core <- f0_rows_core(data, token, time, f0, gap_ratio, unvoiced, min_run,
                       run = run)
  tid  <- core$tid
  has  <- core$has
  tv   <- core$tv
  frag <- core$short_start | core$short_end
  keep <- has & !frag
  with_f0 <- tabulate(tid[has], core$nt) > 0L

  # Runs left once the short edge runs are gone: the same runs, numbered from
  # the token's first run that is kept.
  runs <- core$runs - (core$first_long[tid] - 1L)
  no_long <- is.na(core$first_long[tid])
  runs[no_long] <- core$runs[no_long]
  runs <- as.integer(runs)

  # Gaps between the runs that are kept.
  sr <- core$runs[core$s]
  fl <- core$first_long[core$stok]
  ll <- core$last_long[core$stok]
  kept_brk <- is.na(fl) | (sr > fl & sr <= ll)

  removed <- sum(!keep)
  add <- !is.null(run) &&
    (removed > 0L || any(runs[keep] > 1L, na.rm = TRUE))

  out <- data
  if (removed > 0L) {
    out <- data[keep, , drop = FALSE]
    if (!inherits(out, "tbl_df")) rownames(out) <- NULL
  }
  if (add) {
    if (run %in% names(data)) {
      message("Replaced the existing `", run, "` column. The new one still ",
              "marks the gaps the old one marked, and adds the gaps found ",
              "now.")
    }
    out[[run]] <- runs[keep]
  }

  lost <- !with_f0[tid]
  attr(out, "rows_without_f0") <- list(
    f0          = f0,
    unvoiced    = core$unvoiced,
    rows        = length(tid),
    removed     = removed,
    start       = sum(core$n_start),
    end         = sum(core$n_end),
    inside      = sum(core$n_inside),
    no_f0       = sum(lost),
    no_time     = core$n_unplaced,
    short_runs  = sum(frag),
    tokens_lost = as.character(core$tok[match(which(!with_f0), tid)]),
    gaps_rows   = sum(core$brk_row & kept_brk),
    gaps_time_only = sum(core$brk_step & !core$brk_row & kept_brk),
    gaps_run    = sum(core$brk_col & !core$brk_step & kept_brk),
    grid        = core$grid)
  out
}

#' Flag tokens with too few samples with f0, or with points missing
#'
#' @description
#' Adds a column, `screen_flag`, that says in a few words what is wrong
#' with each token: `""` (empty) for a token with no issue, otherwise its
#' reasons, such as `"only 2 samples with f0"`, the same text on every row
#' of the token. No row is removed: keep the tokens with no issue with
#' `data[data$screen_flag == "", ]` if wanted. This is what the app's Start
#' page does before its *Drop the N flagged tokens* box.
#'
#' @details
#' **Reasons**, in this order, joined by `"; "`:
#'
#' 1. `"no f0"`: no row of the token has f0. No other reason is given.
#' 2. `"only 2 samples with f0"` or `"only 1 sample with f0"`: fewer rows
#'    with f0 than `min_f0` (with the default 3, 1 or 2 rows). A contour
#'    needs at least 3 points to show a turn. No other reason is given
#'    either, as such a token cannot be used whatever else is missing. With
#'    `min_run` above 1, the rows that [drop_rows_without_f0()] removes as
#'    short edge runs with the same `min_run` are not counted.
#' 3. Equal-point data only: `"no f0 at the first point"`,
#'    `"no f0 at the last point"` or `"no f0 at the first and last points"`.
#' 4. Equal-point data only: `"3 points without f0 inside"` or
#'    `"1 point without f0 inside"`: every row without f0 between the
#'    token's first and last samples with f0, whatever the length of the
#'    gap.
#'
#' **Equal-point data.** When there are at least two tokens and all have
#' the same number of rows, as for 21 equidistant points from resampling,
#' F0 Extraction or a Praat script (`equal_rows` in [screen_f0_rows()]),
#' each token should have f0 at every point, so reasons 3 and 4 apply too,
#' and the reasons say "points". In frame-by-frame data (tokens of
#' different lengths), rows without f0 at a token's start or end or inside
#' it are normal, and [drop_rows_without_f0()] removes them. So only
#' reasons 1 and 2 apply, and the reasons say "samples".
#'
#' **The data as given.** The reasons are read from `data` as it is. A row
#' has no f0 when its value in the `f0` column is `NA`, `NaN` or infinite;
#' 0 and negative values count as f0 here. So run [zero_hz_to_na()] first,
#' and set values such as -1 that a tracker writes for unvoiced frames to
#' `NA`. Rows are taken in time order within each token, whatever their
#' order in `data`. A row with no time (`NA`) counts in reason 2 when it has
#' f0, but cannot be placed for reasons 3 and 4. A row whose token is `NA`
#' belongs to a token of its own.
#'
#' **`min_run`.** It changes reason 2 only when it is below `min_f0`: a
#' token with a run of at least `min_run` samples keeps that run, and a
#' token with no such run keeps all its samples. So with `min_run = 3` and
#' the default `min_f0 = 3`, the flags are the same as with `min_run = 1`.
#'
#' @inheritParams screen_f0_rows
#' @param min_f0 A token with fewer rows with f0 than this is flagged
#'   (reason 2). Default `3`. Must be at least 1.
#' @param min_run Samples a run at the start or end of a token must have to
#'   be counted, as in [drop_rows_without_f0()]. Default `1` counts every
#'   row with f0.
#' @param flag Name of the column to add. Default `"screen_flag"`. A column
#'   of that name already in `data` is replaced.
#' @return `data` with the `flag` column, a character column, added or
#'   replaced, of the same class (a data frame or a tibble), with the rows
#'   in their original order and every other column as it was. Attribute
#'   `flagged` is a data frame with one row per flagged token, in order of
#'   first appearance in `data`: the token, in a column named as the
#'   `token` column, and `reason`, the text in the `flag` column. It has no
#'   rows when no token is flagged.
#' @seealso [zero_hz_to_na()] to run first; [screen_f0_rows()] for the
#'   counts; [drop_rows_without_f0()] to remove the rows without f0.
#' @examples
#' # 7 equidistant points per token
#' df <- data.frame(
#'   token = rep(c("a", "b", "c", "d"), each = 7),
#'   time  = rep(seq(0, 0.3, length.out = 7), 4),
#'   f0    = c(200, 205, 210, 215, 220, 225, 230,
#'             NA, 181, 183, NA, NA, NA, 190,
#'             NA, NA, 150, NA, NA, 154, NA,
#'             rep(NA, 7)))
#' # b misses its first point and 3 inside; c has only 2 points with f0,
#' # which is its only reason; d has no f0
#' out <- flag_f0_tokens(df)
#' unique(out[c("token", "screen_flag")])
#' attr(out, "flagged")
#' # Keep the tokens with no issue
#' out[out$screen_flag == "", ]
#'
#' # Frame-by-frame data: only too few samples with f0 count
#' fr <- data.frame(token = rep(c("x", "y"), c(8, 5)),
#'                  time  = c(1:8, 1:5) / 100,
#'                  f0    = c(NA, 200, 202, 204, NA, 206, 208, NA,
#'                            NA, 150, NA, 152, NA))
#' attr(flag_f0_tokens(fr), "flagged")
#' @export
flag_f0_tokens <- function(data, token = "token", time = "time", f0 = "f0",
                           gap_ratio = 1.5, run = "voiced_run", min_f0 = 3,
                           min_run = 1, flag = "screen_flag") {
  if (!is.numeric(min_f0) || length(min_f0) != 1L || is.na(min_f0) ||
      min_f0 < 1) {
    stop("`min_f0` must be a single number of at least 1.", call. = FALSE)
  }
  if (!is.character(flag) || length(flag) != 1L || is.na(flag) ||
      !nzchar(flag)) {
    stop("`flag` must be a single column name.", call. = FALSE)
  }
  if (flag %in% c(token, time, f0, run)) {
    stop("`flag` must not be the token, time, f0 or run column.",
         call. = FALSE)
  }
  core <- f0_rows_core(data, token, time, f0, gap_ratio, unvoiced = NULL,
                       min_run = min_run, run = run)
  nt  <- core$nt
  tid <- core$tid
  has <- core$has

  n_rows  <- tabulate(tid, nt)
  with_f0 <- tabulate(tid[has], nt) > 0L
  # The rows with f0 that drop_rows_without_f0() with this min_run keeps
  n_keep  <- tabulate(tid[has & !core$short_start & !core$short_end], nt)
  equal   <- nt >= 2L && all(n_rows == n_rows[1L])
  unit    <- if (equal) "point" else "sample"
  # "1 point", "3 points", for each count
  n_units <- function(n, u) paste(fmt_n(n), ifelse(n == 1, u, paste0(u, "s")))

  r_none <- r_few <- r_edge <- r_in <- rep("", nt)
  r_none[!with_f0] <- "no f0"
  few <- with_f0 & n_keep < min_f0
  if (any(few)) {
    r_few[few] <- paste("only", n_units(n_keep[few], unit), "with f0")
  }
  if (equal) {
    s0 <- with_f0 & core$n_start > 0L
    e0 <- with_f0 & core$n_end > 0L
    r_edge[s0 & e0]  <- "no f0 at the first and last points"
    r_edge[s0 & !e0] <- "no f0 at the first point"
    r_edge[!s0 & e0] <- "no f0 at the last point"
    inside <- with_f0 & core$n_inside > 0L
    if (any(inside)) {
      r_in[inside] <- paste(n_units(core$n_inside[inside], "point"),
                            "without f0 inside")
    }
  }
  # "only N ... with f0" stands alone, as "no f0" does
  r_edge[few] <- ""
  r_in[few]   <- ""
  join <- function(a, b) {
    both <- nzchar(a) & nzchar(b)
    out  <- paste0(a, b)
    out[both] <- paste0(a[both], "; ", b[both])
    out
  }
  reason <- join(join(join(r_none, r_few), r_edge), r_in)

  data[[flag]] <- reason[tid]
  hit <- which(nzchar(reason))
  flagged <- data.frame(token = core$tok[match(hit, tid)],
                        reason = reason[hit], stringsAsFactors = FALSE)
  names(flagged)[1L] <- token
  attr(data, "flagged") <- flagged
  data
}

# The counts shared by screen_f0_rows() and drop_rows_without_f0(), so the
# two always agree. Runs are split at gaps from rows and, on a regular grid,
# from time steps (f0_runs()), and where the `run` column changes when the
# data have one. Short edge runs are the runs before a token's first run of
# at least `min_run` samples and after its last one.
f0_rows_core <- function(data, token, time, f0, gap_ratio, unvoiced, min_run,
                         run = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  for (a in list(token, time, f0)) {
    if (!is.character(a) || length(a) != 1L || is.na(a)) {
      stop("`token`, `time` and `f0` must each be a single column name.",
           call. = FALSE)
    }
  }
  if (!is.null(run) && (!is.character(run) || length(run) != 1L ||
                        is.na(run) || !nzchar(run))) {
    stop("`run` must be a single column name, or NULL.", call. = FALSE)
  }
  if (!is.null(run) && run %in% c(token, time, f0)) {
    stop("`run` must not be the token, time or f0 column.", call. = FALSE)
  }
  missing_cols <- setdiff(c(token, time, f0), names(data))
  if (length(missing_cols) > 0L) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(min_run) || length(min_run) != 1L || is.na(min_run) ||
      min_run < 1) {
    stop("`min_run` must be a single number of at least 1.", call. = FALSE)
  }
  if (!is.null(unvoiced)) {
    if (!is.numeric(unvoiced)) {
      stop("`unvoiced` must be NULL or numbers, such as -1 or c(0, -1).",
           call. = FALSE)
    }
    unvoiced <- unique(as.numeric(unvoiced[is.finite(unvoiced)]))
    if (!length(unvoiced)) unvoiced <- NULL
  }

  tok <- data[[token]]
  tv  <- gaps_as_numeric(data[[time]])
  x   <- gaps_as_numeric(data[[f0]])
  unv <- if (length(unvoiced)) !is.na(x) & x %in% unvoiced else
    logical(length(x))
  x[unv] <- NA
  has <- has_f0(x, hz = FALSE)
  tid <- match(tok, unique(tok))
  nt  <- length(unique(tok))

  steps <- f0_runs(tok, tv, has, gap_ratio)        # gaps from rows and steps
  grid  <- attr(steps, "grid")
  steps <- as.integer(steps)
  rows  <- as.integer(f0_runs(tok, tv, has, Inf))  # gaps from rows only
  grid$why <- grid_why(grid)

  # Samples with f0 and a time, in time order within each token. Runs never
  # go down along this order.
  s <- which(!is.na(steps))
  s <- s[order(tid[s], tv[s])]
  k <- length(s)
  stok <- tid[s]
  newtok <- c(TRUE, stok[-1L] != stok[-k])
  if (k == 0L) newtok <- logical(0)
  brk_row  <- !newtok & c(FALSE, diff(rows[s]) != 0L)
  brk_step <- !newtok & c(FALSE, diff(steps[s]) != 0L)

  # Gaps an existing run column records, such as the voiced_run of an earlier
  # removal. On a grid that is not regular it is the only record of them, so
  # the runs also break where its value changes, as in voiced_run_ids(),
  # which contour_duration() and trim_to_voiced() use. NA is a value of its
  # own there.
  brk_col <- logical(k)
  if (!is.null(run) && run %in% names(data) && k > 1L) {
    rk <- data[[run]]
    rk <- if (is.factor(rk)) as.character(rk) else rk
    rk <- as.character(rk)[s]
    rk[is.na(rk)] <- "\rNA"
    brk_col <- !newtok & c(FALSE, rk[-1L] != rk[-k])
  }
  brk_all <- brk_step | brk_col

  # Number the runs within each token, along `s`.
  runs <- rep(NA_integer_, length(tid))
  if (k > 0L) {
    cs   <- cumsum(brk_all)
    fpos <- cummax(ifelse(newtok, seq_len(k), 0L))
    runs[s] <- as.integer(1L + cs - cs[fpos])
  }

  # Where the rows without f0 are: before the first sample with f0, after
  # the last, or between.
  t_first <- t_last <- rep(NA_real_, nt)
  t_first[rev(stok)] <- rev(tv[s])
  t_last[stok] <- tv[s]
  off   <- !has & !is.na(tv)
  at_s  <- off & tv < t_first[tid]
  at_e  <- off & tv > t_last[tid]
  at_i  <- off & tv >= t_first[tid] & tv <= t_last[tid]
  at_s[is.na(at_s)] <- FALSE
  at_e[is.na(at_e)] <- FALSE
  at_i[is.na(at_i)] <- FALSE
  # Rows without f0 that cannot be placed: no time, or a token whose samples
  # with f0 have no time. Tokens with no f0 are counted apart.
  with_f0  <- tabulate(tid[has], nt) > 0L
  unplaced <- !has & with_f0[tid] & !(at_s | at_e | at_i)

  # Run sizes, and each token's first and last run of at least min_run.
  mx <- run_max(tid, runs, nt)
  first_long <- last_long <- rep(NA_integer_, nt)
  if (k > 0L) {
    base <- c(0L, cumsum(mx))[stok]
    size <- tabulate(base + runs[s], sum(mx))
    gtok <- rep(seq_len(nt), mx)
    grun <- sequence(mx)
    il   <- which(size >= min_run)
    first_long[rev(gtok[il])] <- rev(grun[il])
    last_long[gtok[il]] <- grun[il]
  }
  fl <- first_long[tid]
  ll <- last_long[tid]
  short_start <- !is.na(runs) & !is.na(fl) & runs < fl
  short_end   <- !is.na(runs) & !is.na(ll) & runs > ll

  list(tok = tok, tid = tid, nt = nt, tv = tv, has = has,
       unvoiced = unvoiced, n_unvoiced = sum(unv),
       runs = runs, grid = grid,
       s = s, stok = stok, brk_all = brk_all, brk_row = brk_row,
       brk_step = brk_step, brk_col = brk_col,
       n_start = tabulate(tid[at_s], nt), n_end = tabulate(tid[at_e], nt),
       n_inside = tabulate(tid[at_i], nt), n_unplaced = sum(unplaced),
       first_long = first_long, last_long = last_long,
       short_start = short_start, short_end = short_end,
       short_runs_start = ifelse(is.na(first_long), 0L, first_long - 1L),
       short_runs_end = ifelse(is.na(last_long), 0L, mx - last_long))
}

# Highest run number of each token (0 for a token with no run).
run_max <- function(tid, runs, nt) {
  out <- integer(nt)
  ok  <- which(!is.na(runs))
  if (!length(ok)) return(out)
  o <- ok[order(tid[ok], runs[ok])]
  out[tid[o]] <- runs[o]
  out
}

# Why the time steps are not a regular grid, in plain sentences named by the
# check that failed. Empty when the grid is regular.
grid_why <- function(grid) {
  if (isTRUE(grid$regular)) return(stats::setNames(character(0), character(0)))
  every_rep <- paste(
    "In every token, two samples with f0 have the same time, so no gaps are",
    "read from the time steps.")
  if (grid$n_steps == 0L) {
    if (grid$nonincreasing > 0L) return(c(nonincreasing = every_rep))
    return(c(no_steps = paste(
      "There are no time steps between samples with f0 to judge.")))
  }
  why <- character(0)
  if (grid$ambiguous > 0.001 * grid$n_steps) {
    why["ambiguous"] <- sprintf(paste(
      "%s of %s time steps are 1.25 to 1.75 times the usual step, so one",
      "step and two steps are not clearly apart."),
      fmt_n(grid$ambiguous), fmt_n(grid$n_steps))
  }
  res <- grid$resolution
  r1 <- format(res)
  r2 <- format(2 * res)
  if (grid$n_tokens_read == 0L) {
    oth <- if (is.null(grid$other_step)) 0L else grid$other_step
    if (oth == 0L && grid$repeated > 0L) {
      why["nonincreasing"] <- every_rep
    } else if (oth > 0L) {
      why["set_aside"] <- sprintf(paste(
        "No token can be read from its time steps:%s in %s, more than 1",
        "step in %s is exactly %s, so the frames are probably not %s apart."),
        if (grid$repeated > 0L) sprintf(
          " in %s, two samples with f0 have the same time, and",
          count_txt(grid$repeated, "token")) else "",
        if (grid$repeated > 0L) count_txt(oth, "token") else "every token",
        format(round(1 / units_tok_share)), r2, r1)
    }
  }
  if (isTRUE(grid$units)) {
    if (!isTRUE(grid$units_ok)) {
      why["units"] <- sprintf(paste(
        "The times are written to the nearest %s, the same as the usual",
        "step, so a step of %s or more could be a gap. But only %s of the",
        "steps are exactly %s, and at least %s must be. So either the frames",
        "are not exactly %s apart, or more than 1 step in %s is a gap. From",
        "times written this way, the two cannot be told apart."),
        r1, r2, pct_txt(1 - grid$off_unit / grid$n_steps), r1,
        pct_txt(1 - units_max_off, digits = 0), r1,
        format(round(1 / units_max_off)))
    }
  } else if (is.na(res) || res > grid$step / 5 * (1 + 1e-9)) {
    why["resolution"] <- sprintf(paste(
      "The times are written to the nearest %s. That is more than a fifth",
      "of the usual step (%s), so rounding could look like a gap."),
      format(res), format(signif(grid$step, 4)))
  }
  why
}

# Whole numbers with a thousands separator, counts with their unit, and
# shares as percentages, rounded down so a share under a limit never prints
# as the limit (0.98996 is "98.9%", not "99.0%").
fmt_n <- function(x) formatC(x, format = "d", big.mark = ",")
count_txt <- function(n, unit) {
  paste(fmt_n(n), if (n == 1) unit else paste0(unit, "s"))
}
pct_txt <- function(p, digits = 1) {
  k <- 10^digits
  sprintf(paste0("%.", digits, "f%%"), floor(100 * k * p + 1e-9) / k)
}

#' Values that may mark rows without f0 in an f0 column in Hz
#'
#' @description
#' Some pitch trackers write 0, or a value such as -1, for frames where they
#' found no f0. [zero_hz_to_na()] reads 0 as `NA` only in the columns it
#' recognises as Hz, so a column such as `f0_rapt` can keep its 0s. This
#' looks at the values of one column and returns those that probably mark no
#' f0, to pass to the `unvoiced` argument of [screen_f0_rows()] and
#' [drop_rows_without_f0()]. The app's Start page offers to read them as no
#' f0. [screen_f0_rows()] also reports them, with their row counts.
#'
#' @details
#' A column looks like Hz when the median of its positive values is 30 to
#' 2000, and it has no negative values apart from one value that occurs
#' more than once, such as -1. In such a column, 0 and that negative value
#' are returned, 0 first.
#'
#' Nothing is returned (`NULL`) for:
#'
#' * a signed or normalised column, such as semitones or z-scores: it has
#'   several different negative values, or a median outside 30 to 2000;
#' * a column with a single negative value. One value cannot be told from a
#'   real one, so the column is not taken for Hz, and its 0s are not offered
#'   either;
#' * a column with no 0 and no negative marker.
#'
#' The check looks only at the values. A scale that is never negative but is
#' not Hz, such as cents above each speaker's lowest f0, can have its 0s
#' returned too, although they are real values there. Use this on columns in
#' Hz only.
#'
#' @param x A vector of f0 values. A factor is read by its labels, and text
#'   through `as.numeric()`, so labels such as `"--undefined--"` are ignored.
#' @return A numeric vector of the values that may mark no f0, such as
#'   `c(0, -1)`, or `NULL` when there are none.
#' @seealso [screen_f0_rows()] and [drop_rows_without_f0()], whose
#'   `unvoiced` argument takes these values; [zero_hz_to_na()].
#' @examples
#' # 0 and -1 for unvoiced frames
#' suggest_unvoiced(c(0, 0, 182.5, 190.1, -1, -1, 201.3))
#' # A column zero_hz_to_na() has already read: nothing left to offer
#' suggest_unvoiced(c(NA, NA, 182.5, 190.1, 201.3))
#' # Semitones: 0 and negative values are real f0
#' suggest_unvoiced(c(-2.5, 0, 1.2, -0.3, 3.1, 0))
#'
#' df <- data.frame(token = "a", time = 0.005 + 0.01 * (0:5),
#'                  f0_rapt = c(0, 150, 152, 0, 155, 0))
#' unv <- suggest_unvoiced(df$f0_rapt)
#' drop_rows_without_f0(df, f0 = "f0_rapt", unvoiced = unv)
#' @export
suggest_unvoiced <- function(x) {
  x <- gaps_as_numeric(x)
  x <- x[is.finite(x)]
  pos <- x[x > 0]
  if (!length(pos)) return(NULL)
  med <- stats::median(pos)
  if (med < 30 || med > 2000) return(NULL)
  neg <- x[x < 0]
  marker <- unique(neg)
  if (length(marker) > 1L) return(NULL)
  if (length(marker) == 1L && length(neg) < 2L) return(NULL)
  out <- c(if (any(x == 0)) 0, marker)
  if (length(out)) out else NULL
}
