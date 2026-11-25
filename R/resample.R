# =============================================================================
# Equal-N resampling of long-format f0 contours.
#
# Turns each token's native pitch-frame grid (fixed time step, so long tokens
# have more frames than short ones) into a fixed number of equidistant points
# across the token's duration: the "measure f0 at every 5% / 10% of the
# syllable" convention common in tone research. Used by the F0 Extraction tab
# when the user picks "Equal number of points per token".
# =============================================================================

# Unvoiced marker. Pitch trackers disagree on how they signal "no f0": some
# write NA, others 0 Hz (RAPT/REAPER, some Praat scripts). 0 Hz is not a
# possible pitch, so both count as unvoiced here, matching inspect_f0() in
# R/inspect.R and the wrassp / Praat extraction paths in the app.
f0_voiced <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  !is.na(x) & x > 0
}

#' Find the f0 columns in Hz
#'
#' @description
#' Returns the names of the columns of `df` that hold f0 in Hz. These are
#' the columns where [zero_hz_to_na()] reads 0 as `NA`. The app's Start page
#' also takes its default f0 column from them.
#'
#' @details
#' A column is found by its name and by its values:
#'
#' * The name has the word `f0`, `pitch` or `hz` (so `f0hz` and `pitchhz`
#'   count too), or a VoiceSauce f0 name: `strF0`, `sF0`, `pF0`, `shrF0` or
#'   `oF0`. Words are split at any character other than a letter or digit.
#' * Every other word in the name keeps the values in Hz: a processing step
#'   (`raw`, `corrected`, `original`, `edited`, `manual`, `final`, `clean`,
#'   `smoothed`, `interp`, `filtered`), a tracker (`praat`, `reaper`), a
#'   summary (`mean`, `median`, `max`, `min`, `peak`), or a number. So
#'   `f0_corrected` and `mean_f0` are found, but not `f0_st`, `f0_dur`,
#'   `f0_peak_loc` or `pitch_rank`, where 0 is a real value.
#' * The values are numeric and never negative. A normalised column has
#'   negative values, so it is never found.
#' * The median of the positive values is between 30 and 2000.
#'
#' A column in Hz with another name, such as `f0_rapt`, is not found, and nor
#' is one that marks unvoiced frames with a negative number such as -1. Use
#' [suggest_unvoiced()] on such a column.
#'
#' @param df A data frame.
#' @return The names of the f0 columns in Hz, in the order of `df`. An empty
#'   character vector when there are none.
#' @seealso [zero_hz_to_na()], which reads 0 as `NA` in these columns;
#'   [suggest_unvoiced()] for values that may mark no f0 in other columns.
#' @examples
#' df <- data.frame(token = "a", time = 1:4 / 100,
#'                  f0 = c(0, 212, 215, 0), f0_corrected = c(0, 212, 0, 0),
#'                  f0_st = c(NA, 0.1, -0.2, NA), intensity = c(40, 68, 70, 41))
#' hz_f0_columns(df)
#' @export
hz_f0_columns <- function(df) {
  nm <- names(df)
  if (!length(nm)) return(character(0))
  words <- strsplit(gsub("(^|[^a-z0-9])f_0(?=[^a-z0-9]|$)", "\\1f0",
                         tolower(nm), perl = TRUE), "[^a-z0-9]+")
  f0_word <- "^(f0|pitch)(hz)?$|^hz$|^(str|s|p|shr|o)f0$"
  hz_word <- paste0("^(|[0-9]+|",
    "raw|corrected|corr|original|orig|edited|manual|final|clean|cleaned|",
    "smooth|smoothed|interp|interpolated|filtered|",
    "praat|reaper|mean|avg|median|max|min|peak)$")
  named <- vapply(words, function(w) {
    f0 <- grepl(f0_word, w)
    any(f0) && all(f0 | grepl(hz_word, w))
  }, logical(1))
  hz <- vapply(seq_along(nm), function(k) {
    x <- df[[k]]
    if (!named[k] || !is.numeric(x) || any(x < 0, na.rm = TRUE)) return(FALSE)
    pos <- x[!is.na(x) & x > 0]
    length(pos) > 0 && stats::median(pos) >= 30 && stats::median(pos) <= 2000
  }, logical(1))
  nm[hz]
}

#' Read 0 Hz f0 values as missing
#'
#' @description
#' Pitch trackers write 0 Hz for unvoiced frames, where no pitch was
#' measured. A 0 left in the data is read as a very low pitch: it pulls mean
#' contours and fits towards 0, and gives `-Inf` in semitones. This sets 0 to
#' `NA` in the f0 columns of `df`. The app's Start page does this to every
#' CSV it reads. Its screen ([screen_f0_rows()]) then counts the rows without
#' f0 in the chosen f0 column, and a box removes them with
#' [drop_rows_without_f0()]. The box is ticked by default, except for files
#' where every token has the same number of points and some token's first
#' or last point has no f0.
#'
#' @details
#' By default the f0 columns are found by name and by value. A column is
#' used when:
#' * its name has the word `f0`, `pitch` or `hz`, or one of VoiceSauce's f0
#'   names (`strF0`, `sF0`, `pF0`, `shrF0`, `oF0`), and its other words are
#'   ones that keep the values in Hz: `raw`, `corrected`, `original`,
#'   `edited`, `smoothed`, `interp`, `praat`, `reaper`, `mean`, `median`,
#'   `max`, `min`, `peak` and the like, or a number. So `f0`, `F0_Hz`,
#'   `pitch`, `f0_corrected` and `mean_f0` are used, but not `f0_st`,
#'   `f0_dur`, `f0_peak_loc` or `pitch_rank`, where 0 is a real value. Words
#'   are split at any character other than a letter or digit;
#' * it is numeric with no negative values, so a normalised column (which
#'   has negatives) is never touched;
#' * the median of its positive values lies between 30 and 2000, a
#'   plausible range for f0 in Hz.
#'
#' Negative values, which some trackers use for unvoiced frames, are left
#' as they are, and a column that has any is not used. Give `cols` to choose
#' the columns yourself. For a marker such as -1, or 0 in a column in Hz
#' that is not found (such as `f0_rapt`), [suggest_unvoiced()] finds the
#' values, and the `unvoiced` argument of [screen_f0_rows()] and
#' [drop_rows_without_f0()] reads them as no f0. The Start page offers to
#' read them as no f0.
#'
#' @param df A data frame.
#' @param cols Names of the columns to clean. Defaults to the f0 columns
#'   found as described in Details.
#' @return `df` with 0 set to `NA` in `cols`. Attribute `"zero_hz"` gives
#'   the number of values changed, named by column, for the columns with at
#'   least one; it is an empty integer vector when there were none.
#' @seealso [hz_f0_columns()] for the columns used; [normalise_f0()], which
#'   also reads f0 of 0 or below as `NA`; [screen_f0_rows()] and
#'   [drop_rows_without_f0()] to run next.
#' @examples
#' df <- data.frame(token = "ma1", time = 1:5 / 100,
#'                  f0 = c(0, 212, 215, 0, 210),
#'                  intensity = c(0, 68, 70, 66, 0))
#' out <- zero_hz_to_na(df)
#' out$f0          # NA where the tracker wrote 0
#' out$intensity   # unchanged
#' attr(out, "zero_hz")
#' @export
zero_hz_to_na <- function(df, cols = hz_f0_columns(df)) {
  cols <- intersect(as.character(cols), names(df))
  n <- stats::setNames(integer(length(cols)), cols)
  for (cl in cols) {
    x <- df[[cl]]
    if (!is.numeric(x)) next
    z <- !is.na(x) & x == 0
    if (!any(z)) next
    x[z] <- NA
    df[[cl]] <- x
    n[cl] <- sum(z)
  }
  attr(df, "zero_hz") <- n[n > 0]
  df
}

#' Resample each token's f0 contour to N equidistant points
#'
#' @description
#' For each token, replaces the native pitch-frame grid with `n` points
#' equally spaced between the token's first and last frame time, so every
#' token contributes the same number of samples at the same proportional
#' positions (with `n = 21`, one point every 5% of the token's duration;
#' with `n = 11`, every 10%). Intended for monosyllabic (single-contour)
#' tokens; for multisyllabic data, TextGrid landmark axes (see
#' [normalise_time_landmarks()]) are usually the better route.
#'
#' @details
#' f0 at each new point follows Praat's own rule for
#' `Pitch: Get value at time... Linear` (`Sampled_getValueAtX()` in
#' Praat's source), so a contour resampled here matches one extracted by
#' a Praat script querying the same times. Of the two native frames
#' bracketing the point, call the closer one *near* and the other *far*:
#'
#' * both voiced: the value is linearly interpolated between them;
#' * *far* unvoiced: the point takes the *near* frame's measured value,
#'   so no value is blended across an unvoiced frame while a usable
#'   measurement at the edge of one is not thrown away;
#' * *near* unvoiced: the point is `NA`, since nothing was measured there.
#'
#' A point landing exactly on a frame has a phase of 0 and therefore takes
#' that frame's value exactly. At a point falling exactly midway between two
#' frames the choice of *near* is a floating-point tie and may fall either
#' way; it only changes the answer when exactly one of the two is unvoiced.
#'
#' This reads gaps from rows only: unvoiced frames must be **present as `NA`
#' (or 0 Hz) rows**, which is how the wrassp and `.Pitch` paths deliver them.
#' Input without such rows has a gap only as a wide step between two voiced
#' frames: a `.PitchTier`, a CSV listing only voiced samples, or data whose
#' rows without f0 were removed (the app's Start page, or
#' [drop_rows_without_f0()]). Both bracketing frames are then voiced, and the
#' point is interpolated straight across the gap. Praat does the same on a
#' PitchTier. The time steps and a `voiced_run` column are not read here. So
#' resample before removing the rows without f0, as F0 Extraction does: its
#' "Drop rows without f0" option is applied after resampling.
#' [screen_f0_rows()] counts the gaps that the time steps show
#' (`n_gaps_time`), so a file can be checked for them first.
#'
#' With `method = "nearest"` (Praat's other option) the middle case goes
#' away: a point always takes the nearer frame's measured value, or `NA`
#' when that frame is unvoiced. Every exported value is then a number the
#' tracker produced, at the cost of a timing error of up to half a frame
#' step.
#'
#' If an `intensity` column is present, it is linearly interpolated
#' across all frames with finite intensity (ends extended, matching how
#' the extraction aligns the intensity track to the f0 frames).
#'
#' Any other column is carried through when it is constant within every
#' token (metadata, `token_dropped`, and similar token-level columns);
#' per-frame columns that vary within a token cannot survive a change of
#' grid and are dropped. Their names are recorded in the
#' `dropped_columns` attribute of the result.
#'
#' Rows with a missing time are dropped, and rows sharing a frame time
#' are collapsed to the first of them, so interpolation has strictly
#' increasing anchors. A token left with fewer than two distinct frame
#' times cannot be resampled and keeps its single remaining row, with
#' `time_prop = 0.5`.
#'
#' `time_prop` is used as it is by the model fits (see
#' [time_already_normalised()]). Over a region trimmed to its voiced part
#' (F0 Extraction's *Whole token (voiced region)*, or [trim_to_voiced()]), 0
#' and 1 are close to the first and last f0. Over a TextGrid interval, an
#' end point may have no f0, and the contour then does not reach 0 or 1.
#'
#' @param df Long-format f0 data frame.
#' @param n Number of points per token (at least 2). Default `21`.
#' @param token,time,f0 Column names. Defaults `"token"`, `"time"`,
#'   `"f0"`.
#' @param intensity Name of the optional intensity column; used only
#'   when present in `df`. Default `"intensity"`.
#' @param method How each point takes its value, mirroring the two
#'   interpolation choices Praat offers in `Pitch: Get value at time...`:
#'   `"linear"` (default, and Praat's own default) applies the rule in
#'   Details; `"nearest"` always takes the value of the frame the point
#'   falls in, and is `NA` when that frame is unvoiced, so nothing is
#'   ever computed.
#' @return A data frame with the `token`, `point` (measurement number,
#'   `1`...`n`), `time` (seconds, on the new grid), `time_prop`
#'   (proportional position 0-1 within the token: 0 and 1 are the first and
#'   last frames of the extracted region, which need not have f0), and `f0`
#'   columns, plus
#'   `intensity` (when present) and any carried token-constant columns.
#'   Input columns named `point` or `time_prop` are dropped, since those
#'   names are computed here. Rows are sorted by time within each token;
#'   tokens keep their order of first appearance. The attribute
#'   `dropped_columns` names any columns that could not be carried.
#' @seealso [normalise_time_token()] for a 0-1 column from the first f0 to
#'   the last, without changing the sampling grid. On equal-point data with
#'   no f0 at an end point, it moves the other points off the grid.
#' @export
resample_f0_equal <- function(df, n = 21, token = "token", time = "time",
                              f0 = "f0", intensity = "intensity",
                              method = c("linear", "nearest")) {
  method <- match.arg(method)
  n <- suppressWarnings(as.integer(n)[1])
  if (is.na(n) || n < 2) {
    stop("`n` must be an integer of at least 2.", call. = FALSE)
  }
  required <- c(token, time, f0)
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  for (cl in c(time, f0)) {
    if (!is.numeric(df[[cl]])) {
      stop("Column '", cl, "' must be numeric.", call. = FALSE)
    }
  }
  has_int <- intensity %in% names(df)
  # An intensity column that is missing throughout reads back from read.csv()
  # as logical; it carries no measurements either way, so accept it rather
  # than refusing to resample f0 over it.
  if (has_int && !is.numeric(df[[intensity]]) && !all(is.na(df[[intensity]]))) {
    stop("Column '", intensity, "' must be numeric.", call. = FALSE)
  }
  core <- c(token, time, f0, if (has_int) intensity)

  # A non-core column survives the change of grid only if it is constant
  # (a single unique value, NA included) within every token. Grouping by
  # match() rather than as.character() compares token IDs exactly and keeps
  # NA-token rows (which `==` would drop) in a group of their own.
  # `point` / `time_prop` are computed below, so same-named input columns are
  # dropped rather than allowed to overwrite them.
  tok_key <- match(df[[token]], unique(df[[token]]))
  extra   <- setdiff(names(df), c(core, "point", "time_prop"))
  # Judge constancy only over the rows that survive: a frame line with an
  # unreadable time is dropped below, and its (often NA) metadata must not
  # make a genuinely token-constant column look per-frame.
  usable  <- !is.na(suppressWarnings(as.numeric(df[[time]])))
  carried <- extra[vapply(extra, function(cl) {
    all(vapply(split(df[[cl]][usable], tok_key[usable]),
               function(x) length(unique(x)) <= 1L, logical(1)))
  }, logical(1))]
  dropped <- setdiff(extra, carried)

  pieces <- lapply(seq_len(max(tok_key, 0L)), function(g) {
    rows <- df[tok_key == g, , drop = FALSE]
    tok  <- rows[[token]][1]
    tv   <- as.numeric(rows[[time]])
    rows <- rows[!is.na(tv), , drop = FALSE]
    tv   <- tv[!is.na(tv)]
    if (nrow(rows) == 0) return(NULL)

    ord  <- order(tv)
    rows <- rows[ord, , drop = FALSE]
    tt   <- tv[ord]
    # Collapse duplicate frame times (keep the first) so interpolation has
    # strictly increasing anchors.
    keep <- !duplicated(tt)
    rows <- rows[keep, , drop = FALSE]
    tt   <- tt[keep]
    ff   <- suppressWarnings(as.numeric(rows[[f0]]))
    ff[!f0_voiced(ff)] <- NA_real_            # 0 Hz is unvoiced, not a value

    if (length(tt) < 2) {
      out <- data.frame(tok, 1L, tt, 0.5, ff, stringsAsFactors = FALSE)
      names(out) <- c(token, "point", time, "time_prop", f0)
      if (has_int) out[[intensity]] <- suppressWarnings(as.numeric(rows[[intensity]]))
    } else {
      t_out <- seq(tt[1], tt[length(tt)], length.out = n)
      # Praat's rule, from Sampled_getValueAtX() in fon/Sampled.cpp, which is
      # what `Pitch: Get value at time... Linear` runs. Work out which of the
      # two bracketing frames is NEAR and which is FAR, then:
      #   near unvoiced -> undefined (nothing measured to report)
      #   far  unvoiced -> the near frame's own measured value
      #   both voiced   -> linear blend, weighted by the distance to NEAR
      # Deciding on the nearer frame also removes any need to special-case a
      # point that lands exactly on a frame: there phase is 0, so the blend
      # returns that frame's value exactly, whatever the far frame holds.
      m <- length(tt)
      # Praat derives the frame index GLOBALLY, index_real = (x - x1)/dx, and
      # takes both the left index and the phase from it. Deriving the two
      # separately (findInterval for one, a local quotient for the other) lets
      # them disagree by an ulp and pick the wrong neighbour, so compute both
      # from one number. Praat's Pitch object is uniformly sampled by
      # construction; irregular input (a .PitchTier) falls back to the local
      # interval, which is the same quantity computed per segment.
      steps <- diff(tt)
      dx    <- (tt[m] - tt[1]) / (m - 1L)
      if (isTRUE(all.equal(steps, rep(dx, length(steps)), tolerance = 1e-9))) {
        idx   <- (t_out - tt[1]) / dx
        left  <- floor(idx) + 1L                  # Praat's leftIndex, 1-based
        phase <- idx - floor(idx)
      } else {
        left  <- findInterval(t_out, tt, all.inside = TRUE)
        phase <- (t_out - tt[left]) / (tt[left + 1L] - tt[left])
      }
      lo    <- phase < 0.5
      near  <- ifelse(lo, left, left + 1L)
      far   <- ifelse(lo, left + 1L, left)
      ph    <- ifelse(lo, phase, 1 - phase)                 # 0 .. 0.5
      # Praat returns undefined when NEAR falls outside the frames, and the
      # near value when only FAR does; an out-of-range index is read as NA
      # here so the rule below handles both the same way it handles unvoiced.
      at    <- function(i) ifelse(i >= 1L & i <= m, ff[pmin(pmax(i, 1L), m)], NA_real_)
      nv    <- at(near)
      f_out <- if (method == "nearest") {
        # Praat's "nearest": the frame the point falls in, and nothing when
        # that frame is unvoiced. No value is ever computed.
        nv
      } else {
        fv <- at(far)
        ifelse(is.na(nv), NA_real_,
               ifelse(is.na(fv), nv, nv + ph * (fv - nv)))
      }
      out <- data.frame(tok, seq_len(n), t_out, seq(0, 1, length.out = n), f_out,
                        stringsAsFactors = FALSE)
      names(out) <- c(token, "point", time, "time_prop", f0)
      if (has_int) {
        iv  <- suppressWarnings(as.numeric(rows[[intensity]]))
        fin <- is.finite(iv)
        out[[intensity]] <- if (method == "nearest") {
          ifelse(near >= 1L & near <= m, iv[pmin(pmax(near, 1L), m)], NA_real_)
        } else if (sum(fin) >= 2) {
          stats::approx(tt[fin], iv[fin], xout = t_out, rule = 2)$y
        } else if (sum(fin) == 1) {
          rep(iv[fin], n)          # rule = 2 with one anchor: hold it flat
        } else {
          rep(NA_real_, n)
        }
      }
    }
    for (cl in carried) out[[cl]] <- rows[[cl]][1]
    out
  })

  out <- do.call(rbind, Filter(Negate(is.null), pieces))
  if (is.null(out)) {
    out <- data.frame(character(0), integer(0), numeric(0), numeric(0), numeric(0),
                      stringsAsFactors = FALSE)
    names(out) <- c(token, "point", time, "time_prop", f0)
    if (has_int) out[[intensity]] <- numeric(0)
    for (cl in carried) out[[cl]] <- df[[cl]][0]
  }
  rownames(out) <- NULL
  attr(out, "dropped_columns") <- dropped
  out
}

# Runs of samples with f0, as contour_duration(), trim_to_voiced() and
# flag_f0_gaps() use them. A run is numbered within its token in time order
# (1, 2, ...) and ends at a gap: rows without f0 between two samples with f0,
# a time step longer than 1.5 times the token's usual step (f0_runs() in
# R/gaps.R, only on a regular grid), or a change in `run`, the voiced_run
# column, when the data have one. That column keeps the gaps the Start page
# took from the rows it removed, which the time steps cannot show on an
# irregular grid. `use_steps = FALSE` reads no gaps from the time steps (for
# row order standing in for time). Rows without f0 or time get NA.
voiced_run_ids <- function(token, time, measured, run = NULL,
                           use_steps = TRUE) {
  runs <- as.integer(f0_runs(token, time, measured,
                             gap_ratio = if (use_steps) 1.5 else Inf))
  if (is.null(run)) return(runs)
  ok <- which(!is.na(runs))
  if (length(ok) < 2L) return(runs)
  tid <- match(token, unique(token))
  tv  <- gaps_as_numeric(time)
  o   <- ok[order(tid[ok], runs[ok], tv[ok])]
  m   <- length(o)
  rk  <- as.character(if (is.factor(run)) as.character(run) else run)
  rk[is.na(rk)] <- "\rNA"                     # no run value: a value of its own
  g     <- tid[o]
  first <- c(TRUE, g[-1L] != g[-m])
  brk   <- first | c(FALSE, runs[o][-1L] != runs[o][-m] |
                            rk[o][-1L] != rk[o][-m])
  cs   <- cumsum(brk)
  fpos <- cummax(ifelse(first, seq_len(m), 0L))
  runs[o] <- as.integer(1L + cs - cs[fpos])
  runs
}

# The first and last time of each unit's contour, anchored on runs: from the
# first sample of the unit's first run with at least `min_run` samples to the
# last sample of its last such run. With no run that long, the first and last
# runs are used. Only the unit's own rows count towards a run's length, so a
# run that crosses a segment boundary is cut there. `unit` is an integer ID
# (NA for rows outside every unit) and `runs` comes from voiced_run_ids().
# Returns list(lo, hi), indexed by unit, NA for a unit with no f0.
run_anchor_span <- function(unit, time, runs, min_run) {
  if (!is.numeric(min_run) || length(min_run) != 1L || is.na(min_run)) {
    stop("`min_run` must be a single number.", call. = FALSE)
  }
  nu <- max(c(0L, unit), na.rm = TRUE)
  lo <- hi <- rep(NA_real_, nu)
  ok <- which(!is.na(unit) & !is.na(runs) & !is.na(time))
  if (length(ok) == 0L) return(list(lo = lo, hi = hi))
  o <- ok[order(unit[ok], runs[ok], time[ok])]
  u <- unit[o]; r <- runs[o]; t <- time[o]
  m <- length(o)
  newg  <- c(TRUE, u[-1L] != u[-m] | r[-1L] != r[-m])
  gid   <- cumsum(newg)
  size  <- tabulate(gid)
  gu    <- u[newg]
  g_lo  <- t[newg]
  g_hi  <- t[c(which(newg)[-1L] - 1L, m)]
  long  <- size >= min_run
  unit_long <- rep(FALSE, nu)
  unit_long[gu[long]] <- TRUE
  cand  <- which(long | !unit_long[gu])        # fall back to any run
  first <- cand[!duplicated(gu[cand])]
  last  <- cand[!duplicated(gu[cand], fromLast = TRUE)]
  lo[gu[first]] <- g_lo[first]
  hi[gu[last]]  <- g_hi[last]
  list(lo = lo, hi = hi)
}

#' Trim each token to its voiced region
#'
#' @description
#' Keeps only the rows between a token's first and last voiced frame, so a
#' downstream measurement spans the syllable rather than the whole recording.
#' Frame-based trackers emit a frame every step across the entire file, with
#' `NA` (or 0 Hz) f0 in silence; without trimming, "21 equidistant points
#' across the token" spends part of the grid on leading and trailing silence.
#'
#' @details
#' The edges are anchored on a *run* of at least `min_run` voiced frames in a
#' row, so one stray voiced frame in silence (a common octave-error artefact)
#' cannot stretch the region. If no run is that long, the first and last
#' voiced frames are used. Tokens with no voiced frame at all keep no rows, so
#' count them before and after if their loss should be reported. Rows with no
#' time are dropped too.
#'
#' A run ends at a gap. A gap is one of these:
#'
#' * rows without f0 between two voiced frames;
#' * a time step longer than 1.5 times the token's usual step, so input with
#'   the rows without f0 already removed gives the same region. This needs a
#'   regular frame grid, so it does not apply to sparse input such as a
#'   `.PitchTier`;
#' * a change in the `run` column (`voiced_run`, which the Start page adds
#'   when it removes rows without f0 or marks gaps), when the data have one.
#'
#' Frames *inside* the region are kept whether or not they are voiced, so an
#' internal unvoiced stretch (a medial voiceless stop, a creaky patch) stays
#' in place and still reads as `NA`.
#'
#' Data whose rows without f0 were removed beforehand have no silence left
#' to trim. Only a short run cut off by a gap at a token's start or end can
#' still go, and only where the time steps (on a regular grid, see
#' [screen_f0_rows()]) or the `run` column show the gap. Without either,
#' every row is kept.
#'
#' @param df Long-format f0 data frame.
#' @param token,time,f0 Column names. Defaults `"token"`, `"time"`, `"f0"`.
#'   `time` must be the real time column, not a 0-1 axis.
#' @param min_run Minimum number of voiced frames in a row that an edge
#'   anchor must belong to. Default `2`.
#' @param run Name of a column that numbers the runs of f0 within each token,
#'   such as `voiced_run`. Used only when the column is present; `NULL`
#'   ignores it.
#' @return `df` with out-of-region rows removed, row order preserved.
#' @seealso [resample_f0_equal()], which is normally applied after this;
#'   [filter_interval_rows()] for a TextGrid-interval region instead;
#'   [contour_duration()], which measures the same region.
#' @export
trim_to_voiced <- function(df, token = "token", time = "time", f0 = "f0",
                           min_run = 2, run = "voiced_run") {
  missing_cols <- setdiff(c(token, time, f0), names(df))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  tv  <- gaps_as_numeric(df[[time]])
  rv  <- if (!is.null(run) && run %in% names(df)) df[[run]] else NULL
  runs <- voiced_run_ids(df[[token]], tv, has_f0(df[[f0]]), run = rv)
  key  <- match(df[[token]], unique(df[[token]]))
  span <- run_anchor_span(key, tv, runs, min_run)

  # A token with no voiced frame has no span, so it drops out.
  keep <- !is.na(tv) & tv >= span$lo[key] & tv <= span$hi[key]
  keep[is.na(keep)] <- FALSE
  out <- df[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Flag tokens whose f0 has gaps inside the measured region
#'
#' @description
#' Adds two token-level columns so unvoiced stretches are visible in the
#' exported data rather than being silently scattered `NA`s: `n_missing`,
#' the number of that token's rows with no f0, and `has_gap`, `TRUE` when
#' the token's voicing is interrupted somewhere between its first and last
#' f0.
#'
#' @details
#' The distinction matters because a missing value at the edge of a region
#' usually just means the region ran slightly past the voicing, while a
#' missing value in the middle means voicing was interrupted (a medial
#' voiceless stop, a creaky patch, or a tracking failure). Only the latter
#' sets `has_gap`, so `has_gap` marks the tokens worth looking at in F0
#' Correction. Run this on the native frames rather than on a resampled
#' grid: resampling can fill a short dropout from its nearer frame, leaving
#' no `NA` behind for this to find, and its equal time steps show no gap
#' either.
#'
#' A gap is one of these:
#'
#' * rows without f0 between two rows with f0;
#' * a time step longer than 1.5 times the token's usual step, so the gaps
#'   still show once the rows without f0 are removed. This needs a regular
#'   frame grid and a `time` column;
#' * a change in the `run` column (`voiced_run`), when the data have one.
#'
#' Once the rows without f0 are removed, `n_missing` can no longer count
#' them. So for a token with no rows without f0, the values the data already
#' hold in `n_missing` and `has_gap` are kept, as they may record rows removed
#' earlier. `has_gap` is also set to `TRUE` there when the token has a gap
#' (above). Other tokens get both columns afresh. On data cleaned beforehand
#' that hold neither column and no `run` column, on a grid that is not
#' regular, no gap can be seen: such a token gets `n_missing = 0` and
#' `has_gap = FALSE`.
#'
#' A token with no f0 at all gets `has_gap = FALSE`: there is no interior to
#' speak of.
#'
#' @param df Long-format f0 data frame.
#' @param token,f0,time Column names. Defaults `"token"`, `"f0"`, `"time"`.
#'   Without the `time` column, rows are taken in their order in `df` and
#'   only rows without f0 mark a gap.
#' @param run Name of a column that numbers the runs of f0 within each token,
#'   such as `voiced_run`. Used only when the column is present; `NULL`
#'   ignores it.
#' @return `df` with the `n_missing` and `has_gap` columns added. Columns of
#'   these names already in `df` are replaced, keeping their values for the
#'   tokens with no rows without f0 (see Details).
#' @seealso [resample_f0_equal()], [trim_to_voiced()]; [screen_f0_rows()] for
#'   the gaps counted per token without changing the data.
#' @export
flag_f0_gaps <- function(df, token = "token", f0 = "f0", time = "time",
                         run = "voiced_run") {
  missing_cols <- setdiff(c(token, f0), names(df))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (nrow(df) == 0) {
    df$n_missing <- integer(0); df$has_gap <- logical(0)
    return(df)
  }
  measured <- has_f0(df[[f0]])
  has_time <- !is.null(time) && time %in% names(df)
  tv   <- if (has_time) gaps_as_numeric(df[[time]]) else seq_len(nrow(df))
  rv   <- if (!is.null(run) && run %in% names(df)) df[[run]] else NULL
  runs <- voiced_run_ids(df[[token]], tv, measured, run = rv,
                         use_steps = has_time)
  key  <- match(df[[token]], unique(df[[token]]))
  nk   <- max(key)
  tok  <- factor(key, levels = seq_len(nk))

  n_miss <- tabulate(key[!measured], nbins = nk)
  n_runs <- vapply(split(runs, tok),
                   function(x) max(c(0L, x), na.rm = TRUE), integer(1),
                   USE.NAMES = FALSE)
  gap <- n_runs > 1L

  # Keep the data's own values for tokens with no rows without f0.
  clean <- n_miss == 0L
  if ("n_missing" %in% names(df)) {
    old <- suppressWarnings(as.integer(gaps_as_numeric(df$n_missing)))
    old <- vapply(split(old, tok),
                  function(x) if (all(is.na(x))) NA_integer_ else
                    max(x, na.rm = TRUE),
                  integer(1), USE.NAMES = FALSE)
    use_old <- clean & !is.na(old)
    n_miss[use_old] <- old[use_old]
  }
  if ("has_gap" %in% names(df)) {
    old <- vapply(split(gap_flag_values(df$has_gap), tok),
                  function(x) any(x %in% TRUE), logical(1),
                  USE.NAMES = FALSE)
    gap <- gap | (clean & old)
  }
  df$n_missing <- as.integer(n_miss[key])
  df$has_gap   <- gap[key]
  df
}

# A has_gap column as logical, whatever type it was read as.
gap_flag_values <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  as.logical(trimws(as.character(x)))
}
