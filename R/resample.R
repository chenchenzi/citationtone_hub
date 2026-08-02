# =============================================================================
# Equal-N resampling of long-format f0 contours.
#
# Turns each token's native pitch-frame grid (fixed time step, so long tokens
# have more frames than short ones) into a fixed number of equidistant points
# across the token's duration — the "measure f0 at every 5% / 10% of the
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
#' This rests on unvoiced frames being **present as `NA` (or 0 Hz) rows**,
#' which is how the wrassp and `.Pitch` paths deliver them. Sparse input
#' carries no such rows: a `.PitchTier`, or a CSV listing only voiced
#' samples, represents an unvoiced stretch as nothing more than a wide gap
#' between two voiced anchors, so both bracketing frames are voiced and the
#' point is interpolated straight across. Praat does the same on a PitchTier.
#' Add explicit `NA` rows if such stretches should read as unvoiced.
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
#' grid and are dropped — their names are recorded in the
#' `dropped_columns` attribute of the result.
#'
#' Rows with a missing time are dropped, and rows sharing a frame time
#' are collapsed to the first of them, so interpolation has strictly
#' increasing anchors. A token left with fewer than two distinct frame
#' times cannot be resampled and keeps its single remaining row, with
#' `time_prop = 0.5`.
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
#'   (proportional position 0-1 within the token), and `f0` columns, plus
#'   `intensity` (when present) and any carried token-constant columns.
#'   Input columns named `point` or `time_prop` are dropped, since those
#'   names are computed here. Rows are sorted by time within each token;
#'   tokens keep their order of first appearance. The attribute
#'   `dropped_columns` names any columns that could not be carried.
#' @seealso [normalise_time_token()] for adding a proportional time
#'   column without changing the sampling grid.
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
      ff[!f0_voiced(ff)] <- NA_real_          # 0 Hz is unvoiced, not a value
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

#' Trim each token to its voiced region
#'
#' @description
#' Keeps only the rows between a token's first and last voiced frame, so a
#' downstream measurement spans the syllable rather than the whole recording.
#' Frame-based trackers emit a frame every step across the entire file, with
#' `NA` (or 0 Hz) f0 in silence; without trimming, "21 equidistant points
#' across the token" spends part of the grid on leading and trailing silence.
#' Sparse input such as a `.PitchTier` has no silent rows to trim, so this is
#' a no-op there.
#'
#' @details
#' The edges are anchored on a *run* of at least `min_run` consecutive voiced
#' frames, so one stray voiced frame in silence (a common octave-error
#' artefact) cannot stretch the region. If no run is that long, the first and
#' last voiced frames are used. Tokens with no voiced frame at all keep no
#' rows — count them before and after if their loss should be reported.
#'
#' Frames *inside* the region are kept whether or not they are voiced, so an
#' internal unvoiced stretch (a medial voiceless stop, a creaky patch) stays
#' in place and still reads as `NA`.
#'
#' @param df Long-format f0 data frame.
#' @param token,time,f0 Column names. Defaults `"token"`, `"time"`, `"f0"`.
#' @param min_run Minimum number of consecutive voiced frames an edge anchor
#'   must belong to. Default `2`.
#' @return `df` with out-of-region rows removed, row order preserved.
#' @seealso [resample_f0_equal()], which is normally applied after this;
#'   [filter_interval_rows()] for a TextGrid-interval region instead.
#' @export
trim_to_voiced <- function(df, token = "token", time = "time", f0 = "f0",
                           min_run = 2) {
  missing_cols <- setdiff(c(token, time, f0), names(df))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  tv  <- suppressWarnings(as.numeric(df[[time]]))
  fv  <- suppressWarnings(as.numeric(df[[f0]]))
  fv[!f0_voiced(fv)] <- NA_real_             # 0 Hz is unvoiced, not a value
  key <- match(df[[token]], unique(df[[token]]))

  keep <- rep(FALSE, nrow(df))
  for (g in seq_len(max(key, 0L))) {
    idx <- which(key == g)
    ord <- idx[order(tv[idx])]
    voiced <- !is.na(fv[ord])
    if (!any(voiced)) next                     # no f0 at all: token drops out
    r <- rle(voiced)
    long <- which(r$values & r$lengths >= min_run)
    if (length(long) == 0) long <- which(r$values)   # fall back to any voiced
    ends   <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1L
    first  <- starts[long[1]]
    last   <- ends[long[length(long)]]
    keep[ord[first:last]] <- TRUE
  }
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
#' at least one of those rows sits *between* two rows that do have f0.
#'
#' @details
#' The distinction matters because a missing value at the edge of a region
#' usually just means the region ran slightly past the voicing, while a
#' missing value in the middle means voicing was interrupted (a medial
#' voiceless stop, a creaky patch, or a tracking failure). Only the latter
#' sets `has_gap`, so `has_gap` marks the tokens worth looking at in F0
#' Correction. Run this on the native frames rather than on a resampled
#' grid: resampling can fill a short dropout from its nearer frame, leaving
#' no `NA` behind for this to find.
#'
#' A token with no f0 at all gets `has_gap = FALSE`: there is no interior to
#' speak of. Existing columns of either name are replaced.
#'
#' @param df Long-format f0 data frame.
#' @param token,f0,time Column names. Defaults `"token"`, `"f0"`, `"time"`.
#' @return `df` with the `n_missing` and `has_gap` columns added.
#' @seealso [resample_f0_equal()], [trim_to_voiced()].
#' @export
flag_f0_gaps <- function(df, token = "token", f0 = "f0", time = "time") {
  missing_cols <- setdiff(c(token, f0), names(df))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (nrow(df) == 0) {
    df$n_missing <- integer(0); df$has_gap <- logical(0)
    return(df)
  }
  fv  <- suppressWarnings(as.numeric(df[[f0]]))
  fv[!f0_voiced(fv)] <- NA_real_             # 0 Hz is unvoiced, not a value
  key <- match(df[[token]], unique(df[[token]]))
  ord_within <- if (time %in% names(df)) suppressWarnings(as.numeric(df[[time]])) else seq_len(nrow(df))

  n_miss <- integer(nrow(df))
  gap    <- logical(nrow(df))
  for (g in seq_len(max(key, 0L))) {
    idx     <- which(key == g)
    ord     <- idx[order(ord_within[idx])]
    present <- !is.na(fv[ord])
    n_miss[idx] <- sum(!present)
    if (any(present)) {
      inner <- seq(which(present)[1], utils::tail(which(present), 1))
      gap[idx] <- any(!present[inner])
    }
  }
  df$n_missing <- n_miss
  df$has_gap   <- gap
  df
}
