# duration.R - per-unit f0 contour duration. Used by the F0 Analysis Start
# tab's "Add contour duration" step, and scriptable on its own.

#' Duration of each f0 contour
#'
#' @description
#' Adds the time from the first to the last frame with a measured f0 in each
#' unit: a whole token, or each labelled segment of a landmark set (e.g. each
#' syllable) within a token. This is the span the contour covers, not the
#' length of the unit's interval: rows without f0 at either edge do not
#' count, and rows without f0 inside the span (a gap) do not shorten it.
#'
#' @details
#' A frame counts as measured when its f0 is a number other than 0; `NA` and
#' 0 (which pitch trackers write for unvoiced frames) do not count. Negative
#' values do, so a normalised f0 column (semitones, z-scores) works as well as
#' Hz. A unit with a single measured frame gets 0, and a unit with none gets
#' `NA`.
#'
#' The first and last frames are anchored on a *run* of at least `min_run`
#' measured frames in a row, the rule [trim_to_voiced()] uses for the voiced
#' region. A lone measured frame cut off from the contour by a gap, such as a
#' tracking artefact in the silence, then cannot stretch the duration. If no
#' run is that long, the first and last measured frames are used. On native
#' frames the default makes a whole token's duration equal to its voiced span
#' (`voiced_s` in the F0 Extraction summary).
#'
#' A run ends at a gap. A gap is one of these:
#'
#' * rows without f0 between two measured frames;
#' * a time step longer than 1.5 times the token's usual step. So data with
#'   the rows without f0 removed give the same durations as data with them
#'   kept. This needs a regular frame grid (see [screen_f0_rows()]), so it
#'   does not apply to sparse input such as a `.PitchTier`;
#' * a change in the `run` column (`voiced_run`, which the Start page adds
#'   when it removes rows without f0 or marks gaps), when the data have one.
#'   It keeps the gaps on any grid.
#'
#' Gaps are found over the whole token, from the real time column. With
#' `set`, each segment then counts only its own rows, so a run that crosses a
#' segment boundary is cut there. Frames are grouped by token and by the
#' segment's `_start` / `_end` boundaries, and rows with no token get `NA`.
#' Only labelled segments are measured: frames in an empty-label interval
#' (silence, as written by [attach_landmarks()]) or outside the tier get
#' `NA`.
#'
#' The value is repeated on every row of its unit, so the column can serve as
#' a token- or segment-level predictor directly.
#'
#' @param df Long-format f0 data frame.
#' @param token,time,f0 Column names. Defaults `"token"`, `"time"`, `"f0"`.
#'   `time` must be the real time column, not a 0-1 axis.
#' @param set Landmark-set base name (e.g. `"syllable"`) to measure each
#'   segment of that set, from its `<set>_start` / `<set>_end` columns (see
#'   [attach_landmarks()]). `NULL` (default) measures whole tokens.
#' @param name Name of the new column. Default `"token_f0_dur"`, or
#'   `"<set>_f0_dur"` with `set`.
#' @param min_run Minimum number of measured frames in a row that the first
#'   and last frame must belong to. Default `2`; `1` takes the first and last
#'   measured frames as they are.
#' @param run Name of a column that numbers the runs of f0 within each token,
#'   such as `voiced_run`. Used only when the column is present; `NULL`
#'   ignores it.
#' @return `df` with the duration column appended, in the units of the `time`
#'   column. Row order is preserved.
#' @seealso [trim_to_voiced()] for the voiced span as a row filter;
#'   [normalise_time_landmarks()] for a 0-1 time axis over the same segments,
#'   from their first f0 to their last, taken as they are (like
#'   `min_run = 1`, except that 0 in a signed or normalised column counts as
#'   f0 there but not here).
#' @examples
#' df <- data.frame(token = "a", time = seq(0, 0.6, by = 0.1),
#'                  f0 = c(NA, 200, 210, NA, 190, 195, NA))
#' contour_duration(df)$token_f0_dur   # 0.5 - 0.1 = 0.4, on every row
#'
#' # A lone frame at 0.025 s, cut off by a gap, does not count. Removing the
#' # rows without f0 gives the same duration: the time step shows the gap.
#' df <- data.frame(token = "b", time = seq(0.005, 0.105, by = 0.01),
#'                  f0 = c(NA, NA, 300, NA, NA, 200, 205, 210, 212, 215, NA))
#' contour_duration(df)$token_f0_dur[1]                   # 0.095 - 0.055
#' contour_duration(df[!is.na(df$f0), ])$token_f0_dur[1]  # the same
#' @export
contour_duration <- function(df, token = "token", time = "time", f0 = "f0",
                             set = NULL, name = NULL, min_run = 2,
                             run = "voiced_run") {
  need <- c(token, time, f0)
  if (!is.null(set)) {
    sc <- paste0(set, "_start"); ec <- paste0(set, "_end")
    need <- c(need, sc, ec)
  }
  missing_cols <- setdiff(need, names(df))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (is.null(name)) {
    name <- if (is.null(set)) "token_f0_dur" else paste0(set, "_f0_dur")
  }

  tv   <- gaps_as_numeric(df[[time]])
  fv   <- gaps_as_numeric(df[[f0]])
  # Units: whole tokens, or with `set` the labelled segments within each
  # token, keyed as normalise_time_landmarks() keys them (landmark_units() in
  # R/landmarks.R), so <set>_f0_dur and <set>_t01 describe the same segments.
  if (is.null(set)) {
    unit <- as.character(df[[token]])
    key  <- match(unit, unique(unit))
    key[is.na(unit)] <- NA_integer_
  } else {
    key <- landmark_units(df, token, set)
  }

  # Runs over the whole token (see voiced_run_ids() in R/resample.R); each
  # unit then counts only its own rows.
  measured <- is.finite(fv) & fv != 0
  rv   <- if (!is.null(run) && run %in% names(df)) df[[run]] else NULL
  runs <- voiced_run_ids(df[[token]], tv, measured, run = rv)
  span <- run_anchor_span(key, tv, runs, min_run)
  dur  <- span$hi[key] - span$lo[key]           # NA for a unit with no f0
  dur[is.na(tv)] <- NA_real_
  df[[name]] <- dur
  df
}
