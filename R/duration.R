# duration.R — per-unit f0 contour duration. Used by the F0 Analysis Start
# tab's "Add contour duration" step, and scriptable on its own.

#' Duration of each f0 contour
#'
#' @description
#' Adds the time from the first to the last frame with a measured f0 in each
#' unit: a whole token, or each labelled segment of a landmark set (e.g. each
#' syllable) within a token. This is the span the contour covers, not the
#' length of the unit's interval: unmeasured frames at either edge do not
#' count, and unmeasured frames inside the span (a gap) do not shorten it.
#'
#' @details
#' A frame counts as measured when its f0 is a number other than 0; `NA` and
#' 0 (which pitch trackers write for unvoiced frames) do not count. Negative
#' values do, so a normalised f0 column (semitones, z-scores) works as well as
#' Hz. A unit with a single measured frame gets 0, and a unit with none gets
#' `NA`.
#'
#' The first and last frames are anchored on a *run* of at least `min_run`
#' consecutive measured frames, the rule [trim_to_voiced()] uses for the
#' voiced region. A lone measured frame cut off from the contour by unmeasured
#' frames, such as a tracking artefact in the silence, then cannot stretch the
#' duration. If no run is that long, the first and last measured frames are
#' used. On native frames the default makes a whole token's duration equal to
#' its voiced span (`voiced_s` in the F0 Extraction summary). Input without
#' unmeasured rows (a `.PitchTier`, or an export with those rows dropped) has
#' no gaps to see, so there every measured frame counts.
#'
#' With `set`, frames are grouped by token and by the segment's `_start` /
#' `_end` boundaries. Only labelled segments are measured: frames in an
#' empty-label interval (silence, as written by [attach_landmarks()]) or
#' outside the tier get `NA`.
#'
#' The value is repeated on every row of its unit, so the column can serve as
#' a token- or segment-level predictor directly.
#'
#' @param df Long-format f0 data frame.
#' @param token,time,f0 Column names. Defaults `"token"`, `"time"`, `"f0"`.
#' @param set Landmark-set base name (e.g. `"syllable"`) to measure each
#'   segment of that set, from its `<set>_start` / `<set>_end` columns (see
#'   [attach_landmarks()]). `NULL` (default) measures whole tokens.
#' @param name Name of the new column. Default `"token_f0_dur"`, or
#'   `"<set>_f0_dur"` with `set`.
#' @param min_run Minimum number of consecutive measured frames the first and
#'   last frame must belong to. Default `2`; `1` takes the first and last
#'   measured frames as they are.
#' @return `df` with the duration column appended, in the units of the `time`
#'   column. Row order is preserved.
#' @seealso [trim_to_voiced()] for the voiced span as a row filter;
#'   [normalise_time_landmarks()] for a time axis within the same segments.
#' @examples
#' df <- data.frame(token = "a", time = seq(0, 0.5, by = 0.1),
#'                  f0 = c(NA, 200, 210, NA, 190, NA))
#' contour_duration(df)$token_f0_dur   # 0.4 - 0.1 = 0.3, on every row
#' @export
contour_duration <- function(df, token = "token", time = "time", f0 = "f0",
                             set = NULL, name = NULL, min_run = 2) {
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

  tv   <- suppressWarnings(as.numeric(df[[time]]))
  fv   <- suppressWarnings(as.numeric(df[[f0]]))
  unit <- as.character(df[[token]])
  if (!is.null(set)) {
    st <- suppressWarnings(as.numeric(df[[sc]]))
    en <- suppressWarnings(as.numeric(df[[ec]]))
    inside <- is.finite(st) & is.finite(en)
    # Labelled segments only. attach_landmarks() leaves <set>_i NA in an
    # empty-label interval; without an index column, read the label itself.
    ic <- paste0(set, "_i")
    if (ic %in% names(df)) {
      inside <- inside & !is.na(df[[ic]])
    } else if (set %in% names(df)) {
      lab <- as.character(df[[set]])
      inside <- inside & !is.na(lab) & nzchar(trimws(lab))
    }
    unit[!inside] <- NA_character_
    unit[inside]  <- paste(unit[inside], st[inside], en[inside], sep = "\r")
  }

  key    <- match(unit, unique(unit))
  key[is.na(unit)] <- NA_integer_
  use    <- !is.na(key) & !is.na(tv)
  voiced <- is.finite(fv) & fv != 0
  dur    <- rep(NA_real_, nrow(df))
  for (rows in split(which(use), key[use])) {
    ord <- rows[order(tv[rows])]
    v   <- voiced[ord]
    if (!any(v)) next                          # no measured f0: stays NA
    r      <- rle(v)
    ends   <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1L
    long   <- which(r$values & r$lengths >= min_run)
    if (length(long) == 0) long <- which(r$values)   # fall back to any
    first  <- starts[long[1]]
    last   <- ends[long[length(long)]]
    dur[rows] <- tv[ord[last]] - tv[ord[first]]
  }
  df[[name]] <- dur
  df
}
