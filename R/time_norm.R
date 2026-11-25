# =============================================================================
# Shared per-token time-axis preparation for the modelling functions.
#
# fit_gca(), fit_gamm(), fit_polynomial() and compute_mean_contour() put time
# on [0, 1] within each token before fitting: from the token's first f0 (0)
# to its last f0 (1), so rows without f0 at its edges do not move the axis.
# When the time column is ALREADY proportional (token_t01 from
# normalise_time_token(), <tier>_t01 from normalise_time_landmarks(), or
# time_prop from equal-point resampling), rescaling it again would stretch
# any token whose samples do not span the unit interval. The helpers here
# detect that case so the fitters use such a column as it is, with an
# explicit override in both directions.
#
# A column named like a 0-1 axis (<tier>_t01, token_t01, time_prop) with
# every value in [0, 1] is recognised by its name. token_t01 and <tier>_t01
# run from the first f0 to the last and are NA elsewhere, so removing rows
# never changes them; the name matters for time_prop, for columns made by
# older versions, and for subsets taken after a column was made. For any
# other column the detection is deliberately conservative: it only fires
# when the column looks proportional AS A WHOLE (see
# time_already_normalised()). A dataset in
# which EVERY token is a partial span (a vowel-only subset, say, where each
# token covers [0.3, 0.7] of its proportional axis) is indistinguishable from
# ordinary variable-duration time by any value-based test. Unless its name
# marks it, it is rescaled as before; pass time_normalised = "yes" to keep such
# an axis intact.
# =============================================================================

# Group key for per-token operations. as.character()/tapply() would merge two
# numeric token IDs that print alike and silently drop NA-token rows; match()
# on the raw values compares exactly and gives NA its own group, matching the
# dplyr::group_by() semantics these helpers replaced.
token_group_key <- function(tokens) {
  match(tokens, unique(tokens))
}

# Coerce a time column to numeric the way the modelling functions need it.
# Factors go through as.character() so factor("0.15") means 0.15 rather than
# its level code, and a column that is not numeric at all is a loud error --
# the per-token min()/max() this replaced used to stop on such input.
as_time_numeric <- function(x, name = "time") {
  if (is.factor(x)) x <- as.character(x)
  tv <- suppressWarnings(as.numeric(x))
  if (!is.numeric(x) && any(!is.na(x)) && all(is.na(tv))) {
    stop("Column '", name, "' is not numeric and could not be read as time.",
         call. = FALSE)
  }
  tv
}

# Is `name` the name of a 0-1 time axis? <tier>_t01 (normalise_time_landmarks),
# token_t01 (normalise_time_token) and time_prop (resample_f0_equal) are. A
# <tier>_tseq or <tier>_tbound is not: it runs from 0 to the number of
# segments, and the already-normalised path would clamp it to [0, 1]
# ("auto") or stop ("yes").
t01_axis_name <- function(name) {
  is.character(name) && length(name) == 1 && !is.na(name) &&
    (grepl("_t01$", name) || name == "time_prop")
}

#' Does a time column look already normalised to \[0, 1\]?
#'
#' @description
#' Heuristic used by the modelling functions (via [resolve_time_norm()]) to
#' decide whether a time column is already proportional (per-token normalised
#' to the unit interval) and should be used as-is rather than rescaled within
#' each token again.
#'
#' @details
#' ## By name
#'
#' Three column names mark a 0-1 time axis: `<tier>_t01` from
#' [normalise_time_landmarks()], `token_t01` from [normalise_time_token()]
#' and `time_prop` from [resample_f0_equal()]. When `name` is one of these,
#' the column counts as already normalised as soon as every finite value lies
#' in `[0, 1]` (within `eps`). Its tokens do not need to cover the interval.
#' The first two run from the first f0 to the last and are `NA` on the other
#' rows, so removing rows never changes them. The name matters for
#' `time_prop`, which runs over the frames of the extracted region, for
#' columns made by older versions, which ran from the first row or interval
#' boundary to the last, and for a subset taken after a column was made.
#'
#' A sequential landmark axis (`<tier>_tseq` or `<tier>_tbound`) is not
#' recognised by its name. It runs from 0 to the number of segments, so it
#' goes through the value test below, as before.
#'
#' ## By value
#'
#' Any other column, or a call without `name`, counts as already normalised
#' only when all three hold:
#'
#' 1. Every finite value lies in `[0, 1]` (within `eps`).
#' 2. The pooled values actually use the unit scale: pooled minimum
#'    at or below 0.05 and pooled maximum at or above 0.95.
#' 3. Tokens individually cover the interval: the median per-token span
#'    (`max - min`) is at least 0.9. The span runs over every row with a
#'    finite time, rows without f0 included, so for a column without such a
#'    name, removing rows at a token's edges can change the decision. The
#'    app's Start page says so when removal would change it for its time
#'    column.
#'
#' Condition 3 is the load-bearing one: raw time in *seconds* for citation
#' tones (durations of, say, 0.2 to 0.9 s) can satisfy the first two, but its
#' per-token spans equal the (variable, well under 0.9) durations, so it is
#' correctly treated as unnormalised. Millisecond-scale time fails condition 1
#' outright, as do sequential landmark axes (`<tier>_tseq` and
#' `<tier>_tbound`, which run from 0 to the number of segments).
#'
#' Two limits of the value test are worth knowing, both following from the
#' fact that no value-based test can separate these cases:
#'
#' * **Not detected**: a set in which *every* token is a partial span of the
#'   proportional axis (e.g. a vowel-only subset where each token covers
#'   `[0.3, 0.7]`) looks exactly like ordinary variable-duration time, so it
#'   is rescaled per token as before, unless its name marks it (see "By
#'   name"). Pass `time_normalised = "yes"` to [resolve_time_norm()] (or to
#'   the fitters) to keep such an axis intact.
#' * **Detected**: raw seconds in which every token happens to last just under
#'   one second, with a median duration of 0.9 s or more, satisfies all three
#'   conditions and is used as-is. Pass `time_normalised = "no"` to force the
#'   per-token rescale for such data.
#'
#' @param time_values Numeric vector of time values.
#' @param tokens Vector of token IDs, same length as `time_values`.
#' @param eps Numeric slop allowed beyond the `[0, 1]` bounds. Default `1e-8`.
#' @param name The time column's name, or `NULL` (the default) to decide by
#'   value alone. See "By name" in Details.
#' @return `TRUE` if the column looks already normalised, else `FALSE`.
#' @seealso [resolve_time_norm()], which applies this decision.
#' @export
time_already_normalised <- function(time_values, tokens, eps = 1e-8,
                                    name = NULL) {
  tv  <- tryCatch(as_time_numeric(time_values), error = function(e) NULL)
  if (is.null(tv)) return(FALSE)
  fin <- is.finite(tv)
  if (!any(fin)) return(FALSE)
  t <- tv[fin]
  if (min(t) < -eps || max(t) > 1 + eps) return(FALSE)
  if (t01_axis_name(name)) return(TRUE)
  if (min(t) > 0.05 || max(t) < 0.95) return(FALSE)
  spans <- tapply(t, token_group_key(tokens)[fin], function(x) max(x) - min(x))
  isTRUE(stats::median(spans, na.rm = TRUE) >= 0.9)
}

#' Per-token normalised time, honouring an already-normalised column
#'
#' @description
#' Builds the `[0, 1]` time axis the modelling functions fit on. By default
#' (`time_normalised = "auto"`) the column and its name are passed to
#' [time_already_normalised()]: a column that is already proportional is used
#' as-is (clamped to `[0, 1]`); anything else is rescaled to `[0, 1]` within
#' each token, from its first f0 to its last when `f0` is given.
#'
#' @details
#' In the rescaling path with `f0`, each token runs from its first f0 (0) to
#' its last f0 (1), as in [normalise_time_token()]. Rows have f0 as there:
#' a row has f0 when its `f0` value is a number, and in a column in Hz (one
#' that [hz_f0_columns()] finds) 0 is no f0. A column with negative markers,
#' such as -1, is not read as Hz, so those values count as f0: set them to
#' `NA` first (see [suggest_unvoiced()]). Rows before the first f0 or after the
#' last get `NA`, so rows without f0 at a token's edges do not move the axis,
#' and removing them changes nothing. A token whose f0 is all at one time
#' gets `0.5` on the rows at that time, and a token with no f0 gets `NA`.
#' Rows with an `NA` token form one token of their own, as in the rescale
#' without `f0`. [fit_gca()], [fit_gamm()], [fit_polynomial()] and
#' [compute_mean_contour()] pass their f0 column.
#'
#' Without `f0`, each token runs from its first row with a time to its last,
#' rows without f0 included. A token whose times are all the same gets
#' `0.5` on every row, rows with no time included. Other `NA` times stay
#' `NA`. On data with no rows without f0 at the token edges, the two
#' rescales are the same.
#'
#' In `"auto"` mode, a `<tier>_t01`, `token_t01` or `time_prop` column whose
#' values all lie in `[0, 1]` is used as it is, whatever rows it has left, so
#' removing rows does not switch it to the rescale. Columns made by
#' [normalise_time_token()] and [normalise_time_landmarks()] already run
#' from the first f0 to the last. On `time_prop`, a column made by an older
#' version or a subset taken after a column was made, the f0 may not reach
#' 0 or 1. A `<tier>_tseq` or `<tier>_tbound` column is judged by its
#' values only, so one that spans several segments is rescaled per token
#' here; for tokens with the same number of segments the boundaries then
#' sit at 1/n, 2/n and so on. A token whose first or last segment has no
#' values on the axis is stretched: its other segments fill 0 to 1. On a
#' `<tier>_tseq` that is a segment with f0 at fewer than two times; on a
#' `<tier>_tbound`, one with no f0. ([fit_gamm()] keeps a `<tier>_tseq` or
#' `<tier>_tbound` on its own scale instead.)
#'
#' @param data A data frame containing the `time` and `token` columns.
#' @param time Name of the time column.
#' @param token Name of the token-ID column.
#' @param time_normalised One of `"auto"` (default; detect and use an
#'   already-normalised column as-is), `"no"` (always rescale per token), or
#'   `"yes"` (declare the column already normalised to `[0, 1]`; values outside
#'   that interval are an error).
#' @param quiet Suppress the message emitted when auto-detection decides the
#'   column is already normalised. Default `FALSE`.
#' @param f0 Name of the f0 column, or `NULL` (the default). With `f0`, the
#'   rescale runs from each token's first f0 to its last; without it, from
#'   each token's first row to its last.
#' @return A list with `time_norm` (numeric vector, one value per row of
#'   `data`) and `prenormalised` (logical: was the column used as-is?). A
#'   missing `time`, `token` or `f0` column is an error.
#' @seealso [time_already_normalised()] for the detection rule.
#' @export
resolve_time_norm <- function(data, time, token,
                              time_normalised = c("auto", "no", "yes"),
                              quiet = FALSE, f0 = NULL) {
  time_normalised <- match.arg(time_normalised)
  need_columns(data, c(time, token, f0))
  tv <- as_time_numeric(data[[time]], time)
  tk <- token_group_key(data[[token]])

  # The name decides for a 0-1 axis such as token_t01, so removing rows
  # cannot flip it; a <tier>_tseq or <tier>_tbound is judged by its values
  # only.
  pre <- switch(time_normalised,
                auto = time_already_normalised(tv, tk, name = time),
                yes  = TRUE,
                no   = FALSE)

  if (pre) {
    # Infinite values are outside [0, 1] just as surely as 1.4 is, and the
    # clamp below would quietly turn them into an endpoint.
    if (time_normalised == "yes" &&
        (any(is.infinite(tv)) ||
         (any(is.finite(tv)) &&
          (min(tv[is.finite(tv)]) < -1e-8 || max(tv[is.finite(tv)]) > 1 + 1e-8)))) {
      stop("time_normalised = \"yes\", but '", time,
           "' has values outside [0, 1]. Pass time_normalised = \"no\" to ",
           "rescale per token instead.", call. = FALSE)
    }
    if (time_normalised == "auto" && !isTRUE(quiet)) {
      if (t01_axis_name(time)) {
        message("Time column '", time, "' is a 0-1 time axis by its name, so ",
                "it is already normalised; using it as-is. Pass ",
                "time_normalised = \"no\" to force per-token rescaling.")
      } else {
        message("Time column '", time, "' appears to be already normalised to ",
                "[0, 1]; using it as-is. Pass time_normalised = \"no\" to force ",
                "per-token rescaling.")
      }
    }
    return(list(time_norm = pmin(pmax(tv, 0), 1), prenormalised = TRUE))
  }

  if (!is.null(f0)) {
    # From each token's first f0 (0) to its last (1), as normalise_time_token()
    # does, so rows without f0 at a token's edges do not move the axis.
    s  <- f0_span(tv, tk, f0_rows(data, f0))
    tn <- (tv - s$lo) / (s$hi - s$lo)
    degen <- is.finite(s$lo) & s$hi == s$lo
    tn[degen] <- ifelse(tv[degen] == s$lo[degen], 0.5, NA_real_)
    tn[!is.finite(tn) | tn < 0 | tn > 1] <- NA_real_
    return(list(time_norm = tn, prenormalised = FALSE))
  }

  # Per-token rescale from the first row to the last (no f0 given). suppressWarnings:
  # min/max over an all-NA token warn and return +/-Inf; the arithmetic below
  # then yields NA for those rows, as the old dplyr block did. Indexing by
  # position (not by name) keeps NA-token rows in their own group.
  # Explicit levels so group i sits at position i: tapply() would otherwise
  # order the integer keys as strings ("10" before "2") and misalign the lookup.
  grp  <- factor(tk, levels = seq_len(max(tk)))
  mn   <- tapply(tv, grp, function(x) suppressWarnings(min(x, na.rm = TRUE)))
  mx   <- tapply(tv, grp, function(x) suppressWarnings(max(x, na.rm = TRUE)))
  mn_v <- as.numeric(mn)[tk]
  mx_v <- as.numeric(mx)[tk]
  degen <- is.na(mn_v) | (is.finite(mn_v) & mx_v == mn_v)
  tn <- ifelse(degen, 0.5, (tv - mn_v) / (mx_v - mn_v))
  list(time_norm = tn, prenormalised = FALSE)
}

# Linear interpolation that never averages two segments at one time. With
# the segments in sequence (<tier>_tseq) one segment's last f0 and the next
# segment's first f0 share a whole number, often with an f0 jump between
# them (on a <tier>_tbound, only rows exactly on a boundary can share one);
# approx(ties = mean) would put a blend of the two, which neither segment
# has, at that point. `seg` gives each row's segment (stack_segments()).
# Where the rows at one time belong to different segments, the curve steps
# there: exactly at that time, and to its right, it starts from the rows of
# the LATER segment (the next segment's first f0); to its left it ends at
# the rows of the EARLIER one. So the row order does not matter. Rows at
# one time in the same segment, and every tie when `seg` is NULL, are
# averaged, as approx(ties = mean) does: two recordings that share a token
# name, say. Without tied times the result is approx()'s, bit for bit.
# `rule` is as in approx(): 1 gives NA outside [min(t), max(t)], 2 holds
# the end values. Rows with a non-finite t or y are skipped. Used by
# compute_mean_contour() and .interp_contour().
interp_no_blend <- function(t, y, xout, rule = 1, seg = NULL) {
  ok <- is.finite(t) & is.finite(y)
  # Segment codes 1, 2, ... in segment order; NA is a segment of its own.
  sg <- if (is.null(seg)) rep(1L, length(t)) else
          match(seg, sort(unique(seg), na.last = TRUE))
  t <- t[ok]; y <- y[ok]; sg <- sg[ok]
  out <- rep(NA_real_, length(xout))
  if (length(t) == 0L) return(out)
  o <- order(t, sg)
  t <- t[o]; y <- y[o]; sg <- sg[o]
  # One value per (time, segment): the mean of its rows.
  n   <- length(t)
  grp <- cumsum(c(TRUE, t[-1L] != t[-n] | sg[-1L] != sg[-n]))
  gy  <- if (grp[n] == n) y else as.numeric(rowsum(y, grp)) / tabulate(grp)
  gt  <- t[!duplicated(grp)]
  ux    <- gt[!duplicated(gt)]
  first <- gy[!duplicated(gt)]                    # earliest segment at each time
  last  <- gy[!duplicated(gt, fromLast = TRUE)]   # latest segment at each time
  nu    <- length(ux)
  if (nu == 1L) {
    out[!is.na(xout) & xout == ux] <- last
    if (rule == 2) {
      out[!is.na(xout) & xout < ux] <- first
      out[!is.na(xout) & xout > ux] <- last
    }
    return(out)
  }
  # At each time and to its right, the latest segment; then, where a time
  # holds two segments that differ, the stretch just before it ends at the
  # earliest.
  out <- stats::approx(ux, last, xout = xout, rule = rule, ties = "ordered")$y
  for (k in which(first != last)) {
    if (k == 1L) {
      if (rule == 2) out[!is.na(xout) & xout < ux[1L]] <- first[1L]
      next
    }
    w <- which(xout > ux[k - 1L] & xout < ux[k])
    if (length(w)) {
      out[w] <- stats::approx(ux[c(k - 1L, k)], c(last[k - 1L], first[k]),
                              xout = xout[w])$y
    }
  }
  out
}

# Each row's segment on an axis with the segments in sequence, for
# interp_no_blend(): 0 for the first segment, 1 for the second, and so on.
# Only for a time column named
# <set>_tseq or <set>_tbound; NULL for any other, so ties there are
# averaged. On a <set>_tseq, read as <set>_tseq - <set>_t01 (the tseq is
# (i - 1) + t01), else as <set>_i - 1; on a <set>_tbound, as <set>_i - 1.
# So the rows at a boundary are told apart in any row order. Rows that
# have neither fall back on the axis itself: floor() off a whole number,
# and at a whole number inside the token's range that more than one row
# holds, the last of those rows in the data starts the next segment, so
# there the rows must be in time order.
stack_segments <- function(data, time, token) {
  if (!is.character(time) || length(time) != 1L || is.na(time) ||
      !grepl("_(tseq|tbound)$", time) || !time %in% names(data)) return(NULL)
  set <- sub("_(tseq|tbound)$", "", time)
  tq  <- suppressWarnings(as_time_numeric(data[[time]], time))
  num <- function(col) {
    if (!col %in% names(data)) return(rep(NA_real_, nrow(data)))
    x <- data[[col]]
    suppressWarnings(as.numeric(if (is.factor(x)) as.character(x) else x))
  }
  # tbound - t01 is not a whole number: the gap at a boundary is kept.
  s <- if (grepl("_tseq$", time)) round(tq - num(paste0(set, "_t01")))
       else rep(NA_real_, length(tq))
  s[!is.finite(s)] <- (num(paste0(set, "_i")) - 1)[!is.finite(s)]
  s[!is.finite(tq)] <- NA_real_
  miss <- which(is.finite(tq) & !is.finite(s))
  if (length(miss)) {
    s[miss] <- floor(tq[miss])
    key  <- token_group_key(data[[token]])
    lo   <- stats::ave(tq, key, FUN = function(x) suppressWarnings(min(x, na.rm = TRUE)))
    hi   <- stats::ave(tq, key, FUN = function(x) suppressWarnings(max(x, na.rm = TRUE)))
    w    <- miss[tq[miss] == round(tq[miss]) & tq[miss] > lo[miss] & tq[miss] < hi[miss]]
    held <- paste(key[w], tq[w])
    end  <- duplicated(held, fromLast = TRUE)   # not the last row at that time
    s[w[end]] <- tq[w[end]] - 1
  }
  s
}

# The tokens in which two or more rows share a time in one segment, so that
# interp_no_blend() averages their f0 there, with a message. `key` holds the
# token of each row and `label` its name.
note_averaged_ties <- function(key, t, y, seg = NULL, label = key) {
  ok <- is.finite(t) & is.finite(y)
  n  <- sum(ok)
  if (n < 2L) return(invisible(NULL))
  k  <- token_group_key(key[ok]); tt <- t[ok]
  sg <- if (is.null(seg)) rep(0, n) else seg[ok]
  sg[is.na(sg)] <- -1
  o  <- order(k, tt, sg)
  k <- k[o]; tt <- tt[o]; sg <- sg[o]
  dup <- c(FALSE, k[-1L] == k[-n] & tt[-1L] == tt[-n] & sg[-1L] == sg[-n])
  if (!any(dup)) return(invisible(NULL))
  tok <- unique(as.character(label[ok][o][dup]))
  shown <- paste(utils::head(tok, 5), collapse = ", ")
  if (length(tok) > 5) shown <- paste0(shown, ", ...")
  message("In ", length(tok), " token(s) (", shown, "), two or more rows ",
          "have the same time, so their f0 values there are averaged. Two ",
          "recordings may share a token name, or the time axis may start ",
          "again within a token, as <tier>_t01 does at each syllable of a word.")
  invisible(tok)
}

# Per tone, the first and last time on a fit's axis over the rows the model
# fits: tone, t_first, t_last, with tones in the order of the factor's
# levels (unused levels left out). On a rescaled axis every tone runs from 0
# to 1 (0 to n on a <tier>_tseq or <tier>_tbound); on a time column used
# as it is, such as time_prop with no f0 at an end point, a tone can stop
# short, and its predicted curve beyond that point is extrapolated. fit_gca() and
# fit_gamm() store it, and predict_gca() and predict_gamm() carry it as an
# attribute, so the Summarise tab can mark such tones.
tone_time_coverage <- function(t, tone) {
  ok <- is.finite(t) & !is.na(tone)
  tone <- if (is.factor(tone)) droplevels(tone[ok]) else factor(tone[ok])
  t <- t[ok]
  if (!length(t)) {
    return(data.frame(tone = character(0), t_first = numeric(0),
                      t_last = numeric(0), stringsAsFactors = FALSE))
  }
  data.frame(tone = levels(tone),
             t_first = as.numeric(tapply(t, tone, min)),
             t_last  = as.numeric(tapply(t, tone, max)),
             stringsAsFactors = FALSE)
}
