# =============================================================================
# Shared per-token time-axis preparation for the modelling functions.
#
# fit_gca(), fit_gamm(), fit_polynomial(), and compute_mean_contour() all
# rescale time within each token before fitting. When the supplied time column
# is ALREADY proportional (e.g. token_t01 from normalise_time_token(), or the
# time_prop column written by equal-N resampling), re-applying the per-token
# min-max rescale stretches any token whose samples do not span the full unit
# interval. The helpers here detect that case so the fitters can use such a
# column as-is, with an explicit override in both directions.
#
# The detection is deliberately conservative: it only fires when the column
# looks proportional AS A WHOLE (see time_already_normalised()). A dataset in
# which EVERY token is a partial span — a vowel-only subset, say, where each
# token covers [0.3, 0.7] of its proportional axis — is indistinguishable from
# ordinary variable-duration time by any value-based test, so it is rescaled as
# before; pass time_normalised = "yes" to keep such an axis intact.
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

#' Does a time column look already normalised to \[0, 1\]?
#'
#' @description
#' Heuristic used by the modelling functions (via [resolve_time_norm()]) to
#' decide whether a time column is already proportional (per-token normalised
#' to the unit interval) and should be used as-is rather than min-max rescaled
#' within each token again.
#'
#' @details
#' Returns `TRUE` only when all three hold:
#'
#' 1. Every finite value lies in `[0, 1]` (within `eps`).
#' 2. The pooled values actually use the unit scale: pooled minimum
#'    at or below 0.05 and pooled maximum at or above 0.95.
#' 3. Tokens individually cover the interval: the median per-token span
#'    (`max - min`) is at least 0.9.
#'
#' Condition 3 is the load-bearing one: raw time in *seconds* for citation
#' tones (durations of, say, 0.2–0.9 s) can satisfy the first two, but its
#' per-token spans equal the (variable, well under 0.9) durations, so it is
#' correctly treated as unnormalised. Millisecond-scale time fails condition 1
#' outright, as do sequential landmark axes (`<tier>_tseq`, which run from 0 to
#' the number of segments).
#'
#' Two limits are worth knowing, both following from the fact that no
#' value-based test can separate these cases:
#'
#' * **Not detected**: a set in which *every* token is a partial span of the
#'   proportional axis (e.g. a vowel-only subset where each token covers
#'   `[0.3, 0.7]`) looks exactly like ordinary variable-duration time, so it
#'   is rescaled per token as before. Pass `time_normalised = "yes"` to
#'   [resolve_time_norm()] (or to the fitters) to keep such an axis intact.
#' * **Detected**: raw seconds in which every token happens to last just under
#'   one second, with a median duration of 0.9 s or more, satisfies all three
#'   conditions and is used as-is. Pass `time_normalised = "no"` to force the
#'   per-token rescale for such data.
#'
#' @param time_values Numeric vector of time values.
#' @param tokens Vector of token IDs, same length as `time_values`.
#' @param eps Numeric slop allowed beyond the `[0, 1]` bounds. Default `1e-8`.
#' @return `TRUE` if the column looks already normalised, else `FALSE`.
#' @seealso [resolve_time_norm()], which applies this decision.
#' @export
time_already_normalised <- function(time_values, tokens, eps = 1e-8) {
  tv  <- tryCatch(as_time_numeric(time_values), error = function(e) NULL)
  if (is.null(tv)) return(FALSE)
  fin <- is.finite(tv)
  if (!any(fin)) return(FALSE)
  t <- tv[fin]
  if (min(t) < -eps || max(t) > 1 + eps) return(FALSE)
  if (min(t) > 0.05 || max(t) < 0.95) return(FALSE)
  spans <- tapply(t, token_group_key(tokens)[fin], function(x) max(x) - min(x))
  isTRUE(stats::median(spans, na.rm = TRUE) >= 0.9)
}

#' Per-token normalised time, honouring an already-normalised column
#'
#' @description
#' Builds the `[0, 1]` time axis the modelling functions fit on. By default
#' (`time_normalised = "auto"`) the column is inspected with
#' [time_already_normalised()]: a column that is already proportional is used
#' as-is (clamped to `[0, 1]`), anything else is min-max rescaled to `[0, 1]`
#' within each token exactly as before.
#'
#' @details
#' In the rescaling path, a token whose time has zero range (a single sample,
#' or all-identical times) gets `0.5` for every row, and `NA` times inside an
#' otherwise valid token propagate as `NA` — matching the behaviour the
#' modelling functions have always had.
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
#' @return A list with `time_norm` (numeric vector, one value per row of
#'   `data`) and `prenormalised` (logical: was the column used as-is?).
#' @seealso [time_already_normalised()] for the detection rule.
#' @export
resolve_time_norm <- function(data, time, token,
                              time_normalised = c("auto", "no", "yes"),
                              quiet = FALSE) {
  time_normalised <- match.arg(time_normalised)
  tv <- as_time_numeric(data[[time]], time)
  tk <- token_group_key(data[[token]])

  pre <- switch(time_normalised,
                auto = time_already_normalised(tv, tk),
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
      message("Time column '", time, "' appears to be already normalised to ",
              "[0, 1]; using it as-is. Pass time_normalised = \"no\" to force ",
              "per-token rescaling.")
    }
    return(list(time_norm = pmin(pmax(tv, 0), 1), prenormalised = TRUE))
  }

  # Per-token min-max rescale (the historical behaviour). suppressWarnings:
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
