# landmarks.R — attach Praat TextGrid interval boundaries to long-format f0
# data as landmark columns, and discover which interval tiers are available.
# Used by the F0 Extraction tab (to write landmark columns into the output) and,
# downstream, by the Visualise tab to align contours by those landmarks.

#' Interval-tier names across a set of TextGrids
#'
#' Reads up to `max_read` TextGrid files and returns the union of their
#' interval-tier names. Point tiers are ignored: landmarks come from interval
#' boundaries. Tiers are usually uniform across a corpus, so a small sample is
#' enough to populate a tier selector without reading thousands of files.
#'
#' @param tg_paths Character vector of `.TextGrid` paths (NA / "" entries skipped).
#' @param max_read Maximum number of files to read.
#' @return Sorted character vector of unique interval-tier names (possibly empty).
#' @export
tg_interval_tiers <- function(tg_paths, max_read = 12) {
  tg_paths <- tg_paths[!is.na(tg_paths) & nzchar(tg_paths)]
  if (length(tg_paths) == 0) return(character(0))
  tg_paths <- utils::head(unique(tg_paths), max_read)
  nms <- character(0)
  for (p in tg_paths) {
    tg <- tryCatch(rPraat::tg.read(p), error = function(e) NULL)
    if (is.null(tg)) next
    for (ti in tg) {
      if (!is.null(ti$type) && ti$type == "interval" && !is.null(ti$name))
        nms <- c(nms, ti$name)
    }
  }
  sort(unique(nms))
}

#' Assign each time to its interval in a TextGrid interval tier
#'
#' @param times Numeric vector of frame times (seconds).
#' @param t1,t2 Interval start/end times (from an rPraat interval tier).
#' @param labels Interval labels (same length as `t1`/`t2`).
#' @return A data.frame with one row per `times` value: `label`, `start`, `end`,
#'   and `idx` — the 1-based ordinal of the interval among non-empty-labelled
#'   intervals (NA inside empty/unlabelled intervals or outside the tier's span).
#' @export
assign_tier_landmarks <- function(times, t1, t2, labels) {
  times <- as.numeric(times)
  n <- length(t1)
  if (n == 0 || length(times) == 0) {
    return(data.frame(label = rep(NA_character_, length(times)),
                      start = rep(NA_real_, length(times)),
                      end   = rep(NA_real_, length(times)),
                      idx   = rep(NA_integer_, length(times)),
                      stringsAsFactors = FALSE))
  }
  k  <- findInterval(times, t1)          # 0 = before the first interval start
  k[k < 1] <- NA_integer_
  kk <- pmin(pmax(k, 1L), n)
  past <- !is.na(k) & times > t2[kk]     # beyond the last interval's end
  k[past] <- NA_integer_

  nonempty <- nzchar(trimws(labels))
  lab_idx  <- cumsum(nonempty)
  lab_idx[!nonempty] <- NA_integer_

  data.frame(
    label = ifelse(is.na(k), NA_character_, labels[k]),
    start = ifelse(is.na(k), NA_real_,      t1[k]),
    end   = ifelse(is.na(k), NA_real_,      t2[k]),
    idx   = ifelse(is.na(k), NA_integer_,   lab_idx[k]),
    stringsAsFactors = FALSE
  )
}

#' Attach TextGrid landmark columns to a long-format f0 data frame
#'
#' For each selected interval tier and each token, reads the token's TextGrid
#' and tags every f0 frame with the interval it falls in. Adds four columns per
#' tier, named from a sanitised tier name `p`: `p` (interval label), `p_start`,
#' `p_end` (interval boundaries, seconds), and `p_i` (1-based segment index
#' among labelled intervals — e.g. syllable 1, 2, 3 ...).
#'
#' @param df Long-format f0 data frame with `token` and `time` columns.
#' @param audio Data frame with `basename` and `tg_path` columns (fp_audio_data).
#' @param tier_names Character vector of interval-tier names to attach.
#' @param strip_ext Strip file extensions when matching `token` to `basename`.
#' @return `df` with the landmark columns appended. Returned unchanged when there
#'   are no tiers, no token/time columns, or no matching TextGrids.
#' @export
attach_landmarks <- function(df, audio, tier_names, strip_ext = TRUE) {
  if (is.null(df) || !nrow(df) || length(tier_names) == 0) return(df)
  if (!all(c("token", "time") %in% names(df))) return(df)
  if (is.null(audio) || !all(c("basename", "tg_path") %in% names(audio))) return(df)

  norm <- function(x) {
    x <- as.character(x)
    if (isTRUE(strip_ext)) x <- tools::file_path_sans_ext(x)
    tolower(trimws(x))
  }
  sani <- function(nm) {
    p <- tolower(gsub("[^A-Za-z0-9]+", "_", nm)); p <- gsub("^_+|_+$", "", p)
    if (!nzchar(p)) "tier" else p
  }

  audio_key <- norm(audio$basename)
  prefixes  <- make.unique(vapply(tier_names, sani, character(1)), sep = "_")

  out <- df
  for (p in prefixes) {
    out[[p]] <- NA_character_
    out[[paste0(p, "_start")]] <- NA_real_
    out[[paste0(p, "_end")]]   <- NA_real_
    out[[paste0(p, "_i")]]     <- NA_integer_
  }

  for (tok in unique(out$token)) {
    rows <- which(out$token == tok)
    ai <- match(norm(tok), audio_key)
    if (is.na(ai) || is.na(audio$tg_path[ai])) next
    tg <- tryCatch(rPraat::tg.read(audio$tg_path[ai]), error = function(e) NULL)
    if (is.null(tg)) next
    for (j in seq_along(tier_names)) {
      tn <- tier_names[j]; p <- prefixes[j]
      tier <- NULL
      for (ti in tg) {
        if (identical(ti$name, tn) && !is.null(ti$type) && ti$type == "interval") {
          tier <- ti; break
        }
      }
      if (is.null(tier)) next
      res <- assign_tier_landmarks(out$time[rows], tier$t1, tier$t2, tier$label)
      out[[p]][rows]                   <- res$label
      out[[paste0(p, "_start")]][rows] <- res$start
      out[[paste0(p, "_end")]][rows]   <- res$end
      out[[paste0(p, "_i")]][rows]     <- res$idx
    }
  }
  out
}

#' Landmark-normalised time columns
#'
#' Rescales a time column within each segment of a landmark set so multisyllabic
#' contours share a syllable-aware time axis. Adds two columns, named from the
#' set:
#'
#' * `<set>_t01` — time rescaled to 0-1 *within* each segment (segments overlap;
#'   good for comparing segment shapes against each other).
#' * `<set>_tseq` — *sequential* time, `(<set>_i - 1) + <set>_t01`, so segments
#'   lie end to end across the word (a word-level time axis that keeps order).
#'
#' @param df Data frame with the `time` column and the set's `_start` / `_end`
#'   (and optionally `_i`) columns, e.g. from [attach_landmarks()].
#' @param time Name of the raw time column (seconds, matching the landmark units).
#' @param set Landmark-set base name (e.g. "syllable").
#' @return `df` with the two columns appended. Returned unchanged when the
#'   required columns are absent.
#' @export
normalise_time_landmarks <- function(df, time, set) {
  sc <- paste0(set, "_start"); ec <- paste0(set, "_end"); ic <- paste0(set, "_i")
  if (is.null(df) || !all(c(time, sc, ec) %in% names(df))) return(df)
  tv <- suppressWarnings(as.numeric(df[[time]]))
  st <- suppressWarnings(as.numeric(df[[sc]]))
  en <- suppressWarnings(as.numeric(df[[ec]]))
  p  <- (tv - st) / (en - st)
  p[!is.finite(p)] <- NA_real_
  p  <- pmin(pmax(p, 0), 1)
  df[[paste0(set, "_t01")]]  <- p
  df[[paste0(set, "_tseq")]] <- if (ic %in% names(df))
    (suppressWarnings(as.integer(df[[ic]])) - 1) + p
  else p
  df
}

#' Whole-token 0-1 time normalisation
#'
#' Rescales each token's time to the interval 0-1, treating the whole token as a
#' single segment. No landmarks are needed, so it works for monosyllabic data,
#' or to put many tokens on one common 0-1 axis. Adds a single column,
#' `token_t01`.
#'
#' @param df Data frame with the `time` and `token` columns.
#' @param time Name of the raw time column (seconds).
#' @param token Name of the token-ID column (rows are grouped by it).
#' @return `df` with `token_t01` appended. Returned unchanged when the required
#'   columns are absent.
#' @export
normalise_time_token <- function(df, time, token) {
  if (is.null(df) || !all(c(time, token) %in% names(df))) return(df)
  tv <- suppressWarnings(as.numeric(df[[time]]))
  tk <- as.character(df[[token]])
  mn <- tapply(tv, tk, min, na.rm = TRUE)
  mx <- tapply(tv, tk, max, na.rm = TRUE)
  p  <- as.numeric((tv - mn[tk]) / (mx[tk] - mn[tk]))  # as.numeric drops tapply's dim/names
  p[!is.finite(p)] <- NA_real_
  df[["token_t01"]] <- pmin(pmax(p, 0), 1)
  df
}
