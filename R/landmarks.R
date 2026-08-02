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
    tg <- suppressWarnings(tryCatch(rPraat::tg.read(p), error = function(e) NULL))
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
    tg <- suppressWarnings(tryCatch(rPraat::tg.read(audio$tg_path[ai]), error = function(e) NULL))
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

#' Does an interval label denote a vowel (IPA)?
#'
#' @description
#' Vectorised heuristic for picking vowel intervals out of a segmental
#' TextGrid tier without knowing the language. A label counts as a vowel
#' when, after stripping length marks, stress marks, tone digits/letters,
#' superscripts, ties, spaces, and combining diacritics, every remaining
#' character is an IPA vowel letter — so long vowels (`aː`), nasalised
#' vowels (`ã`), and di-/triphthongs (`ai`, `iau`) all match, while
#' anything containing a consonant letter (`ang`, `pa`, `n`) does not.
#'
#' Di- and triphthongs need no special handling: the test applies to *every*
#' character of the label, so a label matches when all of its letters are
#' vowels -- `ai`, `au`, `iau`, `uai`, `ɔi`, `aːi` and `ai̯` all pass.
#'
#' Offglides written as consonant letters are accepted too, since many
#' traditions spell diphthongs that way: `j`, `w` and `ɥ` count inside a
#' nucleus that *also* contains a vowel letter, so `aj`, `aw`, `ja`, `jaw`
#' and `waj` match, while a bare `j` or `w` onset does not. A vowel mixed
#' with any other consonant (`ang`, `an`, `pa`) never matches.
#'
#' Syllabic nasals count as vowel-equivalent nuclei: a label whose letters are
#' all nasals and which carries a syllabicity mark (`m\u0329`, `n\u0329`, `\u014B\u0329`) matches,
#' since these are tone-bearing units in e.g. Cantonese (\u5514, \u4E94). The same
#' nasals *without* the mark (`m`, `n`, `\u014B`) do not.
#'
#' @details
#' Recognised base letters: the IPA vowel letters (`a e i o u y æ ɐ
#' ɑ ɒ ə ɘ ɵ ɛ œ ɜ ɞ ɤ
#' ɪ ɨ ɔ ø ʉ ʊ ʌ ɯ ʏ ɶ
#' ɚ ɝ` and the near-close central `ᵻ ᵿ`), their ASCII
#' upper-case counterparts `A E I O U Y`, `@` (schwa in SAMPA-style labels),
#' and precomposed accented Latin vowels, including the pinyin tone-marked
#' forms (`ã é ü ā á ǎ à ū ǔ ǖ ǘ ǚ ǜ` ...) in either Unicode normalisation.
#' Labels in other schemes (whole pinyin finals such as `ang`, X-SAMPA
#' consonant-bearing rhymes) should be selected with explicit labels instead
#' — see [filter_interval_rows()].
#'
#' @param x Character vector of interval labels (`NA` and empty labels
#'   return `FALSE`).
#' @return Logical vector, same length as `x`.
#' @seealso [filter_interval_rows()] for subsetting f0 frames by interval.
#' @export
ipa_vowel_label <- function(x) {
  x   <- as.character(x)
  out <- rep(FALSE, length(x))
  ok  <- !is.na(x)
  if (!any(ok)) return(out)
  s <- x[ok]
  # Syllabicity marks (combining vertical line below / above) must be spotted
  # BEFORE the diacritic strip below removes them: they are what makes a nasal
  # a nucleus rather than a consonant.
  syllabic <- grepl("[̩̍]", s)
  s <- gsub("\\p{Mn}", "", s, perl = TRUE)               # combining diacritics
  s <- gsub("[ːˑˈˌ˞]", "", s)   # length, stress, rhotic hook
  s <- gsub("[˥-˩‿⁀]", "", s)        # Chao tone letters, ties
  s <- gsub("[0-9¹²³⁰-⁹]", "", s)  # tone digits, superscripts
  s <- gsub("[[:space:]]+", "", s)
  vowel_class <- paste0(
    "aeiouyAEIOUY@",
    "æÆøØœŒɶ",              # æ Æ ø Ø œ Œ ɶ
    "ɐɑɒɔɘəɚ",     # ɐ ɑ ɒ ɔ ɘ ə ɚ
    "ɛɜɝɞ",                       # ɛ ɜ ɝ ɞ
    "ɤɨɪɯɵ",                 # ɤ ɨ ɪ ɯ ɵ
    "ʉʊʌʏᵻᵿ",           # ʉ ʊ ʌ ʏ ᵻ ᵿ
    "À-ÆÈ-ÏÒ-ÖØ-Ý",
    "à-æè-ïò-öø-ýÿ",
    # Latin Extended-A/B precomposed vowels, incl. the pinyin tone marks
    # (ā ǎ ē ě ī ǐ ō ǒ ū ǔ ǖ ǘ ǚ ǜ) that NFC-encoded TextGrids carry. The
    # \p{Mn} strip above only catches their NFD spellings.
    "Ā-ąĒ-ěĨ-ı",        # Ā-ą Ē-ě Ĩ-ı
    "Ō-őŨ-ųŶ-Ÿ",        # Ō-ő Ũ-ų Ŷ-Ÿ
    "Ǎ-ǜǞ-ǡǪ-ǭ",        # Ǎ-ǜ Ǟ-ǡ Ǫ-ǭ
    "Ǻ-ǿȲȳ"                       # Ǻ-ǿ Ȳ ȳ
  )
  # Offglides written as consonant letters (aj, aw, ja, jaw) are diphthongs in
  # many transcription traditions, so a glide counts INSIDE a nucleus -- but
  # only alongside a real vowel letter, so a bare "j"/"w" onset stays a
  # consonant.
  glide_class <- "jw\u0265JW"
  has_vowel   <- grepl(paste0("[", vowel_class, "]"), s, perl = TRUE)
  is_vowel <- nzchar(s) & has_vowel &
    !grepl(paste0("[^", vowel_class, glide_class, "]"), s, perl = TRUE)
  # A syllabic nasal is a nucleus: all-nasal letters plus a syllabicity mark.
  nasal_class <- "mn\u014B\u0271\u0272\u0273\u0274MN"
  is_nucleus  <- syllabic & nzchar(s) &
    !grepl(paste0("[^", nasal_class, "]"), s, perl = TRUE)
  out[ok] <- is_vowel | is_nucleus
  out
}

#' Keep only f0 frames that fall inside chosen TextGrid intervals
#'
#' @description
#' Subsets a long-format f0 data frame — with landmark columns already
#' attached by [attach_landmarks()] — to the rows whose time falls inside
#' selected intervals of one tier: automatically detected vowel intervals,
#' the rhyme (first vowel interval to the end of the token; monosyllabic
#' data only), or an explicit set of labels.
#'
#' @details
#' The three modes:
#'
#' * `"vowel"`: keep rows whose interval label passes
#'   [ipa_vowel_label()] — all vowel intervals, whatever the syllable
#'   count.
#' * `"rhyme"`: keep rows in labelled intervals from the *first* vowel
#'   interval of each token to the token's end (vowel + coda). This
#'   assumes each token is one syllable; for multisyllabic tokens it
#'   would span from the first vowel across every following syllable.
#'   Tokens with no vowel-labelled interval are dropped entirely.
#' * `"labels"`: keep rows whose (whitespace-trimmed) label equals one of
#'   `labels` — for tiers where the region of interest is marked
#'   explicitly (e.g. a `rhyme` tier, or non-IPA label schemes).
#'
#' Rows with an `NA` label — tokens with no matching TextGrid, or frames
#' outside the tier's span — are always excluded, as are frames in
#' empty-labelled (silence) intervals. Callers should tell users how many
#' tokens were lost that way rather than let them vanish silently.
#'
#' @param df Long-format data frame with `token` plus the tier's landmark
#'   columns (`<set>`, `<set>_i`, ...) from [attach_landmarks()].
#' @param set Landmark-set base name, i.e. the sanitised tier name used as
#'   the label column (e.g. `"vowel"`, `"segment"`).
#' @param mode One of `"vowel"`, `"rhyme"`, or `"labels"`. See Details.
#' @param labels Character vector of interval labels to keep (only for
#'   `mode = "labels"`).
#' @param token Name of the token-ID column (used by `mode = "rhyme"`).
#'   Default `"token"`.
#' @return The subset of `df`, row order preserved.
#' @seealso [attach_landmarks()] to add the landmark columns;
#'   [ipa_vowel_label()] for the vowel test.
#' @export
filter_interval_rows <- function(df, set,
                                 mode = c("vowel", "rhyme", "labels"),
                                 labels = NULL, token = "token") {
  mode <- match.arg(mode)
  if (is.null(df) || !set %in% names(df)) {
    stop("Landmark label column '", set, "' not found; attach the tier ",
         "with attach_landmarks() first.", call. = FALSE)
  }
  lab <- as.character(df[[set]])

  keep <- switch(mode,
    vowel = ipa_vowel_label(lab),
    labels = {
      wanted <- trimws(as.character(labels))
      wanted <- wanted[!is.na(wanted) & nzchar(wanted)]
      if (length(wanted) == 0) {
        stop("mode = \"labels\" needs a non-empty `labels` vector.",
             call. = FALSE)
      }
      !is.na(lab) & trimws(lab) %in% wanted
    },
    rhyme = {
      idx_col <- paste0(set, "_i")
      if (!idx_col %in% names(df)) {
        stop("Column '", idx_col, "' not found; attach the tier with ",
             "attach_landmarks() first.", call. = FALSE)
      }
      if (!token %in% names(df)) {
        stop("Token column '", token, "' not found.", call. = FALSE)
      }
      idx <- suppressWarnings(as.integer(df[[idx_col]]))
      tk  <- as.character(df[[token]])
      v_idx <- ifelse(ipa_vowel_label(lab), idx, NA_integer_)
      # First vowel-labelled interval per token; NA when the token has none.
      fv <- tapply(v_idx, tk, function(x) {
        if (all(is.na(x))) NA_integer_ else min(x, na.rm = TRUE)
      })
      fv_v <- as.integer(fv[tk])
      !is.na(idx) & !is.na(fv_v) & idx >= fv_v
    })

  out <- df[which(keep), , drop = FALSE]
  rownames(out) <- NULL
  out
}
