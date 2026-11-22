# landmarks.R — attach Praat TextGrid interval boundaries to long-format f0
# data as landmark columns, and discover which interval tiers are available.
# Used by the F0 Extraction tab (to write landmark columns into the output) and,
# downstream, by the Visualise tab to align contours by those landmarks.
# Every TextGrid is read through read_textgrid(), which also reads Praat's
# UTF-16 files and says why a file could not be read.

#' Read a Praat TextGrid in any text encoding
#'
#' @description
#' Reads a TextGrid saved as a Praat text file (long or short format) and
#' parses it with [rPraat::tg.read()]. Praat saves a TextGrid as UTF-16
#' whenever a label or tier name is not plain ASCII (Chinese characters or
#' IPA, for example), and `rPraat::tg.read()` cannot read those files on its
#' own. Here the file is converted to UTF-8 first, so UTF-16 (with or without
#' a byte-order mark), UTF-8 and older 8-bit encodings all read the same way.
#'
#' @details
#' A file with no byte-order mark that is not valid UTF-8 was saved in an
#' 8-bit code page, which has to be guessed, as for [read_csv_utf8()]. Praat's
#' own 8-bit files (its "try ISO Latin-1" preference) read as Windows-1252,
#' and files with mostly Chinese text as GBK or Big5. The result then has a
#' `note` that says so. A UTF-8 file with a few bytes that are not UTF-8
#' stays UTF-8 instead, as in [read_csv_utf8()]: those bytes are shown as
#' U+FFFD (the replacement character), and `note` and `stray_lines` say where.
#'
#' Praat writes a double quote inside a label or tier name as two (`""`);
#' these are turned back into one, as Praat shows them. Labels are otherwise
#' kept exactly as written: no Unicode normalisation is applied.
#'
#' Praat's binary and chronological TextGrid formats cannot be read. Open such
#' a file in Praat and use Save as text file.
#'
#' @param path Path to a `.TextGrid` file.
#' @param name File name to use in the note. Defaults to the file's own name;
#'   give the original name when `path` is a temporary copy, as with a Shiny
#'   upload.
#' @return A list with five elements:
#'   * `tg`: the TextGrid as returned by [rPraat::tg.read()] (a named list of
#'     tiers), or `NULL` when the file could not be read.
#'   * `encoding`: the encoding the file was read as, such as `"UTF-16BE"`,
#'     `"UTF-8"` or a guessed code page (`"CP1252"`, `"GB18030"`, `"BIG5"`);
#'     `NA` when the file could not be decoded.
#'   * `reason`: `NULL` on success; otherwise a short phrase saying why the
#'     file could not be read, such as "it is in Praat's binary format; in
#'     Praat, use Save as text file".
#'   * `note`: `NULL`, or a sentence for the user when the code page was
#'     guessed or bytes that are not UTF-8 were replaced.
#'   * `stray_lines`: `NULL`, or the numbers of the lines that held bytes
#'     that are not UTF-8 in a file otherwise read as UTF-8.
#' @examples
#' # A short-format TextGrid with a Chinese label, saved as UTF-16BE with a
#' # byte-order mark, as Praat does by default
#' txt <- paste('File type = "ooTextFile"', 'Object class = "TextGrid"', "",
#'              "0", "0.5", "<exists>", "1", '"IntervalTier"', '"syllable"',
#'              "0", "0.5", "1", "0", "0.5",
#'              paste0('"', intToUtf8(0x5988), '"'), sep = "\n")
#' f <- tempfile(fileext = ".TextGrid")
#' writeBin(c(as.raw(c(0xFE, 0xFF)),
#'            iconv(txt, "UTF-8", "UTF-16BE", toRaw = TRUE)[[1]]), f)
#' if (requireNamespace("rPraat", quietly = TRUE)) {
#'   r <- read_textgrid(f)
#'   r$encoding
#'   r$tg$syllable$label
#' }
#' @export
read_textgrid <- function(path, name = basename(path)) {
  res <- list(tg = NULL, encoding = NA_character_, reason = NULL, note = NULL,
              stray_lines = NULL)
  if (!requireNamespace("rPraat", quietly = TRUE)) {
    res$reason <- "the rPraat package is not installed"
    return(res)
  }
  txt <- tryCatch(tg_decode_text(path), error = function(e)
    structure(NA_character_, reason = sprintf("it could not be opened (%s)", conditionMessage(e))))
  if (is.na(txt)) {
    res$reason <- attr(txt, "reason")
    return(res)
  }
  res$encoding <- attr(txt, "encoding")
  if (is_legacy_encoding(res$encoding)) {
    res$note <- sprintf(paste("'%s' is not saved as UTF-8 or UTF-16, so it was read as %s.",
                              "If a label looks wrong, save it as UTF-8 and upload it again."),
                        name, legacy_label(res$encoding))
  } else if (length(attr(txt, "stray"))) {
    res$stray_lines <- attr(txt, "stray")
    res$note <- stray_note(name, res$stray_lines)
  }
  read <- function(f) suppressWarnings(tryCatch(rPraat::tg.read(f, encoding = "UTF-8"),
                                                error = function(e) e))
  if (isTRUE(attr(txt, "direct"))) {
    tg <- read(path)                      # UTF-8 as rPraat expects it: no copy
  } else {
    # rPraat reads a UTF-8 file directly only under the current header; the
    # old short header ("ooTextFile short") has the same layout.
    txt <- gsub("\r\n?", "\n", txt)
    txt <- sub('^File type = "ooTextFile[^\n]*', 'File type = "ooTextFile"', txt)
    tmp <- tempfile(fileext = ".TextGrid")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(charToRaw(enc2utf8(txt)), tmp)
    tg <- read(tmp)
  }
  if (inherits(tg, "error")) {
    res$reason <- sprintf("its text could not be parsed (%s)",
                          sub("[.[:space:]]+$", "", conditionMessage(tg)))
    return(res)
  }
  unquote <- function(x) if (is.character(x)) gsub('""', '"', x, fixed = TRUE) else x
  for (k in seq_along(tg)) {
    tg[[k]]$name  <- unquote(tg[[k]]$name)
    tg[[k]]$label <- unquote(tg[[k]]$label)
  }
  names(tg) <- vapply(tg, function(t) if (is.character(t$name)) t$name else "", "")
  class(tg)["name"] <- basename(path)
  res$tg <- tg
  res
}

# A TextGrid file's text as one UTF-8 string, with attribute "encoding", and
# "direct" = TRUE when the file itself is UTF-8 with the header rPraat
# expects, so rPraat can read it as is. On failure, NA with attribute
# "reason" (a short phrase for the user). The file is read once; the
# encoding comes from sniff_text_encoding() (R/read_text.R). UTF-8 text that
# is not valid UTF-8 goes through resolve_non_utf8(): a few stray bytes are
# shown as U+FFFD, and attribute "stray" gives their line numbers; otherwise,
# with no BOM, it is read in the code page guess_legacy_encoding() picks.
# Line ends are left as they are.
tg_decode_text <- function(path) {
  fail <- function(reason) structure(NA_character_, reason = reason)
  info <- if (length(path) == 1L && !is.na(path) && nzchar(path))
    file.info(path, extra_cols = FALSE)
  if (is.null(info) || is.na(info$size) || isTRUE(info$isdir))
    return(fail("the file was not found"))
  bytes <- readBin(path, "raw", info$size)
  save_text <- "in Praat, use Save as text file"
  if (length(bytes) >= 12L && identical(bytes[1:12], charToRaw("ooBinaryFile")))
    return(fail(paste("it is in Praat's binary format;", save_text)))
  enc <- sniff_text_encoding(bytes)
  if (enc$kind == "empty") return(fail("the file is empty"))
  not_text <- "it is not a text file"
  if (enc$kind %in% c("zip", "ole", "binary")) return(fail(not_text))
  used  <- enc$encoding
  nul   <- FALSE
  stray <- NULL
  if (used == "UTF-8") {                  # no BOM, or a UTF-8 one
    if (enc$skip > 0L) bytes <- bytes[-seq_len(enc$skip)]
    nul <- any(bytes == as.raw(0))        # padding, possibly past the sniff
    if (nul) bytes <- bytes[bytes != as.raw(0)]
    txt <- rawToChar(bytes)
    if (!validUTF8(txt)) {
      # A few stray bytes in UTF-8 text, or an 8-bit code page (R/read_text.R).
      how <- resolve_non_utf8(bytes)
      if (enc$kind == "bom" && how$encoding != "UTF-8")
        return(fail("its text is not valid UTF-8"))
      used  <- how$encoding
      stray <- how$lines
      txt   <- if (used == "UTF-8") rawToChar(how$bytes) else iconv(txt, used, "UTF-8")
    }
  } else {
    unit <- if (startsWith(used, "UTF-32")) 4L else 2L    # UTF-16 or UTF-32
    keep <- (length(bytes) - enc$skip) %/% unit * unit   # drop a stray last byte
    txt  <- tryCatch(iconv(list(bytes[enc$skip + seq_len(keep)]), used, "UTF-8"),
                     error = function(e) NA_character_)
    if (is.na(txt)) return(fail(sprintf("its text is not valid %s", used)))
  }
  if (is.na(txt)) return(fail(not_text))
  Encoding(txt) <- "UTF-8"
  top  <- substr(txt, 1L, 300L)
  head <- strsplit(top, "\r\n|\n|\r")[[1]]
  if (length(head) && grepl("chronological", head[1], fixed = TRUE))
    return(fail(paste("it is in Praat's chronological format;", save_text)))
  if (length(head) < 2L || !grepl('^File type = "ooTextFile', head[1]) ||
      !grepl('"TextGrid"', head[2], fixed = TRUE))
    return(fail("it is not a Praat TextGrid text file"))
  direct <- enc$kind == "plain" && used == "UTF-8" && !nul && is.null(stray) &&
    head[1] == 'File type = "ooTextFile"' && !grepl("\r(?!\n)", top, perl = TRUE)
  structure(txt, encoding = used, direct = direct, stray = stray)
}

#' Interval-tier names across a set of TextGrids
#'
#' Reads up to `max_read` TextGrid files and returns the union of their
#' interval-tier names. Point tiers are ignored: landmarks come from interval
#' boundaries. Tiers are usually uniform across a corpus, so a small sample is
#' enough to populate a tier selector without reading thousands of files.
#'
#' Files are read with [read_textgrid()], so UTF-16 TextGrids work too.
#'
#' @param tg_paths Character vector of `.TextGrid` paths (NA / "" entries skipped).
#' @param max_read Maximum number of files to read.
#' @return Sorted character vector of unique interval-tier names (possibly
#'   empty). When some of the files read could not be parsed, attribute
#'   `"unreadable"` gives the reason for each, named by path (see
#'   [read_textgrid()]). When some were read in a guessed 8-bit code page,
#'   attribute `"guessed"` names that code page for the user (such as
#'   "Western European (Windows-1252)"), named by path. When some were read
#'   as UTF-8 with bytes that are not UTF-8 replaced, attribute `"stray"`
#'   gives those lines (such as "line 12"), named by path.
#' @export
tg_interval_tiers <- function(tg_paths, max_read = 12) {
  tg_paths <- tg_paths[!is.na(tg_paths) & nzchar(tg_paths)]
  if (length(tg_paths) == 0) return(character(0))
  tg_paths <- utils::head(unique(tg_paths), max_read)
  nms <- character(0)
  bad <- character(0)
  guessed <- character(0)
  stray <- character(0)
  for (p in tg_paths) {
    r <- read_textgrid(p)
    if (is.null(r$tg)) {
      bad[p] <- r$reason
      next
    }
    if (is_legacy_encoding(r$encoding)) guessed[p] <- legacy_label(r$encoding)
    if (length(r$stray_lines)) stray[p] <- line_numbers_text(r$stray_lines)
    for (ti in r$tg) {
      if (!is.null(ti$type) && ti$type == "interval" && !is.null(ti$name))
        nms <- c(nms, ti$name)
    }
  }
  out <- sort(unique(nms))
  if (length(bad)) attr(out, "unreadable") <- bad
  if (length(guessed)) attr(out, "guessed") <- guessed
  if (length(stray)) attr(out, "stray") <- stray
  out
}

#' Tier names across a set of TextGrids, read quickly
#'
#' Scans each file's text for tier headers instead of parsing it with
#' `rPraat::tg.read()`, which takes tens of milliseconds per file, so it is fast
#' enough to run over every TextGrid in a corpus. Reads Praat's long and short
#' text formats in the same encodings as [read_textgrid()], UTF-16 included.
#' Files that cannot be read, or are in Praat's binary format, contribute no
#' names.
#'
#' @param tg_paths Character vector of `.TextGrid` paths (NA / "" entries skipped).
#' @return Character vector of unique tier names (interval and point tiers), in
#'   the order first seen.
#' @export
tg_tier_names <- function(tg_paths) {
  tg_paths <- unique(tg_paths[!is.na(tg_paths) & nzchar(tg_paths)])
  nms <- character(0)
  for (p in tg_paths) nms <- c(nms, tg_tier_names_one(p))
  unique(nms)
}

tg_tier_names_one <- function(path) {
  txt <- tryCatch(tg_decode_text(path), error = function(e) NA_character_)
  if (is.na(txt)) return(character(0))
  lines <- strsplit(txt, "\r\n|\n|\r")[[1]]
  unquote <- function(x) gsub('""', '"', x, fixed = TRUE)
  # Long format: each tier header has a `name = "..."` line, and nothing else
  # in the file does (intervals use `text =`, points `mark =`).
  long <- '^\\s*name\\s*=\\s*"(.*)"\\s*$'
  hit <- grepl(long, lines)
  if (any(hit)) return(unquote(sub(long, "\\1", lines[hit])))
  # Short format: the line after "IntervalTier" / "TextTier" holds the name.
  k <- grep('^\\s*"(IntervalTier|TextTier)"\\s*$', lines)
  k <- k[k < length(lines)]
  short <- '^\\s*"(.*)"\\s*$'
  nm <- lines[k + 1]
  unquote(sub(short, "\\1", nm[grepl(short, nm)]))
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

#' Column names for TextGrid landmark tiers
#'
#' @description
#' The column prefix `p` that [attach_landmarks()] uses for each tier, whose
#' columns are then `p`, `p_start`, `p_end` and `p_i`. A tier gets the same
#' prefix whichever tiers are attached with it, and in whatever order, as long
#' as `all_tiers` is the same.
#'
#' @details
#' The prefix keeps the letters, in any script, and the digits of the tier
#' name. ASCII letters are lowercased. Any other run of characters becomes
#' `_`, and `_` at either end is dropped; a name with nothing left becomes
#' `"tier"`. So `"Syllable (broad)"` gives `syllable_broad`, and a tier named
#' in Chinese characters or IPA keeps its name. Names are first put in Unicode
#' NFC form (when the utf8 package is installed) and combining marks are then
#' dropped, so in a UTF-8 locale every prefix is a syntactic R name unless it
#' starts with a digit.
#'
#' A prefix already in use gets `_2`, `_3` and so on: when two tiers give the
#' same prefix, or when a prefix or one of its columns is in `reserved`. A
#' tier whose name is already its own prefix keeps it, and the others are
#' numbered in the byte order of their names. So tiers `syllable`, `Syllable`
#' and `syllable!` give `syllable`, `syllable_2` and `syllable_3`, and a tier
#' named `time` gives `time_2`. Columns made later from a prefix (`p_t01`,
#' `p_tseq`, `p_f0_dur`) are kept apart too.
#'
#' @param tier_names Tier names to give prefixes.
#' @param all_tiers Every tier name the columns could come from, such as all
#'   interval tiers of the corpus from [tg_interval_tiers()]. The numbering
#'   depends only on this set, so pass the same one in every call. Names in
#'   `tier_names` but not in `all_tiers` are numbered after it.
#' @param reserved Column names a tier must not take. Defaults to the columns
#'   of the f0 data.
#' @return A character vector of prefixes, named by `tier_names`.
#' @examples
#' landmark_prefixes(c("syllable", "Syllable (broad)", "time"))
#' # The same prefix whichever of the tiers is attached
#' tiers <- c("syllable", "Syllable", "syllable!")
#' landmark_prefixes("Syllable", all_tiers = tiers)
#' landmark_prefixes(tiers)
#' # A tier named in Chinese characters keeps its name
#' landmark_prefixes(intToUtf8(c(0x58F0, 0x8C03)))
#' @export
landmark_prefixes <- function(tier_names, all_tiers = tier_names,
                              reserved = c("token", "time", "f0", "intensity",
                                           "f0_corrected", "f0_original", "edited",
                                           "token_dropped", "n_missing", "has_gap",
                                           "point", "time_prop")) {
  tier_names <- enc2utf8(as.character(tier_names))
  pool <- unique(c(sort(unique(enc2utf8(as.character(all_tiers))), method = "radix"),
                   tier_names))
  base <- landmark_base(pool)
  base[!nzchar(base)] <- "tier"

  sfx   <- c("", "_start", "_end", "_i", "_t01", "_tseq", "_f0_dur")
  taken <- enc2utf8(as.character(reserved))
  out   <- character(length(pool))
  # Tiers whose name is already their prefix first, then the pool's order.
  for (k in order(base != pool, seq_along(pool))) {
    p <- base[k]
    n <- 1L
    while (any(paste0(p, sfx) %in% taken)) {
      n <- n + 1L
      p <- paste0(base[k], "_", n)
    }
    out[k] <- p
    taken  <- c(taken, paste0(p, sfx))
  }
  stats::setNames(out[match(tier_names, pool)], tier_names)
}

# The prefix of each name before any _2, _3 is added: its letters and digits,
# ASCII letters lowercased, other runs of characters as "_". "" when nothing
# is left (landmark_prefixes() then uses "tier").
landmark_base <- function(x) {
  x <- enc2utf8(as.character(x))
  if (requireNamespace("utf8", quietly = TRUE)) {
    x <- tryCatch(utf8::utf8_normalize(x), error = function(e) x)
  }
  # Combining marks would make the name non-syntactic (make.names turns them
  # into "."), as would \p{N} digits such as superscripts, so keep \p{Nd} only.
  x <- gsub("\\p{M}+", "", x, perl = TRUE)
  x <- gsub("[^\\p{L}\\p{Nd}]+", "_", x, perl = TRUE)
  x <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)
  gsub("^_+|_+$", "", x)
}

#' Attach TextGrid landmark columns to a long-format f0 data frame
#'
#' For each selected interval tier and each token, reads the token's TextGrid
#' (with [read_textgrid()]) and tags every f0 frame with the interval it falls
#' in. Adds four columns per tier, named from the tier's prefix `p` (see
#' [landmark_prefixes()]): `p` (interval label), `p_start`, `p_end` (interval
#' boundaries, seconds), and `p_i` (1-based segment index among labelled
#' intervals, e.g. syllable 1, 2, 3 ...).
#'
#' @param df Long-format f0 data frame with `token` and `time` columns.
#' @param audio Data frame with `basename` and `tg_path` columns (fp_audio_data).
#' @param tier_names Character vector of interval-tier names to attach.
#' @param strip_ext Strip file extensions when matching `token` to `basename`.
#' @param all_tiers Every tier name the columns could come from, such as all
#'   interval tiers of the corpus from [tg_interval_tiers()]; passed to
#'   [landmark_prefixes()]. Give the same set in every call, so a tier always
#'   gets the same columns.
#' @return `df` with the landmark columns appended. Returned unchanged when there
#'   are no tiers, no `token`/`time` columns, or no `basename`/`tg_path`
#'   columns in `audio`. A token with no matching TextGrid gets `NA` in these
#'   columns, so when no TextGrid matches at all, the columns are added and
#'   are all `NA`. These attributes are present only when there is something
#'   to report:
#'   * `"tg_unreadable"`: a token whose TextGrid could not be read keeps `NA`
#'     landmarks; this gives the reason for each such token, named by token.
#'   * `"tg_guessed"`: for each TextGrid read in a guessed 8-bit code page
#'     (see [read_textgrid()]), that code page named for the user (such as
#'     "Western European (Windows-1252)"), named by token.
#'   * `"tg_stray"`: for each TextGrid read as UTF-8 with bytes that are not
#'     UTF-8 shown as U+FFFD (see [read_textgrid()]), the lines that held
#'     them (such as "line 12"), named by token.
#'   * `"renamed_tiers"`: the prefix of each tier that got `_2`, `_3` and so
#'     on, or `"tier"` for a name with no letters or digits, named by tier.
#'     Lowercasing and `_` for other characters are not reported, so
#'     `"Syllable"` giving `syllable` is not listed.
#'   * `"missing_tiers"`: the tiers that none of the TextGrids read has as an
#'     interval tier. Their columns are all `NA`, and a warning says so.
#' @export
attach_landmarks <- function(df, audio, tier_names, strip_ext = TRUE,
                             all_tiers = tier_names) {
  if (is.null(df) || !nrow(df) || length(tier_names) == 0) return(df)
  if (!all(c("token", "time") %in% names(df))) return(df)
  if (is.null(audio) || !all(c("basename", "tg_path") %in% names(audio))) return(df)

  norm <- function(x) {
    x <- as.character(x)
    if (isTRUE(strip_ext)) x <- tools::file_path_sans_ext(x)
    tolower(trimws(x))
  }

  tier_names <- unique(enc2utf8(as.character(tier_names)))
  audio_key  <- norm(audio$basename)
  prefixes   <- landmark_prefixes(tier_names, all_tiers)

  out <- df
  for (p in prefixes) {
    out[[p]] <- NA_character_
    out[[paste0(p, "_start")]] <- NA_real_
    out[[paste0(p, "_end")]]   <- NA_real_
    out[[paste0(p, "_i")]]     <- NA_integer_
  }

  bad     <- character(0)
  guessed <- character(0)
  stray   <- character(0)
  found   <- rep(FALSE, length(tier_names))
  n_read  <- 0L
  for (tok in unique(out$token)) {
    rows <- which(out$token == tok)
    ai <- match(norm(tok), audio_key)
    if (is.na(ai) || is.na(audio$tg_path[ai]) || !nzchar(audio$tg_path[ai])) next
    r  <- read_textgrid(audio$tg_path[ai])
    tg <- r$tg
    if (is.null(tg)) {
      bad[as.character(tok)] <- r$reason
      next
    }
    n_read <- n_read + 1L
    if (is_legacy_encoding(r$encoding)) guessed[as.character(tok)] <- legacy_label(r$encoding)
    if (length(r$stray_lines)) stray[as.character(tok)] <- line_numbers_text(r$stray_lines)
    for (j in seq_along(tier_names)) {
      tn <- tier_names[j]; p <- prefixes[[j]]
      tier <- NULL
      for (ti in tg) {
        if (identical(ti$name, tn) && !is.null(ti$type) && ti$type == "interval") {
          tier <- ti; break
        }
      }
      if (is.null(tier)) next
      found[j] <- TRUE
      res <- assign_tier_landmarks(out$time[rows], tier$t1, tier$t2, tier$label)
      out[[p]][rows]                   <- res$label
      out[[paste0(p, "_start")]][rows] <- res$start
      out[[paste0(p, "_end")]][rows]   <- res$end
      out[[paste0(p, "_i")]][rows]     <- res$idx
    }
  }
  if (length(bad)) attr(out, "tg_unreadable") <- bad
  if (length(guessed)) attr(out, "tg_guessed") <- guessed
  if (length(stray)) attr(out, "tg_stray") <- stray
  # Report only a _2 suffix or the "tier" fallback, not lowercasing or "_".
  base    <- landmark_base(tier_names)
  renamed <- prefixes[!nzchar(base) | prefixes != base]
  if (length(renamed)) attr(out, "renamed_tiers") <- renamed
  # Only when some TextGrid was read: otherwise nothing says the tier is absent.
  if (n_read > 0L && !all(found)) {
    miss <- tier_names[!found]
    attr(out, "missing_tiers") <- miss
    warning(sprintf("No TextGrid has an interval tier named %s, so %s columns are NA.",
                    paste0("'", miss, "'", collapse = ", "),
                    if (length(miss) == 1L) "its" else "their"),
            call. = FALSE)
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
#' all nasals and which carries a syllabicity mark (`m̩`, `n̩`, `ŋ̩`) matches,
#' since these are tone-bearing units in e.g. Cantonese (唔, 五). The same
#' nasals *without* the mark (`m`, `n`, `ŋ`) do not.
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
  syllabic <- grepl("[\u0329\u030d]", s)
  s <- gsub("\\p{Mn}", "", s, perl = TRUE)               # combining diacritics
  s <- gsub("[\u02d0\u02d1\u02c8\u02cc\u02de]", "", s)   # length, stress, rhotic hook
  s <- gsub("[\u02e5-\u02e9\u203f\u2040]", "", s)        # Chao tone letters, ties
  s <- gsub("[0-9\u00b9\u00b2\u00b3\u2070-\u2079]", "", s)  # tone digits, superscripts
  s <- gsub("[[:space:]]+", "", s)
  vowel_class <- paste0(
    "aeiouyAEIOUY@",
    "\u00e6\u00c6\u00f8\u00d8\u0153\u0152\u0276",              # æ Æ ø Ø œ Œ ɶ
    "\u0250\u0251\u0252\u0254\u0258\u0259\u025a",     # ɐ ɑ ɒ ɔ ɘ ə ɚ
    "\u025b\u025c\u025d\u025e",                       # ɛ ɜ ɝ ɞ
    "\u0264\u0268\u026a\u026f\u0275",                 # ɤ ɨ ɪ ɯ ɵ
    "\u0289\u028a\u028c\u028f\u1d7b\u1d7f",           # ʉ ʊ ʌ ʏ ᵻ ᵿ
    "\u00c0-\u00c6\u00c8-\u00cf\u00d2-\u00d6\u00d8-\u00dd",
    "\u00e0-\u00e6\u00e8-\u00ef\u00f2-\u00f6\u00f8-\u00fd\u00ff",
    # Latin Extended-A/B precomposed vowels, incl. the pinyin tone marks
    # (ā ǎ ē ě ī ǐ ō ǒ ū ǔ ǖ ǘ ǚ ǜ) that NFC-encoded TextGrids carry. The
    # \p{Mn} strip above only catches their NFD spellings.
    "\u0100-\u0105\u0112-\u011b\u0128-\u0131",        # Ā-ą Ē-ě Ĩ-ı
    "\u014c-\u0151\u0168-\u0173\u0176-\u0178",        # Ō-ő Ũ-ų Ŷ-Ÿ
    "\u01cd-\u01dc\u01de-\u01e1\u01ea-\u01ed",        # Ǎ-ǜ Ǟ-ǡ Ǫ-ǭ
    "\u01fa-\u01ff\u0232\u0233"                       # Ǻ-ǿ Ȳ ȳ
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
