# read_text.R: read user text files whatever Unicode encoding they were
# saved in. read_csv_utf8() is what the app uses for every uploaded CSV; the
# internal helpers below (detect_text_encoding(), transcode_to_utf8()) are
# meant to serve any other text reader too, e.g. TextGrids.
#
# Praat writes text as UTF-16BE with a byte-order mark (BOM) whenever any
# character is non-ASCII (its default "try ASCII, then UTF-16" setting), and
# Excel's "Unicode Text" is UTF-16LE. A bare read.csv() reads both as bytes in
# the native encoding and fails, or returns one garbage column.

#' Read a CSV in any Unicode encoding
#'
#' @description
#' Reads a CSV (or tab-separated) file saved as UTF-8 (with or without a
#' byte-order mark), UTF-16 or UTF-32, and returns its text as UTF-8. This
#' covers Praat's tables, which Praat saves as UTF-16 whenever any cell is not
#' plain ASCII (for example Chinese token names), and Excel's "CSV UTF-8" and
#' "Unicode Text" formats. The app reads every uploaded CSV this way.
#'
#' @details
#' The encoding is found from the byte-order mark, or, without one, from where
#' the zero bytes fall (UTF-16 stores each ASCII character with a zero byte).
#' A file with neither is read as UTF-8. If its text is not valid UTF-8, it was
#' saved in an older 8-bit code page. It is read as Chinese GBK/GB18030 (as
#' Excel on Chinese Windows saves CSVs) or Big5 when most of its other
#' characters are common Chinese characters, punctuation or pinyin in that
#' code page, and as Windows-1252 (Western European) otherwise. Either way the
#' result carries a note, as this is a guess.
#'
#' A UTF-8 file can also hold a few bytes that are not UTF-8, such as one cell
#' pasted in from a Latin-1 source. When its valid UTF-8 characters far
#' outnumber those bytes and it does not read as Chinese GBK or Big5, it stays
#' UTF-8: only those bytes are replaced, each with U+FFFD (the replacement
#' character), and the note names their lines. This also applies to a file
#' with a UTF-8 byte-order mark.
#'
#' Zero bytes in a text file, such as the padding a crashed save can leave at
#' its end, are dropped.
#'
#' The separator is a comma unless the first line has tabs and no commas, as
#' in Praat's "Save as tab-separated file".
#'
#' Excel workbooks (.xlsx, .xls), other binary files, empty files and UTF-16
#' files with broken characters are refused with an error that says what to
#' do, rather than read in part.
#'
#' @param file Path to the file.
#' @param ... Passed to [utils::read.csv()], e.g. `stringsAsFactors`,
#'   `check.names`, `colClasses`, `na.strings`. Not `fileEncoding`,
#'   `encoding` or `text`, which are set here.
#' @param sep Field separator. `NULL` (default) picks a comma or a tab from
#'   the first line, as described in Details.
#' @param name File name to use in messages. Defaults to the file's own name;
#'   give the original name when `file` is a temporary copy, as with a Shiny
#'   upload.
#' @return A data frame as from [utils::read.csv()], with all text (values and
#'   column names) in UTF-8. Attribute `"encoding"` gives the encoding the file
#'   was read as (e.g. `"UTF-8"`, `"UTF-16BE"`, `"GB18030"`), and attribute
#'   `"encoding_note"` a sentence for the user when a code page had to be
#'   guessed or bytes that are not UTF-8 were replaced (otherwise it is
#'   absent). On failure it stops with an error of
#'   class `"shinytone_read_error"` whose message names the file.
#' @examples
#' # A UTF-16LE file with a byte-order mark, as Excel's "Unicode Text" writes
#' f <- tempfile(fileext = ".csv")
#' txt <- "token,f0\nma1,210\nma2,180\n"
#' writeBin(c(as.raw(c(0xFF, 0xFE)),
#'            iconv(txt, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), f)
#' df <- read_csv_utf8(f)
#' df
#' attr(df, "encoding")
#' @export
read_csv_utf8 <- function(file, ..., sep = NULL, name = basename(file)) {
  dots <- list(...)
  bad  <- intersect(names(dots), c("fileEncoding", "encoding", "text"))
  if (length(bad)) {
    stop("read_csv_utf8() sets ", paste(bad, collapse = ", "), " itself.",
         call. = FALSE)
  }
  if (!length(name)) name <- basename(file)
  if (!file.exists(file)) read_error(sprintf("Could not find the file '%s'.", name))

  enc <- detect_text_encoding(file)
  save_as <- "In Excel, use File > Save As and choose 'CSV UTF-8 (Comma delimited)'."
  switch(enc$kind,
    empty  = read_error(sprintf("'%s' is empty.", name)),
    zip    = read_error(sprintf("'%s' is a zipped file, such as an Excel workbook (.xlsx), not a CSV. %s",
                                name, save_as)),
    ole    = read_error(sprintf("'%s' is an Excel workbook (.xls), not a CSV. %s", name, save_as)),
    binary = read_error(sprintf("'%s' is not a text file, so it cannot be read as a CSV.", name)),
    NULL)
  if (file.size(file) <= enc$skip) read_error(sprintf("'%s' is empty.", name))   # a BOM alone
  # Text with zero bytes (e.g. padding after a crashed save): read a copy
  # without them. read.csv() drops any past the first 4 KB.
  tmps <- character(0)
  on.exit(unlink(tmps), add = TRUE)
  if (isTRUE(enc$nul)) {
    file <- tmps[length(tmps) + 1L] <- drop_nul_copy(file)
  }
  if (!"skipNul" %in% names(dots)) dots$skipNul <- TRUE

  # Read a path that holds UTF-8 bytes with no BOM. Returns the data frame,
  # or the read.csv() error as a condition object.
  read_utf8_path <- function(p) {
    if (is.null(sep)) {
      first <- readLines(p, n = 1L, warn = FALSE, encoding = "UTF-8")
      first <- if (length(first)) first[1] else ""
      sep <- if (!grepl(",", first, fixed = TRUE) && grepl("\t", first, fixed = TRUE)) "\t" else ","
    }
    withCallingHandlers(
      tryCatch(do.call(utils::read.csv, c(list(p, sep = sep, encoding = "UTF-8"), dots)),
               error = function(e) e),
      warning = function(w) {
        if (grepl("incomplete final line", conditionMessage(w), fixed = TRUE))
          invokeRestart("muffleWarning")
      })
  }
  via_tmp <- function(from, skip = 0L) {
    tmp <- transcode_to_utf8(file, from, skip)
    if (is.null(tmp)) {
      read_error(sprintf(paste("'%s' looks like %s text but has bytes that are not valid %s,",
                               "so it cannot be read. Save it as CSV UTF-8 and upload it again."),
                         name, from, from))
    }
    on.exit(unlink(tmp))
    read_utf8_path(tmp)
  }

  used <- enc$encoding
  note <- NULL
  df <- if (enc$kind == "plain") read_utf8_path(file) else via_tmp(enc$encoding, enc$skip)
  # UTF-8 (by its BOM, or by default) that is not valid UTF-8: a UTF-8 file
  # with a few stray bytes, read as UTF-8 with those shown as U+FFFD, or,
  # with no BOM, an 8-bit code page, guessed and read again.
  if (used == "UTF-8" &&
      (is.data.frame(df) && !all_valid_utf8(df) ||
       inherits(df, "error") && !file_valid_utf8(file))) {
    bytes <- readBin(file, "raw", file.size(file))
    if (enc$skip > 0L) bytes <- bytes[-seq_len(enc$skip)]
    bytes <- bytes[bytes != as.raw(0)]
    how <- resolve_non_utf8(bytes)
    if (how$encoding == "UTF-8") {
      file <- tmps[length(tmps) + 1L] <- tempfile(fileext = ".csv")
      writeBin(how$bytes, file)
      df   <- read_utf8_path(file)
      note <- stray_note(name, how$lines)
    } else if (enc$kind == "bom") {
      read_error(sprintf("'%s' starts with a UTF-8 mark but is not valid UTF-8. Save it as CSV UTF-8 and upload it again.",
                         name))
    } else {
      if (length(bytes) < file.size(file)) {      # zero bytes dropped
        file <- tmps[length(tmps) + 1L] <- tempfile(fileext = ".csv")
        writeBin(bytes, file)
      }
      used <- how$encoding
      df <- via_tmp(used)
      note <- sprintf(paste("'%s' is not saved as UTF-8, so it was read as %s.",
                            "If any text looks wrong, save it as CSV UTF-8 and upload it again."),
                      name, legacy_label(used))
    }
  }
  if (inherits(df, "error")) {
    m <- conditionMessage(df)
    if (grepl("no lines available", m, fixed = TRUE)) read_error(sprintf("'%s' is empty.", name))
    read_error(sprintf("Could not read '%s' as a CSV: %s.", name, sub("[.[:space:]]+$", "", m)))
  }
  names(df) <- enc2utf8(names(df))
  attr(df, "encoding")      <- used
  attr(df, "encoding_note") <- note
  df
}

# An error the app can show as is: the message is written for the user.
read_error <- function(msg) {
  stop(structure(class = c("shinytone_read_error", "error", "condition"),
                 list(message = msg, call = NULL)))
}

# What a file is, from its first `sniff` bytes. Returns list(kind, encoding,
# skip), plus nul = TRUE when the text has zero bytes to drop:
#   kind "empty", "zip" (.xlsx, .ods), "ole" (.xls) or "binary": not text;
#     encoding is NA.
#   kind "bom": encoding from the byte-order mark ("UTF-8", "UTF-16LE",
#     "UTF-16BE", "UTF-32LE" or "UTF-32BE"); skip is the BOM's length.
#   kind "nul-pattern": "UTF-16LE" or "UTF-16BE" with no BOM, from where the
#     zero bytes fall (ASCII text in UTF-16 has one in every other byte).
#   kind "plain": no BOM; encoding "UTF-8" is only a guess, so check the text
#     with validUTF8() and fall back on guess_legacy_encoding(). Zero bytes
#     at the end (padding) or a few elsewhere do not make a file binary;
#     control bytes in more than 5% of the rest do.
# For any kind but "plain", transcode_to_utf8(path, encoding, skip) gives a
# UTF-8 copy.
detect_text_encoding <- function(path, sniff = 4096L) {
  sniff_text_encoding(readBin(path, "raw", n = min(file.size(path), sniff)), sniff)
}

# The same from bytes already read: the start of the file, or all of it.
sniff_text_encoding <- function(bytes, sniff = 4096L) {
  if (length(bytes) > sniff) bytes <- bytes[seq_len(sniff)]
  n <- length(bytes)
  starts <- function(...) {
    b <- as.raw(c(...))
    n >= length(b) && all(bytes[seq_along(b)] == b)
  }
  out <- function(kind, encoding = NA_character_, skip = 0L, nul = FALSE) {
    r <- list(kind = kind, encoding = encoding, skip = skip)
    if (nul) r$nul <- TRUE
    r
  }
  if (n == 0L)                        return(out("empty"))
  if (starts(0x50, 0x4B, 0x03, 0x04)) return(out("zip"))
  if (starts(0xD0, 0xCF, 0x11, 0xE0)) return(out("ole"))
  # UTF-32 before UTF-16: FF FE 00 00 also starts with the UTF-16LE BOM.
  if (starts(0xFF, 0xFE, 0x00, 0x00)) return(out("bom", "UTF-32LE", 4L))
  if (starts(0x00, 0x00, 0xFE, 0xFF)) return(out("bom", "UTF-32BE", 4L))
  if (starts(0xEF, 0xBB, 0xBF))       return(out("bom", "UTF-8",    3L))
  if (starts(0xFF, 0xFE))             return(out("bom", "UTF-16LE", 2L))
  if (starts(0xFE, 0xFF))             return(out("bom", "UTF-16BE", 2L))
  nul  <- bytes == as.raw(0)
  last <- if (any(!nul)) max(which(!nul)) else 0L
  if (last == 0L)                     return(out("binary"))   # zero bytes only
  nul  <- nul[seq_len(last)]          # without the padding at the end
  m <- last - last %% 2L
  if (m >= 2L) {
    n_1st <- sum(nul[seq_len(m)][c(TRUE, FALSE)])
    n_2nd <- sum(nul[seq_len(m)][c(FALSE, TRUE)])
    pairs <- m / 2
    if (n_2nd >= 0.2 * pairs && n_1st <= 0.05 * pairs) return(out("nul-pattern", "UTF-16LE"))
    if (n_1st >= 0.2 * pairs && n_2nd <= 0.05 * pairs) return(out("nul-pattern", "UTF-16BE"))
  }
  if (any(nul)) {
    b <- as.integer(bytes[seq_len(last)])
    if (mean(b < 9L | (b > 13L & b < 32L)) > 0.05) return(out("binary"))
  }
  out("plain", "UTF-8", nul = last < n || any(nul))
}

# A copy of `path` without its zero bytes, in a temp file the caller unlinks.
drop_nul_copy <- function(path, bytes = readBin(path, "raw", file.size(path))) {
  tmp <- tempfile(fileext = ".csv")
  writeBin(bytes[bytes != as.raw(0)], tmp)
  tmp
}

# Write the bytes of `path` (after skipping `skip` bytes, e.g. a BOM),
# decoded from `from`, to a UTF-8 temp file in raw chunks. Returns the temp
# path, which the caller unlinks, or NULL when the bytes are not valid `from`.
# Works in any session locale, unlike file(encoding =) or
# read.csv(fileEncoding =), which re-encode to the native charset and stop
# at the first bad byte with only a warning.
transcode_to_utf8 <- function(path, from, skip = 0L, chunk = 4194304L) {
  unit <- switch(from, "UTF-16LE" = , "UTF-16BE" = 2L, "UTF-32LE" = , "UTF-32BE" = 4L, 1L)
  # A multibyte code page (GB18030) has no safe split point: one piece.
  if (unit == 1L && from != "UTF-8") chunk <- max(chunk, file.size(path))
  tmp <- tempfile(fileext = ".csv")
  inp <- file(path, "rb")
  out <- file(tmp, "wb")
  ok  <- FALSE
  on.exit({
    close(inp)
    close(out)
    if (!ok) unlink(tmp)
  })
  if (skip > 0L) readBin(inp, "raw", skip)
  carry <- raw(0)
  repeat {
    got <- readBin(inp, "raw", chunk)
    b   <- c(carry, got)
    if (!length(b)) break
    eof  <- length(got) < chunk
    keep <- length(b) - length(b) %% unit
    if (unit == 2L && !eof && keep >= 2L) {   # never split a surrogate pair
      hi <- if (from == "UTF-16LE") b[keep] else b[keep - 1L]
      if (hi >= as.raw(0xD8) && hi <= as.raw(0xDB)) keep <- keep - 2L
    }
    carry <- if (keep < length(b)) b[(keep + 1L):length(b)] else raw(0)
    if (keep > 0L) {
      if (from == "UTF-8") {
        writeBin(b[seq_len(keep)], out)
      } else {
        # Keep the character form: iconv(toRaw = TRUE) returns the input bytes
        # unchanged on invalid input (R 4.6), where this returns NA.
        s <- tryCatch(iconv(list(b[seq_len(keep)]), from, "UTF-8"),
                      error = function(e) NA_character_)
        if (is.na(s)) return(NULL)
        writeBin(charToRaw(s), out)
      }
    }
    if (eof) break
  }
  ok <- TRUE
  tmp   # an odd byte left at the very end (a truncated file) is dropped
}

# Code page of text with no BOM that is not valid UTF-8, from its bytes (zero
# bytes are ignored): "GB18030" (GBK) or "BIG5" when more than half of its
# non-ASCII characters look like Chinese text in that code page, else
# "CP1252", or "latin1" when CP1252 cannot decode it. A Western accented
# letter and the plain letter after it also decode as one valid GBK or Big5
# character, so only these count:
#   GBK: GB2312 punctuation, fullwidth forms, pinyin and its 3755 level-1
#     hanzi; or a frequent traditional hanzi (Big5 A440-C67E) that is not
#     next to a Latin letter.
#   Big5: its symbols and 5401 frequent hanzi (A140-C67E), not next to a
#     Latin letter.
# GBK wins a tie.
guess_legacy_encoding <- function(bytes) {
  txt <- rawToChar(bytes[bytes != as.raw(0)])
  two_bytes <- function(u, enc)
    lapply(iconv(intToUtf8(u, multiple = TRUE), "UTF-8", enc, toRaw = TRUE), as.integer)
  big5_frequent <- function(b)
    length(b) == 2L && b[1] >= 0xA1 && (b[1] < 0xC6 || b[1] == 0xC6 && b[2] <= 0x7E)
  gb2312_common <- function(b) length(b) == 2L && b[2] >= 0xA1 &&
    (b[1] >= 0xA1 && b[1] <= 0xA3 || b[1] == 0xA8 || b[1] >= 0xB0 && b[1] <= 0xD7)
  share <- function(enc) {
    s <- tryCatch(iconv(txt, enc, "UTF-8", sub = NA), error = function(e) NA_character_)
    if (is.na(s)) return(0)
    cp <- utf8ToInt(s)
    i  <- which(cp > 127L)
    if (!length(i)) return(0)
    letter <- function(x) x >= 65L & x <= 90L | x >= 97L & x <= 122L
    pad    <- c(0L, cp, 0L)
    loose  <- !letter(pad[i]) & !letter(pad[i + 2L])   # no Latin letter either side
    u <- unique(cp[i])
    k <- match(cp[i], u)
    freq <- vapply(two_bytes(u, "BIG5"), big5_frequent, logical(1))[k]
    ok <- if (enc == "BIG5") freq & loose else {
      hanzi <- (u >= 0x4E00 & u <= 0x9FFF | u >= 0x3400 & u <= 0x4DBF)[k]
      vapply(two_bytes(u, enc), gb2312_common, logical(1))[k] | hanzi & freq & loose
    }
    mean(ok)
  }
  gb <- share("GB18030")
  b5 <- share("BIG5")
  if (max(gb, b5) > 0.5) return(if (gb >= b5) "GB18030" else "BIG5")
  if (!is.na(iconv(txt, "CP1252", "UTF-8", sub = NA))) "CP1252" else "latin1"
}

# How to read text with no BOM (or a UTF-8 one) that is not valid UTF-8,
# from its bytes without zero bytes. Usually it was saved in an 8-bit code
# page. But a UTF-8 file can also hold a few stray bytes, such as one cell
# pasted in from a Latin-1 source, and reading all of it in a code page would
# garble every valid character. So it stays UTF-8 when its valid multi-byte
# characters take at least 3 times as many bytes as the stray ones and it
# does not read as Chinese GBK or Big5, whose text holds valid UTF-8
# sequences by chance (a tiny GBK file can pass the 3:1 test). Returns
# list(encoding, bytes, lines): encoding "UTF-8" with each stray byte in
# `bytes` replaced by U+FFFD and `lines` the line numbers that held one, or
# else the code page from guess_legacy_encoding(), `bytes` unchanged and
# `lines` NULL.
resolve_non_utf8 <- function(bytes) {
  scan  <- utf8_scan(bytes)
  n_bad <- length(scan$bad)
  guess <- guess_legacy_encoding(bytes)
  if (n_bad > 0L && scan$good >= 3L * n_bad && !guess %in% c("GB18030", "BIG5")) {
    return(list(encoding = "UTF-8", bytes = replace_bytes(bytes, scan$bad),
                lines = line_of_byte(bytes, scan$bad)))
  }
  list(encoding = guess, bytes = bytes, lines = NULL)
}

# Where text stops being UTF-8. Returns list(bad, good): the positions of the
# bytes that are in no valid UTF-8 sequence (as validUTF8() judges it: no
# overlong forms, surrogates or code points past U+10FFFF), and how many
# bytes are in valid multi-byte sequences. Decoding left to right and
# skipping one byte at each error gives the same bad bytes: a valid
# sequence's trailing bytes (0x80-0xBF) can never start one. Only the
# non-ASCII bytes are looked at, so it is cheap on mostly ASCII text.
# iconv(from = "UTF-8", to = "UTF-8") is no check: macOS lets surrogates and
# 5-byte forms through.
utf8_scan <- function(bytes) {
  n  <- length(bytes)
  hi <- which(bytes >= as.raw(0x80))
  if (!length(hi)) return(list(bad = integer(0), good = 0L))
  after <- function(k) {                  # the byte k places on; -1 past the end
    j <- hi + k
    out <- rep(-1L, length(hi))
    out[j <= n] <- as.integer(bytes[j[j <= n]])
    out
  }
  b  <- as.integer(bytes[hi])
  b1 <- after(1L)
  b2 <- after(2L)
  b3 <- after(3L)
  cont <- function(x, lo = 0x80L, up = 0xBFL) x >= lo & x <= up
  s2 <- b >= 0xC2L & b <= 0xDFL & cont(b1)
  s3 <- (b == 0xE0L & cont(b1, 0xA0L) | b >= 0xE1L & b <= 0xECL & cont(b1) |
           b == 0xEDL & cont(b1, 0x80L, 0x9FL) | b >= 0xEEL & b <= 0xEFL & cont(b1)) &
    cont(b2)
  s4 <- (b == 0xF0L & cont(b1, 0x90L) | b >= 0xF1L & b <= 0xF3L & cont(b1) |
           b == 0xF4L & cont(b1, 0x80L, 0x8FL)) & cont(b2) & cont(b3)
  # A sequence's trailing bytes are the next non-ASCII bytes in `hi`.
  ok <- s2 | s3 | s4
  ok[which(s2 | s3 | s4) + 1L] <- TRUE
  ok[which(s3 | s4) + 2L] <- TRUE
  ok[which(s4) + 3L] <- TRUE
  list(bad = hi[!ok], good = sum(ok))
}

# `bytes` with the bytes at positions `pos` (sorted) each replaced by U+FFFD.
replace_bytes <- function(bytes, pos) {
  fffd <- as.raw(c(0xEF, 0xBF, 0xBD))
  cuts <- c(0L, pos, length(bytes) + 1L)
  unlist(lapply(seq_len(length(cuts) - 1L), function(k) {
    from <- cuts[k] + 1L
    to   <- cuts[k + 1L] - 1L
    c(if (to >= from) bytes[from:to], if (k <= length(pos)) fffd)
  }))
}

# The line (1-based) of each byte position, counting "\n" (or "\r" alone in
# a file with no "\n").
line_of_byte <- function(bytes, pos) {
  nl <- which(bytes == as.raw(0x0A))
  if (!length(nl)) nl <- which(bytes == as.raw(0x0D))
  sort(unique(findInterval(pos - 1L, nl) + 1L))
}

# "line 4", "lines 4 and 9", "lines 4, 9, 12, 20, 31 and 3 more".
line_numbers_text <- function(lines) {
  lines <- sort(unique(lines))
  if (length(lines) == 1L) return(paste("line", lines))
  shown <- utils::head(lines, 5L)
  more  <- length(lines) - length(shown)
  paste("lines", if (more > 0L) {
    paste0(paste(shown, collapse = ", "), " and ", more, " more")
  } else {
    paste0(paste(utils::head(shown, -1L), collapse = ", "), " and ", shown[length(shown)])
  })
}

# The note for a file read as UTF-8 with stray bytes replaced.
stray_note <- function(name, lines) {
  sprintf("'%s' is UTF-8 except on %s, where some text is not valid UTF-8 and is shown as %s.",
          name, line_numbers_text(lines), intToUtf8(0xFFFD))
}

# How a code page from guess_legacy_encoding() is named to the user, and
# whether an encoding is one of those guesses.
legacy_labels <- c(GB18030 = "Chinese (GBK/GB18030)", BIG5 = "Chinese (Big5)",
                   CP1252  = "Western European (Windows-1252)", latin1 = "Latin-1")
legacy_label <- function(enc) unname(legacy_labels[enc])
is_legacy_encoding <- function(enc) enc %in% names(legacy_labels)

# TRUE when the whole file is valid UTF-8 text (zero bytes aside). Reads it
# all, so it is only used once a read has failed.
file_valid_utf8 <- function(path) {
  b <- readBin(path, "raw", n = file.size(path))
  validUTF8(rawToChar(b[b != as.raw(0)]))
}

# TRUE when every name, string and factor level of `df` is valid UTF-8.
all_valid_utf8 <- function(df) {
  s <- c(names(df), unlist(lapply(df, function(x)
    if (is.factor(x)) levels(x) else if (is.character(x)) x[!is.na(x)] else NULL),
    use.names = FALSE))
  all(validUTF8(s))
}
