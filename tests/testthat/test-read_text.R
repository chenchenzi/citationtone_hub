# ---------- read_csv_utf8: test files ----------------------------------------

# Chinese text as \u escapes: 妈 5988, 麻 9EBB, 阴平 9634 5E73, 阳平 9633 5E73.
zh_lines <- c("speaker,token,tone,time,f0",
              "S01,\u{5988}1,\u{9634}\u{5E73},0.01,210.5",
              "S01,\u{5988}1,\u{9634}\u{5E73},0.02,211.25",
              "S01,\u{9EBB}2,\u{9633}\u{5E73},0.01,180",
              "S01,\u{9EBB}2,\u{9633}\u{5E73},0.02,NA")
zh_ref <- data.frame(
  speaker = "S01",
  token   = c("\u{5988}1", "\u{5988}1", "\u{9EBB}2", "\u{9EBB}2"),
  tone    = rep(c("\u{9634}\u{5E73}", "\u{9633}\u{5E73}"), each = 2),
  time    = c(0.01, 0.02, 0.01, 0.02),
  f0      = c(210.5, 211.25, 180, NA),
  stringsAsFactors = FALSE)

lines_text <- function(lines, eol = "\n", sep = ",") {
  paste0(gsub(",", sep, lines, fixed = TRUE), eol, collapse = "")
}
enc_bytes <- function(s, to) {
  if (to == "UTF-8") charToRaw(enc2utf8(s)) else iconv(s, "UTF-8", to, toRaw = TRUE)[[1]]
}
bom <- list(utf8 = as.raw(c(0xEF, 0xBB, 0xBF)), le = as.raw(c(0xFF, 0xFE)),
            be = as.raw(c(0xFE, 0xFF)), le32 = as.raw(c(0xFF, 0xFE, 0x00, 0x00)))
write_bytes <- function(bytes, ext = ".csv") {
  f <- tempfile(fileext = ext)
  writeBin(bytes, f)
  f
}
strip_attrs <- function(df) {
  attr(df, "encoding") <- NULL
  attr(df, "encoding_note") <- NULL
  df
}
fixture <- function(name) test_path("fixtures", name)

# ---------- Unicode files ------------------------------------------------------

test_that("every Unicode variant reads to the same UTF-8 data", {
  txt <- lines_text(zh_lines)
  variants <- list(
    "UTF-8"                  = list("UTF-8",    enc_bytes(txt, "UTF-8")),
    "UTF-8 + BOM"            = list("UTF-8",    c(bom$utf8, enc_bytes(txt, "UTF-8"))),
    "UTF-8 + BOM, CRLF"      = list("UTF-8",    c(bom$utf8, enc_bytes(lines_text(zh_lines, "\r\n"), "UTF-8"))),
    "UTF-16LE + BOM"         = list("UTF-16LE", c(bom$le, enc_bytes(txt, "UTF-16LE"))),
    "UTF-16LE"               = list("UTF-16LE", enc_bytes(txt, "UTF-16LE")),
    "UTF-16BE + BOM"         = list("UTF-16BE", c(bom$be, enc_bytes(txt, "UTF-16BE"))),
    "UTF-16BE"               = list("UTF-16BE", enc_bytes(txt, "UTF-16BE")),
    "UTF-16LE + BOM, CRLF"   = list("UTF-16LE", c(bom$le, enc_bytes(lines_text(zh_lines, "\r\n"), "UTF-16LE"))),
    "UTF-16LE + BOM, CRLF, tab" =
      list("UTF-16LE", c(bom$le, enc_bytes(lines_text(zh_lines, "\r\n", "\t"), "UTF-16LE"))),
    "UTF-16BE + BOM, tab"    = list("UTF-16BE", c(bom$be, enc_bytes(lines_text(zh_lines, sep = "\t"), "UTF-16BE"))),
    "UTF-16LE, no final EOL" = list("UTF-16LE", enc_bytes(sub("\n$", "", txt), "UTF-16LE")),
    "UTF-32LE + BOM"         = list("UTF-32LE", c(bom$le32, enc_bytes(txt, "UTF-32LE")))
  )
  for (v in names(variants)) {
    f  <- write_bytes(variants[[v]][[2]])
    df <- expect_silent(read_csv_utf8(f, stringsAsFactors = FALSE))
    expect_identical(strip_attrs(df), zh_ref, info = v)
    expect_identical(attr(df, "encoding"), variants[[v]][[1]], info = v)
    expect_null(attr(df, "encoding_note"))
    expect_true(all(Encoding(df$token) == "UTF-8"), info = v)
    expect_true(all(Encoding(df$tone) == "UTF-8"), info = v)
  }
})

test_that("read.csv() options pass through", {
  f <- write_bytes(c(bom$be, enc_bytes(lines_text(zh_lines), "UTF-16BE")))
  df <- read_csv_utf8(f, stringsAsFactors = FALSE, colClasses = "character")
  expect_true(all(vapply(df, is.character, logical(1))))
  expect_identical(df$f0, c("210.5", "211.25", "180", NA))
  expect_identical(nrow(read_csv_utf8(f, nrows = 2)), 2L)
  expect_true(is.factor(read_csv_utf8(f, stringsAsFactors = TRUE)$token))

  semi <- write_bytes(enc_bytes(lines_text(zh_lines, sep = ";"), "UTF-8"))
  expect_identical(strip_attrs(read_csv_utf8(semi, sep = ";")), zh_ref)
})

test_that("Chinese column names come through with check.names = FALSE", {
  hdr <- c("\u{8BF4}\u{8BDD}\u{4EBA},token,\u{58F0}\u{8C03},time,f0",
           "S01,\u{5988}1,\u{9634}\u{5E73},0.01,210.5")
  f  <- write_bytes(c(bom$be, enc_bytes(lines_text(hdr), "UTF-16BE")))
  df <- read_csv_utf8(f, stringsAsFactors = FALSE, check.names = FALSE)
  expect_identical(names(df), c("\u{8BF4}\u{8BDD}\u{4EBA}", "token", "\u{58F0}\u{8C03}", "time", "f0"))
  expect_true(all(validUTF8(names(df))))
})

# ---------- Praat output -------------------------------------------------------

test_that("a table saved by Praat with its default settings (UTF-16BE) reads", {
  f <- fixture("praat_default_utf16be_bom.csv")
  expect_identical(readBin(f, "raw", 2L), bom$be)
  df <- read_csv_utf8(f, stringsAsFactors = FALSE, na.strings = c("NA", "--undefined--"))
  expect_identical(attr(df, "encoding"), "UTF-16BE")
  expect_identical(strip_attrs(df), zh_ref)
  expect_true(all(Encoding(df$token) == "UTF-8"))
  # Without na.strings, Praat's --undefined-- keeps f0 as text.
  raw_f0 <- read_csv_utf8(f, stringsAsFactors = FALSE)$f0
  expect_identical(raw_f0[4], "--undefined--")
})

# ---------- 8-bit code pages ---------------------------------------------------

test_that("GBK (Excel on Chinese Windows) is read as GB18030, with a note", {
  gb <- tryCatch(enc_bytes(lines_text(zh_lines), "GB18030"), error = function(e) NULL)
  skip_if(is.null(gb) || anyNA(gb), "GB18030 not supported by iconv here")
  f  <- write_bytes(gb)
  df <- read_csv_utf8(f, stringsAsFactors = FALSE, name = "gbk.csv")
  expect_identical(strip_attrs(df), zh_ref)
  expect_identical(attr(df, "encoding"), "GB18030")
  expect_match(attr(df, "encoding_note"), "'gbk.csv' is not saved as UTF-8", fixed = TRUE)
  expect_true(all(Encoding(df$token) == "UTF-8"))
})

test_that("Latin-1 / Windows-1252 is read as Windows-1252, with a note", {
  lat <- c("speaker,token,tone,time,f0", "S01,caf\u{E9},T1,0.01,210.5",
           "S01,na\u{EF}ve,T2,0.02,180")
  f  <- write_bytes(enc_bytes(lines_text(lat), "latin1"))
  df <- read_csv_utf8(f, stringsAsFactors = FALSE, name = "latin.csv")
  expect_identical(df$token, c("caf\u{E9}", "na\u{EF}ve"))
  expect_identical(attr(df, "encoding"), "CP1252")
  expect_match(attr(df, "encoding_note"), "Windows-1252", fixed = TRUE)
  # check.names = TRUE takes the same path (make.names() errors on bad bytes).
  expect_identical(read_csv_utf8(f)$token, c("caf\u{E9}", "na\u{EF}ve"))
})

test_that("GBK with pinyin, Chinese punctuation or traditional characters reads as Chinese", {
  skip_if(is.na(iconv("\u{5988}", "UTF-8", "GB18030")), "GB18030 not supported by iconv here")
  check <- function(lines, enc = "GB18030") {
    f  <- write_bytes(enc_bytes(lines_text(lines), enc))
    df <- read_csv_utf8(f, stringsAsFactors = FALSE)
    ref <- utils::read.csv(text = lines_text(lines), stringsAsFactors = FALSE, encoding = "UTF-8")
    expect_identical(strip_attrs(df), ref, info = lines[2])
    expect_identical(attr(df, "encoding"), enc, info = lines[2])
    expect_false(is.null(attr(df, "encoding_note")))
  }
  # Excel on Chinese Windows: hanzi with a pinyin column (m\u0101 = ma1).
  check(c("speaker,token,pinyin,tone,time,f0",
          "S01,\u{5988},m\u{101},\u{9634}\u{5E73},0.01,210",
          "S01,\u{9EBB},m\u{E1},\u{9633}\u{5E73},0.01,180"))
  # Curly quotes, a middle dot and an ellipsis.
  check(c("speaker,token,note",
          "S01,\u{5988},\u{201C}\u{597D}\u{201D}",
          "S01,\u{9EBB},\u{5F20}\u{B7}\u{4E09}\u{2026}"))
  # Pinyin only, no hanzi.
  check(c("speaker,token,tone", "S01,m\u{101},T1", "S01,m\u{E1},T2", "S01,m\u{1CE},T3"))
  # Traditional characters saved as GBK.
  check(c("speaker,token,tone", "S01,\u{5ABD},\u{9670}\u{5E73}", "S01,\u{99AC},\u{4E0A}\u{8072}"))
  # Big5 (Taiwan, Hong Kong).
  skip_if(is.na(iconv("\u{5ABD}", "UTF-8", "BIG5")), "BIG5 not supported by iconv here")
  check(c("speaker,token,tone", "S01,\u{5ABD},\u{9670}\u{5E73}", "S01,\u{9EBB},\u{967D}\u{5E73}"),
        "BIG5")
})

test_that("Western European text is not taken for Chinese", {
  # An accented letter and the letter after it also form a valid GBK pair.
  for (words in list(c("M\u{FC}ller", "J\u{FC}rgen", "Stra\u{DF}e"),
                     c("ann\u{E9}e", "tr\u{E8}s", "\u{E9}l\u{E8}ve"),
                     c("se\u{F1}or", "a\u{F1}o", "acci\u{F3}n"),
                     c("informa\u{E7}\u{E3}o", "a\u{E7}\u{F5}es", "n\u{E3}o"))) {
    lines <- c("speaker,time,f0", paste0(words, ",0.01,210"))
    f  <- write_bytes(enc_bytes(lines_text(lines), "latin1"))
    df <- read_csv_utf8(f, stringsAsFactors = FALSE)
    expect_identical(df$speaker, words)
    expect_identical(attr(df, "encoding"), "CP1252", info = words[1])
  }
})

test_that("a UTF-8 CSV with a few stray bytes stays UTF-8, with a note naming the lines", {
  # One speaker typed in Latin-1 (caf + 0xE9) in a UTF-8 file of Chinese tones:
  # reading it all as Windows-1252 would garble every tone.
  stray <- c(enc_bytes(lines_text(zh_lines), "UTF-8"),
             charToRaw("S01,\u{5988}1,\u{9634}\u{5E73},0.03,212\nS01,caf"), as.raw(0xE9),
             charToRaw(",\u{9633}\u{5E73},0.04,181\n"))
  for (with_bom in c(FALSE, TRUE)) {
    f  <- write_bytes(c(if (with_bom) bom$utf8, stray))
    df <- read_csv_utf8(f, stringsAsFactors = FALSE, name = "mixed.csv")
    expect_identical(attr(df, "encoding"), "UTF-8")
    expect_identical(df$tone, c(zh_ref$tone, "\u{9634}\u{5E73}", "\u{9633}\u{5E73}"))
    expect_identical(df$token[6], "caf\u{FFFD}")
    expect_true(all(validUTF8(df$token)))
    expect_identical(attr(df, "encoding_note"),
                     "'mixed.csv' is UTF-8 except on line 7, where some text is not valid UTF-8 and is shown as \u{FFFD}.")
  }
  # Stray bytes on several lines
  many <- unlist(lapply(1:8, function(i)
    c(charToRaw(sprintf("S%02d,\u{5988}\u{9EBB}\u{9634}\u{5E73},", i)),
      if (i %% 2 == 0) as.raw(0xE9) else charToRaw("x"), charToRaw("\n"))))
  df <- read_csv_utf8(write_bytes(c(charToRaw("speaker,token,note\n"), many)),
                      stringsAsFactors = FALSE, name = "m.csv")
  expect_identical(df$token, rep("\u{5988}\u{9EBB}\u{9634}\u{5E73}", 8))
  expect_match(attr(df, "encoding_note"), "except on lines 3, 5, 7 and 9,", fixed = TRUE)
  expect_identical(line_numbers_text(c(9, 2, 4, 2, 30, 31, 40, 50)),
                   "lines 2, 4, 9, 30, 31 and 2 more")
})

test_that("a tiny GBK file with chance UTF-8 sequences still reads as GBK", {
  skip_if(is.na(iconv("\u{5988}", "UTF-8", "GB18030")), "GB18030 not supported by iconv here")
  # C4A3 C5A5 C6A7 are GB2312 hanzi that are also valid UTF-8 pairs, so the
  # valid bytes outnumber the stray ones (B0A1) 3 to 1.
  gbk <- as.raw(c(0xC4, 0xA3, 0xC5, 0xA5, 0xC6, 0xA7, 0x2C, 0xB0, 0xA1))
  df  <- read_csv_utf8(write_bytes(c(charToRaw("a,b\n"), gbk, charToRaw("\n"))),
                       stringsAsFactors = FALSE)
  expect_identical(attr(df, "encoding"), "GB18030")
  expect_identical(c(df$a, df$b), strsplit(iconv(rawToChar(gbk), "GB18030", "UTF-8"), ",")[[1]])
  # Latin-1 text has no valid UTF-8 sequences at all.
  lat <- enc_bytes("caf\u{E9},na\u{EF}ve,\u{C9}l\u{E8}ve", "latin1")
  expect_identical(resolve_non_utf8(lat)$encoding, "CP1252")
})

test_that("utf8_scan() finds exactly the bytes validUTF8() rejects", {
  set.seed(7)
  pool <- c(1:255, rep(0x80:0xBF, 4), rep(0xE4:0xE9, 4), rep(0x41, 30))
  wrong <- 0L
  for (i in 1:2000) {
    x <- as.raw(sample(pool, sample(1:14, 1), replace = TRUE))
    s <- utf8_scan(x)
    ok <- (length(s$bad) == 0) == validUTF8(rawToChar(x))
    if (length(s$bad)) {
      fixed <- replace_bytes(x, s$bad)
      ok <- ok && validUTF8(rawToChar(x[-s$bad])) && validUTF8(rawToChar(fixed)) &&
        length(fixed) == length(x) + 2L * length(s$bad)
    }
    wrong <- wrong + !ok
  }
  expect_identical(wrong, 0L)
  # Surrogates and overlong forms are not UTF-8 (macOS iconv lets them through).
  expect_length(utf8_scan(as.raw(c(0x61, 0xED, 0xA0, 0x80)))$bad, 3L)
  expect_length(utf8_scan(as.raw(c(0xC0, 0xAF)))$bad, 2L)
  expect_identical(utf8_scan(enc_bytes("\u{9634}a\u{E9}", "UTF-8")), list(bad = integer(0), good = 5L))
})

test_that("guess_legacy_encoding() weighs common characters, not just valid ones", {
  g <- function(txt, enc) guess_legacy_encoding(enc_bytes(txt, enc))
  expect_identical(g("\u{5988},\u{9634}\u{5E73}", "GB18030"), "GB18030")
  expect_identical(g("m\u{101},m\u{E1}", "GB18030"), "GB18030")
  expect_identical(g("M\u{FC}ller,Stra\u{DF}e", "latin1"), "CP1252")
  expect_identical(g("\u{E9}l\u{E8}ve", "latin1"), "CP1252")
  expect_identical(g("\u{201C}Hello\u{201D} \u{2013} caf\u{E9}", "CP1252"), "CP1252")
  # 0x81 is not in CP1252
  expect_identical(guess_legacy_encoding(as.raw(c(0x61, 0x81, 0x2C))), "latin1")
  # Zero bytes are ignored
  expect_identical(guess_legacy_encoding(c(enc_bytes("M\u{FC}ller", "latin1"), as.raw(0))), "CP1252")
})

# ---------- zero bytes ---------------------------------------------------------

test_that("zero bytes in a text CSV are dropped, not taken as binary", {
  csv <- enc_bytes(lines_text(zh_lines), "UTF-8")
  # Padding left by a crashed save or a sync tool
  padded <- write_bytes(c(csv, as.raw(rep(0, 64))))
  df <- read_csv_utf8(padded, stringsAsFactors = FALSE)
  expect_identical(strip_attrs(df), zh_ref)
  expect_identical(attr(df, "encoding"), "UTF-8")
  expect_true(detect_text_encoding(padded)$nul)
  # Padding longer than the 4 KB sniff
  long_pad <- write_bytes(c(csv, as.raw(rep(0, 10000))))
  expect_identical(strip_attrs(read_csv_utf8(long_pad, stringsAsFactors = FALSE)), zh_ref)
  # A stray zero byte inside the text, and one past the first 4 KB
  stray <- csv
  stray <- append(stray, as.raw(0), after = 40)
  expect_identical(strip_attrs(read_csv_utf8(write_bytes(stray), stringsAsFactors = FALSE)), zh_ref)
  big <- c(paste0("x", 1:600, ",", 1:600), "")
  late <- c(charToRaw(paste(c("x,y", big), collapse = "\n")))
  late <- append(late, as.raw(0), after = 5000)
  expect_identical(nrow(read_csv_utf8(write_bytes(late))), 600L)
  # 8-bit text with padding
  lat <- c(enc_bytes(lines_text(c("speaker,f0", "M\u{FC}ller,210")), "latin1"), as.raw(rep(0, 8)))
  df <- read_csv_utf8(write_bytes(lat), stringsAsFactors = FALSE)
  expect_identical(df$speaker, "M\u{FC}ller")
  expect_identical(attr(df, "encoding"), "CP1252")
  # Binary data is still refused, and a file of zero bytes too
  expect_error(read_csv_utf8(write_bytes(as.raw(rep(0, 100)))), "not a text file",
               class = "shinytone_read_error")
  set.seed(1)
  noise <- as.raw(sample(0:255, 4096, replace = TRUE))
  expect_error(read_csv_utf8(write_bytes(noise)), "not a text file",
               class = "shinytone_read_error")
})

# ---------- refused files ------------------------------------------------------

test_that("files that are not text CSVs are refused with a clear message", {
  expect_read_error <- function(bytes, pattern) {
    f <- write_bytes(bytes)
    expect_error(read_csv_utf8(f, name = "my data.csv"), class = "shinytone_read_error")
    msg <- tryCatch(read_csv_utf8(f, name = "my data.csv"), error = conditionMessage)
    expect_match(msg, "'my data.csv'", fixed = TRUE)
    expect_match(msg, pattern, fixed = TRUE)
  }
  expect_read_error(raw(0), "is empty")
  expect_read_error(bom$be, "is empty")
  expect_read_error(bom$utf8, "is empty")
  expect_read_error(c(as.raw(c(0x50, 0x4B, 0x03, 0x04)), as.raw(1:200)), "Excel workbook (.xlsx)")
  expect_read_error(c(as.raw(c(0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1)), as.raw(1:200)),
                    "Excel workbook (.xls)")
  expect_read_error(rep(as.raw(c(0x41, 0x42, 0x43, 0, 0, 0, 0x44, 0x45)), 20), "not a text file")
  # A lone UTF-16 surrogate: read.csv(fileEncoding =) would stop there and
  # return the rows before it with only a warning.
  lone <- c(bom$le, enc_bytes("a,b\n1,", "UTF-16LE"), as.raw(c(0x00, 0xD8)),
            enc_bytes("\n2,3\n", "UTF-16LE"))
  expect_read_error(lone, "not valid UTF-16LE")

  expect_error(read_csv_utf8(tempfile(), name = "gone.csv"), "'gone.csv'",
               class = "shinytone_read_error")
})

test_that("encoding arguments are refused", {
  f <- write_bytes(enc_bytes(lines_text(zh_lines), "UTF-8"))
  expect_error(read_csv_utf8(f, fileEncoding = "UTF-16"), "fileEncoding")
  expect_error(read_csv_utf8(f, encoding = "latin1"), "encoding")
})

# ---------- internal helpers ---------------------------------------------------

test_that("detect_text_encoding() returns kind, encoding and skip", {
  txt <- lines_text(zh_lines)
  det <- function(bytes) detect_text_encoding(write_bytes(bytes))
  expect_identical(det(c(bom$be, enc_bytes(txt, "UTF-16BE"))),
                   list(kind = "bom", encoding = "UTF-16BE", skip = 2L))
  expect_identical(det(c(bom$le32, enc_bytes(txt, "UTF-32LE"))),
                   list(kind = "bom", encoding = "UTF-32LE", skip = 4L))
  expect_identical(det(c(bom$utf8, enc_bytes(txt, "UTF-8"))),
                   list(kind = "bom", encoding = "UTF-8", skip = 3L))
  expect_identical(det(enc_bytes(txt, "UTF-16LE")),
                   list(kind = "nul-pattern", encoding = "UTF-16LE", skip = 0L))
  expect_identical(det(enc_bytes(txt, "UTF-8")),
                   list(kind = "plain", encoding = "UTF-8", skip = 0L))
  expect_identical(det(raw(0))$kind, "empty")
  expect_identical(det(as.raw(c(0x50, 0x4B, 0x03, 0x04, 0x00)))$kind, "zip")
})

test_that("transcode_to_utf8() never splits a surrogate pair across chunks", {
  # U+20000 needs a surrogate pair in UTF-16.
  txt <- paste0(rep("a\U{20000}b,\u{5988}\n", 7), collapse = "")
  want <- enc_bytes(txt, "UTF-8")
  for (from in c("UTF-16LE", "UTF-16BE")) {
    f <- write_bytes(enc_bytes(txt, from))
    for (chunk in 2:9) {
      out <- transcode_to_utf8(f, from, chunk = chunk)
      expect_identical(readBin(out, "raw", 1000L), want, info = paste(from, chunk))
      unlink(out)
    }
  }
  bad <- write_bytes(c(enc_bytes("a,", "UTF-16LE"), as.raw(c(0x00, 0xD8)), enc_bytes("b\n", "UTF-16LE")))
  expect_null(transcode_to_utf8(bad, "UTF-16LE"))
})

# ---------- the app's Show R code ----------------------------------------------

test_that("the app's R code snippets read a CSV as the Start tab does", {
  global <- system.file("app", "global.R", package = "shinytone")
  skip_if(!nzchar(global), "app not found")
  def <- Filter(function(e) is.call(e) && identical(e[[2]], as.name("read_data_code")),
                as.list(parse(global, keep.source = FALSE)))
  expect_length(def, 1)
  env <- new.env()
  eval(def[[1]], env)
  f <- tempfile(fileext = ".csv")
  writeLines(c("token,time,f0", "a,0,0", "a,0.01,210", "a,0.02,--undefined--"), f)
  run <- new.env()
  eval(parse(text = env$read_data_code(f)), run)
  # As raw_dataset() in server.R: --undefined-- and 0 Hz both NA
  expect_equal(run$dat$f0, c(NA, 210, NA))
  app <- zero_hz_to_na(read_csv_utf8(f, stringsAsFactors = FALSE,
                                     na.strings = c("NA", "--undefined--")))
  expect_identical(run$dat, app)
})

# With the Start-page settings (server.R's start_settings()), the code also
# repeats the discarded-token exclusion, the metadata join, the duration
# columns and the screen of rows without f0, so running it gives the rows the
# app's tabs read. The server runs in shiny::testServer() (helper-app-start.R).
test_that("read_data_code() repeats the Start-page steps in dataset()'s order", {
  fx <- start_padded_fixture()
  fx$token_dropped <- fx$token == "S2_c"
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "padded.csv"))
    # Metadata from the token names, then a duration column
    session$setInputs(meta_source = "derive", meta_key = "token", meta_sep = "_",
                      meta_colnames = "spk word")
    session$setInputs(meta_apply = 1)
    session$setInputs(dur_unit = "__token__", dur_token = "token", dur_time = "time",
                      dur_f0 = "f0")
    session$setInputs(dur_apply = 1)
    expect_true(all(c("spk", "word", "token_f0_dur", "voiced_run") %in% names(dataset())))
    expect_false("S2_c" %in% dataset()$token)
    code <- s$env$read_data_code(f, start_settings())
    steps <- c("packageVersion", "zero_hz_to_na", "token_dropped", "strsplit",
               "contour_duration", "screen_f0_rows", "drop_rows_without_f0")
    at <- vapply(steps, function(p) grep(p, code, fixed = TRUE)[1], integer(1))
    expect_false(anyNA(at))
    expect_false(is.unsorted(at))
    expect_true(any(grepl(sprintf('>= "%s"', utils::packageVersion("shinytone")), code, fixed = TRUE)))
    run <- new.env()
    eval(parse(text = code), run)
    expect_identical(run$dat, dataset())
    # Removal unticked: the code keeps the rows
    session$setInputs(screen_remove = FALSE)
    code2 <- s$env$read_data_code(f, start_settings())
    expect_false(any(grepl("drop_rows_without_f0", code2, fixed = TRUE)))
    run2 <- new.env()
    eval(parse(text = code2), run2)
    expect_identical(run2$dat, dataset())
    # The toast for the duration column counts the same data as the column
    expect_true(any(vapply(s$rec$toasts, function(t) grepl("Added token_f0_dur for 3 token", t$text), logical(1))))
  })
})

test_that("read_data_code() drops a token_dropped column that marks no token, as the app does", {
  # all_correctedf0.csv when nothing was discarded: token_dropped = FALSE on
  # every row. The app drops the column (the box is on), so the code must too.
  fx <- start_padded_fixture()
  fx$token_dropped <- FALSE
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "padded.csv"))
    expect_false("token_dropped" %in% names(dataset()))
    code <- s$env$read_data_code(f, start_settings())
    expect_true(any(grepl("(token_dropped = TRUE: no token is marked)", code, fixed = TRUE)))
    expect_true(any(grepl("dat$token_dropped <- NULL", code, fixed = TRUE)))
    run <- new.env()
    eval(parse(text = code), run)
    expect_identical(run$dat, dataset())
    # Kept when the user unticks the box (it is not drawn for this file, but
    # the choice holds as for any file)
    session$setInputs(exclude_dropped = FALSE)
    expect_true("token_dropped" %in% names(dataset()))
    code2 <- s$env$read_data_code(f, start_settings())
    expect_false(any(grepl("token_dropped <- NULL", code2, fixed = TRUE)))
    run2 <- new.env()
    eval(parse(text = code2), run2)
    expect_identical(run2$dat, dataset())
  })
})

test_that("read_data_code() repeats the screen of the bundled sample", {
  sample <- system.file("app", "www", "dc21f0_test.csv", package = "shinytone")
  skip_if(!nzchar(sample), "sample not found")
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(sample))
    code <- s$env$read_data_code(sample, start_settings())
    expect_true(any(grepl("# Every row has f0 in column f0_Hz; nothing to remove", code, fixed = TRUE)))
    run <- new.env()
    eval(parse(text = code, encoding = "UTF-8"), run)
    expect_identical(run$dat, dataset())
  })
})

test_that("read_data_code() repeats a metadata CSV join", {
  f <- start_write_csv(start_padded_fixture())
  meta <- data.frame(file = c("S1_a.wav", "s1_b.wav", "S2_a.WAV", "S2_b.wav"),
                     age = c(30, 30, 41, 41), group = c("x", "x", "y", "y"))
  mf <- start_write_csv(meta)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "padded.csv"))
    # The name is the full path here, so the code can read the file
    session$setInputs(meta_source = "csv", meta_file = start_upload(mf, mf),
                      meta_key = "token", meta_join_meta = "file", meta_strip_ext = TRUE)
    session$setInputs(meta_apply = 1)
    expect_true(all(c("age", "group") %in% names(dataset())))
    expect_equal(sum(is.na(dataset()$age)), sum(dataset()$token == "S2_c"))
    code <- s$env$read_data_code(f, start_settings())
    expect_true(any(grepl("file_path_sans_ext", code, fixed = TRUE)))
    run <- new.env()
    eval(parse(text = code), run)
    expect_identical(run$dat, dataset())
  })
})

test_that("read_data_code() without Start settings is the read and zero_hz_to_na() only", {
  global <- system.file("app", "global.R", package = "shinytone")
  skip_if(!nzchar(global), "app not found")
  env <- new.env()
  for (e in Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                   as.list(parse(global, keep.source = FALSE, encoding = "UTF-8")))) eval(e, env)
  expect_identical(env$read_data_code("f0.csv"), env$read_data_code("f0.csv", start = NULL))
  expect_length(env$read_data_code("f0.csv"), 5)
  expect_identical(env$read_data_code("f0.csv")[5], "dat <- shinytone::zero_hz_to_na(dat)")
})
