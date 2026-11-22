# ---------- assign_tier_landmarks --------------------------------------------

# A simple 4-interval tier: silence | S1 | S2 | silence
tier_t1  <- c(0.0, 0.1, 0.3, 0.6)
tier_t2  <- c(0.1, 0.3, 0.6, 0.8)
tier_lab <- c("",  "S1", "S2", "")

test_that("assign_tier_landmarks tags each time with its interval", {
  res <- assign_tier_landmarks(c(0.2, 0.45), tier_t1, tier_t2, tier_lab)
  expect_equal(res$label, c("S1", "S2"))
  expect_equal(res$start, c(0.1, 0.3))
  expect_equal(res$end,   c(0.3, 0.6))
  expect_equal(res$idx,   c(1L, 2L))     # 1-based index among labelled intervals
})

test_that("empty intervals get an NA segment index but real boundaries", {
  res <- assign_tier_landmarks(c(0.05, 0.7), tier_t1, tier_t2, tier_lab)
  expect_equal(res$label, c("", ""))
  expect_equal(res$start, c(0.0, 0.6))
  expect_equal(res$end,   c(0.1, 0.8))
  expect_true(all(is.na(res$idx)))
})

test_that("times outside the tier span return all-NA", {
  res <- assign_tier_landmarks(c(-0.1, 0.9), tier_t1, tier_t2, tier_lab)
  expect_true(all(is.na(res$label)))
  expect_true(all(is.na(res$start)))
  expect_true(all(is.na(res$idx)))
})

test_that("assign_tier_landmarks handles empty inputs", {
  expect_equal(nrow(assign_tier_landmarks(numeric(0), tier_t1, tier_t2, tier_lab)), 0L)
  res <- assign_tier_landmarks(c(0.2, 0.4), numeric(0), numeric(0), character(0))
  expect_equal(nrow(res), 2L)
  expect_true(all(is.na(res$idx)))
})

# ---------- attach_landmarks -------------------------------------------------

test_that("attach_landmarks adds <tier>/_start/_end/_i columns from a TextGrid", {
  skip_if_not_installed("rPraat")
  tg <- rPraat::tg.createNewTextGrid(0, 0.8)
  tg <- rPraat::tg.insertNewIntervalTier(tg, 1, "syllable")
  tg <- rPraat::tg.insertInterval(tg, "syllable", 0.1, 0.3, "S1")
  tg <- rPraat::tg.insertInterval(tg, "syllable", 0.3, 0.6, "S2")
  f <- tempfile(fileext = ".TextGrid")
  rPraat::tg.write(tg, f)

  df <- data.frame(token = "w1",
                   time  = c(0.2, 0.45, 0.7),
                   f0    = c(120, 130, 140),
                   stringsAsFactors = FALSE)
  audio <- data.frame(basename = "w1", tg_path = f, stringsAsFactors = FALSE)

  out <- attach_landmarks(df, audio, "syllable")
  expect_true(all(c("syllable", "syllable_start", "syllable_end", "syllable_i") %in% names(out)))
  expect_equal(out$syllable, c("S1", "S2", ""))
  expect_equal(out$syllable_i, c(1L, 2L, NA_integer_))
})

test_that("attach_landmarks is a no-op without tiers or token/time columns", {
  df <- data.frame(token = "w1", time = 0.2, f0 = 100)
  audio <- data.frame(basename = "w1", tg_path = NA_character_)
  expect_identical(attach_landmarks(df, audio, character(0)), df)
  expect_identical(attach_landmarks(data.frame(a = 1), audio, "syllable"), data.frame(a = 1))
})

# ---------- normalise_time_landmarks -----------------------------------------

test_that("normalise_time_landmarks adds within-segment and sequential time", {
  # syllable 1 over [0.0, 0.2], syllable 2 over [0.2, 0.6]
  df <- data.frame(
    time           = c(0.05, 0.10, 0.40),
    syllable_start = c(0.0,  0.0,  0.2),
    syllable_end   = c(0.2,  0.2,  0.6),
    syllable_i     = c(1L,   1L,   2L)
  )
  out <- normalise_time_landmarks(df, "time", "syllable")
  # within-segment 0-1
  expect_equal(out$syllable_t01, c(0.25, 0.50, 0.50))
  # sequential: segment 1 keeps 0-1, segment 2 shifted into 1-2
  expect_equal(out$syllable_tseq, c(0.25, 0.50, 1.50))
})

test_that("normalise_time_token rescales each token's time to 0-1", {
  df <- data.frame(
    token = c("a", "a", "a", "b", "b"),
    time  = c(0.0, 0.5, 1.0, 2.0, 2.4)
  )
  out <- normalise_time_token(df, "time", "token")
  expect_equal(out$token_t01, c(0, 0.5, 1, 0, 1))   # per-token min->0, max->1
  expect_identical(normalise_time_token(df, "time", "nope"), df)  # no-op, missing col
})

test_that("normalise_time_landmarks clamps to 0-1 and is a no-op when columns missing", {
  df <- data.frame(time = c(-1, 5), syllable_start = c(0, 0), syllable_end = c(1, 1))
  out <- normalise_time_landmarks(df, "time", "syllable")
  expect_equal(out$syllable_t01, c(0, 1))                 # clamped
  expect_equal(out$syllable_tseq, c(0, 1))                # no _i -> equals t01
  expect_identical(normalise_time_landmarks(data.frame(a = 1), "time", "syllable"),
                   data.frame(a = 1))
})

# ---------- ipa_vowel_label --------------------------------------------------

test_that("ipa_vowel_label recognises vowels with length, diacritics, diphthongs", {
  expect_true(all(ipa_vowel_label(
    c("a", "aː", "ai", "iau", "ã", "ə̃", "aʊ", "ɤ", "ɚ", "A", "AI", "@", "a51", "u2"))))
  expect_false(any(ipa_vowel_label(
    c("ang", "pa", "n", "p", "s", "", " ", NA, "aŋ", "kʰ"))))
})

# ---------- filter_interval_rows ---------------------------------------------

# Frames for two tokens over a segment tier p | a | n (token m1, monosyllable
# with coda) and a token m2 with no vowel interval; token m3 has NA landmarks
# (no TextGrid).
filter_df <- data.frame(
  token       = c(rep("m1", 5), rep("m2", 2), "m3"),
  time        = c(0.05, 0.15, 0.25, 0.35, 0.45, 0.05, 0.10, 0.05),
  f0          = c(100, 110, 120, 130, NA, 90, 95, 80),
  segment     = c("p", "a", "a", "n", "", "s", "s", NA),
  segment_i   = c(1L, 2L, 2L, 3L, NA, 1L, 1L, NA),
  stringsAsFactors = FALSE
)

test_that("vowel mode keeps only vowel-labelled frames", {
  out <- filter_interval_rows(filter_df, "segment", mode = "vowel")
  expect_equal(out$token, c("m1", "m1"))
  expect_equal(out$time, c(0.15, 0.25))
})

test_that("rhyme mode keeps labelled frames from the first vowel to token end", {
  out <- filter_interval_rows(filter_df, "segment", mode = "rhyme")
  expect_equal(out$token, c("m1", "m1", "m1"))
  expect_equal(out$segment, c("a", "a", "n"))   # vowel + coda; onset excluded
})

test_that("labels mode keeps the requested labels only", {
  out <- filter_interval_rows(filter_df, "segment", mode = "labels",
                              labels = c("a", "n"))
  expect_equal(out$segment, c("a", "a", "n"))
  out2 <- filter_interval_rows(filter_df, "segment", mode = "labels",
                               labels = " s ")
  expect_equal(out2$token, c("m2", "m2"))       # labels are trimmed
  expect_error(filter_interval_rows(filter_df, "segment", mode = "labels"),
               "non-empty")
})

test_that("NA-landmark frames (no TextGrid) are always excluded", {
  for (m in c("vowel", "rhyme")) {
    out <- filter_interval_rows(filter_df, "segment", mode = m)
    expect_false("m3" %in% out$token)
  }
})

test_that("filter_interval_rows validates its columns", {
  expect_error(filter_interval_rows(filter_df, "nope", mode = "vowel"),
               "not found")
  df2 <- filter_df[, setdiff(names(filter_df), "segment_i")]
  expect_error(filter_interval_rows(df2, "segment", mode = "rhyme"),
               "segment_i")
})

test_that("rhyme mode spans to the token end, so it is monosyllables-only", {
  # A disyllabic token p-a-n-t-a: "rhyme" runs from the FIRST vowel to the end,
  # which crosses the second syllable. Documented as a monosyllabic-only mode;
  # this pins the behaviour so the caveat stays honest.
  df <- data.frame(token = "w1", time = seq(0.1, 0.5, by = 0.1), f0 = 100:104,
                   seg = c("p", "a", "n", "t", "a"), seg_i = 1:5,
                   stringsAsFactors = FALSE)
  expect_equal(filter_interval_rows(df, "seg", "vowel")$seg, c("a", "a"))
  expect_equal(filter_interval_rows(df, "seg", "rhyme")$seg, c("a", "n", "t", "a"))
})

test_that("ipa_vowel_label accepts pinyin tone marks in both Unicode forms", {
  # NFC precomposed (what most TextGrid editors write) and NFD decomposed.
  expect_true(all(ipa_vowel_label(
    c("ā", "ǎ", "ū", "ǔ", "ē", "ě",
      "ī", "ǐ", "ō", "ǒ", "ǖ", "ǜ"))))
  expect_true(all(ipa_vowel_label(c("ā", "ǎ", "ū", "ǔ"))))
  # Whole pinyin syllables and consonant letters are still not vowels.
  expect_false(any(ipa_vowel_label(
    c("ang", "āng", "zhī", "shǔ", "ng", "ć", "č",
      "ń", "ř", "ž", "ł"))))
})

test_that("syllabic nasals count as nuclei, plain nasals do not", {
  # Tone-bearing in e.g. Cantonese (\u5514 m\u0329, \u4E94 \u014B\u0329).
  expect_true(all(ipa_vowel_label(c("m\u0329", "n\u0329", "\u014B\u0329", "m\u030D",
                                    "\u014B\u03295", "m\u0329\u02E8\u02E9"))))
  expect_false(any(ipa_vowel_label(c("m", "n", "\u014B", "ma", "am"))))
})

test_that("di- and triphthongs match, including j/w offglide spellings", {
  expect_true(all(ipa_vowel_label(
    c("ai", "au", "ei", "ou", "iau", "uai", "ɔi", "aːi", "ai̯"))))
  # Many traditions spell the offglide as a consonant letter: a glide counts
  # inside a nucleus that also contains a vowel letter.
  expect_true(all(ipa_vowel_label(
    c("aj", "aw", "ja", "jaw", "waj", "aɥ", "ɥa"))))
  # A bare glide is an onset, not a nucleus; other consonants never match.
  expect_false(any(ipa_vowel_label(
    c("j", "w", "ɥ", "jw", "ang", "an", "ai n"))))
})

# ---------- tg_tier_names -----------------------------------------------------

test_that("tg_tier_names reads long-format tier names, interval and point", {
  skip_if_not_installed("rPraat")
  tg <- rPraat::tg.createNewTextGrid(0, 0.8)
  tg <- rPraat::tg.insertNewIntervalTier(tg, 1, "syllable")
  tg <- rPraat::tg.insertNewPointTier(tg, 2, "tone_target")
  f <- tempfile(fileext = ".TextGrid")
  rPraat::tg.write(tg, f)                       # long format by default
  expect_equal(tg_tier_names(f), c("syllable", "tone_target"))
  # Names match a full parse.
  expect_equal(tg_tier_names(f), unname(vapply(rPraat::tg.read(f), `[[`, "", "name")))
})

test_that("tg_tier_names reads the short format, UTF-16 and escaped quotes", {
  short <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', "",
             "0", "0.8", "<exists>", "2",
             '"IntervalTier"', '"say ""hi"""', "0", "0.8", "1", "0", "0.8", '""',
             '"TextTier"', '"声调"', "0", "0.8", "0")
  f1 <- tempfile(fileext = ".TextGrid")
  writeLines(enc2utf8(short), f1, useBytes = TRUE)
  expect_equal(tg_tier_names(f1), c('say "hi"', "声调"))

  long <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', "",
            "xmin = 0", "xmax = 0.8", "tiers? <exists>", "size = 1", "item []:",
            "    item [1]:", '        class = "IntervalTier"', '        name = "ségment"',
            "        xmin = 0", "        xmax = 0.8", "        intervals: size = 1",
            "        intervals [1]:", "            xmin = 0", "            xmax = 0.8",
            '            text = "name = x"')
  f2 <- tempfile(fileext = ".TextGrid")
  con <- file(f2, "wb")
  writeBin(as.raw(c(0xff, 0xfe)), con)
  writeBin(iconv(paste(long, collapse = "\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]], con)
  close(con)
  expect_equal(tg_tier_names(f2), "ségment")

  expect_equal(tg_tier_names(c(f1, f2, f1, NA, "")),
               c('say "hi"', "声调", "ségment"))
})

test_that("tg_tier_names skips unreadable files", {
  expect_equal(tg_tier_names(c(tempfile(), NA, "")), character(0))
})

# ---------- read_textgrid -----------------------------------------------------

# Praat-written TextGrids. Praat saves any file with a non-ASCII character as
# UTF-16BE with a byte-order mark (its default "try ASCII, then UTF-16"); the
# *_utf8 twins are the same TextGrids saved with the UTF-8 preference.
tg_fixture <- function(name) test_path("fixtures", paste0("tg_", name, ".TextGrid"))
# Tier contents only: the class attribute carries the file name.
tg_tiers <- function(tg) lapply(unclass(tg), function(t)
  t[c("name", "type", intersect(c("t1", "t2", "t", "label"), names(t)))])

test_that("read_textgrid reads Praat's UTF-16BE files like their UTF-8 twins", {
  skip_if_not_installed("rPraat")
  expect_identical(readBin(tg_fixture("zh_long_utf16be"), "raw", 2L), as.raw(c(0xFE, 0xFF)))
  pairs <- list(c("zh_long_utf16be",     "zh_long_utf8"),
                c("zh_short_utf16be",    "zh_long_utf8"),
                c("zhname_short_utf16be", "zhname_long_utf8"),
                c("ipa_long_utf16be",    "ipa_long_utf8"),
                c("nfd_long_utf16be",    "nfd_long_utf8"))
  for (p in pairs) {
    a <- read_textgrid(tg_fixture(p[1]))
    b <- read_textgrid(tg_fixture(p[2]))
    expect_null(a$reason)
    expect_identical(a$encoding, "UTF-16BE")
    expect_identical(b$encoding, "UTF-8")
    expect_identical(tg_tiers(a$tg), tg_tiers(b$tg), label = p[1])
  }
})

test_that("read_textgrid returns Praat's labels and tier names exactly", {
  skip_if_not_installed("rPraat")
  tg <- read_textgrid(tg_fixture("zh_long_utf16be"))$tg
  expect_identical(names(tg), c("phone", "word", "tone", "event"))
  expect_identical(tg$phone$label, c("", "pʰ", "ɔ", ""))
  expect_identical(tg$word$label,  c("", "怕", ""))
  expect_identical(tg$tone$label,  c("", "阴平", ""))
  expect_identical(tg$event$label, "高")
  expect_identical(class(tg)[["name"]], "tg_zh_long_utf16be.TextGrid")

  zn <- read_textgrid(tg_fixture("zhname_short_utf16be"))$tg
  expect_identical(names(zn), c("phone", "汉字", "声调", "事件"))

  ipa <- read_textgrid(tg_fixture("ipa_long_utf16be"))$tg$phone$label
  expect_identical(ipa[-1], c("pʰ", "ã", "mā", "mā",
                              "a˥˩", "ŋ̍", "aː"))
  # Decomposed (NFD) labels are kept as written, not normalised.
  nfd <- read_textgrid(tg_fixture("nfd_long_utf16be"))$tg$phone$label
  expect_identical(nfd, c("", "ã", "mā", "mā"))
})

test_that("read_textgrid undoes Praat's doubled quotes and keeps multi-line labels", {
  skip_if_not_installed("rPraat")
  tg <- read_textgrid(tg_fixture("quote_newline_utf16be"))$tg
  expect_identical(tg$word$label[2:3],
                   c("说 \"好\"", "第一行\n第二行"))

  short <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', "",
             "0", "0.8", "<exists>", "1",
             '"IntervalTier"', '"say ""hi"""', "0", "0.8", "2",
             "0", "0.4", '"a ""b"""', "0.4", "0.8", '""')
  f <- tempfile(fileext = ".TextGrid")
  writeLines(short, f)
  expect_identical(names(read_textgrid(f)$tg), 'say "hi"')
  expect_identical(read_textgrid(f)$tg[[1]]$label, c('a "b"', ""))
  expect_identical(rPraat::tg.read(f)[[1]]$label, c('a ""b""', ""))
})

test_that("read_textgrid handles other encodings and line ends", {
  skip_if_not_installed("rPraat")
  ref <- tg_tiers(read_textgrid(tg_fixture("zh_long_utf8"))$tg)
  be  <- readBin(tg_fixture("zh_long_utf16be"), "raw", 1e5)
  txt <- rawToChar(readBin(tg_fixture("zh_long_utf8"), "raw", 1e5))
  Encoding(txt) <- "UTF-8"
  le  <- iconv(txt, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]
  variants <- list(
    "UTF-16BE" = be[-(1:2)],                                    # no BOM
    "UTF-16LE" = c(as.raw(c(0xFF, 0xFE)), le),
    "UTF-16LE" = le,                                            # no BOM
    "UTF-8"    = c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(txt)),
    "UTF-8"    = charToRaw(gsub("\n", "\r\n", txt, fixed = TRUE)))
  for (i in seq_along(variants)) {
    f <- tempfile(fileext = ".TextGrid")
    writeBin(variants[[i]], f)
    r <- read_textgrid(f)
    expect_identical(r$encoding, names(variants)[i])
    expect_identical(tg_tiers(r$tg), ref)
  }
  # An 8-bit file (Praat's "try ISO Latin-1" preference).
  lat <- read_textgrid(tg_fixture("latin1"))
  expect_null(lat$reason)
  expect_identical(lat$tg[[1]]$label[nzchar(lat$tg[[1]]$label)], "é")
})

test_that("read_textgrid says why a file cannot be read", {
  skip_if_not_installed("rPraat")
  bin <- read_textgrid(tg_fixture("binary"))
  expect_null(bin$tg)
  expect_match(bin$reason, "Praat's binary format; in Praat, use Save as text file", fixed = TRUE)
  chr <- read_textgrid(tg_fixture("chrono_utf16be"))
  expect_null(chr$tg)
  expect_match(chr$reason, "Praat's chronological format", fixed = TRUE)

  expect_match(read_textgrid(tempfile())$reason, "not found")
  empty <- tempfile(fileext = ".TextGrid")
  file.create(empty)
  expect_match(read_textgrid(empty)$reason, "empty")
  other <- tempfile(fileext = ".TextGrid")
  writeLines(c("token,f0", "a,1"), other)
  expect_match(read_textgrid(other)$reason, "not a Praat TextGrid text file")
  # A TextGrid header with a body rPraat cannot parse.
  broken <- tempfile(fileext = ".TextGrid")
  writeLines(c('File type = "ooTextFile"', 'Object class = "TextGrid"', "", "xmin = 0"), broken)
  r <- read_textgrid(broken)
  expect_null(r$tg)
  expect_match(r$reason, "could not be parsed")
})

test_that("read_textgrid matches rPraat::tg.read on UTF-8 TextGrids", {
  skip_if_not_installed("rPraat")
  # rPraat cannot open the non-ASCII file names outside a UTF-8 locale.
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "needs a UTF-8 locale")
  dirs <- test_path("..", "..", "test", c("cs_wavs", "cong_wavs"))
  files <- list.files(dirs, "\\.TextGrid$", full.names = TRUE)
  skip_if(length(files) == 0, "sample TextGrids not available")
  unquote <- function(x) if (is.character(x)) gsub('""', '"', x, fixed = TRUE) else x
  for (f in files) {
    ref <- rPraat::tg.read(f)
    for (k in seq_along(ref)) {
      ref[[k]]$name  <- unquote(ref[[k]]$name)
      ref[[k]]$label <- unquote(ref[[k]]$label)
    }
    names(ref) <- vapply(ref, `[[`, "", "name")
    r <- read_textgrid(f)
    expect_identical(r$encoding, "UTF-8")
    expect_identical(r$tg, ref, label = basename(f))
  }
})

test_that("tg_interval_tiers reads UTF-16 TextGrids and reports unreadable ones", {
  skip_if_not_installed("rPraat")
  expect_identical(tg_interval_tiers(tg_fixture("zh_long_utf16be")),
                   c("phone", "tone", "word"))
  expect_setequal(tg_interval_tiers(tg_fixture("zhname_short_utf16be")),
                  c("phone", "汉字", "声调"))

  bin <- tg_fixture("binary")
  chr <- tg_fixture("chrono_utf16be")
  none <- tg_interval_tiers(c(bin, chr))
  expect_length(none, 0)
  bad <- attr(none, "unreadable")
  expect_named(bad, c(bin, chr))
  expect_match(bad[[1]], "binary format")
  expect_match(bad[[2]], "chronological format")

  mixed <- tg_interval_tiers(c(tg_fixture("zh_long_utf16be"), bin))
  expect_equal(as.vector(mixed), c("phone", "tone", "word"))
  expect_named(attr(mixed, "unreadable"), bin)
})

test_that("tg_tier_names agrees with read_textgrid, BOM or not", {
  skip_if_not_installed("rPraat")
  f <- tg_fixture("zhname_short_utf16be")
  expect_identical(tg_tier_names(f), names(read_textgrid(f)$tg))
  nobom <- tempfile(fileext = ".TextGrid")
  writeBin(readBin(f, "raw", 1e5)[-(1:2)], nobom)
  expect_identical(tg_tier_names(nobom), names(read_textgrid(f)$tg))
  expect_identical(tg_tier_names(c(tg_fixture("binary"), tg_fixture("chrono_utf16be"))),
                   character(0))
})

test_that("attach_landmarks reads UTF-16 TextGrids and reports unreadable ones", {
  skip_if_not_installed("rPraat")
  times <- seq(0, 0.5, by = 0.01)
  df <- data.frame(token = rep(c("be", "u8", "bin"), each = length(times)),
                   time = rep(times, 3), f0 = 200)
  audio <- data.frame(basename = c("be", "u8", "bin"),
                      tg_path = c(tg_fixture("zh_long_utf16be"), tg_fixture("zh_long_utf8"),
                                  tg_fixture("binary")))
  out <- attach_landmarks(df, audio, c("phone", "word"))
  be <- out[out$token == "be", -1]
  u8 <- out[out$token == "u8", -1]
  rownames(be) <- rownames(u8) <- NULL
  expect_identical(be, u8)
  expect_true(all(!is.na(be$phone)))
  expect_setequal(unique(be$word), c("", "怕"))
  expect_true(all(is.na(out$phone[out$token == "bin"])))
  bad <- attr(out, "tg_unreadable")
  expect_named(bad, "bin")
  expect_match(bad[["bin"]], "binary format")

  # No attribute when every TextGrid could be read.
  ok <- attach_landmarks(df[df$token != "bin", ], audio, "phone")
  expect_null(attr(ok, "tg_unreadable"))
  # Numeric tokens name the reason by token, not by position.
  num <- attach_landmarks(data.frame(token = 7, time = 0.1),
                          data.frame(basename = "7", tg_path = tg_fixture("binary")),
                          "phone")
  expect_named(attr(num, "tg_unreadable"), "7")
})

# Long-format TextGrid with one interval tier, written in `enc`, no BOM.
tg_8bit <- function(labels, tier = "mot", enc = "latin1") {
  n <- length(labels)
  b <- seq(0, 1, length.out = n + 1)
  txt <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', "", "xmin = 0 ",
           "xmax = 1 ", "tiers? <exists> ", "size = 1 ", "item []: ", "    item [1]:",
           '        class = "IntervalTier" ', sprintf('        name = "%s" ', tier),
           "        xmin = 0 ", "        xmax = 1 ", sprintf("        intervals: size = %d ", n),
           unlist(lapply(seq_len(n), function(i) c(
             sprintf("        intervals [%d]:", i), sprintf("            xmin = %s ", b[i]),
             sprintf("            xmax = %s ", b[i + 1]),
             sprintf('            text = "%s" ', labels[i])))))
  f <- tempfile(fileext = ".TextGrid")
  writeBin(iconv(paste0(paste(txt, collapse = "\n"), "\n"), "UTF-8", enc, toRaw = TRUE)[[1]], f)
  f
}

test_that("8-bit TextGrids read in the right code page, with a note", {
  skip_if_not_installed("rPraat")
  # Accented letters inside words also form valid GBK pairs (Müller would
  # read as M + a rare hanzi + ler); Praat's Latin-1 files must stay Latin.
  for (labs in list(c("Müller", "Straße"), c("señor", "año"),
                    c("élève", "étés"), c("café", "ça"))) {
    f <- tg_8bit(labs)
    r <- read_textgrid(f)
    expect_identical(r$encoding, "CP1252", info = labs[1])
    expect_identical(r$tg$mot$label, labs)
    expect_match(r$note, "so it was read as Western European (Windows-1252)", fixed = TRUE)
    expect_match(r$note, sprintf("'%s' is not saved as UTF-8 or UTF-16", basename(f)), fixed = TRUE)
  }
  expect_match(read_textgrid(f, name = "ma1.TextGrid")$note, "'ma1.TextGrid' is not", fixed = TRUE)
  # A Latin-1 tier name: the fast scan used to read it so, and still does.
  f <- tg_8bit(c("a", "b"), tier = "Wörter")
  expect_identical(tg_tier_names(f), "Wörter")
  expect_identical(names(read_textgrid(f)$tg), "Wörter")

  skip_if(is.na(iconv("声", "UTF-8", "GB18030")), "GB18030 not supported by iconv here")
  gb <- read_textgrid(tg_8bit(c("声调", "mā"), tier = "声调", enc = "GB18030"))
  expect_identical(gb$encoding, "GB18030")
  expect_identical(gb$tg[[1]]$label, c("声调", "mā"))
  skip_if(is.na(iconv("聲", "UTF-8", "BIG5")), "BIG5 not supported by iconv here")
  b5 <- read_textgrid(tg_8bit(c("聲調", "音節"), tier = "聲調", enc = "BIG5"))
  expect_identical(b5$encoding, "BIG5")
  expect_identical(names(b5$tg), "聲調")
  expect_match(b5$note, "Chinese (Big5)", fixed = TRUE)

  # UTF-8 and UTF-16 need no guess, so no note.
  expect_null(read_textgrid(tg_fixture("zh_long_utf8"))$note)
  expect_null(read_textgrid(tg_fixture("zh_long_utf16be"))$note)
})

test_that("tg_interval_tiers and attach_landmarks report guessed code pages", {
  skip_if_not_installed("rPraat")
  lat <- tg_8bit(c("Müller", "Straße"))
  u8  <- tg_fixture("zh_long_utf8")
  tiers <- tg_interval_tiers(c(lat, u8))
  expect_identical(attr(tiers, "guessed"),
                   stats::setNames("Western European (Windows-1252)", lat))
  expect_null(attr(tg_interval_tiers(u8), "guessed"))

  df <- data.frame(token = rep(c("lat", "u8"), each = 3), time = rep(c(0.1, 0.3, 0.7), 2))
  audio <- data.frame(basename = c("lat", "u8"), tg_path = c(lat, u8))
  out <- suppressWarnings(attach_landmarks(df, audio, "mot"))
  expect_identical(attr(out, "tg_guessed"), c(lat = "Western European (Windows-1252)"))
  expect_identical(out$mot[1:3], c("Müller", "Müller", "Straße"))
  expect_null(attr(attach_landmarks(df[4:6, ], audio, "phone"), "tg_guessed"))
})

test_that("a UTF-8 TextGrid with a stray byte keeps its UTF-8 labels", {
  skip_if_not_installed("rPraat")
  # One label typed in Latin-1 (caf + 0xE9): reading the whole file as
  # Windows-1252 would garble the Chinese label.
  f <- tg_8bit(c("阴平", "cafX"), tier = "声调", enc = "UTF-8")
  b <- readBin(f, "raw", 1e4)
  b[b == charToRaw("X")] <- as.raw(0xE9)
  for (with_bom in c(FALSE, TRUE)) {
    writeBin(c(if (with_bom) as.raw(c(0xEF, 0xBB, 0xBF)), b), f)
    r <- read_textgrid(f, name = "ma1.TextGrid")
    expect_identical(r$encoding, "UTF-8")
    expect_identical(names(r$tg), "声调")
    expect_identical(r$tg[[1]]$label, c("阴平", "caf�"))
    expect_identical(r$stray_lines, 22L)
    expect_identical(r$note, paste("'ma1.TextGrid' is UTF-8 except on line 22, where some text",
                                   "is not valid UTF-8 and is shown as �."))
    expect_identical(tg_tier_names(f), "声调")
  }
  tiers <- tg_interval_tiers(f)
  expect_identical(attr(tiers, "stray"), stats::setNames("line 22", f))
  expect_null(attr(tiers, "guessed"))
  out <- attach_landmarks(data.frame(token = "ma1", time = c(0.2, 0.7)),
                          data.frame(basename = "ma1", tg_path = f), "声调")
  expect_identical(out[["声调"]], c("阴平", "caf�"))
  expect_identical(attr(out, "tg_stray"), c(ma1 = "line 22"))
  expect_null(attr(out, "tg_guessed"))
  # A file with a UTF-8 mark that is mostly not UTF-8 is still refused.
  lat <- readBin(tg_8bit(c("Müller", "Straße")), "raw", 1e4)
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), lat), f)
  expect_match(read_textgrid(f)$reason, "not valid UTF-8", fixed = TRUE)
})

test_that("a TextGrid with zero-byte padding reads", {
  skip_if_not_installed("rPraat")
  f <- tempfile(fileext = ".TextGrid")
  writeBin(c(readBin(tg_fixture("zh_long_utf8"), "raw", 1e5), as.raw(rep(0, 32))), f)
  expect_identical(tg_tiers(read_textgrid(f)$tg),
                   tg_tiers(read_textgrid(tg_fixture("zh_long_utf8"))$tg))
  # Padding that starts past the first 4 KB
  long <- tempfile(fileext = ".TextGrid")
  writeBin(c(readBin(tg_fixture("zh_long_utf8"), "raw", 1e5), charToRaw(strrep(" ", 5000)),
             as.raw(rep(0, 32))), long)
  expect_identical(tg_tiers(read_textgrid(long)$tg),
                   tg_tiers(read_textgrid(tg_fixture("zh_long_utf8"))$tg))
})

test_that("plain UTF-8 TextGrids go to rPraat as they are, others via a UTF-8 copy", {
  skip_if_not_installed("rPraat")
  direct <- function(f) isTRUE(attr(tg_decode_text(f), "direct"))
  expect_true(direct(tg_fixture("zh_long_utf8")))
  expect_false(direct(tg_fixture("zh_long_utf16be")))
  expect_false(direct(tg_8bit("café")))
  # The old short header has the same layout but rPraat wants the new one.
  short <- tempfile(fileext = ".TextGrid")
  writeLines(c('File type = "ooTextFile short"', '"TextGrid"', "", "0", "0.8", "<exists>", "1",
               '"IntervalTier"', '"w"', "0", "0.8", "1", "0", "0.8", '"a"'), short)
  expect_false(direct(short))
  expect_identical(read_textgrid(short)$tg$w$label, "a")
  # Old Mac line ends (CR only)
  cr <- tempfile(fileext = ".TextGrid")
  writeBin(charToRaw(gsub("\n", "\r", rawToChar(readBin(tg_fixture("zh_long_utf8"), "raw", 1e5)),
                          fixed = TRUE)), cr)
  expect_false(direct(cr))
  expect_identical(tg_tiers(read_textgrid(cr)$tg),
                   tg_tiers(read_textgrid(tg_fixture("zh_long_utf8"))$tg))
})

# ---------- landmark_prefixes -------------------------------------------------

# One-token TextGrid with the given interval tiers; tier k is labelled
# "<k>a" over 0.1-0.3 s and "<k>b" over 0.3-0.6 s.
tg_with_tiers <- function(tiers) {
  tg <- rPraat::tg.createNewTextGrid(0, 0.8)
  for (k in seq_along(tiers)) {
    tg <- rPraat::tg.insertNewIntervalTier(tg, k, tiers[k])
    tg <- rPraat::tg.insertInterval(tg, k, 0.1, 0.3, paste0(k, "a"))
    tg <- rPraat::tg.insertInterval(tg, k, 0.3, 0.6, paste0(k, "b"))
  }
  f <- tempfile(fileext = ".TextGrid")
  rPraat::tg.write(tg, f)
  f
}
lm_df    <- data.frame(token = "w1", time = c(0.2, 0.45, 0.7), f0 = c(120, 130, 140))
lm_audio <- function(f) data.frame(basename = "w1", tg_path = f)

test_that("landmark_prefixes keeps tier names in any script", {
  zh <- c("声调", "音节")
  expect_identical(landmark_prefixes(zh), stats::setNames(zh, zh))
  expect_identical(landmark_prefixes(rev(zh)), stats::setNames(rev(zh), rev(zh)))
  ipa <- c("ʂəŋtjɑʊ", "tʰa", "ʔa")
  expect_identical(landmark_prefixes(ipa), stats::setNames(ipa, ipa))
  expect_identical(unname(landmark_prefixes(c("Tone 声调", "声调 (2)", "!!"))),
                   c("tone_声调", "声调_2", "tier"))
  # A tier named like another tier's column does not take it.
  expect_identical(unname(landmark_prefixes(c("声调_start", "声调"))),
                   c("声调_start_2", "声调"))
})

test_that("tiers with the same prefix are numbered whatever the order", {
  tiers <- c("syllable", "Syllable", "syllable!")
  want  <- c(syllable = "syllable", Syllable = "syllable_2", "syllable!" = "syllable_3")
  for (o in list(1:3, 3:1, c(2L, 3L, 1L))) {
    expect_identical(landmark_prefixes(tiers[o]), want[tiers[o]])
  }
  # A subset gets the same prefixes when all_tiers is the same.
  for (t in tiers) expect_identical(landmark_prefixes(t, all_tiers = rev(tiers)), want[t])
  expect_identical(landmark_prefixes(c("syllable!", "Syllable"), all_tiers = tiers),
                   want[c("syllable!", "Syllable")])
  # Without the corpus's tiers, a lone tier keeps the plain prefix.
  expect_identical(landmark_prefixes("Syllable"), c(Syllable = "syllable"))
})

test_that("tiers named after core columns get _2 and leave them intact", {
  expect_identical(landmark_prefixes(c("time", "token", "f0", "Time")),
                   c(time = "time_2", token = "token_2", f0 = "f0_2", Time = "time_3"))
  skip_if_not_installed("rPraat")
  f <- tg_with_tiers(c("time", "token", "f0"))
  out <- attach_landmarks(lm_df, lm_audio(f), c("time", "token", "f0"))
  expect_identical(out$token, lm_df$token)
  expect_identical(out$time, lm_df$time)
  expect_identical(out$f0, lm_df$f0)
  expect_equal(out$time_2, c("1a", "1b", ""))
  expect_equal(out$token_2, c("2a", "2b", ""))
  expect_equal(out$f0_2, c("3a", "3b", ""))
  expect_identical(attr(out, "renamed_tiers"),
                   c(time = "time_2", token = "token_2", f0 = "f0_2"))
})

test_that("ASCII tier names keep the column names of earlier versions", {
  old <- function(nm) {   # the rule before landmark_prefixes()
    p <- tolower(gsub("[^A-Za-z0-9]+", "_", nm)); p <- gsub("^_+|_+$", "", p)
    if (!nzchar(p)) "tier" else p
  }
  asc <- c("syllable", "Syllable (broad)", "phones", "words", "rhyme", "Tone-2",
           "1st", " vowel ", "Seg.Tier", "!!!")
  expect_identical(unname(landmark_prefixes(asc)), unname(vapply(asc, old, "")))
  # Every three-character name from a mix of letters, digits and symbols, on
  # its own, unless it clashes with a core column.
  ch  <- c("a", "Z", "0", "_", "-", " ", "!", ".")
  nms <- apply(expand.grid(ch, ch, ch, stringsAsFactors = FALSE), 1, paste, collapse = "")
  nms <- nms[!vapply(nms, old, "") %in% eval(formals(landmark_prefixes)$reserved)]
  new <- vapply(nms, function(x) landmark_prefixes(x)[[1]], "")
  expect_identical(unname(new), unname(vapply(nms, old, "")))
})

test_that("prefixes from CJK, IPA and Thai names are syntactic", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "needs a UTF-8 locale")
  nms <- c("声调", "音节", "성조", "ʂəŋtjɑʊ", "tʰa", "ˈsyl", "ʔa",
           "ɕɥɛ̃", "n̩", "ã", "a¹",
           "วรรณยุกต์",   # Thai, with marks
           "स्वर")                                  # Devanagari
  p <- landmark_prefixes(nms)
  expect_identical(make.names(p), unname(p))
  expect_length(unique(p), length(nms))
})

test_that("landmark_prefixes puts names in NFC before dropping marks", {
  skip_if_not_installed("utf8")
  expect_identical(unname(landmark_prefixes("ã")), "ã")
  expect_identical(unname(landmark_prefixes(c("ã", "ã"))),
                   c("ã_2", "ã"))
})

test_that("attach_landmarks adds a column set for each Chinese tier", {
  skip_if_not_installed("rPraat")
  df <- data.frame(token = "zh", time = c(0.03, 0.1, 0.3, 0.5), f0 = 200)
  cols <- paste0(rep(c("汉字", "声调"), each = 4), c("", "_start", "_end", "_i"))
  for (f in c(tg_fixture("zhname_long_utf8"), tg_fixture("zhname_short_utf16be"))) {
    audio <- data.frame(basename = "zh", tg_path = f)
    for (tiers in list(c("汉字", "声调"), c("声调", "汉字"))) {
      out <- attach_landmarks(df, audio, tiers, all_tiers = c("phone", "汉字", "声调"))
      expect_setequal(setdiff(names(out), names(df)), cols)
      expect_equal(out[["汉字"]], c("", "怕", "怕", ""))
      expect_equal(out[["声调"]], c("", "阴平", "阴平", ""))
      expect_equal(out[["声调_start"]], c(0, 0.063, 0.063, 0.442))
      expect_equal(out[["声调_i"]], c(NA, 1L, 1L, NA))
      expect_null(attr(out, "renamed_tiers"))
      expect_null(attr(out, "missing_tiers"))
    }
  }
})

test_that("attach_landmarks reports renamed tiers and tiers in no TextGrid", {
  skip_if_not_installed("rPraat")
  f <- tg_with_tiers(c("syllable", "Syllable"))
  expect_warning(
    out <- attach_landmarks(lm_df, lm_audio(f), c("Syllable", "音节"),
                            all_tiers = c("syllable", "Syllable")),
    "No TextGrid has an interval tier")
  expect_equal(out$syllable_2, c("2a", "2b", ""))
  expect_false("syllable" %in% names(out))
  expect_true(all(is.na(out[["音节"]])))
  expect_identical(attr(out, "renamed_tiers"), c(Syllable = "syllable_2"))
  expect_identical(attr(out, "missing_tiers"), "音节")

  # No such warning when no TextGrid could be read at all.
  expect_no_warning(bin <- attach_landmarks(lm_df, lm_audio(tg_fixture("binary")), "音节"))
  expect_null(attr(bin, "missing_tiers"))
})

test_that("renamed_tiers lists only tiers given _2 or the 'tier' fallback", {
  no_tg <- data.frame(basename = "w1", tg_path = NA_character_)
  nfd   <- intToUtf8(c(0x61, 0x303))   # "a" + combining tilde; NFC is one letter
  # Lowercasing, "_" for other characters and NFC are not reported.
  tiers <- c("Syllable", "Tone", "Syllable (broad)", nfd, "tier", "Tier 2")
  out <- attach_landmarks(lm_df, no_tg, tiers)
  expect_null(attr(out, "renamed_tiers"))
  # No TextGrid matched: the columns are still added, all NA.
  for (p in c("syllable", "tone", "syllable_broad", "tier", "tier_2")) {
    expect_true(all(is.na(out[[p]])), label = p)
    expect_true(all(is.na(out[[paste0(p, "_i")]])), label = p)
  }
  expect_identical(out[names(lm_df)], lm_df)

  out <- attach_landmarks(lm_df, no_tg, c("Syllable", "syllable", "!!!", "tier", "Time"))
  expect_identical(attr(out, "renamed_tiers"),
                   c(Syllable = "syllable_2", "!!!" = "tier_2", Time = "time_2"))
  expect_identical(attr(attach_landmarks(lm_df, no_tg, "!!!"), "renamed_tiers"),
                   c("!!!" = "tier"))
})
