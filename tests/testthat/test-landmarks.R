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
