# =============================================================================
# Tests for flag_outliers(), flag_pitch_jumps(), and inspect_f0().
# =============================================================================

# ---------- flag_outliers ----------------------------------------------------

test_that("flag_outliers returns one row per token with expected columns", {
  df <- data.frame(
    token   = rep(c("t1", "t2", "t3"), each = 4),
    speaker = rep("S01", 12),
    f0      = c(150, 160, 155, 158,
                152, 159, 154, 157,
                153, 161, 156, 160)
  )
  out <- flag_outliers(df, z_threshold = 3)

  expect_equal(nrow(out), 3)
  expect_true(all(c("f0_token_max", "f0_token_min", "f0_token_mean",
                    "f0_token_sd", "z_max", "z_min",
                    "flag_too_high", "flag_too_low") %in% names(out)))
})

test_that("flag_outliers flags an obvious tracking error", {
  df <- data.frame(
    token   = rep(c("t1", "t2", "t3", "t4"), each = 4),
    speaker = rep("S01", 16),
    f0      = c(150, 160, 155, 158,
                152, 159, 154, 157,
                153, 161, 156, 160,
                400, 420, 410, 415)   # clearly off
  )
  out <- flag_outliers(df, z_threshold = 1)
  flagged_token <- out$token[out$flag_too_high]
  expect_true("t4" %in% flagged_token)
})

test_that("flag_outliers ignores zero-valued (unvoiced) f0", {
  df <- data.frame(
    token   = c("t1", "t1", "t1", "t1"),
    speaker = c("S01", "S01", "S01", "S01"),
    f0      = c(0, 150, 0, 160)
  )
  out <- flag_outliers(df)
  expect_equal(out$f0_token_min, 150)   # 0 excluded
  expect_equal(out$f0_token_max, 160)
})


# ---------- flag_pitch_jumps -------------------------------------------------

test_that("flag_pitch_jumps detects an obvious octave doubling", {
  df <- data.frame(
    token = rep("t1", 5),
    time  = seq(0, 0.04, by = 0.01),
    f0    = c(150, 152, 305, 153, 151)   # frame 3 is doubled
  )
  out <- flag_pitch_jumps(df, time_unit = "s")
  # The landing sample (index 3) should be flagged.
  expect_true(out$flagged_jump[3])
  expect_match(out$jump_note[3], "octave|jump")
})

test_that("flag_pitch_jumps respects rise/fall thresholds", {
  # A single-sample upward spike on an otherwise flat 150 Hz contour
  # (10 ms steps). The +6 ST step into frame 3 and the -6 ST step out of
  # it both far exceed the rise/fall thresholds, and frame 3 is the lone
  # outlier (farthest from the token median), so it is the sample flagged.
  spike <- c(150, 150, 150 * 2^0.5, 150, 150)   # frame 3 is +6 ST, then back
  df <- data.frame(
    token = rep("t1", 5),
    time  = seq(0, 0.04, by = 0.01),
    f0    = spike
  )
  out <- flag_pitch_jumps(df, time_unit = "s", carryover_mult = 0)
  expect_true(out$flagged_jump[3])
  expect_false(out$flagged_jump[2])
  expect_false(out$flagged_jump[4])

  # A gentle ramp of +1 ST per 10 ms stays under rise_threshold = 1.263
  # and must not be flagged anywhere.
  ramp <- 150 * 2^((0:4) / 12)
  df2 <- data.frame(
    token = rep("t1", 5),
    time  = seq(0, 0.04, by = 0.01),
    f0    = ramp
  )
  out2 <- flag_pitch_jumps(df2, time_unit = "s", carryover_mult = 0)
  expect_false(any(out2$flagged_jump))
})

test_that("flag_pitch_jumps picks the side farther from token median", {
  # Artefact at the START of the token (e.g. pitch doubling on frame 1).
  # Frame 1 is the outlier; frames 2..6 are the real signal.
  df <- data.frame(
    token = rep("t1", 6),
    time  = seq(0, 0.05, by = 0.01),
    f0    = c(400, 200, 200, 202, 198, 201)
  )
  out <- flag_pitch_jumps(df, time_unit = "s", carryover_mult = 0)
  # Median-aware flag placement should land on frame 1 (the outlier),
  # NOT frame 2 (which is the correct value the contour falls back to).
  expect_true(out$flagged_jump[1])
  expect_false(out$flagged_jump[2])

  # Same dataset reversed: artefact at the END of the token.
  # Frame 6 is the outlier; frames 1..5 are the real signal.
  df2 <- data.frame(
    token = rep("t1", 6),
    time  = seq(0, 0.05, by = 0.01),
    f0    = c(201, 198, 202, 200, 200, 400)
  )
  out2 <- flag_pitch_jumps(df2, time_unit = "s", carryover_mult = 0)
  expect_true(out2$flagged_jump[6])
  expect_false(out2$flagged_jump[5])

  # Artefact in the MIDDLE: a single bad sample surrounded by good ones.
  # Both the prev->middle and middle->next pairs trigger detection, and
  # both should flag the middle (median-farther) sample, not the
  # neighbours.
  df3 <- data.frame(
    token = rep("t1", 7),
    time  = seq(0, 0.06, by = 0.01),
    f0    = c(200, 198, 202, 400, 199, 201, 200)
  )
  out3 <- flag_pitch_jumps(df3, time_unit = "s", carryover_mult = 0)
  expect_true(out3$flagged_jump[4])
  expect_false(out3$flagged_jump[3])
  expect_false(out3$flagged_jump[5])
})

test_that("flag_pitch_jumps handles f0 == 0 as unvoiced", {
  df <- data.frame(
    token = rep("t1", 5),
    time  = seq(0, 0.04, by = 0.01),
    f0    = c(150, 0, 0, 152, 151)
  )
  expect_no_error(flag_pitch_jumps(df, time_unit = "s"))
})

# Gap-aware jump check (A2). Frame times are written to 4 decimals, as a
# Praat export writes them, so the time grid is regular and gaps can be read
# from the time steps.
.frames <- function(k, from = 0.0125) round(from + (k - 1) * 0.01, 4)

# Token "a": a contour ending in a steep fall (8 samples), a gap of two rows
# without f0, then a 2-sample fragment that sits nearer the token median.
# Token "b": an octave spike in the middle and a gap of 0 Hz rows. Both have
# rows without f0 at their edges (NA, 0 and the marker -1).
.jump_fixture <- function() {
  a_f0 <- c(NA, NA, NA, 300, 300, 300, 300, 290, 280, 260, 240, NA, NA,
            330, 330, NA, NA)
  b_f0 <- c(0, 0, 150, 152, 305, 153, 151, 150, 0, 0, 0, 149, 148, 147,
            -1, -1)
  data.frame(
    token = c(rep("a", length(a_f0)), rep("b", length(b_f0))),
    time  = c(.frames(seq_along(a_f0)), .frames(seq_along(b_f0))),
    f0    = c(a_f0, b_f0)
  )
}

# Flags and notes on the rows with f0 and a time, keyed by token and time.
.jump_sig <- function(out) {
  ok <- !is.na(out$f0) & out$f0 > 0 & !is.na(out$time)
  s <- data.frame(key  = paste(out$token, format(out$time, nsmall = 4)),
                  flag = out$flagged_jump, note = out$jump_note)[ok, ]
  s <- s[order(s$key), ]
  rownames(s) <- NULL
  s
}

test_that("flag_pitch_jumps gives the same flags with or without rows without f0", {
  full <- .jump_fixture()
  ref  <- flag_pitch_jumps(full)
  expect_true(any(ref$flagged_jump))

  # Rows without f0 removed (NA, 0 and -1 alike).
  none <- full[!is.na(full$f0) & full$f0 > 0, ]
  expect_identical(.jump_sig(flag_pitch_jumps(none)), .jump_sig(ref))

  # Only the edges trimmed.
  trim <- full[-c(1:3, 16:17, 18:19, 32:33), ]
  expect_identical(.jump_sig(flag_pitch_jumps(trim)), .jump_sig(ref))

  # Shuffled rows.
  set.seed(1)
  shuf <- full[sample(nrow(full)), ]
  expect_identical(.jump_sig(flag_pitch_jumps(shuf)), .jump_sig(ref))

  # An extra row with NA time takes no part and is never flagged.
  natime <- rbind(full, data.frame(token = "a", time = NA, f0 = 600))
  out <- flag_pitch_jumps(natime)
  expect_identical(.jump_sig(out), .jump_sig(ref))
  expect_false(out$flagged_jump[is.na(out$time)])
  expect_identical(out$jump_note[is.na(out$time)], "")

  # Rows without f0 are never flagged and carry an empty note.
  no_f0 <- is.na(ref$f0) | ref$f0 <= 0
  expect_false(any(ref$flagged_jump[no_f0]))
  expect_true(all(ref$jump_note[no_f0] == ""))
})

test_that("flag_pitch_jumps handles negative markers and NA time without error", {
  df <- data.frame(token = "t1", time = c(0, 0.01, 0.02, 0.03, 0.04),
                   f0 = c(200, 201, -1, -1, 202))
  expect_no_error(out <- flag_pitch_jumps(df))
  expect_false(any(out$flagged_jump))
  expect_no_error(inspect_f0(cbind(df, speaker = "S1", tone = "T1")))

  df2 <- data.frame(token = "t1", time = c(0, 0.01, NA, 0.03, 0.04),
                    f0 = c(200, 201, 205, 203, 202))
  expect_no_error(out2 <- flag_pitch_jumps(df2))
  expect_false(any(out2$flagged_jump))
})

test_that("a jump across a gap says so in its note", {
  out <- flag_pitch_jumps(.jump_fixture())
  a <- out[out$token == "a" & !is.na(out$f0), ]
  # 240 Hz -> 330 Hz over a 30 ms step: a rise across a 20 ms gap.
  expect_identical(a$jump_note[a$f0 == 330][1],
                   "jump (rise) across a 20 ms gap")
  # Jumps between neighbouring frames get no gap note.
  b <- out[out$token == "b", ]
  expect_false(any(grepl("gap", b$jump_note)))
  expect_true(any(grepl("^octave jump", b$jump_note)))

  # The same in milliseconds.
  ms <- .jump_fixture()
  ms$time <- ms$time * 1000
  out_ms <- flag_pitch_jumps(ms, time_unit = "ms")
  expect_identical(.jump_sig(out_ms)$note, .jump_sig(out)$note)

  # Times rounded to 2 decimals: the grid is not regular enough to read
  # gaps from the time steps, so there is no gap note. The jump is still
  # found, scaled by the real step.
  rough <- .jump_fixture()
  rough$time <- round(rough$time, 2)
  out_r <- flag_pitch_jumps(rough)
  expect_false(any(grepl("gap", out_r$jump_note)))
  expect_true(any(out_r$flagged_jump[out_r$token == "a"]))
})

test_that("across a gap the flag goes on a short fragment (at most 5 samples)", {
  out <- flag_pitch_jumps(.jump_fixture())
  a <- out[out$token == "a" & !is.na(out$f0), ]
  # The 2-sample fragment is flagged (with its carryover), not the end of
  # the main contour, although 240 Hz is farther from the token median.
  expect_identical(a$flagged_jump, c(rep(FALSE, 8), TRUE, TRUE))
  expect_identical(a$jump_note[10], "carryover")

  # Without the rule the side farther from the median would be flagged.
  old <- detect_token_artefacts(a$f0, a$time, "s", 1.263, 1.714,
                                c(0.49, 1.99), 1.5, max_fragment = 0L)
  expect_identical(old$flagged_jump, c(rep(FALSE, 6), TRUE, TRUE, FALSE, FALSE))
  expect_identical(old$jump_note[8], "jump (rise) across a 20 ms gap")

  # A fragment of 6 samples is past the limit: the median rule applies.
  f0_6 <- c(300, 300, 300, 300, 290, 280, 260, 240, rep(330, 6))
  t_6  <- c(.frames(1:8), .frames(11:16))
  six <- flag_pitch_jumps(data.frame(token = "c", time = t_6, f0 = f0_6),
                          carryover_mult = 0)
  expect_identical(which(six$flagged_jump), 8L)
  no_limit <- detect_token_artefacts(f0_6, t_6, "s", 1.263, 1.714,
                                     c(0.49, 1.99), 0, max_fragment = Inf)
  expect_identical(which(no_limit$flagged_jump), 9L)

  # Runs of the same length: the median rule applies (220 Hz is farther
  # from the token median than 300 Hz).
  eq <- flag_pitch_jumps(
    data.frame(token = "d", time = .frames(c(1:4, 7:10)),
               f0 = c(250, 240, 230, 220, 300, 290, 280, 270)),
    carryover_mult = 0)
  expect_identical(which(eq$flagged_jump), 4L)
  expect_identical(eq$jump_note[4], "jump (rise) across a 20 ms gap")
})

test_that("flag_pitch_jumps carryover_mult = 0 disables carryover", {
  df <- data.frame(
    token = rep("t1", 6),
    time  = seq(0, 0.05, by = 0.01),
    f0    = c(150, 152, 305, 303, 300, 152)
  )
  with_carry    <- flag_pitch_jumps(df, time_unit = "s", carryover_mult = 1.5)
  without_carry <- flag_pitch_jumps(df, time_unit = "s", carryover_mult = 0)
  # Disabling carryover should never produce MORE flags.
  expect_lte(sum(without_carry$flagged_jump),
             sum(with_carry$flagged_jump))
})


# ---------- inspect_f0 -------------------------------------------------------

test_that("inspect_f0 returns the expected columns", {
  df <- data.frame(
    token   = rep(c("t1", "t2"), each = 5),
    time    = rep(seq(0, 0.04, by = 0.01), 2),
    f0      = c(150, 160, 155, 158, 159,
                152, 159, 154, 157, 158),
    speaker = rep("S01", 10),
    tone    = rep("T1",  10)
  )
  out <- inspect_f0(df)
  expect_true(all(c("token", "time", "f0", "speaker", "tone",
                    "f0_token_max", "f0_token_min",
                    "f0_token_mean", "f0_token_sd",
                    "flagged_token", "flagged_jump",
                    "flag_notes") %in% names(out)))
  expect_equal(nrow(out), 10)   # long format preserved
})

test_that("inspect_f0 errors on missing columns", {
  df <- data.frame(token = "t1", time = 0, f0 = 150, speaker = "S01", tone = "T1")
  expect_error(inspect_f0(df, f0 = "nonexistent"),
               "Column.*not found")
})

test_that("inspect_f0(tone = NULL) skips the level check and omits the tone column", {
  # No tone column at all (the pre-tone-discovery case). One clear pooled
  # outlier (t4 ~ 400 Hz vs ~150 Hz peers); all tokens smooth (no jumps).
  mk <- function(tok, base) data.frame(
    token   = tok, time = seq(0, 0.04, by = 0.01),
    f0      = base + c(0, 1, 0, -1, 0),
    speaker = "S01")
  df <- do.call(rbind, list(mk("t1", 150), mk("t2", 152),
                            mk("t3", 149), mk("t4", 400)))

  out <- inspect_f0(df, tone = NULL, z_threshold = 1)

  # tone column omitted; level check never ran.
  expect_false("tone" %in% names(out))
  expect_false(any(grepl("level too", out$flag_notes)))
  # the two tone-free screens still run, report, and flag.
  expect_true(all(c("flagged_token", "flagged_jump", "flag_notes",
                    "f0_token_max", "f0_token_min") %in% names(out)))
  expect_true(all(out$flagged_token[out$token == "t4"]))
  expect_true(any(grepl("max too high", out$flag_notes[out$token == "t4"])))
})

test_that("inspect_f0(tone = NULL) drops a stray tone column from the output", {
  df <- data.frame(
    token   = rep(c("t1", "t2"), each = 5),
    time    = rep(seq(0, 0.04, by = 0.01), 2),
    f0      = c(150, 160, 155, 158, 159, 152, 159, 154, 157, 158),
    speaker = rep("S01", 10),
    tone    = rep("T1",  10))
  out <- inspect_f0(df, tone = NULL)
  expect_false("tone" %in% names(out))
  expect_equal(nrow(out), 10)
})


# ---------- flag_level_outliers ----------------------------------------------

# Helper: build a token of 3 samples around `med` Hz.
.lvl_token <- function(tok, tone, med, speaker = "S01") {
  data.frame(token = tok, speaker = speaker, tone = tone,
             f0 = c(med - 1, med, med + 1))
}

test_that("flag_level_outliers returns one row per voiced token with expected columns", {
  df <- do.call(rbind, list(
    .lvl_token("t1", "T1", 145), .lvl_token("t2", "T1", 148),
    .lvl_token("t3", "T1", 150), .lvl_token("t4", "T1", 152),
    .lvl_token("t5", "T1", 155), .lvl_token("t6", "T1", 250)
  ))
  out <- flag_level_outliers(df)
  expect_equal(nrow(out), 6)
  expect_true(all(c("f0_token_median", "level_st", "n_tone_peers",
                    "level_modz", "flag_level_high", "flag_level_low")
                  %in% names(out)))
})

test_that("flag_level_outliers flags a token shifted from its same-tone peers", {
  df <- do.call(rbind, list(
    .lvl_token("t1", "T1", 145), .lvl_token("t2", "T1", 148),
    .lvl_token("t3", "T1", 150), .lvl_token("t4", "T1", 152),
    .lvl_token("t5", "T1", 155), .lvl_token("t6", "T1", 250)
  ))
  out <- flag_level_outliers(df)
  expect_true(out$flag_level_high[out$token == "t6"])
  expect_false(any(out$flag_level_high[out$token != "t6"]))
  expect_false(any(out$flag_level_low))
})

test_that("flag_level_outliers does not run below min_tokens", {
  df <- do.call(rbind, list(
    .lvl_token("t1", "T1", 148), .lvl_token("t2", "T1", 150),
    .lvl_token("t3", "T1", 152), .lvl_token("t4", "T1", 250)  # 4 tokens
  ))
  # Default min_tokens is 5, so a 4-token group is skipped entirely.
  out <- flag_level_outliers(df)
  expect_true(all(is.na(out$level_modz)))
  expect_false(any(out$flag_level_high))

  # Lowering the minimum to 4 lets the same data run and flag the outlier.
  out4 <- flag_level_outliers(df, min_tokens = 4)
  expect_true(out4$flag_level_high[out4$token == "t4"])
})

test_that("flag_level_outliers compares within tone, not across tones", {
  # T1 sits ~150 Hz, T3 ~250 Hz. A T1 token mis-tracked up to 250 is
  # perfectly normal for the speaker overall (T3 lives there) but is an
  # outlier *within* T1.
  df <- do.call(rbind, c(
    list(.lvl_token("a1", "T1", 145), .lvl_token("a2", "T1", 148),
         .lvl_token("a3", "T1", 150), .lvl_token("a4", "T1", 152),
         .lvl_token("a5", "T1", 155), .lvl_token("a_bad", "T1", 250)),
    list(.lvl_token("b1", "T3", 245), .lvl_token("b2", "T3", 248),
         .lvl_token("b3", "T3", 250), .lvl_token("b4", "T3", 252),
         .lvl_token("b5", "T3", 255))
  ))
  out <- flag_level_outliers(df)
  expect_true(out$flag_level_high[out$token == "a_bad"])
  # The genuine T3 tokens (also ~250 Hz) are normal for their tone.
  expect_false(any(out$flag_level_high[grepl("^b", out$token)]))
})


# ---------- inspect_f0: level layer ------------------------------------------

test_that("inspect_f0 catches a smooth, within-speaker-normal token that is off for its tone", {
  mk <- function(tok, tone, base) data.frame(
    token = tok, time = seq(0, 0.04, by = 0.01),
    f0 = base + c(0, 1, 0, -1, 0),         # smooth: 1 Hz steps, no jumps
    speaker = "S01", tone = tone)
  df <- do.call(rbind, c(
    list(mk("a1", "T1", 145), mk("a2", "T1", 148), mk("a3", "T1", 150),
         mk("a4", "T1", 152), mk("a5", "T1", 155),
         mk("a_bad", "T1", 250)),           # mis-tracked up into T3 range
    list(mk("b1", "T3", 245), mk("b2", "T3", 248), mk("b3", "T3", 250),
         mk("b4", "T3", 252), mk("b5", "T3", 255))
  ))
  out <- inspect_f0(df)
  bad <- out[out$token == "a_bad", ]
  expect_true(all(bad$flagged_token))
  expect_false(any(bad$flagged_jump))                        # no sample jumps
  expect_true(any(grepl("level too high", bad$flag_notes)))  # caught by level
  expect_false(any(grepl("max too high",  bad$flag_notes)))  # not by pooled max
})


# ---------- flag_low_intensity -----------------------------------------------

test_that("flag_low_intensity flags quiet edge frames, not the loud core", {
  df <- data.frame(
    token     = rep("t1", 5),
    f0        = c(120, 122, 121, 119, 118),
    intensity = c(45, 62, 64, 63, 46)   # peak 64; the two edges sit ~18 dB down
  )
  out <- flag_low_intensity(df, intensity_drop = 15)
  expect_equal(out$flag_low_intensity, c(TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("flag_low_intensity ignores unvoiced and missing-intensity frames", {
  df <- data.frame(
    token     = rep("t1", 5),
    f0        = c(0, 122, NA, 119, 118),   # frames 1, 3 unvoiced
    intensity = c(20, 62, 64, NA, 40)      # frame 4 intensity missing
  )
  # Peak over voiced + intensity-present frames (2 and 5) is 62; only the
  # quiet voiced frame 5 (40 < 62 - 15) is flagged.
  out <- flag_low_intensity(df, intensity_drop = 15)
  expect_equal(out$flag_low_intensity, c(FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("flag_low_intensity leaves a token with no voiced sample unflagged", {
  df <- data.frame(
    token     = rep("t1", 3),
    f0        = c(0, NA, 0),
    intensity = c(30, 31, 29)
  )
  out <- flag_low_intensity(df)
  expect_false(any(out$flag_low_intensity))
})

test_that("flag_low_intensity errors on a non-numeric intensity column", {
  df <- data.frame(token = "t1", f0 = 120, intensity = "loud")
  expect_error(flag_low_intensity(df), "must be numeric")
})


# ---------- inspect_f0: intensity layer --------------------------------------

test_that("inspect_f0 adds intensity columns and note only when intensity is supplied", {
  mk <- function(tok, base, int) data.frame(
    token = tok, time = seq(0, 0.04, by = 0.01),
    f0 = base + c(0, 1, 0, -1, 0),
    speaker = "S01", tone = "T1",
    intensity = int)
  df <- do.call(rbind, list(
    mk("a1", 150, c(60, 62, 63, 62, 61)),
    mk("a2", 151, c(60, 62, 63, 62, 61)),
    mk("a3", 150, c(60, 62, 63, 62, 61)),
    mk("a4", 152, c(60, 62, 63, 62, 61)),
    mk("a5", 149, c(60, 62, 63, 62, 43))   # last frame ~20 dB below the peak
  ))

  with_int <- inspect_f0(df, intensity = "intensity", intensity_drop = 15)
  expect_true(all(c("intensity", "flag_low_intensity") %in% names(with_int)))
  a5_last <- with_int[with_int$token == "a5" & with_int$time == 0.04, ]
  expect_true(a5_last$flag_low_intensity)
  expect_match(a5_last$flag_notes, "low intensity")

  without_int <- inspect_f0(df)
  expect_false(any(c("intensity", "flag_low_intensity") %in% names(without_int)))
})

test_that("inspect_f0 low-intensity flag is advisory (does not set flagged_token)", {
  mk <- function(tok, base, int) data.frame(
    token = tok, time = seq(0, 0.04, by = 0.01),
    f0 = base + c(0, 1, 0, -1, 0),         # smooth: no jumps
    speaker = "S01", tone = "T1",
    intensity = int)
  df <- do.call(rbind, list(
    mk("a1", 150, c(60, 62, 63, 62, 61)),
    mk("a2", 151, c(60, 62, 63, 62, 61)),
    mk("a3", 150, c(60, 62, 63, 62, 61)),
    mk("a4", 152, c(60, 62, 63, 62, 61)),
    mk("a5", 150, c(60, 62, 63, 62, 43))   # only a quiet tail, normal level
  ))
  out <- inspect_f0(df, intensity = "intensity", intensity_drop = 15)
  a5 <- out[out$token == "a5", ]
  expect_true(any(a5$flag_low_intensity))   # the quiet frame is flagged
  expect_false(any(a5$flagged_token))        # but the token itself is not
})


# ---------- token-level speaker and tone (A9) ---------------------------------

# A tone column read from a TextGrid tier: the label on the rows with f0,
# "" or NA on the rows without f0 at the edges.
.tier_fixture <- function() {
  mk <- function(tok, tone, med, speaker = "S01") {
    data.frame(token = tok, speaker = c("", speaker, speaker, speaker, NA),
               tone = c("", tone, tone, tone, NA),
               f0 = c(NA, med - 1, med, med + 1, NA))
  }
  do.call(rbind, list(
    mk("t1", "T1", 145), mk("t2", "T1", 148), mk("t3", "T1", 150),
    mk("t4", "T1", 152), mk("t5", "T1", 155), mk("t6", "T1", 250)))
}

test_that("flag_level_outliers takes speaker and tone from the rows with f0", {
  tier <- .tier_fixture()
  out  <- flag_level_outliers(tier)
  expect_identical(sort(out$tone), rep("T1", 6))
  expect_identical(sort(out$speaker), rep("S01", 6))
  expect_true(out$flag_level_high[out$token == "t6"])
  expect_identical(sum(out$flag_level_high | out$flag_level_low), 1L)

  # The same with the rows without f0 removed, or the rows shuffled.
  sig <- function(o) {
    o <- as.data.frame(o)[order(o$token), ]
    rownames(o) <- NULL
    o
  }
  none <- tier[!is.na(tier$f0), ]
  expect_identical(sig(flag_level_outliers(none)), sig(out))
  set.seed(2)
  expect_identical(sig(flag_level_outliers(tier[sample(nrow(tier)), ])),
                   sig(out))
})

test_that("token-level values: most frequent non-empty value on rows with f0", {
  df <- data.frame(
    token = rep(c("a", "b", "c"), each = 4),
    f0    = c(NA, 150, 151, 152,   150, 151, 152, 0,   NA, NA, NA, NA),
    tone  = factor(c("T9", "T2", "T1", "T1",  " ", "T3", "T4", "T5",
                     "", "T7", "T7", NA))
  )
  out <- token_level_meta(df, "token", "tone", "f0")
  expect_identical(out$token, c("a", "b", "c"))
  expect_true(is.factor(out$tone))
  # a: T1 twice beats T2 once; the row without f0 (T9) does not count.
  # b: a blank is empty; T3 and T4 tie, so the first in C-locale order wins.
  # c: no row with f0, so the most frequent non-empty value on any row.
  expect_identical(as.character(out$tone), c("T1", "T3", "T7"))
  # The same with the rows in reverse order.
  rev_out <- token_level_meta(df[nrow(df):1, ], "token", "tone", "f0")
  expect_identical(as.character(rev_out$tone[order(rev_out$token)]),
                   c("T1", "T3", "T7"))
})

test_that("a tie between labels does not depend on the row order", {
  # Each token: two rows labelled T2, then two labelled T1, all with f0.
  set.seed(1)
  dd <- do.call(rbind, lapply(1:8, function(i) data.frame(
    token = paste0("k", i), time = (1:4) / 100, f0 = 200 + stats::rnorm(4),
    speaker = "s1", tone = c("T2", "T2", "T1", "T1"))))
  rev_dd <- dd[nrow(dd):1, ]
  shuf   <- dd[sample(nrow(dd)), ]
  tones  <- function(o) o$tone[order(o$token)]
  # Without a time column: the first in C-locale order, whatever the order.
  expect_identical(tones(flag_level_outliers(dd)), rep("T1", 8))
  expect_identical(tones(flag_level_outliers(rev_dd)), rep("T1", 8))
  expect_identical(tones(flag_level_outliers(shuf)), rep("T1", 8))
  # With one: the first in time, as fit_polynomial() picks.
  for (x in list(dd, rev_dd, shuf)) {
    expect_identical(tones(flag_level_outliers(x, time = "time")), rep("T2", 8))
    co <- suppressMessages(fit_polynomial(x, degree = 1))
    expect_identical(tones(co), rep("T2", 8))
  }
  # inspect_f0() passes its time column on. Token x ties T2 (first in
  # time) with T1, at the T2 level: read as T1 it is "level too high".
  peers <- do.call(rbind, lapply(1:12, function(i) data.frame(
    token = paste0("p", i), time = (1:4) / 100, speaker = "s1",
    tone = if (i <= 6) "T1" else "T2",
    f0 = (if (i <= 6) 150 else 250) + i %% 3 + c(0, 1, 2, 3))))
  x <- data.frame(token = "x", time = (1:4) / 100, speaker = "s1",
                  tone = c("T2", "T2", "T1", "T1"), f0 = 251 + c(0, 1, 2, 3))
  tie <- rbind(peers, x)
  for (y in list(tie, tie[nrow(tie):1, ])) {
    lvl <- flag_level_outliers(y)
    expect_true(lvl$flag_level_high[lvl$token == "x"])
    lvl_t <- flag_level_outliers(y, time = "time")
    expect_false(lvl_t$flag_level_high[lvl_t$token == "x"])
    ins <- inspect_f0(y)
    expect_false(any(grepl("level too high", ins$flag_notes[ins$token == "x"])))
  }
  # The speaker follows the same rule in flag_outliers().
  sp <- dd
  sp$speaker <- rep(c("s2", "s2", "s1", "s1"), 8)
  for (x in list(sp, sp[nrow(sp):1, ], sp[sample(nrow(sp)), ])) {
    expect_identical(sort(unique(flag_outliers(x)$speaker)), "s1")
    expect_identical(sort(unique(flag_outliers(x, time = "time")$speaker)), "s2")
  }
})

test_that("a label only on rows without f0 is not borrowed", {
  # The rows with f0 are unlabelled; only the rows without f0 carry T3 or s9.
  e <- data.frame(token = "k", time = (1:5) / 100, f0 = c(NA, 200, 210, 220, NA),
                  speaker = c("s9", NA, NA, NA, "s9"), tone = c("T3", "", "", "", "T3"))
  r <- e[!is.na(e$f0), ]
  expect_identical(flag_level_outliers(e)$tone, "")
  expect_identical(flag_level_outliers(r)$tone, "")
  expect_identical(flag_outliers(e)$speaker, NA_character_)
  expect_identical(flag_outliers(r)$speaker, NA_character_)
  # The same rule as fit_polynomial().
  co <- suppressMessages(fit_polynomial(e, degree = 1))
  expect_identical(co$tone, "")
  # A token with no f0 at all still gets the label on its rows.
  z <- rbind(e, data.frame(token = "z", time = (1:3) / 100, f0 = NA,
                           speaker = "s4", tone = "T5"))
  expect_identical(token_level_meta(z, "token", c("speaker", "tone"), "f0")$tone,
                   c("", "T5"))
})

test_that("flag_outliers keeps a token's speaker when its first row is empty", {
  mk <- function(tok, base, spk) data.frame(
    token = tok, speaker = c("", spk, spk, spk), f0 = c(NA, base, base + 2, base + 1))
  df <- do.call(rbind, list(mk("t1", 150, "S01"), mk("t2", 152, "S01"),
                            mk("t3", 151, "S01"), mk("t4", 400, "S02")))
  out <- flag_outliers(df)
  expect_identical(out$speaker[order(out$token)], c("S01", "S01", "S01", "S02"))
  # Negative markers are no f0: they do not set the minimum.
  df$f0[is.na(df$f0)] <- -1
  out2 <- flag_outliers(df)
  expect_identical(as.data.frame(out2), as.data.frame(out))
  expect_true(all(out2$f0_token_min > 0))
})


# ---------- jump check on the test files (skipped when absent) ---------------

test_that("flag_pitch_jumps on f0_long.csv: same flags with or without rows without f0", {
  path <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(path), "test/f0_long.csv not found")
  raw <- zero_hz_to_na(utils::read.csv(path, encoding = "UTF-8",
                                       stringsAsFactors = FALSE))
  none <- raw[!is.na(raw$f0), ]
  a <- flag_pitch_jumps(raw)
  b <- flag_pitch_jumps(none)
  expect_identical(.jump_sig(a), .jump_sig(b))
  expect_identical(sum(b$flagged_jump), 2940L)
  expect_identical(length(unique(b$token[b$flagged_jump])), 578L)
  gap <- b$flagged_jump & grepl("across a [0-9]+ ms gap", b$jump_note)
  expect_identical(sum(gap), 75L)
  expect_identical(length(unique(b$token[gap])), 71L)
  x <- b[b$token == "dc102\u{4e00}3s23.5725" &
         abs(b$time - c(0.47)) < 1e-6, ]
  expect_identical(x$jump_note, "jump (rise) across a 20 ms gap")
  y <- b[b$token == "dc102\u{4e00}3s23.5725" & abs(b$time - 0.48) < 1e-6, ]
  expect_identical(y$jump_note, "carryover")
})

test_that("flag_pitch_jumps on dc21f0.csv: 660 flags, no gap notes", {
  path <- test_path("..", "..", "test", "dc21f0.csv")
  skip_if_not(file.exists(path), "test/dc21f0.csv not found")
  dc <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  dc$token <- paste(dc$speaker, dc$char, dc$position, dc$start_time, sep = "|")
  out <- flag_pitch_jumps(dc, f0 = "f0_Hz")
  expect_identical(sum(out$flagged_jump), 660L)
  expect_false(any(grepl("gap", out$jump_note)))
})


# ---------- inspect_f0: tokens with no f0, small groups, voiced_run (E2) -----

# Speaker S1, tone T1: five tokens with f0 (t5 sits well above the others)
# and one token with no f0 at all (NA, 0 and the marker -1).
.no_f0_fixture <- function() {
  mk <- function(tok, f0, tone = "T1") data.frame(
    token = tok, time = .frames(seq_along(f0)), f0 = f0,
    speaker = "S1", tone = tone)
  rbind(mk("t1", c(NA, 150, 151, 152, NA)), mk("t2", c(NA, 148, 149, 150)),
        mk("t3", c(152, 153, 154, NA)), mk("t4", c(149, 150, 151)),
        mk("t5", c(NA, 230, 231, 232, NA)), mk("t9", c(NA, 0, -1, NA)))
}

test_that("inspect_f0 does not flag a token with no f0 and notes it", {
  out <- inspect_f0(.no_f0_fixture(), z_threshold = 1)
  t9 <- out[out$token == "t9", ]
  expect_identical(nrow(t9), 4L)
  # Not NA (the speaker-level flags of a token with no f0 are FALSE)
  expect_identical(t9$flagged_token, rep(FALSE, 4))
  expect_identical(t9$flagged_jump, rep(FALSE, 4))
  expect_identical(t9$flag_notes, rep("no f0", 4))
  expect_false(anyNA(out$flagged_token))
  expect_false(anyNA(out$flag_notes))
  # The other tokens get no "no f0" note, and their checks still run
  expect_false(any(grepl("no f0", out$flag_notes[out$token != "t9"])))
  expect_true(all(out$flagged_token[out$token == "t5"]))
  # Without a tone the same holds
  out_nt <- inspect_f0(.no_f0_fixture(), tone = NULL, z_threshold = 1)
  expect_identical(out_nt$flag_notes[out_nt$token == "t9"], rep("no f0", 4))
  expect_identical(out_nt$flagged_token[out_nt$token == "t9"], rep(FALSE, 4))
  expect_null(attr(out_nt, "level_groups"))
})

test_that("inspect_f0 counts the speaker x tone groups on tokens with f0", {
  # Four tokens with f0 and one with no f0: the group is too small, so
  # t5 is not checked against its tone.
  d4 <- .no_f0_fixture()
  d4 <- d4[d4$token != "t1", ]
  out <- inspect_f0(d4)
  grp <- attr(out, "level_groups")
  expect_identical(grp$speaker, "S1")
  expect_identical(grp$tone, "T1")
  expect_identical(grp$n_tokens, 4L)
  expect_false(grp$checked)
  expect_false(any(grepl("level too", out$flag_notes)))
  # Five tokens with f0: the group is checked and t5 is too high
  out5 <- inspect_f0(.no_f0_fixture())
  grp5 <- attr(out5, "level_groups")
  expect_identical(grp5$n_tokens, 5L)
  expect_true(grp5$checked)
  expect_true(any(grepl("level too high", out5$flag_notes[out5$token == "t5"])))
  # min_tokens is the limit
  expect_false(attr(inspect_f0(.no_f0_fixture(), min_tokens = 6), "level_groups")$checked)
  # One row per group; the same with the rows without f0 removed
  two <- rbind(.no_f0_fixture(),
               transform(.no_f0_fixture()[.no_f0_fixture()$token == "t2", ],
                         token = "u2", tone = "T2"))
  g2 <- attr(inspect_f0(two), "level_groups")
  expect_identical(g2$tone, c("T1", "T2"))
  expect_identical(g2$n_tokens, c(5L, 1L))
  expect_identical(g2$checked, c(TRUE, FALSE))
  none <- two[!is.na(two$f0) & two$f0 > 0, ]
  expect_identical(attr(inspect_f0(none), "level_groups"), g2)
})

test_that("inspect_f0 gives each token the speaker and tone the level check used", {
  # The tone changes within a token: each token's first row is "x" (a
  # TextGrid boundary row) and its other rows are "1" or "2". k01 is a level
  # outlier in tone 1. Token k13 has no f0.
  set.seed(2)
  d <- do.call(rbind, lapply(1:12, function(i) {
    f  <- 150 + 30 * (i > 6) + cumsum(stats::rnorm(10, 0, 0.3))
    tn <- if (i > 6) "2" else "1"
    data.frame(token = sprintf("k%02d", i), time = .frames(1:10), f0 = f,
               speaker = "S", tone = c("x", rep(tn, 9)))
  }))
  d$f0[d$token == "k01"] <- d$f0[d$token == "k01"] * 1.5
  d <- rbind(d, data.frame(token = "k13", time = .frames(1:3), f0 = NA,
                           speaker = "S", tone = "1"))
  out <- inspect_f0(d)
  tg <- attr(out, "token_groups")
  expect_s3_class(tg, "data.frame")
  expect_identical(names(tg), c("token", "speaker", "tone"))
  # One row per token with f0, with the token-level tone, never "x"
  expect_identical(sort(tg$token), sprintf("k%02d", 1:12))
  expect_identical(tg$tone[match(sprintf("k%02d", 1:12), tg$token)],
                   rep(c("1", "2"), each = 6))
  expect_identical(tg$speaker, rep("S", 12))
  # The same groups as level_groups
  grp <- attr(out, "level_groups")
  expect_identical(grp$tone, c("1", "2"))
  expect_identical(as.integer(table(tg$tone)), grp$n_tokens)
  expect_true(any(grepl("level too high", out$flag_notes[out$token == "k01"])))
  # The same with the first rows removed or the rows shuffled
  sorted <- function(x) { x <- x[order(x$token), ]; rownames(x) <- NULL; x }
  first <- !duplicated(d$token)
  expect_identical(sorted(attr(inspect_f0(d[!first, ]), "token_groups")), sorted(tg))
  set.seed(3)
  expect_identical(sorted(attr(inspect_f0(d[sample(nrow(d)), ]), "token_groups")),
                   sorted(tg))
  # Other column names are kept
  d2 <- d
  names(d2)[names(d2) == "tone"] <- "category"
  names(d2)[names(d2) == "speaker"] <- "talker"
  tg2 <- attr(inspect_f0(d2, speaker = "talker", tone = "category"), "token_groups")
  expect_identical(names(tg2), c("token", "talker", "category"))
  # No tone: no level check, no attribute
  expect_null(attr(inspect_f0(d, tone = NULL), "token_groups"))
})

test_that("inspect_f0 passes voiced_run through, row by row", {
  full <- .jump_fixture()
  full$speaker <- "S1"
  full$tone <- "T1"
  dr <- suppressMessages(drop_rows_without_f0(full))
  expect_true("voiced_run" %in% names(dr))
  out <- inspect_f0(dr)
  expect_identical(names(out)[1:4], c("token", "time", "f0", "voiced_run"))
  key_in  <- paste(dr$token, dr$time)
  key_out <- paste(out$token, out$time)
  expect_identical(out$voiced_run, dr$voiced_run[match(key_out, key_in)])
  expect_identical(max(out$voiced_run[out$token == "a"]), 2L)
  # With intensity, voiced_run comes after the intensity column
  dr$intensity <- 60
  out_i <- inspect_f0(dr, intensity = "intensity")
  expect_identical(names(out_i)[1:5], c("token", "time", "f0", "intensity", "voiced_run"))
  # No voiced_run in the input: none in the output
  expect_false("voiced_run" %in% names(inspect_f0(full)))
})

test_that("inspect_f0 gives the same flags with or without rows without f0", {
  full <- rbind(transform(.jump_fixture(), speaker = "S1", tone = "T1"),
                data.frame(token = "z", time = .frames(1:3), f0 = NA,
                           speaker = "S1", tone = "T1"))
  sig <- function(o) {
    o <- as.data.frame(o)
    ok <- !is.na(o$f0) & o$f0 > 0
    s <- data.frame(key = paste(o$token, format(o$time, nsmall = 4)),
                    tok = o$flagged_token, jump = o$flagged_jump,
                    note = o$flag_notes)[ok, ]
    s <- s[order(s$key), ]
    rownames(s) <- NULL
    s
  }
  ref <- inspect_f0(full, z_threshold = 1)
  expect_true(any(ref$flagged_jump))
  expect_true(any(grepl("across a 20 ms gap", ref$flag_notes)))
  none <- full[!is.na(full$f0) & full$f0 > 0, ]
  expect_identical(sig(inspect_f0(none, z_threshold = 1)), sig(ref))
  dr <- suppressMessages(drop_rows_without_f0(full))
  expect_identical(sig(inspect_f0(dr, z_threshold = 1)), sig(ref))
  set.seed(4)
  shuf <- full[sample(nrow(full)), ]
  expect_identical(sig(inspect_f0(shuf, z_threshold = 1)), sig(ref))
  # Shares over the samples with f0 are the same too
  share <- function(o) {
    ok <- !is.na(o$f0) & o$f0 > 0
    c(sum(o$flagged_jump[ok]), sum(ok))
  }
  expect_identical(share(inspect_f0(none)), share(ref))
  expect_identical(share(inspect_f0(shuf)), share(ref))
})

test_that("inspect_f0 with intensity: the same flags with or without rows without f0, NA times, shuffled rows and -1 markers", {
  # Six tokens of one speaker and tone. The rows without f0 (NA, 0 Hz and
  # the marker -1) carry intensity too, and some are loud (80-90 dB, a click
  # or a voiceless burst): they must not set a token's peak intensity. t2
  # has a jump across a gap, t4 an octave spike, t6 a level outlier, and t1
  # and t3 a quiet frame with f0. A row without f0 in t2 and a row with f0
  # in t3 have no time.
  mk <- function(tok, f0, int) data.frame(
    token = tok, time = .frames(seq_along(f0)), f0 = f0, speaker = "S1",
    tone = "T1", intensity = int)
  full <- rbind(
    mk("t1", c(NA, 150, 151, 152, 151, 150, 149, -1),
       c(40, 60, 64, 65, 64, 63, 47, 80)),
    mk("t2", c(0, 148, 149, 150, NA, NA, 220, 221, 0),
       c(85, 61, 63, 64, 30, 30, 64, 62, 82)),
    mk("t3", c(-1, 152, 153, 154, 153, 152, 151, NA),
       c(90, 62, 64, 65, 63, 62, 41, 70)),
    mk("t4", c(NA, 149, 150, 300, 151, 150, 149, NA),
       c(NA, 60, 62, 64, 63, 62, 60, 85)),
    mk("t5", c(0, 151, 152, 153, 152, 151, 150, -1),
       c(20, 63, 64, 65, 64, 63, 62, 88)),
    mk("t6", c(NA, 230, 231, 232, 231, 230, 229, NA),
       c(40, 60, 62, 64, 63, 62, 61, 40)))
  full$time[full$token == "t2"][1] <- NA
  full$time[full$token == "t3"][4] <- NA
  sig <- function(o) {
    o <- as.data.frame(o)
    ok <- !is.na(o$f0) & o$f0 > 0
    s <- data.frame(key = paste(o$token, trimws(format(o$time, nsmall = 4))),
                    tok = o$flagged_token, jump = o$flagged_jump,
                    low = o$flag_low_intensity, note = o$flag_notes)[ok, ]
    s <- s[order(s$key), ]
    rownames(s) <- NULL
    s
  }
  run <- function(d) inspect_f0(d, intensity = "intensity")
  ref <- run(full)
  s <- sig(ref)
  # Each check finds something
  expect_identical(s$key[s$low], c("t1 0.0725", "t3 0.0725"))
  expect_identical(s$note[s$key == "t2 0.0725"], "jump (rise) across a 20 ms gap")
  expect_match(s$note[s$key == "t4 0.0425"], "octave jump", fixed = TRUE)
  expect_true(all(grepl("level too high", s$note[startsWith(s$key, "t6")])))
  # The row with f0 but no time takes no part in the jump check
  expect_identical(s$jump[s$key == "t3 NA"], FALSE)
  expect_identical(s$note[s$key == "t3 NA"], "")
  # No row without f0 is flagged for its intensity
  no_f0 <- is.na(ref$f0) | ref$f0 <= 0
  expect_false(any(ref$flag_low_intensity[no_f0]))

  none <- full[!is.na(full$f0) & full$f0 > 0, ]
  expect_identical(sig(run(none)), s)
  set.seed(5)
  expect_identical(sig(run(full[sample(nrow(full)), ])), s)
  expect_identical(attr(run(none), "level_groups"), attr(ref, "level_groups"))
  dr <- suppressMessages(drop_rows_without_f0(full))
  expect_identical(nrow(dr), 41L)
  expect_identical(sig(run(dr)), s)
})

test_that("inspect_f0 on f0_long.csv: 2,940 of 52,431 samples with f0 in every version", {
  path <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(path), "test/f0_long.csv not found")
  skip_on_cran()
  raw <- zero_hz_to_na(utils::read.csv(path, encoding = "UTF-8",
                                       stringsAsFactors = FALSE))
  raw$speaker <- sub("^(dc[0-9]+).*$", "\\1", raw$token, perl = TRUE)
  raw$tone <- sub("^dc[0-9]+[^0-9]+([0-9])s.*$", "\\1", raw$token, perl = TRUE)
  versions <- list(raw = raw, trimmed = trim_to_voiced(raw, min_run = 1),
                   removed = suppressMessages(drop_rows_without_f0(raw)))
  set.seed(1)
  versions$shuffled <- raw[sample(nrow(raw)), ]
  tok_ref <- NULL
  for (v in names(versions)) {
    out <- inspect_f0(versions[[v]])
    ok <- !is.na(out$f0) & out$f0 > 0
    expect_identical(sum(ok), 52431L, label = v)
    expect_identical(sum(out$flagged_jump[ok]), 2940L, label = v)
    expect_identical(round(100 * 2940 / sum(ok), 1), 5.6)
    tok <- unique(out[ok, c("token", "flagged_token")])
    tok <- tok[order(tok$token), ]
    rownames(tok) <- NULL
    if (is.null(tok_ref)) tok_ref <- tok
    expect_identical(tok, tok_ref, label = v)
  }
  expect_true("voiced_run" %in% names(inspect_f0(versions$removed)))
})
