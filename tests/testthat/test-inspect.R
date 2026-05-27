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
  # ~10 ms steps; a +5 ST jump in one step well exceeds rise_threshold=1.263
  hz <- 150 * 2^(c(0, 0, 0.5, 0.5, 0.5))   # frame 3 jumps +6 ST
  df <- data.frame(
    token = rep("t1", 5),
    time  = seq(0, 0.04, by = 0.01),
    f0    = hz
  )
  out <- flag_pitch_jumps(df, time_unit = "s", carryover_mult = 0)
  expect_true(out$flagged_jump[3])
})

test_that("flag_pitch_jumps handles f0 == 0 as unvoiced", {
  df <- data.frame(
    token = rep("t1", 5),
    time  = seq(0, 0.04, by = 0.01),
    f0    = c(150, 0, 0, 152, 151)
  )
  expect_no_error(flag_pitch_jumps(df, time_unit = "s"))
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
