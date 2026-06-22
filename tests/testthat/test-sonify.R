test_that("sonify_f0 returns a 16-bit mono Wave of the right length", {
  w <- sonify_f0(rep(200, 21), fs = 16000, dur = 0.5, source = "tone")
  expect_s4_class(w, "Wave")
  expect_equal(w@samp.rate, 16000)
  expect_equal(w@bit, 16)
  expect_equal(length(w@left), 8000L)        # fs * dur
  expect_true(max(abs(w@left)) <= 32767)
})

test_that("the rendered tone tracks the contour frequency", {
  w <- sonify_f0(rep(200, 11), fs = 16000, dur = 0.6, source = "tone")
  x <- as.numeric(w@left)[2000:6000]
  x <- x - mean(x)
  sp <- Mod(stats::fft(x))[seq_len(floor(length(x) / 2))]
  peak <- which.max(sp)
  freq <- (peak - 1) * 16000 / length(x)
  expect_lt(abs(freq - 200), 15)             # dominant frequency near 200 Hz
})

test_that("complex source works and NA gaps are handled", {
  w <- sonify_f0(c(120, NA, 160, 150, NA), dur = 0.4, source = "complex")
  expect_s4_class(w, "Wave")
  expect_false(anyNA(w@left))
  expect_error(sonify_f0(c(NA, NA)), "at least 2 finite")
})

test_that("intensity shapes the loudness envelope (louder where dB is higher)", {
  f <- rep(200, 40)
  w  <- sonify_f0(f, fs = 16000, dur = 0.6, source = "tone",
                  intensity = seq(50, 85, length.out = 40))   # rising loudness
  x  <- abs(as.numeric(w@left)); n <- length(x)
  early <- mean(x[round(0.15 * n):round(0.30 * n)])
  late  <- mean(x[round(0.70 * n):round(0.85 * n)])
  expect_gt(late, early * 1.5)                                # end is louder
  # NULL intensity -> roughly flat amplitude across the same windows
  x0 <- abs(as.numeric(sonify_f0(f, fs = 16000, dur = 0.6, source = "tone")@left))
  e0 <- mean(x0[round(0.15 * n):round(0.30 * n)])
  l0 <- mean(x0[round(0.70 * n):round(0.85 * n)])
  expect_lt(abs(l0 - e0) / max(e0, 1), 0.2)
})

test_that("vowel source (source-filter) produces a finite Wave for a/i/u", {
  for (vw in c("a", "i", "u")) {
    w <- sonify_f0(c(120, 140, 160, 150, 130), dur = 0.4, source = "vowel", vowel = vw)
    expect_s4_class(w, "Wave")
    expect_false(anyNA(w@left))
    expect_true(max(abs(w@left)) <= 32767)
    expect_true(max(abs(w@left)) > 0)
  }
})
