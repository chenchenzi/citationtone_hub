# =============================================================================
# Tests for apply_relabels().
# =============================================================================

make_df <- function() {
  data.frame(
    token = rep(c("t1", "t2", "t3"), each = 3),
    time  = rep(1:3, 3),
    f0    = c(150, 160, 155, 152, 159, 154, 200, 210, 205),
    tone  = rep(c("T1", "T1", "T4"), each = 3),
    stringsAsFactors = FALSE
  )
}

test_that("apply_relabels adds the curation columns and preserves originals", {
  df  <- make_df()
  out <- apply_relabels(df)
  expect_true(all(c("tone_relabelled", "excluded", "curate_note") %in% names(out)))
  expect_identical(out$tone, df$tone)               # original untouched
  expect_identical(out$tone_relabelled, df$tone)    # defaults to original
  expect_false(any(out$excluded))                   # nothing excluded
  expect_true(all(is.na(out$curate_note)))          # no notes by default
})

test_that("apply_relabels records per-token notes", {
  df  <- make_df()
  out <- apply_relabels(df, relabel = c("t1" = "T1lit"),
                        note = c("t1" = "literary form"))
  expect_equal(unique(out$curate_note[out$token == "t1"]), "literary form")
  expect_true(all(is.na(out$curate_note[out$token != "t1"])))
})

test_that("apply_relabels errors on an unnamed note vector", {
  df <- make_df()
  expect_error(apply_relabels(df, note = c("reason")), "named vector")
})

test_that("apply_relabels relabels named tokens at every row of the token", {
  df  <- make_df()
  out <- apply_relabels(df, relabel = c("t1" = "T1lit"))
  expect_equal(unique(out$tone_relabelled[out$token == "t1"]), "T1lit")
  expect_equal(unique(out$tone_relabelled[out$token == "t2"]), "T1")  # unchanged
  expect_identical(out$tone[out$token == "t1"], rep("T1", 3))         # original kept
})

test_that("apply_relabels marks excluded tokens", {
  df  <- make_df()
  out <- apply_relabels(df, exclude = c("t3"))
  expect_true(all(out$excluded[out$token == "t3"]))
  expect_false(any(out$excluded[out$token != "t3"]))
})

test_that("apply_relabels handles simultaneous relabel and exclude", {
  df  <- make_df()
  out <- apply_relabels(df, relabel = c("t2" = "T2"), exclude = c("t3"))
  expect_equal(unique(out$tone_relabelled[out$token == "t2"]), "T2")
  expect_true(all(out$excluded[out$token == "t3"]))
  expect_false(any(out$excluded[out$token == "t2"]))
})

test_that("apply_relabels errors on missing columns", {
  df <- make_df()
  expect_error(apply_relabels(df, token = "nope"), "not found")
})

test_that("apply_relabels errors on an unnamed relabel vector", {
  df <- make_df()
  expect_error(apply_relabels(df, relabel = c("T1lit")), "named vector")
})

test_that("apply_relabels respects custom column names", {
  df <- make_df()
  names(df)[names(df) == "token"] <- "item"
  names(df)[names(df) == "tone"]  <- "cat"
  out <- apply_relabels(df, token = "item", tone = "cat",
                        relabel = c("t1" = "X"))
  expect_equal(unique(out$tone_relabelled[out$item == "t1"]), "X")
})
