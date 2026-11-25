# The Start page's screen of rows without f0 (inst/app/server.R and
# inst/app/ui/start_ui.R): the counts, the removal and voiced_run, the boxes'
# defaults, data_version and the clearing of shared results, and the R code
# that repeats the Start-page steps (read_data_code() in global.R).

# The harness, start_server(), is in helper-app-start.R.

# The report as plain text: tags dropped (inline runs have no space around
# their tags), entities read, no-break spaces as spaces, whitespace collapsed.
start_text <- function(x) {
  h <- gsub("<[^>]*>", "", start_html(x))
  for (e in list(c("&#39;", "'"), c("&quot;", "\""), c("&gt;", ">"), c("&lt;", "<"), c("&amp;", "&")))
    h <- gsub(e[1], e[2], h, fixed = TRUE)
  h <- gsub(intToUtf8(160), " ", h, fixed = TRUE)
  trimws(gsub("\\s+", " ", h))
}
EN <- intToUtf8(0x2013)   # the en dash in "0-1"
# How many times `pattern` (fixed text) appears in `x`.
n_hits <- function(x, pattern) sum(gregexpr(pattern, x, fixed = TRUE)[[1]] > 0)

test_that("the app reaches only exported package functions for the screen", {
  used <- c("screen_f0_rows", "drop_rows_without_f0", "suggest_unvoiced", "hz_f0_columns",
            "zero_hz_to_na", "read_csv_utf8", "contour_duration", "time_already_normalised",
            "flag_f0_tokens")
  expect_true(all(used %in% getNamespaceExports("shinytone")))
  # The fill is gone from the package and the app
  expect_false("fill_f0_gaps" %in% getNamespaceExports("shinytone"))
  app <- system.file("app", package = "shinytone")
  for (fl in c("server.R", "global.R", file.path("ui", "start_ui.R"))) {
    src <- readLines(file.path(app, fl), warn = FALSE, encoding = "UTF-8")
    expect_false(any(grepl("fill_f0_gaps|f0_filled|screen_fill|fill_n|What filling|max_gap|fillable|long_gap", src)),
                 info = fl)
  }
})

test_that("start_col() and resolve_start_cols() keep valid picks and fall back to the defaults", {
  h <- start_server()$env
  expect_null(h$start_col(NULL, "f0"))
  expect_null(h$start_col(function() NULL, "f0"))
  expect_identical(h$start_col(function() list(token = "tk", f0 = "f0"), "f0"), "f0")
  expect_null(h$start_col(function() list(token = NULL, f0 = "f0"), "token"))
  raw <- data.frame(token = "a", time = 1:3 / 100, f0 = c(NA, 200, 201),
                    f0_corrected = c(NA, 200, 202), syllable_t01 = c(0, 0.5, 1))
  expect_identical(h$start_screen_defaults(raw), list(token = "token", time = "time", f0 = "f0_corrected"))
  expect_identical(h$resolve_start_cols(raw, "token", "syllable_t01", "f0"),
                   list(token = "token", time = "time", f0 = "f0"))
  # "(none)" is offered only when no column looks like a token column
  expect_identical(h$resolve_start_cols(raw, "", NULL, NULL)$token, "token")
  expect_identical(h$resolve_start_cols(raw, "gone", "gone", "gone")$f0, "f0_corrected")
  # F0 Extraction's export: f0 is the corrected value, f0_original the tracker's
  raw2 <- data.frame(token = "a", time = 1:3 / 100, f0_original = c(NA, 200, 201), f0 = c(NA, 200, 202))
  expect_identical(h$start_screen_defaults(raw2)$f0, "f0")
  # No token column: no guess (dc21f0.csv's point number must not be one)
  raw3 <- data.frame(index = 1:3, time = 1:3 / 100, f0_Hz = c(200, 201, 202))
  expect_null(h$start_screen_defaults(raw3)$token)
  expect_null(h$resolve_start_cols(raw3, "", NULL, NULL)$token)
  expect_identical(h$resolve_start_cols(raw3, "index", NULL, NULL)$token, "index")
  expect_identical(h$start_screen_defaults(raw3)$f0, "f0_Hz")
})

test_that("screened_f0_note() speaks only when another f0 column still lacks f0", {
  h <- start_server()$env
  sc <- function() list(token = "token", time = "time", f0 = "f0_corrected")
  d <- data.frame(f0 = c(NA, 200, NA), f0_corrected = c(NA, 200, 201))
  expect_null(h$screened_f0_note(d, "f0_corrected", sc))
  expect_null(h$screened_f0_note(d, "f0", NULL))
  note <- paste(as.character(h$screened_f0_note(d, "f0", sc)), collapse = "")
  expect_match(note, "f0 has 2 rows without f0. The Start page screens f0_corrected.", fixed = TRUE)
  expect_null(h$screened_f0_note(d[2, ], "f0", sc))
})

test_that("the screen counts, removes and marks the rows without f0 on a padded fixture", {
  s <- start_server()
  f <- start_write_csv(start_padded_fixture())
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "padded.csv"))
    expect_identical(start_cols(), list(token = "token", time = "time", f0 = "f0"))
    scr <- f0_screen()
    sm <- attr(scr$screen, "summary")
    expect_equal(sm[["rows_without_f0"]], 21L)
    expect_equal(sm[["no_f0_tokens"]], 1L)
    expect_equal(sm[["gaps"]], 3L)
    expect_true(screen_plan()$remove)
    d <- dataset()
    expect_equal(nrow(d), sm[["rows_after"]])
    expect_false(anyNA(d$f0))
    expect_true("voiced_run" %in% names(d))
    expect_equal(max(d$voiced_run[d$token == "S1_a"]), 2L)
    expect_false("S2_b" %in% d$token)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "21 of 45 rows (46.7%)", fixed = TRUE)
    # Removed: the rows the removal takes, not the rows left
    expect_match(rep, paste("Removed: 21 rows without f0. voiced_run numbers the stretches with f0 in each",
                            "token, so plots break the line at gaps."), fixed = TRUE)
    expect_no_match(rep, "rows left", fixed = TRUE)
    # S2_b has no f0: flagged with the others, and no tile of its own.
    # Removal takes its rows, so the callout says so instead of naming the
    # column and the drop box: no row left is flagged.
    expect_match(rep, paste("1 token flagged: fewer than 3 samples with f0 (S2_b). It has no f0, so removal",
                            "takes its rows. In 2 tokens"), fixed = TRUE)
    expect_no_match(rep, "The reason is in", fixed = TRUE)
    expect_no_match(rep, "Tick Drop the flagged token", fixed = TRUE)
    expect_false(any(nzchar(d$screen_flag)))
    expect_no_match(rep, "in tokens with no f0", fixed = TRUE)
    # Rows removed: the folded save note, and no fold on what counts as no f0
    expect_match(rep, paste("Your raw file is not changed The boxes change only the app's copy of the data.",
                            "To save that copy, download it in Normalise or Curate."),
                 fixed = TRUE)
    expect_no_match(rep, "What removal changes", fixed = TRUE)
    expect_no_match(rep, "What counts as no f0", fixed = TRUE)
    # One toast for the screen, with the 0 Hz count
    expect_true("f0a_f0_screen" %in% start_toast_ids(s$rec))
    expect_false("f0a_zero_hz" %in% start_toast_ids(s$rec))
    # The 0 Hz rows of the screened column go in brackets after the count
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"), paste(
      "21 of 45 rows have no f0 (2 of them 0 Hz) and are removed. 1 token is flagged.",
      "See the Start page."))
    expect_identical(data_version(), 0L)

    # The short edge runs (S1_b starts with 2 samples cut off by a gap)
    expect_true(output$screen_show_short)
    expect_match(start_html(output$ui_f0_screen), "start or end (2 tokens)", fixed = TRUE)
    session$setInputs(screen_short = TRUE)
    expect_equal(nrow(dataset()), sm[["rows_after_short"]])
    expect_identical(data_version(), 1L)
    session$setInputs(screen_short = FALSE)
    expect_identical(data_version(), 2L)
  })
})

test_that("every analysis tab gets the Start columns, data_version and start_settings by name", {
  s <- start_server()
  f <- start_write_csv(start_padded_fixture())
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    for (m in names(s$rec$modules)) {
      got <- s$rec$modules[[m]]
      expect_true(is.function(got$start_cols), info = m)
      expect_true(is.function(got$data_version), info = m)
      expect_true(is.function(got$start_settings), info = m)
    }
    expect_length(s$rec$modules, 9)
    expect_identical(s$rec$modules$model_ui$start_cols()$f0, "f0")
    expect_identical(s$rec$modules$gca_ui$data_version(), 0L)
  })
})

test_that("data_version goes up when the boxes or columns change the rows, not on a new upload", {
  s <- start_server()
  # A stand-in for Curate that publishes its data from dataset(), as the real
  # tab's observer does (priority 0), so curated_data() changes with the rows
  # and server.R's own clearing of the curated + normalised data could fire.
  s$env$curate_ui <- function(input, output, session, dataset, normalised_data = NULL,
                              curated_data, ...) {
    observe({
      d <- dataset()
      if (!is.null(d)) d$tone <- paste0(d$tone, "x")
      curated_data(d)
    })
  }
  # Without the token with no f0: it is flagged (a screen_flag column),
  # and in an f0 column with values for it it would not be, so another f0
  # column would change the data even with removal unticked
  fx <- start_padded_fixture()
  fx <- fx[fx$token != "S2_b", ]
  f <- start_write_csv(fx)
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    n_kept <- nrow(dataset())
    normalised_data(data.frame(x = 1))
    inspect_result(list(x = 1))
    gca_pred_data(data.frame(x = 1))
    # Curated + normalised data that matches the curated data: only a change
    # of the curated rows would clear it
    cd <- curated_data()
    expect_false(is.null(cd))
    key_cols <- c("token", "tone", "speaker", "f0")
    cn <- cd
    attr(cn, "curated_key") <- s$env$curated_key(cd, key_cols)
    attr(cn, "curated_key_cols") <- key_cols
    curated_norm_data(cn)
    session$setInputs(screen_remove = FALSE)
    expect_equal(nrow(dataset()), nrow(fx))
    expect_equal(nrow(curated_data()), nrow(fx))   # Curate published the new rows
    expect_false("screen_flag" %in% names(dataset()))
    expect_identical(data_version(), 1L)
    expect_null(normalised_data())
    expect_null(inspect_result())
    expect_null(gca_pred_data())
    expect_null(curated_norm_data())
    msg <- start_toast_text(s$rec, "start_data_cleared")
    expect_match(msg, "\"Remove rows without f0\" was unticked", fixed = TRUE)
    expect_match(msg, "These results came from the old rows, so they were cleared:", fixed = TRUE)
    expect_match(msg, "normalised data", fixed = TRUE)
    expect_match(msg, "Inspect results", fixed = TRUE)
    expect_match(msg, "curated + normalised data", fixed = TRUE)
    # Only one toast about the curated + normalised data: the data_version
    # path clears it before Curate publishes its new rows
    expect_false("curated_norm_cleared" %in% start_toast_ids(s$rec))

    # Another f0 column while removal is unticked: the rows stay, nothing clears
    normalised_data(data.frame(x = 1))
    session$setInputs(screen_f0 = "intensity")
    expect_identical(data_version(), 1L)
    expect_false(is.null(normalised_data()))
    # ... and with removal ticked again, the column change counts
    session$setInputs(screen_f0 = "f0")
    session$setInputs(screen_remove = TRUE)
    expect_identical(data_version(), 2L)
    expect_equal(nrow(dataset()), n_kept)
    normalised_data(data.frame(x = 1))
    session$setInputs(screen_f0 = "intensity")
    expect_identical(data_version(), 3L)
    expect_match(start_toast_text(s$rec, "start_data_cleared"), "the f0 variable is now intensity", fixed = TRUE)
    # One result cleared: the same wording, with no "was" / "were" to agree
    expect_match(start_toast_text(s$rec, "start_data_cleared"),
                 "These results came from the old rows, so they were cleared: normalised data.",
                 fixed = TRUE)
    session$setInputs(screen_f0 = "f0")
    expect_identical(data_version(), 4L)

    # "Exclude discarded tokens" is not in this file; a new upload resets the
    # boxes and does not count
    session$setInputs(screen_remove = FALSE)
    expect_identical(data_version(), 5L)
    session$setInputs(uploadfile = start_upload(f, "again.csv"))
    expect_identical(data_version(), 5L)
    expect_true(screen_plan()$remove)
    expect_equal(nrow(dataset()), n_kept)
  })
})

test_that("an upload where every row has f0 and a token column has nothing to remove", {
  sample <- system.file("app", "www", "dc21f0_test.csv", package = "shinytone")
  skip_if(!nzchar(sample), "sample not found")
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(sample))
    expect_identical(start_cols()$f0, "f0_Hz")
    expect_identical(start_cols()$token, "token")
    sm <- attr(f0_screen()$screen, "summary")
    expect_equal(sm[["rows_without_f0"]], 0L)
    expect_equal(sm[["gaps_time"]], 0L)
    expect_identical(screen_plan()$action, "none")
    expect_false("voiced_run" %in% names(dataset()))
    expect_equal(nrow(dataset()), 38808L)
    for (fl in c("screen_show_unvoiced", "screen_show_remove", "screen_show_short",
                "screen_show_mark", "screen_need_token",
                "screen_show_drop_flagged")) expect_false(output[[fl]], info = fl)
    expect_false("f0_filled" %in% names(dataset()))
    expect_false("screen_flag" %in% names(dataset()))
    expect_no_match(start_text(output$f0_screen_report), "flagged", fixed = TRUE)
    rep <- start_text(output$f0_screen_report)
    # Nothing to remove: all good, in one line
    expect_match(rep, "Data screening Every row has f0 All good: every row has f0.", fixed = TRUE)
    expect_no_match(rep, "What counts as no f0", fixed = TRUE)
    expect_no_match(rep, "nothing to remove", fixed = TRUE)
    expect_match(start_html(output$f0_screen_report),
                 '<div><strong class="st-verdict">All good:</strong> every row has f0.</div>', fixed = TRUE)
    expect_match(start_html(output$f0_screen_report), "st-decision-done", fixed = TRUE)
    # Your data: tokens, speakers, tones (in order), rows, 21 equidistant points
    expect_match(rep, "1,848 tokens 13 speakers 6 tones 1, 2, 3, 4, 5, 6 38,808 rows every row has f0",
                 fixed = TRUE)
    # The title says it; no line under it repeats it
    expect_match(rep, "21 equidistant points per token Variables used", fixed = TRUE)
    expect_no_match(rep, "Every token has 21 points.", fixed = TRUE)
    expect_match(rep, "Variables used f0 f0_Hz (Hz) token token time time speaker speaker tone tone",
                 fixed = TRUE)
    expect_match(rep, "dc21f0_test.csv", fixed = TRUE)
    expect_no_match(rep, "gaps in the middle", fixed = TRUE)
    expect_no_match(rep, "can't be found", fixed = TRUE)
    # The app's copy is the file: no save note, and the card as before
    expect_no_match(rep, "Your raw file is not changed", fixed = TRUE)
    expect_match(rep, "Data screening Every row has f0", fixed = TRUE)
    expect_false("f0a_f0_screen" %in% setdiff(start_toast_ids(s$rec), "")[
      vapply(s$rec$toasts, function(t) t$type != "removed", logical(1))])
  })
})

# The checkbox a rendered Data screening box draws, and whether it is ticked.
start_box_html <- function(html, id) {
  regmatches(html, regexpr(sprintf('<input id="%s" type="checkbox"[^>]*/>', id), html))
}
# The comment lines of the R code as one text, without their "# ".
start_code_comments <- function(code) {
  gsub("\\s+", " ", paste(sub("^# ?", "", code[startsWith(code, "#")]), collapse = " "))
}
# The R code reproduces dataset() as the app has it now.
start_code_ok <- function(s, f, settings, data) {
  code <- s$env$read_data_code(f, settings)
  run <- new.env()
  eval(parse(text = code), run)
  expect_identical(run$dat, data)
  invisible(code)
}
# The Data screening boxes of an equal-point file: removal as in a frame
# file (shown while rows without f0 are left), the drop when a token is
# flagged, and never the short runs, as a gap there flags its token.
start_eq_boxes <- function(output, remove = TRUE) {
  expect_identical(output$screen_show_remove, remove)
  expect_false(output$screen_show_short)
  expect_false(output$screen_show_mark)
}

test_that("an equal-point file: removal as for frames, and the flagged tokens listed by reason", {
  f <- start_write_csv(start_user_fixture())
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "equal_points.csv"))
    expect_true(isTRUE(attr(f0_screen()$screen, "equal_rows")))
    expect_identical(attr(f0_screen()$screen, "summary")[["rows_without_f0"]], 16L)
    expect_true(screen_plan()$points)
    # Removal is shown and ticked by default, as for frames; the short runs
    # are not offered
    expect_true(screen_box("remove"))
    expect_true(screen_plan()$remove)
    expect_true(output$screen_show_drop_flagged)
    expect_false(output$screen_show_unvoiced)
    start_eq_boxes(output)
    expect_false(screen_box("drop_flagged"))
    ui <- start_html(output$ui_f0_screen)
    expect_match(start_box_html(ui, "screen_remove"), "checked", fixed = TRUE)
    expect_no_match(start_box_html(ui, "screen_drop_flagged"), "checked", fixed = TRUE)
    expect_match(start_text(output$ui_f0_screen), "Drop the 5 flagged tokens Remove 16 rows without f0 (recommended)",
                 fixed = TRUE)
    expect_identical(start_text(output$ui_f0_screen_hint), paste(
      "16 of 132 rows have no f0. See the report above the Data Preview.",
      "The other tabs start with these f0 and token variables."))
    # The data: the rows without f0 removed, voiced_run added, and the 5
    # tokens with fewer than 11 points with f0 flagged
    d <- dataset()
    expect_identical(nrow(d), 116L)
    expect_false(anyNA(d$f0))
    expect_true("voiced_run" %in% names(d))
    expect_identical(max(d$voiced_run[d$token == "p04"]), 2L)
    expect_identical(sum(d$screen_flag != ""), 39L)
    expect_identical(sort(unique(d$token[d$screen_flag != ""])), sprintf("p%02d", 2:6))
    # The reasons describe the uploaded grid
    expect_identical(unique(d$screen_flag[d$token == "p04"]), "3 points without f0 inside")
    expect_identical(unique(d$screen_flag[d$token == "p05"]), "no f0 at the first point")
    expect_identical(unique(d$screen_flag[d$token == "p06"]), "only 2 points with f0")
    # The card: the same chip, lead, tiles and decision as for frames, then
    # the flag callout with the tokens by reason
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "12 tokens 2 speakers 132 rows 16 without f0 see below", fixed = TRUE)
    expect_match(rep, paste(
      "Data screening 12.1% without f0 16 of 132 rows (12.1%) have no f0 in column f0: 5 at the start",
      "of 2 tokens 5 at the end of 1 token 6 inside 3 tokens, in 3 gaps Removed: 16 rows without f0.",
      "voiced_run numbers the stretches with f0 in each token, so plots break the line at gaps. 5",
      "tokens flagged: fewer than 11 points with f0. Reasons are in a new screen_flag column. Points",
      "without f0 inside: p02, p03, p04 No f0 at the first or last point: p05 Fewer than 3 points with",
      "f0: p06 Tick Drop the 5 flagged tokens to leave them out. Your raw file is not changed The boxes",
      "change only the app's copy of the data. To save that copy, download it in Normalise or Curate."),
      fixed = TRUE)
    for (w in c("Missing first or last points", "What counts as no f0", "Export again",
                "at all 11 points", "not removed", "Your file is not changed", "points without f0", "Kept", "No f0 at all", "and 1 more",
                "Also remove runs", "a gap cuts off")) {
      expect_no_match(rep, w, fixed = TRUE)
    }
    expect_identical(n_hits(rep, "16 of 132 rows"), 1L)
    expect_identical(n_hits(rep, "Drop the 5 flagged tokens"), 1L)
    html <- start_html(output$f0_screen_report)
    expect_match(html, '<span class="st-chip st-chip-warn">12.1% without f0</span>', fixed = TRUE)
    expect_match(html, paste0("<div><strong>5 tokens flagged:</strong> fewer than 11 points with f0. Reasons ",
                              "are in a new <code>screen_flag</code> column.</div>"), fixed = TRUE)
    expect_match(html, paste0('<li>Points without f0 inside: <code class="st-tok">p02</code>, ',
                              '<code class="st-tok">p03</code>, <code class="st-tok">p04</code></li>'), fixed = TRUE)
    expect_match(html, '<li>Fewer than 3 points with f0: <code class="st-tok">p06</code></li>', fixed = TRUE)
    expect_identical(n_hits(html, "<li>"), 3L)
    expect_match(html, "<div>Tick <em>Drop the 5 flagged tokens</em> to leave them out.</div>", fixed = TRUE)
    expect_match(html, "st-where", fixed = TRUE)
    expect_match(html, "st-decision-done", fixed = TRUE)
    expect_match(html, "<summary>Your raw file is not changed</summary>", fixed = TRUE)
    # One toast, as for frames
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "16 of 132 rows have no f0 and are removed. 5 tokens are flagged. See the Start page.")
    # The R code flags, then removes, with no token names
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == 'dat <- shinytone::flag_f0_tokens(dat, token = "token", time = "time", f0 = "f0")'))
    expect_true(any(code == '# Flag 5 tokens in a screen_flag column ("" for the other tokens)'))
    expect_true(any(code == "# Remove the 16 rows without f0 and add voiced_run"))
    expect_false(any(grepl("leaving", code, fixed = TRUE)))
    expect_true(any(code == 'dat <- shinytone::drop_rows_without_f0(dat, token = "token", time = "time", f0 = "f0")'))
    expect_false(any(grepl("p0", code, fixed = TRUE)))
    expect_false(any(grepl("points", code, fixed = TRUE)))
    at <- function(x) which(grepl(x, code, fixed = TRUE))[1]
    expect_true(at("flag_f0_tokens(") < at("drop_rows_without_f0("))
    expect_identical(data_version(), 0L)

    # The drop ticked: the 5 tokens go, and nothing is left to remove
    normalised_data(data.frame(x = 1))
    session$setInputs(screen_drop_flagged = TRUE)
    expect_identical(data_version(), 1L)
    expect_null(normalised_data())
    expect_true(screen_box("remove"))
    d <- dataset()
    expect_identical(nrow(d), 77L)
    expect_identical(sort(unique(d$token)), sprintf("p%02d", c(1, 7:12)))
    expect_false(anyNA(d$f0))
    expect_false("screen_flag" %in% names(d))
    start_eq_boxes(output, remove = FALSE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "5 at the start of 2 tokens 5 at the end of 1 token 6 inside 3 tokens, in 3 gaps All good: every",
      "row left has f0. Dropped: 5 flagged tokens (55 rows), fewer than 11 points with f0: Points without",
      "f0 inside: p02, p03, p04 No f0 at the first or last point: p05 Fewer than 3 points with f0: p06",
      "Your raw file is not changed"), fixed = TRUE)
    for (w in c("Untick", "Tick Drop", "Kept", "Removed:", "screen_flag", "Missing first or last points",
                "Nothing left to remove", "(77 rows)")) {
      expect_no_match(rep, w, fixed = TRUE)
    }
    html <- start_html(output$f0_screen_report)
    expect_match(html, '<div><strong class="st-verdict">All good:</strong> every row left has f0.</div>',
                 fixed = TRUE)
    expect_match(html, paste0("<div><strong>Dropped:</strong> 5 flagged tokens (55 rows), fewer than 11 points ",
                              "with f0:</div>"), fixed = TRUE)
    expect_identical(n_hits(html, "<li>"), 3L)
    expect_identical(start_text(output$ui_f0_screen_hint), paste(
      "16 of 132 rows have no f0. See the report above the Data Preview.",
      "The other tabs start with these f0 and token variables."))
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# Drop the 5 flagged tokens (55 rows)"))
    expect_true(any(code == 'dat <- dat[dat[["screen_flag"]] == "", , drop = FALSE]'))
    expect_true(any(code == "# Every row left has f0 in column f0; nothing to remove"))
    expect_false(any(grepl("drop_rows_without_f0(", code, fixed = TRUE)))
    msg <- start_toast_text(s$rec, "start_data_cleared")
    expect_match(msg, "\"Drop the flagged tokens\" was ticked", fixed = TRUE)
    expect_no_match(msg, "Remove rows without f0", fixed = TRUE)

    # The drop unticked and removal unticked: every row stays, flagged
    session$setInputs(screen_drop_flagged = FALSE)
    expect_identical(data_version(), 2L)
    normalised_data(data.frame(x = 1))
    session$setInputs(screen_remove = FALSE)
    expect_identical(data_version(), 3L)
    expect_match(start_toast_text(s$rec, "start_data_cleared"), "\"Remove rows without f0\" was unticked",
                 fixed = TRUE)
    d <- dataset()
    expect_identical(nrow(d), 132L)
    expect_identical(sum(is.na(d$f0)), 16L)
    expect_false("voiced_run" %in% names(d))
    expect_identical(sum(d$screen_flag != ""), 55L)
    start_eq_boxes(output)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "6 inside 3 tokens, in 3 gaps Kept: rows without f0 stay. Tick Remove rows without f0 to remove",
      "them. 5 tokens flagged: fewer than 11 points with f0. Reasons are in a new screen_flag column.",
      "Points without f0 inside: p02, p03, p04 No f0 at the first or last point: p05 Fewer than 3 points",
      "with f0: p06 Tick Drop the 5 flagged tokens to leave them out. Your raw file is not changed"),
      fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# The 16 rows without f0 stay (removal unticked)"))
    expect_false(any(grepl("drop_rows_without_f0(", code, fixed = TRUE)))
    # ... and the drop with it: the 7 tokens left keep their 11 points
    session$setInputs(screen_drop_flagged = TRUE)
    expect_identical(nrow(dataset()), 77L)
    expect_match(start_text(output$f0_screen_report),
                 "All good: every row left has f0. Dropped: 5 flagged tokens", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())

    # A new upload resets the boxes (removal ticked, the drop not), and
    # does not count
    dv <- data_version()
    session$setInputs(uploadfile = start_upload(f, "again.csv"))
    expect_false(screen_box("drop_flagged"))
    expect_true(screen_box("remove"))
    expect_identical(nrow(dataset()), 116L)
    expect_true("screen_flag" %in% names(dataset()))
    expect_identical(data_version(), dv)
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("equal points: two reasons on two lines, gaps inside only, 0 Hz, -1, and short runs not offered", {
  # start_points_fixture(): p06 misses its last point and one inside
  f <- start_write_csv(start_points_fixture())
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "points.csv"))
    fl <- screen_plan()$flagged
    expect_identical(fl$token, sprintf("p%02d", 2:7))
    expect_identical(fl$reason[fl$token == "p06"], "no f0 at the last point; 1 point without f0 inside")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "Removed: 11 rows without f0. voiced_run numbers the stretches with f0 in each token, so plots",
      "break the line at gaps. 6 tokens flagged: fewer than 11 points with f0. Reasons are in a new",
      "screen_flag column. Points without f0 inside: p02, p03, p04, p06 No f0 at the first or last",
      "point: p05, p06, p07 Tick Drop the 6 flagged tokens to leave them out."), fixed = TRUE)
    expect_identical(n_hits(rep, "p06"), 2L)
    expect_identical(nrow(dataset()), 319L)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_drop_flagged = TRUE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("All good: every row left has f0. Dropped: 6 flagged tokens (66 rows), fewer",
                            "than 11 points with f0: Points without f0 inside"),
                 fixed = TRUE)
    expect_identical(n_hits(rep, "p06"), 2L)
    expect_identical(nrow(dataset()), 264L)
    start_code_ok(s, f, start_settings(), dataset())
  })

  # Only gaps inside the tokens, no end point missing: removal is shown and
  # ticked by default too
  fx <- start_points_fixture()
  keep <- !(fx$token %in% c("p02", "p03"))
  fx$f0[keep] <- fx$truth[keep]
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "inside.csv"))
    expect_true(screen_box("remove"))
    expect_true(screen_plan()$remove)
    start_eq_boxes(output)
    expect_match(start_box_html(start_html(output$ui_f0_screen), "screen_remove"), "checked", fixed = TRUE)
    expect_identical(screen_plan()$flagged$token, c("p02", "p03"))
    d <- dataset()
    expect_identical(nrow(d), 327L)
    expect_false(anyNA(d$f0))
    expect_identical(d$voiced_run[d$token == "p03"], c(rep(1L, 5), rep(2L, 4)))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "3 of 330 rows (0.9%) have no f0 in column f0: 3 inside 2 tokens, in 2 gaps Removed: 3 rows",
      "without f0. voiced_run numbers the stretches with f0 in each token, so plots break the line at gaps. 2",
      "tokens flagged: fewer than 11 points with f0. Reasons are in a new screen_flag column. Points",
      "without f0 inside: p02, p03 Tick Drop the 2 flagged tokens to leave them out. Your raw file is",
      "not changed"), fixed = TRUE)
    expect_no_match(rep, "Missing first or last points", fixed = TRUE)
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "3 of 330 rows have no f0 and are removed. 2 tokens are flagged. See the Start page.")
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(grepl("drop_rows_without_f0(", code, fixed = TRUE)))
  })

  # A run of 1 sample cut off by a gap at the start (p02's point 2): the
  # short-run box stays hidden, and a stray tick removes nothing
  fx <- start_points_fixture()
  fx$f0 <- fx$truth
  fx$f0[fx$token == "p02"][2] <- NA
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "short.csv"))
    expect_gt(attr(f0_screen()$screen, "summary")[["short_runs"]], 0L)
    start_eq_boxes(output)
    expect_identical(screen_plan()$flagged$reason, "1 point without f0 inside")
    expect_identical(nrow(dataset()), 329L)
    session$setInputs(screen_short = TRUE)
    expect_false(screen_plan()$short)
    expect_identical(nrow(dataset()), 329L)
    expect_identical(data_version(), 0L)
    rep <- start_text(output$f0_screen_report)
    expect_no_match(rep, "a gap cuts off", fixed = TRUE)
    expect_no_match(rep, "Also remove runs", fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_false(any(grepl("min_run", code, fixed = TRUE)))
  })

  # 0 Hz read as no f0: the count goes in brackets after the rows
  fx <- start_points_fixture()
  fx$f0[fx$token == "p03"][6:7] <- 0
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "zero.csv"))
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     paste("11 of 330 rows have no f0 (2 of them 0 Hz) and are removed. 6 tokens are flagged.",
                           "See the Start page."))
    expect_no_match(start_text(output$f0_screen_report), "What counts as no f0", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })

  # A column zero_hz_to_na() does not pick, with -1 for no f0: read as no
  # f0 (ticked, the box shown), then flagged and removed
  fx <- start_points_fixture("f0_rapt")
  fx$f0_rapt[is.na(fx$f0_rapt)] <- -1
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "rapt.csv"))
    expect_identical(start_cols()$f0, "f0_rapt")
    expect_identical(f0_screen()$unvoiced, -1)
    expect_true(output$screen_show_unvoiced)
    expect_true(output$screen_show_drop_flagged)
    start_eq_boxes(output)
    d <- dataset()
    expect_identical(nrow(d), 319L)
    expect_false(any(d$f0_rapt %in% -1))
    expect_identical(screen_plan()$flagged$token, sprintf("p%02d", 2:7))
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "11 of 330 rows have no f0 (all -1) and are removed. 6 tokens are flagged. See the Start page.")
    code <- start_code_ok(s, f, start_settings(), dataset())
    at <- function(x) which(grepl(x, code, fixed = TRUE))[1]
    expect_true(at('dat[["f0_rapt"]][dat[["f0_rapt"]] %in% -1] <- NA') < at("flag_f0_tokens("))
    # Removal unticked: the -1 rows stay as NA
    session$setInputs(screen_remove = FALSE)
    expect_identical(nrow(dataset()), 330L)
    expect_identical(sum(is.na(dataset()$f0_rapt)), 11L)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_drop_flagged = TRUE)
    expect_identical(nrow(dataset()), 264L)
    start_code_ok(s, f, start_settings(), dataset())
    # Read 0 / -1 unticked: -1 counts as f0, so nothing is flagged and the
    # card is the one for a file where every row has f0
    session$setInputs(screen_unvoiced = FALSE)
    expect_false(screen_plan()$flag)
    expect_identical(sum(dataset()$f0_rapt %in% -1), 11L)
    expect_match(start_text(output$f0_screen_report), "Every row has f0 All good: every row has f0.",
                 fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })

  # Frame-by-frame data (tokens of different lengths): no point reasons,
  # and the token with no f0 is flagged whatever the removal box says
  f <- start_write_csv(start_padded_fixture())
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_true(output$screen_show_drop_flagged)
    expect_true(output$screen_show_remove)
    expect_true(output$screen_show_short)
    expect_identical(screen_plan()$flagged$reason, "no f0")
    # Removal takes every row of S2_b, so the column is there, with no reason left
    expect_true("screen_flag" %in% names(dataset()))
    expect_identical(unique(dataset()$screen_flag), "")
    expect_match(start_text(output$f0_screen_report), paste(
      "1 token flagged: fewer than 3 samples with f0 (S2_b). It has no f0, so removal takes its rows."),
      fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_remove = FALSE)
    expect_identical(screen_plan()$flagged$reason, "no f0")
    expect_identical(unique(dataset()$screen_flag[dataset()$token == "S2_b"]), "no f0")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "Kept: rows without f0 stay.", fixed = TRUE)
    # Its rows stay, so the column has its reason and the drop box would
    # leave it out
    expect_match(rep, paste("1 token flagged: fewer than 3 samples with f0 (S2_b). The reason is in a new",
                            "screen_flag column. Tick Drop the flagged token to leave it out."), fixed = TRUE)
    expect_no_match(rep, "so removal takes", fixed = TRUE)
    expect_no_match(rep, "points with f0", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("equal points: the wording at n = 1, and a token with no f0 as for frames", {
  # One token with no last point
  fx <- start_points_fixture()
  fx$f0 <- fx$truth
  fx$f0[fx$token == "p06"][11] <- NA
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "one.csv"))
    expect_identical(start_text(output$ui_f0_screen_hint), paste(
      "1 of 330 rows has no f0. See the report above the Data Preview.",
      "The other tabs start with these f0 and token variables."))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "Data screening 0.3% without f0 1 of 330 rows (0.3%) has no f0 in column f0: 1 at the end of 1",
      "token Removed: 1 row without f0. voiced_run numbers the stretches with f0 in each token, so plots",
      "break the line at gaps. 1 token flagged: fewer than 11 points with f0. The reason is in a new",
      "screen_flag column. No f0 at the first or last point: p06 Tick Drop the flagged token to leave it",
      "out."), fixed = TRUE)
    expect_no_match(rep, "Missing first or last points", fixed = TRUE)
    html <- start_html(output$f0_screen_report)
    expect_match(html, paste0("<div><strong>1 token flagged:</strong> fewer than 11 points with f0. The reason ",
                              "is in a new <code>screen_flag</code> column.</div>"), fixed = TRUE)
    expect_match(html, "<div>Tick <em>Drop the flagged token</em> to leave it out.</div>", fixed = TRUE)
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "1 of 330 rows has no f0 and is removed. 1 token is flagged. See the Start page.")
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == '# Flag 1 token in a screen_flag column ("" for the other tokens)'))
    expect_true(any(code == "# Remove the row without f0 and add voiced_run"))
    session$setInputs(screen_drop_flagged = TRUE)
    d <- dataset()
    expect_identical(nrow(d), 319L)
    expect_false("screen_flag" %in% names(d))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "All good: every row left has f0. Dropped: 1 flagged token (11 rows), fewer than 11 points with",
      "f0: No f0 at the first or last point: p06 Your raw file is not changed"), fixed = TRUE)
    expect_no_match(rep, "Untick", fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# Drop the flagged token (11 rows)"))
  })

  # A token with no f0 at all (p10, after p07 in the file): flagged like
  # the others whatever the removal box says, on the "Fewer than 3" line,
  # and counted in the box, the toast, the R code and the Dropped line.
  # Removal still takes its rows (the lead and the Removed count have
  # them), and it has no tile of its own.
  fx <- start_points_fixture()
  fx$f0[fx$token == "p10"] <- NA
  f <- start_write_csv(fx)
  s <- start_server()
  upd <- new.env()
  s$env$updateCheckboxInput <- function(session, inputId, label = NULL, value = NULL) {
    upd[[inputId]] <- list(label = label, value = value)
  }
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "nof0.csv"))
    expect_null(screen_plan()$keep_no_f0)
    flagged <- sprintf("p%02d", c(2:7, 10))
    expect_identical(screen_plan()$flagged$token, flagged)
    expect_identical(screen_plan()$flagged$reason[7], "no f0")
    expect_identical(screen_plan()$n_flag_rows, 77L)
    expect_false("p10" %in% dataset()$token)
    expect_identical(nrow(dataset()), 308L)
    expect_true(output$screen_show_drop_flagged)
    expect_identical(upd$screen_drop_flagged$label, "Drop the 7 flagged tokens")
    expect_identical(upd$screen_remove$label, "Remove 22 rows without f0 (recommended)")
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "22 of 330 rows have no f0 and are removed. 7 tokens are flagged. See the Start page.")
    rep <- start_text(output$f0_screen_report)
    # The tiles hold the other 11 rows: no tile for p10's
    expect_match(rep, paste(
      "22 of 330 rows (6.7%) have no f0 in column f0: 3 at the start of 2 tokens 1 at the end of 1 token",
      "7 inside 4 tokens, in 4 gaps Removed: 22 rows without f0. voiced_run numbers the stretches with",
      "f0 in each token, so plots break the line at gaps. 7 tokens flagged: fewer than 11 points with",
      "f0. Reasons are in a new screen_flag column. Points without f0 inside: p02, p03, p04, p06 No f0",
      "at the first or last point: p05, p06, p07 Fewer than 3 points with f0: p10 Tick Drop the 7",
      "flagged tokens to leave them out."), fixed = TRUE)
    for (w in c("No f0 at all", "in tokens with no f0", "which removal drops", "which the drop leaves out")) {
      expect_no_match(rep, w, fixed = TRUE)
    }
    html <- start_html(output$f0_screen_report)
    expect_identical(n_hits(html, "<li>"), 3L)
    expect_match(html, '<li>Fewer than 3 points with f0: <code class="st-tok">p10</code></li>', fixed = TRUE)
    expect_no_match(html, 'fa-ban', fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == '# Flag 7 tokens in a screen_flag column ("" for the other tokens)'))
    expect_true(any(code == "# Remove the 22 rows without f0 and add voiced_run"))
    expect_false(any(grepl("flagged too", code, fixed = TRUE)))
    expect_false(any(grepl("p10", code, fixed = TRUE)))

    # Removal unticked: the same 7 tokens, and the box's label stays
    dv <- data_version()
    session$setInputs(screen_remove = FALSE)
    expect_identical(data_version(), dv + 1L)
    expect_false(screen_box("drop_flagged"))
    expect_identical(screen_plan()$flagged$token, flagged)
    expect_identical(unique(dataset()$screen_flag[dataset()$token == "p10"]), "no f0")
    expect_identical(upd$screen_drop_flagged$label, "Drop the 7 flagged tokens")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("Kept: rows without f0 stay. Tick Remove rows without f0 to remove them. 7",
                            "tokens flagged:"), fixed = TRUE)
    expect_match(rep, paste("p05, p06, p07 Fewer than 3 points with f0: p10 Tick Drop the 7 flagged tokens",
                            "to leave them out."), fixed = TRUE)
    expect_no_match(rep, "No f0 at all", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())

    # The drop ticked: p10 goes with the others, counted in the Dropped line
    session$setInputs(screen_drop_flagged = TRUE)
    expect_false(screen_box("remove"))
    expect_false("p10" %in% dataset()$token)
    expect_identical(nrow(dataset()), 253L)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# Drop the 7 flagged tokens (77 rows)"))
    expect_true(any(code == 'dat <- dat[dat[["screen_flag"]] == "", , drop = FALSE]'))
    expect_false(any(grepl("no f0\"", code, fixed = TRUE)))
    expect_true(all(nchar(code[startsWith(code, "#")]) <= 80))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("All good: every row left has f0. Dropped: 7 flagged tokens (77 rows), fewer than",
                            "11 points with f0: Points without f0 inside: p02, p03, p04, p06 No f0 at the",
                            "first or last point: p05, p06, p07 Fewer than 3 points with f0: p10"), fixed = TRUE)

    # Removal ticked again with the drop: the same 7 tokens and 77 rows
    session$setInputs(screen_remove = TRUE)
    expect_identical(nrow(dataset()), 253L)
    expect_match(start_text(output$f0_screen_report),
                 "All good: every row left has f0. Dropped: 7 flagged tokens (77 rows),", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })

  # Three tokens of 9 points, two flagged
  mk <- function(tk, miss) {
    pts <- 1:9
    x <- data.frame(token = tk, time = round(0.1 + 0.2 * (pts - 1) / 8, 4), f0 = 200 + 2 * pts,
                    stringsAsFactors = FALSE)
    x$f0[miss] <- NA
    x
  }
  f <- start_write_csv(rbind(mk("a", 4), mk("b", 1), mk("c", integer(0))))
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "three.csv"))
    session$setInputs(screen_drop_flagged = TRUE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("All good: every row left has f0. Dropped: 2 flagged tokens (18 rows), fewer",
                            "than 9 points with f0: Points without f0 inside: a No f0 at the first or last",
                            "point: b"), fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("equal points: many flagged tokens, at most 5 a line", {
  # 50 tokens of 11 points over their own durations: m01-m41 miss point 6,
  # m03 and m45 their first point, m48 has f0 at 2 points and m50 none
  toks <- sprintf("m%02d", 1:50)
  fx <- do.call(rbind, lapply(seq_along(toks), function(i) {
    pts <- 1:11
    data.frame(token = toks[i], time = round((0.15 + 0.004 * i) * (pts - 1) / 10, 5),
               f0 = 170 + i + 2 * (pts - 1), stringsAsFactors = FALSE)
  }))
  fx$f0[fx$token %in% toks[1:41]][rep(c(rep(FALSE, 5), TRUE, rep(FALSE, 5)), 41)] <- NA
  fx$f0[fx$token %in% c("m03", "m45")][c(1, 12)] <- NA
  fx$f0[fx$token == "m48"][-c(3, 4)] <- NA
  fx$f0[fx$token == "m50"] <- NA
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "many.csv"))
    # m50 has no f0: flagged with the others, after m48 on the "Fewer than
    # 3" line, as flag_f0_tokens() orders them. Removal still takes its 11
    # rows, which have no tile.
    expect_identical(nrow(screen_plan()$flagged), 44L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "11 equidistant points per token", fixed = TRUE)
    expect_match(rep, paste(
      "Data screening 11.5% without f0 63 of 550 rows (11.5%) have no f0 in column f0: 4 at the start",
      "of 3 tokens 7 at the end of 1 token 41 inside 41 tokens, in 41 gaps Removed: 63 rows without f0.",
      "voiced_run numbers the stretches with f0 in each token, so plots break the line at gaps. 44",
      "tokens flagged: fewer than 11 points with f0. Reasons are in a new screen_flag column. Points",
      "without f0 inside: m01, m02, m03, m04, m05 and 36 more No f0 at the first or last point: m03, m45",
      "Fewer than 3 points with f0: m48, m50 Tick Drop the 44 flagged tokens to leave them out."),
      fixed = TRUE)
    expect_no_match(rep, "No f0 at all", fixed = TRUE)
    expect_no_match(rep, "in tokens with no f0", fixed = TRUE)
    html <- start_html(output$f0_screen_report)
    expect_identical(n_hits(html, "<li>"), 3L)
    expect_match(html, '<code class="st-tok">m05</code> and 36 more</li>', fixed = TRUE)
    flag_html <- sub(".*st-callout-info", "", html)
    expect_identical(n_hits(flag_html, '<code class="st-tok">'), 5L + 2L + 2L)
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "63 of 550 rows have no f0 and are removed. 44 tokens are flagged. See the Start page.")
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_false(any(grepl("m0", code, fixed = TRUE)))
    expect_true(any(code == "# Remove the 63 rows without f0 and add voiced_run"))
    # Removal unticked: the same 44 tokens, on the same three lines
    session$setInputs(screen_remove = FALSE)
    expect_identical(nrow(screen_plan()$flagged), 44L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "44 tokens flagged: fewer than 11 points with f0. Reasons are in a new screen_flag column. Points",
      "without f0 inside: m01, m02, m03, m04, m05 and 36 more No f0 at the first or last point: m03, m45",
      "Fewer than 3 points with f0: m48, m50 Tick Drop the 44 flagged tokens to leave them out."),
      fixed = TRUE)
    html <- start_html(output$f0_screen_report)
    expect_identical(n_hits(html, "<li>"), 3L)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_drop_flagged = TRUE)
    expect_identical(nrow(dataset()), 66L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "All good: every row left has f0. Dropped: 44 flagged tokens (484 rows), fewer than 11 points with",
      "f0: Points without f0 inside: m01, m02, m03, m04, m05 and 36 more"),
      fixed = TRUE)
    expect_no_match(rep, "Untick", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    # Removal ticked again with the drop: the same 44 tokens and 484 rows
    session$setInputs(screen_remove = TRUE)
    expect_identical(nrow(dataset()), 66L)
    expect_match(start_text(output$f0_screen_report),
                 "Dropped: 44 flagged tokens (484 rows), fewer than 11 points with f0:", fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# Drop the 44 flagged tokens (484 rows)"))
  })
})

test_that("equal points: every token flagged, and the file's own screen_flag column", {
  # Every token misses its first point: the drop is offered as dropping
  # every token, and ticked it leaves no rows, which the card says plainly
  fx <- start_points_fixture()
  fx$f0 <- fx$truth
  fx$f0[c(TRUE, rep(FALSE, 10))] <- NA
  fx$f0[fx$token == "p02"][5] <- NA
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "all_edge.csv"))
    expect_identical(nrow(screen_plan()$flagged), 30L)
    expect_match(start_text(output$ui_f0_screen), "Drop the 30 flagged tokens (every token)", fixed = TRUE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "Removed: 31 rows without f0. voiced_run numbers the stretches with f0 in each token, so plots",
      "break the line at gaps. 30 tokens flagged: fewer than 11 points with f0. Reasons are in a new",
      "screen_flag column. Points without f0 inside: p02 No f0 at the first or last point: p01, p02,",
      "p03, p04, p05 and 25 more Dropping them would leave no rows."), fixed = TRUE)
    expect_no_match(rep, "to leave them out", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_drop_flagged = TRUE)
    expect_identical(nrow(dataset()), 0L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "No rows left: every token is flagged. Untick Drop the 30 flagged tokens to keep them. Dropped:",
      "30 flagged tokens (330 rows), fewer than 11 points with f0: Points without f0 inside: p02 No f0",
      "at the first or last point: p01, p02, p03, p04, p05 and 25 more"), fixed = TRUE)
    for (w in c("All good", "Removed:", "Untick Drop the 30 flagged tokens (every token)")) {
      expect_no_match(rep, w, fixed = TRUE)
    }
    html <- start_html(output$f0_screen_report)
    expect_match(html, "st-decision-stop", fixed = TRUE)
    expect_no_match(html, "st-decision-done", fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# No rows are left"))
  })

  # The file has a screen_flag column of its own: the new one replaces it,
  # in a short extra sentence
  fx <- start_user_fixture()
  fx$screen_flag <- ifelse(fx$token == "p01", "old reason", "")
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "old_flags.csv"))
    expect_true(screen_plan()$old_flag)
    d <- dataset()
    expect_identical(unique(d$screen_flag[d$token == "p01"]), "")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "5 tokens flagged: fewer than 11 points with f0. Reasons are in a new screen_flag column. The new",
      "column replaces the file's own. Points without f0 inside: p02, p03, p04"), fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_drop_flagged = TRUE)
    expect_false("screen_flag" %in% names(dataset()))
    rep <- start_text(output$f0_screen_report)
    expect_no_match(rep, "replaces", fixed = TRUE)
    # The drop takes the column with it, and the callout says so last
    expect_match(rep, paste("Fewer than 3 points with f0: p06 The file's own screen_flag column is left out.",
                            "Your raw file is not changed"), fixed = TRUE)
    expect_match(start_html(output$f0_screen_report),
                 "<div>The file's own <code>screen_flag</code> column is left out.</div>", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })

  # Nothing flagged now (every point has f0): the old column goes, the
  # card is the one for a file where every row has f0, and the copy
  # differs from the file, so the save note shows
  fx <- start_points_fixture()
  fx$f0 <- fx$truth
  fx$screen_flag <- ifelse(fx$token == "p01", "old reason", "")
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "old_only.csv"))
    expect_false(screen_plan()$flag)
    expect_false(output$screen_show_drop_flagged)
    expect_false(output$screen_show_remove)
    expect_false("screen_flag" %in% names(dataset()))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "Data screening Every row has f0 All good: every row has f0. No token is flagged now, so the",
      "file's own screen_flag column is left out. Your raw file is not changed"), fixed = TRUE)
    expect_identical(start_text(output$ui_f0_screen_hint), paste(
      "Every row has f0. See the report above the Data Preview. The other tabs start with these f0",
      "and token variables."))
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("equal points: no f0 at all, rows without a place, blank names and the rows tile", {
  # The f0 variable has no f0 at all: removal leaves no rows, and the card,
  # the sidebar and the toast say to choose another. Every token is
  # flagged (no f0), with removal ticked too, and there is no tile, so the
  # lead ends with a full stop. Removal takes every flagged token's rows,
  # so the callout says so instead of naming the column and the box.
  fx <- start_user_fixture()
  fx$f0 <- NA_real_
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "no_f0.csv"))
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "No row has f0 in column f0. Choose another f0 variable on the Start page.")
    expect_identical(start_text(output$ui_f0_screen_hint), paste(
      "No row has f0 in this f0 variable. Choose another f0 variable. See the report above the Data",
      "Preview. The other tabs start with these f0 and token variables."))
    expect_true(output$screen_show_drop_flagged)
    expect_identical(nrow(screen_plan()$flagged), 12L)
    expect_true(all(screen_plan()$flagged$reason == "no f0"))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "132 rows 132 without f0 see below", fixed = TRUE)
    expect_match(rep, paste(
      "132 of 132 rows (100.0%) have no f0 in column f0. Removed: no rows left. Choose another f0",
      "variable in the sidebar. 12 tokens flagged: fewer than 11 points with f0. Fewer than 3 points",
      "with f0: p01, p02, p03, p04, p05 and 7 more They have no f0, so removal takes their rows. Your",
      "raw file is not changed"), fixed = TRUE)
    expect_match(start_html(output$f0_screen_report),
                 "<div>They have no f0, so removal takes their rows.</div>", fixed = TRUE)
    expect_no_match(rep, "Reasons are in", fixed = TRUE)
    expect_no_match(rep, "Dropping them", fixed = TRUE)
    expect_no_match(rep, "in tokens with no f0", fixed = TRUE)
    expect_no_match(start_html(output$f0_screen_report), "st-where", fixed = TRUE)
    expect_identical(nrow(dataset()), 0L)
    start_code_ok(s, f, start_settings(), dataset())
    # Removal unticked: the same flags, now with their rows, so the
    # column and the box
    session$setInputs(screen_remove = FALSE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "12 tokens flagged: fewer than 11 points with f0. Reasons are in a new screen_flag column. Fewer",
      "than 3 points with f0: p01, p02, p03, p04, p05 and 7 more Dropping them would leave no rows."),
      fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_drop_flagged = TRUE)
    expect_identical(nrow(dataset()), 0L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste(
      "No rows left: every token is flagged. Untick Drop the 12 flagged tokens to keep them. Dropped: 12",
      "flagged tokens (132 rows), fewer than 11 points with f0: Fewer than 3 points with f0: p01, p02,",
      "p03, p04, p05 and 7 more"), fixed = TRUE)
    expect_match(start_html(output$f0_screen_report), "st-decision-stop", fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# No rows are left"))
    # Removal ticked again with the drop: the same stop line
    session$setInputs(screen_remove = TRUE)
    expect_match(start_text(output$f0_screen_report),
                 "No rows left: every token is flagged. Untick Drop the 12 flagged tokens to keep them.",
                 fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })

  # A row without f0 and with no time (p08's point 4) cannot be placed, so
  # it flags no token; removal takes it, after the drop too
  fx <- start_user_fixture()
  i <- which(fx$token == "p08")[4]
  fx$time[i] <- NA
  fx$f0[i] <- NA
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "unplaced.csv"))
    expect_identical(nrow(screen_plan()$flagged), 5L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "132 rows 17 without f0 see below", fixed = TRUE)
    expect_match(rep, "1 without a place they or their token's samples have no time Removed: 17 rows without f0.",
                 fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_drop_flagged = TRUE)
    expect_identical(screen_plan()$without, 1L)
    expect_identical(nrow(dataset()), 76L)
    expect_false(anyNA(dataset()$f0))
    rep <- start_text(output$f0_screen_report)
    # After a partial drop, the rows the removal takes: fewer than the
    # lead's 17, and "row" at n = 1
    expect_match(rep, "17 of 132 rows (12.9%) have no f0", fixed = TRUE)
    expect_match(rep, paste("Removed: 1 row without f0. voiced_run numbers the stretches with f0 in each",
                            "token, so plots break the line at gaps."), fixed = TRUE)
    expect_match(rep, "Dropped: 5 flagged tokens (55 rows), fewer than 11 points with f0:", fixed = TRUE)
    expect_no_match(rep, "All good", fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# Remove the row without f0 and add voiced_run"))
  })

  # A blank token name shows as "(no name)", not as an empty chip; the
  # Your data tile counts rows, as for frames
  fx <- start_user_fixture()
  fx$token[fx$token == "p04"] <- ""
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "blank.csv"))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "12 tokens 2 speakers 132 rows 16 without f0 see below", fixed = TRUE)
    expect_match(rep, "Points without f0 inside: p02, p03, (no name) No f0 at the first", fixed = TRUE)
    html <- start_html(output$f0_screen_report)
    expect_match(html, '<code class="st-tok">(no name)</code></li>', fixed = TRUE)
    expect_no_match(html, '<code class="st-tok"></code>', fixed = TRUE)
    session$setInputs(screen_drop_flagged = TRUE)
    expect_no_match(start_html(output$f0_screen_report), '<code class="st-tok"></code>', fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("equal points: the Exclude box can change the file's kind, and the toast names only that box", {
  # p99, a discarded token of 9 rows: excluded, the file is an equal-point
  # file; kept, it is not. Removal is ticked either way. The short runs
  # (p07's point 1, cut off by a gap) follow the kind: the toast names the
  # box that was clicked, not the short runs that followed.
  fx <- start_user_fixture()
  fx$f0[fx$token == "p07"][2] <- NA
  fx$token_dropped <- FALSE
  p99 <- fx[fx$token == "p01", ][1:9, ]
  p99$token <- "p99"
  p99$token_dropped <- TRUE
  f <- start_write_csv(rbind(fx, p99))
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "mode.csv"))
    expect_true(screen_plan()$points)
    expect_true(screen_box("remove"))
    start_eq_boxes(output)
    expect_identical(nrow(dataset()), 115L)
    start_code_ok(s, f, start_settings(), dataset())
    normalised_data(data.frame(x = 1))
    session$setInputs(exclude_dropped = FALSE)
    expect_false(screen_plan()$points)
    expect_true(screen_plan()$remove)
    expect_true(output$screen_show_short)
    expect_identical(data_version(), 1L)
    msg <- start_toast_text(s$rec, "start_data_cleared")
    expect_match(msg, "On the Start page, \"Exclude discarded tokens\" was unticked. These results",
                 fixed = TRUE)
    expect_no_match(msg, "Remove rows without f0", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    # The short runs ticked in the frame kind: removed
    n_frame <- nrow(dataset())
    session$setInputs(screen_short = TRUE)
    expect_true(screen_plan()$short)
    expect_lt(nrow(dataset()), n_frame)
    expect_identical(data_version(), 2L)
    start_code_ok(s, f, start_settings(), dataset())
    # Back to the equal-point kind: the short runs stay, and only the
    # Exclude box is named
    normalised_data(data.frame(x = 1))
    session$setInputs(exclude_dropped = TRUE)
    expect_true(screen_plan()$points)
    expect_false(screen_plan()$short)
    expect_true(screen_box("short"))
    expect_identical(nrow(dataset()), 115L)
    expect_identical(data_version(), 3L)
    msg <- start_toast_text(s$rec, "start_data_cleared")
    expect_match(msg, "\"Exclude discarded tokens\" was ticked.", fixed = TRUE)
    expect_no_match(msg, "Also remove runs", fixed = TRUE)
    expect_no_match(msg, "Remove rows without f0", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })
  # The change text names the boxes whose values changed (start_state()
  # keeps the boxes as the user left them)
  env <- start_server()$env
  old <- list(remove = TRUE, short = TRUE, drop_flagged = FALSE)
  expect_identical(env$start_change_text(old, list(remove = FALSE, short = TRUE, drop_flagged = FALSE)),
                   "\"Remove rows without f0\" was unticked")
  expect_identical(env$start_change_text(old, list(remove = TRUE, short = TRUE, drop_flagged = TRUE)),
                   "\"Drop the flagged tokens\" was ticked")
  expect_identical(env$start_change_text(old, list(remove = TRUE, short = FALSE, drop_flagged = FALSE)),
                   "\"Also remove runs of 1\u20132 samples\" was unticked")
  expect_identical(env$start_change_text(old, old), "the data changed")
})

test_that("excluding the discarded tokens alone shows the save note", {
  # Every row has f0; A3 is discarded. Excluded, the app's copy has fewer
  # rows and no token_dropped column, so it differs from the file
  mk <- start_frame_tok
  fx <- rbind(mk("A1", c(200, 201, 202, 203)), mk("A2", c(190, 191, 192)),
              mk("A3", c(150, 151, 152, 153, 154)))
  fx$token_dropped <- fx$token == "A3"
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "excl.csv"))
    expect_identical(nrow(dataset()), 7L)
    expect_false("token_dropped" %in% names(dataset()))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("All good: every row has f0. Your raw file is not changed The boxes change only",
                            "the app's copy of the data."), fixed = TRUE)
    # Unticked, the copy is the file: no note
    session$setInputs(exclude_dropped = FALSE)
    expect_identical(nrow(dataset()), 12L)
    expect_no_match(start_text(output$f0_screen_report), "Your raw file is not changed", fixed = TRUE)
  })
  # Without a token column, the same
  fx$token <- NULL
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "excl_no_token.csv"))
    expect_null(f0_screen()$screen)
    expect_match(start_text(output$f0_screen_report), "Your raw file is not changed", fixed = TRUE)
    session$setInputs(exclude_dropped = FALSE)
    expect_no_match(start_text(output$f0_screen_report), "Your raw file is not changed", fixed = TRUE)
  })
})

test_that("equal points: a file's voiced_run column numbers the stretches as before", {
  # voiced_run goes up by 1 after the gap, as F0 Extraction numbers it.
  # Removal keeps that numbering on the rows left; unticked, the file's
  # column stays as it is
  mk <- function(tk, miss) {
    pts <- 1:9
    x <- data.frame(token = tk, time = round(0.1 + 0.2 * (pts - 1) / 8, 4), f0 = 200 + 2 * pts,
                    speaker = "s1", tone = "T1", stringsAsFactors = FALSE)
    x$f0[miss] <- NA
    h <- !is.na(x$f0)
    r <- cumsum(h & !c(FALSE, utils::head(h, -1)))
    r[!h] <- NA
    x$voiced_run <- r
    x
  }
  f <- start_write_csv(rbind(mk("a", 4), mk("b", integer(0)), mk("c", integer(0))))
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "vrun.csv"))
    expect_true(screen_plan()$remove)
    expect_identical(screen_plan()$flagged$reason, "1 point without f0 inside")
    d <- dataset()
    expect_identical(nrow(d), 26L)
    expect_identical(d$voiced_run[d$token == "a"], c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("1 token flagged: fewer than 9 points with f0. The reason is in a new",
                            "screen_flag column. Points without f0 inside: a Tick Drop the flagged token to",
                            "leave it out."), fixed = TRUE)
    expect_no_match(rep, "The file's voiced_run column marks", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_remove = FALSE)
    d <- dataset()
    expect_identical(nrow(d), 27L)
    expect_identical(d$voiced_run[d$token == "a"], c(1L, 1L, 1L, NA, 2L, 2L, 2L, 2L, 2L))
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("21-point data with an edge row missing: the rows are removed, and the two tokens flagged", {
  sample <- system.file("app", "www", "dc21f0_test.csv", package = "shinytone")
  skip_if(!nzchar(sample), "sample not found")
  d <- utils::read.csv(sample, stringsAsFactors = FALSE)
  d$f0_Hz[c(1, 21 * 5)] <- NA
  f <- start_write_csv(d)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "dc21_edges.csv"))
    expect_true(screen_box("remove"))
    expect_true(screen_plan()$remove)
    start_eq_boxes(output)
    expect_equal(nrow(dataset()), 38806L)
    expect_true("voiced_run" %in% names(dataset()))
    toks <- unique(d$token)[c(1, 5)]
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste0(
      "Removed: 2 rows without f0. voiced_run numbers the stretches with f0 in each token, so plots ",
      "break the line at gaps. 2 tokens flagged: fewer than 21 points with f0. Reasons are in a new ",
      "screen_flag column. No f0 at the first or last point: ", toks[1], ", ", toks[2],
      " Tick Drop the 2 flagged tokens to leave them out."), fixed = TRUE)
    expect_identical(n_hits(rep, "To save that copy"), 1L)
    for (w in c("Export again", "Missing first or last points", "Kept", "The time axes run", "Chao numerals",
                "stay the same", "fill", "token_t01", "time_prop", "stretched", "20/19")) {
      expect_no_match(rep, w, fixed = TRUE)
    }
    d1 <- dataset()
    expect_identical(unique(d1$screen_flag[d1$token == toks[1]]), "no f0 at the first point")
    expect_identical(unique(d1$screen_flag[d1$token == toks[2]]), "no f0 at the last point")
    expect_identical(sum(d1$screen_flag != ""), 40L)
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "2 of 38,808 rows have no f0 and are removed. 2 tokens are flagged. See the Start page.")
    expect_match(start_text(output$ui_f0_screen_hint), "2 of 38,808 rows have no f0. See the report",
                 fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_drop_flagged = TRUE)
    expect_equal(nrow(dataset()), 38808L - 42L)
    expect_identical(data_version(), 1L)
    expect_match(start_text(output$f0_screen_report),
                 "All good: every row left has f0. Dropped: 2 flagged tokens (42 rows)",
                 fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("frames: a token with 2 samples and a token with no f0 are flagged, whatever removal says", {
  fx <- rbind(start_padded_fixture(), start_frame_tok("S3_two", c(NA, 180, NA, 182, NA)))
  f <- start_write_csv(fx)
  s <- start_server()
  upd <- new.env()
  s$env$updateCheckboxInput <- function(session, inputId, label = NULL, value = NULL) {
    upd[[inputId]] <- list(label = label, value = value)
  }
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "frames.csv"))
    expect_false(isTRUE(screen_plan()$points))
    # Removal is ticked, and S2_b (no f0) is flagged all the same, in the
    # order of the file
    expect_true(screen_box("remove"))
    expect_identical(screen_plan()$flagged$token, c("S2_b", "S3_two"))
    expect_identical(screen_plan()$flagged$reason, c("no f0", "only 2 samples with f0"))
    expect_identical(screen_plan()$n_flag_rows, 9L)
    expect_true(output$screen_show_drop_flagged)
    expect_false(screen_box("drop_flagged"))
    ui <- start_html(output$ui_f0_screen)
    expect_no_match(start_box_html(ui, "screen_drop_flagged"), "checked", fixed = TRUE)
    expect_match(start_text(output$ui_f0_screen), "Drop the 2 flagged tokens Remove 24 rows without f0",
                 fixed = TRUE)
    expect_identical(upd$screen_drop_flagged$label, "Drop the 2 flagged tokens")
    d <- dataset()
    # Removal takes S2_b's rows, as they have no f0
    expect_false("S2_b" %in% d$token)
    expect_identical(unique(d$screen_flag[d$token == "S3_two"]), "only 2 samples with f0")
    expect_identical(unique(d$screen_flag[d$token != "S3_two"]), "")
    rep <- start_text(output$f0_screen_report)
    # The flag callout in one line: the count, what the tokens share, the
    # tokens inline, the column and the box, by its label; no list
    expect_match(rep, paste("Removed: 24 rows without f0. voiced_run numbers the stretches with f0 in each",
                            "token, so plots break the line at gaps. 2 tokens flagged: fewer than 3 samples",
                            "with f0 (S2_b, S3_two). Reasons are in a new screen_flag column. Tick Drop the 2",
                            "flagged tokens to leave them out."), fixed = TRUE)
    html <- start_html(output$f0_screen_report)
    expect_match(html, paste0('<div><strong>2 tokens flagged:</strong> fewer than 3 samples with f0 (<code ',
                              'class="st-tok">S2_b</code>, <code class="st-tok">S3_two</code>). Reasons are in ',
                              'a new <code>screen_flag</code> column. Tick <em>Drop the 2 flagged tokens</em> ',
                              'to leave them out.</div>'), fixed = TRUE)
    expect_no_match(html, "st-flag-list", fixed = TRUE)
    expect_no_match(rep, "(only 2 samples with f0)", fixed = TRUE)
    expect_no_match(rep, "Fewer than 3 samples", fixed = TRUE)
    # Tiles and lines as before, and the save note last; no tile for
    # S2_b's rows
    expect_match(rep, "To save that copy, download it in Normalise or Curate.", fixed = TRUE)
    expect_no_match(rep, "What counts as no f0", fixed = TRUE)
    for (w in c("in tokens with no f0", "which removal drops", "which the drop leaves out",
                "Missing first or last points")) {
      expect_no_match(rep, w, fixed = TRUE)
    }
    expect_identical(n_hits(rep, "fewer than 3 samples"), 1L)
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     paste("24 of 50 rows have no f0 (2 of them 0 Hz) and are removed. 2 tokens are flagged.",
                           "See the Start page."))
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == '# Flag 2 tokens in a screen_flag column ("" for the other tokens)'))
    expect_true(any(code == "# Remove the 24 rows without f0 and add voiced_run"))
    expect_false(any(grepl("flagged too", code, fixed = TRUE)))
    expect_false(any(grepl("S3_two|S2_b", code)))

    # Removal unticked: the same two tokens, and the drop box as it was
    dv <- data_version()
    session$setInputs(screen_remove = FALSE)
    expect_identical(data_version(), dv + 1L)
    expect_false(screen_box("drop_flagged"))
    expect_identical(screen_plan()$flagged$token, c("S2_b", "S3_two"))
    expect_identical(upd$screen_drop_flagged$label, "Drop the 2 flagged tokens")
    d <- dataset()
    expect_identical(unique(d$screen_flag[d$token == "S2_b"]), "no f0")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("Kept: rows without f0 stay. Tick Remove rows without f0 to remove them. 2 tokens",
                            "flagged: fewer than 3 samples with f0 (S2_b, S3_two). Reasons are in a new",
                            "screen_flag column. Tick Drop the 2 flagged tokens to leave them out."), fixed = TRUE)
    expect_no_match(rep, "in tokens with no f0", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())

    # The drop ticked: both go, and the column with them
    normalised_data(data.frame(x = 1))
    dv <- data_version()
    session$setInputs(screen_drop_flagged = TRUE)
    expect_identical(data_version(), dv + 1L)
    expect_false(screen_box("remove"))
    expect_null(normalised_data())
    d <- dataset()
    expect_false(any(c("S2_b", "S3_two") %in% d$token))
    expect_false("screen_flag" %in% names(d))
    expect_match(start_toast_text(s$rec, "start_data_cleared"), "\"Drop the flagged tokens\" was ticked",
                 fixed = TRUE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("Kept: rows without f0 stay. Tick Remove rows without f0 to remove them. Dropped:",
                            "2 flagged tokens (9 rows), fewer than 3 samples with f0 (S2_b, S3_two)."),
                 fixed = TRUE)
    expect_match(start_html(output$f0_screen_report),
                 "<strong>Dropped:</strong> 2 flagged tokens (9 rows), fewer than 3 samples with f0 (",
                 fixed = TRUE)
    expect_no_match(rep, "Tick Drop", fixed = TRUE)
    expect_no_match(rep, "Untick", fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# Drop the 2 flagged tokens (9 rows)"))
    expect_true(any(code == 'dat <- dat[dat[["screen_flag"]] == "", , drop = FALSE]'))

    # Removal ticked again with the drop: the drop still takes both, and
    # the removal the 17 rows without f0 left, fewer than the lead's 24
    session$setInputs(screen_remove = TRUE)
    expect_identical(screen_plan()$flagged$token, c("S2_b", "S3_two"))
    expect_identical(upd$screen_drop_flagged$label, "Drop the 2 flagged tokens")
    expect_identical(upd$screen_remove$label, "Remove 17 rows without f0 (recommended)")
    d <- dataset()
    expect_false(any(c("S2_b", "S3_two") %in% d$token))
    expect_false("screen_flag" %in% names(d))
    expect_false(anyNA(d$f0))
    expect_identical(nrow(d), 50L - 9L - 17L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "24 of 50 rows (48.0%) have no f0 in column f0:", fixed = TRUE)
    expect_match(rep, paste("Removed: 17 rows without f0. voiced_run numbers the stretches with f0 in each",
                            "token, so plots break the line at gaps. Dropped: 2 flagged tokens (9 rows), fewer",
                            "than 3 samples with f0 (S2_b, S3_two)."), fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# Drop the 2 flagged tokens (9 rows)"))
    expect_true(any(code == "# Remove the 17 rows without f0 and add voiced_run"))
    expect_false(any(grepl("no f0\"", code, fixed = TRUE)))

    # A new upload unticks the drop again, and does not count
    dv <- data_version()
    session$setInputs(uploadfile = start_upload(f, "again.csv"))
    expect_false(screen_box("drop_flagged"))
    expect_true("S3_two" %in% dataset()$token)
    expect_identical(data_version(), dv)
  })
})

test_that("frames: a flagged token in a file where every row has f0, and a file of one token", {
  # Every row has f0, and one token has only 2 rows
  fx <- rbind(start_frame_tok("a1", 200 + 0:9), start_frame_tok("a2", 180 + 0:7),
              start_frame_tok("tw", c(150, 151)))
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "voiced.csv"))
    expect_identical(screen_plan()$action, "none")
    expect_identical(screen_plan()$flagged$reason, "only 2 samples with f0")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("Every row has f0 All good: every row has f0. 1 token flagged: fewer than 3",
                            "samples with f0 (tw). The reason is in a new screen_flag column. Tick Drop the",
                            "flagged token to leave it out. Your raw file is not changed"), fixed = TRUE)
    expect_match(start_html(output$f0_screen_report),
                 paste0("<div><strong>1 token flagged:</strong> fewer than 3 samples with f0 (<code ",
                        'class="st-tok">tw</code>). The reason is in a new <code>screen_flag</code> column. ',
                        "Tick <em>Drop the flagged token</em> to leave it out.</div>"), fixed = TRUE)
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"), "1 token is flagged. See the Start page.")
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_drop_flagged = TRUE)
    expect_identical(nrow(dataset()), 18L)
    expect_false("screen_flag" %in% names(dataset()))
    start_code_ok(s, f, start_settings(), dataset())
  })

  # One token, with 2 samples: the drop would take every row
  f <- start_write_csv(start_frame_tok("x", c(NA, 200, NA, 201, NA)))
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "one.csv"))
    expect_match(start_text(output$ui_f0_screen), "Drop the flagged token (the only token)", fixed = TRUE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("1 token flagged: fewer than 3 samples with f0 (x). The reason is in a new",
                            "screen_flag column. Dropping it would leave no rows."), fixed = TRUE)
    expect_no_match(rep, "Tick Drop", fixed = TRUE)
    session$setInputs(screen_drop_flagged = TRUE)
    expect_identical(nrow(dataset()), 0L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "No rows left: the only token is flagged. Untick Drop the flagged token to keep it.",
                 fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("frames: many flagged tokens follow inline, 5 and then the rest counted", {
  # z0 has no f0 (first in the file); f01-f08 have 2 samples with f0 each;
  # g1 and g2 are clean
  mk <- start_frame_tok
  fx <- rbind(mk("z0", rep(NA, 4)),
              do.call(rbind, lapply(sprintf("f%02d", 1:8), function(tk) mk(tk, c(NA, 180, NA, 182, NA)))),
              mk("g1", 200 + 0:9), mk("g2", 190 + 0:11))
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "frames_many.csv"))
    # z0 (no f0) is flagged with the others, first as in the file
    expect_identical(nrow(screen_plan()$flagged), 9L)
    expect_identical(screen_plan()$flagged$token[1], "z0")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("9 tokens flagged: fewer than 3 samples with f0 (z0, f01, f02, f03, f04 and 4",
                            "more). Reasons are in a new screen_flag column. Tick Drop the 9 flagged tokens",
                            "to leave them out."), fixed = TRUE)
    html <- start_html(output$f0_screen_report)
    expect_no_match(html, "st-flag-list", fixed = TRUE)
    expect_match(html, '<code class="st-tok">f04</code> and 4 more). Reasons are in a new', fixed = TRUE)
    expect_identical(n_hits(sub(".*st-callout-info", "", html), '<code class="st-tok">'), 5L)
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "28 of 66 rows have no f0 and are removed. 9 tokens are flagged. See the Start page.")
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_false(any(grepl("f0[0-9]|z0", code)))
    # Removal unticked: the same 9
    session$setInputs(screen_remove = FALSE)
    expect_identical(screen_plan()$flagged$token[1], "z0")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("9 tokens flagged: fewer than 3 samples with f0 (z0, f01, f02, f03, f04 and 4",
                            "more). Reasons are in a new screen_flag column. Tick Drop the 9 flagged tokens",
                            "to leave them out."), fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_drop_flagged = TRUE)
    expect_identical(nrow(dataset()), 22L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("All good: every row left has f0. Dropped: 9 flagged tokens (44 rows), fewer than",
                            "3 samples with f0 (z0, f01, f02, f03, f04 and 4 more)."), fixed = TRUE)
    expect_no_match(rep, "Untick", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("the app has no note on missing first or last points and no equal-point removal switch", {
  app <- system.file("app", package = "shinytone")
  for (fl in c("server.R", "global.R", file.path("ui", "start_ui.R"))) {
    src <- readLines(file.path(app, fl), warn = FALSE, encoding = "UTF-8")
    expect_false(any(grepl("Missing first or last points|Export again in F0 Processing|edge_more", src)),
                 info = fl)
    expect_false(any(grepl("screen_equal|remove_ok|mark_ok", src)), info = fl)
  }
})

test_that("when removal takes every flagged token, the callout says so", {
  mk <- start_frame_tok
  # Frames: two tokens with no f0 are the only flagged tokens; b1 has a
  # row without f0 inside, which flags nothing in a frame file
  fx <- rbind(mk("a1", c(NA, NA, NA, NA, NA)), mk("b1", c(190, 191, NA, 193, 194, 195)),
              mk("z1", c(NA, NA, NA)), mk("c1", c(180, 181, 182, 183, 184)))
  f <- start_write_csv(fx)
  s <- start_server()
  upd <- new.env()
  s$env$updateCheckboxInput <- function(session, inputId, label = NULL, value = NULL) {
    upd[[inputId]] <- list(label = label, value = value)
  }
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "two_no_f0.csv"))
    expect_identical(screen_plan()$flagged$token, c("a1", "z1"))
    d <- dataset()
    expect_false(any(c("a1", "z1") %in% d$token))
    expect_false(any(nzchar(d$screen_flag)))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("Removed: 9 rows without f0. voiced_run numbers the stretches with f0 in each",
                            "token, so plots break the line at gaps. 2 tokens flagged: fewer than 3 samples",
                            "with f0 (a1, z1). They have no f0, so removal takes their rows."), fixed = TRUE)
    expect_no_match(rep, "Reasons are in", fixed = TRUE)
    expect_no_match(rep, "Tick Drop", fixed = TRUE)
    # The box, the toast and the sidebar still count them
    expect_true(output$screen_show_drop_flagged)
    expect_identical(upd$screen_drop_flagged$label, "Drop the 2 flagged tokens")
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "9 of 19 rows have no f0 and are removed. 2 tokens are flagged. See the Start page.")
    html <- start_html(output$f0_screen_report)
    expect_match(html, '<code class="st-tok">z1</code>). They have no f0, so removal takes their rows.',
                 fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    # Removal unticked: their rows stay, with the reason
    session$setInputs(screen_remove = FALSE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("2 tokens flagged: fewer than 3 samples with f0 (a1, z1). Reasons are in a new",
                            "screen_flag column. Tick Drop the 2 flagged tokens to leave them out."), fixed = TRUE)
    expect_no_match(rep, "so removal takes", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    # Dropped: the Dropped line, and removal takes b1's row
    session$setInputs(screen_remove = TRUE, screen_drop_flagged = TRUE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("Removed: 1 row without f0. voiced_run numbers the stretches with f0 in each",
                            "token, so plots break the line at gaps. Dropped: 2 flagged tokens (8 rows), fewer",
                            "than 3 samples with f0 (a1, z1)."), fixed = TRUE)
    expect_no_match(rep, "so removal takes", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })

  # The file's own screen_flag column, and the only flagged token has no
  # f0: the new column replaces it, and removal leaves it with no reason
  fx <- rbind(mk("a1", c(NA, NA, NA, NA, NA)), mk("b1", c(190, 191, NA, 193, 194, 195)),
              mk("c1", c(180, 181, 182, 183, 184)))
  fx$screen_flag <- ifelse(fx$token == "c1", "old reason", "")
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "own_flag.csv"))
    expect_true(screen_plan()$old_flag)
    d <- dataset()
    expect_identical(unique(d$screen_flag), "")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("1 token flagged: fewer than 3 samples with f0 (a1). It has no f0, so removal",
                            "takes its rows. The file's own screen_flag column is cleared."), fixed = TRUE)
    expect_no_match(rep, "replaces", fixed = TRUE)
    expect_match(start_html(output$f0_screen_report),
                 "so removal takes its rows. The file's own <code>screen_flag</code> column is cleared.", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_remove = FALSE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("The reason is in a new screen_flag column. The new column replaces the file's",
                            "own. Tick Drop the flagged token to leave it out."), fixed = TRUE)
    expect_no_match(rep, "cleared", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("gaps only the time steps show: 'more' and 'too' only after the inside tile", {
  mk <- start_frame_tok
  g1 <- mk("g1", 200 + 1:8)
  # A row without f0 at g3's start only: no inside tile
  fx <- rbind(g1[-4, ], mk("g3", c(NA, 191:197)))
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "edge_gaps.csv"))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "1 at the start of 1 token Removed: 1 row without f0.", fixed = TRUE)
    expect_match(rep, "The time steps show 1 gap in 1 token, where the file has no rows. voiced_run marks it.",
                 fixed = TRUE)
    expect_no_match(rep, "more gap", fixed = TRUE)
    expect_no_match(rep, "marks it too", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
    session$setInputs(screen_remove = FALSE)
    rep <- start_text(output$f0_screen_report)
    # Nothing follows the gap sentence: no "voiced_run marks it"
    expect_match(rep, "The time steps show 1 gap in 1 token, where the file has no rows\\.\\s*$")
  })
  # A row without f0 inside g3: the inside tile, then "more" and "too"
  fx <- rbind(g1[-4, ], mk("g3", c(190, 191, NA, 193:197)))
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "inside_gaps.csv"))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "1 inside 1 token, in 1 gap Removed: 1 row without f0.", fixed = TRUE)
    expect_match(rep, paste("The time steps show 1 more gap in 1 token, where the file has no rows.",
                            "voiced_run marks it too."), fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("the app has no tile for tokens with no f0 and no rule that flags them only sometimes", {
  # A token with no f0 is flagged like the others, whatever the removal box
  # says, and the Removed line counts the rows removed, not the rows left
  app <- system.file("app", package = "shinytone")
  for (fl in c("server.R", "global.R", file.path("ui", "start_ui.R"))) {
    src <- readLines(file.path(app, fl), warn = FALSE, encoding = "UTF-8")
    expect_false(any(grepl(paste("keep_no_f0|in tokens with no f0|No f0 at all|which removal drops",
                                 "which the drop leaves out|Nothing left to remove|flagged too|, leaving",
                                 "flagged or has no f0|as no row has f0", sep = "|"), src)), info = fl)
  }
})

test_that("the drop can leave only gaps the time steps show, which voiced_run then marks", {
  # A has 2 samples (flagged); B has 2 frames missing, which only the time
  # steps show; C is clean. With the drop, no row without f0 is left, so
  # the mark box takes over from removal.
  mk <- start_frame_tok
  b <- mk("B", 200 + 1:12)
  fx <- rbind(mk("A", c(NA, 200, 201, NA)), b[-c(5, 6), ], mk("C", 180 + 1:9))
  f <- start_write_csv(fx)
  s <- start_server()
  upd <- new.env()
  s$env$updateCheckboxInput <- function(session, inputId, label = NULL, value = NULL) {
    upd[[inputId]] <- list(label = label, value = value)
  }
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "frames_mark.csv"))
    normalised_data(data.frame(x = 1))
    session$setInputs(screen_drop_flagged = TRUE)
    expect_true(screen_plan()$mark)
    expect_false(screen_plan()$remove)
    # The toast names the box that was clicked, not the removal and marking
    # that followed it
    expect_identical(start_toast_text(s$rec, "start_data_cleared"),
                     paste("On the Start page, \"Drop the flagged tokens\" was ticked. These results came",
                           "from the old rows, so they were cleared: normalised data."))
    d <- dataset()
    expect_identical(max(d$voiced_run[d$token == "B"]), 2L)
    rep <- start_text(output$f0_screen_report)
    # The gaps go in the All good line, as when every row has f0, and no
    # note repeats them ("more" and "too" would follow nothing)
    expect_match(rep, paste("All good: every row left has f0. The time steps show 1 gap in 1 token.",
                            "voiced_run numbers the stretches with f0 in each token, so plots break the",
                            "line at gaps. Dropped:"), fixed = TRUE)
    expect_no_match(rep, "where the file has no rows", fixed = TRUE)
    expect_no_match(rep, "more gap", fixed = TRUE)
    expect_no_match(rep, "marks it", fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# Every row left has f0. Mark the 1 gap the time steps show in voiced_run."))
    # The mark box unticked: the All good line says to tick it, by its
    # label, at n = 1
    session$setInputs(screen_mark = FALSE)
    expect_false("voiced_run" %in% names(dataset()))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("All good: every row left has f0. The time steps show 1 gap in 1 token. Tick",
                            "Mark the 1 gap to mark it in a voiced_run column. Dropped:"), fixed = TRUE)
    expect_no_match(rep, "voiced_run numbers", fixed = TRUE)
    expect_no_match(rep, "where the file has no rows", fixed = TRUE)
    expect_identical(upd$screen_mark$label, "Mark the 1 gap in a voiced_run column (recommended)")
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("a screen_flag column the file already has is replaced, or left out when nothing is flagged", {
  mk <- start_frame_tok
  old <- rbind(mk("A", c(NA, 200, 201, 202, NA)), mk("B", c(NA, 190, 191, 192, 193, NA)))
  old$screen_flag <- ifelse(old$token == "A", "no f0 at the first point", "")
  # Nothing is flagged now: the old column goes, and the card says so
  f <- start_write_csv(old)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "old_flags.csv"))
    expect_false(screen_plan()$flag)
    expect_true(screen_plan()$old_flag)
    expect_false(output$screen_show_drop_flagged)
    expect_false("screen_flag" %in% names(dataset()))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "No token is flagged now, so the file's own screen_flag column is left out.", fixed = TRUE)
    # The copy differs from the file (a column left out, rows removed)
    expect_match(rep, "column is left out. Your raw file is not changed", fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# No token is flagged now, so the file's own screen_flag column goes"))
    expect_true(any(code == 'dat[["screen_flag"]] <- NULL'))
    expect_false(any(grepl("flag_f0_tokens", code, fixed = TRUE)))
    # With removal unticked too
    session$setInputs(screen_remove = FALSE)
    expect_false("screen_flag" %in% names(dataset()))
    start_code_ok(s, f, start_settings(), dataset())
  })
  # A token is flagged now: the column is replaced, and the callout says so
  # briefly
  f <- start_write_csv(rbind(old, cbind(mk("C", c(NA, 150, 151, NA)), screen_flag = "x")))
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "old_flags2.csv"))
    d <- dataset()
    expect_identical(unique(d$screen_flag[d$token != "C"]), "")
    expect_identical(unique(d$screen_flag[d$token == "C"]), "only 2 samples with f0")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("1 token flagged: fewer than 3 samples with f0 (C). The reason is in a new",
                            "screen_flag column. The new column replaces the file's own. Tick Drop the",
                            "flagged token to leave it out."), fixed = TRUE)
    expect_no_match(rep, "No token is flagged now", fixed = TRUE)
    expect_match(start_toast_text(s$rec, "f0a_f0_screen"), "1 token is flagged. See the Start page.",
                 fixed = TRUE)
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_false(any(code == 'dat[["screen_flag"]] <- NULL'))
    session$setInputs(screen_drop_flagged = TRUE)
    expect_false("screen_flag" %in% names(dataset()))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("Dropped: 1 flagged token (4 rows), fewer than 3 samples with f0 (C). The file's",
                            "own screen_flag column is left out."), fixed = TRUE)
    expect_no_match(rep, "replaces", fixed = TRUE)
    start_code_ok(s, f, start_settings(), dataset())
  })
})

test_that("0 and -1 in an Hz column zero_hz_to_na() did not pick are read as no f0 when ticked", {
  df <- data.frame(token = rep(c("a", "b"), c(8, 9)),
                   time = c(0.005 + 0.01 * (0:7), 0.005 + 0.01 * (0:8)),
                   f0_rapt = c(0, 150, 152, 154, -1, 158, 160, 0,
                               -1, -1, 180, 181, 182, 183, 184, 185, 0))
  f <- start_write_csv(df)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_identical(start_cols()$f0, "f0_rapt")
    expect_equal(f0_screen()$suggested, c(0, -1))
    expect_true(output$screen_show_unvoiced)
    expect_match(start_html(output$ui_f0_screen), "Read 0 / -1 in f0_rapt as no f0 (6 rows)", fixed = TRUE)
    d <- dataset()
    expect_equal(nrow(d), 11L)
    expect_false(any(d$f0_rapt <= 0))
    expect_equal(max(d$voiced_run[d$token == "a"]), 2L)
    code <- s$env$read_data_code("x.csv", start_settings())
    expect_true(any(grepl("unvoiced = c(0, -1)", code, fixed = TRUE)))
    # The sidebar box names the values; the card has no rule of its own
    rep <- start_text(output$f0_screen_report)
    expect_no_match(rep, "What counts as no f0", fixed = TRUE)
    expect_no_match(rep, "count as f0", fixed = TRUE)
    # Unticked: the values stay, and there is nothing to remove
    session$setInputs(screen_unvoiced = FALSE)
    expect_equal(nrow(dataset()), 17L)
    expect_equal(sum(dataset()$f0_rapt <= 0), 6L)
    expect_identical(data_version(), 1L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "Every row has f0 All good: every row has f0.", fixed = TRUE)
    expect_no_match(rep, "count as f0", fixed = TRUE)
  })
})

test_that("a duration on the screened column reads the 0 / -1 the screen reads as no f0", {
  # Token b starts with two -1 frames: they are no f0 once the box is ticked,
  # so its duration is the span of the rows that are left (0.05 s, not 0.07)
  df <- data.frame(token = rep(c("a", "b"), c(8, 9)),
                   time = c(0.005 + 0.01 * (0:7), 0.005 + 0.01 * (0:8)),
                   f0_rapt = c(0, 150, 152, 154, -1, 158, 160, 0,
                               -1, -1, 180, 181, 182, 183, 184, 185, 0))
  f <- start_write_csv(df)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    session$setInputs(dur_unit = "__token__", dur_token = "token", dur_time = "time",
                      dur_f0 = "f0_rapt")
    session$setInputs(dur_apply = 1)
    d <- dataset()
    dur <- as.vector(tapply(d$token_f0_dur, d$token, unique))
    expect_equal(dur, c(0.05, 0.05))
    # The same as measuring dataset(), where the markers are NA
    expect_equal(contour_duration(d, f0 = "f0_rapt")$token_f0_dur, d$token_f0_dur)
    # The toast counts the same column
    expect_true(any(vapply(s$rec$toasts, function(t) grepl("range 0.05 to 0.05", t$text, fixed = TRUE),
                           logical(1))))
    # The R code measures it the same way, and reproduces dataset()
    code <- s$env$read_data_code(f, start_settings())
    expect_true(any(grepl('dur[["f0_rapt"]][dur[["f0_rapt"]] %in% c(0, -1)] <- NA', code, fixed = TRUE)))
    run <- new.env()
    eval(parse(text = code), run)
    expect_identical(run$dat, dataset())
    # Unticked, the markers count as f0 in the screen and in the duration
    session$setInputs(screen_unvoiced = FALSE)
    d <- dataset()
    expect_equal(nrow(d), nrow(df))
    expect_equal(d$token_f0_dur, contour_duration(df, f0 = "f0_rapt")$token_f0_dur)
    expect_equal(as.vector(tapply(d$token_f0_dur, d$token, unique)), c(0.05, 0.07))
    code <- s$env$read_data_code(f, start_settings())
    expect_false(any(grepl("dur <- dat", code, fixed = TRUE)))
    run <- new.env()
    eval(parse(text = code), run)
    expect_identical(run$dat, dataset())
  })
})

test_that("without a token column, rows without f0 are counted over the whole file", {
  df <- data.frame(index = rep(1:5, 2), time = rep(1:5 / 100, 2),
                   f0_Hz = c(200, 201, NA, 203, 204, 180, 181, 182, 183, 184))
  f <- start_write_csv(df)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_null(start_cols()$token)
    expect_null(f0_screen()$screen)
    expect_equal(f0_screen()$without, 1L)
    expect_true(output$screen_need_token)
    expect_false(output$screen_show_remove)
    expect_equal(nrow(dataset()), 10L)
    expect_match(start_text(output$f0_screen_report), "Choose the token variable", fixed = TRUE)
    expect_match(start_html(output$ui_f0_screen_hint), "Choose the token variable", fixed = TRUE)
    # The report says how to choose a token variable once, in the Data screening
    # card; the tiles above only say what is missing
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "No token variable Tokens not counted see below", fixed = TRUE)
    expect_match(rep, "f0 data Sampling not checked: no token variable.", fixed = TRUE)
    expect_match(rep, paste("Choose the token variable in the sidebar to see where it is and to remove it.",
                            "If the file has none, add a column that names each token."), fixed = TRUE)
    expect_identical(n_hits(tolower(rep), "choose the token variable"), 1L)
    expect_identical(n_hits(rep, "names each token"), 1L)
  })
})

test_that("dc21f0.csv: no token column, every row has f0, nothing is removed", {
  path <- normalizePath(test_path("..", "..", "test", "dc21f0.csv"), mustWork = FALSE)
  skip_if_not(file.exists(path))
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(path))
    expect_null(start_cols()$token)
    expect_identical(start_cols()$f0, "f0_Hz")
    expect_equal(f0_screen()$without, 0L)
    expect_false(output$screen_need_token)
    expect_identical(names(dataset()), names(raw_dataset()))
    expect_equal(nrow(dataset()), nrow(raw_dataset()))
    expect_match(start_text(output$f0_screen_report), "Every row has f0 in column f0_Hz", fixed = TRUE)
    # Every row has f0, so the token picker hides behind the gap check: the
    # report says to tick it first, once, in the Data screening card. The
    # token tile and the f0 data tile only say what is missing.
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "No token variable Tokens not counted see below 13 speakers", fixed = TRUE)
    expect_match(rep, "f0 data Sampling not checked: no token variable. Variables used", fixed = TRUE)
    expect_match(rep, paste("Every row has f0 in column f0_Hz (38,808 rows). There is no token variable,",
                            "so gaps inside tokens were not checked. Tick Check the time steps for gaps",
                            "in the sidebar and choose the token variable to check them. If the file has",
                            "none, add a column that names each token."), fixed = TRUE)
    expect_identical(n_hits(tolower(rep), "choose the token variable"), 1L)
    expect_identical(n_hits(rep, "Check the time steps for gaps"), 1L)
    expect_identical(n_hits(rep, "names each token"), 1L)
    expect_no_match(rep, "Choose the token variable in the sidebar", fixed = TRUE)
    expect_match(start_html(output$ui_f0_screen_hint), "Every row has f0.", fixed = TRUE)
  })
})

test_that("f0_long.csv: the screen gives the B2 counts and removal leaves 52,431 rows", {
  path <- normalizePath(test_path("..", "..", "test", "f0_long.csv"), mustWork = FALSE)
  skip_if_not(file.exists(path))
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(path))
    sm <- attr(f0_screen()$screen, "summary")
    expect_equal(sm[["rows"]], 83301L)
    expect_equal(sm[["tokens"]], 1853L)
    expect_equal(sm[["rows_without_f0"]], 30870L)
    expect_equal(sm[["start"]], 14023L)
    expect_equal(sm[["end"]], 16584L)
    expect_equal(sm[["inside"]], 263L)
    expect_equal(sm[["gaps"]], 109L)
    expect_equal(sm[["gap_tokens"]], 105L)
    expect_equal(sm[["few_tokens"]], 0L)
    expect_equal(sm[["short_runs"]], 52L)
    expect_equal(sm[["short_rows"]], 97L)
    expect_equal(sm[["short_tokens"]], 50L)
    expect_equal(c(sm[["short_tokens_start"]], sm[["short_tokens_end"]], sm[["short_tokens_both"]]),
                 c(13L, 38L, 1L))
    expect_true(screen_plan()$remove)
    d <- dataset()
    expect_equal(nrow(d), 52431L)
    vr <- tapply(d$voiced_run, d$token, max)
    expect_equal(sum(vr > 1), 105L)
    expect_equal(max(vr), 3L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "30,870 of 83,301 rows (37.1%) have no f0 in column f0:", fixed = TRUE)
    expect_match(rep, "14,023 at the start of 1,853 tokens", fixed = TRUE)
    expect_match(rep, "263 inside 105 tokens, in 109 gaps", fixed = TRUE)
    expect_no_match(rep, "52 runs", fixed = TRUE)
    # 13 + 38 count the token with runs at both ends twice: never a sum
    expect_match(rep, "In 50 tokens (13 at the start and 38 at the end; 1 token has both)", fixed = TRUE)
    # Where, said once (in the brackets), and one count
    expect_match(rep, paste0("1 token has both), a gap cuts off a run of 1", EN, "2 samples, 97 samples in ",
                             "all. These samples hold the first or last f0, so the 0", EN, "1 time axes ",
                             "start or end there. Tick Also remove runs of 1", EN, "2 samples to remove them."),
                 fixed = TRUE)
    expect_identical(n_hits(rep, "at the start or end"), 0L)
    expect_match(rep, paste("Removed: 30,870 rows without f0. voiced_run numbers the stretches with f0 in",
                            "each token, so plots break the line at gaps."), fixed = TRUE)
    expect_match(rep, "16,584 at the end of 1,853 tokens", fixed = TRUE)
    # The fewest samples a token keeps is 3: enough, so nothing is flagged
    expect_no_match(rep, "fewer than", fixed = TRUE)
    expect_no_match(rep, "flagged", fixed = TRUE)
    expect_false("screen_flag" %in% names(dataset()))
    # The R code repeats the removal
    code <- s$env$read_data_code(path, start_settings())
    run <- new.env()
    eval(parse(text = code, encoding = "UTF-8"), run)
    expect_identical(run$dat, dataset())
  })
})

test_that("\"Exclude discarded tokens\" counts as a change, and a new upload ticks it again", {
  fx <- start_padded_fixture()
  fx$token_dropped <- fx$token == "S2_c"
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "dropped.csv"))
    expect_false("S2_c" %in% dataset()$token)
    n <- nrow(dataset())
    inspect_result(list(x = 1))
    session$setInputs(exclude_dropped = FALSE)
    expect_true("S2_c" %in% dataset()$token)
    expect_identical(data_version(), 1L)
    expect_null(inspect_result())
    expect_match(start_toast_text(s$rec, "start_data_cleared"),
                 "\"Exclude discarded tokens\" was unticked", fixed = TRUE)
    # The box the user unticked for the last file does not carry over
    session$setInputs(uploadfile = start_upload(f, "dropped_again.csv"))
    expect_false("S2_c" %in% dataset()$token)
    expect_equal(nrow(dataset()), n)
    expect_identical(data_version(), 1L)
  })
})

# The option a rendered picker selects.
start_selected <- function(x, id) {
  html <- start_html(x)
  sel <- regmatches(html, regexpr(sprintf('(?s)<select id="%s".*?</select>', id), html, perl = TRUE))
  if (!length(sel)) return(NULL)
  m <- regmatches(sel, regexpr('<option value="[^"]*" selected>', sel))
  if (!length(m)) return(NULL)
  sub('<option value="([^"]*)" selected>', "\\1", m)
}

test_that("the Start columns are per upload: a file uploaded second gets its own defaults", {
  fx <- start_padded_fixture()
  f1 <- start_write_csv(fx)
  # An F0 Correction file: f0_corrected next to f0, with 2 rows deleted there
  fx2 <- fx
  fx2$f0_corrected <- fx2$f0
  del <- c(4L, 17L)
  expect_false(anyNA(fx2$f0[del]))
  fx2$f0_corrected[del] <- NA
  fx2$edited <- seq_len(nrow(fx2)) %in% del
  f2 <- start_write_csv(fx2)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f1, "plain.csv"))
    expect_identical(start_cols(), list(token = "token", time = "time", f0 = "f0"))
    # The browser sends back the values the pickers were drawn with: not picks
    session$setInputs(screen_token = "token", screen_time = "time", screen_f0 = "f0",
                      screen_remove = TRUE)
    session$setInputs(uploadfile = start_upload(f2, "all_correctedf0.csv"))
    # The stale input values ("f0") do not override the new file's default
    expect_identical(start_cols(), list(token = "token", time = "time", f0 = "f0_corrected"))
    expect_identical(start_selected(output$ui_f0_screen, "screen_f0"), "f0_corrected")
    expect_identical(start_selected(output$dur_ui, "dur_f0"), "f0_corrected")
    expect_equal(f0_screen()$without, 23L)
    expect_false(anyNA(dataset()$f0_corrected))
    expect_false(any(dataset()$edited))   # the rows deleted in F0 Correction are gone
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "have no f0 in column f0_corrected", fixed = TRUE)
    expect_match(rep, "2 were deleted in F0 Correction", fixed = TRUE)
    expect_identical(s$rec$modules$model_ui$start_cols()$f0, "f0_corrected")
    expect_identical(data_version(), 0L)

    # Within the upload: the redrawn picker's value is not a pick; the user's
    # own pick holds, and picking the default again follows the default
    session$setInputs(screen_f0 = "f0_corrected")
    expect_identical(start_cols()$f0, "f0_corrected")
    session$setInputs(screen_f0 = "f0")
    expect_identical(start_cols()$f0, "f0")
    expect_identical(data_version(), 1L)
    session$setInputs(screen_f0 = "f0_corrected")
    expect_identical(start_cols()$f0, "f0_corrected")
    expect_identical(data_version(), 2L)

    # The user's own pick does not carry over to the next upload either
    session$setInputs(screen_f0 = "intensity", screen_time = "intensity")
    expect_identical(start_cols()$f0, "intensity")
    dv <- data_version()
    session$setInputs(uploadfile = start_upload(f2, "again.csv"))
    expect_identical(start_cols(), list(token = "token", time = "time", f0 = "f0_corrected"))
    expect_identical(data_version(), dv)   # a new upload does not count
  })
})

test_that("f0_long.csv, then an F0 Correction file: the second is screened on f0_corrected", {
  path <- normalizePath(test_path("..", "..", "test", "f0_long.csv"), mustWork = FALSE)
  skip_if_not(file.exists(path))
  fl <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  sub <- fl[fl$token %in% unique(fl$token)[1:300], ]
  sub$f0_corrected <- sub$f0
  del <- which(is.finite(sub$f0))[c(5, 50, 500, 1000, 2000, 3000, 4000)]
  sub$f0_corrected[del] <- NA
  sub$edited <- seq_len(nrow(sub)) %in% del
  f2 <- tempfile(fileext = ".csv")
  utils::write.csv(sub, f2, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(path))
    session$setInputs(screen_token = "token", screen_time = "time", screen_f0 = "f0")
    session$setInputs(uploadfile = start_upload(f2, "all_correctedf0.csv"))
    expect_identical(start_cols()$f0, "f0_corrected")
    n_without <- sum(!is.finite(sub$f0)) + length(del)
    expect_equal(f0_screen()$without, n_without)
    expect_equal(nrow(dataset()), nrow(sub) - n_without)
    expect_match(start_text(output$f0_screen_report), "7 were deleted in F0 Correction", fixed = TRUE)
  })
})

test_that("without a token column, the token picker's (none) is the default, not a pick", {
  df <- data.frame(index = rep(1:5, 2), time = rep(1:5 / 100, 2),
                   f0_Hz = c(200, 201, NA, 203, 204, 180, 181, 182, 183, 184))
  f <- start_write_csv(df)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    session$setInputs(screen_token = "")
    expect_null(start_cols()$token)
    expect_identical(start_selected(output$ui_f0_screen, "screen_token"), "")
    session$setInputs(screen_token = "index")
    expect_identical(start_cols()$token, "index")
    expect_false(is.null(f0_screen()$screen))
    session$setInputs(screen_token = "")
    expect_null(start_cols()$token)
    expect_null(f0_screen()$screen)
  })
})

test_that("the Start page's pickers and notes say variable, not column", {
  pickers <- c("f0 variable:", "Token ID variable:", "Time variable:")
  old <- c("f0 column:", "Token column:", "Time column:")
  check_pickers <- function(html, what) {
    for (lab in pickers) {
      expect_identical(n_hits(html, paste0(">", lab, "</label>")), 1L, label = paste(what, lab))
    }
    for (o in old) expect_no_match(html, o, fixed = TRUE, label = paste(what, o))
  }
  # With a token column: the Data screening and duration pickers, the hint
  # under them and the report's line of the variables used
  f <- start_write_csv(start_padded_fixture())
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    check_pickers(start_html(output$ui_f0_screen), "Data screening")
    dur <- start_html(output$dur_ui)
    check_pickers(dur, "Duration")
    expect_match(dur, "segment, in the units of the time variable. Segment units are listed", fixed = TRUE)
    # Only the f0 and token variables carry over to the other tabs (they
    # guess time on their own)
    expect_match(start_html(output$ui_f0_screen_hint),
                 paste("See the report above the Data Preview. The other tabs start with these f0 and",
                       "token variables."), fixed = TRUE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "Variables used f0 f0 (Hz) token token time time", fixed = TRUE)
    expect_no_match(rep, "Columns used", fixed = TRUE)
    # The sidebar box and the report card are both titled Data screening; the
    # card's chip says what its share counts
    expect_match(start_html(output$ui_f0_screen), "fa-filter[^>]*></i>\\s*Data screening\\s*</div>")
    rep_html <- start_html(output$f0_screen_report)
    expect_match(rep_html, "<h5>Data screening</h5>", fixed = TRUE)
    expect_no_match(rep_html, "<h5>Rows without f0</h5>", fixed = TRUE)
    expect_no_match(start_html(output$ui_f0_screen), "Rows without f0</div>", fixed = TRUE)
    expect_match(rep, "Data screening 46.7% without f0", fixed = TRUE)
    expect_no_match(rep, "% of rows", fixed = TRUE)
  })
  # Without a token column, a row without f0: the pickers are drawn behind
  # the condition, and the hint names the token variable
  df <- data.frame(index = rep(1:5, 2), time = rep(1:5 / 100, 2),
                   f0_Hz = c(200, 201, NA, 203, 204, 180, 181, 182, 183, 184))
  f <- start_write_csv(df)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    check_pickers(start_html(output$ui_f0_screen), "Data screening, no token")
    check_pickers(start_html(output$dur_ui), "Duration, no token")
    hint <- start_html(output$ui_f0_screen_hint)
    expect_match(hint, "1 of 10 rows has no f0. Choose the token variable to see where it is and to remove it.",
                 fixed = TRUE)
    expect_no_match(hint, "token column", fixed = TRUE)
    rep <- start_text(output$f0_screen_report)
    expect_no_match(rep, "token column", fixed = TRUE)
    expect_no_match(rep, "Columns used", fixed = TRUE)
  })
})

test_that("the report's wording fits the counts: one row, no rows without f0, no rows left", {
  # One row without f0, inside a token
  df <- data.frame(token = "x", time = 0.005 + 0.01 * (0:5), f0 = c(200, 201, NA, 203, 204, 205))
  f <- start_write_csv(df)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "1 of 6 rows (16.7%) has no f0 in column f0:", fixed = TRUE)
    # Removed: the one row, at n = 1
    expect_match(rep, paste("Removed: 1 row without f0. voiced_run numbers the stretches with f0 in each token,",
                            "so plots break the line at gaps."), fixed = TRUE)
    # The R code at n = 1: "the row", not "the 1 row"
    code <- start_code_ok(s, f, start_settings(), dataset())
    expect_true(any(code == "# Remove the row without f0 and add voiced_run"))
    expect_false(any(grepl("the 1 row", code, fixed = TRUE)))
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "1 of 6 rows has no f0 and is removed. See the Start page.")
    expect_match(start_html(output$ui_f0_screen_hint), "1 of 6 rows has no f0.", fixed = TRUE)
    expect_match(rep, "Your raw file is not changed", fixed = TRUE)
    session$setInputs(screen_remove = FALSE)
    code <- s$env$read_data_code(f, start_settings())
    expect_true(any(code == "# The row without f0 stays (removal unticked)"))
    start_code_ok(s, f, start_settings(), dataset())
    # Removal unticked and nothing flagged: the app's copy is the file, so
    # there is no save note
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "Kept: the row without f0 stays. Tick Remove rows without f0 to remove it.", fixed = TRUE)
    expect_no_match(rep, "Your raw file is not changed", fixed = TRUE)
  })

  # Rows without f0 removed before the upload: every row has f0, the time
  # steps show the gaps, and the short runs are already the edge samples
  fx <- start_padded_fixture()
  fx <- fx[is.finite(fx$f0) & fx$f0 > 0, ]
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_identical(screen_plan()$action, "mark")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste0("In 2 tokens (1 at the start and 1 at the end), a gap cuts off a run of 1\u20132 ",
                             "samples, 4 samples in all. These samples hold the first or last f0, so the 0"),
                 fixed = TRUE)
    expect_no_match(rep, "Once the rows without f0 are gone", fixed = TRUE)
  })

  # An f0 column with no f0 at all: not the default when another f0 column
  # has values; picked anyway, the report says no rows are left
  fx <- start_padded_fixture()
  fx$f0 <- NA_real_
  fx$f0_alt <- 150 + seq_len(nrow(fx))
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_identical(start_cols()$f0, "f0_alt")
    session$setInputs(screen_f0 = "f0")
    expect_equal(nrow(dataset()), 0L)
    rep <- start_text(output$f0_screen_report)
    # The lead has just said no row has f0, so the stop line does not
    # repeat it
    expect_match(rep, paste("45 of 45 rows (100.0%) have no f0 in column f0. Removed: no rows left. Choose",
                            "another f0 variable in the sidebar."), fixed = TRUE)
    expect_no_match(rep, "as no row has f0", fixed = TRUE)
    expect_no_match(rep, "voiced_run", fixed = TRUE)
    expect_no_match(rep, "What counts as no f0", fixed = TRUE)
  })

  # The only f0 column has no f0 at all: the toast says so, and nothing else
  fx <- start_padded_fixture()
  fx$f0 <- NA_real_
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_identical(start_cols()$f0, "f0")
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     "No row has f0 in column f0. Choose another f0 variable on the Start page.")
  })

  # 0 Hz in another Hz column: a sentence of its own, before the count
  fx <- start_padded_fixture()
  fx$f0_praat <- 150 + seq_len(nrow(fx))
  fx$f0_praat[c(2, 3, 9)] <- 0
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_identical(start_cols()$f0, "f0")
    expect_identical(start_toast_text(s$rec, "f0a_f0_screen"),
                     paste("Read 0 Hz in f0_praat (3 rows) as missing (NA). 21 of 45 rows have no f0 (2 of",
                           "them 0 Hz) and are removed. 1 token is flagged. See the Start page."))
  })
})

test_that("the report speaks of the time steps only when gaps hang on them and they cannot be read", {
  # A 10 ms grid written to 2 decimals, every row with f0. Three tokens miss
  # one frame; token dup has a repeated time and misses 3 frames.
  mk <- function(tok, frames, step = 0.01) {
    data.frame(token = tok, time = round(0.02 + step * (frames - 1), 2), f0 = 200 + frames,
               stringsAsFactors = FALSE)
  }
  toks <- lapply(sprintf("w%02d", 1:40), function(tk)
    mk(tk, if (tk %in% c("w01", "w02", "w03")) setdiff(1:25, 10) else 1:25))
  dup <- mk("dup", setdiff(1:25, 12:14))
  dup$time[6] <- dup$time[5]
  f <- start_write_csv(do.call(rbind, c(toks, list(dup))))
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    g <- attr(f0_screen()$screen, "grid")
    expect_true(g$regular)
    expect_true(g$units)
    expect_identical(screen_plan()$action, "mark")
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "Pitch-tracker frames, one every 10 ms 3 tokens have gaps in the middle (3 gaps).",
                 fixed = TRUE)
    # Times written to the step itself read fine: nothing about decimals
    expect_no_match(rep, "decimals", fixed = TRUE)
    expect_no_match(rep, "written to the nearest", fixed = TRUE)
    expect_no_match(rep, "This assumes", fixed = TRUE)
    # dup has no rows without f0, so its gaps hang on its time steps
    expect_match(rep, "1 token has two rows with the same time, so gaps inside it can't be found: dup.",
                 fixed = TRUE)
    # Nothing to remove: all good, with the gaps and what voiced_run does
    expect_match(rep, paste("Every row has f0 All good: every row has f0. The time steps show 3 gaps in 3",
                            "tokens. voiced_run numbers the stretches with f0 in each token, so plots break",
                            "the line at gaps."), fixed = TRUE)
    expect_no_match(rep, "Marked:", fixed = TRUE)
    expect_identical(n_hits(start_html(output$f0_screen_report), "st-decision-done"), 1L)
    expect_identical(as.integer(tapply(dataset()$voiced_run, dataset()$token, max)[c("w01", "dup")]),
                     c(2L, 1L))
  })

  # A 12.5 ms grid written to 2 decimals: steps of 0.01 and 0.02 in turn
  # are rounding, so no gaps can be read from the time steps, and with every
  # row having f0 nothing else can show them. v20 ends a frame early, so the
  # tokens differ in length, as frames do.
  toks <- lapply(sprintf("v%02d", 1:20), function(tk) mk(tk, 1:25, step = 0.0125))
  d <- do.call(rbind, toks)
  f <- start_write_csv(d[-nrow(d), ])
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_false(attr(f0_screen()$screen, "grid")$regular)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "Pitch-tracker frames, about one every 12.5 ms", fixed = TRUE)
    expect_match(rep, paste(
      "Gaps inside tokens can't be found: the times are rounded to 0.01 s, but the frames are",
      "about 12.5 ms apart. Save the times with 4 decimals (0.0125, not 0.01) to find them."), fixed = TRUE)
    expect_no_match(rep, "gaps in the middle", fixed = TRUE)
    expect_no_match(rep, "have the same time", fixed = TRUE)
    expect_no_match(rep, "Gaps cannot be read", fixed = TRUE)
    expect_match(rep, "Data screening Every row has f0 All good: every row has f0.", fixed = TRUE)
    expect_no_match(rep, "What counts as no f0", fixed = TRUE)
  })
  # With 25 rows in every token, the frames are counted as points, as the
  # Data screening card counts them, and the same sentence says points
  f <- start_write_csv(d)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "f0 data 25 points per token, about one every 12.5 ms", fixed = TRUE)
    expect_match(rep, paste(
      "Gaps inside tokens can't be found: the times are rounded to 0.01 s, but the points are",
      "about 12.5 ms apart. Save the times with 4 decimals (0.0125, not 0.01) to find them."), fixed = TRUE)
    expect_no_match(rep, "frames", fixed = TRUE)
  })
  # With rows without f0 inside a token, the file keeps its gaps as rows:
  # the same grid is not a problem, and nothing is said about it. (v20
  # ends a frame early again.)
  d$f0[d$token == "v01"][8:10] <- NA
  d <- d[-nrow(d), ]
  f <- start_write_csv(d)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    rep <- start_text(output$f0_screen_report)
    expect_no_match(rep, "can't be found", fixed = TRUE)
    expect_no_match(rep, "regular grid", fixed = TRUE)
    expect_no_match(rep, "rounded", fixed = TRUE)
    expect_match(rep, "1 token has a gap in the middle.", fixed = TRUE)
    expect_false(isTRUE(screen_plan()$points))
    expect_identical(max(dataset()$voiced_run[dataset()$token == "v01"]), 2L)
  })
  # 10 ms frames written to 0.01 with more than 1 step in 100 a gap: gaps
  # and rounding look alike, and the fix is the same. g40 ends a frame
  # early, so the tokens differ in length, as frames do.
  d10 <- do.call(rbind, lapply(sprintf("g%02d", 1:40), function(tk)
    mk(tk, setdiff(if (tk == "g40") 1:24 else 1:25, c(9, 17)))))
  f <- start_write_csv(d10)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "Pitch-tracker frames, one every 10 ms", fixed = TRUE)
    expect_match(rep, paste(
      "Gaps inside tokens can't be found: the times are rounded to 0.01 s, the same as the step",
      "between frames (10 ms), so a longer step could be a gap or rounding. Save the times with",
      "3 decimals to find them."), fixed = TRUE)
  })
  # Nor when a voiced_run column (an earlier removal) already marks the gaps
  d <- do.call(rbind, toks)
  d$voiced_run <- 1L
  f <- start_write_csv(d)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_no_match(start_text(output$f0_screen_report), "can't be found", fixed = TRUE)
  })

  # With rows without f0: a gap from rows (r01), a gap only the time steps
  # show (r02), a repeated time (dup) and a token at 12.5 ms (slow). Only
  # the tokens with no rows without f0 inside get a sentence.
  toks <- lapply(sprintf("w%03d", 1:100), function(tk) mk(tk, 1:25))
  r01 <- mk("r01", 1:25)
  r01$f0[c(1, 2, 12)] <- NA
  r02 <- mk("r02", setdiff(1:25, c(8, 16)))
  dup <- mk("dup", 1:25)
  dup$time[6] <- dup$time[5]
  slow <- mk("slow", 1:40, step = 0.0125)
  # dup2 has a repeated time too, but a row without f0 inside shows its gap
  dup2 <- mk("dup2", 1:25)
  dup2$time[6] <- dup2$time[5]
  dup2$f0[15] <- NA
  f <- start_write_csv(do.call(rbind, c(toks, list(r01, r02, dup, slow, dup2))))
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    g <- attr(f0_screen()$screen, "grid")
    expect_true(g$regular)
    expect_true(g$units)
    expect_identical(sort(g$repeated_tokens), c("dup", "dup2"))
    expect_identical(g$other_step_tokens, "slow")
    expect_true(screen_plan()$remove)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("Removed: 4 rows without f0. voiced_run numbers the stretches with f0 in each",
                            "token, so plots break the line at gaps."), fixed = TRUE)
    # r01 (1 gap from rows), r02 (2 from the time steps) and dup2 (1 from rows)
    expect_match(rep, "3 tokens have gaps in the middle (4 gaps).", fixed = TRUE)
    expect_match(rep, paste("The time steps show 2 more gaps in 1 token, where the file has no rows.",
                            "voiced_run marks them too."), fixed = TRUE)
    expect_match(rep, "1 token has two rows with the same time, so gaps inside it can't be found: dup.",
                 fixed = TRUE)
    expect_match(rep, paste(
      "In 1 token, the frames are not 10 ms apart (about 12.5 ms), and the times are rounded to",
      "0.01 s, so gaps inside it can't be found: slow. Save the times with 4 decimals to find them."),
      fixed = TRUE)
    expect_no_match(rep, "dup2", fixed = TRUE)
    expect_no_match(rep, "This assumes", fixed = TRUE)
    mx <- tapply(dataset()$voiced_run, dataset()$token, max)
    expect_identical(as.integer(mx[c("r01", "r02", "dup", "slow", "w001", "dup2")]),
                     c(2L, 3L, 1L, 1L, 1L, 2L))
  })
})

test_that("the column guesses never fall back to voiced_run", {
  h <- start_server()$env
  vars <- c("token", "time", "f0", "intensity", "voiced_run")
  expect_identical(h$guess_var(vars, h$var_patterns$tone, 5), "token")
  expect_identical(h$role_var(data.frame(), "tone", vars, h$var_patterns$tone, 5), "token")
  expect_identical(h$guess_var(c("a", "voiced_run", "b"), "^zz", 2), "b")
  expect_identical(h$guess_var("voiced_run", "^zz", 1), "voiced_run")
})

test_that("read_data_code() gives a comment, not an error, when the screen failed", {
  h <- start_server()$env
  st <- list(version = NA, screen = list(token = NULL, time = "time", f0 = "f0", action = "none",
                                         without = NA_integer_, rows = 10L, error = "boom"))
  code <- h$read_data_code("x.csv", st)
  expect_true(any(grepl("# Start page: the rows without f0 in column f0 could not be screened (boom)",
                        code, fixed = TRUE)))
  st$screen$error <- NULL
  expect_no_error(h$read_data_code("x.csv", st))
})

test_that("the View table drops a column that went between two draws of its menu", {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  skip_if_not_installed("DT")
  pkgs <- new.env(parent = globalenv())
  for (p in c("shiny", "dplyr"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  sys.source(file.path(app, "ui", "view_ui.R"), env, keep.source = FALSE)
  ds <- shiny::reactiveVal(data.frame(token = "a", time = 1:3, f0 = c(200, 201, 202),
                                      voiced_run = 1L))
  bound <- function(input, output, session) NULL
  body(bound) <- body(env$view_ui)
  environment(bound) <- list2env(list(dataset = ds), parent = env)
  shiny::testServer(bound, {
    session$setInputs(view_vars = c("token", "f0", "voiced_run"), view_dec = 2)
    expect_no_error(output$dataviewer)
    # Unticking a Start-page box drops voiced_run before the menu is drawn again
    ds(data.frame(token = "a", time = 1:3, f0 = c(200, 201, 202)))
    session$flushReact()
    expect_no_error(output$dataviewer)
    expect_false(grepl("voiced_run", output$dataviewer, fixed = TRUE))
  })
})

# ---- The "Your data" card ----------------------------------------------------

test_that("Your data: frames, with the middle gaps counted the same raw and pre-cleaned", {
  fx <- start_padded_fixture()
  texts <- list()
  for (nm in c("raw", "cleaned")) {
    d <- if (nm == "raw") fx else fx[is.finite(fx$f0) & fx$f0 > 0, ]
    f <- start_write_csv(d)
    s <- start_server()
    shiny::testServer(s$server, {
      session$setInputs(uploadfile = start_upload(f, paste0(nm, ".csv")))
      texts[[nm]] <<- start_text(output$f0_screen_report)
    })
  }
  # Rows without f0 at a token's start or end are not gaps; S2_b has no f0
  expect_match(texts$raw, "5 tokens 2 speakers 1 tone T1 45 rows 21 without f0 see below", fixed = TRUE)
  expect_match(texts$cleaned, "4 tokens 2 speakers 1 tone T1 24 rows every row has f0", fixed = TRUE)
  for (x in texts) {
    expect_match(x, "Pitch-tracker frames, one every 10 ms 3 tokens have gaps in the middle (3 gaps).",
                 fixed = TRUE)
    expect_match(x, "Variables used f0 f0 (Hz) token token time time speaker speaker tone tone", fixed = TRUE)
    expect_no_match(x, "can't be found", fixed = TRUE)
  }
})

test_that("Your data on f0_long.csv: 109 gaps in 105 tokens, raw and with the rows without f0 removed", {
  path <- normalizePath(test_path("..", "..", "test", "f0_long.csv"), mustWork = FALSE)
  skip_if_not(file.exists(path))
  fl <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  f2 <- tempfile(fileext = ".csv")
  utils::write.csv(fl[is.finite(fl$f0) & fl$f0 > 0, ], f2, row.names = FALSE, fileEncoding = "UTF-8")
  for (f in c(path, f2)) {
    s <- start_server()
    shiny::testServer(s$server, {
      session$setInputs(uploadfile = start_upload(f))
      rep <- start_text(output$f0_screen_report)
      expect_match(rep, paste("1,853 tokens", if (f == path) "83,301 rows 30,870 without f0"
                              else "52,431 rows every row has f0"), fixed = TRUE)
      expect_match(rep, "Pitch-tracker frames, one every 10 ms 105 tokens have gaps in the middle (109 gaps).",
                   fixed = TRUE)
      # No speaker or tone column: no line for them
      expect_no_match(rep, "speaker", fixed = TRUE)
      expect_no_match(rep, " tones", fixed = TRUE)
      expect_no_match(rep, "can't be found", fixed = TRUE)
    })
  }
})

test_that("Your data: equidistant points, and the tokens with fewer or more", {
  # 30 tokens of 11 points evenly spaced over their own duration
  mk <- function(tok, dur, pts = 1:11) {
    data.frame(token = tok, time = 0.1 + dur * (pts - 1) / 10, f0 = 180 + pts,
               speaker = "s1", stringsAsFactors = FALSE)
  }
  toks <- sprintf("p%02d", 1:30)
  d <- do.call(rbind, lapply(seq_along(toks), function(i) {
    pts <- switch(toks[i], p03 = setdiff(1:11, 6), p05 = setdiff(1:11, 4), p07 = 2:11, 1:11)
    mk(toks[i], 0.15 + 0.01 * i, pts)
  }))
  d$token[d$token == "p10"] <- "p09"   # two recordings share a name: 22 points
  f <- start_write_csv(d)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste("11 equidistant points per token",
                            "3 tokens have fewer (10 points: p03, p05 and p07).",
                            "1 token has more (22 points: p09; two recordings may share this token name)."),
                 fixed = TRUE)
    expect_match(rep, "29 tokens 1 speaker 327 rows every row has f0", fixed = TRUE)
    # No middle-gap count for equidistant data, and no time-step warning:
    # the count of points shows a missing point
    expect_no_match(rep, "gaps in the middle", fixed = TRUE)
    expect_no_match(rep, "can't be found", fixed = TRUE)
    expect_no_match(rep, "Pitch-tracker", fixed = TRUE)
  })
  # Counted on the uploaded rows: a token whose first point has no f0
  # still has 11 points, though removal (ticked) leaves it 10, and the
  # drop leaves the two flagged tokens out
  d2 <- do.call(rbind, lapply(seq_along(toks), function(i) mk(toks[i], 0.15 + 0.01 * i)))
  d2$f0[d2$token == "p04"][1] <- NA
  d2$f0[d2$token == "p08"][5] <- NA
  f <- start_write_csv(d2)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_true(screen_plan()$remove)
    expect_identical(sum(dataset()$token == "p04"), 10L)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "11 equidistant points per token Variables used", fixed = TRUE)
    expect_no_match(rep, "have fewer (", fixed = TRUE)
    expect_match(rep, "2 tokens flagged: fewer than 11 points with f0.", fixed = TRUE)
    session$setInputs(screen_drop_flagged = TRUE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "11 equidistant points per token Variables used", fixed = TRUE)
    expect_match(rep, "All good: every row left has f0. Dropped: 2 flagged tokens (22 rows)", fixed = TRUE)
  })
  # Many tokens with fewer: the lists are capped
  d3 <- do.call(rbind, lapply(sprintf("q%02d", 1:40), function(tk) {
    n <- if (tk == "q01") 10 else if (tk %in% sprintf("q%02d", 2:8)) 9 else 11
    mk(tk, 0.2 + 0.001 * as.integer(substr(tk, 2, 3)), seq_len(n))
  }))
  f <- start_write_csv(d3)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_match(start_text(output$f0_screen_report),
                 "8 tokens have fewer (10 points: q01; 9 points: q02, q03, q04, q05, q06 and 2 more).",
                 fixed = TRUE)
  })
  # A time_prop column marks equidistant points whatever the counts
  rs <- resample_f0_equal(start_padded_fixture()[!is.na(start_padded_fixture()$f0), ], n = 7)
  f <- start_write_csv(rs)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_match(start_text(output$f0_screen_report), "7 equidistant points per token", fixed = TRUE)
  })
})

test_that("Your data: uneven points, and their warning only when no row lacks f0 inside a token", {
  set.seed(7)
  mk <- function(tok, n) {
    data.frame(token = tok, time = cumsum(stats::runif(n, 0.004, 0.025)), f0 = 150 + seq_len(n),
               stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, lapply(sprintf("u%02d", 1:30), function(tk) mk(tk, 20 + as.integer(substr(tk, 2, 3)))))
  st <- unlist(lapply(split(d$time, d$token), diff))
  q <- vapply(signif(1000 * stats::quantile(st, c(0.05, 0.95), names = FALSE), 2), format, "")
  f <- start_write_csv(d)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, sprintf("Uneven points, mostly %s to %s ms apart", q[1], q[2]), fixed = TRUE)
    expect_match(rep, "The points are not evenly spaced, so gaps inside tokens can't be found from the times.",
                 fixed = TRUE)
  })
  d$f0[d$token == "u03"][5] <- NA
  f <- start_write_csv(d)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "Uneven points", fixed = TRUE)
    expect_no_match(rep, "can't be found", fixed = TRUE)
  })
})

test_that("Your data: no token column, many tones, and columns from attached metadata", {
  df <- data.frame(index = rep(1:5, 2), time = rep(1:5 / 100, 2),
                   f0_Hz = c(200, 201, NA, 203, 204, 180, 181, 182, 183, 184),
                   tone = rep(c(3, 1), each = 5))
  f <- start_write_csv(df)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    rep <- start_text(output$f0_screen_report)
    # A row lacks f0, so the sidebar shows the token picker
    expect_true(output$screen_need_token)
    expect_match(rep, "No token variable Tokens not counted see below 2 tones 1, 3 10 rows 1 without f0",
                 fixed = TRUE)
    expect_match(rep, "Sampling not checked: no token variable.", fixed = TRUE)
    expect_identical(n_hits(tolower(rep), "choose the token variable"), 1L)
    expect_match(rep, "token none", fixed = TRUE)
  })
  # More than 10 tones: the count only
  df <- data.frame(token = rep(sprintf("t%02d", 1:12), each = 4), time = rep(1:4 / 100, 12),
                   f0 = 200, tone = rep(sprintf("T%d", 1:12), each = 4))
  f <- start_write_csv(df)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "12 tokens 12 tones 48 rows", fixed = TRUE)
    expect_no_match(rep, "T1, T2", fixed = TRUE)
  })
  # Speaker and tone from the file name, attached on the Start page
  df <- data.frame(token = rep(c("S1_T1_a", "S1_T2_a", "S2_T1_a"), each = 5),
                   time = rep(1:5 / 100, 3), f0 = 200 + 1:15)
  f <- start_write_csv(df)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_no_match(start_text(output$f0_screen_report), "speakers", fixed = TRUE)
    session$setInputs(meta_source = "derive", meta_key = "token", meta_sep = "_",
                      meta_colnames = "speaker tone rep")
    session$setInputs(meta_apply = 1)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "3 tokens 2 speakers 2 tones T1, T2 15 rows", fixed = TRUE)
    expect_match(rep, "speaker speaker tone tone", fixed = TRUE)
  })
})

test_that("removing the short runs is said to move the time axes, in the report and the sidebar", {
  f <- start_write_csv(start_padded_fixture())
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_match(start_text(output$ui_f0_screen),
                 "These samples have f0, so removing them moves their tokens' 0\u20131 time axes.",
                 fixed = TRUE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "To save that copy, download it in Normalise or Curate.", fixed = TRUE)
    expect_no_match(rep, "The time axes run", fixed = TRUE)
    session$setInputs(screen_short = TRUE)
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, paste0("These samples hold the first or last f0. They are removed too, so these tokens' 0",
                             EN, "1 time axes change."), fixed = TRUE)
    expect_identical(n_hits(rep, "time axes change"), 1L)
    expect_no_match(rep, "notes above say", fixed = TRUE)
    expect_no_match(rep, "Removing the short runs", fixed = TRUE)
  })
})

test_that("the data-report helpers: steps in ms, decimals, role columns and the kind of f0 data", {
  h <- start_server()$env
  nb <- intToUtf8(160)
  expect_identical(h$start_step_text(0.0125, "s"), paste0("12.5", nb, "ms"))
  expect_identical(h$start_step_text(10, "ms"), paste0("10", nb, "ms"))
  expect_identical(vapply(c(0.0125, 0.01, 0.025, 0.004, 0.0075, 2.5), h$start_decimals, 1L),
                   c(4L, 3L, 3L, 4L, 4L, 1L))
  expect_identical(h$start_role_col(c("token", "time", "Speaker_ID", "tone_cat"), "speaker"), "Speaker_ID")
  expect_null(h$start_role_col(c("token", "time", "f0"), "speaker"))   # no fallback column
  expect_null(h$start_role_col(c("voiced_run"), "tone"))
  fr <- data.frame(token = rep(c("a", "b"), c(30, 20)), time = c(1:30, 1:20) / 100)
  expect_identical(h$start_f0_kind(fr, "token", "time")$kind, "frames")
  expect_identical(h$start_f0_kind(fr, "token", "time")$unit, "s")
  ms <- fr
  ms$time <- ms$time * 1000
  expect_identical(h$start_f0_kind(ms, "token", "time")$unit, "ms")
  pt <- data.frame(token = rep(c("a", "b", "c"), each = 5),
                   time = c(seq(0, 0.2, length.out = 5), seq(0, 0.31, length.out = 5),
                            seq(0, 0.44, length.out = 5)))
  k <- h$start_f0_kind(pt, "token", "time")
  expect_identical(k$kind, "points")
  expect_identical(k$n_points, 5L)
  expect_length(k$fewer, 0)
})

test_that("the Start page's report sources are ASCII", {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  for (fl in c(file.path("ui", "start_ui.R"), file.path("www", "styles.css"))) {
    x <- readLines(file.path(app, fl), warn = FALSE, encoding = "UTF-8")
    expect_false(any(grepl("[^\t -~]", x, useBytes = TRUE)), info = fl)
  }
  g <- readLines(file.path(app, "global.R"), warn = FALSE, encoding = "UTF-8")
  a <- grep("^# Start page data report", g)
  b <- grep("end of the Start page data report", g)
  expect_length(a, 1)
  expect_false(any(grepl("[^\t -~]", g[a:b], useBytes = TRUE)))
})

test_that("counts on the Start page, its preview and its toasts have thousands separators", {
  # The sample's toast and the preview's footer
  sample <- system.file("app", "www", "dc21f0_test.csv", package = "shinytone")
  skip_if(!nzchar(sample), "sample not found")
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(try_sample = 1)
    session$setInputs(uploadfile = start_upload(sample))
    expect_match(start_toast_text(s$rec, "f0a_sample_loaded"),
                 "Sample data loaded: 38,808 rows, 1,848 tokens, 13 speakers.", fixed = TRUE)
    expect_identical(output$row_count_info, "10 of 38,808 rows shown. See View-tab for details.")
  })
  # Discarded tokens, a duration and derived metadata, over 1,100 tokens
  fx <- data.frame(token = rep(sprintf("w_%04d", 1:1100), each = 3),
                   time = rep(c(0.01, 0.02, 0.03), 1100), f0 = 200)
  fx$token_dropped <- fx$token %in% sprintf("w_%04d", 1:1050)
  f <- start_write_csv(fx)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "many.csv"))
    expect_match(start_toast_text(s$rec, "f0a_dropped_tokens"),
                 "This file marks 1,050 of 1,100 tokens as discarded", fixed = TRUE)
    ui <- start_html(output$ui_dropped_tokens)
    expect_match(ui, "Exclude 1,050 discarded tokens (recommended)", fixed = TRUE)
    expect_match(ui, "1,050 of 1,100 tokens in this file carry", fixed = TRUE)
    session$setInputs(exclude_dropped = FALSE)
    expect_identical(output$row_count_info, "10 of 3,300 rows shown. See View-tab for details.")
    session$setInputs(dur_unit = "__token__", dur_token = "token", dur_time = "time", dur_f0 = "f0")
    session$setInputs(dur_apply = 1)
    added <- Filter(function(t) grepl("Added token_f0_dur", t$text, fixed = TRUE), s$rec$toasts)
    expect_length(added, 1)
    expect_match(added[[1]]$text, "Added token_f0_dur for 1,100 token(s)", fixed = TRUE)
    session$setInputs(meta_source = "derive", meta_key = "token", meta_sep = "_")
    expect_match(start_html(output$meta_status), "2 segment(s) in all 1,100 unique value(s).", fixed = TRUE)
    expect_match(start_html(output$meta_preview), "Preview (first 3 of 1,100 rows):", fixed = TRUE)
  })
})

test_that("the Start intro says what upload does with rows without f0, in a few sentences", {
  s <- start_server()
  shiny::testServer(s$server, {
    txt <- start_text(output$instruction_text)
    expect_match(txt, paste("Pitch trackers often write a row for every frame, with no f0 (NA or 0 Hz)",
                            "where there is no voicing. Data screening, in the sidebar, checks for these",
                            "rows without f0 and removes any it finds by default."), fixed = TRUE)
    expect_no_match(txt, "The other tabs start on", fixed = TRUE)
    # The other tabs guess time on their own, so the intro does not say they
    # start with the Start page's time variable
    expect_no_match(txt, "token and time variables that", fixed = TRUE)
    # The F0 Correction note is a folded guide box after the welcome, and it
    # and the column list have no stray space before punctuation (htmltools
    # puts a newline between tag children)
    html <- start_html(output$instruction_text)
    expect_match(html, paste0('<details class="guide-box">\\s*<summary class="guide-summary" ',
                              'style="font-weight: 700;">Continuing from the F0 Correction tab\\?</summary>'))
    expect_no_match(html, '<details class="guide-box" open', fixed = TRUE)
    expect_match(txt, paste("Continuing from the F0 Correction tab? Upload its all_correctedf0.csv here",
                            "directly. Tokens you discarded there (token_dropped = TRUE) are found on",
                            "upload and left out of the working data by default, and Data screening uses",
                            "f0_corrected."), fixed = TRUE)
    expect_no_match(txt, "You can change both in the sidebar", fixed = TRUE)
    expect_match(txt, "time: time index within each token", fixed = TRUE)
    expect_no_match(txt, "Sidebar checkboxes", fixed = TRUE)
    # The page's order: welcome, the F0 Correction box, what each tab does,
    # then what the data look like (with the sample and its button)
    at <- function(x) regexpr(x, txt, fixed = TRUE)[[1]]
    pos <- c(at("Welcome!"), at("Continuing from the F0 Correction tab?"), at("What each tab does"),
             at("What the data look like"), at("Pitch trackers often write"),
             at("Here's what our sample data looks like"))
    expect_true(all(pos > 0))
    expect_false(is.unsorted(pos))
    # Every F0 Analysis tab is listed, in the tab bar's order
    tabs <- c("Start Upload", "View Browse", "Normalise Apply", "Visualise Plot", "Inspect Flag",
              "Cluster Group", "Curate Relabel", "Model: Polynomials Fit", "Model: GCA Run",
              "Model: GAMM Fit", "Summarise Convert")
    tpos <- vapply(tabs, at, 1L)
    expect_true(all(tpos > 0))
    expect_false(is.unsorted(tpos))
    expect_match(txt, "Upload your CSV data, screen it for rows without f0 and preview its structure.",
                 fixed = TRUE)
    # Each tab has one Font Awesome icon on a pale circle coloured by stage, in the
    # tab bar's order, and no emoji
    ico <- regmatches(html, gregexpr('<span class="st-tab-ico">\\s*<i class="[^"]*fa-([a-z-]+)', html))[[1]]
    expect_identical(sub('.*fa-([a-z-]+)$', "\\1", ico),
                     c("folder-open", "table", "sliders", "chart-line", "magnifying-glass", "shapes", "tags",
                       "superscript", "arrow-trend-up", "bezier-curve", "list-ol"))
    expect_no_match(html, "&#1(27|28|29)[0-9]{3};")
    # Each row is coloured by its stage of the work, with no legend
    st <- regmatches(html, gregexpr('<li class="st-tab-([a-z]+)">', html))[[1]]
    expect_identical(sub('<li class="st-tab-([a-z]+)">', "\\1", st),
                     c("data", "data", "prep", "prep", "prep", "expl", "expl", "model", "model", "model", "sum"))
    expect_match(html, ".st-tab-model .st-tab-ico{background:#fde6e7;color:#9e3c43;}", fixed = TRUE)
    expect_no_match(html, "st-legend", fixed = TRUE)
    expect_no_match(txt, "  ", fixed = TRUE)
    # voiced_run is explained in the Data screening card, not here
    expect_no_match(txt, "voiced_run", fixed = TRUE)
    expect_no_match(txt, "keeps the gaps marked", fixed = TRUE)
    expect_no_match(txt, "become the defaults", fixed = TRUE)
    # (The exact match above also rules out a stray space before the full stops)
    # The time axes and the equal-points exception belong in the report, not here
    for (w in c("token_t01", "<tier>_t01", "time_prop", "rescale", "regular grid",
                "same number of points")) {
      expect_no_match(txt, w, fixed = TRUE)
    }
  })
})

test_that("the Data screening box shows its heading and a note before any upload", {
  s <- start_server()
  shiny::testServer(s$server, {
    html <- start_html(output$ui_f0_screen)
    expect_match(html, "fa-filter[^>]*></i>\\s*Data screening\\s*</div>")
    expect_match(start_text(output$ui_f0_screen),
                 "Upload a file to choose the f0, token and time variables and check for rows without f0.",
                 fixed = TRUE)
    expect_no_match(html, "<select", fixed = TRUE)
  })
  # After an upload: the same heading, with the pickers and no note
  f <- start_write_csv(start_padded_fixture())
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    html <- start_html(output$ui_f0_screen)
    expect_match(html, "fa-filter[^>]*></i>\\s*Data screening\\s*</div>")
    expect_match(html, "<select", fixed = TRUE)
    expect_no_match(html, "Upload a file to choose", fixed = TRUE)
  })
})

test_that("the metadata pickers say variable, like the other pickers", {
  f <- start_write_csv(start_padded_fixture())
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f), meta_source = "derive")
    html <- start_html(output$meta_key_ui)
    expect_match(html, ">Split this variable:</label>", fixed = TRUE)
    expect_no_match(html, "Split this column", fixed = TRUE)
    session$setInputs(meta_source = "csv")
    html <- start_html(output$meta_key_ui)
    expect_match(html, ">Filename variable in your data:</label>", fixed = TRUE)
    expect_no_match(html, "Filename column in your data", fixed = TRUE)
  })
})

test_that("a new f0 variable does not name the 0 / -1 box in the cleared-results toast", {
  # f0 reads 0 and -1 as no f0; f0b has NA in the same rows
  z <- start_user_fixture()
  i0 <- which(is.na(z$f0))
  z$f0b <- z$f0
  z$f0[i0[1:6]] <- 0
  z$f0[i0[7:12]] <- -1
  f <- start_write_csv(z)
  s <- start_server()
  cleared <- function(what) paste("On the Start page,", what, "These results came from the old rows,",
                                  "so they were cleared: normalised data.")
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_identical(start_cols()$f0, "f0")
    expect_true(length(f0_screen()$unvoiced) > 0)
    v0 <- data_version()
    normalised_data(data.frame(x = 1))
    session$setInputs(screen_f0 = "f0b")
    expect_identical(data_version(), v0 + 1L)
    expect_identical(start_toast_text(s$rec, "start_data_cleared"),
                     cleared("the f0 variable is now f0b."))
    normalised_data(data.frame(x = 1))
    session$setInputs(screen_f0 = "f0")
    expect_identical(data_version(), v0 + 2L)
    expect_identical(start_toast_text(s$rec, "start_data_cleared"),
                     cleared("the f0 variable is now f0."))
    # Unticking the box itself still names it
    normalised_data(data.frame(x = 1))
    session$setInputs(screen_unvoiced = FALSE)
    expect_identical(data_version(), v0 + 3L)
    expect_identical(start_toast_text(s$rec, "start_data_cleared"),
                     cleared("\"Read 0 / -1 as no f0\" was unticked."))
  })
})

test_that("frames with the same number in every token are points in both cards", {
  mk <- start_frame_tok
  d <- rbind(mk("A1", c(NA, 180, 181, 182, NA, 184, 185, 186, NA)),
             mk("A2", c(170, NA, NA, 173, 174, 175, 176, 177, 178)),
             mk("A3", c(160, 161, 162, 163, 164, 165, 166, 167, 168)),
             mk("A4", c(NA, NA, 150, 151, NA, NA, NA, NA, NA)))
  f <- start_write_csv(d)
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_true(isTRUE(screen_plan()$points))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "f0 data 9 points per token, one every 10 ms 2 tokens have gaps in the middle (2 gaps).",
                 fixed = TRUE)
    expect_no_match(rep, "Pitch-tracker frames", fixed = TRUE)
    expect_match(rep, "3 tokens flagged: fewer than 9 points with f0.", fixed = TRUE)
    expect_match(start_html(output$f0_screen_report), "fa-ellipsis", fixed = TRUE)
  })
  # One frame fewer in A3: frames again
  f <- start_write_csv(d[-27, ])
  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f))
    expect_false(isTRUE(screen_plan()$points))
    rep <- start_text(output$f0_screen_report)
    expect_match(rep, "f0 data Pitch-tracker frames, one every 10 ms", fixed = TRUE)
    expect_no_match(rep, "points per token", fixed = TRUE)
  })
})
