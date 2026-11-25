# F0 Extraction (inst/app/ui/fp_extraction_ui.R), "Drop rows without f0":
# the export runs drop_rows_without_f0(), so the file carries voiced_run; a
# resumed file's old voiced_run is not carried into the new export; the help
# says what the column is for. The file it writes is seen as cleaned on the
# Start page, with voiced_run kept. Equidistant points and a TextGrid
# interval region mark only the gaps the rows without f0 leave, not gaps
# read from the time steps. The role pickers of F0 Extraction and F0
# Correction say "variable", as in F0 Analysis.

# The module, with the global.R helpers it calls and shiny's exports, and a
# showNotification() that records what it is given (as in
# test-app-landmarks.R).
fpx_server <- function(audio, f0) {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  sh <- new.env(parent = globalenv())
  for (nm in getNamespaceExports("shiny")) assign(nm, getExportedValue("shiny", nm), envir = sh)
  helpers <- new.env(parent = sh)
  keep <- c("nows", "min_audio_dur", "landmark_sets", "guide_box")
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                   as.character(e[[2]]) %in% keep,
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, helpers)
  sys.source(file.path(app, "ui", "fp_extraction_ui.R"), helpers, keep.source = FALSE)
  shown <- new.env()
  shown$log <- list()
  args <- list2env(list(
    fp_audio_data = shiny::reactiveVal(audio), fp_f0_data = shiny::reactiveVal(f0),
    fp_pitch_candidates = shiny::reactiveVal(NULL), fp_metadata = shiny::reactiveVal(NULL),
    fp_corrected_data = shiny::reactiveVal(NULL),
    showNotification = function(ui, ..., id = NULL, type = "default", duration = 5) {
      shown$log[[length(shown$log) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                  id = id, type = type)
      invisible(id)
    }), parent = helpers)
  bound <- function(input, output, session) NULL
  body(bound) <- body(helpers$fp_extraction_ui)
  environment(bound) <- args
  list(server = bound, shown = shown, args = args)
}

fpx_audio <- function(toks, tg_path = NA_character_) {
  data.frame(basename = toks, wav_path = paste0(toks, ".wav"), tg_path = tg_path,
             pitch_path = NA_character_, pitchtier_path = NA_character_,
             stringsAsFactors = FALSE)
}

fpx_frames <- function(tok, f0) {
  data.frame(token = tok, time = round(0.005 + 0.01 * (seq_along(f0) - 1), 4),
             f0 = f0, intensity = 60 + seq_along(f0), stringsAsFactors = FALSE)
}

# Extraction-like frames: 10 ms, rows without f0 at both edges, and gaps
# inside tokens. tokB's tracker wrote 0 and -1 for two frames, which F0
# Extraction reads as no f0 (NA or <= 0 Hz). tokD starts with a run of 2
# samples cut off by a gap: the voiced region keeps it (trim_to_voiced()
# needs runs of 2), and so does the drop (all samples with f0 are kept).
fpx_fixture <- function() {
  rbind(
    fpx_frames("tokA", c(NA, NA, 200, 202, 204, 206, NA, NA, 210, 212, 214, 216, NA, 218, 220, 222, NA, NA)),
    fpx_frames("tokB", c(NA, 180, 181, 182, 183, 0, 185, 186, 187, -1, 189, 190, 191, NA)),
    fpx_frames("tokC", c(NA, 150, 151, 152, 153, 154, 155, NA)),
    fpx_frames("tokD", c(NA, 170, 171, NA, 173, 174, 175, 176, NA)))
}

# The export with the box off and on, for the inputs given.
fpx_exports <- function(session, fp_export_data, ...) {
  session$setInputs(..., fp_drop_unvoiced = FALSE)
  keep <- fp_export_data()
  session$setInputs(fp_drop_unvoiced = TRUE)
  list(keep = keep, drop = fp_export_data())
}

fpx_summary <- function(output) {
  gsub("\\s+", " ", paste(as.character(output$fp_export_summary$html), collapse = " "))
}

# The overview plot colours each token, and with more than 8 tokens
# RColorBrewer warns that Set2 has 8 colours. Only that warning is muffled.
fpx_quiet_palette <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("palette Set2|that many colors", conditionMessage(w))) invokeRestart("muffleWarning")
  })
}

# drop_rows_without_f0() on an export with the box off, with F0 Extraction's
# rule: f0 that is NA or not above 0 Hz is no f0. `gap_ratio = Inf` for
# equidistant points and interval regions, which mark only removed rows.
fpx_reference <- function(d, gap_ratio = 1.5) {
  d$f0[is.na(d$f0) | d$f0 <= 0] <- NA
  out <- drop_rows_without_f0(d, token = "token", time = "time", f0 = "f0", gap_ratio = gap_ratio)
  attr(out, "rows_without_f0") <- NULL
  out
}

# A TextGrid with one interval tier `phone`.
fpx_textgrid <- function(path, iv, xmax = 0.6) {
  lines <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', '',
             sprintf('xmin = 0'), sprintf('xmax = %s', xmax), 'tiers? <exists>', 'size = 1',
             'item []:', '    item [1]:', '        class = "IntervalTier"',
             '        name = "phone"', '        xmin = 0', sprintf('        xmax = %s', xmax),
             sprintf('        intervals: size = %d', nrow(iv)))
  for (i in seq_len(nrow(iv))) {
    lines <- c(lines, sprintf('        intervals [%d]:', i),
               sprintf('            xmin = %s', iv$a[i]), sprintf('            xmax = %s', iv$b[i]),
               sprintf('            text = "%s"', iv$lab[i]))
  }
  writeLines(lines, path)
}

test_that("the drop step removes rows without f0 and numbers the runs in voiced_run", {
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  f0  <- fpx_fixture()
  srv <- fpx_server(fpx_audio(unique(f0$token)), f0)
  shiny::testServer(srv$server, {
    ex <- fpx_exports(session, fp_export_data, fp_region = "voiced", fp_sampling_mode = "native")
    keep <- ex$keep$data
    drop <- ex$drop$data
    expect_false("voiced_run" %in% names(keep))
    # The voiced region keeps 6 rows without f0 inside tokens: 3 in tokA,
    # tokB's 0 and -1, and 1 in tokD. The 2 samples that start tokD stay.
    expect_equal(sum(is.na(keep$f0) | keep$f0 <= 0), 6L)
    expect_equal(sum(keep$token == "tokD"), 7L)
    expect_equal(ex$drop$n_unvoiced, 6L)
    expect_equal(ex$drop$run_gaps, 5L)
    expect_equal(nrow(drop), nrow(keep) - 6L)
    expect_true(all(is.finite(drop$f0) & drop$f0 > 0))
    expect_false(any(drop$f0 %in% c(0, -1)))
    expect_type(drop$voiced_run, "integer")
    expect_identical(as.vector(tapply(drop$voiced_run, drop$token, max)[c("tokA", "tokB", "tokC", "tokD")]),
                     c(3L, 3L, 1L, 2L))
    expect_identical(drop$voiced_run[drop$token == "tokA"], rep(1:3, c(4, 4, 3)))
    expect_identical(drop$voiced_run[drop$token == "tokB"], rep(1:3, c(4, 3, 3)))
    # The short run at tokD's start is kept and numbered 1
    expect_identical(drop$f0[drop$token == "tokD"], c(170, 171, 173, 174, 175, 176))
    expect_identical(drop$voiced_run[drop$token == "tokD"], rep(1:2, c(2, 4)))
    # Rows with f0 are as they were, in their order, with the token-level flags
    expect_identical(drop[setdiff(names(drop), "voiced_run")],
                     {k <- keep[is.finite(keep$f0) & keep$f0 > 0, ]; rownames(k) <- NULL; k})
    expect_true(all(drop$has_gap[drop$token %in% c("tokA", "tokB", "tokD")]))
    expect_false(any(drop$has_gap[drop$token == "tokC"]))
    # The same as drop_rows_without_f0() on the export with the box off
    expect_identical(drop, fpx_reference(keep))
    # drop_rows_without_f0() reads only NA as no f0 in this column: tokB's 0
    # and -1 go only when given as unvoiced, as F0 Extraction reads them
    plain <- drop_rows_without_f0(keep)
    expect_equal(nrow(plain), nrow(drop) + 2L)
    via_unvoiced <- drop_rows_without_f0(keep, unvoiced = c(0, -1))
    attr(via_unvoiced, "rows_without_f0") <- NULL
    expect_identical(via_unvoiced, drop)

    # The summary under the Download button says what happened
    cap <- fpx_summary(output)
    expect_match(cap, "Dropped 6 row(s) without f0.", fixed = TRUE)
    expect_match(cap, "The voiced_run column marks 5 gap(s) inside tokens.", fixed = TRUE)

    # The helper on its own: an old voiced_run, which breaks at every row,
    # is replaced by a fresh one, quietly
    old <- keep
    old$voiced_run <- seq_len(nrow(old))
    expect_no_message(res <- export_drop_rows_without_f0(old))
    expect_identical(res$data, drop)
    expect_identical(c(res$n, res$gaps), c(6L, 5L))
    # Nothing to drop, no gap: no column is added
    clean <- keep[keep$token == "tokC" & is.finite(keep$f0), ]
    res <- export_drop_rows_without_f0(clean)
    expect_false("voiced_run" %in% names(res$data))
    expect_identical(c(res$n, res$gaps), c(0L, 0L))

    # Equidistant points: points left empty go, and voiced_run marks them
    ex <- fpx_exports(session, fp_export_data, fp_sampling_mode = "equal", fp_sampling_n = 21,
                      fp_sampling_method = "nearest")
    keep <- ex$keep$data
    drop <- ex$drop$data
    expect_true(all(c("point", "time_prop") %in% names(drop)))
    expect_gt(ex$drop$n_unvoiced, 0L)
    expect_equal(nrow(drop), nrow(keep) - ex$drop$n_unvoiced)
    expect_false(anyNA(drop$f0))
    expect_identical(drop, fpx_reference(keep, gap_ratio = Inf))
    expect_equal(max(drop$voiced_run[drop$token == "tokA"]), 3L)
    expect_equal(max(drop$voiced_run[drop$token == "tokC"]), 1L)
    expect_equal(max(drop$voiced_run[drop$token == "tokD"]), 2L)
  })
})

test_that("rows already removed: the time steps still mark the gap at native frames", {
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  # As in a re-loaded file that was dropped before: r1 has no rows without
  # f0 but one 20 ms step where a row was removed.
  r1 <- fpx_frames("r1", 150 + 0:11)
  r1 <- r1[-5, ]
  f0 <- rbind(r1, fpx_frames("r2", 160 + 0:9))
  rownames(f0) <- NULL
  srv <- fpx_server(fpx_audio(unique(f0$token)), f0)
  shiny::testServer(srv$server, {
    ex <- fpx_exports(session, fp_export_data, fp_region = "voiced", fp_sampling_mode = "native")
    keep <- ex$keep$data
    drop <- ex$drop$data
    expect_true(all(keep$has_gap[keep$token == "r1"]))
    expect_false(any(keep$has_gap[keep$token == "r2"]))
    expect_equal(ex$drop$n_unvoiced, 0L)
    expect_equal(nrow(drop), nrow(keep))
    expect_equal(ex$drop$run_gaps, 1L)
    expect_identical(drop$voiced_run[drop$token == "r1"], rep(1:2, c(4, 7)))
    expect_identical(drop$voiced_run[drop$token == "r2"], rep(1L, 10))
    expect_identical(drop, fpx_reference(keep))
    cap <- fpx_summary(output)
    expect_no_match(cap, "Dropped", fixed = TRUE)
    expect_match(cap, "The voiced_run column marks 1 gap(s) inside tokens.", fixed = TRUE)

    # The helper: rows_only reads no gap from the time steps
    expect_identical(export_drop_rows_without_f0(keep)$gaps, 1L)
    res <- export_drop_rows_without_f0(keep, rows_only = TRUE)
    expect_false("voiced_run" %in% names(res$data))
    expect_identical(c(res$n, res$gaps), c(0L, 0L))
  })
})

test_that("equidistant points with 5 or fewer per token mark no false gaps", {
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  # Nine tokens voiced for 0.20 s and one (t10) for 0.40 s, no rows without
  # f0 inside the voiced region. With 5 points, t10's step is twice the
  # others', which the time steps alone would read as a gap at every point.
  mk <- function(tok, nvoiced) fpx_frames(tok, c(NA, NA, 150 + seq_len(nvoiced), NA, NA))
  f0 <- do.call(rbind, c(lapply(sprintf("t%02d", 1:9), mk, nvoiced = 21), list(mk("t10", 41))))
  srv <- fpx_server(fpx_audio(unique(f0$token)), f0)
  shiny::testServer(srv$server, {
    for (n in c(3, 5, 6, 21)) {
      ex <- fpx_quiet_palette(fpx_exports(session, fp_export_data, fp_region = "voiced",
                                          fp_sampling_mode = "equal", fp_sampling_n = n,
                                          fp_sampling_method = "linear"))
      drop <- ex$drop$data
      expect_equal(nrow(drop), 10L * n)
      expect_equal(ex$drop$n_unvoiced, 0L)
      expect_equal(ex$drop$run_gaps, 0L)
      expect_false("voiced_run" %in% names(drop))
      expect_no_match(fpx_summary(output), "voiced_run column marks", fixed = TRUE)
    }
  })
  # A point left without f0 is still a gap with 5 points
  f0 <- rbind(f0[f0$token != "t01", ],
              fpx_frames("t01", c(NA, NA, 150 + 1:10, NA, 162 + 1:10, NA, NA)))
  srv <- fpx_server(fpx_audio(unique(f0$token)), f0)
  shiny::testServer(srv$server, {
    ex <- fpx_quiet_palette(fpx_exports(session, fp_export_data, fp_region = "voiced",
                                        fp_sampling_mode = "equal", fp_sampling_n = 5,
                                        fp_sampling_method = "nearest"))
    drop <- ex$drop$data
    expect_equal(ex$drop$n_unvoiced, 1L)
    expect_equal(ex$drop$run_gaps, 1L)
    expect_identical(drop$voiced_run[drop$token == "t01"], c(1L, 1L, 2L, 2L))
    expect_identical(drop$voiced_run[drop$token == "t10"], rep(1L, 5))
    expect_identical(drop, fpx_reference(ex$keep$data, gap_ratio = Inf))
  })
})

test_that("a TextGrid interval region: the time between intervals is not a gap", {
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  dir <- tempfile("tg")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # Two vowels per token, with a consonant between them that has f0. w1 has
  # f0 in every frame; w2 has one row without f0 inside its second vowel.
  iv <- data.frame(a = c(0, 0.05, 0.10, 0.25, 0.32, 0.50), b = c(0.05, 0.10, 0.25, 0.32, 0.50, 0.6),
                   lab = c("", "m", "a", "m", "a", ""))
  toks <- c("w1", "w2")
  for (t in toks) fpx_textgrid(file.path(dir, paste0(t, ".TextGrid")), iv)
  f2 <- 150 + (0:59)
  f2[41] <- NA                                    # 0.405 s, in the second /a/
  f0 <- rbind(fpx_frames("w1", 150 + (0:59)), fpx_frames("w2", f2))
  srv <- fpx_server(fpx_audio(toks, file.path(dir, paste0(toks, ".TextGrid"))), f0)
  shiny::testServer(srv$server, {
    ex <- fpx_exports(session, fp_export_data, fp_region = "interval", fp_region_tier = "phone",
                      fp_region_mode = "vowel", fp_sampling_mode = "native")
    keep <- ex$keep$data
    drop <- ex$drop$data
    expect_equal(nrow(keep), 66L)
    expect_identical(as.vector(tapply(keep$has_gap, keep$token, any)[toks]), c(FALSE, TRUE))
    expect_identical(as.vector(tapply(keep$n_missing, keep$token, max)[toks]), c(0L, 1L))
    expect_equal(ex$drop$n_unvoiced, 1L)
    expect_equal(ex$drop$run_gaps, 1L)
    expect_false(any(drop$voiced_run[drop$token == "w1"] != 1L))
    w2 <- drop[drop$token == "w2", ]
    expect_identical(w2$voiced_run, as.integer(ifelse(w2$time < 0.405, 1L, 2L)))
    expect_identical(drop, fpx_reference(keep, gap_ratio = Inf))
    cap <- fpx_summary(output)
    expect_match(cap, "1 token(s) had an unvoiced gap inside the measured region", fixed = TRUE)
    expect_match(cap, "The voiced_run column marks 1 gap(s) inside tokens.", fixed = TRUE)

    # The flag helper keeps the rows in their order, and reads them in time
    # order: with w2's row without f0 moved first, w2 still has a gap
    na_row <- which(keep$token == "w2" & is.na(keep$f0))
    perm <- c(na_row, setdiff(rev(seq_len(nrow(keep))), na_row))
    moved <- keep[perm, setdiff(names(keep), c("n_missing", "has_gap"))]
    rownames(moved) <- NULL
    fl <- export_flag_gaps(moved, rows_only = TRUE)
    expect_identical(fl$time, moved$time)
    expect_identical(fl$token, moved$token)
    expect_identical(fl$has_gap, keep$has_gap[perm])
    expect_identical(fl$n_missing, keep$n_missing[perm])
    # Read from the time steps, the jump between the vowels is a gap
    expect_true(all(export_flag_gaps(moved)$has_gap))
  })
})

test_that("a resumed file's voiced_run is not carried into the new export", {
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  f0 <- fpx_fixture()
  # A resumed F0 Data Export: f0 = correction, f0_original = tracker value,
  # with the columns an earlier export added, and a stale voiced_run that
  # breaks at every row.
  old <- f0
  old$f0_original <- old$f0
  old$edited      <- FALSE
  old$has_gap     <- TRUE
  old$n_missing   <- 99L
  old$voiced_run  <- seq_len(nrow(old))
  old$speaker     <- "S1"
  path <- tempfile(fileext = ".csv")
  utils::write.csv(old, path, row.names = FALSE)
  srv <- fpx_server(fpx_audio(unique(f0$token)), NULL)
  shiny::testServer(srv$server, {
    session$setInputs(fp_extract_mode = "csv",
                      fp_f0_upload_file = list(datapath = path, name = "resumed.csv"),
                      fp_f0_col_token = "token", fp_f0_col_time = "time", fp_f0_col_f0 = "f0")
    d <- srv$args$fp_f0_data()
    expect_false(is.null(d))
    expect_equal(nrow(d), nrow(f0))
    # Landmark / metadata columns are carried; the export's own columns are not
    expect_true("speaker" %in% names(d))
    expect_false(any(c("voiced_run", "has_gap", "n_missing") %in% names(d)))

    ex <- fpx_exports(session, fp_export_data, fp_region = "voiced", fp_sampling_mode = "native")
    expect_false("voiced_run" %in% names(ex$keep$data))
    drop <- ex$drop$data
    expect_identical(as.vector(tapply(drop$voiced_run, drop$token, max)[c("tokA", "tokB", "tokC", "tokD")]),
                     c(3L, 3L, 1L, 2L))
    expect_identical(drop, fpx_reference(ex$keep$data))
  })
})

test_that("the help says what voiced_run is for, inside and outside shinytone", {
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  srv <- fpx_server(fpx_audio("tokA"), NULL)
  shiny::testServer(srv$server, {
    guide <- paste(as.character(output$fp_extraction_guide$html), collapse = " ")
    expect_match(guide, "A <code>voiced_run</code> column then numbers the stretches with f0", fixed = TRUE)
    expect_match(guide, "Curate and Visualise break the line at each gap either way", fixed = TRUE)
    expect_match(guide, "Other plotting tools join the line across the removed rows unless they also group by <code>voiced_run</code>",
                 fixed = TRUE)
    expect_no_match(guide, "for tools that expect measured values only", fixed = TRUE)
    # Multisyllabic words: the syllables in sequence, with or without the gap
    expect_match(guide, paste0("<code>&lt;tier&gt;_tseq</code> (syllables in sequence), ",
                               "<code>&lt;tier&gt;_tbound</code> (in sequence, gap kept) or ",
                               "<code>token_t01</code> (the whole word, syllable ratio kept)"),
                 fixed = TRUE)
    expect_no_match(guide, "stacked", ignore.case = TRUE)
    side <- paste(as.character(output$ui_fp_extraction$html), collapse = " ")
    expect_match(side, "Drop rows without f0", fixed = TRUE)
    expect_match(side, "<code>voiced_run</code>\\s+column\\s+marks where they were")
    # No em dash in the new wording
    expect_no_match(paste(guide, side), "\u2014", fixed = TRUE)
  })
})

# The download with the box ticked, uploaded to the Start page: every row
# has f0, nothing more is removed, and voiced_run is kept.
fpx_round_trip <- function(f0, name, ...) {
  srv  <- fpx_server(fpx_audio(unique(f0$token)), f0)
  file <- tempfile(fileext = ".csv")
  got  <- new.env()
  shiny::testServer(srv$server, {
    got$ex <- fpx_quiet_palette(fpx_exports(session, fp_export_data, ...))
    session$setInputs(fp_extract_filename = name)
    expect_true(file.copy(output$fp_extract_download, file, overwrite = TRUE))
  })
  ex  <- got$ex
  out <- utils::read.csv(file, encoding = "UTF-8", stringsAsFactors = FALSE)
  expect_true("voiced_run" %in% names(out))
  expect_false(anyNA(out$f0))
  expect_identical(as.integer(out$voiced_run), ex$drop$data$voiced_run)

  s <- start_server()
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(file, paste0(name, ".csv")))
    expect_identical(start_cols()$f0, "f0")
    sm <- attr(f0_screen()$screen, "summary")
    expect_equal(sm[["rows_without_f0"]], 0L)
    expect_equal(sm[["rows"]], nrow(out))
    expect_false(screen_plan()$remove)
    d <- dataset()
    expect_equal(nrow(d), nrow(out))
    expect_identical(as.integer(d$voiced_run), as.integer(out$voiced_run))
    expect_match(start_html(output$f0_screen_report),
                 '<strong class="st-verdict">All good:</strong> every row has f0.', fixed = TRUE)
  })
  list(ex = ex, out = out)
}

test_that("the dropped download is cleaned, with voiced_run, and the Start page keeps it", {
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  rt <- fpx_round_trip(fpx_fixture(), "fixture_dropped", fp_region = "voiced",
                       fp_sampling_mode = "native")
  expect_equal(nrow(rt$out), nrow(rt$ex$keep$data) - 6L)
  expect_identical(as.vector(tapply(rt$out$voiced_run, rt$out$token, max)[c("tokA", "tokB", "tokC", "tokD")]),
                   c(3L, 3L, 1L, 2L))
})

test_that("f0_long.csv: the dropped download is cleaned, with voiced_run, and the Start page keeps it", {
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  src <- normalizePath(test_path("..", "..", "test", "f0_long.csv"), mustWork = FALSE)
  skip_if_not(file.exists(src))
  f0 <- utils::read.csv(src, encoding = "UTF-8", stringsAsFactors = FALSE)
  rt <- fpx_round_trip(f0, "f0_long_dropped", fp_region = "voiced", fp_sampling_mode = "native")
  keep <- rt$ex$keep$data
  drop <- rt$ex$drop$data
  expect_equal(nrow(drop), nrow(keep) - rt$ex$drop$n_unvoiced)
  expect_identical(drop, fpx_reference(keep))
  # As drop_rows_without_f0() on the export with the box off, with no
  # values to read as no f0: the extraction has no 0 or negative Hz
  expect_false(any(keep$f0 <= 0, na.rm = TRUE))
  expect_identical(drop$voiced_run, drop_rows_without_f0(keep)$voiced_run)
})

# The role pickers say "variable", as in F0 Analysis.
test_that("the f0 CSV pickers say variable, not column", {
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  labs <- c("Token / filename variable:", "Time variable:", "f0 variable:")
  old  <- c("filename column:", "Time column:", "f0 column:")
  path <- tempfile(fileext = ".csv")
  utils::write.csv(fpx_frames("tokA", c(200, 202, 204)), path, row.names = FALSE)
  srv <- fpx_server(fpx_audio("tokA"), NULL)
  shiny::testServer(srv$server, {
    check <- function(what) {
      html <- paste(as.character(output$fp_f0_csv_col_pickers$html), collapse = " ")
      for (lab in labs) expect_match(html, paste0(">", lab, "</label>"), fixed = TRUE,
                                     label = paste(what, lab))
      for (o in old) expect_no_match(html, o, fixed = TRUE, label = paste(what, o))
    }
    session$setInputs(fp_extract_mode = "csv")
    check("before upload")
    session$setInputs(fp_f0_upload_file = list(datapath = path, name = "f0.csv"))
    check("after upload")
    side <- paste(as.character(output$fp_extract_run_btn$html), collapse = " ")
    expect_match(side, paste("f0 data loads automatically once you choose a CSV and pick its",
                             "token, time and f0 variables."), fixed = TRUE)
    expect_no_match(side, "the columns are mapped", fixed = TRUE)
  })
})

test_that("the F0 Correction filter pickers and notes say variable, not column", {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  pd <- utils::getParseData(parse(file.path(app, "ui", "fp_correction_ui.R"),
                                  keep.source = TRUE, encoding = "UTF-8"))
  # getParseText(): the parse data shortens long strings
  strs <- vapply(utils::getParseText(pd, pd$id[pd$token == "STR_CONST"]),
                 function(s) eval(parse(text = s, keep.source = FALSE)), "")
  for (s in c("Token ID variable (match key)", "Speaker variable (optional)",
              "Tone category variable (optional)",
              "Values of this variable must match the audio file basenames ",
              "Loaded CSV: %d rows, %d columns. Pick the token variable below.",
              "tone variables picked in the filter", "variables from the dropdowns.",
              "speaker / tone variables.")) {
    expect_true(s %in% strs, label = s)
  }
  for (o in c("Token column", "Speaker column", "Tone column", "this column",
              "Pick the token column", "tone columns picked", "columns from the dropdowns",
              "speaker / tone columns.")) {
    expect_false(any(grepl(o, strs, fixed = TRUE)), label = o)
  }
})
