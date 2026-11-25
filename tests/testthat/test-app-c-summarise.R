# The Summarise tab (inst/app/ui/summarise_ui.R) and the Start page: its
# column defaults follow the Start-page columns (start_cols), the user's own
# picks stay, a data_version bump clears the conversion, and "Show R code"
# starts with the Start-page steps (read_data_code() with start_settings).

# global.R's definitions and the Summarise module, with shiny's exports in
# reach, as in the running app. showNotification() records its messages.
sum_app_env <- function() {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  pkgs <- new.env(parent = globalenv())
  for (nm in getNamespaceExports("shiny")) assign(nm, getExportedValue("shiny", nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  for (f in c("summarise_ui.R", "summarise_plot.R"))
    sys.source(file.path(app, "ui", f), env, keep.source = FALSE)
  env$toasts <- list()
  env$showNotification <- function(ui, ..., id = NULL, type = "default", duration = 5) {
    env$toasts[[length(env$toasts) + 1L]] <- paste(as.character(ui), collapse = " ")
    invisible(id)
  }
  env
}

# summarise_ui bound to its reactives for shiny::testServer(). Arguments left
# out get the module's defaults (no Start columns, data_version or settings).
sum_bind <- function(env, ...) {
  rv <- function(x = NULL) shiny::reactiveVal(x)
  args <- utils::modifyList(
    list(dataset = rv(), normalised_data = rv(), gca_pred_data = rv(), gamm_pred_data = rv(),
         curated_data = NULL, curated_norm_data = NULL,
         start_cols = NULL, data_version = NULL, start_settings = NULL),
    list(...))
  bound <- function(input, output, session) NULL
  body(bound) <- body(env$summarise_ui)
  environment(bound) <- list2env(args, parent = env)
  bound
}

# Long-format f0: 2 speakers x 3 tones x 2 items x 2 repetitions, 11 frames.
# `rec` is the token ID (a name no guess picks; the guess would be `item`).
# f0 is the tracker's, with no f0 at both edges of every token; f0_corrected
# has f0 on every row.
sum_fixture <- function() {
  set.seed(3)
  g <- expand.grid(speaker = c("S1", "S2"), tone = c("T1", "T2", "T3"), item = c("w1", "w2"),
                   rep = 1:2, stringsAsFactors = FALSE)
  do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    t  <- seq(0, 0.2, length.out = 11)
    hz <- switch(g$tone[i], T1 = 220 + 0 * t, T2 = 170 + 400 * t, T3 = 260 - 400 * t) +
          40 * (g$speaker[i] == "S2") + stats::rnorm(11, sd = 1)
    raw <- hz
    raw[c(1, 11)] <- NA
    data.frame(speaker = g$speaker[i], tone = g$tone[i], item = g$item[i],
               rec = paste(g$speaker[i], g$tone[i], g$item[i], g$rep[i], sep = "_"),
               time = t, f0 = raw, f0_corrected = hz, f0_praat = hz * 1.01,
               stringsAsFactors = FALSE)
  }))
}

sum_html <- function(x) paste(as.character(x$html), collapse = " ")
# The value a rendered selectInput selects.
sum_selected <- function(html, id) {
  sel <- regmatches(html, regexpr(sprintf('(?s)<select id="%s".*?</select>', id), html, perl = TRUE))
  if (!length(sel)) return(NULL)
  opt <- regmatches(sel, regexpr('<option value="[^"]*" selected>', sel))
  if (!length(opt)) return(NULL)
  sub('<option value="([^"]*)" selected>', "\\1", opt)
}
# The R code as text: what the <code> block holds, unescaped.
sum_code <- function(x) {
  txt <- sum_html(x)
  txt <- sub("(?s)^.*?<code>(.*)</code>.*$", "\\1", txt, perl = TRUE)
  txt <- gsub("&lt;", "<", gsub("&gt;", ">", txt, fixed = TRUE), fixed = TRUE)
  gsub("&amp;", "&", txt, fixed = TRUE)
}
sum_raw_inputs <- function(session, f0 = "f0_corrected", token = "rec") {
  session$setInputs(sum_source = "raw_hz", sum_raw_token_var = token, sum_raw_f0_var = f0,
                    sum_raw_time_var = "time", sum_raw_tone_var = "tone",
                    sum_raw_speaker_var = "speaker", sum_boundary_tol = 0.1,
                    sum_turn_threshold = 0.5)
}

test_that("Summarise's token and f0 defaults follow the Start-page columns", {
  env <- sum_app_env()
  d  <- sum_fixture()
  sc <- shiny::reactiveVal(list(token = "rec", time = "time", f0 = "f0_corrected"))
  nd <- d
  nd$f0_lz <- as.numeric(scale(log(nd$f0_corrected)))
  norm <- shiny::reactiveVal(nd)
  srv <- sum_bind(env, dataset = shiny::reactiveVal(d), normalised_data = norm, start_cols = sc)
  shiny::testServer(srv, {
    html <- sum_html(output$ui_summarise)
    expect_identical(sum_selected(html, "sum_raw_token_var"), "rec")
    expect_identical(sum_selected(html, "sum_raw_f0_var"), "f0_corrected")
    expect_identical(sum_selected(html, "sum_raw_time_var"), "time")      # unchanged
    expect_identical(sum_selected(html, "sum_raw_tone_var"), "tone")
    # Normalised data: the normalised column keeps its priority
    expect_identical(sum_selected(html, "sum_norm_token_var"), "rec")
    expect_identical(sum_selected(html, "sum_norm_f0_var"), "f0_lz")
    # ... and without one, the screened column rather than the raw f0
    norm(d)
    session$flushReact()
    html <- sum_html(output$ui_summarise)
    expect_identical(sum_selected(html, "sum_norm_f0_var"), "f0_corrected")
    # The f0 the Start page screens gets no note
    session$setInputs(sum_raw_f0_var = "f0_corrected")
    expect_null(output$sum_raw_f0_note)
  })

  # Without the Start columns (an older call), the guesses as before
  srv0 <- sum_bind(env, dataset = shiny::reactiveVal(d))
  shiny::testServer(srv0, {
    html <- sum_html(output$ui_summarise)
    expect_identical(sum_selected(html, "sum_raw_token_var"), "item")
    expect_identical(sum_selected(html, "sum_raw_f0_var"), "f0")
  })
})

test_that("Summarise keeps the user's own pick when the Start columns change", {
  env <- sum_app_env()
  d  <- sum_fixture()
  sc <- shiny::reactiveVal(list(token = "rec", time = "time", f0 = "f0_corrected"))
  ds <- shiny::reactiveVal(d)
  srv <- sum_bind(env, dataset = ds, start_cols = sc)
  shiny::testServer(srv, {
    html <- sum_html(output$ui_summarise)
    expect_identical(sum_selected(html, "sum_raw_f0_var"), "f0_corrected")
    # The browser sends the token default back; f0 is the user's own pick
    session$setInputs(sum_source = "raw_hz", sum_raw_token_var = "rec", sum_raw_f0_var = "f0",
                      sum_raw_tone_var = "speaker")
    # The pick is not the screened column and has rows without f0: a note
    note <- sum_html(output$sum_raw_f0_note)
    expect_match(note, "f0 has 48 rows without f0. The Start page screens f0_corrected.", fixed = TRUE)

    sc(list(token = "item", time = "time", f0 = "f0_praat"))
    session$flushReact()
    html <- sum_html(output$ui_summarise)
    expect_identical(sum_selected(html, "sum_raw_f0_var"), "f0")          # the user's pick stays
    expect_identical(sum_selected(html, "sum_raw_tone_var"), "speaker")   # so does this one
    expect_identical(sum_selected(html, "sum_raw_token_var"), "item")     # the default follows
    expect_identical(sum_selected(html, "sum_source"), "raw_hz")
    expect_match(sum_html(output$sum_raw_f0_note), "The Start page screens f0_praat.", fixed = TRUE)

    # A pick the data no longer has falls back to the default
    ds(d[, setdiff(names(d), "f0")])
    session$flushReact()
    expect_identical(sum_selected(sum_html(output$ui_summarise), "sum_raw_f0_var"), "f0_praat")
  })
})

test_that("Summarise keeps the user's pick when the Start default changes and changes back", {
  env <- sum_app_env()
  d  <- sum_fixture()
  sc <- shiny::reactiveVal(list(token = "rec", time = "time", f0 = "f0_corrected"))
  srv <- sum_bind(env, dataset = shiny::reactiveVal(d), start_cols = sc)
  shiny::testServer(srv, {
    output$ui_summarise
    sum_raw_inputs(session, f0 = "f0_praat")   # the user's own pick
    sc(list(token = "rec", time = "time", f0 = "f0_praat"))
    session$flushReact()
    expect_identical(sum_selected(sum_html(output$ui_summarise), "sum_raw_f0_var"), "f0_praat")
    sc(list(token = "rec", time = "time", f0 = "f0_corrected"))
    session$flushReact()
    expect_identical(sum_selected(sum_html(output$ui_summarise), "sum_raw_f0_var"), "f0_praat")
    # Picking the default again follows the Start page
    session$setInputs(sum_raw_f0_var = "f0_corrected")
    sc(list(token = "rec", time = "time", f0 = "f0"))
    session$flushReact()
    expect_identical(sum_selected(sum_html(output$ui_summarise), "sum_raw_f0_var"), "f0")
  })
})

test_that("Summarise's curated route, which has no pickers, uses the Start columns", {
  env <- sum_app_env()
  d  <- sum_fixture()
  cd <- d
  attr(cd, "curate_vars") <- list(token = "rec", tone = "tone", speaker = "speaker")
  sc <- shiny::reactiveVal(list(token = "rec", time = "time", f0 = "f0_corrected"))
  srv <- sum_bind(env, dataset = shiny::reactiveVal(d), curated_data = shiny::reactiveVal(cd),
                  start_cols = sc)
  shiny::testServer(srv, {
    session$setInputs(sum_source = "curated", sum_boundary_tol = 0.1, sum_turn_threshold = 0.5)
    # f0_corrected has f0 at every token's edges (the guess, f0, has not)
    expect_null(output$sum_track_edges)
    expect_null(output$sum_cur_f0_note)
    session$setInputs(sum_button = 1)
    lz <- sum_params()$lz
    expect_identical(lz[c("token", "f0", "time", "tone", "speaker")],
                     list(token = "rec", f0 = "f0_corrected", time = "time", tone = "tone",
                          speaker = "speaker"))
    expect_identical(sum_result()$tone, c("T1", "T2", "T3"))
    expect_identical(sum_result()$refline, c("33", "15", "51"))
  })
})

test_that("a data_version bump or a new upload clears the Summarise conversion", {
  env <- sum_app_env()
  d  <- sum_fixture()
  dv <- shiny::reactiveVal(0L)
  srv <- sum_bind(env, dataset = shiny::reactiveVal(d), data_version = dv,
                  start_cols = shiny::reactive(list(token = "rec", time = "time", f0 = "f0_corrected")))
  shiny::testServer(srv, {
    sum_raw_inputs(session)
    session$setInputs(sum_button = 1)
    expect_false(is.null(sum_result()))
    expect_false(is.null(sum_norm_contours()))
    expect_false(is.null(sum_params()))
    dv(1L)
    session$flushReact()
    expect_null(sum_result())
    expect_null(sum_norm_contours())
    expect_null(sum_params())
    session$setInputs(sum_button = 2)
    expect_false(is.null(sum_result()))
    session$setInputs(uploadfile = list(name = "next.csv", datapath = "next.csv"))
    expect_null(sum_result())
  })

  # Without data_version (an older call), a new upload still clears it
  srv0 <- sum_bind(env, dataset = shiny::reactiveVal(d))
  shiny::testServer(srv0, {
    sum_raw_inputs(session)
    session$setInputs(sum_button = 1)
    expect_false(is.null(sum_result()))
    session$setInputs(uploadfile = list(name = "next.csv", datapath = "next.csv"))
    expect_null(sum_result())
  })
})

test_that("Summarise's R code starts with the Start-page steps", {
  env <- sum_app_env()
  d  <- sum_fixture()
  settings <- list(
    version = "0.9.1", zero_hz = c(f0 = 3L), dropped = NULL, metadata = NULL, durations = list(),
    screen = list(token = "rec", time = "time", f0 = "f0", unvoiced = NULL, action = "remove",
                  min_run = 1, rows = 264L, without = 48L,
                  summary = c(rows_without_f0 = 48L, start = 24L, end = 24L, inside = 0L, gaps = 0L),
                  rows_after = 216L, gaps_marked = 0, short_rows = 0))
  gca <- shiny::reactiveVal(NULL)
  srv <- sum_bind(env, dataset = shiny::reactiveVal(d), start_settings = shiny::reactive(settings),
                  gca_pred_data = gca)
  shiny::testServer(srv, {
    session$setInputs(dataset_name = "tones")
    sum_raw_inputs(session)
    session$setInputs(sum_button = 1, sum_show_code = 1)
    code <- sum_code(output$summarise_r_code)
    at <- function(x) regexpr(x, code, fixed = TRUE)[[1]]
    expect_gt(at('stopifnot(utils::packageVersion("shinytone") >= "0.9.1")'), 0)
    expect_gt(at('dat <- shinytone::read_csv_utf8("tones.csv"'), at("stopifnot("))
    expect_gt(at('dat <- shinytone::drop_rows_without_f0(dat, token = "rec", time = "time", f0 = "f0")'),
              at("dat <- shinytone::zero_hz_to_na(dat)"))
    expect_gt(at("# Step 0: Convert each speaker"), at("drop_rows_without_f0(dat"))
    expect_gt(at('pred <- compute_mean_contour(dat, token = "rec", f0 = "f0_lz"'), at("normalise_f0(dat"))
    expect_identical(at("# dat: your f0 data"), -1L)

    # Model predictions: the same start, then a pointer to the model's fit
    gca(data.frame(time = rep(c(0, 0.5, 1), 2), f0_predicted = c(1, 2, 3, 3, 2, 1),
                             tone = rep(c("T2", "T4"), each = 3)))
    session$setInputs(sum_source = "gca", sum_button = 2)
    code <- sum_code(output$summarise_r_code)
    expect_gt(at("drop_rows_without_f0(dat"), at("stopifnot("))
    expect_gt(at("# pred <- shinytone::predict_gca(fit, n = 100)"), at("drop_rows_without_f0(dat"))
  })

  # Without start_settings, the read lines alone, as before
  srv0 <- sum_bind(env, dataset = shiny::reactiveVal(d))
  shiny::testServer(srv0, {
    session$setInputs(dataset_name = "tones")
    sum_raw_inputs(session)
    session$setInputs(sum_button = 1, sum_show_code = 1)
    code <- sum_code(output$summarise_r_code)
    expect_match(code, paste(env$read_data_code("tones.csv"), collapse = "\n"), fixed = TRUE)
    expect_false(grepl("stopifnot(", code, fixed = TRUE))
  })
})

# The app's server with the real Summarise tab (start_server(), in
# helper-app-start.R, stubs the analysis tabs).
sum_start_server <- function(.local = parent.frame()) {
  s <- start_server(.local)
  app <- system.file("app", package = "shinytone")
  for (f in c("summarise_ui.R", "summarise_plot.R"))
    sys.source(file.path(app, "ui", f), s$env, keep.source = FALSE)
  s
}

test_that("in the app, Summarise gets the Start columns and its R code reproduces its numerals", {
  s <- sum_start_server()
  d <- sum_fixture()
  # Only the second repetitions miss their first and last points, so the
  # drop leaves every speaker, tone and item
  r1 <- grepl("_1$", d$rec)
  d$f0[r1] <- d$f0_corrected[r1]
  d$f0_corrected <- d$f0_praat <- NULL
  f <- start_write_csv(d)
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "tones.csv"),
                      dataset_name = sub("\\.csv$", "", f))
    # The Start page guesses item; the user picks rec
    expect_identical(start_cols()$token, "item")
    session$setInputs(screen_token = "rec")
    expect_identical(start_cols(), list(token = "rec", time = "time", f0 = "f0"))
    # Every token has 11 rows: removal is ticked, as for frames, and the
    # rows without f0 flag their tokens; the user drops those tokens, so
    # nothing is left to remove
    expect_true(output$screen_show_remove)
    expect_true(screen_plan()$remove)
    expect_false(anyNA(dataset()$f0))
    expect_true(screen_plan()$flag)
    session$setInputs(screen_drop_flagged = TRUE)
    expect_false(screen_plan()$remove)
    expect_false(anyNA(dataset()$f0))
    expect_identical(length(unique(dataset()$rec)), 12L)
    html <- sum_html(output$ui_summarise)
    expect_identical(sum_selected(html, "sum_raw_token_var"), "rec")
    sum_raw_inputs(session, f0 = "f0")
    session$setInputs(sum_button = 1, sum_show_code = 1)
    tab <- utils::read.csv(output$sum_download_csv, check.names = FALSE, stringsAsFactors = FALSE)
    expect_identical(tab$Tone, c("T1", "T2", "T3"))
    code <- sum_code(output$summarise_r_code)
    expect_match(code, 'dat <- shinytone::flag_f0_tokens(dat, token = "rec", time = "time", f0 = "f0")',
                 fixed = TRUE)
    expect_match(code, 'dat <- dat[dat[["screen_flag"]] == "", , drop = FALSE]', fixed = TRUE)
    expect_no_match(code, "drop_rows_without_f0(", fixed = TRUE)

    # Run the code (without the library lines and the version check, as the
    # package here is the source tree): the same numerals as the app's
    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    lines <- lines[!grepl("^(library\\(|stopifnot\\()", lines)]
    run <- new.env(parent = globalenv())
    utils::capture.output(eval(parse(text = lines), envir = run))
    expect_identical(nrow(run$dat), nrow(dataset()))
    expect_identical(run$chao_summary$refline, as.character(tab$`Ref Numeral`))
    expect_identical(run$chao_summary$interval, as.character(tab$`Int Numeral`))

    # Unticking the drop changes the rows: the conversion is cleared
    expect_false(is.null(output$summarise_summary))
    dv <- data_version()
    session$setInputs(screen_drop_flagged = FALSE)
    expect_identical(data_version(), dv + 1L)
    expect_error(output$summarise_summary)
  })
})

# 3 tones x 3 tokens at 21 equidistant points, with time_prop (0 to 1 over
# the extracted region) and the real time. The first point of every token has
# no f0, so on time_prop the contours start at 0.05.
sum_tp_fixture <- function() {
  set.seed(5)
  g <- expand.grid(tone = c("T1", "T2", "T3"), rep = 1:3, stringsAsFactors = FALSE)
  do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    tp <- seq(0, 1, length.out = 21)
    hz <- switch(g$tone[i], T1 = 220 + 0 * tp, T2 = 170 + 80 * tp, T3 = 260 - 80 * tp) +
          stats::rnorm(21, sd = 0.5)
    hz[1] <- NA
    data.frame(token = paste(g$tone[i], g$rep[i], sep = "_"), speaker = "S1", tone = g$tone[i],
               point = 1:21, time_prop = tp, time = 0.1 + 0.2 * tp, f0 = hz,
               stringsAsFactors = FALSE)
  }))
}

test_that("Summarise marks a tone whose contour does not reach 0 or 1 (F5)", {
  env <- sum_app_env()
  d <- sum_tp_fixture()
  srv <- sum_bind(env, dataset = shiny::reactiveVal(d),
                  start_cols = shiny::reactive(list(token = "token", time = "time", f0 = "f0")))
  shiny::testServer(srv, {
    guide <- sum_html(output$summarise_guide)
    expect_match(guide, "halfway between its first and last f0 (0.5 on the time axis)", fixed = TRUE)
    expect_match(guide, "which runs from each syllable's first f0 (0) to its last f0 (1)", fixed = TRUE)
    # One syllable per token: the guide recommends the syllable tier's _t01
    # and no longer lists the whole-word axes
    txt <- gsub("<[^>]*>", "", guide)
    expect_match(txt, paste("One syllable per token. Each numeral describes one syllable's tone, as in",
                            "citation tones. For multisyllabic words, use &lt;syllable_tier&gt;_t01 from",
                            "Normalise, which runs from each syllable's first f0 (0) to its last f0 (1).",
                            "Make each syllable its own token (the token ID plus &lt;syllable_tier&gt;_i,",
                            "its number in the word), and label its tone with the word's tone pattern",
                            "and its place (e.g. T1T3_1, T1T3_2). Each syllable then gets its own",
                            "numeral, all on one range."), fixed = TRUE)
    expect_no_match(txt, "is read as one contour", fixed = TRUE)
    expect_no_match(txt, "&lt;tier&gt;", fixed = TRUE)
    # time_prop is used as it is: the contours start at its second point
    session$setInputs(sum_source = "raw_hz", sum_raw_token_var = "token", sum_raw_f0_var = "f0",
                      sum_raw_time_var = "time_prop", sum_raw_tone_var = "tone",
                      sum_raw_speaker_var = "speaker", sum_boundary_tol = 0.1,
                      sum_turn_threshold = 0.5, sum_button = 1)
    expect_equal(sum_result()$t_first, rep(0.05, 3))
    expect_equal(sum_result()$t_last, rep(1, 3))
    tab <- sum_html(output$summarise_summary)
    expect_identical(lengths(regmatches(tab, gregexpr("\u2020</span>", tab, fixed = TRUE))), 3L)
    expect_match(tab, "Contours that do not reach 0 or 1 (\u2020):", fixed = TRUE)
    expect_match(tab, "Tone T2 runs from 0.05 to 1.00.", fixed = TRUE)
    expect_match(tab, "read at the nearest measured point", fixed = TRUE)
    # The amber notice above says the same for the tokens
    expect_match(sum_html(output$sum_track_edges), "in all 9 tokens the f0 does not", fixed = TRUE)
    # On the real time column each token runs from its first f0 to its last
    session$setInputs(sum_raw_time_var = "time", sum_button = 2)
    expect_equal(sum_result()$t_first, rep(0, 3))
    expect_equal(sum_result()$t_last, rep(1, 3))
    tab <- sum_html(output$summarise_summary)
    expect_false(grepl("\u2020", tab, fixed = TRUE))
    expect_false(grepl("Contours that do not reach", tab, fixed = TRUE))
    expect_null(output$sum_track_edges)
  })
})

test_that("Summarise marks a model's tone whose fitted rows miss 0 or 1", {
  env <- sum_app_env()
  tt <- seq(0, 1, length.out = 101)
  pred <- data.frame(time = rep(tt, 3),
                     f0_predicted = c(0 * tt + 1, -1 + 2 * tt, 1 - 2 * tt),
                     tone = rep(c("T1", "T2", "T3"), each = 101))
  attr(pred, "time_coverage") <- data.frame(tone = c("T1", "T2", "T3"), t_first = c(0, 0.05, 0),
                                            t_last = c(1, 1, 0.9), stringsAsFactors = FALSE)
  gca <- shiny::reactiveVal(pred)
  srv <- sum_bind(env, dataset = shiny::reactiveVal(sum_fixture()), gca_pred_data = gca)
  shiny::testServer(srv, {
    session$setInputs(sum_source = "gca", sum_boundary_tol = 0.1, sum_turn_threshold = 0.5,
                      sum_button = 1)
    tab <- sum_html(output$summarise_summary)
    # The predicted curves reach 0 and 1; the fitted rows of T2 and T3 do not
    expect_equal(sum_result()$t_first, rep(0, 3))
    expect_identical(lengths(regmatches(tab, gregexpr("\u2020</span>", tab, fixed = TRUE))), 2L)
    expect_match(tab, "Tone T2: the rows the model fits run from 0.05 to 1.00 on the time axis.", fixed = TRUE)
    expect_match(tab, "Tone T3: the rows the model fits run from 0.00 to 0.90 on the time axis.", fixed = TRUE)
    expect_match(tab, "curve beyond them is extrapolated", fixed = TRUE)
    # A fit that reaches 0 and 1, or one made by an earlier version: no mark
    attr(pred, "time_coverage")$t_first <- 0
    attr(pred, "time_coverage")$t_last <- 1
    gca(pred)
    session$setInputs(sum_button = 2)
    expect_false(grepl("\u2020", sum_html(output$summarise_summary), fixed = TRUE))
    attr(pred, "time_coverage") <- NULL
    gca(pred)
    session$setInputs(sum_button = 3)
    expect_false(grepl("\u2020", sum_html(output$summarise_summary), fixed = TRUE))
  })
})

# Words of two syllables: syllable_t01 runs 0 to 1 in each syllable, so with
# token = word two rows share each time. Tone T1 is level, T2 falls in the
# first syllable and rises in the second.
sum_tie_fixture <- function() {
  g <- expand.grid(tone = c("T1", "T2"), rep = 1:3, stringsAsFactors = FALSE)
  do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    t01 <- seq(0, 1, length.out = 11)
    hz <- if (g$tone[i] == "T1") rep(220, 22) else c(260 - 80 * t01, 180 + 80 * t01)
    data.frame(word = paste(g$tone[i], g$rep[i], sep = "_"), speaker = "S1", tone = g$tone[i],
               syllable = rep(1:2, each = 11), time = seq(0, 0.42, by = 0.02),
               syllable_t01 = rep(t01, 2), f0 = hz, stringsAsFactors = FALSE)
  }))
}

test_that("Summarise shows the averaged-ties message as a warning, and no other message", {
  env <- sum_app_env()
  rec <- new.env()
  rec$toasts <- list()
  env$showNotification <- function(ui, ..., id = NULL, type = "default", duration = 5) {
    rec$toasts[[length(rec$toasts) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                  type = type)
    invisible(id)
  }
  # The messages that still reach the R console
  console <- function(expr) {
    rec$msgs <- character(0)
    withCallingHandlers(expr, message = function(m) {
      rec$msgs <- c(rec$msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
    rec$msgs
  }
  srv <- sum_bind(env, dataset = shiny::reactiveVal(sum_tie_fixture()))
  shiny::testServer(srv, {
    session$setInputs(sum_source = "raw_hz", sum_raw_token_var = "word", sum_raw_f0_var = "f0",
                      sum_raw_time_var = "syllable_t01", sum_raw_tone_var = "tone",
                      sum_raw_speaker_var = "speaker", sum_boundary_tol = 0.1,
                      sum_turn_threshold = 0.5)
    # The averaged-ties message leaves the R console for a warning toast; the
    # "already normalised" message stays in the console
    msgs <- console(session$setInputs(sum_button = 1))
    expect_false(is.null(sum_result()))
    expect_false(any(grepl("are averaged", msgs, fixed = TRUE)))
    expect_true(any(grepl("already normalised; using it as-is", msgs, fixed = TRUE)))
    tie <- Filter(function(t) grepl("are averaged", t$text, fixed = TRUE), rec$toasts)
    expect_length(tie, 1)
    expect_identical(tie[[1]]$type, "warning")
    expect_match(tie[[1]]$text, paste("In 6 token(s) (T1_1, T2_1, T1_2, T2_2, T1_3, ...), two or",
                                      "more rows have the same time, so their f0 values there are",
                                      "averaged."), fixed = TRUE)
    texts <- vapply(rec$toasts, function(t) t$text, "")
    expect_false(any(grepl("already normalised", texts, fixed = TRUE)))
    # On the real time column no rows share a time: no toast
    rec$toasts <- list()
    session$setInputs(sum_raw_time_var = "time")
    msgs <- console(session$setInputs(sum_button = 2))
    expect_false(is.null(sum_result()))
    expect_length(rec$toasts, 0)
  })
})

test_that("the averaged-ties toast writes a large count of tokens with a separator", {
  env <- sum_app_env()
  # 1,200 words whose two rows share a time: the package message counts them
  # as "In 1200 token(s)"; the toast says "In 1,200 token(s)"
  key <- rep(sprintf("w%04d", 1:1200), each = 2)
  withCallingHandlers(note_averaged_ties(key, t = rep(0, 2400), y = rep(200, 2400)),
                      message = env$averaged_ties_toast)
  expect_length(env$toasts, 1)
  expect_match(env$toasts[[1]], paste("In 1,200 token(s) (w0001, w0002, w0003, w0004, w0005,",
                                      "...), two or more rows have the same time"), fixed = TRUE)
  expect_match(env$toasts[[1]], "as <tier>_t01 does at each syllable of a word.", fixed = TRUE)
})

test_that("Summarise refuses a GAMM fit across syllables in sequence, with a warning", {
  env <- sum_app_env()
  rec <- new.env()
  rec$toasts <- list()
  env$showNotification <- function(ui, ..., id = NULL, type = "default", duration = 5) {
    rec$toasts[[length(rec$toasts) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                  type = type)
    invisible(id)
  }
  # A GAMM on <tier>_tseq: two syllables in sequence, so time runs 0 to 2
  tt <- seq(0, 2, length.out = 101)
  pred <- data.frame(time = rep(tt, 2), f0_predicted = c(0 * tt + 1, 1 - tt),
                     tone = rep(c("T1", "T2"), each = 101))
  attr(pred, "time_axis") <- "sequential"
  attr(pred, "n_segments") <- 2L
  gamm <- shiny::reactiveVal(pred)
  srv <- sum_bind(env, dataset = shiny::reactiveVal(sum_fixture()), gamm_pred_data = gamm)
  shiny::testServer(srv, {
    session$setInputs(sum_source = "gamm", sum_boundary_tol = 0.1, sum_turn_threshold = 0.5,
                      sum_button = 1)
    expect_null(sum_result())
    expect_length(rec$toasts, 1)
    expect_identical(rec$toasts[[1]]$type, "warning")
    expect_identical(rec$toasts[[1]]$text,
                     paste("Chao numerals describe one syllable's contour, and this",
                           "GAMM was fit across 2 segments in sequence.",
                           "For one numeral per syllable, model each syllable as its own",
                           "token on <syllable_tier>_t01 (see the guide)."))
    expect_no_match(rec$toasts[[1]]$text, "stacked", fixed = TRUE)
    # One segment (a GAMM on <tier>_t01 or token_t01): numerals, no toast
    pred$time <- pred$time / 2
    attr(pred, "time_axis") <- "sequential"
    attr(pred, "n_segments") <- 1L
    gamm(pred)
    rec$toasts <- list()
    session$setInputs(sum_button = 2)
    expect_false(is.null(sum_result()))
    expect_identical(sum_result()$tone, c("T1", "T2"))
    expect_false(any(grepl("segments in sequence", vapply(rec$toasts, function(t) t$text, ""),
                           fixed = TRUE)))
  })
})
