# The Start page's columns and data_version in the Inspect and Curate tabs
# (inst/app/ui/inspect_ui.R, curate_ui.R; Part C4 and C5): the token and f0
# pickers default to the Start page's columns and keep the user's own pick,
# the note under the f0 picker, and what a change of rows on the Start page
# clears. Neither tab has a "Show R code", so neither calls read_data_code().

# global.R's definitions and the two modules, with shiny's and dplyr's
# exports in reach, as in the running app.
ic_env <- function() {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  pkgs <- new.env(parent = globalenv())
  for (p in c("shiny", "dplyr"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  for (f in c("inspect_ui.R", "curate_ui.R"))
    sys.source(file.path(app, "ui", f), env, keep.source = FALSE)
  env
}

# A module bound to its reactives, for shiny::testServer(). updateSelectInput()
# records what the module sends (testServer has no browser to apply it), and
# showNotification() is silenced.
ic_bind <- function(env, name, ...) {
  rec <- new.env()
  rec$updates <- list()
  args <- list(...)
  args$updateSelectInput <- function(session, inputId, label = NULL, choices = NULL,
                                     selected = NULL) {
    rec$updates[[length(rec$updates) + 1L]] <- list(id = inputId, selected = selected)
    invisible(NULL)
  }
  args$showNotification <- function(...) invisible(NULL)
  bound <- function(input, output, session) NULL
  body(bound) <- body(env[[name]])
  environment(bound) <- list2env(args, parent = env)
  list(server = bound, rec = rec)
}

# Curate's plot is never drawn here, so plotly warns that its select and
# click events are not registered. Those warnings only are muffled.
ic_quiet <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("is not registered", conditionMessage(w), fixed = TRUE))
      invokeRestart("muffleWarning")
  })
}

# The value a select in a renderUI() output shows.
ic_selected <- function(ui, id) {
  html <- paste(as.character(ui$html), collapse = " ")
  m <- regmatches(html, regexpr(sprintf('(?s)<select id="%s".*?</select>', id), html, perl = TRUE))
  if (!length(m) || !grepl("selected>", m, fixed = TRUE)) return(NULL)
  sub('(?s).*<option value="([^"]*)" selected>.*', "\\1", m, perl = TRUE)
}
ic_updates <- function(rec, id) {
  vapply(Filter(function(u) identical(u$id, id), rec$updates), function(u) u$selected, "")
}

# Long-format f0: 2 speakers x 2 tones x 6 repetitions, 12 frames of 10 ms.
# f0 and f0_corrected have f0 in every row; f0_praat has none in each
# token's first and last row (48 rows); token and filename both name the
# token.
ic_data <- function() {
  set.seed(3)
  g <- expand.grid(spk = c("S1", "S2"), tone = c("T1", "T2"), rep = 1:6,
                   stringsAsFactors = FALSE)
  do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    t  <- round(seq(0.01, 0.12, by = 0.01), 2)
    f0 <- 150 + 30 * (g$tone[i] == "T2") + 100 * t + stats::rnorm(12, sd = 1)
    tok <- sprintf("%s_%s_%d", g$spk[i], g$tone[i], g$rep[i])
    data.frame(token = tok, filename = paste0(tok, ".wav"), time = t, f0 = f0,
               f0_corrected = f0 + 1, f0_praat = ifelse(seq_along(t) %in% c(1, 12), NA, f0),
               speaker = g$spk[i], tone = g$tone[i], stringsAsFactors = FALSE)
  }))
}

test_that("the two modules take the Start-page arguments last, with NULL defaults", {
  env <- ic_env()
  for (m in c("inspect_ui", "curate_ui")) {
    f <- formals(env[[m]])
    expect_identical(tail(names(f), 3), c("start_cols", "data_version", "start_settings"))
    expect_true(all(vapply(tail(f, 3), is.null, logical(1))))
  }
})

test_that("Inspect's token and f0 pickers follow the Start page and keep the user's pick", {
  env <- ic_env()
  d  <- ic_data()
  ds <- shiny::reactiveVal(d)
  sc <- shiny::reactiveVal(list(token = "filename", time = "time", f0 = "f0_corrected"))
  b  <- ic_bind(env, "inspect_ui", dataset = ds, inspect_result = shiny::reactiveVal(NULL),
                start_cols = sc, data_version = shiny::reactiveVal(0L), start_settings = NULL)
  shiny::testServer(b$server, {
    ui <- output$ui_inspect
    # The Start page's columns, not the tab's own guesses (token, f0)
    expect_identical(ic_selected(ui, "inspect_token_var"), "filename")
    expect_identical(ic_selected(ui, "inspect_f0_var"), "f0_corrected")
    expect_identical(ic_selected(ui, "inspect_time_var"), "time")
    # The browser sends the values it shows: not the user's pick
    session$setInputs(inspect_token_var = "filename", inspect_f0_var = "f0_corrected",
                      inspect_time_var = "time", inspect_speaker_var = "speaker",
                      inspect_tone_var = "tone", inspect_intensity_var = "")
    expect_null(picks$user[["inspect_f0_var"]])

    # The Start page's columns change: both pickers follow
    sc(list(token = "token", time = "time", f0 = "f0"))
    session$flushReact()
    expect_identical(ic_updates(b$rec, "inspect_token_var"), "token")
    expect_identical(ic_updates(b$rec, "inspect_f0_var"), "f0")
    session$setInputs(inspect_token_var = "token", inspect_f0_var = "f0")

    # The user picks f0_praat; the Start page's f0 changes again: the pick stays
    session$setInputs(inspect_f0_var = "f0_praat")
    expect_identical(picks$user[["inspect_f0_var"]], "f0_praat")
    sc(list(token = "filename", time = "time", f0 = "f0_corrected"))
    session$flushReact()
    expect_identical(ic_updates(b$rec, "inspect_token_var"), c("token", "filename"))
    expect_identical(ic_updates(b$rec, "inspect_f0_var"), "f0")
    session$setInputs(inspect_token_var = "filename")

    # The note under the f0 picker: f0_praat still has rows without f0
    note <- paste(as.character(output$inspect_f0_note$html), collapse = " ")
    expect_match(note, "f0_praat has 48 rows without f0. The Start page screens f0_corrected.",
                 fixed = TRUE)

    # A new column (say voiced_run) draws the sidebar again: the user's pick,
    # the Start page's token and a threshold the user changed all stay
    session$setInputs(inspect_z_thresh = 2.5)
    ds(cbind(d, voiced_run = 1L))
    session$flushReact()
    ui <- output$ui_inspect
    expect_identical(ic_selected(ui, "inspect_f0_var"), "f0_praat")
    expect_identical(ic_selected(ui, "inspect_token_var"), "filename")
    expect_match(paste(as.character(ui$html), collapse = " "),
                 'id="inspect_z_thresh"[^>]*value="2.5"')

    # Only the rows change: the sidebar is not drawn again (drawing it puts
    # each picker's default in picks$offered)
    saved <- picks$offered
    picks$offered <- list()
    ds(cbind(d, voiced_run = 1L)[-1, ])
    session$flushReact()
    ui <- output$ui_inspect
    expect_length(picks$offered, 0)
    picks$offered <- saved

    # The user picks the default back: no pick of their own now, so the
    # picker follows the Start page again
    session$setInputs(inspect_f0_var = "f0_corrected")
    expect_null(picks$user[["inspect_f0_var"]])
    sc(list(token = "filename", time = "time", f0 = "f0"))
    session$flushReact()
    expect_identical(ic_updates(b$rec, "inspect_f0_var"), c("f0", "f0"))
  })
})

test_that("Inspect takes the Start page's f0 only when it is in Hz", {
  env <- ic_env()
  d <- ic_data()
  d$f0_st <- 12 * log2(d$f0 / 160)
  d$f0_rapt <- ifelse(d$time %in% c(0.01, 0.12), -1, d$f0)   # -1 marks no f0
  sc <- shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0_st"))
  b  <- ic_bind(env, "inspect_ui", dataset = shiny::reactiveVal(d),
                inspect_result = shiny::reactiveVal(NULL), start_cols = sc,
                data_version = shiny::reactiveVal(0L), start_settings = NULL)
  shiny::testServer(b$server, {
    # f0_st is normalised: Inspect keeps its own Hz guess
    expect_identical(ic_selected(output$ui_inspect, "inspect_f0_var"), "f0")
    expect_identical(insp_defaults(d)$inspect_f0_var, "f0")
    # An Hz column with a -1 marker is taken
    sc(list(token = "token", time = "time", f0 = "f0_rapt"))
    session$flushReact()
    expect_identical(insp_defaults(d)$inspect_f0_var, "f0_rapt")
  })
})

test_that("Inspect's results are cleared when the rows change or a file is uploaded", {
  env <- ic_env()
  d  <- ic_data()
  ir <- shiny::reactiveVal(NULL)
  dv <- shiny::reactiveVal(0L)
  b  <- ic_bind(env, "inspect_ui", dataset = shiny::reactiveVal(d), inspect_result = ir,
                start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
                data_version = dv, start_settings = NULL)
  shiny::testServer(b$server, {
    session$setInputs(inspect_token_var = "token", inspect_f0_var = "f0",
                      inspect_time_var = "time", inspect_speaker_var = "speaker",
                      inspect_tone_var = "tone", inspect_intensity_var = "",
                      inspect_time_unit = "s", inspect_z_thresh = 3,
                      inspect_rise_thresh = 1.263, inspect_fall_thresh = 1.714,
                      inspect_carryover_mult = 1.5, inspect_level_thresh = 3.5,
                      inspect_intensity_drop = 15)
    session$setInputs(inspect_button = 1)
    expect_s3_class(ir(), "data.frame")
    expect_identical(nrow(ir()), nrow(d))

    dv(1L)
    session$flushReact()
    expect_null(ir())

    session$setInputs(inspect_button = 2)
    expect_s3_class(ir(), "data.frame")
    session$setInputs(uploadfile = list(name = "other.csv", datapath = tempfile()))
    expect_null(ir())
  })
})

test_that("Curate's token and f0 pickers follow the Start page; normalised f0 comes first", {
  env <- ic_env()
  up <- ic_data()
  ds <- shiny::reactiveVal(up)
  nd <- shiny::reactiveVal(NULL)
  cl <- shiny::reactiveVal(NULL)
  sc <- shiny::reactiveVal(list(token = "filename", time = "time", f0 = "f0_corrected"))
  b  <- ic_bind(env, "curate_ui", dataset_in = ds, normalised_data = nd,
                curated_data = shiny::reactiveVal(NULL), inspect_result = shiny::reactiveVal(NULL),
                cluster_data = cl, curated_norm_data = shiny::reactiveVal(NULL),
                start_cols = sc, data_version = shiny::reactiveVal(0L), start_settings = NULL)
  ic_quiet(shiny::testServer(b$server, {
    ui <- output$ui_curate
    expect_identical(ic_selected(ui, "curate_token_var"), "filename")
    expect_identical(ic_selected(ui, "curate_f0_var"), "f0_corrected")
    session$setInputs(curate_token_var = "filename", curate_f0_var = "f0_corrected",
                      curate_tone_var = "tone", curate_speaker_var = "speaker",
                      curate_item_var = "__none__")

    # Normalised data: its normalised column comes first
    nd(cbind(up, f0_st = 12 * log2(up$f0_corrected / 160)))
    session$flushReact()
    ui <- output$ui_curate
    expect_identical(ic_selected(ui, "curate_f0_var"), "f0_st")
    expect_identical(ic_selected(ui, "curate_token_var"), "filename")
    session$setInputs(curate_source = "normalised", curate_f0_var = "f0_st")
    expect_null(picks$user[["curate_f0_var"]])

    # The Start page's columns change: the token follows, f0 stays normalised
    sc(list(token = "token", time = "time", f0 = "f0"))
    session$flushReact()
    expect_identical(ic_updates(b$rec, "curate_token_var"), "token")
    expect_length(ic_updates(b$rec, "curate_f0_var"), 0)
    session$setInputs(curate_token_var = "token")

    # The user's own pick stays when the Start page changes
    session$setInputs(curate_f0_var = "f0_praat")
    sc(list(token = "filename", time = "time", f0 = "f0_corrected"))
    session$flushReact()
    expect_identical(ic_updates(b$rec, "curate_token_var"), c("token", "filename"))
    expect_length(ic_updates(b$rec, "curate_f0_var"), 0)
    note <- paste(as.character(output$curate_f0_note$html), collapse = " ")
    expect_match(note, "f0_praat has 48 rows without f0. The Start page screens f0_corrected.",
                 fixed = TRUE)
    # No note on the screened column itself
    session$setInputs(curate_f0_var = "f0_corrected")
    expect_null(output$curate_f0_note)
  }))
})

test_that("Curate sets the tone picker afresh for the clustered data", {
  env <- ic_env()
  up <- ic_data()
  cl <- shiny::reactiveVal(NULL)
  b  <- ic_bind(env, "curate_ui", dataset_in = shiny::reactiveVal(up),
                normalised_data = shiny::reactiveVal(NULL),
                curated_data = shiny::reactiveVal(NULL), inspect_result = shiny::reactiveVal(NULL),
                cluster_data = cl, curated_norm_data = shiny::reactiveVal(NULL),
                start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
                data_version = shiny::reactiveVal(0L), start_settings = NULL)
  ic_quiet(shiny::testServer(b$server, {
    expect_identical(ic_selected(output$ui_curate, "curate_tone_var"), "tone")
    session$setInputs(curate_tone_var = "tone")
    session$setInputs(curate_tone_var = "speaker")   # the user's pick
    expect_identical(picks$user[["curate_tone_var"]], "speaker")
    cl(cbind(up, cluster = ifelse(up$tone == "T1", "1", "2")))
    session$flushReact()
    expect_identical(ic_selected(output$ui_curate, "curate_tone_var"), "cluster")
  }))
})

test_that("Curate keeps its relabels and exclusions when the rows change, not on a new upload", {
  env <- ic_env()
  up <- ic_data()
  ds <- shiny::reactiveVal(up)
  cd <- shiny::reactiveVal(NULL)
  dv <- shiny::reactiveVal(0L)
  b  <- ic_bind(env, "curate_ui", dataset_in = ds, normalised_data = shiny::reactiveVal(NULL),
                curated_data = cd, inspect_result = shiny::reactiveVal(NULL),
                cluster_data = shiny::reactiveVal(NULL), curated_norm_data = shiny::reactiveVal(NULL),
                start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0_praat")),
                data_version = dv, start_settings = NULL)
  ic_quiet(shiny::testServer(b$server, {
    session$setInputs(curate_token_var = "token", curate_tone_var = "tone",
                      curate_speaker_var = "speaker", curate_f0_var = "f0_praat",
                      curate_item_var = "__none__")
    Sys.sleep(0.3); session$elapse(300); session$flushReact()
    rv_relabel(c(S1_T1_1 = "T9"))
    rv_exclude("S2_T2_3")
    session$flushReact()
    expect_identical(nrow(cd()), nrow(up) - 12L)
    expect_identical(unique(cd()$tone[cd()$token == "S1_T1_1"]), "T9")

    # The Start page removes the rows without f0 in f0_praat
    ds(up[!is.na(up$f0_praat), ])
    dv(1L)
    session$flushReact()
    expect_identical(rv_relabel(), c(S1_T1_1 = "T9"))
    expect_identical(rv_exclude(), "S2_T2_3")
    expect_identical(nrow(cd()), nrow(up) - 48L - 10L)
    expect_identical(unique(cd()$tone[cd()$token == "S1_T1_1"]), "T9")

    # A new upload starts a new curation
    session$setInputs(uploadfile = list(name = "other.csv", datapath = tempfile()))
    expect_length(rv_relabel(), 0)
    expect_length(rv_exclude(), 0)
  }))
})

test_that("the two modules still work without the Start-page arguments", {
  env <- ic_env()
  d  <- ic_data()
  ds <- shiny::reactiveVal(d)
  ir <- shiny::reactiveVal(NULL)
  srv <- function(input, output, session) {
    env$inspect_ui(input, output, session, ds, ir)
    env$curate_ui(input, output, session, ds, curated_data = shiny::reactiveVal(NULL),
                  inspect_result = ir)
  }
  ic_quiet(shiny::testServer(srv, {
    expect_identical(ic_selected(output$ui_inspect, "inspect_token_var"), "token")
    expect_identical(ic_selected(output$ui_inspect, "inspect_f0_var"), "f0")
    expect_identical(ic_selected(output$ui_curate, "curate_token_var"), "token")
    expect_null(output$inspect_f0_note)
    ir(data.frame(x = 1))
    session$setInputs(uploadfile = list(name = "other.csv", datapath = tempfile()))
    expect_null(ir())
  }))
})
