# GAMM tab (inst/app/ui/gamm_ui.R) and the Start page (Part C): the token
# and f0 menus default to the Start columns and keep the user's own pick,
# a data_version bump clears the fit, the f0 menu notes a column that still
# has rows without f0, and the R code starts with the Start-page steps.

# The module, bound to the reactives the server passes it, with global.R's
# helpers and shiny's exports in reach. updateSelectInput() and
# showNotification() record what they are given.
gamm_c_server <- function(data, start_cols = NULL, data_version = NULL, start_settings = NULL,
                          normalised = NULL) {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  sh <- new.env(parent = globalenv())
  for (nm in getNamespaceExports("shiny")) assign(nm, getExportedValue("shiny", nm), envir = sh)
  helpers <- new.env(parent = sh)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, helpers)
  sys.source(file.path(app, "ui", "gamm_ui.R"), helpers, keep.source = FALSE)
  rec <- new.env()
  rec$updates <- list()
  rec$toasts  <- list()
  data_rv <- shiny::reactiveVal(data)
  norm_rv <- shiny::reactiveVal(normalised)
  pred_rv <- shiny::reactiveVal(NULL)
  args <- list2env(list(
    dataset = data_rv, normalised_data = norm_rv, gamm_pred_data = pred_rv,
    curated_data = NULL, cluster_data = NULL, curated_norm_data = NULL,
    start_cols = start_cols, data_version = data_version, start_settings = start_settings,
    updateSelectInput = function(session, inputId, label = NULL, choices = NULL,
                                 selected = NULL, ...) {
      rec$updates[[length(rec$updates) + 1L]] <- list(id = inputId, selected = selected)
      invisible(NULL)
    },
    showNotification = function(ui, ..., id = NULL, type = "default", duration = 5) {
      rec$toasts[[length(rec$toasts) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                    id = id, type = type)
      invisible(id)
    },
    removeNotification = function(id, ...) invisible(id)), parent = helpers)
  bound <- function(input, output, session) NULL
  body(bound) <- body(helpers$gamm_ui)
  environment(bound) <- args
  list(server = bound, rec = rec, data = data_rv, normalised = norm_rv, pred = pred_rv,
       helpers = helpers)
}

# 24 tokens of 11 frames (10 ms): the tracker's f0 has no f0 at both edges,
# f0_corrected and f0_smooth have f0 on every row. utt is another token id.
gamm_c_data <- function() {
  set.seed(7)
  g <- expand.grid(speaker = c("S1", "S2", "S3"), tone = c("T1", "T2"), item = c("w1", "w2"),
                   rep = 1:2, stringsAsFactors = FALSE)
  g$token <- paste(g$speaker, g$tone, g$item, g$rep, sep = "_")
  do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    t   <- round(0.005 + 0.01 * (0:10), 3)
    f0c <- (if (g$tone[i] == "T1") 220 else 160) +
      25 * sin(pi * seq(0, 1, length.out = 11)) + stats::rnorm(11, sd = 3)
    raw <- f0c
    raw[c(1, 11)] <- NA
    data.frame(token = g$token[i], utt = paste0("u", i), speaker = g$speaker[i],
               tone = g$tone[i], item = g$item[i], time = t, f0 = raw,
               f0_corrected = f0c, f0_smooth = f0c, stringsAsFactors = FALSE)
  }))
}

gamm_c_html <- function(x) paste(as.character(x$html), collapse = " ")
# The option a <select> shows as selected, from rendered HTML.
gamm_c_selected <- function(html, id) {
  sel <- regmatches(html, regexpr(sprintf('(?s)<select id="%s"[^>]*>.*?</select>', id), html, perl = TRUE))
  if (!length(sel)) return(NA_character_)
  m <- regmatches(sel, regexec('<option value="([^"]*)" selected>', sel))[[1]]
  if (length(m) == 2) m[2] else NA_character_
}
# The text of the "Show R code" block.
gamm_c_code <- function(x) {
  h <- gamm_c_html(x)
  h <- sub("(?s).*<code>", "", h, perl = TRUE)
  h <- sub("(?s)</code>.*", "", h, perl = TRUE)
  for (p in list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&#39;", "'"), c("&amp;", "&")))
    h <- gsub(p[1], p[2], h, fixed = TRUE)
  h
}

gamm_c_fit_inputs <- list(
  gamm_dataset = "uploaded", gamm_token_var = "token", gamm_f0_var = "f0_corrected",
  gamm_time_var = "time", gamm_speaker_var = "speaker", gamm_tone_var = "tone",
  gamm_item_var = "item", gamm_dur_var = "none", gamm_k = 5, gamm_bs = "tp",
  gamm_smooth_type = "separate", gamm_ri_speaker = TRUE, gamm_ri_item = TRUE,
  gamm_rs_type = "none", gamm_ar1 = FALSE)

test_that("GAMM: the token and f0 menus default to the Start columns; normalised f0 keeps priority", {
  d  <- gamm_c_data()
  sc <- shiny::reactiveVal(list(token = "utt", time = "time", f0 = "f0_corrected"))
  srv <- gamm_c_server(d, start_cols = shiny::reactive(sc()))
  shiny::testServer(srv$server, {
    html <- gamm_c_html(output$ui_gamm)
    expect_identical(gamm_c_selected(html, "gamm_token_var"), "utt")
    expect_identical(gamm_c_selected(html, "gamm_f0_var"), "f0_corrected")
    # Time keeps its own default
    expect_identical(gamm_c_selected(html, "gamm_time_var"), "time")
    expect_match(html, 'id="gamm_f0_note"', fixed = TRUE)

    # Normalised data: its f0_st comes first, as before
    nd <- d
    nd$f0_st <- 12 * log2(nd$f0_corrected / 200)
    srv$normalised(nd)
    session$setInputs(gamm_dataset = "normalised")
    html <- gamm_c_html(output$ui_gamm)
    expect_identical(gamm_c_selected(html, "gamm_f0_var"), "f0_st")
    expect_identical(gamm_c_selected(html, "gamm_token_var"), "utt")

    # A Start token column the data does not have falls back to the guess
    sc(list(token = "segment_id", time = "time", f0 = "f0_corrected"))
    session$setInputs(gamm_dataset = "uploaded")
    html <- gamm_c_html(output$ui_gamm)
    expect_identical(gamm_c_selected(html, "gamm_token_var"), "token")
    expect_identical(gamm_c_selected(html, "gamm_f0_var"), "f0_corrected")
  })

  # Without start_cols (calls that leave it out) the menus guess as before
  srv0 <- gamm_c_server(d)
  shiny::testServer(srv0$server, {
    html <- gamm_c_html(output$ui_gamm)
    expect_identical(gamm_c_selected(html, "gamm_token_var"), "token")
    expect_identical(gamm_c_selected(html, "gamm_f0_var"), "f0")
    session$setInputs(gamm_f0_var = "f0")
    expect_null(output$gamm_f0_note)
  })
})

test_that("GAMM: the f0 menu notes another column that still has rows without f0", {
  d  <- gamm_c_data()
  sc <- shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0_corrected"))
  srv <- gamm_c_server(d, start_cols = shiny::reactive(sc()))
  shiny::testServer(srv$server, {
    session$setInputs(gamm_dataset = "uploaded", gamm_f0_var = "f0_corrected")
    expect_null(output$gamm_f0_note)
    session$setInputs(gamm_f0_var = "f0")
    note <- gamm_c_html(output$gamm_f0_note)
    expect_match(note, "f0 has 48 rows without f0. The Start page screens f0_corrected.", fixed = TRUE)
    # Another column with f0 on every row needs no note
    session$setInputs(gamm_f0_var = "f0_smooth")
    expect_null(output$gamm_f0_note)
  })
})

test_that("GAMM: the user's own pick stays when the Start columns change; untouched menus follow", {
  d  <- gamm_c_data()
  sc <- shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0_corrected"))
  srv <- gamm_c_server(d, start_cols = shiny::reactive(sc()))
  rec <- srv$rec
  shiny::testServer(srv$server, {
    html <- gamm_c_html(output$ui_gamm)
    # The menus send the values they were drawn with: not the user's picks
    session$setInputs(gamm_dataset = "uploaded", gamm_token_var = "token",
                      gamm_f0_var = "f0_corrected")
    expect_null(gamm_user_pick$token)
    expect_null(gamm_user_pick$f0)
    # The user picks the raw f0
    session$setInputs(gamm_f0_var = "f0")
    expect_identical(gamm_user_pick$f0, "f0")

    # The Start page now screens f0_smooth and groups by utt: only the
    # untouched token menu is updated
    rec$updates <- list()
    sc(list(token = "utt", time = "time", f0 = "f0_smooth"))
    session$flushReact()
    expect_identical(rec$updates, list(list(id = "gamm_token_var", selected = "utt")))
    session$setInputs(gamm_token_var = "utt")   # the menu sends its new value
    expect_null(gamm_user_pick$token)

    # Drawn again for new rows: the pick and the new token default
    srv$data(d[-1, ])
    session$flushReact()
    html <- gamm_c_html(output$ui_gamm)
    expect_identical(gamm_c_selected(html, "gamm_f0_var"), "f0")
    expect_identical(gamm_c_selected(html, "gamm_token_var"), "utt")
    # ... and on another dataset that still has the column
    nd <- d
    nd$f0_st <- 12 * log2(nd$f0_corrected / 200)
    srv$normalised(nd)
    session$setInputs(gamm_dataset = "normalised")
    expect_identical(gamm_c_selected(gamm_c_html(output$ui_gamm), "gamm_f0_var"), "f0")
    session$setInputs(gamm_dataset = "uploaded")

    # A pick the data no longer has gives way to the default
    srv$data(d[, setdiff(names(d), "f0")])
    session$flushReact()
    html <- gamm_c_html(output$ui_gamm)
    expect_identical(gamm_c_selected(html, "gamm_f0_var"), "f0_smooth")

    # Picking the default again: the menu follows the Start page once more
    session$setInputs(gamm_f0_var = "f0_smooth")
    expect_null(gamm_user_pick$f0)
    rec$updates <- list()
    sc(list(token = "utt", time = "time", f0 = "f0_corrected"))
    session$flushReact()
    expect_identical(rec$updates, list(list(id = "gamm_f0_var", selected = "f0_corrected")))
  })
})

test_that("GAMM: a data_version bump or a new upload clears the fit", {
  skip_if_not_installed("mgcv")
  d  <- gamm_c_data()
  dv <- shiny::reactiveVal(0L)
  srv <- gamm_c_server(d, data_version = dv)
  shiny::testServer(srv$server, {
    fit_state <- list(gamm_model, gamm_fit_obj, gamm_summary_data, gamm_formula_str,
                      gamm_plot_data)
    do.call(session$setInputs, gamm_c_fit_inputs)
    suppressWarnings(session$setInputs(gamm_button = 1))
    for (rv in fit_state) expect_false(is.null(rv()))
    expect_false(is.null(srv$pred()))
    gamm_diag_data(list(x = 1))
    gamm_rho_val(0.5)
    gamm_convergence_warning("w")

    dv(1L)
    session$flushReact()
    for (rv in c(fit_state, gamm_diag_data, gamm_rho_val, gamm_convergence_warning))
      expect_null(rv())
    expect_error(output$gamm_summary)

    # Fitted again, then a new upload
    suppressWarnings(session$setInputs(gamm_button = 2))
    expect_false(is.null(gamm_model()))
    session$setInputs(uploadfile = list(datapath = "new.csv", name = "new.csv"))
    expect_null(gamm_model())
  })

  # Without data_version (calls that leave it out) an upload still clears it
  srv0 <- gamm_c_server(d)
  shiny::testServer(srv0$server, {
    session$flushReact()   # the app's first flush, before any upload
    gamm_model("fit")
    session$setInputs(uploadfile = list(datapath = "new.csv", name = "new.csv"))
    expect_null(gamm_model())
  })
})

test_that("GAMM: the R code starts with the Start-page steps when start_settings is given", {
  d   <- gamm_c_data()
  scr <- screen_f0_rows(d, token = "token", time = "time", f0 = "f0")
  s   <- attr(scr, "summary")
  st  <- list(version = "0.9.0", zero_hz = c(f0 = 3L), dropped = list(rows = 11, tokens = 1),
              metadata = NULL, durations = list(),
              screen = list(token = "token", time = "time", f0 = "f0", unvoiced = NULL,
                            action = "remove", min_run = 1, rows = nrow(d),
                            without = s[["rows_without_f0"]], summary = s,
                            rows_after = s[["rows_after"]], gaps_marked = 0,
                            short_rows = s[["short_rows"]]))
  srv <- gamm_c_server(d, start_settings = shiny::reactive(st))
  shiny::testServer(srv$server, {
    do.call(session$setInputs, gamm_c_fit_inputs)
    session$setInputs(dataset_name = "f0_tracks", gamm_show_code = 1)
    code  <- gamm_c_code(output$gamm_r_code)
    start <- paste(srv$helpers$read_data_code("f0_tracks.csv", start = st), collapse = "\n")
    # The library lines, then the Start-page steps, then the tab's own steps
    expect_true(startsWith(code, paste0("library(dplyr)\nlibrary(mgcv)\nlibrary(ggplot2)\n\n",
                                        start, "\n\n")))
    expect_match(code, 'stopifnot(utils::packageVersion("shinytone") >= "0.9.0")', fixed = TRUE)
    expect_match(code, "dat <- dat[!(as.logical(dat$token_dropped) %in% TRUE), ]", fixed = TRUE)
    expect_match(code, 'dat <- shinytone::drop_rows_without_f0(dat, token = "token", time = "time", f0 = "f0")',
                 fixed = TRUE)
    expect_lt(regexpr("drop_rows_without_f0(", code, fixed = TRUE)[1],
              regexpr("time_norm", code, fixed = TRUE)[1])
  })

  # Without start_settings the code reads the file as before
  srv0 <- gamm_c_server(d)
  shiny::testServer(srv0$server, {
    do.call(session$setInputs, gamm_c_fit_inputs)
    session$setInputs(dataset_name = "f0_tracks", gamm_show_code = 1)
    code <- gamm_c_code(output$gamm_r_code)
    expect_true(startsWith(code, paste0("library(dplyr)\nlibrary(mgcv)\nlibrary(ggplot2)\n\n",
                                        paste(srv0$helpers$read_data_code("f0_tracks.csv"),
                                              collapse = "\n"),
                                        "\n\n")))
    expect_false(grepl("drop_rows_without_f0", code, fixed = TRUE))
    expect_false(grepl("stopifnot", code, fixed = TRUE))
  })
})

test_that("GAMM in the app's server: Start columns, data_version and the R code reach the tab", {
  s   <- start_server()
  app <- system.file("app", package = "shinytone")
  sys.source(file.path(app, "ui", "gamm_ui.R"), s$env, keep.source = FALSE)
  # The real module, which leaves its environment in cap$env for the test
  cap  <- new.env()
  real <- s$env$gamm_ui
  body(real) <- as.call(c(as.list(body(real)), quote(cap$env <- environment())))
  environment(real) <- list2env(list(cap = cap), parent = s$env)
  s$env$gamm_ui <- real
  f <- start_write_csv(start_padded_fixture())
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "padded.csv"), dataset_name = "padded")
    g <- cap$env
    expect_true(is.environment(g))
    html <- gamm_c_html(output$ui_gamm)
    expect_identical(gamm_c_selected(html, "gamm_token_var"), "token")
    expect_identical(gamm_c_selected(html, "gamm_f0_var"), "f0")

    session$setInputs(gamm_dataset = "uploaded", gamm_token_var = "token", gamm_f0_var = "f0",
                      gamm_time_var = "time", gamm_speaker_var = "speaker",
                      gamm_tone_var = "tone", gamm_item_var = "token", gamm_dur_var = "none",
                      gamm_k = 5, gamm_bs = "tp", gamm_smooth_type = "separate",
                      gamm_ri_speaker = FALSE, gamm_ri_item = TRUE, gamm_rs_type = "none",
                      gamm_ar1 = FALSE, gamm_show_code = 1)
    code <- gamm_c_code(output$gamm_r_code)
    expect_match(code, 'dat <- shinytone::drop_rows_without_f0(dat, token = "token", time = "time", f0 = "f0")',
                 fixed = TRUE)
    expect_match(code, "# shinytone::screen_f0_rows(", fixed = TRUE)
    expect_lt(regexpr("stopifnot(", code, fixed = TRUE)[1],
              regexpr("read_csv_utf8(", code, fixed = TRUE)[1])

    # Unticking removal changes the rows: data_version goes up and the fit
    # goes, and the code keeps the rows
    g$gamm_model("fit")
    g$gamm_summary_data(list(p_table = NULL))
    session$setInputs(screen_remove = FALSE)
    expect_identical(data_version(), 1L)
    expect_null(g$gamm_model())
    expect_null(g$gamm_summary_data())
    code <- gamm_c_code(output$gamm_r_code)
    expect_false(grepl("drop_rows_without_f0(", code, fixed = TRUE))
    expect_match(code, "stay (removal unticked)", fixed = TRUE)

    # Another Start f0 column: the untouched f0 menu follows it, and the note
    # says that f0 still has rows without f0
    session$setInputs(screen_f0 = "intensity")
    expect_identical(start_cols()$f0, "intensity")
    expect_identical(g$gamm_col_selected("f0", names(dataset())), "intensity")
    expect_match(gamm_c_html(output$gamm_f0_note), "The Start page screens intensity.", fixed = TRUE)
  })
})

# Disyllabic tokens, 10 ms frames, with a syllable tier (two parts per
# token) and a word tier (one part per token). Rows without f0 at both
# edges and at the syllable boundary. The 0-1 axes come from the package's
# builders, from the first f0 to the last.
gamm_c_disyllabic <- function() {
  set.seed(5)
  g <- expand.grid(speaker = c("S1", "S2"), tone = c("T1", "T2"), item = c("w1", "w2"),
                   rep = 1:2, stringsAsFactors = FALSE)
  d <- do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    t  <- round(0.005 + 0.01 * (0:19), 3)
    s1 <- t < 0.1
    f0 <- ifelse(s1, 200 - 300 * t, 230 + 50 * (t - 0.1)) + 20 * (g$tone[i] == "T2") +
      stats::rnorm(20, sd = 1)
    f0[c(1, 2, 10, 11, 20)] <- NA
    data.frame(token = paste(g$speaker[i], g$tone[i], g$item[i], g$rep[i], sep = "_"),
               time = t, f0 = f0, speaker = g$speaker[i], tone = g$tone[i], item = g$item[i],
               syllable = ifelse(s1, "a", "b"), syllable_start = ifelse(s1, 0, 0.1),
               syllable_end = ifelse(s1, 0.1, 0.2), syllable_i = ifelse(s1, 1L, 2L),
               word = "ab", word_start = 0, word_end = 0.2, word_i = 1L,
               stringsAsFactors = FALSE)
  }))
  d <- normalise_time_landmarks(d, "time", "syllable", f0 = "f0", token = "token")
  d <- normalise_time_landmarks(d, "time", "word", f0 = "f0", token = "token")
  normalise_time_token(d, "time", "token", f0 = "f0")
}

test_that("GAMM: the red warning and the in-sequence note count the parts of the tier", {
  d <- gamm_c_disyllabic()
  srv <- gamm_c_server(d)
  shiny::testServer(srv$server, {
    do.call(session$setInputs, modifyList(gamm_c_fit_inputs, list(gamm_f0_var = "f0")))
    # Two parts per token: syllable_tbound is the default Time (gap kept)
    expect_identical(gamm_c_selected(gamm_c_html(output$ui_gamm), "gamm_time_var"), "syllable_tbound")

    # syllable_t01 restarts in each syllable: the red warning, no blue note
    session$setInputs(gamm_time_var = "syllable_t01")
    warn <- gamm_c_html(output$gamm_multisyl_note)
    expect_match(warn, "runs from 0 to 1 <em>within</em> each syllable, from its first f0 to its last",
                 fixed = TRUE)
    expect_match(warn, "not a valid time axis for a whole-word model", fixed = TRUE)
    expect_match(warn, "<code>token_t01</code> (whole word, syllable ratio kept)", fixed = TRUE)
    expect_null(output$gamm_timenorm_note)

    # word_t01 has one part per token: a whole-token axis, so the blue note
    session$setInputs(gamm_time_var = "word_t01")
    expect_null(output$gamm_multisyl_note)
    note <- gamm_c_html(output$gamm_timenorm_note)
    expect_match(note, "<code>word_t01</code> is already on 0&ndash;1", fixed = TRUE)
    expect_match(note, "its name marks a 0&ndash;1 axis", fixed = TRUE)
    expect_match(note, "each word&rsquo;s first and last f0", fixed = TRUE)
    expect_match(note, "fit_gamm(..., time_normalised = \"no\")", fixed = TRUE)
    expect_null(output$gamm_track_edges)

    # The syllables in sequence: their own note, with the limit at the boundary
    session$setInputs(gamm_time_var = "syllable_tseq")
    expect_null(output$gamm_multisyl_note)
    expect_null(output$gamm_track_edges)
    note <- gamm_c_html(output$gamm_timenorm_note)
    expect_match(note, "<code>syllable_tseq</code> puts the syllables in sequence", fixed = TRUE)
    expect_match(note, "one smooth cannot follow an f0 jump there", fixed = TRUE)
    expect_match(note, "on <code>syllable_t01</code>", fixed = TRUE)
    expect_match(note, paste("To model the whole word, use <code>syllable_tbound</code> (in sequence, gap kept)",
                             "or <code>token_t01</code>"), fixed = TRUE)

    # The syllables at the TextGrid boundaries: also kept on its own scale,
    # and the gap is kept, so the f0-jump limit does not apply
    session$setInputs(gamm_time_var = "syllable_tbound")
    expect_null(output$gamm_multisyl_note)
    expect_null(output$gamm_track_edges)
    note <- gamm_c_html(output$gamm_timenorm_note)
    expect_match(note, paste("<code>syllable_tbound</code> puts the syllables in sequence, gap kept:",
                             "0 is the word&rsquo;s first f0, the TextGrid boundaries sit at 1, 2"),
                 fixed = TRUE)
    expect_match(note, "The fit keeps this scale rather than rescaling each token to 0&ndash;1",
                 fixed = TRUE)
    expect_match(note, "so one smooth can follow an f0 jump across it.", fixed = TRUE)
    expect_no_match(note, "cannot follow", fixed = TRUE)
    # word_tbound has one part per token: the 0-1 note, as for word_tseq
    session$setInputs(gamm_time_var = "word_tbound")
    expect_null(output$gamm_multisyl_note)
    note <- gamm_c_html(output$gamm_timenorm_note)
    expect_no_match(note, "puts the syllables in sequence", fixed = TRUE)
    expect_match(note, "<code>word_tbound</code> looks already on 0&ndash;1", fixed = TRUE)

    # word_tseq has one part per token: nothing to put in sequence, so not
    # that note but the 0-1 note, by its values
    session$setInputs(gamm_time_var = "word_tseq")
    expect_null(output$gamm_multisyl_note)
    note <- gamm_c_html(output$gamm_timenorm_note)
    expect_no_match(note, "puts the syllables in sequence", fixed = TRUE)
    expect_match(note, "<code>word_tseq</code> looks already on 0&ndash;1", fixed = TRUE)

    # The guide: the limit at the boundary in the "Raise k" tip, and the
    # whole word as the other route
    guide <- gamm_c_html(output$gamm_guide)
    expect_match(guide, "no <em>k</em> lets one smooth follow an f0 jump there", fixed = TRUE)
    expect_match(guide, paste("For such words, model the whole word on <code>&lt;tier&gt;_tbound</code>,",
                              "which keeps the gap and still lines the boundaries up, or on",
                              "<code>token_t01</code>"), fixed = TRUE)
    # The Time item: both in-sequence axes keep their scale; the tie is on _tseq
    txt <- gsub("\\s+", " ", gsub("<[^>]*>", "", guide))
    expect_match(txt, paste("A &lt;tier&gt;_tseq or &lt;tier&gt;_tbound column is kept on its own",
                            "0-to-n scale. On a &lt;tier&gt;_tseq column, one syllable's last f0"),
                 fixed = TRUE)
    expect_match(guide, "(<code>&lt;tier&gt;_tbound</code>) or on the whole word", fixed = TRUE)
    expect_match(guide, "Or model each syllable as its own token on <code>&lt;syllable_tier&gt;_t01</code>",
                 fixed = TRUE)
    # One tip holds both routes; no second bullet repeats token_t01
    expect_no_match(guide, "For the whole word with its syllable ratio kept", fixed = TRUE)
    # The red warning names the in-sequence columns, not a placeholder
    session$setInputs(gamm_time_var = "syllable_t01")
    warn <- gamm_c_html(output$gamm_multisyl_note)
    expect_match(warn, paste("use <code>syllable_tseq</code> (syllables in sequence),",
                             "<code>syllable_tbound</code> (in sequence, gap kept) or <code>token_t01</code>"),
                 fixed = TRUE)
    expect_no_match(warn, "&hellip;", fixed = TRUE)
    expect_match(guide, "so keep each token", fixed = TRUE)
    expect_no_match(guide, "fit one smooth across the whole word", fixed = TRUE)
  })

  # Only the word tier: the Time menu picks word_t01, not word_tseq
  w <- d[, setdiff(names(d), c(grep("^syl", names(d), value = TRUE), "token_t01"))]
  shiny::testServer(gamm_c_server(w)$server, {
    expect_identical(gamm_c_selected(gamm_c_html(output$ui_gamm), "gamm_time_var"), "word_t01")
  })
})

test_that("GAMM: a word_t01 whose f0 misses 0 or 1 gets the notice, judged per word", {
  d <- gamm_c_disyllabic()
  # word_t01 as shinytone 1.0.0 built it: from the interval's start boundary
  # to its end boundary
  d$word_t01 <- (d$time - d$word_start) / (d$word_end - d$word_start)
  shiny::testServer(gamm_c_server(d)$server, {
    do.call(session$setInputs, modifyList(gamm_c_fit_inputs,
                                          list(gamm_f0_var = "f0", gamm_time_var = "word_t01")))
    edge <- gamm_c_html(output$gamm_track_edges)
    expect_match(edge, "in all 16 <code>word</code> parts the f0 does not", fixed = TRUE)
    expect_match(edge, "Make <code>word_t01</code> again in <strong>Normalise</strong>", fixed = TRUE)
    expect_match(gamm_c_html(output$gamm_timenorm_note), "(see the notice above)", fixed = TRUE)
  })
})
