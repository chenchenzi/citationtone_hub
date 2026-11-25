# Part C in the Visualise, Normalise and Cluster tabs: the token and f0 menus
# default to the Start page's columns (D8), a pick of the user's own is kept,
# a note shows when the chosen f0 is not the screened column, data_version
# clears the tab's results, and Visualise's R code repeats the Start steps.

# global.R's definitions and the three modules, with shiny's and ggplot2's
# exports in reach, as in the running app.
cvnc_env <- function() {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  pkgs <- new.env(parent = globalenv())
  for (p in c("shiny", "ggplot2"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  for (f in c("visualise_ui.R", "normalise_ui.R", "cluster_ui.R"))
    sys.source(file.path(app, "ui", f), env, keep.source = FALSE)
  env
}

# A module's body bound to its extra arguments, for shiny::testServer().
cvnc_bind <- function(env, name, ...) {
  bound <- function(input, output, session) NULL
  body(bound) <- body(env[[name]])
  environment(bound) <- list2env(list(...), parent = env)
  bound
}

# The value a rendered selectInput selects.
cvnc_selected <- function(ui, id) {
  html <- paste(as.character(ui$html), collapse = " ")
  sel <- regmatches(html, regexpr(sprintf('(?s)<select id="%s"[^>]*>.*?</select>', id), html, perl = TRUE))
  if (!length(sel)) return(NA_character_)
  opt <- regmatches(sel, regexpr('<option value="[^"]*" selected>', sel))
  if (!length(opt)) return(NA_character_)
  sub('<option value="([^"]*)" selected>', "\\1", opt)
}
cvnc_text <- function(ui) paste(as.character(ui$html), collapse = " ")
# The R code a "Show R code" panel shows, as lines.
cvnc_code <- function(ui) {
  html <- paste(as.character(ui$html), collapse = "")
  code <- regmatches(html, regexpr("(?s)<code>.*</code>", html, perl = TRUE))
  code <- sub("^<code>", "", sub("</code>$", "", code))
  for (p in list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&#39;", "'"),
                 c("&amp;", "&"))) code <- gsub(p[1], p[2], code, fixed = TRUE)
  strsplit(code, "\n", fixed = TRUE)[[1]]
}

# Long-format f0 with three Hz columns: f0 (3 rows without f0), f0_corrected
# and f0_alt (every row has f0). Two tones, a level and a rising one.
cvnc_data <- function(n_tok = 12) {
  set.seed(1)
  d <- do.call(rbind, lapply(seq_len(n_tok), function(i) {
    t    <- seq(0, 0.09, by = 0.01)
    tone <- c("T1", "T2")[(i %% 2) + 1]
    f0   <- (if (tone == "T1") 200 + 0 * t else 170 + 500 * t) + stats::rnorm(10, sd = 1)
    data.frame(token = sprintf("tok%02d", i), item = sprintf("w%02d", i), time = t,
               f0 = f0, f0_corrected = f0 + 1, f0_alt = f0 + 2,
               speaker = if (i <= n_tok / 2) "S1" else "S2", tone = tone,
               stringsAsFactors = FALSE)
  }))
  d$f0[c(1, 11, 25)] <- NA
  d
}

rv <- function(x = NULL) shiny::reactiveVal(x)

test_that("the token and f0 menus default to the Start page's columns", {
  env <- cvnc_env()
  d   <- cvnc_data()
  sc  <- rv(NULL)                                   # before the Start columns are known
  norm <- cvnc_bind(env, "normalised_ui", dataset = rv(d), normalised_data = rv(),
                    curated_data = NULL, curated_norm_data = NULL, start_cols = sc)
  shiny::testServer(norm, {
    expect_identical(cvnc_selected(output$ui_normalise, "norm_f0_var"), "f0")
    sc(list(token = "item", time = "time", f0 = "f0_corrected"))
    session$flushReact()
    expect_identical(cvnc_selected(output$ui_normalise, "norm_f0_var"), "f0_corrected")
    expect_identical(cvnc_selected(output$norm_time_landmark_ui, "norm_time_token_var"), "item")
    # A Start column the data does not have: the usual guess
    sc(list(token = "speaker_id", time = "time", f0 = "pitch"))
    session$flushReact()
    expect_identical(cvnc_selected(output$ui_normalise, "norm_f0_var"), "f0")
    expect_identical(cvnc_selected(output$norm_time_landmark_ui, "norm_time_token_var"), "token")
  })

  sc <- rv(list(token = "item", time = "time", f0 = "f0_corrected"))
  vis <- cvnc_bind(env, "visualise_ui", dataset = rv(d), normalised_data = rv(),
                   curated_norm_data = NULL, start_cols = sc)
  shiny::testServer(vis, {
    ui <- output$ui_visualise
    expect_identical(cvnc_selected(ui, "y_var"), "f0_corrected")
    expect_identical(cvnc_selected(ui, "x_var"), "time")           # time: as before
    # The Start page now screens f0_alt: the untouched menu follows
    sc(list(token = "item", time = "time", f0 = "f0_alt"))
    session$flushReact()
    expect_identical(cvnc_selected(output$ui_visualise, "y_var"), "f0_alt")
  })

  cl <- cvnc_bind(env, "cluster_ui", dataset = rv(d), normalised_data = rv(),
                  curated_data = NULL, cluster_data = rv(), start_cols = sc)
  shiny::testServer(cl, {
    ui <- output$ui_cluster
    expect_identical(cvnc_selected(ui, "cluster_token_var"), "item")
    expect_identical(cvnc_selected(ui, "cluster_f0_var"), "f0_alt")
    expect_identical(cvnc_selected(ui, "cluster_time_var"), "time")
  })

  # Normalised columns keep their priority over the screened column, except
  # in Normalise, which takes Hz.
  dn <- d
  dn$f0_st <- 12 * log2(dn$f0_corrected / 190)
  sc <- rv(list(token = "token", time = "time", f0 = "f0_corrected"))
  vis <- cvnc_bind(env, "visualise_ui", dataset = rv(dn), normalised_data = rv(),
                   curated_norm_data = NULL, start_cols = sc)
  shiny::testServer(vis, expect_identical(cvnc_selected(output$ui_visualise, "y_var"), "f0_st"))
  cl <- cvnc_bind(env, "cluster_ui", dataset = rv(dn), normalised_data = rv(),
                  curated_data = NULL, cluster_data = rv(), start_cols = sc)
  shiny::testServer(cl, expect_identical(cvnc_selected(output$ui_cluster, "cluster_f0_var"), "f0_st"))
  norm <- cvnc_bind(env, "normalised_ui", dataset = rv(dn), normalised_data = rv(),
                    curated_data = NULL, curated_norm_data = NULL, start_cols = sc)
  shiny::testServer(norm,
    expect_identical(cvnc_selected(output$ui_normalise, "norm_f0_var"), "f0_corrected"))
})

test_that("the modules still run when the Start-page arguments are left out", {
  env <- cvnc_env()
  d   <- cvnc_data()
  vis <- cvnc_bind(env, "visualise_ui", dataset = rv(d), normalised_data = rv(),
                   curated_norm_data = NULL)
  shiny::testServer(vis, {
    expect_identical(cvnc_selected(output$ui_visualise, "y_var"), "f0")
    session$setInputs(y_var = "f0")
    expect_null(output$vis_f0_note)
  })
  cl <- cvnc_bind(env, "cluster_ui", dataset = rv(d), normalised_data = rv(),
                  curated_data = NULL, cluster_data = rv())
  shiny::testServer(cl, {
    expect_identical(cvnc_selected(output$ui_cluster, "cluster_token_var"), "token")
    expect_identical(cvnc_selected(output$ui_cluster, "cluster_f0_var"), "f0")
  })
  norm <- cvnc_bind(env, "normalised_ui", dataset = rv(d), normalised_data = rv(),
                    curated_data = NULL, curated_norm_data = NULL)
  shiny::testServer(norm, expect_identical(cvnc_selected(output$ui_normalise, "norm_f0_var"), "f0"))
})

test_that("a pick of the user's own is kept when the Start columns change", {
  env <- cvnc_env()
  d   <- cvnc_data()
  sc  <- rv(list(token = "token", time = "time", f0 = "f0_corrected"))

  norm <- cvnc_bind(env, "normalised_ui", dataset = rv(d), normalised_data = rv(),
                    curated_data = NULL, curated_norm_data = NULL, start_cols = sc)
  shiny::testServer(norm, {
    expect_identical(cvnc_selected(output$ui_normalise, "norm_f0_var"), "f0_corrected")
    session$setInputs(norm_f0_var = "f0")             # the user's own pick
    expect_match(cvnc_text(output$norm_f0_note),
                 "f0 has 3 rows without f0. The Start page screens f0_corrected.", fixed = TRUE)
    sc(list(token = "token", time = "time", f0 = "f0_alt"))
    session$flushReact()
    expect_identical(cvnc_selected(output$ui_normalise, "norm_f0_var"), "f0")
    # Back on the default: the menu follows the Start page again
    session$setInputs(norm_f0_var = "f0_alt")
    expect_null(output$norm_f0_note)                   # the screened column
    sc(list(token = "token", time = "time", f0 = "f0_corrected"))
    session$flushReact()
    expect_identical(cvnc_selected(output$ui_normalise, "norm_f0_var"), "f0_corrected")
  })

  nd <- rv(NULL)
  vis <- cvnc_bind(env, "visualise_ui", dataset = rv(d), normalised_data = nd,
                   curated_norm_data = NULL, start_cols = sc)
  shiny::testServer(vis, {
    expect_identical(cvnc_selected(output$ui_visualise, "y_var"), "f0_corrected")
    session$setInputs(y_var = "f0")
    expect_match(cvnc_text(output$vis_f0_note), "f0 has 3 rows without f0.", fixed = TRUE)
    sc(list(token = "token", time = "time", f0 = "f0_alt"))
    session$flushReact()
    expect_identical(cvnc_selected(output$ui_visualise, "y_var"), "f0")
    # Another data source starts the menus on their defaults again
    dn <- d
    dn$f0_st <- 12 * log2(dn$f0_corrected / 190)
    nd(dn)
    session$setInputs(vis_data_source = "normalised")
    expect_identical(cvnc_selected(output$ui_visualise, "y_var"), "f0_st")
  })

  cl <- cvnc_bind(env, "cluster_ui", dataset = rv(d), normalised_data = rv(),
                  curated_data = NULL, cluster_data = rv(), start_cols = sc)
  shiny::testServer(cl, {
    ui <- output$ui_cluster
    expect_identical(cvnc_selected(ui, "cluster_f0_var"), "f0_alt")
    session$setInputs(cluster_f0_var = "f0")
    expect_match(cvnc_text(output$cluster_f0_note),
                 "f0 has 3 rows without f0. The Start page screens f0_alt.", fixed = TRUE)
    sc(list(token = "item", time = "time", f0 = "f0_corrected"))
    session$flushReact()
    ui <- output$ui_cluster
    expect_identical(cvnc_selected(ui, "cluster_f0_var"), "f0")         # kept
    expect_identical(cvnc_selected(ui, "cluster_token_var"), "item")     # untouched: follows
  })
})

test_that("a data_version bump clears Normalise's table and Cluster's results", {
  env <- cvnc_env()
  d   <- cvnc_data(n_tok = 16)
  sc  <- rv(list(token = "token", time = "time", f0 = "f0_corrected"))
  dv  <- rv(0L)

  norm <- cvnc_bind(env, "normalised_ui", dataset = rv(d), normalised_data = rv(),
                    curated_data = NULL, curated_norm_data = NULL,
                    start_cols = sc, data_version = dv)
  shiny::testServer(norm, {
    session$setInputs(norm_f0_var = "f0_corrected", norm_speaker_var = "speaker",
                      norm_tone_var = "tone", mean_calc_method = "simple",
                      normalisation_method = "semitone", sd_correction = TRUE,
                      norm_time_set = "__none__")
    session$setInputs(normalise_button = 1)
    expect_true("f0_st" %in% names(norm_display()))
    dv(1L)
    session$flushReact()
    expect_null(norm_display())
    # A new upload still clears it
    session$setInputs(normalise_button = 2)
    expect_false(is.null(norm_display()))
    session$setInputs(uploadfile = list(name = "new.csv", datapath = "new.csv"))
    expect_null(norm_display())
  })
  # Without data_version, an upload clears it as before
  norm <- cvnc_bind(env, "normalised_ui", dataset = rv(d), normalised_data = rv(),
                    curated_data = NULL, curated_norm_data = NULL)
  shiny::testServer(norm, {
    session$setInputs(norm_f0_var = "f0_corrected", norm_speaker_var = "speaker",
                      norm_tone_var = "tone", mean_calc_method = "simple",
                      normalisation_method = "semitone", norm_time_set = "__none__")
    session$setInputs(normalise_button = 1)
    expect_false(is.null(norm_display()))
    session$setInputs(uploadfile = list(name = "new.csv", datapath = "new.csv"))
    expect_null(norm_display())
  })

  dv <- rv(0L)
  cl <- cvnc_bind(env, "cluster_ui", dataset = rv(d), normalised_data = rv(),
                  curated_data = NULL, cluster_data = rv(),
                  start_cols = sc, data_version = dv)
  shiny::testServer(cl, {
    session$setInputs(cluster_dataset = "uploaded", cluster_token_var = "token",
                      cluster_f0_var = "f0_corrected", cluster_time_var = "time",
                      cluster_speaker_var = "speaker", cluster_tone_var = "__none__",
                      cluster_features = "points", cluster_degree = 4, cluster_npoints = 10,
                      cluster_register = "level", cluster_method = "kmeans",
                      cluster_linkage = "ward.D2", cluster_kmode = "manual", cluster_k = 2,
                      cluster_kmax = 4)
    session$setInputs(cluster_run = 1)
    expect_identical(cl_trigger(), 1)
    expect_identical(cl_run()$k, 2L)
    dv(1L)
    session$flushReact()
    expect_null(cl_trigger())
    expect_error(cl_run(), class = "shiny.silent.error")
    # Run again: the results come back
    session$setInputs(cluster_run = 2)
    expect_identical(cl_run()$k, 2L)
  })
})

test_that("Visualise's R code starts with the Start page's steps", {
  env <- cvnc_env()
  raw <- cvnc_data()
  f   <- tempfile(fileext = ".csv")
  utils::write.csv(raw, f, row.names = FALSE, na = "")
  on.exit(unlink(f))
  # What the Start page would do with removal ticked on f0
  rd  <- zero_hz_to_na(read_csv_utf8(f, stringsAsFactors = FALSE,
                                     na.strings = c("NA", "--undefined--")))
  s   <- attr(screen_f0_rows(rd, token = "token", time = "time", f0 = "f0"), "summary")
  ds  <- suppressMessages(drop_rows_without_f0(rd, token = "token", time = "time", f0 = "f0"))
  settings <- list(version = "0.0.1", zero_hz = attr(rd, "zero_hz"), dropped = NULL,
                   metadata = NULL, durations = list(),
                   screen = list(token = "token", time = "time", f0 = "f0", unvoiced = NULL,
                                 action = "remove", min_run = 1, rows = nrow(rd),
                                 without = s[["rows_without_f0"]], summary = s,
                                 rows_after = nrow(ds),
                                 gaps_marked = s[["gaps_time_only"]] + s[["gaps_run"]],
                                 short_rows = s[["short_rows"]]))
  code_of <- function(srv) {
    got <- new.env()
    shiny::testServer(srv, {
      session$setInputs(dataset_name = sub("\\.csv$", "", f), x_var = "time", y_var = "f0",
                        tone_var = "tone", speaker_var = "speaker",
                        convert_tone_to_factor = FALSE, plot_facet = "none",
                        plot_button = 1, show_code_button = 1)
      got$code <- cvnc_code(output$r_code_output)
    })
    got$code
  }

  # Without start_settings: the read and zero_hz_to_na() only, as before
  plain <- code_of(cvnc_bind(env, "visualise_ui", dataset = rv(ds), normalised_data = rv(),
                             curated_norm_data = NULL))
  expect_identical(plain[1:3], c("library(tidyverse)", "",
                                 "# Read your data as the app does (adjust the path to where the file is):"))
  expect_false(any(grepl("drop_rows_without_f0|stopifnot", plain)))

  code <- code_of(cvnc_bind(env, "visualise_ui", dataset = rv(ds), normalised_data = rv(),
                            curated_norm_data = NULL, start_settings = shiny::reactive(settings)))
  expect_identical(code[1:2], c("library(tidyverse)", ""))
  expect_match(code[4], 'stopifnot(utils::packageVersion("shinytone") >= "0.0.1")', fixed = TRUE)
  at_read <- grep("read_csv_utf8", code, fixed = TRUE)
  at_drop <- grep('dat <- shinytone::drop_rows_without_f0(dat, token = "token", time = "time", f0 = "f0")',
                  code, fixed = TRUE)
  at_plot <- grep("# Create the plot", code, fixed = TRUE)
  expect_length(at_drop, 1)
  expect_true(at_read < at_drop && at_drop < at_plot)
  expect_true(any(grepl("# shinytone::screen_f0_rows(dat", code, fixed = TRUE)))

  # The data part of the code gives the rows the plot was drawn from
  run <- new.env()
  eval(parse(text = code[3:(at_plot - 1)]), run)
  strip <- function(x) {
    x <- as.data.frame(x)
    attributes(x)[setdiff(names(attributes(x)), c("names", "row.names", "class"))] <- NULL
    rownames(x) <- NULL
    x
  }
  expect_identical(nrow(run$dat), nrow(ds))
  expect_equal(strip(run$dat), strip(ds))
})

test_that("in the app's server the three tabs follow the Start page and clear on its changes", {
  skip_if_not_installed("DT")
  s <- start_server()
  # The real modules in place of the stubs
  app <- system.file("app", package = "shinytone")
  for (f in c("visualise_ui.R", "normalise_ui.R", "cluster_ui.R"))
    sys.source(file.path(app, "ui", f), s$env, keep.source = FALSE)
  for (nm in getNamespaceExports("ggplot2"))
    if (!exists(nm, envir = s$env, inherits = FALSE))
      assign(nm, getExportedValue("ggplot2", nm), envir = s$env)
  # F0 Correction's columns: the Start page screens f0_corrected. The user
  # filled S1_a's gap (its 6th row) and deleted a frame near its end.
  pf <- start_padded_fixture()
  pf$f0_corrected <- pf$f0
  pf$f0_corrected[which(pf$token == "S1_a")[6]] <- 205
  pf$f0_corrected[pf$token == "S1_a" & pf$time > 0.075] <- NA
  pf$edited <- FALSE
  f <- start_write_csv(pf)
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "corrected.csv"))
    expect_identical(start_cols()$f0, "f0_corrected")
    expect_identical(cvnc_selected(output$ui_normalise, "norm_f0_var"), "f0_corrected")
    expect_identical(cvnc_selected(output$ui_visualise, "y_var"), "f0_corrected")
    expect_identical(cvnc_selected(output$ui_cluster, "cluster_f0_var"), "f0_corrected")
    expect_identical(cvnc_selected(output$ui_cluster, "cluster_token_var"), "token")
    # f0 still has a row without f0 after the screen of f0_corrected (the
    # filled gap)
    session$setInputs(y_var = "f0")
    expect_match(cvnc_text(output$vis_f0_note),
                 "f0 has 1 row without f0. The Start page screens f0_corrected.", fixed = TRUE)

    session$setInputs(norm_f0_var = "f0_corrected", norm_speaker_var = "speaker",
                      norm_tone_var = "tone", mean_calc_method = "simple",
                      normalisation_method = "semitone", norm_time_set = "__none__")
    session$setInputs(normalise_button = 1)
    expect_false(is.null(normalised_data()))
    expect_no_error(output$normalised_data)
    session$setInputs(screen_remove = FALSE)                # a Start-page change
    expect_identical(data_version(), 1L)
    expect_null(normalised_data())
    expect_error(output$normalised_data)                     # the tab's table is cleared too

    # Visualise's R code repeats the Start page's steps
    session$setInputs(screen_remove = TRUE)
    expect_identical(data_version(), 2L)
    session$setInputs(dataset_name = sub("\\.csv$", "", f), x_var = "time", y_var = "f0_corrected",
                      tone_var = "tone", speaker_var = "speaker", convert_tone_to_factor = FALSE,
                      plot_facet = "none", plot_button = 1, show_code_button = 1)
    code <- cvnc_code(output$r_code_output)
    expect_match(code[4], "stopifnot(utils::packageVersion(", fixed = TRUE)
    expect_true(any(code == paste0('dat <- shinytone::drop_rows_without_f0(dat, token = "token", ',
                                   'time = "time", f0 = "f0_corrected")')))
    # Run up to the plot, it gives the rows the tabs read
    run <- new.env()
    eval(parse(text = code[3:(grep("# Create the plot", code, fixed = TRUE) - 1)]), run)
    strip <- function(x) {
      x <- as.data.frame(x)
      attributes(x)[setdiff(names(attributes(x)), c("names", "row.names", "class"))] <- NULL
      rownames(x) <- NULL
      x
    }
    expect_equal(strip(run$dat), strip(dataset()))
  })
})

test_that("Cluster shows the averaged-ties message as a warning toast", {
  env <- cvnc_env()
  rec <- new.env()
  rec$toasts <- list()
  env$showNotification <- function(ui, ..., id = NULL, type = "default", duration = 5) {
    rec$toasts[[length(rec$toasts) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                  type = type)
    invisible(id)
  }
  # 12 words of two syllables; syllable_t01 runs 0 to 1 in each, so with
  # token = word two rows share each time
  set.seed(2)
  d <- do.call(rbind, lapply(1:12, function(i) {
    t01  <- seq(0, 1, length.out = 10)
    tone <- c("T1", "T2")[(i %% 2) + 1]
    f0   <- (if (tone == "T1") rep(200, 20) else c(170 + 50 * t01, 220 - 50 * t01)) +
            stats::rnorm(20, sd = 1)
    data.frame(word = sprintf("w%02d", i), time = seq(0, 0.38, by = 0.02),
               syllable_t01 = rep(t01, 2), f0 = f0,
               speaker = if (i <= 6) "S1" else "S2", tone = tone, stringsAsFactors = FALSE)
  }))
  cl <- cvnc_bind(env, "cluster_ui", dataset = rv(d), normalised_data = rv(),
                  curated_data = NULL, cluster_data = rv())
  shiny::testServer(cl, {
    session$setInputs(cluster_dataset = "uploaded", cluster_token_var = "word",
                      cluster_f0_var = "f0", cluster_time_var = "syllable_t01",
                      cluster_speaker_var = "speaker", cluster_tone_var = "__none__",
                      cluster_features = "points", cluster_degree = 4, cluster_npoints = 10,
                      cluster_register = "level", cluster_method = "kmeans",
                      cluster_linkage = "ward.D2", cluster_kmode = "manual", cluster_k = 2,
                      cluster_kmax = 4)
    # The message leaves the R console for a warning toast
    expect_no_message(session$setInputs(cluster_run = 1), message = "are averaged")
    expect_no_message(cl_run(), message = "are averaged")
    expect_identical(cl_run()$k, 2L)
    tie <- Filter(function(t) grepl("are averaged", t$text, fixed = TRUE), rec$toasts)
    expect_length(tie, 1)
    expect_identical(tie[[1]]$type, "warning")
    expect_match(tie[[1]]$text, "In 12 token(s) (w01, w02, w03, w04, w05, ...), two or more rows",
                 fixed = TRUE)
    # On the real time column no rows share a time: no toast
    rec$toasts <- list()
    session$setInputs(cluster_time_var = "time", cluster_run = 2)
    expect_identical(cl_run()$k, 2L)
    expect_length(rec$toasts, 0)
  })
})

test_that("the Cluster guide puts the syllables in sequence, not stacked", {
  env <- cvnc_env()
  cl <- cvnc_bind(env, "cluster_ui", dataset = rv(cvnc_data()), normalised_data = rv(),
                  curated_data = NULL, cluster_data = rv())
  shiny::testServer(cl, {
    guide <- cvnc_text(output$cluster_guide)
    # The figure's description, its _tseq panel, and the Normalise and Cluster steps
    expect_match(guide, "_tseq puts each part&#8217;s f0 in sequence, end to end, so the boundaries line up",
                 fixed = TRUE)
    expect_match(guide, ">parts in sequence</text>", fixed = TRUE)
    expect_match(guide, "<code>&lt;tier&gt;_tseq</code> (the parts in sequence: 0&ndash;1, 1&ndash;2, ...)",
                 fixed = TRUE)
    expect_match(guide, "<code>&lt;tier&gt;_tbound</code> (the same, with the gap kept at each TextGrid boundary)",
                 fixed = TRUE)
    expect_match(guide, paste("Set the <strong>Time variable</strong> to <code>&lt;tier&gt;_tseq</code>",
                              "or <code>&lt;tier&gt;_tbound</code> to put the syllables in sequence"),
                 fixed = TRUE)
    expect_no_match(guide, "stacked", ignore.case = TRUE)
  })
})
