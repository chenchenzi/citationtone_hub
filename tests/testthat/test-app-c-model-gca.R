# The Model and GCA tabs (inst/app/ui/model_ui.R, gca_ui.R) and the Start
# page: the token and f0 pickers default to the Start-page columns and keep
# the user's own pick, a data_version bump clears the fit, and Show R code
# starts with the Start-page steps (read_data_code() with start_settings).

# global.R's definitions and the two modules, with the exports of the
# packages the app attaches, as in the running app. No CJK font lookup.
mg_env <- function() {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  pkgs <- new.env(parent = globalenv())
  for (p in c("shiny", "ggplot2", "dplyr"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  assign("cache", "", environment(env$cjk_family))
  for (f in c("model_ui.R", "gca_ui.R")) sys.source(file.path(app, "ui", f), env, keep.source = FALSE)
  env
}

# A module bound to its extra reactives, for shiny::testServer(). Every
# argument after `session` is given, the three Start-page ones included.
mg_bind <- function(env, name, ...) {
  bound <- function(input, output, session) NULL
  body(bound) <- body(env[[name]])
  environment(bound) <- list2env(list(...), parent = env)
  bound
}
# `d` is the data or a reactiveVal holding it; testServer()'s expression sees
# the module's own variables but not these arguments, so a test that changes
# them keeps its own reactiveVal.
mg_rv <- function(x) if (is.function(x)) x else shiny::reactiveVal(x)
mg_model <- function(env, d, start_cols = NULL, data_version = NULL, start_settings = NULL,
                     normalised = NULL) {
  mg_bind(env, "model_ui", dataset = mg_rv(d),
          normalised_data = shiny::reactiveVal(normalised), curated_data = NULL,
          cluster_data = NULL, curated_norm_data = NULL, start_cols = start_cols,
          data_version = data_version, start_settings = start_settings)
}
mg_gca <- function(env, d, start_cols = NULL, data_version = NULL, start_settings = NULL,
                   gca_pred_data = NULL) {
  mg_bind(env, "gca_ui", dataset = mg_rv(d),
          normalised_data = shiny::reactiveVal(NULL), gca_pred_data = mg_rv(gca_pred_data),
          curated_data = NULL, cluster_data = NULL, curated_norm_data = NULL,
          start_cols = start_cols, data_version = data_version, start_settings = start_settings)
}

# The option a rendered sidebar selects in the <select> with this id.
mg_selected <- function(ui, id) {
  html <- paste(as.character(ui$html), collapse = "")
  sel <- regmatches(html, regexpr(sprintf('(?s)<select id="%s".*?</select>', id), html, perl = TRUE))
  if (!length(sel)) return(NULL)
  m <- regmatches(sel, regexpr('<option value="[^"]*" selected>', sel))
  if (!length(m)) return(NULL)
  sub('<option value="([^"]*)" selected>', "\\1", m)
}
mg_html <- function(x) paste(as.character(x$html), collapse = "")
# The text of the R code block, unescaped, so it can be run.
mg_code <- function(x) {
  html <- mg_html(x)
  code <- sub("(?s).*<code>(.*)</code>.*", "\\1", html, perl = TRUE)
  for (p in list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&#39;", "'"),
                 c("&amp;", "&")))
    code <- gsub(p[1], p[2], code, fixed = TRUE)
  code
}

# Long-format f0, 10 ms frames: 3 speakers x 2 tones x 2 items x 2 repetitions.
# f0_corrected and f0_rapt have f0 on every row; f0 lacks it on two rows.
mg_data <- function() {
  set.seed(3)
  g <- expand.grid(speaker = c("S1", "S2", "S3"), tone = c("T1", "T2"), item = c("w1", "w2"),
                   rep = 1:2, stringsAsFactors = FALSE)
  d <- do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    t   <- seq(0.005, 0.095, by = 0.01)
    up  <- if (g$tone[i] == "T1") 1 else -1
    f0  <- 150 + 30 * (g$tone[i] == "T2") + 400 * up * t + 2000 * (t - 0.05)^2 +
           stats::rnorm(10, sd = 2)
    tok <- paste(g$speaker[i], g$tone[i], g$item[i], g$rep[i], sep = "_")
    data.frame(token = tok, filename = paste0(tok, ".wav"), speaker = g$speaker[i],
               tone = g$tone[i], item = g$item[i], time = t, f0 = f0, f0_corrected = f0,
               f0_rapt = f0 + 1, stringsAsFactors = FALSE)
  }))
  d$f0[c(1, 15)] <- NA
  d
}

mg_set_model <- function(session, f0 = "f0_corrected", ...) {
  session$setInputs(model_dataset = "uploaded", model_token_var = "token", model_f0_var = f0,
                    model_time_var = "time", model_speaker_var = "speaker",
                    model_tone_var = "tone", model_degree = "2", ...)
}
mg_set_gca <- function(session, f0 = "f0_corrected", ...) {
  session$setInputs(gca_dataset = "uploaded", gca_token_var = "token", gca_f0_var = f0,
                    gca_time_var = "time", gca_speaker_var = "speaker", gca_tone_var = "tone",
                    gca_item_var = "item", gca_degree = "2", gca_ri_speaker = TRUE,
                    gca_ri_item = FALSE, gca_rs_speaker = FALSE, gca_rs_item = FALSE, ...)
}

test_that("Model and GCA default to the Start page's token and f0 columns", {
  env <- mg_env()
  d <- mg_data()
  # Without the Start columns (an older call): the guesses as before
  shiny::testServer(mg_model(env, d), {
    ui <- output$ui_model
    expect_identical(mg_selected(ui, "model_token_var"), "token")
    expect_identical(mg_selected(ui, "model_f0_var"), "f0")
    expect_identical(mg_selected(ui, "model_time_var"), "time")
  })

  sc <- shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0_corrected"))
  norm <- transform(d, f0_lz = as.numeric(scale(f0_corrected)))
  shiny::testServer(mg_model(env, d, start_cols = sc, data_version = shiny::reactiveVal(0L),
                             normalised = norm), {
    ui <- output$ui_model
    expect_identical(mg_selected(ui, "model_token_var"), "token")
    expect_identical(mg_selected(ui, "model_f0_var"), "f0_corrected")   # not the raw f0
    expect_identical(mg_selected(ui, "model_time_var"), "time")
    # The browser sends back what it was given; pickers left there follow
    # the Start page.
    mg_set_model(session)
    sc(list(token = "filename", time = "time", f0 = "f0_rapt"))
    session$flushReact()
    ui <- output$ui_model
    expect_identical(mg_selected(ui, "model_token_var"), "filename")
    expect_identical(mg_selected(ui, "model_f0_var"), "f0_rapt")
    # A normalised column keeps its priority over the screened column
    session$setInputs(model_token_var = "filename", model_f0_var = "f0_rapt",
                      model_dataset = "normalised")
    ui <- output$ui_model
    expect_identical(mg_selected(ui, "model_f0_var"), "f0_lz")
    expect_identical(mg_selected(ui, "model_token_var"), "filename")
  })

  sc <- shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0_corrected"))
  shiny::testServer(mg_gca(env, d, start_cols = sc, data_version = shiny::reactiveVal(0L)), {
    ui <- output$ui_gca
    expect_identical(mg_selected(ui, "gca_token_var"), "token")
    expect_identical(mg_selected(ui, "gca_f0_var"), "f0_corrected")
    expect_identical(mg_selected(ui, "gca_time_var"), "time")
    expect_identical(mg_selected(ui, "gca_item_var"), "item")
    mg_set_gca(session)
    sc(list(token = "filename", time = "time", f0 = "f0_rapt"))
    session$flushReact()
    ui <- output$ui_gca
    expect_identical(mg_selected(ui, "gca_token_var"), "filename")
    expect_identical(mg_selected(ui, "gca_f0_var"), "f0_rapt")
  })
})

test_that("Model and GCA keep the user's own column while the data still has it", {
  env <- mg_env()
  d <- mg_data()
  for (tab in c("model", "gca")) {
    sc <- shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0_corrected"))
    ds <- shiny::reactiveVal(d)
    srv <- if (tab == "model") mg_model(env, ds, start_cols = sc)
           else mg_gca(env, ds, start_cols = sc)
    shiny::testServer(srv, {
      ui_id <- paste0("ui_", tab)
      id <- function(x) paste0(tab, "_", x)
      set <- if (tab == "model") mg_set_model else mg_set_gca
      output[[ui_id]]
      set(session)
      expect_null(output[[id("f0_note")]])
      # The user's own picks: the raw f0 and the speaker as the tone
      do.call(session$setInputs, stats::setNames(list("f0", "speaker"), id(c("f0_var", "tone_var"))))
      expect_match(mg_html(output[[id("f0_note")]]),
                   "f0 has 2 rows without f0. The Start page screens f0_corrected.",
                   fixed = TRUE, info = tab)
      sc(list(token = "filename", time = "time", f0 = "f0_rapt"))
      session$flushReact()
      ui <- output[[ui_id]]
      expect_identical(mg_selected(ui, id("f0_var")), "f0", info = tab)
      expect_identical(mg_selected(ui, id("tone_var")), "speaker", info = tab)
      expect_identical(mg_selected(ui, id("token_var")), "filename", info = tab)  # not the user's
      expect_match(mg_html(output[[id("f0_note")]]), "The Start page screens f0_rapt.",
                   fixed = TRUE, info = tab)
      # Once the data no longer has it, the default comes back
      ds(d[, names(d) != "f0"])
      session$flushReact()
      expect_identical(mg_selected(output[[ui_id]], id("f0_var")), "f0_rapt", info = tab)
    })
  }
})

test_that("Model and GCA keep the user's pick when the Start default changes and changes back", {
  env <- mg_env()
  d <- mg_data()
  for (tab in c("model", "gca")) {
    sc <- shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0_corrected"))
    srv <- if (tab == "model") mg_model(env, d, start_cols = sc) else mg_gca(env, d, start_cols = sc)
    shiny::testServer(srv, {
      ui_id <- paste0("ui_", tab)
      f0_id <- paste0(tab, "_f0_var")
      output[[ui_id]]
      (if (tab == "model") mg_set_model else mg_set_gca)(session)
      # The user's own pick: the raw f0
      do.call(session$setInputs, stats::setNames(list("f0"), f0_id))
      # The Start f0 passes through the user's column and back
      sc(list(token = "token", time = "time", f0 = "f0"))
      session$flushReact()
      expect_identical(mg_selected(output[[ui_id]], f0_id), "f0", info = tab)
      sc(list(token = "token", time = "time", f0 = "f0_corrected"))
      session$flushReact()
      expect_identical(mg_selected(output[[ui_id]], f0_id), "f0", info = tab)
      # Picking the default again makes the menu follow the Start page
      do.call(session$setInputs, stats::setNames(list("f0_corrected"), f0_id))
      sc(list(token = "token", time = "time", f0 = "f0_rapt"))
      session$flushReact()
      expect_identical(mg_selected(output[[ui_id]], f0_id), "f0_rapt", info = tab)
    })
  }
})

test_that("a data_version bump or a new upload clears the Model fit", {
  env <- mg_env()
  d <- mg_data()
  dv <- shiny::reactiveVal(0L)
  shiny::testServer(mg_model(env, d, data_version = dv), {
    mg_set_model(session, model_button = 1)
    expect_equal(nrow(model_result()), 24L)
    expect_match(mg_html(output$model_summary), "Tokens fitted: 24 / 24", fixed = TRUE)
    dv(1L)
    session$flushReact()
    expect_null(model_result())
    expect_error(output$model_summary)
    session$setInputs(model_button = 2)
    expect_false(is.null(model_result()))
    session$setInputs(uploadfile = list(name = "next.csv"))
    expect_null(model_result())
  })
  # An older harness that binds the body without the three Start-page
  # arguments: they count as left out, and a new upload still clears the fit
  old <- mg_bind(env, "model_ui", dataset = shiny::reactiveVal(d),
                 normalised_data = shiny::reactiveVal(NULL), curated_data = NULL,
                 cluster_data = NULL, curated_norm_data = NULL)
  shiny::testServer(old, {
    expect_identical(mg_selected(output$ui_model, "model_f0_var"), "f0")
    mg_set_model(session, model_button = 1, model_show_code = 1)
    expect_false(is.null(model_result()))
    expect_no_match(mg_code(output$model_r_code), "stopifnot(", fixed = TRUE)
    session$setInputs(uploadfile = list(name = "next.csv"))
    expect_null(model_result())
  })
})

test_that("a data_version bump clears the GCA fit, table, plot and comparisons", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("emmeans")
  env <- mg_env()
  d <- mg_data()
  dv <- shiny::reactiveVal(0L)
  pred <- shiny::reactiveVal(NULL)
  shiny::testServer(mg_gca(env, d, data_version = dv, gca_pred_data = pred), {
    mg_set_gca(session)
    suppressWarnings(suppressMessages(session$setInputs(gca_button = 1)))
    fit_state <- function() list(gca_model(), gca_fixef_table(), gca_plot_data(), gca_obs_data(),
                                 gca_formula_str(), gca_tone_estimates(), gca_pairwise())
    expect_false(any(vapply(fit_state(), is.null, logical(1))))
    expect_false(is.null(pred()))
    dv(1L)
    session$flushReact()
    expect_true(all(vapply(fit_state(), is.null, logical(1))))
    expect_null(gca_convergence_warning())
    expect_error(output$gca_summary)
  })
  # An older harness without the three Start-page arguments still fits
  old <- mg_bind(env, "gca_ui", dataset = shiny::reactiveVal(d),
                 normalised_data = shiny::reactiveVal(NULL), gca_pred_data = shiny::reactiveVal(NULL),
                 curated_data = NULL, cluster_data = NULL, curated_norm_data = NULL)
  shiny::testServer(old, {
    expect_identical(mg_selected(output$ui_gca, "gca_f0_var"), "f0")
    mg_set_gca(session)
    suppressWarnings(suppressMessages(session$setInputs(gca_button = 1)))
    expect_false(is.null(gca_model()))
    session$setInputs(uploadfile = list(name = "next.csv"))
    expect_null(gca_model())
  })
})

# A CSV with rows without f0 in f0_corrected at the token edges, and the
# settings the Start page gives read_data_code() for it with removal ticked.
mg_padded_csv <- function() {
  d <- mg_data()
  d$f0_corrected[d$time < 0.01 | d$time > 0.09] <- NA
  d$f0_corrected[d$token == "S1_T1_w1_1" & abs(d$time - 0.045) < 1e-9] <- NA   # a gap
  f <- tempfile(fileext = ".csv")
  utils::write.csv(d, f, row.names = FALSE, na = "")
  raw <- shinytone::zero_hz_to_na(shinytone::read_csv_utf8(f, stringsAsFactors = FALSE,
                                                           na.strings = c("NA", "--undefined--")))
  scr <- shinytone::screen_f0_rows(raw, token = "token", time = "time", f0 = "f0_corrected")
  s <- attr(scr, "summary")
  settings <- list(
    version = as.character(utils::packageVersion("shinytone")), zero_hz = NULL,
    dropped = NULL, metadata = NULL, durations = list(),
    screen = list(token = "token", time = "time", f0 = "f0_corrected", unvoiced = NULL,
                  action = "remove", min_run = 1, rows = nrow(raw),
                  without = s[["rows_without_f0"]], summary = s, rows_after = s[["rows_after"]],
                  gaps_marked = 0, short_rows = 0))
  data <- suppressMessages(shinytone::drop_rows_without_f0(raw, token = "token", time = "time",
                                                           f0 = "f0_corrected"))
  list(file = f, name = sub("\\.csv$", "", f), settings = settings, data = data)
}

test_that("Model's R code starts with the Start-page steps and reproduces the fit", {
  env <- mg_env()
  p <- mg_padded_csv()
  start <- paste(env$read_data_code(paste0(p$name, ".csv"), start = p$settings), collapse = "\n")
  shiny::testServer(mg_model(env, p$data, start_settings = shiny::reactive(p$settings)), {
    session$setInputs(dataset_name = p$name)
    mg_set_model(session, model_show_code = 1)
    code <- mg_code(output$model_r_code)
    expect_true(startsWith(code, paste0("library(dplyr)\n\n", start, "\n\n")))
    at <- vapply(c("stopifnot(utils::packageVersion", "read_csv_utf8(", "zero_hz_to_na(dat)",
                   "screen_f0_rows(dat", "drop_rows_without_f0(dat", "legendre_basis <-"),
                 function(x) regexpr(x, code, fixed = TRUE)[[1]], numeric(1))
    expect_true(all(at > 0))
    expect_false(is.unsorted(at))
    expect_match(code, 'drop_rows_without_f0(dat, token = "token", time = "time", f0 = "f0_corrected")',
                 fixed = TRUE)

    # Run it: the same rows as the app's data, and the same coefficients
    run <- new.env(parent = globalenv())
    suppressMessages(eval(parse(text = code, encoding = "UTF-8"), run))
    expect_equal(nrow(run$dat), nrow(p$data))
    expect_false(anyNA(run$dat$f0_corrected))
    session$setInputs(model_button = 1)
    app <- as.data.frame(model_result())
    got <- as.data.frame(run$results)
    got <- got[match(app$token, got$token), ]
    for (cn in c("c0", "c1", "c2")) expect_equal(got[[cn]], app[[cn]], tolerance = 1e-8)
  })
  # Without start_settings: the read lines only, as before
  shiny::testServer(mg_model(env, p$data), {
    session$setInputs(dataset_name = p$name)
    mg_set_model(session, model_show_code = 1)
    code <- mg_code(output$model_r_code)
    old <- paste(env$read_data_code(paste0(p$name, ".csv")), collapse = "\n")
    expect_true(startsWith(code, paste0("library(dplyr)\n\n", old, "\n\n# Legendre")))
    expect_no_match(code, "stopifnot(", fixed = TRUE)
  })
})

test_that("GCA's R code starts with the Start-page steps and reproduces the fit", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("emmeans")
  env <- mg_env()
  p <- mg_padded_csv()
  start <- paste(env$read_data_code(paste0(p$name, ".csv"), start = p$settings), collapse = "\n")
  shiny::testServer(mg_gca(env, p$data, start_settings = shiny::reactive(p$settings)), {
    session$setInputs(dataset_name = p$name)
    mg_set_gca(session, gca_show_code = 1)
    code <- mg_code(output$gca_r_code)
    expect_true(startsWith(code, paste0("library(dplyr)\nlibrary(lme4)\n\n", start, "\n\n")))
    at <- vapply(c("stopifnot(utils::packageVersion", "read_csv_utf8(", "drop_rows_without_f0(dat",
                   "poly_matrix <-", "model <- lmer("),
                 function(x) regexpr(x, code, fixed = TRUE)[[1]], numeric(1))
    expect_true(all(at > 0))
    expect_false(is.unsorted(at))

    run <- new.env(parent = globalenv())
    suppressWarnings(suppressMessages(eval(parse(text = code, encoding = "UTF-8"), run)))
    suppressWarnings(suppressMessages(session$setInputs(gca_button = 1)))
    expect_equal(unname(lme4::fixef(run$model)), unname(lme4::fixef(gca_model())),
                 tolerance = 1e-6)
  })
  shiny::testServer(mg_gca(env, p$data), {
    session$setInputs(dataset_name = p$name)
    mg_set_gca(session, gca_show_code = 1)
    code <- mg_code(output$gca_r_code)
    old <- paste(env$read_data_code(paste0(p$name, ".csv")), collapse = "\n")
    expect_true(startsWith(code, paste0("library(dplyr)\nlibrary(lme4)\n\n", old, "\n\n")))
    expect_no_match(code, "stopifnot(", fixed = TRUE)
  })
})

test_that("in the app, Model and GCA follow the Start page and clear on its changes", {
  s <- start_server()
  app <- system.file("app", package = "shinytone")
  # The real Model and GCA tabs instead of the stubs, with the packages the
  # app attaches (dplyr after shiny, as in server.R)
  pkgs <- parent.env(s$env)
  for (p in c("ggplot2", "dplyr"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  for (f in c("model_ui.R", "gca_ui.R")) sys.source(file.path(app, "ui", f), s$env, keep.source = FALSE)

  # F0 Correction's file: f0 is the tracker's, f0_corrected the corrected
  # track, which fills one row inside S1_a that the tracker missed.
  fx <- start_padded_fixture()
  fx$f0_corrected <- fx$f0
  fx$f0_corrected[fx$token == "S1_a" & is.na(fx$f0)][3] <- 205
  f <- start_write_csv(fx)
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "padded.csv"), dataset_name = sub("\\.csv$", "", f))
    expect_identical(start_cols()$f0, "f0_corrected")
    expect_identical(mg_selected(output$ui_model, "model_f0_var"), "f0_corrected")
    expect_identical(mg_selected(output$ui_model, "model_token_var"), "token")
    expect_identical(mg_selected(output$ui_gca, "gca_f0_var"), "f0_corrected")
    expect_identical(mg_selected(output$ui_gca, "gca_token_var"), "token")

    # The raw f0 still lacks f0 on the row the correction filled
    mg_set_model(session, f0 = "f0")
    expect_match(mg_html(output$model_f0_note),
                 "f0 has 1 row without f0. The Start page screens f0_corrected.", fixed = TRUE)
    session$setInputs(model_f0_var = "f0_corrected", model_button = 1)
    expect_match(mg_html(output$model_summary), "Tokens fitted: 4 / 4", fixed = TRUE)
    session$setInputs(model_show_code = 1, gca_show_code = 1)
    mg_set_gca(session)
    for (code in list(mg_code(output$model_r_code), mg_code(output$gca_r_code))) {
      expect_match(code, "stopifnot(utils::packageVersion(\"shinytone\")", fixed = TRUE)
      expect_match(code, paste0('dat <- shinytone::drop_rows_without_f0(dat, token = "token", ',
                                'time = "time", f0 = "f0_corrected")'), fixed = TRUE)
    }

    # Unticking the removal changes the rows: the fit is cleared and the
    # R code says the rows are kept
    session$setInputs(screen_remove = FALSE)
    expect_identical(data_version(), 1L)
    expect_error(output$model_summary)
    expect_match(mg_code(output$model_r_code), "stay (removal unticked)", fixed = TRUE)
    expect_no_match(mg_code(output$gca_r_code), "drop_rows_without_f0(dat", fixed = TRUE)
  })
})

# Disyllabic tokens, 10 ms frames, with a syllable tier (two parts per
# token) and a word tier (one part per token). Rows without f0 at both
# edges and at the syllable boundary. The 0-1 axes come from the package's
# builders, from the first f0 to the last.
mg_disyllabic <- function() {
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

test_that("Model and GCA count the parts of a tier: one part per token is a whole-token axis", {
  env <- mg_env()
  d <- mg_disyllabic()
  expect_identical(env$t01_parts(d, "syllable_t01", "token"), 2L)
  expect_identical(env$t01_parts(d, "word_t01", "token"), 1L)
  # A copy of syllable_t01 under a name whose set has no columns: the parts
  # cannot be counted, so it counts as several
  d$syl_t01 <- d$syllable_t01
  nudge <- "Are you modelling a multisyllabic time axis?"
  for (tab in c("model", "gca")) {
    srv <- if (tab == "model") mg_model(env, d) else mg_gca(env, d)
    set <- if (tab == "model") mg_set_model else mg_set_gca
    ms_id <- paste0(tab, "_multisyl_note")
    tn_id <- paste0(tab, "_timenorm_note")
    tv_id <- paste0(tab, "_time_var")
    shiny::testServer(srv, {
      set(session, f0 = "f0")
      # The stacked syllable tier is the default Time (two parts per token)
      expect_identical(mg_selected(output[[paste0("ui_", tab)]], tv_id), "syllable_tseq")
      for (v in c("syllable_t01", "syllable_tseq", "syllable_tbound", "syl_t01")) {
        do.call(session$setInputs, stats::setNames(list(v), tv_id))
        expect_match(mg_html(output[[ms_id]]), nudge, fixed = TRUE, info = paste(tab, v))
        expect_null(output[[tn_id]])
      }
      # One part per token: no nudge, and the blue note, which says where 0
      # and 1 fall
      do.call(session$setInputs, stats::setNames(list("word_t01"), tv_id))
      expect_null(output[[ms_id]])
      note <- mg_html(output[[tn_id]])
      expect_match(note, "<code>word_t01</code> is already on 0&ndash;1", fixed = TRUE)
      expect_match(note, "its name marks a 0&ndash;1 axis", fixed = TRUE)
      expect_match(note, "each word&rsquo;s first and last f0", fixed = TRUE)
      expect_null(output[[paste0(tab, "_track_edges")]])
      # word_tseq is word_t01 here: no nudge, and the note by its values
      do.call(session$setInputs, stats::setNames(list("word_tseq"), tv_id))
      expect_null(output[[ms_id]])
      expect_match(mg_html(output[[tn_id]]), "<code>word_tseq</code> looks already on 0&ndash;1",
                   fixed = TRUE)
      # So is word_tbound (one part per token)
      do.call(session$setInputs, stats::setNames(list("word_tbound"), tv_id))
      expect_null(output[[ms_id]])
      expect_match(mg_html(output[[tn_id]]), "<code>word_tbound</code> looks already on 0&ndash;1",
                   fixed = TRUE)
      do.call(session$setInputs, stats::setNames(list("token_t01"), tv_id))
      expect_null(output[[ms_id]])
      expect_match(mg_html(output[[tn_id]]), "each token&rsquo;s first and last f0", fixed = TRUE)
    })
  }

  # Only the word tier: the Time menu picks word_t01, not word_tseq
  w <- d[, setdiff(names(d), c(grep("^syl", names(d), value = TRUE), "token_t01"))]
  shiny::testServer(mg_model(env, w), {
    expect_identical(mg_selected(output$ui_model, "model_time_var"), "word_t01")
  })
  shiny::testServer(mg_gca(env, w), {
    expect_identical(mg_selected(output$ui_gca, "gca_time_var"), "word_t01")
  })
})

test_that("the Time menu: GAMM starts on <tier>_tbound, Model and GCA on <tier>_tseq", {
  app <- system.file("app", package = "shinytone")
  env <- mg_env()
  sys.source(file.path(app, "ui", "gamm_ui.R"), env, keep.source = FALSE)
  mg_gamm <- function(d) {
    mg_bind(env, "gamm_ui", dataset = mg_rv(d), normalised_data = shiny::reactiveVal(NULL),
            gamm_pred_data = shiny::reactiveVal(NULL), curated_data = NULL,
            cluster_data = NULL, curated_norm_data = NULL, start_cols = NULL,
            data_version = NULL, start_settings = NULL)
  }
  d <- mg_disyllabic()
  expect_identical(env$t01_parts(d, "syllable_tbound", "token"), 2L)
  expect_identical(env$t01_parts(d, "word_tbound", "token"), 1L)
  # Both of the syllable tier's in-sequence axes: GAMM takes the one with
  # the gap kept, Model and GCA the other. Only one of them: every tab takes
  # it, and a one-part tier's axis (word_tseq, word_tbound) is passed over.
  cases <- list(
    both        = list(d = d, gamm = "syllable_tbound", other = "syllable_tseq"),
    only_tseq   = list(d = d[setdiff(names(d), "syllable_tbound")],
                       gamm = "syllable_tseq", other = "syllable_tseq"),
    only_tbound = list(d = d[setdiff(names(d), "syllable_tseq")],
                       gamm = "syllable_tbound", other = "syllable_tbound"))
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    shiny::testServer(mg_model(env, cs$d), {
      expect_identical(mg_selected(output$ui_model, "model_time_var"), cs$other, info = nm)
    })
    shiny::testServer(mg_gca(env, cs$d), {
      expect_identical(mg_selected(output$ui_gca, "gca_time_var"), cs$other, info = nm)
    })
    shiny::testServer(mg_gamm(cs$d), {
      expect_identical(mg_selected(output$ui_gamm, "gamm_time_var"), cs$gamm, info = nm)
    })
    # guess_time_var() itself, with the data and without it
    v <- names(cs$d)
    expect_identical(env$guess_time_var(v, 3, d = cs$d, token = "token"), cs$other, info = nm)
    expect_identical(env$guess_time_var(v, 3, d = cs$d, token = "token", prefer = "tbound"),
                     cs$gamm, info = nm)
  }
  v <- names(d)
  expect_identical(env$guess_time_var(v, 3), "syllable_tseq")
  expect_identical(env$guess_time_var(v, 3, prefer = "tbound"), "syllable_tbound")
  # One part per token on every tier: the word tier's _t01 either way
  w <- d[, setdiff(names(d), c(grep("^syl", names(d), value = TRUE), "token_t01"))]
  expect_identical(env$guess_time_var(names(w), 3, d = w, token = "token", prefer = "tbound"),
                   "word_t01")
  shiny::testServer(mg_gamm(w), {
    expect_identical(mg_selected(output$ui_gamm, "gamm_time_var"), "word_t01")
  })
  # Time counted from the token start, boundaries on another clock: every
  # token is off the clock, so syllable_tbound is all NA and GAMM, like
  # Model and GCA, starts on syllable_tseq
  oc <- d
  oc$syllable_start <- oc$syllable_start + 10
  oc$syllable_end   <- oc$syllable_end + 10
  oc <- suppressMessages(normalise_time_landmarks(oc, "time", "syllable", f0 = "f0",
                                                  token = "token"))
  expect_true(all(is.na(oc$syllable_tbound)))
  expect_true(any(is.finite(oc$syllable_tseq)))
  v <- names(oc)
  expect_identical(env$guess_time_var(v, 3, d = oc, token = "token", prefer = "tbound"),
                   "syllable_tseq")
  expect_identical(env$guess_time_var(v, 3, d = oc, token = "token"), "syllable_tseq")
  shiny::testServer(mg_model(env, oc), {
    expect_identical(mg_selected(output$ui_model, "model_time_var"), "syllable_tseq")
  })
  shiny::testServer(mg_gca(env, oc), {
    expect_identical(mg_selected(output$ui_gca, "gca_time_var"), "syllable_tseq")
  })
  shiny::testServer(mg_gamm(oc), {
    expect_identical(mg_selected(output$ui_gamm, "gamm_time_var"), "syllable_tseq")
  })
  # Only the empty syllable_tbound: it is passed over, so the word tier's
  # one-part axis gives word_t01, and with no word tier the usual time guess
  ob <- oc[setdiff(names(oc), "syllable_tseq")]
  expect_identical(env$guess_time_var(names(ob), 3, d = ob, token = "token",
                                      prefer = "tbound"), "word_t01")
  ob <- ob[!grepl("^word_", names(ob))]
  expect_identical(env$guess_time_var(names(ob), 3, d = ob, token = "token",
                                      prefer = "tbound"), "time")
})

test_that("GAMM's Time menu passes over a <tier>_tbound that is NA in some tokens", {
  app <- system.file("app", package = "shinytone")
  env <- mg_env()
  sys.source(file.path(app, "ui", "gamm_ui.R"), env, keep.source = FALSE)
  mg_gamm <- function(d) {
    mg_bind(env, "gamm_ui", dataset = mg_rv(d), normalised_data = shiny::reactiveVal(NULL),
            gamm_pred_data = shiny::reactiveVal(NULL), curated_data = NULL,
            cluster_data = NULL, curated_norm_data = NULL, start_cols = NULL,
            data_version = NULL, start_settings = NULL)
  }
  d <- mg_disyllabic()
  toks <- unique(d$token)[1:10]
  d <- d[d$token %in% toks, ]
  rownames(d) <- NULL
  # Every token on the TextGrid's clock: GAMM still starts on syllable_tbound
  v <- names(d)
  expect_identical(env$guess_time_var(v, 3, d = d, token = "token", prefer = "tbound"),
                   "syllable_tbound")
  shiny::testServer(mg_gamm(d), {
    expect_identical(mg_selected(output$ui_gamm, "gamm_time_var"), "syllable_tbound")
  })

  # Two tokens' boundaries on another clock: syllable_tbound is NA in those
  # two tokens only, while syllable_tseq has values in all ten. A GAMM on
  # syllable_tbound would fit 8 tokens, so it starts on syllable_tseq, as
  # Model and GCA do
  off <- toks[c(3, 7)]
  k <- d$token %in% off
  oc <- d
  oc$syllable_start[k] <- oc$syllable_start[k] + 10
  oc$syllable_end[k]   <- oc$syllable_end[k] + 10
  expect_message(oc <- normalise_time_landmarks(oc, "time", "syllable", f0 = "f0",
                                                token = "token"),
                 "syllable_tbound is NA in 2 token(s)", fixed = TRUE)
  empty <- tapply(oc$syllable_tbound, oc$token, function(x) !any(is.finite(x)))
  expect_setequal(names(empty)[empty], off)
  expect_true(all(tapply(oc$syllable_tseq, oc$token, function(x) any(is.finite(x)))))
  v <- names(oc)
  expect_identical(env$guess_time_var(v, 3, d = oc, token = "token", prefer = "tbound"),
                   "syllable_tseq")
  expect_identical(env$guess_time_var(v, 3, d = oc, token = "token"), "syllable_tseq")
  shiny::testServer(mg_gamm(oc), {
    expect_identical(mg_selected(output$ui_gamm, "gamm_time_var"), "syllable_tseq")
  })
  shiny::testServer(mg_model(env, oc), {
    expect_identical(mg_selected(output$ui_model, "model_time_var"), "syllable_tseq")
  })
  shiny::testServer(mg_gca(env, oc), {
    expect_identical(mg_selected(output$ui_gca, "gca_time_var"), "syllable_tseq")
  })
  # Without the data, or without its token, the check cannot run: as before
  expect_identical(env$guess_time_var(v, 3, prefer = "tbound"), "syllable_tbound")
  expect_identical(env$guess_time_var(v, 3, d = oc, prefer = "tbound"), "syllable_tbound")
  # No syllable_tseq twin: the partly NA syllable_tbound is kept
  nt <- oc[setdiff(names(oc), "syllable_tseq")]
  expect_identical(env$guess_time_var(names(nt), 3, d = nt, token = "token", prefer = "tbound"),
                   "syllable_tbound")
})

test_that("Model and GCA: a 0-1 column whose f0 misses 0 or 1 gets the notice, and the note points to it", {
  env <- mg_env()
  d <- mg_disyllabic()
  # token_t01 as shinytone 1.0.0 built it: from each token's first row to
  # its last, rows without f0 included
  d$token_t01 <- stats::ave(d$time, d$token, FUN = function(t) (t - min(t)) / (max(t) - min(t)))
  for (tab in c("model", "gca")) {
    srv <- if (tab == "model") mg_model(env, d) else mg_gca(env, d)
    set <- if (tab == "model") mg_set_model else mg_set_gca
    tv_id <- paste0(tab, "_time_var")
    shiny::testServer(srv, {
      set(session, f0 = "f0")
      do.call(session$setInputs, stats::setNames(list("token_t01"), tv_id))
      edge <- mg_html(output[[paste0(tab, "_track_edges")]])
      expect_match(edge, "is already on 0&ndash;1 and is used as it is", fixed = TRUE)
      expect_match(edge, "in all 16 tokens the f0 does not", fixed = TRUE)
      note <- mg_html(output[[paste0(tab, "_timenorm_note")]])
      expect_match(note, "stay where the column puts them (see the notice above)", fixed = TRUE)
      expect_no_match(note, "each token&rsquo;s first and last f0", fixed = TRUE)
      # The real time column: no notice and no note
      do.call(session$setInputs, stats::setNames(list("time"), tv_id))
      expect_null(output[[paste0(tab, "_track_edges")]])
      expect_null(output[[paste0(tab, "_timenorm_note")]])
    })
  }
})
