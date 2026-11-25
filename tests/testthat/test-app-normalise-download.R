# Normalise's Download Normalised Data (inst/app/ui/normalise_ui.R) saves
# every column: the full data of the last run, as the other tabs get it
# (normalised_data(), or curated_norm_data() on the curated route), not the
# narrow table on screen (norm_display()).
# - An f0-only run keeps every input column, in its order, then speaker_mean
#   and the normalised f0.
# - A run with time normalisation adds the axes after those.
# - The table stays the narrow subset.
# - A new data_version, a new upload, a switch of source, or the curated +
#   normalised data being cleared leaves nothing to download.

# The helpers below copy vf_env(), vf_norm(), vf_norm_data() and
# vf_norm_inputs() from test-app-f-voiced-axes.R, under nd_ names.

# global.R's definitions and the Normalise module, with shiny's and
# ggplot2's exports in reach, as in the running app. showNotification()
# records its messages.
nd_env <- function() {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  pkgs <- new.env(parent = globalenv())
  for (p in c("shiny", "ggplot2"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  sys.source(file.path(app, "ui", "normalise_ui.R"), env, keep.source = FALSE)
  env$toasts <- list()
  env$showNotification <- function(ui, ..., id = NULL, type = "default", duration = 5) {
    env$toasts[[length(env$toasts) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                  type = type)
    invisible(id)
  }
  env
}

# The Normalise module bound to its reactives, for shiny::testServer(): the
# uploaded data, normalised_data (`nd`), the Start page's data_version, and
# (for the curated route) curated_data and curated_norm_data.
nd_norm <- function(env, d, start_cols = NULL, nd = shiny::reactiveVal(NULL),
                    data_version = NULL, curated_data = NULL, curated_norm_data = NULL) {
  bound <- function(input, output, session) NULL
  body(bound) <- body(env$normalised_ui)
  environment(bound) <- list2env(list(
    dataset = shiny::reactiveVal(d), normalised_data = nd,
    curated_data = curated_data, curated_norm_data = curated_norm_data,
    start_cols = if (!is.null(start_cols)) shiny::reactiveVal(start_cols),
    data_version = data_version), parent = env)
  bound
}

# Two-syllable tokens, 10 ms frames, syllable 1 at 0-0.1 s and syllable 2 at
# 0.1-0.2 s (vf_norm_data()), with the columns the table never shows: the
# Start page's voiced_run and screen_flag, and an intensity column between
# f0_corrected and speaker, so the input order is not the table's.
nd_norm_data <- function() {
  mk <- function(tok, spk, tone, shift) {
    t  <- round(0.005 + 0.01 * (0:19), 4)
    f0 <- 180 + 2 * (0:19) + shift
    f0[c(1, 20)] <- NA
    fc <- f0
    fc[c(2, 19, 7, 10, 11)] <- NA
    s1 <- t < 0.1
    int <- round(60 + 0.5 * (0:19) - shift / 10, 2)
    int[20] <- NA
    data.frame(token = tok, time = t, f0 = f0, f0_corrected = fc, intensity = int,
               speaker = spk, tone = tone,
               syllable = ifelse(s1, "ma", "ba"),
               syllable_start = ifelse(s1, 0, 0.1), syllable_end = ifelse(s1, 0.1, 0.2),
               syllable_i = ifelse(s1, 1L, 2L),
               voiced_run = ifelse(is.na(fc), NA_integer_, cumsum(is.na(fc)) + 1L),
               screen_flag = is.na(fc), stringsAsFactors = FALSE)
  }
  rbind(mk("a1", "S1", "T1", 0), mk("a2", "S1", "T2", 20),
        mk("b1", "S2", "T1", 30), mk("b2", "S2", "T2", 50))
}
nd_norm_inputs <- function(session, f0 = "f0_corrected", set = "__none__", button = 1) {
  session$setInputs(norm_f0_var = f0, norm_speaker_var = "speaker", norm_tone_var = "tone",
                    mean_calc_method = "simple", normalisation_method = "semitone",
                    sd_correction = TRUE, norm_time_set = set, norm_time_var = "time",
                    norm_time_token_var = "token", output_filename = "out")
  session$setInputs(normalise_button = button)
}

# The downloaded file, read back, and its raw lines
nd_read <- function(path) utils::read.csv(path, stringsAsFactors = FALSE)
nd_lines <- function(path) readLines(path, warn = FALSE)

# Every column of the file equals the run's data frame, values and types
nd_expect_same <- function(dl, full) {
  expect_identical(names(dl), names(full))
  expect_identical(nrow(dl), nrow(full))
  for (nm in names(full))
    expect_equal(dl[[nm]], full[[nm]], tolerance = 1e-12, ignore_attr = TRUE, label = nm)
}

nd_sc <- list(token = "token", time = "time", f0 = "f0_corrected")

test_that("an f0-only run downloads every input column, then speaker_mean and f0_st", {
  env <- nd_env()
  d   <- nd_norm_data()
  nd  <- shiny::reactiveVal(NULL)
  srv <- nd_norm(env, d, nd_sc, nd = nd)
  shiny::testServer(srv, {
    nd_norm_inputs(session, set = "__none__")
    full <- nd()
    expect_identical(names(full), c(names(d), "speaker_mean", "f0_st"))
    path <- output$download_data
    dl <- nd_read(path)
    # Every column, the input order first, then the new ones
    expect_identical(names(dl), c(names(d), "speaker_mean", "f0_st"))
    expect_true(all(c("token", "time", "voiced_run", "screen_flag", "intensity") %in% names(dl)))
    # The values are normalised_data()'s, the input columns as they were read
    nd_expect_same(dl, full)
    for (nm in names(d)) expect_equal(dl[[nm]], d[[nm]], ignore_attr = TRUE, label = nm)
    expect_type(dl$screen_flag, "logical")
    expect_type(dl$voiced_run, "integer")
    # Only the names and values: one header line and one line per row
    ln <- nd_lines(path)
    expect_length(ln, nrow(d) + 1L)
    expect_identical(ln[1], paste0('"', c(names(d), "speaker_mean", "f0_st"), '"', collapse = ","))
    # The table on screen is still the narrow subset
    expect_identical(names(norm_display()),
                     c("f0_corrected", "speaker", "tone", "speaker_mean", "f0_st"))
    expect_identical(norm_lead(), character(0))
    expect_equal(norm_display(), full[, names(norm_display())])
  })
})

test_that("a run with time normalisation adds the axes and still keeps every column", {
  env <- nd_env()
  d   <- nd_norm_data()
  nd  <- shiny::reactiveVal(NULL)
  srv <- nd_norm(env, d, nd_sc, nd = nd)
  shiny::testServer(srv, {
    # A tier: the three axes after speaker_mean and f0_st
    nd_norm_inputs(session, set = "syllable")
    full <- nd()
    new  <- c("speaker_mean", "f0_st", "syllable_t01", "syllable_tseq", "syllable_tbound")
    dl <- nd_read(output$download_data)
    expect_identical(names(dl), c(names(d), new))
    nd_expect_same(dl, full)
    ref <- normalise_time_landmarks(d, "time", "syllable", f0 = "f0_corrected", token = "token")
    expect_equal(dl$syllable_tbound, ref$syllable_tbound)
    # The table is the subset it was, with the axes after the columns they
    # came from
    expect_identical(names(norm_display()),
                     c("f0_corrected", "speaker", "tone", "speaker_mean", "f0_st",
                       "time", "token", "syllable_i",
                       "syllable_t01", "syllable_tseq", "syllable_tbound"))
    expect_identical(norm_lead(), c("time", "token", "syllable_i",
                                    "syllable_t01", "syllable_tseq", "syllable_tbound"))
    expect_false(any(c("voiced_run", "screen_flag", "intensity", "syllable") %in%
                       names(norm_display())))

    # Whole token: token_t01 at the end; a rebuild keeps one copy of each column
    nd_norm_inputs(session, set = "__token__", button = 2)
    dl <- nd_read(output$download_data)
    expect_identical(names(dl), c(names(d), "speaker_mean", "f0_st", "token_t01"))
    nd_expect_same(dl, nd())
    expect_identical(names(norm_display()),
                     c("f0_corrected", "speaker", "tone", "speaker_mean", "f0_st",
                       "time", "token", "token_t01"))
  })
})

test_that("a new data_version or upload leaves nothing to download", {
  env <- nd_env()
  d   <- nd_norm_data()
  dv  <- shiny::reactiveVal(0L)
  nd  <- shiny::reactiveVal(NULL)
  srv <- nd_norm(env, d, nd_sc, nd = nd, data_version = dv)
  shiny::testServer(srv, {
    # Nothing before the first run
    expect_error(output$download_data, class = "shiny.silent.error")
    nd_norm_inputs(session)
    expect_identical(names(nd_read(output$download_data)), names(nd()))
    dv(1L)
    session$flushReact()
    expect_null(norm_full())
    expect_null(norm_display())
    expect_error(output$download_data, class = "shiny.silent.error")
    # A new run fills it again; a new upload clears it
    nd_norm_inputs(session, button = 2)
    expect_false(is.null(norm_full()))
    session$setInputs(uploadfile = list(name = "new.csv", datapath = "new.csv"))
    expect_null(norm_full())
    expect_error(output$download_data, class = "shiny.silent.error")
  })
})

test_that("on the curated route the download is the curated + normalised data, every column", {
  env <- nd_env()
  d   <- nd_norm_data()
  # Curate dropped token b2 and relabelled a2's tone; its attributes say
  # which columns it used
  cd  <- d[d$token != "b2", ]
  cd$tone[cd$token == "a2"] <- "T3"
  rownames(cd) <- NULL
  attr(cd, "curate_vars") <- list(token = "token", tone = "tone", speaker = "speaker")
  cdat  <- shiny::reactiveVal(cd)
  cnorm <- shiny::reactiveVal(NULL)
  nd    <- shiny::reactiveVal(NULL)
  dv    <- shiny::reactiveVal(0L)
  srv <- nd_norm(env, d, nd_sc, nd = nd, data_version = dv,
                 curated_data = cdat, curated_norm_data = cnorm)
  shiny::testServer(srv, {
    session$flushReact()
    nd_norm_inputs(session)
    full <- cnorm()
    expect_false(is.null(full))
    expect_null(nd())                       # normalised_data is left alone
    path <- output$download_data
    dl <- nd_read(path)
    expect_identical(names(dl), c(names(cd), "speaker_mean", "f0_st"))
    expect_identical(nrow(dl), nrow(cd))
    expect_setequal(unique(dl$token), c("a1", "a2", "b1"))
    expect_true(all(dl$tone[dl$token == "a2"] == "T3"))
    nd_expect_same(dl, full)
    # The run's attributes stay in R, not in the file
    expect_false(is.null(attr(full, "curated_key")))
    ln <- nd_lines(path)
    expect_length(ln, nrow(cd) + 1L)
    expect_identical(ln[1], paste0('"', names(full), '"', collapse = ","))
    expect_false(any(grepl("curate_vars|curated_key", ln)))
    # The table is the narrow subset
    expect_identical(names(norm_display()),
                     c("f0_corrected", "speaker", "tone", "speaker_mean", "f0_st"))

    # The curated + normalised data cleared (Curate changed): nothing to download
    cnorm(NULL)
    session$flushReact()
    expect_null(norm_full())
    expect_error(output$download_data, class = "shiny.silent.error")

    # A refused f0 (a normalised column) on the curated route clears it too
    nd_norm_inputs(session, button = 2)
    expect_false(is.null(norm_full()))
    session$setInputs(norm_f0_var = "f0_st", normalise_button = 3)
    expect_null(norm_full())
    expect_null(norm_display())

    # A curated run that cannot normalise (no such speaker variable) clears it too
    nd_norm_inputs(session, button = 31)
    expect_false(is.null(norm_full()))
    session$setInputs(norm_speaker_var = "nope", normalise_button = 32)
    expect_null(norm_full())
    expect_error(output$download_data, class = "shiny.silent.error")

    # A switch of source clears it; the uploaded run then downloads the upload
    nd_norm_inputs(session, button = 4)
    expect_false(is.null(norm_full()))
    session$setInputs(norm_source = "uploaded")
    expect_null(norm_full())
    expect_error(output$download_data, class = "shiny.silent.error")
    nd_norm_inputs(session, button = 5)
    dl <- nd_read(output$download_data)
    expect_identical(names(dl), c(names(d), "speaker_mean", "f0_st"))
    expect_identical(nrow(dl), nrow(d))
    nd_expect_same(dl, nd())
  })
})
