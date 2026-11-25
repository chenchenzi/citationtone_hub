# The Inspect tab (inst/app/ui/inspect_ui.R; Part E1): the summary counts
# samples with f0, the time unit defaults from the data's frame step and
# shows it, a unit the user picks by hand stays until the next upload, and
# an error in the run is shown as a notification instead of ending the
# session.

# global.R's definitions and the Inspect module, with shiny's and dplyr's
# exports in reach, as in the running app.
ei_env <- function() {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  pkgs <- new.env(parent = globalenv())
  for (p in c("shiny", "dplyr"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  sys.source(file.path(app, "ui", "inspect_ui.R"), env, keep.source = FALSE)
  env
}

# Records what a module sends: notifications, and the radio and select
# updates (testServer has no browser to apply them).
ei_recorder <- function() {
  rec <- new.env()
  rec$toasts  <- list()
  rec$updates <- list()
  rec$fns <- list(
    showNotification = function(ui, ..., id = NULL, type = "default", duration = 5) {
      rec$toasts[[length(rec$toasts) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                    id = id, type = type)
      invisible(id)
    },
    updateRadioButtons = function(session, inputId, label = NULL, choices = NULL,
                                  selected = NULL, ...) {
      rec$updates[[length(rec$updates) + 1L]] <- list(id = inputId, selected = selected)
      invisible(NULL)
    },
    updateSelectInput = function(session, inputId, label = NULL, choices = NULL,
                                 selected = NULL) {
      rec$updates[[length(rec$updates) + 1L]] <- list(id = inputId, selected = selected)
      invisible(NULL)
    })
  rec
}

# The module bound to its reactives, for shiny::testServer().
ei_bind <- function(env, ...) {
  rec <- ei_recorder()
  args <- c(list(...), rec$fns)
  bound <- function(input, output, session) NULL
  body(bound) <- body(env$inspect_ui)
  environment(bound) <- list2env(args, parent = env)
  list(server = bound, rec = rec)
}

ei_html <- function(x) paste(as.character(x$html), collapse = " ")
# The checked value of a radio group in a renderUI() output.
ei_radio <- function(ui, id) {
  html <- ei_html(ui)
  m <- regmatches(html, gregexpr(sprintf('<input type="radio" name="%s" value="[^"]*"[^>]*>', id), html))[[1]]
  hit <- m[grepl("checked", m)]
  if (!length(hit)) return(NULL)
  sub('.*value="([^"]*)".*', "\\1", hit[1])
}
ei_updates <- function(rec, id) {
  vapply(Filter(function(u) identical(u$id, id), rec$updates), function(u) u$selected, "")
}
# The summary's line that starts with `what`, up to its percentage.
ei_line <- function(html, what) {
  m <- regmatches(html, regexpr(paste0(what, "[^<]*?\\([0-9.]+%\\)"), html))
  if (length(m)) m else NA_character_
}

# Long-format f0: 2 speakers x 2 tones x 6 repetitions, 14 frames of 10 ms
# with times written to 4 decimals. Each token has rows without f0 at both
# ends; S1_T1_1 has a gap of 2 rows and a 2-frame fragment after it, and
# S2_T2_2 an octave spike. Token S1_T1_9 has no f0 at all.
ei_data <- function() {
  set.seed(5)
  g <- expand.grid(spk = c("S1", "S2"), tone = c("T1", "T2"), rep = 1:6,
                   stringsAsFactors = FALSE)
  out <- do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    t  <- round(0.0125 + 0.01 * (0:13), 4)
    f0 <- 150 + 40 * (g$tone[i] == "T2") + 50 * t + stats::rnorm(14, sd = 0.5)
    f0[c(1, 2, 14)] <- NA
    tok <- sprintf("%s_%s_%d", g$spk[i], g$tone[i], g$rep[i])
    if (tok == "S1_T1_1") { f0[c(9, 10)] <- NA; f0[c(11, 12)] <- 230 }
    if (tok == "S2_T2_2") f0[7] <- 2 * f0[7]
    data.frame(token = tok, time = t, f0 = f0, intensity = 60,
               speaker = g$spk[i], tone = g$tone[i], stringsAsFactors = FALSE)
  }))
  rbind(out, data.frame(token = "S1_T1_9", time = round(0.0125 + 0.01 * (0:3), 4),
                        f0 = NA, intensity = 60, speaker = "S1", tone = "T1"))
}

# The Inspect settings, as the browser sends them once the sidebar is drawn.
ei_inputs <- function(session, unit = "s", f0 = "f0") {
  session$setInputs(inspect_f0_var = f0, inspect_token_var = "token",
                    inspect_time_var = "time", inspect_speaker_var = "speaker",
                    inspect_tone_var = "tone", inspect_intensity_var = "intensity",
                    inspect_time_unit = unit, inspect_z_thresh = 3,
                    inspect_rise_thresh = 1.263, inspect_fall_thresh = 1.714,
                    inspect_carryover_mult = 1.5, inspect_level_thresh = 3.5,
                    inspect_intensity_drop = 15)
}

test_that("the Inspect summary counts samples with f0, whatever rows the data keep", {
  env <- ei_env()
  raw <- ei_data()
  versions <- list(
    raw      = raw,
    trimmed  = trim_to_voiced(raw, min_run = 1),
    removed  = suppressMessages(drop_rows_without_f0(raw)),
    shuffled = raw[sample(nrow(raw)), ])
  n_f0 <- sum(!is.na(raw$f0))
  lines <- list()
  for (v in names(versions)) {
    ds <- shiny::reactiveVal(versions[[v]])
    b  <- ei_bind(env, dataset = ds, inspect_result = shiny::reactiveVal(NULL),
                  start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
                  data_version = shiny::reactiveVal(0L), start_settings = NULL)
    shiny::testServer(b$server, {
      ei_inputs(session)
      session$setInputs(inspect_button = 1)
      html <- ei_html(output$inspect_summary)
      lines[[v]] <<- c(jumps  = ei_line(html, "Samples with potential jumps:"),
                       lowint = ei_line(html, "Low-intensity samples \\(advisory\\):"),
                       tokens = ei_line(html, "Tokens flagged:"))
      if (v %in% c("raw", "shuffled")) {
        expect_match(html, "Tokens with no f0: 1. They are not checked", fixed = TRUE)
      } else {
        expect_false(grepl("Tokens with no f0", html, fixed = TRUE))
      }
    })
  }
  expect_match(lines$raw[["jumps"]], sprintf(" of %d samples with f0 (", n_f0), fixed = TRUE)
  expect_match(lines$raw[["lowint"]], sprintf("0 of %d samples with f0 (0%%)", n_f0), fixed = TRUE)
  expect_match(lines$raw[["tokens"]], " of 24 tokens with f0 (", fixed = TRUE)
  expect_false(grepl("^Samples with potential jumps: 0 ", lines$raw[["jumps"]]))
  for (v in names(versions)) expect_identical(lines[[v]], lines$raw, label = v)
})

test_that("the small-group note counts tokens with f0 only", {
  env <- ei_env()
  d <- ei_data()
  # S1 x T1 keeps 4 tokens with f0, plus S1_T1_9 with no f0
  d <- d[!d$token %in% c("S1_T1_3", "S1_T1_4"), ]
  b <- ei_bind(env, dataset = shiny::reactiveVal(d), inspect_result = shiny::reactiveVal(NULL),
               start_cols = NULL, data_version = NULL, start_settings = NULL)
  shiny::testServer(b$server, {
    ei_inputs(session)
    session$setInputs(inspect_button = 1)
    html <- ei_html(output$inspect_summary)
    expect_match(html, "Level check skipped for 4 tokens in 1 speaker \u00d7 tone group with fewer than 5 tokens with f0",
                 fixed = TRUE)
  })
})

test_that("the time unit defaults from the frame step and shows it", {
  env <- ei_env()
  sec <- ei_data()
  ms  <- transform(sec, time = time * 1000)
  run_flags <- list()
  for (v in c("s", "ms")) {
    d <- if (v == "s") sec else ms
    ir <- shiny::reactiveVal(NULL)
    b <- ei_bind(env, dataset = shiny::reactiveVal(d), inspect_result = ir,
                 start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
                 data_version = shiny::reactiveVal(0L), start_settings = NULL)
    shiny::testServer(b$server, {
      expect_identical(ei_radio(output$ui_inspect, "inspect_time_unit"), v)
      ei_inputs(session, unit = v)
      step <- ei_html(output$inspect_time_step)
      if (v == "s") {
        expect_match(step, "Frame step 0.01 s (10 ms)", fixed = TRUE)
      } else {
        expect_match(step, "Frame step 10 ms", fixed = TRUE)
      }
      expect_false(grepl("look like", step))
      # The rate test runs: the 2-frame fragment after the gap is a rise
      session$setInputs(inspect_button = 1)
      r <- ir()
      run_flags[[v]] <<- r$flag_notes
      expect_true(any(grepl("jump (rise) across a 20 ms gap", r$flag_notes, fixed = TRUE)))
      # The wrong unit says so
      session$setInputs(inspect_time_unit = if (v == "s") "ms" else "s")
      expect_match(ei_html(output$inspect_time_step),
                   if (v == "s") "Frame step 0.01 ms. The times look like seconds."
                   else "Frame step 10 s (10000 ms). The times look like milliseconds.",
                   fixed = TRUE)
    })
  }
  # Seconds read as seconds and ms read as ms give the same flags
  expect_identical(run_flags$ms, run_flags$s)
})

test_that("a hand-picked time unit stays when the rows change, not on a new upload", {
  env <- ei_env()
  sec <- ei_data()
  ms  <- transform(sec, time = time * 1000)
  ds  <- shiny::reactiveVal(sec)
  dv  <- shiny::reactiveVal(0L)
  b <- ei_bind(env, dataset = ds, inspect_result = shiny::reactiveVal(NULL),
               start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
               data_version = dv, start_settings = NULL)
  shiny::testServer(b$server, {
    expect_identical(ei_radio(output$ui_inspect, "inspect_time_unit"), "s")
    ei_inputs(session, unit = "s")
    expect_null(units$user)

    # A new upload of ms data: the unit follows the data
    ds(ms)
    session$setInputs(uploadfile = list(name = "ms.csv", datapath = tempfile()))
    expect_identical(ei_updates(b$rec, "inspect_time_unit"), "ms")
    session$setInputs(inspect_time_unit = "ms")   # the browser applies it
    expect_null(units$user)

    # The user picks seconds by hand
    session$setInputs(inspect_time_unit = "s")
    expect_identical(units$user, "s")
    # The rows change (a Start-page box): the pick stays
    ds(ms[!is.na(ms$f0), ])
    dv(1L)
    session$flushReact()
    expect_identical(ei_updates(b$rec, "inspect_time_unit"), "ms")
    # A new column draws the sidebar again: still the user's pick
    ds(cbind(ms[!is.na(ms$f0), ], voiced_run = 1L))
    dv(2L)
    session$flushReact()
    expect_identical(ei_radio(output$ui_inspect, "inspect_time_unit"), "s")
    # Seconds data on the Start page (the guess changes): the pick stays
    ds(sec)
    dv(3L)
    session$flushReact()
    expect_identical(ei_updates(b$rec, "inspect_time_unit"), "ms")
    ds(ms)
    dv(4L)
    session$flushReact()
    expect_identical(ei_updates(b$rec, "inspect_time_unit"), "ms")

    # A new upload: the data choose again, even with the same guess
    session$setInputs(uploadfile = list(name = "ms2.csv", datapath = tempfile()))
    expect_identical(ei_updates(b$rec, "inspect_time_unit"), c("ms", "ms"))
    expect_null(units$user)
  })
})

test_that("a guess that changes and changes back before the browser answers still reaches the radio", {
  env <- ei_env()
  sec <- ei_data()
  ms  <- transform(sec, time = time * 1000)
  ds  <- shiny::reactiveVal(sec)
  dv  <- shiny::reactiveVal(0L)
  b <- ei_bind(env, dataset = ds, inspect_result = shiny::reactiveVal(NULL),
               start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
               data_version = dv, start_settings = NULL)
  shiny::testServer(b$server, {
    expect_identical(ei_radio(output$ui_inspect, "inspect_time_unit"), "s")
    ei_inputs(session, unit = "s")
    # ms rows on the Start page: "ms" is sent
    ds(ms)
    dv(1L)
    session$flushReact()
    expect_identical(ei_updates(b$rec, "inspect_time_unit"), "ms")
    # Back to s before the browser applies "ms": "s" is sent after it, so
    # the radio does not end on "ms"
    ds(sec)
    dv(2L)
    session$flushReact()
    expect_identical(ei_updates(b$rec, "inspect_time_unit"), c("ms", "s"))
    # The browser applies both, in order: no pick by hand is left
    session$setInputs(inspect_time_unit = "ms")
    session$setInputs(inspect_time_unit = "s")
    expect_null(units$user)
    expect_null(units$sent)
    expect_match(ei_html(output$inspect_time_step), "Frame step 0.01 s (10 ms)", fixed = TRUE)
    # Later guesses still reach the radio
    ds(ms)
    dv(3L)
    session$flushReact()
    expect_identical(ei_updates(b$rec, "inspect_time_unit"), c("ms", "s", "ms"))
    session$setInputs(inspect_time_unit = "ms")
    expect_null(units$user)
    # Twice more before the browser answers; it reports only the last value,
    # which is the value the server already has, so no input event comes
    ds(sec)
    dv(4L)
    session$flushReact()
    ds(ms)
    dv(5L)
    session$flushReact()
    expect_identical(ei_updates(b$rec, "inspect_time_unit"), c("ms", "s", "ms", "s", "ms"))
    expect_null(units$sent)
    expect_null(units$user)
    # A pick by hand after that still counts, and stays when the guess changes
    session$setInputs(inspect_time_unit = "s")
    expect_identical(units$user, "s")
    ds(sec)
    dv(6L)
    session$flushReact()
    ds(ms)
    dv(7L)
    session$flushReact()
    expect_length(ei_updates(b$rec, "inspect_time_unit"), 5)
    expect_identical(units$user, "s")
  })
})

test_that("a time column that is not numeric is named under the radio, and the run shows an error", {
  env <- ei_env()
  for (kind in c("character", "factor")) {
    d <- ei_data()
    d$t_sec <- d$time
    d$time <- if (kind == "factor") factor(d$time) else as.character(d$time)
    ir <- shiny::reactiveVal(NULL)
    b <- ei_bind(env, dataset = shiny::reactiveVal(d), inspect_result = ir,
                 start_cols = NULL, data_version = NULL, start_settings = NULL)
    shiny::testServer(b$server, {
      ei_inputs(session)
      expect_null(insp_step())
      step <- ei_html(output$inspect_time_step)
      expect_match(step, "The time variable (time) is not numeric, so the check cannot run.",
                   fixed = TRUE, label = kind)
      expect_false(grepl("Frame step", step))
      # Run stops with the same reason
      session$setInputs(inspect_button = 1)
      expect_null(ir())
      t <- b$rec$toasts[[length(b$rec$toasts)]]
      expect_identical(t$type, "error")
      expect_match(t$text, "Column 'time' must be numeric.", fixed = TRUE)
      # A numeric time column: the step is back
      session$setInputs(inspect_time_var = "t_sec")
      expect_match(ei_html(output$inspect_time_step), "Frame step 0.01 s (10 ms)", fixed = TRUE)
      session$setInputs(inspect_button = 2)
      expect_s3_class(ir(), "data.frame")
    })
  }
})

test_that("the summary keeps the columns of the last run when a picker changes", {
  env <- ei_env()
  n_f0 <- sum(!is.na(ei_data()$f0))
  b <- ei_bind(env, dataset = shiny::reactiveVal(ei_data()),
               inspect_result = shiny::reactiveVal(NULL),
               start_cols = NULL, data_version = NULL, start_settings = NULL)
  shiny::testServer(b$server, {
    ei_inputs(session)
    session$setInputs(inspect_button = 1)
    before <- ei_html(output$inspect_summary)
    expect_match(before, sprintf("Samples with potential jumps: [0-9]+ of %d samples with f0", n_f0))
    expect_match(before, "Flagged tokens by tone:", fixed = TRUE)
    # Other columns in the pickers: the summary does not change
    session$setInputs(inspect_f0_var = "intensity")
    expect_identical(ei_html(output$inspect_summary), before)
    session$setInputs(inspect_token_var = "speaker", inspect_speaker_var = "tone",
                      inspect_tone_var = "")
    expect_identical(ei_html(output$inspect_summary), before)
  })
})

test_that("the per-tone counts and small groups use each token's tone as the level check read it", {
  env <- ei_env()
  # The tone changes within a token: each token's first row is "x" (a
  # TextGrid boundary row), the other rows "1" (6 tokens) or "2" (4 tokens,
  # too few for the level check). k01 is a level outlier in tone 1.
  set.seed(2)
  d <- do.call(rbind, lapply(1:10, function(i) {
    f  <- 150 + 30 * (i > 6) + cumsum(stats::rnorm(10, 0, 0.3))
    tn <- if (i > 6) "2" else "1"
    data.frame(token = sprintf("k%02d", i), time = round(0.005 + 0.01 * (0:9), 4),
               f0 = f, intensity = 60, speaker = "S", tone = c("x", rep(tn, 9)))
  }))
  d$f0[d$token == "k01"] <- d$f0[d$token == "k01"] * 1.5
  r <- inspect_f0(d)
  expect_true(any(grepl("level too high", r$flag_notes[r$token == "k01"])))
  expect_identical(sum(!duplicated(r$token) & r$flagged_token), 1L)
  b <- ei_bind(env, dataset = shiny::reactiveVal(d), inspect_result = shiny::reactiveVal(NULL),
               start_cols = NULL, data_version = NULL, start_settings = NULL)
  shiny::testServer(b$server, {
    ei_inputs(session)
    session$setInputs(inspect_button = 1)
    html <- ei_html(output$inspect_summary)
    expect_match(html, "<li>1: 1 / 6 (16.7%)</li>", fixed = TRUE)
    expect_false(grepl("<li>x:", html, fixed = TRUE))
    expect_match(html, "Level check skipped for 4 tokens in 1 speaker \u00d7 tone group",
                 fixed = TRUE)
  })
  # Without "level_groups" the small groups are counted here, with the same
  # token-level tone
  b2 <- ei_bind(env, dataset = shiny::reactiveVal(d), inspect_result = shiny::reactiveVal(NULL),
                start_cols = NULL, data_version = NULL, start_settings = NULL)
  real <- inspect_f0
  environment(b2$server)$inspect_f0 <- function(...) {
    out <- real(...)
    attr(out, "level_groups") <- NULL
    out
  }
  shiny::testServer(b2$server, {
    ei_inputs(session)
    session$setInputs(inspect_button = 1)
    html <- ei_html(output$inspect_summary)
    expect_match(html, "Level check skipped for 4 tokens in 1 speaker \u00d7 tone group",
                 fixed = TRUE)
    expect_match(html, "<li>1: 1 / 6 (16.7%)</li>", fixed = TRUE)
  })
})

test_that("a hand-picked unit survives a Start-page change but not a new upload (full server)", {
  s <- start_server()
  # The real Inspect module in place of the stub, with dplyr's exports (the
  # app attaches dplyr) and the recorder for the radio's updates
  for (nm in getNamespaceExports("dplyr"))
    if (!exists(nm, envir = s$env, inherits = FALSE))
      assign(nm, getExportedValue("dplyr", nm), envir = s$env)
  sys.source(file.path(system.file("app", package = "shinytone"), "ui", "inspect_ui.R"),
             s$env, keep.source = FALSE)
  rec <- ei_recorder()
  s$env$updateRadioButtons <- rec$fns$updateRadioButtons
  f <- start_write_csv(start_padded_fixture())
  shiny::testServer(s$server, {
    session$setInputs(uploadfile = start_upload(f, "padded.csv"))
    expect_identical(ei_radio(output$ui_inspect, "inspect_time_unit"), "s")
    session$setInputs(inspect_time_unit = "s", inspect_token_var = "token",
                      inspect_time_var = "time")
    expect_match(ei_html(output$inspect_time_step), "Frame step 0.01 s (10 ms)", fixed = TRUE)
    # The user picks ms by hand, then unticks removal on the Start page
    session$setInputs(inspect_time_unit = "ms")
    session$setInputs(screen_remove = FALSE)
    expect_identical(data_version(), 1L)
    expect_length(ei_updates(rec, "inspect_time_unit"), 0)
    expect_match(ei_html(output$inspect_time_step),
                 "Frame step 0.01 ms. The times look like seconds.", fixed = TRUE)
    # A new upload: back to the data's unit
    session$setInputs(uploadfile = start_upload(f, "again.csv"))
    expect_identical(ei_updates(rec, "inspect_time_unit"), "s")
    session$setInputs(inspect_time_unit = "s")   # the browser applies it
    expect_match(ei_html(output$inspect_time_step), "Frame step 0.01 s (10 ms)", fixed = TRUE)
  })
})

test_that("an error in the run shows a notification, clears the result and keeps the session", {
  env <- ei_env()
  ir <- shiny::reactiveVal(NULL)
  b <- ei_bind(env, dataset = shiny::reactiveVal(ei_data()), inspect_result = ir,
               start_cols = NULL, data_version = NULL, start_settings = NULL)
  shiny::testServer(b$server, {
    ei_inputs(session)
    session$setInputs(inspect_button = 1)
    expect_s3_class(ir(), "data.frame")
    expect_length(b$rec$toasts, 0)

    # A text column as f0: inspect_f0() stops
    session$setInputs(inspect_f0_var = "token")
    expect_no_error(session$setInputs(inspect_button = 2))
    expect_null(ir())
    expect_error(output$inspect_summary, class = "shiny.silent.error")
    t <- b$rec$toasts[[length(b$rec$toasts)]]
    expect_identical(t$type, "error")
    expect_match(t$text, "Inspection failed: Column 'token' must be numeric (f0 in Hz).", fixed = TRUE)

    # The session goes on: a good run works again
    session$setInputs(inspect_f0_var = "f0")
    session$setInputs(inspect_button = 3)
    expect_s3_class(ir(), "data.frame")
    expect_match(ei_html(output$inspect_summary), "samples with f0", fixed = TRUE)
  })

  # Any error in the check (forced here) is caught the same way
  b2 <- ei_bind(env, dataset = shiny::reactiveVal(ei_data()), inspect_result = ir,
                start_cols = NULL, data_version = NULL, start_settings = NULL)
  environment(b2$server)$inspect_f0 <- function(...) stop("forced failure")
  ir(data.frame(old = 1))
  shiny::testServer(b2$server, {
    ei_inputs(session)
    expect_no_error(session$setInputs(inspect_button = 1))
    expect_null(ir())
    t <- b2$rec$toasts[[length(b2$rec$toasts)]]
    expect_identical(t$type, "error")
    expect_identical(t$text, "Inspection failed: forced failure")
    expect_no_error(session$setInputs(inspect_button = 2))
    expect_length(Filter(function(x) x$type == "error", b2$rec$toasts), 2)
  })
})

test_that("Inspect on f0_long.csv shows 5.6% in every version, through the Start page", {
  path <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(path), "test/f0_long.csv not found")
  skip_on_cran()
  raw <- zero_hz_to_na(utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE))
  raw$speaker <- sub("^(dc[0-9]+).*$", "\\1", raw$token, perl = TRUE)
  raw$tone <- sub("^dc[0-9]+[^0-9]+([0-9])s.*$", "\\1", raw$token, perl = TRUE)
  set.seed(1)
  versions <- list(raw = raw, trimmed = trim_to_voiced(raw, min_run = 1),
                   removed = suppressMessages(drop_rows_without_f0(raw)),
                   shuffled = raw[sample(nrow(raw)), ])
  files <- lapply(versions, function(d) {
    f <- tempfile(fileext = ".csv")
    utils::write.csv(d, f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    f
  })
  s <- start_server()
  for (nm in getNamespaceExports("dplyr"))
    if (!exists(nm, envir = s$env, inherits = FALSE))
      assign(nm, getExportedValue("dplyr", nm), envir = s$env)
  sys.source(file.path(system.file("app", package = "shinytone"), "ui", "inspect_ui.R"),
             s$env, keep.source = FALSE)
  rec <- ei_recorder()
  s$env$updateRadioButtons <- rec$fns$updateRadioButtons
  rows <- c(raw = 83301L, trimmed = 52694L, removed = 52431L, shuffled = 83301L)
  shiny::testServer(s$server, {
    for (k in seq_along(versions)) {
      v <- names(versions)[k]
      session$setInputs(uploadfile = start_upload(files[[v]], paste0(v, ".csv")))
      # Keep the rows the version has: removal unticked
      session$setInputs(screen_remove = FALSE)
      expect_identical(nrow(dataset()), rows[[v]], label = v)
      expect_identical(ei_radio(output$ui_inspect, "inspect_time_unit"), "s", label = v)
      ei_inputs(session)
      expect_match(ei_html(output$inspect_time_step), "Frame step 0.01 s (10 ms)", fixed = TRUE)
      session$setInputs(inspect_button = k)
      html <- ei_html(output$inspect_summary)
      expect_match(html, "Samples with potential jumps: 2,940 of 52,431 samples with f0 (5.6%)",
                   fixed = TRUE, label = v)
      expect_match(html, "of 1,853 tokens with f0", fixed = TRUE, label = v)
    }
  })
})
