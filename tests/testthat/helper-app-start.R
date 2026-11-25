# Helpers for the app tests of the Start page (test-app-start.R and
# test-read_text.R): the app's server in shiny::testServer() and fixtures.

# The app's server function with global.R's helpers, start_ui.R and shiny's
# exports in reach, as in the running app, run from the app's folder (give
# files by full path). The other modules are stubs that
# record what the server passes them, and the deferred package loader and
# the URL sync are left out. showNotification() records its messages.
start_server <- function(.local = parent.frame()) {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  # The Start page reads the sample's preview from www/, as in the app
  withr::local_dir(app, .local_envir = .local)
  pkgs <- new.env(parent = globalenv())
  for (nm in getNamespaceExports("shiny")) assign(nm, getExportedValue("shiny", nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  sys.source(file.path(app, "ui", "start_ui.R"), env, keep.source = FALSE)
  rec <- new.env()
  rec$toasts <- list()
  rec$modules <- list()
  env$showNotification <- function(ui, ..., id = NULL, type = "default", duration = 5) {
    rec$toasts[[length(rec$toasts) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                  id = id, type = type)
    invisible(id)
  }
  env$removeNotification <- function(id, ...) {
    rec$toasts[[length(rec$toasts) + 1L]] <- list(text = "", id = id, type = "removed")
    invisible(id)
  }
  analysis <- c("normalised_ui", "visualise_ui", "inspect_ui", "cluster_ui", "curate_ui",
                "model_ui", "gca_ui", "gamm_ui", "summarise_ui")
  for (m in analysis) local({
    name <- m
    env[[name]] <- function(..., start_cols = NULL, data_version = NULL, start_settings = NULL) {
      rec$modules[[name]] <- list(start_cols = start_cols, data_version = data_version,
                                  start_settings = start_settings)
      invisible(NULL)
    }
  })
  for (m in c("about_usage_ui", "view_ui", "checklist_ui", "filename_guide_ui",
              "waveform_guide_ui", "fp_start_ui", "fp_extraction_ui", "fp_correction_ui",
              "fp_praat_script_ui")) {
    env[[m]] <- function(...) invisible(NULL)
  }
  srv_def <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                      identical(e[[2]], as.name("server")),
                    as.list(parse(file.path(app, "server.R"), keep.source = FALSE, encoding = "UTF-8")))
  f <- eval(srv_def[[1]][[3]], env)
  stmts <- as.list(body(f))[-1]
  drop <- vapply(stmts, function(s) {
    txt <- paste(deparse(s), collapse = " ")
    grepl("onFlushed|url_search|updateQueryString", txt)
  }, logical(1))
  body(f) <- as.call(c(as.name("{"), stmts[!drop]))
  list(server = f, rec = rec, env = env)
}

start_upload <- function(path, name = basename(path)) list(datapath = path, name = name)
start_toast_ids <- function(rec) vapply(rec$toasts, function(t) if (is.null(t$id)) "" else t$id, "")
start_toast_text <- function(rec, id) {
  hit <- Filter(function(t) identical(t$id, id) && t$type != "removed", rec$toasts)
  if (length(hit)) hit[[length(hit)]]$text else NULL
}
start_html <- function(x) paste(as.character(x$html), collapse = " ")

# One token of 10 ms frames, with the f0 values given, as in the fixture
# below.
start_frame_tok <- function(tok, f0) {
  data.frame(token = tok, time = round(0.005 + 0.01 * (seq_along(f0) - 1), 4),
             f0 = f0, intensity = 50 + seq_along(f0),
             speaker = substr(tok, 1, 2), tone = "T1",
             stringsAsFactors = FALSE)
}

# A long-format fixture like f0_long.csv: 10 ms frames, rows without f0 at
# both ends of every token and gaps inside some, one token with no f0, and
# 2 samples cut off by a gap at one token's start.
start_padded_fixture <- function() {
  mk <- start_frame_tok
  rbind(
    mk("S1_a", c(NA, NA, 200, 202, 204, NA, 206, 208, 210, NA, NA)),
    mk("S1_b", c(NA, 190, 191, NA, NA, 193, 194, 195, 196, 197, NA)),
    mk("S2_a", c(NA, NA, NA, 150, 151, 152, 153, 154, 155, NA)),
    mk("S2_b", c(NA, NA, NA, NA)),
    mk("S2_c", c(0, 170, 171, 172, NA, NA, 175, 176, 0)))
}

# Equidistant points, as from resampling: 30 tokens of 11 points evenly
# spaced over durations that differ by token, with f0 a straight line in
# time (the column truth keeps every value). Then:
#   p02: point 4 has no f0 (a 1-point gap)
#   p03: points 6-7 (a 2-point gap)
#   p04: points 3-5 (a 3-point gap)
#   p05: point 1 (the first point)
#   p06: point 11 (the last point) and point 5 (a 1-point gap)
#   p07: points 1-2 (the first two)
start_points_fixture <- function(f0_name = "f0") {
  toks <- sprintf("p%02d", 1:30)
  d <- do.call(rbind, lapply(seq_along(toks), function(i) {
    pts <- 1:11
    data.frame(token = toks[i], time = round(0.1 + (0.15 + 0.01 * i) * (pts - 1) / 10, 5),
               f0 = 180 + i + 2 * (pts - 1), speaker = "s1", tone = "T1",
               stringsAsFactors = FALSE)
  }))
  d$truth <- d$f0
  miss <- list(p02 = 4, p03 = 6:7, p04 = 3:5, p05 = 1, p06 = c(5, 11), p07 = 1:2)
  for (tk in names(miss)) d$f0[d$token == tk][miss[[tk]]] <- NA
  names(d)[names(d) == "f0"] <- f0_name
  d
}

# The user's equal-point file: 12 tokens of 11 equidistant points, with
# durations that differ by token and f0 a straight line in time. Then:
#   p02: point 5 has no f0 (1 point inside)
#   p03: points 6-7 (2 points inside)
#   p04: points 4-6 (3 points inside)
#   p05: point 1 (the first point)
#   p06: f0 at points 5-6 only (only 2 points with f0)
# 16 rows without f0 in all, and 5 tokens flagged (55 rows).
start_user_fixture <- function() {
  toks <- sprintf("p%02d", 1:12)
  d <- do.call(rbind, lapply(seq_along(toks), function(i) {
    pts <- 1:11
    data.frame(token = toks[i], time = round((0.18 + 0.01 * i) * (pts - 1) / 10, 4),
               f0 = 180 + i + 3 * (pts - 1), speaker = c("s1", "s2")[(i - 1) %% 2 + 1],
               stringsAsFactors = FALSE)
  }))
  miss <- list(p02 = 5, p03 = 6:7, p04 = 4:6, p05 = 1, p06 = c(1:4, 7:11))
  for (tk in names(miss)) d$f0[d$token == tk][miss[[tk]]] <- NA
  d
}

start_write_csv <- function(df) {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(df, f, row.names = FALSE, na = "")
  f
}

