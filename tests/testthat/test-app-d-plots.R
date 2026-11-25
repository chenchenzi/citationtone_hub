# Part D: lines in the Curate and Visualise plots break at the gaps that
# voiced_run marks (inst/app/ui/curate_ui.R, visualise_ui.R).
# - Curate groups its lines by token and voiced run. Its select, click and
#   hover stay keyed by the token in customdata.
# - Visualise, aligned by a TextGrid tier: each part from its first f0 to its last,
#   by normalise_time_landmarks(), in the plot and the R code. Lines are
#   grouped by token, part and voiced run in both layouts. The token is the
#   Start page's token column (D8). Its R code builds the same groups.

# global.R's definitions and the two modules, with shiny's, dplyr's and
# ggplot2's exports in reach, as in the running app.
dp_env <- function() {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  pkgs <- new.env(parent = globalenv())
  for (p in c("shiny", "dplyr", "ggplot2"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  for (f in c("curate_ui.R", "visualise_ui.R"))
    sys.source(file.path(app, "ui", f), env, keep.source = FALSE)
  env
}

# A module's body bound to its extra arguments, for shiny::testServer().
dp_bind <- function(env, name, ...) {
  bound <- function(input, output, session) NULL
  body(bound) <- body(env[[name]])
  environment(bound) <- list2env(list(...), parent = env)
  bound
}

# Curate's events are read before the plot is drawn, so plotly warns that
# they are not registered. Those warnings only are muffled.
dp_quiet <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("is not registered", conditionMessage(w), fixed = TRUE))
      invokeRestart("muffleWarning")
  })
}

# Curate bound to data `d`, with the Start page's columns.
dp_curate <- function(env, d) {
  dp_bind(env, "curate_ui", dataset_in = shiny::reactiveVal(d),
          normalised_data = shiny::reactiveVal(NULL), curated_data = shiny::reactiveVal(NULL),
          inspect_result = shiny::reactiveVal(NULL), cluster_data = shiny::reactiveVal(NULL),
          curated_norm_data = shiny::reactiveVal(NULL),
          start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
          data_version = shiny::reactiveVal(0L), start_settings = NULL,
          showNotification = function(...) invisible(NULL))
}
# Inside testServer: show every tone (or one), then wait out the debounce.
dp_curate_view <- function(session, tone = "__all__") {
  session$setInputs(curate_token_var = "token", curate_tone_var = "tone",
                    curate_speaker_var = "speaker", curate_f0_var = "f0",
                    curate_item_var = "__none__", curate_view_tone = tone,
                    curate_view_speaker = "__all__")
  Sys.sleep(0.15); session$elapse(150); session$flushReact()
}

# The traces of a rendered plotly output.
dp_traces <- function(out) jsonlite::fromJSON(out, simplifyVector = FALSE)$x$data
dp_num <- function(l) vapply(l, function(v) if (is.null(v)) NA_real_ else as.numeric(v), 0)
dp_chr <- function(l) vapply(l, function(v) if (is.null(v)) NA_character_ else as.character(v), "")
# The pieces the line traces draw: each run of points with x and y, one row
# per point, with the trace's name (the status colour) and its customdata.
dp_pieces <- function(traces) {
  out <- list()
  for (tr in traces) {
    if (!grepl("lines", tr$mode, fixed = TRUE)) next
    x <- dp_num(tr$x); y <- dp_num(tr$y); cd <- dp_chr(tr$customdata)
    ok <- !is.na(x) & !is.na(y)
    r <- rle(ok); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
    for (k in which(r$values)) {
      i <- starts[k]:ends[k]
      out[[length(out) + 1]] <- data.frame(piece = length(out) + 1L, token = cd[i], time = x[i],
                                            f0 = y[i], status = tr$name, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}
# One row per piece: its token, first and last time and points, as text.
dp_piece_keys <- function(P) {
  s <- lapply(split(P, P$piece), function(p)
    sprintf("%s %.6f %.6f %d", p$token[1], min(p$time), max(p$time), nrow(p)))
  sort(unlist(s, use.names = FALSE))
}
# The segments a piece draws between two voiced runs (looked up in `ref` by
# token and time), and between two tokens.
dp_bridges <- function(P, ref) {
  k  <- function(tok, t) paste(tok, sprintf("%.9f", t))
  vr <- ref$voiced_run[match(k(P$token, P$time), k(as.character(ref$token), ref$time))]
  same <- P$piece[-1] == P$piece[-nrow(P)]
  i <- which(same & vr[-1] != vr[-nrow(P)])
  list(n = length(i), tokens = sum(same & P$token[-1] != P$token[-nrow(P)]),
       st = abs(12 * log2(P$f0[i + 1] / P$f0[i])), lookup_na = sum(is.na(vr)))
}

# Long-format f0 like f0_long.csv: 10 ms frames, rows without f0 at the
# edges, a 2-row gap in S1_a, a 1-row gap in S2_a, none in S1_b.
dp_raw <- function() {
  mk <- function(tok, f0, tone) data.frame(token = tok, time = round(0.005 + 0.01 * (seq_along(f0) - 1), 4),
                                           f0 = f0, speaker = substr(tok, 1, 2), tone = tone,
                                           stringsAsFactors = FALSE)
  rbind(mk("S1_a", c(NA, 200, 202, 204, NA, NA, 230, 232, 234, NA), "T1"),
        mk("S1_b", c(NA, 190, 191, 192, 193, 194, 195, NA), "T1"),
        mk("S2_a", c(NA, 150, 151, NA, 160, 161, 162, NA), "T2"))
}

test_that("Curate's lines break at the gaps voiced_run marks, keyed by the token", {
  skip_if_not_installed("plotly")
  env <- dp_env()
  raw <- dp_raw()
  d   <- suppressMessages(drop_rows_without_f0(raw))
  expect_identical(d$voiced_run[d$token == "S1_a"], c(1L, 1L, 1L, 2L, 2L, 2L))

  dp_quiet(shiny::testServer(dp_curate(env, d), {
    dp_curate_view(session)
    tr <- dp_traces(output$curate_plot)
    P  <- dp_pieces(tr)
    # One piece per token and run: S1_a 2, S1_b 1, S2_a 2
    expect_identical(length(unique(P$piece)), 5L)
    expect_identical(as.vector(table(tapply(P$token, P$piece, `[`, 1))[c("S1_a", "S1_b", "S2_a")]),
                     c(2L, 1L, 2L))
    b <- dp_bridges(P, d)
    expect_identical(b$lookup_na, 0L)
    expect_identical(b$n, 0L)
    expect_identical(b$tokens, 0L)
    # Each piece's customdata is still its token, on the lines and the points
    expect_true(all(tapply(P$token, P$piece, function(x) length(unique(x))) == 1))
    pts <- Filter(function(t) grepl("markers", t$mode, fixed = TRUE), tr)
    cd  <- unlist(lapply(pts, function(t) dp_chr(t$customdata)))
    expect_setequal(cd, unique(d$token))
    expect_identical(length(cd), nrow(d))

    # A box selection that catches only the second piece of S1_a selects the
    # token: both its pieces are drawn as selected
    sel <- data.frame(curveNumber = 0L, pointNumber = 3L, x = 0.075, y = 232, customdata = "S1_a")
    session$setInputs(`plotly_selected-curate_plot` = as.character(jsonlite::toJSON(sel)))
    expect_identical(rv_selection(), "S1_a")
    P2 <- dp_pieces(dp_traces(output$curate_plot))
    hit <- P2[P2$status == "selected", ]
    expect_identical(unique(hit$token), "S1_a")
    expect_identical(length(unique(hit$piece)), 2L)
    expect_identical(sort(hit$time), sort(d$time[d$token == "S1_a"]))
    # A click on a point of S2_a's first piece adds S2_a; a click on
    # S1_a's first piece takes S1_a out again
    clk <- data.frame(curveNumber = 1L, pointNumber = 0L, x = 0.015, y = 150, customdata = "S2_a")
    session$setInputs(`plotly_click-curate_plot` = as.character(jsonlite::toJSON(clk)))
    expect_setequal(rv_selection(), c("S1_a", "S2_a"))
    clk <- data.frame(curveNumber = 0L, pointNumber = 0L, x = 0.015, y = 200, customdata = "S1_a")
    session$setInputs(`plotly_click-curate_plot` = as.character(jsonlite::toJSON(clk)))
    expect_identical(rv_selection(), "S2_a")
    # Hover shows the token only, on every point
    for (t in pts) {
      expect_identical(t$hoverinfo, "text")
      txt <- unname(dp_chr(t$text))
      cdt <- unname(dp_chr(t$customdata))
      if (length(txt) == 1) txt <- rep(txt, length(cdt))   # plotly writes one text for all
      expect_identical(txt, paste0(".tok: ", cdt))
    }
  }))
})

test_that("Curate without voiced_run groups by token, and rows kept draw the same pieces", {
  skip_if_not_installed("plotly")
  env <- dp_env()
  raw <- dp_raw()
  d   <- suppressMessages(drop_rows_without_f0(raw))

  pieces_of <- function(data) {
    got <- new.env()
    dp_quiet(shiny::testServer(dp_curate(env, data), {
      dp_curate_view(session)
      got$P <- dp_pieces(dp_traces(output$curate_plot))
    }))
    got$P
  }
  # The rows removed but no voiced_run (as before Part D): one line per
  # token, which joins the samples on either side of each gap
  token_only <- pieces_of(d[setdiff(names(d), "voiced_run")])
  expect_identical(length(unique(token_only$piece)), 3L)
  b <- dp_bridges(token_only, d)
  expect_identical(b$n, 2L)
  expect_equal(b$st, abs(12 * log2(c(230 / 204, 160 / 151))))

  # The rows kept: the lines break at the rows without f0, the same pieces as
  # the rows removed with voiced_run
  kept    <- pieces_of(raw)
  removed <- pieces_of(d)
  expect_identical(dp_piece_keys(kept), dp_piece_keys(removed))
  expect_identical(nrow(kept), nrow(d))

  # A voiced_run of NA (a row with f0 but no time) is a group of its own, one
  # per token. S1_a and S1_b are both T1, so their NA runs share a panel:
  # paste() keeps them apart, where interaction() would make them one group
  # and draw a segment from one token to the other.
  d2 <- d
  d2$voiced_run[d2$token == "S1_b"][3] <- NA
  d2$voiced_run[d2$token == "S1_a"][2] <- NA
  P <- pieces_of(d2)
  expect_identical(sum(P$token == "S1_b"), sum(d2$token == "S1_b"))
  expect_identical(nrow(P), nrow(d2))
  expect_identical(dp_bridges(P, d2)$tokens, 0L)
  expect_true(all(tapply(P$token, P$piece, function(x) length(unique(x))) == 1))
})

test_that("on f0_long.csv after removal, Curate's All tones view draws no line across a gap", {
  skip_if_not_installed("plotly")
  f <- test_path("..", "..", "test", "f0_long.csv")
  skip_if_not(file.exists(f), "test/f0_long.csv not found")
  env <- dp_env()
  raw <- zero_hz_to_na(read_csv_utf8(f, stringsAsFactors = FALSE,
                                     na.strings = c("NA", "--undefined--")))
  raw$tone    <- sub("^dc[0-9]+[^0-9]+([0-9])s[0-9.]+$", "\\1", raw$token, perl = TRUE)
  raw$speaker <- sub("^(dc[0-9]+).*$", "\\1", raw$token)
  expect_setequal(unique(raw$tone), as.character(1:6))
  d <- suppressMessages(drop_rows_without_f0(raw, token = "token", time = "time", f0 = "f0"))
  expect_identical(nrow(d), 52431L)
  expect_identical(sum(tapply(d$voiced_run, d$token, max) - 1L), 109L)

  pieces_of <- function(data) {
    got <- new.env()
    dp_quiet(shiny::testServer(dp_curate(env, data), {
      dp_curate_view(session)
      got$P <- dp_pieces(dp_traces(output$curate_plot))
    }))
    got$P
  }
  fixed <- pieces_of(d)
  b <- dp_bridges(fixed, d)
  expect_identical(b$lookup_na, 0L)
  expect_identical(b$n, 0L)
  expect_identical(length(unique(fixed$piece)), 1853L + 109L)

  # Grouped by token only, the same data draw 109 bridges, a median jump of
  # about 7.3 semitones
  old <- dp_bridges(pieces_of(d[setdiff(names(d), "voiced_run")]), d)
  expect_identical(old$n, 109L)
  expect_identical(old$tokens, 0L)
  expect_equal(stats::median(old$st), 7.3, tolerance = 0.01)

  # The rows kept (removal unticked): the same pieces
  expect_identical(dp_piece_keys(pieces_of(raw)), dp_piece_keys(fixed))
})

# ---- Visualise, aligned by a TextGrid tier ----------------------------------

# Two-syllable tokens: syllable 1 is 0-0.1 s, syllable 2 is 0.1-0.2 s, 10 ms
# frames. A has a gap inside syllable 1, B one across the boundary, C none.
# filename names the token too.
dp_lm_raw <- function() {
  mk <- function(tok, tone, gap) {
    t  <- round(0.005 + 0.01 * (0:19), 4)
    f0 <- 180 + 2 * (0:19) + if (tone == "T2") 30 else 0
    f0[gap] <- NA
    s1 <- t < 0.1
    data.frame(token = tok, filename = paste0(tok, ".wav"), time = t, f0 = f0,
               tone = tone, speaker = "S1",
               syllable_start = ifelse(s1, 0, 0.1), syllable_end = ifelse(s1, 0.1, 0.2),
               syllable_i = ifelse(s1, 1L, 2L), stringsAsFactors = FALSE)
  }
  rbind(mk("A", "T1", c(1, 4, 5, 20)), mk("B", "T2", c(10, 11, 12)), mk("C", "T1", integer(0)))
}

# The Start page's settings for a removal on token column `tok`, as
# server.R's start_settings() gives them.
dp_settings <- function(rd, ds, tok) {
  s <- attr(screen_f0_rows(rd, token = tok, time = "time", f0 = "f0"), "summary")
  list(version = "0.0.1", zero_hz = attr(rd, "zero_hz"), dropped = NULL,
       metadata = NULL, durations = list(),
       screen = list(token = tok, time = "time", f0 = "f0", unvoiced = NULL,
                     action = "remove", min_run = 1, rows = nrow(rd),
                     without = s[["rows_without_f0"]], summary = s,
                     rows_after = nrow(ds),
                     gaps_marked = s[["gaps_time_only"]] + s[["gaps_run"]],
                     short_rows = s[["short_rows"]]))
}

# The R code a "Show R code" panel shows, as lines.
dp_code <- function(ui) {
  html <- paste(as.character(ui$html), collapse = "")
  code <- regmatches(html, regexpr("(?s)<code>.*</code>", html, perl = TRUE))
  code <- sub("^<code>", "", sub("</code>$", "", code))
  for (p in list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&#39;", "'"),
                 c("&amp;", "&"))) code <- gsub(p[1], p[2], code, fixed = TRUE)
  strsplit(code, "\n", fixed = TRUE)[[1]]
}

# The labels of the vis_align_layout radio in a rendered UI, in order.
dp_radio_labels <- function(html) {
  m <- regmatches(html, gregexpr(paste0('name="vis_align_layout" value="[^"]*"( checked="checked")?/>',
                                        '\\s*<span>[^<]*</span>'), html))[[1]]
  sub("^.*<span>([^<]*)</span>$", "\\1", m)
}

# The Plotting guide's item on aligning by a tier, its spaces collapsed.
dp_vis_guide_item <- paste(
  "<strong>Align by TextGrid tier (optional):</strong> Appears when the data have TextGrid tier",
  "columns (e.g. <code>syllable_start</code> / <code>syllable_end</code>, added by F0 Extraction",
  "when you choose TextGrid tiers). Pick a tier, and the X axis is rebuilt from it. <em>In",
  "sequence</em> puts syllable 1 at 0 to 1, syllable 2 at 1 to 2, and so on, each from its first",
  "f0 to its last. <em>In sequence, gap kept</em> puts the TextGrid boundaries at 1, 2, ..., with",
  "the token&rsquo;s first f0 at 0 and its last f0 at the end. <em>Overlaid</em> puts every",
  "syllable on one 0&ndash;1 axis, each from its first f0 to its last. Rows with f0 are read from",
  "the Start page&rsquo;s f0 variable.",
  "When Normalise uses the same f0 variable, these axes equal <code>&lt;tier&gt;_tseq</code>,",
  "<code>&lt;tier&gt;_tbound</code> and <code>&lt;tier&gt;_t01</code>.")

# Draw the aligned plot in each layout; return the plot data, the line
# layer's groups and the R code's data (run up to the plot) for each. `y` is
# the Y variable, `x` the X (time) variable.
dp_vis_run <- function(env, ds, csv, start_cols = NULL, settings = NULL, y = "f0",
                       layouts = c("sequential", "overlay"), x = "time") {
  srv <- dp_bind(env, "visualise_ui", dataset = shiny::reactiveVal(ds),
                 normalised_data = shiny::reactiveVal(NULL), curated_norm_data = NULL,
                 start_cols = start_cols,
                 start_settings = if (!is.null(settings)) shiny::reactive(settings))
  got <- new.env()
  shiny::testServer(srv, {
    # The code button toggles the code, so it is pressed once
    session$setInputs(show_code_button = 1)
    for (lay in layouts) {
      session$setInputs(dataset_name = sub("\\.csv$", "", csv), x_var = x, y_var = y,
                        tone_var = "tone", speaker_var = "speaker",
                        convert_tone_to_factor = FALSE, plot_facet = "none",
                        vis_align_by = "syllable", vis_align_layout = lay,
                        plot_button = 1)
      p    <- current_plot()
      code <- dp_code(output$r_code_output)
      at   <- grep("# Create the plot", code, fixed = TRUE)
      run  <- new.env()
      # The package's notes (a part on another clock, no <set>_i) stay quiet
      suppressMessages(eval(parse(text = code[3:(at - 1)]), run))
      li <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), NA))
      vl <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomVline"), NA))
      got[[lay]] <- list(data = p$data, code = code, dat = run$dat,
                         vlines = if (length(vl)) ggplot2::layer_data(p, vl)$xintercept,
                         line_groups = length(unique(ggplot2::layer_data(p, li)$group)),
                         line_map = rlang::as_label(p$layers[[li]]$mapping$group),
                         lines = ggplot2::layer_data(p, li), x_lab = p$labels$x,
                         x_labels = p$scales$get_scales("x")$labels)
    }
  })
  as.list(got)
}

test_that("Visualise's aligned lines break at the gaps in both layouts, as its R code does", {
  env <- dp_env()
  raw <- dp_lm_raw()
  f <- tempfile(fileext = ".csv")
  utils::write.csv(raw, f, row.names = FALSE, na = "")
  on.exit(unlink(f))
  rd <- zero_hz_to_na(read_csv_utf8(f, stringsAsFactors = FALSE,
                                    na.strings = c("NA", "--undefined--")))
  ds <- suppressMessages(drop_rows_without_f0(rd, token = "token", time = "time", f0 = "f0"))
  expect_identical(c(tapply(ds$voiced_run, ds$token, max)), c(A = 2L, B = 2L, C = 1L))

  got <- dp_vis_run(env, ds, f,
                    start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
                    settings = dp_settings(rd, ds, "token"))
  for (lay in c("sequential", "overlay")) {
    g <- got[[lay]]
    pd <- g$data
    expect_identical(g$line_map, ".grp")
    # No group spans two runs, two tokens or two parts
    expect_true(all(tapply(pd$voiced_run, pd$.grp, function(x) length(unique(x))) == 1))
    expect_true(all(tapply(pd$token, pd$.grp, function(x) length(unique(x))) == 1))
    expect_true(all(tapply(pd$syllable_i, pd$.grp, function(x) length(unique(x))) == 1))
    # The line layer draws one line per group
    expect_identical(g$line_groups, length(unique(pd$.grp)))
    # The R code's data and groups are the plot's
    expect_true(any(grepl("dat <- shinytone::drop_rows_without_f0(dat", g$code, fixed = TRUE)))
    expect_identical(nrow(g$dat), nrow(pd))
    expect_equal(g$dat$aligned_x, pd$.aligned_x)
    expect_identical(g$dat$grp, pd$.grp)
  }
  # Both layouts: per token, part and run (part.run). A: 1.1, 1.2, 2.2; B:
  # 1.1, 2.2; C: 1.1, 2.1. In sequence, part 1's last f0 and part 2's first
  # f0 both sit at 1, so a line across parts would only add a vertical join.
  for (lay in c("sequential", "overlay")) {
    expect_identical(length(unique(got[[lay]]$data$.grp)), 7L)
    expect_true(any(got[[lay]]$code == "dat$grp <- paste(dat$token, dat$syllable_i, dat$voiced_run)"))
  }
})

test_that("Visualise joins the Start page's token, and leaves voiced_run out when there is none", {
  env <- dp_env()
  raw <- dp_lm_raw()
  f <- tempfile(fileext = ".csv")
  utils::write.csv(raw, f, row.names = FALSE, na = "")
  on.exit(unlink(f))
  rd <- zero_hz_to_na(read_csv_utf8(f, stringsAsFactors = FALSE,
                                    na.strings = c("NA", "--undefined--")))

  # The Start page's token is filename (vis_token_col() would pick token)
  ds <- suppressMessages(drop_rows_without_f0(rd, token = "filename", time = "time", f0 = "f0"))
  expect_identical(env$vis_token_col(names(ds)), "token")
  got <- dp_vis_run(env, ds, f,
                    start_cols = shiny::reactiveVal(list(token = "filename", time = "time", f0 = "f0")),
                    settings = dp_settings(rd, ds, "filename"))
  expect_true(all(startsWith(got$sequential$data$.grp, got$sequential$data$filename)))
  for (lay in c("sequential", "overlay"))
    expect_true(any(got[[lay]]$code ==
                      "dat$grp <- paste(dat$filename, dat$syllable_i, dat$voiced_run)"))
  for (lay in c("sequential", "overlay"))
    expect_identical(got[[lay]]$dat$grp, got[[lay]]$data$.grp)

  # A Start token the data do not have: the first token-like column, as before
  got <- dp_vis_run(env, ds, f,
                    start_cols = shiny::reactiveVal(list(token = "id", time = "time", f0 = "f0")),
                    settings = dp_settings(rd, ds, "filename"))
  expect_true(any(got$sequential$code == "dat$grp <- paste(dat$token, dat$syllable_i, dat$voiced_run)"))
  expect_identical(got$sequential$dat$grp, got$sequential$data$.grp)

  # No voiced_run (every row has f0, no gaps): groups as before, no
  # voiced_run in the code
  full <- rd[!rd$token %in% c("A", "B"), ]
  f2 <- tempfile(fileext = ".csv")
  utils::write.csv(full, f2, row.names = FALSE, na = "")
  on.exit(unlink(f2), add = TRUE)
  got <- dp_vis_run(env, full, f2,
                    start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")))
  expect_false("voiced_run" %in% names(got$sequential$data))
  expect_false(any(grepl("voiced_run", c(got$sequential$code, got$overlay$code))))
  for (lay in c("sequential", "overlay"))
    expect_true(any(got[[lay]]$code == "dat$grp <- paste(dat$token, dat$syllable_i)"))
  for (lay in c("sequential", "overlay"))
    expect_identical(got[[lay]]$dat$grp, got[[lay]]$data$.grp)
})

test_that("Visualise keeps a voiced_run of NA as a group of its own, one per token", {
  env <- dp_env()
  raw <- dp_lm_raw()
  f <- tempfile(fileext = ".csv")
  utils::write.csv(raw, f, row.names = FALSE, na = "")
  on.exit(unlink(f))
  rd <- zero_hz_to_na(read_csv_utf8(f, stringsAsFactors = FALSE,
                                    na.strings = c("NA", "--undefined--")))
  ds <- suppressMessages(drop_rows_without_f0(rd, token = "token", time = "time", f0 = "f0"))
  # An NA run in A and in C (both T1, both in syllable 2)
  ds$voiced_run[ds$token == "A"][12] <- NA
  ds$voiced_run[ds$token == "C"][15] <- NA

  got <- dp_vis_run(env, ds, f,
                    start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
                    settings = dp_settings(rd, ds, "token"))
  for (lay in c("sequential", "overlay")) {
    pd <- got[[lay]]$data
    expect_identical(sum(is.na(pd$voiced_run)), 2L)
    expect_false(anyNA(pd$.grp))
    expect_true(all(tapply(pd$token, pd$.grp, function(x) length(unique(x))) == 1))
    # One line per token, part and run, the NA runs included
    key <- paste(pd$token, pd$syllable_i, pd$voiced_run)
    expect_identical(got[[lay]]$line_groups, length(unique(key)))
  }
})

test_that("Visualise's R code drops a row in a part with one f0 point, as the plot does", {
  env <- dp_env()
  raw <- dp_lm_raw()
  # Two frames of C in syllable 2 get a part of zero length, each its own
  # part with one f0 point, so no span to run from 0 to 1: one at 0.2 s, away
  # from the frame's time, one at the frame's own time
  i <- which(raw$token == "C")[15]
  j <- which(raw$token == "C")[16]
  raw$syllable_start[i] <- raw$syllable_end[i] <- 0.2
  raw$syllable_start[j] <- raw$syllable_end[j] <- raw$time[j]
  f <- tempfile(fileext = ".csv")
  utils::write.csv(raw, f, row.names = FALSE, na = "")
  on.exit(unlink(f))
  rd <- zero_hz_to_na(read_csv_utf8(f, stringsAsFactors = FALSE,
                                    na.strings = c("NA", "--undefined--")))
  ds <- suppressMessages(drop_rows_without_f0(rd, token = "token", time = "time", f0 = "f0"))

  got <- dp_vis_run(env, ds, f,
                    start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
                    settings = dp_settings(rd, ds, "token"))
  for (lay in c("sequential", "overlay")) {
    g <- got[[lay]]
    # The plot drops both frames; so does the code, with the same x and groups
    expect_identical(nrow(g$data), nrow(ds) - 2L)
    expect_false(any(g$data$token == "C" & g$data$time %in% raw$time[c(i, j)]))
    expect_identical(nrow(g$dat), nrow(g$data))
    expect_equal(g$dat$aligned_x, g$data$.aligned_x)
    expect_identical(g$dat$grp, g$data$.grp)
    expect_true(any(grepl("al <- shinytone::normalise_time_landmarks(dat, time = \"time\", set = \"syllable\",",
                          g$code, fixed = TRUE)))
  }
})

# The segments a line layer draws: consecutive points of a group, both with
# a y, as "x0 y0 x1 y1" text.
dp_segments <- function(ld) {
  out <- character(0)
  for (g in split(ld, ld$group)) {
    g <- g[order(g$x), ]
    if (nrow(g) < 2) next
    k <- which(is.finite(g$y[-1]) & is.finite(g$y[-nrow(g)]))
    out <- c(out, sprintf("%.6f %.4f %.6f %.4f", g$x[k], g$y[k], g$x[k + 1], g$y[k + 1]))
  }
  sort(out)
}

test_that("Visualise aligns each part from its first f0 to its last, with or without the rows without f0", {
  env <- dp_env()
  raw <- dp_lm_raw()
  raw$intensity <- 60 + (seq_len(nrow(raw)) %% 7)
  f <- tempfile(fileext = ".csv")
  utils::write.csv(raw, f, row.names = FALSE, na = "")
  on.exit(unlink(f))
  rd <- zero_hz_to_na(read_csv_utf8(f, stringsAsFactors = FALSE,
                                    na.strings = c("NA", "--undefined--")))
  ds <- suppressMessages(drop_rows_without_f0(rd, token = "token", time = "time", f0 = "f0"))
  sc <- shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0"))
  # Rows kept: the 2 rows without f0 inside A's syllable 1 have an x and no
  # y, which ggplot2 leaves out of the points with a warning
  kept <- withCallingHandlers(dp_vis_run(env, rd, f, start_cols = sc), warning = function(w) {
    if (grepl("Removed 2 rows containing missing values", conditionMessage(w), fixed = TRUE))
      invokeRestart("muffleWarning")
  })
  removed <- dp_vis_run(env, ds, f, start_cols = sc, settings = dp_settings(rd, ds, "token"))
  key <- function(d) paste(d$token, sprintf("%.4f", d$time))
  for (lay in c("sequential", "overlay")) {
    for (g in list(kept[[lay]], removed[[lay]])) {
      pd <- g$data[is.finite(g$data$f0), ]
      # Each part runs from its first f0 to its last: i - 1 to i in
      # sequence, 0 to 1 overlaid (the old axis gave A1 0.15-0.95, B2 0.25-0.95)
      lo <- c(tapply(pd$.aligned_x, paste(pd$token, pd$syllable_i), min))
      hi <- c(tapply(pd$.aligned_x, paste(pd$token, pd$syllable_i), max))
      i  <- as.integer(sub("^.* ", "", names(lo)))
      off <- if (lay == "sequential") i - 1 else rep(0, length(i))
      expect_equal(unname(lo), off)
      expect_equal(unname(hi), off + 1)
      # The code calls the same function, with the Start page's f0 and token
      expect_true(any(g$code == '                                          f0 = "f0", token = "token")'))
      expect_identical(nrow(g$dat), nrow(g$data))
      expect_equal(g$dat$aligned_x, g$data$.aligned_x)
      expect_identical(g$dat$grp, g$data$.grp)
    }
    # Rows with f0 sit at the same place with the rows without f0 kept or removed
    k <- kept[[lay]]$data[is.finite(kept[[lay]]$data$f0), ]
    r <- removed[[lay]]$data
    expect_identical(k$.aligned_x[match(key(r), key(k))], r$.aligned_x)
    # ... and the lines draw the same segments (kept: broken at NA f0;
    # removed: grouped by voiced run)
    expect_identical(dp_segments(kept[[lay]]$lines), dp_segments(removed[[lay]]$lines))
  }
  expect_identical(kept$sequential$x_lab, "syllables in sequence, first to last f0 in each")
  # In sequence: each syllable's name at its middle, as in the R code; the
  # boundaries at 1, 2, ... are the dashed lines
  expect_identical(kept$sequential$x_labels[1:2], c("syllable 1", "syllable 2"))
  expect_true(any(grepl('labels = paste("syllable", 1:', kept$sequential$code, fixed = TRUE)))
  expect_true(any(grepl('# (rows with f0 in column "f0")', kept$sequential$code, fixed = TRUE)))
  # The code's comment says what <tier>_tseq is, not the 0-1 time within each
  expect_true(any(kept$sequential$code == paste("# The syllables in sequence: syllable 1 at 0 to 1,",
                                                "syllable 2 at 1 to 2, and so on,")))
  expect_true(any(kept$sequential$code == "# each from its first f0 to its last"))
  expect_false(any(grepl("# Time within each", kept$sequential$code, fixed = TRUE)))
  expect_true(any(kept$overlay$code ==
                    "# Time within each syllable: 0 at its first f0 and 1 at its last f0"))
  expect_identical(kept$overlay$x_lab, "time within each syllable, first to last f0 (0\u20131)")

  # Y is intensity: which rows have f0 still comes from the Start page's f0
  int <- dp_vis_run(env, ds, f, start_cols = sc, settings = dp_settings(rd, ds, "token"),
                    y = "intensity")
  for (lay in c("sequential", "overlay")) {
    expect_true(any(int[[lay]]$code == '                                          f0 = "f0", token = "token")'))
    expect_equal(int[[lay]]$data$.aligned_x, removed[[lay]]$data$.aligned_x)
    expect_equal(int[[lay]]$dat$aligned_x, int[[lay]]$data$.aligned_x)
  }
  # A Start f0 the data do not have: the Y variable decides, in both
  int <- dp_vis_run(env, rd, f, start_cols = shiny::reactiveVal(list(token = "token", time = "time",
                                                                     f0 = "pitch")),
                    y = "intensity")
  for (lay in c("sequential", "overlay")) {
    expect_true(any(int[[lay]]$code == '                                          f0 = "intensity", token = "token")'))
    expect_identical(nrow(int[[lay]]$dat), nrow(int[[lay]]$data))
    expect_equal(int[[lay]]$dat$aligned_x, int[[lay]]$data$.aligned_x)
  }
})

test_that("Visualise's third layout puts the boundaries at 1, 2, ... with the gap kept, as its R code does", {
  env <- dp_env()
  raw <- dp_lm_raw()
  f <- tempfile(fileext = ".csv")
  utils::write.csv(raw, f, row.names = FALSE, na = "")
  on.exit(unlink(f))
  rd <- zero_hz_to_na(read_csv_utf8(f, stringsAsFactors = FALSE,
                                    na.strings = c("NA", "--undefined--")))
  ds <- suppressMessages(drop_rows_without_f0(rd, token = "token", time = "time", f0 = "f0"))
  sc <- shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0"))
  # Rows kept: the rows without f0 inside a token (A's two in syllable 1,
  # B's three in the gap, which keeps its room) have an x and no y, which
  # ggplot2 leaves out with a warning
  kept <- withCallingHandlers(dp_vis_run(env, rd, f, start_cols = sc, layouts = "boundary"),
                              warning = function(w) {
    if (grepl("Removed [0-9]+ rows containing missing values", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })
  removed <- dp_vis_run(env, ds, f, start_cols = sc, settings = dp_settings(rd, ds, "token"),
                        layouts = "boundary")
  ref <- normalise_time_landmarks(rd, "time", "syllable", f0 = "f0", token = "token")
  key <- function(d) paste(d$token, sprintf("%.4f", d$time))
  for (g in list(kept$boundary, removed$boundary)) {
    pd <- g$data
    # The plot's x is syllable_tbound, as Normalise makes it
    expect_equal(pd$.aligned_x, ref$syllable_tbound[match(key(pd), key(ref))])
    # 0 at each token's first f0, 2 at its last; B keeps its gap around 1
    v <- pd[is.finite(pd$f0), ]
    expect_equal(unname(c(tapply(v$.aligned_x, v$token, min))), rep(0, 3))
    expect_equal(unname(c(tapply(v$.aligned_x, v$token, max))), rep(2, 3))
    b <- v[v$token == "B", ]
    expect_equal(max(b$.aligned_x[b$syllable_i == 1]), (0.085 - 0.005) / (0.1 - 0.005))
    expect_equal(min(b$.aligned_x[b$syllable_i == 2]), 1 + (0.125 - 0.1) / (0.195 - 0.1))
    # The axis: the dashed boundary at 1, each syllable named at its middle
    expect_identical(g$x_lab, "syllables in sequence, gap kept at the TextGrid boundaries")
    expect_identical(g$x_labels[1:2], c("syllable 1", "syllable 2"))
    expect_equal(g$vlines, 1)
    # The R code: the same call and column, the same rows and groups
    expect_true(any(g$code == 'dat$aligned_x <- al[["syllable_tbound"]]'))
    expect_true(any(grepl("# The syllables in sequence, gap kept: 0 at the token's first f0, the TextGrid",
                          g$code, fixed = TRUE)))
    expect_true(any(grepl('geom_vline(xintercept = 1:1, linetype = "dashed"', g$code, fixed = TRUE)))
    expect_true(any(grepl('labels = paste("syllable", 1:2))', g$code, fixed = TRUE)))
    expect_true(any(grepl('labs(x = "syllables in sequence, gap kept at the TextGrid boundaries"', g$code,
                          fixed = TRUE)))
    expect_identical(nrow(g$dat), nrow(g$data))
    expect_equal(g$dat$aligned_x, g$data$.aligned_x)
    expect_identical(g$dat$grp, g$data$.grp)
  }
  # Rows with f0 sit at the same place with the rows without f0 kept or removed
  k <- kept$boundary$data[is.finite(kept$boundary$data$f0), ]
  r <- removed$boundary$data
  expect_identical(k$.aligned_x[match(key(r), key(k))], r$.aligned_x)

  # The radio offers the layout, between the other two
  srv <- dp_bind(env, "visualise_ui", dataset = shiny::reactiveVal(rd),
                 normalised_data = shiny::reactiveVal(NULL), curated_norm_data = NULL,
                 start_cols = sc)
  shiny::testServer(srv, {
    html <- paste(as.character(output$ui_visualise$html), collapse = " ")
    vals <- regmatches(html, gregexpr('name="vis_align_layout" value="[^"]*"', html))[[1]]
    expect_identical(sub('.*value="([^"]*)"', "\\1", vals), c("sequential", "boundary", "overlay"))
    # Each label names the tier's column (the only tier here: syllable)
    expect_identical(dp_radio_labels(html),
                     c("In sequence (syllable_tseq)", "In sequence, gap kept (syllable_tbound)",
                       "Overlaid (syllable_t01)"))
    expect_match(html, 'value="sequential" checked="checked"', fixed = TRUE)
    # The hint: one short sentence per layout, in the radio's order, in the
    # tier's words
    hint <- gsub("\\s+", " ", paste(as.character(output$vis_align_hint$html), collapse = " "))
    expect_match(hint, paste("<em>In sequence</em>: syllable 1 at 0 to 1, syllable 2 at 1 to 2, and so",
                             "on, each from its first f0 to its last. <em>In sequence, gap kept</em>: the",
                             "TextGrid boundaries sit at 1, 2, ..., so the gap between syllables keeps its",
                             "room. <em>Overlaid</em>: every syllable on one 0\u20131 axis.</div>"),
                 fixed = TRUE)
    # The second layout has the radio's name, not "Gap kept" alone
    expect_no_match(hint, "<em>Gap kept</em>", fixed = TRUE)
    # No space before a comma, colon or full stop (other than the "..." in "1, 2, ...")
    expect_no_match(hint, " [,:;]| \\.[^.]", perl = TRUE)
    expect_no_match(html, "Rows without f0 before or after that are left out.", fixed = TRUE)
    expect_no_match(html, "Rows without f0 outside these ends are left out.", fixed = TRUE)
    expect_no_match(html, "Side by side|stacked|landmark")
    guide <- paste(as.character(output$visualise_guide$html), collapse = " ")
    expect_match(guide, "<strong>Align by TextGrid tier (optional):</strong>", fixed = TRUE)
    expect_match(gsub("\\s+", " ", guide), dp_vis_guide_item, fixed = TRUE)
    expect_no_match(guide, "Side by side|side by side|stacked|landmark")
  })

  # Without syllable_i the two layouts in sequence fall back to overlaid
  noi <- rd[setdiff(names(rd), "syllable_i")]
  f2 <- tempfile(fileext = ".csv")
  utils::write.csv(noi, f2, row.names = FALSE, na = "")
  on.exit(unlink(f2), add = TRUE)
  got <- withCallingHandlers(dp_vis_run(env, noi, f2, start_cols = sc, layouts = "boundary"),
                             warning = function(w) invokeRestart("muffleWarning"))
  expect_identical(got$boundary$x_lab, "time within each syllable, first to last f0 (0\u20131)")
  expect_true(any(got$boundary$code == 'dat$aligned_x <- al[["syllable_t01"]]'))
})

test_that("Visualise's gap-kept layout says why the plot is empty when time is on another clock", {
  env <- dp_env()
  raw <- dp_lm_raw()
  raw$time_ms <- raw$time * 1000
  sc <- shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0"))
  srv <- dp_bind(env, "visualise_ui", dataset = shiny::reactiveVal(raw),
                 normalised_data = shiny::reactiveVal(NULL), curated_norm_data = NULL,
                 start_cols = sc)
  shiny::testServer(srv, {
    session$setInputs(dataset_name = "ms", x_var = "time_ms", y_var = "f0",
                      tone_var = "tone", speaker_var = "speaker",
                      convert_tone_to_factor = FALSE, plot_facet = "none",
                      vis_align_by = "syllable", vis_align_layout = "boundary", plot_button = 1)
    err <- tryCatch(current_plot(), error = function(e) e)
    expect_s3_class(err, "shiny.silent.error")
    expect_match(conditionMessage(err), "syllable_tbound is NA in 3 token", fixed = TRUE)
    expect_match(conditionMessage(err), "same clock", fixed = TRUE)
    # The plot says it, so the note above the plot stays away
    expect_null(output$vis_align_warning)
    # In sequence needs only the order and spacing of time: it still draws
    # (A's two rows without f0 inside syllable 1 have no y, and are left out)
    withCallingHandlers({
      session$setInputs(vis_align_layout = "sequential")
      expect_s3_class(current_plot(), "ggplot")
      expect_gt(nrow(current_plot()$data), 0)
    }, warning = function(w) {
      if (grepl("Removed 2 rows containing missing values", conditionMessage(w), fixed = TRUE))
        invokeRestart("muffleWarning")
    })
  })
})

test_that("Visualise's gap-kept layout says which tokens it leaves out when others are still drawn", {
  env <- dp_env()
  # A one-syllable token D next to the two-syllable A, B and C: on another
  # clock, D keeps its syllable_tbound (its f0 span sets 0 and 1, so the
  # boundaries are not used) and the others lose theirs
  raw <- dp_lm_raw()
  d1  <- raw[raw$token == "C", ]
  d1$token <- "D"; d1$filename <- "D.wav"
  d1$syllable_start <- 0; d1$syllable_end <- 0.2; d1$syllable_i <- 1L
  raw <- rbind(raw, d1)
  raw$time_ms <- raw$time * 1000
  f <- tempfile(fileext = ".csv")
  utils::write.csv(raw, f, row.names = FALSE, na = "")
  on.exit(unlink(f))
  sc <- shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0"))
  quiet <- function(expr) withCallingHandlers(expr, warning = function(w) {
    if (grepl("Removed [0-9]+ rows containing missing values", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })
  got <- quiet(dp_vis_run(env, raw, f, start_cols = sc, layouts = c("boundary", "sequential"),
                          x = "time_ms"))
  # (In sequence keeps A's and B's rows without f0 inside a token, which
  # have an x and no y: ggplot2 leaves them out with a warning, muffled)
  g <- got$boundary
  expect_identical(unique(as.character(g$data$token)), "D")
  # Only syllable 1 is drawn: no dashed line at 1 and no "syllable 2" over
  # an empty stretch, in the plot and in the R code
  expect_null(g$vlines)
  expect_identical(g$x_labels, "syllable 1")
  expect_false(any(grepl("geom_vline", g$code, fixed = TRUE)))
  expect_true(any(grepl('labels = paste("syllable", 1:1))', g$code, fixed = TRUE)))
  expect_identical(nrow(g$dat), nrow(g$data))
  expect_equal(g$dat$aligned_x, g$data$.aligned_x)
  # In sequence draws every token, with both syllables
  s <- got$sequential
  expect_setequal(unique(as.character(s$data$token)), c("A", "B", "C", "D"))
  expect_equal(s$vlines, 1)
  expect_identical(s$x_labels, c("syllable 1", "syllable 2"))

  # The note above the plot, on the gap-kept layout only
  srv <- dp_bind(env, "visualise_ui", dataset = shiny::reactiveVal(raw),
                 normalised_data = shiny::reactiveVal(NULL), curated_norm_data = NULL,
                 start_cols = sc)
  # (The plot is drawn at each change: on the other layouts, as above)
  quiet(shiny::testServer(srv, {
    session$setInputs(dataset_name = "ms", x_var = "time_ms", y_var = "f0",
                      tone_var = "tone", speaker_var = "speaker",
                      convert_tone_to_factor = FALSE, plot_facet = "none",
                      vis_align_by = "syllable", vis_align_layout = "boundary", plot_button = 1)
    note <- paste(as.character(output$vis_align_warning$html), collapse = " ")
    expect_match(note, "3 tokens are not drawn.", fixed = TRUE)
    expect_match(note, "syllable_tbound is NA in 3 token(s)", fixed = TRUE)
    expect_match(note, "must be on the same clock", fixed = TRUE)
    expect_match(note, "In sequence (syllable_tseq) and Overlaid (syllable_t01) draw them.",
                 fixed = TRUE)
    session$setInputs(vis_align_layout = "sequential")
    expect_null(output$vis_align_warning)
    # Time and the boundaries both in seconds: no note
    session$setInputs(x_var = "time", vis_align_layout = "boundary")
    expect_null(output$vis_align_warning)
    expect_setequal(unique(as.character(current_plot()$data$token)), c("A", "B", "C", "D"))
  }))
})

test_that("Visualise without a token column offers no alignment, with a hint", {
  env <- dp_env()
  raw <- dp_lm_raw()
  names(raw)[names(raw) == "token"] <- "rec"
  raw$filename <- NULL
  srv <- dp_bind(env, "visualise_ui", dataset = shiny::reactiveVal(raw),
                 normalised_data = shiny::reactiveVal(NULL), curated_norm_data = NULL)
  shiny::testServer(srv, {
    html <- paste(as.character(output$ui_visualise$html), collapse = " ")
    expect_false(grepl('id="vis_align_by"', html, fixed = TRUE))
    expect_match(html, paste("No token variable, so contours cannot be aligned by a TextGrid tier:",
                             "each part runs from its first f0 to its last within a token."), fixed = TRUE)
    expect_match(html, "Align by TextGrid tier", fixed = TRUE)
  })
  # With a token column, the selector and the help
  srv <- dp_bind(env, "visualise_ui", dataset = shiny::reactiveVal(dp_lm_raw()),
                 normalised_data = shiny::reactiveVal(NULL), curated_norm_data = NULL)
  shiny::testServer(srv, {
    html <- paste(as.character(output$ui_visualise$html), collapse = " ")
    expect_true(grepl('id="vis_align_by"', html, fixed = TRUE))
    expect_match(html, "Align by TextGrid tier", fixed = TRUE)
    hint <- paste(as.character(output$vis_align_hint$html), collapse = " ")
    expect_match(hint, "and so on, each from its first f0 to its last.", fixed = TRUE)
    guide <- paste(as.character(output$visualise_guide$html), collapse = " ")
    expect_match(guide, "rebuilt from it. <em>In sequence</em> puts syllable 1 at 0 to 1",
                 fixed = TRUE)
    # Only In sequence and Overlaid run each syllable from its first f0 to
    # its last; with the gap kept, the inner ends are the TextGrid boundaries
    expect_false(grepl("rebuilt from it: each syllable", guide, fixed = TRUE))
  })
  # No tier columns: the heading and a hint on where to add them
  srv <- dp_bind(env, "visualise_ui",
                 dataset = shiny::reactiveVal(dp_lm_raw()[c("token", "time", "f0", "tone", "speaker")]),
                 normalised_data = shiny::reactiveVal(NULL), curated_norm_data = NULL)
  shiny::testServer(srv, {
    html <- gsub("\\s+", " ", paste(as.character(output$ui_visualise$html), collapse = " "))
    expect_false(grepl('id="vis_align_by"', html, fixed = TRUE))
    expect_match(html, '<h5> Align by TextGrid tier <span style="font-weight:400; color:#888; font-size:0.8rem;">(optional)</span> </h5>',
                 fixed = TRUE)
    expect_match(html, paste("No TextGrid tier columns in this dataset. Add them in F0 Extraction",
                             "(choose TextGrid tiers) to align by a tier."), fixed = TRUE)
  })
})

test_that("Visualise's layout radio names the columns of the tier picked, and keeps the layout", {
  env <- dp_env()
  # Two tiers: syllable (a unit word) and tier2 (not one), each with its parts
  raw <- dp_lm_raw()
  raw$tier2_start <- raw$syllable_start; raw$tier2_end <- raw$syllable_end
  raw$tier2_i <- raw$syllable_i
  calls <- new.env()
  calls$log <- list()
  srv <- dp_bind(env, "visualise_ui", dataset = shiny::reactiveVal(raw),
                 normalised_data = shiny::reactiveVal(NULL), curated_norm_data = NULL,
                 start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
                 updateRadioButtons = function(session, inputId, ...)
                   calls$log[[length(calls$log) + 1]] <- c(list(id = inputId), list(...)))
  # (The plot is drawn at each change; A's and B's rows without f0 have no y,
  # and ggplot2 leaves them out with a warning, muffled)
  quiet <- function(expr) withCallingHandlers(expr, warning = function(w) {
    if (grepl("Removed [0-9]+ rows containing missing values", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })
  quiet(shiny::testServer(srv, {
    # Drawn with the first tier's columns while it is hidden
    html <- paste(as.character(output$ui_visualise$html), collapse = " ")
    expect_identical(dp_radio_labels(html),
                     c("In sequence (syllable_tseq)", "In sequence, gap kept (syllable_tbound)",
                       "Overlaid (syllable_t01)"))
    expect_match(html, '<option value="tier2">tier2</option>', fixed = TRUE)
    session$setInputs(x_var = "time", y_var = "f0", tone_var = "tone", speaker_var = "speaker",
                      convert_tone_to_factor = FALSE, plot_facet = "none",
                      vis_align_by = "__none__", vis_align_layout = "sequential")
    # No tier picked: nothing to rename
    expect_length(calls$log, 0)

    # tier2 with the gap-kept layout picked: its columns, the layout kept
    session$setInputs(vis_align_layout = "boundary")
    session$setInputs(vis_align_by = "tier2")
    expect_length(calls$log, 1)
    u <- calls$log[[1]]
    expect_identical(u$id, "vis_align_layout")
    expect_identical(u$choiceNames, c("In sequence (tier2_tseq)", "In sequence, gap kept (tier2_tbound)",
                                      "Overlaid (tier2_t01)"))
    expect_identical(u$choiceValues, c("sequential", "boundary", "overlay"))
    expect_identical(u$selected, "boundary")
    expect_null(u$choices)
    # The hint in tier2's words ("tier2 part", "tier2 parts")
    hint <- gsub("\\s+", " ", paste(as.character(output$vis_align_hint$html), collapse = " "))
    expect_match(hint, paste("<em>In sequence</em>: tier2 part 1 at 0 to 1, tier2 part 2 at 1 to 2,",
                             "and so on, each from its first f0 to its last. <em>In sequence, gap",
                             "kept</em>: the TextGrid boundaries sit at 1, 2, ..., so the gap between",
                             "tier2 parts keeps its room. <em>Overlaid</em>: every tier2 part on one",
                             "0\u20131 axis."), fixed = TRUE)
    # The plot still follows the layout picked, on tier2's column
    session$setInputs(plot_button = 1)
    expect_identical(vis_aligned()$layout, "boundary")
    expect_identical(current_plot()$labels$x, "tier2 parts in sequence, gap kept at the TextGrid boundaries")

    # Back to syllable, now overlaid: syllable's columns, Overlaid kept
    session$setInputs(vis_align_layout = "overlay")
    session$setInputs(vis_align_by = "syllable")
    expect_length(calls$log, 2)
    u <- calls$log[[2]]
    expect_identical(u$choiceNames, c("In sequence (syllable_tseq)", "In sequence, gap kept (syllable_tbound)",
                                      "Overlaid (syllable_t01)"))
    expect_identical(u$selected, "overlay")
    hint <- gsub("\\s+", " ", paste(as.character(output$vis_align_hint$html), collapse = " "))
    expect_match(hint, "<em>Overlaid</em>: every syllable on one 0\u20131 axis.", fixed = TRUE)
    expect_identical(vis_aligned()$layout, "overlay")
    expect_identical(current_plot()$labels$x, "time within each syllable, first to last f0 (0\u20131)")

    # Back to the raw X axis: the labels stay as they are
    session$setInputs(vis_align_by = "__none__")
    expect_length(calls$log, 2)
    expect_null(vis_aligned())
  }))
})

test_that("Visualise offers Overlaid only for a tier without <tier>_i, and all three again for one with it", {
  env <- dp_env()
  # Two tiers: syllable with syllable_i, tier2 without tier2_i (so no
  # tier2_tseq or tier2_tbound)
  raw <- dp_lm_raw()
  raw$tier2_start <- raw$syllable_start; raw$tier2_end <- raw$syllable_end
  three <- function(set) c(sprintf("In sequence (%s_tseq)", set),
                           sprintf("In sequence, gap kept (%s_tbound)", set),
                           sprintf("Overlaid (%s_t01)", set))
  hint_of <- function(output) gsub("\\s+", " ", paste(as.character(output$vis_align_hint$html),
                                                      collapse = " "))
  one_hint <- paste("<em>Overlaid</em>: every tier2 part on one 0\u20131 axis. <em>In sequence</em>",
                    "needs a tier2_i column.</div>")
  quiet <- function(expr) withCallingHandlers(expr, warning = function(w) {
    if (grepl("Removed [0-9]+ rows containing missing values", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })
  calls <- new.env()
  calls$log <- list()
  mk_srv <- function(d) dp_bind(env, "visualise_ui", dataset = shiny::reactiveVal(d),
    normalised_data = shiny::reactiveVal(NULL), curated_norm_data = NULL,
    start_cols = shiny::reactiveVal(list(token = "token", time = "time", f0 = "f0")),
    updateRadioButtons = function(session, inputId, ...)
      calls$log[[length(calls$log) + 1]] <- c(list(id = inputId), list(...)))
  last <- function() calls$log[[length(calls$log)]]

  quiet(shiny::testServer(mk_srv(raw), {
    # syllable is the first tier: the radio is drawn with all three
    html <- paste(as.character(output$ui_visualise$html), collapse = " ")
    expect_identical(dp_radio_labels(html), three("syllable"))
    session$setInputs(x_var = "time", y_var = "f0", tone_var = "tone", speaker_var = "speaker",
                      convert_tone_to_factor = FALSE, plot_facet = "none",
                      vis_align_by = "__none__", vis_align_layout = "sequential")
    # syllable, then the gap-kept layout
    session$setInputs(vis_align_by = "syllable")
    expect_identical(last()$choiceNames, three("syllable"))
    expect_identical(last()$selected, "sequential")
    session$setInputs(vis_align_layout = "boundary")

    # tier2 has no tier2_i: Overlaid only, picked
    session$setInputs(vis_align_by = "tier2")
    u <- last()
    expect_identical(u$id, "vis_align_layout")
    expect_identical(u$choiceNames, "Overlaid (tier2_t01)")
    expect_identical(u$choiceValues, "overlay")
    expect_identical(u$selected, "overlay")
    # The hint describes Overlaid only, and says why
    expect_match(hint_of(output), one_hint, fixed = TRUE)
    expect_no_match(hint_of(output), "gap kept|tseq|tbound")
    # The page sends the layout the radio now has; the plot is overlaid
    session$setInputs(vis_align_layout = "overlay", plot_button = 1)
    expect_identical(vis_aligned()$layout, "overlay")
    expect_identical(current_plot()$labels$x, "time within each tier2 part, first to last f0 (0\u20131)")

    # Back to syllable: all three again, with the gap-kept layout picked before
    session$setInputs(vis_align_by = "syllable")
    u <- last()
    expect_identical(u$choiceNames, three("syllable"))
    expect_identical(u$choiceValues, c("sequential", "boundary", "overlay"))
    expect_identical(u$selected, "boundary")
    expect_match(hint_of(output), "<em>In sequence, gap kept</em>: the TextGrid", fixed = TRUE)
    session$setInputs(vis_align_layout = "boundary")
    expect_identical(vis_aligned()$layout, "boundary")

    # Overlaid picked on syllable, then tier2 and back: Overlaid kept
    session$setInputs(vis_align_layout = "overlay")
    session$setInputs(vis_align_by = "tier2")
    expect_identical(last()$selected, "overlay")
    session$setInputs(vis_align_by = "syllable")
    expect_identical(last()$selected, "overlay")
  }))

  # tier2 first: the radio is drawn with Overlaid only, checked, and the
  # hint says so; syllable then offers all three, starting In sequence
  lm <- c("syllable_start", "syllable_end", "syllable_i")
  raw2 <- raw[c(setdiff(names(raw), lm), lm)]
  calls$log <- list()
  quiet(shiny::testServer(mk_srv(raw2), {
    html <- paste(as.character(output$ui_visualise$html), collapse = " ")
    expect_identical(dp_radio_labels(html), "Overlaid (tier2_t01)")
    expect_match(html, 'value="overlay" checked="checked"', fixed = TRUE)
    expect_no_match(html, 'value="sequential"', fixed = TRUE)
    expect_match(hint_of(output), one_hint, fixed = TRUE)
    session$setInputs(x_var = "time", y_var = "f0", tone_var = "tone", speaker_var = "speaker",
                      convert_tone_to_factor = FALSE, plot_facet = "none",
                      vis_align_by = "__none__", vis_align_layout = "overlay")
    session$setInputs(vis_align_by = "syllable")
    expect_identical(last()$choiceNames, three("syllable"))
    expect_identical(last()$selected, "sequential")
    session$setInputs(vis_align_layout = "sequential")
    session$setInputs(vis_align_by = "tier2")
    expect_identical(last()$choiceNames, "Overlaid (tier2_t01)")
    expect_identical(last()$selected, "overlay")
  }))
})

test_that("the Curate and Visualise texts are ASCII in the source, with no em dash", {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  for (f in c("curate_ui.R", "visualise_ui.R")) {
    ex <- parse(file.path(app, "ui", f), keep.source = TRUE, encoding = "UTF-8")
    pd <- utils::getParseData(ex)
    s  <- utils::getParseText(pd, pd$id[pd$token == "STR_CONST"])
    # Written as \u escapes, a character outside ASCII reads the same in any
    # locale (server.R sources these files with the default encoding)
    expect_identical(s[grepl("[^\\x01-\\x7F]", s, perl = TRUE)], character(0), label = f)
    # No em dash in what the user reads
    v <- vapply(s, function(x) eval(parse(text = x, keep.source = FALSE)), "", USE.NAMES = FALSE)
    expect_identical(v[grepl("\u2014|&mdash;|&#8212;", v)], character(0), label = f)
  }
})
