# # Define server logic required to draw a histogram
# server <- function(input, output) {
#  
#   # Source your external UI components (start_ui and view_ui)
#   source("ui/start_ui.R", local = TRUE)
#   source("ui/view_ui.R", local = TRUE)
#   
#   # Define the dataset variable to be used across the application
#   dataset <- reactiveVal(NULL)
#   
#   
# }

# Eager: only what is needed to build the UI and render the landing (About)
# page. The heavy analysis / plot / audio packages are loaded right after the
# first render (see the `session$onFlushed` deferred loader inside server()),
# so the landing page appears immediately and the rest load while the user
# reads it. tidyr / purrr / stringr / readr are never used by bare name, so
# dplyr (here) + ggplot2 (deferred) cover all former `tidyverse` usage.
library(shiny)
library(bslib)
library(dplyr)

source("ui/start_ui.R")
source("ui/view_ui.R")
source("ui/visualise_ui.R")
source("ui/normalise_ui.R")
source("ui/inspect_ui.R")
source("ui/cluster_ui.R")
source("ui/curate_ui.R")
source("ui/model_ui.R")
source("ui/gca_ui.R")
source("ui/gamm_ui.R")
source("ui/checklist_ui.R")
source("ui/filename_guide_ui.R")
source("ui/waveform_guide_ui.R")
source("ui/summarise_ui.R")
source("ui/summarise_plot.R")
source("ui/fp_start_ui.R")
source("ui/fp_extraction_ui.R")
source("ui/fp_correction_ui.R")
source("ui/fp_praat_script_ui.R")
source("ui/about_usage_ui.R")
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB cap for batch WAV uploads
#options(shiny.useragg = TRUE)

# Plot theming (thematic auto-theming + the default ggplot2 theme) is set up
# in the deferred loader inside server(), once ggplot2 has been loaded.

# Note: shared helpers like guess_var() and var_patterns now live in
# global.R, which Shiny sources into the global environment before any
# session. They are reachable from every ui_*.R module from there.

server <- function(input, output, session) {

  # ---- Deferred package loading (faster first paint) ----------------------
  # The landing page only needs shiny / bslib / dplyr. Everything heavy is
  # loaded right after the first flush, so the About page renders immediately
  # and the analysis / plot / audio packages load while the user reads it.
  # R is single-threaded, so a click during loading simply waits until it
  # finishes (the front page is already in the browser). Loaded once per
  # process; later sessions hit a warm process where these are no-ops.
  session$onFlushed(function() {
    # Update reminder: run_app() sets this option when a newer shinytone
    # release exists on GitHub. Deployed instances never set it (run_app is
    # not called there), so this is a local-install-only nudge.
    upd <- getOption("shinytone.update_available")
    if (!is.null(upd)) {
      tryCatch(showNotification(
        HTML(paste0("shinytone ", upd, " is available. Update with<br>",
                    "<code>remotes::install_github(\"chenchenzi/citationtone_hub\")</code>")),
        type = "message", duration = 15, session = session),
        error = function(e) NULL)
    }
    # The "Loading analysis tools…" toast is shown client-side the instant the
    # page appears (see shiny:connected in ui.R), so the wait is visible from the
    # very start. On a warm process (a later session) the packages are already
    # attached, so there is nothing to load — just clear that toast.
    if ("package:mgcv" %in% search()) {
      session$sendCustomMessage("pkgload_done", TRUE)
      return(invisible())
    }
    # Run the blocking load one tick later (the page is already up). The later()
    # callback runs outside Shiny's reactive context, so pass `session` explicitly
    # to the sendCustomMessage that clears the toast when loading finishes.
    later::later(function() {
      tryCatch(
        suppressWarnings(suppressPackageStartupMessages({
          library(DT); library(ggplot2); library(plotly)
          library(RColorBrewer); library(gridExtra)
          library(lme4); library(emmeans); library(mgcv)
          library(tuneR); library(rPraat); library(praatpicture)
          library(thematic)
          # Pin the plot device background to white (the page background) rather
          # than "auto". With "auto", shiny::startPNG calls
          # getCurrentOutputInfo()[["bg"]]() which errors ("attempt to apply
          # non-function") whenever a plot output does not report its CSS theme
          # info (e.g. plotOutput nested inside styled section cards). White
          # matches the page, so this is visually identical but crash-proof.
          thematic::thematic_shiny(bg = "white", font = "auto")
          ggplot2::theme_set(ggplot2::theme_bw(base_size = 16))
        })),
        error = function(e) NULL)
      session$sendCustomMessage("pkgload_done", TRUE)
    }, delay = 0.25)
  }, once = TRUE)

  # Reactive dataset storage. Single source of truth: an uploaded CSV.
  # The "Try with our sample data" button populates input$uploadfile via JS
  # (see the load_sample_csv handler), so the sample path goes through the
  # exact same code path as a manual upload.
  # Raw uploaded CSV — the single source of truth before any optional,
  # View-tab metadata is attached.
  # Read in any Unicode encoding (Praat saves UTF-16 for non-ASCII text);
  # Praat's --undefined-- reads as NA. A file that cannot be read gives NULL
  # and a message, never an error: an error here would reach the observers
  # and close the session.
  raw_dataset <- reactive({
    fi <- input$uploadfile
    if (is.null(fi)) return(NULL)
    df <- tryCatch(
      read_csv_utf8(fi$datapath, stringsAsFactors = FALSE,
                    na.strings = c("NA", "--undefined--"), name = fi$name),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = NULL,
                         id = "upload_read")
        NULL
      })
    if (is.null(df)) return(NULL)
    # 0 Hz in an f0 column is an unvoiced frame: read it as NA. Counts go in
    # attr "zero_hz" for the Start-tab notice.
    df <- zero_hz_to_na(df)
    note <- attr(df, "encoding_note")
    if (!is.null(note)) {
      showNotification(note, type = "warning", duration = 10, id = "upload_read")
    } else {
      removeNotification("upload_read")
    }
    df
  })

  # Optional metadata attached in the Start tab, so users who already have an
  # f0 dataframe (e.g. from Praat) can add speaker/tone/etc. columns without
  # the audio-based F0 Extraction path. Stored as
  #   list(key = <join column>, data = <df with key + new columns>).
  # Reset whenever a new file is uploaded.
  # `source` says how it was made, for the exported R code:
  # list(type = "csv", file, meta_key, strip) or list(type = "derive", sep, names).
  attached_metadata <- reactiveVal(NULL)
  # Contour-duration columns added in the Start tab, as a list of
  # list(token, time, f0, set, name) specs keyed by column name; each adds one
  # column via contour_duration(). Reset along with the metadata.
  duration_specs <- reactiveVal(list())

  # The Start page's boxes as the user set them for this upload: "Exclude
  # discarded tokens" (exclude) and the "Data screening" boxes remove, short
  # (min_run = 3), unvoiced, mark and drop_flagged (the tokens
  # flag_f0_tokens() flags). NULL means the user has not changed the box,
  # so it follows its default (screen_box() below). A new upload resets
  # them, so a box the user unticked for the previous file does not carry
  # over, not even for the moment before the new boxes are drawn.
  screen_keys   <- c(exclude = "exclude_dropped", remove = "screen_remove",
                     short = "screen_short", unvoiced = "screen_unvoiced",
                     mark = "screen_mark", drop_flagged = "screen_drop_flagged")
  screen_choice <- reactiveValues(exclude = NULL, remove = NULL, short = NULL,
                                  unvoiced = NULL, mark = NULL, drop_flagged = NULL)
  # The Start page's token, time and f0 columns as the user picked them for
  # this upload. NULL means the picker is on its default
  # (start_screen_defaults(), global.R). Like the boxes, the picks are per
  # upload: the last file's columns must not override the new file's
  # defaults (f0_corrected next to f0 in an F0 Correction file, say), not
  # even when the new file has the same column.
  screen_pick <- reactiveValues(token = NULL, time = NULL, f0 = NULL)
  # Counts uploads, so data_version (below) can tell a new upload from a
  # change on the Start page.
  upload_n <- reactiveVal(0L)
  # Priority 1001: above the observer that resolves the Start columns
  # (start_cols_val, priority 1000), so it never sees the last file's picks.
  observeEvent(input$uploadfile, {
    attached_metadata(NULL)
    duration_specs(list())
    for (k in names(screen_keys)) screen_choice[[k]] <- NULL
    for (k in c("token", "time", "f0")) screen_pick[[k]] <- NULL
    upload_n(isolate(upload_n()) + 1L)
  }, priority = 1001)

  # ---- F0 Analysis data, in the order the Start page builds it -------------
  # 1. prepared(): the upload with the discarded tokens excluded, the
  #    metadata joined and the duration columns added.
  # 2. f0_screen(): the rows without f0 in it, on the Start-page columns.
  # 3. fixed(): the tokens with an issue flagged in a screen_flag column
  #    (flag_f0_tokens()), and dropped when ticked (screen_fix()).
  #    post_screen() is the screen of fixed(), whose counts the removal
  #    reads (its default reads the upload's screen, screen_box()).
  # 4. screened(): the rows without f0 removed and voiced_run added, when
  #    ticked. dataset() is screened() with the "as factors" option, and is
  #    what every F0 Analysis tab reads.
  # None of this goes into raw_dataset(): the Start pickers are built from its
  # column names, so it must not depend on them.
  prepared <- reactive({
    raw <- raw_dataset()
    if (is.null(raw)) return(NULL)
    # Whole-token discards from the F0 Correction tab travel in the CSV as
    # token_dropped = TRUE (all_correctedf0.csv keeps every token). Excluded
    # here by default (checkbox in the Start sidebar; not yet changed counts
    # as on), so downstream tabs see only kept tokens. The raw upload itself
    # stays untouched; unticking restores the full data.
    if ("token_dropped" %in% names(raw) && screen_box("exclude")) {
      td  <- suppressWarnings(as.logical(raw$token_dropped))
      raw <- raw[!(td %in% TRUE), , drop = FALSE]
      raw$token_dropped <- NULL   # constant FALSE now; keep pickers clean
      rownames(raw) <- NULL
    }
    m <- attached_metadata()
    if (!is.null(m) && !is.null(m$data) && !is.null(m$key) &&
        m$key %in% names(raw) && m$key %in% names(m$data)) {
      new_cols <- setdiff(names(m$data), names(raw))
      meta     <- m$data[, c(m$key, new_cols), drop = FALSE]
      raw      <- tryCatch(dplyr::left_join(raw, meta, by = m$key),
                           error = function(e) raw)
    }
    # A duration on the screened column reads the values the screen reads as
    # no f0 (0 / -1, when ticked) as no f0 too (duration_unvoiced(), global.R).
    for (s in duration_specs()) {
      raw <- tryCatch(add_duration(raw, s, duration_unvoiced(raw, s$f0, start_cols, screen_box)),
                      error = function(e) raw)
    }
    raw
  })

  # A value a picker sends is the user's pick when it differs from the
  # picker's default for this upload. The value a picker sends when it is
  # drawn is its default, so it is not a pick, and picking the default again
  # makes the picker follow the default. The raw input values are never
  # used as they are: after a new upload they still hold the last file's
  # columns until the pickers are drawn again.
  for (key in c("token", "time", "f0")) local({
    k  <- key
    id <- paste0("screen_", k)
    observeEvent(input[[id]], {
      raw <- raw_dataset()
      v   <- input[[id]]
      if (is.null(raw)) return()
      # Token "" is "(none)", offered only for a file with no token column,
      # where it is the default
      if (k == "token" && (is.null(v) || identical(v, ""))) {
        screen_pick$token <- NULL
        return()
      }
      if (!is.character(v) || length(v) != 1 || is.na(v)) return()
      def <- start_screen_defaults(raw)[[k]]
      screen_pick[[k]] <- if (identical(v, def) || !v %in% start_screen_choices(raw)[[k]]) NULL else v
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  })

  # The Start-page columns, checked against the upload: no pick, or a stale
  # one, falls back to the default the picker shows (resolve_start_cols(),
  # global.R). Held in a reactiveVal, which signals only a real change. The
  # tabs take their token and f0 defaults from it.
  start_cols_val <- reactiveVal(NULL)
  observe({
    start_cols_val(resolve_start_cols(raw_dataset(), screen_pick$token,
                                      screen_pick$time, screen_pick$f0))
  }, priority = 1000)
  start_cols <- reactive(start_cols_val())

  # A box's value: the user's choice, or its default, the same for every
  # kind of file, so one box never ticks another. Removal, reading 0 / -1
  # as no f0 and marking gaps are ticked by default; the short runs and the
  # drop of the flagged tokens are not.
  screen_box <- function(key) {
    v <- screen_choice[[key]]
    if (!is.null(v)) return(isTRUE(v))
    switch(key,
           exclude   = TRUE,
           remove    = TRUE,
           short     = FALSE,
           unvoiced  = TRUE,
           mark      = TRUE,
           drop_flagged = FALSE,
           FALSE)
  }
  # A click that differs from the box's value is the user's choice. The value
  # a box sends when it is drawn equals its default, so it is not one.
  for (key in names(screen_keys)) local({
    k  <- key
    id <- screen_keys[[key]]
    observeEvent(input[[id]], {
      v <- isTRUE(input[[id]])
      if (!identical(v, isolate(screen_box(k)))) screen_choice[[k]] <- v
    })
  })

  # The screen: screen_f0_rows() on the Start columns, after any values the
  # user reads as no f0 (0 or -1 in an Hz column that zero_hz_to_na() did not
  # pick, suggest_unvoiced()). Without a token column the rows without f0 are
  # only counted over the whole file. A list: cols, suggested (values that
  # may mark no f0), unvoiced (those read as no f0), zero_hz (0 Hz read as NA
  # in raw_dataset()), rows, without (rows without f0), screen (the
  # screen_f0_rows() result, NULL without a token column) and error.
  f0_screen <- reactive({
    d    <- prepared()
    cols <- start_cols()
    if (is.null(d) || is.null(cols) || is.null(cols$f0) || !cols$f0 %in% names(d)) return(NULL)
    x   <- d[[cols$f0]]
    xv  <- suppressWarnings(as.numeric(if (is.factor(x)) as.character(x) else x))
    sug <- suggest_unvoiced(x)
    unv <- if (length(sug) && screen_box("unvoiced")) sug else NULL
    out <- list(cols = cols, suggested = sug, unvoiced = unv,
                unvoiced_rows = if (length(sug)) sum(!is.na(xv) & xv %in% sug) else 0L,
                zero_hz = attr(raw_dataset(), "zero_hz"), rows = nrow(d),
                without = NA_integer_, screen = NULL, error = NULL)
    if (is.null(cols$token) || is.null(cols$time) || !cols$token %in% names(d) ||
        !cols$time %in% names(d)) {
      v <- xv
      if (length(unv)) v[v %in% unv] <- NA
      out$without <- sum(!is.finite(v))
      return(out)
    }
    scr <- tryCatch(screen_f0_rows(d, token = cols$token, time = cols$time,
                                   f0 = cols$f0, unvoiced = unv),
                    error = function(e) e)
    if (inherits(scr, "error")) {
      out$error <- conditionMessage(scr)
      return(out)
    }
    out$screen  <- scr
    out$without <- attr(scr, "summary")[["rows_without_f0"]]
    out
  })

  # Before the removal: flag the tokens with an issue in a screen_flag
  # column (flag_f0_tokens(): no f0, or fewer than 3 samples with f0, and,
  # for a file where every token has the same number of points, a first,
  # last or inside point without f0), and drop them when ticked (unticked
  # by default, offered when a token is flagged). The values read as no f0
  # (0 / -1) become NA first, so the flags read them as no f0. A token with
  # no f0 is flagged like the others, whatever the removal box says.
  # In an equal-point file every point without f0 flags its token, so the
  # flagged tokens are the tokens with fewer than N points with f0.
  # That box is read with screen_box(), as the plan reads the data after
  # the drop. The short-run removal (min_run = 3) is not read: a token with
  # a run of 3 or more samples keeps it, and one without keeps all its
  # samples, so it never leaves a token with fewer than 3.
  # screen_flags_data(): the flags, apart from the box that only chooses
  # what to drop, so ticking it does not flag again. A list: points (every
  # token has the same number of points), data (the upload with the values
  # read as no f0 as NA) and flagged (the data flagged, NULL when the flags
  # failed). NULL without a token column.
  screen_flags_data <- reactive({
    scr <- f0_screen()
    sc  <- if (!is.null(scr)) scr$screen
    p   <- prepared()
    if (is.null(sc) || is.null(p)) return(NULL)
    cols <- scr$cols
    d <- p
    if (length(scr$unvoiced)) {
      x <- d[[cols$f0]]
      x[!is.na(x) & x %in% scr$unvoiced] <- NA
      d[[cols$f0]] <- x
    }
    out <- list(points = isTRUE(attr(sc, "equal_rows")), data = d, flagged = NULL)
    fl <- tryCatch(flag_f0_tokens(d, token = cols$token, time = cols$time, f0 = cols$f0),
                   error = function(e) e)
    if (inherits(fl, "error")) {
      showNotification(paste("Could not flag the tokens:", conditionMessage(fl)),
                       type = "error", duration = 10, id = "f0a_f0_flag_error")
    } else {
      out$flagged <- fl
    }
    out
  })

  # screen_fix(): a list: flag (some token is flagged, so the data get the
  # column), drop_flagged (what is done), points (TRUE for an equal-point
  # file), n_points (N), flagged (the flagged tokens, with their reasons,
  # as in flag_f0_tokens()'s "flagged" attribute), n_flag_rows (the rows
  # of the flagged tokens), all_flagged (every token is flagged), old_flag
  # (the upload has a screen_flag column of its own: replaced when a token
  # is flagged, left out otherwise) and data (the data flagged, for
  # fixed()).
  screen_fix <- reactive({
    scr <- f0_screen()
    sc  <- if (!is.null(scr)) scr$screen
    out <- list(flag = FALSE, drop_flagged = FALSE, points = FALSE,
                n_points = NA_integer_, flagged = NULL,
                n_flag_rows = 0L, all_flagged = FALSE,
                old_flag = FALSE, data = NULL)
    fd <- screen_flags_data()
    if (is.null(sc) || is.null(fd)) return(out)
    s <- attr(sc, "summary")
    if (fd$points) {
      out$points   <- TRUE
      out$n_points <- attr(sc, "stretch")$n_rows
    }
    fl <- fd$flagged
    if (!is.null(fl)) {
      flagged <- attr(fl, "flagged")
      out$flagged  <- flagged
      out$flag     <- nrow(flagged) > 0
      out$old_flag <- "screen_flag" %in% names(fd$data)
    }
    out$drop_flagged <- out$flag && screen_box("drop_flagged")
    out$data <- if (out$flag) fl else fd$data
    if (out$flag) {
      fv <- fl[["screen_flag"]]
      out$n_flag_rows <- sum(nzchar(fv))
      out$all_flagged <- nrow(out$flagged) == s[["tokens"]]
    }
    out
  })

  # prepared() after the flags and the drop (screen_fix()), when a token is
  # flagged: the values read as no f0 (0 / -1) are NA there. The drop
  # takes every flagged token, and the screen_flag column with them.
  # Otherwise prepared() as it is. An upload's own screen_flag column, from an
  # earlier screening, is replaced by the flags, or left out when no token
  # is flagged now, so the column always says what the card says.
  fixed <- reactive({
    d <- prepared()
    if (is.null(d)) return(NULL)
    fx <- screen_fix()
    if (fx$flag && !is.null(fx$data)) {
      d <- fx$data
      if (fx$drop_flagged) {
        d <- d[d[["screen_flag"]] == "", , drop = FALSE]
        rownames(d) <- NULL
        d[["screen_flag"]] <- NULL
      }
    }
    if (fx$old_flag && !fx$flag) d[["screen_flag"]] <- NULL
    d
  })

  # The screen of fixed(): what is left for the removal. The screen of the
  # upload when nothing was dropped (the flags alone change no count). NULL
  # without a token column.
  post_screen <- reactive({
    scr <- f0_screen()
    if (is.null(scr) || is.null(scr$screen)) return(NULL)
    fx <- screen_fix()
    if (!fx$drop_flagged) return(scr$screen)
    cols <- scr$cols
    tryCatch(screen_f0_rows(fixed(), token = cols$token, time = cols$time, f0 = cols$f0),
             error = function(e) scr$screen)
  })

  # What dataset() does with the screen, from the boxes. drop: run
  # drop_rows_without_f0() (to remove rows, or to mark gaps on an upload
  # whose rows all have f0); otherwise the values read as no f0 become NA.
  # remove, mark and short read the screen after the drop (post, with its
  # count of rows without f0 in `without`), as their boxes do
  # (screen_flags(), start_ui.R). The short runs are never removed in an
  # equal-point file (points, read from the upload), where their box is
  # hidden: a gap there flags its token, which the drop can leave out.
  screen_plan <- reactive({
    scr <- f0_screen()
    if (is.null(scr)) return(NULL)
    fx <- screen_fix()
    ps <- post_screen()
    s  <- if (!is.null(ps)) attr(ps, "summary")
    without <- if (!is.null(s)) s[["rows_without_f0"]]
               else if (is.na(scr$without)) 0L else scr$without
    remove  <- !is.null(s) && without > 0 && screen_box("remove")
    mark    <- !is.null(s) && without == 0 && s[["gaps_time_only"]] > 0 && screen_box("mark")
    short   <- (remove || mark) && !fx$points && s[["short_runs"]] > 0 && screen_box("short")
    list(cols = scr$cols, remove = remove, mark = mark, short = short,
         min_run = if (short) 3 else 1, unvoiced = scr$unvoiced,
         drop = remove || mark,
         action = if (remove) "remove" else if (mark) "mark" else "none",
         flag = fx$flag, drop_flagged = fx$drop_flagged,
         flagged = fx$flagged, n_flag_rows = fx$n_flag_rows,
         all_flagged = fx$all_flagged, old_flag = fx$old_flag, points = fx$points,
         n_points = fx$n_points, post = ps, without = without)
  })

  screened <- reactive({
    d <- fixed()
    if (is.null(d)) return(NULL)
    plan <- screen_plan()
    if (is.null(plan)) return(d)
    cols <- plan$cols
    if (plan$drop) {
      d <- tryCatch(suppressMessages(drop_rows_without_f0(
                      d, token = cols$token, time = cols$time, f0 = cols$f0,
                      min_run = plan$min_run, unvoiced = plan$unvoiced)),
                    error = function(e) {
                      showNotification(paste("Could not remove the rows without f0:",
                                             conditionMessage(e)),
                                       type = "error", duration = 10, id = "f0a_f0_screen_error")
                      d
                    })
    } else if (length(plan$unvoiced)) {
      x <- d[[cols$f0]]
      x[!is.na(x) & x %in% plan$unvoiced] <- NA
      d[[cols$f0]] <- x
    }
    d
  })

  # The dataset every F0 Analysis tab reads.
  dataset <- reactive({
    d <- screened()
    if (is.null(d)) return(NULL)
    if (isTRUE(input$convert_to_factor)) {
      d[] <- lapply(d, function(x) if (is.character(x)) as.factor(x) else x)
    }
    d
  })

  # The Start-page settings, for the R code the tabs show (read_data_code()
  # in global.R). The tabs pass it on as it is.
  start_settings <- reactive({
    raw <- raw_dataset()
    if (is.null(raw)) return(NULL)
    # As prepared(): the column goes whenever it is there and the box is on,
    # also when no token is marked (rows = 0), so the code drops it too.
    dropped <- NULL
    if ("token_dropped" %in% names(raw) && screen_box("exclude")) {
      td <- suppressWarnings(as.logical(raw$token_dropped)) %in% TRUE
      tk <- vis_token_col(names(raw))
      dropped <- list(rows = sum(td),
                      tokens = if (!is.null(tk)) length(unique(raw[[tk]][td])))
    }
    meta <- NULL
    m <- attached_metadata()
    if (!is.null(m) && !is.null(m$data) && !is.null(m$key) && m$key %in% names(m$data)) {
      cols <- setdiff(names(m$data), setdiff(names(raw), "token_dropped"))
      if (length(cols)) meta <- list(key = m$key, cols = cols, source = m$source)
    }
    sc  <- NULL
    scr <- f0_screen()
    plan <- screen_plan()
    if (!is.null(scr) && !is.null(plan)) {
      s <- if (!is.null(scr$screen)) attr(scr$screen, "summary")
      # The removal's counts come from the screen after the drop (the same
      # as `s` when nothing is dropped)
      sp <- if (!is.null(plan$post)) attr(plan$post, "summary")
      fixed_any <- isTRUE(plan$drop_flagged)
      sc <- list(token = if (!is.null(scr$screen)) scr$cols$token, time = scr$cols$time,
                 f0 = scr$cols$f0, unvoiced = plan$unvoiced, action = plan$action,
                 min_run = plan$min_run, rows = scr$rows, without = scr$without,
                 summary = s, post = if (fixed_any) sp, error = scr$error,
                 rows_after = if (plan$drop || fixed_any) nrow(screened()) else scr$rows,
                 gaps_marked = if (!is.null(sp)) sp[["gaps_time_only"]] + sp[["gaps_run"]] else 0,
                 short_rows = if (!is.null(sp)) sp[["short_rows"]] else 0,
                 # The flags: the flagged tokens, their rows and the drop
                 flag = if (isTRUE(plan$flag))
                   list(tokens = nrow(plan$flagged), rows = plan$n_flag_rows,
                        drop = isTRUE(plan$drop_flagged)),
                 # The upload's own screen_flag column, left out as no
                 # token is flagged now (fixed())
                 old_flag = isTRUE(plan$old_flag) && !isTRUE(plan$flag))
    }
    # Each duration with the values it reads as no f0, as prepared() does
    pd   <- prepared()
    durs <- lapply(unname(duration_specs()), function(s) {
      s$unvoiced <- duration_unvoiced(pd, s$f0, start_cols, screen_box)
      s
    })
    list(version = shinytone_version(), zero_hz = attr(raw, "zero_hz"),
         dropped = dropped, metadata = meta, durations = durs,
         screen = sc)
  })

  # What the Start page did, in a form that names a change: the settings
  # behind dataset()'s rows (see data_version below). The boxes as the user
  # left them, not what the plan does with them, so the toast names the box
  # that was clicked: a drop that leaves nothing to remove does not untick
  # removal, and a change of the file's kind does not untick the short runs.
  start_state <- function() {
    raw  <- raw_dataset()
    plan <- screen_plan()
    m    <- attached_metadata()
    list(exclude = !is.null(raw) && "token_dropped" %in% names(raw) &&
                   screen_box("exclude"),
         meta = if (!is.null(m)) names(m$data),
         dur = names(duration_specs()),
         token = plan$cols$token, time = plan$cols$time, f0 = plan$cols$f0,
         remove = screen_box("remove"), short = screen_box("short"),
         unvoiced = screen_box("unvoiced"),
         mark = screen_box("mark"), drop_flagged = screen_box("drop_flagged"))
  }

  # When the sample button is clicked, fire JS that fetches the bundled
  # sample file and injects it into the #uploadfile <input>. This triggers
  # Shiny's normal fileInput binding, which uploads + sets input$uploadfile
  # just like a manual Browse... action.
  sample_just_clicked <- reactiveVal(FALSE)
  observeEvent(input$try_sample, {
    sample_just_clicked(TRUE)
    session$sendCustomMessage("load_sample_csv", list(
      url      = "dc21f0_test.csv",   # served from www/
      filename = "dc21f0_test.csv"
    ))
  })

  # When the upload arrives — only if it came from the sample-click flow —
  # fire a confirmation toast and smooth-scroll to the Data Preview anchor.
  observeEvent(input$uploadfile, {
    req(input$uploadfile)
    if (!isTRUE(sample_just_clicked())) return()
    sample_just_clicked(FALSE)
    ds <- dataset()
    if (!is.null(ds)) {
      n_rows <- nrow(ds)
      n_tok  <- if ("token"   %in% names(ds)) length(unique(ds$token))   else NA
      n_spk  <- if ("speaker" %in% names(ds)) length(unique(ds$speaker)) else NA
      msg <- sprintf("Sample data loaded: %s rows", fmt_count(n_rows))
      if (!is.na(n_tok)) msg <- paste0(msg, sprintf(", %s tokens", fmt_count(n_tok)))
      if (!is.na(n_spk)) msg <- paste0(msg, sprintf(", %s speakers", fmt_count(n_spk)))
      msg <- paste0(msg, ". ✅  Try the Visualise tab to plot the contours!")
      showNotification(msg, type = "message", duration = 6,
                       id = "f0a_sample_loaded")
    }
    session$sendCustomMessage("scroll_to_id", "f0a-data-preview-anchor")
  })
  
  # Shared storage for normalised dataset (written by Normalise tab, read by Model tab)
  normalised_data <- reactiveVal(NULL)

  # Shared storage for curated dataset (written by Curate tab: relabels applied
  # to the tone column, excluded tokens dropped; offered as a "Curated data"
  # source in the modelling and summarise tabs). NULL until the user curates.
  curated_data <- reactiveVal(NULL)

  # Shared storage for curated + normalised data (written by the Normalise tab
  # when its source is the curated data: normalised columns recomputed on the
  # kept tokens and the new tone labels). Curate never reads it, so nothing
  # loops; the modelling, Visualise and Summarise tabs offer it as a source.
  # It records a fingerprint of the curated tokens, labels, speakers and Hz
  # values it came from (curated_key() in global.R), and is cleared when those
  # change, so a stale copy is never modelled.
  curated_norm_data <- reactiveVal(NULL)
  observeEvent(curated_data(), {
    cn <- curated_norm_data()
    if (is.null(cn)) return()
    cd <- curated_data()
    if (is.null(cd)) {
      curated_norm_data(NULL)
      showNotification("Curation was reset, so the curated + normalised data was cleared.",
                       type = "warning", duration = 6, id = "curated_norm_cleared")
    } else if (!identical(curated_key(cd, attr(cn, "curated_key_cols")),
                          attr(cn, "curated_key"))) {
      curated_norm_data(NULL)
      showNotification(paste("The curated tokens or labels changed, so the curated +",
                             "normalised data was cleared. Normalise the curated data",
                             "again (Normalise tab, data source: Curated data)."),
                       type = "warning", duration = 8, id = "curated_norm_cleared")
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Shared inspection result (written by Inspect tab, read by Curate tab so the
  # actual Inspect flags can seed re-label candidates). NULL until Inspect runs.
  inspect_result <- reactiveVal(NULL)

  # Shared storage for clustered dataset (written by Cluster tab: adds a
  # `cluster` column of candidate tone categories; offered as a "Clustered
  # data" source in the modelling tabs). NULL until the user runs clustering.
  cluster_data <- reactiveVal(NULL)

  # Shared storage for model prediction data (written by GCA/GAMM, read by Summarise)
  gca_pred_data <- reactiveVal(NULL)
  gamm_pred_data <- reactiveVal(NULL)

  # A new upload starts afresh: everything computed from the previous file is
  # cleared, so no tab offers (or selects by default) the old file's
  # normalised, clustered or model data. Each tab also clears its own shown
  # results, and Curate its relabels and exclusions.
  observeEvent(input$uploadfile, {
    derived <- list(normalised_data, cluster_data, curated_norm_data,
                    inspect_result, gca_pred_data, gamm_pred_data)
    had <- any(vapply(derived, function(rv) !is.null(rv()), logical(1))) ||
           !is.null(curated_data())
    for (rv in derived) rv(NULL)
    if (had)
      showNotification(paste("New file: normalised, inspected, clustered, curated and",
                             "model results from the previous file were cleared."),
                       type = "message", duration = 6, id = "upload_cleared")
  }, priority = 100)

  # ---- data_version: dataset()'s rows changed without a new upload ---------
  # It goes up by 1 whenever the rows the tabs read change for another reason
  # than a new upload: the screen's boxes or columns, "Exclude discarded
  # tokens", metadata or duration columns. The tabs clear their own results
  # when it changes, as they do on an upload. A change is a new fingerprint
  # of screened() within the same upload (the "as factors" option changes
  # only the column types, so it does not count), so a change that leaves
  # the rows as they were (say, another f0 column while removal is
  # unticked) clears nothing.
  data_version <- reactiveVal(0L)
  dv_track <- new.env()
  dv_track$upload <- NULL
  dv_track$hash   <- NULL
  dv_track$state  <- NULL
  dv_track$reason <- NULL
  observe({
    d  <- screened()
    up <- upload_n()
    h  <- if (is.null(d)) NULL else rlang::hash(d)
    st <- isolate(start_state())
    if (!identical(up, dv_track$upload)) {
      dv_track$upload <- up
      dv_track$hash   <- h
      dv_track$state  <- st
      return()
    }
    if (identical(h, dv_track$hash)) {
      dv_track$state <- st
      return()
    }
    dv_track$reason <- start_change_text(dv_track$state, st)
    dv_track$hash   <- h
    dv_track$state  <- st
    data_version(isolate(data_version()) + 1L)
  }, priority = 10)

  # The shared results built from the old rows are cleared, with a toast that
  # names them and the Start-page change. Curated + normalised data is
  # cleared here too, before Curate publishes its data again, so its own
  # observer above does not add a second toast with another reason.
  observeEvent(data_version(), {
    derived <- list("normalised data" = normalised_data, "clustered data" = cluster_data,
                    "curated + normalised data" = curated_norm_data,
                    "Inspect results" = inspect_result, "GCA predictions" = gca_pred_data,
                    "GAMM predictions" = gamm_pred_data)
    had <- names(derived)[vapply(derived, function(rv) !is.null(rv()), logical(1))]
    for (rv in derived) rv(NULL)
    if (length(had)) {
      showNotification(
        sprintf("On the Start page, %s. These results came from the old rows, so they were cleared: %s.",
                dv_track$reason %||% "the data changed", list_some(had, 6)),
        type = "warning", duration = 10, id = "start_data_cleared")
    }
  }, ignoreInit = TRUE, priority = 9)

  # Shared storage for f0 processing tab
  fp_audio_data       <- reactiveVal(NULL)   # data.frame: basename, wav_path, tg_path, pitch_path, sr, bit, dur, channels
  fp_f0_data          <- reactiveVal(NULL)   # extracted/parsed f0 contours (long format)
  fp_pitch_candidates <- reactiveVal(list()) # named list: token -> list of per-frame data.frame(frequency, strength). Populated only for tokens parsed from .Pitch files.
  fp_corrected_data   <- reactiveVal(NULL)   # fp_f0_data + f0_corrected/edited/token_dropped, mirrored from the F0 Correction tab so the F0 Extraction analysis export can measure corrected contours
  fp_metadata         <- reactiveVal(NULL)   # optional user-uploaded metadata CSV (data.frame). Joined to fp_f0_data at download time on a filename column the user selects.

  # Call the Start tab UI and server logic (start_ui function)
  about_usage_ui(input, output, session)
  start_ui(input, output, session, dataset, raw_dataset, attached_metadata, duration_specs,
           prepared = prepared, screened = screened, f0_screen = f0_screen,
           screen_plan = screen_plan, screen_box = screen_box, start_cols = start_cols)
  view_ui(input, output, session, dataset)
  # The nine analysis tabs get the Start-page columns (their token and f0
  # defaults), data_version (to clear their results when the rows change)
  # and start_settings (passed on to read_data_code() for their R code).
  normalised_ui(input, output, session, dataset, normalised_data, curated_data, curated_norm_data,
                start_cols = start_cols, data_version = data_version, start_settings = start_settings)
  visualise_ui(input, output, session, dataset, normalised_data, curated_norm_data,
               start_cols = start_cols, data_version = data_version, start_settings = start_settings)
  inspect_ui(input, output, session, dataset, inspect_result,
             start_cols = start_cols, data_version = data_version, start_settings = start_settings)
  cluster_ui(input, output, session, dataset, normalised_data, curated_data, cluster_data,
             start_cols = start_cols, data_version = data_version, start_settings = start_settings)
  curate_ui(input, output, session, dataset, normalised_data, curated_data, inspect_result, cluster_data, curated_norm_data,
            start_cols = start_cols, data_version = data_version, start_settings = start_settings)
  model_ui(input, output, session, dataset, normalised_data, curated_data, cluster_data, curated_norm_data,
           start_cols = start_cols, data_version = data_version, start_settings = start_settings)
  gca_ui(input, output, session, dataset, normalised_data, gca_pred_data, curated_data, cluster_data, curated_norm_data,
         start_cols = start_cols, data_version = data_version, start_settings = start_settings)
  gamm_ui(input, output, session, dataset, normalised_data, gamm_pred_data, curated_data, cluster_data, curated_norm_data,
          start_cols = start_cols, data_version = data_version, start_settings = start_settings)
  checklist_ui(input, output, session)
  filename_guide_ui(input, output, session)
  waveform_guide_ui(input, output, session)
  summarise_ui(input, output, session, dataset, normalised_data, gca_pred_data, gamm_pred_data, curated_data, curated_norm_data,
               start_cols = start_cols, data_version = data_version, start_settings = start_settings)

  # f0 processing tab modules
  fp_start_ui(input, output, session, fp_audio_data)
  # F0 Extraction returns the function its downloads use to add landmark and
  # metadata columns, so F0 Correction's downloads carry the same columns.
  fp_decorate_download <- fp_extraction_ui(input, output, session, fp_audio_data, fp_f0_data, fp_pitch_candidates, fp_metadata, fp_corrected_data)
  fp_correction_ui(input, output, session, fp_audio_data, fp_f0_data, fp_pitch_candidates, fp_corrected_data, fp_decorate_download)
  fp_praat_script_ui(input, output, session)

  # Feature-card navigation from the About tab.
  # Cards fire Shiny.setInputValue('about_nav_target', 'F0 Analysis|View'), etc.
  observeEvent(input$about_nav_target, {
    val <- input$about_nav_target
    parts <- strsplit(val, "|", fixed = TRUE)[[1]]
    if (length(parts) != 2) return()
    main <- parts[1]; sub <- parts[2]
    sub_id <- if (main == "F0 Analysis") "tabs_data"
              else if (main == "F0 Processing") "tabs_fp"
              else if (main == "Data Collection") "tabs_collection"
              else NULL
    updateNavbarPage(session, "main_nav", selected = main)
    if (!is.null(sub_id)) {
      updateTabsetPanel(session, sub_id, selected = sub)
    }
  })

  # ---- Deep links for the four top-level tabs (tab name only; never data) ----
  # The URL carries only which workspace is open, e.g. ?tab=analysis. For the
  # Data Collection tab only, the active sub-tab is also encoded as a URL hash,
  # e.g. ?tab=data-collection#reading-waveform. Uploaded data stays in session
  # memory and is never placed in the URL, so this does not affect the app's
  # privacy guarantees.
  nav_slug2tab <- c("about"           = "About",
                    "analysis"        = "F0 Analysis",
                    "processing"      = "F0 Processing",
                    "data-collection" = "Data Collection")
  nav_tab2slug <- stats::setNames(names(nav_slug2tab), unname(nav_slug2tab))

  # Sub-tabs of Data Collection only (slug <-> tab title), used for the hash.
  collection_slug2tab <- c("filename"         = "Filename",
                           "checklist"        = "Checklist",
                           "reading-waveform" = "Reading waveform")
  collection_tab2slug <- stats::setNames(names(collection_slug2tab),
                                         unname(collection_slug2tab))
  nav_ready <- reactiveVal(FALSE)

  # Build the URL for the current navigation state: ?tab=<slug>, plus a
  # #<sub-slug> hash only while the Data Collection tab is the open workspace.
  nav_current_url <- function() {
    slug <- nav_tab2slug[[input$main_nav]]
    if (is.null(slug) || is.na(slug)) return(NULL)
    url <- paste0("?tab=", slug)
    if (identical(input$main_nav, "Data Collection")) {
      sub <- collection_tab2slug[[input$tabs_collection]]
      if (!is.null(sub) && !is.na(sub)) url <- paste0(url, "#", sub)
    }
    url
  }

  # On load: open the tab named in ?tab=<slug> (if any) and, for Data
  # Collection, the sub-tab named in the #<hash>; then start syncing.
  observeEvent(session$clientData$url_search, {
    slug <- parseQueryString(session$clientData$url_search)[["tab"]]
    if (!is.null(slug) && slug %in% names(nav_slug2tab)) {
      updateNavbarPage(session, "main_nav", selected = nav_slug2tab[[slug]])
      if (identical(nav_slug2tab[[slug]], "Data Collection")) {
        hash <- session$clientData$url_hash
        hash <- if (is.null(hash)) "" else sub("^#", "", hash)
        if (nzchar(hash) && hash %in% names(collection_slug2tab)) {
          updateTabsetPanel(session, "tabs_collection",
                            selected = collection_slug2tab[[hash]])
        }
      }
    }
    nav_ready(TRUE)
  }, once = TRUE, ignoreNULL = FALSE)

  # Reflect the active top tab in the URL whenever it changes.
  observeEvent(input$main_nav, {
    if (!isTRUE(nav_ready())) return()
    url <- nav_current_url()
    if (!is.null(url)) updateQueryString(url, mode = "push")
  })

  # Reflect the active Data Collection sub-tab as a URL hash when it changes
  # (only while Data Collection is the open workspace).
  observeEvent(input$tabs_collection, {
    if (!isTRUE(nav_ready())) return()
    if (!identical(input$main_nav, "Data Collection")) return()
    url <- nav_current_url()
    if (!is.null(url)) updateQueryString(url, mode = "push")
  })

}