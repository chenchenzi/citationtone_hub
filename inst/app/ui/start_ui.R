#######################################
# Simplified UI for loading a CSV file
#######################################

start_ui <- function(input, output, session, dataset, raw_dataset, attached_metadata,
                     duration_specs = NULL, prepared = NULL, screened = NULL,
                     f0_screen = NULL, screen_plan = NULL, screen_box = NULL,
                     start_cols = NULL) {
# Render UI for uploading a CSV file
output$ui_fileUpload <- renderUI({
  tagList(
    fileInput("uploadfile", "Choose a CSV File",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    tags$div(style = "margin-top: -8px;",
      checkboxInput("convert_to_factor",
                    "String variables as factors (recommended)", FALSE)
    ),
    tags$hr(style = "margin: 10px 0;"),
    tags$details(
      tags$summary(style = "cursor: pointer; font-weight: 600; color: #2c5f4f; font-size: 0.9rem;",
                   icon("tags"), " Attach metadata (optional)"),
      tags$div(style = "padding-top: 8px;",
        tags$p(style = "font-size: 0.8rem; color: #777; margin: 0 0 8px 0;",
          "Already have an f0 dataframe without speaker / tone / item columns (e.g. straight from ",
          "Praat)? Add them here after uploading, and they flow to every F0 Analysis tab."),
        radioButtons("meta_source", NULL,
          choices = c("Off" = "none",
                      "Derive from filename" = "derive",
                      "Upload metadata CSV"  = "csv"),
          selected = "none"),
        conditionalPanel("input.meta_source == 'csv'",
          fileInput("meta_file", "Metadata CSV:",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          tags$div(style = "font-size: 0.78rem; color: #888; margin: -6px 0 6px 0;",
            "Both files need a column identifying each recording (usually the audio filename). ",
            "Pick it on each side below; the columns can be named differently, but their values must match.")),
        conditionalPanel("input.meta_source != 'none'",
          uiOutput("meta_key_ui")),
        conditionalPanel("input.meta_source == 'derive'",
          tags$div(style = "font-size: 0.78rem; color: #888; margin: -4px 0 6px 0;",
            "Its values should encode the metadata to split out (e.g. ",
            tags$code("S01_T1_rep1"), " becomes speaker, tone, rep). ",
            tags$a(href = "#", style = "color: #2c5f4f; font-weight: 600;",
              onclick = "Shiny.setInputValue('about_nav_target', 'Data Collection|Filename', {priority:'event'}); return false;",
              "See the filename guide.")),
          textInput("meta_sep", "Separator:", value = "_"),
          uiOutput("meta_status"),
          textInput("meta_colnames", "Column names:",
            placeholder = "e.g. language speaker tone word rep"),
          tags$div(style = "font-size: 0.78rem; color: #888; margin: -6px 0 0 0;",
            "Type names in order, separated by spaces or commas."),
          uiOutput("meta_preview")),
        conditionalPanel("input.meta_source == 'csv'",
          uiOutput("meta_join_ui"),
          tags$div(style = "font-size: 0.78rem; color: #888; margin: -6px 0 8px 0;",
            "It may be called ", tags$code("token"), ", ", tags$code("filename"),
            ", ", tags$code("basename"), ", ... and values may include a ",
            tags$code(".wav"), " extension."),
          checkboxInput("meta_strip_ext", "Strip file extensions when matching", value = TRUE),
          uiOutput("meta_csv_status")),
        conditionalPanel("input.meta_source != 'none'",
          tags$div(style = "margin-top: 6px;",
            actionButton("meta_apply", "Apply metadata", class = "btn btn-primary"))),
        uiOutput("meta_applied")
      )
    ),
    tags$details(style = "margin-top: 8px;",
      tags$summary(style = "cursor: pointer; font-weight: 600; color: #2c5f4f; font-size: 0.9rem;",
                   icon("stopwatch"), " Add contour duration (optional)"),
      tags$div(style = "padding-top: 8px;",
        tags$p(style = "font-size: 0.8rem; color: #777; margin: 0 0 8px 0;",
          "Adds how long each f0 contour lasts: the time from the first to the last sample ",
          "with f0 in each unit, on every row of that unit. A gap inside the contour does ",
          "not shorten it, and a lone sample cut off from it by a gap (e.g. a tracking ",
          "artefact in the silence) does not stretch it. The result is the same whether or ",
          "not the rows without f0 are removed."),
        uiOutput("dur_ui"),
        tags$div(style = "margin-top: 6px;",
          actionButton("dur_apply", "Add duration column", class = "btn btn-primary")),
        uiOutput("dur_applied")
      )
    )
  )

})

# Render instructional texts
# IMPORTANT: do NOT depend on dataset() here: this renderUI builds the
# "Try with our sample data" actionButton, and re-rendering it would
# destroy + recreate the button, resetting input$try_sample back to 0
# and immediately wiping out the data the user just loaded.
output$instruction_text <- renderUI({
  tagList(
    h2("Welcome!"),
    p("Upload a CSV file to get started. Your file should have column headers and ideally contain
      columns for time, f0, tone category, speaker ID, and token ID. ",
      "Or click ", tags$em("Try with our sample data"),
      " below to load an example."),
    # Coming from F0 Correction: folded, as most users do not
    guide_box("Continuing from the F0 Correction tab?", open = FALSE,
      tags$p(style = "margin: 0;", st_inline(
        "Upload its ", tags$code("all_correctedf0.csv"), " here directly. Tokens you discarded ",
        "there (", tags$code("token_dropped = TRUE"), ") are found on upload and left out of the ",
        "working data by default, and ", tags$em("Data screening"), " uses ", tags$code("f0_corrected"),
        "."))),
    tags$hr(),
    h4("What each tab does"),
    # One Font Awesome icon per tab on a pale circle coloured by the stage of
    # the work (tab_item() and tab_list() in global.R, shared with the F0
    # Processing Start page)
    tab_list(
      tab_item("folder-open", "Start", "data", "Upload your CSV data, screen it for rows without f0 and preview its structure."),
      tab_item("table", "View", "data", "Browse the full dataset in an interactive table with sorting and search."),
      tab_item("sliders", "Normalise", "prep", "Apply f0 normalisation (semitone, z-score or log z-score) by speaker, and download the result."),
      tab_item("chart-line", "Visualise", "prep", "Plot f0 contours coloured by tone category, with optional faceting by speaker. Save the plot or view the corresponding R code."),
      tab_item("magnifying-glass", "Inspect", "prep", "Flag potential f0 outliers and pitch tracking artefacts using by-speaker z-scores and sample-to-sample jump detection."),
      tab_item("shapes", "Cluster", "expl", "Group tokens by the shape of their f0 contour, without tone labels, to see how many tone categories the data suggest."),
      tab_item("tags", "Curate", "expl", "Relabel tokens whose tone category needs revising, or exclude tokens; Normalise and the later tabs can use the curated data."),
      tab_item("superscript", "Model: Polynomials", "model", "Fit Legendre polynomials to each token\u2019s f0 contour. Returns one row per token with coefficients capturing the shape of contour."),
      tab_item("arrow-trend-up", "Model: GCA", "model", "Run Growth Curve Analysis using a mixed-effects model with orthogonal polynomials and configurable random effects by speaker and item."),
      tab_item("bezier-curve", "Model: GAMM", "model", "Fit Generalised Additive Mixed Models (GAMMs) to f0 contours with configurable basis functions, random effects, and AR1 autocorrelation correction."),
      tab_item("list-ol", "Summarise", "sum", "Convert tone contours into Chao tone numerals (1\u20135 scale). Compare reference-line and interval-based conversion methods.")
    ),
    tags$hr(),
    # --- What the data look like (with our sample as a concrete example) ---
    h4("What the data look like"),
    tags$p("Shinytone expects a CSV with at least the following columns ",
           "(names can differ, but the content should be):"),
    tags$ul(
      tags$li(nows(tags$strong("time"), ": time index within each token (e.g., in seconds)")),
      tags$li(nows(tags$strong("f0"), ": fundamental frequency in Hz")),
      tags$li(nows(tags$strong("tone category"), ": tone label (e.g., T1, T2, ...)")),
      tags$li(nows(tags$strong("speaker"), ": speaker ID (for by-speaker normalisation)")),
      tags$li(nows(tags$strong("token"), ": unique recording / utterance ID"))
    ),
    tags$p(st_inline("Pitch trackers often write a row for every frame, with no f0 (NA or 0 Hz) ",
           "where there is no voicing. ", tags$em("Data screening"), ", in the sidebar, checks ",
           "for these rows without f0 and removes any it finds by default.")),
    tags$p("Here's what our sample data looks like (first 5 rows):"),
    {
      # Read the bundled sample CSV that lives in www/ alongside the JS
      # handler that fetches it for "Try with our sample data".
      sample_preview <- tryCatch(
        utils::read.csv("www/dc21f0_test.csv",
                        stringsAsFactors = FALSE,
                        check.names = FALSE,
                        nrows = 5),
        error = function(e) NULL
      )
      if (is.null(sample_preview)) {
        tags$div(style = "color: #888; font-style: italic;",
                 "Sample preview unavailable.")
      } else {
        tags$div(style = "overflow-x: auto; margin-bottom: 8px;",
          tags$table(class = "table table-sm table-bordered",
            style = "font-size: 0.82rem; margin-bottom: 0;",
            tags$thead(
              tags$tr(lapply(names(sample_preview), function(c) tags$th(c)))
            ),
            tags$tbody(
              lapply(seq_len(nrow(sample_preview)), function(i)
                tags$tr(lapply(names(sample_preview),
                               function(c) tags$td(as.character(sample_preview[i, c])))))
            )
          )
        )
      }
    },
    tags$p(style = "color: #777; font-size: 0.82rem; font-style: italic;",
      "Source: a subset of the citation-tone recordings from ",
      tags$a(href = "https://doi.org/10.1515/phon-2025-0001",
             target = "_blank", rel = "noopener noreferrer",
             "Xu (2025)"),
      ", ",
      tags$em("Plastic Mandarin tones: regional identity in prosody"),
      ", Phonetica 82(5), 331\u2013362."
    ),
    tags$div(style = "margin: 12px 0;",
      actionButton("try_sample", "Try with our sample data", icon = icon("flask"))
    ),
    tags$p(style = "color: #777; font-size: 0.82rem; margin-top: -4px;",
      "Uploading a file (or the sample) starts afresh: results from the previous file, ",
      "such as normalised data, curation and fitted models, are cleared."),
    tags$hr()
  )
})

# ---- Discarded tokens (token_dropped) --------------------------------------
# The F0 Correction tab's all_correctedf0.csv keeps every token and marks
# whole-token discards with token_dropped = TRUE. Detect the column on upload,
# tell the user how many tokens it flags, and offer a one-click exclusion
# (default on). The filter itself lives in the shared dataset() reactive
# (server.R); the raw upload is never modified, so unticking restores the
# full data. Kept out of ui_fileUpload: re-rendering that output would
# destroy the fileInput and wipe the upload.

# Count discarded tokens in the raw upload. NULL when the column is absent.
dropped_token_info <- reactive({
  raw <- raw_dataset()
  if (is.null(raw) || !"token_dropped" %in% names(raw)) return(NULL)
  td <- suppressWarnings(as.logical(raw$token_dropped))
  if ("token" %in% names(raw)) {
    list(n_drop = length(unique(raw$token[td %in% TRUE])),
         n_tot  = length(unique(raw$token)), unit = "token")
  } else {
    list(n_drop = sum(td %in% TRUE), n_tot = nrow(raw), unit = "row")
  }
})

output$ui_dropped_tokens <- renderUI({
  info <- dropped_token_info()
  if (is.null(info)) return(NULL)
  if (info$n_drop == 0) {
    return(tags$div(style = "font-size: 0.78rem; color: #888; margin: -4px 0 6px 0;",
      tags$code("token_dropped"), " column detected; no ",
      paste0(info$unit, "s"), " are marked discarded."))
  }
  tagList(
    tags$div(style = "margin-top: -4px;",
      checkboxInput("exclude_dropped",
                    sprintf("Exclude %s discarded %s%s (recommended)",
                            fmt_count(info$n_drop), info$unit, if (info$n_drop == 1) "" else "s"),
                    TRUE)),
    tags$div(style = "font-size: 0.78rem; color: #888; margin: -10px 0 6px 0;",
      sprintf("%s of %s %ss in this file carry ", fmt_count(info$n_drop), fmt_count(info$n_tot), info$unit),
      tags$code("token_dropped = TRUE"),
      " (whole-token discards from the ", tags$strong("F0 Correction"), " tab). ",
      "Excluding removes those rows (and the flag column) from every F0 Analysis ",
      "tab; the uploaded file itself is untouched.")
  )
})

# One-off notification when an upload carries discarded tokens, so the
# exclusion is never silent even if the user skips the sidebar.
observeEvent(raw_dataset(), {
  info <- dropped_token_info()
  if (is.null(info) || info$n_drop == 0) return()
  showNotification(
    sprintf(paste0("This file marks %s of %s %ss as discarded (token_dropped). ",
                   "They are excluded from the working dataset. Untick ",
                   "\"Exclude discarded %ss\" in the sidebar to keep them."),
            fmt_count(info$n_drop), fmt_count(info$n_tot), info$unit, info$unit),
    type = "message", duration = 8, id = "f0a_dropped_tokens")
})

# ---- Data screening: rows without f0 (the screen) ------------------------
# The sidebar box "Data screening": the token, time and f0 pickers and the
# boxes that remove the rows (D2), also remove short edge runs (D6), read 0 /
# -1 as no f0 (D3), mark gaps on an upload whose rows all have f0 and drop
# the flagged tokens (a screen_flag column gives their reasons). The screen
# itself, the flags, the drop and the removal run in server.R (f0_screen(),
# screen_fix(), fixed(), screened()).
# The box is drawn once per upload, from the column names only: a box drawn
# again would go back to its default and undo the user's untick. Its labels
# and values are then updated in place (updateCheckboxInput), and a box is
# shown only when it has something to do (the screen_show_* flags below).
# Kept out of ui_fileUpload, which must never be drawn again.
has_screen <- is.function(f0_screen) && is.function(screen_plan) &&
  is.function(screen_box)

# The values a box offers to read as no f0, as text: c(0, -1) -> "0 / -1".
unv_text <- function(v, sep = " / ") paste(format(v, trim = TRUE), collapse = sep)

# The boxes' labels for the current screen, or NULL. drop_flagged counts
# the flagged tokens (screen_fix(), server.R); remove, short and mark
# count what is left after the drop (the plan's post screen).
screen_labels <- reactive({
  if (!has_screen) return(NULL)
  scr <- f0_screen()
  if (is.null(scr)) return(NULL)
  plan <- screen_plan()
  s  <- if (!is.null(scr$screen)) attr(scr$screen, "summary")
  ps <- if (!is.null(plan$post)) attr(plan$post, "summary") else s
  without <- if (!is.null(plan$without)) plan$without else scr$without
  list(
    unvoiced = if (length(scr$suggested))
      sprintf("Read %s in %s as no f0 (%s)", unv_text(scr$suggested),
              scr$cols$f0, count_label(scr$unvoiced_rows, "row"))
      else sprintf("Read 0 / -1 in %s as no f0", scr$cols$f0),
    drop_flagged = drop_flagged_label(plan),
    remove = sprintf("Remove %s without f0 (recommended)", count_label(without, "row")),
    short = sprintf(paste("Also remove runs of 1\u20132 samples cut off by a gap at",
                          "a token's start or end (%s)"),
                    count_label(if (is.null(ps)) 0 else ps[["short_tokens"]], "token")),
    mark = sprintf("Mark the %s in a voiced_run column (recommended)",
                   count_label(if (is.null(ps)) 0 else ps[["gaps_time_only"]] + ps[["gaps_run"]], "gap")))
})

# The drop box's label: "Drop the 3 flagged tokens", "Drop the flagged
# token", with " (every token)" when it would take every token. Also the
# card's name for the box (`all = FALSE` leaves the part in brackets out).
drop_flagged_label <- function(plan, all = TRUE) {
  n   <- if (is.null(plan$flagged)) 0L else nrow(plan$flagged)
  all <- all && isTRUE(plan$all_flagged)
  if (n == 1) {
    paste0("Drop the flagged token", if (all) " (the only token)" else "")
  } else {
    sprintf("Drop the %s flagged tokens%s", fmt_count(n), if (all) " (every token)" else "")
  }
}

# When each box is shown (conditionalPanel conditions in ui_f0_screen). The
# drop is offered when some token is flagged. Removal and marking read
# what is left after the drop (the plan's post screen), as the plan does
# (screen_plan(), server.R), the same for every kind of file. The short
# runs are offered only in a frame file: in an equal-point file a gap
# flags its token, which the drop can leave out.
screen_flags <- reactive({
  out <- list(unvoiced = FALSE, drop_flagged = FALSE, remove = FALSE, short = FALSE,
              mark = FALSE, need_token = FALSE)
  if (!has_screen) return(out)
  scr  <- f0_screen()
  plan <- screen_plan()
  if (is.null(scr) || is.null(plan)) return(out)
  s  <- if (!is.null(scr$screen)) attr(scr$screen, "summary")
  ps <- if (!is.null(plan$post)) attr(plan$post, "summary") else s
  out$unvoiced  <- length(scr$suggested) > 0
  out$drop_flagged <- isTRUE(plan$flag)
  out$remove    <- !is.null(ps) && isTRUE(plan$without > 0)
  out$mark      <- !is.null(ps) && isTRUE(plan$without == 0) && ps[["gaps_time_only"]] > 0
  out$short     <- !is.null(ps) && !isTRUE(plan$points) && (plan$remove || plan$mark) &&
    ps[["short_runs"]] > 0
  out$need_token <- is.null(scr$cols$token) && isTRUE(scr$without > 0)
  out
})
# Plain output functions (not reactive()), which shiny::testServer() can
# also read; the value reaches the browser as true or false.
output$screen_show_unvoiced  <- function(...) isTRUE(screen_flags()$unvoiced)
output$screen_show_drop_flagged <- function(...) isTRUE(screen_flags()$drop_flagged)
output$screen_show_remove    <- function(...) isTRUE(screen_flags()$remove)
output$screen_show_short     <- function(...) isTRUE(screen_flags()$short)
output$screen_show_mark      <- function(...) isTRUE(screen_flags()$mark)
output$screen_need_token     <- function(...) isTRUE(screen_flags()$need_token)
for (.id in c("screen_show_unvoiced", "screen_show_drop_flagged",
              "screen_show_remove", "screen_show_short", "screen_show_mark",
              "screen_need_token")) {
  outputOptions(output, .id, suspendWhenHidden = FALSE)
}

# The box's heading, shown before an upload too, so the page's intro has
# something to point at.
screen_head <- function() {
  tags$div(style = "font-weight: 600; color: #2c5f4f; font-size: 0.9rem; margin-bottom: 6px;",
           icon("filter"), " Data screening")
}
output$ui_f0_screen <- renderUI({
  if (!has_screen) return(NULL)
  raw <- raw_dataset()
  if (is.null(raw)) {
    return(tags$div(style = "margin-top: 12px;", screen_head(),
      tags$div(style = "font-size: 0.78rem; color: #6b6b6b;",
               "Upload a file to choose the f0, token and time variables and check for rows without f0.")))
  }
  vars <- names(raw)
  ch   <- start_screen_choices(raw)
  # The columns the screen uses for this upload (server.R: the user's picks
  # for this upload, else the defaults). Never the pickers' input values:
  # after a new upload they still hold the last file's columns.
  sel  <- if (is.function(start_cols)) isolate(start_cols())
  if (is.null(sel)) sel <- resolve_start_cols(raw)
  labs <- isolate(screen_labels())
  box <- function(key) {
    checkboxInput(paste0("screen_", key),
                  if (is.null(labs)) key else labs[[key]],
                  isolate(screen_box(key)))
  }
  tok_choices <- ch$token
  names(tok_choices) <- ifelse(tok_choices == "", "(none)", tok_choices)
  token_sel <- selectInput("screen_token", "Token ID variable:",
                           choices = tok_choices,
                           selected = if (is.null(sel$token)) "" else sel$token)
  time_sel <- selectInput("screen_time", "Time variable:", choices = ch$time,
                          selected = sel$time)
  f0_sel <- selectInput("screen_f0", "f0 variable:", choices = ch$f0, selected = sel$f0)
  # Without a token column, the token and time pickers are offered only when
  # rows lack f0, or when the user asks to check the time steps for gaps.
  tok_block <- if (!is.null(vis_token_col(vars))) tagList(token_sel, time_sel) else tagList(
    conditionalPanel("!output.screen_need_token && (input.screen_token || '') == ''",
      checkboxInput("screen_check_gaps", "Check the time steps for gaps", FALSE)),
    conditionalPanel(paste("output.screen_need_token || input.screen_check_gaps ||",
                           "(input.screen_token || '') != ''"),
      token_sel, time_sel))
  tags$div(style = "margin-top: 12px;",
    screen_head(),
    f0_sel,
    tok_block,
    conditionalPanel("output.screen_show_unvoiced", box("unvoiced")),
    conditionalPanel("output.screen_show_drop_flagged", box("drop_flagged")),
    conditionalPanel("output.screen_show_remove", box("remove")),
    conditionalPanel("output.screen_show_mark", box("mark")),
    conditionalPanel("output.screen_show_short", box("short"),
      tags$div(style = "font-size: 0.78rem; color: #6b6b6b; margin: -10px 0 8px 0;",
               "These samples have f0, so removing them moves their tokens' 0\u20131 time axes.")),
    uiOutput("ui_f0_screen_hint"))
})
outputOptions(output, "ui_f0_screen", suspendWhenHidden = FALSE)

# Labels follow the screen, values follow the boxes (a box the user has not
# changed follows its default). Never drawn again, so no choice is undone.
observe({
  labs <- screen_labels()
  if (is.null(labs)) return()
  for (k in names(labs)) {
    updateCheckboxInput(session, paste0("screen_", k), label = labs[[k]],
                        value = screen_box(k))
  }
})

# One line under the box: what the screen found, or what it needs.
output$ui_f0_screen_hint <- renderUI({
  if (!has_screen) return(NULL)
  scr <- f0_screen()
  if (is.null(scr)) return(NULL)
  grey  <- "font-size: 0.78rem; color: #888; margin: -4px 0 6px 0;"
  amber <- paste("font-size: 0.78rem; color: #6b5a1e; background: #fff8e1;",
                 "border-radius: 4px; padding: 5px 8px; margin: 0 0 6px 0;")
  if (!is.null(scr$error)) {
    return(tags$div(style = amber, "Could not screen the rows: ", scr$error))
  }
  if (is.null(scr$screen)) {
    if (scr$without > 0) {
      return(tags$div(style = amber, icon("lightbulb"),
        sprintf(" %s of %s %s no f0. Choose the token variable to see where %s and to remove %s.",
                fmt_count(scr$without), count_label(scr$rows, "row"),
                n_verb(scr$without, "has", "have"), n_verb(scr$without, "it is", "they are"),
                n_verb(scr$without, "it", "them"))))
    }
    return(tags$div(style = grey, "Every row has f0."))
  }
  # An f0 variable with no f0 at all needs another
  tags$div(style = grey,
    if (scr$rows > 0 && isTRUE(scr$without == scr$rows))
      "No row has f0 in this f0 variable. Choose another f0 variable. "
    else if (scr$without > 0)
      sprintf("%s of %s %s no f0. ", fmt_count(scr$without), count_label(scr$rows, "row"),
              n_verb(scr$without, "has", "have"))
    else "Every row has f0. ",
    "See the report above the Data Preview. The other tabs start with these f0 and token variables.")
})
outputOptions(output, "ui_f0_screen_hint", suspendWhenHidden = FALSE)

# Facts for the report beyond screen_f0_rows(): other Hz columns, rows
# deleted in F0 Correction, token-level columns that change within a token,
# and the time axis decision before and after removal.
screen_extra <- reactive({
  if (!has_screen || !is.function(prepared)) return(NULL)
  scr <- f0_screen()
  d   <- prepared()
  if (is.null(scr) || is.null(d)) return(NULL)
  f0c <- scr$cols$f0
  num <- function(x) suppressWarnings(as.numeric(if (is.factor(x)) as.character(x) else x))
  x <- num(d[[f0c]])
  if (length(scr$unvoiced)) x[x %in% scr$unvoiced] <- NA
  no_f0 <- !is.finite(x)
  raw <- raw_dataset()
  hz  <- tryCatch(hz_f0_columns(raw), error = function(e) character(0))
  # hz_f0_columns() needs values to judge a column; one with no f0 at all
  # counts as Hz when its name is an Hz name (f0, F0_Hz, pitch, ...).
  by_name <- !any(is.finite(x)) &&
    length(tryCatch(hz_f0_columns(stats::setNames(data.frame(200), f0c)),
                    error = function(e) character(0))) > 0
  out <- list(is_hz = f0c %in% hz || by_name)
  # Other Hz columns: their rows without f0, and the rows where they and the
  # screened column disagree on having f0.
  other <- setdiff(intersect(hz, names(d)), f0c)
  out$other_hz <- lapply(stats::setNames(other, other), function(cn) {
    o <- !is.finite(num(d[[cn]]))
    c(without = sum(o), disagree = sum(o != no_f0))
  })
  # F0 Correction: rows the user deleted (edited = TRUE) among the rows
  # without f0, as against tracker gaps.
  if ("edited" %in% names(d) && (f0c == "f0_corrected" || "f0_original" %in% names(d))) {
    ed <- suppressWarnings(as.logical(d$edited)) %in% TRUE
    out$deleted <- sum(no_f0 & ed)
  }
  tok <- scr$cols$token
  if (!is.null(tok) && tok %in% names(d)) {
    vars <- names(d)
    first <- function(pats, pool) {
      for (p in pats) { m <- grep(p, pool, ignore.case = TRUE, value = TRUE); if (length(m)) return(m[1]) }
      NULL
    }
    # Landmark columns (a tier's label, _start, _end, _i and the axes made
    # from it) and duration columns change within a token by design: a
    # syllable tier's '^syllable' would otherwise match syllable_start.
    lm <- names(landmark_sets(vars))
    lm_cols <- c(lm, if (length(lm)) c(outer(lm, c("_start", "_end", "_i", "_t01", "_tseq", "_tbound"),
                                             paste0)))
    pool <- setdiff(vars, c(tok, scr$cols$time, f0c, hz, "voiced_run", lm_cols))
    pool <- pool[!grepl("_(t01|tseq|tbound|f0_dur)$", pool)]
    m <- if (is.function(attached_metadata)) attached_metadata()
    cand <- unique(c(first(var_patterns$speaker, pool), first(var_patterns$tone, pool),
                     first(var_patterns$item, pool),
                     intersect(if (!is.null(m)) setdiff(names(m$data), m$key), pool)))
    tid <- match(d[[tok]], unique(d[[tok]]))
    vary <- vapply(cand, function(cn) {
      v <- as.character(d[[cn]])
      u <- !duplicated(cbind(tid, match(v, unique(v))))
      sum(tabulate(tid[u], max(tid, 0L)) > 1L)
    }, integer(1))
    out$varying <- vary[vary > 0]
    # The time axis decision of the fits (resolve_time_norm()) before and
    # after removing the rows without f0.
    tcol <- scr$cols$time
    if (!is.null(tcol) && tcol %in% vars && any(no_f0)) {
      before <- isTRUE(tryCatch(time_already_normalised(d[[tcol]], d[[tok]], name = tcol),
                                error = function(e) FALSE))
      after  <- isTRUE(tryCatch(time_already_normalised(d[[tcol]][!no_f0], d[[tok]][!no_f0],
                                                        name = tcol),
                                error = function(e) FALSE))
      if (before != after) out$time_flip <- c(before = before, after = after)
    }
  }
  out
})

# ---- The report above the Data Preview -------------------------------------
# Two cards. "Your data": what the upload holds (tokens, speakers, tones,
# rows), how its f0 is sampled, the columns used, and a time-step problem
# only when it matters (start_data_facts(), global.R, on prepared(): before
# any removal). "Data screening": the rows without f0, where they are,
# what the boxes do with them, and a folded note on saving the copy. The
# styles are in www/styles.css, scoped to .st-report; ui.R does not link that
# file, so the report brings it as a dependency.
st_css <- htmltools::htmlDependency("shinytone-start-report", "1.1.0",
                                    src = c(href = "."), stylesheet = "styles.css")

# The facts depend only on the upload and the screen's columns, not on the
# boxes, so they are kept apart from the report.
screen_facts <- reactive({
  if (!has_screen || !is.function(prepared)) return(NULL)
  scr <- f0_screen()
  d   <- prepared()
  if (is.null(scr) || is.null(d)) return(NULL)
  tryCatch(start_data_facts(d, scr), error = function(e) NULL)
})

# Inline HTML from text and tags with no whitespace between them (as nows()),
# with the text escaped: token names are data.
st_inline <- function(...) {
  parts <- lapply(list(...), function(x) {
    if (is.null(x)) return("")
    if (is.character(x) && !inherits(x, "html")) return(htmltools::htmlEscape(paste(x, collapse = "")))
    paste(as.character(x), collapse = "")
  })
  HTML(paste0(unlist(parts), collapse = ""))
}
# Token names as small code chips: "a, b and c", or the first 5 and "and 12
# more", with "(no name)" for a blank name. `after` is text after each
# chip, such as a count: "a (3)". `last`
# goes before the last chip: ", " for a list of tokens with their reasons,
# "a (no f0), b (only 2 points with f0)".
st_toks <- function(x, n = 5, after = NULL, last = " and ") {
  x <- as.character(x)
  # A blank token name (an empty cell) would show as an empty chip
  x[!is.na(x) & !nzchar(trimws(x))] <- "(no name)"
  k <- min(length(x), n)
  more <- length(x) - k
  parts <- list()
  for (i in seq_len(k)) {
    parts <- c(parts, list(tags$code(class = "st-tok", x[i])), if (!is.null(after)) list(after[i]))
    if (i < k - 1 || (i == k - 1 && more > 0)) parts <- c(parts, list(", "))
    else if (i == k - 1) parts <- c(parts, list(last))
  }
  if (more > 0) parts <- c(parts, list(sprintf(" and %s more", fmt_count(more))))
  do.call(st_inline, parts)
}
# The flagged tokens (flag_f0_tokens()'s "flagged" table) of an equal-point
# file as a bulleted list, one line per reason that has tokens, in this
# order: points without f0 inside, no f0 at the first or last point, fewer
# than 3 points with f0 (a token with no f0 at all among them). A line is
# the reason and its tokens as code chips, in the order of the table, at
# most 5 and then "and 36 more", with no count. The numbers in a reason
# ("3 points without f0 inside", "only 2 points with f0") are left out:
# the screen_flag column has them. A token with two reasons is on both
# lines.
st_flag_kind <- function(r) {
  ifelse(r == "no f0" | startsWith(r, "only "), "few",
  ifelse(startsWith(r, "no f0 at the "), "edge",
  ifelse(endsWith(r, " without f0 inside"), "inside", "")))
}
st_flag_list <- function(fl, style = "margin: 4px 0; padding-left: 20px;") {
  if (is.null(fl) || !nrow(fl)) return(NULL)
  why <- strsplit(as.character(fl$reason), "; ", fixed = TRUE)
  labs <- c(inside = "Points without f0 inside", edge = "No f0 at the first or last point",
            few = "Fewer than 3 points with f0")
  items <- lapply(names(labs), function(k) {
    hit <- vapply(why, function(r) k %in% st_flag_kind(r), logical(1))
    if (any(hit)) tags$li(st_inline(labs[[k]], ": ", st_toks(fl[[1L]][hit], last = ", ")))
  })
  tags$ul(class = "st-flag-list", style = style, Filter(Negate(is.null), items))
}
# The flagged tokens of a frame file in one run of chips, in the order of
# the table: the only reasons there are no f0 and 1 or 2 samples with f0,
# both fewer than 3. "a, b, c, d, e and 2 more".
st_flag_toks <- function(fl) st_toks(fl[[1L]], last = ", ")
st_chip <- function(text, kind = "ok", icon_name = NULL) {
  tags$span(class = paste0("st-chip st-chip-", kind),
            if (!is.null(icon_name)) icon(icon_name), text)
}
# A fact: an icon, a bold number with a muted label, and a line under it.
st_fact <- function(icon_name, num, label = NULL, sub = NULL, class = NULL) {
  tags$div(class = paste(c("st-fact", class), collapse = " "),
    tags$span(class = "st-fact-icon", icon(icon_name)),
    tags$div(class = "st-fact-body st-fact-line", tags$span(class = "st-num", num),
             if (!is.null(label)) tags$span(class = "st-label", label)),
    if (!is.null(sub)) tags$div(class = "st-sub", sub))
}
# "0-1" (with an en dash) that never breaks across lines.
st_01 <- function() tags$span(class = "st-nw", "0\u20131")
st_callout <- function(..., kind = "warn", icon_name = "triangle-exclamation") {
  tags$div(class = paste0("st-callout st-callout-", kind),
           tags$span(class = "st-callout-icon", icon(icon_name)), tags$div(...))
}
st_more <- function(summary, ...) {
  tags$details(class = "st-more", tags$summary(summary), tags$div(class = "st-more-body", ...))
}
st_decimals_txt <- function(n) if (identical(n, 1L)) "1 decimal" else paste(n, "decimals")

# A time-step problem from start_grid_problems() as one sentence with its fix.
st_problem <- function(key, p) {
  res_txt <- function(res, unit) paste(format(res), if (identical(unit, "s")) "s" else "ms")
  switch(key,
    rounded = {
      dec <- start_decimals(p$step)
      if (isTRUE(abs(p$step / p$resolution - 1) < 0.02)) {
        sprintf(paste("Gaps inside tokens can't be found: the times are rounded to %s, the same",
                      "as the step between %s (%s), so a longer step could be a gap or rounding.",
                      "Save the times with %s to find them."),
                res_txt(p$resolution, p$unit), p$noun, start_step_text(p$step, p$unit),
                st_decimals_txt(dec))
      } else {
        sprintf(paste("Gaps inside tokens can't be found: the times are rounded to %s, but the",
                      "%s are about %s apart. Save the times with %s (%s, not %s) to find them."),
                res_txt(p$resolution, p$unit), p$noun, start_step_text(p$step, p$unit),
                st_decimals_txt(dec), formatC(p$step, format = "f", digits = dec),
                format(p$resolution))
      }
    },
    uneven = sprintf("The %s are not evenly spaced, so gaps inside tokens can't be found from the times.",
                     p$noun),
    repeated = if (isTRUE(p$all)) {
      "Every token has two rows with the same time, so gaps inside tokens can't be found."
    } else {
      n <- length(p$tokens)
      st_inline(sprintf("%s %s two rows with the same time, so gaps inside %s can't be found: ",
                        count_label(n, "token"), n_verb(n, "has", "have"), n_verb(n, "it", "them")),
                st_toks(p$tokens), ".")
    },
    other_step = {
      n <- length(p$tokens)
      st_inline(sprintf(paste("In %s, the %s are not %s apart (about %s), and the times are",
                              "rounded to %s, so gaps inside %s can't be found: "),
                        count_label(n, "token"), p$noun, start_step_text(p$usual, p$unit),
                        start_step_text(p$step, p$unit), res_txt(p$resolution, p$unit),
                        n_verb(n, "it", "them")),
                st_toks(p$tokens),
                sprintf(". Save the times with %s to find them.", st_decimals_txt(start_decimals(p$step))))
    },
    NULL)
}

# The "Your data" card.
st_your_data <- function(fa, scr, ex) {
  cols <- fa$cols
  facts <- list()
  if (!is.null(fa$tokens)) {
    facts <- c(facts, list(st_fact("layer-group", fmt_count(fa$tokens), n_verb(fa$tokens, "token", "tokens"))))
  } else if (is.null(cols$token)) {
    # How to choose a token column is said once, in the "Data screening"
    # card (st_rows_card()); this tile and the f0 data tile only point there.
    facts <- c(facts, list(st_fact("layer-group", "No token variable", NULL,
                                   tagList("Tokens not counted", tags$span(class = "st-see", "see below")),
                                   class = "st-fact-text")))
  }
  if (!is.null(fa$speakers)) {
    facts <- c(facts, list(st_fact("users", fmt_count(fa$speakers$n),
                                   n_verb(fa$speakers$n, "speaker", "speakers"))))
  }
  if (!is.null(fa$tones)) {
    v <- fa$tones$values
    facts <- c(facts, list(st_fact("music", fmt_count(fa$tones$n), n_verb(fa$tones$n, "tone", "tones"),
                                   if (length(v)) paste(v, collapse = ", "))))
  }
  without <- if (is.na(fa$without)) 0L else fa$without
  facts <- c(facts, list(st_fact("table-list", fmt_count(fa$rows), n_verb(fa$rows, "row", "rows"),
    if (without > 0) tagList(st_chip(sprintf("%s without f0", fmt_count(without)), "warn"),
                             tags$span(class = "st-see", "see below"))
    else st_chip("every row has f0", "ok", "circle-check"))))

  # How the f0 is sampled
  k <- fa$kind
  kind <- if (!is.null(fa$tokens) && !is.null(k) && !is.na(k$kind)) {
    lines <- list()
    if (k$kind == "frames") {
      step_txt <- sprintf("%sone every %s", if (isTRUE(k$about)) "about " else "",
                          start_step_text(k$step, k$unit))
      # Every token has the same number of rows: the Data screening card
      # counts them as points (equal_rows in screen_f0_rows()), so this
      # title does too.
      n_eq <- if (isTRUE(attr(scr$screen, "equal_rows"))) attr(scr$screen, "stretch")$n_rows
      if (length(n_eq) == 1 && !is.na(n_eq)) {
        title <- sprintf("%s points per token, %s", fmt_count(n_eq), step_txt)
        icon_name <- "ellipsis"
      } else {
        title <- sprintf("Pitch-tracker frames, %s", step_txt)
        icon_name <- "wave-square"
      }
      if (isTRUE(k$gaps > 0)) {
        lines <- list(if (k$gaps == 1) "1 token has a gap in the middle."
                      else sprintf("%s %s gaps in the middle (%s).", count_label(k$gap_tokens, "token"),
                                   n_verb(k$gap_tokens, "has", "have"), count_label(k$gaps, "gap")))
      } else if (!length(fa$problems)) {
        lines <- list("No token has a gap in the middle.")
      }
    } else if (k$kind == "points") {
      title <- sprintf("%s equidistant points per token", fmt_count(k$n_points))
      icon_name <- "ellipsis"
      grp_txt <- function(groups, more) {
        cnt <- as.integer(names(groups))
        shown <- utils::head(seq_along(groups), 3)
        parts <- lapply(shown, function(i) {
          hint <- NULL
          if (more && cnt[i] %% k$n_points == 0) {
            r <- cnt[i] %/% k$n_points
            hint <- sprintf("; %s recordings may share %s", if (r == 2) "two" else as.character(r),
                            if (length(groups[[i]]) == 1) "this token name" else "each of these token names")
          }
          st_inline(sprintf("%s points: ", fmt_count(cnt[i])), st_toks(groups[[i]]), hint)
        })
        rest <- sum(lengths(groups)[-shown])
        if (rest > 0) parts <- c(parts, list(sprintf("and %s with other counts", count_label(rest, "token"))))
        out <- list()
        for (i in seq_along(parts)) out <- c(out, if (i > 1) list("; "), parts[i])
        do.call(st_inline, out)
      }
      n_few  <- sum(lengths(k$fewer))
      n_more <- sum(lengths(k$more))
      if (n_few > 0) lines <- c(lines, list(st_inline(
        sprintf("%s %s fewer (", count_label(n_few, "token"), n_verb(n_few, "has", "have")),
        grp_txt(k$fewer, FALSE), ").")))
      if (n_more > 0) lines <- c(lines, list(st_inline(
        sprintf("%s %s more (", count_label(n_more, "token"), n_verb(n_more, "has", "have")),
        grp_txt(k$more, TRUE), ").")))
    } else {
      ms <- function(x) format(signif(if (identical(k$unit, "s")) x * 1000 else x, 2), trim = TRUE)
      title <- sprintf("Uneven points, mostly %s to %s ms apart", ms(k$range[1]), ms(k$range[2]))
      icon_name <- "chart-line"
    }
    tags$div(class = "st-fact st-kind",
      tags$span(class = "st-fact-icon", icon(icon_name)),
      tags$div(class = "st-fact-body",
        tags$div(class = "st-kind-label", "f0 data"),
        tags$div(class = "st-kind-title", title),
        lapply(lines, function(l) tags$div(class = "st-kind-line", l))))
  } else if (is.null(cols$token)) {
    tags$div(class = "st-fact st-kind",
      tags$span(class = "st-fact-icon", icon("wave-square")),
      tags$div(class = "st-fact-body",
        tags$div(class = "st-kind-label", "f0 data"),
        tags$div(class = "st-kind-line", "Sampling not checked: no token variable.")))
  }

  # The columns used
  col_item <- function(what, col, note = NULL) {
    tags$span(class = "st-col", st_inline(what, " ", if (is.null(col)) tags$em("none") else tags$code(col),
                                          if (!is.null(note)) paste0(" ", note)))
  }
  colline <- tags$div(class = "st-cols",
    tags$span(class = "st-cols-label", "Variables used"),
    col_item("f0", cols$f0, if (isTRUE(ex$is_hz)) "(Hz)" else "(not Hz)"),
    col_item("token", cols$token),
    col_item("time", cols$time),
    if (!is.null(fa$speakers)) col_item("speaker", fa$speakers$col),
    if (!is.null(fa$tones)) col_item("tone", fa$tones$col))

  probs <- lapply(names(fa$problems), function(key) {
    txt <- st_problem(key, fa$problems[[key]])
    if (!is.null(txt)) st_callout(txt)
  })
  file <- input$uploadfile$name
  tags$div(class = "st-card st-data",
    tags$div(class = "st-head",
      tags$span(class = "st-head-icon", icon("database")),
      tags$h5("Your data"),
      if (length(file) && nzchar(file)) tags$span(class = "st-head-aside", icon("file-csv"), file)),
    tags$div(class = "st-facts", facts),
    kind, colline, probs)
}

# The "Data screening" card: the rows without f0 and what the boxes do with
# them, one idea per sentence, the same for frame and equal-point files.
# Only the flagged tokens read differently: in an equal-point file every
# point without f0 flags its token, so they are listed by reason. Where
# to save the app's copy is folded away.
st_rows_card <- function(scr, plan, ex) {
  f0c <- scr$cols$f0
  # The f0 column as a code chip, so a column named f0 does not read as prose
  f0_in <- function(before, after = "") st_inline(before, " in column ", tags$code(f0c), after)
  pct <- function(n, of) {
    p <- if (of > 0) 100 * n / of else 0
    if (p > 0 && p < 0.1) "under 0.1%" else sprintf("%.1f%%", p)
  }
  without <- if (is.na(scr$without)) 0L else scr$without
  head <- function(chip) {
    tags$div(class = "st-head",
      tags$span(class = "st-head-icon", icon("filter")),
      tags$h5("Data screening"), chip)
  }
  if (!is.null(scr$error)) {
    return(tags$div(class = "st-card st-rows", head(NULL),
                    st_callout("Could not screen the rows: ", scr$error)))
  }
  chip <- if (without > 0) st_chip(sprintf("%s without f0", pct(without, scr$rows)), "warn")
          else st_chip("Every row has f0", "ok", "circle-check")

  # Where to save the app's copy, folded away: shown whenever that copy
  # differs from the file (see `differs` below). Excluding the discarded
  # tokens (prepared(), server.R) changes it too: their rows and the
  # token_dropped column go.
  excl <- is.function(raw_dataset) && "token_dropped" %in% names(raw_dataset()) &&
    isTRUE(screen_box("exclude"))
  save_more <- st_more("Your raw file is not changed",
    tags$p(paste("The boxes change only the app's copy of the data. To save that copy,",
                 "download it in Normalise or Curate.")))

  # No token column: a count over the whole file only. This card is the one
  # place that says how to choose a token column. The sidebar's token picker
  # shows when rows lack f0; otherwise it hides behind "Check the time steps
  # for gaps" (ui_f0_screen), which then has to be ticked first.
  none_txt <- "If the file has none, add a column that names each token."
  if (is.null(scr$screen)) {
    body <- if (without == 0) {
      tags$p(class = "st-lead",
        f0_in("Every row has f0", sprintf(" (%s). ", count_label(scr$rows, "row"))),
        "There is no token variable, so gaps inside tokens were not checked. Tick ",
        tags$em("Check the time steps for gaps"), " in the sidebar and choose the token variable ",
        "to check them. ", none_txt)
    } else {
      tagList(
        tags$p(class = "st-lead", f0_in(sprintf("%s of %s (%s) %s no f0", fmt_count(without),
                                                count_label(scr$rows, "row"), pct(without, scr$rows),
                                                n_verb(without, "has", "have")), ".")),
        tags$div(class = "st-decision st-decision-kept",
          tags$span(class = "st-decision-icon", icon("hand-point-left")),
          tags$div(tags$strong("Choose the token variable"),
                   sprintf(" in the sidebar to see where %s and to remove %s. %s",
                           n_verb(without, "it is", "they are"), n_verb(without, "it", "them"), none_txt))))
    }
    return(tags$div(class = "st-card st-rows", head(chip), body,
                    if (length(plan$unvoiced) || excl) save_more))
  }

  # `s`: what the upload has (the lead and where the rows are). `sp`: what
  # is left for the removal, after the drop of the flagged tokens (the
  # plan's post screen; the same as the upload's when nothing is dropped).
  sc  <- scr$screen
  s   <- attr(sc, "summary")
  ps  <- if (!is.null(plan$post)) plan$post else sc
  sp  <- attr(ps, "summary")
  left_without <- if (!is.null(plan$without)) plan$without else without
  # The rows dataset() keeps
  kept_rows <- if (is.function(screened)) screened() else NULL
  n_left <- if (!is.null(kept_rows)) nrow(kept_rows) else sp[["rows_after"]]
  # Every token has the same number of points (equal_rows): an equal-point
  # file, where every point without f0 flags its token and the short runs
  # are not offered
  eq    <- isTRUE(plan$points)
  notes <- list()
  note <- function(icon_name, ...) tags$li(tags$span(class = "st-note-icon", icon(icon_name)), tags$div(...))
  run_txt <- function(lead, after = NULL) {
    st_inline(tags$strong(class = "st-verdict", lead), after, " ", tags$code("voiced_run"),
              " numbers the stretches with f0 in each token, so plots break the line at gaps.")
  }
  decision <- function(kind, icon_name, ...) {
    tags$div(class = paste0("st-decision st-decision-", kind),
             tags$span(class = "st-decision-icon", icon(icon_name)), tags$div(...))
  }
  # The mark box by its label (screen_labels()): "Tick Mark the 1 gap to
  # mark it in a voiced_run column."
  n_mark <- sp[["gaps_time_only"]] + sp[["gaps_run"]]
  mark_tick <- st_inline(" Tick ", tags$em(sprintf("Mark the %s", count_label(n_mark, "gap"))),
                         " to mark ", n_verb(n_mark, "it", "them"), " in a voiced_run column.")
  # Every row (left) has f0: "All good:", then the gaps the time steps
  # show after the drop (it can take the tokens with these gaps, and the
  # mark box with them) and what voiced_run does with them: it marks them,
  # or the box would. `what` is " every row has f0." or, after a drop,
  # " every row left has f0."
  n_g <- sp[["gaps_time_only"]]
  all_good <- function(what) {
    gaps_txt <- if (n_g > 0) sprintf(" The time steps show %s in %s.", count_label(n_g, "gap"),
                                     count_label(sp[["gaps_time_only_tokens"]], "token"))
    if (n_g > 0 && isTRUE(plan$mark)) {
      decision("done", "circle-check", run_txt("All good:", paste0(what, gaps_txt)))
    } else {
      decision("done", "circle-check",
               st_inline(tags$strong(class = "st-verdict", "All good:"), what, gaps_txt,
                         if (n_g > 0) mark_tick))
    }
  }

  # The flagged tokens (screen_fix(), server.R) and their drop. A token
  # with no f0 is among them, whatever the removal box says.
  fl       <- plan$flagged
  n_flag   <- if (is.null(fl)) 0L else nrow(fl)
  one_flag <- n_flag == 1
  them     <- if (one_flag) "it" else "them"
  dropped  <- isTRUE(plan$drop_flagged)
  drop_lab <- drop_flagged_label(plan, all = FALSE)
  # The box as the sidebar labels it, for "Tick ..." and "Untick ..."
  drop_box <- drop_flagged_label(plan)
  # Dropping them leaves no rows: every token is flagged
  no_rows  <- n_flag > 0 && isTRUE(plan$all_flagged)
  # The drop took every row: nothing is left for the other tabs
  stop_dec <- if (dropped && n_left == 0) {
    decision("stop", "triangle-exclamation", tags$strong(class = "st-verdict", "No rows left:"),
             st_inline(if (s[["tokens"]] == 1) " the only token is flagged." else " every token is flagged.",
                       " Untick ", tags$em(drop_lab), if (one_flag) " to keep it." else " to keep them."))
  }
  # What ticking the drop does, by the box's label
  tick_drop <- if (no_rows) sprintf("Dropping %s would leave no rows.", them)
               else st_inline("Tick ", tags$em(drop_box), " to leave ", them, " out.")

  # One callout: what the flagged tokens have in common, the column and the
  # box. "Flagged" here, as Curate's Flagged button means the Inspect flags.
  # In an equal-point file every point without f0 flags its token, so they
  # have fewer than N points with f0 (fewer than 3 for N under 3), and the
  # tokens follow by reason. In a frame file the only reasons are no f0 and
  # 1 or 2 samples with f0, both fewer than 3, so the tokens follow inline.
  flag_box <- NULL
  if (n_flag > 0) {
    why <- if (eq) sprintf("fewer than %s points with f0", fmt_count(max(plan$n_points, 3L, na.rm = TRUE)))
           else "fewer than 3 samples with f0"
    toks <- if (!eq) st_inline(" (", st_flag_toks(fl), ")")
    # The file's own screen_flag column goes with the dropped tokens' column
    own_out <- if (dropped && isTRUE(plan$old_flag))
      st_inline("The file's own ", tags$code("screen_flag"), " column is left out.")
    # Every flagged token has no f0 and removal is ticked: removal takes
    # all their rows, so no row left is flagged and the drop would change
    # no row. The callout says so instead of naming the column and the box.
    gone <- !dropped && isTRUE(plan$remove) && all(fl$reason == "no f0")
    # What the flagged tokens' rows come to, after the reasons: the column
    # and the drop box, or that removal takes them
    after <- if (gone) {
      st_inline(if (one_flag) "It has no f0, so removal takes its rows."
                else "They have no f0, so removal takes their rows.",
                if (isTRUE(plan$old_flag))
                  st_inline(" The file's own ", tags$code("screen_flag"), " column is cleared."))
    } else {
      tick_drop
    }
    flag_box <- st_callout(kind = "info", icon_name = "flag",
      if (dropped) {
        tags$div(st_inline(tags$strong("Dropped:"),
                           sprintf(" %s (%s), %s", count_label(n_flag, "flagged token"),
                                   count_label(plan$n_flag_rows, "row"), why),
                           if (eq) ":" else st_inline(toks, ".", if (!is.null(own_out)) " ", own_out)))
      } else {
        tags$div(st_inline(tags$strong(sprintf("%s flagged:", count_label(n_flag, "token"))),
                           " ", why, toks, ".",
                           if (!gone) st_inline(
                             if (one_flag) " The reason is" else " Reasons are",
                             " in a new ", tags$code("screen_flag"), " column.",
                             if (isTRUE(plan$old_flag)) " The new column replaces the file's own."),
                           if (!eq) " ", if (!eq) after))
      },
      if (eq) st_flag_list(fl),
      if (eq && !dropped) tags$div(after),
      if (eq && !is.null(own_out)) tags$div(own_out))
  }

  if (without > 0) {
    where <- list(
      if (s[["start"]] > 0) st_fact("backward-step", fmt_count(s[["start"]]), "at the start",
                                    sprintf("of %s", count_label(s[["start_tokens"]], "token"))),
      if (s[["end"]] > 0) st_fact("forward-step", fmt_count(s[["end"]]), "at the end",
                                  sprintf("of %s", count_label(s[["end_tokens"]], "token"))),
      if (s[["inside"]] > 0) st_fact("pause", fmt_count(s[["inside"]]), "inside",
                                     sprintf("%s, in %s", count_label(s[["gap_tokens"]], "token"),
                                             count_label(s[["gaps"]], "gap"))),
      # (The rows of the tokens with no f0 have no tile: those tokens are
      # flagged, so the tiles need not add up to the lead's count)
      if (s[["no_time"]] > 0) st_fact("clock", fmt_count(s[["no_time"]]), "without a place",
                                      "they or their token's samples have no time"))
    where <- Filter(Negate(is.null), where)
    # The lead introduces the tiles, so it ends with a full stop when there
    # are none: every row without f0 is in a token with no f0
    lead <- tags$p(class = "st-lead",
      f0_in(sprintf("%s of %s (%s) %s no f0", fmt_count(without),
                    count_label(s[["rows"]], "row"), pct(without, s[["rows"]]),
                    n_verb(without, "has", "have")), if (length(where)) ":" else "."))
    where <- if (length(where)) tags$div(class = "st-facts st-where", where)
    dec <- if (!is.null(stop_dec)) {
      stop_dec
    } else if (left_without == 0) {
      # The drop left no row without f0
      all_good(" every row left has f0.")
    } else if (plan$remove) {
      if (n_left == 0) {
        # The lead has just said that no row has f0
        decision("stop", "triangle-exclamation", tags$strong(class = "st-verdict", "Removed:"),
                 " no rows left. Choose another f0 variable in the sidebar.")
      } else {
        # The rows the removal takes: those left after the drop, so fewer
        # than the lead's when the drop took some
        decision("done", "circle-check",
                 run_txt("Removed:", sprintf(" %s without f0.", count_label(left_without, "row"))))
      }
    } else {
      # Removal unticked (its box shows while rows without f0 are left)
      stays <- if (left_without == 1) "the row without f0 stays" else "rows without f0 stay"
      decision("kept", "circle-pause", tags$strong(class = "st-verdict", "Kept:"), sprintf(" %s.", stays),
               st_inline(" Tick ", tags$em("Remove rows without f0"),
                         if (left_without == 1) " to remove it." else " to remove them."))
    }
    # Gaps that only the time steps show, while rows without f0 are left
    # (else the All good line has them): no rows without f0 lie in them.
    # "More" and "too" follow the inside tile's gaps.
    if (n_g > 0 && left_without > 0) {
      more <- s[["inside"]] > 0
      notes <- c(notes, list(note("wave-square", st_inline(
        sprintf("The time steps show %s%s %s in %s, where the file has no rows.",
                fmt_count(n_g), if (more) " more" else "", n_verb(n_g, "gap", "gaps"),
                count_label(sp[["gaps_time_only_tokens"]], "token")),
        if (plan$remove) sprintf(" voiced_run marks %s%s.", n_verb(n_g, "it", "them"),
                                 if (more) " too" else "")))))
    }
    body <- tagList(lead, where, dec, flag_box)
  } else {
    # Nothing to remove
    dec <- if (!is.null(stop_dec)) stop_dec else all_good(" every row has f0.")
    body <- tagList(dec, flag_box)
  }
  # An upload's own screen_flag column, from an earlier screening (fixed(),
  # server.R): left out when no token is flagged now
  if (isTRUE(plan$old_flag) && !isTRUE(plan$flag)) {
    notes <- c(notes, list(note("flag", st_inline(
      "No token is flagged now, so the file's own ", tags$code("screen_flag"), " column is left out."))))
  }
  if (sp[["gaps_run"]] > 0) {
    notes <- c(notes, list(note("wave-square",
      sprintf("The file's voiced_run column marks %s more in %s.",
              count_label(sp[["gaps_run"]], "gap"), count_label(sp[["gaps_run_tokens"]], "token")))))
  }
  # The short runs, only while their box shows (screen_flags()): with
  # removal or marking ticked, in a frame file
  if (sp[["short_tokens"]] > 0 && plan$drop && !eq) {
    # short_tokens_start and _end each count a token with runs at both ends,
    # so the parts are "13 at the start and 38 at the end; 1 token has both"
    # for 50 tokens, never a sum.
    where_txt <- if (sp[["short_tokens"]] > 1) {
      n_st <- sp[["short_tokens_start"]]; n_en <- sp[["short_tokens_end"]]; n_bo <- sp[["short_tokens_both"]]
      parts <- if (n_st > 0 && n_en > 0) sprintf("%s at the start and %s at the end", fmt_count(n_st), fmt_count(n_en))
               else if (n_st > 0) "all at the start" else "all at the end"
      if (n_bo > 0) parts <- sprintf("%s; %s %s both", parts, count_label(n_bo, "token"), n_verb(n_bo, "has", "have"))
      paste0(" (", parts, ")")
    } else ""
    n_sh <- sp[["short_tokens"]]
    notes <- c(notes, list(note("scissors", st_inline(
      sprintf("In %s%s, a gap cuts off a run of 1\u20132 samples%s, %s in all. ",
              count_label(n_sh, "token"), where_txt,
              if (nzchar(where_txt)) "" else " at the start or end",
              count_label(sp[["short_rows"]], "sample")),
      if (plan$short) {
        st_inline("These samples hold the first or last f0. ", tags$strong("They are removed too,"),
                  " so ", n_verb(n_sh, "this token's ", "these tokens' "), st_01(), " time axes change.")
      } else {
        st_inline("These samples hold the first or last f0, so the ", st_01(),
                  " time axes start or end there. Tick ", tags$em("Also remove runs of 1\u20132 samples"),
                  " to remove them.")
      }))))
  }
  if (length(ex$other_hz)) {
    oh <- vapply(names(ex$other_hz), function(cn) {
      v <- ex$other_hz[[cn]]
      sprintf("%s has %s without f0 and differs from %s on %s", cn,
              count_label(v[["without"]], "row"), f0c, count_label(v[["disagree"]], "row"))
    }, character(1))
    notes <- c(notes, list(note("wave-square", paste0("Other Hz columns: ", paste(oh, collapse = "; "), "."))))
  }
  if (!is.null(ex$deleted) && without > 0) {
    notes <- c(notes, list(note("eraser",
      sprintf("Of the rows without f0, %s %s deleted in F0 Correction (edited = TRUE); the rest are tracker gaps.",
              fmt_count(ex$deleted), if (ex$deleted == 1) "was" else "were"))))
  }
  # Removing rows can change what a token-level column says, and which 0-1
  # axis the fits take
  if (length(ex$varying)) {
    notes <- c(notes, list(note("tags",
      sprintf("Not the same on every row of a token: %s. Removing rows can change the value such a token gets.",
              list_some(sprintf("%s (%s)", names(ex$varying), count_label(ex$varying, "token")))))))
  }
  if (!is.null(ex$time_flip)) {
    tcol <- scr$cols$time
    notes <- c(notes, list(note("clock",
      if (ex$time_flip[["before"]])
        st_inline("The models take ", tags$code(tcol), " as already on 0\u20131 with these rows, but ",
                  "rescale each token from its first f0 to its last without them.")
      else st_inline("The models rescale ", tags$code(tcol), " from each token's first f0 to its last ",
                     "with these rows, but take it as already on 0\u20131 without them."))))
  }
  # Folded away: the save note, whenever the
  # app's copy differs from the file: rows removed, tokens dropped or
  # discarded tokens excluded, values read as no f0, or a screen_flag or
  # voiced_run column added (or the file's own screen_flag left out).
  # Curate's Curated CSV and Normalise's download both save every column.
  differs <- isTRUE(plan$drop) || dropped || isTRUE(plan$flag) || isTRUE(plan$old_flag) ||
    length(plan$unvoiced) > 0 || excl
  tags$div(class = "st-card st-rows", head(chip), body,
           if (length(notes)) tags$ul(class = "st-notes", notes),
           if (differs) save_more)
}

output$f0_screen_report <- renderUI({
  if (!has_screen) return(NULL)
  scr  <- f0_screen()
  plan <- screen_plan()
  if (is.null(scr) || is.null(plan)) return(NULL)
  ex <- screen_extra()
  fa <- screen_facts()
  tags$div(class = "st-report", st_css,
    if (!is.null(fa)) st_your_data(fa, scr, ex),
    st_rows_card(scr, plan, ex))
})
outputOptions(output, "f0_screen_report", suspendWhenHidden = FALSE)

# One toast per upload: what the screen found, in at most three short
# sentences and "See the Start page." The 0 Hz rows (which raw_dataset()
# reads as NA, zero_hz_to_na()) and the values read as no f0 go in
# brackets after the count of rows without f0, which holds them. A 0 Hz
# count of another column gets a sentence of its own. The report on the
# Start page has the detail.
observeEvent(raw_dataset(), {
  raw <- raw_dataset()
  z   <- attr(raw, "zero_hz")
  scr <- if (has_screen) f0_screen()
  plan <- if (has_screen) screen_plan()
  msg <- character(0)
  ok  <- !is.null(scr) && is.null(scr$error)
  f0c <- if (ok) scr$cols$f0
  z_f0 <- if (ok && length(z) && f0c %in% names(z)) z[[f0c]] else 0
  z_other <- if (length(z)) z[!(names(z) %in% f0c) & z > 0] else z
  if (length(z_other)) {
    msg <- c(msg, sprintf("Read 0 Hz in %s as missing (NA).",
                          list_some(sprintf("%s (%s)", names(z_other), count_label(z_other, "row")))))
  }
  if (ok && isTRUE(scr$without > 0) && isTRUE(scr$without == scr$rows)) {
    # No row has f0: nothing else matters until another f0 column is chosen
    msg <- c(msg, sprintf("No row has f0 in column %s. Choose another f0 variable on the Start page.", f0c))
  } else if (ok) {
    s <- if (!is.null(scr$screen)) attr(scr$screen, "summary")
    n_w <- scr$without
    one <- isTRUE(n_w == 1)
    # "16 of 132 rows have no f0", "1 of 6 rows has no f0", with "(2 of
    # them 0 Hz)", "(all 0 or -1)" or, for the one row, "(0 Hz)"
    held_one <- function(n, what) {
      if (isTRUE(n == n_w)) (if (one) what else paste("all", what))
      else sprintf("%s of them %s", fmt_count(n), what)
    }
    held <- c(if (z_f0 > 0) held_one(z_f0, "0 Hz"),
              if (length(scr$unvoiced) && isTRUE(scr$unvoiced_rows > 0))
                held_one(scr$unvoiced_rows, unv_text(scr$unvoiced, " or ")))
    held_txt <- if (length(held)) sprintf(" (%s)", paste(held, collapse = ", ")) else ""
    rows_txt <- if (isTRUE(n_w > 0)) sprintf("%s of %s %s no f0%s", fmt_count(n_w), count_label(scr$rows, "row"),
                                             n_verb(n_w, "has", "have"), held_txt)
    see <- FALSE
    if (is.null(s)) {
      if (isTRUE(n_w > 0)) {
        msg <- c(msg, sprintf("%s. Choose the token variable on the Start page to remove %s.",
                              rows_txt, if (one) "it" else "them"))
      }
    } else if (n_w > 0) {
      see <- TRUE
      msg <- c(msg, paste0(rows_txt, if (isTRUE(plan$remove)) (if (one) " and is removed." else " and are removed.")
                                     else if (one) " and is kept." else " and are kept."))
    } else if (isTRUE(plan$mark)) {
      see <- TRUE
      msg <- c(msg, sprintf("Every row has f0. The time steps show %s, marked in voiced_run.",
                            count_label(s[["gaps_time_only"]], "gap")))
    }
    # The flagged tokens (screen_fix(), server.R), with the boxes as drawn
    n_flag <- if (is.null(plan$flagged)) 0L else nrow(plan$flagged)
    if (n_flag > 0) {
      see <- TRUE
      msg <- c(msg, sprintf("%s %s flagged.", count_label(n_flag, "token"), n_verb(n_flag, "is", "are")))
    }
    if (see) msg <- c(msg, "See the Start page.")
  }
  if (!length(msg)) {
    removeNotification("f0a_f0_screen")
    return()
  }
  showNotification(paste(msg, collapse = " "), type = "message", duration = 10,
                   id = "f0a_f0_screen")
})

# Observe when the file is uploaded and set the dataset name
observe({
  req(input$uploadfile)
  # Extract the name of the uploaded file
  dataset_name <- tools::file_path_sans_ext(basename(input$uploadfile$name))

  # Update the dataset name for consistent use in the app
  updateTextInput(session, "dataset_name", value = dataset_name)
})

# Create a hidden input for dataset_name to be used in other tabs
output$ui_dataset_name <- renderUI({
  div(textInput("dataset_name", NULL, value = ""), style = "display:none;")
})

# Render the dataset preview/structure/summary options
output$ui_datasets <- renderUI({
  tagList(
    radioButtons("dman_preview", "Display:",
                 c("Preview" = "preview",
                   "Structure" = "str",
                   "Summary" = "summary"),
                 selected = "preview",
                 inline = TRUE)
  )
})

# Update title dynamically based on the selected preview option
output$preview_title <- renderText({
  req(input$dman_preview) # Ensure input$dman_preview is available
  switch(input$dman_preview,
         "preview" = "Data Preview",
         "str" = "Data Structure",
         "summary" = "Data Summary")
})

# Display preview (first 10 rows), structure, or summary based on user selection
output$man_example <- renderUI({
  ds <- dataset()
  if (is.null(ds)) {
    # A refused upload leaves raw_dataset() NULL; its error toast says why.
    return(tags$div(style = "color: #888; font-style: italic; margin: 8px 0;",
                    if (is.null(input$uploadfile)) "No file uploaded yet."
                    else "This file could not be read."))
  }

  if (input$dman_preview == "preview") {
    tagList(
      DT::dataTableOutput("data_preview"),
      textOutput("row_count_info")  # Text for row count info
    )
  } else if (input$dman_preview == "str") {
    verbatimTextOutput("data_structure")
  } else if (input$dman_preview == "summary") {
    verbatimTextOutput("data_summary")
  }
})


output$data_preview <- DT::renderDataTable({
  req(dataset())
  DT::datatable(
    head(dataset(), 10),
    rownames = FALSE,
    options  = list(
      dom         = "t",            # only the table (no search / pagination)
      pageLength  = 10,
      scrollX     = TRUE,           # horizontal scroll if many wide columns
      ordering    = FALSE,
      columnDefs  = list(list(className = "dt-center", targets = "_all"))
    )
  )
})

output$row_count_info <- renderText({
  req(dataset())
  total_rows <- nrow(dataset())
  paste(fmt_count(min(10L, total_rows)), "of", fmt_count(total_rows), "rows shown. See View-tab for details.")
})

output$data_structure <- renderPrint({
  req(dataset())
  str(dataset())
})

output$data_summary <- renderPrint({
  req(dataset())
  summary(dataset())
})

# ---- Optional metadata attachment ------------------------------------------
# Lets users who already have an f0 dataframe add speaker/tone/item columns at
# upload time, without the audio-based F0 Extraction path. The result is stored
# in attached_metadata() and joined into dataset() (see server.R).

# Derive-from-column: pick which column to split.
output$meta_key_ui <- renderUI({
  raw <- raw_dataset(); req(raw)
  vars <- names(raw)
  lab <- if (identical(input$meta_source, "csv")) "Filename variable in your data:"
         else "Split this variable:"
  cur <- isolate(input$meta_key)
  sel <- if (!is.null(cur) && cur %in% vars) cur else guess_var(vars, var_patterns$token)
  selectInput("meta_key", lab, choices = vars, selected = sel)
})

# Split the chosen column's unique values by the separator.
meta_splits <- reactive({
  req(input$meta_source == "derive")
  raw <- raw_dataset(); req(raw)
  key <- input$meta_key; req(key, key %in% names(raw))
  sep <- input$meta_sep; req(!is.null(sep), nzchar(sep))
  keys <- unique(as.character(raw[[key]]))
  list(keys = keys, parts = strsplit(keys, sep, fixed = TRUE))
})

output$meta_status <- renderUI({
  sp <- tryCatch(meta_splits(), error = function(e) NULL)
  if (is.null(sp)) return(NULL)
  n_each <- vapply(sp$parts, length, integer(1))
  n_max <- max(n_each); n_min <- min(n_each)
  if (n_max == n_min) {
    tags$div(style = "color: #2a7a5a; font-size: 0.8rem; margin-bottom: 8px;",
      sprintf("%d segment(s) in all %s unique value(s).", n_max, fmt_count(length(sp$keys))))
  } else {
    tags$div(style = "color: #8a6d00; font-size: 0.8rem; margin-bottom: 8px;",
      sprintf("%d-%d segments across %s unique value(s); shorter ones pad with NA.",
              n_min, n_max, fmt_count(length(sp$keys))))
  }
})

meta_colnames <- reactive({
  sp <- meta_splits()
  n_max <- max(vapply(sp$parts, length, integer(1)))
  raw <- input$meta_colnames
  typed <- if (is.null(raw) || !nzchar(trimws(raw))) character(0)
           else { p <- unlist(strsplit(trimws(raw), "[,[:space:]]+", perl = TRUE)); p[nzchar(p)] }
  if (length(typed) >= n_max) typed[seq_len(n_max)]
  else if (length(typed) == 0) sprintf("column_%d", seq_len(n_max))
  else c(typed, sprintf("column_%d", seq.int(length(typed) + 1, n_max)))
})

# Per-key derived metadata: key column + named segment columns.
meta_derived <- reactive({
  sp <- meta_splits()
  n_max <- max(vapply(sp$parts, length, integer(1)))
  cols <- meta_colnames()
  padded <- lapply(sp$parts, function(x) c(x, rep(NA_character_, n_max - length(x))))
  mat <- do.call(rbind, padded)
  colnames(mat) <- cols
  out <- data.frame(.k = sp$keys, mat, stringsAsFactors = FALSE, check.names = FALSE)
  names(out)[1] <- input$meta_key
  out
})

output$meta_preview <- renderUI({
  df <- tryCatch(meta_derived(), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  sub <- utils::head(df, 3)
  hdr <- tags$tr(lapply(names(sub), function(n)
    tags$th(style = "padding: 2px 6px; border-bottom: 1px solid #ccc; font-size: 0.76rem;", n)))
  rows <- lapply(seq_len(nrow(sub)), function(i)
    tags$tr(lapply(names(sub), function(n)
      tags$td(style = "padding: 2px 6px; font-size: 0.76rem; color: #555;", as.character(sub[i, n])))))
  tags$div(style = "margin-top: 6px; overflow-x: auto;",
    tags$div(style = "font-size: 0.76rem; color: #777; margin-bottom: 3px;",
             sprintf("Preview (first 3 of %s rows):", fmt_count(nrow(df)))),
    tags$table(style = "border-collapse: collapse;", hdr, rows))
})

# Upload-CSV path.
meta_csv <- reactive({
  f <- input$meta_file
  if (is.null(f)) return(NULL)
  # One toast id, so a new file replaces the last file's error or note.
  df <- tryCatch(read_csv_utf8(f$datapath, stringsAsFactors = FALSE, name = f$name),
                 error = function(e) {
                   showNotification(conditionMessage(e), type = "error", duration = 10,
                                    id = "meta_csv_read"); NULL })
  note <- attr(df, "encoding_note")
  if (!is.null(note)) {
    showNotification(note, type = "warning", duration = 10, id = "meta_csv_read")
  } else if (!is.null(df)) {
    removeNotification("meta_csv_read")
  }
  df
})

# Always render the metadata-CSV column picker (with a placeholder before a
# file is uploaded), so users see both selectors up front.
output$meta_join_ui <- renderUI({
  csv <- meta_csv()
  if (is.null(csv) || ncol(csv) == 0) {
    return(selectInput("meta_join_meta", "Filename column in the metadata CSV:",
                       choices = c("Upload a CSV first" = ""), selected = ""))
  }
  mvars <- names(csv)
  mguess <- mvars[grepl("^(token|filename|file|wav|basename|audio|id)$", mvars, ignore.case = TRUE)]
  msel <- if (length(mguess)) mguess[1] else guess_var(mvars, var_patterns$token)
  selectInput("meta_join_meta", "Filename column in the metadata CSV:",
    choices = mvars, selected = msel)
})

output$meta_csv_status <- renderUI({
  csv <- tryCatch(meta_csv(), error = function(e) NULL)
  if (is.null(csv)) return(NULL)
  tags$div(style = "color: #2a7a5a; font-size: 0.8rem; margin-top: 4px;",
    sprintf("Metadata CSV: %s rows, %s columns.", fmt_count(nrow(csv)), fmt_count(ncol(csv))))
})

# Apply / detach.
observeEvent(input$meta_apply, {
  src <- input$meta_source
  if (identical(src, "derive")) {
    df <- tryCatch(meta_derived(), error = function(e) NULL)
    if (is.null(df)) { showNotification("Nothing to derive yet.", type = "warning"); return() }
    attached_metadata(list(key = input$meta_key, data = df,
                           source = list(type = "derive", sep = input$meta_sep,
                                         names = setdiff(names(df), input$meta_key))))
    showNotification(sprintf("Attached %d metadata column(s) by splitting '%s'.",
                             ncol(df) - 1, input$meta_key), type = "message")
  } else if (identical(src, "csv")) {
    csv <- meta_csv(); raw <- raw_dataset()
    dkey <- input$meta_key; mkey <- input$meta_join_meta
    strip <- isTRUE(input$meta_strip_ext)
    if (is.null(csv) || is.null(raw) || is.null(dkey) || is.null(mkey) ||
        !mkey %in% names(csv) || !dkey %in% names(raw)) {
      showNotification("Pick a metadata CSV and the key columns first.", type = "warning"); return()
    }
    # Normalised-key match (mirrors the F0 Extraction tab): tolerant of case
    # and of .wav-style file extensions on either side. Build a per-data-key
    # metadata table so the downstream join (server.R) is a plain left_join.
    mk_norm <- make_token_key(csv[[mkey]], strip)
    keep    <- !duplicated(mk_norm)
    csv_u   <- csv[keep, , drop = FALSE]; mk_u <- mk_norm[keep]
    dk      <- unique(as.character(raw[[dkey]]))
    idx     <- match(make_token_key(dk, strip), mk_u)
    meta_cols <- setdiff(names(csv_u), mkey)
    res <- data.frame(setNames(list(dk), dkey), stringsAsFactors = FALSE, check.names = FALSE)
    for (cn in meta_cols) res[[cn]] <- csv_u[[cn]][idx]
    attached_metadata(list(key = dkey, data = res,
                           source = list(type = "csv", file = input$meta_file$name,
                                         meta_key = mkey, strip = strip)))
    n_match <- sum(!is.na(idx))
    showNotification(sprintf("Metadata joined on '%s': %s of %s unique values matched%s.",
                             dkey, fmt_count(n_match), fmt_count(length(dk)),
                             if (n_match < length(dk)) " (the rest get NA)" else ""),
                     type = if (n_match == 0) "warning" else "message", duration = 5)
  }
})

observeEvent(input$meta_clear, {
  attached_metadata(NULL); showNotification("Metadata detached.", type = "message")
})

output$meta_applied <- renderUI({
  m <- attached_metadata()
  if (is.null(m) || is.null(m$data)) return(NULL)
  raw <- raw_dataset()
  new_cols <- if (!is.null(raw)) setdiff(names(m$data), names(raw)) else setdiff(names(m$data), m$key)
  tags$div(style = "margin-top: 8px; padding: 6px 8px; background: #eef8f3; border: 1px solid #bfe3d4; border-radius: 4px; font-size: 0.8rem; color: #1f6f4d;",
    tags$strong("Attached: "),
    if (length(new_cols)) paste(new_cols, collapse = ", ") else "(no new columns)",
    tags$div(style = "margin-top: 4px;",
      actionButton("meta_clear", "Detach metadata", class = "btn btn-sm")))
})

# ---- Optional contour duration ---------------------------------------------
# Adds a <unit>_f0_dur column with contour_duration() (R/duration.R). The specs
# live in duration_specs() and server.R applies them to dataset() after the
# metadata join, so the column reaches every F0 Analysis tab. These outputs
# sit in a <details> that starts closed, hence suspendWhenHidden = FALSE.

# The token and f0 defaults: the Start page's columns when the upload has
# them, else the usual guesses.
dur_defaults <- function(vars) {
  tok <- start_col(start_cols, "token")
  scr <- start_col(start_cols, "f0")
  vt  <- vis_token_col(vars)
  list(token = if (!is.null(tok) && tok %in% vars) tok else if (!is.null(vt)) vt else vars[1],
       f0    = if (!is.null(scr) && scr %in% vars) scr
               else guess_var(vars, c("^f0_hz$", "^f0$", var_patterns$f0), 1))
}
dur_shown <- new.env()   # the default each picker was last drawn or set with
# A Start-page column change reaches a picker still on its default.
if (is.function(start_cols)) {
  observeEvent(start_cols(), {
    raw <- raw_dataset()
    if (is.null(raw)) return()
    def <- dur_defaults(names(raw))
    for (k in names(def)) {
      id <- paste0("dur_", k)
      if (identical(input[[id]], dur_shown[[k]]) && !identical(def[[k]], dur_shown[[k]])) {
        updateSelectInput(session, id, selected = def[[k]])
        dur_shown[[k]] <- def[[k]]
      }
    }
  }, ignoreInit = TRUE)
}

output$dur_ui <- renderUI({
  raw <- raw_dataset()
  if (is.null(raw)) {
    return(tags$div(style = "font-size: 0.78rem; color: #999; font-style: italic;",
                    "Upload a CSV first."))
  }
  vars  <- names(raw)
  sets  <- landmark_sets(vars)
  units <- c("Whole token" = "__token__")
  if (length(sets)) {
    units <- c(units, stats::setNames(names(sets), paste(names(sets), "segments")))
  }
  # Drawn once per upload (it reads only raw_dataset()), with this file's
  # defaults: the pickers' input values still hold the last file's columns.
  # The token and f0 columns default to the Start page's (D8), e.g.
  # f0_corrected rather than f0, and follow them (observer below) while the
  # user has not picked another one.
  def <- isolate(dur_defaults(vars))
  for (k in names(def)) dur_shown[[k]] <- def[[k]]
  tagList(
    selectInput("dur_unit", "Measure within:", choices = units, selected = "__token__"),
    selectInput("dur_token", "Token ID variable:", choices = vars, selected = def$token),
    selectInput("dur_time", "Time variable:", choices = vars,
                selected = guess_var(vars, var_patterns$time, 1)),
    selectInput("dur_f0", "f0 variable:", choices = vars, selected = def$f0),
    tags$div(style = "font-size: 0.78rem; color: #888; margin: -6px 0 0 0;",
      HTML(paste(
        "Adds <code>token_f0_dur</code>, or <code>&lt;tier&gt;_f0_dur</code> per landmark",
        "segment, in the units of the time variable. Segment units are listed when the data",
        "has <code>&lt;tier&gt;_start</code> / <code>&lt;tier&gt;_end</code> columns.")))
  )
})
outputOptions(output, "dur_ui", suspendWhenHidden = FALSE)

observeEvent(input$dur_apply, {
  # The data the column is computed on in server.R (prepared(): discarded
  # tokens excluded, metadata joined), so the toast's counts match the column.
  raw <- if (is.function(prepared)) prepared() else raw_dataset()
  if (is.null(raw) || is.null(duration_specs)) {
    showNotification("Upload a CSV first.", type = "warning"); return()
  }
  set  <- if (is.null(input$dur_unit) || identical(input$dur_unit, "__token__")) NULL
          else input$dur_unit
  spec <- list(token = input$dur_token, time = input$dur_time, f0 = input$dur_f0,
               set = set, name = if (is.null(set)) "token_f0_dur" else paste0(set, "_f0_dur"))
  # As server.R adds the column: on the screened column, the values the
  # screen reads as no f0 (0 / -1, when ticked) count as no f0 here too.
  res <- tryCatch(add_duration(raw, spec, duration_unvoiced(raw, spec$f0, start_cols, screen_box)),
                  error = function(e) e)
  if (inherits(res, "error")) {
    showNotification(paste("Could not add the duration:", conditionMessage(res)),
                     type = "error", duration = 6)
    return()
  }
  specs <- duration_specs(); specs[[spec$name]] <- spec; duration_specs(specs)

  # One value per unit (constant within a unit) for the confirmation.
  key <- as.character(res[[spec$token]])
  if (!is.null(set)) {
    key <- paste(key, res[[paste0(set, "_start")]], res[[paste0(set, "_end")]])
  }
  per  <- res[[spec$name]][!duplicated(key)]
  got  <- per[!is.na(per)]
  what <- if (is.null(set)) "token" else paste(set, "segment")
  if (length(got) == 0) {
    showNotification(sprintf("Added %s, but no %s has an f0 value in '%s', so it is all NA. Check the f0 variable.",
                             spec$name, what, spec$f0), type = "warning", duration = 8)
    return()
  }
  fmt <- function(x) format(signif(x, 3))
  n_na <- if (is.null(set)) sum(is.na(per)) else 0L
  # Tokens with no f0 go with the rows without f0 when removal is ticked.
  gone <- n_na > 0 && is.function(screen_plan) && isTRUE(isolate(screen_plan())$remove)
  showNotification(
    sprintf("Added %s for %s %s(s): median %s, range %s to %s (units of '%s').%s",
            spec$name, fmt_count(length(got)), what, fmt(stats::median(got)), fmt(min(got)),
            fmt(max(got)), spec$time,
            if (n_na > 0) sprintf(" %s token(s) have no f0 and get NA%s.", fmt_count(n_na),
                                  if (gone) "; the Start page removes them with the rows without f0" else "")
            else ""),
    type = "message", duration = 8)
})

output$dur_applied <- renderUI({
  specs <- if (is.null(duration_specs)) list() else duration_specs()
  if (length(specs) == 0) return(NULL)
  tags$div(style = "margin-top: 8px; padding: 6px 8px; background: #eef8f3; border: 1px solid #bfe3d4; border-radius: 4px; font-size: 0.8rem; color: #1f6f4d;",
    tags$strong("Added: "), paste(names(specs), collapse = ", "),
    tags$div(style = "margin-top: 4px;",
      actionButton("dur_clear", "Remove duration columns", class = "btn btn-sm")))
})
outputOptions(output, "dur_applied", suspendWhenHidden = FALSE)

observeEvent(input$dur_clear, {
  duration_specs(list())
  showNotification("Duration columns removed.", type = "message")
})

}
