###############################################
# F0 Processing → F0 Correction subtab (MVP)
#
# Reads:  fp_audio_data (.wav paths) and fp_f0_data (extracted contours).
# Lets the user step through tokens, listen, inspect waveform + f0,
# select frames (single click or box-select), and apply fixes:
#   * Halve f0 (octave-doubling fix)
#   * Double f0 (octave-halving fix)
#   * Delete frame(s)
#   * Undo
# Output: corrected f0 CSV with both original and corrected columns.
###############################################

fp_correction_ui <- function(input, output, session, fp_audio_data, fp_f0_data,
                             fp_pitch_candidates = NULL) {

  # ---- Reactive state ----
  # Named list: token -> data.frame(time, f0)  (corrected contour)
  fp_corrections <- reactiveVal(list())
  # Named list: token -> list of past states (for undo)
  fp_history <- reactiveVal(list())
  # Chronological log of finalised edits. One row per apply_edit() call.
  # When the user undoes an edit, the matching row is removed from the
  # log (rather than appending an "Undo" row), so the log captures only
  # the edits the user kept.
  # Surfaced in the "Edit log" table and downloadable as CSV.
  #   date           : YYYY-MM-DD when the edit was applied
  #   token          : which token
  #   action         : edit type ("Halve", "Smooth", ...)
  #   n_frames       : how many frames were touched
  #   frame_indices  : 1-based positions in the contour (comma-separated)
  #   frame_times_s  : the corresponding within-token times in seconds
  #   frame_pct      : positions normalised to [0, 100]% within the token
  #                    (useful for comparing where in the tonal contour the
  #                    edit happened across tokens of different durations)
  #   details        : free-text method / parameters (e.g., "median, window=3")
  fp_edit_log <- reactiveVal(data.frame(
    date          = character(0),
    token         = character(0),
    action        = character(0),
    n_frames      = integer(0),
    frame_indices = character(0),
    frame_times_s = character(0),
    frame_pct     = character(0),
    details       = character(0),
    stringsAsFactors = FALSE
  ))
  log_edit <- function(token, action, n_frames, details = "",
                       indices = integer(0), times = numeric(0), n_total = NA_integer_) {
    # Format multi-frame fields as compact comma-separated strings.
    fmt_int <- function(x) if (length(x) == 0) NA_character_
                           else paste(x, collapse = ",")
    fmt_num <- function(x, digits = 3)
      if (length(x) == 0) NA_character_
      else paste(formatC(x, format = "f", digits = digits), collapse = ",")
    pct <- if (length(indices) == 0 || is.na(n_total) || n_total <= 1) numeric(0)
           else (indices - 1) / (n_total - 1) * 100
    cur <- fp_edit_log()
    new_row <- data.frame(
      date          = format(Sys.Date(), "%Y-%m-%d"),
      token         = as.character(token),
      action        = as.character(action),
      n_frames      = as.integer(n_frames),
      frame_indices = fmt_int(indices),
      frame_times_s = fmt_num(times, 3),
      frame_pct     = fmt_num(pct, 1),
      details       = as.character(details),
      stringsAsFactors = FALSE
    )
    fp_edit_log(rbind(cur, new_row))
  }
  # Optional flagged-token filter: raw uploaded CSV + extracted token IDs.
  # fp_flagged_frames (if the CSV came from the Inspect tab) holds per-frame
  # flag info keyed by token: a named list, token -> data.frame(time, note).
  fp_flagged_raw    <- reactiveVal(NULL)
  fp_flagged_tokens <- reactiveVal(NULL)
  fp_flagged_frames <- reactiveVal(NULL)

  # Current token's TextGrid (if available), parsed via rPraat::tg.read.
  # Returns a list of tiers (each with $name, $type, intervals/points), or NULL.
  current_textgrid <- reactive({
    tok <- input$fp_corr_token
    audio <- fp_audio_data()
    if (is.null(tok) || is.null(audio)) return(NULL)
    row <- audio[audio$basename == tok, , drop = FALSE]
    if (nrow(row) == 0 || is.na(row$tg_path[1])) return(NULL)
    tryCatch(rPraat::tg.read(row$tg_path[1]), error = function(e) NULL)
  })

  # Current token's contour (corrected if edited, else original)
  current_f0 <- reactive({
    req(input$fp_corr_token)
    tok <- input$fp_corr_token
    corr <- fp_corrections()
    if (tok %in% names(corr)) return(corr[[tok]])
    df <- fp_f0_data()
    if (is.null(df)) return(NULL)
    sub <- df[df$token == tok, c("time", "f0")]
    sub[order(sub$time), , drop = FALSE]
  })

  # Helper: push a snapshot of the current corrected contour onto the undo stack
  push_history <- function(tok, snapshot) {
    h <- fp_history()
    stack <- if (tok %in% names(h)) h[[tok]] else list()
    stack[[length(stack) + 1]] <- snapshot
    if (length(stack) > 50) stack <- tail(stack, 50)  # cap depth
    h[[tok]] <- stack
    fp_history(h)
  }

  # Helper: apply an edit fn to selected indices for current token
  # action  : short verb shown in the Edit log (e.g., "Halve", "Smooth")
  # details : extra context (e.g., "median, window=3")  — optional
  apply_edit <- function(edit_fn, action, details = "") {
    tok <- input$fp_corr_token
    sel <- selected_indices()
    if (length(sel) == 0) {
      showNotification("Select one or more f0 points first (click or box-select on the plot).",
                       type = "warning", duration = 4)
      return()
    }
    cur <- current_f0()
    if (is.null(cur)) return()
    push_history(tok, cur)
    new_df <- edit_fn(cur, sel)
    corr <- fp_corrections()
    corr[[tok]] <- new_df
    fp_corrections(corr)
    # Capture which frames were edited and their within-token times for the log
    sel_sorted <- sort(sel)
    times_at_sel <- if ("time" %in% names(cur)) cur$time[sel_sorted] else numeric(0)
    log_edit(tok, action, length(sel), details,
             indices = sel_sorted, times = times_at_sel, n_total = nrow(cur))
    showNotification(sprintf("%s applied to %d frame(s).", action, length(sel)),
                     type = "message", duration = 2)
  }

  # Selected point indices, derived from plotly events.
  # Defensive against malformed event payloads (plotly sends "NA" as JSON
  # for empty events, which Shiny can't parse cleanly).
  selected_indices <- reactive({
    # suppressWarnings() silences plotly's startup-time "event not
    # registered" warning. The event handlers ARE registered via
    # plotly::event_register() in the renderPlotly block, but the plot
    # is built lazily, so this reactive may evaluate before the plot
    # has had a chance to register. Once the user navigates to the
    # F0 Correction tab and the plot renders, registration takes hold
    # and the event_data calls return real selections.
    sel <- tryCatch(
      suppressWarnings(
        plotly::event_data("plotly_selected", source = "fp_corr_plot")
      ),
      error = function(e) NULL
    )
    if (!is.null(sel) && is.data.frame(sel) && nrow(sel) > 0 &&
        "customdata" %in% names(sel)) {
      cd <- suppressWarnings(as.integer(sel$customdata))
      cd <- cd[!is.na(cd)]
      if (length(cd) > 0) return(unique(cd))
    }
    click <- tryCatch(
      suppressWarnings(
        plotly::event_data("plotly_click", source = "fp_corr_plot")
      ),
      error = function(e) NULL
    )
    if (!is.null(click) && is.data.frame(click) && nrow(click) > 0 &&
        "customdata" %in% names(click)) {
      cd <- suppressWarnings(as.integer(click$customdata))
      cd <- cd[!is.na(cd)]
      if (length(cd) > 0) return(cd)
    }
    integer(0)
  })

  # ---- Sidebar ----
  # We render the sidebar ONCE per session, with an empty/placeholder dropdown.
  # An observer (below) updates the dropdown's choices via updateSelectInput()
  # as filtered_tokens() changes — so uploading the flagged-tokens CSV updates
  # the token list WITHOUT re-rendering (and therefore resetting) the sidebar,
  # the open <details> drawer, or the fileInput element.
  #
  # suspendWhenHidden = FALSE is required: this uiOutput lives inside a
  # conditionalPanel and Shiny does not always call bindAll() on the new
  # content when the panel becomes visible, leaving input$fp_corr_token
  # unregistered. Eager rendering side-steps that.
  #
  # Plain (non-selectize) select is also intentional — selectize caused
  # brittle Shiny bind state on first render of dynamic content.
  output$ui_fp_correction <- renderUI({
    token_choices  <- c("(run F0 Extraction first)" = "")
    token_selected <- ""
    tagList(
      # ---- Per-group styling (injected once per re-render of this sidebar) ----
      tags$style(HTML("
        .fp-edit-group {
          background: #f7faf9;
          border-left: 3px solid #78c2ad;
          padding: 6px 10px 8px 10px;
          margin-top: 8px;
          border-radius: 3px;
        }
        .fp-edit-group-label {
          font-size: 0.72rem;
          text-transform: uppercase;
          letter-spacing: 0.6px;
          color: #4a7868;
          font-weight: 700;
          margin-bottom: 5px;
        }
        .fp-edit-row {
          display: flex; gap: 4px; flex-wrap: wrap; align-items: center;
        }
        .fp-edit-meta { font-size: 0.80rem; color: #666; }
      ")),

      # ---- Token + progress + nav ----
      selectInput("fp_corr_token", "Token:",
                  choices = token_choices, selected = token_selected,
                  selectize = FALSE),
      tags$div(style = "color: #888; font-size: 0.72rem; margin-top: -8px; margin-bottom: 4px; font-style: italic;",
        HTML("Tokens prefixed with <strong>●</strong> have frame-level flags (visible markers on the plot). ",
             "Unprefixed tokens are flagged for out-of-range only, so look at the whole contour.")),
      verbatimTextOutput("fp_corr_progress", placeholder = TRUE),
      div(style = "display:flex; gap:4px; align-items:center; margin-top: 6px;",
        actionButton("fp_corr_prev", HTML("&#9664;"), title = "Previous token"),
        actionButton("fp_corr_next", HTML("&#9654;"), title = "Next token")
      ),

      # ---- Flagged-tokens filter (collapsible, optional) ----
      tags$details(style = "margin-top: 8px; margin-bottom: 4px;",
        tags$summary(style = "cursor:pointer; font-size: 0.85rem; color: #4a7868; font-weight: 600;",
                     icon("filter"), " Filter by flagged tokens"),
        div(style = "padding: 6px 0 0 0;",
          fileInput("fp_corr_flagged_csv", NULL,
                    accept = c(".csv", "text/csv"),
                    placeholder = "Upload Inspect-tab CSV"),
          tags$div(style = "color: #888; font-size: 0.72rem; font-style: italic; margin-top: -8px; margin-bottom: 6px;",
            HTML("Either the <strong>full</strong> or the <strong>flagged-only</strong> ",
                 "download from the Inspect tab works.")),
          uiOutput("fp_corr_flagged_col_picker"),
          uiOutput("fp_corr_speaker_col_picker"),
          uiOutput("fp_corr_speaker_keep_picker"),
          uiOutput("fp_corr_tone_col_picker"),
          uiOutput("fp_corr_tone_keep_picker"),
          checkboxInput("fp_corr_only_flagged",
                        "Only show flagged tokens", value = FALSE),
          actionButton("fp_corr_clear_filters", "Clear filters",
                       icon = icon("filter-circle-xmark"),
                       title = "Reset all filters (CSV stays uploaded)",
                       class = "btn-sm")
        )
      ),

      tags$hr(),

      # ---- Edit section heading ----
      h5("F0 Correction"),

      # ---- Selection readout ----
      verbatimTextOutput("fp_corr_selection_text", placeholder = TRUE),

      # ---- Group 1: Quick edits ----
      div(class = "fp-edit-group",
        div(class = "fp-edit-group-label", "Quick edits"),
        div(class = "fp-edit-row",
          actionButton("fp_corr_halve",  "÷ 2",    icon = icon("arrow-down"),
                       title = "Halve f0 (fix octave doubling)"),
          actionButton("fp_corr_double", "× 2",    icon = icon("arrow-up"),
                       title = "Double f0 (fix octave halving)"),
          actionButton("fp_corr_delete", "Delete", icon = icon("trash"),
                       title = "Set selected frame(s) to NA")
        )
      ),

      # ---- Group 2: Smooth ----
      div(class = "fp-edit-group",
        div(class = "fp-edit-group-label", "Smooth"),
        div(class = "fp-edit-row",
          actionButton("fp_corr_smooth", "Smooth", icon = icon("broom"),
                       title = "Replace each selected frame with a centered window aggregate (median or mean)"),
          selectInput("fp_corr_smooth_method", NULL,
                      choices = c("median" = "median", "mean" = "mean"),
                      selected = "median", width = "90px"),
          tags$span(class = "fp-edit-meta",
                    title = "Number of consecutive frames (centered on the selected frame) the median/mean is computed over.",
                    "window:"),
          selectInput("fp_corr_smooth_window", NULL,
                      choices = c("3" = 3, "5" = 5, "7" = 7),
                      selected = 3, width = "70px")
        )
      ),

      # ---- Group 3: Interpolate / Extrapolate ----
      div(class = "fp-edit-group",
        div(class = "fp-edit-group-label", "Interpolate / Extrapolate"),
        div(class = "fp-edit-row",
          actionButton("fp_corr_interp", "Interpolate", icon = icon("wave-square"),
                       title = "Replace each selected frame by a fit through the N nearest non-selected valid frames"),
          selectInput("fp_corr_interp_method", NULL,
                      choices = c("linear"   = "linear",
                                  "parabola" = "parabola",
                                  "spline"   = "spline"),
                      selected = "linear", width = "100px"),
          tags$span(class = "fp-edit-meta",
                    title = "Number of nearest non-selected valid frames used as anchors for the fit. 'all' uses every valid frame.",
                    "window:"),
          selectInput("fp_corr_interp_window", NULL,
                      choices = c("2" = 2, "3" = 3, "5" = 5, "7" = 7, "all" = -1),
                      selected = 2, width = "70px")
        )
      ),

      # ---- Group 4: Manual entry ----
      div(class = "fp-edit-group",
        div(class = "fp-edit-group-label", "Manual entry"),
        div(class = "fp-edit-row",
          tags$span(class = "fp-edit-meta", "Set to (Hz):"),
          numericInput("fp_corr_manual_value", NULL, value = NA,
                       min = 30, max = 800, width = "100px"),
          actionButton("fp_corr_manual_apply", "Apply", icon = icon("check"))
        )
      ),

      # ---- Undo (separated from edit groups) ----
      div(style = "margin-top: 12px;",
        actionButton("fp_corr_undo", "Undo last edit",
                     icon = icon("rotate-left"))
      ),

      tags$hr(),
      tags$strong("Display:"),
      div(style = "margin-top: 4px;",
        checkboxInput("fp_corr_show_pulses",
                      "Glottal pulses on waveform",
                      value = FALSE),
        checkboxInput("fp_corr_show_candidates",
                      "Top-3 Praat candidates on f0 plot",
                      value = TRUE)
      ),

      # Praat candidates (only when current token came from .Pitch).
      # The output includes its own leading <hr> when it renders content,
      # so the sidebar avoids a double separator when this block is empty.
      uiOutput("fp_corr_candidates_ui"),

      tags$hr(),
      h5("Download"),
      div(style = "display:flex; gap:6px; flex-wrap: wrap;",
        downloadButton("fp_corr_download_current", "Current token"),
        downloadButton("fp_corr_download_all",     "All tokens")
      ),
      tags$small(style = "color:#888; display:block; margin-top:4px; line-height: 1.5;",
        HTML(paste0(
          "<strong>Current token</strong> &rarr; <code>&lt;token&gt;_f0.csv</code><br>",
          "<strong>All tokens</strong> &rarr; <code>all_correctedf0.csv</code>")))
    )
  })
  # Eager render so Shiny binds the selectInput even before the conditional
  # panel becomes visible (avoids unbound input$fp_corr_token).
  outputOptions(output, "ui_fp_correction", suspendWhenHidden = FALSE)
  # NOTE: outputOptions for the filter-drawer child uiOutputs
  # (fp_corr_flagged_col_picker, _speaker_col_picker, etc.) is set further
  # down, *after* each output$... <- renderUI(...) is defined. Setting
  # them here errors with "not in list of output objects".

  # Keep the token dropdown in sync with filtered_tokens() WITHOUT re-rendering
  # the sidebar. The currently-selected token is preserved if it still appears
  # in the new filtered list; otherwise we fall back to the first token.
  # Display labels are prefixed with a small bullet for tokens that have
  # frame-level flags (so you can tell them apart from range-only flagged
  # tokens at a glance). Underlying values stay as plain token names so all
  # downstream matching logic keeps working.
  observe({
    toks <- filtered_tokens()
    if (length(toks) == 0) {
      # Pick a more specific placeholder depending on why the list is empty:
      # no extracted data yet vs. all extracted tokens filtered out.
      df <- fp_f0_data()
      placeholder <- if (is.null(df) || nrow(df) == 0) {
        "(run F0 Extraction first)"
      } else {
        "(no tokens match active filters)"
      }
      updateSelectInput(session, "fp_corr_token",
                        choices  = setNames("", placeholder),
                        selected = "")
      return()
    }

    frames_by_tok <- fp_flagged_frames()
    labels <- if (!is.null(frames_by_tok) && length(frames_by_tok) > 0) {
      strip_ext  <- isTRUE(input$fp_corr_strip_ext)
      tok_keys   <- make_corr_key(toks, strip_ext)
      flagged_set <- names(frames_by_tok)
      ifelse(tok_keys %in% flagged_set, paste("●", toks), toks)
    } else {
      toks
    }
    named_choices <- setNames(toks, labels)

    current  <- isolate(input$fp_corr_token)
    selected <- if (!is.null(current) && nzchar(current) && current %in% toks) current
                else toks[1]
    updateSelectInput(session, "fp_corr_token",
                      choices  = named_choices,
                      selected = selected)
  })

  # Prev / Next handlers — step through the *filtered* token list, wrap at ends
  observeEvent(input$fp_corr_prev, {
    tokens <- filtered_tokens()
    if (length(tokens) == 0) return()
    idx <- match(input$fp_corr_token, tokens)
    new <- if (is.na(idx) || idx <= 1) length(tokens) else idx - 1
    updateSelectInput(session, "fp_corr_token", selected = tokens[new])
  })
  observeEvent(input$fp_corr_next, {
    tokens <- filtered_tokens()
    if (length(tokens) == 0) return()
    idx <- match(input$fp_corr_token, tokens)
    new <- if (is.na(idx) || idx >= length(tokens)) 1 else idx + 1
    updateSelectInput(session, "fp_corr_token", selected = tokens[new])
  })

  # ---- Flagged-token filter handlers ----
  # Clear filters: reset all filter inputs back to "off" state, but keep the
  # CSV uploaded so the user can re-enable filtering without re-uploading.
  observeEvent(input$fp_corr_clear_filters, {
    updateSelectInput(session,    "fp_corr_speaker_col", selected = "")
    updateSelectInput(session,    "fp_corr_tone_col",    selected = "")
    updateCheckboxInput(session,  "fp_corr_only_flagged", value = FALSE)
    showNotification("Filters cleared. The uploaded CSV is still loaded.",
                     type = "message", duration = 3)
  })

  observeEvent(input$fp_corr_flagged_csv, {
    req(input$fp_corr_flagged_csv)
    df <- tryCatch(
      utils::read.csv(input$fp_corr_flagged_csv$datapath, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || ncol(df) == 0) {
      showNotification("Could not read flagged-tokens CSV.",
                       type = "warning", duration = 4)
      return()
    }
    fp_flagged_raw(df)
    showNotification(sprintf("Loaded CSV: %d rows, %d columns. Pick the token column below.",
                             nrow(df), ncol(df)),
                     type = "message", duration = 4)
  })

  output$fp_corr_flagged_col_picker <- renderUI({
    df <- fp_flagged_raw()
    if (is.null(df)) return(NULL)
    cols <- names(df)
    default <- if ("token" %in% cols) "token"
               else if ("token_id" %in% cols) "token_id"
               else cols[1]
    tagList(
      selectInput("fp_corr_flagged_col",
                  "Token column (match key)",
                  choices = cols, selected = default, width = "100%"),
      tags$div(style = "color: #888; font-size: 0.75rem; margin-top: -8px; margin-bottom: 6px; font-style: italic;",
        "Values in this column must match the audio file basenames ",
        "(e.g. ", tags$code("S01_T1_001"), " for ", tags$code("S01_T1_001.wav"), ")."),
      checkboxInput("fp_corr_strip_ext",
                    "Strip file extensions when matching",
                    value = TRUE)
    )
  })

  # ---- Speaker column picker (optional) ----
  output$fp_corr_speaker_col_picker <- renderUI({
    df <- fp_flagged_raw()
    if (is.null(df)) return(NULL)
    cols <- c("(none)" = "", names(df))
    guess <- names(df)[grepl("^(speaker|spk|talker|subject|participant)$",
                             names(df), ignore.case = TRUE)]
    default <- if (length(guess)) guess[1] else ""
    selectInput("fp_corr_speaker_col",
                "Speaker column (optional)",
                choices = cols, selected = default, width = "100%")
  })

  # ---- Speaker keep-list (only when speaker column is picked) ----
  output$fp_corr_speaker_keep_picker <- renderUI({
    df <- fp_flagged_raw()
    col <- input$fp_corr_speaker_col
    if (is.null(df) || is.null(col) || !nzchar(col) || !(col %in% names(df))) return(NULL)
    levs <- sort(unique(as.character(df[[col]])))
    levs <- levs[!is.na(levs) & nzchar(levs)]
    checkboxGroupInput("fp_corr_speaker_keep",
                       "Keep speakers:",
                       choices = levs, selected = levs, inline = TRUE)
  })

  # ---- Tone column picker (optional) ----
  output$fp_corr_tone_col_picker <- renderUI({
    df <- fp_flagged_raw()
    if (is.null(df)) return(NULL)
    cols <- c("(none)" = "", names(df))
    guess <- names(df)[grepl("^(tone|tone_cat|tonecat|category|tonal_category)$",
                             names(df), ignore.case = TRUE)]
    default <- if (length(guess)) guess[1] else ""
    selectInput("fp_corr_tone_col",
                "Tone column (optional)",
                choices = cols, selected = default, width = "100%")
  })

  # ---- Tone keep-list (only when tone column is picked) ----
  output$fp_corr_tone_keep_picker <- renderUI({
    df <- fp_flagged_raw()
    col <- input$fp_corr_tone_col
    if (is.null(df) || is.null(col) || !nzchar(col) || !(col %in% names(df))) return(NULL)
    levs <- sort(unique(as.character(df[[col]])))
    levs <- levs[!is.na(levs) & nzchar(levs)]
    checkboxGroupInput("fp_corr_tone_keep",
                       "Keep tones:",
                       choices = levs, selected = levs, inline = TRUE)
  })

  # The filter drawer's child uiOutputs sit inside a <details> element that is
  # closed on first render. Without suspendWhenHidden = FALSE, Shiny suspends
  # them and skips re-evaluation when fp_flagged_raw() updates (CSV upload),
  # so the speaker / tone column pickers never appear.
  outputOptions(output, "fp_corr_flagged_col_picker",  suspendWhenHidden = FALSE)
  outputOptions(output, "fp_corr_speaker_col_picker",  suspendWhenHidden = FALSE)
  outputOptions(output, "fp_corr_speaker_keep_picker", suspendWhenHidden = FALSE)
  outputOptions(output, "fp_corr_tone_col_picker",     suspendWhenHidden = FALSE)
  outputOptions(output, "fp_corr_tone_keep_picker",    suspendWhenHidden = FALSE)

  # Extract the set of "flagged tokens" whenever the user (re-)picks the column.
  # Detection rules (first match wins):
  #   1. CSV has 'flagged_token'  -> use rows where flagged_token == TRUE
  #      (token-level flag from the Inspect tab: out-of-range OR jump-flagged)
  #   2. CSV has 'flagged_jump'   -> use rows where flagged_jump == TRUE
  #      (frame-level only — token is flagged if any of its frames are)
  #   3. Neither column           -> every row is treated as a flagged entry
  #      (CSV is treated as a pre-filtered list of bad tokens)
  #
  # If the CSV has flagged_jump + time, also build per-frame info so individual
  # samples get highlighted in the plot.
  observeEvent(input$fp_corr_flagged_col, {
    df <- fp_flagged_raw()
    col <- input$fp_corr_flagged_col
    req(df, col, col %in% names(df))

    # --- 1) Token-level flag detection ---
    if ("flagged_token" %in% names(df)) {
      flagged_df <- df[as.logical(df$flagged_token) %in% TRUE, , drop = FALSE]
      flag_source <- "flagged_token column"
    } else if ("flagged_jump" %in% names(df)) {
      flagged_df <- df[as.logical(df$flagged_jump) %in% TRUE, , drop = FALSE]
      flag_source <- "flagged_jump column (any-frame match)"
    } else {
      flagged_df <- df
      flag_source <- "every row in CSV"
    }
    toks <- unique(as.character(flagged_df[[col]]))
    toks <- toks[!is.na(toks) & nzchar(toks)]
    fp_flagged_tokens(toks)

    showNotification(
      sprintf("Flagged tokens identified from %s: %d unique.",
              flag_source, length(toks)),
      type = "message", duration = 4
    )

    # --- 2) Per-frame flag info (for in-plot highlighting) ---
    has_jump <- "flagged_jump" %in% names(df)
    has_time <- "time" %in% names(df) || any(grepl("^time$", names(df), ignore.case = TRUE))
    if (has_jump && has_time) {
      time_col <- if ("time" %in% names(df)) "time"
                  else grep("^time$", names(df), ignore.case = TRUE, value = TRUE)[1]
      notes_col <- if ("flag_notes" %in% names(df)) "flag_notes" else NULL
      sub <- df[as.logical(df$flagged_jump) %in% TRUE, , drop = FALSE]
      sub <- sub[!is.na(sub[[col]]) & !is.na(sub[[time_col]]), , drop = FALSE]
      if (nrow(sub) > 0) {
        # Normalise key (strip ext + lowercase) so per-frame lookup matches the
        # extracted basenames regardless of how the CSV stored the filenames.
        keys <- make_corr_key(sub[[col]], isTRUE(input$fp_corr_strip_ext))
        frames_by_token <- split(
          data.frame(
            time = as.numeric(sub[[time_col]]),
            note = if (!is.null(notes_col)) as.character(sub[[notes_col]]) else "",
            stringsAsFactors = FALSE
          ),
          keys
        )
        fp_flagged_frames(frames_by_token)
        showNotification(
          sprintf("Per-frame highlights: %d flagged frames across %d tokens.",
                  nrow(sub), length(frames_by_token)),
          type = "message", duration = 4
        )
      } else {
        fp_flagged_frames(NULL)
      }
    } else {
      fp_flagged_frames(NULL)
    }
  })

  # Normalise a token-like string for matching:
  # optionally strip file extension, then trim whitespace and lowercase.
  make_corr_key <- function(x, strip_ext = TRUE) {
    k <- as.character(x)
    if (isTRUE(strip_ext)) k <- tools::file_path_sans_ext(k)
    tolower(trimws(k))
  }

  # Filtered list of tokens to populate the dropdown.
  # Filters AND together (intersected with the set of extracted tokens):
  #   - flagged-tokens checkbox (token IDs from the CSV's token column)
  #   - speaker keep-list (when a speaker column is picked)
  #   - tone keep-list    (when a tone column is picked)
  # All comparisons normalise via make_corr_key() so case + extension
  # differences don't cause silent mismatches.
  filtered_tokens <- reactive({
    df <- fp_f0_data()
    if (is.null(df)) return(character(0))
    all_t <- sort(unique(df$token))
    strip_ext <- isTRUE(input$fp_corr_strip_ext)
    all_key <- make_corr_key(all_t, strip_ext)
    keep <- all_t

    raw       <- fp_flagged_raw()
    token_col <- input$fp_corr_flagged_col

    # 1) Flagged-tokens checkbox
    if (isTRUE(input$fp_corr_only_flagged)) {
      flagged <- fp_flagged_tokens()
      if (!is.null(flagged) && length(flagged) > 0) {
        keep <- keep[all_key %in% make_corr_key(flagged, strip_ext)]
        all_key <- make_corr_key(keep, strip_ext)
      }
    }

    # 2) Speaker keep-list — apply only when both the column AND the keep-list
    # input have been registered. NULL means "input not loaded yet" (transient
    # state right after CSV upload before the keep-list renderUI completes);
    # we skip the filter so the dropdown doesn't briefly empty out. An empty
    # character(0) means "user actively unchecked all", which DOES filter to
    # nothing — we honour that.
    spk_col  <- input$fp_corr_speaker_col
    spk_keep <- input$fp_corr_speaker_keep
    if (!is.null(raw) && !is.null(spk_col) && nzchar(spk_col) &&
        spk_col %in% names(raw) && !is.null(token_col) && token_col %in% names(raw) &&
        !is.null(spk_keep)) {
      m  <- as.character(raw[[spk_col]]) %in% spk_keep
      ok <- make_corr_key(unique(as.character(raw[[token_col]])[m]), strip_ext)
      keep <- keep[all_key %in% ok]
      all_key <- make_corr_key(keep, strip_ext)
    }

    # 3) Tone keep-list — same logic
    tone_col  <- input$fp_corr_tone_col
    tone_keep <- input$fp_corr_tone_keep
    if (!is.null(raw) && !is.null(tone_col) && nzchar(tone_col) &&
        tone_col %in% names(raw) && !is.null(token_col) && token_col %in% names(raw) &&
        !is.null(tone_keep)) {
      m  <- as.character(raw[[tone_col]]) %in% tone_keep
      ok <- make_corr_key(unique(as.character(raw[[token_col]])[m]), strip_ext)
      keep <- keep[all_key %in% ok]
    }

    keep
  })

  # NOTE: the sidebar (ui_fp_correction) is rendered once with an empty
  # placeholder dropdown. The observer above (next to the Prev / Next handlers)
  # keeps fp_corr_token's choices in sync with filtered_tokens() via
  # updateSelectInput, so uploading the flagged-tokens CSV no longer
  # re-renders the sidebar and reset the file input / drawer state.

  # Progress counter: current position + how many tokens have been edited
  output$fp_corr_progress <- renderText({
    df <- fp_f0_data()
    has_data <- !is.null(df) && nrow(df) > 0
    if (!has_data) return("")          # no extraction yet — keep area quiet
    toks <- filtered_tokens()
    if (length(toks) == 0) {
      # Data exists but the active filter(s) exclude everything.
      active <- character(0)
      if (isTRUE(input$fp_corr_only_flagged)) active <- c(active, "flagged list")
      if (!is.null(input$fp_corr_speaker_col) && nzchar(input$fp_corr_speaker_col)) active <- c(active, "speaker")
      if (!is.null(input$fp_corr_tone_col)    && nzchar(input$fp_corr_tone_col))    active <- c(active, "tone")
      if (length(active) > 0) {
        return(sprintf("No tokens match the active filter%s (%s).",
                       if (length(active) > 1) "s" else "",
                       paste(active, collapse = " + ")))
      }
      return("")
    }
    cur <- input$fp_corr_token
    idx <- if (is.null(cur) || !nzchar(cur)) 0L
           else { m <- match(cur, toks); if (is.na(m)) 0L else m }
    n_edited <- length(fp_corrections())
    flagged_total <- if (isTRUE(input$fp_corr_only_flagged) &&
                         !is.null(fp_flagged_tokens())) {
      sprintf("  ·  Flagged: %d", length(toks))
    } else {
      ""
    }
    sprintf("Token %d / %d  ·  Edited: %d%s",
            idx, length(toks), n_edited, flagged_total)
  })

  # Selection text: count + time / f0 readout of selected frame(s)
  output$fp_corr_selection_text <- renderText({
    sel <- selected_indices()
    if (length(sel) == 0) {
      return("No frames selected.\nClick a point, or box/lasso-select.")
    }
    cur <- current_f0()
    if (is.null(cur)) return(sprintf("Selected: %d frame(s).", length(sel)))
    sub <- cur[sel, , drop = FALSE]
    if (nrow(sub) == 1) {
      sprintf("Selected frame %d:\n  time: %.3f s\n  f0:   %s",
              sel, sub$time,
              if (is.na(sub$f0)) "NA" else sprintf("%.2f Hz", sub$f0))
    } else {
      sprintf(paste0("Selected: %d frames\n",
                     "  time: %.3f → %.3f s\n",
                     "  f0:   %.1f → %.1f Hz"),
              nrow(sub),
              min(sub$time, na.rm = TRUE), max(sub$time, na.rm = TRUE),
              min(sub$f0, na.rm = TRUE), max(sub$f0, na.rm = TRUE))
    }
  })

  # ---- Audio player (base64 data URI) ----
  output$fp_corr_audio <- renderUI({
    req(input$fp_corr_token)
    tok <- input$fp_corr_token
    audio <- fp_audio_data()
    if (is.null(audio)) return(NULL)
    row <- audio[audio$basename == tok, , drop = FALSE]
    if (nrow(row) == 0 || is.na(row$wav_path[1])) {
      return(tags$div(style = "color:#888; font-style:italic;",
        "No .wav file for this token."))
    }
    bin <- readBin(row$wav_path[1], "raw", n = file.info(row$wav_path[1])$size)
    uri <- paste0("data:audio/wav;base64,", base64enc::base64encode(bin))
    tags$audio(controls = NA, src = uri, type = "audio/wav",
               style = "width: 100%; max-width: 600px;")
  })

  # ---- Combined waveform + f0 plot (single plot_ly w/ stacked y-axes) ----
  # Using one plot_ly() with yaxis (f0) + yaxis2 (waveform) avoids the
  # source-ambiguity problem that subplot() introduces, so plotly_click /
  # plotly_selected events fire reliably under one source.
  output$fp_corr_plot <- plotly::renderPlotly({
    req(input$fp_corr_token, current_f0())
    tok <- input$fp_corr_token
    f0_df <- current_f0()
    sel <- selected_indices()

    audio <- fp_audio_data()
    row <- if (!is.null(audio)) audio[audio$basename == tok, , drop = FALSE] else NULL

    # --- Optional waveform downsampled for plotting ---
    wav <- NULL
    if (!is.null(row) && nrow(row) > 0 && !is.na(row$wav_path[1])) {
      w <- tryCatch(tuneR::readWave(row$wav_path[1]), error = function(e) NULL)
      if (!is.null(w)) {
        samp <- as.integer(w@left)
        n <- length(samp)
        max_pts <- 5000
        if (n > max_pts) {
          idx <- round(seq(1, n, length.out = max_pts))
          samp_plot <- samp[idx]
          t_wav <- (idx - 1) / w@samp.rate
        } else {
          samp_plot <- samp
          t_wav <- (seq_along(samp) - 1) / w@samp.rate
        }
        wav <- list(t = t_wav, y = samp_plot / max(abs(samp_plot)))
      }
    }

    # --- Per-frame flag lookup (from Inspect-tab CSV, if uploaded) ---
    # Match frames whose time is within half a frame-step of a flagged time.
    flagged_idx <- integer(0); flag_notes_full <- rep("", nrow(f0_df))
    frames_by_tok <- fp_flagged_frames()
    tok_key <- make_corr_key(tok, isTRUE(input$fp_corr_strip_ext))
    if (!is.null(frames_by_tok) && tok_key %in% names(frames_by_tok)) {
      fl <- frames_by_tok[[tok_key]]
      step <- if (nrow(f0_df) >= 2) median(diff(f0_df$time), na.rm = TRUE) else 0.005
      if (is.na(step) || step <= 0) step <- 0.005
      tol <- step / 2
      for (k in seq_len(nrow(fl))) {
        i <- which(abs(f0_df$time - fl$time[k]) <= tol)
        if (length(i) > 0) {
          flagged_idx <- c(flagged_idx, i[1])
          flag_notes_full[i[1]] <- fl$note[k]
        }
      }
      flagged_idx <- unique(flagged_idx)
    }

    # --- Pre-filter NA rows before plotting ---
    # Plotly silently drops NA y-values from x/y but KEEPS per-point arrays
    # (marker.color, .size) at their original length, mis-aligning colors with
    # displayed points. Filtering in R first keeps every per-point array in
    # lockstep. customdata preserves the *original* frame index so click events
    # and edit handlers stay correct.
    plot_idx <- which(!is.na(f0_df$f0))
    f0_plot <- f0_df[plot_idx, , drop = FALSE]
    flag_notes <- flag_notes_full[plot_idx]

    # Map flagged + selected indices from full-data → plot-data positions
    flagged_in_plot <- match(flagged_idx, plot_idx)
    flagged_in_plot <- flagged_in_plot[!is.na(flagged_in_plot)]
    sel_in_plot <- match(sel, plot_idx)
    sel_in_plot <- sel_in_plot[!is.na(sel_in_plot)]

    point_colors <- rep("#5cb89a", nrow(f0_plot))
    point_sizes  <- rep(7L,        nrow(f0_plot))
    if (length(flagged_in_plot) > 0) {
      point_colors[flagged_in_plot] <- "#d9534f"   # red — Inspect-flagged
      point_sizes [flagged_in_plot] <- 10L
    }
    if (length(sel_in_plot) > 0) {
      point_colors[sel_in_plot] <- "#e0712d"       # orange — selected (overrides flag)
      point_sizes [sel_in_plot] <- 12L
    }

    # Hover text — original frame index, time, f0, and flag note if any
    hover_text <- sprintf("frame %d<br>time: %.3fs<br>f0: %.1f Hz",
                          plot_idx, f0_plot$time, f0_plot$f0)
    if (length(flagged_in_plot) > 0) {
      hover_text[flagged_in_plot] <- paste0(hover_text[flagged_in_plot],
                                            "<br><b>⚑ flagged:</b> ",
                                            flag_notes[flagged_in_plot])
    }

    p <- plotly::plot_ly(source = "fp_corr_plot")

    if (!is.null(wav)) {
      p <- plotly::add_trace(
        p,
        x = wav$t, y = wav$y,
        type = "scatter", mode = "lines",
        yaxis = "y2",
        line = list(width = 0.6, color = "#5a5a5a"),
        showlegend = FALSE, hoverinfo = "skip"
      )
    }

    # --- Praat top-N pitch candidates as grey markers (under main f0) ---
    # Only when the current token came from a .Pitch file (so per-frame
    # candidates exist) AND the user has the toggle on.
    # Rank is encoded by both size and opacity (more prominent = more likely).
    # Each marker carries customdata of the form "cand_<frame_idx>_<freq>" so
    # the click handler below can route it to the same Pick-candidate logic
    # that the sidebar list uses (skipping the "select frame first" step).
    if (isTRUE(input$fp_corr_show_candidates) && !is.null(fp_pitch_candidates)) {
      cands_by_tok <- fp_pitch_candidates()
      cands_list   <- if (!is.null(cands_by_tok) && tok %in% names(cands_by_tok))
                        cands_by_tok[[tok]] else NULL
      if (!is.null(cands_list) && length(cands_list) == nrow(f0_df)) {
        n_top <- 3L
        cand_x <- numeric(0); cand_y <- numeric(0); cand_rank <- integer(0)
        cand_str <- numeric(0); cand_frame <- integer(0)
        for (i in seq_along(cands_list)) {
          cs <- cands_list[[i]]
          if (is.null(cs) || nrow(cs) == 0) next
          n_show <- min(n_top, nrow(cs))
          for (j in seq_len(n_show)) {
            freq <- cs$frequency[j]
            if (is.na(freq) || freq == 0) next      # skip unvoiced
            cand_x     <- c(cand_x, f0_df$time[i])
            cand_y     <- c(cand_y, freq)
            cand_rank  <- c(cand_rank, j)
            cand_str   <- c(cand_str, cs$strength[j])
            cand_frame <- c(cand_frame, i)
          }
        }
        if (length(cand_x) > 0) {
          cand_hover <- sprintf(
            "candidate %d  (click to apply)<br>time: %.3fs<br>f0: %.1f Hz<br>strength: %.2f",
            cand_rank, cand_x, cand_y, cand_str
          )
          # Rank-based size + opacity: more visible = higher rank (Praat-likelier)
          size_by_rank    <- c(7L, 6L, 5L)
          opacity_by_rank <- c(0.75, 0.55, 0.40)
          cand_sizes      <- size_by_rank[cand_rank]
          cand_opacities  <- opacity_by_rank[cand_rank]
          p <- plotly::add_trace(
            p,
            x = cand_x, y = cand_y,
            type = "scatter", mode = "markers+text",
            yaxis = "y",
            marker = list(size = cand_sizes, color = "#777777",
                          opacity = cand_opacities,
                          line = list(width = 0)),
            text = as.character(cand_rank),            # tiny "1" / "2" / "3" label
            textposition = "top right",
            textfont = list(size = 9, color = "#888888"),
            hovertext = cand_hover, hoverinfo = "text",
            customdata = sprintf("cand_%d_%.4f", cand_frame, cand_y),
            showlegend = FALSE
          )
        }
      }
    }

    p <- plotly::add_trace(
      p,
      x = f0_plot$time, y = f0_plot$f0,
      type = "scatter", mode = "markers+lines",
      yaxis = "y",
      marker = list(size = point_sizes, color = point_colors,
                    line = list(width = 1, color = "#2c5f4f")),
      line = list(color = "#5cb89a", width = 1),
      customdata = plot_idx,               # original frame index
      text = hover_text, hoverinfo = "text",
      showlegend = FALSE
    )

    # --- Optional TextGrid annotation strip below f0 ---
    tg <- current_textgrid()
    n_tiers <- if (!is.null(tg)) length(tg) else 0
    shapes <- list(); annotations <- list()

    if (n_tiers > 0) {
      # Stack: waveform (top) → f0 (middle) → TextGrid (bottom).
      # The shared x-axis is pinned to paper y=0 (in the bottom margin) so
      # the time-axis labels don't collide with the TextGrid strip.
      tg_bottom <- 0.00
      tg_top    <- 0.24
      tier_h    <- (tg_top - tg_bottom) / n_tiers
      f0_dom    <- c(0.32, 0.62)
      wav_dom   <- c(0.66, 1.00)

      for (k in seq_along(tg)) {
        tier <- tg[[k]]
        # First tier goes at the TOP of the TextGrid strip
        y_max <- tg_top - (k - 1) * tier_h
        y_min <- y_max - tier_h

        # Separator line between tiers (top of this tier)
        shapes[[length(shapes) + 1]] <- list(
          type = "line", xref = "paper", yref = "paper",
          x0 = 0, x1 = 1, y0 = y_max, y1 = y_max,
          line = list(color = "#ddd", width = 1)
        )
        # Tier name on the left margin
        annotations[[length(annotations) + 1]] <- list(
          x = -0.005, y = (y_min + y_max) / 2,
          xref = "paper", yref = "paper",
          xanchor = "right", text = tier$name,
          showarrow = FALSE,
          font = list(size = 14, color = "#666")
        )

        if (identical(tier$type, "interval")) {
          n_iv <- length(tier$t1)
          for (j in seq_len(n_iv)) {
            # Right boundary — extend from this tier's bottom UP through f0
            # and waveform, so the line is visible against the audio + f0.
            if (j < n_iv) {
              shapes[[length(shapes) + 1]] <- list(
                type = "line", xref = "x", yref = "paper",
                x0 = tier$t2[j], x1 = tier$t2[j],
                y0 = y_min, y1 = 1,
                line = list(color = "#999", width = 0.6, dash = "dot")
              )
            }
            lbl <- tier$label[j]
            if (!is.na(lbl) && nzchar(lbl)) {
              annotations[[length(annotations) + 1]] <- list(
                x = (tier$t1[j] + tier$t2[j]) / 2,
                y = (y_min + y_max) / 2,
                xref = "x", yref = "paper",
                text = lbl, showarrow = FALSE,
                font = list(size = 15, color = "#222")
              )
            }
          }
        } else if (identical(tier$type, "point")) {
          for (j in seq_along(tier$t)) {
            shapes[[length(shapes) + 1]] <- list(
              type = "line", xref = "x", yref = "paper",
              x0 = tier$t[j], x1 = tier$t[j],
              y0 = y_min, y1 = 1,
              line = list(color = "#5a8caa", width = 1, dash = "dot")
            )
            lbl <- tier$label[j]
            if (!is.na(lbl) && nzchar(lbl)) {
              annotations[[length(annotations) + 1]] <- list(
                x = tier$t[j], y = y_min,
                xref = "x", yref = "paper",
                xanchor = "left", yanchor = "bottom",
                text = paste0(" ", lbl), showarrow = FALSE,
                font = list(size = 14, color = "#5a8caa")
              )
            }
          }
        }
      }
    } else {
      f0_dom  <- c(0,    0.6)
      wav_dom <- c(0.65, 1)
    }

    # --- Vertical guide line(s) for currently-selected frame(s) ---
    # Spans from the bottom of the f0 panel up to the top of the waveform,
    # so you can read off the frame's time position against both the audio
    # and the f0 contour. Drawn in the selection orange to match the marker.
    if (length(sel) > 0) {
      sel_times <- f0_df$time[sel]
      sel_times <- sel_times[!is.na(sel_times)]
      for (st in sel_times) {
        shapes[[length(shapes) + 1]] <- list(
          type = "line", xref = "x", yref = "paper",
          x0 = st, x1 = st,
          y0 = f0_dom[1], y1 = wav_dom[2],
          line = list(color = "#e0712d", width = 1, dash = "dot")
        )
      }
    }

    # --- Optional glottal pulse markers on the waveform ---
    # For each continuous voiced segment, walk forward one period = 1/f0 at
    # a time, interpolating f0 between frames. Each step lands on a glottal
    # pulse boundary. Drawn as short vertical lines within the waveform
    # y-axis (yref = "y2"), spanning the full waveform amplitude.
    if (!is.null(wav) && isTRUE(input$fp_corr_show_pulses)) {
      voiced_idx <- which(!is.na(f0_df$f0))
      if (length(voiced_idx) >= 2) {
        # Group consecutive voiced frames into segments
        gaps <- which(diff(voiced_idx) > 1)
        seg_starts <- c(1, gaps + 1)
        seg_ends   <- c(gaps, length(voiced_idx))
        for (s in seq_along(seg_starts)) {
          seg <- voiced_idx[seg_starts[s]:seg_ends[s]]
          if (length(seg) < 2) next
          t_seg <- f0_df$time[seg]
          f_seg <- f0_df$f0[seg]
          t_cur <- t_seg[1]
          t_end <- t_seg[length(t_seg)]
          # Cap pulses per segment to keep plotly fast for long tokens
          max_pulses_per_seg <- 1500
          count <- 0
          while (t_cur < t_end && count < max_pulses_per_seg) {
            shapes[[length(shapes) + 1]] <- list(
              type = "line", xref = "x", yref = "y2",
              x0 = t_cur, x1 = t_cur,
              y0 = -1, y1 = 1,
              line = list(color = "#7fb1d6", width = 0.6)
            )
            f_cur <- tryCatch(stats::approx(t_seg, f_seg, t_cur, rule = 2)$y,
                              error = function(e) NA_real_)
            if (is.na(f_cur) || f_cur <= 0) break
            t_cur <- t_cur + 1 / f_cur
            count <- count + 1
          }
        }
      }
    }

    # When TextGrid is shown the x-axis is detached from the f0 yaxis and
    # pinned to paper y=0 so it sits below the TextGrid strip.
    if (n_tiers > 0) {
      xaxis_def <- list(title = "time (s)", domain = c(0, 1),
                        anchor = "free", position = 0)
      bottom_margin <- 60
    } else {
      xaxis_def <- list(title = "time (s)", domain = c(0, 1))
      bottom_margin <- 40
    }

    layout_args <- list(
      xaxis = xaxis_def,
      yaxis = list(title = "f0 (Hz)", domain = f0_dom, fixedrange = TRUE),
      dragmode = "select",
      shapes = shapes,
      annotations = annotations,
      margin = list(l = 60, r = 20, t = 20, b = bottom_margin)
    )
    if (!is.null(wav)) {
      layout_args$yaxis2 <- list(title = "waveform",
                                 domain = wav_dom, anchor = "x",
                                 fixedrange = TRUE)
    }
    p <- do.call(plotly::layout, c(list(p), layout_args))
    # Register the click event so plotly::event_data("plotly_click", ...)
    # receives clicks without the per-session "event not registered" warning.
    p <- plotly::event_register(p, "plotly_click")
    p <- plotly::event_register(p, "plotly_selected")
    plotly::config(p, displaylogo = FALSE, scrollZoom = TRUE)
  })

  # ---- Main panel layout ----
  # The guide is rendered unconditionally so users can read it
  # before uploading audio / extracting f0.
  output$fp_correction_guide <- renderUI({
    box_style <- "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;"
    tagList(
      # Keyboard zoom/pan handler — installed once per session
      tags$script(HTML("
        (function(){
          if (window._fpCorrKeyboardZoom) return;
          window._fpCorrKeyboardZoom = true;
          document.addEventListener('keydown', function(e){
            // Skip if focus is in a text-y input
            var t = e.target && e.target.tagName ? e.target.tagName.toLowerCase() : '';
            if (t === 'input' || t === 'textarea' || t === 'select') return;
            // Only act when F0 Correction tab is visible
            var fp = window.Shiny && Shiny.shinyapp ? Shiny.shinyapp.$inputValues : null;
            if (!fp || fp.tabs_fp !== 'F0 Correction') return;
            var nav = document.querySelector('.navbar .nav-link.active');
            if (!nav || nav.textContent.trim() !== 'F0 Processing') return;
            var gd = document.getElementById('fp_corr_plot');
            if (!gd || !gd._fullLayout || !gd._fullLayout.xaxis) return;
            var xa = gd._fullLayout.xaxis;
            var range = xa.range; if (!range || range.length !== 2) return;
            var lo = range[0], hi = range[1], span = hi - lo, mid = (lo + hi) / 2;
            var newRange = null, reset = false, prevent = true;
            switch (e.key) {
              case '+': case '=':
                newRange = [mid - span * 0.4,  mid + span * 0.4]; break;   // zoom in
              case '-': case '_':
                newRange = [mid - span * 0.625, mid + span * 0.625]; break; // zoom out
              case 'ArrowLeft':
                newRange = [lo - span * 0.2, hi - span * 0.2]; break;       // pan left
              case 'ArrowRight':
                newRange = [lo + span * 0.2, hi + span * 0.2]; break;       // pan right
              case '0':
                reset = true; break;                                        // reset
              default:
                prevent = false; return;
            }
            if (prevent) e.preventDefault();
            if (reset)  Plotly.relayout(gd, {'xaxis.autorange': true});
            else        Plotly.relayout(gd, {'xaxis.range': newRange});
          });
        })();
      ")),
      tags$div(style = box_style,
        tags$strong("F0 Correction"),
        tags$p(style = "margin: 6px 0 6px 0;",
          "Work on one token at a time — pick one in the sidebar, or step through with ",
          HTML("&#9664;"), " / ", HTML("&#9654;"), ". ",
          "Click a point on the f0 plot to select a single frame, or drag a ",
          tags$strong("box / lasso"), " to select multiple frames. ",
          "Then apply one of the edits in the sidebar."),
        tags$ul(style = "margin-bottom: 4px; padding-left: 18px;",
          tags$li(tags$strong("Quick edits:"), " ", tags$code("÷ 2"),
            " (fix octave doubling), ", tags$code("× 2"),
            " (fix octave halving), ", tags$code("Delete"),
            " (set selected frames to NA)."),
          tags$li(tags$strong("Smooth:"),
            " replace each selected frame by a centered ", tags$em("median"),
            " (recommended for f0) or ", tags$em("mean"), " over the ",
            tags$em("window"), " — i.e. ", tags$em("N"),
            " consecutive frames (3 / 5 / 7) centered on the selected one."),
          tags$li(tags$strong("Interpolate / Extrapolate:"),
            " replace selected frames by a ", tags$em("linear"), ", ",
            tags$em("parabola"), " (local quadratic) or ",
            tags$em("spline"), " (cubic) fit through the nearest ",
            tags$em("N"), " non-selected valid frames (the ", tags$em("window"),
            "). If a selected frame is past the last valid anchor, the same fit ",
            "extrapolates."),
          tags$li(tags$strong("Manual entry:"),
            " type a specific Hz value and click ", tags$code("Apply"),
            " to set every selected frame to it."),
          tags$li(tags$strong("Praat candidates"),
            " (only when the token came from a ", tags$code(".Pitch"),
            " file): when exactly one frame is selected, alternative ",
            "Praat-extracted candidates appear in the sidebar with their ",
            "strengths — click one to pick it.")
        ),
        tags$p(style = "margin: 6px 0 0 0;",
          tags$strong("Undo last edit"), " reverts the most recent change. ",
          "When you're done, download the corrected f0 from the sidebar."
        )
      ),
      tags$h4("Audio"),
      uiOutput("fp_corr_audio"),
      tags$h4(style = "margin-top: 14px;", "Waveform + f0"),
      tags$p(style = "color: #888; font-size: 0.82rem; margin: 0 0 6px 0;",
        "Zoom with the mouse wheel or use ",
        tags$kbd("+"), " / ", tags$kbd("-"), " (zoom), ",
        tags$kbd("←"), " / ", tags$kbd("→"), " (pan), ",
        tags$kbd("0"), " (reset). Only the time axis zooms; f0 / waveform stay fixed."),
      plotly::plotlyOutput("fp_corr_plot", height = "560px"),
      tags$h4(style = "margin-top: 16px;", "Edit log"),
      tags$div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 6px;",
        downloadButton("fp_corr_log_download", "Download edit log (CSV)",
                       icon = icon("download")),
        actionButton("fp_corr_log_clear", "Clear log", icon = icon("eraser")),
        tags$span(style = "color: #888; font-size: 0.8rem; font-style: italic;",
                  "Chronological record of every edit applied in this session.")
      ),
      DT::dataTableOutput("fp_corr_edits_table")
    )
  })

  # Chronological edit log: one row per finalised apply_edit this session
  # (undo removes the corresponding row rather than appending its own).
  output$fp_corr_edits_table <- DT::renderDataTable({
    df <- fp_edit_log()
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(date = character(0), token = character(0), action = character(0),
                   `n frames` = integer(0), `frame indices` = character(0),
                   `frame times (s)` = character(0), `frame %` = character(0),
                   details = character(0),
                   stringsAsFactors = FALSE, check.names = FALSE),
        rownames = FALSE,
        options = list(pageLength = 10, dom = "tip",
                       language = list(emptyTable = "No edits yet."))
      ))
    }
    # Show most-recent first; readable column names
    df_show <- df[seq.int(nrow(df), 1L), , drop = FALSE]
    names(df_show) <- c("date", "token", "action", "n frames",
                        "frame indices", "frame times (s)", "frame %", "details")
    DT::datatable(
      df_show, rownames = FALSE,
      options = list(pageLength = 10, dom = "tip", scrollX = TRUE,
                     columnDefs = list(list(className = "dt-center", targets = c(0, 3))))
    )
  })

  output$fp_corr_log_download <- downloadHandler(
    filename = function() {
      sprintf("edit_log_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      df <- fp_edit_log()
      if (nrow(df) == 0) {
        writeLines("# Shinytone: edit log is empty.", file)
        showNotification("Edit log is empty — nothing to download yet.",
                         type = "warning", duration = 4)
        return()
      }
      utils::write.csv(df, file, row.names = FALSE)
    }
  )

  observeEvent(input$fp_corr_log_clear, {
    if (nrow(fp_edit_log()) == 0) return()
    fp_edit_log(data.frame(
      date          = character(0),
      token         = character(0),
      action        = character(0),
      n_frames      = integer(0),
      frame_indices = character(0),
      frame_times_s = character(0),
      frame_pct     = character(0),
      details       = character(0),
      stringsAsFactors = FALSE
    ))
    showNotification("Edit log cleared.", type = "message", duration = 2)
  })

  # ---- Edit handlers ----
  observeEvent(input$fp_corr_halve, {
    apply_edit(function(df, idx) { df$f0[idx] <- df$f0[idx] / 2; df },
               "Halve", "f0 / 2")
  })
  observeEvent(input$fp_corr_double, {
    apply_edit(function(df, idx) { df$f0[idx] <- df$f0[idx] * 2; df },
               "Double", "f0 * 2")
  })
  observeEvent(input$fp_corr_delete, {
    apply_edit(function(df, idx) { df$f0[idx] <- NA_real_; df },
               "Delete", "set to NA")
  })
  observeEvent(input$fp_corr_interp, {
    # Method × window controls how each selected frame is interpolated.
    # The "anchors" are every non-selected, non-NA frame; window=N picks
    # the N nearest anchors per target (window=-1 means "all").
    method <- input$fp_corr_interp_method
    if (is.null(method) || !nzchar(method)) method <- "linear"
    win <- suppressWarnings(as.integer(input$fp_corr_interp_window))
    if (is.na(win)) win <- 2L

    apply_edit(function(df, idx) {
      tt <- df$time; y <- df$f0; n <- length(y)
      valid <- !is.na(y) & !(seq_len(n) %in% idx)
      x_anc_all <- tt[valid]; y_anc_all <- y[valid]
      if (length(x_anc_all) < 2) {
        y[idx] <- NA_real_; df$f0 <- y; return(df)
      }

      # Per-method minimum anchors
      min_anc <- switch(method, spline = 4L, parabola = 3L, 2L)
      # Effective window: -1 means use all
      win_use <- if (win == -1L) length(x_anc_all) else max(win, min_anc)

      x_new <- tt[idx]

      pick_local <- function(target) {
        k <- min(win_use, length(x_anc_all))
        ord <- order(abs(x_anc_all - target))[seq_len(k)]
        ord <- sort(ord)  # keep time-order for stable fits
        list(x = x_anc_all[ord], y = y_anc_all[ord])
      }

      new_vals <- vapply(x_new, function(target) {
        loc <- pick_local(target)
        n_loc <- length(loc$x)
        if (method == "spline" && n_loc >= 4) {
          return(stats::spline(loc$x, loc$y, xout = target, method = "fmm")$y)
        }
        if (method == "parabola" && n_loc >= 3) {
          out <- tryCatch({
            cf <- coef(stats::lm(loc$y ~ loc$x + I(loc$x^2)))
            unname(cf[1] + cf[2] * target + cf[3] * target^2)
          }, error = function(e) NA_real_)
          if (!is.na(out)) return(out)
        }
        # linear: 2-point if exactly 2 anchors, else local linear regression
        if (n_loc == 2) {
          stats::approx(loc$x, loc$y, xout = target, rule = 2)$y
        } else {
          out <- tryCatch({
            cf <- coef(stats::lm(loc$y ~ loc$x))
            unname(cf[1] + cf[2] * target)
          }, error = function(e) NA_real_)
          if (is.na(out)) stats::approx(loc$x, loc$y, xout = target, rule = 2)$y
          else out
        }
      }, numeric(1))

      y[idx] <- new_vals
      df$f0 <- y; df
    }, "Interpolate",
       sprintf("%s, window=%s", method, if (win == -1L) "all" else as.character(win)))
  })
  observeEvent(input$fp_corr_smooth, {
    method <- input$fp_corr_smooth_method
    if (is.null(method) || !nzchar(method)) method <- "median"
    win <- suppressWarnings(as.integer(input$fp_corr_smooth_window))
    if (is.na(win) || win < 3) win <- 3
    half <- (win - 1L) %/% 2L
    agg <- if (method == "mean") mean else stats::median
    apply_edit(function(df, idx) {
      y <- df$f0; n <- length(y)
      new_y <- y
      for (i in idx) {
        lo <- max(1L, i - half); hi <- min(n, i + half)
        vals <- y[lo:hi]
        if (any(!is.na(vals))) new_y[i] <- agg(vals, na.rm = TRUE)
      }
      df$f0 <- new_y; df
    }, "Smooth", sprintf("%s, window=%d", method, win))
  })
  observeEvent(input$fp_corr_manual_apply, {
    v <- suppressWarnings(as.numeric(input$fp_corr_manual_value))
    if (is.na(v) || v <= 0) {
      showNotification("Enter a positive Hz value first.",
                       type = "warning", duration = 4)
      return()
    }
    apply_edit(function(df, idx) { df$f0[idx] <- v; df },
               "Manual", sprintf("set to %.2f Hz", v))
  })
  observeEvent(input$fp_corr_undo, {
    tok <- input$fp_corr_token
    h <- fp_history()
    if (!(tok %in% names(h)) || length(h[[tok]]) == 0) {
      showNotification("Nothing to undo.", type = "warning", duration = 3)
      return()
    }
    stack <- h[[tok]]
    last  <- stack[[length(stack)]]
    stack <- stack[-length(stack)]
    h[[tok]] <- stack
    fp_history(h)

    corr <- fp_corrections()
    corr[[tok]] <- last
    fp_corrections(corr)

    # Remove the most recent edit log row for this token so the log
    # reflects only the finalised edits (rather than accumulating "Undo"
    # entries). If the user undoes everything for a token, the log for
    # that token ends up empty, which is what we want.
    cur_log <- fp_edit_log()
    if (nrow(cur_log) > 0) {
      tok_rows <- which(cur_log$token == tok)
      if (length(tok_rows) > 0) {
        cur_log <- cur_log[-tail(tok_rows, 1), , drop = FALSE]
        fp_edit_log(cur_log)
      }
    }

    showNotification("Undone.", type = "message", duration = 2)
  })

  # ---- Praat candidate selection (only when current token came from .Pitch) ----
  # Shown only if (a) candidates are available for the current token, and
  # (b) exactly one frame is selected. Lists all alternative candidates
  # with frequency + strength; clicking one sets that frame's f0 to it.
  current_candidates <- reactive({
    if (is.null(fp_pitch_candidates)) return(NULL)
    cands <- fp_pitch_candidates()
    tok <- input$fp_corr_token
    if (is.null(tok) || !(tok %in% names(cands))) return(NULL)
    sel <- selected_indices()
    if (length(sel) != 1L) return(NULL)
    cur <- current_f0(); if (is.null(cur)) return(NULL)
    if (sel < 1 || sel > length(cands[[tok]])) return(NULL)
    cdf <- cands[[tok]][[sel]]
    if (is.null(cdf) || nrow(cdf) == 0) return(NULL)
    # Mark which candidate currently equals the displayed f0 (within rounding)
    current_f0_val <- cur$f0[sel]
    cdf$is_current <- !is.na(current_f0_val) &
      abs(cdf$frequency - current_f0_val) < 0.01
    cdf
  })

  output$fp_corr_candidates_ui <- renderUI({
    if (is.null(fp_pitch_candidates)) return(NULL)
    cands <- fp_pitch_candidates()
    tok <- input$fp_corr_token
    if (is.null(tok) || !(tok %in% names(cands))) {
      return(NULL)  # not a .Pitch source — section hidden
    }
    cdf <- current_candidates()
    if (is.null(cdf) || nrow(cdf) == 0) {
      return(tagList(
        tags$hr(),
        tags$strong("Praat candidates:"),
        tags$div(style = "color:#888; font-style: italic; font-size: 0.85rem;",
          "Select exactly one frame on the plot to see its alternative candidates.")
      ))
    }
    # Sort by strength descending
    cdf <- cdf[order(-cdf$strength), , drop = FALSE]
    rows <- lapply(seq_len(nrow(cdf)), function(i) {
      freq <- cdf$frequency[i]
      strength <- cdf$strength[i]
      label_txt <- if (freq == 0) {
        sprintf("unvoiced (s = %.3f)", strength)
      } else {
        sprintf("%.2f Hz  (s = %.3f)", freq, strength)
      }
      mark <- if (isTRUE(cdf$is_current[i])) " ✓" else ""
      tags$button(
        type = "button", class = "btn btn-default btn-sm",
        style = "display:block; width:100%; text-align:left; margin-bottom: 2px;",
        onclick = sprintf(
          "Shiny.setInputValue('fp_corr_pick_candidate', %s, {priority:'event'});",
          freq
        ),
        paste0(label_txt, mark)
      )
    })
    tagList(
      tags$hr(),
      tags$strong("Praat candidates:"),
      tags$div(style = "color:#666; font-size: 0.78rem; margin-bottom: 4px;",
               "Click an alternative to set this frame's f0."),
      tagList(rows)
    )
  })

  observeEvent(input$fp_corr_pick_candidate, {
    v <- suppressWarnings(as.numeric(input$fp_corr_pick_candidate))
    if (is.na(v)) return()
    apply_edit(function(df, idx) {
      # Praat unvoiced is freq = 0 → write NA
      df$f0[idx] <- if (v == 0) NA_real_ else v
      df
    }, "Pick candidate", sprintf("%.2f Hz", v))
  })

  # Click directly on a grey candidate marker in the plot → apply that
  # candidate to its frame, no need to select a frame first.
  # customdata for candidate markers is "cand_<frame_idx>_<freq>"; the regular
  # f0 trace's customdata is a plain integer, so the prefix check cleanly
  # distinguishes them.
  observeEvent(
    suppressWarnings(plotly::event_data("plotly_click",
                                        source = "fp_corr_plot")),
  {
    click <- suppressWarnings(
      plotly::event_data("plotly_click", source = "fp_corr_plot")
    )
    if (is.null(click) || is.null(click$customdata)) return()
    cd <- as.character(click$customdata)
    if (!startsWith(cd, "cand_")) return()
    parts <- strsplit(sub("^cand_", "", cd), "_", fixed = TRUE)[[1]]
    if (length(parts) < 2) return()
    frame_idx <- suppressWarnings(as.integer(parts[1]))
    freq      <- suppressWarnings(as.numeric(parts[2]))
    if (is.na(frame_idx) || is.na(freq)) return()

    tok <- input$fp_corr_token
    cur <- current_f0()
    if (is.null(cur) || frame_idx < 1 || frame_idx > nrow(cur)) return()
    push_history(tok, cur)
    cur$f0[frame_idx] <- if (freq == 0) NA_real_ else freq
    corr <- fp_corrections()
    corr[[tok]] <- cur
    fp_corrections(corr)
    log_edit(tok, "Pick candidate (plot click)", 1L,
             sprintf("%.2f Hz", freq),
             indices = frame_idx, times = cur$time[frame_idx], n_total = nrow(cur))
    showNotification(
      sprintf("Set frame %d to %.2f Hz (Praat candidate).", frame_idx, freq),
      type = "message", duration = 2
    )
  })

  # ---- Helper: build full (orig + corrected) dataframe ----
  build_corrected_df <- function() {
    orig <- fp_f0_data()
    if (is.null(orig)) return(NULL)
    corr <- fp_corrections()
    out <- orig
    out$f0_corrected <- out$f0
    if (length(corr) > 0) {
      for (tok in names(corr)) {
        mask <- out$token == tok
        out$f0_corrected[mask] <- corr[[tok]]$f0[seq_len(sum(mask))]
      }
    }
    out
  }

  # ---- Download: current token only (named after the token) ----
  output$fp_corr_download_current <- downloadHandler(
    filename = function() {
      tok <- input$fp_corr_token
      if (is.null(tok) || !nzchar(tok)) tok <- "current"
      paste0(tok, "_f0.csv")
    },
    content = function(file) {
      out <- build_corrected_df()
      req(out)
      tok <- input$fp_corr_token
      req(tok)
      sub <- out[out$token == tok, , drop = FALSE]
      fname <- paste0(tok, "_f0.csv")
      write.csv(sub, file, row.names = FALSE)
      showNotification(paste("Saved", fname),
                       type = "message", duration = 4)
    }
  )

  # ---- Download: all tokens in one CSV ----
  output$fp_corr_download_all <- downloadHandler(
    filename = function() "all_correctedf0.csv",
    content = function(file) {
      out <- build_corrected_df()
      req(out)
      write.csv(out, file, row.names = FALSE)
      showNotification("Saved all_correctedf0.csv",
                       type = "message", duration = 4)
    }
  )
}
