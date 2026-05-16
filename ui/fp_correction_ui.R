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
  # Optional flagged-token filter: raw uploaded CSV + extracted token IDs.
  # fp_flagged_frames (if the CSV came from the Inspect tab) holds per-frame
  # flag info keyed by token: a named list, token -> data.frame(time, note).
  fp_flagged_raw    <- reactiveVal(NULL)
  fp_flagged_tokens <- reactiveVal(NULL)
  fp_flagged_frames <- reactiveVal(NULL)

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
  apply_edit <- function(edit_fn, label) {
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
    showNotification(sprintf("%s applied to %d frame(s).", label, length(sel)),
                     type = "message", duration = 2)
  }

  # Selected point indices, derived from plotly events.
  # Defensive against malformed event payloads (plotly sends "NA" as JSON
  # for empty events, which Shiny can't parse cleanly).
  selected_indices <- reactive({
    sel <- tryCatch(
      plotly::event_data("plotly_selected", source = "fp_corr_plot"),
      error = function(e) NULL
    )
    if (!is.null(sel) && is.data.frame(sel) && nrow(sel) > 0 &&
        "customdata" %in% names(sel)) {
      cd <- suppressWarnings(as.integer(sel$customdata))
      cd <- cd[!is.na(cd)]
      if (length(cd) > 0) return(unique(cd))
    }
    click <- tryCatch(
      plotly::event_data("plotly_click", source = "fp_corr_plot"),
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
  output$ui_fp_correction <- renderUI({
    df <- fp_f0_data()
    if (is.null(df) || nrow(df) == 0) {
      return(tags$div(style = "color: #888; font-style: italic;",
        "Run F0 Extraction first."))
    }
    tokens <- sort(unique(df$token))
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
                  choices = tokens, selected = tokens[1]),
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
          uiOutput("fp_corr_flagged_col_picker"),
          checkboxInput("fp_corr_only_flagged",
                        "Only show flagged tokens", value = FALSE)
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
                       title = "Smooth selected frame(s) with a centered window filter"),
          selectInput("fp_corr_smooth_method", NULL,
                      choices = c("median" = "median", "mean" = "mean"),
                      selected = "median", width = "90px"),
          tags$span(class = "fp-edit-meta", "window:"),
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
                       title = "Fit through the N nearest non-selected valid anchors and evaluate at the selected frame(s)"),
          selectInput("fp_corr_interp_method", NULL,
                      choices = c("linear"   = "linear",
                                  "parabola" = "parabola",
                                  "spline"   = "spline"),
                      selected = "linear", width = "100px"),
          tags$span(class = "fp-edit-meta", "window:"),
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
      tags$small(style = "color:#888; display:block; margin-top:4px;",
        "Files are named after the token or ", tags$code("all_correctedf0.csv"), ".")
    )
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
    selectInput("fp_corr_flagged_col",
                "Token column",
                choices = cols, selected = default, width = "100%")
  })

  # Extract unique flagged token IDs whenever the user (re-)picks the column.
  # If the CSV looks like Inspect output (has flagged_jump / time / flag_notes),
  # also build per-frame flag info so we can highlight individual points later.
  observeEvent(input$fp_corr_flagged_col, {
    df <- fp_flagged_raw()
    col <- input$fp_corr_flagged_col
    req(df, col, col %in% names(df))
    toks <- unique(as.character(df[[col]]))
    toks <- toks[!is.na(toks) & nzchar(toks)]
    fp_flagged_tokens(toks)

    # Per-frame flag info (Inspect-tab format detection)
    has_jump <- "flagged_jump" %in% names(df)
    has_time <- "time" %in% names(df) || any(grepl("^time$", names(df), ignore.case = TRUE))
    if (has_jump && has_time) {
      time_col <- if ("time" %in% names(df)) "time" else grep("^time$", names(df), ignore.case = TRUE, value = TRUE)[1]
      notes_col <- if ("flag_notes" %in% names(df)) "flag_notes" else NULL
      sub <- df[isTRUE(as.logical(df$flagged_jump)) | df$flagged_jump %in% c(TRUE, "TRUE", "true", 1L, "1"), , drop = FALSE]
      # Defensive: ensure boolean filter actually filters
      sub <- df[as.logical(df$flagged_jump) %in% TRUE, , drop = FALSE]
      sub <- sub[!is.na(sub[[col]]) & !is.na(sub[[time_col]]), , drop = FALSE]
      if (nrow(sub) > 0) {
        frames_by_token <- split(
          data.frame(
            time = as.numeric(sub[[time_col]]),
            note = if (!is.null(notes_col)) as.character(sub[[notes_col]]) else "",
            stringsAsFactors = FALSE
          ),
          as.character(sub[[col]])
        )
        fp_flagged_frames(frames_by_token)
        showNotification(sprintf("Found %d flagged frames across %d tokens.",
                                 nrow(sub), length(frames_by_token)),
                         type = "message", duration = 4)
      } else {
        fp_flagged_frames(NULL)
      }
    } else {
      fp_flagged_frames(NULL)
    }
  })

  # Filtered list of tokens to populate the dropdown.
  filtered_tokens <- reactive({
    df <- fp_f0_data()
    if (is.null(df)) return(character(0))
    all_t <- sort(unique(df$token))
    if (isTRUE(input$fp_corr_only_flagged)) {
      flagged <- fp_flagged_tokens()
      if (!is.null(flagged) && length(flagged) > 0) {
        return(intersect(all_t, flagged))
      }
    }
    all_t
  })

  # When the filtered set changes, update the dropdown choices in-place
  # (avoids re-rendering the whole sidebar and losing edit state).
  observe({
    toks <- filtered_tokens()
    cur <- isolate(input$fp_corr_token)
    sel <- if (!is.null(cur) && cur %in% toks) cur
           else if (length(toks) > 0) toks[1] else NULL
    if (length(toks) == 0) {
      updateSelectInput(session, "fp_corr_token",
                        choices = c("(no matching tokens)" = ""),
                        selected = "")
    } else {
      updateSelectInput(session, "fp_corr_token",
                        choices = toks, selected = sel)
    }
  })

  # Progress counter: current position + how many tokens have been edited
  output$fp_corr_progress <- renderText({
    toks <- filtered_tokens()
    if (length(toks) == 0) return("No tokens match the current filter.")
    idx <- match(input$fp_corr_token, toks)
    if (is.na(idx)) idx <- 0
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
    if (!is.null(frames_by_tok) && tok %in% names(frames_by_tok)) {
      fl <- frames_by_tok[[tok]]
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

    # Stack waveform (top, y2) and f0 (bottom, y) on a shared x-axis.
    # fixedrange = TRUE locks the y axes so modebar zoom acts only horizontally.
    layout_args <- list(
      xaxis = list(title = "time (s)", domain = c(0, 1)),
      yaxis = list(title = "f0 (Hz)", domain = c(0, 0.6),
                   fixedrange = TRUE),
      dragmode = "select"
    )
    if (!is.null(wav)) {
      layout_args$yaxis2 <- list(title = "waveform",
                                 domain = c(0.65, 1), anchor = "x",
                                 fixedrange = TRUE)
    }
    p <- do.call(plotly::layout, c(list(p), layout_args))
    plotly::config(p, displaylogo = FALSE)
  })

  # ---- Main panel layout ----
  output$fp_correction_guide <- renderUI({
    box_style <- "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;"
    df <- fp_f0_data()
    if (is.null(df) || nrow(df) == 0) {
      return(tagList(
        tags$div(style = box_style,
          tags$strong("F0 Correction"),
          tags$p(style = "margin: 6px 0 0 0;",
            "Run extraction in the ", tags$strong("F0 Extraction"),
            " tab first, then come back here to inspect and correct each token's contour."),
          tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
            tags$li("Octave jumps (e.g. doubling/halving on creaky voice)"),
            tags$li("Spurious frames outside the speaker's typical range"),
            tags$li("Misaligned voiced/unvoiced boundaries")
          )
        )
      ))
    }
    tagList(
      tags$div(style = box_style,
        tags$strong("F0 Correction"),
        tags$p(style = "margin: 6px 0 6px 0;",
          "Pick a token in the sidebar (or use ", HTML("&#9664;"), " / ",
          HTML("&#9654;"), " to step). ",
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
            " (recommended for f0) or ", tags$em("mean"),
            " filter; choose the window size (3 / 5 / 7)."),
          tags$li(tags$strong("Interpolate / Extrapolate:"),
            " replace selected frames by a ", tags$em("linear"), ", ",
            tags$em("parabola"), " (local quadratic) or ",
            tags$em("spline"), " (cubic) fit through the ",
            tags$em("window"), " nearest non-selected valid anchors. ",
            "If a selected frame is past the last valid anchor, the same fit ",
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
      plotly::plotlyOutput("fp_corr_plot", height = "560px"),
      tags$h4(style = "margin-top: 16px;", "Edit summary"),
      DT::dataTableOutput("fp_corr_edits_table")
    )
  })

  # Per-token edit count: how many frames differ from the original extraction
  output$fp_corr_edits_table <- DT::renderDataTable({
    corr <- fp_corrections()
    orig <- fp_f0_data()
    req(orig)
    tokens_all <- sort(unique(orig$token))
    rows <- lapply(tokens_all, function(tok) {
      orig_f0 <- orig$f0[orig$token == tok]
      if (tok %in% names(corr)) {
        new_f0 <- corr[[tok]]$f0
        n_len <- min(length(orig_f0), length(new_f0))
        # A frame "differs" if NA-status changes, or values differ
        diff_mask <- xor(is.na(orig_f0[seq_len(n_len)]), is.na(new_f0[seq_len(n_len)])) |
                     (!is.na(orig_f0[seq_len(n_len)]) & !is.na(new_f0[seq_len(n_len)]) &
                      orig_f0[seq_len(n_len)] != new_f0[seq_len(n_len)])
        n_diff <- sum(diff_mask)
      } else {
        n_diff <- 0L
      }
      data.frame(token = tok, edits = n_diff, stringsAsFactors = FALSE)
    })
    df <- do.call(rbind, rows)
    df <- df[order(-df$edits, df$token), , drop = FALSE]
    DT::datatable(
      df, rownames = FALSE,
      options = list(pageLength = 10, dom = "tip",
                     columnDefs = list(list(className = "dt-center", targets = "_all")))
    )
  })

  # ---- Edit handlers ----
  observeEvent(input$fp_corr_halve, {
    apply_edit(function(df, idx) { df$f0[idx] <- df$f0[idx] / 2; df },
               "Halve")
  })
  observeEvent(input$fp_corr_double, {
    apply_edit(function(df, idx) { df$f0[idx] <- df$f0[idx] * 2; df },
               "Double")
  })
  observeEvent(input$fp_corr_delete, {
    apply_edit(function(df, idx) { df$f0[idx] <- NA_real_; df },
               "Delete")
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
    }, sprintf("Interpolate (%s, window %s)", method,
               if (win == -1L) "all" else as.character(win)))
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
    }, sprintf("Smooth (%s, window %d)", method, win))
  })
  observeEvent(input$fp_corr_manual_apply, {
    v <- suppressWarnings(as.numeric(input$fp_corr_manual_value))
    if (is.na(v) || v <= 0) {
      showNotification("Enter a positive Hz value first.",
                       type = "warning", duration = 4)
      return()
    }
    apply_edit(function(df, idx) { df$f0[idx] <- v; df },
               sprintf("Set to %.2f Hz", v))
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
    }, sprintf("Pick candidate (%.2f Hz)", v))
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
      paste0(tok, "_correctedf0.csv")
    },
    content = function(file) {
      out <- build_corrected_df()
      req(out)
      tok <- input$fp_corr_token
      req(tok)
      sub <- out[out$token == tok, , drop = FALSE]
      fname <- paste0(tok, "_correctedf0.csv")
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
