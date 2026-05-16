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

      # ---- Token nav + progress ----
      div(style = "display:flex; gap:4px; align-items:center; margin-bottom: 6px;",
        actionButton("fp_corr_prev", HTML("&#9664;"), title = "Previous token"),
        actionButton("fp_corr_next", HTML("&#9654;"), title = "Next token")
      ),
      selectInput("fp_corr_token", "Token:",
                  choices = tokens, selected = tokens[1]),
      verbatimTextOutput("fp_corr_progress", placeholder = TRUE),
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

      # ---- Undo (separated, full-width) ----
      div(style = "margin-top: 12px;",
        actionButton("fp_corr_undo", "Undo last edit",
                     icon = icon("rotate-left"), width = "100%")
      ),

      tags$hr(),
      # Praat candidates (only when current token came from .Pitch)
      uiOutput("fp_corr_candidates_ui"),

      tags$hr(),
      h5("Download"),
      textInput("fp_corr_filename", "Filename:", value = "corrected_f0"),
      downloadButton("fp_corr_download", "Download corrected f0 (CSV)")
    )
  })

  # Prev / Next handlers (wrap around at ends)
  observeEvent(input$fp_corr_prev, {
    df <- fp_f0_data(); req(df)
    tokens <- sort(unique(df$token))
    idx <- match(input$fp_corr_token, tokens)
    new <- if (is.na(idx) || idx <= 1) length(tokens) else idx - 1
    updateSelectInput(session, "fp_corr_token", selected = tokens[new])
  })
  observeEvent(input$fp_corr_next, {
    df <- fp_f0_data(); req(df)
    tokens <- sort(unique(df$token))
    idx <- match(input$fp_corr_token, tokens)
    new <- if (is.na(idx) || idx >= length(tokens)) 1 else idx + 1
    updateSelectInput(session, "fp_corr_token", selected = tokens[new])
  })

  # Progress counter: current position + how many tokens have been edited
  output$fp_corr_progress <- renderText({
    df <- fp_f0_data(); req(df)
    tokens <- sort(unique(df$token))
    idx <- match(input$fp_corr_token, tokens)
    if (is.na(idx)) return("")
    n_edited <- length(fp_corrections())
    sprintf("Token %d / %d  ·  Edited: %d", idx, length(tokens), n_edited)
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

    # --- f0 marker colour / size with selected points highlighted ---
    point_colors <- rep("#5cb89a", nrow(f0_df))
    if (length(sel) > 0) point_colors[sel] <- "#e0712d"
    point_sizes  <- rep(7, nrow(f0_df))
    if (length(sel) > 0) point_sizes[sel] <- 12

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
      data = f0_df, x = ~time, y = ~f0,
      type = "scatter", mode = "markers+lines",
      yaxis = "y",
      marker = list(size = point_sizes, color = point_colors,
                    line = list(width = 1, color = "#2c5f4f")),
      line = list(color = "#5cb89a", width = 1),
      customdata = ~seq_len(nrow(f0_df)),
      hovertemplate = paste0(
        "frame %{customdata}<br>time: %{x:.3f}s<br>f0: %{y:.1f} Hz<extra></extra>"
      ),
      showlegend = FALSE
    )

    # Stack waveform (top, y2) and f0 (bottom, y) on a shared x-axis.
    layout_args <- list(
      xaxis = list(title = "time (s)", domain = c(0, 1)),
      yaxis = list(title = "f0 (Hz)", domain = c(0, 0.6)),
      dragmode = "select"
    )
    if (!is.null(wav)) {
      layout_args$yaxis2 <- list(title = "waveform",
                                 domain = c(0.65, 1), anchor = "x")
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

  # ---- Download corrected CSV ----
  output$fp_corr_download <- downloadHandler(
    filename = function() paste0(input$fp_corr_filename, ".csv"),
    content = function(file) {
      orig <- fp_f0_data()
      req(orig)
      corr <- fp_corrections()
      # Build a "corrected" column: corrected if edited, else original
      out <- orig
      out$f0_corrected <- out$f0
      if (length(corr) > 0) {
        for (tok in names(corr)) {
          mask <- out$token == tok
          # Align by row order within token (we kept row order in current_f0)
          out$f0_corrected[mask] <- corr[[tok]]$f0[seq_len(sum(mask))]
        }
      }
      fname <- paste0(input$fp_corr_filename, ".csv")
      write.csv(out, file, row.names = FALSE)
      showNotification(paste("Corrected f0 saved as", fname),
                       type = "message", duration = 4)
    }
  )
}
