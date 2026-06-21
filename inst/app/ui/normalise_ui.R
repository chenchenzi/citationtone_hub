normalised_ui <- function(input, output, session, dataset, normalised_data) {

  # Guide text for the Normalise tab
  output$normalise_guide <- renderUI({
    tagList(
      guide_box("Normalisation guide",
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("f0 (Hz):"), " The raw fundamental frequency column in Hertz."),
          tags$li(tags$strong("Speaker:"), " A speaker ID column for by-speaker normalisation."),
          tags$li(tags$strong("Tone category:"), " The column labelling tone types.")
        ),
        tags$strong("Speaker mean methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Simple average:"), " Mean f0 across all data points for each speaker. Fast and straightforward."),
          tags$li(tags$strong(HTML("Equally weighted by tone &#128077;:")), " Compute per-tone means first, then average those. This better estimates the centre of a speaker's tonal space, giving each tone equal weight.")
        ),
        tags$strong("F0 normalisation methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Z-score:"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "z = (f0 \u2212 \u03bc) / \u03c3"), " Centres f0 on 0 and scales by speaker variability."),
          tags$li(tags$strong("Semitone:"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "ST = 12 \u00d7 log\u2082(f0 / \u03bc)"), " Converts Hz to a perceptually uniform scale referenced on each speaker's mean.")
        )
      ),
        # --- Collapsible illustrated guide, shown below the green guide ---
        tags$details(class = "msg-route",
          tags$style(HTML("
            details.msg-route{background:#f3f8fc;border:1px solid #cfe2f1;border-radius:8px;padding:7px 14px 11px;margin:6px 0 0;}
            .msg-route>summary{cursor:pointer;font-weight:700;color:#2c5d80;font-size:0.9rem;list-style:none;padding:1px 0;}
            .msg-route>summary::-webkit-details-marker{display:none;}
            .msg-route>summary::before{content:'\\25B8';color:#5b9bd5;display:inline-block;margin-right:8px;transition:transform .15s ease;}
            .msg-route[open]>summary::before{transform:rotate(90deg);}
            .msg-hint{color:#7aa6cc;font-weight:400;font-size:0.78rem;margin-left:6px;}
            .msg-route[open] .msg-hint{display:none;}
            .msg-intro{color:#3f5a72;font-size:0.83rem;line-height:1.5;margin:9px 0 0;}
            .msg-illus{margin:11px 0 3px;}
            .msg-illus svg{width:100%;height:auto;display:block;}
            .msg-opts{display:flex;gap:9px;margin-top:11px;flex-wrap:wrap;}
            .msg-step{flex:1 1 165px;background:#fff;border:1px solid #e1e9f2;border-radius:7px;padding:8px 12px;}
            .msg-stitle{font-weight:700;color:#2c5f4f;font-size:0.86rem;}
            .msg-swhy{font-size:0.76rem;color:#5f6b66;line-height:1.4;margin-top:2px;}
            .msg-tab{display:inline-block;background:#e8f5f0;color:#2c5f4f;padding:1px 7px;border-radius:10px;font-size:0.72rem;font-weight:600;white-space:nowrap;}
            .msg-tip{font-size:0.78rem;color:#33536f;background:#eaf3fb;border:1px solid #d3e6f5;border-radius:6px;padding:6px 11px;margin-top:11px;line-height:1.55;}
            .msg-tip .fa,.msg-tip svg{color:#5b9bd5;margin-right:4px;}
          ")),
          tags$summary(icon("ruler-horizontal"), " Multisyllabic or sub-syllabic data?",
                       tags$span(class = "msg-hint", "(click to expand)")),
          tags$p(class = "msg-intro",
            HTML('Add a normalised-time column so contours (e.g. <span style="color:#3a7ca5;font-weight:700;">A</span> / <span style="color:#2f9e79;font-weight:700;">B</span>) share a common axis. Match your data to one of the cases below.')),
          tags$div(class = "msg-illus", HTML('<svg width="100%" viewBox="0 0 680 150" role="img" xmlns="http://www.w3.org/2000/svg"><title>Single 0 to 1 axis versus multisyllabic _tseq and _t01</title><desc>A single 0 to 1 axis serves a whole monosyllable or one region; a multisyllabic word gives _tseq in sequence and _t01 with each syllable in its own 0 to 1 box.</desc><text x="140" y="16" font-size="12" font-weight="700" fill="#2c5d80" text-anchor="middle">Single 0&#8211;1 axis</text><text x="440" y="16" font-size="12" font-weight="700" fill="#2c5d80" text-anchor="middle">Multisyllabic word</text><line x1="240" y1="9" x2="240" y2="132" stroke="#d9e6f1" stroke-width="1"/><text x="140" y="36" font-size="12" font-weight="600" fill="#48667e" text-anchor="middle">Whole token / region</text><text x="340" y="36" font-size="12" font-weight="600" fill="#48667e" text-anchor="middle">&lt;tier&gt;_tseq</text><text x="540" y="36" font-size="12" font-weight="600" fill="#48667e" text-anchor="middle">&lt;tier&gt;_t01</text><rect x="60" y="46" width="160" height="62" fill="none" stroke="#b8d2e8" stroke-width="1"/><polyline points="60,96 100,70 140,62 180,72 220,96" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="60,100 100,76 140,68 180,78 220,100" fill="none" stroke="#2f9e79" stroke-width="2.5"/><line x1="60" y1="108" x2="60" y2="112" stroke="#b8d2e8"/><line x1="220" y1="108" x2="220" y2="112" stroke="#b8d2e8"/><text x="60" y="122" font-size="10" fill="#97a4ac" text-anchor="start">0</text><text x="220" y="122" font-size="10" fill="#97a4ac" text-anchor="end">1</text><text x="140" y="140" font-size="11" fill="#5f6b66" text-anchor="middle">monosyllabic, or one region</text><rect x="260" y="46" width="160" height="62" fill="none" stroke="#b8d2e8" stroke-width="1"/><polyline points="260,96 300,66 340,58 380,84 420,100" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="260,100 300,72 340,64 380,88 420,102" fill="none" stroke="#2f9e79" stroke-width="2.5"/><line x1="340" y1="46" x2="340" y2="108" stroke="#9fbbd6" stroke-width="1.5" stroke-dasharray="4,3"/><line x1="260" y1="108" x2="260" y2="112" stroke="#b8d2e8"/><line x1="340" y1="108" x2="340" y2="112" stroke="#b8d2e8"/><line x1="420" y1="108" x2="420" y2="112" stroke="#b8d2e8"/><text x="260" y="122" font-size="10" fill="#97a4ac" text-anchor="start">0</text><text x="340" y="122" font-size="10" fill="#97a4ac" text-anchor="middle">1</text><text x="420" y="122" font-size="10" fill="#97a4ac" text-anchor="end">2</text><text x="340" y="140" font-size="11" fill="#5f6b66" text-anchor="middle">syllables in sequence</text><rect x="460" y="46" width="75" height="62" fill="none" stroke="#b8d2e8" stroke-width="1"/><rect x="545" y="46" width="75" height="62" fill="none" stroke="#b8d2e8" stroke-width="1"/><polyline points="460,94 485,76 510,64 535,58" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="460,98 485,80 510,68 535,62" fill="none" stroke="#2f9e79" stroke-width="2.5"/><polyline points="545,58 570,74 595,90 620,100" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="545,62 570,78 595,93 620,102" fill="none" stroke="#2f9e79" stroke-width="2.5"/><line x1="460" y1="108" x2="460" y2="112" stroke="#b8d2e8"/><line x1="535" y1="108" x2="535" y2="112" stroke="#b8d2e8"/><line x1="545" y1="108" x2="545" y2="112" stroke="#b8d2e8"/><line x1="620" y1="108" x2="620" y2="112" stroke="#b8d2e8"/><text x="460" y="122" font-size="10" fill="#97a4ac" text-anchor="start">0</text><text x="535" y="122" font-size="10" fill="#97a4ac" text-anchor="end">1</text><text x="545" y="122" font-size="10" fill="#97a4ac" text-anchor="start">0</text><text x="620" y="122" font-size="10" fill="#97a4ac" text-anchor="end">1</text><text x="497" y="140" font-size="11" fill="#5f6b66" text-anchor="middle">&#963;1</text><text x="582" y="140" font-size="11" fill="#5f6b66" text-anchor="middle">&#963;2</text></svg>')),
          tags$div(class = "msg-opts",
            tags$div(class = "msg-step", style = "flex:1 1 190px;",
              tags$div(class = "msg-stitle", "Monosyllabic word, or a sub-syllabic region"),
              tags$div(class = "msg-swhy",
                HTML("Pick <code>Whole token (0&ndash;1)</code> for a whole syllable, or a <code>rhyme</code> / <code>vowel</code> tier to normalise just that region. Either gives the single 0&ndash;1 axis (left)."))),
            tags$div(class = "msg-step", style = "flex:1.7 1 300px;",
              tags$div(class = "msg-stitle", "Multisyllabic word"),
              tags$div(class = "msg-swhy",
                HTML("Pick the <code>syllable</code> tier. <code>syllable_tseq</code> lays syllables end to end (one word axis); <code>syllable_t01</code> puts each syllable in its own 0&ndash;1 box, to overlay and compare.")))
          ),
          tags$div(class = "msg-tip",
            icon("lightbulb"),
            HTML(' Use the new column as the X axis in <span class="msg-tab">Visualise</span> or the Time variable in the <span class="msg-tab">Model</span>. Frames <strong>outside the chosen region</strong> (or before the first landmark) are <code>NA</code> on this axis, so they drop out of any downstream plot, clustering, or model that uses it.'))
        )
    )
  })

  # Render UI for selecting f0, speaker, and tone variables
  output$ui_normalise <- renderUI({
    ds   <- dataset()
    vars <- if (!is.null(ds)) names(ds) else c("No dataset available")
    data_types <- if (!is.null(ds)) sapply(ds, class) else rep("NA", length(vars))
    var_types  <- paste0(vars, " {", data_types, "}")

    tagList(
      wellPanel(
        h5("Dataset",
           tags$small(style = "color: #777; margin-left: 6px; font-weight: 400;",
                      input$dataset_name)),
        selectInput("f0_var", "Select f0 (Hz) variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$f0, 1), multiple = FALSE),
        selectInput("speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$speaker, 2), multiple = FALSE),
        selectInput("tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$tone, 3), multiple = FALSE),
        tags$hr(),
        radioButtons("mean_calc_method", "Speaker Mean f0 Options",
                     choices = list("Simple average" = "simple",
                                    "Equally weighted by each tone" = "weighted"),
                     selected = "weighted"),
        tags$hr(),
        radioButtons("normalisation_method", "F0 Normalisation Options",
                     choices = list("Semitone referenced on speaker mean" = "semitone",
                                    "By-speaker Z-score" = "zscore"),
                     selected = "semitone"),
        tags$hr(),
        # Pointer to the illustrated time-normalisation guide on the right.
        tags$div(style = "color:#888; font-size:0.72rem; margin:0 0 4px;",
                 "Multisyllabic or sub-syllabic data? See the illustrated guide on the right for which option to pick."),
        h5("Time Normalisation options ",
           tags$span(style = "font-weight:400; color:#888; font-size:0.8rem;", "(optional)")),
        uiOutput("norm_time_landmark_ui"),
        tags$hr(),
        actionButton("normalise_button", "Normalise"),
        tags$hr(),
        h5("Download"),
        textInput("output_filename", "Enter filename (without extension):",
                  value = if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                            paste0(input$dataset_name, "_normalised")
                          else "normalised_data"),
        downloadButton("download_data", "Download Normalised Data")
      )
    )
  })

  # Optional landmark time-normalisation controls. Always shown (an italic hint
  # stands in when the dataset carries no landmark columns), so it reads as an
  # optional step alongside the f0 options.
  output$norm_time_landmark_ui <- renderUI({
    ds   <- dataset()
    vars <- if (!is.null(ds)) names(ds) else character(0)
    sets <- landmark_sets(vars)
    tok  <- vis_token_col(vars)   # token column enables the whole-token 0-1 option

    choices <- c("(none)" = "__none__")
    if (!is.null(tok))  choices <- c(choices, "Whole token (0–1)" = "__token__")
    if (length(sets))   choices <- c(choices, stats::setNames(names(sets), names(sets)))

    if (length(choices) == 1) {   # only "(none)": no token and no landmarks
      return(tags$div(style = "color: #999; font-size: 0.78rem; font-style: italic;",
        "No token or landmark columns detected. Add landmark columns in F0 Extraction (choose TextGrid tiers) to normalise time within segments."))
    }
    var_types <- paste0(vars, " {", sapply(ds, class), "}")
    tagList(
      selectInput("norm_time_set", "Normalise time by:",
                  choices = choices, selected = "__none__"),
      conditionalPanel(
        "input.norm_time_set && input.norm_time_set != '__none__'",
        selectInput("norm_time_var", "Time variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$time, 1))),
      conditionalPanel(
        "input.norm_time_set == '__token__'",
        selectInput("norm_time_token_var", "Token ID variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = if (!is.null(tok)) tok else vars[1]))
    )
  })

  # Store normalisation result
  norm_display <- reactiveVal(NULL)

  # Compute normalisation on button click
  observeEvent(input$normalise_button, {
    req(dataset())
    data <- dataset()

    # --- f0 normalisation (R/normalise.R; scriptable + unit-tested) ----------
    # Wrapped so a failure (e.g. no usable speaker/tone column on raw extracted
    # data) still lets the optional time normalisation below run.
    f0_ok <- FALSE
    if (!is.null(input$f0_var) && !is.null(input$speaker_var) &&
        all(c(input$f0_var, input$speaker_var) %in% names(data))) {
      data <- tryCatch({
        d <- normalise_f0(
          data,
          f0          = input$f0_var,
          speaker     = input$speaker_var,
          tone        = input$tone_var,
          method      = input$normalisation_method,
          mean_method = input$mean_calc_method
        )
        f0_ok <- TRUE
        d
      }, error = function(e) {
        showNotification(paste("f0 normalisation skipped:", conditionMessage(e)),
                         type = "warning", duration = 6)
        data
      })
    }

    # --- optional time normalisation: whole token (0-1) or per landmark ------
    set <- input$norm_time_set
    time_cols <- character(0); time_ctx <- character(0)
    if (!is.null(set) && nzchar(set) && set != "__none__") {
      tvar <- input$norm_time_var
      if (is.null(tvar) || !nzchar(tvar)) tvar <- guess_var(names(data), var_patterns$time, 1)
      before <- names(data)
      if (identical(set, "__token__")) {
        tok <- input$norm_time_token_var
        if (is.null(tok) || !nzchar(tok)) tok <- vis_token_col(names(data))
        if (!is.null(tok)) { data <- normalise_time_token(data, tvar, tok); time_ctx <- tok }
      } else {
        data <- normalise_time_landmarks(data, tvar, set)
        time_ctx <- paste0(set, "_i")
      }
      time_cols <- setdiff(names(data), before)
      if (length(time_cols) > 0)
        showNotification(sprintf("Added time column(s): %s.",
                                 paste(time_cols, collapse = ", ")),
                         type = "message", duration = 5)
    }

    # Store the full dataset with normalised columns (for other tabs)
    normalised_data(data)

    # Display subset: f0 columns (when produced) + any time-normalisation columns.
    norm_col <- if (input$normalisation_method == "zscore") "f0_zscore" else "f0_st"
    keep <- c(input$f0_var, input$speaker_var, input$tone_var,
              if (f0_ok) c("speaker_mean", norm_col),
              if (length(time_cols) > 0) c(time_ctx, time_cols))
    keep <- unique(intersect(keep, names(data)))
    norm_display(if (length(keep) > 0) data[, keep, drop = FALSE] else data)
  })

  # Display the normalised dataset
  output$normalised_data <- DT::renderDataTable({
    req(norm_display())
    DT::datatable(norm_display())
  })

  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$output_filename, ".csv")
    },
    content = function(file) {
      req(norm_display())
      fname <- paste0(input$output_filename, ".csv")
      write.csv(norm_display(), file, row.names = FALSE)
      showNotification(paste("Data saved as", fname), type = "message", duration = 4)
    }
  )
}
