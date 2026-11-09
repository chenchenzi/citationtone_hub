normalised_ui <- function(input, output, session, dataset, normalised_data,
                          curated_data = NULL, curated_norm_data = NULL) {

  # Data source: the upload, or (after Curate) the curated data. Normalising
  # the curated data writes curated_norm_data and leaves normalised_data alone:
  # Curate reads normalised_data, so writing curated data there would make
  # Curate apply its relabels a second time.
  # has_curated is a reactiveVal, which only invalidates when its value
  # changes, so a curation click does not re-render this panel.
  has_curated <- reactiveVal(FALSE)
  observe(has_curated(!is.null(curated_data) && !is.null(curated_data())))
  # Once curated data exists, the source starts on it (About page, path 5:
  # Curate, then Normalise). The user's switch to Uploaded data is kept until
  # the curation is reset.
  src_choice <- reactiveVal(NULL)
  observeEvent(input$norm_source, src_choice(input$norm_source))
  observeEvent(has_curated(), if (!has_curated()) src_choice(NULL))
  norm_source <- reactive(
    if (!has_curated()) "uploaded" else if (is.null(src_choice())) "curated" else src_choice())
  norm_input  <- reactive(if (norm_source() == "curated") curated_data() else dataset())
  # Columns this tab writes: never offered as the f0 (Hz) default, and refused
  # as the f0 to normalise.
  norm_cols <- c("f0_st", "f0_zscore", "f0_lz", "speaker_mean")
  # Source and dataset the panel was last drawn for: while they are unchanged,
  # a re-render (e.g. after a curation change) keeps the user's choices.
  drawn_for <- NULL

  # Guide text for the Normalise tab
  output$normalise_guide <- renderUI({
    tagList(
      guide_box("Normalisation guide",
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(nows(tags$strong("Data source:"), " The uploaded data, or, after Curate, the ",
                  tags$strong("Curated data"), ". Normalising that again recomputes each speaker\u2019s mean (and SD) on the kept tokens and your new tone labels, and creates ",
                  tags$strong("Curated + normalised data"), " for Visualise, the modelling tabs and Summarise. The curated data itself is not changed.")),
          tags$li(tags$strong("f0 (Hz):"), " The raw fundamental frequency column in Hertz."),
          tags$li(tags$strong("Speaker:"), " A speaker ID column for by-speaker normalisation."),
          tags$li(tags$strong("Tone category:"), " The column labelling tone types.")
        ),
        tags$strong("Speaker mean methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Simple average:"), " Mean (and, for z-scores, SD) over all data points for each speaker, as R\u2019s mean() and sd(). The classic z-score and LZ formulas."),
          tags$li(tags$strong(HTML("Equally weighted by tone &#128077;:")), " Each tone counts once, however many tokens it has: the mean of the per-tone means, and for z-scores an SD weighted the same way, as R\u2019s cov.wt(). Same as simple when every tone has the same number of data points.")
        ),
        tags$strong("F0 normalisation methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Semitone:"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "ST = 12 \u00d7 log\u2082(f0 / \u03bc)"), " Converts Hz to a perceptually uniform scale referenced on each speaker's mean. Keeps each speaker's own pitch range."),
          tags$li(tags$strong("Z-score:"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "z = (f0 \u2212 \u03bc) / \u03c3"), " Centres f0 on 0 and scales by speaker variability, in Hz."),
          tags$li(tags$strong("Log z-score (LZ):"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "LZ = (log\u2081\u2080 f0 \u2212 \u03bc) / \u03c3"), " A z-score of log\u2081\u2080 f0 (\u03bc, \u03c3 taken on log\u2081\u2080 f0): like semitones, but also evens out speakers' pitch ranges. Common in Chinese tone studies (Zhu 1999).")
        ),
        tags$strong("Bessel\u2019s correction (z-score and LZ only):"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Corrected (default):"), " The sample SD, dividing by n \u2212 1, as R\u2019s sd() and the published formulas do (cov.wt(method = \"unbiased\") when equally weighted)."),
          tags$li(tags$strong("Uncorrected:"), " The population SD, dividing by n (cov.wt(method = \"ML\") when equally weighted). The difference is negligible unless a tone has only a few data points.")
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
    ds   <- norm_input()
    src  <- norm_source()
    vars <- if (!is.null(ds)) names(ds) else c("No dataset available")
    data_types <- if (!is.null(ds)) sapply(ds, class) else rep("NA", length(vars))
    var_types  <- paste0(vars, " {", data_types, "}")
    key <- paste(src, input$dataset_name)
    same <- identical(drawn_for, key); drawn_for <<- key
    # Re-renders keep the method choices, and the variables and filename while
    # the source and dataset are unchanged.
    keep <- function(id, default) { v <- isolate(input[[id]]); if (is.null(v)) default else v }
    keep_var <- function(id, default) {
      v <- isolate(input[[id]]); if (same && !is.null(v) && v %in% vars) v else default }
    # On curated data, default to the speaker and tone variables used in Curate,
    # so the tone weighting follows the curated labels (e.g. `cluster`).
    cv <- if (src == "curated") attr(ds, "curate_vars") else NULL
    pick <- function(v, patterns, idx) if (!is.null(v) && v %in% vars) v else guess_var(vars, patterns, idx)
    hz_vars <- setdiff(vars, norm_cols); if (length(hz_vars) == 0) hz_vars <- vars
    fname <- paste0(if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                      input$dataset_name else "data",
                    if (src == "curated") "_curated_normalised" else "_normalised")

    tagList(
      wellPanel(
        h5("Dataset",
           tags$small(style = "color: #777; margin-left: 6px; font-weight: 400;",
                      input$dataset_name)),
        if (has_curated())
          selectInput("norm_source", "Data source:",
                      choices = c("Uploaded data" = "uploaded", "Curated data" = "curated"),
                      selected = src),
        if (src == "curated")
          tags$div(style = "font-size: 0.75rem; color: #2a7a5a; margin: -4px 0 8px 0;",
                   icon("rotate"), " Normalising again on the kept tokens and new labels. ",
                   "Creates Curated + normalised data; the curated data stays as it is."),
        # IDs prefixed norm_: Visualise has its own tone_var / speaker_var.
        selectInput("norm_f0_var", "Select f0 (Hz) variable:",
                    choices = setNames(vars, var_types),
                    # Hz first: a re-uploaded normalised file also has f0_st / f0_lz.
                    selected = keep_var("norm_f0_var",
                                        guess_var(hz_vars, c("^f0_hz$", "^f0$", var_patterns$f0), 1)),
                    multiple = FALSE),
        selectInput("norm_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = keep_var("norm_speaker_var", pick(cv$speaker, var_patterns$speaker, 2)),
                    multiple = FALSE),
        selectInput("norm_tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = keep_var("norm_tone_var", pick(cv$tone, var_patterns$tone, 3)),
                    multiple = FALSE),
        tags$hr(),
        radioButtons("mean_calc_method", "Speaker Mean f0 Options",
                     choices = list("Simple average" = "simple",
                                    "Equally weighted by each tone" = "weighted"),
                     selected = keep("mean_calc_method", "weighted")),
        tags$hr(),
        radioButtons("normalisation_method", "F0 Normalisation Options",
                     choices = list("Semitone referenced on speaker mean" = "semitone",
                                    "By-speaker Z-score" = "zscore",
                                    "By-speaker log Z-score (LZ)" = "lz"),
                     selected = keep("normalisation_method", "semitone")),
        # Semitones use no SD, so the correction only shows for the z-scores.
        conditionalPanel("input.normalisation_method != 'semitone'",
          checkboxInput("sd_correction", "Bessel\u2019s correction for the SD (n \u2212 1)",
                        value = keep("sd_correction", TRUE))),
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
                  value = if (same) keep("output_filename", fname) else fname),
        downloadButton("download_data", "Download Normalised Data")
      )
    )
  })

  # Optional landmark time-normalisation controls. Always shown (an italic hint
  # stands in when the dataset carries no landmark columns), so it reads as an
  # optional step alongside the f0 options.
  output$norm_time_landmark_ui <- renderUI({
    ds   <- norm_input()
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
    # Keep a choice across re-renders (e.g. a curation change) while valid.
    keep_in <- function(id, valid, default) {
      v <- isolate(input[[id]]); if (!is.null(v) && v %in% valid) v else default }
    tagList(
      selectInput("norm_time_set", "Normalise time by:",
                  choices = choices, selected = keep_in("norm_time_set", choices, "__none__")),
      conditionalPanel(
        "input.norm_time_set && input.norm_time_set != '__none__'",
        selectInput("norm_time_var", "Time variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = keep_in("norm_time_var", vars, guess_var(vars, var_patterns$time, 1)))),
      conditionalPanel(
        "input.norm_time_set == '__token__'",
        selectInput("norm_time_token_var", "Token ID variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = keep_in("norm_time_token_var", vars, if (!is.null(tok)) tok else vars[1]))),
      # Tiers are listed only when the data carries their landmark columns;
      # say why none are, rather than leave a tier-less menu unexplained.
      if (length(sets) == 0)
        tags$div(style = "color: #999; font-size: 0.75rem; font-style: italic; margin-top: -6px;",
          HTML(paste(
            "No landmark columns (<code>&lt;tier&gt;_start</code>, <code>&lt;tier&gt;_end</code>)",
            "in this dataset, so no tier is listed. For multisyllabic words, tick the",
            "<code>syllable</code> tier under <strong>Landmarks from TextGrid</strong> in",
            "F0 Extraction and export at native frame times.")))
    )
  })

  # Store normalisation result
  norm_display <- reactiveVal(NULL)

  # Compute normalisation on button click
  observeEvent(input$normalise_button, {
    req(norm_input())
    src  <- norm_source()
    data <- norm_input()
    src_obj  <- data
    src_vars <- attr(data, "curate_vars")
    if (isTRUE(input$norm_f0_var %in% norm_cols)) {
      showNotification(sprintf(paste("%s is a normalised column. Choose the f0 column in Hz",
                                     "(normalising is always done from Hz)."), input$norm_f0_var),
                       type = "error", duration = 8)
      if (src == "curated") norm_display(NULL)
      return()
    }

    # --- f0 normalisation (R/normalise.R; scriptable + unit-tested) ----------
    # Wrapped so a failure (e.g. no usable speaker/tone column on raw extracted
    # data) still lets the optional time normalisation below run.
    f0_ok <- FALSE
    if (!is.null(input$norm_f0_var) && !is.null(input$norm_speaker_var) &&
        all(c(input$norm_f0_var, input$norm_speaker_var) %in% names(data))) {
      data <- tryCatch({
        d <- normalise_f0(
          data,
          f0          = input$norm_f0_var,
          speaker     = input$norm_speaker_var,
          tone        = input$norm_tone_var,
          method      = input$normalisation_method,
          mean_method = input$mean_calc_method,
          sd_correction = !isFALSE(input$sd_correction)
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

    norm_col <- switch(input$normalisation_method,
                       zscore = "f0_zscore", lz = "f0_lz", "f0_st")

    # Store the full dataset with normalised columns (for other tabs)
    if (src == "curated") {
      if (!f0_ok) {
        showNotification(paste("The curated data was not normalised, so no curated +",
                               "normalised data was made. Check the f0, speaker and tone variables."),
                         type = "error", duration = 8)
        norm_display(NULL)
        return()
      }
      # Normalised columns carried over from the first run (all tokens, the
      # original labels) are out of date: keep only the one this run produced.
      stale <- setdiff(c("f0_st", "f0_zscore", "f0_lz"), norm_col)
      data  <- data[, setdiff(names(data), stale), drop = FALSE]
      # The variables this run used (downstream menus default to them), the
      # run (source_picker() selects each new run once), and a fingerprint of
      # the curated data it came from (server.R clears this data when it changes).
      used <- c(src_vars, list(tone = input$norm_tone_var, speaker = input$norm_speaker_var))
      attr(data, "curate_vars") <- used[!duplicated(names(used), fromLast = TRUE)]
      attr(data, "run") <- input$normalise_button
      key_cols <- unique(c(src_vars$token, input$norm_f0_var, input$norm_speaker_var, input$norm_tone_var))
      attr(data, "curated_key_cols") <- key_cols
      attr(data, "curated_key") <- curated_key(src_obj, key_cols)
      curated_norm_data(data)
      tok <- vis_token_col(names(data))
      showNotification(sprintf(paste("Curated + normalised data ready%s. Model, GCA and GAMM",
                                     "switch to it; Visualise and Summarise list it."),
                               if (!is.null(tok)) sprintf(": %d tokens", length(unique(data[[tok]]))) else ""),
                       type = "message", duration = 6)
    } else {
      normalised_data(data)
    }

    # Display subset: f0 columns (when produced) + any time-normalisation columns.
    keep <- c(input$norm_f0_var, input$norm_speaker_var, input$norm_tone_var,
              if (f0_ok) c("speaker_mean", norm_col),
              if (length(time_cols) > 0) c(time_ctx, time_cols))
    keep <- unique(intersect(keep, names(data)))
    norm_display(if (length(keep) > 0) data[, keep, drop = FALSE] else data)
    norm_route(src)
  })

  # The table and download show the last result: drop it when the source is
  # switched, or when the curated + normalised data it showed is cleared.
  norm_route <- reactiveVal(NULL)
  observeEvent(input$uploadfile, norm_display(NULL), ignoreInit = TRUE)
  observeEvent(norm_source(), norm_display(NULL), ignoreInit = TRUE)
  if (!is.null(curated_norm_data))
    observeEvent(curated_norm_data(), {
      if (is.null(curated_norm_data()) && identical(norm_route(), "curated")) norm_display(NULL)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

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
