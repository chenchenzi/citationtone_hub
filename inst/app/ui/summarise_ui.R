###############################################
# Summarise tab — Chao Tone Digit Conversion
###############################################

summarise_ui <- function(input, output, session, dataset, normalised_data, gca_pred_data, gamm_pred_data, curated_data = NULL,
                         curated_norm_data = NULL) {

  # classify_contour() and compute_mean_contour() are now package-level
  # functions (R/chao.R), available in the global namespace via global.R.
  # The HTML-highlighted scaled-value formatter below stays here because
  # it's UI-specific.

  # --- UI helper: highlight scaled values within `tol` of a reference set ---
  # `dropped`: index of a sampled turning point whose digit is left out of the
  # numeral (it rounds to the same digit as an endpoint); shown in grey brackets.
  format_scaled_values_html <- function(vals, refs, tol = 0.1, dropped = integer(0)) {
    pieces <- vapply(vals, function(v) {
      val_str <- sprintf("%.2f", v)
      if (any(abs(v - refs) < tol, na.rm = TRUE)) {
        paste0('<span style="color: #d9534f; font-weight: bold;" title="within ±',
               tol, ' of reference">', val_str, "</span>")
      } else {
        val_str
      }
    }, character(1))
    for (j in dropped)
      pieces[j] <- paste0('<span style="color: #999;" title="Turning point: same digit as an ',
                          'endpoint, so not in the numeral">(', pieces[j], ')</span>')
    paste(pieces, collapse = " → ")
  }

  # --- Speaker column for the LZ step on raw f0: the first name that looks
  # like a speaker ID, or ".none" to treat the data as one speaker ---
  guess_speaker <- function(vars) {
    for (pat in var_patterns$speaker) {
      m <- grep(pat, vars, ignore.case = TRUE, value = TRUE)
      if (length(m) > 0) return(m[1])
    }
    ".none"
  }

  # --- Guide text ---
  output$summarise_guide <- renderUI({
    code_style <- "color: #2c5f4f; background: #dcefe7; padding: 1px 3px; border-radius: 3px;"
    # Digit-scheme diagram in the app's palette: the same colour for digits in
    # both schemes (the dark green of headings), boundaries in the gold the
    # app uses for flags and hints, and everything else muted.
    digit <- function(x) sprintf("<b style='color:#2c5f4f;'>%s</b>", x)
    edge  <- function(x) sprintf("<b style='color:#e0a800;'>%s</b>", x)
    title <- function(x) sprintf("<b style='color:#2c5f4f;'>%s</b>", x)
    # Interval digits are bands: each sits in a light mint box between two
    # boundaries.
    band  <- function(x) sprintf("<span style='background:#e8f5f0; border-radius:3px;'>%s</span>", x)
    code  <- function(x) tags$code(style = code_style, x)
    note_style <- "margin: 4px 0 8px 0; font-size: 0.84rem; color: #777;"
    rec <- tags$span(style = "color: #2c8a63; margin: 0 1px 0 4px;", title = "Recommended", icon("thumbs-up"),
                     .noWS = "inside")
    tagList(
      guide_box("Chao tone numeral guide",
        tags$p(style = "margin-bottom: 8px;", nows(
          "Chao numerals describe a tone on a 5-level scale (1 = lowest, 5 = highest): Mandarin T1 ", code("55"),
          ", T2 ", code("35"), ", T3 ", code("214"), ", T4 ", code("51"), ". Each is also shown in IPA tone letters, e.g. ",
          code("214"), " = ", tags$span(class = "tone-letters", "\u02e8\u02e9\u02e6"), "."
        )),
        tags$p(style = "margin-bottom: 8px;", nows(
          "Both methods use ", tags$strong("Fraction of Range (FOR)"),
          ": f0 is placed between min and max, the lowest and highest points of the tones\u2019 mean (or model-predicted) contours. ",
          "They differ only in how values become digits:"
        )),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(
            tags$strong("Reference-line FOR [1, 5]:"),
            tags$div(style = "margin: 4px 0 2px 0; font-family: 'Georgia', serif; font-style: italic; font-size: 0.92rem;",
              HTML("f\u2080\u2032 = (f\u2080 \u2212 f\u2080<sub>min</sub>) / (f\u2080<sub>max</sub> \u2212 f\u2080<sub>min</sub>) \u00d7 4 + 1")
            ),
            nows(" Each value is rounded to the nearest of 5 lines (", code("round()"), ").")
          ),
          tags$li(
            tags$strong("Interval-based FOR (0, 5]:"),
            tags$div(style = "margin: 4px 0 2px 0; font-family: 'Georgia', serif; font-style: italic; font-size: 0.92rem;",
              HTML("f\u2080\u2032 = (f\u2080 \u2212 f\u2080<sub>min</sub>) / (f\u2080<sub>max</sub> \u2212 f\u2080<sub>min</sub>) \u00d7 5")
            ),
            nows(" Each value takes the number of its band, 1 to 5 (", code("ceiling()"), ").")
          )
        ),
        tags$pre(style = "font-size: 0.78rem; line-height: 1.4; color: #6b7d75; background: #fff; border: 1px solid #d6e7df; border-radius: 4px; padding: 8px 10px; margin: 2px 0 8px 0; overflow-x: auto;",
          HTML(paste(c(
            paste0(title("Reference-line, round()"), ": digits are the lines, boundaries half-way"),
            paste0("  line:   ", paste(1:5, collapse = "       ")),
            paste0("  digit:  ", paste(digit(1:5), collapse = paste0("   ", edge("|"), "   "))),
            paste0("             ", paste(edge(c("1.5", "2.5", "3.5", "4.5")), collapse = "     ")),
            "",
            paste0(title("Interval, ceiling()"), ": digits are the bands, boundaries at integers"),
            paste0("  value:  ", paste(c(0, edge(1:4), 5), collapse = "       ")),
            paste0("  digit:  |", paste0(band(paste0("   ", digit(1:5), "   ")), c(rep(edge("|"), 4), "|"), collapse = ""))
          ), collapse = "\n"))),
        tags$p(style = note_style,
          HTML(paste("Values near a boundary (<span style='color:#e0a800; font-weight:bold;'>|</span>) are ambiguous: 2.45 could be 2 or 3.",
                     "Those within the fuzzy boundary threshold (\u00b1<em>t</em>) are in <span style='color:#d9534f; font-weight:bold;'>red</span>;",
                     "see Xu &amp; Zhang (2024)."))
        ),
        tags$p(style = note_style,
          HTML(paste("<strong>Turning points:</strong> a dip or peak (between 10% and 90% of the tone) that goes more than the",
                     "<em>turning-point threshold</em> (default 0.5) beyond both endpoints adds a middle digit, e.g. 214 rather than 24,",
                     "if that digit is lower (dip) or higher (peak) than both endpoint digits. Otherwise the numeral keeps two digits",
                     "(23, not 223). <em>Dip / peak</em> sizes near the threshold are in <span style='color:#d9534f; font-weight:bold;'>red</span>."))
        ),
        tags$strong("Data source:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(nows(tags$strong("GCA / GAMM predictions"), rec, tags$strong(":"),
                  " the fitted model\u2019s population-level contours, smooth and representative. Available after fitting a model.")),
          tags$li(nows(tags$strong("Normalised or Curated + normalised f0"), rec, tags$strong(":"),
                  " mean contours on the scale chosen in the Normalise tab, each token resampled to 21 points. ",
                  "Semitones or LZ suit Chao numerals; z-scores are linear in Hz.")),
          tags$li(tags$strong("Uploaded or curated f0, converted to LZ here:"),
                  " a quick route without the Normalise tab: f0 is converted to LZ (Zhu 1999) by speaker, then averaged.")
        )
      )
    )
  })

  # --- Sidebar UI ---
  output$ui_summarise <- renderUI({
    has_gca <- !is.null(gca_pred_data())
    has_gamm <- !is.null(gamm_pred_data())
    has_data <- !is.null(dataset())
    has_norm <- !is.null(normalised_data())
    has_cur  <- !is.null(curated_data) && !is.null(curated_data())
    has_cn   <- !is.null(curated_norm_data) && !is.null(curated_norm_data())

    # Build source choices dynamically: normalised data first (recommended),
    # then model predictions, then the Hz routes that convert to LZ here.
    src_choices <- c()
    if (has_norm) src_choices <- c(src_choices, "Normalised f0" = "normalised")
    if (has_cn)   src_choices <- c(src_choices, "Curated + normalised f0" = "curated_norm")
    if (has_gca)  src_choices <- c(src_choices, "GCA predictions" = "gca")
    if (has_gamm) src_choices <- c(src_choices, "GAMM predictions" = "gamm")
    if (has_data) src_choices <- c(src_choices, "Uploaded f0 (converted to LZ here)" = "raw_hz")
    if (has_cur)  src_choices <- c(src_choices, "Curated f0 (converted to LZ here)" = "curated")
    if (length(src_choices) == 0) src_choices <- c("No data available" = "none")

    # Default: best available
    default_src <- if (has_gamm) "gamm" else if (has_gca) "gca" else if (has_cn) "curated_norm" else if (has_norm) "normalised" else if (has_data) "raw_hz" else "none"

    # Column info for raw data
    # (guess_var() helper is defined globally in server.R)
    raw_data <- dataset()
    raw_vars <- if (!is.null(raw_data)) names(raw_data) else c("No dataset available")
    raw_types <- if (!is.null(raw_data)) sapply(raw_data, class) else rep("NA", length(raw_vars))
    raw_labels <- paste0(raw_vars, " {", raw_types, "}")

    # Smart defaults for raw data
    raw_token_default <- guess_var(raw_vars, c("^token", "^item", "^segment", "^id"), 1)
    raw_f0_default    <- guess_var(raw_vars, c("^f0$", "^f0_", "^pitch", "^F0"), 2)
    raw_time_default  <- guess_var(raw_vars, c("^time", "^t$", "^timepoint", "^measurement"), 3)
    raw_tone_default  <- guess_var(raw_vars, c("^tone", "^category", "^tonecat"), 4)
    raw_speaker_default <- guess_speaker(raw_vars)

    # Variable selectors for normalised data (from the Normalise tab, or the
    # curated + normalised data): one set per source, shown by its condition.
    # A z-score column gets a note, as it is linear in Hz.
    norm_panel <- function(source, prefix, d) {
      vars <- if (!is.null(d)) names(d) else c("No dataset available")
      ch   <- setNames(vars, paste0(vars, " {", if (!is.null(d)) sapply(d, class) else "NA", "}"))
      cv   <- attr(d, "curate_vars")   # curated + normalised: the tone used in Curate
      tone_default <- if (!is.null(cv$tone) && cv$tone %in% vars) cv$tone
                      else guess_var(vars, c("^tone", "^category", "^tonecat"), 4)
      conditionalPanel(sprintf("input.sum_source == '%s'", source),
        selectInput(paste0(prefix, "_token_var"), "Token ID variable:", choices = ch,
                    selected = guess_var(vars, c("^token", "^item", "^segment", "^id"), 1)),
        selectInput(paste0(prefix, "_f0_var"), "f0 variable:", choices = ch,
                    selected = guess_var(vars, c("^f0_st$", "^f0_zscore$", "^f0_lz$", "^f0_normalised$", "^f0_norm", "^f0$", "^f0_"), 2)),
        conditionalPanel(sprintf("/zscore/i.test(input.%s_f0_var || '')", prefix),
          tags$div(style = sum_note_style, icon("circle-info"),
                   " z-scores are linear in Hz. Chao numerals are usually read on a log scale (semitones or LZ), and the scale can move a digit near a boundary.")),
        selectInput(paste0(prefix, "_time_var"), "Time variable:", choices = ch,
                    selected = guess_var(vars, c("^time", "^t$", "^timepoint"), 3)),
        selectInput(paste0(prefix, "_tone_var"), "Tone variable:", choices = ch,
                    selected = tone_default)
      )
    }
    sum_note_style <- "font-size: 0.75rem; color: #6b5a1e; background: #fff8e1; border-radius: 4px; padding: 5px 8px; margin: -6px 0 10px 0;"
    # The panel re-renders when a data source appears: keep the numbers set.
    keep_num <- function(id, default) {
      v <- isolate(input[[id]]); if (is.numeric(v) && length(v) == 1 && !is.na(v)) v else default }

    tagList(
      wellPanel(
        # --- Data source ---
        selectInput("sum_source", "Data source:",
                    choices = src_choices, selected = default_src),
        conditionalPanel("input.sum_source == 'raw_hz'",
          tags$div(style = sum_note_style, icon("lightbulb"),
                   HTML(paste(" Converted to LZ by speaker here. Recommended: normalise first",
                              "(<strong>Normalise</strong> tab), then choose <strong>Normalised f0</strong>,",
                              "so Chao numerals and your models share one scale.")))),
        conditionalPanel("input.sum_source == 'curated'",
          tags$div(style = sum_note_style, icon("lightbulb"),
                   HTML(paste(" Converted to LZ by speaker here. Recommended: normalise the curated data",
                              "(<strong>Normalise</strong> tab, data source <strong>Curated data</strong>),",
                              "then choose <strong>Curated + normalised f0</strong>, so Chao numerals and",
                              "your models share one scale.")))),

        # --- Variable selectors for raw Hz ---
        conditionalPanel("input.sum_source == 'raw_hz'",
          selectInput("sum_raw_token_var", "Token ID variable:",
                      choices = setNames(raw_vars, raw_labels),
                      selected = raw_token_default),
          selectInput("sum_raw_f0_var", "f0 variable:",
                      choices = setNames(raw_vars, raw_labels),
                      selected = raw_f0_default),
          selectInput("sum_raw_time_var", "Time variable:",
                      choices = setNames(raw_vars, raw_labels),
                      selected = raw_time_default),
          selectInput("sum_raw_tone_var", "Tone variable:",
                      choices = setNames(raw_vars, raw_labels),
                      selected = raw_tone_default),
          selectInput("sum_raw_speaker_var", "Speaker variable (f0 is converted to LZ by speaker):",
                      choices = c("(none: one speaker)" = ".none",
                                  setNames(raw_vars, raw_labels)),
                      selected = raw_speaker_default)
        ),

        # --- Variable selectors for normalised / curated + normalised ---
        norm_panel("normalised", "sum_norm", normalised_data()),
        if (has_cn) norm_panel("curated_norm", "sum_cn", curated_norm_data()),
        tags$hr(),

        # --- Fuzzy boundary threshold ---
        numericInput("sum_boundary_tol", "Fuzzy boundary threshold (\u00b1):",
                     value = keep_num("sum_boundary_tol", 0.1), min = 0, max = 0.5, step = 0.05),

        # --- Turning-point threshold: how far a dip or peak must go for a middle digit ---
        numericInput("sum_turn_threshold", "Turning-point threshold (Chao units):",
                     value = keep_num("sum_turn_threshold", 0.5), min = 0, max = 4, step = 0.1),
        tags$div(style = "font-size: 0.75rem; color: #888; margin: -8px 0 10px 0;",
                 "A dip or peak beyond both endpoints larger than this can add a middle digit (reference-line scale)."),

        # --- Action buttons ---
        actionButton("sum_button", "Convert to Chao Digits"),
        div(style = "margin-top: 4px;",
          actionButton("sum_show_code", "Show R code", icon = icon("code"))
        ),
        tags$hr(),

        # --- Download ---
        h5("Download"),
        textInput("sum_filename", "Enter filename (without extension):",
                  value = if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                            paste0(input$dataset_name, "_summary")
                          else "chao_summary"),
        downloadButton("sum_download_csv", "Download CSV"),
        div(style = "margin-top: 6px;",
          downloadButton("sum_download_plot", "Download Plot")
        )
      )
    )
  })

  # --- Result storage ---
  sum_result <- reactiveVal(NULL)
  sum_norm_contours <- reactiveVal(NULL)
  sum_params <- reactiveVal(NULL)
  observeEvent(input$uploadfile, {   # new file: clear the old conversion
    sum_result(NULL); sum_norm_contours(NULL); sum_params(NULL)
  }, ignoreInit = TRUE)

  # compute_mean_contour() is now in R/chao.R (package-level).

  # --- Core conversion ---
  observeEvent(input$sum_button, {
    src <- input$sum_source

    if (is.null(src) || src == "none") {
      showNotification("No data available. Upload a CSV file first.",
                       type = "warning", duration = 5)
      return()
    }

    # --- Step 1: Get the per-tone contours -------------------------------
    src_data <- NULL
    raw_token <- raw_f0 <- raw_tone <- NULL
    lz_info <- NULL

    if (src == "gca") {
      contour_data <- gca_pred_data()
    } else if (src == "gamm") {
      contour_data <- gamm_pred_data()
      # Chao numerals read one syllable's contour on a 0-1 axis (onset at 0,
      # offset at 1). A GAMM on a sequential landmark axis spans 0 to n
      # segments, so its offset at 1 would be a segment boundary: refuse
      # rather than return numerals for the wrong span.
      n_seg <- attr(contour_data, "n_segments")
      if (identical(attr(contour_data, "time_axis"), "sequential") &&
          isTRUE(n_seg > 1)) {
        showNotification(
          sprintf(paste("Chao numerals describe one syllable's contour, and this",
                        "GAMM was fit across %d segments on a sequential time axis.",
                        "Use a monosyllabic batch (or one segment's rows) for Chao",
                        "numerals."), n_seg),
          type = "warning", duration = 10)
        return()
      }
    } else if (src %in% c("raw_hz", "curated")) {
      if (src == "raw_hz") {
        src_data <- dataset()
        req(src_data)
        req(input$sum_raw_token_var, input$sum_raw_f0_var,
            input$sum_raw_time_var,  input$sum_raw_tone_var)
        raw_token <- input$sum_raw_token_var
        raw_f0    <- input$sum_raw_f0_var
        raw_tone  <- input$sum_raw_tone_var
        raw_time  <- input$sum_raw_time_var
        raw_spk   <- input$sum_raw_speaker_var
      } else {
        # Curated data is structurally identical to the raw dataset (tone column
        # holds the relabels, excluded tokens dropped); resolve columns by name.
        src_data <- curated_data()
        req(src_data)
        cv <- names(src_data)
        used <- attr(src_data, "curate_vars")   # tone used in Curate, e.g. `cluster`
        raw_token <- guess_var(cv, c("^token", "^item", "^segment", "^id"), 1)
        raw_f0    <- guess_var(cv, c("^f0$", "^f0_", "^pitch", "^F0"), 2)
        raw_tone  <- if (!is.null(used$tone) && used$tone %in% cv) used$tone
                     else guess_var(cv, c("^tone", "^category", "^tonecat"), 4)
        raw_time  <- guess_var(cv, c("^time", "^t$", "^timepoint", "^measurement"), 3)
        raw_spk   <- guess_speaker(cv)   # ".none" (one speaker) if no speaker column
      }

      # Averaging Hz across speakers mixes higher and lower voices, so first
      # put each speaker's f0 on one scale: LZ (log z-score, Zhu 1999).
      # With no speaker variable, the data are one speaker.
      f0_vals <- suppressWarnings(as.numeric(src_data[[raw_f0]]))
      if (any(f0_vals < 0, na.rm = TRUE)) {
        showNotification(paste("The f0 variable has negative values, so it looks",
                               "normalised already. Choose the f0 column in Hz,",
                               "or use the Normalised f0 source."),
                         type = "warning", duration = 8)
        return()
      }
      one_spk <- is.null(raw_spk) || !(raw_spk %in% names(src_data))
      if (one_spk) {
        raw_spk <- ".speaker"
        src_data[[raw_spk]] <- "all"
      }
      lz_data <- tryCatch(
        normalise_f0(src_data, f0 = raw_f0, speaker = raw_spk,
                     tone = raw_tone, method = "lz"),
        error = function(e) {
          showNotification(paste("LZ conversion failed:", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        })
      if (is.null(lz_data)) return()
      lz_info <- list(f0 = raw_f0, token = raw_token, time = raw_time,
                      tone = raw_tone,
                      speaker = if (one_spk) NA_character_ else raw_spk,
                      n_speakers = length(unique(lz_data[[raw_spk]])))
      src_data <- lz_data
      raw_f0 <- "f0_lz"
      contour_data <- compute_mean_contour(
        src_data,
        token = raw_token, f0 = raw_f0,
        time  = raw_time, tone = raw_tone
      )
    } else if (src %in% c("normalised", "curated_norm")) {
      src_data <- if (src == "normalised") normalised_data()
                  else if (!is.null(curated_norm_data)) curated_norm_data()
      req(src_data)
      pre <- if (src == "normalised") "sum_norm" else "sum_cn"
      sel <- function(x) input[[paste0(pre, "_", x, "_var")]]
      req(sel("token"), sel("f0"), sel("time"), sel("tone"))
      raw_token <- sel("token")
      raw_f0    <- sel("f0")
      raw_tone  <- sel("tone")
      contour_data <- compute_mean_contour(
        src_data,
        token = raw_token, f0 = raw_f0,
        time  = sel("time"), tone = raw_tone
      )
    } else {
      contour_data <- NULL
    }

    if (is.null(contour_data) || nrow(contour_data) == 0) {
      showNotification("No contour data available. Check variable selections or fit a model first.",
                       type = "warning", duration = 5)
      return()
    }

    # --- Step 2: Delegate all analytics to the package function -----------
    # contour_to_chao() handles continuous Chao scaling, turning-point
    # detection and the two numeral methods; the range comes back as
    # attributes.
    turn_th <- input$sum_turn_threshold
    if (!is.numeric(turn_th) || length(turn_th) != 1 || is.na(turn_th) || turn_th < 0)
      turn_th <- 0.5
    chao <- contour_to_chao(
      contour_data,
      tone_col  = "tone", time_col = "time", f0_col = "f0_predicted",
      threshold = turn_th
    )
    f0_min <- attr(chao, "f0_min")
    f0_max <- attr(chao, "f0_max")

    # --- Step 3: Build the contour the plot tab consumes ------------------
    if (f0_max == f0_min) {
      contour_data$chao_ref <- 3
      contour_data$chao_int <- 2.5
    } else {
      contour_data$chao_ref <- (contour_data$f0_predicted - f0_min) /
                                 (f0_max - f0_min) * 4 + 1
      contour_data$chao_int <- (contour_data$f0_predicted - f0_min) /
                                 (f0_max - f0_min) * 5
    }
    contour_data$chao_continuous <- contour_data$chao_ref
    sum_norm_contours(contour_data)

    # --- Step 4: Reshape chao result into the wide UI-display format ------
    boundary_tol <- input$sum_boundary_tol
    if (!is.numeric(boundary_tol) || length(boundary_tol) != 1 ||
        is.na(boundary_tol) || boundary_tol < 0) boundary_tol <- 0.1

    rows <- lapply(seq_len(nrow(chao)), function(i) {
      r            <- chao[i, ]
      sample_times <- r$sample_times[[1]]
      ref_vals     <- r$ref_continuous[[1]]
      int_vals     <- r$int_continuous[[1]]
      n_samples    <- length(sample_times)   # 3 when a turning point is sampled

      # One digit per sampled point; a scale whose numeral is a digit short
      # left out the turning point (same digit as an endpoint).
      ref_digits <- pmax(1L, pmin(5L, round(ref_vals)))
      int_digits <- pmax(1L, pmin(5L, ceiling(int_vals)))
      ref_drop   <- n_samples == 3 && nchar(r$refline)  < 3
      int_drop   <- n_samples == 3 && nchar(r$interval) < 3

      # Turning point: the larger of the dip and the peak, as contour_to_chao()
      # samples it. Near the threshold (within the fuzzy margin), also give
      # the numerals the other side of it would produce.
      kind <- if (r$dip >= r$peak) "dip" else "peak"
      size <- if (kind == "dip") r$dip else r$peak
      near <- size > 0 && abs(size - turn_th) <= boundary_tol
      alt  <- NA_character_
      if (near) {
        a <- contour_to_chao(contour_data, tone_col = "tone", time_col = "time",
                             f0_col = "f0_predicted",
                             threshold = max(0, if (n_samples == 3) size + 1e-6 else size - 1e-6))
        a <- a[a$tone == r$tone, , drop = FALSE]
        if (nrow(a) == 1) alt <- paste0(a$refline, " / ", a$interval)
        # Flag only when the other side of the threshold changes the numerals
        near <- !is.na(alt) && alt != paste0(r$refline, " / ", r$interval)
        if (!near) alt <- NA_character_
      }

      row <- data.frame(
        tone              = r$tone,
        n_samples         = n_samples,
        ref_drop          = ref_drop,
        int_drop          = int_drop,
        # round() changes digit half-way between the reference lines, so the
        # ambiguous values are those near 1.5, 2.5, 3.5 and 4.5.
        ref_scaled_values = format_scaled_values_html(ref_vals, 1:4 + 0.5, boundary_tol,
                                                      if (ref_drop) 2L else integer(0)),
        refline           = r$refline,
        int_scaled_values = format_scaled_values_html(int_vals, 1:4, boundary_tol,
                                                      if (int_drop) 2L else integer(0)),
        interval          = r$interval,
        ref_letters       = chao_to_letters(r$refline),
        int_letters       = chao_to_letters(r$interval),
        contour_shape     = r$shape,
        turn_kind         = if (size > 0) kind else "none",
        turn_size         = size,
        turn_near         = near,
        turn_alt          = alt,
        stringsAsFactors  = FALSE
      )

      # Wide per-sample columns (always 3 slots; NA for unused ones). The
      # digits are per sampled point, including a turning point left out of
      # a numeral (ref_drop / int_drop).
      max_digits <- 3
      for (j in seq_len(max_digits)) {
        if (j <= n_samples) {
          row[[paste0("t",  j)]] <- sample_times[j]
          row[[paste0("v",  j)]] <- ref_vals[j]
          row[[paste0("r",  j)]] <- ref_digits[j]
          row[[paste0("iv", j)]] <- int_vals[j]
          row[[paste0("ic", j)]] <- int_digits[j]
        } else {
          row[[paste0("t",  j)]] <- NA_real_
          row[[paste0("v",  j)]] <- NA_real_
          row[[paste0("r",  j)]] <- NA_real_
          row[[paste0("iv", j)]] <- NA_real_
          row[[paste0("ic", j)]] <- NA_real_
        }
      }
      row
    })

    sum_result(do.call(rbind, rows))

    sum_params(list(
      source       = src,
      lz           = lz_info,
      boundary_tol = boundary_tol,
      turn_threshold = turn_th,
      f0_min       = f0_min,
      f0_max       = f0_max
    ))
  })

  # --- Summary box ---
  output$summarise_summary <- renderUI({
    req(sum_result())
    result <- sum_result()
    params <- sum_params()

    src_label <- switch(params$source,
      "raw_hz" = "Uploaded f0 (Hz), converted to LZ",
      "curated" = "Curated f0 (Hz), converted to LZ",
      "normalised" = "Normalised f0",
      "curated_norm" = "Curated + normalised f0",
      "gca" = "GCA predictions",
      "gamm" = "GAMM predictions"
    )
    # Raw and curated f0 are converted to LZ before conversion; normalised f0
    # and model predictions may be in semitones, z-scores or LZ.
    lz <- params$lz
    f0_unit <- if (!is.null(lz)) " LZ" else ""
    lz_note <- if (!is.null(lz)) paste0(
      "f0 (", lz$f0, ") converted to LZ by speaker before conversion: ",
      if (is.na(lz$speaker)) "no speaker variable, so one speaker"
      else paste0(lz$n_speakers, if (lz$n_speakers == 1) " speaker" else " speakers",
                  " (", lz$speaker, ")"))
    # Shared table styles
    th_style <- "padding: 6px 8px; border-bottom: 2px solid #ddd; text-align: center; font-size: 0.82rem;"
    th_group <- "padding: 6px 8px; border-bottom: 1px solid #ccc; text-align: center; font-size: 0.82rem; font-weight: bold;"
    td_style <- "padding: 5px 8px; text-align: center; font-size: 0.82rem;"
    td_style_mono <- "padding: 5px 8px; text-align: left; font-family: monospace; font-size: 0.82rem;"
    td_style_bold <- "padding: 5px 8px; text-align: center; font-size: 1rem; font-weight: bold; font-family: monospace;"
    td_style_letters <- "padding: 2px 8px; text-align: center; font-size: 1.6rem; line-height: 1;"

    # Build side-by-side comparison table with two-row header
    header_row1_cells <- list(
      tags$th(style = paste0(th_style, " border-right: 1px solid #ddd;"), rowspan = 2, "Tone"),
      tags$th(style = paste0(th_group, " background-color: #f0f7ff; border-right: 1px solid #ddd;"), colspan = 3,
              HTML("Reference-line FOR [1, 5]<br><span style='font-weight:normal; font-size:0.72rem; color:#888;'>\u00d7 4 + 1, round()</span>")),
      tags$th(style = paste0(th_group, " background-color: #fff8f0; border-right: 1px solid #ddd;"), colspan = 3,
              HTML("Interval-based FOR (0, 5]<br><span style='font-weight:normal; font-size:0.72rem; color:#888;'>\u00d7 5, ceiling()</span>"))
    )
    header_row2_cells <- list(
      tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #f0f7ff;"), "Scaled"),
      tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #f0f7ff;"),
              HTML("round()")),
      tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #f0f7ff; border-right: 1px solid #ddd;"), "Letters"),
      tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #fff8f0;"), "Scaled"),
      tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #fff8f0;"),
              HTML("ceiling()")),
      tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #fff8f0; border-right: 1px solid #ddd;"), "Letters")
    )

    header_row1_cells <- c(header_row1_cells, list(
      tags$th(style = th_style, rowspan = 2, "Shape"),
      tags$th(style = th_style, rowspan = 2,
              HTML(paste0("Dip / peak<br><span style='font-weight:normal; font-size:0.72rem; color:#888;'>sampled above ",
                          params$turn_threshold, "</span>")))
    ))

    header_row1 <- do.call(tags$tr, header_row1_cells)
    header_row2 <- do.call(tags$tr, header_row2_cells)

    # Tones whose dip or peak is near the turning-point threshold: say what
    # the other side of it gives (reference-line / interval numerals).
    near_i <- which(result$turn_near %in% TRUE)
    turn_note <- if (length(near_i) > 0) {
      th <- params$turn_threshold
      lines <- lapply(near_i, function(i) {
        sz <- result$turn_size[i]
        now <- paste0(result$refline[i], " / ", result$interval[i])
        other <- if (result$n_samples[i] == 3)
          sprintf("with a threshold of %.2f or more it would be %s", ceiling(sz * 100) / 100, result$turn_alt[i])
        else
          sprintf("with a threshold of %.2f or less it would be %s", (ceiling(sz * 100) - 1) / 100, result$turn_alt[i])
        tags$li(sprintf("Tone %s: %s %.2f, so %s here; %s.", result$tone[i], result$turn_kind[i], sz, now, other))
      })
      tags$div(style = "margin-top: 10px; font-size: 0.8rem; color: #6b5a1e;",
        tags$strong(sprintf("Near the turning-point threshold (%s \u00b1 %s):", th, params$boundary_tol)),
        tags$ul(style = "margin: 4px 0 0 0; padding-left: 18px;", lines))
    }

    # Tones whose turning point was sampled but rounds to the same digit as an
    # endpoint on one or both scales, so that numeral keeps two digits.
    drop_i <- which(result$ref_drop | result$int_drop)
    drop_note <- if (length(drop_i) > 0) {
      lines <- lapply(drop_i, function(i) {
        where <- if (result$ref_drop[i] && result$int_drop[i]) "on both scales"
                 else if (result$ref_drop[i]) "on the reference-line scale"
                 else "on the interval scale"
        tags$li(sprintf("Tone %s: %s %.2f, but %s the %s rounds to the same digit as an endpoint, so %s (%s / %s).",
                        result$tone[i], result$turn_kind[i], result$turn_size[i], where, result$turn_kind[i],
                        if (result$ref_drop[i] && result$int_drop[i]) "both numerals keep two digits"
                        else "that numeral keeps two digits",
                        result$refline[i], result$interval[i]))
      })
      tags$div(style = "margin-top: 10px; font-size: 0.8rem; color: #555;",
        tags$strong("Turning points too shallow to show in the digits:"),
        tags$ul(style = "margin: 4px 0 0 0; padding-left: 18px;", lines))
    }

    body_rows <- lapply(seq_len(nrow(result)), function(i) {
      # Highlight where the two methods disagree
      any_disagree <- result$refline[i] != result$interval[i]
      hl_ref <- if (any_disagree) "background-color: #e8f0fe;" else ""
      hl_int <- if (any_disagree) "background-color: #fff3cd;" else ""

      cells <- list(
        tags$td(style = paste0(td_style, " font-weight: bold; border-right: 1px solid #ddd;"), result$tone[i]),
        tags$td(style = paste0(td_style_mono, " background-color: #f8fbff;"), HTML(result$ref_scaled_values[i])),
        tags$td(style = paste0(td_style_bold, " background-color: #f8fbff; ", hl_ref), result$refline[i]),
        tags$td(class = "tone-letters", style = paste0(td_style_letters, " background-color: #f8fbff; border-right: 1px solid #ddd; ", hl_ref), result$ref_letters[i]),
        tags$td(style = paste0(td_style_mono, " background-color: #fffcf8;"), HTML(result$int_scaled_values[i])),
        tags$td(style = paste0(td_style_bold, " background-color: #fffcf8; ", hl_int), result$interval[i]),
        tags$td(class = "tone-letters", style = paste0(td_style_letters, " background-color: #fffcf8; border-right: 1px solid #ddd; ", hl_int), result$int_letters[i])
      )

      turn_txt <- if (result$turn_kind[i] == "none") "none"
                  else sprintf("%s %.2f", result$turn_kind[i], result$turn_size[i])
      turn_css <- if (isTRUE(result$turn_near[i])) " color: #d9534f; font-weight: bold;"
                  else if (result$turn_kind[i] == "none") " color: #999;" else ""
      cells <- c(cells, list(
        tags$td(style = td_style, result$contour_shape[i]),
        tags$td(style = paste0(td_style, turn_css), turn_txt)
      ))

      do.call(tags$tr, cells)
    })

    tagList(
      tags$div(
        style = "background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem;",
        tags$strong("Conversion parameters:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(paste0("Source: ", src_label)),
          if (!is.null(lz_note)) tags$li(lz_note),
          tags$li(HTML(paste0("f0 range (lowest and highest points of the contours): ", round(params$f0_min, 2), " \u2013 ", round(params$f0_max, 2),
                              f0_unit,
                              " &nbsp;(\u0394 = ", round(params$f0_max - params$f0_min, 2), ")"))),
          tags$li(paste0("Turning-point threshold: ", params$turn_threshold,
                         " Chao units (reference-line scale)"))
        ),
        tags$strong("Chao tone numeral comparison:"),
        tags$p(style = "font-size: 0.8rem; color: #888; margin: 4px 0 6px 0;",
          HTML(paste0("<span style='color:#d9534f;font-weight:bold;'>Red</span>: a value within \u00b1",
                      params$boundary_tol, " of a digit boundary (1.5, 2.5, 3.5, 4.5 on the reference-line scale; ",
                      "1\u20134 on the interval scale), or a dip or peak within \u00b1", params$boundary_tol,
                      " of the turning-point threshold that would change the numeral."))),
        tags$table(
          style = "margin-top: 4px; border-collapse: collapse; font-size: 0.84rem; width: 100%;",
          tags$thead(header_row1, header_row2),
          tags$tbody(body_rows)
        ),
        turn_note,
        drop_note
      )
    )
  })

  # --- Plots (panels: reference-line and interval) ---
  # The data behind the figure; the figure itself is drawn by plot_chao_panels()
  # (ui/summarise_plot.R) on whichever device is open, the app's or the PNG's.
  sum_plot_data <- reactive({
    req(sum_norm_contours())
    req(sum_result())

    pdat <- sum_norm_contours()
    result <- sum_result()
    params <- sum_params()

    # Sampled points (onset, any turning point, offset) on both scales.
    # `shown` is FALSE for a turning point left out of that scale's numeral.
    ref_point_rows <- list()
    int_point_rows <- list()
    for (i in seq_len(nrow(result))) {
      for (j in seq_len(result$n_samples[i])) {
        t_col <- paste0("t", j)
        if (t_col %in% names(result) && !is.na(result[[t_col]][i])) {
          # Reference-line points
          ref_point_rows[[length(ref_point_rows) + 1]] <- data.frame(
            time = result[[t_col]][i],
            chao_val = result[[paste0("v", j)]][i],
            chao_digit = as.character(result[[paste0("r", j)]][i]),
            tone = result$tone[i],
            shown = !(j == 2 && result$ref_drop[i]),
            stringsAsFactors = FALSE
          )
          # Interval points
          int_point_rows[[length(int_point_rows) + 1]] <- data.frame(
            time = result[[t_col]][i],
            chao_val = result[[paste0("iv", j)]][i],
            chao_digit = as.character(result[[paste0("ic", j)]][i]),
            tone = result$tone[i],
            shown = !(j == 2 && result$int_drop[i]),
            stringsAsFactors = FALSE
          )
        }
      }
    }
    list(pdat = pdat,
         ref_point_df = do.call(rbind, ref_point_rows),
         int_point_df = do.call(rbind, int_point_rows),
         tol = if (is.null(params$boundary_tol)) 0.1 else params$boundary_tol)
  })

  # Build after grid.newpage() so thematic has resolved the app font first.
  draw_sum_plot <- function(base_family = "") {
    d <- sum_plot_data()
    grid::grid.newpage()
    grid::grid.draw(plot_chao_panels(d$pdat, d$ref_point_df, d$int_point_df, d$tol,
                                     base_family = base_family))
  }

  output$summarise_plot <- renderPlot({
    draw_sum_plot()
  }, height = function() { if (!is.null(sum_norm_contours())) 440 else 1 })

  # --- Download handlers ---
  output$sum_download_csv <- downloadHandler(
    filename = function() paste0(input$sum_filename, ".csv"),
    content = function(file) {
      req(sum_result())
      # Export clean columns only
      result <- sum_result()
      # Strip HTML from scaled values for CSV export
      clean_ref_scaled <- gsub("<[^>]+>", "", result$ref_scaled_values)
      clean_int_scaled <- gsub("<[^>]+>", "", result$int_scaled_values)
      export <- data.frame(
        Tone = result$tone,
        `Ref Scaled [1,5]` = clean_ref_scaled,
        `Ref Numeral` = result$refline,
        `Ref Letters` = result$ref_letters,
        `Int Scaled (0,5]` = clean_int_scaled,
        `Int Numeral` = result$interval,
        `Int Letters` = result$int_letters,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      export$Shape <- result$contour_shape
      export$`Dip/peak` <- result$turn_kind
      export$`Dip/peak size (Chao units)` <- round(result$turn_size, 3)
      export$`Turning-point threshold` <- sum_params()$turn_threshold
      write.csv(export, file, row.names = FALSE)
      showNotification(paste("Data saved as", paste0(input$sum_filename, ".csv")),
                       type = "message", duration = 4)
    }
  )

  output$sum_download_plot <- downloadHandler(
    filename = function() paste0(input$sum_filename, ".png"),
    content = function(file) {
      req(sum_norm_contours())
      # ragg + the bundled Open Sans (as in the app); plain png() otherwise.
      fam <- ""
      if (requireNamespace("ragg", quietly = TRUE) &&
          requireNamespace("systemfonts", quietly = TRUE)) {
        fam <- tryCatch({
          if (!"Open Sans" %in% systemfonts::registry_fonts()$family) {
            systemfonts::register_font("Open Sans",
              plain = "open-sans/regular.ttf", bold = "open-sans/bold.ttf",
              italic = "open-sans/italic.ttf", bolditalic = "open-sans/bolditalic.ttf")
          }
          "Open Sans"
        }, error = function(e) "")
        ragg::agg_png(file, width = 12, height = 5, units = "in", res = 300)
      } else {
        png(file, width = 12, height = 5, units = "in", res = 300)
      }
      on.exit(grDevices::dev.off(), add = TRUE)
      draw_sum_plot(base_family = fam)
      showNotification(paste("Plot saved as", paste0(input$sum_filename, ".png")),
                       type = "message", duration = 4)
    }
  )

  # --- Show R code toggle ---
  sum_code_visible <- reactiveVal(FALSE)

  observeEvent(input$sum_show_code, {
    sum_code_visible(!sum_code_visible())
  })

  output$summarise_r_code <- renderUI({
    req(sum_code_visible())
    req(sum_params())

    params <- sum_params()

    # Raw and curated f0: show the LZ step that builds pred.
    lz <- params$lz
    dq <- function(x) deparse(x)
    code_lz <- if (!is.null(lz)) {
      spk <- if (is.na(lz$speaker)) ".speaker" else lz$speaker
      paste0(
        '# Step 0: Convert each speaker\'s f0 to LZ (log z-score, Zhu 1999), so that\n',
        '# higher and lower voices are averaged on a common scale, then take the\n',
        '# per-tone mean contours\n',
        '# dat: your f0 data, one row per f0 sample\n',
        if (is.na(lz$speaker)) 'dat$.speaker <- "all"   # no speaker variable: one speaker\n' else "",
        'dat <- normalise_f0(dat, f0 = ', dq(lz$f0), ', speaker = ', dq(spk), ',\n',
        '                    tone = ', dq(lz$tone), ', method = "lz")\n',
        'pred <- compute_mean_contour(dat, token = ', dq(lz$token), ', f0 = "f0_lz",\n',
        '                             time = ', dq(lz$time), ', tone = ', dq(lz$tone), ')\n',
        '\n')
    } else paste0(
      '# pred is a data frame with columns: time, f0_predicted, tone\n',
      '# (from GCA/GAMM predictions or per-tone mean contours)\n\n')

    code_text <- paste0(
      if (!is.null(lz)) 'library(shinytone)\n' else "",
      'library(ggplot2)\n\n',
      code_lz,
      '# Step 1: Define pitch range\n',
      'f0_min <- min(pred$f0_predicted, na.rm = TRUE)\n',
      'f0_max <- max(pred$f0_predicted, na.rm = TRUE)\n\n',
      '# Step 2: Fraction of Range normalisation\n',
      '# Eq. 3 (reference-line): f0\' = (f0 - f0_min) / (f0_max - f0_min) * 4 + 1\n',
      '# Eq. 1a (interval):      f0\' = (f0 - f0_min) / (f0_max - f0_min) * 5\n',
      'if (f0_max == f0_min) {   # flat contours: mid-scale\n',
      '  pred$chao_ref <- 3; pred$chao_int <- 2.5\n',
      '} else {\n',
      '  pred$chao_ref <- (pred$f0_predicted - f0_min) / (f0_max - f0_min) * 4 + 1\n',
      '  pred$chao_int <- (pred$f0_predicted - f0_min) / (f0_max - f0_min) * 5\n',
      '}\n',
      '\n# Step 3: Per tone, sample the onset, the offset and any turning point, then convert\n',
      '# A turning point is the lowest (dip) or highest (peak) point between 10% and 90%\n',
      '# of the time axis on the reference-line scale, whichever lies further beyond both\n',
      '# endpoints. It is sampled when it lies more than `threshold` Chao units beyond them.\n',
      'threshold <- ', if (is.null(params$turn_threshold)) 0.5 else params$turn_threshold, '\n',
      'tone_levels <- unique(pred$tone)\n',
      'results <- lapply(tone_levels, function(tone_val) {\n',
      '  td <- pred[pred$tone == tone_val & !is.na(pred$chao_ref) & !is.na(pred$time), ]\n',
      '  td <- td[order(td$time), ]\n',
      '  if (nrow(td) < 2) return(NULL)\n',
      '  fn_ref <- approxfun(td$time, td$chao_ref, rule = 2)\n',
      '  fn_int <- approxfun(td$time, td$chao_int, rule = 2)\n\n',
      '  fine_time <- seq(0, 1, length.out = 200)\n',
      '  fine_ref  <- fn_ref(fine_time)\n',
      '  inner <- 20:180   # skip the first and last 10%\n',
      '  i_min <- inner[which.min(fine_ref[inner])]\n',
      '  i_max <- inner[which.max(fine_ref[inner])]\n',
      '  onset <- fine_ref[1]; offset <- fine_ref[200]\n',
      '  dip  <- min(onset, offset) - fine_ref[i_min]   # how far below both endpoints\n',
      '  peak <- fine_ref[i_max] - max(onset, offset)   # how far above both endpoints\n',
      '  use_dip <- dip >= peak                         # sample the larger turn\n',
      '  turn <- if (use_dip) dip else peak\n',
      '  sample_times <- if (turn <= threshold) c(0, 1) else\n',
      '                  if (use_dip) c(0, fine_time[i_min], 1) else c(0, fine_time[i_max], 1)\n\n',
      '  # Keep a middle digit only where it shows the turn: below both endpoint\n',
      '  # digits for a dip, above both for a peak (223 would read as a late rise)\n',
      '  keep_turn <- function(d) {\n',
      '    if (length(d) < 3) return(d)\n',
      '    shows <- if (use_dip) d[2] < min(d[-2]) else d[2] > max(d[-2])\n',
      '    if (shows) d else d[-2]\n',
      '  }\n\n',
      '  # Eq. 3 [1,5]: round to nearest integer\n',
      '  ref_vals <- fn_ref(sample_times)\n',
      '  refline <- paste0(keep_turn(pmax(1, pmin(5, round(ref_vals)))), collapse = "")\n\n',
      '  # Eq. 1a (0,5]: ceiling -> Chao digits 1-5\n',
      '  int_vals <- fn_int(sample_times)\n',
      '  interval <- paste0(keep_turn(pmax(1, pmin(5, ceiling(int_vals)))), collapse = "")\n',
      '\n  data.frame(tone = tone_val,\n',
      '             ref_scaled = paste(round(ref_vals, 2), collapse = " -> "),\n',
      '             refline = refline,\n',
      '             int_scaled = paste(round(int_vals, 2), collapse = " -> "),\n',
      '             interval = interval',
      ')\n',
      '})\n',
      'chao_summary <- do.call(rbind, results)\n\n',
      '# Step 4: IPA tone letters, one per digit (1 = \u02e9 ... 5 = \u02e5)\n',
      'chao_summary$refline_letters <- chartr("12345", "\u02e9\u02e8\u02e7\u02e6\u02e5", chao_summary$refline)\n',
      'chao_summary$interval_letters <- chartr("12345", "\u02e9\u02e8\u02e7\u02e6\u02e5", chao_summary$interval)\n',
      'print(chao_summary)\n'
    )

    tagList(
      tags$hr(),
      tags$div(
        style = "position: relative;",
        tags$h5(icon("code"), " R Code"),
        tags$pre(
          style = "padding: 15px; border-radius: 5px; overflow-x: auto;",
          tags$code(code_text)
        )
      )
    )
  })

}
