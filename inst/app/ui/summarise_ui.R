###############################################
# Summarise tab — Chao Tone Digit Conversion
###############################################

summarise_ui <- function(input, output, session, dataset, normalised_data, gca_pred_data, gamm_pred_data, curated_data = NULL) {

  # classify_contour() and compute_mean_contour() are now package-level
  # functions (R/chao.R), available in the global namespace via global.R.
  # The HTML-highlighted scaled-value formatter below stays here because
  # it's UI-specific.

  # --- UI helper: highlight scaled values within `tol` of a reference set ---
  format_scaled_values_html <- function(vals, refs, tol = 0.1) {
    pieces <- vapply(vals, function(v) {
      val_str <- sprintf("%.2f", v)
      if (any(abs(v - refs) < tol, na.rm = TRUE)) {
        paste0('<span style="color: #d9534f; font-weight: bold;" title="within ±',
               tol, ' of reference">', val_str, "</span>")
      } else {
        val_str
      }
    }, character(1))
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
    code_style <- "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;"
    tagList(
      guide_box("Chao tone numeral guide",
        tags$p(style = "margin-bottom: 8px;",
          "Chao tone numerals describe tone contours using a 5-level pitch scale (1 = lowest, 5 = highest). ",
          "For example, Mandarin T1 = ", tags$code(style = code_style, "55"),
          " (high level), T2 = ", tags$code(style = code_style, "35"),
          " (mid rising), T3 = ", tags$code(style = code_style, "214"),
          " (low dipping), T4 = ", tags$code(style = code_style, "51"),
          " (high falling)."
        ),
        tags$p(style = "margin-bottom: 8px;",
          "Each numeral is also written in IPA tone letters, one per digit from ",
          tags$span(class = "tone-letters", "\u02e9"), " (1) to ",
          tags$span(class = "tone-letters", "\u02e5"), " (5): ",
          tags$code(style = code_style, "35"), " = ", tags$span(class = "tone-letters", "\u02e7\u02e5"), ", ",
          tags$code(style = code_style, "214"), " = ", tags$span(class = "tone-letters", "\u02e8\u02e9\u02e6"), "."
        ),
        tags$p(style = "margin-bottom: 8px;",
          "Both methods use ",
          tags$strong("Fraction of Range (FOR)"),
          " normalisation, which maps f0 onto a bounded scale between the lowest and highest points of the per-tone contours (min/max). With mean contours, each of these points is itself an average over tokens, not a single f0 measurement. The two methods differ in how Tone Numerals (i.e. 1\u20135) are assigned. ",
          "The mapping is linear in the units of the input, so the input sets the pitch scale: semitones and LZ give a logarithmic scale, closer to how pitch is perceived, whereas Hz and z-scores give a linear one."
        ),
        tags$strong("Conversion methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(
            tags$strong("Reference-line FOR [1, 5]:"),
            tags$div(style = "margin: 4px 0 2px 0; font-family: 'Georgia', serif; font-style: italic; font-size: 0.92rem;",
              HTML("f\u2080\u2032 = (f\u2080 \u2212 f\u2080<sub>min</sub>) / (f\u2080<sub>max</sub> \u2212 f\u2080<sub>min</sub>) \u00d7 4 + 1")
            ),
            " Scales f0 onto [1, 5] with 5 reference lines at integer values. Each sampled value is ", tags$code(style = code_style, "round()"), "ed to the nearest integer."
          ),
          tags$li(
            tags$strong("Interval-based FOR (0, 5]:"),
            tags$div(style = "margin: 4px 0 2px 0; font-family: 'Georgia', serif; font-style: italic; font-size: 0.92rem;",
              HTML("f\u2080\u2032 = (f\u2080 \u2212 f\u2080<sub>min</sub>) / (f\u2080<sub>max</sub> \u2212 f\u2080<sub>min</sub>) \u00d7 5")
            ),
            " Scales f0 onto (0, 5] with 5 equal-width intervals. Each sampled value is assigned to an interval via ", tags$code(style = code_style, "ceiling()"),
            ": (0,1]\u21921, (1,2]\u21922, (2,3]\u21923, (3,4]\u21924, (4,5]\u21925."
          )
        ),
        tags$p(style = "margin: 4px 0 8px 0; font-size: 0.84rem; color: #777;",
          HTML("Note: Within a given conversion system, scaled values near a digit boundary can be ambiguous. In the interval-based system the boundaries are the integers 1\u20134: a scaled value of 1.1 could reasonably be assigned to digit 1 or 2, depending on the tonal contrasts in the language. In the reference-line system they lie half-way between the lines (1.5, 2.5, 3.5, 4.5), so 2.45 could be 2 or 3. Values within the fuzzy boundary threshold (\u00b1<em>t</em>) are highlighted in <span style='color:#d9534f; font-weight:bold;'>red</span>. For more discussion on conversion methods and boundary handling, see Xu &amp; Zhang (2024).")
        ),
        tags$strong("Data source:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Raw or curated f0 (Hz):"), " Each speaker\u2019s f0 is first converted to LZ (log z-score), the normalisation Zhu (1999) found best, so that higher and lower voices are not averaged together in Hz; per-tone mean contours are then taken. Choose the speaker variable, or none to treat the data as one speaker."),
          tags$li(tags$strong("Normalised f0:"), " Per-tone mean contours of the f0 from the Normalise tab, on the scale chosen there."),
          tags$li(tags$strong("GCA / GAMM predictions:"), " Available after fitting the respective model. Uses the population-level predicted contours (smoother, more representative).")
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

    # Build source choices dynamically
    src_choices <- c()
    if (has_data) src_choices <- c(src_choices, "Raw f0 (Hz)" = "raw_hz")
    if (has_cur)  src_choices <- c(src_choices, "Curated f0" = "curated")
    if (has_norm) src_choices <- c(src_choices, "Normalised f0" = "normalised")
    if (has_gca)  src_choices <- c(src_choices, "GCA predictions" = "gca")
    if (has_gamm) src_choices <- c(src_choices, "GAMM predictions" = "gamm")
    if (length(src_choices) == 0) src_choices <- c("No data available" = "none")

    # Default: best available
    default_src <- if (has_gamm) "gamm" else if (has_gca) "gca" else if (has_norm) "normalised" else if (has_data) "raw_hz" else "none"

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

    # Column info for normalised data
    norm_data <- normalised_data()
    norm_vars <- if (!is.null(norm_data)) names(norm_data) else c("No dataset available")
    norm_types <- if (!is.null(norm_data)) sapply(norm_data, class) else rep("NA", length(norm_vars))
    norm_labels <- paste0(norm_vars, " {", norm_types, "}")

    # Smart defaults for normalised data
    norm_token_default <- guess_var(norm_vars, c("^token", "^item", "^segment", "^id"), 1)
    norm_f0_default    <- guess_var(norm_vars, c("^f0_st$", "^f0_zscore$", "^f0_lz$", "^f0_normalised$", "^f0_norm", "^f0$", "^f0_"), 2)
    norm_time_default  <- guess_var(norm_vars, c("^time", "^t$", "^timepoint"), 3)
    norm_tone_default  <- guess_var(norm_vars, c("^tone", "^category", "^tonecat"), 4)

    tagList(
      wellPanel(
        # --- Data source ---
        selectInput("sum_source", "Data source:",
                    choices = src_choices, selected = default_src),

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

        # --- Variable selectors for normalised ---
        conditionalPanel("input.sum_source == 'normalised'",
          selectInput("sum_norm_token_var", "Token ID variable:",
                      choices = setNames(norm_vars, norm_labels),
                      selected = norm_token_default),
          selectInput("sum_norm_f0_var", "f0 variable:",
                      choices = setNames(norm_vars, norm_labels),
                      selected = norm_f0_default),
          selectInput("sum_norm_time_var", "Time variable:",
                      choices = setNames(norm_vars, norm_labels),
                      selected = norm_time_default),
          selectInput("sum_norm_tone_var", "Tone variable:",
                      choices = setNames(norm_vars, norm_labels),
                      selected = norm_tone_default)
        ),
        tags$hr(),

        # --- Fuzzy boundary threshold ---
        numericInput("sum_boundary_tol", "Fuzzy boundary threshold (\u00b1):",
                     value = 0.1, min = 0, max = 0.5, step = 0.05),

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
        raw_token <- guess_var(cv, c("^token", "^item", "^segment", "^id"), 1)
        raw_f0    <- guess_var(cv, c("^f0$", "^f0_", "^pitch", "^F0"), 2)
        raw_tone  <- guess_var(cv, c("^tone", "^category", "^tonecat"), 4)
        raw_time  <- guess_var(cv, c("^time", "^t$", "^timepoint", "^measurement"), 3)
        raw_spk   <- guess_speaker(cv)
      }

      # Averaging Hz across speakers mixes higher and lower voices, so first
      # put each speaker's f0 on one scale: LZ, the log z-score Zhu (1999)
      # found best for tone. With no speaker variable, the data are one speaker.
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
    } else if (src == "normalised") {
      src_data <- normalised_data()
      req(src_data)
      req(input$sum_norm_token_var, input$sum_norm_f0_var,
          input$sum_norm_time_var,  input$sum_norm_tone_var)
      raw_token <- input$sum_norm_token_var
      raw_f0    <- input$sum_norm_f0_var
      raw_tone  <- input$sum_norm_tone_var
      contour_data <- compute_mean_contour(
        src_data,
        token = raw_token, f0 = raw_f0,
        time  = input$sum_norm_time_var, tone = raw_tone
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
    chao <- contour_to_chao(
      contour_data,
      tone_col  = "tone", time_col = "time", f0_col = "f0_predicted",
      threshold = 0.5
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
    boundary_tol <- if (!is.null(input$sum_boundary_tol) &&
                       input$sum_boundary_tol >= 0) input$sum_boundary_tol else 0.1

    rows <- lapply(seq_len(nrow(chao)), function(i) {
      r            <- chao[i, ]
      sample_times <- r$sample_times[[1]]
      ref_vals     <- r$ref_continuous[[1]]
      int_vals     <- r$int_continuous[[1]]
      n_digits     <- r$n_digits

      ref_digits <- as.integer(strsplit(r$refline,  "")[[1]])
      int_digits <- as.integer(strsplit(r$interval, "")[[1]])

      row <- data.frame(
        tone              = r$tone,
        n_digits          = n_digits,
        # round() changes digit half-way between the reference lines, so the
        # ambiguous values are those near 1.5, 2.5, 3.5 and 4.5.
        ref_scaled_values = format_scaled_values_html(ref_vals, 1:4 + 0.5, boundary_tol),
        refline           = r$refline,
        int_scaled_values = format_scaled_values_html(int_vals, 1:4, boundary_tol),
        interval          = r$interval,
        ref_letters       = chao_to_letters(r$refline),
        int_letters       = chao_to_letters(r$interval),
        contour_shape     = r$shape,
        stringsAsFactors  = FALSE
      )

      # Wide per-sample columns (always 3 slots; NA for unused ones).
      max_digits <- 3
      for (j in seq_len(max_digits)) {
        if (j <= n_digits) {
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
      "raw_hz" = "Raw f0 (Hz)",
      "curated" = "Curated f0",
      "normalised" = "Normalised f0",
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
      tags$th(style = th_style, rowspan = 2, "Shape")
    ))

    header_row1 <- do.call(tags$tr, header_row1_cells)
    header_row2 <- do.call(tags$tr, header_row2_cells)

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

      cells <- c(cells, list(
        tags$td(style = td_style, result$contour_shape[i])
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
                              " &nbsp;(\u0394 = ", round(params$f0_max - params$f0_min, 2), ")")))
        ),
        tags$strong("Chao tone numeral comparison:"),
        tags$p(style = "font-size: 0.8rem; color: #888; margin: 4px 0 6px 0;",
          HTML(paste0("Values in <span style='color:#d9534f;font-weight:bold;'>red</span> are within \u00b1",
                      params$boundary_tol, " of a digit boundary: half-way between reference lines (1.5, 2.5, 3.5, 4.5), ",
                      "or the integers 1\u20134 for the interval-based method. These are sensitive to the choice of conversion method."))),
        tags$table(
          style = "margin-top: 4px; border-collapse: collapse; font-size: 0.84rem; width: 100%;",
          tags$thead(header_row1, header_row2),
          tags$tbody(body_rows)
        )
      )
    )
  })

  # --- Plots (panels: reference-line and interval) ---
  sum_plot_reactive <- reactive({
    req(sum_norm_contours())
    req(sum_result())

    pdat <- sum_norm_contours()
    result <- sum_result()
    params <- sum_params()

    # Build sampling point overlay data for both scales
    ref_point_rows <- list()
    int_point_rows <- list()
    for (i in seq_len(nrow(result))) {
      n_digits <- result$n_digits[i]
      for (j in 1:n_digits) {
        t_col <- paste0("t", j)
        if (t_col %in% names(result) && !is.na(result[[t_col]][i])) {
          # Reference-line points
          ref_point_rows[[length(ref_point_rows) + 1]] <- data.frame(
            time = result[[t_col]][i],
            chao_val = result[[paste0("v", j)]][i],
            chao_digit = as.character(result[[paste0("r", j)]][i]),
            tone = result$tone[i],
            stringsAsFactors = FALSE
          )
          # Interval points
          int_point_rows[[length(int_point_rows) + 1]] <- data.frame(
            time = result[[t_col]][i],
            chao_val = result[[paste0("iv", j)]][i],
            chao_digit = as.character(result[[paste0("ic", j)]][i]),
            tone = result$tone[i],
            stringsAsFactors = FALSE
          )
        }
      }
    }
    ref_point_df <- do.call(rbind, ref_point_rows)
    int_point_df <- do.call(rbind, int_point_rows)

    # Shared theme
    base_theme <- theme_bw(base_size = 13) +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5))

    # --- Plot 1: Reference-line [1, 5] ---
    ref_breaks <- 1:5
    ref_edges  <- 1:4 + 0.5   # round() changes digit half-way between lines
    tol <- if (is.null(params$boundary_tol)) 0.1 else params$boundary_tol
    p_ref <- ggplot(pdat, aes(x = time, y = chao_ref, colour = tone)) +
      annotate("rect", xmin = -Inf, xmax = Inf,
               ymin = ref_edges - tol, ymax = ref_edges + tol,
               fill = "grey90", alpha = 0.5) +
      geom_hline(yintercept = ref_breaks, linetype = "dashed", colour = "grey60", linewidth = 0.3) +
      geom_line(linewidth = 1.1) +
      geom_point(data = ref_point_df, aes(x = time, y = chao_val),
                 size = 3, shape = 16) +
      geom_text(data = ref_point_df, aes(x = time, y = chao_val, label = chao_digit),
                vjust = -1.3, size = 4, fontface = "bold", show.legend = FALSE) +
      scale_y_continuous(breaks = ref_breaks, limits = c(0.5, 5.5)) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
      labs(title = "Reference-line FOR [1, 5]", subtitle = "round()",
           x = "Normalised time", y = "Chao scale [1, 5]", colour = "Tone") +
      base_theme

    # --- Plot 2: Interval (0, 5] ---
    band_fills <- c("#f0f4ff", "#ffffff", "#f0f4ff", "#ffffff", "#f0f4ff")
    band_ymin  <- c(0, 1, 2, 3, 4)
    band_ymax  <- c(1, 2, 3, 4, 5)
    int_boundaries <- 1:4

    p_int <- ggplot(pdat, aes(x = time, y = chao_int, colour = tone)) +
      annotate("rect", xmin = -Inf, xmax = Inf,
               ymin = band_ymin, ymax = band_ymax,
               fill = band_fills, alpha = 0.6) +
      geom_hline(yintercept = int_boundaries, linetype = "dashed", colour = "grey70", linewidth = 0.3) +
      annotate("text", x = 1.06, y = c(0.5, 1.5, 2.5, 3.5, 4.5),
               label = as.character(1:5), size = 3.5, colour = "grey40", fontface = "bold") +
      geom_line(linewidth = 1.1) +
      geom_point(data = int_point_df, aes(x = time, y = chao_val),
                 size = 3, shape = 16) +
      geom_text(data = int_point_df, aes(x = time, y = chao_val, label = chao_digit),
                vjust = -1.3, size = 4, fontface = "bold", show.legend = FALSE) +
      scale_y_continuous(breaks = 0:5, limits = c(-0.3, 5.5)) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1.12)) +
      labs(title = "Interval-based FOR (0, 5]", subtitle = "ceiling()",
           x = "Normalised time", y = "Chao scale (0, 5]", colour = "Tone") +
      base_theme

    gridExtra::grid.arrange(p_ref, p_int, ncol = 2)
  })

  output$summarise_plot <- renderPlot({
    sum_plot_reactive()
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
      write.csv(export, file, row.names = FALSE)
      showNotification(paste("Data saved as", paste0(input$sum_filename, ".csv")),
                       type = "message", duration = 4)
    }
  )

  output$sum_download_plot <- downloadHandler(
    filename = function() paste0(input$sum_filename, ".png"),
    content = function(file) {
      req(sum_norm_contours())
      png(file, width = 12, height = 5, units = "in", res = 300)
      sum_plot_reactive()
      dev.off()
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
        '# higher and lower voices are not averaged together in Hz, then take the\n',
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
      'pred$chao_ref <- (pred$f0_predicted - f0_min) / (f0_max - f0_min) * 4 + 1\n',
      'pred$chao_int <- (pred$f0_predicted - f0_min) / (f0_max - f0_min) * 5\n',
      '\n# Step 3: Per tone, sample the onset, the offset and any turning point, then convert\n',
      '# A turning point is the lowest (dip) or highest (peak) point between 10% and 90%\n',
      '# of the time axis on the reference-line scale. It adds a middle digit when it lies\n',
      '# more than `threshold` Chao units below (dip) or above (peak) both endpoints.\n',
      'threshold <- 0.5\n',
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
      '  has_dip  <- onset - fine_ref[i_min] > threshold && offset - fine_ref[i_min] > threshold\n',
      '  has_peak <- fine_ref[i_max] - onset > threshold && fine_ref[i_max] - offset > threshold\n',
      '  sample_times <- if (has_dip) c(0, fine_time[i_min], 1) else\n',
      '                  if (has_peak) c(0, fine_time[i_max], 1) else c(0, 1)\n\n',
      '  # Eq. 3 [1,5]: round to nearest integer\n',
      '  ref_vals <- fn_ref(sample_times)\n',
      '  refline <- paste0(pmax(1, pmin(5, round(ref_vals))), collapse = "")\n\n',
      '  # Eq. 1a (0,5]: ceiling -> Chao digits 1-5\n',
      '  int_vals <- fn_int(sample_times)\n',
      '  interval <- paste0(pmax(1, pmin(5, ceiling(int_vals))), collapse = "")\n',
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
