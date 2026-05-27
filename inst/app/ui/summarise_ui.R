###############################################
# Summarise tab — Chao Tone Digit Conversion
###############################################

summarise_ui <- function(input, output, session, dataset, normalised_data, gca_pred_data, gamm_pred_data) {

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

  # --- Guide text ---
  output$summarise_guide <- renderUI({
    code_style <- "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;"
    tagList(
      tags$div(
        id = "summarise_guide",
        style = "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;",
        tags$strong("Chao tone numeral guide:"),
        tags$p(style = "margin-bottom: 8px;",
          "Chao tone numerals describe tone contours using a 5-level pitch scale (1 = lowest, 5 = highest). ",
          "For example, Mandarin T1 = ", tags$code(style = code_style, "55"),
          " (high level), T2 = ", tags$code(style = code_style, "35"),
          " (mid rising), T3 = ", tags$code(style = code_style, "214"),
          " (low dipping), T4 = ", tags$code(style = code_style, "51"),
          " (high falling)."
        ),
        tags$p(style = "margin-bottom: 8px;",
          "All three methods use ",
          tags$strong("Fraction of Range (FOR)"),
          " normalisation, which maps f0 onto a bounded scale representing the speaker\u2019s pitch range (min/max). They differ in how Tone Numerals (i.e. 1\u20135) are mapped and how pitch range is defined."
        ),
        tags$strong("Conversion methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(
            tags$strong("Reference-line FOR [1, 5]:"),
            tags$div(style = "margin: 4px 0 2px 0; font-family: 'Georgia', serif; font-style: italic; font-size: 0.92rem;",
              HTML("f\u2080\u2032 = (f\u2080 \u2212 f\u2080<sub>min</sub>) / (f\u2080<sub>max</sub> \u2212 f\u2080<sub>min</sub>) \u00d7 4 + 1")
            ),
            " Scales f0 onto [1, 5] with 5 reference lines at integer values. Each sampled value is ", tags$code(style = code_style, "round()"), "ed to the nearest integer. ",
            tags$em("Recommended input: model predictions.")
          ),
          tags$li(
            tags$strong("Interval-based FOR (0, 5]:"),
            tags$div(style = "margin: 4px 0 2px 0; font-family: 'Georgia', serif; font-style: italic; font-size: 0.92rem;",
              HTML("f\u2080\u2032 = (f\u2080 \u2212 f\u2080<sub>min</sub>) / (f\u2080<sub>max</sub> \u2212 f\u2080<sub>min</sub>) \u00d7 5")
            ),
            " Scales f0 onto (0, 5] with 5 equal-width intervals. Each sampled value is assigned to an interval via ", tags$code(style = code_style, "ceiling()"),
            ": (0,1]\u21921, (1,2]\u21922, (2,3]\u21923, (3,4]\u21924, (4,5]\u21925. ",
            tags$em("Recommended input: model predictions.")
          ),
          tags$li(
            tags$strong("Robust FOR (0, 5]:"),
            tags$div(style = "margin: 4px 0 2px 0; font-family: 'Georgia', serif; font-style: italic; font-size: 0.92rem;",
              HTML("f\u2080\u2032 = (f\u2080 \u2212 (\u03bc<sub>min</sub> \u2212 \u03c3<sub>min</sub>)) / ((\u03bc<sub>max</sub> + \u03c3<sub>max</sub>) \u2212 (\u03bc<sub>min</sub> \u2212 \u03c3<sub>min</sub>)) \u00d7 5")
            ),
            " Same interval-based conversion, but the range is defined by \u03bc \u00b1 \u03c3 of per-token f0 peaks from the highest-pitched tone (max) and per-token f0 valleys from the lowest-pitched tone (min). ",
            tags$em("Works with raw f0 data or normalised f0 data.")
          )
        ),
        tags$p(style = "margin: 4px 0 8px 0; font-size: 0.84rem; color: #777;",
          HTML("Note: Within a given conversion system, scaled values near integer boundaries can be ambiguous. For example, in the interval-based system a scaled value of 1.1 could reasonably be assigned to digit 1 or 2, depending on the tonal contrasts in the language. Values within the fuzzy boundary threshold (\u00b1<em>t</em>) are highlighted in <span style='color:#d9534f; font-weight:bold;'>red</span>. For more discussion on conversion methods and boundary handling, see Xu &amp; Zhang (2024).")
        ),
        tags$strong("Data source:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Raw data mean:"), " Always available. Computes per-tone mean f0 contours from the uploaded or normalised data."),
          tags$li(tags$strong("GCA / GAMM predictions:"), " Available after fitting the respective model. Uses the population-level predicted contours (smoother, more representative). Note: Robust FOR is not available for model predictions.")
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

    # Build source choices dynamically
    src_choices <- c()
    if (has_data) src_choices <- c(src_choices, "Raw f0 (Hz)" = "raw_hz")
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

    # Column info for normalised data
    norm_data <- normalised_data()
    norm_vars <- if (!is.null(norm_data)) names(norm_data) else c("No dataset available")
    norm_types <- if (!is.null(norm_data)) sapply(norm_data, class) else rep("NA", length(norm_vars))
    norm_labels <- paste0(norm_vars, " {", norm_types, "}")

    # Smart defaults for normalised data
    norm_token_default <- guess_var(norm_vars, c("^token", "^item", "^segment", "^id"), 1)
    norm_f0_default    <- guess_var(norm_vars, c("^f0_st$", "^f0_zscore$", "^f0_normalised$", "^f0_norm", "^f0$", "^f0_"), 2)
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
                      selected = raw_tone_default)
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

    # --- Step 1: Get contour data + raw column names for robust FOR -------
    raw_data_for_robust <- NULL
    raw_token <- raw_f0 <- raw_tone <- NULL

    if (src == "gca") {
      contour_data <- gca_pred_data()
    } else if (src == "gamm") {
      contour_data <- gamm_pred_data()
    } else if (src == "raw_hz") {
      raw_data_for_robust <- dataset()
      req(raw_data_for_robust)
      req(input$sum_raw_token_var, input$sum_raw_f0_var,
          input$sum_raw_time_var,  input$sum_raw_tone_var)
      raw_token <- input$sum_raw_token_var
      raw_f0    <- input$sum_raw_f0_var
      raw_tone  <- input$sum_raw_tone_var
      contour_data <- compute_mean_contour(
        raw_data_for_robust,
        token = raw_token, f0 = raw_f0,
        time  = input$sum_raw_time_var, tone = raw_tone
      )
    } else if (src == "normalised") {
      raw_data_for_robust <- normalised_data()
      req(raw_data_for_robust)
      req(input$sum_norm_token_var, input$sum_norm_f0_var,
          input$sum_norm_time_var,  input$sum_norm_tone_var)
      raw_token <- input$sum_norm_token_var
      raw_f0    <- input$sum_norm_f0_var
      raw_tone  <- input$sum_norm_tone_var
      contour_data <- compute_mean_contour(
        raw_data_for_robust,
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
    # detection, the three numeral methods, and (when raw_data is given)
    # the robust FOR. Stats are returned as attributes.
    chao <- contour_to_chao(
      contour_data,
      tone_col  = "tone", time_col = "time", f0_col = "f0_predicted",
      threshold = 0.5,
      raw_data  = raw_data_for_robust,
      raw_token = raw_token, raw_f0 = raw_f0, raw_tone = raw_tone
    )
    f0_min <- attr(chao, "f0_min")
    f0_max <- attr(chao, "f0_max")
    rstats <- attr(chao, "robust_stats")
    has_1b <- !is.null(rstats)

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
    if (has_1b) {
      lr <- log(rstats$upper) - log(rstats$lower)
      contour_data$chao_1b <- pmax(0, pmin(5,
        (log(contour_data$f0_predicted) - log(rstats$lower)) / lr * 5))
    } else {
      contour_data$chao_1b <- NA_real_
    }
    sum_norm_contours(contour_data)

    # --- Step 4: Reshape chao result into the wide UI-display format ------
    boundary_tol <- if (!is.null(input$sum_boundary_tol) &&
                       input$sum_boundary_tol >= 0) input$sum_boundary_tol else 0.1

    rows <- lapply(seq_len(nrow(chao)), function(i) {
      r            <- chao[i, ]
      sample_times <- r$sample_times[[1]]
      ref_vals     <- r$ref_continuous[[1]]
      int_vals     <- r$int_continuous[[1]]
      rob_vals     <- r$rob_continuous[[1]]
      n_digits     <- r$n_digits

      ref_digits <- as.integer(strsplit(r$refline,  "")[[1]])
      int_digits <- as.integer(strsplit(r$interval, "")[[1]])
      rob_digits <- if (!is.na(r$robust))
                      as.integer(strsplit(r$robust, "")[[1]])
                    else
                      rep(NA_integer_, n_digits)

      row <- data.frame(
        tone              = r$tone,
        n_digits          = n_digits,
        ref_scaled_values = format_scaled_values_html(ref_vals, 1:5, boundary_tol),
        refline           = r$refline,
        int_scaled_values = format_scaled_values_html(int_vals, 1:4, boundary_tol),
        interval          = r$interval,
        b_scaled_values   = if (has_1b)
                              format_scaled_values_html(rob_vals, 1:4, boundary_tol)
                            else NA_character_,
        numeral_1b        = r$robust,
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
          row[[paste0("bv", j)]] <- rob_vals[j]
          row[[paste0("bc", j)]] <- rob_digits[j]
        } else {
          row[[paste0("t",  j)]] <- NA_real_
          row[[paste0("v",  j)]] <- NA_real_
          row[[paste0("r",  j)]] <- NA_real_
          row[[paste0("iv", j)]] <- NA_real_
          row[[paste0("ic", j)]] <- NA_real_
          row[[paste0("bv", j)]] <- NA_real_
          row[[paste0("bc", j)]] <- NA_real_
        }
      }
      row
    })

    sum_result(do.call(rbind, rows))

    sum_params(list(
      source       = src,
      f0_min       = f0_min,
      f0_max       = f0_max,
      has_1b       = has_1b,
      highest_tone = if (has_1b) rstats$highest_tone else NA,
      lowest_tone  = if (has_1b) rstats$lowest_tone  else NA,
      mu_max_1b    = if (has_1b) rstats$mu_max       else NA,
      sigma_max_1b = if (has_1b) rstats$sigma_max    else NA,
      mu_min_1b    = if (has_1b) rstats$mu_min       else NA,
      sigma_min_1b = if (has_1b) rstats$sigma_min    else NA,
      upper_1b     = if (has_1b) rstats$upper        else NA,
      lower_1b     = if (has_1b) rstats$lower        else NA
    ))
  })

  # --- Summary box ---
  output$summarise_summary <- renderUI({
    req(sum_result())
    result <- sum_result()
    params <- sum_params()

    src_label <- switch(params$source,
      "raw_hz" = "Raw f0 (Hz)",
      "normalised" = "Normalised f0",
      "gca" = "GCA predictions",
      "gamm" = "GAMM predictions"
    )
    # Shared table styles
    th_style <- "padding: 6px 8px; border-bottom: 2px solid #ddd; text-align: center; font-size: 0.82rem;"
    th_group <- "padding: 6px 8px; border-bottom: 1px solid #ccc; text-align: center; font-size: 0.82rem; font-weight: bold;"
    td_style <- "padding: 5px 8px; text-align: center; font-size: 0.82rem;"
    td_style_mono <- "padding: 5px 8px; text-align: left; font-family: monospace; font-size: 0.82rem;"
    td_style_bold <- "padding: 5px 8px; text-align: center; font-size: 1rem; font-weight: bold; font-family: monospace;"

    # Build side-by-side comparison table with two-row header
    show_1b <- isTRUE(params$has_1b)

    header_row1_cells <- list(
      tags$th(style = paste0(th_style, " border-right: 1px solid #ddd;"), rowspan = 2, "Tone"),
      tags$th(style = paste0(th_group, " background-color: #f0f7ff; border-right: 1px solid #ddd;"), colspan = 2,
              HTML("Reference-line FOR [1, 5]<br><span style='font-weight:normal; font-size:0.72rem; color:#888;'>\u00d7 4 + 1, round()</span>")),
      tags$th(style = paste0(th_group, " background-color: #fff8f0; border-right: 1px solid #ddd;"), colspan = 2,
              HTML("Interval-based FOR (0, 5]<br><span style='font-weight:normal; font-size:0.72rem; color:#888;'>\u00d7 5, ceiling()</span>"))
    )
    header_row2_cells <- list(
      tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #f0f7ff;"), "Scaled"),
      tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #f0f7ff; border-right: 1px solid #ddd;"),
              HTML("round()")),
      tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #fff8f0;"), "Scaled"),
      tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #fff8f0; border-right: 1px solid #ddd;"),
              HTML("ceiling()"))
    )

    if (show_1b) {
      header_row1_cells <- c(header_row1_cells, list(
        tags$th(style = paste0(th_group, " background-color: #f0fff0; border-right: 1px solid #ddd;"), colspan = 2,
                HTML("Robust FOR (0, 5]<br><span style='font-weight:normal; font-size:0.72rem; color:#888;'>\u03bc\u00b1\u03c3 range, ceiling()</span>"))
      ))
      header_row2_cells <- c(header_row2_cells, list(
        tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #f0fff0;"), "Scaled"),
        tags$th(style = paste0(th_style, " font-size: 0.76rem; background-color: #f0fff0; border-right: 1px solid #ddd;"),
                HTML("ceiling()"))
      ))
    }

    header_row1_cells <- c(header_row1_cells, list(
      tags$th(style = th_style, rowspan = 2, "Shape")
    ))

    header_row1 <- do.call(tags$tr, header_row1_cells)
    header_row2 <- do.call(tags$tr, header_row2_cells)

    body_rows <- lapply(seq_len(nrow(result)), function(i) {
      # Highlight where methods disagree (compare all available methods)
      all_numerals <- c(result$refline[i], result$interval[i])
      if (show_1b && !is.na(result$numeral_1b[i])) all_numerals <- c(all_numerals, result$numeral_1b[i])
      any_disagree <- length(unique(all_numerals)) > 1
      hl_ref <- if (any_disagree) "background-color: #e8f0fe;" else ""
      hl_int <- if (any_disagree) "background-color: #fff3cd;" else ""
      hl_1b  <- if (any_disagree) "background-color: #e8f8e8;" else ""

      cells <- list(
        tags$td(style = paste0(td_style, " font-weight: bold; border-right: 1px solid #ddd;"), result$tone[i]),
        tags$td(style = paste0(td_style_mono, " background-color: #f8fbff;"), HTML(result$ref_scaled_values[i])),
        tags$td(style = paste0(td_style_bold, " background-color: #f8fbff; border-right: 1px solid #ddd; ", hl_ref), result$refline[i]),
        tags$td(style = paste0(td_style_mono, " background-color: #fffcf8;"), HTML(result$int_scaled_values[i])),
        tags$td(style = paste0(td_style_bold, " background-color: #fffcf8; border-right: 1px solid #ddd; ", hl_int), result$interval[i])
      )

      if (show_1b) {
        cells <- c(cells, list(
          tags$td(style = paste0(td_style_mono, " background-color: #f8fff8;"), HTML(result$b_scaled_values[i])),
          tags$td(style = paste0(td_style_bold, " background-color: #f8fff8; border-right: 1px solid #ddd; ", hl_1b), result$numeral_1b[i])
        ))
      }

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
          tags$li(HTML(paste0("Reference-line & Interval FOR, f0 range (min/max): ", round(params$f0_min, 2), " \u2013 ", round(params$f0_max, 2),
                              " Hz &nbsp;(\u0394 = ", round(params$f0_max - params$f0_min, 2), ")"))),
          if (show_1b) tags$li(HTML(paste0(
            "Robust FOR, f0 range (\u03bc\u00b1\u03c3): ",
            round(params$lower_1b, 2), " \u2013 ", round(params$upper_1b, 2),
            " Hz &nbsp;(lowest tone: ", params$lowest_tone,
            ", \u03bc=", round(params$mu_min_1b, 1),
            ", \u03c3=", round(params$sigma_min_1b, 1),
            "; highest tone: ", params$highest_tone,
            ", \u03bc=", round(params$mu_max_1b, 1),
            ", \u03c3=", round(params$sigma_max_1b, 1), ")"
          )))
        ),
        tags$strong("Chao tone numeral comparison:"),
        tags$p(style = "font-size: 0.8rem; color: #888; margin: 4px 0 6px 0;",
          HTML("Values in <span style='color:#d9534f;font-weight:bold;'>red</span> are within \u00b10.1 of a boundary. These are sensitive to the choice of conversion method.")),
        tags$table(
          style = "margin-top: 4px; border-collapse: collapse; font-size: 0.84rem; width: 100%;",
          tags$thead(header_row1, header_row2),
          tags$tbody(body_rows)
        )
      )
    )
  })

  # --- Plots (panels: reference-line, interval, and optionally 1b) ---
  sum_plot_reactive <- reactive({
    req(sum_norm_contours())
    req(sum_result())

    pdat <- sum_norm_contours()
    result <- sum_result()
    params <- sum_params()
    show_1b <- isTRUE(params$has_1b)

    # Build sampling point overlay data for all scales
    ref_point_rows <- list()
    int_point_rows <- list()
    b_point_rows <- list()
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
          # 1b points
          if (show_1b && paste0("bv", j) %in% names(result) && !is.na(result[[paste0("bv", j)]][i])) {
            b_point_rows[[length(b_point_rows) + 1]] <- data.frame(
              time = result[[t_col]][i],
              chao_val = result[[paste0("bv", j)]][i],
              chao_digit = as.character(result[[paste0("bc", j)]][i]),
              tone = result$tone[i],
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    ref_point_df <- do.call(rbind, ref_point_rows)
    int_point_df <- do.call(rbind, int_point_rows)
    b_point_df <- if (length(b_point_rows) > 0) do.call(rbind, b_point_rows) else NULL

    # Shared theme (smaller text when 3 panels)
    base_sz <- if (show_1b) 11 else 13
    base_theme <- theme_bw(base_size = base_sz) +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = if (show_1b) 10 else 12, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = if (show_1b) 8.5 else 10, hjust = 0.5))

    # --- Plot 1: Reference-line [1, 5] ---
    ref_breaks <- 1:5
    p_ref <- ggplot(pdat, aes(x = time, y = chao_ref, colour = tone)) +
      annotate("rect", xmin = -Inf, xmax = Inf,
               ymin = ref_breaks - 0.1, ymax = ref_breaks + 0.1,
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

    if (show_1b && !is.null(b_point_df)) {
      # --- Plot 3: 1b μ±σ range (0, 5] ---
      # Same band shading as interval but with green tint
      band_fills_1b <- c("#f0fff0", "#ffffff", "#f0fff0", "#ffffff", "#f0fff0")

      p_1b <- ggplot(pdat, aes(x = time, y = chao_1b, colour = tone)) +
        annotate("rect", xmin = -Inf, xmax = Inf,
                 ymin = band_ymin, ymax = band_ymax,
                 fill = band_fills_1b, alpha = 0.6) +
        geom_hline(yintercept = int_boundaries, linetype = "dashed", colour = "grey70", linewidth = 0.3) +
        annotate("text", x = 1.06, y = c(0.5, 1.5, 2.5, 3.5, 4.5),
                 label = as.character(1:5), size = 3.5, colour = "grey40", fontface = "bold") +
        geom_line(linewidth = 1.1) +
        geom_point(data = b_point_df, aes(x = time, y = chao_val),
                   size = 3, shape = 16) +
        geom_text(data = b_point_df, aes(x = time, y = chao_val, label = chao_digit),
                  vjust = -1.3, size = 4, fontface = "bold", show.legend = FALSE) +
        scale_y_continuous(breaks = 0:5, limits = c(-0.3, 5.5)) +
        scale_x_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1.12)) +
        labs(title = "Robust FOR (0, 5]",
             subtitle = expression(paste(mu, "\u00b1", sigma, " range, ceiling()")),
             x = "Normalised time", y = "Chao scale (0, 5]", colour = "Tone") +
        base_theme

      gridExtra::grid.arrange(p_ref, p_int, p_1b, ncol = 3)
    } else {
      gridExtra::grid.arrange(p_ref, p_int, ncol = 2)
    }
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
      params <- sum_params()
      show_1b <- isTRUE(params$has_1b)
      export <- data.frame(
        Tone = result$tone,
        `Ref Scaled [1,5]` = clean_ref_scaled,
        `Ref Numeral` = result$refline,
        `Int Scaled (0,5]` = clean_int_scaled,
        `Int Numeral` = result$interval,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      if (show_1b) {
        clean_1b_scaled <- gsub("<[^>]+>", "", result$b_scaled_values)
        export$`1b Scaled (0,5]` <- clean_1b_scaled
        export$`1b Numeral` <- result$numeral_1b
      }
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
      params <- sum_params()
      plot_width <- if (isTRUE(params$has_1b)) 16 else 12
      png(file, width = plot_width, height = 5, units = "in", res = 300)
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
    show_1b <- isTRUE(params$has_1b)

    code_1b <- if (show_1b) paste0(
      '\n# --- Method 1b: mu +/- sigma range (requires raw token data) ---\n',
      '# raw_data: data frame with columns: token, f0, time, tone\n',
      '# Normalise time per token to [0, 1]\n',
      'raw_data <- raw_data %>%\n',
      '  filter(!is.na(f0) & f0 > 0) %>%\n',
      '  group_by(token) %>%\n',
      '  mutate(time_norm = (time - min(time)) / (max(time) - min(time))) %>%\n',
      '  ungroup()\n',
      'raw_data$time_bin <- round(raw_data$time_norm * 49) / 49\n\n',
      '# Find time bins with highest/lowest mean f0\n',
      'bin_means <- raw_data %>%\n',
      '  group_by(time_bin) %>%\n',
      '  summarise(mean_f0 = mean(f0, na.rm = TRUE))\n',
      'max_bin <- bin_means$time_bin[which.max(bin_means$mean_f0)]\n',
      'min_bin <- bin_means$time_bin[which.min(bin_means$mean_f0)]\n\n',
      '# Compute mu and sigma at extreme time bins\n',
      'mu_max  <- mean(raw_data$f0[raw_data$time_bin == max_bin])\n',
      'sigma_max <- sd(raw_data$f0[raw_data$time_bin == max_bin])\n',
      'mu_min  <- mean(raw_data$f0[raw_data$time_bin == min_bin])\n',
      'sigma_min <- sd(raw_data$f0[raw_data$time_bin == min_bin])\n\n',
      '# Eq. 1b: f0\' = (log(f0) - log(mu_min - sigma_min)) /\n',
      '#                (log(mu_max + sigma_max) - log(mu_min - sigma_min)) * 5\n',
      'upper_1b <- mu_max + sigma_max\n',
      'lower_1b <- max(mu_min - sigma_min, 1)\n',
      'pred$chao_1b <- (log(pred$f0_predicted) - log(lower_1b)) /\n',
      '               (log(upper_1b) - log(lower_1b)) * 5\n'
    ) else ""

    code_1b_convert <- if (show_1b) paste0(
      '\n  # Method 1b: ceiling on mu+/-sigma (0,5] scale\n',
      '  fn_1b <- approxfun(td$time, td$chao_1b, rule = 2)\n',
      '  b_vals <- c(fn_1b(0), fn_1b(1))\n',
      '  numeral_1b <- paste0(pmax(1, pmin(5, ceiling(b_vals))), collapse = "")\n'
    ) else ""

    code_1b_df <- if (show_1b) paste0(
      ',\n             b_scaled = paste(round(b_vals, 2), collapse = " -> "),\n',
      '             numeral_1b = numeral_1b'
    ) else ""

    code_text <- paste0(
      'library(dplyr)\nlibrary(ggplot2)\n\n',
      '# pred is a data frame with columns: time, f0_predicted, tone\n',
      '# (from GCA/GAMM predictions or per-tone mean contours)\n\n',
      '# Step 1: Define pitch range\n',
      'f0_min <- min(pred$f0_predicted, na.rm = TRUE)\n',
      'f0_max <- max(pred$f0_predicted, na.rm = TRUE)\n\n',
      '# Step 2: Fraction of Range normalisation\n',
      '# Eq. 3 (reference-line): f0\' = (log(f0) - log(f0_min)) / (log(f0_max) - log(f0_min)) * 4 + 1\n',
      '# Eq. 1a (interval):      f0\' = (log(f0) - log(f0_min)) / (log(f0_max) - log(f0_min)) * 5\n',
      'pred$chao_ref <- (pred$f0_predicted - f0_min) / (f0_max - f0_min) * 4 + 1\n',
      'pred$chao_int <- (pred$f0_predicted - f0_min) / (f0_max - f0_min) * 5\n',
      code_1b,
      '\n# Step 3: Sample onset and offset per tone, convert\n',
      'tone_levels <- unique(pred$tone)\n',
      'results <- lapply(tone_levels, function(tone_val) {\n',
      '  td <- pred[pred$tone == tone_val, ]\n',
      '  td <- td[order(td$time), ]\n',
      '  fn_ref <- approxfun(td$time, td$chao_ref, rule = 2)\n',
      '  fn_int <- approxfun(td$time, td$chao_int, rule = 2)\n\n',
      '  # Eq. 3 [1,5]: round to nearest integer\n',
      '  ref_vals <- c(fn_ref(0), fn_ref(1))\n',
      '  refline <- paste0(pmax(1, pmin(5, round(ref_vals))), collapse = "")\n\n',
      '  # Eq. 1a (0,5]: ceiling -> Chao digits 1-5\n',
      '  int_vals <- c(fn_int(0), fn_int(1))\n',
      '  interval <- paste0(pmax(1, pmin(5, ceiling(int_vals))), collapse = "")\n',
      code_1b_convert,
      '\n  data.frame(tone = tone_val,\n',
      '             ref_scaled = paste(round(ref_vals, 2), collapse = " -> "),\n',
      '             refline = refline,\n',
      '             int_scaled = paste(round(int_vals, 2), collapse = " -> "),\n',
      '             interval = interval',
      code_1b_df, ')\n',
      '})\n',
      'chao_summary <- do.call(rbind, results)\n',
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
