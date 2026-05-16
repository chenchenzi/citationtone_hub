###############################################
# Summarise tab — Chao Tone Digit Conversion
###############################################

summarise_ui <- function(input, output, session, dataset, normalised_data, gca_pred_data, gamm_pred_data) {

  # --- Helper: classify contour shape from Chao string ---
  classify_contour <- function(chao_str) {
    digits <- as.integer(strsplit(as.character(chao_str), "")[[1]])
    n <- length(digits)
    if (n < 2) return("level")

    first <- digits[1]
    last <- digits[n]

    if (all(digits == digits[1])) {
      level_names <- c("1" = "low level", "2" = "mid-low level", "3" = "mid level",
                       "4" = "mid-high level", "5" = "high level")
      return(unname(level_names[as.character(digits[1])]))
    }

    # Check for dipping/peaking (n >= 3)
    if (n >= 3) {
      interior <- digits[2:(n - 1)]
      if (min(interior) < first && min(interior) < last) return("dipping")
      if (max(interior) > first && max(interior) > last) return("peaking")
    }

    if (last > first) return("rising")
    if (last < first) return("falling")
    return("level")
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

    # Helper: guess best column match by name patterns
    guess_var <- function(vars, patterns, fallback_idx = 1) {
      for (pat in patterns) {
        match <- grep(pat, vars, ignore.case = TRUE, value = TRUE)
        if (length(match) > 0) return(match[1])
      }
      if (fallback_idx <= length(vars)) vars[fallback_idx] else vars[1]
    }

    # Column info for raw data
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
    norm_f0_default    <- guess_var(norm_vars, c("^f0_normalised$", "^f0_norm", "^f0$", "^f0_"), 2)
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
                  value = "chao_summary"),
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

  # --- Helper: compute per-tone mean contour from a dataset ---
  compute_mean_contour <- function(data, token_var, f0_var, time_var, tone_var) {
    # Normalise time to [0, 1] per token
    dat <- data %>%
      group_by(.data[[token_var]]) %>%
      mutate(
        .time_norm = {
          t_raw <- .data[[time_var]]
          t_min <- min(t_raw, na.rm = TRUE)
          t_max <- max(t_raw, na.rm = TRUE)
          if (t_max == t_min) rep(0.5, n()) else (t_raw - t_min) / (t_max - t_min)
        }
      ) %>%
      ungroup()

    # Bin time and compute per-tone mean f0
    n_bins <- 50
    dat$.time_bin <- round(dat$.time_norm * (n_bins - 1)) / (n_bins - 1)
    dat$.f0 <- as.numeric(dat[[f0_var]])
    dat$.tone <- as.character(dat[[tone_var]])

    dat %>%
      group_by(tone = .tone, time = .time_bin) %>%
      summarise(f0_predicted = mean(.f0, na.rm = TRUE), .groups = "drop") %>%
      arrange(tone, time) %>%
      as.data.frame()
  }

  # --- Core conversion ---
  observeEvent(input$sum_button, {
    src <- input$sum_source

    if (is.null(src) || src == "none") {
      showNotification("No data available. Upload a CSV file first.",
                       type = "warning", duration = 5)
      return()
    }

    # --- Step 1: Get contour data ---
    if (src == "gca") {
      contour_data <- gca_pred_data()
    } else if (src == "gamm") {
      contour_data <- gamm_pred_data()
    } else if (src == "raw_hz") {
      data <- dataset()
      req(data)
      req(input$sum_raw_token_var, input$sum_raw_f0_var, input$sum_raw_time_var, input$sum_raw_tone_var)
      contour_data <- compute_mean_contour(
        data, input$sum_raw_token_var, input$sum_raw_f0_var,
        input$sum_raw_time_var, input$sum_raw_tone_var
      )
    } else if (src == "normalised") {
      data <- normalised_data()
      req(data)
      req(input$sum_norm_token_var, input$sum_norm_f0_var, input$sum_norm_time_var, input$sum_norm_tone_var)
      contour_data <- compute_mean_contour(
        data, input$sum_norm_token_var, input$sum_norm_f0_var,
        input$sum_norm_time_var, input$sum_norm_tone_var
      )
    } else {
      contour_data <- NULL
    }

    if (is.null(contour_data) || nrow(contour_data) == 0) {
      showNotification("No contour data available. Check variable selections or fit a model first.",
                       type = "warning", duration = 5)
      return()
    }

    # --- Step 2: Rescale to Chao scale (both methods) ---
    f0_min <- min(contour_data$f0_predicted, na.rm = TRUE)
    f0_max <- max(contour_data$f0_predicted, na.rm = TRUE)

    if (f0_max == f0_min) {
      contour_data$chao_ref <- 3       # [1,5] midpoint
      contour_data$chao_int <- 2.5     # (0,5] midpoint
    } else {
      contour_data$chao_ref <- (contour_data$f0_predicted - f0_min) / (f0_max - f0_min) * 4 + 1  # [1,5]
      contour_data$chao_int <- (contour_data$f0_predicted - f0_min) / (f0_max - f0_min) * 5      # (0,5]
    }

    # Use [1,5] reference-line scale for plotting (more standard)
    contour_data$chao_continuous <- contour_data$chao_ref

    # --- Robust FOR: range from highest/lowest tone extremes (only for raw data) ---
    has_1b <- src %in% c("raw_hz", "normalised")
    if (has_1b) {
      # Get raw token-level data for computing per-tone peak/valley μ and σ
      if (src == "raw_hz") {
        raw_1b_data <- dataset()
        f0_var_1b   <- input$sum_raw_f0_var
        tone_var_1b <- input$sum_raw_tone_var
        token_var_1b <- input$sum_raw_token_var
      } else {
        raw_1b_data <- normalised_data()
        f0_var_1b   <- input$sum_norm_f0_var
        tone_var_1b <- input$sum_norm_tone_var
        token_var_1b <- input$sum_norm_token_var
      }

      # Filter valid f0
      raw_1b <- raw_1b_data %>%
        mutate(.f0 = as.numeric(.data[[f0_var_1b]])) %>%
        filter(!is.na(.f0) & .f0 > 0)

      # Compute per-tone mean f0 to identify the highest and lowest tones
      tone_means <- raw_1b %>%
        group_by(.data[[tone_var_1b]]) %>%
        summarise(.tone_mean = mean(.f0, na.rm = TRUE), .groups = "drop")

      highest_tone <- tone_means[[tone_var_1b]][which.max(tone_means$.tone_mean)]
      lowest_tone  <- tone_means[[tone_var_1b]][which.min(tone_means$.tone_mean)]

      # Per-token f0 peaks from the highest-pitched tone → μ_max, σ_max
      token_peaks <- raw_1b %>%
        filter(.data[[tone_var_1b]] == highest_tone) %>%
        group_by(.data[[token_var_1b]]) %>%
        summarise(.f0_peak = max(.f0, na.rm = TRUE), .groups = "drop")
      mu_max_1b    <- mean(token_peaks$.f0_peak, na.rm = TRUE)
      sigma_max_1b <- sd(token_peaks$.f0_peak, na.rm = TRUE)

      # Per-token f0 valleys from the lowest-pitched tone → μ_min, σ_min
      token_valleys <- raw_1b %>%
        filter(.data[[tone_var_1b]] == lowest_tone) %>%
        group_by(.data[[token_var_1b]]) %>%
        summarise(.f0_valley = min(.f0, na.rm = TRUE), .groups = "drop")
      mu_min_1b    <- mean(token_valleys$.f0_valley, na.rm = TRUE)
      sigma_min_1b <- sd(token_valleys$.f0_valley, na.rm = TRUE)

      upper_1b <- mu_max_1b + sigma_max_1b
      lower_1b <- max(mu_min_1b - sigma_min_1b, 1)  # floor at 1 Hz to avoid log(0)

      # Apply tone-specific FOR formula to the per-tone mean contour
      log_range_1b <- log(upper_1b) - log(lower_1b)
      if (log_range_1b > 0) {
        contour_data$chao_1b <- pmax(0, pmin(5,
          (log(contour_data$f0_predicted) - log(lower_1b)) / log_range_1b * 5
        ))  # clamp to [0, 5] per Zhu (1999)
      } else {
        contour_data$chao_1b <- 2.5  # fallback
        has_1b <- FALSE
      }
    } else {
      contour_data$chao_1b <- NA_real_
      mu_max_1b <- sigma_max_1b <- mu_min_1b <- sigma_min_1b <- upper_1b <- lower_1b <- NA_real_
    }

    # Store normalised contours for plotting
    sum_norm_contours(contour_data)

    # --- Step 3: Auto-detect digit count + sample + convert (both methods) ---
    tone_levels <- unique(contour_data$tone)
    threshold <- 0.5  # Chao units threshold for turning point detection (on [1,5] scale)
    boundary_tol <- if (!is.null(input$sum_boundary_tol) && input$sum_boundary_tol >= 0) input$sum_boundary_tol else 0.1

    results <- list()
    for (tone_val in tone_levels) {
      td <- contour_data[contour_data$tone == tone_val, ]
      td <- td[order(td$time), ]

      # Skip tones with too few data points
      td <- td[!is.na(td$chao_ref) & !is.na(td$time), ]
      if (nrow(td) < 2) next

      # Interpolation functions for both scales
      interp_ref <- approxfun(td$time, td$chao_ref, rule = 2)
      interp_int <- approxfun(td$time, td$chao_int, rule = 2)

      # Get onset and offset values on [1,5] scale (for turning point detection)
      onset_ref <- interp_ref(0)
      offset_ref <- interp_ref(1)

      # Detect turning point on [1,5] scale
      fine_time <- seq(0, 1, length.out = 200)
      fine_ref <- interp_ref(fine_time)

      interior_min_idx <- which.min(fine_ref[20:180]) + 19
      interior_max_idx <- which.max(fine_ref[20:180]) + 19
      interior_min <- fine_ref[interior_min_idx]
      interior_max <- fine_ref[interior_max_idx]

      has_dip <- (onset_ref - interior_min > threshold) && (offset_ref - interior_min > threshold)
      has_peak <- (interior_max - onset_ref > threshold) && (interior_max - offset_ref > threshold)

      if (has_dip) {
        tp_time <- fine_time[interior_min_idx]
        sample_times <- c(0, tp_time, 1)
      } else if (has_peak) {
        tp_time <- fine_time[interior_max_idx]
        sample_times <- c(0, tp_time, 1)
      } else {
        sample_times <- c(0, 1)
      }

      n_digits <- length(sample_times)

      # Sample both scales at the same time points
      ref_vals <- interp_ref(sample_times)  # [1,5]
      int_vals <- interp_int(sample_times)  # [0,5]

      # Reference-line method: round on [1,5] scale
      chao_refline <- pmax(1L, pmin(5L, round(ref_vals)))
      str_refline <- paste0(chao_refline, collapse = "")

      # Interval method: ceiling on (0,5] scale
      # (0,1]→1, (1,2]→2, (2,3]→3, (3,4]→4, (4,5]→5
      chao_interval <- pmax(1L, pmin(5L, ceiling(int_vals)))
      str_interval <- paste0(chao_interval, collapse = "")

      # Method 1b: μ±σ range (only when raw data)
      # Values are clamped to [0, 5] following Zhu (1999) convention
      if (has_1b) {
        interp_1b <- approxfun(td$time, td$chao_1b, rule = 2)
        vals_1b <- pmax(0, pmin(5, interp_1b(sample_times)))  # clamp to [0, 5]
        chao_1b_digits <- pmax(1L, pmin(5L, ceiling(vals_1b)))
        str_1b <- paste0(chao_1b_digits, collapse = "")
      } else {
        vals_1b <- rep(NA_real_, n_digits)
        chao_1b_digits <- rep(NA_integer_, n_digits)
        str_1b <- NA_character_
      }

      # Scaled value display on [1,5] with boundary highlighting (near reference lines 1-5)
      ref_scaled_strs <- sapply(ref_vals, function(v) {
        near_boundary <- any(abs(v - 1:5) < boundary_tol)
        val_str <- sprintf("%.2f", v)
        if (near_boundary) {
          paste0('<span style="color: #d9534f; font-weight: bold;" title="within \u00b10.1 of reference line">', val_str, '</span>')
        } else {
          val_str
        }
      })
      ref_scaled_display <- paste(ref_scaled_strs, collapse = " \u2192 ")

      # Scaled value display on (0,5] with boundary highlighting (near interval boundaries 1-4)
      int_scaled_strs <- sapply(int_vals, function(v) {
        near_boundary <- any(abs(v - 1:4) < boundary_tol)
        val_str <- sprintf("%.2f", v)
        if (near_boundary) {
          paste0('<span style="color: #d9534f; font-weight: bold;" title="within \u00b10.1 of interval boundary">', val_str, '</span>')
        } else {
          val_str
        }
      })
      int_scaled_display <- paste(int_scaled_strs, collapse = " \u2192 ")

      # Scaled value display for 1b (near interval boundaries 1-4)
      if (has_1b) {
        b_scaled_strs <- sapply(vals_1b, function(v) {
          near_boundary <- any(abs(v - 1:4) < boundary_tol)
          val_str <- sprintf("%.2f", v)
          if (near_boundary) {
            paste0('<span style="color: #d9534f; font-weight: bold;" title="within \u00b10.1 of interval boundary">', val_str, '</span>')
          } else { val_str }
        })
        b_scaled_display <- paste(b_scaled_strs, collapse = " \u2192 ")
      } else {
        b_scaled_display <- NA_character_
      }

      # Contour shape (using reference-line result)
      shape <- classify_contour(str_refline)

      row <- data.frame(
        tone = tone_val,
        n_digits = n_digits,
        ref_scaled_values = ref_scaled_display,
        refline = str_refline,
        int_scaled_values = int_scaled_display,
        interval = str_interval,
        b_scaled_values = b_scaled_display,
        numeral_1b = str_1b,
        contour_shape = shape,
        stringsAsFactors = FALSE
      )

      # Store raw sample values/times for plotting (always 3 slots for consistent rbind)
      max_digits <- 3
      for (i in 1:max_digits) {
        if (i <= n_digits) {
          row[[paste0("t", i)]] <- sample_times[i]
          row[[paste0("v", i)]] <- ref_vals[i]          # [1,5] continuous
          row[[paste0("r", i)]] <- chao_refline[i]      # [1,5] rounded digit
          row[[paste0("iv", i)]] <- int_vals[i]          # (0,5] continuous
          row[[paste0("ic", i)]] <- chao_interval[i]     # (0,5] ceiling digit
          row[[paste0("bv", i)]] <- vals_1b[i]            # 1b continuous
          row[[paste0("bc", i)]] <- chao_1b_digits[i]     # 1b ceiling digit
        } else {
          row[[paste0("t", i)]] <- NA_real_
          row[[paste0("v", i)]] <- NA_real_
          row[[paste0("r", i)]] <- NA_real_
          row[[paste0("iv", i)]] <- NA_real_
          row[[paste0("ic", i)]] <- NA_real_
          row[[paste0("bv", i)]] <- NA_real_
          row[[paste0("bc", i)]] <- NA_real_
        }
      }

      results[[length(results) + 1]] <- row
    }

    result_df <- do.call(rbind, results)
    sum_result(result_df)

    # Store parameters for display
    sum_params(list(
      source = src,
      f0_min = f0_min,
      f0_max = f0_max,
      has_1b = has_1b,
      highest_tone = if (has_1b) highest_tone else NA,
      lowest_tone = if (has_1b) lowest_tone else NA,
      mu_max_1b = if (has_1b) mu_max_1b else NA,
      sigma_max_1b = if (has_1b) sigma_max_1b else NA,
      mu_min_1b = if (has_1b) mu_min_1b else NA,
      sigma_min_1b = if (has_1b) sigma_min_1b else NA,
      upper_1b = if (has_1b) upper_1b else NA,
      lower_1b = if (has_1b) lower_1b else NA
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
