###############################################
# Model: GAMM tab — Generalised Additive Mixed Model using mgcv
###############################################

gamm_ui <- function(input, output, session, dataset, normalised_data, gamm_pred_data = NULL) {

  # --- Guide text ---
  output$gamm_guide <- renderUI({
    code_style <- "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;"

    tagList(
      tags$div(
        style = "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;",
        tags$strong("GAMM guide:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Token ID:"), " A unique identifier for each token/syllable."),
          tags$li(tags$strong("f0:"), " An f0-related variable. Normalised f0 (e.g. semitone or z-score) is recommended for more interpretable results.",
            " Use the ", tags$strong("Normalise"), " tab first, then select ", tags$em("Normalised data"),
            " from the dataset dropdown to access ", tags$code(style = code_style, "f0_normalised"), "."),
          tags$li(tags$strong("Time:"), " The time variable that orders f0 samples within each token. Will be normalised to [0, 1] per token before fitting."),
          tags$li(tags$strong("Speaker:"), " Grouping variable for by-speaker random effects."),
          tags$li(tags$strong("Item:"), " The word or syllable type (e.g. different segmental compositions). Grouping variable for by-item random effects."),
          tags$li(tags$strong("Tone category:"), " Fixed effect factor. Used to model f0 contour differences across tone categories."),
          tags$li(tags$strong("Duration* (optional):"), " Syllable or token duration. Included as a smooth covariate if selected.")
        ),
        tags$strong("How GAMMs work:"),
        tags$p(style = "margin-bottom: 8px;",
          "GAMMs fit non-linear smooth curves to the data, offering more flexibility than polynomials. The model learns the contour shape directly from the data and controls overfitting through penalisation."
        ),
        tags$strong("Smooth type:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Separate smooths:"), " Fits an independent smooth for each tone: ",
            tags$code(style = code_style, "s(time, by = tone)"),
            ". Each tone gets its own curve."),
          tags$li(tags$strong("Difference smooths:"), " Fits a reference smooth + difference smooths: ",
            tags$code(style = code_style, "s(time) + s(time, by = tone.ord)"),
            ". Tone is converted to an ordered factor internally. The difference smooths directly test whether each non-reference tone differs in shape from the reference (Sóskuthy, 2017).")
        ),
        tags$strong("Random smooths (speaker):"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Speaker:"), " Basic random smooth by speaker ",
            tags$code(style = code_style, "s(time, speaker, bs = \"fs\")"),
            ". Cannot capture speaker-level variation in the tone effect."),
          tags$li(tags$strong("Speaker \u00d7 Tone:"), " Separate smooth per speaker\u2013tone combination ",
            tags$code(style = code_style, "s(time, speaker.tone, bs = \"fs\")"),
            ". Treats tokens from different tones within the same speaker as independent."),
          tags$li(tags$strong("Speaker by Tone:"), " Random smooth by speaker, separately for each tone level ",
            tags$code(style = code_style, "s(time, speaker, by = tone, bs = \"fs\")"),
            ". Behaves similarly to Speaker \u00d7 Tone."),
          tags$li(tags$strong("Speaker + Speaker by Tone:"), " Reference + difference random smooths ",
            tags$code(style = code_style, "s(time, speaker, bs = \"fs\") + s(time, speaker, by = tone.ord, bs = \"fs\")"),
            ". Recommended for difference smooths. Mirrors the fixed-effects structure in the random effects for well-calibrated Type I error (S\u00f3skuthy, 2021).")
        ),
        tags$strong("Key settings:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Basis dimension k:"), " Controls the maximum wiggliness of the smooth. Higher k allows more complex curves, but k must be less than the number of unique data points per smooth. The actual complexity is determined by the data via penalisation."),
          tags$li(tags$strong("Basis type:"), " tp (thin plate regression spline, general-purpose default), cr (cubic regression spline, faster for large data), cc (cyclic cubic spline, for periodic contours)."),
          tags$li(tags$strong("AR1:"), " Corrects for temporal autocorrelation in the residuals. Fits an initial model, estimates the autocorrelation parameter rho, then refits with the correction.")
        )
      )
    )
  })

  # --- Helper: get active dataset based on selector ---
  active_data <- reactive({
    has_norm <- !is.null(normalised_data())
    sel <- input$gamm_dataset
    if (!is.null(sel) && sel == "normalised" && has_norm) {
      normalised_data()
    } else {
      dataset()
    }
  })

  # --- Sidebar controls ---
  output$ui_gamm <- renderUI({
    has_norm <- !is.null(normalised_data())

    # Dataset choices
    ds_choices <- c("Uploaded data" = "uploaded")
    if (has_norm) {
      ds_choices <- c(ds_choices, "Normalised data" = "normalised")
    }
    ds_selected <- if (!is.null(input$gamm_dataset) && input$gamm_dataset %in% ds_choices) {
      input$gamm_dataset
    } else if (has_norm) {
      "normalised"
    } else {
      "uploaded"
    }

    # Get variable names from active dataset
    active <- active_data()
    vars <- if (!is.null(active)) names(active) else c("No dataset available")
    data_types <- if (!is.null(active)) sapply(active, class) else rep("NA", length(vars))
    var_types <- paste0(vars, " {", data_types, "}")

    # Duration choices: "None" + all variables
    dur_choices <- c("None" = "none", setNames(vars, var_types))

    tagList(
      wellPanel(
        selectInput("gamm_dataset", "Select dataset:",
                    choices = ds_choices, selected = ds_selected),
        selectInput("gamm_token_var", "Select Token ID variable:",
                    choices = setNames(vars, var_types), selected = vars[1]),
        selectInput("gamm_f0_var", "Select f0 variable (normalised f0 recommended):",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 1, vars[2], vars[1])),
        selectInput("gamm_time_var", "Select Time variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 2, vars[3], vars[1])),
        selectInput("gamm_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 3, vars[4], vars[1])),
        selectInput("gamm_tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 4, vars[5], vars[1])),
        selectInput("gamm_item_var", "Select Item variable (word/syllable type):",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 5, vars[6], vars[1])),
        selectInput("gamm_dur_var", "Select Duration variable (optional):",
                    choices = dur_choices, selected = "none"),
        tags$hr(),
        tags$strong("Smooth type:"),
        radioButtons("gamm_smooth_type", NULL,
                     choices = list("Separate smooths by tone" = "separate",
                                    "Reference + difference smooths" = "difference"),
                     selected = "difference"),
        tags$strong("Smooth settings:"),
        numericInput("gamm_k", "Basis dimension k:",
                     value = 10, min = 4, max = 20, step = 1),
        radioButtons("gamm_bs", "Basis type:",
                     choices = list("tp (thin plate)" = "tp",
                                    "cr (cubic regression)" = "cr",
                                    "cc (cyclic cubic)" = "cc"),
                     selected = "tp", inline = TRUE),
        tags$hr(),
        tags$strong("Random intercepts:"),
        checkboxInput("gamm_ri_speaker", "Speaker", value = FALSE),
        checkboxInput("gamm_ri_item", "Item", value = TRUE),
        tags$strong("Random smooths (speaker):"),
        radioButtons("gamm_rs_type", NULL,
                     choices = list(
                       "None" = "none",
                       "Speaker" = "speaker",
                       "Speaker \u00d7 Tone" = "speaker_tone",
                       "Speaker by Tone" = "speaker_by_tone",
                       "Speaker + Speaker by Tone" = "ref_diff"
                     ),
                     selected = "ref_diff"),
        tags$hr(),
        tags$strong("Autocorrelation:"),
        checkboxInput("gamm_ar1", "Include AR1 correction", value = FALSE),
        tags$hr(),
        uiOutput("gamm_warning"),
        actionButton("gamm_button", "Fit GAMM"),
        div(style = "margin-top: 4px;",
          actionButton("gamm_show_code", "Show R code", icon = icon("code"))
        ),
        tags$hr(),
        h5("Download"),
        textInput("gamm_filename", "Enter filename (without extension):",
                  value = "gamm_plot"),
        downloadButton("gamm_download", "Download Plot"),
        div(style = "margin-top: 6px;",
          downloadButton("gamm_download_model", "Download Model (.rds)")
        )
      )
    )
  })

  # --- Auto-uncheck Speaker random intercept when any random smooth is enabled ---
  # (bs = "fs" already includes random intercepts, so separate intercept is redundant)
  observeEvent(input$gamm_rs_type, {
    if (!is.null(input$gamm_rs_type) && input$gamm_rs_type != "none") {
      updateCheckboxInput(session, "gamm_ri_speaker", value = FALSE)
    }
  })

  # --- Dynamic warning based on dataset size + model complexity ---
  output$gamm_warning <- renderUI({
    data <- active_data()
    if (is.null(data)) return(NULL)
    n_rows <- nrow(data)
    rs_type <- input$gamm_rs_type
    use_ar1 <- input$gamm_ar1

    # Estimate complexity
    is_complex_rs <- !is.null(rs_type) && rs_type %in% c("speaker_tone", "speaker_by_tone", "ref_diff")
    is_large <- n_rows > 5000
    is_very_large <- n_rows > 20000

    msg <- NULL
    if (is_very_large) {
      msg <- paste0(
        "Very large dataset (", format(n_rows, big.mark = ","), " rows). ",
        "Fitting may take a long time. Consider using the R code in your local environment."
      )
    } else if (is_large && (is_complex_rs || isTRUE(use_ar1))) {
      msg <- paste0(
        "Large dataset (", format(n_rows, big.mark = ","), " rows) with complex random effects",
        if (isTRUE(use_ar1)) " and AR1 correction" else "",
        ". Fitting may take several minutes."
      )
    } else if (is_large) {
      msg <- paste0(
        "Large dataset (", format(n_rows, big.mark = ","), " rows). ",
        "Fitting may take a minute or two."
      )
    }

    if (!is.null(msg)) {
      tags$div(
        style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 6px 10px; border-radius: 4px; margin-bottom: 8px; font-size: 0.82rem;",
        icon("clock"), " ", msg
      )
    }
  })

  # --- Result storage ---
  gamm_model <- reactiveVal(NULL)
  gamm_summary_data <- reactiveVal(NULL)
  gamm_convergence_warning <- reactiveVal(NULL)
  gamm_plot_data <- reactiveVal(NULL)
  gamm_formula_str <- reactiveVal(NULL)
  gamm_rho_val <- reactiveVal(NULL)
  gamm_fitting <- reactiveVal(FALSE)

  # --- Core computation ---
  observeEvent(input$gamm_button, {
    req(active_data())
    req(input$gamm_token_var, input$gamm_time_var,
        input$gamm_speaker_var, input$gamm_tone_var,
        input$gamm_f0_var, input$gamm_item_var)

    # Show progress notification
    progress_id <- showNotification(
      tagList(icon("spinner", class = "fa-spin"), " Fitting GAMM model... This may take a while."),
      duration = NULL, closeButton = FALSE, type = "message"
    )
    on.exit(removeNotification(progress_id), add = TRUE)

    data        <- active_data()
    token_var   <- input$gamm_token_var
    time_var    <- input$gamm_time_var
    speaker_var <- input$gamm_speaker_var
    tone_var    <- input$gamm_tone_var
    f0_var      <- input$gamm_f0_var
    item_var    <- input$gamm_item_var
    k_val       <- as.integer(input$gamm_k)
    bs_type     <- input$gamm_bs
    ri_speaker  <- input$gamm_ri_speaker
    ri_item     <- input$gamm_ri_item
    rs_type     <- input$gamm_rs_type
    use_ar1     <- input$gamm_ar1
    smooth_type <- input$gamm_smooth_type
    dur_var     <- input$gamm_dur_var
    use_dur     <- !is.null(dur_var) && dur_var != "none"

    # Reset state
    gamm_convergence_warning(NULL)
    gamm_rho_val(NULL)

    # --- Data preparation ---
    # Normalise time to [0, 1] within each token
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

    # Ensure factors and internal column names
    dat$.f0 <- dat[[f0_var]]
    dat$.tone <- as.factor(dat[[tone_var]])
    dat$.speaker <- as.factor(dat[[speaker_var]])
    dat$.item <- as.factor(dat[[item_var]])
    dat$.token <- as.factor(dat[[token_var]])

    # Duration variable
    if (use_dur) {
      dat$.duration <- as.numeric(dat[[dur_var]])
    }

    # Order by token then time (needed for AR1)
    dat <- dat %>% arrange(.token, .time_norm)

    # Create start_event column for AR1
    dat <- dat %>%
      group_by(.token) %>%
      mutate(.start_event = row_number() == 1) %>%
      ungroup()

    # Convert to data.frame (bam with discrete=TRUE prefers data.frame)
    dat <- as.data.frame(dat)

    # Ordered factor for difference smooths
    # Must be set AFTER dplyr operations which can strip contrasts
    # Use ordered() with explicit levels — as.ordered() can drop unused levels
    if (smooth_type == "difference") {
      dat$.tone_ord <- ordered(dat$.tone, levels = levels(dat$.tone))
      contrasts(dat$.tone_ord) <- "contr.treatment"
    }

    # Interaction factor for speaker × tone random smooths (strategy #3)
    if (rs_type == "speaker_tone") {
      dat$.speaker_tone <- interaction(dat$.speaker, dat$.tone, drop = TRUE)
    }

    # --- Build formula dynamically ---
    if (smooth_type == "separate") {
      # Separate smooths: s(time, by = tone)
      smooth_term <- paste0("s(.time_norm, by = .tone, k = ", k_val, ', bs = "', bs_type, '")')
      fixed_part <- paste0(".f0 ~ .tone + ", smooth_term)
    } else {
      # Difference smooths: s(time) + s(time, by = tone_ord)
      ref_smooth <- paste0("s(.time_norm, k = ", k_val, ', bs = "', bs_type, '")')
      diff_smooth <- paste0("s(.time_norm, by = .tone_ord, k = ", k_val, ', bs = "', bs_type, '")')
      fixed_part <- paste0(".f0 ~ .tone_ord + ", ref_smooth, " + ", diff_smooth)
    }

    # Duration smooth
    dur_term <- ""
    if (use_dur) {
      dur_term <- " + s(.duration)"
    }

    # Random effects
    random_parts <- c()
    # Speaker random smooths (strategies from Sóskuthy 2021)
    if (rs_type == "speaker") {
      # Strategy 1: basic random smooth by speaker
      random_parts <- c(random_parts,
        paste0("s(.time_norm, .speaker, bs = \"fs\", m = 1, k = ", k_val, ")"))
    } else if (rs_type == "speaker_tone") {
      # Strategy 3: item × effect — separate smooth per speaker-tone combo
      random_parts <- c(random_parts,
        paste0("s(.time_norm, .speaker_tone, bs = \"fs\", m = 1, k = ", k_val, ")"))
    } else if (rs_type == "speaker_by_tone") {
      # Strategy 4: item-by-effect — random smooth by speaker, separately per tone
      if (smooth_type == "separate") {
        random_parts <- c(random_parts,
          paste0("s(.time_norm, .speaker, by = .tone, bs = \"fs\", m = 1, k = ", k_val, ")"))
      } else {
        random_parts <- c(random_parts,
          paste0("s(.time_norm, .speaker, by = .tone, bs = \"fs\", m = 1, k = ", k_val, ")"))
      }
    } else if (rs_type == "ref_diff") {
      # Strategy 5: reference + difference random smooths
      random_parts <- c(random_parts,
        paste0("s(.time_norm, .speaker, bs = \"fs\", m = 1, k = ", k_val, ")"))
      if (smooth_type == "difference") {
        random_parts <- c(random_parts,
          paste0("s(.time_norm, .speaker, by = .tone_ord, bs = \"fs\", m = 1, k = ", k_val, ")"))
      } else {
        random_parts <- c(random_parts,
          paste0("s(.time_norm, .speaker, by = .tone, bs = \"fs\", m = 1, k = ", k_val, ")"))
      }
    } else if (ri_speaker) {
      # No random smooth — just random intercept
      random_parts <- c(random_parts, 's(.speaker, bs = "re")')
    }
    if (ri_item) {
      random_parts <- c(random_parts, 's(.item, bs = "re")')
    }

    formula_str <- paste0(fixed_part, dur_term)
    if (length(random_parts) > 0) {
      formula_str <- paste0(formula_str, " + ", paste(random_parts, collapse = " + "))
    }

    # --- Build user-readable display formula ---
    if (smooth_type == "separate") {
      smooth_display <- paste0("s(", time_var, ", by = ", tone_var, ", k = ", k_val, ', bs = "', bs_type, '")')
      display_formula <- paste0(f0_var, " ~ ", tone_var, " + ", smooth_display)
    } else {
      ref_display <- paste0("s(", time_var, ", k = ", k_val, ', bs = "', bs_type, '")')
      diff_display <- paste0("s(", time_var, ", by = ", tone_var, ".ord, k = ", k_val, ', bs = "', bs_type, '")')
      display_formula <- paste0(f0_var, " ~ ", tone_var, ".ord + ", ref_display, " + ", diff_display)
    }
    if (use_dur) {
      display_formula <- paste0(display_formula, " + s(", dur_var, ")")
    }
    random_display <- c()
    tone_display <- if (smooth_type == "difference") paste0(tone_var, ".ord") else tone_var
    if (rs_type == "speaker") {
      random_display <- c(random_display,
        paste0("s(", time_var, ", ", speaker_var, ', bs = "fs", m = 1, k = ', k_val, ")"))
    } else if (rs_type == "speaker_tone") {
      random_display <- c(random_display,
        paste0("s(", time_var, ", ", speaker_var, ".", tone_var, ', bs = "fs", m = 1, k = ', k_val, ")"))
    } else if (rs_type == "speaker_by_tone") {
      random_display <- c(random_display,
        paste0("s(", time_var, ", ", speaker_var, ", by = ", tone_display, ', bs = "fs", m = 1, k = ', k_val, ")"))
    } else if (rs_type == "ref_diff") {
      random_display <- c(random_display,
        paste0("s(", time_var, ", ", speaker_var, ', bs = "fs", m = 1, k = ', k_val, ")"))
      random_display <- c(random_display,
        paste0("s(", time_var, ", ", speaker_var, ", by = ", tone_display, ', bs = "fs", m = 1, k = ', k_val, ")"))
    } else if (ri_speaker) {
      random_display <- c(random_display, paste0('s(', speaker_var, ', bs = "re")'))
    }
    if (ri_item) {
      random_display <- c(random_display, paste0('s(', item_var, ', bs = "re")'))
    }
    if (length(random_display) > 0) {
      display_formula <- paste0(display_formula, " + ",
                                paste(random_display, collapse = " + "))
    }
    gamm_formula_str(display_formula)

    model_formula <- as.formula(formula_str)

    # --- Fit model ---
    warn_msg <- NULL
    model <- tryCatch(
      withCallingHandlers(
        mgcv::bam(model_formula, data = dat, discrete = TRUE),
        warning = function(w) {
          warn_msg <<- conditionMessage(w)
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        showNotification(paste("Model fitting error:", e$message),
                        type = "error", duration = 10)
        return(NULL)
      }
    )

    if (is.null(model)) return()

    # --- AR1 refit if requested ---
    if (use_ar1) {
      rho <- acf(resid(model), plot = FALSE)$acf[2]
      gamm_rho_val(rho)

      model <- tryCatch(
        withCallingHandlers(
          mgcv::bam(model_formula, data = dat,
                    rho = rho, AR.start = dat$.start_event, discrete = TRUE),
          warning = function(w) {
            warn_msg <<- conditionMessage(w)
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          showNotification(paste("AR1 refit error:", e$message),
                          type = "error", duration = 10)
          return(NULL)
        }
      )
      if (is.null(model)) return()
    }

    if (!is.null(warn_msg)) {
      gamm_convergence_warning(warn_msg)
    }

    # Store model
    gamm_model(model)

    # --- Extract summary tables ---
    model_sum <- summary(model)

    # Parametric coefficients table
    p_table <- as.data.frame(model_sum$p.table)
    p_table <- tibble::rownames_to_column(p_table, var = "Term")
    # Clean up term names
    p_table$Term <- gsub("^\\.tone_ord", paste0(tone_var, ".ord"), p_table$Term)
    p_table$Term <- gsub("^\\.tone", paste0(tone_var, ""), p_table$Term)

    # Smooth terms table
    s_table <- as.data.frame(model_sum$s.table)
    s_table <- tibble::rownames_to_column(s_table, var = "Smooth")
    # Clean up smooth names for display
    s_table$Smooth <- gsub("\\.time_norm", time_var, s_table$Smooth)
    s_table$Smooth <- gsub("\\.tone_ord", paste0(tone_var, ".ord"), s_table$Smooth)
    s_table$Smooth <- gsub("\\.tone", tone_var, s_table$Smooth)
    s_table$Smooth <- gsub("\\.speaker_tone", paste0(speaker_var, ".", tone_var), s_table$Smooth)
    s_table$Smooth <- gsub("\\.speaker", speaker_var, s_table$Smooth)
    s_table$Smooth <- gsub("\\.item", item_var, s_table$Smooth)
    s_table$Smooth <- gsub("\\.duration", dur_var, s_table$Smooth)

    gamm_summary_data(list(p_table = p_table, s_table = s_table))

    # --- Prepare plot data: predicted smooth curves by tone ---
    # Use the original .tone factor for tone levels (works for both approaches)
    tone_levels <- levels(dat$.tone)
    time_seq <- seq(0, 1, length.out = 200)

    # Reference levels for random effect columns
    ref_speaker <- levels(dat$.speaker)[1]
    ref_item <- levels(dat$.item)[1]

    # Identify random effect terms to exclude from prediction
    exclude_terms <- c()
    if (rs_type == "speaker") {
      exclude_terms <- c(exclude_terms, "s(.time_norm,.speaker)")
    } else if (rs_type == "speaker_tone") {
      exclude_terms <- c(exclude_terms, "s(.time_norm,.speaker_tone)")
    } else if (rs_type == "speaker_by_tone") {
      exclude_terms <- c(exclude_terms, "s(.time_norm,.speaker)")
    } else if (rs_type == "ref_diff") {
      exclude_terms <- c(exclude_terms, "s(.time_norm,.speaker)")
    } else if (ri_speaker) {
      exclude_terms <- c(exclude_terms, "s(.speaker)")
    }
    if (ri_item) exclude_terms <- c(exclude_terms, "s(.item)")
    # Also exclude duration smooth from per-tone predictions (use mean duration)
    if (use_dur) exclude_terms <- c(exclude_terms, "s(.duration)")

    plot_rows <- list()
    for (tone in tone_levels) {
      nd <- data.frame(
        .time_norm = time_seq,
        .speaker = factor(rep(ref_speaker, length(time_seq)), levels = levels(dat$.speaker)),
        .item = factor(rep(ref_item, length(time_seq)), levels = levels(dat$.item)),
        stringsAsFactors = FALSE
      )
      if (smooth_type == "separate") {
        nd$.tone <- factor(rep(tone, length(time_seq)), levels = tone_levels)
      } else {
        # Use ordered() with explicit levels — as.ordered() drops unused levels
        nd$.tone_ord <- ordered(rep(tone, length(time_seq)), levels = tone_levels)
        contrasts(nd$.tone_ord) <- "contr.treatment"
        # Strategy #4 random smooth uses by = .tone (regular factor), so add it to newdata
        if (rs_type == "speaker_by_tone") {
          nd$.tone <- factor(rep(tone, length(time_seq)), levels = tone_levels)
        }
      }
      # Interaction factor for speaker × tone (strategy #3)
      if (rs_type == "speaker_tone") {
        nd$.speaker_tone <- interaction(nd$.speaker,
          factor(rep(tone, length(time_seq)), levels = tone_levels), drop = FALSE)
        nd$.speaker_tone <- factor(nd$.speaker_tone, levels = levels(dat$.speaker_tone))
      }
      if (use_dur) {
        nd$.duration <- rep(mean(dat$.duration, na.rm = TRUE), length(time_seq))
      }
      preds <- predict(model, newdata = nd, type = "response",
                       exclude = exclude_terms, se.fit = TRUE)
      plot_rows[[length(plot_rows) + 1]] <- data.frame(
        time = time_seq,
        f0_predicted = preds$fit,
        se = preds$se.fit,
        tone = tone,
        stringsAsFactors = FALSE
      )
    }

    gamm_plot_data(do.call(rbind, plot_rows))

    # Export to shared prediction store for Summarise tab
    if (!is.null(gamm_pred_data)) {
      gamm_pred_data(do.call(rbind, plot_rows))
    }
  })

  # --- Summary box ---
  output$gamm_summary <- renderUI({
    req(gamm_summary_data())
    sum_data <- gamm_summary_data()
    p_table <- sum_data$p_table
    s_table <- sum_data$s_table
    model <- gamm_model()
    warn_msg <- gamm_convergence_warning()
    rho <- gamm_rho_val()

    data <- active_data()
    token_var <- input$gamm_token_var
    speaker_var <- input$gamm_speaker_var
    tone_var <- input$gamm_tone_var
    item_var <- input$gamm_item_var
    formula_display <- gamm_formula_str()

    n_obs <- nrow(model$model)
    n_speakers <- length(unique(data[[speaker_var]]))
    n_tokens <- length(unique(data[[token_var]]))
    n_items <- length(unique(data[[item_var]]))
    n_tones <- length(unique(data[[tone_var]]))

    # Shared table styles
    th_style <- "padding: 4px 10px; border-bottom: 2px solid #ddd; text-align: center;"
    td_style <- "padding: 4px 10px; text-align: center;"
    td_style_left <- "padding: 4px 10px; text-align: left; font-family: monospace; font-size: 0.82rem;"

    # --- Parametric coefficients table ---
    p_col_names <- names(p_table)
    p_header <- tagList(lapply(p_col_names, function(cn) {
      display_name <- cn
      if (cn == "Std. Error") display_name <- "SE"
      if (cn == "z value") display_name <- "z"
      if (cn == "t value") display_name <- "t"
      if (cn == "Pr(>|z|)") display_name <- "p"
      if (cn == "Pr(>|t|)") display_name <- "p"
      tags$th(style = th_style, display_name)
    }))

    p_body <- lapply(seq_len(nrow(p_table)), function(i) {
      row <- p_table[i, ]
      cells <- tagList(
        tags$td(style = td_style_left, as.character(row[["Term"]])),
        lapply(p_col_names[-1], function(cn) {
          val <- as.numeric(row[[cn]])
          tags$td(style = td_style, if (abs(val) < 0.0001 && val != 0) formatC(val, format = "e", digits = 3) else round(val, 4))
        })
      )
      tags$tr(cells)
    })

    # --- Smooth terms table ---
    s_col_names <- names(s_table)
    s_header <- tagList(lapply(s_col_names, function(cn) {
      display_name <- cn
      if (cn == "p-value") display_name <- "p"
      tags$th(style = th_style, display_name)
    }))

    s_body <- lapply(seq_len(nrow(s_table)), function(i) {
      row <- s_table[i, ]
      cells <- tagList(
        tags$td(style = td_style_left, as.character(row[["Smooth"]])),
        lapply(s_col_names[-1], function(cn) {
          val <- as.numeric(row[[cn]])
          tags$td(style = td_style, if (abs(val) < 0.0001 && val != 0) formatC(val, format = "e", digits = 3) else round(val, 4))
        })
      )
      tags$tr(cells)
    })

    tagList(
      tags$div(
        style = "background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem;",
        tags$strong("Model formula:"),
        tags$pre(style = "background: #fef9e7; padding: 6px 10px; border-radius: 4px; font-size: 0.82rem; margin: 6px 0 10px 0; overflow-x: auto; white-space: pre-wrap; word-break: break-word;",
          formula_display
        ),
        tags$strong("Data summary:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(paste0("Observations: ", n_obs)),
          tags$li(paste0("Speakers: ", n_speakers)),
          tags$li(paste0("Items: ", n_items)),
          tags$li(paste0("Tokens: ", n_tokens)),
          tags$li(paste0("Tone categories: ", n_tones))
        ),
        # AR1 rho display
        if (!is.null(rho)) {
          tags$p(style = "margin-bottom: 8px;",
            tags$strong("AR1 rho: "), round(rho, 4)
          )
        },
        # Convergence/warning messages
        if (!is.null(warn_msg)) {
          tags$div(
            style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 6px 10px; border-radius: 4px; margin-bottom: 8px; font-size: 0.84rem;",
            tags$strong(icon("exclamation-triangle"), " Warning: "),
            warn_msg,
            tags$p(style = "margin-top: 6px; margin-bottom: 0;",
              "Try reducing basis dimension k, removing random smooths, or removing a random intercept."
            )
          )
        },
        tags$strong("Parametric coefficients:"),
        tags$table(
          style = "margin-top: 6px; border-collapse: collapse; font-size: 0.86rem; width: 100%;",
          tags$thead(tags$tr(p_header)),
          tags$tbody(p_body)
        ),
        tags$br(),
        tags$strong("Smooth terms:"),
        tags$table(
          style = "margin-top: 6px; border-collapse: collapse; font-size: 0.86rem; width: 100%;",
          tags$thead(tags$tr(s_header)),
          tags$tbody(s_body)
        )
      )
    )
  })

  # --- Model plot ---
  output$gamm_plot <- renderPlot({
    req(gamm_plot_data())
    pdat <- gamm_plot_data()
    tone_var <- input$gamm_tone_var
    f0_var <- input$gamm_f0_var

    ggplot(pdat, aes(x = time, y = f0_predicted, colour = tone, fill = tone)) +
      geom_ribbon(aes(ymin = f0_predicted - 1.96 * se,
                      ymax = f0_predicted + 1.96 * se),
                  alpha = 0.2, colour = NA) +
      geom_line(linewidth = 1.2) +
      labs(
        x = "Normalised time",
        y = paste0("Predicted ", f0_var),
        colour = tone_var,
        fill = tone_var
      ) +
      theme_bw(base_size = 14) +
      theme(legend.position = "bottom")
  }, height = function() { if (!is.null(gamm_plot_data())) 400 else 1 })

  # --- Download handler (plot image) ---
  output$gamm_download <- downloadHandler(
    filename = function() {
      paste0(input$gamm_filename, ".png")
    },
    content = function(file) {
      req(gamm_plot_data())
      pdat <- gamm_plot_data()
      tone_var <- input$gamm_tone_var
      f0_var <- input$gamm_f0_var

      p <- ggplot(pdat, aes(x = time, y = f0_predicted, colour = tone, fill = tone)) +
        geom_ribbon(aes(ymin = f0_predicted - 1.96 * se,
                        ymax = f0_predicted + 1.96 * se),
                    alpha = 0.2, colour = NA) +
        geom_line(linewidth = 1.2) +
        labs(
          x = "Normalised time",
          y = paste0("Predicted ", f0_var),
          colour = tone_var,
          fill = tone_var
        ) +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom")

      ggsave(file, plot = p, width = 8, height = 5, dpi = 300)
      showNotification(paste("Plot saved as", paste0(input$gamm_filename, ".png")),
                       type = "message", duration = 4)
    }
  )

  # --- Download handler (model .rds) ---
  output$gamm_download_model <- downloadHandler(
    filename = function() {
      paste0(input$gamm_filename, "_model.rds")
    },
    content = function(file) {
      req(gamm_model())
      fname <- paste0(input$gamm_filename, "_model.rds")
      saveRDS(gamm_model(), file)
      showNotification(paste("Model saved as", fname), type = "message", duration = 4)
    }
  )

  # --- Show R code toggle ---
  gamm_code_visible <- reactiveVal(FALSE)

  observeEvent(input$gamm_show_code, {
    gamm_code_visible(!gamm_code_visible())
  })

  output$gamm_r_code <- renderUI({
    req(gamm_code_visible())
    req(input$gamm_token_var, input$gamm_f0_var, input$gamm_time_var,
        input$gamm_speaker_var, input$gamm_tone_var, input$gamm_item_var)

    token_var   <- input$gamm_token_var
    f0_var      <- input$gamm_f0_var
    time_var    <- input$gamm_time_var
    speaker_var <- input$gamm_speaker_var
    tone_var    <- input$gamm_tone_var
    item_var    <- input$gamm_item_var
    k_val       <- as.integer(input$gamm_k)
    bs_type     <- input$gamm_bs
    ri_speaker  <- input$gamm_ri_speaker
    ri_item     <- input$gamm_ri_item
    rs_type     <- input$gamm_rs_type
    use_ar1     <- input$gamm_ar1
    smooth_type <- input$gamm_smooth_type
    dur_var     <- input$gamm_dur_var
    use_dur     <- !is.null(dur_var) && dur_var != "none"

    # Build formula string for display
    if (smooth_type == "separate") {
      smooth_term <- paste0('s(time_norm, by = ', tone_var, ', k = ', k_val, ', bs = "', bs_type, '")')
      formula_display <- paste0(f0_var, ' ~ ', tone_var, ' + ', smooth_term)
    } else {
      ref_smooth <- paste0('s(time_norm, k = ', k_val, ', bs = "', bs_type, '")')
      diff_smooth <- paste0('s(time_norm, by = ', tone_var, '.ord, k = ', k_val, ', bs = "', bs_type, '")')
      formula_display <- paste0(f0_var, ' ~ ', tone_var, '.ord + ', ref_smooth, ' + ', diff_smooth)
    }
    if (use_dur) {
      formula_display <- paste0(formula_display, ' + s(', dur_var, ')')
    }
    random_parts <- c()
    tone_code_var <- if (smooth_type == "separate") tone_var else paste0(tone_var, ".ord")
    if (rs_type == "speaker") {
      random_parts <- c(random_parts,
        paste0('s(time_norm, ', speaker_var, ', bs = "fs", m = 1, k = ', k_val, ')'))
    } else if (rs_type == "speaker_tone") {
      random_parts <- c(random_parts,
        paste0('s(time_norm, ', speaker_var, '.', tone_var, ', bs = "fs", m = 1, k = ', k_val, ')'))
    } else if (rs_type == "speaker_by_tone") {
      random_parts <- c(random_parts,
        paste0('s(time_norm, ', speaker_var, ', by = ', tone_code_var, ', bs = "fs", m = 1, k = ', k_val, ')'))
    } else if (rs_type == "ref_diff") {
      random_parts <- c(random_parts,
        paste0('s(time_norm, ', speaker_var, ', bs = "fs", m = 1, k = ', k_val, ')'))
      random_parts <- c(random_parts,
        paste0('s(time_norm, ', speaker_var, ', by = ', tone_code_var, ', bs = "fs", m = 1, k = ', k_val, ')'))
    } else if (ri_speaker) {
      random_parts <- c(random_parts, paste0('s(', speaker_var, ', bs = "re")'))
    }
    if (ri_item) random_parts <- c(random_parts, paste0('s(', item_var, ', bs = "re")'))
    if (length(random_parts) > 0) {
      formula_display <- paste0(formula_display, ' + ', paste(random_parts, collapse = ' + '))
    }

    # Build exclude vector string for predict()
    exclude_parts <- c()
    if (rs_type == "speaker") {
      exclude_parts <- c(exclude_parts, paste0('"s(time_norm,', speaker_var, ')"'))
    } else if (rs_type == "speaker_tone") {
      exclude_parts <- c(exclude_parts, paste0('"s(time_norm,', speaker_var, '.', tone_var, ')"'))
    } else if (rs_type %in% c("speaker_by_tone", "ref_diff")) {
      exclude_parts <- c(exclude_parts, paste0('"s(time_norm,', speaker_var, ')"'))
    } else if (ri_speaker) {
      exclude_parts <- c(exclude_parts, paste0('"s(', speaker_var, ')"'))
    }
    if (ri_item) exclude_parts <- c(exclude_parts, paste0('"s(', item_var, ')"'))
    if (use_dur) exclude_parts <- c(exclude_parts, paste0('"s(', dur_var, ')"'))
    exclude_str <- if (length(exclude_parts) > 0) {
      paste0('c(', paste(exclude_parts, collapse = ', '), ')')
    } else {
      'NULL'
    }

    code_text <- paste0(
      'library(dplyr)\n',
      'library(mgcv)\n',
      'library(ggplot2)\n\n',
      '# Read data\n',
      'dat <- read.csv("your_data.csv", stringsAsFactors = FALSE)\n\n',
      '# Normalise time to [0, 1] within each token\n',
      'dat <- dat %>%\n',
      '  group_by(', token_var, ') %>%\n',
      '  mutate(\n',
      '    time_norm = (', time_var, ' - min(', time_var, ')) / \n',
      '                (max(', time_var, ') - min(', time_var, '))\n',
      '  ) %>%\n',
      '  ungroup()\n\n',
      '# Ensure factor variables\n',
      'dat$', speaker_var, ' <- as.factor(dat$', speaker_var, ')\n',
      'dat$', item_var, ' <- as.factor(dat$', item_var, ')\n'
    )

    if (smooth_type == "separate") {
      code_text <- paste0(code_text,
        'dat$', tone_var, ' <- as.factor(dat$', tone_var, ')\n')
    } else {
      code_text <- paste0(code_text,
        'dat$', tone_var, '.ord <- as.ordered(dat$', tone_var, ')\n',
        'contrasts(dat$', tone_var, '.ord) <- "contr.treatment"\n')
    }
    if (rs_type == "speaker_tone") {
      code_text <- paste0(code_text,
        'dat$', speaker_var, '.', tone_var, ' <- interaction(dat$', speaker_var, ', dat$', tone_var, ', drop = TRUE)\n')
    }
    code_text <- paste0(code_text, '\n')

    if (use_ar1) {
      code_text <- paste0(code_text,
        '# Create AR1 start event column\n',
        'dat <- dat %>%\n',
        '  arrange(', token_var, ', time_norm) %>%\n',
        '  group_by(', token_var, ') %>%\n',
        '  mutate(start_event = row_number() == 1) %>%\n',
        '  ungroup()\n\n',
        '# Fit initial model (to estimate rho)\n',
        'model_init <- bam(\n',
        '  ', formula_display, ',\n',
        '  data = dat,\n',
        '  discrete = TRUE\n',
        ')\n\n',
        '# Estimate AR1 rho from residual autocorrelation\n',
        'rho <- acf(resid(model_init), plot = FALSE)$acf[2]\n',
        'cat("Estimated rho:", round(rho, 4), "\\n")\n\n',
        '# Refit with AR1 correction\n',
        'model <- bam(\n',
        '  ', formula_display, ',\n',
        '  data = dat,\n',
        '  rho = rho,\n',
        '  AR.start = dat$start_event,\n',
        '  discrete = TRUE\n',
        ')\n\n'
      )
    } else {
      code_text <- paste0(code_text,
        '# Fit GAMM\n',
        'model <- bam(\n',
        '  ', formula_display, ',\n',
        '  data = dat,\n',
        '  discrete = TRUE\n',
        ')\n\n'
      )
    }

    # Prediction code — use appropriate tone variable
    tone_var_pred <- if (smooth_type == "separate") tone_var else paste0(tone_var, ".ord")

    code_text <- paste0(code_text,
      '# View results\n',
      'summary(model)\n\n',
      '# Predict population-level smooth curves\n',
      'tone_levels <- levels(dat$', tone_var_pred, ')\n',
      'newdat <- expand.grid(\n',
      '  time_norm = seq(0, 1, length.out = 200),\n',
      '  ', tone_var_pred, ' = tone_levels\n',
      ')\n',
      'newdat$', speaker_var, ' <- levels(dat$', speaker_var, ')[1]\n',
      'newdat$', item_var, ' <- levels(dat$', item_var, ')[1]\n'
    )

    if (use_dur) {
      code_text <- paste0(code_text,
        'newdat$', dur_var, ' <- mean(dat$', dur_var, ', na.rm = TRUE)\n')
    }

    code_text <- paste0(code_text,
      '\npreds <- predict(model, newdata = newdat,\n',
      '                 exclude = ', exclude_str, ',\n',
      '                 se.fit = TRUE)\n',
      'newdat$fit <- preds$fit\n',
      'newdat$se <- preds$se.fit\n\n',
      '# Plot predicted smooth curves with 95% CI\n',
      'ggplot(newdat, aes(x = time_norm, y = fit,\n',
      '                   colour = ', tone_var_pred, ', fill = ', tone_var_pred, ')) +\n',
      '  geom_ribbon(aes(ymin = fit - 1.96 * se,\n',
      '                  ymax = fit + 1.96 * se),\n',
      '             alpha = 0.2, colour = NA) +\n',
      '  geom_line(linewidth = 1.2) +\n',
      '  labs(x = "Normalised time",\n',
      '       y = "Predicted ', f0_var, '",\n',
      '       colour = "', tone_var, '", fill = "', tone_var, '") +\n',
      '  theme_bw(base_size = 14) +\n',
      '  theme(legend.position = "bottom")\n\n',
      '# --- Load a saved model (.rds) ---\n',
      '# model <- readRDS("', input$gamm_filename, '_model.rds")\n',
      '# summary(model)\n',
      '# predict(model, newdata = newdat, exclude = ', exclude_str, ', se.fit = TRUE)'
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
