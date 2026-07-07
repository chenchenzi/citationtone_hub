###############################################
# Model: GAMM tab — Generalised Additive Mixed Model using mgcv
###############################################

gamm_ui <- function(input, output, session, dataset, normalised_data, gamm_pred_data = NULL, curated_data = NULL, cluster_data = NULL) {

  # GAMM is the right tab for multisyllabic words, so the only thing to catch
  # here is picking the wrong landmark column: the within-segment 0-1 axis
  # (_t01) resets each segment and is not a valid model time axis. The
  # sequential _tseq column is the one to use.
  output$gamm_multisyl_note <- renderUI({
    tv <- input$gamm_time_var
    if (is.null(tv) || !grepl("_t01$", tv)) return(NULL)
    tags$div(
      style = paste("background-color:#fff8e1; border-left:4px solid #e0a800;",
                    "padding:10px 14px; margin:8px 0; border-radius:4px;",
                    "color:#7a5d00; font-size:0.9rem;"),
      tags$span(style = "color:#c0392b;", icon("triangle-exclamation")),
      HTML(sprintf(" <code>%s</code> rescales time to 0&ndash;1 <em>within</em> each segment, so it resets at every boundary and is <strong>not a valid model time axis</strong>. Use the sequential <code>&hellip;_tseq</code> column instead.", tv)))
  })

  # --- Guide text ---
  output$gamm_guide <- renderUI({
    code_style <- "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;"

    tagList(
      guide_box("GAMM guide",
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Token ID:"), " A unique identifier for each token/syllable."),
          tags$li(tags$strong("f0:"), " An f0-related variable. Normalised f0 (e.g. semitone or z-score) is recommended for more interpretable results.",
            " Use the ", tags$strong("Normalise"), " tab first, then select ", tags$em("Normalised data"),
            " from the dataset dropdown to access ", tags$code(style = code_style, "f0_st"), " or ", tags$code(style = code_style, "f0_zscore"), "."),
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
          tags$li(tags$strong("AR1:"), " Corrects for temporal autocorrelation in the residuals. Fits an initial model, estimates the autocorrelation parameter rho (from the lag-1 correlation ",
            tags$em("within"), " each token), then refits with the correction. ",
            tags$strong("On by default:"), " densely-sampled f0 frames are strongly autocorrelated, so without it the smooth p-values are anticonservative. Uncheck it for a quick exploratory fit (it doubles fitting time, since rho needs an initial fit).",
            tags$br(),
            tags$span(style = "color: #7a5d00;",
              "AR1 assumes ", tags$strong("evenly-spaced frames within each token."),
              " It is robust to the occasional dropped frame, but treat rho with more caution for contours with long voiceless gaps (e.g. a medial voiceless consonant). Use the ",
              tags$strong("Diagnose model"), " ACF panel to check the correction worked."))
        )
      ),
      # --- Collapsible illustrated guide for multisyllabic words ---
      tags$details(class = "msg-route",
        tags$style(HTML("
          details.msg-route{background:#f3f8fc;border:1px solid #cfe2f1;border-radius:8px;padding:7px 14px 11px;margin:12px 0 0;}
          .msg-route>summary{cursor:pointer;font-weight:700;color:#2c5d80;font-size:0.92rem;list-style:none;padding:1px 0;}
          .msg-route>summary::-webkit-details-marker{display:none;}
          .msg-route>summary::before{content:'\\25B8';color:#5b9bd5;display:inline-block;margin-right:8px;transition:transform .15s ease;}
          .msg-route[open]>summary::before{transform:rotate(90deg);}
          .msg-hint{color:#7aa6cc;font-weight:400;font-size:0.78rem;margin-left:6px;}
          .msg-route[open] .msg-hint{display:none;}
          .msg-intro{color:#3f5a72;font-size:0.83rem;line-height:1.5;margin:9px 0 0;}
          .msg-flow{display:flex;align-items:stretch;gap:9px;margin-top:11px;flex-wrap:wrap;}
          .msg-step{flex:1 1 185px;background:#fff;border:1px solid #e1e9f2;border-radius:7px;padding:8px 12px;}
          .msg-step-here{border-color:#78c2ad;box-shadow:0 0 0 2px rgba(120,194,173,0.18);}
          .msg-shead{display:flex;align-items:center;gap:7px;margin-bottom:3px;flex-wrap:wrap;}
          .msg-badge{width:20px;height:20px;border-radius:50%;flex-shrink:0;display:inline-flex;align-items:center;justify-content:center;font-size:0.72rem;font-weight:700;color:#fff;}
          .msg-num{background:#aab9c6;}
          .msg-here{background:#d9534f;font-size:0.5rem;}
          .msg-stitle{font-weight:700;color:#2c5f4f;font-size:0.86rem;}
          .msg-tab{display:inline-block;background:#e8f5f0;color:#2c5f4f;padding:1px 7px;border-radius:10px;font-size:0.66rem;font-weight:600;font-family:'SFMono-Regular',Menlo,Consolas,monospace;white-space:nowrap;}
          .msg-swhy{font-size:0.76rem;color:#5f6b66;line-height:1.4;margin-top:2px;}
          .msg-arrow{display:flex;align-items:center;color:#9fbbd6;font-size:1.3rem;}
          @media (max-width:760px){.msg-arrow{display:none;}}
          .msg-tip{font-size:0.78rem;color:#33536f;background:#eaf3fb;border:1px solid #d3e6f5;border-radius:6px;padding:6px 11px;margin-top:11px;line-height:1.5;}
          .msg-tip .fa,.msg-tip svg{color:#5b9bd5;margin-right:4px;}
        ")),
        tags$summary(icon("music"), " Working with multisyllabic words?",
                     tags$span(class = "msg-hint", "(click to expand)")),
        tags$p(class = "msg-intro",
          "Fit a disyllabic (or longer) word as one curve on a syllable-aligned time axis. GAMM handles the extra shape complexity that polynomials and GCA cannot summarise with a few interpretable terms."),
        tags$div(class = "msg-flow",
          tags$div(class = "msg-step",
            tags$div(class = "msg-shead",
              tags$span(class = "msg-badge msg-num", "1"),
              tags$span(class = "msg-stitle", "Extract"),
              tags$span(class = "msg-tab", "F0 Extraction")),
            tags$div(class = "msg-swhy",
              "Add a landmark tier (e.g. syllable) so every f0 frame carries its segment boundaries.")),
          tags$div(class = "msg-arrow", HTML("&#10132;")),
          tags$div(class = "msg-step",
            tags$div(class = "msg-shead",
              tags$span(class = "msg-badge msg-num", "2"),
              tags$span(class = "msg-stitle", "Normalise time"),
              tags$span(class = "msg-tab", "Normalise")),
            tags$div(class = "msg-swhy",
              HTML("Time Normalisation options: pick the tier to add a sequential time axis (<code>&lt;tier&gt;_tseq</code>)."))),
          tags$div(class = "msg-arrow", HTML("&#10132;")),
          tags$div(class = "msg-step msg-step-here",
            tags$div(class = "msg-shead",
              tags$span(class = "msg-badge msg-here", HTML("&#9679;")),
              tags$span(class = "msg-stitle", "GAMM")),
            tags$div(class = "msg-swhy",
              HTML("You are here. Set the <strong>Time</strong> variable to <code>&lt;tier&gt;_tseq</code> and fit one smooth across the whole word.")))
        ),
        tags$div(class = "msg-tip",
          icon("lightbulb"), HTML(" <strong>For this workflow:</strong>"),
          tags$ul(style = "margin:4px 0 0 0; padding-left:18px;",
            tags$li(HTML("<strong>Tone category</strong> = the per-word <strong>tone-sequence label</strong> (e.g. <code>H.L</code>), one per token; not a per-syllable tone.")),
            tags$li(HTML("Raise the <strong>basis dimension k</strong> so the smooth can capture every syllable's shape (penalisation keeps the effective complexity in check).")),
            tags$li(HTML("Use a batch with a <strong>uniform syllable count</strong>, so segment boundaries line up across tokens (e.g. a disyllabic batch, then a trisyllabic batch, as separate runs)."))))
      )
    )
  })

  # --- Helper: get active dataset based on selector ---
  active_data <- reactive({
    has_norm <- !is.null(normalised_data())
    has_cur  <- !is.null(curated_data) && !is.null(curated_data())
    has_clu  <- !is.null(cluster_data) && !is.null(cluster_data())
    sel <- input$gamm_dataset
    if (!is.null(sel) && sel == "clustered" && has_clu) {
      cluster_data()
    } else if (!is.null(sel) && sel == "curated" && has_cur) {
      curated_data()
    } else if (!is.null(sel) && sel == "normalised" && has_norm) {
      normalised_data()
    } else {
      dataset()
    }
  })

  # --- Sidebar controls ---
  output$ui_gamm <- renderUI({
    has_norm <- !is.null(normalised_data())
    has_cur  <- !is.null(curated_data) && !is.null(curated_data())
    has_clu  <- !is.null(cluster_data) && !is.null(cluster_data())

    # Dataset choices
    ds_choices <- c("Uploaded data" = "uploaded")
    if (has_norm) {
      ds_choices <- c(ds_choices, "Normalised data" = "normalised")
    }
    if (has_cur) {
      ds_choices <- c(ds_choices, "Curated data" = "curated")
    }
    if (has_clu) {
      ds_choices <- c(ds_choices, "Clustered data" = "clustered")
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
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$token, 1)),
        selectInput("gamm_f0_var", "Select f0 variable (normalised f0 recommended):",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$f0, 2)),
        selectInput("gamm_time_var", "Select Time variable:",  # auto-prefers <tier>_tseq
                    choices = setNames(vars, var_types),
                    selected = guess_time_var(vars, 3)),
        selectInput("gamm_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$speaker, 4)),
        selectInput("gamm_tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$tone, 5)),
        selectInput("gamm_item_var", "Select Item variable (word/syllable type):",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$item, 6)),
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
        checkboxInput("gamm_ar1", "Include AR1 correction", value = TRUE),
        tags$hr(),
        uiOutput("gamm_warning"),
        actionButton("gamm_button", "Fit GAMM"),
        div(style = "margin-top: 4px;",
          actionButton("gamm_show_code", "Show R code", icon = icon("code"))
        ),
        uiOutput("gamm_diagnose_ui"),
        tags$hr(),
        h5("Download"),
        textInput("gamm_filename", "Enter filename (without extension):",
                  value = if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                            paste0(input$dataset_name, "_gamm")
                          else "gamm_plot"),
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
  gamm_fit_obj <- reactiveVal(NULL)   # full shinytone_gamm object (for diagnostics)
  gamm_summary_data <- reactiveVal(NULL)
  gamm_convergence_warning <- reactiveVal(NULL)
  gamm_plot_data <- reactiveVal(NULL)
  gamm_formula_str <- reactiveVal(NULL)
  gamm_rho_val <- reactiveVal(NULL)
  gamm_fitting <- reactiveVal(FALSE)
  gamm_diag_data <- reactiveVal(NULL)  # NULL until the user clicks "Diagnose model"

  # --- Core computation ---
  observeEvent(input$gamm_button, {
    req(active_data())
    req(input$gamm_token_var, input$gamm_time_var,
        input$gamm_speaker_var, input$gamm_tone_var,
        input$gamm_f0_var, input$gamm_item_var)

    progress_id <- showNotification(
      tagList(icon("spinner", class = "fa-spin"),
              " Fitting GAMM model... This may take a while."),
      duration = NULL, closeButton = FALSE, type = "message"
    )
    on.exit(removeNotification(progress_id), add = TRUE)

    gamm_convergence_warning(NULL)
    gamm_rho_val(NULL)
    gamm_diag_data(NULL)   # stale diagnostics belong to the previous fit

    # Map the radio-button random-smooth strategy onto the package API.
    rs_type    <- input$gamm_rs_type
    rs_for_pkg <- if (rs_type %in% c("speaker", "speaker_tone",
                                     "speaker_by_tone", "ref_diff"))
                    rs_type
                  else
                    "none"

    dur_var <- input$gamm_dur_var
    use_dur <- !is.null(dur_var) && dur_var != "none"

    # All preparation, formula construction, fitting (and optional AR1
    # refit) lives in the package function fit_gamm() (R/gamm.R).
    fit <- tryCatch(
      fit_gamm(
        active_data(),
        f0       = input$gamm_f0_var,
        time     = input$gamm_time_var,
        token    = input$gamm_token_var,
        tone     = input$gamm_tone_var,
        speaker  = input$gamm_speaker_var,
        item     = input$gamm_item_var,
        duration = if (use_dur) dur_var else NULL,
        k                        = as.integer(input$gamm_k),
        bs                       = input$gamm_bs,
        smooth_type              = input$gamm_smooth_type,
        random_intercept_speaker = isTRUE(input$gamm_ri_speaker),
        random_intercept_item    = isTRUE(input$gamm_ri_item),
        random_smooth            = rs_for_pkg,
        use_ar1                  = isTRUE(input$gamm_ar1)
      ),
      error = function(e) {
        showNotification(paste("Model fitting error:", e$message),
                         type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(fit)) return()

    model       <- fit$model
    tone_var    <- input$gamm_tone_var
    speaker_var <- input$gamm_speaker_var
    item_var    <- input$gamm_item_var
    time_var    <- input$gamm_time_var

    if (!is.null(fit$convergence_warning)) {
      gamm_convergence_warning(fit$convergence_warning)
    }
    if (!is.null(fit$rho)) gamm_rho_val(fit$rho)

    gamm_formula_str(fit$formula_str)
    gamm_model(model)
    gamm_fit_obj(fit)

    # --- Extract summary tables ---
    # Name-cleaning back to user's column names is UI-specific so it stays
    # here rather than in the package function.
    model_sum <- summary(model)
    p_table <- as.data.frame(model_sum$p.table)
    p_table <- tibble::rownames_to_column(p_table, var = "Term")
    p_table$Term <- gsub("^\\.tone_ord", paste0(tone_var, ".ord"), p_table$Term)
    p_table$Term <- gsub("^\\.tone",     tone_var,                 p_table$Term)

    s_table <- as.data.frame(model_sum$s.table)
    s_table <- tibble::rownames_to_column(s_table, var = "Smooth")
    s_table$Smooth <- gsub("\\.time_norm",    time_var,                              s_table$Smooth)
    s_table$Smooth <- gsub("\\.tone_ord",     paste0(tone_var, ".ord"),              s_table$Smooth)
    s_table$Smooth <- gsub("\\.tone",         tone_var,                              s_table$Smooth)
    s_table$Smooth <- gsub("\\.speaker_tone", paste0(speaker_var, ".", tone_var),    s_table$Smooth)
    s_table$Smooth <- gsub("\\.speaker",      speaker_var,                           s_table$Smooth)
    s_table$Smooth <- gsub("\\.item",         item_var,                              s_table$Smooth)
    if (use_dur) {
      s_table$Smooth <- gsub("\\.duration", dur_var, s_table$Smooth)
    }

    gamm_summary_data(list(p_table = p_table, s_table = s_table))

    # Population-level per-tone predictions, computed by predict_gamm()
    # which knows the smooth_type / random_smooth / duration setup.
    plot_df <- predict_gamm(fit, n = 200)
    gamm_plot_data(plot_df)
    if (!is.null(gamm_pred_data)) gamm_pred_data(plot_df)
  })

  # ==========================================================================
  # Model diagnostics
  # --------------------------------------------------------------------------
  # After a model is fit, offer a "Diagnose model" button. Diagnostics mirror
  # mgcv::gam.check(): residual behaviour (Q-Q, residuals vs fitted, histogram,
  # observed vs fitted), the basis-dimension (k) check, and residual
  # autocorrelation (which motivates the AR1 option). The heavy lifting lives
  # in the package function diagnose_gamm() (R/gamm.R); this section only turns
  # its numeric output into tables and ggplots.
  # ==========================================================================

  # --- Sidebar button (only appears once a model exists) ---
  output$gamm_diagnose_ui <- renderUI({
    if (is.null(gamm_model())) return(NULL)
    tagList(
      tags$hr(),
      tags$strong("Model diagnostics:"),
      tags$p(style = "font-size: 0.8rem; color: #555; margin: 4px 0 8px;",
        "Check whether the fit is trustworthy: residual normality, whether the ",
        "basis dimension ", tags$em("k"), " is large enough, and leftover ",
        "autocorrelation."),
      actionButton("gamm_diagnose", "Diagnose model", icon = icon("stethoscope")),
      if (!is.null(gamm_diag_data())) {
        div(style = "margin-top: 6px;",
          downloadButton("gamm_diag_download", "Download diagnostics"))
      }
    )
  })

  # --- Compute diagnostics on demand ---
  observeEvent(input$gamm_diagnose, {
    req(gamm_fit_obj())
    progress_id <- showNotification(
      tagList(icon("spinner", class = "fa-spin"), " Computing diagnostics..."),
      duration = NULL, closeButton = FALSE, type = "message"
    )
    on.exit(removeNotification(progress_id), add = TRUE)

    diag <- tryCatch(
      diagnose_gamm(gamm_fit_obj()),
      error = function(e) {
        showNotification(paste("Diagnostics error:", e$message),
                         type = "error", duration = 10)
        NULL
      }
    )
    gamm_diag_data(diag)
  })

  # --- Shared ggplot builders (used by both the on-screen plots and the
  #     downloaded image) ---
  gamm_diag_resid_grobs <- function(diag) {
    rdf <- diag$resid_df
    base_theme <- theme_bw(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 12))
    pt_col <- "#2c6e91"; line_col <- "#c0392b"

    p_qq <- ggplot(rdf, aes(sample = residual)) +
      stat_qq(size = 0.7, alpha = 0.45, colour = pt_col) +
      stat_qq_line(colour = line_col, linewidth = 0.7) +
      labs(title = "Q–Q plot of residuals",
           x = "Theoretical quantiles", y = "Deviance residuals") +
      base_theme
    p_rf <- ggplot(rdf, aes(x = fitted, y = residual)) +
      geom_hline(yintercept = 0, colour = line_col, linewidth = 0.6) +
      geom_point(size = 0.7, alpha = 0.35, colour = pt_col) +
      labs(title = "Residuals vs fitted",
           x = "Fitted values", y = "Deviance residuals") +
      base_theme
    p_hist <- ggplot(rdf, aes(x = residual)) +
      geom_histogram(bins = 40, fill = "#78c2ad", colour = "white",
                     linewidth = 0.2) +
      labs(title = "Histogram of residuals",
           x = "Deviance residuals", y = "Count") +
      base_theme
    p_of <- ggplot(rdf, aes(x = fitted, y = observed)) +
      geom_abline(slope = 1, intercept = 0, colour = line_col, linewidth = 0.6) +
      geom_point(size = 0.7, alpha = 0.35, colour = pt_col) +
      labs(title = "Observed vs fitted",
           x = "Fitted values", y = "Observed values") +
      base_theme
    list(p_qq, p_rf, p_hist, p_of)
  }

  gamm_diag_acf_ggplot <- function(diag) {
    adf <- diag$acf
    ci  <- diag$acf_ci
    title <- if (isTRUE(diag$acf_whitened))
               "ACF of AR1-whitened residuals (per token)"
             else if (isTRUE(diag$acf_grouped))
               "Autocorrelation of residuals (per token)"
             else
               "Autocorrelation of residuals (ACF)"
    p <- ggplot(adf, aes(x = lag, y = acf)) +
      geom_hline(yintercept = 0, colour = "#888", linewidth = 0.5) +
      geom_segment(aes(xend = lag, yend = 0), colour = "#2c6e91", linewidth = 0.8) +
      labs(title = title, x = "Lag", y = "ACF") +
      theme_bw(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 12))
    if (!is.na(ci)) {
      p <- p + geom_hline(yintercept = c(-ci, ci), linetype = "dashed",
                          colour = "#c0392b", linewidth = 0.6)
    }
    p
  }

  # --- Residual panel (2x2 grid) ---
  output$gamm_diag_resid_plot <- renderPlot({
    req(gamm_diag_data())
    gridExtra::grid.arrange(grobs = gamm_diag_resid_grobs(gamm_diag_data()),
                            ncol = 2)
  }, width = 800, height = 560)

  # --- Residual ACF ---
  output$gamm_diag_acf_plot <- renderPlot({
    req(gamm_diag_data())
    gamm_diag_acf_ggplot(gamm_diag_data())
  }, width = 800, height = 260)

  # --- Diagnostics main-panel container (tables + plot slots + guide) ---
  output$gamm_diagnostics <- renderUI({
    diag <- gamm_diag_data()
    if (is.null(diag)) return(NULL)

    th_style   <- "padding: 4px 10px; border-bottom: 2px solid #ddd; text-align: center;"
    td_style   <- "padding: 4px 10px; text-align: center;"
    td_left    <- "padding: 4px 10px; text-align: left; font-family: monospace; font-size: 0.82rem;"

    # ---- Basis-dimension (k) check table ----
    kc <- diag$k_check
    k_block <- if (is.null(kc)) {
      tags$p(style = "color: #777; font-size: 0.86rem;",
        "The basis-dimension check could not be computed for this model.")
    } else {
      disp_cols <- intersect(c("k'", "edf", "k-index", "p-value"), names(kc))
      fmt_k <- function(cn, val) {
        if (is.na(val)) return("–")
        if (cn == "k'") return(formatC(round(as.numeric(val)), format = "d"))
        if (cn == "edf") return(formatC(as.numeric(val), format = "f", digits = 2))
        if (cn == "p-value") {
          v <- as.numeric(val)
          return(if (v < 0.0001) formatC(v, format = "e", digits = 2)
                 else formatC(v, format = "f", digits = 4))
        }
        formatC(as.numeric(val), format = "f", digits = 3)
      }
      header <- tags$tr(
        tags$th(style = paste0(th_style, "text-align:left;"), "Smooth"),
        lapply(disp_cols, function(cn) {
          nm <- if (cn == "p-value") "p-value" else cn
          tags$th(style = th_style, nm)
        }),
        tags$th(style = th_style, "flag")
      )
      body <- lapply(seq_len(nrow(kc)), function(i) {
        row  <- kc[i, ]
        flag <- row$k_flag
        row_bg  <- if (identical(flag, "low")) "background-color: #fdecea;" else ""
        txt_col <- if (identical(flag, "na")) "color: #9a9a9a;" else ""
        tags$tr(style = row_bg,
          tags$td(style = paste0(td_left, txt_col), as.character(row$Smooth)),
          lapply(disp_cols, function(cn)
            tags$td(style = paste0(td_style, txt_col), fmt_k(cn, row[[cn]]))),
          tags$td(style = td_style,
            if (identical(flag, "low"))
              tags$span(style = "color: #c0392b; font-weight: 600;",
                        icon("triangle-exclamation"), " k low?")
            else if (identical(flag, "ok"))
              tags$span(style = "color: #2e7d32;", icon("check"))
            else tags$span(style = "color: #bbb;", "–"))
        )
      })
      any_low <- any(kc$k_flag == "low", na.rm = TRUE)
      tagList(
        tags$table(
          style = "margin-top: 6px; border-collapse: collapse; font-size: 0.86rem; width: 100%;",
          tags$thead(header),
          tags$tbody(body)
        ),
        if (any_low) {
          tags$div(
            style = "background-color: #fdecea; border: 1px solid #e6a9a1; padding: 6px 10px; border-radius: 4px; margin-top: 8px; font-size: 0.84rem;",
            icon("triangle-exclamation"),
            HTML(" One or more time smooths look under-resourced: a <strong>k-index below 1</strong> with a <strong>small p-value</strong> means the basis dimension <em>k</em> may be too low. Raise <em>k</em> in the sidebar and refit, then diagnose again."))
        } else {
          tags$div(
            style = "background-color: #eafaf1; border: 1px solid #b7e4c7; padding: 6px 10px; border-radius: 4px; margin-top: 8px; font-size: 0.84rem;",
            icon("check"),
            HTML(" Basis dimensions look adequate: no time smooth combines a k-index below 1 with a significant p-value."))
        }
      )
    }

    # ---- Concurvity table (advanced, collapsible) ----
    cc <- diag$concurvity
    conc_block <- if (is.null(cc)) {
      NULL
    } else {
      term_cols <- setdiff(names(cc), "Measure")
      colour_cell <- function(v) {
        vn <- suppressWarnings(as.numeric(v))
        col <- if (is.na(vn)) "" else if (vn >= 0.8) "color:#c0392b; font-weight:600;"
               else if (vn >= 0.5) "color:#c07a00;" else "color:#2e7d32;"
        list(txt = if (is.na(vn)) "–" else formatC(vn, format = "f", digits = 2),
             style = col)
      }
      header <- tags$tr(
        tags$th(style = paste0(th_style, "text-align:left;"), "Measure"),
        lapply(term_cols, function(cn) tags$th(style = th_style, cn))
      )
      body <- lapply(seq_len(nrow(cc)), function(i) {
        row <- cc[i, ]
        tags$tr(
          tags$td(style = td_left, as.character(row$Measure)),
          lapply(term_cols, function(cn) {
            cell <- colour_cell(row[[cn]])
            tags$td(style = paste0(td_style, cell$style), cell$txt)
          })
        )
      })
      tags$details(
        style = "margin-top: 14px; background: #f7f9fb; border: 1px solid #e1e9f2; border-radius: 6px; padding: 6px 12px 10px;",
        tags$summary(style = "cursor: pointer; font-weight: 700; color: #2c5d80;",
                     "Concurvity (advanced)"),
        tags$p(style = "font-size: 0.83rem; color: #555; margin: 8px 0 4px;",
          HTML("Concurvity is the smooth-term analogue of collinearity (0 = independent, 1 = fully confounded). Values near 1 mean a smooth is nearly reproducible from the others, so its individual estimate and p-value are unreliable. High concurvity between the population time smooths and the by-speaker random smooths is common and usually harmless; treat it as a caution, not a failure.")),
        tags$div(style = "overflow-x: auto;",
          tags$table(
            style = "margin-top: 4px; border-collapse: collapse; font-size: 0.84rem; width: 100%;",
            tags$thead(header),
            tags$tbody(body)))
      )
    }

    # ---- ACF section note (AR1- and grouping-aware) ----
    acf_note <- if (isTRUE(diag$acf_whitened)) {
      tags$div(
        style = "font-size: 0.84rem; color: #555; margin: 2px 0 8px;",
        HTML(sprintf("An AR1 correction was applied (rho = %s). The ACF below is computed on the <strong>AR1-whitened residuals, per token</strong>, so it shows whether the correction worked: if it did, the lag-1 bar should now sit <em>inside</em> the dashed band. Bars still poking well outside it mean autocorrelation remains &mdash; consider a different structure.",
                     formatC(diag$rho, format = "f", digits = 3))))
    } else if (isTRUE(diag$acf_grouped)) {
      tags$div(
        style = "font-size: 0.84rem; color: #555; margin: 2px 0 8px;",
        HTML("Computed <strong>per token</strong> (within-token autocorrelation pooled across tokens, so token boundaries don't distort the low lags &mdash; the same idea as <code>itsadug::acf_resid</code>). Bars decaying slowly across lags (rather than dropping inside the dashed band after lag 1) indicate temporal autocorrelation; enable <strong>AR1 correction</strong> in the sidebar and refit."))
    } else {
      tags$div(
        style = "font-size: 0.84rem; color: #555; margin: 2px 0 8px;",
        HTML("Bars decaying slowly across lags (rather than dropping inside the dashed band after lag 1) indicate temporal autocorrelation. If you see that, enable <strong>AR1 correction</strong> in the sidebar and refit."))
    }

    tagList(
      tags$hr(),
      tags$h4(icon("stethoscope"), " Model diagnostics"),

      guide_box("How to read these diagnostics",
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Basis dimension (k):"),
            " the key check. If a time smooth has a k-index below 1 and a small p-value, its ",
            tags$em("k"), " is likely too low — most convincingly when its ",
            tags$em("edf"), " is also close to ", tags$em("k'"),
            ". Raise ", tags$em("k"),
            " and refit. Random-effect terms (by-speaker / by-item) are not checked this way."),
          tags$li(tags$strong("Q–Q plot:"),
            " residuals should fall along the red line. Systematic curvature at the tails signals non-normal residuals."),
          tags$li(tags$strong("Residuals vs fitted:"),
            " should be a flat, patternless band around zero. A funnel shape means non-constant variance."),
          tags$li(tags$strong("Histogram:"),
            " should look roughly bell-shaped and centred on zero."),
          tags$li(tags$strong("Observed vs fitted:"),
            " points should hug the red 1:1 line; scatter reflects unexplained variation."),
          tags$li(tags$strong("Residual ACF:"),
            " checks for leftover temporal autocorrelation, computed per token. Without AR1 it motivates turning the correction on; with AR1 it shows the whitened residuals so you can confirm the correction worked.")),
        open = FALSE),

      tags$h5(style = "margin-top: 14px;", "Basis dimension check (k)"),
      k_block,

      tags$h5(style = "margin-top: 16px;", "Residual checks"),
      tags$div(class = "plot-spinner-wrap",
        plotOutput("gamm_diag_resid_plot", height = "560px", width = "800px"),
        tags$div(class = "plot-spinner")
      ),

      tags$h5(style = "margin-top: 16px;", "Residual autocorrelation"),
      acf_note,
      tags$div(class = "plot-spinner-wrap",
        plotOutput("gamm_diag_acf_plot", height = "260px", width = "800px"),
        tags$div(class = "plot-spinner")
      ),

      conc_block
    )
  })

  # --- Download handler (diagnostics image: 4 residual panels + ACF) ---
  output$gamm_diag_download <- downloadHandler(
    filename = function() {
      base <- if (!is.null(input$gamm_filename) && nzchar(input$gamm_filename))
                input$gamm_filename else "gamm"
      paste0(base, "_diagnostics.png")
    },
    content = function(file) {
      req(gamm_diag_data())
      diag  <- gamm_diag_data()
      grobs <- gamm_diag_resid_grobs(diag)
      acf_p <- gamm_diag_acf_ggplot(diag)
      grDevices::png(file, width = 9, height = 11, units = "in", res = 200)
      on.exit(grDevices::dev.off(), add = TRUE)
      gridExtra::grid.arrange(
        grobs = c(grobs, list(acf_p)),
        layout_matrix = rbind(c(1, 2), c(3, 4), c(5, 5)),
        heights = c(1, 1, 0.8)
      )
      showNotification("Diagnostics image saved.", type = "message", duration = 4)
    }
  )

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
  }, width = 800, height = 500)

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

    # Use the actual uploaded filename if known so the snippet visually
    # matches the dataset; users may need to adjust the path on disk.
    ds_name <- if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                 paste0(input$dataset_name, ".csv")
               else "your_data.csv"

    code_text <- paste0(
      'library(dplyr)\n',
      'library(mgcv)\n',
      'library(ggplot2)\n\n',
      '# Read your data (adjust path to where the file is on your machine)\n',
      'dat <- read.csv("', ds_name, '", stringsAsFactors = FALSE)\n\n',
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
      'summary(model)\n\n')

    # Diagnostics block — mirrors the app's "Diagnose model" panel so the
    # exported script reproduces it. Points at the standard packages.
    acf_line <- if (use_ar1) {
      paste0('acf_resid(model)   # AR1 fit: uses the stored AR.start/rho to\n',
             '                   # show the whitened residuals (did AR1 work?)\n')
    } else {
      paste0('acf_resid(model, split_pred = list(dat$', token_var, '))  # per-token ACF\n')
    }
    code_text <- paste0(code_text,
      '# --- Model diagnostics ---\n',
      '# Base mgcv checks: residual plots (Q-Q, residuals vs linear predictor,\n',
      '# histogram, response vs fitted) and basis-dimension (k) adequacy.\n',
      'par(mfrow = c(2, 2))\n',
      'gam.check(model)\n',
      'k.check(model)                 # k-index < 1 with small p-value => raise k, refit\n',
      'concurvity(model, full = TRUE) # smooth-term collinearity (0 = independent, 1 = confounded)\n\n',
      '# Residual autocorrelation, respecting token boundaries (the app\'s ACF panel).\n',
      '# itsadug::acf_resid is the standard tool in phonetics GAMM workflows:\n',
      '#   install.packages("itsadug")\n',
      'library(itsadug)\n',
      acf_line,
      '\n# gratia gives ggplot-based equivalents if you prefer:\n',
      '#   gratia::appraise(model); gratia::draw(model)\n\n')

    code_text <- paste0(code_text,
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
