###############################################
# Model: GCA tab — Growth Curve Analysis using lme4
###############################################

gca_ui <- function(input, output, session, dataset, normalised_data, gca_pred_data = NULL, curated_data = NULL, cluster_data = NULL) {

  # On a landmark (multisyllabic) time axis, GCA's interpretable low-order terms
  # stop meaning much, so just nudge to GAMM. The _t01-vs-_tseq distinction is
  # handled on the GAMM tab, where the right column actually matters.
  output$gca_multisyl_note <- renderUI({
    tv <- input$gca_time_var
    if (is.null(tv) || !grepl("_(tseq|t01)$", tv)) return(NULL)
    tags$div(
      style = paste("background-color:#fff8e1; border-left:4px solid #e0a800;",
                    "padding:10px 14px; margin:8px 0; border-radius:4px;",
                    "color:#7a5d00; font-size:0.9rem;"),
      tags$span(style = "color:#e0a800;", icon("wand-magic-sparkles")),
      " Are you modelling a multisyllabic time axis? Polynomial/GCA coefficients here summarise single-syllable shapes; for multisyllabic words, consider GAMM for these contours.")
  })

  # --- Guide text ---
  output$gca_guide <- renderUI({
    code_style <- "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;"

    tagList(
      guide_box("Growth Curve Analysis guide",
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Token ID:"), " A unique identifier for each token/syllable."),
          tags$li(tags$strong("f0:"), " An f0-related variable. Normalised f0 (e.g. semitone or z-score) is recommended for more interpretable and comparable coefficients across speakers.",
            " Use the ", tags$strong("Normalise"), " tab first, then select ", tags$em("Normalised data"),
            " from the dataset dropdown to access ", tags$code(style = code_style, "f0_st"), " or ", tags$code(style = code_style, "f0_zscore"), "."),
          tags$li(tags$strong("Time:"), " The time variable that orders f0 samples within each token. Will be normalised to [0, 1] per token before fitting."),
          tags$li(tags$strong("Speaker:"), " Grouping variable for by-speaker random effects."),
          tags$li(tags$strong("Item:"), " The word or syllable type (e.g. different segmental compositions). Grouping variable for by-item random effects."),
          tags$li(tags$strong("Tone category:"), " Fixed effect factor interacted with time polynomial terms.")
        ),
        tags$strong("How GCA works:"),
        tags$p(style = "margin-bottom: 8px;",
          "GCA fits a single multilevel model across all data using orthogonal polynomials (",
          tags$code(style = code_style, "poly()"),
          ") as time terms. Unlike per-token polynomial fitting, GCA accounts for the hierarchical structure of the data (observations nested within tokens, tokens nested within speakers) through random effects."
        ),
        tags$strong("Fixed effects formula:"),
        tags$p(style = "margin-bottom: 8px; font-family: monospace; font-size: 0.82rem;",
          "f0 ~ (ot1 + ot2 + ...) * tone"
        ),
        tags$strong("Orthogonal polynomial terms"),
        " (the number depends on the chosen degree):",
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("ot1"), " (linear) captures the overall rising or falling slope."),
          tags$li(tags$strong("ot2"), " (quadratic) captures the curvature of the contour."),
          tags$li(tags$strong("ot3"), " (cubic) captures the asymmetry of curvature.")
        ),
        tags$p(style = "margin-bottom: 0;",
          tags$strong("Contrast coding:"), " Treatment (Dummy) Coding is used here."
        )
      )
    )
  })

  # --- Helper: get active dataset based on selector ---
  active_data <- reactive({
    has_norm <- !is.null(normalised_data())
    has_cur  <- !is.null(curated_data) && !is.null(curated_data())
    has_clu  <- !is.null(cluster_data) && !is.null(cluster_data())
    sel <- input$gca_dataset
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
  output$ui_gca <- renderUI({
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
    ds_selected <- if (!is.null(input$gca_dataset) && input$gca_dataset %in% ds_choices) {
      input$gca_dataset
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

    tagList(
      wellPanel(
        selectInput("gca_dataset", "Select dataset:",
                    choices = ds_choices, selected = ds_selected),
        selectInput("gca_token_var", "Select Token ID variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$token, 1)),
        selectInput("gca_f0_var", "Select f0 variable (normalised f0 recommended):",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$f0, 2)),
        selectInput("gca_time_var", "Select Time variable:",  # auto-prefers <tier>_tseq
                    choices = setNames(vars, var_types),
                    selected = guess_time_var(vars, 3)),
        selectInput("gca_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$speaker, 4)),
        selectInput("gca_tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$tone, 5)),
        selectInput("gca_item_var", "Select Item variable (word/syllable type):",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$item, 6)),
        tags$hr(),
        radioButtons("gca_degree", "Polynomial degree:",
                     choices = list("1 (linear)" = 1,
                                    "2 (quadratic)" = 2,
                                    "3 (cubic)" = 3),
                     selected = 2, inline = TRUE),
        tags$hr(),
        tags$strong("Random intercepts:"),
        checkboxInput("gca_ri_speaker", "Speaker", value = TRUE),
        checkboxInput("gca_ri_item", "Item", value = TRUE),
        tags$strong("Random slopes:"),
        checkboxInput("gca_rs_speaker", "Speaker", value = TRUE),
        checkboxInput("gca_rs_item", "Item", value = FALSE),
        tags$hr(),
        actionButton("gca_button", "Fit GCA Model"),
        div(style = "margin-top: 4px;",
          actionButton("gca_show_pairwise", "Pairwise Tone Comparisons",
                       icon = icon("scale-balanced"))
        ),
        div(style = "margin-top: 4px;",
          actionButton("gca_show_code", "Show R code", icon = icon("code"))
        ),
        tags$hr(),
        tags$strong("Plot options:"),
        checkboxInput("gca_overlay",
                      "Overlay observed per-tone means",
                      value = TRUE),
        tags$p(style = "font-size: 0.78rem; color: #777; margin: -4px 0 0 0;",
               "Semi-transparent points show the observed mean contour per tone, so you can see how well the fitted curves track the data. Turn off if the overlay makes a busy plot hard to read."),
        tags$hr(),
        h5("Download"),
        textInput("gca_filename", "Enter filename (without extension):",
                  value = if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                            paste0(input$dataset_name, "_gca")
                          else "gca_plot"),
        downloadButton("gca_download", "Download Plot")
      )
    )
  })

  # --- Result storage ---
  gca_model <- reactiveVal(NULL)
  gca_fixef_table <- reactiveVal(NULL)
  gca_convergence_warning <- reactiveVal(NULL)
  gca_plot_data <- reactiveVal(NULL)
  gca_obs_data <- reactiveVal(NULL)   # observed per-tone mean contour, for the overlay
  gca_formula_str <- reactiveVal(NULL)
  gca_tone_estimates <- reactiveVal(NULL)
  gca_pairwise <- reactiveVal(NULL)
  gca_pairwise_visible <- reactiveVal(FALSE)

  observeEvent(input$gca_show_pairwise, {
    if (is.null(gca_model())) {
      showNotification(
        "Fit the GCA model first, then click Pairwise Tone Comparisons.",
        type = "warning", duration = 4
      )
      return()
    }
    gca_pairwise_visible(!gca_pairwise_visible())
  })

  # --- Core computation ---
  observeEvent(input$gca_button, {
    req(active_data())
    req(input$gca_token_var, input$gca_time_var,
        input$gca_speaker_var, input$gca_tone_var,
        input$gca_f0_var)

    # Progress notification — GCA can be slow on large datasets.
    progress_id <- showNotification(
      tagList(icon("spinner", class = "fa-spin"),
              " Fitting GCA model... This may take a while."),
      duration = NULL, closeButton = FALSE, type = "message"
    )
    on.exit(removeNotification(progress_id), add = TRUE)

    # Reset convergence warning before refit.
    gca_convergence_warning(NULL)

    # All model preparation, formula construction, and fitting lives in the
    # package function fit_gca() (R/gca.R). The Shiny observer only owns
    # the UI-coupled bits: notifications, the display formula, name-cleaning
    # for the fixed-effects table, per-tone estimates, and pairwise tests.
    fit <- tryCatch(
      fit_gca(
        active_data(),
        f0      = input$gca_f0_var,
        time    = input$gca_time_var,
        token   = input$gca_token_var,
        tone    = input$gca_tone_var,
        speaker = input$gca_speaker_var,
        item    = input$gca_item_var,
        degree                    = as.integer(input$gca_degree),
        random_intercept_speaker  = isTRUE(input$gca_ri_speaker),
        random_intercept_item     = isTRUE(input$gca_ri_item),
        random_slope_speaker      = isTRUE(input$gca_rs_speaker),
        random_slope_item         = isTRUE(input$gca_rs_item)
      ),
      error = function(e) {
        showNotification(paste("Model fitting error:", e$message),
                         type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(fit)) return()

    model    <- fit$model
    degree   <- fit$degree
    ot_names <- paste0("ot", seq_len(degree))
    tone_var <- input$gca_tone_var
    f0_var   <- input$gca_f0_var

    if (!is.null(fit$convergence_warning)) {
      gca_convergence_warning(fit$convergence_warning)
    }
    gca_model(model)
    gca_formula_str(fit$formula_str)

    # Fixed-effects table with internal '.tone'/'.f0' renamed back to the
    # user's chosen column names for display.
    fe_summary <- as.data.frame(summary(model)$coefficients)
    fe_summary <- tibble::rownames_to_column(fe_summary, var = "Term")
    fe_summary$Term <- gsub("^\\.tone", paste0(tone_var, ""), fe_summary$Term)
    fe_summary$Term <- gsub("^\\.f0$",  f0_var,               fe_summary$Term)
    gca_fixef_table(fe_summary)

    # Per-tone polynomial coefficients (intercept + ot terms for each tone).
    fe <- lme4::fixef(model)
    tone_levels <- levels(model@frame$.tone)
    tone_est <- data.frame(Tone = tone_levels, stringsAsFactors = FALSE)
    for (tone in tone_levels) {
      intercept_val  <- fe["(Intercept)"]
      tone_coef_name <- paste0(".tone", tone)
      if (tone_coef_name %in% names(fe)) {
        intercept_val <- intercept_val + fe[tone_coef_name]
      }
      tone_est[tone_est$Tone == tone, "Intercept"] <- intercept_val
      for (k in seq_len(degree)) {
        ot_name <- paste0("ot", k)
        ot_val  <- fe[ot_name]
        n1 <- paste0("ot", k, ":.tone", tone)
        n2 <- paste0(".tone", tone, ":ot", k)
        if      (n1 %in% names(fe)) ot_val <- ot_val + fe[n1]
        else if (n2 %in% names(fe)) ot_val <- ot_val + fe[n2]
        tone_est[tone_est$Tone == tone, ot_name] <- ot_val
      }
    }
    gca_tone_estimates(tone_est)

    # Population-level predictions for the plot, computed via the package
    # helper that knows about the cached poly() coefs.
    plot_df <- predict_gca(fit, n = 100)
    gca_plot_data(plot_df)
    if (!is.null(gca_pred_data)) gca_pred_data(plot_df)

    # Observed per-tone mean contour on the same normalised-time / f0 scale,
    # for the optional overlay. Same columns and dataset as the fit, so its
    # tone / time / f0_predicted schema lines up with predict_gca()'s output.
    obs_df <- tryCatch(
      compute_mean_contour(active_data(),
                           token = input$gca_token_var,
                           f0    = input$gca_f0_var,
                           time  = input$gca_time_var,
                           tone  = input$gca_tone_var),
      error = function(e) NULL)
    gca_obs_data(obs_df)

    # Pairwise tone comparisons (intercept-level + per polynomial slope).
    pw_tables <- list()
    pw_tables$Intercept <- tryCatch({
      at_list <- setNames(as.list(rep(0, degree)), ot_names)
      emm <- emmeans::emmeans(model, ~ .tone, at = at_list)
      as.data.frame(pairs(emm, adjust = "tukey"))
    }, error = function(e) NULL)
    for (k in seq_len(degree)) {
      otn <- paste0("ot", k)
      pw_tables[[otn]] <- tryCatch({
        emt <- emmeans::emtrends(model, ~ .tone, var = otn)
        as.data.frame(pairs(emt, adjust = "tukey"))
      }, error = function(e) NULL)
    }
    gca_pairwise(pw_tables)
  })

  # --- Summary box ---
  output$gca_summary <- renderUI({
    req(gca_fixef_table())
    fe_table <- gca_fixef_table()
    model <- gca_model()
    warn_msg <- gca_convergence_warning()

    data <- active_data()
    token_var <- input$gca_token_var
    speaker_var <- input$gca_speaker_var
    tone_var <- input$gca_tone_var
    item_var <- input$gca_item_var
    formula_display <- gca_formula_str()

    n_obs <- nrow(model@frame)
    n_speakers <- length(unique(data[[speaker_var]]))
    n_tokens <- length(unique(data[[token_var]]))
    n_items <- length(unique(data[[item_var]]))
    n_tones <- length(unique(data[[tone_var]]))

    # Build fixed effects HTML table
    th_style <- "padding: 4px 10px; border-bottom: 2px solid #ddd; text-align: center;"
    td_style <- "padding: 4px 10px; text-align: center;"
    td_style_left <- "padding: 4px 10px; text-align: left; font-family: monospace; font-size: 0.82rem;"

    col_names <- names(fe_table)
    header_cells <- tagList(
      lapply(col_names, function(cn) {
        display_name <- cn
        if (cn == "Std. Error") display_name <- "SE"
        if (cn == "t value") display_name <- "t"
        tags$th(style = th_style, display_name)
      })
    )

    body_rows <- lapply(seq_len(nrow(fe_table)), function(i) {
      row <- fe_table[i, ]
      cells <- tagList(
        tags$td(style = td_style_left, as.character(row[["Term"]])),
        lapply(col_names[-1], function(cn) {
          tags$td(style = td_style, round(as.numeric(row[[cn]]), 4))
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
        if (!is.null(warn_msg)) {
          tags$div(
            style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 6px 10px; border-radius: 4px; margin-bottom: 8px; font-size: 0.84rem;",
            tags$strong(icon("exclamation-triangle"), " Convergence warning: "),
            warn_msg,
            tags$p(style = "margin-top: 6px; margin-bottom: 0;",
              "Try simplifying the random effects structure: remove random slopes, reduce polynomial degree, or remove a grouping variable."
            )
          )
        },
        tags$strong("Fixed effects:"),
        tags$table(
          style = "margin-top: 6px; border-collapse: collapse; font-size: 0.86rem; width: 100%;",
          tags$thead(tags$tr(header_cells)),
          tags$tbody(body_rows)
        ),
        # Per-tone estimates table
        if (!is.null(gca_tone_estimates())) {
          tone_est <- gca_tone_estimates()
          te_col_names <- names(tone_est)
          # Rename first column to user's tone variable name
          te_col_names[1] <- tone_var

          te_header_cells <- tagList(
            lapply(te_col_names, function(cn) {
              tags$th(style = th_style, cn)
            })
          )
          te_body_rows <- lapply(seq_len(nrow(tone_est)), function(i) {
            row <- tone_est[i, ]
            cells <- tagList(
              tags$td(style = td_style_left, as.character(row[[1]])),
              lapply(seq_along(te_col_names)[-1], function(j) {
                tags$td(style = td_style, round(as.numeric(row[[j]]), 4))
              })
            )
            tags$tr(cells)
          })
          tagList(
            tags$br(),
            tags$strong("Estimated polynomial terms by tone:"),
            tags$table(
              style = "margin-top: 6px; border-collapse: collapse; font-size: 0.86rem; width: 100%;",
              tags$thead(tags$tr(te_header_cells)),
              tags$tbody(te_body_rows)
            )
          )
        },
        # Pairwise tone comparisons (emmeans), shown when toggled on
        if (isTRUE(gca_pairwise_visible()) && !is.null(gca_pairwise())) {
          pw <- gca_pairwise()
          term_label <- function(nm) {
            if (nm == "Intercept") "Intercept (mean level)"
            else if (nm == "ot1") "ot1 (linear slope)"
            else if (nm == "ot2") "ot2 (quadratic curvature)"
            else if (nm == "ot3") "ot3 (cubic asymmetry)"
            else nm
          }
          render_pw_table <- function(df) {
            if (is.null(df) || nrow(df) == 0) {
              return(tags$p(style = "color: #999; font-style: italic;", "Not estimable for this model."))
            }
            cn <- names(df)
            tags$table(
              style = "margin-top: 6px; border-collapse: collapse; font-size: 0.86rem; width: 100%;",
              tags$thead(tags$tr(lapply(cn, function(x) tags$th(style = th_style, x)))),
              tags$tbody(lapply(seq_len(nrow(df)), function(i) {
                tags$tr(
                  tags$td(style = td_style_left, as.character(df[i, 1])),
                  lapply(cn[-1], function(x) {
                    v <- df[i, x]
                    if (is.numeric(v)) tags$td(style = td_style, signif(v, 4))
                    else tags$td(style = td_style, as.character(v))
                  })
                )
              }))
            )
          }
          tagList(
            tags$br(),
            tags$strong("Pairwise tone comparisons (Tukey-adjusted):"),
            tags$p(style = "color: #777; font-size: 0.82rem; margin: 4px 0 0 0;",
                   "All pairwise tone contrasts on each polynomial term, computed via emmeans / emtrends."),
            lapply(names(pw), function(nm) {
              tagList(
                tags$div(style = "margin-top: 10px;",
                  tags$em(term_label(nm))
                ),
                render_pw_table(pw[[nm]])
              )
            })
          )
        }
      )
    )
  })

  # --- Shared plot builder (used by both the on-screen plot and the download) ---
  # Fitted per-tone curves, with an optional semi-transparent overlay of the
  # observed per-tone mean contour so you can judge how well the polynomial
  # captures the data — the standard GCA "fit over data" check (Mirman 2014).
  # The overlay points inherit the ggplot() aes (time / f0_predicted / tone),
  # which is why predict_gca() and compute_mean_contour() share that schema.
  gca_build_plot <- function(pdat, obs, tone_var, f0_var, overlay) {
    show_overlay <- isTRUE(overlay) && !is.null(obs) && nrow(obs) > 0
    # Format the f0 axis title the same way the Visualise tab does
    # (f0_axis_label(): "f₀ (Hz)" / "f₀ (semitone)" / "normalised f₀"). With the
    # overlay the axis carries both observed points and the fitted line, so it
    # reads the plain quantity; without it, only the fitted curve is shown, so
    # "Predicted …" is accurate.
    y_lab <- if (show_overlay) f0_axis_label(f0_var)
             else paste0("Predicted ", f0_axis_label(f0_var))
    p <- ggplot(pdat, aes(x = time, y = f0_predicted, colour = tone))
    if (show_overlay) {
      p <- p + geom_point(data = obs, alpha = 0.4, size = 1.5,
                          show.legend = FALSE)
    }
    p +
      geom_line(linewidth = 1.2) +
      labs(
        x = "Normalised time",
        y = y_lab,
        colour = tone_var,
        caption = if (show_overlay)
                    "Points: observed per-tone means · Line: GCA model fit"
                  else NULL
      ) +
      theme_bw(base_size = 14) +
      theme(legend.position = "bottom")
  }

  # --- Model plot ---
  output$gca_plot <- renderPlot({
    req(gca_plot_data())
    gca_build_plot(gca_plot_data(), gca_obs_data(),
                   input$gca_tone_var, input$gca_f0_var,
                   isTRUE(input$gca_overlay))
  }, width = 800, height = 500)

  # --- Download handler (plot image) ---
  output$gca_download <- downloadHandler(
    filename = function() {
      paste0(input$gca_filename, ".png")
    },
    content = function(file) {
      req(gca_plot_data())
      p <- gca_build_plot(gca_plot_data(), gca_obs_data(),
                          input$gca_tone_var, input$gca_f0_var,
                          isTRUE(input$gca_overlay))
      ggsave(file, plot = p, width = 8, height = 5, dpi = 300)
      showNotification(paste("Plot saved as", paste0(input$gca_filename, ".png")),
                       type = "message", duration = 4)
    }
  )

  # --- Show R code toggle ---
  gca_code_visible <- reactiveVal(FALSE)

  observeEvent(input$gca_show_code, {
    gca_code_visible(!gca_code_visible())
  })

  output$gca_r_code <- renderUI({
    req(gca_code_visible())
    req(input$gca_token_var, input$gca_f0_var, input$gca_time_var,
        input$gca_speaker_var, input$gca_tone_var, input$gca_degree)

    token_var   <- input$gca_token_var
    f0_var      <- input$gca_f0_var
    time_var    <- input$gca_time_var
    speaker_var <- input$gca_speaker_var
    tone_var    <- input$gca_tone_var
    degree      <- as.integer(input$gca_degree)
    item_var    <- input$gca_item_var
    ri_speaker  <- input$gca_ri_speaker
    ri_item     <- input$gca_ri_item
    rs_speaker  <- input$gca_rs_speaker
    rs_item     <- input$gca_rs_item

    ot_names <- paste0("ot", 1:degree)
    ot_terms <- paste(ot_names, collapse = " + ")

    # Build random effects string
    random_parts <- c()
    if (rs_speaker) {
      random_parts <- c(random_parts, paste0("(1 + ", ot_terms, " | ", speaker_var, ")"))
    } else if (ri_speaker) {
      random_parts <- c(random_parts, paste0("(1 | ", speaker_var, ")"))
    }
    if (rs_item) {
      random_parts <- c(random_parts, paste0("(1 + ", ot_terms, " | ", item_var, ")"))
    } else if (ri_item) {
      random_parts <- c(random_parts, paste0("(1 | ", item_var, ")"))
    }
    if (length(random_parts) == 0) {
      random_parts <- paste0("(1 | ", item_var, ")")
    }

    formula_display <- paste0(f0_var, " ~ (", ot_terms, ") * ", tone_var,
                              " + ", paste(random_parts, collapse = " + "))

    # Use the actual uploaded filename if known so the snippet visually
    # matches the dataset; users may need to adjust the path on disk.
    ds_name <- if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                 paste0(input$dataset_name, ".csv")
               else "your_data.csv"

    code_text <- paste0(
      'library(dplyr)\n',
      'library(lme4)\n\n',
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
      '# Generate orthogonal polynomial time terms\n',
      'poly_matrix <- poly(dat$time_norm, ', degree, ')\n',
      paste0(sapply(1:degree, function(k) {
        paste0('dat$ot', k, ' <- poly_matrix[, ', k, ']')
      }), collapse = "\n"), '\n\n',
      '# Ensure factor variables\n',
      'dat$', tone_var, ' <- as.factor(dat$', tone_var, ')\n',
      'dat$', speaker_var, ' <- as.factor(dat$', speaker_var, ')\n',
      'dat$', item_var, ' <- as.factor(dat$', item_var, ')\n\n',
      '# Fit GCA model\n',
      'model <- lmer(\n',
      '  ', formula_display, ',\n',
      '  data = dat,\n',
      '  REML = TRUE\n',
      ')\n\n',
      '# View results\n',
      'summary(model)\n',
      'coef(summary(model))  # Fixed effects table'
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
