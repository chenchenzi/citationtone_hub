###############################################
# Model: GCA tab — Growth Curve Analysis using lme4
###############################################

gca_ui <- function(input, output, session, dataset, normalised_data, gca_pred_data = NULL) {

  # --- Guide text ---
  output$gca_guide <- renderUI({
    code_style <- "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;"

    tagList(
      tags$div(
        style = "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;",
        tags$strong("Growth Curve Analysis guide:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Token ID:"), " A unique identifier for each token/syllable."),
          tags$li(tags$strong("f0:"), " An f0-related variable. Normalised f0 (e.g. semitone or z-score) is recommended for more interpretable and comparable coefficients across speakers.",
            " Use the ", tags$strong("Normalise"), " tab first, then select ", tags$em("Normalised data"),
            " from the dataset dropdown to access ", tags$code(style = code_style, "f0_normalised"), "."),
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
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("ot1"), " (linear) captures the overall rising or falling slope."),
          tags$li(tags$strong("ot2"), " (quadratic) captures the curvature of the contour."),
          tags$li(tags$strong("ot3"), " (cubic) captures the asymmetry of curvature.")
        )
      )
    )
  })

  # --- Helper: get active dataset based on selector ---
  active_data <- reactive({
    has_norm <- !is.null(normalised_data())
    sel <- input$gca_dataset
    if (!is.null(sel) && sel == "normalised" && has_norm) {
      normalised_data()
    } else {
      dataset()
    }
  })

  # --- Sidebar controls ---
  output$ui_gca <- renderUI({
    has_norm <- !is.null(normalised_data())

    # Dataset choices
    ds_choices <- c("Uploaded data" = "uploaded")
    if (has_norm) {
      ds_choices <- c(ds_choices, "Normalised data" = "normalised")
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
                    choices = setNames(vars, var_types), selected = vars[1]),
        selectInput("gca_f0_var", "Select f0 variable (normalised f0 recommended):",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 1, vars[2], vars[1])),
        selectInput("gca_time_var", "Select Time variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 2, vars[3], vars[1])),
        selectInput("gca_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 3, vars[4], vars[1])),
        selectInput("gca_tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 4, vars[5], vars[1])),
        selectInput("gca_item_var", "Select Item variable (word/syllable type):",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 5, vars[6], vars[1])),
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
          actionButton("gca_show_code", "Show R code", icon = icon("code"))
        ),
        tags$hr(),
        h5("Download:"),
        textInput("gca_filename", "Enter filename (without extension):",
                  value = "gca_plot"),
        downloadButton("gca_download", "Download Plot")
      )
    )
  })

  # --- Result storage ---
  gca_model <- reactiveVal(NULL)
  gca_fixef_table <- reactiveVal(NULL)
  gca_convergence_warning <- reactiveVal(NULL)
  gca_plot_data <- reactiveVal(NULL)
  gca_formula_str <- reactiveVal(NULL)
  gca_tone_estimates <- reactiveVal(NULL)

  # --- Core computation ---
  observeEvent(input$gca_button, {
    req(active_data())
    req(input$gca_token_var, input$gca_time_var,
        input$gca_speaker_var, input$gca_tone_var,
        input$gca_f0_var)

    data        <- active_data()
    token_var   <- input$gca_token_var
    time_var    <- input$gca_time_var
    speaker_var <- input$gca_speaker_var
    tone_var    <- input$gca_tone_var
    f0_var      <- input$gca_f0_var
    degree      <- as.integer(input$gca_degree)
    item_var    <- input$gca_item_var
    ri_speaker  <- input$gca_ri_speaker
    ri_item     <- input$gca_ri_item
    rs_speaker  <- input$gca_rs_speaker
    rs_item     <- input$gca_rs_item

    # Reset convergence warning
    gca_convergence_warning(NULL)

    # Prepare data: normalise time to [0, 1] within each token
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

    # Generate orthogonal polynomial terms
    poly_matrix <- poly(dat$.time_norm, degree)
    ot_names <- paste0("ot", 1:degree)
    for (i in 1:degree) {
      dat[[ot_names[i]]] <- poly_matrix[, i]
    }

    # Rename columns for formula building
    dat$.f0 <- dat[[f0_var]]
    dat$.tone <- as.factor(dat[[tone_var]])
    dat$.speaker <- as.factor(dat[[speaker_var]])
    dat$.item <- as.factor(dat[[item_var]])

    # Build formula
    ot_terms <- paste(ot_names, collapse = " + ")
    fixed_part <- paste0(".f0 ~ (", ot_terms, ") * .tone")

    random_parts <- c()
    # Speaker: slopes include intercept; intercept-only if no slopes
    if (rs_speaker) {
      random_parts <- c(random_parts, paste0("(1 + ", ot_terms, " | .speaker)"))
    } else if (ri_speaker) {
      random_parts <- c(random_parts, "(1 | .speaker)")
    }
    # Item: slopes include intercept; intercept-only if no slopes
    if (rs_item) {
      random_parts <- c(random_parts, paste0("(1 + ", ot_terms, " | .item)"))
    } else if (ri_item) {
      random_parts <- c(random_parts, "(1 | .item)")
    }

    if (length(random_parts) == 0) {
      # If no random effects selected, use at least an item intercept
      random_parts <- "(1 | .item)"
    }

    formula_str <- paste0(fixed_part, " + ", paste(random_parts, collapse = " + "))
    # Store a user-readable version of the formula (with original variable names)
    ot_terms_display <- paste(ot_names, collapse = " + ")
    random_display <- c()
    if (rs_speaker) {
      random_display <- c(random_display, paste0("(1 + ", ot_terms_display, " | ", speaker_var, ")"))
    } else if (ri_speaker) {
      random_display <- c(random_display, paste0("(1 | ", speaker_var, ")"))
    }
    if (rs_item) {
      random_display <- c(random_display, paste0("(1 + ", ot_terms_display, " | ", item_var, ")"))
    } else if (ri_item) {
      random_display <- c(random_display, paste0("(1 | ", item_var, ")"))
    }
    if (length(random_display) == 0) random_display <- paste0("(1 | ", item_var, ")")
    display_formula <- paste0(f0_var, " ~ (", ot_terms_display, ") * ", tone_var,
                              " + ", paste(random_display, collapse = " + "))
    gca_formula_str(display_formula)

    model_formula <- as.formula(formula_str)

    # Fit model with convergence warning capture
    warn_msg <- NULL
    model <- tryCatch(
      withCallingHandlers(
        lme4::lmer(model_formula, data = dat, REML = TRUE),
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

    if (!is.null(warn_msg)) {
      gca_convergence_warning(warn_msg)
    }

    # Store model
    gca_model(model)

    # Extract fixed effects table
    fe_summary <- as.data.frame(summary(model)$coefficients)
    fe_summary <- tibble::rownames_to_column(fe_summary, var = "Term")

    # Clean up term names: replace internal names with user's variable names
    fe_summary$Term <- gsub("^\\.tone", paste0(tone_var, ""), fe_summary$Term)
    fe_summary$Term <- gsub("^\\.f0$", f0_var, fe_summary$Term)

    gca_fixef_table(fe_summary)

    # Get fixed effect coefficients
    fe <- lme4::fixef(model)

    # Compute per-tone estimates (intercept + ot terms for each tone level)
    tone_levels <- levels(dat$.tone)
    tone_est <- data.frame(Tone = tone_levels, stringsAsFactors = FALSE)
    for (tone in tone_levels) {
      # Intercept for this tone
      intercept_val <- fe["(Intercept)"]
      tone_coef_name <- paste0(".tone", tone)
      if (tone_coef_name %in% names(fe)) {
        intercept_val <- intercept_val + fe[tone_coef_name]
      }
      tone_est[tone_est$Tone == tone, "Intercept"] <- intercept_val
      # ot terms for this tone
      for (k in 1:degree) {
        ot_name <- paste0("ot", k)
        ot_val <- fe[ot_name]
        int_name <- paste0("ot", k, ":.tone", tone)
        int_name2 <- paste0(".tone", tone, ":ot", k)
        if (int_name %in% names(fe)) {
          ot_val <- ot_val + fe[int_name]
        } else if (int_name2 %in% names(fe)) {
          ot_val <- ot_val + fe[int_name2]
        }
        tone_est[tone_est$Tone == tone, ot_name] <- ot_val
      }
    }
    gca_tone_estimates(tone_est)

    # Prepare plot data: predicted values from fixed effects by tone
    time_seq <- seq(0, 1, length.out = 100)
    poly_pred <- poly(time_seq, degree)

    tone_levels <- levels(dat$.tone)
    plot_rows <- list()

    for (tone in tone_levels) {
      # Build design matrix for this tone
      pred_f0 <- rep(0, length(time_seq))
      for (j in seq_along(time_seq)) {
        # Intercept
        val <- fe["(Intercept)"]
        # Tone intercept (if not reference level)
        tone_coef_name <- paste0(".tone", tone)
        if (tone_coef_name %in% names(fe)) {
          val <- val + fe[tone_coef_name]
        }
        # ot terms
        for (k in 1:degree) {
          ot_name <- paste0("ot", k)
          val <- val + fe[ot_name] * poly_pred[j, k]
          # ot:tone interaction
          int_name <- paste0("ot", k, ":.tone", tone)
          int_name2 <- paste0(".tone", tone, ":ot", k)
          if (int_name %in% names(fe)) {
            val <- val + fe[int_name] * poly_pred[j, k]
          } else if (int_name2 %in% names(fe)) {
            val <- val + fe[int_name2] * poly_pred[j, k]
          }
        }
        pred_f0[j] <- val
      }
      plot_rows[[length(plot_rows) + 1]] <- data.frame(
        time = time_seq,
        f0_predicted = pred_f0,
        tone = tone,
        stringsAsFactors = FALSE
      )
    }

    gca_plot_data(do.call(rbind, plot_rows))

    # Export to shared prediction store for Summarise tab
    if (!is.null(gca_pred_data)) {
      gca_pred_data(do.call(rbind, plot_rows))
    }
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
        }
      )
    )
  })

  # --- Model plot ---
  output$gca_plot <- renderPlot({
    req(gca_plot_data())
    pdat <- gca_plot_data()
    tone_var <- input$gca_tone_var
    f0_var <- input$gca_f0_var

    ggplot(pdat, aes(x = time, y = f0_predicted, colour = tone)) +
      geom_line(linewidth = 1.2) +
      labs(
        x = "Normalised time",
        y = paste0("Predicted ", f0_var),
        colour = tone_var
      ) +
      theme_bw(base_size = 14) +
      theme(legend.position = "bottom")
  }, height = function() { if (!is.null(gca_plot_data())) 400 else 1 })

  # --- Download handler (plot image) ---
  output$gca_download <- downloadHandler(
    filename = function() {
      paste0(input$gca_filename, ".png")
    },
    content = function(file) {
      req(gca_plot_data())
      pdat <- gca_plot_data()
      tone_var <- input$gca_tone_var
      f0_var <- input$gca_f0_var

      p <- ggplot(pdat, aes(x = time, y = f0_predicted, colour = tone)) +
        geom_line(linewidth = 1.2) +
        labs(
          x = "Normalised time",
          y = paste0("Predicted ", f0_var),
          colour = tone_var
        ) +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom")

      ggsave(file, plot = p, width = 8, height = 5, dpi = 300)
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

    code_text <- paste0(
      'library(dplyr)\n',
      'library(lme4)\n\n',
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
