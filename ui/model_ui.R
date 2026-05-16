###############################################
# Model: Polynomials tab — Legendre polynomial fitting per token
###############################################

model_ui <- function(input, output, session, dataset, normalised_data) {

  # --- Guide text ---
  output$model_guide <- renderUI({
    code_style <- "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;"

    tagList(
      tags$div(
        style = "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;",
        tags$strong("Polynomial modelling guide:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Token ID:"), " A unique identifier for each token/syllable. One polynomial fit per token."),
          tags$li(tags$strong("f0:"), " An f0-related variable. Normalised f0 (e.g. semitone or z-score) is recommended for more interpretable and comparable coefficients across speakers.",
            " Use the ", tags$strong("Normalise"), " tab first, then select ", tags$em("Normalised data"),
            " from the dataset dropdown to access ", tags$code(style = code_style, "f0_normalised"), "."),
          tags$li(tags$strong("Time:"), " The time variable that orders f0 samples within each token. Will be normalised to [-1, 1] per token before fitting."),
          tags$li(tags$strong("Speaker / Tone category:"), " Meta information to keep in the output.")
        ),
        tags$strong("Legendre polynomial basis on [-1, 1]:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$code(style = code_style, "P\u2080(t) = 1")),
          tags$li(tags$code(style = code_style, "P\u2081(t) = t")),
          tags$li(tags$code(style = code_style, "P\u2082(t) = (3t\u00b2 \u2212 1) / 2")),
          tags$li(tags$code(style = code_style, "P\u2083(t) = (5t\u00b3 \u2212 3t) / 2"))
        ),
        tags$strong("Coefficient interpretations"),
        " (the number of coefficients depends on the chosen degree):",
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("c0"), " captures the mean f0 level of the contour."),
          tags$li(tags$strong("c1"), " captures the overall rising or falling slope."),
          tags$li(tags$strong("c2"), " captures the curvature: positive = concave up (valley), negative = concave down (arch)."),
          tags$li(tags$strong("c3"), " captures the asymmetry of curvature.")
        )
      )
    )
  })

  # --- Helper: get active dataset based on selector ---
  active_data <- reactive({
    has_norm <- !is.null(normalised_data())
    sel <- input$model_dataset
    if (!is.null(sel) && sel == "normalised" && has_norm) {
      normalised_data()
    } else {
      dataset()
    }
  })

  # --- Sidebar controls ---
  output$ui_model <- renderUI({
    has_norm <- !is.null(normalised_data())

    # Dataset choices
    ds_choices <- c("Uploaded data" = "uploaded")
    if (has_norm) {
      ds_choices <- c(ds_choices, "Normalised data" = "normalised")
    }
    # Preserve current selection
    ds_selected <- if (!is.null(input$model_dataset) && input$model_dataset %in% ds_choices) {
      input$model_dataset
    } else if (has_norm) {
      "normalised"
    } else {
      "uploaded"
    }

    # Get variable names from the active dataset
    active <- active_data()
    vars <- if (!is.null(active)) names(active) else c("No dataset available")
    data_types <- if (!is.null(active)) sapply(active, class) else rep("NA", length(vars))
    var_types <- paste0(vars, " {", data_types, "}")

    tagList(
      wellPanel(
        selectInput("model_dataset", "Select dataset:",
                    choices = ds_choices, selected = ds_selected),
        selectInput("model_token_var", "Select Token ID variable:",
                    choices = setNames(vars, var_types), selected = vars[1]),
        selectInput("model_f0_var", "Select f0 variable (normalised f0 recommended):",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 1, vars[2], vars[1])),
        selectInput("model_time_var", "Select Time variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 2, vars[3], vars[1])),
        selectInput("model_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 3, vars[4], vars[1])),
        selectInput("model_tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 4, vars[5], vars[1])),
        tags$hr(),
        radioButtons("model_degree", "Polynomial degree:",
                     choices = list("1 (linear)" = 1,
                                    "2 (quadratic)" = 2,
                                    "3 (cubic)" = 3),
                     selected = 2, inline = TRUE),
        actionButton("model_button", "Fit Polynomials"),
        div(style = "margin-top: 4px;",
          actionButton("model_show_code", "Show R code", icon = icon("code"))
        ),
        tags$hr(),
        h5("Download"),
        textInput("model_filename", "Enter filename (without extension):",
                  value = "polynomial_coefficients"),
        downloadButton("model_download", "Download Coefficients")
      )
    )
  })

  # --- Result storage ---
  model_result <- reactiveVal(NULL)

  # --- Core computation ---
  observeEvent(input$model_button, {
    req(active_data())
    req(input$model_token_var, input$model_time_var,
        input$model_speaker_var, input$model_tone_var,
        input$model_f0_var)

    data        <- active_data()
    token_var   <- input$model_token_var
    time_var    <- input$model_time_var
    speaker_var <- input$model_speaker_var
    tone_var    <- input$model_tone_var
    f0_var      <- input$model_f0_var
    degree      <- as.integer(input$model_degree)

    # Use the selected f0 column directly
    data <- data %>% mutate(.f0_fit = .data[[f0_var]])

    # Legendre basis matrix on [-1, 1]
    legendre_basis <- function(t, degree) {
      n <- length(t)
      B <- matrix(NA_real_, nrow = n, ncol = degree + 1)
      B[, 1] <- 1                                    # P0
      if (degree >= 1) B[, 2] <- t                   # P1
      if (degree >= 2) B[, 3] <- (3 * t^2 - 1) / 2  # P2
      if (degree >= 3) B[, 4] <- (5 * t^3 - 3 * t) / 2  # P3
      colnames(B) <- paste0("c", 0:degree)
      B
    }

    # Fit one token: normalise time, build basis, OLS
    fit_one_token <- function(token_data, degree, time_var) {
      t_raw <- token_data[[time_var]]
      t_min <- min(t_raw, na.rm = TRUE)
      t_max <- max(t_raw, na.rm = TRUE)

      coef_names <- paste0("c", 0:degree)

      # Degenerate: single time point or all same time
      if (is.na(t_min) || is.na(t_max) || t_max == t_min) {
        result <- setNames(rep(NA_real_, degree + 1), coef_names)
        result["c0"] <- mean(token_data$.f0_fit, na.rm = TRUE)
        return(as.data.frame(as.list(result)))
      }

      # Normalise time to [-1, 1]
      t_norm <- 2 * (t_raw - t_min) / (t_max - t_min) - 1
      y <- token_data$.f0_fit
      valid <- !is.na(t_norm) & !is.na(y)

      # Not enough valid points to fit
      if (sum(valid) < degree + 1) {
        result <- setNames(rep(NA_real_, degree + 1), coef_names)
        return(as.data.frame(as.list(result)))
      }

      B <- legendre_basis(t_norm[valid], degree)
      fit <- lm.fit(B, y[valid])
      as.data.frame(as.list(setNames(fit$coefficients, coef_names)))
    }

    # Per-token metadata (speaker, tone — first value per token)
    token_meta <- data %>%
      arrange(.data[[token_var]], .data[[time_var]]) %>%
      group_by(.data[[token_var]]) %>%
      summarise(
        !!speaker_var := first(.data[[speaker_var]]),
        !!tone_var    := first(.data[[tone_var]]),
        .groups = "drop"
      )

    # Fit per token
    coef_rows <- data %>%
      arrange(.data[[token_var]], .data[[time_var]]) %>%
      group_by(.data[[token_var]]) %>%
      group_map(~ fit_one_token(.x, degree, time_var))

    # Token IDs in same order as group_map
    token_ids <- data %>%
      arrange(.data[[token_var]], .data[[time_var]]) %>%
      group_by(.data[[token_var]]) %>%
      group_keys() %>%
      pull(.data[[token_var]])

    # Combine into dataframe
    coef_df <- do.call(rbind, coef_rows)
    coef_df[[token_var]] <- token_ids

    result <- coef_df %>%
      left_join(token_meta, by = token_var) %>%
      select(all_of(c(token_var, speaker_var, tone_var, paste0("c", 0:degree))))

    model_result(result)
  })

  # --- Summary box ---
  output$model_summary <- renderUI({
    req(model_result())
    result   <- model_result()
    tone_var <- input$model_tone_var

    # Infer degree from result columns
    coef_cols <- grep("^c\\d+$", names(result), value = TRUE)

    n_tokens  <- nrow(result)
    n_valid   <- sum(!is.na(result[["c0"]]))
    n_invalid <- n_tokens - n_valid

    # Per-tone summary table: n + mean of each coefficient
    tone_summary <- result %>%
      group_by(.data[[tone_var]]) %>%
      summarise(
        n = n(),
        across(all_of(coef_cols), ~ round(mean(.x, na.rm = TRUE), 3)),
        .groups = "drop"
      )

    # Build HTML table
    th_style <- "padding: 4px 10px; border-bottom: 2px solid #ddd; text-align: center;"
    td_style <- "padding: 4px 10px; text-align: center;"
    td_style_left <- "padding: 4px 10px; text-align: left;"

    header_cells <- tagList(
      tags$th(style = th_style, "Tone"),
      tags$th(style = th_style, "n"),
      lapply(coef_cols, function(cn) tags$th(style = th_style, paste0("mean ", cn)))
    )

    body_rows <- lapply(seq_len(nrow(tone_summary)), function(i) {
      row <- tone_summary[i, ]
      cells <- tagList(
        tags$td(style = td_style_left, tags$strong(as.character(row[[tone_var]]))),
        tags$td(style = td_style, row$n),
        lapply(coef_cols, function(cn) tags$td(style = td_style, row[[cn]]))
      )
      tags$tr(cells)
    })

    tagList(
      tags$div(
        style = "background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem;",
        tags$strong("Fit summary:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(paste0("Tokens fitted: ", n_valid, " / ", n_tokens)),
          if (n_invalid > 0)
            tags$li(paste0("Tokens skipped (insufficient data): ", n_invalid))
        ),
        tags$strong("Coefficient means by tone category:"),
        tags$table(
          style = "margin-top: 6px; border-collapse: collapse; font-size: 0.86rem; width: 100%;",
          tags$thead(tags$tr(header_cells)),
          tags$tbody(body_rows)
        )
      )
    )
  })

  # --- DT table ---
  output$model_data <- DT::renderDataTable({
    req(model_result())
    result <- model_result()

    # Infer coefficient columns from actual result
    coef_cols <- grep("^c\\d+$", names(result), value = TRUE)

    DT::datatable(
      result,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      )
    ) %>%
      DT::formatRound(coef_cols, 4)
  })

  # --- Coefficient-space scatter plot (plotly) ---
  # Only meaningful for degree >= 2, since with degree=1 there's only c0/c1.
  # Wrapping controls + plotlyOutput in a single uiOutput ensures the plot
  # DOM is fully removed when degree drops to 1 (plotly does not auto-clear).
  output$model_plot_block <- renderUI({
    req(model_result())
    coef_cols <- grep("^c\\d+$", names(model_result()), value = TRUE)
    if (length(coef_cols) < 2) return(NULL)  # need at least c0, c1

    # At degree=1 we only have c0/c1 → 2D only, no Z dropdown.
    # At degree>=2 we expose the full X/Y/Z choice.
    can_3d <- length(coef_cols) >= 3
    default_y <- if (can_3d) "c2" else "c1"
    default_x <- if (can_3d) "c1" else "c0"
    z_choices <- c("(none)" = "", setNames(coef_cols, coef_cols))

    tagList(
      tags$div(
        style = "margin: 10px 0 6px 0; padding: 8px 12px; background: #f5fbf8; border-left: 3px solid #78c2ad; border-radius: 4px;",
        tags$strong("Coefficient-space scatter "),
        tags$span(style = "color: #777; font-size: 0.85rem;",
                  " — each point is one token, coloured by tone. Use the camera icon on the plot toolbar to download a high-resolution PNG."),
        fluidRow(
          column(4, selectInput("model_plot_x", "X axis", choices = coef_cols, selected = default_x)),
          column(4, selectInput("model_plot_y", "Y axis", choices = coef_cols, selected = default_y)),
          if (can_3d) {
            column(4, selectInput("model_plot_z", "Z axis", choices = z_choices, selected = ""))
          }
        )
      ),
      plotly::plotlyOutput("model_plot", height = "500px")
    )
  })

  output$model_plot <- plotly::renderPlotly({
    req(model_result())
    res <- model_result()
    coef_cols <- grep("^c\\d+$", names(res), value = TRUE)
    if (length(coef_cols) < 2) return(NULL)  # nothing to scatter

    x_var <- if (!is.null(input$model_plot_x) && input$model_plot_x %in% coef_cols) input$model_plot_x else coef_cols[min(2, length(coef_cols))]
    y_var <- if (!is.null(input$model_plot_y) && input$model_plot_y %in% coef_cols) input$model_plot_y else coef_cols[1]
    z_var <- input$model_plot_z
    if (is.null(z_var) || !nzchar(z_var) || !(z_var %in% coef_cols)) z_var <- NA_character_

    tone_var    <- input$model_tone_var
    token_var   <- input$model_token_var
    speaker_var <- input$model_speaker_var

    plot_df <- res[!is.na(res[[x_var]]) & !is.na(res[[y_var]]), , drop = FALSE]
    if (!is.na(z_var)) plot_df <- plot_df[!is.na(plot_df[[z_var]]), , drop = FALSE]
    if (nrow(plot_df) == 0) return(NULL)

    plot_df$.tone    <- as.factor(plot_df[[tone_var]])
    plot_df$.token   <- as.character(plot_df[[token_var]])
    plot_df$.speaker <- as.character(plot_df[[speaker_var]])

    hover_text <- paste0(
      "<b>", plot_df$.token, "</b><br>",
      tone_var, ": ", as.character(plot_df$.tone), "<br>",
      speaker_var, ": ", plot_df$.speaker, "<br>",
      x_var, ": ", signif(plot_df[[x_var]], 4), "<br>",
      y_var, ": ", signif(plot_df[[y_var]], 4),
      if (!is.na(z_var)) paste0("<br>", z_var, ": ", signif(plot_df[[z_var]], 4)) else ""
    )

    apply_config <- function(p) {
      plotly::config(
        p,
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          filename = "polynomial_scatter",
          width = 1600, height = 1000, scale = 2
        )
      )
    }

    if (is.na(z_var)) {
      plotly::plot_ly(
        data = plot_df,
        x = ~.data[[x_var]], y = ~.data[[y_var]],
        color = ~.tone,
        text = hover_text, hoverinfo = "text",
        type = "scatter", mode = "markers",
        marker = list(size = 8, opacity = 0.75, line = list(width = 0.5, color = "#444"))
      ) |>
        plotly::layout(
          xaxis = list(title = x_var, zeroline = TRUE),
          yaxis = list(title = y_var, zeroline = TRUE),
          legend = list(title = list(text = tone_var))
        ) |>
        apply_config()
    } else {
      plotly::plot_ly(
        data = plot_df,
        x = ~.data[[x_var]], y = ~.data[[y_var]], z = ~.data[[z_var]],
        color = ~.tone,
        text = hover_text, hoverinfo = "text",
        type = "scatter3d", mode = "markers",
        marker = list(size = 4, opacity = 0.8, line = list(width = 0.3, color = "#444"))
      ) |>
        plotly::layout(
          scene = list(
            xaxis = list(title = x_var),
            yaxis = list(title = y_var),
            zaxis = list(title = z_var)
          ),
          legend = list(title = list(text = tone_var))
        ) |>
        apply_config()
    }
  })

  # --- Download handler ---
  output$model_download <- downloadHandler(
    filename = function() {
      paste0(input$model_filename, ".csv")
    },
    content = function(file) {
      req(model_result())
      fname <- paste0(input$model_filename, ".csv")
      write.csv(model_result(), file, row.names = FALSE)
      showNotification(paste("Coefficients saved as", fname), type = "message", duration = 4)
    }
  )

  # --- Show R code toggle ---
  model_code_visible <- reactiveVal(FALSE)

  observeEvent(input$model_show_code, {
    model_code_visible(!model_code_visible())
  })

  output$model_r_code <- renderUI({
    req(model_code_visible())
    req(input$model_token_var, input$model_f0_var, input$model_time_var,
        input$model_speaker_var, input$model_tone_var, input$model_degree)

    token_var   <- input$model_token_var
    f0_var      <- input$model_f0_var
    time_var    <- input$model_time_var
    speaker_var <- input$model_speaker_var
    tone_var    <- input$model_tone_var
    degree      <- as.integer(input$model_degree)

    coef_names <- paste0("c", 0:degree)
    coef_str   <- paste0('"', coef_names, '"', collapse = ", ")

    code_text <- paste0(
      'library(dplyr)\n\n',
      '# Read data\n',
      'dat <- read.csv("your_data.csv", stringsAsFactors = FALSE)\n\n',
      '# Legendre polynomial basis on [-1, 1]\n',
      'legendre_basis <- function(t, degree) {\n',
      '  B <- matrix(NA_real_, nrow = length(t), ncol = degree + 1)\n',
      '  B[, 1] <- 1                              # P0\n',
      '  if (degree >= 1) B[, 2] <- t             # P1\n',
      '  if (degree >= 2) B[, 3] <- (3*t^2 - 1) / 2  # P2\n',
      '  if (degree >= 3) B[, 4] <- (5*t^3 - 3*t) / 2  # P3\n',
      '  colnames(B) <- paste0("c", 0:degree)\n',
      '  B\n',
      '}\n\n',
      '# Fit one token\n',
      'fit_token <- function(df, degree) {\n',
      '  t_raw <- df$', time_var, '\n',
      '  t_norm <- 2 * (t_raw - min(t_raw)) / (max(t_raw) - min(t_raw)) - 1\n',
      '  B <- legendre_basis(t_norm, degree)\n',
      '  fit <- lm.fit(B, df$', f0_var, ')\n',
      '  setNames(fit$coefficients, paste0("c", 0:degree))\n',
      '}\n\n',
      '# Fit per token and combine results\n',
      'results <- dat %>%\n',
      '  arrange(', token_var, ', ', time_var, ') %>%\n',
      '  group_by(', token_var, ') %>%\n',
      '  group_modify(~ {\n',
      '    coefs <- fit_token(.x, degree = ', degree, ')\n',
      '    as.data.frame(as.list(coefs))\n',
      '  }) %>%\n',
      '  ungroup()\n\n',
      '# Add speaker and tone metadata\n',
      'meta <- dat %>%\n',
      '  group_by(', token_var, ') %>%\n',
      '  summarise(\n',
      '    ', speaker_var, ' = first(', speaker_var, '),\n',
      '    ', tone_var, ' = first(', tone_var, '),\n',
      '    .groups = "drop"\n',
      '  )\n\n',
      'results <- results %>%\n',
      '  left_join(meta, by = "', token_var, '") %>%\n',
      '  select(', token_var, ', ', speaker_var, ', ', tone_var, ', ', paste(coef_names, collapse = ", "), ')\n\n',
      'head(results)'
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
