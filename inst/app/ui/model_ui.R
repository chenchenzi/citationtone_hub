###############################################
# Model: Polynomials tab — Legendre polynomial fitting per token
###############################################

model_ui <- function(input, output, session, dataset, normalised_data, curated_data = NULL, cluster_data = NULL,
                     curated_norm_data = NULL,
                     start_cols = NULL, data_version = NULL, start_settings = NULL) {

  # The app tests bind this body to their own reactives, without these three
  # arguments in older tests: then they count as left out (NULL).
  for (nm in c("start_cols", "data_version", "start_settings"))
    if (!exists(nm, inherits = TRUE)) assign(nm, NULL)

  # A landmark axis with several parts per token: a <set>_tseq or
  # <set>_tbound (the parts in sequence), or a <set>_t01 that restarts at 0 in each part. The parts are counted in the data
  # (t01_parts(), global.R), not read from the name: a tier with one part per
  # token (a word, rhyme or vowel tier) is a whole-token axis, like
  # token_t01. Parts that cannot be counted (the set's columns are missing)
  # count as several.
  model_multi_axis <- function(tv, tok, df) {
    is.character(tv) && length(tv) == 1 && grepl("_(t01|tseq|tbound)$", tv) &&
      !identical(tv, "token_t01") && !identical(t01_parts(df, tv, tok), 1L)
  }

  # On a landmark (multisyllabic) time axis, per-token Legendre coefficients stop
  # being interpretable, so just nudge to GAMM. Which of _t01, _tseq and
  # _tbound to use is handled on the GAMM tab, where it actually matters.
  output$model_multisyl_note <- renderUI({
    tv <- input$model_time_var
    if (is.null(tv) || !grepl("_(tseq|tbound|t01)$", tv)) return(NULL)
    if (!model_multi_axis(tv, input$model_token_var,
                          tryCatch(active_data(), error = function(e) NULL))) return(NULL)
    tags$div(
      style = paste("background-color:#fff8e1; border-left:4px solid #e0a800;",
                    "padding:10px 14px; margin:8px 0; border-radius:4px;",
                    "color:#7a5d00; font-size:0.9rem;"),
      tags$span(style = "color:#e0a800;", icon("wand-magic-sparkles")),
      " Are you modelling a multisyllabic time axis? Polynomial/GCA coefficients here summarise single-syllable shapes; for multisyllabic words, consider GAMM for these contours.")
  })

  # The amber notice (edge_f0_line(), global.R): tokens whose f0 does not
  # reach 0 or 1 on a time column the fit uses as it is. Never on the real
  # time column, which the fit rescales from each token's first f0 to its
  # last. One reactive, shared by the notice and the blue note below.
  model_edges <- reactive({
    edge_f0_line(tryCatch(active_data(), error = function(e) NULL),
                 input$model_token_var, input$model_f0_var, input$model_time_var)
  })

  # The fit's own check (resolve_time_norm()): a time column that is already
  # proportional ([0, 1] per token) is mapped onto the Legendre domain with the
  # fixed 2t - 1 transform instead of a per-token rescale, so say so before the
  # user clicks Fit. *_t01 and time_prop are recognised by name (with every
  # value in [0, 1]), so removing rows does not flip this note. A <set>_t01,
  # <set>_tseq or <set>_tbound with several parts per token gets the nudge
  # above instead.
  output$model_timenorm_note <- renderUI({
    tv  <- input$model_time_var
    tok <- input$model_token_var
    if (is.null(tv) || is.null(tok)) return(NULL)
    df <- tryCatch(active_data(), error = function(e) NULL)
    if (is.null(df) || !all(c(tv, tok) %in% names(df))) return(NULL)
    if (model_multi_axis(tv, tok, df)) return(NULL)
    if (!time_already_normalised(df[[tv]], df[[tok]], name = tv)) return(NULL)
    by_name <- grepl("_t01$", tv) || tv == "time_prop"
    why <- if (by_name) "its name marks a 0&ndash;1 axis, and every value lies in that range"
           else "every value lies in that range and a typical token covers most of it; some tokens may cover less"
    tags$div(
      style = paste("background-color:#e8f5f0; border-left:4px solid #78c2ad;",
                    "padding:10px 14px; margin:8px 0; border-radius:4px;",
                    "color:#2a7a5a; font-size:0.9rem;"),
      icon("circle-info"),
      HTML(sprintf(" <code>%s</code> %s already on 0&ndash;1 (%s). The fit maps it onto the Legendre domain with the fixed transform 2&thinsp;t&nbsp;&minus;&nbsp;1, so 0 and 1 stay where the column puts them%s. To rescale each token from its first f0 to its last instead, call <code>fit_polynomial(..., time_normalised = \"no\")</code> in R.",
                 tv, if (by_name) "is" else "looks", why,
                 t01_where(tv, !is.null(model_edges())))))
  })

  output$model_track_edges <- renderUI(model_edges())

  # The Normalise step in the exported code's data-source note. The time
  # columns there need the f0 (Hz) and token columns that tab used, since
  # each token, or each part of a tier, runs from its first f0 to its last.
  # A time column the data had before Normalise gets the general line.
  norm_time_lines <- function(time_var, token_var, before = NULL) {
    q <- function(x) encodeString(x, quote = '"')
    made <- !time_var %in% names(before)
    if (made && identical(time_var, "token_t01")) {
      c("# token_t01, made with the time, token and f0 (Hz) columns that tab used:",
        sprintf('#   dat <- shinytone::normalise_time_token(dat, "<time>", %s,', q(token_var)),
        '#                                          f0 = "<f0 (Hz)>")')
    } else if (made && grepl("_(t01|tseq|tbound)$", time_var)) {
      c(sprintf("# %s, made with the time, token and f0 (Hz) columns that tab used:", time_var),
        sprintf('#   dat <- shinytone::normalise_time_landmarks(dat, "<time>", %s,',
                q(sub("_(t01|tseq|tbound)$", "", time_var))),
        sprintf('#                                              f0 = "<f0 (Hz)>", token = %s)',
                q(token_var)))
    } else {
      c("# if it made a time column, normalise_time_token() or normalise_time_landmarks()",
        "# with the f0 (Hz) and token columns that tab used (f0 =, token =).")
    }
  }

  # Where 0 and 1 fall on a column used as it is, for the blue note. Only
  # when the amber notice is not shown: then every token's (or part's) f0
  # reaches 0 and 1. With the notice, the note points to it instead.
  t01_where <- function(tv, edge) {
    if (isTRUE(edge)) return(" (see the notice above)")
    if (identical(tv, "token_t01"))
      return(": on <code>token_t01</code> from Normalise, each token&rsquo;s first and last f0")
    if (grepl("_t01$", tv))
      return(sprintf(": on <code>%s</code> from Normalise, each %s&rsquo;s first and last f0",
                     tv, sub("_t01$", "", tv)))
    if (identical(tv, "time_prop"))
      return(": on <code>time_prop</code>, the first and last frames of the extracted region")
    ""
  }

  # --- Guide text ---
  output$model_guide <- renderUI({
    code_style <- "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;"

    tagList(
      guide_box("Polynomial modelling guide",
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Token ID:"), " A unique identifier for each token/syllable. One polynomial fit per token."),
          tags$li(tags$strong("f0:"), " An f0-related variable. Normalised f0 (e.g. semitone, z-score or log z-score) is recommended for more interpretable and comparable coefficients across speakers.",
            " Use the ", tags$strong("Normalise"), " tab first, then select ", tags$em("Normalised data"),
            " (or, after Curate, ", tags$em("Curated + normalised data"), ") from the dataset dropdown to access ", tags$code(style = code_style, "f0_st"), ", ", tags$code(style = code_style, "f0_zscore"), " or ", tags$code(style = code_style, "f0_lz"), "."),
          tags$li(tags$strong("Time:"),
            " The time variable that orders f0 samples within each token. Each token is mapped onto ",
            tags$strong("[\u22121, 1]"),
            " from its first f0 to its last before fitting (the Legendre basis below is defined on that range),",
            " so rows without f0 at its edges do not move it.",
            " A column already on 0\u20131 is used as it is, with the fixed transform 2t \u2212 1: ",
            tags$code(style = code_style, "token_t01"),
            " from the Normalise tab (0 = each token's first f0, 1 = its last), or ",
            tags$code(style = code_style, "time_prop"),
            " from equal-point extraction (0 and 1 = the first and last frames of the extracted region)."),
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

  pick_dataset <- source_picker()   # selects Curated + normalised data once, when it appears

  # --- Column defaults (Start page, D8) ---
  # Token: the Start page's token column. f0: a normalised column first (the
  # f0_st, f0_zscore, f0_lz and f0_norm* columns that var_patterns$f0 ranks
  # first), then the column the Start page screens (f0_corrected, say, not
  # the raw f0), then the usual guess. Time and the other roles: as before.
  model_token_default <- function(vars) {
    tok <- start_col(start_cols, "token")
    if (!is.null(tok) && tok %in% vars) tok else guess_var(vars, var_patterns$token, 1)
  }
  model_f0_default <- function(vars) {
    g   <- guess_var(vars, var_patterns$f0, 2)
    scr <- start_col(start_cols, "f0")
    if (grepl("^f0_(st|zscore|lz)$|^f0_norm", g, ignore.case = TRUE) ||
        is.null(scr) || !scr %in% vars) g else scr
  }
  # A column picker keeps the user's own pick while the data still has it,
  # until the user picks the default again (column_picker(), global.R). A
  # picker on its default follows the new default, so a change of column on
  # the Start page reaches it.
  model_pick <- column_picker(input, c("model_token_var", "model_f0_var", "model_time_var",
                                       "model_speaker_var", "model_tone_var"))

  # --- Helper: get active dataset based on selector ---
  active_data <- reactive({
    has_norm <- !is.null(normalised_data())
    has_cur  <- !is.null(curated_data) && !is.null(curated_data())
    has_clu  <- !is.null(cluster_data) && !is.null(cluster_data())
    has_cn   <- !is.null(curated_norm_data) && !is.null(curated_norm_data())
    sel <- input$model_dataset
    if (!is.null(sel) && sel == "clustered" && has_clu) {
      cluster_data()
    } else if (!is.null(sel) && sel == "curated_norm" && has_cn) {
      curated_norm_data()
    } else if (!is.null(sel) && sel == "curated" && has_cur) {
      curated_data()
    } else if (!is.null(sel) && sel == "normalised" && has_norm) {
      normalised_data()
    } else {
      dataset()
    }
  })

  # --- Sidebar controls ---
  output$ui_model <- renderUI({
    has_norm <- !is.null(normalised_data())
    has_cur  <- !is.null(curated_data) && !is.null(curated_data())
    has_clu  <- !is.null(cluster_data) && !is.null(cluster_data())
    has_cn   <- !is.null(curated_norm_data) && !is.null(curated_norm_data())

    # Dataset choices
    ds_choices <- c("Uploaded data" = "uploaded")
    if (has_norm) {
      ds_choices <- c(ds_choices, "Normalised data" = "normalised")
    }
    if (has_cur) {
      ds_choices <- c(ds_choices, "Curated data" = "curated")
    }
    if (has_cn) {
      ds_choices <- c(ds_choices, "Curated + normalised data" = "curated_norm")
    }
    if (has_clu) {
      ds_choices <- c(ds_choices, "Clustered data" = "clustered")
    }
    # Keep the user's choice (see source_picker() in global.R)
    ds_selected <- pick_dataset(input$model_dataset, ds_choices,
                                if (has_norm) "normalised" else "uploaded",
                                stamp = if (has_cn) attr(curated_norm_data(), "run"))

    # Get variable names from the active dataset
    active <- active_data()
    vars <- if (!is.null(active)) names(active) else c("No dataset available")
    data_types <- if (!is.null(active)) sapply(active, class) else rep("NA", length(vars))
    var_types <- paste0(vars, " {", data_types, "}")
    # The time default reads the data with this token: a <tier>_tseq only
    # when some token has several parts of the tier (guess_time_var()).
    tok_sel <- model_pick("model_token_var", vars, model_token_default(vars))

    tagList(
      wellPanel(
        selectInput("model_dataset", "Select dataset:",
                    choices = ds_choices, selected = ds_selected),
        curated_reminder("model_dataset"),
        selectInput("model_token_var", "Select Token ID variable:",
                    choices = setNames(vars, var_types),
                    selected = tok_sel),
        selectInput("model_f0_var", "Select f0 variable (normalised f0 recommended):",
                    choices = setNames(vars, var_types),
                    selected = model_pick("model_f0_var", vars, model_f0_default(vars))),
        uiOutput("model_f0_note"),
        selectInput("model_time_var", "Select Time variable:",  # auto-prefers <tier>_tseq
                    choices = setNames(vars, var_types),
                    selected = model_pick("model_time_var", vars,
                                          guess_time_var(vars, 3, d = active, token = tok_sel))),
        selectInput("model_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = model_pick("model_speaker_var", vars,
                                          role_var(active, "speaker", vars, var_patterns$speaker, 4))),
        selectInput("model_tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = model_pick("model_tone_var", vars,
                                          role_var(active, "tone", vars, var_patterns$tone, 5))),
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
                  value = if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                            paste0(input$dataset_name, "_polynomials")
                          else "polynomial_coefficients"),
        downloadButton("model_download", "Download Coefficients")
      )
    )
  })

  # A note under the f0 picker when the chosen column is not the one the
  # Start page screens and still has rows without f0.
  output$model_f0_note <- renderUI({
    screened_f0_note(tryCatch(active_data(), error = function(e) NULL),
                     input$model_f0_var, start_cols)
  })

  # --- Result storage ---
  model_result <- reactiveVal(NULL)
  # A new file, or new rows from the Start page (data_version): clear the old fit
  observeEvent({ input$uploadfile; if (!is.null(data_version)) data_version() },
               model_result(NULL), ignoreInit = TRUE, ignoreNULL = FALSE)

  # --- Core computation ---
  # All Legendre fitting lives in fit_polynomial() (R/polynomial.R), which
  # is unit-tested in tests/testthat/test-polynomial.R.
  observeEvent(input$model_button, {
    req(active_data())
    req(input$model_token_var, input$model_time_var,
        input$model_speaker_var, input$model_tone_var,
        input$model_f0_var)

    # An error here would end the session, so show it instead.
    result <- tryCatch(
      fit_polynomial(
        active_data(),
        f0      = input$model_f0_var,
        token   = input$model_token_var,
        time    = input$model_time_var,
        speaker = input$model_speaker_var,
        tone    = input$model_tone_var,
        degree  = as.integer(input$model_degree)
      ),
      error = function(e) {
        showNotification(paste("Model fitting error:", e$message),
                         type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(result)) return()

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
        tags$strong("Coefficient-space scatter."),
        tags$span(style = "color: #777; font-size: 0.85rem;",
                  "Each point is one token, coloured by tone. Use the camera icon on the plot toolbar to download a high-resolution PNG."),
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

    # Scatter axes for the plotting section: mirror the app's current picks
    # (falling back to the plot block's own defaults) so the snippet
    # reproduces the scatter the user is looking at — including the Z axis,
    # which switches the app plot (and hence the snippet) to 3-D plotly.
    plot_x <- if (!is.null(input$model_plot_x) &&
                  input$model_plot_x %in% coef_names) input$model_plot_x
              else if (degree >= 2) "c1" else "c0"
    plot_y <- if (!is.null(input$model_plot_y) &&
                  input$model_plot_y %in% coef_names) input$model_plot_y
              else if (degree >= 2) "c2" else "c1"
    plot_z <- if (!is.null(input$model_plot_z) && nzchar(input$model_plot_z) &&
                  input$model_plot_z %in% coef_names) input$model_plot_z
              else NA_character_
    coef_gloss <- c(c0 = "mean level", c1 = "slope",
                    c2 = "curvature", c3 = "asymmetry")
    axis_lab <- function(v) paste0(v, " (", coef_gloss[[v]], ")")

    plot_section <- if (is.na(plot_z)) {
      paste0(
        '# Coefficient-space scatter, as in this tab: each point is one token,\n',
        '# coloured by tone\n',
        'library(ggplot2)\n',
        'ggplot(results, aes(x = ', plot_x, ', y = ', plot_y, ',\n',
        '                    colour = factor(', tone_var, '))) +\n',
        '  geom_point(alpha = 0.8, size = 2) +\n',
        '  labs(x = "', axis_lab(plot_x), '",\n',
        '       y = "', axis_lab(plot_y), '",\n',
        '       colour = "', tone_var, '") +\n',
        '  theme_minimal()'
      )
    } else {
      paste0(
        '# Coefficient-space scatter (3-D), as in this tab: each point is one\n',
        '# token, coloured by tone; drag to rotate\n',
        'library(plotly)\n',
        'plot_ly(results,\n',
        '        x = ~', plot_x, ', y = ~', plot_y, ', z = ~', plot_z, ',\n',
        '        color = ~factor(', tone_var, '),\n',
        '        type = "scatter3d", mode = "markers",\n',
        '        marker = list(size = 4, opacity = 0.8)) |>\n',
        '  layout(scene = list(\n',
        '    xaxis = list(title = "', axis_lab(plot_x), '"),\n',
        '    yaxis = list(title = "', axis_lab(plot_y), '"),\n',
        '    zaxis = list(title = "', axis_lab(plot_z), '")))'
      )
    }

    # Use the actual uploaded filename if known so the snippet visually
    # matches the dataset; users may need to adjust the path on disk.
    ds_name <- if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                 paste0(input$dataset_name, ".csv")
               else "your_data.csv"

    # The code reads the uploaded file. When this tab used other data, a note
    # at the top says so, and how to get that data. A source counts only when
    # its data exists, as in active_data().
    src      <- input$model_dataset
    src_data <- switch(if (is.null(src)) "" else src,
                       normalised = normalised_data, curated = curated_data,
                       curated_norm = curated_norm_data, clustered = cluster_data)
    if (is.null(src_data) || is.null(src_data())) src <- "uploaded"
    src_note <- switch(src,
      normalised = c(
        "# Note: this tab used the Normalised data, not the uploaded file read below.",
        "# After the lines that read and prepare that file, add the Normalise tab's",
        "# steps with that tab's settings: shinytone::normalise_f0(), and",
        norm_time_lines(time_var, token_var, tryCatch(dataset(), error = function(e) NULL))),
      curated = c(
        "# Note: this tab used the Curated data, not the uploaded file read below.",
        "# Read the Curate tab's \"Curated CSV (analysis-ready)\" download in place of",
        "# that file, and leave out the Start-page steps below: the download has them."),
      curated_norm = c(
        "# Note: this tab used the Curated + normalised data, not the uploaded file",
        "# read below. Read the Curate tab's \"Curated CSV (analysis-ready)\" download",
        "# in place of that file, and leave out the Start-page steps below: the",
        "# download has them. Then add the Normalise tab's steps with that tab's",
        "# settings: shinytone::normalise_f0(), and",
        norm_time_lines(time_var, token_var,
                        if (!is.null(curated_data)) tryCatch(curated_data(), error = function(e) NULL))),
      clustered = c(
        "# Note: this tab used the Clustered data, not the uploaded file read below.",
        "# Read the Cluster tab's \"Download clustered CSV\" file in place of that",
        "# file, and leave out the Start-page steps below: the download has them."))
    src_note <- if (length(src_note)) paste0(paste(src_note, collapse = "\n"), "\n\n") else ""

    # Mirror fit_polynomial()'s auto-detection so the snippet reproduces the
    # app's fit: an already-normalised [0, 1] time column gets the fixed
    # 2t - 1 map, anything else a per-token rescale from the token's first
    # f0 to its last. The name goes in too, as in resolve_time_norm(), so a
    # token_t01 or time_prop column keeps its decision after rows without f0
    # are removed.
    df_now  <- tryCatch(active_data(), error = function(e) NULL)
    prenorm <- !is.null(df_now) && all(c(time_var, token_var) %in% names(df_now)) &&
      time_already_normalised(df_now[[time_var]], df_now[[token_var]], name = time_var)
    # The rescale runs from the token's first f0 to its last, as
    # resolve_time_norm(f0 =) does: ok is the rows with a time and f0 (0 Hz
    # is NA after zero_hz_to_na()), so rows without f0 at the token's edges
    # do not move it, with the rows removed or kept.
    t_norm_lines <- if (prenorm) {
      paste0(
      '  # ', time_var, ' is already on [0, 1]: map it onto the Legendre domain\n',
      '  # with 2t - 1 (no per-token rescale: 0 and 1 stay where the column puts them)\n',
      '  t_norm <- 2 * t_raw - 1\n')
    } else {
      paste0(
      '  # Map time onto [-1, 1], from the token\'s first f0 to its last\n',
      '  t_min  <- min(t_raw[ok])\n',
      '  t_max  <- max(t_raw[ok])\n',
      '  t_norm <- 2 * (t_raw - t_min) / (t_max - t_min) - 1\n')
    }

    # Speaker and tone per token as fit_polynomial() reads them
    # (pick_token_values(), R/polynomial.R): the most frequent non-empty value
    # among the token's rows with f0, a tie going to the value that comes
    # first in time. first() would take the first row, which can be a row
    # without f0 or another label from a TextGrid tier. One line per column,
    # so the same column picked twice is read once.
    meta_cols  <- unique(c(speaker_var, tone_var))
    meta_lines <- paste0('    ', meta_cols, ' = pick_value(', meta_cols, ', ', time_var,
                         ', is.finite(', f0_var, ')),\n', collapse = '')

    code_text <- paste0(
      'library(dplyr)\n\n',
      src_note,
      paste(read_data_code(ds_name, start = if (is.null(start_settings)) NULL else start_settings()),
            collapse = "\n"), '\n\n',
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
      '# Fit one token. Its samples are the rows with f0 and time: rows without\n',
      '# f0 are skipped, as the app does.\n',
      'fit_token <- function(df, degree) {\n',
      '  coefs <- setNames(rep(NA_real_, degree + 1), paste0("c", 0:degree))\n',
      '  t_raw <- df$', time_var, '\n',
      '  y     <- df$', f0_var, '\n',
      '  ok    <- is.finite(t_raw) & is.finite(y)\n',
      '  if (!any(ok)) return(coefs)\n',
      '  # All samples at one time point: keep their mean as c0\n',
      '  if (min(t_raw[ok]) == max(t_raw[ok])) {\n',
      '    coefs["c0"] <- mean(y[ok])\n',
      '    return(coefs)\n',
      '  }\n',
      '  if (sum(ok) < degree + 1) return(coefs)\n',
      t_norm_lines,
      '  B   <- legendre_basis(t_norm[ok], degree)\n',
      '  fit <- lm.fit(B, y[ok])\n',
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
      '# Speaker and tone per token, as the app reads them: the most frequent\n',
      '# non-empty value among the token\'s rows with f0 (all its rows if it has\n',
      '# none). A tie goes to the value that comes first in time.\n',
      'pick_value <- function(x, time, has_f0) {\n',
      '  if (!any(has_f0)) has_f0 <- rep(TRUE, length(x))\n',
      '  empty <- is.na(x) | trimws(as.character(x)) == ""\n',
      '  keep  <- has_f0 & !empty\n',
      '  if (!any(keep)) keep <- has_f0   # every value is empty: keep it empty\n',
      '  x    <- x[keep]\n',
      '  time <- time[keep]\n',
      '  key  <- paste(is.na(x), x)       # counts NA apart from the text "NA"\n',
      '  n    <- as.vector(table(key)[key])\n',
      '  x[order(-n, time, as.character(x), method = "radix")[1]]\n',
      '}\n\n',
      'meta <- dat %>%\n',
      '  group_by(', token_var, ') %>%\n',
      '  summarise(\n',
      meta_lines,
      '    .groups = "drop"\n',
      '  )\n\n',
      'results <- results %>%\n',
      '  left_join(meta, by = "', token_var, '") %>%\n',
      '  select(', token_var, ', ', speaker_var, ', ', tone_var, ', ', paste(coef_names, collapse = ", "), ')\n\n',
      'head(results)\n\n',
      plot_section
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
