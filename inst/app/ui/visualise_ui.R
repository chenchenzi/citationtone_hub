visualise_ui <- function(input, output, session, dataset, normalised_data) {

  # Active dataset for the Visualise tab (uploaded or normalised)
  vis_dataset <- reactive({
    if (!is.null(input$vis_data_source) && input$vis_data_source == "normalised" && !is.null(normalised_data())) {
      normalised_data()
    } else {
      dataset()
    }
  })

  # Variable guide box on the right panel
  output$visualise_guide <- renderUI({
    tagList(
      guide_box("Plotting guide",
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Data source:"), " Use the uploaded (raw) dataset or the normalised dataset (available after using the Normalise tab)."),
          tags$li(tags$strong("X (time):"), " A time-related or index variable, e.g., normalised time points within a syllable."),
          tags$li(tags$strong("Y (f0):"), " An f0-related variable, e.g., raw Hz or normalised (z-score, semitone)."),
          tags$li(tags$strong("Tone category:"), " The column labelling tone types (e.g., T1, T2, T3, T4)."),
          tags$li(tags$strong("Speaker* (optional):"), " A speaker ID column, used for faceting."),
          tags$li(tags$strong("Align time by landmark (optional):"),
            HTML(paste0(
              " Appears when the data carries landmark columns (e.g. ",
              "<code>syllable_start</code> / <code>syllable_end</code>, added by the ",
              "F0 Extraction TextGrid step). It renormalises time within each segment so ",
              "contours line up. For multi-syllable words, <em>side by side</em> aligns them ",
              "syllable by syllable.")))
        )
      )
    )
  })

  # Render the UI for selecting X, Y, tone category, and speaker variables
  output$ui_visualise <- renderUI({
    # Build dataset choices: always have uploaded, conditionally add normalised
    has_norm <- !is.null(normalised_data())
    data_choices <- c("Uploaded data" = "uploaded")
    if (has_norm) {
      data_choices <- c(data_choices, "Normalised data" = "normalised")
    }
    ds_selected <- if (!is.null(input$vis_data_source) && input$vis_data_source %in% data_choices) {
      input$vis_data_source
    } else {
      "uploaded"
    }

    active_data <- vis_dataset()
    vars <- if (!is.null(active_data)) names(active_data) else c("No dataset available")
    data_types <- if (!is.null(active_data)) sapply(active_data, class) else rep("NA", length(vars))
    var_types <- paste0(vars, " {", data_types, "}")
    lm_sets <- landmark_sets(vars)   # landmark columns (X_start/X_end) for alignment

    tagList(
      wellPanel(
        selectInput("vis_data_source", "Select dataset:",
                    choices = data_choices, selected = ds_selected),
        selectInput("x_var", "Select X (time) variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$time, 1), multiple = FALSE),
        selectInput("y_var", "Select Y (f0) variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$f0, 2), multiple = FALSE),
        tags$hr(),
        selectInput("tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$tone, 3), multiple = FALSE),
        checkboxInput("convert_tone_to_factor", "Convert Tone category as factors", TRUE),
        selectInput("speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$speaker, 4), multiple = FALSE),
        tags$hr(),
        if (length(lm_sets) > 0) tagList(
          selectInput("vis_align_by", "Align time by landmark (optional):",
                      choices = c("(raw X axis)" = "__none__",
                                  stats::setNames(names(lm_sets), names(lm_sets))),
                      selected = "__none__"),
          conditionalPanel(
            "input.vis_align_by && input.vis_align_by != '__none__'",
            radioButtons("vis_align_layout", NULL,
                         choices = c("Side by side (segment by segment)" = "sequential",
                                     "Overlaid (all segments at 0–1)" = "overlay"),
                         selected = "sequential"),
            tags$div(style = "color:#888; font-size:0.76rem; margin-top:-6px;",
              "Time is renormalised within each segment using the chosen ",
              tags$strong("X (time)"), " variable. ",
              tags$em("Side by side"), " lines up segment 1 of every token, then segment 2, and so on ",
              "(syllable by syllable); ", tags$em("Overlaid"), " stacks every segment on one 0–1 axis.")
          )
        ) else tagList(
          tags$div(style = "font-weight:700; font-size:0.85rem; margin-bottom:3px;",
                   "Align time by landmark ",
                   tags$span(style = "font-weight:400; color:#888; font-size:0.8rem;", "(optional)")),
          tags$div(style = "color:#999; font-size:0.78rem; font-style:italic;",
            "No landmark columns in this dataset. Add them in F0 Extraction (choose TextGrid tiers) to enable alignment.")
        ),
        tags$hr(),
        h5("Graph options"),
        radioButtons("plot_facet", NULL,
                     choices = c("All in one plot"         = "none",
                                 "By-Tone plot"            = "tone",
                                 "By-Speaker by-Tone plot" = "speaker_tone"),
                     selected = "none"),
        actionButton("plot_button", "Visualise f0 Contours"),
        downloadButton("save_plot_button", "Save Plot", icon = icon("download")),
        tags$hr(),
        actionButton("show_code_button", "Show R code", icon = icon("code"))
      )
    )
  })


  # Shared reactive to build the plot
  current_plot <- reactive({
    req(input$plot_button > 0)
    req(!is.null(vis_dataset()))
    req(input$x_var, input$y_var, input$tone_var, input$speaker_var)

    plot_data <- vis_dataset()

    # Colour by Tone only when it has a manageable number of distinct values. A
    # high-cardinality column (e.g. a continuous variable mis-selected as Tone,
    # which happens with freshly-extracted f0 data that has no tone labels yet)
    # would explode scale_color_brewer's discrete legend. Compute this on the
    # raw column BEFORE any factor coercion, and only coerce when we will really
    # colour by it: coercing otherwise would also wreck the axis when the same
    # column is mapped to y (e.g. Tone and Y both default to "f0").
    n_tone_lv      <- length(unique(plot_data[[input$tone_var]]))
    colour_by_tone <- n_tone_lv <= 12
    one_col        <- "#3b8a6e"

    # scale_color_brewer needs a discrete scale, so factor a numeric tone column
    # (e.g. integer codes 1..5). The checkbox keeps an already-factor column as-is.
    if (colour_by_tone) {
      tone_col <- plot_data[[input$tone_var]]
      if (is.numeric(tone_col) || isTRUE(input$convert_tone_to_factor)) {
        plot_data[[input$tone_var]] <- as.factor(tone_col)
      }
    }

    # ---- Optional landmark alignment ----------------------------------------
    # Renormalise the X (time) variable within each segment of a landmark set
    # (X_start / X_end). "sequential" lays segments side by side (segment index
    # i -> x in [i-1, i]) so multi-syllable words align syllable by syllable;
    # "overlay" maps every segment onto a single 0-1 axis.
    align_by <- input$vis_align_by
    aligned  <- !is.null(align_by) && nzchar(align_by) && align_by != "__none__" &&
                all(c(paste0(align_by, "_start"), paste0(align_by, "_end")) %in% names(plot_data))
    x_aes <- input$x_var; x_lab <- input$x_var
    seq_layout <- FALSE; max_i <- NA_integer_
    if (aligned) {
      sc <- paste0(align_by, "_start"); ec <- paste0(align_by, "_end"); ic <- paste0(align_by, "_i")
      tv <- suppressWarnings(as.numeric(plot_data[[input$x_var]]))
      st <- suppressWarnings(as.numeric(plot_data[[sc]]))
      en <- suppressWarnings(as.numeric(plot_data[[ec]]))
      p_in <- (tv - st) / (en - st)
      p_in[!is.finite(p_in)] <- NA_real_
      p_in <- pmin(pmax(p_in, 0), 1)
      layout <- input$vis_align_layout; if (is.null(layout)) layout <- "sequential"
      if (identical(layout, "sequential") && ic %in% names(plot_data)) {
        seg_i <- suppressWarnings(as.integer(plot_data[[ic]]))
        plot_data$.aligned_x <- (seg_i - 1) + p_in
        seq_layout <- TRUE
        max_i <- suppressWarnings(max(seg_i, na.rm = TRUE))
      } else {
        plot_data$.aligned_x <- p_in
      }
      plot_data <- plot_data[is.finite(plot_data$.aligned_x), , drop = FALSE]
      x_aes <- ".aligned_x"
      x_lab <- if (seq_layout) sprintf("%s (segment by segment)", align_by)
               else sprintf("normalised time within %s (0–1)", align_by)
      # Grouping for the connecting line: by token when laid side by side (one
      # continuous contour across segments); by token + segment when overlaid,
      # so a line doesn't wrap from the end of one segment back to 0.
      tok_col <- vis_token_col(names(plot_data))
      if (!is.null(tok_col)) {
        plot_data$.grp <- if (!seq_layout && ic %in% names(plot_data))
                            paste(plot_data[[tok_col]], plot_data[[ic]], sep = "##")
                          else as.character(plot_data[[tok_col]])
      }
    }

    # Use aes() + .data[[var]] (the modern replacement for aes_string(),
    # which was deprecated in ggplot2 3.0). colour_by_tone / one_col were
    # computed above (before any factor coercion of the tone column).
    p <- if (colour_by_tone)
      ggplot(plot_data, aes(x = .data[[x_aes]], y = .data[[input$y_var]],
                            color = .data[[input$tone_var]]))
    else
      ggplot(plot_data, aes(x = .data[[x_aes]], y = .data[[input$y_var]]))

    # Dashed segment boundaries when laying segments side by side.
    if (seq_layout && is.finite(max_i) && max_i > 1) {
      p <- p + geom_vline(xintercept = seq_len(max_i - 1), linetype = "dashed",
                          colour = "grey75", linewidth = 0.4)
    }
    # When aligned, connect each token's frames into a contour line.
    if (aligned && ".grp" %in% names(plot_data)) {
      p <- p + (if (colour_by_tone)
                  geom_line(aes(group = .data[[".grp"]]), alpha = 0.45, linewidth = 0.4)
                else
                  geom_line(aes(group = .data[[".grp"]]), colour = one_col,
                            alpha = 0.4, linewidth = 0.4))
    }
    p <- p + (if (colour_by_tone) geom_point(alpha = 0.75)
              else geom_point(colour = one_col, alpha = 0.6))
    if (colour_by_tone) p <- p + scale_color_brewer(palette = "Set3")
    p <- p + labs(
      x = x_lab, y = f0_axis_label(input$y_var),
      color = if (colour_by_tone) input$tone_var else NULL)
    if (seq_layout && is.finite(max_i)) {
      p <- p + scale_x_continuous(breaks = seq(0.5, max_i - 0.5, by = 1),
                                  labels = seq_len(max_i), minor_breaks = NULL)
    }

    facet_mode <- input$plot_facet
    if (is.null(facet_mode)) facet_mode <- "none"
    if (facet_mode == "tone") {
      p <- p + facet_wrap(reformulate(input$tone_var))
    } else if (facet_mode == "speaker_tone") {
      p <- p + facet_grid(get(input$speaker_var) ~ get(input$tone_var))
    }

    p
  })

  # Dynamic plot dimensions (shared by display and download)
  plot_height <- reactive({
    facet_mode <- input$plot_facet
    if (is.null(facet_mode)) facet_mode <- "none"
    if (!is.null(vis_dataset()) && facet_mode == "speaker_tone") {
      300 + 100 * length(unique(vis_dataset()[[input$speaker_var]]))
    } else {
      600
    }
  })

  plot_width <- reactive({
    facet_mode <- input$plot_facet
    if (is.null(facet_mode)) facet_mode <- "none"
    if (!is.null(vis_dataset()) && facet_mode %in% c("tone", "speaker_tone")) {
      n_tones <- length(unique(vis_dataset()[[input$tone_var]]))
      max(400 + 100 * n_tones, 600)
    } else {
      800
    }
  })

  # Render the plot in the UI
  output$ggplot_output <- renderPlot({
    current_plot()
  }, height = function() { plot_height() },
     width = function() { plot_width() })

  # Warn above the plot when the chosen Tone variable has too many distinct
  # values to colour by (usually a continuous column mis-selected as Tone, as
  # happens with freshly-extracted f0 data that has no tone labels yet). The
  # plot still renders, in a single colour.
  output$vis_tone_warning <- renderUI({
    req(input$plot_button > 0, input$tone_var)
    d <- vis_dataset(); req(d)
    if (!input$tone_var %in% names(d)) return(NULL)
    n_lv <- length(unique(d[[input$tone_var]]))
    if (n_lv <= 12) return(NULL)
    tags$div(
      style = paste("background-color: #fff8e1; border-left: 4px solid #e0a800;",
                    "padding: 10px 14px; margin-bottom: 10px; border-radius: 4px;",
                    "color: #7a5d00; font-size: 0.88rem;"),
      icon("triangle-exclamation"),
      HTML(sprintf(
        paste(" The <strong>Tone category</strong> variable <code>%s</code> has <strong>%d</strong>",
              "distinct values, which looks like a continuous column rather than tone categories.",
              "Contours are shown in a single colour. Pick a categorical tone column to colour by tone."),
        input$tone_var, n_lv))
    )
  })

  # Download handler for saving the plot
  output$save_plot_button <- downloadHandler(
    filename = function() {
      paste0("f0_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      fname <- paste0("f0_plot_", Sys.Date(), ".png")
      ggsave(file, plot = current_plot(),
             width = plot_width() / 100, height = plot_height() / 100,
             dpi = 300, bg = "white")
      showNotification(paste("Plot saved as", fname), type = "message", duration = 4)
    }
  )

  # Toggle visibility of R code block
  code_visible <- reactiveVal(FALSE)

  observeEvent(input$show_code_button, {
    code_visible(!code_visible())
  })

  output$r_code_output <- renderUI({
    req(code_visible())
    req(input$x_var, input$y_var, input$tone_var, input$speaker_var)

    # Friendly nudge when no plot has been generated yet
    if (is.null(input$plot_button) || input$plot_button == 0) {
      return(tags$div(
        style = "background-color: #fff8e1; border-left: 4px solid #e0a800; padding: 12px 14px; margin-top: 12px; border-radius: 4px; color: #555;",
        icon("circle-info"),
        " Click ", tags$strong("Visualise f0 Contours"),
        " first to generate the plot, then the corresponding R code will appear here."
      ))
    }

    # Build the R code string dynamically.
    # Use the actual uploaded filename if known so the snippet visually
    # matches the dataset the plot was drawn from; users may still need
    # to adjust the path to where the file lives on their machine.
    ds_name <- if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                 paste0(input$dataset_name, ".csv")
               else "your_data.csv"

    code_lines <- c(
      "library(tidyverse)",
      "",
      "# Read your data (adjust path to where the file is on your machine)",
      paste0('dat <- read.csv("', ds_name, '")'),
      ""
    )

    # Colour by tone only when it has few enough levels (mirrors the plot).
    cbt <- is.null(vis_dataset()) ||
           length(unique(vis_dataset()[[input$tone_var]])) <= 12
    if (cbt && input$convert_tone_to_factor) {
      code_lines <- c(code_lines,
        "# Convert tone category to factor",
        paste0('dat$', input$tone_var, ' <- as.factor(dat$', input$tone_var, ')'),
        ""
      )
    }

    # Landmark alignment, mirroring the displayed plot.
    align_by <- input$vis_align_by
    aligned  <- !is.null(align_by) && nzchar(align_by) && align_by != "__none__" &&
                !is.null(vis_dataset()) &&
                all(c(paste0(align_by, "_start"), paste0(align_by, "_end")) %in% names(vis_dataset()))
    x_code <- input$x_var; x_lab_code <- input$x_var
    seqL <- FALSE; n_seg <- NA_integer_
    tok_code <- if (!is.null(vis_dataset())) vis_token_col(names(vis_dataset())) else NULL
    if (aligned) {
      sc <- paste0(align_by, "_start"); ec <- paste0(align_by, "_end"); ic <- paste0(align_by, "_i")
      layout <- input$vis_align_layout; if (is.null(layout)) layout <- "sequential"
      has_i <- ic %in% names(vis_dataset())
      seqL  <- identical(layout, "sequential") && has_i
      base_norm <- paste0("pmin(pmax((dat$", input$x_var, " - dat$", sc, ") / (dat$", ec, " - dat$", sc, "), 0), 1)")
      code_lines <- c(code_lines,
        paste0("# Renormalise time within each ", align_by, " segment"),
        if (seqL) paste0("dat$aligned_x <- (dat$", ic, " - 1) + ", base_norm)
        else      paste0("dat$aligned_x <- ", base_norm),
        "dat <- dat[is.finite(dat$aligned_x), ]")
      if (!is.null(tok_code)) {
        code_lines <- c(code_lines,
          if (seqL || !has_i) paste0("dat$grp <- dat$", tok_code)
          else                paste0("dat$grp <- paste(dat$", tok_code, ", dat$", ic, ")"))
      }
      code_lines <- c(code_lines, "")
      x_code     <- "aligned_x"
      x_lab_code <- if (seqL) paste0(align_by, " (segment by segment)")
                    else      paste0("normalised time within ", align_by, " (0-1)")
      if (seqL) n_seg <- suppressWarnings(max(as.integer(vis_dataset()[[ic]]), na.rm = TRUE))
    }

    code_lines <- c(code_lines, "# Create the plot")
    if (cbt) {
      code_lines <- c(code_lines,
        paste0('p <- ggplot(dat, aes(x = ', x_code, ', y = ', input$y_var, ', color = ', input$tone_var, ')) +'))
    } else {
      code_lines <- c(code_lines,
        paste0("# Tone variable '", input$tone_var, "' has too many values to colour by; using one colour."),
        paste0('p <- ggplot(dat, aes(x = ', x_code, ', y = ', input$y_var, ')) +'))
    }
    if (seqL && is.finite(n_seg) && n_seg > 1) {
      code_lines <- c(code_lines,
        paste0('  geom_vline(xintercept = 1:', n_seg - 1, ', linetype = "dashed", colour = "grey75") +'))
    }
    if (aligned && !is.null(tok_code)) {
      code_lines <- c(code_lines,
        if (cbt) '  geom_line(aes(group = grp), alpha = 0.45, linewidth = 0.4) +'
        else     '  geom_line(aes(group = grp), colour = "#3b8a6e", alpha = 0.4, linewidth = 0.4) +')
    }
    code_lines <- c(code_lines,
      if (cbt) "  geom_point(alpha = 0.75) +"
      else     '  geom_point(colour = "#3b8a6e", alpha = 0.6) +')
    if (cbt) code_lines <- c(code_lines, '  scale_color_brewer(palette = "Set3") +')
    code_lines <- c(code_lines,
      if (cbt) paste0('  labs(x = "', x_lab_code, '", y = "', f0_axis_label(input$y_var), '", color = "', input$tone_var, '")')
      else     paste0('  labs(x = "', x_lab_code, '", y = "', f0_axis_label(input$y_var), '")'))
    if (seqL && is.finite(n_seg)) {
      code_lines <- c(code_lines,
        paste0('p <- p + scale_x_continuous(breaks = seq(0.5, ', n_seg - 0.5, ', by = 1), labels = 1:', n_seg, ')'))
    }

    facet_mode <- input$plot_facet
    if (is.null(facet_mode)) facet_mode <- "none"
    if (facet_mode == "tone") {
      code_lines <- c(code_lines,
        "",
        "# Add faceting by tone",
        paste0('p <- p + facet_wrap(~ ', input$tone_var, ')')
      )
    } else if (facet_mode == "speaker_tone") {
      code_lines <- c(code_lines,
        "",
        "# Add faceting by speaker and tone",
        paste0('p <- p + facet_grid(', input$speaker_var, ' ~ ', input$tone_var, ')')
      )
    }

    code_lines <- c(code_lines,
      "",
      "p",
      "",
      "# If you would like to save it as a PNG",
      paste0('ggsave("my_plot.png", plot = p, width = ', plot_width() / 100, ', height = ', plot_height() / 100, ', dpi = 300)')
    )

    code_text <- paste(code_lines, collapse = "\n")

    tagList(
      tags$p(style = "text-align: center; color: #999; font-style: italic; margin-top: 8px;",
             icon("arrow-down"), " Scroll down to see the R code"),
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
