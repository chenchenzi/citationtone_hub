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
      tags$div(style = "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;",
        tags$strong("Plotting guide:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Data source:"), " Use the uploaded (raw) dataset or the normalised dataset (available after using the Normalise tab)."),
          tags$li(tags$strong("X (time):"), " A time-related or index variable, e.g., normalised time points within a syllable."),
          tags$li(tags$strong("Y (f0):"), " An f0-related variable, e.g., raw Hz or normalised (z-score, semitone)."),
          tags$li(tags$strong("Tone category:"), " The column labelling tone types (e.g., T1, T2, T3, T4)."),
          tags$li(tags$strong("Speaker* (optional):"), " A speaker ID column, used for faceting.")
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
        checkboxInput("convert_tone_to_factor", "Convert Tone category as factors", FALSE),
        selectInput("speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$speaker, 4), multiple = FALSE),
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

    if (input$convert_tone_to_factor) {
      plot_data[[input$tone_var]] <- as.factor(plot_data[[input$tone_var]])
    }

    p <- ggplot(plot_data,
                aes_string(x = input$x_var, y = input$y_var, color = input$tone_var)) +
      geom_point(alpha = 0.75) +
      scale_color_brewer(palette = "Set3") +
      labs(x = input$x_var, y = input$y_var, color = input$tone_var)

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

    if (input$convert_tone_to_factor) {
      code_lines <- c(code_lines,
        "# Convert tone category to factor",
        paste0('dat$', input$tone_var, ' <- as.factor(dat$', input$tone_var, ')'),
        ""
      )
    }

    code_lines <- c(code_lines,
      "# Create the plot",
      paste0('p <- ggplot(dat, aes(x = ', input$x_var, ', y = ', input$y_var, ', color = ', input$tone_var, ')) +'),
      "  geom_point(alpha = 0.75) +",
      '  scale_color_brewer(palette = "Set3") +',
      paste0('  labs(x = "', input$x_var, '", y = "', input$y_var, '", color = "', input$tone_var, '")')
    )

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
