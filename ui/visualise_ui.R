visualise_ui <- function(input, output, session, dataset) {
  
  # Render the UI for selecting X, Y, tone category, and speaker variables
  output$ui_visualise <- renderUI({
    vars <- if (!is.null(dataset())) names(dataset()) else c("No dataset available")  # Get variable names from the dataset
    #req(vars)  # Ensure the dataset is available
    data_types <- if (!is.null(dataset())) sapply(dataset(), class) else rep("NA", length(vars))  # Get data types for each column
    var_types <- paste0(vars, " {", data_types, "}")  # Create "x {type}" labels
    
    tagList(
      wellPanel(
        h5(paste("Dataset:", input$dataset_name)),  # Display the dataset name
        selectInput("x_var", "Select X (time) variable:", 
                    choices = setNames(vars, var_types), selected = vars[1], multiple = FALSE), 
        selectInput("y_var", "Select Y (f0) variable:", 
                    choices = setNames(vars, var_types), selected = ifelse(length(vars) > 1, vars[2], vars[1]), multiple = FALSE),
        tags$hr(),
        selectInput("tone_var", "Select Tone category variable:", 
                    choices = setNames(vars, var_types), selected = ifelse(length(vars) > 2, vars[3], vars[1]), multiple = FALSE),
        checkboxInput("convert_tone_to_factor", "Convert Tone category as factors", FALSE),
        selectInput("speaker_var", "Select Speaker variable:", 
                    choices = setNames(vars, var_types), selected = ifelse(length(vars) > 3, vars[4], vars[1]), multiple = FALSE), 
        tags$hr(),
        h5("Graph options"),
        checkboxInput("facet_by_speaker", "By-Speaker by-Tone plot", FALSE),
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
    req(!is.null(dataset()))
    req(input$x_var, input$y_var, input$tone_var, input$speaker_var)

    plot_data <- dataset()

    if (input$convert_tone_to_factor) {
      plot_data[[input$tone_var]] <- as.factor(plot_data[[input$tone_var]])
    }

    p <- ggplot(plot_data,
                aes_string(x = input$x_var, y = input$y_var, color = input$tone_var)) +
      geom_point(alpha = 0.75) +
      scale_color_brewer(palette = "Set3") +
      labs(x = input$x_var, y = input$y_var, color = input$tone_var)

    if (input$facet_by_speaker) {
      p <- p + facet_grid(get(input$speaker_var) ~ get(input$tone_var))
    }

    p
  })

  # Dynamic plot dimensions (shared by display and download)
  plot_height <- reactive({
    if (!is.null(dataset()) && input$facet_by_speaker) {
      300 + 100 * length(unique(dataset()[[input$speaker_var]]))
    } else {
      600
    }
  })

  plot_width <- reactive({
    if (!is.null(dataset()) && input$facet_by_speaker) {
      400 + 100 * length(unique(dataset()[[input$tone_var]]))
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
      ggsave(file, plot = current_plot(),
             width = plot_width() / 100, height = plot_height() / 100,
             dpi = 300, bg = "white")
    }
  )

  # Toggle visibility of R code block
  code_visible <- reactiveVal(FALSE)

  observeEvent(input$show_code_button, {
    code_visible(!code_visible())
  })

  output$r_code_output <- renderUI({
    req(code_visible())
    req(input$plot_button > 0)
    req(input$x_var, input$y_var, input$tone_var, input$speaker_var)

    # Build the R code string dynamically
    code_lines <- c(
      "library(tidyverse)",
      "",
      '# Read your data',
      paste0('dat <- read.csv("your_data.csv")'),
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

    if (input$facet_by_speaker) {
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
      'ggsave("my_plot.png", plot = p, width = 8, height = 6, dpi = 300)'
    )

    code_text <- paste(code_lines, collapse = "\n")

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