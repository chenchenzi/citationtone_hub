normalised_ui <- function(input, output, session, dataset) {
  
  # Render UI for selecting f0, speaker, and tone variables
  output$ui_normalise <- renderUI({
    vars <- if (!is.null(dataset())) names(dataset()) else c("No dataset available")
    #req(vars)
    data_types <- if (!is.null(dataset())) sapply(dataset(), class) else rep("NA", length(vars))
    var_types <- paste0(vars, " {", data_types, "}")
    
    tagList(
      wellPanel(
        h5(paste("Dataset:", input$dataset_name)),
        selectInput("f0_var", "Select f0 (Hz) variable:", 
                    choices = setNames(vars, var_types), selected = vars[1], multiple = FALSE),
        selectInput("speaker_var", "Select Speaker variable:", 
                    choices = setNames(vars, var_types), selected = ifelse(length(vars) > 1, vars[2], vars[1]), multiple = FALSE),
        selectInput("tone_var", "Select Tone category variable:", 
                    choices = setNames(vars, var_types), selected = ifelse(length(vars) > 2, vars[3], vars[1]), multiple = FALSE),
        tags$hr(),
        radioButtons("mean_calc_method", "Speaker Mean f0 Options",
                     choices = list("Simple average" = "simple", 
                                    "Equally weighted by each tone" = "weighted"), 
                     selected = "simple"),
        tags$hr(),
        radioButtons("normalisation_method", "F0 Normalisation Options",
                     choices = list("By-speaker Z-score" = "zscore",
                                    "Semitone referenced on speaker mean" = "semitone"), 
                     selected = "zscore"),
        actionButton("normalise_button", "Normalise f0 (Hz)"),
        tags$hr(),
        h5("Download:"),
        textInput("output_filename", "Enter filename (without extension):", value = "normalised_data"),
        downloadButton("download_data", "Download Normalised Data")
      )
    )
  })
  
  # Generate the normalised dataset
  output$normalised_data <- DT::renderDataTable({
    req(input$normalise_button > 0)
    req(input$f0_var, input$speaker_var, input$tone_var)
    
    data <- dataset()
    
    # Calculate the speaker mean
    if (input$mean_calc_method == "simple") {
      speaker_means <- data %>%
        group_by(.data[[input$speaker_var]]) %>%
        summarise(speaker_mean = mean(.data[[input$f0_var]], na.rm = TRUE),.groups = "drop")
    } else if (input$mean_calc_method == "weighted") {
      speaker_means <- data %>%
        group_by(.data[[input$speaker_var]], .data[[input$tone_var]]) %>%
        summarise(tone_mean = mean(.data[[input$f0_var]], na.rm = TRUE),.groups = "drop") %>%
        group_by(.data[[input$speaker_var]]) %>%
        summarise(speaker_mean = mean(tone_mean, na.rm = TRUE),.groups = "drop")
    }
    
    # Append the speaker mean
    data <- data %>%
      left_join(speaker_means, by = input$speaker_var)
    
    # Add normalised f0
    if (input$normalisation_method == "zscore") {
      data <- data %>%
        group_by(.data[[input$speaker_var]]) %>%
        mutate(f0_normalised = (.data[[input$f0_var]] - speaker_mean) / sd(.data[[input$f0_var]], na.rm = TRUE)) %>%
        ungroup()
    } else if (input$normalisation_method == "semitone") {
      data <- data %>%
        mutate(f0_normalised = 12 * log2(.data[[input$f0_var]] / speaker_mean))
    }
    
    data <- data %>%
      select(all_of(input$f0_var), all_of(input$speaker_var), all_of(input$tone_var), speaker_mean, f0_normalised)
    
    # Store the updated dataset for download
    output$download_data <- downloadHandler(
      filename = function() {
        paste(input$output_filename, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # Display the updated dataset
    DT::datatable(data)
  })
}
