normalised_ui <- function(input, output, session, dataset) {
  
  # Render UI for selecting f0, speaker, and tone variables
  output$ui_normalise <- renderUI({
    vars <- names(dataset())
    req(vars)
    data_types <- sapply(dataset(), class)
    var_types <- paste0(vars, " {", data_types, "}")
    
    tagList(
      wellPanel(
        h5(paste("Dataset:", input$dataset_name)),  # Display the dataset name
        selectInput("f0_var", "Select f0 (Hz) variable:", 
                    choices = setNames(vars, var_types), selected = vars[1], multiple = FALSE),
        selectInput("speaker_var", "Select Speaker variable:", 
                    choices = setNames(vars, var_types), selected = vars[2], multiple = FALSE),
        checkboxInput("convert_speaker_to_factor", "Convert Speaker to factor", FALSE),
        selectInput("tone_var", "Select Tone category variable:", 
                    choices = setNames(vars, var_types), selected = vars[3], multiple = FALSE),
        checkboxInput("convert_tone_to_factor", "Convert Tone category to factor", FALSE),
        tags$hr(),
        h5("Speaker Mean f0 Options"),
        radioButtons("mean_calc_method", "",
                     choices = list("Simple average" = "simple", 
                                    "Equally weighted by each tone" = "weighted"), 
                     selected = "simple"),
        tags$hr(),
        h5("F0 Normalisation Options"),
      )
    )
  })
  
  # Generate the normalised dataset
  output$normalised_data <- DT::renderDataTable({
    req(input$f0_var, input$speaker_var, input$tone_var)
    
    data <- dataset()
    
    # Convert to factors if checkboxes are selected
    if (input$convert_speaker_to_factor) {
      data[[input$speaker_var]] <- as.factor(data[[input$speaker_var]])
    }
    if (input$convert_tone_to_factor) {
      data[[input$tone_var]] <- as.factor(data[[input$tone_var]])
    }
    
    
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
    
    # Append the speaker mean column
    data <- data %>%
      left_join(speaker_means, by = input$speaker_var)
    
    data <- data %>%
      select(all_of(input$f0_var), all_of(input$speaker_var), all_of(input$tone_var), speaker_mean)
    
    # Display the updated dataset
    DT::datatable(data)
  })
}
