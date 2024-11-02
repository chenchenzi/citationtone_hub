visualise_ui <- function(input, output, session, dataset) {
  
  # Render the UI for selecting X, Y, tone category, and speaker variables
  output$ui_visualise <- renderUI({
    vars <- names(dataset())  # Get variable names from the dataset
    req(vars)  # Ensure the dataset is available
    data_types <- sapply(dataset(), class)  # Get data types for each column
    var_types <- paste0(vars, " {", data_types, "}")  # Create "x {type}" labels
    
    tagList(
      wellPanel(
        h5(paste("Dataset:", input$dataset_name)),  # Display the dataset name
        selectInput("x_var", "Select X (time) variable:", 
                    choices = setNames(vars, var_types), selected = vars[1], multiple = FALSE), 
        selectInput("y_var", "Select Y (f0) variable:", 
                    choices = setNames(vars, var_types), selected = vars[2], multiple = FALSE),
        tags$hr(),
        selectInput("tone_var", "Select Tone category variable:", 
                    choices = setNames(vars, var_types), selected = vars[3], multiple = FALSE),
        checkboxInput("convert_tone_to_factor", "Convert Tone category as factors", FALSE),
        selectInput("speaker_var", "Select Speaker variable:", 
                    choices = setNames(vars, var_types), selected = vars[4], multiple = FALSE), 
        tags$hr(),
        h5("Graph options"),
        checkboxInput("facet_by_speaker", "By-Speaker by-Tone plot", FALSE)
      )
    )
  })
  
  
  # Generate the ggplot based on selected inputs
  output$ggplot_output <- renderPlot({
    req(input$x_var, input$y_var, input$tone_var, input$speaker_var)
    
    # Prepare the dataset for plotting
    plot_data <- dataset()
    
    if (input$convert_tone_to_factor) {
      plot_data[[input$tone_var]] <- as.factor(plot_data[[input$tone_var]])
    }
    
    # Generate the base plot
    p <- ggplot(plot_data, 
                aes_string(x = input$x_var, y = input$y_var, color = input$tone_var)) +
      geom_point(alpha = 0.75) +
      scale_color_brewer(palette = "Set3") +  
      labs(x = input$x_var, y = input$y_var, color = input$tone_var)
    
    # If faceting is enabled, add faceting by speaker
    if (input$facet_by_speaker) {
      p <- p + facet_grid(get(input$speaker_var) ~ get(input$tone_var))
    } 
    
    # Return the plot
    p
  }, height = function() { 
    if (input$facet_by_speaker) {
      # Dynamic height when faceting by speaker and tone
      return(300 + 100 * length(unique(dataset()[[input$speaker_var]])))
    } else {
      return(600)  
    }}, width = function() {
      if (input$facet_by_speaker) {
        # Dynamic width when faceting by speaker and tone
        return(400 + 100 * length(unique(dataset()[[input$tone_var]])))
      } else {
        return(800)
      }
    })
}