#############################################
# View table output of the selected dataset
#############################################

view_ui <- function(input, output, session, dataset) {
# Render UI for selecting variables to show in the table
output$ui_view_vars <- renderUI({
  vars <- names(dataset())  # Get the variable names from the dataset
  req(vars)  # Ensure the dataset is available
  data_types <- sapply(dataset(), class)  # Get data types for each column
  var_types <- paste0(vars, " {", data_types, "}")  # Create "x {type}" labels
  
  selectInput(
    "view_vars", "Select variables to show:",
    choices = setNames(vars, var_types),    # Populate choices with dataset variables
    selected = vars,   # Select all variables by default
    multiple = TRUE,   # Allow multiple selections
    selectize = FALSE, # Show in a list format
    size = min(15, length(vars))  # Limit list size
  )
})

# Render the main UI for viewing the dataset
output$ui_View <- renderUI({
  tagList(
    wellPanel(
      # Display the file name that the user input in the "Start" panel
      h5(paste("Dataset:", input$dataset_name)),  # Assuming `input$dataset_name` holds the file name
      uiOutput("ui_view_vars"),  # Render the variable selection UI
      numericInput("view_dec", "Decimals:", value = 2, min = 0)  # Input for rounding decimals
    ),
    #DT::dataTableOutput("dataviewer")  # Output the data table
  )
})

# Render the data table based on selected variables
output$dataviewer <- DT::renderDataTable({
  req(input$view_vars)  # Ensure that variables are selected
  dat <- dataset() %>%
    dplyr::select(all_of(input$view_vars))  # Subset the data based on selected variables

  # Handle rounding of decimal places
  isInt <- sapply(dat, is.integer)  # Check for integer columns
  isDbl <- sapply(dat, is.double)   # Check for double columns
  dec <- input$view_dec %>%
    (function(x) ifelse(is.null(x) || x < 0, 2, round(x, 0)))  # Set decimal rounding

  # Render the data table
  DT::datatable(
    dat,
    rownames = FALSE,
    options = list(
      pageLength = 10,  # Show 10 rows per page
      autoWidth = TRUE,
      columnDefs = list(
        list(orderSequence = c("desc", "asc"), targets = "_all"),
        list(className = "dt-center", targets = "_all")
      )
    )
  ) %>%
    # Apply formatting for decimals and integers
    (function(x) if (sum(isDbl) > 0) DT::formatRound(x, names(isDbl)[isDbl], dec) else x) %>%
    (function(x) if (sum(isInt) > 0) DT::formatRound(x, names(isInt)[isInt], 0) else x)
})

}
