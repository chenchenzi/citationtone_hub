#######################################
# Simplified UI for loading a CSV file
#######################################

start_ui <- function(input, output, session, dataset) {
# Render UI for uploading a CSV file
output$ui_fileUpload <- renderUI({
  tagList(
    fileInput("uploadfile", "Choose a CSV File",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput("convert_to_factor", "String variables as factors (recommended)", FALSE)
  )

})

# Render instructional texts
output$instruction_text <- renderUI({
  if (is.null(input$uploadfile)) {
    tagList(
      h2("Instructions"),
      p("Please upload a CSV file to start. The file should be structured with column headers. 
        Ideally, it should contain columns for time, f0, tone category, and speaker variables."),
      p("Once the file is uploaded, you will see a preview of the first 10 rows here.")
    )
  }
})

# Observe when the file is uploaded and set the dataset name
observe({
  req(input$uploadfile)
  # Extract the name of the uploaded file
  dataset_name <- tools::file_path_sans_ext(basename(input$uploadfile$name))

  # Update the dataset name for consistent use in the app
  updateTextInput(session, "dataset_name", value = dataset_name)
})

# Create a hidden input for dataset_name to be used in other tabs
output$ui_dataset_name <- renderUI({
  div(textInput("dataset_name", NULL, value = ""), style = "display:none;")
})

# Render the dataset preview/structure/summary options
output$ui_datasets <- renderUI({
  tagList(
    radioButtons("dman_preview", "Display:",
                 c("Preview" = "preview",
                   "Structure" = "str",
                   "Summary" = "summary"),
                 selected = "preview",
                 inline = TRUE)
  )
})

# Update title dynamically based on the selected preview option
output$preview_title <- renderText({
  req(input$dman_preview) # Ensure input$dman_preview is available
  switch(input$dman_preview,
         "preview" = "Data Preview",
         "str" = "Data Structure",
         "summary" = "Data Summary")
})

# Display preview (first 10 rows), structure, or summary based on user selection
output$man_example <- renderUI({
  if (is.null(input$uploadfile)) {
    return(NULL)
  }
  
  if (input$dman_preview == "preview") {
    tagList(
      verbatimTextOutput("data_preview"),  
      textOutput("row_count_info")  # Text for row count info
    )
  } else if (input$dman_preview == "str") {
    verbatimTextOutput("data_structure")
  } else if (input$dman_preview == "summary") {
    verbatimTextOutput("data_summary")
  }
})


output$data_preview <- renderPrint({
  req(input$uploadfile)
  head(dataset(), 10)
})

output$row_count_info <- renderText({
  req(input$uploadfile)
  total_rows <- nrow(dataset())
  paste("10 of", total_rows, "rows shown. See View-tab for details.")
})

output$data_structure <- renderPrint({
  req(input$uploadfile)
  str(dataset())
})

output$data_summary <- renderPrint({
  req(input$uploadfile)
  summary(dataset())
})

}
