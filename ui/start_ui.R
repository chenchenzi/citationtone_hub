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
    # Pull the checkbox slightly closer to the file input above.
    tags$div(style = "margin-top: -8px;",
      checkboxInput("convert_to_factor",
                    "String variables as factors (recommended)", FALSE)
    ),
    # Generous gap before the sample-data action.
    tags$div(style = "margin-top: 22px;",
      actionButton("try_sample", "Try with our sample data", icon = icon("flask"))
    ),
    tags$div(style = "color: #888; font-size: 0.78rem; margin-top: 6px; font-style: italic;",
      "The sample is just an example for you to know the data structure. ",
      "Your CSV can have different or additional columns as long as it has columns for ",
      "time, f0, tone category, speaker, and token."),
    tags$div(style = "color: #888; font-size: 0.75rem; margin-top: 4px;",
      "Source: a subset of the Changsha tone data from ",
      tags$a(href = "https://doi.org/10.1515/phon-2025-0001",
             target = "_blank", rel = "noopener noreferrer",
             "Xu (2025)"),
      "."
    )
  )

})

# Render instructional texts
output$instruction_text <- renderUI({
  tagList(
    h2("Welcome!"),
    p("Upload a CSV file to get started. Your file should have column headers and ideally contain
      columns for time, f0, tone category, speaker ID, and token ID."),
    if (is.null(dataset())) {
      p("Once uploaded, a preview of the first 10 rows will appear below. ",
        "Or click ", tags$em("Try with sample data"),
        " in the sidebar to load an example.")
    },
    tags$hr(),
    h4("What each tab does"),
    tags$ul(
      tags$li(HTML("&#128194; "), tags$strong(style = "color: #78c2ad;", "Start"), " Upload your CSV data and preview its structure."),
      tags$li(HTML("&#128269; "), tags$strong(style = "color: #78c2ad;", "View"), " Browse the full dataset in an interactive table with sorting and search."),
      tags$li(HTML("&#128200; "), tags$strong(style = "color: #78c2ad;", "Normalise"), " Apply f0 normalisation (z-score or semitone) by speaker, and download the result."),
      tags$li(HTML("&#127912; "), tags$strong(style = "color: #78c2ad;", "Visualise"), " Plot f0 contours coloured by tone category, with optional faceting by speaker. Save the plot or view the corresponding R code."),
      tags$li(HTML("&#128270; "), tags$strong(style = "color: #78c2ad;", "Inspect"), " Flag potential f0 outliers and pitch tracking artefacts using by-speaker z-scores and sample-to-sample jump detection."),
      tags$li(HTML("&#128202; "), tags$strong(style = "color: #78c2ad;", "Model: Polynomials"), " Fit Legendre polynomials to each token\u2019s f0 contour. Returns one row per token with coefficients capturing the shape of contour."),
      tags$li(HTML("&#128202; "), tags$strong(style = "color: #78c2ad;", "Model: GCA"), " Run Growth Curve Analysis using a mixed-effects model with orthogonal polynomials and configurable random effects by speaker and item."),
      tags$li(HTML("&#128202; "), tags$strong(style = "color: #78c2ad;", "Model: GAMM"), " Fit Generalised Additive Mixed Models (GAMMs) to f0 contours with configurable basis functions, random effects, and AR1 autocorrelation correction."),
      tags$li(HTML("&#128203; "), tags$strong(style = "color: #78c2ad;", "Summarise"), " Convert tone contours into Chao tone numerals (1\u20135 scale). Compare reference-line and interval-based conversion methods.")
    ),
    tags$hr()
  )
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
  if (is.null(dataset())) {
    return(tags$div(style = "color: #888; font-style: italic; margin: 8px 0;",
                    "No file uploaded yet."))
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
  req(dataset())
  head(dataset(), 10)
})

output$row_count_info <- renderText({
  req(dataset())
  total_rows <- nrow(dataset())
  paste("10 of", total_rows, "rows shown. See View-tab for details.")
})

output$data_structure <- renderPrint({
  req(dataset())
  str(dataset())
})

output$data_summary <- renderPrint({
  req(dataset())
  summary(dataset())
})

}
