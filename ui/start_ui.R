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
  tagList(
    h2("Welcome!"),
    p("Upload a CSV file to get started. Your file should have column headers and ideally contain
      columns for time, f0, tone category, speaker ID, and token ID."),
    if (is.null(input$uploadfile)) {
      p("Once uploaded, a preview of the first 10 rows will appear below.")
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
    tags$hr(),
    h4("Recommended workflows"),
    # --- Pipeline CSS ---
    tags$style(HTML("
      .pipeline-container { margin-bottom: 18px; }
      .pipeline-label {
        font-size: 0.82rem; font-weight: 600; color: #555;
        margin-bottom: 6px; display: flex; align-items: center; gap: 6px;
      }
      .pipeline-label .badge {
        display: inline-block; padding: 2px 8px; border-radius: 10px;
        font-size: 0.72rem; font-weight: 600; color: #fff;
      }
      .pipeline-flow {
        display: flex; align-items: center; flex-wrap: wrap;
        gap: 0; padding: 4px 0;
      }
      .pipeline-step {
        display: inline-flex; align-items: center; padding: 5px 12px;
        border-radius: 6px; font-size: 0.8rem; font-weight: 500;
        white-space: nowrap;
      }
      .pipeline-step.app-step {
        background-color: #e8f5f0; border: 1.5px solid #78c2ad; color: #2a7a5a;
      }
      .pipeline-step.external-step {
        background-color: #fff8e1; border: 1.5px dashed #e0a800; color: #8a6d00;
        font-style: italic;
      }
      .pipeline-step.data-step {
        background-color: #e3f2fd; border: 1.5px solid #90caf9; color: #1565c0;
        font-weight: 600;
      }
      .pipeline-arrow {
        color: #aaa; font-size: 1.1rem; margin: 0 4px;
        display: inline-flex; align-items: center;
      }
      .pipeline-loop {
        display: inline-flex; align-items: center;
        font-size: 0.75rem; color: #999; font-style: italic; margin-left: 6px;
      }
      .pipeline-loop .loop-icon { font-size: 0.9rem; margin-right: 3px; }
      .pipeline-legend {
        display: flex; gap: 16px; margin-top: 10px; font-size: 0.75rem; color: #888;
      }
      .pipeline-legend span {
        display: inline-flex; align-items: center; gap: 4px;
      }
      .legend-box {
        display: inline-block; width: 14px; height: 14px; border-radius: 3px;
      }
    ")),
    # --- Pipeline 1: Data quality check ---
    tags$div(class = "pipeline-container",
      tags$div(class = "pipeline-label",
        tags$span(class = "badge", style = "background-color: #78c2ad;", "1"),
        "Data quality check"
      ),
      tags$div(class = "pipeline-flow",
        tags$span(class = "pipeline-step data-step", "Raw f0 data"),
        tags$span(class = "pipeline-arrow", HTML("&#10132;")),
        tags$span(class = "pipeline-step app-step", "Start"),
        tags$span(class = "pipeline-arrow", HTML("&#10132;")),
        tags$span(class = "pipeline-step app-step", "Normalise"),
        tags$span(class = "pipeline-arrow", HTML("&#10132;")),
        tags$span(class = "pipeline-step app-step", "Visualise"),
        tags$span(class = "pipeline-arrow", HTML("&#10132;")),
        tags$span(class = "pipeline-step app-step", "Inspect"),
        tags$span(class = "pipeline-arrow", HTML("&#10132;")),
        tags$span(class = "pipeline-step external-step", "Manual correction"),
        tags$span(class = "pipeline-loop",
          tags$span(class = "loop-icon", HTML("&#8634;")),
          "repeat until clean"
        )
      )
    ),
    # --- Pipeline 2: Model & summarise ---
    tags$div(class = "pipeline-container",
      tags$div(class = "pipeline-label",
        tags$span(class = "badge", style = "background-color: #78c2ad;", "2"),
        "Model & summarise"
      ),
      tags$div(class = "pipeline-flow",
        tags$span(class = "pipeline-step data-step", "Cleaned f0 data"),
        tags$span(class = "pipeline-arrow", HTML("&#10132;")),
        tags$span(class = "pipeline-step app-step", "Start"),
        tags$span(class = "pipeline-arrow", HTML("&#10132;")),
        tags$span(class = "pipeline-step app-step", "Normalise"),
        tags$span(class = "pipeline-arrow", HTML("&#10132;")),
        tags$span(class = "pipeline-step app-step", "Model"),
        tags$span(class = "pipeline-arrow", HTML("&#10132;")),
        tags$span(class = "pipeline-step app-step", "Summarise")
      )
    ),
    # --- Legend ---
    tags$div(class = "pipeline-legend", style = "margin-bottom: 24px;",
      tags$span(tags$span(class = "legend-box", style = "background-color: #e3f2fd; border: 1px solid #90caf9;"), "Data input"),
      tags$span(tags$span(class = "legend-box", style = "background-color: #e8f5f0; border: 1px solid #78c2ad;"), "In-app step"),
      tags$span(tags$span(class = "legend-box", style = "background-color: #fff8e1; border: 1px dashed #e0a800;"), "External step (e.g. Praat)")
    )
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
