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
    tags$div(style = "margin-top: -8px;",
      checkboxInput("convert_to_factor",
                    "String variables as factors (recommended)", FALSE)
    )
  )

})

# Render instructional texts
# IMPORTANT: do NOT depend on dataset() here — this renderUI builds the
# "Try with our sample data" actionButton, and re-rendering it would
# destroy + recreate the button, resetting input$try_sample back to 0
# and immediately wiping out the data the user just loaded.
output$instruction_text <- renderUI({
  tagList(
    h2("Welcome!"),
    p("Upload a CSV file to get started. Your file should have column headers and ideally contain
      columns for time, f0, tone category, speaker ID, and token ID. ",
      "Or click ", tags$em("Try with our sample data"),
      " below to load an example."),
    tags$hr(),
    # --- What the data look like (with our sample as a concrete example) ---
    h4("What the data look like"),
    tags$p("Shinytone expects a CSV with at least the following columns ",
           "(names can differ, but the content should be):"),
    tags$ul(
      tags$li(tags$strong("time"), ": time index within each token (e.g., in seconds)"),
      tags$li(tags$strong("f0"),   ": fundamental frequency in Hz"),
      tags$li(tags$strong("tone category"), ": tone label (e.g., T1, T2, ...)"),
      tags$li(tags$strong("speaker"), ": speaker ID (for by-speaker normalisation)"),
      tags$li(tags$strong("token"), ": unique recording / utterance ID")
    ),
    tags$p("Here's what our sample data looks like (first 5 rows):"),
    {
      # Read the bundled sample CSV that lives in www/ alongside the JS
      # handler that fetches it for "Try with our sample data".
      sample_preview <- tryCatch(
        utils::read.csv("www/dc21f0_test.csv",
                        stringsAsFactors = FALSE,
                        check.names = FALSE,
                        nrows = 5),
        error = function(e) NULL
      )
      if (is.null(sample_preview)) {
        tags$div(style = "color: #888; font-style: italic;",
                 "Sample preview unavailable.")
      } else {
        tags$div(style = "overflow-x: auto; margin-bottom: 8px;",
          tags$table(class = "table table-sm table-bordered",
            style = "font-size: 0.82rem; margin-bottom: 0;",
            tags$thead(
              tags$tr(lapply(names(sample_preview), function(c) tags$th(c)))
            ),
            tags$tbody(
              lapply(seq_len(nrow(sample_preview)), function(i)
                tags$tr(lapply(names(sample_preview),
                               function(c) tags$td(as.character(sample_preview[i, c])))))
            )
          )
        )
      }
    },
    tags$p(style = "color: #777; font-size: 0.82rem; font-style: italic;",
      "Source: a subset of the citation-tone recordings from ",
      tags$a(href = "https://doi.org/10.1515/phon-2025-0001",
             target = "_blank", rel = "noopener noreferrer",
             "Xu (2025)"),
      ", ",
      tags$em("Plastic Mandarin tones: regional identity in prosody"),
      ", Phonetica 82(5), 331–362."
    ),
    tags$div(style = "margin: 12px 0;",
      actionButton("try_sample", "Try with our sample data", icon = icon("flask"))
    ),
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
  ds <- dataset()
  if (is.null(ds)) {
    return(tags$div(style = "color: #888; font-style: italic; margin: 8px 0;",
                    "No file uploaded yet."))
  }

  if (input$dman_preview == "preview") {
    tagList(
      DT::dataTableOutput("data_preview"),
      textOutput("row_count_info")  # Text for row count info
    )
  } else if (input$dman_preview == "str") {
    verbatimTextOutput("data_structure")
  } else if (input$dman_preview == "summary") {
    verbatimTextOutput("data_summary")
  }
})


output$data_preview <- DT::renderDataTable({
  req(dataset())
  DT::datatable(
    head(dataset(), 10),
    rownames = FALSE,
    options  = list(
      dom         = "t",            # only the table (no search / pagination)
      pageLength  = 10,
      scrollX     = TRUE,           # horizontal scroll if many wide columns
      ordering    = FALSE,
      columnDefs  = list(list(className = "dt-center", targets = "_all"))
    )
  )
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
