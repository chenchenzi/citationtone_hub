normalised_ui <- function(input, output, session, dataset, normalised_data) {

  # Guide text for the Normalise tab
  output$normalise_guide <- renderUI({
    tagList(
      tags$div(style = "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;",
        tags$strong("Normalisation guide:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("f0 (Hz):"), " The raw fundamental frequency column in Hertz."),
          tags$li(tags$strong("Speaker:"), " A speaker ID column for by-speaker normalisation."),
          tags$li(tags$strong("Tone category:"), " The column labelling tone types.")
        ),
        tags$strong("Speaker mean methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Simple average:"), " Mean f0 across all data points for each speaker. Fast and straightforward."),
          tags$li(tags$strong(HTML("Equally weighted by tone &#128077;:")), " Compute per-tone means first, then average those. This better estimates the centre of a speaker's tonal space, giving each tone equal weight.")
        ),
        tags$strong("F0 normalisation methods:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Z-score:"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "z = (f0 \u2212 \u03bc) / \u03c3"), " Centres f0 on 0 and scales by speaker variability."),
          tags$li(tags$strong("Semitone:"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "ST = 12 \u00d7 log\u2082(f0 / \u03bc)"), " Converts Hz to a perceptually uniform scale referenced on each speaker's mean.")
        )
      )
    )
  })

  # Render UI for selecting f0, speaker, and tone variables
  output$ui_normalise <- renderUI({
    ds   <- dataset()
    vars <- if (!is.null(ds)) names(ds) else c("No dataset available")
    data_types <- if (!is.null(ds)) sapply(ds, class) else rep("NA", length(vars))
    var_types  <- paste0(vars, " {", data_types, "}")

    tagList(
      wellPanel(
        h5("Dataset",
           tags$small(style = "color: #777; margin-left: 6px; font-weight: 400;",
                      input$dataset_name)),
        selectInput("f0_var", "Select f0 (Hz) variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$f0, 1), multiple = FALSE),
        selectInput("speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$speaker, 2), multiple = FALSE),
        selectInput("tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$tone, 3), multiple = FALSE),
        tags$hr(),
        radioButtons("mean_calc_method", "Speaker Mean f0 Options",
                     choices = list("Simple average" = "simple",
                                    "Equally weighted by each tone" = "weighted"),
                     selected = "weighted"),
        tags$hr(),
        radioButtons("normalisation_method", "F0 Normalisation Options",
                     choices = list("Semitone referenced on speaker mean" = "semitone",
                                    "By-speaker Z-score" = "zscore"),
                     selected = "semitone"),
        actionButton("normalise_button", "Normalise f0 (Hz)"),
        tags$hr(),
        h5("Download"),
        textInput("output_filename", "Enter filename (without extension):",
                  value = if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                            paste0(input$dataset_name, "_normalised")
                          else "normalised_data"),
        downloadButton("download_data", "Download Normalised Data")
      )
    )
  })

  # Store normalisation result
  norm_display <- reactiveVal(NULL)

  # Compute normalisation on button click
  observeEvent(input$normalise_button, {
    req(dataset())
    req(input$f0_var, input$speaker_var, input$tone_var)

    # All the dplyr work lives in the package function normalise_f0()
    # (R/normalise.R), so the same logic is also usable from scripts /
    # RMarkdown and has unit tests in tests/testthat/test-normalise.R.
    data <- normalise_f0(
      dataset(),
      f0          = input$f0_var,
      speaker     = input$speaker_var,
      tone        = input$tone_var,
      method      = input$normalisation_method,
      mean_method = input$mean_calc_method
    )

    # Store the full dataset with normalised columns (for other tabs)
    normalised_data(data)

    # Store the display subset (for the DT table and download handler)
    norm_col <- if (input$normalisation_method == "zscore") "f0_zscore" else "f0_st"
    display <- data %>%
      select(all_of(input$f0_var), all_of(input$speaker_var),
             all_of(input$tone_var), speaker_mean, all_of(norm_col))
    norm_display(display)
  })

  # Display the normalised dataset
  output$normalised_data <- DT::renderDataTable({
    req(norm_display())
    DT::datatable(norm_display())
  })

  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$output_filename, ".csv")
    },
    content = function(file) {
      req(norm_display())
      fname <- paste0(input$output_filename, ".csv")
      write.csv(norm_display(), file, row.names = FALSE)
      showNotification(paste("Data saved as", fname), type = "message", duration = 4)
    }
  )
}
