normalised_ui <- function(input, output, session, dataset, normalised_data) {

  # Guide text for the Normalise tab
  output$normalise_guide <- renderUI({
    tagList(
      guide_box("Normalisation guide",
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
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Z-score:"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "z = (f0 \u2212 \u03bc) / \u03c3"), " Centres f0 on 0 and scales by speaker variability."),
          tags$li(tags$strong("Semitone:"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "ST = 12 \u00d7 log\u2082(f0 / \u03bc)"), " Converts Hz to a perceptually uniform scale referenced on each speaker's mean.")
        ),
        tags$strong("Time normalisation (optional):"),
        tags$p(style = "margin: 4px 0 0 0;",
          HTML(paste0(
            "For multisyllabic words with landmark columns (from F0 Extraction), rescale time ",
            "within each segment. Adds <code>&lt;tier&gt;_t01</code> (0&ndash;1 within each segment, ",
            "for overlaying segments) and <code>&lt;tier&gt;_tseq</code> (sequential, for a ",
            "word-level time axis). Either can then be the X variable in Visualise.")))
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
        tags$hr(),
        h5("Time Normalisation options"),
        tags$p(style = "color: #777; font-size: 0.8rem; margin-bottom: 6px;",
          "Optional. For multisyllabic words, rescale time within each landmark ",
          "segment so syllables line up. Adds ", tags$code("<tier>_t01"),
          " (0–1 within each segment) and ", tags$code("<tier>_tseq"),
          " (sequential, segment by segment) to the output."),
        uiOutput("norm_time_landmark_ui"),
        tags$hr(),
        actionButton("normalise_button", "Normalise"),
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

  # Optional landmark time-normalisation controls. Always shown (an italic hint
  # stands in when the dataset carries no landmark columns), so it reads as an
  # optional step alongside the f0 options.
  output$norm_time_landmark_ui <- renderUI({
    ds   <- dataset()
    vars <- if (!is.null(ds)) names(ds) else character(0)
    sets <- landmark_sets(vars)
    if (length(sets) == 0) {
      return(tags$div(style = "color: #999; font-size: 0.78rem; font-style: italic;",
        "No landmark columns in this dataset. Add them in F0 Extraction (choose TextGrid tiers) to enable time normalisation."))
    }
    var_types <- paste0(vars, " {", sapply(ds, class), "}")
    tagList(
      selectInput("norm_time_set", "Landmark tier:",
                  choices = c("(none)" = "__none__",
                              stats::setNames(names(sets), names(sets))),
                  selected = "__none__"),
      conditionalPanel(
        "input.norm_time_set && input.norm_time_set != '__none__'",
        selectInput("norm_time_var", "Time variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$time, 1)))
    )
  })

  # Store normalisation result
  norm_display <- reactiveVal(NULL)

  # Compute normalisation on button click
  observeEvent(input$normalise_button, {
    req(dataset())
    data <- dataset()

    # --- f0 normalisation (R/normalise.R; scriptable + unit-tested) ----------
    # Wrapped so a failure (e.g. no usable speaker/tone column on raw extracted
    # data) still lets the optional time normalisation below run.
    f0_ok <- FALSE
    if (!is.null(input$f0_var) && !is.null(input$speaker_var) &&
        all(c(input$f0_var, input$speaker_var) %in% names(data))) {
      data <- tryCatch({
        d <- normalise_f0(
          data,
          f0          = input$f0_var,
          speaker     = input$speaker_var,
          tone        = input$tone_var,
          method      = input$normalisation_method,
          mean_method = input$mean_calc_method
        )
        f0_ok <- TRUE
        d
      }, error = function(e) {
        showNotification(paste("f0 normalisation skipped:", conditionMessage(e)),
                         type = "warning", duration = 6)
        data
      })
    }

    # --- optional landmark time normalisation --------------------------------
    set <- input$norm_time_set
    time_cols <- character(0)
    if (!is.null(set) && nzchar(set) && set != "__none__") {
      tvar <- input$norm_time_var
      if (is.null(tvar) || !nzchar(tvar)) tvar <- guess_var(names(data), var_patterns$time, 1)
      before    <- names(data)
      data      <- normalise_time_landmarks(data, tvar, set)
      time_cols <- setdiff(names(data), before)
      if (length(time_cols) > 0)
        showNotification(sprintf("Added landmark time column(s): %s.",
                                 paste(time_cols, collapse = ", ")),
                         type = "message", duration = 5)
    }

    # Store the full dataset with normalised columns (for other tabs)
    normalised_data(data)

    # Display subset: f0 columns (when produced) + any landmark time columns.
    norm_col <- if (input$normalisation_method == "zscore") "f0_zscore" else "f0_st"
    keep <- c(input$f0_var, input$speaker_var, input$tone_var,
              if (f0_ok) c("speaker_mean", norm_col),
              if (length(time_cols) > 0) c(paste0(set, "_i"), time_cols))
    keep <- unique(intersect(keep, names(data)))
    norm_display(if (length(keep) > 0) data[, keep, drop = FALSE] else data)
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
