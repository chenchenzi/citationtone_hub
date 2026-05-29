###############################################
# Inspect tab: F0 outlier & artefact detection
###############################################

inspect_ui <- function(input, output, session, dataset) {

  # Guide text for the Inspect tab
  output$inspect_guide <- renderUI({
    tagList(
      tags$div(style = "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;",
        tags$strong("Inspect guide:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("f0 (Hz):"), " The raw fundamental frequency column in Hertz."),
          tags$li(tags$strong("Token ID:"), " A unique identifier for each token/syllable (groups rows belonging to the same contour)."),
          tags$li(tags$strong("Time:"), " The time variable that orders f0 samples within each token."),
          tags$li(tags$strong("Speaker:"), " A speaker ID for by-speaker z-score computation."),
          tags$li(tags$strong("Tone category:"), " The column labelling tone types.")
        ),
        tags$strong("Three complementary checks:"),
        tags$ul(style = "margin: 4px 0 8px 0; padding-left: 18px;",
          tags$li(
            tags$em("Extreme-value"), " (speaker-level): per-token max / min, z-scored against the ",
            "speaker's other tokens. Flags a token mis-tracked as a whole, such as an octave ",
            "too high or low, or one pushed to an extreme by creak or diplophonia."
          ),
          tags$li(
            tags$em("Token-level"), " (speaker × tone): per-token median, compared against the ",
            tags$strong("same speaker and tone"), ". Flags a contour smoothly shifted too high ",
            "or low for its tone but still appearing normal for the speaker."
          ),
          tags$li(
            tags$em("Sample-level"), " (within token): rate of change, octave bounds, and carryover, ",
            tags$strong("frame by frame"), ". Flags individual mis-tracked samples in an otherwise normal token."
          )
        ),
        tags$p(style = "margin: 0 0 8px 0;",
          tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "flagged_token = TRUE"),
          " whenever any check fires. The token-level check needs at least 4 same-speaker-same-tone tokens."
        ),
        tags$strong("Detection methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Extreme-value (speaker-level):"), " Computes per-token summary statistics (max, min, mean, SD), then z-scores the per-token ", tags$em("max"), " and ", tags$em("min"), " within each speaker. Tokens whose max or min exceeds the threshold are flagged as ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "max too high"), " or ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "min too low"), ". The mean and SD are reported alongside (in the table and tooltips) for context, but the flag decision uses only the max / min z-scores."),
          tags$li(tags$strong("Token-level (speaker × tone):"), " Computes each token's median f0 in semitones, then a ", tags$em("modified z-score"), " (median/MAD-based; Iglewicz & Hoaglin, 1993) against other tokens of the same speaker and tone. Tokens beyond the threshold are flagged ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "level too high"), " or ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "level too low"), ". This catches smoothly shifted contours that the other checks miss. The median (not mean) is used so a few jump-flagged frames don't shift a token's apparent level; groups with fewer than the minimum number of tokens are skipped."),
          tags$li(tags$strong("Jump detection:"), " Computes sample-to-sample semitone differences within each token. Flags where the change exceeds a rise or fall threshold, labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "jump (rise)"), " or ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "jump (fall)"), ". When a jump is detected, the side ", tags$em("farther from the token median"), " is flagged, so artefacts at the start of a token are caught correctly. Adapted from Steffman & Cole (2022)."),
          tags$li(tags$strong("Octave jumps:"), " Flags samples where the Hz ratio to the previous sample is < 0.49 or > 1.99 (pitch halving/doubling), labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "octave jump"), "."),
          tags$li(tags$strong("Carryover:"), " Samples ", tags$em("around"), " a detected artefact (forward and backward) that stay within ", tags$em("mult"), "\u00d7 the rise/fall threshold (in semitones) of the artefact\u2019s f0. These may still be erroneous even if their own step is small, labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "carryover"), ". The band is direction-specific: ", tags$code("rise_threshold \u00d7 mult"), " when the trend is rising, ", tags$code("fall_threshold \u00d7 mult"), " when falling (adapted from Steffman & Cole, 2022). The multiplier defaults to ", tags$strong("1.5"), " (paper\u2019s value) and is configurable in the sidebar (set to 0 to disable).")
        ),
        tags$strong("Default thresholds:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Extreme-value (z-score) = 3:"), " A standard convention for outlier detection (\u00b13 SD covers 99.7% of a normal distribution)."),
          tags$li(tags$strong("Rise = 1.263 ST, Fall = 1.714 ST per 10ms:"), " Based on the maximum rate of f0 change in human speech production (Sundberg, 1973). Changes exceeding these rates are physiologically implausible and likely tracking errors."),
          tags$li(tags$strong("Token-level (modified z) = 3.5:"), " The cutoff recommended by Iglewicz & Hoaglin (1993) for the median/MAD modified z-score, the robust analogue of the \u00b13 SD rule. Requires at least 4 same-speaker-same-tone tokens.")
        )
      )
    )
  })

  # Sidebar controls
  output$ui_inspect <- renderUI({
    vars <- if (!is.null(dataset())) names(dataset()) else c("No dataset available")
    data_types <- if (!is.null(dataset())) sapply(dataset(), class) else rep("NA", length(vars))
    var_types <- paste0(vars, " {", data_types, "}")

    tagList(
      wellPanel(
        h5("Dataset",
           tags$small(style = "color: #777; margin-left: 6px; font-weight: 400;",
                      input$dataset_name)),
        selectInput("inspect_f0_var", "Select f0 (Hz) variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$f0, 1)),
        selectInput("inspect_token_var", "Select Token ID variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$token, 2)),
        selectInput("inspect_time_var", "Select Time variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$time, 3)),
        selectInput("inspect_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$speaker, 4)),
        selectInput("inspect_tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$tone, 5)),
        tags$hr(),
        radioButtons("inspect_time_unit", "Time unit:",
                     choices = list("Milliseconds" = "ms",
                                    "Seconds" = "s"),
                     selected = "s", inline = TRUE),
        tags$hr(),
        h5("Thresholds"),
        numericInput("inspect_z_thresh", "Extreme-value threshold (z-score):",
                     value = 3, min = 1, max = 6, step = 0.5),
        numericInput("inspect_rise_thresh", "Rise threshold (ST per 10ms):",
                     value = 1.263, min = 0.1, max = 5, step = 0.1),
        numericInput("inspect_fall_thresh", "Fall threshold (ST per 10ms):",
                     value = 1.714, min = 0.1, max = 5, step = 0.1),
        numericInput("inspect_carryover_mult",
                     "Carryover band (× threshold):",
                     value = 1.5, min = 0, max = 3, step = 0.1),
        numericInput("inspect_level_thresh",
                     "Token-level threshold (modified z):",
                     value = 3.5, min = 1, max = 8, step = 0.5),
        tags$hr(),
        actionButton("inspect_button", "Run Inspection"),
        tags$hr(),
        h5("Download"),
        textInput("inspect_filename", "Enter filename (without extension):",
                  value = if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                            paste0(input$dataset_name, "_inspected")
                          else "inspected_data"),
        downloadButton("inspect_download", "Download Inspected Data"),
        div(style = "margin-top: 4px;",
          downloadButton("inspect_download_flagged", "Download Flagged Tokens")
        )
      )
    )
  })

  # Store inspection result as reactiveVal
  inspect_result <- reactiveVal(NULL)

  # Run inspection when button is clicked.
  # The detection logic lives in the package function inspect_f0()
  # (R/inspect.R), which is unit-tested in tests/testthat/test-inspect.R.
  observeEvent(input$inspect_button, {
    req(dataset())
    req(input$inspect_f0_var, input$inspect_token_var, input$inspect_time_var,
        input$inspect_speaker_var, input$inspect_tone_var)

    carry_mult <- as.numeric(input$inspect_carryover_mult)
    if (is.na(carry_mult) || carry_mult < 0) carry_mult <- 1.5

    level_thresh <- as.numeric(input$inspect_level_thresh)
    if (is.na(level_thresh) || level_thresh <= 0) level_thresh <- 3.5

    result <- inspect_f0(
      dataset(),
      f0              = input$inspect_f0_var,
      token           = input$inspect_token_var,
      time            = input$inspect_time_var,
      speaker         = input$inspect_speaker_var,
      tone            = input$inspect_tone_var,
      z_threshold     = input$inspect_z_thresh,
      rise_threshold  = input$inspect_rise_thresh,
      fall_threshold  = input$inspect_fall_thresh,
      carryover_mult  = carry_mult,
      level_threshold = level_thresh,
      time_unit       = input$inspect_time_unit
    )

    inspect_result(result)
  })

  # Summary panel
  output$inspect_summary <- renderUI({
    req(inspect_result())
    result <- inspect_result()
    token_var <- input$inspect_token_var
    tone_var <- input$inspect_tone_var
    speaker_var <- input$inspect_speaker_var

    # Overall counts
    token_level <- result %>%
      distinct(.data[[token_var]], .keep_all = TRUE)
    n_tokens <- nrow(token_level)
    n_flagged_tokens <- sum(token_level$flagged_token, na.rm = TRUE)
    n_samples <- nrow(result)
    n_flagged_samples <- sum(result$flagged_jump, na.rm = TRUE)

    # Speaker x tone groups too small to run the level check (fixed
    # minimum of 4 tokens; matches inspect_f0()'s default min_tokens).
    LEVEL_MIN_TOKENS <- 4
    small_grps <- token_level %>%
      group_by(.data[[speaker_var]], .data[[tone_var]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n < LEVEL_MIN_TOKENS)
    n_level_skipped <- sum(small_grps$n)
    n_small_groups  <- nrow(small_grps)

    # Per-tone breakdown
    tone_summary <- token_level %>%
      group_by(.data[[tone_var]]) %>%
      summarise(
        total = n(),
        flagged = sum(flagged_token, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(flagged > 0)

    tone_items <- lapply(seq_len(nrow(tone_summary)), function(i) {
      row <- tone_summary[i, ]
      tags$li(paste0(row[[tone_var]], ": ", row$flagged, " / ", row$total,
                     " (", round(100 * row$flagged / max(row$total, 1), 1), "%)"))
    })

    tagList(
      tags$div(style = "background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem;",
        tags$strong("Inspection summary:"),
        tags$ul(style = "margin-bottom: 4px; padding-left: 18px;",
          tags$li(paste0("Tokens flagged: ", n_flagged_tokens, " / ", n_tokens,
                         " (", round(100 * n_flagged_tokens / max(n_tokens, 1), 1), "%)")),
          tags$li(paste0("Tokens with potential jumps: ", n_flagged_samples, " / ", n_samples,
                         " (", round(100 * n_flagged_samples / max(n_samples, 1), 1), "%)"))
        ),
        if (n_level_skipped > 0) {
          tags$p(style = "margin: 4px 0 6px 0; font-size: 0.82rem; color: #8a6d00;",
            tags$em(paste0(
              "Level check skipped for ", n_level_skipped, " token",
              if (n_level_skipped == 1) "" else "s", " in ", n_small_groups,
              " speaker × tone group", if (n_small_groups == 1) "" else "s",
              " with fewer than ", LEVEL_MIN_TOKENS,
              " tokens (not enough to estimate a spread)."))
          )
        },
        if (nrow(tone_summary) > 0) {
          tagList(
            tags$strong("Flagged tokens by tone:"),
            tags$ul(style = "margin-bottom: 0; padding-left: 18px;", tone_items)
          )
        }
      )
    )
  })

  # DT table output
  output$inspect_data <- DT::renderDataTable({
    req(inspect_result())
    result <- inspect_result()

    DT::datatable(
      result,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      )
    ) %>%
      DT::formatStyle("flagged_token",
        backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#ffe0e0", "white"))) %>%
      DT::formatStyle("flagged_jump",
        backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#fff3cd", "white"))) %>%
      DT::formatRound(c("f0_token_max", "f0_token_min", "f0_token_mean", "f0_token_sd"), 2)
  })

  # Download handler - all data
  output$inspect_download <- downloadHandler(
    filename = function() {
      paste0(input$inspect_filename, ".csv")
    },
    content = function(file) {
      req(inspect_result())
      fname <- paste0(input$inspect_filename, ".csv")
      write.csv(inspect_result(), file, row.names = FALSE)
      showNotification(paste("Data saved as", fname), type = "message", duration = 4)
    }
  )

  # Download handler - flagged tokens only
  output$inspect_download_flagged <- downloadHandler(
    filename = function() {
      paste0(input$inspect_filename, "_flagged.csv")
    },
    content = function(file) {
      req(inspect_result())
      fname <- paste0(input$inspect_filename, "_flagged.csv")
      flagged <- inspect_result() %>% filter(flagged_token == TRUE)
      write.csv(flagged, file, row.names = FALSE)
      showNotification(paste("Flagged tokens saved as", fname), type = "message", duration = 4)
    }
  )
}
