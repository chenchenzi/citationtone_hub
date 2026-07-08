###############################################
# Inspect tab: F0 outlier & artefact detection
###############################################

inspect_ui <- function(input, output, session, dataset, inspect_result = reactiveVal(NULL)) {

  # Guide text for the Inspect tab
  output$inspect_guide <- renderUI({
    guide_box("Inspect guide",
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("f0 (Hz):"), " The raw fundamental frequency column in Hertz."),
          tags$li(tags$strong("Token ID:"), " A unique identifier for each token/syllable (groups rows belonging to the same contour)."),
          tags$li(tags$strong("Time:"), " The time variable that orders f0 samples within each token."),
          tags$li(tags$strong("Speaker:"), " A speaker ID for by-speaker z-score computation."),
          tags$li(tags$strong("Tone category (optional):"), " The column labelling tone types. Enables the token-level (by-tone) check; leave as ", tags$em("— none —"), " to skip it, e.g. before tone categories are known (tone discovery)."),
          tags$li(tags$strong("Intensity (dB, optional):"), " A per-frame intensity column (e.g. from the Praat script or in-app wrassp extraction). Enables the low-intensity check; leave as ", tags$em("— none —"), " if your data has no intensity.")
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
            HTML("<strong>same speaker and tone</strong>."),
            " Flags a contour smoothly shifted too high ",
            "or low for its tone but still appearing normal for the speaker."
          ),
          tags$li(
            tags$em("Sample-level"), " (within token): rate of change, octave bounds, and carryover, ",
            HTML("<strong>frame by frame</strong>."),
            " Flags individual mis-tracked samples in an otherwise normal token."
          )
        ),
        tags$p(style = "margin: 0 0 8px 0;",
          tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "flagged_token = TRUE"),
          " whenever any check fires. The token-level check needs at least 5 same-speaker-same-tone tokens, and is more reliable with more (repetitions, different items, etc.)."
        ),
        tags$strong("Detection methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Extreme-value (speaker-level):"), " Computes per-token summary statistics (max, min, mean, SD), then z-scores the per-token ", tags$em("max"), " and ", tags$em("min"), " within each speaker. Tokens whose max or min exceeds the threshold are flagged as ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "max too high"), " or ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "min too low"), ". The mean and SD are reported alongside (in the table and tooltips) for context, but the flag decision uses only the max / min z-scores."),
          tags$li(tags$strong("Token-level (speaker × tone):"), " Computes each token's median f0 in semitones, then a ", tags$em("modified z-score"), " (median/MAD-based; Iglewicz & Hoaglin, 1993) against other tokens of the same speaker and tone. Tokens beyond the threshold are flagged ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "level too high"), " or ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "level too low"), ". This catches smoothly shifted contours that the other checks miss. The median (not mean) is used so a few jump-flagged frames don't shift a token's apparent level; groups with fewer than the minimum number of tokens are skipped."),
          tags$li(tags$strong("Jump detection:"), " Computes sample-to-sample semitone differences within each token. Flags where the change exceeds a rise or fall threshold, labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "jump (rise)"), " or ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "jump (fall)"), ". When a jump is detected, the side ", tags$em("farther from the token median"), " is flagged, so artefacts at the start of a token are caught correctly. Adapted from Steffman & Cole (2022)."),
          tags$li(tags$strong("Octave jumps:"), " Flags samples where the Hz ratio to the previous sample is < 0.49 or > 1.99 (pitch halving/doubling), labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "octave jump"), "."),
          tags$li(tags$strong("Carryover:"), " Samples ", tags$em("around"), " a detected artefact (forward and backward) that stay within ", tags$em("mult"), "\u00d7 the rise/fall threshold (in semitones) of the artefact\u2019s f0. These may still be erroneous even if their own step is small, labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "carryover"), ". The band is direction-specific: ", tags$code("rise_threshold \u00d7 mult"), " when the trend is rising, ", tags$code("fall_threshold \u00d7 mult"), " when falling (adapted from Steffman & Cole, 2022). The multiplier defaults to ", tags$strong("1.5"), " (paper\u2019s value) and is configurable in the sidebar (set to 0 to disable)."),
          tags$li(HTML(paste0(
            "<strong>Low intensity (optional):</strong> When an intensity column is selected, ",
            "flags voiced samples sitting more than the dB threshold below their token&rsquo;s ",
            "<em>peak</em> intensity, typically at voicing onsets/offsets and devoiced or creaky ",
            "tails, where f0 is least reliable. Labelled ",
            "<code style='color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;'>",
            "low intensity</code>. This check is <strong>advisory</strong>: it is noted in ",
            "<code>flag_notes</code> and counted in the summary, but does not on its own set ",
            "<code>flagged_token</code> (a quiet but modal frame can track fine). It is most ",
            "valuable where it co-occurs with a jump, corroborating that the jump is a tracking error.")))
        ),
        tags$strong("Default thresholds:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Extreme-value (z-score) = 3:"), " A standard convention for outlier detection (\u00b13 SD covers 99.7% of a normal distribution)."),
          tags$li(tags$strong("Rise = 1.263 ST, Fall = 1.714 ST per 10ms:"), " Based on the maximum rate of f0 change in human speech production (Sundberg, 1973). Changes exceeding these rates are physiologically implausible and likely tracking errors."),
          tags$li(tags$strong("Token-level (modified z) = 3.5:"), " The cutoff recommended by Iglewicz & Hoaglin (1993) for the median/MAD modified z-score, the robust analogue of the \u00b13 SD rule. Requires at least 5 same-speaker-same-tone tokens (more is more reliable)."),
          tags$li(tags$strong("Low-intensity drop = 15 dB:"), " Voiced frames more than 15 dB below the token\u2019s peak intensity are flagged. The comparison is relative-to-peak (not an absolute dB level), so it transfers across recordings regardless of microphone distance or gain.")
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
        selectInput("inspect_tone_var", "Select Tone category variable (optional):",
                    choices = c("— none —" = "", setNames(vars, var_types)),
                    selected = guess_var(vars, var_patterns$tone, 5)),
        selectInput("inspect_intensity_var",
                    "Select Intensity (dB) variable (optional):",
                    choices  = c("— none —" = "",
                                 setNames(vars, var_types)),
                    selected = {
                      hit <- vars[grepl("intens|rms|energy|\\bdb\\b",
                                        vars, ignore.case = TRUE)]
                      if (length(hit)) hit[1] else ""
                    }),
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
        numericInput("inspect_intensity_drop",
                     "Low-intensity drop below peak (dB):",
                     value = 15, min = 1, max = 50, step = 1),
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

  # Inspection result is stored in the shared `inspect_result` reactiveVal
  # (passed in), so the Curate tab can offer "use Inspect flags" as a way to
  # find re-label candidates. Defaults to a private reactiveVal if not shared.

  # Run inspection when button is clicked.
  # The detection logic lives in the package function inspect_f0()
  # (R/inspect.R), which is unit-tested in tests/testthat/test-inspect.R.
  observeEvent(input$inspect_button, {
    req(dataset())
    req(input$inspect_f0_var, input$inspect_token_var, input$inspect_time_var,
        input$inspect_speaker_var)   # tone is optional (coalesced below)

    carry_mult <- as.numeric(input$inspect_carryover_mult)
    if (is.na(carry_mult) || carry_mult < 0) carry_mult <- 1.5

    level_thresh <- as.numeric(input$inspect_level_thresh)
    if (is.na(level_thresh) || level_thresh <= 0) level_thresh <- 3.5

    intensity_drop <- as.numeric(input$inspect_intensity_drop)
    if (is.na(intensity_drop) || intensity_drop <= 0) intensity_drop <- 15

    # Optional intensity check. Only run it when the user has picked a column
    # that exists and is numeric; otherwise fall back to NULL (skip).
    intensity_var <- input$inspect_intensity_var
    if (is.null(intensity_var) || !nzchar(intensity_var) ||
        !(intensity_var %in% names(dataset()))) {
      intensity_var <- NULL
    } else if (!is.numeric(dataset()[[intensity_var]])) {
      showNotification(
        "Intensity column is not numeric; skipping the low-intensity check.",
        type = "warning", duration = 5)
      intensity_var <- NULL
    }

    # Tone is optional: an empty selection (— none —) skips the token-level
    # (speaker x tone) check via inspect_f0(tone = NULL).
    tone_var <- input$inspect_tone_var
    if (is.null(tone_var) || !nzchar(tone_var)) tone_var <- NULL

    result <- inspect_f0(
      dataset(),
      f0              = input$inspect_f0_var,
      token           = input$inspect_token_var,
      time            = input$inspect_time_var,
      speaker         = input$inspect_speaker_var,
      tone            = tone_var,
      z_threshold     = input$inspect_z_thresh,
      rise_threshold  = input$inspect_rise_thresh,
      fall_threshold  = input$inspect_fall_thresh,
      carryover_mult  = carry_mult,
      level_threshold = level_thresh,
      intensity       = intensity_var,
      intensity_drop  = intensity_drop,
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
    if (is.null(tone_var) || !nzchar(tone_var)) tone_var <- NULL
    has_tone <- !is.null(tone_var)
    speaker_var <- input$inspect_speaker_var

    # Overall counts
    token_level <- result %>%
      distinct(.data[[token_var]], .keep_all = TRUE)
    n_tokens <- nrow(token_level)
    n_flagged_tokens <- sum(token_level$flagged_token, na.rm = TRUE)
    n_samples <- nrow(result)
    n_flagged_samples <- sum(result$flagged_jump, na.rm = TRUE)

    # Low-intensity samples (only present when an intensity column was used).
    has_intensity    <- "flag_low_intensity" %in% names(result)
    n_lowint_samples <- if (has_intensity)
                          sum(result$flag_low_intensity, na.rm = TRUE) else 0

    # Which check(s) fired, per token (derived from flag_notes / flagged_jump;
    # the checks can overlap, so these need not sum to the flagged total).
    per_token_checks <- result %>%
      group_by(.data[[token_var]]) %>%
      summarise(
        extreme = any(grepl("max too high|min too low", flag_notes), na.rm = TRUE),
        level   = any(grepl("level too", flag_notes), na.rm = TRUE),
        jump    = any(flagged_jump, na.rm = TRUE),
        .groups = "drop"
      )
    n_extreme_tokens <- sum(per_token_checks$extreme)
    n_level_tokens   <- sum(per_token_checks$level)
    n_jump_tokens    <- sum(per_token_checks$jump)

    # Speaker x tone groups too small to run the level check (fixed
    # minimum of 5 tokens; matches inspect_f0()'s default min_tokens).
    # Only meaningful when a tone column was supplied.
    LEVEL_MIN_TOKENS <- 5
    if (has_tone) {
      small_grps <- token_level %>%
        group_by(.data[[speaker_var]], .data[[tone_var]]) %>%
        summarise(n = n(), .groups = "drop") %>%
        filter(n < LEVEL_MIN_TOKENS)
      n_level_skipped <- sum(small_grps$n)
      n_small_groups  <- nrow(small_grps)
    } else {
      n_level_skipped <- 0
      n_small_groups  <- 0
    }

    # Per-tone breakdown (only when a tone column was supplied)
    if (has_tone) {
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
    } else {
      tone_summary <- data.frame()
      tone_items   <- list()
    }

    tagList(
      tags$div(style = "background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem;",
        tags$strong("Inspection summary:"),
        tags$ul(style = "margin-bottom: 4px; padding-left: 18px;",
          tags$li(paste0("Tokens flagged: ", n_flagged_tokens, " / ", n_tokens,
                         " (", round(100 * n_flagged_tokens / max(n_tokens, 1), 1), "%)"),
            tags$ul(style = "margin: 2px 0; padding-left: 18px;",
              tags$li(paste0("Extreme-value (max / min by speaker): ", n_extreme_tokens)),
              if (has_tone)
                tags$li(paste0("Token-level (median by speaker × tone): ", n_level_tokens))
              else
                tags$li(tags$em("Token-level (by tone): not run (no tone selected)")),
              tags$li(paste0("Sample-level (jumps): ", n_jump_tokens)),
              tags$li(style = "list-style: none; margin-left: -18px; color: #8a6d00;",
                tags$em("A token can fire more than one check, so these may sum to more than the flagged total."))
            )
          ),
          tags$li(paste0("Samples with potential jumps: ", n_flagged_samples, " / ", n_samples,
                         " (", round(100 * n_flagged_samples / max(n_samples, 1), 1), "%)")),
          if (has_intensity) tags$li(paste0(
            "Low-intensity samples (advisory): ", n_lowint_samples, " / ", n_samples,
            " (", round(100 * n_lowint_samples / max(n_samples, 1), 1), "%)"))
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

    dt <- DT::datatable(
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

    # Style the low-intensity flag and round the intensity column, both only
    # present when an intensity variable was selected.
    if ("flag_low_intensity" %in% names(result)) {
      dt <- dt %>%
        DT::formatStyle("flag_low_intensity",
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#e7f0ff", "white")))
    }
    intensity_var <- input$inspect_intensity_var
    if (!is.null(intensity_var) && nzchar(intensity_var) &&
        intensity_var %in% names(result) && is.numeric(result[[intensity_var]])) {
      dt <- dt %>% DT::formatRound(intensity_var, 1)
    }
    dt
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
