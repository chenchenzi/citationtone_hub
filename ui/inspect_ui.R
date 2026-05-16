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
        tags$strong("Detection methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Token-level:"), " Computes per-token max and min f0, then z-scores these by speaker. Tokens exceeding the threshold are flagged as ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "max too high"), " or ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "min too low"), "."),
          tags$li(tags$strong("Jump detection:"), " Computes sample-to-sample semitone differences within each token. Flags where the change exceeds a rise or fall threshold, labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "jump (rise)"), " or ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "jump (fall)"), " (Steffman & Cole, 2022)."),
          tags$li(tags$strong("Octave jumps:"), " Flags samples where the Hz ratio to the previous sample is < 0.49 or > 1.99 (pitch halving/doubling), labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "octave jump"), "."),
          tags$li(tags$strong("Carryover:"), " Samples following a detected error that remain within 1.5\u00d7 the threshold of the error\u2019s f0. These may still be erroneous even if their own step is small, labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "carryover"), ".")
        ),
        tags$strong("Default thresholds:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("z-score = 3:"), " A standard convention for outlier detection (\u00b13 SD covers 99.7% of a normal distribution)."),
          tags$li(tags$strong("Rise = 1.263 ST, Fall = 1.714 ST per 10ms:"), " Based on the maximum rate of f0 change in human speech production (Sundberg, 1973). Changes exceeding these rates are physiologically implausible and likely tracking errors.")
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
                    choices = setNames(vars, var_types), selected = vars[1]),
        selectInput("inspect_token_var", "Select Token ID variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 1, vars[2], vars[1])),
        selectInput("inspect_time_var", "Select Time variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 2, vars[3], vars[1])),
        selectInput("inspect_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 3, vars[4], vars[1])),
        selectInput("inspect_tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = ifelse(length(vars) > 4, vars[5], vars[1])),
        tags$hr(),
        radioButtons("inspect_time_unit", "Time unit:",
                     choices = list("Milliseconds" = "ms",
                                    "Seconds" = "s",
                                    "Normalised (per step)" = "norm"),
                     selected = "ms", inline = TRUE),
        tags$hr(),
        h5("Thresholds"),
        numericInput("inspect_z_thresh", "Token z-score threshold (SD):",
                     value = 3, min = 1, max = 6, step = 0.5),
        numericInput("inspect_rise_thresh", "Rise threshold (ST per 10ms):",
                     value = 1.263, min = 0.1, max = 5, step = 0.1),
        numericInput("inspect_fall_thresh", "Fall threshold (ST per 10ms):",
                     value = 1.714, min = 0.1, max = 5, step = 0.1),
        tags$hr(),
        actionButton("inspect_button", "Run Inspection"),
        tags$hr(),
        h5("Download"),
        textInput("inspect_filename", "Enter filename (without extension):",
                  value = "inspected_data"),
        downloadButton("inspect_download", "Download Inspected Data"),
        div(style = "margin-top: 4px;",
          downloadButton("inspect_download_flagged", "Download Flagged Tokens")
        )
      )
    )
  })

  # Store inspection result as reactiveVal
  inspect_result <- reactiveVal(NULL)

  # Run inspection when button is clicked
  observeEvent(input$inspect_button, {
    req(dataset())
    req(input$inspect_f0_var, input$inspect_token_var, input$inspect_time_var,
        input$inspect_speaker_var, input$inspect_tone_var)

    data <- dataset()
    f0_var <- input$inspect_f0_var
    token_var <- input$inspect_token_var
    time_var <- input$inspect_time_var
    speaker_var <- input$inspect_speaker_var
    tone_var <- input$inspect_tone_var
    z_thresh <- input$inspect_z_thresh
    rise_thresh <- input$inspect_rise_thresh
    fall_thresh <- input$inspect_fall_thresh
    time_unit <- input$inspect_time_unit

    # Treat f0 == 0 as NA (pitch trackers output 0 for unvoiced)
    data <- data %>%
      mutate(!!f0_var := ifelse(.data[[f0_var]] == 0, NA_real_, .data[[f0_var]]))

    # --- Step 1: Token-level outlier detection ---
    token_stats <- data %>%
      filter(!is.na(.data[[f0_var]])) %>%
      group_by(.data[[token_var]]) %>%
      summarise(
        f0_token_max = max(.data[[f0_var]], na.rm = TRUE),
        f0_token_min = min(.data[[f0_var]], na.rm = TRUE),
        f0_token_mean = mean(.data[[f0_var]], na.rm = TRUE),
        f0_token_sd = sd(.data[[f0_var]], na.rm = TRUE),
        .groups = "drop"
      )

    # Get speaker for each token (first occurrence)
    token_speaker <- data %>%
      distinct(.data[[token_var]], .keep_all = TRUE) %>%
      select(all_of(c(token_var, speaker_var)))

    token_stats <- token_stats %>%
      left_join(token_speaker, by = token_var) %>%
      group_by(.data[[speaker_var]]) %>%
      mutate(
        z_max = (f0_token_max - mean(f0_token_max, na.rm = TRUE)) / sd(f0_token_max, na.rm = TRUE),
        z_min = (f0_token_min - mean(f0_token_min, na.rm = TRUE)) / sd(f0_token_min, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        flag_too_high = !is.na(z_max) & abs(z_max) > z_thresh,
        flag_too_low = !is.na(z_min) & abs(z_min) > z_thresh
      )

    # --- Step 2: Sample-to-sample jump detection (Steffman & Cole 2022) ---
    result <- data %>%
      arrange(.data[[token_var]], .data[[time_var]]) %>%
      group_by(.data[[token_var]]) %>%
      mutate(
        f0_st = 12 * log2(.data[[f0_var]]),
        lead_f0_st = lead(f0_st, order_by = .data[[time_var]]),
        lead_f0_hz = lead(.data[[f0_var]], order_by = .data[[time_var]]),
        diff_st = lead_f0_st - f0_st,
        diff_time = lead(.data[[time_var]], order_by = .data[[time_var]]) - .data[[time_var]],
        hz_ratio = lead_f0_hz / .data[[f0_var]]
      ) %>%
      ungroup()

    # Time adjustment factor (normalise to 10ms reference)
    result <- result %>%
      mutate(
        time_factor = case_when(
          time_unit == "ms" ~ ifelse(is.na(diff_time) | diff_time == 0, 1, diff_time / 10),
          time_unit == "s" ~ ifelse(is.na(diff_time) | diff_time == 0, 1, (diff_time * 1000) / 10),
          time_unit == "norm" ~ 1  # treat each step as one 10ms-equivalent unit
        )
      )

    # Flag jumps: the error is on the NEXT sample (where the jump lands)
    # So we shift: if current sample has a big diff to the next, the NEXT sample is the error
    result <- result %>%
      group_by(.data[[token_var]]) %>%
      mutate(
        # Octave jump detection (on lead sample)
        oct_jump = !is.na(hz_ratio) & (hz_ratio < 0.49 | hz_ratio > 1.99),
        # Threshold jump detection (on lead sample)
        jump_rise = !is.na(diff_st) & diff_st > 0 & (abs(diff_st) * time_factor) > rise_thresh,
        jump_fall = !is.na(diff_st) & diff_st < 0 & (abs(diff_st) * time_factor) > fall_thresh,
        # Shift flags to the sample where the jump lands
        flagged_jump = lag(oct_jump | jump_rise | jump_fall, default = FALSE),
        jump_note_raw = case_when(
          lag(oct_jump, default = FALSE) & lag(jump_rise, default = FALSE) ~ "octave jump; jump (rise)",
          lag(oct_jump, default = FALSE) & lag(jump_fall, default = FALSE) ~ "octave jump; jump (fall)",
          lag(oct_jump, default = FALSE) ~ "octave jump",
          lag(jump_rise, default = FALSE) ~ "jump (rise)",
          lag(jump_fall, default = FALSE) ~ "jump (fall)",
          TRUE ~ ""
        )
      ) %>%
      ungroup()

    # Carryover detection: per-token forward loop
    detect_carryover <- function(flagged_jump, f0_st, rise_thresh, fall_thresh) {
      n <- length(flagged_jump)
      carryover <- rep(FALSE, n)
      carryover_note <- rep("", n)
      if (n < 2) return(list(carryover = carryover, carryover_note = carryover_note))

      threshold_band <- 1.5 * max(rise_thresh, fall_thresh)
      error_f0 <- NA_real_

      for (i in 2:n) {
        if (flagged_jump[i] && !is.na(f0_st[i])) {
          error_f0 <- f0_st[i]
        } else if (!is.na(error_f0) && !is.na(f0_st[i]) &&
                   (flagged_jump[i - 1] || carryover[i - 1])) {
          if (abs(f0_st[i] - error_f0) <= threshold_band) {
            carryover[i] <- TRUE
            carryover_note[i] <- "carryover"
          } else {
            error_f0 <- NA_real_  # chain broken
          }
        }
      }
      list(carryover = carryover, carryover_note = carryover_note)
    }

    result <- result %>%
      group_by(.data[[token_var]]) %>%
      mutate({
        co <- detect_carryover(flagged_jump, f0_st, rise_thresh, fall_thresh)
        data.frame(carryover = co$carryover, carryover_note = co$carryover_note)
      }) %>%
      ungroup()

    # Update flagged_jump to include carryover
    result <- result %>%
      mutate(
        flagged_jump = flagged_jump | carryover,
        jump_note_raw = ifelse(nchar(carryover_note) > 0 & nchar(jump_note_raw) > 0,
                               paste0(jump_note_raw, "; ", carryover_note),
                               ifelse(nchar(carryover_note) > 0, carryover_note, jump_note_raw))
      )

    # --- Merge token-level flags ---
    result <- result %>%
      left_join(
        token_stats %>% select(all_of(token_var), f0_token_max, f0_token_min,
                               f0_token_mean, f0_token_sd,
                               z_max, z_min, flag_too_high, flag_too_low),
        by = token_var
      )

    # Has any jump in this token?
    result <- result %>%
      group_by(.data[[token_var]]) %>%
      mutate(has_jump_in_token = any(flagged_jump, na.rm = TRUE)) %>%
      ungroup()

    # Build final flag columns
    result <- result %>%
      mutate(
        flag_notes = {
          notes <- rep("", n())
          notes <- ifelse(flag_too_high, "max too high", notes)
          notes <- ifelse(flag_too_low,
                          ifelse(nchar(notes) > 0, paste0(notes, "; min too low"), "min too low"),
                          notes)
          notes <- ifelse(nchar(jump_note_raw) > 0,
                          ifelse(nchar(notes) > 0, paste0(notes, "; ", jump_note_raw), jump_note_raw),
                          notes)
          notes
        },
        flagged_token = flag_too_high | flag_too_low | has_jump_in_token
      )

    # Select output columns
    result <- result %>%
      select(
        all_of(c(token_var, time_var, f0_var, speaker_var, tone_var)),
        f0_token_max, f0_token_min, f0_token_mean, f0_token_sd,
        flagged_token, flagged_jump, flag_notes
      )

    inspect_result(result)
  })

  # Summary panel
  output$inspect_summary <- renderUI({
    req(inspect_result())
    result <- inspect_result()
    token_var <- input$inspect_token_var
    tone_var <- input$inspect_tone_var

    # Overall counts
    token_level <- result %>%
      distinct(.data[[token_var]], .keep_all = TRUE)
    n_tokens <- nrow(token_level)
    n_flagged_tokens <- sum(token_level$flagged_token, na.rm = TRUE)
    n_samples <- nrow(result)
    n_flagged_samples <- sum(result$flagged_jump, na.rm = TRUE)

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
