###############################################
# F0 Processing → F0 Extraction subtab
# Backends:
#   (1) Extract from .wav via wrassp::ksvF0()
#   (2) Parse uploaded Praat .Pitch / .PitchTier via rPraat
# Both populate fp_f0_data with a long-format data frame:
#   token (basename), time (s), f0 (Hz, NA on unvoiced frames).
###############################################

fp_extraction_ui <- function(input, output, session, fp_audio_data, fp_f0_data) {

  # ---- Sidebar controls ----
  output$ui_fp_extraction <- renderUI({
    tagList(
      radioButtons("fp_extract_mode", "f0 source:",
                   choices = c("Extract from .wav (wrassp)" = "wrassp",
                               "Use uploaded .Pitch / .PitchTier (Praat)" = "praat"),
                   selected = "wrassp"),
      tags$hr(),
      conditionalPanel("input.fp_extract_mode == 'wrassp'",
        numericInput("fp_f0_min",   "Min f0 (Hz)",   value = 75,  min = 30,  max = 300),
        numericInput("fp_f0_max",   "Max f0 (Hz)",   value = 600, min = 200, max = 1000),
        numericInput("fp_window_ms","Frame step (ms)", value = 5,  min = 1,   max = 50)
      ),
      tags$hr(),
      actionButton("fp_extract_run", "Run extraction", icon = icon("play")),
      tags$hr(),
      conditionalPanel("output.fp_have_f0 === 'yes'",
        h5("Download:"),
        textInput("fp_extract_filename", "Filename:", value = "extracted_f0"),
        downloadButton("fp_extract_download", "Download f0 (CSV)")
      )
    )
  })

  # Output flag exposed to JS for conditionalPanel on the Download block
  output$fp_have_f0 <- reactive({
    df <- fp_f0_data()
    if (!is.null(df) && nrow(df) > 0) "yes" else "no"
  })
  outputOptions(output, "fp_have_f0", suspendWhenHidden = FALSE)

  # ---- Main panel: introductory guide ----
  output$fp_extraction_guide <- renderUI({
    box_style <- "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;"
    tagList(
      tags$div(
        style = box_style,
        tags$strong("F0 Extraction"),
        tags$p(style = "margin: 6px 0 0 0;",
          "Choose how to obtain f0 contours from your audio files, then click ",
          tags$strong("Run extraction"), "."),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("wrassp"),
            " runs the ksvF0 algorithm in R — no external dependencies, ",
            "deploys cleanly on shinyapps.io."),
          tags$li(tags$strong("Praat"),
            " uses the .Pitch / .PitchTier files you uploaded alongside the .wav files. ",
            "Choose this if you've already extracted pitch in Praat with custom settings.")
        )
      )
    )
  })

  # ---- Helpers: extract one token from .wav (wrassp) ----
  extract_wrassp_one <- function(wav_path, basename, f0_min, f0_max, step_ms) {
    obj <- tryCatch(
      wrassp::ksvF0(wav_path, toFile = FALSE,
                    minF = f0_min, maxF = f0_max,
                    windowShift = step_ms, verbose = FALSE),
      error = function(e) {
        warning("ksvF0 failed for ", basename, ": ", e$message)
        NULL
      }
    )
    if (is.null(obj)) return(NULL)
    sr        <- attr(obj, "sampleRate")
    t0        <- attr(obj, "startTime")
    n_frames  <- nrow(obj$F0)
    t         <- seq(t0, by = 1 / sr, length.out = n_frames)
    f0        <- as.vector(obj$F0)
    f0[f0 == 0] <- NA_real_   # wrassp uses 0 for unvoiced
    data.frame(token = basename, time = t, f0 = f0, stringsAsFactors = FALSE)
  }

  # ---- Helpers: parse one .Pitch or .PitchTier file (rPraat) ----
  extract_praat_one <- function(pitch_path, pitchtier_path, basename) {
    # Prefer .Pitch (full output) over .PitchTier (sparse, possibly cleaned).
    if (!is.na(pitch_path)) {
      parsed <- tryCatch(rPraat::pitch.read(pitch_path), error = function(e) NULL)
      if (!is.null(parsed)) {
        n <- parsed$nx
        if (is.null(n) || n == 0) return(NULL)
        t <- if (!is.null(parsed$t)) parsed$t
             else parsed$x1 + (0:(n - 1)) * parsed$dx
        f0 <- vapply(parsed$frame, function(fr) {
          if (is.null(fr$nCandidates) || fr$nCandidates == 0) return(NA_real_)
          fr$frequency[1]    # selected (best) candidate
        }, numeric(1))
        f0[f0 == 0] <- NA_real_   # Praat uses 0 for unvoiced
        return(data.frame(token = basename, time = t, f0 = f0,
                          stringsAsFactors = FALSE))
      }
    }
    if (!is.na(pitchtier_path)) {
      parsed <- tryCatch(rPraat::pt.read(pitchtier_path), error = function(e) NULL)
      if (!is.null(parsed)) {
        return(data.frame(token = basename, time = parsed$t, f0 = parsed$f,
                          stringsAsFactors = FALSE))
      }
    }
    NULL
  }

  # ---- Run extraction ----
  observeEvent(input$fp_extract_run, {
    audio <- fp_audio_data()
    if (is.null(audio) || nrow(audio) == 0) {
      showNotification("Upload audio files in the Start tab first.",
                       type = "warning", duration = 4)
      return()
    }

    mode <- input$fp_extract_mode

    if (mode == "wrassp") {
      wavs <- audio[!is.na(audio$wav_path), , drop = FALSE]
      if (nrow(wavs) == 0) {
        showNotification("No .wav files in the upload.",
                         type = "warning", duration = 4)
        return()
      }
      f0_min  <- as.numeric(input$fp_f0_min)
      f0_max  <- as.numeric(input$fp_f0_max)
      step_ms <- as.numeric(input$fp_window_ms)

      withProgress(message = "Extracting f0 (wrassp)", value = 0, {
        results <- list()
        for (i in seq_len(nrow(wavs))) {
          b <- wavs$basename[i]
          incProgress(1 / nrow(wavs), detail = b)
          one <- extract_wrassp_one(wavs$wav_path[i], b, f0_min, f0_max, step_ms)
          if (!is.null(one)) results[[b]] <- one
        }
        if (length(results) == 0) {
          showNotification("Extraction failed for all files.",
                           type = "error", duration = 6)
          return()
        }
        fp_f0_data(do.call(rbind, results))
        showNotification(sprintf("Extracted f0 for %d / %d tokens.",
                                 length(results), nrow(wavs)),
                         type = "message", duration = 4)
      })
    } else {
      # Praat mode
      has_praat <- audio[!is.na(audio$pitch_path) | !is.na(audio$pitchtier_path), , drop = FALSE]
      if (nrow(has_praat) == 0) {
        showNotification("No .Pitch / .PitchTier files in the upload.",
                         type = "warning", duration = 4)
        return()
      }

      withProgress(message = "Parsing Praat pitch files", value = 0, {
        results <- list()
        for (i in seq_len(nrow(has_praat))) {
          b <- has_praat$basename[i]
          incProgress(1 / nrow(has_praat), detail = b)
          one <- extract_praat_one(has_praat$pitch_path[i],
                                   has_praat$pitchtier_path[i], b)
          if (!is.null(one)) results[[b]] <- one
        }
        if (length(results) == 0) {
          showNotification("Parsing failed for all files.",
                           type = "error", duration = 6)
          return()
        }
        fp_f0_data(do.call(rbind, results))
        showNotification(sprintf("Parsed f0 for %d / %d tokens.",
                                 length(results), nrow(has_praat)),
                         type = "message", duration = 4)
      })
    }
  })

  # ---- Results area ----
  output$fp_extraction_results <- renderUI({
    if (is.null(fp_audio_data()) || nrow(fp_audio_data()) == 0) {
      return(tags$div(style = "color: #888; font-style: italic; margin: 8px 0;",
                      "Upload audio files in the Start tab first."))
    }
    if (is.null(fp_f0_data()) || nrow(fp_f0_data()) == 0) {
      return(tags$div(style = "color: #888; font-style: italic; margin: 8px 0;",
                      "Set parameters in the sidebar and click ",
                      tags$strong("Run extraction"), "."))
    }
    tagList(
      tags$h4("Extracted f0 contours"),
      tags$p(style = "color: #777; font-size: 0.85rem;",
        "Click a token name in the legend to hide/show its contour. ",
        "Use the camera icon for a high-resolution PNG."),
      plotly::plotlyOutput("fp_f0_overview", height = "450px"),
      tags$h4(style = "margin-top: 20px;", "Per-token summary"),
      DT::dataTableOutput("fp_f0_summary_table")
    )
  })

  # ---- Overlay plot of all token contours ----
  output$fp_f0_overview <- plotly::renderPlotly({
    df <- fp_f0_data()
    req(df, nrow(df) > 0)
    plotly::plot_ly(
      df, x = ~time, y = ~f0,
      color = ~token, type = "scatter", mode = "lines+markers",
      marker = list(size = 4), line = list(width = 1.2),
      hovertemplate = paste0(
        "<b>%{fullData.name}</b><br>time: %{x:.3f}s<br>f0: %{y:.1f} Hz<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(title = "Time (s)"),
        yaxis = list(title = "f0 (Hz)"),
        legend = list(title = list(text = "token"))
      ) |>
      plotly::config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          filename = "extracted_f0",
          width = 1600, height = 1000, scale = 2
        )
      )
  })

  # ---- Per-token summary table ----
  output$fp_f0_summary_table <- DT::renderDataTable({
    df <- fp_f0_data()
    req(df, nrow(df) > 0)
    summary_df <- df |>
      dplyr::group_by(token) |>
      dplyr::summarise(
        n_frames   = dplyr::n(),
        n_voiced   = sum(!is.na(f0)),
        n_unvoiced = sum(is.na(f0)),
        mean_f0    = round(mean(f0, na.rm = TRUE), 2),
        min_f0     = round(min(f0, na.rm = TRUE), 2),
        max_f0     = round(max(f0, na.rm = TRUE), 2),
        duration_s = round(max(time) - min(time), 3),
        .groups    = "drop"
      )
    DT::datatable(
      summary_df,
      rownames = FALSE, filter = "top",
      options = list(
        pageLength = 25, autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    )
  })

  # ---- Download f0 CSV ----
  output$fp_extract_download <- downloadHandler(
    filename = function() {
      paste0(input$fp_extract_filename, ".csv")
    },
    content = function(file) {
      req(fp_f0_data())
      fname <- paste0(input$fp_extract_filename, ".csv")
      write.csv(fp_f0_data(), file, row.names = FALSE)
      showNotification(paste("f0 data saved as", fname),
                       type = "message", duration = 4)
    }
  )
}
