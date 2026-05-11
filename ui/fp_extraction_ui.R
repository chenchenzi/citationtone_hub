###############################################
# f0 processing → f0 extraction subtab
# Two modes:
#   (1) Extract from .wav via wrassp::ksvF0()
#   (2) Parse uploaded Praat .Pitch / .PitchTier files via rPraat
# Scaffolding only — full extraction logic comes in a later iteration.
###############################################

fp_extraction_ui <- function(input, output, session, fp_audio_data, fp_f0_data) {

  # --- Sidebar: backend toggle + (later) extraction params ---
  output$ui_fp_extraction <- renderUI({
    tagList(
      radioButtons("fp_extract_mode", "f0 source:",
                   choices = c("Extract from .wav (wrassp)" = "wrassp",
                               "Use uploaded .Pitch files (Praat)" = "praat"),
                   selected = "wrassp"),
      tags$hr(),
      conditionalPanel("input.fp_extract_mode == 'wrassp'",
        numericInput("fp_f0_min", "Min f0 (Hz)", value = 75,  min = 30,  max = 300),
        numericInput("fp_f0_max", "Max f0 (Hz)", value = 600, min = 200, max = 1000),
        numericInput("fp_window_ms", "Frame step (ms)", value = 10, min = 1, max = 50)
      ),
      tags$hr(),
      actionButton("fp_extract_run", "Run extraction", icon = icon("play"))
    )
  })

  # --- Main panel: guide ---
  output$fp_extraction_guide <- renderUI({
    box_style <- "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;"
    tagList(
      tags$div(
        style = box_style,
        tags$strong("F0 Extraction"),
        tags$p(style = "margin: 6px 0 0 0;",
          "Choose how to obtain f0 contours from your audio files."),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("wrassp"),
            " runs the ksvF0 algorithm in R — no external dependencies, works on the deployed app."),
          tags$li(tags$strong("Praat"),
            " uses the .Pitch (or .PitchTier) files you uploaded alongside the .wav files. ",
            "Choose this if you've already extracted pitch in Praat with custom settings.")
        )
      ),
      tags$div(style = "color: #888; font-style: italic; margin: 8px 0;",
        "(Extraction logic will be wired up in a follow-up — UI scaffolding only for now.)")
    )
  })

  output$fp_extraction_results <- renderUI({
    if (is.null(fp_audio_data())) {
      return(tags$div(style = "color: #888; font-style: italic; margin: 8px 0;",
                      "Upload audio files in the Start tab first."))
    }
    NULL
  })

  # --- Placeholder run handler ---
  observeEvent(input$fp_extract_run, {
    if (is.null(fp_audio_data())) {
      showNotification("Upload audio files first.", type = "warning", duration = 4)
      return()
    }
    showNotification("Extraction runner is not yet implemented.",
                     type = "warning", duration = 4)
  })
}
