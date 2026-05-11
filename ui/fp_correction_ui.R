###############################################
# f0 processing → f0 correction subtab
# Interactive cleanup of extracted f0 contours (octave jumps, halving, outliers).
# Scaffolding only — full correction UI comes in a later iteration.
###############################################

fp_correction_ui <- function(input, output, session, fp_audio_data, fp_f0_data) {

  # --- Sidebar placeholder ---
  output$ui_fp_correction <- renderUI({
    tagList(
      tags$div(style = "color: #888; font-style: italic;",
        "Correction tools will appear here once you've extracted f0 contours.")
    )
  })

  # --- Main panel: guide ---
  output$fp_correction_guide <- renderUI({
    box_style <- "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;"
    tagList(
      tags$div(
        style = box_style,
        tags$strong("F0 Correction"),
        tags$p(style = "margin: 6px 0 0 0;",
          "Review the extracted contours per token and fix common pitch-tracking errors:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li("Octave jumps (e.g. doubling/halving on creaky voice)"),
          tags$li("Spurious frames outside the speaker's typical range"),
          tags$li("Misaligned voiced/unvoiced boundaries")
        )
      ),
      tags$div(style = "color: #888; font-style: italic; margin: 8px 0;",
        "(Correction UI will be wired up in a follow-up.)")
    )
  })
}
