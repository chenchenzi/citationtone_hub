###############################################
# Data collection — Checklist tab
# Based on: Xu, C. & Zhang, C. (2024). JASA, 156(4), 2538-2565.
###############################################

checklist_ui <- function(input, output, session) {

  # Helper: build one checklist section
  checklist_section <- function(number, title, question, items) {
    section_id <- paste0("checklist_", number)
    tags$div(
      style = "margin-bottom: 20px;",
      tags$h5(
        style = "border-bottom: 1px solid #ddd; padding-bottom: 6px; margin-bottom: 8px; color: #3a3a3a;",
        paste0("(", number, ") ", title)
      ),
      tags$p(style = "font-style: italic; color: #666; margin-bottom: 10px;", question),
      tags$div(
        style = "padding-left: 8px;",
        lapply(seq_along(items), function(i) {
          item_id <- paste0(section_id, "_", i)
          tags$div(
            style = "margin-bottom: 6px; display: flex; align-items: flex-start; gap: 8px;",
            tags$input(type = "checkbox", id = item_id,
                       style = "margin-top: 4px; min-width: 16px; min-height: 16px; cursor: pointer;"),
            tags$label(`for` = item_id, style = "cursor: pointer; color: #444;", items[[i]])
          )
        })
      )
    )
  }

  output$checklist_content <- renderUI({
    tagList(
      # Citation block
      tags$div(
        style = "background-color: #f0faf7; border-left: 4px solid #78c2ad; padding: 12px 16px; margin-bottom: 20px; border-radius: 4px; font-size: 0.88rem; color: #555;",
        tags$strong("Checklist for citation tone research"),
        tags$p(style = "margin-top: 6px; margin-bottom: 6px;",
          "The following checklist is designed to guide researchers through the critical stages of conducting rigorous and reproducible research on the production of lexical tones. It will serve as a valuable roadmap for your production study, from the initial planning phase to the analysis and interpretation of results."
        ),
        tags$p(style = "margin-bottom: 0; font-size: 0.84rem;",
          tags$em("Source: "),
          "Xu, C. & Zhang, C. (2024). A cross-linguistic review of citation tone production studies: Methodology and recommendations. ",
          tags$em("Journal of the Acoustical Society of America"), ", 156(4), 2538\u20132565. ",
          tags$a(href = "https://doi.org/10.1121/10.0032356", target = "_blank", "https://doi.org/10.1121/10.0032356")
        )
      ),

      # Save as PDF button
      tags$div(
        style = "margin-bottom: 16px;",
        actionButton("checklist_print", "Save as PDF", icon = icon("file-pdf"),
                     onclick = "window.print();")
      ),

      # Print-friendly CSS for A4
      tags$style(HTML("
        @media print {
          /* Hide all app chrome */
          .navbar, .nav-tabs, .sidebar, .tab-pane > .nav,
          #checklist_print, .shiny-bound-output:not(#checklist_content),
          .tab-content > .tab-pane:not(.active) {
            display: none !important;
          }

          /* Page setup */
          @page {
            size: A4;
            margin: 20mm 18mm 20mm 18mm;
          }
          body {
            background: white !important;
            margin: 0 !important;
            padding: 0 !important;
            font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
          }

          /* Content styling */
          #checklist_content {
            font-size: 10.5pt;
            line-height: 1.5;
            color: #222 !important;
            max-width: 100% !important;
            padding: 0 !important;
            margin: 0 !important;
          }

          /* Citation block */
          #checklist_content > div:first-child {
            background-color: #f5f5f5 !important;
            -webkit-print-color-adjust: exact;
            print-color-adjust: exact;
            border-left: 3px solid #999 !important;
            padding: 10pt 14pt !important;
            margin-bottom: 14pt !important;
            font-size: 9.5pt;
          }

          /* Section headings */
          h5 {
            font-size: 12pt !important;
            font-weight: 600 !important;
            color: #000 !important;
            border-bottom: 0.5pt solid #ccc !important;
            padding-bottom: 4pt !important;
            margin-top: 14pt !important;
            margin-bottom: 6pt !important;
            page-break-after: avoid;
          }

          /* Guiding questions */
          h5 + p {
            font-size: 10pt !important;
            color: #444 !important;
            margin-bottom: 6pt !important;
            page-break-after: avoid;
          }

          /* Checkbox items */
          input[type='checkbox'] {
            -webkit-appearance: none;
            appearance: none;
            width: 11pt !important;
            height: 11pt !important;
            min-width: 11pt !important;
            min-height: 11pt !important;
            border: 1pt solid #666 !important;
            border-radius: 1.5pt;
            margin-top: 3pt !important;
            -webkit-print-color-adjust: exact;
            print-color-adjust: exact;
          }

          label {
            font-size: 10.5pt !important;
            color: #333 !important;
          }

          /* Avoid breaking sections across pages */
          div[style*='margin-bottom: 20px'] {
            page-break-inside: avoid;
          }

          /* Links */
          a { color: #333 !important; text-decoration: underline !important; }
          a[href]::after { content: none !important; }
        }
      ")),

      # Sections
      tags$div(
        style = "max-width: 800px;",

        checklist_section(1, "Language Variety",
          "Which language variety are you studying and why?",
          list(
            "Introduce the variety you are researching and its language family.",
            "Introduce the geographical location of the variety and the speech community.",
            "Report any previous findings in the literature about the variety.",
            "Report a summary of the tonal system of the variety in conclusion."
          )
        ),

        checklist_section(2, "Research Ethics",
          "Does the research comply with ethical guidelines?",
          list(
            "Apply for ethical approval from your research institute before data collection.",
            "Assess the risks involved in field trips and data collection.",
            "Consider how to recruit and compensate participants.",
            "Prepare recruitment materials, consent forms, and other supporting documents.",
            "Check out relevant laws and regulations concerning data protection and privacy.",
            "Store all data in secure environments, using encryption and controlled access."
          )
        ),

        checklist_section(3, "Speaker Selection",
          "Are the sample population representative of the target speech community?",
          list(
            "Consider the number of speakers.",
            "Consider the gender balance of the speakers.",
            "Consider the age range of the speakers.",
            "Administer a sociolinguistic questionnaire to learn about speakers\u2019 language use and language backgrounds."
          )
        ),

        checklist_section(4, "Speech Materials Design",
          "Are the speech materials designed in a well-controlled and motivated way?",
          list(
            "Consider the number of monosyllabic words in the speech materials.",
            "Consider the number of repetitions of each word.",
            "Consider the syllable structure of the selected monosyllabic words.",
            "Consider the segmental composition of the selected monosyllabic words.",
            "Consider the usage frequency of the selected monosyllabic words.",
            "Consider the presence of tonotactic gaps in the chosen materials and discuss the strategies for addressing them."
          )
        ),

        checklist_section(5, "Experiment Setup",
          "Have you considered how to present your speech materials?",
          list(
            "Decide on the presentation format of the speech materials.",
            "Consider the presentation sequence of the monosyllabic words.",
            "Add pauses between utterances.",
            "Use the same language variety as the medium of instructions.",
            "Standardise the instructions to ensure that each participant receives identical information."
          )
        ),

        checklist_section(6, "Recording",
          "Where do you conduct the recording sessions and what equipment do you use?",
          list(
            "Conduct the speech recordings in a phonetics lab or a quiet small room with soft furnishings.",
            "Ensure sufficient power supply for all equipment (e.g. spare batteries, power banks, or access to mains electricity).",
            "Use a head-mounted unidirectional microphone.",
            "Locate the microphone head to the side of or below the mouth.",
            "Consider the choice of the recorder and recording software.",
            "Take a picture of the recording setup.",
            "Configure the recording setting including the sampling rate, bit depth, and channel.",
            "Save the recordings in WAV format."
          )
        ),

        checklist_section(7, "Data Management",
          "Have you considered how to organise your corpus of monosyllables?",
          list(
            "Backup all data securely, preferably in multiple locations.",
            "Assign a unique ID to each participant to anonymise the data.",
            "Log the relevant metadata including the original filename of the recordings, recording date and time, and participant ID, after each recording session in a spreadsheet.",
            "Devise a file-naming system for the recordings and the relevant processing files such as the TextGrids.",
            "Keep track of the processing steps and do not overwrite the original data."
          )
        ),

        checklist_section(8, "Acoustic Measurement",
          "How do you extract f0 contours?",
          list(
            "Select an acoustic analysis interface and algorithm for f0 measurement.",
            "Check if any pre-processing is needed.",
            "Set appropriate parameters for the f0 estimation algorithm.",
            "Inspect the first-pass f0 measurements and identify potential outliers.",
            "Examine the f0 data points flagged as potential outliers and address them accordingly.",
            "Consider normalisation strategies for f0 and time."
          )
        ),

        checklist_section(9, "Pitch Contour Analysis",
          "How do you analyse f0 contours and present findings of citation tones?",
          list(
            "Validate the number of lexical tones (distinct contour shapes).",
            "Describe the tone contours qualitatively.",
            "Provide some descriptive statistics of f0 contours and/or their duration by tone, such as the mean, standard deviation, maximum, minimum, and f0 range.",
            "Document any voice quality variations.",
            "Select an f0 contour analysis method that handles hierarchical data structure.",
            "Visualise the prototypical f0 contour of each tone based on f0 data from all speakers.",
            "Summarise the prototypical f0 contour of each tone in textual description.",
            "Report the specific rescaling strategy and decision boundaries if summarising the f0 contours in tone numerals."
          )
        ),

        checklist_section(10, "Open and Reproducible Research",
          "How to adopt open and reproducible research practices?",
          list(
            "Share the study materials, f0 data, preprocessing logs, and analysis scripts via open platforms such as Open Science Framework (OSF) and Github.",
            "Where appropriate and consented to, openly share the monosyllabic corpus including audio recordings and anonymised metadata."
          )
        )
      )
    )
  })

}
