###############################################
# f0 processing → Start subtab
# Upload .wav (+ optional .TextGrid / .Pitch / .PitchTier) files,
# show file specs grouped by basename.
###############################################

fp_start_ui <- function(input, output, session, fp_audio_data) {

  # --- Sidebar: file upload + reset ---
  output$ui_fp_upload <- renderUI({
    tagList(
      fileInput("fp_upload",
                "Choose audio files",
                multiple = TRUE,
                accept = c(".wav", ".WAV",
                           ".TextGrid", ".textgrid",
                           ".Pitch", ".pitch",
                           ".PitchTier", ".pitchtier")),
      tags$p(style = "color: #888; font-size: 0.8rem; margin-top: -8px;",
        "Hold ", tags$kbd("⌘"), " / ", tags$kbd("Ctrl"),
        " to select multiple files. You can include ",
        tags$strong(".wav"), " files alongside optional ",
        tags$strong(".TextGrid"), " annotations and ",
        tags$strong(".Pitch"), " files exported from Praat."
      ),
      tags$hr(),
      actionButton("fp_clear", "Clear all", icon = icon("trash"))
    )
  })

  observeEvent(input$fp_clear, {
    fp_audio_data(NULL)
    showNotification("Uploaded files cleared.", type = "message", duration = 3)
  })

  # --- Process upload into a per-token info table ---
  observeEvent(input$fp_upload, {
    req(input$fp_upload)
    files <- input$fp_upload  # data.frame: name, size, type, datapath

    # Separate by extension
    ext <- tolower(tools::file_ext(files$name))
    base <- tools::file_path_sans_ext(files$name)
    df <- data.frame(name = files$name, ext = ext, base = base,
                     path = files$datapath, size = files$size,
                     stringsAsFactors = FALSE)

    # Per-basename grouping
    bases <- sort(unique(df$base))
    rows <- lapply(bases, function(b) {
      g <- df[df$base == b, , drop = FALSE]
      wav   <- g$path[g$ext == "wav"]
      tg    <- g$path[g$ext == "textgrid"]
      ptch  <- g$path[g$ext == "pitch"]
      ptier <- g$path[g$ext == "pitchtier"]

      info <- list(
        basename = b,
        wav_path = if (length(wav))   wav[1]   else NA_character_,
        tg_path  = if (length(tg))    tg[1]    else NA_character_,
        pitch_path  = if (length(ptch))  ptch[1]  else NA_character_,
        pitchtier_path = if (length(ptier)) ptier[1] else NA_character_,
        sr = NA_integer_, bit = NA_integer_,
        dur = NA_real_, channels = NA_integer_,
        wav_size_kb = NA_real_
      )

      if (!is.na(info$wav_path)) {
        w <- tryCatch(tuneR::readWave(info$wav_path, header = TRUE),
                      error = function(e) NULL)
        if (!is.null(w)) {
          info$sr       <- w$sample.rate
          info$bit      <- w$bits
          info$channels <- w$channels
          info$dur      <- round(w$samples / w$sample.rate, 3)
          info$wav_size_kb <- round(g$size[g$ext == "wav"][1] / 1024, 1)
        }
      }
      as.data.frame(info, stringsAsFactors = FALSE)
    })

    fp_audio_data(do.call(rbind, rows))
  })

  # --- Main panel: introductory guide ---
  # Matches the style of the F0 Analysis Start tab (Welcome + "What each tab does").
  output$fp_start_guide <- renderUI({
    tagList(
      h2("Welcome!"),
      tags$p("Upload your audio files to get started. Each .wav should be a recording of a single syllable or word. Optional ",
             tags$strong(".TextGrid"), ", ", tags$strong(".Pitch"),
             ", and ", tags$strong(".PitchTier"),
             " files exported from Praat can be uploaded alongside the audio."),
      if (is.null(fp_audio_data())) {
        tags$p("Once uploaded, a per-token file summary will appear below.")
      },
      tags$hr(),
      h4("What each tab does"),
      tags$ul(
        tags$li(HTML("&#128194; "),
                tags$strong(style = "color: #78c2ad;", "Start"),
                " Upload your audio files (.wav) and optional companion files (.TextGrid, .Pitch, .PitchTier) and preview their specifications."),
        tags$li(HTML("&#127908; "),
                tags$strong(style = "color: #78c2ad;", "F0 Extraction"),
                " Extract f0 contours from .wav using ", tags$code("wrassp::ksvF0()"),
                ", or use already-computed .Pitch / .PitchTier files from Praat."),
        tags$li(HTML("&#128295; "),
                tags$strong(style = "color: #78c2ad;", "F0 Correction"),
                " Interactively review and correct extraction errors (octave jumps, halving, spurious frames, voiced/unvoiced boundaries).")
      ),
      tags$hr(),
      tags$div(
        style = "background-color: #fff8e1; border-left: 4px solid #e0a800; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem; color: #555;",
        tags$strong("File-naming convention:"),
        " Use a common ", tags$em("basename"),
        " across companion files. For example, ",
        tags$code("ma1.wav"), ", ", tags$code("ma1.TextGrid"),
        ", and ", tags$code("ma1.Pitch"),
        " will all be grouped as one token."
      )
    )
  })

  # --- Main panel: per-token file info ---
  output$fp_files_summary <- renderUI({
    df <- fp_audio_data()
    if (is.null(df) || nrow(df) == 0) {
      return(tags$div(style = "color: #888; font-style: italic; margin: 8px 0;",
                      "No files uploaded yet."))
    }
    n_wav   <- sum(!is.na(df$wav_path))
    n_tg    <- sum(!is.na(df$tg_path))
    n_pitch <- sum(!is.na(df$pitch_path) | !is.na(df$pitchtier_path))
    total_kb <- round(sum(df$wav_size_kb, na.rm = TRUE), 1)

    sr_uniq  <- unique(stats::na.omit(df$sr))
    bit_uniq <- unique(stats::na.omit(df$bit))
    sr_str   <- if (length(sr_uniq))  paste(sr_uniq, collapse = ", ") else "—"
    bit_str  <- if (length(bit_uniq)) paste(bit_uniq, collapse = ", ") else "—"

    tags$div(
      style = "background-color: #eef6f2; border: 1px solid #c8e1d6; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.9rem;",
      tags$strong("Upload summary"),
      tags$ul(style = "margin: 6px 0 0 0; padding-left: 18px;",
        tags$li("Tokens (unique basenames): ", tags$strong(nrow(df))),
        tags$li(".wav files: ", tags$strong(n_wav),
                "  (total ", tags$strong(format(total_kb, big.mark = ",")), " KB)"),
        tags$li(".TextGrid files: ", tags$strong(n_tg)),
        tags$li(".Pitch / .PitchTier files: ", tags$strong(n_pitch)),
        tags$li("Sample rate(s): ", tags$strong(sr_str), " Hz"),
        tags$li("Bit depth(s): ", tags$strong(bit_str), " bits")
      )
    )
  })

  output$fp_files_table <- DT::renderDataTable({
    df <- fp_audio_data()
    req(df)

    display <- data.frame(
      basename = df$basename,
      `.wav`         = ifelse(is.na(df$wav_path), "—", "✓"),
      `.TextGrid`    = ifelse(is.na(df$tg_path),  "—", "✓"),
      `.Pitch`       = ifelse(is.na(df$pitch_path) & is.na(df$pitchtier_path), "—", "✓"),
      `Sample rate (Hz)` = df$sr,
      `Bit depth`        = df$bit,
      Channels           = df$channels,
      `Duration (s)`     = df$dur,
      `.wav size (KB)`   = df$wav_size_kb,
      check.names = FALSE, stringsAsFactors = FALSE
    )

    DT::datatable(
      display,
      rownames = FALSE, filter = "top",
      options = list(
        pageLength = 25, autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    )
  })
}
