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
        tags$strong("Companion files:"),
        " Use a common ", tags$em("basename"),
        " across companion files. For example, ",
        tags$code("ma1.wav"), ", ", tags$code("ma1.TextGrid"),
        ", and ", tags$code("ma1.Pitch"),
        " are treated as one token."
      ),

      # --- Recommended structured filename convention ---
      tags$style(HTML("
        .fn-pattern, .fn-example {
          font-family: 'SFMono-Regular', Menlo, Consolas, monospace;
          font-size: 1.0rem; padding: 8px 10px; border-radius: 4px;
          background: #ffffff; border: 1px solid #d6e7df;
          display: inline-block; margin: 4px 0;
        }
        .fn-seg { display: inline-block; padding: 1px 6px; border-radius: 3px;
                  font-weight: 600; }
        .fn-sep { color: #aaa; padding: 0 1px; }
        .fn-ext { color: #888; }
        .fn-lang { background: #e8f5f0; color: #2a7a5a; }
        .fn-spk  { background: #e3f2fd; color: #1565c0; }
        .fn-tone { background: #fff3e0; color: #c2410c; }
        .fn-word { background: #fce4ec; color: #c2185b; }
        .fn-rep  { background: #f3e5f5; color: #6a1b9a; }
        .fn-more { background: #ffffff; color: #888; border: 1px dashed #bbb; font-weight: 500; }
        .fn-legend { display: flex; flex-wrap: wrap; gap: 10px 18px;
                     margin: 6px 0 0 0; font-size: 0.82rem; color: #555; }
        .fn-legend > div { display: inline-flex; align-items: center; gap: 6px; }
        .fn-swatch { width: 12px; height: 12px; border-radius: 3px; display: inline-block; }
        .fn-swatch.fn-more { border: 1px dashed #bbb; background: #ffffff; }
        /* Highlight the token-ID portion of the filename (everything before .wav) */
        .fn-tid {
          background: #f4f4f6;
          padding: 4px 8px;
          border-radius: 4px;
          border: 1px solid #d8d8de;
          border-bottom: 2px solid #6c757d;
          box-shadow: 0 1px 2px rgba(0,0,0,0.04);
        }
        .fn-tid-caption {
          font-size: 0.78rem;
          color: #555;
          font-style: italic;
          margin: 4px 0 8px 4px;
        }
      ")),
      h4("Recommended filename convention"),
      tags$p("A consistent, structured filename makes downstream filtering and metadata joins much easier. ",
             "Encode each piece of information you'd want to filter or group by as a separate ",
             "underscore-separated segment:"),
      tags$div(class = "fn-pattern",
        HTML(paste0(
          "<span class='fn-tid'>",
            "<span class='fn-seg fn-lang'>{language}</span>",
            "<span class='fn-sep'>_</span>",
            "<span class='fn-seg fn-spk'>{speaker}</span>",
            "<span class='fn-sep'>_</span>",
            "<span class='fn-seg fn-tone'>{tone}</span>",
            "<span class='fn-sep'>_</span>",
            "<span class='fn-seg fn-word'>{word}</span>",
            "<span class='fn-sep'>_</span>",
            "<span class='fn-seg fn-rep'>{rep}</span>",
            "<span class='fn-sep'>_</span>",
            "<span class='fn-seg fn-more'>{your_variable}</span>",
          "</span>",
          "<span class='fn-ext'>.wav</span>"))),
      tags$div(class = "fn-tid-caption",
               HTML("↑ The shaded portion is the <strong>token ID</strong>.")),
      tags$div(style = "margin-top: 4px;", "Example:"),
      tags$div(class = "fn-example",
        HTML(paste0(
          "<span class='fn-tid'>",
            "<span class='fn-seg fn-lang'>yue</span>",
            "<span class='fn-sep'>_</span>",
            "<span class='fn-seg fn-spk'>S01</span>",
            "<span class='fn-sep'>_</span>",
            "<span class='fn-seg fn-tone'>T2</span>",
            "<span class='fn-sep'>_</span>",
            "<span class='fn-seg fn-word'>ma</span>",
            "<span class='fn-sep'>_</span>",
            "<span class='fn-seg fn-rep'>r01</span>",
          "</span>",
          "<span class='fn-ext'>.wav</span>"))),
      tags$div(class = "fn-tid-caption",
               HTML("→ token ID: <code>yue_S01_T2_ma_r01</code>")),
      tags$div(class = "fn-legend",
        tags$div(tags$span(class = "fn-swatch fn-lang"), "language / project code"),
        tags$div(tags$span(class = "fn-swatch fn-spk"),  "speaker ID (zero-padded)"),
        tags$div(tags$span(class = "fn-swatch fn-tone"), "tone category"),
        tags$div(tags$span(class = "fn-swatch fn-word"), "word (in IPA or romanisation)"),
        tags$div(tags$span(class = "fn-swatch fn-rep"),  "repetition number"),
        tags$div(tags$span(class = "fn-swatch fn-more"), "any extra factor you need (session, context, carrier, etc.)")
      ),
      tags$p(style = "margin-top: 14px; margin-bottom: 4px;", tags$strong("Why this helps")),
      tags$ul(
        tags$li("In ", tags$strong("F0 Correction"),
                ", you can filter the token list by speaker or tone from your Inspect-tab CSV."),
        tags$li("In ", tags$strong("F0 Extraction"),
                ", the downloaded f0 already carries enough info in the token ID to split into metadata columns later."),
        tags$li("Sorted file lists fall in a sensible order, so iterating through tokens is predictable.")),
      tags$p(style = "margin-top: 10px; margin-bottom: 4px;", tags$strong("Tips")),
      tags$ul(
        tags$li("Use ", tags$code("_"), " as the separator. Avoid spaces, dots, and other special symbols."),
        tags$li("Zero-pad numeric parts (", tags$code("S01"), " not ", tags$code("S1"),
                ") so filenames sort naturally."),
        tags$li("Keep each segment short but unambiguous."))
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
