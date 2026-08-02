###############################################
# F0 Processing → F0 Extraction subtab
# Backends:
#   (1) Extract from .wav via wrassp::ksvF0()
#   (2) Parse uploaded Praat .Pitch / .PitchTier via rPraat
# Both populate fp_f0_data with a long-format data frame:
#   token (basename), time (s), f0 (Hz, NA on unvoiced frames).
#   The wrassp backend additionally adds an intensity (dB) column (short-term
#   RMS), used by the Inspect tab's low-intensity check. The Praat .Pitch
#   backend has no intensity source, so it omits the column; use the offline
#   Praat script (F0 Processing > Praat script) if you want Praat intensity.
###############################################

fp_extraction_ui <- function(input, output, session, fp_audio_data, fp_f0_data,
                             fp_pitch_candidates = NULL, fp_metadata = NULL,
                             fp_corrected_data = NULL) {

  # Which f0 source the sidebar starts on. Flipped to "praat" by the
  # pitch-file observer further down; declared here so the sidebar
  # renderUI below can read it.
  fp_mode_default <- reactiveVal("wrassp")

  # ---- Sidebar controls ----
  output$ui_fp_extraction <- renderUI({
    tagList(
      # Default read from fp_mode_default(), which the pitch-file observer
      # below sets to "praat". isolate() so a later change re-renders
      # nothing (the observer's updateRadioButtons covers the already-
      # rendered case); without this, uploading pitch files before ever
      # opening this tab would render the sidebar back on "wrassp", since
      # the update message arrives while the input does not yet exist.
      radioButtons("fp_extract_mode", "f0 source:",
                   choices = c("Extract from .wav (wrassp)" = "wrassp",
                               "Use uploaded .Pitch / .PitchTier (Praat)" = "praat",
                               "Upload existing f0 CSV" = "csv"),
                   selected = isolate(fp_mode_default())),
      conditionalPanel("input.fp_extract_mode == 'wrassp'",
        tags$hr(),
        numericInput("fp_f0_min",   "Min f0 (Hz)",   value = 75,  min = 30,  max = 300),
        numericInput("fp_f0_max",   "Max f0 (Hz)",   value = 600, min = 200, max = 1000),
        numericInput("fp_window_ms","Frame step (ms)", value = 10, min = 1,   max = 50)
      ),
      conditionalPanel("input.fp_extract_mode == 'csv'",
        tags$hr(),
        fileInput("fp_f0_upload_file", "Pre-extracted f0 CSV",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                  buttonLabel = "Choose CSV",
                  placeholder = "No file selected"),
        # Column pickers (with inline token-match note); auto-detected names.
        uiOutput("fp_f0_csv_col_pickers")
      ),
      # Separator before the Run button (only for sources that need one — for
      # CSV the auto-load hint sits visually close to the pickers).
      conditionalPanel("input.fp_extract_mode != 'csv'",
        tags$hr()
      ),
      uiOutput("fp_extract_run_btn"),
      tags$hr(),
      # ---- Landmarks from TextGrid (optional) ----
      h5("Landmarks from TextGrid"),
      tags$p(style = "color: #777; font-size: 0.8rem; margin-bottom: 6px;",
        "If you uploaded ", tags$code(".TextGrid"), " files, choose interval tier(s) to read ",
        "boundaries from. Each adds ", tags$code("<tier>"), ", ", tags$code("<tier>_start"),
        ", ", tags$code("<tier>_end"), ", ", tags$code("<tier>_i"),
        " columns (the segment label, its start and end in seconds, and its index) to every ",
        "f0 frame, so the Visualise tab can align contours by these landmarks."),
      uiOutput("fp_landmark_picker"),
      tags$hr(),
      # ---- Metadata (optional) ----
      h5("Metadata"),
      tags$p(style = "color: #777; font-size: 0.8rem; margin-bottom: 6px;",
        "Attach metadata to each audio file so the downloaded dataframe is ready for F0 Analysis."),
      radioButtons("fp_meta_source", NULL,
                   choices = c("None"                    = "none",
                               "Upload metadata CSV"     = "csv",
                               "Derive from filename"    = "filename"),
                   selected = "none"),
      # --- CSV upload path ---
      conditionalPanel("input.fp_meta_source == 'csv'",
        fileInput("fp_meta_file", NULL,
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                  buttonLabel = "Choose CSV",
                  placeholder = "No file selected"),
        # Always-visible filename column picker (empty placeholder before upload) +
        # strip-extension toggle, so users see the full setup up front.
        uiOutput("fp_meta_keycol_ui"),
        checkboxInput("fp_meta_strip_ext",
                      "Strip file extensions when matching",
                      value = TRUE),
        tags$div(style = "color: #888; font-size: 0.75rem; font-style: italic; margin-top: -4px;",
                 "Your CSV needs a column that links each row to an audio file ",
                 "(e.g., a ", tags$code("token"), ", ", tags$code("filename"),
                 ", or ", tags$code("basename"), " column). ",
                 "Values can be with or without the ",
                 tags$code(".wav"), " extension.")
      ),
      # --- Derive-from-filename path ---
      conditionalPanel("input.fp_meta_source == 'filename'",
        textInput("fp_meta_split_sep", "Separator:", value = "_", width = "100%"),
        uiOutput("fp_meta_split_status"),
        textInput("fp_meta_split_colnames", "Column names:",
                  value = "",
                  placeholder = "e.g., language speaker tone word rep",
                  width = "100%"),
        tags$div(style = "color: #888; font-size: 0.75rem; margin-top: -8px; margin-bottom: 6px;",
          "Type names in order, separated by spaces or commas. ",
          "Leave blank (or supply fewer) to use generic ", tags$code("column_N"),
          " for unfilled positions."),
        uiOutput("fp_meta_split_preview")
      ),
      tags$hr(),
      # ---- F0 data export: region + sampling ----
      # These shape the DOWNLOAD only. The in-app data (and so F0 Correction)
      # always keeps the full native grid, so changing them can never
      # invalidate frame edits.
      h5("F0 Data Export"),
      tags$p(style = "color: #777; font-size: 0.8rem; margin-bottom: 6px;",
        "Export f0 data after extraction. ",
        tags$em("See the guide for what each choice does.")),
      tags$strong(style = "font-size: 0.85rem;", "Region"),
      radioButtons("fp_region", NULL,
                   choices = c("Whole token (voiced region)" = "voiced",
                               "TextGrid interval"           = "interval"),
                   selected = "voiced"),
      conditionalPanel("input.fp_region == 'interval'",
        uiOutput("fp_region_interval_ui")),
      tags$strong(style = "font-size: 0.85rem;", "Sampling"),
      radioButtons("fp_sampling_mode", NULL,
                   choices = c("Native frame times"          = "native",
                               "Equidistant points across the region" = "equal"),
                   selected = "native"),
      conditionalPanel("input.fp_sampling_mode == 'equal'",
        numericInput("fp_sampling_n", "Points per token:",
                     value = 21, min = 2, max = 1000, step = 1),
        tags$div(style = "color: #888; font-size: 0.75rem; font-style: italic; margin-top: -4px; margin-bottom: 8px;",
          "21 points = one every 5%; 11 = every 10%."),
        radioButtons("fp_sampling_method", "Value at each point:",
                     choices = c("Linear interpolation" = "linear",
                                 "Nearest measured frame" = "nearest"),
                     selected = "linear")),
      tags$hr(),

      h5("Download"),
      textInput("fp_extract_filename", "Enter filename (without extension):", value = "extracted_f0"),
      downloadButton("fp_extract_download", "Download f0 (CSV)"),
      conditionalPanel("output.fp_have_f0 !== 'yes'",
        tags$div(style = "color: #888; font-size: 0.8rem; margin-top: 6px; font-style: italic;",
                 "Run extraction first to generate the file.")
      ),
      uiOutput("fp_export_summary")
    )
  })

  # How many uploaded recordings Praat pitch files actually cover. Praat mode
  # can only produce f0 for recordings that have a .Pitch / .PitchTier, so
  # partial coverage silently drops the rest; callers use this to refuse to
  # default into that state and to warn before it happens.
  praat_coverage <- function(audio) {
    if (is.null(audio) || nrow(audio) == 0) {
      return(list(n_wav = 0L, n_pitch = 0L, n_missing = 0L, missing = character(0)))
    }
    is_wav   <- !is.na(audio$wav_path)
    has_p    <- !is.na(audio$pitch_path) | !is.na(audio$pitchtier_path)
    uncovered <- is_wav & !has_p
    list(n_wav = sum(is_wav), n_pitch = sum(has_p),
         n_missing = sum(uncovered), missing = audio$basename[uncovered])
  }

  # ---- Default the f0 source to Praat, but only on FULL coverage ----
  # Someone who uploads .Pitch / .PitchTier files for all their recordings
  # almost certainly wants them used: they carry Praat's per-frame candidate
  # lists, which the F0 Correction tab can offer as alternatives. Before this,
  # the radio stayed on wrassp and hitting Extract silently produced wrassp
  # output instead, discarding the candidates the upload was for.
  #
  # Coverage matters because Praat mode yields NOTHING for a .wav without a
  # pitch file: those recordings drop out of the extraction entirely. So a
  # partly-covered upload stays on wrassp (which handles every .wav) and gets
  # a warning naming the uncovered files, rather than defaulting into silent
  # data loss.
  #
  # Fires at most once per session (fp_mode_autoswitched), so a later manual
  # choice is never overridden, and announces itself either way.
  fp_mode_autoswitched <- reactiveVal(FALSE)
  observeEvent(fp_audio_data(), {
    if (isTRUE(fp_mode_autoswitched())) return()
    audio <- fp_audio_data()
    if (is.null(audio) || nrow(audio) == 0) return()
    cov <- praat_coverage(audio)
    if (cov$n_pitch == 0) return()
    fp_mode_autoswitched(TRUE)

    # Partial coverage: do NOT default into Praat. Praat mode would extract
    # only the covered recordings and silently drop the rest, so stay on
    # wrassp (which handles every .wav) and say why.
    if (cov$n_missing > 0) {
      showNotification(
        tags$div(
          tags$strong(sprintf("Praat pitch files cover only %d of %d recordings.",
                              cov$n_pitch, cov$n_wav)),
          tags$div(style = "margin-top:4px;",
            nows(sprintf("%d .wav file(s) have no matching .Pitch / .PitchTier: ",
                         cov$n_missing),
                 tags$code(paste(utils::head(cov$missing, 3), collapse = ", ")),
                 if (cov$n_missing > 3) sprintf(" and %d more", cov$n_missing - 3),
                 ".")),
          tags$div(style = "margin-top:4px;",
            "Left on ", tags$strong("Extract from .wav (wrassp)"),
            ", which covers every recording. Choosing ",
            tags$strong("Praat"),
            " would extract only the covered ones and skip the rest."),
          tags$div(style = "margin-top:6px;",
            "To use Praat for the whole set, re-run the script over every ",
            ".wav and upload the new ", tags$code(".Pitch"), " files: ",
            nows(tags$a(href = "#", style = "font-weight:600; text-decoration:underline;",
                        onclick = paste0("Shiny.setInputValue('about_nav_target', ",
                                         "'F0 Processing|Measure f0 with Praat', ",
                                         "{priority:'event'}); return false;"),
                        "open Measure f0 with Praat"), "."))),
        type = "warning", duration = 20, id = "fp_mode_autoswitch")
      return()
    }

    # Full coverage: Praat is the better default (it keeps the per-frame
    # candidate lists). Covers the not-yet-rendered case (see radioButtons).
    fp_mode_default("praat")
    if (identical(isolate(input$fp_extract_mode), "praat")) return()
    updateRadioButtons(session, "fp_extract_mode", selected = "praat")
    showNotification(
      sprintf(paste("Praat pitch files found for all %d recording(s), so the",
                    "f0 source is set to Praat. It keeps Praat's alternative",
                    "pitch candidates, which you can pick from in F0",
                    "Correction. Switch back to wrassp above if you prefer."),
              cov$n_wav),
      type = "message", duration = 8, id = "fp_mode_autoswitch")
  }, ignoreNULL = TRUE)

  # ---- Read uploaded CSV reactively (only when CSV source is active) ----
  uploaded_csv <- reactive({
    req(input$fp_meta_source == "csv", input$fp_meta_file)
    tryCatch(
      utils::read.csv(input$fp_meta_file$datapath,
                      stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
  })
  observeEvent(input$fp_meta_file, {
    req(input$fp_meta_source == "csv", input$fp_meta_file)
    df <- uploaded_csv()
    if (is.null(df) || ncol(df) == 0) {
      showNotification("Could not read metadata CSV.",
                       type = "error", duration = 5)
      return()
    }
    showNotification(sprintf("Metadata loaded: %d row(s), %d column(s).",
                             nrow(df), ncol(df)),
                     type = "message", duration = 3)
  })

  # ---- Derive-from-filename path ----
  # Default segment names matching the recommended convention.
  default_seg_names <- function(n) {
    base <- c("language", "speaker", "tone", "word", "rep")
    if (n <= length(base)) base[seq_len(n)]
    else c(base, sprintf("var%d", seq.int(length(base) + 1, n)))
  }

  # Reactive: each basename split by separator (list of character vectors).
  fp_meta_splits <- reactive({
    req(input$fp_meta_source == "filename")
    audio <- fp_audio_data()
    req(audio, nrow(audio) > 0)
    sep <- input$fp_meta_split_sep
    req(!is.null(sep), nzchar(sep))
    strsplit(as.character(audio$basename), sep, fixed = TRUE)
  })

  # Status banner: how many segments, are they uniform?
  output$fp_meta_split_status <- renderUI({
    audio <- fp_audio_data()
    if (is.null(audio) || nrow(audio) == 0) {
      return(tags$div(style = "color: #888; font-size: 0.8rem; margin-bottom: 8px; font-style: italic;",
        "Upload audio files in the Start tab; segment counts will be detected from their filenames."))
    }
    splits <- fp_meta_splits()
    n_each <- vapply(splits, length, integer(1))
    n_max  <- max(n_each)
    n_min  <- min(n_each)
    if (n_max == n_min) {
      tags$div(style = "color: #2a7a5a; font-size: 0.8rem; margin-bottom: 8px;",
        sprintf("Detected %d segment(s) in all %d file(s).", n_max, length(splits)))
    } else {
      tags$div(style = "color: #8a6d00; font-size: 0.8rem; margin-bottom: 8px;",
        sprintf("Detected %d–%d segments across %d file(s). Shorter rows pad with NA.",
                n_min, n_max, length(splits)))
    }
  })

  # Parse the user's single column-names input (space- or comma-separated).
  # Returns a character vector of length max-segments, padded with
  # 'column_N' for any positions the user didn't supply.
  fp_meta_split_colnames <- reactive({
    splits <- fp_meta_splits()
    n_max  <- max(vapply(splits, length, integer(1)))
    raw    <- input$fp_meta_split_colnames
    typed  <- if (is.null(raw) || !nzchar(trimws(raw))) character(0)
              else {
                parts <- unlist(strsplit(trimws(raw), "[,[:space:]]+", perl = TRUE))
                parts[nzchar(parts)]
              }
    if (length(typed) >= n_max) {
      typed[seq_len(n_max)]
    } else if (length(typed) == 0) {
      sprintf("column_%d", seq_len(n_max))
    } else {
      c(typed, sprintf("column_%d", seq.int(length(typed) + 1, n_max)))
    }
  })

  # Derived metadata data.frame: token + named segment columns.
  derived_metadata <- reactive({
    splits <- fp_meta_splits()
    audio  <- fp_audio_data()
    n_max  <- max(vapply(splits, length, integer(1)))
    cols   <- fp_meta_split_colnames()
    # Pad each split to n_max with NA, then bind into a matrix
    padded <- lapply(splits, function(x) c(x, rep(NA_character_, n_max - length(x))))
    mat    <- do.call(rbind, padded)
    colnames(mat) <- cols
    data.frame(token = as.character(audio$basename), mat,
               stringsAsFactors = FALSE, check.names = FALSE)
  })

  # Small preview (first 3 rows) shown right below the column-name inputs.
  output$fp_meta_split_preview <- renderUI({
    audio <- fp_audio_data()
    if (is.null(audio) || nrow(audio) == 0) {
      return(tags$div(style = "color: #888; font-size: 0.78rem; font-style: italic; margin-top: 6px;",
                      "A preview of the first 3 rows will appear here after audio is uploaded."))
    }
    df <- derived_metadata()
    req(nrow(df) > 0)
    sub <- utils::head(df, 3)
    hdr <- tags$tr(lapply(names(sub), function(n)
      tags$th(style = "padding: 2px 6px; border-bottom: 1px solid #ccc; font-size: 0.78rem;", n)))
    rows <- lapply(seq_len(nrow(sub)), function(i)
      tags$tr(lapply(names(sub), function(n)
        tags$td(style = "padding: 2px 6px; font-size: 0.78rem; color: #555;",
                as.character(sub[i, n])))))
    tags$div(style = "margin-top: 6px;",
      tags$div(style = "font-size: 0.78rem; color: #777; margin-bottom: 3px;",
               sprintf("Preview (first 3 of %d rows):", nrow(df))),
      tags$table(style = "border-collapse: collapse; width: 100%;",
                 hdr, rows)
    )
  })

  # Active metadata: drive fp_metadata reactively from whichever source is on.
  observe({
    src <- input$fp_meta_source
    if (is.null(src) || src == "none") {
      fp_metadata(NULL)
    } else if (src == "csv") {
      fp_metadata(uploaded_csv())
    } else if (src == "filename") {
      fp_metadata(derived_metadata())
    }
  })

  # Active join-key column: 'token' for the derived path, user's pick for CSV.
  active_keycol <- reactive({
    if (isTRUE(input$fp_meta_source == "filename")) "token"
    else input$fp_meta_keycol
  })

  # Flag for conditionalPanel: do we have metadata loaded?
  output$fp_have_metadata <- reactive({
    md <- if (!is.null(fp_metadata)) fp_metadata() else NULL
    if (!is.null(md) && nrow(md) > 0) "yes" else "no"
  })
  outputOptions(output, "fp_have_metadata", suspendWhenHidden = FALSE)

  # Column selector — auto-populated from metadata columns; defaults to a column
  # that looks like a filename (basename/filename/file/wav).
  # When no CSV is loaded yet, render a placeholder dropdown so users can see
  # the control exists.
  output$fp_meta_keycol_ui <- renderUI({
    md <- if (!is.null(fp_metadata)) fp_metadata() else NULL
    if (is.null(md) || ncol(md) == 0 || !isTRUE(input$fp_meta_source == "csv")) {
      return(selectInput("fp_meta_keycol", "Filename column:",
                         choices  = c("Upload a CSV first" = ""),
                         selected = "",
                         selectize = FALSE))
    }
    cols <- names(md)
    guess <- cols[grepl("^(filename|file|wav|basename|audio|token)$", cols,
                        ignore.case = TRUE)]
    sel <- if (length(guess)) guess[1] else cols[1]
    selectInput("fp_meta_keycol", "Filename column:",
                choices = cols, selected = sel, selectize = FALSE)
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
      guide_box("F0 Extraction guide",
        tags$p(style = "margin: 6px 0 8px 0;",
          HTML(paste0(
            "The sidebar runs top to bottom: pick where the f0 comes from and run the extraction, ",
            "optionally attach metadata and TextGrid landmarks, then choose what the download contains."))),

        tags$strong("F0 extraction:"),
        tags$ul(class = "glist",
          tags$li(HTML(paste0(
            "<strong>wrassp</strong> runs the ksvF0 pitch algorithm (Sch&auml;fer-Vincent, 1983) in R, ",
            "with no external dependencies, and deploys cleanly on shinyapps.io (",
            "<a href='https://cran.r-project.org/package=wrassp' target='_blank'>CRAN</a> &middot; ",
            "<a href='https://github.com/IPS-LMU/wrassp' target='_blank'>GitHub</a>",
            "). It also computes a per-frame <strong>intensity</strong> (dB) track from short-term RMS, ",
            "used by the Inspect tab&#39;s low-intensity check."))),
          tags$li(HTML(paste0(
            "<strong>Praat</strong> uses the .Pitch / .PitchTier files you uploaded alongside the .wav ",
            "files. Choose this if you have already extracted pitch in Praat with custom settings; it ",
            "keeps the per-frame candidates that F0 Correction can offer as alternatives."))),
          tags$li(HTML(paste0(
            "<strong>Upload existing f0 CSV</strong> skips extraction entirely: use it when you ",
            "already have the f0 data from another tool. The CSV needs columns for token / filename, ",
            "time, and f0; common names are ",
            "auto-detected and you can remap. Token values must match the .wav basenames from Start.")))
        ),

        tags$strong("Metadata and landmarks (optional):"),
        tags$ul(class = "glist",
          tags$li(HTML(paste0(
            "<strong>Metadata</strong> either comes from a CSV (one row per audio file) or is derived ",
            "by splitting each filename on a separator (default <code>_</code>) and naming the ",
            "segments. Either way it is joined to the f0 output, so the download is ready for F0 ",
            "Analysis."))),
          tags$li(HTML(paste0(
            "<strong>Landmarks from TextGrid</strong> tag every f0 frame with the segment it falls in. ",
            "Pick one or more <em>interval</em> tiers (<code>syllable</code>, <code>phoneme</code>, ",
            "<code>vowel</code>, <code>rhyme</code>); each adds <code>&lt;tier&gt;</code>, ",
            "<code>&lt;tier&gt;_start</code>, <code>&lt;tier&gt;_end</code> and <code>&lt;tier&gt;_i</code> ",
            "columns. The <strong>Visualise</strong> tab can then align contours by them, syllable by ",
            "syllable for multi-syllable words.")))
        ),

        tags$strong("F0 Data Export:"),
        tags$p(class = "gnote",
          HTML(paste0(
            "These shape the <em>download</em> only. F0 Correction always works on the extracted data ",
            "exactly as it arrived, never on the export, so changing them cannot disturb your frame ",
            "edits. How much detail Correction has therefore depends on the source: wrassp and ",
            "<code>.Pitch</code> give it every frame, while a <code>.PitchTier</code> or an uploaded CSV ",
            "give it only the rows that file contains."))),
        tags$ul(class = "glist",
          tags$li(HTML("<strong>Region</strong> is what the measurement covers."),
            tags$ul(class = "gsub",
              tags$li(HTML(paste0(
                "<em>Whole token</em>: first to last voiced frame. Silence carries no f0, and an edge ",
                "needs two voiced frames in a row, so a lone frame stranded in silence (a tracking ",
                "error) cannot stretch it."))),
              tags$li(HTML(paste0(
                "<em>TextGrid interval</em>: a tier&#39;s vowels (found automatically), the rhyme, or ",
                "labels you provide. Vowel matching ignores length marks, tone digits and diacritics, ",
                "and counts di-/triphthongs and syllabic nasals."))))),
          tags$li(HTML("<strong>Sampling</strong> is how densely."),
            tags$ul(class = "gsub",
              tags$li(HTML("<em>Native frame times</em>: every frame the tracker produced.")),
              tags$li(HTML(paste0(
                "<em>Equidistant points</em>: N points across the region (21 = every 5%, 11 = every ",
                "10%). This is time normalisation at the sampling stage, so every token lands on the ",
                "same 0 to 100% axis. It adds <code>point</code> (1 to N) and <code>time_prop</code> ",
                "(0 to 1), which the Model tabs recognise as an already-normalised time axis."))))),
          tags$li(HTML(paste0(
            "<strong>Value at each point</strong> works the same as Praat&#39;s ",
            "<code>Pitch: Get value at time...</code> command, with the same two interpolation ",
            "choices, so the numbers match a Praat script querying the same times.")),
            tags$ul(class = "gsub",
              tags$li(HTML(paste0(
                "<em>Linear interpolation</em>: a weighted average of the two frames on either side, ",
                "favouring the nearer one. If only the far frame is unvoiced, the nearer frame&#39;s ",
                "measured value is kept; if the nearer frame itself is unvoiced, the point is empty."))),
              tags$li(HTML(paste0(
                "<em>Nearest measured frame</em>: the frame the point falls in, so every value is one ",
                "the tracker measured."))))),
          tags$li(HTML(paste0(
            "<strong>Unvoiced stretches are never averaged across</strong> when the input marks them, ",
            "as wrassp and <code>.Pitch</code> do. A <code>.PitchTier</code> or a CSV of only voiced ",
            "samples has no such frames, so a silent stretch there reads as a plain gap and is ",
            "interpolated across, exactly as Praat would."))),
          tags$li(HTML("<strong>Two checks run on every export.</strong>"),
            tags$ul(class = "gsub",
              tags$li(HTML(paste0(
                "<code>has_gap</code> and <code>n_missing</code> mark tokens whose voicing was ",
                "interrupted mid-region. Measured on the native frames before resampling, since ",
                "resampling can fill a short dropout from the nearer frame."))),
              tags$li(HTML(paste0(
                "<code>voiced_s</code> and <code>voiced_prop</code> in the summary table above give ",
                "each token&#39;s voiced span and its share of the recording ",
                "(<code>voiced_s</code> / <code>duration_s</code>). Cells below half the corpus median ",
                "are shaded, and the summary under the Download button names those tokens: their ",
                "percentage positions are squeezed into whatever was tracked, so they are not ",
                "comparable with the others. The comparison is median-relative because some silence ",
                "around a token is normal.")))))
        ),

        tags$p(class = "gnote",
          HTML(paste0(
            "The complete frame grid, silence rows included, is still available from F0 Correction&#39;s ",
            "download if you need the raw extraction."))),
        tags$style(HTML("
          .glist{margin:4px 0 10px 0;padding-left:18px;}
          .glist>li{margin-bottom:6px;}
          .gsub{margin:3px 0 0 0;padding-left:16px;list-style-type:circle;}
          .gsub>li{margin-bottom:3px;}
          .gnote{color:#5f6b66;font-size:0.85rem;margin:4px 0 6px;}
        "))
      ),

        # --- Collapsible illustrated guide for the f0 data export ---
        tags$details(class = "msg-route",
          tags$style(HTML("
            details.msg-route{background:#f3f8fc;border:1px solid #cfe2f1;border-radius:8px;padding:7px 14px 11px;margin:6px 0 0;}
            .msg-route>summary{cursor:pointer;font-weight:700;color:#2c5d80;font-size:0.9rem;list-style:none;padding:1px 0;}
            .msg-route>summary::-webkit-details-marker{display:none;}
            .msg-route>summary::before{content:'\\25B8';color:#5b9bd5;display:inline-block;margin-right:8px;transition:transform .15s ease;}
            .msg-route[open]>summary::before{transform:rotate(90deg);}
            .msg-hint{color:#7aa6cc;font-weight:400;font-size:0.78rem;margin-left:6px;}
            .msg-route[open] .msg-hint{display:none;}
            .msg-intro{color:#3f5a72;font-size:0.83rem;line-height:1.5;margin:9px 0 0;}
            .msg-illus{margin:11px 0 3px;}
            .msg-illus svg{width:100%;height:auto;display:block;}
            .msg-opts{display:flex;gap:9px;margin-top:11px;flex-wrap:wrap;}
            .msg-step{flex:1 1 165px;background:#fff;border:1px solid #e1e9f2;border-radius:7px;padding:8px 12px;}
            .msg-stitle{font-weight:700;color:#2c5f4f;font-size:0.86rem;}
            .msg-swhy{font-size:0.76rem;color:#5f6b66;line-height:1.4;margin-top:2px;}
            .msg-tab{display:inline-block;background:#e8f5f0;color:#2c5f4f;padding:1px 7px;border-radius:10px;font-size:0.72rem;font-weight:600;white-space:nowrap;}
            .msg-tip{font-size:0.78rem;color:#33536f;background:#eaf3fb;border:1px solid #d3e6f5;border-radius:6px;padding:6px 11px;margin-top:11px;line-height:1.55;}
            .msg-tip .fa,.msg-tip svg{color:#5b9bd5;margin-right:4px;}
          ")),
          tags$summary(icon("ruler-horizontal"),
                       " What should I export? Monosyllables and multisyllables",
                       tags$span(class = "msg-hint", "(click to expand)")),
          tags$p(class = "msg-intro",
            HTML(paste0(
              "The default, <strong>native frame times</strong> across the ",
              "<strong>whole token</strong>, is the right starting point for both ",
              "monosyllabic and multisyllabic data: it keeps every measurement the ",
              "pitch tracker made, and drops only the silence that carries no f0. ",
              "Change it when your analysis needs something more specific."))),
          tags$div(class = "msg-illus", HTML('<svg width="100%" viewBox="0 0 680 206" role="img" xmlns="http://www.w3.org/2000/svg"> <title>Sampling and region choices for the analysis export</title> <desc>Left: native frame times give a dense fixed-step grid, equidistant points give the same count at the same percentage positions in every token. Right: the region is either the whole token from first to last voiced frame, or a TextGrid interval such as the vowel or the rhyme.</desc> <text x="160" y="14" font-size="12" font-weight="700" fill="#2c5d80" text-anchor="middle">Sampling</text> <text x="500" y="14" font-size="12" font-weight="700" fill="#2c5d80" text-anchor="middle">Region</text> <line x1="330" y1="6" x2="330" y2="200" stroke="#d9e6f1" stroke-width="1"/> <!-- SAMPLING: native --> <text x="160" y="34" font-size="11" font-weight="600" fill="#48667e" text-anchor="middle">Native frame times</text> <rect x="30" y="42" width="45" height="24" fill="#f3f6f8" stroke="#e2e8ee" stroke-width="1"/> <rect x="75" y="42" width="180" height="24" fill="#e8f2fa" stroke="#b8d2e8" stroke-width="1"/> <rect x="255" y="42" width="35" height="24" fill="#f3f6f8" stroke="#e2e8ee" stroke-width="1"/> <g stroke="#5b9bd5" stroke-width="1.4"> <line x1="78" y1="44" x2="78" y2="64"/><line x1="87" y1="44" x2="87" y2="64"/><line x1="96" y1="44" x2="96" y2="64"/> <line x1="105" y1="44" x2="105" y2="64"/><line x1="114" y1="44" x2="114" y2="64"/><line x1="123" y1="44" x2="123" y2="64"/> <line x1="132" y1="44" x2="132" y2="64"/><line x1="141" y1="44" x2="141" y2="64"/><line x1="150" y1="44" x2="150" y2="64"/> <line x1="159" y1="44" x2="159" y2="64"/><line x1="168" y1="44" x2="168" y2="64"/><line x1="177" y1="44" x2="177" y2="64"/> <line x1="186" y1="44" x2="186" y2="64"/><line x1="195" y1="44" x2="195" y2="64"/><line x1="204" y1="44" x2="204" y2="64"/> <line x1="213" y1="44" x2="213" y2="64"/><line x1="222" y1="44" x2="222" y2="64"/><line x1="231" y1="44" x2="231" y2="64"/> <line x1="240" y1="44" x2="240" y2="64"/><line x1="249" y1="44" x2="249" y2="64"/> </g> <text x="52" y="78" font-size="9" fill="#a8b3bb" text-anchor="middle">silence</text> <text x="165" y="78" font-size="9" fill="#7f97ad" text-anchor="middle">voiced</text> <text x="160" y="92" font-size="10" fill="#5f6b66" text-anchor="middle">one frame every 10 ms, so a longer token gets more</text> <!-- SAMPLING: equidistant points --> <text x="160" y="118" font-size="11" font-weight="600" fill="#48667e" text-anchor="middle">Equidistant points across the region</text> <rect x="30" y="126" width="45" height="24" fill="#f3f6f8" stroke="#e2e8ee" stroke-width="1"/> <rect x="75" y="126" width="180" height="24" fill="#e8f2fa" stroke="#b8d2e8" stroke-width="1"/> <rect x="255" y="126" width="35" height="24" fill="#f3f6f8" stroke="#e2e8ee" stroke-width="1"/> <g stroke="#2f9e79" stroke-width="2"> <line x1="75" y1="128" x2="75" y2="148"/><line x1="93" y1="128" x2="93" y2="148"/><line x1="111" y1="128" x2="111" y2="148"/> <line x1="129" y1="128" x2="129" y2="148"/><line x1="147" y1="128" x2="147" y2="148"/><line x1="165" y1="128" x2="165" y2="148"/> <line x1="183" y1="128" x2="183" y2="148"/><line x1="201" y1="128" x2="201" y2="148"/><line x1="219" y1="128" x2="219" y2="148"/> <line x1="237" y1="128" x2="237" y2="148"/><line x1="255" y1="128" x2="255" y2="148"/> </g> <text x="75" y="162" font-size="9" fill="#97a4ac" text-anchor="middle">0%</text> <text x="165" y="162" font-size="9" fill="#97a4ac" text-anchor="middle">50%</text> <text x="255" y="162" font-size="9" fill="#97a4ac" text-anchor="middle">100%</text> <text x="160" y="178" font-size="10" fill="#5f6b66" text-anchor="middle">same count, same % positions, in every token</text> <!-- REGION --> <rect x="352" y="30" width="30" height="24" fill="#f3f6f8" stroke="#e2e8ee" stroke-width="1"/> <rect x="382" y="30" width="50" height="24" fill="#eef4f9" stroke="#b8d2e8" stroke-width="1"/> <rect x="432" y="30" width="100" height="24" fill="#e3f1ea" stroke="#8dc4ac" stroke-width="1"/> <rect x="532" y="30" width="50" height="24" fill="#eef4f9" stroke="#b8d2e8" stroke-width="1"/> <rect x="582" y="30" width="30" height="24" fill="#f3f6f8" stroke="#e2e8ee" stroke-width="1"/> <text x="407" y="46" font-size="11" fill="#48667e" text-anchor="middle">m</text> <text x="482" y="46" font-size="11" font-weight="700" fill="#2c5f4f" text-anchor="middle">a</text> <text x="557" y="46" font-size="11" fill="#48667e" text-anchor="middle">n</text> <text x="367" y="24" font-size="8" fill="#a8b3bb" text-anchor="middle">sil</text> <text x="597" y="24" font-size="8" fill="#a8b3bb" text-anchor="middle">sil</text> <path d="M382,68 v5 h200 v-5" fill="none" stroke="#3a7ca5" stroke-width="1.8"/> <text x="482" y="88" font-size="10" font-weight="600" fill="#2c5d80" text-anchor="middle">Whole token (voiced region)</text> <text x="482" y="101" font-size="9" fill="#5f6b66" text-anchor="middle">first to last voiced frame</text> <path d="M432,116 v5 h100 v-5" fill="none" stroke="#2f9e79" stroke-width="1.8"/> <text x="482" y="136" font-size="10" font-weight="600" fill="#2c5f4f" text-anchor="middle">TextGrid interval: vowel</text> <text x="482" y="149" font-size="9" fill="#5f6b66" text-anchor="middle">the nucleus only</text> <path d="M432,164 v5 h150 v-5" fill="none" stroke="#2f9e79" stroke-width="1.8"/> <text x="507" y="184" font-size="10" font-weight="600" fill="#2c5f4f" text-anchor="middle">TextGrid interval: rhyme</text> <text x="507" y="196" font-size="9" fill="#5f6b66" text-anchor="middle">vowel + coda, monosyllables only</text> </svg>')),
          tags$div(class = "msg-illus", HTML('<svg width="100%" viewBox="0 0 680 250" role="img" xmlns="http://www.w3.org/2000/svg"> <title>How each equidistant point takes its f0, following Praat</title> <desc>The tracker measures f0 on its own frame grid every 10 ms, drawn as filled dots where it found f0 and hollow dots where the sound was unvoiced. Resampling lays an evenly spaced grid over the same span. Following Praat, a point is a weighted average of the two frames around it, favouring the nearer one; if only the far frame is unvoiced the nearer frame value is kept; if the nearer frame is unvoiced the point is NA.</desc> <text x="340.0" y="13" font-size="12" font-weight="700" fill="#2c5d80" text-anchor="middle">How each point takes its f0 (linear interpolation)</text> <g stroke="#cfe8dc" stroke-width="1" stroke-dasharray="3,3"> <line x1="95.2" y1="34" x2="95.2" y2="150"/><line x1="185.87" y1="34" x2="185.87" y2="150"/> <line x1="276.53" y1="34" x2="276.53" y2="150"/><line x1="367.2" y1="34" x2="367.2" y2="150"/> <line x1="457.87" y1="34" x2="457.87" y2="150"/><line x1="548.53" y1="34" x2="548.53" y2="150"/> <line x1="639.2" y1="34" x2="639.2" y2="150"/> </g> <polyline points="95.2,100 149.6,92 204.0,84 258.4,76 312.8,70" fill="none" stroke="#9fc4dd" stroke-width="2"/> <polyline points="476.0,64 530.4,58 584.8,54 639.2,50" fill="none" stroke="#9fc4dd" stroke-width="2"/> <g fill="#2b6f9e" stroke="#fff" stroke-width="1"> <circle cx="95.2" cy="100" r="4.6"/><circle cx="149.6" cy="92" r="4.6"/><circle cx="204.0" cy="84" r="4.6"/> <circle cx="258.4" cy="76" r="4.6"/><circle cx="312.8" cy="70" r="4.6"/> <circle cx="476.0" cy="64" r="4.6"/><circle cx="530.4" cy="58" r="4.6"/><circle cx="584.8" cy="54" r="4.6"/> <circle cx="639.2" cy="50" r="4.6"/> </g> <g fill="#fff" stroke="#c9b7ab" stroke-width="1.8"> <circle cx="367.2" cy="67" r="4.6"/><circle cx="421.6" cy="65" r="4.6"/> </g> <g fill="#2f9e79" stroke="#fff" stroke-width="0.8"> <circle cx="95.2" cy="100" r="3.4"/><circle cx="185.87" cy="86.67" r="3.4"/> <circle cx="276.53" cy="74" r="3.4"/><circle cx="457.87" cy="64" r="3.4"/> <circle cx="548.53" cy="56.67" r="3.4"/><circle cx="639.2" cy="50" r="3.4"/> </g> <text x="367.2" y="46" font-size="10.5" font-weight="700" fill="#b08a72" text-anchor="middle">NA</text> <text x="276.53" y="30" font-size="11" font-weight="700" fill="#2f9e79" text-anchor="middle">1</text> <text x="367.2" y="30" font-size="11" font-weight="700" fill="#b08a72" text-anchor="middle">2</text> <text x="457.87" y="30" font-size="11" font-weight="700" fill="#2f9e79" text-anchor="middle">3</text> <line x1="81.6" y1="122" x2="652.8" y2="122" stroke="#c8d6e0" stroke-width="1"/> <g stroke="#2b6f9e" stroke-width="1.8"> <line x1="95.2" y1="116" x2="95.2" y2="128"/><line x1="149.6" y1="116" x2="149.6" y2="128"/> <line x1="204.0" y1="116" x2="204.0" y2="128"/><line x1="258.4" y1="116" x2="258.4" y2="128"/> <line x1="312.8" y1="116" x2="312.8" y2="128"/><line x1="367.2" y1="116" x2="367.2" y2="128"/> <line x1="421.6" y1="116" x2="421.6" y2="128"/><line x1="476.0" y1="116" x2="476.0" y2="128"/> <line x1="530.4" y1="116" x2="530.4" y2="128"/><line x1="584.8" y1="116" x2="584.8" y2="128"/> <line x1="639.2" y1="116" x2="639.2" y2="128"/> </g> <text x="340.0" y="140" font-size="9.5" fill="#5a7285" text-anchor="middle">native frames, one every 10 ms (more of them in a longer token)</text> <line x1="81.6" y1="156" x2="652.8" y2="156" stroke="#bfe0d2" stroke-width="1"/> <g stroke="#2f9e79" stroke-width="2.8"> <line x1="95.2" y1="149" x2="95.2" y2="163"/><line x1="185.87" y1="149" x2="185.87" y2="163"/> <line x1="276.53" y1="149" x2="276.53" y2="163"/><line x1="367.2" y1="149" x2="367.2" y2="163"/> <line x1="457.87" y1="149" x2="457.87" y2="163"/><line x1="548.53" y1="149" x2="548.53" y2="163"/> <line x1="639.2" y1="149" x2="639.2" y2="163"/> </g> <text x="95.2" y="175" font-size="9" fill="#97a4ac" text-anchor="middle">0%</text> <text x="367.2" y="175" font-size="9" fill="#97a4ac" text-anchor="middle">50%</text> <text x="639.2" y="175" font-size="9" fill="#97a4ac" text-anchor="middle">100%</text> <text x="231.2" y="175" font-size="9.5" fill="#3f7d67" text-anchor="middle">the equidistant grid</text> <circle cx="35.36" cy="194" r="4.6" fill="#2b6f9e" stroke="#fff" stroke-width="1"/> <text x="47.6" y="197" font-size="9" fill="#7f8b93">native frame with f0</text> <circle cx="206.72" cy="194" r="4.6" fill="#fff" stroke="#c9b7ab" stroke-width="1.8"/> <text x="218.96" y="197" font-size="9" fill="#7f8b93">unvoiced frame</text> <circle cx="342.72" cy="194" r="3.4" fill="#2f9e79"/> <text x="354.96" y="197" font-size="9" fill="#7f8b93">resampled point</text> <text x="27.2" y="214" font-size="10" fill="#2f9e79" font-weight="700">1</text> <text x="43.52" y="214" font-size="10" fill="#5f6b66">both frames around it have f0: a weighted average of the two, favouring the nearer.</text> <text x="27.2" y="228" font-size="10" fill="#b08a72" font-weight="700">2</text> <text x="43.52" y="228" font-size="10" fill="#5f6b66">the nearer frame is unvoiced: NA, since nothing was measured there.</text> <text x="27.2" y="242" font-size="10" fill="#2f9e79" font-weight="700">3</text> <text x="43.52" y="242" font-size="10" fill="#5f6b66">only the far frame is unvoiced: the nearer frame value is kept, never averaged across.</text> </svg>')),
          
          tags$div(class = "msg-opts",
            tags$div(class = "msg-step",
              tags$div(class = "msg-stitle", "Monosyllabic words"),
              tags$div(class = "msg-swhy",
                HTML(paste0(
                  "Keep the default. Switch to <strong>equidistant points</strong> when you need a fixed ",
                  "number of measurements per token, and narrow the region to the vowel or rhyme when the ",
                  "onset consonant is not considered in your study.")))),
            tags$div(class = "msg-step",
              tags$div(class = "msg-stitle", "Multisyllabic words"),
              tags$div(class = "msg-swhy",
                HTML(paste0(
                  "Keep the default here too, and align by landmarks rather than by percentage: tick the ",
                  "<code>syllable</code> tier under <strong>Landmarks from TextGrid</strong>, then build the ",
                  "<code>&lt;tier&gt;_tseq</code> axis in <span class=\"msg-tab\">Normalise</span>."))))
          ),
          tags$div(class = "msg-tip",
            icon("lightbulb"),
            HTML(paste0(
              " These settings shape the download only, so nothing is lost by trying one: ",
              "F0 Correction keeps working on the extracted data as it arrived. Corrections you ",
              "make in this session are picked up by the export with no upload needed; only if you ",
              "close the app and come back do you need to re-upload the corrected CSV.")))
        )
    )
  })

  # ---- Helpers: extract one token from .wav (wrassp) ----
  # Returns list(df, candidates = NULL) for uniformity with extract_praat_one.
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
    f0[f0 == 0] <- NA_real_

    # Intensity (dB) via short-term RMS, sampled at the f0 frame times so the
    # two tracks line up one-to-one. Matching windowShift keeps the frame
    # grids close; approx() with rule = 2 fills the ends without adding NA.
    intensity <- tryCatch({
      rms_obj <- wrassp::rmsana(wav_path, toFile = FALSE,
                                windowShift = step_ms, verbose = FALSE)
      rms_v  <- as.vector(rms_obj$rms)
      rms_sr <- attr(rms_obj, "sampleRate")
      rms_t0 <- attr(rms_obj, "startTime")
      rms_t  <- seq(rms_t0, by = 1 / rms_sr, length.out = length(rms_v))
      stats::approx(rms_t, rms_v, xout = t, rule = 2)$y
    }, error = function(e) {
      warning("rmsana failed for ", basename, ": ", e$message)
      rep(NA_real_, length(t))
    })

    list(
      df = data.frame(token = basename, time = t, f0 = f0,
                      intensity = intensity,
                      stringsAsFactors = FALSE),
      candidates = NULL
    )
  }

  # ---- Helpers: parse one .Pitch or .PitchTier file (rPraat) ----
  # Returns a list(df = ..., candidates = ... or NULL)
  #   df         : data.frame(token, time, f0)
  #   candidates : list of per-frame data.frame(frequency, strength) — only
  #                populated for .Pitch files (the source has alternatives);
  #                NULL for .PitchTier (which is a sparse curve).
  extract_praat_one <- function(pitch_path, pitchtier_path, basename) {
    if (!is.na(pitch_path)) {
      # suppressWarnings: a binary / non-UTF-8 file makes rPraat spew dozens of
      # vroom + "unable to translate to a wide string" warnings before erroring.
      parsed <- suppressWarnings(tryCatch(rPraat::pitch.read(pitch_path), error = function(e) NULL))
      if (!is.null(parsed)) {
        n <- parsed$nx
        if (is.null(n) || n == 0) return(NULL)
        t <- if (!is.null(parsed$t)) parsed$t
             else parsed$x1 + (0:(n - 1)) * parsed$dx
        # Preserve ALL candidates per frame for the Correction tab to pick from
        cands_list <- lapply(parsed$frame, function(fr) {
          if (is.null(fr$nCandidates) || fr$nCandidates == 0) {
            return(data.frame(frequency = numeric(0), strength = numeric(0)))
          }
          data.frame(frequency = fr$frequency, strength = fr$strength)
        })
        f0 <- vapply(cands_list, function(c) {
          if (nrow(c) == 0) NA_real_ else c$frequency[1]
        }, numeric(1))
        # Praat semantics: frequency 0 OR at/above the analysis ceiling means
        # voiceless (the ceiling is stored in the .Pitch file). Praat's own
        # editor never draws the >= ceiling placeholders, so drop them from the
        # stored candidates too; otherwise they skew the Correction plot
        # (e.g. 17 kHz "candidates" in unvoiced edge frames). 0 Hz stays in the
        # candidate lists: it is the legitimate "unvoiced" pick option.
        ceil <- if (!is.null(parsed$ceiling)) as.numeric(parsed$ceiling) else Inf
        f0[f0 == 0 | f0 >= ceil] <- NA_real_
        cands_list <- lapply(cands_list, function(c)
          c[c$frequency < ceil, , drop = FALSE])
        # Per-frame intensity from the Pitch frames (Praat stores a RELATIVE
        # 0-1 value, not dB). Drives the Correction plot's dot sizing and the
        # sonification loudness envelope.
        intens <- vapply(parsed$frame, function(fr) {
          if (is.null(fr$intensity)) NA_real_ else as.numeric(fr$intensity)
        }, numeric(1))
        return(list(
          df = data.frame(token = basename, time = t, f0 = f0,
                          intensity = intens, stringsAsFactors = FALSE),
          candidates = cands_list
        ))
      }
    }
    if (!is.na(pitchtier_path)) {
      parsed <- suppressWarnings(tryCatch(rPraat::pt.read(pitchtier_path), error = function(e) NULL))
      if (!is.null(parsed)) {
        return(list(
          # intensity = NA keeps columns identical to the .Pitch branch, so a
          # run mixing .Pitch and .PitchTier files rbinds cleanly.
          df = data.frame(token = basename, time = parsed$t, f0 = parsed$f,
                          intensity = NA_real_, stringsAsFactors = FALSE),
          candidates = NULL
        ))
      }
    }
    NULL
  }

  # Action button: only shown for wrassp / Praat sources. CSV upload
  # auto-loads via the observe() below as soon as file + column picks are valid.
  output$fp_extract_run_btn <- renderUI({
    mode <- input$fp_extract_mode
    if (is.null(mode)) mode <- "wrassp"
    if (mode == "csv") {
      tags$div(style = "color: #888; font-size: 0.78rem; font-style: italic;",
        "f0 data loads automatically once you choose a CSV and the columns are mapped.")
    } else {
      actionButton("fp_extract_run", "Run extraction", icon = icon("play"))
    }
  })

  # ---- Landmarks from TextGrid (optional) ----
  # Interval-tier names available across the uploaded TextGrids (sampled, since
  # tiers are usually uniform across a corpus). Empty unless TextGrids exist.
  fp_tg_tiers <- reactive({
    audio <- fp_audio_data()
    if (is.null(audio) || !"tg_path" %in% names(audio)) return(character(0))
    tg_interval_tiers(audio$tg_path)
  })

  output$fp_landmark_picker <- renderUI({
    tiers <- fp_tg_tiers()
    if (length(tiers) == 0) {
      return(tags$div(style = "color: #999; font-size: 0.78rem; font-style: italic;",
        "Upload .TextGrid files in the Start tab to enable landmark columns."))
    }
    selectizeInput("fp_landmark_tiers", NULL, choices = tiers, selected = character(0),
                   multiple = TRUE,
                   options = list(placeholder = "No landmarks (leave empty)"))
  })

  # Attach the selected TextGrid landmark columns to a freshly-extracted frame.
  attach_landmarks_if_any <- function(d) {
    tiers <- input$fp_landmark_tiers
    if (is.null(tiers) || length(tiers) == 0 || is.null(d)) return(d)
    out <- tryCatch(attach_landmarks(d, fp_audio_data(), tiers),
                    error = function(e) {
                      showNotification(paste("Could not attach landmarks:", conditionMessage(e)),
                                       type = "warning", duration = 6)
                      d
                    })
    n_new <- length(setdiff(names(out), names(d)))
    if (n_new > 0) {
      n_tok <- length(unique(out$token[!is.na(out[[setdiff(names(out), names(d))[1]]])]))
      showNotification(
        sprintf("Attached %d landmark column(s) from tier(s) %s. Matched a TextGrid for %d token(s).",
                n_new, paste(tiers, collapse = ", "), n_tok),
        type = "message", duration = 6)
    }
    out
  }

  # ---- F0 data export: region + sampling ----------------------------------
  # fp_f0_data ALWAYS holds the full native grid, so F0 Correction's frame
  # edits can never be invalidated by a change here. Region and sampling are
  # applied on the way out, as a derived dataset for download.
  #
  #   region   : which rows count  (whole file / voiced span / TextGrid interval)
  #   sampling : how they are measured (native frame times / N equidistant points)

  # Interval-region controls, shown only when that region is chosen.
  output$fp_region_interval_ui <- renderUI({
    tiers <- fp_tg_tiers()
    if (length(tiers) == 0) {
      return(tags$div(style = "color: #999; font-size: 0.78rem; font-style: italic; margin-bottom: 8px;",
        "Upload .TextGrid files in the Start tab to measure across an interval."))
    }
    tagList(
      selectInput("fp_region_tier", "Tier:", choices = tiers, selectize = FALSE),
      radioButtons("fp_region_mode", NULL,
                   choices = c("Vowel (automatic, IPA labels)" = "vowel",
                               "Rhyme (for monosyllables only)" = "rhyme",
                               "Custom labels"                  = "labels"),
                   selected = "vowel"),
      conditionalPanel("input.fp_region_mode == 'labels'",
        textInput("fp_region_labels", "Labels to keep:",
                  placeholder = "e.g. a, ai, an")),
      conditionalPanel("input.fp_region_mode == 'rhyme'",
        tags$div(style = "color: #888; font-size: 0.75rem; font-style: italic; margin-top: -4px; margin-bottom: 8px;",
          "Keeps labelled intervals from the first vowel to the end of the ",
          "token (vowel + coda). Only valid for monosyllabic tokens.")),
      conditionalPanel("input.fp_region_mode == 'vowel'",
        tags$div(style = "color: #888; font-size: 0.75rem; font-style: italic; margin-top: -4px; margin-bottom: 8px;",
          "Matches labels made only of IPA vowel letters. Length marks, tone ",
          "digits and diacritics are ignored, di-/triphthongs and syllabic ",
          "nasals count."))
    )
  })

  # Apply the chosen region to the full grid. Returns list(data, note) where
  # `note` explains any token loss; NULL when the region cannot be built.
  apply_region <- function(d) {
    region <- input$fp_region
    if (is.null(region)) region <- "all"
    if (identical(region, "all")) return(list(data = d, note = NULL))

    n_tok0 <- length(unique(d$token))
    if (identical(region, "voiced")) {
      out <- tryCatch(trim_to_voiced(d), error = function(e) NULL)
      if (is.null(out)) return(list(data = d, note = "Could not find voiced regions; exporting the whole file."))
      lost <- n_tok0 - length(unique(out$token))
      return(list(data = out,
                  note = if (lost > 0)
                    sprintf("%d token(s) have no voiced frame and are excluded.", lost)))
    }

    # TextGrid interval region.
    tier <- input$fp_region_tier
    if (is.null(tier) || !nzchar(tier)) return(NULL)
    mode <- input$fp_region_mode
    if (is.null(mode)) mode <- "vowel"

    # Attach the tier to a minimal token/time frame, then graft its landmark
    # columns on, so a tier that was not ticked as a landmark still works.
    key <- d[, c("token", "time"), drop = FALSE]
    res <- tryCatch(attach_landmarks(key, fp_audio_data(), tier),
                    error = function(e) NULL)
    if (is.null(res)) return(NULL)
    new_cols <- setdiff(names(res), names(key))
    lab_col  <- new_cols[paste0(new_cols, "_start") %in% new_cols][1]
    if (is.na(lab_col)) return(NULL)
    d2 <- d
    for (cl in new_cols) d2[[cl]] <- res[[cl]]

    labels <- NULL
    if (identical(mode, "labels")) {
      raw <- input$fp_region_labels
      if (is.null(raw)) raw <- ""
      labels <- trimws(unlist(strsplit(raw, "[,;]")))
      labels <- labels[nzchar(labels)]
      if (length(labels) == 0) return(NULL)
    }
    sub <- tryCatch(filter_interval_rows(d2, lab_col, mode = mode, labels = labels),
                    error = function(e) NULL)
    if (is.null(sub)) return(NULL)

    no_lab <- sum(tapply(is.na(d2[[lab_col]]), d2$token, all))
    list(data = sub,
         note = if (no_lab > 0)
           sprintf(paste("%d token(s) have no labelled interval on this tier",
                         "(no matching TextGrid, no tier of that name, or no",
                         "frame inside its span) and are excluded."), no_lab))
  }

  # The dataset the Download button writes: fp_f0_data -> region -> sampling.
  # Returns list(data, region, sampling, notes, dropped) or NULL.
  fp_export_data <- reactive({
    d <- fp_f0_data()
    if (is.null(d) || nrow(d) == 0) return(NULL)

    # Measure the CORRECTED contours when F0 Correction has produced any:
    # f0_corrected replaces f0, and whole-token discards drop out. Falls back
    # to the raw extraction when nothing has been corrected yet.
    n_edited <- 0L; n_discarded <- 0L
    cd <- if (is.null(fp_corrected_data)) NULL else fp_corrected_data()
    if (!is.null(cd) && nrow(cd) == nrow(d) && "f0_corrected" %in% names(cd)) {
      if ("edited" %in% names(cd)) {
        n_edited <- length(unique(cd$token[cd$edited %in% TRUE]))
      }
      d$f0 <- cd$f0_corrected
      if ("token_dropped" %in% names(cd)) {
        drop <- cd$token_dropped %in% TRUE
        n_discarded <- length(unique(cd$token[drop]))
        d <- d[!drop, , drop = FALSE]
      }
      if (nrow(d) == 0) return(NULL)
    }

    reg <- apply_region(d)
    if (is.null(reg)) return(NULL)
    out   <- reg$data
    notes <- reg$note

    # Equidistant percentages are relative to the REGION, so a token whose
    # voicing was only partly tracked has its points squeezed into whatever
    # was found: its 50% is not the same linguistic position as everyone
    # else's. Flag the tokens whose voiced span is far shorter than the rest
    # of the corpus, rather than let that pass silently. Compared against the
    # corpus median, since a healthy share (silence padding) is normal.
    if (identical(input$fp_region, "voiced") && nrow(out) > 0) {
      span <- function(x) if (length(x) > 1) diff(range(x, na.rm = TRUE)) else NA_real_
      full <- tapply(d$time, d$token, span)
      kept <- tapply(out$time, out$token, span)
      prop <- kept[names(kept) %in% names(full)] / full[names(kept)]
      prop <- prop[is.finite(prop)]
      if (length(prop) >= 4) {
        med <- stats::median(prop)
        odd <- names(prop)[prop < 0.5 * med]
        if (length(odd) > 0) {
          notes <- c(notes, sprintf(paste("%d token(s) are voiced across a much shorter span than",
                                          "the rest (%s), so their percentage positions are not",
                                          "comparable with the others. Worth a look in F0 Correction."),
                                    length(odd),
                                    paste(utils::head(odd, 3), collapse = ", ")))
        }
      }
    }
    dropped <- character(0)

    # Mark unvoiced stretches on the NATIVE frames of the region, before any
    # resampling: the Praat rule fills a point from its nearer frame, so a
    # short dropout can leave no NA in the resampled grid even though voicing
    # really was interrupted. resample_f0_equal() carries these token-constant
    # columns through, so the flags describe the voicing either way.
    out <- tryCatch(flag_f0_gaps(out), error = function(e) out)

    if (identical(input$fp_sampling_mode, "equal") && nrow(out) > 0) {
      n <- suppressWarnings(as.integer(input$fp_sampling_n))
      if (is.na(n) || n < 2) {
        notes <- c(notes, "Points per token must be at least 2, so the export keeps the native frame times.")
      } else {
        meth <- input$fp_sampling_method
        if (is.null(meth) || !meth %in% c("linear", "nearest")) meth <- "linear"
        rs <- tryCatch(resample_f0_equal(out, n = n, method = meth),
                       error = function(e) NULL)
        if (is.null(rs)) {
          notes <- c(notes, "Could not resample, so the export keeps the native frame times.")
        } else {
          out <- rs
          dropped <- attr(out, "dropped_columns")
          if (is.null(dropped)) dropped <- character(0)
        }
      }
    }
    n_gap <- if ("has_gap" %in% names(out)) {
      length(unique(out$token[out$has_gap %in% TRUE]))
    } else 0L
    if (n_gap > 0) {
      notes <- c(notes, sprintf(paste("%d token(s) had an unvoiced gap inside the",
                                      "measured region (marked has_gap in the download).",
                                      "Resampling may fill some of those points from",
                                      "the nearer frame, so the flag can outlive the NA."),
                                n_gap))
    }
    if (n_edited > 0) {
      notes <- c(notes, sprintf("Using corrected f0 for %d edited token(s).", n_edited))
    }
    if (n_discarded > 0) {
      notes <- c(notes, sprintf("%d token(s) discarded in F0 Correction are excluded.",
                                n_discarded))
    }
    list(data = out,
         region = if (is.null(input$fp_region)) "voiced" else input$fp_region,
         sampling = input$fp_sampling_mode,
         notes = notes, dropped = dropped)
  })

  # One-line description of what the Download button will write.
  output$fp_export_summary <- renderUI({
    res <- fp_export_data()
    d   <- fp_f0_data()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    if (is.null(res)) {
      return(tags$div(style = "color: #8a6d00; font-size: 0.75rem; font-style: italic; margin-top: 6px;",
        "Finish choosing the region (tier / labels) to build the export."))
    }
    region_txt <- switch(res$region,
                         all      = "the whole file",   # legacy value, no UI path
                         voiced   = "each whole token (voiced region)",
                         interval = sprintf("the %s intervals of tier '%s'",
                                            switch(input$fp_region_mode,
                                                   vowel = "vowel", rhyme = "rhyme",
                                                   labels = "selected", "selected"),
                                            input$fp_region_tier))
    sampling_txt <- if (identical(res$sampling, "equal") &&
                        "time_prop" %in% names(res$data)) {
      sprintf("%d equidistant points", length(unique(res$data$time_prop)))
    } else "native frame times"
    cap <- sprintf("Export: %s across %s. %s rows, %d token(s).",
                   sampling_txt, region_txt,
                   format(nrow(res$data), big.mark = ","),
                   length(unique(res$data$token)))
    if (length(res$dropped) > 0) {
      cap <- paste0(cap, sprintf(" Resampling dropped frame-level column(s): %s.",
                                 paste(res$dropped, collapse = ", ")))
    }
    tagList(
      tags$div(style = "color: #666; font-size: 0.75rem; font-style: italic; margin-top: 6px;", cap),
      if (length(res$notes) > 0)
        tags$div(style = "color: #8a6d00; font-size: 0.75rem; margin-top: 4px;",
                 paste(res$notes, collapse = " "))
    )
  })

  # ---- CSV column auto-detection + auto-load ----
  # When the user picks a CSV in "Upload existing f0 CSV" mode, we read it
  # once into a reactive cache, surface three column-pickers (token / time / f0)
  # with auto-detected defaults, and load fp_f0_data automatically whenever
  # the picks are valid.
  fp_f0_csv_raw <- reactive({
    req(input$fp_extract_mode == "csv", input$fp_f0_upload_file)
    tryCatch(
      utils::read.csv(input$fp_f0_upload_file$datapath,
                      stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
  })

  output$fp_f0_csv_col_pickers <- renderUI({
    # Always render the three selects when the CSV source is active so users
    # can see the full setup up front. Before a file is uploaded they show a
    # placeholder option; once a file is uploaded they populate with auto-
    # detected column names from the CSV header.
    if (!isTRUE(input$fp_extract_mode == "csv")) return(NULL)
    # Only call fp_f0_csv_raw() once a file is actually picked, since the
    # reactive uses req() and would silentStop this renderUI otherwise.
    df <- if (!is.null(input$fp_f0_upload_file)) fp_f0_csv_raw() else NULL
    has_csv <- !is.null(df) && ncol(df) > 0

    # Inline note attached to the token picker (the only column whose values
    # need to match audio basenames).
    token_match_note <- tags$div(
      style = "color: #888; font-size: 0.75rem; margin-top: -8px; margin-bottom: 6px; font-style: italic;",
      "Values must match the .wav basenames uploaded in Start."
    )

    if (!has_csv) {
      placeholder <- c("Upload a CSV first" = "")
      return(tagList(
        selectInput("fp_f0_col_token", "Token / filename column:",
                    choices = placeholder, selected = "", selectize = FALSE),
        token_match_note,
        selectInput("fp_f0_col_time", "Time column:",
                    choices = placeholder, selected = "", selectize = FALSE),
        selectInput("fp_f0_col_f0", "f0 column:",
                    choices = placeholder, selected = "", selectize = FALSE)
      ))
    }

    cols <- names(df)
    # Case-insensitive auto-match against likely column names.
    auto_match <- function(candidates) {
      hit <- cols[tolower(cols) %in% tolower(candidates)]
      if (length(hit) > 0) hit[1] else cols[1]
    }
    tok_default  <- auto_match(c("token", "wav", "filename", "basename",
                                 "audio", "file", "token_id"))
    time_default <- auto_match(c("time", "t", "timestamp", "time_s", "time_ms"))
    f0_default   <- auto_match(c("f0", "f0_hz", "f0_Hz", "pitch",
                                 "frequency", "freq"))
    tagList(
      selectInput("fp_f0_col_token", "Token / filename column:",
                  choices = cols, selected = tok_default, selectize = FALSE),
      token_match_note,
      selectInput("fp_f0_col_time", "Time column:",
                  choices = cols, selected = time_default, selectize = FALSE),
      selectInput("fp_f0_col_f0", "f0 column:",
                  choices = cols, selected = f0_default, selectize = FALSE)
    )
  })
  outputOptions(output, "fp_f0_csv_col_pickers", suspendWhenHidden = FALSE)

  # Auto-load fp_f0_data whenever the CSV + column picks are valid and audio
  # is uploaded.
  observe({
    req(input$fp_extract_mode == "csv")
    df <- fp_f0_csv_raw()
    req(df)
    tcol <- input$fp_f0_col_token
    scol <- input$fp_f0_col_time
    fcol <- input$fp_f0_col_f0
    req(tcol, scol, fcol)
    if (!all(c(tcol, scol, fcol) %in% names(df))) return()

    audio <- fp_audio_data()
    if (is.null(audio) || nrow(audio) == 0) return()  # wait for .wav uploads

    out <- data.frame(
      token = as.character(df[[tcol]]),
      time  = suppressWarnings(as.numeric(df[[scol]])),
      f0    = suppressWarnings(as.numeric(df[[fcol]])),
      stringsAsFactors = FALSE
    )
    # Carry an intensity column through if the CSV has one (auto-detected by
    # name) so the Inspect tab's low-intensity check can use it downstream.
    icol <- names(df)[tolower(names(df)) %in%
                        c("intensity", "intensity_db", "rms", "energy")]
    if (length(icol) > 0) {
      out$intensity <- suppressWarnings(as.numeric(df[[icol[1]]]))
    }
    # Resume-from-previous-session metadata. If the uploaded CSV is a
    # shinytone all_correctedf0.csv from an earlier session, it carries
    # `f0_corrected` and `edited` columns (and, since whole-token discards
    # were added, `token_dropped`). Keep them so the Correction tab can
    # restore previous edits and discards, show ghost markers, and mark the
    # tokens with ✎ / ✗.
    if ("f0_corrected" %in% names(df)) {
      out$f0_corrected <- suppressWarnings(as.numeric(df$f0_corrected))
    }
    if ("edited" %in% names(df)) {
      out$edited <- suppressWarnings(as.logical(df$edited))
    }
    if ("token_dropped" %in% names(df)) {
      out$token_dropped <- suppressWarnings(as.logical(df$token_dropped))
    }
    have_wav <- audio$basename[!is.na(audio$wav_path)]
    keep <- out$token %in% have_wav
    n_unmatched <- length(setdiff(unique(out$token), have_wav))
    out <- out[keep, , drop = FALSE]
    if (nrow(out) == 0) {
      showNotification(
        "No CSV tokens match any uploaded .wav basename. Check the Start tab.",
        type = "warning", duration = 5, id = "fp_csv_load"
      )
      return()
    }
    # A re-uploaded F0 Data Export is recognisable by its computed columns.
    # Correcting it works (times are absolute, so the points still line up with
    # the waveform), but it corrects the resampled points rather than the frames
    # they came from, and Praat candidates are gone. The intended order is
    # correct first, then export, since the export follows corrections.
    if (all(c("point", "time_prop") %in% names(df))) {
      showNotification(
        tags$div(
          tags$strong("This looks like an F0 Data Export (resampled points)."),
          tags$div(style = "margin-top:4px;",
            "You can still correct it, and the points line up with the waveform, ",
            "but you would be editing the resampled points rather than the pitch ",
            "frames they came from, and Praat pitch candidates are not available."),
          tags$div(style = "margin-top:4px;",
            "For frame-level correction, load the original audio instead: correct ",
            "first, then export. The export always follows your corrections.")),
        type = "warning", duration = 14, id = "fp_csv_is_export")
    }

    # Only fire the success toast when fp_f0_data is actually changing.
    cur <- isolate(fp_f0_data())
    same <- !is.null(cur) && identical(cur, out)
    fp_f0_data(out)
    if (!is.null(fp_pitch_candidates)) fp_pitch_candidates(list())
    if (!same) {
      msg <- sprintf("Loaded f0 for %d token(s) from CSV. ✅",
                     length(unique(out$token)))
      if (n_unmatched > 0) {
        msg <- paste0(msg, sprintf(" (%d CSV token(s) had no matching .wav and were skipped.)",
                                   n_unmatched))
      }
      showNotification(msg, type = "message", duration = 4, id = "fp_csv_load")
    }
  })

  # ---- Run extraction (or load CSV) ----
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

      # Drop files too short to yield a single pitch frame at the chosen floor
      # (flagged red in the Start preview), plus any whose .wav header could not
      # be read. These produce no f0/intensity, so we skip them up front.
      min_dur <- min_audio_dur(f0_min)
      drop    <- is.na(wavs$dur) | wavs$dur < min_dur
      if (any(drop)) {
        dropped <- wavs$basename[drop]
        wavs <- wavs[!drop, , drop = FALSE]
        showNotification(
          sprintf("Skipped %d file(s) too short for f0 (< %.3f s) or unreadable: %s%s",
                  length(dropped), min_dur,
                  paste(utils::head(dropped, 5), collapse = ", "),
                  if (length(dropped) > 5) sprintf(", and %d more", length(dropped) - 5) else ""),
          type = "warning", duration = 7)
      }
      if (nrow(wavs) == 0) {
        showNotification("All .wav files are too short for f0 extraction.",
                         type = "warning", duration = 5)
        return()
      }

      # Wipe any previous extraction so the user sees a clean transition,
      # and announce that work has started (the progress bar can be subtle).
      fp_f0_data(NULL)
      showNotification(sprintf("Running wrassp extraction on %d file(s)…", nrow(wavs)),
                       type = "message", duration = 3, id = "fp_extract_starting")

      withProgress(message = "Extracting f0 (wrassp)", value = 0, {
        results <- list()
        for (i in seq_len(nrow(wavs))) {
          b <- wavs$basename[i]
          incProgress(1 / nrow(wavs),
                      detail = sprintf("%d / %d  ·  %s", i, nrow(wavs), b))
          one <- extract_wrassp_one(wavs$wav_path[i], b, f0_min, f0_max, step_ms)
          if (!is.null(one)) results[[b]] <- one
        }
        if (length(results) == 0) {
          showNotification("Extraction failed for all files.",
                           type = "error", duration = 6)
          return()
        }
        fp_f0_data(attach_landmarks_if_any(do.call(rbind, lapply(results, `[[`, "df"))))
        if (!is.null(fp_pitch_candidates)) fp_pitch_candidates(list())  # wrassp has no candidates
        n_fail <- nrow(wavs) - length(results)
        msg <- if (n_fail == 0) {
          sprintf("Extracted f0 for all %d tokens. ✅", length(results))
        } else {
          sprintf("Extracted f0 for %d / %d tokens (%d failed).",
                  length(results), nrow(wavs), n_fail)
        }
        showNotification(msg,
                         type = if (n_fail == 0) "message" else "warning",
                         duration = 5)
      })
    } else {
      # Praat mode
      has_praat <- audio[!is.na(audio$pitch_path) | !is.na(audio$pitchtier_path), , drop = FALSE]
      if (nrow(has_praat) == 0) {
        showNotification("No .Pitch / .PitchTier files in the upload.",
                         type = "warning", duration = 4)
        return()
      }

      # Partial coverage: Praat mode can only yield f0 for recordings that
      # have a pitch file, so the rest are dropped from the extraction
      # entirely. Say so before it happens, with both ways out.
      cov <- praat_coverage(audio)
      if (cov$n_missing > 0) {
        showNotification(
          tags$div(
            tags$strong(sprintf("Skipping %d of %d recording(s): no matching .Pitch / .PitchTier.",
                                cov$n_missing, cov$n_wav)),
            tags$div(style = "margin-top:4px;",
              nows(tags$code(paste(utils::head(cov$missing, 3), collapse = ", ")),
                   if (cov$n_missing > 3) sprintf(" and %d more", cov$n_missing - 3),
                   " will have no f0 at all.")),
            tags$div(style = "margin-top:6px;",
              "For the whole set, either switch to ",
              tags$strong("Extract from .wav (wrassp)"),
              " above, or re-run the Praat script over every .wav and upload ",
              "the new ", tags$code(".Pitch"), " files: ",
              nows(tags$a(href = "#", style = "font-weight:600; text-decoration:underline;",
                          onclick = paste0("Shiny.setInputValue('about_nav_target', ",
                                           "'F0 Processing|Measure f0 with Praat', ",
                                           "{priority:'event'}); return false;"),
                          "open Measure f0 with Praat"), "."))),
          type = "warning", duration = 20, id = "fp_praat_partial")
      }

      # Wipe any previous extraction and announce start.
      fp_f0_data(NULL)
      showNotification(sprintf("Parsing %d Praat pitch file(s)…", nrow(has_praat)),
                       type = "message", duration = 3, id = "fp_extract_starting")

      withProgress(message = "Parsing Praat pitch files", value = 0, {
        results <- list()
        for (i in seq_len(nrow(has_praat))) {
          b <- has_praat$basename[i]
          incProgress(1 / nrow(has_praat),
                      detail = sprintf("%d / %d  ·  %s", i, nrow(has_praat), b))
          one <- extract_praat_one(has_praat$pitch_path[i],
                                   has_praat$pitchtier_path[i], b)
          if (!is.null(one)) results[[b]] <- one
        }
        if (length(results) == 0) {
          # Re-read the first file outside the silent tryCatch so we can show the
          # actual reason (binary-format Praat files are the usual culprit; rPraat
          # only reads text / short-text files).
          first  <- has_praat[1, , drop = FALSE]
          reason <- suppressWarnings(tryCatch({
            if (!is.na(first$pitch_path))          rPraat::pitch.read(first$pitch_path)
            else if (!is.na(first$pitchtier_path)) rPraat::pt.read(first$pitchtier_path)
            "the file parsed but held no usable f0 frames"
          }, error = function(e) conditionMessage(e)))
          wav_present <- any(!is.na(audio$wav_path))
          showNotification(
            tags$div(
              tags$strong("Could not parse any .Pitch / .PitchTier file."),
              tags$div(style = "color:#666; font-size:0.82rem; margin:4px 0;",
                       "Reason: ", tags$code(substr(reason, 1, 150))),
              "Praat pitch files must be saved as ", tags$strong("text"),
              " (in Praat: ", tags$em("Save as text file…"),
              "); binary files cannot be read.",
              if (wav_present) tags$div(style = "margin-top:4px;",
                "Your upload includes .wav files, so you can instead choose ",
                tags$strong("Extract from .wav (wrassp)"), " above.")
            ),
            type = "error", duration = 14)
          return()
        }
        fp_f0_data(attach_landmarks_if_any(do.call(rbind, lapply(results, `[[`, "df"))))
        if (!is.null(fp_pitch_candidates)) {
          # The in-app grid is always the native one, so the .Pitch frames
          # line up one-to-one and the Correction tab's candidate picker
          # stays usable whatever the export settings are.
          cands <- list()
          for (b in names(results)) {
            if (!is.null(results[[b]]$candidates)) cands[[b]] <- results[[b]]$candidates
          }
          fp_pitch_candidates(cands)
        }
        n_fail <- nrow(has_praat) - length(results)
        msg <- if (n_fail == 0) {
          sprintf("Parsed f0 for all %d tokens. ✅", length(results))
        } else {
          sprintf("Parsed f0 for %d / %d tokens (%d failed).",
                  length(results), nrow(has_praat), n_fail)
        }
        showNotification(msg,
                         type = if (n_fail == 0) "message" else "warning",
                         duration = 5)
      })
    }
  })

  # ---- Helpers: build the join key + join metadata into a long-format f0 df ----
  # Returns NULL if no metadata is loaded.
  make_token_key <- function(x, strip_ext = TRUE) {
    k <- as.character(x)
    if (isTRUE(strip_ext)) k <- tools::file_path_sans_ext(k)
    tolower(trimws(k))
  }
  # Diagnostics from a single join attempt — used both by the banner and the
  # download handler so the two stay in sync.
  metadata_join <- function(df, md, keycol, strip_ext) {
    if (is.null(md) || is.null(keycol) || !(keycol %in% names(md))) {
      return(list(joined = df, matched = NULL, unmatched_tokens = character(0),
                  unmatched_meta = character(0)))
    }
    md_keys <- make_token_key(md[[keycol]], strip_ext)
    f0_keys <- unique(make_token_key(df$token, strip_ext))
    matched_tokens   <- intersect(f0_keys, md_keys)
    unmatched_tokens <- setdiff(f0_keys, md_keys)
    unmatched_meta   <- setdiff(md_keys, f0_keys)

    md2 <- md
    md2$.token_key <- md_keys
    df2 <- df
    df2$.token_key <- make_token_key(df2$token, strip_ext)
    # Avoid collisions: rename any metadata columns that clash with f0 columns
    clash <- intersect(setdiff(names(md2), ".token_key"),
                       setdiff(names(df2), ".token_key"))
    if (length(clash)) {
      names(md2)[match(clash, names(md2))] <- paste0(clash, ".meta")
    }
    joined <- merge(df2, md2, by = ".token_key", all.x = TRUE, sort = FALSE)
    joined$.token_key <- NULL
    # Preserve original f0 row order (merge can reshuffle even with sort=FALSE)
    joined <- joined[order(joined$token,
                           if ("time" %in% names(joined)) joined$time else seq_len(nrow(joined))), , drop = FALSE]
    rownames(joined) <- NULL
    list(joined = joined,
         matched = length(matched_tokens),
         unmatched_tokens = unmatched_tokens,
         unmatched_meta   = unmatched_meta)
  }

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
      uiOutput("fp_meta_match_summary"),
      tags$h4("Extracted f0 contours"),
      tags$p(style = "color: #777; font-size: 0.85rem;",
        "Click a token name in the legend to hide/show its contour. ",
        "Use the camera icon for a high-resolution PNG."),
      plotly::plotlyOutput("fp_f0_overview", height = "450px"),
      tags$h4(style = "margin-top: 20px;", "Per-token summary"),
      DT::dataTableOutput("fp_f0_summary_table"),
      tags$h4(style = "margin-top: 20px;", "Export preview"),
      uiOutput("fp_export_preview_note"),
      DT::dataTableOutput("fp_export_preview")
    )
  })

  # ---- Match-summary banner (appears above the plot when metadata is loaded) ----
  output$fp_meta_match_summary <- renderUI({
    md <- if (!is.null(fp_metadata)) fp_metadata() else NULL
    df <- fp_f0_data()
    if (is.null(md) || is.null(df) || nrow(df) == 0) return(NULL)
    keycol <- active_keycol()
    req(keycol)
    res <- metadata_join(df, md, keycol,
                         strip_ext = isTRUE(input$fp_meta_strip_ext))
    n_f0  <- length(unique(df$token))
    n_md  <- nrow(md)
    n_ok  <- if (is.null(res$matched)) 0 else res$matched
    n_f0_only <- length(res$unmatched_tokens)
    n_md_only <- length(res$unmatched_meta)
    # Pick colour + message based on match completeness
    if (n_ok == n_f0 && n_ok == n_md) {
      bg <- "#e8f5f0"; bord <- "#78c2ad"; col <- "#2a7a5a"; icon_txt <- "✅"
      msg <- sprintf("Metadata: all %d tokens matched.", n_ok)
    } else if (n_ok == 0) {
      bg <- "#fde8e8"; bord <- "#d9534f"; col <- "#a02622"; icon_txt <- "⚠"
      msg <- sprintf("Metadata: no rows matched. Check the filename column (currently \"%s\") and the strip-extension option.", keycol)
    } else {
      bg <- "#fff8e1"; bord <- "#e0a800"; col <- "#8a6d00"; icon_txt <- "ℹ"
      msg <- sprintf("Metadata: %d of %d tokens matched.", n_ok, n_f0)
    }
    extra <- tagList()
    if (n_f0_only > 0) {
      sample <- paste(utils::head(res$unmatched_tokens, 5), collapse = ", ")
      if (n_f0_only > 5) sample <- paste0(sample, ", ...")
      extra <- tagAppendChildren(extra,
        tags$li(sprintf("%d token(s) have no metadata row: %s",
                        n_f0_only, sample)))
    }
    if (n_md_only > 0) {
      sample <- paste(utils::head(res$unmatched_meta, 5), collapse = ", ")
      if (n_md_only > 5) sample <- paste0(sample, ", ...")
      extra <- tagAppendChildren(extra,
        tags$li(sprintf("%d metadata row(s) have no audio: %s",
                        n_md_only, sample)))
    }
    tags$div(
      style = sprintf("background:%s; border-left:4px solid %s; color:%s; padding:10px 14px; margin-bottom:12px; border-radius:4px; font-size:0.88rem;",
                      bg, bord, col),
      tags$div(style = "font-weight: 600;", icon_txt, " ", msg),
      if (length(extra) > 0) tags$ul(style = "margin: 6px 0 0 0; padding-left: 20px;", extra)
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
    # Append mean intensity (dB) per token when an intensity column is present.
    if ("intensity" %in% names(df)) {
      mi <- df |>
        dplyr::group_by(token) |>
        dplyr::summarise(
          mean_intensity = round(mean(intensity, na.rm = TRUE), 1),
          .groups = "drop"
        )
      summary_df <- dplyr::left_join(summary_df, mi, by = "token")
    }

    # Voiced span per token, on the same definition the "Whole token" region
    # uses, plus its share of the recording. This is where a token that the
    # tracker only partly voiced shows up: with equidistant sampling its
    # percentage positions are squeezed into voiced_s, so a low voiced_prop
    # means its 50% is not the same position as the other tokens' 50%.
    vs <- tryCatch({
      tv <- trim_to_voiced(df)
      if (nrow(tv) == 0) NULL else
        tv |>
          dplyr::group_by(token) |>
          dplyr::summarise(voiced_s = round(max(time) - min(time), 3),
                           .groups = "drop")
    }, error = function(e) NULL)
    if (!is.null(vs)) {
      summary_df <- summary_df |>
        dplyr::left_join(vs, by = "token") |>
        dplyr::mutate(voiced_prop = ifelse(duration_s > 0,
                                           round(voiced_s / duration_s, 2), NA_real_))
    }

    dt <- DT::datatable(
      summary_df,
      rownames = FALSE, filter = "top",
      options = list(
        pageLength = 25, autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    )
    # Shade the tokens voiced across far less of their recording than the
    # rest, so they can be spotted without reading every row.
    if ("voiced_prop" %in% names(summary_df)) {
      vp  <- summary_df$voiced_prop
      med <- suppressWarnings(stats::median(vp, na.rm = TRUE))
      if (is.finite(med) && med > 0 && sum(!is.na(vp)) >= 4) {
        dt <- DT::formatStyle(dt, "voiced_prop",
                              backgroundColor = DT::styleInterval(0.5 * med,
                                                                  c("#fdecea", "")))
      }
    }
    dt
  })

  # ---- Export preview: exactly what the Download button will write ----
  # Reads fp_export_data(), so region, sampling and any corrections are all
  # reflected; the metadata join is applied here too, as at download time.
  fp_export_preview_data <- reactive({
    res <- fp_export_data()
    if (is.null(res) || nrow(res$data) == 0) return(NULL)
    out <- res$data
    md  <- if (!is.null(fp_metadata)) fp_metadata() else NULL
    keycol <- active_keycol()
    if (!is.null(md) && nrow(md) > 0 && !is.null(keycol)) {
      out <- tryCatch(metadata_join(out, md, keycol,
                                    strip_ext = isTRUE(input$fp_meta_strip_ext))$joined,
                      error = function(e) out)
    }
    out
  })

  output$fp_export_preview_note <- renderUI({
    df <- fp_export_preview_data()
    if (is.null(df)) {
      return(tags$div(style = "color: #888; font-size: 0.85rem; font-style: italic;",
        "Finish choosing the export options to preview the file."))
    }
    tags$p(style = "color: #777; font-size: 0.85rem;",
      sprintf("The first rows of the file the Download button will write: %s rows x %d columns, %d token(s).",
              format(nrow(df), big.mark = ","), ncol(df), length(unique(df$token))))
  })

  output$fp_export_preview <- DT::renderDataTable({
    df <- fp_export_preview_data()
    req(df)
    num <- names(df)[vapply(df, is.numeric, logical(1))]
    dt <- DT::datatable(utils::head(df, 50), rownames = FALSE,
                        options = list(pageLength = 10, scrollX = TRUE,
                                       dom = "tp", autoWidth = TRUE))
    if (length(num)) dt <- DT::formatSignif(dt, num, 5)
    dt
  })

  # ---- Download f0 CSV (joined with metadata if available) ----
  output$fp_extract_download <- downloadHandler(
    filename = function() {
      paste0(input$fp_extract_filename, ".csv")
    },
    content = function(file) {
      if (is.null(fp_f0_data()) || nrow(fp_f0_data()) == 0) {
        showNotification(
          "No f0 data yet. Click Run extraction first.",
          type = "warning", duration = 5
        )
        # Write a one-line placeholder so the browser doesn't hang on the request
        writeLines("# Shinytone: no f0 data. Run extraction first.", file)
        return()
      }
      # Region + sampling are applied here, on the way out; the in-app grid
      # (and so F0 Correction) is untouched. Defaults reproduce the full
      # native grid exactly.
      res <- fp_export_data()
      if (is.null(res) || nrow(res$data) == 0) {
        showNotification(
          "The chosen region is empty. Check the tier, mode, and labels.",
          type = "warning", duration = 6)
        writeLines("# Shinytone: the chosen export region is empty.", file)
        return()
      }
      df <- res$data
      md <- if (!is.null(fp_metadata)) fp_metadata() else NULL
      keycol <- active_keycol()
      out <- df
      msg <- "f0 data saved as %s"
      if (!is.null(md) && nrow(md) > 0 && !is.null(keycol)) {
        res <- metadata_join(df, md, keycol,
                             strip_ext = isTRUE(input$fp_meta_strip_ext))
        out <- res$joined
        n_ok <- if (is.null(res$matched)) 0 else res$matched
        n_f0 <- length(unique(df$token))
        msg <- sprintf("f0 + metadata saved as %%s (%d / %d tokens matched).",
                       n_ok, n_f0)
      }
      fname <- paste0(input$fp_extract_filename, ".csv")
      write.csv(out, file, row.names = FALSE)
      showNotification(sprintf(msg, fname), type = "message", duration = 5)
    }
  )
}
