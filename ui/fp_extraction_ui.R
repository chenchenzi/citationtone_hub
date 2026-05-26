###############################################
# F0 Processing → F0 Extraction subtab
# Backends:
#   (1) Extract from .wav via wrassp::ksvF0()
#   (2) Parse uploaded Praat .Pitch / .PitchTier via rPraat
# Both populate fp_f0_data with a long-format data frame:
#   token (basename), time (s), f0 (Hz, NA on unvoiced frames).
###############################################

fp_extraction_ui <- function(input, output, session, fp_audio_data, fp_f0_data,
                             fp_pitch_candidates = NULL, fp_metadata = NULL) {

  # ---- Sidebar controls ----
  output$ui_fp_extraction <- renderUI({
    tagList(
      radioButtons("fp_extract_mode", "f0 source:",
                   choices = c("Extract from .wav (wrassp)" = "wrassp",
                               "Use uploaded .Pitch / .PitchTier (Praat)" = "praat",
                               "Upload existing f0 CSV" = "csv"),
                   selected = "wrassp"),
      conditionalPanel("input.fp_extract_mode == 'wrassp'",
        tags$hr(),
        numericInput("fp_f0_min",   "Min f0 (Hz)",   value = 75,  min = 30,  max = 300),
        numericInput("fp_f0_max",   "Max f0 (Hz)",   value = 600, min = 200, max = 1000),
        numericInput("fp_window_ms","Frame step (ms)", value = 5,  min = 1,   max = 50)
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
      h5("Download"),
      textInput("fp_extract_filename", "Enter filename (without extension):", value = "extracted_f0"),
      downloadButton("fp_extract_download", "Download f0 (CSV)"),
      conditionalPanel("output.fp_have_f0 !== 'yes'",
        tags$div(style = "color: #888; font-size: 0.8rem; margin-top: 6px; font-style: italic;",
                 "Run extraction first to generate the file.")
      )
    )
  })

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
        "Upload audio files in the Start tab — segment counts will be detected from their filenames."))
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
      tags$div(
        style = box_style,
        HTML("<strong>F0 Extraction</strong>"),
        tags$p(style = "margin: 6px 0 0 0;",
          HTML("Choose how to obtain f0 contours from your audio files, then click <strong>Run extraction</strong>.")),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(HTML(paste0(
            "<strong>wrassp</strong> runs the ksvF0 algorithm in R, with no external dependencies, ",
            "and deploys cleanly on shinyapps.io. (",
            "<a href='https://cran.r-project.org/package=wrassp' target='_blank'>CRAN</a> &middot; ",
            "<a href='https://github.com/IPS-LMU/wrassp' target='_blank'>GitHub</a>",
            "; algorithm: Sch&auml;fer-Vincent, 1983.)"))),
          tags$li(HTML(paste0(
            "<strong>Praat</strong> uses the .Pitch / .PitchTier files you uploaded alongside ",
            "the .wav files. Choose this if you've already extracted pitch in Praat with custom settings."))),
          tags$li(HTML(paste0(
            "<strong>Upload existing f0 CSV</strong> skips extraction entirely. Useful if you've already ",
            "run wrassp / Praat / any other tool elsewhere and just want to use F0 Correction or the ",
            "metadata join. The CSV needs columns for token / filename, time, and f0 — ",
            "Shinytone auto-detects common column names and lets you remap if needed. ",
            "Token values must match the .wav basenames uploaded in Start."))),
          tags$li(HTML(paste0(
            "<strong>Metadata (optional):</strong> either upload a metadata CSV (one row per audio file) ",
            "or have Shinytone derive metadata by splitting each filename on a separator (default ",
            "<code>_</code>) and naming the segments. Either way, the metadata is joined to the f0 ",
            "output so the download is ready for F0 Analysis.")))
        )
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
    list(
      df = data.frame(token = basename, time = t, f0 = f0,
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
      parsed <- tryCatch(rPraat::pitch.read(pitch_path), error = function(e) NULL)
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
        f0[f0 == 0] <- NA_real_
        return(list(
          df = data.frame(token = basename, time = t, f0 = f0,
                          stringsAsFactors = FALSE),
          candidates = cands_list
        ))
      }
    }
    if (!is.na(pitchtier_path)) {
      parsed <- tryCatch(rPraat::pt.read(pitchtier_path), error = function(e) NULL)
      if (!is.null(parsed)) {
        return(list(
          df = data.frame(token = basename, time = parsed$t, f0 = parsed$f,
                          stringsAsFactors = FALSE),
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
        fp_f0_data(do.call(rbind, lapply(results, `[[`, "df")))
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
          showNotification("Parsing failed for all files.",
                           type = "error", duration = 6)
          return()
        }
        fp_f0_data(do.call(rbind, lapply(results, `[[`, "df")))
        if (!is.null(fp_pitch_candidates)) {
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
      DT::dataTableOutput("fp_f0_summary_table")
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
    DT::datatable(
      summary_df,
      rownames = FALSE, filter = "top",
      options = list(
        pageLength = 25, autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    )
  })

  # ---- Download f0 CSV (joined with metadata if available) ----
  output$fp_extract_download <- downloadHandler(
    filename = function() {
      paste0(input$fp_extract_filename, ".csv")
    },
    content = function(file) {
      df <- fp_f0_data()
      if (is.null(df) || nrow(df) == 0) {
        showNotification(
          "No f0 data yet — click Run extraction first.",
          type = "warning", duration = 5
        )
        # Write a one-line placeholder so the browser doesn't hang on the request
        writeLines("# Shinytone: no f0 data — run extraction first.", file)
        return()
      }
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
