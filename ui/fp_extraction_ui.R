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
                               "Use uploaded .Pitch / .PitchTier (Praat)" = "praat"),
                   selected = "wrassp"),
      conditionalPanel("input.fp_extract_mode == 'wrassp'",
        tags$hr(),
        numericInput("fp_f0_min",   "Min f0 (Hz)",   value = 75,  min = 30,  max = 300),
        numericInput("fp_f0_max",   "Max f0 (Hz)",   value = 600, min = 200, max = 1000),
        numericInput("fp_window_ms","Frame step (ms)", value = 5,  min = 1,   max = 50)
      ),
      tags$hr(),
      actionButton("fp_extract_run", "Run extraction", icon = icon("play")),
      tags$hr(),
      # ---- Metadata (optional) ----
      h5("Metadata ", tags$small(style = "color: #888; font-weight: normal;", "(optional)")),
      tags$p(style = "color: #777; font-size: 0.8rem; margin-bottom: 6px;",
        "Upload a CSV with one row per audio file. It will be joined to the f0 output by filename."),
      fileInput("fp_meta_file", NULL,
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                buttonLabel = "Choose CSV",
                placeholder = "No file selected"),
      conditionalPanel("output.fp_have_metadata === 'yes'",
        uiOutput("fp_meta_keycol_ui"),
        checkboxInput("fp_meta_strip_ext",
                      "Strip file extensions when matching",
                      value = TRUE)
      ),
      tags$hr(),
      conditionalPanel("output.fp_have_f0 === 'yes'",
        h5("Download"),
        textInput("fp_extract_filename", "Filename:", value = "extracted_f0"),
        downloadButton("fp_extract_download", "Download f0 (CSV)")
      )
    )
  })

  # ---- Read uploaded metadata CSV into fp_metadata ----
  observeEvent(input$fp_meta_file, {
    req(input$fp_meta_file)
    df <- tryCatch(
      utils::read.csv(input$fp_meta_file$datapath,
                      stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || ncol(df) == 0) {
      showNotification("Could not read metadata CSV.",
                       type = "error", duration = 5)
      return()
    }
    if (!is.null(fp_metadata)) fp_metadata(df)
    showNotification(sprintf("Metadata loaded: %d row(s), %d column(s).",
                             nrow(df), ncol(df)),
                     type = "message", duration = 3)
  })

  # Flag for conditionalPanel: do we have metadata loaded?
  output$fp_have_metadata <- reactive({
    md <- if (!is.null(fp_metadata)) fp_metadata() else NULL
    if (!is.null(md) && nrow(md) > 0) "yes" else "no"
  })
  outputOptions(output, "fp_have_metadata", suspendWhenHidden = FALSE)

  # Column selector — auto-populated from metadata columns; defaults to a column
  # that looks like a filename (basename/filename/file/wav).
  output$fp_meta_keycol_ui <- renderUI({
    md <- if (!is.null(fp_metadata)) fp_metadata() else NULL
    req(md, ncol(md) > 0)
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
            "Choose this if you've already extracted pitch in Praat with custom settings."),
          tags$li(tags$strong("Metadata (optional):"),
            " upload a CSV with one row per audio file (e.g., speaker, tone, word). ",
            "It will be joined to the f0 output by filename, so the download is ready for F0 Analysis.")
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
        fp_f0_data(do.call(rbind, lapply(results, `[[`, "df")))
        if (!is.null(fp_pitch_candidates)) fp_pitch_candidates(list())  # wrassp has no candidates
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
        fp_f0_data(do.call(rbind, lapply(results, `[[`, "df")))
        if (!is.null(fp_pitch_candidates)) {
          cands <- list()
          for (b in names(results)) {
            if (!is.null(results[[b]]$candidates)) cands[[b]] <- results[[b]]$candidates
          }
          fp_pitch_candidates(cands)
        }
        showNotification(sprintf("Parsed f0 for %d / %d tokens.",
                                 length(results), nrow(has_praat)),
                         type = "message", duration = 4)
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
    keycol <- input$fp_meta_keycol
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
      req(fp_f0_data())
      df <- fp_f0_data()
      md <- if (!is.null(fp_metadata)) fp_metadata() else NULL
      out <- df
      msg <- "f0 data saved as %s"
      if (!is.null(md) && nrow(md) > 0 && !is.null(input$fp_meta_keycol)) {
        res <- metadata_join(df, md, input$fp_meta_keycol,
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
