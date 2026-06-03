#######################################
# Simplified UI for loading a CSV file
#######################################

start_ui <- function(input, output, session, dataset, raw_dataset, attached_metadata) {
# Render UI for uploading a CSV file
output$ui_fileUpload <- renderUI({
  tagList(
    fileInput("uploadfile", "Choose a CSV File",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    tags$div(style = "margin-top: -8px;",
      checkboxInput("convert_to_factor",
                    "String variables as factors (recommended)", FALSE)
    ),
    tags$hr(style = "margin: 10px 0;"),
    tags$details(
      tags$summary(style = "cursor: pointer; font-weight: 600; color: #2c5f4f; font-size: 0.9rem;",
                   icon("tags"), " Attach metadata (optional)"),
      tags$div(style = "padding-top: 8px;",
        tags$p(style = "font-size: 0.8rem; color: #777; margin: 0 0 8px 0;",
          "Already have an f0 dataframe without speaker / tone / item columns (e.g. straight from ",
          "Praat)? Add them here after uploading, and they flow to every F0 Analysis tab."),
        radioButtons("meta_source", NULL,
          choices = c("Off" = "none",
                      "Derive from filename" = "derive",
                      "Upload metadata CSV"  = "csv"),
          selected = "none"),
        conditionalPanel("input.meta_source == 'csv'",
          fileInput("meta_file", "Metadata CSV:",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          tags$div(style = "font-size: 0.78rem; color: #888; margin: -6px 0 6px 0;",
            "Both files need a column identifying each recording (usually the audio filename). ",
            "Pick it on each side below; the columns can be named differently, but their values must match.")),
        conditionalPanel("input.meta_source != 'none'",
          uiOutput("meta_key_ui")),
        conditionalPanel("input.meta_source == 'derive'",
          tags$div(style = "font-size: 0.78rem; color: #888; margin: -4px 0 6px 0;",
            "Its values should encode the metadata to split out (e.g. ",
            tags$code("S01_T1_rep1"), " becomes speaker, tone, rep). ",
            tags$a(href = "#", style = "color: #2c5f4f; font-weight: 600;",
              onclick = "Shiny.setInputValue('about_nav_target', 'Data Collection|Filename', {priority:'event'}); return false;",
              "See the filename guide.")),
          textInput("meta_sep", "Separator:", value = "_"),
          uiOutput("meta_status"),
          textInput("meta_colnames", "Column names:",
            placeholder = "e.g. language speaker tone word rep"),
          tags$div(style = "font-size: 0.78rem; color: #888; margin: -6px 0 0 0;",
            "Type names in order, separated by spaces or commas."),
          uiOutput("meta_preview")),
        conditionalPanel("input.meta_source == 'csv'",
          uiOutput("meta_join_ui"),
          tags$div(style = "font-size: 0.78rem; color: #888; margin: -6px 0 8px 0;",
            "It may be called ", tags$code("token"), ", ", tags$code("filename"),
            ", ", tags$code("basename"), ", ... and values may include a ",
            tags$code(".wav"), " extension."),
          checkboxInput("meta_strip_ext", "Strip file extensions when matching", value = TRUE),
          uiOutput("meta_csv_status")),
        conditionalPanel("input.meta_source != 'none'",
          tags$div(style = "margin-top: 6px;",
            actionButton("meta_apply", "Apply metadata", class = "btn btn-primary"))),
        uiOutput("meta_applied")
      )
    )
  )

})

# Render instructional texts
# IMPORTANT: do NOT depend on dataset() here — this renderUI builds the
# "Try with our sample data" actionButton, and re-rendering it would
# destroy + recreate the button, resetting input$try_sample back to 0
# and immediately wiping out the data the user just loaded.
output$instruction_text <- renderUI({
  tagList(
    h2("Welcome!"),
    p("Upload a CSV file to get started. Your file should have column headers and ideally contain
      columns for time, f0, tone category, speaker ID, and token ID. ",
      "Or click ", tags$em("Try with our sample data"),
      " below to load an example."),
    tags$hr(),
    # --- What the data look like (with our sample as a concrete example) ---
    h4("What the data look like"),
    tags$p("Shinytone expects a CSV with at least the following columns ",
           "(names can differ, but the content should be):"),
    tags$ul(
      tags$li(tags$strong("time"), ": time index within each token (e.g., in seconds)"),
      tags$li(tags$strong("f0"),   ": fundamental frequency in Hz"),
      tags$li(tags$strong("tone category"), ": tone label (e.g., T1, T2, ...)"),
      tags$li(tags$strong("speaker"), ": speaker ID (for by-speaker normalisation)"),
      tags$li(tags$strong("token"), ": unique recording / utterance ID")
    ),
    tags$p("Here's what our sample data looks like (first 5 rows):"),
    {
      # Read the bundled sample CSV that lives in www/ alongside the JS
      # handler that fetches it for "Try with our sample data".
      sample_preview <- tryCatch(
        utils::read.csv("www/dc21f0_test.csv",
                        stringsAsFactors = FALSE,
                        check.names = FALSE,
                        nrows = 5),
        error = function(e) NULL
      )
      if (is.null(sample_preview)) {
        tags$div(style = "color: #888; font-style: italic;",
                 "Sample preview unavailable.")
      } else {
        tags$div(style = "overflow-x: auto; margin-bottom: 8px;",
          tags$table(class = "table table-sm table-bordered",
            style = "font-size: 0.82rem; margin-bottom: 0;",
            tags$thead(
              tags$tr(lapply(names(sample_preview), function(c) tags$th(c)))
            ),
            tags$tbody(
              lapply(seq_len(nrow(sample_preview)), function(i)
                tags$tr(lapply(names(sample_preview),
                               function(c) tags$td(as.character(sample_preview[i, c])))))
            )
          )
        )
      }
    },
    tags$p(style = "color: #777; font-size: 0.82rem; font-style: italic;",
      "Source: a subset of the citation-tone recordings from ",
      tags$a(href = "https://doi.org/10.1515/phon-2025-0001",
             target = "_blank", rel = "noopener noreferrer",
             "Xu (2025)"),
      ", ",
      tags$em("Plastic Mandarin tones: regional identity in prosody"),
      ", Phonetica 82(5), 331–362."
    ),
    tags$div(style = "margin: 12px 0;",
      actionButton("try_sample", "Try with our sample data", icon = icon("flask"))
    ),
    tags$hr(),
    h4("What each tab does"),
    tags$ul(
      tags$li(HTML("&#128194; "), tags$strong(style = "color: #78c2ad;", "Start"), " Upload your CSV data and preview its structure."),
      tags$li(HTML("&#128269; "), tags$strong(style = "color: #78c2ad;", "View"), " Browse the full dataset in an interactive table with sorting and search."),
      tags$li(HTML("&#128200; "), tags$strong(style = "color: #78c2ad;", "Normalise"), " Apply f0 normalisation (z-score or semitone) by speaker, and download the result."),
      tags$li(HTML("&#127912; "), tags$strong(style = "color: #78c2ad;", "Visualise"), " Plot f0 contours coloured by tone category, with optional faceting by speaker. Save the plot or view the corresponding R code."),
      tags$li(HTML("&#128270; "), tags$strong(style = "color: #78c2ad;", "Inspect"), " Flag potential f0 outliers and pitch tracking artefacts using by-speaker z-scores and sample-to-sample jump detection."),
      tags$li(HTML("&#128202; "), tags$strong(style = "color: #78c2ad;", "Model: Polynomials"), " Fit Legendre polynomials to each token\u2019s f0 contour. Returns one row per token with coefficients capturing the shape of contour."),
      tags$li(HTML("&#128202; "), tags$strong(style = "color: #78c2ad;", "Model: GCA"), " Run Growth Curve Analysis using a mixed-effects model with orthogonal polynomials and configurable random effects by speaker and item."),
      tags$li(HTML("&#128202; "), tags$strong(style = "color: #78c2ad;", "Model: GAMM"), " Fit Generalised Additive Mixed Models (GAMMs) to f0 contours with configurable basis functions, random effects, and AR1 autocorrelation correction."),
      tags$li(HTML("&#128203; "), tags$strong(style = "color: #78c2ad;", "Summarise"), " Convert tone contours into Chao tone numerals (1\u20135 scale). Compare reference-line and interval-based conversion methods.")
    ),
    tags$hr()
  )
})

# Observe when the file is uploaded and set the dataset name
observe({
  req(input$uploadfile)
  # Extract the name of the uploaded file
  dataset_name <- tools::file_path_sans_ext(basename(input$uploadfile$name))

  # Update the dataset name for consistent use in the app
  updateTextInput(session, "dataset_name", value = dataset_name)
})

# Create a hidden input for dataset_name to be used in other tabs
output$ui_dataset_name <- renderUI({
  div(textInput("dataset_name", NULL, value = ""), style = "display:none;")
})

# Render the dataset preview/structure/summary options
output$ui_datasets <- renderUI({
  tagList(
    radioButtons("dman_preview", "Display:",
                 c("Preview" = "preview",
                   "Structure" = "str",
                   "Summary" = "summary"),
                 selected = "preview",
                 inline = TRUE)
  )
})

# Update title dynamically based on the selected preview option
output$preview_title <- renderText({
  req(input$dman_preview) # Ensure input$dman_preview is available
  switch(input$dman_preview,
         "preview" = "Data Preview",
         "str" = "Data Structure",
         "summary" = "Data Summary")
})

# Display preview (first 10 rows), structure, or summary based on user selection
output$man_example <- renderUI({
  ds <- dataset()
  if (is.null(ds)) {
    return(tags$div(style = "color: #888; font-style: italic; margin: 8px 0;",
                    "No file uploaded yet."))
  }

  if (input$dman_preview == "preview") {
    tagList(
      DT::dataTableOutput("data_preview"),
      textOutput("row_count_info")  # Text for row count info
    )
  } else if (input$dman_preview == "str") {
    verbatimTextOutput("data_structure")
  } else if (input$dman_preview == "summary") {
    verbatimTextOutput("data_summary")
  }
})


output$data_preview <- DT::renderDataTable({
  req(dataset())
  DT::datatable(
    head(dataset(), 10),
    rownames = FALSE,
    options  = list(
      dom         = "t",            # only the table (no search / pagination)
      pageLength  = 10,
      scrollX     = TRUE,           # horizontal scroll if many wide columns
      ordering    = FALSE,
      columnDefs  = list(list(className = "dt-center", targets = "_all"))
    )
  )
})

output$row_count_info <- renderText({
  req(dataset())
  total_rows <- nrow(dataset())
  paste("10 of", total_rows, "rows shown. See View-tab for details.")
})

output$data_structure <- renderPrint({
  req(dataset())
  str(dataset())
})

output$data_summary <- renderPrint({
  req(dataset())
  summary(dataset())
})

# ---- Optional metadata attachment ------------------------------------------
# Lets users who already have an f0 dataframe add speaker/tone/item columns at
# upload time, without the audio-based F0 Extraction path. The result is stored
# in attached_metadata() and joined into dataset() (see server.R).

# Derive-from-column: pick which column to split.
output$meta_key_ui <- renderUI({
  raw <- raw_dataset(); req(raw)
  vars <- names(raw)
  lab <- if (identical(input$meta_source, "csv")) "Filename column in your data:"
         else "Split this column:"
  cur <- isolate(input$meta_key)
  sel <- if (!is.null(cur) && cur %in% vars) cur else guess_var(vars, var_patterns$token)
  selectInput("meta_key", lab, choices = vars, selected = sel)
})

# Split the chosen column's unique values by the separator.
meta_splits <- reactive({
  req(input$meta_source == "derive")
  raw <- raw_dataset(); req(raw)
  key <- input$meta_key; req(key, key %in% names(raw))
  sep <- input$meta_sep; req(!is.null(sep), nzchar(sep))
  keys <- unique(as.character(raw[[key]]))
  list(keys = keys, parts = strsplit(keys, sep, fixed = TRUE))
})

output$meta_status <- renderUI({
  sp <- tryCatch(meta_splits(), error = function(e) NULL)
  if (is.null(sp)) return(NULL)
  n_each <- vapply(sp$parts, length, integer(1))
  n_max <- max(n_each); n_min <- min(n_each)
  if (n_max == n_min) {
    tags$div(style = "color: #2a7a5a; font-size: 0.8rem; margin-bottom: 8px;",
      sprintf("%d segment(s) in all %d unique value(s).", n_max, length(sp$keys)))
  } else {
    tags$div(style = "color: #8a6d00; font-size: 0.8rem; margin-bottom: 8px;",
      sprintf("%d-%d segments across %d unique value(s); shorter ones pad with NA.",
              n_min, n_max, length(sp$keys)))
  }
})

meta_colnames <- reactive({
  sp <- meta_splits()
  n_max <- max(vapply(sp$parts, length, integer(1)))
  raw <- input$meta_colnames
  typed <- if (is.null(raw) || !nzchar(trimws(raw))) character(0)
           else { p <- unlist(strsplit(trimws(raw), "[,[:space:]]+", perl = TRUE)); p[nzchar(p)] }
  if (length(typed) >= n_max) typed[seq_len(n_max)]
  else if (length(typed) == 0) sprintf("column_%d", seq_len(n_max))
  else c(typed, sprintf("column_%d", seq.int(length(typed) + 1, n_max)))
})

# Per-key derived metadata: key column + named segment columns.
meta_derived <- reactive({
  sp <- meta_splits()
  n_max <- max(vapply(sp$parts, length, integer(1)))
  cols <- meta_colnames()
  padded <- lapply(sp$parts, function(x) c(x, rep(NA_character_, n_max - length(x))))
  mat <- do.call(rbind, padded)
  colnames(mat) <- cols
  out <- data.frame(.k = sp$keys, mat, stringsAsFactors = FALSE, check.names = FALSE)
  names(out)[1] <- input$meta_key
  out
})

output$meta_preview <- renderUI({
  df <- tryCatch(meta_derived(), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  sub <- utils::head(df, 3)
  hdr <- tags$tr(lapply(names(sub), function(n)
    tags$th(style = "padding: 2px 6px; border-bottom: 1px solid #ccc; font-size: 0.76rem;", n)))
  rows <- lapply(seq_len(nrow(sub)), function(i)
    tags$tr(lapply(names(sub), function(n)
      tags$td(style = "padding: 2px 6px; font-size: 0.76rem; color: #555;", as.character(sub[i, n])))))
  tags$div(style = "margin-top: 6px; overflow-x: auto;",
    tags$div(style = "font-size: 0.76rem; color: #777; margin-bottom: 3px;",
             sprintf("Preview (first 3 of %d rows):", nrow(df))),
    tags$table(style = "border-collapse: collapse;", hdr, rows))
})

# Upload-CSV path.
meta_csv <- reactive({
  f <- input$meta_file
  if (is.null(f)) return(NULL)
  tryCatch(read.csv(f$datapath, stringsAsFactors = FALSE),
           error = function(e) { showNotification("Could not read metadata CSV.", type = "error"); NULL })
})

# Always render the metadata-CSV column picker (with a placeholder before a
# file is uploaded), so users see both selectors up front.
output$meta_join_ui <- renderUI({
  csv <- meta_csv()
  if (is.null(csv) || ncol(csv) == 0) {
    return(selectInput("meta_join_meta", "Filename column in the metadata CSV:",
                       choices = c("Upload a CSV first" = ""), selected = ""))
  }
  mvars <- names(csv)
  mguess <- mvars[grepl("^(token|filename|file|wav|basename|audio|id)$", mvars, ignore.case = TRUE)]
  msel <- if (length(mguess)) mguess[1] else guess_var(mvars, var_patterns$token)
  selectInput("meta_join_meta", "Filename column in the metadata CSV:",
    choices = mvars, selected = msel)
})

output$meta_csv_status <- renderUI({
  csv <- tryCatch(meta_csv(), error = function(e) NULL)
  if (is.null(csv)) return(NULL)
  tags$div(style = "color: #2a7a5a; font-size: 0.8rem; margin-top: 4px;",
    sprintf("Metadata CSV: %d rows, %d columns.", nrow(csv), ncol(csv)))
})

# Apply / detach.
observeEvent(input$meta_apply, {
  src <- input$meta_source
  if (identical(src, "derive")) {
    df <- tryCatch(meta_derived(), error = function(e) NULL)
    if (is.null(df)) { showNotification("Nothing to derive yet.", type = "warning"); return() }
    attached_metadata(list(key = input$meta_key, data = df))
    showNotification(sprintf("Attached %d metadata column(s) by splitting '%s'.",
                             ncol(df) - 1, input$meta_key), type = "message")
  } else if (identical(src, "csv")) {
    csv <- meta_csv(); raw <- raw_dataset()
    dkey <- input$meta_key; mkey <- input$meta_join_meta
    strip <- isTRUE(input$meta_strip_ext)
    if (is.null(csv) || is.null(raw) || is.null(dkey) || is.null(mkey) ||
        !mkey %in% names(csv) || !dkey %in% names(raw)) {
      showNotification("Pick a metadata CSV and the key columns first.", type = "warning"); return()
    }
    # Normalised-key match (mirrors the F0 Extraction tab): tolerant of case
    # and of .wav-style file extensions on either side. Build a per-data-key
    # metadata table so the downstream join (server.R) is a plain left_join.
    mk_norm <- make_token_key(csv[[mkey]], strip)
    keep    <- !duplicated(mk_norm)
    csv_u   <- csv[keep, , drop = FALSE]; mk_u <- mk_norm[keep]
    dk      <- unique(as.character(raw[[dkey]]))
    idx     <- match(make_token_key(dk, strip), mk_u)
    meta_cols <- setdiff(names(csv_u), mkey)
    res <- data.frame(setNames(list(dk), dkey), stringsAsFactors = FALSE, check.names = FALSE)
    for (cn in meta_cols) res[[cn]] <- csv_u[[cn]][idx]
    attached_metadata(list(key = dkey, data = res))
    n_match <- sum(!is.na(idx))
    showNotification(sprintf("Metadata joined on '%s': %d of %d unique values matched%s.",
                             dkey, n_match, length(dk),
                             if (n_match < length(dk)) " (the rest get NA)" else ""),
                     type = if (n_match == 0) "warning" else "message", duration = 5)
  }
})

observeEvent(input$meta_clear, {
  attached_metadata(NULL); showNotification("Metadata detached.", type = "message")
})

output$meta_applied <- renderUI({
  m <- attached_metadata()
  if (is.null(m) || is.null(m$data)) return(NULL)
  raw <- raw_dataset()
  new_cols <- if (!is.null(raw)) setdiff(names(m$data), names(raw)) else setdiff(names(m$data), m$key)
  tags$div(style = "margin-top: 8px; padding: 6px 8px; background: #eef8f3; border: 1px solid #bfe3d4; border-radius: 4px; font-size: 0.8rem; color: #1f6f4d;",
    tags$strong("Attached: "),
    if (length(new_cols)) paste(new_cols, collapse = ", ") else "(no new columns)",
    tags$div(style = "margin-top: 4px;",
      actionButton("meta_clear", "Detach metadata", class = "btn btn-sm")))
})

}
