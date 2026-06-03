# # Define server logic required to draw a histogram
# server <- function(input, output) {
#  
#   # Source your external UI components (start_ui and view_ui)
#   source("ui/start_ui.R", local = TRUE)
#   source("ui/view_ui.R", local = TRUE)
#   
#   # Define the dataset variable to be used across the application
#   dataset <- reactiveVal(NULL)
#   
#   
# }

library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(RColorBrewer)
library(lme4)
library(emmeans)
library(mgcv)
library(thematic)
library(gridExtra)
library(plotly)
# Audio / Praat ecosystem (f0 processing tab)
library(tuneR)
library(rPraat)
library(praatpicture)
#library(ragg)

source("ui/start_ui.R")
source("ui/view_ui.R")
source("ui/visualise_ui.R")
source("ui/normalise_ui.R")
source("ui/inspect_ui.R")
source("ui/model_ui.R")
source("ui/gca_ui.R")
source("ui/gamm_ui.R")
source("ui/checklist_ui.R")
source("ui/filename_guide_ui.R")
source("ui/waveform_guide_ui.R")
source("ui/summarise_ui.R")
source("ui/fp_start_ui.R")
source("ui/fp_extraction_ui.R")
source("ui/fp_correction_ui.R")
source("ui/fp_praat_script_ui.R")
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB cap for batch WAV uploads
#options(shiny.useragg = TRUE)

# Enable thematic
thematic::thematic_shiny(font = "auto")
theme_set(theme_bw(base_size = 16))

# Note: shared helpers like guess_var() and var_patterns now live in
# global.R, which Shiny sources into the global environment before any
# session. They are reachable from every ui_*.R module from there.

server <- function(input, output, session) {

  # Reactive dataset storage. Single source of truth: an uploaded CSV.
  # The "Try with our sample data" button populates input$uploadfile via JS
  # (see the load_sample_csv handler), so the sample path goes through the
  # exact same code path as a manual upload.
  # Raw uploaded CSV — the single source of truth before any optional,
  # View-tab metadata is attached.
  raw_dataset <- reactive({
    if (is.null(input$uploadfile)) return(NULL)
    read.csv(input$uploadfile$datapath, stringsAsFactors = FALSE)
  })

  # Optional metadata attached in the Start tab, so users who already have an
  # f0 dataframe (e.g. from Praat) can add speaker/tone/etc. columns without
  # the audio-based F0 Extraction path. Stored as
  #   list(key = <join column>, data = <df with key + new columns>).
  # Reset whenever a new file is uploaded.
  attached_metadata <- reactiveVal(NULL)
  observeEvent(input$uploadfile, { attached_metadata(NULL) }, priority = 100)

  # The dataset every F0 Analysis tab reads: the raw upload left-joined with
  # any attached metadata (adding only columns the raw data lacks).
  dataset <- reactive({
    raw <- raw_dataset()
    if (is.null(raw)) return(NULL)
    m <- attached_metadata()
    if (!is.null(m) && !is.null(m$data) && !is.null(m$key) &&
        m$key %in% names(raw) && m$key %in% names(m$data)) {
      new_cols <- setdiff(names(m$data), names(raw))
      meta     <- m$data[, c(m$key, new_cols), drop = FALSE]
      raw      <- tryCatch(dplyr::left_join(raw, meta, by = m$key),
                           error = function(e) raw)
    }
    if (isTRUE(input$convert_to_factor)) {
      raw <- raw %>% dplyr::mutate(across(where(is.character), as.factor))
    }
    raw
  })

  # When the sample button is clicked, fire JS that fetches the bundled
  # sample file and injects it into the #uploadfile <input>. This triggers
  # Shiny's normal fileInput binding, which uploads + sets input$uploadfile
  # just like a manual Browse... action.
  sample_just_clicked <- reactiveVal(FALSE)
  observeEvent(input$try_sample, {
    sample_just_clicked(TRUE)
    session$sendCustomMessage("load_sample_csv", list(
      url      = "dc21f0_test.csv",   # served from www/
      filename = "dc21f0_test.csv"
    ))
  })

  # When the upload arrives — only if it came from the sample-click flow —
  # fire a confirmation toast and smooth-scroll to the Data Preview anchor.
  observeEvent(input$uploadfile, {
    req(input$uploadfile)
    if (!isTRUE(sample_just_clicked())) return()
    sample_just_clicked(FALSE)
    ds <- dataset()
    if (!is.null(ds)) {
      n_rows <- nrow(ds)
      n_tok  <- if ("token"   %in% names(ds)) length(unique(ds$token))   else NA
      n_spk  <- if ("speaker" %in% names(ds)) length(unique(ds$speaker)) else NA
      msg <- sprintf("Sample data loaded: %s rows", format(n_rows, big.mark = ","))
      if (!is.na(n_tok)) msg <- paste0(msg, sprintf(", %d tokens", n_tok))
      if (!is.na(n_spk)) msg <- paste0(msg, sprintf(", %d speakers", n_spk))
      msg <- paste0(msg, ". ✅  Try the Visualise tab to plot the contours!")
      showNotification(msg, type = "message", duration = 6,
                       id = "f0a_sample_loaded")
    }
    session$sendCustomMessage("scroll_to_id", "f0a-data-preview-anchor")
  })
  
  # Shared storage for normalised dataset (written by Normalise tab, read by Model tab)
  normalised_data <- reactiveVal(NULL)

  # Shared storage for model prediction data (written by GCA/GAMM, read by Summarise)
  gca_pred_data <- reactiveVal(NULL)
  gamm_pred_data <- reactiveVal(NULL)

  # Shared storage for f0 processing tab
  fp_audio_data       <- reactiveVal(NULL)   # data.frame: basename, wav_path, tg_path, pitch_path, sr, bit, dur, channels
  fp_f0_data          <- reactiveVal(NULL)   # extracted/parsed f0 contours (long format)
  fp_pitch_candidates <- reactiveVal(list()) # named list: token -> list of per-frame data.frame(frequency, strength). Populated only for tokens parsed from .Pitch files.
  fp_metadata         <- reactiveVal(NULL)   # optional user-uploaded metadata CSV (data.frame). Joined to fp_f0_data at download time on a filename column the user selects.

  # Call the Start tab UI and server logic (start_ui function)
  start_ui(input, output, session, dataset, raw_dataset, attached_metadata)
  view_ui(input, output, session, dataset)
  normalised_ui(input, output, session, dataset, normalised_data)
  visualise_ui(input, output, session, dataset, normalised_data)
  inspect_ui(input, output, session, dataset)
  model_ui(input, output, session, dataset, normalised_data)
  gca_ui(input, output, session, dataset, normalised_data, gca_pred_data)
  gamm_ui(input, output, session, dataset, normalised_data, gamm_pred_data)
  checklist_ui(input, output, session)
  filename_guide_ui(input, output, session)
  waveform_guide_ui(input, output, session)
  summarise_ui(input, output, session, dataset, normalised_data, gca_pred_data, gamm_pred_data)

  # f0 processing tab modules
  fp_start_ui(input, output, session, fp_audio_data)
  fp_extraction_ui(input, output, session, fp_audio_data, fp_f0_data, fp_pitch_candidates, fp_metadata)
  fp_correction_ui(input, output, session, fp_audio_data, fp_f0_data, fp_pitch_candidates)
  fp_praat_script_ui(input, output, session)

  # Feature-card navigation from the About tab.
  # Cards fire Shiny.setInputValue('about_nav_target', 'F0 Analysis|View'), etc.
  observeEvent(input$about_nav_target, {
    val <- input$about_nav_target
    parts <- strsplit(val, "|", fixed = TRUE)[[1]]
    if (length(parts) != 2) return()
    main <- parts[1]; sub <- parts[2]
    sub_id <- if (main == "F0 Analysis") "tabs_data"
              else if (main == "F0 Processing") "tabs_fp"
              else if (main == "Data Collection") "tabs_collection"
              else NULL
    updateNavbarPage(session, "main_nav", selected = main)
    if (!is.null(sub_id)) {
      updateTabsetPanel(session, sub_id, selected = sub)
    }
  })

}