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

# Eager: only what is needed to build the UI and render the landing (About)
# page. The heavy analysis / plot / audio packages are loaded right after the
# first render (see the `session$onFlushed` deferred loader inside server()),
# so the landing page appears immediately and the rest load while the user
# reads it. tidyr / purrr / stringr / readr are never used by bare name, so
# dplyr (here) + ggplot2 (deferred) cover all former `tidyverse` usage.
library(shiny)
library(bslib)
library(dplyr)

source("ui/start_ui.R")
source("ui/view_ui.R")
source("ui/visualise_ui.R")
source("ui/normalise_ui.R")
source("ui/inspect_ui.R")
source("ui/cluster_ui.R")
source("ui/curate_ui.R")
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

# Plot theming (thematic auto-theming + the default ggplot2 theme) is set up
# in the deferred loader inside server(), once ggplot2 has been loaded.

# Note: shared helpers like guess_var() and var_patterns now live in
# global.R, which Shiny sources into the global environment before any
# session. They are reachable from every ui_*.R module from there.

server <- function(input, output, session) {

  # ---- Deferred package loading (faster first paint) ----------------------
  # The landing page only needs shiny / bslib / dplyr. Everything heavy is
  # loaded right after the first flush, so the About page renders immediately
  # and the analysis / plot / audio packages load while the user reads it.
  # R is single-threaded, so a click during loading simply waits until it
  # finishes (the front page is already in the browser). Loaded once per
  # process; later sessions hit a warm process where these are no-ops.
  session$onFlushed(function() {
    # Update reminder: run_app() sets this option when a newer shinytone
    # release exists on GitHub. Deployed instances never set it (run_app is
    # not called there), so this is a local-install-only nudge.
    upd <- getOption("shinytone.update_available")
    if (!is.null(upd)) {
      tryCatch(showNotification(
        HTML(paste0("shinytone ", upd, " is available. Update with<br>",
                    "<code>remotes::install_github(\"chenchenzi/citationtone_hub\")</code>")),
        type = "message", duration = 15, session = session),
        error = function(e) NULL)
    }
    # The "Loading analysis tools…" toast is shown client-side the instant the
    # page appears (see shiny:connected in ui.R), so the wait is visible from the
    # very start. On a warm process (a later session) the packages are already
    # attached, so there is nothing to load — just clear that toast.
    if ("package:mgcv" %in% search()) {
      session$sendCustomMessage("pkgload_done", TRUE)
      return(invisible())
    }
    # Run the blocking load one tick later (the page is already up). The later()
    # callback runs outside Shiny's reactive context, so pass `session` explicitly
    # to the sendCustomMessage that clears the toast when loading finishes.
    later::later(function() {
      tryCatch(
        suppressWarnings(suppressPackageStartupMessages({
          library(DT); library(ggplot2); library(plotly)
          library(RColorBrewer); library(gridExtra)
          library(lme4); library(emmeans); library(mgcv)
          library(tuneR); library(rPraat); library(praatpicture)
          library(thematic)
          # Pin the plot device background to white (the page background) rather
          # than "auto". With "auto", shiny::startPNG calls
          # getCurrentOutputInfo()[["bg"]]() which errors ("attempt to apply
          # non-function") whenever a plot output does not report its CSS theme
          # info (e.g. plotOutput nested inside styled section cards). White
          # matches the page, so this is visually identical but crash-proof.
          thematic::thematic_shiny(bg = "white", font = "auto")
          ggplot2::theme_set(ggplot2::theme_bw(base_size = 16))
        })),
        error = function(e) NULL)
      session$sendCustomMessage("pkgload_done", TRUE)
    }, delay = 0.25)
  }, once = TRUE)

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

  # Shared storage for curated dataset (written by Curate tab: relabels applied
  # to the tone column, excluded tokens dropped; offered as a "Curated data"
  # source in the modelling and summarise tabs). NULL until the user curates.
  curated_data <- reactiveVal(NULL)

  # Shared inspection result (written by Inspect tab, read by Curate tab so the
  # actual Inspect flags can seed re-label candidates). NULL until Inspect runs.
  inspect_result <- reactiveVal(NULL)

  # Shared storage for clustered dataset (written by Cluster tab: adds a
  # `cluster` column of candidate tone categories; offered as a "Clustered
  # data" source in the modelling tabs). NULL until the user runs clustering.
  cluster_data <- reactiveVal(NULL)

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
  inspect_ui(input, output, session, dataset, inspect_result)
  cluster_ui(input, output, session, dataset, normalised_data, curated_data, cluster_data)
  curate_ui(input, output, session, dataset, normalised_data, curated_data, inspect_result, cluster_data)
  model_ui(input, output, session, dataset, normalised_data, curated_data, cluster_data)
  gca_ui(input, output, session, dataset, normalised_data, gca_pred_data, curated_data, cluster_data)
  gamm_ui(input, output, session, dataset, normalised_data, gamm_pred_data, curated_data, cluster_data)
  checklist_ui(input, output, session)
  filename_guide_ui(input, output, session)
  waveform_guide_ui(input, output, session)
  summarise_ui(input, output, session, dataset, normalised_data, gca_pred_data, gamm_pred_data, curated_data)

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

  # ---- Deep links for the four top-level tabs (tab name only; never data) ----
  # The URL carries only which workspace is open, e.g. ?tab=analysis. For the
  # Data Collection tab only, the active sub-tab is also encoded as a URL hash,
  # e.g. ?tab=data-collection#reading-waveform. Uploaded data stays in session
  # memory and is never placed in the URL, so this does not affect the app's
  # privacy guarantees.
  nav_slug2tab <- c("about"           = "About",
                    "analysis"        = "F0 Analysis",
                    "processing"      = "F0 Processing",
                    "data-collection" = "Data Collection")
  nav_tab2slug <- stats::setNames(names(nav_slug2tab), unname(nav_slug2tab))

  # Sub-tabs of Data Collection only (slug <-> tab title), used for the hash.
  collection_slug2tab <- c("filename"         = "Filename",
                           "checklist"        = "Checklist",
                           "reading-waveform" = "Reading waveform")
  collection_tab2slug <- stats::setNames(names(collection_slug2tab),
                                         unname(collection_slug2tab))
  nav_ready <- reactiveVal(FALSE)

  # Build the URL for the current navigation state: ?tab=<slug>, plus a
  # #<sub-slug> hash only while the Data Collection tab is the open workspace.
  nav_current_url <- function() {
    slug <- nav_tab2slug[[input$main_nav]]
    if (is.null(slug) || is.na(slug)) return(NULL)
    url <- paste0("?tab=", slug)
    if (identical(input$main_nav, "Data Collection")) {
      sub <- collection_tab2slug[[input$tabs_collection]]
      if (!is.null(sub) && !is.na(sub)) url <- paste0(url, "#", sub)
    }
    url
  }

  # On load: open the tab named in ?tab=<slug> (if any) and, for Data
  # Collection, the sub-tab named in the #<hash>; then start syncing.
  observeEvent(session$clientData$url_search, {
    slug <- parseQueryString(session$clientData$url_search)[["tab"]]
    if (!is.null(slug) && slug %in% names(nav_slug2tab)) {
      updateNavbarPage(session, "main_nav", selected = nav_slug2tab[[slug]])
      if (identical(nav_slug2tab[[slug]], "Data Collection")) {
        hash <- session$clientData$url_hash
        hash <- if (is.null(hash)) "" else sub("^#", "", hash)
        if (nzchar(hash) && hash %in% names(collection_slug2tab)) {
          updateTabsetPanel(session, "tabs_collection",
                            selected = collection_slug2tab[[hash]])
        }
      }
    }
    nav_ready(TRUE)
  }, once = TRUE, ignoreNULL = FALSE)

  # Reflect the active top tab in the URL whenever it changes.
  observeEvent(input$main_nav, {
    if (!isTRUE(nav_ready())) return()
    url <- nav_current_url()
    if (!is.null(url)) updateQueryString(url, mode = "push")
  })

  # Reflect the active Data Collection sub-tab as a URL hash when it changes
  # (only while Data Collection is the open workspace).
  observeEvent(input$tabs_collection, {
    if (!isTRUE(nav_ready())) return()
    if (!identical(input$main_nav, "Data Collection")) return()
    url <- nav_current_url()
    if (!is.null(url)) updateQueryString(url, mode = "push")
  })

}