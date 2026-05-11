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
source("ui/summarise_ui.R")
source("ui/fp_start_ui.R")
source("ui/fp_extraction_ui.R")
source("ui/fp_correction_ui.R")
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB cap for batch WAV uploads
#options(shiny.useragg = TRUE)

# Enable thematic
thematic::thematic_shiny(font = "auto")
theme_set(theme_bw(base_size = 16))

server <- function(input, output, session) {

  # Reactive dataset storage
  dataset <- reactive({
    #req(input$uploadfile)
    if (is.null(input$uploadfile)) {
      return(NULL)
    }
    dat <-read.csv(input$uploadfile$datapath, stringsAsFactors = FALSE)

    if (input$convert_to_factor) {
      dat <- dat %>%
        dplyr::mutate(across(where(is.character), as.factor))
    }

    return(dat)
  })
  
  # Shared storage for normalised dataset (written by Normalise tab, read by Model tab)
  normalised_data <- reactiveVal(NULL)

  # Shared storage for model prediction data (written by GCA/GAMM, read by Summarise)
  gca_pred_data <- reactiveVal(NULL)
  gamm_pred_data <- reactiveVal(NULL)

  # Shared storage for f0 processing tab
  fp_audio_data <- reactiveVal(NULL)   # data.frame: basename, wav_path, tg_path, pitch_path, sr, bit, dur, channels
  fp_f0_data    <- reactiveVal(NULL)   # extracted/parsed f0 contours (long format)

  # Call the Start tab UI and server logic (start_ui function)
  start_ui(input, output, session, dataset)
  view_ui(input, output, session, dataset)
  normalised_ui(input, output, session, dataset, normalised_data)
  visualise_ui(input, output, session, dataset, normalised_data)
  inspect_ui(input, output, session, dataset)
  model_ui(input, output, session, dataset, normalised_data)
  gca_ui(input, output, session, dataset, normalised_data, gca_pred_data)
  gamm_ui(input, output, session, dataset, normalised_data, gamm_pred_data)
  checklist_ui(input, output, session)
  summarise_ui(input, output, session, dataset, normalised_data, gca_pred_data, gamm_pred_data)

  # f0 processing tab modules
  fp_start_ui(input, output, session, fp_audio_data)
  fp_extraction_ui(input, output, session, fp_audio_data, fp_f0_data)
  fp_correction_ui(input, output, session, fp_audio_data, fp_f0_data)

}