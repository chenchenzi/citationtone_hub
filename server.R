
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
library(thematic)
#library(ragg)

source("ui/start_ui.R")
source("ui/view_ui.R")
source("ui/visualise_ui.R")
options(shiny.maxRequestSize = 20 * 1024^2) 
#options(shiny.useragg = TRUE)

# Enable thematic
thematic::thematic_shiny(font = "auto")
theme_set(theme_minimal(base_size = 16))

server <- function(input, output, session) {
  
  # Reactive dataset storage
  dataset <- reactive({
    req(input$uploadfile)
    dat <-read.csv(input$uploadfile$datapath, stringsAsFactors = FALSE)
    
    if (input$convert_to_factor) {
      dat <- dat %>%
        dplyr::mutate(across(where(is.character), as.factor))
    }
    
    return(dat)
  })
  
  # Call the Start tab UI and server logic (start_ui function)
  start_ui(input, output, session, dataset)
  view_ui(input, output, session, dataset)
  visualise_ui(input, output, session, dataset)
 
}