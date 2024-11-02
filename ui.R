library(bslib)

# Define UI for data upload----
ui <- fluidPage(
  # Include the custom CSS file
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  # ),
  #Navbar structure for UI
  navbarPage("Citation tones", 
             theme = bs_theme(version = 5, bootswatch = "minty",
                              heading_font = font_google("Open Sans"),
                              base_font = font_google("Open Sans"),
                              "font-size-base" = "0.9rem"),
             # First Navbar for F0 modelling
             tabPanel("F0 modelling", 
                      #fluid = TRUE, 
                      icon = icon("chart-bar"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("F0 Time-series"),
                          # Conditional UI based on tabs
                          conditionalPanel("input.tabs_data == 'Start'", 
                                           uiOutput("ui_fileUpload"),
                                           # Horizontal line ----
                                           tags$hr(),
                                           uiOutput("ui_dataset_name"),
                                           uiOutput("ui_datasets")),
                          conditionalPanel("input.tabs_data == 'View'", 
                                           uiOutput("ui_View")),
                          conditionalPanel("input.tabs_data == 'Normalise'", 
                                           uiOutput("ui_normalise")),
                          conditionalPanel("input.tabs_data == 'Visualise'", 
                                           uiOutput("ui_visualise"))
                          #conditionalPanel("input.tabs_data == 'Transform'", uiOutput("ui_Transform"))
                        ),
                        mainPanel(
                          tabsetPanel(id = "tabs_data",
                                      tabPanel("Start",
                                               h2(textOutput("preview_title")),
                                               uiOutput("man_example")),
                                      tabPanel("View",
                                               DT::dataTableOutput("dataviewer")),
                                      tabPanel("Normalise",
                                               DT::dataTableOutput("normalised_data")),
                                      tabPanel("Visualise",
                                               plotOutput("ggplot_output",height = "auto", width = "auto"))
                          )
                        )
                      )
             ),
  )
)
