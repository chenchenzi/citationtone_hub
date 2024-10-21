
# Define UI for data upload----
ui <- fluidPage(
  #Navbar structure for UI
  navbarPage("Citation tones", 
             theme = bs_theme(version = 5, bootswatch = "minty",
                              heading_font = font_google("Petrona"),
                              base_font = font_google("Open Sans"),
                              "font-size-base" = "0.9rem"),
             tabPanel("F0 modelling", 
                      #fluid = TRUE, 
                      icon = icon("chart-bar"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("F0 Time-series"),
                          # Input: Select a file ----
                          fileInput("file1", "Choose a CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Check box if file has header ----
                          checkboxInput("header", "Header", TRUE),
                          
                          # Input: Select separator ----
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),

                        ),
                        # Main panel for displaying outputs ----
                        mainPanel(
                          tabsetPanel(
                            tabPanel("View",
                                     h4("Table"),
                                     # Output: Data file ----
                                     tableOutput("contents"),

                                     h4("Verbatim text output"),
                                     verbatimTextOutput("txtout"),
                                     h1("Header 1"),
                                     h2("Header 2"),
                                     h3("Header 3"),
                                     h4("Header 4"),
                                     h5("Header 5")
                            ),
                            tabPanel("Visualise", "This panel is intentionally left blank"),
                            tabPanel("Transform", "This panel is intentionally left blank")
                          )
                          
                          
                        )
                      )
 
             ),
             
  )
)
