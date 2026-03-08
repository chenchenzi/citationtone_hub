library(bslib)

# Define UI for data upload----
ui <- fluidPage(
  # Include the custom CSS file
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  # ),
  # Syntax highlighting for R code blocks
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;700&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/r.min.js"),
    tags$style("
      #plot_button, #show_code_button { display: block; }
      #save_plot_button { display: block; margin-top: 4px; width: fit-content; }
      #visualise_guide strong, #normalise_guide strong, #view_guide strong { font-weight: 900; color: #3a3a3a; }
    "),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var target = document.getElementById('r_code_output');
        if (!target) return;
        var observer = new MutationObserver(function() {
          var codeEls = target.querySelectorAll('pre code');
          codeEls.forEach(function(el) {
            if (!el.classList.contains('hljs')) {
              el.classList.add('language-r');
              hljs.highlightElement(el);
            }
          });
        });
        observer.observe(target, { childList: true, subtree: true });
      });
    "))
  ),
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
                                               uiOutput("instruction_text"),
                                               h2(textOutput("preview_title")),
                                               uiOutput("man_example")),
                                      tabPanel("View",
                                               uiOutput("view_guide"),
                                               DT::dataTableOutput("dataviewer")),
                                      tabPanel("Normalise",
                                               uiOutput("normalise_guide"),
                                               DT::dataTableOutput("normalised_data")),
                                      tabPanel("Visualise",
                                               uiOutput("visualise_guide"),
                                               plotOutput("ggplot_output",height = "auto", width = "auto"),
                                               uiOutput("r_code_output"))
                          )
                        )
                      )
             ),
  )
)
