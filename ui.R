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
      #visualise_guide strong, #normalise_guide strong, #view_guide strong, #inspect_guide strong, #model_guide strong, #gca_guide strong, #gamm_guide strong, #summarise_guide strong { font-weight: 900; color: #3a3a3a; }
      .plot-spinner-wrap { position: relative; }
      .plot-spinner-wrap .recalculating { opacity: 0.3 !important; }
      .plot-spinner-wrap .recalculating ~ .plot-spinner,
      .plot-spinner-wrap .recalculating + .plot-spinner { display: block !important; }
      .plot-spinner {
        display: none; position: absolute; top: 50%; left: 50%;
        transform: translate(-50%, -50%); z-index: 10;
      }
      .plot-spinner::after {
        content: ''; display: block; width: 40px; height: 40px;
        border: 4px solid #e0e0e0; border-top-color: #78c2ad;
        border-radius: 50%; animation: spin 0.8s linear infinite;
      }
      @keyframes spin { to { transform: rotate(360deg); } }
      /* Startup loading screen */
      #loading-screen {
        position: fixed; top: 0; left: 0; width: 100%; height: 100%;
        background: #fff; z-index: 9999;
        display: flex; flex-direction: column;
        align-items: center; justify-content: center;
        transition: opacity 0.4s ease;
      }
      #loading-screen.fade-out { opacity: 0; pointer-events: none; }
      #loading-screen .loader {
        width: 48px; height: 48px;
        border: 5px solid #e0e0e0; border-top-color: #78c2ad;
        border-radius: 50%; animation: spin 0.8s linear infinite;
      }
      #loading-screen p {
        margin-top: 16px; font-family: 'Open Sans', sans-serif;
        color: #888; font-size: 0.95rem;
      }
    "),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        function watchForCode(id) {
          var target = document.getElementById(id);
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
        }
        watchForCode('r_code_output');
        watchForCode('model_r_code');
        watchForCode('gca_r_code');
        watchForCode('gamm_r_code');
        watchForCode('summarise_r_code');
      });
      $(document).on('shiny:connected', function() {
        var el = document.getElementById('loading-screen');
        if (el) { el.classList.add('fade-out'); setTimeout(function(){ el.remove(); }, 500); }
      });
    "))
  ),
  # Startup loading screen
  tags$div(id = "loading-screen",
    tags$div(class = "loader"),
    tags$p("Loading app...")
  ),
  #Navbar structure for UI
  navbarPage("Citation tones", 
             theme = bs_theme(version = 5, bootswatch = "minty",
                              heading_font = font_google("Open Sans"),
                              base_font = font_google("Open Sans"),
                              "font-size-base" = "0.9rem"),
             # F0 modelling panel
             tabPanel("F0 Analysis",
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
                                           uiOutput("ui_visualise")),
                          conditionalPanel("input.tabs_data == 'Inspect'",
                                           uiOutput("ui_inspect")),
                          conditionalPanel("input.tabs_data == 'Model: Polynomials'",
                                           uiOutput("ui_model")),
                          conditionalPanel("input.tabs_data == 'Model: GCA'",
                                           uiOutput("ui_gca")),
                          conditionalPanel("input.tabs_data == 'Model: GAMM'",
                                           uiOutput("ui_gamm")),
                          conditionalPanel("input.tabs_data == 'Summarise'",
                                           uiOutput("ui_summarise"))
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
                                               tags$div(class = "plot-spinner-wrap",
                                                 plotOutput("ggplot_output",height = "auto", width = "auto"),
                                                 tags$div(class = "plot-spinner")
                                               ),
                                               uiOutput("r_code_output")),
                                      tabPanel("Inspect",
                                               uiOutput("inspect_guide"),
                                               uiOutput("inspect_summary"),
                                               DT::dataTableOutput("inspect_data")),
                                      tabPanel("Model: Polynomials",
                                               uiOutput("model_guide"),
                                               tags$div(class = "plot-spinner-wrap",
                                                 uiOutput("model_summary"),
                                                 tags$div(class = "plot-spinner")
                                               ),
                                               DT::dataTableOutput("model_data"),
                                               uiOutput("model_r_code")),
                                      tabPanel("Model: GCA",
                                               uiOutput("gca_guide"),
                                               tags$div(class = "plot-spinner-wrap",
                                                 uiOutput("gca_summary"),
                                                 plotOutput("gca_plot", height = "auto", width = "auto"),
                                                 tags$div(class = "plot-spinner")
                                               ),
                                               uiOutput("gca_r_code")),
                                      tabPanel("Model: GAMM",
                                               uiOutput("gamm_guide"),
                                               tags$div(class = "plot-spinner-wrap",
                                                 uiOutput("gamm_summary"),
                                                 plotOutput("gamm_plot", height = "auto", width = "auto"),
                                                 tags$div(class = "plot-spinner")
                                               ),
                                               uiOutput("gamm_r_code")),
                                      tabPanel("Summarise",
                                               uiOutput("summarise_guide"),
                                               tags$div(class = "plot-spinner-wrap",
                                                 uiOutput("summarise_summary"),
                                                 plotOutput("summarise_plot", height = "auto", width = "auto"),
                                                 tags$div(class = "plot-spinner")
                                               ),
                                               uiOutput("summarise_r_code"))
                          )
                        )
                      )
             ),
             # Data collection panel
             tabPanel("Data collection",
                      icon = icon("clipboard-check"),
                      tabsetPanel(id = "tabs_collection",
                        tabPanel("Checklist",
                                 uiOutput("checklist_content"))
                      )
             ),
  )
)
