library(bslib)

# Define UI for data upload----
ui <- fluidPage(
  # Include the custom CSS file
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  # ),
  # Syntax highlighting for R code blocks
  tags$head(
    # Favicons
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "192x192", href = "favicon-192.png"),
    tags$link(rel = "apple-touch-icon", sizes = "192x192", href = "favicon-192.png"),
    tags$link(rel = "apple-touch-icon", sizes = "512x512", href = "favicon-512.png"),
    # Google Analytics (GA4) — replace G-5RG08QWPPG with your measurement ID
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-5RG08QWPPG"),
    tags$script(HTML("
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-5RG08QWPPG');
    ")),
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
      // Dismiss loading screen once Shiny connects
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
  navbarPage(title = tags$img(src = "shinytone.svg", height = "60px", alt = "Shinytone",
                              style = "margin-top: -16px; margin-bottom: -16px;"),
             windowTitle = "Shinytone",
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
                                               # Static welcome content (shown immediately, replaced once renderUI loads)
                                               tags$div(id = "static-welcome",
                                                 h2("Welcome!"),
                                                 tags$p("Upload a CSV file to get started. Your file should have column headers and ideally contain columns for time, f0, tone category, speaker ID, and token ID."),
                                                 tags$p("Once uploaded, a preview of the first 10 rows will appear below.")
                                               ),
                                               uiOutput("instruction_text"),
                                               h2(textOutput("preview_title")),
                                               uiOutput("man_example"),
                                               tags$script(HTML("
                                                 $(document).on('shiny:value', function(e) {
                                                   if (e.name === 'instruction_text') {
                                                     $('#static-welcome').remove();
                                                   }
                                                 });
                                               "))),
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
             # About & Privacy panel
             tabPanel("About",
                      icon = icon("circle-info"),
                      tags$div(style = "max-width: 800px; margin: 0 auto; padding: 20px 15px;",
                        h2("About Shinytone"),
                        tags$p("Shinytone is an open-source R Shiny web application that integrates the full citation tone analysis workflow into a single interactive tool. It is designed for phoneticians, typologists, fieldworkers, and students studying lexical tone production."),
                        tags$p("Developed by ", icon("laptop-code"), " ", tags$a(href = "https://chenzixu.rbind.io/", target = "_blank", "Chenzi Xu")),
                        tags$p("Source code: ", icon("github"), " ", tags$a(href = "https://github.com/chenchenzi/citationtone_hub", target = "_blank", "github.com/chenchenzi/citationtone_hub")),

                        tags$hr(),
                        h3("Frequently Asked Questions"),

                        # --- Privacy & Data ---
                        h4(style = "color: #78c2ad; margin-top: 20px;", "Privacy & Data"),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("Is my data stored or shared?")),
                          tags$p("No. When you upload a CSV file, it is loaded into temporary server memory for in-session analysis only. It is never written to any persistent database, cloud storage, or file system, and is never transmitted to the developer or any third party.")
                        ),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("What happens when I close the app?")),
                          tags$p("All uploaded data and results are irrecoverably discarded when your session ends, the browser tab is closed, or the app reaches its idle timeout (typically 5 minutes of inactivity). No copy is retained. Download any outputs (CSV, plots) before closing.")
                        ),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("Can anyone access my data during my session?")),
                          tags$p("No. No individual, including the developer, can access, retrieve, or view data uploaded during your session. The source code is publicly available for independent verification.")
                        ),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("Should I upload sensitive or personal data?")),
                          tags$p("This app is designed for linguistic research data (e.g., acoustic measurements, tone labels). Do not upload personally identifiable information (PII) or protected health information (PHI). The developer accepts no liability for data uploaded in violation of applicable privacy regulations.")
                        ),

                        # --- Hosting & Platform ---
                        h4(style = "color: #78c2ad; margin-top: 20px;", "Hosting & Platform"),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("Where is the app hosted?")),
                          tags$p("The app is hosted on ", tags$a(href = "https://www.shinyapps.io/", target = "_blank", "shinyapps.io"), ", operated by Posit, PBC, using Amazon Web Services (AWS) infrastructure in the us-east-1 region. All connections use HTTPS/TLS encryption. Note: Posit plans to migrate shinyapps.io users to ",
                            tags$a(href = "https://connect.posit.cloud/", target = "_blank", "Posit Connect Cloud"),
                            " by the end of 2026.")
                        ),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("Does the platform collect any metadata?")),
                          tags$p("Posit may independently collect server-level metadata such as IP address, browser/device information, and request timestamps. See ", tags$a(href = "https://posit.co/about/privacy-policy/", target = "_blank", "Posit's privacy policy"), " for details.")
                        ),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("Does this app use cookies?")),
                          tags$p("The app itself sets no cookies. Shiny/shinyapps.io sets a session-scoped cookie to route server responses to the correct client. It expires when the browser session ends.")
                        ),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("Does this app use analytics?")),
                          tags$p("This app uses Google Analytics to collect anonymous usage statistics (e.g., page views, session duration). No personally identifiable information or uploaded data is sent to Google Analytics.")
                        ),

                        # --- Usage ---
                        h4(style = "color: #78c2ad; margin-top: 20px;", "Usage"),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("Is this app open source?")),
                          tags$p("Yes. The source code is available at ", tags$a(href = "https://github.com/chenchenzi/citationtone_hub", target = "_blank", "github.com/chenchenzi/citationtone_hub"), ". It is licensed under ", tags$a(href = "https://creativecommons.org/licenses/by-nc/4.0/", target = "_blank", "CC BY-NC 4.0"), ". Free to use for research and teaching (non-commercial) purposes with attribution.")
                        ),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("Can I run Shinytone locally?")),
                          tags$p("Yes. Clone the repository and run ", tags$code("shiny::runApp()"), " in R. When running locally, your data never leaves your machine. For very large datasets, consider running the app locally.")
                        ),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("How do I cite Shinytone?")),
                          tags$p("If you use Shinytone in your research, please cite it as:"),
                          tags$blockquote(style = "border-left: 3px solid #78c2ad; padding-left: 12px; color: #555; font-size: 0.9rem;",
                            "Xu, Chenzi (2026). Shinytone: A citation tone research hub. [Web application]. Available at https://chenzixu.shinyapps.io/shinytone/"
                          )
                        ),

                        tags$div(class = "faq-item", style = "margin-bottom: 16px;",
                          tags$p(tags$strong("I found a bug or have a feature request.")),
                          tags$p("Please open an issue on the ", tags$a(href = "https://github.com/chenchenzi/citationtone_hub/issues", target = "_blank", "GitHub repository"), ".")
                        ),

                        # --- Footer ---
                        tags$hr(),
                        tags$p(style = "color: #999; font-size: 0.8rem;", "Last updated: March 2026")
                      )
             ),
  )
)
