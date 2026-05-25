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
      /* Feature cards on the About tab */
      .feature-section { margin-bottom: 28px; }
      .feature-section h3 {
        margin: 16px 0 4px 0; color: #2c5f4f;
      }
      .feature-section p.lead {
        color: #666; font-size: 0.95rem; margin-bottom: 12px;
      }
      .feature-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(230px, 1fr));
        gap: 12px;
      }
      .feature-card {
        background: #ffffff;
        border: 1px solid #d6e7df;
        border-left: 3px solid #78c2ad;
        border-radius: 4px;
        padding: 12px 14px;
        display: flex;
        flex-direction: column;
        transition: box-shadow 0.15s, transform 0.15s;
      }
      .feature-card:hover {
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        transform: translateY(-1px);
      }
      .feature-card-title {
        font-weight: 700; font-size: 0.97rem;
        color: #2c5f4f; margin-bottom: 4px;
      }
      .feature-card-icon { font-size: 1.15rem; margin-right: 6px; }
      .feature-card-desc {
        font-size: 0.84rem; color: #555; flex-grow: 1; line-height: 1.4;
      }
      .feature-section-cta {
        display: inline-block;
        margin-top: 14px;
        padding: 7px 16px;
        background: #e8f5f0;
        color: #2c5f4f;
        border-radius: 4px;
        font-size: 0.92rem;
        font-weight: 600;
        text-decoration: none;
        transition: background 0.15s, color 0.15s;
      }
      .feature-section-cta:hover {
        background: #78c2ad;
        color: #ffffff;
        text-decoration: none;
      }
      /* Showcase (Option B): image on the left, clickable feature list on the right */
      .showcase-row {
        display: grid;
        grid-template-columns: 1.55fr 1fr;
        gap: 24px;
        align-items: stretch;
        margin-top: 8px;
      }
      @media (max-width: 900px) {
        .showcase-row { grid-template-columns: 1fr; }
      }
      .showcase-image {
        background: #f7faf9;
        border: 1px solid #d6e7df;
        border-radius: 6px;
        padding: 10px;
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 280px;
      }
      .showcase-image img {
        width: 100%;
        max-height: 460px;
        object-fit: contain;
        border-radius: 4px;
        box-shadow: 0 1px 6px rgba(0,0,0,0.08);
      }
      .showcase-cards {
        display: flex; flex-direction: column; gap: 8px;
      }
      .showcase-card {
        background: #ffffff;
        border: 1px solid #d6e7df;
        border-left: 3px solid transparent;
        border-radius: 4px;
        padding: 11px 14px;
        cursor: pointer;
        transition: border-left-color 0.15s, background 0.15s, transform 0.1s;
      }
      .showcase-card:hover {
        background: #f0faf7;
        border-left-color: #78c2ad;
        transform: translateY(-1px);
      }
      .showcase-card.active {
        background: #e8f5f0;
        border-left-color: #2c5f4f;
      }
      .showcase-card-title {
        font-weight: 700; color: #2c5f4f;
        font-size: 0.98rem; margin-bottom: 4px;
      }
      .showcase-card-icon { font-size: 1.1rem; margin-right: 6px; }
      .showcase-card-desc {
        font-size: 0.83rem; color: #555; line-height: 1.45;
      }
      /* Inline chip showing the corresponding tab name next to a friendly section heading */
      .tab-ref-chip {
        font-family: 'SFMono-Regular', Menlo, Consolas, monospace;
        font-size: 0.7rem;
        font-weight: 500;
        color: #6b7d75;
        background: #eef5f1;
        border: 1px solid #d6e7df;
        padding: 2px 8px;
        border-radius: 10px;
        letter-spacing: 0.02em;
        white-space: nowrap;
      }
      .tab-ref-chip::before { content: '\\21AA  '; opacity: 0.6; margin-right: 2px; }
      /* FAQ subsection cards (Privacy & Data / Hosting / Usage) */
      .faq-section {
        background: #f0faf7;
        border-radius: 8px;
        padding: 18px 24px 6px 24px;
        margin: 20px 0;
      }
      .faq-section > h4 {
        margin-top: 0 !important;
        margin-bottom: 12px;
        color: #2c5f4f !important;
        font-weight: 700;
      }
      .faq-section .faq-item {
        background: #ffffff;
        border-radius: 4px;
        border-left: 3px solid #78c2ad;
        padding: 10px 14px 4px 14px;
        margin-bottom: 10px !important;
      }
      .faq-section .faq-item p { margin-bottom: 6px; }
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
             id = "main_nav",
             windowTitle = "Shinytone",
             theme = bs_theme(version = 5, bootswatch = "minty",
                              heading_font = font_google("Open Sans"),
                              base_font = font_google("Open Sans"),
                              "font-size-base" = "0.9rem"),
             # About & Privacy panel (landing page)
             tabPanel("About",
                      icon = icon("house"),
                      # --- Hero: logo + tagline + author/source ---
                      tags$div(style = "text-align: center; padding: 32px 15px 16px 15px;",
                        tags$img(src = "shinytone.svg", height = "110px", alt = "Shinytone",
                                 style = "margin-bottom: 16px;"),
                        tags$p(style = "color: #555; font-size: 1.02rem; max-width: 820px; margin: 0 auto 14px auto; line-height: 1.55;",
                          "An open-source tool for citation tone research across tone languages. ",
                          "It integrates the full citation tone analysis workflow into an interactive tool. ",
                          "It is designed for phoneticians, typologists, fieldworkers, and students working on lexical tone production."),
                        tags$p(style = "color: #777; font-size: 0.88rem; margin: 0;",
                          icon("laptop-code"), " ",
                          tags$a(href = "https://chenzixu.rbind.io/", target = "_blank", "Chenzi Xu"),
                          " ",
                          tags$a(href = "https://congzhang-linguist.github.io", target = "_blank", "Cong Zhang"),
                          tags$span(style = "margin: 0 10px; color: #ccc;", "·"),
                          icon("github"), " ",
                          tags$a(href = "https://github.com/chenchenzi/citationtone_hub", target = "_blank",
                                 "github.com/chenchenzi/citationtone_hub")
                        )
                      ),

                      # --- Section break + landing heading ---
                      tags$div(style = "max-width: 900px; margin: 40px auto 8px auto; text-align: center; padding: 0 15px;",
                        # Short teal accent bar (visual divider)
                        tags$div(style = "width: 56px; height: 3px; background: #78c2ad; margin: 0 auto 24px auto; border-radius: 2px;"),
                        tags$h2(style = "color: #2c5f4f; margin: 0 0 8px 0; font-weight: 700;",
                                "What you can do"),
                        tags$p(style = "color: #777; font-size: 0.95rem; margin: 0;",
                          "Two complementary pipelines, from audio recordings to pitch contour analysis.")
                      ),

                      # --- What you can do (interactive showcase: image + clickable cards) ---
                      tags$div(style = "max-width: 1080px; margin: 0 auto; padding: 8px 15px 0 15px;",
                        # Inline JS handler — swaps the image and highlights the active card
                        tags$script(HTML("
                          window.shinytoneShowcase = function(section, file) {
                            var img = document.getElementById('showcase-img-' + section);
                            if (img) img.src = 'screenshots/' + file;
                            document.querySelectorAll('.showcase-card[data-section=\"' + section + '\"]').forEach(function(c){
                              c.classList.toggle('active', c.dataset.file === file);
                            });
                          };
                        ")),
                        local({
                          showcase_card <- function(section, emoji, name, file, desc, active = FALSE) {
                            cls <- if (active) "showcase-card active" else "showcase-card"
                            js  <- sprintf("shinytoneShowcase('%s', '%s'); return false;", section, file)
                            tags$div(class = cls,
                              `data-section` = section, `data-file` = file,
                              onclick = js,
                              tags$div(class = "showcase-card-title",
                                HTML(paste0("<span class='showcase-card-icon'>", emoji, "</span>", name))),
                              tags$div(class = "showcase-card-desc", desc)
                            )
                          }
                          section_cta <- function(main, label) {
                            js <- sprintf(
                              "Shiny.setInputValue('about_nav_target', '%s|Start', {priority:'event'}); return false;",
                              main)
                            tags$div(style = "text-align: right; margin-top: 12px;",
                              tags$a(class = "feature-section-cta",
                                     href = "#", onclick = js, label)
                            )
                          }
                          tagList(
                            # --- F0 Analysis section ---
                            tags$div(class = "feature-section",
                              h4(style = "color: #78c2ad; margin-top: 20px; display: flex; align-items: baseline; flex-wrap: wrap; gap: 10px;",
                                 "Pitch Contour Analysis",
                                 tags$span(class = "tab-ref-chip", "F0 Analysis tab")
                              ),
                              tags$p(class = "lead",
                                "Run the full citation-tone analysis pipeline on f0 measurements (CSV)."),
                              tags$div(class = "showcase-row",
                                tags$div(class = "showcase-image",
                                  tags$img(id = "showcase-img-analysis",
                                           src = "screenshots/visualise.png",
                                           alt = "F0 Analysis screenshot")
                                ),
                                tags$div(class = "showcase-cards",
                                  showcase_card("analysis", "\U0001F3A8", "Visualise", "visualise.png",
                                    "Plot f0 contours coloured by tone, with optional speaker faceting and normalisation (z-score or semitone). Export the plots and the underlying R code.",
                                    active = TRUE),
                                  showcase_card("analysis", "\U0001F50E", "Inspect", "inspect.png",
                                    "Flag likely f0 artefacts such as octave jumps, out-of-range outliers, and tracking errors, using by-speaker z-scores and sample-to-sample jump detection."),
                                  showcase_card("analysis", "\U0001F4CA", "Model", "model.png",
                                    "Three modelling approaches: per-token Legendre polynomials, Growth Curve Analysis (GCA) with mixed effects, and Generalised Additive Mixed Models (GAMM)."),
                                  showcase_card("analysis", "\U0001F4CB", "Summarise", "summarise.png",
                                    "Convert tone contours into Chao tone numerals (1–5 scale) for cross-language comparison. Compare reference-line and interval-based methods.")
                                )
                              ),
                              section_cta("F0 Analysis", "Get started →")
                            ),
                            # --- F0 Processing section ---
                            tags$div(class = "feature-section",
                              h4(style = "color: #78c2ad; margin-top: 20px; display: flex; align-items: baseline; flex-wrap: wrap; gap: 10px;",
                                 "Pitch Measurement",
                                 tags$span(class = "tab-ref-chip", "F0 Processing tab")
                              ),
                              tags$p(class = "lead",
                                "Go from raw .wav files to clean f0 contours, ready for the analysis pipeline above."),
                              tags$div(class = "showcase-row",
                                tags$div(class = "showcase-image",
                                  tags$img(id = "showcase-img-processing",
                                           src = "screenshots/extraction.png",
                                           alt = "F0 Processing screenshot")
                                ),
                                tags$div(class = "showcase-cards",
                                  showcase_card("processing", "\U0001F30A", "Extract", "extraction.png",
                                    "Extract f0 contours from .wav files using the wrassp R package, or import pre-computed Praat .Pitch and .PitchTier files.",
                                    active = TRUE),
                                  showcase_card("processing", "\U0001F527", "Correct", "correction.png",
                                    "Review and correct extraction artefacts by listening to the audio, inspecting the waveform, and editing individual frames (halve, double, interpolate, smooth, manual entry, etc.).")
                                )
                              ),
                              section_cta("F0 Processing", "Get started →")
                            )
                          )
                        })
                      ),

                      # --- FAQ ---
                      tags$div(style = "max-width: 900px; margin: 48px auto 0 auto; text-align: center; padding: 0 15px;",
                        tags$div(style = "width: 56px; height: 3px; background: #78c2ad; margin: 0 auto 24px auto; border-radius: 2px;"),
                        tags$h2(style = "color: #2c5f4f; margin: 0 0 8px 0; font-weight: 700;",
                                "Frequently Asked Questions")
                      ),
                      tags$div(style = "max-width: 900px; margin: 0 auto; padding: 20px 15px;",

                        # --- Privacy & Data ---
                        tags$div(class = "faq-section",
                          h4("Privacy & Data"),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Is my data stored or shared?")),
                            tags$p("No. When you upload a CSV or audio file, it is loaded into temporary server memory for in-session analysis only. It is never written to any persistent database, cloud storage, or file system, and is never transmitted to the developer or any third party.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("What happens when I close the app?")),
                            tags$p("All uploaded data and results are irrecoverably discarded when your session ends, the browser tab is closed, or the app reaches its idle timeout (typically 5 minutes of inactivity). No copy is retained. Download any outputs (CSV, plots) before closing.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Can anyone access my data during my session?")),
                            tags$p("No. No individual, including the developer, can access, retrieve, or view data uploaded during your session. The source code is publicly available for independent verification.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("What about audio recordings? Aren't those personal data?")),
                            tags$p("Yes — voice recordings are personal data under GDPR (biometric data), CCPA, and most institutional ethics frameworks, especially when paired with speaker IDs or demographic metadata. Shinytone processes audio in temporary server memory only and never writes it to persistent storage, but the data does pass through Posit/AWS infrastructure during your session."),
                            tags$p("Before uploading audio:"),
                            tags$ul(style = "margin-top: -4px;",
                              tags$li("Confirm your participant consent covers web-based processing on third-party infrastructure (Posit, AWS)."),
                              tags$li("For sensitive populations (children, clinical, endangered-language, or otherwise identifiable participants), running Shinytone locally is strongly recommended. Clone the repo and run ", tags$code("shiny::runApp()"), " — your audio never leaves your machine."),
                              tags$li("If in doubt, ask your IRB / ethics committee.")
                            )
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Should I upload sensitive or personal data?")),
                            tags$p("This app is designed for linguistic research data (acoustic measurements, tone labels, and field recordings). Do not upload personally identifiable information beyond the voice and metadata your participants have consented to share. For sensitive recordings (children, clinical populations, etc.), use the local installation. The developer accepts no liability for data uploaded in violation of applicable privacy regulations or ethics approvals.")
                          )
                        ),

                        # --- Hosting & Platform ---
                        tags$div(class = "faq-section",
                          h4("Hosting & Platform"),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Where is the app hosted?")),
                            tags$p("The app is hosted on ", tags$a(href = "https://www.shinyapps.io/", target = "_blank", "shinyapps.io"), ", operated by Posit, PBC, using Amazon Web Services (AWS) infrastructure in the us-east-1 region. All connections use HTTPS/TLS encryption. Note: Posit plans to migrate shinyapps.io users to ",
                              tags$a(href = "https://connect.posit.cloud/", target = "_blank", "Posit Connect Cloud"),
                              " by the end of 2026.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Does the platform collect any metadata?")),
                            tags$p("Posit may independently collect server-level metadata such as IP address, browser/device information, and request timestamps. See ", tags$a(href = "https://posit.co/about/privacy-policy/", target = "_blank", "Posit's privacy policy"), " for details.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Does this app use cookies?")),
                            tags$p("The app itself sets no cookies. Shiny/shinyapps.io sets a session-scoped cookie to route server responses to the correct client. It expires when the browser session ends.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Does this app use analytics?")),
                            tags$p("This app uses Google Analytics to collect anonymous usage statistics (e.g., page views, session duration). No personally identifiable information or uploaded data is sent to Google Analytics.")
                          )
                        ),

                        # --- Usage ---
                        tags$div(class = "faq-section",
                          h4("Usage"),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Is this app open source?")),
                            tags$p("Yes. The source code is available at ", tags$a(href = "https://github.com/chenchenzi/citationtone_hub", target = "_blank", "github.com/chenchenzi/citationtone_hub"), ". It is licensed under ", tags$a(href = "https://creativecommons.org/licenses/by-nc/4.0/", target = "_blank", "CC BY-NC 4.0"), ". Free to use for research and teaching (non-commercial) purposes with attribution.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Can I run Shinytone locally?")),
                            tags$p("Yes. Clone the repository and run ", tags$code("shiny::runApp()"), " in R. When running locally, your data never leaves your machine. For very large datasets, consider running the app locally.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("How do I cite Shinytone?")),
                            tags$p("If you use Shinytone in your research, please cite it as:"),
                            tags$blockquote(style = "border-left: 3px solid #78c2ad; padding-left: 12px; color: #555; font-size: 0.9rem;",
                              "Xu, Chenzi (2026). Shinytone: A citation tone research hub. [Web application]. Available at https://chenzixu.shinyapps.io/shinytone/"
                            )
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("I found a bug or have a feature request.")),
                            tags$p("Please open an issue on the ", tags$a(href = "https://github.com/chenchenzi/citationtone_hub/issues", target = "_blank", "GitHub repository"), ".")
                          )
                        ),

                        # --- Footer ---
                        tags$hr(),
                        tags$p(style = "color: #999; font-size: 0.8rem;", "Last updated: March 2026")
                      )
             ),
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
                                               uiOutput("model_plot_block"),
                                               DT::dataTableOutput("model_data"),
                                               uiOutput("model_r_code")),
                                      tabPanel("Model: GCA",
                                               uiOutput("gca_guide"),
                                               tags$div(class = "plot-spinner-wrap",
                                                 uiOutput("gca_summary"),
                                                 plotOutput("gca_plot", height = "500px", width = "800px"),
                                                 tags$div(class = "plot-spinner")
                                               ),
                                               uiOutput("gca_r_code")),
                                      tabPanel("Model: GAMM",
                                               uiOutput("gamm_guide"),
                                               tags$div(class = "plot-spinner-wrap",
                                                 uiOutput("gamm_summary"),
                                                 plotOutput("gamm_plot", height = "500px", width = "800px"),
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
             # F0 Processing panel ------------------------------------------------
             tabPanel("F0 Processing",
                      icon = icon("wave-square"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Audio files"),
                          conditionalPanel("input.tabs_fp == 'Start'",
                                           uiOutput("ui_fp_upload")),
                          conditionalPanel("input.tabs_fp == 'F0 Extraction'",
                                           uiOutput("ui_fp_extraction")),
                          conditionalPanel("input.tabs_fp == 'F0 Correction'",
                                           uiOutput("ui_fp_correction"))
                        ),
                        mainPanel(
                          tabsetPanel(id = "tabs_fp",
                                      tabPanel("Start",
                                               uiOutput("fp_start_guide"),
                                               uiOutput("fp_files_summary"),
                                               DT::dataTableOutput("fp_files_table")),
                                      tabPanel("F0 Extraction",
                                               uiOutput("fp_extraction_guide"),
                                               uiOutput("fp_extraction_results")),
                                      tabPanel("F0 Correction",
                                               uiOutput("fp_correction_guide"))
                          )
                        )
                      )
             ),
             tabPanel("Data Collection",
                      icon = icon("clipboard-check"),
                      tabsetPanel(id = "tabs_collection",
                        tabPanel("Filename",
                                 uiOutput("filename_guide_content")),
                        tabPanel("Checklist",
                                 uiOutput("checklist_content"))
                      )
             ),
  )
)
