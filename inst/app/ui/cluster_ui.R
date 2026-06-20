###############################################
# Cluster tab: unsupervised discovery of how many tone categories a language
# has, by clustering f0-contour shape (no tone labels needed). Wraps the
# package functions cluster_features() / choose_k_f0() / cluster_f0() /
# cluster_agreement(). Publishes candidate tone labels to `cluster_data` so
# Curate / Model can pick them up.
###############################################

cluster_ui <- function(input, output, session, dataset, normalised_data,
                        curated_data = NULL, cluster_data) {

  has_mclust <- requireNamespace("mclust", quietly = TRUE)

  # Cluster colour palette: ColorBrewer "Set3" (the same set the Visualise tab
  # uses, so the tabs feel consistent). Hard-coded here (rather than via
  # RColorBrewer::brewer.pal) so it is available regardless of package load
  # order. 12 colours = the maximum number of groups the slider allows.
  .cluster_pal <- c("#8DD3C7", "#FFED6F", "#BEBADA", "#FB8072", "#80B1D3",
                    "#FDB462", "#B3DE69", "#FCCDE5", "#BC80BD", "#CCEBC5",
                    "#D9D9D9", "#FFFFB3")
  cluster_colour_scale <- function(...)
    ggplot2::scale_colour_manual(values = .cluster_pal, na.value = "#9aa5ad", ...)

  # Friendly algorithm names for plot titles (avoid raw "hclust" etc.).
  .method_label <- c(kmeans = "k-means", hclust = "hierarchical",
                     gmm = "Gaussian mixture")
  method_label <- function(m) {
    lab <- unname(.method_label[m]); if (length(lab) != 1 || is.na(lab)) m else lab
  }

  # Y-axis label reflecting the chosen f0 variable's scale (f with subscript 0).
  f0_axis_label <- function(v) {
    v <- tolower(v %||% "")
    if (grepl("semitone|_st$|_st[^a-z]", v))        "mean f₀ (semitone)"
    else if (grepl("zscore|z[._-]?score|norm", v))  "mean f₀ (normalised)"
    else if (grepl("(^|[^a-z])hz", v))              "mean f₀ (Hz)"
    else                                            "mean f₀"
  }

  # Shared larger-type theme so ticks / titles on the result plots are legible.
  cluster_plot_theme <- function(base = 14)
    ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      axis.text     = ggplot2::element_text(size = 13),
      axis.title    = ggplot2::element_text(size = 14),
      plot.title    = ggplot2::element_text(size = 15, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12.5),
      legend.text   = ggplot2::element_text(size = 12),
      strip.text    = ggplot2::element_text(size = 13, face = "bold"))

  # Build a PNG download handler for a reactive ggplot object.
  cl_dl_plot <- function(obj, stem, w = 9, h = 5)
    downloadHandler(
      filename = function() sprintf("%s_%s.png", input$dataset_name %||% "cluster", stem),
      content  = function(file)
        ggplot2::ggsave(file, plot = obj(), width = w, height = h, dpi = 150, bg = "white"))

  # --- active dataset (uploaded / normalised / curated) ---
  active_data <- reactive({
    has_norm <- !is.null(normalised_data())
    has_cur  <- !is.null(curated_data) && !is.null(curated_data())
    sel <- input$cluster_dataset
    if (!is.null(sel) && sel == "curated" && has_cur) curated_data()
    else if (!is.null(sel) && sel == "normalised" && has_norm) normalised_data()
    else dataset()
  })

  # Remember the dataset choice in a reactiveVal so the sidebar re-render (which
  # fires when a normalised/curated dataset first appears, and would otherwise
  # reset the dropdown to "uploaded") cannot clobber it. Default to the
  # normalised dataset as soon as it exists, since clustering raw Hz mostly
  # separates speakers by pitch range.
  ds_choice <- reactiveVal(NULL)
  observeEvent(input$cluster_dataset, ds_choice(input$cluster_dataset), ignoreInit = TRUE)
  observeEvent(normalised_data(), {
    if (!is.null(normalised_data()) &&
        (is.null(ds_choice()) || identical(ds_choice(), "uploaded")))
      ds_choice("normalised")
  }, ignoreNULL = TRUE)

  # --- guide ---
  output$cluster_guide <- renderUI({
    guide_box("Tone clustering guide",
      tags$p(style = "margin: 4px 0 8px 0;", HTML(paste(
        "Don't know how many tones a language has? Group tokens by the",
        "<strong>shape</strong> of their f0 contour and let the data reveal the number",
        "of categories. This is a <strong>hypothesis generator</strong> (Kaland, 2023a),",
        "not a verdict: clusters can reflect speaker, syllable structure, or recording effects",
        "as well as tone, so confirm the result with linguistic analysis.",
        "Cluster speaker-normalised f0 (semitone or z-score, from the <strong>Normalise</strong> tab):",
        "on raw Hz, clusters tend to separate speakers by pitch range rather than tones."))),
      tags$p(style = "margin: 4px 0 2px 0; font-size:0.85rem; color:#444; font-weight:700;",
             "What you do here"),
      tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
        tags$li(HTML("<strong>Choose a feature space.</strong> Each contour is summarised as resampled <em>points</em>, compact <em>Legendre</em> / <em>DCT</em> coefficients, or its <em>derivative</em> (movement, discarding height; Kaland, 2023a): this is what gets clustered. Keep the <em>register</em> so high and low level tones separate, or cluster on <em>shape</em> alone.")),
        tags$li(HTML("<strong>Cluster without labels.</strong> An <em>unsupervised</em> algorithm (k-means, hierarchical, or Gaussian mixture) groups tokens purely by contour shape, with no tone labels needed.")),
        tags$li(HTML("<strong>Decide how many groups.</strong> The diagnostics (elbow, silhouette, gap, GMM/BIC, MDL) rarely agree exactly, so read off a plausible <em>range</em>, <em>compare</em> candidate solutions side by side, then set the number of groups with the slider.")),
        tags$li(HTML("<strong>Inspect, validate, reuse.</strong> Read the candidate-mean contours, the dendrogram (its merge-height axis is square-root scaled so the early, low merges stay legible) and the token map; if you have provisional labels, check agreement (adjusted Rand index); then send the clusters on as candidate tone labels for Curate / Model."))
      ),
      tags$p(style = "font-size:0.85rem; color:#444; margin:10px 0 2px 0; font-weight:700;", "Methods"),
      tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
        tags$li(HTML("<strong>Contour features.</strong> Each contour is time-normalised and resampled to a fixed number of points; the <em>derivative</em> (rate-of-change) representation discards height and clusters by movement (Kaland, 2023a), which best matches perceived contour similarity (Kaland, 2023b). <em>Legendre</em> and <em>DCT</em> coefficients are compact orthogonal-basis summaries of contour shape.")),
        tags$li(HTML("<strong>Clustering algorithms.</strong> k-means (Hartigan &amp; Wong, 1979), hierarchical agglomerative clustering with Ward linkage (Ward, 1963), and Gaussian mixture models selected by BIC (Scrucca et al., 2016).")),
        tags$li(HTML("<strong>Number of groups.</strong> The silhouette (Rousseeuw, 1987), gap statistic (Tibshirani et al., 2001) and a minimum-description-length information cost (Kaland &amp; Ellison, 2023) each suggest a value of k.")),
        tags$li(HTML("<strong>Token map.</strong> Tokens are projected to two dimensions with PCA (Jolliffe, 2002) or UMAP (McInnes et al., 2018)."))
      ),
      # --- Collapsible illustrated guide: clustering by landmark unit ---
      tags$details(class = "msg-route",
        tags$style(HTML("
          details.msg-route{background:#f3f8fc;border:1px solid #cfe2f1;border-radius:8px;padding:7px 14px 11px;margin:12px 0 4px;}
          .msg-route>summary{cursor:pointer;font-weight:700;color:#2c5d80;font-size:0.92rem;list-style:none;padding:1px 0;}
          .msg-route>summary::-webkit-details-marker{display:none;}
          .msg-route>summary::before{content:'\\25B8';color:#5b9bd5;display:inline-block;margin-right:8px;transition:transform .15s ease;}
          .msg-route[open]>summary::before{transform:rotate(90deg);}
          .msg-hint{color:#7aa6cc;font-weight:400;font-size:0.78rem;margin-left:6px;}
          .msg-route[open] .msg-hint{display:none;}
          .msg-intro{color:#3f5a72;font-size:0.83rem;line-height:1.5;margin:9px 0 0;}
          .cl-illus{margin:11px 0 3px;}
          .cl-illus svg{width:100%;height:auto;display:block;}
          .msg-flow{display:flex;align-items:stretch;gap:9px;margin-top:11px;flex-wrap:wrap;}
          .msg-step{flex:1 1 165px;background:#fff;border:1px solid #e1e9f2;border-radius:7px;padding:8px 12px;}
          .msg-step-here{border-color:#78c2ad;box-shadow:0 0 0 2px rgba(120,194,173,0.18);}
          .msg-shead{display:flex;align-items:center;gap:7px;margin-bottom:3px;flex-wrap:wrap;}
          .msg-badge{width:20px;height:20px;border-radius:50%;flex-shrink:0;display:inline-flex;align-items:center;justify-content:center;font-size:0.72rem;font-weight:700;color:#fff;}
          .msg-num{background:#aab9c6;}
          .msg-here{background:#d9534f;font-size:0.5rem;}
          .msg-stitle{font-weight:700;color:#2c5f4f;font-size:0.86rem;}
          .msg-tab{display:inline-block;background:#e8f5f0;color:#2c5f4f;padding:1px 7px;border-radius:10px;font-size:0.66rem;font-weight:600;font-family:'SFMono-Regular',Menlo,Consolas,monospace;white-space:nowrap;}
          .msg-swhy{font-size:0.76rem;color:#5f6b66;line-height:1.4;margin-top:2px;}
          .msg-arrow{display:flex;align-items:center;color:#9fbbd6;font-size:1.3rem;}
          @media (max-width:760px){.msg-arrow{display:none;}}
          .msg-tip{font-size:0.78rem;color:#33536f;background:#eaf3fb;border:1px solid #d3e6f5;border-radius:6px;padding:6px 11px;margin-top:11px;line-height:1.5;}
          .msg-tip .fa,.msg-tip svg{color:#5b9bd5;margin-right:4px;}
        ")),
        tags$summary(icon("layer-group"), " Clustering multisyllabic words, or just the vowel or rhyme?",
                     tags$span(class = "msg-hint", "(click to expand)")),
        tags$p(class = "msg-intro",
          "Cluster whatever unit you mark in a TextGrid. Prepare it once, then the time column you pick here decides what gets clustered and how it lines up."),
        tags$div(class = "cl-illus", HTML('<svg width="100%" viewBox="0 0 680 172" role="img" xmlns="http://www.w3.org/2000/svg"><title>Raw time, landmark _tseq, and landmark _t01 for clustering</title><desc>Raw time leaves boundaries misaligned; _tseq lays a whole word out in sequence; _t01 puts each part in its own 0 to 1 box.</desc><line x1="235" y1="12" x2="259" y2="12" stroke="#3a7ca5" stroke-width="2.5"/><text x="263" y="16" font-size="12" fill="#1f4e6b">token A</text><line x1="335" y1="12" x2="359" y2="12" stroke="#2f9e79" stroke-width="2.5"/><text x="363" y="16" font-size="12" fill="#1c5d47">token B</text><text x="120" y="36" font-size="13" font-weight="600" fill="#2c5d80" text-anchor="middle">Raw time</text><text x="315" y="36" font-size="13" font-weight="600" fill="#2c5d80" text-anchor="middle">&lt;tier&gt;_tseq</text><text x="530" y="36" font-size="13" font-weight="600" fill="#2c5d80" text-anchor="middle">&lt;tier&gt;_t01</text><rect x="40" y="46" width="160" height="82" fill="none" stroke="#b8d2e8" stroke-width="1"/><polyline points="40,104 80,72 120,62 160,98 200,116" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="40,108 96,74 152,64 176,100 200,114" fill="none" stroke="#2f9e79" stroke-width="2.5"/><line x1="120" y1="46" x2="120" y2="128" stroke="#3a7ca5" stroke-width="1.5" stroke-dasharray="4,3" opacity="0.7"/><line x1="152" y1="46" x2="152" y2="128" stroke="#2f9e79" stroke-width="1.5" stroke-dasharray="4,3" opacity="0.7"/><text x="120" y="148" font-size="12" fill="#5f6b66" text-anchor="middle">boundaries wander</text><rect x="235" y="46" width="160" height="82" fill="none" stroke="#b8d2e8" stroke-width="1"/><polyline points="235,104 275,72 315,62 355,98 395,116" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="235,108 275,75 315,65 355,101 395,114" fill="none" stroke="#2f9e79" stroke-width="2.5"/><line x1="315" y1="46" x2="315" y2="128" stroke="#9fbbd6" stroke-width="1.5" stroke-dasharray="4,3"/><text x="275" y="123" font-size="11" fill="#5f6b66" text-anchor="middle">&#963;1</text><text x="355" y="123" font-size="11" fill="#5f6b66" text-anchor="middle">&#963;2</text><text x="315" y="148" font-size="12" fill="#5f6b66" text-anchor="middle">whole word, in sequence</text><rect x="430" y="46" width="95" height="82" fill="none" stroke="#b8d2e8" stroke-width="1"/><rect x="535" y="46" width="95" height="82" fill="none" stroke="#b8d2e8" stroke-width="1"/><polyline points="430,102 454,84 478,72 502,64 525,60" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="430,106 454,88 478,76 502,68 525,64" fill="none" stroke="#2f9e79" stroke-width="2.5"/><polyline points="535,62 559,82 583,98 607,110 630,116" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="535,66 559,86 583,101 607,112 630,118" fill="none" stroke="#2f9e79" stroke-width="2.5"/><text x="477" y="123" font-size="11" fill="#5f6b66" text-anchor="middle">&#963;1</text><text x="582" y="123" font-size="11" fill="#5f6b66" text-anchor="middle">&#963;2</text><text x="530" y="148" font-size="12" fill="#5f6b66" text-anchor="middle">each part, own 0&#8211;1</text></svg>')),
        tags$div(class = "msg-flow",
          tags$div(class = "msg-step",
            tags$div(class = "msg-shead",
              tags$span(class = "msg-badge msg-num", "1"),
              tags$span(class = "msg-stitle", "Prepare TextGrid")),
            tags$div(class = "msg-swhy",
              "Before the app, mark your unit (a word, syllable, vowel, or rhyme) as an interval tier.")),
          tags$div(class = "msg-arrow", HTML("&#10132;")),
          tags$div(class = "msg-step",
            tags$div(class = "msg-shead",
              tags$span(class = "msg-badge msg-num", "2"),
              tags$span(class = "msg-stitle", "Extract"),
              tags$span(class = "msg-tab", "F0 Extraction")),
            tags$div(class = "msg-swhy",
              "Pick that tier to attach per-part timestamps to every f0 frame.")),
          tags$div(class = "msg-arrow", HTML("&#10132;")),
          tags$div(class = "msg-step",
            tags$div(class = "msg-shead",
              tags$span(class = "msg-badge msg-num", "3"),
              tags$span(class = "msg-stitle", "Normalise time"),
              tags$span(class = "msg-tab", "Normalise")),
            tags$div(class = "msg-swhy",
              HTML("Normalise by the tier: adds <code>&lt;tier&gt;_t01</code> (each part 0&ndash;1) and <code>&lt;tier&gt;_tseq</code> (whole word)."))),
          tags$div(class = "msg-arrow", HTML("&#10132;")),
          tags$div(class = "msg-step msg-step-here",
            tags$div(class = "msg-shead",
              tags$span(class = "msg-badge msg-here", HTML("&#9679;")),
              tags$span(class = "msg-stitle", "Cluster")),
            tags$div(class = "msg-swhy",
              HTML("You are here. Set the <strong>Time variable</strong> to <code>&lt;tier&gt;_tseq</code> for the whole word, or <code>&lt;tier&gt;_t01</code> for a single unit.")))
        ),
        tags$div(class = "msg-tip",
          icon("lightbulb"), HTML(" Cluster words with the same number of syllables together (disyllabic apart from trisyllabic), so the boundary lands in the same place."))
      ),
      tags$details(style = "margin-top: 6px;",
        tags$summary(style = "cursor:pointer; font-size:0.82rem; color:#4a7868; font-weight:600;",
                     "References"),
        tags$ol(style = "margin: 6px 0 0 0; padding-left: 20px; font-size: 0.76rem; color:#666; line-height:1.55;",
          tags$li(HTML("Hartigan, J. A., &amp; Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. <em>Journal of the Royal Statistical Society: Series C (Applied Statistics)</em>, 28(1), 100&ndash;108.")),
          tags$li(HTML("Jolliffe, I. T. (2002). <em>Principal Component Analysis</em> (2nd ed.). Springer.")),
          tags$li(HTML("Kaland, C. (2023a). Contour clustering: A field-data-driven approach for documenting and analysing prototypical f0 contours. <em>Journal of the International Phonetic Association</em>, 53(1), 159&ndash;188.")),
          tags$li(HTML("Kaland, C. (2023b). Intonation contour similarity: f0 representations and distance measures compared to human perception in two languages. <em>The Journal of the Acoustical Society of America</em>, 154(1), 95&ndash;107.")),
          tags$li(HTML("Kaland, C., &amp; Ellison, T. M. (2023). Evaluating cluster analysis on f0 contours: An information theoretic approach on three languages. In <em>Proceedings of the 20th International Congress of Phonetic Sciences</em> (pp. 3448&ndash;3452).")),
          tags$li(HTML("McInnes, L., Healy, J., &amp; Melville, J. (2018). UMAP: Uniform manifold approximation and projection for dimension reduction. <em>arXiv:1802.03426</em>.")),
          tags$li(HTML("Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. <em>Journal of Computational and Applied Mathematics</em>, 20, 53&ndash;65.")),
          tags$li(HTML("Scrucca, L., Fop, M., Murphy, T. B., &amp; Raftery, A. E. (2016). mclust 5: Clustering, classification and density estimation using Gaussian finite mixture models. <em>The R Journal</em>, 8(1), 289&ndash;317.")),
          tags$li(HTML("Tibshirani, R., Walther, G., &amp; Hastie, T. (2001). Estimating the number of clusters in a data set via the gap statistic. <em>Journal of the Royal Statistical Society: Series B (Statistical Methodology)</em>, 63(2), 411&ndash;423.")),
          tags$li(HTML("Ward, J. H. (1963). Hierarchical grouping to optimize an objective function. <em>Journal of the American Statistical Association</em>, 58(301), 236&ndash;244."))
        )
      )
    )
  })

  # --- sidebar ---
  output$ui_cluster <- renderUI({
    has_norm <- !is.null(normalised_data())
    has_cur  <- !is.null(curated_data) && !is.null(curated_data())
    ds_choices <- c("Uploaded data" = "uploaded")
    if (has_norm) ds_choices <- c(ds_choices, "Normalised data" = "normalised")
    if (has_cur)  ds_choices <- c(ds_choices, "Curated data" = "curated")
    ds_selected <- if (!is.null(ds_choice()) && ds_choice() %in% ds_choices) ds_choice()
                   else if (has_norm) "normalised" else "uploaded"

    active <- active_data()
    vars <- if (!is.null(active)) names(active) else c("No dataset available")
    var_types <- paste0(vars, " {", if (!is.null(active)) sapply(active, class) else "NA", "}")

    methods <- c("k-means" = "kmeans", "Hierarchical (Ward)" = "hclust")
    if (has_mclust) methods <- c(methods, "Gaussian mixture (BIC)" = "gmm")

    ds_label <- names(ds_choices)[match(ds_selected, ds_choices)]
    if (length(ds_label) != 1 || is.na(ds_label)) ds_label <- "uploaded data"

    tagList(
      wellPanel(
        # Data + variable mapping is collapsed by default so the main controls
        # (features, method, k, run) sit near the top without scrolling. The
        # summary shows which dataset is active so it is clear when closed.
        tags$details(class = "cluster-data-group", style = "margin-bottom:4px;",
          tags$summary(style = "cursor:pointer; padding:2px 0; list-style:revert;",
                       tags$h5(style = "display:inline-block; margin:0;",
                               "Data & variables ",
                               tags$span(style = "color:#8a9aa3; font-weight:400; font-size:0.8rem;",
                                         sprintf("(%s)", ds_label)))),
          tags$div(style = "margin-top:8px;",
            selectInput("cluster_dataset", "Select dataset:", choices = ds_choices, selected = ds_selected),
            selectInput("cluster_token_var", "Token ID variable:",
                        choices = setNames(vars, var_types), selected = guess_var(vars, var_patterns$token, 1)),
            selectInput("cluster_f0_var", "Normalised f0 variable:",
                        choices = setNames(vars, var_types), selected = guess_var(vars, var_patterns$f0, 2)),
            selectInput("cluster_time_var", "Time variable:",
                        choices = setNames(vars, var_types), selected = guess_var(vars, var_patterns$time, 3)),
            tags$div(style = "color:#888; font-size:0.72rem; margin-top:-6px;",
                     "Normalised time by a tier? Pick that column here (e.g. ",
                     tags$code("<tier>_tseq"),
                     "). See the multisyllabic guide for the full workflow."),
            selectInput("cluster_speaker_var", "Speaker variable (optional):",
                        choices = setNames(vars, var_types), selected = guess_var(vars, var_patterns$speaker, 4)),
            selectInput("cluster_tone_var", "Reference tone labels (optional, for validation):",
                        choices = c("(none)" = "__none__", setNames(vars, var_types)),
                        selected = "__none__"),
            tags$div(style = "color:#888; font-size:0.72rem; margin-top:-6px;",
                     "Known categories or your own auditory judgements. Used only to score the clusters, never to build them."))),
        tags$hr(),
        h5("Contour features"),
        selectInput("cluster_features", "Represent each contour as:",
                    choices = c("Contour points" = "points", "Legendre coefficients" = "legendre",
                                "DCT coefficients" = "dct", "Derivative (movement)" = "derivative"),
                    selected = "legendre"),
        conditionalPanel("input.cluster_features == 'legendre' || input.cluster_features == 'dct'",
          sliderInput("cluster_degree", "Coefficient order:", min = 2, max = 6, value = 4, step = 1)),
        sliderInput("cluster_npoints", "Resample points per contour:", min = 5, max = 41, value = 21, step = 1),
        tags$div(style = "color:#888; font-size:0.72rem; margin-top:-6px;",
                 "Each contour is resampled to this many equally spaced points so all tokens are comparable. If your f0 is already measured at a fixed number of equidistant points (commonly 11 or 21), set this to that count for an exact, lossless match."),
        radioButtons("cluster_register", "Cluster on:",
                     choices = c("Shape + register (height matters)" = "level",
                                 "Shape only (centre each contour)" = "shape"), selected = "level"),
        tags$hr(),
        h5("Clustering method"),
        selectInput("cluster_method", "Algorithm:", choices = methods, selected = "kmeans"),
        conditionalPanel("input.cluster_method == 'hclust'",
          selectInput("cluster_linkage", "Linkage:",
                      choices = c("Ward" = "ward.D2", "Complete" = "complete",
                                  "Average" = "average", "Single" = "single"),
                      selected = "ward.D2")),
        tags$hr(),
        h5("How many tones?"),
        radioButtons("cluster_kmode", NULL,
                     choices = c("Let the data suggest it" = "auto", "Set it myself" = "manual"),
                     selected = "auto"),
        conditionalPanel("input.cluster_kmode == 'manual'",
          sliderInput("cluster_k", "Use this many tones (k):", min = 2, max = 12, value = 5, step = 1)),
        sliderInput("cluster_kmax", "Diagnostics: evaluate up to:", min = 4, max = 12, value = 8, step = 1),
        sliderInput("cluster_bending", "MDL bending factor:", min = 1, max = 30, value = 1, step = 1),
        tags$div(style = "color:#888; font-size:0.72rem; margin-top:-6px;",
                 "For the MDL criterion (Kaland & Ellison 2023): raise it until the MDL curve forms a clear dip. Updates live."),
        tags$hr(),
        actionButton("cluster_run", "Run clustering", icon = icon("circle-nodes"), class = "btn-primary"),
        tags$hr(),
        h5("Use & download"),
        actionButton("cluster_use_labels", "Use clusters as candidate tone labels",
                     icon = icon("tag")),
        tags$div(style = "color:#888; font-size:0.72rem; margin-top:3px;",
                 "Adds a ", tags$code("cluster"), " column and publishes a dataset that Curate and Model can use."),
        div(style = "margin-top: 8px;",
            downloadButton("cluster_dl", "Download clustered CSV"))
      )
    )
  })

  # --- run clustering on demand ---
  cl_run <- eventReactive(input$cluster_run, {
    d <- active_data(); req(d)
    tok <- input$cluster_token_var; tm <- input$cluster_time_var; f0 <- input$cluster_f0_var
    spk <- input$cluster_speaker_var; tn <- input$cluster_tone_var
    validate(need(all(c(tok, tm, f0) %in% names(d)), "Pick valid Token, Time and f0 columns."))
    withProgress(message = "Clustering f0 contours", value = 0, {
      incProgress(0.15, detail = "building contour features")
      feat <- cluster_features(d, f0 = f0, token = tok, time = tm,
                               speaker = if (!is.null(spk) && spk %in% names(d)) spk else NULL,
                               tone = if (!is.null(tn) && tn != "__none__" && tn %in% names(d)) tn else NULL,
                               n_points = input$cluster_npoints,
                               features = input$cluster_features,
                               degree = input$cluster_degree %||% 4,
                               register = input$cluster_register)
      incProgress(0.30, detail = "estimating the number of tones")
      diag <- choose_k_f0(feat, k_range = 2:input$cluster_kmax)
      k <- if (input$cluster_kmode == "manual") as.integer(input$cluster_k) else {
        if (!is.na(diag$k_gmm)) diag$k_gmm
        else if (!is.na(diag$k_silhouette)) diag$k_silhouette
        else if (!is.na(diag$k_gap)) diag$k_gap else 3L
      }
      incProgress(0.85, detail = "assigning clusters")
      res <- cluster_f0(feat, method = input$cluster_method, k = k,
                        hclust_method = input$cluster_linkage %||% "ward.D2")
      list(feat = feat, diag = diag, res = res, k = k, token = tok,
           linkage = input$cluster_linkage %||% "ward.D2")
    })
  })

  # --- chosen number of groups -------------------------------------------
  # The run picks a starting k (auto suggestion or the manual slider), but the
  # diagnostics rarely agree, so the result panel exposes a live "Number of
  # groups" slider. final_k()/final_res() re-cut the SAME features + method to
  # the chosen k, and the contours, dendrogram, token map, names, validation
  # and published labels all follow it. Defaults to the run's k (no recompute).
  final_k <- reactive({
    r <- cl_run()
    k <- input$cluster_ngroups
    if (is.null(k)) r$k else as.integer(k)
  })
  final_res <- reactive({
    r <- cl_run(); k <- final_k()
    if (length(k) != 1 || is.na(k) || k == r$k) return(r$res)   # reuse the run
    cluster_f0(r$feat, method = r$res$method, k = k,
               hclust_method = r$linkage %||% "ward.D2")
  })

  # The 2-D projection depends only on the features + projection choice, not on
  # k, so cache it separately. Re-cutting the groups then only recolours points
  # (no costly UMAP / PCA recompute on every slider nudge).
  cluster_embedding <- reactive({
    r <- cl_run(); X <- r$feat$features
    proj <- input$cluster_projection %||% "pca"
    if (proj == "umap" && requireNamespace("uwot", quietly = TRUE) && nrow(X) > 5) {
      nn <- max(2L, min(15L, nrow(X) - 1L))
      emb <- withProgress(message = "Computing UMAP", value = 0.4, {
        set.seed(1); uwot::umap(X, n_neighbors = nn, min_dist = 0.1,
                                n_components = 2, verbose = FALSE) })
      list(D1 = emb[, 1], D2 = emb[, 2], xl = "UMAP-1", yl = "UMAP-2",
           tokens = r$feat$tokens)
    } else {
      pca <- stats::prcomp(X, scale. = FALSE)
      ve <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
      list(D1 = pca$x[, 1], D2 = pca$x[, 2],
           xl = sprintf("PC1 (%.1f%%)", ve[1]), yl = sprintf("PC2 (%.1f%%)", ve[2]),
           tokens = r$feat$tokens)
    }
  })

  # --- live MDL curve: recomputed as the bending slider changes, without
  # re-running the slow gap / GMM diagnostics ---
  mdl_live <- reactive({
    r <- cl_run()
    m <- cluster_mdl(r$feat$contours, r$diag$assignments, bending = input$cluster_bending %||% 1)
    data.frame(k = r$diag$table$k, mdl = unname(m))
  })

  # --- suggested-k summary ---
  output$cluster_suggest <- renderUI({
    r <- cl_run(); dg <- r$diag
    ml <- mdl_live(); k_mdl <- if (any(is.finite(ml$mdl))) ml$k[which.min(ml$mdl)] else NA
    chip <- function(lab, v) tags$span(
      style = "display:inline-block; background:#eef4fb; border:1px solid #cfe2f1; color:#2c5d80; border-radius:10px; padding:2px 10px; margin-right:6px; font-size:0.82rem;",
      HTML(sprintf("%s: <strong>%s</strong>", lab, if (is.na(v)) "n/a" else v)))
    tags$div(style = "margin: 4px 0 10px 0;",
      tags$strong("Suggested number of tones: "),
      chip("silhouette", dg$k_silhouette), chip("gap", dg$k_gap),
      chip("GMM/BIC", dg$k_gmm), chip("MDL", k_mdl),
      tags$span(style = "color:#555; font-size:0.85rem;",
        sprintf(" (currently showing %d groups; adjust below).", final_k())))
  })

  # --- diagnostics plot (elbow / silhouette / gap vs k) ---
  # --- nudge to normalise (clustering raw Hz mostly separates speakers) ---
  output$cluster_norm_nudge <- renderUI({
    sel <- input$cluster_dataset
    if (!is.null(sel) && sel == "normalised") return(NULL)
    has_norm <- !is.null(normalised_data())
    msg <- if (has_norm)
      "A normalised dataset is available. Select \"Normalised data\" above so clusters reflect tone shape, not each speaker's pitch range."
    else
      "Tip: cluster speaker-normalised f0 (Normalise tab, semitone or z-score). On raw Hz, clusters often just separate speakers by pitch range."
    tags$div(style = "background:#fff8e1; border-left:4px solid #e0a800; padding:6px 11px; border-radius:5px; font-size:0.82rem; color:#8a6d00; margin-bottom:10px;",
      HTML("&#9888; "), msg)
  })

  diag_plot_obj <- reactive({
    dg <- cl_run()$diag$table; ml <- mdl_live()
    long <- rbind(
      data.frame(k = dg$k, value = dg$wss,        metric = "Elbow (lower better)"),
      data.frame(k = dg$k, value = dg$silhouette, metric = "Silhouette (higher better)"),
      data.frame(k = dg$k, value = dg$gap,        metric = "Gap (higher better)"),
      data.frame(k = ml$k, value = ml$mdl,        metric = "MDL cost (lower better)"))
    long <- long[is.finite(long$value), , drop = FALSE]
    long$metric <- factor(long$metric, levels = c(
      "Elbow (lower better)", "Silhouette (higher better)",
      "Gap (higher better)", "MDL cost (lower better)"))
    ggplot2::ggplot(long, ggplot2::aes(x = .data$k, y = .data$value)) +
      ggplot2::geom_line(colour = "#2c5f4f", linewidth = 0.8) +
      ggplot2::geom_point(colour = "#2c5f4f", size = 2.4) +
      ggplot2::facet_wrap(~ metric, scales = "free_y", ncol = 2) +
      ggplot2::scale_x_continuous(breaks = dg$k) +
      ggplot2::labs(x = "number of clusters (k)", y = NULL) +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::theme(
        strip.text  = ggplot2::element_text(size = 12.5, face = "bold"),
        axis.text   = ggplot2::element_text(size = 13),
        axis.title  = ggplot2::element_text(size = 14),
        panel.spacing = ggplot2::unit(1.2, "lines"))
  })
  output$cluster_diag_plot <- renderPlot({ diag_plot_obj() })
  output$cluster_diag_dl <- cl_dl_plot(diag_plot_obj, "diagnostics", w = 10, h = 6)

  # --- cluster-mean contours (the candidate prototypical tones) ---
  mean_plot_obj <- reactive({
    res <- final_res(); k <- final_k(); cm <- res$cluster_means
    np <- ncol(cm); xs <- seq(0, 1, length.out = np)
    # Colour by cluster number so a cluster keeps the same colour here and in
    # the token map; the legend is relabelled with the cluster size.
    labs <- sprintf("Cluster %d (n=%d)", seq_len(nrow(cm)), res$sizes)
    long <- do.call(rbind, lapply(seq_len(nrow(cm)), function(i)
      data.frame(x = xs, f0 = cm[i, ],
                 cluster = factor(i, levels = seq_len(nrow(cm))))))
    ggplot2::ggplot(long, ggplot2::aes(x = .data$x, y = .data$f0,
                                       colour = .data$cluster, group = .data$cluster)) +
      ggplot2::geom_line(linewidth = 1.2) +
      cluster_colour_scale(name = NULL, labels = labs) +
      ggplot2::labs(x = "normalised time", y = f0_axis_label(input$cluster_f0_var),
                    title = sprintf("%d candidate tone contours (%s)", k, method_label(res$method))) +
      cluster_plot_theme(14)
  })
  output$cluster_mean_plot <- renderPlot({ mean_plot_obj() })
  output$cluster_mean_dl <- cl_dl_plot(mean_plot_obj, "candidate_contours", w = 9, h = 5)

  # --- compare candidate k (small multiples; uses the CHOSEN method) ---
  # Reacts to the compare-range slider, so the user can line up e.g. 6, 7, 8
  # whatever the committed solution is.
  compare_plot_obj <- reactive({
    r <- cl_run(); feat <- r$feat; req(feat)
    meth <- input$cluster_method %||% "kmeans"
    rng <- input$cluster_compare_range %||% c(4, 7)
    kset <- seq.int(rng[1], rng[2])
    kset <- kset[kset >= 2 & kset < length(feat$tokens)]
    kset <- utils::head(kset, 6L)                 # cap facets
    validate(need(length(kset) >= 1, "Choose a comparison range."))
    cp <- withProgress(message = "Comparing solutions", value = 0, {
      do.call(rbind, lapply(seq_along(kset), function(j) {
        incProgress(1 / length(kset))
        cm <- cluster_f0(feat, method = meth, k = kset[j],
                         hclust_method = input$cluster_linkage %||% "ward.D2")$cluster_means
        xs <- seq(0, 1, length.out = ncol(cm))
        do.call(rbind, lapply(seq_len(nrow(cm)), function(i)
          data.frame(kk = kset[j], cluster = factor(i), x = xs, f0 = cm[i, ])))
      }))
    })
    cp$k <- factor(sprintf("%d tones", cp$kk),
                   levels = sprintf("%d tones", sort(unique(cp$kk))))
    ggplot2::ggplot(cp, ggplot2::aes(x = .data$x, y = .data$f0,
                                     colour = .data$cluster, group = .data$cluster)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::facet_wrap(~ k, nrow = 1) +
      ggplot2::scale_x_continuous(breaks = c(0, 0.5, 1)) +
      cluster_colour_scale(guide = "none") +
      ggplot2::labs(x = "normalised time", y = f0_axis_label(input$cluster_f0_var),
                    subtitle = sprintf("%s solution per number of tones (drag the compare slider to change)",
                                       method_label(meth))) +
      cluster_plot_theme(13)
  })
  output$cluster_compare_plot <- renderPlot({ compare_plot_obj() })
  output$cluster_compare_dl <- cl_dl_plot(compare_plot_obj, "compare_solutions", w = 11, h = 4.5)

  # --- GMM membership confidence ---
  output$cluster_confidence <- renderUI({
    u <- final_res()$uncertainty
    if (is.null(u)) return(NULL)
    n_amb <- sum(u > 0.4)
    tags$div(style = "color:#8a6d00; font-size:0.85rem; margin:4px 0 0 0;",
      sprintf("GMM membership confidence: %d of %d token(s) (%.0f%%) sit ambiguously between clusters (posterior < 60%%); mean uncertainty %.2f. Treat these as borderline.",
              n_amb, length(u), 100 * n_amb / length(u), mean(u)))
  })

  # --- interactive dendrogram (hierarchical method only) ----------------
  # Built directly from the hclust merge tree (no extra dependency): leaf and
  # node coordinates come from $merge / $height / $order, drawn as plotly line
  # traces coloured by the k-cut (same cluster colours as the other plots).
  # Hovering a leaf shows its token + cluster; hovering an internal node shows
  # how many tokens sit below it.
  output$cluster_dendro_plot <- plotly::renderPlotly({
    r <- cl_run(); tree <- r$res$tree; k <- final_k(); res <- final_res()
    validate(need(!is.null(tree), "Dendrogram is only available for the hierarchical method."))
    tokens <- r$feat$tokens
    n <- length(tree$order)
    grp <- unname(res$assignment[tokens])              # cluster id per leaf/row
    leaf_x <- numeric(n); leaf_x[tree$order] <- seq_len(n)
    mg <- tree$merge; ht <- tree$height
    node_x <- numeric(n - 1); node_g <- rep(NA_integer_, n - 1); node_sz <- integer(n - 1)
    cx <- function(i) if (i < 0) leaf_x[-i] else node_x[i]
    cy <- function(i) if (i < 0) 0 else ht[i]
    cg <- function(i) if (i < 0) grp[-i] else node_g[i]
    csz <- function(i) if (i < 0) 1L else node_sz[i]
    pal <- .cluster_pal
    colf <- function(g) if (is.na(g)) "#c2cad1" else pal[((g - 1) %% length(pal)) + 1]
    segx <- list(); segy <- list()
    addseg <- function(key, x0, y0, x1, y1) {
      segx[[key]] <<- c(segx[[key]], x0, x1, NA); segy[[key]] <<- c(segy[[key]], y0, y1, NA) }
    for (m in seq_len(n - 1)) {
      a <- mg[m, 1]; b <- mg[m, 2]
      xa <- cx(a); xb <- cx(b); ya <- cy(a); yb <- cy(b); ga <- cg(a); gb <- cg(b)
      g <- if (!is.na(ga) && !is.na(gb) && ga == gb) ga else NA_integer_
      node_x[m] <- (xa + xb) / 2; node_g[m] <- g; node_sz[m] <- csz(a) + csz(b)
      h <- ht[m]; key <- if (is.na(g)) "trunk" else as.character(g)
      # Plot on a square-root height scale so the dense low merges (where the
      # cluster structure lives) are not crushed under the few very tall merges.
      addseg(key, xa, sqrt(ya), xa, sqrt(h)); addseg(key, xb, sqrt(yb), xb, sqrt(h))
      addseg(key, xa, sqrt(h), xb, sqrt(h))
    }
    p <- plotly::plot_ly()
    for (key in names(segx)) {
      g <- if (key == "trunk") NA_integer_ else as.integer(key)
      p <- plotly::add_trace(p, x = segx[[key]], y = segy[[key]], type = "scatter",
                             mode = "lines", line = list(color = colf(g), width = 1.3),
                             hoverinfo = "skip", showlegend = FALSE)
    }
    p <- plotly::add_trace(p, x = leaf_x, y = rep(0, n), type = "scatter", mode = "markers",
                           marker = list(size = 5, color = vapply(grp, colf, character(1))),
                           text = sprintf("%s (cluster %d)", tokens, grp),
                           hoverinfo = "text", showlegend = FALSE)
    p <- plotly::add_trace(p, x = node_x, y = sqrt(ht), type = "scatter", mode = "markers",
                           marker = list(size = 4, color = "rgba(90,107,120,0.30)"),
                           text = sprintf("%d tokens below (merge height %.2f)", node_sz, ht),
                           hoverinfo = "text", showlegend = FALSE)
    # Tick marks are placed at sqrt() positions but labelled with the real
    # merge heights, so the axis still reads in normal units.
    ycand <- c(0, 10, 25, 50, 100, 200, 400, 800, 1600, 3200)
    yt <- sort(unique(c(ycand[ycand <= max(ht)], round(max(ht)))))
    plotly::layout(p,
      title = list(text = sprintf("Hierarchical merge tree (%s linkage), cut into %d groups",
                                  r$linkage %||% "ward.D2", k), font = list(size = 15)),
      xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
      yaxis = list(title = "merge height (square-root scale)", zeroline = FALSE,
                   tickmode = "array", tickvals = sqrt(yt), ticktext = as.character(yt)),
      margin = list(t = 42))
  })

  # --- name the clusters (one text box per cluster) ---
  output$cluster_name_ui <- renderUI({
    res <- final_res(); k <- final_k()
    tags$div(style = "display:flex; flex-wrap:wrap; gap:10px; align-items:flex-end;",
      lapply(seq_len(k), function(i)
        tags$div(style = "display:flex; flex-direction:column;",
          tags$label(style = "font-size:0.74rem; color:#666;",
                     sprintf("Cluster %d (n=%d)", i, res$sizes[i])),
          textInput(paste0("cluster_name_", i), label = NULL,
                    value = sprintf("T%d", i), width = "110px"))))
  })

  # --- token map: PCA (linear) or UMAP (non-linear) projection ---
  # Embedding is cached (cluster_embedding); changing the number of groups only
  # recolours the points by the re-cut assignment, so UMAP is not recomputed.
  output$cluster_proj_plot <- plotly::renderPlotly({
    e <- cluster_embedding(); res <- final_res()
    df <- data.frame(D1 = e$D1, D2 = e$D2,
                     cluster = factor(res$assignment[e$tokens]),
                     token = e$tokens, stringsAsFactors = FALSE)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$D1, y = .data$D2,
                                          colour = .data$cluster, customdata = .data$token)) +
      ggplot2::geom_point(alpha = 0.7, size = 1.6) +
      cluster_colour_scale(name = "cluster") +
      ggplot2::labs(x = e$xl, y = e$yl) +
      ggplot2::theme_minimal(base_size = 13)
    plotly::ggplotly(p, tooltip = "customdata")
  })

  # --- agreement with known labels (if a tone column was chosen) ---
  output$cluster_agreement <- renderUI({
    r <- cl_run()
    if (is.null(r$feat$meta$tone)) {
      return(tags$div(style = "color:#888; font-size:0.85rem; font-style:italic;",
        "No reference tone column selected, so no validation is shown. (Pick one in the sidebar to compare clusters against existing labels.)"))
    }
    ag <- cluster_agreement(final_res()$assignment[r$feat$tokens], r$feat$meta$tone)
    interp <- if (ag$ari >= 0.7) "strong" else if (ag$ari >= 0.4) "moderate"
              else if (ag$ari >= 0.2) "weak-to-moderate" else "weak"
    tagList(
      tags$div(style = "margin-bottom:4px; font-size:0.95rem;",
        tags$strong("Agreement with your reference tones (Adjusted Rand Index): "),
        tags$span(style = "color:#2c5f4f; font-weight:700;",
                  sprintf("%.2f", ag$ari)),
        tags$span(style = "color:#555;", sprintf(" (%s)", interp))),
      tags$div(style = "font-size:0.8rem; color:#777; margin-bottom:8px;",
        HTML("ARI scores how well the clusters line up with your existing tone labels: <strong>1.0</strong> = identical, <strong>0</strong> = no better than chance. It is <em>not a grade</em>: <strong>weak-to-moderate values are normal and expected</strong> here, because tone categories often overlap in f0 shape, so unsupervised clusters only partly recover them. Use it to compare settings (features, k, register), and lean on the candidate-contour shapes and your own judgement.")),
      tags$div(style = "font-size:0.82rem; color:#555; margin-bottom:4px;",
        "Cross-tab below: rows = clusters, columns = your tone labels. A tidy near-diagonal means good recovery; spread-out rows mean tones share a cluster."),
      renderTable(as.data.frame.matrix(ag$table), rownames = TRUE)
    )
  })

  output$cluster_result_block <- renderUI({
    r <- cl_run(); req(r)
    kmax <- 12L                       # upper bound for the group-count slider
    # Each result group is wrapped in its own bordered section card so the long
    # panel reads as distinct steps (diagnostics, contours, compare, map,
    # naming, validation) rather than one continuous scroll.
    sec <- function(title, ic, ...) tags$div(
      style = "border:1px solid #e6ebf1; border-radius:8px; padding:12px 16px 16px; margin:16px 0; background:#ffffff;",
      tags$h4(style = "margin:2px 0 12px 0; font-size:1.3rem; font-weight:600; color:#2c4a5e;",
              icon(ic), " ", title),
      ...)
    dlbtn <- function(id) tags$div(style = "margin-top:8px;",
      downloadButton(id, "Download plot (PNG)", class = "btn-sm", icon = icon("download"),
                     style = "font-size:0.78rem; padding:3px 12px;"))
    tagList(
      uiOutput("cluster_norm_nudge"),
      uiOutput("cluster_suggest"),
      sec("How many tones? (cluster-count diagnostics)", "chart-line",
        plotOutput("cluster_diag_plot", height = "360px"),
        dlbtn("cluster_diag_dl"),
        tags$details(style = "background:#eef4fb; border:1px solid #cfe2f1; border-radius:6px; padding:6px 12px; margin:8px 0 0 0;",
          tags$summary(style = "cursor:pointer; font-weight:700; color:#2c5d80; font-size:0.85rem;",
                       icon("circle-info"), " How to read these diagnostics"),
          tags$ul(style = "font-size:0.82rem; color:#3f5a72; margin:8px 0 2px 0; padding-left:18px;",
            tags$li(HTML("<strong>Elbow</strong>: within-cluster spread always shrinks with more clusters; look for the bend where it stops dropping steeply.")),
            tags$li(HTML("<strong>Silhouette</strong>: how tight and well-separated clusters are; pick the peak (closer to 1 is better).")),
            tags$li(HTML("<strong>Gap</strong>: compares the clustering to random noise; the smallest k within one SE of the next is the choice.")),
            tags$li(HTML("<strong>GMM / BIC</strong>: model-based; the k with the best Bayesian Information Criterion.")),
            tags$li(HTML("<strong>MDL info-cost</strong> (Kaland &amp; Ellison 2023): total bits to describe the data; raise the <em>bending factor</em> on the left until the curve forms a clear dip, and the dip is the preferred k.")),
            tags$li(HTML("<strong>Why they disagree:</strong> elbow / silhouette / gap reward <em>well-separated blobs</em>, so when tone categories overlap in f0 shape they favour <em>fewer</em> clusters than the true linguistic count (e.g. silhouette = 2, gap = 3). BIC / MDL reward fit and lean higher. If you know roughly how many tones there are, set the number of groups below and judge by the candidate contours + the validation."))))),
      tags$div(style = "border:1px solid #cfe0d8; background:#f1f8f4; border-radius:8px; padding:12px 16px; margin:16px 0;",
        tags$div(style = "font-weight:600; color:#2c4a5e; font-size:1.05rem; margin-bottom:2px;",
                 icon("sliders"), " Number of groups (tones) to use"),
        tags$div(style = "color:#5a6b78; font-size:0.82rem; margin-bottom:6px;",
                 "Starts at the suggested number. Drag to re-cut: the candidate contours, dendrogram, token map, group names and the published labels all follow this."),
        sliderInput("cluster_ngroups", NULL, min = 2, max = kmax,
                    value = min(max(2L, r$k), kmax), step = 1, width = "340px")),
      sec("Candidate tone contours", "wave-square",
        plotOutput("cluster_mean_plot", height = "340px"),
        dlbtn("cluster_mean_dl"),
        uiOutput("cluster_confidence")),
      conditionalPanel("input.cluster_method == 'hclust'",
        sec("Dendrogram (hierarchical merge tree)", "sitemap",
          tags$p(style = "color:#666; font-size:0.82rem; margin:0 0 6px 0;",
            "Interactive merge tree over all tokens (leaf labels hidden). Tall vertical gaps suggest natural cut points; coloured branches are the current groups. The merge-height axis uses a square-root scale so the dense low merges stay legible. Hover a leaf to see its token and cluster, or a node to see how many tokens sit below it. Use the camera icon in the plot toolbar to download."),
          plotly::plotlyOutput("cluster_dendro_plot", height = "520px"))),
      sec("Compare candidate solutions", "layer-group",
        tags$p(style = "color:#666; font-size:0.85rem; margin:0 0 8px 0;",
          "Cluster-mean contours at several values of k side by side, using your chosen algorithm. Drag the range to line up e.g. 6, 7 and 8 tones."),
        sliderInput("cluster_compare_range", "Show side-by-side for k =",
                    min = 2, max = 12, value = c(4, 7), step = 1, width = "340px"),
        plotOutput("cluster_compare_plot", height = "230px"),
        dlbtn("cluster_compare_dl")),
      sec("Token map", "diagram-project",
        radioButtons("cluster_projection", NULL, inline = TRUE,
                     choices = if (requireNamespace("uwot", quietly = TRUE))
                       c("PCA (linear)" = "pca", "UMAP (non-linear)" = "umap")
                     else c("PCA (linear)" = "pca"),
                     selected = "pca"),
        tags$div(style = "color:#888; font-size:0.78rem; margin:-4px 0 6px 0;",
          "A 2-D map of every token coloured by cluster. PCA is linear; UMAP can separate groups more visibly but distorts distances, so read it as a sketch, not a measurement. Hover a point to see its token; use the camera icon in the plot toolbar to download."),
        plotly::plotlyOutput("cluster_proj_plot", height = "380px")),
      sec("Name the groups", "tag",
        tags$p(style = "color:#666; font-size:0.85rem; margin:0 0 8px 0;",
          "Give each cluster a tone label, then publish or download. Downstream tabs see these names in the ",
          tags$code("cluster"), " column (the integer id is kept as ", tags$code("cluster_id"), ")."),
        uiOutput("cluster_name_ui")),
      sec("Validation (agreement with reference labels)", "circle-check",
        uiOutput("cluster_agreement"))
    )
  })

  output$cluster_discovery <- renderUI({
    if (is.null(active_data()))
      return(tags$div(style = "color:#888; font-style:italic; margin:8px 0;",
                      "Upload a dataset in the Start tab to begin."))
    if (is.null(input$cluster_run) || input$cluster_run == 0)
      return(tags$div(style = "color:#777; margin:10px 0;",
        "Set the variables and options in the left panel, then click ",
        tags$strong("Run clustering"), "."))
    uiOutput("cluster_result_block")
  })

  # --- publish candidate labels + download ---
  clustered_df <- reactive({
    r <- cl_run(); res <- final_res(); k <- final_k(); d <- active_data(); req(d)
    nm <- vapply(seq_len(k), function(i) {
      v <- input[[paste0("cluster_name_", i)]]
      if (is.null(v) || !nzchar(trimws(v))) sprintf("T%d", i) else trimws(v)
    }, character(1))
    ids <- unname(res$assignment[as.character(d[[r$token]])])  # integer per row
    d$cluster_id <- ids
    d$cluster    <- ifelse(is.na(ids), NA_character_, nm[ids])   # named tone label
    d
  })

  observeEvent(input$cluster_use_labels, {
    cd <- try(clustered_df(), silent = TRUE)
    if (inherits(cd, "try-error") || is.null(cd)) {
      showNotification("Run clustering first.", type = "warning", duration = 4); return() }
    cluster_data(cd)
    showNotification("Published your named clusters as the 'cluster' column. Pick \"Clustered data\" in a Model tab and set the tone variable to 'cluster'.",
                     type = "message", duration = 8)
  })

  output$cluster_dl <- downloadHandler(
    filename = function() paste0(input$dataset_name %||% "data", "_clustered.csv"),
    content = function(file) {
      cd <- try(clustered_df(), silent = TRUE)
      if (inherits(cd, "try-error") || is.null(cd)) {
        writeLines("# Shinytone: run clustering first.", file)
        showNotification("Run clustering first.", type = "warning", duration = 4); return() }
      utils::write.csv(cd, file, row.names = FALSE)
    }
  )
}
