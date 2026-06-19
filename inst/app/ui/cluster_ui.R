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
        "<strong>shape</strong> of their f0 contour and let the data suggest the number",
        "of categories. This is a <strong>hypothesis generator</strong> (after Kaland 2023),",
        "not a verdict: clusters can reflect speaker, vowel or recording effects as well as",
        "tone, so confirm the result with linguistic analysis."))),
      tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
        tags$li(HTML("<strong>Normalise first (important).</strong> Cluster speaker-normalised f0 (semitone or z-score, from the Normalise tab). On raw Hz, clusters tend to separate <em>speakers</em> by pitch range rather than tones.")),
        tags$li(tags$strong("Feature space."), " Cluster the resampled contour points, compact ", tags$em("Legendre"), " / ", tags$em("DCT"), " coefficients, or the ", tags$em("derivative"), " (rate of change: clusters by movement and discards height; Kaland 2023)."),
        tags$li(tags$strong("Register vs shape."), " Keep height so high and low level tones separate, or centre each contour to cluster on shape alone."),
        tags$li(tags$strong("How many tones?"), " The diagnostics (elbow, silhouette, gap, GMM/BIC) rarely agree exactly, so read off a plausible ", tags$em("range"), ", then inspect the cluster-mean contours."),
        tags$li(tags$strong("Validate / reuse."), " If you have provisional labels, check agreement (adjusted Rand index). Send the clusters on as candidate tone labels for Curate / Model.")
      ),
      tags$details(style = "margin-top: 4px;",
        tags$summary(style = "cursor:pointer; font-size:0.82rem; color:#4a7868; font-weight:600;",
                     "Methods & references"),
        tags$ul(style = "margin: 6px 0 0 0; padding-left: 18px; font-size: 0.8rem; color:#555;",
          tags$li(HTML("<strong>Contour features.</strong> Time-normalised resampling and the <em>derivative</em> (rate-of-change) representation follow <strong>Kaland (2023)</strong>, <em>J. Int. Phonetic Assoc.</em> 53(1), 159&ndash;188 (derivative best for perception: <strong>Kaland 2023</strong>, <em>JASA</em> 154(1), 95&ndash;107). <em>Legendre</em> and <em>DCT</em> coefficients are standard orthogonal-basis summaries of a contour.")),
          tags$li(HTML("<strong>Clustering algorithms.</strong> k-means (Hartigan &amp; Wong 1979); hierarchical / Ward (Ward 1963); Gaussian mixture with BIC (Scrucca et al. 2016, <em>mclust</em>).")),
          tags$li(HTML("<strong>Number of groups.</strong> Silhouette (Rousseeuw 1987); gap statistic (Tibshirani et al. 2001); <em>MDL</em> information cost (<strong>Kaland &amp; Ellison 2023</strong>, <em>Proc. ICPhS 20</em>, 3448&ndash;3452).")),
          tags$li(HTML("<strong>Token map.</strong> PCA (Jolliffe 2002) or UMAP (McInnes, Healy &amp; Melville 2018, <em>arXiv:1802.03426</em>).")),
          tags$li(HTML("Kaland's own tool clusters hierarchically on DTW / correlation distances; this tab offers the methods above as accessible alternatives."))
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

  output$cluster_diag_plot <- renderPlot({
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

  # --- cluster-mean contours (the candidate prototypical tones) ---
  output$cluster_mean_plot <- renderPlot({
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
      ggplot2::labs(x = "normalised time", y = "mean f0 (normalised)",
                    title = sprintf("%d candidate tone contours (%s)", k, res$method)) +
      ggplot2::theme_minimal(base_size = 13)
  })

  # --- compare candidate k (small multiples; uses the CHOSEN method) ---
  # Reacts to the compare-range slider, so the user can line up e.g. 6, 7, 8
  # whatever the committed solution is.
  output$cluster_compare_plot <- renderPlot({
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
    mlab <- c(kmeans = "k-means", hclust = "hierarchical", gmm = "GMM")[meth]
    ggplot2::ggplot(cp, ggplot2::aes(x = .data$x, y = .data$f0,
                                     colour = .data$cluster, group = .data$cluster)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::facet_wrap(~ k, nrow = 1) +
      cluster_colour_scale(guide = "none") +
      ggplot2::labs(x = "normalised time", y = "mean f0",
                    subtitle = sprintf("%s solution per number of tones (drag the compare slider to change)", mlab)) +
      ggplot2::theme_minimal(base_size = 12)
  })

  # --- GMM membership confidence ---
  output$cluster_confidence <- renderUI({
    u <- final_res()$uncertainty
    if (is.null(u)) return(NULL)
    n_amb <- sum(u > 0.4)
    tags$div(style = "color:#8a6d00; font-size:0.85rem; margin:4px 0 0 0;",
      sprintf("GMM membership confidence: %d of %d token(s) (%.0f%%) sit ambiguously between clusters (posterior < 60%%); mean uncertainty %.2f. Treat these as borderline.",
              n_amb, length(u), 100 * n_amb / length(u), mean(u)))
  })

  # --- dendrogram (hierarchical method only; base R, no extra dependency) ---
  output$cluster_dendro_plot <- renderPlot({
    r <- cl_run(); tree <- r$res$tree; k <- final_k()
    validate(need(!is.null(tree), "Dendrogram is only available for the hierarchical method."))
    op <- graphics::par(mar = c(0.5, 4, 2.5, 0.5), cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2)
    on.exit(graphics::par(op))
    plot(tree, labels = FALSE, hang = -1, ylab = "merge height", xlab = "", sub = "",
         main = sprintf("Hierarchical merge tree (%s linkage), cut into %d groups",
                        r$linkage %||% "ward.D2", k))
    stats::rect.hclust(tree, k = k, border = "#2c5f4f")
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
    tagList(
      uiOutput("cluster_norm_nudge"),
      uiOutput("cluster_suggest"),
      sec("How many tones? (cluster-count diagnostics)", "chart-line",
        plotOutput("cluster_diag_plot", height = "360px"),
        tags$details(style = "background:#eef4fb; border:1px solid #cfe2f1; border-radius:6px; padding:6px 12px; margin:6px 0 0 0;",
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
        uiOutput("cluster_confidence")),
      conditionalPanel("input.cluster_method == 'hclust'",
        sec("Dendrogram (hierarchical merge tree)", "sitemap",
          tags$p(style = "color:#666; font-size:0.82rem; margin:0 0 6px 0;",
            "Merge tree over all tokens (leaf labels hidden). Tall vertical gaps suggest natural cut points; the boxes show the current number of groups."),
          plotOutput("cluster_dendro_plot", height = "300px"))),
      sec("Compare candidate solutions", "layer-group",
        tags$p(style = "color:#666; font-size:0.85rem; margin:0 0 8px 0;",
          "Cluster-mean contours at several values of k side by side, using your chosen algorithm. Drag the range to line up e.g. 6, 7 and 8 tones."),
        sliderInput("cluster_compare_range", "Show side-by-side for k =",
                    min = 2, max = 12, value = c(4, 7), step = 1, width = "340px"),
        plotOutput("cluster_compare_plot", height = "230px")),
      sec("Token map", "diagram-project",
        radioButtons("cluster_projection", NULL, inline = TRUE,
                     choices = if (requireNamespace("uwot", quietly = TRUE))
                       c("PCA (linear)" = "pca", "UMAP (non-linear)" = "umap")
                     else c("PCA (linear)" = "pca"),
                     selected = "pca"),
        tags$div(style = "color:#888; font-size:0.78rem; margin:-4px 0 6px 0;",
          "A 2-D map of every token coloured by cluster. PCA is linear; UMAP can separate groups more visibly but distorts distances, so read it as a sketch, not a measurement."),
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
