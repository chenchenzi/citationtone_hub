###############################################
# Curate tab: re-label tone-category variants (e.g. colloquial / literary
# 文白异读) and mark tokens for exclusion. Annotation-level curation, distinct
# from f0 correction (the f0 is assumed clean; the category label is revised).
#
# Discovery is a single interactive plot: pick one tone, optionally facet by
# speaker or item, and box-/lasso-select the variant cluster. Tokens the
# Inspect tab flagged (if it has been run) are drawn in amber so visual and
# flag-based discovery happen together; the current selection is drawn in red.
# The selection is then relabelled / excluded from the left panel, with an
# optional free-text reason recorded in the log and the data (curate_note).
#
# Curation never overwrites the original tone; it adds tone_relabelled,
# excluded and curate_note, logs every action (in the style of the F0
# Correction edit log), and publishes an analysis-ready dataset (tone =
# relabelled, excluded rows dropped) to the shared curated_data reactiveVal.
###############################################

curate_ui <- function(input, output, session, dataset_in, normalised_data = NULL,
                      curated_data, inspect_result = NULL) {

  # ---- curation state ----
  rv_relabel   <- reactiveVal(stats::setNames(character(0), character(0)))
  rv_exclude   <- reactiveVal(character(0))
  rv_note      <- reactiveVal(stats::setNames(character(0), character(0)))  # token -> reason
  rv_selection <- reactiveVal(character(0))
  rv_log <- reactiveVal(data.frame(
    date = character(0), token = character(0), speaker = character(0),
    tone = character(0), action = character(0), to = character(0),
    note = character(0), stringsAsFactors = FALSE))

  # Prefer the normalised dataset once Normalise has been run: it is a superset
  # of the upload (adds f0_st / f0_zscore), so every selector still works and the
  # f0-variable guess below defaults to the normalised column. Falls back to the
  # raw upload otherwise.
  dataset <- reactive({
    nd <- if (!is.null(normalised_data)) normalised_data() else NULL
    if (!is.null(nd)) nd else dataset_in()
  })
  using_normalised <- reactive({
    !is.null(if (!is.null(normalised_data)) normalised_data() else NULL)
  })

  cvar <- reactive({
    req(dataset())
    list(token = input$curate_token_var, tone = input$curate_tone_var,
         speaker = input$curate_speaker_var, f0 = input$curate_f0_var)
  })

  lex_col <- reactive({
    d <- dataset(); if (is.null(d)) return(NULL)
    hit <- grep("^char$|^word|^item|^syll|^vowel|^ipa", names(d), ignore.case = TRUE, value = TRUE)
    if (length(hit)) hit[1] else NULL
  })

  flagged_tokens <- reactive({
    ir <- if (!is.null(inspect_result)) inspect_result() else NULL
    if (is.null(ir)) return(character(0))
    v <- cvar()
    tokcol <- if (v$token %in% names(ir)) v$token else guess_var(names(ir), var_patterns$token, 1)
    keep <- if ("flagged_token" %in% names(ir)) ir$flagged_token %in% TRUE else rep(FALSE, nrow(ir))
    unique(as.character(ir[[tokcol]][keep]))
  })
  level_flagged_tokens <- reactive({
    ir <- if (!is.null(inspect_result)) inspect_result() else NULL
    if (is.null(ir) || !("flag_notes" %in% names(ir))) return(character(0))
    v <- cvar()
    tokcol <- if (v$token %in% names(ir)) v$token else guess_var(names(ir), var_patterns$token, 1)
    unique(as.character(ir[[tokcol]][grepl("level too", ir$flag_notes)]))
  })

  log_curate <- function(action, tokens, to = NA_character_, note = "") {
    if (length(tokens) == 0) return(invisible())
    d <- dataset(); v <- isolate(cvar()); key <- as.character(d[[v$token]])
    look <- function(col) vapply(tokens, function(tk) {
      i <- match(tk, key); if (is.na(i)) NA_character_ else as.character(d[[col]][i]) }, character(1))
    rv_log(rbind(rv_log(), data.frame(
      date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), token = as.character(tokens),
      speaker = look(v$speaker), tone = look(v$tone), action = action,
      to = ifelse(is.na(to), "", to), note = ifelse(is.na(note), "", note),
      stringsAsFactors = FALSE)))
  }

  # ---- guide ----
  output$curate_guide <- renderUI({
    guide_box("Curate guide",
      tags$p(style = "margin: 4px 0 8px 0;", HTML(
        "Re-label tokens whose tone category needs revising, or mark tokens to exclude.",
        "Use this for genuine <em>linguistic variation</em>: tone splits or mergers, variant",
        "readings (colloquial vs. literary, 文白异读), sandhi or sociolinguistic variants, or",
        "mis-elicited tokens. It is <strong>not</strong> for pitch-tracking errors, which are",
        "handled in <strong>Inspect</strong> and <strong>F0 Correction</strong>.",
        "The key here is to identify contours whose shape or normalised height diverges markedly",
        "from the rest of the same tone category.")),
      tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
        tags$li("Pick one tone, optionally facet by speaker or item, and ",
          tags$strong("box-/lasso-select"), " the variant cluster on the plot. Tokens the ",
          tags$strong("Inspect"), " tab flagged appear in ",
          tags$span(style = "color:#c9920a; font-weight:600;", "amber"),
          "; your current selection is ", tags$span(style = "color:#d9534f; font-weight:600;", "red"), "."),
        tags$li("In the left panel, relabel the selection to a ", tags$em("new"),
          " label to split a category (e.g. ", tags$code("T4-literary"), "), or to an ",
          tags$em("existing"), " label to merge categories; optionally add a reason (e.g. ",
          tags$em("literary form"), "). Or exclude the selection. You act on a ",
          tags$em("subset"), ", not the whole dataset."),
        tags$li("The original tone is never overwritten; every action is logged. Downstream ",
          "tabs gain a ", tags$strong("Curated data"), " source (tone = your relabels, excluded ",
          "tokens dropped; original kept as ", tags$code("<tone>_original"), ").")
      )
    )
  })

  # ---- recommended-prerequisites mini illustration ----
  # Normalise + Inspect both make divergent tokens easier to find, so nudge the
  # user to run them first. Each step shows a tick once done; the whole box
  # collapses by default once both are complete (still available, just tidy).
  output$curate_prep <- renderUI({
    norm_done <- isTRUE(using_normalised())
    insp_done <- !is.null(if (!is.null(inspect_result)) inspect_result() else NULL)

    step <- function(n, title, tab, why, done, here = FALSE) {
      badge_cls <- if (here) "cp-cur" else if (done) "cp-done" else "cp-todo"
      badge_txt <- if (here) HTML("&#9679;") else if (done) HTML("&#10003;") else as.character(n)
      status <- if (here)
                  tags$div(class = "cp-status", style = "color:#2f5d86;", "You are here")
                else if (done)
                  tags$div(class = "cp-status cp-done-t", HTML("&#10003; Done"))
                else
                  tags$div(class = "cp-status cp-todo-t", "Recommended first")
      tags$div(class = if (here) "cp-step cp-here" else "cp-step",
        tags$div(class = "cp-head",
          tags$span(class = paste("cp-badge", badge_cls), badge_txt),
          tags$span(class = "cp-title", title),
          if (!is.null(tab)) tags$span(class = "cp-tab", tab)),
        tags$div(class = "cp-why", why),
        status)
    }
    arrow <- tags$div(class = "cp-arrow", HTML("&#10132;"))

    tagList(
      tags$style(HTML("
        .curate-prep { background:#eef4fb; border:1px solid #d2e1f1; border-radius:8px;
          padding:6px 16px 9px 16px; margin:0 0 12px 0; }
        .curate-prep > summary { cursor:pointer; font-weight:700; color:#2f5d86;
          font-size:0.9rem; list-style:none; padding:1px 0; }
        .curate-prep > summary::-webkit-details-marker { display:none; }
        .curate-prep > summary::before { content:'\\25B8'; color:#5a8fc0;
          display:inline-block; margin-right:8px; transition:transform .15s ease; }
        .curate-prep[open] > summary::before { transform:rotate(90deg); }
        .curate-prep > summary .cp-hint { color:#7a9ec2; font-weight:400; font-size:0.8rem; margin-left:4px; }
        .curate-prep[open] > summary .cp-hint { display:none; }
        .curate-prep-flow { display:flex; align-items:stretch; gap:9px;
          margin-top:9px; flex-wrap:wrap; }
        .cp-step { flex:1 1 220px; background:#fff; border:1px solid #e1e9f2;
          border-radius:7px; padding:8px 12px; }
        .cp-step.cp-here { border-color:#78c2ad; box-shadow:0 0 0 2px rgba(120,194,173,0.18); }
        .cp-head { display:flex; align-items:center; gap:7px; margin-bottom:3px; flex-wrap:wrap; }
        .cp-badge { width:20px; height:20px; border-radius:50%; flex-shrink:0;
          display:inline-flex; align-items:center; justify-content:center;
          font-size:0.72rem; font-weight:700; color:#fff; }
        .cp-badge.cp-todo { background:#aab9c6; }
        .cp-badge.cp-done { background:#46a37e; }
        .cp-badge.cp-cur  { background:#d9534f; font-size:0.55rem; }
        .cp-title { font-weight:700; color:#2c5f4f; font-size:0.88rem; }
        .cp-tab { display:inline-block; background:#e8f5f0; color:#2c5f4f;
          padding:1px 8px; border-radius:10px; font-size:0.68rem; font-weight:600;
          font-family:'SFMono-Regular',Menlo,Consolas,monospace; white-space:nowrap; }
        .cp-why { font-size:0.78rem; color:#5f6b66; line-height:1.4; margin-top:2px; }
        .cp-status { font-size:0.72rem; font-weight:600; margin-top:5px; }
        .cp-status.cp-done-t { color:#46a37e; }
        .cp-status.cp-todo-t { color:#c0852a; }
        .cp-arrow { display:flex; align-items:center; color:#9fbbd6; font-size:1.35rem; }
        @media (max-width:760px){ .cp-arrow{ display:none; } }
      ")),
      tags$details(class = "curate-prep",
        tags$summary("Recommended before you curate: Normalise, then Inspect",
                     tags$span(class = "cp-hint", "(click to expand)")),
        tags$div(class = "cp-why", style = "margin-top:5px;",
          "Both are optional but make divergent tokens far easier to spot."),
        tags$div(class = "curate-prep-flow",
          step(1, "Normalise", "Normalise tab",
               "Put speakers on one scale (semitones or z-score) so contour height is comparable across speakers.",
               norm_done),
          arrow,
          step(2, "Inspect", "Inspect tab",
               "Flag token-level level outliers; flagged tokens appear in amber in the plot as ready-made candidates to review.",
               insp_done),
          arrow,
          step(3, "Curate", NULL,
               "Box- or lasso-select the divergent tokens, then re-label or exclude them.",
               FALSE, here = TRUE)
        )
      )
    )
  })

  # ---- sidebar: setup + curate actions ----
  output$ui_curate <- renderUI({
    vars <- if (!is.null(dataset())) names(dataset()) else c("No dataset available")
    data_types <- if (!is.null(dataset())) sapply(dataset(), class) else rep("NA", length(vars))
    var_types <- paste0(vars, " {", data_types, "}")
    tone_default <- guess_var(vars, var_patterns$tone, 5)
    tone_choices <- if (!is.null(dataset()) && tone_default %in% names(dataset()))
                      sort(unique(as.character(dataset()[[tone_default]]))) else NULL
    item_default <- lex_col(); if (is.null(item_default)) item_default <- "__none__"
    tagList(
      wellPanel(
        h5("Dataset",
           tags$small(style = "color: #777; margin-left: 6px; font-weight: 400;", input$dataset_name)),
        if (isTRUE(using_normalised()))
          tags$div(style = "font-size: 0.75rem; color: #2a7a5a; margin: -4px 0 8px 0;",
                   icon("wave-square"), " Using your normalised dataset."),
        selectInput("curate_token_var", "Token ID variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$token, 1)),
        selectInput("curate_tone_var", "Tone category variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$tone, 5)),
        selectInput("curate_speaker_var", "Speaker variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$speaker, 4)),
        selectInput("curate_f0_var", "f0 variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$f0, 2)),
        tags$div(style = "font-size: 0.72rem; color: #999; margin: -8px 0 10px 0;",
                 "Defaults to your normalised f0 (e.g. semitones or z-score) when available."),
        selectInput("curate_item_var", "Item / word variable (for facetting):",
                    choices = c("(none)" = "__none__", stats::setNames(vars, var_types)),
                    selected = item_default),
        tags$hr(),
        h5("Curate the selection"),
        uiOutput("curate_sel_count"),
        tags$div(style = "color:#888; font-size:0.78rem; margin:-2px 0 8px 0; line-height:1.45;",
                 "Relabel and exclude are independent: you can relabel a token and keep it, or exclude it (with or without relabelling)."),
        selectizeInput("curate_new_label", "Relabel to:", choices = tone_choices,
                       options = list(create = TRUE,
                                      placeholder = "existing tone or new label")),
        textInput("curate_note", "Reason / note (optional):", value = "",
                  placeholder = "e.g. literary form, sandhi form"),
        actionButton("curate_relabel_apply", "Relabel selected", icon = icon("tag"),
                     class = "btn-primary"),
        div(style = "margin-top: 6px;",
            actionButton("curate_exclude_apply", "Exclude selected", icon = icon("ban"))),
        tags$div(style = "color: #888; font-size: 0.75rem; margin-top: 3px;",
                 "Excluded tokens are dropped from the ", tags$strong("Curated data"),
                 " used in Model / Summarise (the original upload is untouched)."),
        div(style = "margin-top: 6px;",
            actionButton("curate_clear_apply", "Clear curation on selected",
                         icon = icon("rotate-left"))),
        div(style = "margin-top: 6px;",
            actionButton("curate_exclude_relabelled", "Exclude all relabelled",
                         icon = icon("ban"))),
        tags$div(style = "color: #888; font-size: 0.75rem; margin-top: 3px;",
                 "Shortcut: also exclude every token you have relabelled (e.g. to drop a separated variant from the analysis)."),
        tags$hr(),
        actionButton("curate_reset", "Reset all curation", class = "btn-outline-secondary"),
        tags$hr(),
        h5("Download"),
        downloadButton("curate_dl_curated", "Curated CSV (analysis-ready)"),
        div(style = "margin-top: 4px;",
            downloadButton("curate_dl_annot", "Annotated CSV (all rows)"))
      )
    )
  })

  # populate the relabel dropdown with existing tones (stable sidebar target,
  # re-fires once the tone variable resolves)
  observe({
    d <- dataset(); req(d); v <- cvar(); req(v$tone %in% names(d))
    updateSelectizeInput(session, "curate_new_label",
                         choices = sort(unique(as.character(d[[v$tone]]))),
                         selected = character(0), server = FALSE)
  })

  output$curate_sel_count <- renderUI({
    n <- length(rv_selection())
    if (n == 0) tags$div(style = "color: #888; font-size: 0.82rem; margin-bottom: 6px;",
                         "No tokens selected yet (select on the plot).")
    else tags$div(style = "font-size: 0.85rem; margin-bottom: 6px; color: #2a7a5a;",
                  tags$strong(sprintf("Acting on %d selected token(s).", n)))
  })

  # ---- main panel: find -> select (plot) -> log ----
  output$curate_discovery <- renderUI({
    d <- dataset()
    if (is.null(d)) {
      return(tags$div(style = "color: #888; font-style: italic; margin: 8px 0;",
                      "Upload a dataset in the Start tab to begin curating."))
    }
    v <- cvar(); req(v$tone %in% names(d))
    tones <- sort(unique(as.character(d[[v$tone]])))
    speakers <- if (v$speaker %in% names(d)) sort(unique(as.character(d[[v$speaker]]))) else character(0)
    facet_choices <- c("None" = "__none__",
                       "Speaker" = "__speaker__",
                       "Item / word" = "__item__")
    has_inspect <- length(level_flagged_tokens()) > 0

    tagList(
      tags$h4("Identify and select tokens to curate"),
      tags$div(style = "display: flex; gap: 10px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 4px;",
        selectInput("curate_view_tone", "Tone (one at a time):",
                    choices = tones, selected = tones[1], width = "130px"),
        selectInput("curate_view_speaker", "Speaker:",
                    choices = c("All speakers" = "__all__", stats::setNames(speakers, speakers)),
                    selected = "__all__", width = "150px"),
        selectInput("curate_facet_by", "Facet by:",
                    choices = facet_choices, selected = "__none__", width = "160px")
      ),
      tags$div(style = "display: flex; gap: 6px; align-items: center; flex-wrap: wrap; margin-bottom: 4px;",
        tags$span(style = "font-size: 0.8rem; color: #777;", "Select (current view):"),
        if (has_inspect)
          actionButton("curate_sel_flagged", "Flagged", class = "btn-sm", icon = icon("flag")),
        actionButton("curate_sel_all", "All", class = "btn-sm"),
        actionButton("curate_invert", "Invert", class = "btn-sm"),
        actionButton("curate_clear_selection", "Clear", class = "btn-sm btn-outline-secondary",
                     icon = icon("xmark"))
      ),
      uiOutput("curate_status_box"),
      if (has_inspect)
        tags$div(style = "background:#fff8e1; border-left:4px solid #e0a800; padding:7px 12px; border-radius:5px; font-size:0.8rem; color:#8a6d00; margin-bottom:8px; line-height:1.55;",
          tags$span(style = "color:#e0a800; margin-right:5px;", icon("lightbulb")),
          HTML(paste(
            "<strong>Amber</strong> marks tokens that <strong>Inspect</strong> flagged for sitting at an unusual",
            "overall level relative to the rest of the tokens by the same speaker and tone",
            "(<em>&ldquo;level too high / low&rdquo;</em>). These are potential variant candidates, so review",
            "them: a flagged token may have an atypical yet valid contour. A token-level flag is summarised",
            "from all its f0 points, so a few extreme points can skew it: flags are not always right, and they",
            "can miss patterns too."))),
      tags$p(style = "color: #888; font-size: 0.8rem; margin: 2px 0 6px 0;",
        "Drag a ", tags$strong("box"), " (or pick the ", tags$strong("lasso"),
        " in the plot toolbar) around a cluster to select it; selections accumulate, then relabel or exclude on the left."),
      uiOutput("curate_plot_holder")
    )
  })

  # resolve the facet column from the "Facet by" selector + item-variable selector
  facet_col <- reactive({
    fct <- input$curate_facet_by; v <- cvar()
    if (identical(fct, "__speaker__")) v$speaker
    else if (identical(fct, "__item__")) {
      iv <- input$curate_item_var
      if (!is.null(iv) && iv != "__none__") iv else NULL
    } else NULL
  })

  output$curate_plot_holder <- renderUI({
    d <- dataset(); req(d); v <- cvar()
    fc <- facet_col(); h <- 520
    if (!is.null(fc) && fc %in% names(d)) {
      keep <- as.character(d[[v$tone]]) == input$curate_view_tone
      spk <- input$curate_view_speaker
      if (!is.null(spk) && spk != "__all__" && v$speaker %in% names(d))
        keep <- keep & as.character(d[[v$speaker]]) == spk
      n <- length(unique(as.character(d[[fc]][keep])))
      h <- max(420, ceiling(n / 3) * 240)
    }
    plotly::plotlyOutput("curate_plot", height = paste0(h, "px"))
  })

  output$curate_plot <- plotly::renderPlotly({
    d <- dataset(); req(d, input$curate_view_tone)
    v <- cvar(); req(all(c(v$tone, v$token, v$f0) %in% names(d)))
    sub <- d[as.character(d[[v$tone]]) == input$curate_view_tone, , drop = FALSE]
    spk <- input$curate_view_speaker
    if (!is.null(spk) && spk != "__all__" && v$speaker %in% names(sub))
      sub <- sub[as.character(sub[[v$speaker]]) == spk, , drop = FALSE]
    req(nrow(sub) > 0)

    time_guess <- grep("^time|^t$|^timepoint|^measurement|^index", names(d), ignore.case = TRUE, value = TRUE)
    if (length(time_guess) == 0) {
      sub$.x <- stats::ave(seq_len(nrow(sub)), as.character(sub[[v$token]]), FUN = seq_along)
      xlab <- "sample index"
    } else { sub$.x <- sub[[time_guess[1]]]; xlab <- time_guess[1] }

    sub$.tok <- as.character(sub[[v$token]])
    flagged <- level_flagged_tokens(); selected <- rv_selection()
    sub$.status <- ifelse(sub$.tok %in% selected, "selected",
                   ifelse(sub$.tok %in% flagged, "flagged (Inspect)", "other"))
    sub$.status <- factor(sub$.status, levels = c("other", "flagged (Inspect)", "selected"))
    pal <- c("other" = "grey72", "flagged (Inspect)" = "#e0a800", "selected" = "#d9534f")

    p <- ggplot2::ggplot(sub, ggplot2::aes(x = .data$.x, y = .data[[v$f0]],
                                           group = .data$.tok, customdata = .data$.tok,
                                           colour = .data$.status)) +
      ggplot2::geom_line(alpha = 0.7) +
      ggplot2::geom_point(size = 0.7, alpha = 0.45) +
      ggplot2::scale_colour_manual(values = pal, drop = FALSE, name = NULL) +
      ggplot2::labs(x = xlab, y = f0_axis_label(v$f0))

    fc <- facet_col()
    if (!is.null(fc) && fc %in% names(sub)) {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[fc]]), scales = "free_y", ncol = 3)
    }

    gg <- plotly::ggplotly(p, tooltip = "customdata")
    gg <- plotly::layout(gg, dragmode = "select",
                         legend = list(orientation = "h", x = 0, y = 1.02,
                                       xanchor = "left", yanchor = "bottom"))
    gg <- plotly::config(gg, displayModeBar = TRUE,
                         modeBarButtonsToAdd = list("select2d", "lasso2d"))
    gg$x$source <- "curate_plot"
    gg
  })

  observeEvent(plotly::event_data("plotly_selected", source = "curate_plot"), {
    ev <- plotly::event_data("plotly_selected", source = "curate_plot")
    if (is.null(ev) || is.null(ev$customdata)) return()
    rv_selection(union(rv_selection(), unique(as.character(ev$customdata))))
  }, ignoreNULL = TRUE)

  observeEvent(plotly::event_data("plotly_click", source = "curate_plot"), {
    ev <- plotly::event_data("plotly_click", source = "curate_plot")
    if (is.null(ev) || is.null(ev$customdata)) return()
    tk <- as.character(ev$customdata)[1]; cur <- rv_selection()
    rv_selection(if (tk %in% cur) setdiff(cur, tk) else union(cur, tk))
  }, ignoreNULL = TRUE)

  # tokens in the current view (tone + optional speaker filter)
  view_tokens <- reactive({
    d <- dataset(); req(d, input$curate_view_tone); v <- cvar()
    keep <- as.character(d[[v$tone]]) == input$curate_view_tone
    spk <- input$curate_view_speaker
    if (!is.null(spk) && spk != "__all__" && v$speaker %in% names(d))
      keep <- keep & as.character(d[[v$speaker]]) == spk
    unique(as.character(d[[v$token]][keep]))
  })

  # Select level-flagged tokens within the current view
  observeEvent(input$curate_sel_flagged, {
    vt <- view_tokens(); add <- intersect(level_flagged_tokens(), vt)
    if (length(add) == 0) {
      showNotification("No level-flagged tokens in this view.", type = "warning", duration = 4); return() }
    rv_selection(union(rv_selection(), add))
  })
  # Select all tokens in the current view
  observeEvent(input$curate_sel_all, { rv_selection(union(rv_selection(), view_tokens())) })
  # Invert selection within the current view (selections elsewhere kept)
  observeEvent(input$curate_invert, {
    vt <- view_tokens(); cur <- rv_selection()
    rv_selection(union(setdiff(cur, vt), setdiff(vt, cur)))
  })
  observeEvent(input$curate_clear_selection, { rv_selection(character(0)) })

  # combined status box: two compact chips — how many tokens are level-flagged
  # in the current view, and the current selection (replaces the two separate
  # summary lines, which felt crowded stacked around the amber note).
  output$curate_status_box <- renderUI({
    sel <- rv_selection()
    has_flags <- length(level_flagged_tokens()) > 0
    chips <- list()

    if (has_flags) {
      vt <- view_tokens(); fl <- intersect(level_flagged_tokens(), vt)
      spk <- input$curate_view_speaker
      where <- if (!is.null(spk) && spk != "__all__")
                 sprintf("tone %s &middot; %s", input$curate_view_tone, spk)
               else sprintf("tone %s &middot; all speakers", input$curate_view_tone)
      chips[[length(chips) + 1]] <- tags$div(
        style = "flex:0 0 auto; min-width:150px; border:1px solid #f0e3b8; background:#fffaf0; border-left:4px solid #e0a800; border-radius:6px; padding:5px 12px;",
        tags$div(style = "font-size:0.68rem; text-transform:uppercase; letter-spacing:0.04em; color:#a07c00;",
                 HTML("&#9873; Flagged in view")),
        tags$div(style = "font-size:1.05rem; color:#8a6d00; line-height:1.3;",
                 HTML(sprintf("<strong>%d</strong> <span style='color:#bda653;'>of %d</span>",
                              length(fl), length(vt)))),
        tags$div(style = "font-size:0.7rem; color:#bda653;", HTML(where)))
    }

    sel_preview <- ""
    if (length(sel) > 0) {
      sel_preview <- paste(utils::head(sel, 8), collapse = ", ")
      if (length(sel) > 8) sel_preview <- paste0(sel_preview, ", …")
    }
    sel_border <- if (length(sel) > 0) "#d9534f" else "#cfcfcf"
    sel_bg     <- if (length(sel) > 0) "#fdf3f2" else "#f7f7f7"
    chips[[length(chips) + 1]] <- tags$div(
      style = sprintf("flex:1 1 240px; min-width:200px; border:1px solid #eee; background:%s; border-left:4px solid %s; border-radius:6px; padding:5px 12px; overflow:hidden;",
                      sel_bg, sel_border),
      tags$div(style = "font-size:0.68rem; text-transform:uppercase; letter-spacing:0.04em; color:#b3433f;",
               HTML("&#9679; Selected")),
      tags$div(style = "font-size:1.05rem; color:#c0392b; line-height:1.3;",
               if (length(sel) > 0) HTML(sprintf("<strong>%d</strong> token(s)", length(sel)))
               else tags$span(style = "color:#999; font-weight:400; font-size:0.85rem;",
                              "none yet (select on the plot)")),
      if (length(sel) > 0)
        tags$div(style = "font-size:0.72rem; color:#999; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
                 sel_preview))

    tags$div(style = "display:flex; gap:10px; flex-wrap:wrap; align-items:stretch; margin:4px 0 8px 0;",
             chips)
  })

  # ---- apply actions (on the current selection) ----
  observeEvent(input$curate_relabel_apply, {
    toks <- rv_selection(); lab <- input$curate_new_label; reason <- input$curate_note %||% ""
    if (length(toks) == 0) { showNotification("Select one or more tokens first.", type = "warning", duration = 4); return() }
    if (is.null(lab) || !nzchar(lab)) { showNotification("Choose or type a label to relabel to.", type = "warning", duration = 4); return() }
    rel <- rv_relabel(); rel[toks] <- lab; rv_relabel(rel)
    if (nzchar(reason)) { nt <- rv_note(); nt[toks] <- reason; rv_note(nt) }
    log_curate("relabel", toks, to = lab, note = reason)
    showNotification(sprintf("Relabelled %d token(s) to \"%s\". They stay in the Curated data unless you also exclude them.", length(toks), lab),
                     type = "message", duration = 5)
  })
  observeEvent(input$curate_exclude_apply, {
    toks <- rv_selection(); reason <- input$curate_note %||% ""
    if (length(toks) == 0) { showNotification("Select one or more tokens first.", type = "warning", duration = 4); return() }
    rv_exclude(union(rv_exclude(), toks))
    if (nzchar(reason)) { nt <- rv_note(); nt[toks] <- reason; rv_note(nt) }
    log_curate("exclude", toks, note = reason)
    showNotification(sprintf("Excluded %d token(s) from the Curated data (the original upload is untouched).", length(toks)),
                     type = "message", duration = 5)
  })
  observeEvent(input$curate_clear_apply, {
    toks <- rv_selection(); if (length(toks) == 0) return()
    rel <- rv_relabel(); rel <- rel[setdiff(names(rel), toks)]; rv_relabel(rel)
    nt <- rv_note(); nt <- nt[setdiff(names(nt), toks)]; rv_note(nt)
    rv_exclude(setdiff(rv_exclude(), toks)); log_curate("clear", toks)
    showNotification(sprintf("Cleared curation on %d token(s).", length(toks)), type = "message", duration = 3)
  })
  observeEvent(input$curate_exclude_relabelled, {
    toks <- names(rv_relabel())
    if (length(toks) == 0) {
      showNotification("No relabelled tokens to exclude yet.", type = "warning", duration = 4); return() }
    rv_exclude(union(rv_exclude(), toks))
    log_curate("exclude (all relabelled)", toks)
    showNotification(sprintf("Excluded all %d relabelled token(s).", length(toks)),
                     type = "message", duration = 3)
  })
  observeEvent(input$curate_reset, {
    rv_relabel(stats::setNames(character(0), character(0)))
    rv_exclude(character(0)); rv_note(stats::setNames(character(0), character(0)))
    rv_selection(character(0)); rv_log(rv_log()[0, , drop = FALSE])
    showNotification("All curation reset.", type = "message", duration = 3)
  })

  # ---- publish analysis-ready curated data (keeps curate_note) ----
  observe({
    d <- dataset(); rel <- rv_relabel(); exc <- rv_exclude()
    if (is.null(d) || (length(rel) == 0 && length(exc) == 0)) { curated_data(NULL); return() }
    v <- isolate(cvar()); req(v$token %in% names(d), v$tone %in% names(d))
    full <- apply_relabels(d, token = v$token, tone = v$tone,
                           relabel = rel, exclude = exc, note = rv_note())
    ar <- full[!full$excluded, , drop = FALSE]
    ar[[paste0(v$tone, "_original")]] <- ar[[v$tone]]
    ar[[v$tone]] <- ar$tone_relabelled
    ar$tone_relabelled <- NULL; ar$excluded <- NULL
    curated_data(ar)
  })

  # ---- curation log (yellow summary right under the heading) ----
  output$curate_log_block <- renderUI({
    tagList(
      tags$h4(style = "margin-top: 18px;", "Curation log"),
      uiOutput("curate_summary"),
      tags$div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 6px;",
        downloadButton("curate_log_download", "Download curation log (CSV)", icon = icon("download")),
        actionButton("curate_log_clear", "Clear log", icon = icon("eraser"), class = "btn-sm"),
        tags$span(style = "color: #888; font-size: 0.8rem; font-style: italic;",
                  "Chronological record of every relabel / exclusion this session.")),
      DT::dataTableOutput("curate_log_table")
    )
  })

  output$curate_summary <- renderUI({
    d <- dataset(); if (is.null(d)) return(NULL)
    rel <- rv_relabel(); exc <- rv_exclude(); v <- cvar()
    n_tok <- length(unique(as.character(d[[v$token]])))
    tags$div(style = "background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 8px 14px; margin-bottom: 10px; border-radius: 4px; font-size: 0.88rem;",
      tags$strong("Curation so far: "),
      sprintf("%d token(s) relabelled, %d excluded, of %d total.",
              length(rel), length(exc), n_tok))
  })

  output$curate_log_table <- DT::renderDataTable({
    df <- rv_log()
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(date = character(0), token = character(0), speaker = character(0),
                   tone = character(0), action = character(0), to = character(0),
                   note = character(0), stringsAsFactors = FALSE),
        rownames = FALSE, options = list(pageLength = 10, dom = "tip",
                       language = list(emptyTable = "No curation yet."))))
    }
    df_show <- df[seq.int(nrow(df), 1L), , drop = FALSE]
    names(df_show) <- c("date", "token", "speaker", "original tone", "action", "relabelled to", "note")
    DT::datatable(df_show, rownames = FALSE,
      options = list(pageLength = 10, dom = "tip", scrollX = TRUE,
                     columnDefs = list(list(className = "dt-center", targets = c(0, 4)))))
  })

  output$curate_log_download <- downloadHandler(
    filename = function() sprintf("curation_log_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      df <- rv_log()
      if (nrow(df) == 0) { writeLines("# Shinytone: curation log is empty.", file)
        showNotification("Curation log is empty.", type = "warning", duration = 4); return() }
      utils::write.csv(df, file, row.names = FALSE)
    }
  )
  observeEvent(input$curate_log_clear, { if (nrow(rv_log()) > 0) rv_log(rv_log()[0, , drop = FALSE]) })

  # ---- dataset downloads ----
  output$curate_dl_annot <- downloadHandler(
    filename = function() paste0(input$dataset_name %||% "data", "_annotated.csv"),
    content = function(file) {
      d <- dataset(); req(d); v <- cvar()
      utils::write.csv(apply_relabels(d, token = v$token, tone = v$tone,
                                      relabel = rv_relabel(), exclude = rv_exclude(),
                                      note = rv_note()),
                       file, row.names = FALSE)
    }
  )
  output$curate_dl_curated <- downloadHandler(
    filename = function() paste0(input$dataset_name %||% "data", "_curated.csv"),
    content = function(file) {
      cd <- curated_data(); if (is.null(cd)) { d <- dataset(); req(d); cd <- d }
      utils::write.csv(cd, file, row.names = FALSE)
    }
  )
}
