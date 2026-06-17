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

curate_ui <- function(input, output, session, dataset, curated_data, inspect_result = NULL) {

  # ---- curation state ----
  rv_relabel   <- reactiveVal(stats::setNames(character(0), character(0)))
  rv_exclude   <- reactiveVal(character(0))
  rv_note      <- reactiveVal(stats::setNames(character(0), character(0)))  # token -> reason
  rv_selection <- reactiveVal(character(0))
  rv_log <- reactiveVal(data.frame(
    date = character(0), token = character(0), speaker = character(0),
    tone = character(0), action = character(0), to = character(0),
    note = character(0), stringsAsFactors = FALSE))

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
      tags$p(style = "margin: 4px 0 8px 0;",
        "Re-label tokens whose tone category needs revising, or mark tokens to exclude. ",
        "Use this for genuine ", tags$em("linguistic variation"), ": tone splits or mergers, ",
        "variant readings (colloquial vs. literary, ", HTML("文白异读"),
        "), sandhi or sociolinguistic variants, or mis-elicited tokens. It is ", tags$strong("not"),
        " for pitch-tracking errors, which belong in ",
        tags$strong("Inspect"), " and ", tags$strong("F0 Correction"), "."),
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

  # ---- sidebar: setup + curate actions ----
  output$ui_curate <- renderUI({
    vars <- if (!is.null(dataset())) names(dataset()) else c("No dataset available")
    data_types <- if (!is.null(dataset())) sapply(dataset(), class) else rep("NA", length(vars))
    var_types <- paste0(vars, " {", data_types, "}")
    tone_default <- guess_var(vars, var_patterns$tone, 5)
    tone_choices <- if (!is.null(dataset()) && tone_default %in% names(dataset()))
                      sort(unique(as.character(dataset()[[tone_default]]))) else NULL
    tagList(
      wellPanel(
        h5("Dataset",
           tags$small(style = "color: #777; margin-left: 6px; font-weight: 400;", input$dataset_name)),
        selectInput("curate_token_var", "Token ID variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$token, 1)),
        selectInput("curate_tone_var", "Tone category variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$tone, 5)),
        selectInput("curate_speaker_var", "Speaker variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$speaker, 4)),
        selectInput("curate_f0_var", "f0 (Hz) variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$f0, 2)),
        tags$hr(),
        h5("Curate the selection"),
        uiOutput("curate_sel_count"),
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
                         icon = icon("rotate-left"), class = "btn-sm")),
        div(style = "margin-top: 6px;",
            actionButton("curate_exclude_relabelled", "Exclude all relabelled",
                         icon = icon("ban"), class = "btn-sm")),
        tags$div(style = "color: #888; font-size: 0.75rem; margin-top: 3px;",
                 "Shortcut: also exclude every token you have relabelled (e.g. to drop a separated variant from the analysis)."),
        tags$hr(),
        actionButton("curate_reset", "Reset all curation", class = "btn-outline-secondary btn-sm"),
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
    facet_choices <- c("None (single overlay)" = "__none__", "Speaker" = v$speaker)
    lc <- lex_col()
    if (!is.null(lc)) facet_choices <- c(facet_choices, stats::setNames(lc, paste0("Item (", lc, ")")))
    has_inspect <- length(flagged_tokens()) > 0

    tagList(
      tags$h4("Find tokens to curate"),
      tags$div(style = "display: flex; gap: 10px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 4px;",
        selectInput("curate_view_tone", "Tone (one at a time):",
                    choices = tones, selected = tones[1], width = "140px"),
        selectInput("curate_facet_by", "Facet by:",
                    choices = facet_choices, selected = "__none__", width = "190px")
      ),
      tags$div(style = "display: flex; gap: 6px; align-items: center; flex-wrap: wrap; margin-bottom: 4px;",
        tags$span(style = "font-size: 0.8rem; color: #777;", "Select (this tone):"),
        if (has_inspect)
          actionButton("curate_sel_flagged", "Flagged", class = "btn-sm", icon = icon("flag")),
        actionButton("curate_sel_all", "All", class = "btn-sm"),
        actionButton("curate_invert", "Invert", class = "btn-sm"),
        actionButton("curate_clear_selection", "Clear", class = "btn-sm btn-outline-secondary",
                     icon = icon("xmark"))
      ),
      if (has_inspect)
        tags$div(style = "background:#fff8e1; border-left:4px solid #e0a800; padding:6px 10px; border-radius:4px; font-size:0.8rem; color:#8a6d00; margin-bottom:6px;",
          HTML("&#9888; Flags are candidates, not verdicts. Review each visually before relabelling: a flagged (amber) token may simply have an unusual but valid shape rather than a different tone category.")),
      tags$p(style = "color: #777; font-size: 0.82rem; margin: 2px 0 4px 0;",
        "Drag a ", tags$strong("box"), " (or pick the ", tags$strong("lasso"),
        " in the plot toolbar) around a cluster to select it; selections accumulate. ",
        if (has_inspect) tagList(tags$span(style="color:#c9920a;font-weight:600;","Amber"),
                                 " = flagged by Inspect. ") else NULL,
        tags$span(style="color:#d9534f;font-weight:600;","Red"), " = selected. ",
        "Then relabel or exclude the selection in the left panel."),
      uiOutput("curate_selection_info"),
      uiOutput("curate_plot_holder")
    )
  })

  output$curate_plot_holder <- renderUI({
    d <- dataset(); req(d); v <- cvar()
    fct <- input$curate_facet_by; h <- 520
    if (!is.null(fct) && fct != "__none__" && fct %in% names(d)) {
      n <- length(unique(as.character(d[[fct]][as.character(d[[v$tone]]) == input$curate_view_tone])))
      h <- max(420, ceiling(n / 3) * 240)
    }
    plotly::plotlyOutput("curate_plot", height = paste0(h, "px"))
  })

  output$curate_plot <- plotly::renderPlotly({
    d <- dataset(); req(d, input$curate_view_tone)
    v <- cvar(); req(all(c(v$tone, v$token, v$f0) %in% names(d)))
    sub <- d[as.character(d[[v$tone]]) == input$curate_view_tone, , drop = FALSE]
    req(nrow(sub) > 0)

    time_guess <- grep("^time|^t$|^timepoint|^measurement|^index", names(d), ignore.case = TRUE, value = TRUE)
    if (length(time_guess) == 0) {
      sub$.x <- stats::ave(seq_len(nrow(sub)), as.character(sub[[v$token]]), FUN = seq_along)
      xlab <- "sample index"
    } else { sub$.x <- sub[[time_guess[1]]]; xlab <- time_guess[1] }

    sub$.tok <- as.character(sub[[v$token]])
    flagged <- flagged_tokens(); selected <- rv_selection()
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
      ggplot2::labs(x = xlab, y = "f0 (Hz)", title = paste0("Tone ", input$curate_view_tone))

    fct <- input$curate_facet_by
    if (!is.null(fct) && fct != "__none__" && fct %in% names(sub)) {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[fct]]), scales = "free_y", ncol = 3)
    }

    gg <- plotly::ggplotly(p, tooltip = "customdata")
    gg <- plotly::layout(gg, dragmode = "select")
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

  # tokens of the currently-viewed tone (the active filter)
  tone_tokens <- reactive({
    d <- dataset(); req(d, input$curate_view_tone); v <- cvar()
    unique(as.character(d[[v$token]][as.character(d[[v$tone]]) == input$curate_view_tone]))
  })

  # Select flagged within the current tone only
  observeEvent(input$curate_sel_flagged, {
    tt <- tone_tokens()
    lvl <- intersect(level_flagged_tokens(), tt); fl <- intersect(flagged_tokens(), tt)
    add <- if (length(lvl) > 0) lvl else fl
    if (length(add) == 0) {
      showNotification("No flagged tokens in this tone.", type = "warning", duration = 4); return() }
    rv_selection(union(rv_selection(), add))
  })
  # Select all tokens of the current tone
  observeEvent(input$curate_sel_all, { rv_selection(union(rv_selection(), tone_tokens())) })
  # Invert selection within the current tone (selections in other tones kept)
  observeEvent(input$curate_invert, {
    tt <- tone_tokens(); cur <- rv_selection()
    rv_selection(union(setdiff(cur, tt), setdiff(tt, cur)))
  })
  observeEvent(input$curate_clear_selection, { rv_selection(character(0)) })

  output$curate_selection_info <- renderUI({
    sel <- rv_selection()
    if (length(sel) == 0) {
      return(tags$div(style = "font-size: 0.85rem; color: #888; margin: 2px 0 4px 0;",
                      "Nothing selected yet."))
    }
    preview <- paste(utils::head(sel, 10), collapse = ", ")
    if (length(sel) > 10) preview <- paste0(preview, ", …")
    tags$div(style = "font-size: 0.88rem; margin: 2px 0 4px 0;",
      tags$strong(sprintf("%d token(s) selected: ", length(sel))),
      tags$span(style = "color: #555;", preview))
  })

  # ---- apply actions (on the current selection) ----
  observeEvent(input$curate_relabel_apply, {
    toks <- rv_selection(); lab <- input$curate_new_label; reason <- input$curate_note %||% ""
    if (length(toks) == 0) { showNotification("Select one or more tokens first.", type = "warning", duration = 4); return() }
    if (is.null(lab) || !nzchar(lab)) { showNotification("Choose or type a label to relabel to.", type = "warning", duration = 4); return() }
    rel <- rv_relabel(); rel[toks] <- lab; rv_relabel(rel)
    if (nzchar(reason)) { nt <- rv_note(); nt[toks] <- reason; rv_note(nt) }
    log_curate("relabel", toks, to = lab, note = reason)
    showNotification(sprintf("Relabelled %d token(s) to \"%s\".", length(toks), lab), type = "message", duration = 3)
  })
  observeEvent(input$curate_exclude_apply, {
    toks <- rv_selection(); reason <- input$curate_note %||% ""
    if (length(toks) == 0) { showNotification("Select one or more tokens first.", type = "warning", duration = 4); return() }
    rv_exclude(union(rv_exclude(), toks))
    if (nzchar(reason)) { nt <- rv_note(); nt[toks] <- reason; rv_note(nt) }
    log_curate("exclude", toks, note = reason)
    showNotification(sprintf("Excluded %d token(s).", length(toks)), type = "message", duration = 3)
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
