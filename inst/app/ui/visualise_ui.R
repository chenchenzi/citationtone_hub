visualise_ui <- function(input, output, session, dataset, normalised_data, curated_norm_data = NULL,
                         start_cols = NULL, data_version = NULL, start_settings = NULL) {

  # Tone colour palette: a 12-colour qualitative set matched to the app's minty
  # theme. It leads with the brand pine-green (#3B8A6E) and coral accent
  # (#E07A5F) so a typical small tone inventory reads as on-brand, then fans
  # across the hue wheel at a coordinated medium saturation (greens never
  # adjacent). Every colour is saturated enough to read as points and thin lines
  # on the white plot background, replacing brewer "Set3", whose pastel colours
  # (e.g. the near-white #FFFFB3 yellow) vanished on white. 12 colours match the
  # `n_tone_lv <= 12` colour-by-tone cap below, so every coloured tone level gets
  # a distinct, visible colour.
  vis_tone_palette <- c("#3B8A6E", "#E07A5F", "#4F86B3", "#E0A458",
                        "#8E6CA8", "#2FA89B", "#CB5E84", "#B5654A",
                        "#7E9B3E", "#5566A0", "#A88B2D", "#6E7177")

  # The Start-page arguments are NULL when a caller leaves them out. A test may
  # bind this body to its own reactives without them, so look them up. This
  # tab keeps no results, so it has no use for data_version: the plot and the
  # R code follow dataset() as it is.
  start_cols     <- get0("start_cols", ifnotfound = NULL)
  start_settings <- get0("start_settings", ifnotfound = NULL)

  # --- Lines when aligned by landmark ---
  # The token whose frames a line joins: the Start page's token column when
  # the data have it (D8), otherwise the first token-like column.
  vis_join_col <- function(d) {
    st <- start_col(start_cols, "token")
    if (!is.null(st) && st %in% names(d)) st else vis_token_col(names(d))
  }
  # The column that says which rows have f0 when aligned by landmark: the
  # Start page's f0 when the data have it (the Y variable may be intensity),
  # otherwise the Y variable.
  vis_span_f0 <- function(d) {
    scr <- start_col(start_cols, "f0")
    if (!is.null(scr) && scr %in% names(d)) scr else input$y_var
  }
  # The words for a landmark set's parts in labels and comments: the set's
  # own name when it names a unit ("syllable", "syllables"), else "tier1
  # part" and "tier1 parts".
  vis_unit <- function(set, plural = FALSE) {
    units <- c("syllable", "word", "rhyme", "rime", "vowel", "nucleus", "mora", "foot",
               "phrase", "segment", "phone", "onset", "coda", "tone", "interval")
    if (tolower(set) %in% units) paste0(set, if (plural) "s") else paste(set, if (plural) "parts" else "part")
  }
  # The axis title of the aligned plot, the same in the R code. `layout` is
  # the layout drawn: "sequential" (<set>_tseq), "boundary" (<set>_tbound)
  # or "overlay" (<set>_t01).
  vis_align_lab <- function(set, layout, dash = "\u2013") {
    if (identical(layout, "sequential")) sprintf("%s in sequence, first to last f0 in each", vis_unit(set, TRUE))
    else if (identical(layout, "boundary"))
      sprintf("%s in sequence, gap kept at the TextGrid boundaries", vis_unit(set, TRUE))
    else sprintf("time within each %s, first to last f0 (0%s1)", vis_unit(set), dash)
  }
  # The radio's labels for tier `set`: each layout with the column it
  # matches, e.g. "In sequence (syllable_tseq)". The values stay the same
  # for every tier, so a change of tier keeps the layout picked. A tier
  # without its <set>_i column (`has_i` FALSE) has no <set>_tseq or
  # <set>_tbound, so it offers Overlaid only.
  vis_layout_choices <- function(set, has_i = TRUE) {
    if (!isTRUE(has_i))
      return(list(names = sprintf("Overlaid (%s_t01)", set), values = "overlay"))
    list(names  = c(sprintf("In sequence (%s_tseq)", set),
                    sprintf("In sequence, gap kept (%s_tbound)", set),
                    sprintf("Overlaid (%s_t01)", set)),
         values = c("sequential", "boundary", "overlay"))
  }
  # Whether tier `set` has its <set>_i column in the columns `vars`, so the
  # layouts in sequence can be drawn (TRUE for a name that is not a tier).
  vis_set_has_i <- function(set, vars) {
    s <- landmark_sets(vars)[[set]]
    is.null(s) || isTRUE(s$has_i)
  }
  # The grey hint under the radio, in the words of tier `set` ("syllable",
  # "syllables"). Without <set>_i it describes Overlaid only, and says why.
  vis_align_hint <- function(set, has_i = TRUE) {
    one  <- htmltools::htmlEscape(vis_unit(set))
    many <- htmltools::htmlEscape(vis_unit(set, TRUE))
    style <- "color:#888; font-size:0.76rem; margin-top:-6px;"
    if (!isTRUE(has_i))
      return(tags$div(style = style, nows(
        tags$em("Overlaid"), ": every ", one, " on one 0\u20131 axis. ", tags$em("In sequence"),
        " needs a ", htmltools::htmlEscape(paste0(set, "_i")), " column.")))
    tags$div(style = style, nows(
      tags$em("In sequence"), ": ", one, " 1 at 0 to 1, ", one, " 2 at 1 to 2, and so on, each ",
      "from its first f0 to its last. ", tags$em("In sequence, gap kept"), ": the TextGrid ",
      "boundaries sit at 1, 2, ..., so the gap between ", many, " keeps its room. ",
      tags$em("Overlaid"), ": every ", one, " on one 0\u20131 axis."))
  }
  # The layout to draw, and its column: the two layouts in sequence need the
  # set's <set>_i column, and without it the plot falls back to overlaid.
  vis_layout <- function(layout, ic, vars) {
    if (is.null(layout) || !layout %in% c("sequential", "boundary", "overlay")) layout <- "sequential"
    if (layout != "overlay" && !ic %in% vars) layout <- "overlay"
    layout
  }
  vis_layout_col <- function(set, layout)
    paste0(set, switch(layout, sequential = "_tseq", boundary = "_tbound", "_t01"))
  # The columns a line is grouped by: the token, then the part (`part`, when
  # given and in the data), then voiced_run when the data have it, so a line
  # breaks at the gaps voiced_run marks.
  vis_grp_cols <- function(vars, tok, part = NULL) {
    c(tok, if (!is.null(part) && part %in% vars) part,
      if ("voiced_run" %in% vars) "voiced_run")
  }
  # The groups themselves, the same values as the R code's grp column:
  # paste() with its default space, not interaction(), so a run that is NA
  # stays a group of its own.
  vis_line_groups <- function(d, tok, part = NULL) {
    cols <- vis_grp_cols(names(d), tok, part)
    if (length(cols) == 1) as.character(d[[tok]])
    else do.call(paste, unname(lapply(cols, function(cl) d[[cl]])))
  }

  # --- Column defaults (Start page, D8) ---
  # Y: a normalised column first (the f0_st, f0_zscore, f0_lz and f0_norm*
  # columns that var_patterns$f0 ranks first), then the column the Start page
  # screens (f0_corrected, say, not the raw f0), then the usual guess. X and
  # the other roles: as before.
  vis_f0_default <- function(vars) {
    g   <- guess_var(vars, var_patterns$f0, 2)
    scr <- start_col(start_cols, "f0")
    if (grepl("^f0_(st|zscore|lz)$|^f0_norm", g, ignore.case = TRUE) ||
        is.null(scr) || !scr %in% vars) g else scr
  }
  # The user's own picks in the variable menus: a value a menu sends that
  # differs from the default it was drawn with. A re-render keeps a pick while
  # the data still has the column and the data source is the same. A menu
  # still on its default follows the new default, so a change of column on
  # the Start page reaches it.
  vis_picked <- list()
  vis_shown  <- list()
  vis_src    <- NULL
  for (vis_id in c("x_var", "y_var", "tone_var", "speaker_var")) local({
    i <- vis_id
    observeEvent(input[[i]], {
      v <- input[[i]]
      vis_picked[[i]] <<- if (!identical(v, vis_shown[[i]])) v
    })
  })
  vis_pick <- function(id, vars, default) {
    vis_shown[[id]] <<- default
    v <- vis_picked[[id]]
    if (is.character(v) && length(v) == 1 && v %in% vars) v else default
  }

  # Active dataset for the Visualise tab (uploaded or normalised)
  vis_dataset <- reactive({
    if (!is.null(input$vis_data_source) && input$vis_data_source == "normalised" && !is.null(normalised_data())) {
      normalised_data()
    } else if (identical(input$vis_data_source, "curated_norm") &&
               !is.null(curated_norm_data) && !is.null(curated_norm_data())) {
      curated_norm_data()
    } else {
      dataset()
    }
  })

  # ---- Landmark alignment, shared by the plot, its note and the R code ----
  # Each part of a landmark set (one <set>_start / <set>_end interval in a
  # token) runs from its first f0 (0) to its last f0 (1) on the X (time)
  # variable. This is the call Normalise makes for <set>_t01 and
  # <set>_tseq and <set>_tbound, so the plot and those columns cannot drift
  # apart. "sequential" puts part i at [i - 1, i] (<set>_tseq), so
  # multi-syllable words line up syllable by syllable; "boundary" puts the
  # TextGrid boundaries at 1, 2, ... with the gap kept (<set>_tbound);
  # "overlay" puts every part on one 0-1 axis (<set>_t01). Rows before a
  # part's first f0 or after its last (on <set>_tbound, the token's), rows
  # outside the tier and parts with f0 at fewer than two times are NA, and
  # are left out. NULL when the plot is not aligned. `x` is the aligned
  # axis, one value per row of vis_dataset() (NULL with an `error`).
  # `clock_msg` is the package's message that <set>_tbound is NA in tokens
  # whose time is on another clock than the boundaries; it matters only in
  # the gap-kept layout, and the other messages stay out of the console.
  vis_aligned <- reactive({
    d        <- vis_dataset()
    align_by <- input$vis_align_by
    if (is.null(d) || is.null(align_by) || !nzchar(align_by) || align_by == "__none__" ||
        !all(c(paste0(align_by, "_start"), paste0(align_by, "_end")) %in% names(d)))
      return(NULL)
    tok_col <- vis_join_col(d)
    if (is.null(tok_col)) return(NULL)
    ic     <- paste0(align_by, "_i")
    layout <- vis_layout(input$vis_align_layout, ic, names(d))
    clock_msg <- character(0)
    al <- tryCatch(withCallingHandlers(
      normalise_time_landmarks(d, time = input$x_var, set = align_by,
                               f0 = vis_span_f0(d), token = tok_col),
      message = function(m) {
        if (grepl("_tbound is NA in", conditionMessage(m), fixed = TRUE))
          clock_msg <<- trimws(conditionMessage(m))
        invokeRestart("muffleMessage")
      }),
      error = function(e) conditionMessage(e))
    ok <- is.data.frame(al)
    list(set = align_by, tok = tok_col, ic = ic, layout = layout,
         x = if (ok) al[[vis_layout_col(align_by, layout)]],
         error = if (!ok) al,
         clock_msg = if (layout == "boundary") clock_msg else character(0))
  })
  # The number of parts on an axis in sequence: the largest <set>_i among the
  # rows that are drawn, so no boundary line or part name falls on an
  # empty stretch (e.g. a word on another clock left out of the gap-kept
  # layout). NA when nothing is drawn or <set>_i is not a number.
  vis_n_parts <- function(d, ic, keep) {
    n <- suppressWarnings(max(as.integer(d[[ic]][keep]), na.rm = TRUE))
    if (is.finite(n)) n else NA_integer_
  }

  # Variable guide box on the right panel
  output$visualise_guide <- renderUI({
    tagList(
      guide_box("Plotting guide",
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Data source:"), " Use the uploaded (raw) dataset, the normalised dataset (available after using the Normalise tab), or the curated + normalised dataset (after normalising the curated data)."),
          tags$li(tags$strong("X (time):"), " A time-related or index variable, e.g., normalised time points within a syllable."),
          tags$li(tags$strong("Y (f0):"), " An f0-related variable, e.g., raw Hz or normalised (semitone, z-score, log z-score). It starts on a normalised f0 variable if there is one, otherwise on the f0 variable the Start page screens."),
          tags$li(tags$strong("Tone category:"), " The variable labelling tone types (e.g., T1, T2, T3, T4)."),
          tags$li(tags$strong("Speaker* (optional):"), " A speaker ID variable, used for faceting."),
          tags$li(tags$strong("Align by TextGrid tier (optional):"),
            HTML(paste(
              " Appears when the data have TextGrid tier columns (e.g.",
              "<code>syllable_start</code> / <code>syllable_end</code>, added by F0 Extraction",
              "when you choose TextGrid tiers). Pick a tier, and the X axis is rebuilt from it.",
              "<em>In sequence</em> puts syllable 1 at 0 to 1, syllable 2 at 1 to 2, and so on,",
              "each from its first f0 to its last. <em>In sequence, gap kept</em> puts the",
              "TextGrid boundaries at 1, 2, ..., with the token&rsquo;s first f0 at 0 and its",
              "last f0 at the end. <em>Overlaid</em> puts every syllable on one 0&ndash;1 axis,",
              "each from its first f0 to its last. Rows with f0 are read from the Start",
              "page&rsquo;s f0 variable.",
              "When Normalise uses the same f0 variable, these axes equal",
              "<code>&lt;tier&gt;_tseq</code>, <code>&lt;tier&gt;_tbound</code> and",
              "<code>&lt;tier&gt;_t01</code>.")))
        )
      )
    )
  })

  # The layout radio between tiers: whether it offers all three layouts
  # now (FALSE while it offers Overlaid only, for a tier without
  # <tier>_i), and the layout picked while it did, for the next tier with
  # <tier>_i. Drawing the radio sets both again.
  vis_radio_full  <- TRUE
  vis_layout_kept <- NULL

  # Render the UI for selecting X, Y, tone category, and speaker variables
  output$ui_visualise <- renderUI({
    # Build dataset choices: always have uploaded, conditionally add normalised
    has_norm <- !is.null(normalised_data())
    data_choices <- c("Uploaded data" = "uploaded")
    if (has_norm) {
      data_choices <- c(data_choices, "Normalised data" = "normalised")
    }
    if (!is.null(curated_norm_data) && !is.null(curated_norm_data())) {
      data_choices <- c(data_choices, "Curated + normalised data" = "curated_norm")
    }
    ds_selected <- if (!is.null(input$vis_data_source) && input$vis_data_source %in% data_choices) {
      input$vis_data_source
    } else {
      "uploaded"
    }

    active_data <- vis_dataset()
    vars <- if (!is.null(active_data)) names(active_data) else c("No dataset available")
    data_types <- if (!is.null(active_data)) sapply(active_data, class) else rep("NA", length(vars))
    var_types <- paste0(vars, " {", data_types, "}")
    lm_sets <- landmark_sets(vars)   # landmark columns (X_start/X_end) for alignment
    # Another data source starts the menus on their defaults again.
    if (!identical(vis_src, ds_selected)) { vis_picked <<- list(); vis_src <<- ds_selected }

    tagList(
      wellPanel(
        selectInput("vis_data_source", "Select dataset:",
                    choices = data_choices, selected = ds_selected),
        selectInput("x_var", "Select X (time) variable:",
                    choices = setNames(vars, var_types),
                    selected = vis_pick("x_var", vars, guess_var(vars, var_patterns$time, 1)),
                    multiple = FALSE),
        selectInput("y_var", "Select Y (f0) variable:",
                    choices = setNames(vars, var_types),
                    selected = vis_pick("y_var", vars, vis_f0_default(vars)), multiple = FALSE),
        uiOutput("vis_f0_note"),
        tags$hr(),
        selectInput("tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = vis_pick("tone_var", vars,
                                        role_var(active_data, "tone", vars, var_patterns$tone, 3)),
                    multiple = FALSE),
        checkboxInput("convert_tone_to_factor", "Convert Tone category as factors", TRUE),
        selectInput("speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = vis_pick("speaker_var", vars,
                                        role_var(active_data, "speaker", vars, var_patterns$speaker, 4)),
                    multiple = FALSE),
        tags$hr(),
        if (length(lm_sets) > 0 && !is.null(vis_join_col(active_data))) tagList(
          h5("Align by TextGrid tier ",
             tags$span(style = "font-weight:400; color:#888; font-size:0.8rem;", "(optional)")),
          selectInput("vis_align_by", NULL,
                      choices = c("(raw X axis)" = "__none__",
                                  stats::setNames(names(lm_sets), names(lm_sets))),
                      selected = "__none__"),
          # Hidden until a tier is picked, so the labels start with the first
          # tier's columns (Overlaid only when it has no <tier>_i); the
          # observer below renames them for the tier picked.
          conditionalPanel(
            "input.vis_align_by && input.vis_align_by != '__none__'",
            local({
              full <- isTRUE(lm_sets[[1]]$has_i)
              vis_radio_full  <<- full
              vis_layout_kept <<- NULL
              ch <- vis_layout_choices(names(lm_sets)[1], full)
              radioButtons("vis_align_layout", NULL, choiceNames = ch$names,
                           choiceValues = ch$values,
                           selected = if (full) "sequential" else "overlay")
            }),
            uiOutput("vis_align_hint")
          )
        ) else if (length(lm_sets) > 0) tagList(
          h5("Align by TextGrid tier ",
             tags$span(style = "font-weight:400; color:#888; font-size:0.8rem;", "(optional)")),
          tags$div(style = "color:#999; font-size:0.78rem; font-style:italic;",
            paste("No token variable, so contours cannot be aligned by a TextGrid tier: each part runs",
                  "from its first f0 to its last within a token."))
        ) else tagList(
          h5("Align by TextGrid tier ",
             tags$span(style = "font-weight:400; color:#888; font-size:0.8rem;", "(optional)")),
          tags$div(style = "color:#999; font-size:0.78rem; font-style:italic;",
            paste("No TextGrid tier columns in this dataset. Add them in F0 Extraction",
                  "(choose TextGrid tiers) to align by a tier."))
        ),
        tags$hr(),
        h5("Graph options"),
        radioButtons("plot_facet", NULL,
                     choices = c("All in one plot"         = "none",
                                 "By-Tone plot"            = "tone",
                                 "By-Speaker by-Tone plot" = "speaker_tone"),
                     selected = "none"),
        actionButton("plot_button", "Visualise f0 Contours"),
        downloadButton("save_plot_button", "Save Plot", icon = icon("download")),
        tags$hr(),
        actionButton("show_code_button", "Show R code", icon = icon("code"))
      )
    )
  })


  # The layout radio names the chosen tier's columns ("In sequence
  # (word_tseq)"). Only the labels change: the values stay the same and the
  # layout picked stays picked, so the plot and the R code are as before.
  # A tier without <tier>_i offers Overlaid only; a tier with it offers all
  # three again, with the layout picked before (else In sequence).
  observeEvent(input$vis_align_by, {
    set <- input$vis_align_by
    if (!nzchar(set) || identical(set, "__none__")) return()
    full <- vis_set_has_i(set, names(vis_dataset()))
    if (vis_radio_full) vis_layout_kept <<- input$vis_align_layout
    ch  <- vis_layout_choices(set, full)
    sel <- if (full) vis_layout_kept else "overlay"
    if (is.null(sel) || !sel %in% ch$values) sel <- "sequential"
    vis_radio_full <<- full
    updateRadioButtons(session, "vis_align_layout", choiceNames = ch$names,
                       choiceValues = ch$values, selected = sel)
  })
  # The hint under the radio, in the words of the tier picked (the first
  # tier while none is, as the radio is drawn).
  output$vis_align_hint <- renderUI({
    d <- vis_dataset(); req(d)
    sets <- names(landmark_sets(names(d)))
    set  <- input$vis_align_by
    if (is.null(set) || !set %in% sets) set <- sets[1]
    req(!is.na(set))
    vis_align_hint(set, vis_set_has_i(set, names(d)))
  })

  # A note under the Y menu when the chosen column is not the one the Start
  # page screens and still has rows without f0.
  output$vis_f0_note <- renderUI(screened_f0_note(vis_dataset(), input$y_var, start_cols))

  # Shared reactive to build the plot
  current_plot <- reactive({
    req(input$plot_button > 0)
    req(!is.null(vis_dataset()))
    req(input$x_var, input$y_var, input$tone_var, input$speaker_var)

    plot_data <- vis_dataset()

    # Colour by Tone only when it has a manageable number of distinct values. A
    # high-cardinality column (e.g. a continuous variable mis-selected as Tone,
    # which happens with freshly-extracted f0 data that has no tone labels yet)
    # would explode scale_color_brewer's discrete legend. Compute this on the
    # raw column BEFORE any factor coercion, and only coerce when we will really
    # colour by it: coercing otherwise would also wreck the axis when the same
    # column is mapped to y (e.g. Tone and Y both default to "f0").
    n_tone_lv      <- length(unique(plot_data[[input$tone_var]]))
    colour_by_tone <- n_tone_lv <= 12
    one_col        <- "#3b8a6e"

    # The colour scale needs a discrete tone column, so factor it (e.g. integer
    # codes 1..5) in the order as.factor() gives, except that Chinese tone
    # names keep their traditional order (sort_tones()); ggplot2 would sort
    # them by code point. A factor column keeps its own levels.
    if (colour_by_tone) {
      tone_col <- plot_data[[input$tone_var]]
      if (!is.factor(tone_col) && (is.numeric(tone_col) ||
          isTRUE(input$convert_tone_to_factor) || tone_needs_levels(tone_col))) {
        plot_data[[input$tone_var]] <- factor(tone_col,
                                              levels = sort_tones(tone_col, natural = FALSE))
      }
    }

    # ---- Optional landmark alignment ----------------------------------------
    # vis_aligned() (above) makes the axis; rows where it is NA are left out.
    va      <- vis_aligned()
    aligned <- !is.null(va)
    x_aes <- input$x_var; x_lab <- input$x_var
    seq_layout <- FALSE; max_i <- NA_integer_
    if (aligned) {
      align_by <- va$set; tok_col <- va$tok; ic <- va$ic; layout <- va$layout
      # Both layouts in sequence: dashed boundaries at 1, 2, ..., names at the middles
      seq_layout <- layout != "overlay"
      validate(need(is.null(va$error), paste("Cannot align by", align_by, "-", va$error)))
      plot_data$.aligned_x <- va$x
      plot_data <- plot_data[is.finite(plot_data$.aligned_x), , drop = FALSE]
      # Every token on another clock left out: say why here. With some
      # tokens still drawn, vis_align_warning says it above the plot.
      validate(need(nrow(plot_data) > 0 || !length(va$clock_msg), va$clock_msg))
      if (seq_layout) max_i <- vis_n_parts(plot_data, ic, TRUE)
      x_aes <- ".aligned_x"
      x_lab <- vis_align_lab(align_by, layout)
      # Grouping for the connecting line: by token and part in every layout.
      # Overlaid, so a line does not wrap from the end of one part back to 0.
      # In sequence, part 1's last f0 and part 2's first f0 both sit at 1,
      # so a line across parts would only add a vertical join there, and only
      # with the rows without f0 kept (with them removed, voiced_run breaks
      # the line at a gap). With the gap kept, a line across parts would
      # bridge the gap. Grouped by part, the plot is the same either way.
      # With a voiced_run column, by voiced run too, so the line breaks at a
      # gap. vis_line_groups() builds the same groups as the R code below.
      plot_data$.grp <- vis_line_groups(plot_data, tok_col, part = ic)
    }

    # Use aes() + .data[[var]] (the modern replacement for aes_string(),
    # which was deprecated in ggplot2 3.0). colour_by_tone / one_col were
    # computed above (before any factor coercion of the tone column).
    p <- if (colour_by_tone)
      ggplot(plot_data, aes(x = .data[[x_aes]], y = .data[[input$y_var]],
                            color = .data[[input$tone_var]]))
    else
      ggplot(plot_data, aes(x = .data[[x_aes]], y = .data[[input$y_var]]))

    # Dashed part boundaries when the parts are in sequence (in both such
    # layouts the boundaries sit at 1, 2, ...).
    if (seq_layout && is.finite(max_i) && max_i > 1) {
      p <- p + geom_vline(xintercept = seq_len(max_i - 1), linetype = "dashed",
                          colour = "grey75", linewidth = 0.4)
    }
    # When aligned, connect each token's frames into a contour line.
    if (aligned && ".grp" %in% names(plot_data)) {
      p <- p + (if (colour_by_tone)
                  geom_line(aes(group = .data[[".grp"]]), alpha = 0.45, linewidth = 0.4)
                else
                  geom_line(aes(group = .data[[".grp"]]), colour = one_col,
                            alpha = 0.4, linewidth = 0.4))
    }
    p <- p + (if (colour_by_tone) geom_point(alpha = 0.75)
              else geom_point(colour = one_col, alpha = 0.6))
    if (colour_by_tone) p <- p + scale_color_manual(values = vis_tone_palette)
    p <- p + labs(
      x = x_lab, y = f0_axis_label(input$y_var),
      color = if (colour_by_tone) input$tone_var else NULL)
    # In sequence: each part's name at its middle ("syllable 1"); the
    # dashed lines mark the boundaries at 1, 2, ...
    if (seq_layout && is.finite(max_i)) {
      p <- p + scale_x_continuous(breaks = seq(0.5, max_i - 0.5, by = 1),
                                  labels = paste(vis_unit(align_by), seq_len(max_i)),
                                  minor_breaks = NULL)
    }

    facet_mode <- input$plot_facet
    if (is.null(facet_mode)) facet_mode <- "none"
    if (facet_mode == "tone") {
      p <- p + facet_wrap(reformulate(input$tone_var))
    } else if (facet_mode == "speaker_tone") {
      p <- p + facet_grid(get(input$speaker_var) ~ get(input$tone_var))
    }
    # Room between facets, so the last x label of one panel does not run
    # into the first of the next (wider digits in CJK fonts made 0.30.0).
    if (facet_mode %in% c("tone", "speaker_tone"))
      p <- p + theme(panel.spacing.x = grid::unit(14, "pt"))

    # Chinese labels need a CJK font in the PNG (cjk_family_theme(), global.R)
    p + cjk_family_theme(p)
  })

  # Dynamic plot dimensions (shared by display and download)
  plot_height <- reactive({
    facet_mode <- input$plot_facet
    if (is.null(facet_mode)) facet_mode <- "none"
    if (!is.null(vis_dataset()) && facet_mode == "speaker_tone") {
      300 + 100 * length(unique(vis_dataset()[[input$speaker_var]]))
    } else {
      600
    }
  })

  plot_width <- reactive({
    facet_mode <- input$plot_facet
    if (is.null(facet_mode)) facet_mode <- "none"
    if (!is.null(vis_dataset()) && facet_mode %in% c("tone", "speaker_tone")) {
      n_tones <- length(unique(vis_dataset()[[input$tone_var]]))
      max(400 + 100 * n_tones, 600)
    } else {
      800
    }
  })

  # Render the plot in the UI
  output$ggplot_output <- renderPlot({
    current_plot()
  }, height = function() { plot_height() },
     width = function() { plot_width() })

  # Warn above the plot when the chosen Tone variable has too many distinct
  # values to colour by (usually a continuous column mis-selected as Tone, as
  # happens with freshly-extracted f0 data that has no tone labels yet). The
  # plot still renders, in a single colour.
  output$vis_tone_warning <- renderUI({
    req(input$plot_button > 0, input$tone_var)
    d <- vis_dataset(); req(d)
    if (!input$tone_var %in% names(d)) return(NULL)
    n_lv <- length(unique(d[[input$tone_var]]))
    if (n_lv <= 12) return(NULL)
    tags$div(
      style = paste("background-color: #fff8e1; border-left: 4px solid #e0a800;",
                    "padding: 10px 14px; margin-bottom: 10px; border-radius: 4px;",
                    "color: #7a5d00; font-size: 0.88rem;"),
      icon("triangle-exclamation"),
      HTML(sprintf(
        paste(" The <strong>Tone category</strong> variable <code>%s</code> has <strong>%d</strong>",
              "distinct values, which looks like a continuous variable rather than tone categories.",
              "Contours are shown in a single colour. Pick a categorical tone variable to colour by tone."),
        input$tone_var, n_lv))
    )
  })

  # Above the plot when the gap-kept layout leaves out tokens whose time is on
  # another clock than the TextGrid boundaries while other tokens are still
  # drawn. (With none left, the plot itself says why.)
  output$vis_align_warning <- renderUI({
    req(input$plot_button > 0)
    va <- vis_aligned()
    if (is.null(va) || !length(va$clock_msg) || !any(is.finite(va$x))) return(NULL)
    # The count, from the package's message ("... is NA in 3 token(s): ...")
    n_off <- suppressWarnings(as.integer(sub("^.*_tbound is NA in ([0-9]+) token.*$", "\\1",
                                             va$clock_msg)))
    one   <- identical(n_off, 1L)
    tags$div(
      style = paste("background-color: #fff8e1; border-left: 4px solid #e0a800;",
                    "padding: 10px 14px; margin-bottom: 10px; border-radius: 4px;",
                    "color: #7a5d00; font-size: 0.88rem;"),
      icon("triangle-exclamation"), " ",
      tags$strong(if (is.na(n_off)) "Some tokens are not drawn."
                  else sprintf("%s %s not drawn.", count_label(n_off, "token"),
                               n_verb(n_off, "is", "are"))),
      " ", va$clock_msg,
      sprintf(" In sequence (%s_tseq) and Overlaid (%s_t01) draw %s.", va$set, va$set,
              if (one) "it" else "them")
    )
  })

  # Download handler for saving the plot
  output$save_plot_button <- downloadHandler(
    filename = function() {
      paste0("f0_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      fname <- paste0("f0_plot_", Sys.Date(), ".png")
      ggsave(file, plot = current_plot(),
             width = plot_width() / 100, height = plot_height() / 100,
             dpi = 300, bg = "white")
      showNotification(paste("Plot saved as", fname), type = "message", duration = 4)
    }
  )

  # Toggle visibility of R code block
  code_visible <- reactiveVal(FALSE)

  observeEvent(input$show_code_button, {
    code_visible(!code_visible())
  })

  output$r_code_output <- renderUI({
    req(code_visible())
    req(input$x_var, input$y_var, input$tone_var, input$speaker_var)

    # Friendly nudge when no plot has been generated yet
    if (is.null(input$plot_button) || input$plot_button == 0) {
      return(tags$div(
        style = "background-color: #fff8e1; border-left: 4px solid #e0a800; padding: 12px 14px; margin-top: 12px; border-radius: 4px; color: #555;",
        icon("circle-info"),
        " Click ", tags$strong("Visualise f0 Contours"),
        " first to generate the plot, then the corresponding R code will appear here."
      ))
    }

    # Build the R code string dynamically.
    # Use the actual uploaded filename if known so the snippet visually
    # matches the dataset the plot was drawn from; users may still need
    # to adjust the path to where the file lives on their machine.
    ds_name <- if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                 paste0(input$dataset_name, ".csv")
               else "your_data.csv"

    # The read, then the Start-page steps dataset() ran (read_data_code(),
    # global.R), so the code starts from the same rows as the plot.
    code_lines <- c(
      "library(tidyverse)",
      "",
      read_data_code(ds_name, start = if (is.null(start_settings)) NULL else start_settings()),
      ""
    )

    # Colour by tone only when it has few enough levels (mirrors the plot).
    cbt <- is.null(vis_dataset()) ||
           length(unique(vis_dataset()[[input$tone_var]])) <= 12
    tone_now <- if (!is.null(vis_dataset())) vis_dataset()[[input$tone_var]]
    if (cbt && (isTRUE(input$convert_tone_to_factor) || tone_needs_levels(tone_now))) {
      code_lines <- c(code_lines,
        "# Convert tone category to factor",
        tone_factor_code(input$tone_var, tone_now),
        ""
      )
    }

    # Landmark alignment, mirroring the displayed plot: the same
    # normalise_time_landmarks() call, on dat.
    align_by <- input$vis_align_by
    tok_code <- if (!is.null(vis_dataset())) vis_join_col(vis_dataset()) else NULL
    aligned  <- !is.null(align_by) && nzchar(align_by) && align_by != "__none__" &&
                !is.null(vis_dataset()) && !is.null(tok_code) &&
                all(c(paste0(align_by, "_start"), paste0(align_by, "_end")) %in% names(vis_dataset()))
    x_code <- input$x_var; x_lab_code <- input$x_var
    seqL <- FALSE; n_seg <- NA_integer_
    if (aligned) {
      ic <- paste0(align_by, "_i")
      layout <- vis_layout(input$vis_align_layout, ic, names(vis_dataset()))
      seqL  <- layout != "overlay"
      span_f0 <- vis_span_f0(vis_dataset())
      q <- function(x) encodeString(x, quote = '"')
      code_lines <- c(code_lines,
        if (layout == "boundary") c(
          sprintf("# The %s in sequence, gap kept: 0 at the token's first f0, the TextGrid",
                  vis_unit(align_by, TRUE)),
          "# boundaries at 1, 2, ... and the token's last f0 at the end",
          sprintf("# (rows with f0 in column %s). Rows before the first f0 or after the last", q(span_f0)),
          "# are NA and left out.")
        else c(
          if (layout == "sequential") c(
            sprintf("# The %s in sequence: %s 1 at 0 to 1, %s 2 at 1 to 2, and so on,",
                    vis_unit(align_by, TRUE), vis_unit(align_by), vis_unit(align_by)),
            "# each from its first f0 to its last")
          else
            sprintf("# Time within each %s: 0 at its first f0 and 1 at its last f0", vis_unit(align_by)),
          sprintf("# (rows with f0 in column %s). Rows before the first f0 or after the last", q(span_f0)),
          sprintf("# are NA and left out, and so are %s with f0 at fewer than two times.",
                  vis_unit(align_by, TRUE))),
        sprintf("al <- shinytone::normalise_time_landmarks(dat, time = %s, set = %s,",
                q(input$x_var), q(align_by)),
        sprintf("                                          f0 = %s, token = %s)",
                q(span_f0), q(tok_code)),
        sprintf("dat$aligned_x <- al[[%s]]", q(vis_layout_col(align_by, layout))),
        "dat <- dat[is.finite(dat$aligned_x), ]")
      # The plot's line groups (vis_line_groups()): voiced_run only when the
      # plot data have it, and so does dat, which repeats the Start page's
      # removal (or reads a file that already has the column).
      grp_cols <- vis_grp_cols(names(vis_dataset()), tok_code, part = ic)
      per <- c(if (ic %in% grp_cols) paste(align_by, "part"),
               if ("voiced_run" %in% grp_cols) "voiced run")
      code_lines <- c(code_lines,
        if (length(per))
          paste0("# One line per token, ", paste(per, collapse = " and "),
                 if ("voiced_run" %in% grp_cols) ",\n# so a line breaks where voiced_run marks a gap"),
        if (length(grp_cols) == 1) paste0("dat$grp <- dat$", tok_code)
        else paste0("dat$grp <- paste(", paste0("dat$", grp_cols, collapse = ", "), ")"))
      code_lines <- c(code_lines, "")
      x_code     <- "aligned_x"
      x_lab_code <- vis_align_lab(align_by, layout, dash = "-")
      # The parts drawn, as in the plot (vis_n_parts())
      if (seqL) {
        va    <- vis_aligned()
        n_seg <- vis_n_parts(vis_dataset(), ic, if (is.null(va$x)) TRUE else is.finite(va$x))
      }
    }

    code_lines <- c(code_lines, "# Create the plot")
    if (cbt) {
      code_lines <- c(code_lines,
        paste0('p <- ggplot(dat, aes(x = ', x_code, ', y = ', input$y_var, ', color = ', input$tone_var, ')) +'))
    } else {
      code_lines <- c(code_lines,
        paste0("# Tone variable '", input$tone_var, "' has too many values to colour by; using one colour."),
        paste0('p <- ggplot(dat, aes(x = ', x_code, ', y = ', input$y_var, ')) +'))
    }
    if (seqL && is.finite(n_seg) && n_seg > 1) {
      code_lines <- c(code_lines,
        paste0('  geom_vline(xintercept = 1:', n_seg - 1, ', linetype = "dashed", colour = "grey75") +'))
    }
    if (aligned && !is.null(tok_code)) {
      code_lines <- c(code_lines,
        if (cbt) '  geom_line(aes(group = grp), alpha = 0.45, linewidth = 0.4) +'
        else     '  geom_line(aes(group = grp), colour = "#3b8a6e", alpha = 0.4, linewidth = 0.4) +')
    }
    code_lines <- c(code_lines,
      if (cbt) "  geom_point(alpha = 0.75) +"
      else     '  geom_point(colour = "#3b8a6e", alpha = 0.6) +')
    if (cbt) code_lines <- c(code_lines,
      paste0('  scale_color_manual(values = c(',
             paste0('"', vis_tone_palette, '"', collapse = ", "), ')) +'))
    code_lines <- c(code_lines,
      if (cbt) paste0('  labs(x = "', x_lab_code, '", y = "', f0_axis_label(input$y_var), '", color = "', input$tone_var, '")')
      else     paste0('  labs(x = "', x_lab_code, '", y = "', f0_axis_label(input$y_var), '")'))
    if (seqL && is.finite(n_seg)) {
      code_lines <- c(code_lines,
        paste0('p <- p + scale_x_continuous(breaks = seq(0.5, ', n_seg - 0.5, ', by = 1),'),
        paste0('                            labels = paste(', q(vis_unit(align_by)), ', 1:', n_seg, '))'))
    }

    facet_mode <- input$plot_facet
    if (is.null(facet_mode)) facet_mode <- "none"
    if (facet_mode == "tone") {
      code_lines <- c(code_lines,
        "",
        "# Add faceting by tone",
        paste0('p <- p + facet_wrap(~ ', input$tone_var, ')')
      )
    } else if (facet_mode == "speaker_tone") {
      code_lines <- c(code_lines,
        "",
        "# Add faceting by speaker and tone",
        paste0('p <- p + facet_grid(', input$speaker_var, ' ~ ', input$tone_var, ')')
      )
    }

    code_lines <- c(code_lines,
      "",
      "p",
      "",
      "# If you would like to save it as a PNG",
      paste0('ggsave("my_plot.png", plot = p, width = ', plot_width() / 100, ', height = ', plot_height() / 100, ', dpi = 300)')
    )

    code_text <- paste(code_lines, collapse = "\n")

    tagList(
      tags$p(style = "text-align: center; color: #999; font-style: italic; margin-top: 8px;",
             icon("arrow-down"), " Scroll down to see the R code"),
      tags$hr(),
      tags$div(
        style = "position: relative;",
        tags$h5(icon("code"), " R Code"),
        tags$pre(
          style = "padding: 15px; border-radius: 5px; overflow-x: auto;",
          tags$code(code_text)
        )
      )
    )
  })
}
