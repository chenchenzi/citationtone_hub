###############################################
# Inspect tab: F0 outlier & artefact detection
###############################################

inspect_ui <- function(input, output, session, dataset, inspect_result = reactiveVal(NULL),
                       start_cols = NULL, data_version = NULL, start_settings = NULL) {

  # Guide text for the Inspect tab
  output$inspect_guide <- renderUI({
    guide_box("Inspect guide",
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("f0 (Hz):"), " The raw fundamental frequency variable in Hertz."),
          tags$li(tags$strong("Token ID:"), " A unique identifier for each token/syllable (groups rows belonging to the same contour)."),
          tags$li(tags$strong("Time:"), " The time variable that orders f0 samples within each token. Use real time, not a 0-1 axis."),
          tags$li(tags$strong("Time unit:"), " Seconds or milliseconds. It is set from the data: the median step between rows within a token, shown under the buttons. A step below 1 means seconds, otherwise milliseconds. Check it: with the wrong unit the rate test flags almost nothing, or almost everything."),
          tags$li(tags$strong("Speaker:"), " A speaker ID for by-speaker z-score computation."),
          tags$li(tags$strong("Tone category (optional):"), " The variable labelling tone types. Enables the token-level (by-tone) check; leave as ", tags$em("(none)"), " to skip it, e.g. before tone categories are known (tone discovery)."),
          tags$li(tags$strong("Intensity (dB, optional):"), " A per-frame intensity variable (e.g. from the Praat script or in-app wrassp extraction). Enables the low-intensity check; leave as ", tags$em("(none)"), " if your data has no intensity.")
        ),
        tags$strong("Three complementary checks:"),
        tags$ul(style = "margin: 4px 0 8px 0; padding-left: 18px;",
          tags$li(
            tags$em("Extreme-value"), " (speaker-level): per-token max / min, z-scored against the ",
            "speaker's other tokens. Flags a token mis-tracked as a whole, such as an octave ",
            "too high or low, or one pushed to an extreme by creak or diplophonia."
          ),
          tags$li(
            tags$em("Token-level"), " (speaker \u00d7 tone): per-token median, compared against the ",
            HTML("<strong>same speaker and tone</strong>."),
            " Flags a contour smoothly shifted too high ",
            "or low for its tone but still appearing normal for the speaker."
          ),
          tags$li(
            tags$em("Sample-level"), " (within token): rate of change, octave bounds, and carryover, ",
            HTML("<strong>sample by sample</strong>, skipping the rows without f0."),
            " Flags individual mis-tracked samples in an otherwise normal token."
          )
        ),
        tags$p(style = "margin: 0 0 8px 0;",
          tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "flagged_token = TRUE"),
          " whenever any check fires. The token-level check needs at least 5 same-speaker-same-tone tokens with f0, and is more reliable with more (repetitions, different items, etc.)."
        ),
        tags$p(style = "margin: 0 0 8px 0;",
          "Rows without f0 are never flagged as samples, and the checks skip them; they keep their token's flag. The summary counts samples ",
          "with f0, so its numbers are the same whether or not the Start page removed those rows. ",
          "A token with no f0 at all gets the note ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "no f0"),
          " and is left out of the counts."
        ),
        tags$strong("Detection methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Extreme-value (speaker-level):"), " Computes per-token summary statistics (max, min, mean, SD), then z-scores the per-token ", tags$em("max"), " and ", tags$em("min"), " within each speaker. Tokens whose max or min exceeds the threshold are flagged as ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "max too high"), " or ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "min too low"), ". The mean and SD are reported alongside (in the table and tooltips) for context, but the flag decision uses only the max / min z-scores."),
          tags$li(tags$strong("Token-level (speaker \u00d7 tone):"), " Computes each token's median f0 in semitones, then a ", tags$em("modified z-score"), " (median/MAD-based; Iglewicz & Hoaglin, 1993) against other tokens of the same speaker and tone. Tokens beyond the threshold are flagged ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "level too high"), " or ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "level too low"), ". This catches smoothly shifted contours that the other checks miss. The median (not mean) is used so a few jump-flagged frames don't shift a token's apparent level; groups with fewer than the minimum number of tokens are skipped."),
          tags$li(tags$strong("Jump detection:"), " Compares each sample with the next sample that has f0, in semitones, with the rate scaled by the real time between them. Flags where the change exceeds a rise or fall threshold, labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "jump (rise)"), " or ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "jump (fall)"), ". When a jump is detected, the side ", tags$em("farther from the token median"), " is flagged, so artefacts at the start of a token are caught correctly. Adapted from Steffman & Cole (2022)."),
          tags$li(tags$strong("Jumps across a gap:"), " A gap is a stretch inside a token with no f0. When the two samples of a jump lie on either side of one, the note says so, e.g. ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "jump (rise) across a 20 ms gap"), " (the step minus the usual frame step). The flag then goes on the shorter side when it is a run of at most 5 samples, since a short fragment cut off by a gap is the likelier error. Otherwise the side farther from the token median is flagged, as above. Gaps are found from the time steps, so only when the times form a regular grid (see the Start page report). When they do not, no gap note is given, and the flag goes on the side farther from the token median."),
          tags$li(tags$strong("Octave jumps:"), " Flags samples where the Hz ratio to the previous sample with f0 is < 0.49 or > 1.99 (pitch halving/doubling), labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "octave jump"), "."),
          tags$li(tags$strong("Carryover:"), " Samples ", tags$em("around"), " a detected artefact (forward and backward) that stay within ", tags$em("mult"), "\u00d7 the rise/fall threshold (in semitones) of the artefact\u2019s f0. These may still be erroneous even if their own step is small, labelled ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "carryover"), ". The band is direction-specific: ", tags$code("rise_threshold \u00d7 mult"), " when the trend is rising, ", tags$code("fall_threshold \u00d7 mult"), " when falling (adapted from Steffman & Cole, 2022). The multiplier defaults to ", tags$strong("1.5"), " (paper\u2019s value) and is configurable in the sidebar (set to 0 to disable)."),
          tags$li(HTML(paste0(
            "<strong>Low intensity (optional):</strong> When an intensity variable is selected, ",
            "flags voiced samples sitting more than the dB threshold below their token&rsquo;s ",
            "<em>peak</em> intensity, typically at voicing onsets/offsets and devoiced or creaky ",
            "tails, where f0 is least reliable. Labelled ",
            "<code style='color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;'>",
            "low intensity</code>. This check is <strong>advisory</strong>: it is noted in ",
            "<code>flag_notes</code> and counted in the summary, but does not on its own set ",
            "<code>flagged_token</code> (a quiet but modal frame can track fine). It is most ",
            "valuable where it co-occurs with a jump, corroborating that the jump is a tracking error.")))
        ),
        tags$strong("Default thresholds:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Extreme-value (z-score) = 3:"), " A standard convention for outlier detection (\u00b13 SD covers 99.7% of a normal distribution)."),
          tags$li(tags$strong("Rise = 1.263 ST, Fall = 1.714 ST per 10ms:"), " Based on the maximum rate of f0 change in human speech production (Sundberg, 1973). Changes exceeding these rates are physiologically implausible and likely tracking errors."),
          tags$li(tags$strong("Token-level (modified z) = 3.5:"), " The cutoff recommended by Iglewicz & Hoaglin (1993) for the median/MAD modified z-score, the robust analogue of the \u00b13 SD rule. Requires at least 5 same-speaker-same-tone tokens (more is more reliable)."),
          tags$li(tags$strong("Low-intensity drop = 15 dB:"), " Voiced frames more than 15 dB below the token\u2019s peak intensity are flagged. The comparison is relative-to-peak (not an absolute dB level), so it transfers across recordings regardless of microphone distance or gain.")
        )
    )
  })

  # ---- Column pickers --------------------------------------------------
  # The token and f0 pickers default to the Start page's columns (D8): its
  # token column, and its f0 column when that column is in Hz, as Inspect
  # works on Hz only. The other pickers keep their own guesses.
  # A picker shows its default until the user picks another column. That
  # pick then stays while it is still a column of the data, even when the
  # Start page's columns or the data change. A value that differs from the
  # default this tab last put in the picker is the user's own pick.
  insp_pickers <- c("inspect_f0_var", "inspect_token_var", "inspect_time_var",
                    "inspect_speaker_var", "inspect_tone_var", "inspect_intensity_var")
  picks <- new.env(parent = emptyenv())
  picks$offered <- list()
  picks$user    <- list()
  pick_col <- function(id, choices, default) {
    picks$offered[id] <- list(default)
    u <- picks$user[[id]]
    if (!is.null(u) && u %in% choices) u else default
  }
  for (id in insp_pickers) local({
    i <- id
    observeEvent(input[[i]], {
      v <- input[[i]]
      picks$user[i] <- list(if (identical(v, picks$offered[[i]])) NULL else v)
    })
  })

  # An f0 column in Hz: not a normalised name (f0_st, f0_zscore, f0_lz,
  # f0_norm...), at most one negative value (an unvoiced marker such as -1),
  # and positive values with a median of 30 to 2000.
  hz_like <- function(x, name) {
    if (!is.numeric(x) ||
        grepl("(^|_)(st|z|zscore|lz|erb|log|semitones?)(_|$)|norm", name, ignore.case = TRUE))
      return(FALSE)
    neg <- unique(x[is.finite(x) & x < 0])
    pos <- x[is.finite(x) & x > 0]
    length(neg) <= 1 && length(pos) > 0 &&
      stats::median(pos) >= 30 && stats::median(pos) <= 2000
  }

  # The default of each picker for data `d` (NULL before an upload).
  insp_defaults <- function(d) {
    vars <- if (!is.null(d)) names(d) else "No dataset available"
    tok  <- start_col(start_cols, "token")
    scr  <- start_col(start_cols, "f0")
    hit  <- vars[grepl("intens|rms|energy|\\bdb\\b", vars, ignore.case = TRUE)]
    list(
      inspect_f0_var = if (!is.null(scr) && scr %in% vars && hz_like(d[[scr]], scr)) scr
                       else guess_var(vars, c("^f0_hz$", "^f0$", var_patterns$f0), 1),
      inspect_token_var = if (!is.null(tok) && tok %in% vars) tok
                          else guess_var(vars, var_patterns$token, 2),
      inspect_time_var = guess_var(vars, var_patterns$time, 3),
      inspect_speaker_var = guess_var(vars, var_patterns$speaker, 4),
      inspect_tone_var = guess_var(vars, var_patterns$tone, 5),
      inspect_intensity_var = if (length(hit)) hit[1] else "")
  }

  # The columns' names and types. The sidebar is drawn again only when these
  # change, not when only the rows change (a box on the Start page), so a
  # change of rows keeps every setting. A reactiveVal signals only a real
  # change.
  insp_cols <- reactiveVal(NULL)
  observe({
    d <- dataset()
    insp_cols(if (is.null(d)) NULL else list(vars = names(d), types = sapply(d, class)))
  }, priority = 1)

  # When the Start page's token or f0 column changes, a picker the user has
  # not set follows it.
  if (is.function(start_cols)) {
    observeEvent(start_cols(), {
      d <- dataset()
      if (is.null(d)) return()
      def <- insp_defaults(d)
      for (id in c("inspect_token_var", "inspect_f0_var")) {
        cur <- input[[id]]
        u   <- picks$user[[id]]
        if (is.null(cur) || (!is.null(u) && u %in% names(d))) next
        picks$offered[id] <- list(def[[id]])
        if (!identical(cur, def[[id]])) updateSelectInput(session, id, selected = def[[id]])
      }
    }, ignoreInit = TRUE)
  }

  # Under the f0 picker: a note when the column is not the one the Start
  # page screens and still has rows without f0.
  output$inspect_f0_note <- renderUI({
    screened_f0_note(dataset(), input$inspect_f0_var, start_cols)
  })

  # ---- Time unit ---------------------------------------------------------
  # The rate test reads the time step in the chosen unit, so millisecond
  # data left at "s" would turn it off. The unit is guessed from the data:
  # the median step between consecutive rows within a token (in time
  # order, rows with a time only), below 1 means seconds, otherwise ms.
  # dataset() is the screened data, and the step is taken on the Inspect
  # token and time columns. A time column that is not numeric (text or a
  # factor) gives no step, as inspect_f0() does not accept it.
  insp_time_step <- function(d, tok, tim) {
    if (is.null(d) || !is.character(tok) || !is.character(tim) ||
        length(tok) != 1 || length(tim) != 1 || !all(c(tok, tim) %in% names(d)) ||
        !is.numeric(d[[tim]]))
      return(NULL)
    tv <- d[[tim]]
    ok <- is.finite(tv)
    if (sum(ok) < 2) return(NULL)
    g  <- as.character(d[[tok]])[ok]
    tv <- tv[ok]
    o  <- order(g, tv, method = "radix")
    g  <- g[o]
    tv <- tv[o]
    same <- (g[-1] == g[-length(g)]) %in% TRUE
    st <- diff(tv)[same]
    st <- st[is.finite(st) & st > 0]
    if (!length(st)) NULL else stats::median(st)
  }
  # The column a picker has now, or its default before it is drawn (or
  # when its column is gone).
  insp_col_now <- function(d, id) {
    v <- input[[id]]
    if (is.character(v) && length(v) == 1 && v %in% names(d)) v
    else insp_defaults(d)[[id]]
  }
  insp_step <- reactive({
    d <- dataset()
    if (is.null(d)) return(NULL)
    insp_time_step(d, insp_col_now(d, "inspect_token_var"),
                   insp_col_now(d, "inspect_time_var"))
  })
  # The guessed unit. A reactiveVal signals only a real change, so a new
  # step that gives the same guess changes nothing.
  unit_guess <- reactiveVal(NULL)
  observe({
    s <- insp_step()
    unit_guess(if (is.null(s)) NULL else if (s < 1) "s" else "ms")
  }, priority = 1)

  # The radio shows the guess until the user picks a unit by hand. That pick
  # stays until the next upload, even when the data or the columns change
  # the guess. A value that differs from the one this tab last put in the
  # radio is the user's own pick. `sent` is a value sent to the radio that
  # the browser has not confirmed yet. The browser applies the updates in
  # order, so while one is pending the radio will end on `sent`, and a new
  # guess is compared with it, not with the value the browser last
  # confirmed (else a guess that changes and changes back before the browser
  # answers is never sent, and the radio ends on the stale value). An update
  # back to the confirmed value may get no answer (the input does not
  # change), so it is not kept as pending; if the browser reports the
  # earlier update first, the next report sets the pick back.
  units <- new.env(parent = emptyenv())
  units$offered <- NULL
  units$user    <- NULL
  units$sent    <- NULL
  unit_now <- function() {
    if (!is.null(units$user)) return(units$user)
    if (!is.null(units$offered)) return(units$offered)
    g <- isolate(unit_guess())
    if (is.null(g)) "s" else g
  }
  set_unit <- function(u) {
    if (is.null(u)) return(invisible())
    units$offered <- u
    cur  <- isolate(input$inspect_time_unit)
    last <- if (!is.null(units$sent)) units$sent else cur
    if (!is.null(cur) && !identical(last, u)) {
      units$sent <- if (identical(cur, u)) NULL else u
      updateRadioButtons(session, "inspect_time_unit", selected = u)
    }
  }
  observeEvent(input$inspect_time_unit, {
    v <- input$inspect_time_unit
    units$sent <- NULL
    units$user <- if (identical(v, units$offered)) NULL else v
  })
  # A new upload: the data choose the unit again. If the new data give
  # another guess, the observer below sets it when the guess changes.
  observeEvent(input$uploadfile, {
    units$user <- NULL
    set_unit(unit_guess())
  }, ignoreInit = TRUE)
  observeEvent(unit_guess(), {
    if (is.null(units$user)) set_unit(unit_guess())
  })

  # Under the radio: the step, read in the chosen unit, and a warning when
  # the unit does not fit the step, or when the time column is not numeric
  # (the run would stop).
  output$inspect_time_step <- renderUI({
    u <- input$inspect_time_unit
    if (is.null(u)) return(NULL)
    warn_style <- "color: #6b5a1e; background: #fff8e1; border-radius: 4px; padding: 5px 8px;"
    d   <- dataset()
    tim <- if (!is.null(d)) insp_col_now(d, "inspect_time_var")
    if (is.character(tim) && length(tim) == 1 && tim %in% names(d) &&
        !is.numeric(d[[tim]])) {
      return(tags$div(style = paste("font-size: 0.75rem; margin: -6px 0 6px 0;", warn_style),
        sprintf("The time variable (%s) is not numeric, so the check cannot run.", tim)))
    }
    s <- insp_step()
    if (is.null(s)) return(NULL)
    num <- function(x) format(signif(x, 3), scientific = FALSE, trim = TRUE,
                              drop0trailing = TRUE)
    txt <- if (u == "s") sprintf("Frame step %s s (%s ms)", num(s), num(s * 1000))
           else sprintf("Frame step %s ms", num(s))
    fits <- identical(u, if (s < 1) "s" else "ms")
    if (!fits) txt <- sprintf("%s. The times look like %s.", txt,
                              if (s < 1) "seconds" else "milliseconds")
    tags$div(style = paste("font-size: 0.75rem; margin: -6px 0 6px 0;",
                           if (fits) "color: #777;" else warn_style),
      txt)
  })

  # Sidebar controls
  output$ui_inspect <- renderUI({
    cols <- insp_cols()
    vars <- if (!is.null(cols)) cols$vars else c("No dataset available")
    data_types <- if (!is.null(cols)) cols$types else rep("NA", length(vars))
    var_types <- paste0(vars, " {", data_types, "}")
    def <- isolate(insp_defaults(dataset()))
    sel <- function(id, choices = vars) pick_col(id, choices, def[[id]])
    # A setting the user changed stays when the sidebar is drawn again.
    keep <- function(id, value) {
      v <- isolate(input[[id]])
      if (is.null(v)) value else v
    }
    # The time unit: the user's pick, else the data's guess.
    unit <- unit_now()
    if (is.null(units$user)) units$offered <- unit

    tagList(
      wellPanel(
        h5("Dataset",
           tags$small(style = "color: #777; margin-left: 6px; font-weight: 400;",
                      input$dataset_name)),
        selectInput("inspect_f0_var", "Select f0 (Hz) variable:",
                    choices = setNames(vars, var_types),
                    selected = sel("inspect_f0_var")),
        uiOutput("inspect_f0_note"),
        selectInput("inspect_token_var", "Select Token ID variable:",
                    choices = setNames(vars, var_types),
                    selected = sel("inspect_token_var")),
        selectInput("inspect_time_var", "Select Time variable:",
                    choices = setNames(vars, var_types),
                    selected = sel("inspect_time_var")),
        selectInput("inspect_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = sel("inspect_speaker_var")),
        selectInput("inspect_tone_var", "Select Tone category variable (optional):",
                    choices = c("(none)" = "", setNames(vars, var_types)),
                    selected = sel("inspect_tone_var", c("", vars))),
        selectInput("inspect_intensity_var",
                    "Select Intensity (dB) variable (optional):",
                    choices  = c("(none)" = "",
                                 setNames(vars, var_types)),
                    selected = sel("inspect_intensity_var", c("", vars))),
        tags$hr(),
        radioButtons("inspect_time_unit", "Time unit:",
                     choices = list("Milliseconds" = "ms",
                                    "Seconds" = "s"),
                     selected = unit, inline = TRUE),
        uiOutput("inspect_time_step"),
        tags$hr(),
        h5("Thresholds"),
        numericInput("inspect_z_thresh", "Extreme-value threshold (z-score):",
                     value = keep("inspect_z_thresh", 3), min = 1, max = 6, step = 0.5),
        numericInput("inspect_rise_thresh", "Rise threshold (ST per 10ms):",
                     value = keep("inspect_rise_thresh", 1.263), min = 0.1, max = 5, step = 0.1),
        numericInput("inspect_fall_thresh", "Fall threshold (ST per 10ms):",
                     value = keep("inspect_fall_thresh", 1.714), min = 0.1, max = 5, step = 0.1),
        numericInput("inspect_carryover_mult",
                     "Carryover band (\u00d7 threshold):",
                     value = keep("inspect_carryover_mult", 1.5), min = 0, max = 3, step = 0.1),
        numericInput("inspect_level_thresh",
                     "Token-level threshold (modified z):",
                     value = keep("inspect_level_thresh", 3.5), min = 1, max = 8, step = 0.5),
        numericInput("inspect_intensity_drop",
                     "Low-intensity drop below peak (dB):",
                     value = keep("inspect_intensity_drop", 15), min = 1, max = 50, step = 1),
        tags$hr(),
        actionButton("inspect_button", "Run Inspection"),
        tags$hr(),
        h5("Download"),
        textInput("inspect_filename", "Enter filename (without extension):",
                  value = if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                            paste0(input$dataset_name, "_inspected")
                          else "inspected_data"),
        downloadButton("inspect_download", "Download Inspected Data"),
        div(style = "margin-top: 4px;",
          downloadButton("inspect_download_flagged", "Download Flagged Tokens")
        )
      )
    )
  })

  # Inspection result is stored in the shared `inspect_result` reactiveVal
  # (passed in), so the Curate tab can offer "use Inspect flags" as a way to
  # find re-label candidates. Defaults to a private reactiveVal if not shared.

  # The columns of the last run, so the summary reads the result with the
  # columns it was made with, even after the user changes a picker.
  insp_run <- reactiveVal(NULL)

  # Run inspection when button is clicked.
  # The detection logic lives in the package function inspect_f0()
  # (R/inspect.R), which is unit-tested in tests/testthat/test-inspect.R.
  observeEvent(input$inspect_button, {
    req(dataset())
    req(input$inspect_f0_var, input$inspect_token_var, input$inspect_time_var,
        input$inspect_speaker_var)   # tone is optional (coalesced below)

    carry_mult <- as.numeric(input$inspect_carryover_mult)
    if (is.na(carry_mult) || carry_mult < 0) carry_mult <- 1.5

    level_thresh <- as.numeric(input$inspect_level_thresh)
    if (is.na(level_thresh) || level_thresh <= 0) level_thresh <- 3.5

    intensity_drop <- as.numeric(input$inspect_intensity_drop)
    if (is.na(intensity_drop) || intensity_drop <= 0) intensity_drop <- 15

    # Optional intensity check. Only run it when the user has picked a column
    # that exists and is numeric; otherwise fall back to NULL (skip).
    intensity_var <- input$inspect_intensity_var
    if (is.null(intensity_var) || !nzchar(intensity_var) ||
        !(intensity_var %in% names(dataset()))) {
      intensity_var <- NULL
    } else if (!is.numeric(dataset()[[intensity_var]])) {
      showNotification(
        "Intensity variable is not numeric; skipping the low-intensity check.",
        type = "warning", duration = 5)
      intensity_var <- NULL
    }

    # Tone is optional: an empty selection (none) skips the token-level
    # (speaker x tone) check via inspect_f0(tone = NULL).
    tone_var <- input$inspect_tone_var
    if (is.null(tone_var) || !nzchar(tone_var)) tone_var <- NULL

    # An error in the check would close the session, so it becomes a
    # notification instead, and the old result is cleared: it was made with
    # other settings.
    run_cols <- list(f0 = input$inspect_f0_var, token = input$inspect_token_var,
                     speaker = input$inspect_speaker_var, tone = tone_var)
    result <- tryCatch(
      inspect_f0(
        dataset(),
        f0              = input$inspect_f0_var,
        token           = input$inspect_token_var,
        time            = input$inspect_time_var,
        speaker         = input$inspect_speaker_var,
        tone            = tone_var,
        z_threshold     = input$inspect_z_thresh,
        rise_threshold  = input$inspect_rise_thresh,
        fall_threshold  = input$inspect_fall_thresh,
        carryover_mult  = carry_mult,
        level_threshold = level_thresh,
        intensity       = intensity_var,
        intensity_drop  = intensity_drop,
        time_unit       = input$inspect_time_unit
      ),
      error = function(e) {
        showNotification(paste("Inspection failed:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      })

    insp_run(if (is.null(result)) NULL else run_cols)
    inspect_result(result)
  })

  # The results shown here (inspect_result) are built from dataset()'s rows,
  # so a new upload or a change of rows on the Start page (data_version)
  # clears them. The server clears the shared copy too, with a toast that
  # names it; this also covers the tab's own copy when it is not shared.
  observeEvent({ input$uploadfile; if (!is.null(data_version)) data_version() },
               inspect_result(NULL), ignoreInit = TRUE, ignoreNULL = FALSE)

  # Summary panel
  output$inspect_summary <- renderUI({
    req(inspect_result())
    result <- inspect_result()
    # The columns the result was made with (the pickers may have changed).
    rc <- insp_run()
    if (is.null(rc)) {
      tone_in <- input$inspect_tone_var
      rc <- list(f0 = input$inspect_f0_var, token = input$inspect_token_var,
                 speaker = input$inspect_speaker_var,
                 tone = if (is.null(tone_in) || !nzchar(tone_in)) NULL else tone_in)
    }
    req(rc$f0 %in% names(result), rc$token %in% names(result))
    token_var <- rc$token
    tone_var <- rc$tone
    if (!is.null(tone_var) && !tone_var %in% names(result)) tone_var <- NULL
    has_tone <- !is.null(tone_var)
    speaker_var <- rc$speaker

    # Counts over the samples with f0 (above 0 Hz) and the tokens with f0.
    # Rows without f0 are never flagged as samples (they carry only their
    # token's flag), so counting them would make the shares depend on
    # whether the Start page removed them. A token with no
    # f0 is not checked ("no f0" note) and is reported on its own line.
    f0_x    <- suppressWarnings(as.numeric(result[[rc$f0]]))
    with_f0 <- is.finite(f0_x) & f0_x > 0
    tok_all <- as.character(result[[token_var]])
    tok_f0  <- unique(tok_all[with_f0])
    n_no_f0_tokens <- length(setdiff(unique(tok_all), tok_f0))

    # Overall counts. Each token's row is its first row with f0 (the rows
    # are in time order).
    token_level <- result[with_f0, , drop = FALSE] %>%
      distinct(.data[[token_var]], .keep_all = TRUE)
    n_tokens <- nrow(token_level)
    n_flagged_tokens <- sum(token_level$flagged_token, na.rm = TRUE)
    n_samples <- sum(with_f0)
    n_flagged_samples <- sum(result$flagged_jump[with_f0], na.rm = TRUE)

    # Low-intensity samples (only present when an intensity column was used).
    has_intensity    <- "flag_low_intensity" %in% names(result)
    n_lowint_samples <- if (has_intensity)
                          sum(result$flag_low_intensity[with_f0], na.rm = TRUE) else 0

    # Which check(s) fired, per token (derived from flag_notes / flagged_jump;
    # the checks can overlap, so these need not sum to the flagged total).
    per_token_checks <- result %>%
      group_by(.data[[token_var]]) %>%
      summarise(
        extreme = any(grepl("max too high|min too low", flag_notes), na.rm = TRUE),
        level   = any(grepl("level too", flag_notes), na.rm = TRUE),
        jump    = any(flagged_jump, na.rm = TRUE),
        .groups = "drop"
      )
    n_extreme_tokens <- sum(per_token_checks$extreme)
    n_level_tokens   <- sum(per_token_checks$level)
    n_jump_tokens    <- sum(per_token_checks$jump)

    # Each token's speaker and tone as the level check read them: the most
    # frequent value on its rows with f0 (inspect_f0()'s "token_groups"
    # attribute). A tone column read from a TextGrid tier can change within
    # a token, so the token's first row is used only without the attribute.
    token_tone <- NULL
    token_spk  <- NULL
    if (has_tone) {
      tg  <- attr(result, "token_groups")
      key <- as.character(token_level[[token_var]])
      if (is.data.frame(tg) && all(c(token_var, tone_var) %in% names(tg))) {
        at <- match(key, as.character(tg[[token_var]]))
        token_tone <- tg[[tone_var]][at]
        if (!is.null(speaker_var) && speaker_var %in% names(tg))
          token_spk <- tg[[speaker_var]][at]
      } else {
        token_tone <- token_level[[tone_var]]
      }
      if (is.null(token_spk) && !is.null(speaker_var) && speaker_var %in% names(token_level))
        token_spk <- token_level[[speaker_var]]
    }

    # Speaker x tone groups too small to run the level check, counted on the
    # tokens with f0, as the level check counts them. inspect_f0() gives
    # them in its "level_groups" attribute; without it, count the tokens
    # with f0 here (fixed minimum of 5, inspect_f0()'s default min_tokens).
    # Only meaningful when a tone column was supplied.
    LEVEL_MIN_TOKENS <- 5
    n_level_skipped <- 0
    n_small_groups  <- 0
    if (has_tone) {
      grp <- attr(result, "level_groups")
      if (is.data.frame(grp) && all(c("n_tokens", "checked") %in% names(grp))) {
        small <- !grp$checked
        n_level_skipped <- sum(grp$n_tokens[small])
        n_small_groups  <- sum(small)
      } else if (!is.null(token_spk)) {
        small_grps <- data.frame(spk = token_spk, tone = token_tone) %>%
          group_by(spk, tone) %>%
          summarise(n = n(), .groups = "drop") %>%
          filter(n < LEVEL_MIN_TOKENS)
        n_level_skipped <- sum(small_grps$n)
        n_small_groups  <- nrow(small_grps)
      }
    }

    # Per-tone breakdown (only when a tone column was supplied)
    if (has_tone) {
      tone_summary <- data.frame(tone = token_tone,
                                 flagged_token = token_level$flagged_token) %>%
        group_by(tone) %>%
        summarise(
          total = n(),
          flagged = sum(flagged_token, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(flagged > 0)

      tone_items <- lapply(seq_len(nrow(tone_summary)), function(i) {
        row <- tone_summary[i, ]
        tags$li(paste0(row$tone, ": ", row$flagged, " / ", row$total,
                       " (", round(100 * row$flagged / max(row$total, 1), 1), "%)"))
      })
    } else {
      tone_summary <- data.frame()
      tone_items   <- list()
    }

    tagList(
      tags$div(style = "background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem;",
        tags$strong("Inspection summary:"),
        tags$ul(style = "margin-bottom: 4px; padding-left: 18px;",
          tags$li(paste0("Tokens flagged: ", fmt_count(n_flagged_tokens), " of ",
                         count_label(n_tokens, "token"), " with f0",
                         " (", round(100 * n_flagged_tokens / max(n_tokens, 1), 1), "%)"),
            tags$ul(style = "margin: 2px 0; padding-left: 18px;",
              tags$li(paste0("Extreme-value (max / min by speaker): ", n_extreme_tokens)),
              if (has_tone)
                tags$li(paste0("Token-level (median by speaker \u00d7 tone): ", n_level_tokens))
              else
                tags$li(tags$em("Token-level (by tone): not run (no tone selected)")),
              tags$li(paste0("Sample-level (jumps): ", n_jump_tokens)),
              tags$li(style = "list-style: none; margin-left: -18px; color: #8a6d00;",
                tags$em("A token can fire more than one check, so these may sum to more than the flagged total."))
            )
          ),
          tags$li(paste0("Samples with potential jumps: ", fmt_count(n_flagged_samples),
                         " of ", count_label(n_samples, "sample"), " with f0",
                         " (", round(100 * n_flagged_samples / max(n_samples, 1), 1), "%)")),
          if (has_intensity) tags$li(paste0(
            "Low-intensity samples (advisory): ", fmt_count(n_lowint_samples),
            " of ", count_label(n_samples, "sample"), " with f0",
            " (", round(100 * n_lowint_samples / max(n_samples, 1), 1), "%)")),
          if (n_no_f0_tokens > 0) tags$li(paste0(
            "Tokens with no f0: ", fmt_count(n_no_f0_tokens),
            ". They are not checked, and their rows have the note \"no f0\"."))
        ),
        if (n_level_skipped > 0) {
          tags$p(style = "margin: 4px 0 6px 0; font-size: 0.82rem; color: #8a6d00;",
            tags$em(paste0(
              "Level check skipped for ", n_level_skipped, " token",
              if (n_level_skipped == 1) "" else "s", " in ", n_small_groups,
              " speaker \u00d7 tone group", if (n_small_groups == 1) "" else "s",
              " with fewer than ", LEVEL_MIN_TOKENS,
              " tokens with f0 (not enough to estimate a spread)."))
          )
        },
        if (nrow(tone_summary) > 0) {
          tagList(
            tags$strong("Flagged tokens by tone:"),
            tags$ul(style = "margin-bottom: 0; padding-left: 18px;", tone_items)
          )
        }
      ),

      # Bulk-discard offer: for a large corpus with a small flagged share
      # (thresholds in global.R), repairing every flagged token one by one
      # is rarely worth the time, so point at the F0 Correction tab's
      # discard-the-set-then-review route instead.
      if (offer_bulk_triage(n_tokens, n_flagged_tokens)) {
        tags$div(style = paste(
            "background-color: #e8f5f0; border-left: 4px solid #78c2ad;",
            "padding: 10px 14px; margin-bottom: 12px; border-radius: 4px;",
            "font-size: 0.88rem; color: #2c5f4f;"),
          nows(icon("bolt"), " ",
            tags$strong("Fast path for this corpus: "),
            sprintf("only %.1f%% of %d tokens are flagged. ",
                    100 * n_flagged_tokens / max(n_tokens, 1), n_tokens),
            "Instead of repairing each one, consider a bulk discard: download ",
            "the flagged tokens below, then in ",
            tags$strong("F0 Processing \u2192 F0 Correction"),
            " upload that CSV and click ",
            tags$strong("Discard all flagged tokens"),
            ". Reviewing the discards afterwards (",
            tags$em("Only discarded"), ") is optional, and you can ",
            tags$strong("Restore"),
            " any worth repairing. The illustrated guide on that tab walks ",
            "through it.")
        )
      }
    )
  })

  # DT table output
  output$inspect_data <- DT::renderDataTable({
    req(inspect_result())
    result <- inspect_result()

    dt <- DT::datatable(
      result,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      )
    ) %>%
      DT::formatStyle("flagged_token",
        backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#ffe0e0", "white"))) %>%
      DT::formatStyle("flagged_jump",
        backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#fff3cd", "white"))) %>%
      DT::formatRound(c("f0_token_max", "f0_token_min", "f0_token_mean", "f0_token_sd"), 2)

    # Style the low-intensity flag and round the intensity column, both only
    # present when an intensity variable was selected.
    if ("flag_low_intensity" %in% names(result)) {
      dt <- dt %>%
        DT::formatStyle("flag_low_intensity",
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#e7f0ff", "white")))
    }
    intensity_var <- input$inspect_intensity_var
    if (!is.null(intensity_var) && nzchar(intensity_var) &&
        intensity_var %in% names(result) && is.numeric(result[[intensity_var]])) {
      dt <- dt %>% DT::formatRound(intensity_var, 1)
    }
    dt
  })

  # Download handler - all data
  output$inspect_download <- downloadHandler(
    filename = function() {
      paste0(input$inspect_filename, ".csv")
    },
    content = function(file) {
      req(inspect_result())
      fname <- paste0(input$inspect_filename, ".csv")
      write.csv(inspect_result(), file, row.names = FALSE)
      showNotification(paste("Data saved as", fname), type = "message", duration = 4)
    }
  )

  # Download handler - flagged tokens only
  output$inspect_download_flagged <- downloadHandler(
    filename = function() {
      paste0(input$inspect_filename, "_flagged.csv")
    },
    content = function(file) {
      req(inspect_result())
      fname <- paste0(input$inspect_filename, "_flagged.csv")
      flagged <- inspect_result() %>% filter(flagged_token == TRUE)
      write.csv(flagged, file, row.names = FALSE)
      showNotification(paste("Flagged tokens saved as", fname), type = "message", duration = 4)
    }
  )
}
