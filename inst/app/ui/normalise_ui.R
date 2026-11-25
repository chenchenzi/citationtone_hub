normalised_ui <- function(input, output, session, dataset, normalised_data,
                          curated_data = NULL, curated_norm_data = NULL,
                          start_cols = NULL, data_version = NULL, start_settings = NULL) {

  # Data source: the upload, or (after Curate) the curated data. Normalising
  # the curated data writes curated_norm_data and leaves normalised_data alone:
  # Curate reads normalised_data, so writing curated data there would make
  # Curate apply its relabels a second time.
  # has_curated is a reactiveVal, which only invalidates when its value
  # changes, so a curation click does not re-render this panel.
  has_curated <- reactiveVal(FALSE)
  observe(has_curated(!is.null(curated_data) && !is.null(curated_data())))
  # Once curated data exists, the source starts on it (About page, path 5:
  # Curate, then Normalise). The user's switch to Uploaded data is kept until
  # the curation is reset.
  src_choice <- reactiveVal(NULL)
  observeEvent(input$norm_source, src_choice(input$norm_source))
  observeEvent(has_curated(), if (!has_curated()) src_choice(NULL))
  norm_source <- reactive(
    if (!has_curated()) "uploaded" else if (is.null(src_choice())) "curated" else src_choice())
  norm_input  <- reactive(if (norm_source() == "curated") curated_data() else dataset())
  # Columns this tab writes: never offered as the f0 (Hz) default, and refused
  # as the f0 to normalise.
  norm_cols <- c("f0_st", "f0_zscore", "f0_lz", "speaker_mean")
  # Source and dataset the panel was last drawn for: while they are unchanged,
  # a re-render (e.g. after a curation change) keeps the user's choices.
  drawn_for <- NULL

  # The Start-page arguments are NULL when a caller leaves them out. A test may
  # bind this body to its own reactives without them, so look them up.
  start_cols   <- get0("start_cols", ifnotfound = NULL)
  data_version <- get0("data_version", ifnotfound = NULL)

  # --- Column defaults (Start page, D8) ---
  # f0 (Hz): the column the Start page screens (f0_corrected, say, not the raw
  # f0), then the usual Hz guess. Token (whole-token time): the Start page's
  # token column, then the usual guess. Speaker, tone and time: as before.
  norm_f0_default <- function(hz_vars) {
    scr <- start_col(start_cols, "f0")
    if (!is.null(scr) && scr %in% hz_vars) scr
    # Hz first: a re-uploaded normalised file also has f0_st / f0_lz.
    else guess_var(hz_vars, c("^f0_hz$", "^f0$", var_patterns$f0), 1)
  }
  norm_token_col <- function(vars) {
    tok <- start_col(start_cols, "token")
    if (!is.null(tok) && tok %in% vars) tok else vis_token_col(vars)
  }
  # The options of "Normalise time by" for these columns: "(none)", and,
  # with a token column, "Whole token (0-1)" and each landmark tier. The
  # observer checks the input against the same list, since a menu that is
  # no longer drawn (a new upload without a token column) keeps its value.
  norm_time_sets <- function(vars) {
    sets <- landmark_sets(vars)
    choices <- c("(none)" = "__none__")
    if (!is.null(norm_token_col(vars)))
      choices <- c(choices, "Whole token (0\u20131)" = "__token__",
                   stats::setNames(names(sets), names(sets)))
    choices
  }
  # Where to choose the token column: the Start page's token picker shows
  # when rows lack f0, and otherwise once its gap check is ticked.
  norm_no_token <- paste("Choose the token variable on the Start page, under Data screening (if",
                         "the picker is hidden, tick Check the time steps for gaps first). If the",
                         "file has none, add a column that names each token.")
  # Time columns are never built from a 0-1 axis (as on the Start page).
  norm_time_choices <- function(vars) vars[!grepl("_(t01|tseq|tbound)$", vars)]
  # The column that says which rows have f0 for the time axes: the f0 (Hz)
  # column picked above, else the Start page's f0, else the Hz guess.
  norm_span_f0 <- function(data) {
    v <- input$norm_f0_var
    if (is.character(v) && length(v) == 1 && v %in% names(data) && !v %in% norm_cols) return(v)
    scr <- start_col(start_cols, "f0")
    if (!is.null(scr) && scr %in% names(data)) return(scr)
    g <- norm_f0_default(setdiff(names(data), norm_cols))
    if (length(g) == 1 && !is.na(g) && g %in% names(data)) g else NULL
  }
  # The noun for one part of a landmark set in the texts: the set's own name
  # when it names a unit ("syllable"), else "<set> part" ("tier1 part").
  norm_unit_word <- function(set) {
    units <- c("syllable", "word", "rhyme", "rime", "vowel", "nucleus", "mora", "foot",
               "phrase", "segment", "phone", "onset", "coda", "tone", "interval")
    if (tolower(set) %in% units) set else paste(set, "part")
  }
  # The toast after the time axes are built: what 0 and 1 are, and how many
  # tokens (or parts of the tier) got no axis because they have f0 at fewer
  # than two points. Parts are counted as normalise_time_landmarks() takes
  # them (an internal rule the app cannot call): one per token x <set>_start
  # x <set>_end, for rows with a token, finite start and end, and a <set>_i
  # (or, without <set>_i, a label).
  norm_time_toast <- function(data, set, tok, f0) {
    na_txt <- "Rows before the first f0 or after the last are NA."
    na_in  <- ""
    if (identical(set, "__token__")) {
      head <- sprintf(paste("Added token_t01, with the tokens in column %s. Each token runs from",
                            "its first f0 (0) to its last f0 (1), read from column %s. %s"),
                      tok, f0, na_txt)
      unit <- ifelse(is.na(data[[tok]]), NA_character_, as.character(data[[tok]]))
      t01  <- data$token_t01
      what <- "token"
    } else {
      sc <- paste0(set, "_start"); ec <- paste0(set, "_end"); ic <- paste0(set, "_i")
      has_seq <- paste0(set, "_tseq") %in% names(data)
      has_bnd <- paste0(set, "_tbound") %in% names(data)
      # "Each syllable", but "Each tier1 part" for a set name that is not a unit
      what <- norm_unit_word(set)
      # "syllable_t01, syllable_tseq and syllable_tbound"; without <set>_i,
      # the package adds only <set>_t01
      made <- paste0(set, c("_t01", if (has_seq) "_tseq", if (has_bnd) "_tbound"))
      made <- if (length(made) > 1)
                paste(paste(made[-length(made)], collapse = ", "), "and", made[length(made)])
              else made
      head <- sprintf("Added %s, with the tokens in column %s. Each %s runs from its first f0 (0) to its last f0 (1), read from column %s.%s%s %s",
                      if (has_seq || has_bnd) made
                      else sprintf("%s (no %s column, so no %s_tseq or %s_tbound)", made, ic, set, set),
                      tok, what, f0,
                      if (has_seq) sprintf(" In %s_tseq, %s runs from 1 to 2, and so on.", set,
                                           if (grepl(" part$", what)) "part 2" else paste(what, "2"))
                      else "",
                      if (has_bnd) sprintf(paste(" In %s_tbound, the TextGrid boundaries sit at 1,",
                                                 "2, and so on, and the gaps are kept."), set)
                      else "",
                      na_txt)
      st <- suppressWarnings(as.numeric(as.character(data[[sc]])))
      en <- suppressWarnings(as.numeric(as.character(data[[ec]])))
      lab <- is.finite(st) & is.finite(en) & !is.na(data[[tok]])
      if (ic %in% names(data)) {
        lab <- lab & !is.na(data[[ic]])
      } else if (set %in% names(data)) {
        l <- as.character(data[[set]])
        lab <- lab & !is.na(l) & nzchar(trimws(l))
      }
      unit <- ifelse(lab, paste(as.character(data[[tok]]), st, en, sep = "\r"), NA_character_)
      t01  <- data[[paste0(set, "_t01")]]
      # A part with f0 at fewer than two points is NA on <set>_t01 and
      # <set>_tseq. On <set>_tbound it still has values when the token has
      # f0 in another part too (its TextGrid boundaries stand in for the
      # missing f0 span), so with that column the sentence names the two
      # columns it holds for.
      na_in <- if (has_bnd) sprintf(" in %s_t01 and %s_tseq", set, set) else ""
    }
    ok   <- !is.na(unit)
    n_na <- if (any(ok)) sum(!tapply(is.finite(t01[ok]), unit[ok], any), na.rm = TRUE) else 0L
    if (n_na > 0) head <- paste(head, sprintf("%s %s f0 at fewer than two points, so %s NA%s.",
                                              count_label(n_na, what),
                                              if (n_na == 1) "has" else "have",
                                              if (n_na == 1) "it is" else "they are",
                                              na_in))
    head
  }
  # The user's own picks in the variable menus: a value a menu sends that
  # differs from the default it was drawn with. A re-render keeps a pick while
  # the data still has the column (and, in the panel, while the source and
  # dataset are the same). A menu still on its default follows the new
  # default, so a change of column on the Start page reaches it.
  norm_picked <- list()
  norm_shown  <- list()
  for (norm_id in c("norm_f0_var", "norm_speaker_var", "norm_tone_var",
                    "norm_time_token_var")) local({
    i <- norm_id
    observeEvent(input[[i]], {
      v <- input[[i]]
      norm_picked[[i]] <<- if (!identical(v, norm_shown[[i]])) v
    })
  })
  norm_pick <- function(id, vars, default) {
    norm_shown[[id]] <<- default
    v <- norm_picked[[id]]
    if (is.character(v) && length(v) == 1 && v %in% vars) v else default
  }

  # Guide text for the Normalise tab
  output$normalise_guide <- renderUI({
    tagList(
      guide_box("Normalisation guide",
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(nows(tags$strong("Data source:"), " The uploaded data, or, after Curate, the ",
                  tags$strong("Curated data"), ". Normalising that again recomputes each speaker\u2019s mean (and SD) on the kept tokens and your new tone labels, and creates ",
                  tags$strong("Curated + normalised data"), " for Visualise, the modelling tabs and Summarise. The curated data itself is not changed.")),
          tags$li(tags$strong("f0 (Hz):"), " The raw fundamental frequency variable in Hertz. It starts on the f0 variable the Start page screens."),
          tags$li(tags$strong("Speaker:"), " A speaker ID variable for by-speaker normalisation."),
          tags$li(tags$strong("Tone category:"), " The variable labelling tone types.")
        ),
        tags$strong("Speaker mean methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Simple average:"), " Mean (and, for z-scores, SD) over all data points for each speaker, as R\u2019s mean() and sd(). The classic z-score and LZ formulas."),
          tags$li(tags$strong(HTML("Equally weighted by tone &#128077;:")), " Each tone counts once, however many tokens it has: the mean of the per-tone means, and for z-scores an SD weighted the same way, as R\u2019s cov.wt(). Same as simple when every tone has the same number of data points.")
        ),
        tags$strong("F0 normalisation methods:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Semitone:"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "ST = 12 \u00d7 log\u2082(f0 / \u03bc)"), " Converts Hz to a perceptually uniform scale referenced on each speaker's mean. Keeps each speaker's own pitch range."),
          tags$li(tags$strong("Z-score:"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "z = (f0 \u2212 \u03bc) / \u03c3"), " Centres f0 on 0 and scales by speaker variability, in Hz."),
          tags$li(tags$strong("Log z-score (LZ):"), " ", tags$code(style = "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;", "LZ = (log\u2081\u2080 f0 \u2212 \u03bc) / \u03c3"), " A z-score of log\u2081\u2080 f0 (\u03bc, \u03c3 taken on log\u2081\u2080 f0): like semitones, but also evens out speakers' pitch ranges. Common in Chinese tone studies (Zhu 1999).")
        ),
        tags$strong("Bessel\u2019s correction (z-score and LZ only):"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Corrected (default):"), " The sample SD, dividing by n \u2212 1, as R\u2019s sd() and the published formulas do (cov.wt(method = \"unbiased\") when equally weighted)."),
          tags$li(tags$strong("Uncorrected:"), " The population SD, dividing by n (cov.wt(method = \"ML\") when equally weighted). The difference is negligible unless a tone has only a few data points.")
        )
      ),
        # --- Collapsible illustrated guide, shown below the green guide ---
        tags$details(class = "msg-route",
          tags$style(HTML("
            details.msg-route{background:#f3f8fc;border:1px solid #cfe2f1;border-radius:8px;padding:7px 14px 11px;margin:6px 0 0;}
            .msg-route>summary{cursor:pointer;font-weight:700;color:#2c5d80;font-size:0.9rem;list-style:none;padding:1px 0;}
            .msg-route>summary::-webkit-details-marker{display:none;}
            .msg-route>summary::before{content:'\\25B8';color:#5b9bd5;display:inline-block;margin-right:8px;transition:transform .15s ease;}
            .msg-route[open]>summary::before{transform:rotate(90deg);}
            .msg-hint{color:#7aa6cc;font-weight:400;font-size:0.78rem;margin-left:6px;}
            .msg-route[open] .msg-hint{display:none;}
            .msg-intro{color:#3f5a72;font-size:0.83rem;line-height:1.5;margin:9px 0 0;}
            .msg-tier{color:#8d9aa3;font-style:italic;}
            .msg-suf{color:#2c5d80;font-weight:700;}
            .msg-illus{margin:11px 0 3px;}
            .msg-illus svg,.msg-trio svg{width:100%;height:auto;display:block;margin:0 auto;}
            .msg-a-wide{display:none;}
            .msg-a-wide svg{max-width:760px;}
            .msg-a-narrow svg{max-width:360px;}
            .msg-group{text-align:center;font-size:0.84rem;font-weight:700;color:#2c5f4f;border-top:1px solid #d9e6f1;padding-top:10px;margin:10px 0 8px;}
            .msg-trio{display:grid;grid-template-columns:minmax(0,1fr);gap:16px;}
            .msg-trio>div{display:flex;flex-direction:column;min-width:0;}
            .msg-trio svg{max-width:300px;}
            .msg-trio .msg-step{flex:1 0 auto;margin-top:6px;}
            .msg-fit{container-type:inline-size;}
            @container (min-width:521px){.msg-trio>div{flex-direction:row;align-items:center;gap:14px;}.msg-trio>div>svg{flex:0 0 46%;}.msg-trio .msg-step{flex:1 1 0;margin-top:0;}}
            @container (min-width:651px){.msg-a-wide{display:block;}.msg-a-narrow{display:none;}}
            @container (min-width:781px){.msg-trio{grid-template-columns:repeat(3,minmax(0,1fr));gap:14px;}.msg-trio>div{flex-direction:column;align-items:stretch;gap:0;}.msg-trio>div>svg{flex:none;}.msg-trio .msg-step{flex:1 0 auto;margin-top:6px;}}
            .msg-step{flex:1 1 300px;background:#fff;border:1px solid #e1e9f2;border-radius:7px;padding:8px 12px;}
            .msg-swhy{font-size:0.76rem;color:#5f6b66;line-height:1.4;margin-top:2px;}
            .msg-tab{display:inline-block;background:#e8f5f0;color:#2c5f4f;padding:1px 7px;border-radius:10px;font-size:0.72rem;font-weight:600;white-space:nowrap;}
            .msg-tip{font-size:0.78rem;color:#33536f;background:#eaf3fb;border:1px solid #d3e6f5;border-radius:6px;padding:6px 11px;margin-top:11px;line-height:1.55;}
            .msg-tip .fa,.msg-tip svg{color:#5b9bd5;margin-right:4px;}
          ")),
          tags$summary(icon("ruler-horizontal"), " Multisyllabic or sub-syllabic data?",
                       tags$span(class = "msg-hint", "(click to expand)")),
          tags$p(class = "msg-intro",
            HTML(paste('Time normalisation adds a time column that runs from 0 to 1 within each token',
                       '(or each syllable). Tokens differ in length, so on raw time their contours start',
                       'and end at different points. On the new column their shapes share one axis, so',
                       'they can be compared, averaged and modelled together (see tokens',
                       '<span style="color:#3a7ca5;font-weight:700;">A</span> and',
                       '<span style="color:#2f9e79;font-weight:700;">B</span> below).'))),
          # The syllable tier's columns: the tier's name (light) and the ending
          # (dark), as in the pictures.
          tags$p(class = "msg-intro",
            HTML(paste('<em>Whole token</em> adds <code>token_t01</code>, whatever your token variable is',
                       'called. That is enough when each token is one syllable; for part of a syllable,',
                       'pick a <code>rhyme</code> or <code>vowel</code> tier instead (<code>vowel_t01</code>).',
                       'For multisyllabic data, your syllable tier adds three columns, named after it:',
                       '<code><span class="msg-tier">&lt;syllable_tier&gt;</span><span class="msg-suf">_t01</span></code> puts each syllable on its own 0&ndash;1, and',
                       '<code><span class="msg-tier">&lt;syllable_tier&gt;</span><span class="msg-suf">_tseq</span></code> and <code><span class="msg-tier">&lt;syllable_tier&gt;</span><span class="msg-suf">_tbound</span></code> put the',
                       'syllables in sequence, as below. In the pictures the syllable tier is called',
                       '<code>syllable</code>.'))),
          # Token A in time first, wide, with a ruler for each axis: where 0 and
          # the end fall. Then the three axes for several syllables, two tokens
          # on each, in a row, each with its card under it. The layout follows
          # the guide's own width (container queries on .msg-fit): three in a
          # row (781 px and up), then each picture beside its card, then (below
          # 521 px) each picture above it. Below 651 px Token A swaps to a narrow
          # drawing that puts each label above its ruler. The narrow layout is
          # the default, so a browser without container queries shows it. Text
          # shows at about 10-13 px throughout.
          tags$div(class = "msg-fit",
            tags$div(class = "msg-illus",
              tags$div(class = "msg-a-wide", HTML('<svg width="100%" viewBox="0 0 680 272" role="img" xmlns="http://www.w3.org/2000/svg"><title>Token A in time: where 0 and 1 fall</title><desc>One two-syllable token in time, with no f0 in a voiceless onset, in a gap at the syllable boundary and in the silence at the end. On syllable_t01 each syllable runs from its first f0 (0) to its last f0 (1). On syllable_tseq syllable 1 runs from 0 to 1 and syllable 2 from 1 to 2, so 1 is both syllable 1&#8217;s last f0 and syllable 2&#8217;s first f0. On syllable_tbound 0 is the first f0, 1 the TextGrid boundary and 2 the last f0. On token_t01 the word runs from its first f0 (0) to its last f0 (1). The stretches without f0 at the edges take no room on any axis. The gap at the syllable boundary takes no room on syllable_t01 and syllable_tseq, and keeps its length on syllable_tbound and token_t01.</desc><text x="340" y="17" font-size="13" font-weight="700" fill="#2c5d80" text-anchor="middle">Token A in time: where 0 and 1 fall</text><text x="128" y="41.5" font-size="11.5" fill="#97a4ac" text-anchor="end">TextGrid</text><text x="128" y="94" font-size="11.5" fill="#97a4ac" text-anchor="end">f0</text><text x="128" y="150" font-size="11.5" text-anchor="end"><tspan fill="#a9b4bb">syllable</tspan><tspan fill="#2c5d80" font-weight="700">_t01</tspan></text><text x="128" y="178" font-size="11.5" text-anchor="end"><tspan fill="#a9b4bb">syllable</tspan><tspan fill="#2c5d80" font-weight="700">_tseq</tspan></text><text x="128" y="206" font-size="11.5" text-anchor="end"><tspan fill="#a9b4bb">syllable</tspan><tspan fill="#2c5d80" font-weight="700">_tbound</tspan></text><text x="128" y="234" font-size="11.5" text-anchor="end"><tspan fill="#2c5d80" font-weight="700">token_t01</tspan></text><rect x="140" y="28" width="246.2" height="18" fill="#f3f6f8" stroke="#cfdbe5" stroke-width="1"/><rect x="386.2" y="28" width="277.8" height="18" fill="#f3f6f8" stroke="#cfdbe5" stroke-width="1"/><text x="263.1" y="41.5" font-size="12" fill="#5f6b66" text-anchor="middle">&#963;1</text><text x="525.1" y="41.5" font-size="12" fill="#5f6b66" text-anchor="middle">&#963;2</text><rect x="140" y="52" width="56.3" height="76" fill="#eef1f4"/><text x="168.1" y="123" font-size="10.5" fill="#a8b3bb" text-anchor="middle">no f0</text><rect x="351" y="52" width="70.3" height="76" fill="#eef1f4"/><text x="386.2" y="123" font-size="10.5" fill="#a8b3bb" text-anchor="middle">no f0</text><rect x="607.7" y="52" width="56.3" height="76" fill="#eef1f4"/><text x="635.9" y="123" font-size="10.5" fill="#a8b3bb" text-anchor="middle">no f0</text><line x1="386.2" y1="28" x2="386.2" y2="111" stroke="#9fbbd6" stroke-width="1.2" stroke-dasharray="3,3"/><line x1="386.2" y1="128" x2="386.2" y2="196" stroke="#9fbbd6" stroke-width="1.2" stroke-dasharray="3,3"/><polyline points="196.3,103.6 226.9,91.4 257.3,81.9 287.9,73.7 318.5,68.3 351,65.6" fill="none" stroke="#3a7ca5" stroke-width="2.8"/><polyline points="421.3,73.7 457.7,79.1 494.3,87.3 530.7,95.4 571.3,103.6 607.7,109" fill="none" stroke="#3a7ca5" stroke-width="2.8"/><line x1="196.3" y1="128" x2="196.3" y2="140" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="196.3" y1="159" x2="196.3" y2="168" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="196.3" y1="187" x2="196.3" y2="196" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="196.3" y1="215" x2="196.3" y2="224" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="607.7" y1="128" x2="607.7" y2="140" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="607.7" y1="159" x2="607.7" y2="168" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="607.7" y1="187" x2="607.7" y2="196" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="607.7" y1="215" x2="607.7" y2="224" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="351" y1="128" x2="351" y2="140" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="351" y1="159" x2="351" y2="168" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="421.3" y1="128" x2="421.3" y2="140" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="421.3" y1="159" x2="421.3" y2="168" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><path d="M196.3,140 v5 h154.7 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><text x="196.3" y="157" font-size="12" fill="#2c5d80" text-anchor="middle">0</text><text x="351" y="157" font-size="12" fill="#2c5d80" text-anchor="middle">1</text><path d="M421.3,140 v5 h186.4 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><text x="421.3" y="157" font-size="12" fill="#2c5d80" text-anchor="middle">0</text><text x="607.7" y="157" font-size="12" fill="#2c5d80" text-anchor="middle">1</text><path d="M196.3,168 v5 h154.7 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><text x="196.3" y="185" font-size="12" fill="#2c5d80" text-anchor="middle">0</text><text x="351" y="185" font-size="12" fill="#2c5d80" text-anchor="middle">1</text><path d="M421.3,168 v5 h186.4 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><text x="421.3" y="185" font-size="12" fill="#2c5d80" text-anchor="middle">1</text><text x="607.7" y="185" font-size="12" fill="#2c5d80" text-anchor="middle">2</text><path d="M196.3,196 v5 h411.5 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><line x1="386.2" y1="196" x2="386.2" y2="201" stroke="#2c5d80" stroke-width="1.4"/><text x="196.3" y="213" font-size="12" fill="#2c5d80" text-anchor="middle">0</text><text x="386.2" y="213" font-size="12" fill="#2c5d80" text-anchor="middle">1</text><text x="607.7" y="213" font-size="12" fill="#2c5d80" text-anchor="middle">2</text><path d="M196.3,224 v5 h411.5 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><text x="196.3" y="241" font-size="12" fill="#2c5d80" text-anchor="middle">0</text><text x="607.7" y="241" font-size="12" fill="#2c5d80" text-anchor="middle">1</text><text x="340" y="264" font-size="12" fill="#5f6b66" text-anchor="middle">0 is the first f0, the end is the last f0; edges without f0 take no room</text></svg>')),
              tags$div(class = "msg-a-narrow", HTML('<svg width="100%" viewBox="0 0 320 364" role="img" xmlns="http://www.w3.org/2000/svg"><title>Token A in time: where 0 and 1 fall</title><desc>One two-syllable token in time, with no f0 in a voiceless onset, in a gap at the syllable boundary and in the silence at the end. On syllable_t01 each syllable runs from its first f0 (0) to its last f0 (1). On syllable_tseq syllable 1 runs from 0 to 1 and syllable 2 from 1 to 2, so 1 is both syllable 1&#8217;s last f0 and syllable 2&#8217;s first f0. On syllable_tbound 0 is the first f0, 1 the TextGrid boundary and 2 the last f0. On token_t01 the word runs from its first f0 (0) to its last f0 (1). The stretches without f0 at the edges take no room on any axis. The gap at the syllable boundary takes no room on syllable_t01 and syllable_tseq, and keeps its length on syllable_tbound and token_t01.</desc><text x="160" y="16" font-size="12.5" font-weight="700" fill="#2c5d80" text-anchor="middle">Token A in time: where 0 and 1 fall</text><text x="12" y="33" font-size="10.5" fill="#97a4ac">TextGrid</text><rect x="12" y="38" width="139.1" height="17" fill="#f3f6f8" stroke="#cfdbe5" stroke-width="1"/><rect x="151.1" y="38" width="156.9" height="17" fill="#f3f6f8" stroke="#cfdbe5" stroke-width="1"/><text x="81.5" y="50.5" font-size="11" fill="#5f6b66" text-anchor="middle">&#963;1</text><text x="229.5" y="50.5" font-size="11" fill="#5f6b66" text-anchor="middle">&#963;2</text><text x="12" y="71" font-size="10.5" fill="#97a4ac">f0</text><rect x="12" y="76" width="31.8" height="66" fill="#eef1f4"/><text x="27.9" y="137" font-size="10" fill="#a8b3bb" text-anchor="middle">no f0</text><rect x="131.2" y="76" width="39.7" height="66" fill="#eef1f4"/><text x="151.1" y="137" font-size="10" fill="#a8b3bb" text-anchor="middle">no f0</text><rect x="276.2" y="76" width="31.8" height="66" fill="#eef1f4"/><text x="292.1" y="137" font-size="10" fill="#a8b3bb" text-anchor="middle">no f0</text><line x1="151.1" y1="38" x2="151.1" y2="127" stroke="#9fbbd6" stroke-width="1.2" stroke-dasharray="3,3"/><line x1="151.1" y1="142" x2="151.1" y2="260" stroke="#9fbbd6" stroke-width="1.2" stroke-dasharray="3,3"/><polyline points="43.8,120.8 61.1,110.2 78.3,101.9 95.5,94.9 112.8,90.1 131.2,87.8" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="170.9,94.9 191.5,99.6 212.1,106.6 232.7,113.7 255.7,120.8 276.2,125.5" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><line x1="43.8" y1="142" x2="43.8" y2="172" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="43.8" y1="191" x2="43.8" y2="216" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="43.8" y1="235" x2="43.8" y2="260" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="43.8" y1="279" x2="43.8" y2="304" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="276.2" y1="142" x2="276.2" y2="172" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="276.2" y1="191" x2="276.2" y2="216" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="276.2" y1="235" x2="276.2" y2="260" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="276.2" y1="279" x2="276.2" y2="304" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="131.2" y1="142" x2="131.2" y2="172" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="131.2" y1="191" x2="131.2" y2="216" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="170.9" y1="142" x2="170.9" y2="172" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><line x1="170.9" y1="191" x2="170.9" y2="216" stroke="#c9d9e6" stroke-width="1" stroke-dasharray="2,2"/><text x="12" y="164" font-size="10.5" stroke="#f3f8fc" stroke-width="4" paint-order="stroke"><tspan fill="#a9b4bb">syllable</tspan><tspan fill="#2c5d80" font-weight="700">_t01</tspan></text><text x="12" y="208" font-size="10.5" stroke="#f3f8fc" stroke-width="4" paint-order="stroke"><tspan fill="#a9b4bb">syllable</tspan><tspan fill="#2c5d80" font-weight="700">_tseq</tspan></text><text x="12" y="252" font-size="10.5" stroke="#f3f8fc" stroke-width="4" paint-order="stroke"><tspan fill="#a9b4bb">syllable</tspan><tspan fill="#2c5d80" font-weight="700">_tbound</tspan></text><text x="12" y="296" font-size="10.5" stroke="#f3f8fc" stroke-width="4" paint-order="stroke"><tspan fill="#2c5d80" font-weight="700">token_t01</tspan></text><path d="M43.8,172 v5 h87.4 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><text x="43.8" y="188" font-size="11.5" fill="#2c5d80" text-anchor="middle">0</text><text x="131.2" y="188" font-size="11.5" fill="#2c5d80" text-anchor="middle">1</text><path d="M170.9,172 v5 h105.3 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><text x="170.9" y="188" font-size="11.5" fill="#2c5d80" text-anchor="middle">0</text><text x="276.2" y="188" font-size="11.5" fill="#2c5d80" text-anchor="middle">1</text><path d="M43.8,216 v5 h87.4 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><text x="43.8" y="232" font-size="11.5" fill="#2c5d80" text-anchor="middle">0</text><text x="131.2" y="232" font-size="11.5" fill="#2c5d80" text-anchor="middle">1</text><path d="M170.9,216 v5 h105.3 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><text x="170.9" y="232" font-size="11.5" fill="#2c5d80" text-anchor="middle">1</text><text x="276.2" y="232" font-size="11.5" fill="#2c5d80" text-anchor="middle">2</text><path d="M43.8,260 v5 h232.4 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><line x1="151.1" y1="260" x2="151.1" y2="265" stroke="#2c5d80" stroke-width="1.4"/><text x="43.8" y="276" font-size="11.5" fill="#2c5d80" text-anchor="middle">0</text><text x="151.1" y="276" font-size="11.5" fill="#2c5d80" text-anchor="middle">1</text><text x="276.2" y="276" font-size="11.5" fill="#2c5d80" text-anchor="middle">2</text><path d="M43.8,304 v5 h232.4 v-5" fill="none" stroke="#2c5d80" stroke-width="1.4"/><text x="43.8" y="320" font-size="11.5" fill="#2c5d80" text-anchor="middle">0</text><text x="276.2" y="320" font-size="11.5" fill="#2c5d80" text-anchor="middle">1</text><text x="160" y="343" font-size="11" fill="#5f6b66" text-anchor="middle">0 is the first f0, the end is the last f0;</text><text x="160" y="357" font-size="11" fill="#5f6b66" text-anchor="middle">edges without f0 take no room</text></svg>'))),
            tags$div(class = "msg-group",
              HTML(paste('Several syllables: two tokens of one word, <span style="color:#3a7ca5;">A</span>',
                         'and <span style="color:#2f9e79;">B</span>, on each axis'))),
            tags$div(class = "msg-trio",
              tags$div(
                HTML('<svg width="100%" viewBox="0 0 270 236" role="img" xmlns="http://www.w3.org/2000/svg"><title>In sequence: syllable_tseq</title><desc>Two tokens, A and B, on syllable_tseq. Syllable 1 runs from 0 to 1 and syllable 2 from 1 to 2, so the boundaries of all tokens line up at 1, and the gap at the boundary takes no room.</desc><text x="135" y="15" font-size="12" font-weight="700" fill="#2c5d80" text-anchor="middle">In sequence</text><text x="135" y="30" font-size="11.5" text-anchor="middle"><tspan fill="#a9b4bb">syllable</tspan><tspan fill="#2c5d80" font-weight="700">_tseq</tspan></text><rect x="14" y="44" width="242" height="131.2" fill="none" stroke="#b8d2e8" stroke-width="1"/><line x1="135" y1="44" x2="135" y2="175.2" stroke="#9fbbd6" stroke-width="1.5" stroke-dasharray="4,3"/><text x="74.5" y="60" font-size="11" fill="#97a4ac" text-anchor="middle">&#963;1</text><text x="195.5" y="60" font-size="11" fill="#97a4ac" text-anchor="middle">&#963;2</text><polyline points="14,147.1 47.9,126 81.8,109.6 110.8,100.2 135,95.5" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="135,107.3 164,111.9 193.1,123.7 227,137.7 256,144.7" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="14,161.1 47.9,142.4 81.8,128.3 110.8,119 135,116.6" fill="none" stroke="#2f9e79" stroke-width="2.5"/><polyline points="135,128.3 164,135.4 193.1,147.1 227,158.8 256,165.8" fill="none" stroke="#2f9e79" stroke-width="2.5"/><line x1="14" y1="175.2" x2="14" y2="179.2" stroke="#b8d2e8" stroke-width="1"/><text x="14" y="192" font-size="11" fill="#97a4ac" text-anchor="middle">0</text><line x1="135" y1="175.2" x2="135" y2="179.2" stroke="#b8d2e8" stroke-width="1"/><text x="135" y="192" font-size="11" fill="#97a4ac" text-anchor="middle">1</text><line x1="256" y1="175.2" x2="256" y2="179.2" stroke="#b8d2e8" stroke-width="1"/><text x="256" y="192" font-size="11" fill="#97a4ac" text-anchor="middle">2</text><text x="135" y="215" font-size="11" fill="#5f6b66" text-anchor="middle">the gap takes no room;</text><text x="135" y="229" font-size="11" fill="#5f6b66" text-anchor="middle">boundaries line up at 1</text></svg>'),
                tags$div(class = "msg-step",
                  tags$div(class = "msg-swhy",
                    HTML(paste("Pick the <code>syllable</code> tier and use <code>syllable_tseq</code>.",
                               "Syllable 1 fills 0 to 1 and syllable 2 fills 1 to 2, so the boundaries line",
                               "up across tokens. <strong>Use it</strong> to compare each syllable&rsquo;s",
                               "shape across tokens."))))),
              tags$div(
                HTML('<svg width="100%" viewBox="0 0 270 236" role="img" xmlns="http://www.w3.org/2000/svg"><title>In sequence, gap kept: syllable_tbound</title><desc>The same two tokens on syllable_tbound. Each word runs from its first f0 (0) to its last f0 (2), with the TextGrid boundary at 1, so the boundaries of all tokens line up at 1 and each token keeps its own gap around it: A&#8217;s gap is wider than B&#8217;s.</desc><text x="135" y="15" font-size="12" font-weight="700" fill="#2c5d80" text-anchor="middle">In sequence, gap kept</text><text x="135" y="30" font-size="11.5" text-anchor="middle"><tspan fill="#a9b4bb">syllable</tspan><tspan fill="#2c5d80" font-weight="700">_tbound</tspan></text><rect x="14" y="44" width="242" height="131.2" fill="none" stroke="#b8d2e8" stroke-width="1"/><line x1="135" y1="44" x2="135" y2="175.2" stroke="#9fbbd6" stroke-width="1.5" stroke-dasharray="4,3"/><text x="74.5" y="60" font-size="11" fill="#97a4ac" text-anchor="middle">&#963;1</text><text x="195.5" y="60" font-size="11" fill="#97a4ac" text-anchor="middle">&#963;2</text><polyline points="14,147.1 41.6,126 69.2,109.6 92.9,100.2 112.6,95.5" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="154.2,107.3 178.7,111.9 203.1,123.7 231.6,137.7 256,144.7" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="14,161.1 45.8,142.4 77.7,128.3 105,119 127.8,116.6" fill="none" stroke="#2f9e79" stroke-width="2.5"/><polyline points="146,128.3 172.4,135.4 198.8,147.1 229.6,158.8 256,165.8" fill="none" stroke="#2f9e79" stroke-width="2.5"/><line x1="14" y1="175.2" x2="14" y2="179.2" stroke="#b8d2e8" stroke-width="1"/><text x="14" y="192" font-size="11" fill="#97a4ac" text-anchor="middle">0</text><line x1="135" y1="175.2" x2="135" y2="179.2" stroke="#b8d2e8" stroke-width="1"/><text x="135" y="192" font-size="11" fill="#97a4ac" text-anchor="middle">1</text><line x1="256" y1="175.2" x2="256" y2="179.2" stroke="#b8d2e8" stroke-width="1"/><text x="256" y="192" font-size="11" fill="#97a4ac" text-anchor="middle">2</text><text x="135" y="215" font-size="11" fill="#5f6b66" text-anchor="middle">the gap is kept;</text><text x="135" y="229" font-size="11" fill="#5f6b66" text-anchor="middle">boundaries line up at 1</text></svg>'),
                tags$div(class = "msg-step",
                  tags$div(class = "msg-swhy",
                    HTML(paste("Pick the <code>syllable</code> tier and use <code>syllable_tbound</code>.",
                               "The TextGrid boundaries between syllables sit at 1, 2, ..., so they line up across tokens,",
                               "and the gap at each is kept. <strong>Use it</strong> to model the whole",
                               "word with the syllables lined up."))))),
              tags$div(
                HTML('<svg width="100%" viewBox="0 0 270 236" role="img" xmlns="http://www.w3.org/2000/svg"><title>Whole word: token_t01</title><desc>The same two tokens on token_t01. The word runs from its first f0 (0) to its last f0 (1), with the gap and the syllable duration ratio kept, so the boundary falls at a different point in each token.</desc><text x="135" y="15" font-size="12" font-weight="700" fill="#2c5d80" text-anchor="middle">Whole word</text><text x="135" y="30" font-size="11.5" text-anchor="middle"><tspan fill="#2c5d80" font-weight="700">token_t01</tspan></text><rect x="14" y="44" width="242" height="131.2" fill="none" stroke="#b8d2e8" stroke-width="1"/><line x1="125.7" y1="44" x2="125.7" y2="175.2" stroke="#3a7ca5" stroke-width="1" stroke-dasharray="2,3" opacity="0.7"/><line x1="160.2" y1="44" x2="160.2" y2="175.2" stroke="#2f9e79" stroke-width="1" stroke-dasharray="2,3" opacity="0.7"/><polyline points="14,147.1 39.5,126 64.9,109.6 86.8,100.2 105,95.5" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="146.3,107.3 172.7,111.9 199,123.7 229.7,137.7 256,144.7" fill="none" stroke="#3a7ca5" stroke-width="2.5"/><polyline points="14,161.1 52.5,142.4 91,128.3 123.9,119 151.4,116.6" fill="none" stroke="#2f9e79" stroke-width="2.5"/><polyline points="168.9,128.3 189.8,135.4 210.7,147.1 235.1,158.8 256,165.8" fill="none" stroke="#2f9e79" stroke-width="2.5"/><line x1="14" y1="175.2" x2="14" y2="179.2" stroke="#b8d2e8" stroke-width="1"/><text x="14" y="192" font-size="11" fill="#97a4ac" text-anchor="middle">0</text><line x1="256" y1="175.2" x2="256" y2="179.2" stroke="#b8d2e8" stroke-width="1"/><text x="256" y="192" font-size="11" fill="#97a4ac" text-anchor="middle">1</text><text x="135" y="215" font-size="11" fill="#5f6b66" text-anchor="middle">gap and syllable ratio kept;</text><text x="135" y="229" font-size="11" fill="#5f6b66" text-anchor="middle">the boundary varies</text></svg>'),
                tags$div(class = "msg-step",
                  tags$div(class = "msg-swhy",
                    HTML(paste("Pick <em>Whole token (0&ndash;1)</em>, which adds <code>token_t01</code>.",
                               "The gap and the syllables&rsquo; relative durations are kept, so the boundary",
                               "moves from token to token. <strong>Use it</strong> to keep the word&rsquo;s own",
                               "timing. If each token holds more than the word, such as a carrier phrase,",
                               "pick a <code>word</code> tier with only the target word labelled",
                               "(<code>word_t01</code>)."))))))),
          tags$div(class = "msg-tip",
            icon("lightbulb"),
            HTML(paste(' Use the new column as the X axis in <span class="msg-tab">Visualise</span>, or as',
                       'the Time variable in <span class="msg-tab">Model: Polynomials</span>,',
                       '<span class="msg-tab">Model: GCA</span>, <span class="msg-tab">Model: GAMM</span>,',
                       '<span class="msg-tab">Cluster</span> and <span class="msg-tab">Summarise</span>.'))),
          tags$div(class = "msg-tip",
            icon("lightbulb"),
            HTML(paste(' On <code>syllable_tseq</code>, syllable 1&rsquo;s last f0 and syllable 2&rsquo;s',
                       'first f0 both sit at 1, so one GAMM smooth cannot follow an f0 jump there. To',
                       'model the whole word, use <code>syllable_tbound</code> (in sequence, gap kept) or',
                       '<code>token_t01</code>, or model each syllable as its own token on',
                       '<code>syllable_t01</code>.')))
        )
    )
  })

  # Render UI for selecting f0, speaker, and tone variables
  output$ui_normalise <- renderUI({
    ds   <- norm_input()
    src  <- norm_source()
    vars <- if (!is.null(ds)) names(ds) else c("No dataset available")
    data_types <- if (!is.null(ds)) sapply(ds, class) else rep("NA", length(vars))
    var_types  <- paste0(vars, " {", data_types, "}")
    key <- paste(src, input$dataset_name)
    same <- identical(drawn_for, key); drawn_for <<- key
    # Re-renders keep the method choices, and the variables the user picked
    # and the filename while the source and dataset are unchanged.
    keep <- function(id, default) { v <- isolate(input[[id]]); if (is.null(v)) default else v }
    if (!same) norm_picked[c("norm_f0_var", "norm_speaker_var", "norm_tone_var")] <<- NULL
    keep_var <- function(id, default) norm_pick(id, vars, default)
    # On curated data, default to the speaker and tone variables used in Curate,
    # so the tone weighting follows the curated labels (e.g. `cluster`).
    cv <- if (src == "curated") attr(ds, "curate_vars") else NULL
    pick <- function(v, patterns, idx) if (!is.null(v) && v %in% vars) v else guess_var(vars, patterns, idx)
    hz_vars <- setdiff(vars, norm_cols); if (length(hz_vars) == 0) hz_vars <- vars
    fname <- paste0(if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                      input$dataset_name else "data",
                    if (src == "curated") "_curated_normalised" else "_normalised")

    tagList(
      wellPanel(
        h5("Dataset",
           tags$small(style = "color: #777; margin-left: 6px; font-weight: 400;",
                      input$dataset_name)),
        if (has_curated())
          selectInput("norm_source", "Data source:",
                      choices = c("Uploaded data" = "uploaded", "Curated data" = "curated"),
                      selected = src),
        if (src == "curated")
          tags$div(style = "font-size: 0.75rem; color: #2a7a5a; margin: -4px 0 8px 0;",
                   icon("rotate"), " Normalising again on the kept tokens and new labels. ",
                   "Creates Curated + normalised data; the curated data stays as it is."),
        # IDs prefixed norm_: Visualise has its own tone_var / speaker_var.
        selectInput("norm_f0_var", "Select f0 (Hz) variable:",
                    choices = setNames(vars, var_types),
                    selected = keep_var("norm_f0_var", norm_f0_default(hz_vars)),
                    multiple = FALSE),
        uiOutput("norm_f0_note"),
        selectInput("norm_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = keep_var("norm_speaker_var", pick(cv$speaker, var_patterns$speaker, 2)),
                    multiple = FALSE),
        selectInput("norm_tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = keep_var("norm_tone_var", pick(cv$tone, var_patterns$tone, 3)),
                    multiple = FALSE),
        tags$hr(),
        radioButtons("mean_calc_method", "Speaker Mean f0 Options",
                     choices = list("Simple average" = "simple",
                                    "Equally weighted by each tone" = "weighted"),
                     selected = keep("mean_calc_method", "weighted")),
        tags$hr(),
        radioButtons("normalisation_method", "F0 Normalisation Options",
                     choices = list("Semitone referenced on speaker mean" = "semitone",
                                    "By-speaker Z-score" = "zscore",
                                    "By-speaker log Z-score (LZ)" = "lz"),
                     selected = keep("normalisation_method", "semitone")),
        # Semitones use no SD, so the correction only shows for the z-scores.
        conditionalPanel("input.normalisation_method != 'semitone'",
          checkboxInput("sd_correction", "Bessel\u2019s correction for the SD (n \u2212 1)",
                        value = keep("sd_correction", TRUE))),
        tags$hr(),
        # Pointer to the illustrated time-normalisation guide on the right.
        tags$div(style = "color:#888; font-size:0.72rem; margin:0 0 4px;",
                 "Multisyllabic or sub-syllabic data? See the illustrated guide on the right for which option to pick."),
        h5("Time Normalisation options ",
           tags$span(style = "font-weight:400; color:#888; font-size:0.8rem;", "(optional)")),
        uiOutput("norm_time_landmark_ui"),
        tags$hr(),
        actionButton("normalise_button", "Normalise"),
        tags$hr(),
        h5("Download"),
        textInput("output_filename", "Enter filename (without extension):",
                  value = if (same) keep("output_filename", fname) else fname),
        downloadButton("download_data", "Download Normalised Data")
      )
    )
  })

  # A note under the f0 menu when the chosen column is not the one the Start
  # page screens and still has rows without f0.
  output$norm_f0_note <- renderUI(screened_f0_note(norm_input(), input$norm_f0_var, start_cols))

  # Optional landmark time-normalisation controls. Always shown (an italic hint
  # stands in when the dataset carries no landmark columns), so it reads as an
  # optional step alongside the f0 options.
  output$norm_time_landmark_ui <- renderUI({
    ds   <- norm_input()
    vars <- if (!is.null(ds)) names(ds) else character(0)
    sets <- landmark_sets(vars)
    tok  <- norm_token_col(vars)   # every option needs the token column

    # Each token, or each part of a tier within a token, runs from its first
    # f0 to its last, so every option needs the token column.
    choices <- norm_time_sets(vars)

    if (length(choices) == 1) {   # only "(none)": no token column
      return(tags$div(style = "color: #999; font-size: 0.78rem; font-style: italic;",
        paste("No token variable, so time cannot be normalised here. Each token runs from its",
              "first f0 to its last, so the app must know which rows form a token.", norm_no_token)))
    }
    tvars <- norm_time_choices(vars)
    var_types <- paste0(vars, " {", sapply(ds, class), "}")
    # Keep a choice across re-renders (e.g. a curation change) while valid.
    keep_in <- function(id, valid, default) {
      v <- isolate(input[[id]]); if (!is.null(v) && v %in% valid) v else default }
    tagList(
      selectInput("norm_time_set", "Normalise time by:",
                  choices = choices, selected = keep_in("norm_time_set", choices, "__none__")),
      conditionalPanel(
        "input.norm_time_set && input.norm_time_set != '__none__'",
        selectInput("norm_time_var", "Time variable:",
                    choices = stats::setNames(tvars, var_types[match(tvars, vars)]),
                    selected = keep_in("norm_time_var", tvars, guess_var(tvars, var_patterns$time, 1))),
        selectInput("norm_time_token_var", "Token ID variable:",
                    choices = stats::setNames(vars, var_types),
                    selected = norm_pick("norm_time_token_var", vars, tok)),
        tags$div(style = "color:#888; font-size:0.72rem; margin:-6px 0 8px;",
          paste("Each token, or each part of the tier, runs from its first f0 (0) to its last (1),",
                "read from the f0 (Hz) variable above. Rows outside that span are NA. Removing rows",
                "without f0 does not change these columns."))),
      # Tiers are listed only when the data carries their landmark columns;
      # say why none are, rather than leave a tier-less menu unexplained.
      if (length(sets) == 0)
        tags$div(style = "color: #999; font-size: 0.75rem; font-style: italic; margin-top: -6px;",
          HTML(paste(
            "No landmark columns (<code>&lt;tier&gt;_start</code>, <code>&lt;tier&gt;_end</code>)",
            "in this dataset, so no tier is listed. For multisyllabic words, tick the",
            "<code>syllable</code> tier under <strong>Landmarks from TextGrid</strong> in",
            "F0 Extraction and export at native frame times.")))
    )
  })

  # Store normalisation result
  norm_display <- reactiveVal(NULL)
  norm_lead    <- reactiveVal(character(0))
  # The download: the full data of the last run, as stored for the other tabs
  # (every column of the data read, in its order, then the new columns; on the
  # curated route, the curated + normalised data). norm_display() is only the
  # narrow subset the table shows. Both are set and cleared together.
  norm_full    <- reactiveVal(NULL)
  norm_clear   <- function() { norm_display(NULL); norm_full(NULL) }
  # The table: the columns in `lead` first (the new time axes next to the
  # time, token and <set>_i columns they came from), so the result of a
  # click shows without scrolling, then the rest as before. Numbers show up
  # to 4 decimals (a time of 0.005 keeps its 3), and a wide table scrolls
  # sideways in its own box.
  norm_table <- function(d, lead = character(0)) {
    lead <- intersect(lead, names(d))
    d <- d[, c(lead, setdiff(names(d), lead)), drop = FALSE]
    dt <- DT::datatable(d, options = list(pageLength = 10, scrollX = TRUE))
    dec <- vapply(d, function(x) {
      if (!is.double(x)) return(NA_integer_)
      x <- x[is.finite(x)]
      if (!length(x)) return(NA_integer_)
      for (k in 0:3) if (all(abs(x - round(x, k)) < 1e-9 * pmax(1, abs(x)))) return(as.integer(k))
      4L
    }, integer(1))
    for (k in setdiff(unique(stats::na.omit(dec)), 0L))
      dt <- DT::formatRound(dt, names(d)[dec %in% k], digits = k, mark = "")
    dt
  }

  # Compute normalisation on button click
  observeEvent(input$normalise_button, {
    req(norm_input())
    src  <- norm_source()
    data <- norm_input()
    src_obj  <- data
    src_vars <- attr(data, "curate_vars")
    if (isTRUE(input$norm_f0_var %in% norm_cols)) {
      showNotification(sprintf(paste("%s is a normalised f0 variable. Choose the f0 variable in Hz",
                                     "(normalising is always done from Hz)."), input$norm_f0_var),
                       type = "error", duration = 8)
      if (src == "curated") norm_clear()
      return()
    }

    # --- f0 normalisation (R/normalise.R; scriptable + unit-tested) ----------
    # Wrapped so a failure (e.g. no usable speaker/tone column on raw extracted
    # data) still lets the optional time normalisation below run.
    f0_ok <- FALSE
    if (!is.null(input$norm_f0_var) && !is.null(input$norm_speaker_var) &&
        all(c(input$norm_f0_var, input$norm_speaker_var) %in% names(data))) {
      data <- tryCatch({
        d <- normalise_f0(
          data,
          f0          = input$norm_f0_var,
          speaker     = input$norm_speaker_var,
          tone        = input$norm_tone_var,
          method      = input$normalisation_method,
          mean_method = input$mean_calc_method,
          sd_correction = !isFALSE(input$sd_correction)
        )
        f0_ok <- TRUE
        d
      }, error = function(e) {
        showNotification(paste("f0 normalisation skipped:", conditionMessage(e)),
                         type = "warning", duration = 6)
        data
      })
    }

    # --- optional time normalisation: whole token (0-1) or per landmark ------
    # Each token, or each part of a tier within a token, runs from its first
    # f0 (0) to its last (1), read from the f0 (Hz) column (norm_span_f0()).
    # Only an option the panel offers for these columns (norm_time_sets()):
    # after a new upload without a token column the menus are no longer
    # drawn, but their inputs keep the last file's values.
    set <- input$norm_time_set
    if (!is.character(set) || length(set) != 1 ||
        !set %in% setdiff(norm_time_sets(names(data)), "__none__")) set <- NULL
    time_cols <- character(0); time_ctx <- character(0)
    if (!is.null(set)) {
      tvar <- input$norm_time_var
      if (is.null(tvar) || !tvar %in% norm_time_choices(names(data)))
        tvar <- guess_var(norm_time_choices(names(data)), var_patterns$time, 1)
      tok <- input$norm_time_token_var
      if (is.null(tok) || !tok %in% names(data)) tok <- norm_token_col(names(data))
      span_f0 <- norm_span_f0(data)
      if (is.null(tok) || is.null(span_f0)) {
        showNotification(if (is.null(tok))
          paste("No token variable, so time was not normalised. Each token runs from its",
                "first f0 to its last, so the app must know which rows form a token.",
                norm_no_token)
          else "No f0 (Hz) variable, so time was not normalised.",
          type = "error", duration = 8)
      } else {
        # The package's messages are kept back: the toast says what was made.
        # Only the one about <set>_tbound and the clock (the time column and
        # the boundaries in other units, or counted from another zero) is
        # shown, after the toast.
        clock_msg <- character(0)
        res <- tryCatch(withCallingHandlers(
          if (identical(set, "__token__")) normalise_time_token(data, tvar, tok, f0 = span_f0)
          else normalise_time_landmarks(data, tvar, set, f0 = span_f0, token = tok),
          message = function(m) {
            txt <- trimws(conditionMessage(m))
            if (grepl("_tbound is NA in", txt, fixed = TRUE)) clock_msg <<- c(clock_msg, txt)
            invokeRestart("muffleMessage")
          }),
          error = function(e) {
            showNotification(paste("Time normalisation skipped:", conditionMessage(e)),
                             type = "warning", duration = 8)
            NULL
          })
        if (!is.null(res)) {
          data <- res
          # intersect(), not a diff of the names: a rebuild replaces columns
          time_cols <- intersect(if (identical(set, "__token__")) "token_t01"
                                 else paste0(set, c("_t01", "_tseq", "_tbound")), names(data))
          time_ctx <- unique(c(tvar, tok, span_f0,
                               if (!identical(set, "__token__")) paste0(set, "_i")))
          showNotification(norm_time_toast(data, set, tok, span_f0),
                           type = "message", duration = 8)
          for (m in clock_msg) showNotification(m, type = "warning", duration = 12)
        }
      }
    }

    norm_col <- switch(input$normalisation_method,
                       zscore = "f0_zscore", lz = "f0_lz", "f0_st")

    # Store the full dataset with normalised columns (for other tabs)
    if (src == "curated") {
      if (!f0_ok) {
        showNotification(paste("The curated data was not normalised, so no curated +",
                               "normalised data was made. Check the f0, speaker and tone variables."),
                         type = "error", duration = 8)
        norm_clear()
        return()
      }
      # Normalised columns carried over from the first run (all tokens, the
      # original labels) are out of date: keep only the one this run produced.
      stale <- setdiff(c("f0_st", "f0_zscore", "f0_lz"), norm_col)
      data  <- data[, setdiff(names(data), stale), drop = FALSE]
      # The variables this run used (downstream menus default to them), the
      # run (source_picker() selects each new run once), and a fingerprint of
      # the curated data it came from (server.R clears this data when it changes).
      used <- c(src_vars, list(tone = input$norm_tone_var, speaker = input$norm_speaker_var))
      attr(data, "curate_vars") <- used[!duplicated(names(used), fromLast = TRUE)]
      attr(data, "run") <- input$normalise_button
      key_cols <- unique(c(src_vars$token, input$norm_f0_var, input$norm_speaker_var, input$norm_tone_var))
      attr(data, "curated_key_cols") <- key_cols
      attr(data, "curated_key") <- curated_key(src_obj, key_cols)
      curated_norm_data(data)
      tok <- vis_token_col(names(data))
      showNotification(sprintf(paste("Curated + normalised data ready%s. Model, GCA and GAMM",
                                     "switch to it; Visualise and Summarise list it."),
                               if (!is.null(tok)) sprintf(": %d tokens", length(unique(data[[tok]]))) else ""),
                       type = "message", duration = 6)
    } else {
      normalised_data(data)
    }

    # Display subset: f0 columns (when produced) + any time-normalisation
    # columns, with the time, token, span f0 and <set>_i columns they came from.
    keep <- c(input$norm_f0_var, input$norm_speaker_var, input$norm_tone_var,
              if (f0_ok) c("speaker_mean", norm_col),
              if (length(time_cols) > 0) c(time_ctx, time_cols))
    keep <- unique(intersect(keep, names(data)))
    norm_display(if (length(keep) > 0) data[, keep, drop = FALSE] else data)
    # The download writes every column: the data the other tabs get.
    norm_full(data)
    # The table shows the new axes first, after the time, token and <set>_i
    # columns they came from (norm_table()).
    norm_lead(if (length(time_cols) > 0) c(setdiff(time_ctx, span_f0), time_cols) else character(0))
    norm_route(src)
  })

  # The table and download show the last result: drop it on a new file or new
  # rows from the Start page (data_version), when the source is switched, or
  # when the curated + normalised data it showed is cleared.
  norm_route <- reactiveVal(NULL)
  observeEvent({ input$uploadfile; if (!is.null(data_version)) data_version() },
               norm_clear(), ignoreInit = TRUE, ignoreNULL = FALSE)
  observeEvent(norm_source(), norm_clear(), ignoreInit = TRUE)
  if (!is.null(curated_norm_data))
    observeEvent(curated_norm_data(), {
      if (is.null(curated_norm_data()) && identical(norm_route(), "curated")) norm_clear()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Display the normalised dataset
  output$normalised_data <- DT::renderDataTable({
    req(norm_display())
    norm_table(norm_display(), norm_lead())
  })

  # Download handler: every column of the last run (norm_full()), not just
  # the ones the table shows. write.csv() writes only the names and values,
  # so the attributes the curated route sets do not reach the file.
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$output_filename, ".csv")
    },
    content = function(file) {
      req(norm_full())
      fname <- paste0(input$output_filename, ".csv")
      write.csv(norm_full(), file, row.names = FALSE)
      showNotification(paste("Data saved as", fname), type = "message", duration = 4)
    }
  )
}
