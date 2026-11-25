###############################################
# Model: GAMM tab — Generalised Additive Mixed Model using mgcv
###############################################

gamm_ui <- function(input, output, session, dataset, normalised_data, gamm_pred_data = NULL, curated_data = NULL, cluster_data = NULL,
                    curated_norm_data = NULL,
                    start_cols = NULL, data_version = NULL, start_settings = NULL) {

  # The app tests bind this body to their own reactives, without these three
  # arguments in older tests: then they count as left out (NULL).
  for (nm in c("start_cols", "data_version", "start_settings"))
    if (!exists(nm, inherits = TRUE)) assign(nm, NULL)

  # Parts of the tier per token on a <set>_t01, <set>_tseq or <set>_tbound
  # (t01_parts(), global.R), counted in the data rather than read from the
  # name. 1 for a word, rhyme or vowel tier: a whole-token axis, like
  # token_t01. NA when they cannot be counted (the set's columns are
  # missing), which counts as several.
  gamm_parts <- function(tv, tok, df) {
    if (!is.character(tv) || length(tv) != 1 || !grepl("_(t01|tseq|tbound)$", tv) ||
        identical(tv, "token_t01")) return(NA_integer_)
    t01_parts(df, tv, tok)
  }

  # GAMM is the right tab for multisyllabic words, so the only thing to catch
  # here is picking the wrong landmark column: the within-segment 0-1 axis
  # (_t01) restarts in each part and is not a valid time axis for a
  # whole-word model. The in-sequence _tseq or _tbound, or token_t01, is the
  # one to use for that; _t01 suits syllables modelled one by one (each its own token and
  # label). A <set>_t01 with one part per token is not warned about: it is a
  # whole-token axis and gets the blue note below.
  output$gamm_multisyl_note <- renderUI({
    tv <- input$gamm_time_var
    if (is.null(tv) || !grepl("_t01$", tv) || identical(tv, "token_t01")) return(NULL)
    if (identical(gamm_parts(tv, input$gamm_token_var,
                             tryCatch(active_data(), error = function(e) NULL)), 1L)) return(NULL)
    tags$div(
      style = paste("background-color:#fff8e1; border-left:4px solid #e0a800;",
                    "padding:10px 14px; margin:8px 0; border-radius:4px;",
                    "color:#7a5d00; font-size:0.9rem;"),
      tags$span(style = "color:#c0392b;", icon("triangle-exclamation")),
      HTML(sprintf(" <code>%s</code> runs from 0 to 1 <em>within</em> each %s, from its first f0 to its last, so it restarts at every boundary and is <strong>not a valid time axis for a whole-word model</strong>. For that, use <code>%s</code> (syllables in sequence), <code>%s</code> (in sequence, gap kept) or <code>token_t01</code> (whole word, syllable ratio kept). It does suit syllables modelled one by one, each its own token with a label for its tone pattern and place, as for one Chao numeral per syllable in Summarise.",
                   tv, sub("_t01$", "", tv), sub("_t01$", "_tseq", tv), sub("_t01$", "_tbound", tv))))
  })

  # The amber notice (edge_f0_line(), global.R): tokens whose f0 does not
  # reach 0 or 1 on a time column the fit uses as it is. Never on the real
  # time column, which the fit rescales from each token's first f0 to its
  # last. Not on a <tier>_tseq or <tier>_tbound axis (several parts in
  # sequence per token): fit_gamm() keeps it as it is, on its own 0-to-n scale. Both
  # from Normalise run from the token's first f0 to its last (on _tseq, each
  # syllable's), so edge_f0_line() would find nothing there anyway. One with
  # one part per token runs from 0 to 1, so it is checked like a <tier>_t01.
  # One reactive, shared by the notice and the blue note below.
  gamm_edges <- reactive({
    tv <- input$gamm_time_var
    if (is.null(tv)) return(NULL)
    df <- tryCatch(active_data(), error = function(e) NULL)
    if (grepl("_(tseq|tbound)$", tv) && !identical(gamm_parts(tv, input$gamm_token_var, df), 1L))
      return(NULL)
    edge_f0_line(df, input$gamm_token_var, input$gamm_f0_var, tv)
  })

  # The fit's own check (resolve_time_norm()): a time column that is already
  # proportional ([0, 1] per token) is used as-is by fit_gamm() rather than
  # rescaled again, so say so before the user clicks Fit. *_t01 and
  # time_prop are recognised by name (with every value in [0, 1]), so
  # removing rows does not flip this note. An in-sequence _tseq or _tbound
  # axis is also used as it is, on its own 0-to-n scale (fit_gamm() recognises
  # it by name), and gets its own note: on _tseq one smooth cannot follow an
  # f0 jump at a boundary, on _tbound the gap is kept so it can. The red
  # warning above owns a <set>_t01 with several parts per token. A tier with
  # one part per token falls through to the 0-1 note.
  output$gamm_timenorm_note <- renderUI({
    tv  <- input$gamm_time_var
    tok <- input$gamm_token_var
    if (is.null(tv)) return(NULL)
    df <- tryCatch(active_data(), error = function(e) NULL)
    parts <- gamm_parts(tv, tok, df)
    if (grepl("_tseq$", tv) && !identical(parts, 1L)) {
      return(tags$div(
        style = paste("background-color:#e8f5f0; border-left:4px solid #78c2ad;",
                      "padding:10px 14px; margin:8px 0; border-radius:4px;",
                      "color:#2a7a5a; font-size:0.9rem;"),
        icon("circle-info"),
        HTML(sprintf(paste(
          " <code>%s</code> puts the syllables in sequence. Each runs from its first f0 to its last over",
          "one unit (0&ndash;1, 1&ndash;2, &hellip;), and a gap without f0 between them takes no",
          "room. The fit keeps this scale rather than rescaling each token to 0&ndash;1, so the",
          "syllable boundaries line up across tokens, and the plot marks them. One",
          "syllable&rsquo;s last f0 and the next one&rsquo;s first f0 both sit on the boundary,",
          "so one smooth cannot follow an f0 jump there, and raising <em>k</em> does not help.",
          "To model the whole word, use <code>%s_tbound</code> (in sequence, gap kept) or",
          "<code>token_t01</code>, or model each syllable as its own token on",
          "<code>%s_t01</code>."), tv, sub("_tseq$", "", tv), sub("_tseq$", "", tv)))))
    }
    if (grepl("_tbound$", tv) && !identical(parts, 1L)) {
      return(tags$div(
        style = paste("background-color:#e8f5f0; border-left:4px solid #78c2ad;",
                      "padding:10px 14px; margin:8px 0; border-radius:4px;",
                      "color:#2a7a5a; font-size:0.9rem;"),
        icon("circle-info"),
        HTML(sprintf(paste(
          " <code>%s</code> puts the syllables in sequence, gap kept: 0 is the",
          "word&rsquo;s first f0, the TextGrid boundaries sit at 1, 2, &hellip;, and the last f0 at the",
          "end. The fit keeps this scale rather than rescaling each token to 0&ndash;1, so the",
          "boundaries line up across tokens, and the plot marks them. The gap without f0 at a",
          "boundary is kept, so one smooth can follow an f0 jump across it."), tv))))
    }
    if (is.null(tok)) return(NULL)
    if (is.null(df) || !all(c(tv, tok) %in% names(df))) return(NULL)
    if (grepl("_t01$", tv) && !identical(tv, "token_t01") && !identical(parts, 1L)) return(NULL)
    if (!time_already_normalised(df[[tv]], df[[tok]], name = tv)) return(NULL)
    by_name <- grepl("_t01$", tv) || tv == "time_prop"
    why <- if (by_name) "its name marks a 0&ndash;1 axis, and every value lies in that range"
           else "every value lies in that range and a typical token covers most of it; some tokens may cover less"
    tags$div(
      style = paste("background-color:#e8f5f0; border-left:4px solid #78c2ad;",
                    "padding:10px 14px; margin:8px 0; border-radius:4px;",
                    "color:#2a7a5a; font-size:0.9rem;"),
      icon("circle-info"),
      HTML(sprintf(" <code>%s</code> %s already on 0&ndash;1 (%s). The fit uses it as it is, so 0 and 1 stay where the column puts them%s. To rescale each token from its first f0 to its last instead, call <code>fit_gamm(..., time_normalised = \"no\")</code> in R.",
                 tv, if (by_name) "is" else "looks", why,
                 t01_where(tv, !is.null(gamm_edges())))))
  })

  output$gamm_track_edges <- renderUI(gamm_edges())

  # Where 0 and 1 fall on a column used as it is, for the blue note. Only
  # when the amber notice is not shown: then every token's (or part's) f0
  # reaches 0 and 1. With the notice, the note points to it instead.
  t01_where <- function(tv, edge) {
    if (isTRUE(edge)) return(" (see the notice above)")
    if (identical(tv, "token_t01"))
      return(": on <code>token_t01</code> from Normalise, each token&rsquo;s first and last f0")
    if (grepl("_t01$", tv))
      return(sprintf(": on <code>%s</code> from Normalise, each %s&rsquo;s first and last f0",
                     tv, sub("_t01$", "", tv)))
    if (identical(tv, "time_prop"))
      return(": on <code>time_prop</code>, the first and last frames of the extracted region")
    ""
  }

  # The Normalise step in the exported code's data-source note. The time
  # columns there need the f0 (Hz) and token columns that tab used, since
  # each token, or each part of a tier, runs from its first f0 to its last.
  # A time column the data had before Normalise gets the general line.
  norm_time_lines <- function(time_var, token_var, before = NULL) {
    q <- function(x) encodeString(x, quote = '"')
    made <- !time_var %in% names(before)
    if (made && identical(time_var, "token_t01")) {
      c("# token_t01, made with the time, token and f0 (Hz) columns that tab used:",
        sprintf('#   dat <- shinytone::normalise_time_token(dat, "<time>", %s,', q(token_var)),
        '#                                          f0 = "<f0 (Hz)>")')
    } else if (made && grepl("_(t01|tseq|tbound)$", time_var)) {
      c(sprintf("# %s, made with the time, token and f0 (Hz) columns that tab used:", time_var),
        sprintf('#   dat <- shinytone::normalise_time_landmarks(dat, "<time>", %s,',
                q(sub("_(t01|tseq|tbound)$", "", time_var))),
        sprintf('#                                              f0 = "<f0 (Hz)>", token = %s)',
                q(token_var)))
    } else {
      c("# if it made a time column, normalise_time_token() or normalise_time_landmarks()",
        "# with the f0 (Hz) and token columns that tab used (f0 =, token =).")
    }
  }

  # --- Guide text ---
  output$gamm_guide <- renderUI({
    code_style <- "color: #555; background: #e8f5f0; padding: 1px 4px; border-radius: 3px;"

    tagList(
      guide_box("GAMM guide",
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Token ID:"), " A unique identifier for each token/syllable."),
          tags$li(tags$strong("f0:"), " An f0-related variable. Normalised f0 (e.g. semitone, z-score or log z-score) is recommended for more interpretable results.",
            " Use the ", tags$strong("Normalise"), " tab first, then select ", tags$em("Normalised data"),
            " (or, after Curate, ", tags$em("Curated + normalised data"), ") from the dataset dropdown to access ", tags$code(style = code_style, "f0_st"), ", ", tags$code(style = code_style, "f0_zscore"), " or ", tags$code(style = code_style, "f0_lz"), "."),
          tags$li(tags$strong("Time:"),
            " The time variable that orders f0 samples within each token. Each token is rescaled to [0, 1]",
            " from its first f0 to its last before fitting, so rows without f0 at its edges do not move it.",
            " A column already on 0\u20131 is used as it is: ",
            tags$code(style = code_style, "token_t01"),
            " from the Normalise tab (0 = each token's first f0, 1 = its last), or ",
            tags$code(style = code_style, "time_prop"),
            " from equal-point extraction (0 and 1 = the first and last frames of the extracted region).",
            " A ", tags$code(style = code_style, "<tier>_tseq"), " or ",
            tags$code(style = code_style, "<tier>_tbound"),
            " column is kept on its own 0-to-n scale. On a ", tags$code(style = code_style, "<tier>_tseq"),
            " column, one syllable's last f0 and the next",
            " one's first f0 share a time and keep their order in the data, so keep each token's",
            " rows in time order: the AR1 correction runs along that order."),
          tags$li(tags$strong("Speaker:"), " Grouping variable for by-speaker random effects."),
          tags$li(tags$strong("Item:"), " The word or syllable type (e.g. different segmental compositions). Grouping variable for by-item random effects."),
          tags$li(tags$strong("Tone category:"), " Fixed effect factor. Used to model f0 contour differences across tone categories."),
          tags$li(tags$strong("Duration* (optional):"), " Syllable or token duration. Included as a smooth covariate if selected.")
        ),
        tags$strong("How GAMMs work:"),
        tags$p(style = "margin-bottom: 8px;",
          "GAMMs fit non-linear smooth curves to the data, offering more flexibility than polynomials. The model learns the contour shape directly from the data and controls overfitting through penalisation."
        ),
        tags$strong("Smooth type:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Separate smooths:"), " Fits an independent smooth for each tone: ",
            tags$code(style = code_style, "s(time, by = tone)"),
            ". Each tone gets its own curve."),
          tags$li(tags$strong("Difference smooths:"), " Fits a reference smooth + difference smooths: ",
            tags$code(style = code_style, "s(time) + s(time, by = tone.ord)"),
            ". Tone is converted to an ordered factor internally. The difference smooths directly test whether each non-reference tone differs in shape from the reference (Sóskuthy, 2017).")
        ),
        tags$strong("Random smooths (speaker):"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(tags$strong("Speaker:"), " Basic random smooth by speaker ",
            tags$code(style = code_style, "s(time, speaker, bs = \"fs\")"),
            ". Cannot capture speaker-level variation in the tone effect."),
          tags$li(tags$strong("Speaker \u00d7 Tone:"), " Separate smooth per speaker\u2013tone combination ",
            tags$code(style = code_style, "s(time, speaker.tone, bs = \"fs\")"),
            ". Treats tokens from different tones within the same speaker as independent."),
          tags$li(tags$strong("Speaker by Tone:"), " Random smooth by speaker, separately for each tone level ",
            tags$code(style = code_style, "s(time, speaker, by = tone, bs = \"fs\")"),
            ". Behaves similarly to Speaker \u00d7 Tone."),
          tags$li(tags$strong("Speaker + Speaker by Tone:"), " Reference + difference random smooths ",
            tags$code(style = code_style, "s(time, speaker, bs = \"fs\") + s(time, speaker, by = tone.ord, bs = \"fs\")"),
            ". Recommended for difference smooths. Mirrors the fixed-effects structure in the random effects for well-calibrated Type I error (S\u00f3skuthy, 2021).")
        ),
        tags$strong("Key settings:"),
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Basis dimension k:"), " Controls the maximum wiggliness of the smooth. Higher k allows more complex curves, but k must be less than the number of unique data points per smooth. The actual complexity is determined by the data via penalisation."),
          tags$li(tags$strong("Basis type:"), " tp (thin plate regression spline, general-purpose default), cr (cubic regression spline, faster for large data), cc (cyclic cubic spline, for periodic contours)."),
          tags$li(tags$strong("AR1:"), " Corrects for temporal autocorrelation in the residuals. Fits an initial model, estimates the autocorrelation parameter rho (from the lag-1 correlation ",
            tags$em("within"), " each token), then refits with the correction. ",
            tags$strong("On by default:"), " densely-sampled f0 frames are strongly autocorrelated, so without it the smooth p-values are anticonservative. Uncheck it for a quick exploratory fit (it doubles fitting time, since rho needs an initial fit).",
            tags$br(),
            tags$span(style = "color: #7a5d00;",
              "AR1 assumes ", tags$strong("evenly-spaced frames within each token."),
              " It is robust to the occasional dropped frame, but treat rho with more caution for contours with long voiceless gaps (e.g. a medial voiceless consonant). Use the ",
              tags$strong("Run model diagnostics"), " ACF panel to check the correction worked."))
        ),
        tags$p(style = "margin-bottom: 0; margin-top: 8px;",
          tags$strong("Beyond the built-in options:"),
          " The controls above cover the smooth and random-smooth structures",
          " most often used for tone contours, but mgcv supports many more,",
          " e.g. tensor-product interactions with a covariate such as duration, ",
          tags$code(style = code_style, "te(time_norm, duration)"),
          ", or smooths that vary by a further factor such as prosodic",
          " position. Click ", tags$strong("Show R code"), " to get a runnable",
          " script that reproduces the current fit, then edit its model formula",
          " to fit any structure mgcv accepts."
        )
      ),
      # --- Collapsible illustrated guide for multisyllabic words ---
      tags$details(class = "msg-route",
        tags$style(HTML("
          details.msg-route{background:#f3f8fc;border:1px solid #cfe2f1;border-radius:8px;padding:7px 14px 11px;margin:12px 0 0;}
          .msg-route>summary{cursor:pointer;font-weight:700;color:#2c5d80;font-size:0.92rem;list-style:none;padding:1px 0;}
          .msg-route>summary::-webkit-details-marker{display:none;}
          .msg-route>summary::before{content:'\\25B8';color:#5b9bd5;display:inline-block;margin-right:8px;transition:transform .15s ease;}
          .msg-route[open]>summary::before{transform:rotate(90deg);}
          .msg-hint{color:#7aa6cc;font-weight:400;font-size:0.78rem;margin-left:6px;}
          .msg-route[open] .msg-hint{display:none;}
          .msg-intro{color:#3f5a72;font-size:0.83rem;line-height:1.5;margin:9px 0 0;}
          .msg-flow{display:flex;align-items:stretch;gap:9px;margin-top:11px;flex-wrap:wrap;}
          .msg-step{flex:1 1 185px;background:#fff;border:1px solid #e1e9f2;border-radius:7px;padding:8px 12px;}
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
        tags$summary(icon("music"), " Working with multisyllabic words?",
                     tags$span(class = "msg-hint", "(click to expand)")),
        tags$p(class = "msg-intro",
          HTML(paste("Fit a disyllabic (or longer) word as one curve, on the syllables in sequence",
                     "(<code>&lt;tier&gt;_tseq</code>), in sequence with the gap kept",
                     "(<code>&lt;tier&gt;_tbound</code>) or on the whole word (<code>token_t01</code>).",
                     "GAMM handles the extra shape complexity that polynomials and GCA cannot",
                     "summarise with a few interpretable terms."))),
        tags$div(class = "msg-flow",
          tags$div(class = "msg-step",
            tags$div(class = "msg-shead",
              tags$span(class = "msg-badge msg-num", "1"),
              tags$span(class = "msg-stitle", "Extract"),
              tags$span(class = "msg-tab", "F0 Extraction")),
            tags$div(class = "msg-swhy",
              "Add a landmark tier (e.g. syllable) so every f0 frame knows which syllable it belongs to.")),
          tags$div(class = "msg-arrow", HTML("&#10132;")),
          tags$div(class = "msg-step",
            tags$div(class = "msg-shead",
              tags$span(class = "msg-badge msg-num", "2"),
              tags$span(class = "msg-stitle", "Normalise time"),
              tags$span(class = "msg-tab", "Normalise")),
            tags$div(class = "msg-swhy",
              HTML(paste("Time Normalisation options: pick the tier. <code>&lt;tier&gt;_tseq</code>",
                         "puts the syllables in sequence: each runs from its first f0 to its last over one unit.",
                         "<code>&lt;tier&gt;_tbound</code> puts the TextGrid boundaries at 1, 2, ...,",
                         "with the gap kept.")))),
          tags$div(class = "msg-arrow", HTML("&#10132;")),
          tags$div(class = "msg-step msg-step-here",
            tags$div(class = "msg-shead",
              tags$span(class = "msg-badge msg-here", HTML("&#9679;")),
              tags$span(class = "msg-stitle", "GAMM")),
            tags$div(class = "msg-swhy",
              HTML(paste("You are here. Set the <strong>Time</strong> variable to",
                         "<code>&lt;tier&gt;_tseq</code> or <code>&lt;tier&gt;_tbound</code> to line the",
                         "syllables up, or to <code>token_t01</code> to keep the word&rsquo;s own timing."))))
        ),
        tags$div(class = "msg-tip",
          icon("lightbulb"), HTML(" <strong>For this workflow:</strong>"),
          tags$ul(style = "margin:4px 0 0 0; padding-left:18px;",
            tags$li(HTML("<strong>Tone category</strong> = the per-word <strong>tone-sequence label</strong> (e.g. <code>H.L</code>), one per token; not a per-syllable tone.")),
            tags$li(HTML(paste("Raise the <strong>basis dimension k</strong> so the smooth can capture",
                               "every syllable's shape (penalisation keeps the effective complexity in",
                               "check). On <code>&lt;tier&gt;_tseq</code>, one syllable&rsquo;s last f0 and the",
                               "next one&rsquo;s first f0 both sit on the boundary, so no <em>k</em> lets one",
                               "smooth follow an f0 jump there, such as a fall before a voiceless consonant",
                               "and a high start after it. For such words, model the whole word on",
                               "<code>&lt;tier&gt;_tbound</code>, which keeps the gap and still lines the",
                               "boundaries up, or on <code>token_t01</code> (<em>Whole token (0&ndash;1)</em>",
                               "in Normalise), which keeps the gap and the syllable ratio, so the boundary",
                               "falls at a different point in each token. Or model each syllable as its",
                               "own token on <code>&lt;syllable_tier&gt;_t01</code>."))),
            tags$li(HTML("Use a batch with a <strong>uniform syllable count</strong>, so segment boundaries line up across tokens (e.g. a disyllabic batch, then a trisyllabic batch, as separate runs)."))))
      )
    )
  })

  pick_dataset <- source_picker()   # selects Curated + normalised data once, when it appears

  # --- Helper: get active dataset based on selector ---
  active_data <- reactive({
    has_norm <- !is.null(normalised_data())
    has_cur  <- !is.null(curated_data) && !is.null(curated_data())
    has_clu  <- !is.null(cluster_data) && !is.null(cluster_data())
    has_cn   <- !is.null(curated_norm_data) && !is.null(curated_norm_data())
    sel <- input$gamm_dataset
    if (!is.null(sel) && sel == "clustered" && has_clu) {
      cluster_data()
    } else if (!is.null(sel) && sel == "curated_norm" && has_cn) {
      curated_norm_data()
    } else if (!is.null(sel) && sel == "curated" && has_cur) {
      curated_data()
    } else if (!is.null(sel) && sel == "normalised" && has_norm) {
      normalised_data()
    } else {
      dataset()
    }
  })

  # --- Column defaults (Start page, D8) ---
  # Token: the Start page's token column, when the data has it. f0: a
  # normalised column first (f0_st, f0_zscore, f0_lz and f0_norm*, which
  # var_patterns$f0 ranks first), then the column the Start page screens
  # (f0_corrected, say, rather than the raw f0), then the usual guess. Time
  # and the other roles keep their defaults (time prefers <tier>_tbound).
  gamm_col_default <- function(key, vars) {
    if (!length(vars)) return(NULL)
    if (identical(key, "token")) {
      tok <- start_col(start_cols, "token")
      return(if (!is.null(tok) && tok %in% vars) tok else guess_var(vars, var_patterns$token, 1))
    }
    g   <- guess_var(vars, var_patterns$f0, 2)
    scr <- start_col(start_cols, "f0")
    if (grepl("^f0_(st|zscore|lz)$|^f0_norm", g, ignore.case = TRUE) ||
        is.null(scr) || !scr %in% vars) g else scr
  }
  # The token and f0 menus show the default until the user picks another
  # column, and then keep that pick while the data has it: a change on the
  # Start page, another dataset in the menu or new rows do not override it.
  # A value equal to the default is not a pick, so the value a menu sends
  # when it is drawn does not count, and picking the default again makes the
  # menu follow the Start page once more.
  gamm_user_pick <- new.env()
  gamm_col_selected <- function(key, vars) {
    v <- gamm_user_pick[[key]]
    if (is.character(v) && length(v) == 1 && v %in% vars) v else gamm_col_default(key, vars)
  }
  for (key in c("token", "f0")) local({
    k  <- key
    id <- paste0("gamm_", k, "_var")
    observeEvent(input[[id]], {
      vars <- names(tryCatch(active_data(), error = function(e) NULL))
      v    <- input[[id]]
      gamm_user_pick[[k]] <- if (!length(vars) || identical(v, gamm_col_default(k, vars))) NULL else v
    })
  })
  # A change of column on the Start page reaches a menu still on its default
  # here, without drawing the sidebar again (which would reset the model
  # settings). When the change also changes the rows, the sidebar is drawn
  # again anyway, with the same columns selected.
  if (is.function(start_cols)) {
    observeEvent(start_cols(), {
      vars <- names(tryCatch(active_data(), error = function(e) NULL))
      if (!length(vars)) return()
      for (k in c("token", "f0")) {
        id  <- paste0("gamm_", k, "_var")
        cur <- input[[id]]
        sel <- gamm_col_selected(k, vars)
        if (!is.null(cur) && !is.null(sel) && !identical(cur, sel))
          updateSelectInput(session, id, selected = sel)
      }
    }, ignoreInit = TRUE)
  }

  # A one-line note under the f0 menu when it is not the column the Start
  # page screens and still has rows without f0 (screened_f0_note(), global.R).
  output$gamm_f0_note <- renderUI({
    screened_f0_note(tryCatch(active_data(), error = function(e) NULL),
                     input$gamm_f0_var, start_cols)
  })

  # --- Sidebar controls ---
  output$ui_gamm <- renderUI({
    has_norm <- !is.null(normalised_data())
    has_cur  <- !is.null(curated_data) && !is.null(curated_data())
    has_clu  <- !is.null(cluster_data) && !is.null(cluster_data())
    has_cn   <- !is.null(curated_norm_data) && !is.null(curated_norm_data())

    # Dataset choices
    ds_choices <- c("Uploaded data" = "uploaded")
    if (has_norm) {
      ds_choices <- c(ds_choices, "Normalised data" = "normalised")
    }
    if (has_cur) {
      ds_choices <- c(ds_choices, "Curated data" = "curated")
    }
    if (has_cn) {
      ds_choices <- c(ds_choices, "Curated + normalised data" = "curated_norm")
    }
    if (has_clu) {
      ds_choices <- c(ds_choices, "Clustered data" = "clustered")
    }
    ds_selected <- pick_dataset(input$gamm_dataset, ds_choices,
                                if (has_norm) "normalised" else "uploaded",
                                stamp = if (has_cn) attr(curated_norm_data(), "run"))

    # Get variable names from active dataset
    active <- active_data()
    vars <- if (!is.null(active)) names(active) else c("No dataset available")
    data_types <- if (!is.null(active)) sapply(active, class) else rep("NA", length(vars))
    var_types <- paste0(vars, " {", data_types, "}")

    # Duration choices: "None" + all variables
    dur_choices <- c("None" = "none", setNames(vars, var_types))

    tagList(
      wellPanel(
        selectInput("gamm_dataset", "Select dataset:",
                    choices = ds_choices, selected = ds_selected),
        curated_reminder("gamm_dataset"),
        # Start-page defaults; isolated, so a change there updates the menus
        # (observer above) rather than drawing the sidebar again
        selectInput("gamm_token_var", "Select Token ID variable:",
                    choices = setNames(vars, var_types),
                    selected = isolate(gamm_col_selected("token", vars))),
        selectInput("gamm_f0_var", "Select f0 variable (normalised f0 recommended):",
                    choices = setNames(vars, var_types),
                    selected = isolate(gamm_col_selected("f0", vars))),
        uiOutput("gamm_f0_note"),
        # auto-prefers a <tier>_tbound, then a <tier>_tseq, when some token
        # has several parts of the tier (guess_time_var(), with the data and
        # the token above); Model and GCA prefer <tier>_tseq first
        selectInput("gamm_time_var", "Select Time variable:",
                    choices = setNames(vars, var_types),
                    selected = guess_time_var(vars, 3, d = active,
                                              token = isolate(gamm_col_selected("token", vars)),
                                              prefer = "tbound")),
        selectInput("gamm_speaker_var", "Select Speaker variable:",
                    choices = setNames(vars, var_types),
                    selected = role_var(active, "speaker", vars, var_patterns$speaker, 4)),
        selectInput("gamm_tone_var", "Select Tone category variable:",
                    choices = setNames(vars, var_types),
                    selected = role_var(active, "tone", vars, var_patterns$tone, 5)),
        selectInput("gamm_item_var", "Select Item variable (word/syllable type):",
                    choices = setNames(vars, var_types),
                    selected = guess_var(vars, var_patterns$item, 6)),
        selectInput("gamm_dur_var", "Select Duration variable (optional):",
                    choices = dur_choices, selected = "none"),
        tags$hr(),
        tags$strong("Smooth type:"),
        radioButtons("gamm_smooth_type", NULL,
                     choices = list("Separate smooths by tone" = "separate",
                                    "Reference + difference smooths" = "difference"),
                     selected = "difference"),
        tags$strong("Smooth settings:"),
        numericInput("gamm_k", "Basis dimension k:",
                     value = 10, min = 4, max = 20, step = 1),
        radioButtons("gamm_bs", "Basis type:",
                     choices = list("tp (thin plate)" = "tp",
                                    "cr (cubic regression)" = "cr",
                                    "cc (cyclic cubic)" = "cc"),
                     selected = "tp", inline = TRUE),
        tags$hr(),
        tags$strong("Random intercepts:"),
        checkboxInput("gamm_ri_speaker", "Speaker", value = FALSE),
        checkboxInput("gamm_ri_item", "Item", value = TRUE),
        tags$strong("Random smooths (speaker):"),
        radioButtons("gamm_rs_type", NULL,
                     choices = list(
                       "None" = "none",
                       "Speaker" = "speaker",
                       "Speaker \u00d7 Tone" = "speaker_tone",
                       "Speaker by Tone" = "speaker_by_tone",
                       "Speaker + Speaker by Tone" = "ref_diff"
                     ),
                     selected = "ref_diff"),
        tags$hr(),
        tags$strong("Autocorrelation:"),
        checkboxInput("gamm_ar1", "Include AR1 correction", value = TRUE),
        tags$hr(),
        uiOutput("gamm_warning"),
        actionButton("gamm_button", "Fit GAMM"),
        div(style = "margin-top: 4px;",
          actionButton("gamm_show_code", "Show R code", icon = icon("code"))
        ),
        uiOutput("gamm_diagnose_ui"),
        tags$hr(),
        h5("Download"),
        textInput("gamm_filename", "Enter filename (without extension):",
                  value = if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                            paste0(input$dataset_name, "_gamm")
                          else "gamm_plot"),
        downloadButton("gamm_download", "Download Plot"),
        div(style = "margin-top: 6px;",
          downloadButton("gamm_download_model", "Download Model (.rds)")
        )
      )
    )
  })

  # --- Auto-uncheck Speaker random intercept when any random smooth is enabled ---
  # (bs = "fs" already includes random intercepts, so separate intercept is redundant)
  observeEvent(input$gamm_rs_type, {
    if (!is.null(input$gamm_rs_type) && input$gamm_rs_type != "none") {
      updateCheckboxInput(session, "gamm_ri_speaker", value = FALSE)
    }
  })

  # --- Dynamic warning based on dataset size + model complexity ---
  output$gamm_warning <- renderUI({
    data <- active_data()
    if (is.null(data)) return(NULL)
    n_rows <- nrow(data)
    rs_type <- input$gamm_rs_type
    use_ar1 <- input$gamm_ar1

    # Estimate complexity
    is_complex_rs <- !is.null(rs_type) && rs_type %in% c("speaker_tone", "speaker_by_tone", "ref_diff")
    is_large <- n_rows > 5000
    is_very_large <- n_rows > 20000

    msg <- NULL
    if (is_very_large) {
      msg <- paste0(
        "Very large dataset (", format(n_rows, big.mark = ","), " rows). ",
        "Fitting may take a long time. Consider using the R code in your local environment."
      )
    } else if (is_large && (is_complex_rs || isTRUE(use_ar1))) {
      msg <- paste0(
        "Large dataset (", format(n_rows, big.mark = ","), " rows) with complex random effects",
        if (isTRUE(use_ar1)) " and AR1 correction" else "",
        ". Fitting may take several minutes."
      )
    } else if (is_large) {
      msg <- paste0(
        "Large dataset (", format(n_rows, big.mark = ","), " rows). ",
        "Fitting may take a minute or two."
      )
    }

    if (!is.null(msg)) {
      tags$div(
        style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 6px 10px; border-radius: 4px; margin-bottom: 8px; font-size: 0.82rem;",
        icon("clock"), " ", msg
      )
    }
  })

  # --- Result storage ---
  gamm_model <- reactiveVal(NULL)
  gamm_fit_obj <- reactiveVal(NULL)   # full shinytone_gamm object (for diagnostics)
  gamm_summary_data <- reactiveVal(NULL)
  gamm_convergence_warning <- reactiveVal(NULL)
  gamm_plot_data <- reactiveVal(NULL)
  gamm_formula_str <- reactiveVal(NULL)
  gamm_rho_val <- reactiveVal(NULL)
  gamm_fitting <- reactiveVal(FALSE)
  gamm_diag_data <- reactiveVal(NULL)  # NULL until the user clicks "Run model diagnostics"
  # A new file, or new rows from the Start page (data_version: its boxes or
  # columns, discarded tokens, metadata or duration columns), clears the old
  # fit, which was fitted on rows the tab no longer has.
  observeEvent({ input$uploadfile; if (!is.null(data_version)) data_version() }, {
    for (rv in list(gamm_model, gamm_fit_obj, gamm_summary_data, gamm_convergence_warning,
                    gamm_plot_data, gamm_formula_str, gamm_rho_val, gamm_diag_data)) rv(NULL)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # --- Core computation ---
  observeEvent(input$gamm_button, {
    req(active_data())
    req(input$gamm_token_var, input$gamm_time_var,
        input$gamm_speaker_var, input$gamm_tone_var,
        input$gamm_f0_var, input$gamm_item_var)

    progress_id <- showNotification(
      tagList(icon("spinner", class = "fa-spin"),
              " Fitting GAMM model and running diagnostics... This may take a while."),
      duration = NULL, closeButton = FALSE, type = "message"
    )
    on.exit(removeNotification(progress_id), add = TRUE)

    gamm_convergence_warning(NULL)
    gamm_rho_val(NULL)
    gamm_diag_data(NULL)   # stale diagnostics belong to the previous fit

    # Map the radio-button random-smooth strategy onto the package API.
    rs_type    <- input$gamm_rs_type
    rs_for_pkg <- if (rs_type %in% c("speaker", "speaker_tone",
                                     "speaker_by_tone", "ref_diff"))
                    rs_type
                  else
                    "none"

    dur_var <- input$gamm_dur_var
    use_dur <- !is.null(dur_var) && dur_var != "none"

    # All preparation, formula construction, fitting (and optional AR1
    # refit) lives in the package function fit_gamm() (R/gamm.R).
    fit <- tryCatch(
      fit_gamm(
        active_data(),
        f0       = input$gamm_f0_var,
        time     = input$gamm_time_var,
        token    = input$gamm_token_var,
        tone     = input$gamm_tone_var,
        speaker  = input$gamm_speaker_var,
        item     = input$gamm_item_var,
        duration = if (use_dur) dur_var else NULL,
        k                        = as.integer(input$gamm_k),
        bs                       = input$gamm_bs,
        smooth_type              = input$gamm_smooth_type,
        random_intercept_speaker = isTRUE(input$gamm_ri_speaker),
        random_intercept_item    = isTRUE(input$gamm_ri_item),
        random_smooth            = rs_for_pkg,
        use_ar1                  = isTRUE(input$gamm_ar1)
      ),
      error = function(e) {
        showNotification(paste("Model fitting error:", e$message),
                         type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(fit)) return()

    model       <- fit$model
    tone_var    <- input$gamm_tone_var
    speaker_var <- input$gamm_speaker_var
    item_var    <- input$gamm_item_var
    time_var    <- input$gamm_time_var

    if (!is.null(fit$convergence_warning)) {
      gamm_convergence_warning(fit$convergence_warning)
    }
    if (!is.null(fit$rho)) gamm_rho_val(fit$rho)

    gamm_formula_str(fit$formula_str)
    gamm_model(model)
    gamm_fit_obj(fit)

    # --- Extract summary tables ---
    # Name-cleaning back to user's column names is UI-specific so it stays
    # here rather than in the package function.
    model_sum <- summary(model)
    p_table <- as.data.frame(model_sum$p.table)
    p_table <- tibble::rownames_to_column(p_table, var = "Term")
    p_table$Term <- gsub("^\\.tone_ord", paste0(tone_var, ".ord"), p_table$Term)
    p_table$Term <- gsub("^\\.tone",     tone_var,                 p_table$Term)

    s_table <- as.data.frame(model_sum$s.table)
    s_table <- tibble::rownames_to_column(s_table, var = "Smooth")
    s_table$Smooth <- gsub("\\.time_norm",    time_var,                              s_table$Smooth)
    s_table$Smooth <- gsub("\\.tone_ord",     paste0(tone_var, ".ord"),              s_table$Smooth)
    s_table$Smooth <- gsub("\\.tone",         tone_var,                              s_table$Smooth)
    s_table$Smooth <- gsub("\\.speaker_tone", paste0(speaker_var, ".", tone_var),    s_table$Smooth)
    s_table$Smooth <- gsub("\\.speaker",      speaker_var,                           s_table$Smooth)
    s_table$Smooth <- gsub("\\.item",         item_var,                              s_table$Smooth)
    if (use_dur) {
      s_table$Smooth <- gsub("\\.duration", dur_var, s_table$Smooth)
    }

    gamm_summary_data(list(p_table = p_table, s_table = s_table))

    # Population-level per-tone predictions, computed by predict_gamm()
    # which knows the smooth_type / random_smooth / duration setup. An error
    # here shows a notification and leaves no curves; it must not end the
    # session.
    plot_df <- tryCatch(
      predict_gamm(fit, n = 200),
      error = function(e) {
        showNotification(paste("The model was fitted, but its curves could not be predicted:",
                               conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )
    gamm_plot_data(plot_df)
    if (!is.null(gamm_pred_data)) gamm_pred_data(plot_df)

    # Diagnostics are NOT run automatically: diagnose_gamm() (k-check,
    # per-token ACF, concurvity) is not free, so running it on every fit would
    # slow the fit down. gamm_diag_data() was reset to NULL at the start of
    # this fit; the user runs diagnostics on demand via the sidebar button.
  })

  # ==========================================================================
  # Model diagnostics
  # --------------------------------------------------------------------------
  # Diagnostics are computed on demand when the user clicks the sidebar
  # "Run model diagnostics" button (not automatically, to keep fits fast).
  # Diagnostics mirror
  # mgcv::gam.check(): residual behaviour (Q-Q, residuals vs fitted, histogram,
  # observed vs fitted), the basis-dimension (k) check, and residual
  # autocorrelation (which motivates the AR1 option). The heavy lifting lives
  # in the package function diagnose_gamm() (R/gamm.R); this section only turns
  # its numeric output into tables and ggplots.
  # ==========================================================================

  # --- Sidebar diagnostics section (always shown; the button is disabled
  #     until a model exists, so users can see the control before fitting). ---
  output$gamm_diagnose_ui <- renderUI({
    has_fit <- !is.null(gamm_model())
    tagList(
      tags$hr(),
      h5("Model diagnostics"),
      tags$p(style = "font-size: 0.8rem; color: #555; margin: 4px 0 8px;",
        "Check residual normality, whether the basis dimension ", tags$em("k"),
        " is large enough, and leftover autocorrelation. Run once a model is ",
        "fitted (kept separate from fitting so the fit itself stays fast)."),
      if (has_fit) {
        actionButton("gamm_diagnose", "Run model diagnostics", icon = icon("stethoscope"))
      } else {
        tagList(
          tagAppendAttributes(
            actionButton("gamm_diagnose", "Run model diagnostics",
                         icon = icon("stethoscope")),
            disabled = NA
          ),
          tags$p(style = "font-size: 0.8rem; color: #888; margin-top: 4px;",
            "Fit a model first to enable diagnostics.")
        )
      },
      if (!is.null(gamm_diag_data())) {
        div(style = "margin-top: 6px;",
          downloadButton("gamm_diag_download", "Download diagnostics"))
      }
    )
  })

  # --- Compute diagnostics on demand ---
  observeEvent(input$gamm_diagnose, {
    req(gamm_fit_obj())
    progress_id <- showNotification(
      tagList(icon("spinner", class = "fa-spin"), " Computing diagnostics..."),
      duration = NULL, closeButton = FALSE, type = "message"
    )
    on.exit(removeNotification(progress_id), add = TRUE)

    diag <- tryCatch(
      diagnose_gamm(gamm_fit_obj()),
      error = function(e) {
        showNotification(paste("Diagnostics error:", e$message),
                         type = "error", duration = 10)
        NULL
      }
    )
    gamm_diag_data(diag)
  })

  # --- Shared ggplot builders (used by both the on-screen plots and the
  #     downloaded image) ---
  gamm_diag_resid_grobs <- function(diag) {
    rdf <- diag$resid_df
    base_theme <- theme_bw(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 12))
    pt_col <- "#2c6e91"; line_col <- "#c0392b"

    p_qq <- ggplot(rdf, aes(sample = residual)) +
      stat_qq(size = 0.7, alpha = 0.45, colour = pt_col) +
      stat_qq_line(colour = line_col, linewidth = 0.7) +
      labs(title = "Q–Q plot of residuals",
           x = "Theoretical quantiles", y = "Deviance residuals") +
      base_theme
    p_rf <- ggplot(rdf, aes(x = fitted, y = residual)) +
      geom_hline(yintercept = 0, colour = line_col, linewidth = 0.6) +
      geom_point(size = 0.7, alpha = 0.35, colour = pt_col) +
      labs(title = "Residuals vs fitted",
           x = "Fitted values", y = "Deviance residuals") +
      base_theme
    p_hist <- ggplot(rdf, aes(x = residual)) +
      geom_histogram(bins = 40, fill = "#78c2ad", colour = "white",
                     linewidth = 0.2) +
      labs(title = "Histogram of residuals",
           x = "Deviance residuals", y = "Count") +
      base_theme
    p_of <- ggplot(rdf, aes(x = fitted, y = observed)) +
      geom_abline(slope = 1, intercept = 0, colour = line_col, linewidth = 0.6) +
      geom_point(size = 0.7, alpha = 0.35, colour = pt_col) +
      labs(title = "Observed vs fitted",
           x = "Fitted values", y = "Observed values") +
      base_theme
    list(p_qq, p_rf, p_hist, p_of)
  }

  gamm_diag_acf_ggplot <- function(diag) {
    adf <- diag$acf
    ci  <- diag$acf_ci
    title <- if (isTRUE(diag$acf_whitened))
               "ACF of AR1-whitened residuals (per token)"
             else if (isTRUE(diag$acf_grouped))
               "Autocorrelation of residuals (per token)"
             else
               "Autocorrelation of residuals (ACF)"
    p <- ggplot(adf, aes(x = lag, y = acf)) +
      geom_hline(yintercept = 0, colour = "#888", linewidth = 0.5) +
      geom_segment(aes(xend = lag, yend = 0), colour = "#2c6e91", linewidth = 0.8) +
      labs(title = title, x = "Lag", y = "ACF") +
      theme_bw(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 12))
    if (!is.na(ci)) {
      p <- p + geom_hline(yintercept = c(-ci, ci), linetype = "dashed",
                          colour = "#c0392b", linewidth = 0.6)
    }
    p
  }

  # --- Residual panel (2x2 grid) ---
  output$gamm_diag_resid_plot <- renderPlot({
    req(gamm_diag_data())
    gridExtra::grid.arrange(grobs = gamm_diag_resid_grobs(gamm_diag_data()),
                            ncol = 2)
  }, width = 800, height = 560)

  # --- Residual ACF ---
  output$gamm_diag_acf_plot <- renderPlot({
    req(gamm_diag_data())
    gamm_diag_acf_ggplot(gamm_diag_data())
  }, width = 800, height = 260)

  # --- Diagnostics main-panel container (tables + plot slots + guide) ---
  output$gamm_diagnostics <- renderUI({
    diag <- gamm_diag_data()
    if (is.null(diag)) {
      msg <- if (is.null(gamm_fit_obj()))
        "Fit a GAMM, then click “Run model diagnostics” in the sidebar to check residuals, the basis dimension k, and leftover autocorrelation."
      else
        "Click “Run model diagnostics” in the sidebar to compute the residual, k, and autocorrelation checks for the current fit."
      return(tags$p(style = "color: #777; font-size: 0.9rem; margin-top: 8px;", msg))
    }

    th_style   <- "padding: 4px 10px; border-bottom: 2px solid #ddd; text-align: center;"
    td_style   <- "padding: 4px 10px; text-align: center;"
    td_left    <- "padding: 4px 10px; text-align: left; font-family: monospace; font-size: 0.82rem;"

    # ---- Basis-dimension (k) check table ----
    kc <- diag$k_check
    k_block <- if (is.null(kc)) {
      tags$p(style = "color: #777; font-size: 0.86rem;",
        "The basis-dimension check could not be computed for this model.")
    } else {
      disp_cols <- intersect(c("k'", "edf", "k-index", "p-value"), names(kc))
      fmt_k <- function(cn, val) {
        if (is.na(val)) return("–")
        if (cn == "k'") return(formatC(round(as.numeric(val)), format = "d"))
        if (cn == "edf") return(formatC(as.numeric(val), format = "f", digits = 2))
        if (cn == "p-value") {
          v <- as.numeric(val)
          return(if (v < 0.0001) formatC(v, format = "e", digits = 2)
                 else formatC(v, format = "f", digits = 4))
        }
        formatC(as.numeric(val), format = "f", digits = 3)
      }
      header <- tags$tr(
        tags$th(style = paste0(th_style, "text-align:left;"), "Smooth"),
        lapply(disp_cols, function(cn) {
          nm <- if (cn == "p-value") "p-value" else cn
          tags$th(style = th_style, nm)
        }),
        tags$th(style = th_style, "flag")
      )
      body <- lapply(seq_len(nrow(kc)), function(i) {
        row  <- kc[i, ]
        flag <- row$k_flag
        row_bg  <- if (identical(flag, "low")) "background-color: #fdecea;" else ""
        txt_col <- if (identical(flag, "na")) "color: #9a9a9a;" else ""
        tags$tr(style = row_bg,
          tags$td(style = paste0(td_left, txt_col), as.character(row$Smooth)),
          lapply(disp_cols, function(cn)
            tags$td(style = paste0(td_style, txt_col), fmt_k(cn, row[[cn]]))),
          tags$td(style = td_style,
            if (identical(flag, "low"))
              tags$span(style = "color: #c0392b; font-weight: 600;",
                        icon("triangle-exclamation"), " k low?")
            else if (identical(flag, "ok"))
              tags$span(style = "color: #2e7d32;", icon("check"))
            else tags$span(style = "color: #bbb;", "–"))
        )
      })
      any_low <- any(kc$k_flag == "low", na.rm = TRUE)
      tagList(
        tags$table(
          style = "margin-top: 6px; border-collapse: collapse; font-size: 0.86rem; width: 100%;",
          tags$thead(header),
          tags$tbody(body)
        ),
        if (any_low) {
          tags$div(
            style = "background-color: #fdecea; border: 1px solid #e6a9a1; padding: 6px 10px; border-radius: 4px; margin-top: 8px; font-size: 0.84rem;",
            icon("triangle-exclamation"),
            HTML(" One or more time smooths look under-resourced: a <strong>k-index below 1</strong> with a <strong>small p-value</strong> means the basis dimension <em>k</em> may be too low. Raise <em>k</em> in the sidebar and refit, then diagnose again."))
        } else {
          tags$div(
            style = "background-color: #eafaf1; border: 1px solid #b7e4c7; padding: 6px 10px; border-radius: 4px; margin-top: 8px; font-size: 0.84rem;",
            icon("check"),
            HTML(" Basis dimensions look adequate: no time smooth combines a k-index below 1 with a significant p-value."))
        }
      )
    }

    # ---- Concurvity table (advanced, collapsible) ----
    cc <- diag$concurvity
    conc_block <- if (is.null(cc)) {
      NULL
    } else {
      term_cols <- setdiff(names(cc), "Measure")
      colour_cell <- function(v) {
        vn <- suppressWarnings(as.numeric(v))
        col <- if (is.na(vn)) "" else if (vn >= 0.8) "color:#c0392b; font-weight:600;"
               else if (vn >= 0.5) "color:#c07a00;" else "color:#2e7d32;"
        list(txt = if (is.na(vn)) "–" else formatC(vn, format = "f", digits = 2),
             style = col)
      }
      header <- tags$tr(
        tags$th(style = paste0(th_style, "text-align:left;"), "Measure"),
        lapply(term_cols, function(cn) tags$th(style = th_style, cn))
      )
      body <- lapply(seq_len(nrow(cc)), function(i) {
        row <- cc[i, ]
        tags$tr(
          tags$td(style = td_left, as.character(row$Measure)),
          lapply(term_cols, function(cn) {
            cell <- colour_cell(row[[cn]])
            tags$td(style = paste0(td_style, cell$style), cell$txt)
          })
        )
      })
      tags$details(
        style = "margin-top: 14px; background: #f7f9fb; border: 1px solid #e1e9f2; border-radius: 6px; padding: 6px 12px 10px;",
        tags$summary(style = "cursor: pointer; font-weight: 700; color: #2c5d80;",
                     "Concurvity (advanced)"),
        tags$p(style = "font-size: 0.83rem; color: #555; margin: 8px 0 4px;",
          HTML("Concurvity is the smooth-term analogue of collinearity (0 = independent, 1 = fully confounded). Values near 1 mean a smooth is nearly reproducible from the others, so its individual estimate and p-value are unreliable. High concurvity between the population time smooths and the by-speaker random smooths is common and usually harmless; treat it as a caution, not a failure.")),
        tags$div(style = "overflow-x: auto;",
          tags$table(
            style = "margin-top: 4px; border-collapse: collapse; font-size: 0.84rem; width: 100%;",
            tags$thead(header),
            tags$tbody(body)))
      )
    }

    # ---- ACF section note (AR1- and grouping-aware) ----
    acf_note <- if (isTRUE(diag$acf_whitened)) {
      tags$div(
        style = "font-size: 0.84rem; color: #555; margin: 2px 0 8px;",
        HTML(sprintf("An AR1 correction was applied (rho = %s). The ACF below is computed on the <strong>AR1-whitened residuals, per token</strong>, so it shows whether the correction worked: if it did, the lag-1 bar should now sit <em>inside</em> the dashed band. Bars still poking well outside it mean autocorrelation remains. Consider a different structure.",
                     formatC(diag$rho, format = "f", digits = 3))))
    } else if (isTRUE(diag$acf_grouped)) {
      tags$div(
        style = "font-size: 0.84rem; color: #555; margin: 2px 0 8px;",
        HTML("Computed <strong>per token</strong>, as <code>itsadug::acf_resid</code> does: autocorrelation within tokens is pooled across tokens, so token boundaries don't distort the low lags. Bars decaying slowly across lags (rather than dropping inside the dashed band after lag 1) indicate temporal autocorrelation; enable <strong>AR1 correction</strong> in the sidebar and refit."))
    } else {
      tags$div(
        style = "font-size: 0.84rem; color: #555; margin: 2px 0 8px;",
        HTML("Bars decaying slowly across lags (rather than dropping inside the dashed band after lag 1) indicate temporal autocorrelation. If you see that, enable <strong>AR1 correction</strong> in the sidebar and refit."))
    }

    tagList(
      tags$hr(),
      tags$h4(icon("stethoscope"), " Model diagnostics"),

      guide_box("How to read these diagnostics",
        tags$ul(style = "margin-bottom: 0; padding-left: 18px;",
          tags$li(tags$strong("Basis dimension (k):"),
            " the key check. If a time smooth has a k-index below 1 and a small p-value, its ",
            tags$em("k"), " is likely too low, most convincingly when its ",
            tags$em("edf"), " is also close to ", tags$em("k'"),
            ". Raise ", tags$em("k"),
            " and refit. Random-effect terms (by-speaker / by-item) are not checked this way."),
          tags$li(tags$strong("Q–Q plot:"),
            " residuals should fall along the red line. Systematic curvature at the tails signals non-normal residuals."),
          tags$li(tags$strong("Residuals vs fitted:"),
            " should be a flat, patternless band around zero. A funnel shape means non-constant variance."),
          tags$li(tags$strong("Histogram:"),
            " should look roughly bell-shaped and centred on zero."),
          tags$li(tags$strong("Observed vs fitted:"),
            " points should hug the red 1:1 line; scatter reflects unexplained variation."),
          tags$li(tags$strong("Residual ACF:"),
            " checks for leftover temporal autocorrelation, computed per token. Without AR1 it motivates turning the correction on; with AR1 it shows the whitened residuals so you can confirm the correction worked.")),
        open = FALSE),

      tags$h5(style = "margin-top: 14px;", "Basis dimension check (k)"),
      k_block,

      tags$h5(style = "margin-top: 16px;", "Residual checks"),
      tags$div(class = "plot-spinner-wrap",
        plotOutput("gamm_diag_resid_plot", height = "560px", width = "800px"),
        tags$div(class = "plot-spinner")
      ),

      tags$h5(style = "margin-top: 16px;", "Residual autocorrelation"),
      acf_note,
      tags$div(class = "plot-spinner-wrap",
        plotOutput("gamm_diag_acf_plot", height = "260px", width = "800px"),
        tags$div(class = "plot-spinner")
      ),

      conc_block
    )
  })

  # --- Download handler (diagnostics image: 4 residual panels + ACF) ---
  output$gamm_diag_download <- downloadHandler(
    filename = function() {
      base <- if (!is.null(input$gamm_filename) && nzchar(input$gamm_filename))
                input$gamm_filename else "gamm"
      paste0(base, "_diagnostics.png")
    },
    content = function(file) {
      req(gamm_diag_data())
      diag  <- gamm_diag_data()
      grobs <- gamm_diag_resid_grobs(diag)
      acf_p <- gamm_diag_acf_ggplot(diag)
      grDevices::png(file, width = 9, height = 11, units = "in", res = 200)
      on.exit(grDevices::dev.off(), add = TRUE)
      gridExtra::grid.arrange(
        grobs = c(grobs, list(acf_p)),
        layout_matrix = rbind(c(1, 2), c(3, 4), c(5, 5)),
        heights = c(1, 1, 0.8)
      )
      showNotification("Diagnostics image saved.", type = "message", duration = 4)
    }
  )

  # --- Summary box ---
  output$gamm_summary <- renderUI({
    req(gamm_summary_data())
    sum_data <- gamm_summary_data()
    p_table <- sum_data$p_table
    s_table <- sum_data$s_table
    model <- gamm_model()
    warn_msg <- gamm_convergence_warning()
    rho <- gamm_rho_val()

    data <- active_data()
    token_var <- input$gamm_token_var
    speaker_var <- input$gamm_speaker_var
    tone_var <- input$gamm_tone_var
    item_var <- input$gamm_item_var
    formula_display <- gamm_formula_str()

    n_obs <- nrow(model$model)
    n_speakers <- length(unique(data[[speaker_var]]))
    n_tokens <- length(unique(data[[token_var]]))
    n_items <- length(unique(data[[item_var]]))
    n_tones <- length(unique(data[[tone_var]]))

    # Shared table styles
    th_style <- "padding: 4px 10px; border-bottom: 2px solid #ddd; text-align: center;"
    td_style <- "padding: 4px 10px; text-align: center;"
    td_style_left <- "padding: 4px 10px; text-align: left; font-family: monospace; font-size: 0.82rem;"

    # --- Parametric coefficients table ---
    p_col_names <- names(p_table)
    p_header <- tagList(lapply(p_col_names, function(cn) {
      display_name <- cn
      if (cn == "Std. Error") display_name <- "SE"
      if (cn == "z value") display_name <- "z"
      if (cn == "t value") display_name <- "t"
      if (cn == "Pr(>|z|)") display_name <- "p"
      if (cn == "Pr(>|t|)") display_name <- "p"
      tags$th(style = th_style, display_name)
    }))

    p_body <- lapply(seq_len(nrow(p_table)), function(i) {
      row <- p_table[i, ]
      cells <- tagList(
        tags$td(style = td_style_left, as.character(row[["Term"]])),
        lapply(p_col_names[-1], function(cn) {
          val <- as.numeric(row[[cn]])
          tags$td(style = td_style, if (abs(val) < 0.0001 && val != 0) formatC(val, format = "e", digits = 3) else round(val, 4))
        })
      )
      tags$tr(cells)
    })

    # --- Smooth terms table ---
    s_col_names <- names(s_table)
    s_header <- tagList(lapply(s_col_names, function(cn) {
      display_name <- cn
      if (cn == "p-value") display_name <- "p"
      tags$th(style = th_style, display_name)
    }))

    s_body <- lapply(seq_len(nrow(s_table)), function(i) {
      row <- s_table[i, ]
      cells <- tagList(
        tags$td(style = td_style_left, as.character(row[["Smooth"]])),
        lapply(s_col_names[-1], function(cn) {
          val <- as.numeric(row[[cn]])
          tags$td(style = td_style, if (abs(val) < 0.0001 && val != 0) formatC(val, format = "e", digits = 3) else round(val, 4))
        })
      )
      tags$tr(cells)
    })

    tagList(
      tags$div(
        style = "background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 10px 14px; margin-bottom: 12px; border-radius: 4px; font-size: 0.88rem;",
        tags$strong("Model formula:"),
        tags$pre(style = "background: #fef9e7; padding: 6px 10px; border-radius: 4px; font-size: 0.82rem; margin: 6px 0 10px 0; overflow-x: auto; white-space: pre-wrap; word-break: break-word;",
          formula_display
        ),
        tags$strong("Data summary:"),
        tags$ul(style = "margin-bottom: 8px; padding-left: 18px;",
          tags$li(paste0("Observations: ", n_obs)),
          tags$li(paste0("Speakers: ", n_speakers)),
          tags$li(paste0("Items: ", n_items)),
          tags$li(paste0("Tokens: ", n_tokens)),
          tags$li(paste0("Tone categories: ", n_tones))
        ),
        # AR1 rho display
        if (!is.null(rho)) {
          tags$p(style = "margin-bottom: 8px;",
            tags$strong("AR1 rho: "), round(rho, 4)
          )
        },
        # Convergence/warning messages
        if (!is.null(warn_msg)) {
          tags$div(
            style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 6px 10px; border-radius: 4px; margin-bottom: 8px; font-size: 0.84rem;",
            tags$strong(icon("exclamation-triangle"), " Warning: "),
            warn_msg,
            tags$p(style = "margin-top: 6px; margin-bottom: 0;",
              "Try reducing basis dimension k, removing random smooths, or removing a random intercept."
            )
          )
        },
        tags$strong("Parametric coefficients:"),
        tags$table(
          style = "margin-top: 6px; border-collapse: collapse; font-size: 0.86rem; width: 100%;",
          tags$thead(tags$tr(p_header)),
          tags$tbody(p_body)
        ),
        tags$br(),
        tags$strong("Smooth terms:"),
        tags$table(
          style = "margin-top: 6px; border-collapse: collapse; font-size: 0.86rem; width: 100%;",
          tags$thead(tags$tr(s_header)),
          tags$tbody(s_body)
        )
      )
    )
  })

  # --- Model plot ---
  # Shared by the panel and the PNG download. Column names come from the fit,
  # so the labels match the plotted model even if the pickers changed since.
  # On a sequential landmark axis (<tier>_tseq or <tier>_tbound) the x axis
  # keeps its 0-to-n scale, with a dashed line at each boundary (1, 2, ...:
  # on _tseq where one segment's f0 ends and the next begins, on _tbound the
  # TextGrid boundary), and each segment named.
  gamm_prediction_plot <- function(pdat, fit) {
    cn       <- if (!is.null(fit)) fit$col_names else list()
    f0_var   <- if (!is.null(cn$f0))   cn$f0   else input$gamm_f0_var
    tone_var <- if (!is.null(cn$tone)) cn$tone else input$gamm_tone_var
    time_var <- if (!is.null(cn$time)) cn$time else input$gamm_time_var
    seq_axis <- identical(attr(pdat, "time_axis"), "sequential")
    n_seg    <- if (seq_axis) as.integer(attr(pdat, "n_segments")) else 1L
    tier     <- sub("_(tseq|tbound)$", "", time_var)
    # Chinese labels need a CJK font (global.R). thematic sets the family of
    # text layers in renderPlot, so the segment names get it explicitly.
    seg_family <- label_family(pdat$tone, tone_var, tier, time_var, f0_var)

    # Tones in the fit's level order (predict_gamm() lists them so), which puts
    # Chinese tone names in their traditional order; ggplot2 would sort the
    # labels by code point.
    pdat$tone <- factor(pdat$tone, levels = unique(as.character(pdat$tone)))
    p <- ggplot(pdat, aes(x = time, y = f0_predicted, colour = tone, fill = tone))
    if (seq_axis) {
      if (n_seg > 1) {
        p <- p + geom_vline(xintercept = seq_len(n_seg - 1), linetype = "dashed",
                            colour = "grey55", linewidth = 0.6)
      }
      p <- p + do.call(annotate, c(list("text", x = seq_len(n_seg) - 0.5, y = Inf, vjust = 1.6,
                                        label = paste(tier, seq_len(n_seg)), colour = "grey35",
                                        size = 4.2),
                                   if (nzchar(seg_family)) list(family = seg_family)))
    }
    p <- p +
      geom_ribbon(aes(ymin = f0_predicted - 1.96 * se,
                      ymax = f0_predicted + 1.96 * se),
                  alpha = 0.2, colour = NA) +
      geom_line(linewidth = 1.2) +
      labs(
        x = if (seq_axis) sprintf("Normalised time, %s by %s (%s)", tier, tier, time_var)
            else "Normalised time",
        # Same axis title as the other tabs: f₀ with its scale, not the raw
        # column name (f0_axis_label() in global.R).
        y = paste0("Predicted ", f0_axis_label(f0_var)),
        colour = tone_var,
        fill = tone_var
      )
    if (seq_axis) {
      p <- p + scale_x_continuous(limits = c(0, n_seg), breaks = seq(0, n_seg, by = 0.5))
    }
    # No complete theme here: the app default, theme_bw(base_size = 16) from
    # server.R, applies through thematic, as on the Visualise tab. With a
    # theme_bw() of its own, thematic shrank the tick labels to about 7 pt
    # (rel() sizes compounded) and dropped the app font.
    q <- p + theme(legend.position = "bottom")
    q + cjk_family_theme(q)
  }

  output$gamm_plot <- renderPlot({
    req(gamm_plot_data())
    gamm_prediction_plot(gamm_plot_data(), gamm_fit_obj())
  }, width = 800, height = 500)

  # --- Download handler (plot image) ---
  output$gamm_download <- downloadHandler(
    filename = function() {
      paste0(input$gamm_filename, ".png")
    },
    content = function(file) {
      req(gamm_plot_data())
      p <- gamm_prediction_plot(gamm_plot_data(), gamm_fit_obj())

      ggsave(file, plot = p, width = 8, height = 5, dpi = 300)
      showNotification(paste("Plot saved as", paste0(input$gamm_filename, ".png")),
                       type = "message", duration = 4)
    }
  )

  # --- Download handler (model .rds) ---
  output$gamm_download_model <- downloadHandler(
    filename = function() {
      paste0(input$gamm_filename, "_model.rds")
    },
    content = function(file) {
      req(gamm_model())
      fname <- paste0(input$gamm_filename, "_model.rds")
      saveRDS(gamm_model(), file)
      showNotification(paste("Model saved as", fname), type = "message", duration = 4)
    }
  )

  # --- Show R code toggle ---
  gamm_code_visible <- reactiveVal(FALSE)

  observeEvent(input$gamm_show_code, {
    gamm_code_visible(!gamm_code_visible())
  })

  output$gamm_r_code <- renderUI({
    req(gamm_code_visible())
    req(input$gamm_token_var, input$gamm_f0_var, input$gamm_time_var,
        input$gamm_speaker_var, input$gamm_tone_var, input$gamm_item_var)

    token_var   <- input$gamm_token_var
    f0_var      <- input$gamm_f0_var
    time_var    <- input$gamm_time_var
    speaker_var <- input$gamm_speaker_var
    tone_var    <- input$gamm_tone_var
    item_var    <- input$gamm_item_var
    k_val       <- as.integer(input$gamm_k)
    bs_type     <- input$gamm_bs
    ri_speaker  <- input$gamm_ri_speaker
    ri_item     <- input$gamm_ri_item
    rs_type     <- input$gamm_rs_type
    use_ar1     <- input$gamm_ar1
    smooth_type <- input$gamm_smooth_type
    dur_var     <- input$gamm_dur_var
    use_dur     <- !is.null(dur_var) && dur_var != "none"

    # Build formula string for display
    if (smooth_type == "separate") {
      smooth_term <- paste0('s(time_norm, by = ', tone_var, ', k = ', k_val, ', bs = "', bs_type, '")')
      formula_display <- paste0(f0_var, ' ~ ', tone_var, ' + ', smooth_term)
    } else {
      ref_smooth <- paste0('s(time_norm, k = ', k_val, ', bs = "', bs_type, '")')
      diff_smooth <- paste0('s(time_norm, by = ', tone_var, '.ord, k = ', k_val, ', bs = "', bs_type, '")')
      formula_display <- paste0(f0_var, ' ~ ', tone_var, '.ord + ', ref_smooth, ' + ', diff_smooth)
    }
    if (use_dur) {
      formula_display <- paste0(formula_display, ' + s(', dur_var, ')')
    }
    random_parts <- c()
    tone_code_var <- if (smooth_type == "separate") tone_var else paste0(tone_var, ".ord")
    # "Speaker by Tone" smooths go by the plain tone factor with either
    # smooth type, as in fit_gamm(). Only "Reference + difference" uses the
    # ordered factor of difference smooths. With difference smooths, the
    # code then makes both factors.
    plain_tone_too <- rs_type == "speaker_by_tone" && smooth_type != "separate"
    if (rs_type == "speaker") {
      random_parts <- c(random_parts,
        paste0('s(time_norm, ', speaker_var, ', bs = "fs", m = 1, k = ', k_val, ')'))
    } else if (rs_type == "speaker_tone") {
      random_parts <- c(random_parts,
        paste0('s(time_norm, ', speaker_var, '.', tone_var, ', bs = "fs", m = 1, k = ', k_val, ')'))
    } else if (rs_type == "speaker_by_tone") {
      random_parts <- c(random_parts,
        paste0('s(time_norm, ', speaker_var, ', by = ', tone_var, ', bs = "fs", m = 1, k = ', k_val, ')'))
    } else if (rs_type == "ref_diff") {
      random_parts <- c(random_parts,
        paste0('s(time_norm, ', speaker_var, ', bs = "fs", m = 1, k = ', k_val, ')'))
      random_parts <- c(random_parts,
        paste0('s(time_norm, ', speaker_var, ', by = ', tone_code_var, ', bs = "fs", m = 1, k = ', k_val, ')'))
    } else if (ri_speaker) {
      random_parts <- c(random_parts, paste0('s(', speaker_var, ', bs = "re")'))
    }
    if (ri_item) random_parts <- c(random_parts, paste0('s(', item_var, ', bs = "re")'))
    if (length(random_parts) > 0) {
      formula_display <- paste0(formula_display, ' + ', paste(random_parts, collapse = ' + '))
    }

    # Build exclude vector string for predict()
    exclude_parts <- c()
    if (rs_type == "speaker") {
      exclude_parts <- c(exclude_parts, paste0('"s(time_norm,', speaker_var, ')"'))
    } else if (rs_type == "speaker_tone") {
      exclude_parts <- c(exclude_parts, paste0('"s(time_norm,', speaker_var, '.', tone_var, ')"'))
    } else if (rs_type %in% c("speaker_by_tone", "ref_diff")) {
      exclude_parts <- c(exclude_parts, paste0('"s(time_norm,', speaker_var, ')"'))
    } else if (ri_speaker) {
      exclude_parts <- c(exclude_parts, paste0('"s(', speaker_var, ')"'))
    }
    if (ri_item) exclude_parts <- c(exclude_parts, paste0('"s(', item_var, ')"'))
    if (use_dur) exclude_parts <- c(exclude_parts, paste0('"s(', dur_var, ')"'))
    exclude_str <- if (length(exclude_parts) > 0) {
      paste0('c(', paste(exclude_parts, collapse = ', '), ')')
    } else {
      'NULL'
    }

    # Use the actual uploaded filename if known so the snippet visually
    # matches the dataset; users may need to adjust the path on disk.
    ds_name <- if (!is.null(input$dataset_name) && nzchar(input$dataset_name))
                 paste0(input$dataset_name, ".csv")
               else "your_data.csv"

    # The code reads the uploaded file. When this tab used other data, a note
    # at the top says so, and how to get that data. A source counts only when
    # its data exists, as in active_data().
    src      <- input$gamm_dataset
    src_data <- switch(if (is.null(src)) "" else src,
                       normalised = normalised_data, curated = curated_data,
                       curated_norm = curated_norm_data, clustered = cluster_data)
    if (is.null(src_data) || is.null(src_data())) src <- "uploaded"
    src_note <- switch(src,
      normalised = c(
        "# Note: this tab used the Normalised data, not the uploaded file read below.",
        "# After the lines that read and prepare that file, add the Normalise tab's",
        "# steps with that tab's settings: shinytone::normalise_f0(), and",
        norm_time_lines(time_var, token_var, tryCatch(dataset(), error = function(e) NULL))),
      curated = c(
        "# Note: this tab used the Curated data, not the uploaded file read below.",
        "# Read the Curate tab's \"Curated CSV (analysis-ready)\" download in place of",
        "# that file, and leave out the Start-page steps below: the download has them."),
      curated_norm = c(
        "# Note: this tab used the Curated + normalised data, not the uploaded file",
        "# read below. Read the Curate tab's \"Curated CSV (analysis-ready)\" download",
        "# in place of that file, and leave out the Start-page steps below: the",
        "# download has them. Then add the Normalise tab's steps with that tab's",
        "# settings: shinytone::normalise_f0(), and",
        norm_time_lines(time_var, token_var,
                        if (!is.null(curated_data)) tryCatch(curated_data(), error = function(e) NULL))),
      clustered = c(
        "# Note: this tab used the Clustered data, not the uploaded file read below.",
        "# Read the Cluster tab's \"Download clustered CSV\" file in place of that",
        "# file, and leave out the Start-page steps below: the download has them."))
    src_note <- if (length(src_note)) paste0(paste(src_note, collapse = "\n"), "\n\n") else ""

    # Mirror fit_gamm()'s auto-detection so the snippet reproduces the app's
    # fit: an already-normalised [0, 1] time column is used as-is, anything
    # else is rescaled per token from its first f0 to its last. The name goes
    # in too, as in resolve_time_norm(), so a token_t01 or time_prop column
    # keeps its decision after rows without f0 are removed.
    df_now  <- tryCatch(active_data(), error = function(e) NULL)
    prenorm <- !is.null(df_now) && all(c(time_var, token_var) %in% names(df_now)) &&
      time_already_normalised(df_now[[time_var]], df_now[[token_var]], name = time_var)
    # A sequential landmark axis (<tier>_tseq or <tier>_tbound) is kept on
    # its own scale, as fit_gamm() does; predictions then span its observed
    # range.
    seq_axis <- grepl("_(tseq|tbound)$", time_var)
    tseq_axis <- grepl("_tseq$", time_var)
    norm_block <- if (seq_axis && !tseq_axis) {
      paste0(
      '# ', time_var, ' puts the syllables in sequence, gap kept: 0 is the first\n',
      '# f0, and the TextGrid boundaries are at 1, 2, and so on. Use it as it is,\n',
      '# so the boundaries stay at whole numbers.\n',
      'dat$time_norm <- dat$', time_var, '\n\n')
    } else if (seq_axis) {
      paste0(
      '# ', time_var, ' puts the syllables in sequence: syllable i runs from its\n',
      '# first f0 to its last over [i - 1, i]. Use it as it is, so the boundaries\n',
      '# stay at whole numbers.\n',
      'dat$time_norm <- dat$', time_var, '\n\n')
    } else if (prenorm) {
      paste0(
      '# ', time_var, ' is already on [0, 1]: use it as it is\n',
      '# (no per-token rescale: 0 and 1 stay where the column puts them)\n',
      'dat$time_norm <- pmin(pmax(dat$', time_var, ', 0), 1)\n\n')
    } else {
      # resolve_time_norm(f0 =)'s rescale: from each token's first f0 to its
      # last, over the rows with a time and f0 (0 Hz is NA after
      # zero_hz_to_na()), so rows without f0 at the token's edges do not
      # move it, with the rows removed or kept. Rows before the first f0 or
      # after the last get NA, and a token whose f0 is all at one time point
      # gets 0.5 there.
      paste0(
      '# Normalise time to [0, 1] within each token, from its first f0 to its\n',
      '# last, as the app does. Rows before the first f0 or after the last get\n',
      '# NA. A token whose f0 is all at one time point sits at 0.5 there.\n',
      'rescale01 <- function(t, has_f0) {\n',
      '  ok <- has_f0 & is.finite(t)\n',
      '  if (!any(ok)) return(rep(NA_real_, length(t)))\n',
      '  t_min <- min(t[ok])\n',
      '  t_max <- max(t[ok])\n',
      '  if (t_max == t_min) return(ifelse(t == t_min, 0.5, NA_real_))\n',
      '  p <- (t - t_min) / (t_max - t_min)\n',
      '  p[p < 0 | p > 1] <- NA\n',
      '  p\n',
      '}\n',
      'dat <- dat %>%\n',
      '  group_by(', token_var, ') %>%\n',
      '  mutate(time_norm = rescale01(', time_var, ', is.finite(', f0_var, '))) %>%\n',
      '  ungroup()\n\n')
    }

    # Keep the rows the model fits, as fit_gamm() does: bam() leaves out rows
    # that lack a model variable, such as rows without f0 at a token's edges.
    # Time is normalised above, from each token's first f0 to its last; rows
    # outside are NA and drop out with the other rows the model leaves out.
    # The rows go before the factors are made, so the factor
    # levels, the AR1 start marker and the prediction grid (its time range and
    # its reference levels) all come from the rows the model uses.
    fit_vars <- unique(c(f0_var, "time_norm", tone_var,
      if (isTRUE(ri_speaker) || rs_type %in% c("speaker", "speaker_tone", "speaker_by_tone", "ref_diff"))
        speaker_var,
      if (isTRUE(ri_item)) item_var,
      if (use_dur) dur_var))

    code_text <- paste0(
      'library(dplyr)\n',
      'library(mgcv)\n',
      'library(ggplot2)\n\n',
      # The Start-page steps (discarded tokens, metadata, durations, rows
      # without f0) come first, so the code fits the rows the app fits.
      src_note,
      paste(read_data_code(ds_name, start = if (is.null(start_settings)) NULL else start_settings()),
            collapse = "\n"), '\n\n',
      norm_block,
      '# Keep the rows the model fits (bam() leaves out rows without f0)\n',
      'dat <- dat[complete.cases(dat[, c(', paste0('"', fit_vars, '"', collapse = ', '), ')]), ]\n\n',
      '# Ensure factor variables\n',
      'dat$', speaker_var, ' <- as.factor(dat$', speaker_var, ')\n',
      'dat$', item_var, ' <- as.factor(dat$', item_var, ')\n'
    )

    tone_now <- if (!is.null(df_now)) df_now[[tone_var]]
    if (smooth_type == "separate") {
      code_text <- paste0(code_text,
        tone_factor_code(tone_var, tone_now), '\n')
    } else {
      code_text <- paste0(code_text,
        tone_factor_code(tone_var, tone_now, ordered = TRUE,
                         to = paste0(tone_var, '.ord')), '\n',
        'contrasts(dat$', tone_var, '.ord) <- "contr.treatment"\n',
        if (plain_tone_too) paste0(
          '# The Speaker by Tone smooths go by the plain tone factor\n',
          tone_factor_code(tone_var, tone_now), '\n'))
    }
    if (rs_type == "speaker_tone") {
      code_text <- paste0(code_text,
        'dat$', speaker_var, '.', tone_var, ' <- interaction(dat$', speaker_var, ', dat$', tone_var, ', drop = TRUE)\n')
    }
    code_text <- paste0(code_text, '\n')

    if (use_ar1) {
      code_text <- paste0(code_text,
        '# Create AR1 start event column: the first row of each token\n',
        if (tseq_axis) paste0(
        '# Rows at the same time (one syllable\'s last f0 and the next one\'s first)\n',
        '# keep their order in the file, as in the app: give each token\'s rows in\n',
        '# time order.\n'),
        'dat <- dat %>%\n',
        '  arrange(', token_var, ', time_norm) %>%\n',
        '  group_by(', token_var, ') %>%\n',
        '  mutate(start_event = row_number() == 1) %>%\n',
        '  ungroup()\n\n',
        '# Fit initial model (to estimate rho)\n',
        'model_init <- bam(\n',
        '  ', formula_display, ',\n',
        '  data = dat,\n',
        '  discrete = TRUE\n',
        ')\n\n',
        '# Estimate AR1 rho from the lag-1 autocorrelation within tokens\n',
        'r    <- resid(model_init) - mean(resid(model_init))\n',
        'same <- which(dat$', token_var, '[-1] == dat$', token_var, '[-nrow(dat)])\n',
        'rho  <- sum(r[-1][same] * r[-length(r)][same]) / sum(r^2)\n',
        'cat("Estimated rho:", round(rho, 4), "\\n")\n\n',
        '# Refit with AR1 correction\n',
        'model <- bam(\n',
        '  ', formula_display, ',\n',
        '  data = dat,\n',
        '  rho = rho,\n',
        '  AR.start = dat$start_event,\n',
        '  discrete = TRUE\n',
        ')\n\n'
      )
    } else {
      code_text <- paste0(code_text,
        '# Fit GAMM\n',
        'model <- bam(\n',
        '  ', formula_display, ',\n',
        '  data = dat,\n',
        '  discrete = TRUE\n',
        ')\n\n'
      )
    }

    # Prediction code — use appropriate tone variable
    tone_var_pred <- if (smooth_type == "separate") tone_var else paste0(tone_var, ".ord")

    code_text <- paste0(code_text,
      '# View results\n',
      'summary(model)\n\n')

    # Diagnostics block — mirrors the app's "Run model diagnostics" panel so the
    # exported script reproduces it. Points at the standard packages.
    acf_line <- if (use_ar1) {
      paste0('acf_resid(model)   # AR1 fit: uses the stored AR.start/rho to\n',
             '                   # show the whitened residuals (did AR1 work?)\n')
    } else {
      paste0('acf_resid(model, split_pred = list(dat$', token_var, '))  # per-token ACF\n')
    }
    code_text <- paste0(code_text,
      '# --- Model diagnostics ---\n',
      '# Base mgcv checks: residual plots (Q-Q, residuals vs linear predictor,\n',
      '# histogram, response vs fitted) and basis-dimension (k) adequacy.\n',
      'par(mfrow = c(2, 2))\n',
      'gam.check(model)\n',
      'k.check(model)                 # k-index < 1 with small p-value => raise k, refit\n',
      '# Smooth-term collinearity (0 = independent, 1 = confounded). It can fail on\n',
      '# a large model (a singular matrix), as in the app, which then shows no table;\n',
      '# try() lets the rest of the code run.\n',
      'try(concurvity(model, full = TRUE))\n\n',
      '# Residual autocorrelation, respecting token boundaries (the app\'s ACF panel).\n',
      '# itsadug::acf_resid is the standard tool in phonetics GAMM workflows:\n',
      '#   install.packages("itsadug")\n',
      'library(itsadug)\n',
      acf_line,
      '\n# gratia gives ggplot-based equivalents if you prefer:\n',
      '#   gratia::appraise(model); gratia::draw(model)\n\n')

    code_text <- paste0(code_text,
      if (seq_axis) paste0(
      '# Predict population-level smooth curves over the span the model fits.\n',
      '# dat holds only the rows the model fits, so the time range and the\n',
      '# levels below come from them.\n')
      else paste0(
      '# Predict population-level smooth curves on the 0-1 axis, as the app does.\n',
      '# dat holds only the rows the model fits, so the levels below come from them.\n'),
      'tone_levels <- levels(dat$', tone_var_pred, ')\n',
      'newdat <- expand.grid(\n',
      if (seq_axis)
        '  time_norm = seq(min(dat$time_norm, na.rm = TRUE), max(dat$time_norm, na.rm = TRUE), length.out = 200),\n'
      else
        '  time_norm = seq(0, 1, length.out = 200),\n',
      '  ', tone_var_pred, ' = tone_levels\n',
      ')\n',
      'newdat$', speaker_var, ' <- levels(dat$', speaker_var, ')[1]\n',
      'newdat$', item_var, ' <- levels(dat$', item_var, ')[1]\n'
    )
    if (rs_type == "speaker_tone") {
      # predict() needs every model variable; the term itself is excluded.
      code_text <- paste0(code_text,
        'newdat$', speaker_var, '.', tone_var, ' <- levels(dat$', speaker_var, '.', tone_var, ')[1]\n')
    }
    if (plain_tone_too) {
      # The Speaker by Tone smooths read the plain tone factor, as
      # predict_gamm() gives it.
      code_text <- paste0(code_text,
        'newdat$', tone_var, ' <- factor(as.character(newdat$', tone_var, '.ord), levels = levels(dat$',
        tone_var, '))\n')
    }

    if (use_dur) {
      code_text <- paste0(code_text,
        'newdat$', dur_var, ' <- mean(dat$', dur_var, ', na.rm = TRUE)\n')
    }

    code_text <- paste0(code_text,
      '\npreds <- predict(model, newdata = newdat,\n',
      '                 exclude = ', exclude_str, ',\n',
      '                 se.fit = TRUE)\n',
      'newdat$fit <- preds$fit\n',
      'newdat$se <- preds$se.fit\n\n',
      '# Plot predicted smooth curves with 95% CI\n',
      'ggplot(newdat, aes(x = time_norm, y = fit,\n',
      '                   colour = ', tone_var_pred, ', fill = ', tone_var_pred, ')) +\n',
      if (seq_axis) paste0(
      if (tseq_axis) '  # dashed line where one syllable\'s f0 ends and the next begins (1, 2, ...)\n'
      else '  # dashed line at each TextGrid boundary (1, 2, ...)\n',
      '  geom_vline(xintercept = seq_len(ceiling(max(dat$time_norm, na.rm = TRUE)) - 1),\n',
      '             linetype = "dashed", colour = "grey55") +\n'),
      '  geom_ribbon(aes(ymin = fit - 1.96 * se,\n',
      '                  ymax = fit + 1.96 * se),\n',
      '             alpha = 0.2, colour = NA) +\n',
      '  geom_line(linewidth = 1.2) +\n',
      '  labs(x = "', if (seq_axis) sprintf("Normalised time, %s by %s (%s)",
                                            sub("_(tseq|tbound)$", "", time_var),
                                            sub("_(tseq|tbound)$", "", time_var), time_var)
                    else "Normalised time", '",\n',
      '       y = "Predicted ', f0_axis_label(f0_var), '",\n',
      '       colour = "', tone_var, '", fill = "', tone_var, '") +\n',
      '  theme_bw(base_size = 14) +\n',
      '  theme(legend.position = "bottom")\n\n',
      '# --- Load a saved model (.rds) ---\n',
      '# model <- readRDS("', input$gamm_filename, '_model.rds")\n',
      '# summary(model)\n',
      '# predict(model, newdata = newdat, exclude = ', exclude_str, ', se.fit = TRUE)'
    )

    tagList(
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
