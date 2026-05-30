###############################################
# Data Collection → Filename convention subtab
# Recommended structured filename pattern, why it helps, and tips.
# Lives here (rather than F0 Processing) because naming is a planning
# decision made *before* recording. Layout mirrors the Reading-waveform
# guide: a centred reading column with a floating table of contents.
###############################################

filename_guide_ui <- function(input, output, session) {

  # Numbered green section heading, matching the Reading-waveform guide.
  fsec <- function(n, title) {
    tags$div(class = "fn-sec", id = paste0("fn-s", n),
      tags$span(class = "fn-num", n),
      tags$span(class = "fn-sec-t", title))
  }

  output$filename_guide_content <- renderUI({
    tagList(

      # --- Shared CSS: layout (page column + floating ToC) and the pattern tray ---
      tags$style(HTML("
        .fn-page { max-width: 720px; margin: 0 auto; }
        .fn-toc {
          position: fixed; top: 138px; left: 16px; width: 168px;
          font-size: 0.82rem; line-height: 1.5;
          border-left: 2px solid #e6dec3; padding-left: 12px;
          max-height: calc(100vh - 170px); overflow-y: auto;
        }
        .fn-toc-h {
          font-weight: 700; color: #2c5f4f; font-size: 0.7rem;
          text-transform: uppercase; letter-spacing: 0.06em; margin-bottom: 8px;
        }
        .fn-toc a { display: block; color: #5a6b63; text-decoration: none; padding: 3px 0; }
        .fn-toc a:hover { color: #2c8a63; }
        @media (max-width: 1100px) { .fn-toc { display: none; } }
        .fn-title { margin-bottom: 14px; }
        .fn-lead { margin: 0 0 22px 0; font-size: 0.95rem; color: #555; line-height: 1.5; }
        .fn-sec {
          display: flex; align-items: center; gap: 11px;
          margin: 34px 0 10px 0; padding-top: 14px;
          border-top: 1px solid #e6dec3; scroll-margin-top: 80px;
        }
        .fn-sec .fn-num {
          flex: none; width: 27px; height: 27px;
          border: 2px solid #78c2ad; border-radius: 50%;
          color: #2c8a63; font-weight: 700; font-size: 0.92rem;
          text-align: center; line-height: 23px;
        }
        .fn-sec .fn-sec-t { color: #2c5f4f; font-weight: 700; font-size: 1.18rem; }
        .fn-body { font-size: 0.9rem; color: #444; line-height: 1.55; }
        .fn-body li { margin-bottom: 5px; }
        .fn-pattern, .fn-example {
          font-family: 'SFMono-Regular', Menlo, Consolas, monospace;
          font-size: 1.0rem; padding: 8px 10px; border-radius: 4px;
          background: #ffffff; border: 1px solid #d6e7df;
          display: inline-block; margin: 4px 0;
        }
        .fn-seg { display: inline-block; padding: 1px 6px; border-radius: 3px;
                  font-weight: 600; }
        .fn-sep { color: #aaa; padding: 0 1px; }
        .fn-ext { color: #888; }
        .fn-lang { background: #e8f5f0; color: #2a7a5a; }
        .fn-spk  { background: #e3f2fd; color: #1565c0; }
        .fn-tone { background: #fff3e0; color: #c2410c; }
        .fn-word { background: #fce4ec; color: #c2185b; }
        .fn-rep  { background: #f3e5f5; color: #6a1b9a; }
        .fn-more { background: #ffffff; color: #888; border: 1px dashed #bbb; font-weight: 500; }
        .fn-legend { display: flex; flex-wrap: wrap; gap: 10px 18px;
                     margin: 6px 0 0 0; font-size: 0.82rem; color: #555; }
        .fn-legend > div { display: inline-flex; align-items: center; gap: 6px; }
        .fn-swatch { width: 12px; height: 12px; border-radius: 3px; display: inline-block; }
        .fn-swatch.fn-more { border: 1px dashed #bbb; background: #ffffff; }
        .fn-tid {
          background: #f4f4f6;
          padding: 4px 8px;
          border-radius: 4px;
          border: 1px solid #d8d8de;
          border-bottom: 2px solid #6c757d;
          box-shadow: 0 1px 2px rgba(0,0,0,0.04);
        }
        .fn-tid-caption {
          font-size: 0.78rem;
          color: #555;
          font-style: italic;
          margin: 4px 0 8px 4px;
        }
      ")),

      # --- Floating table of contents (left margin, wide screens only) ---
      tags$nav(class = "fn-toc",
        tags$div(class = "fn-toc-h", "On this page"),
        tags$a(href = "#fn-s1", "1. The filename structure"),
        tags$a(href = "#fn-s2", "2. Why this helps"),
        tags$a(href = "#fn-s3", "3. Tips")),

      tags$div(class = "fn-page",
        h2("Recommended filename convention", class = "fn-title"),
        tags$p(class = "fn-lead",
          "A consistent, structured filename, decided ", tags$em("before"),
          " recording, makes downstream filtering and metadata joins much easier. ",
          "Encode each piece of information you would want to filter or group by as a ",
          "separate underscore-separated segment."),

        # ===================== 1. The filename structure =====================
        fsec("1", "The filename structure"),
        tags$div(class = "fn-pattern",
          HTML(paste0(
            "<span class='fn-tid'>",
              "<span class='fn-seg fn-lang'>{language}</span>",
              "<span class='fn-sep'>_</span>",
              "<span class='fn-seg fn-spk'>{speaker}</span>",
              "<span class='fn-sep'>_</span>",
              "<span class='fn-seg fn-tone'>{tone}</span>",
              "<span class='fn-sep'>_</span>",
              "<span class='fn-seg fn-word'>{word}</span>",
              "<span class='fn-sep'>_</span>",
              "<span class='fn-seg fn-rep'>{rep}</span>",
              "<span class='fn-sep'>_</span>",
              "<span class='fn-seg fn-more'>{your_variable}</span>",
              "<span class='fn-sep' style='font-weight:600;'>*</span>",
            "</span>",
            "<span class='fn-ext'>.wav</span>"))),
        tags$div(class = "fn-tid-caption",
                 HTML("↑ The shaded portion is the <strong>token ID</strong>.")),

        tags$div(style = "margin-top: 12px;", tags$strong("Example:")),
        tags$div(class = "fn-example",
          HTML(paste0(
            "<span class='fn-tid'>",
              "<span class='fn-seg fn-lang'>yue</span>",
              "<span class='fn-sep'>_</span>",
              "<span class='fn-seg fn-spk'>S01</span>",
              "<span class='fn-sep'>_</span>",
              "<span class='fn-seg fn-tone'>T2</span>",
              "<span class='fn-sep'>_</span>",
              "<span class='fn-seg fn-word'>ma</span>",
              "<span class='fn-sep'>_</span>",
              "<span class='fn-seg fn-rep'>r01</span>",
            "</span>",
            "<span class='fn-ext'>.wav</span>"))),
        tags$div(class = "fn-tid-caption",
                 HTML("→ token ID: <code>yue_S01_T2_ma_r01</code>")),

        tags$div(style = "margin-top: 12px;", tags$strong("Legend:")),
        tags$div(class = "fn-legend",
          tags$div(tags$span(class = "fn-swatch fn-lang"), "language / project code"),
          tags$div(tags$span(class = "fn-swatch fn-spk"),  "speaker ID (zero-padded)"),
          tags$div(tags$span(class = "fn-swatch fn-tone"), "tone category"),
          tags$div(tags$span(class = "fn-swatch fn-word"), "word (in orthography or romanised form)"),
          tags$div(tags$span(class = "fn-swatch fn-rep"),  "repetition number"),
          tags$div(tags$span(class = "fn-swatch fn-more"),
                   "any extra factor(s) you need (session, context, carrier, etc.)")
        ),

        # ===================== 2. Why this helps =====================
        fsec("2", "Why this helps"),
        tags$ul(class = "fn-body",
          tags$li(tags$strong("Self-documenting data."),
                  " Each filename carries its own metadata, so you can tell what a file contains just by looking at it."),
          tags$li(tags$strong("Built-in per-token labels."),
                  " Filename segments map directly to columns for the variable info attached to each recording ",
                  "(speaker ID, tone, word, repetition, etc.). For richer information (e.g., per-speaker ",
                  "demographics in sociolinguistic studies), a separate metadata spreadsheet still does ",
                  "the heavy lifting alongside."),
          tags$li(tags$strong("Easier filtering and grouping."),
                  " Once metadata sits in columns, any downstream step can filter or group by speaker, tone, repetition, or any other encoded factor."),
          tags$li(tags$strong("Predictable order."),
                  " Sorted file lists iterate in a natural sequence, keeping batch processing consistent."),
          tags$li(tags$strong("Easier collaboration."),
                  " A consistent convention lets collaborators (and your future self) pick up the dataset without needing a separate key.")),

        # ===================== 3. Tips =====================
        fsec("3", "Tips"),
        tags$ul(class = "fn-body",
          tags$li("Use ", tags$code("_"), " as the separator. Avoid spaces, dots, and other special symbols."),
          tags$li("Zero-pad numeric parts (", tags$code("S01"), " not ", tags$code("S1"),
                  ") so filenames sort naturally."),
          tags$li("Keep each segment short but unambiguous."),
          tags$li("If your orthography uses non-ASCII characters, consider romanising in the filename ",
                  "for portability across operating systems and downstream tools."))
      )
    )
  })
}
