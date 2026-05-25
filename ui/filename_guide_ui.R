###############################################
# Data Collection → Filename convention subtab
# Recommended structured filename pattern, why it helps, and tips.
# Lives here (rather than F0 Processing) because naming is a planning
# decision made *before* recording.
###############################################

filename_guide_ui <- function(input, output, session) {

  output$filename_guide_content <- renderUI({
    tagList(
      h2("Recommended filename convention"),
      tags$p("A consistent, structured filename, decided ", tags$em("before"),
             " recording, makes downstream filtering and metadata joins much easier. ",
             "Encode each piece of information you'd want to filter or group by as a ",
             "separate underscore-separated segment:"),

      # --- Shared CSS for the pattern, example, legend, and token-ID tray ---
      tags$style(HTML("
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

      # --- Pattern ---
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

      # --- Example ---
      tags$div(style = "margin-top: 4px;", "Example:"),
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

      # --- Legend ---
      tags$div(class = "fn-legend",
        tags$div(tags$span(class = "fn-swatch fn-lang"), "language / project code"),
        tags$div(tags$span(class = "fn-swatch fn-spk"),  "speaker ID (zero-padded)"),
        tags$div(tags$span(class = "fn-swatch fn-tone"), "tone category"),
        tags$div(tags$span(class = "fn-swatch fn-word"), "word (in IPA or orthography)"),
        tags$div(tags$span(class = "fn-swatch fn-rep"),  "repetition number"),
        tags$div(tags$span(class = "fn-swatch fn-more"),
                 "any extra factor(s) you need (session, context, carrier, etc.)")
      ),

      # --- Why this helps ---
      tags$p(style = "margin-top: 14px; margin-bottom: 4px;", tags$strong("Why this helps")),
      tags$ul(
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

      # --- Tips ---
      tags$p(style = "margin-top: 10px; margin-bottom: 4px;", tags$strong("Tips")),
      tags$ul(
        tags$li("Use ", tags$code("_"), " as the separator. Avoid spaces, dots, and other special symbols."),
        tags$li("Zero-pad numeric parts (", tags$code("S01"), " not ", tags$code("S1"),
                ") so filenames sort naturally."),
        tags$li("Keep each segment short but unambiguous."),
        tags$li("If your orthography uses non-ASCII characters, consider romanising in the filename ",
                "for portability across operating systems and downstream tools."))
    )
  })
}
