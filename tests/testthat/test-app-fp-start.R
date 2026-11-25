# The F0 Processing Start page's guide (inst/app/ui/fp_start_ui.R,
# output$fp_start_guide): "What each tab does" lists the four F0 Processing
# tabs with one Font Awesome icon each on a pale circle coloured by the
# stage of the work, as the F0 Analysis Start page does, through the same
# helpers (tab_item() and tab_list() in inst/app/global.R).

# The module, with the global.R helpers it calls and shiny's exports, bound
# to its data for shiny::testServer() (as in test-app-e-extraction.R).
fps_server <- function(audio = NULL) {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  sh <- new.env(parent = globalenv())
  for (nm in getNamespaceExports("shiny")) assign(nm, getExportedValue("shiny", nm), envir = sh)
  helpers <- new.env(parent = sh)
  keep <- c("tab_item", "tab_list", "min_audio_dur", "nows", "guide_box")
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                   as.character(e[[2]]) %in% keep,
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, helpers)
  sys.source(file.path(app, "ui", "fp_start_ui.R"), helpers, keep.source = FALSE)
  args <- list2env(list(fp_audio_data = shiny::reactiveVal(audio)), parent = helpers)
  bound <- function(input, output, session) NULL
  body(bound) <- body(helpers$fp_start_ui)
  environment(bound) <- args
  list(server = bound, helpers = helpers)
}

fps_html <- function(x) paste(as.character(x$html), collapse = " ")
# Plain text: tags dropped, entities read, whitespace collapsed.
fps_text <- function(h) {
  h <- gsub("<[^>]*>", "", h)
  for (e in list(c("&#39;", "'"), c("&quot;", "\""), c("&gt;", ">"), c("&lt;", "<"), c("&amp;", "&")))
    h <- gsub(e[1], e[2], h, fixed = TRUE)
  trimws(gsub("\\s+", " ", h))
}
# The <style> and <ul class="st-tabs"> of "What each tab does".
fps_tabs <- function(html) {
  a <- regexpr("<style>ul.st-tabs", html, fixed = TRUE)[[1]]
  expect_gt(a, 0)
  rest <- substring(html, a)
  substring(rest, 1, regexpr("</ul>", rest, fixed = TRUE)[[1]] + 4L)
}

test_that("the F0 Processing Start guide lists its four tabs with one icon each, coloured by stage", {
  s <- fps_server()
  shiny::testServer(s$server, {
    html <- fps_html(output$fp_start_guide)
    tabs <- fps_tabs(html)
    # Four rows, in the tab bar's order: icon, then stage
    ico <- regmatches(tabs, gregexpr('<span class="st-tab-ico">\\s*<i class="[^"]*fa-([a-z-]+)', tabs))[[1]]
    expect_identical(sub(".*fa-([a-z-]+)$", "\\1", ico),
                     c("folder-open", "file-code", "wave-square", "wrench"))
    st <- regmatches(tabs, gregexpr('<li class="st-tab-([a-z]+)">', tabs))[[1]]
    expect_identical(sub('<li class="st-tab-([a-z]+)">', "\\1", st), c("data", "prep", "prep", "expl"))
    names_at <- vapply(c("<strong>Start</strong>", "<strong>Measure f0 with Praat</strong>",
                         "<strong>F0 Extraction</strong>", "<strong>F0 Correction</strong>"),
                       function(x) regexpr(x, tabs, fixed = TRUE)[[1]], 1L)
    expect_true(all(names_at > 0))
    expect_false(is.unsorted(names_at))
    # The shared style, and no emoji or inline colour
    expect_match(tabs, '<ul class="st-tabs">', fixed = TRUE)
    expect_match(tabs, ".st-tab-expl .st-tab-ico{background:#fff3d1;color:#7a5a00;}", fixed = TRUE)
    expect_no_match(html, "&#1(27|28|29)[0-9]{3};")
    expect_false(any(utf8ToInt(enc2utf8(html)) >= 0x1F000L))
    expect_no_match(html, "#78c2ad", fixed = TRUE)
    # Every description, word for word, with no stray space before punctuation
    txt <- fps_text(tabs)
    for (d in c(
      paste("Start Upload your audio files (.wav) and optional companion files (.TextGrid, .Pitch,",
            ".PitchTier) and preview their specifications."),
      paste("Measure f0 with Praat Offline alternative: a downloadable Praat script that batch-extracts",
            "f0 from a folder of .wav files. Output (.Pitch + CSV) feeds back into the in-app workflow."),
      paste("F0 Extraction Pick a source for f0 data: run wrassp::ksvF0() on the uploaded .wav, parse",
            "already-computed .Pitch / .PitchTier files from Praat, or upload a pre-extracted f0 CSV.",
            "Optionally attaches metadata (upload CSV or derive from filename) and downloads a tidy result."),
      paste("F0 Correction Interactively review and correct extraction errors (octave jumps, halving,",
            "spurious frames, voiced/unvoiced boundaries), listening to any stretch of the recording or",
            "of its synthesised f0 contour."))) {
      expect_match(txt, d, fixed = TRUE)
    }
    expect_match(tabs, "run <code>wrassp::ksvF0()</code> on the uploaded .wav", fixed = TRUE)
    expect_no_match(txt, " [.,;:)](\\s|$)")
    # The list sits between the welcome and the filename convention
    full <- fps_text(html)
    pos <- vapply(c("Welcome!", "What each tab does", "Recommended filename convention"),
                  function(x) regexpr(x, full, fixed = TRUE)[[1]], 1L)
    expect_true(all(pos > 0))
    expect_false(is.unsorted(pos))
  })
})

test_that("both Start pages build their tab lists with the shared helpers and the same style", {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  # The list's style is defined once, in global.R, and not in either page
  for (fl in c("start_ui.R", "fp_start_ui.R")) {
    src <- paste(readLines(file.path(app, "ui", fl), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_match(src, "tab_list(", fixed = TRUE, info = fl)
    expect_match(src, "tab_item(", fixed = TRUE, info = fl)
    expect_no_match(src, "ul.st-tabs{", fixed = TRUE, info = fl)
    expect_no_match(src, "st_tab <- function", fixed = TRUE, info = fl)
  }
  # The F0 Processing page renders the same style block as the F0 Analysis page
  s <- fps_server()
  fp_style <- NULL
  shiny::testServer(s$server, {
    t <- fps_tabs(fps_html(output$fp_start_guide))
    fp_style <<- substring(t, 1, regexpr("</style>", t, fixed = TRUE)[[1]] + 7L)
  })
  a <- start_server()
  an_style <- NULL
  shiny::testServer(a$server, {
    t <- fps_tabs(start_html(output$instruction_text))
    an_style <<- substring(t, 1, regexpr("</style>", t, fixed = TRUE)[[1]] + 7L)
  })
  expect_identical(fp_style, an_style)
})

test_that("tab_item() escapes its text, keeps inline tags flush and checks the stage", {
  h <- fps_server()$helpers
  li <- as.character(h$tab_item("wrench", "Fix", "expl", "a < b & c, then ", shiny::tags$code("x"), "."))
  expect_match(li, "a &lt; b &amp; c, then <code>x</code>.", fixed = TRUE)
  expect_match(li, '<li class="st-tab-expl">', fixed = TRUE)
  expect_error(h$tab_item("wrench", "Fix", "nope", "Text."))
})

test_that("the F0 Processing Start guide has no stray space before punctuation", {
  s <- fps_server()
  shiny::testServer(s$server, {
    txt <- fps_text(fps_html(output$fp_start_guide))
    expect_match(txt, "Optional .TextGrid, .Pitch, and .PitchTier files exported from Praat", fixed = TRUE)
    expect_match(txt, "For example, ma1.wav, ma1.TextGrid, and ma1.Pitch are treated as one token.", fixed = TRUE)
    expect_match(txt, "A consistent filename pattern (e.g., yue_S01_T2_ma_r01.wav) lets F0 Extraction", fixed = TRUE)
    expect_no_match(txt, " [,;:)]")
  })
})

test_that("the F0 Processing Start guide folds the pre-extracted f0 note into a guide box", {
  s <- fps_server()
  shiny::testServer(s$server, {
    html <- fps_html(output$fp_start_guide)
    expect_match(html, paste0('<details class="guide-box">\\s*<summary class="guide-summary" ',
                              'style="font-weight: 700;">Already have f0 extracted with another tool\\?</summary>'))
    expect_no_match(html, '<details class="guide-box" open', fixed = TRUE)
    txt <- fps_text(html)
    expect_match(txt, paste("Already have f0 extracted with another tool? You can load it directly as a CSV",
                            "(with token / filename, time, f0 columns) from the F0 Extraction tab to proceed",
                            "to F0 Correction."), fixed = TRUE)
    # It sits after the welcome and before the tab list
    at <- function(x) regexpr(x, txt, fixed = TRUE)[[1]]
    expect_false(is.unsorted(c(at("Welcome!"), at("Already have f0 extracted"), at("What each tab does"))))
  })
})

test_that("the companion-files note has a soft left bar and recommends a syllable tier for multisyllabic words", {
  s <- fps_server()
  shiny::testServer(s$server, {
    html <- fps_html(output$fp_start_guide)
    expect_match(html, "border-left: 3px solid #f0c75e; padding: 4px 0 4px 12px;", fixed = TRUE)
    expect_no_match(html, "background-color: #fff8e1", fixed = TRUE)
    txt <- fps_text(html)
    expect_match(txt, paste("Multisyllabic words: a .TextGrid with a syllable tier is highly recommended, so",
                            "the analysis can line up the syllables across tokens."), fixed = TRUE)
    expect_no_match(txt, "highly recommended:", fixed = TRUE)
    # Body-size text, now that the note has no fill
    expect_no_match(html, "padding: 4px 0 4px 12px; margin-bottom: 12px; font-size: 0.88rem", fixed = TRUE)
    expect_match(txt, "For example, ma1.wav, ma1.TextGrid, and ma1.Pitch are treated as one token.", fixed = TRUE)
  })
})
