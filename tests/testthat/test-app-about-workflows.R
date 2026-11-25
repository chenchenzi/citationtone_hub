# The About page's "Recommended workflows" diagram (inst/app/ui.R): the
# chips of each pipeline, in order, and the data-flow-out / data-flow-in
# attributes the connector script draws its curves between.

# The app's whole UI, built as in the running app: global.R's helpers in
# reach, shiny and bslib exports in scope, run from the app's folder.
about_ui <- function(.local = parent.frame()) {
  skip_if_not_installed("bslib")
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  withr::local_dir(app, .local_envir = .local)
  # ui.R calls library(bslib); leave the search path as it was
  if (!"package:bslib" %in% search())
    withr::defer(try(detach("package:bslib", character.only = TRUE), silent = TRUE),
                 envir = .local)
  pkgs <- new.env(parent = globalenv())
  for (p in c("shiny", "bslib"))
    for (nm in getNamespaceExports(p)) assign(nm, getExportedValue(p, nm), envir = pkgs)
  env <- new.env(parent = pkgs)
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")),
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, env)
  suppressPackageStartupMessages(sys.source(file.path(app, "ui.R"), env, keep.source = FALSE))
  env$ui
}

# The workflow row whose title is `title`, as a tagQuery
workflow_row <- function(ui, title) {
  rows <- htmltools::tagQuery(ui)$find(".workflow-row")
  hit <- rows$filter(function(x, i) {
    t <- htmltools::tagQuery(x)$find(".workflow-title")$selectedTags()
    length(t) == 1L && identical(trimws(paste(unlist(t[[1]]$children), collapse = "")), title)
  })
  expect_identical(length(hit$selectedTags()), 1L)
  hit
}

# Class, text and data-flow attributes of each chip of a row's flow, in order
flow_chips <- function(row) {
  kids <- row$find(".workflow-flow")$children()$selectedTags()
  data.frame(
    class = vapply(kids, function(k) htmltools::tagGetAttribute(k, "class") %||% "", ""),
    # Tags and the emoji entities stripped, e.g. "cleaned f0 .csv"
    text  = vapply(kids, function(k)
      trimws(gsub("<[^>]+>|&#[0-9]+;", "", paste(as.character(k$children), collapse = ""))), ""),
    out   = vapply(kids, function(k) htmltools::tagGetAttribute(k, "data-flow-out") %||% NA_character_, ""),
    stringsAsFactors = FALSE
  )
}

test_that("workflow 3 offers an optional F0 Extraction export after F0 Correction", {
  ui <- about_ui()
  row <- workflow_row(ui, "Pitch-tracking correction")
  chips <- flow_chips(row)
  steps <- chips[grepl("\\bwf-step\\b", chips$class), ]

  corr <- which(steps$text == "F0 Correction")
  expect_length(corr, 1L)
  expect_match(steps$class[corr], "\\bwf-app\\b")
  expect_no_match(steps$class[corr], "\\bwf-opt\\b")

  # Next chip: the optional export step (dashed, as in the legend)
  expect_identical(steps$text[corr + 1L], "F0 Extraction (export)")
  expect_match(steps$class[corr + 1L], "\\bwf-app\\b")
  expect_match(steps$class[corr + 1L], "\\bwf-opt\\b")

  # Then the result, still the source of the curve into workflows 4 and 5
  expect_identical(nrow(steps), corr + 2L)
  expect_match(steps$class[corr + 2L], "\\bwf-result\\b")
  expect_identical(steps$out[corr + 2L], "cleaned-f0")
  expect_identical(steps$text[corr + 2L], "cleaned f0 .csv")

  # An arrow between each pair of chips from F0 Correction on
  i <- which(chips$text == "F0 Correction")
  expect_identical(chips$class[i + c(1L, 3L)], c("wf-arrow", "wf-arrow"))

  # Exactly one data-flow-out = "cleaned-f0" on the page, and its consumers remain
  html <- as.character(ui)
  expect_identical(lengths(regmatches(html, gregexpr('data-flow-out="cleaned-f0"', html, fixed = TRUE))), 1L)
  expect_match(html, 'data-flow-in="cleaned-f0"', fixed = TRUE)
  expect_match(html, 'data-flow-in="cleaned-f0 clustered-labels"', fixed = TRUE)

  # The description says so
  desc <- row$find(".workflow-desc")$selectedTags()
  expect_length(desc, 1L)
  desc <- paste(unlist(desc[[1]]$children), collapse = "")
  expect_match(desc, "fix problematic frames by ear.", fixed = TRUE)
  expect_match(desc, paste("F0 Correction saves the cleaned CSV directly.",
                           "For another region or sampling, such as equidistant points,",
                           "export it from F0 Extraction instead."),
               fixed = TRUE)
  expect_no_match(desc, "\u2014", fixed = TRUE)
})

test_that("the other workflows keep their chips", {
  ui <- about_ui()
  step_texts <- function(title) {
    ch <- flow_chips(workflow_row(ui, title))
    ch$text[grepl("\\bwf-step\\b", ch$class)]
  }
  # No workflow shows the Start page as a step: each goes from its data file
  # to the first analysis tab
  expect_identical(step_texts("Pitch-tracking quality check")[-1],
                   c("Normalise", "Visualise", "Inspect", "f0 + metadata + flags .csv"))
  expect_identical(step_texts("Tone discovery")[-1],
                   c("Normalise", "Cluster", "Curate", "clean f0 + cluster labels .csv"))
})

test_that("the top menu is a step larger than the sub-tabs", {
  ui <- about_ui()
  html <- paste(as.character(htmltools::renderTags(ui)$head), as.character(ui), collapse = " ")
  expect_match(html, "#main_nav .nav-link { font-size: 17px; }", fixed = TRUE)
})

test_that("the app's header CSS reaches the browser unescaped, child selectors included", {
  ui <- about_ui()
  html <- paste(as.character(htmltools::renderTags(ui)$head), as.character(ui), collapse = " ")
  # A plain tags$style() string would turn ">" into "&gt;" and the browser
  # would drop the guide-box heading rules
  expect_match(html, "details.guide-box > summary.guide-summary::before", fixed = TRUE)
  expect_match(html, ".faq-section > h4", fixed = TRUE)
  expect_no_match(html, "guide-box &gt; summary", fixed = TRUE)
  # The open/close marker is big enough to see
  expect_match(html, "color: #5aae96;\n        font-size: 1.25em;", fixed = TRUE)
})
