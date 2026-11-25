# Data Collection > Filename (inst/app/ui/filename_guide_ui.R)

fng_server <- function() {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  sh <- new.env(parent = globalenv())
  for (nm in getNamespaceExports("shiny")) assign(nm, getExportedValue("shiny", nm), envir = sh)
  sys.source(file.path(app, "ui", "filename_guide_ui.R"), sh, keep.source = FALSE)
  sh$filename_guide_ui
}

test_that("the Filename guide says what a token is, in one sentence", {
  shiny::testServer(fng_server(), {
    html <- paste(as.character(output$filename_guide_content$html), collapse = " ")
    expect_match(html, paste("Here a <strong>token</strong> is one recording of one syllable or word by one speaker",
                             "(one repetition): one <code>.wav</code> file, and all the rows of f0 data that share",
                             "its token ID."), fixed = TRUE)
    # It comes before the filename structure that shows the token ID
    expect_lt(regexpr("<strong>token</strong> is one recording", html, fixed = TRUE)[[1]],
              regexpr('class="fn-sec-t">The filename structure', html, fixed = TRUE)[[1]])
  })
})
