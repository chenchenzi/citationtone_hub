# F0 Extraction (inst/app/ui/fp_extraction_ui.R): landmark columns added
# when a file is written (a tier ticked after Run extraction, any tier in
# "Upload existing f0 CSV" mode, F0 Correction's downloads) must say which
# TextGrids they could not use, as the notification at extraction does.

# The module, with the global.R helpers it calls and shiny's exports, and a
# showNotification() that records what it is given.
fp_extraction_server <- function(audio, f0) {
  app <- system.file("app", package = "shinytone")
  skip_if(!nzchar(app), "app not found")
  sh <- new.env(parent = globalenv())
  for (nm in getNamespaceExports("shiny")) assign(nm, getExportedValue("shiny", nm), envir = sh)
  helpers <- new.env(parent = sh)
  keep <- c("nows", "min_audio_dur", "landmark_sets", "guide_box")
  defs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                   as.character(e[[2]]) %in% keep,
                 as.list(parse(file.path(app, "global.R"), keep.source = FALSE, encoding = "UTF-8")))
  for (e in defs) eval(e, helpers)
  sys.source(file.path(app, "ui", "fp_extraction_ui.R"), helpers, keep.source = FALSE)
  shown <- new.env()
  shown$log <- list()
  args <- list2env(list(
    fp_audio_data = shiny::reactiveVal(audio), fp_f0_data = shiny::reactiveVal(f0),
    fp_pitch_candidates = shiny::reactiveVal(NULL), fp_metadata = shiny::reactiveVal(NULL),
    fp_corrected_data = shiny::reactiveVal(NULL),
    showNotification = function(ui, ..., id = NULL, type = "default", duration = 5) {
      shown$log[[length(shown$log) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                  id = id, type = type)
      invisible(id)
    }), parent = helpers)
  bound <- function(input, output, session) NULL
  body(bound) <- body(helpers$fp_extraction_ui)
  environment(bound) <- args
  list(server = bound, shown = shown)
}

test_that("landmarks added at download time report the TextGrids they could not use", {
  skip_if_not_installed("rPraat")
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  dir <- tempfile("tg")
  dir.create(dir)
  # tokA reads; tokB is a binary TextGrid; tokC is UTF-8 with one Latin-1 byte.
  file.copy(test_path("fixtures", "tg_zh_long_utf8.TextGrid"), file.path(dir, "tokA.TextGrid"))
  file.copy(test_path("fixtures", "tg_binary.TextGrid"), file.path(dir, "tokB.TextGrid"))
  b <- readBin(test_path("fixtures", "tg_zh_long_utf8.TextGrid"), "raw", 1e5)
  at <- grepRaw('text = "', b, fixed = TRUE, all = TRUE)[2] + 8L   # first byte of label 2
  b[at] <- as.raw(0xE9)
  writeBin(b, file.path(dir, "tokC.TextGrid"))
  toks  <- c("tokA", "tokB", "tokC")
  audio <- data.frame(basename = toks, wav_path = NA_character_,
                      tg_path = file.path(dir, paste0(toks, ".TextGrid")),
                      pitch_path = NA_character_, pitchtier_path = NA_character_)
  f0 <- data.frame(token = rep(toks, each = 5), time = rep(seq(0.1, 0.4, length.out = 5), 3),
                   f0 = 200)
  srv <- fp_extraction_server(audio, f0)

  shiny::testServer(srv$server, {
    session$setInputs(fp_region = "voiced", fp_sampling_mode = "native",
                      fp_landmark_tiers = "phone")
    res <- fp_export_data()
    expect_true("phone" %in% names(res$data))
    n_lab <- tapply(!is.na(res$data$phone), res$data$token, sum)
    expect_identical(as.vector(n_lab[toks]), c(5L, 0L, 5L))
    expect_true(any(grepl("1 TextGrid(s) could not be read (tokB: it is in Praat's binary format",
                          res$notes, fixed = TRUE)))
    expect_true(any(grepl("'tokC.TextGrid' is UTF-8 except on line 22", res$notes, fixed = TRUE)))

    # The interval region says the unreadable TextGrid once, not twice.
    session$setInputs(fp_region = "interval", fp_region_tier = "phone", fp_region_mode = "vowel")
    res <- fp_export_data()
    expect_length(grep("could not be read", res$notes, fixed = TRUE), 1L)

    # F0 Correction's downloads get the columns and one notification with a
    # fixed id, so a second download replaces it.
    d <- fp_decorate_download(f0)
    expect_true(all(c("phone", "phone_start", "phone_end", "phone_i") %in% names(d)))
    fp_decorate_download(f0)
    log <- Filter(function(l) identical(l$type, "warning") || identical(l$id, "fp_download_landmarks"),
                  srv$shown$log)
    expect_length(log, 2L)
    expect_identical(vapply(log, `[[`, "", "id"), rep("fp_download_landmarks", 2))
    expect_identical(log[[1]]$type, "warning")
    expect_match(log[[1]]$text, "could not be read", fixed = TRUE)
    expect_match(log[[1]]$text, "'tokC.TextGrid' is UTF-8", fixed = TRUE)
  })
})
